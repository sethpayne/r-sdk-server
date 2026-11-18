# Design — launchdarklyServer (unofficial R server-side SDK)

**Status:** Milestones in progress (M1 design done; M2+ implementation). Unofficial until LaunchDarkly assigns an official SDK name/identity.

**Primary reference:** [launchdarkly/python-server-sdk](https://github.com/launchdarkly/python-server-sdk) (including contract-tests).  
**FDv2 cross-check:** [ruby-server-sdk](https://github.com/launchdarkly/ruby-server-sdk), [go-server-sdk](https://github.com/launchdarkly/go-server-sdk), [java-core](https://github.com/launchdarkly/java-core).  
**Contract harness:** [sdk-test-harness](https://github.com/launchdarkly/sdk-test-harness) `v3`.  
**Open specs:** [open-sdk-specs](https://github.com/launchdarkly/open-sdk-specs) (hooks/OTEL/log codes — out of scope for v1).

Identity constants live in one place: `R/constants.R` (repo name, R package name, SDK name pending LaunchDarkly assignment, version).

---

## 1. Process model

### Constraint

R has **no OS threads** in package code. Server SDKs must keep an in-memory flag store fresh via streaming/polling and flush analytics events **without** blocking `variation()` / `all_flags_state()` on network I/O. Hosts include scripts, Shiny, Plumber, and forked worker pools (e.g. `parallel`, future, Shiny with multiple processes).

### Options compared

| | **A. `callr` background worker** | **B. In-process `later` + `httr2` async** |
|---|---|---|
| Concurrency | Separate R process owns network, store mutations, event buffer | Single interpreter; I/O scheduled on `later` |
| Eval path | Parent holds a local store snapshot; eval never waits on network | Same if store updates are carefully non-blocking |
| Scripts / batch | Works without an application event loop | Requires pumping `later` (easy to forget; stalls updates) |
| Shiny / Plumber | Works; parent drains IPC via `later` | Fits Shiny’s existing loop, but competes with app callbacks |
| Fork | Child detects PID change, does not reuse parent worker; starts own client | Fork breaks `later` / open connections; same re-init need |
| Complexity | IPC + process lifecycle | Less process mgmt; harder “always progressing” guarantees |
| CRAN / ops | Spawning processes needs care (tests, Windows) | Fewer moving parts |

### Decision: **A — `callr` background worker** (with `later` only for parent IPC drain)

**Reasons:**

1. **One writer.** The worker process is the sole mutator of the authoritative store, event buffer, and FDv2 client state. The parent never writes those structures.
2. **Non-blocking evaluations.** `variation()` / `all_flags_state()` read a parent-side snapshot updated by asynchronous IPC. They never perform HTTP/SSE.
3. **Scripts work.** Batch jobs do not need a host event loop for sync progress; the worker runs independently. Parent drains IPC opportunistically (before eval, on timers via `later` when available, and on `flush()` / `close()`).
4. **Clean shutdown.** Worker is supervised: session `on.exit` / finalizers / explicit `$close()` send shutdown and wait with timeout; no orphaned workers by design (see §3).
5. **`later` alone is insufficient** as the primary model: without a continuously pumped loop (non-Shiny scripts), streaming and event delivery stall while still sharing the interpreter with user code.

**Rejected for v1 primary model:** pure in-process `later` + `httr2`. May be revisited later if callr proves too costly on CRAN/Windows; the message schema (§2) should allow an alternate transport.

### Runtime topology

```
┌──────────────────────────── parent R process ────────────────────────────┐
│  LdClient (R6)                                                            │
│    • read-only store snapshot                                             │
│    • IPC drain (later + opportunistic)                                    │
│    • variation / all_flags_state / track enqueue                          │
│    • fork check (PID)                                                     │
└───────────────┬──────────────────────────────▲────────────────────────────┘
                │ commands (JSON lines)         │ events / store deltas
                ▼                               │
┌──────────────────────────── callr worker ───────────────────────────────┐
│  FDv2 coordinator                                                         │
│    • initializers → synchronizers                                         │
│    • authoritative store + selector                                       │
│    • event buffer + flush to events URI                                   │
│    • status / diagnostics                                                 │
└───────────────────────────────────────────────────────────────────────────┘
```

---

## 2. Message format (parent ↔ worker)

Transport: **newline-delimited JSON** over the callr connection (stdin/stdout or a dedicated socketpair). One JSON object per line. Versions are additive; unknown fields ignored; unknown `type` → log + ignore (worker) or error status (parent).

### Envelope

```json
{
  "v": 1,
  "id": "optional-correlation-id",
  "type": "MessageType",
  "payload": {}
}
```

### Parent → worker

| `type` | Purpose |
|--------|---------|
| `Start` | Config snapshot (endpoints, timeouts, offline, stream/poll prefs). **SDK key sent once over IPC; never logged.** |
| `Track` / `Identify` | Analytics enqueue |
| `Flush` | Request immediate event flush |
| `Shutdown` | Flush (best effort), close connections, exit 0 |
| `Ping` | Liveness |

### Worker → parent

| `type` | Purpose |
|--------|---------|
| `Ready` | First usable basis (selector defined or offline/test mode) |
| `StoreSnapshot` | Full flag/segment dataset (init / reconnect full payload) |
| `StoreDelta` | Incremental puts/deletes from FDv2 change sets |
| `Status` | Data-source state (`initializing` / `valid` / `interrupted` / `off`) + error info |
| `EventsAck` | Flush completed / dropped counts |
| `Log` | Structured log line (level, code, message; **no secrets**) |
| `Fatal` | Unrecoverable; worker exiting |
| `Pong` | Liveness reply |

Store payloads use the same logical shapes as other server SDKs (flag/segment JSON objects keyed by kind + key), compressed in spirit with FDv2 change-set apply semantics on the worker; the parent applies snapshots/deltas to its mirror only.

---

## 3. Fork detection, ownership, shutdown

Aligned with python-server-sdk’s `_owner_pid` / `postfork()` behavior ([`ldclient/client.py`](https://github.com/launchdarkly/python-server-sdk/blob/main/ldclient/client.py)):

1. **Ownership:** Each OS process that evaluates flags owns exactly one worker for that `LdClient` instance. Record `owner_pid` at start.
2. **Detection:** On public API entry, if `Sys.getpid() != owner_pid`, the inherited client is invalid (callr handles/connections do not safely transfer).
3. **Behavior:** Log a clear warning once per child PID; evaluations return defaults / empty `all_flags_state` until the app creates a **new** client in the child (or we expose `postfork()` that tears down inherited state and starts a new worker). Do **not** silently reuse the parent worker.
4. **Shutdown:**
   - `$close()` / `on.exit` / `reg.finalizer` → `Shutdown` message → wait (bounded) → `process$kill()` if needed.
   - Worker is non-session-leader child; if parent dies, worker should exit (callr supervision / detecting broken pipe).
   - Goal: **no orphaned long-lived workers** after session end.

---

## 4. FDv2 data system (no FDv1 implementation)

### Model (from Python/Ruby/Go FDv2)

Two phases ([python `FDv2`](https://github.com/launchdarkly/python-server-sdk/blob/main/ldclient/impl/datasystem/fdv2.py), Ruby `DataSystem`, Go `internal/datasystem`):

1. **Initializers** (ordered): each `fetch` tries to obtain a **Basis** (change set + environment id + **Selector**). Continue until an initializer returns a **defined selector**, or the list is exhausted. File/test initializers may apply data with **no selector** and must **not** alone mark the SDK fully initialized (per LaunchDarkly data-saving / file-initializer guidance).
2. **Synchronizers** (ordered preference): primary typically **streaming** (`/meval/v2/stream` family), fallback **polling** (`/sdk/poll`). Selector sent as `basis` query param for incremental payloads. Fallback/recovery timing mirrors reference SDKs (interrupted/initializing age thresholds; recovery to primary after sustained validity).

Default chain (parity with `ldclient.datasystem.default()`):

- Initializers: polling  
- Synchronizers: streaming, then polling  
- **No FDv1 data source** in this R SDK

### `fdv1Fallback` / `X-LD-FD-Fallback`

Reference SDKs can switch to an FDv1-compatible synchronizer. **This package will not implement FDv1.**

On fallback directive (header or update flag):

1. Apply any accompanying payload already received (so eval can use last server data).
2. **Fail-safe:** stop FDv2 sync attempts; set data-source status to `off` (or equivalent interrupted-terminal); continue serving the last store snapshot; evaluations keep working from cache / defaults if empty.
3. **Log** a loud warning that FDv1 fallback was requested and is unsupported in this unofficial R SDK.
4. Do not spin forever on a protocol we do not speak.

### Selector

Opaque state string + version as defined by FDv2 payloads (`payload-transferred` events). Worker stores the latest defined selector and attaches it to subsequent poll/stream requests. Parent does not need the selector for evaluation.

---

## 5. Package layout

```
r-server-sdk/                    # GitHub repo name (see NOTES for remote naming)
├── DESCRIPTION
├── NAMESPACE
├── LICENSE / LICENSE.md         # Apache-2.0
├── README.md                    # unofficial status up front
├── DESIGN.md                    # this file
├── FEATURES.md
├── NOTES.md
├── R/
│   ├── constants.R              # SINGLE source of identity + version
│   ├── aaa.R                    # package env / onLoad hooks (later)
│   ├── config.R                 # LdConfig
│   ├── context.R                # Context builders
│   ├── client.R                 # LdClient public API
│   ├── evaluation.R             # EvaluationDetail, FeatureFlagsState
│   ├── process-worker.R         # callr supervision + IPC (later milestones)
│   ├── fdv2-*.R                 # initializer/synchronizer (worker side)
│   ├── store.R
│   ├── events.R
│   └── zzz.R
├── inst/
│   └── worker/                  # optional entry script for callr
├── tests/testthat/
├── man/                         # roxygen (milestone 10 polish)
└── .gitignore
```

Logical modules map closely to Python’s `ldclient` / `ldclient.impl` split, adapted to R file conventions.

---

## 6. Public API surface (v1)

R-friendly wrappers over an R6 client. Naming uses `ld_*` helpers where natural; methods are snake_case.

| API | Role |
|-----|------|
| `ld_config(...)` / `LdConfig` | SDK key, URIs, offline, start wait, streaming/polling, dial timeouts, private attrs, app metadata |
| `ld_client(config)` / `LdClient$new(config)` | Construct + start worker |
| `$variation(key, context, default)` | Flag value |
| `$variation_detail(key, context, default)` | Value + reason |
| `$all_flags_state(context, ...)` | Bootstrap state (`client_side_only`, `with_reasons`, `details_only_for_tracked_flags`) |
| `$track(event_name, context, data = NULL, metric_value = NULL)` | Custom events |
| `$identify(context)` | Identify event |
| `$flush()` | Async flush request to worker |
| `$close()` | Shutdown worker |
| `$is_initialized()` | Data availability |
| `$is_offline()` | Config flag |
| `$secure_mode_hash(context)` | HMAC for JS secure mode (`digest`) |
| `$postfork()` | Re-init after fork (parity with Python) |
| `ld_context(...)` / builders | Context / multi-context |
| `EvaluationDetail`, `FeatureFlagsState` | Result types |

**Intentionally deferred from v1 public surface** (see FEATURES.md): migrations, plugins, hooks, OpenTelemetry, big segments, persistent/Redis/Dynamo/Consul stores, Relay daemon mode, async dual client.

Optional later: package-level singleton helpers analogous to `ldclient.set_config` / `get()` — open question in NOTES.md.

---

## 7. Dependencies and minimum R

| Package | Role | Justification |
|---------|------|----------------|
| **httr2** | HTTP + SSE/polling in the worker | Modern HTTP client; streaming-friendly; TLS via curl |
| **jsonlite** | IPC + LD JSON payloads | Standard, CRAN-friendly JSON |
| **digest** | `secure_mode_hash` | HMAC-SHA256 without bringing in heavier crypto stacks |
| **R6** | Client/config objects | Mutable client with methods; familiar pattern for SDK-like APIs |
| **later** | Parent-side IPC drain / timers | Non-blocking receive when an event loop exists (Shiny); optional pump helpers for scripts |
| **callr** | Background worker | Chosen process model (§1) |

**Minimum R version: 4.1** — aligns with current httr2/later stacks and avoids ancient tidyverse edge cases. Revisit if reviewers need older R.

Suggests (not Depends): `testthat`, `httptest`/`webfakes` for contract tests in later milestones.

---

## 8. Threat model (initial)

| Asset / threat | Mitigation |
|----------------|------------|
| **SDK key** | Pass only via Start IPC; never write to logs, messages of type `Log`, or error strings; avoid `print` of config; document env-var loading for apps |
| **TLS** | Default HTTPS to LD endpoints via httr2/curl; no custom insecure defaults; optional custom CA only via explicit config |
| **Logging** | Structured levels; redact Authorization and key-shaped strings; context attrs subject to private-attribute rules before events leave the worker |
| **Untrusted JSON** | Treat stream/poll/file payloads as untrusted: size limits, schema-tolerant parse, reject pathological nesting where practical; never `eval` JSON |
| **IPC surface** | Worker accepts commands only on the private callr channel; no listening TCP port by default |
| **Fork** | Do not leak parent worker control to children (§3) |
| **CRAN constraints** | No network in default tests/examples; skip/online tests gated; deterministic offline/test fixtures; avoid download in `R CMD check` |
| **Supply chain** | Prefer small dependency set; pin policy TBD in NOTES |

---

## 9. Milestone 1 gate

M1 is **green** when:

- [x] DESIGN.md covers process model, IPC, fork/shutdown, FDv2, layout, API, deps, threats  
- [x] FEATURES.md matrix vs Python with explicit out-of-scope items  
- [x] NOTES.md decisions + open questions for human review  
- [x] Skeleton: constants, Apache-2.0 LICENSE, DESCRIPTION/NAMESPACE stubs, unofficial README  
- [x] Conventional commit(s) recording M1 complete  

**Out of M1:** implementation, test harness, network I/O (milestones 2+).
