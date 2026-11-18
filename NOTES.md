# NOTES — decisions and open questions

Living document for the unofficial R server-side SDK. Update when decisions change.

---

## Decisions (Milestone 1)

| ID | Decision | Rationale |
|----|----------|-----------|
| D1 | Process model = **`callr` worker** + parent store mirror; `later` only for IPC drain | R has no threads; eval must never block on network; scripts lack a host event loop (DESIGN §1) |
| D2 | **FDv2 only**; no FDv1 implementation | Matches LD direction; on `fdv1Fallback`, fail-safe + log (DESIGN §4) |
| D3 | Identity constants in **`R/constants.R` only** | Single place for repo name, package name, SDK name (pending), version |
| D4 | R package name **`launchdarklyServer`** | Project identity; `library(launchdarklyServer)` |
| D5 | License **Apache-2.0** | Same as python-server-sdk / LD open SDKs |
| D6 | Min R **4.1** | httr2/later ecosystem |
| D7 | Depends: httr2, jsonlite, digest, R6, later, callr | DESIGN §7 |
| D8 | Persistent stores, big segments, migrations, plugins/hooks/OTEL = **out of scope** for v1 | FEATURES.md |
| D9 | Unofficial until LD assigns SDK name; **no logos / endorsement** | README + constants `SDK_NAME` pending |
| D10 | Conventional commits; small reviewable commits | Project process |
| D11 | Primary behavioral reference = **python-server-sdk** (+ harness v3) | Cross-check FDv2 with Ruby/Go/Java |

---

## Decisions (Milestone 2+) — resolved without Seth consult

Per project direction: implement using Python SDK + specs as source of truth.

| ID | Decision | Rationale |
|----|----------|-----------|
| D12 | Repo constant stays **`r-server-sdk`**; remote may be `r-sdk-server` (`LD_GITHUB_REMOTE_HINT`) | Project identity; do not rename remote from agent |
| D13 | User-Agent stays **`RServerClient-Unofficial/<ver>`** until LD assigns | Matches M1 placeholder |
| D14 | **callr confirmed** as process model | DESIGN D1; no CRAN/Windows evidence against yet |
| D15 | **Construct-only R6** for v1 (`ld_client` / `LdClient`); no singleton helpers | Simpler; Python singleton is optional sugar |
| D16 | **`postfork()` explicit** (Python parity); on PID mismatch log once + fail-safe defaults until re-init | Avoid magic auto-reinit |
| D17 | CRAN-ready hygiene (no net in default check) but **publication not blocking** v1 | Prepare Examples/skips; ship unofficial GitHub first |
| D18 | **Test data source** promoted into harness path (M8); file DS still later unless harness needs | FEATURES “later” → v1 when required |
| D19 | Contract service = **httpuv** (no Plumber dep) | Assignment: httpuv/plumber; httpuv is lighter Suggests |
| D20 | Default **start wait = 5s** | Python `startWaitTimeMs` default 5000 |
| D21 | Migrations suites **omitted/suppressed**; migrations stay out | FEATURES out of scope |
| D22 | Context builder method is **`mark_private()`** (R6 reserves `private`) | R deviation from Python `.private()`; harness maps field `private` → `mark_private` |
| D23 | Context is immutable **S3** (`ld_context`); builders are **R6** | Matches Python immutability without R6 copy traps |

---

## Open questions (non-blocking)

None that block M2–M10. Official SDK name / user-agent remains pending LaunchDarkly assignment.

---

## Suppressions

See `contract-tests/suppressions.txt` — migrations, big-segments, persistent stores, FDv1 retry suites, OTEL/plugins (justifications inline).

---

## Deviations from Python

| Area | Deviation | Why |
|------|-----------|-----|
| Concurrency | callr process vs threads | R has no threads |
| FDv1 | Not implemented | Fail-safe + log on `fdv1Fallback` |
| Context API | `mark_private()` not `private()` | R6 keyword |
| Context type | S3 list vs Python class | Idiomatic immutable R |
| Singleton | Omitted in v1 | D15 |

---

## Sources read for M1–M2 (non-exhaustive)

- https://github.com/launchdarkly/python-server-sdk (`ldclient/context.py`, `contract-tests/client_entity.py`, `contract-tests/service.py`)
- https://github.com/launchdarkly/sdk-test-harness/tree/v3
- https://github.com/launchdarkly/open-sdk-specs
