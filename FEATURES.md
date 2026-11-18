# Feature matrix — launchdarklyServer vs python-server-sdk

Living matrix (updated as milestones land). Source of feature list: [python-server-sdk `.sdk_metadata.json`](https://github.com/launchdarkly/python-server-sdk/blob/main/.sdk_metadata.json) plus core client APIs in `ldclient/client.py`.

**Legend**

| Status | Meaning |
|--------|---------|
| **v1** | Planned for first useful release of this unofficial R SDK |
| **later** | In scope after v1 (additional milestones) |
| **out** | Explicitly out of scope unless LaunchDarkly parity later requires it |

---

## Matrix

| Feature (Python metadata / API) | Python | R plan | Notes |
|---------------------------------|--------|--------|-------|
| Contexts | yes (8.0+) | **done (M2)** | `ld_context` / builders; S3 + R6 builders |
| `variation` / `variation_detail` | yes | **done (M5)** | Full evaluator; feature/summary/index events |
| `all_flags_state` / allFlags | yes | **done (M5)** | Bootstrap / front-end state |
| `track` / custom events | yes | **done (M6)** | EventBuffer + `/bulk` |
| `identify` | yes | **done (M6)** | |
| `flush` / `close` | yes | **done (M4/M6)** | |
| Offline mode | yes | **done (M4)** | |
| FDv2 data system | yes (9.13+) | **done (M4/M7)** | Only FDv2; see DESIGN.md |
| Streaming synchronizer | yes | **done (M7/DoD)** | curl-multi SSE; secondary after 401/403; poll fallback |
| Polling initializer + fallback sync | yes | **done (M4/M7)** | Default chain parity |
| FDv1 data source / fdv1 fallback sync | yes | **out** | Fail-safe + log if server requests fallback (DESIGN §4) |
| App metadata | yes | **done** | Tags header |
| Private attributes | yes | **done (M6)** | Event redaction |
| Secure mode hash | yes | **done** | `digest` HMAC |
| Web proxy | yes | **later** | httr2 proxy config; not blocking v1 |
| Event compression | yes | **later** | Nice-to-have bandwidth |
| Polling response compression | yes | **later** | |
| Inline context (eval/custom events) | yes | **done (M6)** | Schema 4 inline context |
| Omit anonymous contexts | yes | **done** | `omit_anonymous_contexts` (index/identify) |
| Experimentation (event roles) | yes | **partial** | `inExperiment` on eval; summary event polish later |
| Diagnostics | yes | **later** | Opt-in diagnostic events |
| File data source (FDv2 initializer) | yes | **later** | |
| Test data source | yes | **done (M8)** | `ld_test_data()` |
| Flag change listeners | yes | **done (M8)** | `FlagTracker` |
| Data source status provider | yes | **done** | Worker Status messages / `is_initialized` |
| Data store status provider | yes | **out** | No persistent store |
| Relay proxy (proxy mode) | yes | **later** | Custom base URIs cover common case in v1 |
| Relay proxy daemon + persistent store | yes | **out** | Depends on storingData |
| **Persistent feature stores** (Redis/Consul/Dynamo/generic) | yes | **out** | Complexity + native deps; in-memory only |
| **Big segments** | yes | **out** | Needs external store integration |
| **Migrations** API | yes | **out** | Niche; high API surface |
| **Hooks** | yes | **done (M8)** | Minimal `LdHook` (not full open-sdk-specs) |
| **Plugins** | yes | **out** | |
| **OpenTelemetry** | yes | **out** | |
| Async client (`AsyncLDClient`) | experimental | **out** | R model is sync eval + background worker |
| Multiple concurrent clients | yes | **done** | Each owns a worker |
| `postfork` / fork safety | yes | **done (M4/M9)** | DESIGN §3 |
| Contract tests (sdk-test-harness v3) | yes | **done (M3+)** | Service + suppressions |

---

## Explicit out-of-scope (v1 product boundary)

Unless a future LaunchDarkly review requires parity:

1. **Persistent stores** (Redis, DynamoDB, Consul, custom `FeatureStore`) — out.  
2. **Big segments** — out.  
3. **Migrations** — out.  
4. **Plugins / hooks / OpenTelemetry** — out.  
5. **FDv1 protocol implementation** — out (fail-safe + log only).

---

## Gaps vs Python that are “later” (not out)

- File data source (test data source is done)  
- Proxy / compression / diagnostics polish  
- Relay-as-proxy via URI overrides beyond basic config  
- Package singleton helpers  
- Full streaming HTTP retry-status matrix (reconnect + poll fallback exist; see suppressions)  

Reasons: shrink v1 to correct FDv2 evaluation + events + process safety; add integrations after the core is harness-green.

**Coverage (v1 DoD):** hard gate ≥90% on evaluator / context / data_system / event_processor via offline tests (measured ~95.3% / 93.7% / ≥90% / 95.9%). Worker IPC (`process-worker.R` / `inst/worker`) excluded per NOTES.md D24.
