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
| Contexts | yes (8.0+) | **v1 (M2)** | `ld_context` / builders; S3 + R6 builders |
| `variation` / `variation_detail` | yes | **v1** | |
| `all_flags_state` / allFlags | yes | **v1** | Bootstrap / front-end state |
| `track` / custom events | yes | **v1** | |
| `identify` | yes | **v1** | |
| `flush` / `close` | yes | **v1** | |
| Offline mode | yes | **v1** | |
| FDv2 data system | yes (9.13+) | **v1** | Only FDv2; see DESIGN.md |
| Streaming synchronizer | yes | **v1** | Primary sync |
| Polling initializer + fallback sync | yes | **v1** | Default chain parity |
| FDv1 data source / fdv1 fallback sync | yes | **out** | Fail-safe + log if server requests fallback (DESIGN §4) |
| App metadata | yes | **v1** | Headers / diagnostic identity |
| Private attributes | yes | **v1** | Event redaction |
| Secure mode hash | yes | **v1** | `digest` HMAC |
| Web proxy | yes | **later** | httr2 proxy config; not blocking v1 |
| Event compression | yes | **later** | Nice-to-have bandwidth |
| Polling response compression | yes | **later** | |
| Inline context (eval/custom events) | yes | **v1** | Match current server event schema as implemented in Python 9.x |
| Omit anonymous contexts | yes | **later** | Config flag parity |
| Experimentation (event roles) | yes | **v1** | Needed for correct event shaping with experiments |
| Diagnostics | yes | **later** | Opt-in diagnostic events |
| File data source (FDv2 initializer) | yes | **later** | Useful for tests/local; harness may need sooner |
| Test data source | yes | **later** | Prefer for unit tests; contract tests may drive timing |
| Flag change listeners | yes | **later** | R callback model TBD |
| Data source status provider | yes | **v1** | Minimal status for ops / `is_initialized` |
| Data store status provider | yes | **out** | No persistent store |
| Relay proxy (proxy mode) | yes | **later** | Custom base URIs cover common case in v1 |
| Relay proxy daemon + persistent store | yes | **out** | Depends on storingData |
| **Persistent feature stores** (Redis/Consul/Dynamo/generic) | yes | **out** | Complexity + native deps; in-memory only for this SDK unless LD requires parity |
| **Big segments** | yes | **out** | Needs external store integration |
| **Migrations** API | yes | **out** | Niche; high API surface |
| **Hooks** | yes | **out** | open-sdk-specs HOOK-\*; revisit if harness mandates |
| **Plugins** | yes | **out** | |
| **OpenTelemetry** | yes | **out** | open-sdk-specs OTEL-\*; no R OTEL story required for v1 |
| Async client (`AsyncLDClient`) | experimental | **out** | R model is sync eval + background worker |
| Multiple concurrent clients | yes | **v1** | Allowed; each owns a worker |
| `postfork` / fork safety | yes | **v1** | DESIGN §3 |
| Contract tests (sdk-test-harness v3) | yes | **later** | Milestone after core impl (not M1) |

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

- File + test data sources  
- Flag change listeners  
- Proxy / compression / diagnostics polish  
- Relay-as-proxy via URI overrides beyond basic config  
- Package singleton helpers  
- Full sdk-test-harness service  

Reasons: shrink v1 to correct FDv2 evaluation + events + process safety; add integrations after the core is harness-green.
