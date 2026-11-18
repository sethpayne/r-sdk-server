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

## Open questions for Seth (human review)

1. **GitHub repo name:** Project identity says `r-server-sdk`; this remote is `sethpayne/r-sdk-server`. Rename remote, keep both names in docs, or treat `r-sdk-server` as canonical in `constants.R`?
2. **Official SDK name / user-agent string:** Placeholder `SDK_NAME` / `USER_AGENT` until LaunchDarkly assigns — confirm format preference (e.g. `RServerClient/<version>` vs waiting).
3. **Confirm callr process model** vs preference for pure `later`+httr2 (e.g. stronger CRAN/Windows concerns).
4. **Singleton API:** Expose `ld_set_config()` / `ld_get_client()` like Python, or construct-only R6 for v1?
5. **`postfork()`:** Require explicit call (Python) vs auto-reinit on first use after PID change (more magic)?
6. **CRAN goal:** Is CRAN publication a near-term goal (affects Examples, net checks, callr policies)?
7. **File/test data sources:** Promote into v1 if needed for harness earlier than FEATURES “later”?
8. **Contract-test hosting:** Plumber vs httpuv vs `servr` for sdk-test-harness service process?
9. **Default start wait:** Python uses 5s — same for R?
10. **Multi-context / Migration flags:** Confirm migrations stay out even if harness suites mention them (plan: skip/omit those suites).

---

## Suppressions

N/A for Milestone 1 (no harness / CI suppressions yet).

---

## Sources read for M1 (non-exhaustive)

- https://github.com/launchdarkly/python-server-sdk (README, `.sdk_metadata.json`, `ldclient/client.py`, `ldclient/datasystem.py`, `ldclient/impl/datasystem/fdv2.py`, LICENSE)
- https://github.com/launchdarkly/ruby-server-sdk (`lib/ldclient-rb/data_system.rb`)
- https://github.com/launchdarkly/go-server-sdk (FDv2 initializer/synchronizer behavior via public docs/commits)
- https://github.com/launchdarkly/sdk-test-harness/tree/v3
- https://github.com/launchdarkly/open-sdk-specs (hooks/OTEL present; not full SDK spec)
- https://launchdarkly.com/docs/sdk/features/data-saving-mode (initializer vs selector / ready semantics)
