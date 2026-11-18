# Contract tests (sdk-test-harness v3)

HTTP service implementing the [sdk-test-harness](https://github.com/launchdarkly/sdk-test-harness) v3 protocol for `launchdarklyServer`.

## Run the service

From the repository root (dev load via `pkgload`):

```bash
Rscript contract-tests/service.R 8000
```

## Milestone coverage

| Milestone | Commands / capabilities |
|-----------|-------------------------|
| **M3** | `GET /` status, create/delete client, `contextBuild`, `contextConvert`; capabilities: `server-side`, `context-type` |
| M4+ | polling / offline / evaluate / events / streaming / … |

## Harness

Use a floating sdk-test-harness **v3** binary or container against this service. Filter to context suites for M3. Suppressions (with justifications) live in `contract-tests/suppressions.txt` when needed.
