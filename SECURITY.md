# Security checklist — launchdarklyServer (unofficial)

| Control | Status |
|---------|--------|
| SDK key never logged (Start IPC only; Log messages redacted) | Implemented |
| TLS via httr2/curl defaults (HTTPS endpoints) | Implemented |
| No `eval` of JSON payloads | Implemented |
| Private attribute redaction before event flush | Implemented |
| Worker has no listening TCP port | Implemented |
| Fork: no reuse of parent worker; `postfork()` required | Implemented |
| CRAN: no network in default examples/tests | Offline/webfakes only |
| Dependency surface kept small | See DESCRIPTION |

Report vulnerabilities privately to the repository maintainers. This package is **unofficial** and not endorsed by LaunchDarkly.
