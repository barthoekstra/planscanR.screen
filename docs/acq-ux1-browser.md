# ACQ-UX1 actual browser baseline

This run used Chromium 134.0.6998.35 against the disposable loopback app in
`inst/curation/acq_ux1_browser_app.R`, with 128,000 synthetic records, synthetic
reviewer identity and temporary SQLite/artifact stores. No production login,
review, job, setting or model mutation occurred. The existing dirty app and
config sources were loaded with `pkgload`; this is source-checkout evidence,
not a claim about installed production behavior.

The full run is [acq-ux1-browser.json](acq-ux1-browser.json) and its end-state
screenshot is [acq-ux1-browser.png](acq-ux1-browser.png). Server helper timings
and RSS are recorded separately in `acq-ux1-baseline.md`.

| Actual browser action | Seconds | Received websocket bytes | Used JS heap bytes | Visible rows |
| --- | ---: | ---: | ---: | ---: |
| First session landing | 2.789 | 14,841 | 13,778,724 | 0 |
| Reload landing | 2.058 | 14,841 | 17,849,432 | 0 |
| First Review + select All | 43.661 | 7,844,538 | 40,845,380 | 25 |
| Return to Review | 0.668 | 0 | 41,595,684 | 25 |
| Next page | 0.578 | 0 | 48,747,808 | 25 |
| Previous then next page | 0.633 | 0 | 62,720,556 | 25 |
| Search a displayed title | 0.555 | 0 | 42,355,372 | 1 |
| Clear then repeat search | 0.620 | 0 | 61,049,940 | 1 |
| Threshold 0.7 | 44.356 | 7,844,620 | 63,418,128 | 25 |
| Threshold 0.5 then 0.7 | 93.387 | 15,688,096 | 85,289,156 | 25 |

These are single-run characterization values, not acceptance medians. Landing
starts at navigation against an already listening R process; R process startup
and snapshot preparation are excluded. The first Review action includes the
initial tab load and selecting All from the default empty model-selected view.
Repeated page/filter/threshold actions contain the stated reset and action,
so they are not one-operation latency estimates. Threshold readiness requires receipt of the relevant `review_tbl` output
and no visible recalculating outputs or Shiny busy state. A 500 ms settling
delay is included in every action; thresholds include another 500 ms per
threshold change. Hidden suspended outputs are excluded from this readiness
check. Every final visible-state check reports `recalculating: false`. Overview
selected-count text remains suspended while Review is active; the recorded
Overview count is stale and is not used as threshold-completion evidence. Websocket bytes are
traffic received within that measured interval. They are not exact widget JSON sizes. Browser memory
is Chromium CDP JS heap and DOM counters, not total browser RSS. The server
helper's 25-row render timings simulate a hypothetical smaller payload; actual
current page navigation operates locally on the already received 5,000 rows.

The Review status states `showing 5000 of 128000 (capped)`. Fast local page and
search actions therefore do not demonstrate full-corpus search. The browser
reports 25 DOM rows, while first Review received about 7.8 MB of websocket
traffic. No page JavaScript errors occurred.

Reproduce from the package root, with the workspace R library configured:

```sh
R_LIBS_USER=/data/biogain-docs/.Rlib Rscript inst/curation/acq_ux1_browser_app.R
PLAYWRIGHT_MODULE=/path/to/playwright/index.mjs node scripts/acq-ux1-browser.mjs /tmp/acq-ux1-browser.json
```

The runner is hardcoded to the disposable loopback port 43819. It changes
view/selection thresholds only and never clicks review or administrative
mutation buttons. Signed-in production browser verification was unavailable;
acceptance of this concrete isolated fixture alternative is required by the
parent plan before ACQ-UX1 completion.
