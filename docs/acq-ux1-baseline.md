# ACQ-UX1 screen characterization baseline

This checked-in note records the current offline characterization. The runner
is `inst/curation/acq_ux1_characterization.R`; it uses a disposable provider
and fixture, reads no sidecar contents or secrets, and writes no production
state. Full sanitized output is retained at
`../../.claude/state/acq-ux1-evidence/screen-baseline.json` and
`../../.claude/state/acq-ux1-evidence/screen-baseline.md` from the workspace
root.

## Fixture and representative corpus

The synthetic fixture has 128,000 rows and 20 columns. It contains 32,000
native document IDs reused across four countries, yielding 96,000 native-ID
collision cases and 128,000 country/document composite keys. The current
server snapshot retains all 128,000 rows. The available representative cache
is `/data/biogain-docs/data/biogain/cache`; the run recorded 141,695 metadata
sidecars. It inspected only sidecar names, sizes, and country directory
counts, never sidecar contents.

## Measured server baseline

Measurements were made with R 4.6.1 and the workspace package library. They
are server/offline timings; browser rendering is recorded separately.

| Operation | Elapsed | Widget rows | Widget JSON bytes |
| --- | ---: | ---: | ---: |
| Cold landing snapshot | 42.951 s | 128,000 server rows | — |
| Warm landing snapshot | 42.953 s | 128,000 server rows | — |
| First tab helper | 22.376 s | 5,000 | 1,290,767 |
| Repeat tab helper | 19.696 s | 5,000 | 1,290,767 |
| First page helper simulation | 0.104 s | 25 | 6,923 |
| Repeat page helper simulation | 0.103 s | 25 | 6,923 |
| First country filter helper (`nl`) | 20.160 s | 5,000 | 1,290,767 |
| Repeat country filter helper (`nl`) | 19.971 s | 5,000 | 1,290,767 |
| First threshold helper | 0.001 s | 38,400 | — |
| Repeat threshold helper | 0.011 s | 38,400 | — |

The captured process RSS reached 670,031,872 bytes during repeated widget
serialization. The server app currently caps the displayed frame at 5,000
rows (`R/curation_app.R`); this is why tab/filter payload counts are lower than
the retained snapshot. The page timings above are helper simulations that
pass a 25-row frame directly to the widget. Actual app page navigation initially
sends the capped 5,000-row display frame and is measured by the host browser
fixture separately.

The installed widget inventory and payload columns are in the JSON evidence.
The current `reactable` object marks non-display columns hidden but still sends
them in its JSON data. The fixture confirms `internal_note` and
`source_payload` are present in that payload.

The JSON also records isolated `reactable` probes for the workspace, curation,
and acquisition R libraries. Each available library reports version `0.4.5`,
the full `reactable()` formal list, and the server exports
`reactableOutput`, `renderReactable`, `getReactableState`, and
`updateReactable`. The runner binds the source checkout with
`pkgload::load_all` before measuring it, so the source and installed-library
observations are kept separate.

`acquisition_dashboard_poll_token()` returns `Sys.time()`, so two no-change
tokens differed in the fixture. This records token instability only; it does
not count database reads. The BIOGAIN deployment's five-second setting is the
acquisition worker idle poll interval, while the dashboard runner uses the
screen package default `refresh_ms = 2000`; the timing token and those clocks
are separate contracts.

The same disposable dashboard probe seeded 1,000 synthetic successful ingest
attempts and called `acquisition_dashboard_summary()` twice. Both summaries
reported 500,500 completed bytes, while the current SQL materialized 1,000
attempt rows per call (2,000 total). The future regression keeps this as a
bounded-read target; it does not require memoizing the refresh itself.

The additional disposable surface probe measured the actual
`acquisition_campaign_panel_server` `output[['campaign-items']]` value through
`shiny::testServer`: two HTML data rows occupied 1,395 bytes. The corresponding
preview item JSON was 671 bytes. The historical-only reactable payload was one
row and 498 bytes; the admin review reactable payload was one row and 807
bytes. These are fixture measurements of existing output builders and did not
submit work or mutate review state.

## Browser and deliberate regressions

A managed Playwright module is available at
`/tmp/acq-c1-browser/node_modules/playwright/index.mjs`; the host owns the
disposable Chromium page/memory probe and its evidence. This runner did not
sign in or touch a production origin. The server evidence marks managed
Playwright availability without treating it as browser-memory evidence.

The isolated suite
`tests/characterization/test-acq-ux1-future.R` intentionally remains outside
the default tests. Its literal expected-red output is retained at
`../../.claude/state/acq-ux1-evidence/future-red.log` and currently has five
failures: the actual app-server 5,000-row cap, an unbounded widget page
payload, hidden fields in widget JSON, changing no-change poll tokens, and
materialization of all 2,000 repeated dashboard-attempt rows.
The country/document/revision collision identity check passes. The default
baseline and existing curation/dashboard/records tests pass.

For context only, existing C4 deployment evidence records a prior production
load of 34,423 rows and 98 columns (141,545,328-byte frame) with approximately
490,576 KiB process RSS high-water. That historical observation is separate
from this offline run.

The sanitized server evidence and literal expected-red output are also retained
in [acq-ux1-server.json](acq-ux1-server.json) and
[acq-ux1-red.txt](acq-ux1-red.txt).
