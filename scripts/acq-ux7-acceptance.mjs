// ACQ-UX7 browser acceptance runner for the disposable loopback fixture.
//
// Usage:
//   node scripts/acq-ux7-acceptance.mjs /tmp/acq-ux7-results.json
//
// Set ACQ_UX7_URL to point at a locally launched
// inst/curation/acq_ux7_acceptance_fixture.R. The runner never navigates to a
// production origin and fails closed if the URL is not loopback.

import {writeFile, mkdir} from 'node:fs/promises';
import {URL} from 'node:url';

const playwrightModule = process.env.PLAYWRIGHT_MODULE || '/tmp/acq-c1-browser/node_modules/playwright/index.mjs';
const {chromium} = await import(playwrightModule);
const output = process.argv[2] || process.env.ACQ_UX7_OUTPUT || '/tmp/acq-ux7-results.json';
const outputDir = output.includes('/') ? output.slice(0, output.lastIndexOf('/')) : '.';
await mkdir(outputDir || '.', {recursive: true});

const url = process.env.ACQ_UX7_URL || 'http://127.0.0.1:43867';
const parsedUrl = new URL(url);
if (!['127.0.0.1', 'localhost', '::1'].includes(parsedUrl.hostname)) {
  throw new Error(`ACQ-UX7 runner only permits loopback URLs, got ${url}`);
}
const reviewerUrl = process.env.ACQ_UX7_REVIEWER_URL || '';
if (!reviewerUrl) {
  throw new Error('ACQ_UX7_REVIEWER_URL must point to the separate reviewer fixture.');
}
const parsedReviewerUrl = new URL(reviewerUrl);
if (!['127.0.0.1', 'localhost', '::1'].includes(parsedReviewerUrl.hostname)) {
  throw new Error(`ACQ-UX7 runner only permits loopback reviewer URLs, got ${reviewerUrl}`);
}

const result = {
  schema: 'planscanR.acq-ux7-acceptance.v1',
  task: 'ACQ-UX7',
  scope: 'disposable loopback synthetic curation/acquisition fixture; no production state or authentication',
  execution: {
    url,
    reviewer_url: reviewerUrl,
    writes_to_production: false,
    secrets_read: false,
    workers_started: false,
    downloads_started: false,
    browser_authorization: 'isolated fixture only; user acceptance remains a separate gate'
  },
  thresholds: {
    warm_landing_seconds: 3,
    warm_page_seconds: 1,
    warm_filter_seconds: 1,
    warm_acknowledgement_seconds: 0.5
  },
  cold: {},
  warm: {landing: [], page: [], filter: [], acknowledgement: []},
  flows: [],
  errors: [],
  browser: {}
};

const browser = await chromium.launch({
  headless: true,
  args: ['--enable-precise-memory-info']
});
const context = await browser.newContext({viewport: {width: 1440, height: 1100}});
await context.tracing.start({screenshots: true, snapshots: true});
const page = await context.newPage();
const cdp = await context.newCDPSession(page);
await cdp.send('Performance.enable');
let receivedBytes = 0;
page.on('pageerror', error => result.errors.push(`pageerror: ${error.message}`));
page.on('console', message => {
  if (message.type() === 'error') result.errors.push(`console: ${message.text()}`);
});
page.on('websocket', socket => socket.on('framereceived', payload => {
  const frame = payload?.payload ?? payload;
  const text = typeof frame === 'string' ? frame : JSON.stringify(frame ?? '');
  receivedBytes += Buffer.byteLength(text);
}));

const stable = async () => {
  await page.waitForFunction(() => {
    const recalculating = [...document.querySelectorAll('.recalculating')]
      .some(node => node.getClientRects().length > 0);
    const busy = document.documentElement.classList.contains('shiny-busy') ||
      document.body.classList.contains('shiny-busy');
    return !recalculating && !busy;
  }, null, {timeout: 120000});
};
const ready = async () => {
  await page.locator('#fixture-ready').filter({hasText: /ready 128000 synthetic records/}).waitFor({timeout: 120000});
  await page.locator('#vb_total').filter({hasText: '128,000'}).waitFor({timeout: 120000});
  await stable();
};
const performanceMetrics = async () => {
  const metrics = (await cdp.send('Performance.getMetrics')).metrics;
  return Object.fromEntries(metrics
    .filter(item => ['JSHeapUsedSize', 'JSHeapTotalSize', 'Nodes', 'Documents'].includes(item.name))
    .map(item => [item.name, item.value]));
};
const tableState = async () => page.evaluate(() => {
  let state = {};
  try { state = window.Reactable?.getState('review_tbl') || {}; } catch (_) {}
  let fixtureMetrics = null;
  try {
    fixtureMetrics = JSON.parse(document.querySelector('#fixture-metrics')?.textContent || 'null');
  } catch (_) {}
  return {
    review_info: document.querySelector('#review_info')?.textContent?.trim() || '',
    review_page_info: document.querySelector('#review_page_info')?.textContent?.trim() || '',
    reactable_page: state.page,
    reactable_page_size: state.pageSize,
    dom_rows: document.querySelectorAll('#review_tbl .rt-tr-group').length,
    campaign_status: document.querySelector('#acquisition-campaign-campaign-status')?.textContent?.trim() || '',
    acknowledgement: document.querySelector('#acquisition-campaign-campaign-submit-result')?.textContent?.trim() || '',
    action_result: document.querySelector('#acquisition-dashboard-action-result')?.textContent?.trim() || '',
    state_text: document.querySelector('#fixture-state')?.textContent?.trim() || '',
    fixture_metrics: fixtureMetrics
  };
});
const record = (bucket, label, elapsed, beforeBytes, state) => {
  const item = {
    label,
    elapsed_seconds: elapsed,
    received_websocket_bytes: receivedBytes - beforeBytes,
    browser_memory: state.browser_memory,
    state: {...state, browser_memory: undefined}
  };
  if (bucket === 'cold') result.cold[label] = item;
  else result.warm[bucket].push(item);
  return item;
};
const measure = async (bucket, label, action, wait = stable) => {
  const beforeBytes = receivedBytes;
  const started = performance.now();
  await action();
  await wait();
  await stable();
  const elapsed = (performance.now() - started) / 1000;
  // Refresh server resource/call counters after the timed action. The
  // refresh round trip is intentionally outside the measured interval.
  await refreshFixtureMetrics();
  const state = await tableState();
  state.browser_memory = await performanceMetrics();
  const item = record(bucket, label, elapsed, beforeBytes, state);
  await writeFile(output, JSON.stringify(result, null, 2) + '\n');
  return item;
};
const check = (name, passed, detail = undefined) => {
  result.flows.push({name, passed: Boolean(passed), ...(detail === undefined ? {} : {detail})});
};
const selectValue = async (selector, value, targetPage = page) => targetPage.locator(selector).evaluate((element, value) => {
  if (element.selectize) element.selectize.setValue(value);
  else {
    element.value = value;
    element.dispatchEvent(new Event('change', {bubbles: true}));
  }
}, value);
const refreshFixtureMetrics = async () => {
  const before = await page.locator('#fixture-metrics').textContent();
  await page.evaluate(() => {
    const actor = document.querySelector('#fixture-actor')?.value;
    if (window.Shiny && actor) window.Shiny.setInputValue('fixture-actor', actor, {priority: 'event'});
  });
  await page.waitForFunction(previous =>
    document.querySelector('#fixture-metrics')?.textContent !== previous,
  before, {timeout: 120000});
  await stable();
};

try {
  await measure('cold', 'cold_landing', () => page.goto(url, {waitUntil: 'domcontentloaded'}), ready);
  for (let index = 1; index <= 5; index += 1) {
    await measure('landing', `warm_landing_${index}`, () => page.reload({waitUntil: 'domcontentloaded'}), ready);
  }

  // Read-only navigation covers Overview, Review, Random review and the
  // Acquisition tab before any fixture mutation is attempted.
  for (const tab of ['Overview', 'Review', 'Random review', 'Acquisition']) {
    const tabLocator = page.getByRole('tab', {name: tab, exact: true});
    await tabLocator.click();
    await tabLocator.getAttribute('aria-selected').then(value => {
      if (value !== 'true') throw new Error(`tab did not activate: ${tab}`);
    });
    // bslib keeps the acquisition module alive while the tab transition is
    // delivered; allow that transition to finish before the next navigation.
    await page.waitForTimeout(1000);
    await stable();
    check(`read-only navigation: ${tab}`, await tabLocator.getAttribute('aria-selected') === 'true');
  }

  await page.getByRole('tab', {name: 'Review', exact: true}).click();
  await selectValue('#stage_filter', 'all');
  await page.locator('#review_tbl').waitFor({timeout: 120000});
  await page.locator('#review_page_info').filter({hasText: /128000 rows/}).waitFor({timeout: 120000});
  for (let index = 1; index <= 5; index += 1) {
    await measure('page', `warm_page_${index}`,
      () => page.locator('#review_page_next').click(),
      () => page.locator('#review_page_info').filter({hasText: /Page 2 of/}).waitFor({timeout: 120000}));
    await page.locator('#review_page_prev').click();
    await page.locator('#review_page_info').filter({hasText: /Page 1 of/}).waitFor({timeout: 120000});
  }

  const filterInput = page.locator('#review_search');
  for (let index = 1; index <= 5; index += 1) {
    await measure('filter', `warm_filter_${index}`,
      async () => {
        await filterInput.fill('Energy planning record 128000');
      },
      () => page.locator('#review_page_info').filter({hasText: /1 rows/}).waitFor({timeout: 120000}));
    await filterInput.fill('');
    await page.locator('#review_page_info').filter({hasText: /128000 rows/}).waitFor({timeout: 120000});
  }

  await page.getByRole('tab', {name: 'Acquisition', exact: true}).click();
  await page.getByRole('tab', {name: 'Acquisition', exact: true}).getAttribute('aria-selected').then(value => {
    if (value !== 'true') throw new Error('Acquisition tab did not activate');
  });
  await page.waitForTimeout(1000);
  await page.locator('#acquisition-campaign-campaign-preview').waitFor({timeout: 120000});
  await page.locator('#acquisition-campaign-campaign-status').filter({hasText: /No jobs created/}).waitFor({timeout: 120000});
  check('preview is read-only before submit',
    (await page.locator('#acquisition-campaign-campaign-status').innerText()).includes('No jobs created') &&
    (await page.locator('#fixture-state').innerText()).includes('jobs 0'));

  await page.locator('#acquisition-storage-effective').waitFor({timeout: 120000});
  const initialEstimate = Number(await page.locator('#acquisition-storage-default-estimate').inputValue());
  await page.locator('#acquisition-storage-default-estimate').fill(String(initialEstimate * 2));
  await page.locator('#acquisition-storage-save').click();
  await page.locator('#acquisition-storage-result').filter({hasText: /Saved version/}).waitFor({timeout: 120000});
  check('administrator settings save is bounded and future-job scoped',
    (await page.locator('#acquisition-storage-result').innerText()).includes('future download jobs') &&
    (await page.locator('#acquisition-storage-result').innerText()).includes('restart is not required'));

  for (let index = 1; index <= 5; index += 1) {
    const expectedCampaign = `campaign-acq-ux7-fixture-${String(index).padStart(2, '0')}`;
    await measure('acknowledgement', `warm_acknowledgement_${index}`,
      () => page.locator('#acquisition-campaign-campaign-submit').click(),
      () => page.locator('#acquisition-campaign-campaign-submit-result')
        .filter({hasText: expectedCampaign}).waitFor({timeout: 120000}));
  }
  check('submission acknowledgement records isolated queued work',
    (await page.locator('#acquisition-campaign-campaign-submit-result').innerText()).includes('independent workers') &&
    (await page.locator('#fixture-state').innerText()).includes('campaigns 5 jobs 4') &&
    (await page.locator('#fixture-state').innerText()).includes('campaign-acq-ux7-fixture-01') &&
    (await page.locator('#fixture-state').innerText()).includes('campaign-acq-ux7-fixture-05'));

  await page.locator('#acquisition-dashboard-target-id').waitFor({state: 'attached', timeout: 120000});
  await selectValue('#acquisition-dashboard-target-type', 'job');
  await page.waitForFunction(() => {
    const select = document.querySelector('#acquisition-dashboard-target-id');
    return select && [...select.options].some(option => option.value);
  }, null, {timeout: 120000});
  const targetId = await page.locator('#acquisition-dashboard-target-id').evaluate(select =>
    [...select.options].find(option => option.value)?.value);
  await selectValue('#acquisition-dashboard-target-id', targetId);
  await selectValue('#acquisition-dashboard-action', 'pause');
  await page.locator('#acquisition-dashboard-apply').click();
  await page.locator('#acquisition-dashboard-action-result').filter({hasText: /paused|pause/i}).waitFor({timeout: 120000});
  check('administrator control applies only to the explicit fixture target',
    (await page.locator('#acquisition-dashboard-action-result').innerText()).includes(targetId));

  // Actor identity is immutable for a curation session. Verify reviewer
  // read-only and control denial through a separate fixture process/session.
  const reviewerContext = await browser.newContext({viewport: {width: 1440, height: 1100}});
  const reviewerPage = await reviewerContext.newPage();
  reviewerPage.on('pageerror', error => result.errors.push(`reviewer pageerror: ${error.message}`));
  reviewerPage.on('console', message => {
    if (message.type() === 'error') result.errors.push(`reviewer console: ${message.text()}`);
  });
  try {
    await reviewerPage.goto(reviewerUrl, {waitUntil: 'domcontentloaded'});
    await reviewerPage.locator('#fixture-ready').filter({hasText: /ready 128000 synthetic records; actor reviewer/}).waitFor({timeout: 120000});
    const reviewerTab = reviewerPage.getByRole('tab', {name: 'Acquisition', exact: true});
    await reviewerTab.click();
    await reviewerTab.getAttribute('aria-selected').then(value => {
      if (value !== 'true') throw new Error('reviewer Acquisition tab did not activate');
    });
    await reviewerPage.waitForTimeout(1000);
    await reviewerPage.locator('#fixture-state').filter({hasText: /jobs 4/}).waitFor({timeout: 120000});
    await reviewerPage.locator('#acquisition-storage-controls').waitFor({timeout: 120000});
    const reviewerStateBefore = await reviewerPage.locator('#fixture-state').innerText();
    check('reviewer sees storage controls as read-only',
      (await reviewerPage.locator('#acquisition-storage-controls').innerText()).includes('read-only') &&
      await reviewerPage.locator('#acquisition-storage-save').count() === 0);
    await reviewerPage.locator('#acquisition-dashboard-target-id').waitFor({state: 'attached', timeout: 120000});
    await selectValue('#acquisition-dashboard-target-type', 'job', reviewerPage);
    await reviewerPage.waitForFunction(() => {
      const select = document.querySelector('#acquisition-dashboard-target-id');
      return select && [...select.options].some(option => option.value);
    }, null, {timeout: 120000});
    const reviewerTargetId = await reviewerPage.locator('#acquisition-dashboard-target-id').evaluate(select =>
      [...select.options].find(option => option.value)?.value);
    await selectValue('#acquisition-dashboard-target-id', reviewerTargetId, reviewerPage);
    await selectValue('#acquisition-dashboard-action', 'pause', reviewerPage);
    await reviewerPage.locator('#acquisition-dashboard-apply').click();
    await reviewerPage.locator('#acquisition-dashboard-action-result')
      .filter({hasText: /administrator|admin role/i}).waitFor({timeout: 120000});
    const reviewerStateAfter = await reviewerPage.locator('#fixture-state').innerText();
    check('reviewer control attempt is rejected without a catalog write',
      reviewerStateAfter.includes(reviewerStateBefore.match(/campaigns \d+ jobs \d+/)?.[0] || '') &&
      reviewerStateAfter.includes('campaign-acq-ux7-reviewer-seed'));
  } finally {
    await reviewerContext.close();
  }

  await page.screenshot({path: output.replace(/\.json$/, '-desktop.png'), fullPage: true});
  await page.setViewportSize({width: 390, height: 844});
  check('narrow fixture has no horizontal overflow', await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth));
  await page.screenshot({path: output.replace(/\.json$/, '-narrow.png'), fullPage: true});
} catch (error) {
  result.errors.push(error?.stack || String(error));
} finally {
  const warmValues = Object.fromEntries(Object.entries(result.warm).map(([key, values]) => [key, values.map(value => value.elapsed_seconds)]));
  const samples = [
    ...Object.values(result.cold),
    ...Object.values(result.warm).flat()
  ];
  const metrics = samples.map(sample => sample.state?.fixture_metrics).filter(Boolean);
  const maxMetric = key => {
    const values = metrics.map(metric => Number(metric[key])).filter(Number.isFinite);
    return values.length ? Math.max(...values) : null;
  };
  const maxBrowserHeap = Math.max(
    ...samples.map(sample => Number(sample.browser_memory?.JSHeapUsedSize)).filter(Number.isFinite),
    0
  );
  result.summary = {
    warm_medians_seconds: Object.fromEntries(Object.entries(warmValues).map(([key, values]) => [key, values.length ? values.slice().sort((a, b) => a - b)[Math.floor(values.length / 2)] : null])),
    warm_runs_recorded: Object.fromEntries(Object.entries(result.warm).map(([key, values]) => [key, values.length])),
    threshold_status: Object.fromEntries(Object.entries(result.warm).map(([key, values]) => {
      const threshold = result.thresholds[`warm_${key}_seconds`];
      const median = values.length ? values.map(value => value.elapsed_seconds).sort((a, b) => a - b)[Math.floor(values.length / 2)] : null;
      return [key, values.length === 5 && median <= threshold];
    }))
  };
  result.resource_bounds = {
    max_browser_js_heap_used_bytes: maxBrowserHeap || null,
    max_server_rss_bytes: maxMetric('server_rss_bytes'),
    max_server_shared_bytes: maxMetric('server_shared_bytes'),
    max_snapshot_bytes: maxMetric('snapshot_bytes'),
    last_fixture_calls: metrics.at(-1)?.calls || null
  };
  result.passed = result.errors.length === 0 && result.flows.every(flow => flow.passed) &&
    Object.values(result.summary.threshold_status).every(Boolean);
  await writeFile(output, JSON.stringify(result, null, 2) + '\n');
  await context.tracing.stop({path: output.replace(/\.json$/, '-trace.zip')});
  result.browser.version = browser.version();
  await browser.close();
  await writeFile(output, JSON.stringify(result, null, 2) + '\n');
}

console.log(JSON.stringify(result));
if (!result.passed) process.exitCode = 1;
