// ACQ-RUN4 disposable dashboard browser runner.
//
// The URL must be a loopback fixture. The runner records a trace, screenshot,
// and JSON UI-flow log; it never navigates to a production origin.

import {mkdir, writeFile} from 'node:fs/promises';
import {URL} from 'node:url';

const playwrightModule = process.env.PLAYWRIGHT_MODULE ||
  '/home/bhoekstra4/.npm/_npx/e41f203b7505f1fb/node_modules/playwright/index.mjs';
const chromiumPath = process.env.PLAYWRIGHT_EXECUTABLE_PATH ||
  '/home/bhoekstra4/.cache/ms-playwright/chromium_headless_shell-1161/chrome-linux/headless_shell';
const {chromium} = await import(playwrightModule);
const output = process.argv[2] ||
  process.env.ACQ_RUN4_OUTPUT ||
  '/tmp/acq-run4-dashboard-browser.json';
const outputDir = output.includes('/') ? output.slice(0, output.lastIndexOf('/')) : '.';
await mkdir(outputDir || '.', {recursive: true});
const url = process.env.ACQ_RUN4_URL || 'http://127.0.0.1:43874';
const parsed = new URL(url);
if (!['127.0.0.1', 'localhost', '::1'].includes(parsed.hostname)) {
  throw new Error(`ACQ-RUN4 runner only permits loopback URLs, got ${url}`);
}

const result = {
  schema: 'planscanR.acq-run4-dashboard-browser.v1',
  task: 'ACQ-RUN4',
  scope: 'disposable loopback synthetic acquisition catalog',
  execution: {url, writes_to_production: false, secrets_read: false, workers_started: false},
  flows: [],
  errors: [],
  browser: {}
};
const browser = await chromium.launch({headless: true, executablePath: chromiumPath});
const context = await browser.newContext({viewport: {width: 1440, height: 1100}});
await context.tracing.start({screenshots: true, snapshots: true});
const page = await context.newPage();
page.on('pageerror', error => result.errors.push(`pageerror: ${error.message}`));
page.on('console', message => {
  if (message.type() === 'error') result.errors.push(`console: ${message.text()}`);
});

const stable = async () => {
  await page.waitForTimeout(250);
};
const flow = async (name, action) => {
  try {
    await action();
    result.flows.push({name, passed: true});
  } catch (error) {
    result.flows.push({name, passed: false, detail: error.message});
    throw error;
  }
};
const clickMode = async (mode, expected) => {
  await page.locator(`#fixture-${mode}`).click();
  await page.locator('#acquisition-dashboard-health-status')
    .filter({hasText: new RegExp(`Status: ${expected}`)})
    .waitFor({timeout: 30000});
  await stable();
};
const waitForPendingWarning = async (expected) => {
  await page.waitForFunction((shouldWarn) => {
    const text = document.querySelector('#acquisition-dashboard-pending-warning')?.textContent || '';
    return shouldWarn ? /action required|no available worker/i.test(text) : text.trim() === '';
  }, expected, {timeout: 30000});
};

try {
  await flow('landing reports idle health and no pending warning', async () => {
    await page.goto(url, {waitUntil: 'domcontentloaded'});
    await page.locator('#fixture-state').waitFor({timeout: 30000});
    await clickMode('idle', 'Idle');
    await waitForPendingWarning(false);
    const warning = await page.locator('#acquisition-dashboard-pending-warning').innerText();
    if (warning.trim()) throw new Error(`unexpected idle warning: ${warning}`);
  });
  await flow('busy health is independent from queue rows', () => clickMode('busy', 'Busy'));
  await flow('offline health shows actionable pending warning', async () => {
    await clickMode('offline', 'Offline');
    await waitForPendingWarning(true);
    const warning = await page.locator('#acquisition-dashboard-pending-warning').innerText();
    if (!/action required|no available worker/i.test(warning)) {
      throw new Error(`missing actionable warning: ${warning}`);
    }
  });
  await flow('stale health remains visible', () => clickMode('stale', 'Stale'));
  await flow('unknown health remains visible', () => clickMode('unknown', 'Unknown'));
  await flow('refresh to idle clears warning', async () => {
    await clickMode('idle', 'Idle');
    await waitForPendingWarning(false);
    const warning = await page.locator('#acquisition-dashboard-pending-warning').innerText();
    if (warning.trim()) throw new Error(`idle warning did not clear: ${warning}`);
  });
  await flow('verified completion changes files, bytes, and latest success', async () => {
    await page.locator('#fixture-verified').click();
    await page.locator('#acquisition-dashboard-progress')
      .filter({hasText: /Verified files/})
      .waitFor({timeout: 30000});
    await page.waitForFunction(() => {
      const progress = document.querySelector('#acquisition-dashboard-progress')?.textContent || '';
      const state = document.querySelector('#fixture-state')?.textContent || '';
      return /Verified files/.test(progress) &&
        /Latest successful completion/.test(progress) &&
        /verified files\s*\|\s*1/i.test(state) &&
        /verified bytes\s*\|\s*[1-9]/i.test(state) &&
        !/latest success\s*\|\s*unknown/i.test(state);
    }, null, {timeout: 30000});
    await stable();
    const progress = await page.locator('#acquisition-dashboard-progress').innerText();
    const fixtureState = await page.locator('#fixture-state').innerText();
    if (!/Latest successful completion|Latest successful download/.test(progress)) {
      throw new Error(`latest success is not visible: ${progress}`);
    }
    if (!/verified files \| 1/.test(fixtureState) || !/verified bytes \| [1-9]/.test(fixtureState)) {
      throw new Error(`verified receipt did not refresh: ${fixtureState}`);
    }
  });
  await page.screenshot({path: output.replace(/\.json$/, '.png'), fullPage: true});
  result.browser.version = browser.version();
} catch (error) {
  result.errors.push(error.message);
} finally {
  result.passed = result.errors.length === 0 && result.flows.every(item => item.passed);
  await writeFile(output, JSON.stringify(result, null, 2) + '\n');
  await writeFile(output.replace(/\.json$/, '-ui-flow.json'), JSON.stringify(result, null, 2) + '\n');
  await context.tracing.stop({path: output.replace(/\.json$/, '-trace.zip')});
  await browser.close();
}

console.log(JSON.stringify(result));
if (!result.passed) process.exitCode = 1;
