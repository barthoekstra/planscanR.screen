// Run against inst/curation/acq_ux1_browser_app.R; never point at production.
// PLAYWRIGHT_MODULE may specify an existing local Playwright module.
import {writeFile, mkdir} from 'node:fs/promises';
const {chromium} = await import(process.env.PLAYWRIGHT_MODULE || 'playwright');
const output = process.argv[2] || '/tmp/acq-ux1-browser.json';
const browser = await chromium.launch({headless:true, args:['--enable-precise-memory-info']});
const context = await browser.newContext();
const page = await context.newPage();
const errors = [];
page.on('pageerror', error => errors.push(error.message));
let receivedBytes = 0;
page.on('websocket', socket => socket.on('framereceived', event => {
  receivedBytes += Buffer.byteLength(event.payload);
}));
const cdp = await context.newCDPSession(page);
await cdp.send('Performance.enable');
const actions = [];
async function settled() {
  await page.waitForFunction(()=>![...document.querySelectorAll('.recalculating')].some(x=>x.getClientRects().length) && !document.documentElement.classList.contains('shiny-busy'),null,{timeout:120000});
  await page.waitForTimeout(500);
  await page.waitForFunction(()=>![...document.querySelectorAll('.recalculating')].some(x=>x.getClientRects().length) && !document.documentElement.classList.contains('shiny-busy'),null,{timeout:120000});
}
async function measure(label, action, ready) {
  const bytesBefore = receivedBytes;
  const start = performance.now();
  await action();
  await ready();
  await settled();
  const seconds = (performance.now()-start)/1000;
  const metrics = (await cdp.send('Performance.getMetrics')).metrics;
  const memory = Object.fromEntries(metrics.filter(x=>['JSHeapUsedSize','JSHeapTotalSize','Nodes','Documents'].includes(x.name)).map(x=>[x.name,x.value]));
  const state = await page.evaluate(()=>{
    let table={}; try {table=window.Reactable?.getState('review_tbl')||{};} catch {}
    return ({
    total:document.querySelector('#vb_total')?.textContent,
    rows:document.querySelector('#review_info')?.textContent,
    page:table.page,
    pageSize:table.pageSize,
    domRows:document.querySelectorAll('#review_tbl .rt-tr-group').length,
    selected:document.querySelector('#vb_selected')?.textContent,
    recalculating:!![...document.querySelectorAll('.recalculating')].some(x=>x.getClientRects().length)
  });});
  actions.push({label,seconds,received_websocket_bytes:receivedBytes-bytesBefore,browser_memory:memory,state});
  console.log(label,seconds.toFixed(3),state.rows||state.total);
  await writeFile(output,JSON.stringify({schema:'acq-ux1-browser.v1',status:'running',actions,errors},null,2)+'\n');
}
const landingReady=()=>page.waitForFunction(()=>document.querySelector('#vb_total')?.textContent==='128,000',null,{timeout:90000});
await measure('cold_landing',()=>page.goto('http://127.0.0.1:43819'),landingReady);
await measure('warm_landing',()=>page.reload(),landingReady);
const reviewReady=()=>page.waitForFunction(()=>document.querySelector('#review_info')?.textContent?.includes('5000'),null,{timeout:90000});
await measure('first_review_tab',async()=>{
  await page.getByRole('tab',{name:'Review',exact:true}).click();
  await page.evaluate(()=>{const x=$('#stage_filter')[0].selectize; x.setValue('all');});
},reviewReady);

await measure('repeat_review_tab',async()=>{
  await page.getByRole('tab',{name:'Overview',exact:true}).click();
  await page.getByRole('tab',{name:'Review',exact:true}).click();
},reviewReady);
await measure('first_page',()=>page.locator('#review_tbl .rt-next-button').click(),()=>page.waitForFunction(()=>window.Reactable.getState('review_tbl').page===1));
await measure('repeat_page',async()=>{await page.locator('#review_tbl .rt-prev-button').click();await page.locator('#review_tbl .rt-next-button').click();},()=>page.waitForFunction(()=>window.Reactable.getState('review_tbl').page===1));
const query=(await page.locator('#review_tbl').innerText()).match(/Energy planning record \d+/)[0];
const searchInput=page.locator('#review_tbl input[placeholder="Search"]');
await measure('first_filter',()=>searchInput.fill(query),()=>page.waitForFunction(()=>document.querySelector('#review_tbl')?.textContent?.includes('1–1')));
await measure('repeat_filter',async()=>{await searchInput.fill('');await searchInput.fill(query);},()=>page.waitForFunction(()=>document.querySelector('#review_tbl')?.textContent?.includes('1–1')));
await searchInput.fill('');
await settled();
async function threshold(value) {
  await page.evaluate(v=>{ window.__ux1TableReceived=false;$(document).off('shiny:value.ux1').on('shiny:value.ux1',(event)=>{if(event.name==='review_tbl') window.__ux1TableReceived=true;});Shiny.setInputValue('threshold',v,{priority:'event'});},value);
  await page.waitForFunction(()=>window.__ux1TableReceived,null,{timeout:120000});
  await settled();
}
await measure('first_threshold',()=>threshold(0.7),reviewReady);
await measure('repeat_threshold',async()=>{await threshold(0.5);await threshold(0.7);},reviewReady);
await mkdir(output.substring(0,output.lastIndexOf('/')) || '.',{recursive:true});
await page.screenshot({path:output.replace(/\.json$/,'.png'),fullPage:true});
await writeFile(output,JSON.stringify({schema:'acq-ux1-browser.v1',browser:browser.version(),fixture_rows:128000,scope:'loopback disposable synthetic fixture; no signed-in production session',actions,errors},null,2)+'\n');
await browser.close();
console.log(output);
