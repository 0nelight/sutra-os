// A watched set only a hidden page uses: a change waits until that page is shown.
const { chromium } = require('playwright-core');

const BASE = process.argv[2];
const PATH = process.argv[3];

function say(line) { process.stdout.write(line + '\n'); }

(async () => {
  const browser = await chromium.launch({
    executablePath: '/usr/bin/chromium',
    headless: true,
    args: ['--no-sandbox', '--disable-dev-shm-usage'],
  });
  let page = null;
  try {
    page = await browser.newPage();
    let reads = 0;
    let lastRead = Date.now();
    const watched = [];
    page.on('pageerror', (e) => say('PAGEERROR ' + e.message));
    page.on('console', (m) => { if (m.type() === 'error') say('CONSOLE ' + m.text().slice(0, 500)); });
    // the browser sending its own log is not the page reading data
    page.on('request', (r) => {
      if (r.url().includes('/api/') && !r.url().includes('/api/client-log')) {
        reads += 1;
        lastRead = Date.now();
      }
    });
    page.on('websocket', (ws) => {
      if (ws.url().includes('/api/ontology/watch')) {
        ws.on('framesent', (f) => watched.push(f.payload));
      }
    });
    await page.goto(BASE + PATH, { waitUntil: 'load' });
    const first = page.getByText('erste seite').first();
    const anon = page.locator('.login__guest-link');
    await anon.or(first).first().waitFor({ timeout: 30000 });
    if (await anon.count()) { await anon.first().click(); }
    await first.waitFor({ timeout: 30000 });
    for (let i = 0; i < 100 && !watched.length; i += 1) { await page.waitForTimeout(100); }
    // the builder comes after the page and reads what it shows; the window opens once it
    // stands and nothing has been asked for a second (a second networkidle wait returns at
    // once: the state was reached on the way in)
    await page.locator('.shell-bar__search').first().waitFor({ timeout: 30000 });
    for (let i = 0; i < 150 && Date.now() - lastRead < 1000; i += 1) {
      await page.waitForTimeout(100);
    }
    const before = reads;
    say('WATCHING ' + watched[watched.length - 1]);
    await page.waitForTimeout(6000);
    say('QUIET ' + JSON.stringify({ reads: reads - before }));
    const atClick = reads;
    await page.getByText('Zweite', { exact: true }).first().click();
    await page.getByText('answered-live').first().waitFor({ timeout: 30000 });
    say('SAW ' + JSON.stringify({ reads: reads - atClick }));
  } catch (e) {
    const where = page ? (await page.locator('body').innerText()).slice(0, 600) : '';
    say('ERROR ' + e.message.split('\n')[0] + ' | page: ' + where.replace(/\s+/g, ' '));
  } finally {
    await browser.close();
  }
})();
