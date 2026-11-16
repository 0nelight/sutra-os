// An object page with its own view, opened cold while the catalog is slow: the own view stands
// before /api/snapshot answers, and neither that answer nor a later catalog replaces its DOM.
const { chromium } = require('playwright-core');

const BASE = process.argv[2];
const PATH = process.argv[3];
const OWN = process.argv[4];

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
    page.on('pageerror', (e) => say('PAGEERROR ' + e.message));
    let bodyFetches = 0;
    let release = () => {};
    const held = new Promise((resolve) => { release = resolve; });
    page.on('request', (r) => { if (r.url().includes('/api/schema/by-rid/')) bodyFetches += 1; });
    await page.route('**/api/snapshot*', async (route) => {
      await held;
      await route.continue();
    });
    await page.goto(BASE + PATH, { waitUntil: 'load' });
    const own = page.getByText(OWN).first();
    const guest = page.locator('.login__guest-link');
    await guest.or(own).first().waitFor({ timeout: 30000 });
    if (await guest.count()) { await guest.first().click(); }
    await own.waitFor({ timeout: 30000 });
    say('BEFORE-CATALOG');
    const node = await own.elementHandle();
    release();
    await page.waitForLoadState('networkidle');
    await page.waitForTimeout(1000);
    const connected = await node.evaluate((e) => e.isConnected);
    say('SETTLED ' + JSON.stringify({ connected, bodyFetches }));
  } catch (e) {
    const where = page ? (await page.locator('body').innerText()).slice(0, 600) : '';
    say('ERROR ' + e.message.split('\n')[0] + ' | page: ' + where.replace(/\s+/g, ' '));
  } finally {
    await browser.close();
  }
})();
