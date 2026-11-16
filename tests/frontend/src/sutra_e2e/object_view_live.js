// An object page whose view declares :live: a change to a watched type shows without a reload.
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
    let loads = 0;
    const watched = [];
    page.on('pageerror', (e) => say('PAGEERROR ' + e.message));
    page.on('load', () => { loads += 1; });
    page.on('websocket', (ws) => {
      if (ws.url().includes('/api/ontology/watch')) {
        ws.on('framesent', (f) => watched.push(f.payload));
      }
    });
    await page.goto(BASE + PATH, { waitUntil: 'load' });
    const ringing = page.getByText('ringing').first();
    const anon = page.locator('.login__guest-link');
    await anon.or(ringing).first().waitFor({ timeout: 30000 });
    if (await anon.count()) { await anon.first().click(); }
    await ringing.waitFor({ timeout: 30000 });
    const before = loads;
    for (let i = 0; i < 100 && !watched.length; i += 1) { await page.waitForTimeout(100); }
    say('WATCHING ' + watched.join(' '));
    try {
      await page.getByText('answered-live').first().waitFor({ timeout: 30000 });
      say('SAW ' + JSON.stringify({ reloads: loads - before }));
    } catch (e) {
      say('MISSED ' + e.message.split('\n')[0]);
    }
  } catch (e) {
    const where = page ? (await page.locator('body').innerText()).slice(0, 600) : '';
    say('ERROR ' + e.message.split('\n')[0] + ' | page: ' + where.replace(/\s+/g, ' '));
  } finally {
    await browser.close();
  }
})();
