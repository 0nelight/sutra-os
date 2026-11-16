
const { chromium } = require('playwright-core');

const BASE = process.argv[2];
const STREAM = process.argv[3];
const CHROMIUM = '/usr/bin/chromium';

function say(line) { process.stdout.write(line + '\n'); }

(async () => {
  const browser = await chromium.launch({
    executablePath: CHROMIUM,
    headless: true,
    args: ['--no-sandbox', '--disable-dev-shm-usage'],
  });
  let page = null;
  try {
    page = await browser.newPage();
    page.on('pageerror', (e) => say('PAGEERROR ' + e.message));
    page.on('console', (m) => { if (m.type() === 'error') say('CONSOLE ' + m.text()); });
    await page.goto(BASE + '/source-manager', { waitUntil: 'networkidle' });

    // the module header carries the tabs Quellen/Streams/Agenten (page/header + ui/tabs)
    await page.waitForSelector('.login__guest-link, .dataconn-tab--streams', { timeout: 20000 });
    const anon = page.locator('.login__guest-link');
    if (await anon.count()) { await anon.first().click(); }
    await page.waitForSelector('.dataconn-tab--streams', { timeout: 20000 });

    await page.locator('.dataconn-tab--streams').click({ timeout: 15000 });
    await page.locator('.streams-list__row', { hasText: STREAM }).click({ timeout: 15000 });

    await page.locator('.streams-detail .capp-l-header__title', { hasText: STREAM })
      .waitFor({ timeout: 15000 });
    await page.waitForSelector('.streams-pill--live', { timeout: 15000 });
    say('WATCHING');

    try {
      await page.waitForSelector('.streams-tail__row', { timeout: 20000 });
      await page.waitForSelector('.streams-detail__card--view table tbody tr', { timeout: 20000 });
      const row = await page.locator('.streams-tail__row').first().innerText();
      const view = await page.locator('.streams-detail__card--view table').innerText();
      await page.screenshot({ path: 'shots/streams-live.png', fullPage: true });
      say('SAW ' + JSON.stringify({ tail: row, view: view }));
    } catch (e) {
      say('MISSED ' + e.message);
    }
  } catch (e) {
    let where = '';
    if (page) {
      try {
        await page.screenshot({ path: 'shots/streams-live-failure.png', fullPage: true });
        where = (await page.locator('body').innerText()).slice(0, 800).replace(/\s+/g, ' ');
      } catch (_) { where = '(page unreadable)'; }
    }
    say('ERROR ' + e.message.split('\n')[0] + ' | page: ' + where);
  } finally {
    await browser.close();
  }
})();
