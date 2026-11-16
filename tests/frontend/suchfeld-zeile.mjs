// Bedient das Suchfeld einer Schleifenzeile; scenario.aviation-suchfeld-test wertet die Ausgabe aus.
// Aufruf: node suchfeld-zeile.mjs <seiten-url> <basis-url> <token> <namen-json>
import { chromium } from 'playwright-core';

const [url, base, token, namenJson] = process.argv.slice(2);
const namen = JSON.parse(namenJson);
const b = await chromium.launch({
  executablePath: '/usr/bin/chromium', args: ['--no-sandbox', '--disable-gpu'] });
const raus = {};
try {
  const ctx = await b.newContext({ viewport: { width: 1400, height: 900 } });
  await ctx.addCookies([{ name: 'sutra-session', value: token, url: base }]);
  const p = await ctx.newPage();
  p.on('response', async (r) => {
    if (r.url().includes('/submit')) raus.absenden = r.status() + ' ' + (await r.text()).slice(0, 300);
    if (r.url().endsWith('/search')) raus.suche = r.request().postData() + ' => ' + r.status() + ' ' + (await r.text()).slice(0, 300);
  });
  await p.goto(url, { waitUntil: 'domcontentloaded' });
  const feld = p.locator('input.suchfeld').first();
  await feld.waitFor({ timeout: 30000 });
  const optionen = () => p.locator('.capp-portal [role="option"]').allTextContents();

  raus.vorbelegt = await feld.inputValue();

  await feld.evaluate((e) => { e.dataset.marke = 'dasselbe'; });
  await p.getByRole('button', { name: 'wählen' }).click();
  await p.getByText('gewählt', { exact: true }).waitFor({ timeout: 10000 });
  raus.gleichesFeld = await p.locator('input.suchfeld').first()
    .evaluate((e) => e.dataset.marke === 'dasselbe');

  await feld.click();
  await p.waitForTimeout(300);
  raus.beimFokus = await optionen();
  raus.listeObenauf = await p.locator('.capp-portal [role="option"]').first().evaluate((o) => {
    const l = o.parentElement;
    const r = l.getBoundingClientRect();
    const oben = document.elementFromPoint(r.x + r.width / 2, r.bottom - 5);
    return l.contains(oben);
  });

  const fehlt = namen.find((n) => !raus.beimFokus.includes(n));
  raus.ausserhalbDerSeite = fehlt ?? null;
  if (fehlt) {
    await feld.fill(fehlt.slice(0, 5));
    await p.locator('.capp-portal [role="option"]', { hasText: fehlt })
      .waitFor({ timeout: 5000 }).catch(() => {});
    raus.beimTippen = await optionen();
    raus.getippt = await feld.inputValue();
    if (raus.beimTippen.includes(fehlt)) {
      await p.locator('.capp-portal [role="option"]', { hasText: fehlt }).click();
      await p.waitForTimeout(1500);
      raus.nachDerWahl = await p.locator('input.suchfeld').first().inputValue();
    }
  }
} catch (e) {
  raus.fehler = String(e);
}
console.log('ERGEBNIS ' + JSON.stringify(raus));
await b.close();
