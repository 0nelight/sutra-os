// .kombi.mjs <token> <url> <placeholder> <term> — selects the first position group, opens
// its assignment dialog and types into the combo; nothing is submitted
import { chromium } from 'playwright-core';
const [tok, URL_, ph, term] = process.argv.slice(2);
const b = await chromium.launch({ executablePath: '/usr/bin/chromium', args: ['--no-sandbox', '--disable-gpu'] });
const ctx = await b.newContext({ viewport: { width: 1600, height: 1000 }, locale: 'de-AT' });
await ctx.addCookies([{ name: 'sutra-session', value: tok, domain: new URL(URL_).hostname, path: '/', httpOnly: true, secure: false }]);
const p = await ctx.newPage();
const errs = []; p.on('pageerror', e => errs.push(e.message.slice(0, 200)));
const calls = [];
p.on('request', r => { if (/\/(search|around)$/.test(r.url()))
  calls.push(r.url().split('/').pop() + ' ' + (r.postData() || '').slice(0, 160)); });
await p.goto(URL_, { waitUntil: 'domcontentloaded' });
await p.waitForTimeout(5000);
await p.locator('table').filter({ has: p.locator('th', { hasText: 'Kommission' }) }).first().locator('tbody tr').first().locator('td').nth(2).click();
await p.waitForTimeout(2500);
const z = p.getByRole('button', { name: 'Zuordnen…' });
console.log('Zuordnen buttons:', await z.count());
if (await z.count()) { await z.first().click(); await p.waitForTimeout(800); }
const inputs = await p.locator(`input[placeholder="${ph}"]`).count();
console.log('combo inputs:', inputs);
if (inputs) {
  const i = p.locator(`input[placeholder="${ph}"]`).first();
  await i.click(); await i.fill(term);
  await p.waitForTimeout(1500);
  console.log('list:', await p.locator('.capp-combo__list li').allInnerTexts());
}
console.log('calls:', calls);
if (errs.length) console.log('ERRORS', errs);
await b.close();
