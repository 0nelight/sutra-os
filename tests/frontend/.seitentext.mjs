// .seitentext.mjs <token> <url> — the visible text of #app once the page has settled
import { chromium } from 'playwright-core';
const [tok, URL_] = process.argv.slice(2);
const b = await chromium.launch({ executablePath: '/usr/bin/chromium', args: ['--no-sandbox', '--disable-gpu'] });
const ctx = await b.newContext({ viewport: { width: 1600, height: 1000 }, locale: 'de-AT' });
await ctx.addCookies([{ name: 'sutra-session', value: tok, domain: new URL(URL_).hostname, path: '/', httpOnly: true, secure: false }]);
const p = await ctx.newPage();
const errs = [];
p.on('pageerror', e => errs.push(e.message.slice(0, 200)));
await p.goto(URL_, { waitUntil: 'domcontentloaded' });
await p.waitForTimeout(7000);
console.log(await p.evaluate(() => document.getElementById('app').innerText));
if (errs.length) console.log('ERRORS', errs);
await b.close();
