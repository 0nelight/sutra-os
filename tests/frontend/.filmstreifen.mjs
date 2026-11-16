// .filmstreifen.mjs <token> <url> — every distinct visible state of #app while a page loads
import { chromium } from 'playwright-core';

const [tok, URL_] = process.argv.slice(2);
const B = new URL(URL_).origin;
const b = await chromium.launch({ executablePath: '/usr/bin/chromium',
                                  args: ['--no-sandbox', '--disable-gpu'] });
const ctx = await b.newContext({ viewport: { width: 1600, height: 1000 }, locale: 'de-AT' });
await ctx.addCookies([{ name: 'sutra-session', value: tok, domain: new URL(B).hostname,
                        path: '/', httpOnly: true, secure: false }]);
await ctx.addInitScript(() => {
  window.__film = [];
  let last = null;
  const sig = () => {
    const app = document.getElementById('app');
    if (!app) return 'no #app';
    const cls = [...app.querySelectorAll('*')].slice(0, 400)
      .map(e => (e.className && typeof e.className === 'string') ? e.className.split(' ')[0] : '')
      .filter(c => /loading|spinner|shell|capp-|empty|boot|objectview|pdf|login/.test(c));
    const uniq = [...new Set(cls)].slice(0, 12).join(',');
    const txt = (app.innerText || '').replace(/\s+/g, ' ').trim().slice(0, 70);
    return uniq + ' | ' + txt;
  };
  const tick = () => {
    const s = sig();
    const full = location.search + ' ' + s;
    if (full !== last) { last = full;
      window.__film.push([Math.round(performance.now()), decodeURIComponent(location.search) + ' :: ' + s]); }
    requestAnimationFrame(tick);
  };
  requestAnimationFrame(tick);
});
const p = await ctx.newPage();
await p.goto(URL_, { waitUntil: 'domcontentloaded' });
await p.waitForTimeout(4000);
if (process.env.ZWEIMAL) { await p.goto(URL_, { waitUntil: 'domcontentloaded' });
                           await p.waitForTimeout(4000); }
const film = await p.evaluate(() => window.__film);
for (const [t, s] of film) console.log(String(t).padStart(5), s);
const res = await p.evaluate(() => performance.getEntriesByType('resource')
  .filter(r => /\/api\/my-app|main\.|\.css/.test(r.name))
  .map(r => [Math.round(r.startTime), Math.round(r.responseEnd), r.transferSize,
             r.name.replace(location.origin, '').slice(0, 60)]));
for (const r of res) console.log('   res', r.join(' '));
await b.close();
