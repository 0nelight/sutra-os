// .cpuprofil.mjs <token> <url> <out.json> — CPU profile of a warm second load, plus timeline marks
import { chromium } from 'playwright-core';
import { writeFileSync } from 'fs';
const [tok, URL_, out] = process.argv.slice(2);
const b = await chromium.launch({ executablePath: '/usr/bin/chromium', args: ['--no-sandbox', '--disable-gpu'] });
const ctx = await b.newContext({ viewport: { width: 1600, height: 1000 }, locale: 'de-AT' });
await ctx.addCookies([{ name: 'sutra-session', value: tok, domain: new URL(URL_).hostname, path: '/', httpOnly: true, secure: false }]);
await ctx.addInitScript(() => {
  window.__lt = [];
  new PerformanceObserver(l => l.getEntries().forEach(e => window.__lt.push([Math.round(e.startTime), Math.round(e.duration)]))).observe({ type: 'longtask', buffered: true });
  const tick = () => { if (!window.__app && document.querySelector('.capp-l-app')) window.__app = Math.round(performance.now()); requestAnimationFrame(tick); };
  requestAnimationFrame(tick);
});
const p = await ctx.newPage();
await p.goto(URL_, { waitUntil: 'load' }); await p.waitForTimeout(3000);
const cdp = await ctx.newCDPSession(p);
await cdp.send('Profiler.enable'); await cdp.send('Profiler.setSamplingInterval', { interval: 200 });
await cdp.send('Profiler.start');
await p.goto(URL_, { waitUntil: 'load' }); await p.waitForTimeout(2500);
const { profile } = await cdp.send('Profiler.stop');
const info = await p.evaluate(() => ({
  app: window.__app, longtasks: window.__lt,
  nav: (({ responseEnd, domInteractive, domContentLoadedEventEnd, loadEventEnd }) => ({ responseEnd, domInteractive, domContentLoadedEventEnd, loadEventEnd }))(performance.getEntriesByType('navigation')[0]),
  res: performance.getEntriesByType('resource').filter(r => /main\.|my-application|\.css/.test(r.name)).map(r => [r.name.replace(location.origin, '').slice(0, 50), Math.round(r.startTime), Math.round(r.responseEnd)])
}));
writeFileSync(out, JSON.stringify({ info, profile }));
console.log(JSON.stringify(info, null, 1).slice(0, 3000));
await b.close();
