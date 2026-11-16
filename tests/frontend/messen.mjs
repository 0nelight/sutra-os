// messen.mjs <token> <workspace-slug> <app-slug>
//
// Öffnet eine App und liest aus, wohin die Zeit ging — die Phasen, die das Frontend
// selbst benannt hat (sutra-ui.debug/measure!, via window.__sutra_debug), neben den
// Netzwerkzahlen, die Playwright ohnehin sieht. NUR LESEND: kein Klick, kein Submit.
import { chromium } from 'playwright-core';

const [tok, ws, app] = process.argv.slice(2);
const B = process.env.SUTRA_HTTP_URL || 'http://localhost:8030';
const URL_ = `${B}/workspace/${encodeURIComponent(ws)}/app/${encodeURIComponent(app)}`;

const b = await chromium.launch({ executablePath: '/usr/bin/chromium',
                                  args: ['--no-sandbox', '--disable-gpu'] });
const ctx = await b.newContext({ viewport: { width: 1600, height: 1000 }, locale: 'de-AT' });
await ctx.addCookies([{ name: 'sutra-session', value: tok,
                        domain: new URL(B).hostname, path: '/', httpOnly: true, secure: false }]);
const p = await ctx.newPage();

const net = [], errs = [];
p.on('pageerror', e => errs.push('JS ' + e.message.slice(0, 160)));
p.on('console', m => { if (m.type() === 'error') errs.push('CONSOLE ' + m.text().slice(0, 160)); });
// Woraus eine Antwort BESTEHT: je Schlüssel der dekodierte Anteil. Eine Gesamtgröße
// sagt, dass es viel ist; erst das sagt, WAS viel ist.
const breakdown = (body) => {
  try {
    const o = JSON.parse(body);
    // transit ist gültiges JSON, aber eine Liste — nach Schlüsseln zählen ergäbe Indizes
    if (Array.isArray(o)) return { chars: body.length, note: 'transit — bitte als JSON anfordern' };
    const sz = (v) => JSON.stringify(v).length;
    const top = Object.entries(o).map(([k, v]) => [k, sz(v)]).sort((a, b) => b[1] - a[1]);
    const biggest = top[0] && o[top[0][0]] && typeof o[top[0][0]] === 'object'
      ? Object.entries(o[top[0][0]]).map(([k, v]) => [top[0][0] + '.' + k, sz(v)])
          .sort((a, b) => b[1] - a[1]).slice(0, 12)
      : [];
    return { chars: body.length, top: top.slice(0, 8), inner: biggest };
  } catch { return { chars: body.length, note: 'kein JSON (transit?)' }; }
};

p.on('response', async r => {
  const u = r.url();
  if (!/\/api\//.test(u)) return;
  const t = r.request().timing();
  let len = r.headers()['content-length'] || null;
  const row = { url: u.replace(B, ''), status: r.status(),
                ttfb: t ? Math.round(t.responseStart - t.requestStart) : null,
                enc: r.headers()['content-encoding'] || '-',
                bytes: len ? +len : null };
  if (/\/api\/my-application\//.test(u)) {
    try {
      const body = await r.text();
      row.parts = breakdown(body);
      if (process.env.DUMP_DIR) {
        const { writeFileSync } = await import('fs');
        const asked = new URL(u).searchParams.get('data') || 'all';
        const part = /^[a-z]+$/.test(asked) ? asked : 'other';   // Dateiname aus der URL
        writeFileSync(`${process.env.DUMP_DIR}/door-${part}.txt`, body);
      }
    } catch (e) { row.parts = { err: String(e).slice(0, 80) }; }
  }
  net.push(row);
});

const t0 = Date.now();
await p.goto(URL_, { waitUntil: 'domcontentloaded' });

// Warten, bis das Frontend selbst sagt, dass es fertig geladen hat — und zwar an den
// Phasen, die es als `settled` DEKLARIERT (sutra.observe/settled-phases). Vorher stand hier
// ein Regex auf Phasennamen, die in vier .cljs-Dateien als String-Literale lagen: ein
// Umbenennen dort, und dieses Skript wartete still 60s auf etwas, das es nie gab.
await p.waitForFunction(
  () => { try { const dbg = window.__sutra_debug;
                if (!dbg) return false;
                const settled = dbg.settled || [];
                return dbg.dump().some(e => e.level === 'perf' && settled.includes(e.src)); }
          catch { return false; } },
  { timeout: 60000 }).catch(() => console.log('(keine settled-Phase innerhalb 60s)'));
await p.waitForTimeout(2500);
const wall = Date.now() - t0;

const perf = await p.evaluate(() =>
  (window.__sutra_debug ? window.__sutra_debug.dump() : [])
    .filter(e => e.level === 'perf')
    .map(e => ({ t: e.t, phase: e.src, msg: e.msg })));

// die Phasen als Bus-EREIGNISSE, mit Span: das ist, was der Server sich auf seine eigene
// Zeitachse holt, also auch das, womit man hier eine Anfrage über die Prozessgrenze verfolgt
const events = await p.evaluate(() =>
  (window.__sutra_debug && window.__sutra_debug.events) ? window.__sutra_debug.events() : []);

console.log(JSON.stringify({ url: URL_, wall_ms: wall, perf, events, net,
                             errors: [...new Set(errs)].slice(0, 6) }, null, 1));
await b.close();
