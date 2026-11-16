// Spielt eine Szene ab, die szene.clj aus EDN normalisiert hat, und nimmt sie auf.
//   node szene.mjs <plan.json>
// Jeder Schritt ist [op, ...args]; Ziele sind {text|css|label|role,name, in, nth, exact}.
import { chromium } from 'playwright-core';
import { readFileSync, mkdirSync, readdirSync, unlinkSync, renameSync } from 'fs';
import { zeigerImSeite, fahreZu, klicke } from './film-kern.mjs';

const plan = JSON.parse(readFileSync(process.argv[2], 'utf8'));
const { base, out, name, viewport: [w, h], beat = 600, travel = 22, typeDelay = 75 } = plan;
mkdirSync(out, { recursive: true });
for (const f of readdirSync(out)) if (f.endsWith('.webm')) unlinkSync(`${out}/${f}`);

const b = await chromium.launch({ executablePath: '/usr/bin/chromium', args: ['--no-sandbox', '--disable-gpu'] });
const ctx = await b.newContext({
  viewport: { width: w, height: h }, locale: plan.locale || 'en-US',
  recordVideo: { dir: out, size: { width: w, height: h } },
});
const t0 = Date.now();
await ctx.addInitScript(zeigerImSeite);
if (plan.css) await ctx.addInitScript((css) => {
  const put = () => { const s = document.createElement('style'); s.textContent = css; document.head.appendChild(s); };
  document.head ? put() : addEventListener('DOMContentLoaded', put);
}, plan.css);
const p = await ctx.newPage();
p.on('pageerror', (e) => console.log('  page error:', String(e).slice(0, 160)));

const ziel = (t) => {
  let l = t.in ? p.locator(t.in) : p;
  if (t.css) l = l.locator(t.css);
  else if (t.label) l = l.getByLabel(t.label, { exact: !!t.exact });
  else if (t.role) l = l.getByRole(t.role, { name: t.name, exact: !!t.exact });
  else if (t.text) l = (t.in ? l.locator(`text=${t.text}`) : l.getByText(t.text, { exact: !!t.exact }));
  if (t.has) l = l.filter({ hasText: t.has });
  return t.nth === undefined ? l.first() : l.nth(t.nth);
};

const overlay = (id, html, css) => p.evaluate(([id, html, css]) => {
  let e = document.getElementById(id);
  if (html === null) { e?.remove(); return; }
  if (!e) { e = document.createElement('div'); e.id = id; document.body.appendChild(e); }
  e.style.cssText = css; e.innerHTML = html;
}, [id, html, css]);

const esc = (s) => String(s).replace(/[&<>]/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;' }[c]));
let caption = null;
const zeigeCaption = async () => caption && overlay('szene-caption', esc(caption),
  'position:fixed;left:50%;bottom:48px;transform:translateX(-50%);z-index:2147483646;max-width:80%;' +
  'padding:14px 26px;border-radius:12px;background:rgba(17,17,17,.88);color:#fff;text-align:center;' +
  'font:600 26px/1.3 system-ui,-apple-system,sans-serif;box-shadow:0 8px 30px rgba(0,0,0,.3);pointer-events:none');

const ops = {
  async goto(path) {
    await p.goto(base + path, { waitUntil: 'domcontentloaded' });
    await p.waitForLoadState('load').catch(() => {});
    await p.waitForTimeout(plan.settle ?? 2500);
  },
  async wait(ms) { await p.waitForTimeout(ms); },
  async caption(text) { caption = text; if (text === null) await overlay('szene-caption', null); },
  async card({ title, sub, code, ms = 3000 }) {
    await overlay('szene-card',
      `<div style="font:800 54px/1.15 system-ui,sans-serif;letter-spacing:-.02em">${esc(title || '')}</div>` +
      (sub ? `<div style="margin-top:22px;font:400 28px/1.4 system-ui,sans-serif;opacity:.85">${esc(sub)}</div>` : '') +
      (code ? `<pre style="margin-top:30px;text-align:left;display:inline-block;padding:22px 28px;border-radius:12px;` +
              `background:#0b1020;color:#c9d7ff;font:500 21px/1.45 ui-monospace,Menlo,monospace">${esc(code)}</pre>` : ''),
      'position:fixed;inset:0;z-index:2147483647;display:flex;flex-direction:column;align-items:center;' +
      'justify-content:center;text-align:center;padding:60px;background:#111827;color:#fff');
    await p.waitForTimeout(ms);
    await overlay('szene-card', null);
  },
  async click(t, o = {}) {
    const el = ziel(t);
    await el.waitFor({ state: 'visible', timeout: 15000 });
    await el.scrollIntoViewIfNeeded();
    await klicke(p, el, o.travel ?? travel);
    if (o.settle) await p.waitForTimeout(o.settle);
  },
  async hover(t, o = {}) {
    const el = ziel(t);
    await el.scrollIntoViewIfNeeded();
    const k = await el.boundingBox();
    if (k) await fahreZu(p, k.x + k.width / 2, k.y + k.height / 2, o.travel ?? travel);
  },
  async type(t, text, o = {}) {
    await ops.click(t);
    const el = ziel(t);
    if (o.clear !== false) await el.fill('');
    // Nicht-ASCII verschluckt das headless Chromium beim Tippen.
    if (/^[\x20-\x7e]*$/.test(text)) await p.keyboard.type(text, { delay: o.delay ?? typeDelay });
    else await el.fill(text);
  },
  async select(t, label) {
    await ops.click(t);
    await ziel(t).selectOption({ label });
  },
  async press(key) { await p.keyboard.press(key); },
  async highlight(t, o = {}) {
    const el = ziel(t);
    await el.scrollIntoViewIfNeeded();
    const k = await el.boundingBox();
    if (!k) return;
    const pad = o.pad ?? 8;
    await overlay('szene-ring', '',
      `position:fixed;z-index:2147483645;pointer-events:none;border:4px solid ${o.color || '#f59e0b'};` +
      `border-radius:12px;box-shadow:0 0 0 9999px rgba(0,0,0,${o.dim ?? 0.35});` +
      `left:${k.x - pad}px;top:${k.y - pad}px;width:${k.width + 2 * pad}px;height:${k.height + 2 * pad}px`);
    await p.waitForTimeout(o.ms ?? 1800);
    await overlay('szene-ring', null);
  },
  async scroll(px) { await p.mouse.wheel(0, px); await p.waitForTimeout(500); },
  async shot(file) { await p.screenshot({ path: `${out}/${file}.png` }); },
};

let start = null;
for (const [i, [op, ...args]] of plan.steps.entries()) {
  if (!ops[op]) throw new Error(`step ${i}: unknown op ${op}`);
  if (start === null && op !== 'goto') start = Date.now() - t0;
  try {
    await ops[op](...args);
  } catch (e) {
    await p.screenshot({ path: `${out}/failed-step-${i}.png` }).catch(() => {});
    console.error(`  step ${i} ${JSON.stringify([op, ...args]).slice(0, 140)}\n  ${String(e.message).split('\n')[0]}`);
    process.exitCode = 1;
    break;
  }
  await zeigeCaption().catch(() => {});
  if (!['wait', 'caption', 'shot'].includes(op)) await p.waitForTimeout(beat);
  console.log(`  ${String(i).padStart(3)} ${op} ${JSON.stringify(args).slice(0, 90)}`);
}

const roh = await p.video().path();
await ctx.close();
await b.close();
renameSync(roh, `${out}/${name}.webm`);
console.log(JSON.stringify({ webm: `${out}/${name}.webm`, trim: (start ?? 0) / 1000 }));
