// readme-shots.mjs <base-url> <out-dir> <lot-id> <lineage-graph-rid> <model-graph-rid> [lang]
//
// The README's screenshots, taken from a running instance that bin/readme-media.clj seeded.
// Fixed viewport, scale, locale (?lang=<lang>, default en), timezone and colour scheme.
// In another language the farm's own words come from media/hero/farm/<lang>.edn, the
// dictionary the instance was seeded through, and the UI's from its <lang> bundles. Every wait names
// what must be on screen; a missing view or element throws, and the run exits non-zero.
// Each shot is cropped to one element; floating chrome is hidden by CSS injected here only.
import { chromium } from 'playwright-core';
import { readFileSync, readdirSync, existsSync } from 'node:fs';

const [base, out, lotId, lineageRid, modelRid, lang = 'en'] = process.argv.slice(2);
if (!modelRid) { console.error('usage: readme-shots.mjs <base> <out> <lot-id> <lineage-rid> <model-rid>'); process.exit(2); }

const src = new URL('../../clients/frontend/src/', import.meta.url);
// the shell's texts, then every app's own bundle beside its sources
const bundle = [new URL(`i18n/ui.${lang}.edn`, src),
            ...readdirSync(new URL('sutra_ui/apps/', src), { withFileTypes: true })
              .filter(d => d.isDirectory())
              .map(d => new URL(`sutra_ui/apps/${d.name}/i18n.${lang}.edn`, src))
              .filter(u => existsSync(u))]
  .map(u => readFileSync(u, 'utf8')).join('\n');
const t = k => {
  const m = bundle.match(new RegExp(`:ui/${k} "([^"]*)"`));
  if (!m) throw new Error(`no ${lang} text for :ui/${k}`);
  return m[1];
};
// The farm's data and model strings in this language; English is the source.
const dict = lang === 'en' ? null : Object.fromEntries(
  [...readFileSync(new URL(`../../media/hero/farm/${lang}.edn`, import.meta.url), 'utf8')
     .replace(/^\s*;.*$/gm, '')
     .matchAll(/"((?:[^"\\]|\\.)*)"\s+"((?:[^"\\]|\\.)*)"/g)].map(m => [m[1], m[2]]));
const d = s => {
  if (!dict) return s;
  if (!(s in dict)) throw new Error(`no ${lang} text for "${s}" in media/hero/farm/${lang}.edn`);
  return dict[s];
};
const locale = { en: 'en-US', de: 'de-AT' }[lang] || lang;
const esc = s => s.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');

// Hidden before the view settles (so the canvas fits the full width) and after it
// (the controls the script itself clicks).
const HIDE_EARLY = `
  .assistant-fab, .viewnote-fab, .shell-bar__search, .graph-node__expand-slot, .rail, .lineage-seed, .lineage-legend,
  .react-flow__attribution, .react-flow__minimap,
  .react-flow__edgelabel-renderer button, .react-flow__node button { display: none !important; }
  .react-flow__edgelabel-renderer div:has(> button) { border: 0 !important; background: none !important; box-shadow: none !important; }
`;
const HIDE_LATE = `.canvas-topbar, .react-flow__controls { display: none !important; }`;

const b = await chromium.launch({ executablePath: process.env.CHROMIUM || '/usr/bin/chromium',
                                  args: ['--no-sandbox', '--disable-gpu', '--font-render-hinting=none'] });

// trim: a selector whose bottom edge ends the shot, so a view that fills the window
// is not followed by empty space.
// bare: the platform's own page as it is — nothing injected, nothing hidden by this script.
const shot = async (name, { width, height, bare }, path, ready, target, trim) => {
  const ctx = await b.newContext({ viewport: { width, height }, deviceScaleFactor: 1.6, locale,
                                   timezoneId: 'UTC', colorScheme: 'light', reducedMotion: 'reduce' });
  const p = await ctx.newPage();
  const errs = [];
  p.on('pageerror', e => errs.push(e.message));
  await p.goto(`${base}${path}${path.includes('?') ? '&' : '?'}lang=${lang}`, { waitUntil: 'load' });
  if (!bare) await p.addStyleTag({ content: HIDE_EARLY });
  await ready(p);
  if (!bare) await p.addStyleTag({ content: HIDE_LATE });
  // A dialog's rounded corners show what is behind it; behind it there is nothing, so the
  // PNG stays transparent there and fits a light or a dark page.
  if (target === '.capp-modal')
    await p.addStyleTag({ content: `.bp-overlay-backdrop, .shell { visibility: hidden !important; }
      html, body { background: transparent !important; }` });
  await p.waitForTimeout(800);
  if (errs.length) throw new Error(`${name}: page errors: ${errs.join(' | ')}`);
  if (typeof target === 'function') {
    await p.screenshot({ path: `${out}/${name}.png`, clip: await target(p) });
    console.log(`${out}/${name}.png`);
    await ctx.close();
    return;
  }
  const el = p.locator(target).first();
  await el.waitFor({ timeout: 10000 });
  if (trim) {
    const box = await el.boundingBox();
    const end = await p.locator(trim).last().boundingBox();
    if (!box || !end) throw new Error(`${name}: nothing to trim to`);
    await p.screenshot({ path: `${out}/${name}.png`,
                         clip: { x: box.x, y: box.y, width: box.width, height: end.y + end.height + 28 - box.y } });
  } else {
    await el.screenshot({ path: `${out}/${name}.png`, omitBackground: true });
  }
  console.log(`${out}/${name}.png`);
  await ctx.close();
};

const lotView = async p => {
  await p.getByText('S-2026-007', { exact: true }).waitFor({ timeout: 30000 });
  await p.getByText('tom', { exact: true }).waitFor({ timeout: 10000 });
};
const sellHay = async (p, bales, price) => {
  await lotView(p);
  await p.getByRole('button', { name: t('actions-menu') }).click();
  await p.locator('.bp-menu-item', { hasText: d('Sell hay') }).click();
  const field = label => p.locator('.capp-actionform__row', { hasText: new RegExp(`^\\s*${esc(d(label))}`) })
    .locator('input');
  await field('Bales').fill(bales);
  await field('Buyer').fill(d('Neighbour'));
  await field('Sale no.').fill('S-2026-009');
  if (price) await field('Price / bale').fill(price);
  await field('Sold on').fill('2026-02-14');
  await p.locator('.capp-modal .bp-dialog-header').click(); // closes the date picker
  await p.locator('.explorer-action__subtitle', { hasText: d('North Meadow, 1st cut 2025') }).waitFor({ timeout: 5000 });
};

// The bounding box of every graph node plus a margin: a shot that fills with the graph.
const nodesClip = async p => {
  const r = await p.locator('.react-flow__node').evaluateAll(ns => ns.map(n => n.getBoundingClientRect())
    .reduce((a, b) => ({ l: Math.min(a.l, b.left), t: Math.min(a.t, b.top),
                         r: Math.max(a.r, b.right), b: Math.max(a.b, b.bottom) }),
            { l: 1e9, t: 1e9, r: -1e9, b: -1e9 }));
  const pad = 36;
  return { x: r.l - pad, y: r.t - pad, width: r.r - r.l + 2 * pad, height: r.b - r.t + 2 * pad };
};

// Until the laid-out nodes stop moving: the layout lands, then the view fits to it.
const settled = async p => {
  let last = '';
  for (let i = 0; i < 50; i++) {
    const now = JSON.stringify(await p.locator('.react-flow__node').evaluateAll(
      ns => ns.map(n => { const r = n.getBoundingClientRect(); return [r.x, r.y, r.width]; })));
    if (now === last) return;
    last = now;
    await p.waitForTimeout(400);
  }
  throw new Error('graph never settled');
};

const fit = async (p, nodes) => {
  await p.waitForFunction(n => document.querySelectorAll('.react-flow__node').length >= n, nodes, { timeout: 30000 });
  await p.locator('.react-flow__controls-fitview').click();
  await p.waitForTimeout(600);
};

try {
  // The hero: the whole farm in Data Lineage, from the baler's log through the transform to
  // the object types and the actions allowed to change them. The embedded view, in the
  // layout the graph is saved to open in (`lineage-graph arrange`), as the platform draws it.
  await shot('model-farm', { width: 1600, height: 640, bare: true },
    `/view/${modelRid}/data-lineage?embedded=true`, async p => {
      for (const label of ['baling_log', d('Sell hay'), d('Harvest'), d('Buyer')])
        await p.locator('.react-flow__node', { hasText: label }).first().waitFor({ timeout: 30000 });
      await settled(p);
    }, nodesClip);

  // "Every number has a past": the lot and its sales, back to the baler's log.
  await shot('lineage-farm', { width: 1200, height: 520 },
    `/view/${lineageRid}/data-lineage`, async p => {
      // as the reference design shows it: the colour legend, and each node's ‹ › expand chevrons
      await p.addStyleTag({ content: `.canvas-surface > .lineage-legend { display: grid !important; }
        .canvas-surface .react-flow__node .graph-node__expand-slot,
        .canvas-surface .react-flow__node .graph-node__expand { display: flex !important; }
        .canvas-surface .react-flow__node .graph-node__rel { display: flex !important; }` });
      await fit(p, 3);
      await p.locator('.react-flow__node', { hasText: 'baling_log' }).waitFor({ timeout: 10000 });
    }, '.canvas-surface');

  // Object types: one hay lot as the Object View shows it, at the README's column width.
  await shot('object-view-lot', { width: 860, height: 900 },
    `/object/lot/${lotId}`, lotView, '.main', '.capp-l-section');

  // Object types: every type, and the hay lots as a table. Wide enough that the pane stays
  // above the phone line (700px), where the table becomes cards.
  await shot('objects-lots', { width: 1280, height: 600 },
    `/object-workspace/lot`, async p => {
      await p.getByText(d('Long Acre, 1st cut 2025'), { exact: true }).waitFor({ timeout: 30000 });
    }, '.capp--data', '.capp-table');

  // Link types: the same lot as a graph, its sales on one side and its field on the other.
  await shot('object-graph-lot', { width: 1000, height: 640 },
    `/object/lot/${lotId}`, async p => {
      await lotView(p);
      await p.getByRole('button', { name: t('ov-explore-graph') }).click();
      await fit(p, 5);
      for (const label of [d('North Meadow'), 'S-2025-019', 'S-2026-002', 'S-2026-007'])
        await p.locator('.react-flow', { hasText: label }).waitFor({ timeout: 10000 });
    }, '.canvas-surface');

  // Nothing happens until you say so: a sale the barn can take, previewed, not run.
  await shot('sell-hay-preview', { width: 1200, height: 1000 },
    `/object/lot/${lotId}`, async p => {
      await sellHay(p, '20', '5');
      await p.getByRole('button', { name: t('ov-action-preview') }).click();
      await p.getByText(t('ov-action-preview-head').split(', ').pop()).waitFor({ timeout: 15000 });
      await p.evaluate(() => document.activeElement?.blur());
    }, '.capp-modal');

  // The same shape elsewhere: a field's margin from its harvests and its hay, and the
  // harvests at each step of their lifecycle.
  await shot('object-view-field', { width: 860, height: 900 },
    `/object/field/${d('hilltop')}`, async p => {
      await p.getByText(d('Spring barley'), { exact: true }).waitFor({ timeout: 30000 });
      await p.getByText('planted', { exact: true }).waitFor({ timeout: 10000 });
    }, '.main', '.capp-l-section');

  // Action types: Saturday's forty bales, refused by the server in the real form.
  await shot('sell-hay-refused', { width: 1200, height: 900 },
    `/object/lot/${lotId}`, async p => {
      await sellHay(p, '40');
      // The form checks the submission criteria as you type; Run stays disabled.
      await p.locator('.capp-actionform__note', { hasText: d('Not that many bales in the barn.') }).waitFor({ timeout: 15000 });
      await p.evaluate(() => document.activeElement?.blur());
    }, '.capp-modal');
} finally {
  await b.close();
}
