// bp-check.mjs <base> <out> <lot-id> — screenshots after the Blueprint switch, light and dark
import { chromium } from 'playwright-core';

const [base, out, lotId] = process.argv.slice(2);
const b = await chromium.launch({ executablePath: process.env.CHROMIUM || '/usr/bin/chromium',
                                  args: ['--no-sandbox', '--disable-gpu'] });

const lotView = async p => {
  await p.getByText('S-2026-007', { exact: true }).waitFor({ timeout: 30000 });
};
const wait = async p => p.waitForTimeout(4000);
const pages = lotId ? [] : [
  ['app-konten', `/apps/konten`, wait],
  ['app-rechnungen', `/apps/rechnungen`, wait],
  ['app-vertraege', `/apps/vertraege`, wait],
  ['app-zahlungen', `/apps/zahlungen`, wait],
  ['explorer-bankkonto', `/object-workspace/bankkonto`, wait],
  ['ontology-bankkonto', `/ontology/object-types/bankkonto`, wait],
  ['ontology', `/ontology`, async p => p.waitForTimeout(4000)],
  ['apps', `/apps`, async p => p.waitForTimeout(4000)],
  ['workspace', `/workspace`, async p => p.waitForTimeout(4000)],
  ['home', `/`, async p => p.waitForTimeout(4000)],
];

for (const scheme of ['light', 'dark']) {
  for (const [name, path, ready] of pages) {
    const ctx = await b.newContext({ viewport: { width: 1400, height: 900 }, locale: 'en-US',
                                     colorScheme: scheme, reducedMotion: 'reduce' });
    const p = await ctx.newPage();
    const errs = [];
    p.on('pageerror', e => errs.push(e.message));
    p.on('console', m => { if (m.type() === 'error') errs.push('console: ' + m.text()); });
    try {
      await p.goto(`${base}${path}${path.includes('?') ? '&' : '?'}lang=en`, { waitUntil: 'load' });
      await ready(p);
      await p.waitForTimeout(800);
      await p.screenshot({ path: `${out}/${name}-${scheme}.png` });
      console.log(`ok ${name}-${scheme}${errs.length ? ' ERRORS: ' + errs.slice(0, 3).join(' | ') : ''}`);
    } catch (e) {
      await p.screenshot({ path: `${out}/${name}-${scheme}-FAIL.png` }).catch(() => {});
      console.log(`FAIL ${name}-${scheme}: ${e.message.split('\n')[0]} ${errs.slice(0, 3).join(' | ')}`);
    }
    await ctx.close();
  }
}
await b.close();
