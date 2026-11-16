// Proben, die IN der Seite laufen (page.evaluate serialisiert jede Funktion einzeln) — darum
// Pfeilfunktionen ohne Bezug nach außen. sutra-e2e.walkthrough bettet die Datei beim Kompilieren ein.
({
  inventar: (wort) => {
    const sicht = (e) => {
      const r = e.getBoundingClientRect(); const s = getComputedStyle(e);
      return r.width > 0 && r.height > 0 && s.visibility !== 'hidden' && s.display !== 'none';
    };
    const imBild = (e) => {
      const r = e.getBoundingClientRect(); return sicht(e) && r.bottom > 0 && r.top < innerHeight;
    };
    const t = (e, n = 80) => (e.innerText || e.value || e.placeholder
      || e.getAttribute('aria-label') || '').replace(/\s+/g, ' ').trim().slice(0, n);
    const alle = (sel) => [...document.querySelectorAll(sel)].filter(imBild);
    const eins = (xs, n) => xs.filter(Boolean).filter((x, i, a) => a.indexOf(x) === i).slice(0, n);
    let treffer = null;
    if (wort) {
      const w = wort.toLowerCase();
      const roh = (e) => `${e.textContent} ${e.value || ''} ${e.placeholder || ''} ${
        e.getAttribute('aria-label') || ''}`.toLowerCase();
      const els = [...document.querySelectorAll('body *')].filter((e) => roh(e).includes(w) && sicht(e));
      const tiefste = els.filter((e) => !els.some((o) => o !== e && e.contains(o)));
      treffer = { wort, imBild: tiefste.filter(imBild).length, irgendwo: tiefste.length };
    }
    return {
      url: location.pathname + location.search,
      titel: eins(alle('h1,h2,h3,[role=heading],[class*=title]').map((e) => t(e)), 25),
      knoepfe: eins(alle('button,[role=button],a[href]')
        .map((e) => (e.disabled ? '⊘ ' : '') + t(e)).filter((x) => x.replace('⊘ ', '')), 60),
      felder: alle('input,select,textarea').map((e) => `${e.type || e.tagName}:${t(e)}`).slice(0, 20),
      badges: eins(alle('[class*=badge],[class*=chip],[class*=pill]').map((e) => t(e)), 40),
      modal: alle('[role=dialog],[class*=modal],[class*=overlay]').map((e) => t(e, 300)).slice(0, 3),
      live: eins(alle('[role=status],[role=alert],[aria-live],[class*=toast],[class*=snack]')
        .map((e) => t(e, 200)), 5),
      text: (document.querySelector('main,.consumer__app,body')?.innerText || '')
        .replace(/\n{2,}/g, '\n').slice(0, 2500),
      treffer,
    };
  },

  ziel: (e) => {
    const r = e.getBoundingClientRect();
    return {
      name: (e.getAttribute('aria-label') || e.innerText || e.value || e.placeholder || e.title || '')
        .replace(/\s+/g, ' ').trim().slice(0, 80),
      breite: Math.round(r.width), hoehe: Math.round(r.height), y: Math.round(r.top),
      imBild: r.top >= 0 && r.bottom <= innerHeight && r.left >= 0 && r.right <= innerWidth,
      gesperrt: !!e.disabled || e.getAttribute('aria-disabled') === 'true',
    };
  },

  lade: () => document.querySelectorAll('[aria-busy=true],[role=progressbar],[class*=spinner],'
    + '[class*=loading],[class*=skeleton]').length,

  layout: () => {
    const kurz = (e) => (e.innerText || e.getAttribute('aria-label') || e.tagName)
      .replace(/\s+/g, ' ').trim().slice(0, 40);
    const sicht = (e) => {
      const r = e.getBoundingClientRect(); const s = getComputedStyle(e);
      return r.width > 0 && r.height > 0 && s.visibility !== 'hidden' && s.display !== 'none'
        && r.bottom > 0 && r.top < innerHeight;
    };
    // Erreichbar = der Mittelpunkt trifft das Element selbst; was ein Dialog verdeckt, zählt nicht.
    const erreichbar = (e) => {
      const r = e.getBoundingClientRect();
      const x = document.elementFromPoint(r.left + r.width / 2, r.top + r.height / 2);
      return !!x && (x === e || e.contains(x));
    };
    const bedien = [...document.querySelectorAll('button,a[href],input,select,textarea,'
      + '[role=button],[role=tab],[role=checkbox],[role=menuitem],[tabindex]:not([tabindex="-1"])')]
      .filter(sicht).filter(erreichbar);
    const klein = bedien.filter((e) => {
      if (e.tagName === 'A' && e.closest('p,li')) return false;
      const r = e.getBoundingClientRect(); return r.width < 24 || r.height < 24;
    }).map(kurz);
    const ueberlappung = [];
    const rs = bedien.slice(0, 200).map((e) => [e, e.getBoundingClientRect()]);
    for (let i = 0; i < rs.length; i++) for (let j = i + 1; j < rs.length; j++) {
      const [a, ra] = rs[i]; const [c, rc] = rs[j];
      if (a.contains(c) || c.contains(a)) continue;
      const x = Math.min(ra.right, rc.right) - Math.max(ra.left, rc.left);
      const y = Math.min(ra.bottom, rc.bottom) - Math.max(ra.top, rc.top);
      if (x > 2 && y > 2) ueberlappung.push(`${kurz(a)} ⟷ ${kurz(c)}`);
    }
    const textEls = [...document.querySelectorAll('body *')].filter((e) =>
      [...e.childNodes].some((n) => n.nodeType === 3 && n.textContent.trim()) && sicht(e));
    const abgeschnitten = textEls.filter((e) => {
      const s = getComputedStyle(e);
      const versteckt = /hidden|clip/.test(s.overflowX + s.overflow) || s.textOverflow === 'ellipsis';
      return versteckt && e.scrollWidth > e.clientWidth + 1 && !e.closest('[title]');
    }).map(kurz);
    const kleineSchrift = textEls.filter((e) => parseFloat(getComputedStyle(e).fontSize) < 12)
      .map(kurz);
    const text = document.body.innerText;
    // In de-AT schreibt man 1.234,56 und 02.11.2026 — 1,234.56 oder 2026-11-02 ist fremd.
    const fremdeFormate = [...new Set([...(text.match(/\b\d{1,3}(,\d{3})+\.\d{2}\b/g) || []),
      ...(text.match(/\b\d{4}-\d{2}-\d{2}\b/g) || [])])].slice(0, 5);
    return {
      bedienelemente: bedien.length, klein, ueberlappung: ueberlappung.slice(0, 10),
      abgeschnitten, kleineSchrift: kleineSchrift.slice(0, 10), fremdeFormate,
      hauptaktionen: bedien.filter((e) => /primary|cta/i.test(e.className)).map(kurz),
      querscroll: Math.max(0, document.documentElement.scrollWidth - innerWidth),
    };
  },

  axe: () => window.axe.run(document, { resultTypes: ['violations'],
    runOnly: { type: 'tag', values: ['wcag2a', 'wcag2aa', 'wcag21a', 'wcag21aa', 'wcag22aa'] } })
    .then((r) => r.violations.map((v) => ({ id: v.id, impact: v.impact, hilfe: v.help,
      wcag: v.tags.filter((t) => /^wcag\d{3,}$/.test(t)), knoten: v.nodes.length,
      beispiel: v.nodes[0]?.target.join(' ') }))),
})
