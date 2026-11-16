// Was jeder Film braucht: ein sichtbarer Zeiger und eine Maus, die hinfaehrt
// statt zu springen. Headless Chromium zeichnet keinen Cursor ins Video.

export const zeigerImSeite = () => {
  if (window.__dbz) return;
  window.__dbz = true;
  const bau = () => {
    let n = document.getElementById('drehbuch-zeiger');
    if (!n && document.body) {
      n = document.createElement('div');
      n.id = 'drehbuch-zeiger';
      n.style.cssText =
        'position:fixed;z-index:2147483647;width:18px;height:18px;margin:-9px 0 0 -9px;' +
        'left:-40px;top:-40px;border-radius:50%;background:rgba(255,64,64,.35);' +
        'border:2px solid rgba(255,64,64,.95);pointer-events:none;transition:transform .08s linear';
      document.body.appendChild(n);
    }
    return n;
  };
  addEventListener('mousemove', (e) => {
    const n = bau();
    if (n) { n.style.left = e.clientX + 'px'; n.style.top = e.clientY + 'px'; }
  }, true);
  addEventListener('mousedown', () => { const n = bau(); if (n) n.style.transform = 'scale(.6)'; }, true);
  addEventListener('mouseup', () => { const n = bau(); if (n) n.style.transform = 'scale(1)'; }, true);
};

export const fahreZu = async (p, x, y, schritte = 16) => {
  await p.mouse.move(x, y, { steps: schritte });
  await p.waitForTimeout(90);
};

export const klicke = async (p, el, schritte) => {
  const k = await el.boundingBox();
  if (!k) return false;
  await fahreZu(p, k.x + k.width / 2, k.y + k.height / 2, schritte);
  await p.mouse.down();
  await p.waitForTimeout(110);
  await p.mouse.up();
  return true;
};
