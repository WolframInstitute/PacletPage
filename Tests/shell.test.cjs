// Run with wolframscript, Chrome and playwright-core: node --test Tests/shell.test.cjs
// The shell's module, shell.js, mounted on the shell PacletPage writes in "Expression" mode (as a host
// such as Wolfram Studio shows it), with the host's callbacks: the filter, Enter, the active page,
// the layout buttons, the splitters, the Feedback panel's report, and destroy().
const test = require('node:test');
const assert = require('node:assert');
const { chromium } = require('playwright-core');
const { execFileSync } = require('node:child_process');
const fs = require('node:fs');
const path = require('node:path');

const assets = path.join(__dirname, '../Assets/Web');
const shellHTML = execFileSync(process.env.WOLFRAMSCRIPT || 'wolframscript', ['-f', 'Tests/shell-fixture.wls'], {
  cwd: path.join(__dirname, '..'), encoding: 'utf8'
}).replace(/^[^<]*/, '');

function page() {
  return '<!doctype html><html><head><meta charset="utf-8"><link rel="stylesheet" href="/shell.css"></head><body>' +
    shellHTML +
    '<script type="module">import { mountPacletShell } from "/shell.js"; window.mountPacletShell = mountPacletShell; window.shellReady = true;</script>' +
    '</body></html>';
}

async function withPage(body) {
  const browser = await chromium.launch(process.env.CHROME_BIN ? { executablePath: process.env.CHROME_BIN } : { channel: 'chrome' });
  try {
    const context = await browser.newContext({ viewport: { width: 1200, height: 800 }, permissions: ['clipboard-read', 'clipboard-write'] });
    const tab = await context.newPage();
    await tab.route('https://shell.test/**', (route) => {
      const name = new URL(route.request().url()).pathname;
      if (name === '/shell.css') return route.fulfill({ contentType: 'text/css', body: fs.readFileSync(path.join(assets, 'shell.css'), 'utf8') });
      if (name === '/shell.js') return route.fulfill({ contentType: 'text/javascript', body: fs.readFileSync(path.join(assets, 'shell.js'), 'utf8') });
      return route.fulfill({ contentType: 'text/html', body: page() });
    });
    await tab.goto('https://shell.test/');
    await tab.waitForFunction(() => window.shellReady === true);
    await body(tab);
  } finally { await browser.close(); }
}

test('The filter hides what does not match, Enter navigates to the best match, and clearing restores the rails', () => withPage(async (tab) => {
  const result = await tab.evaluate(() => {
    const root = document.querySelector('.pp-shell'), calls = [], store = {};
    // a guide tree with a closed group, as a host's rail may have
    const tree = document.createElement('ul');
    tree.innerHTML = '<li><details><summary><a class="link gd" data-id="guide/Group" href="#">Group</a></summary>' +
      '<ul><li><a class="link gd" data-id="guide/Inner" href="#">Inner Topic</a></li></ul></details></li>';
    document.querySelector('.pp-side').appendChild(tree);
    const shell = window.mountPacletShell(root, {
      onNavigate: (uri, link) => calls.push([uri, link.dataset.id]),
      getState: (key, fallback) => (key in store ? store[key] : fallback), setState: (key, value) => { store[key] = value; }
    });
    const q = root.querySelector('.pp-filter'), shown = (id) => root.querySelector('[data-id="' + id + '"]').closest('li').style.display !== 'none';
    const type = (value) => { q.value = value; q.dispatchEvent(new Event('input', { bubbles: true })); };
    type('alphab');
    const filtered = { alpha: shown('ref/Alpha'), alphaBeta: shown('ref/AlphaBeta'), steps: shown('tutorial/Steps'), saved: store.filter };
    type('inner');
    const opened = root.querySelector('details').open;
    q.dispatchEvent(new KeyboardEvent('keydown', { key: 'Enter', bubbles: true }));
    type('zzz');
    const empty = !!root.querySelector('.pp-side .pp-nav-empty');
    type('');
    const restored = { closed: !root.querySelector('details').open, all: shown('ref/Gamma') && shown('tutorial/Steps'), empty: !!root.querySelector('.pp-nav-empty') };
    type('kwgamma');
    const byKeyword = shown('ref/Gamma') && !shown('ref/Beta');
    // an Escape the filter clears on goes no further, to a host's own Escape
    let hostEscapes = 0;
    const host = (e) => { if (e.key === 'Escape') hostEscapes++; };
    document.addEventListener('keydown', host);
    q.dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape', bubbles: true }));
    document.removeEventListener('keydown', host);
    const escaped = { cleared: q.value === '', hostEscapes };
    shell.destroy();
    return { filtered, opened, calls, empty, restored, byKeyword, escaped, same: window.mountPacletShell(root) === window.mountPacletShell(root) };
  });
  assert.deepStrictEqual(result.filtered, { alpha: false, alphaBeta: true, steps: false, saved: 'alphab' });
  assert.ok(result.opened, 'A match inside a closed guide opens it');
  assert.deepStrictEqual(result.calls, [['#', 'guide/Inner']], 'Enter hands the best match to onNavigate');
  assert.ok(result.empty, 'A rail with no match says so');
  assert.deepStrictEqual(result.restored, { closed: true, all: true, empty: false });
  assert.ok(result.byKeyword, 'A page matches on its keywords');
  assert.deepStrictEqual(result.escaped, { cleared: true, hostEscapes: 0 });
  assert.ok(result.same, 'Mounting a mounted root gives the same handle');
}));

test('setActive marks one link and its row, a click goes to onNavigate, and the layout buttons keep their state', () => withPage(async (tab) => {
  const result = await tab.evaluate(() => {
    const root = document.querySelector('.pp-shell'), calls = [], store = {};
    const shell = window.mountPacletShell(root, {
      onNavigate: (uri) => calls.push(uri),
      getState: (key, fallback) => (key in store ? store[key] : fallback), setState: (key, value) => { store[key] = value; }
    });
    shell.setActive('ref/Beta');
    const active = Array.from(root.querySelectorAll('a.active'), (a) => a.dataset.id);
    const row = root.querySelector('[data-id="ref/Beta"]').closest('li').classList.contains('active');
    root.querySelector('[data-id="ref/Gamma"]').click();
    root.querySelector('[data-pp-layout="left"]').click();
    root.querySelector('[data-pp-layout="fullscreen"]').click();
    const layout = { hideLeft: root.classList.contains('pp-hide-left'), full: root.classList.contains('pp-fullscreen'), store: { ...store } };
    document.dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape' }));
    const afterEscape = root.classList.contains('pp-fullscreen');
    // the rails' widths: the arrow keys move a splitter, a double-click resets it
    const split = root.querySelector('.pp-split-left'), rail = root.querySelector('.pp-side');
    root.classList.remove('pp-hide-left');
    const before = rail.getBoundingClientRect().width;
    split.dispatchEvent(new KeyboardEvent('keydown', { key: 'ArrowRight', bubbles: true }));
    const moved = rail.getBoundingClientRect().width - before, stored = store['pp-side-w'];
    split.dispatchEvent(new MouseEvent('dblclick', { bubbles: true }));
    const reset = rail.style.width === '' && store['pp-side-w'] === '';
    shell.destroy();
    return { active, row, calls, layout, afterEscape, moved, stored, reset, activeAfter: Array.from(root.querySelectorAll('a.active'), (a) => a.dataset.id) };
  });
  assert.deepStrictEqual(result.active, ['ref/Beta']);
  assert.ok(result.row);
  assert.deepStrictEqual(result.calls, ['paclet:Test/Shell/ref/Gamma']);
  assert.deepStrictEqual(result.layout, { hideLeft: true, full: true, store: { hideLeft: true, fullscreen: true } });
  assert.strictEqual(result.afterEscape, false);
  assert.strictEqual(result.moved, 12);
  assert.ok(result.stored > 0);
  assert.ok(result.reset);
  assert.deepStrictEqual(result.activeAfter, ['ref/Gamma']);
}));

test('The Feedback panel hands its report, files included, to onFeedback, and destroy() takes away what mounting added', () => withPage(async (tab) => {
  const result = await tab.evaluate(async () => {
    const root = document.querySelector('.pp-shell'), reports = [];
    let shell = window.mountPacletShell(root, {
      defaultEmail: 'reader@example.org',
      onFeedback: async (payload) => { reports.push(payload); return { submitted: true }; }
    });
    const tool0 = root.querySelector('[data-pp-tool="feedback"]');
    // the form closes, out of sight, by its close button, its backdrop and Escape
    const shown = () => getComputedStyle(document.querySelector('.pp-fb-overlay')).display !== 'none';
    const closes = [];
    tool0.click(); closes.push(shown()); document.querySelector('.pp-fb-dismiss').click(); closes.push(shown());
    tool0.click(); closes.push(shown()); document.querySelector('.pp-fb-overlay').click(); closes.push(shown());
    tool0.click(); closes.push(shown());
    document.querySelector('.pp-fb-overlay textarea').dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape', bubbles: true }));
    closes.push(shown());
    tool0.click();
    const overlay = document.querySelector('.pp-fb-overlay'), form = overlay.querySelector('form');
    const prefilled = form.querySelector('input[type="email"]').value;
    form.querySelector('textarea').value = 'The example is wrong.';
    const files = new DataTransfer();
    files.items.add(new File(['png-bytes'], 'image.png', { type: 'image/png' }));
    const paste = new Event('paste', { bubbles: true });
    paste.clipboardData = files;
    document.dispatchEvent(paste);
    const listed = overlay.querySelectorAll('.pp-fb-list li').length;
    form.requestSubmit();
    for (let i = 0; i < 50 && !reports.length; i++) await new Promise((r) => setTimeout(r, 20));
    const sent = form.classList.contains('sent');
    // and on its own, a moment after the report is sent
    await new Promise((r) => setTimeout(r, 2700));
    const autoClosed = !shown();
    shell.destroy();
    const gone = !document.querySelector('.pp-fb-overlay');
    // without onFeedback, a shell whose page names no feedback form and no email hides the button
    const tool = root.querySelector('[data-pp-tool="feedback"]');
    tool.removeAttribute('data-pp-feedback-url');
    tool.setAttribute('href', '#feedback');
    shell = window.mountPacletShell(root, {});
    const hidden = tool.hidden;
    shell.destroy();
    // after destroy(), nothing the shell bound answers
    const q = root.querySelector('.pp-filter');
    document.dispatchEvent(new KeyboardEvent('keydown', { key: '/' }));
    root.querySelector('[data-pp-layout="right"]').click();
    return { closes, autoClosed, prefilled, listed, report: reports[0], sent, gone, hidden, quiet: document.activeElement !== q && !root.classList.contains('pp-hide-right') };
  });
  assert.deepStrictEqual(result.closes, [true, false, true, false, true, false], 'The form opens, and closes out of sight by its close button, its backdrop and Escape');
  assert.ok(result.autoClosed, 'The form closes itself after the report is sent');
  assert.strictEqual(result.prefilled, 'reader@example.org');
  assert.strictEqual(result.listed, 1);
  const { attachments, ...rest } = result.report;
  assert.deepStrictEqual(rest, {
    feedback: 'The example is wrong.', where: '', expected: '', email: 'reader@example.org', title: 'Alpha',
    view: 'built page', page: 'ref/Alpha', paclet: 'Test/Shell', feedbackUrl: 'https://feedback.test/Form'
  });
  assert.strictEqual(attachments.length, 1);
  assert.match(attachments[0].name, /^screenshot-\d{8}-\d{6}\.png$/);
  assert.match(attachments[0].base64, /^data:image\/png;base64,/);
  assert.ok(result.sent && result.gone && result.hidden && result.quiet);
}));

test('Copy link copies the address of the page shown, the host\'s where it gives one, and shows a check mark for a moment', () => withPage(async (tab) => {
  const result = await tab.evaluate(async () => {
    const root = document.querySelector('.pp-shell'), copies = [];
    const shell = window.mountPacletShell(root, {
      getPageUrl: (id) => 'https://studio.test/?paclet=Test/Shell&page=' + id,
      onCopyLink: (url, id, ok) => copies.push({ url, id, ok })
    });
    shell.setActive('ref/Beta');
    const button = root.querySelector('[data-pp-tool="copylink"]');
    button.click();
    for (let i = 0; i < 50 && !copies.length; i++) await new Promise((r) => setTimeout(r, 20));
    const marked = button.classList.contains('copied'), copied = await navigator.clipboard.readText();
    await new Promise((r) => setTimeout(r, 1500));
    const unmarked = !button.classList.contains('copied');
    shell.destroy();
    return { copies, marked, copied, unmarked, title: button.getAttribute('title') };
  });
  assert.deepStrictEqual(result.copies, [{ url: 'https://studio.test/?paclet=Test/Shell&page=ref/Beta', id: 'ref/Beta', ok: true }]);
  assert.strictEqual(result.copied, 'https://studio.test/?paclet=Test/Shell&page=ref/Beta');
  assert.ok(result.marked && result.unmarked);
  assert.strictEqual(result.title, 'Copy link to this page');
}));
