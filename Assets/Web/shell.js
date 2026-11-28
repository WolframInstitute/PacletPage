// PacletPage's documentation shell, the behavior of the elements PacletPage writes around a page:
// the filter over the rails, the A-Z index of the symbols, the layout buttons, the splitters, the
// active page and the Feedback panel. A published site and any host that shows a paclet's pages in
// the shell, such as Wolfram Studio, mount the same module (PacletPageWebAssets), so the shell
// behaves the same in both; what a host does its own way comes in as callbacks:
//
//   import { mountPacletShell } from './shell.js';
//   const shell = mountPacletShell(rootElement, { onNavigate, onFeedback, ... });
//   shell.setActive(pageId); shell.refresh(); shell.destroy();
//
// The module is CSP-safe: no eval, no Function, no inline handlers, and the elements it makes are
// made with the DOM's own methods, their text set as text.

const MAX_FILES = 3;
const MAX_BYTES = 20 * 1000 * 1000;
const RAIL_MIN = 140;
const RAIL_MAX = 480;

const ENGLISH = {
  noMatches: 'no matches',
  feedbackHeading: 'Feedback on this page',
  feedbackWhat: 'What is wrong, unclear or missing?',
  feedbackWhere: 'Where on the page?',
  feedbackWherePlaceholder: 'a section, an example or an output',
  feedbackExpected: 'What did you expect instead?',
  feedbackFiles: 'Screenshots or files',
  feedbackFilesNote: 'optional, up to three',
  feedbackDrop: 'Paste a screenshot (Cmd+V or Ctrl+V), drop files here, or ',
  feedbackBrowse: 'choose files',
  feedbackEmail: 'Your email',
  feedbackEmailNote: 'optional, if you would like a reply',
  optional: 'optional',
  close: 'Close',
  remove: 'Remove {name}',
  send: 'Send feedback',
  sending: 'Sending…',
  thanksTitle: 'Thank you',
  thanks: 'Your feedback was sent.',
  allFeedback: 'All feedback',
  orEmail: ' · or email ',
  emailOnly: 'Or email ',
  needText: 'Please say what is wrong, unclear or missing.',
  badEmail: 'That email address does not look right.',
  badField: 'The form could not read one of the fields; please check the email address.',
  failed: 'Sorry, that did not go through. Please try again.',
  failedEmail: 'Sorry, that did not go through. Please try again, or email {to}.',
  skipped: 'Up to three files of at most 20 MB each are attached; {count} left out.',
  noMail: 'No mail app opened? Write to {to}, or copy the prefilled message.',
  copyMessage: 'Copy message',
  copied: 'Copied',
  copyFailed: 'Copy failed: select the address',
  showMore: 'Show {count} more',
  allNames: 'All names',
  letterNames: 'All {letter} names',
  otherNames: 'Other {letter} names',
  ofSymbols: '{count} of {total} {letter} symbols',
  letterSymbols: '{letter} · {total} symbols',
  letterSymbol: '{letter} · {total} symbol',
  matchingSymbols: '{count} matching symbols',
  matchingSymbol: '{count} matching symbol',
  noMatchingSymbols: 'No matching symbols'
};

const mounted = new WeakMap();

function english(key, vars) {
  const text = Object.prototype.hasOwnProperty.call(ENGLISH, key) ? ENGLISH[key] : key;
  return text.replace(/\{(\w+)\}/g, (all, name) => (vars && name in vars ? String(vars[name]) : all));
}

// the text a link matches on: its title and the page's keywords
function matchText(a) { return (a.textContent + ' ' + (a.getAttribute('data-kw') || '')).toLowerCase(); }
// a term with spaces also matches the title with its spaces taken out
function flat(s) { return s.replace(/\s+/g, ''); }
function matches(a, term) {
  return matchText(a).indexOf(term) !== -1 ||
    (/\s/.test(term) && flat(a.textContent.toLowerCase()).indexOf(flat(term)) !== -1);
}
// Enter opens the best entry the filter shows: a title equal to the term, then one that starts with
// it, then one with a word that starts with it (after a separator or at a CamelCase step), then one
// that contains it, last one that matches only on its keywords; within a rank a guide or tutorial
// before a symbol, a written tutorial before a placeholder, then the shorter title.
function wordStartAt(title, i) {
  if (i === 0) return true;
  const p = title.charAt(i - 1), c = title.charAt(i);
  return !/[A-Za-z0-9]/.test(p) || (/[a-z]/.test(p) && /[A-Z]/.test(c)) || (/[0-9]/.test(p) !== /[0-9]/.test(c));
}
function titleRank(a, term) {
  const title = (a.textContent || '').trim(), t = title.toLowerCase();
  let i = t.indexOf(term);
  if (i === -1 && /\s/.test(term)) {
    const j = flat(t).indexOf(flat(term));
    if (j !== -1) return j > 0 ? 3 : flat(t) === flat(term) ? 0 : 1;
  }
  if (i === -1) return matches(a, term) ? 4 : -1;
  if (t === term) return 0;
  if (i === 0) return 1;
  for (; i !== -1; i = t.indexOf(term, i + 1)) { if (wordStartAt(title, i)) return 2; }
  return 3;
}
const KIND = { resource: 0, guide: 0, tutorial: 0, ref: 1 };
function bestMatch(links, term) {
  let best = null, bk = null;
  for (const a of links) {
    if (a.closest('li') && a.closest('li').style.display === 'none') continue;
    const r = titleRank(a, term);
    if (r < 0) continue;
    const k = [r, KIND[(a.getAttribute('data-id') || '').split('/')[0]] || 0, a.classList.contains('stub') ? 1 : 0, (a.textContent || '').trim().length];
    let j = 0;
    if (bk) { for (; j < k.length; j++) { if (k[j] !== bk[j]) break; } }
    if (!bk || (j < k.length && k[j] < bk[j])) { best = a; bk = k; }
  }
  return best;
}

function el(doc, tag, attrs, children) {
  const node = doc.createElement(tag);
  if (attrs) {
    for (const [name, value] of Object.entries(attrs)) {
      if (value === undefined || value === null || value === false) continue;
      if (name === 'text') node.textContent = value;
      else if (name === 'className') node.className = value;
      else node.setAttribute(name, value === true ? '' : String(value));
    }
  }
  for (const child of children || []) if (child) node.appendChild(typeof child === 'string' ? doc.createTextNode(child) : child);
  return node;
}

// a reader's choices kept for the next visit: through the host's getState/setState, else in this
// browser's localStorage, by the paclet
function makeState(root, opts) {
  const prefix = 'pp-shell:' + (root.getAttribute('data-paclet-page') || '') + ':';
  return {
    get(key, fallback) {
      if (typeof opts.getState === 'function') {
        const value = opts.getState(key, fallback);
        return value === undefined || value === null ? fallback : value;
      }
      try {
        const raw = window.localStorage.getItem(prefix + key);
        return raw === null ? fallback : JSON.parse(raw);
      } catch (e) { return fallback; }
    },
    set(key, value) {
      if (typeof opts.setState === 'function') { opts.setState(key, value); return; }
      try {
        if (value === '' || value === undefined || value === null) window.localStorage.removeItem(prefix + key);
        else window.localStorage.setItem(prefix + key, JSON.stringify(value));
      } catch (e) { /* a browser that keeps nothing */ }
    }
  };
}

export function mountPacletShell(root, opts = {}) {
  if (!root || root.nodeType !== 1) throw new TypeError('mountPacletShell: root must be an element');
  const known = mounted.get(root);
  if (known) return known;

  const doc = root.ownerDocument, win = doc.defaultView || window;
  const t = typeof opts.t === 'function' ? (key, vars) => opts.t(key, vars) || english(key, vars) : english;
  const state = makeState(root, opts);
  const life = new AbortController();
  const on = (target, type, fn, options) => { if (target) target.addEventListener(type, fn, Object.assign({ signal: life.signal }, options || {})); };
  const timers = new Set(), objectURLs = new Set(), added = [];
  const later = (fn, ms) => { const id = win.setTimeout(() => { timers.delete(id); fn(); }, ms); timers.add(id); return id; };
  const clearLater = (id) => { if (id) { win.clearTimeout(id); timers.delete(id); } };

  let parts = {}, rails = new Map(), current = null, openState = null, symbolIndex = null, destroyed = false;

  // ---- the shell's elements, read again by refresh() ------------------------------------------
  function find() {
    const q = (sel) => root.querySelector(sel);
    parts = {
      filter: q('.pp-filter, #q'),
      left: q('.pp-side, #side'),
      right: q('.pp-side-right, #side-right'),
      splitLeft: q('.pp-split-left, #splitL'),
      splitRight: q('.pp-split-right, #splitR'),
      feedback: q('[data-pp-tool="feedback"]')
    };
    // each rail's links by page id, kept while the rail is the same element, so a host that keeps
    // its rails across pages does not have them indexed again
    const next = new Map();
    for (const nav of [parts.left, parts.right]) {
      if (!nav) continue;
      const key = (nav.dataset && nav.dataset.ppRailKey) || '';
      const old = rails.get(nav);
      if (old && old.key === key) { next.set(nav, old); continue; }
      const byId = new Map();
      for (const a of nav.querySelectorAll('a[data-id]')) {
        const id = a.getAttribute('data-id');
        if (!byId.has(id)) byId.set(id, []);
        byId.get(id).push(a);
      }
      next.set(nav, { key, byId });
    }
    rails = next;
  }
  function allLinks() {
    const links = [];
    for (const { byId } of rails.values()) for (const list of byId.values()) links.push(...list);
    if (symbolIndex) for (const entry of symbolIndex.entries) if (!links.includes(entry.link)) links.push(entry.link);
    return links;
  }

  // ---- the filter -------------------------------------------------------------------------------
  // What does not match is hidden in place: a guide stays while it or anything below it matches,
  // and clearing the box restores the tree as it was.
  function railEmptyNote(nav, show) {
    let note = nav.querySelector(':scope > .pp-nav-empty');
    if (show && !note) { note = el(doc, 'div', { className: 'pp-nav-empty nav-empty', text: t('noMatches') }); nav.appendChild(note); }
    else if (!show && note) note.remove();
  }
  function hitIn(node) {
    if (!node) return false;
    for (const li of node.querySelectorAll('li')) if (li.__ppHit) return true;
    return false;
  }
  function filterRail(nav, term) {
    if (!nav) return;
    const lis = nav.querySelectorAll('li');
    for (const li of lis) li.__ppHit = false;
    for (const a of nav.querySelectorAll('a[data-id]')) {
      if (term && !matches(a, term)) continue;
      for (let n = a.closest('li'); n && nav.contains(n); n = n.parentElement && n.parentElement.closest('li')) n.__ppHit = true;
    }
    for (const li of lis) li.style.display = li.__ppHit ? '' : 'none';
    for (const d of nav.querySelectorAll('details')) {
      if (!term) d.open = !!(openState && openState.get(d));
      else { const li = d.closest('li'); if (li && li.__ppHit) d.open = true; }
    }
    // a group's label stays while its list shows a match, a separator while anything after it does
    for (const label of nav.querySelectorAll('.pp-nav-label, .nav-label')) label.style.display = !term || hitIn(label.nextElementSibling) ? '' : 'none';
    for (const sep of nav.querySelectorAll('.pp-nav-sep, .nav-sep')) {
      let shown = !term;
      for (let n = sep.nextElementSibling; n && !shown; n = n.nextElementSibling) shown = hitIn(n);
      sep.style.display = shown ? '' : 'none';
    }
    railEmptyNote(nav, !!term && lis.length > 0 && Array.prototype.every.call(lis, (li) => li.style.display === 'none'));
  }
  function runFilter(save) {
    const term = parts.filter ? (parts.filter.value || '').trim().toLowerCase() : '';
    if (term && !openState && parts.left) {
      openState = new Map();
      for (const d of parts.left.querySelectorAll('details')) openState.set(d, d.open);
    }
    filterRail(parts.left, term);
    if (symbolIndex && symbolIndex.active()) symbolIndex.filter(term);
    else filterRail(parts.right, term);
    if (!term) openState = null;
    if (save && parts.filter) state.set('filter', parts.filter.value || '');
  }

  // ---- navigation --------------------------------------------------------------------------------
  function navigate(a, event) {
    if (typeof opts.onNavigate === 'function') {
      if (event) event.preventDefault();
      opts.onNavigate(a.getAttribute('href') || a.getAttribute('data-id'), a, event);
    } else if (!event) {
      a.click();
    }
    setActive(a.getAttribute('data-id'), a);
  }

  // ---- the active page -----------------------------------------------------------------------------
  // A guide appears under every guide that links it, so an id can match several links: the one
  // nearest what is open (most <details> ancestors shared with the current link), then the shallowest.
  function chain(a, nav) {
    const c = [];
    for (let n = a && a.parentNode; n && n !== nav; n = n.parentNode) if (n.tagName === 'DETAILS') c.unshift(n);
    return c;
  }
  function pick(hits, nav) {
    if (hits.length < 2) return hits[0] || null;
    const ref = current && nav.contains(current) ? chain(current, nav) : [];
    let best = hits[0], bestScore = -Infinity;
    for (const hit of hits) {
      const c = chain(hit, nav);
      let k = 0;
      while (k < c.length && k < ref.length && c[k] === ref[k]) k++;
      const score = k * 1000 - c.length;
      if (score > bestScore) { bestScore = score; best = hit; }
    }
    return best;
  }
  function markActive(a, active) {
    a.classList.toggle('active', active);
    if (active) a.setAttribute('aria-current', 'page'); else a.removeAttribute('aria-current');
    const li = a.closest('li');
    if (li) li.classList.toggle('active', active);
  }
  function setActive(id, link) {
    if (destroyed) return;
    for (const old of root.querySelectorAll('a[data-id].active')) markActive(old, false);
    current = null;
    if (!id) return;
    let chosen = link || null;
    if (!chosen) {
      for (const [nav, { byId }] of rails) {
        const hit = pick(byId.get(id) || [], nav);
        if (hit) { chosen = hit; break; }
      }
    }
    if (!chosen && symbolIndex) chosen = symbolIndex.linkFor(id);
    if (!chosen) return;
    if (symbolIndex && symbolIndex.active() && symbolIndex.has(chosen)) {
      if (parts.filter && parts.filter.value.trim() && !matches(chosen, parts.filter.value.trim().toLowerCase())) {
        parts.filter.value = ''; runFilter(true);
      }
      symbolIndex.reveal(chosen);
    }
    markActive(chosen, true);
    for (let n = chosen.parentNode; n && n !== root; n = n.parentNode) if (n.tagName === 'DETAILS') n.open = true;
    current = chosen;
    if (typeof chosen.scrollIntoView === 'function') chosen.scrollIntoView({ block: 'nearest' });
  }

  // ---- the layout buttons ------------------------------------------------------------------------------
  function pressed(kind, value) {
    for (const button of root.querySelectorAll('[data-pp-layout="' + kind + '"]')) {
      button.classList.toggle('on', value);
      button.setAttribute('aria-pressed', value ? 'true' : 'false');
    }
  }
  function setLayout(kind, value, save) {
    if (kind === 'left') { root.classList.toggle('pp-hide-left', !value); pressed('left', value); if (save) state.set('hideLeft', !value); }
    else if (kind === 'right') { root.classList.toggle('pp-hide-right', !value); pressed('right', value); if (save) state.set('hideRight', !value); }
    else if (kind === 'fullscreen') { root.classList.toggle('pp-fullscreen', value); pressed('fullscreen', value); if (save) state.set('fullscreen', value); }
  }
  function applyLayout() {
    setLayout('left', !state.get('hideLeft', false), false);
    setLayout('right', !state.get('hideRight', false), false);
    setLayout('fullscreen', !!state.get('fullscreen', false), false);
  }

  // ---- the splitters ---------------------------------------------------------------------------------
  // Drag a divider to rebalance the columns, double-click to restore it; the arrow keys move it too.
  function railWidth(rail, px) { rail.style.width = Math.round(Math.min(RAIL_MAX, Math.max(RAIL_MIN, px))) + 'px'; }
  function widthKey(rail) { return rail === parts.left ? 'pp-side-w' : 'pp-side-right-w'; }
  function storeWidth(rail) { state.set(widthKey(rail), parseInt(rail.style.width, 10) || ''); }
  function applyWidths() {
    for (const rail of [parts.left, parts.right]) {
      if (!rail) continue;
      const w = Number(state.get(widthKey(rail), ''));
      if (w) railWidth(rail, w);
    }
  }
  function splitterFor(handle) {
    if (handle === parts.splitLeft) return { rail: parts.left, fromLeft: true };
    if (handle === parts.splitRight) return { rail: parts.right, fromLeft: false };
    return null;
  }
  function startDrag(e, handle, rail, fromLeft) {
    e.preventDefault();
    if (handle.setPointerCapture && e.pointerId !== undefined) handle.setPointerCapture(e.pointerId);
    handle.classList.add('dragging');
    root.classList.add('pp-resizing');
    const startX = e.clientX, startW = rail.getBoundingClientRect().width, drag = new AbortController();
    const stop = () => { drag.abort(); handle.classList.remove('dragging'); root.classList.remove('pp-resizing'); storeWidth(rail); };
    handle.addEventListener('pointermove', (ev) => railWidth(rail, startW + (fromLeft ? ev.clientX - startX : startX - ev.clientX)), { signal: drag.signal });
    handle.addEventListener('pointerup', stop, { signal: drag.signal });
    handle.addEventListener('pointercancel', stop, { signal: drag.signal });
    life.signal.addEventListener('abort', () => drag.abort(), { signal: drag.signal });
  }

  // ---- the A-Z index of the symbols ------------------------------------------------------------------
  // A rail written with the index (.indexable) has a heading with the index's switch above its list;
  // only the batch of the letter shown is in the list, the other links wait in a template until a
  // letter, the filter or the active page needs them. Off, the list holds every symbol.
  function makeSymbolIndex(rail) {
    if (!rail || !rail.classList.contains('indexable')) return null;
    const list = rail.querySelector('.nav-symbols, .pp-nav-symbols'), batchSize = Number(rail.dataset.batchSize) || 200;
    const tools = rail.querySelector('.symbol-index');
    if (!list || !tools) return null;
    const browse = tools.querySelector('.symbol-browse'), buttons = tools.querySelector('.symbol-letters'),
      letterPicker = tools.querySelector('.symbol-letter-picker select'), prefixLabel = tools.querySelector('.symbol-prefix'),
      prefixPicker = prefixLabel && prefixLabel.querySelector('select'), selection = prefixLabel && prefixLabel.querySelector('.symbol-selection'),
      count = tools.querySelector('.symbol-count'), more = rail.querySelector('.symbol-more'), scroll = rail.querySelector('.symbol-scroll') || rail,
      toggle = tools.querySelector('.symbol-index-toggle');
    // the painted batch merged into the catalog's alphabetical order with the links of the template
    const painted = Array.from(list.querySelectorAll('a[data-id]'));
    const rest = root.querySelector('template.symbol-rest, #symbol-rest');
    const links = rest ? Array.from(rest.content.querySelectorAll('a[data-id]')) : painted.slice();
    if (rest) {
      for (const link of painted) links.splice(Number(link.parentNode.dataset.position), 0, link);
      rest.remove();
    }
    const byLetter = new Map(), byLink = new Map(), byId = new Map(), prefixCache = new Map();
    const entries = links.map((link) => {
      const row = link.parentNode, id = link.getAttribute('data-id'), name = id.replace(/^ref\//, ''),
        initial = row.dataset.letter || (Array.from(name)[0] || '#').toUpperCase(),
        entry = { row, link, name, folded: name.toLowerCase(), letter: initial };
      if (!byLetter.has(initial)) byLetter.set(initial, []);
      byLetter.get(initial).push(entry);
      byLink.set(link, entry);
      byId.set(id, link);
      return entry;
    });
    byLetter.set('All', entries);
    let indexOn = state.get('symbolIndex', rail.classList.contains('indexed') ? 'on' : 'off') === 'on';
    let letter, prefixes, visible = [], shown = 0, term = '', savedTop = 0, savedCount = 0, searchSelection = null;

    function listAll() {
      list.replaceChildren(...entries.map((entry) => { entry.row.style.display = ''; return entry.row; }));
    }
    function endsWord(name, head) {
      const tail = name.slice(head.length);
      return !tail || /^\p{Lu}\p{Ll}/u.test(tail) || (/\p{Ll}$/u.test(head) && /^\p{Lu}/u.test(tail)) || (/\p{N}$/u.test(head) !== /^\p{N}/u.test(tail));
    }
    // Ad split into families such as Adele and Adjoint: a whole word of four characters, such as
    // Abel, or at least six shared characters of a longer word
    function prefixesFor(group) {
      const choices = new Map();
      function split(items, depth) {
        const branches = new Map();
        for (const entry of items) {
          const next = entry.folded.charAt(depth);
          if (!branches.has(next)) branches.set(next, []);
          branches.get(next).push(entry);
        }
        for (const branch of branches.values()) {
          const first = branch[0];
          let prefix = first.folded;
          for (const entry of branch) while (!entry.folded.startsWith(prefix)) prefix = prefix.slice(0, -1);
          const length = Array.from(prefix).length;
          const readable = length >= 6 || (length >= 4 && branch.some((entry) => {
            const head = entry.name.slice(0, prefix.length);
            return head.toLowerCase() === prefix && endsWord(entry.name, head);
          }));
          if (branch.length > 1 && prefix.length > depth && (!readable || branch.length > 60)) split(branch, prefix.length);
          else choices.set(prefix, { entries: branch, label: first.name + (branch.length > 1 ? '…' : '') });
        }
      }
      if (group.length > batchSize) split(group, (Array.from(group[0].folded)[0] || '').length);
      if (choices.size > batchSize) {
        const singles = new Set();
        for (const choice of choices.values()) if (choice.entries.length === 1) singles.add(choice.entries[0]);
        if (singles.size > 1 && singles.size < choices.size) {
          for (const [key, choice] of choices) if (choice.entries.length === 1) choices.delete(key);
          choices.set(':other', { label: t('otherNames', { letter: group[0].letter }), entries: group.filter((entry) => singles.has(entry)) });
        }
      }
      return choices;
    }
    function chooseLetter(value) {
      if (!byLetter.has(value)) value = 'All';
      letter = value;
      if (letterPicker) letterPicker.value = value;
      if (buttons) for (const button of buttons.children) button.setAttribute('aria-pressed', String(button.value === value));
      if (!prefixCache.has(value)) prefixCache.set(value, value === 'All' ? new Map() : prefixesFor(byLetter.get(value)));
      prefixes = prefixCache.get(value);
      if (prefixPicker) {
        prefixPicker.replaceChildren(new Option(value === 'All' ? t('allNames') : t('letterNames', { letter: value }), ''));
        for (const [key, group] of prefixes) prefixPicker.add(new Option(group.label + ' (' + group.entries.length + ')', key));
      }
      if (prefixLabel) prefixLabel.hidden = prefixes.size < 2;
    }
    function browseEntries() {
      const prefix = prefixPicker && prefixes.get(prefixPicker.value);
      return prefix ? prefix.entries : byLetter.get(letter);
    }
    function appendUntil(end) {
      const fragment = doc.createDocumentFragment();
      end = Math.min(end, visible.length);
      while (shown < end) fragment.appendChild(visible[shown++].row);
      list.appendChild(fragment);
      if (more) {
        more.hidden = shown === visible.length;
        if (!more.hidden) more.textContent = t('showMore', { count: Math.min(batchSize, visible.length - shown) });
      }
    }
    function showEntries(items, label) {
      visible = items; shown = 0;
      list.replaceChildren();
      appendUntil(batchSize);
      scroll.scrollTop = 0;
      if (count) count.textContent = label;
    }
    function showBrowse() {
      const items = browseEntries(), prefix = prefixPicker && prefixes.get(prefixPicker.value);
      if (selection && prefixPicker && prefixPicker.selectedOptions[0]) selection.textContent = prefixPicker.selectedOptions[0].textContent;
      const total = byLetter.get(letter).length.toLocaleString('en');
      showEntries(items, prefix ? t('ofSymbols', { count: items.length.toLocaleString('en'), total, letter })
        : t(items.length === 1 ? 'letterSymbol' : 'letterSymbols', { letter, total }));
    }
    function jump(value) { chooseLetter(value); showBrowse(); }
    function reveal(link) {
      const entry = byLink.get(link);
      if (!entry) return;
      let index = visible.indexOf(entry);
      if (term) {
        searchSelection = link;
        if (!browseEntries().includes(entry)) chooseLetter(entry.letter);
      } else if (index < 0) {
        jump(entry.letter);
        index = visible.indexOf(entry);
      }
      if (index >= shown) appendUntil(index + 1);
    }
    function filter(value) {
      if (value === term) return;
      if (!term) { savedTop = scroll.scrollTop; savedCount = shown; searchSelection = null; }
      term = value;
      if (browse) browse.hidden = !!term;
      if (term) {
        const hits = entries.filter((entry) => matches(entry.link, term));
        showEntries(hits, hits.length ? t(hits.length === 1 ? 'matchingSymbol' : 'matchingSymbols', { count: hits.length.toLocaleString('en') }) : t('noMatchingSymbols'));
      } else {
        showBrowse();
        if (searchSelection) { reveal(searchSelection); if (searchSelection.scrollIntoView) searchSelection.scrollIntoView({ block: 'nearest' }); }
        else { appendUntil(savedCount); scroll.scrollTop = savedTop; }
      }
    }
    function setOn(value) {
      indexOn = value;
      rail.classList.toggle('indexed', indexOn);
      if (toggle) toggle.setAttribute('aria-pressed', String(indexOn));
      term = '';
      if (browse) browse.hidden = false;
      if (indexOn) { for (const entry of entries) entry.row.style.display = ''; jump(letter || (letterPicker && letterPicker.value) || rail.dataset.pendingLetter || 'All'); }
      else { visible = []; shown = 0; if (more) more.hidden = true; listAll(); }
    }
    const controls = new AbortController();
    const bind = (target, type, fn) => { if (target) target.addEventListener(type, fn, { signal: controls.signal }); };
    bind(buttons, 'click', (event) => { if (event.target.tagName === 'BUTTON') jump(event.target.value); });
    bind(letterPicker, 'change', () => jump(letterPicker.value));
    bind(prefixPicker, 'change', showBrowse);
    bind(more, 'click', () => { const first = visible[shown]; appendUntil(shown + batchSize); if (first) first.link.focus(); });
    bind(scroll, 'scroll', () => {
      if (more && !more.hidden && scroll.clientHeight && scroll.scrollHeight - scroll.scrollTop - scroll.clientHeight < 48) appendUntil(shown + batchSize);
    });
    bind(toggle, 'click', () => {
      setOn(!indexOn);
      state.set('symbolIndex', indexOn ? 'on' : 'off');
      runFilter(false);
      if (current && byLink.has(current)) { if (indexOn) reveal(current); if (current.scrollIntoView) current.scrollIntoView({ block: 'nearest' }); }
    });
    life.signal.addEventListener('abort', () => controls.abort());
    rail.onclick = rail.onchange = null;
    setOn(indexOn);
    if (indexOn && rail.dataset.extraBatches) appendUntil(shown + Number(rail.dataset.extraBatches) * batchSize);
    delete rail.dataset.pendingLetter;
    delete rail.dataset.extraBatches;
    return {
      rail, entries, filter, reveal, active: () => indexOn, has: (link) => byLink.has(link), linkFor: (id) => byId.get(id) || null,
      dispose: () => controls.abort()
    };
  }

  // ---- the Feedback panel --------------------------------------------------------------------------
  // The Feedback button opens the panel when there is somewhere to send it: the host's onFeedback,
  // else the site's feedback form; with only an email template it opens a prefilled email and a
  // note with the address; with none of them it is hidden.
  let overlay = null, note = null, noteTimer = null, closeTimer = null, attached = [], fb = null, signedInAs;
  function feedbackInfo(tool) {
    const data = (name) => tool.getAttribute('data-pp-feedback-' + name) || '';
    return {
      paclet: data('paclet') || root.getAttribute('data-paclet-page') || '',
      page: data('page') || root.getAttribute('data-paclet-id') || '',
      title: data('title') || root.getAttribute('data-paclet-title') || '',
      view: data('view') || root.getAttribute('data-paclet-view') || '',
      url: data('url'), to: data('to'), subject: data('subject'), body: data('body'), thanks: data('thanks'),
      all: tool.getAttribute('data-pp-all-feedback') || ''
    };
  }
  function mailto(info) {
    return 'mailto:' + info.to + '?subject=' + encodeURIComponent(info.subject) + '&body=' + encodeURIComponent(info.body.replace(/\n/g, '\r\n'));
  }
  function setupFeedbackTool() {
    const tool = parts.feedback;
    if (!tool) return;
    const info = feedbackInfo(tool);
    // the form where the page names one, an email where it has a template, its own link where it
    // points elsewhere; none of them, and the button is hidden
    const external = /^https?:\/\//i.test(tool.getAttribute('href') || '');
    tool.hidden = !(info.url || info.to || external);
    if (info.to && !info.url) tool.setAttribute('href', mailto(info));
  }
  // the text written to the clipboard, through the host's onCopy where it gives one; true once written
  function writeClipboard(text, viaHost) {
    if (viaHost && typeof opts.onCopy === 'function') return Promise.resolve(opts.onCopy(text)).then(() => true, () => false);
    const legacy = () => {
      const ta = el(doc, 'textarea');
      ta.value = text; ta.style.position = 'fixed'; ta.style.opacity = '0';
      doc.body.appendChild(ta); ta.select();
      let ok = false;
      try { ok = doc.execCommand('copy'); } catch (e) { ok = false; }
      ta.remove();
      return ok;
    };
    if (win.navigator.clipboard && win.navigator.clipboard.writeText) return win.navigator.clipboard.writeText(text).then(() => true, () => legacy());
    return Promise.resolve(legacy());
  }
  function copyText(text, button) {
    writeClipboard(text, true).then((ok) => { if (button) button.textContent = ok ? t('copied') : t('copyFailed'); });
  }
  // Copy link: the address of the page shown, the host's (getPageUrl) or the site's, to the clipboard
  let copyTimer = null;
  function copyLink(button) {
    const id = (current && current.getAttribute('data-id')) || root.getAttribute('data-paclet-id') || '';
    const url = typeof opts.getPageUrl === 'function' ? opts.getPageUrl(id) : win.location.origin + win.location.pathname + (id ? '#' + id : '');
    writeClipboard(url, false).then((ok) => {
      button.classList.toggle('copied', ok);
      clearLater(copyTimer);
      copyTimer = later(() => button.classList.remove('copied'), 1400);
      if (typeof opts.onCopyLink === 'function') opts.onCopyLink(url, id, ok);
    });
  }
  function showNote(info) {
    if (!note) {
      note = el(doc, 'div', { className: 'pp-fb-note', role: 'status' });
      doc.body.appendChild(note);
    }
    const copy = el(doc, 'button', { type: 'button', text: t('copyMessage') });
    const close = el(doc, 'button', { type: 'button', className: 'pp-fb-close', 'aria-label': t('close'), text: '×' });
    note.replaceChildren(
      el(doc, 'div', { className: 'pp-fb-hdr' }, [el(doc, 'span', { text: t('feedbackHeading') }), close]),
      el(doc, 'div', { className: 'pp-fb-meta', text: t('noMail', { to: info.to }) }),
      el(doc, 'div', { className: 'pp-fb-acts' }, [el(doc, 'a', { className: 'primary', href: mailto(info), target: '_top', text: info.to }), copy])
    );
    copy.addEventListener('click', () => copyText('To: ' + info.to + '\nSubject: ' + info.subject + '\n\n' + info.body, copy), { signal: life.signal });
    close.addEventListener('click', hideNote, { signal: life.signal });
    note.hidden = false;
    clearLater(noteTimer);
    noteTimer = later(hideNote, 30000);
  }
  function hideNote() { if (note) note.hidden = true; }

  function buildForm() {
    const field = (tag, attrs) => el(doc, tag, attrs);
    const ids = 'pp-fb-' + Math.random().toString(36).slice(2, 8) + '-';
    const text = field('textarea', { id: ids + 'feedback', name: 'Feedback', rows: 5, maxlength: 5000 });
    const where = field('input', { id: ids + 'where', name: 'Where', type: 'text', maxlength: 500, placeholder: t('feedbackWherePlaceholder') });
    const expected = field('textarea', { id: ids + 'expected', name: 'Expected', rows: 2, maxlength: 2000 });
    const picker = field('input', { type: 'file', multiple: true, hidden: true });
    const browseButton = el(doc, 'button', { type: 'button', className: 'pp-fb-browse', text: t('feedbackBrowse') });
    const drop = el(doc, 'div', { className: 'pp-fb-drop' }, [t('feedbackDrop'), browseButton, '.', picker]);
    const list = el(doc, 'ul', { className: 'pp-fb-list' });
    const email = field('input', { id: ids + 'email', name: 'Email', type: 'email', maxlength: 200, placeholder: 'you@example.org' });
    const honeypot = field('input', { name: 'Website', type: 'text', tabindex: -1, autocomplete: 'off', 'aria-hidden': 'true' });
    honeypot.style.position = 'absolute'; honeypot.style.left = '-10000px'; honeypot.style.opacity = '0';
    const status = el(doc, 'p', { className: 'pp-fb-status', role: 'status' });
    const thanksText = el(doc, 'span', { text: t('thanks') });
    const thanks = el(doc, 'div', { className: 'pp-fb-thanks' }, [el(doc, 'strong', { text: t('thanksTitle') }), thanksText]);
    const all = el(doc, 'a', { target: '_blank', rel: 'noopener', text: t('allFeedback') });
    const mailSep = el(doc, 'span', { text: t('orEmail') });
    const mail = el(doc, 'a', { target: '_top' });
    const mailLine = el(doc, 'span', { hidden: true }, [mailSep, mail]);
    const send = el(doc, 'button', { type: 'submit', className: 'pp-fb-send', text: t('send') });
    const dismiss = el(doc, 'button', { type: 'button', className: 'pp-fb-dismiss', 'aria-label': t('close'), text: '×' });
    const hmeta = el(doc, 'span', { className: 'pp-fb-hmeta' });
    const optional = (label) => el(doc, 'span', { className: 'pp-fb-opt', text: label });
    const form = el(doc, 'form', { className: 'pp-fb-form', role: 'dialog', 'aria-modal': 'true', 'aria-label': t('feedbackHeading'), novalidate: true }, [
      el(doc, 'div', { className: 'pp-fb-head' }, [el(doc, 'span', { className: 'pp-fb-htitle' }, [t('feedbackHeading'), hmeta]), dismiss]),
      el(doc, 'div', { className: 'pp-fb-body' }, [
        el(doc, 'label', { for: ids + 'feedback', text: t('feedbackWhat') }), text,
        el(doc, 'label', { for: ids + 'where' }, [t('feedbackWhere') + ' ', optional(t('optional'))]), where,
        el(doc, 'label', { for: ids + 'expected' }, [t('feedbackExpected') + ' ', optional(t('optional'))]), expected,
        el(doc, 'label', null, [t('feedbackFiles') + ' ', optional(t('feedbackFilesNote'))]), drop, list,
        el(doc, 'label', { for: ids + 'email' }, [t('feedbackEmail') + ' ', optional(t('feedbackEmailNote'))]), email,
        honeypot, status, thanks
      ]),
      el(doc, 'div', { className: 'pp-fb-foot' }, [el(doc, 'span', { className: 'pp-fb-links' }, [all, mailLine]), send])
    ]);
    const root_ = el(doc, 'div', { className: 'pp-fb-overlay', hidden: true }, [form]);
    return { overlay: root_, form, text, where, expected, picker, browseButton, drop, list, email, status, thanksText, all, mailSep, mail, mailLine, send, dismiss, hmeta };
  }
  function say(message, isError) { fb.status.textContent = message || ''; fb.status.classList.toggle('error', !!isError); }
  function stamp() { return new Date().toISOString().slice(0, 19).replace(/[-:]/g, '').replace('T', '-'); }
  function renderFiles() {
    for (const url of objectURLs) win.URL.revokeObjectURL(url);
    objectURLs.clear();
    fb.list.replaceChildren(...attached.map((file, i) => {
      const remove = el(doc, 'button', { type: 'button', className: 'pp-fb-fremove', 'aria-label': t('remove', { name: file.name }), text: '×' });
      remove.addEventListener('click', () => { attached.splice(i, 1); renderFiles(); say(''); }, { signal: life.signal });
      let img = null;
      if (/^image\//.test(file.type) && win.URL && win.URL.createObjectURL) {
        const url = win.URL.createObjectURL(file);
        objectURLs.add(url);
        img = el(doc, 'img', { src: url, alt: '' });
      }
      return el(doc, 'li', { title: file.name + ' (' + Math.max(1, Math.round(file.size / 1000)) + ' KB)' },
        [img, el(doc, 'span', { className: 'pp-fb-fname', text: file.name }), remove]);
    }));
  }
  function attach(files) {
    let skipped = 0;
    Array.prototype.forEach.call(files || [], (file, i) => {
      if (attached.length >= MAX_FILES || file.size > MAX_BYTES) { skipped++; return; }
      // a pasted screenshot arrives as "image.png": a name that says what it is
      if (/^image\.\w+$/.test(file.name)) file = new File([file], 'screenshot-' + stamp() + (i ? '-' + (i + 1) : '') + '.' + file.name.split('.').pop(), { type: file.type });
      attached.push(file);
    });
    renderFiles();
    say(skipped ? t('skipped', { count: skipped }) : '', skipped > 0);
  }
  function readDataURL(file) {
    return new Promise((resolve, reject) => {
      const reader = new win.FileReader();
      reader.onload = () => resolve(reader.result);
      reader.onerror = () => reject(reader.error);
      reader.readAsDataURL(file);
    });
  }
  function resetForm() {
    fb.form.reset();
    const prefill = opts.defaultEmail || signedInAs;
    if (prefill) fb.email.value = prefill;
    fb.form.classList.remove('sent');
    attached = []; renderFiles(); say(''); fb.send.disabled = false;
  }
  function closeForm() {
    clearLater(closeTimer);
    if (!fb) return;
    fb.overlay.hidden = true;
    if (fb.form.classList.contains('sent')) resetForm();
  }
  function prefillEmail(info) {
    const fill = (value) => { if (value && !fb.email.value) fb.email.value = value; };
    if (opts.defaultEmail) { fill(opts.defaultEmail); return; }
    if (typeof opts.onFeedback === 'function' || !info.url || !/\/Form$/.test(info.url)) return;
    // a reader signed in to the Wolfram Cloud: Me, beside the form, answers with their Wolfram ID
    if (signedInAs !== undefined) { fill(signedInAs); return; }
    signedInAs = null;
    win.fetch(info.url.replace(/\/Form$/, '/Me'), { credentials: 'same-origin', cache: 'no-store' })
      .then((r) => (r.ok ? r.json() : null))
      .then((j) => { signedInAs = j && typeof j.email === 'string' ? j.email : null; fill(signedInAs); }, () => {});
  }
  function openForm(tool) {
    const info = feedbackInfo(tool);
    if (!fb) {
      fb = buildForm();
      doc.body.appendChild(fb.overlay);
      on(fb.browseButton, 'click', () => fb.picker.click());
      on(fb.picker, 'change', () => { attach(fb.picker.files); fb.picker.value = ''; });
      for (const type of ['dragenter', 'dragover']) {
        on(fb.form, type, (e) => {
          if (e.dataTransfer && Array.prototype.indexOf.call(e.dataTransfer.types, 'Files') >= 0) { e.preventDefault(); fb.drop.classList.add('dragging'); }
        });
      }
      on(fb.form, 'dragleave', (e) => { if (!fb.form.contains(e.relatedTarget)) fb.drop.classList.remove('dragging'); });
      on(fb.form, 'drop', (e) => {
        fb.drop.classList.remove('dragging');
        if (e.dataTransfer && e.dataTransfer.files.length) { e.preventDefault(); attach(e.dataTransfer.files); }
      });
      on(fb.form, 'submit', (e) => { e.preventDefault(); submit(); });
      on(fb.dismiss, 'click', closeForm);
      on(fb.overlay, 'click', (e) => { if (e.target === fb.overlay) closeForm(); });
      // an Escape the form closes on goes no further, to a host's own Escape
      on(fb.overlay, 'keydown', (e) => { if (e.key === 'Escape') { e.preventDefault(); e.stopPropagation(); closeForm(); } });
      on(fb.mail, 'click', () => { closeForm(); showNote(fb.info); });
    }
    fb.info = info;
    fb.hmeta.textContent = info.title;
    fb.thanksText.textContent = info.thanks || t('thanks');
    fb.all.hidden = !info.all;
    if (info.all) fb.all.href = info.all;
    fb.mailLine.hidden = !info.to;
    if (info.to) { fb.mail.href = mailto(info); fb.mail.textContent = info.to; fb.mailSep.textContent = info.all ? t('orEmail') : t('emailOnly'); }
    clearLater(closeTimer);
    if (fb.form.classList.contains('sent')) resetForm();
    fb.overlay.hidden = false;
    fb.text.focus();
    prefillEmail(info);
  }
  function sent() {
    fb.form.classList.add('sent');
    closeTimer = later(closeForm, 2500);
  }
  function failText() { return fb.info.to ? t('failedEmail', { to: fb.info.to }) : t('failed'); }
  async function submit() {
    const info = fb.info, text = fb.text.value.trim(), email = fb.email.value.trim();
    if (!text) { say(t('needText'), true); fb.text.focus(); return; }
    if (email && !/^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(email)) { say(t('badEmail'), true); fb.email.focus(); return; }
    fb.send.disabled = true;
    say(t('sending'));
    if (typeof opts.onFeedback === 'function') {
      try {
        const attachments = [];
        for (const file of attached) attachments.push({ name: file.name, base64: await readDataURL(file) });
        const answer = await opts.onFeedback({
          feedback: fb.text.value, where: fb.where.value, expected: fb.expected.value, email,
          title: info.title, view: info.view, page: info.page, paclet: info.paclet, feedbackUrl: info.url, attachments
        });
        if (answer && answer.submitted) sent();
        else { fb.send.disabled = false; say((answer && answer.error) || failText(), true); }
      } catch (e) { fb.send.disabled = false; say(failText(), true); }
      return;
    }
    // the site's feedback form: the answers, up to three files with their names, and the page's
    // identity; the form answers a stored submission with a page carrying #pp-feedback-thanks
    const data = new win.FormData(fb.form);
    attached.forEach((file, i) => data.append('Attachment' + (i + 1), file, file.name));
    data.append('AttachmentNames', attached.map((file) => file.name).join('\n'));
    data.append('Page', info.page);
    data.append('Title', info.title);
    data.append('Link', opts.pageLink || win.location.href);
    data.append('View', info.view);
    data.append('Notebook', opts.notebookURL || '');
    try {
      const body = await (await win.fetch(info.url, { method: 'POST', body: data, credentials: 'same-origin' })).text();
      if (body.indexOf('id="pp-feedback-thanks"') >= 0) sent();
      else { fb.send.disabled = false; say(body.indexOf('has-error') >= 0 ? t('badField') : failText(), true); }
    } catch (e) { fb.send.disabled = false; say(failText(), true); }
  }

  // ---- the switch between a page and its source ------------------------------------------------------
  async function toggleView(button) {
    const currentView = button.getAttribute('data-view') || 'built', nextView = currentView === 'source' ? 'built' : 'source';
    const detail = {
      uri: 'paclet:' + (root.getAttribute('data-paclet-page') || '') + '/' + (root.getAttribute('data-paclet-id') || ''),
      paclet: root.getAttribute('data-paclet-page') || '', page: root.getAttribute('data-paclet-id') || '',
      currentView, nextView, button
    };
    if (typeof opts.onToggleView !== 'function') { root.dispatchEvent(new win.CustomEvent('pp-toggle-view', { bubbles: true, detail })); return; }
    button.disabled = true;
    try { await opts.onToggleView(detail); } finally { button.disabled = false; }
  }

  // ---- the listeners, on the root and the document, made once ------------------------------------------
  on(root, 'click', (e) => {
    const target = e.target;
    if (!(target instanceof win.Element)) return;
    const layout = target.closest('[data-pp-layout]');
    if (layout && root.contains(layout)) {
      const kind = layout.getAttribute('data-pp-layout');
      if (kind === 'fullscreen-exit') setLayout('fullscreen', false, true);
      else if (kind === 'fullscreen') setLayout('fullscreen', !root.classList.contains('pp-fullscreen'), true);
      else if (kind === 'left') setLayout('left', root.classList.contains('pp-hide-left'), true);
      else if (kind === 'right') setLayout('right', root.classList.contains('pp-hide-right'), true);
      return;
    }
    const tool = target.closest('[data-pp-tool]');
    if (tool && root.contains(tool)) {
      const kind = tool.getAttribute('data-pp-tool');
      if (kind === 'viewtoggle') { e.preventDefault(); toggleView(tool); }
      else if (kind === 'feedback') {
        const info = feedbackInfo(tool);
        if (info.url) { e.preventDefault(); openForm(tool); }
        else if (info.to) showNote(info);
      } else if (kind === 'copylink') { e.preventDefault(); copyLink(tool); }
      return;
    }
    const a = target.closest('a[data-id]');
    if (a && (parts.left && parts.left.contains(a) || parts.right && parts.right.contains(a))) navigate(a, e);
  });
  on(root, 'input', (e) => { if (e.target === parts.filter) runFilter(true); });
  on(root, 'keydown', (e) => {
    if (e.target === parts.filter) {
      if (e.key === 'Escape') { parts.filter.value = ''; runFilter(true); parts.filter.blur(); e.stopPropagation(); }
      else if (e.key === 'Enter' && !e.isComposing) {
        const term = (parts.filter.value || '').trim().toLowerCase(), a = term && bestMatch(allLinks(), term);
        if (!a) return;
        e.preventDefault();
        navigate(a, null);
      }
      return;
    }
    const split = splitterFor(e.target);
    if (split && split.rail) {
      const step = e.shiftKey ? 40 : 12, w = split.rail.getBoundingClientRect().width;
      if (e.key === 'ArrowLeft') railWidth(split.rail, w + (split.fromLeft ? -step : step));
      else if (e.key === 'ArrowRight') railWidth(split.rail, w + (split.fromLeft ? step : -step));
      else return;
      e.preventDefault();
      storeWidth(split.rail);
    }
  });
  on(root, 'pointerdown', (e) => {
    const split = splitterFor(e.target);
    if (split && split.rail) startDrag(e, e.target, split.rail, split.fromLeft);
  });
  on(root, 'dblclick', (e) => {
    const split = splitterFor(e.target);
    if (split && split.rail) { split.rail.style.width = ''; state.set(widthKey(split.rail), ''); }
  });
  on(doc, 'keydown', (e) => {
    if (e.key === '/' && parts.filter && doc.activeElement !== parts.filter && !(doc.activeElement && /^(INPUT|TEXTAREA|SELECT)$/.test(doc.activeElement.tagName)) && !(doc.activeElement && doc.activeElement.isContentEditable)) {
      e.preventDefault(); parts.filter.focus();
    } else if (e.key === 'Escape') {
      if (fb && !fb.overlay.hidden) { closeForm(); e.stopPropagation(); }
      else if (root.classList.contains('pp-fullscreen')) { setLayout('fullscreen', false, true); e.stopPropagation(); }
    }
  });
  on(doc, 'paste', (e) => {
    const files = e.clipboardData && e.clipboardData.files;
    if (!fb || fb.overlay.hidden || !files || !files.length) return;
    e.preventDefault();
    attach(files);
  });

  // ---- the handle -----------------------------------------------------------------------------------------
  function refresh() {
    if (destroyed) return;
    const activeId = current ? current.getAttribute('data-id') : root.getAttribute('data-paclet-id');
    find();
    if (!symbolIndex || symbolIndex.rail !== parts.right) {
      if (symbolIndex) symbolIndex.dispose();
      symbolIndex = makeSymbolIndex(parts.right);
    }
    if (parts.filter) {
      const saved = state.get('filter', null), given = parts.filter.closest('[data-pp-filter]');
      if (typeof saved === 'string') parts.filter.value = saved;
      else if (given && !parts.filter.value) parts.filter.value = given.getAttribute('data-pp-filter') || '';
    }
    runFilter(false);
    applyLayout();
    applyWidths();
    setupFeedbackTool();
    if (activeId) setActive(activeId);
  }
  function destroy() {
    if (destroyed) return;
    destroyed = true;
    life.abort();
    for (const id of timers) win.clearTimeout(id);
    timers.clear();
    for (const url of objectURLs) win.URL.revokeObjectURL(url);
    objectURLs.clear();
    if (symbolIndex) symbolIndex.dispose();
    if (fb) fb.overlay.remove();
    if (note) note.remove();
    for (const node of added) node.remove();
    for (const nav of [parts.left, parts.right]) if (nav) railEmptyNote(nav, false);
    root.classList.remove('pp-resizing');
    mounted.delete(root);
  }
  const handle = { destroy, setActive: (id) => setActive(id), refresh };
  mounted.set(root, handle);
  refresh();
  return handle;
}

export default mountPacletShell;
