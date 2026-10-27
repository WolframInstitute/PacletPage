(function () {
  // The site's shell: the navigation rails, the filter and one frame. Every documentation page
  // opens in the frame through the host page (page.html), which paints the cloud's static render
  // of the page's notebook and hydrates it with the notebook embedder. The configuration comes
  // from the #pp-config element PacletPageFiles writes.
  var C = JSON.parse(document.getElementById('pp-config').textContent),
      frame = document.getElementById('frame'),
      loading = document.getElementById('loading'),
      side = document.getElementById('side'),
      sideRight = document.getElementById('side-right'),
      resLink = document.getElementById('resourcelink'),
      PAGE = C.page ? new URL(C.page, location.href).href : '',
      cur = null,
      current = '';

  // every page the navigation lists, by id: guide/X, ref/X or tutorial/X, with X as it is
  // published, "$" and all
  var KNOWN = {}, links = document.querySelectorAll('#side a[data-id], #side-right a[data-id]');
  for (var i = 0; i < links.length; i++) {
    var lid = links[i].getAttribute('data-id');
    (KNOWN[lid] = KNOWN[lid] || []).push(links[i]);
  }

  // routes given relative to the site, a local site's, resolve against it
  C.routes = C.routes.map(function (r) {
    return /^[a-z][a-z0-9+.-]*:/i.test(r) ? r : new URL(r, location.href).href;
  });

  function safeDecode(s) {
    try { return decodeURIComponent(s); } catch (e) { return s; }
  }

  // the id of a page of this paclet that a URL addresses, when the navigation lists it: a
  // published page, a notebook, a reference.wolfram.com documentation URL or a paclet: URI
  function pageID(href) {
    if (!href) return null;
    var s = href.split('#')[0].split('?')[0];
    for (var i = 0; i < C.routes.length; i++) {
      if (s.indexOf(C.routes[i]) !== 0) continue;
      var m = s.slice(C.routes[i].length).match(/^(guide|ref|tutorial)\/([^\/]+?)(?:\.(?:html|nb))?$/);
      if (!m) continue;
      var id = m[1] + '/' + safeDecode(m[2]);
      if (KNOWN[id]) return id;
    }
    return null;
  }

  function pagePath(id) {
    var k = id.indexOf('/');
    return id.slice(0, k + 1) + encodeURI(id.slice(k + 1));
  }

  // the published page of id, for the header's link and for framing without a host page; the
  // definition notebook's published page is the resource page
  function publishedURL(id) {
    return id.indexOf('resource/') === 0 ? C.resource : C.docBase + pagePath(id) + '.html';
  }

  // the notebook behind id, relative to C.notebooks: named on its navigation link when the
  // published name differs from the id (the Paclet Repository's storage names), else the id
  function notebookOf(id) {
    var a = KNOWN[id] && KNOWN[id][0];
    return (a && a.getAttribute('data-nb')) || (pagePath(id) + '.nb');
  }

  function titleOf(id) {
    var a = KNOWN[id] && KNOWN[id][0];
    return a ? a.textContent.trim() : id.slice(id.indexOf('/') + 1);
  }

  function setTwin(id) {
    if (resLink) resLink.href = id ? publishedURL(id) : C.resource;
  }

  // The host page asks for these: whether a page is part of the site, its title and its
  // notebook. Same origin, so a direct call.
  window.ppSite = {
    has: function (id) { return !!KNOWN[id]; },
    title: titleOf,
    notebook: notebookOf,
    pageID: pageID
  };

  // A page the shell frames as it is published (the resource page as home, or every page when no
  // host page is deployed) carries the stock header, rail and footer; on the same origin they are
  // hidden by the stylesheet C.hide, injected into the frame.
  function injectHide() {
    try {
      var d = frame.contentDocument;
      if (!d || !d.head) return;
      if (!d.getElementById('pp-hide-chrome')) {
        var s = d.createElement('style');
        s.id = 'pp-hide-chrome';
        s.textContent = C.hide;
        d.head.appendChild(s);
      }
      hideEmptyExamples(d);
    } catch (e) {}
  }

  // The resource page always has an Examples section; one whose definition had no examples is
  // empty, and then hidden with its heading. A page that embeds its example notebook fills the
  // section as the notebook renders, so it is never hidden.
  function hideEmptyExamples(d) {
    var examples = d.getElementById('nbCollapseExampleNotebook');
    if (!examples) return;
    var embeds = d.documentElement.innerHTML.indexOf('ExampleNotebook_embed') >= 0;
    var empty = !embeds && !examples.textContent.replace(/\s+/g, '') && !examples.querySelector('img, svg, canvas, iframe');
    examples.style.display = empty ? 'none' : '';
    var heading = d.querySelector('[data-target="#nbCollapseExampleNotebook"]');
    if (heading) heading.style.display = empty ? 'none' : '';
  }

  // A link to a page of the site opens in the shell; a link elsewhere opens in a new tab, since
  // another site may refuse to be framed and the frame would go blank.
  function hookFrame() {
    try {
      var d = frame.contentDocument;
      if (!d) return;
      var as = d.querySelectorAll('a[href]');
      for (var i = 0; i < as.length; i++) {
        (function (a) {
          if (a.__pp) return;
          a.__pp = 1;
          var id = pageID(a.href);
          if (id) {
            a.addEventListener('click', function (e) { e.preventDefault(); load(id, true); });
          } else if (/^https?:$/.test(a.protocol) && a.origin + a.pathname !== location.origin + location.pathname) {
            a.target = '_blank';
            a.rel = 'noopener';
          }
        })(as[i]);
      }
    } catch (e) {}
  }

  function openAnc(a) {
    var n = a.parentNode;
    while (n && n !== side) { if (n.tagName === 'DETAILS') n.open = true; n = n.parentNode; }
  }
  function setActive(a) {
    if (cur) cur.classList.remove('active');
    cur = a;
    if (a) { a.classList.add('active'); openAnc(a); a.scrollIntoView({ block: 'nearest' }); }
  }
  // A guide appears under every guide that links it, so an id can match several links: take the
  // one nearest what is open (most <details> ancestors shared with the current link), then the
  // shallowest. A click passes its own link and skips this.
  function chain(a) {
    var c = [], n = a && a.parentNode;
    while (n && n !== side && n !== sideRight) { if (n.tagName === 'DETAILS') c.unshift(n); n = n.parentNode; }
    return c;
  }
  function findLink(id) {
    var hits = KNOWN[id] || [];
    if (hits.length < 2) return hits[0] || null;
    var ref = chain(cur), best = hits[0], bestScore = -1;
    for (var j = 0; j < hits.length; j++) {
      var c = chain(hits[j]), k = 0;
      while (k < c.length && k < ref.length && c[k] === ref[k]) k++;
      var score = k * 1000 - c.length;
      if (score > bestScore) { bestScore = score; best = hits[j]; }
    }
    return best;
  }
  // The frame is navigated with location.replace: setting its src would add an entry to the
  // window's history for every page, so Back would step through the frame instead of the pages.
  function show(url) {
    loading.classList.add('on');
    current = url;
    // The page being left may still be fetching the notebook renderer, dozens of scripts that hold
    // the browser's connections to the cloud; the next page would queue behind them, so they stop.
    try { frame.contentWindow.stop(); } catch (e) {}
    try { frame.contentWindow.location.replace(url); } catch (e) { frame.src = url; }
    watchFrame();
  }

  // A page framed without the host page (the resource page as home) builds its content as it
  // loads, and its load event can come many seconds later; so its chrome is hidden and its links
  // routed every quarter second from the moment it is framed until well after it has loaded.
  var watch = null;
  function watchFrame() {
    clearInterval(watch);
    if (hostPageQ()) return;
    var n = 0;
    watch = setInterval(function () {
      injectHide();
      hookFrame();
      if (++n >= 120) clearInterval(watch);
    }, 250);
  }

  function frameURL(id) {
    if (!PAGE) return publishedURL(id);
    var nb = notebookOf(id);
    return PAGE + '?p=' + encodeURIComponent(id) + (nb === pagePath(id) + '.nb' ? '' : '&nb=' + encodeURIComponent(nb));
  }

  // the host page hands link clicks up rather than navigating itself; only the framed page may
  window.addEventListener('message', function (e) {
    if (e.source !== frame.contentWindow || e.origin !== location.origin) return;
    if (e.data && typeof e.data.ppNav === 'string') load(e.data.ppNav, true);
  });

  function load(id, push, el) {
    if (!KNOWN[id]) return false;
    show(frameURL(id));
    setActive(el || findLink(id));
    setTwin(id);
    document.title = titleOf(id) + ' - ' + C.title;
    if (push && safeDecode(location.hash.slice(1)) !== id) history.pushState(null, '', '#' + id);
    return true;
  }
  function goHome(push) {
    if (C.home.type === 'page' && KNOWN[C.home.id]) {
      show(frameURL(C.home.id));
      setActive(findLink(C.home.id));
    } else {
      show(C.home.url);
      setActive(null);
    }
    setTwin(null);
    document.title = C.title;
    if (push && location.hash !== '') history.pushState(null, '', location.pathname + location.search);
  }

  function hostPageQ() {
    return PAGE && current.indexOf(PAGE) === 0;
  }

  frame.addEventListener('load', function () {
    loading.classList.remove('on');
    if (!hostPageQ()) { injectHide(); hookFrame(); watchFrame(); }
  });

  // ---- filter --------------------------------------------------------------------------------
  // Hide what does not match in place: a guide stays visible when it or anything below it
  // matches, and clearing the box restores the tree as it was.
  var qBox = C.filter === false ? null : document.getElementById('q'), openState = null;

  function railEmptyNote(root, on) {
    var note = root.querySelector('.nav-empty');
    if (on && !note) {
      note = document.createElement('div');
      note.className = 'nav-empty';
      note.textContent = 'no matches';
      root.appendChild(note);
    } else if (!on && note) { note.parentNode.removeChild(note); }
  }

  // the title and the page's keywords
  function matchText(a) {
    return (a.textContent + ' ' + (a.getAttribute('data-kw') || '')).toLowerCase();
  }
  // a term with spaces also matches the title with its spaces taken out
  function flat(s) { return s.replace(/\s+/g, ''); }
  function matches(a, term) {
    return matchText(a).indexOf(term) !== -1 ||
      (/\s/.test(term) && flat(a.textContent.toLowerCase()).indexOf(flat(term)) !== -1);
  }

  function filterRail(root, term) {
    if (!root) return;
    var lis = root.querySelectorAll('li'), i, li, a, n;
    for (i = 0; i < lis.length; i++) lis[i].__hit = false;
    var as = root.querySelectorAll('a[data-id]');
    for (i = 0; i < as.length; i++) {
      a = as[i];
      if (!term || matches(a, term)) {
        n = a.closest('li');
        while (n && root.contains(n)) {
          n.__hit = true;
          n = n.parentElement && n.parentElement.closest('li');
        }
      }
    }
    for (i = 0; i < lis.length; i++) { li = lis[i]; li.style.display = li.__hit ? '' : 'none'; }
    var ds = root.querySelectorAll('details');
    for (i = 0; i < ds.length; i++) {
      if (!term) { ds[i].open = !!(openState && openState.get(ds[i])); }
      else { li = ds[i].closest('li'); if (li && li.__hit) ds[i].open = true; }
    }
    // a group's label stays while its list shows a match, a separator while anything after it does
    var labels = root.querySelectorAll('.nav-label');
    for (i = 0; i < labels.length; i++) labels[i].style.display = !term || hitIn(labels[i].nextElementSibling) ? '' : 'none';
    var seps = root.querySelectorAll('.nav-sep');
    for (i = 0; i < seps.length; i++) {
      var shown = !term;
      for (n = seps[i].nextElementSibling; n && !shown; n = n.nextElementSibling) shown = hitIn(n);
      seps[i].style.display = shown ? '' : 'none';
    }
    railEmptyNote(root, !!term && lis.length > 0 && root.querySelectorAll('li[style*="none"]').length === lis.length);
  }

  function hitIn(el) {
    if (!el) return false;
    var lis = el.querySelectorAll('li');
    for (var i = 0; i < lis.length; i++) { if (lis[i].__hit) return true; }
    return false;
  }

  function runFilter() {
    var term = (qBox.value || '').trim().toLowerCase();
    if (term && !openState) {
      openState = new Map();
      var ds = side.querySelectorAll('details');
      for (var i = 0; i < ds.length; i++) openState.set(ds[i], ds[i].open);
    }
    filterRail(side, term);
    filterRail(sideRight, term);
    if (!term) openState = null;
  }

  // Enter opens the best entry the filter shows: a title equal to the term, then one that starts
  // with it, then one with a word that starts with it (after a separator or at a CamelCase step),
  // then one that contains it, last one that matches only on its keywords. Within a rank a guide
  // or tutorial comes before a symbol, a written tutorial before a placeholder, then the shorter
  // title.
  function wordStartAt(title, i) {
    if (i === 0) return true;
    var p = title.charAt(i - 1), c = title.charAt(i);
    return !/[A-Za-z0-9]/.test(p) || (/[a-z]/.test(p) && /[A-Z]/.test(c)) ||
      (/[0-9]/.test(p) !== /[0-9]/.test(c));
  }
  function titleRank(a, term) {
    var title = (a.textContent || '').trim(), t = title.toLowerCase(), i = t.indexOf(term);
    if (i === -1 && /\s/.test(term)) {
      var j = flat(t).indexOf(flat(term));
      if (j !== -1) return j > 0 ? 3 : flat(t) === flat(term) ? 0 : 1;
    }
    if (i === -1) return matches(a, term) ? 4 : -1;
    if (t === term) return 0;
    if (i === 0) return 1;
    for (; i !== -1; i = t.indexOf(term, i + 1)) { if (wordStartAt(title, i)) return 2; }
    return 3;
  }
  var KIND = { resource: 0, guide: 0, tutorial: 0, ref: 1 };
  function bestMatch(term) {
    var as = document.querySelectorAll('#side a[data-id], #side-right a[data-id]'), best = null, bk = null;
    for (var i = 0; i < as.length; i++) {
      var a = as[i], r = titleRank(a, term);
      if (r < 0) continue;
      var k = [r, KIND[a.getAttribute('data-id').split('/')[0]] || 0, a.classList.contains('stub') ? 1 : 0, (a.textContent || '').trim().length];
      for (var j = 0; j < k.length && bk; j++) { if (k[j] !== bk[j]) break; }
      if (!bk || (j < k.length && k[j] < bk[j])) { best = a; bk = k; }
    }
    return best;
  }

  if (qBox) {
    qBox.addEventListener('input', runFilter);
    qBox.addEventListener('keydown', function (e) {
      if (e.key === 'Escape') { qBox.value = ''; runFilter(); qBox.blur(); }
      else if (e.key === 'Enter' && !e.isComposing) {
        var term = (qBox.value || '').trim().toLowerCase(), a = term && bestMatch(term);
        if (!a) return;
        e.preventDefault();
        load(a.getAttribute('data-id'), true, a);
        closeDrawers();
        if (window.matchMedia && window.matchMedia('(max-width: 900px)').matches) qBox.blur();
      }
    });
    document.addEventListener('keydown', function (e) {
      if (e.key === '/' && document.activeElement !== qBox) { e.preventDefault(); qBox.focus(); }
    });
  }

  // ---- drawers (narrow screens) ----------------------------------------------------------------
  var scrim = document.getElementById('scrim'),
      navToggle = document.getElementById('navToggle'),
      symToggle = document.getElementById('symToggle');
  function closeDrawers() {
    side.classList.remove('open');
    if (sideRight) sideRight.classList.remove('open');
    if (scrim) scrim.classList.remove('on');
  }
  function toggleDrawer(el) {
    if (!el) return;
    var open = el.classList.contains('open');
    closeDrawers();
    if (!open) { el.classList.add('open'); if (scrim) scrim.classList.add('on'); }
  }
  if (navToggle) navToggle.addEventListener('click', function () { toggleDrawer(side); });
  if (symToggle) symToggle.addEventListener('click', function () { toggleDrawer(sideRight); });
  if (scrim) scrim.addEventListener('click', closeDrawers);
  document.addEventListener('keydown', function (e) { if (e.key === 'Escape') closeDrawers(); });

  // ---- rail splitters --------------------------------------------------------------------------
  // Drag a divider to rebalance the columns, double-click to restore it. The widths are kept per
  // site: every site on the cloud shares one origin, and so one localStorage.
  var main = document.getElementById('main'), DEFAULTS = { side: 340, sideRight: 240 },
      RAILS = 'pp-rails:' + location.pathname;
  function railWidth(el, px) {
    var min = el === side ? 200 : 160, max = Math.max(min, window.innerWidth * 0.45);
    el.style.width = Math.round(Math.min(max, Math.max(min, px))) + 'px';
  }
  function storeWidths() {
    try {
      localStorage.setItem(RAILS, JSON.stringify({
        side: parseInt(side.style.width, 10) || DEFAULTS.side,
        sideRight: sideRight ? (parseInt(sideRight.style.width, 10) || DEFAULTS.sideRight) : DEFAULTS.sideRight
      }));
    } catch (e) {}
  }
  (function restoreWidths() {
    try {
      var w = JSON.parse(localStorage.getItem(RAILS) || 'null');
      if (!w) return;
      if (w.side) railWidth(side, w.side);
      if (w.sideRight && sideRight) railWidth(sideRight, w.sideRight);
    } catch (e) {}
  })();
  function dragSplitter(handle, el, fromLeft) {
    if (!handle || !el) return;
    handle.addEventListener('pointerdown', function (e) {
      e.preventDefault();
      handle.setPointerCapture(e.pointerId);
      handle.classList.add('dragging');
      if (main) main.classList.add('resizing');
      var startX = e.clientX, startW = el.getBoundingClientRect().width;
      function move(ev) { railWidth(el, startW + (fromLeft ? ev.clientX - startX : startX - ev.clientX)); }
      function up(ev) {
        handle.releasePointerCapture(ev.pointerId);
        handle.classList.remove('dragging');
        if (main) main.classList.remove('resizing');
        handle.removeEventListener('pointermove', move);
        handle.removeEventListener('pointerup', up);
        storeWidths();
      }
      handle.addEventListener('pointermove', move);
      handle.addEventListener('pointerup', up);
    });
    handle.addEventListener('dblclick', function () {
      railWidth(el, el === side ? DEFAULTS.side : DEFAULTS.sideRight);
      storeWidths();
    });
    handle.addEventListener('keydown', function (e) {
      var step = e.shiftKey ? 40 : 12, w = el.getBoundingClientRect().width;
      if (e.key === 'ArrowLeft') { railWidth(el, w + (fromLeft ? -step : step)); }
      else if (e.key === 'ArrowRight') { railWidth(el, w + (fromLeft ? step : -step)); }
      else return;
      e.preventDefault();
      storeWidths();
    });
  }
  dragSplitter(document.getElementById('splitL'), side, true);
  dragSplitter(document.getElementById('splitR'), sideRight, false);

  function navClick(e) {
    var a = e.target.closest && e.target.closest('a[data-id]');
    if (!a) return;
    e.preventDefault();
    load(a.getAttribute('data-id'), true, a);
    closeDrawers();
  }
  side.addEventListener('click', navClick);
  if (sideRight) sideRight.addEventListener('click', navClick);
  var brand = document.getElementById('homelink');
  if (brand) brand.addEventListener('click', function (e) { e.preventDefault(); goHome(true); });
  window.addEventListener('popstate', function () {
    var id = safeDecode(location.hash.slice(1));
    if (!(id && load(id, false))) goHome(false);
  });

  var start = safeDecode(location.hash.slice(1));
  if (!(start && load(start, false))) goHome(false);

  // The embedder's renderer bundle (about half a megabyte, cached for a year) is fetched ahead,
  // through the cloud's embedding resolver for the root guide's notebook, so the first page does
  // not wait for it.
  (function prefetchEmbedder() {
    try {
      var i = C.prefetch ? C.prefetch.indexOf('/obj/') : -1;
      if (i < 0) return;
      var origin = C.prefetch.slice(0, i), path = C.prefetch.slice(i + 5);
      if (origin !== location.origin) return;
      var x = new XMLHttpRequest();
      x.open('GET', origin + '/notebooks/embedding?path=' + encodeURIComponent(safeDecode(path)), true);
      x.onload = function () {
        if (x.status !== 200) return;
        try {
          var d = JSON.parse(x.responseText);
          [d.mainScript].concat(d.otherScripts || []).forEach(function (sc) {
            if (typeof sc !== 'string' || sc.charAt(0) !== '/') return;
            var l = document.createElement('link');
            l.rel = 'prefetch'; l.href = origin + sc;
            document.head.appendChild(l);
          });
        } catch (e) {}
      };
      x.send();
    } catch (e) {}
  })();
})();
