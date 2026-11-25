// Run with Chrome and playwright-core: node --test Tests/symbol-index.test.cjs
const test = require('node:test');
const { chromium } = require('playwright-core');
const fs = require('node:fs');
const path = require('node:path');

const assets = path.join(__dirname, '../Assets/Web');
const read = name => fs.readFileSync(path.join(assets, name), 'utf8');

function checkRail() {
  const check = (condition, reason) => { if (!condition) throw Error(reason); };
  const rail = document.getElementById('side-right');
  const list = rail.querySelector('.nav-symbols');
  const active = () => list.querySelector('[aria-current="page"]')?.dataset.id;
  const prefix = rail.querySelector('.symbol-prefix select');
  const query = document.getElementById('q');
  const change = element => element.dispatchEvent(new Event('change'));
  const filter = value => { query.value = value; query.dispatchEvent(new Event('input')); };
  check(active() === 'ref/AzureTail', 'A late deep link must be mounted and selected');
  check(window.ppSite.has('ref/$Version'), 'The registry must retain off-screen links');
  const mobile = window.matchMedia('(max-width: 900px)').matches;
  if (mobile) document.getElementById('symToggle').click();
  const labels = Array.from(prefix.options, option => option.label);
  check(labels.includes('AbstractCategory… (28)') && labels.includes('AbundantQ (1)'),
    'Only a family with additional symbols gets a continuation mark');
  check(labels.includes('AdeleQ… (2)') && labels.includes('AdjointOperator… (2)') && labels.includes('ApproximateIdentity… (3)'),
    'Each family must use the complete name of its first link');
  const longName = 'AstronomicalObservationCatalogue';
  check(labels.includes(longName + ' (1)'), 'A long singleton keeps its full name');
  prefix.value = longName.toLowerCase(); change(prefix);
  const caption = rail.querySelector('.symbol-selection'), text = document.createRange();
  check(caption.textContent === longName + ' (1)' && list.firstElementChild.textContent === longName,
    'The selected singleton and its link must match');
  text.selectNodeContents(caption);
  let control = prefix.getBoundingClientRect();
  check(Array.from(text.getClientRects()).every(rect => rect.right <= control.right - 22 && rect.bottom <= control.bottom),
    'Even long names must stay inside the control and clear of the arrow');
  prefix.value = 'adjoint'; change(prefix);
  check(caption.textContent === 'AdjointOperator… (2)' && Array.from(list.querySelectorAll('a'), a => a.textContent).join(' ') ===
    'AdjointOperator AdjointRepresentation', 'A family must open its full symbol names');
  text.selectNodeContents(caption);
  control = prefix.getBoundingClientRect();
  const displayed = text.getBoundingClientRect();
  check(displayed.right <= control.right - 22 && displayed.bottom <= control.bottom,
    'The complete selected label must fit before the arrow');
  check(document.elementFromPoint(control.right - 15, control.top + control.height / 2) === prefix,
    'The arrow must activate the native picker');
  prefix.value = 'abstract'; change(prefix);
  check(list.children.length === 28, 'Abstract should contain all 28 family members');
  const category = list.querySelector('[data-id="ref/AbstractCategory"]');
  if (mobile) {
    const rect = category.getBoundingClientRect();
    check(category.contains(document.elementFromPoint(rect.x + 12, rect.y + rect.height / 2)),
      'The drawer backdrop must allow taps on symbol links');
  }
  category.click();
  check(location.hash === '#ref/AbstractCategory', 'The selected symbol must open in the shell');
  if (mobile) check(!rail.classList.contains('open'), 'Selecting a symbol must close the drawer');
  filter('no such symbol'); filter('');
  check(prefix.value === 'abstract' && list.children.length === 28, 'Clearing must restore the prefix');
  filter('package setting');
  query.dispatchEvent(new KeyboardEvent('keydown', { key: 'Enter' }));
  check(active() === 'ref/$Version', 'Enter must search keywords beyond the selected prefix');
  filter('');
  check(rail.querySelector('.symbol-letter-picker select').value === '$', 'The selected result must be revealed');
  const letter = rail.querySelector('.symbol-letter-picker select');
  letter.value = 'C'; change(letter);
  check(Array.from(prefix.options, option => option.label).includes('Other C names (48)'),
    'A crowded menu must keep its one-off names accessible together');
  prefix.value = ':other'; change(prefix);
  const more = rail.querySelector('.symbol-more');
  while (!more.hidden) more.click();
  check(list.children.length === 48 && !list.textContent.includes('Cauchy'), 'The remaining group must contain only the one-off names');
}

function sampleHTML() {
  const names = ['AbelianGroupQ', 'AbelTransform', 'AbsoluteValue', 'AbstractCategory', 'AbstractGroup', 'AbundantQ',
    'AdaptedProcessQ', 'AdditiveEnergy', 'AdeleQ', 'AdeleRing', 'AdjointOperator', 'AdjointRepresentation',
    'AstronomicalObservationCatalogue', 'CauchyIntegral', 'CauchySequence',
    ...Array.from({ length: 48 }, (_, i) => 'C' + String.fromCodePoint(0x4e00 + i) + 'Value'),
    'ApollonianGasket', 'ApolloniusCircles', 'ApproximateIdentity', 'ApproximateIntermediateValue', 'ApproximateSupremum',
    ...Array.from({ length: 26 }, (_, i) => 'AbstractSymbol' + i),
    ...Array.from({ length: 70 }, (_, i) => 'Algebra' + i), 'AzureTail'].sort().concat('$Version');
  const rows = names.map(name => '<li><a class="sym" data-id="ref/' + name + '" href="#ref/' + name + '"' +
    (name === '$Version' ? ' data-kw="package setting"' : '') + '>' + name + '</a></li>');
  const config = { title: 'Index test', resource: 'about:blank', page: 'viewer.html',
    home: { type: 'resource', url: 'about:blank' }, routes: [], filter: true };
  const initials = [...new Set(names.map(name => name[0]))];
  const letterButtons = initials.map(ch => '<button type="button" value="' + ch + '" aria-label="Jump to ' + ch + '">' + ch + '</button>').join('');
  const letterOptions = initials.map(ch => '<option value="' + ch + '">' + ch + '</option>').join('');
  const body =
    '<header id="hdr">' +
      '<button id="navToggle" type="button" aria-label="Guides and tutorials" aria-controls="side" aria-expanded="false">&#9776;</button>' +
      '<a class="brand" id="homelink" href="#">' + config.title + '</a>' +
      '<input id="q" type="search" autocomplete="off" spellcheck="false" />' +
      '<button id="symToggle" type="button" aria-label="Symbols" aria-controls="side-right" aria-expanded="false">&#8942;</button>' +
      '<a class="home" id="resourcelink" href="about:blank" target="_blank" rel="noopener">Resource page</a>' +
    '</header>' +
    '<div id="main">' +
      '<div id="scrim"></div>' +
      '<nav id="side-right" class="indexed" data-batch-size="45" aria-label="Symbols">' +
        '<div class="symbol-index">' +
          '<div class="symbol-heading"><h2>Symbol index</h2><span class="symbol-total">' + names.length + '</span></div>' +
          '<div class="symbol-browse">' +
            '<div class="symbol-letters" role="group" aria-label="First letter">' + letterButtons + '</div>' +
            '<div class="symbol-letter-picker"><label for="symbol-first-letter">First letter</label><div class="symbol-select"><select id="symbol-first-letter">' + letterOptions + '</select></div></div>' +
            '<div class="symbol-prefix"><label for="symbol-prefix">Symbol group</label><div class="symbol-select"><select id="symbol-prefix"><option value=""></option></select><span class="symbol-selection" aria-hidden="true"></span></div></div>' +
          '</div>' +
          '<div class="symbol-count" role="status"></div>' +
        '</div>' +
        '<div class="symbol-scroll"><ul class="nav-symbols">' + rows.join('') + '</ul><button class="symbol-more" type="button"></button></div>' +
      '</nav>' +
      '<nav id="side" aria-label="Guides and tutorials"></nav>' +
      '<div class="splitter" id="splitL" role="separator" aria-orientation="vertical" tabindex="0"></div>' +
      '<div id="content"><div id="loading">Loading...</div><iframe id="frame" title="documentation"></iframe></div>' +
      '<div class="splitter" id="splitR" role="separator" aria-orientation="vertical" tabindex="0"></div>' +
    '</div>';
  const slots = { title: config.title, bodyClass: '', css: read('site.css') + '\n* { transition: none !important; }',
    body, js: read('site.js'), config: JSON.stringify(config) };
  return read('site.html').replace(/`(\w+)`/g, (_, slot) => slots[slot] || '');
}

for (const width of [1200, 700]) {
  test('Alphabetical navigation at ' + width + ' pixels', async () => {
    const browser = await chromium.launch(process.env.CHROME_BIN ? { executablePath: process.env.CHROME_BIN } : { channel: 'chrome' });
    try {
      const page = await browser.newPage({ viewport: { width, height: 850 } });
      await page.route('http://pacletpage.test/**', route => route.fulfill({ contentType: 'text/html',
        body: new URL(route.request().url()).pathname === '/' ? sampleHTML() : '<title>Reference page</title>' }));
      await page.goto('http://pacletpage.test/#ref/AzureTail');
      await page.evaluate(checkRail);
    } finally { await browser.close(); }
  });
}
