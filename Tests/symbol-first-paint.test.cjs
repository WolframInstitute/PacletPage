// Run with wolframscript, Chrome and playwright-core: node --test Tests/symbol-first-paint.test.cjs
const test = require('node:test');
const assert = require('node:assert/strict');
const { execFileSync } = require('node:child_process');
const { createServer } = require('node:http');
const path = require('node:path');
const { chromium } = require('playwright-core');

function buildShell() {
  const source = `
    Get["Kernel/PacletPage.wl"];
    names = Join[{"0Before", "$Setting", "Zebra"}, "Apple" <> IntegerString[#, 10, 2] & /@ Range[30],
        "Apricot" <> IntegerString[#, 10, 2] & /@ Range[30]];
    pages = AssociationMap[Function[id, With[{name = StringDrop[id, 4]}, <|
        "Kind" -> "ref", "Name" -> name, "Title" -> name, "Keywords" -> {}, "Guides" -> {},
        "HTML" -> "./" <> id <> ".html", "Notebook" -> "./" <> id <> ".nb"|>]], "ref/" <> # & /@ names];
    catalog = <|"Name" -> "Test/Index", "Kind" -> "Local", "Title" -> "Index", "RootGuide" -> "guide/Home",
        "Resource" -> "./home.html", "DocBase" -> "./", "Pages" -> pages|>;
    WriteString["stdout", PacletPageFiles[catalog, "PageURL" -> ""]["index.html"]];
  `;
  return execFileSync('wolframscript', ['-code', source], {
    cwd: path.join(__dirname, '..'), encoding: 'utf8'
  });
}

test('The streamed shell paints its first letter and batch before the catalogs, script and web fonts', async () => {
  const html = buildShell();
  const railEnd = html.indexOf('</nav>') + '</nav>'.length;
  const script = html.indexOf('<script type="application/json"');
  assert.ok(railEnd > 6 && script > railEnd);
  let response;
  const server = createServer((request, stream) => {
    stream.setHeader('Content-Type', 'text/html; charset=utf-8');
    stream.setHeader('Cache-Control', 'no-store');
    if (request.url !== '/') return stream.end('<title>Reference page</title>');
    response = stream;
    stream.write(html.slice(0, railEnd));
  });
  await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
  let browser;
  try {
    browser = await chromium.launch(process.env.CHROME_BIN ? { executablePath: process.env.CHROME_BIN } : { channel: 'chrome' });
    const page = await browser.newPage({ viewport: { width: 1200, height: 850 } });
    await page.route('https://www.wolframcdn.com/**', () => {});
    await page.goto('http://127.0.0.1:' + server.address().port + '/', { waitUntil: 'commit' });
    await page.waitForFunction(() => document.querySelector('.nav-symbols > :nth-child(45)'));
    await page.evaluate(() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve))));

    async function checkFirstBatch() {
      const rail = page.locator('#side-right');
      assert.ok(await rail.getByRole('button', { name: 'Jump to A', pressed: true }).isVisible());
      assert.equal(await rail.getByLabel('Symbol group').inputValue(), '');
      assert.equal(await rail.locator('.symbol-selection').innerText(), 'All A names');
      assert.equal(await rail.getByRole('status').innerText(), 'A · 60 symbols');
      assert.equal(await rail.locator('.symbol-total').innerText(), '63');
      assert.equal(await rail.locator('.nav-symbols > li').count(), 45);
      const first = rail.getByRole('link', { name: 'Apple01' });
      assert.ok(await first.isVisible());
      return (await first.boundingBox()).y;
    }

    const initialTop = await checkFirstBatch();
    assert.equal(await page.evaluate(() => typeof window.ppSite), 'undefined');
    assert.ok(await page.evaluate(() => performance.getEntriesByName('first-contentful-paint').length > 0));
    // Hold the shell a second time after every catalog byte and before its script.
    response.write(html.slice(railEnd, script));
    await page.waitForFunction(() => !!document.querySelector('#symbol-rest'));
    assert.equal(await checkFirstBatch(), initialTop);
    assert.equal(await page.locator('#side-right a[data-id="ref/Zebra"]').count(), 0);

    response.end(html.slice(script));
    await page.waitForFunction(() => window.ppSite?.has('ref/Zebra'));
    assert.equal(await checkFirstBatch(), initialTop);
    await page.getByRole('button', { name: 'Jump to Z' }).click();
    await page.getByRole('link', { name: 'Zebra' }).click();
    assert.equal(await page.evaluate(() => location.hash), '#ref/Zebra');

    for (const action of ['letter button', 'more', 'letter picker', 'symbol link']) {
      await page.setViewportSize({ width: 1200, height: action === 'letter picker' ? 530 : 850 });
      await page.goto('http://127.0.0.1:' + server.address().port + '/', { waitUntil: 'commit' });
      const rail = page.locator('#side-right');
      await rail.locator('.nav-symbols > :nth-child(45)').waitFor();
      if (action === 'letter button') await rail.getByRole('button', { name: 'Jump to Z' }).click();
      if (action === 'more') await rail.getByRole('button', { name: 'Show 15 more' }).click();
      if (action === 'letter picker') await rail.getByRole('combobox', { name: 'First letter' }).selectOption('$');
      if (action === 'symbol link') {
        await rail.getByRole('button', { name: 'Jump to Z' }).click();
        await rail.getByRole('link', { name: 'Apple01' }).click();
      }
      response.end(html.slice(railEnd));
      await page.waitForFunction(() => !!window.ppSite);
      const rows = rail.locator('.nav-symbols > li');
      if (action === 'letter button') assert.deepEqual(await rows.allTextContents(), ['Zebra']);
      if (action === 'letter picker') assert.deepEqual(await rows.allTextContents(), ['$Setting']);
      if (action === 'more') assert.equal(await rows.count(), 60);
      if (action === 'symbol link') assert.equal(await rail.locator('[aria-current="page"]').getAttribute('data-id'), 'ref/Apple01');
    }
  } finally {
    if (response && !response.writableEnded) response.end();
    if (browser) await browser.close();
    await new Promise(resolve => server.close(resolve));
  }
});
