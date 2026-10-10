# PacletPage

Build and publish a paclet's documentation, from the markdown in its `docs/` folder to a published
paclet with a documentation site, without a front end and without scripts of the paclet's own.

| Function | |
|---|---|
| `BuildPacletDocumentation[dir]` | `docs/**/*.md` into `Documentation/English` notebooks with MarkdownToNotebook, `ResourceDefinition.md` into the definition notebook; unchanged pages from a cache, the rest on parallel kernels when asked, examples evaluated afresh when the code they run changes |
| `DeployPacletResource[dir]` | a fresh PacletBuild checked to hold every documentation page and to load alone, then the paclet resource deployed to the cloud (hero image kept, version and commit recorded, archive optionally published for `PacletInstall`) |
| `SubmitPacletResource[dir]` | the same build, the definition notebook checked, submitted to the Paclet Repository headless |
| `PublishPacletPage[dir, name]` | all of it and the site, in one kernel and one account; `"Submit" -> True` submits too |
| `PacletPageCatalog[resource]` | the published pages, the notebook behind each, the guide tree |
| `PacletPageFiles[catalog]` | the site's two pages, for a local preview |
| `DeployPacletPage[catalog, name]` | publish the site and its feedback in the connected account |
| `DeployPacletPageFeedback[name]` | publish the feedback form and the team's pages |
| `PacletPageFeedback[name]` | the collected feedback, with statuses |
| `SetPacletPageFeedbackStatus[name, id, status, note]` | triage an entry |
| `SetPacletPagePermissions[dir, permissions]` | set permissions over a cloud tree, directory by directory |
| `CheckPacletPage[catalog, name]` | check, as an anonymous reader, that everything is reachable |

The site frames the published pages: a filter over every title and keyword, the definition
notebook, the guide tree and the tutorials in the left rail, the symbols in the right rail, and each
page rendered in the middle from the notebook behind it, painted at once from the cloud's static
render and then made live by the Wolfram notebook embedder. Links to the paclet's own pages stay
inside the site; any other link opens in a new tab. Every page has a Feedback button, whose panel
links to the team's list of everything collected, and a toggle to the page's source.

## Use

A paclet's whole release, from a kernel connected to the account that publishes it:

```wl
PacletDirectoryLoad["path/to/PacletPage"]
Needs["WolframInstitute`PacletPage`"]

PublishPacletPage["path/to/MyPaclet", "MyPaclet",
    "Account" -> "publisher@example.org",
    "SiteOptions" -> {"FeedbackOptions" -> {"Domains" -> {"wolframinstitute.org"}}}]
```

`"BuildOptions"` reach `BuildPacletDocumentation` (`"Source"` for markdown outside `docs/`,
`"Definition" -> None` for a definition notebook made another way, `"ConverterOptions"`,
`"Parallel" -> n` and `"TimeConstraint"` for a large paclet, `"CacheDirectory"` for a cache a
build server keeps between runs). `"ResourceOptions"` reach `DeployPacletResource` and the
submission: `"Exclude"` for files the archive leaves out (a symbolic link that leads outside the
paclet is refused otherwise), `"Parallel" -> n` and `"Incremental" -> True` for a large paclet's
pages, `"ArchiveCheck"` for a check of the paclet's own. `"DryRun" -> True` builds without
deploying anything, and without a cloud session it builds and loads the archive offline.

PacletPage installs from its resource, in a fresh kernel for the newest release:

```wl
PacletInstall[ResourceObject["https://www.wolframcloud.com/obj/wolframinstitute/DeployedResources/Paclet/WolframInstitute/PacletPage"]]
```

For a paclet already published, the site alone, from a shell:

```bash
wl -f publish.wls WolframInstitute/TuringMachine TuringMachine --domains wolframinstitute.org --email docs@wolframinstitute.org
```

To preview a site before publishing it, serve `PacletPageFiles[catalog]` from the kernel with
`LocalDeploy` (Paclet Repository: `ToneAr/LocalDeploy`); the tutorial *Publishing a Documentation
Site* does, and publishes PacletPage's own site.

## What a site is in the cloud

| Object | Permissions | |
|---|---|---|
| `<name>` | public | the site |
| `<name>Page` | public | the page the site's frame shows |
| `<name>Feedback/Form`, `Me` | public | the feedback form; the signed-in reader's ID, for the site's pages only |
| `<name>Feedback/All`, `File`, `SetStatus` | signed-in team | the entries, their attachments, their statuses |
| `<name>Feedback/config`, `status`, `attachments/` | private | the Databin's id, the statuses, the files |
| `<name>Source/`, `<name>SourceManifest` | public, private | the authoring notebooks and what was uploaded |

The site must live on `https://www.wolframcloud.com`, the origin of the notebooks it renders.

## What it fixes

PacletPage replaces seven hand-copied site builders. Among the fixes it starts with:

- pages of `$`-named symbols open like any other;
- the frame is navigated without adding history entries, so Back steps through pages;
- only links to the paclet's own pages are taken over, System documentation opens in a new tab;
- pages are ASCII with character references, and are exported as UTF-8 HTML;
- feedback text is stored in the Databin as ASCII: a non-ASCII submission made `DatabinAdd` fail
  although it stored the entry;
- the panel recognizes a stored submission by its element's id, so a failure page is never taken
  for a thank-you;
- the Feedback button shows whenever there is a form or an address;
- the `Me` endpoint answers only the site's own pages, matched exactly, not by prefix;
- cleanup deletes only objects strictly beneath its own directory;
- directories are enumerated by listing subdirectories, never by probing each object, and no
  permissions pass is skipped;
- no personal account is named anywhere: the account is whichever is connected, guarded by
  `"Account"`.

## Layout

```
PacletPage/
|-- PacletInfo.wl
|-- Kernel/
|   |-- PacletPage.wl     the package: usage, messages, loading
|   |-- Catalog.wl        the published pages and the guide tree
|   |-- Site.wl           the site's HTML from the templates
|   |-- Cloud.wl          publishing the site, source notebooks, permissions
|   |-- Feedback.wl       the form, the Databin, the team's pages
|   |-- Check.wl          reachability checks
|   `-- Publish.wl        markdown -> notebooks -> resource -> Paclet Repository, and the release
|-- Assets/Web/           site.html, site.css, site.js, page.html
|-- Assets/Example/       a small paclet the documentation builds and dry-runs
|-- docs/                 the documentation, in markdown
|-- Tests/PacletPage.wlt  offline tests
|-- ResourceDefinition.md the paclet resource's definition
|-- build.wls             BuildPacletDocumentation of PacletPage itself
|-- deploy.wls            DeployPacletResource of PacletPage itself (--dry-run)
|-- release.wls           PublishPacletPage of PacletPage itself (--dry-run)
|-- run-tests.wls
`-- publish.wls           publish and check a site from the command line
```

## Build, test and release

```bash
wl -f run-tests.wls    # offline: no network, no cloud
wl -f release.wls      # the whole release: documentation, resource, archive, site, check
```

PacletPage releases itself with `PublishPacletPage`, into the account `PACLETPAGE_ACCOUNT` (by
default the Wolfram Institute's; any other connected account is refused). Its tutorial *Publishing
a Documentation Site* evaluates every cell, so building it publishes PacletPage's own site from the
resource already deployed; a first release into an account therefore runs `deploy.wls` before
`release.wls`. The build caches each page's notebook under a key of everything it depends on, in
`build/cache`, and converts a page with the converter's example cache off whenever the code its
examples last ran against has changed.

`DeployPacletResource` checks the fresh archive holds every documentation page before it deploys,
and works around the stock deploy: it drops the `AppearanceElements` option where the account's
subscription rejects it (probed on each run), defines the missing `htmlFragmentExport` the resource
page's Details section needs, scrapes the definition notebook's data and deploys with the notebook
open (so the page has its examples, hero image and source link), warms only the renders the
resource page inlines (the main guide and the examples) rather than every notebook, and replaces
the resource page, main guide and example notebooks in place instead of deleting them first. The documentation directory is still replaced whole. It
refuses a version already deployed from another commit (the resource records both in its
MetaInformation), a symbolic link that leads outside the paclet (a runtime linked from a toolchain
would be packed whole), and a paclet that declares LibraryLink libraries without shipping any. A
paclet that declares no symbols gets its public ones declared in the archive's `PacletInfo.wl`
(never its own), so the resource page's examples link them and copied code runs in a fresh
kernel; the deployed resource has the paclet's version.
`SubmitPacletResource` makes the Paclet Repository submission the definition notebook's dialog
would, headless: the notebook checked, the archive built (from the authoring notebooks, which a 15.1
build needs) and checked, the scrape read with the notebook open, the version matched.
