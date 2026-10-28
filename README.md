# PacletPage

Build and publish a paclet's documentation, from the markdown in its `docs/` folder to a published
paclet with a documentation site, without a front end and without scripts of the paclet's own.

- **[Documentation](https://www.wolframcloud.com/obj/wolframinstitute/PacletPage)**: PacletPage's own
  site, which PacletPage builds and publishes, with a feedback form on every page
- **[Paclet resource](https://www.wolframcloud.com/obj/wolframinstitute/DeployedResources/Paclet/WolframInstitute/PacletPage)**:
  the resource page, with examples; install from it, in a fresh kernel for the newest release:

  ```wl
  PacletInstall[ResourceObject["https://www.wolframcloud.com/obj/wolframinstitute/DeployedResources/Paclet/WolframInstitute/PacletPage"]]
  ```

| Function | |
|---|---|
| `BuildPacletDocumentation[dir]` | `docs/**/*.md` into `Documentation/English` notebooks with [MarkdownToNotebook](https://github.com/WolframInstitute/MarkdownToNotebook), `ResourceDefinition.md` into the definition notebook; unchanged pages from a cache, the rest on parallel kernels when asked, examples evaluated afresh when the code they run changes |
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

## Features

- One call per release: `PublishPacletPage` builds the documentation, deploys the paclet resource,
  submits it to the Paclet Repository when asked and publishes the site, from one kernel connected
  to the publishing account; `"Account"` refuses any other.
- Documentation from markdown: the pages under `docs/` and `ResourceDefinition.md` become notebooks
  through MarkdownToNotebook, with their examples evaluated. Unchanged pages come from a cache that a
  build server can keep, the others convert on parallel kernels when asked, and examples run again
  when the code they run changes.
- Checked releases: nothing is published unless the archive holds every documentation page and
  loads alone in a fresh kernel. A version already published from another commit, a link leading
  outside the paclet, and LibraryLink libraries declared but not shipped are refused.
- A complete resource page from its first visit, on any cloud subscription: Details, examples, the
  main guide, the hero image and the source link, with the paclet's version and commit recorded.
  The examples link the paclet's symbols, and code copied from them installs the paclet once, then
  finds it by name. A paclet with several guides lists them as a tree beside every page.
- Deploys that scale: only the documentation notebooks and pages that changed are written again,
  and the pages are made over parallel kernels, each kept for the next deploy. A release can build
  in one process and deploy from that build in another.
- Paclet Repository submission without a front end, after the definition notebook's own checks and
  the release's.
- A documentation site around the published pages: a filter over every title and keyword, the
  definition notebook, the guides and the tutorials in the left rail, the symbols in the right, and
  each page painted at once from the cloud's static render, then made live. Every page has its own
  address, Back and Forward step through the pages read, links to the paclet's own pages stay in
  the site, and any other link opens in a new tab. A label under the site's name can show the
  paclet's version, and a footer the PacletPage the site was made with.
- Feedback on every page: text in any language and attachments, kept in a Databin of the
  publishing account and confirmed to the reader once stored, with an email fallback. The team
  reads and triages it, each entry with a status, set one at a time or many in one write, on pages
  open to the account, named readers and whole email domains.
- Every page's source a toggle away: the notebook the page was built from.
- A preview without the cloud: `BuildPacletPageSite` renders the documentation on this computer
  and writes the same site around it, keeping each render for the next build.
- Any published paclet: a site frames pages from the Paclet Repository or from a paclet resource
  deployed to a cloud account.
- Upkeep: `CheckPacletPage` checks, as an anonymous reader, that the site, its pages and its
  feedback form are reachable, and can send an example entry through the form, and `SetPacletPagePermissions` sets permissions over a whole cloud
  tree. Cleaning up removes only the site's own objects.

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
