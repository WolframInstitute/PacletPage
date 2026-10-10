---
Template: Paclet
ResourceType: Paclet
Name: WolframInstitute/PacletPage
Context: WolframInstitute`PacletPage`
Paclet: WolframInstitute/PacletPage
Description: Build and publish a paclet's documentation, from markdown to a published paclet with its documentation site
ContributedBy: Nikolay Murzin, Claude (Anthropic)
Keywords: [paclet, documentation, website, Paclet Repository, cloud, feedback, permissions, markdown, release]
MainGuide: Documentation/English/Guides/PacletPage.nb
License: MIT
WolframVersion: 14.3+
Categories: [Cloud & Deployment, Notebook Documents & Presentation]
Disclosures: [LocalFiles, WolframAccount]
SourceControlURL: https://github.com/WolframInstitute/PacletPage
Links: ["[PacletPage's own documentation site, published by PacletPage](https://www.wolframcloud.com/obj/wolframinstitute/PacletPage)", "[MarkdownToNotebook, the converter PacletPage builds with](https://github.com/WolframInstitute/MarkdownToNotebook)", "[Wolfram Paclet Repository](https://resources.wolframcloud.com/PacletRepository/)", "[Paclets (Wolfram Language guide)](https://reference.wolfram.com/language/guide/Paclets.html)", "[PacletBuild (PacletTools)](https://reference.wolfram.com/language/PacletTools/ref/PacletBuild.html)"]
RelatedResources: [ToneAr/LocalDeploy]
---

## Details & Options

- PacletPage takes a paclet from the markdown in its `docs/` folder to a published paclet with a documentation site: the documentation notebooks, the paclet resource deployed to the cloud, the submission to the Paclet Repository, and the site, each step without a front end.
- The site frames the published pages: a filter over every title and keyword, the definition notebook, the guide tree and the tutorials in one rail, the symbols in another, and each page rendered from the notebook behind it, with the links between pages kept inside the site.
- Every page has a feedback panel and a toggle to its source. What readers send is stored in a Databin of the publishing account, and the team reads it on a page of its own, open to the account, named readers and whole email domains.
- The site works around pages published either on the Paclet Repository or as a paclet resource deployed to a cloud account.
- A large paclet builds on parallel kernels and processes, and only what changed: each page's notebook is cached under a key of everything it depends on, in a directory a build server can keep between runs.
- A release is checked before anything is published: the archive must hold every documentation page and load alone in a fresh kernel, a version already published from another commit is refused, and nothing outside the paclet is packed.
- Publishing uses the connected Wolfram Cloud account: the resource, the site, its feedback form and the team's pages are cloud objects, and the feedback is kept in a Databin.

## Usage

[BuildPacletDocumentation]() converts a paclet's markdown into documentation notebooks, [DeployPacletResource]() deploys its resource, [SubmitPacletResource]() submits it to the Paclet Repository, and [PublishPacletPage]() does all of it and publishes the site. [PacletPageCatalog]() reads the published pages of a paclet, [PacletPageFiles]() makes the site's pages, [DeployPacletPage]() publishes them with a feedback form, [PacletPageFeedback]() and [SetPacletPageFeedbackStatus]() triage the feedback, and [CheckPacletPage]() and [SetPacletPagePermissions]() keep the site working.

## Basic Examples

The catalog of a paclet on the Paclet Repository: its pages, the notebook behind each, and its guide tree:

```wl
PacletPageCatalog["Wolfram/AccessibleColors"]
```

---

The two pages of a site for that paclet, by size in characters:

```wl
PacletPageFiles[PacletPageCatalog["Wolfram/AccessibleColors"]] // Map[StringLength]
```

---

Check a published site, PacletPage's own, as an anonymous reader would:

```wl
CheckPacletPage[PacletPageCatalog["https://www.wolframcloud.com/obj/wolframinstitute/DeployedResources/Paclet/WolframInstitute/PacletPage"], "https://www.wolframcloud.com/obj/wolframinstitute/PacletPage"]
```

## Scope

### Documentation from markdown

A copy of the small example paclet that comes with PacletPage, whose documentation is markdown under `docs/`:

```wl
dir = CopyDirectory[PacletObject["WolframInstitute/PacletPage"]["AssetLocation", "Example"], FileNameJoin[{$TemporaryDirectory, CreateUUID["Greetings-"]}]];
```

Its documentation notebooks and its definition notebook:

```wl
BuildPacletDocumentation[dir]
```

```wl
DeleteDirectory[dir, DeleteContents -> True]
```

### A deployed paclet

The catalog of PacletPage's own resource, deployed to the Wolfram Institute's cloud account:

```wl
catalog = PacletPageCatalog["https://www.wolframcloud.com/obj/wolframinstitute/DeployedResources/Paclet/WolframInstitute/PacletPage"]
```

Its pages, by kind; the resource page is a page of its own:

```wl
GroupBy[catalog["Pages"], #Kind &, Keys]
```

## Hero Image

PacletPage's own documentation site, which PacletPage built and published: a reference page with its Feedback panel open, the markdown the page was built from, and the resource page. Loaded from the paclet's registered `Hero` asset:

```wl
Import[PacletObject["WolframInstitute/PacletPage"]["AssetLocation", "Hero"], "WXF"]
```

## Author Notes

This paclet was written with substantial help from an AI assistant (Claude Opus 5.5, Anthropic): the code, the documentation and the examples were drafted by the model and reviewed by the human contributor. It replaces the build and publishing scripts that had been copied by hand between several Wolfram Institute paclets, with the fixes each copy had found.
