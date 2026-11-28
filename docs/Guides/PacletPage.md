---
Template: Guide
Name: PacletPage
Title: Paclet Documentation Sites
Context: WolframInstitute`PacletPage`
Paclet: WolframInstitute/PacletPage
URI: WolframInstitute/PacletPage/guide/PacletPage
Description: Build, publish and maintain a paclet's documentation, from markdown to a site around its published pages.
Keywords: [paclet, documentation, website, Paclet Repository, cloud, feedback, permissions, markdown, submission]
RelatedGuides: [PacletDependencyManagement]
RelatedTutorials: [PublishingADocumentationSite, CachingADocumentationBuild, DependencyManagement, UsingTheDependencyDialog]
---

## Abstract

PacletPage takes a paclet from the markdown in its `docs/` folder to a published paclet with a documentation site. It converts the markdown into documentation notebooks, builds the paclet and deploys its resource to the cloud, submits it to the Paclet Repository, and publishes a site around the published pages: a filter over every page, the definition notebook, the guide tree and the tutorials in one rail, the symbols in another, each page rendered from the notebook behind it, with a feedback form, a page for the team that reads the feedback, and a toggle to each page's source. Every step runs without a front end, so a paclet's build and release need no scripts of their own.

## Functions

### From markdown to a published paclet
- `BuildPacletDocumentation` convert a paclet's markdown documentation into documentation notebooks
- `DeployPacletResource` build the paclet afresh and deploy its resource to the cloud
- `SubmitPacletResource` submit the paclet to the Paclet Repository
- `PublishPacletPage` all of it, and the site, in one call

### The documentation cache
- `PacletDefinitionScan` each symbol's fingerprint, and the graph of what uses what
- `PacletPageDependencies` the symbols a page names and reaches, and the code in its cache key

### The site
- `PacletPageCatalog` the pages of a published paclet or of a paclet directory, the notebook behind each, and the guide tree
- `PacletPageFiles` the site's pages, to preview locally or to publish elsewhere
- `PacletPageWebAssets` the shell's stylesheet and script, which a host such as Wolfram Studio loads to show a paclet's pages as its site does
- `BuildPacletPageSite` the site rendered on this computer, without the cloud, to preview the documentation
- `PacletPage` a page of a local or installed paclet's documentation as a three-column `XMLElement` shell with its live notebook
- `DeployPacletPage` publish the site, with its feedback form, in the connected cloud account

### Feedback
- `DeployPacletPageFeedback` publish the feedback form and the team's pages
- `SubmitPacletPageFeedback` submit documentation feedback from Wolfram Language to a paclet's published feedback form and Databin
- `PacletPageFeedback` the feedback a site has collected, with the status of each entry
- `SetPacletPageFeedbackStatus` record where a feedback entry stands

### Maintenance
- `SetPacletPagePermissions` set permissions on a cloud directory tree, directory by directory
- `CheckPacletPage` check that a site, its pages and its feedback form are reachable

### [Paclet Dependency Management](paclet:WolframInstitute/PacletPage/guide/PacletDependencyManagement)

- `PacletDependencyInformation` read the required and optional libraries declared by a paclet
- `PacletDependencyDialog` manage them in a desktop notebook
