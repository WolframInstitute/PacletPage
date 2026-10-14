---
Template: Symbol
Name: DeployPacletPage
Context: WolframInstitute`PacletPage`
Paclet: WolframInstitute/PacletPage
URI: WolframInstitute/PacletPage/ref/DeployPacletPage
Keywords: [paclet, documentation, website, deploy, publish, cloud]
SeeAlso: [PacletPageCatalog, PacletPageFiles, DeployPacletPageFeedback, CheckPacletPage, SetPacletPagePermissions, PublishPacletPage]
RelatedGuides: [PacletPage]
RelatedTutorials: [PublishingADocumentationSite]
---

## Usage

`DeployPacletPage[catalog, name]` publishes the site for `catalog` under `name` in the connected cloud account, with its feedback form, and gives the URLs of what it published.

## Details & Options

- `name` is a cloud object name such as `"PAdic"` or `"Sites/PAdic"`.
- The site lives in the account as sibling objects: `name`, the site; `name <> "Page"`, the page its frame shows; `name <> "Feedback"`, the feedback form and the team's pages; and, with `"Source"`, `name <> "Source"`, the authoring notebooks.
- Both pages are published as UTF-8 HTML, the frame's page first, so the site never frames a page that is not there yet.
- With `"Source" -> dir`, the authoring notebooks under `dir`, and its `ResourceDefinition.nb`, are published beside the site, and each page gets a toggle between the built page and its source. Only notebooks that changed since the last publish are uploaded; notebooks that were removed are deleted. Cleanup only deletes objects strictly beneath the source directory.
- The following options can be given:

| Option | Default | Description |
|---|---|---|
| `"Feedback"` | `True` | whether to publish the feedback form and the team's pages |
| `"FeedbackOptions"` | `{}` | options for `DeployPacletPageFeedback` |
| `"Source"` | `None` | a paclet directory, whose authoring notebooks become the pages' sources |
| `"Upload"` | `Automatic` | `All` uploads every source notebook again, whatever changed |
| `"Account"` | `Automatic` | the Wolfram ID that must be connected |
| `Permissions` | `"Public"` | the permissions of the two pages and the source notebooks |

- `DeployPacletPage` also takes the options of `PacletPageFiles` that shape the site: `"Title"`, `"Subtitle"`, `"Accent"`, `"Home"`, `"FeedbackEmail"`, `"FeedbackThanks"` and `"DefinitionPage"`.
- One email setting serves the site and the feedback pages: the address of `"FeedbackEmail"` is the feedback pages' `"Email"` too, and an `"Email"` among `"FeedbackOptions"` gives the site its email fallback, unless each is given.
- The site works on the same origin as the pages it shows, `https://www.wolframcloud.com`, where both the Paclet Repository's notebooks and deployed resources are served.

## Basic Examples

The catalog of PacletPage's own published documentation:

```wl
catalog = PacletPageCatalog["https://www.wolframcloud.com/obj/wolframinstitute/DeployedResources/Paclet/WolframInstitute/PacletPage", "Documentation" -> PacletObject["WolframInstitute/PacletPage"]];
```

Publish its site in the connected account, with the authoring notebooks as the pages' sources and a feedback form whose entries the accounts of two email domains may read:

```wl
site = DeployPacletPage[catalog, "PacletPage",
    "Source" -> PacletObject["WolframInstitute/PacletPage"]["Location"],
    "FeedbackOptions" -> {"Domains" -> {"wolframinstitute.org", "wolfram.com"}}]
```

The site:

```wl
Hyperlink[site["Site"]]
```

The authoring notebooks it published, and how many changed since the last time:

```wl
site["Source"]
```

The feedback form and the team's pages:

```wl
site["Feedback"]
```

## Possible Issues

The catalog of PacletPage's own published documentation:

```wl
catalog = PacletPageCatalog["https://www.wolframcloud.com/obj/wolframinstitute/DeployedResources/Paclet/WolframInstitute/PacletPage"];
```

A site is published only from the account `"Account"` names; any other connected account is refused, and nothing is published:

```wl
DeployPacletPage[catalog, "PacletPage", "Account" -> "someone@example.org"]
```
