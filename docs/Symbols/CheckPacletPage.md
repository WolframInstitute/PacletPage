---
Template: Symbol
Name: CheckPacletPage
Context: WolframInstitute`PacletPage`
Paclet: WolframInstitute/PacletPage
URI: WolframInstitute/PacletPage/ref/CheckPacletPage
Keywords: [check, reachability, maintenance, permissions]
SeeAlso: [DeployPacletPage, SetPacletPagePermissions, PacletPageCatalog]
RelatedGuides: [PacletPage]
RelatedTutorials: [PublishingADocumentationSite]
---

## Usage

`CheckPacletPage[catalog, name]` checks that the site `name`, the pages it frames and its feedback form are reachable, and gives a report.

## Details & Options

- Every request is made the way a reader's browser makes it: anonymously, without the kernel's cloud session.
- The checks: the site and its frame's page are public; each checked page's notebook renders; on the Paclet Repository, the pages still show the notebooks the catalog names; the feedback form and `Me` are public; and the team's pages are closed to anonymous readers.
- A notebook that answers 403 is not public: `SetPacletPagePermissions` repairs it. One that answers 404 is gone; on the Paclet Repository that means the paclet was updated, and the site has to be published again from a new catalog.
- The report has `"OK"`, `"Site"` and `"Checks"`, a `Dataset` with one row per check.
- `name` is a site of the connected account, or the URL of a site in any account.
- The following options can be given:

| Option | Default | Description |
|---|---|---|
| `"Pages"` | `10` | how many pages to check, the root guide first, or `All` |
| `"Feedback"` | `True` | whether to check the feedback form and the team's pages |

## Basic Examples

The catalog of PacletPage's own published documentation:

```wl
catalog = PacletPageCatalog["https://www.wolframcloud.com/obj/wolframinstitute/DeployedResources/Paclet/WolframInstitute/PacletPage"];
```

Check its site, every page of it:

```wl
report = CheckPacletPage[catalog, "https://www.wolframcloud.com/obj/wolframinstitute/PacletPage", "Pages" -> All]
```

Whether everything is reachable:

```wl
report["OK"]
```

The checks that did not pass:

```wl
report["Checks"][Select[! #OK &]]
```

## Options

### Feedback

The catalog of PacletPage's own published documentation:

```wl
catalog = PacletPageCatalog["https://www.wolframcloud.com/obj/wolframinstitute/DeployedResources/Paclet/WolframInstitute/PacletPage"];
```

Check the site and its pages alone:

```wl
CheckPacletPage[catalog, "https://www.wolframcloud.com/obj/wolframinstitute/PacletPage", "Feedback" -> False]
```
