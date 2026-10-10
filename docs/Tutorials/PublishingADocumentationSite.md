---
Template: TechNote
Name: PublishingADocumentationSite
Title: Publishing a Documentation Site
Context: WolframInstitute`PacletPage`
Paclet: WolframInstitute/PacletPage
URI: WolframInstitute/PacletPage/tutorial/PublishingADocumentationSite
Keywords: [paclet, documentation, website, publish, preview, feedback, maintenance, release]
RelatedGuides: [PacletPage]
---

## A site for these pages

A paclet's documentation, once published, is a set of separate pages. This page is one of them: PacletPage's own documentation, deployed as a paclet resource to the Wolfram Cloud. Each page is complete, but nothing ties them together: no page lists them all, there is no way to filter them, and a reader who follows a link lands on a page that has lost the way back. A documentation site gives the pages one frame: a filter over every title and keyword, the resource and the guides in one rail, the symbols in another, links that stay inside the site, a Feedback button and a toggle to each page's source.

This tutorial publishes that site for PacletPage itself. Building this page is the publishing run: every result below comes from it.

## The catalog

PacletPage's resource, in the account that publishes it:

```wl
resource = CloudObject["DeployedResources/Paclet/WolframInstitute/PacletPage"]
```

Everything starts from the catalog of its published pages. The built documentation, given as `"Documentation"`, adds each page's keywords for the site's filter:

```wl
catalog = PacletPageCatalog[resource, "Documentation" -> PacletObject["WolframInstitute/PacletPage"]]
```

The site's title, from the root guide:

```wl
catalog["Title"]
```

The pages, by kind:

```wl
GroupBy[catalog["Pages"], #Kind &, Keys]
```

The resource, the page the site opens on: its resource page, with the definition notebook as its source:

```wl
catalog["Pages", "resource/PacletPage"]
```

## A preview on this computer

The catalog of PacletPage's published pages, as above:

```wl
catalog = PacletPageCatalog[CloudObject["DeployedResources/Paclet/WolframInstitute/PacletPage"], "Documentation" -> PacletObject["WolframInstitute/PacletPage"]];
```

`PacletPageFiles` gives the site's two pages. Any web server can serve them; here the Wolfram Language serves them itself, with `LocalDeploy` from the Paclet Repository:

```wl
PacletInstall["ToneAr/LocalDeploy"];
Needs["ToneAr`LocalDeploy`"]
```

Each page goes out as HTML:

```wl
html[page_String] := HTTPResponse[page, <|"ContentType" -> "text/html; charset=utf-8"|>]
```

The site at `/`, the frame's page beside it:

```wl
preview = LocalDeploy[URLDispatcher[{"/page.html" -> html[PacletPageFiles[catalog]["page.html"]], "/" -> html[PacletPageFiles[catalog]["index.html"]]}]]
```

Opened in a browser, the preview is the site, served by this kernel while it is idle; the pages it renders come from the cloud. It answers here too:

```wl
URLRead[preview["URL"], "StatusCode"]
```

The cloud paints each page from its static render before the notebook comes alive, but only for a site on the cloud's own origin; the preview waits for the live notebook instead. Stop the preview when done:

```wl
DeleteObject[preview]
```

## Publishing

The catalog of PacletPage's published pages:

```wl
catalog = PacletPageCatalog[CloudObject["DeployedResources/Paclet/WolframInstitute/PacletPage"], "Documentation" -> PacletObject["WolframInstitute/PacletPage"]];
```

`DeployPacletPage` publishes the site in the connected account: its pages, PacletPage's authoring notebooks as their sources, and a feedback form whose entries the accounts of two email domains may read:

```wl
site = DeployPacletPage[catalog, "PacletPage",
    "Source" -> PacletObject["WolframInstitute/PacletPage"]["Location"],
    "FeedbackOptions" -> {"Domains" -> {"wolframinstitute.org", "wolfram.com"}}]
```

The site lives on `https://www.wolframcloud.com`, the origin that serves the notebooks it renders:

```wl
Hyperlink[site["Site"]]
```

## Feedback

Every page has a Feedback button. It opens a panel where a reader says what is wrong, where, and what they expected, and can paste screenshots; the panel posts it to the form with the page's identity, and its All feedback link opens the team's list. The form is published beside the site, as `PacletPageFeedback/Form`:

```wl
form = CloudObject["PacletPageFeedback/Form"]
```

A submission sent the way the panel sends it:

```wl
response = URLRead[HTTPRequest[form, <|"Method" -> "POST", "Body" -> {
    "Feedback" -> "An example entry sent by the Publishing a Documentation Site tutorial.",
    "Where" -> "Feedback section",
    "Page" -> "tutorial/PublishingADocumentationSite",
    "Title" -> "Publishing a Documentation Site",
    "Link" -> First[CloudObject["PacletPage"]] <> "#tutorial/PublishingADocumentationSite",
    "View" -> "built page"
}|>]]
```

The form answered with its thank-you:

```wl
StringContainsQ[response["Body"], "id=\"pp-feedback-thanks\""]
```

The team reads the entries on the site's `All` page; from a kernel signed in as the publishing account, `PacletPageFeedback` gives them, newest first. The entry just sent:

```wl
example = PacletPageFeedback["PacletPage"][SelectFirst[#Feedback === "An example entry sent by the Publishing a Documentation Site tutorial." &]]
```

Each entry stands somewhere: open, decided, fixed, follow-up or not a defect. `SetPacletPageFeedbackStatus` records it, and each Status button on the `All` page copies the same call:

```wl
SetPacletPageFeedbackStatus["PacletPage", example["ID"], "not a defect", "an example sent by this tutorial"]
```

## Checking

PacletPage's resource and the catalog of its published pages:

```wl
resource = CloudObject["DeployedResources/Paclet/WolframInstitute/PacletPage"];
catalog = PacletPageCatalog[resource];
```

A site depends on pages it does not own. `CheckPacletPage` asks for everything the way an anonymous reader's browser does:

```wl
report = CheckPacletPage[catalog, "PacletPage", "Pages" -> All]
```

Whether everything is reachable:

```wl
report["OK"]
```

A page whose notebook answers 403 is not public. `SetPacletPagePermissions` sets permissions over the resource's tree, listing each directory's subdirectories rather than probing every object:

```wl
SetPacletPagePermissions[resource, "Public"]
```

## The whole release

The site is the last step of a paclet's release. `BuildPacletDocumentation` converts the markdown in the paclet's `docs/` folder into documentation notebooks and its `ResourceDefinition.md` into the definition notebook; `DeployPacletResource` builds the paclet afresh, checks the archive holds every documentation page, and deploys the resource; `SubmitPacletResource` submits the paclet to the Paclet Repository; and `PublishPacletPage` does all of it and publishes the site, in one kernel and so as one cloud account. PacletPage is released that way: its `release.wls` is one call to `PublishPacletPage`, and building this page along the way is what published the site above.

## Keeping it current

When PacletPage is deployed again, its pages keep their URLs, so the site shows the new pages at once; a new page or guide joins the site when the site is published again from a new catalog. A paclet on the Paclet Repository stores its notebooks anew with each update, so its site is published again after every update, and `CheckPacletPage` says when the catalog no longer matches the published pages.
