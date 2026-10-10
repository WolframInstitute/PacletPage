---
Template: Symbol
Name: PacletPageCatalog
Context: WolframInstitute`PacletPage`
Paclet: WolframInstitute/PacletPage
URI: WolframInstitute/PacletPage/ref/PacletPageCatalog
Keywords: [paclet, documentation, catalog, Paclet Repository, guide tree]
SeeAlso: [PacletPageFiles, DeployPacletPage, CheckPacletPage]
RelatedGuides: [PacletPage]
RelatedTutorials: [PublishingADocumentationSite]
---

## Usage

`PacletPageCatalog[resource]` gives the catalog of the documentation pages of a published paclet: each page's title, its published page, the notebook behind it, and the guide tree.

## Details & Options

- `resource` is `"Publisher/Name"` or a Paclet Repository URL for a paclet on the Paclet Repository, or the URL or `CloudObject` of a paclet resource deployed to a cloud account, under `DeployedResources/Paclet`.
- The catalog is an association with keys `"Name"`, `"Title"`, `"Kind"` (`"Repository"` or `"Cloud"`), `"Resource"`, `"DocBase"`, `"RootGuide"`, `"Pages"`, `"Unpublished"` and `"Date"`.
- `"Pages"` associates each page id, such as `"guide/X"`, `"ref/X"` or `"tutorial/X"`, with the page's `"Kind"`, `"Name"`, `"Title"`, `"Keywords"`, `"Guides"`, `"HTML"` and `"Notebook"`. A page name keeps any `$` it has.
- The pages are the ones the resource page links to, and the ones its guides link to in turn, when they are published. `"Unpublished"` lists the pages a guide links to that are not published, such as pages added since the resource was last deployed.
- A guide's `"Guides"` are the guides its section headings link to, in its order: they place it in the site's guide tree. A guide may sit under several guides.
- A resource deployed to a cloud account has its definition notebook beside its resource page; the catalog includes it as the page `"resource/Name"`.
- On the Paclet Repository each page names the notebook it shows, and the catalog reads every page to find it. A deployed resource keeps each notebook beside its page.
- The following options can be given:

| Option | Default | Description |
|---|---|---|
| `"Documentation"` | `None` | the built documentation (a paclet directory, its `Documentation/English` directory or a `PacletObject`), for keywords and titles |
| `"RootGuide"` | `Automatic` | the guide at the root of the tree |
| `"Title"` | `Automatic` | the site's title; by default the root guide's |
| `"Monitor"` | `False` | whether to print progress |

- Keywords come from the built documentation alone, so pass `"Documentation"` to let the site's filter match them.

## Basic Examples

The catalog of a paclet on the Paclet Repository:

```wl
catalog = PacletPageCatalog["Wolfram/AccessibleColors"]
```

Its title, taken from its root guide:

```wl
catalog["Title"]
```

The guide at the root of its guide tree:

```wl
catalog["RootGuide"]
```

Its pages, by id:

```wl
Keys[catalog["Pages"]]
```

One page: its title, where it is published, and the notebook behind it:

```wl
catalog["Pages", "ref/WCAGContrastRatio"]
```

## Scope

A paclet resource deployed to a cloud account is read from its URL; its definition notebook is a page of its own:

```wl
cloudCatalog = PacletPageCatalog["https://www.wolframcloud.com/obj/wolframinstitute/DeployedResources/Paclet/WolframInstitute/PacletPage"]
```

The definition notebook's page:

```wl
cloudCatalog["Pages", "resource/PacletPage"]
```

## Options

### Documentation

The paclet's built documentation gives each page its keywords:

```wl
PacletPageCatalog["https://www.wolframcloud.com/obj/wolframinstitute/DeployedResources/Paclet/WolframInstitute/PacletPage", "Documentation" -> PacletObject["WolframInstitute/PacletPage"]] // Query["Pages", "ref/PacletPageCatalog", "Keywords"]
```

### Title

Title the site differently from its root guide:

```wl
PacletPageCatalog["Wolfram/AccessibleColors", "Title" -> "Accessible Colors"] // Query["Title"]
```

## Possible Issues

Something that names no published paclet gives a message:

```wl
PacletPageCatalog["NotAPaclet"]
```
