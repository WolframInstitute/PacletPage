---
Template: Symbol
Name: PacletPageCatalog
Context: WolframInstitute`PacletPage`
Paclet: WolframInstitute/PacletPage
URI: WolframInstitute/PacletPage/ref/PacletPageCatalog
Keywords: [paclet, documentation, catalog, Paclet Repository, guide tree]
SeeAlso: [PacletPageFiles, PacletPage, DeployPacletPage, CheckPacletPage]
RelatedGuides: [PacletPage]
RelatedTutorials: [PublishingADocumentationSite]
---

## Usage

`PacletPageCatalog[resource]` gives the catalog of the documentation pages of a published paclet: each page's title, its published page, the notebook behind it, and the guide tree.

`PacletPageCatalog[File[dir]]` or `PacletPageCatalog[paclet]` gives the catalog of the documentation notebooks of a local or installed paclet, without the cloud.

## Details & Options

- `resource` is `"Publisher/Name"` or a Paclet Repository URL for a paclet on the Paclet Repository, or the URL or `CloudObject` of a paclet resource deployed to a cloud account, under `DeployedResources/Paclet`.
- The catalog is an association with keys `"Name"`, `"Title"`, `"Kind"` (`"Repository"` or `"Cloud"`), `"Resource"`, `"DocBase"`, `"RootGuide"`, `"Pages"`, `"Unpublished"`, `"LeftOut"`, `"Version"` and `"Date"`.
- `File[dir]` (or a `PacletObject`) is a local or installed paclet with its documentation notebooks under `Documentation/English`, as `BuildPacletDocumentation` writes them. Its catalog has `"Kind"` `"Local"`, the paclet's `"Version"` and `"Description"`, and each page's notebook file as `"File"`, read from the directory alone. A symbol's or another reference page's title and keywords come from its notebook's metadata, read from the end of the file without its cells, and a guide or a tutorial is read whole, for its children and its state. A paclet built by `DeployPacletResource` ships its catalog beside its notebooks, `PacletPageCatalog.wxf`, so an install of it opens its catalog in about a second without reading them; a notebook of another size than the shipped catalog records is read. Otherwise each page's entry is kept in a cache of the user's own, under `$UserBaseDirectory`, by its notebook's size and date, so a paclet's catalog is read whole once and in about a second after, in any kernel; a changed notebook is read again alone. `PacletPage` builds its documentation page expression on it.
- `"Pages"` associates each page id, such as `"guide/X"`, `"ref/X"` or `"tutorial/X"`, with the page's `"Kind"`, `"Name"`, `"Title"`, `"Keywords"`, `"Guides"`, `"HTML"` and `"Notebook"`. A page name keeps any `$` it has.
- A reference page other than a symbol's, made with the Format, ServiceConnection, Device, Interpreter, Entity, Character, Message or Program template, has an id of more parts, as its URI ends: `"ref/format/X"`, `"ref/service/X"`, and `"ref/message/Symbol/tag"` for a message. The site lists these pages in the left rail under a heading for their kind, apart from the symbols.
- The pages are the ones the resource page links to, and the ones its guides link to in turn, when they are published. `"Unpublished"` lists the pages a guide links to that are not published, such as pages added since the resource was last deployed. A page that does not read is read again twice, four at a time, after 5 and then 20 seconds, as a page the cloud is redeploying answers as private now and then; `"LeftOut"` lists the published pages whose notebook still could not be read, which the site lacks.
- A page's title is its link's text, without the right guillemet that a sub-guide heading ends in.
- A page that answers an anonymous read with 401 and belongs to the account the kernel is connected to, as the pages of a resource published privately do, is read through the kernel's cloud session, so that its owner can build the catalog and site of a private resource.
- The published pages are read anonymously by an external curl, sixteen at a time, as `CheckPacletPage` reads them; the notebooks of pages built elsewhere are read from the cloud.
- `"Version"` is the paclet's published version: the one the deployed resource's last deploy records, or the newest on the Paclet Repository. It is `None` when neither can be read, such as for a deployed resource from a kernel not connected to the cloud.
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

The version it is published at:

```wl
catalog["Version"]
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
