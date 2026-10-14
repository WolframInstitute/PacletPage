---
Template: Symbol
Name: BuildPacletPageSite
Context: WolframInstitute`PacletPage`
Paclet: WolframInstitute/PacletPage
URI: WolframInstitute/PacletPage/ref/BuildPacletPageSite
Keywords: [paclet, documentation, website, HTML, preview, offline, local]
SeeAlso: [PacletPageFiles, BuildPacletDocumentation, DeployPacletPage]
RelatedGuides: [PacletPage]
RelatedTutorials: [PublishingADocumentationSite]
---

## Usage

`BuildPacletPageSite[dir]` renders the documentation notebooks of the paclet in *dir* to HTML on this computer and writes the site around them, under *dir*`/build/site`, without the cloud.

## Details & Options

- The site is the published site's shell, its filter, its rails and its frame, around the paclet's own documentation notebooks, each rendered as the documentation center shows it. Nothing is read from the cloud or written to it, so it previews the documentation before anything is published, and needs no account.
- `dir` needs its documentation notebooks, which `BuildPacletDocumentation` writes.
- Each page renders in a kernel process of its own, with a front end and `"TimeConstraint"` seconds; a page past it is reported and left out. A process records each page as it renders it; one that renders nothing for a page's time is stopped, and the pages it left get one more process. `"Parallel" -> n` renders in `n` processes at once, `True` in as many as the processors and the memory allow.
- Each render is kept in *dir*`/build/html`, keyed by its notebook and the versions that render it, so a site built again renders only the pages whose notebooks changed. `"UseCache" -> False` renders every page again.
- `"Pages" -> {id, ...}` renders those pages, such as `"ref/Name"` or `"guide/Name"`, and takes the others from the cache: a quick look at a page being written.
- The site is written beside the old one and replaces it whole once it is complete, so a server showing it never shows half a site.
- Served from the root of any web server, `index.html` opens the site, such as with `python3 -m http.server` in the site's directory. The pages have no Feedback panel.
- The result has `"Site"`, the site's `index.html`; `"Pages"`, how many pages it has; `"Rendered"` and `"Cached"`, how many were rendered and taken from the cache; and `"Failed"`, the pages that did not render.
- The following options can be given:

| Option | Default | Description |
|---|---|---|
| `"Target"` | `Automatic` | the site's directory, by default *dir*`/build/site` |
| `"Pages"` | `All` | the pages to render, the others taken from the cache |
| `"UseCache"` | `True` | whether to take the pages rendered before from the cache |
| `"Parallel"` | `False` | the number of rendering processes, or `True` |
| `"TimeConstraint"` | `300` | the seconds a page may take to render |
| `"Title"` | `Automatic` | the site's title |
| `"Subtitle"` | `Automatic` | the line beside the title |
| `"Accent"` | `"#dd1100"` | the accent color |
| `"Monitor"` | `False` | whether to print how many pages render and how many come from the cache |

## Basic Examples

A copy of the example paclet that comes with PacletPage, with its documentation built:

```wl
dir = CopyDirectory[PacletObject["WolframInstitute/PacletPage"]["AssetLocation", "Example"], FileNameJoin[{$TemporaryDirectory, CreateUUID["Greetings-"]}]];
BuildPacletDocumentation[dir];
```

The site, rendered on this computer:

```wl
site = BuildPacletPageSite[dir]
```

Every page rendered:

```wl
{site["Pages"], site["Rendered"]}
```

Built again, every page comes from the cache:

```wl
BuildPacletPageSite[dir]["Cached"]
```

The site's files:

```wl
FileNames["*", FileNameJoin[{dir, "build", "site"}]]
```

The copy is no longer needed:

```wl
DeleteDirectory[dir, DeleteContents -> True]
```
