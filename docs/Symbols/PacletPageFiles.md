---
Template: Symbol
Name: PacletPageFiles
Context: WolframInstitute`PacletPage`
Paclet: WolframInstitute/PacletPage
URI: WolframInstitute/PacletPage/ref/PacletPageFiles
Keywords: [paclet, documentation, website, HTML, preview]
SeeAlso: [PacletPageCatalog, DeployPacletPage]
RelatedGuides: [PacletPage]
RelatedTutorials: [PublishingADocumentationSite]
---

## Usage

`PacletPageFiles[catalog]` gives the site's pages for `catalog`, an association of the file names `"index.html"` and `"page.html"` with their HTML.

## Details & Options

- `"index.html"` is the site: a header with a filter over every page, the resource, the guide tree and the tutorials in the left rail, the symbols in the right rail, and a frame for the pages.
- `"page.html"` is the page the frame shows: it paints the cloud's static render of a page's notebook, then the Wolfram notebook embedder makes it live in place. For the resource, it shows the resource page as published. It carries the Feedback button and the toggle between a page and its source: the resource's source is its definition notebook, every other page's its authoring notebook when the site publishes them. Links to the paclet's own pages open in the site; other links open in a new tab.
- The pages are ASCII, with every character past ASCII as a character reference, so they read right whatever character set a server declares.
- Served by any web server, the two files are a working preview of the site, `"index.html"` at `/` and `"page.html"` beside it. `DeployPacletPage` publishes them with their URLs filled in.
- The following options can be given:

| Option | Default | Description |
|---|---|---|
| `"SiteURL"` | `""` | the site's own URL, which the feedback links back to |
| `"PageURL"` | `"page.html"` | the URL of `"page.html"`, from the site |
| `"FeedbackURL"` | `""` | the feedback form; the Feedback button opens it |
| `"SourceURL"` | `""` | the directory of authoring notebooks; a toggle shows them |
| `"Title"` | `Automatic` | the site's title |
| `"Subtitle"` | `Automatic` | the line beside the title; by default the publisher's name |
| `"Accent"` | `"#dd1100"` | the accent color, a hex string or a color |
| `"Home"` | `Automatic` | `"Definition"`, `"RootGuide"` or `"Resource"`, the page the site opens on |
| `"FeedbackEmail"` | `None` | an address, an email template with `To:` and `Subject:` lines, or a `File` of one |
| `"FeedbackThanks"` | `Automatic` | what the Feedback panel shows once feedback is sent |
| `"DefinitionPage"` | `True` | whether the definition notebook is a page of the site |

- With `"Home" -> Automatic`, the site opens on the resource, with its Feedback button and its source, when the catalog has its definition notebook, else on the root guide. `"DefinitionPage" -> False` leaves the definition notebook out of the site; the resource page itself stays its home on request, with `"Home" -> "Resource"`, which hides an empty Examples section and the author's Paclet Source link.
- The tutorials are listed in the order of their titles, an Overview page first, labelled Overview unless its title says so already, and the ones with the keyword "application" apart as Applications. A page is an Overview when its notebook's metadata types it as one, or, with no notebook at hand, when its name is Overview. A tutorial whose authoring notebook has nothing between its title and the template's closing sections is a placeholder: it is shown greyed, and the filter's Enter opens a written page before it.
- While the filter shows a match, the labels of the groups it shows stay, so a match keeps its heading.
- An email template's body may use `{title}`, `{page}`, `{link}`, `{view}`, `{notebook}` and `{date}`, filled in for the page the reader is on; an address alone gets a template that asks what is wrong, where, what was expected, and anything else.

## Basic Examples

The catalog of a paclet on the Paclet Repository:

```wl
catalog = PacletPageCatalog["Wolfram/AccessibleColors"];
```

The site's two pages, each tens of thousands of characters of HTML; their first lines:

```wl
PacletPageFiles[catalog] // Map[Snippet[#, 8] &]
```

Their sizes in characters:

```wl
PacletPageFiles[catalog] // Map[StringLength]
```

## Options

### Accent

The catalog of a paclet on the Paclet Repository:

```wl
catalog = PacletPageCatalog["Wolfram/AccessibleColors"];
```

A different accent color, on both pages:

```wl
PacletPageFiles[catalog, "Accent" -> RGBColor[0.1, 0.4, 0.8]] // Map[StringCases["--pp-accent: " ~~ Shortest[__] ~~ ";"]]
```

### FeedbackEmail

The catalog of a paclet on the Paclet Repository:

```wl
catalog = PacletPageCatalog["Wolfram/AccessibleColors"];
```

An address makes the Feedback button open a prefilled email when the site has no feedback form:

```wl
PacletPageFiles[catalog, "FeedbackEmail" -> "docs@example.org"]["page.html"] // StringContainsQ["docs@example.org"]
```

## Applications

The catalog of a paclet on the Paclet Repository:

```wl
catalog = PacletPageCatalog["Wolfram/AccessibleColors"];
```

Preview its site on this computer, served by the Wolfram Language itself with `LocalDeploy` from the Paclet Repository:

```wl
PacletInstall["ToneAr/LocalDeploy"];
Needs["ToneAr`LocalDeploy`"]
```

The site at `/`, the frame's page beside it:

```wl
preview = LocalDeploy[URLDispatcher[{
    "/page.html" -> HTTPResponse[PacletPageFiles[catalog]["page.html"], <|"ContentType" -> "text/html; charset=utf-8"|>],
    "/" -> HTTPResponse[PacletPageFiles[catalog]["index.html"], <|"ContentType" -> "text/html; charset=utf-8"|>]
}]]
```

It answers while the kernel is idle, at its URL:

```wl
URLRead[preview["URL"], "StatusCode"]
```

Stop it:

```wl
DeleteObject[preview]
```
