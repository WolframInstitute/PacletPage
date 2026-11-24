---
Template: Symbol
Name: PacletPage
Context: WolframInstitute`PacletPage`
Paclet: WolframInstitute/PacletPage
URI: WolframInstitute/PacletPage/ref/PacletPage
Keywords: [page, documentation, guide tree, XMLElement, offline, notebook, layout]
SeeAlso: [PacletPageCatalog, PacletPageFiles, BuildPacletPageSite, BuildPacletDocumentation, SubmitPacletPageFeedback]
RelatedGuides: [PacletPage]
---

## Usage

<code>[PacletPage]()[*spec*, *page*]</code> gives the documentation page *page* of the local or installed paclet *spec* as an [XMLElement]() shell holding the three-column PacletPage layout: header, guide tree and tutorials, the page's live [Notebook]() in the center frame, symbols rail, and footer.

<code>[PacletPage]()[*spec*]</code> gives the paclet's main guide, or the addressed page when *spec* is a `paclet:` URI.

## Details & Options

- *spec* is a paclet directory, `File[dir]`, a `PacletObject`, the name of an installed paclet such as `"WolframInstitute/PacletPage"`, a local catalog from `PacletPageCatalog`, or a `paclet:` URI such as `"paclet:WolframInstitute/PacletPage/ref/PacletPage"`.
- *page* is `Automatic` for the main guide (or first tutorial or symbol when the paclet has no guide), a `paclet:` URI of the paclet, a page id such as `"ref/Function"`, `"guide/Name"` or `"tutorial/Name"`, or a bare page or symbol name.
- The result is `XMLElement["div", {"class" -> "pp-shell ...", "data-paclet-page" -> ..., "data-paclet-id" -> ..., "data-paclet-title" -> ...}, {...}]`, holding the header (`.pp-hdr`), the main layout (`.pp-main`) with the left navigation rail (`.pp-side`), the center frame (`.pp-content`) containing the page's live `Notebook[...]` expression, and the right symbols rail (`.pp-side-right`), followed by the footer (`.pp-ftr`).
- Navigation links carry `href` set to each page's `paclet:` URI, `data-id` set to its page id, and `data-kw` with its lowercase keywords. The active page carries the `active` class and expands its enclosing guide branches.
- Nothing is read from the cloud, built or rendered, and no front end is asked: any Wolfram kernel computes the expression from the paclet's files, caching each directory's catalog by notebook count and modification times.
- Missing paclets return `Failure["PacletNotFound", ...]`, directories without documentation notebooks return `Failure["PacletPageCatalog", ...]`, unknown pages return `Failure["PacletPageNotFound", ...]`, unreadable notebooks return `Failure["PacletPageUnreadable", ...]`, and invalid option values return `Failure["InvalidOption", ...]`.
- The following options can be given:

| Option | Default | Description |
|---|---|---|
| `"Filter"` | `""` | initial filter text matching titles, names, or keywords; `False` or `None` omits the filter box |
| `"StripTitleNumbers"` | `True` | whether leading numbering on a tutorial's or guide's title is left out of its entry |
| `"SymbolsRail"` | `True` | whether symbols appear in the right rail; with `False`, symbols are listed in the left rail |
| `"Header"` | `True` | whether to include the top header bar |
| `"Footer"` | `True` | whether to include the bottom footer bar |
| `"Title"` | `Automatic` | the title shown in the header; by default the catalog's title |
| `"Subtitle"` | `Automatic` | the subtitle shown beside the title; by default the publisher name |
| `"Accent"` | `Automatic` | custom accent color (a hex string or color), setting `--pp-accent`, `--pp-active-bg`, and `--pp-active-fg` on the shell |
| `"VersionLabel"` | `Automatic` | the version tag under the title: `Automatic` or `True` for the paclet's version, a string for a custom label, or `False` to omit it |
| `"RootGuide"` | `Automatic` | the guide id or name used as the home page and root of the guide tree, or `False` to omit the root guide link above its children |
| `"ResourceLink"` | `Automatic` | header link to the published resource page: `Automatic` uses the catalog's resource or paclet URL when present, a URL string sets it directly, `False` omits it |
| `"LayoutControls"` | `False` | whether to include toggle buttons for showing/hiding the left sidebar, showing/hiding the right sidebar, and toggling a fullscreen view of the center notebook frame; accepts `False`, `True`, `All`, or a list of `"LeftSidebar"`, `"RightSidebar"`, and `"Fullscreen"` |
| `"ViewSource"` | `True` | whether the page tools bar includes the View source / View built page toggle |
| `"View"` | `"Built"` | `"Built"` for the built documentation notebook, or `"Source"` for the page's authoring source (authoring notebook, `docs/` markdown, or raw notebook source) |
| `"Source"` | `Automatic` | optional directory of authoring `.nb` or `.md` files for `"View" -> "Source"` |
| `"SourceURL"` | `""` | optional published URL for the page's source |
| `"Feedback"` | `Automatic` | whether the page tools bar includes the Feedback button; `Automatic` includes it when a feedback URL or email is available |
| `"FeedbackURL"` | `Automatic` | URL of the deployed feedback form; `Automatic` deduces `https://www.wolframcloud.com/obj/<publisher>/<ShortName>Feedback/Form` from the paclet's `PublisherID` and name (or `"FeedbackURL"` / `"SiteURL"` metadata), and `""` or `None` disables the form URL |
| `"FeedbackEmail"` | `None` | an email address, template with `To:` and `Subject:` lines, or `File` of one for the Feedback button's prefilled `mailto:` link |
| `"FeedbackThanks"` | `Automatic` | confirmation text shown after feedback is submitted |
| `"AllFeedbackLink"` | `True` | whether the feedback panel links to the team's list of collected feedback |

## Basic Examples

The main guide of an installed paclet:

```wl
#| eval: false
PacletPage["WolframInstitute/PacletPage"]
```

A symbol's page, addressed directly by its `paclet:` URI:

```wl
#| eval: false
PacletPage["paclet:WolframInstitute/PacletPage/ref/PacletPage"]
```

Include optional sidebar and fullscreen layout controls:

```wl
#| eval: false
PacletPage["WolframInstitute/PacletPage", "LayoutControls" -> True]
```
