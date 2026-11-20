---
Template: Symbol
Name: PacletPageDOM
Context: WolframInstitute`PacletPage`
Paclet: WolframInstitute/PacletPage
URI: WolframInstitute/PacletPage/ref/PacletPageDOM
Keywords: [Wolfram Studio, page, documentation, guide tree, WebColumn, offline]
SeeAlso: [PacletPageCatalog, BuildPacletPageSite, BuildPacletDocumentation]
RelatedGuides: [PacletPage]
---

## Usage

<code>[PacletPageDOM]()[*dir*, *page*]</code> gives the documentation page *page* of the paclet in *dir* as [WebColumn](), [WebRow]() and [WebItem](): the page's notebook beside the paclet's guides, tutorials and symbols, a page of Wolfram Studio.

<code>[PacletPageDOM]()[*dir*]</code> gives the paclet's main guide.

## Details & Options

- *dir* is a paclet directory with its documentation notebooks under `Documentation/English`, as `BuildPacletDocumentation` writes them.
- *page* is a `paclet:` URI of the paclet, such as `"paclet:Publisher/Name/ref/Function"`, a page id, such as `"ref/Function"`, `"guide/Name"` or `"tutorial/Name"`, or the name of a symbol.
- The result is a `WebRow` of two parts: a `WebColumn` of the paclet's pages, and a `WebItem` that holds the page's notebook as its file has it, links and all.
- The column lists the guides as a tree from the main guide, then the guides the tree does not reach, the tutorials, the applications and the symbols, in the order and with the titles of the site's rails. Each entry is a `Hyperlink` to its page's `paclet:` URI, and the page shown is marked.
- Nothing is read from the cloud, built or rendered, and no front end is asked: the kernel of a Wolfram Studio session computes the page from the paclet's files.
- A page the paclet does not have gives a `Failure` tagged `"PacletPageNotFound"`.
- The following options can be given:

| Option | Default | Description |
|---|---|---|
| `"Filter"` | `""` | text the titles or keywords of the pages listed hold; the guides are then listed rather than shown as a tree |
| `"StripTitleNumbers"` | `True` | whether the numbering a tutorial's or guide's title starts with is left out of its entry |

## Basic Examples

The main guide of a paclet, as a page of Wolfram Studio:

```wl
#| eval: false
PacletPageDOM[PacletObject["WolframInstitute/PacletPage"]["Location"]]
```

A symbol's page, by its `paclet:` URI, as a link on a page gives it:

```wl
#| eval: false
PacletPageDOM[PacletObject["WolframInstitute/PacletPage"]["Location"], "paclet:WolframInstitute/PacletPage/ref/PacletPageDOM"]
```
