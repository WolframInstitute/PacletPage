---
Template: Symbol
Name: BuildPacletDocumentation
Context: WolframInstitute`PacletPage`
Paclet: WolframInstitute/PacletPage
URI: WolframInstitute/PacletPage/ref/BuildPacletDocumentation
Keywords: [markdown, documentation, notebooks, build, MarkdownToNotebook]
SeeAlso: [DeployPacletResource, SubmitPacletResource, PublishPacletPage]
RelatedGuides: [PacletPage]
RelatedTutorials: [PublishingADocumentationSite]
---

## Usage

`BuildPacletDocumentation[dir]` converts the markdown documentation of the paclet in *dir* into documentation notebooks under `Documentation/English`, and its `ResourceDefinition.md` into the definition notebook its resource is deployed from.

## Details & Options

- The markdown is the `.md` files under `docs/`, in any layout. Each file's front matter says what page it is: a `Template: Symbol` page becomes a notebook in `ReferencePages/Symbols`, a `Guide` in `Guides`, a `TechNote` in `Tutorials`, and so on, named by the last segment of its `URI`. A file whose front matter names no page template, such as notes for the authors, is no page.
- The definition notebook is built from `ResourceDefinition.md` beside `PacletInfo.wl`, else in the markdown directory, else from the markdown directory's `Template: Paclet` page. When there is none and a `ResourceDefinition.nb` is there already, a message says it was not rebuilt.
- The paclet is loaded first, so the examples on its pages evaluate against it.
- MarkdownToNotebook converts the pages: the one already loaded, else the copy beside PacletPage in its source repository, else the published function.
- Each page's notebook is cached, under a key of everything it depends on: its markdown and options, the converter, the titles of the paclet's guides, which its links show, and the code its examples run, the files under `Kernel/` and `Assets/` of the paclet and of its dependencies. A page whose key is cached is copied from the cache, without converting it.
- The converter caches each example's result by the example's code alone, so a page whose examples last ran against other code is converted with that cache off: its examples are evaluated afresh.
- With `"Parallel" -> True` or a number of kernels, the pages are converted on parallel kernels, launched for the build and closed after it; with `Automatic`, on the parallel kernels already running, if any. This kernel converts the first page, so that anything the converter installs is in place first, and tries again any page that failed on a parallel kernel.
- Each conversion has `"TimeConstraint"` seconds. A page that runs past them fails, unless it wrote its whole notebook first.
- The cache is in `build/cache`, or the directory given as `"CacheDirectory"`, which a build server can keep between runs. After a build of every page, it keeps only what that build used.
- With `"Prune" -> True`, the notebooks under the target that a whole build did not write, pages whose markdown is gone, are deleted; only after a build of every file with no failure, since a page that did not convert keeps its last notebook.
- The result has `"Notebooks"`, the notebooks written; `"Definition"`, the definition notebook or `None`; `"Failed"`, the pages that did not convert; `"Converted"`, the pages converted; `"Cached"`, the pages copied from the cache; `"Evaluated"`, the pages whose examples were evaluated afresh; `"Skipped"`, the markdown that is no page; `"Pruned"`, the notebooks deleted; `"Kernels"`, the number of parallel kernels; and `"Unevaluated"`, by page, the sections that use names an earlier section defines, which the converter reports since they come out unevaluated. Such a page stays out of the cache, so every build converts and reports it until it is fixed.
- The following options can be given:

| Option | Default | Description |
|---|---|---|
| `"Source"` | `Automatic` | the markdown directory, by default `docs/` |
| `"Target"` | `Automatic` | the notebook directory, by default `Documentation/English` |
| `"Files"` | `All` | the files to build, as a list or a pattern for their names |
| `"EvaluateSeparator"` | `Automatic` | the converter's evaluation separator, one value for every page or an association by template: by default a tutorial or an Overview page runs in one context, since its later sections use what earlier ones define, and every other page runs each section, and each example between `---`, in a clean context |
| `"ConverterOptions"` | `{}` | options for every conversion, such as `"MathFont"` |
| `"UseCache"` | `Automatic` | whether to reuse cached pages and example results |
| `"Definition"` | `Automatic` | the definition's markdown, or `None` |
| `"Converter"` | `Automatic` | the converter: a function, a file or the URL of a published function |
| `"Prune"` | `False` | whether to delete the notebooks of pages whose markdown is gone |
| `"Parallel"` | `Automatic` | `True`, a number of kernels, or `False` |
| `"TimeConstraint"` | `1800` | the seconds each page may take |
| `"CacheDirectory"` | `Automatic` | where the cache is, by default `build/cache` |
| `"Dependencies"` | `Automatic` | the paclets whose code the examples run besides the paclet's own, as directories or names; by default the ones its `PacletInfo.wl` declares |
| `"Monitor"` | `False` | whether to print each page as it is converted |

- After a change to the converter, `"UseCache" -> False` converts every page and evaluates every example afresh.

## Basic Examples

A copy of the small example paclet that comes with PacletPage:

```wl
dir = CopyDirectory[PacletObject["WolframInstitute/PacletPage"]["AssetLocation", "Example"], FileNameJoin[{$TemporaryDirectory, CreateUUID["Greetings-"]}]]
```

Its documentation, in markdown:

```wl
FileNames["*.md", dir, Infinity]
```

Build its documentation notebooks and its definition notebook:

```wl
build = BuildPacletDocumentation[dir]
```

The notebooks, by where they went under `Documentation/English`:

```wl
FileNameDrop[#, FileNameDepth[FileNameJoin[{dir, "Documentation", "English"}]]] & /@ build["Notebooks"]
```

Nothing has changed since, so a second build copies every page from the cache:

```wl
BuildPacletDocumentation[dir]
```

The copy is no longer needed:

```wl
DeleteDirectory[dir, DeleteContents -> True]
```

## Options

### Files

A copy of the example paclet:

```wl
dir = CopyDirectory[PacletObject["WolframInstitute/PacletPage"]["AssetLocation", "Example"], FileNameJoin[{$TemporaryDirectory, CreateUUID["Greetings-"]}]];
```

Build one page:

```wl
BuildPacletDocumentation[dir, "Files" -> "Greet.md", "Definition" -> None]
```

```wl
DeleteDirectory[dir, DeleteContents -> True]
```

### UseCache

A copy of the example paclet, built once:

```wl
dir = CopyDirectory[PacletObject["WolframInstitute/PacletPage"]["AssetLocation", "Example"], FileNameJoin[{$TemporaryDirectory, CreateUUID["Greetings-"]}]];
BuildPacletDocumentation[dir];
```

Nothing has changed, but without the cache every page is converted and every example evaluated afresh:

```wl
BuildPacletDocumentation[dir, "UseCache" -> False]
```

```wl
DeleteDirectory[dir, DeleteContents -> True]
```

### Parallel

A copy of the example paclet:

```wl
dir = CopyDirectory[PacletObject["WolframInstitute/PacletPage"]["AssetLocation", "Example"], FileNameJoin[{$TemporaryDirectory, CreateUUID["Greetings-"]}]];
```

Convert the pages on two parallel kernels, launched for the build:

```wl
BuildPacletDocumentation[dir, "Parallel" -> 2]
```

The kernels are closed after the build:

```wl
Kernels[]
```

```wl
DeleteDirectory[dir, DeleteContents -> True]
```

### CacheDirectory

A copy of the example paclet:

```wl
dir = CopyDirectory[PacletObject["WolframInstitute/PacletPage"]["AssetLocation", "Example"], FileNameJoin[{$TemporaryDirectory, CreateUUID["Greetings-"]}]];
```

Keep the cache in another directory, such as one a build server saves between runs:

```wl
BuildPacletDocumentation[dir, "CacheDirectory" -> FileNameJoin[{dir, "saved-cache"}]]
```

The cache holds each page's notebook and the code each page's examples last ran against:

```wl
FileNames["*", FileNameJoin[{dir, "saved-cache"}]]
```

```wl
DeleteDirectory[dir, DeleteContents -> True]
```
