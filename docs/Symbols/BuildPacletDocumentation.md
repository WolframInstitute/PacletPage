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
- The paclet is loaded first, so the examples on its pages evaluate against it; with `"EvaluationKernel"`, it is loaded on that kernel instead, and not here.
- MarkdownToNotebook converts the pages: the one already loaded, else the copy beside PacletPage in its source repository, else the published function. A converter file, that copy or one given as `"Converter"`, is read into a context of its own, on no context path, so its helpers never shadow, or are shadowed by, the symbols of a package that a page's examples load.
- Each page's notebook is cached, under a key of everything it depends on: its markdown and options, the converter, the titles of the paclet's guides, which its links show, and the code its examples run, the files under `Kernel/` and `Assets/` of the paclet and of its dependencies. A page whose key is cached is copied from the cache, without converting it.
- The converter caches each example's result by the example's code alone, so a page whose examples last ran against other code is converted with that cache off: its examples are evaluated afresh. That code is the paclet's, its dependencies' and the converter's, so another converter, or a redeploy of the published one, evaluates every example afresh.
- With `"Parallel" -> True` or a number of kernels, the pages are converted on parallel kernels, launched for the build and closed after it; with `Automatic`, on the parallel kernels already running, if any. This kernel converts the first page, so that anything the converter installs is in place first, and tries again any page that failed on a parallel kernel.
- The parallel kernels a page's examples launch are closed once the page is converted, so that none of them is left to the examples of the pages after it.
- The ``Global` `` symbols a page gives definitions to are cleared once the page is converted, so that a page whose examples put ``Global` `` on their context path, and so read and assign names there, leaves no value to the pages after it. A symbol defined before the page keeps its definitions.
- A parallel kernel takes the switches of the `General` messages from the kernel that builds, such as a script kernel's `General::shdw` off, so that a page shows the same messages whichever kernel converts it.
- Each conversion has `"TimeConstraint"` seconds. A page that runs past them fails, unless it wrote its whole notebook first.
- With `"EvaluationKernel"`, the examples are evaluated on another kernel, for a paclet that loads only there. A command, `{program, argument, ...}`, launches it with `LinkLaunch`, which adds the arguments of its link; `-wstp` among them makes a kernel of this Wolfram answer on a link. The kernel loads the paclet from its directory, and each page's cells are entered there in ``Global` ``, a statement at a time, as a front end enters them. Each section and each page starts from the names ``Global` `` had once the paclet loaded.
- An association gives more than the command: `"Command"`; `"Directory"`, where the kernel starts; `"Initialization"`, code in `Hold` that the kernel evaluates once the paclet loads, its symbols in the contexts they were read in; `"Scope"`, `"Page"` for a kernel of each page's own, the default, or `"Template"` for one kernel that the pages of a template share; and `"Identity"`, which names the kernel in the caches of pages and examples, by default the command and a hash of its program. The scope, the initialization and the directory key the caches too. An association of templates to commands or associations, such as `<|"Symbol" -> spec1, "TechNote" -> spec2|>`, gives each template its kernel, and a template it leaves out is evaluated here.
- A page's kernel is closed after the page, and a template's after the build; each parallel kernel launches kernels of its own. A kernel still running two seconds after its link closed is killed. A page fails when its kernel closes its link, as a kernel does when it dies, or gives no prompt within five minutes of its launch; a template's kernel that a page failed on is replaced for the next page.
- A kernel that answers with text rather than boxes, as a kernel of this Wolfram does when no front end launched it, gives that text as each output, and its messages and prints as it formats them.
- The cache is in `build/cache`, or the directory given as `"CacheDirectory"`, which a build server can keep between runs. After a build of every page, it keeps only what that build used.
- With `"Prune" -> True`, the notebooks under the target that a whole build did not write, pages whose markdown is gone, are deleted; only after a build of every file with no failure, since a page that did not convert keeps its last notebook.
- The result has `"Notebooks"`, the notebooks written; `"Definition"`, the definition notebook or `None`; `"Failed"`, the pages that did not convert; `"Converted"`, the pages converted; `"Cached"`, the pages copied from the cache; `"Evaluated"`, the pages whose examples were evaluated afresh; `"Skipped"`, the markdown that is no page; `"Pruned"`, the notebooks deleted; `"Kernels"`, the number of parallel kernels; `"Flagged"`, by page, the flag of each page the documentation build leaves out, `"Future"`, `"Excised"`, `"Internal"` or `"Temporary"`, which a message names too; and `"Unevaluated"`, by page, the sections that use names an earlier section defines, which the converter reports since they come out unevaluated. Such a page stays out of the cache, so every build converts and reports it until it is fixed.
- The following options can be given:

| Option | Default | Description |
|---|---|---|
| `"Source"` | `Automatic` | the markdown directory, by default `docs/` |
| `"Target"` | `Automatic` | the notebook directory, by default `Documentation/English` |
| `"Files"` | `All` | the files to build, as a list or a pattern for their names |
| `"EvaluateSeparator"` | `Automatic` | the converter's evaluation separator, one value for every page or an association by template: by default a tutorial or an Overview page runs in one context, since its later sections use what earlier ones define, and every other page runs each section, and each example between `---`, in a clean context |
| `"ConverterOptions"` | `{}` | options for every conversion, such as `"MathFont"`, or an association of a page's file name, or a string pattern for it, to that page's options |
| `"UseCache"` | `Automatic` | whether to reuse cached pages and example results |
| `"Definition"` | `Automatic` | the definition's markdown, or `None` |
| `"Converter"` | `Automatic` | the converter: a function, a file or the URL of a published function |
| `"Prune"` | `False` | whether to delete the notebooks of pages whose markdown is gone |
| `"Parallel"` | `Automatic` | `True`, a number of kernels, or `False` |
| `"TimeConstraint"` | `1800` | the seconds each page may take |
| `"CacheDirectory"` | `Automatic` | where the cache is, by default `build/cache` |
| `"Dependencies"` | `Automatic` | the paclets whose code the examples run besides the paclet's own, as directories, which are loaded, or names; by default the ones its `PacletInfo.wl` declares |
| `"EvaluationKernel"` | `None` | the kernel the examples are evaluated on: a command, an association, or an association of templates to them; `None` evaluates them here |
| `"Monitor"` | `False` | whether to print how many pages there are, cached and to convert, then each page as it is converted |

- `"UseCache" -> False` converts every page and evaluates every example afresh, whatever has changed.

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

### ConverterOptions

A copy of the example paclet:

```wl
dir = CopyDirectory[PacletObject["WolframInstitute/PacletPage"]["AssetLocation", "Example"], FileNameJoin[{$TemporaryDirectory, CreateUUID["Greetings-"]}]];
```

Build one page with its examples left unevaluated, the converter's `"Evaluate" -> False` given to that page alone:

```wl
BuildPacletDocumentation[dir, "Files" -> "Greet.md", "ConverterOptions" -> <|"Greet.md" -> {"Evaluate" -> False}|>, "Definition" -> None]
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

### EvaluationKernel

A copy of the example paclet:

```wl
dir = CopyDirectory[PacletObject["WolframInstitute/PacletPage"]["AssetLocation", "Example"], FileNameJoin[{$TemporaryDirectory, CreateUUID["Greetings-"]}]];
```

The program of another kernel, here one of this Wolfram:

```wl
kernel = First[FileNames["WolframKernel" | "WolframKernel.exe", FileNameJoin[{$InstallationDirectory, #}] & /@ {"MacOS", "Executables"}]]
```

Evaluate the examples on that kernel, which each page launches with the paclet loaded on it and closes after it:

```wl
BuildPacletDocumentation[dir, "EvaluationKernel" -> {kernel, "-wstp", "-noinit"}]
```

The symbol pages on one kernel that they share, each page starting from the names it had once the paclet loaded, and the other pages evaluated here:

```wl
BuildPacletDocumentation[dir, "EvaluationKernel" -> <|"Symbol" -> <|"Command" -> {kernel, "-wstp", "-noinit"}, "Scope" -> "Template"|>|>]
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
