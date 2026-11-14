---
Template: Symbol
Name: BuildPacletDocumentation
Context: WolframInstitute`PacletPage`
Paclet: WolframInstitute/PacletPage
URI: WolframInstitute/PacletPage/ref/BuildPacletDocumentation
Keywords: [markdown, documentation, notebooks, build, MarkdownToNotebook]
SeeAlso: [DeployPacletResource, SubmitPacletResource, PublishPacletPage, PacletDefinitionScan, PacletPageDependencies]
RelatedGuides: [PacletPage]
RelatedTutorials: [PublishingADocumentationSite, CachingADocumentationBuild]
---

## Usage

`BuildPacletDocumentation[dir]` converts the markdown documentation of the paclet in *dir* into documentation notebooks under `Documentation/English`, and its `ResourceDefinition.md` into the definition notebook its resource is deployed from.

## Details & Options

- The markdown is the `.md` files under `docs/`, in any layout. Each file's front matter says what page it is: a `Template: Symbol` page becomes a notebook in `ReferencePages/Symbols`, a `Guide` in `Guides`, a `TechNote` in `Tutorials`, and so on, named by the last segment of its `URI`. A file whose front matter names no page template, such as notes for the authors, is no page.
- The definition notebook is built from `ResourceDefinition.md` beside `PacletInfo.wl`, else in the markdown directory, else from the markdown directory's `Template: Paclet` page. When there is none and a `ResourceDefinition.nb` is there already, a message says it was not rebuilt.
- The paclet is loaded first, so the examples on its pages evaluate against it; with `"EvaluationKernel"`, it is loaded on that kernel instead, and not here.
- MarkdownToNotebook converts the pages: the one already loaded, else the copy beside PacletPage in its source repository, else the published converter this release of PacletPage carries, the one it was tested with, else the published function. The converter a release carries is loaded from its file with no network, and a published function given as `"Converter"` is fetched once and handed to the kernels that convert pages the same way. Each kernel that converts pages loads Wolfram/Parser, which the converter uses for TeX, from a copy vendored beside a converter checkout or one installed, and never installs it during a conversion. A converter file, that copy or one given as `"Converter"`, is read into a context of its own, on no context path, so its helpers never shadow, or are shadowed by, the symbols of a package that a page's examples load.
- Each page's notebook is cached, under a key of everything it depends on: its markdown and options, the converter, the titles of the paclet's guides, which its links show, and the code its examples run. A page whose key is cached is copied from the cache, without converting it.
- The code a page's examples run is what the page uses of the paclet's definitions. A fresh kernel loads the paclet and reads the whole definition, `` Language`ExtendedDefinition ``, of each symbol of the contexts the loaded paclets declare, as a primary context or a Kernel extension's, the paclet's dependencies among them. A symbol uses every symbol of its code, and the names in its code's strings that have no space, as a name given to `Symbol` or `ToExpression` is, in full or by their own name; its messages, usage among them, name nothing. A name in a string without its context is a symbol of the same paclet, or one in a context that the symbol's code, or the code it reaches, holds as a string, as code that makes a symbol of a name gives it one; a page's own text names the paclet's symbols and the public ones of the paclets it depends on. A symbol with no definition at all, such as a pattern's name, uses nothing and is used by nothing. Each symbol's cone, everything it uses and what that uses, has one digest. A page uses the symbols its markdown names, and its key has their cones' digests, and what no definition shows: the `PacletInfo.wl`, the files that the loading did not read, such as data a function reads when it runs, under `Kernel/` and `Assets/` and under the roots its `PacletInfo.wl` declares for its Kernel, LibraryLink, AutoCompletionData and Asset extensions, the definitions of ``System` `` and ``Global` `` symbols that the loading made, and the converter. The paclet's dependencies, and theirs all the way down, are loaded with it, also one it loads only on first use, so their symbols are in the cones: a new release of a dependency converts again only the pages that reach what it changed. A file that the loading did not read counts for the pages that reach a symbol whose code names it, by a string that is a directory on its path below the paclet's, its name, its name without extension or its asset name; a dependency's file that no code names, and its `PacletInfo.wl` without its version, count for the pages that reach any of its symbols, and the paclet's own for every page. On a build server, where `WOLFRAM_CLOUD_USER` and `WOLFRAM_CLOUD_PASSWORD` are set, the scanning kernel connects to the cloud as that account, since a dependency may reach the cloud as it loads. A dependency that an installed newer version outranks is reported, and a dependency that does not load is in every page's key whole. So a change to one function converts again only the pages that reach it. The scan is kept in the cache, and done again when the code changes; with `"Monitor" -> True` it prints a line at each step.
- The converter caches each example's result by the code of its cell and of the cells before it that it runs after. The build keeps that cache true to the paclet's code, example by example: it records the code each result was evaluated against, what those cells' names reach, and before a page is converted, deletes the results whose code has changed. The converter is no part of that code, so another converter, or a redeploy of the published one, converts every page again and evaluates no example again. A page whose examples all keep their results evaluates none of them; the converter evaluates every example of a page that has one without a result. The results are kept in the cache directory, under `examples/`, so that a build server that keeps the cache keeps them too, and a build of every page keeps only the results of its pages' examples.
- With `"CacheKey" -> "Code"`, or when the paclet has no primary context, its examples run on another kernel or the scan fails, the code is the paclet's whole code, the files under `Kernel/` and `Assets/` of the paclet and of its dependencies, and the converter. A page whose examples last ran against other code is then converted with the converter's cache off: its examples are evaluated afresh. Another converter, or a redeploy of the published one, evaluates every example afresh.
- With `"Parallel" -> True` or a number of kernels, the pages are converted on parallel kernels, launched for the build and closed after it; with `Automatic`, on the parallel kernels already running, if any. This kernel converts the first page, so that anything the converter installs is in place first, and tries again any page that failed on a parallel kernel.
- The parallel kernels a page's examples launch are closed once the page is converted, so that none of them is left to the examples of the pages after it.
- The ``Global` `` symbols a page gives definitions to are cleared once the page is converted, so that a page whose examples put ``Global` `` on their context path, and so read and assign names there, leaves no value to the pages after it. A symbol defined before the page keeps its definitions.
- A parallel kernel takes the switches of the `General` messages from the kernel that builds, such as a script kernel's `General::shdw` off, so that a page shows the same messages whichever kernel converts it.
- A build starts no asynchronous transfer of its own kernel's: the paclet manager's weekly update of its sites and the resource system's refresh of its information are kept from starting while it runs, and in every kernel it starts, as an asynchronous transfer can crash a Wolfram 15.0 kernel. When PacletPage loads in a kernel whose sites were last updated over three days ago, as in a new container, it updates them at once, synchronously; each site has twelve seconds, a site that cannot be reached ends the update without a message, and loading goes on.
- Each conversion has `"TimeConstraint"` seconds. A page that runs past them fails, unless it wrote its whole notebook first. On a parallel kernel the limit is also kept from outside: a page whose code never yields has its kernel killed shortly after the limit and is converted once more on another kernel; stopped a second time, it fails as timed out, and the build goes on with the other pages.
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
| `"Converter"` | `Automatic` | the converter: a function, a file, a saved converter as `File[...]` of its `.wxf`, or the URL of a published function |
| `"Prune"` | `False` | whether to delete the notebooks of pages whose markdown is gone |
| `"Parallel"` | `Automatic` | `True`, a number of kernels, or `False` |
| `"TimeConstraint"` | `1800` | the seconds each page may take |
| `"CacheDirectory"` | `Automatic` | where the cache is, by default `build/cache` |
| `"Dependencies"` | `Automatic` | the paclets whose code the examples run besides the paclet's own, as directories, which are loaded, or names; by default the ones its `PacletInfo.wl` declares |
| `"CacheExcludedFiles"` | `{}` | files of the paclet and its dependencies, as paths relative to each paclet's directory with `*` for any characters, whose changes leave the cached results of the examples as they are, such as `"Kernel/Usage.wl"` of usage messages |
| `"CacheVersion"` | `True` | whether the version in a `PacletInfo.wl` is part of the key of the cached results; with `False` a new version alone keeps them, for pages whose examples do not show it |
| `"CacheKey"` | `Automatic` | what a page's key has of the paclet's code: `"Definitions"`, the definitions the page reaches, `"Code"`, all of it; `Automatic` is `"Definitions"` for a paclet with a primary context whose examples run here |
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

### CacheKey

A copy of the example paclet, built once:

```wl
dir = CopyDirectory[PacletObject["WolframInstitute/PacletPage"]["AssetLocation", "Example"], FileNameJoin[{$TemporaryDirectory, CreateUUID["Greetings-"]}]];
BuildPacletDocumentation[dir];
```

Add a function that no page names to the paclet's code:

```wl
file = FileNameJoin[{dir, "Kernel", "Greetings.wl"}];
Export[file, ByteArrayToString[ReadByteArray[file], "UTF-8"] <> "\nDemo`Greetings`Private`unused[x_] := x\n", "Text"]
```

No page reaches it, so every page is copied from the cache:

```wl
BuildPacletDocumentation[dir]
```

With the paclet's whole code in the key, the pages are converted again and their examples evaluated afresh:

```wl
BuildPacletDocumentation[dir, "CacheKey" -> "Code"]
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
