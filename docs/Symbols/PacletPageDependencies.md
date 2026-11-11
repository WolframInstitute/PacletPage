---
Template: Symbol
Name: PacletPageDependencies
Context: WolframInstitute`PacletPage`
Paclet: WolframInstitute/PacletPage
URI: WolframInstitute/PacletPage/ref/PacletPageDependencies
Keywords: [cache, page, dependencies, cone, key]
SeeAlso: [PacletDefinitionScan, BuildPacletDocumentation]
RelatedGuides: [PacletPage]
RelatedTutorials: [CachingADocumentationBuild]
---

## Usage

`PacletPageDependencies[dir, file]` gives what the markdown page *file* of the paclet in *dir* depends on: the symbols it names, every symbol those reach, and the code its cache key holds.

## Details & Options

- The result is an association:

| Key | Value |
|---|---|
| `"Named"` | the paclet's and its dependencies' symbols that the page's markdown names |
| `"Reached"` | those symbols and every symbol they use, directly or through others |
| `"Graph"` | the graph of the reached symbols |
| `"Code"` | the code in the page's cache key: the digests of the named symbols' cones and the shared part |
| `"Shared"` | the part of the code every page shares |

- `BuildPacletDocumentation` converts the page again when `"Code"` changes, which it does when the definition of a reached symbol changes, or the shared part.
- A page names a symbol by its full name or by its own name, anywhere in its markdown: in its code, its text and its front matter. The page's file name is a name too.
- The following options can be given:

| Option | Default | Description |
|---|---|---|
| `"Dependencies"` | `Automatic` | the paclets scanned with the paclet |
| `"CacheDirectory"` | `Automatic` | where the scan is kept, by default `build/cache` |
| `"CacheExcludedFiles"` | `{}` | files left out of the keys |
| `"CacheVersion"` | `True` | whether the version in `PacletInfo.wl` is part of the shared part |
| `"Converter"` | `Automatic` | the converter, which is part of the shared part |
| `"Monitor"` | `False` | whether to print a line at each step of the scan |

- The options are those of the build whose key is asked for: with other values the code is another.

## Basic Examples

A copy of the example paclet that comes with PacletPage:

```wl
dir = CopyDirectory[PacletObject["WolframInstitute/PacletPage"]["AssetLocation", "Example"], FileNameJoin[{$TemporaryDirectory, CreateUUID["Greetings-"]}]];
```

What the page of `Greet` depends on:

```wl
dependencies = PacletPageDependencies[dir, FileNameJoin[{dir, "docs", "Symbols", "Greet.md"}]]
```

The symbols it names:

```wl
dependencies["Named"]
```

```wl
DeleteDirectory[dir, DeleteContents -> True]
```
