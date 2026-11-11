---
Template: Symbol
Name: PacletDefinitionScan
Context: WolframInstitute`PacletPage`
Paclet: WolframInstitute/PacletPage
URI: WolframInstitute/PacletPage/ref/PacletDefinitionScan
Keywords: [cache, definitions, dependencies, graph, fingerprint]
SeeAlso: [PacletPageDependencies, BuildPacletDocumentation]
RelatedGuides: [PacletPage]
RelatedTutorials: [CachingADocumentationBuild]
---

## Usage

`PacletDefinitionScan[dir]` loads the paclet in *dir* and its dependencies in a fresh kernel and gives what the documentation cache is keyed by: each symbol's fingerprint, the digest of everything it uses, and the graph of the uses.

## Details & Options

- The scan is the one `BuildPacletDocumentation` makes before it converts a page. It is kept in the cache directory, and read from there while the paclet's and its dependencies' files are the same.
- The result is an association:

| Key | Value |
|---|---|
| `"Symbols"` | each symbol that has a definition, by full name, with the fingerprint of its whole definition |
| `"Cones"` | each symbol with the digest of its cone: itself, the symbols it uses, and the symbols those use |
| `"Graph"` | the graph of the symbols, an edge from each symbol to each one it uses |
| `"Contexts"` | the contexts scanned: those the paclet and its dependencies declare, and the contexts under them |
| `"Dependencies"` | the directories of the paclet's dependencies, all the way down |
| `"FilesRead"` | the paclets' files that the loading read |
| `"FilesNamed"` | the files that the loading did not read and that some symbol's code names |
| `"SystemChanged"` | the ``System` `` and ``Global` `` symbols whose definitions the paclet's own loading changed |
| `"Unloaded"` | the contexts of the dependencies that did not load |
| `"Outranked"` | the dependencies that an installed newer version outranks |

- A symbol uses the symbols of its code. It also uses a name in a string of its code: a full name, or a bare name of a symbol of the same paclet or of a context that the code, or code it calls, holds as a string.
- The following options can be given:

| Option | Default | Description |
|---|---|---|
| `"Dependencies"` | `Automatic` | the paclets scanned with the paclet, as directories or names; by default the ones its `PacletInfo.wl` declares, and theirs |
| `"CacheDirectory"` | `Automatic` | where the scan is kept, by default `build/cache` |
| `"CacheExcludedFiles"` | `{}` | files left out of the keys, as paths relative to each paclet's directory with `*` for any characters |
| `"Monitor"` | `False` | whether to print a line at each step of the scan |

## Basic Examples

A copy of the example paclet that comes with PacletPage:

```wl
dir = CopyDirectory[PacletObject["WolframInstitute/PacletPage"]["AssetLocation", "Example"], FileNameJoin[{$TemporaryDirectory, CreateUUID["Greetings-"]}]];
```

Scan its definitions:

```wl
scan = PacletDefinitionScan[dir]
```

The fingerprint of each symbol that has a definition:

```wl
scan["Symbols"]
```

The graph of what uses what:

```wl
scan["Graph"]
```

```wl
DeleteDirectory[dir, DeleteContents -> True]
```
