---
Template: Symbol
Name: SubmitPacletResource
Context: WolframInstitute`PacletPage`
Paclet: WolframInstitute/PacletPage
URI: WolframInstitute/PacletPage/ref/SubmitPacletResource
Keywords: [Paclet Repository, submit, ResourceSubmit, definition notebook, headless]
SeeAlso: [BuildPacletDocumentation, DeployPacletResource, PublishPacletPage]
RelatedGuides: [PacletPage]
RelatedTutorials: [PublishingADocumentationSite]
---

## Usage

`SubmitPacletResource[dir]` builds the paclet in *dir* afresh, checks its definition notebook and the archive, and submits the paclet to the Wolfram Paclet Repository, without a front end.

## Details & Options

- The definition notebook's Submit button asks for the version in a dialog, which a kernel without a front end cannot show. `SubmitPacletResource` makes the same submission headless: the notebook scraped into a resource and handed to `ResourceSubmit` with the version and publisher of `PacletInfo.wl`.
- Before anything is built, it checks the definition notebook, and any error stops the submission. The exception is the missing file listing of `PacletInfo.wl`, which a definition notebook built from markdown lacks; the built paclet takes its place.
- The archive is built afresh into `build/`, in a kernel of its own, and nothing is submitted unless it holds a page for every documentation notebook and loads, alone, in a fresh kernel: a build that fails on the pages still succeeds, with none, and a submission without them publishes a paclet without documentation.
- With `"EvaluationKernel"`, for a paclet that loads only on another kernel, the fresh kernel the archive loads on is one of that spec, as `DeployPacletResource` checks it.
- When `Documentation` already holds finished pages, give their authoring notebooks as `"DocumentationSource"`: the paclet is built from a copy whose documentation is those notebooks, since rebuilding finished pages packs none.
- The scrape reads the definition notebook while it is open, with the notebook's own hints, which a headless front end otherwise looks for in a notebook it does not have; and the packed archive must be the version being submitted.
- The connected account must own the paclet's publisher. A rejected request reports the repository's answer as a message.
- The repository takes only a version newer than the ones it has, so a version it has already is refused before anything is built; a dry run reports it and goes on.
- A dry run as another account than `"Account"` reports it and goes on. A dry run without a cloud session runs offline: it builds and loads the archive, without the account check, the notebook check and the scrape, which need a session.
- The archive is built as `DeployPacletResource` builds it, and takes the same options for it: `"Exclude"`, for symbolic links that lead outside the paclet and other files to leave out, `"ArchiveCheck"`, `"Symbols"`, `"Incremental"`, `"Parallel"` and `"TimeConstraint"`. A paclet that declares LibraryLink libraries must have some in the archive.
- The result has `"Name"`, `"Version"`, `"Account"`, the account it submits as, `"Submitted"`, `"Archive"` and `"Pages"`, and the repository's answer as `"Result"`.
- The following options can be given:

| Option | Default | Description |
|---|---|---|
| `"Account"` | `Automatic` | the Wolfram ID that must be connected |
| `"DryRun"` | `False` | do everything but the submission |
| `"DocumentationSource"` | `Automatic` | the authoring notebooks to build from, when `Documentation` holds pages already built |
| `"LoadCheck"` | `True` | whether to load the built archive in a fresh kernel first |
| `"EvaluationKernel"` | `None` | the kernel the archive is checked to load alone on, a command or an association as `BuildPacletDocumentation` takes it, for a paclet that loads only there; `None` checks it in a fresh kernel of this Wolfram |
| `"Exclude"` | `{}` | patterns for the paths within the paclet the archive leaves out |
| `"ArchiveCheck"` | `None` | a function of the built archive that must give `True` |
| `"Symbols"` | `Automatic` | the symbols the archive declares, when the paclet declares none |
| `"Incremental"` | `False` | whether to build only the pages whose notebooks changed |
| `"Parallel"` | `False` | the number of processes that build the changed pages, or `True` |
| `"TimeConstraint"` | `900` | the seconds each page may take in those processes |

## Basic Examples

A copy of the example paclet that comes with PacletPage, with its documentation built:

```wl
dir = CopyDirectory[PacletObject["WolframInstitute/PacletPage"]["AssetLocation", "Example"], FileNameJoin[{$TemporaryDirectory, CreateUUID["Greetings-"]}]]
```

```wl
BuildPacletDocumentation[dir]
```

A dry run checks the definition notebook, builds and checks the archive and scrapes the submission, but submits nothing:

```wl
submission = SubmitPacletResource[dir, "DryRun" -> True]
```

The version that would be submitted, from `PacletInfo.wl`:

```wl
submission["Version"]
```

The copy is no longer needed:

```wl
DeleteDirectory[dir, DeleteContents -> True]
```
