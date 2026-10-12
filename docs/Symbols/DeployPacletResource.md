---
Template: Symbol
Name: DeployPacletResource
Context: WolframInstitute`PacletPage`
Paclet: WolframInstitute/PacletPage
URI: WolframInstitute/PacletPage/ref/DeployPacletResource
Keywords: [paclet resource, deploy, cloud, PacletBuild, DeployedResources]
SeeAlso: [BuildPacletDocumentation, SubmitPacletResource, PublishPacletPage, DeployPacletPage]
RelatedGuides: [PacletPage]
RelatedTutorials: [PublishingADocumentationSite]
---

## Usage

`DeployPacletResource[dir]` builds the paclet in *dir* afresh, checks the archive holds every documentation page, and deploys the paclet resource publicly to the connected cloud account.

## Details & Options

- It is what the definition notebook's Deploy button does, without a front end: the paclet's resource page and documentation pages are published at `DeployedResources/Paclet/<Publisher>/<Name>` in the connected account.
- `dir` needs its documentation notebooks and `ResourceDefinition.nb`, which `BuildPacletDocumentation` writes.
- The archive is built afresh into `build/` every time, in a kernel of its own, since the deploy packs the archive it finds there; `PacletBuild` also turns the documentation notebooks into finished pages. A build that fails on the pages still succeeds, with none, so nothing is deployed unless the archive holds a page for every documentation notebook.
- The archive is then loaded in a fresh kernel, extracted to a temporary directory: its primary context must load without a message, from there, at the archive's version. The kernel that deploys has the paclet's source loaded, so it cannot tell. A message about two of the kernel's own packages defining one symbol, which names none of the paclet's contexts, does not count.
- Both fresh kernels load the paclet directories this kernel has loaded with `PacletDirectoryLoad`, such as the paclet's dependencies, but never the paclet's own source or another copy of it.
- A symbolic link in the paclet that leads outside it would put what it leads to in the archive, such as a runtime the paclet links to from a toolchain on this computer. Such a link is refused, unless `"Exclude"` leaves it out. `"Exclude"` takes string patterns for paths within the paclet, such as `"LibraryResources/*/libruntime*"`; the archive is then built from a copy of the paclet without them.
- A paclet whose `PacletInfo.wl` declares LibraryLink libraries must have some in the archive, under the extension's root. A check of the paclet's own, given as `"ArchiveCheck"`, is applied to the built archive's file name last, and must give `True`.
- With `"Incremental" -> True`, the pages built last time are kept, in `build/`, and only pages whose documentation notebook changed are built again; pages whose notebook is gone are removed. A page also depends on the guides that link to it, so a whole build, the default, is the one to publish after the guides change.
- With `"Parallel" -> n`, the changed pages are built in `n` kernel processes before `PacletBuild` runs, each with `"TimeConstraint"` seconds a page; `PacletBuild` builds whatever they did not, makes the search indexes and packs the archive. `"Parallel" -> True` starts as many processes as the processors and the memory allow.
- The resource page is complete from its first visit, on any cloud subscription: its Details, the definition's examples, the main guide, the hero image (or the image given as `"HeroImage"`) and the source link. Deploying again replaces the page, the main guide, the example notebooks and every documentation page in place, so they stay readable throughout; the pages the new documentation no longer has are deleted once the deploy is complete.
- A failed cloud write stops the deploy at once, with a `DeployPacletResource::write` message, and the result is a `Failure` whose `"Step"` names where it stopped. Nothing has been deleted then, so the resource keeps serving its pages; deploy again.
- The resource page links the paclet's symbols in its examples, so that copied code runs in a fresh kernel, for the symbols the paclet declares in its Kernel extension. A paclet that declares none gets the public symbols the fresh kernel found, in the archive's `PacletInfo.wl` only; `"Symbols"` gives the list instead, or `None` leaves the archive as built.
- The deployed resource records, in its `MetaInformation`, the paclet's version and the git commit it was built from. A version already deployed from another commit, or with no commit recorded, is refused before anything is built, since what installs as that version would silently change; `"Republish" -> True` replaces it. Outside a git checkout the commit is unknown, so give it as `"Commit"`.
- With `"Archive" -> dir`, the built archive is also published in the cloud directory `dir`, as `<Publisher>__<Name>.paclet`, the latest, and with its version in the name, so `PacletInstall` can install it from a URL alone. The versioned archive records the commit too, and is refused the same way.
- The deployed resource has the paclet's version as its `"Version"`.
- The result has `"Resource"`, the resource's URL; `"Account"`, the account it is deployed as, `None` offline; `"Deployed"`; `"Archive"`, the built archive; `"Pages"`, the documentation pages in it; `"Documentation"`, how many pages changed, were removed and were built in parallel; `"DeclaredSymbols"`, how many symbols the archive was given, if any; `"HeroImage"`, whether the page has one; `"Install"`, the archive's URLs when `"Archive"` publishes it; `"ArchiveSize"`; `"Version"`; `"Commit"`; `"Load"`, the fresh kernel's report; and `"AppearanceElements"`, whether the account's subscription takes the resource notebooks' appearance settings: `False` where it refuses them and the notebooks deploy with the cloud's standard toolbar, `None` when nothing was deployed; and `"Removed"`, how many documentation pages the new documentation no longer has were deleted.
- A dry run writes nothing to the cloud. With a session, the account check and the version gate report what a real deploy would refuse, and the dry run goes on; as another account than `"Account"`, it has no deployment to gate. Without a cloud session it runs offline: it builds and loads the archive, without the account check, the version gate, the deploy location and the scrape, which all need a session, so a build server needs no credentials for it.
- The following options can be given:

| Option | Default | Description |
|---|---|---|
| `"Account"` | `Automatic` | the Wolfram ID that must be connected |
| `"DryRun"` | `False` | do everything but the deploy |
| `"DocumentationSource"` | `Automatic` | the authoring notebooks to build from, when `Documentation` holds pages already built |
| `"HeroImage"` | `Automatic` | the resource page's image: by default the definition notebook's; an image, a file, or `None` |
| `"Archive"` | `None` | a cloud directory to publish the archive in, for `PacletInstall` |
| `"Commit"` | `Automatic` | the commit the release records; by default the paclet's git commit, marked `+dirty` and a hash of its sources when it has uncommitted changes |
| `"Republish"` | `False` | whether to replace a version deployed from another commit |
| `"LoadCheck"` | `True` | whether to load the built archive in a fresh kernel first |
| `"Exclude"` | `{}` | patterns for the paths within the paclet the archive leaves out |
| `"ArchiveCheck"` | `None` | a function of the built archive that must give `True` |
| `"Symbols"` | `Automatic` | the symbols the archive declares, when the paclet declares none |
| `"Incremental"` | `False` | whether to build only the pages whose notebooks changed |
| `"Parallel"` | `False` | the number of processes that build the changed pages, or `True` |
| `"TimeConstraint"` | `900` | the seconds each page may take in those processes |
| `Permissions` | `"Public"` | the resource's permissions |

## Basic Examples

A copy of the example paclet that comes with PacletPage, with its documentation built:

```wl
dir = CopyDirectory[PacletObject["WolframInstitute/PacletPage"]["AssetLocation", "Example"], FileNameJoin[{$TemporaryDirectory, CreateUUID["Greetings-"]}]]
```

```wl
BuildPacletDocumentation[dir]
```

A dry run builds the archive, checks its pages and scrapes the resource, but deploys nothing:

```wl
deploy = DeployPacletResource[dir, "DryRun" -> True]
```

Where the resource would be published:

```wl
deploy["Resource"]
```

How many documentation pages the archive holds:

```wl
deploy["Pages"]
```

How the archive loaded, alone, in a fresh kernel:

```wl
deploy["Load"]
```

How big the archive is:

```wl
deploy["ArchiveSize"]
```

The copy is no longer needed:

```wl
DeleteDirectory[dir, DeleteContents -> True]
```

## Possible Issues

A copy of the example paclet, with its documentation built:

```wl
dir = CopyDirectory[PacletObject["WolframInstitute/PacletPage"]["AssetLocation", "Example"], FileNameJoin[{$TemporaryDirectory, CreateUUID["Greetings-"]}]];
BuildPacletDocumentation[dir];
```

A resource is deployed only from the account `"Account"` names; any other connected account is refused before anything is built:

```wl
DeployPacletResource[dir, "Account" -> "someone@example.org"]
```

```wl
DeleteDirectory[dir, DeleteContents -> True]
```
