---
Template: Symbol
Name: PublishPacletPage
Context: WolframInstitute`PacletPage`
Paclet: WolframInstitute/PacletPage
URI: WolframInstitute/PacletPage/ref/PublishPacletPage
Keywords: [publish, release, paclet, documentation, site, pipeline]
SeeAlso: [BuildPacletDocumentation, DeployPacletResource, SubmitPacletResource, DeployPacletPage, CheckPacletPage]
RelatedGuides: [PacletPage]
RelatedTutorials: [PublishingADocumentationSite]
---

## Usage

`PublishPacletPage[dir, name]` builds the documentation of the paclet in *dir*, deploys its resource, publishes the site *name* around it and checks the site.

## Details & Options

- The account is checked before anything is built, since a paclet's documentation may publish as it is built: with `"Account"`, any other connected account is refused, and nothing is built or published.
- It runs, in one kernel and so as one cloud account:
  - `BuildPacletDocumentation`, the markdown into documentation notebooks;
  - `DeployPacletResource`, the paclet resource;
  - `PacletPageCatalog`, of the resource just deployed;
  - `DeployPacletPage`, the site, with the paclet's authoring notebooks as each page's source;
  - `CheckPacletPage`, every page;
  - and, with `"Submit" -> True`, `SubmitPacletResource`.
- The result has `"Build"`, `"Resource"`, `"Site"`, `"Check"` and `"Submission"`: each step's result, `None` for a step not taken.
- With `"DryRun" -> True`, it builds the documentation and the archive, loads the archive in a fresh kernel and scrapes the resource, but deploys and submits nothing, and publishes no site. Without a cloud session the dry run runs offline.
- The following options can be given:

| Option | Default | Description |
|---|---|---|
| `"Account"` | `Automatic` | the Wolfram ID that must be connected |
| `"Build"` | `True` | whether to build the documentation first |
| `"Check"` | `True` | whether to check the site |
| `"Submit"` | `False` | whether to submit the paclet to the Paclet Repository |
| `"DryRun"` | `False` | build and scrape, deploy nothing |
| `"BuildOptions"` | `{}` | options for `BuildPacletDocumentation` |
| `"ResourceOptions"` | `{}` | options for `DeployPacletResource`, such as `"Archive"` or `"Exclude"`; the submission builds its archive with them too |
| `"SiteOptions"` | `{}` | options for `DeployPacletPage` |

- PacletPage releases itself with `PublishPacletPage`, in its `release.wls`.

## Basic Examples

A copy of the example paclet that comes with PacletPage:

```wl
dir = CopyDirectory[PacletObject["WolframInstitute/PacletPage"]["AssetLocation", "Example"], FileNameJoin[{$TemporaryDirectory, CreateUUID["Greetings-"]}]]
```

A dry run of its whole release, its submission too:

```wl
release = PublishPacletPage[dir, "Greetings", "DryRun" -> True, "Submit" -> True]
```

The documentation it built:

```wl
release["Build"]
```

Where its resource would be published:

```wl
release["Resource", "Resource"]
```

The version it would submit:

```wl
release["Submission", "Version"]
```

The copy is no longer needed:

```wl
DeleteDirectory[dir, DeleteContents -> True]
```
