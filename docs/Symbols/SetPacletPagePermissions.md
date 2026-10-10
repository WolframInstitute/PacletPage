---
Template: Symbol
Name: SetPacletPagePermissions
Context: WolframInstitute`PacletPage`
Paclet: WolframInstitute/PacletPage
URI: WolframInstitute/PacletPage/ref/SetPacletPagePermissions
Keywords: [permissions, cloud, directory, public]
SeeAlso: [CheckPacletPage, DeployPacletPage]
RelatedGuides: [PacletPage]
RelatedTutorials: [PublishingADocumentationSite]
---

## Usage

`SetPacletPagePermissions[dir, permissions]` sets `permissions` on the cloud directory `dir` and on every directory beneath it.

## Details & Options

- `dir` is a cloud object name, a URL or a `CloudObject`.
- The directories are found by listing each directory's subdirectories, one request per directory, never by probing each object, so a tree of thousands of pages is enumerated in seconds.
- The permissions are set on every directory found, in parallel when the caller has launched parallel kernels; none is skipped.
- The result says how many directories and objects were set and which failed.
- The following option can be given:

| Option | Default | Description |
|---|---|---|
| `"Objects"` | `"Directories"` | `All` to set the permissions on every object in the tree too |

## Basic Examples

Make the directories of PacletPage's deployed resource, in the connected account, public:

```wl
SetPacletPagePermissions["DeployedResources/Paclet/WolframInstitute/PacletPage", "Public"]
```

## Options

### Objects

Set the permissions on every page and notebook of the tree too:

```wl
SetPacletPagePermissions["DeployedResources/Paclet/WolframInstitute/PacletPage", "Public", "Objects" -> All]
```
