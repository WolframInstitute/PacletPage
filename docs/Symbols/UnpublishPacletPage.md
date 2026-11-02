---
Template: Symbol
Name: UnpublishPacletPage
Context: WolframInstitute`PacletPage`
Paclet: WolframInstitute/PacletPage
URI: WolframInstitute/PacletPage/ref/UnpublishPacletPage
Keywords: [unpublish, delete, cloud objects, site, resource, Databin, dry run]
SeeAlso: [PublishPacletPage, DeployPacletPage, DeployPacletResource, SetPacletPagePermissions]
---

## Usage

<code>[UnpublishPacletPage]()[*dir*, *name*]</code> gives everything [PublishPacletPage]() deployed for the paclet in *dir* under the site *name* in the connected account, deleting nothing.

<code>[UnpublishPacletPage]()[*paclet*, *name*, "Delete" -> True]</code> deletes it, for the paclet named *paclet*, such as `"Publisher/Name"`.

## Details & Options

- The objects are those under the roots the publish deploys to, named as it names them: the site's objects *name*, *name*`Page`, *name*`Feedback`, *name*`Source` and *name*`SourceManifest`, the paclet resource's directory under `DeployedResources`, and in each archive directory the paclet's own archives. Another site or paclet whose name starts with the same characters is never touched.
- The result is an association: `"Roots"`, `"Objects"` (deepest first, the order they are deleted in), `"Databin"`, the id of the feedback form's Databin or `None`, and `"Deleted"`. After a delete, `"Failed"` lists the objects that could not be deleted.
- The feedback form's Databin holds the feedback the site received. It is deleted only with `"DeleteDatabin" -> True`.
- A resource submitted to the Paclet Repository is not the deployed resource and stays.
- The options:

| Option | Default | |
| --- | --- | --- |
| `"Delete"` | `False` | delete the objects; without it, list them |
| `"DeleteDatabin"` | `False` | delete the feedback form's Databin too |
| `"Account"` | `Automatic` | the account the connected one must be |
| `"ArchiveDirectories"` | `{"Paclets"}` | the directories of the account the paclet's archives are looked for in |

## Basic Examples

The options and their defaults: nothing is deleted unless asked:

```wl
Options[UnpublishPacletPage]
```
