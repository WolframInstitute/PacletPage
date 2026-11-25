---
Template: Symbol
Name: InstallPacletDependencies
Context: WolframInstitute`PacletPage`
Paclet: WolframInstitute/PacletPage
URI: WolframInstitute/PacletPage/ref/InstallPacletDependencies
Keywords: [install, archive, paclet sites, readiness]
SeeAlso: [PacletDependencyInformation, UpdatePacletDependencies, UninstallPacletDependencies, PacletDependencyDialog]
RelatedGuides: [PacletDependencyManagement]
RelatedTutorials: [DependencyManagement]
---

## Usage

<code>[InstallPacletDependencies]()[*owner*, *names*]</code> installs the selected dependencies and runs their declared readiness checks.

## Details & Options

- Give a full dependency name, a list, or `All`. The result is an association with a `PacletObject` or a `Failure` for each selected dependency.
- A compatible installation is reused. A check that returns anything other than `True` gives a `"DependencySetupRequired"` failure. The installed files remain available for setup and another check.
- A dependency can come from a configured paclet site, a published Wolfram Cloud resource, a local `.paclet` archive, or a GitHub repository at a specified commit. For a Git dependency, reusing an installation requires the same repository, subdirectory, and commit.
- Installing, updating and removing a Git dependency run one at a time across kernels. While another kernel is working on the same library, a call waits ten seconds and then gives a `"DependencyBusy"` failure.
- A failure for one declared library leaves the other selections available for installation. An unknown selection is rejected before any installation starts.

## Basic Examples

The Wolfram System already supplies `PacletTools`. Installing the declared dependency returns that copy.

```wl
consumer = PacletObject[<|"Name" -> "Demo/Consumer", "Version" -> "1.0", "Dependencies" -> {"PacletTools"}|>];
```

```wl
InstallPacletDependencies[consumer, "PacletTools"]
```
