---
Template: Symbol
Name: UpdatePacletDependencies
Context: WolframInstitute`PacletPage`
Paclet: WolframInstitute/PacletPage
URI: WolframInstitute/PacletPage/ref/UpdatePacletDependencies
Keywords: [update, version, upgrade, restart]
SeeAlso: [PacletDependencyInformation, InstallPacletDependencies, UninstallPacletDependencies, PacletDependencyDialog]
RelatedGuides: [PacletDependencyManagement]
RelatedTutorials: [DependencyManagement]
---

## Usage

<code>[UpdatePacletDependencies]()[*owner*, *names*]</code> updates the selected installed dependencies to newer compatible releases.

## Details & Options

- Give a full dependency name, a list, or `All`. Each result is a `PacletObject` or a `Failure` keyed by dependency name.
- An equal or newer compatible local version is retained. When an update replaces a library whose context has loaded, its status is `"Restart required"` for the rest of the kernel session.
- A missing library gives `"DependencyNotInstalled"`. Use [InstallPacletDependencies]() for the first installation.
- An interrupted cloud download leaves the previous version available. The next call can retry the operation.

## Basic Examples

After installing the consumer from the Dependency Management tutorial, apply an available update to its optional library.

```wl
#| eval: false
UpdatePacletDependencies["Demo/Consumer", "WolframInstitute/TypeTheory"]
```

Check which version the kernel sees.

```wl
#| eval: false
PacletDependencyInformation["Demo/Consumer", "WolframInstitute/TypeTheory"]
```
