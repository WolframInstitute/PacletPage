---
Template: Symbol
Name: UninstallPacletDependencies
Context: WolframInstitute`PacletPage`
Paclet: WolframInstitute/PacletPage
URI: WolframInstitute/PacletPage/ref/UninstallPacletDependencies
Keywords: [uninstall, remove, user repository]
SeeAlso: [PacletDependencyInformation, InstallPacletDependencies, UpdatePacletDependencies, PacletDependencyDialog]
RelatedGuides: [PacletDependencyManagement]
RelatedTutorials: [DependencyManagement]
---

## Usage

<code>[UninstallPacletDependencies]()[*owner*, *names*]</code> removes user-installed versions of the selected dependencies.

## Details & Options

- Give a full dependency name, a list, or `All`. Each successfully removed or already absent dependency has the value `Null` in the result association.
- Removal applies to the current user’s paclet repository. A dependency supplied solely by the Wolfram installation or a development directory gives `"ExternalInstallation"`.
- Removing a user installation affects other paclets that use it. A system copy or a registered development copy can become visible after removal.
- Loaded definitions remain in the current kernel. Start a fresh kernel to clear them.

## Basic Examples

After installing the consumer from the Dependency Management tutorial, remove its optional type checker.

```wl
#| eval: false
UninstallPacletDependencies["Demo/Consumer", "WolframInstitute/TypeTheory"]
```

Inspect the locally visible dependencies after removal.

```wl
#| eval: false
PacletDependencyInformation["Demo/Consumer"]
```
