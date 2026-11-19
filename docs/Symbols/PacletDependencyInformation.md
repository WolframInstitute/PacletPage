---
Template: Symbol
Name: PacletDependencyInformation
Context: WolframInstitute`PacletPage`
Paclet: WolframInstitute/PacletPage
URI: WolframInstitute/PacletPage/ref/PacletDependencyInformation
Keywords: [manifest, installed version, optional libraries, readiness]
SeeAlso: [InstallPacletDependencies, UpdatePacletDependencies, UninstallPacletDependencies, PacletDependencyDialog]
RelatedGuides: [PacletDependencyManagement]
RelatedTutorials: [DependencyManagement]
---

## Usage

<code>[PacletDependencyInformation]()[*owner*]</code> gives the dependencies declared by *owner*, with their installed versions and local status.

<code>[PacletDependencyInformation]()[*owner*, *names*]</code> gives information for the selected dependencies.

## Details & Options

- Give an installed paclet name, a paclet directory, `File[dir]` or a `PacletObject`. Owner names resolve locally.
- The result is an association keyed by full paclet name. Each value contains the declaration, `"Required"`, `"InstalledVersion"` and `"Status"`. A missing installation has version `Missing["NotInstalled"]`.
- Names can be a string, a list of strings or `All`. The order follows the selection or the manifest. An unknown name gives a `Failure` before checking any entry.
- `"CheckReadiness" -> True` runs a declared local check for an installed library. A failed check supplies its diagnostic in `"ReadinessFailure"`. Possible statuses include `"Not installed"`, `"Installed"`, `"Ready"`, `"Needs setup"`, `"Unsupported"`, `"Version mismatch"` and `"Restart required"`.
- `"CheckUpdates" -> True` adds `"LatestVersion"` for each supported library. Its value is a version string or a `Failure` describing the lookup. Both options default to `False`.

## Basic Examples

This consumer uses the build tools bundled with the Wolfram System.

```wl
consumer = PacletObject[<|"Name" -> "Demo/Consumer", "Version" -> "1.0", "Dependencies" -> {"PacletTools"}|>];
```

Read the declaration and the installed version.

```wl
PacletDependencyInformation[consumer]
```
