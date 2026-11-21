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
- The result is a [PacletDependencyReport](). In a notebook, expand its summary to see the details. `report["Publisher/Library"]` gives the library's record. `Normal[report]` returns the association keyed by full name. Use `"Output" -> "Association"` to request that association directly.
- Each record contains the declaration, `"Required"`, `"InstalledVersion"` and `"Status"`. A missing installation has version `Missing["NotInstalled"]`. The consumer's description takes precedence over the description supplied by the installed library.
- For an installed Git dependency, `"InstalledGitSource"` gives its recorded repository, subdirectory, and full commit, or `Missing["Unknown"]` for a copy of unknown origin. `"GitRevisionMatch"` indicates whether it matches the declaration. An otherwise compatible installation with a different or unknown revision has status `"Revision mismatch"`.
- Names can be a string, a list of strings or `All`. The order follows the selection or the manifest. An unknown name gives a `Failure` before checking any entry.
- `"CheckReadiness" -> True` runs the declared check when the installed library satisfies the version and Git requirements and can run in this kernel. Success sets the status to `"Ready"`. Failure sets it to `"Needs setup"`. Expand the report to read messages from loading the consumer or running its check. The underlying `Failure` is available as `"ReadinessFailure"`.
- The other local statuses are `"Not installed"`, `"Installed"`, `"Unsupported"`, `"Version mismatch"` and `"Restart required"`.
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
