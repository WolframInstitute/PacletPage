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

- Give a full dependency name, a list, `All`, `"Required"`, or `"Optional"`. The result is an association with a `PacletObject` or a `Failure` for each selected dependency.
- Pass `"CheckReadiness" -> False` to install the selected dependencies without running their declared readiness checks.
- Pass `"TargetDirectory" -> dir` to unpack each release into `dir` instead of the user paclet repository. The directory is loaded with `PacletDirectoryLoad`, so this kernel and the kernels it starts find its copies, which win a version tie with any other installation. Nothing is written under the user paclet base, and Git locks are kept in `dir` as well. A documentation build installs this way.
- Pass `"Pinned" -> True` to install what each declaration's `"Pin"` names instead of its own source and version, as a documentation build does. A pin's version is exact: a published release of another version is refused rather than taken. With a `"TargetDirectory"`, only a copy inside that directory counts as installed, and a newer copy elsewhere, which would load instead, gives a `"DependencyVersionConflict"` failure. Outside pinned mode a `"Pin"` is ignored.
- Only the consumer's own `"Dependencies"` list is read. A dependency's own dependencies are not followed, so declare every library the consumer needs.
- A compatible installation is reused. A check that returns anything other than `True` gives a `"DependencySetupRequired"` failure. The installed files remain available for setup and another check.
- If the consumer paclet bundles `.paclet` archives in its `"Dependencies"` Asset directory (`Assets/Dependencies/`, packaged with `"BundleDependencies"` during release), compatible bundled archives are installed offline first without contacting the network.
- A dependency can come from a configured paclet site, a published Wolfram Cloud resource, a local `.paclet` archive, or a Git repository at a specified commit. Git dependencies use `WolframInstitute/GitLink` for shallow fetch and sparse path checkout when available, with GitHub archive fallback. For a Git dependency, reusing an installation requires the same repository, subdirectory, and commit.
- An `"Archive"` can also be the https address of a `.paclet` file, with an exact `"Version"`. The download must contain that name and version, and when a SHA-256 is published beside it, as `<address>.sha256` in the format `sha256sum` writes, the download must match it.
- A download runs to its end however slowly it arrives; one that receives nothing for a minute is given up and tried once more.
- When the kernel cannot download a declared archive, as on a machine whose VPN routes the kernel's connections apart from other programs', the system's `curl` fetches it, and its published SHA-256, once more; the download is checked as any other is.
- For a paclet built for each system, `"Archive"` can be an association of system IDs, and `"All"` for any other system, to such addresses. A system without an address is unsupported, as with `"Platforms"`.
- Beside a `"Resource"`, an archive address must be the publisher's own cloud file, a file of a GitHub release (`https://github.com/<owner>/<repository>/releases/download/<tag>/<file>.paclet`), or the Paclet Repository's paclet site. Inside a `"Pin"` that names an `"Archive"`, the entry's `"Resource"` gives way to it.
- A declaration can give a `"Resource"` beside its `"Git"` source. The published release is installed when it has a compatible version and an archive for `$SystemID`. Otherwise the repository is checked out at its commit. A copy from the published release, which has no Git receipt, also meets the declaration.
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
