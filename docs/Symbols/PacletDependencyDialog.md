---
Template: Symbol
Name: PacletDependencyDialog
Context: WolframInstitute`PacletPage`
Paclet: WolframInstitute/PacletPage
URI: WolframInstitute/PacletPage/ref/PacletDependencyDialog
Keywords: [notebook, setup, desktop, optional libraries]
SeeAlso: [PacletDependencyInformation, InstallPacletDependencies, UpdatePacletDependencies, UninstallPacletDependencies]
RelatedGuides: [PacletDependencyManagement]
RelatedTutorials: [DependencyManagement]
---

## Usage

<code>[PacletDependencyDialog]()[*owner*]</code> opens the desktop notebook dialog for the dependencies of *owner*.

<code>[PacletDependencyDialog]()[*owner*, "Notify"]</code> issues a desktop setup message when optional dependencies are missing and the user has not dismissed setup for this paclet.

## Details & Options

- The dialog opens with local versions and statuses. Choose **Check readiness** to run consumer checks or **Check for updates** to look up releases. Select **Install** or **Update** for a library. Installed optional libraries also offer **Uninstall**, except for PacletPage itself.
- Each row shows the consumer's description or, when available, the description from the installed library. Authors can declare a description so users can read it before installation. Rows can also include the download size, license and setup links.
- Use the copy control to take the corresponding install, update or uninstall command to another notebook.
- An operation keeps the dialog open. A transfer shows progress and offers cancellation. Opening setup again selects the existing dialog for that consumer.
- The `"Notify"` form is suitable for a consumer’s optional desktop startup hook. Dismissing setup records a local preference for that consumer. Scripts may use the other dependency functions directly.

## Basic Examples

Open the dependency dialog for the consumer from the Dependency Management tutorial in a desktop notebook.

```wl
#| eval: false
PacletDependencyDialog["Demo/Consumer"]
```
