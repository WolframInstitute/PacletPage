---
Template: Symbol
Name: PacletDependencyReport
Context: WolframInstitute`PacletPage`
Paclet: WolframInstitute/PacletPage
URI: WolframInstitute/PacletPage/ref/PacletDependencyReport
Keywords: [dependencies, report, status, notebook]
SeeAlso: [PacletDependencyInformation, PacletDependencyDialog]
RelatedGuides: [PacletDependencyManagement]
RelatedTutorials: [DependencyManagement]
---

## Usage

<code>[PacletDependencyReport]()[*owner*, *records*]</code> represents the dependency snapshot returned by [PacletDependencyInformation]().

## Details & Options

- A report for a single library shows its name, status and installed version. An update check also supplies the available version. Click the plus sign to see details.
- A report for several libraries shows the owner and count. Expand it to compare the libraries. Opening a report displays the snapshot captured by the original call.
- Use `report[name]` for one library, `Keys[report]` for the names, `Length[report]` for the count, or `Normal[report]` for the entire association.

## Basic Examples

Read a single entry from a report.

```wl
report = PacletDependencyInformation[PacletObject[<|"Name" -> "Demo/Example", "Version" -> "1.0", "Dependencies" -> {"PacletTools"}|>]];
report["PacletTools"]["Status"]
```
