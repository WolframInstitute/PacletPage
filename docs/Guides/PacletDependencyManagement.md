---
Template: Guide
Name: PacletDependencyManagement
Title: Paclet Dependency Management
Context: WolframInstitute`PacletPage`
Paclet: WolframInstitute/PacletPage
URI: WolframInstitute/PacletPage/guide/PacletDependencyManagement
Description: Inspect and manage the required and optional libraries declared by a paclet.
Keywords: [dependencies, optional libraries, installation, readiness, setup]
RelatedGuides: [PacletPage]
RelatedTutorials: [DependencyManagement]
---

## Abstract

A paclet can offer additional computations through optional libraries. Its `PacletInfo.wl` declares the libraries and their sources. Users can inspect local versions, test a runtime, install compatible releases and manage them in a notebook or a script.

## Functions

### Discovering libraries

- `PacletDependencyInformation` read declarations, versions and readiness for one consumer
- `PacletDependencyReport` expand a snapshot in a notebook or extract its records
- `PacletDependencyDialog` review and manage its libraries in a desktop notebook

### Managing installations

- `InstallPacletDependencies` install selected libraries and run their checks
- `UpdatePacletDependencies` update installed libraries to newer compatible versions
- `UninstallPacletDependencies` remove user-installed copies of selected libraries
