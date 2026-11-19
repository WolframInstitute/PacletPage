---
Template: TechNote
Name: DependencyManagement
Title: Dependency Management
Context: WolframInstitute`PacletPage`
Paclet: WolframInstitute/PacletPage
URI: WolframInstitute/PacletPage/tutorial/DependencyManagement
Keywords: [dependencies, optional libraries, paclet metadata, installation, PureMath, setup]
RelatedGuides: [PacletDependencyManagement, PacletPage]
---

## Declare an optional computation

Suppose `Demo/Consumer` offers proofs through TypeTheory. Its ordinary computations can run before the user installs the proof engine. Put the following declaration in the consumer's `PacletInfo.wl`. The standard `"Dependencies"` list names PacletPage so that the consumer can call its setup functions. The separate `"OptionalDependencies"` list describes the proof engine.

```wl
consumer = PacletObject[<|
    "Name" -> "Demo/Consumer",
    "Version" -> "1.0.0",
    "PrimaryContext" -> "Demo`Consumer`",
    "Extensions" -> {{"Kernel", "Root" -> "Kernel", "Context" -> "Demo`Consumer`"}},
    "Dependencies" -> {"WolframInstitute/PacletPage"},
    "OptionalDependencies" -> {
        <|"Name" -> "WolframInstitute/TypeTheory",
          "Description" -> "Type checking for the Calculus of Inductive Constructions.",
          "Resource" -> "https://www.wolframcloud.com/obj/wolframinstitute/DeployedResources/Paclet/WolframInstitute/TypeTheory",
          "License" -> "MIT",
          "URL" -> "https://github.com/WolframInstitute/TypeTheory#loading",
          "Check" -> "Demo`Consumer`TypeTheoryReadyQ"|>
    }
|>];
```

Save the `PacletObject[…]` expression as the contents of the file. The `consumer` binding here also lets you inspect the declaration before registering the paclet.

```wl
Keys[PacletDependencyInformation[consumer]]
```

<!-- => {"WolframInstitute/PacletPage", "WolframInstitute/TypeTheory"} -->

A bare dependency name uses the configured paclet sites. The other sources are a public Wolfram Cloud paclet `"Resource"` or a local `"Archive"`, such as `"Resources/Algebra.paclet"`, relative to the consumer directory. Choose one source per library. Cloud resources may select their archive by system. Each dependency appears once across both lists.

| Declaration | Meaning |
|---|---|
| `"Name"` | Exact paclet name, including its publisher when applicable |
| `"Version"` | A numeric version such as `"1.2.0"`, a minimum such as `"1.2+"`, a version family such as `"1.2.*"`, or `"*"` for any version |
| `"Platforms"` | List of supported system IDs. Omitting the field allows every platform |
| `"Description"`, `"License"` | What the library provides and its license or account requirements |
| `"DownloadSize"` | Display text or an association of system IDs to display text |
| `"URL"`, `"SetupLinks"` | Setup instructions and an optional association of additional link labels to HTTPS URLs |
| `"Check"` | Fully qualified name of a zero-argument readiness function in the consumer's kernel context |

## Check that a computation works

Put the check in the consumer's kernel package. This one checks whether the installed engine can assign a type to a natural number. Keeping a library's context local to the check lets a notebook continue to resolve unqualified symbols as before.

```wl
#| eval: false
Demo`Consumer`TypeTheoryReadyQ[] := Block[{$ContextPath = $ContextPath},
    Needs["WolframInstitute`TypeTheory`"] === Null &&
        With[{term = Symbol["WolframInstitute`TypeTheory`TypeTheoryTerm"],
              type = Symbol["WolframInstitute`TypeTheory`TypeTheoryType"]},
            MatchQ[type[term[2 + 3]], HoldPattern[term[NonNegativeIntegers, _]]]]
]
```

Register the consumer directory with `PacletDirectoryLoad` during development. Installed consumers can be addressed directly by name. Call the check explicitly or let installation run it. A failed check produces **Needs setup**. A missing toolchain, license or system executable can require a separate setup step even after installation succeeds.

```wl
#| eval: false
PacletDependencyInformation["Demo/Consumer", "WolframInstitute/TypeTheory", "CheckReadiness" -> True]
```

## Install and update

From a desktop notebook, open the consumer's dialog. It reads local state first. Checking for updates reads release metadata. The dialog shows progress during a cloud archive transfer and lets the user cancel it.

```wl
#| eval: false
PacletDependencyDialog["Demo/Consumer"]
```

A script can apply the same operations to one name, a list of names or `All`. Each selected name has its own result. The following cells change the current user's installation when evaluated.

```wl
#| eval: false
InstallPacletDependencies["Demo/Consumer", "WolframInstitute/TypeTheory"]
```

```wl
#| eval: false
UpdatePacletDependencies["Demo/Consumer", "WolframInstitute/TypeTheory"]
```

```wl
#| eval: false
UninstallPacletDependencies["Demo/Consumer", "WolframInstitute/TypeTheory"]
```

An update retains an equal or newer compatible version. Restart the kernel when a loaded library changes. Uninstalling removes user-installed copies, which other consumers may also use.

## Connect an existing consumer

PureMath already has readiness checks for GAPLink, LeanLink and TypeTheory. A migration can keep those checks next to the computations that use them. Copy each library's name, public resource, supported platforms, download sizes, license and setup links into an optional dependency record. Expose a zero-argument check in the consumer context for each library. For example, a PureMath wrapper for its existing Lean probe can call ``WolframInstitute`PureMath`PackageScope`leanLinkReadyQ[]``.

Once that manifest is present, the consumer's setup function needs only the shared dialog.

```wl
#| eval: false
PureMathSetup[] := WolframInstitute`PacletPage`PacletDependencyDialog["WolframInstitute/PureMath"]
```

A consumer that wants a desktop reminder can call `PacletDependencyDialog[owner, "Notify"]` at startup. Dismissing the dialog records the preference for that consumer. Batch kernels can load the consumer and run ordinary computations independently of desktop setup.
