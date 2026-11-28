(* ::Package:: *)

PacletObject[<|
    "Name" -> "WolframInstitute/PacletPage",
    "Description" -> "Build and publish a paclet's documentation, from markdown to a published paclet with its documentation site",
    "Creator" -> "Nikolay Murzin",
    "PublisherID" -> "WolframInstitute",
    "License" -> "MIT",
    "Version" -> "0.3.106",
    "WolframVersion" -> "14.3+",
    "PrimaryContext" -> "WolframInstitute`PacletPage`",
    "Dependencies" -> {
        <|
            "Name" -> "WolframInstitute/GitLink",
            "Optional" -> True,
            "Version" -> "2.0+",
            "Resource" -> "https://www.wolframcloud.com/obj/wolframinstitute/DeployedResources/Paclet/WolframInstitute/GitLink",
            "Platforms" -> {"MacOSX-ARM64", "Linux-x86-64"},
            "Description" -> "Native libgit2 1.9 bindings for shallow Git fetches and sparse path checkouts",
            "Pin" -> <|"Git" -> "https://github.com/WolframInstitute/GitLink", "Commit" -> "7482aef620549b61c3c1408b07908cfd6e2f5eb2", "Path" -> "GitLink"|>
        |>,
        <|
            "Name" -> "Wolfram/Parser",
            "Version" -> "1.9.13+",
            "Resource" -> "https://www.wolframcloud.com/obj/wolframinstitute/DeployedResources/Paclet/Wolfram/Parser",
            "Description" -> "LaTeX math parsing for the documentation converter",
            "Pin" -> <|"Git" -> "https://github.com/sw1sh/WolframParser", "Commit" -> "5e05149ead1c292ebd39d02eee85f1c0b8059f55", "Path" -> "Parser"|>
        |>
    },
    "Extensions" -> {
        {
            "Kernel",
            "Root" -> "Kernel",
            "Context" -> "WolframInstitute`PacletPage`",
            "Symbols" -> {
                "WolframInstitute`PacletPage`PacletPageCatalog",
                "WolframInstitute`PacletPage`PacletPage",
                "WolframInstitute`PacletPage`PacletPageFiles",
                "WolframInstitute`PacletPage`PacletPageWebAssets",
                "WolframInstitute`PacletPage`BuildPacletPageSite",
                "WolframInstitute`PacletPage`DeployPacletPage",
                "WolframInstitute`PacletPage`DeployPacletPageFeedback",
                "WolframInstitute`PacletPage`PacletPageFeedback",
                "WolframInstitute`PacletPage`SubmitPacletPageFeedback",
                "WolframInstitute`PacletPage`SetPacletPageFeedbackStatus",
                "WolframInstitute`PacletPage`SetPacletPagePermissions",
                "WolframInstitute`PacletPage`CheckPacletPage",
                "WolframInstitute`PacletPage`BuildPacletDocumentation",
                "WolframInstitute`PacletPage`PacletDefinitionScan",
                "WolframInstitute`PacletPage`PacletPageDependencies",
                "WolframInstitute`PacletPage`PacletDependencyInformation",
                "WolframInstitute`PacletPage`PacletDependencyReport",
                "WolframInstitute`PacletPage`InstallPacletDependencies",
                "WolframInstitute`PacletPage`UpdatePacletDependencies",
                "WolframInstitute`PacletPage`UninstallPacletDependencies",
                "WolframInstitute`PacletPage`PacletDependencyDialog",
                "WolframInstitute`PacletPage`DeployPacletResource",
                "WolframInstitute`PacletPage`SubmitPacletResource",
                "WolframInstitute`PacletPage`PublishPacletPage",
                "WolframInstitute`PacletPage`UnpublishPacletPage"
            }
        },
        {
            "Asset",
            "Assets" -> {{"Web", "Assets/Web"}, {"Example", "Assets/Example"}, {"Hero", "Assets/Hero.wxf"},
                {"Converter", "Assets/Converter.wxf"}, {"DependencyLoader", "Assets/LoadDependencyManager.wl"},
                {"RequiredDependencyLoader", "Assets/LoadRequiredDependencies.wl"}}
        },
        {
            "Documentation",
            "Language" -> "English"
        }
    }
|>]
