(* ::Package:: *)

BeginPackage["Demo`Greetings`"]

Greet::usage = "Greet[name] greets name.";

Begin["`Private`"]

Greet[name_String] := "Hello, " <> name <> "!"

End[]

EndPackage[]
