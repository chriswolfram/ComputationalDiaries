(* ::Package:: *)

BeginPackage["ComputationalDiaries`DiaryObservation`",{
	"ComputationalDiaries`DiaryTypes`"
}];


DiaryObservation::usage = "Represents an observation in the Astronomical Diaries.";


Begin["`Private`"];


(* ::Subsection:: *)
(*DiaryObservation*)


DiaryObservation[data_]["Data"] := data
DiaryObservation[data_]["Type"] := data["Type"]
DiaryObservation[data_]["Contents"] := data["Contents"]
DiaryObservation[data_]["Date"] := data["Date"]
DiaryObservation[data_]["Provenance"] := data["Provenance"]
DiaryObservation[data_][other_] := data["Content"][other]


(* ::Subsection:: *)
(*End*)


End[];
EndPackage[];
