(* ::Package:: *)

BeginPackage["ComputationalDiaries`ReferenceText`", {"ComputationalDiaries`"}];


Begin["`Private`"];


(* ::Subsection:: *)
(*Implementation*)


(*MX files generated in astronomicalDiariesScraping-02.nb*)


tabletTexts = Import[FileNameJoin[{DirectoryName[$InputFileName],"tabletTexts.mx"}]];


DiaryTabletData[] := AssociationThread[Keys[tabletTexts], <||>] (*placeholder*)


DiaryTabletText::unknownTablet = "`` is not a valid tablet ID.";


DiaryTabletText[] := tabletTexts
DiaryTabletText[tabletID_] :=
	Lookup[tabletTexts, tabletID, Message[DiaryTabletText::unknownTablet,tabletID];$Failed]


DiaryTabletTable[tabletID_] := With[{lines = DiaryTabletText[tabletID]},
	If[lines === $Failed, $Failed,
		Grid[lines/.Delimiter->{},Alignment->Left,Frame->All]
	]
]


(* ::Subsection:: *)
(*End*)


End[];
EndPackage[];
