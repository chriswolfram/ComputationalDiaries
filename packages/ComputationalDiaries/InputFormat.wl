(* ::Package:: *)

BeginPackage["ComputationalDiaries`InputFormat`",{
	"ComputationalDiaries`DiaryTypes`",
	"ComputationalDiaries`DiaryObservation`"
}];


ClearAll["ComputationalDiaries`InputFormat`*"];


ProcessInputFormat::usage = "Canonicalizes the input format.";
LaunchCurationPalette::usage = "Opens a palette with useful templates for Diary curation.";


Begin["`Private`"];


diaryDateAdd[date_DiaryDate, days_, time_] :=
	Module[{newJulianDate},
		newJulianDate = date["JulianDate"]+Quantity[days,"Days"];
		DiaryDate[<|
			"BabylonianYear"->date["BabylonianYear"],
			"BabylonianMonth"->date["BabylonianMonth"],
			"BabylonianDay"->date["BabylonianDay"]+days,
			(*Years are transformed to make them AD (so it goes -1,0,1 instead of -1,1)*)
			"JulianYear"->
				DateValue[newJulianDate,"Year"]+Boole[Negative[DateValue[newJulianDate,"Year"]]],
			"JulianMonth"->DateValue[newJulianDate,"Month"],
			"JulianDay"->DateValue[newJulianDate,"Day"],
			"Time"->time
		|>]
	]


ProcessInputFormat[tablets_] :=
	Function[tab, Module[{tabletID,creator},
		tabletID = tab[[1,"TabletID"]];
		creator = tab[[1,"Creator"]];
		Function[month, Module[{dayZero},
			dayZero = month[[1]];
			Function[observation,
				If[MatchQ[observation,_DiaryObservation],
					observation,
					DiaryObservation[<|
						"Type"->observation["Type"],
						"Content"->observation["Content"],
						"Date"->diaryDayAdd[dayZero,observation["Date"][[1]],observation["Date"][[2]]],
						"Provenance"-><|
							"Creator"->creator,
							"TabletID"->tabletID,
							"LineNumber"->observation["LineNumber"],
							"Notes"->Lookup[observation,"Notes",Missing[]]
						|>
					|>]
				]
			]/@month[[2]]
		]]/@tab[[2]]
	]]/@tablets


makePalette[l_List] := makePalette/@l
makePalette[Cell[CellGroupData[{Cell[title_,"Subsection",___],rest___},___]]] :=
	OpenerView[{title,Column[makePalette/@{rest}]},True]
makePalette[Cell[CellGroupData[{Cell[title_,"Subsubsection",___],Cell[BoxData[boxes_],___]},___]]] :=
	PasteButton[title,RawBoxes[boxes]]


referenceNB = Import[
		FileNameJoin[{DirectoryName[$InputFileName],"curationPaletteReference.nb"}],
	"Notebook"];


LaunchCurationPalette[] :=
	CreatePalette[makePalette[First[referenceNB]],WindowTitle->"Curation"]


(* ::Subsection:: *)
(*End*)


End[];


EndPackage[];
