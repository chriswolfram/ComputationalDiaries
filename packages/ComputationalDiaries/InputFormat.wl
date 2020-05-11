(* ::Package:: *)

BeginPackage["ComputationalDiaries`InputFormat`",{
	"ComputationalDiaries`DiaryTypes`",
	"ComputationalDiaries`DiaryObservation`"
}];


processInputFormat::usage = "Canonicalizes the input format.";
launchCurationPalette::usage = "Opens a palette with useful templates for Diary curation.";
inputMonthDay::usage =
	"inputMonthDay[\!\(\*
StyleBox[\"day\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"time\",\nFontSlant->\"Italic\"]\)] represents a day and time of the month for the input format.";


Begin["`Private`"];


diaryDateAddDay[date_DiaryDate, days_Integer, time_] :=
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
diaryDateAddDay[date_DiaryDate, days_Missing, time_] := days
diaryDateAddDay[date_Missing, days_, time_] := date


processInputFormat[tablets_] :=
	Flatten[Function[tab, Module[{tabletID,creator},
		tabletID = tab["TabletID"];
		creator = tab["Creator"];
		Function[month, With[{dayZero = month["DayZero"]},
			Function[observation,
				Identity//@If[MatchQ[observation,_DiaryObservation],
					observation,
					DiaryObservation[Association[Normal[<|
						"Type"->observation["Type"],
						"Content"->observation["Content"],
						"Date"->observation["Date"],
						"Inferred"->observation["Inferred"],
						"UUID"->observation["UUID"],
						"Provenance"-><|
							"Creator"->creator,
							"TabletID"->tabletID,
							"LineNumber"->observation["LineNumber"],
							"Notes"->observation["Notes"]
						|>
					|>]/.inputMonthDay[day_,time_]:>diaryDateAddDay[dayZero,day,time]]]
				]
			]/@month["Observations"]
		]]/@tab["Months"]
	]]/@tablets,2]


makePalette[l_List] := makePalette/@l
makePalette[Cell[CellGroupData[{Cell[title_,"Subsection",___],rest___},___]]] :=
	OpenerView[{title,Column[makePalette/@{rest}]},True]
makePalette[Cell[CellGroupData[{Cell[title_,"Subsubsection",___],Cell[BoxData[boxes_],___]},___]]] :=
	Button[
		title,
		Paste[RawBoxes[boxes]/.
			RowBox[{"CreateUUID", "[", "]"}]:>StringTemplate["\"``\""][CreateUUID[]]],
		Appearance->"Palette"
	]


referenceNB = Import[
		FileNameJoin[{DirectoryName[$InputFileName],"curationPaletteReference.nb"}],
	"Notebook"];


launchCurationPalette[] :=
	CreatePalette[makePalette[First[referenceNB]],WindowTitle->"Curation"]


(* ::Subsection:: *)
(*End*)


End[];


EndPackage[];
