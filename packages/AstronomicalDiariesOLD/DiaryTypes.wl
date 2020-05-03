(* ::Package:: *)

BeginPackage["AstronomicalDiaries`DiaryTypes`",{
	"AstronomicalDiaries`Constants`"
}];


ClearAll["AstronomicalDiaries`DiaryTypes`*"];


DiaryDistance::usage = "Represents a distance on the sky measured in cubits (ku\[SHacek]) and fingers (\[SHacek]i or u)";
DiaryDisplacement::usage = "Represents a displacment between two astronomical objects.";
DiaryDate::usage = "Represents a date that can use both Babylonian and Julian calendars.";
DiaryEvent::usage = "Represents an event recorded in the Astronomical Diares.";


Begin["`Private`"];


(* ::Subsection:: *)
(*DiaryDistance*)


DiaryDistance::invalid = "`` is not a valid DiaryDistance.";


(* ::Subsubsection:: *)
(*Accessors*)


DiaryDistance[data_Association]["Data"] := data
DiaryDistance[data_Association]["Cubits"] := data["Cubits"]
DiaryDistance[data_Association]["Fingers"] := data["Fingers"]
DiaryDistance[data_Association]["TotalCubits"] :=
	If[MissingQ[data["Cubits"]]&&MissingQ[data["Fingers"]],Missing[],
		If[MissingQ[data["Cubits"]],0,data["Cubits"]]+If[MissingQ[data["Fingers"]],0,data["Fingers"]]/24
	]
DiaryDistance[data_Association]["Degrees"] :=
	With[{cubits = DiaryDistance[data]["TotalCubits"]},
		If[MissingQ[cubits],Missing[],2*cubits]
	]


(* ::Subsection:: *)
(*Constructors*)


DiaryDistance[{cubits_,fingers_}] := DiaryDistance[<|"Cubits"->cubits,"Fingers"->fingers|>]


(* ::Subsection:: *)
(*Verifiers*)


DiaryDistance[d:Except[KeyValuePattern[{
		"Cubits"->_Rational|_Integer|_Missing,
		"Fingers"->_Rational|_Integer|_Missing
	}]]] :=
		(Message[DiaryDistance::invalid, {d}]; Missing["InvalidDiaryDistance"])


(* ::Subsection:: *)
(*DiaryDisplacement*)


DiaryDisplacement::invalid = "`` is not a valid DiaryDisplacement.";


(* ::Subsubsection:: *)
(*Accessors*)


longitudeRelations = {"InFrontOf","Behind","East","West"};
latitudeRelations = {"Above","Below","North","South"};
negativeRelations = {"InFrontOf","East","North","Above"};


DiaryDisplacement[data_Association]["Data"] := data
DiaryDisplacement[data_Association]["Distances"] := data["Distances"]
DiaryDisplacement[data_Association]["Relations"] := data["Relations"]
DiaryDisplacement[data_Association]["EclipticDisplacement",agg_] :=
	Module[{distRelPairs},
		distRelPairs = Transpose[{data["Distances"],data["Relations"]}];
		{
			agg[If[MemberQ[negativeRelations,#[[2]]],-1,1]*#[[1]]["Degrees"]&/@
				Select[distRelPairs, MemberQ[longitudeRelations,#[[2]]]&]],
			agg[If[MemberQ[negativeRelations,#[[2]]],-1,1]*#[[1]]["Degrees"]&/@
				Select[distRelPairs, MemberQ[latitudeRelations,#[[2]]]&]]
		}
	]
(dd:DiaryDisplacement[data_Association])["EclipticDisplacement"] :=
	dd["EclipticDisplacement", If[Length[#]=!=1,Missing[],#[[1]]]&]


(* ::Subsubsection:: *)
(*Constructors*)


DiaryDisplacement[{{dist1_,rel1_},{dist2_,rel2_}}] :=
	DiaryDisplacement[<|
		"Distances"->{dist1,dist2},
		"Relations"->{rel1,rel2}
	|>]


(* ::Subsubsection:: *)
(*Verifiers*)


DiaryDisplacement[d:Except[KeyValuePattern[{
		"Distances"->{_DiaryDistance, _DiaryDistance},
		"Relations"->{
			"InFrontOf"|"Behind"|"East"|"West"|"Above"|"Below"|"North"|"South"|_Missing,
			"InFrontOf"|"Behind"|"East"|"West"|"Above"|"Below"|"North"|"South"|_Missing}
	}]]] :=
		(Message[DiaryDisplacement::invalid, {d}]; Missing["InvalidDiaryDisplacement"])


(* ::Subsection:: *)
(*DiaryDate*)


DiaryDate::invalid = "`` is not a valid DiaryDate.";


(* ::Subsubsection:: *)
(*Accessors*)


DiaryDate[data_Association]["Data"] := data
DiaryDate[data_Association]["JulianDate"] := 
	If[MissingQ[data["JulianYear"]]||MissingQ[data["JulianMonth"]]||MissingQ[data["JulianDay"]],
		Missing[],
		DateObject[
			{data["JulianYear"]-Boole[Negative[data["JulianYear"]]],data["JulianMonth"],data["JulianDay"]},
			"Day",
			CalendarType->"Julian",
			TimeZone->0
		]
	]
DiaryDate[data_Association]["BabylonianMonth"] := data["BabylonianMonth"]
DiaryDate[data_Association]["BabylonianDay"] := data["BabylonianDay"]
DiaryDate[data_Association]["Time"] := data["Time"]


(* ::Subsubsection:: *)
(*Constructors*)


(* ::Subsubsection:: *)
(*Verifiers*)


dateTimes = {
	"BeginningOfTheNight",
	"FirstPartOfTheNight",
	"MiddlePartOfTheNight",
	"LastPartOfTheNight",
	"Morning",
	"Noon",
	"Afternoon",
	"Sunset"
};


DiaryDate[d:Except[KeyValuePattern[{
		"JulianYear"->_Integer|_Missing,
		"JulianMonth"->_Integer|_Missing,
		"JulianDay"->_Integer|_Missing,
		"BabylonianMonth"->_Integer|_Missing,
		"BabylonianDay"->_Integer|_Missing,
		"Time"->(Alternatives@@dateTimes)|_Missing
	}]]] :=
		(Message[DiaryDate::invalid, {d}]; Missing["InvalidDiaryDate"])


(* ::Subsection:: *)
(*DiaryEvent*)


DiaryEvent::invalid = "`` is not a valid DiaryEvent.";


(* ::Subsubsection:: *)
(*Accessors*)


DiaryEvent[data_Association]["Data"] := data
DiaryEvent[data_Association]["Type"] := data["Type"]
DiaryEvent[data_Association]["Provenance"] := data["Provenance"]
DiaryEvent[data_Association]["Content"] := data["Content"]


(* ::Subsubsection:: *)
(*Constructors*)


(* ::Subsubsection:: *)
(*Verifiers*)


DiaryEvent[d:Except[KeyValuePattern[{
		"Type"->_String,
		"Provenance"->_,
		"Content"->_
	}]]] :=
		(Message[DiaryEvent::invalid, {d}]; Missing["InvalidDiaryEvent"])


(* ::Subsection:: *)
(*Display code*)


DiaryDistance /:
	MakeBoxes[dist:DiaryDistance[_Association],StandardForm] :=
		BoxForm`ArrangeSummaryBox[
			DiaryDistance,
			dist,
			Graphics[{
					{Dashing[0.1],Orange,Line[{{-0.5,0},{0.5,0}}]},
					Line[{{-0.5,-0.1},{-0.5,0.1}}],Line[{{0.5,-0.1},{0.5,0.1}}]},
				ImageSize->30,
				Background->Transparent
			],
			{{"cubits: ",dist["Cubits"]},{"fingers: ",dist["Fingers"]}},
			{},
			StandardForm
		]


DiaryDisplacement /:
	MakeBoxes[dis:DiaryDisplacement[_Association],StandardForm] :=
		BoxForm`ArrangeSummaryBox[
			DiaryDisplacement,
			dist,
			Graphics[{
					{Orange,Dashing[0.1],Line[{{-0.5,-0.5},{0.5,0.5}}]},
					Circle[],Red,Point[{-0.5,-0.5}],Blue,Point[{0.5,0.5}]},
				ImageSize->30,
				Background->Transparent
			],
			{
				{"distances: ",
					Row[{If[MissingQ[#],Missing[],#["TotalCubits"]]&/@dis["Distances"]," cubits"}]},
				{"relations: ",dis["Relations"]}
			},
			{},
			StandardForm
		]


DiaryDate /:
	MakeBoxes[date:DiaryDate[_Association],StandardForm] :=
		BoxForm`ArrangeSummaryBox[
			DiaryDate,
			date,
			Style["\|012000",24],
			{
				{"Julian date: ",date["JulianDate"]},
				{"Babylonian month: ",date["BabylonianMonth"]},
				{"Babylonian day: ",date["BabylonianDay"]},
				{"Time: ",date["Time"]}
			},
			{},
			StandardForm
		]


(* ::Subsection:: *)
(*End*)


End[];


EndPackage[];
