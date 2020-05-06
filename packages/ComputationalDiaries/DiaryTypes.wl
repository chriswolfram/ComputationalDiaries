(* ::Package:: *)

BeginPackage["ComputationalDiaries`DiaryTypes`",{
	"ComputationalDiaries`Constants`"
}];


ClearAll["ComputationalDiaries`DiaryTypes`*"];


DiaryDate::usage = "Represents a date in a combination of the Julian and Babylonian calendars.";
DiaryDistance::usage = "Represents a distance in the sky in cubits (K\[CapitalUGrave]\[CapitalSHacek]) and fingers (\[CapitalSHacek]I or U).";
DiaryDisplacement::usage = "Represents the displacement between two objects in the sky.";
DiaryDuration::usage = "Represents a duration in Babylonian units.";
DiaryCapacity::usage =
	"Represents an amount of barely, dates, mustard, cress, or sesame in MarketRates observations.";


Begin["`Private`"];


(* ::Subsection:: *)
(*DiaryDate*)


DiaryDate::invalid = "`` is not a valid DiaryDate.";


(* ::Subsubsection:: *)
(*Constructors*)


DiaryDate[{julYear_,babMonth_,babDay_}] :=
	DiaryDate[]


(* ::Subsubsection:: *)
(*Accessors*)


(* ::Text:: *)
(*TODO: Look at time conversions. Right now we do approximately midnight in the morning (at the start of the day by the Julian calendar).*)


DiaryDate[data_]["Data"] := data
DiaryDate[data_]["BabylonianYear"] := data["BabylonianYear"]
DiaryDate[data_]["BabylonianMonth"] := data["BabylonianMonth"]
DiaryDate[data_]["BabylonianDay"] := data["BabylonianDay"]
DiaryDate[data_]["JulianYear"] := data["JulianYear"]
DiaryDate[data_]["JulianMonth"] := data["JulianMonth"]
DiaryDate[data_]["JulianDay"] := data["JulianDay"]
DiaryDate[data_]["Time"] := data["Time"]
DiaryDate[data_]["JulianDate"] :=
	If[MissingQ[data["JulianYear"]]||MissingQ[data["JulianMonth"]]||MissingQ[data["JulianDay"]],
		Missing[],
		DateObject[{
				data["JulianYear"]-Boole[Negative[data["JulianYear"]]],
				data["JulianMonth"],
				data["JulianDay"],
				12,0,1
			},
			"Instant",
			CalendarType->"Julian",
			TimeZone->3(*time zone of Babylon*)
		]
	]


(* ::Subsubsection:: *)
(*Verifiers*)


months = {"I","II","III","IV","V","VI","VII","VIII","IX","X","XI","XII","VI2","XII2"};
times = {"BeginningOfTheNight","FirstPartOfTheNight","MiddlePartOfTheNight",
		"LastPartOfTheNight","Morning","Noon","Afternoon","Sunset"};


dd:DiaryDate[Except[KeyValuePattern[{
		"BabylonianYear"->_Integer|{_String,_Integer}|_Missing,
		"BabylonianMonth"->(Alternatives@@months)|_Missing,
		"BabylonianDay"->_Integer|_Missing,
		"JulianYear"->_Integer|_Missing,
		"JulianMonth"->_Integer|_Missing,
		"JulianDay"->_Integer|_Missing,
		"Time"->(Alternatives@@times)|_Missing
	}]]] :=
	(Message[DiaryDate::invalid, Hold[dd]]; Missing["InvalidDiaryDate"])


(* ::Subsection:: *)
(*DiaryDistance*)


DiaryDistance::invalid = "`` is not a valid DiaryDistance.";


(* ::Subsubsection:: *)
(*Constructors*)


(* ::Subsubsection:: *)
(*Accessors*)


(* ::Text:: *)
(*TODO: add support for non-numerical distances like "ALittle", etc.*)


DiaryDistance[data_]["Data"] := data
DiaryDistance[{cubits_,fingers_}]["Cubits"] := cubits
DiaryDistance[{cubits_,fingers_}]["Fingers"] := fingers
DiaryDistance[{cubits_,fingers_}]["TotalCubits"] :=
	If[MissingQ[cubits] && MissingQ[fingers],
		Missing[],
		If[MissingQ[cubits],0,cubits] + If[MissingQ[fingers],0,fingers]/24
	]
dd_DiaryDistance["Degrees"] :=
	With[{totalCubits=dd["TotalCubits"]}, If[MissingQ[totalCubits],Missing[],2*totalCubits]]


DiaryDistance["ALittle"]["Cubits"] := Missing[]
DiaryDistance["ALittle"]["Fingers"] := Missing[]
DiaryDistance["ALittle"]["TotalCubits"] := Missing[]


(* ::Subsubsection:: *)
(*Verifiers*)


dd:DiaryDistance[Except[
		{_Rational|_Integer|_Missing,_Rational|_Integer|_Missing}|
		"ALittle"]] :=
	(Message[DiaryDistance::invalid, Hold[dd]]; Missing["InvalidDiaryDistance"])


(* ::Subsection:: *)
(*DiaryDisplacement*)


DiaryDisplacement::invalid = "`` is not a valid DiaryDisplacement.";


(* ::Subsubsection:: *)
(*Constructors*)


DiaryDisplacement[{dlong_,rlong_},{dlat_,rlat_}] :=
	DiaryDisplacement[<|
		"Distances"->{dlong,dlat},
		"Relations"->{rlong,rlat}
	|>]


(* ::Subsubsection:: *)
(*Accessors*)


negativeRelations = {"Below","South","InFrontOf","West"};


DiaryDisplacement[data_]["Data"] := data
DiaryDisplacement[data_]["Distances"] := data["Distances"]
DiaryDisplacement[data_]["Relations"] := data["Relations"]
DiaryDisplacement[data_]["Degrees"] := 
	MapThread[Function[{d,r},
		If[MissingQ[d]||MissingQ[r],
			If[MatchQ[d,Missing["Unmentioned"]]||MatchQ[d,Missing["Unmentioned"]],
				0,
				Missing[]
			],
			With[{deg = d["Degrees"]},
				If[MissingQ[deg],
					Missing[],
					deg*If[MemberQ[negativeRelations,r],-1,1]
				]
			]
		]
	],{data["Distances"],data["Relations"]}]


(* ::Subsubsection:: *)
(*Verifiers*)


dd:DiaryDisplacement[Except[
		<|
			"Distances"->{_DiaryDistance|_Missing,_DiaryDistance|_Missing},
			"Relations"->{
				"InFrontOf"|"Behind"|"East"|"West"|_Missing,
				"Above"|"Below"|"North"|"South"|_Missing}
		|> |
		{{_,_},{_,_}} (*Constructor*)
	]] :=
	(Message[DiaryDisplacement::invalid, Hold[dd]]; Missing["InvalidDiaryDisplacement"])


(* ::Subsection:: *)
(*DiaryDuration*)


DiaryDuration::invalid = "`` is not a valid DiaryDuration.";


(* ::Subsubsection:: *)
(*Constructors*)


(* ::Subsubsection:: *)
(*Accessors*)


DiaryDuration[data_]["Data"] := data
DiaryDuration[{deg_,nin_}]["Degrees"] := deg
DiaryDuration[{deg_,nin_}]["ArcMinutes"] := nin
DiaryDuration[{deg_,nin_}]["Minutes"] :=
	If[MissingQ[deg]&&MissingQ[nin],
		Missing[],
		Quantity[4*(If[MissingQ[deg],0,deg]+If[MissingQ[nin],0,nin/60]),"Minutes"]
	]


(* ::Subsubsection:: *)
(*Verifiers*)


dd:DiaryDuration[Except[{_Rational|_Integer|_Missing,_Rational|_Integer|_Missing}]] :=
	(Message[DiaryDuration::invalid, Hold[dd]]; Missing["InvalidDiaryDuration"])


(* ::Subsection:: *)
(*DiaryCapacity*)


DiaryCapacity::invalid = "`` is not a valid DiaryCapacity.";


(* ::Subsubsection:: *)
(*Constructors*)


(* ::Subsubsection:: *)
(*Accessors*)


DiaryCapacity[data_]["Data"] := data
DiaryCapacity[{kur_,pan_,sut_,qa_}]["Kur"] := kur
DiaryCapacity[{kur_,pan_,sut_,qa_}]["Pan"] := pan
DiaryCapacity[{kur_,pan_,sut_,qa_}]["Sut"] := sut
DiaryCapacity[{kur_,pan_,sut_,qa_}]["Qa"] := qa
DiaryCapacity[q:{kur_,pan_,sut_,qa_}]["TotalQa"] :=
	If[AllTrue[q,MissingQ],
		Missing[],
		Replace[{kur,pan,sut,qa},_Missing->0,{1}].{180,36,6,1}
	]


(* ::Subsubsection:: *)
(*Verifiers*)


dc:DiaryCapacity[Except[{Repeated[_Rational|_Integer|_Missing,{4}]}]] :=
	(Message[DiaryCapacity::invalid, Hold[dc]]; Missing["InvalidDiaryCapacity"])


(* ::Subsection:: *)
(*Summary boxes*)


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


DiaryDistance /:
	MakeBoxes[dist:DiaryDistance[{cubits_,fingers_}],StandardForm] :=
		BoxForm`ArrangeSummaryBox[
			DiaryDistance,
			dist,
			Graphics[{
					{Dashing[0.1],Orange,Line[{{-0.5,0},{0.5,0}}]},
					Line[{{-0.5,-0.1},{-0.5,0.1}}],Line[{{0.5,-0.1},{0.5,0.1}}]},
				ImageSize->30,
				Background->Transparent
			],
			{{"cubits: ",cubits},{"fingers: ",fingers}},
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


DiaryDuration /:
	MakeBoxes[dur:DiaryDuration[{deg_,nin_}],StandardForm] :=
		BoxForm`ArrangeSummaryBox[
			DiaryDuration,
			dur,
			Graphics[{
					Circle[],Orange,EdgeForm[Black],Disk[{0,0},1,{Pi/2,0.25}]},
				ImageSize->30,
				Background->Transparent
			],
			{{"degrees: ",deg},{"NINDA: ",nin}},
			{},
			StandardForm
		]


DiaryCapacity /:
	MakeBoxes[cap:DiaryCapacity[{kur_,pan_,sut_,qa_}],StandardForm] :=
		BoxForm`ArrangeSummaryBox[
			DiaryCapacity,
			cap,
			Graphics[{
					Orange,Rectangle[{0,0},{1,0.7}],
					Black,Line[{{0,1},{0,0},{1,0},{1,1}}],Line[{{0,0.7},{1,0.7}}]},
				ImageSize->30,
				Background->Transparent
			],
			{{"kur: ",kur},{"p\[ABar]n: ",pan},{"s\:016bt: ",sut},{"qa: ",qa}},
			{},
			StandardForm
		]


(* ::Subsection:: *)
(*End*)


End[];
EndPackage[];
