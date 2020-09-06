(* ::Package:: *)

BeginPackage["ComputationalDiaries`DiaryTypes`", {"ComputationalDiaries`"}];


Begin["`Private`"];


(* ::Subsection:: *)
(*DiaryDate*)


DiaryDate::invalid = "`` is not a valid DiaryDate.";


(* ::Subsubsection:: *)
(*Constructors*)


(*DiaryDate[{julYear_,babMonth_,babDay_}] :=
	DiaryDate[]*)


(* ::Subsubsection:: *)
(*Accessors*)


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
		"LastPartOfTheNight","Morning","Noon","Afternoon","Sunset","Day","Night"};


dd:DiaryDate[Except[KeyValuePattern[{
		"BabylonianYear"->_Integer|{_String,_Integer}|_Missing,
		"BabylonianMonth"->(Alternatives@@months)|_Missing,
		"BabylonianDay"->_Integer|_Missing,
		"JulianYear"->_Integer|_Missing,
		"JulianMonth"->_Integer|_Missing,
		"JulianDay"->_Integer|_Missing,
		"Time"->(Alternatives@@times)|_Missing
	}]]] :=
	(Message[DiaryDate::invalid, HoldForm[dd]]; Missing["InvalidDiaryDate"])


(* ::Subsection:: *)
(*DiaryDistance*)


DiaryDistance::invalid = "`` is not a valid DiaryDistance.";


(* ::Subsubsection:: *)
(*Constructors*)


(* ::Subsubsection:: *)
(*Accessors*)


DiaryDistance[data_]["Data"] := data
DiaryDistance[{cubits_,fingers_}]["Cubits"] := cubits
DiaryDistance[{cubits_,fingers_}]["Fingers"] := fingers
DiaryDistance[{cubits_,fingers_}]["TotalCubits"] :=
	Module[{cubitsM,fingersM},
		{cubitsM,fingersM} = {cubits,fingers}/.Missing["Unmentioned"]->0;
		Enclose[
			Confirm[cubitsM] + Confirm[fingersM]*1/24,
			DiaryMergeMissing[{cubitsM,fingersM}]&
		]
	]
dd_DiaryDistance["IdealDegrees"] := Enclose[2*Confirm[dd["TotalCubits"]], "Information"]
dd_DiaryDistance["RealDegrees"] := Enclose[2.27*Confirm[dd["TotalCubits"]], "Information"]


DiaryDistance["ALittle"]["Cubits"] := Missing[]
DiaryDistance["ALittle"]["Fingers"] := Missing[]
DiaryDistance["ALittle"]["TotalCubits"] := Missing[]


(* ::Subsubsection:: *)
(*Verifiers*)


dd:DiaryDistance[Except[
		{_Rational|_Integer|_Missing,_Rational|_Integer|_Missing}|
		"ALittle"]] :=
	(Message[DiaryDistance::invalid, HoldForm[dd]]; Missing["InvalidDiaryDistance"])


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
		DiaryMergeMissing[{deg,nin}],
		Quantity[4*(If[MissingQ[deg],0,deg]+If[MissingQ[nin],0,nin/60]),"Minutes"]
	]


(* ::Subsubsection:: *)
(*Verifiers*)


dd:DiaryDuration[Except[{_Rational|_Integer|_Missing,_Rational|_Integer|_Missing}]] :=
	(Message[DiaryDuration::invalid, HoldForm[dd]]; Missing["InvalidDiaryDuration"])


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
		DiaryMergeMissing[q],
		Replace[{kur,pan,sut,qa},_Missing->0,{1}].{180,36,6,1}
	]


(* ::Subsubsection:: *)
(*Verifiers*)


dc:DiaryCapacity[Except[{Repeated[_Rational|_Integer|_Missing,{4}]}]] :=
	(Message[DiaryCapacity::invalid, HoldForm[dc]]; Missing["InvalidDiaryCapacity"])


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
					{Dashing[0.1],LightOrange,Line[{{-0.5,0},{0.5,0}}]},
					Line[{{-0.5,-0.1},{-0.5,0.1}}],Line[{{0.5,-0.1},{0.5,0.1}}]},
				ImageSize->30,
				Background->Transparent
			],
			{{"cubits: ",cubits},{"fingers: ",fingers}},
			{},
			StandardForm
		]


DiaryDuration /:
	MakeBoxes[dur:DiaryDuration[{deg_,nin_}],StandardForm] :=
		BoxForm`ArrangeSummaryBox[
			DiaryDuration,
			dur,
			Graphics[{
					Circle[],LightOrange,EdgeForm[Black],Disk[{0,0},1,{Pi/2,0.25}]},
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
					LightOrange,Rectangle[{0,0},{1,0.7}],
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
