(* ::Package:: *)

BeginPackage["ComputationalDiaries`DiaryTypes`", {"ComputationalDiaries`"}];


Begin["`Private`"];


(* ::Subsection:: *)
(*DiaryDates*)


DiaryJulianDate::invalid = "`` is not a valid DiaryJulianDate.";
DiaryBabylonianDate::invalid = "`` is not a valid DiaryBabylonianDate.";
DiaryCombinedDate::invalid = "`` is not a valid DiaryCombinedDate.";


(* ::Subsubsection:: *)
(*Constructors*)


DiaryJulianDate[d_?DateObjectQ] :=
	With[{ymd = DateValue[CalendarConvert[d,"Julian"],{"Year","Month","Day"}]},
		DiaryJulianDate[ymd + {Boole[ymd[[1]]<=0],0,0}]
	]


DiaryJulianDate[dbd_DiaryBabylonianDate, chron_] :=
	Enclose[
		With[{
			alignment = Confirm@SelectFirst[chron,
				#[[1]]["Year"]===dbd["Year"]&&#[[1]]["Month"]===dbd["Month"]&]},
			DiaryJulianDate@DatePlus[
				Confirm[alignment[[2]]["DateObject"]],
				ConfirmMatch[dbd["Day"],_Integer]-Confirm[alignment[[1]]["Day"]]]
		],
		Missing["CouldNotInferJulianDate"]&
	]
DiaryJulianDate[dbd_DiaryBabylonianDate] := DiaryJulianDate[dbd, $DiaryChronology]


DiaryCombinedDate[dbd_DiaryBabylonianDate, time_] :=
	DiaryBabylonianDate[<|
		"JulianDate"->Missing[],
		"BabylonianDate"->dbd,
		"Time"->time
	|>]


(* ::Subsubsection:: *)
(*Accessors*)


DiaryJulianDate[{year_,month_,day_}]["Year"] := year
DiaryJulianDate[{year_,month_,day_}]["Month"] := month
DiaryJulianDate[{year_,month_,day_}]["Day"] := day
djd_DiaryJulianDate["DateObject"] :=
	If[MissingQ[djd["Year"]]||MissingQ[djd["Month"]]||MissingQ[djd["Day"]],
		DiaryMergeMissing[{djd["Year"],djd["Month"],djd["Day"]}],
		DateObject[{
				djd["Year"]-Boole[djd["Year"] <= 0],
				djd["Month"],
				djd["Day"]
			},"Day",
			CalendarType->"Julian",
			TimeZone->3 (*time zone of Babylon*)
		]
	]


DiaryBabylonianDate[{year_,month_,day_}]["Year"] := year
DiaryBabylonianDate[{{king_,regnalYear_},month_,day_}]["King"] := king
DiaryBabylonianDate[{{king_,regnalYear_},month_,day_}]["RegnalYear"] := regnalYear
DiaryBabylonianDate[{year_,month_,day_}]["Month"] := month
DiaryBabylonianDate[{year_,month_,day_}]["Day"] := day


DiaryCombinedDate[data_]["JulianDate"] :=
	Which[
		!MissingQ[data["JulianDate"]], data["JulianDate"],
		!MissingQ[data["BabylonianDate"]], DiaryJulianDate[data["BabylonianDate"]],
		True, DiaryMergeMissing[{data["JulianDate"],data["BabylonianDate"]}]
	]
DiaryCombinedDate[data_]["BabylonianDate"] := data["BabylonianDate"]
DiaryCombinedDate[data_]["Time"] := data["Time"]


(* ::Subsubsection:: *)
(*Verifiers*)


months = {"I","II","III","IV","V","VI","VII","VIII","IX","X","XI","XII","VI2","XII2"};
times = {"BeginningOfTheNight","FirstPartOfTheNight","MiddlePartOfTheNight",
		"LastPartOfTheNight","Morning","Noon","Afternoon","Sunset","Day","Night"};


djd:DiaryJulianDate[Except[{_Integer|_Missing, _Integer|_Missing, _Integer|_Missing}]] :=
	(Message[DiaryJulianDate::invalid, HoldForm[djd]]; Missing["InvalidDiaryJulianDate"])


dbd:DiaryBabylonianDate[Except[{
		{_String|_Missing, _Integer|_Missing},
		(Alternatives@@months)|_Missing,
		_Integer|"Middle"|"End"|_Missing
	}]] :=
	(Message[DiaryBabylonianDate::invalid, HoldForm[dbd]]; Missing["DiaryBabylonianDate"])


dcd:DiaryCombinedDate[Except[KeyValuePattern[{
		"JulianDate"->_DiaryJulianDate|_Missing,
		"BabylonianDate"->_DiaryBabylonianDate|_Missing,
		"Time"->(Alternatives@@times)|_Missing
	}]]] :=
	(Message[DiaryCombinedDate::invalid, HoldForm[dcd]]; Missing["DiaryCombinedDate"])


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
