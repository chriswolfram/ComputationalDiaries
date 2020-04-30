(* ::Package:: *)

BeginPackage["AstronomicalDiaries`DiaryTypes`",{
	"AstronomicalDiaries`Constants`"
}];


ClearAll["AstronomicalDiaries`DiaryTypes`*"];


DiaryDistance::usage = "Represents a distance on the sky measured in cubits (ku\[SHacek]) and fingers (\[SHacek]i or u)";
DiaryDisplacment::usage = "Represents a displacment between two astronomical objects.";
DiaryDirection::usage = "Represents a direction in which an observation was made.";
DiaryDate::usage = "Represents a date that can use both Babylonian and Julian calendars.";
DiaryEvent::usage = "Represents an event recorded in the Astronomical Diares."


Begin["`Private`"];


(* ::Subsection:: *)
(*DiaryDistance*)


DiaryDistance::invalid = "`` is not a valid DiaryDistance.";


DiaryDistance[data_Association]["Data"] := data
DiaryDistance[data_Association]["Cubits"] := data["Cubits"]
DiaryDistance[data_Association]["Fingers"] := data["Fingers"]
DiaryDistance[data_Association]["TotalCubits"] :=
	If[MissingQ[data["Cubits"]],data["Cubits"],
		data["Cubits"]+If[MissingQ[data["Fingers"]],0,data["Fingers"]]/24
	]
DiaryDistance[data_Association]["Degrees"] :=
	If[MissingQ[data["TotalCubits"]],Missing[],2*data["TotalCubits"]]


DiaryDistance[d:Except[KeyValuePattern[{
		"Cubits"->_Rational|_Integer|_Missing,
		"Fingers"->_Rational|_Integer|_Missing
	}]]] :=
		(Message[DiaryDistance::invalid, {d}]; Missing["InvalidDiaryDistance"])


(* ::Subsection:: *)
(*DiaryDisplacement*)


DiaryDisplacment::invalid = "`` is not a valid DiaryDisplacment.";


DiaryDisplacment[data_Association]["Data"] := data
DiaryDisplacment[data_Association]["Distance"] := data["Distance"]
DiaryDisplacment[data_Association]["Relation"] := data["Relation"]


DiaryDisplacment[d:Except[KeyValuePattern[{
		"Distance"->_DiaryDistance|_Missing,
		"Relation"->"Above"|"Below"|"InFrontOf"|"Behind"|_Missing
	}]]] :=
		(Message[DiaryDisplacment::invalid, {d}]; Missing["InvalidDiaryDisplacment"])


(* ::Subsection:: *)
(*DiaryDirection*)


DiaryDirection::invalid = "`` is not a valid DiaryDirection.";


DiaryDirection[data_Association]["Data"] := data
DiaryDirection[data_Association]["Distance"] := data["Distance"]
DiaryDirection[data_Association]["Direction"] := data["Direction"]


DiaryDirection[d:Except[KeyValuePattern[{
		"Distance"->_DiaryDistance|_Missing,
		"Direction"->"West"|"North"|"South"|"East"|_Missing
	}]]] :=
		(Message[DiaryDirection::invalid, {d}]; Missing["InvalidDiaryDirection"])


(* ::Subsection:: *)
(*DiaryDate*)


DiaryDate::invalid = "`` is not a valid DiaryDate.";


DiaryDate[data_Association]["Data"] := data
DiaryDate[data_Association]["JulianDate"] := 
	If[MissingQ[data["JulianYear"]]||MissingQ[data["JulianMonth"]]||MissingQ[data["JulianDay"]],
		Missing["Underspecified"],
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


DiaryEvent[data_Association]["Data"] := data
DiaryEvent[data_Association]["Type"] := data["Type"]
DiaryEvent[data_Association]["Provenance"] := data["Provenance"]
DiaryEvent[data_Association]["Content"] := data["Content"]


DiaryEvent[d:Except[KeyValuePattern[{
		"Type"->_String,
		"Provenance"->_,
		"Content"->_
	}]]] :=
		(Message[DiaryDate::invalid, {d}]; Missing["InvalidDiaryEvent"])


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


DiaryDisplacment /:
	MakeBoxes[dis:DiaryDisplacment[_Association],StandardForm] :=
		BoxForm`ArrangeSummaryBox[
			DiaryDisplacment,
			dist,
			Graphics[{
					{Orange,Dashing[0.1],Line[{{-0.5,-0.5},{0.5,0.5}}]},
					Circle[],Red,Point[{-0.5,-0.5}],Blue,Point[{0.5,0.5}]},
				ImageSize->30,
				Background->Transparent
			],
			{
				{"distance: ",Row[{
					If[MissingQ[dis["Distance"]],dis["Distance"],dis["Distance"]["TotalCubits"]],
					" cubits"}]},
				{"relation: ",dis["Relation"]}
			},
			{},
			StandardForm
		]


DiaryDirection /:
	MakeBoxes[dir:DiaryDirection[_Association],StandardForm] :=
		BoxForm`ArrangeSummaryBox[
			DiaryDirection,
			dist,
			Graphics[{
					Circle[],
					Orange,
					Arrowheads[Large],
					Arrow[{{0,0},
						0.9*Normalize@
							Switch[dir["Direction"],"North",{0,1},"South",{0,-1},"East",{1,0},"West",{-1,0},_,{1,1}]
					}]},
				ImageSize->30,
				PlotRange->All,
				Background->Transparent
			],
			{
				{"distance: ",Row[{
					If[MissingQ[dir["Distance"]],dir["Distance"],dir["Distance"]["TotalCubits"]],
					" cubits"}]},
				{"direction: ",dir["Direction"]}
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
