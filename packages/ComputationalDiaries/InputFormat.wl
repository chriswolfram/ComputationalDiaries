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
replaceButton::usage = "Makes a button that replaces its parent box when pressed.";
replaceButtonTemplate::usage =
	"Makes a collection of replaceButtons for importing common formats.";


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
			RowBox[{"Evaluate", "[", content_, "]"}]:>
				ToBoxes[ReleaseHold[MakeExpression[content,StandardForm]]]],
		Appearance->"Palette"
	]


referenceNBPath =
	FileNameJoin[{DirectoryName[$InputFileName],"curationPaletteReference.nb"}];


launchCurationPalette[] :=
	CreatePalette[makePalette[First[Import[referenceNBPath,"Notebook"]]],WindowTitle->"Curation"]


(* ::Subsubsection:: *)
(*Interface Functions*)


replaceButton[name_, payload_, depth_:1] := 
	Button[name,
		NotebookWrite[Nest[ParentBox,EvaluationBox[],depth],payload,All],
		Appearance->"Palette",Background->Hue[0.6`,0.21568627450980393`,1.`]
	]
replaceButton[name_] := replaceButton[name,ToBoxes[name]]


replaceButtonTemplate["Row", names_] :=
	Panel[Alternatives@@(replaceButton[#,ToBoxes[#],2]&/@names)]
replaceButtonTemplate["Column", names_, n_:3] :=
	Panel[Multicolumn[replaceButton[#,ToBoxes[#],3]&/@names,n]]
replaceButtonTemplate["Missing", name_] := replaceButtonTemplate["Row",
		{Placeholder[name],Missing["Destroyed"],Missing["Unmentioned"]}]
replaceButtonTemplate["Boolean"] := replaceButtonTemplate["Row",
	{True,False,Missing["Destroyed"],Missing["Unmentioned"]}]
replaceButtonTemplate["Object"] :=
	Panel[Grid[Append[
			Map[replaceButton[#[[1]],#[[2]],3]&,objectGroups,{2}],{
				replaceButton[Missing["Unmentioned"],Missing["Unmentioned"],3],
				replaceButton[Missing["Destroyed"],Missing["Destroyed"],3]
			}]]]
replaceButtonTemplate["Planet"] := replaceButtonTemplate["Row",{
		Entity["PlanetaryMoon","Moon"],
		Entity["Planet","Mercury"],
		Entity["Planet","Venus"],
		Entity["Planet","Mars"],
		Entity["Planet","Jupiter"],
		Entity["Planet","Saturn"],
		Entity["Star","Sirius"],
		Missing["Destroyed"],
		Missing["Unmentioned"]}]
replaceButtonTemplate["ZodiacSign"] := replaceButtonTemplate["Row",{
		Entity["Constellation","Aries"],
		Entity["Constellation","Taurus"],
		Entity["Constellation","Gemini"],
		Entity["Constellation","Cancer"],
		Entity["Constellation","Leo"],
		Entity["Constellation","Virgo"],
		Entity["Constellation","Libra"],
		Entity["Constellation","Scorpius"],
		Entity["Constellation","Sagittarius"],
		Entity["Constellation","Capricornus"],
		Entity["Constellation","Aquarius"],
		Entity["Constellation","Pisces"],
		Missing["Destroyed"],
		Missing["Unmentioned"]}]
replaceButtonTemplate["King"] := replaceButtonTemplate["Row",{
		"\[CapitalSHacek]ama\[SHacek]\[SHacek]umukin",
		"NebukadnezarII",
		"ArtaxerxesI",
		"DariusII",
		"ArtaxerxesII",
		"ArtaxerxesIII",
		"DariusIII",
		"AlexanderIII",
		"PhilipArrhidaeus",
		"AlexanderIV",
		"SE"}]
replaceButtonTemplate["Time"] := replaceButtonTemplate["Column",{
		"BeginningOfTheNight",
		"FirstPartOfTheNight",
		"MiddlePartOfTheNight",
		"LastPartOfTheNight",
		"Morning",
		"Noon",
		"Afternoon",
		"Sunset",
		Missing["Destroyed"],
		Missing["Unmentioned"]},3]


objectGroups = {{
	{"Moon",ToBoxes@Entity["PlanetaryMoon","Moon"]},
	{"Mercury",ToBoxes@Entity["Planet","Mercury"]},
	{"Venus",ToBoxes@Entity["Planet","Venus"]},
	{"Mars",ToBoxes@Entity["Planet","Mars"]},
	{"Jupiter",ToBoxes@Entity["Planet","Jupiter"]},
	{"Saturn",ToBoxes@Entity["Planet","Saturn"]}},{
	{"EtaPiscium",MakeBoxes@getNormalStars["EtaPiscium"]}},{
	{"BetaArietis",MakeBoxes@getNormalStars["BetaArietis"]},
	{"AlphaArietis",MakeBoxes@getNormalStars["AlphaArietis"]}},{
	{"EtaTauri",MakeBoxes@getNormalStars["EtaTauri"]},
	{"AlphaTauri",MakeBoxes@getNormalStars["AlphaTauri"]},
	{"BetaTauri",MakeBoxes@getNormalStars["BetaTauri"]},
	{"ZetaTauri",MakeBoxes@getNormalStars["ZetaTauri"]}},{
	{"EtaGeminorum",MakeBoxes@getNormalStars["EtaGeminorum"]},
	{"MuGeminorum",MakeBoxes@getNormalStars["MuGeminorum"]},
	{"GammaGeminorum",MakeBoxes@getNormalStars["GammaGeminorum"]},
	{"AlphaGeminorum",MakeBoxes@getNormalStars["AlphaGeminorum"]},
	{"BetaGeminorum",MakeBoxes@getNormalStars["BetaGeminorum"]}},{
	{"EtaCancri",MakeBoxes@getNormalStars["EtaCancri"]},
	{"ThetaCancri",MakeBoxes@getNormalStars["ThetaCancri"]},
	{"GammaCancri",MakeBoxes@getNormalStars["GammaCancri"]},
	{"DeltaCancri",MakeBoxes@getNormalStars["DeltaCancri"]}},{
	{"EpsilonLeonis",MakeBoxes@getNormalStars["EpsilonLeonis"]},
	{"AlphaLeonis",MakeBoxes@getNormalStars["AlphaLeonis"]},
	{"RhoLeonis",MakeBoxes@getNormalStars["RhoLeonis"]},
	{"ThetaLeonis",MakeBoxes@getNormalStars["ThetaLeonis"]}},{
	{"BetaVirginis",MakeBoxes@getNormalStars["BetaVirginis"]},
	{"GammaVirginis",MakeBoxes@getNormalStars["GammaVirginis"]},
	{"AlphaVirginis",MakeBoxes@getNormalStars["AlphaVirginis"]}},{
	{"AlphaLibrae",MakeBoxes@getNormalStars["AlphaLibrae"]},
	{"BetaLibrae",MakeBoxes@getNormalStars["BetaLibrae"]}},{
	{"DeltaScorpii",MakeBoxes@getNormalStars["DeltaScorpii"]},
	{"BetaScorpii",MakeBoxes@getNormalStars["BetaScorpii"]},
	{"AlphaScorpii",MakeBoxes@getNormalStars["AlphaScorpii"]},
	{"PiScorpii",MakeBoxes@getNormalStars["PiScorpii"]}},{
	{"ThetaOphiuchi",MakeBoxes@getNormalStars["ThetaOphiuchi"]}},{
	{"BetaCapricorni",MakeBoxes@getNormalStars["BetaCapricorni"]},
	{"GammaCapricorni",MakeBoxes@getNormalStars["GammaCapricorni"]},
	{"DeltaCapricorni",MakeBoxes@getNormalStars["DeltaCapricorni"]}}};


(* ::Subsection:: *)
(*End*)


End[];


EndPackage[];
