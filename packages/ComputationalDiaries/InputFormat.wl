(* ::Package:: *)

BeginPackage["ComputationalDiaries`InputFormat`", {"ComputationalDiaries`"}];


Begin["`Private`"];


DiaryParse[tablets_] :=
	Flatten[Function[tab, Module[{tabletID,creator},
		tabletID = tab["TabletID"];
		creator = tab["Creator"];
		Function[month, Function[observation,
				Identity//@If[MatchQ[observation,_DiaryObservation],
					observation,
					DiaryObservation[Association[Normal[<|
						"Type"->observation["Type"],
						"Content"->observation["Content"],
						"Date"->observation["Date"],
						"UUID"->observation["UUID"],
						"Provenance"-><|
							"Creator"->creator,
							"TabletID"->tabletID,
							"LineNumber"->observation["LineNumber"],
							"Notes"->observation["Notes"]
						|>
					|>] /. {
							Inferred[DiaryInputDate[day_,time_:Missing[]]] :>
								DiaryCombinedDate[
									DiaryBabylonianDate[{month["BabylonianYear"],month["BabylonianMonth"],Inferred[day]}],
									Inferred[time]
								],
							DiaryInputDate[day_,time_:Missing[]]:>
								DiaryCombinedDate[
									DiaryBabylonianDate[{month["BabylonianYear"],month["BabylonianMonth"],day}],
									time
								]
							}
						]
					]
				]
			]/@month["Observations"]]/@tab["Months"]
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


DiaryCurationPalette[] :=
	CreatePalette[makePalette[First[Import[referenceNBPath,"Notebook"]]],WindowTitle->"Curation"]


(* ::Subsubsection:: *)
(*Interface Functions*)


replaceButton[name_, payload_, depth_:1] := 
	Button[name,
		NotebookWrite[Nest[ParentBox,EvaluationBox[],depth],payload,All],
		Appearance->"Palette",Background->Hue[0.6`,0.21568627450980393`,1.`]
	]
replaceButton[name_] := replaceButton[name,ToBoxes[name]]


DiaryInputTemplate["Row", names_] :=
	Panel[Alternatives@@(replaceButton[#,ToBoxes[#],2]&/@names)]
DiaryInputTemplate["Column", names_, n_:3] :=
	Panel[Multicolumn[replaceButton[#,ToBoxes[#],3]&/@names,n]]
DiaryInputTemplate["Missing", name_] := DiaryInputTemplate["Row",
		{Placeholder[name],Missing["Destroyed"],Missing["Unmentioned"]}]
DiaryInputTemplate["Boolean"] := DiaryInputTemplate["Row",
	{True,False,Missing["Destroyed"],Missing["Unmentioned"]}]
DiaryInputTemplate["Object"] :=
	Panel[Grid[Append[
			Map[replaceButton[#[[1]],#[[2]],3]&,objectGroups,{2}],{
				replaceButton[Missing["Unmentioned"],ToBoxes@Missing["Unmentioned"],3],
				replaceButton[Missing["Destroyed"],ToBoxes@Missing["Destroyed"],3]
			}]]]
DiaryInputTemplate["Planet"] := DiaryInputTemplate["Row",{
		Entity["PlanetaryMoon","Moon"],
		Entity["Planet","Mercury"],
		Entity["Planet","Venus"],
		Entity["Planet","Mars"],
		Entity["Planet","Jupiter"],
		Entity["Planet","Saturn"],
		Entity["Star","Sirius"],
		Missing["Destroyed"],
		Missing["Unmentioned"]}]
DiaryInputTemplate["ZodiacSign"] := DiaryInputTemplate["Row",{
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
DiaryInputTemplate["King"] := DiaryInputTemplate["Row",{
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
DiaryInputTemplate["Time"] := DiaryInputTemplate["Column",{
		"BeginningOfTheNight",
		"FirstPartOfTheNight",
		"MiddlePartOfTheNight",
		"LastPartOfTheNight",
		"Morning",
		"Noon",
		"Afternoon",
		"Sunset",
		"Day",
		"Night",
		Missing["Destroyed"],
		Missing["Unmentioned"]},3]


objectGroups = {{
	{"Moon",ToBoxes@Entity["PlanetaryMoon","Moon"]},
	{"Mercury",ToBoxes@Entity["Planet","Mercury"]},
	{"Venus",ToBoxes@Entity["Planet","Venus"]},
	{"Mars",ToBoxes@Entity["Planet","Mars"]},
	{"Jupiter",ToBoxes@Entity["Planet","Jupiter"]},
	{"Saturn",ToBoxes@Entity["Planet","Saturn"]}},{
	{"EtaPiscium",MakeBoxes@DiaryNormalStar["EtaPiscium"]}},{
	{"BetaArietis",MakeBoxes@DiaryNormalStar["BetaArietis"]},
	{"AlphaArietis",MakeBoxes@DiaryNormalStar["AlphaArietis"]}},{
	{"EtaTauri",MakeBoxes@DiaryNormalStar["EtaTauri"]},
	{"AlphaTauri",MakeBoxes@DiaryNormalStar["AlphaTauri"]},
	{"BetaTauri",MakeBoxes@DiaryNormalStar["BetaTauri"]},
	{"ZetaTauri",MakeBoxes@DiaryNormalStar["ZetaTauri"]}},{
	{"EtaGeminorum",MakeBoxes@DiaryNormalStar["EtaGeminorum"]},
	{"MuGeminorum",MakeBoxes@DiaryNormalStar["MuGeminorum"]},
	{"GammaGeminorum",MakeBoxes@DiaryNormalStar["GammaGeminorum"]},
	{"AlphaGeminorum",MakeBoxes@DiaryNormalStar["AlphaGeminorum"]},
	{"BetaGeminorum",MakeBoxes@DiaryNormalStar["BetaGeminorum"]}},{
	{"EtaCancri",MakeBoxes@DiaryNormalStar["EtaCancri"]},
	{"ThetaCancri",MakeBoxes@DiaryNormalStar["ThetaCancri"]},
	{"GammaCancri",MakeBoxes@DiaryNormalStar["GammaCancri"]},
	{"DeltaCancri",MakeBoxes@DiaryNormalStar["DeltaCancri"]}},{
	{"EpsilonLeonis",MakeBoxes@DiaryNormalStar["EpsilonLeonis"]},
	{"AlphaLeonis",MakeBoxes@DiaryNormalStar["AlphaLeonis"]},
	{"RhoLeonis",MakeBoxes@DiaryNormalStar["RhoLeonis"]},
	{"ThetaLeonis",MakeBoxes@DiaryNormalStar["ThetaLeonis"]}},{
	{"BetaVirginis",MakeBoxes@DiaryNormalStar["BetaVirginis"]},
	{"GammaVirginis",MakeBoxes@DiaryNormalStar["GammaVirginis"]},
	{"AlphaVirginis",MakeBoxes@DiaryNormalStar["AlphaVirginis"]}},{
	{"AlphaLibrae",MakeBoxes@DiaryNormalStar["AlphaLibrae"]},
	{"BetaLibrae",MakeBoxes@DiaryNormalStar["BetaLibrae"]}},{
	{"DeltaScorpii",MakeBoxes@DiaryNormalStar["DeltaScorpii"]},
	{"BetaScorpii",MakeBoxes@DiaryNormalStar["BetaScorpii"]},
	{"AlphaScorpii",MakeBoxes@DiaryNormalStar["AlphaScorpii"]},
	{"PiScorpii",MakeBoxes@DiaryNormalStar["PiScorpii"]}},{
	{"ThetaOphiuchi",MakeBoxes@DiaryNormalStar["ThetaOphiuchi"]}},{
	{"BetaCapricorni",MakeBoxes@DiaryNormalStar["BetaCapricorni"]},
	{"GammaCapricorni",MakeBoxes@DiaryNormalStar["GammaCapricorni"]},
	{"DeltaCapricorni",MakeBoxes@DiaryNormalStar["DeltaCapricorni"]}}};


(* ::Subsection:: *)
(*End*)


End[];


EndPackage[];
