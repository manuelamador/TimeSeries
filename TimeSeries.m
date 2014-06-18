(* ::Package:: *)
 
(* Mathematica Package *)

(* :Title: TimeSeries *)

(* :Context: TimeSeries` *)

(* :Author:
        Manuel Amador
        amador.manuel@gmail.com
*)

(* :Package Version: 1.0 *)

(* :Mathematica Version: 9.0 *)

(* :Discussion:
        
        A collection of Time series functions. It provides an interface to the FRED database 
        and the World Bank database, as well as the ability to do Shaded plots (mostly for
        NBER recessions). Some other functions are included, such an HP filter function, and 
        some other basic series manipulation functions. 

        FRED function modified from here: http://library.wolfram.com/infocenter/MathSource/7583/              
*)

BeginPackage["TimeSeries`"];

listPlotShaded::usage =
"listPlotShaded[data, shadeareas, options] where data is the time series; shadeareas \
is of the form {{date1,date2},{date3, date4},...} representing the time intervals to be \
shaded. Additional options are ShadeRange -> {smin, smax}, which affects the range to be \
shaded; ShadeOpacity -> n, which adds opacity to the shade (defaults is 1); and \
ShadeColor -> color, which changes the color of the Shade (default is LightBlue).";

listLogPlotShaded::usage =
"listLogPlotShaded[data, shadeareas, options] where data is the time series; shadeareas \
is of the form {{date1,date2},{date3, date4},...} representing the time intervals to be \
shaded. Additional options are ShadeRange -> {smin, smax}, which affects the range to be \
shaded; ShadeOpacity -> n, which adds opacity to the shade (defaults is 1); and \
ShadeColor -> color, which changes the color of the Shade (default is LightBlue).";

dateListPlotShaded::usage =
"dateListPlotShaded[data, shadeareas, options] where data is the time serie; shadeareas \
is of the form {{date1,date2},{date3, date4},...} representing the time intervals to be \
shaded. Additional options are ShadeRange -> {smin, smax}, which affects the range to be \
shaded; ShadeOpacity -> n, which adds opacity to the shade (defaults is 1); and \
ShadeColor -> color, which changes the color of the Shade (default is LightBlue).";

dateListLogPlotShaded::usage =
"dateListLogPlotShaded[data, shadeareas, options] where data is the time series; shadeareas \
is of the form {{date1,date2},{date3, date4},...} representing the time intervals to be \
shaded. Additional options are ShadeRange -> {smin, smax}, which affects the range to be \
shaded; ShadeOpacity -> n, which adds opacity to the shade (defaults is 1); and \
ShadeColor -> color, which changes the color of the Shade (default is LightBlue).";

twoAxisDateListPlot::usage =
"twoAxisDateListPlot[{timeSeries1, timeSeries2}, axis2ticks, {optionsForTS1, \
optionsForTS2}, generalOptions, hasALegend}] merges two DateListPlots into a single one with \
two axis. The argument axis2ticks provides the ticks for the second axis. If timeSeries1 \
will have a legend (as provided in optionsForTS1), then hasLegend must be set to True.";

getNBERRecessionDates::usage =
"getNBERRecessionDates[] downloads the data of the NBER recessions from FRED. To be used in \
conjunction with DateListPlotShaded and DateListLogPlotShaded";

getTrendHPFilter::usage =
"getTrendHPFilter[series, s] returns the HP trend of series with smoothing parameter s.";

removeUnitsInTimeSeries::usage =
"removeUnitsInTimeSeries[] removes the units in a time series";

mapThreadTimeSeries::usage =
"mapThreadTimeSeries[f, ts] MapThreads f to all the elements of the collection of time series ts \
that share the same dates and returns the result. The argument ts should be a list of time \
series. It handles missing data (i.e. Missing[]).";

removeMissingDataFromTimeSeries::usage =
"removeMissingDataFromTimeSeries removes missing data from a time series.";

movingAverageTimeSeries::usage =
"movingAverageTimeSeries[ts, r, offset: 0] returns a time series with the corresponding \
moving average. The parameter r controls the length of the average, while offset controls the \
position of the center point (a value of 0 is a centered moving average).";

initializeFRED::usage = "";

saveFREDapikeyToFile::usage = 
"saveFREDapikey[s_String]";

FRED::usage =
"FRED[seriesID, {startDate, endDate}, options] takes a series ID, or lists of \
several series ID, and returns the data time series. The currently supported \
FRED options are those listed here: 
        http://api.stlouisfed.org/docs/fred/series_observations.html. 
Options should be entered as rules: FRED option entered as a string \[Rule] option value \
entered as a string. Start and end dates are the date range for the data \
entered as Mathematica date lists.";

FREDSearch::usage =
"FREDSearch[query] takes a text query and returns a list of series (including IDs) \
that matches. Returns a subset of information if  option \"ShowAll\" is False (default).";

initializeWorldBank::usage = 
"initializeWorldBank[] loads the indicators and country names into memory from the \
World Bank online database. Need to do this for searching the database but not for \
obtaining the data.";

worldBankSearch::usage = 
"worldBankSearch[query_String, f_String] or worldBankSearch[{queries__String}, f_] queries \
the World Bank indicators for an indicator that contains the query strings. The arg f \
takes one of two possible string values \"name\" or \"all\". A value of \"all\" queries all \
indicator fields \ for a match. A value of \"name\" queries only the name of the indicator.";

worldBankSearcher::usage = 
"worldBankSearcher[] launches a GUI Searcher."

worldBankGetIndicatorInfo::usage= 
"worldBankGetIndicatorInfo[s_String] returns all the information available about the indicator \
whose ID is s.";

worldBankFindCountryCode::usage=
"worldBankFindCountryCode[s_String] returns all two-digit country codes with a country name \
that matches s."; 

worldBankDataAllCountries::usage=
"worldBankDataAllCountries[s_String, date_List] finds countries in database with non Missing values \
for indicator ID s for the period specified in date = {beginDate_String, endDate_String}. \
The result is equivalent to calling worldBankData[s, \"all\", date] without countries with \
only missing values.";

worldBankGetAllCountryCodes::usage=
"worldBankGetAllCountryCodes[] returns the 2 country codes for all countries in the data set.";

worldBankData::usage=
"worldBankData[s_String, n_String, {initDate_String, endDate_String}] gets the time series for \
indicator ID s for country with country code n from initDate to endDate. Note that the dates are \
strings. The date strings can take the form of \"1990\", \"1990Q1\", \"1990M06\", to get year, \
quarterly or montly frequencies respectively. The arg n can be set to \"all\" in which \
case all countries are queried for the indicator. The data is returned in a format \
{\"country\" -> name, \"data\" -> time series} or a corresponding list.";


Begin["`Private`"];

(* Code below modified from Robert Shimer *)
getTrendHPFilter[ts_?(ArrayQ[#,1]&), smooth_?NumericQ] := 
        With[{T = Length[ts]},
                Join[
                        {
                                PadRight[{-1, 2, -1}, T], 
                                PadRight[{2, -5, 4, -1}, T]
                        },
                        Table[ArrayPad[{-1, 4, -6, 4, -1}, {t, T - t - 5}], 
                             {t, 0, T - 5}],
                        {
                                PadLeft[{-1, 4, -5, 2}, T],
                                PadLeft[{-1, 2, -1}, T]
                        }
                ] // Exp[(Inverse[IdentityMatrix[T] - N[smooth] #]).Log[N[ts]]] &
        ];

getNBERRecessionDates[] := 
        {First[#][[1]], DatePlus[Last[#][[1]], {1,"Month"}]} & /@ 
                DeleteCases[
                        Split[FRED["USREC", {{1800}, Date[]}], #1[[2]] == #2[[2]] &], 
                                {{_, 0}, ___}];

Options[listPlotShaded] = 
        Join[DeleteCases[Options[ListPlot], GridLines -> _], 
                {GridLines -> None, ShadeColor -> LightBlue, ShadeOpacity -> 1}];

With[{opt = First /@ Options[ListPlot]},
        listPlotShaded[ddata_, ShadeDates_, OptionsPattern[]] :=
                Module[{yRange, yPadding},
                        With[{plot = ListPlot[ddata, Sequence@@((#->OptionValue[#])& /@ opt)]},
                                yRange = Last[PlotRange/.AbsoluteOptions[plot, PlotRange]];
                                yPadding = If[OptionValue[Frame] === True, 
                                        - .2 Subtract@@yRange,
                                        0];
                                Show[plot,  
                                        Prolog -> {{OptionValue[ShadeColor], 
                                                        Opacity[OptionValue[ShadeOpacity]], 
                                                        (Rectangle[
                                                            {#[[1]], 
                                                             yRange[[1]] - yPadding}, 
                                                            {#[[2]], 
                                                             yRange[[2]] + yPadding}
                                                        ]&) /@ ShadeDates}, 
                                                {OptionValue[Prolog]}}
                        ]
                ]
        ]
];

Options[listLogPlotShaded] = 
        Join[DeleteCases[Options[ListLogPlot], GridLines -> _], {
                GridLines -> None, ShadeColor -> LightBlue, ShadeOpacity -> 1}];

With[{opt = First /@ Options[ListLogPlot]},
        listLogPlotShaded[ddata_, ShadeDates_, OptionsPattern[]] :=
                Module[{yRange, yPadding},
                        With[{plot = ListLogPlot[ddata, Sequence@@((#->OptionValue[#])& /@ opt)]},
                                yRange = Last[PlotRange/.AbsoluteOptions[plot, PlotRange]];
                                yPadding = If[OptionValue[Frame] === True, 
                                        - .2 Subtract@@yRange,
                                        0];
                                Show[plot,  
                                        Prolog -> {{OptionValue[ShadeColor], 
                                                    Opacity[OptionValue[ShadeOpacity]], 
                                                    (Rectangle[
                                                        {#[[1]], 
                                                         yRange[[1]] - yPadding}, 
                                                        {#[[2]], 
                                                         yRange[[2]] + yPadding}
                                                    ]&) /@ ShadeDates}, 
                                            {OptionValue[Prolog]}}
                        ]
                ]
        ]
];

Options[dateListLogPlotShaded] = 
        Join[DeleteCases[Options[DateListLogPlot], GridLines -> _], {
                GridLines -> None, ShadeColor -> LightBlue, ShadeOpacity -> 1}];

With[{opt = First /@ Options[DateListLogPlot]},
        dateListLogPlotShaded[ddata_, ShadeDates_, OptionsPattern[]] :=
                Module[{yRange, yPadding},
                        With[{plot = DateListLogPlot[ddata, Sequence@@((#->OptionValue[#])& /@ opt)]},
                                yRange = Last[PlotRange/.AbsoluteOptions[plot, PlotRange]];
                                yPadding = If[OptionValue[Frame] === True, 
                                        - .2 Subtract@@yRange,
                                        0];                                
                                Show[plot,  
                                        Prolog -> {{OptionValue[ShadeColor], 
                                                        Opacity[OptionValue[ShadeOpacity]], 
                                                        (Rectangle[
                                                            {AbsoluteTime[#[[1]]], 
                                                             yRange[[1]] - yPadding}, 
                                                            {AbsoluteTime[#[[2]]], 
                                                             yRange[[2]] + yPadding}
                                                        ]&) /@ ShadeDates}, 
                                                {OptionValue[Prolog]}}
                        ]
                ]
        ]
];

Options[dateListPlotShaded] = 
        Join[DeleteCases[Options[DateListPlot], GridLines -> _], 
                {GridLines -> None, ShadeColor -> LightBlue, ShadeOpacity -> 1}];

With[{opt = First /@ Options[DateListPlot]},
        dateListPlotShaded[ddata_, ShadeDates_, OptionsPattern[]] :=
                Module[{yRange, yPadding},
                        With[{plot=DateListPlot[ddata, Sequence@@((#->OptionValue[#])& /@ opt)]},
                                yRange= Last[PlotRange/.AbsoluteOptions[plot, PlotRange]];
                                yPadding = If[OptionValue[Frame] === True, 
                                        - .2 Subtract@@yRange,
                                        0];                                
                                Show[plot,
                                        Prolog -> {{OptionValue[ShadeColor], 
                                                Opacity[OptionValue[ShadeOpacity]], 
                                                (Rectangle[
                                                    {AbsoluteTime[#[[1]]], 
                                                            yRange[[1]] - yPadding}, 
                                                    {AbsoluteTime[#[[2]]], 
                                                            yRange[[2]] + yPadding}
                                                    ]&) /@ ShadeDates}, 
                                        {OptionValue[Prolog]}}
                        ]
                ]
        ]
];


ClearAll[twoAxisDateListPlot];
twoAxisDateListPlot[{timeseries1_, timeseries2_}, axis2ticks_: Automatic,
    {optionsForPlot1_:{}, optionsForPlot2_:{}}, generalOptions_:{},
    hasALegend_ : False] :=
    Module[
        {
            fgraph, ggraph, frange, grange, tticks, fticks, gticks
        },
        {fgraph, ggraph} = Map[DateListPlot[#[[1]], #1[[2]]] &,
            {{timeseries1, optionsForPlot1}, {timeseries2, optionsForPlot2}}];
        {frange, grange} = (PlotRange /. AbsoluteOptions[#, PlotRange])[[2]] & /@ {fgraph, ggraph};
        fticks=N@FindDivisions[frange,5];
        gticks = If[axis2ticks === Automatic,
            Quiet @ Transpose @ {
                fticks, ToString[NumberForm[#, 2], StandardForm] &
                    /@ Rescale[fticks, frange, grange]
            },
            MapThread[{#2, ToString[NumberForm[#1, 2], StandardForm]} &,
                {axis2ticks, Rescale[axis2ticks, grange, frange]}]
        ];
        Show[
            fgraph, ggraph /.
                Graphics[graph_, s___] :> Graphics[GeometricTransformation[graph,
                    RescalingTransform[{{0, 1}, grange}, {{0, 1}, frange}]], s
                ],
            Axes -> False,
            FrameTicks -> {{fticks, gticks},
                Options[If[hasALegend, fgraph[[1]], fgraph], FrameTicks][[1, 2, 2]]},
            generalOptions
        ]
    ];


(* TIME SERIES *)

removeUnitsInTimeSeries[t_] := 
        QuantityMagnitude[t]; 

mapThreadTimeSeries[f_, ts_]:= 
        Module[{date}, 
                First@Rest[
                        Reap[
                                Apply[Sow[#2, date[#1]]&, ts, {2}], 
                                Alternatives@@date /@ Intersection @@ ts[[All, All, 1]],
                                If[FreeQ[#2, Missing], 
                                        {#1[[1]], f @@ #2}, 
                                        {#1[[1]], Missing[]}]&
                        ]
                ]
        ];

removeMissingDataFromTimeSeries[t_] := 
        DeleteCases[t, {_, _Missing}];

ClearAll[movingAverageTimeSeries];
movingAverageTimeSeries[ts_, r_, offset_:0] := With[{center = IntegerPart[r / 2]},
    Transpose[{
        Take[ts[[All, 1]], {center + offset + 1, - r  + center + offset}],
        MovingAverage[ts[[All,2]], r]
    }]
];


(*    FRED  API ACCESS *)

ClearAll[makeQueryString];
makeQueryString[rules_List]:=
    Module[{tmp},
        tmp=rules/.Rule[x_String,y_String]:>StringJoin[x,"=",y];
        AppendTo[tmp, "file_type=xml"];
        StringJoin@@(ToLowerCase/@Riffle[tmp,"&"])
    ];

ClearAll[stringToNumber];
stringToNumber[x_String] :=
    ToExpression[StringReplace[x,","->""]]/.$Failed->Missing[];

apiKey = "";
directory = FileNameTake[$InputFileName, {0, -2}];

initializeFRED[] := 
        (apiKey = Import[FileNameJoin[{directory, "FREDapikey.txt"}], "Plaintext"];)

initializeFRED[key_String] := 
        apiKey = key;

saveFREDapikeyToFile[key_String] := 
        Export[FileNameJoin[{directory, "FREDapikey.txt"}], key, "Plaintext"];

SyntaxInformation[FRED] =
        {"ArgumentsPattern"->{_,_,OptionsPattern[]}};

Options[FRED]=
        {
                "limit"->"",
                "offset"->"",
                "sort_order"->"",
                "realtime_start"->"",
                "realtime_end"->"",
                "units"->"",
                "output_type"->"",
                "vintage_dates"->""
        };

FRED[series:(_String|_List), {startDate:(_List|Automatic), endDate:(_List|Automatic)}, 
        opts:OptionsPattern[]] :=
        If[apiKey === "", Print["FRED apikey missing, use initializeFred[]"];, 
                        Module[{today=DateString[DateList[],{"Year","-","Month","-","Day"}],
                                        options,start1,end1,queryString,tmp},
                                options=Append[{# -> OptionValue[#]}& /@ {
                                        "limit",
                                        "offset", "sort_order",
                                        "realtime_start", "realtime_end",
                                        "units", "output_type", "vintage_dates"
                                      }, "api_key"-> apiKey];
                                end1=If[SameQ[endDate, Automatic], 
                                                today,
                                                DateString[endDate,{"Year","-","Month","-","Day"}]
                                        ];
                                start1=If[SameQ[startDate, Automatic], 
                                                "1900-01-01",
                                                DateString[startDate,{"Year","-","Month","-","Day"}]
                                        ];
                                queryString=makeQueryString[options];
                                tmp=Import["http://api.stlouisfed.org/fred/series/observations?series_id="
                                        <>ToString[#]<>"&observation_start="<>start1<>"&observation_end="
                                        <>end1<>"&"<>queryString,"XML"]& /@ Flatten[{series}];
                                Cases[tmp,XMLElement["observation",{___,"date"->x_,"value"->y_},___]:>
                                        {DateList@x,stringToNumber@y},{1,4}]
                        ] 
        ];

Options[FREDSearch] = {"ShowAll" -> False}; 
FREDSearch[query_, opts:OptionsPattern[]] := 
        If[apiKey === "", Print["FRED apikey missing, use initializeFred[]"];, 
                        With[{q = StringReplace[query, " " -> "%20"]}, 
                                With[
                                        {
                                                dat = DeleteCases[
                                                                (Import["http://api.stlouisfed.org/fred/series/search?search_text=" <> 
                                                                        q <> "&api_key=" <> apiKey, "xml"] // 
                                                                        Cases[#, XMLElement["series", x___] :> x, Infinity] &), 
                                                                {}
                                                        ]
                                        },  
                                        If[!OptionValue["ShowAll"], 
                                                {
                                                           "id", "observation_start", "observation_end", 
                                                            "frequency_short", "units_short", "seasonal_adjustment_short",
                                                            "title"
                                                 } /. # & /@ dat, 
                                                 dat
                                        ]
                                ]      
                        ]
        ];


(*     WORLD BANK API ACCESSS     *) 

initializeWorldBank[] := (
        PrintTemporary["Getting list of indicators and country names from the WB..."];
        indicatorsWB = Import["http://api.worldbank.org/indicator?per_page=200000", "XML"][[2,3]];
        indicatorsWB = (indicatorsWB //. {
                        XMLElement[{_, "name"}, _, {y_}] :> ("name"-> y),
                        XMLElement[{_, "sourceNote"}, _, {y_}] :> ("sourceNote"-> y) , 
                        XMLElement[{_, "sourceOrganization"}, _, {y_}] :> ("sourceOrganization"-> y), 
                        XMLElement[{_, "source"}, _, {y_}] :> ("source"-> y), 
                        XMLElement[{_, "indicator"}, {"id"-> x_}, {y___}] :> {"indicator"-> x, y}}
                )//.XMLElement[__]:>Sequence[];
        countryNamesRawWB = Import["http://api.worldbank.org/countries/all?per_page=20000", "XML"][[2,3]];
        fromCountryNamesTo3CodeWB = Cases[countryNamesRawWB,
                XMLElement[{__, "country"}, {"id" -> code3_}, 
                        {___, XMLElement[{_, "name"}, {}, {name_}], ___}] :> name -> code3];
        fromCountryNamesTo2CodeWB=Cases[countryNamesRawWB,
                XMLElement[{__, "country"}, {"id" -> code3_},
                        {___, XMLElement[{_, "iso2Code"}, {}, {code2_}], ___}] :> (code3 -> code2)] 
                //. Reverse /@ fromCountryNamesTo3CodeWB;
          allCountryCodesWB = fromCountryNamesTo2CodeWB /. (_ -> y_) :> y;
);

worldBankGetAllCountryCodes[] := 
        fromCountryNamesTo2CodeWB;

worldBankDataAllCountries[s_String, date_List] := 
        With[{test = Function[x, If[Select[x, FreeQ[#, Missing]&, 1] == {}, 
                        False, True]]}, 
                Select[
                        worldBankData[s, "all", date], 
                        test["data" /. #] & 
                ]
        ];

worldBankGetIndicatorInfo[indicator_String]:= 
        Cases[indicatorsWB,
                {"indicator" -> indicator, __}];

worldBankFindCountryCode[query_String]:= 
        Select[fromCountryNamesTo2CodeWB, StringMatchQ[#[[1]], 
                        ___ ~~ query ~~ ___, IgnoreCase -> True]&];

worldBankSearch[{queries__String}, field : ("name" | "all")] := 
        With[{selection = Select[indicatorsWB, 
                        (And@@Table[StringMatchQ[
                                If[field == "name",  "name" /. #, 
                                        StringJoin[{"indicator", "name", "sourceNote", "source"} /. #]], 
                                ___ ~~ query ~~ ___, 
                                IgnoreCase->True], {query, {queries}}])& 
                ]}, 
                If[selection!={},  
                        Join[{{"indicator", "name", "source note", "source"}}, 
                                {"indicator", "name", "sourceNote", "source"} /. selection], 
                        "Nothing found"]
        ];

(* a single string is splitted at the commas *)
worldBankSearch[query_String, field : ("name" | "all")] :=
        worldBankSearch[StringTrim /@ StringSplit[query, ","], field];

(* the default is to search all indicator fields *)
worldBankSearch[query_] := 
        worldBankSearch[query, "all"];  

ClearAll[myDateList];
myDateList[date_String] :=
        Which[
                !StringFreeQ[date, "Q"], DateList[{date, {"Year","Q", "Quarter"}}],
		!StringFreeQ[date, "M"], DateList[{date, {"Year", "M", "Month"}}],
		True, DateList[date]
	];

ClearAll[parseData];
SetAttributes[parseData, Listable];
parseData[XMLElement[{_,"country"}, {"id"-> x_}, {y_}]] := 
        "country" -> {x, y};
parseData[XMLElement[{_,"date"}, _, {y_}]] := 
        "date" -> myDateList[y];
parseData[XMLElement[{_,"value"}, _, {}|{" "}|{""}]] := 
        "value" -> Missing[];
parseData[XMLElement[{_,"value"}, _, {y_}]] := 
        "value" -> ToExpression[y];
parseData[XMLElement[{_ ,"data"}, _, children_]] := 
        parseData[children];
parseData[XMLElement[___]] :=
        Sequence[];
 
worldBankData[indicator_String, countryCode2_String, {initDate_String, endDate_String}] :=
	Module[{page = 1, outcome = {}}, 
		While[True, 
			With[{result = Import[
					"http://api.worldbank.org/countries/" <> countryCode2 <> "/indicators/"
                                                <> indicator <> "?date=" <> initDate <> ":" <> endDate <>
                                                "&per_page=20000&page=" <> ToString[page],
					"XML"]},
				AppendTo[outcome,  result[[2, 3]]];
				If[ page >= ToExpression["pages" /. result[[2,2]]], 
					Break[],
					page++];
			]
		];
		With[{parsedOutcome = 
				{
					"country"->("country" /. First[#]), 
					"data"->({"date", "value"} /. #)
				} & /@ GatherBy[parseData[Flatten@outcome], First]
			},
			If[Length[parsedOutcome]==1, First[parsedOutcome], parsedOutcome]	
		]
	];


worldBankSearcher[] :=
        DynamicModule[{x},
              Manipulate[
                With[{outcome = worldBankSearch[search]},
                     If[Length[outcome] > 2,
                         Column[{
                                 x = {};
                                 ListPicker[
                                        Dynamic[x],
                                        StringJoin@Riffle[#, ",  "] & /@ Rest@outcome,
                                         AppearanceElements -> All,
                                         FieldSize -> {{1, Infinity}, {1, 20}},
                                         Scrollbars -> True,
                                         ImageSize -> {Large, Medium}
                                 ],
                                 "",
                                 Button["Copy Selection to Clipboard", 
                                        CopyToClipboard[StringSplit[#, "," , 2] & /@ x]],
                                 "",
                                 "Current selection:",
                                 Dynamic[If[x == {}, 
                                            " <None>",
                                            Column[
                                                    ((Tooltip[First[#],
                                                              Last[#]] &
                                                     ) @ StringSplit[#, ",", 2]) & /@ x
                                            ]
                                         ]
                                 ]
                         }]
                         ,
                         "No matches"]
                ],
                {{search, "GDP, current, US$, gross", 
                  "Search WB (separate by commas):"} , "GDP, current, US", 
                 InputField[#, String, ContinuousAction -> False] &}
              ]
        ];

End[];

EndPackage[];
