(* ::Package:: *)

BeginPackage["Aspen`"];
(* This package allows Mathematica to communicate with Aspen Plus *)

LoadAspenPlusFile[filename_, OptionsPattern[{run -> False, visible->True}]]:=Module[{},
  Needs["NETLink`"];
  ASPEN = NETLink`CreateCOMObject["Apwn.Document"];
  ASPEN@SuppressDialogs=1;
  ASPEN@InitFromFile2[filename];
  If[OptionValue[visible], ASPEN@Visible = True];
  If[OptionValue[run], ASPEN@Run2[]];
]



ExportAspenStreamTable[]:= Module[{}, 
  hierarchies =GetHierarchies[];
  data = Association@Table[
    {h@Name -> (GetStreamDataTable[h])},
    {h, hierarchies}
  ];

  AppendTo[data, "Main" -> GetStreamDataTable[""]];
  Export[DirectoryName[filename]<> "\\"<>FileBaseName[filename]<>".xlsx", "Sheets"->Normal@data, "Rules" ]
]

GetHierarchies[]:=Module[{},
<|#@Name-># &/@Select[
			Table[ASPEN@Tree@Data[]@Blocks[]@Elements@Item[i],{i,ASPEN@Tree@Data[]@Streams[]@Elements@Count-1}],
			#@AttributeValue[6]=="Hierarchy" &]|>
]

GetHierarchyStreams[hierarchy_]:=Module[{},
  DeleteCases[
    If[hierarchy==="",
      elements=ASPEN@Tree@Data[]@Streams[]@Elements,
      elements =hierarchy@Data[]@Streams[]@Elements];

    Table[
      Join[
        <|"ID"->i|>,
        GetStreamData[elements@Item[i]]
        ],
      {i,0,elements@Count-1}],
  ""] 
]
    
GetBlocks[]:=Module[{},
  Table[
    {
      ASPEN@Tree@Data[]@Blocks[]@Elements@Item[i]@Name,
      ASPEN@Tree@Data[]@Blocks[]@Elements@Item[i]@AttributeValue[6]
    },
    {i,0,ASPEN@Tree@Data[]@Blocks[]@Elements@Count-1}]
]


GetComponents[stream_]:=Table[#@Item[i]@Name,{i,0,#@Count-1}]&@stream@Output[]@Elements@Item["MASSFLOW"]@Elements@Item[1]@Elements

GetMassFlows[stream_]:=Total[
  Table[
    Table[#@Item[i]@Value,{i,0,#@Count-1}] & @ (#@Item[sub]@Elements),
    {sub,1,#@Count-1}] & @ stream@Output[]@Elements@Item["MASSFLOW"]@Elements//.Null->0]

GetStreamData[stream_] := Module[{},
  <|
    Switch[stream@AttributeValue[6],
        "MATERIAL",Join[<|
        "Name"->stream@Name,
        "From"-> stream@Ports[]@SOURCE[]@Elements@Item[0]@Name,
        "To"->stream@Ports[]@DEST[]@Elements@Item[0]@Name,
        "Temperature"->stream@Output[]@TEMPUOUT[]@Elements[]@Item[1]@Value,
        "Pressure"->stream@Output[]@PRESUOUT[]@Elements[]@Item[1]@Value,
        "Flow"->stream@Output[]@MASSFLMX[]@Elements@Item[0]@Value, "Type"->"MATERIAL"|>,
        Association@MapThread[Rule,{GetComponents[stream], GetMassFlows[stream]}]],
        "HEAT",

        {"Name"->stream@Name,
        "From"->stream@Ports[]@SOURCE[]@Elements@Item[0]@Name,
        "To"-> stream@Ports[]@DEST[]@Elements@Item[0]@Name,
          "Flow"->stream@Output[]@QCALC[]@Value,
        "Type"->"HEAT"},
        "WORK",

        { "Name"->stream@Name,
          "From"->stream@Ports[]@SOURCE[]@Elements@Item[0]@Name,
          "To"->stream@Ports[]@DEST[]@Elements@Item[0]@Name,
          "Flow"->stream@Output[]@POWERUOUT[]@Value,
        "Type"->"WORK"},True, <||>]
  |>
]

GetStreamDataTable[hierarchy_]:=Module[{},
	s = GetHierarchyStreams[hierarchy];
	headings = First@MaximalBy[Length][ Keys/@Select[s,#["Type"]=="MATERIAL"&]];
	Join[
		List@headings,
		ReplaceAll[
			Table[Replace[row[#]&/@headings,x_Missing:>"",{1}],{row,SortBy[s,<|"MATERIAL"->1,"HEAT"->2,"WORK"->3|>[#["Type"]]&]}],
			{
$Failed[Value] ->0,
""[Value]->0,
Null[Value]->0,
 Null -> "",
 Null[Name] -> "",
x_/;StringContainsQ[ToString@x, "(IN)" | "(OUT)"]->""}]
	]
]



EndPackage[]
