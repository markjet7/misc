BeginPackage["Aspen`"];
(* This package allows Mathematica to communicate with Aspen Plus *)

Options[LoadAspenPlus] = {run -> False, visible->True}
LoadAspenPlusFile[filename_, OptionsPattern[]]:=Module[{},
  ASPEN = CreateCOMObject["Apwn.Document"];
  ASPEN@SuppressDialogs=1;
  ASPEN@InitFromFile2[filename];
  If[OptionValue[visible], ASPEN@Visible = True];
  If[OptionValue[run], ASPEN@Run2[]];
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
        GetStreamData[elements@Item[i]]],
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


GetComponents[streamName_]:=Table[#@Item[i]@Name,{i,0,#@Count-1}]&@ASPEN@Tree@Data[]@Streams[]@Elements@Item[streamName]@Output[]@Elements@Item["MASSFLOW"]@Elements@Item[1]@Elements

GetMassFlows[streamName_]:=Total[
  Table[
    Table[#@Item[i]@Value,{i,0,#@Count-1}] & @ (#@Item[sub]@Elements),
    {sub,1,#@Count-1}] & @ ASPEN@Tree@Data[]@Streams[]@Elements@Item[streamName]@Output[]@Elements@Item["MASSFLOW"]@Elements//.Null->0]

GetStreamData[stream_] := Module[{},
  <|ReplaceAll[
    Switch[stream@AttributeValue[6],
        "MATERIAL",Join[<|
        "Name"->stream@Name,
        "From"-> stream@Ports[]@SOURCE[]@Elements@Item[0]@Name,
        "To"->stream@Ports[]@DEST[]@Elements@Item[0]@Name,
        "Temperature"->stream@Output[]@TEMPUOUT[]@Elements[]@Item[1]@Value,
        "Pressure"->stream@Output[]@PRESUOUT[]@Elements[]@Item[1]@Value,
        "Flow"->stream@Output[]@MASSFLMX[]@Elements@Item[0]@Value, "Type"->"MATERIAL"|>,
        Association@MapThread[Rule,{GetComponents[stream@Name], GetMassFlows[stream@Name]}]],
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
        "Type"->"WORK"},True, <||>], {
      $Failed[Value] ->0,
      ""[Value]->0,
      Null[Value]->0,
      Null -> "",
      Null[Name] -> "",
      x_/;StringContainsQ[ToString@x, "(IN)" | "(OUT)"]->""}
  ]|>
]

EndPackage[]