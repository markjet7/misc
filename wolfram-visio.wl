BeginPackage["Visio`"];

ENUMS =   <|
       visSectionProp -> 243,
       visRowLast -> -2,
       visTagDefault -> 0,
       visCustPropsLabel -> 2,
       visCustPropsType -> 5,
       visCustPropsValue -> 0,
       visSectionUser -> 242,
       visUserValue -> 0,
       visOpenHidden -> 64,
       visOpenMinimized -> 16,
       visOpenDocked -> 4,
       visSectionObject -> 1,
       visRowPageLayout -> 24,
       visPLOAvenueSizeX -> 20,
       visPLOAvenueSizeY -> 21,
       visSectionParagraph -> 4,
       visHorzAlign -> 6,
       visRowLine -> 2,
       visLinePattern -> 2,
       visRowXFormOut -> 1,
       visXFormWidth -> 2,
       visXFormHeight -> 3,
       visXFormPinX -> 0,
       visXFormPinY -> 1,
       visLineWeight -> 0,
       visFillPattern -> 2,
       visGraphicPropertyLabel -> 1,
       visGraphicExpression -> 2,
       visTypeDataGraphic -> 5,
       visBuiltInStencilCallouts -> 3|>;

LoadVisio[filename_] := Module[{},
    VISIO = CreateCOMObject["Visio.Application"];
    VISIO@ActiveDocument[]@GestureFormatSheet@CellsSRC[ENUMS@visSectionCharacter, 0, ENUMS@visCharacterSize]@FormulaU = "16 pt";
    VISIO@Documents@Add[filename];
    STENCILS = <|(# -> 
        VISIO@Documents[]@
          OpenEx[#, 
           ENUMS@visOpenDocked] & /@
            {"BASIC_U.vss", 
       "PEEQP_U.vss", "PEHEAT_U.vss",
              "PEPUMP_U.vss", "PEVESS_U.vss", "PEVALV_U.vss", 
       "SDCALL_U.vssx"})|>;
  
    SHAPES = <|
        "Mixer" -> STENCILS["PEEQP_U.vss"]@Masters["Mixer"],
        "Crusher" -> STENCILS["PEEQP_U.vss"]@Masters["Crusher"],
        "Flash2" -> STENCILS["PEVESS_U.vss"]@Masters["Tray column"],
        "Flash3" -> STENCILS["PEVESS_U.vss"]@Masters["Tray column"],
        "Heater" -> STENCILS["PEHEAT_U.vss"]@Masters["Heat Exchanger1"],
        "Compr" ->STENCILS["PEPUMP_U.vss"]@Masters["Selectable compressor1"],
        "MCompr" ->STENCILS["PEPUMP_U.vss"]@Masters["Selectable compressor1"],
        "Pump" -> STENCILS["PEPUMP_U.vss"]@Masters["Centrifugal pump"],
        "Sep" -> STENCILS["PEVESS_U.vss"]@Masters["Vessel"],
        "RStoic" ->STENCILS["PEVESS_U.vss"]@Masters["Reaction vessel"],
        "RYield" -> STENCILS["PEVESS_U.vss"]@Masters["Reaction vessel"],
        "REquil" -> STENCILS["PEVESS_U.vss"]@Masters["Reaction vessel"],
        "RGibbs" -> STENCILS["PEVESS_U.vss"]@Masters["Reaction vessel"],
        "RCSTR" -> STENCILS["PEVESS_U.vss"]@Masters["Reaction vessel"],
        "Rplug" -> STENCILS["PEVESS_U.vss"]@Masters["Reaction vessel"],
        "RBatch" -> STENCILS["PEVESS_U.vss"]@Masters["Reaction vessel"],
        "DSTWU" -> STENCILS["PEVESS_U.vss"]@Masters["Tray column"] ,
        "Distl" -> STENCILS["PEVESS_U.vss"]@Masters["Tray column"] ,
        "RadFrac" -> STENCILS["PEVESS_U.vss"]@Masters["Tray column"] ,
        "Extract" -> STENCILS["PEVESS_U.vss"]@Masters["Tray column"] ,
        "Valve" -> STENCILS["PEVALV_U.vss"]@Masters["Globe valve"] ,
        "FSplit" -> STENCILS["PEVALV_U.vss"]@Masters["Globe valve"]|>;
    ]

AddLineData[line_, stream_, num_: 0] := Module[{index},
  index = 0;
  KeyValueMap[(
           
     intPropRow2 = 
      line@AddRow[ENUMS@visSectionProp, ENUMS@visRowLast, 
        ENUMS@visTagDefault];
       line@
       CellsSRC[ENUMS@visSectionProp, intPropRow2, 
         ENUMS@visCustPropsLabel]@FormulaU = 
      "\"\"\"" <> ToString@#1 <> "\"\"\"";
        line@CellsSRC[ENUMS@visSectionProp, index, 0]@FormulaU = 
      If[NumericQ@#2, ToString@AccountingForm@#2, 
       "\"\"\"" <> ToString@#2 <> "\"\"\""];
     index = index + 1;) &,
   stream];
  ]

Connect["", to_, label_, num_: 0, placedShapes_] := 
    Module[{line1, line2, anchor, shp},
        shp = VISIO@ActivePage[]@DrawRectangle[1, 1.25, 2.5, 1];
        shp@Text = ToString[label];
        shp@CellsSRC[ENUMS@visSectionObject, ENUMS@visRowLine, 
            ENUMS@visLinePattern]@FormulaU = "0";
        shp@CellsSRC[ENUMS@visSectionObject, ENUMS@visRowLine, 
            ENUMS@visFillPattern]@FormulaU = "0";

        line1 = VISIO@ActivePage[]@Drop[VISIO@ConnectorToolDataObject, 0, 0];
        line1@CellsU["BeginX"]@GlueTo[shp@CellsU["PinX"]];
        line1@CellsU["EndX"]@GlueTo[placedShapes[to]@CellsU["PinX"]];
        line1@CellsU["EndArrow"]@FormulaU = "5";
        line1@Text = ToString@num;
        line1
    ]

Connect[from_, "", label_, num_: 0, placedShapes_] := 
    Module[{line1, line2, anchor, shp},
        shp = VISIO@ActivePage[]@DrawRectangle[1, 1.25, 2.5, 1];
        shp@Text = ToString[label];
        shp@CellsSRC[ENUMS@visSectionObject, ENUMS@visRowLine, 
            ENUMS@visLinePattern]@FormulaU = "0";
        shp@CellsSRC[ENUMS@visSectionObject, ENUMS@visRowLine, 
            ENUMS@visFillPattern]@FormulaU = "0";
  
        line1 = VISIO@ActivePage[]@Drop[VISIO@ConnectorToolDataObject, 0, 0];
        line1@CellsU["EndX"]@GlueTo[shp@CellsU["PinX"]];
        line1@CellsU["BeginX"]@GlueTo[placedShapes[from]@CellsU["PinX"]];
        line1@CellsU["EndArrow"]@FormulaU = "5";
        line1@Text = ToString@num;
        line1
    ]

Connect[from_, to_, label_, num_: 0, placedShapes_] := 
    Module[{line1, line2, anchor},
        line1 = VISIO@ActivePage[]@Drop[VISIO@ConnectorToolDataObject, 0, 0];
        line1@CellsU["BeginX"]@GlueTo[placedShapes[from]@CellsU["PinX"]];
        line1@CellsU["EndX"]@GlueTo[placedShapes[to]@CellsU["PinX"]];
        line1@CellsU["EndArrow"]@FormulaU = "5";
        line1@Text = ToString@num;
        line1
    ]

PlaceLine =.
PlaceLine[stream_, num_: 0, placedShapes_] := 
    Module[{id, from, to, label, temp, press, massf, type, line1, line2, anchor},
        {label, from, to, type, id} = Values@stream[[{"Name", "From", "To", "Type", "ID"}]];
        line1 = Connect[ from, to, label, id, placedShapes];
        AddLineData[line1, stream, id];
        Switch[type,
                "HEAT",
                 (line1@CellsU["LinePattern"]@Formula = "16");
                ( 
                line1@CellsU["LineColor"]@Formula = "THEMEGUARD(RGB(192,0,0))");,
                 "WORK",
                (line1@CellsU["LinePattern"]@Formula = "4");
                (line1@CellsU["LineColor"]@Formula = "THEMEGUARD(RGB(0,0,192))");
          ];
         line1
    ]
  
PlaceLines[lines_, placedShapes_] := MapIndexed[
    Function[{line, num}, PlaceLine[line, num[[1]], placedShapes]], lines]
    changeLineThickness[line1_, anchor_, line2_] := Module[{},
    normMassF = Rescale[ToExpression[anchor@CellsSRC[ENUMS@visSectionProp, ENUMS@visRowLast, 0]@FormulaU], {Min@mf, Max@mf}];
    line1@CellsSRC[ENUMS@visSectionObject, ENUMS@visRowLine, ENUMS@visLineWeight]@FormulaU = ToString@(normMassF*10) <> " pt";
    line2@CellsSRC[ENUMS@visSectionObject, ENUMS@visRowLine, ENUMS@visLineWeight]@FormulaU = ToString@(normMassF*10) <> " pt";
    ]

AddCallouts[line1_, anchor_, line2_, page_] := Module[{callout},
    callout = page@DropCallout[STENCILS["SDCALL_U.vssx"]@Masters@ItemU["Oval"], anchor];
    callout@Characters@Text = StringTake[
        StringJoin[
        ToString[anchor@CellsSRC[ENUMS@visSectionProp, #, 0]@FormulaU] <>
            "\n" & /@ Range[0, 2]], {1, -2}];
]

PlaceShape[unit_, shapes_] := Module[{},
    If[MemberQ[Keys@shapes, unit[[2]]],
     shp = 
    VISIO@ActivePage[]@
      Drop[shapes[unit[[2]]], RandomReal[{0, 10}], 
       RandomReal[{0, 10}]];
     shp@Text = ToString@unit[[1]];,
     shp = 
    VISIO@ActivePage[]@
      Drop[STENCILS["BASIC_U.vss"]@Masters["Rectangle"], 
       RandomReal[{0, 10}], RandomReal[{0, 10}]];
     shp@Text = ToString@unit[[1]] <> "\n" <> ToString@unit[[2]]];
    unit[[1]] -> shp]

PlaceShapes[units_] := <|(PlaceShape[#, SHAPES] & /@ units)|>

LayoutPage[placeStyle_: "2", routeStyle_: "5", distance_: "0.75 in", lineStyle_: 5] := Module[{},
    VISIO@ActivePage[]@PageSheet@CellsU["PlaceStyle"]@FormulaU = placeStyle;
    VISIO@ActivePage[]@PageSheet@CellsU["RouteStyle"]@FormulaU = routeStyle;
    VISIO@ActivePage[]@PageSheet@CellsSRC[ENUMS@visSectionObject, ENUMS@visRowPageLayout, ENUMS@visPLOAvenueSizeX]@FormulaForceU = distance;
    VISIO@ActivePage[]@PageSheet@CellsSRC[ENUMS@visSectionObject, ENUMS@visRowPageLayout, ENUMS@visPLOAvenueSizeY]@FormulaForceU = distance;
    VISIO@ActivePage[]@PageSheet@Cells["LineJumpStyle"]@ResultIU = lineStyle;
    VISIO@ActivePage[]@Layout[]
]

EndPackage[]