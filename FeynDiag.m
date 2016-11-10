(* ::Package:: *)

(* The FeynDiag Mathematica package generates Feynman diagrams *)
(* Author:  Bhubanjyoti Bhattacharya 
            Copyright (\[Copyright]) 2015 *)


(* Fermion line *)
FL[x1_, y1_, x2_, y2_, pe_, ah_] := Module[{perc, x, y, Ar, Li}, x[perc_] := x1 + (x2 - x1) perc; y[perc_] := y1 + (y2 - y1) perc; Ar = Graphics[{Arrowheads[ah], Arrow[{{x1, y1}, {x[pe], y[pe]}}]}]; Li = Graphics[Line[{{x[pe], y[pe]}, {x2, y2}}]]; Show[Ar, Li]];


(* Curved fermion line *)
CFL[x1_, y1_, xc_, yc_, \[Phi]f_, ah_, r_] := Module[{\[CapitalDelta]x, \[CapitalDelta]y, \[Phi]i, R, \[Phi]1, \[Phi]2, x, y, dxl, dyl, d\[Phi]l, xl, yl, Tt, L1}, \[CapitalDelta]x = x1 - xc; \[CapitalDelta]y = y1 - yc; \[Phi]i = (180/\[Pi] Which[\[CapitalDelta]x > 0, \[Pi] (1 - Sign[\[CapitalDelta]y]) - \[Pi] (1 - Abs[Sign[\[CapitalDelta]y]])+ ArcTan[\[CapitalDelta]y/\[CapitalDelta]x], \[CapitalDelta]x < 0, \[Pi] + ArcTan[\[CapitalDelta]y/\[CapitalDelta]x], \[CapitalDelta]x == 0 && \[CapitalDelta]y > 0, \[Pi]/2, \[CapitalDelta]x == 0 && \[CapitalDelta]y < 0, (3 \[Pi])/2])//N; R = \[Sqrt](\[CapitalDelta]x^2 + \[CapitalDelta]y^2); \[Phi]1 = \[Phi]i; \[Phi]2 = If[\[Phi]f > \[Phi]1, \[Phi]f, 360 + \[Phi]f]; x[i_] := xc + R Cos[(\[Pi] i)/180]; y[i_] := yc + R Sin[(\[Pi] i)/180]; Tt = Table[{x[i], y[i]}, {i, \[Phi]1, \[Phi]2, 1/10}]; Tt = If[r == 1, Reverse[Tt], Tt]; Graphics[{Arrowheads[ah], Arrow[Reverse[Tt]]}]];


(* Scalar line *)
SL[x1_, y1_, x2_, y2_] := Graphics[{Dashed, Line[{{x1, y1}, {x2, y2}}]}];


(* Curved scalar line *)
CSL[x1_, y1_, xc_, yc_, \[Phi]f_] := Module[{\[CapitalDelta]x, \[CapitalDelta]y, \[Phi]i, R, \[Phi]1, \[Phi]2, x, y, dxl, dyl, d\[Phi]l, xl, yl, Tt, L1}, \[CapitalDelta]x = x1 - xc; \[CapitalDelta]y = y1 - yc; \[Phi]i = (180/\[Pi] Which[\[CapitalDelta]x > 0, \[Pi] (1 - Sign[\[CapitalDelta]y]) - \[Pi] (1 - Abs[Sign[\[CapitalDelta]y]])+ ArcTan[\[CapitalDelta]y/\[CapitalDelta]x], \[CapitalDelta]x < 0, \[Pi] + ArcTan[\[CapitalDelta]y/\[CapitalDelta]x], \[CapitalDelta]x == 0 && \[CapitalDelta]y > 0, \[Pi]/2, \[CapitalDelta]x == 0 && \[CapitalDelta]y < 0, (3 \[Pi])/2])//N; R = \[Sqrt](\[CapitalDelta]x^2 + \[CapitalDelta]y^2); \[Phi]1 = \[Phi]i; \[Phi]2 = If[\[Phi]f > \[Phi]1, \[Phi]f, 360 + \[Phi]f]; x[i_] := xc + R Cos[(\[Pi] i)/180]; y[i_] := yc + R Sin[(\[Pi] i)/180]; Tt = Table[{x[i], y[i]}, {i, \[Phi]1, \[Phi]2, 1/10}]; L1 = ListLinePlot[Tt, Axes -> False, AspectRatio -> 1, PlotStyle -> {Dotted, RGBColor[0,0,0]}]];


(* Photon/Electro-weak-gauge-boson line *)
PL[x1_, y1_, x2_, y2_, NN_, AA_, p_] := Module[{DelX, DelY, x, y, \[Phi], xx, yy, Dxloc, Dyloc, Tt}, DelX = x2 - x1; DelY = y2 - y1; \[Phi] = If[DelX == 0, \[Pi]/2, ArcTan[DelY/DelX]]; Dxloc[i_] := (DelX Cos[\[Phi]] + DelY Sin[\[Phi]]) i/NN; Dyloc[i_] := p AA Sin[2 \[Pi] i]; xx[i_] := x1 + Dxloc[i] Cos[\[Phi]] - Dyloc[i] Sin[\[Phi]];
yy[i_] := y1 + Dyloc[i] Cos[\[Phi]] + Dxloc[i] Sin[\[Phi]]; Tt = Table[{xx[i], yy[i]}, {i, 0, NN, 1/24}]; ListLinePlot[Tt, AspectRatio -> 1, PlotStyle -> RGBColor[0,0,0], Axes -> False]];


(* Curved photon/electro-weak-gauge-boson line *)
CPL[x1_, y1_, xc_, yc_, \[Phi]f_, \[Phi]s_, AA_] := Module[{\[CapitalDelta]x, \[CapitalDelta]y, \[Phi]i, R, \[Phi]1, \[Phi]2, x, y, dxl, dyl, d\[Phi]l, xl, yl, Tt, L1}, \[CapitalDelta]x = x1 - xc; \[CapitalDelta]y = y1 - yc; \[Phi]i = (180/\[Pi] Which[\[CapitalDelta]x > 0, \[Pi] (1 - Sign[\[CapitalDelta]y]) - \[Pi] (1 - Abs[Sign[\[CapitalDelta]y]])+ ArcTan[\[CapitalDelta]y/\[CapitalDelta]x], \[CapitalDelta]x < 0, \[Pi] + ArcTan[\[CapitalDelta]y/\[CapitalDelta]x], \[CapitalDelta]x == 0 && \[CapitalDelta]y > 0, \[Pi]/2, \[CapitalDelta]x == 0 && \[CapitalDelta]y < 0, (3 \[Pi])/2])//N; R = \[Sqrt](\[CapitalDelta]x^2 + \[CapitalDelta]y^2); \[Phi]1 = \[Phi]i; \[Phi]2 = If[\[Phi]f > \[Phi]1, \[Phi]f, 360 + \[Phi]f]; x[i_] := xc + R Cos[(\[Pi] i)/180]; y[i_] := yc + R Sin[(\[Pi] i)/180]; dxl[i_]:= x[i + \[Phi]s/10] - x[i];dyl[i_] := - AA Sin[(2 \[Pi] i)/\[Phi]s]; d\[Phi]l[i_] := \[Pi]/180 Mod[90 - i, 360]; xl[i_] := dxl[i] Cos[d\[Phi]l[i]] + dyl[i] Sin[d\[Phi]l[i]]; yl[i_] := dyl[i] Cos[d\[Phi]l[i]] - dxl[i] Sin[d\[Phi]l[i]]; Tt = Table[{x[i] + xl[i], y[i] + yl[i]}, {i, \[Phi]1, \[Phi]2, \[Phi]s/10}]; L1 = ListLinePlot[Tt, PlotStyle -> RGBColor[0,0,0], Axes -> False, AspectRatio -> 1]];


(* Gluon line *)
GL[x1_, y1_, x2_, y2_, NN_, AA_, p_] := Module[{DelX, DelY, \[Phi], d, sf, \[Theta]max, imax, xp, yp, xx, yy, ang, Tt}, DelX = x2 - x1; DelY = y2 - y1; ang = \[Pi]/100; \[Phi] = If[DelX == 0, \[Pi]/2, ArcTan[DelY/DelX]]; d = Sqrt[DelX^2 + DelY^2] - 2 AA; \[Theta]max = (2 NN - 1) \[Pi]; sf = ang/\[Theta]max; imax = Floor[\[Theta]max/ang + 1]; xp[i_] := d sf i + AA (1 - Cos[ang (i - 1)]); yp[i_] := p AA Sin[ang (i - 1)]; xx[i_] := x1 + xp[i] Cos[\[Phi]] - yp[i] Sin[\[Phi]];
yy[i_] := y1 + yp[i] Cos[\[Phi]] + xp[i] Sin[\[Phi]]; Tt = Table[{xx[i], yy[i]}, {i, 1, imax}]; ListLinePlot[Tt, AspectRatio -> 1, PlotStyle -> RGBColor[0,0,0], Axes -> False]];


(* Curved photon/electro-weak-gauge-boson line : Development mode
CGL[x1_, y1_, xc_, yc_, \[Phi]f_, \[Phi]s_, AA_] := Module[{\[CapitalDelta]x, \[CapitalDelta]y, \[Phi]i, R, \[Phi]1, \[Phi]2, x, y, dxl, dyl, d\[Phi]l, xl, yl, Tt, L1}, \[CapitalDelta]x = x1 - xc; \[CapitalDelta]y = y1 - yc; \[Phi]i = (180/\[Pi] Which[\[CapitalDelta]x > 0, \[Pi] (1 - Sign[\[CapitalDelta]y]) - \[Pi] (1 - Abs[Sign[\[CapitalDelta]y]])+ ArcTan[\[CapitalDelta]y/\[CapitalDelta]x], \[CapitalDelta]x < 0, \[Pi] + ArcTan[\[CapitalDelta]y/\[CapitalDelta]x], \[CapitalDelta]x == 0 && \[CapitalDelta]y > 0, \[Pi]/2, \[CapitalDelta]x == 0 && \[CapitalDelta]y < 0, (3 \[Pi])/2])//N; R = \[Sqrt](\[CapitalDelta]x^2 + \[CapitalDelta]y^2); \[Phi]1 = \[Phi]i; \[Phi]2 = If[\[Phi]f > \[Phi]1, \[Phi]f, 360 + \[Phi]f]; x[i_] := xc + R Cos[(\[Pi] i)/180]; y[i_] := yc + R Sin[(\[Pi] i)/180]; dxl[i_]:= x[i + \[Phi]s/10] - x[i];dyl[i_] := - AA Sin[(2 \[Pi] i)/\[Phi]s]; d\[Phi]l[i_] := \[Pi]/180 Mod[90 - i, 360]; xl[i_] := dxl[i] Cos[d\[Phi]l[i]] + dyl[i] Sin[d\[Phi]l[i]]; yl[i_] := dyl[i] Cos[d\[Phi]l[i]] - dxl[i] Sin[d\[Phi]l[i]]; Tt = Table[{x[i] + xl[i], y[i] + yl[i]}, {i, \[Phi]1, \[Phi]2, \[Phi]s/10}]; L1 = ListLinePlot[Tt, PlotStyle -> RGBColor[0,0,0], Axes -> False, AspectRatio \[Rule] 1]];*)


(* Plus circle *)
PlCirc[x_, y_, r_, \[Theta]1_] := Module[{x1, x2, y1, y2, x3, x4, y3, y4, \[Theta], \[Theta]p}, \[Theta] = (\[Pi] \[Theta]1)/180; x1 = x - r Cos[\[Theta]]; x2 = x + r Cos[\[Theta]]; y1 = y + r Sin[\[Theta]]; y2 = y - r Sin[\[Theta]]; \[Theta]p = \[Theta] + \[Pi]/2; x3 = x - r Cos[\[Theta]p]; x4 = x + r Cos[\[Theta]p]; y3 = y + r Sin[\[Theta]p]; y4 = y - r Sin[\[Theta]p]; Show[Graphics[Circle[{x, y}, r]], Graphics[Line[{{x1, y1}, {x2, y2}}]], Graphics[Line[{{x3, y3}, {x4, y4}}]]]];


(* Text *)
Tx[x1_, y1_, tx_, fs_] := Graphics[Style[Text[tx, {x1, y1}], FontSize -> fs, FontFamily -> "Times New Roman"]];


Print["Welcome to FeynDiag 1.0! Please follow these directives :"]
Print["\!\(\*SubscriptBox[\(x\), \(i, f, c\)]\) = Initial, Final, Center (curved) x coordinate;"]
Print["\!\(\*SubscriptBox[\(y\), \(i, f, c\)]\) = Initial, Final, Center (curved) y coordinate;"]
Print["A = Oscillation amplitude; N = Number of oscillations;"]
Print["\!\(\*SubscriptBox[\(a\), \(dir\)]\) = Arrowhead direction : (Counter) clockwise : (-)1; "]
Print["\!\(\*SubscriptBox[\(p\), \(dir\)]\) = Pitch direction : (Counter) clockwise : (-)1; "]
Print["___________________________________________________________"]
Print["Fermion line : FL[\!\(\*SubscriptBox[\(x\), \(i\)]\), \!\(\*SubscriptBox[\(y\), \(i\)]\), \!\(\*SubscriptBox[\(x\), \(f\)]\), \!\(\*SubscriptBox[\(y\), \(f\)]\), arrowhead position, arrowhead size];"]
Print["Curved fermion line : CFL[\!\(\*SubscriptBox[\(x\), \(i\)]\), \!\(\*SubscriptBox[\(y\), \(i\)]\), \!\(\*SubscriptBox[\(x\), \(c\)]\), \!\(\*SubscriptBox[\(y\), \(c\)]\), final angle, arrowhead size, \!\(\*SubscriptBox[\(a\), \(dir\)]\)]"]
Print["Scalar line : SL[\!\(\*SubscriptBox[\(x\), \(i\)]\), \!\(\*SubscriptBox[\(y\), \(i\)]\), \!\(\*SubscriptBox[\(x\), \(f\)]\), \!\(\*SubscriptBox[\(y\), \(f\)]\)]; Curved scalar line : CSL[\!\(\*SubscriptBox[\(x\), \(i\)]\), \!\(\*SubscriptBox[\(y\), \(i\)]\), \!\(\*SubscriptBox[\(x\), \(c\)]\), \!\(\*SubscriptBox[\(y\), \(c\)]\), final angle]"]
Print["Photon/Electro-weak-gauge-boson line : PL[\!\(\*SubscriptBox[\(x\), \(i\)]\), \!\(\*SubscriptBox[\(y\), \(i\)]\), \!\(\*SubscriptBox[\(x\), \(f\)]\), \!\(\*SubscriptBox[\(y\), \(f\)]\), N, A, \!\(\*SubscriptBox[\(p\), \(dir\)]\)];"]
Print["Curved Photon/Electro-weak-gauge-boson line : CPL[\!\(\*SubscriptBox[\(x\), \(i\)]\), \!\(\*SubscriptBox[\(y\), \(i\)]\), \!\(\*SubscriptBox[\(x\), \(c\)]\), \!\(\*SubscriptBox[\(y\), \(c\)]\), final angle, angular sensitivity, A]"]
Print["Gluon line : GL[\!\(\*SubscriptBox[\(x\), \(i\)]\), \!\(\*SubscriptBox[\(y\), \(i\)]\), \!\(\*SubscriptBox[\(x\), \(f\)]\), \!\(\*SubscriptBox[\(y\), \(f\)]\), N, A, \!\(\*SubscriptBox[\(p\), \(dir\)]\)];"] (* Curved Photon/Electro-weak-gauge-boson line : CPL[Subscript[x, i], Subscript[y, i], Subscript[x, c], Subscript[y, c], final angle, angular sensitivity, A]"] *)
Print["Text : Tx[\!\(\*SubscriptBox[\(x\), \(c\)]\), \!\(\*SubscriptBox[\(y\), \(c\)]\), text, fontsize]; Crosswire : PlCirc[\!\(\*SubscriptBox[\(x\), \(c\)]\), \!\(\*SubscriptBox[\(y\), \(c\)]\), radius, angle for cross]"]
