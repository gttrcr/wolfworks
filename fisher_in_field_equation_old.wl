 ::Package:: 

ClearAll["Global`*"]
(*ResourceFunction["DarkMode"][]*)
<<"TensoriaCalc.m"//Quiet
VarName[var_, index_]:=ToExpression[StringJoin[ToString[var], ToString[index]]]
Par[index_]:=VarName[\[Theta], index] (*parameters, parametric space*)
Sup[index_]:=VarName[X, index] (*support*)
PP[index_]:=VarName[pp, index] (*just an utility variable*)
(*L[index_]:=VarName[L, index]*)
PrintT[tensor_]:=tensor//TensorComponents//MatrixForm
RemoveAbs[f_]:=ComplexExpand[Abs[f]]
DefaultSimplify[f_]:=FullSimplify[f, Assumptions->_\[Element]PositiveReals]
Unprotect[Abs, Arg];
Abs[x_]:=x
Arg[x_]:=0
Protected[Abs, Arg];

Universe[f_]:=With[{parameters=Flatten[StringCases[ToString[#]&/@DeleteDuplicates[Cases[f, _Symbol, \[Infinity]]], RegularExpression["\[Theta][0-9]+"]]], supports=Flatten[StringCases[ToString[#]&/@DeleteDuplicates[Cases[f, _Symbol, \[Infinity]]], RegularExpression["X[0-9]+"]]], levels=Flatten[StringCases[ToString[#]&/@DeleteDuplicates[Cases[f, _Symbol, \[Infinity]]], RegularExpression["L[0-9]+"]]]},
{Length[parameters], Length[supports], Length[levels]}
]

derivativeProduct[f_, coordinates_]:=Table[D[f, Par[i]] D[f, Par[j]], {i, 1, coordinates}, {j, 1, coordinates}]
gMetric[p_]:=With[{spacesize=Length[p], coordinates=Universe[p][[1]]}, Sum[derivativeProduct[p[[k]], coordinates], {k, 1, spacesize}]]
FIT[p_]:=With[{spacesize=Length[p], coordinates=Universe[p][[1]]}, Sum[derivativeProduct[p[[k]], coordinates]/p[[k]], {k, 1, spacesize}]]
\[Mu][p_]:=1/Total[1/4 p^2]
FITgMetric[p_]:=-derivativeProduct[\[Mu][p], Universe[p][[1]]]/\[Mu][p]^2+\[Mu][p]gMetric[p]
MinorFITgMetric[p_]:=With[{eigen=FITgMetric[p]//Eigenvalues}, DeleteCases[eigen, 0]//DiagonalMatrix]
JacobianMatrix[p_]:=With[{spacesize=Length[p],coordinates=Universe[p][[1]]}, Table[D[p[[i]], Par[j]], {i, 1, spacesize}, {j, 1, coordinates}]]


p=CoordinateTransform["Spherical"->"Cartesian",{Par[1], Par[2], Par[3]}]
JacobianMatrix[p]//DefaultSimplify//MatrixForm
gMetric[p]//DefaultSimplify//Eigenvalues//DiagonalMatrix//MatrixForm
FITgMetric[p]//DefaultSimplify//Eigenvalues//DiagonalMatrix//MatrixForm
MinorFITgMetric[p]//DefaultSimplify//Eigenvalues//DiagonalMatrix//MatrixForm
%//Det//DefaultSimplify


\!\(\*
TagBox[
RowBox[{"(", "", GridBox[{
{"1", "0", "0"},
{"0", 
SuperscriptBox["\[Theta]1", "2"], "0"},
{"0", "0", 
RowBox[{
SuperscriptBox["\[Theta]1", "2"], " ", 
SuperscriptBox[
RowBox[{"Sin", "[", "\[Theta]2", "]"}], "2"]}]}
},
GridBoxAlignment->{"Columns" -> {{Center}}, "Rows" -> {{Baseline}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.7]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}], "", ")"}],
Function[BoxForm`e$, MatrixForm[BoxForm`e$]]]\);
%//MatrixForm
CholeskyDecomposition[%%]//DefaultSimplify;
%//Eigenvalues//DefaultSimplify;
newp=Table[Integrate[%[[i]], Par[i]], {i, 1, Length[%]}]//ComplexExpand//DefaultSimplify
JacobianMatrix[2 newp/Sqrt[Total[newp^2]]]//DefaultSimplify//MatrixForm
(*gMetric[newp]//DefaultSimplify//MatrixForm;*)
(*4 gMetric[newp/Sqrt[Total[newp^2]]]//DefaultSimplify//MatrixForm*)
FITgMetric[newp]//DefaultSimplify//MatrixForm;
MinorFITgMetric[newp]//Det//DefaultSimplify


{\[Theta]1,\[Theta]1 \[Theta]2,\[Theta]1 \[Theta]3 Sin[\[Theta]2]}
(%/Sqrt[Total[%^2]])^(2)
JacobianMatrix[%]//DefaultSimplify//MatrixForm
Times@@DeleteCases[%//Eigenvalues, 0]//DefaultSimplify


\!\(\*
TagBox[
RowBox[{"(", "", GridBox[{
{"0", 
RowBox[{"-", 
FractionBox[
RowBox[{"2", " ", 
RowBox[{"(", 
RowBox[{"\[Theta]2", "+", 
RowBox[{
SuperscriptBox["\[Theta]3", "2"], " ", 
RowBox[{"Cos", "[", "\[Theta]2", "]"}], " ", 
RowBox[{"Sin", "[", "\[Theta]2", "]"}]}]}], ")"}]}], 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
SuperscriptBox["\[Theta]2", "2"], "+", 
RowBox[{
SuperscriptBox["\[Theta]3", "2"], " ", 
SuperscriptBox[
RowBox[{"Sin", "[", "\[Theta]2", "]"}], "2"]}]}], ")"}], 
RowBox[{"3", "/", "2"}]]]}], 
RowBox[{"-", 
FractionBox[
RowBox[{"2", " ", "\[Theta]3", " ", 
SuperscriptBox[
RowBox[{"Sin", "[", "\[Theta]2", "]"}], "2"]}], 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
SuperscriptBox["\[Theta]2", "2"], "+", 
RowBox[{
SuperscriptBox["\[Theta]3", "2"], " ", 
SuperscriptBox[
RowBox[{"Sin", "[", "\[Theta]2", "]"}], "2"]}]}], ")"}], 
RowBox[{"3", "/", "2"}]]]}]},
{"0", 
FractionBox[
RowBox[{"2", "+", 
RowBox[{"2", " ", 
SuperscriptBox["\[Theta]3", "2"], " ", 
RowBox[{"Sin", "[", "\[Theta]2", "]"}], " ", 
RowBox[{"(", 
RowBox[{
RowBox[{
RowBox[{"-", "\[Theta]2"}], " ", 
RowBox[{"Cos", "[", "\[Theta]2", "]"}]}], "+", 
RowBox[{"Sin", "[", "\[Theta]2", "]"}]}], ")"}]}]}], 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
SuperscriptBox["\[Theta]2", "2"], "+", 
RowBox[{
SuperscriptBox["\[Theta]3", "2"], " ", 
SuperscriptBox[
RowBox[{"Sin", "[", "\[Theta]2", "]"}], "2"]}]}], ")"}], 
RowBox[{"3", "/", "2"}]]], 
RowBox[{"-", 
FractionBox[
RowBox[{"2", " ", "\[Theta]2", " ", "\[Theta]3", " ", 
SuperscriptBox[
RowBox[{"Sin", "[", "\[Theta]2", "]"}], "2"]}], 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
SuperscriptBox["\[Theta]2", "2"], "+", 
RowBox[{
SuperscriptBox["\[Theta]3", "2"], " ", 
SuperscriptBox[
RowBox[{"Sin", "[", "\[Theta]2", "]"}], "2"]}]}], ")"}], 
RowBox[{"3", "/", "2"}]]]}]},
{"0", 
FractionBox[
RowBox[{"2", " ", "\[Theta]3", " ", 
RowBox[{"(", 
RowBox[{
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
SuperscriptBox["\[Theta]2", "2"]}], ")"}], " ", 
RowBox[{"Cos", "[", "\[Theta]2", "]"}]}], "-", 
RowBox[{"\[Theta]2", " ", 
RowBox[{"Sin", "[", "\[Theta]2", "]"}]}]}], ")"}]}], 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
SuperscriptBox["\[Theta]2", "2"], "+", 
RowBox[{
SuperscriptBox["\[Theta]3", "2"], " ", 
SuperscriptBox[
RowBox[{"Sin", "[", "\[Theta]2", "]"}], "2"]}]}], ")"}], 
RowBox[{"3", "/", "2"}]]], 
FractionBox[
RowBox[{"2", " ", 
RowBox[{"(", 
RowBox[{"1", "+", 
SuperscriptBox["\[Theta]2", "2"]}], ")"}], " ", 
RowBox[{"Sin", "[", "\[Theta]2", "]"}]}], 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
SuperscriptBox["\[Theta]2", "2"], "+", 
RowBox[{
SuperscriptBox["\[Theta]3", "2"], " ", 
SuperscriptBox[
RowBox[{"Sin", "[", "\[Theta]2", "]"}], "2"]}]}], ")"}], 
RowBox[{"3", "/", "2"}]]]}
},
GridBoxAlignment->{"Columns" -> {{Center}}, "Rows" -> {{Baseline}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.7]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}], "", ")"}],
Function[BoxForm`e$, MatrixForm[BoxForm`e$]]]\);
Transpose[%] . \!\(\*
TagBox[
RowBox[{"(", "", GridBox[{
{"1", "0", "0"},
{"0", "1", "0"},
{"0", "0", "1"}
},
GridBoxAlignment->{"Columns" -> {{Center}}, "Rows" -> {{Baseline}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.7]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}], "", ")"}],
Function[BoxForm`e$, MatrixForm[BoxForm`e$]]]\) . %//DefaultSimplify;
DeleteCases[%//Eigenvalues, 0]//DiagonalMatrix//Det//DefaultSimplify


(*Get the J matrix for a given transformation p*)
p={Par[1] Sin[Par[2]] Cos[Par[3]], Tan[Par[3]], 1/(Tan[Par[2]] Cos[Par[3]] Sin[Tan[Par[3]]])};
pars=Table[Par[i], {i, 1, Universe[p][[1]]}];
Table[PP[i]==p[[i]], {i, 1, Length[p]}];
Solve[%, pars]//Quiet//Last;
pars/.%//Normal;
ReplaceAll[%, Table[PP[i]->Par[i], {i, 1, Universe[p][[1]]}]]
JacobianMatrix[%]//DefaultSimplify//MatrixForm


BuildAll[p_]:=
( 
{
gMetric[p],
JacobianMatrix[p],
FITgMetric[p],
JacobianMatrix[2 p/Sqrt[Total[p^2]]]
}
)

sc=CoordinateTransform["Spherical"->"Cartesian",{Par[1], Par[2], Par[3]}]
SC=BuildAll[sc];
uc={\[Theta]1,\[Theta]1 \[Theta]2,\[Theta]1 \[Theta]3 Sin[\[Theta]2]}
UC=BuildAll[uc];
su={Par[1] Sin[Par[2]] Cos[Par[3]], Tan[Par[3]], 1/(Tan[Par[2]] Cos[Par[3]] Sin[Tan[Par[3]]])}
SU=BuildAll[su];


Transpose[SC[[2]]] . SC[[2]]//DefaultSimplify ;
(SC[[1]]//DefaultSimplify)==%
Transpose[SC[[4]]] . SC[[4]]//DefaultSimplify;
(SC[[3]]//DefaultSimplify)==%
Transpose[UC[[2]]] . UC[[2]]//DefaultSimplify;
(UC[[1]]//DefaultSimplify)==%
Transpose[UC[[4]]] . UC[[4]]//DefaultSimplify;
(UC[[3]]//DefaultSimplify)==%


