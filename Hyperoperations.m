(* ::Package:: *)

(* :Name: Hyperoperations` *)

(* :Title: Hyperoperations *)

(* :Author: Andy Groeneveld *)

(* :Summary:
 A small collection of hyperoperations and related functions.
 Hyperoperations and hyperlogarithms of rank 0 through 4 are
 fully supported;  hyper-roots are fully supported through
 rank 3.  Only the square super-root is available.  Symbolic
 manipulation of other ranks is supported.

 Other hyperoperation sequences are included:
  -Bennett's commutative hyperoperations
  -Frappier's balanced hyperoperations
  -Goodstein's G function
  -Grzegorczyk's f_k (related to the Grzegorczyk Hierarchy)
*)

(* :Context: Hyperoperations` *)

(* :Package Version: 0.1 *)

(* :Copyright: Copyright (c) 2012, Andy Groeneveld.  All rights reserved. *)

(* License Information:

 Redistribution and use in source and binary forms, with or
 without modification, are permitted provided that the following
 conditions are met:

  -Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.
  -Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.
  -Neither the name of Michigan Technological University nor the names of its
   contributors may be used to endorse or promote products derived from this
   software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 POSSIBILITY OF SUCH DAMAGE.

*)

(* :History:
 17 Jul 2012   Version 0.1
*)

(* :Keywords:
 zerate, zeration, tetrate, tetration, hyperoperation, hyperlogarithm,
 hyper-log, hyper-root, Goodstein, Grzegorczyk, super-root, superlogarithm
*)

(* :Mathematica Version: 8.0 *)

BeginPackage["Hyperoperations`"];

Zerate::usage = \!\(\*
TagBox[
StyleBox["\"\<\\!\\(\\*RowBox[{\\\"Zerate\\\", \\\"[\\\", StyleBox[\\\"x\\\", \\\"TI\\\"], \\\",\\\", StyleBox[\\\"y\\\", \\\"TI\\\"], \\\"]\\\"}]\\) gives \\!\\(\\*StyleBox[\\\"x\\\", \\\"TI\\\"]\\) zerated by \\!\\(\\*StyleBox[\\\"y\\\", \\\"TI\\\"]\\) using Rubtsov and Romerio's definition. \>\"",
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\);
TrappmannZerate::usage = \!\(\*
TagBox[
StyleBox["\"\<\\!\\(\\*RowBox[{\\\"Zerate\\\", \\\"[\\\", StyleBox[\\\"x\\\", \\\"TI\\\"], \\\",\\\", StyleBox[\\\"y\\\", \\\"TI\\\"], \\\"]\\\"}]\\) gives \\!\\(\\*StyleBox[\\\"x\\\", \\\"TI\\\"]\\) zerated by \\!\\(\\*StyleBox[\\\"y\\\", \\\"TI\\\"]\\) using Trappmann's alternate definition. \>\"",
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\);
Delta::usage = \!\(\*
TagBox[
StyleBox["\"\<\\!\\(\\*RowBox[{\\\"Delta\\\", \\\"[\\\", StyleBox[\\\"x\\\", \\\"TI\\\"], \\\"]\\\"}]\\) gives the delta number (not a differential) \\!\\(\\*StyleBox[\\\"\\[CapitalDelta]x\\\", \\\"TI\\\"]\\). \>\"",
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\);
Deltate::usage = \!\(\*
TagBox[
StyleBox["\"\<\\!\\(\\*RowBox[{\\\"Deltate\\\", \\\"[\\\", StyleBox[\\\"x\\\", \\\"TI\\\"], \\\",\\\", StyleBox[\\\"y\\\", \\\"TI\\\"], \\\"]\\\"}]\\) gives \\!\\(\\*StyleBox[\\\"x\\\", \\\"TI\\\"]\\) deltated by \\!\\(\\*StyleBox[\\\"y\\\", \\\"TI\\\"]\\). \>\"",
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\);

Tower::usage = \!\(\*
TagBox[
StyleBox["\"\<\\!\\(\\*RowBox[{\\\"Tower\\\", \\\"[\\\", StyleBox[\\\"x\\\", \\\"TI\\\"], \\\",\\\", StyleBox[\\\"n\\\", \\\"TI\\\"], \\\",\\\", StyleBox[\\\"s\\\", \\\"TI\\\"], \\\"]\\\"}]\\) gives a power tower of \\!\\(\\*StyleBox[\\\"n\\\", \\\"TI\\\"]\\) copies of \\!\\(\\*StyleBox[\\\"x\\\", \\\"TI\\\"]\\).  The topmost exponent \\!\\(\\*StyleBox[\\\"s\\\", \\\"TI\\\"]\\) is optional and defaults to 1. \>\"",
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\);
TowerMod::usage = \!\(\*
TagBox[
StyleBox["\"\<\\!\\(\\*RowBox[{\\\"TowerMod\\\", \\\"[\\\", StyleBox[\\\"x\\\", \\\"TI\\\"], \\\",\\\", StyleBox[\\\"n\\\", \\\"TI\\\"], \\\",\\\", StyleBox[\\\"s\\\", \\\"TI\\\"],  \\\",\\\", StyleBox[\\\"m\\\", \\\"TI\\\"], \\\"]\\\"}]\\) gives a power tower of \\!\\(\\*StyleBox[\\\"n\\\", \\\"TI\\\"]\\) copies of \\!\\(\\*StyleBox[\\\"x\\\", \\\"TI\\\"]\\) evaluated using exponentiation modulo \\!\\(\\*StyleBox[\\\"m\\\", \\\"TI\\\"]\\).  The topmost exponent \\!\\(\\*StyleBox[\\\"s\\\", \\\"TI\\\"]\\) is optional and defaults to 1. \>\"",
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\);
Tetrate::usage = \!\(\*
TagBox[
StyleBox["\"\<\\!\\(\\*RowBox[{\\\"Tetrate\\\", \\\"[\\\", StyleBox[\\\"x\\\", \\\"TI\\\"], \\\",\\\", StyleBox[\\\"y\\\", \\\"TI\\\"], \\\"]\\\"}]\\) gives \\!\\(\\*StyleBox[\\\"x\\\", \\\"TI\\\"]\\) tetrated by \\!\\(\\*StyleBox[\\\"y\\\", \\\"TI\\\"]\\). \>\"",
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\);
TetrateDigits::usage = \!\(\*
TagBox[
StyleBox["\"\<\\!\\(\\*RowBox[{\\\"TetrateDigits\\\", \\\"[\\\", StyleBox[\\\"x\\\", \\\"TI\\\"], \\\",\\\", StyleBox[\\\"y\\\", \\\"TI\\\"], \\\",\\\", StyleBox[\\\"n\\\", \\\"TI\\\"], \\\"]\\\"}]\\) gives the \\!\\(\\*StyleBox[\\\"n\\\", \\\"TI\\\"]\\) rightmost digits of \\!\\(\\*StyleBox[\\\"x\\\", \\\"TI\\\"]\\) tetrated by \\!\\(\\*StyleBox[\\\"y\\\", \\\"TI\\\"]\\). \>\"",
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\);

SuperLog::usage = \!\(\*
TagBox[
StyleBox["\"\<\\!\\(\\*RowBox[{\\\"SuperLog\\\", \\\"[\\\", StyleBox[\\\"x\\\", \\\"TI\\\"], \\\",\\\", StyleBox[\\\"y\\\", \\\"TI\\\"], \\\"]\\\"}]\\) gives the base \\!\\(\\*StyleBox[\\\"y\\\", \\\"TI\\\"]\\) super-logarithm of \\!\\(\\*StyleBox[\\\"x\\\", \\\"TI\\\"]\\). \>\"",
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\);
SuperSqrt::usage = \!\(\*
TagBox[
StyleBox["\"\<\\!\\(\\*RowBox[{\\\"SuperSqrt\\\", \\\"[\\\", StyleBox[\\\"x\\\", \\\"TI\\\"], \\\"]\\\"}]\\) gives the square super-root of \\!\\(\\*StyleBox[\\\"x\\\", \\\"TI\\\"]\\). \>\"",
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\);
SuperRoot::usage = \!\(\*
TagBox[
StyleBox["\"\<\\!\\(\\*RowBox[{\\\"SuperRoot\\\", \\\"[\\\", StyleBox[\\\"x\\\", \\\"TI\\\"], \\\",\\\", StyleBox[\\\"y\\\", \\\"TI\\\"], \\\"]\\\"}]\\) gives the \\!\\(\\*StyleBox[\\\"y\\\", \\\"TI\\\"]\\)-th super-root of \\!\\(\\*StyleBox[\\\"x\\\", \\\"TI\\\"]\\). \>\"",
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\);

GoodsteinG::usage = \!\(\*
TagBox[
StyleBox["\"\<\\!\\(\\*RowBox[{\\\"GoodsteinG\\\", \\\"[\\\", StyleBox[\\\"k\\\", \\\"TI\\\"], \\\",\\\", StyleBox[\\\"a\\\", \\\"TI\\\"], \\\",\\\", StyleBox[\\\"n\\\", \\\"TI\\\"], \\\"]\\\"}]\\) gives the Goodstein function \\!\\(\\*StyleBox[\\\"G(k, a, n)\\\", \\\"TI\\\"]\\). \>\"",
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\);
Epsilon::usage = \!\(\*
TagBox[
StyleBox["\"\<\\!\\(\\*RowBox[{\\\"Epsilon\\\", \\\"[\\\", StyleBox[\\\"s\\\", \\\"TI\\\"], \\\",\\\", StyleBox[\\\"a\\\", \\\"TI\\\"], \\\"]\\\"}]\\) gives the left identity element for right operand \\!\\(\\*StyleBox[\\\"a\\\", \\\"TI\\\"]\\) and hyperoperation rank \\!\\(\\*StyleBox[\\\"s\\\", \\\"TI\\\"]\\). \>\"",
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\);
Eta::usage = "\!\(\*RowBox[{\"Eta\", \"[\", StyleBox[\"s\", \"TI\"], \"]\"}]\) gives the right identity element for hyperoperation rank \!\(\*StyleBox[\"s\", \"TI\"]\). ";

Hyper::usage = \!\(\*
TagBox[
StyleBox["\"\<\\!\\(\\*RowBox[{\\\"Hyper\\\", \\\"[\\\", StyleBox[\\\"s\\\", \\\"TI\\\"], \\\",\\\", StyleBox[\\\"a\\\", \\\"TI\\\"], \\\",\\\", StyleBox[\\\"b\\\", \\\"TI\\\"], \\\"]\\\"}]\\) gives the rank \\!\\(\\*StyleBox[\\\"s\\\", \\\"TI\\\"]\\) hyperoperation with operands \\!\\(\\*StyleBox[\\\"a\\\", \\\"TI\\\"]\\) and \\!\\(\\*StyleBox[\\\"b\\\", \\\"TI\\\"]\\). \>\"",
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\);
HyperLog::usage = \!\(\*
TagBox[
StyleBox["\"\<\\!\\(\\*RowBox[{\\\"HyperLog\\\", \\\"[\\\", StyleBox[\\\"s\\\", \\\"TI\\\"], \\\",\\\", StyleBox[\\\"a\\\", \\\"TI\\\"], \\\",\\\", StyleBox[\\\"b\\\", \\\"TI\\\"], \\\"]\\\"}]\\) gives the rank \\!\\(\\*StyleBox[\\\"s\\\", \\\"TI\\\"]\\) hyperlogarithm of \\!\\(\\*StyleBox[\\\"a\\\", \\\"TI\\\"]\\) to the base \\!\\(\\*StyleBox[\\\"b\\\", \\\"TI\\\"]\\). \>\"",
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\);
HyperRoot::usage = \!\(\*
TagBox[
StyleBox["\"\<\\!\\(\\*RowBox[{\\\"HyperRoot\\\", \\\"[\\\", StyleBox[\\\"s\\\", \\\"TI\\\"], \\\",\\\", StyleBox[\\\"a\\\", \\\"TI\\\"], \\\",\\\", StyleBox[\\\"b\\\", \\\"TI\\\"], \\\"]\\\"}]\\) gives the \\!\\(\\*StyleBox[\\\"b\\\", \\\"TI\\\"]\\)-th rank \\!\\(\\*StyleBox[\\\"s\\\", \\\"TI\\\"]\\) hyper-root of \\!\\(\\*StyleBox[\\\"a\\\", \\\"TI\\\"]\\). \>\"",
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\);

FrappierBox::usage = \!\(\*
TagBox[
StyleBox["\"\<\\!\\(\\*RowBox[{\\\"FrappierBox\\\", \\\"[\\\", StyleBox[\\\"x\\\", \\\"TI\\\"], \\\",\\\", StyleBox[\\\"y\\\", \\\"TI\\\"], \\\"]\\\"}]\\) gives Frappier's box operation \\!\\(\\*StyleBox[\\\"x\\\", \\\"TI\\\"]\\) \\[EmptySquare] \\!\\(\\*StyleBox[\\\"y\\\", \\\"TI\\\"]\\). \>\"",
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\);
Bennett::usage = \!\(\*
TagBox[
StyleBox["\"\<\\!\\(\\*RowBox[{\\\"Bennett\\\", \\\"[\\\", StyleBox[\\\"s\\\", \\\"TI\\\"], \\\",\\\", StyleBox[\\\"a\\\", \\\"TI\\\"], \\\",\\\", StyleBox[\\\"b\\\", \\\"TI\\\"], \\\"]\\\"}]\\) gives the rank \\!\\(\\*StyleBox[\\\"s\\\", \\\"TI\\\"]\\) commutative hyperoperation with operands \\!\\(\\*StyleBox[\\\"a\\\", \\\"TI\\\"]\\) and \\!\\(\\*StyleBox[\\\"b\\\", \\\"TI\\\"]\\). \>\"",
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\);
HierarchyF::usage = \!\(\*
TagBox[
StyleBox["\"\<\\!\\(\\*RowBox[{\\\"HierarchyF\\\", \\\"[\\\", StyleBox[\\\"n\\\", \\\"TI\\\"], \\\",\\\", StyleBox[\\\"x\\\", \\\"TI\\\"], \\\",\\\", StyleBox[\\\"y\\\", \\\"TI\\\"], \\\"]\\\"}]\\) gives Grzegorczyk's function \\!\\(\\*RowBox[{SubscriptBox[StyleBox[\\\"f\\\", \\\"TI\\\"], StyleBox[\\\"n\\\", \\\"TI\\\"]], StyleBox[\\\"(x, y)\\\", \\\"TI\\\"]}]\\). \>\"",
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\);


Unprotect[
Zerate, TrappmannZerate, Delta, Deltate,
Tower, TowerMod, Tetrate, TetrateDigits,
SuperLog, SuperSqrt, SuperRoot,
GoodsteinG, Epsilon, Eta,
Hyper, HyperLog, HyperRoot,
FrappierBox, Bennett, HierarchyF
];

Begin["`Private`"];

(* * * * * * * * * * * * * * * * * * * * *
 *          Operations of rank 0         *
 * * * * * * * * * * * * * * * * * * * * *)

Clear[Zerate]
SetAttributes[Zerate, {Orderless, NumericFunction}];
Zerate[-Infinity, b_] := b
Zerate[a_, -Infinity] := a
Zerate[Delta[a_], Delta[b_]] := Delta[Max[a, b]] + 1
Zerate[a_, b_] := Max[a,b] + 1
Zerate[a_, a_] := a + 2

Clear[TrappmannZerate]
SetAttributes[TrappmannZerate, {Orderless, NumericFunction}];
TrappmannZerate[a_, b_] := If[b > a + 1, b + 1, a + 2]

Clear[Delta]
SetAttributes[Delta, Listable];
(* Delta[x] represents a delta number, and is clearer if not
   expanded to its complex representation.  This is basically
   a "dummy" function. *)

Clear[Deltate]
SetAttributes[Deltate, NumericFunction];
Deltate[-Infinity, a_] := Delta[a]
Deltate[c_, a_] := Piecewise[{
{c - 1, (c > a + 2) || (a + 1 < c && c < a + 2)},
{{c - 1, c - 2}, c == a + 2},
{Interval[{-Infinity, a}], c == a + 1},
{-Infinity, a <= c && c < a + 1},
{Delta[Deltate[a, c]], c < a} }]

(* * * * * * * * * * * * * * * * * * * * *
 *          Operations of rank 4         *
 * * * * * * * * * * * * * * * * * * * * *)

Clear[Tower]
SetAttributes[Tower, NumericFunction];
(* Creates a power tower with n x's, with the option of specifying
   the top-most exponent to create an inhomogeneous power tower. *)
Tower[x_, n_Integer] := Nest[Power[x, #]&, 1, n] /; n >= 0
Tower[x_, n_Integer, s_] := Nest[Power[x, #]&, s, n] /; n >= 0

Clear[PowerMod2]
SetAttributes[PowerMod2, NumericFunction];
(* Extension of PowerMod to non-integer bases;
 uses PowerMod when possible since it's faster. *)
PowerMod2[a_Integer, b_, n_Integer] := PowerMod[a, b, n]
PowerMod2[a_, b_, n_] := N[Mod[a^b, n]]

Clear[TowerMod]
SetAttributes[TowerMod, NumericFunction];
(* Essentially, Tower (see above) modulo m. *)
TowerMod[x_, n_Integer, m_Integer] := Nest[PowerMod2[x, #, m]&, 1, n] /; n >= 0
TowerMod[x_, n_Integer, s_, m_Integer] := Nest[PowerMod2[x, #, m]&, s, n] /; n >= 0

Clear[scrit, s25]
SetAttributes[scrit, NumericFunction];
(* Function s25 is the approximation of the critical function of
   the superlogarithm and is so massive it is stored in another file. 
   Function scrit allows the approximation to be changed by changing
   only one line of code. *)
Get["s25"]
scrit[x_, z_] := N[s25[x, z]]

Clear[tcrit]
SetAttributes[tcrit, NumericFunction];
(* I use FindRoot because symbolic methods work only for degree <= 4
   and generate multiple solutions, which would have to be screened to
   get the solution z in the interval [0, 1].  Numerical methods work
   for any degree, and generally stay close to the initial guess. *)
tcrit[x_, y_] := FindRoot[y == scrit[x, z], {z, 1}][[1, 2]] 

Clear[Tetrate]
SetAttributes[Tetrate, NumericFunction];
(* My slight twist on Robbins' tetration extension.  Although technically
   limited to x > 1, it can still be used with *some* accuracy.  This is
   important for use in SuperRoot's numerical root-finding, mainly. *)
Tetrate[SuperRoot[x_, y_], y_] := x
Tetrate[x_, Infinity] := -ProductLog[-Log[x]]/Log[x]
Tetrate[x_, y_Integer] := Tower[x, y, 1] /; y >= 0
Tetrate[x_, y_] := If[y > -1, Tower[x, IntegerPart[y] + 1,
 tcrit[x,FractionalPart[y] - 1]], Log[x, Tetrate[x, y + 1]]]

Clear[TetrateDigits]
SetAttributes[TetrateDigits, NumericFunction];
(* Gives the n rightmost digits of Tetrate[x, y]. *)
TetrateDigits[SuperRoot[x_, y_], y_] := x
TetrateDigits[x_, Infinity, n_Integer] := -ProductLog[-Log[x]]/Log[x]
TetrateDigits[x_, y_Integer, n_Integer] := TowerMod[x, y, 1, 10^n] /; y >= 0
TetrateDigits[x_, y_, n_Integer] := If[y > -1, TowerMod[x, IntegerPart[y] + 1,
 tcrit[x,FractionalPart[y] - 1], 10^n], Log[x, Tetrate[x, y + 1]]]

Clear[SuperLog]
SetAttributes[SuperLog, NumericFunction];
(* The superlogarithm, based on Robbins' formula and using, of course, s25. *)
SuperLog[x_, z_] := scrit[x, x^z] - 1 /; z <= 0
SuperLog[x_, z_] := scrit[x, z] /; 0 < z && z <= 1
SuperLog[x_, z_] := Module[{m = 1},
  While[!(Tower[x, m-1] < z <= Tower[x, m]), m++];
  scrit[x, Nest[Log[x, #]&, z, m]] + m]

Clear[SuperSqrt]
SetAttributes[SuperSqrt, NumericFunction];
(* From Rubtsov and Romerio 2004, section "Mathematica 
   implementations of relevant functions."  This is modified
   from code provided by the authors; changes are limited to
   incorporating a domain restriction into the definitions. *)
SuperSqrt[x_] := Log[x]/ProductLog[Log[x]] /; x > Exp[-1/E] && Im[x] == 0

Clear[Bisect]
SetAttributes[Bisect, NumericFunction];
(* Performs bisection on an interval once, returning
   the new interval.  A component of BisectZero. *)
Bisect[f_, {a_, b_}] := Module[{m = (b + a) / 2},
If[f[a]*f[m] <= 0, {a, m}, {m ,b}]]

Clear[BisectZero]
SetAttributes[BisectZero, NumericFunction];
(* Finds the zero of f on the interval int within tolerance tol,
   assuming it does exist. *)
BisectZero[f_, int_, tol_] := Mean[NestWhile[Bisect[f, #]&, int,
 (#[[2]] - #[[1]]) > tol &]]

Clear[SuperRoot]
(* Gives numerical results whenever possible.  However,
   tetration will properly handle things like
   Tetrate[SuperRoot[x, y], y], which is of course x. *)
SuperRoot[z_, 1] := z
SuperRoot[z_, 2] := SuperSqrt[z] /; Im[z] == 0
SuperRoot[z_, y_] := BisectZero[Tetrate[#, y] - z &,
 {10^-$MachinePrecision, SuperSqrt[z]}, 10^-10] /; y > 2 && Im[z] == 0

(* * * * * * * * * * * * * * * * * * * * *
 *         General hyperoperations       *
 * * * * * * * * * * * * * * * * * * * * *)

Clear[GoodsteinG]
SetAttributes[GoodsteinG, NumericFunction];
(* Modified from Goodstein's original formulation for better performance. *)
GoodsteinG[0, a_, n_Integer] := n + 1
GoodsteinG[1, a_, n_Integer] := a + n
GoodsteinG[2, a_, n_Integer] := a n
GoodsteinG[3, a_, n_Integer] := a^n
GoodsteinG[k_Integer, a_, 0] := 1  /; k >= 3
GoodsteinG[k_Integer, a_, n_Integer] := GoodsteinG[k-1, a, GoodsteinG[k, a, n-1]]

Clear[Epsilon]
(* Left identity element as defined in Rubtsov and Romerio 2004. *)
Epsilon[0, a_] := -Infinity
Epsilon[1, a_] := 0
Epsilon[2, a_] := 1
Epsilon[3, a_] := a^(1/a)
Epsilon[4, a_] := SuperRoot[a, a]
(* Presumably, this trend continues for s>4, but that
   will wait for another day. *)

Clear[Eta]
(* Right identity element as defined in Rubtsov and Romerio 2004. *)
Eta[0] := -Infinity
Eta[1] := 0
Eta[s_Integer] := 1 /; s >= 2

(* Generalized hyperoperation functions, elegant front-ends
   to the functions presented herein.  When not computable
   numerically, they do their best to allow symbolic manipulation,
   at least. *)

Clear[Hyper]
Hyper[0, a_, b_] := Zerate[a, b]
Hyper[1, a_, b_] := Plus[a, b]
Hyper[2, a_, b_] := Times[a, b]
Hyper[3, a_, b_] := Power[a, b]
Hyper[4, a_, b_] := Tetrate[a, b]

Hyper[s_, 2, 2] := 4
Hyper[s_, a_, 0] := 1  /; s >= 3
Hyper[s_, a_, 2] := Hyper[s - 1, a, a]
Hyper[s_Integer, a_, b_Integer] := Hyper[s - 1, a, Hyper[s, a, b - 1]] /; s > 4

Hyper[s_, Epsilon[s_, a_], a_] := a
Hyper[s_, a_, Hyper[z_, a_, b_]] := If[s == z - 1, Hyper[z, a, b + 1]]
Hyper[s_, HyperRoot[s_, a_, b_], b_] := a
Hyper[s_, b_, HyperLog[s_, a_, b_]] := a

Clear[HyperLog]
HyperLog[0, a_, b_] := Deltate[a, b]
HyperLog[1, a_, b_] := Subtract[a, b]
HyperLog[2, a_, b_] := Divide[a, b]
HyperLog[3, a_, b_] := Log[b, a]
HyperLog[4, a_, b_] := SuperLog[b, a]

HyperLog[s_, Hyper[s_, a_, b_], a_] := b
HyperLog[s_, Hyper[z_, a_, b_], a_] := If[s == z - 1, Hyper[z, a, b - 1]]

Clear[HyperRoot]
HyperRoot[0, a_, b_] := Deltate[a, b]
HyperRoot[1, a_, b_] := Subtract[a, b]
HyperRoot[2, a_, b_] := Divide[a, b]
HyperRoot[3, a_, b_] := Power[a, 1/b]
HyperRoot[4, a_, b_] := SuperRoot[a, b]

HyperRoot[s_, Hyper[s_, a_, b_], b_] := a

(* * * * * * * * * * * * * * * * * * * * *
 *         Variant hyperoperations       *
 * * * * * * * * * * * * * * * * * * * * *)

Clear[FrappierBox]
SetAttributes[FrappierBox, NumericFunction];
(* Clement Frappier's "kind" of iterated exponential *)
FrappierBox[0, z_] := z
FrappierBox[n_Integer, z_] := FrappierBox[n - 1, z]^FrappierBox[n - 1, z]


(* Implementations of Exp and Log nested n times.  n need not be an integer.
   It turns out there is a beautiful symmetry in these definitions! *)
Clear[IterExp]
SetAttributes[IterExp, NumericFunction];
IterExp[n_, x_] := Tetrate[E, n + SuperLog[E, x]]

Clear[IterLog]
SetAttributes[IterLog, NumericFunction];
IterLog[n_, x_] := Tetrate[E, SuperLog[E, x] - n]

Clear[Bennett]
SetAttributes[Bennett, NumericFunction];
(* Proposed by Albert Bennett in 1915.  Commutative in
   the second and third arguments. *)
Bennett[0, a_, b_] := a + b
Bennett[n_Integer, a_, b_] := Exp[Bennett[n-1, Log[a], Log[b]]] /; n>0
Bennett[n_, a_, b_] := IterExp[n, IterLog[n, a] + IterLog[n, b]]

Clear[HierarchyF]
SetAttributes[HierarchyF, NumericFunction];
(* Andrzej Grzegorczyk's function f_n(x, y) that appears in class
   E^n of the Grzegorczyk hierarchy.  The name HierarchyF was chosen
   because no one can spell Grzegorczyk the same way twice. *)
HierarchyF[0, x_, y_] := y+1
HierarchyF[1, x_, y_] := x+y
HierarchyF[2, x_, y_] := (x+1) (y+1)
HierarchyF[n_Integer, 0, y_] := HierarchyF[n-1, y+1, y+1] /; n>=2
HierarchyF[n_Integer, x_, y_] := HierarchyF[n, x-1, HierarchyF[n, x-1, y]] /; n>=2


End[ ]; (* End `Private` Context. *)

SetAttributes[
{Zerate, TrappmannZerate, Delta, Deltate,
Tower, TowerMod, Tetrate, TetrateDigits,
SuperLog, SuperSqrt, SuperRoot,
GoodsteinG, Epsilon, Eta,
Hyper, HyperLog, HyperRoot,
FrappierBox, Bennett, HierarchyF},
{Protected, ReadProtected}
];

EndPackage[ ]; (* End package Context. *)
