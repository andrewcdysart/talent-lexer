/*
 * Provided under the MIT license.
 * See License.md for license details
 * Copyright (c) 2019 Andrew Dysart
 */

%%%%%%%%%%%%%%%%%%%%%%
% Define "operators" %
%%%%%%%%%%%%%%%%%%%%%%

:- module('syntax_definitions/operators',[id_operator/3]).
:- use_module('utilities').
operator('=').
operator('+').
operator('-').
operator('/').
operator('*').
operator('<').
operator('>').
operator('.').
operator('<=').
operator('>=').
operator('==').
maxchars_operator(3).

id_operator(X,Token,UnconsumedChars) :-
   find_operator(X,Operator,UnconsumedChars),
   Token=token(type('operator'),Operator).

find_operator(X,Atom,UnconsumedChars) :-
   pull_atom(X,Atom,operator,maxchars_operator,UnconsumedChars).
