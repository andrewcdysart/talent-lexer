/*
 * Provided under the MIT license.
 * See License.md for license details
 * Copyright (c) 2019 Andrew Dysart
 */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Provide definitions for whitespace as well     %
% as required_whitespace and optional_whitespace %
% utility rules                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module('syntax_definitions/whitespace',
      [
         whitespace/1,
         newline/1,
         return/1,
         space/1,
         required_whitespace/2,
         optional_whitespace/2
      ]
   ).
:- use_module(utilities).

newline('\n').
return('\r').
whitespace('\r').
whitespace('\n').
whitespace('\t').
whitespace(' ').
space(' ').

required_whitespace(X,UnconsumedChars) :-
   pull_atom_sequence(X,_,whitespace,UnconsumedChars).

optional_whitespace(X,UnconsumedChars) :-
   pull_atom_sequence(X,_,whitespace,UnconsumedChars) ; !, UnconsumedChars = X.
