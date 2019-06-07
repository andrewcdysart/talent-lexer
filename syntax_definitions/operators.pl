/*
 * Provided under the MIT license.
 * See License.md for license details
 * Copyright (c) 2019 Andrew Dysart
 */

:- module('syntax_definitions/operators',[is_operator/1, id_operator/3]).
operator('=').
operator('+').
operator('-').
operator('/').
operator('*').
operator('<').
operator('>').
operator('.').
operator_sequence(['<', '=']).
operator_sequence(['>', '=']).
operator_sequence(['=', '=']).

is_operator([X|[]]) :- !, operator(X).
is_operator([X|RestOfChars]) :-
   !, [NextChar|_] = RestOfChars,
   operator_sequence([X, NextChar]).
is_operator(X) :- string_chars(X,Chars),
   is_operator(Chars).

id_operator([X|RestOfChars],Token,UnconsumedChars) :-
   is_operator(X),
   [NextChar|UnconsumedChars] = RestOfChars,
   operator_sequence([X, NextChar]), !,
   string_chars(Operator,[X,NextChar]),
   Token=token("operator",Operator).
id_operator([X|UnconsumedChars],Token,UnconsumedChars) :-
   is_operator(X),
   string_chars(Operator,[X]),
   Token=token("operator",Operator).