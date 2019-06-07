/*
 * Provided under the MIT license.
 * See License.md for license details
 * Copyright (c) 2019 Andrew Dysart
 */

:- module('syntax_definitions/identifiers', [id_identifier/3]).
:- use_module(words).
:- use_module(numbers).
:- use_module(punctuation).
term_char('_').
term_char('-').

is_identifier_first_char(X) :- letter(X), !; underscore(X).
is_identifier_innerchar(X) :- letter(X), !; digit(X), !; term_char(X).
/*is_identifier([X|[]]) :- !, is_identifier_innerchar(X).
is_identifier([X|RestOfChars]) :-
   !, is_identifier_innerchar(X), is_identifier(RestOfChars).
is_identifier(X) :- string_chars(X,[FirstChar|RestOfChars]),
   !, is_identifier_first_char(FirstChar),
   is_identifier(RestOfChars).*/

identifier_inner_chars([],[],[]).
identifier_inner_chars([X|RestOfChars],[],[X|RestOfChars]) :- \+is_identifier_innerchar(X).
identifier_inner_chars([X|[]],[],[]) :- \+is_identifier_innerchar(X).
identifier_inner_chars([X|[]],[X],[]) :- is_identifier_innerchar(X).
identifier_inner_chars([X|RestOfChars],FoundChars,UnconsumedChars) :-
   is_identifier_innerchar(X),
   identifier_inner_chars(RestOfChars,OtherChars,UnconsumedChars),
   FoundChars=[X|OtherChars].

id_identifier([X|RestOfChars],Token,UnconsumedChars) :-
   is_identifier_first_char(X),
   identifier_inner_chars(RestOfChars,InnerChars,UnconsumedChars),
   append([X],InnerChars,IdentifierList),
   string_chars(Identifier,IdentifierList),
   Token=token("identifier", Identifier).