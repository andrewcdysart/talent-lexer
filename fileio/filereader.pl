/*
 * Provided under the MIT license.
 * See License.md for license details
 * Copyright (c) 2019 Andrew Dysart
 */

:- module('fileio/filereader',[read_file/2]).

:- use_module(library(pio)).
lines([]) --> call(eos), !.
lines([Line|Lines]) --> line(Line), lines(Lines).
eos([],[]).
line([]) --> ( "\n"; call(eos) ), !.
line([L|Ls]) --> [L], line(Ls).

transform_lines([X|[]],StringLines) :-
   StringLines=X.
transform_lines([X|OtherLines],StringLines) :-
   atom_codes('\n',NlCode),
   append(X,NlCode,Line),
   transform_lines(OtherLines,Lines),
   append(Line,Lines,StringLines).

read_file(F,String) :-
   phrase_from_file(lines(AllLines),F),
   transform_lines(AllLines,CombinedLines),
   string_chars(String,CombinedLines).