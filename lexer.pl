/*
 * Provided under the MIT license.
 * See License.md for license details
 * Copyright (c) 2019 Andrew Dysart
 */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Main program section; defines commands to be %
% invoked by the user.                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module( tokenizer ).
:- use_module( 'fileio/filereader' ).
:- include( 'cmd_utils/cmd' ).
% TODO: Investigate Prolog "ifdefs" to selectively exclude initialization goal during compilation
% :- initialization go.

command( 'r', "Read a file into the lexer", cmd_read_file() ).
cmd_read_file() :-
   write( "Filename: " ),
   read( File ),
   lex_file( File ).

lex_file( File ) :-
   read_file( File,String ), !,
   lex( String ).

command( 'l', "Lex direct input", cmd_lex_input() ).
cmd_lex_input() :-
   write_ln( "Lexing Direct Input" ),
   write( "Input: " ),
   read( Y ),
   lex( Y ).

interpret( X ) :- process_cmd( X ),
   go.

lex(X) :- string_chars( X, Chars ),
   tokenize( Chars, T ),write_ln( T ).

go() :- current_prolog_flag( argv, [Arg|[]] ), !,
   interpret( Arg ).
go() :-
   write( "Command: " ),
   read( X ),
   interpret( X ).

go( File ) :-
   lex_file( File ).
