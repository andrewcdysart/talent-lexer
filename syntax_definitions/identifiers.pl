/*
 * Provided under the MIT license.
 * See License.md for license details
 * Copyright (c) 2019 Andrew Dysart
 */

%%%%%%%%%%%%%%%%%%%%%%%%
% Define "identifiers" %
%%%%%%%%%%%%%%%%%%%%%%%%

:- module( 'syntax_definitions/identifiers', [id_identifier/3] ).
:- use_module( words ).
:- use_module( numbers ).
:- use_module( punctuation ).
:- use_module( utilities ).
term_char( '_' ).
term_char( '-' ).

is_identifier_first_char( X ) :- letter( X ), !; underscore( X ).
is_identifier_innerchar( X ) :- letter( X ), !; digit( X ), !; term_char( X ).

id_identifier( [X|RestOfChars], Token, UnconsumedChars ) :-
   is_identifier_first_char( X ),
   pull_atom_sequence( RestOfChars, Atom, is_identifier_innerchar, UnconsumedChars ),
   append_atoms( [X, Atom], Identifier ),
   Token=token( type( 'identifier' ), Identifier ).
