/*
 * Provided under the MIT license.
 * See License.md for license details
 * Copyright (c) 2019 Andrew Dysart
 */

%%%%%%%%%%%%%%%%%%%%%
% Define "comments" %
%%%%%%%%%%%%%%%%%%%%%

:- module( 'syntax_definitions/comments', [id_comment/3] ).
:- use_module( punctuation ).
:- use_module( whitespace ).
:- use_module( utilities ).

comment_starter( '//' ).
comment_starter( '/*' ).
maxchars_comment_starter( 3 ).
comment_ender( '*/' ).
inline_starter( '//' ).
multiline_starter( '/*' ).

inline_comment_char( X ) :- \+newline( X ), \+return( X ).

/*
 * multiline_comment/3 will recurse through text until a comment ender
 * is found, then build the comment string from back-to-front
 */
multiline_comment([NextChar|RestOfChars], FoundChars, UnconsumedChars) :-
   multiline_comment(RestOfChars,CommentEnd,UnconsumedChars),
   FoundChars = [NextChar|CommentEnd].
multiline_comment([NextChar|RestOfChars], FoundChars, UnconsumedChars) :-
   [SecondChar|UnconsumedChars] = RestOfChars,
   append_atoms( [NextChar, SecondChar], Atom ),
   comment_ender( Atom ), !,
   FoundChars = [NextChar,SecondChar].

/*
 * id_comment/3 will search for comment starts, either inline or multi-
 * line, and try to build a full comment string.
 */
id_comment( X, Token, UnconsumedChars ) :-
   pull_atom( X, CommentStart, comment_starter, maxchars_comment_starter, RestOfChars ),
   ( inline_starter( CommentStart ) ->
      (
         !, pull_atom_sequence( RestOfChars, CommentText, inline_comment_char, UnconsumedChars ),
         append_atoms([CommentStart,CommentText],Comment)
      ) ;
      (
         !, multiline_starter( CommentStart ),
         multiline_comment( RestOfChars, CommentText, UnconsumedChars ),
         atom_chars( CommentTextAtom, CommentText ),
         append_atoms( [CommentStart, CommentTextAtom], Comment )
      )
   ),
   Token=token( type( 'comment' ), Comment ).
