/*
 * Provided under the MIT license.
 * See License.md for license details
 * Copyright (c) 2019 Andrew Dysart
 */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Utilities for ease of writing %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module('syntax_definitions/utilities',
      [
         lt_constant/2,
         pull_atom/5,
         pull_atom_sequence/4,
         add_char_to_atom/3,
         append_atoms/2
      ]
   ).
:- meta_predicate
   lt_constant(?,1),
   pull_atom(+,-,1,1,-),
   pull_atom_sequence(+,-,1,-).

lt_constant( N, Constant ) :-
   call( Constant, X ),
   N < X.

/*
 * Pull out an atom matching the given AtomDef, up to MaxCharsDef
 * (a single-arg predicate), returning the rest of the chars in
 * UnconsumedChars.
 */
pull_atom( X, Atom, AtomDef, MaxCharsDef, UnconsumedChars ) :-
   search_for_atom( X, Atom, AtomDef, MaxCharsDef ),
   atom_chars( Atom, CharList ),
   ord_subtract( X, CharList, UnconsumedChars ).
search_for_atom( Chars, Atom, AtomDef, MaxCharsDef ) :-
   nth0(0, Chars, FirstAtom),
   % If the FirstAtom matches AtomDef, continue the search just in case there's a longer match
   ( call( AtomDef, FirstAtom) ->
      (
         % If subsequent characters don't match, that's okay. The FirstAtom was a match.
         ( search_for_atom(0, Chars, FirstAtom, Atom, AtomDef, MaxCharsDef ) ->
            ( ! ) ;
            ( Atom = FirstAtom )
         )
      ) ;
      % Otherwise search as normal
      ( search_for_atom( 0, Chars, FirstAtom, Atom, AtomDef, MaxCharsDef ) )
   ).
search_for_atom( Idx, Chars, CurrentAtom, FoundAtom, AtomDef, MaxChars ) :-
   NextIdx is Idx + 1,
   lt_constant( NextIdx, MaxChars),
   % If another character is available, continue checking
   ( nth0( NextIdx, Chars, NextChar ) ->
      (
         add_char_to_atom( CurrentAtom, NextChar, NextAtom ),
         ( call( AtomDef, NextAtom ) ->
            ( FoundAtom = NextAtom, ! ) ;
            ( !, search_for_atom( NextIdx, Chars, NextAtom, FoundAtom, AtomDef, MaxChars ) )
         )
      ) ;
      % Otherwise, no match was found
      ( fail )
   ).

/*
 * Pull out an atom where each character matches the given AtomDef,
 * until the string no longer matches the AtomDef, returning the
 * rest of the chars in UnconsumedChars.
 */
pull_atom_sequence( X, Atom, AtomDef, UnconsumedChars ) :-
   search_for_atom_sequence( X, Atom, AtomDef ),
   atom_chars( Atom, CharList ),
   ord_subtract( X, CharList, UnconsumedChars ).
search_for_atom_sequence( Chars, Atom, AtomDef ) :-
   nth0( 0, Chars, FirstAtom ),
   call( AtomDef, FirstAtom ),
   search_for_atom_sequence( 0, Chars, FirstAtom, Atom, AtomDef ).
search_for_atom_sequence( Idx, Chars, CurrentAtom, FoundAtom, AtomDef ) :-
   NextIdx is Idx + 1,
   % If another character is available, continue checking
   ( nth0( NextIdx, Chars, NextChar ) ->
      (
         add_char_to_atom( CurrentAtom, NextChar, NextAtom ),
         ( call( AtomDef, NextChar ) ->
            % The NextAtom matches, so continue searching.
            ( search_for_atom_sequence( NextIdx, Chars, NextAtom, FoundAtom, AtomDef ) ) ;
            % The NextAtom does not match, so return the CurrentAtom only.
            ( FoundAtom = CurrentAtom, ! )
         )
      ) ;
      % Otherwise, return the atom built thus far
      ( FoundAtom = CurrentAtom, ! )
   ).

add_char_to_atom( Atom, Char, Result ) :-
   atom_chars( Atom, CurrentChars ),
   append( [CurrentChars, [Char]], AllChars ),
   atom_chars( Result, AllChars ).

append_atoms( [X|[]], X ).
append_atoms( [X|OtherAtoms], Result ) :-
   append_atoms( OtherAtoms, OthersCombined ),
   atom_chars( X, XChars ),
   atom_chars( OthersCombined, OtherChars ),
   append( [XChars, OtherChars], ResultChars ),
   atom_chars( Result, ResultChars ).
