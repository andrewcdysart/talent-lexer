/*
 * Provided under the MIT license.
 * See License.md for license details
 * Copyright (c) 2019 Andrew Dysart
 */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Define the "context" language facility %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module( 'syntax_definitions/context', [id_context/3] ).
:- use_module( words ).
:- use_module( numbers ).
:- use_module( punctuation ).
:- use_module( whitespace ).
:- use_module( utilities ).

/*
 * A "context" has the form [Pre|Post] context_name { [actions] }
 */
context_time( 'Pre' ).
context_time( 'Post' ).
% The "Always" context_time does not take a context_name
context_time( 'Always' ).
maxchars_context_time( 10 ).

/*
 * The available context_names are defined below.
 */
context_name( 'InitiativeRoll' ).
context_name( 'AttackTest' ).
context_name( 'CalculateAp' ).
context_name( 'TestSkill' ).
context_name( 'TestSkillGovernedBy' ).
maxchars_context_name( 100 ).

/*
 * Select context_names will take arguments.
 */
context_with_args( 'TestSkill' ).
context_with_args( 'TestSkillGovernedBy' ).

/*
 * Step through the input and attempt to find a context token.
 */
id_context( X, Token, RestOfChars ) :-
   find_context_time( X, Time, NonTimeChars ),
   required_whitespace( NonTimeChars, NameChars ),
   find_context_name( NameChars, Name, RestOfChars ),
   ( context_with_args( Name ) ->
      % Do argument stuff
      ( write_ln( "Args context!" ) ) ;
      ( write_ln( "No args!" ) )
   ),
   Token = token( type( 'context' ), time( Time ), name( Name ) ).

/*
 * The first element of a context token is the time specifier
 */
find_context_time( X, ContextTime, UnconsumedChars ) :-
   pull_atom( X, ContextTime, context_time, maxchars_context_time, UnconsumedChars ).

/*
 * Time specifiers must be followed by the context name.
 */
find_context_name( X, ContextName, UnconsumedChars ) :- 
   pull_atom( X, ContextName, context_name, maxchars_context_name, UnconsumedChars ).
