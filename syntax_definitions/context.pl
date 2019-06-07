%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Define the "context" language facility %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module('syntax_definitions/context', [id_context/3]).
:- use_module(words).
:- use_module(numbers).
:- use_module(punctuation).

/*
 * A "context" has the form [Pre|Post] ContextName { [actions] }
 */
context_time('Pre').
context_time('Post').

/*
 * The available ContextNames are defined below.
 */
context_name('InitiativeRoll').
context_name('AttackTest').
context_name('CalculateAp').
context_name('TestSkill').
context_name('TestSkillGovernedBy').

/*
 * Select ContextNames will take arguments.
 */
context_with_args('TestSkill').
context_with_args('TestSkillGovernedBy').

/*
 * Step through the input and attempt to find a context token.
 */
id_context(X,Token,RestOfChars) :- fail.
