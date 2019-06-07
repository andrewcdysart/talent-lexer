/*
 * Provided under the MIT license.
 * See License.md for license details
 * Copyright (c) 2019 Andrew Dysart
 */

:- module('syntax_definitions/whitespace',[whitespace/1,newline/1,return/1,space/1]).

newline('\n').
return('\r').
whitespace('\r').
whitespace('\n').
whitespace('\t').
whitespace(' ').
space(' ').