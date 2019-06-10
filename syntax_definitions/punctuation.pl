/*
 * Provided under the MIT license.
 * See License.md for license details
 * Copyright (c) 2019 Andrew Dysart
 */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Provide names for common punctuation used %
% in language specification                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module('syntax_definitions/punctuation',
      [
         left_paren/1,
         right_paren/1,
         paren/1,
         left_curly_brace/1,
         right_curly_brace/1,
         curly_brace/1,
         forward_slash/1,
         asterisk/1,
         quote/1,
         underscore/1,
         punctuation/1
      ]
   ).
left_paren('(').
right_paren(')').
paren('(').
paren(')').
left_curly_brace('{').
right_curly_brace('}').
curly_brace('{').
curly_brace('}').
forward_slash('/').
asterisk('*').
quote('\"').
punctuation('.').
punctuation('\'').
punctuation(',').
punctuation(';').
punctuation(':').
punctuation('(').
punctuation(')').
punctuation('{').
punctuation('}').
punctuation('-').
punctuation('\"').
underscore('_').