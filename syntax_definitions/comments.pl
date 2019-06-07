%%%%%%%%%%%%%%%%%%%%%
% Define "comments" %
%%%%%%%%%%%%%%%%%%%%%

:- module('syntax_definitions/comments', [id_comment/3]).
:- use_module(punctuation).
:- use_module(whitespace).

/*
 * inline_comment/3 will recurse through text until either a newline or
 * end-of-string is reached, then build the comment string from back-to-
 * front
 */
inline_comment([NextChar|RestOfChars], FoundChars, UnconsumedChars) :-
   \+newline(NextChar),
   inline_comment(RestOfChars, OtherChars, UnconsumedChars),
   FoundChars = [NextChar|OtherChars].
inline_comment([NextChar|UnconsumedChars], [], UnconsumedChars) :-
   newline(NextChar), !.
inline_comment([NextChar|[]], [NextChar], []).

/*
 * multiline_comment/3 will recurse through text until a comment ender
 * is found, then build the comment string from back-to-front
 */
multiline_comment([NextChar|RestOfChars], FoundChars, UnconsumedChars) :-
   multiline_comment(RestOfChars,CommentEnd,UnconsumedChars),
   FoundChars = [NextChar|CommentEnd].
multiline_comment([NextChar|RestOfChars], FoundChars, UnconsumedChars) :-
   asterisk(NextChar),
   [SecondChar|UnconsumedChars] = RestOfChars,
   forward_slash(SecondChar), !,
   FoundChars = [NextChar,SecondChar].

/*
 * id_comment/3 will search for comment starts, either inline or multi-
 * line, and try to build a full comment string from back-to-front.
 */
id_comment([FirstChar|RestOfChars], Token, UnconsumedChars) :-
   forward_slash(FirstChar),
   [SecondChar|MoreChars] = RestOfChars,
   CommentStart = [FirstChar,SecondChar],
   ( forward_slash(SecondChar) ->
      (
         !, inline_comment(MoreChars, CommentText, UnconsumedChars),
         append([CommentStart,CommentText],TokenChars)
      )
   ;
      (
         asterisk(SecondChar), !,
         multiline_comment(MoreChars, CommentText, UnconsumedChars),
         append([CommentStart,CommentText],TokenChars)
      )
   ),
   string_chars(String,TokenChars),
   Token=token("comment",String).
