/*
 * Provided under the MIT license.
 * See License.md for license details
 * Copyright (c) 2019 Andrew Dysart
 */

:- module('syntax_definitions/numbers',[digit/1,id_number/3]).
digit('0').
digit('1').
digit('2').
digit('3').
digit('4').
digit('5').
digit('6').
digit('7').
digit('8').
digit('9').

/*digits([X|[]],[]) :- digit(X).               % True if the last character in sequence is a digit
digits([X|[]],[X]) :- \+ digit(X).           % True if the last character in sequence is not a
                                             %     digit, give back the character
digits([X|RestOfChars],UnconsumedChars) :-   % True if the last characters in sequence are not
                                             %     digits, give back the characters
   \+digit(X),
   UnconsumedChars = [X|RestOfChars].
digits([X|RestOfChars],UnconsumedChars) :-
   digit(X),
   digits(RestOfChars,UnconsumedChars).

fraction([X|RestOfChars],UnconsumedChars) :- % True if the sequence starts with '.' and is followed by digits,
                                             % give back non-matching characters
   X = '.', !,
   digits(RestOfChars,UnconsumedChars).
fraction([],[]).                             % True for an empty sequence

optional_sign([X|RestOfChars],RestOfChars) :- sign(X).
optional_sign([X|RestOfChars],[X|RestOfChars]) :- \+ sign(X).
exponent([X|RestOfChars],UnconsumedChars) :-
   exponent_marker(X),
   optional_sign(RestOfChars,NonSignChars),
   digits(NonSignChars,UnconsumedChars).
exponent([],[]).

is_number([X|RestOfChars]) :-
   !, digits([X|RestOfChars],NonDigitChars),
   fraction(NonDigitChars,NonFractionChars),
   exponent(NonFractionChars,NonExponentChars),
   NonExponentChars = [].
is_number(X) :- string_chars(X,Chars),
   !, is_number(Chars).*/

digits([X|[]],[X],[]) :- digit(X), !.              % True if the last character in sequence is a digit
digits([X|[]],[],[X]) :- \+ digit(X), !.           % True if the last character in sequence is not a
                                                %     digit, give back the character
digits([X|RestOfChars],[],UnconsumedChars) :-   % True if the last characters in sequence are not
                                                %     digits, give back the characters
   \+digit(X), !,
   UnconsumedChars = [X|RestOfChars].
digits([X|RestOfChars],FoundDigits,UnconsumedChars) :-
   digit(X),
   digits(RestOfChars,OtherDigits,UnconsumedChars),
   FoundDigits=[X|OtherDigits].

fraction([X|RestOfChars],FoundFraction,UnconsumedChars) :-  % True if the sequence starts with '.' and is followed by digits,
                                                            % give back non-matching characters
   X = '.', !,
   digits(RestOfChars,FoundDigits,UnconsumedChars),
   FoundFraction = [X|FoundDigits].
fraction([X|RestOfChars],[],[X|RestOfChars]) :- X \= '.'.
fraction([],[],[]).                             % True for an empty sequence

exponent_marker(X) :- X = 'E'; X = 'e'.
sign(X) :- X = '+'; X = '-'.
optional_sign([X|RestOfChars],[X],RestOfChars) :- sign(X).
optional_sign([X|RestOfChars],[],[X|RestOfChars]) :- \+ sign(X).
exponent([X|RestOfChars],FoundExponent,UnconsumedChars) :-
   exponent_marker(X),
   optional_sign(RestOfChars,Sign,NonSignChars),
   digits(NonSignChars,FoundDigits,UnconsumedChars),
   length(FoundDigits,HowMany),
   HowMany > 0,
   FoundMarkerAndSign=[X|Sign],
   append(FoundMarkerAndSign,FoundDigits,FoundExponent).
exponent([X|RestOfChars],[],[X|RestOfChars]) :-
   \+exponent_marker(X);
   exponent_marker(X),
   optional_sign(RestOfChars,_,NonSignChars),
   digits(NonSignChars,FoundDigits,_),
   length(FoundDigits,HowMany),
   HowMany == 0.
exponent([],[],[]).

id_number([X|RestOfChars],Token,UnconsumedChars) :-
   !, digits([X|RestOfChars],FoundDigits,NonDigitChars),
   length(FoundDigits,HowMany),
   HowMany > 0,
   fraction(NonDigitChars,FoundFraction,NonFractionChars),
   exponent(NonFractionChars,FoundExponent,NonExponentChars),
   append([FoundDigits,FoundFraction,FoundExponent], FoundNumberList),
   string_chars(FoundNumber, FoundNumberList),
   Token=token("number", FoundNumber),
   UnconsumedChars = NonExponentChars.