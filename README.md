# UESRPG Talent Lexer

## Purpose

To create a syntax parser (and eventual compiler) for use with the
[UESRPG CharGen][UESRPGCharGen] project which will allow users to
write Talent scripts (as well as other special effect scripts).

## Installation and Usage

The codebase is written using [SWI Prolog][SWI Site], a free Prolog
implementation. The main module is [*lexer.pl*][lexer]. Currently,
the initialization directive is off, so you'll need to invoke go/0
manually. After that, you can enter 'h' to see the available commands!

## Background

I was reading [Faiçal Tchirou's excellent tutorial][LexicalAnalysis]
on lexical analysis a couple of days after beginning my foray into
Prolog. Upon exploring the concepts further, I figured that his finite
state machine layout might be more composable if written as a series of
Prolog rules. The first foray into this idea was my [toy lexer][Toy Lexer]
project, a simple exploration of implementing language rules in Prolog.

Since then, I've created a few sample "scripts" based on existing Talents
in the Unofficial Elder Scrolls RPG, and I'm working on building language
rules for them! The end goal will be to take the output of *this* tool
and compile it into C# (which [UESRPG CharGen][UESRPGCharGen] is written
in) which will be grafted into the game logic using reflection. We're a
ways away from that right now, but there's the roadmap!

## License

This project is licensed under the MIT license. You can read the full
license [here][MIT License].

[SWI Site]: https://swi-prolog.org
[lexer]: https://github.com/andrewcdysart/talent-lexer/blob/master/lexer.pl
[LexicalAnalysis]: https://hackernoon.com/lexical-analysis-861b8bfe4cb0
[UESRPGCharGen]: https://github.com/andrewcdysart/UESRPGCharGen
[Toy Lexer]: https://github.com/andrewcdysart/prolog-toy-lexer/
[MIT License]: https://github.com/andrewcdysart/talent-lexer/blob/master/License.md
