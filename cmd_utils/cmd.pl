:- include(header).

/************
 * COMMANDS *
 ************/
command('h', "Prints help text (like right now!)", cmd_help()).
cmd_help() :- print_command_list().

command('e', "Exit the program", cmd_exit()).
cmd_exit() :-
   write_ln("Bye!"),
   halt.

/**********************
 * UTILITY PREDICATES *
 **********************/
get_command_list(Commands) :-
   findall(A, command(A,_,_), Commands).

print_command(X) :-
   command(X, HelpText, _),
   nl, write(X), write(" - "), write(HelpText).
print_command_list([X|[]]) :- print_command(X), nl.
print_command_list([X|RestOfCommands]) :-
   print_command(X),
   print_command_list(RestOfCommands).
print_command_list() :-
   get_command_list(Commands),
   print_command_list(Commands).

/*********************
 * WORKER PREDICATES *
 *********************/
process_cmd(X) :-
   command(X, _, Predicate), !,
   call(Predicate), nl.
process_cmd(_) :-
   process_cmd('h').