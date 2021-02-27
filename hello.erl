#!/usr/bin/env escript
main(_) ->
    %% Directly reads the number of hellos as a decimal
    {ok, [X]} = io:fread("How many Hellos?> ", "~a"),
    %% Write X hellos
    hello(X).

hello(N) ->
   case N of
   	help -> io:fwrite("help cmd~n");
   	cd -> io:fwrite("cd cmd~n");
   	pwd -> io:fwrite("pwd cmd~n");
   	ls -> io:fwrite("ls cmd~n");
   	cp -> io:fwrite("cp cmd~n");
   	exit -> io:fwrite("exit cmd~n");
   	_ -> io:fwrite("unkown cmd~n")
   end.

