-module(shell_mode).
-export([start_shell_mode/1]).

start_shell_mode([]) ->
    io:fwrite("Please, specify target volume~n");
start_shell_mode(Args) ->
    [Volume|_] = Args,
    read_and_handle().
    
read_and_handle() ->
    {ok, [X]} = io:fread("shell> ", "~a"),
    handle(X).

handle(N) ->
   case N of
   	help -> io:fwrite("help cmd~n"), read_and_handle();
   	cd -> io:fwrite("cd cmd~n"), read_and_handle();
   	pwd -> io:fwrite("pwd cmd~n"), read_and_handle();
   	ls -> io:fwrite("ls cmd~n"), read_and_handle();
   	cp -> io:fwrite("cp cmd~n"), read_and_handle();
   	exit -> io:fwrite("Terminating programm~n");
   	_ -> io:fwrite("unkown cmd~n"), read_and_handle()
   end.

