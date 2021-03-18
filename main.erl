#!/usr/bin/env escript
-import(shell_mode, [start_shell_mode/1]).
-import(list_mode, [start_list_mode/0]).
-import(string, [tokens/2]).

main([]) ->
    %K = tokens("cd New_Project arg", "\""),
    %io:format("~p~n", [K]),
    io:fwrite("Please, specify program mode (shell/list)~n");
main(Args) ->
    [Mode|Rest] = Args,
    handle(list_to_atom(Mode), Rest).
    
handle(Mode, Args) -> 
    case Mode of
   	shell -> 
   		io:fwrite("Starting shell mode...~n"),
   		start_shell_mode(Args);
   	list -> 
   		io:fwrite("Starting list mode...~n"),
   		start_list_mode();
   	_ -> io:fwrite("Unkown mode~n")
   end. 
