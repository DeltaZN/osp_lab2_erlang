#!/usr/bin/env escript
-import(shell_mode, [start_shell_mode/1]).

main(Args) ->
    [Mode|Rest] = Args,
    handle(list_to_atom(Mode), Rest).
    
handle(Mode, Args) -> 
    case Mode of
   	shell -> 
   		start_shell_mode(Args);
   	list -> io:fwrite("list mode~n");
   	_ -> io:fwrite("unkown mode~n")
   end. 
