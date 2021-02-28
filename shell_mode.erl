-module(shell_mode).
-export([start_shell_mode/1]).
-import(fat32,[open_partition/1]). 

start_shell_mode([]) ->
    io:fwrite("Please, specify target volume~n");
start_shell_mode(Args) ->
    [Volume|_] = Args,
    try 
    	open_partition(Volume),
    	io:fwrite("FAT32 supported.~n"),
    	read_and_handle()
    catch
    	_:_ -> io:fwrite("FAT32 not supported!~n")
    end.
    
read_and_handle() ->
    {ok, [X]} = io:fread("shell> ", "~a"),
    handle(X).

handle(N) ->
   case N of
   	help -> help(), read_and_handle();
   	cd -> io:fwrite("cd cmd~n"), read_and_handle();
   	pwd -> io:fwrite("pwd cmd~n"), read_and_handle();
   	ls -> io:fwrite("ls cmd~n"), read_and_handle();
   	cp -> io:fwrite("cp cmd~n"), read_and_handle();
   	exit -> io:fwrite("Terminating programm~n");
   	_ -> io:fwrite("unkown cmd~n"), read_and_handle()
   end.

help() ->
    io:fwrite("cd [arg] - change working directory\n"),
    io:fwrite("pwd - print working directory full name\n"),
    io:fwrite("cp [arg] - copy dir or file to mounted device\n"),
    io:fwrite("ls - show working directory elements\n"),
    io:fwrite("exit - terminate program\n"),
    io:fwrite("help - print help\n").
