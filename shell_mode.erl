-module(shell_mode).
-export([start_shell_mode/1]).
-import(fat32,[open_partition/1, change_dir/1, read_dir/0, cp/2]). 
-import(lists,[droplast/1, dropwhile/2, last/1]). 
-import(string,[trim/1, trim/3]).

start_shell_mode([]) ->
    io:fwrite("Please, specify target volume~n");
start_shell_mode(Args) ->
    [Volume|_] = Args,
    try 
    	X = open_partition(Volume),
    	io:fwrite("FAT32 supported.~n"),
    	read_and_handle("/" ++ Volume ++ "/")
    catch
    	_:_ -> io:fwrite("FAT32 not supported!~n")
    end.
    
read_and_handle(Path) ->
    io:fwrite("~s", [Path]),
    {ok, [X]} = io:fread("$ ", "~a"),
    handle(X, Path).

handle(N, Path) ->
   case N of
   	help -> 
   		help(), 
   		read_and_handle(Path);
   	cd -> 
   		X = io:get_line(""), 
   		read_and_handle(cd(trim(trim(X), both, "\""), Path));
   	pwd -> io:fwrite("~s~n", [Path]), read_and_handle(Path);
   	ls -> ls(), read_and_handle(Path);
   	cp -> 
   		X = io:get_line(""), 
   		cp_cmd(trim(trim(X), both, "\"")),
   		read_and_handle(Path);
   	exit -> io:fwrite("Terminating programm~n");
   	_ -> io:fwrite("unkown cmd~n"), read_and_handle(Path)
   end.
   
ls() ->
	read_dir().
	
cp_cmd(Arg) -> 
    try
    	cp(Arg, "/home/georgii/fat32copy")
    catch
    	_:_ -> io:fwrite("Directory not found.~n")
    end.
   
cd(Arg, Path) -> 
    try
    	change_dir(Arg),
    	case Arg of
    		".." -> dropwhile_reverse($/, droplast(Path));
    		"." -> Path;
    		_ -> Path ++ Arg ++ "/"
    	end
    catch
    	_:_ -> io:fwrite("Directory not found.~n"), Path
    end.
    
dropwhile_reverse(Sym, Path) -> 
	B = last(Path) /= Sym,
	case B of
		true -> dropwhile_reverse(Sym, droplast(Path));
		_ -> Path
	end.

help() ->
    io:fwrite("cd [arg] - change working directory\n"),
    io:fwrite("pwd - print working directory full name\n"),
    io:fwrite("cp [arg] - copy dir or file to mounted device\n"),
    io:fwrite("ls - show working directory elements\n"),
    io:fwrite("exit - terminate program\n"),
    io:fwrite("help - print help\n").
