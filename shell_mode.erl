-module(shell_mode).
-export([start_shell_mode/1]).
-import(fat32,[open_partition/1, change_dir/1, read_dir/0]). 

start_shell_mode([]) ->
    io:fwrite("Please, specify target volume~n");
start_shell_mode(Args) ->
    [Volume|_] = Args,
    try 
    	X = open_partition(Volume),
    	io:fwrite("FAT32 supported.~n")
    catch
    	_:_ -> io:fwrite("FAT32 not supported!~n")
    after
    	read_and_handle("/" ++ Volume ++ "/")
    end.
    
read_and_handle(Path) ->
    io:fwrite("~s", [Path]),
    {ok, [X]} = io:fread("$ ", "~a"),
    handle(X, Path).

handle(N, Path) ->
   case N of
   	help -> help(), read_and_handle(Path);
   	cd -> 
   		{ok, [X]} = io:fread("", "~a"), 
   		read_and_handle(cd(X, Path));
   	pwd -> io:fwrite("~s~n", [Path]), read_and_handle(Path);
   	ls -> ls(), read_and_handle(Path);
   	cp -> io:fwrite("cp cmd~n"), read_and_handle(Path);
   	exit -> io:fwrite("Terminating programm~n");
   	_ -> io:fwrite("unkown cmd~n"), read_and_handle(Path)
   end.
   
ls() ->
	read_dir().
   
cd(Arg, Path) -> 
    try
    	Arg_Str = atom_to_list(Arg),
    	change_dir(Arg_Str),
    	case Arg_Str of
    		".." -> Path -- "/";
    		"." -> Path;
    		_ -> Path ++ Arg_Str ++ "/"
    	end
    catch
    	_:_ -> io:fwrite("Directory not found.~n"), Path
    end.

help() ->
    io:fwrite("cd [arg] - change working directory\n"),
    io:fwrite("pwd - print working directory full name\n"),
    io:fwrite("cp [arg] - copy dir or file to mounted device\n"),
    io:fwrite("ls - show working directory elements\n"),
    io:fwrite("exit - terminate program\n"),
    io:fwrite("help - print help\n").
