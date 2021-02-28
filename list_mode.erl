-module(list_mode).
-export([start_list_mode/0]).
-import(lists, [all/2, any/2, filter/2, reverse/1, reverse/2,
		foreach/2, map/2, member/2, sort/1]).
-import(string,[equal/2]). 
-record(partition, {type, version}).

start_list_mode() ->
    Path = "/sys/block/",
    {ok, L} = file:list_dir(Path),
    map(fun(I) -> {I, process_block_dir([I], Path)} end, sort(L)).
    
process_block_dir(Device, Path) ->
	Prefix = "sd",
	B = string:find(Device, Prefix) =:= Device,
	read_disk_dir(Path ++ Device ++ "/", B).
	
read_disk_dir(Device, IsDisk) ->
	case IsDisk of
		true ->
			{ok, L} = file:list_dir(Device),
			map(fun(I) -> {I, process_disk_dir([I], Device)} end, sort(L));
		_ -> ok
	end.
	
process_disk_dir(Device, Path) ->
	Prefix = "sd",
	B = string:find(Device, Prefix) =:= Device,
	process_dev_file(Device, Path, (Path -- "/sys/block/") -- "/"),
	read_partition_dir(Path ++ Device ++ "/", B, Device).
	
read_partition_dir(Device, IsPartition, DevFile) ->
	case IsPartition of
		true ->
			{ok, L} = file:list_dir(Device),
			map(fun(I) -> {I, process_dev_file([I], Device, DevFile)} end, L);
		_ -> ok
	end.
	
process_dev_file(Filename, Path, PartName) ->
	Dev = "dev",
	B = equal(Filename, Dev),
	case B of
		true ->
			{ok, S} = file:open(Path ++ Dev, read),
			Line = io:get_line(S, ''),
			Properties = read_device_properties([Line -- "\n"]),
			io:fwrite("device: ~s~n", [PartName]),
			case Properties#partition.type of
				[] -> ok;
				_ -> io:fwrite("\tFS type: ~s~n", [Properties#partition.type])
			end,
			case Properties#partition.version of
				[] -> ok;
				_ -> io:fwrite("\tFS version: ~s~n", [Properties#partition.version])
			end;
		_ -> ok
	end.
	
read_device_properties(MajorMinor) ->
	Udevfile = "/run/udev/data/b",
	{ok, F} = file:open(Udevfile ++ MajorMinor, read),
	Partition = #partition{type = "", version = ""},
	X = try populate_partition(F, Partition)
      		after file:close(F)
    	end,
    	X.
	
populate_partition(File, Partition) ->
	case io:get_line(File, "") of
        	eof  -> Partition;
        	Line -> 
        		FS_TYPE_PROPERTY = "E:ID_FS_TYPE",
        		FS_VERSION_PROPERTY = "E:ID_FS_VERSION",
			FS_TYPE = string:find(Line, FS_TYPE_PROPERTY) =:= Line,
			FS_VERSION = string:find(Line, FS_VERSION_PROPERTY) =:= Line,
			Type = case FS_TYPE of
				true -> (Line -- (FS_TYPE_PROPERTY ++ "=")) -- "\n";
        			_ -> Partition#partition.type
        		end,
        		Version = case FS_VERSION of
				true -> (Line -- (FS_VERSION_PROPERTY ++ "=")) -- "\n";
        			_ -> Partition#partition.version
        		end,
        		populate_partition(File, Partition#partition{type = Type, version = Version})
    	end.
