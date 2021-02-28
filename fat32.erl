-module(fat32).
-export([open_partition/1, change_dir/1, read_dir/0]).
-on_load(init/0).

init() ->
    ok = erlang:load_nif("./fat32_nif", 0).

open_partition(_X) ->
    exit(nif_library_not_loaded).
    
change_dir(_X) ->
    exit(nif_library_not_loaded).
    
read_dir() ->
    exit(nif_library_not_loaded).
