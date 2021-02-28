-module(fat32).
-export([open_partition/1]).
-on_load(init/0).

init() ->
    ok = erlang:load_nif("./fat32_nif", 0).

open_partition(_X) ->
    exit(nif_library_not_loaded).
