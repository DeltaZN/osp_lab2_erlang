//
// Created by georgii on 28.02.2021.
//

#include <erl_nif.h>
#include "fat32_lib.c"

static struct partition_value* stored;

static ERL_NIF_TERM open_partition_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char partition_name[256] = {0};
    struct partition_value* ret;
    if (!enif_get_string(env, argv[0], partition_name, 256, ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }
    ret = open_partition(partition_name);
    if (ret) {
    	ERL_NIF_TERM term = enif_make_resource(env, ret);
    	stored = ret;
    	return term;
    } else {
    	return enif_make_badarg(env);
    }
}

static ErlNifFunc nif_funcs[] = {
        {"open_partition", 1, open_partition_nif},
};

ERL_NIF_INIT(fat32, nif_funcs, NULL, NULL, NULL, NULL)
