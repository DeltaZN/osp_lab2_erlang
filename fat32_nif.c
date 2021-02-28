//
// Created by georgii on 28.02.2021.
//

#include <erl_nif.h>
#include "fat32_lib.c"

void print_dir(struct dir_value *pValue) {
    while (pValue != NULL) {
        if (pValue->type == 'd') {
            printf("DIR %s %d\n", pValue->filename, pValue->first_cluster);
        } else {
            printf("FILE %s (%d bytes)\n", pValue->filename, pValue->size);
        }
        pValue = pValue->next;
    }
}

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

static ERL_NIF_TERM change_dir_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char dir_name[256] = {0};
    int32_t ret = 0;
    if (!enif_get_string(env, argv[0], dir_name, 256, ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }
    ret = change_dir(stored, dir_name);
    if (ret) {
    	ERL_NIF_TERM term = enif_make_int(env, ret);
    	return term;
    } else {
    	return enif_make_badarg(env);
    }
}

static ERL_NIF_TERM read_dir_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    struct dir_value *ret;
    ret = read_dir(stored->active_cluster, stored);
    if (ret) {
    	ERL_NIF_TERM term = enif_make_resource(env, ret);
    	print_dir(ret);
    	destroy_dir_value(ret);
    	return term;
    } else {
    	return enif_make_badarg(env);
    }
}

static ErlNifFunc nif_funcs[] = {
        {"open_partition", 1, open_partition_nif},
        {"change_dir", 1, change_dir_nif},
        {"read_dir", 0, read_dir_nif},
};

ERL_NIF_INIT(fat32, nif_funcs, NULL, NULL, NULL, NULL)
