//
// Created by georgii on 28.02.2021.
//

#include <erl_nif.h>
#include <dirent.h>
#include <sys/stat.h>
#include <argp.h>
#include "fat32_lib.c"

static struct partition_value* stored;

int error(const char *msg)
{
    printf("Error: %s\n", msg);
    return -1;
}


int parse(const char *cmd, char **args)
{
    const char *p = cmd;
    int count = 0;

    for (;;) {
        while (isspace(*p)) p++;
        if (count >= 3) {
            return count;
        }
        if (*p == '\0') break;

        if (*p == '"' || *p == '\'') {
            int quote = *p++;
            const char *begin = p;

            while (*p && *p != quote) p++;
            if (*p == '\0') return error("Unmachted quote");
            strncpy(args[count], begin, p-begin);
            count++;
            p++;
            continue;
        }

        if (strchr("<>()|", *p)) {
            args[count] = calloc(1, 256);
            strncpy(args[count], p, 1);
            count++;
            p++;
            continue;
        }

        if (isalnum(*p) || *p == '.' || *p == '/') {
            const char *begin = p;

            while (isalnum(*p) || *p == '.' || *p == '/') p++;
            strncpy(args[count], begin, p-begin);
            count++;
            continue;
        }

        return error("Illegal character");
    }

    return count;
}

void append_path_part(char *path, const char *part) {
    strcat(path, "/");
    strcat(path, part);
}

int check_directory(const char *path) {
    DIR *dir = opendir(path);
    if (dir) return 1;
    else return 0;
}

void copy_file(struct partition_value *part, char *dest, struct dir_value *file) {
    if (file->type != 'f') {
        printf("Not a file\n");
        return;
    }
    char *buf = malloc(part->cluster_size);
    u_int32_t fat_record = file->first_cluster;
    int fd = open(dest, O_RDWR | O_APPEND | O_CREAT, 0777);
    u_int32_t size = file->size < part->cluster_size ? file->size : part->cluster_size;
    while (fat_record < 0x0FFFFFF7) {
        fat_record = read_file_cluster(part, fat_record, buf);
        write(fd, buf, size);
    }
    free(buf);
    close(fd);
}

void copy_dir(struct partition_value *part, char *dest, struct dir_value *file) {
    if (file->type != 'd') {
        printf("Not a dir\n");
        return;
    }
    struct stat dir = {0};
    if (stat(dest, &dir) == -1) {
        mkdir(dest, 0777);
    }
    struct dir_value *dir_val = read_dir(file->first_cluster, part);
    while (dir_val != NULL) {
        if (strcmp((char*)dir_val->filename, ".") != 0 && strcmp((char*)dir_val->filename, "..") != 0) {
            char *path = calloc(1, 512);
            strcat(path, dest);
            append_path_part(path, (char*)dir_val->filename);
            if (dir_val->type == 'd') {
                copy_dir(part, path, dir_val);
            } else {
                copy_file(part, path, dir_val);
            }
            free(path);
        }
        dir_val = dir_val->next;
    }
    destroy_dir_value(dir_val);
}

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

static ERL_NIF_TERM cp_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char arg[512] = {0};
    int32_t ret = 0;
    if (!enif_get_string(env, argv[0], arg, 512, ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }
    char *line;
    char *args[2];
    args[0] = calloc(1, 256);
    args[1] = calloc(1, 256);
    parse(arg, args);
    struct dir_value *dir_value = read_dir(stored->active_cluster, stored);
                char copied = 0;
                while (dir_value != NULL) {
                    if (!strcmp((char*)dir_value->filename, args[0])) {
                        if (check_directory(args[1])) {
                            char filename[256] = {0};
                            strcpy(filename, args[1]);
                            size_t str_len = strlen(args[1]);
                            if (filename[str_len - 1] != '/') {
                                strcat(filename, "/");
                            }
                            strcat(filename, (char *) dir_value->filename);

                            if (dir_value->type == 'd') {
                                copy_dir(stored, filename, dir_value);
                            } else {
                                copy_file(stored, filename, dir_value);
                            }
                            copied = 1;
                            break;
                        } else {
                            printf("Target Directory doesn't exist\n");
                        }
                    }
                    dir_value = dir_value->next;
                }
    destroy_dir_value(dir_value);
    if (copied) {
    	ERL_NIF_TERM term = enif_make_int(env, ret);
    	return term;
    } else {
    	return enif_make_badarg(env);
    }
}


static ErlNifFunc nif_funcs[] = {
        {"open_partition", 1, open_partition_nif},
        {"change_dir", 1, change_dir_nif},
        {"read_dir", 0, read_dir_nif},
        {"cp", 2, cp_nif},
};

ERL_NIF_INIT(fat32, nif_funcs, NULL, NULL, NULL, NULL)
