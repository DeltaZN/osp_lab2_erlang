#!/bin/bash
gcc -o fat32_nif.so -fpic -shared fat32_nif.c
erl -compile fat32
erl -compile shell_mode
erl -compile list_mode
