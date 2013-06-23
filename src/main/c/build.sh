#!/usr/local/bin/bash

clang -std=c11 -Weverything -Wno-padded -Wno-unused-parameter -Wno-unused-label -Wno-unused-variable -g pony_class_ids.c pony_class.c
