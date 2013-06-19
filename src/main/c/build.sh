#!/usr/local/bin/bash

clang -std=c11 -Weverything -Wno-padded pony_class_ids.c pony_class.c
