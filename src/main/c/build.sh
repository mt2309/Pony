#!/usr/local/bin/bash

clang -std=c11 -Weverything -Wno-padded -Wno-unused-parameter -Wno-unused-label pony_class_ids.c pony_class.c
