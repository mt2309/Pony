#ifndef PONY_CLASS_H
#define PONY_CLASS_H

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

void initialise(void);

struct static_clazz;
struct pony_clazz;

/* pony_meths are function pointers:
   takes a clazz pointer to `this`, then an array of clazz* as the parameters
   returning an array of parameters.
*/
typedef union variable** (*pony_meth)(struct pony_clazz*, union variable**);

/* pony_constructs are the default class constructors for each class
   called before each class is built.
*/
typedef struct pony_clazz* (*pony_construct)(union variable**);

/* vtables are linked lists consisting of:
   - an identifier
   - a method
   - a pointer to the next method.
*/
typedef struct vtable
{
    unsigned int identifier;
    pony_meth meth;
    struct vtable * next;
} vtable;

/* static class information consisting of:
   - pointer to vtable
   - pointer to the class bitset
*/
typedef struct static_clazz
{
    vtable * vtab;
    unsigned int * clazz_set;
} static_clazz;

struct instance_variable;

/* a pony class representation at runtime, consisting of:
   - a pointer to the static class information
   - a pointer to an array of bitsets marking the type parameter of the current class.
   - a pointer to a table of instance variables
*/
typedef struct pony_clazz
{
    static_clazz * static_class_info;
    unsigned int ** type_info;
    struct instance_variable * variables;
} pony_clazz;

/* a union representing either a class
   or a primitive type, such as an int
*/
typedef union variable
{
    pony_clazz* clazz_value;
    double double_value;
    int int_value;
    bool bool_value;
    long long_value;
    char char_value;
} variable;

/* instance variables consist of:
   - an identifier
   - a pointer to the value
   - a pointer to the the next variable
*/
typedef struct instance_variable
{
    unsigned int identifier;
    variable * value;
    struct instance_variable * next;
} instance_variable;

// Helper method prototypes
unsigned int*
initialise_bit_set(unsigned int clazz_id);

pony_meth
lookup_meth(vtable*, unsigned int);

variable*
lookup_value(pony_clazz*, unsigned int);

void
set_value(pony_clazz *, variable *, unsigned int);

bool
is_sub_type(unsigned int *, unsigned int *);

vtable*
new_vtable(unsigned int, unsigned int *, pony_meth *);

pony_clazz*
new_pony_clazz(static_clazz *, unsigned int **);

void
create_instance_variables(pony_clazz *, variable **, unsigned int *, unsigned int);

static_clazz*
initialise_static_class(unsigned int * clazz_id, unsigned int method_count,
                        unsigned int * identifiers, pony_meth * methods);

variable*
create_clazz_var(pony_clazz* v);

variable*
create_double_var(double d);

variable*
create_int_var(int i);

variable*
create_bool_var(bool b);

variable*
create_long_var(long l);

variable*
create_char_var(char c);

variable**
call_method(pony_clazz*, unsigned int id, variable ** arguments);

variable**
create_args(unsigned int count, ...);

pony_meth*
create_meths(unsigned int count, ...);

unsigned int*
create_ids(unsigned int count, ...);

#endif
