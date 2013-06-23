#ifndef pony_pony_h
#define pony_pony_h

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/** Opaque definition of an actor.
 *
 * The internals of an actor aren't visible to the programmer.
 */
typedef struct actor_t actor_t;

/** Message ID for the main function.
 *
 * When the pony runtime starts, it extracts runtime specific command line
 * arguments and then sends the the remaining command line to an initial actor.
 */
#define PONY_MAIN 0

/** Command line arguments.
 *
 * The initial actor receives the command line arguments packaged in this
 * structure.
 */
typedef struct pony_main_t
{
  int argc;
  char** argv;
} pony_main_t;

/** Argument to a message.
 *
 * This union type is used to make sending primitive types simpler.
 */
typedef union
{
  void* p;
  intptr_t i;
  double d;
} arg_t;

/** Type mode.
 *
 * Indicates the mode of the object.
 */
typedef enum
{
  PONY_ACTOR,
  PONY_ISOLATED,
  PONY_WRITEABLE,
  PONY_FROZEN,
  PONY_OPAQUE
} type_mode_t;

/** Type function.
 *
 * There are several instances where a function is called on an object. This
 * signature is used.
 */
typedef void (*type_fn)(void* p);

/** Type descriptor.
 *
 * This provides the functions needed for garbage collection, serialisation,
 * deserialisation, etc.
 */
typedef struct type_t
{
  type_mode_t mode;
  type_fn mark;
} type_t;

/** Dispatch function.
 *
 * Each actor has a dispatch function that is invoked when the actor handles
 * a message. The actor, a pointer to the actor's data, a type descriptor for
 * actor's data, the message ID and the message argument are provided.
 */
typedef void (*dispatch_t)(actor_t* this, void* p, type_t* type, uint64_t id,
  arg_t arg);

/** Create a new actor.
 *
 * When an actor is created, it is provided a dispatch function (required) and
 * a finaliser (optional, can be NULL).
 *
 * An actor must not send messages in its finaliser. This is because other
 * actors reachable from that actor may already have been collected.
 */
actor_t* pony_create(dispatch_t disp, type_fn final);

/** Set the actor's data.
 *
 * This sets the actor's data with a pointer and a type descriptor. This can
 * only be called while handling a message. Generally, an actor will do this
 * only once, when it handles its constructor message.
 */
void pony_set(void* p, type_t* type);

/** Sends a message to an actor.
 *
 * Sends a message and an object pointer (with a type descriptor) to another
 * actor. Since actor references can't be synthesised, the sending actor must
 * have created the target actor or have been sent a reference to it.
 */
actor_t * pony_send(actor_t* to, uint64_t id, void* p, type_t* type);

/// Convenience function to send an integer argument in a message
void pony_sendi(actor_t* to, uint64_t id, intptr_t i);

/// Convenience function to send a floating point argument in a message
void pony_sendd(actor_t* to, uint64_t id, double d);

/** Allocate memory on the current actor's heap.
 *
 * This is garbage collected memory. This can only be done while an actor is
 * handling a message, so that there is a current actor.
 */
void* pony_malloc(size_t size)
  __attribute__((malloc,alloc_size(1)));

/** Convenience function for allocating arrays.
 *
 * Note that the memory returned has not necessarily been zeroed out.
 */
void* pony_calloc(size_t count, size_t size)
  __attribute__((malloc,alloc_size(1,2)));

/** Marks an actor as still reachable during garbage collection.
 */
void pony_markactor(actor_t* actor);

/** Marks an isolated object as still reachable during garbage collection.
 */
void pony_markisolated(void* p, type_fn f);

/** Marks a writeable object as still reachable during garbage collection.
 */
void pony_markwriteable(void* p, type_fn f);

/** Marks a frozen object as still reachable during garbage collection.
 */
void pony_markfrozen(void* p, type_fn f);

/** Marks an opaque object as still reachable during garbage collection.
 */
void pony_markopaque(void* p);

/** Starts the pony runtime.
 *
 * Takes the command line arguments and the initial actor. The initial actor
 * will be sent (PONY_MAIN, {argc, argv}) once the runtime is initialised.
 */
int pony_start(int argc, char** argv, actor_t* actor);

/** Temporary function.
 *
 * Used to stop the runtime. Will be removed once the cycle detector is in.
 */
void pony_stop();

/** Set the exit code.
 *
 * The value returned by pony_start() will be 0 unless set to something else
 * with this call. If called more than once, the value from the last call is
 * returned.
 */
void pony_exitcode(int code);

#ifdef __cplusplus
}
#endif

#endif
