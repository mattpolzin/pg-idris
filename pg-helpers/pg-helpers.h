
#ifndef _pg_idris_helpers
#define _pg_idris_helpers

#include "libpq-fe.h"

int is_null(void* ref);

char* string_value(void* s);
PGnotify* notify_struct(void* s);

#endif
