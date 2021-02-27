
#ifndef _PG_IDRIS_HELPERS_H
#define _PG_IDRIS_HELPERS_H

#include <libpq-fe.h>

int is_null(void* ref);

char* string_value(void* s);
PGnotify* notify_struct(void* s);

char *connErrorMessage(const PGconn *conn);

int socket_wait(PGconn* conn);

#endif
