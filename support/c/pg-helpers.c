
#include "pg-helpers.h"
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <sys/types.h>
#include <string.h>
#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif

/**
 * Checks whether the given pointer
 * is null or not. Returns 0 for false
 * and 1 for true.
 */
int is_null(void* ref) {
	return ref == NULL ? 1 : 0;
}

/**
 * Assumes the given pointer is a
 * non-null char* and casts it as
 * such.
 */
char* string_value(void* s) {
	return (char*)s;
}

/**
 * We are not supposed to free the result
 * of PQerrorMessage but Idris insists on
 * freeing any char* it is passed so this
 * function exists to copy the Idris error
 * so it can be freed :/ 
 */
char *connErrorMessage(const PGconn *conn) {
	char * orig = PQerrorMessage(conn);
	char * copy = malloc(strlen(orig) + 1); 
	strcpy(copy, orig);
	return copy;
}

/**
 * Assumes the given pointer is a
 * non-null PGnotify struct and casts
 * it as such.
 */
PGnotify* notify_struct(void* s) {
	return (PGnotify*)s;
}

/**
 * Wait on a socket for any activity on the
 * server. Negative returns are failures. Anything
 * else should be considered OK.
 */
int socket_wait(PGconn* conn) {
	/*
         * Sleep until something happens on the connection.
         */
        int         sock;
        fd_set      input_mask;

        sock = PQsocket(conn);

        if (sock < 0)
            return -1;              /* shouldn't happen */

        FD_ZERO(&input_mask);
        FD_SET(sock, &input_mask);

	return select(sock + 1, &input_mask, NULL, NULL, NULL);
}

int main() {}
