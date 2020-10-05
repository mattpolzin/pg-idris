
#include "pg-helpers.h"
#include "stdio.h"

int is_null(void* ref) {
	return ref == NULL ? 1 : 0;
}

char* string_value(void* s) {
	return (char*)s;
}

PGnotify* notify_struct(void* s) {
	return (PGnotify*)s;
}

int main() {}
