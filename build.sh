#!/usr/bin/env bash

export LDFLAGS="-L$(pg_config --libdir) -lpq"
export CPPFLAGS="-I$(pg_config --includedir)"
export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:$(pg_config --includedir)"
idris2 --build pg-idris.ipkg
idris2 --install pg-idris.ipkg

if [[ "$OSTYPE" == "darwin"* ]]; then
	LIBDIR="$(idris2 --libdir)/pg-idris/lib"
	FNAME="libpg-idris"
	cp "$LIBDIR/$FNAME" "$LIBDIR/$FNAME.dylib"
fi
