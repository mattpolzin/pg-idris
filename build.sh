#!/usr/bin/env bash

export LDFLAGS="-L/usr/local/opt/libpq/lib"
export CPPFLAGS="-I/usr/local/opt/libpq/include"
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/opt/libpq/include
idris2 --build pg-idris.ipkg
idris2 --install pg-idris.ipkg
