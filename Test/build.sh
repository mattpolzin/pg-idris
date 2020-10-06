#!/usr/bin/env bash

export LDFLAGS="-L/usr/local/Cellar/idris2/0.2.1/libexec/idris2-0.2.1/pg-idris/lib"
export CPPFLAGS="-I/usr/local/Cellar/idris2/0.2.1/libexec/idris2-0.2.1/pg-idris/lib"
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/Cellar/idris2/0.2.1/libexec/idris2-0.2.1/pg-idris/lib

idris2 Test.idr -p pg-idris -p contrib -o pg-test
