IDRIS := idris2
LIBDIR = `${IDRIS} --librdir`

TARGET = Main

LDFLAGS = -L$(LIBDIR)/pg-idris/lib
CPPFLAGS = -I$(LIBDIR)/pg-idris/lib
LD_LIBRARY_PATH += $(LIBDIR)/pg-idris/lib

all: build

.PHONY: build

build:
	$(IDRIS) $(TARGET).idr -p pg-idris -p contrib -o pg-test

.PHONY: clean

clean:
	rm -rf ./build
