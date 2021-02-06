IDRIS := idris2
LIBDIR = `${IDRIS} --librdir`

TARGET = example

LDFLAGS = -L$(LIBDIR)/pg-idris/lib
CPPFLAGS = -I$(LIBDIR)/pg-idris/lib
LD_LIBRARY_PATH += $(LIBDIR)/pg-idris/lib

all: build

.PHONY: build

build:
	$(IDRIS) --build $(TARGET).ipkg

.PHONY: clean

clean:
	rm -rf ./build
