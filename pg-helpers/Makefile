
IDRIS := idris2

TARGET = libpg-idris
TARGET_VERSION ?= 0.0.3

INSTALLDIR = `${IDRIS} --libdir`/pg-idris-${TARGET_VERSION}/lib

LDFLAGS = -L$(shell pg_config --libdir) -lpq
CPPFLAGS = -I$(shell pg_config --includedir)

CC_VERSION = $(shell $(CC) --version)
MACHINE := $(shell $(CC) -dumpmachine)
ifneq (,$(findstring cygwin, $(MACHINE)))
	OS := windows
	SHLIB_SUFFIX := .dll
else ifneq (,$(findstring mingw, $(MACHINE)))
	OS := windows
	SHLIB_SUFFIX := .dll
else ifneq (,$(findstring windows, $(MACHINE)))
	OS := windows
	SHLIB_SUFFIX := .dll
else ifneq (,$(findstring darwin, $(MACHINE)))
	OS := darwin
	SHLIB_SUFFIX := .dylib
else ifneq (, $(findstring bsd, $(MACHINE)))
	OS := bsd
	SHLIB_SUFFIX := .so
else
	OS := linux
	SHLIB_SUFFIX := .so
endif

ifeq ($(findstring clang,$(CC_VERSION)),clang)
  LDFLAGS += ''
else
  LDFLAGS += -fuse-ld=gold
endif

SRCS = $(wildcard *.c)
OBJS = $(SRCS:.c=.o)
DEPS = $(OBJS:.o=.d)

all: $(TARGET)$(SHLIB_SUFFIX)

$(TARGET)$(SHLIB_SUFFIX): $(OBJS)
	$(CC) -shared $(LDFLAGS) -o $@ $^

-include $(DEPS)

%.d: %.c
	@$(CPP) $(CFLAGS) $(CPPFLAGS) $< -MM -MT $(@:.d=.o) >$@

.PHONY: clean

clean :
	rm -f $(OBJS) $(TARGET)$(SHLIB_SUFFIX)

cleandep: clean
	rm -f $(DEPS)

.PHONY: install

install:
	@if ! [ -d $(INSTALLDIR) ]; then mkdir -p $(INSTALLDIR); fi
	install $(TARGET)$(SHLIB_SUFFIX) $(wildcard *.h) $(INSTALLDIR)
