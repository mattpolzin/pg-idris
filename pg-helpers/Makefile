
IDRIS := idris2
INSTALLDIR = `${IDRIS} --libdir`/pg-idris/lib

TARGET = libpg-idris

LDFLAGS = -L$(shell pg_config --libdir) -lpq
CPPFLAGS = -I$(shell pg_config --includedir)
LD_LIBRARY_PATH += $(shell pg_config --includedir)

CC_VERSION = $(shell $(CC) --version)

ifeq ($(findstring clang,$(CC_VERSION)),clang)
 DYLIB_WORKAROUND = cp "${INSTALLDIR}/${TARGET}" "${INSTALLDIR}/${TARGET}.dylib"
else
 DYLIB_WORKAROUND = cp "${INSTALLDIR}/${TARGET}" "${INSTALLDIR}/${TARGET}.so"
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
	@$(CPP) $(CFLAGS) $< -MM -MT $(@:.d=.o) >$@

.PHONY: clean

clean :
	rm -f $(OBJS) $(TARGET)$(SHLIB_SUFFIX)

cleandep: clean
	rm -f $(DEPS)


.PHONY: install

install:
	@if ! [ -d $(INSTALLDIR) ]; then mkdir -p $(INSTALLDIR); fi
	install $(TARGET)$(SHLIB_SUFFIX) $(wildcard *.h) $(INSTALLDIR)
	$(DYLIB_WORKAROUND)
