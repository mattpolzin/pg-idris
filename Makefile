INTERACTIVE ?= --interactive

IDRIS := idris2

PACKAGE = pg-idris.ipkg

INDEXED_VERSION = 0.0.9
INDEXED_RELATIVE_DIR = indexed-${INDEXED_VERSION}
IDRIS_LIB_DIR := $(shell ${IDRIS} --libdir)

.PHONY: all deps build clean install test check-readme

all: deps build

./depends/${INDEXED_RELATIVE_DIR}:
	mkdir -p ./build/deps
	mkdir -p ./depends
	cd ./build/deps && \
	git clone https://github.com/mattpolzin/idris-indexed.git && \
	cd idris-indexed && \
	git checkout ${INDEXED_VERSION} && \
	make && \
	cp -R ./build/ttc ../../../depends/${INDEXED_RELATIVE_DIR}/

./depends/idris2-elab-util:
	mkdir -p ./build/deps
	mkdir -p ./depends
	cd ./build/deps && \
	git clone https://github.com/stefan-hoeck/idris2-elab-util.git && \
	cd idris2-elab-util && \
	$(IDRIS) --build elab-util.ipkg && \
	cp -R ./build/ttc ../../../depends/elab-util-0/

./depends/idris2-parser:
	mkdir -p ./build/deps
	mkdir -p ./depends
	cd ./build/deps && \
	git clone https://github.com/stefan-hoeck/idris2-parser.git && \
	cd idris2-parser && \
	IDRIS2_PACKAGE_PATH=../../../depends $(IDRIS) --build parser.ipkg && \
	cp -R ./build/ttc ../../../depends/parser-0/

# parser-json depends on elab-util directly but it does not specify it
# because elab-util is indirectly depended upon via parser already. I have
# not figured out why, but my Makefile invocation of idris2 needs it defined
# as a direct dependency even though pack does not.
define PATCH
5c5,6
< depends   = parser
---
> depends   = elab-util
>           , parser
endef

export PATCH
./depends/idris2-parser/json:
	mkdir -p ./build/deps
	mkdir -p ./depends
	cd ./build/deps && \
	cd idris2-parser/json && \
  echo "$$PATCH" | patch parser-json.ipkg - && \
	IDRIS2_PACKAGE_PATH=../../../../depends $(IDRIS) --build parser-json.ipkg && \
	cp -R ./build/ttc ../../../../depends/parser-json-0/

deps: ./depends/${INDEXED_RELATIVE_DIR} ./depends/idris2-elab-util ./depends/idris2-parser ./depends/idris2-parser/json

build:
	$(IDRIS) --build $(PACKAGE)

clean:
	rm -rf ./depends
	rm -rf ./build/deps
	rm -rf ./tests/build
	$(IDRIS) --clean $(PACKAGE)

install:
	$(IDRIS) --install $(PACKAGE)
	mkdir -p $(IDRIS_LIB_DIR)/${INDEXED_RELATIVE_DIR} && \
	cp -R ./depends/${INDEXED_RELATIVE_DIR} $(IDRIS_LIB_DIR)/ && \
	cp -R ./depends/elab-util-0/ $(IDRIS_LIB_DIR)/ && \
	cp -R ./depends/parser-0/ $(IDRIS_LIB_DIR)/ && \
	cp -R ./depends/parser-json-0/ $(IDRIS_LIB_DIR)/
	
install-with-src:
	$(IDRIS) --install-with-src $(PACKAGE)
	mkdir -p $(IDRIS_LIB_DIR)/${INDEXED_RELATIVE_DIR} && \
	cp -R ./depends/${INDEXED_RELATIVE_DIR} $(IDRIS_LIB_DIR)/ && \
	cp -R ./depends/elab-util-0/ $(IDRIS_LIB_DIR)/ && \
	cp -R ./depends/parser-0/ $(IDRIS_LIB_DIR)/ && \
	cp -R ./depends/parser-json-0/ $(IDRIS_LIB_DIR)/

test:
	cd tests && \
	$(IDRIS) --build pg-idris-tests.ipkg && \
	$(IDRIS) --install pg-idris-tests.ipkg && \
	./build/exec/test $(IDRIS) $(INTERACTIVE)

check-readme:
	idris2 -p indexed -p elab-util -p parser-json -p pg-idris --check README.md

