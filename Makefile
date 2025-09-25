INTERACTIVE ?= --interactive

IDRIS := idris2

PACKAGE = pg-idris.ipkg

INDEXED_VERSION = 0.0.10
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
	IDRIS2_PREFIX=../../../depends make install && \
	mv ../../../depends/idris2*/* ../../../depends/

./depends/idris2-elab-util:
	mkdir -p ./build/deps
	mkdir -p ./depends
	cd ./build/deps && \
	git clone https://github.com/stefan-hoeck/idris2-elab-util.git && \
	cd idris2-elab-util && \
	$(IDRIS) --build elab-util.ipkg && \
	IDRIS2_PACKAGE_PATH=$(IDRIS_LIB_DIR) IDRIS2_PREFIX=../../../depends $(IDRIS) --install elab-util.ipkg && \
	mv ../../../depends/idris2*/* ../../../depends/

./depends/idris2-algebra:
	mkdir -p ./build/deps
	mkdir -p ./depends
	cd ./build/deps && \
	git clone https://github.com/stefan-hoeck/idris2-algebra.git && \
	cd idris2-algebra && \
	$(IDRIS) --build algebra.ipkg && \
	IDRIS2_PACKAGE_PATH=$(IDRIS_LIB_DIR) IDRIS2_PREFIX=../../../depends $(IDRIS) --install algebra.ipkg && \
	mv ../../../depends/idris2*/* ../../../depends/

./depends/idris2-ref1:
	mkdir -p ./build/deps
	mkdir -p ./depends
	cd ./build/deps && \
	git clone https://github.com/stefan-hoeck/idris2-ref1.git && \
	cd idris2-ref1 && \
	$(IDRIS) --build ref1.ipkg && \
	IDRIS2_PACKAGE_PATH=$(IDRIS_LIB_DIR) IDRIS2_PREFIX=../../../depends $(IDRIS) --install ref1.ipkg && \
	mv ../../../depends/idris2*/* ../../../depends/

./depends/idris2-array:
	mkdir -p ./build/deps
	mkdir -p ./depends
	cd ./build/deps && \
	git clone https://github.com/stefan-hoeck/idris2-array.git && \
	cd idris2-array && \
	IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" $(IDRIS) --build array.ipkg && \
  IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" IDRIS2_PREFIX=../../../depends $(IDRIS) --install array.ipkg && \
	mv ../../../depends/idris2*/* ../../../depends/

./depends/idris2-bytestring:
	mkdir -p ./build/deps
	mkdir -p ./depends
	cd ./build/deps && \
	git clone https://github.com/stefan-hoeck/idris2-bytestring.git && \
	cd idris2-bytestring && \
	IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" $(IDRIS) --build bytestring.ipkg && \
  IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" IDRIS2_PREFIX=../../../depends $(IDRIS) --install bytestring.ipkg && \
	mv ../../../depends/idris2*/* ../../../depends/

./depends/idris2-ilex/core:
	mkdir -p ./build/deps
	mkdir -p ./depends
	cd ./build/deps && \
	git clone https://github.com/stefan-hoeck/idris2-ilex.git && \
	cd idris2-ilex/core && \
	IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../../depends" $(IDRIS) --build ilex-core.ipkg && \
  IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../../depends" IDRIS2_PREFIX=../../../../depends $(IDRIS) --install ilex-core.ipkg && \
	mv ../../../../depends/idris2*/* ../../../../depends/

./depends/idris2-parser:
	mkdir -p ./build/deps
	mkdir -p ./depends
	cd ./build/deps && \
	git clone https://github.com/stefan-hoeck/idris2-parser.git && \
	cd idris2-parser && \
	IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" $(IDRIS) --build parser.ipkg && \
	IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" IDRIS2_PREFIX=../../../depends $(IDRIS) --install parser.ipkg && \
	mv ../../../depends/idris2*/* ../../../depends/

./depends/idris2-parser/json:
	mkdir -p ./build/deps
	mkdir -p ./depends
	cd ./build/deps && \
	cd idris2-parser/json && \
	IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../../depends" $(IDRIS) --build parser-json.ipkg && \
  IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../../depends" IDRIS2_PREFIX=../../../../depends $(IDRIS) --install parser-json.ipkg && \
	mv ../../../../depends/idris2*/* ../../../../depends/

deps: ./depends/${INDEXED_RELATIVE_DIR} ./depends/idris2-elab-util ./depends/idris2-algebra ./depends/idris2-ref1 ./depends/idris2-array ./depends/idris2-bytestring ./depends/idris2-ilex/core ./depends/idris2-parser ./depends/idris2-parser/json

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
	cp -R ./depends/elab-util*/ $(IDRIS_LIB_DIR)/ && \
	cp -R ./depends/parser*/ $(IDRIS_LIB_DIR)/ && \
	cp -R ./depends/parser-json*/ $(IDRIS_LIB_DIR)/
	
install-with-src:
	$(IDRIS) --install-with-src $(PACKAGE)
	mkdir -p $(IDRIS_LIB_DIR)/${INDEXED_RELATIVE_DIR} && \
	cp -R ./depends/${INDEXED_RELATIVE_DIR} $(IDRIS_LIB_DIR)/ && \
	cp -R ./depends/elab-util*/ $(IDRIS_LIB_DIR)/ && \
	cp -R ./depends/parser*/ $(IDRIS_LIB_DIR)/ && \
	cp -R ./depends/parser-json*/ $(IDRIS_LIB_DIR)/

# When testing locally, spin up a fresh Postgres instance and set
# TEST_DATABASE_URL to a valid `postgres://` address before running
# the test target.
test:
	cd tests && \
	$(IDRIS) --build pg-idris-tests.ipkg && \
	$(IDRIS) --install pg-idris-tests.ipkg && \
	./build/exec/test $(IDRIS) $(INTERACTIVE)

check-readme:
	idris2 -p indexed -p elab-util -p parser -p parser-json -p pg-idris --check README.md

