INTERACTIVE ?= --interactive

IDRIS := idris2

PACKAGE = pg-idris.ipkg

INDEXED_VERSION = 0.0.5
INDEXED_RELATIVE_DIR = indexed-${INDEXED_VERSION}
IDRIS_LIB_DIR := $(shell ${IDRIS} --libdir)

.PHONY: all deps build clean install test

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

deps: ./depends/${INDEXED_RELATIVE_DIR}

build:
	$(IDRIS) --build $(PACKAGE)

clean:
	rm -rf ./depends
	rm -rf ./build/deps
	$(IDRIS) --clean $(PACKAGE)

install:
	$(IDRIS) --install $(PACKAGE)
	mkdir -p $(IDRIS_LIB_DIR)/${INDEXED_RELATIVE_DIR} && \
	cp -R ./depends/${INDEXED_RELATIVE_DIR} $(IDRIS_LIB_DIR)/
	
install-with-src:
	$(IDRIS) --install-with-src $(PACKAGE)
	mkdir -p $(IDRIS_LIB_DIR)/${INDEXED_RELATIVE_DIR} && \
	cp -R ./depends/${INDEXED_RELATIVE_DIR} $(IDRIS_LIB_DIR)/
	
test:
	cd tests && \
	$(IDRIS) --build pg-idris-tests.ipkg && \
	$(IDRIS) --install pg-idris-tests.ipkg && \
	./build/exec/test $(IDRIS) $(INTERACTIVE)
