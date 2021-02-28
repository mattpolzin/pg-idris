INTERACTIVE ?= --interactive

IDRIS := idris2

PACKAGE = pg-idris.ipkg

all: build

.PHONY: build

build:
	$(IDRIS) --build $(PACKAGE)

.PHONY: clean

clean:
	$(IDRIS) --clean $(PACKAGE)

.PHONY: install

install:
	$(IDRIS) --install $(PACKAGE)
	
.PHONY: test

test:
	cd tests && \
	$(IDRIS) --build pg-idris-tests.ipkg && \
	$(IDRIS) --install pg-idris-tests.ipkg && \
	./build/exec/test $(IDRIS) $(INTERACTIVE)
