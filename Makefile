IDRIS ?= idris
PKG   := ethambda

.PHONY: all clean

all: src
	$(IDRIS) --build ${PKG}.ipkg

clean:
	$(IDRIS) --clean ${PKG}.ipkg
