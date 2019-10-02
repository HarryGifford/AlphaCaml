# Modified from https://github.com/janestreet/incremental/blob/master/Makefile

INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

all:
	dune build

clean:
	dune clean

docs:
	dune build @doc

install:
	dune install $(INSTALL_ARGS)

reinstall: uninstall reinstall

uninstall:
	dune uninstall $(INSTALL_ARGS)

.PHONY: all clean docs install reinstall uninstall
