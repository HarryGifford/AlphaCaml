.PHONY: all clean tool library demos

all: tool library

# Compiling alphaCaml.

tool:
	$(MAKE) -s -C tool $(MFLAGS)

# Compiling alphaLib.

library:
	$(MAKE) -s -C library $(MFLAGS)

# Compiling the demos. Requires the tool and library to have been installed.

demos:
	$(MAKE) -s -C demos $(MFLAGS)

# Installation.

DOC=alphaCaml.pdf

install: tool library
	$(MAKE) -s -C tool $(MFLAGS) $@
	$(MAKE) -s -C library $(MFLAGS) $@
	mkdir -p $(PREFIX)/doc/alphaCaml/
	install $(DOC) $(PREFIX)/doc/alphaCaml/$(DOC)

uninstall:
	$(MAKE) -s -C tool $(MFLAGS) $@
	$(MAKE) -s -C library $(MFLAGS) $@
	/bin/rm -rf $(PREFIX)/doc/alphaCaml

# Cleaning up.

clean:
	/bin/rm -f *~ .*~
	$(MAKE) -s -C tool clean
	$(MAKE) -s -C library clean
	if [ -d test ] ; then $(MAKE) -s -C test/good clean ; fi
	$(MAKE) -s -C demos clean
	if [ -d paper ] ; then $(MAKE) -s -C paper clean ; fi
	if [ -d doc ] ; then $(MAKE) -s -C doc clean ; fi

