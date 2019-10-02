.PHONY: all clean poplmark mixins interactive

all: poplmark mixins interactive

poplmark:
	$(MAKE) -s -C poplmark $(MFLAGS)

mixins:
	$(MAKE) -s -C mixins $(MFLAGS)

interactive:
	if [ -d interactive ] ; then $(MAKE) -s -C interactive $(MFLAGS) ; fi

clean:
	/bin/rm -f *~ .*~
	$(MAKE) -s -C poplmark $(MFLAGS) clean
	$(MAKE) -s -C mixins $(MFLAGS) clean
	if [ -d interactive ] ; then $(MAKE) -s -C interactive $(MFLAGS) clean ; fi
