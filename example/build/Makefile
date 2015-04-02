all:
	ln -sf ../../*.ml{,i} ./
	../../ocamp sh ./builder.sh build

clean:
	rm -rf *.o *.cm[aoix] *.cmxa

distclean: clean
	rm -rf *.byte *.native

.PHONY: all clean distclean
