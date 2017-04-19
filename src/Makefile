OCAMLBUILD=ocamlbuild -classic-display \
		-tags annot,debug,thread \
		-libs unix
TARGET=native

pipes:
	$(OCAMLBUILD) pipes_test.$(TARGET)

sequential:
	$(OCAMLBUILD) sequential_test.$(TARGET)

example:
	$(OCAMLBUILD) example.$(TARGET)


clean:
	$(OCAMLBUILD) -clean

realclean: clean
	rm -f *~

cleanall: realclean
