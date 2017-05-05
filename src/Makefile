OCAMLBUILD=ocamlbuild -classic-display \
		-tags annot,debug,thread \
		-libs unix
TARGET=native

all:pipes sequential networkinter

#network_try:
#	$(OCAMLBUILD) network_try.$(TARGET)
#	$(OCAMLBUILD) network_try_client.$(TARGET)
#	-killall -q network_try.$(TARGET)
#	./network_try.$(TARGET) & ./network_try_client.$(TARGET)

pipes:
	$(OCAMLBUILD) pipes_test.$(TARGET)

sequential:
	$(OCAMLBUILD) sequential_test.$(TARGET)

networkinter:
	$(OCAMLBUILD) network_inter_test.$(TARGET)

example:
	$(OCAMLBUILD) example.$(TARGET)


clean:
	$(OCAMLBUILD) -clean

realclean: clean
	rm -f *~

cleanall: realclean
