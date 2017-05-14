OCAMLBUILD=ocamlbuild -classic-display \
		-tags annot,debug,thread \
		-libs unix,graphics
TARGET=native

all: primes

example:pipes sequential networkinter rapport

#network_try:
#	$(OCAMLBUILD) network_try.$(TARGET)
#	$(OCAMLBUILD) network_try_client.$(TARGET)
#	-killall -q network_try.$(TARGET)
#	./network_try.$(TARGET) & ./network_try_client.$(TARGET)

network_test:
	$(OCAMLBUILD) network_test.$(TARGET)
	./network_test.$(TARGET) -s

primes: primes_pipes primes_network_inter 

pipes:
	$(OCAMLBUILD) pipes_test.$(TARGET)

sequential:
	$(OCAMLBUILD) sequential_test.$(TARGET)

#networkinter:
#	$(OCAMLBUILD) network_inter_test.$(TARGET)

example:
	$(OCAMLBUILD) example.$(TARGET)

primes_pipes:
	$(OCAMLBUILD) primes_pipes.$(TARGET)

primes_network_inter:
	$(OCAMLBUILD) primes_network_inter.$(TARGET)

mandelbrot:  seq_mand pipes_mand

seq_mand:
	$(OCAMLBUILD) seq_mand.$(TARGET)

pipes_mand:
	$(OCAMLBUILD) pipes_mand.$(TARGET)

try: try_client try_server

try_client:
	$(OCAMLBUILD) network_try_client.$(TARGET)

try_server:
	$(OCAMLBUILD) network_try.$(TARGET)

rapport:
	pdflatex rapport.tex

clean:
#	$(OCAMLBUILD) -clean
	rm -rf _build/
	rm -f *.native


realclean: clean
	rm -f *~

cleanall: realclean
