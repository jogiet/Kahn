OCAMLBUILD=ocamlbuild -classic-display \
		-tags annot,debug,thread \
		-libs unix,graphics
TARGET=native

all: example primes mandelbrot rapport 

##################################################################
##            Example
##################################################################


example:pipes sequential networkinter 

pipes:
	$(OCAMLBUILD) pipes_test.$(TARGET)

sequential:
	$(OCAMLBUILD) seq_test.$(TARGET)

networkinter:
	$(OCAMLBUILD) network_inter_test.$(TARGET)

#network_try:
#	$(OCAMLBUILD) network_try.$(TARGET)
#	$(OCAMLBUILD) network_try_client.$(TARGET)
#	-killall -q network_try.$(TARGET)
#	./network_try.$(TARGET) & ./network_try_client.$(TARGET)

network_test:
	$(OCAMLBUILD) network_test.$(TARGET)
	./network_test.$(TARGET) -s

##################################################################
##            Primes
##################################################################

primes: primes_pipes primes_network_inter 

example:
	$(OCAMLBUILD) example.$(TARGET)

primes_pipes:
	$(OCAMLBUILD) pipes_primes.$(TARGET)

primes_network_inter:
	$(OCAMLBUILD) network_inter_primes.$(TARGET)


##################################################################
##           Mandelbrot 
##################################################################

mandelbrot:  seq_mand pipes_mand network_inter_mand

seq_mand:
	$(OCAMLBUILD) seq_mand.$(TARGET)

pipes_mand:
	$(OCAMLBUILD) pipes_mand.$(TARGET)

network_inter_mand:
	$(OCAMLBUILD) network_inter_mand.$(TARGET)
	

##################################################################
##            Un essai de communication sur le réseau de l'ÉNS
##################################################################

try: try_client try_server

try_client:
	$(OCAMLBUILD) network_try_client.$(TARGET)

try_server:
	$(OCAMLBUILD) network_try.$(TARGET)

##################################################################
##           Rapport 
##################################################################

rapport:
	pdflatex rapport.tex

##################################################################
##           CLEAN
##################################################################

clean:
#	$(OCAMLBUILD) -clean
	rm -rf _build/
	rm -f *.native


realclean: clean
	rm -f *~

cleanall: realclean
