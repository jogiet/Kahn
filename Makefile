OCAMLBUILD=ocamlbuild \
		-tags annot,debug,thread \
		-libs unix,graphics
TARGET=native

all: example primes mandelbrot rapport 

##################################################################
##            Example
##################################################################


example:pipes sequential networkinter

pipes:
	$(OCAMLBUILD) src/pipes_test.$(TARGET)

sequential:
	$(OCAMLBUILD) src/seq_test.$(TARGET)

networkinter:
	$(OCAMLBUILD) src/network_inter_test.$(TARGET)

#network_try:
#	$(OCAMLBUILD) network_try.$(TARGET)
#	$(OCAMLBUILD) network_try_client.$(TARGET)
#	-killall -q network_try.$(TARGET)
#	./network_try.$(TARGET) & ./network_try_client.$(TARGET)



##################################################################
##            Primes
##################################################################

primes: primes_pipes primes_network_inter network_primes

primes_pipes:
	$(OCAMLBUILD) src/pipes_primes.$(TARGET)

primes_network_inter:
	$(OCAMLBUILD) src/network_inter_primes.$(TARGET)

network_primes:
	$(OCAMLBUILD) src/network_test.$(TARGET)
#	./network_test.$(TARGET) -s

##################################################################
##           Mandelbrot 
##################################################################

mandelbrot:  seq_mand pipes_mand network_inter_mand network_mand

seq_mand:
	$(OCAMLBUILD) src/seq_mand.$(TARGET)

pipes_mand:
	$(OCAMLBUILD) src/pipes_mand.$(TARGET)

network_inter_mand:
	$(OCAMLBUILD) src/network_inter_mand.$(TARGET)

network_mand:
	$(OCAMLBUILD) src/network_mand.$(TARGET)

##################################################################
##            Un essai de communication sur le réseau de l'ÉNS
##################################################################

try: try_client try_server

try_client:
	$(OCAMLBUILD) src/network_try_client.$(TARGET)

try_server:
	$(OCAMLBUILD) src/network_try.$(TARGET)

##################################################################
##           Rapport 
##################################################################

rapport:
	pdflatex report/rapport.tex
	mv report/rapport.pdf ./

##################################################################
##           CLEAN
##################################################################

clean:
#	$(OCAMLBUILD) -clean
	rm -rf _build/
	rm -f *.native
	rm -f *.aux *.log *.toc


realclean: clean
	rm -f *~
	rm -f *.pdf

cleanall: realclean
