#
# Pandora utility makefile <- Last updated: Wed Mar 19 22:19:44 2014 -> SGK
#
cents.x: cents.o
	$(FC) -o $@ $< $(SYS)/syslib.a
dimes.x: dimes.o
	$(FC) -o $@ $<
he1diff.x: he1diff.o
	$(FC) -o $@ $<
modup.x: modup.o 
	$(FC) -o $@ $< $(SYS)/syslib.a $(ZOO)/zoolib.a
ready.x: ready.o
	$(FC) -o $@ $< $(SYS)/syslib.a
wrap.x: wrap.o
	$(FC) -o $@ $<
lookat.x: lookat.o
	$(FC) -o $@ $<
census.x: census.o box.o
	$(FC) -o $@ $< $(PAN)/panlib.a $(ZOO)/zoolib.a $(SYS)/syslib.a
#
box.o: 
	ar x $(PAN)/panlib.a box.o
modup.o: modup.f modup.inc
#
clean:
	-rm *.o
cleaner:
	-rm *.o *.x

