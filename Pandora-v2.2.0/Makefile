#
# Pandora makefile: <- Last updated: Mon Oct  6 14:42:49 2014 -> SGK
#
FC     = pgfortran
# FFLAGS = -g -DDEBUG
# FFLAGS = 
# FFLAGS = -O
# FFLAGS = -fast
FFLAGS = -g
#
# FC     = pgf77
# FFLAGS = -fast 
#
# FC     = ifort
# FFLAGS = 
# FFLAGS = -O
# FFLAGS = -O3
#
BIN  = bin
#
# ---------------------------------------------------------------------------
#
PAN  = pan
SYS  = sys
ZOO  = zoo
UTIL = util
SBIN = sbin
#
all: pandora.x utilities
#
# ---------------------------------------------------------------------------
#
PLIBS = $(PAN)/panlib.a $(ZOO)/zoolib.a $(SYS)/syslib.a
#
pandora.x: libs
	ar x $(PAN)/panlib.a pandora.o box.o
	$(FC) $(FFLAGS) -o $@ pandora.o box.o $(PLIBS)
#
# ---------------------------------------------------------------------------
#
libs:
	cd $(SYS); make FC=$(FC) FFLAGS="$(FFLAGS)"
	cd $(ZOO); make FC=$(FC) FFLAGS="$(FFLAGS)"
	cd $(PAN); make FC=$(FC) FFLAGS="$(FFLAGS)"
utilities: 
	cd $(UTIL); make FC=$(FC) FFLAGS="$(FFLAGS)"
#
# ---------------------------------------------------------------------------
#
install: 
	mkdir -p $(BIN)
	cp -p pandora.x             $(BIN)/.
	cp -p $(UTIL)/*.x           $(BIN)/.
	cp -p $(SBIN)/setFORx-$(FC).sou $(BIN)/setFORx.sou
	cp -p $(SBIN)/FORxxx        $(BIN)/.
	cp -p $(SBIN)/extract       $(BIN)/.
	cp -p $(SBIN)/pmerge        $(BIN)/.
	cp -p $(SBIN)/pandora       $(BIN)/.
	cp -p $(SBIN)/ready         $(BIN)/.
	cp -p $(SBIN)/census        $(BIN)/.
#
# ---------------------------------------------------------------------------
#
clean:
	-rm *.o
	-cd $(SYS); make clean
	-cd $(ZOO); make clean
	-cd $(PAN); make clean
	-cd $(UTIL); make clean
cleaner:
	-rm *.o *.x
	-cd $(SYS); make cleaner
	-cd $(ZOO); make cleaner
	-cd $(PAN); make cleaner
	-cd $(UTIL); make cleaner
cleanest:
	-rm *.o *.x
	-cd $(SYS); make cleaner
	-cd $(ZOO); make cleaner
	-cd $(PAN); make cleaner
	-cd $(UTIL); make cleaner
	-find . -name '*.prj' -exec rm '{}' \;
	-rm -rf $(BIN)
