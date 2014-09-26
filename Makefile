# Too much clutter ...
#GHCFLAGS = -Wall

MODDIR = src
BINDIR = bin

all: interp obj

interp: $(BINDIR)/GenRand $(BINDIR)/Solve $(BINDIR)/Encode

$(BINDIR)/%: scripts/Runner
	cp scripts/Runner $@

obj: $(BINDIR)/GenRand.exe $(BINDIR)/Solve.exe $(BINDIR)/Encode.exe

$(BINDIR)/%.exe: $(MODDIR)/%.hs
	ghc --make $(GHCFLAGS) -i$(MODDIR) -odir $(BINDIR) -hidir $(BINDIR) $^ -o $@

lintify:
	hlint src -i 'Use camelCase'  # Not using CamelCase and proud of it!
	checkbashisms scripts/* *.sh

test t: interp
	scripts/test.sh

clean:
	rm -f bin/* ghc.log
	rm -f board_out*.cnf  # Ugly hack (see Solve.hs for details)

.PHONY: clean test t lintify all build

