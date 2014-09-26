MODDIR = src
BINDIR = bin

# Too much clutter ...
#GHCFLAGS = -Wall -fno-warn-name-shadowing

# Gimme some dirt...
#GHCFLAGS += -keep-s-files

ifneq (,$(OPT))
  GHCFLAGS += -O
endif

all: interp obj

interp: $(BINDIR)/GenRand.sh $(BINDIR)/Solve.sh $(BINDIR)/Encode.sh

$(BINDIR)/%.sh: scripts/Runner
	cp scripts/Runner $@

obj: $(BINDIR)/GenRand.exe $(BINDIR)/Solve.exe $(BINDIR)/Encode.exe

$(BINDIR)/%.exe: $(MODDIR)/%.hs
	# Clumsyness due to https://ghc.haskell.org/trac/ghc/ticket/7311
	mkdir -p $@.odir
	ghc --make $(GHCFLAGS) -i$(MODDIR) -outputdir $@.odir $^ -o $@.odir/Main.exe
	cp $@.odir/Main.exe $@

lintify:
	hlint src -i 'Use camelCase'  # Not using CamelCase and proud of it!
	checkbashisms scripts/* *.sh

test t: interp
	scripts/test.sh 4

test-obj t-obj: obj
	scripts/test.sh 4 obj

clean:
	rm -rf bin/* test.log
	rm -f board_out*.cnf  # Ugly hack (see Solve.hs for details)

.PHONY: clean test t lintify all obj interp

