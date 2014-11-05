MODDIR = src
OBJDIR = bin

PROFILE ?= 0

GHCFLAGS = -fforce-recomp -O2 -rtsopts -funbox-strict-fields

# Too much clutter..
#GHCFLAGS = -Wall -fno-warn-name-shadowing

# Gimme some dirt...
GHCFLAGS += -keep-s-files
GHCFLAGS += -ddump-simpl -dsuppress-all -ddump-to-file

LDFLAGS = -package random -package directory -package process

OBJS = $(OBJDIR)/Support.o $(OBJDIR)/Board.o $(OBJDIR)/CNF.o

ifneq (0,$(PROFILE))
  GHCFLAGS += -prof -auto-all -caf-all
endif

all: interp obj

interp: $(OBJDIR)/GenRand.sh $(OBJDIR)/Solve.sh

$(OBJDIR)/%.sh: scripts/Runner
	cp scripts/Runner $@

obj: $(OBJDIR)/GenRand.exe $(OBJDIR)/Solve.exe

$(OBJDIR)/%.exe: $(OBJDIR)/%.o $(OBJS) $(OBJDIR)/GHC-FLAGS
	ghc $(GHCFLAGS) $(LDFLAGS) $< $(OBJS) -o $@

$(OBJDIR)/%.o: $(MODDIR)/%.hs $(OBJDIR)/GHC-FLAGS
	ghc -c $(GHCFLAGS) -i$(OBJDIR) $< -o $@ -ohi $(@:o=hi)
	mv $(<:hs=s) $(OBJDIR)
	mv $(<:hs=dump-simpl) $(OBJDIR)

$(OBJDIR)/%.hi: $(OBJDIR)/%.o $(OBJDIR)/GHC-FLAGS
	# Was already moved during compilation of .o

help:
	@echo "Common targets:"
	@echo "  all        Build all executables and scripts"
	@echo "  clean      Clean all build files and temps."
	@echo "  depend     Rebuild dependencies (modifies Makefile!)."
	@echo "  help       Print help on build options."
	@echo ""
	@echo "Less common:"
	@echo "  profile    Run profile tests (requires PROFILE to be set)."
	@echo "  interp     Create scripts."
	@echo "  obj        Build executables."
	@echo "  test       "
	@echo "  t          Regtest scripts."
	@echo "  test-obj   "
	@echo "  to         Regtest executables."
	@echo ""
	@echo "Build options:"
	@echo "  PROFILE=1  Build with profile counters."

lintify:
	hlint src -i 'Use camelCase'  # Not using CamelCase and proud of it!
	checkbashisms scripts/* *.sh

ifneq (0,$(PROFILE))
profile:
	GHCRTS='-s -h -i0.001' scripts/test.sh obj 4
	dos2unix *.hp
	hp2ps -c GenRand.hp
	hp2ps -c Solve.hp
endif

test t:
	scripts/test.sh 2 4

test-obj to:
	scripts/test.sh obj 2 4

depend:
	ghc -M src/Board.hs -isrc -dep-suffix . -odir $(OBJDIR)
	sed -ie '/DO NOT DELETE: Beginning of/,/DO NOT DELETE: End of/s!src/\([^ ]*\.hi\)!bin/\1!' Makefile

clean:
	rm -f $(OBJDIR)/* test.log *.prof *.hp *.aux *.ps *.cnf

$(OBJDIR)/GHC-FLAGS: FORCE
	if test x"$(GHCFLAGS)" != x"$$(cat $@)"; then \
		echo "$(GHCFLAGS)" > $@; \
	fi

.PHONY: clean test t test-obj to lintify all obj interp FORCE profile help

# DO NOT DELETE: Beginning of Haskell dependencies
bin/Support.o : src/Support.hs
bin/CNF.o : src/CNF.hs
bin/CNF.o : bin/Support.hi
bin/Board.o : src/Board.hs
bin/Board.o : bin/CNF.hi
bin/Board.o : bin/Support.hi
# DO NOT DELETE: End of Haskell dependencies
