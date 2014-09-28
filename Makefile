MODDIR = src
OBJDIR = bin

GHCFLAGS = -fforce-recomp -O2 -rtsopts

# Too much clutter..
#GHCFLAGS = -Wall -fno-warn-name-shadowing

# Gimme some dirt...
GHCFLAGS += -keep-s-files

LDFLAGS = -package random -package directory -package process

OBJS = $(OBJDIR)/Support.o $(OBJDIR)/Board.o $(OBJDIR)/CNF.o

ifneq (,$(PROFILE))
  #GHCFLAGS += -prof -fprof-auto
  GHCFLAGS += -prof -auto-all
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

$(OBJDIR)/%.hi: $(OBJDIR)/%.o $(OBJDIR)/GHC-FLAGS
	# Was already moved during compilation of .o

lintify:
	hlint src -i 'Use camelCase'  # Not using CamelCase and proud of it!
	checkbashisms scripts/* *.sh

profile:
	GHCRTS='-s -p -h' scripts/test.sh 4 obj
	dos2unix *.prof *.hp
	hp2ps -c *.hp

test t:
	scripts/test.sh 4

test-obj t-obj:
	scripts/test.sh 4 obj

depend:
	ghc -M src/Board.hs -isrc -dep-suffix . -odir $(OBJDIR)
	sed -ie '/DO NOT DELETE: Beginning of/,/DO NOT DELETE: End of/s!src/\([^ ]*\.hi\)!bin/\1!' Makefile

clean:
	rm -f $(OBJDIR)/* test.log *.prof *.hp *.aux *.ps

$(OBJDIR)/GHC-FLAGS: FORCE
	if test x"$(GHCFLAGS)" != x"$$(cat $@)"; then \
		echo "$(GHCFLAGS)" > $@; \
	fi

.PHONY: clean test t lintify all obj interp FORCE profile

# DO NOT DELETE: Beginning of Haskell dependencies
bin/Support.o : src/Support.hs
bin/CNF.o : src/CNF.hs
bin/CNF.o : bin/Support.hi
bin/Board.o : src/Board.hs
bin/Board.o : bin/CNF.hi
bin/Board.o : bin/Support.hi
# DO NOT DELETE: End of Haskell dependencies
