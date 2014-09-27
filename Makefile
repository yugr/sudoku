MODDIR = src
OBJDIR = bin

GHCFLAGS = -O -rtsopts

# Too much clutter..
#GHCFLAGS = -Wall -fno-warn-name-shadowing

# Gimme some dirt...
GHCFLAGS += -keep-s-files

LDFLAGS = -package random -package directory -package process

OBJS = $(OBJDIR)/Support.o $(OBJDIR)/Board.o $(OBJDIR)/CNF.o

ifneq (,$(PROF))
  GHCFLAGS += -prof -fprof-auto
endif

all: interp obj

interp: $(OBJDIR)/GenRand.sh $(OBJDIR)/Solve.sh $(OBJDIR)/Encode.sh

$(OBJDIR)/%.sh: scripts/Runner
	cp scripts/Runner $@

obj: $(OBJDIR)/GenRand.exe $(OBJDIR)/Solve.exe $(OBJDIR)/Encode.exe

$(OBJDIR)/%.exe: $(OBJDIR)/%.o $(OBJS)
	ghc $(GHCFLAGS) $(LDFLAGS) $^ -o $@

$(OBJDIR)/%.o: $(MODDIR)/%.hs
	ghc -c $(GHCFLAGS) -i$(OBJDIR) $< -o $@ -ohi $(@:o=hi)
	mv $(<:hs=s) $(OBJDIR)

$(OBJDIR)/%.hi: $(OBJDIR)/%.o
	# Was already moved during compilation of .o

lintify:
	hlint src -i 'Use camelCase'  # Not using CamelCase and proud of it!
	checkbashisms scripts/* *.sh

prof: obj
	GHCRTS='-p -h' scripts/test.sh 5 obj
	dos2unix *.prof *.hp

test t: interp
	scripts/test.sh 4

test-obj t-obj: obj
	scripts/test.sh 4 obj

depend:
	ghc -M src/Board.hs -isrc -dep-suffix . -odir $(OBJDIR)
	sed -ie '/DO NOT DELETE: Beginning of/,/DO NOT DELETE: End of/s!src/\([^ ]*\.hi\)!bin/\1!' Makefile

clean:
	rm -f bin/* test.log *.prof *.hp *.aux

.PHONY: clean test t lintify all obj interp

# DO NOT DELETE: Beginning of Haskell dependencies
bin/Support.o : src/Support.hs
bin/CNF.o : src/CNF.hs
bin/CNF.o : bin/Support.hi
bin/Board.o : src/Board.hs
bin/Board.o : bin/CNF.hi
bin/Board.o : bin/Support.hi
# DO NOT DELETE: End of Haskell dependencies
