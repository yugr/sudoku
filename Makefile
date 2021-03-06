# Copyright 2015-2016 Yury Gribov
# 
# Use of this source code is governed by MIT license that can be
# found in the LICENSE.txt file.

MODDIR = src
OBJDIR = bin

PROFILE ?= 0

GHCFLAGS = -fforce-recomp -O2 -rtsopts -funbox-strict-fields

# Too much clutter..
#GHCFLAGS = -Wall -fno-warn-name-shadowing

# Gimme some dirt...
GHCFLAGS += -keep-s-files
GHCFLAGS += -ddump-simpl -fext-core -dsuppress-all -ddump-to-file

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
	mv $(<:hs=s) $(<:hs=dump-simpl) $(<:hs=hcr) $(OBJDIR)

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
	@echo "  check      Run regtests."
	@echo ""
	@echo "Build options:"
	@echo "  PROFILE=1  Build with profile counters."

lintify:
	hlint src -i 'Use camelCase'  # Not using CamelCase and proud of it!
	checkbashisms scripts/* *.sh

profile:
	# Sanity check
	if grep -qv -- -prof $(OBJDIR)/GHC-FLAGS; then \
	  echo >&2 'Can''t collect profile on a non-profile build'; \
	  false; \
	fi
	GHCRTS='-s -h -i0.001' scripts/test.sh obj 4 2>&1 | tee profile.log
	dos2unix *.hp
	hp2ps -c GenRand.hp
	hp2ps -c Solve.hp

check:
	scripts/test.sh 2 4
	scripts/test.sh obj 2 4

# Crude workaround around GHC sillyness wrt Main modules
$(OBJDIR)/Solve.o: $(OBJDIR)/Support.o $(OBJDIR)/CNF.o $(OBJDIR)/Board.o
$(OBJDIR)/GenRand.o: $(OBJDIR)/Support.o $(OBJDIR)/CNF.o $(OBJDIR)/Board.o
$(OBJDIR)/Encode.o: $(OBJDIR)/Support.o $(OBJDIR)/CNF.o $(OBJDIR)/Board.o

depend:
	ghc -M src/Support.hs src/Board.hs src/CNF.hs -isrc -dep-suffix . -odir $(OBJDIR)
	sed -i -e '/[D]O NOT DELETE: Beginning/,/[D]O NOT DELETE: End/s!src/\([^ ]*\.hi\)!bin/\1!' Makefile

clean:
	rm -f $(OBJDIR)/* test.log *.prof *.hp *.aux *.ps *.cnf

$(OBJDIR)/GHC-FLAGS: FORCE
	if test x"$(GHCFLAGS)" != x"$$(cat $@)"; then \
		echo "$(GHCFLAGS)" > $@; \
	fi

.PHONY: clean check lintify all obj interp FORCE profile help

# DO NOT DELETE: Beginning of Haskell dependencies
bin/Support.o : src/Support.hs
bin/CNF.o : src/CNF.hs
bin/CNF.o : bin/Support.hi
bin/Board.o : src/Board.hs
bin/Board.o : bin/CNF.hi
bin/Board.o : bin/Support.hi
# DO NOT DELETE: End of Haskell dependencies
