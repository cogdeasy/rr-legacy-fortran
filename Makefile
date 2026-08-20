#=====================================================================
# MAKEFILE FOR THE ENGINE HEALTH MONITORING SYSTEM (EHMS) REL 4.2
# ROLLS-ROYCE PLC - CIVIL AERO SUPPORT - DERBY
#
#   make            compile and link every load module into BIN
#   make run        compile, link and execute the nightly job stream
#   make clean      delete object modules, load modules and work files
#
# THE SOURCE IS FIXED FORM FORTRAN 77 PUNCHED IN COLUMNS 1 TO 72.
# THE LEGACY DIALECT SWITCH MUST NOT BE REMOVED.
#=====================================================================

FC      = gfortran
FFLAGS  = -std=legacy -ffixed-form -ffixed-line-length-72 -fno-automatic -O1 -Iinc
LDFLAGS =

SRCDIR  = src
BINDIR  = bin
WRKDIR  = work

LIBOBJ  = $(SRCDIR)/ehmlib.o $(SRCDIR)/ehmio.o

PROGS   = EHMGEN EHMLOD EHMTRN EHMALR EHMRUL MROPLN FLTRPT
LOADS   = $(addprefix $(BINDIR)/,$(PROGS))

.PHONY: all run clean

all: $(LOADS)

$(BINDIR)/EHMGEN: $(SRCDIR)/ehmgen.o $(LIBOBJ)
	@mkdir -p $(BINDIR)
	$(FC) $(FFLAGS) -o $@ $^ $(LDFLAGS)

$(BINDIR)/EHMLOD: $(SRCDIR)/ehmlod.o $(LIBOBJ)
	@mkdir -p $(BINDIR)
	$(FC) $(FFLAGS) -o $@ $^ $(LDFLAGS)

$(BINDIR)/EHMTRN: $(SRCDIR)/ehmtrn.o $(LIBOBJ)
	@mkdir -p $(BINDIR)
	$(FC) $(FFLAGS) -o $@ $^ $(LDFLAGS)

$(BINDIR)/EHMALR: $(SRCDIR)/ehmalr.o $(LIBOBJ)
	@mkdir -p $(BINDIR)
	$(FC) $(FFLAGS) -o $@ $^ $(LDFLAGS)

$(BINDIR)/EHMRUL: $(SRCDIR)/ehmrul.o $(LIBOBJ)
	@mkdir -p $(BINDIR)
	$(FC) $(FFLAGS) -o $@ $^ $(LDFLAGS)

$(BINDIR)/MROPLN: $(SRCDIR)/mropln.o $(LIBOBJ)
	@mkdir -p $(BINDIR)
	$(FC) $(FFLAGS) -o $@ $^ $(LDFLAGS)

$(BINDIR)/FLTRPT: $(SRCDIR)/fltrpt.o $(LIBOBJ)
	@mkdir -p $(BINDIR)
	$(FC) $(FFLAGS) -o $@ $^ $(LDFLAGS)

$(SRCDIR)/%.o: $(SRCDIR)/%.f inc/EHMPRM.INC inc/EHMCOM.INC
	$(FC) $(FFLAGS) -c -o $@ $<

run: all
	./jcl/RUNEHM.SH

clean:
	rm -f $(SRCDIR)/*.o
	rm -rf $(BINDIR) $(WRKDIR)
