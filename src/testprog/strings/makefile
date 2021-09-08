# Makefile for test programs.

# ======================================================================
# Declarations
# ======================================================================
# The compiler
FC = gfortran

FFLAGS += -I/usr/include -Wall

PROGRAMS = stringcompare

# ======================================================================
# Main targets
# ======================================================================

stringcompare: \
	stringcompare.o
	$(FC) $(FFLAGS) -o stringcompare.exe $^ $(LDFLAGS)


# ======================================================================
# The general rules.  These generally should not require modification.
# ======================================================================

# General rule for building prog from prog.o; $^ (GNU extension) is
# used in order to list additional object files on which the
# executable depends.
%: %.o
	$(FC) $(FFLAGS) -o $@ $^ $(LDFLAGS)

# General rules for compiling source files into object files.
#
# % = wildcard to match file basename
# $< = source file name

# Compile files with extension .f90 into .o object files.
%.o: %.f90
	$(FC) $(FFLAGS) -c $<

# Compile files with extension .F90 into .o object files.
%.o: %.F90
	$(FC) $(FFLAGS) -c $<

# Compile files with extension .for into .o object files.
%.o: %.for
	$(FC) $(FFLAGS) -c $<
