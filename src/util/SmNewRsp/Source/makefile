# Makefile for compiling SmNewRsp using gfortran
#
# Copied the StateMod 'makefile' and modified to get compile:
# - could be cleaned up more
# - just do a basic compile of 64-bit version
#
# ======================================================================
# Use 'make' or 'make help' to see usage.
# ======================================================================

# ======================================================================
# Declarations
# ======================================================================
# The compiler
FC = gfortran

# Below are the flags for debugging or for maximum performance, commented to explain use.

# -cpp
#
# Enable preprocessing. The preprocessor is automatically invoked if the file extension is
# .fpp, .FPP, .F, .FOR, .FTN, .F90, .F95, .F03 or .F08. Use this option to manually enable
# preprocessing of any kind of Fortran file.
#
# To disable preprocessing of files with any of the above listed extensions, use the negative form: -nocpp.
#
# The preprocessor is run in traditional mode. Any restrictions of the file-format,
# especially the limits on line length, apply for preprocessed output as well, so it might be
# advisable to use the -ffree-line-length-none or -ffixed-line-length-none options.
#-------------------------------------------------------------------------------------------------------
# See compiler options here:  https://gcc.gnu.org/onlinedocs/gfortran/#toc-GNU-Fortran-Command-Options
#-------------------------------------------------------------------------------------------------------
#
# -fcheck=bounds
# -fbounds-check (this is deprecated in favor of -fcheck=bounds)
#
# Enable generation of run-time checks for array subscripts and against the declared
# minimum and maximum values. It also checks array indices for assumed and deferred
# shape arrays against the actual allocated bounds and ensures that all string lengths
# are equal for character array constructors without an explicit typespec.
#
# Some checks require that -fcheck=bounds is set for the compilation of the main program.
#
# Note: In the future this may also include other forms of checking, e.g., checking substring references.
#------------
#
# -finit-local-zero
#
# ALWAYS INCLUDE.
# The -finit-local-zero option instructs the compiler to initialize local INTEGER, REAL,
# and COMPLEX variables to zero, LOGICAL variables to false, and CHARACTER variables to a string of null bytes
#------------
#
# -fno-align-commons
#
# ALWAYS INCLUDE.
# By default, gfortran enforces proper alignment of all variables in a COMMON block
# by padding them as needed. On certain platforms this is mandatory, on others it
# increases performance. If a COMMON block is not declared with consistent data types
# everywhere, this padding can cause trouble, and -fno-align-commons can be used to
# disable automatic alignment. The same form of this option should be used for all
# files that share a COMMON block. To avoid potential alignment issues in COMMON blocks,
# it is recommended to order objects from largest to smallest.
#------------
#
# -fno-automatic
#
# ALWAYS INCLUDE.
# Treat each program unit (except those marked as RECURSIVE) as if the SAVE statement were
# specified for every local variable and array referenced in it. Does not affect common blocks.
# (Some Fortran compilers provide this option under the name -static or -save.)
# The default, which is -fautomatic, uses the stack for local variables smaller than the value
# given by -fmax-stack-var-size. Use the option -frecursive to use no static memory.
#------------
#
# -g 
#
# ALWAYS INCLUDE, but for production version use -Og, which is optimized.
#
# This is from the gcc compiler (see: https://gcc.gnu.org/onlinedocs/gcc-10.2.0/gcc/Debugging-Options.html#Debugging-Options).
#
# Produce debugging information in the operating system’s native format (stabs, COFF, XCOFF, or DWARF).
# GDB can work with this debugging information.
#
# On most systems that use stabs format, -g enables use of extra debugging information that only GDB can use;
# this extra information makes debugging work better in GDB but probably makes other debuggers crash or refuse to read the program.
# If you want to control for certain whether to generate the extra information,
# use -gstabs+, -gstabs, -gxcoff+, -gxcoff, or -gvms.
#
#------------
#
# -static
#
# ALWAYS INCLUDE.
#------------

# Extra checks that can be added for deep check of code logic
# - do not use -all because 'array-temps' seems unnecessary
# - do include all the other checks
FFLAGS_EXTRA_CHECKS = -fcheck=bits,bounds,do,mem,pointer,recursion

# The following compiler options result in all runtime checks but is slower to run.
# - this is used by statemod_check target and 'check' variant
# - DO NOT use in normal development because running automated tests is slow
# - use -ffpe to try to help track down specific line of error
FFLAGS_CHECK = -g -O1 -Wall -cpp -DCOMPILER=$(COMPILER_ENUM) -ffpe-trap=zero,overflow,underflow -fbacktrace -fno-align-commons -static $(FFLAGS_ARCH) $(FFLAGS_EXTRA_CHECKS) -I/usr/include

# The following compiler options result in fewer runtime checks but is faster to run:
# -fcheck=bounds is retained because it catches major issues in array boundary issues
# - this is used by statemod_o3 target and 'o3' variant
# - DO use in normal development because running automated tests is fast
# - use -Wall because it checks for uninitialized variables and other common issues
FFLAGS_O3 = -Og -O3 -Wall -cpp -DCOMPILER=$(COMPILER_ENUM) -fbacktrace -fno-align-commons -static -fcheck=bounds $(FFLAGS_ARCH) -I/usr/include

# The list of executables to be built within the package.
PROGRAMS = SmNewRsp

# Set the source modules based on the operating system and compiler:
# - this allows this single makefile to work in multiple development environments (as long as GNU make is used)
# - currently the focus is gfortran on Windows but could get from the environment and change modules

# Compiler-specific routines to handle command line parsing and date/time processing.
# - set COMPILER_CHAR to use with C preprocessor
#   which can be used as an alternate way to support compiler-specific libraries
#   (see test.for) for example code
# - set COMPILER_ENUM to use with C preprocessor, because COMPILER_CHAR was having issues
ifeq ($(FC),gfortran)
	# gfortran code version
	parse_o_file = parse_gfortran.o
	dattim_o_file = dattim_gfortran.o
	COMPILER_ENUM = 1
else
	# Legacy Lahey code version
	parse_o_file = parse.o
	dattim_o_file = dattim.o
	COMPILER_ENUM= 2
endif

# Operating-system-specific code:
# - Although there are other options, initially use code from Jim Brannon
# - OS environment variable is specific to Windows (assuming the compile occurs
#   in a Windows command shell) so if set assume Windows

ifdef OS
	# Assume Windows legacy naming convention
	getpath_o_file = getpath.o
	putpath_o_file = putpath.o 
else
	# Assume Linux
	getpath_o_file = getpath_linux.o
	putpath_o_file = putpath_linux.o 
endif

# ======================================================================
# Main targets
# ======================================================================

# all
#
# - the default if make is called without arguments
# - use default of 'help' to force intentional actions such as 'make statemod'
# - uncomment line with 'printenv' if troubleshooting the environment
#all: printenv $(PROGRAMS)
all: help

# help
#
# Print the targets that are available.
help:
	@echo "-------------------------------------------------------------------------------------------"
	@echo "SmNewRsp makefile targets:"
	@echo ""
	@echo "all               Default target that prints this help message."
	@echo "clean             Remove dynamically created files (but not final executable)."
	@echo "help              Print this message."
	@echo "printenv          Helper to print which operating system is used, controls configuration."
	@echo "SmNewRsp          Compile the StateMod executable, recompiling any .o if .for modified."
	@echo "                  Same as 'statemod_o3' to compile with all checks for development testing."
	@echo "                  USE FOR NORMAL DEVELOPMENT."
	@echo "veryclean         Make the 'clean' target, and also remove the final executable."
	@echo "file.o            Compile the source file.for file into object file file.o,"
	@echo "                  useful to check syntax for a single file."
	@echo "-------------------------------------------------------------------------------------------"
	@echo "Important makefile variables that are used:"
	@echo ""
	@echo "FC (compiler) = $(FC)"
	@echo "-------------------------------------------------------------------------------------------"
	@echo "Important environment variables that are used:"
	@echo ""
	@echo "OS (to determine if Windows) = $(OS)"
	@echo "MSYSTEM (to determine EXE_BITS on Windows) = $(MSYSTEM)"
	@echo "-------------------------------------------------------------------------------------------"

# Main target.
SmNewRsp: \
	SmNewRsp.o\
	getfn.o\
	getfn2.o\
	$(parse_o_file)\
	skipn.o
ifdef OS
	@# Windows...
	$(FC) $(FFLAGS) -o SmNewRsp.exe $^ $(LDFLAGS)
	@echo "-----------------------------------------------------------------------"
	@echo "Executable is SmNewRsp.exe"
	@echo "-----------------------------------------------------------------------"
else
	@# Linux...
	$(FC) $(FFLAGS) -o SmNewRsp $^ $(LDFLAGS)
	@echo "-----------------------------------------------------------------------"
	@echo "Executable is SmNewRsp"
	@echo "-----------------------------------------------------------------------"
endif

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

# Utility targets.
.PHONY: clean veryclean vercleanrb

# clean
#
# Remove intermediate files used when compiling:
# - Do not remove *.obj because that is used with Lahey and want gfortran compile to be separate.
# - normal artifacts are *.o, etc.
# - additional artifacts may be created due to compiler problem, for example as listed below
#   and are removed with a wildcard pattern
#     parse_gfortran.for.236r.expand
#     parse_gfortran.for.237r.vregs
#     parse_gfortran.for.238r.into_cfglayout
#     parse_gfortran.for.239r.jump
#     parse_gfortran.for.251r.reginfo
#     parse_gfortran.for.274r.outof_cfglayout
#     parse_gfortran.for.275r.split1
#     parse_gfortran.for.277r.dfinit
#     parse_gfortran.for.278r.mode_sw
#     parse_gfortran.for.279r.asmcons
#     parse_gfortran.for.284r.ira
#     parse_gfortran.for.285r.reload
#     parse_gfortran.for.292r.pro_and_epilogue
#     parse_gfortran.for.295r.jump2
#     parse_gfortran.for.306r.split4
#     parse_gfortran.for.307r.stack
#     parse_gfortran.for.308r.alignments
#     parse_gfortran.for.310r.mach
#     parse_gfortran.for.311r.barriers
#     parse_gfortran.for.316r.shorten
#     parse_gfortran.for.317r.nothrow
#     parse_gfortran.for.318r.dwarf2
#     parse_gfortran.for.319r.final
#     parse_gfortran.for.320r.dfinish
clean: printenv
	@echo "-----------------------------------------------------------------------"
	@echo "clean"
	@echo ""
	@echo "Removing compile artifacts but not final executable."
	@echo "-----------------------------------------------------------------------"
	rm -f *.o *.mod *.MOD
	rm -f *.for.*r.*

# printenv
#
# Print important environment information to help understand setup.
printenv:
	@echo "-----------------------------------------------------------------------"
	@echo "Environment information:"
ifdef OS
	@echo "Windows detected from OS environment variable:  yes"
else
	@echo "Windows detected from OS environment variable:  no"
	@echo "...assuming Linux"
endif
ifdef MSYSTEM
	@echo "MINGW (gfortran) detected from MSYSTEM environment variable:  yes"
else
	@echo "MINGW (gfortran) detected from MSYSTEM environment variable:  no"
endif
	@echo "-----------------------------------------------------------------------"

# veryclean
#
# Remove all intermediate files and the final executable.
# - only remove the executable for current architecture in case want to run both
veryclean: printenv clean
	@echo "-----------------------------------------------------------------------"
	@echo "veryclean"
	@echo ""
	@echo "Removing compile artifacts (make clean) and final executable."
	@echo "-----------------------------------------------------------------------"
ifdef OS
	# Windows
	# - only remove the executable for the OS architecture of interest
	rm -f *~ $(PROGRAMS) SmNewRsp.exe
else
	# Linux
	# - only remove the executable for the OS architecture of interest
	# - no file extension
	rm -f *~ $(PROGRAMS) SmNewRsp*
endif
