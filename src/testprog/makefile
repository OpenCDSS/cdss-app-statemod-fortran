# Makefile for test programs.

# ======================================================================
# Declarations
# ======================================================================
# The compiler
FC = gfortran

# Determine the StateMod version
# - used in the executable name
# - also used for installer upload folder, etc.
# - "dev" at the end of the version (e.g., 15.00.14dev) indicates the software is under development
# STATEMOD_VERSION := $(shell cat statem.for | grep 'ver =' | grep -v 'xx' | cut -d '=' -f 2 | sed "s/'//g" | tr -d ' ' )

# Number of bytes for executable
# - used because executable has been 32-bit until 2021
# - use in filename to clearly differentiate between 32-bit and 64-bit versions until transition to 64-bit is complete
# - TODO smalers 2021-03-11 focus on Windows and deal with Linux when there is time
ifdef OS
	# Windows
	EXE_BITS = 32
	ifeq ($(MSYSTEM),MINGW32)
		EXE_BITS = 32
		FFLAGS_ARCH =
		# String to check for in 'file *.o'
		OBJ_FILE_TYPE = 80386
		OBJ_WRONG_FILE_TYPE = amd64
	endif
	ifeq ($(MSYSTEM),MINGW64)
		EXE_BITS = 64
		# The following is needed for 64-bit
		# - see: https://github.com/actions/virtual-environments/issues/2721
		FFLAGS_ARCH = -Wl,--default-image-base-low
		# String to check for in 'file *.o'
		OBJ_FILE_TYPE = amd64
		OBJ_WRONG_FILE_TYPE = 80386
	endif
else
	# Linux
	# - always compile 64-bit version
	EXE_BITS = 64
endif

# BAD_OBJ_COUNT := $(shell file *.o | grep $(OBJ_WRONG_FILE_TYPE) | wc -l)

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
# ALWAYS INCLUDE.  Actually, may want to comment this out for production versions
# when using -O so that executables are smaller and faster.
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

#FFLAGS = -g -fbounds-check -fno-align-commons
# use -static especially for windows compiling 
#FFLAGS = -g -cpp -fbounds-check -fno-align-commons -static
# Extra checks that can be added for deep check of code logic
# - Steve Malers added to track down remaining issues
FFLAGS_EXTRA_CHECKS = -fcheck=do,mem,pointer
#FFLAGS = -g -cpp -DCOMPILER=$(COMPILER_ENUM) -fno-align-commons -static -fcheck=bounds -fno-automatic -finit-local-zero $(FFLAGS_ARCH) $(FFLAGS_EXTRA_CHECKS)

# The following are what is used for general compile, and can be modified in targets.
FFLAGS0 = -g -cpp $(FFLAGS_ARCH)
#FFLAGS = -O2
# The flags for all cases (e.g. look for system .mod files, required in gfortran)
FFLAGS0 += -I/usr/include

# The libraries needed for linking
#LDFLAGS = -li_need_this_lib
# The list of executables to be built within the package.
PROGRAMS = statemod

# ======================================================================
# Main targets
# ======================================================================

# all
#
# - the default if make is called without arguments
# - compiling is fast and trying different combinations so always do a clean of *.o
#   because compiler options may change contents
# - use local value of FFLAGS since each compile needs to use different options
#all: \
#	testprog1_exe \
#	testprog1_init_local_zero_exe

all:
	@# Compile all the programs.
	make testprog1_exe
	make testprog1_init_local_zero_exe
	make testprog2_exe
	make testprog2_init_local_zero_exe
	make testprog3_exe
	make testprog3_init_local_zero_exe
	make testprog3_no_automatic_init_local_zero_exe
	@# Run all the programs.
	@echo "======================================================"
	@echo "Running testprog1"
	@echo "======================================================"
	@./testprog1.exe | tee testprog1.txt
	@echo "======================================================"
	@echo "Running testprog1-init-local-zero"
	@echo "======================================================"
	@./testprog1-init-local-zero.exe | tee testprog1-init-local-zero.txt
	@echo "======================================================"
	@echo "Running testprog2"
	@echo "======================================================"
	@./testprog2.exe
	@echo "======================================================"
	@echo "Running testprog2-init-local-zero"
	@echo "======================================================"
	@./testprog2-init-local-zero.exe | tee testprog2-init-local-zero.txt
	@echo "======================================================"
	@echo "Running testprog3"
	@echo "======================================================"
	@./testprog3.exe
	@echo "======================================================"
	@echo "Running testprog3-init-local-zero"
	@echo "======================================================"
	@./testprog3-init-local-zero.exe | tee testprog3_init_local_zero.txt
	@echo "======================================================"
	@echo "Running testprog3-no-automatic-init-local-zero"
	@echo "======================================================"
	@./testprog3-no-automatic-init-local-zero.exe | tee testprog3-no-totomatic-init-local-zero.txt

testprog1_exe: \
	clean \
	testprog1_compile

testprog1_compile: FFLAGS = $(FFLAGS0)
testprog1_compile: \
	testprog1.o \
	output.o \
	set1.o
	$(FC) $(FFLAGS) -o testprog1.exe $^ $(LDFLAGS)

testprog1_init_local_zero_exe: \
	clean \
	testprog1_init_local_zero_compile

testprog1_init_local_zero_compile: FFLAGS = $(FFLAGS0) -finit-local-zero
testprog1_init_local_zero_compile: \
	testprog1.o \
	output.o \
	set1.o
	$(FC) $(FFLAGS) -o testprog1-init-local-zero.exe $^ $(LDFLAGS)

testprog2_exe: \
	clean \
	testprog2_compile

testprog2_compile: FFLAGS = $(FFLAGS0)
testprog2_compile: \
	testprog2.o \
	output.o \
	set1.o
	$(FC) $(FFLAGS) -o testprog2.exe $^ $(LDFLAGS)

testprog2_init_local_zero_exe: \
	clean \
	testprog2_init_local_zero_compile

testprog2_init_local_zero_compile: FFLAGS = $(FFLAGS0) -finit-local-zero
testprog2_init_local_zero_compile: \
	testprog2.o \
	output.o \
	set1.o
	$(FC) $(FFLAGS) -o testprog2-init-local-zero.exe $^ $(LDFLAGS)

testprog3_exe: \
	clean \
	testprog3_compile

testprog3_compile: FFLAGS = $(FFLAGS0)
testprog3_compile: \
	testprog3.o \
	output.o \
	set_using_local.o
	$(FC) $(FFLAGS) -o testprog3.exe $^ $(LDFLAGS)

testprog3_init_local_zero_exe: \
	clean \
	testprog3_init_local_zero_compile

testprog3_init_local_zero_compile: FFLAGS = $(FFLAGS0) -finit-local-zero
testprog3_init_local_zero_compile: \
	testprog3.o \
	output.o \
	set_using_local.o
	$(FC) $(FFLAGS) -o testprog3-init-local-zero.exe $^ $(LDFLAGS)

testprog3_no_automatic_init_local_zero_exe: \
	clean \
	testprog3_no_automatic_init_local_zero_compile

testprog3_no_automatic_init_local_zero_compile: FFLAGS = $(FFLAGS0) -fno-automatic -finit-local-zero
testprog3_no_automatic_init_local_zero_compile: \
	testprog3.o \
	output.o \
	set_using_local.o
	$(FC) $(FFLAGS) -o testprog3-no-automatic-init-local-zero.exe $^ $(LDFLAGS)

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
# - declared as phony so that they always get run
.PHONY: clean veryclean

# TODO smalers 2021-03-25 remove when verified
#	@if [ $(BAD_OBJ_COUNT) -ne 0 ]; then \
#		echo "Run 'make veryclean' and 'make statemod' to force recompile with correct compiler for current shell."; \
#		echo "Exiting."; \
#		exit 0; \
#	fi;

# TODO smalers 2021-03-25 this did not work because exit in shell does not cause exit in makefile.
# - remove when working
# Check the *.o files for architecture.
# The following is output that can e used to indicate whether a .o file has been compiled with 32-bit or 64-bit compiler.
# file divres.o:
# 32-bit:
#   divres.o: Intel 80386 COFF object file, no line number info, not stripped, 12 sections, symbol offset=0x1e886, 185 symbols
# 64-bit:
#   divres.o: Intel amd64 COFF object file, no line number info, not stripped, 14 sections, symbol offset=0x2023c, 189 symbols
#checkarch:
#	@echo "Count of files compiled with wrong compiler: $(BAD_OBJ_COUNT)"
#	ifneq $(BAD_OBJ_COUNT),0
#		$(error Detected $(BAD_OBJ_COUNT) object files of wrong bit count.  Run 'make veryclean' and 'make statemod' to force recompile with correct compiler for current shell.)
#	endif

# Check the architecture to make sure are not trying to mix 32-bit and 64-bit object files.
ifneq ($(BAD_OBJ_COUNT),0)
ifdef OS
checkarch:
	@echo "Count of files compiled with wrong compiler: $(BAD_OBJ_COUNT)"
	$(error Detected $(BAD_OBJ_COUNT) object files of wrong bit count.  Run 'make veryclean' and 'make statemod' to force recompile with correct compiler for current shell.)
else
checkarch:
	@echo "Assuming 64-bit on Linux."
endif
else
ifdef OS
checkarch:
	@echo "Count of files compiled with wrong compiler: $(BAD_OBJ_COUNT)"
else
checkarch:
	@echo "Assuming 64-bit on Linux."
endif
endif

# Remove intermediate files used when compiling:
# - Do not remove *.obj because that is used with Lahey and want gfortran compile to be separate.
# - normal artifacts are *.o, etc.
# - additional artifacts may be created due to compiler problem, for example as listed below.
#     parse_gfortran.for.236r.expand
clean: printenv
	@echo "-----------------------------------------------------------------------"
	@echo "clean"
	@echo ""
	@echo "Removing compile artifacts but not final executable."
	@echo "-----------------------------------------------------------------------"
	rm -f *.o *.mod *.MOD
	rm -f *.for.*r.*

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

# Remove all intermediate files and the final executable.
# - only remove the executable for current architecture in case want to run both
veryclean: printenv clean
	@echo "-----------------------------------------------------------------------"
	@echo "veryclean"
	@echo ""
	@echo "Removing compile artifacts (make clean) and final executable."
	@echo "Only executable for current StateMod $(EXE_BITS)-bit version is removed."
	@echo "-----------------------------------------------------------------------"
ifdef OS
	# Windows
	# - only remove the executable for the OS architecture of interest
	rm -f *~ $(PROGRAMS) *.exe
else
	# Linux
	# - only remove the executable for the OS architecture of interest
	# - no file extension
	rm -f *~ $(PROGRAMS) statemod-$(STATEMOD_VERSION)-gfortran-lin-$(EXE_BITS)bit
endif
