# Makefile for compiling StateMod using gfortran
#
# ======================================================================
# Use 'make' or 'make help' to see usage.
# ======================================================================
#
# - initial version taken from Jim Brannon's work
# - significant updates to add documentation, help target, make portable
# - should be able to update to work with Windows, Linux, and Lahey compiler;
#   however, Lahey currently still uses the AM.bat and AUTOMAKE.RSP files
# - all filenames were converted to lowercase during initial repository load
#   to ensure easy comparison (because different case was used in different versions)
# - files added since then use mixed case and file system names must be used in this
#   makefile to work on case-specific systems like Linux
# - operating system is determined by checking for OS environment variable,
#   which indicates Windows, otherwise Linux variant is assumed
#
# Modification history from Ray Bennett:
# - TODO smalers 2021-03-11 remove these after testing since repo history includes changes.
# rrb; Try to remove *.exe in case compile fails
# rm -f statemod-gfortran-32bit.exe
# rrb 2018-08-05; Add new programs FlowRes and DivmultR
# rrb 2018-10-25; Add new jmstore in 2 spots
# rrb 2018-11-05; Add new jmflow in 2 spots
# rrb 2018-11-23; add new jmout in 2 spots
# rrb 2018-12-16; Add new chekav3 in 1 spot

# ======================================================================
# Declarations
# ======================================================================
# The compiler
FC = gfortran

# Determine the StateMod version
# - used in the executable name
# - also used for installer upload folder, etc.
# - "dev" at the end of the version (e.g., 15.00.14dev) indicates the software is under development
STATEMOD_VERSION := $(shell cat statem.for | grep 'ver =' | grep -v 'xx' | cut -d '=' -f 2 | sed "s/'//g" | tr -d ' ' )

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

BAD_OBJ_COUNT := $(shell file *.o | grep $(OBJ_WRONG_FILE_TYPE) | wc -l)

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
# Use -O1 -Wall to check for common warnings.
# This generally works but has some issues at runtime that cause different results and are still being resolved
#FFLAGS = -g -cpp -DCOMPILER=$(COMPILER_ENUM) -fno-align-commons -static -fcheck=bounds -fno-automatic -finit-local-zero $(FFLAGS_ARCH) $(FFLAGS_EXTRA_CHECKS)
# Try removing -fno-automatic, results in runtime errors
#FFLAGS = -g -cpp -DCOMPILER=$(COMPILER_ENUM) -fno-align-commons -static -fcheck=bounds -finit-local-zero $(FFLAGS_ARCH) $(FFLAGS_EXTRA_CHECKS)
# Try removing -fno-automatic and -finit-local-zero and print warnings for code review
# - use this periodically to improve code
FFLAGS = -g -O1 -Wall -cpp -DCOMPILER=$(COMPILER_ENUM) -fno-align-commons -static -fcheck=bounds $(FFLAGS_ARCH) $(FFLAGS_EXTRA_CHECKS)
#FFLAGS = -O2
# The flags for all cases (e.g. look for system .mod files, required in gfortran)
FFLAGS += -I/usr/include
# The libraries needed for linking
#LDFLAGS = -li_need_this_lib
# The list of executables to be built within the package.
PROGRAMS = statemod

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
	@echo "StateMod makefile targets:"
	@echo ""
	@echo "all           Default target that prints this help message."
	@echo "checkarch     Check that object files are consistent with shell 32 or 64 bits."
	@echo "clean         Remove dynamically created files (but not final executable)."
	@echo "help          Print this message."
	@echo "printenv      Helper to print which operating system is used, controls configuration."
	@echo "statemod      Compile the StateMod executable, recompiling any .o if .for modified."
	@echo "statemodrb    For Ray Bennett - compile the StateMod executable, and copy to his run folder."
	@echo "veryclean     Make the 'clean' target, and also remove the final executable."
	@echo ""
	@echo "file.o        Compile the source file.for file into object file file.o,"
	@echo "              useful to check syntax for a single file."
	@echo "-------------------------------------------------------------------------------------------"
	@echo "Targets specific to Ray Bennett:"
	@echo ""
	@echo "statemodrb    Compile the StateMod executable and copy to Ray's run folder."
	@echo "verycleanrb   Veryclean full compile, save log to statemod-compile.log,"
	@echo "              and copy executable to Ray's run folder.  DOES NOT ECHO TO SCREEN UNTIL DONE."
	@echo "-------------------------------------------------------------------------------------------"
	@echo "Important makefile variables that are used:"
	@echo ""
	@echo "FC (compiler) = $(FC)"
	@echo "STATEMOD_VERSION (from statem.for) = $(STATEMOD_VERSION)"
	@echo "EXE_BITS (from environment) = $(EXE_BITS)"
	@echo "-------------------------------------------------------------------------------------------"
	@echo "Important environment variables that are used:"
	@echo ""
	@echo "OS (to determine if Windows) = $(OS)"
	@echo "MSYSTEM (to determine EXE_BITS on Windows) = $(MSYSTEM)"
	@echo "-------------------------------------------------------------------------------------------"
	@echo "To force compile of files even if errors:  make -k statemod"
	@echo "To force compile and save output to file:  make -k statemod 2>&1 | tee statemod-compile.log"
	@echo "To count warning types from above:         ./count-warnings.bash"
	@echo "-------------------------------------------------------------------------------------------"

# Dependencies for object files:
# - because statemod is the main output (not library, etc.) only concern is the include file
# - TODO smalers 2021-03-11 need to make this simpler, but something like makedepends may be too much
# - not every file depends on common.inc (do `grep -i common.inc *.for` to find)
# - list alphabetically, ignoring case in order
bintop.o: common.inc
bomsec.o: common.inc
calldat.o: common.inc
carrpl.o: common.inc
closs.o: common.inc
datinp.o: common.inc
daydivo.o: common.inc
dayest.o: common.inc
daymon.o: common.inc
dayoutr.o: common.inc
dayrate.o: common.inc
dayreso.o: common.inc
dayset.o: common.inc
daywelo.o: common.inc
demand.o: common.inc
demcons.o: common.inc
deplete.o: common.inc 
depletep.o: common.inc
directby.o: common.inc
directex.o: common.inc
directfs.o: common.inc
directwr.o: common.inc
divalt.o: common.inc
divcar.o: common.inc
divcar1.o: common.inc
divcar2.o: common.inc
divcarl.o: common.inc
divcarr.o: common.inc
divimpr.o: common.inc
divimpr2.o: common.inc
divmulti.o: common.inc
divmultR.o: common.inc
divres.o: common.inc
divresp.o: common.inc
divresp2.o: common.inc
divresr.o: common.inc
divrig.o: common.inc
divrigs.o: common.inc
divrpl.o: common.inc
divrplp2.o: common.inc
divrplp.o: common.inc
divrplr.o: common.inc
dsamod.o: common.inc
evasec.o: common.inc
execut.o: common.inc
FlowRes.o: common.inc
getcall.o: common.inc
getdiv.o: common.inc
getdiv2.o: common.inc
geteomx.o: common.inc
getid.o: common.inc
getipy2.o: common.inc
getipy4.o: common.inc
getpar.o: common.inc
getpln.o: common.inc
getplnr.o: common.inc
getplnw.o: common.inc
getrep.o: common.inc
getres.o: common.inc
getres2.o: common.inc
getrig.o: common.inc
getrtnx.o: common.inc
getsta.o: common.inc
getwel.o: common.inc
getwel2.o: common.inc
gwsub.o: common.inc
ifrdown.o: common.inc
ifrrig.o: common.inc
ifrrig2.o: common.inc
ifrrigsp.o: common.inc
ifrrigx.o: common.inc
intersup.o: common.inc
JMFlow.o: common.inc
JMStore.o: common.inc
mdainp.o: common.inc
oopbook.o: common.inc
oopbook2.o: common.inc
oopdiv.o: common.inc
oprfind.o: common.inc
oprinp.o: common.inc
ouflow.o: common.inc
outbal2.o: common.inc
outcall.o: common.inc
outcallr.o: common.inc
outcalls.o: common.inc
outcu.o: common.inc
outdeb.o: common.inc
outdivc.o: common.inc
outdivw.o: common.inc
outgvc.o: common.inc
outichk.o: common.inc
outifr.o: common.inc
OutJM.o: common.inc
outmon.o: common.inc
outopr.o: common.inc
outpln.o: common.inc
outplnmo.o: common.inc
outplnw.o: common.inc
outpltd.o: common.inc
outpltr.o: common.inc
outpltw.o: common.inc
outrch.o: common.inc
outrchr.o: common.inc
outres.o: common.inc
outresc.o: common.inc
outrev.o: common.inc
outrg.o: common.inc
outriv.o: common.inc
outsp.o: common.inc
outspd.o: common.inc
outsum.o: common.inc
outsys.o: common.inc
outsyt.o: common.inc
outsyta.o: common.inc
outtbl.o: common.inc
outtop.o: common.inc
outwel.o: common.inc
outwelc.o: common.inc
outwelp.o: common.inc
outwr.o: common.inc
outwr2.o: common.inc
outWW.o: common.inc
outxss.o: common.inc
outxssmo.o: common.inc
planeva.o: common.inc
powres.o: common.inc
powres2.o: common.inc
powresp.o: common.inc
powsea.o: common.inc
powseap.o: common.inc
powsear.o: common.inc
replace.o: common.inc
report.o: common.inc
resoop.o: common.inc
resrg1.o: common.inc
resrg1p.o: common.inc
resrg2.o: common.inc
resrgp.o: common.inc
resrpl.o: common.inc
rgrg.o: common.inc
riginp.o: common.inc
rigsor.o: common.inc
rivrtn.o: common.inc
rsrspu.o: common.inc
rsrspup.o: common.inc
rtnmax.o: common.inc
rtnmaxe.o: common.inc
rtnsec.o: common.inc
rtnsecc.o: common.inc
rtnsecm.o: common.inc
rtnsecp.o: common.inc
rtnsecr.o: common.inc
rtnsecrp.o: common.inc
rtnsecw.o: common.inc
rtnsecwp.o: common.inc
rtnsecx.o: common.inc
rtnxcu.o: common.inc
rtnxcup.o: common.inc
sepsec.o: common.inc
setdem.o: common.inc
seteff.o: common.inc
setgw.o: common.inc
setplano.o: common.inc
sjrip.o: common.inc
soilm.o: common.inc
splatte.o: common.inc
spruse.o: common.inc
statem.o: common.inc
vircom.o: common.inc
virgain.o: common.inc 
virgen.o: common.inc
virin.o: common.inc
virnod.o: common.inc
virout.o: common.inc
virset.o: common.inc
welaugp.o: common.inc
welprity.o: common.inc
welrech.o: common.inc
welrig3.o: common.inc
welrig3p.o: common.inc
xdebug.o: common.inc

# The rule for building the statemod executable:
# - all source modules are listed as dependencies
# - interdependencies are not accounted for, just the main program requirements
# - the following were in Jim Brannon's code and have been integrated more generically:
#
#	parse_gfortran.o
#	dattim_gfortran.o
#
#	getpath_windows.o
#	putpath_windows.o 
#
# If necessary, temporarily copy files with errors into a comment below this line to let all compile.
statemod: \
	checkarch \
	statemod_compile

# This target is used internally to allow 'checkarch' to be executed.
statemod_compile: \
	statem.o \
		namext.o \
		$(parse_o_file) \
		$(getpath_o_file) \
		license.o \
		skipn.o \
		getfn.o \
		setpar.o \
		virgen.o \
		execut.o \
		report.o \
		xdebug.o \
			accdiv.o \
			accou.o \
			adjcase.o \
			average.o \
			bintop.o \
			bomsec.o \
			calldat.o \
			carrpl.o \
			chekav2.o \
			chekav3.o \
			chekava.o \
			chekpor.o \
			chekres.o \
			chekts.o \
			chkavail.o \
			chkavail2.o \
			chkprf.o \
			chkver.o \
			closs.o \
			coeffa.o \
			comment.o \
			count.o \
			datinp.o \
			$(dattim_o_file) \
			dattim_ms.o \
			dattim_string.o \
			daydata.o \
			daydist.o \
			daydivo.o \
			dayest.o \
			daymon.o \
			dayoutr.o \
			dayreso.o \
			dayset.o \
			daywelo.o \
			demand.o \
			demcons.o \
			deplete.o \
			depletep.o \
			directby.o \
			directex.o \
			directfs.o \
			directwr.o \
			divalt.o \
			divcar.o \
			divcarl.o \
			divcarr.o \
			divcar1.o \
			divcar2.o \
			divimpr.o \
			divimpr2.o \
			divmulti.o \
			divmultR.o \
			divres.o \
			divresp2.o \
			divresr.o \
			divrig.o \
			divrigs.o \
			divrpl.o \
			divrplp.o \
			divrplp2.o \
			divrplr.o \
			dnmfso.o \
			dnmfsow.o \
			dnmfso2.o \
			dsamod.o \
			evasec.o \
			FlowRes.o \
			getcall.o \
			getctlc.o \
			getdiv.o \
			getdiv2.o \
			geteomx.o \
			getfile.o \
			getid.o \
			getin.o \
			getipy2.o \
			getipy4.o \
			getpar.o \
			getpln.o \
			getplnr.o \
			getplnw.o \
			getrch.o \
			getrep.o \
			getres.o \
			getres2.o \
			getrig.o \
			getrtn.o \
			getrtnx.o \
			getrtnw.o \
			getrtnw2.o \
			getsta.o \
			getwel.o \
			getwel2.o \
			getver.o \
			gwsub.o \
			ifrdown.o \
			ifrrig.o \
			ifrrigsp.o \
			ifrrigx.o \
			ifrrig2.o \
			interp.o \
			intersup.o \
			isleapyear.o \
			JMFlow.o \
			JMStore.o \
			mdainp.o \
			oopbook2.o \
			oopdiv.o \
			oprdown.o \
			oprexp.o \
			oprfind.o \
			oprinout.o \
			oprinp.o \
			ouflow.o \
			outbal2.o \
			outcallr.o \
			outcalls.o \
			outcu.o \
			outdeb.o \
			outdivc.o \
			outdivw.o \
			outgvc.o \
			outichk.o \
			outifr.o \
			OutJM.o \
			outmon.o \
			outopr.o \
			outpln.o \
			outplnmo.o \
			outpltd.o \
			outpltr.o \
			outpltw.o \
			outrch.o \
			outrchr.o \
			outres.o \
			outresc.o \
			outrev.o \
			outrg.o \
			outriv.o \
			outsp.o \
			outspd.o \
			outsys.o \
			outsyt.o \
			outsyta.o \
			outtbl.o \
			outtop.o \
			outwel.o \
			outwelc.o \
			outwelp.o \
			outwr2.o \
			outWW.o \
			outxss.o \
			outxssmo.o \
			planeva.o \
			powrig.o \
			powres.o \
			powresp.o \
			powres2.o \
			powsea.o \
			powseap.o \
			powsear.o \
			$(putpath_o_file) \
			replace.o \
			repsort.o \
			resrgp.o \
			resrg1.o \
			resrg1p.o \
			resrpl.o \
			return.o \
			return2.o \
			riginp.o \
			rigsor.o \
			rivrtn.o \
			rgrg.o \
			roundof.o \
			rsrspu.o \
			rsrspup.o \
			rtncarry.o \
			rtnmax.o \
			rtnmaxe.o \
			rtnsec.o \
			rtnsecc.o \
			rtnsecp.o \
			rtnsecr.o \
			rtnsecrp.o \
			rtnsecw.o \
			rtnsecwp.o \
			rtnsecx.o \
			rtnxcu.o \
			rtnxcup.o \
			sepsec.o \
			setcarl.o \
			setdem.o \
			seteff.o \
			setgw.o \
			setlimit.o \
			setloss.o \
			setplano.o \
			setqdiv.o \
			setqdivc.o \
			setqdivx.o \
			settc.o \
			sjrip.o \
			skip.o \
			soilcu.o \
			soilin.o \
			soilm.o \
			somnmy.o \
			splatte.o \
			spruse.o \
			stafind.o \
			takout.o \
			takou2.o \
			vircom.o \
			virgain.o \
			virin.o \
			virnod.o \
			virout.o \
			virset.o \
			welaugp.o \
			welprity.o \
			welrech.o \
			welrig3.o \
			welrig3p.o \
			year.o \
			year2.o
ifdef OS
	@# Windows...
	$(FC) $(FFLAGS) -o statemod-$(STATEMOD_VERSION)-gfortran-win-$(EXE_BITS)bit.exe $^ $(LDFLAGS)
	@echo "-----------------------------------------------------------------------"
	@echo "Executable is statemod-$(STATEMOD_VERSION)-gfortran-win-$(EXE_BITS)bit.exe"
	@echo "-----------------------------------------------------------------------"
else
	@# Linux...
	$(FC) $(FFLAGS) -o statemod-$(STATEMOD_VERSION)-gfortran-lin-$(EXE_BITS)bit $^ $(LDFLAGS)
	@echo "-----------------------------------------------------------------------"
	@echo "Executable is statemod-$(STATEMOD_VERSION)-gfortran-lin-$(EXE_BITS)bit"
	@echo "-----------------------------------------------------------------------"
endif

# Special target for Ray Bennett:
# - compile as above
# - copy the executable to a folder that Ray uses to run tests
statemodrb: statemod
ifdef OS
	@# The 
	@echo "Copying executable to /C/cdss/bin."
	@# Create the folder if it does not exist.
	@mkdir -p /C/cdss/bin
	@# Copy the executable to a different location.
	@cp statemod-$(STATEMOD_VERSION)-gfortran-win-$(EXE_BITS)bit.exe /C/cdss/bin
else
	@echo "statemodrb target is only enabled on windows."
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

# Special target for Ray Bennett.
# Clean compile and log the results to a file.
# Count the warnings and print to screen and append to the compile log file.
# 
verycleanrb: veryclean
	@echo "Compiling statemod after veryclean - full compile."
	@# Have to compile using a shell command because of output redirect.
	$(shell make -k statemodrb 2>&1 | tee statemod-compile.log)
	@# Summarize the warning count to the screen and end of compile log file.
	$(shell ./count-warnings.bash 2>&1 | tee --append statemod-compile.log)

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
	rm -f *~ $(PROGRAMS) statemod-$(STATEMOD_VERSION)-gfortran-win-$(EXE_BITS)bit.exe
else
	# Linux
	# - only remove the executable for the OS architecture of interest
	# - no file extension
	rm -f *~ $(PROGRAMS) statemod-$(STATEMOD_VERSION)-gfortran-lin-$(EXE_BITS)bit
endif
