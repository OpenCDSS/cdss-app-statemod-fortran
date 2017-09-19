# A simple hand-made makefile for a package including applications
# built from Fortran 90 sources, taking into account the usual
# dependency cases.
# - Steve Malers removed src and bin folders so all work is done in this folder.
# ======================================================================
# the declarations
# ======================================================================
# The compiler
FC = gfortran
# The flags for debugging or for maximum performance, comment as necessary
#FCFLAGS = -g -fbounds-check -fno-align-commons
# use -static especially for windows compiling 
#FCFLAGS = -g -cpp -fbounds-check -fno-align-commons -static
FCFLAGS = -g -cpp -fno-align-commons -static -fbounds-check -fno-automatic -finit-local-zero
#FCFLAGS = -O2
# The flags for all cases (e.g. look for system .mod files, required in gfortran)
FCFLAGS += -I/usr/include
# The libraries needed for linking
#LDFLAGS = -li_need_this_lib
# The ist of executables to be built within the package
PROGRAMS = statemod

# ======================================================================
# The rules
# ======================================================================
# The rule for making "all"
#   the default if make is called without arguments
#   note it only has the dependency and no actions
all: $(PROGRAMS)

# The rule for object files that depend on more than
#  the same named source (.for) file.  A general rule
#  below handles that simple case.
#  For example, include files.
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
divalt.o: common.inc
divcar1.o: common.inc
divcar2.o: common.inc
divcar.o: common.inc
divcarl.o: common.inc
divcarr.o: common.inc
divimpr.o: common.inc
divmulti.o: common.inc
divres.o: common.inc
divresp2.o: common.inc
divresp.o: common.inc
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
getcall.o: common.inc
getdiv2.o: common.inc
getdiv.o: common.inc
geteomx.o: common.inc      
getid.o: common.inc
getipy2.o: common.inc
getipy4.o: common.inc
getpar.o: common.inc
getpln.o: common.inc
getplnr.o: common.inc
getplnw.o: common.inc
getrep.o: common.inc
getres2.o: common.inc
getres.o: common.inc
getrig.o: common.inc
getsta.o: common.inc
getwel2.o: common.inc
getwel.o: common.inc
gwsub.o: common.inc
ifrdown.o: common.inc
ifrrig2.o: common.inc
ifrrig.o: common.inc
ifrrigsp.o: common.inc
ifrrigx.o: common.inc
intersup.o: common.inc
mdainp.o: common.inc
oopbook2.o: common.inc
oopbook.o: common.inc
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
outresc.o: common.inc
outres.o: common.inc
outrev.o: common.inc
outrg.o: common.inc
outriv.o: common.inc
outspd.o: common.inc
outsp.o: common.inc
outsum.o: common.inc
outsys.o: common.inc
outsyta.o: common.inc
outsyt.o: common.inc
outtbl.o: common.inc
outtop.o: common.inc
outwelc.o: common.inc
outwel.o: common.inc
outwelp.o: common.inc
outwr2.o: common.inc
outwr.o: common.inc
outxss.o: common.inc
outxssmo.o: common.inc
planeva.o: common.inc                      
powres2.o: common.inc
powres.o: common.inc
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
rtnmaxe.o: common.inc
rtnmax.o: common.inc
rtnsecc.o: common.inc
rtnsec.o: common.inc
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

# The rule for building the statemod executable
#   The following is a carefully constructed list created by starting with statem.o
#   and adding files as needed to reconcile missing subroutine error messages.  
statemod: \
	statem.o \
		namext.o \
		parse_gfortran.o \
		getpath_windows.o \
		gnu.o \
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
			dattim_gfortran.o \
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
			divalt.o \
			divcar.o \
			divcarl.o \
			divcarr.o \
			divcar1.o \
			divcar2.o \
			divimpr.o \
			divmulti.o \
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
			putpath_windows.o \
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
	$(FC) $(FCFLAGS) -o statemod $^ $(LDFLAGS)

# Using Fortran MODULES: prog4.f90 USEs a Fortran module defined
# inside mod.f90, this is similar to the include case (prog2), but,
# since there is no standard naming convention for compiled module
# files in f90, the dependency is more easily built on the object
# files, because when mod.o is generated, one is sure that
# any_module_inside_mod.mod has been newly generated as well; mod.o
# must also be linked in when building the executable, so the
# dependency on mod.o is added also for prog4, as in the external
# procedure case (prog3):
#prog4.o: mod.o
#prog4: mod.o

# ======================================================================
# The general rules.  These generally should not require modification
# ======================================================================

# General rule for building prog from prog.o; $^ (GNU extension) is
# used in order to list additional object files on which the
# executable depends
%: %.o
	$(FC) $(FCFLAGS) -o $@ $^ $(LDFLAGS)

# General rules for building prog.o from prog.f90 or prog.F90; $< is
# used in order to list only the first prerequisite (the source file)
# and not the additional prerequisites such as module or include files
%.o: %.f90
	$(FC) $(FCFLAGS) -o $@ -c $<

%.o: %.F90
	$(FC) $(FCFLAGS) -o $@ -c $<

#added by jhb for the statemod files from ray b after changing everything to lower case
%.o: %.for
	$(FC) $(FCFLAGS) -o $@ -c $<

# Utility targets
.PHONY: clean veryclean

clean:
	rm -f *.o *.mod *.MOD

veryclean: clean
	rm -f *~ $(PROGRAMS)

