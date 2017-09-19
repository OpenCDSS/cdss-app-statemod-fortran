C
C
C               Revised common block for linked model
c
c rrb 2010/09/10; Revise dimensions
c   Div Rights		    5500 = 6500
c   Res Rights 		    201 = 351
c   Well Rights		    11000 = 7500  13110
c   Well Returns	    10000 = 13110
c	  Total Structs	    14000 = 7000
c   Operation Rights	601 = 800  3701
c   Diversion Returns	5001 = 3920 3920
c   Well Returns			13110 = 604 3080
c   Well Depletions   3080 8500
c   Number of rights	7000 = 7600 14991
c   Max Dly						3100 = 302 525
c   Max ISF Reach			2299 = 1002
c rrb 2008/09/22; Add water right on/off switch read 
c		    (e.g. idvrsw1 not idvrsw) in order
c		    to allow more than one operating rule
c		    to refrence a water right
c
c rrb 98/11/18; Wells
c rrb 99/10/05; Return table total (dlytot)
c rrb 00/02/07; increase dimension of return data from 49 to 200 
c rrb 00/04/07; add primary sw/gw switch
c rrb 00/04/24; change max # of gw returns 3402 = 5002
c rrb 00/04/24; chagne max # of delay tables 200 = 550

c rrb 00/05/02; change max # of gw returns from 9002 to 20000
c rrb 00/06/16; add (idemtyp) to control source of well demands and 
c               allow IWR data to be provided as a demand
c rrb 00/06/16; added IWR data for diversions (diverir) and 
c               wells (diverirW)
c rrb 00/06/16; added total demand by a structure (divert)
c rrb 00/07/03; added total IWR demand by structure (diverirT)
c rrb 00/08/05; Revised file name length from 72 to 256 and added maxfn
c rrb 00/10/03; Revised res rights 96 - 351
c rrb 00/10/23; Added rgspill to accomodate Elephant Butte Spills
c rrb 00/10/30; Began ability to read randum input file
c rrb 00/11/18; Add time series of Area served by GW (areawa), 
c               served by sprinklers (areasp) and
c               time series of well capacity (divcapwa)
c rrb 00/12/04; For variable efficiency capability add max efficiency
c               for SW ditch (effmax), for GW flood (effmaxw), and
c               and GW sprinkler (effmaxs).  
c rrb 00/12/04; Add IWR for variable efficiency for a ditch (diwr) and
c               a well (diwrw).  Note diwr is for variable efficiency 
c               calculations while diverir is for demand calculations.
c               They differ since diverir uses average efficiency
c rrb 00/12/09; For variable efficiency add running IWR demand for a 
c               diversion (diwrreq()) and for a well (diwrreqw) also 
c               add running cu for a diversion (dcu) and well (dcuw)
c rrb 01/01/05; Sprinkler switch (ispron)
c rrb 01/01/25; CU update 
c               dcu  =CU from SW for a Div or D&W, 
c               dcuw =CU from GW for a Well Only, 
c               dcut =CU from SW, GW & Soil for a Div or D&W
c               dcutw=CU from GW and Soil for a Well Only
c rrb 01/02/23; divsw added to accomodate demand type 4
c rrb 01/03/29; Added generic units (fm(13), fd(13), unitd, unitm)
c rrb 01/03/29; Added tempTS, tempFS and tempCU for baseflow to soil,
c               from soil and CU
c rrb 01/09/25; Added itspd and itspw as indicators if *.tsp data is
c               provided
c
c               980=550       Max number of delay tables
c               930 = 3660 (20*366) max number of daily returns
c               3920=3920 for delay variables only (with 3660)
c rrb 01/12/17; Added idaydem to allow daily SW demand to use monthly 
c               total in demand.for
c rrb 01/12/26; Added avtemp(3920), avwret(3920) & dumx(2278)
c                 to resolve equivalence issues
c rrb 01/12/31; Added cunitd2 and cunitm2 (units w/o a ( )
c rrb 02/01/15; Added common /vir2/ from virset.f and
c                     common/gen1/ from bomsec.f
c rrb 02/05/29; Add iwx iteration per time step
c rrb 03/08/18; Add random inut file data
c               (filetype, filename, filenumb)
c rrb 03/10/24; Add irestyp for reservoir water right type
c               +n = water right to account n
c               -n = water right goes to accounts n based on available
c                    space in accounts 1-n
c               0  = water right goes to all accounts based on
c                    ownership ratio
c rrb 06/15/04; Dimension for linked model
c rrb 04/10/15; change maxuse from 2278 to 2500
c rrb 04/11/22; Added plan (augmentation and Terms and Conditions) 
c		see PlanCom
c rrb 04/12/28; Added reuse plan data. See PlanCom
c rrb 05/07/22; Added call information (imcdC)
c rrb 06/08/29; Data for new CU approach
c
c
c
c _________________________________________________________

       COMMON/WEL/
     1 iwell,
     1 numdivw,  numdxw,   numrtnw,  numrtnw2, 
     1 numdivwS, numdivwX, maxrtnw,  
     1 maxdivw,  maxdvrw,  ndivinw,  maxwr,
     1 numdvrw,  nstrtnw,  nstrtnw2, iprimary, idemtyp, 
     1 maxfutil,        
cx     1 divnamw( 6,402),
     1 idivsww(402),    divcapw(402),     
     1 idvstaw(402),    idvcomw(402),     divefcw(402), areaw(402),  
     1 diveffw(12,402), irturnw(8500),   demsrcw(402), 
     1 nrtnw(402),      pcttotw(8500),   
     1 pctlosw(8500),  irtndlw(8500),   irnordW(8500),
     1 nrtnw2(8500),     pcttotw2(8500),  
     1 pctlosw2(8500), irtndlw2(8500),
     1 irnstaw(8500),  irnstaw2(8500),
     1 diverw(12,402),  diverirw(12,402),  
c
c rrb 2007/10/02; Remove divgw     
c    1 divreqw(402),    divgw(402),
     1 divreqw(402),    
     1 rlossw(402),     rlossw2(402),     
     1 idivcow2(402),   CarryW(402), 
c
c rrb 99/08/16; Well depletion to stream (rdepw)
     1 rdepw(402),
c               Wells rights (riginp.f)
     1 dcrdivw(13110),    idvrsww(13110),   idvrsww1(13110), 
     1 idivcow( 2,13110), nrigusw(13110),   irigusw(13110), 
c               Operations
     1 usemonw(13110),   divdw(13110),     divmonw(13110),
     1 gwstor(2500),    gw2riv(2500),    gw2gw(2500),   gwmaxr(2500), 
c
c               Depletion
     1 depl(240,3920), depld(3660,3920),  deplm(3920)
c
c
c _________________________________________________________
c               Wells real*8
c rrb 00/04/07; Add SW/GW priority switch by structure
       common/WELr/
     1 rdvnkw(13110), primary(402)
c
       real*8
     1 rdvnkw, primary
c
c _________________________________________________________
c     
c               Well rights
       COMMON/WELc/
     1 namedw(13110),      cdividw(402),  crigidw(13110)
c
       character
     1 namedw*24,         cdividw*12,    crigidw*12
c
c
c _________________________________________________________
c                   Streams


       COMMON/SY1/
cx     2 STANAM( 6,2500),XFRNAM( 6, 241),RESNAM( 6,  251),
cx     3 DIVNAM( 6,1530),USENAM( 6,1530),
cx     4 RUNNAM( 6,2500),
     6 MTHDAY(     12),IDATE (      3),ITIME (      4),
     6 IPAGE (     20),ILINE (     20),
     1 iyrmo (     13),imomo (     13)
     
       common/sy1c/
     1 HEADIN1(2)     
       character headin1*80
C
c
c _________________________________________________________

      COMMON/SY2/
     1 IDNCOD(   2500),NDNNOD(   2500),IUPCOD(   2500),
     2 NUPNOD(     61),IRUSTA(   2500),IRUTYP(   2500),
c
c rrb 2009/05/26; Correction     
cx   3 ISTRTN(   2500),IRNORD(   2500),NWRORD(2,14991),
     3 ISTRTN(   5005),IRNORD(   2500),NWRORD(2,14991),
     4 QSTERN(   1530),QTRIBU(   2500),ISTARU(   2500),
     5 IRUDND(   2500),TotSup(2500)
c
c
c _________________________________________________________

c rrb 97/10/22; Redimension for 25 replacement reservoirs
      common/repx/
     1 irepn,          irepk(25),       irepnr(25), 
     1 irepown(25),     ireprnk(25), 
     1 irepexp(25,1530),ireptyp(1530),
c
c grb 4-23-97 added variable iopdesr & iopSouR to reflect diversion 
c	      and source type
     1 iopDesR(3701), iopSouR(3701),
c
c rrb 2007/10/26; Add Replacement Reservoir max release 
c                 operating ID pointer (irepMax) and operating limit
c		  code (iRepLim)  
     1 irepMax(25), iRepLim(25)
c
c
c _________________________________________________________

c rrb 04/15/96
C
      COMMON/SY2a/
     1 PREPRT(12,  251),EVAPRT(12,  251),RUNOFF(     12),
     1 DIVERM(     12)
C
      COMMON/SY2b/
     1 VIRINP(12,2500),RIVER (   2500),AVINP (   2500),
     1 AVAIL (   2500),DIVREQ(   1530),IRSORD( 2,2500),
     1 ineg  (   2500), qneg (   2500),
     1 virinpT(12,2500),
     1 divsw(1530)

C
c
c _________________________________________________________
c                   Unit Response


      COMMON/SY3/
c
c rrb 2009/05/27; Revise      
cx   1                 DLYRAT(240, 525),RETUR (240,2500,2),
     1                 DLYRAT(240, 525),RETUR (240,2500),
     1 QDIV  (39,2500),QRES  (39,  251),CURRTN(   2500),
     1 irtnid(    525),
     1 dlytot(525)
c
c
c _________________________________________________________
      
      COMMON/SY4/
     1 DUMMY (2500, 13), dummyd(500),     dumx(2500),
     1 dum(13,2500),     dum2(13,2500),   dum3(13,2500),
     1 dumsta(2500),     dumown( 251,2),  dumrig(351,2),
     1 dumusr(2500),     dumd(32,2500),
     1 idum(2500),       idum2(2500),     iwarn(2500),
     1 avtemp(2500),     avwret(2500)
C
c
c _________________________________________________________

      COMMON/IND/
     1 MAXSTA ,NUMSTA ,MAXRES ,NUMRES ,MAXDIV ,NUMDIV ,
     2 MAXRTN ,maxrtnA,NUMRTN ,MAXDLY ,NUMDLY ,MAXPRE ,NUMPRE ,
     3 MAXEVA ,NUMEVA ,MAXRUN ,NUMRUN ,numrunG,MAXRAI ,NUMRAI ,
     4 MAXEPT ,NUMEPT ,MAXNWR ,NTORIG ,MAXFRR ,NUMFRR ,
     4 MAXIFR ,NUMIFR ,MAXRSR ,NUMRSR ,MAXDVR ,NUMDVR ,
     5 MAXOWN ,NUMOWN ,MAXUSE ,NUMUSE ,MAXPWR ,NUMPWR ,
     6 MAXOPR ,NUMOPR ,NDIVIN ,NWRTOT ,NSTRTN ,NRSACT ,
     7 MONEVA ,MONDIV ,NDVTAP ,IOPOUT ,IOPFLO ,IRESOP ,
     8         IOPTIO ,MON    ,IMO    ,IYR    ,IYSTR  ,
     9 IYEND  ,INTERV ,NDMDLY ,NUMMIN ,FACTOR ,
     1 RFACTO ,DFACTO ,FFACTO ,CFACTO ,
     1 efacto, pfacto, WFACTO ,
     1 RFACTO1,DFACTO1,FFACTO1,CFACTO1,
     1 efacto1,pfacto1,WFACTO1,    
     1 dfactoA ,dfactoI ,dfactoO, ffactoA, 
     1 CFACTOT, NRVREP ,NWRITE ,
     1 ibrrep , ioprtn ,isgi,
     1 icondem,maxbas, numbas, maxacc, ichk,   ichk99,
     1 iystr0, iyend0, maxdla, 
     1 maxdlm, maxrep, maxrea, numrea, maxio,  maxtrnw,
     1 maxoprin,maxparm, maxQdiv,
     1 ndlymx, numtop, idallx, icall,  io99,   ftot,
     1 monisf, nisfinA,nisfinM,nrgfor, maxrg,  maxfn,
     1 maxgrp, numgrp, iinput,
     1 iwx, maxfile, infile,   nchk,   nlog,   ntmp,
     1 isigfig, ndivO, nresO, nwelO,   maxopr2,
     1 fmo(13),fdy(13),faf(13),ndly(525)
C
c
c _________________________________________________________
c                   Instream Flows

      COMMON/IFR/
     1 DCRIFR(241),    IIFRSW(     241), iifrsw1(241), 
     1 divi(241),      ndnifs(     241), ndnifb(    241),
     1 IFRSTA(    241),IIFRCO(     241), FLOWR (12, 241),
     1 flowrX(12,241), 
     1 FLOWRQ(    241),IFRRSW(     241), ifrst2(    241),
     1 florqr(1002),   divir(1002),      qdivr(1002),
     1 ireach,
     1 iifcom(241),    demconi(241)
C
c
c _________________________________________________________
c                   Reservoirs

      COMMON/RS1/
     1 VOLMIN(    251), VOLMAX(    251),FLOMAX(     251),
     1 DEADST(    251), NOWNER(    251),NEVAPO(     251),
     1 NPRECP(    251), IRESSW(    251),NRANGE(     251),
     1 SUAREA(251,251), CONTEN(251,251),OWNMAX(     251),
     1 CUROWN(    251), PCTEVA(    251),IEVAPO(     251),
     1 WEIGEV(    251), IPRECP(    251),WEIGPR(     251),
     1 SEPCON(251,251), SEPAGE(251,251),SEPACT(     251),
     1 sepact1(251),    nSepCal(251),  
     1 iresP(251),     
c
c ---------------------------------------------------------
c                    Reservoir Recharge (seepage)     
     1 idvRre(2500),    
     1 RrechM(13,251),   RrechD(32,251), Rrech(251)
c
c
c _________________________________________________________

      COMMON/RS1/
     1 IRSSTA(     251),EVAP  (     251),VOLINT(     251),
     1                                   CURSTO(     251),
     1 PROJTF(     251),SPILL (     251),CARRY (     251),
     1 OWNMON(    251),                  IOWNER(    251),
     1 REPLAC(     251),QMAINS( 2, 251)
C
c
c _________________________________________________________

      COMMON/RS1/
     1                 DCRRES(     351),IRESCO( 2,  351),
     1 IRRGST(     351),NRIGOW(    251),IRIGOW(     351),
     1 NRIGRS(     251),IRIGRS(    351),LAST  (     351),
     1 RITREM(     351),RitPaper(351),  ITYRSR(     351),
     1 IRSRSW(     351),irsrsw1(351), irestyp(351),   iresopr(351),
     1 IRSMIN(     251),CONMIN(12,  251),
     1 itarget(    251),targetx(12, 251),tarmax(251),
     1 rdate  (    251),divr(351),targetn(12,251),conminn(12,251),
     1 nextnr(251)
C
c
c _________________________________________________________
c                   Diversions


      COMMON/DV1/
     1 DIVCAP(   1530),AREA  (   1530),NDUSER(   1530),
     1 IDVCOM(   1530),NRIGUS(   1530),IDUSER(   1530),
c
c rrb 2009/05/27; Iopvir is not used     
cx   1 IDVSTA(   1530),IPOVIR(   2228),DIVMON(   1530),
     1 IDVSTA(   1530),DIVMON(   1530),
     1 USEMON(   1530),IDIVSW(   1530),demsrc(   1530),
     1 rloss(1530), idivco2(1530),     iuseTyp(1530),
c
c                    Diversions to Recharge (carried) and Use
     1 idvDre(2500),   
     1 DrechM(13,1530),  DrechD(32,1530),   Drech(1530),
     1 DuseM(13,1530),   DuseD(32,1530),    Duse(1530)
     
C
c
c _________________________________________________________

      COMMON/DV2/
     1 NRTN  (   1530),IRNSTA(   3920),IRTNDL(   3920),
     1 IRTURN(   1530),PCTTOT(   3920),pctlos(   3920),
     1 iretsw,         ireop,          ireopx, divchk,
     1 divchkC,        divchkr
C
c
c _________________________________________________________

      COMMON/DV3/
     1                 DCRDIV(   6500),IDIVCO( 2,6500),
     1 IRIGUS(   6500),IDRGST(   6500),DIVEFF(12,6500), divefc(6500),
     1 IDVRSW(   6500),idvrsw1(6500),  ITYDVR(   6500),
     1 divd(6500)
C
c
c _________________________________________________________

      COMMON/DV4/ 
     1 DIVER (12,1530), divert(12,1530), diverir(12,1530),
     1 diverirT(12,1530),demcond(1530)

C
c
c _________________________________________________________
c
c		Operating Rule Data #2
      COMMON/OPR1/
     1                  DCRPOW(     251),IPRGST( 2,  251),
     2 POWDMD(12,  251),POWREQ(     251),POWREL(     251),
     3 IPOWSW(     251),
     4 imonsw(3701,12),  oprmax(3701,13),  oprmaxM(3701), oprmaxA(3701),     
     1 oprPct(3701),     ireuse(3701),     
     1 dcrdivS(3701),    divdS(3701),      dcrdivE(3701), divdE(3701),
     1 divopr(3701),     OprEff(12, 3701)
C
c
c _________________________________________________________
c
c		Operating Rule Data #2
      COMMON/OPR2/
     1 demopr(   1500),  iopsou(20, 3701), divreqa(3701),
     1 IOPDES(20, 3701), ITYOPR(    3701), IORGST(    3701), 
     1 IOPRSW(    3701), ICONFL(    3701), intern(3701,20),  
     1 divo(3701),       iopid(351),       qdel(13,5),     
     1 qindex(13,5),     
     1 irg1,             irg2,             isp1,
     1 forecast(13,241), qcarry(5),        qdebt(5),
     1 qdebtx(5),
     1 rgspill(241),    iSetOpr(3701),     oprLoss(3701),
     1 oprLimit(3701),
     1 iOprLim(3701),   ioBeg(3701),       ioEnd(3701),
     1 numrg,           ideplete,          iexchang,       
     1 iexPoint(3701),  OprLossC(3701,20), ropdes(3701,20),
     1 internT(3701,20)   

c
c _________________________________________________________
     
      common/oprc/
     1 ciopdeX(20,3701), ciopsoX(20,3701), 
     1 ciopsoX2(3701),   ciopsoX5(3701),
     1 creuseX(3701),    cdivTyp(3701),   ciopdeX2(3701)
     
      character
     1 ciopdeX*12,   ciopsoX*12,   ciopsoX2*12,   ciopsoX5*12,
     1 creuseX*12,   cdivTyp*12,   ciopdeX2*12
     
c
c
c _________________________________________________________

      common/datx/
     1 dat1(40),       dat2(40),       dat1t(40),       dat2t(40),
     1 data1(13,40),   data2(13,40),   datad(32,40),
     1 cname(6)
c
c
c _________________________________________________________

      common/right/
     1 rfrnk (    241),rrsnk (     351),rdvnk (   6500),
     1 rpwnk (     25),ropnk (    3701),
     1 reprnk(     25),reprnkx

       real*8
     1 rfrnk,          rrsnk,          rdvnk,
     1 rpwnk,          ropnk,
     1 reprnk,         reprnkx
c
c
c _________________________________________________________

      common/account/                                                   
     1 accr(39,251), demcon(12,2500), ritremx(351), nrown(351),
     1 n2fill(351), n2own(251) 
c
c
c _________________________________________________________

      common/basef/
     1 cnode(440,2500), coeff(12,2500),
     1 nbase(2500),  nbaset,
     1 cnodm(440,2500), mbase(2500)
c
      common/basefc/
     1 cgagen(2500),  cgagex(440,2500),
     1 cupper(440,2500)
c
      character*12 cgagen, cgagex, cupper
c
      common/bases/
     1 qhisto(12,2500),  rtnacu(12,2500), divacu(12,2500),
     1 resvol(12,251),   idtype(   2500), ioutx  
c
c
c _________________________________________________________
c rrb 04/29/97; Daily model
      COMMON/daycom/
     1 iday, idaydem,    imd, idy,   maxdld, numdld, ido,     imstr,
     1 idays(2500),      idayd(1530),     idaydw(402),
     1 idayi(241),       idayr(251),      idaye(251), 
     1 virind(32,2500),  diverd(32,1530), diverdw(32,402),
     1 flowrd(32,241),   targetd(32,251),  
     1 riverm(2500),     availm(2500),    avinpm(2500), returm(2500),
c 
     1 returd(3660,2500),dlyratd(3660,525),

     1 divreqm(1530),    divreqwm(13110),
     1 flowrqm(241),     qdivm(39,2500),  qdivrm(1002),
     1 qresm(39,251),    accrm(39,251),   volintm(251), evapm(251),
     1 sepactM(251), 
     1 ritremm(351),     tarmaxm(251),    divom(3701),
     1 rlossm(1530),     rlosswm(8500),   
     1 rlossw2m(8500),  divmonwm(402),
     1 rdepwm(13110),
     1 dcum(1530),       dcutm(1530),     dcuwm(402),   dcutwm(402),
     1 gw2rivm(2500),    gw2gwm(2500), 
     1 qdivsm(1002),     qdivsom(1002),   qdivswm(402), qdivswom(402), 
     1 diverird(32,1530),diwrd(32,1530),
     1 diveridw(32,402), diwrdw(32,402),  carryWD(32,402),carryWM(402),
     1 divertd(32,1530), diveritd(32,1530)
c    1 evapd(32,251),    
c
c
c _________________________________________________________
c               Daily data read
      COMMON/daycomc/    
     1 virindx(32,2500), diverdx(32,1530),diverdxw(32,402),
     1 flowrdx(32,241),  targex2(32,251), evapdx(32,251),
     1 diverd2(32,1530),  

     1 crunidx(2500),    cdividx(1530),   cdividxw(402), cifridx(241), 
     1 crunidy(2500),    cdividy(1530),   cdividyw(13110),cifridy(241),
     1 cresidx(251),
     1 cresidy(251)
c
c
c _________________________________________________________
c rrb 00/11/10; Data file for SJRIP
      common/sjripc/
     1 isjrip,           isjpertb,        sjmina,       sjrela, 
     1 sjpertb,          sjtarget(12,251)
c
c
c _________________________________________________________
c rrb 00/12/04; Data for variable efficiency (see also annual TS file)
      common/effmaxc/
     1 ieffmax,          
     1 diwr(12,1530),    diwrw(12,402),  
     1 diwrT(1530),      diwrwT(402),
     1 diwrreq(1530),    diwrreqw(402),
     1 dcu(1530),        dcuw(402),
     1 dcut(1530),       dcutw(402)
c
c
c _________________________________________________________
c rrb 06/08/29; Data for new CU approach
      common/NewCU/
     1 effC(1530),    effF(1530),   effS(1530),
     1 AreaSF(1530),  AreaSS(1530), AreaGF(1530), AreaGS(1530),
     1 effCw(402),    effFw(402),   effSw(402),
     1 AreaGFw(402),  AreaGSw(402),
     1 dIwrSF(1530),  dIwrSS(1530), dIwrGF(1530),  dIwrGS(1530),
     1 dIwrGFw(402),  dIwrGSw(402),
     1 cuactSF(1530), cuactSS(1530),cuactGF(1530), cuactGS(1530),     
     1 cuactGFw(402), cuactGSw(402)
     
     
c
c _________________________________________________________
c rrb 00/12/20; Data for annual time series file
      common/tsfilec/
     1 itsfile,         isprink,
     1 areawa(402),     areasp(402),      igwmode(402),
     1 divcapwa(402),
     1 effmax(1530),    effmaxw(402),     effmaxs(402),
     1 effmaxt(1530),   effmaxd(1530),
     1 itspd(1530),     itspw(402)
c
c
c _________________________________________________________
c rrb 01/01/07; Data for Sprinklers (see also annual TS file)               
c rrb 2007/10/02; Remove Demspr (put isprink above)
c     common/sprinkc/
c    1 isprink,         demspr(402)
c    1 isprink
c
c
c _________________________________________________________
c rrb 01/01/07; Data for Soil Accounting
      common/soilc/
     1 isoil, soild,
     1 awcr(1530), awcr1(1530),soils(1530), qdivs(1530), qdivso(1530),
     1 awcrw(402), awcrw1(402),soilsw(402), qdivsw(402), qdivswo(402)
c
c
c _________________________________________________________
c
c rrb 04/08/25; Data for Downstream call
      common/dcallc/
     1 idcall, dcallm(13), dcalld(32), dcall1     

      real*8 dcallm, dcalld, dcall1
     
    
c
c
c _________________________________________________________
c rrb 01/03/12; Data for daily base flow
      common/vir1/
     1 qhistox(2500),    virinpx(2500), rtnacux(2500), divacux(2500), 
     1 dumy(2500),       dumz(2500),    dumR(2500),    dumU(2500),
     1 diverx(1530),     diwrx(1530),
     1 diverwx(402),     diwrwx(402),
     1 resvolx(251),     resvol1(251),
     1 qdivsx(2500),     qdivsox(2500), qcux(2500), qloss(2500),
     1 qpump(2500),
     1 qhistod(32,2500), ibout(2500)  
c
c
c _________________________________________________________

c               Data for baseflow reporting
       common/vir2/
     1 tempq(2500),      tempd(2500),   tempr(2500), 
     1 tempe(2500),      temps(2500),   tempi(2500),
     1 flowx(2500),      delst(2500),
     1 tempw(2500),      depacu(2500),  temph(2500), tempno(2500),
     1 tempts(2500),     tempfs(2500),  tempcu(2500),templ(2500),
     1 tempp(2500),      tempRre(2500), tempDre(2500), tempUse(2500)
c
c
c _________________________________________________________

c               Data for daily baseflow reporting
        common/vir3/
     1 tempqm(2500),     tempim(2500),  tempdm(2500),
     1 temprm(2500),
     1 tempwm(2500),     tempem(2500),  tempsm(2500),
     1 temphm(2500),     tempnom(2500),
     1 temptsm(2500),    tempfsm(2500), tempcum(2500), templm(2500),
     1 temppm(2500),     tempRreM(2500),tempDreM(2500),tempUseM(2500)
c
c               Data for local subroutine use
       common/gen1/
     1 tot1x(2500),      tot2x(2500),   xzero(2500)
c
c               Data for multiple diversion owners (not users)
       common/DivOwnC/
     1 NumOwnD, MaxOwnD, NdOwn(1530), divnamO(1530), divOwnP(1530),
     1 divownQ(1530)
     
       character 
     1 divnamO*24
c
c
c _________________________________________________________
c rrb 2005/10/14; Character names     
       common/Name1/
     1 DivNam1(1530), DivNamW1(402), ResNam1(251), RunNam1(2500),
     1 StaNam1(2500), XfrNam1(241),  Pname1(720),  UseNam1(1530)
     
       character
     1 DivNam1*24,    DivNamW1*24,   ResNam1*24,   RunNam1*24, 
     1 StaNam1*24,    XfrNam1*24,    Pname1*24,    UseNam1*24
c
c
c _________________________________________________________
c
c		Plan Data (Augmentaton and Terms and Conditions)
       common/PlanCom/
     1 maxplan,           maxPlnO,          maxPlnU,       nplan, 
     1 NumPlan,           maxPlnT,          NumPlnT,
     1 PlnTypX(100),      Pon(720),         Pid(720),    
cx     1 Pname(6,720),      
     1 iPsta(720),        
     1 iPlnOprE(720,3701),  iplnOprS(720,3701), iplnOprR(720,3701),
     1 iplnOprU(720,3701),  iplnOprO(720,3701), iplnOprP(720,3701),
     1 Pobl(240,720),     Pdem(720),        PdemT(720), 
     1 PoblD(3660,720),   PdemM(720),       PdemTm(720),
     1 Pdrive(720),       PdriveM(720),
c
c		Reuse Plan Data     
     1 iPlnTyp(720),      ipnode(2500),     ipSource(720), 
     1 Psup(240,720),     Psuply(720),      PsuplyT(720), 
     1 PsupD(3660,720),   PsuplyM(720),     PsuplyTm(720),
     1 Preuse(3701),       PresueM(720),
     1 Peff(12,720),      iPrf(720),
     1 iPfail(720),       Pfail(720),       iPacct(720),
c
c		Plan Augmentation
c		Note 13110 is because well plans are tied to well rights
     1 iplanWon,          iplanw(13110),     PlntypC(720),          
     1 PlanW(720),        Psource(720), 
     1 PwellC(720),       PwellCM(720),      rdvnkwp(720),
c
c		Reservoir plan storage data
     1 Psto1(720),        Psto2(720),       Pevap(720),           
     1 Psto1M(720),       Psto2M(720),      PevapM(720),
c
c		Plan return flow and recharge plans
     1 maxrtnPP, numRtnPP,
     1 ipPln(720),       rlossP(720),       rlossPM(720), 
c
     1 nrtnPP(722),      pcttotPP(722),   
     1 pctlosPP(722),    irtndlPP(722),     irnordPP(5005),
     1 irnstaPP(722),    irnPlan(722),
c
c		Reservoir return flow and recharge plans
     1 maxrtnRP, numRtnRP,
     1 iplnR(720),       iplnD(720),
cx   1 ipRes(720),       rlossR(720),       rlossRM(720), 
     1                   rlossR(251),       rlossRM(251), 
     1 nrtnRP(180),      pcttotRP(180),   
     1 pctlosRP(180),    irtndlRP(180),     irnordRP(5005),
     1 irnstaRP(180),
     1 ipracct(251) 
c
c _________________________________________________________
c
c		Import Data (
       common/ImpCom/
     1 maxImprt,        nImport,
     1 PImportX(13,720),Pimport(720),    PImportM(720), PimportT(720), 
     1 PImportD(32,720)
      
     
       character Pid*12, plnTypC*25,     PlanW*12,      Psource*12,
     1 PlntypX*25   

c
c _________________________________________________________
c
c		Call Data 
       common/CallCom/
     1 ioutc, ioutcS, ioutcR, 
     1 istrtype(2500),     imcdL(2500), ccallR(2500)

       CHARACTER 
     1 crunidx*12,       cdividx*12,    cdividxw*12,   cifridx*12,
     1 crunidy*12,       cdividy*12,    cdividyw*12,   cifridy*12,
     1 cresidx*12,      
     1 cresidy*12     
c
c _________________________________________________________
c
c rrb 01/31/95; Code Addition water right names
      common/names/
     1 namei(241), namer(351), named(6500), namep(720), nameo(3701),
     1 cyr1,       cunitm,     cunitd,      cunitm2,   cunitd2,
     1 cunitdX

      character 
     1 namei*24,   namer*24,   named*24,    namep*24,  nameo*24,
     1 cyr1*5,     cunitm*5,   cunitd*5,    cunitm2*5, cunitd2*5,
     1 cunitdX*5
c
c _________________________________________________________
c
c rrb 01/31/95; Code Addition IDs
      common/cID/
     1 cstaid(   2500),  crunid(   2500),  cifrid(    241), 
     1 cresid(    251),  cdivid(   1530),
     1 cstadn(   2500),  cgoto,         
     1 corid (    3701),  crigid(   6500),  cntern(10),
     1 cevaid(    251),  cpreid(    251),
     1 cevar (    251),  cprer (    251),  ownnam(251),
     1 creswr(    351),  copid (     351), cretid(1530),
     1 idreq (   2500),  cirtndl(  2500),  cirtndlw(13110),
     1 cirtndl2( 13110),  cirtnid(    525),
     1 cisfwr(    241)
c
      character *12
     1 cstaid,         crunid,         cifrid,        
     1 cresid,         cdivid,
     1 cstadn,         cgoto,
     1 corid,          crigid,         cntern,
     1 cevaid,         cpreid,
     1 cevar,          cprer,          ownnam,
     1 creswr,         copid,          cretid,
     1 idreq,          cirtndl,        cirtndlw,
     1 cirtndl2,       cirtnid,        cisfwr
c
c _________________________________________________________
C
       common/filenum/
     1 ifileNum(80)     
     
       common/charx/
     1 filena,    filenc,    fpath1, vdate,  ver, ccall, xmonam(14),
     1 filetype(80), filename(80), fileSuf(80), rec256
     
       CHARACTER
     1 FILENA*256, filenc*256, fpath1*256, 
     1 ver*8,      vdate*10, ccall*12,  xmonam*4,   
     1 filetype*40, filename*256, fileSuf*5, rec256*256
c
c _________________________________________________________
c rrb 2005/12/03; Parameter types     
       common/ParmCom/
     1 paramD(40), paramR(40), paramW(40)
     
       character  
     1 paramD*24,  paramR*24,  paramW*24
    
c _________________________________________________________
c
c rrb 2006/12/22; Reach (zone) processing
      common/RchCom/
     1 maxrch,       maxyrs,       dum3d(150,200,40),
     1 nrchTo(150),  nRchEnd(150), 
     1 iRch(2500),   RchID(2500),  
     1 RchIDR(150),  RchNameR(150)
     
      character
     1 RchID*12,     RchidR*12, RchNameR*24
c _________________________________________________________
c
c rrb 2006/12/22; Stream Gage (*rig) data
      common/Gage/
     1 nGage,      crunidG(2500),  irustaG(2500), runnamG(2500)
     
      character
     1 crunidG*12, runnamG*24
 
