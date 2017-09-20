C
C
C               Revised common block for linked model
c
c jhb 2014/11/01; Revise dimensions
c   ISF rights			241-2241
c                       because the IfrRigSP routine uses the op rule counter
c                       as an index in the divi() array.  might have to go larger later. 
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
c rrb 04/10/15; change maxuse from 2278 to 5000
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
     1 gwstor(5000),    gw2riv(5000),    gw2gw(5000),   gwmaxr(5000), 
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
cx     2 STANAM( 6,5000),XFRNAM( 6, 2241),RESNAM( 6,  251),
cx     3 DIVNAM( 6,3060),USENAM( 6,3060),
cx     4 RUNNAM( 6,5000),
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
     1 IDNCOD(   5000),NDNNOD(   5000),IUPCOD(   5000),
     2 NUPNOD(     61),IRUSTA(   5000),IRUTYP(   5000),
c
c rrb 2009/05/26; Correction     
cx   3 ISTRTN(   5000),IRNORD(   5000),NWRORD(2,14991),
     3 ISTRTN(   5005),IRNORD(   5000),NWRORD(2,14991),
     4 QSTERN(   3060),QTRIBU(   5000),ISTARU(   5000),
     5 IRUDND(   5000),TotSup(5000)
c
c
c _________________________________________________________

c rrb 97/10/22; Redimension for 25 replacement reservoirs
      common/repx/
     1 irepn,          irepk(25),       irepnr(25), 
     1 irepown(25),     ireprnk(25), 
     1 irepexp(25,3060),ireptyp(3060),
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
     1 VIRINP(12,5000),RIVER (   5000),AVINP (   5000),
     1 AVAIL (   5000),DIVREQ(   3060),IRSORD( 2,5000),
     1 ineg  (   5000), qneg (   5000),
     1 virinpT(12,5000),
     1 divsw(3060)

C
c
c _________________________________________________________
c                   Unit Response


      COMMON/SY3/
c
c rrb 2009/05/27; Revise      
cx   1                 DLYRAT(240, 525),RETUR (240,5000,2),
     1                 DLYRAT(240, 525),RETUR (240,5000),
     1 QDIV  (39,5000),QRES  (39,  251),CURRTN(   5000),
     1 irtnid(    525),
     1 dlytot(525)
c
c
c _________________________________________________________
      
      COMMON/SY4/
     1 DUMMY (5000, 13), dummyd(500),     dumx(5000),
     1 dum(13,5000),     dum2(13,5000),   dum3(13,5000),
     1 dumsta(5000),     dumown( 251,2),  dumrig(351,2),
     1 dumusr(5000),     dumd(32,5000),
     1 idum(5000),       idum2(5000),     iwarn(5000),
     1 avtemp(5000),     avwret(5000)
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
     1 DCRIFR(2241),    IIFRSW(     2241), iifrsw1(2241), 
     1 divi(2241),      ndnifs(     2241), ndnifb(    2241),
     1 IFRSTA(    2241),IIFRCO(     2241), FLOWR (12, 2241),
     1 flowrX(12,2241), 
     1 FLOWRQ(    2241),IFRRSW(     2241), ifrst2(    2241),
     1 florqr(1002),   divir(2241,1002),      qdivr(1002),
     1 ireach, 
     1 iifcom(2241),    demconi(2241)
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
     1 idvRre(5000),
     1 RrechM(13,251),   RrechD(32,251), Rrech(251)
c
c
c _________________________________________________________

      COMMON/RS2/
     1 IRSSTA(     251),EVAP  (     251),VOLINT(     251),
     1                                   CURSTO(     251),
     1 PROJTF(     251),SPILL (     251),CARRY (     251),
     1 OWNMON(    251),                  IOWNER(    251),
     1 REPLAC(     251),QMAINS( 2, 251)
C
c
c _________________________________________________________

      COMMON/RS3/
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
     1 DIVCAP(   3060),AREA  (   3060),NDUSER(   3060),
     1 IDVCOM(   3060),NRIGUS(   3060),IDUSER(   3060),
c
c rrb 2009/05/27; Iopvir is not used     
cx   1 IDVSTA(   3060),IPOVIR(   2228),DIVMON(   3060),
     1 IDVSTA(   3060),DIVMON(   3060),
     1 USEMON(   3060),IDIVSW(   3060),demsrc(   3060),
     1 rloss(3060), idivco2(3060),     iuseTyp(3060),
c
c                    Diversions to Recharge (carried) and Use
     1 idvDre(5000),
     1 DrechM(13,3060),  DrechD(32,3060),   Drech(3060),
     1 DuseM(13,3060),   DuseD(32,3060),    Duse(3060)
     
C
c
c _________________________________________________________

      COMMON/DV2/
     1 NRTN  (   3060),IRNSTA(   3920),IRTNDL(   3920),
     1 IRTURN(   3060),PCTTOT(   3920),pctlos(   3920),
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
     1 DIVER (12,3060), divert(12,3060), diverir(12,3060),
     1 diverirT(12,3060),demcond(3060)

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
     1 forecast(13,2241), qcarry(5),        qdebt(5),
     1 qdebtx(5),
     1 rgspill(2241),    iSetOpr(3701),     oprLoss(3701),
     1 oprLimit(3701),
     1 iOprLim(3701),   ioBeg(3701),       ioEnd(3701),
     1 numrg,           ideplete,          iexchang,       
     1 iexPoint(3701),  OprLossC(3701,20), ropdes(3701,20),
c
c rrb 2015-07-08; Add reoperation control by water right
cx   1 internT(3701,20)   
     1 internT(3701,20), icallOP(3701)
c
c _________________________________________________________
c  
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
     1 rfrnk (    2241),rrsnk (     351),rdvnk (   6500),
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
     1 accr(39,251), demcon(12,5000), ritremx(351), nrown(351),
     1 n2fill(351), n2own(251) 
c
c
c _________________________________________________________

      common/basef/
     1 cnode(440,5000), coeff(12,5000),
     1 nbase(5000),  nbaset,
     1 cnodm(440,5000), mbase(5000)
c
      common/basefc/
     1 cgagen(5000),  cgagex(440,5000),
     1 cupper(440,5000)
c
      character*12 cgagen, cgagex, cupper
c
      common/bases/
     1 qhisto(12,5000),  rtnacu(12,5000), divacu(12,5000),
     1 resvol(12,251),   idtype(   5000), ioutx  
c
c
c _________________________________________________________
c rrb 04/29/97; Daily model
      COMMON/daycom/
     1 iday, idaydem,    imd, idy,   maxdld, numdld, ido,     imstr,
     1 idays(5000),      idayd(3060),     idaydw(402),
     1 idayi(2241),       idayr(251),      idaye(251), 
     1 virind(32,5000),  diverd(32,3060), diverdw(32,402),
     1 flowrd(32,2241),   targetd(32,251),  
     1 riverm(5000),     availm(5000),    avinpm(5000), returm(5000),
c 
     1 returd(3660,5000),dlyratd(3660,525),

     1 divreqm(3060),    divreqwm(13110),
     1 flowrqm(2241),     qdivm(39,5000),  qdivrm(1002),
     1 qresm(39,251),    accrm(39,251),   volintm(251), evapm(251),
     1 sepactM(251), 
     1 ritremm(351),     tarmaxm(251),    divom(3701),
     1 rlossm(3060),     rlosswm(8500),   
     1 rlossw2m(8500),  divmonwm(402),
     1 rdepwm(13110),
     1 dcum(3060),       dcutm(3060),     dcuwm(402),   dcutwm(402),
     1 gw2rivm(5000),    gw2gwm(5000), 
     1 qdivsm(1002),     qdivsom(1002),   qdivswm(402), qdivswom(402), 
     1 diverird(32,3060),diwrd(32,3060),
     1 diveridw(32,402), diwrdw(32,402),  carryWD(32,402),carryWM(402),
     1 divertd(32,3060), diveritd(32,3060)
c    1 evapd(32,251),    
c
c
c _________________________________________________________
c               Daily data read
      COMMON/daycomc/    
     1 virindx(32,5000), diverdx(32,3060),diverdxw(32,402),
     1 flowrdx(32,2241),  targex2(32,251), evapdx(32,251),
     1 diverd2(32,3060),  

     1 crunidx(5000),    cdividx(3060),   cdividxw(402), cifridx(2241), 
     1 crunidy(5000),    cdividy(3060),   cdividyw(13110),cifridy(2241),
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
     1 diwr(12,3060),    diwrw(12,402),  
     1 diwrT(3060),      diwrwT(402),
     1 diwrreq(3060),    diwrreqw(402),
     1 dcu(3060),        dcuw(402),
     1 dcut(3060),       dcutw(402)
c
c
c _________________________________________________________
c rrb 06/08/29; Data for new CU approach
      common/NewCU/
     1 effC(3060),    effF(3060),   effS(3060),
     1 AreaSF(3060),  AreaSS(3060), AreaGF(3060), AreaGS(3060),
     1 effCw(402),    effFw(402),   effSw(402),
     1 AreaGFw(402),  AreaGSw(402),
     1 dIwrSF(3060),  dIwrSS(3060), dIwrGF(3060),  dIwrGS(3060),
     1 dIwrGFw(402),  dIwrGSw(402),
     1 cuactSF(3060), cuactSS(3060),cuactGF(3060), cuactGS(3060),     
     1 cuactGFw(402), cuactGSw(402)
     
     
c
c _________________________________________________________
c rrb 00/12/20; Data for annual time series file
      common/tsfilec/
     1 itsfile,         isprink,
     1 areawa(402),     areasp(402),      igwmode(402),
     1 divcapwa(402),
     1 effmax(3060),    effmaxw(402),     effmaxs(402),
     1 effmaxt(3060),   effmaxd(3060),
     1 itspd(3060),     itspw(402)
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
     1 awcr(3060), awcr1(3060),soils(3060), qdivs(3060), qdivso(3060),
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
     1 qhistox(5000),    virinpx(5000), rtnacux(5000), divacux(5000), 
     1 dumy(5000),       dumz(5000),    dumR(5000),    dumU(5000),
     1 diverx(3060),     diwrx(3060),
     1 diverwx(402),     diwrwx(402),
     1 resvolx(251),     resvol1(251),
     1 qdivsx(5000),     qdivsox(5000), qcux(5000), qloss(5000),
     1 qpump(5000),
     1 qhistod(32,5000), ibout(5000)  
c
c
c _________________________________________________________

c               Data for baseflow reporting
       common/vir2/
     1 tempq(5000),      tempd(5000),   tempr(5000), 
     1 tempe(5000),      temps(5000),   tempi(5000),
     1 flowx(5000),      delst(5000),
     1 tempw(5000),      depacu(5000),  temph(5000), tempno(5000),
     1 tempts(5000),     tempfs(5000),  tempcu(5000),templ(5000),
     1 tempp(5000),      tempRre(5000), tempDre(5000), tempUse(5000)
c
c
c _________________________________________________________

c               Data for daily baseflow reporting
        common/vir3/
     1 tempqm(5000),     tempim(5000),  tempdm(5000),
     1 temprm(5000),
     1 tempwm(5000),     tempem(5000),  tempsm(5000),
     1 temphm(5000),     tempnom(5000),
     1 temptsm(5000),    tempfsm(5000), tempcum(5000), templm(5000),
     1 temppm(5000),     tempRreM(5000),tempDreM(5000),tempUseM(5000)
c
c               Data for local subroutine use
       common/gen1/
     1 tot1x(5000),      tot2x(5000),   xzero(5000)
c
c               Data for multiple diversion owners (not users)
       common/DivOwnC/
     1 NumOwnD, MaxOwnD, NdOwn(3060), divnamO(3060), divOwnP(3060),
     1 divownQ(3060)
     
       character 
     1 divnamO*24
c
c
c _________________________________________________________
c rrb 2005/10/14; Character names     
       common/Name1/
     1 DivNam1(3060), DivNamW1(402), ResNam1(251), RunNam1(5000),
     1 StaNam1(5000), XfrNam1(2241),  Pname1(1440),  UseNam1(3060)
     
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
     1 PlnTypX(100),      Pon(1440),         Pid(1440),    
cx     1 Pname(6,1440),      
     1 iPsta(1440),        
     1 iPlnOprE(1440,3701),  iplnOprS(1440,3701), iplnOprR(1440,3701),
     1 iplnOprU(1440,3701),  iplnOprO(1440,3701), iplnOprP(1440,3701),
     1 Pobl(240,1440),     Pdem(1440),        PdemT(1440), 
     1 PoblD(3660,1440),   PdemM(1440),       PdemTm(1440),
     1 Pdrive(1440),       PdriveM(1440),
c
c		Reuse Plan Data     
     1 iPlnTyp(1440),      ipnode(5000),     ipSource(1440), 
     1 Psup(240,1440),     Psuply(1440),      PsuplyT(1440), 
     1 PsupD(3660,1440),   PsuplyM(1440),     PsuplyTm(1440),
     1 Preuse(3701),       PresueM(1440),
     1 Peff(12,1440),      iPrf(1440),
     1 iPfail(1440),       Pfail(1440),       iPacct(1440),
c
c		Plan Augmentation
c		Note 13110 is because well plans are tied to well rights
     1 iplanWon,          iplanw(13110),     PlntypC(1440),          
     1 PlanW(1440),        Psource(1440), 
     1 PwellC(1440),       PwellCM(1440),      rdvnkwp(1440),
c
c		Reservoir plan storage data
     1 Psto1(1440),        Psto2(1440),       Pevap(1440),           
     1 Psto1M(1440),       Psto2M(1440),      PevapM(1440),
c
c		Plan return flow and recharge plans
     1 maxrtnPP, numRtnPP,
     1 ipPln(1440),       rlossP(1440),       rlossPM(1440), 
c
     1 nrtnPP(1444),      pcttotPP(1444),   
     1 pctlosPP(1444),    irtndlPP(1444),     irnordPP(5005),
     1 irnstaPP(1444),    irnPlan(1444),
c
c		Reservoir return flow and recharge plans
     1 maxrtnRP, numRtnRP,
     1 iplnR(1440),       iplnD(1440),
cx   1 ipRes(1440),       rlossR(1440),       rlossRM(1440), 
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
     1 PImportX(13,1440),Pimport(1440),PImportM(1440),PimportT(1440), 
     1 PImportD(32,1440)
      
     
       character Pid*12, plnTypC*25,     PlanW*12,      Psource*12,
     1 PlntypX*25   

c
c _________________________________________________________
c
c		Call Data 
       common/CallCom/
     1 ioutc, ioutcS, ioutcR, 
     1 istrtype(5000),     imcdL(5000), ccallR(5000)

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
     1 namei(2241), namer(351), named(6500), namep(1440), nameo(3701),
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
     1 cstaid(   5000),  crunid(   5000),  cifrid(    2241), 
     1 cresid(    251),  cdivid(   3060),
     1 cstadn(   5000),  cgoto,         
     1 corid (    3701),  crigid(   6500),  cntern(10),
     1 cevaid(    251),  cpreid(    251),
     1 cevar (    251),  cprer (    251),  ownnam(251),
     1 creswr(    351),  copid (     351), cretid(3060),
     1 idreq (   5000),  cirtndl(  5000),  cirtndlw(13110),
     1 cirtndl2( 13110),  cirtnid(    525),
     1 cisfwr(    2241)
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
     1 iRch(5000),   RchID(5000),  
     1 RchIDR(150),  RchNameR(150)
     
      character
     1 RchID*12,     RchidR*12, RchNameR*24
c _________________________________________________________
c
c rrb 2006/12/22; Stream Gage (*rig) data
      common/Gage/
     1 nGage,      crunidG(5000),  irustaG(5000), runnamG(5000)
     
      character
     1 crunidG*12, runnamG*24
 
