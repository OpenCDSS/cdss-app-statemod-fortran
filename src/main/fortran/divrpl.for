C     Last change:  RRB  28 May 2002    4:31 pm
c
c
      subroutine divrpl(iw,lr,ishort,irep,tranlim,dcrdivx,divdx,
     1                  divactx,divacty,ncallx)
c
c _________________________________________________________
c	Program Description
c
c     Divrpl; It allows a diversion by exchange with a reservoir
c               if (iousop(lr,4)>=0 offset diversion
c               if (iousop(lr,4)<0  offset depletion

c      Called by Execute Type 4
c      Called by Replace for operation of a general replacement
c        reservoir (Type 10) for a diversion by exchange
c _________________________________________________________
c	Update History
c
c rb 96/02/27; modified by Ross Bethel,
c               to consider replacment of either headgate diversion
c               lr (iousop(lr,4)>=0 or depletion (iousop(lr,4)<0
c               above a predetermined exchange point (in riginp) which
c               is the first downstream node the replacment reservoir
c               and exchange destination have in common.
c
c rrb 96/04/31; Add constraint for replacement limited by a bookover
c               tranlim=0; Standard operation
c                      >0; General Replacement Reservoir Rule call
c                          (Constrained by second source storage or 
c                          bookover water right)
c rrb 98/03/03; Added daily capability (see fac)
c
c rrb 98/06/26; lr, l2 clean up
c
c rrb 00/12/26; Revised call rtnsec to include ieff2 for variable
c               efficiency considerations
c
c rrb 01/02/01; For Transmountain (irturn()=4)
c               Add call rtnsec who calls return to calculate CU,
c               reduce IWR and loss (as appropriate)
c
c rrb 01/08/27; Modified 01/08/23 by R. Bennett to have option to only 
c               take reservoir water if their is a CIR (IWR)
c               See ireptyp
c
c rrb 02/03/15; Revised by R. Bennett to correct a problem when the
c               exchange reservoir is on the same stream as the
c               exchange structure (diversion or reservoir)
c
c rrb 02/04/02 and 02/05/28; Revised by R. Bennett to handle
c               depletion option when the depletion offset occurrs
c               downstream of the exchange point.
c               Approach is as follows (see step 8)
c               1. (Steps 1-7) Calculate exchange with full
c                  diversion requirement. May or may not require
c                  return flows
c               2. (Step 8a) Check the min available from the exchange
c                  point down
c               3. (Step 8b) If min avail > 0 then 
c               4. (Step 8c) Calculate CU (diversion * efficiency)
c                  Note this is necessary because a use may try and
c                  run an exchange for depletion only when there is
c                  lots of available flow in the system.
c               5. (Step 8d) Adjust reservoir release to be
c                  min (avail from exchange point down and CU)
c               6. (Step 8e) Check
c               7. (Step 8f) Adjust avail by res release adjustment
c rrb 02/05/29; Added several quick exits to improve performance
c               Added call to chekava to simplify & use code elsewhere
c               Added divacty to call statement. Note
c               divactx is actual release divacty is actual diversion
c               Miscellaneous clean up performed
c rrb 02/10/25; Allow monthly on/off switch
c
c rrb 2012/05/23; revise iopdesr(lr) to be the destinatoin type to
c                 be consistent with other opeating rules
c                 and set iopdes(3,lr) to be the water right limit
c _____________________________________________________________
c	Documentation
c         iw                    water right order
c         lr                    order of operating rule
c         ishort                shortage indicator 0=no, 1=yes
c         irep                  replace indicator 0=no, 1=yes
c         tranlim               transfer limit to amount in a secondary
c                               source storage or source right?
c
c         dcrdivx = dcrdiv(n)   water right
c         divdx = divd(n)       remaining water right amount
c
c         divact                actual diversion
c
c	        divactx               amount released and passed out of routine
c	        divacty               amount diverted and passed out of routine
c                               Note divactx and divacty are not equal 
c                                 if releasing for depletion
c
c       Other Key variables
c         icx   = 6             subroutine call #
c         ieff2 = 0             always use average efficiency
c               = 1             use max efficiency if ieffmax = 1
c         iopsou(1,lr) = nr     replacement (source) reservoir
c         iopsou(2,lr) = iown   replacement (source) reservoir account
c         iExPoint(lr) =        exchange node
c         iopsou(4,lr) =        Replacement type
c                               0=diversion -1=depletion
c         iopsou(5,lr) =        not used
c         iopsou(6,lr) =        Reservoir release type and efficiency
c                                0 = release to meet demand
c                               +n = release only if a CIR (IWR) exists
c                                   and limit release to not exceed
c                                   IWR/n,  
c                               Note nmax = min(nmax, effmax) to save 
c                                 iterating
c         ireltyp               Same as iopsou(6,lr)
c
c         iopdes(1,lr) = nd     destination diversion or reservoir
c         iopdes(2,lr) = iuse   destination user
c
c         iopdes(3,lr)          water right limit 0=no limit, +=water right pointer
c         iopdesr(lr)           Destination type (1=reservoir, 2=ISF, 3=diversion,..
c
c         irssta(nr) = ipcd     replacement reservoir river station
c         idvsta(nd) = idcd     diversion river station
c
c         ndnnod (idcd) = ndnd  number of nodes downstream of diversion
c         ndnnod(ipcd) = ndnp   number of nodes downstream of reservoir
c
c         divcap(nd)            capacity
c         divmon(nd)            demand supplied this time step (grows
c                                 with each diversion each iteration)
c         divreq(iuse)          demand remaining this time step
c                                 (reduces with each diversion each
c                                 iteration)
c
c         divalo                allowable demand after adjusting for
c                                 demand, capacity, amount already
c                                 diverted this time step, water right
c                                 limit, etc.
c
c         small                 a local limit used for convergence, etc.
c        

c         
c
c         iout                  0 no detailed printout
c                               n yes detailed printout for right (iw)
c                               note iw is easily obtained from a *.xwr
c                               output file
c         iouta                 0 no detailed printout for avail
c                               n yes detailed printout for avail
c
c         qres(8,ix)            Reservoir Storage to Trans Mountain Diversion
c         qres(9,ix)            Reservoir Storage to Diversion
c         qres(18,ix)           From River by Exchange
c         qres(21, ix           From storage for Exchange
c
c _________________________________________________________
c	Dimensions
      include 'common.inc'
      character cwhy*48, cdestyp*12, ccarry*3, cpuse*3,cSource*12, 
     1  cDest*12,  cRelTyp*12, cReplace*3, cDest1*12
c
c _____________________________________________________________
c               Step 1 - Initilize

c
c               Step 0; Initilize
c		ioutX = 0 no details
c		       1 details
c                      2 summary      
c	              99 summmary independent of ichk

cx      if(ichk.eq.4) write(nlog,*) ' Divrpl; Type 4 Processing ', 
cx     1  corid(lr)

      iout=0
      ioutiw=0
      ioutX=0
      
      if(ichk.eq.104) iout=2
      if(corid(lr).eq. ccall) ioutiw=iw
      
      if(iout.eq.2 .and. ioutiw.eq.iw) then
cx      ioutX=1
        if(ncallX.eq.0 .and. iday.eq.0) then
        write(nlog,102) corid(lr)
 102    format(/, 72('_'),/ '  DivRpL; ID = ', a12)
        
        endif
      endif
      
      cDest1='360765      '
      
c               Limit debug printout to not include avail
c     iouta=iout
      iouta=0
      
c
c ---------------------------------------------------------
c
c rrb 98/03/03; Daily capability
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif

      iw = iw
      ishort=0
            
      small=0.001
      smalln=-1.0*small
c
c ---------------------------------------------------------
c		Initilize reporting varaibles      
      cwhy='NA'
      cdestyp='NA'
      cSource='NA'
      cDest='NA'
      ccarry='No'
      cpuse='No'
c
c rrb 2006/11/20; Initilize to -1 and zero; critical for reoperation      
c		  e.g. if((divact+small).lt.divalo) ishort = 1
c     divact = 0.0      
c     divalo = -1.0/fac
      divact = -1.0/fac
      divalo = 0.0
      divactx=0.0
      divacty=0.0
      

c     if(iout.eq.99) write(nlog,301) 
c    1 iyr, mon, iwx, iw, iout, ichk99 

      iresw=0
      relact=0.0
      relact1=0.0
      repadj=0.0
c
c rrb 2007/01/04; Use maximum efficiency if ieffmax=1     
c     ieff2=0
      ieff2=1
      icx=6
c
c ---------------------------------------------------------
c
      rcu=0.0
c
c rrb 2006/11/25; Do not initilize dcrdivx, may be set in replace      
cr    dcrdivx=-1/fac
      avail0=-1.0
      avail1=-1.0/fac
      avail2=-1.0
      avail3=-1.0
      divreq1=-1/fac
      iwhy=0
      nr=0
      nd=0
c
c rrb 2012/05/23; revise iopdesr(lr) to be the destinatoin type to
c                 be consistent with other opeating rules
c                 and set iopdes(3,lr) to be teh water right limit      
cx    nrig=iopdesR(lr)
      nrig=iopdes(3,lr)
c
c ---------------------------------------------------------
c
c rrb 02/04/28; Initilize water right and diversion to date
c               if not in replacement mode
c               Note for a replacement reservoir dcrdivx and divdx
c               are set in replace.for and passed in via the common
c               block.
      if(irep.eq.0) then
        dcrdivx=0.0
        divdx=0.0
      endif
c
c ---------------------------------------------------------
c
      if(iopsou(4,lr).ge.0) then
        cRelTyp='Diversion'
      else  
        cRelTyp='Depletion'
      endif      
c
c ---------------------------------------------------------
c      
      if(irep.eq.0) then
        cReplace='No'
      else
        cReplace='Yes'
      endif  
c
c _________________________________________________________
c		Step X; On/Off Switch      
c
c rrb 06/01/18; Allow daily on/off switch
      if(imonsw(lr,mon).eq.0) then
        iwhy=1
        cwhy='Monthly switch Off'
        goto 300
      endif  
c
c ---------------------------------------------------------
c		For a daily model set demand for beginning of season
      if(iday.eq.1 .and. imonsw(lr,mon).gt.0) then
        if (idy.lt.imonsw(lr,mon)) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 300
        endif  
      endif  
c
c ---------------------------------------------------------
c		For a daily model set demand for end of season
      if(iday.eq.1 .and. imonsw(lr,mon).lt.0) then
        if (idy.gt.iabs(imonsw(lr,mon))) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 300
        endif  
      endif  
      
c
c _________________________________________________________
c
c               Step 1b - Set Source reservoir data
c
      NR  =IOPSOU(1,Lr)
c
c rrb 02/04/03; Efficiency update
c     IF(IRESSW(NR).EQ.0) goto 290
      IF(IRESSW(NR).EQ.0) then        
        iwhy=2
        cwhy='Reservoir is off'
        goto 300
      endif  

      IOWN=NOWNER(NR)+IOPSOU(2,Lr)-1
      IPCD=IRSSTA(NR)
      NDNP=NDNNOD(IPCD)       
      csource=cresid(nr)
c
c _________________________________________________________
c
c               Step 1c - Set destination data
      ND  =IOPDES(1,Lr)
      cdestyp='Diversion'
      cDest=cdivid(nd)
c     if(cdivid(nd).eq.cDest1) then
c       ioutiw=iw
c       write(nlog,*) ' Divrpl; cdivid(nd) ', cdivid(nd)
c     endif  
c
c rrb 02/04/03; Efficiency update
c     IF(IDIVSW(ND).EQ.0) goto 290
      IF(IDIVSW(ND).EQ.0) then
        iwhy=3
        cwhy='Diversion is off'
        goto 300
      endif  
      
      IUSE=NDUSER(ND)+IOPDES(2,Lr)-1
      IDCD=IDVSTA(ND)
      NDND=NDNNOD(IDCD)
      divreq1=divreq(iuse)
c
c rrb 2006/01/04; Correction to allow varaible efficiency      
      if(ieff2.eq.0) then
        effX=diveff(mon,iuse)
      else
        effX=effmax(iuse)
      endif
      
c
c _____________________________________________________________
c
c               Step 1d - Quick exit (cannot do an exchange if no
c                         water is available at the diversion
c                         point (idcd))
cr    iwhy=1
      avail1=avail(idcd)
      IF(AVAIL(IDCD).LE.small) then 
        iwhy=4
        cwhy='Avail at diversion (avail1) is zero'
        goto 300
      endif  
c
c _____________________________________________________________
c
c               Step 1e - Check entire Avail array
c                         coming into the routine
c rrb 05/20/96; Check Avail coming into the routine
      call chekava(4, maxsta, numsta, avail)
c
c _____________________________________________________________
c
c               Step 2 - Check Available Supply in the River (Avail)
c                        from diversion (idcd) down.
c                        Note this routine is typically called because
c                        avail is zero from the diversion down.
c
      CALL DNMFSO(maxsta, AVAIL ,IDNCOD,IdCD  ,NDNd  ,IMCD)

      avail0=avail(imcd)
c
c               Debug printout (Print 1)
      IF(AVAIL(IMCD).le.-.001 .or. iout.eq.1) then
        if(iouta.eq.iw) write(nlog,320) (avail(i)*fac, i=1,numsta)
        WRITE(nlog,310) IYR,MON,IW,NWRORD(1,IW),Lr,IUSE, idcd, imcd,
     1           DIVREQ(IUSE)*fac, DIVACT*fac, avail(imcd)*fac

        if(AVAIL(IMCD).le.-.001) goto 9999
      endif
c
c _____________________________________________________________
c
c               Step 3 - Determine Demand
c
c rrb 96/03/11; Insure divalo has been defined for reoperation
c grb 4-20-97 have any replacment constrained by its water right
c
c _____________________________________________________________
c
c               Step 3a. Standard (Not a replacement reservoir call)
       if(irep.eq.0)
     1  DIVALO=AMIN1(DIVREQ(IUSE),DIVCAP(ND)-DIVMON(ND))
        if(ioutx.eq.1) write(nlog,*) ' DivRpl_1', divalo*fac,
     1    DIVREQ(IUSE)*fac,DIVCAP(ND)*fac, DIVMON(ND)*fac
c
c               b. Water Right limited
c  grb 4-23-97 add logic to limit diversions by water right
c              if type 4 directed to water right
c              Note if called by replace; divdx and dicrdivx are
c              set there to insure the exchange is limited to the
c              water right.
c      write(nlog,*) '  Divrpl; lr, iopdes(3,lr), nrig,irep',
c    1   lr, iopdes(3,lr), nrig, irep
c
c rrb 2012/05/23; set iopdes(3,lr) to be the water right limit
cx     if(iopdesr(lr).gt.0) then
       if(nrig.gt.0) then
cx       nrig=iopdesR(lr)

         divdx   = divd(nrig)                                  
         dcrdivx = dcrdiv(nrig)
         divalo=amin1(divalo,dcrdivx-divdx)
         if(ioutx.eq.1) write(nlog,*) ' DivRpl_2', divalo*fac,
     1     dcrdivx*fac, divdx*fac , nrig, idvrsw(nrig)  
         
c
c rrb 2006/04/25; Additional output         
         if(dcrdivx.le.small) then
           iwhy=5
           cwhy='Remaining Water Right (dcrdivX) is zero'
           goto 300
         endif  
         
         if(ioutX.eq.1) then
           write (nlog,*) '  Divrpl; ',iopdes(3,lr),nrig,divdx,dcrdivx
         endif
       endif
c
c
c _____________________________________________________________
c
c               Step 3b. Replacement Reservoir call (irep=1)
c                  Note when irep=1 the water right limits
c                  (dcrdivx and divdx) are set in replace.for and
c		   passed in the common block. Therefor
c                  all replacement exchanges are limited by water right.
       if(irep.ne.0) then
c        write(nlog,*) (dcrdivx-divdx)*fac,
c    1                  divreq(iuse)*fac, (divcap(nd)-divmon(nd))*fac

         divalo=amin1(dcrdivx-divdx,divreq(iuse),divcap(nd)-divmon(nd))
        if(ioutx.eq.1) write(nlog,*) ' DivRpl_3', divalo*fac,
     1    (dcrdivx-divdx)*fac,divreq(iuse)*fac,
     1    (divcap(nd)-divmon(nd))*fac
        
         
       endif
c
c rrb 01/08/23; 
c               d. If release type code is on
c                  Limit release to occur only if an IWR exists
c
       ireltyp=amin0(iopsou(6,lr),ifix(effmax(nd)))
c      write(nlog,*) '  Divrpl; ireltyp', ireltyp
c
       divmax=0.0

       if(ireltyp.gt.0) then 
         if(diwrreq(iuse).le.small) then
c
c rrb 2012/05/31; Set divmax for detailed output
           divmax=0.0
           divalo=0.0
         else
           divmax = diwrreq(iuse)/(float(ireltyp)/100.0)
           divalo=amin1(divalo, divmax)
         endif
       endif

       divalo=amax1(0.0,divalo)
        if(ioutx.eq.1) write(nlog,*) ' DivRpl_4', divalo*fac,
     1    divmax*fac, diwrreq(iuse)*fac, ireltyp
       

c
c 3/11/96 rrb;
c               d. Check Printout for a diversion (Print 2)
        if(iout.eq.1) then
c
c rrb 2012/05/23; set iopdes(3,lr) to be the water right limit        
cx        write(nlog,311) iyr, mon, iopdesr(lr)
          write(nlog,311) iyr, mon, nrig          

          write(nlog,'(10x, 20f12.2)') 
     1      (dcrdivx-divdx)*fac,divreq(iuse)*fac, divcap(nd)-divmon(nd), 
     1      diwrreq(iuse)*fac, 
     1      float(iopsou(6,lr)), effmax(nd), float(ireltyp), 
     1      divmax*fac, divalo*fac
c
c         write(nlog,*) '  Divrpl; nd, iuse, cdivid',
c    1                  nd,iuse,cdivid(nd)
        endif                          
c
c _____________________________________________________________
c
c               Step 3b; Quick exit (no supply, no demand, or
c                        no diversion available)
cr    iwhy=1
      avail1=avail(idcd)
      IF(AVAIL(IDCD).LE.small) then
        iwhy=6
        cwhy='Available at diversion (avail1) is zero'
        goto 300
      endif  
      
      IF(DIVREQ(IUSE).LE.small) then
        iwhy=7
        cwhy='Demand (divreq1) is zero'
        goto 300
      endif  
      
      if(divalo.le.small) then
        iwhy=8
        cwhy='Demand or Capacity (divalo) are zero'
        goto 300
      endif  
c
c _____________________________________________________________
c
c               Step 4 - Determine Reservoir Supply
c
c               FIND ALLOWABLE REPLACEMENT FROM CURRENT OWNERSHIP
cr    iwhy=9
      RPLALO=AMIN1(CUROWN(IOWN),CURSTO(NR)-VOLMIN(NR))
      RPLALO=AMAX1(0.0,RPLALO)
c
c rrb 98/03/03; Daily capability
c     ALOCFS=RPLALO/MTHDAY(MON)/FACTOR
      ALOCFS=RPLALO/fac
      if(alocfs.le.small) then
        iwhy=9
        cwhy='Reservoir supply (alocfs) is zero'
        
c       write(nlog,*) '  Divrpl; alocfs, curown, cursto, volmin ',
c    1   alocfs*fac, curown(iown),cursto(nr), volmin(nr), iown,nr
        goto 300
      endif  
      
      
c
c rrb 04/31/96; Add constraint for replacement limited by a bookover
c               Occurrs for replacement reservoirs only)
      if(tranlim.gt.small) then
c
c rrb 98/03/03; Daily capability
c       tranlic = tranlim/(mthday(mon)*factor)
        tranlic = tranlim/fac
        alocfs1=alocfs
        alocfs=amin1(alocfs,tranlic)
        alocfs=amax1(0.0, alocfs)
        if(iout.eq.1) then
          write(nlog,340) tranlim, tranlic, alocfs1, alocfs
        endif
      endif         
c
c------ Constrain release to maximum reservoir release
      ALOCFS=AMIN1(FLOMAX(NR)-RIVER(IPCD),ALOCFS)
      
c     write(nlog,*) '  Divrpl; alocfs 399 flomax, river, ipcd = ', 
c    1  alocfs, flomax(nr), river(ipcd), ipcd     

      alocfs = amax1(0.0,alocfs)
c
c _____________________________________________________________
c
c               Step 4b - Quick exit (no supply available)
      if(alocfs.le.small) then
        iwhy=10
        cwhy='Available river flow (alocfs) is zero'
        goto 300
      endif  
c
c _____________________________________________________________
c
c               Step 5 - Set up return flow information
c
c     IF(IRTURN(IUSE).EQ.4) DIVALO=AMIN1(DIVALO,QSTERN(ND))
c
C------  FIND STARTING AND ENDING INDEX OF RETURN FLOW STATIONS
      IRI=NRTN(IUSE)
      IRE=NRTN(IUSE+1)-1
c
c _____________________________________________________________
c
c               Step 6 - Find minimum exchange potential
c                        in the river from the diversion
c                        node (idcd) to the Exchange point
c                        iExPoint(lr)
c
      IMCD=Idcd
      ISS=Idcd
c
c rrb 01/08/27; Preserve nd as the diversion id
c     DO 100 ND=1,NDNd
      DO 100 nx=1,NDNd
c
c rrb 02/03/15; Revised to fix problem when the replacement
c               reservoir is downstream of the exchange
c               structure (diversion or reservoir)
c               by moving the exchange node test above the
c               minimum flow test
c
c       IF(AVAIL(IMCD).GT.AVAIL(ISS)) IMCD=ISS
c       if (iss.eq.iExPoint(lr)) goto 110

        if (iss.eq.iExPoint(lr)) goto 110
        IF(AVAIL(IMCD).GT.AVAIL(ISS)) IMCD=ISS
  100 ISS=IDNCOD(ISS)
c
  110 pavail=amax1(0.0,avail(imcd))
      if(pavail.le.small) then
        iwhy=11
        cwhy='Available flow (Pavail) is zero'
      else
        iwhy=0
      endif  

      DIVACT=amin1(pavail,divalo,alocfs)
      divact=amax1(0.0,divact)
     
      divactx=divact

      if(iout.eq.1) then
        write(nlog,342) pavail*fac, divalo*fac, alocfs*fac, divact*fac
      endif
c
c _____________________________________________________________
c
c               Step 7a- Adjust River Type 1 (No returns)
c
c rrb 02/05/30; Simplify Logic
c     IF(IRI.LE.IRE.AND.IRTURN(IUSE).NE.4) GO TO 120
      IF(IRI.gt.IRE .and. IRTURN(IUSE).eq.4) then

        if(iout.eq.1) write(nlog,*) '  Divrpl; type 1 (not returns)'
c
c rrb 98/08/10; Convergence upgrades
c       IF(DIVACT.LE.0.00001) goto 290
        if(divact.le.small) goto 290

        AVAILR=AVAIL(IPCD)
c
c rrb 01/02/01; Calculate returns
c               Note rtnsec who calls return to calculate CU,
c               reduce IWR and loss (as appropriate)
        CALL RTNSEC(icx,DIVACT,lr,IUSE,Idcd,nd,ieff2) 
c
c               Take out diversion (divact)
        CALL TAKOUT(maxsta,AVAIL,RIVER,AVINP,QTRIBU,IDNCOD,
     1            DIVACT,NDND,IDCD)
c
c               Add in reservoir release (relact)
        relact=-DIVACT
        CALL TAKOUT(maxsta,AVAIL,RIVER,AVINP,QTRIBU,IDNCOD,
     1            relact,NDNP,IPCD)
        AVAIL(IPCD)=AVAILR
        IF(IRTURN(IUSE).EQ.4) QSTERN(ND)=QSTERN(ND)-DIVACT
        GO TO 220
      endif
c
c _____________________________________________________________
c
c                Step 7b - Adjust River Type 2 (Full Supply available)
c
c rrb 02/05/30; Simplify Logic
c 120 IF(PAVAIL.LT.DIVALO) GO TO 130
  120 IF(PAVAIL.ge.DIVALO) then

        if(iout.eq.1) then
          write(nlog,*) '  Divrpl; type 2 (full supply)',nd
          write(nlog,*) '  Divrpl; pavail, divalo',pavail*fac,
     1                     divalo*fac
          write(nlog,*) '  Divrpl; iopsou(4,lr) = ',iopsou(4,lr), 
     1                  '  where a negative offsets depletion only'
        endif
c

        AVAILR=AVAIL(IPCD)
        if(divact.le.small) goto 290
c
c ---------------------------------------------------------  
c               Take out diversion (divact)
        CALL TAKOUT(maxsta,AVAIL,RIVER,AVINP,QTRIBU,IDNCOD,
     1            DIVACT,NDND,IDCD)
c
c ---------------------------------------------------------  
c               Add in reservoir release (relact)
        relact=-DIVACT
        CALL TAKOUT(maxsta,AVAIL,RIVER,AVINP,QTRIBU,IDNCOD,
     1            relact,NDNP,IPCD)
        AVAIL(IPCD)=AVAILR
c
c ---------------------------------------------------------  
c               Add in return flows
        CALL RTNSEC(icx,DIVACT,lr,IUSE,Idcd,nd,ieff2)
        GO TO 220
      endif
c
c _____________________________________________________________
c
c                Step 7c - Adjust River Type 3 (Need returns to
c                          maximize Supply)
c
c rrb 2006/01/04; Correction to allow varaible efficiency      
c 130 FORET=1.0-DIVEFF(mon,IUSE)/100.
  130 FORET=1.0-effX/100.

      if(iout.eq.1) then
        write(nlog,*) '  Divrpl; type 3 (Need returns)'
        write(nlog,*) '  Divrpl; at 130, Yes return consideration'
        write(nlog,*) '  Divrpl; pavail, divalo',pavail*fac,divalo*fac
        write(nlog,*) '  Divrpl; effx, foret', effx, foret
      endif
c
c ---------------------------------------------------------  
c		c
c------ 7c.1 Set the temporary return flow array (avwret)
      DO IS=1,NUMSTA
        AVWRET(IS)=0.
      end do
c
c ---------------------------------------------------------  
c		
C------  7c.2 Set THE TEMP0RARY AVAILABLE FLOW (avtemp)
      ISS=IDCD
      DO IS=1,NDND
        AVTEMP(ISS)=AVAIL(ISS)
        ISS=IDNCOD(ISS)
      end do
c
c ---------------------------------------------------------  
c		
C------  7c.3 STEP THROUGH RETURN FLOWS FOR CURRENT DIVERSION.
      DO 170 IRT=IRI,IRE
        IRCD =IRNSTA(IRT)
        NDNR =NDNNOD(IrCD)
c
c rrb 05/28/98; allow return id to not be the array counter
        idly=irtndl(irt)
c
c
c ---------------------------------------------------------  
c		
c        7c.4 Set delay based on time step
c rrb 99/03/03; Daily model temporarily divide by days per month (imd)
        if(iday.eq.0) then
          FACDLY=DLYRAT(1,IDLY)
        else
          facdly=dlyratd(1,idly)
        endif

        RET=PCTTOT(IRT)*FACDLY/10000.

        if(iout.eq.1) then
          write(nlog,*) '  Divrpl; irt, pctot, ret',irt,pcttot(irt),ret
        endif
c
c ---------------------------------------------------------  
c		
c
C------  7c.5 STORE RETURN FLOW PERCENTAGES IN TEMPORARY ARRAY
      ISS=IRCD
      DO 160 NS=1,NDNR
        AVWRET(ISS)=AVWRET(ISS)+RET
  160 ISS=IDNCOD(ISS)
  170 CONTINUE

      if(iout.eq.1) then
        write(nlog,324) (avwret(iss)*100.0,iss=1,numsta)
      endif
c
c ---------------------------------------------------------  
c		
C------  7c.6 COMPUTE THE ALLOWABLE AMOUNT FOR THE CURRENT DIVERSION
      ISS=IdCD
      DO 190 NS=1,NDNd
c
c rrb 98/08/10; Convergence upgrades
        IF(ABS(AVWRET(ISS)*FORET-1.).LE.small) GO TO 180
c
c rrb 02/04/02; Revised depletion approach
c       dumx(iss) = (1.0-AVWRET(ISS)*FORET)
        a1=avwret(iss)
        a2=avtemp(iss)*fac
        AVWRET(ISS)=AVTEMP(ISS)/(1.0-AVWRET(ISS)*FORET)

        a3=avwret(iss)*fac
        if(iout.eq.1) then
          write(nlog,*) '  Divrpl; iss, avwret-0, avtemp, avwret-1',
     1                     iss,a1, a2, a3
        endif
c
c ---------------------------------------------------------  
c		
        GO TO 190
c
c rrb 02/11/12; Simplify detailed output
c 180   AVWRET(ISS)=1.0E10
  180   AVWRET(ISS)=1.0E10/fac
  190 ISS=IDNCOD(ISS)
      AVWRET(IdCD)=AVTEMP(IdCD)
c
c
c ---------------------------------------------------------  
c		
c
c------  FIND THE UPDATED DOWNSTREAM MIN FLOW NODE
c        within the exchange reach (iss .eq. iExPoint(lr))
c
c rrb 96/03/20
      imcd=idcd
      iss=idcd
c
c rrb 01/08/27; Preserve nd as the diversion id
c     DO 200 ND=1,NDNd
      DO 200 nx=1,NDNd
c
c rrb 02/03/29; Revised to fix problem when the replacement
c               reservoir is downstream of the exchange
c               structure (diversion or reservoir)
c               by moving the exchange node test above the
c               minimum flow test
c     IF(AVwret(IMCD).GT.avwret(ISS)) IMCD=ISS
c     if (iss.eq.iExPoint(lr)) goto 210
c
      if (iss.eq.iExPoint(lr)) goto 210
      IF(AVwret(IMCD).GT.avwret(ISS)) IMCD=ISS
  200 ISS=IDNCOD(ISS)

  210 PAVAIL=AVWRET(IMCD)
c     write(nlog,*) '  Divrpl; imcd nr 210 = ', imcd

      DIVACT=amin1(pavail,divalo,alocfs)
      divact = amax1(0.0,divact)

      if(iout.eq.1) then
        write(nlog,342) pavail*fac, divalo*fac, alocfs*fac, divact*fac
      endif

      AVAILR=AVAIL(IPCD)
c
c ---------------------------------------------------------  
c		      
c
c grb 97/10/17; Efficiency reconmendation 
      if(divact.le.small) goto 290
c
c
c ---------------------------------------------------------  
c		
c               Take out diversion (divact)
      AVAILR=AVAIL(IPCD)
      CALL TAKOUT(maxsta,AVAIL,RIVER,AVINP,QTRIBU,IDNCOD,
     1            DIVACT,NDND,IDCD)
c
c
c ---------------------------------------------------------  
c		
c               Add in reservoir release (relact)
      relact=-DIVACT
c
      CALL TAKOUT(maxsta,AVAIL,RIVER,AVINP,QTRIBU,IDNCOD,
     1            relact,NDNP,IPCD)   
     
      AVAIL(IPCD)=AVAILR
c
c ---------------------------------------------------------  
c               Calcuate returns, CU, etc
      CALL RTNSEC(icx,DIVACT,lr,IUSE,Idcd,nd,ieff2)
c
c
c _____________________________________________________________
c rrb 02/04/02
c               Step 8 - Revise for depletion adjustment
c               1. (Steps 1-7 above) Calculate exchange with full
c                  diversion requirement. May or may not require
c                  return flows 
c               2. (Step 8a) Check the min available from the exchange
c                  point down
c               3. (Step 8b) If min avail > 0 then 
c               4. (Step 8c) Calculate CU (diversion * efficiency)
c                  Note this is necessary because a use may try and
c                  run an exchange for depletion only when there is
c                  lots of available flow in the system.
c               5. (Step 8d) Adjust reservoir release to be
c                  min (avail from exchange point down and CU)
c               6. (Step 8e) Check
c               7. (Step 8f) Adjust avail by res release adjustment
c
c rrb 02/04/02; Revised depletion option (handle at the end)
  220 continue
      if(divact.gt.small .and. iopsou(4,lr).lt.0) then
c
c ---------------------------------------------------------  
c               Step 8a - Find minimum avail flow from exchange
c                         point down
        idce=iExPoint(lr)
        ndne=ndnnod(idce)
        CALL DNMFSO(maxsta, avail, idncod, idce, ndne, imcd)
c
c ---------------------------------------------------------  
c               Step 8b - Check if the available flow will allow
c                         a depletion adjustment
        if(avail(imcd).gt.small) then 
          avail1=avail(imcd)
c
c ---------------------------------------------------------  
c               Step 8c - Calculate CU

c
c rrb 2006/01/04; Correction to allow varaible efficiency      
c          rcu=diveff(mon,iuse)/100.*divact
           rcu=effX/100.*divact
c
c ---------------------------------------------------------  
c               Step 8d - Adjust reservoir release (relact)
c                         which is a negative value
           relact1=relact
           relact2=relact+avail1

           relact=amin1(relact2,-1.0*rcu)   
           repadj=relact-relact1
c
c ---------------------------------------------------------  
c               Step 8e - Check reservoir release should be negative
          if(relact.gt.small) then
            write(nlog,350) corid(lr), nameo(lr)
            relact=amin1(relact, 0.0)
          endif
c
c ---------------------------------------------------------  
c               Step 8f - Take out reservoir adjustment
          availr=avail(ipcd)
          CALL TAKOUT(maxsta,AVAIL,RIVER,AVINP,QTRIBU,IDNCOD,
     1            repadj,NDNp,Ipcd)
          avail(ipcd)=availr  
        endif
      endif  
c
c _____________________________________________________________
c
c               Step 9a - Check for a negative avail value
c                         from diversion (idcd) down
c
      CALL DNMFSO(maxsta, AVAIL ,IDNCOD,IdCD  ,NDNd  ,IMCD)
      avail2=avail(imcd)
c
c ---------------------------------------------------------  
c               Step 9b - Check for a negative avail value
c                         from reservoir (ipcd) down

      CALL DNMFSO(maxsta, AVAIL ,IDNCOD,IpCD  ,NDNp  ,IMCD)
      avail3=avail(imcd)
c
c ---------------------------------------------------------  
c               Step 9c - Print if a problem
c
c     IF(AVAIL(IMCD).le.-.001) then
      IF(avail2.le.smalln .or. avail3.le.smalln) then

c
        WRITE(nlog,315) cDest, corid(lr), nameo(lr),
     1    IYR,MON,IW,NWRORD(1,IW),lr,IUSE,DIVREQ(IUSE)*fac,
     1    IdCD,IpCD,IMCD,iopsou(4,lr), DIVACT*fac,
     1    avail0, avail2*fac, avail3*fac

c       write(nlog,320) (avail(iss)*fac,iss=1,numsta)
c       write(nlog,322) (avtemp(iss)*fac,iss=1,numsta)
c       write(nlog,324) (avwret(iss)*fac,iss=1,numsta)
c       write(nlog,330) (river(iss)*fac,iss=1,numsta)
        AVAIL(IMCD)=0.
        goto 9999
      endif
c
c
c ---------------------------------------------------------  
c               Step 9d - Print avail and depletion adjustment data
c                         avail0= into routine from div
c                         avail1= without a depletion adjustment fr Exc
c                         avail2= with a depletion adjustment from div
c                         avail3= with a depletion adjustment from res
cx    iout=iw   
      if(iout.eq.1) then
        write(nlog,312) iyr, mon, imcd,
     1     divact*fac, rcu*fac,
     1     relact1*fac, repadj*fac, relact*fac

        write(nlog,313) iyr, mon, imcd,
     1     avail0*fac, avail1*fac, avail2*fac, avail3*fac
      endif
cx    iout=0            
c
c _____________________________________________________________
c
c               Step 10 Update 
c
c		Step 10a Update Diversion data
  230 USEMON(IUSE)=USEMON(IUSE)+DIVACT
      DIVREQ(IUSE)=DIVREQ(IUSE)-DIVACT
      DIVMON(ND  )=DIVMON(ND  )+DIVACT
      
      
c
c rrb 2009/056/01; Correction update amount diverted by this decree
c
c rrb 2012/05/23; set iopdes(3,lr) to be the water right limit        
cx    if(irep.eq.0 .and. iopdesr(lr).gt.0) then
      if(irep.eq.0 .and. nrig.gt.0) then     
        divdx=divd(nrig) 
c
c rrb 2012/05/31; revise divd(nrig) is the amount diverted in previous time step
cx      divd(nrig) = amax1(0.0, divd(nrig) - divact)
        divd(nrig) = amax1(0.0, divd(nrig) + divact)
c        
        if(ioutx.eq.1) write(nlog,*) ' DivRpl_5', irep, nrig,
     1   divdx*fac, divact*fac, divd(nrig)*fac        
      endif

      
      IF(IDRGST(ND).NE.0) GO TO 250
      IF (IRTURN(IUSE).EQ.4) GO TO 240
      QDIV(7,IDCD)=QDIV(7,IDCD)+divact
      GO TO 260

C-----
  240 QDIV(10,IDCD)=QDIV(10,IDCD)+DIVACT
      GO TO 260
c
c
c ---------------------------------------------------------  
c               Step 10b; Update reservoir data
c               
  250 IR=IRSORD(1,IDCD)
c
c 		Set Qres(18,ir) From River by Exchange
      QRES(18,IR)=QRES(18,IR)-relact*fac
c
c ---------------------------------------------------------  
c		Reservoir ACCOUNTS
  260 continue
      qdiv(21,idcd) = qdiv(21,idcd) + divact
c
c rrb 98/03/03; Daily capability
c     accr(21,iown) = accr(21,iown) - relact*mthday(mon)*factor
c     qres(21,nr)   = qres(21,nr)   - relact*mthday(mon)*factor
c     ACTACF=relact*MTHDAY(MON)*FACTOR
      accr(21,iown) = accr(21,iown) - relact*fac
      qres(21,nr)   = qres(21,nr)   - relact*fac
      
      ACTACF=relact*fac
      CURSTO(NR  )=CURSTO(NR  )+ACTACF
      REPLAC(NR  )=REPLAC(NR  )-relact
      CUROWN(IOWN)=CUROWN(IOWN)+ACTACF
c
c ---------------------------------------------------------  
c               Step 10c; Update reservoir to in basin use data
c
c rrb 02/05/29; Simplify logic
      IF (IRTURN(IUSE).ne.4) then
        QRES(8,NR)=QRES(8,NR)-ACTACF
        accr(8,iown)=accr(8,iown)-actacf
      else
c
c ---------------------------------------------------------  
c               Step 10d - Update Reservoir to Transmountain
c                          (irturn(iuse)=4)
c rrb 02/05/27; Simplify logic
        QRES(9,NR)=QRES(9,NR)-ACTACF
      endif

  290 continue
c
c
c ---------------------------------------------------------  
c               Step 10e - Update data to be passed out
c                          of this routine. Note
c                          divact .NE. relact if relese
c                          for depletion only
c
c _____________________________________________________________
c               
c               Step 11 - Check Entire Avail array
c rrb 05/20/96; Check Avail going out of the routine
      call chekava(4, maxsta, numsta, avail)
c
c _____________________________________________________________
c
c               Step 12 - Convergence Checks
c rrb 02/04/02; Check is not OK if off setting a depletion only
c     if(divactx+relact .gt. small) then
      if(iopsou(4,lr).gt.0 .and. (divactx+relact).gt.small) then
        write(nlog,*)
     1    '  Divrpl Warning; Diversion .ne. Reservoir release ',
     1    divactx*fac, -relact*fac, (divactx+relact)*fac
        goto 9999
      endif
c
c _____________________________________________________________
c
c               Step 13 - Check reservoir for roundoff
c rrb 99/05/10; 
      call chekres(nlog,maxres,1, 4, iyr, mon, nr,nowner,
     1                    curown,cursto,cresid)
c
c _____________________________________________________________
c
c                Step 14 - Final printout befor exit
  300 continue
c
c rrb 2006/05/02 Convergence 
      if(divact.le.small) then
        divact=0.0
        relact=0.0
      endif
c
c ---------------------------------------------------------  
c	`	Set values to pass out of routine    
      divactx=-relact
      divacty=divact
c
c ---------------------------------------------------------  
c		Update diverison by an operating rule
      divo(lr)=divo(lr)-relact
c
c ---------------------------------------------------------  
c		Set shortage
c rrb 2006/11/20; Correction for reoperation code  
      if((divact+small).lt.divalo) ishort = 1
c
c
c _____________________________________________________________
c		Detailed Output
      if(iout.eq.99 .and. divact.lt.small) iout=98
c     if(iout.eq.99 .and. cDest.ne.cDest1) iout=98
cx    if((iout.eq.2 .and. iw.eq.ioutiw) .or. iout.ge.99) then 
      
      iprint=1
      if(iday.eq.1 .and. divact.lt.small) iprint=0      
      if((iout.eq.2 .and. iw.eq.ioutiw .and. iprint.eq.1) 
     1  .or. iout.ge.99) then 
c
c ---------------------------------------------------------  
c		Header         
        ncallX=ncallX+1
        if(ncallX.eq.1) then
          write(nlog,270) corid(lr), cdestyp, ccarry, cpuse,
     1      cRelTyp, Creplace,     
     1      ipcd, idcd, iexpoint(lr)                  
        endif  
c
c ---------------------------------------------------------  
c		Values        
        write(99,280) 
     1      ' DivRpl     ',iyrmo(mon),xmonam(mon), idy, 
     1      cSource, cDest, iwx, lr, nr, nd, nrig,
     1      (divcap(nd)-divmon(nd))*fac, dcrdivx*fac,
     1      divreq1*fac, diwrreq(iuse)*fac,divalo*fac, 
     1      float(iopsou(6,lr)), effmax(nd), float(ireltyp), 
     1      divmax*fac, avail1*fac, pavail*fac,alocfs*fac, 
     1      divmon(nd)*fac, divact*fac, iwhy, cwhy
 280   format(a12, i5, 1x, a4, i5, 1x,a12,1x,a12,1x, 
     1   5i5,14f12.2, i5, 1x, a48)
       
       endif
c
c _____________________________________________________________
c
c                Step 15 - Return
c      
      return
c
c _____________________________________________________________
c
c               Formats
c
c _____________________________________________________________
c
c               Formats
 270    format(/       
     1  ' DivRpl (Type 4); Operation Right ID = ', a12,
     1  ' Destination Type = ', a12,
     1  ' Carrier (Y/N) = ',a3, ' Reuse Plan (Y/N) = ', a3,
     1  ' Release Type = ', a12,' Called by Replace = ', a3,/
     1  ' Source Location ', i5, ' Destination Location ', i5,
     1  ' Exchange Location ',i5,/
     1    ' DivRpl       Iyr Imo   Day Source ID    Dest ID      ',
     1    ' Iter   LR   NR   ND Nrig',
     1    '    Capacity      Decree',
     1    '     Divreq1         IWR      DivAlo',
     1    '     Eff_opr     Eff_max    Eff_used  Dem f(iwr)',      
     1    '      Avail1',
     1    '      Pavail      Alocfs      Divmon      DIVACT',
     1    ' iwhy Comment',/
     1    '____________ ____ ____ ____ ____________ ____________ ',
     1    ' ____ ____ ____ ____ ____'
     1    ' ___________ ___________ ___________ ___________',          
     1    ' ___________',
     1    ' ___________ ___________ ___________ ___________',
     1    ' ___________ ___________',
     1    ' ___________ ___________ ___________ ___________',
     1    ' ____ ', 24('_')) 


  301   format(/,60('_'),/,
     1   '  Divrpl;  iyr  mon iteration ', 3i5,/
     1   '  Divrpl;   iw iout    ichk99 ', 3i5)


  310   format(/, '  Divrpl; Print 1',/
     1    10x, '  Iyr  Mon   Iw nwrd   Lr Iuse Idcd Imcd',
     1         '    Divreq    Divact     Avail',/
     1    10x, 8(' ____'),  3(' _________'),/ 
     1    10x, 8I5,20f10.2)

  311   format(/,
     1    '  Divrpl; Print 2 ', 2i5, ' Note the Water Right (decree)',
     1    ' limit = ', i5, ' where  0 = no limit and +n = yes limit',/
     1    '          ',
     1    '      Decree      Demand    Capacity         IWR',
     1    '     Eff_opr     Eff_max    Eff_used  Dem f(iwr)',      
     1    '      Divalo',/
     1    '          ', 
     1    ' ___________ ___________ ___________ ___________',          
     1    ' ___________ ___________ ___________ ___________',
     1    ' ___________') 

  312   format(/,'   Divrpl; Print 3 Depletion Data ',/
     1    10x, '  Iyr  Mon Imcd',
     1         '    Divact        CU   relact1',
     1         '    Repadj    relact',/
     1    10x, 3(' ____'), 5(' _________'),/ 
     1    10x, 3I5,20F10.2)

  313   format(/,'   Divrpl; Print 4 Avail Data',/
     1    10x, '  Iyr  Mon Imcd',
     1         '    Avail0    Avail1    Avail2    Avail3',/
     1    10x, 3(' ____'), 4(' _________'),/ 
     1    10x, 3I5,20F10.2)

  315   FORMAT(/, 
     1    '  Divrpl; Print 5 Problem negative avail2 or avail3',
     1    ' Destination = ', a12, ' Operation right ', a12, 1x, a24,//,
     1    '       Iyr       mon        iw     nwrord       lr',
     1    '      iuse    divreq      idcd      ipcd      imcd',
     1    ' iopsou(4,)   divact    avail0    avail2    avail3',/
     1    ' _________ _________ _________ _________ _________',
     1    ' _________ _________ _________ _________ _________',
     1    ' _________ _________ _________ _________ _________',/
     1       6I10,F10.2,4I10,20F10.2)
     
  320   format(/,('  Divrpl: avail  ',10f10.2))
  322   format(/,('  Divrpl: availx ',10f10.2))
  324   format(/,('  Divrpl: avwret ',10f10.2))
  330   format(/,('  Divrpl: river  ',10f10.2))

  340    format(
     1     '  Divrpl; Rep limit;',
     1     '  tranlim, tranlic,  alocfs1,    alocfs',/
     1     '                    ', 20f8.2)
  342    format(
     1     '  Divrpl; Diversion Limit;',
     1     '  pavail  divalo  alocfs  divact',/
     1     '                          ', 20f8.2)

  350    format(
     1    '  Divrpl; Warning operation right',/
     1    10x, a12, 1x, a24,/
     1    10x, 'is running an exchange with ',/
     1    10x, 'a depletion option on when lots of available flow ',/
     1    10x, 'exists.  This might occur if the diversion does ',/
     1    10x, 'not have a water right in which case running to ',/
     1    10x, 'offset a depletion probabaly does not make sense ',/
     1    10x, 'To do; turn off the depletion option')

  360   format(' Divrpl; @ end; iwhy= ', i5,
     1    ' avail0 = ',f10.2
     1    ' alocfs = ',f10.2
     1    ' avail(idcd) = ',f10.2
     1    ' divacty = ', f10.2
     1    ' divactx = ', f10.2
     1    ' delta   = ', f10.2
     1    ' CU      = ', f10.2
     1    ' ishort  = ', i5)


 9999  continue
          write(nlog,270) corid(lr), cdestyp, ccarry, cpuse,
     1      cRelTyp, Creplace,     
     1      ipcd, idcd, iexpoint(lr)                  
c
c ---------------------------------------------------------  
c		Values        
        write(99,280) 
     1      ' DivRpl     ',iyrmo(mon),xmonam(mon), idy, 
     1      cSource, cDest, iwx, lr, nr, nd, nrig,
     1      (divcap(nd)-divmon(nd))*fac, dcrdivx*fac,
     1      divreq1*fac, diwrreq(iuse)*fac,divalo*fac, 
     1      float(iopsou(6,lr)), effmax(nd), float(ireltyp), 
     1      divmax*fac, avail1*fac, pavail*fac,alocfs*fac, 
     1      divmon(nd)*fac, divact*fac, iwhy, cwhy
 
 
      write(6,1050) 
      write(nlog,1051) 
      
 1050 format('    Stopped in Divrpl',/,
     1       '    See the *.log file')
 1051 format('    Stopped in Divrpl')
      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)


      stop 
      END




