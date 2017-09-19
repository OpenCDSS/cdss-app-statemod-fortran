C     Last change:  RRB  22 Oct 2002    4:12 pm
c
      subroutine divres(iw,lr,ishort,irep,tranlim,dcrdivx,divdx,
     1           divactx,divacty,ncallX)
c
c _________________________________________________________
c	Program Description
c
c       Divres; It handles diversions by a reseroivr where
c           Type 2: Reservoir to diversion, reservoir,
c                   or carrier by the river
c           Type 3; Reservoir to diversion, or reservoir
c                   by a carrier
c           Called by Execut and Replace
c _________________________________________________________
c
c               Update history
c
c	    2006/03/20 Revised to turn off variable efficiency
c		       and include soil moisture in water use
c		       depletion calculations
c		       Added more detailed output
c           96/02/27 by R. Bethel so that if you want depletion 
c             only release, code second source account as negative
c             on Type 2 water right
c           98/03/03 by R. Bennett to allow daily calculations
c           98/12/03 by R. Bennett for well impact to rtnsec
c           99/11/02 by R. Bennett to include steps in doc
c           00/12/26 by R. Bennett to accomodate variable
c             efficiency (see call rtnsec)
c           01/08/23 by R. Bennett to have option to only 
c             take reservoir water if their is a CIR (IWR)
c           02/03/01 by R. Bennett to test IWR = 2 that allows
c             reservoir releases to limit but not use variable
c             efficiency capability
c           02/10/19 by E. Wilson and R. Bennett to allow
c             release for depletion only when iopsou(6,lr) > 0
c           02/10/28 by R. Bennett clean up and document
c             EW & rrb edits on 2/10/19. Note confirmed control
c             should be iopsou(4,lr) not iopsou(6,lr)
c
c  ________________________________________________________
c	Documentation
c
c ---------------------------------------------------------
c	VARIABLES USED WHEN CALLED BY REPLACE (IREP>0)
c	    irep          = Called by Replace (0=no, >0=yes)
c           dcrdivx       = decree of destination diversion right
c           divdx         = diversion to date of diversion right
c
c ---------------------------------------------------------
c	VARAIABLES USED WHEN CALLED BY EXECUTE OR REPLACE 
c
c           divalo = demand
c           relalo = available supply in reservoir (acft)
c           alocfs = available supply in reservoir (cfs)
c           divact = actual amount diverted
c           relace = actual amount released from the reservoir
c
c	    divactx=amount released and passed out of routine
c	    divacty=amount diverted and passed out of routine
c                   Note divactx and divacty are not equal 
c                        if releasing for depletion
c
c           icx   = subroutine call #
c
c           ieff2 = 0 always use average efficiency
c           ieff2 = 1 use max efficiency if ieffmax=1
c           iopsou(1,lr) = source reservoir #1 
c           iopsou(2,lr) = source reserovir #1 account
c               Note the following source 2 data is only used when
c               releases are tied to another type 6 operating rule  
c           iopsou(3,lr) = if > 0 source reservoir #2    
c           iopsou(4,lr) = if > 0 source reservoir #2 account
c                          if = 0 release to meet diversion demand
c                          if < 0 release to meet depletion
c
c           iopsou(5,lr) = not used
c           iopsou(6,lr) = Reservoir release type and efficiency
c                          if = 0 release to meet demand
c                          if > 0 release only if a CIR (IWR) exists
c                                 and limit release to not exceed
c                                 IWR/n,  
c                          Note nmax = min(nmax, effmax) to save 
c                                 iterating
c           ireltyp        same as iopsou(6,lr)
c
c           iopdes(1,lr) = destination (+=diversion, -=reservoir)
c           iopdes(2,lr) = destination user (account)
c
c           iout         = 0 no detailed printout
c                          1 yes detailed printout
c
c           iown         = source reservoir #1 account
c           ipcd         = Source reservoir #1 river station
c
c           imonsw()     = monthly on off switch  
c           iowna        = source reservoir #2 account
c

c           irow         = destination user (account)
c
c           ires         = switch 0=diversion, 1=reservoir
c            
c           irep = 0; Individual Operating Rule call
c                      (Not constrained by water right)
c                = 1; General Replacement Reservoir Rule call
c                      (Constrained by remaining water right)
c
c           l2 - order of the destination diversion
c           lr - order of operating rule in opr. rule list. Note
c                when called by execute (ityopr=2 or 3); lr=l2
c                when called by replace (ityopr=10) lr is a array 
c                holder

c
c           nd           = destination (+=diversion, -=reservoir)  
c           ndnd         = # of downstream nodes for reservoir #1
c
c           nr           = source reservoir #1          
c           nra          = source reservoir #2 
c
c           tranlim=0; Standard operation
c                  >0; General Replacement Reservoir Rule call
c                      (Constrained by second source storage or 
c                       bookover water right)
c
c	    intern(  )   = If > 0 carrier system with intervening
c                          structures
c
c           qres(4,ix)   = From Carrier by Storage to Reservoir
c           qres(8,ix)   = Reservoir Storage to Trans Mountain Carrier
c           qres(9,ix)   = Reservoir Storage to Carrier??
c           qres(26,ix)  = Reservoir Storage to River
c           qres(11,ix)  = Reservoir Storage to Carrier
c	    qres(29,ix)  = Reservoir to Reservoir
c
c _________________________________________________________
c	Dimensions
      include 'common.inc'
c      
      character cwhy*48, cdestyp*12, ccarry*3, cpuse*3, 
     1 cRelTyp*12, cReplace*3, cidvri*12, cSour*12, cresid1*12,
     1 cDest*12, cDest1*12

c      
c _________________________________________________________
c               
c		Step 1; Detailed Output control and header
c		iout=0 No details
c		iout=1 Details
c		iout=2 Summary
c		iout=99 Summary independent of ichk
c     
      if(ichk.eq.94) write(nlog,*) ' DivRes; Type 2 or 3 Processing ',
     1   corid(lr)
     
      iout=0
      ioutiw=0
      
      cDest1='NA'
      cDest1='36_ADC019   '
      
      
      if(ichk.eq.202 .or. ichk.eq.203) iout=2
      if(corid(lr).eq. ccall) ioutiw=iw
      
      if(iout.eq.2) then
        write(nlog,102) corid(lr)
 102    format(/, 72('_'),/ '  DivRes; ID = ', a12)
        NR  =IOPSOU(1,Lr)
        write(nlog,*) '  DivRes; iout, ioutiw', iout, ioutiw, 
     1    cresid(nr), ccall
      endif             
      
      
      
c     if(iout.ge.1) write(nlog,*) '  Divres.for;'      
      if(ichk.eq.94) write(nlog,*) ' Divres; Entering'
c
c               b. Daily capability
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif
c
c
c ---------------------------------------------------------
c               c. Miscellaneous
      iwhy=0
      cwhy='NA'
      small=0.001
c
c rrb 2006/11/20; Initilize to -1 and zero; critical for reoperation      
c		  e.g. if((divact+small).lt.divalo) ishort = 1
c
c rrb 2006/11/20; Initilize to -1 and zero; critical for reoperation      
c		  e.g. if((divact+small).lt.divalo) ishort = 1
      divact = -1.0/fac
      divalo = 0.0      
      divactx=0.0
      divacty=0.0
      
      divact1=divact
      alocfs=0.0
      relact=0.0
      ishort = 0
      iw=iw
      iowna=0
c
c
c ---------------------------------------------------------
c rrb 00/12/26; Variable efficiency consideration
c     ieff2=1   
c
c rrb 2007/01/04; Use maximum efficiency if ieffmax=1     
c     ieff2=0
      ieff2=1
c
c rrb 01/01/17; Call number
      icx=4

c
c
c ---------------------------------------------------------
c		e. Detailed output data      
      diwrreq1=-1.0/fac
      divmax1=-1.0/fac
      cursto1=-1.0
      avail1=-1/fac
      avail2=-1/fac
      effX=-1.0
      qres291=-1.0
      cimcd1=-1.0     
      cimcd2=-1.0      
      
      cidvri=corid(lr)                  
      nd  =iopdes(1,lr)
      cDest='NA'
c
c ---------------------------------------------------------
      
      if(nd.ge.0) then     
        cdestyp='Diversion'
      else
        cdestyp='Reservoir'
      endif
      
      ccarry='No'      
      if (intern(lr,1).ne.0) ccarry='Yes'
c
c ---------------------------------------------------------
c      
      cpuse='No '      
      ipUse=ireuse(lr)
      if(ipUse.gt.0) cpuse='Yes'
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
      
      if(irep.eq.0) then
        cReplace='No'
      else
        cReplace='Yes'
      endif  
c
c ---------------------------------------------------------
c		Step 1b; Check avail array
       call chekava(2, maxsta, numsta, avail)
c
c _________________________________________________________
c               Step 2; Branch if not on this month
c
      if(ichk.eq.94) write(nlog,*) ' Divres; 1, lr', lr

      if(imonsw(lr,mon).eq.0) then
        iwhy=1
        cwhy='Monthly switch is off'
        goto 330
      endif  
c
c ---------------------------------------------------------
c		For a daily model set demand for beginning of season
      if(iday.eq.1 .and. imonsw(lr,mon).gt.0) then
        if (idy.lt.imonsw(lr,mon)) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 330
        endif  
      endif  
c
c ---------------------------------------------------------
c		For a daily model set demand for end of season
      if(iday.eq.1 .and. imonsw(lr,mon).lt.0) then
        if (idy.gt.iabs(imonsw(lr,mon))) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 330
        endif  
      endif  
      
c
c _________________________________________________________
c
c               Step 3; Set source data
c
c               a. Source reservoir #1 (nr), source account (iown),
c                  river station (ipcd), & # downstream (ndnp)
      NR  =IOPSOU(1,Lr)           
      cSour=cresid(nr)
      if(ichk.eq.94) write(nlog,*) ' Divres; 1, nr', nr, iopsou(2,lr)

      if(iressw(nr).eq.0) then
        iwhy=2
        cwhy='Source Reservoir is off'
        goto 330
      endif  

      IOWN=NOWNER(NR)+IOPSOU(2,Lr)-1
      
      if(ichk.eq.94) write(nlog,*) ' Divres; 1, iown', iown
c
c ---------------------------------------------------------
c rrb 01/05/15; Check reservoir roundoff when entering routine
c			Note in1=0 input from a routine
c			     isub1 = subroutine calling chekres
      in1=0
      isub1 = 2
c     write(nlog,*) ' Divres; Corid(lr) ', corid(lr)
      call chekres(nlog, maxres, in1, isub1, iyr, mon, nr,
     1  nowner, curown, cursto, cresid)

      IPCD=IRSSTA(NR)
      NDNP=NDNNOD(IPCD)
      if(ichk.eq.94) write(nlog,*) ' Divres; 1, ipcd, ndnp',ipcd, ndnp
      
c
c
c ---------------------------------------------------------
c               b. Source reservoir #2 (nra), account (iowna)
      NRA =IOPSOU(3,Lr)
      
      if(ichk.eq.94) write(nlog,*) ' Divres; 1, nra', nra
c
c rrb 2006/11/21; Simplify      
c     if(nra.eq.0) goto 100
        if(nra.ne.0) then
c      
        if(iressw(nra).eq.0) then
          iwhy=3
          cwhy='Source Reservoir 2 is off'      
          goto 330
        endif  
        IOWNA=NOWNER(NRA)+abs(IOPSOU(4,Lr))-1
      endif
c
c _________________________________________________________
c               
c               Step 4; Set destination data
c
c rrb 2006/11/21; Simplify      
c 100 nd  =iopdes(1,lr)
      nd  =iopdes(1,lr)
      if(ichk.eq.94) write(nlog,*) ' Divres; 2, nd', nd
  
c     write(nlog,*) '  Divers; lr, ityopr = ', ityopr(lr)    
c
c               a. Destination is a diversion (nd>0 & iresw=0)       
      iresw=0
c
c ---------------------------------------------------------
c               b. Destination is a reservoir (nd<0 & iresw=1)
      if (nd.lt.0) then
        iresw=1
        nd=-nd
c
        cDest=cresid(nd)

        if (iressw(nd).eq.0) then
          iwhy=4
          cwhy='Destination Reservoir is off'        
          goto 330
        endif  
c
c ---------------------------------------------------------
c rrb 2006/09/25; Allow multiple accounts - Initilize
cr      irow=nowner(nd)+iopdes(2,lr)-1

        nro=1
        if(iopdes(2,lr).lt.0) then
          nro=-iopdes(2,lr)
          irow=nowner(nd)
        endif

        if(iopdes(2,lr).gt.0) then
          nro=1
          irow=nowner(nd)+iopdes(2,lr)-1
        endif
        
        go to 120      
      endif
c
c
c ---------------------------------------------------------
c               c. Additional data when the destination is a diversion
c     write(nlog,*) ' Divres; 3'
      if(ichk.eq.94) write(nlog,*) ' Divres; 3'

      cDest=cdivid(nd)
      if(idivsw(nd).eq.0) then
        iwhy=5
        cwhy='Destination Diversion is off'
        goto 330
      endif  

      IUSE=NDUSER(ND)+IOPDES(2,Lr)-1
      divreq1=divreq(iuse)
c
c rrb 2006/01/04; Correction to allow varaible efficiency      
      if(ieff2.eq.0) then
        effX=diveff(mon,iuse)
      else
        effX=effmax(iuse)
      endif

      
c
c               d. Check
cx      WRITE(6,360)    lr,IDCD,NR,IPCD,IYR,IMO
cx      goto 9999
c
c
c ---------------------------------------------------------
c               e. Carrier system data (for types 2 & 3 only)
 120  if (ityopr(lr).ne.10) then
        if(intern(lr,1).eq.0) go to 130
        ndr=intern(lr,1)

        idcd=idvsta(ndr)  
        ndnd=ndnnod(idcd)
c       write(nlog,*) '  Divres; lr, idcd', lr, idcd
        go to 140
      endif
c
c
c ---------------------------------------------------------
c               f. Additional non carrier data
  130 if (iresw.eq.0) IDCD=IDVSTA(ND)
      if (iresw.eq.1) idcd=irssta(nd)
      NDND=NDNNOD(IDCD)
c
c
c ---------------------------------------------------------
c               Exit if not a reservior and no demand
  140 IF(iresw.eq.0.and.DIVREQ(IUSE).LE.small) then
        iwhy=6
        cwhy='Demand is zero'
        goto 330
      endif  
c
c _________________________________________________________
c
c            **  Step 5; Set demand (DIVALO) when the destination
c                        is a diversion
c        
      if(ichk.eq.94) write(nlog,*) ' Divres; 4'

      if(iresw.eq.0) then
c
c
c ---------------------------------------------------------
c               a. Demand for a standard diversion (not 
c                  called by replace)
        if(irep.eq.0) 
     1    DIVALO=AMIN1(DIVREQ(IUSE),DIVCAP(ND)-DIVMON(ND))
c
c
c ---------------------------------------------------------
c               b. Demand for a diversion called by Replace
c                  (irep=1). Limit diversion to the remaing 
c                  decree (dcrdivx-divdx)	
        if(irep.ne.0)  
     1    divalo=amin1(dcrdivx-divdx,divreq(iuse),
     1                divcap(nd)-divmon(nd))
c
c
c ---------------------------------------------------------
c               c. If release type code is on
c                  Limit release to occur only if an IWR exists
c                  Note still releasing to meet 100% of demand
c
        if(iout.eq.1) then
          write(nlog,*) ' '
          write(nlog,*) '  Divres; iopsou(6,lr)  ', iopsou(6,lr)
          write(nlog,*) '  Divres; effmax(nd)    ', effmax(nd)
          write(nlog,*) '  Divres; diwrreq(iuse) ', diwrreq(iuse)*fac
        endif

        ireltyp=amin0(iopsou(6,lr),ifix(effmax(nd)))
c
        divmax=0.0
        if(ireltyp.gt.0) then
          diwrreq1=diwrreq(iuse) 
          if(diwrreq(iuse).le.small) then
            divalo=0.0
          else
            divmax = diwrreq(iuse)/(float(ireltyp)/100.0)
            divalo=amin1(divalo, divmax)
          endif
        endif

        divalo=amax1(0.0,divalo)
        if(divmax.gt.small) divmax1=divmax
        
c
c
c ---------------------------------------------------------
c rrb 01/08/23; Exit if no demand
        if(divalo.le.small) then
          iwhy=7
          cwhy='Demand is zero'        
          goto 330
        endif  
      endif
c 
c
c _________________________________________________________
c
c            ** Step 6; Set demand (DIVALO) when the destination
c                       is a reservoir
c
c               a. Set allowable storage
      if(ichk.eq.94) write(nlog,*) ' Divres; 6'

      if (iresw.eq.1) then
c
c ---------------------------------------------------------
c rrb 2006/09/25; Allow multiple accounts - Demand
        cursa=0.0
        do n=1, nro
          n1=irow+n-1
          cursa=cursa+(ownmax(n1)-curown(n1))
        end do  
      
cr      divalo=amin1(ownmax(irow)-curown(irow),
        divalo=amin1(cursa,
     1    volmax(nd)-cursto(nd), tarmax(nd)-cursto(nd))/fac

        divalo=amax1(0.0, divalo)
c
c
c ---------------------------------------------------------
c               b. Check Prinout for reservoirs
        if(iout.eq.1) then
          write(nlog,*) '  Divres; nd, irow ',
     1              'ownmax(irow)-curown(irow), ',
     1              'volmax(nd)-cursto(nd),  ',
     1              'tarmax(nd)-cursto(nd), divalo*fac'
          write(nlog,*)  nd, irow,
     1               ownmax(irow)-curown(irow),
     1               volmax(nd)-cursto(nd),
     1               tarmax(nd)-cursto(nd), divalo
        endif
      endif
c
c _________________________________________________________
c
c           **  Step 7; Check for water available in reservoir
c                       RELALO (ac-ft) and ALOCFS (cfs)
c
c               a. Calculate volume in reservoir
        RELALO=AMIN1(CURSTO(NR)-VOLMIN(NR),CUROWN(IOWN))
        cursto1=relalo
        RELALO=AMAX1(0.0,RELALO)
c
c
c ---------------------------------------------------------
c               b. Exit if nothing available
c rrb 2006/04/10; Convergence refinement
cr      if(relalolis.1) then
        if(relalo.lt.small) then
          iwhy=8
          cwhy='Source reservoir storage = zero'
          goto 330
        endif  
c
c
c ---------------------------------------------------------
c               c. Change units and
c                  Limit to maximum river discharge (flowmax)
        ALOCFS=RELALO/fac
        ALOCFS=AMIN1(FLOMAX(NR)-RIVER(IPCD),ALOCFS,DIVALO)
        alocfs = amax1(0.0,alocfs)
c
c _________________________________________________________
c
c               Step 8; Limit release (ALOCFS) to capacity of
c                       intervening structures
c                       if required for type 2 & 3 only
      if(ityopr(lr).ne.10) then
        do 150 i61=1,10
c
          if (intern(lr,i61).eq.0) go to 160
          intvn=intern(lr,i61)

  150     alocfs=amin1(alocfs,(divcap(intvn)-divmon(intvn))) 
          alocfs=amax1(0.0,alocfs)
  160   continue
      endif
c
c
c _________________________________________________________
c
c               Step 9; Limit release (ALOCFS) to reservoir #2
c                       if reservoir #2 exists (iopsou(3,lr)>0)
c
      if(ichk.eq.94) write(nlog,*) ' Divres; 7'

      if(iopsou(3,lr).ne.0) then
        idow=nowner(iopsou(3,lr))+abs(iopsou(4,lr))-1
c
        divown1=curown(idow)/fac
        alocfs=amin1(alocfs,divown1)
        alocfs=amax1(0.0,alocfs)
      endif
c           
c
c _________________________________________________________
c
c               Step 10; Limit release (ALOCFS) to bookover (tranlim)
c                 (Occurrs for replacement reservoirs only)
      if(tranlim.gt.small) then
        tranlic = tranlim/fac
        alocfs1=alocfs
        alocfs=amin1(alocfs,tranlic)
        alocfs=amax1(0.0,alocfs)
        if(iout.eq.1) write(nlog,370) tranlim*fac, tranlic*fac, 
     1                alocfs1*fac, alocfs*fac
      endif
c
c _________________________________________________________
c
c           **  Step 11; Set diversion (DIVACT) and
c               release (RELACT) to be the minimum of
c               (water in reservoir (ALOCFS) and
c               demand (DIVALO))
      divact=amin1(alocfs,divalo)
      divact=amax1(0.0,divact)
      relact=-divact
c
c _________________________________________________________
c
c		Step 12; Depletion Option
c ew& rrb; 02/10/28; Begin updates for depletion option
c               Set diversion (DIVACT) to be the minimum of
c               (water in reservoir (ALOCFS) and
c               demand (DIVALO))
c               Set the reservoir release (RELACT) to be the max
c               (available flow (PAVAIL) - DIVACT) and
c               depletion (DIVACT * DIVEFF))
c
c     if(iopsou(6,lr) .lt. 0) then
      if(iopsou(4,lr) .lt. 0) then
c
c
c ---------------------------------------------------------
c                a. FIND THE MIN AVAIL FLOW DOWNSTREAM (avail(imcd))
        iss=IDcd
        NDNDS=NDND
        CALL DNMFSO(maxsta,avail,idncod,iSS,ndndS,imcd)

        pavail = amax1(0.0,avail(imcd))
        DIVACT=amin1(divalo,alocfs)
        divact=amax1(0.0,divact)
c
c
c ---------------------------------------------------------
c               b. If Available flow < demand
c                  set the release (RELACT) to the difference
c                  (DIVACT - PAVAIL) or the depletion (divact*diveff)
        if(pavail .lt. divact) then
          relact=divact-pavail
          relact=amax1(0.0, relact)       
c
c rrb 2006/01/04; Correction to allow varaible efficiency                
c         relact=amax1(relact,(divact*diveff(mon,iuse)/100.))
          relact=amax1(relact,(divact*effX/100.))
          relact=-relact
        else
c
c ---------------------------------------------------------
c               c. If available flow >= demand (pavail>=divact)
c                  set release to the depletion (divact*diveff)
c
c rrb 2006/01/04; Correction to allow varaible efficiency      
c         relact=-1.*(divact*diveff(mon,iuse)/100.)
          relact=-1.*(divact*effX/100.)
        endif
c
c
c ---------------------------------------------------------
c               d. If iout=1 print detailed results
        if(iout.eq.1) then
c
c rrb 2006/01/04; Correction to allow varaible efficiency              
c         c = divact*diveff(mon,iuse)/100.0
          c = divact*effX/100.0
          write(nlog,390) 2, divact*fac, pavail*fac, relact*fac, c*fac
        endif
      endif
c
c  ew & rrb; 02/10/25; End of updates for depletion
c
c
c _________________________________________________________
c
c               Step 13; Exit if no demand (divact <=0) or 
c
      if (divact.le.small) then
        iwhy=9
        cwhy='Demand is zero'     
        goto 330
      endif  
c _________________________________________________________
c
c               Step 14; Branch around return flow adjustments if
c
c                       1. ityopr(lr) = 3 to carrier
c                       2. releasing for diversion iopsou(4,lr>0) or
c                       3. the destination is a reservoir (iresw=1) or
c                       4. the destination is a transmtn with no
c                          returns (irturn()=4), or
c
c
      if(ityopr(lr).eq.3) GOTO 240
      if(iopsou(4,lr).ge.0.or.iresw.eq.1) goto 230
      if(irturn(iuse).eq.4) goto 230
c
c _________________________________________________________
c
c               Step 15; Adjust release (RELACT) based on return flows
c
c               a. FIND STARTING AND ENDING RETURN FLOW STATIONS
      IRI=NRTN(IUSE)
      IRE=NRTN(IUSE+1)-1
      if(iri.gt.ire) goto 230  
c
c
c ---------------------------------------------------------
c               b. DETERMINE RETURN FLOW PERCENTAGE OF DIVERSION
c
c rrb 2006/01/04; Correction to allow varaible efficiency      
c 170 FORET=1.0-DIVEFF(mon,IUSE)/100.
  170 FORET=1.0-effX/100.
c  
c
c ---------------------------------------------------------
c               c. FILL AVTEMP AND RF ARRAYS USED IN RETURN FLOW CHECK
      ISS=IDCD
      DO 180 IS=1,NDND
        AVTEMP(ISS)=avail(ISS)
  180 ISS=IDNCOD(ISS)

      do 190 is=1,numsta
  190 avwret(is)=0
c
c
c ---------------------------------------------------------
c                d. RETRIEVE LOCATIONS, DELAY PATTERN OF RETURN FLOW
      DO 210 IRT=IRI,IRE
        IRCD =IRNSTA(IRT)
        NDNR =NDNNOD(IrCD)
c
c rrb 05/28/98; allow return id to not be the array counter
c       IDL=IRTNDL(IRT)
c       IDLY=IDLORD(IDL)
        idly=irtndl(irt)
c
c
c ---------------------------------------------------------
c                e. CHECK DELAY TYPE
c rrb 99/03/03; Daily model temporarily divide by days per month (imd)
c       FACDLY=DLYRAT(1,IDLY)
        if(iday.eq.0) then
          FACDLY=DLYRAT(1,IDLY)
        else
          facdly=dlyratd(1,idly)
        endif
c
        RET=PCTTOT(IRT)*FACDLY/10000.
                 
c
c ---------------------------------------------------------
c                f. STORE RETURNS AS % OF DIVERSIONS AT RF LOCATIONS
        ISS=IrCD
          DO 200 NS=1,NDNr
          AVWRET(ISS)=avwret(iss)+(RET*foret)
  200   ISS=IDNCOD(ISS)
  210 CONTINUE
c
c
c ---------------------------------------------------------
c                g. COMPUTE THE AMOUNT FOR THE CURRENT DIVERSION
      ISS=IdCD
      DO 220 NS=1,NDNd
        avtemp(iss)=avtemp(iss)+(divact*avwret(iss))
  220 ISS=IDNCOD(ISS)
c
c
c ---------------------------------------------------------
c                h. FIND THE UPDATED DOWNSTREAM MIN FLOW NODE
      iss=IDcd
      NDNDS=NDND
      CALL DNMFSO(maxsta,AVTEMP,idncod,iSS,ndndS,imcd)
c
c
c ---------------------------------------------------------
c                i. Adjust release (RELACT) to be old release
c                   plus the lesser of return
c                   flows (RETOT) and water availablity
c                   W/RETURN FLOWS(AVTEMP)
      retot= avwret(imcd)*divact
      relact=relact+AMIN1(avtemp(imcd),retot)
c
c _________________________________________________________
c
c               Step 16; Store the available flow at the reservoir
c                       to (availr = avail(ipcd))
c                       Note get here from above (by including return
c                       flow adjustments) and from a goto statements
c                       that allow return flows to be ignored when
c                       1. the destination is a diversion with no
c                          returns (irturn()=4), or
c                       2. releaseing for diversion iopsou(4,lr>0) or
c                       3. the destination is a reservoir (iresw=1) or
c                       4. no return flow locations (iri>ire)
c
  230 continue    
      AVAILR=AVAIL(IPCD)
c
c _________________________________________________________
c
c               Step 17; Exit if no diversion (divact <=0) or 
c
      if(divact.le.small) then
        iwhy=10
        cwhy='Available flow is zero'      
        goto 330
      endif  
c
c
c _________________________________________________________
c
c               Step 18; Remove diversion (DIVACT) from stream

      AVAILR=AVAIL(IPCD)

      CALL TAKOUT(maxsta,AVAIL,RIVER,AVINP,QTRIBU,IDNCOD,
     1            DIVACT,NDND,IDCD)
c     write(nlog,*) '  Divres; idcd,divact,relact = ',idcd,divact,relact
c
c _________________________________________________________
c
c               Step 19; Add reservoir release (RELACT) to stream
c
      call takout(maxsta,avail,river,avinp,qtribu,idncod,
     1            relact,ndnp,ipcd)
c     write(nlog,*) '  Divres; ipcd,divact = ', idcd, divact, relact
c
c
c _________________________________________________________
c
c               Step 20; Reset flow at reservoir node to
c                        unadjusted value
      avail(ipcd)=availr
c
c _________________________________________________________
c
c               Step 21; Add return flows to stream
c
c rrb 98/12/30; Wells
c rrb 00/12/26; Variable efficiency consideration; Set ieffmax2=1
c               so that average efficiency is usedc               
c 240 if (iresw.eq.0) call rtnsec(icx,divact,lr,iuse,IDCD,nd)
  240 continue 
      if (iresw.eq.0) call rtnsec(icx,divact,lr,iuse,IDCD,nd,ieff2)
c
c _________________________________________________________
c
c               Step 22; Update storage, demand, etc.
c
c               a. Destination is a reservoir, update storage data
  250 if (iresw.eq.1) then
c
        divaf=divact*fac
        cursto(nd)=cursto(nd)+divaf
cr      curown(irow)=curown(irow)+divaf
c
c ---------------------------------------------------------
c rrb 2006/09/25; Allow multiple accounts - Allocate
c		  Note:
c		   iResT1=0 distributes based on ownership ratio
c		   nrown1=number of accounts in this reservoir
c		   iown = first account associated with this reservoir  
c		   icx  = subrouine calling accou.for       
c		   ia   = account to adjust
      
        nrX=nd
        iResT1=0
        nrown1=nro
        iownX=irow
        icx=102
        if(ityopr(lr).ne.10 .and. intern(lr,1).gt.0) then
          ia=4
        else
          ia=26
        endif  
        
        cresid1=cresid(nrX)
        call accou(maxacc, maxown, nrX, ownmon, curown, accr, ia, 
     1    ownmax, iownX, nrown1, cursa, divaf, iResT1, icx, cresid1)
        
c
c ---------------------------------------------------------
c
        if(ityopr(lr).ne.10 .and. intern(lr,1).gt.0) then
c
c                  qres(4,ix) =  From Carrier by Storage to Reservoir
          qres(4,nd)=qres(4,nd)+divaf
c
c rrb 2006/10/27; Reservoir to Reservoir for water balance calculations
          qres(29,nd)=qres(29,nd) + divaf          
          qres291=qres(29,nd)
        else           
c
c
c ---------------------------------------------------------
c               From Reservoir Storage to River
          qres(26,nd)=qres(26,nd)+divaf
        endif
        go to 260
      endif
c
c
c ---------------------------------------------------------
c               b. Destination is a diversion update demand data
      USEMON(IUSE)=USEMON(IUSE)+DIVACT
      DIVREQ(IUSE)=DIVREQ(IUSE)-DIVACT
      DIVMON(ND  )=DIVMON(ND  )+DIVACT

C-----  UPDATE STORAGE WATER USE OF DESTINATION DEMAND AT 
c       RIVER (QDIV(7))
C       WATER CARRIED ACCOUNT (QDIV(18) OF OTHER NODES, DESTINATION 
c       STORAGE WATER USE (QDIV(20)
c              idcd  = river ID
c              inode = destination ID 
c              iresw = 1 destination is a reservoir
c                    = 0 destination is a demand
c              Do not update qdiv(20 ) from carrier by storage
c              if no intervening structures
c
c              Update to carrier from storage (Qdiv(20,inode))
c
      itx=ityopr(lr)
      if(itx.eq.3 .or. (itx.eq.2 .and.intern(lr,1).ne.0)) then
        inode=idvsta(nd)
        qdiv(20,inode)=qdiv(20,inode)+divact               
      endif                                             
c
c
c ---------------------------------------------------------
c               d. Destination is a carrier
c                  Set qdiv(22 ) from carrier by storage type 3, 
c                  operating rule for water balance purposes
      if(ityopr(lr).eq.3) then
        qdiv(22,inode)=qdiv(22,inode)+divact               
      endif
c
c
c ---------------------------------------------------------
c               e. Destination is a transmntn diversion
c                  Branch for transmountain (irturn(iuse) = 4)
      IF (IRTURN(IUSE).EQ.4) GO TO 290

      if(ityopr(lr).ne.3) QDIV(7,IDCD)=QDIV(7,IDCD)+DIVACT
c
c
c ---------------------------------------------------------
c               f. For type 2 & 3 only
c                  UPDATE DIVERTED AND CARRIED AMOUNTS OF
c                  INTERVENING STRUCTURES
c                  Carrier passing throught a structure (Qdiv18,inode)
c 260 do 272 i11=1,10
  260 if(ityopr(lr).ne.10) then    
        do 272 i11=1,10
c
          if (intern(lr,i11).eq.0) go to 282
          intvn=intern(lr,i11)
     
          divmon(intvn)=divmon(intvn)+divact
          inode=idvsta(INTVN)
          qdiv(18,inode)=qdiv(18,inode)+divact
  272   continue
      endif
  282 continue
      GO TO 300
c
c
c ---------------------------------------------------------
c               g. Update From river by storage (Qidv(10,icdc)
  290 QDIV(10,IDCD)=QDIV(10,IDCD)+DIVACT
  300 continue      
c  
c
c ---------------------------------------------------------
c               h. Update Source is a reservoir
c		   destination is a carrier (qres(11,nr) &
c                  accr(11,iown)
      if(ityopr(lr).eq.3) then
        qres(11,nr)=qres(11,nr)-relact*fac
        accr(11,iown)=accr(11,iown)-relact*fac        
      endif
c
c
c ---------------------------------------------------------
c               i. Adjust source reservoir #1 data
  310 ACTACF=RELACT*fac
      CURSTO(NR  )=CURSTO(NR  )+ACTACF
      PROJTF(NR  )=PROJTF(NR  )-RELACT
      CUROWN(IOWN)=CUROWN(IOWN)+ACTACF
c
c
c ---------------------------------------------------------
c               j. Set Qmains
c rrb 01/12/26; Revise to operate under f95
c     if(iowna.ne.iown .and. nra.ne.0) 
c    1   qmains(2,iowna)=qmains(2,iowna)+actacf
      if(iowna.gt.0) then
        if(iowna.ne.iown .and. nra.ne.0) then
          qmains(2,iowna)=qmains(2,iowna)+actacf
        endif
      endif
c
c
c ---------------------------------------------------------
c               k. Set Source is a Reservoir
c		   From reservoir to carrier
c                  for transmountain (Qres(8,nr) and accr(8,iown))
      IF (iresw.eq.0.and.IRTURN(IUSE).EQ.4) GO TO 320
      QRES(8,NR)=QRES(8,NR)-ACTACF
      accr(8,iown) = accr(8,iown)-actacf
      goto 330
c
c
c ---------------------------------------------------------
c               l. Set From reservoir to carrier non transmountain
c                  (Qres(9,nr) and accr(9,iown))
  320 QRES(9,NR)= QRES(9,NR)-ACTACF
      accr(9,iown) = accr(9,iown)-actacf
c
c
c _________________________________________________________
c               
c               Step 23;  Set shortage switch (ishort)
c rrb 02/27/96; General replacement reservoir requirement
c rrb 98/08/10; Convergency upgrade
c 330 if(divact.lt.divalo) ishort = 1
  330 if((divact+small) .lt. divalo) ishort=1
c
c rrb 2006/05/02 Convergence 
      if(divact.le.small) then
        divact=0.0
        relact=0.0
      endif  
      
      divactx=-relact
      divacty=divact  
  
c
c _________________________________________________________
c               
c               Step 24; Set operating rule output (DIVO)
c               Note set to reservoir release (relact)
c               not diversion (divact) since may have released
c               for depletion
c
c rrb 04/18/97; Change reconmended by R. Bethel
c 342 divo(lr)=divo(lr)+divact

      divo(lr)=divo(lr)-relact
c
c _________________________________________________________
c               
c               Step 25; Check results
c
c               a. Check that Avail flow > 0
      call chekava(2, maxsta, numsta, avail)
c
c
c ---------------------------------------------------------
c               b. Check if diversion .ne. release if
c                  releasing for diversion (iopsu(4,lr)>0)
c ew
c     IF(iopsou(6,lr).gt.0) then
      IF(iopsou(4,lr).gt.0) then
        if(divact+relact .gt. small) then
          write(nlog,*) '  Divres Concern;',
     1                     divact, -relact, divact+relact
        endif
      endif

c
c ---------------------------------------------------------
c               c. Check reservoir roundoff when exiting routine
c			Note in1=1 output from a routine
c			     isub1 = subroutine calling chekres
      in1=1
      isub1 = 2
      call chekres(nlog, maxres, in1, isub1, iyr, mon, nr,
     1  nowner, curown,cursto,cresid)
c
c _________________________________________________________
c
c               Step 26; Return
c
c
c
c _________________________________________________________
c
c               Step 27; Detailed Output
c
c      write(Nlog,*) ' DivRes_2;', iout, ioutiw, iw

      if(iout.eq.99 .and. divact.lt.small) iout=98
      if((iout.eq.2 .and. iw.eq.ioutiw) .or. iout.ge.99) then      
cx    if(iout.eq.99 .and. cDest.eq.cDest1) then
         ncallX=ncallX+1       
         if(ncallX.eq.1 .or. iout.eq.99)then
           write(nlog,270) corid(lr),cdestyp,ccarry,cpuse,
     1       cRelTyp, Creplace
         endif  
         
         write(nlog,280) 2, iyrmo(mon),xmonam(mon),idy,cSour,cDest,
     1      iwx, lr, nr, nd, irep, ishort,
     1      divreq1*fac, (divcap(nd)-divmon(nd))*fac, cursto1,
     1      diwrreq1*fac, 
     1      float(iopsou(6,lr)), effX, effmax(nd), 
     1      divmax1*fac, pavail*fac, cimcd1, avail1*fac, 
     1      cimcd2, avail2*fac, alocfs*fac, 
     1      divmon(nd)*fac, abs(relact)*fac, qres291, 
     1      divact*fac,
     1      iwhy, cwhy
  280   format(2i6,2x,a4,i6,1x,a12,1x, a12,1x,5i6, i8,
     1 18f12.0, i5, 1x, a48)
       endif     
         

      RETURN

c _________________________________________________________
c               Formats
c
 141    format(       
     1    '  Divres (Type 2 or 3);',/
     1    '     #   Iyr   Imo  Iter  iwhy',
     1    '      Demand    Capacity         IWR',
     1    '     Eff_opr     Eff_max    Eff_used  Dem f(iwr)',      
     1    '      Pavail      Alocfs      Divact      Divmon',/
     1    ' _____ _____ _____ _____ _____', 
     1    ' ___________ ___________ ___________',          
     1    ' ___________ ___________ ___________ ___________',
     1    ' ___________ ___________ ___________ ___________') 
  142   format(5i6, 20f12.2)
  
  270   format(/, 
     1  '  DivRes (Type 2 &3); Operation Right ID = ', a12,
     1  ' Destination Type = ', a12,
     1  ' Carrier (Y/N) = ',a3, ' Reuse Plan (Y/N) = ', a3,
     1  ' Release Type = ', a12,' Called by Replace = ', a3,/
     1    '     #   Iyr   Imo   Day Source ID    Dest ID      ',
     1    '  Iter    lr    nr    nd  irep  ishort',
     1    '      Demand    Capacity     Cursto1         IWR',
     1    ' ReleaseType',
     1    '     Eff(mon)    Eff_max  Dem f(iwr)      Pavail',
     1    '       Imcd1      Avail1       imcd2      Avail2',
     1    '      Alocfs      Divmon      Relact     qres291',
     1    '      Divact',
     1    ' Iwhy Cwhy',/
     1    ' _____ _____ _____ _____ ____________ ____________ ',
     1    ' _____ _____ _____ _____ _____ _______', 
     1    ' ___________ ___________ ___________ ___________',          
     1    ' ___________',
     1    ' ___________ ___________ ___________ ___________',
     1    ' ___________ ___________ ___________ ___________',
     1    ' ___________ ___________ ___________ ___________',
     1    ' ___________',
     1    ' ____ ',48('_')) 
     

  350    format(
     1     '  Divres; Rep limit;  tranlim',/
     1     '                    ', 20f8.2)

  360 FORMAT(I5,'TH OPERATION RIGHT AT RIVER STATION ',I8,
     1 ' ASKS RELEASE FROM RESERVOIR NUMBER ',I5,
     1 ' AT RIVER STATION ',I8,' IN YEAR ',I5,' MONTH',I5)

  370      format(
     1       '  Divres; Rep limit;',
     1       '  tranlim, tranlic,  alocfs1,    alocfs',/
     1       '                    ', 20f8.2)

  380  format(
     1       '  Divres; Problem with ', a12, 1x, a24,' Type = ', i5)

  390      format(
     1       '  Divres; Release for Depletion Data;',/
     1       '                  #  divact  pavail  relact      CU',/
     1       '           ', i8, 20f8.2)
c     
c _________________________________________________________
c               Error warnings
 9999 write(6,1050) 
      write(nlog,1051) 
    
 1050 format('    Stopped in Divres',/,
     1       '    See the *.log file')
 1051 format('    Stopped in Divres')
      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)


      stop 
      END

