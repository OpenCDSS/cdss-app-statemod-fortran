C     Last change:  C    19 May 97   11:53 pm
c
      subroutine carrpl(iw,l2,divactx,ncallX)
c
c _________________________________________________________
c
c       Program Description
c               Type 7 Operating Rule
c               PERFORMS A RESERVOIR TO CARRIER SYSTEM EXCHANGE
c               if(iopsou(4,k) > 0 exchange 100% of diversion
c               if(iopsou(4,k) < 0 Exchange depletion
c
c _________________________________________________________
c
c       Update History
c
c rrb 98/03/03; Added daily option
c rrb 00/12/26; Added variable efficiency capability by adding
c               ieff2 to call rtnsec but turned off operation (ieff2=0)
c rrb 01/12/07; Revised to allow a type 14 operaring rule exchange that
c               limits the carrier based on a demand
c                 iopsou(2,l2opr) = 1 monthly limit from *.ddm
c                 iopsou(2,l2opr) > 1 annual limit from *.opr
c rrb 01/12/26; Removed equivalence by adding avtemp & awvret
c               to common and changing retpct to dumx
c
c rrb 02/10/25; Allow monthly on/off switch
c _________________________________________________________
c
c	Documentation
c
c               icx   = routine calling rtnsec
c               l2    = operational right counter
c               l2opr = Carrier operating rule counter
c
c               ieff2 = 0 always use average efficiency
c               ieff2 = 1 use max efficiency if ieffmax=1
c               iresw = 0 diversion destination
c                       1 reservoir destination
c               ndr   = source water right at operational 
c                       right destination
c                       + = diversion right
c			- = reservoir water right
c               ndc   = structure pointer for carrier
c               nwrc  = water right pointer for carrier
c
c               iExPoint( ) exchange point
c               iopsou(4, ) 0 = 100% replacement
c                        -1 = depletion replacement
c
c
c		  iopsou(3,l2) = ndLoc = diversion poiner
c			 0 = located at the source water right
c			 + = located at iopsou(3,l2) a reservoir location
c			 - = located at iopsou(3,l2) a diversion location

c               iopsou(5,l2) = not used
c               iopsou(6,l2) = Reservoir release type and efficiency
c                          0 = release to meet demand
c                          +n = release only if a CIR (IWR) exists
c                               and limit release to not exceed
c                               IWR/n,  
c                          Note nmax = min(nmax, effmax) to save 
c                               iterating
c               ireltyp        same as iopsou(6,l2)
c
c               qdiv(7   From River by Storage
c               qdiv(10  From river by Storage Transmountain 
c               qdiv(15  Reservoir to power (e.g. powres) or
c                        Reservoir to a T&C plan (Type 49 powresP)
c                        or Res to Res by a carrier
c                        Note shows as a diversion in *.xdd (From River 
c                        by Storage) but is not a diversion in *.xwb
c                        Non consumptive
c
c               qdiv(18  Carrier passing thru a structure 
c               qdiv(20  From Carrier by Storage or Exchange 
c
c		            qres(2  From Carrier by Priority
c               qres(4  From Carrier by Storage or Exch
c               qres(8  From reservoir to carrier for Tmtn
c               qres(9  From reservoir to carrier for use
c               qres(18 From River by Exchange
c               qres(21 From River by Exchange
c               qres(26 From river by Storage to Reservoir
c               qres(29 Reservoir to Reservoir by carrier
c
c _________________________________________________________
c
c       Dimensions
c
      include 'common.inc'
      character cwhy*48, cdestyp*12, ccarry*3, cpuse*3, cresid1*12      
c
c _________________________________________________________
c               Step 1; Initilize
c
c		a. Output Control      
      iout=0
      ioutiw=0
      
      if(ichk.eq.107) iout=2
      if(corid(l2).eq. ccall) ioutiw=iw
c
c		b. Units
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif
c
c		c. Detailed Output      
      iwhy=0
      cwhy='N/A'
      cdestyp='N/A'
      ccarry='No'
      cpuse='No'   
      availX=-1./fac
      imcdX=-1      
      ipuse=-1
      nd=0
      lr=l2
      
      divact = 0.0
      repact = 0.0
      ACTACF=0.0
      divreqX2=-1./fac
      divaloS=-1./fac
      qdiv181=-1./fac
      
      if (intern(l2,1).ne.0) ccarry='Yes'
c
c _________________________________________________________
c		Step X; On/Off Switch      
c
c rrb 06/01/18; Allow daily on/off switch
      if(imonsw(l2,mon).eq.0) then
        iwhy=1
        cwhy='Monthly switch Off'
        goto 380
      endif  
c
c ---------------------------------------------------------
c		For a daily model set demand for beginning of season
      if(iday.eq.1 .and. imonsw(l2,mon).gt.0) then
        if (idy.lt.imonsw(l2,mon)) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 380
        endif  
      endif  
c
c ---------------------------------------------------------
c		For a daily model set demand for end of season
      if(iday.eq.1 .and. imonsw(l2,mon).lt.0) then
        if (idy.gt.iabs(imonsw(l2,mon))) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 380
        endif  
      endif  
      
      
      small=0.001
c
c rrb 00/12/26; Variable efficiency capability
c rrb 01/08/23; Turn on variable efficiency capability
c     ieff2=0
      ieff2=1
c
c rrb 01/01/17; Subroutine call #
      icx=1

      iw=iw
c
c _________________________________________________________
c               Step 2; Check avail array coming in
c      
      ica=6  
c
c rrb; 2010/02/05; Correction      
cx    call chekava(ica, maxsta, numstal avail)
      call chekava(ica, maxsta, numsta, avail)      
c
c _________________________________________________________
c               Step 3a; Set Source Data (a reservoir)    
c               FIND REP. RES. CODE, ASSOCIATED OWNER, AND STA. CODE
      NR  =IOPSOU(1,L2)
      IF(IRESSW(NR).EQ.0) then
        iwhy=2
        cwhy='Source Reservoir is Off'      
        goto 420
      endif  
        
      IOWN=NOWNER(NR)+IOPSOU(2,L2)-1
      IPCD=IRSSTA(NR)
      NDNP=NDNNOD(IPCD)
      
c
c ---------------------------------------------------------
c rrb 2006/08/19; Check source reservoir data going in
c		Note in1=0 into subroutine      
c		isub1=subroutine calling

c     write(nlog,*) ' Carrpl into for source nr ', nr
      in1=0
      isub1=7
      call chekres(nlog, maxres, in1, isub1, iyr, mon, nr,nowner,
     1                     curown,cursto,cresid)
      
c
c _________________________________________________________
c               Step 4; Set destination operating rule
c			   (l2opr)
c
      l2opr  =IOPDES(1,L2)
c
c _________________________________________________________
c               Step 4; Special type 14 check
c               l2opr= destination operating right      
c               ncwr = carrier water right pointer
c               ncd  = carrier structure pointer
      if(ityopr(l2opr).eq.14) then
        nwrc=iopsou(1,l2opr)
        nwrc=-nwrc
c       write(io99,*) ' Carrpl; nwrc = ', nwrc 

        if(nwrc.gt.0) then
          ndc=idivco(1,nwrc)
        else
          write(io99,*) ' Carrpl; problem source is not a water right'
          write(io99,*) ' Carrpl; nwrc = ', nwrc
          goto 9999
        endif 
c
c ---------------------------------------------------------
c      
c rrb 01/12/07; Type 14 update. Allow demand to be limited to an
c               annual total adjusted 1x/year in bomsec.for
        if(iopsou(2,l2opr).eq.1) then
          iusec=nduser(ndc)+iopsou(2,l2opr)-1
        else
          iusec=nduser(ndc)+1-1
c         write(io99,*) '  Carrpl; mon, divreqa = ', mon,divreqa(l2opr)
        endif
      endif
c
c _________________________________________________________
c               Step 5; Set Destination Data for operating rule
c			   destination (l2opr)
      ND  = iopdes(1,l2opr)
c
c ---------------------------------------------------------
c		5a. Destination is a diversion (nd>0)
c
c rrb 2006/11/30; Simplify      
c     if (nd.gt.0) goto 100
      if (nd.gt.0) then
        iresw=0
cr100   IUSE=NDUSER(ND)+IOPDES(2,L2OPR)-1
        IUSE=NDUSER(ND)+IOPDES(2,L2OPR)-1
c
        IRI=NRTN(IUSE)
        IRE=NRTN(IUSE+1)-1
      endif
c
c ---------------------------------------------------------
c		5b. Destination is a reservoir (nd<00)
c
      if(nd.lt.0) then
        iresw=1
        nd=-nd
        if (iressw(nd).eq.0) then
          iwhy=3
          cwhy='Destination Reservoir is Off'      
          goto 420
        endif  
        
c
c ---------------------------------------------------------
c rrb 2006/09/25; Allow multiple accounts - Initilize
cr      irow=nowner(nd)+iopdes(2,l2OPR)-1
        nro=1
c rrb 2006/10/24; Correction      
cr      if(iopdes(2,l2).lt.0) then
        if(iopdes(2,l2Opr).lt.0) then
          nro=-iopdes(2,l2Opr)
          irow=nowner(nd)
        endif

c rrb 2006/10/24; Correction      
cr      if(iopdes(2,l2).gt.0) then
        if(iopdes(2,l2Opr).gt.0) then
          nro=1
          irow=nowner(nd)+iopdes(2,l2Opr)-1
        endif
c
c ---------------------------------------------------------
c rrb 2006/10/24; Check destination reservoir data. Note:
c		Not in1=0 into subroutine      
c		Not in1=1 out of subroutine      
c		isub1=subroutine calling
c
c       write(nlog,*) ' Carrpl into for Destination nd ', nd

        in1=0
        isub1=7
        call chekres(nlog, maxres, in1, isub1, iyr, mon, nd,nowner,
     1                     curown,cursto,cresid)
      
cx      goto 120
      endif  
c
c _________________________________________________________
c               Step 6. Set Source diversion location 
c               associated with Carrier Operating Rule
      ndr=iopsou(1,l2opr)
       if(ndr.ge.0) goto 130
         ndr=idivco(1,-ndr)
  130 IDCD=IDVSTA(NDr)
c
c _________________________________________________________
c rrb 2009/06/25; Set the source diversion location
c		    associated with a Carrier Operating Rule
c		    when it does not divert at the destination location
      ndLoc=iopsou(3,l2opr)
c
c		Right operated at a destination (reservoir) location        
      if(ndloc.gt.0) then
        Ndr = ndloc
        IDCD=IRSSTA(Ndr)          
      endif  
c
c		Right operated at a diversion location        
      if(ndloc.lt.0) then
        Ndr = -ndloc
        IDCD=Idvsta(Ndr)          
      endif       
c
c _________________________________________________________
c               qdiv(18  Carrier passing thru a structure 
      qdiv181=qdiv(18,idcd)
      NDND=NDNNOD(IDCD)
c
c _________________________________________________________
c               Step 7. Initial checks on demand & available flow

C-----  INITIAL CHECKS ON DEMAND AND AVAILABLE FLOWS
c jhb 2014/07/17 the second condition can not always be evaluated so rewrite to isolate it
c                this is something that the lahey compiler allowed, probably by stopping
c                  the condition evaluation sequence after the first one evaluates to false
c                  (since it is an .AND. condition)
c  140 IF(iresw.eq.0.and.DIVREQ(IUSE).LE.small) then
c        iwhy=4
c        cwhy='Demand is Zero'
c        goto 420
c      endif

  140 IF(iresw.eq.0) then
        if(DIVREQ(IUSE).LE.small) then
          iwhy=4
          cwhy='Demand is Zero'
          goto 420
        endif
      endif  

      availX=avail(idcd)
      avail1=avail(idcd)
      IF(AVAIL(IDCD).LE.small) then
        iwhy=5
        cwhy='Available Flow at destination (avail1) is Zero'      
        goto 420
      endif  
c
c _________________________________________________________
c               Step 5a; Destination is a diversion calculate demand
c
      if (iresw.eq.0) then
        divreqx2=divreq(iuse)
        DIVALO=AMIN1(DIVREQ(IUSE),DIVCAP(ND)-DIVMON(ND),
     1               divcap(ndr)-divmon(ndr))
c
c rrb 01/08/23; 
c               a. If release type code is on
c                  Limit release to occur only if an IWR exists
c                  Note still releasing to meet 100% of demand
c
        ireltyp=amin0(iopsou(6,l2),ifix(effmax(nd)))
c
        divmax=0.0
        if(ireltyp.gt.0) then 
          if(diwrreq(iuse).le.small) then
            divalo=0.0
          else
            divmax = diwrreq(iuse)/(float(ireltyp)/100.0)
            divalo=amin1(divalo, divmax)
          endif
        endif

        divalo=amax1(0.0,divalo)
c
c 3/11/96 rrb;
c               b. Check Printout for a diversion
        if(iout.eq.1) then
          write(99,141) 
          write(99,'(10x, 20f12.2)') 
     1      divreq(iuse), divcap(nd)-divmon(nd), 
     1      diwrreq(iuse), 
     1      float(iopsou(6,l2)), effmax(nd), float(ireltyp), 
     1      divmax, divalo
        endif
      endif   
c
c _________________________________________________________
c               Step 5b; Destination is a reservoir calculate demand 
c
c rrb 98/03/03; Daily capability
      if(iresw.eq.1) then
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
     1               volmax(nd)-cursto(nd),tarmax(nd)-cursto(nd))/fac
        divaloS=divalo
        divalo=amax1(divalo,0.0)
        if(divalo.le.small) then
          iwhy=6
          cwhy='Destination Reservoir Demand is Zero'      
          goto 420
        endif       
      endif

c
c _________________________________________________________
c               Step 6; For type 14 Limit Demand
cInsert X   c
c rrb 01/14/07     Type 14 Update.
c                  Destination is a reservoir or a diversion
c                  Limit allowable diversion to monthly
c                  monthly demand (divreq) when iopsou(2,l2) = 1 or
c                  annual demand (divreqa) when iopsou(2,l2) > 1
c
      if(ityopr(l2opr).eq.14) then   
        if(iopsou(2,l2opr).eq.1) then
          divreqx2=divreq(iusec)
          divalo=amin1(divreq(iusec),divalo)
        else
          divalo=amin1(divreqa(l2opr)/fac, divalo)
        endif
        
        divalo=amax1(divalo,0.0)
        if(divalo.le.small) then
          iwhy=7
          cwhy='Demand is Zero'      
          goto 420
        endif  
        
      endif
c
c _________________________________________________________
c               Step 7; Calculate available supply
c
      RPLALO=AMIN1(CUROWN(IOWN),CURSTO(NR)-VOLMIN(NR))
      RPLALO=AMAX1(0.0,RPLALO)

      IF (RPLALO.LT.1) then
        iwhy=8
        cwhy='Available Supply is Zero'      
        goto 420
      endif  
c
c rrb 98/03/03; Daily capability
c     ALOCFS=RPLALO/MTHDAY(MON)/FACTOR
      ALOCFS=RPLALO/fac
c
c _________________________________________________________
c               Step 8; Limit release to max channel capacity
c

      ALOCFS=AMIN1(FLOMAX(NR)-RIVER(IPCD),ALOCFS,DIVALO)
      alocfs = amax1(alocfs, 0.0)
c
c
c _________________________________________________________
c               Step 9; FOR CARRIER SYSTEM LIMIT DEMAND TO CAPACITY OF 
c                       INTERVENING STRUCTURES 
      do 150 i61=1,10
        if (intern(l2OPR,i61).eq.0) goto 160
        intvn=intern(l2OPR,i61)
c
c rrb 98/03/03; Daily capability
  150   alocfs=amin1(alocfs,(divcap(intvn)-divmon(intvn))) 
  160 continue
c
c rrb 06/28/96
        alocfs=amax1(alocfs, 0.0)
c
c _________________________________________________________
c               Step 10; Find Minimum Flow downstream 
c			 Up to exchange point
c
      IMCD=Idcd
      imcdX=imcd
      ISS=Idcd
      DO 170 NDx=1,NDNd
        IF(AVAIL(IMCD).GT.AVAIL(ISS)) IMCD=ISS
        availX=avail(imcd)
        if (iss.eq.iExPoint(l2)) goto 180
  170 ISS=IDNCOD(ISS)

  180 pavail=amax1(avail(imcd),0.0)
      DIVACT=amin1(pavail,divalo,alocfs)
      divactx=divact
c     IF(DIVACT.LE.0.00001) goto 420
      IF(DIVACT.LE.small) then
        iwhy=9
        cwhy='Divact is Zero'      
        goto 420
      endif  
c
c _________________________________________________________
c               Step 11; Destination is a reservoir; 
c                        Take water out and replace
c                        Note Avoid all refrences to return flows 
      if(iresw.eq.1) then
        availr=avail(ipcd)
        call takout(maxsta,avail,river,avinp,qtribu,idncod,
     1              divact,ndnd,idcd)
        repact=-divact
        call takout(maxsta,avail,river,avinp,qtribu,idncod,
     1              repact,ndnp,ipcd)
        avail(ipcd)=availr
c
        goto 290    
      endif
c
c _________________________________________________________
c               Step 12; Destination is a ditch, but no returns

      IF(IRI.LE.IRE.AND.IRTURN(IUSE).NE.4) Goto 190
c
        AVAILR=AVAIL(IPCD)
        CALL TAKOUT(maxsta, AVAIL,RIVER,AVINP,QTRIBU,IDNCOD,
     1              DIVACT,NDND,IDCD)
        REPACT=-DIVACT
        CALL TAKOUT(maxsta,AVAIL,RIVER,AVINP,QTRIBU,IDNCOD,
     1              REPACT,NDNP,IPCD)
        AVAIL(IPCD)=AVAILR
        IF(IRTURN(IUSE).EQ.4) QSTERN(ND)=QSTERN(ND)-DIVACT
c
c rrb 01/02/10; Call rtnsec who calls return to calculate CU,
c               reduce IWR and loss (as appropriate)
        call rtnsec(icx,divact,L2opr,iuse,idcd,nd,ieff2)

        Goto 290
c
c _________________________________________________________
c               Step 13; Destination is a ditch, maybe account for
c                       returns if (pavail.lt.divalo) or
c                       depletion if (iopsou(4,l2).lt.0)
C-----   UPDATE FLOW AND DEMAND IF FULL REPLACEMENT MADE, 
c        ENOUGH WATER AVAILABLE
  190 IF(PAVAIL.LT.DIVALO) Goto 200
      if(iopsou(4,l2).lt.0) goto 200
        AVAILR=AVAIL(IPCD)
        CALL TAKOUT(maxsta,AVAIL,RIVER,AVINP,QTRIBU,IDNCOD,
     1              DIVACT,NDND,IDCD)
        REPACT=-DIVACT
        CALL TAKOUT(maxsta,AVAIL,RIVER,AVINP,QTRIBU,IDNCOD,
     1              REPACT,NDNP,IPCD)
        AVAIL(IPCD)=AVAILR
c
c rrb 98/12/30 & 00/12/26; Wells & Variable Efficiency
c       if(iresw.eq.0) CALL RTNSEC(icx,DIVACT,L2opr,IUSE,Idcd)
        if(iresw.eq.0) CALL RTNSEC(icx,DIVACT,L2opr,IUSE,Idcd,nd,
     1                             ieff2)
        Goto 290
c
c _________________________________________________________
c               Step 14; RETURN FLOW CONSIDERATION FOR DEPLETION
c                       REPLACEMENT OR NOT ENOUGH FLOW
c------ INITIALIZE ACCUMMULATED RETURN FLOW WEIGHT FOR ALL STATION
  200 FORET=1.0-DIVEFF(mon,IUSE)/100.
      DO 210 IS=1,NUMSTA
        AVWRET(IS)=0.
  210 dumx(is) = 0.0

C------  FILL THE TEMP0RARY AVAILABLE FLOW
      ISS=IDCD
      DO 220 IS=1,NDND
      AVTEMP(ISS)=AVAIL(ISS)
  220 ISS=IDNCOD(ISS)

C------  STEP THROUGH RETURN FLOWS FOR CURRENT DIVERSION.
      DO 240 IRT=IRI,IRE
        IRCD =IRNSTA(IRT)
        NDNR =NDNNOD(IrCD)
c
c rrb 05/28/98; allow return id to not be the array counter
        idly=irtndl(irt)
c
c               CHECK DELAY TYPE
c rrb 97/10/15; Daily model temporarily divide by days per month (imd)
        if(iday.eq.0) then
          FACDLY=DLYRAT(1,IDLY)
        else
          facdly=dlyratd(1,idly)
        endif
        RET=PCTTOT(IRT)*FACDLY/10000.
c
c               STORE RETURN FLOW PERCENTAGES IN TEMPORARY ARRAY
        ISS=IRCD
        DO 230 NS=1,NDNR
          AVWRET(ISS)=AVWRET(ISS)+RET
  230   ISS=IDNCOD(ISS)
  240 CONTINUE

c               COMPUTE THE ALLOWABLE AMOUNT FOR THE CURRENT DIVERSION
      ISS=IdCD
      DO 260 NS=1,NDNd
c     IF(ABS(AVWRET(ISS)*FORET-1.).LE.0.00001) Goto 250
      IF(ABS(AVWRET(ISS)*FORET-1.).LE.small) Goto 250
      dumx(iss) = (1.0-AVWRET(ISS)*FORET)
      AVWRET(ISS)=AVTEMP(ISS)/(1.0-AVWRET(ISS)*FORET)
      Goto 260      
  250 AVWRET(ISS)=1.0E10
  260 ISS=IDNCOD(ISS)
      AVWRET(IdCD)=AVTEMP(IdCD)
c
c               FIND THE UPDATED DOWNSTREAM MIN FLOW NODE
c               in the exchange reach (iss.eq.iExPoint(l2)
      iss=idcd
      DO 272 NDx=1,NDNd
      IF(AVwret(IMCD).GT.avwret(ISS)) IMCD=ISS
      if (iss.eq.iExPoint(l2)) goto 282
  272 ISS=IDNCOD(ISS)
  282 PAVAIL=AVWRET(IMCD)
      DIVACT=amin1(pavail,divalo,alocfs)
      CALL TAKOUT(maxsta,AVAIL,RIVER,AVINP,QTRIBU,IDNCOD,
     1            DIVACT,NDND,IDCD)
c
c _________________________________________________________
c               Step 15; Add RESERVOIR RELEASES BACK IN
c
      REPACT=-DIVACT  
      AVAILR=AVAIL(IPCD)
      if (iopsou(4,l2).lt.0) REPACT=-DIVACT* dumx(iExPoint(l2))
      CALL TAKOUT(maxsta,AVAIL,RIVER,AVINP,QTRIBU,IDNCOD,
     1            REPACT,NDNP,IPCD)
      AVAIL(IPCD)=AVAILR
c
c rrb 98/12/30; Add Well capability
c     if(iresw.eq.0) CALL RTNSEC(icx,DIVACT,L2opr,IUSE,Idcd)
c
c rrb 00/12/26; Add Variable efficiency capability 
c     if(iresw.eq.0) CALL RTNSEC(icx,DIVACT,L2opr,IUSE,Idcd,nd)
      if(iresw.eq.0) CALL RTNSEC(icx,DIVACT,L2opr,IUSE,Idcd,nd,
     1                           ieff2)
c
c _________________________________________________________
c
c               Step 16; CHECK AVAILABLE FLOW, PRINT WARNING IF NEGATIVE
c
  290 CALL DNMFSO(maxsta, AVAIL ,IDNCOD,IdCD  ,NDNd  ,IMCD  )
c         
      IF(AVAIL(IMCD).le.(-1.0*small)) then
        WRITE(99,300) corid(l2), nameo(l2)
        write(99,302)
     1    IYR,MON,IW,NWRORD(1,IW),L2,
     1    IUSE,l2opr, iresw, IdCD, IMCD,
     1    iExPoint(l2), iopsou(4,l2), iopdes(1,l2opr), iopdes(2,l2opr)

        write(99,304)
     1    divreq(iuse),DIVACT, avail(imcd) 
        WRITE(6,310) (AVAIL(ISS),ISS=1,NUMSTA)
        write(99,310) (avail(iss),iss=1,numsta)
c       write(99,320) (river(iss),iss=1,numsta)
        AVAIL(IMCD)=0.
        goto 9999
      endif
c
c _________________________________________________________
c               Step 17a; Destination is a Reservoir. Update 
c                        reservoir data
c
  330 if (iresw.eq.1) then
c
c rrb 98/03/03; Daily capability
c       divaf=divact*mthday(mon)*factor
        divaf=divact*fac
        cursto(nd)=cursto(nd)+divaf
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
        icx=107
c
c rrb 2007/01/04; Correction from river by exchange        
c rrb BACK
        ia=4
cy      ia=18
        cresid1=cresid(nd)
c        
        call accou(maxacc, maxown, nrX, ownmon, curown, accr, ia, 
     1    ownmax, iownX, nrown1, cursa, divaf, iResT1, icx, cresid1)
c
c rrb 2007/01/04; Correction from river by exchange        
c rrb BACK
        qres(4,nd)=qres(4,nd)+divaf
cy      qres(18,nd)=qres(18,nd)+divaf
c
c rrb 2006/10/27; Reservoir to Reservoir for water balance calculations
c rrb 2006/01/04 REMOVE   
c rrb 2010/10/15; Remove; diversion is by exchange not reservoir
c                 to reservoir by a carrier
        qres(29,nd)=qres(29,nd) + divaf          
      endif
c
c _________________________________________________________
c               Step 17b; Destination is a Diversion 
c                         update diversion data
c
c
c rrb 2006/11/30; Simplify        
      if (iresw.eq.0) then
        USEMON(IUSE)=USEMON(IUSE)+DIVACT
        DIVREQ(IUSE)=DIVREQ(IUSE)-DIVACT
        DIVMON(ND  )=DIVMON(ND  )+DIVACT
c
c _________________________________________________________
c               Step 18; Destination is a diversion
c               UPDATE STORAGE WATER USE OF DESTINATION DEMAND 
c               AT RIVER (QDIV(7)) 
c               WATER CARRIED ACCOUNT (QDIV(18) OF OTHER NODES, 
c               DESTINATION STORAGE WATER USE (QDIV(20)
c               idcd  = river ID
c               inode = destination ID 
c               iresw = 1 destination is a reservoir
c                     = 0 destination is a demand
c
      
        inode=idvsta(nd)
c
c rrb 2006/11/30; If the destination is the same as the location of 
c                 the source right do not adjust qdiv(20,inode)       
c               qdiv(20  From Carrier by Storage or Exchange 
cx      qdiv(20,inode)=qdiv(20,inode)+divact
        if(inode.ne.idcd) then
          qdiv(20,inode)=qdiv(20,inode)+divact
        endif  
c
c               Branch for transmountain (irturn(iuse) = 4)
c               qdiv(7   From River by Storage
c rrb 2011/02/18; idgrst(nd) is always 0
        IF(IDRGST(ND).NE.0) Goto 380
        IF (IRTURN(IUSE).EQ.4) Goto 370
        QDIV(7,IDCD)=QDIV(7,IDCD)+DIVACT
      endif  
c
c _________________________________________________________
c               Step X; Destination is a diversion or reservoir
c                       Update intervening structures
c               qdiv(18  Carrier passing thru a structure 
c rrb 2006/11/30; Simplify
cx340 do 350 i11=1,10
      do i11=1,10
        if (intern(l2opr,i11).eq.0) goto 360
        intvn=intern(l2opr,i11)
        divmon(intvn)=divmon(intvn)+divact
        inode=idvsta(INTVN)
c
c rrb 2009/06/26; Correction if the carrier is the diversion
c		    location it gets adjusted below
        if(iabs(ndloc).ne.intvn) then        
          qdiv(18,inode)=qdiv(18,inode)+divact
        endif
cx350 continue
      end do
c
c ---------------------------------------------------------
c Adjust carrier at the destination (qdiv(18
  360 continue
  
      qdiv(18,idcd)=qdiv(18,idcd)+divact
      Goto 390
c
c        qdiv(10  From river by Storage to Transmountain 

  370 QDIV(10,IDCD)=QDIV(10,IDCD)+DIVACT
      Goto 390
c
  380 IR=IRSORD(1,IDCD)             
c
c rrb 98/03/03; Daily capability
c               qdiv(18  Carrier passing thru a structure 
c     QRES(18,IR)=QRES(18,IR)-repact*MTHDAY(MON)*FACTOR
      QRES(18,IR)=QRES(18,IR)-repact*fac
c
c ---------------------------------------------------------
c               Adjust destination

  390 continue
c
c               qdiv(7   From River by Storage  
c               qdiv(21  Exchange via a reservoir (e.g.
c               qdiv(15  Reservoir to power (e.g. powres) or
c                        Reservoir to a T&C plan (Type 49 powresP)
c                        or Res to Res by a carrier
c                        Note shows as a diversion in *.xdd (From River 
c                        by Storage) but is not a diversion in *.xwb
c                        Non consumptive
c rrb 2011/02/18; Correction Source is a reservoir
cx    qdiv(21,idcd) = qdiv(21,idcd) + divact
      if(iresw.eq.1) then
        qdiv(15,idcd)  = qdiv(15,idcd)  + divact
cx        qdiv(30,idcd) = qdiv(30,idcd) + divact
      endif
c
c
c _________________________________________________________
c               Step 19; UPDATE Supply Reservoir
c
  400 ACTACF=REPACT*fac
      CURSTO(NR  )=CURSTO(NR  )+ACTACF
      REPLAC(NR  )=REPLAC(NR  )-REPACT
      CUROWN(IOWN)=CUROWN(IOWN)+ACTACF
c
c jhb 2014/07/17 the second condition can not always be evaluated so rewrite to isolate it
c                this is something that the lahey compiler allowed, probably by stopping
c                  the condition evaluation sequence after the first one evaluates to false
c                  (since it is an .AND. condition)
c      IF (iresw.eq.0 .and. IRTURN(IUSE).EQ.4) Goto 410
      IF (iresw.eq.0) then
        if (IRTURN(IUSE).EQ.4) then
          Goto 410
        endif
      endif

      QRES(8,NR)=QRES(8,NR)-ACTACF
      accr(8,iown)=accr(8,iown)-actacf
      divactx=divact
      goto 420
c
  410 QRES(9,NR)=QRES(9,NR)-ACTACF
      divactx=divact

  420 continue
c
c ---------------------------------------------------------
c rrb 01/12/07; Type 14 Update.
c               Allow demand to be limited to a monthly or an 
c               annual total adjusted 1x/year in bomsec.for
      if(ityopr(l2opr).eq.14) then
        if(iopsou(2,l2opr).eq.1) then
          divreq(iusec)=divreq(iusec)-divact
        else
          divreqa(l2opr)=divreqa(l2opr)-divact*fac
        endif
      endif

c
c               UPDATE OPERATING RIGHT DIVERSION
 500  divo(l2) = divo(l2)+divact

      if(iout.eq.2 .and. iw.eq.ioutiw) then      
        ncallx=ncallx+1
        if(ncallx.eq.1) then
          write(nlog,270) corid(l2),cdestyp,ccarry,cpuse
        endif  
      
        write(nlog,280) 'CarRpl     ',
     1    iyrmo(mon),xmonam(mon), idy,
     1    cstaid(idcd),iwx, iw,nwrord(1,iw),l2,lr,
     1    ND,nr, ipuse,imcdX, availX*fac, DIVREQx2*fac, 
     1    divaloS*fac, -1.0, qdiv181*fac, QDIV(18,idcd)*fac, 
     1    divactx*fac, iwhy, cwhy
     
       write(nlog,281) 'CarRpl     ',
     1    iyrmo(mon),xmonam(mon), idy,
     1    cstaid(idcd),
     1    divactx*fac, repact*fac,      
     1    qres(4,nd), qres(29,nd), 
     1    qres(8,nr), qres(9,nr), qres(18,ir) 
     
      endif
c
c ---------------------------------------------------------
c rrb 2006/10/24; Check source reservoir data. Note:
c               in1=0 in to subroutine      
c               in1=1 out of subroutine      
c		isub1=subroutine calling

c     write(nlog,*) ' Carrpl out for Source nr ', nr

      in1=1
      isub1=7
      call chekres(nlog, maxres, in1, isub1, iyr, mon, nr,nowner,
     1                     curown,cursto,cresid)


c ---------------------------------------------------------
c rrb 2006/10/24; Check destination reservoir data. Note:
c		Not in1=0 into subroutine      
c		Not in1=1 out of subroutine      
c		isub1=subroutine calling
        if(iresw.eq.1) then
c         write(nlog,*) ' Carrpl out for Destination nd ', nd

          in1=1
          isub1=7
          call chekres(nlog, maxres, in1, isub1, iyr, mon, nd,nowner,
     1                     curown,cursto,cresid)
        endif
 
c
c _________________________________________________________
c               Step 20; Return
c
      return
c
c _________________________________________________________
c
c               Formats
 141      format(       
     1    '  Carrpl; ',
     1    '      Demand    Capacity         IWR',
     1    '     Eff_opr     Eff_max    Eff_used   Dem f(iwr)',      
     1    '      Divalo',/
     1    ' _________', 
     1    ' ___________ ___________ ___________',          
     1    ' ___________ ___________ ___________ ___________',
     1    ' ___________') 

  270   format(/, 
     1  ' CarRpl(Type 7); Operation Right ID = ', a12,
     1  ' Destination Type = ', a12,
     1  ' Carrier (Y/N) = ',a3, ' Reuse Plan (Y/N) = ', a3/
     1  ' CarRpl       iyr mon   day ID          ',
     1  '    Iter      Iw  nwrord      l2      lr      Nd      nr',
     1  '   ipUse', 
     1  '   imcdX  availX demandX divaloS psuplyT qdiv181 qdiv(18',
     1  ' divactX    iwhy Comment',/
     1  ' ___________ ____ ____ ____ ____________', 
     1  ' _______ _______ _______ _______ _______ _______ _______', 
     1  ' _______',
     1  ' _______ _______ _______ _______ _______ _______ _______',
     1  ' _______ _______ __________________________')
     
  280   FORMAT(a12, i5,1x,a4, i5, 1x, a12,9i8,7F8.1,i8,1x, a48) 
  281   FORMAT(a12, i5,1x,a4, i5, 1x, a12, 20f8.0)

  300   format(
     1    '  Carrpl; Print 5 for operation right ',a12, 1x, a24/,
     1    '     IYR     MON      IW  NWRORD      L2',
     1    '    IUSE   l2opr   iresw    idcd    imcd',
     1    ' iopsou3 iopsou4 iopdes1 iopdes2',
     1    '    divreq(iuse)          DIVACT     avail(imcd)')
  302   format(14i8)
  304   format(112x, 20f16.4)
  310   format(' carrpl: avail  ',10f10.2)
  320   format(' carrpl: river  ',10f10.2)

c
c _________________________________________________________
c
c               Error Messages
c
 9999 write(6,1050) 
      write(99,1051) 

 1050 format('    Stopped in Carrpl',/,
     1       '    See the *.log file')
 1051 format('    Stopped in Carrpl')

      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop 
      END

