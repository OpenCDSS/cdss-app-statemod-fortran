C     Last change:  RRB   7 Dec 101    1:20 pm
C
      SUBROUTINE DIVCAr1(IW,L2,ISHORT,divactx,ncallX)
c
c
c _________________________________________________________
c	Update History
c
c       Divar1;   It handles a type 14 Operating rule which is 
c                 Similar to a standard carrier (type 11) but  
c                 it recognizes an annual limit to a diversion
c                 when iopsou(4,l2) is .gt. 1.
c		    Note Oprinp checks that iopsou(4,l2) >=1)
c                 Also it inlcudes lots of notes added to the old 
c                 type 11
c
c _________________________________________________________
c
c       Update History (most is for Divcar)
c
c
c rrb 2006/08/18; Revised to work with multiple reservoir
c                 destination accounts
c rrb 96/03/13; initilize divact, send returns to bottom & set divo
c
c rrb 00/12/26; For variable efficiency capability replaced lots of 
c               code that appears in many different subroutines
c               with the following:
c               1. Calculate the max available with returns 
c                  (pavail) by calling rtnmax
c
c               2. If pavail.ge.divalo they get what they want.
c                  a. Set the diversion (divact) to demand (divalo)
c                  b. Take out of stream (call takout)
c                  c. Calculate returns (call rtnsec)
c                  d. Exit
c
c               3. If pavail.lt.divalo they are limited to less than
c                     what they want at a maximum efficiency.
c                  a. Set the diversion (divact) to available (pavail)
c                  b. Take out of stream (call takout)
c                  c. Calculate returns (call rtnsec)
c                  d. Check if more can be diverted by operating
c                     at less than maximum efficiency
c                  e. If no more is availble exit.
c
c                  f. If more is available then
c                     f1. Finding max available with returns
c                         (pavail) by calling rtnmax
c                     f2. Take out of stream (call takout)
c                     f3. Calculate returns (call rtnsec)
c               
c rrb 01/02/01; Add call rtnsec who calls return to calculate CU,
c               reduce IWR and loss (as appropriate)
c rrb 02/06/27; Revise logic for maximum efficiency to limit
c               diversion to demand via divalo
c
c _________________________________________________________
c
c	Documentation
c
c               icx        subroutine call # (3)
c
c               IW : OVERALL WATER RIGHT ORDER
c               L2 : LOC. OF operation right  in opr RIGHT TABLE
c
c               IDVSTA(L2) : STATION CODE OF WHERE DIV. RIGHT L2 LOCATES
c               ieff2 = 0 always use average efficiency
c                     = 1 use max efficiency if ieffmax = 1
c
c               nd1     source diversion ID
c               nd2     destination diversion ID
c               ndr     source water right
c               iscd    stream location of source diversion (nd1)
c
c		iOpDesR1  =  iopdesr(l2)
c                        source right type
c			 + = diversion
c			 - = reservoir
c               iopsou(1,l2) = NSR =  source water right 
c			 + = diversion
c			 - = reservoir
c               iopsou(2,l2) Use by Oprinp to turn on or Off the 
c			     source right
c
c		iopsou(3,l2) = not used
c
c               iopsou(4,l2) demand switch
c                       1  = constrained by a monthly demand (divreq)
c                       >1 = constrained by an annual demand (divreqa)
c			Note Oprinp checks that iopsou(4,l2) > or =1
c
c               ndns    # of nodes downstream of source diversion (nd1)
c
c               iuses   if constrained by a monthly demand, source user
c                       if constrained by annual demand, not used
c               iuse    destination user
c
c               irig    switch
c                       1  = source is a water right
c                       <1 = source is a diversion (not used?)
c
c               iresw   switch
c                       1 = destination is a reservoir
c                       0 = destination is a diversion
c               irow    reservoir account
c
c               qdiv(5  Total diversion 
c               qdiv(8  Carrier to a transmtn diversion 
c
c               qdiv(18 Carrier through a structure
c               qdiv(19 Carrier used by a structure
c
c               qres(2  To reservoir from carrier by priority
c
c
c
c _________________________________________________________
c		Dimensions
      include 'common.inc'
c      
      character 
     1  cwhy*48, cdestyp*12, ccarry*3, cpuse*3, cresid1*12,
     1  cCallBy*12, corid1*12
c _________________________________________________________
c
c               Step 1; Initilize
c

      iout=0
      ioutiw=0
      ioutZ=0
      
      cCallBy='DivCar1     '
      corid1=corid(l2)
      
      if(ichk.eq.114) iout=1
      if(corid(l2).eq. ccall) ioutiw=iw
      
c     write(nlog,*) '  Divcar1; iout, ioutiw, iw, ccall'
c     write(nlog,'(10x,3i5,2(1x,a12))') 
c    1  iout, ioutiw, iw, ccall, corid(l2)
c               a. Convergence
      small=0.001
c
c               b. Miscellaneous
      divact=0.0
      ISHORT=0
c
c               c. Daily capability
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif
      
      iwhy=0
      cwhy='NA'
      cdestyp='NA'
      ccarry='No'
      cpuse='No'   
      availX=-1./fac
      imcdX=-1      
      ipuse=0     
      lr=l2
      
      divreqX2=-1./fac
      divaloS=-1./fac
      CapRem2=-1./fac
      
      if (intern(l2,1).ne.0) ccarry='Yes'
c
c _________________________________________________________
c
c               Step X; Branch if not on this month
c
c rrb 04/22/96; Allow month switch
      if(imonsw(l2,mon).eq.0) then
        iwhy=1
        cwhy='Monthly switch is off'
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
      
c
c rrb 00/12/26; Variable efficiency capability set to 1 (on)
      ieff2=1
c
c rrb 01/01/17; Call number
      icx=3
c
c _________________________________________________________
c
c               Step 2; Set source data
c
c               a. Source structure (nd1)
c                   - negative is a water right
c                   - positive is a structure
c                  Source location (iscd)
c                  Number of downstream nodes (ndns)
      ND1  =Iopsou(1,L2)
      IRIT=0
c
c rrb; clean up               
c     IF (ND1.GT.0) GOTO 100
c     write(io99,*) '  Divcar1; nd1 = ', nd1
c
c		Source is a water right (nd1.le.0)
      if(nd1.le.0) then
        IRIT=1 
        NDR=-ND1
        ND1=IDIVCO(1,NDR)
      endif

  100 if (idivsw(nd1).eq.0) then
        iwhy=2
        cwhy='Source structure is off'  
        goto 380
      endif

      ISCD=IDVSTA(ND1)
      NDNS=NDNNOD(ISCD)
c     write(io99,*) '  Divcar1; nd1, iscd', nd1, iscd  
c
cgrb 06/08/97  find user number to check demand level at source
c rrb 99/08/09; Type 14 edit. Allow demand to be limited to an
c               annual total adjusted 1x/year in bomsec.for
      if(ityopr(l2).eq.14 .and. iopsou(4,l2).eq.1) then
        iuses=nduser(nd1)+iopsou(4,l2)-1
      else
        iuses=nduser(nd1)+1-1
c       write(io99,*) '  Divcar1; mon, divreqa = ', mon,divreqa(l2)
      endif
c
c _________________________________________________________
c
c               Step 3; Set destination data
c
c               a. Destination (nd2)
c                   - negative is a reservoir
c                   - positive is a structure
      ND2  =Iopdes(1,L2)
      iresw=0
      if (nd2.gt.0) go to 110
c
c               b. Reservoir Destination (nd2 is negative)
      cdestyp='Reservoir'
      iresw=1
      nd2=-nd2
      if (iressw(nd2).eq.0) then
        iwhy=3
        cwhy='Destination reservoir is Off'
        goto 380
      endif
c
c ---------------------------------------------------------
c rrb 2006/08/18; Reservoir Destination 
c                 Revised to work with multiple reservoir
      nro=1
      if (iopdes(2,l2).lt.0) then
        nro=-iopdes(2,l2)
        irow=nowner(nd2)
      endif

      if (iopdes(2,l2).gt.0) then
        nro=1
        irow=nowner(nd2)+iopdes(2,l2)-1
      endif  
c
c ---------------------------------------------------------
c               c. Reservoir Destination 
c			Check reservoir roundoff when exiting routine
c			Note in1=0 into a routine, 1 out of a routine
c			     isub1 = subroutine calling chekres
      in1=0
      isub1 = 14
      nr=nd2
      call chekres(io99, maxres, in1, isub1, iyr, mon, nr,nowner,
     1                    curown,cursto,cresid)
      
      go to 120
c
c ---------------------------------------------------------
c               c. Diversion Destination (nd2 is positive)
  110 cdestyp='Diversion'
      if(idivsw(nd2).eq.0) then
        iwhy=4
        cwhy='Destination Diversion is Off' 
        goto 380
      endif
C

      IUSE=NDUSER(ND2)+Iopdes(2,L2)-1
c
c _________________________________________________________
c
c               Step 4; Check for demand (divreq) and
c                       capacity limit (divcap-divmon .le. small)
c 
c		Diversion Demand
  120 IF(iresw.eq.0) then
        if(DIVREQ(IUSE).LT.small) then
          divreqx2=divreq(iuse)
          iwhy=5
          cwhy='Diversion Destination demand (Demand) is zero'     
          goto 380
        endif  
        
        CapRem2= divcap(nd2)-divmon(nd2)
        if(CapRem2 .le. small) then
          iwhy=6
          cwhy='Diversion Destination capacity (CapRem2) is zero'     
          goto 380
        endif  
      endif  
c
c _________________________________________________________
c
c               Step 5; Set up checks on available flow (avtemp)
c                 ADD RETURN FLOW FROM PREVIOUS WATER RIGHTS
c

      DO IS=1,NUMSTA
        AVTEMP(IS)=AVAIL(IS)
      end do
C
      IF(iresw.eq.0.and.(IOPRTN.EQ.0.OR.IRTURN(IUSE).EQ.4)) GO TO 140
C
c      
c rrb 2006/09/28; Not required with 1 structure per station      
cr    AVTEMP(ISCD)=AVTEMP(ISCD)+CURRTN(ISCD)
c
c _________________________________________________________
c
c               Step 6; Check available flow at diversion (avtemp(iscd)
c 

c               CHECK AVAILABLE WATER AT CURRENT STATION
c 140 IF(AVTEMP(ISCD).GT.0.00001) GO TO 150
  140 IF(AVTEMP(ISCD).GT.small) GO TO 150
C
      IF(iresw.eq.0.and.IRTURN(IUSE).LE.3) ISHORT=1
        iwhy=7
        cwhy='Available flow (AvailX) is zero'
        goto 380
c
c               FIND STARTING AND ENDING INDEX OF RETURN FLOW STATIONS
  150 if(iresw.eq.0) IRI=NRTN(IUSE)
      if (iresw.eq.0) IRE=NRTN(IUSE+1)-1
c
c _________________________________________________________
c
c               Step 7; Set allowable diversion (divalo)
c 
cgrb 06-05-95; Added ability to look at multiple reservoir accounts
      if (iresw.eq.1) then
c
c               a. Destination is a reservoir
c                    Limit to remaining res. capacity (volmax-cursto)
c
c rrb 2006/08/18; Revised to work with multiple reservoir
        cursa=0
        do n=1, nro
          n1=irow+n-1
          cursa=cursa+(ownmax(n1)-curown(n1))
        end do
c
        divalo=amin1(cursa,volmax(nd2)-cursto(nd2),
     1  tarmax(nd2)-cursto(nd2))/fac
        divalo=amax1(0.0,divalo)
        divreqX2=divalo    
      endif

      if (iresw.eq.1) go to 170
c
c               b. Destination is a diversion
c                    Limit allowable diversion (divalo)
c                    to destination capacity (divcap-divmon)
      IF(IRIT.EQ.0) DIVALO=AMIN1(DIVREQ(IUSE),DIVCAP(ND2)-DIVMON(ND2))
c
c               c. Destination is a diversion
c                    Limit allowable diversion (divalo) to
c                     destination capacity (divcap(Nd2)-divmon(nd2)) &
c                     water right (dcrdiv - divd)
      IF(IRIT.EQ.1) DIVALO=AMIN1(DIVREQ(IUSE),DIVCAP(ND2)-DIVMON(ND2),
     + DCRDIV(NDR)-DIVD(NDR))
      divalo=amax1(0.0,divalo)
      divreqX2=divalo
c
c               d. Destination is a reservoir or a diversion
c                    Limit to allowable diversion (divalo) &
c                    source capacity (divcap(nd1)-divmon(nd1))
  170 DIVALO=AMIN1(DIVALO,DIVCAP(ND1)-DIVMON(ND1))
c
c
c               e. Destination is a reservoir or a diversion
c                    Limit allowable diversion to monthly
c                    monthly demand (divreq) when iopsou(4,l2) = 1 or
c                    annual demand (divreqa) when iopsou(4,l2) > 1
c rrb 99/08/09; Type 14 Edit. Allow demand to be limited to an annual
c               total adjusted 1x/year in bomsec.for
      if(iopsou(4,l2).eq.1) then
        divalo=amin1(divreq(iuses),divalo)
      else
        divalo=amin1(divreqa(l2)/fac, divalo)
      endif
      divalo=amax1(0.0,divalo)
      divreqX2=divalo
c
c               f. Destination is a reservoir or a diversion
c                    Limit diversion to intervening structures
      do 180 i61=1,10
        if (intern(l2,i61).eq.0) go to 190
        intvn=intern(l2,i61)
        if (divalo.le.(divcap(intvn)-divmon(intvn))) go to 180 
        divalo=divcap(intvn)-divmon(intvn)
  180 continue
  190 continue
      divalo=amax1(0.0,divalo)
      divreqX2=divalo
c
c
c               g. Destination is a reservoir
c rrb 99/08/09; I do not think the following is used based on
c               input in oprinp.f
c                    (SOURCE TWO ON RULE 14 CARD IS A RESERVOIR)
c                    LIMIT DEMAND BY AMOUNT IN A  STORAGE ACCOUNT
c rrb 2006/11/21; The following is not documented and should not occurr
c		  Alos iopsou(4,l2) is used in Oprinp.f to turn
c		  ON or OFF the source right
cx      if(iopsou(3,l2).ne.0) then 
cx        IDOW=NOWNER(IOPSOU(3,L2))+IOPSOU(4,L2)-1
cx        DIVOWN1=CUROWN(IDOW)/fac
cx        DIVALO=AMIN1(DIVALO,DIVOWN1)
cx        divalo=amax1(0.0,divalo)
cx        divreqX2=divalo
cx      endif
c
c _________________________________________________________
c
c               Step 8; Set available flow (pavail)
c 
c               FIND DOWNSTREAM MINIMUM FLOW STATION
      CALL DNMFSO(maxsta,AVTEMP,IDNCOD,ISCD,NDNS,IMCD)
      imcdx=imcd
      availax=avtemp(imcdx)
      PAVAIL=AVTEMP(IMCD)
      divaloS=pavail

      IF(iresw.eq.0.and.IRTURN(IUSE).EQ.4) PAVAIL=AMIN1(AVTEMP(IMCD),
     + QSTERN(ND2))
c
c _________________________________________________________
c
c               Step 9; Set actual diversion (divact)
c 
  200 IF(iresw.eq.0.and.IRI.LE.IRE.AND.IRTURN(IUSE).NE.4) GO TO 210
c
c ---------------------------------------------------------
c               9a. Case 1 No return flow adjustment
c                   (irturn=4=transmountain)
c
      DIVACT=AMIN1(PAVAIL,DIVALO)
      
c
c rrb 04/21/96
      divact=amax1(0.0, divact)
c GRB 06-05-95: ADDED CHECK
c     IF (DIVACT.LT.(.01)) GOTO 380
      IF (DIVACT.LT.small) then
        iwhy=8
        cwhy='Available flow (AvailX) is zero'
        GOTO 380
      endif
c
c rrb 01/02/01; Call rtnsec who calls return to calculate CU,
c               reduce IWR and loss (as appropriate)
c rrb 01/02/28; Do not call if destination is a reservoir
c               no cu, loss, etc.
      if(iresw.eq.0) then
        CALL RTNSEC(icx,DIVACT,L2,IUSE,ISCD,nd2,ieff2)
      endif

c
      CALL TAKOUT(maxsta, AVAIL ,RIVER ,AVINP ,QTRIBU,IDNCOD,
     1            DIVACT, NDNS,  ISCD     )
c                        
      IF(iresw.eq.0.and.IRTURN(IUSE).NE.4) GO TO 290
      if (iresw.eq.0) QSTERN(ND2)=QSTERN(ND2)-DIVACT
      GO TO 300
C
  210 IF(PAVAIL.LT.DIVALO) GO TO 220
c
c ---------------------------------------------------------
c
c               9b. Case 2 Can divert 100%
c                   Allowable flow (pavail) > adj demand (divalo)
c
      DIVACT=DIVALO
c
c rrb 04/21/96
      divact=amax1(0.0, divact)
c
c GRB 06-05-95 ADDED CHECK
c     IF (DIVACT.LT.(.01)) GOTO 380
      IF (DIVACT.LT.small) then
        iwhy=9
        cwhy='Available flow (AvailX) is zero'      
        GOTO 380
      endif

      CALL TAKOUT(maxsta, AVAIL ,RIVER ,AVINP ,QTRIBU,IDNCOD,
     1            DIVACT, NDNS,  ISCD    )
c
c rrb 00/12/26; Variable efficiency capability
c     CALL RTNSEC(icx,DIVACT,L2,IUSE,ISCD,nd2) 
      CALL RTNSEC(icx,DIVACT,L2,IUSE,ISCD,nd2,ieff2)
C
      GO TO 290
c
c ---------------------------------------------------------
c
c               9c. Case 3 Limited diversion
c                   Consider immediate return flow
c
  220 continue
      call rtnmax(1, iuse, iri, ire, iscd, ndns, small, pavail,
     1  ieff2, ioutZ, cCallBy, corid1)
      divaloS=pavail
      
      IF(PAVAIL.LT.DIVALO) GO TO 230
c
c ---------------------------------------------------------
c
c               Step 9d. AVAILABLE FLOW IS GREATER ALLOWABLE DIVERSION
c                        with returns
c
      DIVACT=DIVALO
      IF (DIVACT.LT.small) then
        iwhy=10
        cwhy='Available flow (AvailX) is zero'
        GOTO 380
      endif
      CALL TAKOUT(maxsta, AVAIL ,RIVER ,AVINP ,QTRIBU,IDNCOD,
     1            DIVACT, NDNS,  ISCD     )
c
c rrb 00/12/26; Variable efficiency capability
c     CALL RTNSEC(icx,DIVACT,L2,IUSE,ISCD,nd2)
      CALL RTNSEC(icx,DIVACT,L2,IUSE,ISCD,nd2,ieff2)
C
      GO TO 290
C
c
c ---------------------------------------------------------
c
c               Step 9e. Available flow is less than allowable 
c                        diversion with returns

  230 DIVACT=PAVAIL
      IF (DIVACT.LT.small) then
        iwhy=11
        cwhy='Available flow (AvailX) is zero'
        GOTO 380
      endif
      
      CALL TAKOUT(maxsta, AVAIL ,RIVER ,AVINP ,QTRIBU,IDNCOD,
     1            DIVACT, NDNS,  ISCD  )
c
c rrb 00/12/26; Variable efficiency capability
c     CALL RTNSEC(icx,DIVACT,L2,IUSE,ISCD,nd2)
      CALL RTNSEC(icx,DIVACT,L2,IUSE,ISCD,nd2,ieff2)
c
c ---------------------------------------------------------
c rrb 00/12/26;              
c              Step 9f; Check if more can be diverted
c                       by operating at less than maximum efficiency
c
        if(ieffmax.eq.1) then 
          call dnmfso(maxsta, avail ,idncod,iscd  ,ndns  ,imcd)
          imcdx=imcd
          availax=avail(imcdx)    
c
c ---------------------------------------------------------
c
          if(avail(imcd).gt.small) then
c rrb 2009/11/01l Revise to allow some CU to occurr by 
c                 setting iter=2
cx          call rtnmax(2, iuse, iri, ire, iscd, ndns, 
            call rtnmax(1, iuse, iri, ire, iscd, ndns, 
     1        small, pavail, ieff2, ioutZ, cCallBy, corid1)
            divaloS=pavail
c           write(io99,*) '  Divcar1; iteration 2; pavail', pavail*f
c
c rrb 02/06/27; Limit additional diversion to demand, etc.
            divmore=amin1(divalo-divact, pavail)
            pavail=amax1(0.0, divmore)
            divaloS=pavail

            if(pavail.gt.small) then
              CALL TAKOUT(maxsta, AVAIL, RIVER, AVINP,QTRIBU,IDNCOD,
     1                    pavail, NDNS,  ISCD  )
c
c
c rrb 00/12/26; Variable efficiency
c             CALL RTNSEC(icx,pavail,L2,IUSE,ISCD,nd2) 
              CALL RTNSEC(icx,pavail,L2,IUSE,ISCD,nd2,ieff2)
              divaloS=pavail

              divact=divact+pavail
c             write(io99,*) '  Divcar1; iteration 2; divact', divact*f  
            endif  
          endif
        endif
c
      IF(IRTURN(IUSE).LE.3) ISHORT=1
c
c _________________________________________________________
c
c               Step 10.  Update
c

  290 IF(IOPRTN.EQ.0) GO TO 300
C
c               a. Available flow
c      
c rrb 2006/09/28; Not required with 1 structure per station      
cr    TEMP=AVAIL(ISCD)
cr    CURRTN(ISCD)=CURRTN(ISCD)+AMIN1(0.,TEMP)
cr    AVAIL (ISCD)=AMAX1(0.,TEMP)
c
c ---------------------------------------------------------
c               b. Check minimum
  300 CALL DNMFSO(maxsta, AVAIL ,IDNCOD,ISCD  ,NDNS  ,IMCD)
      imcdx=imcd
      availax=avail(imcdx)
      IF(AVAIL(IMCD).lT.(-1.0*small)) then
        WRITE(6,390) IYR,MON,IW,NWRORD(1,IW),L2,IUSE,DIVREQ(IUSE),
     1               ISCD,ISCD,IMCD,DIVACT
        WRITE(6,400) (AVAIL(ISS),ISS=1,NUMSTA)
        WRITE(io99,390) IYR,MON,IW,NWRORD(1,IW),L2,IUSE,DIVREQ(IUSE),
     1               ISCD,ISCD,IMCD,DIVACT
        write(io99,400) (avail(iss),iss=1,numsta)
        write(io99,410) (river(iss),iss=1,numsta)
        goto 9999
      endif
c
      if(iresw.eq.1) then
c
c ---------------------------------------------------------
c               c. Update reservoir data
        divaf=divact*fac
        cursto(nd2)=cursto(nd2)+divaf
        qres(2,nd2)=qres(2,nd2)+divaf
c ---------------------------------------------------------        
c               d. Distribute to accounts
c rrb 2006/09/25; Revised to work with multiple reservoir
c          
c		  Note:
c		   iResT1=0 distributes based on ownership ratio
c		   nrown1=number of accounts in this reservoir
c		   iown = first account associated with this reservoir  
c		   ia   = account to adjust (2=From River by Storage)
c
        nrX=nd2
        iResT1=0
        nrown1=nro
        iownX=irow
        icx=114
        cresid1=cresid(nrX)
        ia=2
        call accou(maxacc, maxown, nrX, ownmon, curown, accr, ia,
     1    ownmax, iownX, nrown1, cursa, divaf, iResT1, icx, cresid1)
     
      endif
c
c ---------------------------------------------------------
c		d. Update destination diversion data      
      if(iresw.eq.0) then
        USEMON(IUSE)=USEMON(IUSE)+DIVACT
        DIVREQ(IUSE)=DIVREQ(IUSE)-DIVACT
      endif
c
c ---------------------------------------------------------
c               f. Update carrier through a structure (qdiv(18,n))
      QDIV(18,ISCD)=QDIV(18,ISCD)+DIVACT
c
c ---------------------------------------------------------
c               g. Update total diversion (qdiv(5,n))
      QDIV(5,ISCD)=QDIV(5,ISCD)+DIVACT
c
c ---------------------------------------------------------
c               h. Update source diversion (divmon)
      DIVMON(ND1)=DIVMON(ND1)+DIVACT
c
c ---------------------------------------------------------
c               i. Update source demand (divreq)
c rrb 99/08/09; Type 14 Edit.  Allow demand to be limited to an
c               annual total adjusted 1x/year in bomsec.for
      if(iopsou(4,l2).eq.1) then
        divreq(iuses)=divreq(iuses)-divact
      else
        divreqa(l2)=divreqa(l2)-divact*fac
      endif
c
c ---------------------------------------------------------
c               j. Update diversion by this water right
      IF (IRIT.EQ.1) DIVD(NDR)=DIVD(NDR)+DIVACT
c
c ---------------------------------------------------------
c               j. Update diversion by this Operating Rule
      DIVO(L2)=DIVO(L2)+DIVACT
c
c ---------------------------------------------------------
c               k. Update intervening Structures
      do 340 i11=1,10
        if (intern(l2,i11).eq.0) go to 350
        intvn=intern(l2,i11)
        divmon(intvn)=divmon(intvn)+divact
        inode=idvsta(INTVN)
        qdiv(18,inode)=qdiv(18,inode)+divact
  340 continue
  350 continue
c
      INODE=IDVSTA(ND2)
c
c                 if the source (nd1) .ne. destiation (nd2)
      if (nd1.ne.nd2) then
c
c               l. Update carrier used by a structure (qdiv(19,x))
c
        if (iresw.eq.0) qdiv(19,inode)=qdiv(19,inode)+divact
c
c               m. Update destination demand ()
        if (iresw.eq.0) DIVMON(ND2)=DIVMON(ND2)+DIVACT
      endif    
 
      IF(iresw.eq.0.and.IRTURN(IUSE).EQ.4) GO TO 360
      GO TO 370
c
c               n. Update carrier to a transmtn diversion (qdiv(8,x))
  360 QDIV(8,ISCD)=QDIV(8,ISCD)+DIVACT
      GO TO 370
  370 continue
c
c               o. Update actual diversion for testing
  380 divactx = divact
c
c _________________________________________________________
c
c               Step 15.  Detalied output
c

      if(iout.eq.1 .and. iw.eq.ioutiw) then      
        ncallx=ncallx+1
        if(ncallx.eq.1) then
          write(nlog,270) corid(l2),cdestyp,ccarry,cpuse
        else
c         write(nlog,*) ' '
        endif  
      
        write(nlog,280) 'DivCar1    ',
     1    iyrmo(mon),xmonam(mon), idy,
     1    cstaid(iscd),iwx, iw,nwrord(1,iw),l2,lr,
     1    ND2,ipuse,imcdX, availX*fac, CapRem2*fac, DIVREQx2*fac, 
     1    divaloS*fac, -1.0, QDIV(18,ISCD)*fac, divactx*fac, 
     1    iwhy, cwhy
      endif
c
c ---------------------------------------------------------
c               c. Check reservoir roundoff when exiting routine
c			Note in1=0 into a routine, 1 out of a routine
c			     isub1 = subroutine calling chekres
      if(iresw.eq.1) then
        in1=1
        isub1 = 14
        nr=nd2      
        call chekres(io99, maxres, in1, isub1, iyr, mon, nr,nowner,
     1                      curown,cursto,cresid)
      endif
  
  
c
c _________________________________________________________
c
c               Step 11.  Return
c 

      RETURN
c
c _________________________________________________________
c
c               Formats
c 
c
c
  270   format(/, 
     1  ' DivCar1(Type 14); Operation Right ID = ', a12,
     1  ' Destination Type = ', a12,
     1  ' Carrier (Y/N) = ',a3, ' Reuse Plan (Y/N) = ', a3/
     1  ' DivCar       iyr mon   day ID          ',
     1  '    Iter      Iw  nwrord      l2      lr     Nd2   ipUse', 
     1  '   imcdX  availX CapRem2  Demand divaloS psuplyT qdiv(18',
     1  ' divactX',
     1  '    iwhy Comment',/
     1  ' ___________ ____ ____ ____ ____________', 
     1  ' _______ _______ _______ _______ _______ _______ _______', 
     1  ' _______ _______ _______ _______ _______ _______ _______',
     1  ' _______',
     1  ' _______ __________________________')
  280   FORMAT(a12, i5,1x,a4, i5, 1x, a12,8i8,7F8.1,i8,1x, a48) 

  390 FORMAT(/, ' divcar1; Print 5 ',6I10,F10.2,3I10,F10.2)
  400 format(' divcar1: avail  ',10f10.2)
  410 format(' divcar1: river  ',10f10.2)
c
c _________________________________________________________
c
c               Error Warnings
c
 9999 write(6,1050)
      write(io99,1051) 
      
 1050 format('    Stopped in Divcar1',/,
     1       '    See the *.log file')
 1051 format('    Stopped in Divcar1')
      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)


      stop 
      END



