c
c _________________________________________________________
c	Documentation
c

c
c _________________________________________________________
c	Dimensions
cC     Last change:  RRB  21 Feb 100   11:58 am
C
      SUBROUTINE DIVCAr2(IW,L2,ISHORT,divactx)
c
c _________________________________________________________
c	Program Description
c
c       Divcar2; It handles a type 19 operating rule for
c                 split channles.  Same as divcar except
c                 edits for type 19 (00/02/21 or greater) for changes
c
c _________________________________________________________
c
c       Update History
c
c rrb 00/02/21; Documented
c rrb 98/03/03; Daily capability added
c
c rrb 01/02/01; Call rtnsec who calls return to calculate CU,
c               reduce IWR and loss (as appropriate)
c
c _________________________________________________________
c       Documentation
c
c               IW : OVERALL WATER RIGHT ORDER
c               L2 : LOC. OF operation right  in opr RIGHT TABLE
c
c               IDVSTA(L2) : STATION CODE OF WHERE DIV. RIGHT L2 LOCATES
c               iresw =0 diversion destination
c                      1 reservoir destination
c               nd1     source diversion ID
c               nd2     destination diversion ID
c
c rrb 00/02/23; Type 19 Edit for split channel
c               nd3     split channel location
c               ndr     source water right
c               iscd    stream location of source diversion (nd1)
c
c               iopsou(5,l2) location of split structure
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
c               irow    reservoir owner
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
      character cresid1*12, cwhy*48
c
c _________________________________________________________
c               Step 1; Initilize
c
      iout=0
      if(ichk.eq.119) iout=1

c               a. Convergence
      small=0.001
c
c               b. Daily capability
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif
C
c               c. Miscellaneous
      divact=0.0
      ISHORT=0
c
c rrb 00/12/26; Variable efficiency capability set to 1 (on)
      ieff2=1
c
c               Call number
      icx=8
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
c               Step 2; Set source data
c _________________________________________________________
c
c               a. Source structure (nd1)
c                   - negative is a water right
c                   - positive is a structure
c                  Source location (iscd)
c                  Number of downstream nodes (ndns)
      ND1  =Iopsou(1,L2)
C
C------  FIND TYPE OF SOURCE NODE (STRUCTURE OR WATER RIGHT)
C  NEGATIVE SOURCE INDICATES WATER RIGHT, POSITIVE INDICATES STRUCTURE
      IRIT=0
      IF (ND1.GT.0) GOTO 100
      IRIT=1 
      NDR=-ND1
      ND1=IDIVCO(1,NDR)
  100 if (idivsw(nd1).eq.0) goto 380
c
c rrb 00/02/21; Type 19 Edit for split channel
c               Water supply is calculated at source (split location)
      if(ityopr(l2).ne.19) then
        ISCD=IDVSTA(ND1)
        NDNS=NDNNOD(ISCD)
      else
        nd3=iopsou(5,l2)
        iscd=idvsta(nd3)
        ndns=ndnnod(iscd)
      endif
c
c               Step 3; Set destination data
c _________________________________________________________
c
c               a. Destination (nd2)
c                   - negative is a reservoir
c                   - positive is a structure
      ND2  =Iopdes(1,L2)
      iresw=0
      if (nd2.gt.0) go to 110
c
c               b. Destination is a reservoir (nd2 is negative)
      iresw=1
      nd2=-nd2
      if (iressw(nd2).eq.0) goto 380
c
c rrb 2006/08/18; Revised to work with multiple reservoir      
      nro=1
      if (iopdes(2,l2).lt.0) then
        nro=-iopdes(2,l2)
        irow=nowner(nd2)
      endif

      if (iopdes(2,l2).gt.0) then
        irow=nowner(nd2)+iopdes(2,l2)-1
        nro=1
      endif  
      go to 120
c
c               c. Destination is a diversion (nd2 is positive)
  110 if(idivsw(nd2).eq.0) goto 380
C
      IUSE=NDUSER(ND2)+Iopdes(2,L2)-1
C
c
c               Step 4; Check for demand (divreq) and
c                       capacity limit (divcap-divmon .le. small)
c _________________________________________________________
C grb do not limit transfer amount by decree
c 120 IF(iresw.eq.0.and.DIVREQ(IUSE).LT.0.00001.OR.
c    1   divcap(nd2)-divmon(nd2).le.0.00001) goto 380
c 
c rrb 00/01/24; Test
c 120 IF(iresw.eq.0.and.DIVREQ(IUSE).LT.small .OR.

  120 continue
c     write(io99,*) '  Divcar; iresw, nd2, iuse', iresw,nd2,iuse
      IF(iresw.eq.0.and.DIVREQ(IUSE).LT.small .OR.  
     1   divcap(nd2)-divmon(nd2).le.small) goto 380
c
c               Step 5; Set up checks on available flow (avtemp)
c                 ADD RETURN FLOW FROM PREVIOUS WATER RIGHTS
c _________________________________________________________

C
C------  ADD RETURN FLOW FROM PREVIOUS WATER RIGHTS
C
      DO 130 IS=1,NUMSTA
  130 AVTEMP(IS)=AVAIL(IS)
C
      IF(iresw.eq.0.and.(IOPRTN.EQ.0.OR.IRTURN(IUSE).EQ.4)) GO TO 140
C
c      
c rrb 2006/09/28; Not required with 1 structure per station      
cr    AVTEMP(ISCD)=AVTEMP(ISCD)+CURRTN(ISCD)
c
c
c               Step 6; Check available flow at diversin (avtemp(iscd)
c _________________________________________________________

C
C------  CHECK AVAILABLE WATER AT CURRENT STATION
C
c 140 IF(AVTEMP(ISCD).GT.0.00001) GO TO 150
  140 IF(AVTEMP(ISCD).GT.small) GO TO 150
C
      IF(iresw.eq.0.and.IRTURN(IUSE).LE.3) ISHORT=1
C
c     RETURN
      goto 380
C
C------  FIND STARTING AND ENDING INDEX OF RETURN FLOW STATIONS
C
  150 if(iresw.eq.0) IRI=NRTN(IUSE)
      if (iresw.eq.0) IRE=NRTN(IUSE+1)-1
c
c
c               Step 7; Set allowable diversion (divalo)
c _________________________________________________________
c grb 06-05-95 added ability to look at multiple reservoir accounts
      if (iresw.eq.1) then
c
c               a. Destination is a reservoir
c                    Limit to remaining res. capacity (volmax-cursto)
c
c rrb 2006/08/18; Revised to work with multiple reservoir
        cursa=0
        do 160 n=1, nro
          n1=irow+n-1
          cursa=cursa+(ownmax(n1)-curown(n1))
  160   continue
c
c rrb 98/03/03; Daily capability
c       divalo=amin1(cursa,volmax(nd2)-cursto(nd2),
c    1  tarmax(nd2)-cursto(nd2))/mthday(mon)/factor
        divalo=amin1(cursa,volmax(nd2)-cursto(nd2),
     1  tarmax(nd2)-cursto(nd2))/fac
c
c rrb 04/21/96
         divalo=amax1(0.0,divalo)
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
c
c rrb 04/21/96
         divalo=amax1(0.0,divalo)
c
c               d. Destination is a reservoir or a diversion
c                    Limit to allowable diversion (divalo) &
c                    source capacity (divcap(nd1)-divmon(nd1))

  170 DIVALO=AMIN1(DIVALO,DIVCAP(ND1)-DIVMON(ND1))
c
c rrb 04/21/96
      divalo=amax1(0.0,divalo)
      do 180 i61=1,10
        if (intern(l2,i61).eq.0) go to 190
        intvn=intern(l2,i61)
        if (divalo.LE.(divcap(intvn)-divmon(intvn))) go to 180 
        divalo=divcap(intvn)-divmon(intvn)
  180 continue
  190 continue
c
c rrb 07/01/96
         divalo=amax1(0.0,divalo)
    
C------  ALSO LIMIT DEMAND BY AMOUNT IN A RESERVOIR STORAGE ACCOUNT
C        IF SOURCE TWO ON RULE 11 CARD IS A RESERVOIR
         if (iopsou(3,l2).ne.0) then 
           IDOW=NOWNER(IOPSOU(3,L2))+IOPSOU(4,L2)-1
c
c rrb 98/03/03; Daily capability
c          DIVOWN1=CUROWN(IDOW)/(MTHDAY(MON)*FACTOR)
           DIVOWN1=CUROWN(IDOW)/fac
           DIVALO=AMIN1(DIVALO,DIVOWN1)
c
c rrb 04/21/96
         divalo=amax1(0.0,divalo)
         endif
c
c
c               Step 8; Set available flow (pavail)
c _________________________________________________________
c               FIND DOWNSTREAM MINIMUM FLOW STATION
      CALL DNMFSO(AVTEMP,IDNCOD,ISCD,NDNS,IMCD)
C
      PAVAIL=AVTEMP(IMCD)
      IF(iresw.eq.0.and.IRTURN(IUSE).EQ.4) PAVAIL=AMIN1(AVTEMP(IMCD),
     + QSTERN(ND2))
c
c
c               Step 9; Set actual diversion (divact)
c _________________________________________________________
c
  200 IF(iresw.eq.0.and.IRI.LE.IRE.AND.IRTURN(IUSE).NE.4) GO TO 210
c
c               a. Case 1 No return flow asjustemnt
c                  (irturn=4=transmountain)
c _________________________________________________________
c
      DIVACT=AMIN1(PAVAIL,DIVALO)
c
c rrb 04/21/96
      divact=amax1(0.0, divact)
C GRB 06-05-95 ADDED CHECK
c     IF (DIVACT.LT.(.01)) GOTO 380
      IF (DIVACT.LT.small) GOTO 380
c
c rrb 01/02/01; Call rtnsec who calls return to calculate CU,
c               reduce IWR and loss (as appropriate)
c rrb 01/02/28; Do not call if a reservoir since no cu, loss, etc.
      if(iresw.eq.0) then
        CALL RTNSEC(icx,DIVACT,L2,IUSE,ISCD,nd2,ieff2) 
      endif
c
      CALL TAKOUT(AVAIL ,RIVER ,AVINP ,QTRIBU,IDNCOD,DIVACT,NDNS,
     1            ISCD     )
C                        
      IF(iresw.eq.0.and.IRTURN(IUSE).NE.4) GO TO 290
      if (iresw.eq.0) QSTERN(ND2)=QSTERN(ND2)-DIVACT
      GO TO 300
c
c               b. Case 2 Can divert 100%
c                  Allowable flow (pavail) > adj demand (divalo)
c _________________________________________________________
c
  210 IF(PAVAIL.LT.DIVALO) GO TO 220
C
C------  AVAILABLE FLOW IS GREAT THAN ALLOWABLE DIVERSION 
C 
      DIVACT=DIVALO
c
c rrb 04/21/96
      divact=amax1(0.0, divact)
C GRB 06-05-95 ADDED CHECK
c     IF (DIVACT.LT.(.01)) GOTO 380
      IF (DIVACT.LT.small) GOTO 380

      CALL TAKOUT(AVAIL ,RIVER ,AVINP ,QTRIBU,IDNCOD,DIVACT,NDNS,
     1            ISCD     )
c
c rrb 98/12/30; Wells, Caution I think nd1 is correct, but ...
c     CALL RTNSEC(DIVACT,L2,IUSE,ISCD)
      CALL RTNSEC(icx,DIVACT,L2,IUSE,ISCD,nd2,ieff2)
      GO TO 290
c
c               c. Case 3 Limited diversion
c                  Consider immediate return flow
c _________________________________________________________
c
  220 FORET=1.0-DIVEFF(mon,IUSE)/100.
C
      DO 230 IS=1,NUMSTA
  230 AVWRET(IS)=0.
c
c               STEP THROUGH RETURN FLOWS FOR CURRENT DIVERSION.
      DO 250 IRT=IRI,IRE
        IRCD =IRNSTA(IRT)
        NDNR =NDNNOD(IRCD)
C
c rrb 05/28/98; allow return id to not be the array counter
c       IDL=IRTNDL(IRT)
c       IDLY=IDLORD(IDL)
        idly=irtndl(irt)
c
c               CHECK DELAY TYPE
c rrb 99/03/03; Daily model temporarily divide by days per month (imd)
c       FACDLY=DLYRAT(1,IDLY)
        if(iday.eq.0) then
          FACDLY=DLYRAT(1,IDLY)
        else
          facdly=dlyratd(1,idly)
        endif

        RET=PCTTOT(IRT)*FACDLY/10000.
        ISS=IRCD
        DO 240 NS=1,NDNR
          AVWRET(ISS)=AVWRET(ISS)+RET
  240   ISS=IDNCOD(ISS)
  250 CONTINUE
c
c               COMPUTE THE ALLOWABLE AMOUNT FOR THE CURRENT DIVERSION
      ISS=ISCD
      DO 270 NS=1,NDNS
c       IF(ABS(AVWRET(ISS)*FORET-1.).LE.0.00001) GO TO 260
        IF(ABS(AVWRET(ISS)*FORET-1.).LE.small) GO TO 260
         AVWRET(ISS)=AVTEMP(ISS)/(1.0-AVWRET(ISS)*FORET)
        GO TO 270      
  260   AVWRET(ISS)=1.0E10
  270 ISS=IDNCOD(ISS)
      AVWRET(ISCD)=AVTEMP(ISCD)
c
c               FIND THE UPDATED DOWNSTREAM MIN FLOW NODE
      CALL DNMFSO(AVWRET,IDNCOD,ISCD,NDNS,IMCD)
C
      PAVAIL=AVWRET(IMCD)
C
      IF(PAVAIL.LT.DIVALO) GO TO 280
c
c               AVAILABLE FLOW IS GREATER ALLOWABLE DIVERSION
      DIVACT=DIVALO
c     IF (DIVACT.LT.(.01)) GOTO 380
      IF (DIVACT.LT.small) GOTO 380

      CALL TAKOUT(AVAIL ,RIVER ,AVINP ,QTRIBU,IDNCOD,DIVACT,NDNS,
     1            ISCD     )
c
c rrb 98/12/30; Wells, caution I think nd1 is correct, but ..
c     CALL RTNSEC(DIVACT,L2,IUSE,ISCD)
      CALL RTNSEC(icx,DIVACT,L2,IUSE,ISCD,nd2,ieff2)
      GO TO 290
C
  280 DIVACT=PAVAIL
c     IF (DIVACT.LT.(.01)) GOTO 380
      IF (DIVACT.LT.small) GOTO 380
      CALL TAKOUT(AVAIL ,RIVER ,AVINP ,QTRIBU,IDNCOD,DIVACT,NDNS,
     1            ISCD  )
c
c rrb 98/12/30; Wells, Caution I think nd1 is correct but ...
c     CALL RTNSEC(DIVACT,L2,IUSE,ISCD)
      CALL RTNSEC(icx,DIVACT,L2,IUSE,ISCD,nd2,ieff2)
C
      IF(IRTURN(IUSE).LE.3) ISHORT=1
c
c               Step 10.  Update
c _________________________________________________________

  290 IF(IOPRTN.EQ.0) GO TO 300
C
c ---------------------------------------------------------        
c               a. Available flow
c      
c rrb 2006/09/28; Not required with 1 structure per station      
cr    TEMP=AVAIL(ISCD)
cr    CURRTN(ISCD)=CURRTN(ISCD)+AMIN1(0.,TEMP)
cr    AVAIL (ISCD)=AMAX1(0.,TEMP)
c
c
c ---------------------------------------------------------        
c               b. Check minimum
  300 CALL DNMFSO(AVAIL ,IDNCOD,ISCD  ,NDNS  ,IMCD  )
c     IF(AVAIL(IMCD).lT.-.01) then
      IF(AVAIL(IMCD).lT.(-1.0*small)) then
        WRITE(6,390) IYR,MON,IW,NWRORD(1,IW),L2,IUSE,DIVREQ(IUSE),
     1               ISCD,ISCD,IMCD,DIVACT
        WRITE(6,400) (AVAIL(ISS),ISS=1,NUMSTA)
c
c rrb 11/15/94; Additional output
        WRITE(99,390) IYR,MON,IW,NWRORD(1,IW),L2,IUSE,DIVREQ(IUSE),
     1               ISCD,ISCD,IMCD,DIVACT
        write(99,400) (avail(iss),iss=1,numsta)
        write(99,410) (river(iss),iss=1,numsta)
        goto 9999
      endif
c
c ---------------------------------------------------------        
c               c. Update reservoir data
      if(iresw.eq.1) then
        divaf=divact*fac
        cursto(nd2)=cursto(nd2)+divaf
        qres(2,nd2)=qres(2,nd2)+divaf
c rrb 2006/08/18; Revised to work with multiple reservoir
c
c
c        
c ---------------------------------------------------------        
c               d. Distribute to accounts
c rrb 2006/08/18; Revised to work with multiple reservoir
c		   ia   = account to adjust (2=From River by Storage)

cx        do 320 n=1,nro
cx          n1=irow+n-1
cx          accr(2,n1)=accr(2,n1)+(divaf*(ownmax(n1)-curown(n1))/cursa)
cx          curown(n1)=curown(n1)+(divaf*(ownmax(n1)-curown(n1))/cursa)
cx320   continue
        
        if(nro.eq.1) then
          n1=irow       
          accr(2,n1)=accr(2,n1)+divaf
          curown(n1)=curown(n1)+divaf
        else
          nr=nd2
          iResT1=0
          nrown1=nro
          iown=irow
          icx=119
          ia=2
          cresid1=cresid(nr)
          call accou(maxacc, maxown, nr, ownmon, curown, accr, ia,
     1      ownmax, iown, nrown1, cursa, divaf, iResT1, icx, cresid1)
        endif
        go to 330
      endif
c
c ---------------------------------------------------------        
c               d. Update destination use (usemon)
      USEMON(IUSE)=USEMON(IUSE)+DIVACT
c
c ---------------------------------------------------------        
c               e. Update destination demand (divreq)
      DIVREQ(IUSE)=DIVREQ(IUSE)-DIVACT
c
c
c ---------------------------------------------------------        
c               f. Update carrier through a structure (qdiv(18,n))
  330 QDIV(18,ISCD)=QDIV(18,ISCD)+DIVACT
c
c ---------------------------------------------------------        
c               g. Update total diversion (qdiv(5,n))

      QDIV(5,ISCD)=QDIV(5,ISCD)+DIVACT
c
c ---------------------------------------------------------        
c               h. Update source diversion (divmon)
      DIVMON(ND1)=DIVMON(ND1)+DIVACT
c
c               i. Update diversion by this water right
      IF (IRIT.EQ.1) DIVD(NDR)=DIVD(NDR)+DIVACT
c
c               j. Update diversion by this Operating Rule
      DIVO(L2)=DIVO(L2)+DIVACT
c
c               k. Update intervening Structures
      do 340 i11=1,10
        if (intern(l2,i11).eq.0) go to 350
        intvn=intern(l2,i11)
        divmon(intvn)=divmon(intvn)+divact
        inode=idvsta(INTVN)
        qdiv(18,inode)=qdiv(18,inode)+divact
  340 continue
  350 continue

c               UPDATE DESTINATION DEMAND      

      INODE=IDVSTA(ND2)
c
c   grb 1-2-96; Bypass updaing of destination demand 
c                 if same as diverting struc.
      if (nd1.ne.nd2) then
c
c               l. Update carrier used by a structure (qdiv(19,x))
        if (iresw.eq.0) qdiv(19,inode)=qdiv(19,inode)+divact
c
c               m. Update destination demand ()
        if (iresw.eq.0) DIVMON(ND2)=DIVMON(ND2)+DIVACT
      endif
c
c rrb 00/02/21;  Type 19 Edit.  Update carrier used by a
c                structure (qdiv(19,x)), 
      if (ityopr(l2).eq.19 .and. iresw.eq.0) then
        inode=idvsta(nd2)
        write(io99,*) '  Divcar2; nd2, inode = ', nd2, inode
        qdiv(19,inode)=qdiv(19,inode)+divact
      endif     
 
      IF(iresw.eq.0.and.IRTURN(IUSE).EQ.4) GO TO 360
      GO TO 370
c
c               n. Update carrier to a transmtn diversion (qdiv(8,x))
  360 QDIV(8,ISCD)=QDIV(8,ISCD)+DIVACT
      GO TO 370
  370 continue
C
c
c               o. Update actual diversion for testing
  380 divactx = divact
c     divo(l2)=divo(l2)+divact
c
c               Step 11.  Return
c _________________________________________________________

      RETURN
c
c               Formats
c _________________________________________________________
  390 FORMAT(/, ' Print 5; DIVCAR2 ',6I10,F10.2,3I10,F10.2)
  400 format(' divcar2: avail  ',10f10.2)
  410 format(' divcar2: river  ',10f10.2)
c
c               Error Warnings
c _________________________________________________________
 9999 write(6,1050)
      write(99,1051) 
      
 1050 format('    Stopped in Divcar2',/,
     1       '    See the *.log file')
 1051 format('    Stopped in Divcar2')
      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)


      stop 
      END



