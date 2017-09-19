C     Last change:  RRB  21 Feb 100   11:58 am
C
      SUBROUTINE DivcarR(IW,L2,ISHORT,divactx,ncallX)
c
c _________________________________________________________
c	Program Description
c
c       DivCarR; It simulates a type 31 operating rule that allows:
c	         Reusable water to be diverted to a reservoir or 
c                  direct flow using a carrier structure water right.
c       	 Also can constrain a diversion to the capacity 
c                 of up to 10 intervening structures
c _________________________________________________________
c
c       Update History
c
c
c rrb 2006/08/18; Revised to work with multiple reservoir
c rrb 2005/03/29; Copied Divcar. 
c               Minor clean up 
c               Added reuse capability
c		Removed ability for source to be a structure
c _________________________________________________________
c
c               Documentation
c
c 	icx 		subtoutine call # (2)
c       IW         	OVERALL WATER RIGHT ORDER
c       L2         	LOC. OF operation right  in opr RIGHT TABLE
c
c       IDVSTA(L2) 	STATION CODE OF WHERE DIV. RIGHT L2 LOCATES
c
c       ieff2 		0 use always average efficiency
c             		1 use max efficiency if ieffmax = 1
c       iresw 		0 diversion destination
c              		1 reservoir destination
c       ndr             source water right
c       nd1   		source strucutre 
c       nd2   		destination pointer for both a diversion 
c                         and reservoir
c       iresw   		destination pointer
c                	  0 = diversion
c                	  1 = reservoir
c
c       dcrdiv 		decreed amount 
c       divd   		total amount diverted under this decree
c               	(dcrdiv - divd) = decree remaining
c
c       ioprtn 		Switch for handling return flows when
c                	multiple structures exist at 1 location
c                	currently always set to 1.0 in Datinp.f
c
c
c        qdiv(5, )      InBasin diversion by priority
c        qdiv(8, )      Transmountain diversion by priority
c
c        qdiv(18        Carrier passing thru a structure (e.g. divcar)
c        qdiv(20        From Carrier by Storage or Exchange (e.g. carrpl)

c        qdiv(26, )     Diversion by Exchange
c        qdiv(27, )     Diversion to Carry, Exchange or Bypass
c        qdiv(28, )     Stored or diverted via a reuse plan  
c
c _________________________________________________________
c	Dimensions		

      include 'common.inc'
      character 
     1  cwhy*48, cdestyp*12, ccarry*3, cpuse*3, cresid1*12,
     1  cCallBy*12, corid1*12
c
c _________________________________________________________
c               Step 1; Initilize
c
c
c		iout = 0 no details
c		       1 details
c                      2 summary      
      iout=0
      ioutiw=0
      ioutZ=0
      
      cCallBy='DivCarR     '
      corid1=corid(l2)
      
      if(ichk.eq.131) iout=1
      if(corid(l2).eq. ccall) ioutiw=iw
c
      small=0.001
      iwhy=0
      cwhy='N/A'
      cdestyp='N/A'
      ccarry='No'
      cpuse='No'
      availX=-1.
c
      divact=0.0
      divaloS=0.0
      ISHORT=0
c
c 		Use maximum efficiency if ieffmax=1     
      ieff2=1
      icx=11
      lr=l2
c
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif
c
c _____________________________________________________________
c               Step 2; Monthly on/off switch
      if(imonsw(l2,mon).eq.0) then
        iwhy=1
        cwhy='Monthly Switch Off'      
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
c _________________________________________________________
c               Step 3; Set source data (a water right)
      NDr=Iopsou(1,L2)
      ND1=IDIVCO(1,NDR)
c
      if (idivsw(nd1).eq.0) then
        iwhy=2
        cwhy='Source Structure Off'      
        goto 380
      endif  
      
      ISCD=IDVSTA(ND1)
      NDNS=NDNNOD(ISCD)
c
c _________________________________________________________
c
c               Step 4; Set destination data
c rrb 05/06/22; error with assigning nd2 later
      ND2    =Iopdes(1,L2)
      if(nd2.gt.0) iresw=0
      if(nd2.lt.0) iresw=1
c
c _________________________________________________________
c
c               4a. Destination is a reservoir
      if(iresw.eq.1) then
        nd2=-nd2
        cdestyp='Reservoir'
        
        if (iressw(nd2).eq.0) then
          iwhy=3
          cwhy='Destination Reservoir is Off'      
          goto 380
        endif  
c
c ---------------------------------------------------------
c rrb 2006/08/18; Revised to work with multiple reservoir
        nro=1
        if(iopdes(2,l2).lt.0) then
          nro=-iopdes(2,l2)
          irow=nowner(nd2)
        endif

        if (iopdes(2,l2).gt.0) then
          irow=nowner(nd2)+iopdes(2,l2)-1
          nro=1
        endif  
      endif  
c
c _________________________________________________________
c               4b. Destination is a diversion (nd2 is positive)
      if(iresw.eq.0) then
        cdestyp='Diversion'        
        if(idivsw(nd2).eq.0) then
          iwhy=4
          cwhy='Destination diversion is Off'      
          goto 380
        endif  
C
        IUSE=NDUSER(ND2)+Iopdes(2,L2)-1
        
        if(DIVREQ(IUSE).LT.small .OR. 
     1    divcap(nd2)-divmon(nd2).le.small) then
          iwhy=5
          cwhy='Destination capacity is zero'
          goto 380
        endif 
      endif
c      
c ________________________________________________________
c               Step X; Set reuse data
c		ipUse = Reuse plan
      ipUse=ireuse(l2)
      if(ipUse.gt.0) cpuse='Yes'
c
c _________________________________________________________
c
c               Step 5; Set up checks on available flow (avtemp)
c                 ADD RETURN FLOW FROM PREVIOUS WATER RIGHTS
C
C------  ADD RETURN FLOW FROM PREVIOUS WATER RIGHTS
C
      DO IS=1,NUMSTA
        AVTEMP(IS)=AVAIL(IS)
      enddo  
C
c      
c rrb 2006/09/28; Not required with 1 structure per station      
cr    AVTEMP(ISCD)=AVTEMP(ISCD)+CURRTN(ISCD)
cr    availX=avtemp(iscd)
cr    imcdX=iscd
c
c _________________________________________________________
c
c               Step 7; Check available flow at diversion (avtemp(iscd)
c
      IF(AVTEMP(ISCD).lt.small) then
        IF(iresw.eq.0 .and. IRTURN(IUSE).LE.3) ISHORT=1
        iwhy=6
        cwhy='Available flow at source equals zero'
        goto 380
      endif  
C
c
c _________________________________________________________
c
c               Step 8; FIND STARTING AND ENDING RETURN FLOW STATIONS
c
      if(iresw.eq.0) then
        iri=nrtn(iuse)
        ire=nrtn(iuse+1)-1
      else
        iuse=0
        iri=0
        ire=0
      endif
c
c _________________________________________________________
c
c               Step 9; Destination is a Reservoir
c               9a. Destination is a reservoir
c
c rrb 2006/08/18; Revised to work with multiple reservoir
      if(iresw.eq.1) then
        cursa=0
        do n=1, nro
          n1=irow+n-1
          cursa=cursa+(ownmax(n1)-curown(n1))
        end do  
c
c
c rrb 04/09/03; Allow carrier to a reservoir even if the 
c               current storage is above the target
c		when iressw(nd2) = 3 (see *.res documentation)
        if(iressw(nd2).ne.3) then
          divalo=amin1(cursa,volmax(nd2)-cursto(nd2),
     1    tarmax(nd2)-cursto(nd2))/fac
        else
          divalo=amin1(cursa,volmax(nd2)-cursto(nd2))/fac
        endif
        
        divalo=amax1(0.0,divalo)
        divreqx2=divalo        
      endif
c
c _________________________________________________________
c                    9b. Destination is a diversion
c                       Set allowable diversion (divalo)
      if(iresw.eq.0) then
c      
c                    Limit allowable diversion (divalo)
c                    to destination capacity (divcap-divmon)
c		     and remaining water right (dcrdiv-divd)
        DIVALO=AMIN1(DIVREQ(IUSE),DIVCAP(ND2)-DIVMON(ND2),
     1                 DCRDIV(NDR)-DIVD(NDR))
        divalo=amax1(0.0,divalo)
        divreqx2=divalo        
      endif      
c
c _________________________________________________________
c
c               Step 10; Destination is a reservoir or a diversion
c                    Limit to allowable diversion (divalo) &
c                    source capacity (divcap(nd1)-divmon(nd1))
c
      DIVALO=AMIN1(DIVALO,DIVCAP(ND1)-DIVMON(ND1))
c
c
c _________________________________________________________
c
c               Step 11; Limit to intervening structures
c rrb 04/21/96
      divalo=amax1(0.0,divalo)
      do 180 i61=1,10
        if (intern(l2,i61).eq.0) go to 190
        intvn=intern(l2,i61)
        ccarry='Yes'        
        if (divalo.LE.(divcap(intvn)-divmon(intvn))) go to 180 
        divalo=divcap(intvn)-divmon(intvn)
  180 continue
  190 continue
c
c rrb 07/01/96
      divalo=amax1(0.0,divalo)
c
c _________________________________________________________
c
c               Step 12; LIMIT DEMAND BY AMOUNT IN A RESERVOIR 
c                       STORAGE ACCOUNT IF SOURCE TWO IS A RESERVOIR
cr    if(iopsou(3,l2).ne.0) then 
cr      IDOW=NOWNER(IOPSOU(3,L2))+IOPSOU(4,L2)-1
cr
cr      DIVOWN1=CUROWN(IDOW)/fac
cr      DIVALO=AMIN1(DIVALO,DIVOWN1)
cr      divalo=amax1(0.0,divalo)
cr    endif
c
c _________________________________________________________
c
c               Step 13; Set available flow (pavail)
c
c               FIND DOWNSTREAM MINIMUM FLOW STATION
      CALL DNMFSO(maxsta,AVTEMP,IDNCOD,ISCD,NDNS,IMCD)
      availX=avtemp(imcd)
      imcdX=imcd
C
      PAVAIL=AVTEMP(IMCD)
      divaloS=pavail      
c
c _________________________________________________________
c
c               13a. Case 1 No return flow adjustemnt
c                   (irturn=4=transmountain)
c
      DIVACT=AMIN1(PAVAIL,DIVALO)
      divact=amax1(0.0, divact)
      IF (DIVACT.LT.small) then
        iwhy=7
        cwhy='Available downstream flow or demand equals zero'      
        GOTO 380
      endif  
c
      iwhy=-1
      CALL TAKOUT(maxsta, AVAIL ,RIVER ,AVINP ,QTRIBU,IDNCOD,
     1            DIVACT, NDNS,  ISCD     )
c
c rrb 01/02/01; Call rtnsec who calls return to calculate CU,
c               reduce IWR and loss (as appropriate)
cr    if(iresw.eq.0) then
      if(iresw.eq.0 .and. ipUse.eq.0) then
        CALL RTNSEC(icx,DIVACT,L2,IUSE,ISCD,nd2,ieff2)
      endif
      
      IF(iresw.eq.0) then
        if(IRTURN(IUSE).NE.4) GO TO 290
      endif

      if (iresw.eq.0) QSTERN(ND2)=QSTERN(ND2)-DIVACT
      GO TO 300
c
c ---------------------------------------------------------
c
c               13b. Case 2 Can divert 100%
c                   Allowable flow (pavail) > adj demand (divalo)
c
cr210 IF(PAVAIL.LT.DIVALO) GO TO 220
      IF(PAVAIL.ge.DIVALO) then
C
C------  AVAILABLE FLOW IS GREAT THAN ALLOWABLE DIVERSION 
C 
        DIVACT=DIVALO
        divact=amax1(0.0, divact)
        IF (DIVACT.LT.small) then
          iwhy=8
          cwhy='Available flow downstream #2 equals zero'      
          GOTO 380
        endif  
        
        iwhy=-1
        CALL TAKOUT(maxsta, AVAIL ,RIVER ,AVINP ,QTRIBU, idncod,
     1              DIVACT,NDNS,  ISCD     )
c
        if(ipUse.eq.0) then
          CALL RTNSEC(icx,DIVACT,L2,IUSE,ISCD,nd2,ieff2) 
        endif  
        GO TO 290
      endif  
c
c ---------------------------------------------------------
c
c               13c. Case 3 Limited diversion
c                   Consider immediate return flow
cr220 continue
      call rtnmax(1, iuse, iri, ire, iscd, ndns, small, pavail,
     1  ieff2, ioutZ, cCallBy, corid1)
cr    IF(PAVAIL.LT.DIVALO) GO TO 250
      IF(PAVAIL.ge.DIVALO) then
c
c ---------------------------------------------------------
c
c               13d. AVAILABLE FLOW IS GREATER ALLOWABLE 
c                    DIVERSION with returns
        DIVACT=DIVALO
        IF(DIVACT.LT.small) then
          iwhy=9
          cwhy='Available flow downstream #3 equals zero'      
          GOTO 380
        endif  
        
        iwhy=-1
        CALL TAKOUT(maxsta, AVAIL ,RIVER ,AVINP ,QTRIBU, idncod,
     1              DIVACT,NDNS,  ISCD     )
c
        if(ipUse.eq.0) then
          CALL RTNSEC(icx,DIVACT,L2,IUSE,ISCD,nd2,ieff2)
        endif  
        GO TO 290
      endif  
c
c ---------------------------------------------------------
c
c               13e. AVAILABLE FLOW IS less than ALLOWABLE 
c                    DIVERSION with returns

cr250 DIVACT=PAVAIL
      DIVACT=PAVAIL
c     IF(DIVACT.LT.(.01)) GOTO 380
      IF(DIVACT.LT.small) then
        iwhy=10
        cwhy='Available flow downstream #4 equals zero'      
        GOTO 380
      endif 
      
      iwhy=-1 
      CALL TAKOUT(maxsta, AVAIL ,RIVER ,AVINP ,QTRIBU,IDNCOD,
     1            DIVACT, NDNS,  ISCD  )
c
      if(ipUse.eq.0) then
        CALL RTNSEC(icx,DIVACT,L2,IUSE,ISCD,nd2,ieff2)
      endif  
c
c ---------------------------------------------------------
c rrb 00/12/26;              
c              13f. Check if more can be diverted
c                       if operating at less than max efficiency
      if(ieffmax.eq.1) then 
        call dnmfso(maxsta, avail ,idncod,iscd  ,ndns  ,imcd)
c
c ---------------------------------------------------------
c
        if(avail(imcd).gt.small) then
c        
c rrb 2009/11/01l Revise to allow some CU to occurr by 
c                 setting iter=2
cx        call rtnmax(2, iuse, iri, ire, iscd, ndns,        
          call rtnmax(1, iuse, iri, ire, iscd, ndns, 
     1      small, pavail, ieff2, ioutZ, cCallBy, corid1)
c           write(io99,*) '  DivCarR; iteration 2; pavail', pavail*f
c
c rrb 02/06/27; Limit additional diversion to demand, etc.
          divmore=amin1(divalo-divact, pavail)
          pavail=amax1(0.0, divmore)

          if(pavail.gt.small) then
            CALL TAKOUT(maxsta,AVAIL,RIVER,AVINP,QTRIBU,IDNCOD,
     1                  pavail,NDNS, ISCD  )
c
            if(ipUse.eq.0) then
              CALL RTNSEC(icx,pavail,L2,IUSE,ISCD,nd2,ieff2)
            endif  

            divact=divact+pavail
c           write(io99,*) '  DivCarR; iteration 2; divact', divact*f  
          endif  
        endif
      endif

c
      IF(IRTURN(IUSE).LE.3) ISHORT=1
      
c
cr 		ioprtn always = 1 per datinp
cr290 IF(IOPRTN.EQ.0) GO TO 300
  290 continue
c      
c _________________________________________________________
c
c
c		Step X; Calculate reuse   
      if(ipUse.gt.0) then
c
c		Diversion reuse
c		Note return flows are stored in Psuply but
c		not added to river system. Therefore 
c		no need to redivert them from system        
        if(iresw.eq.0) then  
          
          CALL RtnsecR(icx,divact,L2,iuse,ISCD,nd2,
     1         ieff2,ipUse)
c
c rrb 04/12/30; Qdiv(28 is the carried / exchanged water
c		Note works outside river system
          ipsta1=ipsta(ipUse)
          qdiv(28,ipsta1) = psuplyT(ipUse)     
        else
c
c		Reservoir Reuse          
cr        ircp=ipsta(ipUse)
          psuply(ipUse)=psuply(ipUse)+divact
          psuplyT(ipUse)=psuplyT(ipUse)+divact
c
c rrb 2006/01/01; Correction
          if(iplntyp(ipuse).eq.3 .or. iplntyp(ipuse).eq.5) then
            psto2(ipUse)=psto2(ipUse)+divact*fac          
          endif  
          
          ipsta1=ipsta(ipUse)
          qdiv(28,ipsta1) = psuplyT(ipUse)                 
        endif  
      endif

c      
c _________________________________________________________
c
c		Step X; Reset current flow?
c      
c rrb 2006/09/28; Not required with 1 structure per station      
cr    TEMP=AVAIL(ISCD)
cr    CURRTN(ISCD)=CURRTN(ISCD)+AMIN1(0.,TEMP)
cr    AVAIL (ISCD)=AMAX1(0.,TEMP)
c
c      
c _________________________________________________________
c
c               Step X; Check minimum available flow
  300 CALL DNMFSO(maxsta, AVAIL ,IDNCOD,ISCD  ,NDNS  ,IMCD)
      
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
c _________________________________________________________
c
c               Step 14.  Update
c
c ---------------------------------------------------------
c               14c. UPDATE destination reservoir data
      if(iresw.eq.1) then
        divaf=divact*fac
cr      cursto(nd2)=cursto(nd2)+divaf
        qres(2,nd2)=qres(2,nd2)+divaf
c        
c ---------------------------------------------------------        
c               d. Distribute to accounts
c rrb 2006/08/18; Revised to work with multiple reservoir
        nrX=nd2
        iResT1=0
        nrown1=nro
        iownX=irow
        ics=31
        ia=2
        cresid1=cresid(nrX)
c          
        call accou(maxacc, maxown, nrX, ownmon, curown, accr, ia,
     1    ownmax, iownX, nrown1, cursa, divaf, iResT1, icx, cresid1)
      endif
c
c ---------------------------------------------------------
c               14d. UPDATE destination diversion data
      if(iresw.eq.0) then
        USEMON(IUSE)=USEMON(IUSE)+DIVACT
        DIVREQ(IUSE)=DIVREQ(IUSE)-DIVACT
      endif
c
c ---------------------------------------------------------
c               14e. UPDATE carrier (qdiv(18 )
c        
cr330 QDIV(18,ISCD)=QDIV(18,ISCD)+DIVACT
      QDIV(18,ISCD)=QDIV(18,ISCD)+DIVACT
c
c ---------------------------------------------------------
c               14f. Update InBasin diversion by priority qdiv(5 )

      QDIV(5,ISCD)=QDIV(5,ISCD)+DIVACT
c
c ---------------------------------------------------------
c               14g. Update source diversion (divmon)
      DIVMON(ND1)=DIVMON(ND1)+DIVACT
c
c ---------------------------------------------------------
c               14h. Update diversion by this water right
      DIVD(NDR)=DIVD(NDR)+DIVACT
c
c ---------------------------------------------------------
c               14i. Update diversion by this Operating Rule
      DIVO(L2)=DIVO(L2)+DIVACT
c
c ---------------------------------------------------------
c               14j. Update intervening Structures
      do 340 i11=1,10
        if (intern(l2,i11).eq.0) go to 350
        intvn=intern(l2,i11)
        divmon(intvn)=divmon(intvn)+divact
        inode=idvsta(INTVN)
        qdiv(18,inode)=qdiv(18,inode)+divact
  340 continue
  350 continue
c ---------------------------------------------------------
c               14k. UPDATE DESTINATION DEMAND      

      INODE=IDVSTA(ND2)
c
c   grb 1-2-96; Bypass updating of destination demand 
c                 if same as diverting struc.
      if (nd1.ne.nd2) then
c
c ---------------------------------------------------------
c               14l. Update carrier used by a structure (qdiv(19,x))
        if (iresw.eq.0) qdiv(19,inode)=qdiv(19,inode)+divact
c
c ---------------------------------------------------------
c               14m. Update destination demand ()
        if (iresw.eq.0) DIVMON(ND2)=DIVMON(ND2)+DIVACT
      endif    
 
c
c               14n. Update actual diversion for testing
  380 divactx = divact
c
c _________________________________________________________
c
c               Step 15.  Detalied output
c

      if(iout.eq.1 .and. iw.eq.ioutiw) then
        ncallX=ncallX+1
        if(ncallX.eq.1) then
          write(nlog,270) corid(l2), cdestyp, ccarry, cpuse
        else
          write(nlog,*) ' '
        endif  
      
        write(nlog,280) 'DivCarR    ',
     1    iyrmo(mon),xmonam(mon), idy,
     1    cstaid(iscd),iwx, iw,nwrord(1,iw),l2,lr,
     1    ND2,ipuse,imcdX, availX*fac, DIVREQx2*fac, 
     1    divaloS*fac, psuplyT(ipUse)*fac, divactx*fac, iwhy, cwhy
      endif
  280   FORMAT(a12, i5,1x,a4, i5, 1x, a12,8i8,5F8.1,i8,1x, a48)
      
c
c _________________________________________________________
c
c               Step 16.  Return
      RETURN
c
c _________________________________________________________
c
c               Formats
c
  270   format(/, 
     1  ' DivCarR (Type 31); Operation Right ID = ', a12,
     1  ' Destination Type = ', a12,
     1  ' Carrier (Y/N) = ',a3, ' Reuse (Y/N) = ', a3/
     1  ' DivCarR      iyr mon   day ID          ',
     1  '    Iter      Iw  nwrord      l2      lr     Nd2   ipUse', 
     1  '   imcdX  availX divreX2 divaloS psuplyT divactX',
     1  '    iwhy Comment',/
     1  ' ___________ ____ ____ ____ ____________', 
     1  ' _______ _______ _______ _______ _______ _______ _______', 
     1  ' _______ _______ _______ _______ _______ _______',
     1  ' _______ __________________________')
     
  390 FORMAT(/, ' Print 5; DivCarR ',6I10,F10.2,3I10,F10.2)
  400 format(' DivCarR: avail  ',10f10.2)
  410 format(' DivCarR: river  ',10f10.2)
c
c _________________________________________________________
c
c               Error Warnings
c
 9999 write(6,1050)
      write(99,1051) 
      
 1050 format('    Stopped in DivCarR',/,
     1       '    See the *.log file')
 1051 format('    Stopped in DivCarR')
      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)


      stop 
      END



