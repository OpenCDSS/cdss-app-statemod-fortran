c
c _________________________________________________________
c	Documentation
c

c
c _________________________________________________________
c	Dimensions
cc     Last change:  RRB  18 Dec 100    2:29 pm
c
      SUBROUTINE DivRigS(IW,L2,ISHORT,ncallx)
c
c _________________________________________________________
c	Program Description
c
c	DivRigS; Type 36
c		It calculates a diversion with seasonal limitations
c		Note it is only required if some rights have seasonal
c		limitations but others do not. If all rights have
c		a seasonal limitation then seasonal limits can 
c		often be contained in the demand file.
c
c _________________________________________________________
c	Update History
c
c rrb 06/01/18; Copy divrig. Added daily on/off capability
c
c_____________________________________________________________
c	 Documentation
c
c        icx            subroutine call # (5)
c        IW             Global water right ID
c        L2             Operational right pointer
c
c	 ndr            Source water right pointer
c	 nd1            Source diversion pointer
c
c        nd             Destination structure pointer
c        iuse           Diversion user
c
c	       nr             Reservoir pointer (0)
c
c        ishort         code for reoperation; 0=no, 1=yes
c
c        divreq         Diversion demand for types 1-3 (standard)
c
c        divsw          SW demand for demand types 4 & 5 
c
c        divreqx        Set to divreq or divsw based on demand type
c
c        dcrdiv         Decree (cfs)
c        divd           Decree diverted in previous iterations
c        divcap         Structure capacity
c        divmon         Amount diverted in previous iterations
c
c        idvsta(l2)     STATION WHERE DIV. RIGHT L2 LOCATES
c
c        ieff2         =0 always use average efficiency
c                      =1 let ieffmax control variable efficiency 
c
c        imd            Number of days this month from Execut.for
c        ioprtn
c        iout           Switch: 0 no print; 1 yes print
c        idvsta(nd)     Diversion station
c        iscd           Diversion station (iscd = idvsta(nd))
c
c        ndnnod(iscd)   Number of downstream nodes
c        ndns           Number of downstream nodes from diversion
c                       (ndns=ndnnod(iscd))
c        ndnr           Number of downstream nodes from return
c                       (ndnr = f (ndnnod)) 
c
c
c        qdiv(5, )      InBasin diversion by priority
c        qdiv(8, )      Transmountain diversion by priority
c
c        currtn         Immediate return to diverting node??
c        qtribu         Tributary Flow ???
c        qstern         ???
c        small          a small value for roundoff (0.0) concerns
c
c        DivownP(n)     Ownership fraction
c	 DivnamO(n)     Owner name
c	 NdOwn(n)       Pointer to ID of owner
c	 DivOwn(n)      Diversion by owner n
c
c	 imcd1          Call information
c
c _____________________________________________________________
c	Dimensions
      include 'common.inc'
      character ctype1*12
      character 
     1  cwhy*45, cdestyp*12, ccarry*3, cpuse*3, 
     1  cCallBy*12, corid1*12
      
c_____________________________________________________________
c               Step 1; Common Initilization
c		iout = 0 no details
c		       1 details
c                      2 summary      
      iout=0
      ioutiw=0
      ioutZ=0
      
      cCallBy='DivRigS     '
      corid1=corid(l2)
      
      if(ichk.eq.136) iout=2
      if(corid(l2).eq. ccall) ioutiw=iw

      divact = 0.0
      divalo = 0.0
c     availd = availd
      ISHORT=0
      imcd1=0
      
c
c 		Use maximum efficiency if ieffmax=1     
      ieff2=1
      
      icx=36
      f = mthday(mon) * factor
      small = 0.001
c
c rrb 2006/01/18            
      iwhy=0
      cwhy='NA'

      cpuse='No '
      ccarry='No '
      cdestyp='Diversion'      
      if (intern(l2,1).ne.0) ccarry='Yes'
      iuse=0
      nr=0
      pavail=-1/f
c
c		Set source water right (ndr), its location (iscd),
c               and destination (nd) here so it can be printed
c		even if off this month (day)      
      Ndr  =Iopsou(1,L2)
      ND1=IDIVCO(1,NDR)
      iscd=idvsta(nd1)
      
      ND   =Iopdes(1,L2)    
      
      imcd=-1
      divreqX=-1.0/f
      dcrdivX=-1.0/f
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
      
c
c ---------------------------------------------------------
c               Step 1a; Set destination data
c
c
      if(idivsw(nd).eq.0) then
        iwhy=3
        cwhy='Destination is off'
        goto 260
      endif  
c
c rrb 2006/01/19; revise to use diversion water right pointer
c     IUSE=NDUSER(ND)+IDIVCO(2,L2)-1
      IUSE=NDUSER(ND)+IDIVCO(2,ndr)-1

c
c ---------------------------------------------------------
c               Step 1b; Set Source data
c
      NDNS=NDNNOD(ISCD)
c
      ioutX=0
      IF(ioutX.eq.99) then
        write(nlog,100)  iyrmo(mon), xmonam(mon), nd, ndr, iuse,iopout,
     1                   iscd, idemtyp, irturn(iuse)
 100    format(
     1    /, 80('_'), /,
     1    '  Divrig;     iyr     mon      nd     ndr    iuse  iopout',
     1             '    iscd idemtyp  irturn',/,
     1   9x, i8, 4x, a4, 20i8)
      endif

c
c ---------------------------------------------------------
c               Step 1c; Set running diversion based on demand type
c
      if(idemtyp.le.3) then
        divreqx=divreq(iuse)
      else
        divreqx=divsw(iuse)
      endif
c
c ---------------------------------------------------------
c               Step 1d; Set remaining decree amount
      dcrdivX=dcrdiv(ndr)-divd(ndr)
c
c ---------------------------------------------------------
c               Step 1e; Detailed printout
c
      if(iout.ge.1 .and. iw.eq.ioutiw) then
        ncallX=ncallX+1
        if(ncallX.eq.1)then
          write(nlog,270) corid(l2),cdestyp,ccarry,cpuse
        else
c          write(nlog,*) ' '
        endif  
        
        write(99,280)' DivRigS_In  ', 
     1    iyrmo(mon),xmonam(mon),idy,imonsw(l2,mon),
     1    cstaid(iscd), iwx, iw,nwrord(1,iw),l2,nr,
     1    -1,            ND,IUSE,DIVREQx*f,
     1    AVAIL(ISCD)*f,CURRTN(ISCD)*f,DIVCAP(ND)*f, DIVMON(ND)*f, 
     1    dcrdivX*f, divd(ndr)*f 
      endif

c
c_____________________________________________________________
c               Step 2; Check demand and return if zero
c
      
c rrb 2006/01/19; Revise for an operating rule
c     if(divreqx.lt.small.or.(dcrdiv(l2)-divd(l2)).le.small.or.
      if(divreqx.lt.small.or.(dcrdiv(ndr)-divd(ndr)).le.small.or.
     1   divcap(nd)-divmon(nd).le.small) then
        if(divreqx.lt.small) then
          iwhy=4
          cwhy='Demand is zero'
          goto 260
        endif  
c
c rrb 2006/01/19; Revise for an operating rule        
c       if((dcrdiv(l2)-divd(l2)).le.small) then
        if((dcrdiv(ndr)-divd(ndr)).le.small) then
          iwhy=5
          cwhy='Remaining decree is zero'
          goto 260
        endif  
        
        if(divcap(nd)-divmon(nd).lt.small) then
          iwhy=6
          cwhy='Remaining capacity is zero'
          goto 260
        endif  
      endif  
c
c_____________________________________________________________
c               Step 3; Begin generic water supply checks
c


      DO IS=1,NUMSTA
        AVTEMP(IS)=AVAIL(IS)
      end do
c
      IF(IOPRTN.EQ.0.OR.IRTURN(IUSE).EQ.4) GO TO 110
c
c               ADD RETURN FLOW FROM PREVIOUS WATER RIGHTS   
      AVTEMP(ISCD)=AVTEMP(ISCD)+CURRTN(ISCD)
c
c               CHECK AVAILABLE WATER AT CURRENT STATION  
c
c               Case 1 No water available at headgate location
c ------------------------------------------------------    
c
  110 continue

      if(avtemp(iscd).le.small) then
        IF(IRTURN(IUSE).LE.3) ISHORT=1
        iwhy=7
        cwhy='Avail is zero'
        goto 260
      endif
c
c               FIND STARTING AND ENDING INDEX OF RETURN FLOW STATIONS
      iri=nrtn(iuse)
      IRE=NRTN(IUSE+1)-1
c
c_____________________________________________________________
c               Step 4; Calculate allowable diversion (divalo)
c
c rrb 2005/01/06; Revise for an operating rule
c      divalo=amin1(dcrdiv(l2)-divd(l2),divreqx, 
      divalo=amin1(dcrdiv(ndr)-divd(ndr),divreqx, 
     1       divcap(nd)-divmon(nd))
      divalo=amax1(0.0,divalo)

      IF(-IOPOUT.eq.ISCD .or. iout.eq.1) then
        write(nlog,*) ' Divrig; Step 4 divalo = ', divalo*f
      endif
c
c_____________________________________________________________
c               Step 5; Find mininum downstream station
c

      CALL DNMFSO(maxsta,AVTEMP,IDNCOD,ISCD,NDNS,IMCD)
c
      PAVAIL=AVTEMP(IMCD)

      IF(-IOPOUT.eq.ISCD .or. iout.eq.1) then
        write(nlog,*) ' Divrig; Step 5a pavail = ', pavail*f,
     1             ' qtribu = ', qtribu(iscd), ' qstern ', qstern(nd)
      endif

      IF(IRTURN(IUSE).EQ.4) PAVAIL=AMIN1(AVTEMP(IMCD),QSTERN(ND))
      pavail=amax1(0.0,pavail)

      IF(-IOPOUT.eq.ISCD .or. iout.eq.1) then
        write(nlog,*) ' Divrig; Step 5b pavail = ', pavail*f
      endif

c
c_____________________________________________________________
c               Step 6; Determine diversion 
c
c
c ---------------------------------------------------------
c               Step 6a 
c               Case 2 No return flow adjustment possible 
c                 (irturn=4=transmountain) or no returns (iri<ire)
c
      if(irturn(iuse).eq.4) then
        
        DIVACT=AMIN1(PAVAIL,DIVALO)
        divact=amax1(0.0,divact)
c
c rrb 01/02/01; Call rtnsec who calls return to calculate CU,
c               reduce IWR and loss (as appropriate)
        CALL RTNSEC(icx,DIVACT,L2,IUSE,ISCD,nd,ieff2)

        CALL TAKOUT(maxsta, AVAIL ,RIVER ,AVINP ,QTRIBU,IDNCOD,
     1              DIVACT, NDNS,  ISCD    )
        IF(IRTURN(IUSE).NE.4) GO TO 220
        QSTERN(ND)=QSTERN(ND)-DIVACT
        GO TO 230
      endif
c
c ---------------------------------------------------------
c               Step 6b
c               Case 3 Can divert 100% w/o any return flow additions
c                 since (divalo) > available (pavail)
c
      if(pavail.ge.divalo) then 
        DIVACT=DIVALO

        IF(-IOPOUT.eq.ISCD .or. iout.eq.1) then
          write(nlog,*) ' Divrig; Step 6b divact = ', divact*f
        endif

        CALL TAKOUT(maxsta, AVAIL ,RIVER ,AVINP ,QTRIBU,IDNCOD,
     1              DIVACT, NDNS,  ISCD    )
c
c rrb 00/12/26; Variable efficiency
c       CALL RTNSEC(icx,DIVACT,L2,IUSE,ISCD,nd) 
        CALL RTNSEC(icx,DIVACT,L2,IUSE,ISCD,nd,ieff2)
        GO TO 220
      endif
c
c ---------------------------------------------------------
c               Step 6c
c               Case 3 Limited diversion, consider return flows
c
      call rtnmax(1, iuse, iri, ire, iscd, ndns, small, pavail,
     1  ieff2, ioutZ, cCallBy, corid1)

        IF(-IOPOUT.eq.ISCD .or. iout.eq.1) then
          write(nlog,*) ' Divrig; Step 6c pavail = ', pavail*f
        endif

c     if(l2.eq.1258) then
c       write(nlog,*) ' Divrig; l2, ieffmax, pavail,divalo', 
c    1                   l2, ieffmax, pavail*f,divalo*f 
c     endif
c
c ---------------------------------------------------------
c               Step 6d
c               Case 4a Divert 100% with return flow addition
c
      if(pavail.ge.divalo) then    
        DIVACT=DIVALO

        IF(-IOPOUT.eq.ISCD .or. iout.eq.1) then
          write(nlog,*) ' Divrig; Step 6d divact = ', divact*f
        endif

        CALL TAKOUT(maxsta, AVAIL ,RIVER ,AVINP ,QTRIBU,IDNCOD,
     1              DIVACT, NDNS,  ISCD  )                     
c
        CALL RTNSEC(icx,DIVACT,L2,IUSE,ISCD,nd,ieff2)
        GO TO 220
      endif
c
c ---------------------------------------------------------
c               Step 6e
c               Case 4B Divert < 100% with return flow additions
c
      if(pavail.lt.divalo) then
c 210   DIVACT=PAVAIL
        divact=pavail

        IF(-IOPOUT.eq.ISCD .or. iout.eq.1) then
          write(nlog,*) ' Divrig; Step 6e divact = ', divact*f
        endif

        if(divact.lt.small) then
          iwhy=8
          cwhy='Diversion is zero'
          goto 260
        endif  

c       if(l2.eq.1258) then
c         write(nlog,*) ' Divrig iteration 1; l2, divact',l2,divact*f  
c       endif

        CALL TAKOUT(maxsta, AVAIL ,RIVER ,AVINP ,QTRIBU,IDNCOD,
     1              DIVACT, NDNS,  ISCD  )
c
c rrb 00/12/26; Variable efficiency
c       CALL RTNSEC(icx,DIVACT,L2,IUSE,ISCD,nd)
        CALL RTNSEC(icx,DIVACT,L2,IUSE,ISCD,nd,ieff2)
c
c rrb 00/12/20;               
c ---------------------------------------------------------
c              Step 6f; Check if more can be diverted
c
        if(ieffmax.eq.1) then 
          call dnmfso(maxsta, avail ,idncod,iscd  ,ndns  ,imcd)
          if(avail(imcd).gt.small) then

            IF(-IOPOUT.eq.ISCD .or. iout.eq.1) then
              write(nlog,*) ' Divrig; Step 6f-1 avail = ', avail(imcd)*f
            endif

            call rtnmax(2, iuse, iri, ire, iscd, ndns, 
     1        small, pavail, ieff2, ioutZ, cCallBy, corid1)

            IF(-IOPOUT.eq.ISCD .or. iout.eq.1) then
              write(nlog,*) ' Divrig; Step 6f-2 pavail = ', pavail*f
            endif
c
c rrb 02/06/27; Limit additional diversion to available diversion
            divmore=amin1(divalo-divact, pavail)
            pavail=amax1(0.0, divmore)

            IF(-IOPOUT.eq.ISCD .or. iout.eq.1) then
              write(nlog,*) ' Divrig; Step 6f-3 pavail = ', pavail*f
            endif

c           write(nlog,*) ' Divrig iteration 2; pavail', pavail*f 
            if(pavail.gt.small) then
              CALL TAKOUT(maxsta,AVAIL,RIVER,AVINP,QTRIBU,IDNCOD,
     1                    pavail,NDNS, ISCD )
c
c
c rrb 00/12/26; Variable efficiency
c             CALL RTNSEC(icx,pavail,L2,IUSE,ISCD,nd) 
              CALL RTNSEC(icx,pavail,L2,IUSE,ISCD,nd,ieff2)

              divact=divact+pavail

              IF(-IOPOUT.eq.ISCD .or. iout.eq.1) then
                write(nlog,*) ' Divrig; Step 6f-4 divact = ', divact*f
              endif

c             write(nlog,*) ' Divrig iteration 2; divact', divact*f  
            endif  
          endif
        endif
      endif
c
c rrb 10/17/97; Convergence Concern
      IF(IRTURN(IUSE).LE.3 .and. (divact+small).lt.divalo) ISHORT=1
c
c_____________________________________________________________
c               Step 7 Update available flow at diversion station
c                 for Cases 2, 3A & 3b 
c
  220 continue
      if(ioprtn.ne.0) then
        TEMP=AVAIL(ISCD)
        CURRTN(ISCD)=CURRTN(ISCD)+AMIN1(0.,TEMP)
        AVAIL (ISCD)=AMAX1(0.,TEMP)
      endif
c
c_____________________________________________________________
c               Step 8; Double Check available flow
c
  230 continue
      CALL DNMFSO(maxsta, AVAIL, IDNCOD, ISCD, NDNS, IMCD)

      IF(-IOPOUT.eq.ISCD .or. iout.eq.1) then
        write(nlog,*) ' Divrig; Step 8 avail = ', avail(imcd)*f
      endif

c             
c               Print warning if negative available flow
      IF(AVAIL(IMCD).le.(-1.*small)) then
        WRITE(99,310) IYRmo(mon),xmonam(MON),IW,NWRORD(1,IW),L2,
     1                IUSE,DIVREQx*f,
     1                ISCD,ISCD,IMCD,DIVACT*f, avail(imcd)*f
        write(99,320) (avail(iss)*f,iss=1,numsta)
        write(99,330) (river(iss)*f,iss=1,numsta)
        goto 9999
      endif
c
c_____________________________________________________________
c               Step 9; UPDATE MONTHLY DIVERSION FOR EACH USER
c

c
c rrb 01/02/23; Demand options 4 & 5
      if(idemtyp.le.3) then
        DIVREQ(IUSE)=DIVREQ(IUSE)-DIVACT
      else
        divreq(iuse)=amax1(0.0, divreq(iuse)-divact)
        divsw(iuse)=divsw(iuse)-divact 
c
c rrb 01/02/25; Demand options 4 & 5               
        nw=idivco2(nd)
        if(nw.gt.0) then
          if(ieffmax.le.0) then
            effd=diveff(mon,nd)/100.
            effw=diveffw(mon,nw)/100.
          else
            effd=effmax(nd)/100.
            effw=effmaxw(nw)/100.
          endif

          dcux=(divact*effd)/effw
c
c rrb 2007/10/01; Remove DivGW                    
c         divgw(nw)=amax1(0.0, divgw(nw)-dcux)
        endif
      endif
c
      USEMON(IUSE)=USEMON(IUSE)+DIVACT 
      DIVMON(ND  )=DIVMON(ND  )+DIVACT
c
c rrb 2005/01/06; Revise for an operating rule      
c     divd(l2) = divd(l2)+divact
      divd(ndr) = divd(ndr)+divact
c
c
      IF(IRTURN(IUSE).ne.4) then
        QDIV(5,ISCD)=QDIV(5,ISCD)+DIVACT
      else
        QDIV(8,ISCD)=QDIV(8,ISCD)+DIVACT
      endif
c
c               g. Update diversion by this Operating Rule
      DIVO(L2)=DIVO(L2)+DIVACT
      
c
c_____________________________________________________________
c               Step 10; Print detailed results if requested
c 
  260 continue
c    
c_____________________________________________________________
c               Step 11; Set return switch (iretsw), shortage (ishort) 
c                 switch and actual diversion (divact)
c

      if(divact.gt.small) iretsw=1
c
c rrb 02/27/96; General replacement reservoir requirement
c
c rrb 10/15/97; Convergence concern
      if((divact+small).lt.divalo) ishort = 1

      IF(-IOPOUT.eq.ISCD .or. iout.eq.1) then
        write(99,*) ' Divrig_4;   l2  divalo  divact  ishort' 
        write(99,'(10x,i5, 2f8.2, i8)') l2, divalo, divact, ishort
      endif
c
c _________________________________________________________
c rrb 04/09/08; Distribute to owners
      no=ndown(nd+1)-ndown(nd)
      if(no.gt.1 .and. divact.gt.small) then
        call AccDiv(nlog, maxownd, nd, divact, ndown, divownP,
     1    divownQ, f, iopout, iscd, iyrmo(mon), xmonam(mon), divnamo)    
      endif
      
 380  continue
      if(iout.ge.1 .and. iw.eq.ioutiw) then
        ncallX=ncallX+1
        if(ncallX.eq.1)then
          write(nlog,270) corid(l2),cdestyp,ccarry,cpuse
        else
c          write(nlog,*) ' '
        endif  
        
c       NR=IRSORD(1,ISCD)
        write(99,280) '  DivRigS_Out',
     1    iyrmo(mon),xmonam(mon),idy, imonsw(l2,mon), 
     1    cstaid(iscd),iwx, iw,nwrord(1,iw),l2,nr,
     1    -1,ND,IUSE,DIVREQx*f,
     1    AVAIL(ISCD)*f,CURRTN(ISCD)*f,DIVCAP(ND)*f, DIVMON(ND)*f, 
     1    dcrdivX*f,divd(ndr)*f, imcd,pavail*f,divact*f,
     1    iwhy, cwhy     
      endif
 
c
c_____________________________________________________________
c               Step 12; Return
c
c
c_____________________________________________________________
c               Step 13; Identify call (note only if short)
c
      if(ishort.gt.0 .and. nd.gt.0) then
        ctype1='Diversion'
cr      call GetCall(iscd, imcdD(nd), nd, ctype1)        
        call GetCall(iscd, imcdL(iscd), nd, ctype1)        
      endif  

      RETURN
c
c_____________________________________________________________
c               Formats
c
  270   format(/, 
     1  '  DivRigS (Type 36); Operation Right ID = ', a12,
     1  ' Destination Type = ', a12,
     1  ' Carrier (Y/N) = ',a3, ' Reuse Plan (Y/N) = ', a3,/
     1  '                                                  ',
     1  '                                                        ',
     1  '          Demand   Avail  Return Capacty  Divmon  Decree',
     1  '    Divd    Imcd  Pavail  Divact             ',/     
     1  '  DivRigS       iyr  mon  day  imonsw ID          ',
     1  '    Iter      Iw  nwrord      l2      nr  irsord      nd',
     1  '    iuse divreqx   avail  currtn  divcap  divmon dcrdivX',
     1  '    divd    imcd  pavail  divact    iwhy cwhy',/
     1  ' _____________ ____ ____ ____ _______ ____________', 
     1  ' _______ _______ _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______ ', 24('_'))
  280   FORMAT(a14, i5,1x,a4,i5,i8,1x,a12,8i8,
     1    7F8.0, i8, 2f8.0,i8,1x,a24)
  290   FORMAT(/, '  Divrig; QDIV ',a12,/,16F7.1)
  300   FORMAT(/, '  Divrig; QRES ',a12,/,16F7.1)
  310   FORMAT(/, '  Divrig Print 5',I10,6x,a4,4i10,
     1               F10.2,3I10,F10.2, f20.10)
  320   format(/, '  Divrig: avail  ',/,(10f10.2))
  330   format(/, '  Divrig: river  ',/,(10f10.2))
c
c_____________________________________________________________
c               Error warnings
c
 9999 write(6,1050) 
      write(99,1051) 
      
 1050 format('    Stopped in Divrig',/,
     1       '    See the *.log file')
 1051 format('    Stopped in Divrig')
      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)


      stop 
      END

