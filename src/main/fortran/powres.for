c
c *********************************************************
c
      subroutine powres(iw,l2,divact,ncallx)
c
c
c _________________________________________________________
c	Program Description
c
c       Powres; It simulates a power or ISF right
c
c _________________________________________________________
c       Update History
c
c rrb 03/13/96; initilize divact, send returns to bottom & set divo
c rrb 98/03/03; Daily Capability
c rrb 02/10/25; Allowm monthly on/off switch
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
      character cwhy*48, cdestyp*12, ccarry*3, cpuse*3 
c
c _________________________________________________________
c		Step 1; Initilize
      iw = iw
      
      iout=0
      ioutiw=0
      
      if(ichk.eq.101) iout=1
      if(corid(l2).eq. ccall) ioutiw=iw
c     write(nlog,*) ' PowRes; ichk, ioutiw, iw',ichk,ioutiw,iw  
c
c rrb 98/08/10; Convergence Update
      small = 0.001
c
c rrb 98/03/03; Daily capability
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif
      
      cwhy='N/A'
      cdestyp='N/A'
      ccarry='No'
      cpuse='No'

      iwhy=0
      ipuse=-1
      imcdx=-1
      divact = 0.0
      divreqx2=-1./fac
      divalos=-1./fac
      divactX=-1./fac
      availX=-1./fac 
      mon2 = imonsw(l2,mon)      
c
c _________________________________________________________
c		Step X; On/Off Switch      
c
c rrb 06/01/18; Allow daily on/off switch
      if(imonsw(l2,mon).eq.0) then
        iwhy=1
        cwhy='Monthly switch Off'
        goto 130
      endif  
c
c ---------------------------------------------------------
c		For a daily model set demand for beginning of season
      if(iday.eq.1 .and. imonsw(l2,mon).gt.0) then
        if (idy.lt.imonsw(l2,mon)) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 130
        endif  
      endif  
c
c ---------------------------------------------------------
c		For a daily model set demand for end of season
      if(iday.eq.1 .and. imonsw(l2,mon).lt.0) then
        if (idy.gt.iabs(imonsw(l2,mon))) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 130
        endif  
      endif  
      
C
c _________________________________________________________
c
c		Step 3; Set SOURCE (RESERVOIR) data
C
      NR  =IOPSOU(1,L2)
      IF(IRESSW(NR).EQ.0) then
        iwhy=3
        cwhy='Source reservoir is off'        
        Goto 130
      endif
      
      IOWN=NOWNER(NR)+IOPSOU(2,L2)-1
      ISCD=IRSSTA(NR)
      NDNS=NDNNOD(ISCD)
c
c ---------------------------------------------------------
c rrb 2006/08/19; Check reservoir data going in
c		Not in1=1 intosubroutine      
c		isub1=subroutine calling
      in1=0
      isub1=1
      call chekres(nlog, maxres, in1, isub1, iyr, mon, nr,nowner,
     1                     curown,cursto,cresid)
      
C
c _________________________________________________________
c
c		Step 4; Set DESTINATION (Isf) data
      NF  =IOPDES(1,L2)
      IFCD=IFRSTA(NF)
c
c rrb 2011/02/06; Exit if the ISF is off
      if (IFRRSW(nf).eq.0) then
        iwhy=4
        cwhy='Instream Structure is off'        
        goto 130
      endif        
c
      IF(FLOWRQ(NF).LE.small) then
        iwhy=5
        cwhy='Instream Demand is zero'    
        Goto 130
      endif
c
c _________________________________________________________      
c
c		Step 5; Set demand
      DIVALO=FLOWRQ(NF)
C
c _________________________________________________________
c
c		Step 6; CALCULATE VOLUME AVAILABLE FROM RESERVOIR
C
      RESAVL=AMIN1(CURSTO(NR)-VOLMIN(NR),CUROWN(IOWN))
      RESAVL=AMAX1(0.,RESAVL)
      RAVCFS=RESAVL/fac
C
c _________________________________________________________
c
c		Step 7; CALCULATE River CAPACITY 
C
      FLOAVL=AMAX1(FLOMAX(NR)-RIVER(IFCD),0.)
      IF(FLOAVL.LE.small) Goto 130
C
c _________________________________________________________
c
c		Step 8; CHECK AVAILABLE WATER AT RESERVOIR
C
c     IF(RAVCFS.LE.0.00001) Goto 130
      IF(RAVCFS.LE.small) Goto 130
C
C
c _________________________________________________________
c
c		Step 9; Set release
      IF(FLOAVL.LE.RAVCFS) GO TO 100
C
C------  MIN VOL IS LIMITING 
C
      DIVACT=AMIN1(RAVCFS,DIVALO)
      GO TO 110
C
C------  OUTLET CAPACITY IS LIMITING 
C
  100 DIVACT=AMIN1(FLOAVL,DIVALO)
C
c _________________________________________________________
c
c		Step 10; Adjust River and Avail. Note
c			Avail at RES avail(iscd) is not adjusted
c			Avail at ISF avail(ifcd) is not adjusted
c rrb 05/03/29; Key avail at the ISF (avail(ifcd) has not increased. 
C
  110 TEMP=-DIVACT
      availr=avail(iscd)
      CALL TAKOUT(maxsta, AVAIL ,RIVER ,AVINP ,QTRIBU,IDNCOD,
     1            TEMP  , NDNS,  ISCD  )
      AVAIL (IFCD)=AVAIL (IFCD)-DIVACT
      avail(iscd)=availr
c      
c _________________________________________________________
c
c		Step 11; Adjust
  120 RELAF=DIVACT*fac
c
c		a. Reservoir
      CURSTO(NR  )=CURSTO(NR  )-RELAF
      PROJTF(NR  )=PROJTF(NR  )+DIVACT
      CUROWN(IOWN)=CUROWN(IOWN)-RELAF
c     IF(IOWNA.NE.IOWN) QMAINS(2,IOWNA)=QMAINS(2,IOWNA)-RELAF
C
c		b. Demand
      FLOWRQ(NF  )=FLOWRQ(NF  )-DIVACT
c
c		c. Stream
      QRES(12,NR)=QRES(12,NR)+RELAF
      QDIV(15,IFCD)=QDIV(15,IFCD)+DIVACT
c
c		d. Accounts
      accr(12,iown) = accr(12,iown)+relaf
c
c		e. Operating rule
 130  divo(l2)=divo(l2)+divact
      divactx=divact
c
c _________________________________________________________
c
c               Step 15.  Detalied output
      if(iout.eq.1 .and. iw.eq.ioutiw) then
        ncallX=ncallX+1
        if(ncallX.eq.1)then
          write(nlog,270) corid(l2),cdestyp,ccarry,cpuse
        else
c          write(nlog,*) ' '
        endif  
      
        write(nlog,280) 'PowRes ',
     1    iyrmo(mon),xmonam(mon), idy,
     1    cstaid(iscd),iwx, iw,nwrord(1,iw),l2,l2,
     1    Nf,ipuse,imcdX, availX*fac, DIVREQx2*fac, 
     1    divaloS*fac, -1.0, divactx*fac, iwhy, cwhy
      endif
c
c ---------------------------------------------------------
c rrb 2006/08/19; Check reservoir data going in
c		Not in1=1 out of a subroutine      
c		isub1=subroutine calling
      in1=1
      isub1=1
      call chekres(nlog, maxres, in1, isub1, iyr, mon, nr,nowner,
     1                     curown,cursto,cresid)
c
c _________________________________________________________
c
c               Formats
      
  270   format(/, 
     1  ' PowRes (Type 1); Operation Right ID = ', a12,
     1  ' Destination Type = ', a12,
     1  ' Carrier (Y/N) = ',a3, ' Reuse Plan (Y/N) = ', a3/
     1  ' DivCar       iyr mon   day ID          ',
     1  '    Iter      Iw  nwrord      l2      l2      nF   ipUse', 
     1  '   imcdX  availX demandX divaloS psuplyT divactX',
     1  '    iwhy Comment',/
     1  ' ___________ ____ ____ ____ ____________', 
     1  ' _______ _______ _______ _______ _______ _______ _______', 
     1  ' _______ _______ _______ _______ _______ _______',
     1  ' _______ __________________________')           
  280   FORMAT(a12, i5,1x,a4, i5, 1x, a12,8i8,5F8.1,i8,1x, a48) 
c
c _________________________________________________________
c
      RETURN
      END



