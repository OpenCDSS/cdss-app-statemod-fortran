C
c *********************************************************
c
      subroutine powsea(iw,l2,divact,ncallX)
c
c
c _________________________________________________________
c	Program Description
c
c       Powsea; It simulates a type 9 opeating rule that
c               makes a reservoir releases if contents are greater
c               than amaximum target level
c
c _________________________________________________________
c       Update History
c
c rrb 03/13/96; initilize divact, send returns to bottom & set divo
c rrb 02/05/97; If source account (iopsou(2,l2) is 0,
c               allow target to release from all accounts by 
c               distributing based on current ownership ratio.
c               Also, miscelaneous clean up for clarity
c rrb 02/10/25; Allow monthly on/off switch
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
      character cwhy*48, cdestyp*12, ccarry*3, cpuse*3, cstaid1*12,
     1          rec12*12, cTandC*3
      
c
c _________________________________________________________
c
c
c		iout=1 details
c		iout=2 summary if ...
c		iout=99 summary independent of ichk
cx    if(ichk.eq.4) write(nlog,*) ' Powsea; Entering'
      divact = 0.0
      iw = iw
      
      iout=0      
      if(ichk.eq.109) iout=2
      if(corid(l2).eq. ccall) ioutiw=iw
c
c rrb 98/08/10
      small=0.001
      small2=0.1
c
c rrb 98/03/03; Daily capability
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif
      
      divaloS = -1.0/fac
      divexP  = -1.0/fac
      pavail  = -1.0/fac
      pdem1   = -1.0/fac
      pdem2   = -1.0/fac
      divCU   = -1.0/fac
      divCarry= -1.0/fac
      curown1 = -1
      curown2 = -1
      divact1= 0.0
      cstaid1='NA'
      
      ISHORT=0
      iwhy=0
      cwhy='NA'
      cdestyp='NA'
      ccarry='No'
      cpuse='No'
      cTandC='No'
      
c
c rrb 02/10/25; Allow monthly on/off switch
      if(imonsw(l2,mon).eq.0) then
          iwhy=1
          cwhy='Monthly switch Off'
        goto 120
      endif  
c
c ---------------------------------------------------------
c		For a daily model set demand for beginning of season
      if(iday.eq.1 .and. imonsw(l2,mon).gt.0) then
        if (idy.lt.imonsw(l2,mon)) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 120
        endif  
      endif  
c
c ---------------------------------------------------------
c		For a daily model set demand for end of season
      if(iday.eq.1 .and. imonsw(l2,mon).lt.0) then
        if (idy.gt.iabs(imonsw(l2,mon))) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 120
        endif  
      endif  
c
c _________________________________________________________
c
c
c              Find reservoir (nr), owner (iown), river location (iscd)
c                and # of downstream nodes (ndns)
C
      NR  =IOPSOU(1,L2)
      IOWN=NOWNER(NR)+IOPSOU(2,L2)-1
      ISCD=IRSSTA(NR)
      NDNS=NDNNOD(ISCD)
      cstaid1=cresid(nr)
      
C
C              CALCULATE VOLUME AVAILABLE FROM RESERVOIR (resval - af,
c               ravcfs - cfs)
C
c rrb 02/05/97; Allow target release from all accounts
      if(iopsou(2,l2).gt.0) then
        RESVAL=AMAX1(AMIN1(CURSTO(NR)-tarmax(nr),CUROWN(IOWN)),0.)
c
c rrb 2006/05/31; Correction, moved from below        
        curown1=curown(iown)        
      else
        resval=amax1(cursto(nr)-tarmax(nr),0.0)
      endif
      
      cursto1=cursto(nr)
      tarmax1=tarmax(nr)
      resval1=resval
c
      RAVCFS=RESVAL/fac
      IF(RAVCFS.LE.small) Goto 120
C
C                Compare max release (flomax) to flow in river (river)
      FLOAVL=AMAX1(FLOMAX(NR)-RIVER(ISCD),0.)
c     IF(FLOAVL.LE.0.00001) Goto 120
      IF(FLOAVL.LE.small) Goto 120
C
C               Determine limiting amount (release or volume)
      IF(FLOAVL.LE.RAVCFS) then
        divact=floavl
      else
        divact=ravcfs
      endif
c
c rrb 2006/04/10; Reoperation concerns
      if(divact.lt.small2) then
        divact=0.0      
        goto 120
      endif  
C
C               ADD RES. RELEASE TO DOWNSTREAM & RESERVOIR
      TEMP=-DIVACT
      CALL TAKOUT(maxsta, AVAIL ,RIVER ,AVINP ,QTRIBU,IDNCOD,
     1            TEMP  , NDNS,  ISCD  )
      AVAIL (ISCD)=AVAIL (ISCD)-DIVACT
C
C               REDUCE RESERVOIR STORAGE BY AMOUNT RELEASED (divact)
c rrb 98/03/03; Daily capability
c     RELAF=DIVACT*MTHDAY(MON)*FACTOR
      RELAF=DIVACT*fac
      cursto1=cursto(nr)
      CURSTO(NR) = CURSTO(NR)-RELAF
      PROJTF(NR) = PROJTF(NR)+DIVACT
C
      POWREQ(NR) = POWREQ(NR)-RELAF
      POWREL(NR) = POWREL(NR)+DIVACT
      
C
c rrb 02/05/97; Distribute to account assigned (iopsou>0) or
c               based on current storage in all accounts
c               (iopsou=0)
      if(iopsou(2,l2).gt.0) then
        CUROWN(IOWN)=CUROWN(IOWN)-RELAF
        accr(19,iown) = accr(19,iown) + relaf
        curown2=curown(iown)
      else
        ct=0.0
        iown=nowner(nr)-1
        if(iopsou(2,l2).eq.0) then
          nrown1=nowner(nr+1)-nowner(nr)
        else
c          nrown1=abs(iopsou(2,l2))
          write(99,111) iopsou(2,l2)
 111      FORMAT('  Powsea; Can only make a target release', /,
     1           'to 1 or all accounts. iopsou(2,l2) = ', i5)
          goto 9999
        endif

        if(iout.eq.1) then        
          write(99,*) ' '
          write(99,*)'  Powsea; nr iown nowner(nr+1) nowner(nr) nrown1'
          write(99,*) nr, iown, nowner(nr+1), nowner(nr), nrown1
          write(99,*)'  Powsea; n1, relaf, curown(n1), cursto1, c, ct'
        endif
c
      
        do 112 n=1,nrown1
          n1=iown + n
          c=(curown(n1)/cursto1)*relaf
          curown(n1)=curown(n1)-c
          accr(19,n1) = accr(19,n1) + c
          ct=ct+c
          if(iout.eq.1) then
            write(99,*) '  Powsea;'
            write(99,'(i5,10f8.0)') n1, relaf, curown(n1),
     1            cursto1, c, ct
          endif
 112    continue
      
c       if(ABS(ct-relaf).gt.0.1) then
        if(ABS(ct-relaf).gt.small2) then
          write(6,*) 'Powsea; Problem in allocating target release '
          write(6,*) '        to accounts ct, relaf, ct-relaf, small2'
          write(6,*)          ct, relaf, ct-relaf, small2

          write(99,*) 'Powsea; Problem in allocating target release '
          write(99,*) '        to accounts ct, relaf, ct-relaf, small2'
          write(99,*)          ct, relaf, ct-relaf, small2
          goto 9999
        endif
      endif
C
 120  divo(l2)=divo(l2)+divact
c
c
c ---------------------------------------------------------
c rrb 99/05/10; Roundoff check
      
      call chekres(io99,maxres, 1, 9, iyr, mon, nr,nowner,
     1                    curown,cursto,cresid)
c
c
c _________________________________________________________
c
c		Detailed output     
      if(iout.eq.99 .and. divact.lt.small) iout=98
c     if(iout.eq.99 .and. cDest.ne.cDest1) iout=98
      if((iout.eq.2 .and. iw.eq.ioutiw) .or. iout.ge.99) then      
      
      
        ncallX=ncallX+1          
        if(ncallX.eq.1 .or. iout.eq.99) then
          write(nlog,270) corid(l2), cdestyp, ccarry, cTandC, cpuse,
     1      cDivTyp(l2)
        else
cr        write(nlog,*) ' '
        endif  
        
        write(nlog,280) '  Powsea    ',
     1    iyrmo(mon),xmonam(mon), idy, cstaid1,     
     1    iwx, iw,l2,
     1    cursto1, tarmax1, curown1, resval1, floavl*fac, curown2, 
     1    divact*fac, 
     1    iwhy, cwhy

      endif
  270   format(/, 
     1  '  PowSea (Type 9); Operation Right ID = ', a12,
     1  ' Destination Type = ', a12,
     1  ' Carrier (Y/N) = ',a3, ' T&C Plan (Y/N) = ',a3,
     1  ' Reuse Plan (Y/N) = ', a3,
     1  ' Diversion Type = ', a12/    
     1  '    PowSea    iyr mon   day ID          ',
     1  '     Iter     Iw      l2',
     1  '   cursto1   tarmax1   curonw1   resval1    floavl   curown2',
     1  '    DIVACT',
     1  '    iwhy Comment',/
     1  ' ___________ ____ ____ ____ ____________', 
     1  ' _______ _______ _______',
     1  ' _________ _________ _________ _________ _________ _________',
     1  ' _________ _______ __________________________')
     
  280   FORMAT(a12, i5,1x,a4, i5, 1x, a12,3i8,6F10.0,f10.4,i8,
     1   1x, a24)
     
cx    if(ichk.eq.4) write(nlog,*) ' Powsea; Exiting'

      RETURN
c
c
c _________________________________________________________
c
c               Print warning
 9999 write(6,*) '  Stopped in Powsea, see the log file (*.log)'
      write(99,*) '  Stopped in Powsea'
      write(6,*) 'Stop 1' 
      call flush(6)
      call exit(1)

      stop 
      end
     
