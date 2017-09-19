C
c *********************************************************
c
      SUBROUTINE ResRgP(IW,L2,ncallX)
c       
c
c _________________________________________________________
c	Program Description
c
c	Type 41 Operating Rule
c       ResRgP; It simulates a standard reservoir storage
c               Limited to the Volume stored by one or 
c		more plans. Also limits to target contents
c
c _________________________________________________________
c       Update History
c
c     2006/09/29; Copied ResRg1 and revised accordingly
c
c _________________________________________________________
c       Documentation
c
c        IW         : OVERALL WATER RIGHT ORDER
c        L2         : LOC. OF WATER RIGHT (IW) ON RES. RIGHT TABLE
c        nrown(l2)  : Number of owners associated with right l2
c        iresco(2,l2) account(s) tied to this right.  Note set to 1
c                     if multiple accounts are tied to this right
c        irestyp(l2): =1 right is tied to 1 account
c                     = 0 right is tied to all accounts in this
c                       reservoir and water is distributed based
c                       on ownership ratio
c                     =-1 right is tied to the firs n accounts in this
c                       reservoir and water is distributed based
c                       on availabel space in each
c
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
      character cwhy*48, cdestyp*12, ccarry*3, cpuse*3, cidvri*12,
     1          cSouTyp*12, ctype1*12, cresid1*12       
c
c
c _________________________________________________________
c
c               Step 1; Initilize
c
c		iout = 0 no details
c		       1 details
c                      2 summary      
      iout=0
      ioutiw=0
      if(ichk.eq.141) iout=2
      if(corid(l2).eq. ccall) ioutiw=iw
c     if(iout.ge.1) write(nlog,*) '  Divres.for;'      
      if(ichk.eq.4) write(nlog,*) ' Divres; Entering'
c
      availx = 0.0
      stoalo = 0.0
      stoact = 0.0
      actaf  = 0.0
      imcd = -999
      tarcon=-999.0
      ishort=0
      icase=0
      iownX=-1
      
      cdestyp='Reservoir'
      cSouTyp='Priority'
      ccarry='No'      
      cpuse='No ' 
      
      small = 0.001
c
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif
      
      iwhy=0
      cwhy='N/A'      
      
      if(iout.eq.2 .and. iw.eq.ioutiw) then      
        if(ncallx.eq.0) then
c
c rrb 2006/10/16; Correction        
c         write(nlog,270) creswr(l2),cdestyp, cSouTyp, ccarry, cpuse
          write(nlog,270) corid(l2),cdestyp, cSouTyp, ccarry, cpuse
          ncallx=ncallx+1
        endif  
        
        write(nlog,280) 
     1    '  In', iyrmo(mon),xmonam(mon), idy, iwx, cstaid(ircd),
     1    iressw(nr),  iw,  nwrord(1,iw), l2, nr,
     1    IRSORD(2,IRCD), IOWNx, imcd, 
     1    Plimit, CapRem, Cursa, Tarcon, StoAlo,
     1    availX*fac,StoAct*fac, iwhy, cwhy
        
      endif  
c
c _________________________________________________________
c
c               Step 2; Branch if not on this month
c
c rrb 04/22/96; Allow month switch
      if(imonsw(l2,mon).eq.0) then
        iwhy=1
        cwhy='Monthly switch is off'
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
c
c _________________________________________________________
c
c               Step 3; Set Source Reservoir Right Data
c		        Note Oprinp checks this right is tied
c			to the destination reservoir
      nwr=iopsou(1,l2)
      ritrem1=ritrem(nwr)
      IF(ritrem(nwr).le.small) then
        iwhy=2
        cwhy='Remaining Reservoir Right = 0'
        Goto 130
      endif  
c
c _________________________________________________________
c
c               Step 4; Set Destination Reservoir data
      NR  =iopdes(1,l2)
C
      IF(IRESSW(NR).EQ.0) then
        iwhy=1
        cwhy='Reservoir is off'
        Goto 130
      endif  
C
      IRCD=IRSSTA(NR)
      NDNR=NDNNOD(IRCD)       
c
c ---------------------------------------------------------
c rrb 2006/09/25; Allow multiple accounts - Initilize
      nro=1
      if(iopdes(2,l2).lt.0) then
        nro=-iopdes(2,l2)
c
c rrb 2008/06/26; Correction        
c       irow=nowner(nr2)
        irow=nowner(nr)
      endif

      if(iopdes(2,l2).gt.0) then
        nro=1
c
c rrb 2008/06/26; Correction        
c       irow=nowner(nr2)+iopdes(2,l2)-1
        irow=nowner(nr)+iopdes(2,l2)-1
      endif
        
      iownX=irow
c     write(nlog,*) ' ResRgP; irow, iownX', irow, iownX, iopdes(2,l2)
      
c
c ---------------------------------------------------------
c rrb 2006/08/19; Check reservoir data going in
c		Not in1=0 into subroutine      
c		isub1=subroutine calling
      in1=0
      isub1=25
      call chekres(nlog, maxres, in1, isub1, iyr, mon, nr,nowner,
     1                     curown,cursto,cresid)
c _________________________________________________________
c
c               Step 5;  CHECK AVAILABLE WATER AT reservoir itself
c
      IF(AVAIL(IRCD).LT.small) then
        iwhy=2
        cwhy='Available Flow at reservoir = 0'
        Goto 130
      endif  
c _________________________________________________________
c
c               Step 6;  Set Plan limitations
      Plimit=0.0
      do i=1,10
        np=intern(l2,i)
        if(np.gt.0) then
          Plimit=plimit+psto2(np)
c
c ---------------------------------------------------------
c		Double check proper plan type        
          iok=0
          if(iplntyp(np).eq.9 .or. iplntyp(np).eq.10) iok=1
          if(iok.eq.0) goto 510
        endif
      end do
      
      IF(Plimit.le.small) then
        iwhy=3
        cwhy='Storage Plan Limit = 0'
        Goto 130
      endif  
c
c _________________________________________________________
c
c               Step 7;  Find storage available 
c
c ---------------------------------------------------------
c		Find Storage availabe in total reservoir
      CAPREM=VOLMAX(NR)-CURSTO(NR)
      IF(caprem.le.small) then
        iwhy=5
        cwhy='Storage in Destination Reservoir = 0'
        Goto 130
      endif  
c
c ---------------------------------------------------------
c 		Find Storage available in appropriate accounts
      cursa=0.0
      do n=1, nro
        n1=irow+n-1
        cursa=cursa+(ownmax(n1)-curown(n1))
      end do  
      
      IF(cursa.le.small) then
        iwhy=4
        cwhy='Storage in Destination Accounts = 0'
        Goto 130
      endif  
c
c ---------------------------------------------------------
c		Find stoage available to reservoir target        
      tarcon=tarmax(nr)-cursto(nr)
c
c rrb 2006/10/16; Allow to store above target.Note
c		  if there is a subsequent release to 
c		  target this results in a paper fill       
      if(iressw(nr).eq.3) then
        stoalo=amin1(caprem,ritrem(nwr),cursa,Plimit)      
      else
        IF(tarcon.le.small) then
          iwhy=6
          cwhy='Storage above target = 0'
          Goto 130
        endif          
        stoalo=amin1(caprem,tarcon,ritrem(nwr),cursa,Plimit)      
      endif
      
      STOALO=AMAX1(STOALO,0.)
      stocfs=stoalo/fac
      
c _________________________________________________________
c
c               Step 8; Find MINIMUM FLOW downstream
C
      CALL DNMFSO(maxsta, AVAIL ,IDNCOD,IRCD,NDNR,IMCD)
      imcdx=imcd
      availx = avail(imcdx)
      
      IF(availx.le.small) then
        iwhy=7
        cwhy='Available Downstream Flow = 0'
        Goto 130
      endif  
c
c ---------------------------------------------------------
c		Set storage actual to the minimum of above      
      STOACT=AMIN1(AVAIL(IMCD),STOCFS)
c
c _________________________________________________________
c
c               Step 9; Remove from system
      CALL TAKOUT(maxsta, AVAIL ,RIVER ,AVINP ,QTRIBU,IDNCOD,
     1            STOACT, NDNR,  IRCD)
c
c _________________________________________________________
c
c               Step 10; CHECK AVAILABLE FLOW
      CALL DNMFSO(maxsta, AVAIL ,IDNCOD,IRCD  ,NDNR  ,IMCD  )

      IF(AVAIL(IMCD).lT.(-1.0*small)) then
        write(nlog,390) icase, iyrmo(mon),xmonam(mon),cstaid(ircd), 
     1    IW,L2,irow,IRCD,IMCDx,imcd,
     1    Stoalo, Stoact*fac,
     1    availx*fac, avail(imcd)*fac
      endif
c _________________________________________________________
c
c               Step 11; UPDATE MONTHLY STORAGE FOR EACH OWNER
c
      actaf=stoact*fac
C
      RITREM(nwr)=RITREM(nwr)-ACTAF
      CURSTO(NR  )=CURSTO(NR  )+ACTAF
      QRES(1,NR  )=QRES(1,NR  )+ACTAF
c
c ---------------------------------------------------------
c		Distribute to accounts

      ia=1
      nrX=nr
      iResT1=0
      nrown1=nro
      iownX=irow
      icx=114
      cresid1=cresid(nrX)
c          
      call accou(maxacc, maxown, nrX, ownmon, curown, accr, ia,
     1      ownmax, iownX, nrown1, cursa, actaf, iResT1, icx, cresid1)
c _________________________________________________________
c
c               Distribute amount stored to individual Plan obligations
c		Note check for Plimit = 0 was done above
      do i=1,10
        np=intern(l2,i)
        if(np.gt.0) then
          c = Psto2(np)/Plimit*ActAf
          psto2(np)=amax1(0.0, psto2(np) - c)
        endif
      end do    
c
c _________________________________________________________
c               
c               Step 12; Set operating rule output (DIVO)
      divo(l2)=divo(l2)+stoact
c
c_____________________________________________________________
c
c		     
 130  continue    
c
c_____________________________________________________________
c
c		Step 13; Set shortage
      if(actaf.lt.stoalo-small) ishort=1              
c
c_____________________________________________________________
c               Step 14; Identify call (note only if short)
c
      if(ishort.gt.0 .and. nr.gt.0) then
        ctype1='Reservoir'
        call GetCall(ircd, imcdL(ircd), nr, ctype1)        
      endif  
C
c _________________________________________________________
c
c               Step 15; Detailed Printout
c
      if(iout.eq.2 .and. iw.eq.ioutiw) then            
        write(nlog,280) 
     1    ' Out', iyrmo(mon),xmonam(mon), idy, iwx, cstaid(ircd),
     1    iressw(nr),  iw,  nwrord(1,iw), l2, nr,
     1    IRSORD(2,IRCD), IOWNx, imcd, 
     1    Plimit, CapRem, Cursa, Tarcon, StoAlo,
     1    availX*fac,StoAct*fac, iwhy, cwhy
      endif        
      
      if(iout.eq.1) then
        write(nlog,*) '  ResRgP; mon, actaf', mon, actaf, 
     1    curown(1), curown(2)
      endif
c
c _________________________________________________________
c
c               Step 16; Check reservoir data going out
c		Not in1=1 out of subroutine      
c		isub1=subroutine calling
      in1=1
      isub1=25
      call chekres(nlog, maxres, in1, isub1, iyr, mon, nr,nowner,
     1                     curown,cursto,cresid)
      
c _________________________________________________________
c
c               Step 17; Return
c
      RETURN
c
c _________________________________________________________
c
c               Formats
  101  format(/,
     1   '  ResRgP;  iyr  mon  l2 nrown iownx   n   n1 cursa',/
     1   '  ResRgP;', 7i5, f10.2)

  160   FORMAT(/, 80('-'),/,' ResRgP; Avail in')
  170   FORMAT(/,' ResRgP; Avail out')
  180   format(10f8.0)
  
  270   FORMAT(/, 72('_'),/
     1 '  ResRgP; Standard Reservoir Storage ',/
     1 '          Reservoir Right ID = ', a12,
     1 ' Destination Type = ', a12, ' Source Type = ', a12,
     1 ' Carrier (Y/N) = ',a3, ' Reuse Plan (Y/N) = ', a3//  
     1  ' ResRgP;   In  iyr  mon  idy iter cstaid        iressw',
     1  '      iw  nwrord      l2      nr  irsord   iownx    imcd',
     1  '  Plimit  Caprem   Cursa  TarCon  StoAlo',
     1  '  AvailX  StoAct    iwhy Comment'/,
     1  ' ResRgP;', 5(' ____'), 1x, '____________', 8(' _______'),
     1  7 (' _______'), ' ________________________')
     
  280   format(9x, a4,i5,1x,a4,2i5,1x, a12,8i8, 7F8.0, i8, 1x, a48)

  390 FORMAT(/, ' DIVCAR; Problem negative avail for Icase = ', i5,/,
     1  '  IYR  MON ID           ',
     1  '      IW      L2    irow    iRcd   imcdx    imcd',
     1  '    StoAlo    StoAct    AvailX     Avail',/
     1  ' ____ ____ _____________',
     1  ' _______ _______ _______ _______ _______ _______',
     1  ' _________ _________ _________ _________ _________',/,
     1  i5,1x,a4,1x,a12,1x, 6i8, 20F10.2)
     
c
c _________________________________________________________
c               Error warnings
 510  write(nlog,512)
 512  format(
     1 '  ResRgP; Problem a Type 41 Operating Rule requires',/
     1 '          Out-of-Priority Plan data',/
     1 '          Reconmend you revise the operating rule (*.opr) file')
      goto 9999
      
 9999 write(6,1050) 
      write(nlog,1051) 
      call flush(6)
 1050 format('    Stopped in ResRgP',/,
     1       '    See the *.log file')
 1051 format('    Stopped in ResRgP')
      write(6,*) 'Stop 1' 
      call flush(6)
      call exit(1)

c
c _________________________________________________________
c
      stop 
      END
