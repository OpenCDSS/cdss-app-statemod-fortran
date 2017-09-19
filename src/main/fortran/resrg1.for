C
c *********************************************************
c
      SUBROUTINE RESRG1(IW,L2,divx,short,ncallX)
c       
c
c _________________________________________________________
c	Program Description
c
c       Resrg1; It simulates a standard reservoir diversion
c               If Iressw(l2)=0 it limits storage to target 
c                  contents and DOES NOT CHARGE against their
c		   decree (paper fill)
c               If Iressw(l2)=3 it limits storage to target 
c                  contents and DOES CHARGE against their
c		   decree (paper fill)
c
c _________________________________________________________
c       Update History
c
c rrb 04/10/96; added special logic for out of priority storage 
c               right (ityrsr(l2) = -1)
c     Revised to distribute one water right to many owners
c     05/012/01; Revised to allow re storage of previously released
c                reusable water
c     2006/06/05; Add Paper Fill (RitPaper)
c     2006/06/16; Set paper fill to ritrem.
c                 Removed all refrences to an OOP reservoir right
c                 (ityrsr(l2)=-1. Replaced with a type 38 rule
c     2006/11/20; Revise storage above target logic when IRESSW(L2)=3
C		  New logic does not allow storage above target
c		  but does reduce the decree.
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
c         iressw(l2)    0 DO NOT STORE above target DO NOT CHARGE
c			  to decree
c			3 DO NOT STORE above target but DO CHARGE to
c		          to decree (e.g. paper fill)
c	  PaperFil	Storage passed because of a target limit
c			(e.g. paper fill)
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
      
      iout=0
      ioutiw=0
      
      if(ichk.eq.202) iout=2
      if(creswr(l2).eq. ccall) ioutiw=iw 
c     write(nlog,*) ' Resrg1; l2, creswr(l2), ccall ', creswr(l2),ccall     
c
      availx = 0.0
      avail1 = 0.0
      stoalo = 0.0
      actaf  = 0.0
      cursa  = 0.0
      short  = 0.0
      imcd = -999
      tarcon=-999.0

      ishort=0
      ritPap1=-1.
      PaperFil=0.0
      
      cdestyp='Reservoir'
      cSouTyp='Priority'
      ccarry='No'      
      cpuse='No ' 
      
      ritrem1=ritrem(l2)
      
      small = 0.001
c
      if(iday.eq.0) then
        f=mthday(mon)*factor
      else
        f=factor
      endif
      
      iwhy=0
      cwhy='N/A'      
      
cx      if(iout.eq.2 .and. iw.eq.ioutiw) then      
cx        if(ncallx.eq.0) then
cx          write(nlog,270) creswr(l2),cdestyp, cSouTyp, ccarry, cpuse
cx          ncallx=ncallx+1
cx        endif  
cx      endif  
c
c _________________________________________________________
c
c               Step 2; Set RESERVOIR CODE, OWNER AND STATION
C
      NR  =IRESCO(1,L2)
      CAPREM=VOLMAX(NR)-CURSTO(NR)      
      short=caprem
C
      IF(IRESSW(NR).EQ.0) then
        iwhy=1
        cwhy='Reservoir is off'
        Goto 130
      endif  
C
      IOWN=NOWNER(NR)+IRESCO(2,L2)-1
      IRCD=IRSSTA(NR)
      NDNR=NDNNOD(IRCD)                     
c
c rrb 2006/08/19; Check reservoir data going in
c		Not in1=0 into subroutine      
c		isub1=subroutine calling
      in1=0
      isub1=23
      call chekres(nlog, maxres, in1, isub1, iyr, mon, nr,nowner,
     1                     curown,cursto,cresid)  
c _________________________________________________________
c
c               Step 3;  CHECK AVAILABLE WATER AT CURRENT STATION
c
      IF(AVAIL(IRCD).LT.small) then
        avail1=avail(ircd)*f
        iwhy=2
        cwhy='Available Flow (Avail) at reservoir = 0'
        Goto 130
      endif  
c _________________________________________________________
c
c               Step 4;  FIND REMAINING RESERVOIR CAPACITY
c      
      if(caprem/f.lt.small) then
        iwhy=3
        cwhy='Available Storage in reservoir (caprem) is 0'
        goto 130
      endif 
c
c _________________________________________________________
c
c               Step 5;  Find storage available to accounts
      cursa=0.0
      do 100 n=1,nrown(l2) 
        n1 = iown+n-1
        cursa=cursa+ownmax(n1)-curown(n1)

        if(iout.eq.1) then
          write(nlog,101) iyr, mon, l2, nrown(l2),iown,n,n1, cursa
        endif
  100 continue
  
      if(cursa/f.lt.small) then
        iwhy=4
        cwhy='Available Storage in accounts (cursa) is 0'
        goto 130
      endif 
      
      
c _________________________________________________________
c
c               Step 6; FIND THE ALLOWABLE STORAGE
C                       also limit to target contents based on type
c		        Note if iressw(nr)=3 it will NOT STORE above a 
c			target but will charge against their decree
c			(e.g. Paper fill)

c
c rrb 2006/11/29; Correct paper fill above target calculations
cx    if(iressw(nr).eq.3) then
cx      stoalo=amin1(caprem,ritrem(l2),cursa)
cx    else
c
c rrb 2006/11/20; Revise storage above target calculations      
        stoalo1=amin1(caprem,ritrem(l2),cursa)
        tarcon=amax1(0.0, tarmax(nr)-cursto(nr))
        stoalo=amin1(caprem,tarcon,ritrem(l2),cursa)
      
cx    endif
c
c rrb 2006/11/29; Correct paper fill above target calculations
      PaperFil=0.0
      if(iressw(nr).eq.3) then
        PaperFil=amax1(0.0, stoalo1-tarcon)
      endif      
c
c ---------------------------------------------------------
c		Exit based on target
      if(tarcon/f.lt.small) then
        iwhy=5
        cwhy='Storage below Target (Tarcon) is 0'
        goto 130
      endif 
c
c ---------------------------------------------------------
c		Exit based on decree
      if(Ritrem(l2)/f.lt.small) then
        iwhy=6
        cwhy='Water Right (Ritrem) is 0'
        goto 130
      endif 
c
c ---------------------------------------------------------
c		Check Paper fill
c rrb 2006/06/05; Paper Fill. Set to remaining right
cr    RitPap1=RitPaper(l2)
      RitPap1=ritrem(l2)

      stoalo1=stoalo
      stoalo=amin1(stoalo, RitPap1)      
      stoalo=amax1(stoalo, 0.0)
      stocfs=stoalo/f      
      
      if(RitPap1/f.lt.small) then
        iwhy=7
        cwhy='Paper fille (RitPap1) is 0'
        goto 130
      endif     
c _________________________________________________________
c
c               Step 8; Print detailed info
c
cx    IF(-IOPOUT.eq.IRCD .or. iout.eq.2) then
cx      if(iout.ge.1 .and. iw.eq.ioutiw) then    
cx        write(nlog,280)
cx     1    '  In', iyrmo(mon),xmonam(mon), idy, iwx, cstaid(ircd),
cx     1    iressw(nr),  iw,  nwrord(1,iw), l2, nr,
cx     1    IRSORD(2,IRCD), IOWN, imcd, 
cx     1               VOLMAX(NR), CURSTO(NR),caprem,
cx     1               tarmax(nr), tarcon, PaperFil,
cx     1               RitRem1, ritrem(l2), cursa, stoalo,
cx     1               avail1, availx, -1., actaf, iwhy, cwhy
cx      endif
c _________________________________________________________
c
c               Step 9; Find MINIMUM FLOW AND ALLOWABLE STORAGE
C
      CALL DNMFSO(maxsta, AVAIL ,IDNCOD,IRCD,NDNR,IMCD)
C
      STOACT=AMIN1(AVAIL(IMCD),STOCFS)
      availx = avail(imcd)*f
      
      if(stoact.lt.small) then
        iwhy=8
        cwhy='Downstream Available flow (Availx) = 0'
        goto 130
      endif 

      CALL TAKOUT(maxsta, AVAIL ,RIVER ,AVINP ,QTRIBU,IDNCOD,
     1            STOACT, NDNR,  IRCD  )
C
c _________________________________________________________
c
c               Step 10; CHECK AVAILABLE FLOW
C
      CALL DNMFSO(maxsta, AVAIL ,IDNCOD,IRCD  ,NDNR  ,IMCD  )

      IF(AVAIL(IMCD).lt.(-1.0*small)) then
        write(nlog,270) creswr(l2),cdestyp, cSouTyp, ccarry, cpuse
        write(nlog,280) 
     1    ' XXX', iyrmo(mon),xmonam(mon), idy, iwx, cstaid(ircd),
     1    iressw(nr),  iw,  nwrord(1,iw), l2, nr,
     1    IRSORD(2,IRCD), IOWN, imcd, 
     1    VOLMAX(NR), CURSTO(NR),caprem,
     1    tarmax(nr), tarcon, PaperFil,
     1    RitRem1, ritrem(l2), cursa, stoalo,
     1    avail1, availx, -1., actaf, iwhy, cwhy
        goto 9999
      endif
c _________________________________________________________
c
c               Step 11; UPDATE MONTHLY STORAGE FOR EACH OWNER
c
      actaf=stoact*f
c
      if (stoact.le.small) goto 130
C
c rrb 2006/11/20; Revise to reduce decree for possible
c		  storage above target
c     RITREM(L2  )=RITREM(L2  )-ACTAF
c
c rrb 2006/11/29; Correct Paper fill calculations
c     RITREM(L2  )=RITREM(L2  )-ACTAF-TarCon
      RITREM(L2  )=RITREM(L2  )-ACTAF-PaperFil
      CURSTO(NR  )=CURSTO(NR  )+ACTAF
      QRES(1,NR  )=QRES(1,NR  )+ACTAF
c
c _________________________________________________________
c
c               Step 12; Distribute to accounts. Note:
c		   iResT1=0 distributes based on ownership ratio
c		   nrown1=number of accounts in this reservoir
c		   iown = first account associated with this reservoir
c		   ia   = account to adjust (1=From River to Priority)
      cresid1=cresid(nr)
      iResT1=1
      nrown1=nrown(l2)
      icx=202
      ia=1
      call accou(maxacc, maxown, nr, ownmon, curown, accr, ia,
     1  ownmax, iown, nrown1, cursa, actaf, iResT1, icx, cresid1)
c
c_____________________________________________________________
c
c		     
 130  continue    
c
c_____________________________________________________________
c
c		Set shortage
c
c rrb 2011/05/15; Additional detailed output
cx    if(actaf.lt.stoalo-small) ishort=1
      if(actaf.lt.stoalo-small) then
        short=stoalo-actaf
        ishort=1    
      endif          
c
c_____________________________________________________________
c               Step X; Identify call (note only if short)
c
      if(ishort.gt.0 .and. nr.gt.0) then
        ctype1='Reservoir'
cr      call GetCall(ircd, imcdR(nr), nr, ctype1)        
        call GetCall(ircd, imcdL(ircd), nr, ctype1)        
      endif  
     
C
c _________________________________________________________
c
c               Step 13; Detailed Printout
c
cr    IF(-IOPOUT.eq.IRCD .or. iout.eq.2) then
      if(iout.eq.2 .and. iw.eq.ioutiw) then            
        if(iday.eq.1 .and. actaf.gt.small) then
        if(ncallx.eq.0) then
          write(nlog,270) creswr(l2),cdestyp, cSouTyp, ccarry, cpuse
          ncallx=ncallx+1
        endif  

        write(nlog,280) 
     1    ' Out', iyrmo(mon),xmonam(mon), idy, iwx, cstaid(ircd),
     1    iressw(nr),  iw,  nwrord(1,iw), l2, nr,
     1    IRSORD(2,IRCD), IOWN, imcd, 
     1    VOLMAX(NR), CURSTO(NR),caprem,
     1    tarmax(nr), tarcon, PaperFil,
     1    Ritrem1, ritrem(l2), cursa, stoalo,
     1    avail1, availx, -1., actaf, iwhy, cwhy
        endif
  280   format(9x, a4,i5,1x,a4,2i5,1x, a12,8i8, 14F8.0, i8, 1x, a48)
     
     
        if(iout.eq.1) then
          write(nlog,170)
          write(nlog,180) (AVAIL(ISS)*f,ISS=1,NUMSTA)
        endif        
      endif        
      
      if(iout.eq.1) then
        write(nlog,*) '  Resrg1; mon, actaf', mon, actaf, 
     1    curown(1), curown(2)
      endif
c
c ---------------------------------------------------------
c rrb 2006/08/19; Check reservoir data going out
c		Not in1=1 out of subroutine      
c		isub1=subroutine calling
      in1=1
      isub1=23
      call chekres(nlog, maxres, in1, isub1, iyr, mon, nr,nowner,
     1                     curown,cursto,cresid)
c
c rrb 2011/05/11; Set diversion
      divx=actaf/f
      
c _________________________________________________________
c
c               Step 14; Return
c
      RETURN
c
c _________________________________________________________
c
c               Formats
  101  format(/,
     1   '  Resrg1;  iyr  mon  l2 nrown iown    n   n1 cursa',/
     1   '  Resrg1;', 7i5, f10.2)

  270   FORMAT(/, 72('_'),/
     1 '  ResRg1; Standard Reservoir Storage ',/
     1 '          Reservoir Right ID = ', a12,
     1 ' Destination Type = ', a12, ' Source Type = ', a12,
     1 ' Carrier (Y/N) = ',a3, ' Reuse Plan (Y/N) = ', a3//  
     1  ' ResRg1;   In  iyr  mon  idy iter cstaid        iressw',
     1  '      iw  nwrord      l2      nr  irsord    iown    imcd',
     1  '  VolMax  CurSto  CapRem  TarMax  TarCon PaperFi',
     1  ' RitRem1  RitRem',
     1  '   Cursa  StoAlo  Avail1  AvailX      NA   ACTAF',
     1  '    iwhy Comment'/,
     1  ' Resrg1;', 5(' ____'), 1x, '____________', 8(' _______'),
     1  14 (' _______'), ' ________________________')
  160   FORMAT(/, 80('-'),/,' Resrg1; Avail in')
  170   FORMAT(/,' Resrg1; Avail out')
  180   format(10f8.0)
c
c               Error warnings
 9999 write(6,1050) 
      write(nlog,1051) 
      call flush(6)
 1050 format('    Stopped in Resrg1',/,
     1       '    See the *.log file')
 1051 format('    Stopped in Resrg1')
      write(6,*) 'Stop 1' 
      call flush(6)
      call exit(1)

c
c _________________________________________________________
c
      stop 
      END
