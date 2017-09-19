C
c *********************************************************
c
      SUBROUTINE RESRG1P(IW,L2,ncallx)
c       
c
c _________________________________________________________
c	Program Description
c
c       Resrg1P; Same as Resrg1 but it limits storage by the 
c                amount released to an T&C plan iva an operating rule
c
c _________________________________________________________
c       Update History
c
c rrb 04/10/96; added special logic for out of priority storage 
c               right (ityrsr(l2) = -1)
c     		  Revised to distribute one water right to many owners
c rrb 05/012/01; Revised to allow re storage of previously released
c                reusable water 
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
      character cwhy*24
c
c _________________________________________________________
c
c               Step 1; Initilize
c
c
c		iout = 0 no details
c		       1 details
c                      2 summary      
      iout=0
      if(ichk.eq.130) iout=2
      
      availx = 0.0
      stoalo = 0.0
      actaf  = 0.0
      imcd = -999
      tarcon=-999.0
      
      stoact=0.0
      caprem=0.0
      cursa=0.0
      preuse1=0.0
      preuse2=0.0
      small = 0.001
c
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif
      
      iwhy=0
      cwhy='N/A'      
      
      if(iout.eq.2) write(nlog,140)      
C
c
c _________________________________________________________
c
c               Step 2; Set Destination CODE, OWNER AND STATION
C
      NR  =iopdes(1,L2)
C
      IF(IRESSW(NR).EQ.0) then
        iwhy=1
        cwhy='Reservoir is off'
        Goto 130
      endif  
C
      IOWN=NOWNER(NR)+iopdes(2,L2)-1
      IRCD=IRSSTA(NR)
      NDNR=NDNNOD(IRCD)                     
c
c _________________________________________________________
c
c               Step 2; Set source data
C
      iopr =iopsou(1,L2)
      Preuse1=Preuse(iopr)*fac
C
      IF(preuse1.le.small) then
        iwhy=2
        cwhy='Release to T&C =0'
        Goto 130
      endif  
c _________________________________________________________
c
c               Step 3;  CHECK AVAILABLE WATER AT CURRENT STATION
c
      IF(AVAIL(IRCD).LT.small) then
        iwhy=2
        cwhy='Available at reservoir = 0'
        Goto 130
      endif  
c _________________________________________________________
c
c               Step 4;  FIND REMAINING RESERVOIR CAPACITY
C
      CAPREM=VOLMAX(NR)-CURSTO(NR)
c
c _________________________________________________________
c
c               Step 5;  Find storage available to accounts
      cursa=ownmax(iown)-curown(iown)
c _________________________________________________________
c
c               Step 6; FIND THE ALLOWABLE STORAGE
c
      stoalo=amin1(caprem, cursa, preuse1)
      stocfs=stoalo/fac
      
      IF(stocfs.LT.small) then
        iwhy=3
        cwhy='Allowable storage = 0'
        Goto 130
      endif  
c                                
c _________________________________________________________
c
c               Step 8; Print detailed info
c
      IF(-IOPOUT.eq.IRCD .or. iout.eq.2) then
c       write(nlog,160)
c       write(nlog,180) (AVAIL(ISS)*fac,ISS=1,NUMSTA)
c       write(nlog,140)
        write(nlog,150) 0, iyr,mon,cstaid(ircd),iressw(nr),
     1               iw,nwrord(1,iw),l2,nr,
     1               IRSORD(2,IRCD),IOWN,imcd, 
     1               VOLMAX(NR), CURSTO(NR),caprem,
     1               tarmax(nr), tarcon,
     1               ritrem(l2), cursa, stoalo,
     1               availx, preuse1, actaf, iwhy, cwhy
      endif
c _________________________________________________________
c
c               Step 9; Find MINIMUM FLOW AND ALLOWABLE STORAGE
C
      CALL DNMFSO(maxsta, AVAIL ,IDNCOD,IRCD,NDNR,IMCD)
C
      STOACT=AMIN1(AVAIL(IMCD),STOCFS)
      availx = avail(imcd)*fac
      
      if(stoact.lt.small) then
        iwhy=4
        cwhy='Downstream Available flow = 0'
        goto 130
      endif 
      

      CALL TAKOUT(maxsta, AVAIL ,RIVER ,AVINP ,QTRIBU,IDNCOD,
     1            STOACT, NDNR,  IRCD)
C
c _________________________________________________________
c
c               Step 10; CHECK AVAILABLE FLOW
C
      CALL DNMFSO(maxsta, AVAIL ,IDNCOD,IRCD  ,NDNR  ,IMCD  )

      IF(AVAIL(IMCD).lt.(-1.0*small)) then
        write(nlog,150) -1, iyr,mon,cstaid(ircd),iressw(nr),
     1               iw,nwrord(1,iw),l2,nr,
     1               IRSORD(2,IRCD),IOWN,imcd, 
     1               VOLMAX(NR), CURSTO(NR),caprem,
     1               tarmax(nr), tarcon,
     1               ritrem(l2), cursa, stoalo,
     1               availx, preuse1, actaf, iwhy, cwhy
        goto 9999
      endif
c _________________________________________________________
c
c               Step 11; UPDATE MONTHLY STORAGE FOR EACH OWNER
c
      actaf=stoact*fac
C
      CURSTO(NR  )=CURSTO(NR  )+ACTAF
      QRES(1,NR  )=QRES(1,NR  )+ACTAF
c _________________________________________________________
c
c               Step 12; Distribute to accounts
c
      ownmon(iown)=ownmon(iown)+ actaf
      curown(iown)=curown(iown)+ actaf
      accr(1,iown)=accr(1,iown)+ actaf
      
c
c _________________________________________________________
c               Step 13; Update demand and the diversion by 
c                        this Operating Rule
      Preuse(iopr) = amax1(0.0, Preuse(iopr) - stoact)
      Preuse2=Preuse(iopr)*fac
      DIVO(L2)=DIVO(L2)+stoact
C
c _________________________________________________________
c
c               Step 13; Detailed Printout
c
  130 IF(-IOPOUT.eq.IRCD .or. iout.eq.2) then
        write(nlog,150) 1, iyr,mon,cstaid(ircd),iressw(nr),
     1               iw,nwrord(1,iw),l2,nr,
     1               IRSORD(2,IRCD),IOWN,imcd, 
     1               VOLMAX(NR), CURSTO(NR),caprem,
     1               tarmax(nr), tarcon,
     1               ritrem(l2), cursa, stoalo,
     1               availx, preuse2, actaf, iwhy, cwhy
        if(iout.eq.1) then
          write(nlog,170)
          write(nlog,180) (AVAIL(ISS)*fac,ISS=1,NUMSTA)
        endif  
      endif        
c
c               b. Check reservoir roundoff 
      call chekres(nlog, maxres, 1, 18, iyr, mon, nr, nowner,
     1             curown,cursto,cresid)
c
c		c. More checks      
      if(iout.eq.1) then
        write(nlog,*) '  Resrg1P; mon, actaf', mon, actaf, 
     1    curown(1), curown(2)
      endif
c _________________________________________________________
c
c               Step 14; Return
c
      RETURN
c
c _________________________________________________________
c
c               Formats

  140   FORMAT(/,
     1  ' Resrg1P;   In  iyr  mon cstaid        iressw',
     1  '      iw  nwrord      l2      nr  irsord    iown    imcd',
     1  '  volmax  cursto  caprem  tarmax  tarcon',
     1  '  ritrem   cursa  stoalo  availx preuseX   ACTAF',
     1  '    iwhy Comment'/,
     1  ' Resrg1P;', 3(' ____'), 1x, '____________', 8(' _______'),
     1  12(' _______'), ' ________________________')
  150   format(9x, 3I5,1x, a12,8i8, 11F8.0, i8, 1x, a24)
  160   FORMAT(/, 80('-'),/,' Resrg1P; Avail in')
  170   FORMAT(/,' Resrg1P; Avail out')
  180   format(10f8.0)
c
c               Error warnings
 9999 write(6,1050) 
      write(nlog,1051) 
      call flush(6)
 1050 format('    Stopped in Resrg1P',/,
     1       '    See the *.log file')
 1051 format('    Stopped in Resrg1P')
      write(6,*) 'Stop 1' 
      call flush(6)
      call exit(1)

c
c _________________________________________________________
c
      stop 
      END
