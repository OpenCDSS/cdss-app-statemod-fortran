
c
c _________________________________________________________
c	Update History
c

c
c _________________________________________________________
c	Documentation
c

c
c _________________________________________________________
c	Dimensions
cC     Last change:  C    20 May 97    0:14 am
C
      SUBROUTINE RESRG2(IW,L2)
c
c _________________________________________________________
c	Program Description
c       
c               Standard reservoir routine.  
c               Limits storage to target contents and does NOT
c               charge paper fill (storable water above target)
c
c _________________________________________________________
c	Update History
c
c rrb 04/10/96; added special logic for 
c
c     Revised to distribute one water right to many owners
C
c
c _________________________________________________________
c	Documentation
C
C        KEY DEFINITION
C
C        IW         : OVERALL WATER RIGHT ORDER
C        L2         : LOC. OF WATER RIGHT (IW) ON RES. RIGHT TABLE
c        nrown(l2)  : Number of owners associated with right l2
c                     begins with owner 1 of that reservoir but may
c                     not include all owners of a given reseroir
C
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
c
c _________________________________________________________
c		Step 1; Initilize
c
      availx = 0.0
      stoalo = 0.0
      actaf  = 0.0
      imcd = -999
      tarcon=-999.0
      f = MTHDAY(MON)*FACTOR
C
C------  FIND RESERVOIR CODE, ASSOCIATED OWNER AND STATION CODE
C
      NR  =IRESCO(1,L2)
C
      IF(IRESSW(NR).EQ.0) Goto 130
C
      IOWN=NOWNER(NR)+IRESCO(2,L2)-1
      IRCD=IRSSTA(NR)
      NDNR=NDNNOD(IRCD)                                         
C
C------  CHECK AVAILABLE WATER AT CURRENT STATION
C
      IF(AVAIL(IRCD).LT.0.00001) Goto 130
C
C------  FIND REMAINING RESERVOIR CAPACITY
C
      CAPREM=VOLMAX(NR)-CURSTO(NR)
c
c grb 06/05/95 sum remaining storage usable by right
      cursa=0.0
      do 100 n=1,nrown(l2)    
        n1 = iown+n-1
        cursa=cursa+ownmax(n1)-curown(n1)
  100 continue
C
C------  FIND THE ALLOWABLE STORAGE
C grb 12-04-94  also limit to target contents
c
c rrb 04/03/96; Do not limit by target if reservoir type 3 
c               (Used for paper fill (fill then spill) considerations)
      if(iressw(nr).eq.3) then
        stoalo=amin1(caprem,ritrem(l2),cursa)
      else
        tarcon=tarmax(nr)-cursto(nr)
        stoalo=amin1(caprem,tarcon,ritrem(l2),cursa)
      endif
c                                
c               Special constraint for out of priority storage
      if(ityopr(l2).eq.-1) then                             
        l3=iopid(l2)
        ndwro = intern(l3,1)
        nrwro = iopsou(3,l3)
c
c               Determine limitations of right remaining in both
c               destination right (Dillon) and subordinated right (GM)
        stoalo=amin1(stoalo,ritrem(ndwro),ritrem(nrwro))
      endif

      STOALO=AMAX1(STOALO,0.)
      STOCFS=STOALO/MTHDAY(MON)/FACTOR
C
c               Print detailed info, if desired
      IF(-IOPOUT.eq.IRCD) then
        write(99,160)
        write(99,180) (AVAIL(ISS)*f,ISS=1,NUMSTA)
        write(99,140)
        write(99,150) 0, iyr,mon,cstaid(ircd),iressw(nr),
     1               iw,nwrord(1,iw),l2,nr,
     1               IRSORD(2,IRCD),IOWN,imcd, 
     1               VOLMAX(NR), CURSTO(NR),caprem,
     1               tarmax(nr), tarcon,
     1               ritrem(l2), cursa, stoalo,
     1               availx, actaf
      endif
C
C-------------------------------------------------------------------
C
C------  CHECK DOWNSTREAM MINIMUM FLOW AND ALLOWABLE STORAGE
C
C-------------------------------------------------------------------
C
      CALL DNMFSO(maxsta, AVAIL ,IDNCOD,IRCD,NDNR,IMCD)
C
      STOACT=AMIN1(AVAIL(IMCD),STOCFS)
      availx = avail(imcd)*MTHDAY(MON)*FACTOR
C
      CALL TAKOUT(maxsta, AVAIL ,RIVER ,AVINP ,QTRIBU,IDNCOD,
     1            STOACT, NDNR,  IRCD)
C
C------  CHECK AVAILABLE FLOW
C
      CALL DNMFSO(maxsta, AVAIL ,IDNCOD,IRCD  ,NDNR  ,IMCD)
      IF(AVAIL(IMCD).lt.-.00001) then
        write(99,150) 0, iyr,mon,cstaid(ircd),iressw(nr),
     1               iw,nwrord(1,iw),l2,nr,
     1               IRSORD(2,IRCD),IOWN,imcd, 
     1               VOLMAX(NR), CURSTO(NR),caprem,
     1               tarmax(nr), tarcon,
     1               ritrem(l2), cursa, stoalo,
     1               availx, actaf

        AVAIL(IMCD)=0.
      endif
C
C------  UPDATE MONTHLY STORAGE FOR EACH OWNER
C
      ACTAF=STOACT*MTHDAY(MON)*FACTOR
c
c  grb 06/05/95 return if no water or space available
      if (stoact.le.(.01)) goto 130
C
      RITREM(L2  )=RITREM(L2  )-ACTAF
      CURSTO(NR  )=CURSTO(NR  )+ACTAF
      QRES(1,NR  )=QRES(1,NR  )+ACTAF
c
c              Distribute right based on ownership ratio
c              Test amount of space (physical and legal) available
      ct = 0.0                                                    
      vola = 0.0                   
      do 110 n=1,nrown(l2)
        n1 = iown+n-1
c
c rrb 04/10/96
c       c = actaf*amin1(ownrem(n1),ownmax(n1)-curown(n1))/cursa
        c = actaf*(ownmax(n1)-curown(n1))/cursa
        ct = ct+c
c       ownrem(n1)=ownrem(n1)-c
        ownmon(n1)=ownmon(n1)+c
        curown(n1)=curown(n1)+c
        qmains(1,n1)=qmains(1,n1)+c
        accr(1,n1)=accr(1,n1)+c
c
c rrb 04/10/96
c       vola = vola + amin1(ownrem(n1),ownmax(n1)-curown(n1))
        vola = vola + ownmax(n1)-curown(n1)
  110 continue
c
c              Distribute unallocated right (if any) 
c              based on ownership space
      if(actaf-ct .gt. 0.1) then
        crem = actaf-ct
      do 120 n=1,nrown(l2)
          n1 = iown+n-1
c
c rrb 04/10/96
c         c = crem*amin1(ownrem(n1),ownmax(n1)-curown(n1))/vola
          c = crem*(ownmax(n1)-curown(n1))/vola
          ct = ct+c
     
c         ownrem(n1)=ownrem(n1)-c
          ownmon(n1)=ownmon(n1)+c
          curown(n1)=curown(n1)+c
          qmains(1,n1)=qmains(1,n1)+c
          accr(1,n1)=accr(1,n1)+c
  120   continue 
      endif

      if (actaf-ct.gt.0.1) then
        write(6,*) 'Resrigx; Problem in allocating a right to owners',
     1 'actaf,ct,crem, nrown',actaf,ct,crem,nrown(l2)
        write(6,*) 'Stop 1' 
        call flush(6)
        call exit(1)

        stop 
      endif
C
  130 IF(-IOPOUT.eq.IRCD) then
        write(99,150) 1, iyr,mon,cstaid(ircd),iressw(nr),
     1               iw,nwrord(1,iw),l2,nr,
     1               IRSORD(2,IRCD),IOWN,imcd, 
     1               VOLMAX(NR), CURSTO(NR),caprem,
     1               tarmax(nr), tarcon,
     1               ritrem(l2), cursa, stoalo,
     1               availx, actaf
        write(99,170)
        write(99,180) (AVAIL(ISS)*f,ISS=1,NUMSTA)
      endif
c
      RETURN
c
c               Formats
  140   FORMAT(/,
     1  '  Resrg1;  In, iyr, mon,      cstaid, iressw,',
     1  '     iw, nwrord,     l2,     nr, irsord,   iown,   imcd,',
     1  ' volmax, cursto, caprem,',
     1  ' tarmax, tarcon,',
     1  ' ritrem,  cursa, stoalo,',
     1  'availx,   actaf,')
  150   format(9x, 3I5,1x, a12,8i8, 20F8.0)
  160   FORMAT(/, 80('-'),/,' Resrg1; Avail in')
  170   FORMAT(/,' Resrg1; Avail out')
  180   format(10f8.0)

      END
