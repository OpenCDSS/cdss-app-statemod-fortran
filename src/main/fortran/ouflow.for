C
      SUBROUTINE OUFLOW(IP)
c
c
c _________________________________________________________
c	Program Description
c
c       Ouflow; It prints selected matrix data
c _________________________________________________________
c       Update History
c
c rrb 2000/11/28; Changed file 10 to 13 to correct a problem related 
c               to adding an annual time series file *.tsp in mdainp.f
c rrb 2003/07/07; revised treatment of missing data to correct a
c                 problem with the *.xtb report
c
c _________________________________________________________
c	Dimensions
c
       include 'common.inc'
       character cistat*12, recin*256
c      data ihisid/850*0/         
c
c
c _________________________________________________________
c		Step 1; Initilze
c
        iout=0
        unitc = 1.0
        if(iresop.eq.2)  unitc = factor
        if(iresop.eq.3)  unitc = .001* factor
C
C-------------------------------------------------------------------
C
C------  Initilize once per run
C
C------------------------------------------------------------------
c 
c rrb 10/15/95; Initilize once per run
c     IF(IP.NE.0) GO TO 130
      IF(IP.eq.0) then
c
c               Print titles
        call outtop(7, 1, 23)
        call outtop(8, 1, 24)
c       call outtop(10, 1, 25)
        call outtop(13, 1, 25)
c 
c rrb 10/15/95
c          Initilize annual average values
        do 128 is=1,numsta
          do 128 im=1,13
            dum2(im,is) = 0.0
            dum3(im,is) = 0.0
            dummy(is,im)= 0.0
  128     continue
c
c ------------  Identify the number of values in the
c               historic streamflow data 
c               numrun = # of streamflow nodes
c               numhis = # of historic gages
        numhis = 1
        iin2=18
        filena='*.rih'
c
c rrb 2008/03/03; Allow last year to work
cx      read (18,160,end=926,err=928) iryr1
        read (18,160,end=182,err=182) iryr1,cistat
  160       format(i4, 1x, a12, 12f8.0)
        if(iout.eq.1) write(99,*) 'Ouflow; year, numhis, cistat' ,
     1     iryr1, numhis,cistat
        
        do 180 is=1,numrun
c
c rrb 2008/03/03; Allow last year to work
cx        read (18,160,end=926,err=928) iryr, cistat
          read (18,160,end=182,err=182) iryr, cistat
          if(iryr1 .eq. iryr) then
            numhis = numhis + 1
            if(iout.eq.1) write(99,*) 'Ouflow; ',iryr, numhis,cistat
            goto 180
          else
c
c rrb 2008/03/03; Allow last year to work
cx            do 170 i=1,numhis+1
cx              backspace(18)
cx170       continue
            goto 182
c
c rrb 2008/03/03l  Allow last year to work
cx          goto 460
            goto 182
          endif     
  180   continue
c
c ---------------------------------------------------------
c rrb 2008/03/03  
  182   write(nlog,183) numhis
  183   format(/, 72('_'),/
     1   '  Ouflow; Number of historic stream gages = ', i5)
     
        do i=1,numhis+1
          backspace(18)
        end do
        goto 460
c
c               End of once per simulation initilization
      endif
C
C----------------------------------------------------------
C
C------  Begin annual and summary processing
C
C----------------------------------------------------------
C
C
      WRITE(7,200) (XMONAM(I),I=1,12)
  200 FORMAT(/,
     1 'River ID       # Year',6x,11(A4,5X),A4,2X,'TOTAL(KAF)',/,
     1 '----------- ---- ----',12(' --------'),'  ----------')
C
C------  INITIALIZE ARRAYS RIVER AND AVINP
C
      DO 210 IS=1,NUMSTA
        DO 210 I=1,13
          Dum(I,IS)=0.
  210 CONTINUE
C
C------ COMPUTE BASE FLOWS AT EACH STATION
c        Note mdainp adjusts total flow (if provided) to gains 
C
      DO 240 NP=1,NUMRUN
        ISS =IRUSTA(NP)
c
c rrb 10/27/94 Additional Output
        NDNN=NDNNOD(ISS)
        DO 230 ND=1,NDNN
          DO 220 I=1,12        
  220     Dum(I,ISS)=Dum(I,ISS)+VIRINP(I,NP)
  230   ISS=IDNCOD(ISS) 
c
c               Total gains
        iss = irusta(np)
        do 232 i=1,12
c 232     totv(iss) = totv(iss) + virinp(i,np)*mthday(i)*unitc
  232     dummy(iss,3) = dummy(iss,3) + virinp(i,np)*mthday(i)*unitc
  240 CONTINUE
C
C-------------------------------------------------------------------
C
C------  WRITE BASE FLOW DATA TAPE 7
C
C------------------------------------------------------------------
C
      DO 270 IS=1,NUMSTA
        YTOT=0.
        DO 250 I=1,12
c 
c rrb 10/15/95
c         FLOW(I,IS)=FLOW(I,IS)*MTHDAY(I)*FACTOR
          Dum(I,IS)=Dum(I,IS)*MTHDAY(I)*unitc
          YTOT=YTOT+Dum(I,IS)
c 
c rrb 10/15/95
          dum2(i,is) = dum2(i,is) + dum(i,is)
          dum2(13,is) = dum2(13,is) + dum(i,is)
  250   CONTINUE
        YTOT=.001*YTOT
C
        write(7,260) cstaid(is),is,iyr,(dum(i,is),i=1,12),ytot
  260   FORMAT(a12,I4,i5,1X,12(F8.0,1X),F9.3)
C
  270   CONTINUE
C
C-------------------------------------------------------------------
C
C------  WRITE DIVERSION DATA TAPE 8 
C
C------------------------------------------------------------------
C
      WRITE(8,200) (XMONAM(I),I=1,12)
C
C------  INITIALIZE ARRAYS RIVER AND AVINP
C
      DO 280 IS=1,NUMSTA+1
      DO 280 I=1,12
        Dum(I,IS)=0.
  280 CONTINUE
C
C------ COMPUTE DIVERSION DEMAND AT EACH STATION
C
      DO 310 ND=1,NUMDIV
C
        IF(IDIVSW(ND).EQ.0) GO TO 310
C
        NUI=NDUSER(ND)
        NUE=NDUSER(ND+1)-1
C
        IF(NUI.GT.NUE) GO TO 310
C
        ISS =IDVSTA(ND)
C
        DO 300 NU=NUI,NUE
C
          IF(IRTURN(NU).GT.3) GO TO 300
C
          DO 290 I=1,12
            IF(DIVER(I,NU).LT.0.00001) GO TO 290
            Dum(I,ISS)=Dum(I,ISS)+DIVER(I,NU)
            Dum(I,NUMSTA+1)=Dum(I,NUMSTA+1)+DIVER(I,NU)
c           totd(iss) = totd(iss) + dum(i,iss)*mthday(i)*unitc
            dummy(iss,1) = dummy(iss,1) + dum(i,iss)*mthday(i)*unitc
  290     CONTINUE
C
  300   CONTINUE
  310 CONTINUE
C
C ------  Print diversion data
c --------------------------------------------------
      DO 330 IS=1,NUMSTA
        YTOT=0.
        DO 320 I=1,12                   
c
          Dum(I,IS)=Dum(I,IS)*MTHDAY(I)*unitc
          YTOT=YTOT+Dum(I,IS)
  320   CONTINUE

        YTOT=.001*YTOT
        write(8,260) cstaid(is),is,iyr,(dum(i,is),i=1,12),ytot
C
  330 CONTINUE
C
c          Print annual total for all stations
      YTOT=0.
      DO 340 I=1,12
        Dum(I,NUMSTA+1)=Dum(I,NUMSTA+1)*MTHDAY(I)*unitc
  340   YTOT=YTOT+Dum(I,NUMSTA+1)
C
      ytot=ytot*0.001
      WRITE(8,350) iyr, (Dum(I,NUMSTA+1),I=1,12),YTOT
  350 FORMAT('Total',11x,i5,1x,12(F8.0,1X),F9.3)
C
c
c-------------------------------------------------------------------
c
c------  WRITE INSTREAM DATA TAPE 10
c
c------------------------------------------------------------------
c
c     write(10,200) (xmonam(i),i=1,12)
      write(13,200) (xmonam(i),i=1,12)
c
c------  initialize STATION array
c
      do 360 is=1,numsta+1
      do 360 i=1,12
        dum(i,is)=0.
  360 continue
c
c------ compute instream demand at each node
c
      do 380 nf=1,numifr
        iss =ifrsta(nf)

        do 370 i=1,12
          if(flowr(i,nf).lt.0.00001) go to 370
          dum(i,iss)=dum(i,iss)+flowr(i,nf)
          dum(i,numsta+1)=dum(i,numsta+1)+flowr(i,nf)
c         toti(iss) = toti(iss) + flowr(i,nf)*mthday(i)*unitc
          dummy(iss,2) = dummy(iss,2) + flowr(i,nf)*mthday(i)*unitc
  370   continue
c
  380 continue
c
c ------  Print instream flow
c
        do 400 is=1,numsta
          ytot=0.
          do 390 i=1,12
            dum(i,is)=dum(i,is)*mthday(i)*factor
            ytot=ytot+dum(i,is)
  390     continue
          ytot=.001*ytot
c
c         write(10,260) cstaid(is),is,iyr,(dum(i,is),i=1,12),ytot
          write(13,260) cstaid(is),is,iyr,(dum(i,is),i=1,12),ytot
c
  400   continue
C     
c          Print annual total for all stations
        ytot=0.
        do 410 i=1,12
        dum(i,numsta+1)=dum(i,numsta+1)*mthday(i)*factor
  410   ytot=ytot+dum(i,numsta+1)
c
        ytot=ytot*0.001
        write(13,350) iyr, (dum(i,numsta+1),i=1,12),ytot
c
c rrb 10/15/95
c
c-------------------------------------------------------------------
c
c------  READ AND STORE HISTORIC STREAMFLOW DATA
c
c------------------------------------------------------------------
c
        do 450 nh=1,numhis
c
          read (18,160,end=926,err=928) iryr,cistat,(runoff(i),i=1,12)
          if(iryr.ne.iyr) goto 490
c
c               Find streamflow gage
          do is=1,numsta
            if(cistat.eq.cstaid(is)) then
c             write(19,'(1x, a12, 1x, a12)') cistat, cstaid(is)
c             ihisid(nh) = is
              dummy(nh,4) = float(is)

c              
c rrb 99/09/17; Check for missing data
              imiss=0
              do im=1,12
                dum3(im,is) = dum3(im,is) + runoff(im)
c
c rrb 2003/07/07; Revise to be sure dum3(13,is) is not reset to a
c                 positive value when only 1 value in 1 year is missing
c               dum3(13,is) = dum3(13,is) + runoff(im)
                if(dum3(13,is).ge.-0.01) then
                  dum3(13,is) = dum3(13,is) + runoff(im)
                endif
c              
c rrb 99/09/17; Check for missing data
                if(abs(runoff(im)+999.0) .lt. 0.01) imiss = 1
              end do
c              
c rrb 99/09/17; Check for missing data
              if(imiss.eq.1) then
                dum3(13,is)=-999.
c               write(io99,*) ' Ouflow; iryr, is, dum3(13,is)',
c    1            iryr, is,cstaid(is),dum3(13,is)
              endif
              goto 450
            endif
          end do
           
          goto 510         
c           
  450   continue
c
c-------------------------------------------------------------------
c
c------  WRITE Well Demand data (file 12)
c
c------------------------------------------------------------------
c
      write(12,200) (xmonam(i),i=1,12)
c
c------  initialize STATION array
c
      do is=1,numsta+1
        do i=1,12
          dum(i,is)=0.
        end do
      end do
c
c------ compute instream demand at each node
c
      do nw=1,numdivw
        iss =idvstaw(nw)

        do i=1,12
          if(diverw(i,nw).gt.0.00001) then
            dum(i,iss)=dum(i,iss)+diverw(i,nw)
            dum(i,numsta+1)=dum(i,numsta+1)+diverw(i,nw)
c           totw(iss) = totw(iss) + diverw(i,nf)*mthday(i)*unitc
            dummy(iss,5) = dummy(iss,5) + diverw(i,nw)*mthday(i)*unitc
          endif
        end do
      end do
c
c ------  Print well data
c
        do is=1,numsta
          ytot=0.
          do i=1,12
            dum(i,is)=dum(i,is)*mthday(i)*factor
            ytot=ytot+dum(i,is)
          end do

          ytot=.001*ytot
          write(12,260) cstaid(is),is,iyr,(dum(i,is),i=1,12), ytot
c
        end do
C     
c          Print annual total for all stations
        ytot=0.
        do i=1,12
          dum(i,numsta+1)=dum(i,numsta+1)*mthday(i)*factor
          ytot=ytot+dum(i,numsta+1)
        end do
c
        ytot=ytot*0.001
        write(12,350) iyr, (dum(i,numsta+1),i=1,12),ytot

c
c-------------------------------------------------------------------
c
c------  WRITE TABLE DATA
c
c------------------------------------------------------------------
c        
       if(iyr.eq.iyend) then
c                           
c rrb 05/29/96; change to generalize dimensions
c        call outtbl(totd, dum2, toti, dum3, totv, ihisid, numhis)
c        write(6,*) '  Ouflow; calling outtbl'
         call outtbl(numhis)
       endif
c
c _________________________________________________________
c

  460  RETURN
c
c _________________________________________________________
c
c       Error Handling
c
  926 write(99,927) iin2, filena
  927 format(' Ouflow.f; End of file # ', i4, ' encountered',/,
     1       '   File name: ', a256)
      goto 9999
c
  928 write(99,929) iin2, filena
  929 format(' Ouflow.f; Problem reading file # ', i4,/,
     1       '   File name: ', a256,/
     1       '   Problem record (next line):')

      backspace(iin2)
      read(iin2, '(a256)',end=926,err=926) recin
      write(99,'(a256)') recin
      goto 9999


c
c               Year out of order, print error
c
c               Year out of order, print error
  490  write(6,500) cistat,iyr,iryr
       write(99,500) cistat,iyr,iryr
  500     format(
     1    ' Ouflow.for; year problem with historic stream data',/
     1    ' Station ', a12, ' sim year = ', i5, ' data year = ', i5)
       goto 9999
c
c               Could not find station, print error
  510  write(6,520) cistat
       write(99,520) cistat
  520     format(
     1    ' Ouflow.for; station problem with historic stream data',/
     1    ' Station ', a12, ' not found')
          goto 9999


 9999  write(6,9997) 
       write(io99,9998) 
       call flush(6)
 9997  format('  Stopped in Ouflow, see the log file (*.log)')
 9998  format('  Stopped in Ouflow')
       write(6,*) 'Stop 1'
       call flush(6)
       call exit(1)

c
c _________________________________________________________
c

       stop 
      END
