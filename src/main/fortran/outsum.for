
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
cC     Last change:  C    20 May 97    0:09 am
C
c *********************************************************
c
      SUBROUTINE OUTSUM(IYS,IYE)
c
c
c _________________________________________________________
c	Program Description
c
c       Outsum; It prints a diversion file as a matrix
c               Need to revise per new binary file output (tape 43 & 44)
c               and reservoir account info in tape 43
c
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
C
      character*20 term1(18),term2(16)
C
      CHARACTER*40 TERM3(5)
C
      DATA TERM1/' FLOW FR UPSTRM NODE','                    ',
     1           ' REACH GAIN         ',' RETURN FLOW        ',
     2           'TRANSMT DEMAND      ',' NATURAL FLOW SUPPLY',
     3           ' STORAGE WATER      ','INBASIN DEMAND      ',
     4           ' NATURAL FLOW SUPPLY',' STORAGE WATER      ',
     5           'SHORT - DEMAND      ','BYPASS REQ.         ',
     6           'OUTFLOW             ','INSTREAM DEMAND      ',
     7           ' NATURAL FLOW SUPPLY',' STORAGE WATER      ',
     8           'CONSTRAINED DEMAND  ','SHORT - CONSTRAINED '/
C
      DATA TERM2/' NATURAL INFLOW     ',' DIVERSIONS         ',
     1           'EVAPORATION         ','PUMPING RELEASE     ',
     2           ' NATURAL FLOW SUPPLY',' STORAGE WATER      ',
     3           'DOWNSTRM RELEASE    ',' TRANSMT USE        ',
     4           ' IN-BASIN USE       ',' IN-STREAM USE      ',
     5           ' SEEPAGE            ','BYPASS REQ.         ',
     6           ' SPILL              ','OUT-OF-PR DIV W/ RPL',
     7           'EOM CONTENT         ','(PAPER FILL)        '/
C
      DATA TERM3/'STATION INFLOW                          ',
     1           '                                        ',
     2           '                                        ',
     3           '                                        ',
     4           'RESERVOIR INFLOW                        '/
c
c _________________________________________________________
c		Step 1; Initilze
c
c rrb 10/27/94 Additional Output
      write(6,*) ' '
      write(6,*) ' Subroutine Outsum'
c
c		Set number of output values 1x in StateM.for
c	        and store in common.inc      
      ndiv = ndivO        
      nres = nresO
      ndivw= nwelO
      

      IW=12
      call outtop(iw, 1, 2)
C
      WRITE(12,100) cunitm
  100 FORMAT(8(/),49X,'***  SUMMARY OF SIMULATION RESULTS ', a5,
     1              '  ***',/,'1')
C
      IPAGE(2)=1
      ILINE(2)=57
C
      WRITE(12,110) IYS,IYE,HEADIN1(1),(IDATE(I),I=1,3),
     1              (ITIME(I),I=1,4),HEADIN1(2),
     1              IPAGE(2),(XMONAM(I),I=1,12)
c
c 110 FORMAT('',' WATER YEAR ',I4,'-',I4,5X,20A4,2X,I2,'-',
  110 FORMAT(    ' WATER YEAR ',I4,'-',I4,5X,a80,2X,I2,'-',
     1       I2,'-',I2,' MST ',I2,':',I2,':',I2,'.',I2,/,26X,
     1       a80,14X,'PAGE NO. ',I3,//,26X,12(4X,A4),'TOTAL(KAF)')
C             
      write(6,*)
c     write(99,*) 'Outsum; Numsta = ', numsta
      DO 680 IS=1,NUMSTA
c
c rrb 10/27/94 Additional Output
      c = float(is)/float(numsta)*100.0
      write(6,120) is, numsta, c
      call flush(6)
  120 format('+', '   Printing Node Accounting  ',i5,' of ', i5,
     1            '; or ',f8.0, ' % Complete')
C
      IDCD=IDNCOD(IS)
C
      IF(IRSORD(2,IS).NE.0) GO TO 430
C
c rrb 11/29/94 Additional Output
      do 130 it=1,20
      DO 130 IM=1,12
  130 DATA1(IM,IT)=0.
C
      DO 150 IY=IYS,IYE
      DO 140 IM=1,12
c
c rrb 12/12/95; Variable report year and date stamp for diversions
c     IRECS=((IY-IYSTR)*12+(IM-1))*NUMSTA+IS
c
c rrb 06/06/96; Include header information
c     IRECS=((IY-IYSTR0)*12+(IM-1))*NUMSTA+IS+1
      IRECS=((IY-IYSTR0)*12+(IM-1))*NUMSTA+IS+numtop
c
c rrb 01/05/95; I/O Addition
      read (43,rec=irecs,err=690) (dat1(i),i=1,ndiv)
C
      DATA1(IM,1 )=DATA1(IM,1 )+DAT1(1 )
      DATA1(IM,2 )=DATA1(IM,2 )+DAT1(2 )
      DATA1(IM,3 )=DATA1(IM,3 )+DAT1(3 )
      DATA1(IM,4 )=DATA1(IM,4 )+DAT1(4 )
      DATA1(IM,5 )=DATA1(IM,5 )+DAT1(7 )
      DATA1(IM,6 )=DATA1(IM,6 )+DAT1(12)
      DATA1(IM,7 )=DATA1(IM,7 )+DAT1(14)
      DATA1(IM,8 )=DATA1(IM,8 )+DAT1(6 )
      DATA1(IM,9 )=DATA1(IM,9 )+DAT1(9 )
      DATA1(IM,10)=DATA1(IM,10)+DAT1(11)
      DATA1(IM,11)=DATA1(IM,11)+DAT1(15)
      DATA1(IM,12)=DATA1(IM,12)+DAT1(21)-DAT1(22)
      DATA1(IM,13)=DATA1(IM,13)+DAT1(21)
c
c rrb 11/29/94 Additional Output
      data1(im,14)=data1(im,14)+dat1(16)
      data1(im,15)=data1(im,15)+dat1(18)
      data1(im,16)=data1(im,16)+dat1(19)
      data1(im,17)=data1(im,17)+dat1(23)
      data1(im,18)=data1(im,18)+dat1(24)
  140 CONTINUE
C
  150 CONTINUE
C
      do 160 it=1,20
      DO 160 IM=1,12
  160 DATA1(IM,IT)=DATA1(IM,IT)/(IYE-IYS+1)
C
      do 200 i=1,20
      DATA1(13,I)=0.
C
      if(iresop.eq.1) then
        DO IM=1,12
c         DATA1(13,I)=DATA1(13,I)+DATA1(IM,I)*FACTOR*MTHDAY(IM)
          DATA1(13,I)=DATA1(13,I)+DATA1(IM,I)*fmo(im)
        end do
        DATA1(13,I)=.001*DATA1(13,I)
        GO TO 200
      endif
C          
      if(iresop.eq.2) then
        DO IM=1,12
c         DATA1(IM,I)=DATA1(IM,I)*FACTOR*MTHDAY(IM)
          DATA1(IM,I)=DATA1(IM,I)*fmo(im)
          DATA1(13,I)=DATA1(13,I)+DATA1(IM,I)
        end do
        DATA1(13,I)=.001*DATA1(13,I)
        GO TO 200
      endif
C     
      if(iresop.eq.3) then
        DO IM=1,12
c         DATA1(IM,I)=.001*DATA1(IM,I)*FACTOR*MTHDAY(IM)
          DATA1(IM,I)=.001*DATA1(IM,I)*fmo(im)
          DATA1(13,I)=DATA1(13,I)+DATA1(IM,I)
        end do
        go to 200
      endif
C
  200 CONTINUE
C
      IF(ILINE(2).GE.16) GO TO 210
C
      IPAGE(2)=IPAGE(2)+1
      ILINE(2)=57
C
      WRITE(12,110) IYS,IYE,HEADIN1(1),(IDATE(I),I=1,3),
     1              (ITIME(I),I=1,4),HEADIN1(2),
     1              IPAGE(2),(XMONAM(I),I=1,12)
C
  210 continue
      if(idcd.gt.0)
     1  write(12,220) stanam1(is),cstaid(is),cstaid(idcd)
  220 format(/,1x,a24,' (',a12,') [GO TO (',a12,')]',76('-'))
C
c              CFS output
c ------------------------------------------
      if(iresop.eq.1) then
        WRITE(12,230) TERM3(1)
  230   FORMAT(3X,A40)
        WRITE(12,250) TERM1(1),(DATA1(IM,1),IM=1,13)

        DO 240 I=3,4
  240   WRITE(12,250) TERM1(I),(DATA1(IM,I),IM=1,13)
  250   FORMAT(3X,A20,2X,12F8.1,F10.3)
c
c rrb 01/05/95; I/O Addition; CONSTRAINED DEMAND
        write(12,230) term3(2)
        write(12,250) term1(17),(data1(im,17),im=1,13)
C
        WRITE(12,230) TERM3(3)
        DO 260 I=5,7
  260   WRITE(12,250) TERM1(I),(DATA1(IM,I),IM=1,13)
C
        WRITE(12,230) TERM3(3)
        DO 270 I=8,10
  270   WRITE(12,250) TERM1(I),(DATA1(IM,I),IM=1,13)
c
c rrb 11/29/94 Additional Output; Instream demand
        write(12,230) term3(3)
        do 280 i=14,16
  280   write(12,250) term1(i),(data1(im,i),im=1,13)
C
        WRITE(12,230) TERM3(3)
        WRITE(12,250) TERM1(11),(DATA1(IM,11),IM=1,13)
c
c rrb 01/05/95; I/O Addition; constrained shortage
        write(12,250) term1(18),(data1(im,18),im=1,13)
C
        WRITE(12,230) TERM3(3)
        DO 290 I=12,13
  290   WRITE(12,250) TERM1(I),(DATA1(IM,I),IM=1,13)
C
        GO TO 420
      endif
C          
C              AF output      
c ------------------------------------------
      if(iresop.eq.2) then
        WRITE(12,230) TERM3(1)
        WRITE(12,310) TERM1(1),(DATA1(IM,1),IM=1,13)
C
        DO 300 I=3,4
  300   WRITE(12,310) TERM1(I),(DATA1(IM,I),IM=1,13)
  310   FORMAT(3X,A20,2X,12F8.0,F10.3)
c
c rrb 01/05/95; I/O Addition; CONSTRAINED DEMAND
        write(12,230) term3(2)
        write(12,310) term1(17),(data1(im,17),im=1,13)
C
        WRITE(12,230) TERM3(3)
        DO 320 I=5,7
  320   WRITE(12,310) TERM1(I),(DATA1(IM,I),IM=1,13)
C
        WRITE(12,230) TERM3(3)
        DO 330 I=8,10
  330   WRITE(12,310) TERM1(I),(DATA1(IM,I),IM=1,13)
c
c rrb 11/29/94 Additional Output; Instream demand
        write(12,230) term3(3)
        do 340 i=14,16
  340   write(12,310) term1(i),(data1(im,i),im=1,13)
C
        WRITE(12,230) TERM3(3)
        WRITE(12,310) TERM1(11),(DATA1(IM,11),IM=1,13)
c
c rrb 01/05/95; I/O Addition; constrained shortage
        write(12,310) term1(18),(data1(im,18),im=1,13)
C
        WRITE(12,230) TERM3(3)
        DO 350 I=12,13
  350   WRITE(12,310) TERM1(I),(DATA1(IM,I),IM=1,13)
C
        GO TO 420
      endif
C
c              KAF Output
c ------------------------------------------
      if(iresop.eq.3) then
        WRITE(12,230) TERM3(1)
        WRITE(12,370) TERM1(1),(DATA1(IM,1),IM=1,13)
C
        DO 360 I=3,4
  360   WRITE(12,370) TERM1(I),(DATA1(IM,I),IM=1,13)
  370   FORMAT(3X,A20,2X,12F8.3,F10.3)
c
c rrb 01/05/95; I/O Addition; CONSTRAINED DEMAND
        write(12,230) term3(2)
        write(12,370) term1(17),(data1(im,17),im=1,13)
C
        WRITE(12,230) TERM3(3)
        DO 380 I=5,7
  380   WRITE(12,370) TERM1(I),(DATA1(IM,I),IM=1,13)
C
        WRITE(12,230) TERM3(3)
        DO 390 I=8,10
  390   WRITE(12,370) TERM1(I),(DATA1(IM,I),IM=1,13)
c
c rrb 11/29/94 Additional Output; Instream demand
        write(12,230) term3(3)
        do 400 i=14,16
  400   write(12,370) term1(i),(data1(im,i),im=1,13)
C
        WRITE(12,230) TERM3(3)
        WRITE(12,370) TERM1(11),(DATA1(IM,11),IM=1,13)
c
c rrb 01/05/95; I/O Addition; constrained shortage
        write(12,370) term1(18),(data1(im,18),im=1,13)
C
        WRITE(12,230) TERM3(3)
        DO 410 I=12,13
  410   WRITE(12,370) TERM1(I),(DATA1(IM,I),IM=1,13)
        goto 420
      endif
C
  420   ILINE(2)=ILINE(2)-16
c
c              Reservoir Output
c -----------------------------------------------
C
  430 IF(NUMRES.EQ.0.OR.NRSACT.EQ.0.OR.IRSORD(2,IS).EQ.0) GO TO 680
C
      DO 440 IT=1,16
      DO 440 IM=1,12
  440 DATA2(IM,IT)=0.
C
      DO 460 IY=IYS,IYE
C
      DO 450 IM=1,12
c
c rrb 12/12/95; Variable report year for reservoirs
c     IRECR=((IY-IYSTR)*12+(IM-1))*NRSACT+IRSORD(2,IS)
c     IRECR=((IY-IYSTR0)*12+(IM-1))*NRSACT+IRSORD(2,IS)
      IRECR=((IY-IYSTR0)*12+(IM-1))*NRSACT+IRSORD(2,IS)+numtop

      READ (44,REC=IRECR,err=690) (DAT2(I),I=1,nres)
C
      DATA2(IM,1 )=DATA2(IM,1 )+DAT2(1 )
      DATA2(IM,2 )=DATA2(IM,2 )+DAT2(4 )+DAT2(6 )
      DATA2(IM,3 )=DATA2(IM,3 )+DAT2(15)
      DATA2(IM,4 )=DATA2(IM,4 )+DAT2(13)+DAT2(23)+DAT2(30)
      DATA2(IM,5 )=DATA2(IM,5 )+DAT2(23)+DAT2(30)
      DATA2(IM,6 )=DATA2(IM,6 )+DAT2(13)
      DATA2(IM,7 )=DATA2(IM,7 )+DAT2(18)
      DATA2(IM,8 )=DATA2(IM,8 )+DAT2(11)+DAT2(12)
      DATA2(IM,9 )=DATA2(IM,9 )+DAT2(10)
      DATA2(IM,10)=DATA2(IM,10)+DAT2(17)+DAT2(14)+DAT2(24)
      DATA2(IM,11)=DATA2(IM,11)+DAT2(26)
      DATA2(IM,12)=DATA2(IM,12)+DAT2(25)
      DATA2(IM,13)=DATA2(IM,13)+DAT2(16)
      DATA2(IM,14)=DATA2(IM,14)+DAT2( 5)+DAT2(30)
      DATA2(IM,15)=DATA2(IM,15)+DAT2(19)
      DATA2(IM,16)=DATA2(IM,16)+DAT2(27)+DAT2(28)+DAT2(29)
  450 CONTINUE
C
  460 CONTINUE
C
      DO 470 IT=1,16
      DO 470 IM=1,12
  470 DATA2(IM,IT)=DATA2(IM,IT)/(IYE-IYS+1)
C
      DO 520 I=1,16
      DATA2(13,I)=0.
C
c              CFS Output
c ------------------------------------------
      if(iresop.eq.1) then
        DO IM=1,12
c         DATA2(13,I)=DATA2(13,I)+DATA2(IM,I)*FACTOR*MTHDAY(IM)
          DATA2(13,I)=DATA2(13,I)+DATA2(IM,I)*fmo(im)
        end do
        DATA2(13,I)=.001*DATA2(13,I)
        GO TO 520
      endif
C          
c              AF Output
c ------------------------------------------
      if(iresop.eq.2) then
        DO IM=1,12
c         DATA2(IM,I)=DATA2(IM,I)*FACTOR*MTHDAY(IM)
          DATA2(IM,I)=DATA2(IM,I)*fmo(im)
          DATA2(13,I)=DATA2(13,I)+DATA2(IM,I)
        end do
        DATA2(13,I)=.001*DATA2(13,I)
        GO TO 520
      endif
C                         
c              KAF Output
c ------------------------------------------
      if(iresop.eq.3) then
  500   DO IM=1,12
c         DATA2(IM,I)=.001*DATA2(IM,I)*FACTOR*MTHDAY(IM)
          DATA2(IM,I)=.001*DATA2(IM,I)*fmo(im)
          DATA2(13,I)=DATA2(13,I)+DATA2(IM,I) 
        end do
        go to 520
      endif
C
  520 CONTINUE
C
      IF(ILINE(2).GE.17) GO TO 530
C
      IPAGE(2)=IPAGE(2)+1
      ILINE(2)=57
C
      WRITE(12,110) IYS,IYE,HEADIN1(1),(IDATE(I),I=1,3),
     1              (ITIME(I),I=1,4), HEADIN1(2),
     1              IPAGE(2),(XMONAM(I),I=1,12)
C
  530 write(12,540) resnam1(irsord(1,is)),cstaid(is),
     1               cstaid(idcd)
  540 format(/,1x,a24,' (',a12,') [GO TO (',a12,')]',76('+'))
C
      if(iresop.eq.1) then
        WRITE(12,230) TERM3(5)
        DO 550 I=1,2
  550   WRITE(12,250) TERM2(I),(DATA2(IM,I),IM=1,13)
C
        WRITE(12,230) TERM3(3)
        WRITE(12,250) TERM2(3),(DATA2(IM,3),IM=1,13)
C
        WRITE(12,230) TERM3(3)
        DO 560 I=4,6
  560   WRITE(12,250) TERM2(I),(DATA2(IM,I),IM=1,13)
C
        WRITE(12,230) TERM3(3)
c
        do 570 i=7,11
  570   write(12,250) term2(i),(data2(im,i),im=1,13)
        write(12,250) term2(13),(data2(im,13),im=1,13)
C
        WRITE(12,230) TERM3(3)
        WRITE(12,250) TERM2(14),(DATA2(IM,14),IM=1,13)
C
        DO 580 I=15,16
  580   WRITE(12,250) TERM2(I),(DATA2(IM,I),IM=1,12)
c
c rrb 01/05/95; I/O Addition
        write(12,230) term3(3)
        write(12,250) term2(12),(data2(im,12),im=1,13)
C
        GO TO 670
      endif
C          
c              AF Output
c ------------------------------------------
      if(iresop.eq.2) then
        WRITE(12,230) TERM3(5)
        DO 590 I=1,2
  590   WRITE(12,310) TERM2(I),(DATA2(IM,I),IM=1,13)
C
        WRITE(12,230) TERM3(3)
        WRITE(12,310) TERM2(3),(DATA2(IM,3),IM=1,13)
C
        WRITE(12,230) TERM3(3)
        DO 600 I=4,6
  600   WRITE(12,310) TERM2(I),(DATA2(IM,I),IM=1,13)
C
        WRITE(12,230) TERM3(3)
c
c rrb 01/05/95; I/O Addition
        do 610 i=7,11
  610   write(12,310) term2(i),(data2(im,i),im=1,13)
        write(12,310) term2(13),(data2(im,13),im=1,13)
C
        WRITE(12,230) TERM3(3)
        WRITE(12,310) TERM2(14),(DATA2(IM,14),IM=1,13)
C
        DO 620 I=15,16
  620   WRITE(12,310) TERM2(I),(DATA2(IM,I),IM=1,12)
c
c rrb 01/05/95; I/O Addition
        write(12,230) term3(3)
        write(12,310) term2(12),(data2(im,12),im=1,13)
C
        GO TO 670
      endif
C 
c              KAF Output
c ------------------------------------------
      if(iresop.eq.3) then
        WRITE(12,230) TERM3(5)
        DO 630 I=1,2
  630   WRITE(12,370) TERM2(I),(DATA2(IM,I),IM=1,13)
C
        WRITE(12,230) TERM3(3)
        WRITE(12,370) TERM2(3),(DATA2(IM,3),IM=1,13)
C
        WRITE(12,230) TERM3(3)
        DO 640 I=4,6
  640   WRITE(12,370) TERM2(I),(DATA2(IM,I),IM=1,13)
C
        WRITE(12,230) TERM3(3)
c
c rrb 01/05/95; I/O Addition
        do 650 i=7,11
  650   write(12,370) term2(i),(data2(im,i),im=1,13)
        write(12,370) term2(13),(data2(im,13),im=1,13)
C
        WRITE(12,230) TERM3(3)
        WRITE(12,370) TERM2(14),(DATA2(IM,14),IM=1,13)
C
        DO 660 I=15,16
  660   WRITE(12,370) TERM2(I),(DATA2(IM,I),IM=1,12)
c
c rrb 01/05/95; I/O Addition
        write(12,230) term3(3)
        write(12,370) term2(12),(data2(im,12),im=1,13)
        go to 670
      endif
C
  670 ILINE(2)=ILINE(2)-16
C
  680 CONTINUE
C
      RETURN
 690  write(6,*)  '   Outsum; Requested data exceeds binary file size'
      write(99,*) '   Outsum; Requested data exceeds binary file size'
      write(6,*) 'Stop 1' 
      call flush(6)
      call exit(1)

      stop 
      END
