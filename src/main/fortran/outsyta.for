c
c *********************************************************
      SUBROUTINE OUTsyta(IYS,IYE)
c
c
c _________________________________________________________
c	Program Description
c
c       OutsytA; It prints a system report by year
c                Similar to outsyt but it prints average
c _________________________________________________________
c	Dimensions

      include 'common.inc'
C
      character*24 term1(35),term2(30)
c                           
c
      DATA TERM1/
     1   ' Total Demand           ',' CU Demand              ',
     4   ' Fr River by Priority   ',' Fr River by Storage    ',
     6   ' Fr River by Exchange   ',' Fr River Loss          ',
     8   ' Fr Well                ',' Fr Carrier by Priority ',
     1   ' Fr Carrier by Storage  ',' Fr Carrier Loss        ',
     2   ' Carried Water          ',' Fr Soil                ',
     4   ' Total Supply           ',' Total Short            ',
     6   ' CU Short               ',' Consumptive Use        ',
     8   ' To Soil                ',' Total Return           ',
     2   ' Loss                   ',' Upstream Inflow        ',
     2   ' Reach Gain             ',' Return Flow            ',
     4   ' Well Depletion         ',' To / From GW Storage   ',
     6   ' River Inflow           ',' River Divert           ',
     8   ' River by Well          ',' River Outflow          ',
     3   ' Available Flow         ','                        ',
     2   '                        ','                        ',
     4   '                        ','                        ',
     5   '                        '/

      DATA TERM2/
     1   ' Initial Storage        ',' Fr River by Priority   ',
     4   ' Fr River by Storage    ',' Fr River by Exchange   ',
     6   ' Fr Carrier by Priority ',' Fr Carrier by Storage  ',
     8   ' Total Supply           ',' To River for Use       ',
     1   ' To River for Exchange  ',' To Carrier for Use     ',
     2   ' Total Release          ',' Evap                   ',
     4   ' Seep & Spill           ',' EOM Content            ',
     6   ' Target                 ',' One Fill Constraint    ',
     8   ' Decree Limit           ',' River Inflow           ',
     2   ' River Release          ',' River Divert           ',
     2   ' River by Well          ',' River Outflow          ',                          ',
     4   '                        ','                        ',
     6   '                        ','                        ',
     8   '                        ','                        ',
     3   '                        ','                        '/
C
C_______________________________________________________________----
C		Step 1; Initilze
c
      write(6,*) ' '
      write(6,*) ' Subroutine Outsyta'
c
c     ndiv =23
c
c rrb 2005/11/29; River and Carrier Loss
c     ndiv =37
c
c rrb 99/08/16; Revise reservoir to include well and stream data
c     nres = 21
c
c rrb 2005/11/29; River and CArrier Loss
c     nres = 24
c     nres = 26
c
c		Set number of output values 1x in StateM.for
c	        and store in common.inc      
      ndiv = ndivO        
      nres = nresO
      ndivw= nwelO
      

      re=iye-iys+1


      call outtop(16, 1, 19)
C
      WRITE(16,150) cunitm
C
      IPAGE(2)=0
C                                          
      write(6,*) ' '
      do 610 is=1,numsta-1
c
c               Initilize for a given structure
        do im=1,13
          do i=1,ndiv
            data1(im,i)=0.0
          end do
          do i=1,nres
            data2(im,i)=0.0
          end do
        end do

C
        IPAGE(2)=IPAGE(2)+1
        ILINE(2)=50
c
        IDCD=IDNCOD(IS)
C
        IF(IRSORD(2,IS).NE.0) GO TO 361
c
        do iy=iys,iye

          DO IM=1,12
            IRECS=((IY-IYSTR0)*12+(IM-1))*NUMSTA+IS+numtop
            read (43,rec=irecs,err=620) (dat1(i),i=1,ndiv)
c           write(io99,*) 'outsyta dat1(i)', (dat1(i),i=1,ndiv)

            do i=1,ndiv
              data1(im,i)=data1(im,i) + dat1(i) * fmo(im)
              data1(13,i)=data1(13,i) + dat1(i) * fmo(im)*ftot
            end do
          end do
        end do
c
c               Print average diversion data
c ________________________________________________________
c
c         IF(ILINE(2).GE.20) GO TO 250
c
c         IPAGE(2)=IPAGE(2)+1
c         ILINE(2)=50
C
          WRITE(16,160) HEADIN1(1), HEADIN1(2), IPAGE(2)
c
c rrb 10/27/94 Additional Output
          if(idcd.gt.0) then
            write(16,260) stanam1(is),cstaid(is),cstaid(idcd),
     1                  (XMONAM(I),I=1,12)
          else
            write(16,262) stanam1(is),cstaid(is),
     1                  (XMONAM(I),I=1,12)
          endif

          do i=1,2
            WRITE(16,290) TERM1(i),(DATA1(IM,i)/re,IM=1,13)
          end do

          write(16,*) ' '
          do i=3,13
            WRITE(16,290) TERM1(i),(DATA1(IM,i)/re,IM=1,13)
          end do

          write(16,*) ' '
          do i=14,15
            WRITE(16,290) TERM1(i),(DATA1(IM,i)/re,IM=1,13)
          end do

          write(16,*) ' '
          do i=16,19
            WRITE(16,290) TERM1(i),(DATA1(IM,i)/re,IM=1,13)
          end do

          write(16,*) ' '
          do i=20,24
            WRITE(16,290) TERM1(i),(DATA1(IM,i)/re,IM=1,13)
          end do
          
          write(16,*) ' '
          do i=25,29
            WRITE(16,290) TERM1(i),(DATA1(IM,i)/re,IM=1,13)
          end do

          goto 610
C
c              Reservoir Output
c _________________________________________________--
c
c 360     ILINE(2)=ILINE(2)-20
C
  361     IF(NUMRES.EQ.0.OR.NRSACT.EQ.0.OR.
     1      IRSORD(2,IS).EQ.0) GO TO 610
C
          nrsactx = nrsact+numown
c         write(99,*) 'nrsactx, irsord(2,is)'
c         write(99,*)  nrsactx, irsord(2,is)
c
c              Get direct access location for reservoir of interest
        do iy=iys,iye
          ir1 = 0
          do ir=1,numres
            if(iressw(ir).ne.0) then
              if(ir.eq.irsord(1,is)) goto 364
              ir1 = ir1 + nowner(ir+1) - nowner(ir) + 1
            endif
          end do
 364      ir1 = ir1 + 1

          DO IM=1,12  
            IRECR=((IY-IYSTR0)*12+(IM-1))*NRSACTx + ir1 + numtop
            READ (44,REC=IRECR,err=620) (DAT2(I),I=1,nres)
C                                          
            do i=1,nres-3
              DATA2(IM,i)=data2(im,i) + DAT2(i) * fmo(im)
              DATA2(13,I)=DATA2(13,I) + DAT2(i) * fmo(im)*ftot
            end do
          end do
        end do
c
c               Print Average Reservoir
c ________________________________________________________
C
c         IF(ILINE(2).GE.20) GO TO 510
C
c         IPAGE(2)=IPAGE(2)+1
c         ILINE(2)=50
C
          WRITE(16,160) HEADIN1(1), HEADIN1(2), IPAGE(2)
c
  510     continue
          if(idcd.gt.0) then
            write(16,520) resnam1(irsord(1,is)),cstaid(is),
     1                 cstaid(idcd),(xmonam(i),i=1,12)
          else
            write(16,522) resnam1(irsord(1,is)),cstaid(is),
     1                              (xmonam(i),i=1,12)
          endif

          DO I=1,1
            WRITE(16,290) TERM2(I),(DATA2(IM,I)/re,IM=1,13)
          end do

          write(16,*) ' '
          DO I=2,9
            WRITE(16,290) TERM2(I),(DATA2(IM,I)/re,IM=1,13)
          end do

          write(16,*) ' '
          DO I=10,13
            WRITE(16,290) TERM2(I),(DATA2(IM,I)/re,IM=1,13)
          end do

          write(16,*) ' '
          DO I=14,16
            WRITE(16,290) TERM2(I),(DATA2(IM,I)/re,IM=1,13)
          end do

          write(16,*) ' '
          DO I=17,18
            WRITE(16,290) TERM2(I),(DATA2(IM,I)/re,IM=1,13)
          end do

          write(16,*) ' '
          DO I=19,23
            WRITE(16,290) TERM2(I),(DATA2(IM,I)/re,IM=1,13)
          end do

  590     ILINE(2)=ILINE(2)-20
c
c               End year loop for reservoirs
  600   continue
c
c               End Station Loop
  610 CONTINUE

  150 FORMAT(8(/),49X,'***  DETAILED SIMULATION OUTPUT ', a5,' ***')
  101 format('+', '   Printing Year = ', i5)
  160 FORMAT( /,1x, 130('_'),//  '       YEAR ',' Ave',8X,a80,/
     1       24x, a80,15X,'PAGE NO. ',I3)
c 260 format(/,1x, 130('_'),
  260 format(
     1       //,1x,a24,' (',a12,') goes to (',a12,')',
     1       //,25X,12(4X,A4),'    TOTAL',
     1        /,24x,12(' _______'), ' _________')
  262 format(/,1x, 130('_'),
     1       //,1x,a24,' (',a12,') goes to (',12x,')',
     1       //,25X,12(4X,A4),'    TOTAL',
     1        /,24x,12(' _______'), ' _________')

  290 format(a24, 12f8.0, f10.0)
  520 format(/,1x, 130('_'),
     1       //,1x,a24,' (',a12,') goes to (',a12,')',
     1       //,25X,12(4X,A4),'    TOTAL',
     1        /,24x,12(' _______'), ' _________')
  522 format(/,1x, 130('_'),
     1       //,1x,a24,' (',a12,') goes to (',12x,')',
     1       //,25X,12(4X,A4),'    TOTAL',
     1        /,24x,12(' _______'), ' _________')

C
      RETURN
 620  write(6,*)  '   Outsyta; Requested data exceeds binary file size'
      write(99,*) '   Outsyta; Requested data exceeds binary file size'
      write(6,*) 'Stop 1' 
      call flush(6)
      call exit(1)

      stop 
      END

