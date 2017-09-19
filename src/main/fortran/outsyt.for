c
      SUBROUTINE OUTSYT(IYS,IYE)
c
c
c _________________________________________________________
c	Program Description
c
c       Outsyt; it prints a system report by year
c
c _________________________________________________________
C	Dimensions
c
      include 'common.inc'
      character*24 term1(35),term2(30)
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
     3   ' Available Flow         ','                        ',                          ',
     2   '                        ','                        ',
     4   '                        ','                        ',
     5   '                        '/



      DATA TERM2/
     1   ' Initial Storage        ',' Fr River by Priority   ',
     4   ' Fr River by Storage    ',' Fr River by Exchange   ',
     6   ' Fr River Loss          ',' Fr Carrier by Priority ',
     8   ' Fr Carrier by Storage  ',' Fr Carrier Loss        ',
     1   ' Total Supply           ',' To River for Use       ',
     2   ' To River for Exchange  ',' To Carrier for Use     ',
     4   ' Total Release          ',' Evap                   ',
     6   ' Seep & Spill           ',' EOM Content            ',
     8   ' Target                 ',' One Fill Constraint    ',
     2   ' River Inflow           ',' River Release          ',
     2   ' River Divert           ',' River by Well          ',
     4   ' River Outflow          ','                        ',
     6   '                        ','                        ',
     8   '                        ','                        ',
     3   '                        ','                        '/
C
c
c _________________________________________________________
c	Initize
C
      write(6,*) ' '
      write(6,*) ' Subroutine Outsyt'
c
c
c		Set number of output values 1x in StateM.for
c	        and store in common.inc      
      ndiv = ndivO        
      nres = nresO
      ndivw= nwelO
c
c		Set ndivP number of columns to print before 
c		    call information (control location and right)
c	 	    e.g. column of Avail Flow)	      
c		Set ndivT number of columns to print in title
c		Set ndivF number of columns to weight by factor

c		Set nrid column of rid, strucutre type 
c		Set nxstr column of xstr,number of structures
c               Set nrimcdX column of rimcdX, call location
c		Set nccallR column of call right
      ndivP=29
      ndivT=31
      ndivF=33
      
      nrid=34
      nxstr=35
      nrimcdx=36
      nccallR=37

      call outtop(16, 1, 19)
c
c rrb 2006/01/05 Print top of file
      call bintop(43,0,16,nchk)      
C
      WRITE(16,150) cunitm
C
      IPAGE(2)=0


C                                          
      write(6,*) ' '
      DO 610 IY=IYS,IYE
c
c rrb 10/27/94 Additional Output
        write(6,101) iy
        call flush(6)
C
        IPAGE(2)=IPAGE(2)+1
        ILINE(2)=50
C
        WRITE(16,160) IY,HEADIN1(1), HEADIN1(2),
     1                IPAGE(2)
C
        do 600 is=1,numsta-1
c
C
          IDCD=IDNCOD(IS)
C
          IF(IRSORD(2,IS).NE.0) GO TO 361
          do i=1,ndiv
            data1(13,i)=0.0
          end do

          DO IM=1,12
            IRECS=((IY-IYSTR0)*12+(IM-1))*NUMSTA+IS+numtop
            read(43,rec=irecs,err=620) (dat1(i),i=1,ndiv)
            do i=1,ndivF
              DATA1(IM,i)=DAT1(i)*fmo(im)
              DATA1(13,I)=DATA1(13,I)+DAT1(i)*fmo(im)*ftot
            end do
          end do
C
          IF(ILINE(2).GE.20) GO TO 250
C
          IPAGE(2)=IPAGE(2)+1
          ILINE(2)=50
C
          WRITE(16,160) IY,HEADIN1(1),HEADIN1(2),
     1                IPAGE(2)
C
c rrb 10/27/94 Additional Output
  250     continue
c
c rrb 01/12/26; Accomodate futile call
          if(idcd.gt.0) then
            write(16,260) stanam1(is),cstaid(is),cstaid(idcd),
     1                (XMONAM(I),I=1,12)
          else
            write(16,262) stanam1(is),cstaid(is),
     1                (XMONAM(I),I=1,12)
          endif

C
  270     continue
c
c		Demand  
          do i=1,2
            WRITE(16,290) TERM1(i),(DATA1(IM,i),IM=1,13)
          end do
c
c		From River, From Well, From Carrier
          write(16,*) ' '
          do i=3,13
            WRITE(16,290) TERM1(i),(DATA1(IM,i),IM=1,13)
          end do
c
c		Shortage
          write(16,*) ' '
          do i=14,15
            WRITE(16,290) TERM1(i),(DATA1(IM,i),IM=1,13)
          end do
c
c		Water Use
          write(16,*) ' '
          do i=16,19
            WRITE(16,290) TERM1(i),(DATA1(IM,i),IM=1,13)
          end do
c
c		Station In
          write(16,*) ' '
          do i=20,24
            WRITE(16,290) TERM1(i),(DATA1(IM,i),IM=1,13)
          end do
c
c		Station Balance an Available Flow
          write(16,*) ' '
          do i=25,29
            WRITE(16,290) TERM1(i),(DATA1(IM,i),IM=1,13)
          end do

C
c              Reservoir Output
c ___________________________________________________
c
  360     ILINE(2)=ILINE(2)-20
C
  361     IF(NUMRES.EQ.0.OR.NRSACT.EQ.0.OR.
     1      IRSORD(2,IS).EQ.0) GO TO 600
C
          nrsactx = nrsact+numown
c         write(99,*) 'nrsactx, irsord(2,is)'
c         write(99,*)  nrsactx, irsord(2,is)
c
c              Get direct access location for reservoir of interest
          ir1 = 0
          do ir=1,numres
            if(iressw(ir).ne.0) then
              if(ir.eq.irsord(1,is)) goto 364
              ir1 = ir1 + nowner(ir+1) - nowner(ir) + 1
            endif
          end do
 364      ir1 = ir1 + 1

          do i=1,nres
            data2(13,i)=0.0
          end do

          DO IM=1,12  
            IRECR=((IY-IYSTR0)*12+(IM-1))*NRSACTx + ir1 + numtop
            READ(44,REC=IRECR,err=620) (DAT2(I),I=1,nres)
C                                          
            do i=1,nres-3
              data2(im,i) = dat2(i) * fmo(im)
              data2(13,i) = data2(13,i) + dat2(i) * fmo(im)*ftot
            end do
          end do
C
          IF(ILINE(2).GE.20) GO TO 510
C
          IPAGE(2)=IPAGE(2)+1
          ILINE(2)=50
C
          WRITE(16,160) IY,HEADIN1(1),HEADIN1(2),
     1              IPAGE(2)
c
  510     continue
          if(idcd.gt.0) then
            write(16,520) resnam1(irsord(1,is)),cstaid(is),
     1                 cstaid(idcd),(xmonam(i),i=1,12)
          else
            write(16,522) resnam1(irsord(1,is)),cstaid(is),
     1                              (xmonam(i),i=1,12)
          endif

C
  530     continue
          DO I=1,1
            WRITE(16,290) TERM2(I),(DATA2(IM,I),IM=1,13)
          end do

          write(16,*) ' '
          DO I=2,9
            WRITE(16,290) TERM2(I),(DATA2(IM,I),IM=1,13)
          end do

          write(16,*) ' '
          DO I=10,13
            WRITE(16,290) TERM2(I),(DATA2(IM,I),IM=1,13)
          end do

          write(16,*) ' '
          DO I=14,15
            WRITE(16,290) TERM2(I),(DATA2(IM,I),IM=1,13)
          end do

          write(16,*) ' '
          DO I=16,18
            WRITE(16,290) TERM2(I),(DATA2(IM,I),IM=1,13)
          end do

          write(16,*) ' '
          DO I=19,23
            WRITE(16,290) TERM2(I),(DATA2(IM,I),IM=1,13)
          end do

  590     ILINE(2)=ILINE(2)-20
C
  600   CONTINUE
C
  610 CONTINUE
c
c _________________________________________________________
c
c       Formats
c
  150 FORMAT(8(/),49X,'***  DETAILED SIMULATION OUTPUT ', a5,' ***')
  101 format('+', '   Printing Year = ', i5)
  160 FORMAT(    ' WATER YEAR ',I4,8X,a80,/,
     1       24x, a80,15X,'PAGE NO. ',I3)
  260 format(/,1x, 130('_'),
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

c
c _________________________________________________________
c

      RETURN
 620  write(6,*)  '   Outsyt; Requested data exceeds binary file size'
      write(99,*) '   Outsyt; Requested data exceeds binary file size'
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)
c
c _________________________________________________________
      stop 
      END

