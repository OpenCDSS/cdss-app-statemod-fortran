c
c _________________________________________________________
c	Program Description
c

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
c *********************************************************
C
      SUBROUTINE OUTSYS(IYS,IYE)
c
c _________________________________________________________
c
c       Description
c       It prints an Ascii Version of the Binary Diversion
c       and Well File
c
c _________________________________________________________
c
c       Update History
c       2003/10/28; Revised to not multiply ndiv-2 elements
c                   by the unit conversion
c
c _________________________________________________________
c
      include 'common.inc'
C
C
C-------------------------------------------------------------------
C
C------  SYSTEM FINAL REPORT - TAPE 25
C
C-------------------------------------------------------------------
C
      write(6,*) ' '
      write(6,*) ' Subroutine Outsys'

      IW=25
c
c
c		Set number of output values 1x in StateM.for
c	        and store in common.inc      
c		NresX number of values to adjust by a factor
      ndiv = ndivO        
      nres = nresO
      ndivw= nwelO
      
      nresX=nres-3
      
      call outtop(25,1,18)
      call outtop(26,1,49)
C
c rrb 10/31/03; Print header
      call bintop(43,1,25,ichk)
      call bintop(44,1,26,ichk)
      DO 150 IY=IYS,IYE
c
c rrb 10/27/94 Additional Output
        write(6,170) iy
        call year(iy, iyrmo, imomo, cyr1)

        call flush(6)
C
        DO 140 IM=1,12     
          WRITE(25,180) cunitm, IYrmo(im),xmonam(im)
C      
          DO IS=1,NUMSTA
            IRECS=((IY-IYSTR0)*12+(IM-1))*NUMSTA+IS+numtop
            READ(43,REC=IRECS,err=210) (DAT1(I),I=1,ndiv)
C
c rrb 2003/10/28; Do not multiply station value and number of
c                 structures by factor
c           DO I=1,ndiv

            DO I=1,ndiv-2
c             DAT1(I)=DAT1(I)*MTHDAY(IM)*FACTOR
              DAT1(I)=DAT1(I)*fmo(im)
            end do
C
c rrb 2006/01/20; Add line number
c           write(25,190) is,cstaid(is),(dat1(i),i=1,NDIV)
            write(25,191) irecs, is,cstaid(is),(dat1(i),i=1,NDIV)
          end do
 140    continue
 150  continue
c
c	Step x Process Reservoirs
c _________________________________________________________
 
      DO 152 IY=IYS,IYE
c
c rrb 10/27/94 Additional Output
        write(6,170) iy
        call year(iy, iyrmo, imomo, cyr1)

        call flush(6)
C
        DO 142 IM=1,12     
C
          IF(NUMRES.EQ.0.OR.NRSACT.EQ.0) GO TO 500
C
          WRITE(26,200) cunitm,nres, IYrmo(im),xmonam(im)
          write(26,192)(ix,ix=1,nres)
          
C        
          nrsactx = nrsact+numown                     
c         write(99,*) ' nrsact, numres, nrsactx'
c         write(99,*)   nrsact, numres, nrsactx
          ir1 = 0

          DO 130 IR=1,numres
            IF(IRESSW(IR).EQ.0) GO TO 130
            IS=IRSSTA(IR)   
             
            nrb = nowner(ir)
            nre = nowner(ir+1)      
            do n=nrb,nre
              ir1 = ir1+1
              IRECR=((IY-IYSTR0)*12+(IM-1))*NRSACTx+ir1+numtop
              READ (44,REC=IRECR,err=210) (DAT2(I),I=1,nres)
c
c rrb 2006/01/20; Do not adjust rid and acc
              DO I=1,nresX
                DAT2(I)=DAT2(I)*fmo(im)
              end do
c
              write(26,191) irecr, ir1,cstaid(is),(dat2(i),i=1,NRES)
            end do
  130     CONTINUE
C
  142   CONTINUE
C
  152 CONTINUE
c
c _________________________________________________________
c
c       Formats
  160 FORMAT(8(/),49X,'***  Diversion & Reservoir Output ',a5,' ***')
  170 format('+', ' Processing Year = ', i5)
c 180 format('',/, 
  180 format(    /, 
     1       '  Stream and Diversion data as follows ', a5,':',
     1       ' irecs, IS, CSTAID(IS), DAT1(I), I=1,ndivO)',//,
     1       '  Year ', i5, ' Month ', a3)
  190 FORMAT(2I5,3x,a12,10F10.0,/,20X,10F10.0,/,
     1       20X,10F10.0,/,20x,10f10.0)
  191 FORMAT(2I5,3x,a12,100F10.0)
  192 format(25x, 100i10)
c 200 FORMAT('',/,
  200 FORMAT(   /,
     1   '  Reservoir data (Total and by Account) as follows ', a5,':',
     1   ' irecr,  IR, CSTAID(IS), DAT2(I), I=1,',i2,//,
     1   '  Year ' i5, ' Month ', a3)
     
     
  500 RETURN
  210 write(6,*)  '   Outsys; Requested data exceeds binary file size'
      write(99,*) '   Outsys; Requested data exceeds binary file size'
      write(6,*) 'Stop 1' 
      call flush(6)
      call exit(1)

c
c _________________________________________________________
      stop 
      END
