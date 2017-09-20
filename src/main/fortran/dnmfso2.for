C
C-------------------------------------------------------------------
C
      SUBROUTINE DnmFso2(maxsta, AVAIL ,IDNCOD,ISCD,NDNS,IMCD,
     1  cCallBy)
c
c       DnmFso2; It finds the minimum flow at station iscd
c               and downstream.
c		    Same as DnmFso but it includes cCallBy
c
      DIMENSION AVAIL(maxsta),IDNCOD(maxsta)
      character cCallBy*12

C
C-------------------------------------------------------------------
C
C------  FIND THE DOWNSTEAM MINMUM FLOW NODE OF STATION ISCD
C
C-------------------------------------------------------------------
C
      iout=0
      if(iout.eq.1) write(99,*) '  DnmFso2; In, maxsta, iscd, ndns ', 
     1  maxsta, iscd, ndns
     
      IMCD=ISCD
      ISS=ISCD
      
      DO ND=1,NDNS
        if(iss.eq.0 .or. imcd.eq.0) goto 9999
        IF(AVAIL(IMCD).GT.AVAIL(ISS)) IMCD=ISS
        ISS=IDNCOD(ISS)
      end do
      if(iout.eq.1) write(99,*) '  DnmFso2; Out imcd ', imcd
C
      RETURN
      
      
c
c_____________________________________________________________
c               Error warnings
c
 9999 continue
      write(99,300) cCallBy, nd, iscd, ndns, imcd, iss
  300 FORMAT(/, 72('_'),/
     1  '  DnmFso2; Problem undefined data when called by ',a12/
     1  '             nd iscd ndns imcd  iss' /,
     1  '         ', 20i5)
 
      write(6,310) 
      write(99,320) 
      
 310  format('    Stopped in DnmFso2',/,
     1       '    See the *.log file')
 320  format('    Stopped in DnmFso2')
      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)


      stop 
      END      
