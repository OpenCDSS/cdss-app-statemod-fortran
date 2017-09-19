C
C *********************************************************
C
      SUBROUTINE TAKOUT(maxsta, AVAIL ,RIVER ,AVINP ,QCHECK,IDNCOD,
     1                  DIVACT, NDNS  ,ISCD)
c
c
c _________________________________________________________
c	Program Description
c
c       Takout; It subtracts divact from 
c               Avail and River at diverting node (iscd) and from
c               Avail, River and Avinp at downstream nodes (iss)
c
c
c _________________________________________________________
c       Documentatoin
c
c               avail is water available for diversion
c               river is wet water at this node
c               avinp is wet water upstream of this node
c
c
c _________________________________________________________
c	Dimensions
c
      DIMENSION AVAIL(maxsta), RIVER(maxsta), AVINP(maxsta),
     1          QCHECK(maxsta),IDNCOD(maxsta)

c
c _________________________________________________________
c		Step 1; Initilze
      nlog=99
      iout=0
      if(iout.eq.1) then
        write(nlog,*) ' '
        write(nlog,*) ' Takout; iscd, ndns', iscd, ndns
      endif
c
c  Adjust avail and river at the diversion itself        
      ISS=ISCD
      AVAIL(ISS)=AVAIL(ISS)-DIVACT
      RIVER(ISS)=RIVER(ISS)-DIVACT
c
c  Adjusst avail, river and avinp at every downstream node
      ISS=IDNCOD(ISS)
      IF(ISS.EQ.0.OR.NDNS.LE.1) goto 500
      
      DO ND=1,NDNS-1
        if(iout.eq.1) write(nlog,*) 
     1    ' Takout; nd, ndns-1, iss', nd, ndns-1, iss
c      
        if(iss.eq.0) then
          write(nlog,*) '  Takout; Warning iscd, ndns, iss = ',
     1       iscd, ndns, iss
          goto 500
        endif
      
        AVAIL(ISS)=AVAIL(ISS)-DIVACT
        RIVER(ISS)=RIVER(ISS)-DIVACT
        AVINP(ISS)=AVINP(ISS)-DIVACT
        
        ISS=IDNCOD(ISS)
      end do
C
C
 500  RETURN
c
c _________________________________________________________
c
c	 Error Tracking
 1000 write(6,1050) 
      write(nlog,1051) iss
      
 1050 format(
     1  '    Stopped in Takout',/,
     1  '    See the *.log file')
 1051 format(
     1  '    Stopped in Takout; iss, iscd = ', i5,/
     1  '      This can happen when the number of downstream nodes',/
     1  '      (ndns) is incorrect for this river node (iscd)')
      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)        
      END
