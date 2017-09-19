c
c *********************************************************
C
      SUBROUTINE RIGSOR(maxnwrx)
C
c
c _________________________________________________________
c	Program Description
c
c       Rigsor; It sorts water rights by priority
c
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
      real*8 rwr
      dimension rwr(maxnwrx)
      DIMENSION IWR(maxnwrx),KWR(maxnwrx),LWR(maxnwrx)
      INTEGER*4 I4
c
c _________________________________________________________
c

      I4(I)=I

c     ldem=5285
c                           
c rrb 06/18/96; Check for local dimensions
c
c rrb 99/08/27; Note maxsta*3 is used to allow data 
c               from the equivalence statement to wrap into
c               first 3 arrays of dummy that is dimensioned
c               based on maxsta size.
c               Note ntorig is total rights from riginp.f
c     if(maxnwr.gt.ldem .or. maxnwr.gt.maxsta*3) then
c     if(maxnwr.gt.ldem .or. ntorig.gt.maxsta*3) then
c       write(99,*) '  Rigsor; Local dimension exceeded'
c       write(99,*) '          local, maxnwr, maxsta*3 = ', 
c    1                         ldem,  maxnwr, maxsta*3
c       write(99,*) '  Revise common.for, statem.f & somnmy'
c       goto 330
c     endif
c                           
c rrb 06/18/96; Check for local dimensions
c     if(ntorig.gt.ldem) then
c       write(99,*) '  Rigsor; Local dimension exceeded'
c       write(99,*) '          local, ntorig = ', ldem, ntorig
c       write(99,*) '  Revise common.for, statem.f & somnmy'
c       goto 330
c     endif
C
C-------------------------------------------------------------------
C
C------  FIND THE PRIORITY ORDER OF ALL WATER RIGHTS
C
C-----------------------------------------------------------------------
C
      I=NUMFRR
      J=NUMRSR+I
      K=NUMDVR+J
      L=NUMPWR+K
c
c rrb 98/11/18; Wells
      m=numopr+L

      if(numfrr.gt.0) then
        DO IW=1,NUMFRR
          KWR(IW  )=1
          LWR(IW  )=IW
          Rwr(IW  )=rFRNK(IW)
        end do
      end if
C
      IF(NUMRSR.gt.0) then
        DO IW=1,NUMRSR
          KWR(IW+I)=2
          LWR(IW+I)=IW
          Rwr(IW+I)=rRSNK(IW)
        end do
      end if
C
      IF(NUMDVR.gt.0) then
        DO IW=1,NUMDVR
          KWR(IW+J)=3
          LWR(IW+J)=IW
          Rwr(IW+J)=rDVNK(IW)
        end do
      end if
C
      IF(NUMPWR.gt.0) then
        DO IW=1,NUMPWR
          KWR(IW+K)=4
          LWR(IW+K)=IW
          Rwr(IW+K)=rPWNK(IW)
        end do
      endif
C
      IF(NUMOPR.gt.0) then
        DO IW=1,NUMOPR
          KWR(IW+L)=5
          LWR(IW+L)=IW
          Rwr(IW+L)=rOPNK(IW)
        end do
      endif
C
c rrb 98/11/18; Wells
      if(numdvrw.gt.0) then
        DO IW=1,numdvrw
          KWR(IW+M)=6
          LWR(IW+M)=IW
          Rwr(IW+M)=rdvNKw(IW)
        end do
      end if
C
c     write(6,*) 'ENTER INTO SORTING ROUTINE'
C
      CALL SOMNMY(Rwr,IWR,NTORIG,maxnwr)
C
c     write(6,*) 'OUT FROM SORTING ROUTINE'
C
  310 DO 320 IW=1,NTORIG
      M=IWR(IW)
      NWRORD(1,IW)=KWR(M)
      NWRORD(2,IW)=LWR(M)
  320 CONTINUE
c
c _________________________________________________________
c
c
      RETURN
c
  330 write(6,*) '  Stopped in Rigsor, see the log file (*.log)'
      write(99,*) '  Stopped in Rigsor'
      write(6,*) 'Stop 1'       
      call flush(6)
      call exit(1)

c
c _________________________________________________________
c
      stop 
      END




