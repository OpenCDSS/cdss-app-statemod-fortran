c
c *********************************************************
c
      SUBROUTINE SOMNMY(Rwr,L,N,maxnwr)
c
c
c _________________________________________________________
c	Program Description
c
c       Somnmy; It sorts water rights
c               Called by rigsor.f
c
c _________________________________________________________
c	Dimensions
c
      real*8 rwr
      DIMENSION Rwr(maxnwr), L(maxnwr)
c
c _________________________________________________________
c		Step 1; Initilize
      DO 10 I=1,N
   10 L(I)=I
      M=N
   20 M1=M-1
      IF(M1) 60,60,30
   30 DO 50 I=1,M1
      II=N-I+1
      J=L(II)
      K=L(II-1)

      if(rwr(j)-rwr(k) .lt. -0.000001) then
        l(ii)=k
        l(ii-1)=j
        m=i
      endif

   50 CONTINUE
      IF(M-M1) 20,20,60
   60 RETURN
c
c _________________________________________________________
c
 9999 write(6,*) '  Stopped in Somnmy, see the log file (*.log)'
      write(99,*) '  Stopped in Somnmy'
      write(6,*) 'Stop 1' 
      call flush(6)
      call exit(1)

      stop 
      END

