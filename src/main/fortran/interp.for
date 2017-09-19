C
      SUBROUTINE INTERP(X,Y,XX,YY,N,icall,nr,csta)
c
c
c _________________________________________________________
c	Program Description
c
c       Interp; It  interpolates to find YY given xx from tables
c               X and Y
c               icall = 1 called by evasec
c                       2 called by sepsec
c                       3 called by rgrg
c _________________________________________________________
c	Dimensions
c
      DIMENSION X(n),Y(n)
      character csta*12
c
c _________________________________________________________
c

      iout=0
c
c rrb 00/11/11; Concern when data provided is below the minimum
      if(iout.eq.1) then
        write(99,*) ' '
        write(99,*) '  Interp; check icall = ', icall
        write(99,*) '          where 1 = Evasec, 2 = Sepsec & 3 = RgRg'
        do i=1,n
          write(99,142) i, x(i), y(i)
        end do
      endif
      
      small=0.001
      small2=-1*small
c
c rrb 02/01/14; Eliminate small negatives
      if(xx.lt.small) xx=0.0

      n1=1

c     if(xx.lt.x(n1)) then
      if(xx-x(n1).lt.small2) then
        write(6,130) xx, x(n1)
        write(99,130) xx, x(n1)
        goto 9999
      endif               

      DO 100 I=2,N-1
      IF(XX.LE.X(I)) GO TO 110
  100 CONTINUE

      I=N  
c
c               Check for minor round off versus bad data
      if(xx.gt.x(n)) then                
        if(xx-x(n).lt.0.1) then
          xx = x(n)
          goto 110
        else
          n1=n
          write(6,130) xx, x(n1)
          write(99,130) xx, x(n1)
          goto 9999
        endif
      endif

 110  continue
c
c rrb 01/03/26; Check for potential division by zero
      if((x(i)-x(i-1)).le.small) then
        write(99,140)  nr, csta, xx 
        do i=1,n
          write(99,142) i, x(i), y(i)
        end do
        goto 9999
      endif               

      DX=X(I)-X(I-1)
      DY=Y(I)-Y(I-1)
      DXX=XX-X(I-1)
      YY=Y(I-1)+DXX*DY/DX

c     if(xx.lt.small) write(99,*) ' dx, dy, dxx, yy', dx, dy, dxx,yy

      RETURN                 

c
c_____________________________________________________________
c
c               Error warnings
c
 130  format(
     1 ' Interp; Problem capacity is not bounded by area-cap data',/
     1 '         value = ', f15.4, ' table value = ', 2f15.4)
 140  format(
     1 ' Interp; Problem for counter ', i5, ' ID = ', a12/
     1 '         division by zero because the area capacity',/
     1 '         curve does not increase when interpolating for ',/  
     1 '         value (xx) = ', f10.3,/
     1 '         To Do; Revise *.res'//        
     1 '                                    Capacity(y)',/
     1 '                #        Area(x)  or Seepage(y)',/
     1 '         ________ ______________ ______________')
 142  format
     1 ((9x, i8, 2f15.3))

c
 9999 write(6,200) 
      write(99,210) nr, csta, icall
      
 200  format('    Stopped in Interp',/,
     1       '    See the *.log file')
 210  format(
     1  '  Stopped in Interp for counter = ', i5, ' ID = ', a12,/
     1  '  icall = ', i5, ' where 1 = Evasec, 2 = Sepsec & 3 = RgRg')
      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)

c
c _________________________________________________________
c

      stop 
      END

