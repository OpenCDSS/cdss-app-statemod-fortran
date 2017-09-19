c
c
c _________________________________________________________
c	Dimensions
cc
       subroutine dattim(idat, itim, isgi)
c
c
c _________________________________________________________
c	Program Description
c
c       	Dattim; It makes a system call to get 
c		the date and time
c
c _________________________________________________________
c	Update History
c		NAc
c               
c _________________________________________________________
c
c               Documentation
c
c              isgi= Switch for date and time call
c                    0 = PC
c                    1 = SGI workstation
c              To switch operation 
c                    1) Change isgi value
c                    2) Change commented values 
c
c _________________________________________________________
c		Dimensions
c
       character rec8*8, rec11*11
       dimension idat(3), itim(4)        
c
c _________________________________________________________
c		Initilzie
       io99=99
       if(isgi.eq.0) then
c
        call date(rec8)
        read(rec8(1:2),'(i2)',end=928,err=928) idat(3)
        read(rec8(4:5),'(i2)',end=928,err=928) idat(2)
        read(rec8(7:8),'(i2)',end=928,err=928) idat(1)

        call time(rec11)
        read(rec11(1:2),'(i2)',end=928,err=928) itim(1)
        read(rec11(4:5),'(i2)',end=928,err=928) itim(2)
        read(rec11(7:8),'(i2)',end=928,err=928) itim(3)
        read(rec11(10:11),'(i2)',end=928,err=928) itim(4)
      
      else 

c       call idate(idat(3),idat(2),idat(1))
c       call time(rec8)
        read(rec8(1:2),'(i2)',end=928,err=928) itim(1)
        read(rec8(4:5),'(i2)',end=928,err=928) itim(2)
        read(rec8(7:8),'(i2)',end=928,err=928) itim(3)
      endif
  
      return
c
c _________________________________________________________
c
c rrb 97/11/02; Error Handling
c
c
  928 write(io99,929)
  929 format(' Dattim.f; Problem with an internal read of date or time')
      goto 9999
 9999 write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop
      end
