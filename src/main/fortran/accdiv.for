cc
c *********************************************
c
        subroutine AccDiv(nlog, maxownd, nd, divactx, ndown, divownP,
     1    divownQ, f, iopout, iscd, iyrmo1, xmonam1, divnamo)
c _________________________________________________________
c
c               Program Description;
c
c       It distributes a diversion right to individual accounts
c
c _________________________________________________________
c
c               Update History
c       2004/09/08; Created
c
c
c _________________________________________________________
c               Documentation
c
c _________________________________________________________
c               Dimensions
c
       dimension ndown(maxownD), divownP(maxownD), divnamo(maxownD),
     1           divownQ(maxownD)  
       character divnamo*24, xmonam1*4
c
c _________________________________________________________
c
c               Initilize
        iout=0
c _________________________________________________________
c
c               Distribute based on ownership fraction
c		Note datinp changes % to a fraction
c
        noi=ndown(nd)
        noe=ndown(nd+1)-1

        do no=noi,noe
          divownQ(no) = divownQ(no) + divactX*divownP(no)
        end do
c
c _________________________________________________________
c
c               Detailed Printout
      if(iout.eq.1 .or. -iopout.eq.iscd) then
        totp=0.0
        totq=0.0
c
        write(nlog,200)
        write(nlog,202)
        do no=noi,noe
          write(nlog,210) iyrmo1, xmonam1,nd, no, divnamo(no), 
     1     divactx*f, divownP(no)*100., divownQ(no)*f, divownQ(no)
          totp=totp+divownP(no)
          totq=totq+divownQ(no)
        end do
        
        write(nlog,202)
        write(nlog,210) iyrmo1, xmonam1, nd, -1, 
     1    'Total                   ',
     1    divactx*f, totp*100, totq*f, totq
      endif     
c
c _________________________________________________________
c
c               Return
      return
c
c _________________________________________________________
c
c               Formats
 200  format(
     1 /,'  AccDiv; ',
     1 ' Detailed allocation of a diversion to owners',/
     1 10x,' Note DivOwnQ-a is in af while DivOwnQ-c is in cfs ',/
     1 10x,   '   yr   mo   nd   no DivnamO                 ',
     1 '   DivActX   DivOwnP DivOwnQ-a DivOwnQ-c')
 202  format(
     1 10x,   ' ____ ____ ____ ____',
     1 1x, 24('_'), ' _________ _________ _________ _________')
 210  format(10x, i5,1x,a4, 2i5, 1x, a24, 20f10.1)
c
c
c _________________________________________________________
c
c               Error Processing

 9999 write(6,*)  '  Stopped in AccDiv, see the log file (*.log)'
      write(99,*) '  Stopped in AccDiv'
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)
      stop 
      end



