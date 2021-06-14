c accdiv - distributes a diversion right to individual accounts
c_________________________________________________________________NoticeStart_
c StateMod Water Allocation Model
c StateMod is a part of Colorado's Decision Support Systems (CDSS)
c Copyright (C) 1994-2021 Colorado Department of Natural Resources
c 
c StateMod is free software:  you can redistribute it and/or modify
c     it under the terms of the GNU General Public License as published by
c     the Free Software Foundation, either version 3 of the License, or
c     (at your option) any later version.
c 
c StateMod is distributed in the hope that it will be useful,
c     but WITHOUT ANY WARRANTY; without even the implied warranty of
c     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c     GNU General Public License for more details.
c 
c     You should have received a copy of the GNU General Public License
c     along with StateMod.  If not, see <https://www.gnu.org/licenses/>.
c_________________________________________________________________NoticeEnd___
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
c               Initialize
        iout=0
c _________________________________________________________
c
c               Distribute based on ownership fraction
c               Note datinp changes % to a fraction
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
c
c rrb 2021/04/18; Compiler not used or initialize
cx 9999 write(6,*)  '  Stopped in AccDiv, see the log file (*.log)'
cx      write(99,*) '  Stopped in AccDiv'
cx      write(6,*) 'Stop 1'
cx      call flush(6)
cx      call exit(1)
      stop 
      end



