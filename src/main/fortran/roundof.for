c roundof - checks for insignificant small numbers
c_________________________________________________________________NoticeStart_
c StateMod Water Allocation Model
c StateMod is a part of Colorado's Decision Support Systems (CDSS)
c Copyright (C) 1994-2018 Colorado Department of Natural Resources
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

       subroutine roundof(avail, numsta, icall, itop, nbug)
c
c
c _________________________________________________________
c	Program Description
c
c       Rounof; it checks for insignificant small numbers
c       
c               icall=  subroutine call
c
c _________________________________________________________
c	Dimensions
c
       dimension avail(*), ccall(10)
       character ccall*8 
       data ccall/
     1   'Bomsec  ', 'Execut  ', 'Divres  ', 'Welrig  ', 'DivresP', 
     1   'Divrpl  ', 'DivrplP ', '        ', '        ', '        '/
c
c _________________________________________________________
c		Step 1; Initilize
c
       small=0.5
       nbug=0

       ax=99999.9
       iy=0
       do ix=1,numsta
         if(avail(ix).lt.ax) then
           iy=ix
           ax=avail(ix)
         endif
       end do

       if(ax.lt.abs(0.0)) then
         iy=iy
c        write(99,100)  iy, ax, ccall(icall), itop

         do ix=1,numsta
           if(avail(ix).lt.abs(0.0)) then 
             if(avail(ix).gt.-1.*small) then
c              write(99,110) ix, avail(ix), ccall(icall), itop  
               avail(ix) = abs(0.0)
             else
               write(99,120) ix, avail(ix), ccall(icall), itop  
               avail(ix) = abs(0.0)
               nbug=1
               goto 500
             endif
           endif
         end do                                
       endif
       return 
c
c
c _________________________________________________________
c
c               Error warnings
c
  100  format(/,'   Roundof; Warning at node ',i5,' avail = ',f20.8,
     1   '.  Called by ',a8, ' at location ', i5) 
  
  110  format(/,'   Roundof; FYI, fixed node ',i5,' avail = ',f20.8,
     1   '.  Called by ',a8, ' at location ', i5)                   

  120  format(/,'   Roundof; Problem at node ',i5,' avail = ',f20.8,
     1   '.  Called by ',a8, ' at location ', i5)                   
  
  210 format(/,'  Roundof; Local dimension exceeded, problem = ',i5)

 500  write(6,300)
      write(99,310) ccall(icall)
  300 format('   Stopped in Roundoff',/,
     1       '   See the *.log file')
  310 format('   Stopped in Roundoff after a call from ', a8)
      write (6,*) 'Stop 1'
      call flush(6)
      stop
cr    call exit(1)
 
      END
