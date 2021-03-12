c roundof - checks for insignificant small numbers
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
c
c
c       Update History
c
c rrb 2020/06/03; Add iout and warn if small values are reset to zero
c
c _________________________________________________________
c	Dimensions
c
       dimension avail(*), ccall(10)
       character ccall*8 
       data ccall/
     1   'Bomsec  ', 'Execut  ', 'Divres  ', 'Welrig  ', 'DivresP', 
     1   'Divrpl  ', 'DivrplP ', 'JMStore ', 'JMFlow  ', '        '/
c
c _________________________________________________________
c		Step 1; Initialize
c
c      small = 0.001
       small=0.5
       nbug=0
       nlog=99
       
       iout=0
       
       if(iout.eq.1) write(nlog,*)'  RoundOf; Called by ',ccall(icall) 
c
c _________________________________________________________
c		Step 2; Check for values < 0

       ax=99999.9
       iy=0
       do ix=1,numsta
         if(avail(ix).lt.ax) then
           iy=ix
           ax=avail(ix)
         endif
       end do
c
c _________________________________________________________
c		Step 3; If < 0, print warning

       if(ax.lt.abs(0.0)) then
         iy=iy
c
c rrb 2020/06/03; Warn if small values are reset to zero
         if(iout.eq.1) then
           write(99,100)  iy, ax, ccall(icall), itop, -1.*small
         endif

         do ix=1,numsta
           if(avail(ix).lt.abs(0.0)) then 
             if(avail(ix).gt.-1.*small) then
             
               if(iout.eq.1) then
                 write(99,110) ix, avail(ix), ccall(icall), itop  
               endif
               
               avail(ix) = abs(0.0)
             else
               write(99,120) ix, avail(ix), ccall(icall), itop  
               avail(ix) = abs(0.0)
               nbug=1
               goto 500
             endif
           endif
         end do 
c
c rrb 2020/06/07; Additional output
         if(nbug.eq.1) goto 500                              
       endif
       
       return 
c
c
c _________________________________________________________
c
c               Error warnings
c
  100  format(/,'   Roundof; Warning at node ',i5,
     1   ' avail (cfs) = ',f20.8,
     1   '  Called by ',a8, ' at Step ', i5,/
     1          '   If > ',f8.3,' it is set to zero') 
  
  110  format(/,'   Roundof; FYI, fixed node ',i5,
     1   ' avail (cfs) = ',f20.8,
     1   '  Called by ',a8, ' at Step ', i5)                   

  120  format(/,'   Roundof; Problem at node ',i5, 
     1   ' avail (cfs) = ',f20.8,
     1   '  Called by ',a8, ' at Step ', i5)                   
  
  210 format(/,'  Roundof; Local dimension exceeded, problem = ',i5)
c
c
c_______________________________________________________________
  500 write(6,300)
      write(99,310) ccall(icall)
  300 format('   Stopped in Roundoff',/,
     1       '   See the *.log file')
  310 format('   Stopped in Roundoff after a call from ', a8)
      write (6,*) 'Stop 1'
      call flush(6)
      stop
cr    call exit(1)
 
      END
