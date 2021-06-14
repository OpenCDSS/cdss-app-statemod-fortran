c chekava - checks array Avail by finding the minimum value and warning if < 0.
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
C
      SUBROUTINE chekava(icall, maxsta, numsta, AVAIL, subtypX)
c
c
c _________________________________________________________
c	Program Description
c
c       Chekava; it checks array Avail by finding the
c                minimum value and warning if < 0.
c
c
c _________________________________________________________
c	Update History
c		NA
c
c _________________________________________________________
c
c Called by: 
c   CARRPL,   DirectBy, DirectEx, DirectWR, DivImpR,  
c   DivimpR2, DivMulti, DIVRES,   DivResP,  DivResP2,
c   DivresR,  DIVRIG,   Divrpl,   divrplP,  divrplP2, 
c   divrplR,  flowres,  ifrrigSP, OopDiv,   PowResP, 
c   Splatte,  WelAugP,  WelPrity, FlowRes

c _________________________________________________________
c
c	Documentation
c		icall	calling routine
c		maxsta	dimension for maximum number of stations
c               numsta  number of downstream stations
c		avail   array to check for min value
c               Avmin   minimum value in array
c
c
c _________________________________________________________
c	Dimensions
c

      DIMENSION AVAIL(maxsta)
c
c rrb 2018/07/29; Replace subtyp2 with a scalar passed in
cxx   dimension subtyp2(60)
      character subtypX*8
c
c _________________________________________________________
c
c               Step 1; Check array avail
c
c rrb 2021/04/18; Compiler warning
      icall=icall
c
      iout=0

      nlog=99
      smalln = -0.001     
      ax = 99999.9
      fx=1.9835*31.
      
      do ix=1,numsta
        ax1=ax
        ix1=ix
        if(iout.eq.1) then
          write(nlog,*) '  Chekava; ', ix, avail(ix), avail(ix)*fx
        endif
        
        ax=amin1(ax, avail(ix))
        if(ax.lt.ax1) ix1=ix
        if(ax.lt.smalln) goto 9999
      end do
      
      if(iout.eq.1) then
        write(nlog,1054) subtypX, ix1, ax, ax*fx
      endif
c
c _________________________________________________________
c               Return
c
      return
c
c _________________________________________________________
c               Formats
c
      
 1050 format('    Stopped in ChekAva',/,
     1       '    See the *.log file')
 1051 format('    Stopped in ChekAva')
 1052 format(/,'    ChekAva; ', 
     1    'Checking Avail going into or out of routine ', a8,/
     1 13x, 'Negative avail at node ', i5, '. Avail = ', f10.3,' cfs',
     1 f10.3, ' af')
     
 1054 format(/,'    ChekAva; ', 
     1    'Checking Avail going into or out of routine ', a8,/
     1 13x, 'No negative avail. Minimum at node ', i5, 
     1 '. Avail = ', f10.3,' cfs', f10.3, ' af')
c
c _________________________________________________________
c               Error Processing
c
 9999 write(6,1050) 
      write(nlog,1052) subtypX, ix, ax, ax*fx
      write(nlog,1051)

      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)
      stop


      end

