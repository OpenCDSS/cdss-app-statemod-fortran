c chekav2 - similar to Chekava, checks the entire Avail array by finding the
c           minimum value and warning if < 0.
c           But instead of stopping, it returns the most negative value. 
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
c rrb 2018/07/15; Revise number of operating rules
cx      SUBROUTINE chekav2(
cx     1 icall, maxsta, numsta, istop, fac, AVAIL, IMCD, AvMin)
      SUBROUTINE chekav2(
     1 icall, maxsta, numsta, istop, fac, AVAIL, IMCD, AvMin, subtypX)
c
c
c _________________________________________________________
c	Program Description
c
c       Chekav2; Similar to Chekava.
c                It checks the entire Avail array by finding the
c                minimum value and warning if < 0.
c		             But instead of stopping, it returns the most 
c                negative value. 
c
c _________________________________________________________
c	Update History
c
c rrb 2021/04/18; Compiler warning
c
c	Called By:
c   Divalt, Reodep, RivRtn
c
c _________________________________________________________
c
c	Documentation
c		icall	calling routine
c		maxsta	dimension for maximum number of stations
c
c	       istop   0 DO NOT STOP if a negative is found
c		             1 DO STOP if a negative is found
c        numsta  number of downstream stations
c        maxoprin maximum number of operating rules (see Statem.f)
c		     avail   array to check for min value
c		     imcd    pointer to min value
c        Avmin   minimum value in array
c
c _________________________________________________________
c
c		Dimensions
      DIMENSION AVAIL(maxsta)
c
c rrb 2018/07/15; Revise to make oerating rules type a scalar
cx    dimension subtyp2(55)
      character subtypX*8
c _________________________________________________________
c
c
c rrb 2021/04/18; Compiler warning
      icall=icall

c               Step 1; Check entire array avail
c
      nlog=99
      smalln = -0.001     
      AvMin = 99999.
      IMCD=0
      
      do ix=1,numsta
        if (avail(ix).lt.AvMin) then
          AvMin=avail(ix)
          imcd=ix
        endif  
      end do    
c
c	
      if(istop.eq.1 .and. AvMin.lt.smalln) goto 520
c
c _________________________________________________________
c               Return
c
      return
c
c _________________________________________________________
c               Error Processing
c
cx510 write(6,1050)
      write(6,1050)
      write(nlog,*) 
     1 '  CheckAv2; Problem max  # of operating rules exceeded'
      goto 550
      
 520  write(6,1050) 
      write(nlog,1052) subtypX, ix, AvMin, AvMin*fac
      write(nlog,1051)

 550  write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)
c _________________________________________________________
c               Stop
      stop
c
c _________________________________________________________
c               Formats
c
      
 1050 format('    Stopped in ChekAv2',/,
     1       '    See the *.log file')
 1051 format('    Stopped in ChekAv2')
 1052 format(/,'    ChekAva2; ', 
     1    'Checking Avail going into or out of routine ', a8,/
     1 13x, 'Negative avail at node ', i5, '. Avail = ', f10.3,' cfs',
     1 f10.3, ' af')
c _________________________________________________________
c               End
c
      end
