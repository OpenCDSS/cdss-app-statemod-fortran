c chekav2 - similar to Chekava, checks the entire Avail array by finding the
c           minimum value and warning if < 0.
c           But instead of stopping, it returns the most negative value. 
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

      SUBROUTINE chekav2(
     1 icall, maxsta, numsta, istop, fac, AVAIL, IMCD, AvMin)
c
c
c _________________________________________________________
c	Program Description
c
c       Chekav2; Similar to Chekava.
c                It checks the entire Avail array by finding the
c                minimum value and warning if < 0.
c		 But instead of stopping, it returns the most 
c                negative value. 
c
c _________________________________________________________
c	Update History
c		NA
c
c _________________________________________________________
c
c	Documentaion
c		icall	calling routine
c		maxsta	dimension for maximum number of stations
c
c	       istop   0 DO NOT STOP if a negative is found
c		        1 DO STOP if a negative is found
c             numsta  number of downstream stations
c		avail   array to check for min value
c		imcd    pointer to min value
c             Avmin   minimum value in array
c
c _________________________________________________________
c
c		Dimensions

      DIMENSION AVAIL(maxsta)
      dimension subtyp2(30)
      character subtyp2*8
      data subtyp2/
     1 'Powres',    'Divres',   'Divres',  'Divrpl', 'Resrpl',
     1 'Rsrspu',    'Carrpl',   'Resoop',  'Powsea', 'Replace',
     1 'Divcar',    'Reoper',   'Ifrrigx', 'Divcar1','Sjrip',     
     1  'Evasec',   'DivResP2', 'DivRplP', 'Welrig', 'DirectEx',
     1  'DirectBy', 'PowResP',  'OopDiv',  'Divrig', 'DivrplP2',
     1  'DivCarL',  'RivRtn',   'DivAlt',  ' ',      ' '/     
c
c _________________________________________________________
c
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
      if(istop.eq.1 .and. AvMin.lt.smalln) goto 9999
c
c _________________________________________________________
c               Return
c
      return
c
c _________________________________________________________
c               Error Processing
c
 9999 write(6,1050) 
      write(nlog,1052) subtyp2(icall), ix, AvMin, AvMin*fac
      write(nlog,1051)

      write (6,*) 'Stop 1'
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

