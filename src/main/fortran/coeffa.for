c coeffa - calculates the fraction of land that can be served by wells
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

      Subroutine coeffa(areax, areat, small, ca, 
     1           izero, iout, io99, cdividx)
c
c _________________________________________________________
c	Program Description
c
c
c       Coeffa; It calculates the fraction of land that can
c               be served by wells
c               areax   = area served by wells in a given year
c               areat   = total area under a ditch or served by wells
c               ca      = fraction served by wells
c               io99    = output file
c               cdividx = ID for debug output
c               izero   = division by zero treatment
c
c
c _________________________________________________________
c	Update History
c		NA
c
c _________________________________________________________
c	Documentation
c
c
c _________________________________________________________
c	Dimensions
         character cdividx*12
c
c _________________________________________________________
c               Step 1; Initilize
c       iout=1
        iout=0
c
c _________________________________________________________
c               Step 2; Check for division by zero
        if(izero.eq.0) then
          ca = 0.0
        else
          ca = 1.0
        endif
c
c _________________________________________________________
c               Step 3; Print debug
        if(areat.le.small) then
          if(iout.eq.1) write(io99,100) cdividx, areat, ca
        else
c
c _________________________________________________________
c               Step 4; Calculate fraction served by wells
          ca = amin1(areax/areat, 1.0)
        endif
c
c _________________________________________________________
c               Step 5; Return
 500    return
c
c _________________________________________________________
c               Error warnings
c 
c9999 write(6,200) 
c     write(99,210) 
 100  format( 
     1 '  Coeffa (Bomsec); Warning *.tsp file has diversion ID ', a12, 
     1         ' with area = ', f8.2, ' Set ca = ', f8.2)
    
 200  format('    Stopped in Coeffa (Bomsec)',/,
     1       '    See the *.log file')
 210  format('    Stopped in Coeffa (Bomsec)')
c     write (6,*) 'Stop 1'
c     call flush(6)
c     call exit(1)
c     stop 
      END

