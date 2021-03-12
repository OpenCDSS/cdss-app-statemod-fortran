c geteomx - get reservoir storage from previous year.
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

      SUBROUTINE GetEomX(nr, c, cid1)
c
c
c _________________________________________________________
c	Program Description
c
c		Called by Daydist to get reservoir storage
c		  from previous year. Note:
c		  For baseflows resvol1 is set in Virin 
c		  For simulate resvol1 is set in Bomsec
c
c _________________________________________________________
c	Dimensions
c

      include 'common.inc'      
      character cid1*12
c
c _________________________________________________________
c		Step 1; 
c
      
      iout=0
      c=resvol1(nr)
      if(iout.eq.1) write(nlog,*) ' GetEomX; ', cid1, iyr, mon, nr, c
      return
      end
     
