c adjcase - changes characters to/from upper/lower case
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
      subroutine AdjCase(nlog, recin, recout, nin, ntype)
c
c _________________________________________________________
c       Program Description
c       It changes characters from upper case to lower
c       case or vice versa
c
c
c _________________________________________________________
ci      Update History
c
c       NA
c
c _________________________________________________________
c       Documentation
c
c
c       nlog   = log file number
c       recin  = input record
c       recout = output record
c       nin    = record size
c       ntype  = 1 caps to lower case
c                2 lower case to capitol
c
c
c _________________________________________________________
c     Dimensions
c
      dimension cap(26), lc(26)
      character cap*1, lc*1
      character recin*132, recout*132
      
      data cap/'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K',
     1         'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 
     1         'W', 'X', 'Y', 'Z'/
      data lc /'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 
     1         'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
     1         'w', 'x', 'y', 'z'/
      
      iout=0
      recout=recin
c
c     Caps to lower case
      if(ntype.eq.1) then
        do i=1,nin
          do j=1,26     
            if(recout(i:i) .eq. cap(j)) recout(i:i) = lc(j)
          end do          
        end do
      else
        do i=1,nin
          do j=1,26     
            if(recout(i:i) .eq. lc(j)) recout(i:i) = cap(j)
          end do          
        end do        
      endif  
      
      if(iout.eq.1) then
        write(nlog,100) recin, recout
 100    format(' AdjCase;   recin = ', a72,/
     1         '           recout = ', a72)
      endif
      return
      end     
