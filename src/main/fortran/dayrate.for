c dayrate - sets daily factor to limit code size
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

        subroutine DayRate(idayX, idly, facdly)
c
c _________________________________________________________
c	Program Description
c
c       DayRate; It sets daily factor to limit code size
c
c _________________________________________________________
c	Dimensions
        
        include 'common.inc'
c       include 'daily.inc'
c
c _________________________________________________________
c
               
        if(idayX.eq.0) then
          FACDLY=DLYRAT(1,IDLY)
        else
          facdly=dlyratd(1,idly)
        endif
        return 
        end
