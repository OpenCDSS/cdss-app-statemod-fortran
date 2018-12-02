c year - sets data associated with a year type
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

        subroutine year(iyr, iyrmo, imomo, cyr1)
c
c
c _________________________________________________________
c	Program Description
c
c       Year; It sets data associated with a year type
c _________________________________________________________
c	Dimensions
c
        dimension iyrmo(13), imomo(13)
        character cyr1*5
c
c _________________________________________________________
c
c               Calendar Year (Jan - Dec
        do j=1,13
          iyrmo(j) = iyr
          imomo(j) = j
        end do
c
c _________________________________________________________
c
c               Water Year (Oct - Sept)

        if(cyr1 .eq. '  WYR') then
          do j=1,3
            iyrmo(j) = iyr-1
            imomo(j) = j+9
          end do

          do j=4,12
            imomo(j)=j-3
          end do
        endif
c
c _________________________________________________________
c
c               Irrigation Year (Nov - Dec)

        if(cyr1 .eq. '  IYR') then
          do j=1,2
            iyrmo(j) = iyr-1
            imomo(j) = j+10
          end do

          do j=3,12
            imomo(j) = j-2
          end do
        endif
c
c _________________________________________________________
c

        return
        end



