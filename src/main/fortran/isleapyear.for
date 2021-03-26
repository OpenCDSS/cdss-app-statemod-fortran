c isleapyear - indicate whether a year is a leap year
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

       FUNCTION isleapyear(year)

c _________________________________________________________
c       Description
c
c       Determine whether a year is a leap year.
c
c _________________________________________________________
c
c      Documentation
c
c      year - 4-digit year to evaluate
c
c      Return .FALSE. if not a leap year, .TRUE. if a leap year.
c
c _________________________________________________________

      implicit none
      logical :: isleapyear
      integer, intent(IN) :: year

      if (((MOD(year,4) == 0) .and. (MOD(year,100) .ne. 0)) .or.
     +  ((MOD(year,100) == 0) .and. (MOD(year,400) == 0)) ) then
         isleapyear = .TRUE.
      else
        isleapyear = .FALSE.
      end if

      END FUNCTION isleapyear
