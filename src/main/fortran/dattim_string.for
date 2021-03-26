c dattim_string - format an ISO 8601 string for date and time
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

       SUBROUTINE dattim_string(idate, itime, formatted_string)
c
c
c _________________________________________________________
c      Description
c
c      Format an ISO string YYYY-MM-DD hh:mm:ss.nnn 
c
c _________________________________________________________
c
c      Documentation
c
c              idate = integer array of date values
c                     (1) = 4-digit year
c                     (2) = 2-digit month 1-12
c                     (3) = 2-digit day 1-31
c
c              itime = integer array of time values
c                     (1) = 2-digit hour (0-23)
c                     (2) = 2-digit minute (0-59)
c                     (3) = 2-digit second (0-59)
c                     (4) = 2-digit hundredths of second (0-99)
c
c              Return number of ms since January 1, 2020, 00:00.
c
c _________________________________________________________

      implicit none
      integer, dimension(3), intent(IN) :: idate
      integer, dimension(4), intent(IN) :: itime
      character(len=*), intent(INOUT) :: formatted_string
      integer :: year, month, day, hour, minute, second, hsecond

      year = idate(1)
      month = idate(2)
      day = idate(3)
      hour = itime(1)
      minute = itime(2)
      second = itime(3)
      hsecond = itime(4)

      write(formatted_string,
     +  "(i0.4,'-',i0.2,'-',i0.2,' ',i0.2,':',i0.2,':',i0.2,'.',i0.3)")
     +  year, month, day, hour, minute, second, hsecond*10
      formatted_string = trim(formatted_string)

      END
