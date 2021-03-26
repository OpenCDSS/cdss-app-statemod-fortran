c dattim_ms - compute milliseconds from dattim output
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

       FUNCTION dattim_ms(idate, itime)
c
c
c _________________________________________________________
c       Description
c
c       Compute milliseconds since January 1, 2020, 00:00,
c       useful for performance calculations.
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
      ! Use a long integer for ms
      integer(kind=8) :: dattim_ms
      integer :: year, month, day, hour, minute, second, hsecond
      integer :: ndays
      integer :: ms_in_day = 86400000
      integer :: ms_in_hour = 3600000
      integer :: iyear
      ! Days in months previous to specific month:
      integer, dimension(12) :: days_in_month_prev =
     +  ( / 0,
     +    31,     ! + 31 Jan
     +    59,     ! + 28 Feb
     +    90,     ! + 31 Mar
     +    120,    ! + 30 Apr
     +    151,    ! + 31 May
     +    181,    ! + 30 Jun
     +    212,    ! + 31 Jul
     +    243,    ! + 31 Aug
     +    273,    ! + 30 Sep
     +    303,    ! + 31 Oct
     +    334 / ) ! + 30 Nov
      ! Declare the called function.
      logical :: isleapyear

      year = idate(1)
      month = idate(2)
      day = idate(3)

      hour = itime(1)
      minute = itime(2)
      second = itime(3)
      hsecond = itime(4)

      ! Initialize.
      dattim_ms = 0

      ! Loop through previous years and add ms for full years.
      do iyear = 2020, (year - 1)
        ndays = 365
        if ( isleapyear(iyear) ) then
          ndays = 366
        end if
        ! Add ms for previous years
        dattim_ms = dattim_ms + ndays*ms_in_day
      end do

      ! Add ms for previous months (can't count current month because it is incomplete)
      dattim_ms = dattim_ms + days_in_month_prev(month)*ms_in_day

      ! Add ms for previous days (can't count current day because it is incomplete)
      dattim_ms = dattim_ms + (day - 1)*ms_in_day

      ! Add ms for hours (can count because hours start with zero).
      dattim_ms = dattim_ms + hour*ms_in_hour

      ! Add seconds (can count because seconds start with zero).
      dattim_ms = dattim_ms + second*1000

      ! Add ms for hseconds (can count because hseconds start with zero).
      dattim_ms = dattim_ms + hsecond*10

      END FUNCTION dattim_ms
