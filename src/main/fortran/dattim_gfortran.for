c dattim - makes a system call to get the date and time in the gfortran way
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

      subroutine dattim(idat, itim)
c
c
c _________________________________________________________
c       Description
c
c       Makes a system call to get the date and time.
c       This version uses the gfortran library function.
c
c _________________________________________________________
c       Update History
c
c       2021-03-24 - Steve Malers
c         Fix code - was not returning expected values.
c       2014-05-06 - Jim Brannon
c         Converted this routine into a gfortran version.
c         Use the values arg of the date_and_time function 
c         to fill out the arrays.
c _________________________________________________________
c
c      Documentation
c
c              idat = integer array of date values
c                     (1) = 4-digit year
c                     (2) = 2-digit month 1-12
c                     (3) = 2-digit day 1-31
c
c              itim = integer array of time values
c                     (1) = 2-digit hour (0-23)
c                     (2) = 2-digit minute (0-59)
c                     (3) = 2-digit second (0-59)
c                     (4) = 2-digit hundredths of second (0-99)
c
c _________________________________________________________
c     Dimensions
c
      !character rec8*8, rec11*11
      dimension idat(3), itim(4)        
      !character(8)  :: date
      !character(10) :: time
      !character(5)  :: zone
      integer,dimension(8) :: values
c
c _________________________________________________________
c     Initialize

      ! These are earlier attempts to call built-in gfortran function.
      ! Using keyword arguments.
      !call date_and_time(date,time,zone,values)
      !call date_and_time(DATE=date,ZONE=zone)
      !call date_and_time(TIME=time)

      ! The following will return date and time using default time zone of UTC.
      ! - time zone defaults to local time
      call date_and_time(VALUES=values)
      !print '(a,2x,a,2x,a)', date, time, zone
      !print '(8i5)', values

      idat(1) = values(1)
      idat(2) = values(2)
      idat(3) = values(3)

      ! values(4) is time differences from UTC in minutes, currently not used

      itim(1) = values(5)
      itim(2) = values(6)
      itim(3) = values(7)
      ! Convert number of milliseconds (0-999) to number of hundredths (0-99)
      itim(4) = values(8)/10

      return

      end
