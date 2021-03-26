c dattim - makes a system call to get the date and time, Lahey version
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
cc
       subroutine dattim(idat, itim)
c
c
c _________________________________________________________
c       Description
c
c       Makes a system call to get the date and time.
c
c _________________________________________________________
c       Update History
c rrb 2020/07/28; Add iout and detailed output
c               
c _________________________________________________________
c
c               Documentation
c
c _________________________________________________________
c     Dimensions
c
      character rec8*8, rec11*11
      dimension idat(3), itim(4)        

c _________________________________________________________
c     Initialize

      iout=0
      io99=99

      call date(rec8)
      read(rec8(1:2),'(i2)',end=928,err=928) idat(3)
      read(rec8(4:5),'(i2)',end=928,err=928) idat(2)
      read(rec8(7:8),'(i2)',end=928,err=928) idat(1)

      call time(rec11)
      read(rec11(1:2),'(i2)',end=928,err=928) itim(1)
      read(rec11(4:5),'(i2)',end=928,err=928) itim(2)
      read(rec11(7:8),'(i2)',end=928,err=928) itim(3)
      read(rec11(10:11),'(i2)',end=928,err=928) itim(4)
  
      if(iout.eq.1) then
        write(io99,*) ' Dattim; rec8 = ', rec8
        write(io99,*) ' Dattim; itim = ',(itim(j), j=1,4)
      endif

      return
c
c _________________________________________________________
c
c rrb 97/11/02; Error Handling
c
c
  928 write(io99,929)
  929 format(' Dattim.f; Problem with internal read of date or time.')
      goto 9999
 9999 write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop
      end
