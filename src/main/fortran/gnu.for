c gnu - prints the conditions and warrant of use per the GNU open licensing agreement 
c       This has been replaced with license.for.
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

       Subroutine Gnu(ioptio, nlog)
c
c
c _________________________________________________________
c	Program Description
c
c	Gnu; It prints the conditions and warrant of use
c	per the GNU open licensing agreenment 
c       Copyright (C) 1989, 1991 Free Software Foundation, Inc.
c       59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
c
c
c _________________________________________________________
c	Dimensions
c

        character rec72*72
c
c _________________________________________________________
c		Step 1; Print Warrenty
c
	if(ioptio.eq.10) then
          open(1, file='Gnu_Warr.txt', status='old')
 100	  read(1,'(a72)',end=200,err=200) rec72
	  write(6,'(a72)') rec72
          write(nlog,'(a72)') rec72
	  goto 100
	endif  
c
c               Print Conditions of Use
        if(ioptio.eq.11) then
          open(1, file='Gnu_Cond.txt', status='old')
 110      read(1,'(a72)',end=200,err=200) rec72
	  write(6,'(a72)') rec72
          write(nlog,'(a72)') rec72
          goto 110
	endif  
c
c               Print Contact
        if(ioptio.eq.12) then
          open(1, file='Gnu_Cont.txt', status='old')
 120      read(1,'(a72)',end=200,err=200) rec72
	  write(6,'(a72)') rec72
          write(nlog,'(a72)') rec72
          goto 120
	endif  
	
 200    close(1)
        return
c       call exit(1)

        stop
        end
