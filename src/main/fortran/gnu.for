c
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
