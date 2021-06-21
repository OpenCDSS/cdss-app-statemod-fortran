
c
c ************************************************************
c
c               skipn it skips any number of comment cards identified
c               as a '#' in column 1 of a file
c
        subroutine skipn(nf)
         character rec1*1
c                 
c              Check first record and store for use on this file
  100   read(nf,'(a1)',end=110,err=110) rec1
c
        if(rec1.eq.'#') then
          goto 100
        else
          backspace(nf)
        endif        

  110   return
        end
c
