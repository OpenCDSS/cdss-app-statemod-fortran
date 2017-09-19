
        subroutine backfil(recin, recout)
c
c _________________________________________________________
c	Program Description
c               It backfills blanks to the unused portion of a
c               character string
c
c               recin character string in
c               recout character string out
c
c
c _________________________________________________________
c	Update History
c		NA
c
c _________________________________________________________
c	Documentation
c
c
c _________________________________________________________
c	Dimensions
cc
        character recin*12, recout*12

        i1=0
        j=0

        recout=' '
        do i=1,12
          i1=12-i+1
c         write(99,110) i1, recin   

          if(recin(i1:i1).ne.' ') then
c           write(99,110) 0, recin

            do j=1,i1
              j1=12-j+1
              recout(j1:j1) = recin(i1:i1)
              i1=i1-1
            end do
c           write(99,110) 1, recin
c           write(99,110) 2, recout
            goto 100 
          endif
        end do 

 100    return
 110    format('  Backfil;', i5, a12)
        end
