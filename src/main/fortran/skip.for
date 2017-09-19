c                            
c ************************************************************
c
c
        subroutine skip(nf, n)
c
c       skip; It skips x lines of a file
c
c _________________________________________________________
c	Program Description
c		
c	Skip; it skips comment cards in a data file
c _________________________________________________________
c	Dimensions
c
        character rec1*1, recin*256, filena*256
c
c _________________________________________________________
c		Step 1; Check first record and store for use
        iin2=nf
        filena='N/A, inside skip.for'

        iout=0

        if(iout.eq.0) then
          do i=1,n
            read(nf,'(a1)',end=926,err=928) rec1
            rec1 = rec1
          end do
        else
c
c rrb 01/06/04; Add testing capability
          do i=1,n
            read(nf,'(a256)',end=926,err=928) recin
            write(99,930) recin
            rec1 = recin(1:1)
          end do
        endif
 999    return

c
c _________________________________________________________
c
c       Error Handling
c
  926 write(99,927) iin2, filena
  927 format(' Skip.f; End of file # ', i4, ' encountered',/,
     1       '   File name: ', a256)
      goto 999
c
  928 write(99,929) iin2, filena
  929 format(' Skip.f; Problem reading file # ', i4,/,
     1       '   File name: ', a256,/
     1       '   Problem record (next line):')

      backspace(iin2)
      read(iin2, '(a256)',end=926,err=926) recin
      write(99,'(a256)') recin
      goto 9999

 930  format(/, '  Skip; Skipping file:',/,a256)


 9999 write(6,9997)
      write(99,9998)
 9997 format('  Stopped in Skip, see the log file (*.log)')
 9998 format('  Stopped in Skip')
      write(6,*) 'Stop 1' 
      call flush(6)
      call exit(1)

      stop 

      end





