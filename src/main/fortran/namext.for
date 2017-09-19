c
c *********************************************************
c
c
        subroutine namext(maxfn, filenc, filext, fileno)
c
c
c _________________________________________________________
c	Program Description
c
c       Namext, it puts an extension (filext) on a name (filenc).
c               Output is fileno
c
c _________________________________________________________
c
c       Update History
c
c rrb 02/08/08; Allow the response file name to include a .xxx
c rrb 00/08/04; Added maxfn and revised file length from 72 to 256
c
c _________________________________________________________
c	Dimensions
        character filenc*256, filext*3, fileno*256
c
c _________________________________________________________
c		Step 1; Initilize
        iout=0
        nlog=99
        fileno = ' '
        if(iout.eq.1) write(nlog,*) ' Namext; In' 

        do 100 i=1,maxfn
c rrb 02/08/08: Allow the response file name to include a .xxx
c         if(filenc(i:i) .eq. ' ') then
          if(filenc(i:i) .eq. ' ' .or. filenc(i:i).eq.'.') then
            fileno(i:i) = '.'
            fileno(i+1:i+3) = filext
c           write(nlog,'(2x, a256)') filenc
c           write(nlog,'(2x, a256)') fileno
            goto 500       
          else
            fileno(i:i) = filenc(i:i)
          endif
 100    continue
 500    if(iout.eq.1) then
          write(nlog,110) filenc, filext, fileno
        endif
c
c _________________________________________________________
c

        return
c
c _________________________________________________________
c
c       Formats

 110    format(/, '  Namext; File in, extension, and file out',/,
     1           '  In:  ', a256,/,'  Ext: ', a3,/,'  Out: ',a256)
c
c _________________________________________________________
c

        end

