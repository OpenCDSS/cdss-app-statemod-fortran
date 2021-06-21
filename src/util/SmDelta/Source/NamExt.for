c
c ************************************************************
c       Namext, it puts or replaces an extension on a name
        subroutine namext(filenc, filext, fileno)
c
        character filenc*72, filext*3, fileno*72
c       
        fileno = ' '                      
        do 100 i=1,72
          if(filenc(i:i) .eq. ' ' .or. filenc(i:i).eq.'.') then
            fileno(i:i) = '.'
            fileno(i+1:i+3) = filext
c           write(99,'(2x, a72)') filenc
c           write(99,'(2x, a72)') fileno
            goto 500       
          else
            fileno(i:i) = filenc(i:i)
          endif
 100    continue
 500    return
        end





