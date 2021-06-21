c                            
c ************************************************************
c       Parse, it parses thru the command line to find request
c
c               filenc  = base file name
c
       subroutine parse(filenc)
         character commnd*127, filenc*72
c
c               Get command line data
         call getcl(commnd)  
c
c               Sgi specific
c       commnd = ' '
c       filenc = ' '
c       narg = 1
c       call getarg (narg, commnd) 
c
c               Find control file name (commnd is packed to left)
c ----------------------------------------------------------------
        do 100 i=1,127
          if(commnd(i:i) .ne. ' ') then
            filenc(i:i) = commnd(i:i)
          else
            goto 110
          endif
  100   continue

  110   if(i.eq.1) filenc = 'SmDelta.rsp'
        write(6,120) filenc
  120   format(' Input file name = ', a72)
        return
        end
