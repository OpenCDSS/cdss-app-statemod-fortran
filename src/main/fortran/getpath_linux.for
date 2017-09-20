c                                                                       
c ************************************************************          
        subroutine getpath(maxfn, filrsp,fpath1)
c
c
c _________________________________________________________
c	Program Description
c
c       Getpath; It finds the path for an input file
c
c _________________________________________________________
c
c       Update History               
c
c rrb 00/08/04; Added maxfn and revised file name from 72 to 256
c
c _________________________________________________________
c	Dimensions
        character filrsp*256, fpath1*256, x*1
c jhb 2014/07/02 removed the precompiler commands
c                and hardwired the path separator for linux
      character(len=1), parameter :: path_sep='/'
c     
c
c _________________________________________________________
c		Step 1; Initilize
c

c !!!           PC Specific                
c       x='\'
c     
c !!!           SGI Specific                
c       x='/'
c jhb added precomiler macro to determine the path separator
        x = path_sep
        fpath1=' '
c
c		iout =	0 no details        
c			1 details
c			2 summary
        iout=2
        if(iout.eq.1) write(99,110)

        do i=1,maxfn
          ii=maxfn+1-i
          if(filrsp(ii:ii).eq.x) then
            do j=1,ii
              fpath1(j:j) = filrsp(j:j)
            end do
            goto 120
          endif
        end do
c
c rrb 99/06/15; Check to see if filename > 8 characters or < 0
  120   j=0
        j1=0
        do i=ii+1,maxfn
          if(filrsp(i:i).eq.'.')  j1=1
          if(j1.eq.0 .and. filrsp(i:i).ne.' ') j=j+1
        end do
c       write(6,*) ' Getpath; ', ii,j
c       stop
c
c rrb 00/08/05; Allow one character name
c rrb 00/08/05; Allow up to 16 character names
c       if(j.lt.1 .or. j.gt.8) then
        if(j.lt.0 .or. j.gt.16) then
          write(6,*) ' Getpath; Problem with filename, see *.log',ii,j
          write(6,130) filrsp, fpath1
          goto 999
        endif

        if(iout.eq.2) write(99,130) filrsp, fpath1
        
  110   format(/,72('_'),/ '  Getpath; ')
        
  130   format(/,72('_'),/
     1   '  Getpath; ',/
     1   '    File Name:       ',a256,/,
     1   '    Path:            ',a256)

        return
 999    write(6,*) 'Stop 1'
        call flush(6)
        call exit(1)

        stop
        end



