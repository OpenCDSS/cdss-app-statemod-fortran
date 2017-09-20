c
c ************************************************************
c
        subroutine putpath(maxfn, filrsp,fpath1)
c
c
c _________________________________________________________
c	Program Description
c
c       Putpath; It adds a path for an iput file if none provided
c
c
c _________________________________________________________
c	Dimensions
c
        character filrsp*256, fpath1*256, filrsp1*256, x*1
c jhb 2014/07/02 removed the precompiler commands
c                and hardwired the path separator for linux
      character(len=1), parameter :: path_sep='/'
c
c _________________________________________________________
c
c		iout=1 details; 2=summary
c
        iout=0
        nlog=99
        
c
c rrb 2007/09/25; Update
c       maxname=12
c       maxname=24
        maxname=48
        if(iout.eq.2) then
          write(nlog,*) ' Putpath; maxfn  ', maxfn
          write(nlog,*) ' Putpath; filrsp ', filrsp
          write(nlog,*) ' Putpath; fpath1 ', fpath1
        endif
        
        if(iout.eq.1) write(nlog,'(a)') ' filrsp ', filrsp
        if(iout.eq.1) write(nlog,'(a)') ' fpath1 ', fpath1
c
c rrb 04/07/14; Branch if file is blank        
        if(filrsp(1:2) .eq. '-1') goto 500
c _________________________________________________________
c               Step 1; Remove data to right of 2 blank spaces
c rrb 04/07/14
c       do i=1,maxfn
        do i=1,maxfn-1
          i2=i+1
          if(filrsp(i:i2).eq.'  ') then
            do ii=i2,maxfn
              filrsp(ii:ii) = ' ' 
            end do
            goto 100 
          endif
        end do
 100    continue
c     
c !!!           PC Specific                
c       x='\'
c     
c !!!           SGI Specific                
c       x='/'
c jhb added precomiler macro to determine the path separator
        x = path_sep
        filrsp1=filrsp
c
c _________________________________________________________
c               Step 2; Check to see if a path was provided
        do i=1,maxfn
          ii=maxfn+1-i
          ix=ii
          if(filrsp(ii:ii).eq.x) goto 130
        end do
c
c _________________________________________________________
c               Step 3; Add path
c
        do 120 i=1,maxfn
          if(fpath1(i:i).ne.' ') then
            filrsp(i:i) = fpath1(i:i)
          else
            ix=i
            ii=i-1
            do 110 j=1,maxfn
              if(filrsp1(j:j).ne.' ') then
                ii=ii+1                
                if(ii.gt.maxfn) goto 130
                filrsp(ii:ii) = filrsp1(j:j)
              else
                goto 130
              endif
  110       continue
          endif
  120   continue
c
c _________________________________________________________
c               Step 4; Check to see if filename > maxname characters or < 0
  130   j=0
        j1=0
        if(iout.eq.1) write(nlog,*) '  Putpath; ix = ', ix
c       write(nlog,131) filrsp 
c
        do i=ix+1,maxfn
c
c rrb; 02/10/04; Revise to allow a one character name before prefix
c         if(filrsp(i:i).eq.'.')  j1=1
          if(j1.eq.0 .and. filrsp(i:i).ne.' ') j=j+1
c
c rrb 2006/10/20; Correction to allow maxname characters to work          
cr        if(filrsp(i:i).eq.'.')  j1=1
          if(filrsp(i:i).eq.'.')  then
            j=j-1
            j1=1
          endif  

          if(iout.eq.1) then
            if(i.eq.ix+1) write(nlog,132) 
            write(nlog,134) i, j1, j, filrsp(i:i)
          endif
        end do
        
        if(j.lt.1 .or. j.gt.maxname) then

          write(6,*) '  Putpath; Problem with filename, see *.log', j
          write(nlog,*) '  Putpath; Problem with filename, see *.log', j
          write(nlog,150)  maxname, filrsp1, fpath1, filrsp
          goto 9999
        endif

        if(iout.ge.1) write(nlog,150)  maxname, filrsp1, fpath1, filrsp
c
c _________________________________________________________
c               Step 5; Return
 500    return      
c
c _________________________________________________________
c               Formats
  131   format('  Putpath; filrsp ',/, a256)
        
  132   format('  Putpath;    i   j1    j filrsp(i:i)',/
     1          '          ____ ____ ____ _')
  134   format('         ', 3i5, 1x, a1)

  150   format('  Putpath results; ',/,
     1         '    Note the current code requires the file name ',
     1             'be >0 and <=',i3' characters',/
     1         '    filrsp1 = ',a256,/,
     1         '    fpath1  = ',a256,/,
     1         '    filrsp  = ',a256)
 9999   write(6,*) 'Stop 1'
        call flush(6)
        call exit(1)

        stop
        end
