c                                                                       
c ************************************************************          
        subroutine getpath(filrsp,fpath1)
c
c               It finds the path for an input file
        character filrsp*72, fpath1*72, x*1
c     
c !!!           PC Specific                
        x='\'
c     
c !!!           SGI Specific                
c       x='/'
        fpath1=' '
        ichk=0

        do i=1,72
          ii=73-i
          if(filrsp(ii:ii).eq.x) then
            do j=1,ii
              fpath1(j:j) = filrsp(j:j)
            end do
            goto 120
          endif
        end do
            

  120   if(ichk.eq.1) write(6,130) filrsp, fpath1
  130   format(/,
     1         '  Getpath results; ',/,
     1         '           filrsp  ',a72,/,
     1         '           fpath   ',a72)
c       stop
        return    
        end
