c                                                                       
c ************************************************************          
        subroutine putpath(filrsp,fpath1)
c
c               It adds a path for an iput file if none provided
        character filrsp*72, fpath1*72, filrsp1*72, x*1
c
c     
c !!!           PC Specific                
        x='\'
c     
c !!!           SGI Specific                
c       x='/'
        icheck=0                       
        filrsp1=filrsp
c
c               Check to see if a path was provided
        do 100 i=1,72
          ii=73-i
          if(filrsp(ii:ii).eq.x) goto 130
  100   continue              
c
c               Add path
        do 120 i=1,72
          if(fpath1(i:i).ne.' ') then
            filrsp(i:i) = fpath1(i:i)
          else         
            ii=i-1
            do 110 j=1,72
              if(filrsp1(j:j).ne.' ') then
                ii=ii+1                
                if(ii.gt.72) goto 140
                filrsp(ii:ii) = filrsp1(j:j)
              else
                goto 130
              endif
  110       continue
          endif
  120   continue
        goto 140

  130   if(icheck.eq.1) write(99,150)  filrsp1, fpath1, filrsp
        return      
                 
  140   write(6,*) '  Problem with putpath, see *.log'
        write(99,150)  filrsp1, fpath1, filrsp
  150   format('  Putpath results; ',/,
     1         '    filrsp1 ',a72,/,
     1         '    fpath1  ',a72,/
     1         '    filrsp  ',a72)
        end
