c
c *********************************************************
c
          subroutine stafind(nlog,istop,itype,nsta,nx,cx,
     1      cstaidx, cCallBy)
c
c
c _________________________________________________________
c	Program Description
c
c       Stafind; it reads various data for mdainp.f
c
c _________________________________________________________
c       Documentation
c               nlog    debug output file
c               istop   0 = stop if not found
c                       1 = OK if not found
c               itype = 0 stream struture
c                       1 ISF structure
c                       2 reservoir structure
c                       3 diversion structure
c                       6 well
c                       7 plan
c               nsta    number of stations   
c               nx      pointer to ID to find
c               cx      character ID to find
c               cstaidx array of station ID's
c
c _________________________________________________________
c	Dimensions
c                       
        dimension cstaidx(*), cstrtypX(10)
        character cstaidx*12, cx*12, cstrtypX*12, cCallBy*12
        data cstrtypX/
     1   'InStream    ', 'Reservoir   ', 'Diversion   ', 'NA          ',
     1   'NA          ', 'Well        ', 'Plan        ', 'NA          ', 
     1   'NA          ', 'NA          '/        
c
c _________________________________________________________
c		Step 1; Initilize
        if(itype.ne.0) iout=0
        iout=0

        if(iout.eq.1) write(nlog,*) ' StaFind; itype, nsta, cstaidx = ',
     1    itype, nsta, cstaidx(1)
     
        do nx =1,nsta            
          if(iout.eq.1) write(nlog,*) ' StaFind', nx, cx, cstaidx(nx)                              
          if(cstaidx(nx).eq.cx) goto 500
        end do
        nx=0
c
c               Print problem could not find station
c rrb 20089/03/16; Do not warn if istop = 1 OK if not found)	
cx        write(nlog,100) cstrtypX(itype), cx, cCallBy 
cx        if(istop.eq.0) goto 9999
        if(istop.eq.0) then
          write(nlog,100) cstrtypX(itype), cx, cCallBy 
          goto 9999
        endif
c
c _________________________________________________________
c
c               Return
  500 continue
      if(iout.eq.2) then
        write(nlog,102) nx, cstaidx(nx), cx
      endif
      return
c
c _________________________________________________________
c
c               Formats
  100 format(/,72('_'),/                                                          
     1 '  Stafind; Warning ',                                             
     1     'A ', a12, ' structure with ID = ', a12,/,
     1 11x,'Called by: ', a12,/
     1 11x,'Could not be found in its station file')                     

  102 format(                                                          
     1 '  Stafind;',                                             
     1 'Structure ID found at position ', i5, 1x, a12,1x, a12)

 9999 write(6,110)
      write(nlog,110)
 110  format('  Stopped in Stafind, see the log file (*.log)')
      write(6,*) 'Stop 1'
      call flush (6)
      call exit(1)

      stop
      end
