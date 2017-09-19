c	    
c ************************************************************
      subroutine getin(inx, iid, maxsta, idallx, idtype,
     1                   ftype, ptype, idreq)                
c
c
c _________________________________________________________
c	Program Description
c
c       Getin; It reads an output file request (*.out)
c               It process file type and parameter type 
c               If all IDs are requested, it exits
c               If selected ID's are requested, it checks
c                 for those turned on and stores in array idreq
c                                    
c               inx =   0 called by outdiv (diversion)
c               inx =   2 called by outres (reservoir)
c               inx =   3 called by outopr (operation right)
c               inx =   4 called by outifr (instream reach)
c               inx =   6 called by outwel (well)
c               inx =   7 called by outxss (structure summary)
c               inx =   8 called by outsp  (special parameter)
c               inx =   9 called by outspd (daily special parameter)
c
c               idallx = 0 print all
c                        1 print selected ID's
c
c _________________________________________________________
c	Dimensions
c
        dimension idreq(maxsta), idtype(maxsta)

	      character ftype*24, ptype*24, idreq*12, rec12*12, 
     1            rec3*3, rec24*24, rec4*4, idtypx*3
c
c _________________________________________________________
c
c                                             
	     ichk1 = 0  
c                           
c
c _________________________________________________________
c               Get file type
       rewind(22)
       call skipn(22)
       read(22,'(a24)',end=180,err=180) ftype
       if(ichk1.eq.1) write(99,100) ftype
c
c _________________________________________________________
c               Get parameter type
       call skipn(22)
       read(22,'(a24)',end=190,err=190) ptype
       if(ichk1.eq.1) write(99,110) ptype
c
c _________________________________________________________
c               Get Id requested (0=all)
       iid = 0
       call skipn(22)
       idallx=1
  120  read(22,130,end=200,err=200) rec12, rec24, idtypx, ix
       if(ichk1.eq.1)  write(99,140) rec12, rec24, idtypx, ix
c
c _________________________________________________________
c               Print all
       rec3=rec12(1:3)
       if(rec3.eq.'All' .or. rec3.eq.'all' .or.
     1    rec3.eq.'ALL' .or. rec3.eq.'0  ') then
         if(ichk1.eq.1) write(99,150) rec12
	       idallx=0
	       goto 170
       endif
			    
       rec4=rec12(1:4)
c      write(99,'(1x, a4)') rec4
       if(rec4.eq.'-999' .or. rec4.eq.'    ') then
c
c _________________________________________________________
c               Warn user no ID's read
	       if(iid.eq.0) then
	         write(99,*) '  Getin.for; Warning no output',
     1         ' stations requested in file *.out' 
	         write( 6,*) '  Getin.for; Warning no output',
     1         ' stations requested in file *.out' 
c
c rrb 2007/09/06; Warning, not an error    
c            goto 200
             goto 170
	       endif
       else
c
c _________________________________________________________
c               Print only stations turned on
	    if(ix.eq.0) goto 120
      
	    iid = iid+1
	    idreq(iid) = rec12                    
c
c               Set structure type
	    if(idtypx.eq.'DIV') idtype(iid) = 0
	    if(idtypx.eq.'ISF') idtype(iid) = 0
	    if(idtypx.eq.'FLO') idtype(iid) = 1
	    if(idtypx.eq.'RES') idtype(iid) = 2
	    if(idtypx.eq.'OPR') idtype(iid) = 3
	    if(idtypx.eq.'OTH') idtype(iid) = 4
	    if(idtypx.eq.'WEL') idtype(iid) = 6
      
	    if(inx.eq.2 .and.idtypx.ne.'RES') goto 160
	    if(inx.eq.3 .and.idtypx.ne.'OPR') goto 160
	    if(inx.ne.3 .and.idtypx.eq.'OPR') goto 160
	    if(inx.eq.4 .and.idtypx.ne.'ISF') goto 160
	    if(inx.eq.6 .and.idtypx.ne.'WEL') goto 160
c
c               Stream Summary *.xss
	    if(inx.eq.7) then
	      if(idtypx.eq.'ISF') goto 160
	      if(idtypx.eq.'FLO') goto 160
	      if(idtypx.eq.'RES') goto 160
	      if(idtypx.eq.'OPR') goto 160
	      if(idtypx.eq.'OTH') goto 160
	    endif

	    if(ichk1.eq.1) then
	      write(99,150) rec12, rec24, idtypx, ix
	    endif
	    if(rec12(1:1).eq.'#') goto 210

  160    goto 120
       endif                      
c
c               Print final results to be printed
       if(ichk1.eq.1) then
	       do i=1,iid
	         write(99,162) i, idreq(i)
  162       format('  Getin; ID to print = ', i5, 1x, a12)
	       end do
       endif

  170  return  
c  
c _________________________________________________________
c               Formats
  100  format('  Getin; Output file type      = ', a24)
  110  format('  Getin; Output parameter type = ', a24)
  
  130  format(a12,1x,a24,1x,a3,1x,i5)
  140  format('  Getin; ID read = ', a12,1x,a24,1x,a3,1x,i5)

  150    format('  Getin; ID Print= ', 
     1             a12, 1x, a24, 1x, a3, 1x, i5)

c
c _________________________________________________________
c
c               Error messages    
c
  180   write(99,*) '   End of file or error reading file type'
	      goto 230
c
c               Problem reading file type
  190   write(99,*) '   End of file or error reading parameter type'
       	goto 230
c
c               Problem reading id
  200   write(99,*) '   Getin; End of file or error reading ID', rec12
	      write(99,*) '          Remember to end with a -999'
	      write(99,*) '          Number of IDs read (should be > ',
     1                        'or = 1): The number read = ',iid
	      goto 230
c
c               Problem reading time
  210   write(99,220)  rec12
  220   format('   Problem with ID provided = ', a12,/,
     1        '   Remember end the Id specification with -999')
	      goto 230

  230   write(6,*) '  Stopped in Getin, see the log file (*.log)'
       	write(99,*) '  Stopped in Getin'
        write(6,*) 'Stop 1'
      	call flush(6)
        call exit(1)

        stop
	      end
