c                                                                       
c ************************************************************          
c                                                                       
	subroutine compT(nlog, nout, maxnf,maxsta,ip,is,fileidX)
c
c               Compare a time series file
c
c               subroutine reads and stores one file,
c               then compares to another
c
c               ip      print switch 
c               is      0 for all but target
c               is      1 for max target
c               is     -1 for min target
c                       For target file
c                         1 Skip records 2,4,... (get min)
c                         -1 Skip records 1,3,... (get max)

	     dimension rec1(maxsta), recid(maxsta), ifound(maxsta)
	     character rec1*140,  recid*12, rec1x*140, rec2x*140,
     1    recidx*12
	     character blank*140, recyr*4, recyrx*4, fileidX*5
c
c
c                                                                       
c               Initilize
       iout=0
	     nf=10 
	     idiff=0       
	                                       
	     blank = '* Not Available * '
c      blank = ' '

	      do 100 i=1,maxsta
  100     ifound(i) = 0
						  
	      do 110 i=1,maxnf                                                
	        nf1 = nf+i                                                    
	        call skipn(nf1,0) 
  110   continue 
  
	      if(ip.eq.0) then
	        write(26,102)   'Full Printout   '
	        write(nlog,102) 'Full Printout   '
	      else
	        write(26,102)   'Differences only'
	        write(nlog,102) 'Differences only'
	      endif
 102    format(/,' ',/ '  CompT; File Comparison - ', a16,/, ' ')
c
c rrb 2011/11/07; Read and echo time series card
        do i=1,maxnf
          nf1=nf+i
	        read(nf1,'(a140)',end=140,err=220) rec1(i)
	        if(iout.eq.1) write(nlog,*) 'CompT rec1 ', rec1(i)
	      end do
c                                                                       
c               One year at a time
	      iend=0
	      nf = 11                                                         
  120   i=0
	      nin=0
	      nin2=0
  130   i=i+1
	      if(i.gt.maxsta) goto 222

	      if(is.eq.-1) call skipx(nf,1)
	      
	      read(nf,'(a140)',end=140,err=220) rec1(i)
c
c rrb 2011/11/07; check for blanks at the end of the file
        rec2x=rec1(i)
        rec2x=adjustL(rec2x)
c       write(nlog,*) ' Rec2x_1 ', rec2x        
        if (rec2x(1:1) .eq.' ') then
c         write(nlog,*) ' Rec2x_2 ', rec2x
          goto 140
        endif
        
        
        if(iout.eq.1) then
          write(nlog,*) ' '
          write(nlog,*) ' compT; i ', i
          write(nlog,'(a140)') rec1(i)
        endif

	      if(is.eq.1)  call skipx(nf,1)
        
	      rec1x=rec1(i)        
	                       
c      
c                Identify and skip for each year
	      if(i.eq.1) then
	        recyr=rec1x(1:4)                                              
          if(iout.eq.1) write(nlog,*) 'Processing year ', recyr
	      else
	        recyrx=rec1x(1:4)
	        if(recyr.ne.recyrx) then
	          backspace(nf)
	          if(is.ne.0) backspace(nf)
	          goto 150
	        endif
	      endif
	     
	      recid(i)=rec1x(6:17)
	      if(iout.eq.1) write(nlog,*) 'CompT; i, recid ', i, recid(i)
	      goto 130
c
c          Read files 2-n
  140   iend=1
  150   nin=i-1
  160   if(is.eq.-1) call skipx(nf+1,1)
  
	      read(nf+1,'(a140)',end=180,err=220) rec1x
        if(iout.eq.1)  write(nlog,*) ' CompT; nin rec1x ', nin, rec1x

	      if(is.eq.1) call skipx(nf+1,1)
        
	      recyrx=rec1x(1:4)
	      recidx=rec1x(6:17)
	      nin2=nin2+1
	      if(iout.eq.1) write(nlog,*) ' CompT ', recyrx, recidx
c
c               Check for correct year
	     if(recyr.ne.recyrx) then
	       backspace(nf+1)
	       if(is.ne.0) backspace(nf+1)
	       goto 180
	     endif

       if(iout.eq.1) write(nlog,*) ' NIN = ', nin	
	     do 170 i=1,nin
c	       if(iout.eq.1) write(nlog,*) ' i, recid  ', recid(i)
c	       if(iout.eq.1) write(nlog,*) ' i, recidx ', recidx
	   
	       if(recid(i).eq.recidx) then
	         ifound(i) = 1
	         if(iout.eq.1) write(nlog,*) ' ifound ', ifound(i),recidx
       
	         if(rec1(i).eq.rec1x) then
	           if(ip.eq.0) then
	             write(26,230) ' Same:      ',1, rec1(i), 2, rec1x
	           endif
	         else
	           write(26,230) ' Difference:', 1, rec1(i), 2, rec1x
	           idiff=idiff+1
	         endif                 
	         goto 160
	       endif
  170   continue
c
c               Not found in file 1; write file 2 data
	      idiff=idiff+1
	      write(26,232) ' Difference:',idiff,1,blank,2,rec1x

	      goto 160
c
c               Check and print if file 1 data is not found
  180   do 190 i=1,nin
	        if(ifound(i).eq.0) then
	          idiff=idiff+1
	          write(26,232) ' Difference:',idiff,1,rec1(i), 2, blank  
	          if(iout.eq.1) then
	            write(nlog,*) '  '
	            write(nlog,232) ' Difference', idiff,1,rec1(i),2,blank 
	          endif      
	        endif
  190   continue   
c
c               Repeat for next year
	      if(iend.eq.0) goto 120
c                                                                       
c              Close input file 
c               (Unless a target file which will be called again)
  200   do 210 nf=11,10+maxnf                                           
	        if(is.eq.1) then
	          rewind(nf)
	        else
	          close(nf)
	        endif
  210   continue                 
	
c       close(26)
     
	      write(26,182)  nin, nin2,idiff
	      write(nlog,182) nin, nin2,idiff
     
 182    format(/
     1         '  Number of stations and differences:',/,
     1         '  File 1 Stations = ', i10,/,
     1         '  File 2 Stations = ', i10,/,
     1         '  Differences     = ', i10)
     
	      write(90,183) nout, fileidX, nin, nin2, idiff    
cx      write(nlog,183) nout, fileidX, nin, nin2, idiff 
 183    format(i4, 1x, a5, 3i10)    
     
     
	      return                             
c
c               Error Messages              
  220   write(nlog,*) '  Problem reading file ', nf,' in compt.f'
	      write(6,*)    '  Unsuccessful termination, see smfc.log'
	      write(nlog,*) '  Unsuccessful termination, see smfc.log'
	      call flush(6)
	      stop 1

  222   write(nlog,*) '  Problem dimension size in compT.f', maxsta
	      write(6,*)  '  Unsuccessful termination, see smfc.log'
	      write(nlog,*) '  Unsuccessful termination, see smfc.log'
	      call flush(6)
	      stop 1

  230   format(a12, /, 2x, i2, ' = ', a140,/ 
     1                 2x, i2, ' = ', a140,/)
  232   format(a12, 1x,i5, /, 2x, i2, ' = ', a140,/ 
     1                        2x, i2, ' = ', a140,/)
	end                                                             
