c                                                                       
c ************************************************************          
c                                                                       
	    subroutine compID(nlog, nout, maxnf, maxsta,ip,it, fileidX)
c
c               It compares a Station file 
c
c               subroutine reads and stores one file,
c               then compares to another
c               it = 0  character id is in column 1-12
c                    1  character id is in column 1-36 (for *.out)
c                    2  character id is in column 6-17 (*.tsp)
	      dimension rec1(maxsta), recid(maxsta), ifound(maxsta),
     1            rec36(maxsta), rec6(maxsta)
        dimension nfile(maxsta)
        character rec1*140,  recid*12, rec1x*140, recidx*12
        character rec36*36, rec36x*36, rec6*12,   rec6x*12, fileidX*5
	      character blank*140
c                                                                       
c               Initilize
	      nf=10 
	      idiff=0    
        nin=0
        nin2=0
	      blank = '* Matching Station not Found * '

	      do i=1,maxsta
          ifound(i) = 0
          nfile(i)=0
        end do
						  
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
 102    format(/,' ',/ ' CompID; File Comparison - ', a16,/, ' ')
c                                                                       
c               Other Control Data based on file 1                      
	      nf = 11        
	      nf1=1                                               
	      i=0
  120   i=i+1
	      if(i.gt.maxsta) then
	        write(nlog,194) maxsta
	         nin=maxsta
	         nin2=maxsta
	         idiff=-1
	        goto 181
	      endif

	      read(nf,'(a140)',end=130,err=190) rec1(i)          
	      rec1x=rec1(i)                    
	      recid(i)=rec1x(1:12)
        rec6(i)=rec1x(6:17)
	      rec36(i)=rec1x(1:36)
	      nfile(i)=nf1
c
c               Skip blanks
	      if(recid(i).eq.'            ') i=i-1
	      if(rec1x(1:1).eq.'#') i=i-1
	      goto 120

  130   nin=i-1
        nf1=nf1+1
  140   read(nf+1,'(a140)',end=160,err=190) rec1x 
	      recidx=rec1x(1:12)
        rec6x=rec1x(6:17)
      	rec36x=rec1x(1:36)
      	nfile1=nf1
c
c               Skip blanks and comments
      	if(recidx.eq.'            ') goto 140
      	if(rec1x(1:1).eq.'#') goto 140
      	nin2=nin2+1

       	do 150 i=1,nin
c
c rrb 2011/11/07; skip checks if the same file
          if(nfile(i).ne.nfile1) then
       	  if(recid(i).eq.recidx .and. it.eq.0 .or.
     1       rec36(i).eq.rec36x .and. it.eq.1 .or.
     1       rec6(i) .eq.rec6x  .and. it.eq.2) then
       	    ifound(i) = 1
       
       	    if(rec1(i).eq.rec1x) then
       	      if(ip.eq.0) then
       	      	write(26,200) ' Same:      ',1, rec1(i), 2, rec1x
       	      endif
       	    else
       	      idiff=idiff+1
       	      write(26,202)   ' Difference:', idiff,1,rec1(i),2,rec1x
       	    endif                 
	          goto 140
	        endif
	        endif
  150   continue
c
c               Not found in file 1; write file 2 data
	      idiff=idiff+1
	      write(26,202) ' Difference:',idiff,1,blank,2,rec1x

	      goto 140
c
c               Check and print if file 1 data is not found
  160   do 170 i=1,nin
	       if(ifound(i).eq.0) then
	         idiff=idiff+1
	         write(26,202) ' Difference:', idiff,1,rec1(i), 2, blank
	       endif
  170   continue
c                                                                       
c              Close input and output file                              
      	do 180 nf=11,10+maxnf                                           
      	  close(nf)                                                     
  180   continue                 
	
c
c _________________________________________________________
c		Exit point when dimension is exceeded
 
  181   continue
	      write(nlog,182) nin, nin2, idiff
	      write(26,182) nin, nin2, idiff
	      write(90,183) nout, fileidX, nin, nin2, idiff     
cx      write(nlog,183) nout, fileidX, nin, nin2, idiff  
 183    format(i4, 1x, a5, 3i10)    
	
 182    format(/
     1         '  Number of stations and differences:',/,
     1         '  File 1 Stations = ', i10,/,
     1         '  File 2 Stations = ', i10,/,
     1         '  Differences     = ', i10)
c       write(26,*) '  Number of stations and differences = ',
c    1              nin, nin2,idiff
	      return                             
c
c               Error Messages              
  190   write(nlog,*) '  Problem reading file ', nf,' in compid.f' 
	     write(6,*)  '  Unsuccessful termination, see smfc.log'
	     write(nlog,*) '  Unsuccessful termination, see smfc.log'
	     call flush(6)
	     stop 1

  194   format(/,
     1  ' CompID; Problem with dimension size = ',i5,/
     1  '         Moving on with a partial comparison')
	      return
	      stop 1

  200   format(a12, /, 2x, i2, ' = ', a140,/ 
     1                 2x, i2, ' = ', a140,/)
  202   format(a12, 1x, i5, /, 2x, i2, ' = ', a140,/ 
     1                         2x, i2, ' = ', a140,/)

	end                                                             
