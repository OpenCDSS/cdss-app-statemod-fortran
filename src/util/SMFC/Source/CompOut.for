c                                                                       
c ************************************************************          
c                                                                       
	    subroutine compOut(nlog, nout, maxnf,ip, fileidX)
c
c               Similar to comp1 but it skips to first output record
c               indicated by the string 'Year'

	    character rec*72, rec1*300, rec1x*300, fileidX*5
c                                                                       
c               Initilize
      write(nlog,*) ' CompOut; fileidX = ', fileidX
c     write(6,*) ' Stopping in CompOut'     
c     write(6,*) nlog, maxnf, ip, fileidX
c     Stop
        
        
	    nf=10 
	    idiff=0          
	    rec=rec
	    iout=0
c
c               Find first line with the string Year
	      do 100 i=1,maxnf                                                
	        nf1 = nf+i                                                                                          
	        call skipy(nf1,100) 
  100   continue 
  
	      if(ip.eq.0) then
	        write(26,102)   'Full Printout   '
	        write(nlog,102) 'Full Printout   '
	      else
	        write(26,102)   'Differences only'
	        write(nlog,102) 'Differences only'
	      endif
	
 102    format(/,' ',/ '  CompOut; File Comparison - ', a16,/, ' ')
c                                                                       
c               Other Control Data based on file 1                      
	      nf = 11
        nin=0
	      i=0
  110   i=i+1
        rec1=' '
        rec1x=' '
	      read(nf,'(a300)',end=120,err=140) rec1 
	      read(nf+1,'(a300)',end=120,err=140) rec1x 

c         call getname(nlog, 1, 120, rec1, rec)
	      if (rec1.eq.rec1x) then  
	        if(ip.eq.0) then
	          write(26,150) ' Same:      ', 1, rec1, 2, rec1x  
            if(iout.eq.1) then
              write(nlog,150) ' Same:      ', 1, rec1, 2, rec1x 
            endif 
	        endif
	      else
	        idiff=idiff+1
	        write(26,152)   ' Difference:', idiff, 1, rec1, 2, rec1x
	        if(iout.eq.1)  then
	          write(nlog,152)' Difference:', idiff, 1, rec1, 2, rec1x
	        endif
	      endif                 
	      goto 110
c                                                                       
c              Close input and output file                              
  120   if(i.lt.16) goto 140
        nin=i
        nin2=i
	      do 130 nf=11,10+maxnf                                           
	        close(nf)                                                     
  130    continue                         


	    write(26,182)   nin, nin2,idiff
	    write(nlog,182) nin, nin2,idiff
	
 182  format(/
     1         '  Number of stations and differences:',/,
     1         '  File 1 Stations = ', i10,/,
     1         '  File 2 Stations = ', i10,/,
     1         '  Differences     = ', i10)

	    write(90,183) nout, fileidX, nin, nin2, idiff
 183  format(i4, 1x, a5, 3i10)    

	    return                             
c
c               Error Messages              
  140 write(nlog,*) '  Problem reading file ', nf,' in compOut'  
	    write(6,*)  '  Unsuccessful termination, see smfc.log'
	    write(nlog,*) '  Unsuccessful termination, see smfc.log'
  150 format(a12, /, 2x, i2, ' = ', a300,/ 
     1                 2x, i2, ' = ', a300,/)
  152 format(a12, 1x, i5, /, 2x, i2, ' = ', a300,/ 
     1                         2x, i2, ' = ', a300,/)

	    call flush(6)
	    stop 1
	    end                                                             
