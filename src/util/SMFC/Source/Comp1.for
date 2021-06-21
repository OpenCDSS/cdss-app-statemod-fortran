c                                                                       
c ************************************************************          
c                                                                       
	      subroutine comp1(nlog, nout, maxnf, ip, fileidx)
c
c               It compares a control file
	      character rec*72, rec1*140, rec1x*140, fileidX*5
c                                                                       
c               Initilize

       iout=0
	     nf=10 
	     idiff=0          
	     rec=rec
	     nin=0
	     nin2=0
	     					  
	     do 100 i=1,maxnf                                                
	       nf1 = nf+i                                                    
	       call skipn(nf1,0) 
  100  continue 
						       
	     if(ip.eq.0) then
	       write(26,102)   'Full Printout   '
	       write(nlog,102) 'Full Printout   '
	     else
	       write(26,102)   'Differences only'
	       write(nlog,102) 'Differences only'
	     endif
 102   format(/,' ',/ ' Comp1; File Comparison - ', a16,/, ' ')
									
c                                                                       
c               Other Control Data based on file 1                      
	     nf = 11                                                         
	     i=0
 110   i=i+1
c
c		Read file 1  
  112	  read(nf,'(a140)',end=120,err=140) rec1  
          if(iout.eq.1) write(nlog,*) ' Comp1; ', rec1
          IF(rec1(1:1).eq.'#') GOTO 112
          if(iout.eq.1) write(nlog,*) rec1
c
c		Read file 2
  114	  read(nf+1,'(a140)',end=120,err=140) rec1x
          IF(rec1x(1:1).eq.'#') GOTO 114

c         call getname(nlog, 1, 120, rec1, rec)
	        if (rec1.eq.rec1x) then  
	          if(ip.eq.0) then
	            write(26,150) ' Same:      ', 1, rec1, 2, rec1x  
	          endif
	        else
	          idiff=idiff+1
	          write(26,152)   ' Difference:', idiff, 1, rec1, 2, rec1x
	        endif                 
	        goto 110
c                                                                       
c              Close input and output file                              
  120     if(i.lt.16) goto 140
	        do 130 nf=11,10+maxnf                                           
	        close(nf)                                                     
  130   continue                         
c       close(26)
	      write(nlog,*) 'Number of differences = ', idiff
	      write(26,*) '  Number of differences = ', idiff
	      
	      write(90,183) nout, fileidX, nin, nin2, idiff
 183    format(i4, 1x, a5, 3i10)    	      
	      return                             
c
c               Error Messages              
  140   write(nlog,*) ' '
        write(nlog,*) '  Problem reading file ', nf,' in comp1'  
	      write(6,*)    '  Unsuccessful termination, see smfc.log'
	      write(nlog,*) '  Unsuccessful termination, see smfc.log'
  150   format(a12, /, 2x, i2, ' = ', a140,/ 
     1                 2x, i2, ' = ', a140,/)
  152   format(a12, 1x, i5, /, 2x, i2, ' = ', a140,/ 
     1                         2x, i2, ' = ', a140,/)

	     call flush(6)
	     stop 1
	     end                                                             
