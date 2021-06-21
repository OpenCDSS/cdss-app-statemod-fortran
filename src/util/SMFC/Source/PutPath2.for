c
c ************************************************************          
	     subroutine putpath2(nlog, filrsp, fpath1)
c
c               It adds a path for an iput file if none provided
c		            It checks for relative path information and sets
c		            accordingly
c
c      Documentation
c        filrsp is the file to be opened
c        fpath is the path to add
c
       character filrsp*72, fpath1*72, filrsp1*72, x*1, rpath*3,
     1	 fpath2*72
	
c
c     
c !!!           PC Specific                
	     x='\'
	     rpath='..\'
c     
c !!!           SGI Specific                
c       x='/'
	     iout=1                           
	     filrsp1=filrsp
	
c
c _________________________________________________________
c               Check to see if a relative path was provided
       write(nlog,*) ' '
       ie=0
	     do i=1,72
	       ib=ie+1
	       ie=ib+2
	       if(filrsp1(ib:ie).eq.rpath) then
	          if(iout.eq.1) then
              write(nlog,*) ' Relative Path provided ', filrsp1
              write(nlog,*) ' Original Path provided ', fpath1
            endif
c
c		Find relative path to be added
            fpath2=fpath1
            ifound=0
            
            j1=73
            do j=1,72
              j1=j1-1
              if(fpath2(j1:j1).eq.'\') ifound=ifound+1
              if(ifound.eq.2) then
                filrsp(1:j1) = fpath2(1:j1)
c	              write(nlog,*) ' Adjusted path ', filrsp
c
c		Now add remainder of name                
                k1=j1+1
                k2=72-ie
c               write(nlog,*) ' k1, k2, ie ', k1, k2, ie
                
                ie1=ie
                do k=k1,k2
                  ie1=ie1+1
                  filrsp(k:k) = filrsp1(ie1:ie1)
                end do  
c	              write(nlog,*) ' Adjusted name ', filrsp
	              goto 130
              endif                
            end do  
          endif  
        end do
c
c _________________________________________________________
c               Check to see if a path was provided
c		            or added above. If so exit
       	do i=1,72
       	  ii=73-i
       	  if(filrsp(ii:ii).eq.x) goto 130
        end do   
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
  110         continue
	          endif
  120     continue
	        goto 140

  130   if(iout.eq.1) write(nlog,150)  filrsp1, fpath1, filrsp
	      return      
		 
  140   write(6,*) '  Problem with putpath2, see *.log'
	      write(nlog,150)  filrsp1, fpath1, filrsp
  150   format(/,
     1         '  Putpath2 results; ',/,
     1         '           filrsp1 ',a72,/,
     1         '           fpath1  ',a72,/
     1         '           filrsp  ',a72)
	      end
