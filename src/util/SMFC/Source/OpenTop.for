c                                                                       
c ************************************************************          
        subroutine opentop(nlog, ix,maxnf,it,iopen,filout,
     1    extx,descx,fpath)
c
c               It reads a file name from file nf and opens it
c               it = Not used
c               iopen >= 1 read file name and open it
c                     <= 0 (typically -1) read file name do not open it
c		            it     = 1 read file names and open the file
c			                   0 do not read file names
c			          Note if iwell=0,it does not open a well file
c
	      dimension fpath(2)
	      character filout*72, filin*72, extx*4, descx*24, fpath*72
	      								
	      write(6,100) extx, descx       
	      write(26,110) extx, descx       
	      write(nlog,110) extx, descx       
  100   format(   ' Opentop; Processing ', a4, '; ', a24)
  110   format(/, 60('_'),/
     1            ' Opentop; Processing ', a4, '; ', a24)
								
			  iout=0	
	      ix=ix
c       filout(ix+1:ix+4) = extx                                        
c       open(26, file=filout, status='unknown')                         
c                                                                       
c               Print output                                            
	      write(26,130) extx, descx, filout   
	      write(nlog,*) '  OpenTop; ', extx, descx, filout                            
	      write(nlog,130) extx, descx, filout                               
c                                                                       
c               Get file names                                          
	      if(it.eq.1) then                                                
	        do 120 nf=1,maxnf     
	          nf1=nf+10                                                   
	          read(nf,'(a72)',end=150,err=150) filin
c       
c                 Skip all characters after 2 spaces
            do i=1,72
              i1=i+1
              IF(filin(i:i1).EQ. '  ') then
                do j=i,72
                  filin(j:j) = ' '
                END do
                GOTO 112
              endif
            end do
c           write(nlog,*) ' Opentop ', nf, filin
  112	      call putpath(nlog, filin,fpath(nf))
        
            if(iopen.ge.1) then
              write(26,140) nf, filin
              write(nlog,140) nf, filin
              open(nf1, file=filin, status='old',err=150)
            else
              write(26,142) nf, filin
              write(nlog,142) nf, filin
            endif
  120     continue                                                      
	      endif                                                           
			  						
  130   format(                                                         
     1  '#',/                                                           
     1  '# *', a4, 1x, a24,/,                                           
     1  '# Opentop; Comparison file name: ', 1x, a72)
  140   format(
     1  '# OpenTop; Opening Source file : ', i4, 1x, a72)                                
  142   format(
     1  '# Skipping Source file : ', i4, 1x, a72)                                
	      return                                                          
c
c               Error Messages              
  150   write(nlog,*)'  Opentop; Problem reading input file ', nf
cx	      write(6,*)'  OpenTop; Unsuccessful termination, see smfc.log'
cx	      write(nlog,*)'  OpenTop; Unsuccessful termination, see smfc.log'
	      call flush(6)
	      stop 1
	      end                                                             
