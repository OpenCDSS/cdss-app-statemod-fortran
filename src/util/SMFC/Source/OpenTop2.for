c                                                                       
c ************************************************************          
        subroutine opentop2(nlog, inf, maxnf, it, ifound,
     1    extx, descx, fpath, filename)
c
c               It opens filename(i,inf)
c		            inf = file type to open
c		            maxnf = number of files to open (2)
c		            it = 1 open a file
c		                 0 do not open a file
c               ifound = 0 file not found and opened
c                        1 file found or opened previouly (if it=0)
c		            extX = file extension
c		            descx= file description
c		            filename = filename
c
	      character extx*5, descx*40, filin*72, fpath*72
              dimension fileName(2,80), fpath(2)
	      character rec256*256, fileName*256, rec2*2	
	      	
	      iout=1	
	      ifound=0	
	      						
	      write(nlog,110) inf, extx, descx       
  100   format(   '  Opentop2; Processing ', i5, 1x, a5, '; ', a40)
  110   format(/, 60('_'),/
     1            '  Opentop2; Processing ', i5, 1x, a5, '; ', a40,/)
c                                                                       
	      if(it.eq.1) then                                                
	        nf=10
	        do i=1,maxnf 
	          nf=nf+1  
c
c		Set file name and open
            rec256=fileName(i,inf)
            if(iout.eq.1) then
              write(nlog,*) '  OpenTop2; nf, i, inf, rec256' 
              write(nlog,*) '  OpenTop2;', nf, i, inf, rec256
            endif
            
            filin=rec256(1:72)
            rec2=filin(1:2)
            
            if(rec2.ne.'-1') then
              ifound=1
c             call putpath(nlog, filin, fpath(i))            
              call putpath2(nlog, filin, fpath(i))            
              if(iout.eq.1) write(nlog,140) nf, filin
              

              open(nf, file=filin, status='old',err=150)
              call skipn(nf,0)  
            endif
c
c print files not opened to *.log    
cx            if(iout.eq.1) then         
cx              write(nlog,140) 
cx            endif
          end do
        endif
c
c rrb 2012/03/08; Update to work for a single file
        if(it.eq.0) ifound=1        
        if(ifound.eq.1) then
          write(6,100) inf, extx, descx       
          write(26,110) inf, extx, descx      
        endif

									
  130   format(                                                         
     1  '#',/                                                           
     1  '# *', a4, 1x, a24,/,                                           
     1  '# Opentop2; Comparison file name: ', 1x, a72)
  140   format('  OpenTop2; Opening file : ', i4, 1x, a72)                                

  160   format(/,
     1  '  OpenTop2; No File Opened')                                    
     
	      return                                                          
c
c               Error Messages              
  150   write(nlog,*) '  Opentop2; Problem reading input file ', nf,
     1    filename(i,inf)
	      write(6,200)
	      write(nlog,200) 	      
 200    format('  OpenTop2; Unsuccessful termination,',
     1         ' see smfc.log')

	      call flush(6)
	      stop 1
	      end                                                             
