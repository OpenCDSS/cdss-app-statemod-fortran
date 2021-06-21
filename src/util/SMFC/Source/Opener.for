c                                                                       
c ************************************************************
c
	subroutine opener(nlog, maxnf,fpath,iall)
c
c               It opens a response file
	    dimension fpath(5)
	    character filrsp*72, fpath*72
      
      iout=1
	    nfin = 0 
	    iadd=0
	    if(iall.eq.0) iadd=10

	    do 100 nf=1,maxnf                                               
	      call skipn(5,0)
	      read(5,'(a72)',end=110,err=120) filrsp
        write(nlog,'(a72)') ' Opener; ', filrsp
        
	      if(filrsp(1:3) .eq. '   ') goto 110
c     
c               Skip all characters beyond 2 blanks
          do i=1,72
            i1=i+1
            IF(filrsp(i:i1).EQ. '  ') then
              do j=i,72
                filrsp(j:j)=' '
              END do
            endif
          end do
     
	    open(nf+iadd, file=filrsp, status='old',err=120)
	    call skipn(nf+iadd,0)
	    nfin = nfin+1
	    call getpath(nlog, filrsp,fpath(nf))
	    write(26,140) nf, filrsp
	    write(nlog,140) nf, filrsp

  100 continue
	
  110 if(nfin.le.maxnf) then
        maxnf = nfin   
      else   
        write(nlog,*) ' '
        write(nlog,*) '  Problem, maximum # of files = 5'
        call flush(6)
        goto 130
      endif
      			     
      return                                                          
c
c               Warning Messages
  120   write(6,*) ' '
	write(6,*) '  Opener: Problem opening file ', filrsp
	write(nlog,*) ' '
	write(nlog,*) '  Opener: Problem opening file ', filrsp
  130   write(6,*)  '  Opener; Unsuccessful termination, see smfc.log'
	write(nlog,*) '  Opener; Unsuccessful termination, see smfc.log'
  140   format(
     1  '#',/
     1  '# Opener; Source file : ', i4, 1x, a72)                                
	call flush(6)
	stop 1
	end                                                             
