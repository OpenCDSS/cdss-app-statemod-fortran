c
      subroutine comment(nf, nlog, iocode, nchk, nchkOut)
c
c
c _________________________________________________________
c	Program Description
c
c     It allows a comment (#) to be in a data file      
c

c
c _________________________________________________________
c	Update History
c		NA
c
c _________________________________________________________
c	Documentation
c
c		nf	 file to read
c	 	nlog	 log file #		
c		iocode = 0 comment found
c			 1 data found
c			 2 EOF found
c			 3 Error reading data
c		nchk	 check file #
c		nchkOut =0 do not print comment to log file
c                        1 print comment to log file and a header
c                          every time a comment (#)in encountered
c                        2 print comment to log file (no addl header)
c jhb more explanation
c     the idea is to continuously read lines until
c       end of file is reached, exiting with iocode=2, or
c       a read error occurs, exiting with iocode=3, or
c       a line is read without a # as the first character
c         this is where it gets tricky...it might NOT be a valid data record
c
c
c _________________________________________________________
c	Dimensions
c        

      character rec1*1, rec80*80, ljrec80*80
c
c _________________________________________________________
c	Initilize
c        
      
      iout=0
      iocode=0
      ncomm=0
c
c _________________________________________________________
c		Check for a comment
 90   read(nf,'(a80)',end=100,err=110) rec80
      if(iout.eq.1) write(nlog,*) rec80
c jhb I think this is necessary for the code logic to work on ifa files
c     where the first character on a data line can be blank
      ljrec80 = adjustl(rec80)
c      rec1=rec80(1:1)
      rec1=ljrec80(1:1)
c
c rrb 2006/12/14; Add EOF character associated with a copy command 
      if(rec1.eq.'') goto 100
 
      if(iout.eq.1) write(nlog,*)  '  Comment; rec1 = ', rec1 
      if(rec1.eq.'#') then
        iocode=0
        ncomm=ncomm+1
c
c		Print header and comment to *.chk        
        if(nchkOut.gt.0) then
          if(nchkOut.eq.1 .and. ncomm.eq.1) write(nchk,200)
          write(nchk,'(a80)') rec80 
        endif  
        goto 90
      else
        if(iout.eq.1) write(nlog,*)  '  Comment; Data = ', rec1
        backspace(nf)
        iocode=1
      endif
      goto 500
c
c _________________________________________________________
c		EOF found      
 100  iocode=2
      goto 500
c
c _________________________________________________________
c		Error reading data      
 110  iocode=3
      goto 500          
      
 200  format('#',/'# ', 60('_'))     
c
c _________________________________________________________
c		Return      
 500  return 
      end
      
