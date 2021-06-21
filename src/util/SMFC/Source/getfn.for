c
c *********************************************************
c
       subroutine Getfn(nlog, nf, iin, infile, maxfile, 
     1   fileName, ifileNum, filetype, fileID, filetypX)
c     
c      Getfn; Subroutine that reads in files in any order         
c              and parses thru them to get actual file name
c               Tyical input is *.rsp = FileName
c
c	      Called by Statem.for
c
c	2006/01/01; Add Network so it can be inlcuded in *.rsp file
c                   for GUI (*.net)
c	2006/03/21; Add Plan Well for well augmentation data (*.plw)
c	2006/03/21; Add Rio Grande Forecast (*.rgf)
c
c _________________________________________________________
c
c	Documentation
c
c               iin = response file number
c               infile = 0 standard sequential input
c                       = 1 random input
c               maxfile = max number of files in *.rsp
c
c               filena = first file read
c               fileName = vector of random input file names
c               ifileNum = vector of random input file #'s
c
c              Tyical input is: 'Response = Filename.rsp'
c
c _________________________________________________________
c
       dimension
     1   ifileNum(2,80), fileName(2,80), fileType(2,80),
     1   filetypX(80),   fileID(80), ifileNx(80)

       character filena*256,
     1   filetype*40, filetypX*40, FileName*256, FileId*5,
     1   fileT1*40,   fileT2*40, fileN1*256
     
c
c _______________________________________________________ 
c
c               Step 1; Initilize

c
c               iout = 0 no details
c               iout = 1 details
c               iout = 2 summary of files read
c		            iout = 3 summary plus list file types
       iout= 0
       
       if(iout.eq.1) write(nlog,*) ' Getfn; infile = ', infile
       infile=0
       nfsize=256
       
       do j=1,maxfile
         filename(nf,j) = '-1'
         ifilenum(nf,j) = 0
         filetype(nf,j) = '-1'
       end do  
       
       if(iout.eq.1) write(nlog,*) '  Getfn; iin, maxfile = ',
     1   iin, maxfile

c
c _______________________________________________________ 
c
c               Step 2; Random file, read in other files and store
c
c
       j1=0
       jp=0
       do i=1,maxfile
         
         if(iout.eq.1) then
           call skipn(iin,0)
           write(nlog,*) '  Getfn; i=', i
           read(iin,'(a256)',err=9999, end=500) filena
           write(nlog,*) '  Getfn 1; filena, ', filena           
           backspace(iin)
         endif
         
         call skipn(iin,0)
         read(iin,'(a256)',err=9999, end=500) filena         
         if(iout.eq.1) write(nlog,*) '  Getfn 2; filena, ', filena
         call getName(i, nlog,nfsize,ifound, filena,fileT1,fileN1)
c
c
c               Check for a blank file and/or Sequential input
         if(ifound.eq.0) then
           if(j1.eq.0) write(nlog,300)        
           goto 500
         else
           if(j1.eq.0) write(nlog,310)
           if(j1.eq.0 .and. iout.ge.1) write(nlog,202)
         endif

c _________________________________________________________
c
c               Find File Type
         ifound=0
         do j=1,maxfile
           if(fileT1.eq.filetypX(j)) then
             ifound=1
             j1=j1+1
             ifileNum(nf,j) = ifileNx(j)
             fileName(nf,j) = FileN1
             fileType(nf,j) = filetypX(j)
c             
c            if(iout.ge.2.and. ifilenum(i,j).ne.0) then
               jp=jp+1
               write(nlog,204)  jp, j, ifileNx(j), fileid(j),
     1          ifileNum(nf,j), fileT1, fileN1
             end if
c          endif
         end do

         if(i.eq.1 .and. ifound.eq.0) then
           write(nlog,302)        
           goto 500
         else
           infile=1
         endif  
c
c rrb 2011/08/15; Keep running to idenfity missing files        
cx         if(i.gt.1 .and. ifound.eq.0) goto 9998
        if(i.gt.1 .and. ifound.eq.0) then
          write(99,1460) fileT1
          pause
        endif
c
c _______________________________________________________ 
c		End file loop
       end do
c
c _______________________________________________________ 
c
c               Step 3; List file types
 500   continue
c      iout=3
       if(iout.eq.3) then
         write(nlog,312)
         write(nlog,203)
         
         do j=1,maxfile
           write(nlog,204)  j, j, ifileNx(j), fileid(j),
     1       ifilenum(nf,j), filetypX(j)
         enddo
       endif
c
c _______________________________________________________ 
c
c rrb 2005/10/14; Separate station files
c
c		Check if only 1 well station file is provided
       if(ifilenum(nf,9) .gt. 0 .and. ifilenum(nf,57) .gt. 0) then
         write(nlog,*) ' Getfn; 9, 57', ifilenum(nf,9), ifilenum(nf,57)
         fileT1=filetypX(9)
         fileT2=filetypX(57)
         goto 9997
       endif  
c       
c		Check if only 1 diversion station file is provided
       if(ifilenum(nf,5) .gt.0 .and. ifilenum(nf,54) .gt. 0) then
         write(nlog,*) ' Getfn; 5, 54', ifilenum(nf,5), ifilenum(nf,54)
         fileT1=filetypX(5)
         fileT2=filetypX(54)
         goto 9997
       endif  
c
c _______________________________________________________ 
c
c               Formats

 200   format(/,
     1 '  Getfn; File # to open = ', i5, /
     1 '         Type           = ', a40,/
     1 '         Name           = ', a256)
 202   format(/,
     1 '  Getfn;',/
     1 '    #    j Fn_1  FnID  Fn_2',
     1 ' Type                                    ', ' Name',/
     1 ' ____ ____ ____ _____ _____',
     1 ' ________________________________________', ' ',  72('_'))
 203   format(/,
     1 '  Getfn;',/
     1 '    #  Fn#  FnID   Fn#',
     1 ' Type                                    ',/
     1 ' ____ ____ _____ _____',
     1 ' ________________________________________')
 204   format(3i5, 1x, a5, 1x, i5, 1x, a40, 1x, a256)
 300   format(/,72('_'),/'  Getfn; Sequential input from *.ctl #1')
 302   format(/,72('_'),/'  Getfn; Sequential input from *.ctl #2')
 310   format(/,72('_'),/'  Getfn; Random input from *.ctl' )
 312   format(/,72('_'),/'  Getfn; Recognized file types for *.ctl' )        
 
 510   return

c
c _________________________________________________________
c
c               Error Tracking
c
 9997 write(99,1470) fileT1, fileT2
      write(6,1440)
      goto 1000
      
 9998 write(99,1460) fileT1
      write(6,1440)
      goto 1000


 9999 write(99,1450) 
      write(6,1440)
      goto 1000

 1440 format(
     1 '  Stopped in getfn, see the log file (*.log)')
 1450 format(
     1 '  Stopped in getfn; Problem reading response (*.rsp) file')
 1460 format(/
     1 '   ** Stopped in getfn; File Type not found ', a40,/)
     
 1470 format(/,
     1 72('_'),/
     1 '  Stopped in Getfn; Problem cannot provide both',/
     1 '  a standard station file = ', a40,/
     1 '  and a new station file  = ', a40,/
     1 '  Reconmend you pick one and remove the other')

 1000 write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop 
      END
c
c *********************************************************
c
       subroutine getName(i, nlog, nfsize, ifound,
     1   filena, fileT1, fileN1)

       character filena*256, fileN1*256, fileT1*40

       FileT1=' '
       FileN1=' '
       iout=0
c _______________________________________________________ 
c
c               Step 1; Find file type
c
       ifound=0
       do j=1,nfsize
         if(ifound.eq.0 .and. filena(j:j).eq.'=') then
           ifound=1
           fileT1=filena(1:j-1)
           j1=j+1
           j2=j1
         endif
       end do

c      write(nlog,*) 'j1,j2 ', j1, j2

       if(ifound.eq.0) goto 500

       if(iout.eq.1) then
         write(nlog,*) ' '
         write(nlog,100) i, fileT1
       endif
c _______________________________________________________ 
c
c               Step 2; Find first character of file name
       ifound=0
       do k=j1,nfsize
         if(ifound.eq.0) then
           if(filena(k:k).ne.' ') then
             ifound=1
c            write(nlog,*) ' k=', k
           else
             j2=j2+1
           endif
         endif
       end do
       j1=j2
c      write(nlog,*) 'j1,j2 ', j1, j2
cx

cx
c _______________________________________________________ 
c
c               Step 3; Find file name ending location
       ifound=0
       do k=j1,nfsize
         if(ifound.eq.0) then
           if(filena(k:k+1).eq.'  ') then
             ifound=1
c            write(nlog,*) ' k=', k
           else
             j2=j2+1
           endif
         endif
       end do
c      write(nlog,*) 'j1,j2 ', j1, j2
c
c _______________________________________________________ 
c
c               Step 3c; Set file name to scalar (rec256)
       fileN1 = filena(j1:j2-1)

       if(iout.eq.1) then
         write(nlog,120) i, fileT1, fileN1
       endif
c
c _______________________________________________________ 
c
c               Step 4; Return
 500  return
c
c               Formats
c _______________________________________________________ 
 100  format(
     1 '  GetName; File # Read = ', i5,/,
     1 '           Type        = ', a40)
 120  format(
     1 '  GetName; File # Read = ', i5,/,
     1 '           Type        = ', a40,/,
     1 '           Name        = ', a256)
      stop 
      END

