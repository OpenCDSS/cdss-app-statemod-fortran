      subroutine GetFile(
     1   nlog, nchk, iin, infile, maxfile, maxfn, 
     1   inX, isufX, numX, fileTypX, fileSufX, fileNamX,
     1   fpathX, filena)
c
c	It gets station data using
c	The old or new Response File format     
c
c	             nlog		log file #
c	             nchk		check file #
c	             iin		    old response file #
c	             infile		response file type 1=old, 2=new
c	             maxfile		maximum number of files
c	             maxfn		  maximum file size (256)
c              
c	             inX		File number to open
c	             isufX		version indicator 
c	             numX		number of stations read
c	             fileTypX	file type to be opened
c	             fileSufX	file suffix (e.g. *.dds)
c	             fileNamX	file (name) to be opened
c	             fpathX		path
c	             filena		file opened
      
      character fileNamX*256, filena*256, fpathX*256, ftype*16,
     1  fout*48, fileTypX*40, fileSufX*5
      
      iout=0
      isufX=1
      filena='-1'
c
c		Print to screen            
      fout=' '
      fout(1:40)=fileTypX
      fout(41:45) = fileSufX
      write(nlog,200) fout
  200 format(/, '  GetFile; Opening ', a48)
      
c
c _________________________________________________________      
c		Step 1; Read or Set response file version
c
      if(infile.eq.1) then
        filena=fileNamX
      else
        READ(IIN,110,end=926,err=928) FILENA
      endif
      
      filena=adjustL(filena)      
      write(nlog,120) filena
c
c _________________________________________________________      
c		Step 2; Warn if no data is provided      
      if(filena(1:2).eq.'-1') then
        write(nlog,210)
 210    format(/,'  GetFile; FYI no data provided')
        numX=0
        goto 500
      else
        numX=1
      endif
c
c _________________________________________________________
c
c		Step 3; Add path and Print header to *.log and *.chk
      call putpath(maxfn, filena, fpathX)
      open(inX, file=filena,status='old',err=928)
      
c _________________________________________________________      
c		Step 4 Get file version      
      call GetVer(nlog, inX, isufX, filena)
c
c _________________________________________________________      
c		Return      
 500  return
c
c _________________________________________________________      
c
c		Formats
  110 FORMAT(A72)  
  120 format(4x, a256)
                                                       
  201 format(/,72('_'),/
     1 '  GetFile; FYI file name = ', a256)
      
c
c _________________________________________________________      
c		Error Processing
  926 write(nlog,927) iin, filena
  927 format(/,
     1 72('_'),/
     1 '  GetFile; Problem. End of file # ', i4, ' encountered',/,
     1       '   File name: ', a256)
      goto 9999
      
  928 write(nlog,929) iin, filena
  929 format(/
     1 72('_'),/
     1 '  GetFile; Problem reading file # ', i4,/,
     1 '          File name: ', a256)
 1250 format(/, 72('_'), /,
     1'  GetFile;  Stopped in GetFile, see the log file (*.log)')
     
c      
c _________________________________________________________      
      
 9999 write(6,1250)
      write(nlog,*) ' Stopped in GetFile, see log file (*.log)'
      write(6,*) 'Stop 1'
      call flush (6)
      call exit(1)

      stop 
      end
