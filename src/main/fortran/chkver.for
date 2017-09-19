c
      Subroutine ChkVer(nlog, inX, itype, nX, filena)
c
c	ChkVer; It estimates the version of a data file provided      
c
c _________________________________________________________
c
c      
c      Documentation
c	nlog	= log file #
c	inX	= input file #
c	itype 	= 31 (*.ipy file)
c	small	= small value (e.g. 0.001)
c	nX	= file version
c	filena	= file name
c
c _________________________________________________________
c
      dimension x(3)
      character filena*256
      character
     1      cidvri*12,   nameoX*24,   cgoto*12,                             
     1      ciopde*12,   ciopso1*12,  ciopso2*12,
     1      creuse*12,   cdivtypX*12
c
c _________________________________________________________
c

      iout=0     
     
      small=0.001     
      
      if(iout.eq.1) then
        write(nlog,*) ' ChkVer; ', nlog, inx, itype, small, nx, filena
        write(nlog,*) ' ChkVer_1; small ', small
      endif
c
c _________________________________________________________
c
c		Test *.ipy file 
c		Approach: Read first line of data using old 2 
c		  land use format. Based on the value of gwmode, 
c		  determine if it is a 2 or 4 land use file type
 
      if(itype.eq.31) then
c
c ---------------------------------------------------------
c rrb 2007/09/28; Read first line of data using old 2 land use format.
c		  Based on the value of gwmode, determine if it
c		  is a 2 or 4 land use file type
c rrb 2008/12/01; Revise to read new, 4 land use format.
c		    also set nx=1 (old format) and exit to 500 
c		    if an error is encountered reading an old format
cx         read (inX,200,end=400,err=400) idyr,cistat, (x(i), i=1,3),
cx     1	     Agw, Asp, Qmax, gwmode1, Atot    
cx
          nx=1   
          read(inX,954,end=400,err=500) idyr,cistat, (x(i), i=1,3),
     1	      AreaSF1, AreaSS1, AreaGF1, AreaGS1, Qmax, gwmode1, Atot
          
         backspace(inX)

         write(nlog,300)  'Annual Time Series File (*.ipy)'   
         if(gwmode1.lt. small) then
           nX=1
           write(nlog,310)
         else
           nX=2
           write(nlog,320)
         endif
         goto 500
      endif
c
c _________________________________________________________
c
c		Test *.opr file 
c		Approach: Read first line of data. 
c		  Based on the value of cdivtypX
c		  determine the file format
      if(itype.eq.13) then

c		Unknown Format, try to read new one
c		If an error goto to 100 to read old one
        read(inX,330,end=400,err=332)
     1      cidvri,      nameoX,    cgoto,                             
     1      rtem,        dumc,      ioprswX,                         
     1      ciopde,      ciopdes, 
     1      ciopso1,     ciopso1x,  ciopso2, ciopso2x, 
     1      ITYOPRX,     creuse,    cdivtypX, OprLossX,
     1      OprLimitX,   ioBegX,    ioEndX
        backspace(inX)

        write(nlog,300) 'Operating Rule File (*.opr)     '      
        if(cdivtypX.eq.' ') then
          nX=0
          write(nlog,340)
        else
          nX=0
          write(nlog,350)
        endif
         
        goto 500
c
c ---------------------------------------------------------
c		Process an error reading data         
 332    nX=0
c
c rrb 2008/10/06; Correction need to backspace if an error was 
c		  encountered (goto 332)
        backspace(inX)
        write(nlog,300) 'Operating Rule File (*.opr)     '      
        write(nlog,340)        
        goto 500
      endif
c
c _________________________________________________________
c
c rrb 2008/09/12; Revise to allow a blank file
  400 write(nlog,410) inX, filena
  410 format(/
     1 '  ChkVer; Warning End of File Encountered while checking',/
     1 '          the version number. This warning is OK if the',/
     1 '          file is supposed to have no data',/
     1 '          File # ', i4,/,
     1 '          File name: ', a256)
      goto 500      
c
c _________________________________________________________
c
c		Return     
 500  continue
      if(iout.eq.1) write(nlog,*) ' ChkVer_2; small ', small
      return     
      
      
c
c _________________________________________________________
c
c		Formats     
 200  format(i4, 1x, a12, 3f6.0, 2f8.0, f12.0, f3.0, f8.0)

 300  format(/,72('_'),/
     1   '  ChkVer; Warning two ',a32,/
     1   '            formats are currently supported but no file',/
     1   '            version has been provided.')
 310  format(    
     1   '          Based on the data in the file it appears',/
     1   '            you have provided version 1 that has',/
     1   '            2 land use types (Total GW and Total Sprinker).',/
     1   '          To eliminate this warning it is recommend you',/
     1   '            migrate to the current standard format or,',/
     1   '            at a minimum, include the following at the top',/
     1   '            of your file: #FileFormatVersion 1')

 320  format(    
     1   '          Based on the data in the file it appears',/
     1   '            you have provided version 2 that has',/
     1   '            4 land use types (SW-Flood, SW-Sprinkler,',/
     1   '            GW-Flood and GW-Sprinkler.',/
     1   '          To eliminate this warning it is recommend you',/
     1   '            include the following at the top of your file',/
     1   '            #FileFormatVersion 2')

 330  format(a12,a24,a12,f16.0,f8.0,i8, 3(1x,a12,f8.0), i8,
     1         1x,a12, 1x,a12, 1x, 2f8.0, 2i8)      
     
 340  format(    
     1   '          Based on the data in the file it appears',/
     1   '            you have provided version 1 that has no',/
     1   '            Plan, Diversion Type, Operating Loss, etc.',/
     1   '          To eliminate this warning it is recommend you',/
     1   '            migrate to the current standard format',/
     1   '            and include the following at the top of',/
     1   '            your file: #FileFormatVersion 2',/
     1   '          If you do migrate to the current standard format',/
     1   '            or add the file version to the top of the file',/
     1   '            StateMod will try to read the current standard',/
     1   '            and set missing data to a default value.') 

 350  format(    
     1   '          Based on the data in the file it appears',/
     1   '            you have provided version 2 that has',/
     1   '            Plan, Diversion Type, Operating Loss, etc.',/
     1   '          To eliminate this warning it is recommend you',/
     1   '            migrate to the current standard format',/
     1   '            and include the following at the top of',/
     1   '            your file: #FileFormatVersion 2',/
     1   '          If you do migrate to the current standard',/
     1   '            and add the above at the top of the file',/
     1   '            StateMod will try to read the current standard',/
     1   '            and set missing data to a default value.') 
     
 954  format(i4, 1x, a12, 3f6.0, 4f8.0, f12.0, f3.0, f8.0)

c _________________________________________________________
c     
c		Error Tracking

c
c _________________________________________________________
c     
 9999 write(6,*)  '  Stopped in ChkVer, see log file (*.log)'
      write(nlog,*) ' Stopped in ChkVer'
      write(6,*) 'Stop 1'
      call flush (6)
      call exit(1)
     
      stop
      end
