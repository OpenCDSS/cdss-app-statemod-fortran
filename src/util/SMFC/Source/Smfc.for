C     Last change:  RRB  11 Jan 2002    4:02 pm
c
C
C      Smfc.f 
c              Code to compare two statemod scenarios
c              It may compare one or all input files
c              It compares all if a response file (*.rsp) is provided
c              It compares 1 when any other file is provided
c               Typical execution:
c                 smfc -v   (print version number)
c                 smfc -h   (print help)
c                 smfc [fn] (fn=response file, if not provided
c                            defaults to smfc.rsp)
c
c                 ifile = 0 no input file found; >1 yes input file
c
c               1.03 Prints the difference number,
c                    Has a new printout for # of stations and
c                      differences
c                    Checks the dimension size for # of stations
c               1.05 Has a better help
c               2.02 Allows wells, sjrip, *.tsp, ...
c                    Daily was begun but testing not completed
c                    particularly with daily delay (*.dld)
c                    Also method to determine file availablity
c                    (e.g. *.tsp, *.wes) is currently hard wired
c               2.03 Handles a -1 file type in *.rsp (e.g. it reads
c                    the file name but does not open it)
c                    Revised skipy to work properly
c                    Revised maxsta from 1000 to 2500
c                    Revised to operate with ireach = 2
c                    Corrected initilization nin2=0 in compsta
c                    Revised parse to operate better with -v & -h
c                    Revised open(5 to include an error message
c               2.04 Revise parse to provide a default response file
c		            3.00 Revised to allow a random response file to be read
c               3.01 Revised to include relatively new file (*.pln, *.plr, etc
c               3.02 Added warning about *.wer file to *.log, *.sum & *.out
c
c               iprint = 0 print matches and  differences
c                        1 print differences only
c               iwell  = 0 no  wells (*.wes, *.wer, *.wem, *.weh)
c                        1 yes wells
c                       -1 in file but skip
c               ireach = 0-1 no  monthly (*.ifm)
c                        >=2 yes monthly (*.ifm)
c               isjrip = 0 no  San Juan sediment file (*.sjr)
c                        1 yes San Juan sediment file
c               itsfile= 0 no  time series file (*.tsp)
c                        1 yes time series file
c               ieffmax= 0 no  IWR file (*.iwr)
c                        1 yes IWR file
c               isoil  = 0 no  soil moisture (*.par)
c                        1 yes soil moisture
c               iday   = 0 no daily files (*.--d)
c                        1 yes daily files
c
c
c !!!   See subroutine parse, getpath, and putpath for
c       Conversion to workstation
c
c _________________________________________________________
c rrb 2007/12/18; Variables for Random Response File
       dimension
     1   ifileNum(2,80), fileName(2,80), fileType(2,80),
     1   filetypX(80),   fileID(80), ifileNx(80)

       character filena*256,
     1   filetype*40, filetypX*40, FileName*256, FileId*5,
     1   fileT1*40,   fileT2*40, fileN1*256,
     1   filrsp*72
     
     
c
c _________________________________________________________
c		Original Variables
	character filenc*72,  filout*72, fpath(2)*72, 
     1            ext(50)*4,  desc(50)*24, 
     1            vdate*10,    fext*4,    fext1*1                               
	data ext/                                                       
     1    '.rsp', '.ctl', '.rin', '.res', '.dds',
     1    '.ris', '.ifs', '.wes', '.ifr', '.rer',
     1    '.ddr', '.opr', '.wer', '.pre', '.eva',
     1    '.rim', '.ddm', '.dum', '.dda', '.ifm',
     1    '.ifa', '.wem', '.dly', '.tar', '.sjr',
     1    '.tsp', '.iwr', '.par', '.eom', '.rib',
     1    '.rih', '.ddh', '.weh', '.gis', '.out',
     1    '.rid', '.ddd', '.ifd', '.wed', '.tad',
     1    '.dld', '.iwd', '.riy', '.ddy', '.wey',
     1    '.eoy', '.x  ', '    ', '    ', '    '/
									
	data desc/                                                      
     1    'Response File           ', 'Control File            ',       
     1    'River network file      ', 'Reservoir Station       ',       
     1    'Direct Diversion Station', 'River Station           ',       
     1    'Instream Flow Station   ', 'Well Station            ',
     1    'Instream Flow Rights    ',
									
     1    'Reservoir Rights        ', 'Direct Diversion Rights ',       
     1    'Operational Rights      ', 'Well Right              ',
     1    'Precipitation           ',
     1    'Evaporation             ', 'Streamflow              ',
     1    'Direct Diversion Dem Mo ', 'Diversion Override      ',
									
     1    'Direct Diversion Dem An ', 'Instream Flow Demand Mo ',
     1    'Instream Flow Demand An ', 'Well Demand Mo          ',
     1    'Delay                   ', 'Reservoir Target        ',
     1    'San Juan Sediment       ', 'Time Series File An     ',
     1    'Consumptive Water Req Mo', 'Soil Moisture An        ',
     1    'Reservoir End-of-Month  ', 'River Baseflow          ',       
     1    'Historic Streamflows    ', 'Historic Diversions     ',
     1    'Historic Well Pumping   ',
     1    'GIS                     ', 'Ouput Control           ',
     1    'Daily Streamflow        ', 'Daily Direct Diver Dem  ',
     1    'Daily Instream Flow Dem ', 'Daily Well Demand       ',
     1    'Daily Reservoir Target  ', 'Daily Delay Table       ',
     1    'Daily IWR               ', 'Daily Historic Stream   ',
     1    'Daily Historic Diversion', 'Daily Historic Pumping  ',
     1    'Daily Res EOY           ',
     1    'Output File             ', '                        ',
     1    '                        ', '                        '/       
c                  
c _________________________________________________________
c               Initilize
       ver = 3.03
       vdate = '2012/03/09'
       maxsta=10000
c      maxsta=2500
       ifile=0
       iderr=0
       maxnf=2
       maxin=50

       iwell=0
       ireach=0
       isjrip=0
       itsfile=0
       ieffmax=0
       isoil=0
       iday=0
       nlog=99
       nsum=90
       filenc=' '
       
       filrsp='smfc.rsp'
c                                                                       
c _________________________________________________________
c               Open files                                              
       open(nlog,file='smfc.log', status='unknown')
       open(90,file='smfc.sum', status='unknown')
c      open(98,file='smfc.dim', status='unknown')
c      open(7, file='smfc.xgr', status='unknown')
c
c

c
c _________________________________________________________
c               Get control (*.rsp) file
       call parse(nlog, filenc)
c
c _________________________________________________________
c               Check for simple -version options
       if(filenc(1:2).eq. '-v') then
	       write(6,120) ver, vdate
         write(nlog,120) ver, vdate   
	       call flush(6)
	       ifile=1
	       goto 110
       endif
c
c _________________________________________________________
c               Check for simple -help option
       if(filenc(1:2).eq. '-h') then
	       write(6,130)   (ext(i), desc(i), i=1,maxin)
	       write(nlog,130)   (ext(i), desc(i), i=1,maxin)
	       ifile=1
	       call flush(6)
	       goto 110
       endif
c
c _________________________________________________________
c               Print version to log file & Screen
       write(nlog,120) ver, vdate   
       write(6,120) ver, vdate   
c
c rrb 2012/02/10; Print warning about *.wer file
       write(nlog,122)

c
c rrb 2012/02/10; Print warning about daily files       
       write(nlog,*) ' Note daily capability implemented but not tested'       
c
       write(90,120) ver, vdate    
       write(90,122)
       write(90,230) 
 230   format(/, 
     1 '              File 1    File 2          ',/
     1 '    # Type  Stations  Stations Different',/
     1 ' ____ ____ _________ _________ _________')      
       
c
c _________________________________________________________
c               Open and read control file
       open(5, file=filenc, status='old',err=160)                               
       call skipn(5,0)
       READ(5,*,END=140,ERR=140) iprint
c      write(nlog,*) '  SmFc; iprint ', iprint
c
c rrb 2007/12/20; Remove old control file capability.
c		  Keep in code for the time being       
c      BACKSPACE(5)
c      read(5,*,end=90,err=90) iprint, iwell, ireach, isjrip, itsfile,
c    1                         ieffmax, isoil,  iday
c      write(nlog,*) iprint, iwell, ireach, isjrip, itsfile,
c    1                         ieffmax, isoil,  iday
       iwell=0
       ireach=0
       isjrip=0
       itsfile=0
       ieffmax=0
       isoil=0
       iday=0
c
c
 90    continue
c
c rrb 2007/12/20; Remove old control file capability  
c      write(nlog,*) '  Main; iprint  = ', iprint      
c      write(nlog,*) '  Main; iwell   = ', iwell
c      write(nlog,*) '  Main; ireach  = ', ireach
c      write(nlog,*) '  Main; isjrip  = ', isjrip
c      write(nlog,*) '  Main; itsfile = ', itsfile
c      write(nlog,*) '  Main; ieffmax = ', ieffmax
c      write(nlog,*) '  Main; isoil   = ', isoil
c      write(nlog,*) '  Main; iday    = ', iday
c
c _________________________________________________________
c               Get comparison type (e.g. *.rsp, *.ctl, etc)
       call skipn(5,0)  
       read(5,'(a4)',end=140,err=140) fext  
       write(nlog,121) fext

c
c               Define iall = 0 for 1 file or 1 for all
       iall=0
       if(fext.eq.'.rsp') iall=1
c
c _________________________________________________________
c               Read output file name
       call skipn(5,0)
       read(5,'(a72)',end=140,err=140) filout
       write(nlog,123) filout

       open(26,file=filout,status='unknown')
c
c               Print version to *.log and *.out file
cx     write(nlog,120) ver, vdate   
       write(26,120) ver, vdate   
       write(26,122)       

c
c rrb 2012/02/10; Print warning about daily files       
       write(nlog,*) ' Note daily capability implemented but not tested'
       ix = 0                                                           

       do 100 i=1,72
	       if(filout(i:i).ne.' ') ix=ix+1                                 
  100  continue     
c
c _________________________________________________________
c rrb 2007/12/18; Process new response file type
       call smfcNew(nlog, maxnf, iprint, maxsta, 
     1   iday, infile, ifile, fext, ext, desc, fpath)  
     
c
c _________________________________________________________
c rrb 2007/12/18; Process old response file type
       write(nlog,*) ' SmFc ', infile
       if(infile.eq.0) then
         call smfcOld(nlog, maxnf, iprint, maxsta, ifile, 
     1   iwell, ireach, isjrip, itsfile,
     1   ieffmax, isoil,  iday, fext, ext, desc, fpath)  
       endif
     
c
c _________________________________________________________
c               Print global message regarding duplicate id's

  110   write(6,*) ' '

	     if(ifile.eq.0) then
	       write(6,111) fext
	       write(nlog,111) fext
  111    format(/,
     1      '  !!! Problem file type to compare ', a4,' not found.',/
     1      '      See *.log for supported types')
	       write(nlog,130) (ext(i), desc(i), i=1,maxin)
	     else
	       if(iderr.eq.0) then
	         write(6,200) 
           write(nlog,202)	 
           write(nlog,200)        
	       else
	         write(6,210)
	       endif
	     endif
 
	     call flush(6)
	     stop 0
c
c _________________________________________________________
c               Formats
  120   format(/,
     1 ' _______________________________________________________'//
     1 '        Smfc                       '/
     1 '        State of Colorado - Statemod Input File Comparison'//
     1 '        Version: ',f5.2,/,
     1 '        Last revision date: ',a10,//
     1 ' _______________________________________________________')
 
  121   format(/,
     1 ' _______________________________________________________'//
     1 '  File to compare = ',a5, ' where *.rsp compares all') 
          
  122   format(/,
     1 '  Note: Some files are not ID specific (e.g. a well',/
     1 '        right may be assigned to several structures)',/ 
     1 '        Therefore the file comparison results are not valid',/
     1 '        This issue occurs for the following two files:',/
     1 '        well right (*.wer) and Plan Wells (*.plw).' /
     1 '        Also a StateCU structure (*.str) file and a ',/
     1 '        GIS (*.gis) file cannot be compared by ID with',/
     1 '        their current file structure')

     
  123   format(/,
     1 ' _______________________________________________________'//
     1 '  Output File Name ',a72)
c    
  130   format(/,
     1 ' _______________________________________________________'//
     1 '        Smfc                       '/
     1 '        State of Colorado - Statemod Input File Comparison'//
     1 ' _______________________________________________________',//
     1 ' Available comparsion file types:',/
     1   (1x, a4, 1x, a24))

c
c _________________________________________________________
c               Warning messages
  140   write(nlog,*) '  Problem reading output file in main'
        goto 150

  160   write(nlog,*) '  Problem opening the response file: ', filenc
        goto 150
  150   write(6,*)  '  Unsuccessful termination, see smfc.log'
	      write(nlog,*) '  Unsuccessful termination, see smfc.log'
c
c _________________________________________________________
c		Formats	
 200    format(/,
     1      60('_'),/ 
     1	    '  Successful termination',//)

 202    format(/,
     1      60('_'),//,
     1      '  Note: if a matching station is not found',/
     1      '        the number of differences = 1, even if',/
     1      '        a station has 10 lines of input',//
     1	    '  Output is in smfc.log, smfc.out and *.sum')
     
 210      format(/,
     1      60('_'),//,  
     1	    '  Successful termination',//      
     1      '  Note differences in some input files'
     1      '  Output is in smfc.out, smfc.sum and smfc.log')
c
c _________________________________________________________
 1110 write(6,*)  '  Opener; Unsuccessful termination, see smfc.log'
      write(nlog,*)'  Opener; Unsuccessful termination, see smfc.log'
	    goto 500
     
 1120 write(6,*) ' '
	    write(6,*) '  SmFc: Problem opening file ', filrsp
	    write(nlog,*) ' '
	    write(nlog,*) '  SmFc: Problem opening file ', filrsp
	    goto 500

     
c
c _________________________________________________________
 500  call flush(6)
	    stop 1
	    end                                                             
c                                                                       
c ************************************************************          
	     subroutine getpath(nlog, filrsp,fpath1)
c
c               It finds the path for an input file
      	character filrsp*72, fpath1*72, x*1

        iout=0
c     
c !!!           PC Specific                
	      x='\'
c     
c !!!           SGI Specific                
c       x='/'
      	fpath1=' '                               
      	do 110 i=1,72
      	  ii=73-i
      	  if(filrsp(ii:ii).eq.x) then
      	    do 100 j=1,ii
      	      fpath1(j:j) = filrsp(j:j)
  100       continue
	          goto 120
	        endif
  110   continue              

  120   if(iout.gt.0) then
          write(nlog,130) filrsp, fpath1
        endif
  130   format('  Getpath results; ',/,
     1         '           filrsp  ',a72,/,
     1         '           fpath   ',a72)
	      return    
	      end
c
c ************************************************************          
	      subroutine putpath(nlog, filrsp,fpath1)
c
c               It adds a path for an iput file if none provided
	      character filrsp*72, fpath1*72, filrsp1*72, x*1
c
c     
c !!!           PC Specific                
	      x='\'
c     
c !!!           SGI Specific                
c       x='/'
	     icheck=0                               
	     filrsp1=filrsp
c
c               Check to see if a path was provided
	     do 100 i=1,72
	       ii=73-i
	       if(filrsp(ii:ii).eq.x) goto 130
  100  continue              
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
  110       continue
	        endif
  120   continue
	      goto 140

  130   if(icheck.eq.1) write(nlog,150)  filrsp1, fpath1, filrsp
	      return      
		 
  140   write(6,*) '  Problem with putpath, see *.log'
	      write(nlog,150)  filrsp1, fpath1, filrsp
  150   format('  Putpath results; ',/,
     1         '           filrsp1 ',a72,/,
     1         '           fpath1  ',a72,/
     1         '           filrsp  ',a72)
	end
c                                                                       
c ************************************************************          
c
        subroutine parse(nlog, filenc)
        character command*127, filenc*72

        iout=1
        maxcl=127
        command = ' '
        call getcl(command)

        filenc=' '
        do i=1,maxcl
          if(command(i:i).ne.' ') then
            filenc(i:i) = command(i:i)
c           ii = i
          else
            goto 150
          endif
        end do
c
c               Set default
 150    if(filenc(1:1).eq.' ') filenc = 'smfc.rsp'
        if(iout.eq.1) then
          write(nlog,100) filenc
        endif
c
c _________________________________________________________
c               Formats
 100    format('  Parse; filenc = ', a72)
        return
	end                                                             
c                            
c ************************************************************
c
c               skipn it skips any number of comment cards identified
c               as a '#' in column 1 of a file
c
      	subroutine skipn(nf,io)
      	character rec1*144
c                 
c              Check first record and store for use on this file
  100   read(nf,'(a144)',end=110,err=110) rec1
c
	     if(rec1(1:1).eq.'#') then  
	       if(io.ne.0) write(io,'(a144)') rec1
	       goto 100
	     else
	       backspace(nf)
	     endif        
  
  110   return
	     end
c                            
c ************************************************************
c
c               skipx it skips x lines from an input file
c
	     subroutine skipx(nf,n)
	     character rec1*1
c                      
c                   Check first record and store for use on this file
	     do 100 i=1,n
  100    read(nf,'(a1)',end=110) rec1
	     rec1 = rec1
  110  return
	     end
c ************************************************************
c
c               skipy it skips up to 100 lines from an output file
c               until the string Year is found
c
	      subroutine skipy(nf,n)
        character rec4*4, rec5*5, rec100*100
c                 
c              Check first record and store for use on this file
        nrec=100
cx       do i=1,n
cx         read(nf,'(a4)',end=110) rec4 
cx         read(nf,'(a5)',end=110) rec5 
cx         if(rec4.eq. 'Year') goto 110 
cx         if(rec5.eq.' Year') goto 110 
cx       end do

        do i=1,n
          read(nf,'(a100)',end=110) rec100
          do j=1,nrec-3
            j1=j
            j2=j+3
            rec4=rec100(j1:j2)
            if(rec4.eq. 'Year') goto 110
          end do
	      end do


  110   return
	      end



