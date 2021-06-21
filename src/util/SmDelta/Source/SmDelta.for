c
c     Last change:  RRB   8 Jan 2002   10:22 am
c
       Program SmDelta
C              SmDelta.f version 3.04 and greater 
c              Code to generate a plot file from one or more
c              Statemod output files
c
c _________________________________________________________
c              Updates
c rrb 99/12/02 Version 1.00
c              Combines SmDelta and SmDeltaw by determining the StateMod
c              version
c rrb 99/12/28 Version 1.01
c              Revised to allow merge to work when mixed data (*.xdd  &
c              *.b43) are provided.
c              Also revised to allow instream flows to be ignored
c rrb 99/12/30;Version 1.02
c              Revised to recognize new data type (StreamID based on
c              a "0..." ID) Vs Streamgage that prints all baseflow
c              points
c
c rrb 02/01/03;Version 1.03 02/01/03
c              Revised to work with StateMod version 10.x and greater
c              Changed # of columns (31=33) and size of *.b43 (124=140)
c              Cleaned up some I/O reporting
c              Added ability to read a well file (*.xwe and *.b42)
c
c rrb 02/07/17; Added version capability
c
c rrb 02/04/26;Version 4.04 02/04/26
c              Revised to work better with StateMod version 9.58 and
c              9.96
c
c rrb 02/07/17;Version 4.05
c              Revised to allow SmDelta -v to print version only then
c              exit
c
c rrb 02/07/17;Version 4.06
c              Redimensioned to handle up to 10 files
c
c rrb 02/07/22; Version 4.07
c              Revised to stop if problems occur reading input files
c               (see bintop.for)
c               Revised to operate using -v if file SmDelta.rsp does
c               not exist
c rrb 02/89/06 Version 4.08
c              Revised to recognize 'Stream' in addition
c               to 'StreagGage'
c              Revised to remove ifounds checks in datstr to resolve
c               a problem of missing some stream gages located in *.xdd
c rrb 02/08/08 Version 4.09
c              Revised to flush file 6 when stop 1 is printed.
c rrb 02/08/09 Version 4.10
c              Revised to print a title if provided
c rrb 06/08/08 Version 4.11
c              Revised to read StateMod Version 11.x output with Loss
c rrb 06/11/30 Version 4.11
c              Continued to revised to read StateMod Version 11.x output with Loss
c	       Revised to read *.xop
c	       Revised to read *.xwb		
c rrb 07/03/17; Version 4.12
c              Revised *.xwb to read both version 10 and version 11 output
c              Major features:      
c               0) Reads a binary or ascii file.
c                  Binary is assumed if the name is *.b*
c               1) Determines if the output file is with or w/o wells
c                  For an ASCII file it reads version number
c                  For a binary file it tries to read the one with wells
c                  wells. if there is an error it opens the one w/o
c                  wells
c               2) Single, Multiple, Diffrence or Diffx file results
c                  Single option will use first file
c                  Multiple will use up to 10 files
c                  Difference will difference first two files and
c                     allows id's found in one file not in another
c                     It prints the difference only
c                  Diff2 will difference first two files and
c                     allows id's found in one file not in another
c                     It prints both pieces of data plus the difference
c                  Diffx will difference first two files and
c                     skips id's found in one file not in another
c               3) Operates on diversions, streamflows or reservoirs
c               4) Provides data for one of 30 parameters 
c                  in each file
c               5) Prints 1, n, or all id's
c               6) Prints a specific year or average 
c
c rrb 07/03/17; Version 4.13
c              Revised DatXop and Getin to read average and correct
c                a problem in previous versions requesting the average
c                when processing *.xop
c
c _________________________________________________________
c               Constraint:
c               When id's are provided the code checks
c               for a -999 as an indicator that no more id's will be
c               provided.
c
c _________________________________________________________
c              Major control variables
c
c               ir      run type; 0=single 1=multiple  2=difference,
c                       3=merge, 4=diffx, 5=diff2
c               ftype   file type diversion, streamgage, reservoir,
c                       streamID
c               ptype   paramter type (see getpar)
c               inver   code to print version only 0=no 1=yes
c               ifx     file counter
c               iyx     year code; 
c                       0=average annual total, 
c                       1=specified year total,
c                       2=specified year and month
c               idreq   id code;   0=all, n=id requested
c               iid     number of id's to match from getin.f
c                       (0 used when all are requested)
c               iend    end of file code from subroutine getin
c               is      number of id's found from diver.f or res.f
c               isx(ifx) number of id's found from file ifx
c               idallx  ID switch 0 = get all stations
c                                 1 = get selected stations
c               idall   # of ID's to get in certain subtoutines
c
c               iwell   1 diversion file w/o wells
c                       2 diversion file with wells
c               iwellr  same as above but for reservoir files
c               iwellw  same as above but for well files
c
c               ibin    0 binary file
c                       1 ascii file
c
c               maxsta  = maximum number of stations
c               maxin   = maximum number of files
c
        include 'SmDelta.inc'
        character verdat*10
        ver=4.13
        verdat= '2009/12/29'
        filenc=' '
        fillog=' '
        filout=' '
        ctitle=' '
        maxin=10
        iout=1
c
c _________________________________________________________
c               Initilize
c
c rrb 2019/08/11; Revise max stations for S Platte
cx      maxsta=2018
        maxsta=5001

        do 100 i=1,maxin
        isx(i) = 0
        do 100 j=1,maxsta
          idz(i,j) = '            '
c
c rrb 2019/08/11; Initilize operating rule type
          ctypeX(i,j) = '-1'
  100     dels(i,j) = 0.0
c
c _________________________________________________________
c
c               Parse control arguements
       call parse(filenc)
c
c _________________________________________________________
c
c rrb 02/07/16; Allow just a version to be printed
       inver=0
       if(filenc(1:2).eq.'-v' .or. filenc(1:2).eq.'-V') then
         filenc='SmDelta.rsp'
         ftype(1)=' '
         rtype='Version'
         inver=1
         iend=0
c        write(6,*) ' Version entered ', filenc, ftype(1)
       endif
c
c _________________________________________________________
c
c rrb 02/07/16; Allow just a help to be printed
       if(filenc(1:2).eq.'-h' .or. filenc(1:2).eq.'-H') then
         filenc='SmDelta.rsp'
         ftype(1)=' '
         rtype='Help'
         inver=1
         iend=0
       endif
c
c _________________________________________________________
c
c
c               Open input files
       if(inver.eq.0) then
         call getpath(filenc, fpath1)
         open(5, file=filenc, status='old',err=140)
       endif

c               Open log and output files
       call namext(filenc, 'log', fillog)
       open(99,file=fillog, status='unknown')
       nlog=99
       
       if(iout.eq.1) write(nlog,*) '  Input file opened = ', filenc
c
c               Open graph output file
       call namext(filenc, 'xgr', filout)
       open(7, file=filout, status='unknown')
c
c _________________________________________________________
c
c               Print version to *.log and screen
       write(6,118) ver,verdat
       write(nlog,118) ver,verdat

c _________________________________________________________
c
c rrb 99/05/17;
       write(nlog,*) ' '
       write(nlog,101) filenc
       write(nlog,102) fillog
       write(nlog,102) filout
  101  format(' Main; Opened response file: ', a72)
  102  format(' Main; Opened output file:   ', a72)  
c
c               Get input and open data files            
       ifx = 0            
       ierr = 1
       ichk = 0
c
c _________________________________________________________
c		Read Control Data and get Version number
c
c rrb 02/07/17; Allow -v to work
  110  if(inver.eq.0) call getin(iend)
       if(iend.eq.1) goto 120
       ifx = ifx+1
c
c _________________________________________________________
c               Process a diversion        
       if(ftype(ifx).eq.'Diversion') then
         idiv=0
c
c        write(nlog,*) '   SmDelta; calling getpar'
         call getpar(0, ip, iwelld, ptype(ifx),fillog)
c
c        write(nlog,*) '   SmDelta; calling outlog'
         call outlog
         if(ibin.eq.0) then
c
c          write(nlog,*) '   SmDelta; calling bintop'
           call bintop(43)
c
c          write(nlog,*) '   SmDelta; calling datbin'
           call datbin
         else
c
c          write(nlog,*) '   SmDelta; calling datdiv'
           call datdiv
           isx(ifx) = is
         endif
         ierr = 0
       endif
c
c _________________________________________________________
c
c               Process a StremGage
c
       if(ftype(ifx).eq.'StreamGage' .or.
     1    ftype(ifx).eq.'Stream    ') then 
         idiv=1
         call getpar(0, ip, iwelld, ptype(ifx),fillog)
         call outlog
         if(ibin.eq.0) then
           call bintop(43)
           call datbin
         else
           call datstr
           isx(ifx) = is
         endif
         ierr = 0
       endif

c
c _________________________________________________________
c
c               Process a Reservoir
       if(ftype(ifx).eq.'Reservoir') then
         idiv=2
         call getpar(1, ip, iwellr, ptype(ifx),fillog)
         call outlog      
         if(ibin.eq.0) then
           call bintop(44)
           call datbinr
         else
           call datres
           isx(ifx) = is
         endif
         ierr = 0
       endif            
c
c _________________________________________________________
c               Process an Instream Flow Stream ID
c rrb 99/12/29; StreamID
       if(ftype(ifx).eq.'Instream ') then
         idiv=3
         call getpar(0, ip, iwelld, ptype(ifx),fillog)
         call outlog

         if(ibin.eq.0) then
           call bintop(43)
           call datbin
         else
           call datdiv
           isx(ifx) = is
         endif
         ierr = 0
       endif
c
c _________________________________________________________
c               Process a Well
       if(ftype(ifx).eq.'Well     ') then
         idiv=4
         call getpar(4, ip, iwellw, ptype(ifx),fillog)
         call outlog

c        write(6,*) '  Well option not complete'
c        goto 180
         if(ibin.eq.0) then
           call bintop(42)
           call datbinw
         else
c          write(6,*) '  Well option for ASCII file not complete'
c          goto 180
           call datwel
           isx(ifx) = is
         endif
         ierr = 0
       endif
c
c _________________________________________________________
c               Process a StremID ("0....
c rrb 99/12/29; StreamID
       if(ftype(ifx).eq.'StreamID') then
         idiv=5
         call getpar(0, ip, iwelld, ptype(ifx),fillog)
         call outlog
         if(ibin.eq.0) then
           call bintop(43)
           call datbin
         else
           call datstr
           isx(ifx) = is
         endif
         ierr = 0
       endif
c
c _________________________________________________________
c               Process an Operational Right Summary
c rrb 99/12/29; StreamID
       if(ftype(ifx).eq.'Operational_Right       ') then
         idiv=6
         call getpar(6, ip, iwelld, ptype(ifx),fillog)
         call outlog
         call datXop
         isx(ifx) = is
         ierr = 0
       endif
c
c _________________________________________________________
c               Process a Water Budget Summary
c rrb 99/12/29; StreamID
       if(ftype(ifx).eq.'Water_Budget            ') then
         idiv=7
         call getpar(7, ip, iwelld, ptype(ifx),fillog)
         call outlog
         call datXwb
         write(nlog,*) ' Smdelta; is = ', is
         isx(ifx) = is
         ierr = 0
       endif
c
c _________________________________________________________
c               Print available parameters
       if(rtype.eq.'Help' .or. rtype.eq.'help') then
         call getpar(nlog, ip, iwelld, ptype(ifx),fillog)
         ierr = 0
         goto 130
       endif
c
c _________________________________________________________
c               Print version
       if(rtype.eq.'Version') then
c
c rrb 02/07/17; Print version every time (above)
cx       write(6,118)  ver,verdat
cx       write(nlog,118) ver,verdat

  118   format(
     1 ' _______________________________________________________'//
     1 '        SmDelta                       '/
     1 '        State of Colorado - Statemod Delta Postprocessor'//
     1 '        Version: ',f5.2,/,
     1 '        Last revision date: ',a10,//
     1 ' _______________________________________________________'/)
         ierr = 0
         goto 130
       endif
c
c _________________________________________________________
c               Print error
       if(ierr.eq.1) goto 160
c
c _________________________________________________________
c               Get second file
       if(ir.ge.1) then
         close(1)
         close(43)
         close(44)
         goto 110             
       endif
c
c _________________________________________________________
c               Print Results
  120  call outdat
c
c _________________________________________________________
c               End printout
  130  write(6,*) '   Successful termination'
       write(6,*) '   Output log in: ', fillog
       write(6,*) '   Output plot in: ', filout
       write(6,*) 'Stop 0'
       call flush(6)
       stop                                
c
c _________________________________________________________
c               Error messages
  140  write(nlog,150) filenc 
  150  format('   Main; Cannot open file ',a72,/
     1        '         Check name, path, etc.')
       goto 180      

  160  write(nlog,170) ftype(ifx)
  170  format('   Main; Bad data type = ', a24)
       goto 180

  180  write(6,190)  fillog
       write(nlog,190) fillog
  190  format('  Main - Unsuccessful termination, see ', a72)
       write(6,*) 'Stop 1'
       call flush(6)
       stop
       end

