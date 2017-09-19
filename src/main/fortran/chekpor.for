c
       subroutine chekpor(
     1   ifinr, ifinx, nlog, nann, ioptiox, nx, iystr,
     1   imstr, idayx, ityp, c, cyr1, maxfn,
     1   infile, idummy, nfmt, fpath1, filena)
c
c
c _________________________________________________________
c	Program Description
c
c       Chekpor; it:
c       1a)If infile=0 it reads an input file from a response 
c          file (ifinr)
c       1b If infile=1 it opens an input file named
c          filena
c       2) opens the input file to number ifinx
c       3) skips header cards (see skipn)
c       4) checks for proper year type (see chekts)
c       5) finds the proper year of data for simulation
c
c _________________________________________________________
c
c       Update History
c
c rrb 03/08/18; Revised to allow a random file read
c rrb 99/08/20; Revised to handle daily data (monthly checks)     
c rrb 99/09/22; Revised to handle dummy files better
c rrb 00/08/04; Added maxfn and revised file length from 72 to 256
c
c
c _________________________________________________________
c	Documentation
c
c
c    ifinr  = file number for the response file
c    ifinx  = file number for the file to be opened 
c               and period of record for time series checked
c    filena = name of file to open
c    nann   = data type:
c             0=monthly by year,
c             1=constant for all years
c            -1=daily
c    nlog  = file number for the detailed output
c    ioptiox= run option (1=baseflow, 2=check , 3=simulate, 4=report, 
c               etc.  Note: Virgen and some of mdainp.f pass 
c               0 in order to read and open a file
c               1 read file name then exit (allows response
c               file to be static)
c    nx     = number of elements to be read (may enter 0 for none or
c             any positive number if unknown)
c    iystr  = year to begin simulation
c    imstr  = month to begin simulation (only used for daily model)
c    idayx  = daily model switch 
c           = 0 for monthly data
c           = 1 for standard daily data
c           = 2 for Downstream call format (year is 2 digets)
c    ityp   = file type (see chekts)
c    c      = coefficient for units (see chekts)
c    idummy = 0 File was opened OK
c	            1 Dummy file determined (no file opened)
c    cyr1   = year type (see chekts)
c
c
c
c _________________________________________________________
c	Dimensions
c                                              
      character filena*256, fpath1*256, cyr1*5, rec3*3
c
c		iout=1 details
c		iout=2 summary      
      iout=0
      iin2=ifinr
      idummy=1
c
c _________________________________________________________
c		Step 1; If infile=0 read file
      if(infile.eq.0) then
        filena='*.rsp'
        READ(iin2,'(a256)',end=926,err=930) filena
      else
        if(iout.ge.1) write(nlog,*) '  Chekpor; fileName = ', filena
      endif
c
c _________________________________________________________
c		Step 2; Add path
c
      call putpath(maxfn, filena, fpath1)
      write(nlog,'(5x, a256)') filena

      if(iout.ge.1) then
        write(nlog,*) ' Chekpor; ifinx, filena', ifinx, filena  
        write(nlog,*) ' Chekpor; ioptiox, nx', ioptiox, nx
      endif  
c
c _________________________________________________________
c		Step 3; Return (500) if Natural Flow or no stations
      if(ioptiox.eq.1 .or. nx.eq.0) goto 500
c
c rrb 2006/09/25; Moved from above
      if(nx.eq.0) then
        write(nlog,114) filena, nx
        goto 9999
      endif
c 
c
c _________________________________________________________
c		Step 4; Open file

      if(iout.ge.1) write(nlog,*) ' Chekpor; ifnix, filena ', 
     1  ifinx, filena
      open(ifinx, file=filena,status='old',err=110)  
c
c _________________________________________________________
c		Step 5; Get Version Number and skip header cards
      iin2=ifinx
c     call skipn(ifinx)
      call GetVer(nlog, ifinx, nfmt, filena)
c
c
c _________________________________________________________
c		Step 6; Read year type control data
      call chekts(nlog, ifinx, ityp, c, idummy, cyr1)
c
c _________________________________________________________
c		Step 7; Return if constant data or a dummy file
      if(iout.eq.1)
     1 write(nlog,*) '  Chekpor; nann, idummy', nann, idummy


      if(nann.eq.1 .or. idummy.eq.1) goto 500      
c
c _________________________________________________________
c		Step 8; Monthly data (standard idayx=0)
c               Parse thru data to get time of interest
      if(idayx.eq.0) then
  100   read (ifinx,*,end=110,err=928) ityr
        if(iout.eq.1) write(nlog,*) ' Chekpor, ityr, iystr', ityr, iystr
c
        if(ityr.eq.0) goto 110
        if((ityr-iystr).lt.0) goto 100
        if((ityr-iystr).gt.0) goto 120

        backspace(ifinx)
        goto 500
      endif
c
c _________________________________________________________
c
c               Step 9; Daily data (standard)
c               Parse thru data to get time of interest
      if(idayx.eq.1) then
  200   read (ifinx,*,end=110,err=928) ityr, itmo
        if(iout.eq.1) write(nlog,*) ' Chekpor, ityr, itmo', ityr, itmo
  
c
c
        if(ityr.eq.0) goto 110
        if((ityr-iystr).lt.0) goto 200
        if((ityr-iystr).gt.0) goto 220

        if((itmo-imstr).lt.0) goto 200
        if((itmo-imstr).gt.0) goto 220

        backspace(ifinx)
        goto 500
      endif
c
c _________________________________________________________
c
c               Step 10; Downstream Call daily format
c               Parse thru data to get time of interest
      if(idayx.eq.2) then
 300    read (ifinx,*,end=928,err=928) ityr, itmo, icd1, dcall1
        if(iout.eq.1) write(nlog,*) 
     1   ' Chekpor, ityr, itmo, icd1, dcall1',ityr, itmo, icd1, dcall1
        
c
        if(ityr.eq.0) goto 110
        if((ityr-iystr).lt.0) goto 300
        if((ityr-iystr).gt.0) goto 320

        if((itmo-imstr).lt.0) goto 300
        if((itmo-imstr).gt.0) goto 320

        backspace(ifinx)
        goto 500
      endif

c
c _________________________________________________________
c		Step 11; Return
  500 return 
c
c _________________________________________________________
c
c               Warning messages
c 
  110 write(nlog,112) filena
c
c rrb 01/02/10; Close dummy file
      close(ifinx)

  112 format(
     1  '  Chekpor; FYI, A dummy (blank) file determined for ',/
     1  '           file: ', a256)

  114 format(
     1  '  Chekpor; Problem',/
     1  '           The following file is provided in the response ',
     1              'file = ', a256,/
     1  '           But the number of stations in the control file ', 
     1              '     = ', i5,/
     1  '           Therefore no data is read',/
     1  '           Reconmend you correct the control file or the ',
     1              'response file')
c
c rrb 00/04/13; Correction
      return
c _________________________________________________________
c
c               Error messages
  120 write(nlog,130) filena, iystr, ityr
  130 format(
     1  '  Chekpor; Problem with standard monthly file:',/,
     1  '          ', a256,/
     1  '           The simulation start year ', i5,/
     1  '           is outside the period of record provided  ', i5)
     
      goto 9999
      
  220 write(nlog,230) filena, iystr, imstr, ityr, itmo
  230 format(
     1  '  Chekpor; Problem with standard daily file: ',/,
     1  '          ', a256,/
     1  '           The simulation start year & month        ', 2(i5),/
     1  '           is outside the period of record provided ', 2(i5))
      goto 9999
     
  320 write(nlog,330) filena, iystr, imstr, ityr, itmo
  330 format(
     1  '  Chekpor; Problem with downstream call daily file: ',/,
     1  '          ', a256,/
     1  '           The simulation start year & month        ', 2(i5),/
     1  '           is outside the period of record provided ', 2(i5))

      goto 9999

c
c _________________________________________________________
c
c rrb 97/11/02; Error Handling
  926 write(99,927) iin2, filena
  927 format(' Chekpor.f; End of file # ', i4, ' encountered',/,
     1       '   File name: ', a256)
      goto 9999
c
  928 write(99,929) ifinX, filena
  929 format(' Chekpor.f; Problem reading file # ', i4,/,
     1       '   File name: ', a256,/
     1       '   Problem record (next line):')

      backspace(ifinX)
      read(iin2, '(a256)',end=926,err=926) recin
      write(99,'(a256)') recin
      goto 9999
c
  930 write(99,931) iin2, filena
  931 format(' Chekpor.f; Problem reading response file # ', i4,/,
     1       '   File name: ', a256)
      goto 9999
      

      
 9999 write(6,*)  '  Stopped in Chekpor, see log file (*.log)'
      write(99,*) '  Stopped in Chekpor'
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop 
      end


