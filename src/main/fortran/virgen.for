c
c _________________________________________________________
C *********************************************************
c
      SUBROUTINE VIRGEN
c
c      Virgen; It controls all virgin (base) flow calculations.
c	Called by StateM.f
c
c
c _________________________________________________________
c
c       Update History
c
c rrb 01/01/02; Added option 10 (baseflows with variable efficiency) &
c               option 11 (baseflow with variable efficiency and
c               wells with sprinklers get used first)

c
c _________________________________________________________
c
c       Documentation
C
C      FILES AND REQUIRED SORT PRIORITIES:
C
C      file 1  - INPUT CONTROL PARAMETER FILE,     close(1)
C      file 2  - INPUT STATION FILE,               close(2)
C      file 3  - INPUT RESERVOIR DATA FILE,        close(3)
C      file 4  - INPUT DIVERSION DATA FILE,        close(4)
C      file 5  - INPUT INFLOWS - SYSTEM DATA FILE  close(55)
C      file 5  - INPUT IFR STA - SYSTEM DATA FILE  close(55)
C
C      file 1  - INPUT PRECIP. FILE - SORT BY YEAR
C      file 2  - INPUT EVAPOR. FILE - SORT BY YEAR
C      file 3  - INPUT HISTORICAL FLOW FILE  - SORT BY YEAR
C      file 4  - INPUT HISTORICAL DIV DATA FILE - SORT BY YEAR
C      file 5  - INPUT CONSTANT MONTHLY EVAP. DATA close(55)
C      file 5  - INPUT CONSTANT MONTHLY DIV. DATA  close(55)
C      file 5  - INPUT INSTREAM FLOW REQUIREMENT   close(55)
C      file 5  - INPUT RETURN FLOW DELAY TABLES,   close(55)
C      file 5  - INPUT EOM RESERVOIR CONTENTS - SORT BY YEAR
C                               
C      file 14 - IWR data (*.iwr)
C      file 16 - base flow information
C      file 17 - VIRGIN FLOWS AT HISTORICAL FLOW STATIONS
C      file 19 - VIRGEN FLOWS IN THE FORMAT REQUIRED FOR SIMULATION
C      file 20 - response file
C
C-------------------------------------------------------------------
C
      include 'common.inc'

c                                            
      character recin*256
c
c _________________________________________________________
c
c               Step 1; Open Output files
c
      if(ioptio.eq.1) then
        write(io99,100)
  100   format(/, ' Baseflow Option = base (Standard)',//)
      endif

      if(ioptio.eq.9) then
        write(io99,109)
  109   format(/, ' Baseflow Option = basefx', 
     1            ' (Ungaged Locations Only)',//)
      endif
c
c               Monthly baseflow information (*.xbi)                
      call namext(maxfn, filenc, 'xbi', filena) 
      open(70,FILE=filena,STATUS='Unknown')
c
c               Monthly baseflow at gages (*.xbg)
      call namext(maxfn, filenc, 'xbg', filena) 
      open(71,FILE=filena,STATUS='Unknown')
c
c               Monthly baseflow estimates (*.xbm) 
      call namext(maxfn, filenc, 'xbm', filena) 
      open(72,FILE=filena,STATUS='Unknown')
c
c rrb 200/03/02; Open after datinp if daily is on
c             Daily baseflow estimates (*.xby)
cr    call namext(maxfn, filenc, 'xby', filena) 
cr    open(73,FILE=filena,STATUS='Unknown')
c
c             Daily baseflow information (*.xbx)
cr    call namext(maxfn, filenc, 'xbx', filena) 
cr    open(74,FILE=filena,STATUS='Unknown')
c
c               Open temporary file for use in vircom.for
c rrb 2006/07/31; Add loss and pumping 4*14=56
c rrb 2008/10/29; Add diversion to recharge and reservoir seepage 4*16=64
c rrb 2008/12/09; Add diversion to use 4*17=68
      open(75,status='Scratch',access='direct',recl=68,
     1     err=9999)
c
c               Monthly baseflow at stream Estimats (*.xbe)
      call namext(maxfn, filenc, 'xbe', filena) 
      open(76,FILE=filena,STATUS='Unknown')
c
c               Monthly Baseflow with negatives(*.xgn)
      call namext(maxfn, filenc, 'xgn', filena) 
      open(81,FILE=filena,STATUS='Unknown')
c
c               Open file *.b78; Binary Return File
c     call namext(maxfn, filenc, 'b78', filena) 
c     open(78,file=filena,  status='replace',access='direct',recl=8) 
c
c rrb 200/03/02; Open after datinp if daily is on
c               Daily StreamGage_Base (*.xgy)
cr    call namext(maxfn, filenc, 'xgy', filena) 
cr    open(79,FILE=filena,STATUS='Unknown')
c
c               Daily Stream Estimate_Base (*.xey)
cr    call namext(maxfn, filenc, 'xgy', filena) 
cr    open(80,FILE=filena,STATUS='Unknown')
c
c rrb 2007/09/18; Structure Summary (*.b67) (33*4=128)
      call namext(maxfn, filenc, 'b67', filena) 
      open(67,file=filena,  status='Replace',access='direct',recl=132) 
c
c rrb; 2007/09/18; Structure Summary
      call namext(maxfn, filenc, 'xss', filena)
      open(40,FILE=filena,STATUS='Replace') 
      
c
c _________________________________________________________
c
c               Step 2; Open response (*.rsp) file
      IIN=20
      call namext(maxfn, filenc, 'rsp', filena) 
      open (iin,file=filena,status='old',err=928)
      call skipn(iin)
c
c _________________________________________________________
c
c               Step 3; Read control data
      numstax=maxsta
      
      CALL DATINP(IIN,0,numstax)
      if(ichk.eq.4) write(nlog,*) ' Virgen; Out of Datinp'
c
c
c _________________________________________________________
c rrb 96/06/06; Write header info to binary files
      iystr0=iystr
      iyend0=iyend   
      call bintop(67,0,nlog,ichk)
      
c
c _________________________________________________________
c		Open daily output files       
      if(iday.eq.1) then
c
c               Daily baseflow estimates (*.xby)
        call namext(maxfn, filenc, 'xby', filena) 
        open(73,FILE=filena,STATUS='Unknown')
c
c               Daily baseflow information (*.xbx)
        call namext(maxfn, filenc, 'xbx', filena) 
        open(74,FILE=filena,STATUS='Unknown')
c
c               Daily StreamGage_Base (*.xgy)
        call namext(maxfn, filenc, 'xgy', filena) 
        open(79,FILE=filena,STATUS='Unknown')
c
c               Daily StreamEstimage_Base (*.xey)
        !jhb May 2014 typo in file name extension
        !call namext(maxfn, filenc, 'xgy', filena)
        call namext(maxfn, filenc, 'xey', filena)
        open(80,FILE=filena,STATUS='Unknown')
      
      endif
      
c
c _________________________________________________________
c
c               Step 4; Read right data
c                       Note required for sprinkler use option
      maxwrx=maxwr
      if(ichk.eq.4) write(nlog,*) ' Virgen; Calling Riginp'
c
c rrb 2009/06/09; Correction                
cx    CALL RIGINP(IIN,maxwrx)
      maxres1=maxres
      maxdvr1=maxdvr
      call riginp(iin, maxres1, maxdvr1)      
      
      if(ichk.eq.4) write(nlog,*) ' Virgen; Back from Riginp'
c
c _________________________________________________________
c
c               Step 5; Print headers
c
      if(ichk.eq.4) write(nlog,*) ' Virgen; Calling Outtop'
      if(ioptio.eq.1) then
        call outtop(70,1,1)
        call outtop(71,0,29)
        call outtop(72,0,15)
        call outtop(76,0,49)
        call outtop(81,0,64)

        if(iday.eq.1) then
          call outtop(73,-2,41)
          call outtop(74,1,43)
        endif  
      endif
      
      if(ioptio.eq.9) then
        call outtop(70,1,37)
        call outtop(71,0,29)
        call outtop(72,0,38)
        call outtop(76,0,49)
        
        if(iday.eq.1) then        
          call outtop(73,-1,42)
          call outtop(74,1,44)
        endif  
      endif
c
c _________________________________________________________
c
c               Step 6; Open time series files
c
c     write(6,*) ' virgen; into mdainp'
      I12=0  
      if(ichk.eq.4) write(nlog,*) ' Virgen; Calling Mdainp(0)'
      
      CALL MDAINP(IIN,I12)
c
c _________________________________________________________
c
c               Step 7; Initilize binary return file
c                       for special CWCB request
c rrb 0/03/28; 
c     irec1=0
c     write(io99,*) '  Execut; numsta, ndlymx', numsta, ndlymx
c     do i=1,nstrtn
c       do j=1,ndlymx
c         irec1=irec1+1
c         write(78,rec=irec1) 0.0,0.0
c       end do
c     end do

c
c _________________________________________________________
c
c               Step 8; Open and reset some monthly files to
c                       historic to allow baseflow option to 
c                       use mdainp.
      if(ichk.eq.4) write(nlog,*) ' Virgen; Calling Virin(0)'
      call virin(0,iin)
c
c _________________________________________________________
c
c               Step 9; Open daily files
      if(iday.eq.1) then

c
c               Skip over demand files, etc to get to historic files
c rrb 01/06/15; Problem with no wells
c       call skip(iin,7)
        call skip(iin,6)
        if(iwell.ne.0) call skip(iin,1)
cx      write(io99,*) '  Virgen.for; Calling dayest iin, i12', iin, i12  
c        
c rrb 01/06/18; Call year
        if(ichk.eq.4) write(nlog,*) ' Virgen; Calling DayEst'

        call year(iystr, iyrmo, imomo, cyr1) 
        call dayest(iin, i12) 
        ido=0
      endif
c
c _________________________________________________________
c
c               Step 10; Annual Initilization
c
      I12=12
      IYR=IYSTR-1
c     IP=0                                             
c
      IMO=0
c
c _________________________________________________________
c
c               Step 11; Year Loop
      write(6,*) ' '
  250 IYR=IYR+1     
c
      IF(IYR.GT.IYEND) GO TO 310
c
c     write(6,260) iyr
c     call flush(6)
c 260 format('+', ' Base flow calculations for year ', i5)
c
c
c _________________________________________________________
c
c               Step 12; Get all monthly data except reservoir
      if(ichk.eq.4) write(nlog,*) ' Virgen; Calling Mdainp(12)'
cx      write(nlog,*) ' Virgen; Calling Mdainp(12)'

      CALL MDAINP(IIN,I12)
c
c
c _________________________________________________________
c
c               Step 13; Get Monthly resrvoir data
      if(ichk.eq.4) write(nlog,*) ' Virgen; Calling Virin(12)'
cx      write(nlog,*) ' Virgen; Calling Virin(12)'

      call virin(1,iin)
c
c _________________________________________________________
c
c               Step 14; Baseflow calculations
c
      if(ichk.eq.4) write(nlog,*) ' Virgen; Calling Vircom(12)'
cx      write(nlog,*) ' Virgen; Calling Vircom(12)'
      CALL VIRCOM(iin, i12, maxsta)
c
c
c _________________________________________________________
c
c               Step 15; GO TO BEGINNING OF YEAR LOOP
c
      GO TO 250
c
c
c _________________________________________________________
c
c               Step 16; End of simulation

  310 continue
c
c _________________________________________________________               
c
c               Step 17; Close files
c               Note 3=stream, 4=diversions, 9=wells, 10=EOM
c               20=control, 70-73=output, 
      close( 1)
      close( 2)
      close( 3)
      close( 4)
      close( 9)
      close(10)
      close(14)
      close(70)
      close(71)
      close(72)
      close(73)
      close(20)
      close(55)
      close(56)
      close(76)
      close(81)
c
c
c _________________________________________________________
c
c               Step 18; Print output files to Screen
c rrb 10/17/95
      if(iday.eq.0) then
        write(6,330) 'Monthly ' 
        write(nlog,330) 'Monthly '
      else
        write(6,330) 'Daily   ' 
        write(nlog,330) 'Daily   '      
        write(6,332) 'Daily   '
        write(nlog,332) 'Daily   '
      endif 
        
      write(6,334)
      write(nlog,334)
      
c
c _________________________________________________________
c
c               Step 19; Return
      return
c
c _____________________________________________________________
c
c               Formats

 330  format(//,72('_'),/, ' Virgen; Baseflow Results ' a8,/
     1 '   Monthly Base flow information:          *.xbi',/
     1 '   Monthly Base flow data for simulation:  *.xbm',/
     1 '   Monthly Base flow at stream gages:      *.xbg',/
     1 '   Monthly Base flow at stream estimates:  *.xbe')

 332  format(//,72('_'),/, ' Virgen; Baseflow Results ' a8,/
     1 '   Daily Base flow information:            *.xbx',/
     1 '   Daily Base flow data for simulation:    *.xby') 
      
 334  format(/, 
     1 ' Virgen; Successful Termination')
c
c _____________________________________________________________
c
c               Error Handling

  926 write(io99,927) iin, filena
  927 format(' Virin.f; End of file # ', i4, ' encountered',/,
     1       '   File name: ', a256)
      goto 9999

  928 write(io99,929) iin, filena
  929 format(' Virin.f; Problem reading file # ', i4,/,
     1       '   File name: ', a256,/
     1       '   Problem record (next line):')

      backspace(iin)
      read(iin, '(a256)',end=926,err=926) recin
      write(io99,'(a256)') recin
      goto 9999

 9999 write(6,*) '  Stopped in Virgin, see the log file (*.log)'
      write(io99,*)'  Stopped in Virgin'
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop 
      END   

