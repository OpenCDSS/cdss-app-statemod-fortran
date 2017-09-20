c
c *********************************************************
c
      SUBROUTINE XDEBUG(nreach)
c
c
c _________________________________________________________
c	Program Description
c
c       Xdebug; It controls the data check option
c
c _________________________________________________________
c       Documentation
C
C      FILES AND REQUIRED SORT PRIORITIES:
C
C      TAPE 1  - INPUT CONTROL PARAMETER FILE,     close(1)
C      TAPE 2  - INPUT STATION FILE,               close(2)
C      TAPE 3  - INPUT RESERVOIR DATA FILE,        close(3)
C      TAPE 4  - INPUT DIVERSION DATA FILE,        close(4)
C      TAPE 55 - INPUT INFLOWS - SYSTEM DATA FILE  close(55)
C      TAPE 55 - INPUT IFR STA - SYSTEM DATA FILE  close(55)
C
C      TAPE 1  - INPUT I.F.R. FILE - SORT BY DATE THEN STATION,
C                                                  close(1)
C      TAPE 2  - INPUT RES RIGHT FILE - SORT BY DATE THEN STATION,
C                                                  close(2)
C      TAPE 3  - INPUT DIVER. RIGHT FILE - SORT BY DATE THEN STA.,
C                                                  close(3)
C      TAPE 4  - INPUT MISCELLANEOUS RIGHT FILE    close(4)
C      TAPE 55 - INPUT OPERATION RULE FILE         close(55)
C
C      TAPE 1  - INPUT PRECIP. FILE - SORT BY YEAR
C      TAPE 2  - INPUT EVAPOR. FILE - SORT BY YEAR
C      TAPE 3  - INPUT RUNOFF FILE  - SORT BY YEAR
C      TAPE 4  - INPUT DIV DMD FILE - SORT BY YEAR
C      TAPE 55 - INPUT CONSTANT MONTHLY EVAP. DATA close(55)
C      TAPE 55 - INPUT CONSTANT MONTHLY DIV. DATA  close(55)
C      TAPE 55 - INPUT INSTREAM FLOW REQUIREMENT   close(55)
C      TAPE 55 - INPUT RETURN FLOW DELAY TABLES,   close(55)
C
C      TAPE 7  - VIRGIN FLOWS AT EACH STATION
C      TAPE 8  - DIVERSION DEMANDS AT EACH STATION
C      TAPE 9  - LINKAGE OF ALL SYSTEM LAYOUT
C      TAPE 77 - End of Month Contents
c
c _________________________________________________________
c	Dimensions
      include 'common.inc'
c
c _________________________________________________________
c		Step 1; Initilize
      write(99,10)
 10   format(/, ' Check Option',//)

c
c _________________________________________________________
c
c       OPEN ALL OUTPUT FILES
c
      call namext(maxfn, filenc, 'xcb', filena) 
      open(7 ,FILE=filena ,STATUS='Unknown')

      call namext(maxfn, filenc, 'xcd', filena) 
      open(8 ,FILE=filena ,STATUS='Unknown')

      call namext(maxfn, filenc, 'xwr', filena) 
      open(11 ,FILE=filena ,STATUS='Unknown')

      call namext(maxfn, filenc, 'xci', filena) 
      open(13,FILE=filena ,STATUS='Unknown')

      call namext(maxfn, filenc, 'xcw', filena) 
      open(12,FILE=filena ,STATUS='Unknown')

      call namext(maxfn, filenc, 'xtb', filena) 
      open(19,FILE=filena ,STATUS='Unknown')

      call namext(maxfn, filenc, 'xou', filena) 
      open(21,FILE=filena ,STATUS='Unknown')
c
c rrb Reach Data      
      call namext(maxfn, filenc, 'xrh', filena) 
      open(22,FILE=filena ,STATUS='Unknown')
c
c	rrb River Reach      
      call namext(maxfn, filenc, 'xri', filena) 
      open(28,FILE=filena ,STATUS='Unknown')
c
c
c _________________________________________________________
c

C               Read in response file (*.rsp)
  100 FORMAT(A20)
      call namext(maxfn, filenc, 'rsp', filena) 
      open (20,file=filena,status='old',err=9997)
      call skipn(20)
      IIN=20
C
  120 numstax=maxsta
      CALL DATINP(IIN,0,numstax)

      maxwrx=maxwr
c
c rrb 2009/06/09; Correction                
cx    CALL RIGINP(IIN,maxwrx)
      maxres1=maxres
      maxdvr1=maxdvr
      call riginp(iin, maxres1, maxdvr1)          

      maxnwrx=maxnwr
      CALL RIGSOR(maxnwrx)
c
c rrb 00/02/23; Check demand Vs decrees Vs capacity
      call demcons(0)
c
c _________________________________________________________
c
c               FIND THE STARTING POINT AT ALL MONTHLY INPUT DATA FILES
      I12=0
C
      CALL MDAINP(IIN,I12)
        if(infile.eq.0) then
          call skip(iin,2)
        endif
c
c              Open historic streamflow data
        write(io99,*) ' '
        Write(io99,*) ' Xdebug; Historic Streamflow File (*.rih) '
        write(6,*) ' '
        Write(6,*) '  Xdebug; Historic Streamflow File (*.rih) ' 
c
        if(infile.eq.1) then
          ifn=27
          rec256=fileName(ifn)
c         write(io99,*) ' Xdebug; fileName = ', filename(ifn)
c         write(io99,*) ' Xdebug; fpath1   = ', fpath1
        endif

        call chekpor(iin, 18, 99, 0, ioptio, 999, iystr, 
     1               imstr, 0, 8, c, cyr1, maxfn,
     1               infile, idummy, nRihX, fpath1, rec256)
C
      I12=12
      IYR=IYSTR-1
      IP=0
      CALL OUFLOW(IP)
c
c _________________________________________________________
c 
c               PRINT STATIONS VIRGIN FLOW AND DIVERSION DEMAND
      write(6,*) ' '
  130 IYR=IYR+1
C
      IF(IYR.GT.IYEND) GO TO 150
c
      write(6,140) iyr
  140 format('+', ' Data check for Year = ', i5)
c 140 format('  Xdebug; Data check for Year = ', i5) 
c
c     write(6,*) '  Xdebug; Calling mdainp'
      CALL MDAINP(IIN,I12)
c
c rrb 00/02/23; Check Demand Vs Decrees Vs Capacity
      call demcons(1)
C
      IP=IP+1
c     write(6,*) '  Xdebug; Calling ouflow'
      CALL OUFLOW(IP)
C
C------  GO TO BEGINNING OF YEAR LOOP
C
      GO TO 130
C
C
  150 continue
c
c _________________________________________________________
c
c		Print Data Check Data
c jhb 2014/07/21 include second outdeb argument, nreach
c      CALL OUTDEB(1)
      CALL OUTDEB(1,nreach)
c 
c
c _________________________________________________________
c rrb 2009/04/26; 
c   If River Reach data is provided (*.rir)
c            Open the River Reach file (80)
c            Call GetRch to get River Reach Data (iget=1)) 
c            Call OutRch to print Reach Data (*.xre & *.rch)
c
      inf = 80
      rec256=fileName(inf)
      filena=rec256
      
      if(filena(1:2).ne.'-1') then  
        close(55) 
cx        write(nlog,*) ' '      
cx        Write(nlog,*) ' Xdebug; River Reach File (*.rir) ', filena
cx        Write(6,*) '  Xdebug; River Reach File (*.rir) ', filena
        
        write(nlog,155) filena
 155    format(/,72('_'),/
     1  ' Xdebug; River Reach File (*.rir) '/,8x, a256)
c
c rrb 2009/04/26; Open River Reach data (*.rir)
c 		      inf = filename
c	  	      nf = file #
        nf=55
        call GetFile(
     1   nlog, nchk, iin, infile, maxfile, maxfn, 
     1   nf, irisX, numRis, fileType(inf), fileSuf(inf), 
     1   fileName(inf), fpath1, filena) 
c
c           Get River Reach data (iget=1)
        iget=1
        call GetRch(nlog, iget, maxsta, maxrch, 
     1    nreach, iRch, nRchTo, nRchEnd,
     1    RchId, RchIdR, RchNameR, cstaid) 
c
c ---------------------------------------------------------
c           Call OutRch to print Reach Data       
        call outRch(nreach)
      endif
c
c _________________________________________________________
c
c   Close files

      CLOSE( 1)
      CLOSE( 2)
      CLOSE( 3)
      CLOSE( 4)
      CLOSE(55)
      CLOSE( 7)
      CLOSE( 8)       
      close( 9)       
      close(10)
      close(11)
      close(12)
      close(18)       
      close(19)       
      close(20)       
      close(21)       
      close(22)       
      close(28)
      close(77)       
c
c _________________________________________________________
c
c
      write(6,*) ' '
      write(6,*) ' Base flow data output in        *.xcb'
      write(6,*) ' Direct Demand by River ID in    *.xcd'
      write(6,*) ' Instream Demand by River ID in  *.xci'
      write(6,*) ' Well Demand by River ID in      *.xcw'
      write(6,*) ' Water Right List in             *.xwr'
      write(6,*) ' Summary Tables in               *.xtb'
      write(6,*) ' Preliminary Output Control      *.xou'
      write(6,*) ' Preliminary Reach Data          *.xrh'

      write(6,900)
      write(nlog,900)
900   format(/,       
     1  ' Xdebug; Successful Termination',/
     1  ' ',/
     1  ' !! Remember to edit the following with default data',/
     1  '     Preliminary Output Control      *.xou',/
     1  '     Preliminary Reach Assignment    *.xrh',/
     1  ' !!  Especially the reach connectivity in *.xrh')
      
c
c _________________________________________________________
c
      return

 9997 write(99,9998) filena
 9998 format('  Xdebug; Problem opening file: ',/, a256)
 9999 write(6,*) '  Stopped in Xdebug, see the log file (*.log)'
      write(99,*) '  Stopped in Xdebug'
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop 

      END



