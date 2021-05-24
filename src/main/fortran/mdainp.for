c mdainp - reads in time series data (streamflow, etc.)
c_________________________________________________________________NoticeStart_
c StateMod Water Allocation Model
c StateMod is a part of Colorado's Decision Support Systems (CDSS)
c Copyright (C) 1994-2021 Colorado Department of Natural Resources
c 
c StateMod is free software:  you can redistribute it and/or modify
c     it under the terms of the GNU General Public License as published by
c     the Free Software Foundation, either version 3 of the License, or
c     (at your option) any later version.
c 
c StateMod is distributed in the hope that it will be useful,
c     but WITHOUT ANY WARRANTY; without even the implied warranty of
c     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c     GNU General Public License for more details.
c 
c     You should have received a copy of the GNU General Public License
c     along with StateMod.  If not, see <https://www.gnu.org/licenses/>.
c_________________________________________________________________NoticeEnd___
C
c
c rrb 2021/05/02; Runtime error tracking - Save issue
cx    SUBROUTINE MDAINP(IIN,I12)
      SUBROUTINE MDAINP(IIN,I12,itarx,iter)   
c _________________________________________________________
c	Program Description
c
c       Mdainp; It reads in time series data (streamflow, etc.)
c		    Called by Execut, Virgen and Xdebug:
c		  - Once to open files and read 1x/year data
c		    (I12=0)
c		  - Once every year thereafter
c		    (I12=12)
c		
c
c
c _________________________________________________________
c       Documentation
c               iin     response file number (20)          
c               i12     read option
c                       = 1 open files first time        
c                       =12 read data one year at a time.
c               itarx   = 0 min and max targets provided
c                       = 1 only max target providedc
c		idvcomw = Well Demand Type 
c			  1 Monthly demand provided
c			  2 Not used
c			  3 Monthly IWR provided
c			  4 Not used
c			  5 Set to zero
c			  6 Tied to a diversion, expect data in *.ddm
c
c _________________________________________________________
c       Update History
c
c rrb 2021/05/02; Runtime error tracking - Save issue
c 
c rrb 2021/04/18; Compiler warning
c
c rrb 2020/07/27; Ver = 16.00.36
c                   Correction do not set zero values in import
c                   data to -0.001 
c
c rrb 2019/12/29; Ver = 16.00.22
c                   Revised Mdainp.f to fix a problem when negative
c                   diversions (import) are mixed with values greater
c                   greater than or equal to zero by setting ',/
c                   zero = -0.001 cfs or warning and stopping',/     
c
c rrb 2018/10/14; Revise to read JMartin monthly baseflow percent
c
c rrb 2000/05/20; Do not allow *.ddm and *.wem to be added for baseflows
c
c rrb 2000/06/16; Revised to allow IWR data to be provided
c               diverir for diversions and diverirw for wells
c               Also control addition of diversions, etc. by global
c               control idemtyp from datinp
c
c rrb 2000/10/23  Revised Rio Grande forecast data (*.ifm) to read 13 
c               values (12 plus total) plus a code rgspill). Where  
c               the month of spill is the number and the % spill 
c               is the % Colorado (used to adjust a surplus)
c
c rrb 2000/11/11  Allow annual time series data to be read if 
c   
c rrb 2000/11/24  If itsfile =-1, *.ipy or *.tsp file provided but skipped
c               If itsfile = 0, no *.tsp (*.ipy) file provided
c               If itsfile = 1, yes *.tsp (*.ipy) use annual GW limit
c               If itsfile = 2, yes *.tsp (*.ipy) use annual well capacity
c               If itsfile = 3, yes *.tsp (*.ipy) use annual max flood n
c               If itsfile = 4, yes *.tsp (*.ipy) use annual GW area
c               If itsfile = 5, yes *.tsp (*.ipy) use annual GW Mode
c                               if isoil.ne.0)
c               If itsfile =10, yes *.tsp (*ipy) use all of above (1-5)

c
c               If isjrip  =-1, *.sjr file provided but skipped
c               If isjrip  = 0, no *.sjr file provided
c               If isjrip  = 1, read annual SJRIP sediment data (*.sjr)
c
c rrb 2000/12/04; 
c               If ieffmax =-1, IRW data provided but skipped 
c               If ieffmax = 0, no IWR data  
c               If ieffmax = 1, read IWR data for max n (*iwr)
c               If ieffmax = 2, read IWR data for max n but do not use
c
c               Reservoir Target data
c
c               itarx = 0 = min and max targets provided
c                       1 = only max target provided
c                           Note read once then used for all subsequent 
c                           years
c               targetx( )= target for this year (note if a forecast it
c                           gets adjusted to reflect # of months, etc.
c               targetn( )= target for next year (required for running 
c                           with just 1 year of data?)
c               sjtarget()= target used for SJRIP (required because 
c                           targetx gets adjusted in bomsec.f
c
c rrb 2003/08/18; Revise to allow random file read
c
c rrb 2001/08/27; Revise *.tsp read to allow extra data in file
c                           
c rrb 1996/05/29; change to generalize dimensions
c     dimension x(1), iwarn(850)
c
c rrb 1999/10/05; make dlytot a global variable
c     dimension x(12), iwarn(1), dlytot(1)
c     equivalence (iwarn, dumusr(1)), (dlytot, dummy(1,1))
c
c rrb 2001/12/21; Dimension
c     dimension x(12), iwarn(1)
c     equivalence (iwarn, dumusr(1))
c rrb 2002/01/15; Dimension clean up
c     dimension x(12), iwarn(1110)
c rrb 2005/04/04; Add imports (PImportX)
c
c rrb 2006/03/21; Read Rio Grande forecast monthly (*.rgf)
c
c _________________________________________________________
c              Dimensions
      include 'common.inc'
      dimension x(12), y(12), iwarnw(maxdivw)
      character rec3*3, rec40*40, rec12*12, rec12b*12, rec132*132
c
      character cistat*12, cista2*12, blank*12, recin*256, rec72*72,
     1  cCallBy*12
c jhb 2014/06/26
      character(len=256) rec256x
c _________________________________________________________
c
c
c              Step 1; Initialize
c
c rrb 2021/04/18; Compiler warning
      monppt=0 
      iuse=0
      rec12b=' '
      rec3=' '
      do i=1,12
        y(i)=0.0
      end do
c
      nscrn=6
c
c rrb 2017/12/12; Reduce output to screen     
cx    write(nscrn,*) ' Mdainp.for'
      blank = '            '
      small = 0.001
      smalln=-1*small
      
      numovr=0
      idyyr=0
      iproblem=0
c
c              iouts = 1 print stream data read
c             ioutGx = 1 print stream gain data
c              ioutd = 1 print demand and iwr data      
c                      2 print demand data summary
c                      3 print *.ddh structures found in *.dds
c              ioutw = 1 print well data
c                      2 print summary of well only data reads
c              ioutx = 1 print import data
c              iouti = 1 print instream flow data
c              ioutr = 1 print reservoir data
c             ioutEv = 1 print evaporation data
c            ioutPpt = 1 print evaporation data
c            ioutRgS = 1 print Rio Grande Spill data
c            ioutRgF = 1 print Rio Grande Spill Forecast data
c            ioutSM =  1 print soil moisture
c                      2 soil moisture summary from station file
c                      3 soil moisture summary from *.ipy file
c            ioutPrf = 1 print Plan return flow data
c            ioutURM = 1 print *.urm details
c                      1 print *.urm summary
cx      ioutS=1
cx      ioutGx=1
cx      ioutX=1
cx      ioutW=1
cx      ioutI=1
cx      ioutD=1
cx      ioutR=1
cx      ioutEv=1
cx      ioutPPt=1
cx      ioutURM=1
      
      
      ioutS=0
      ioutGx=0
      ioutX=0
      ioutW=0
      ioutI=0
      ioutD=0
      ioutR=0
      ioutEv=0
      ioutPPt=0
      ioutURM=0
      
      
      
      if(ichk.eq.11) ioutEv=1
      ioutRgS=0
      ioutRgF=0
      ioutSM=0
      ioutPrf=0
c
c rrb 2021/05/02; Runtime error tracking      
      ioutTar=0
      if(ioutTar.eq.1) then
        write(nlog,*) '  Mdainp;  iyr iin  i12 itarx' 
        write(nlog,'(a10,20i5)') '  Mdainp; ',iyr, iin, i12, itarx
      endif         
c
c
      cCallBy='MdaInp      '

      
      ipDdc=1
      ipDdm=1
      ipIpy=1
c
c rrb 209/05/21; correction      
cx      istart=0
cx      if(iyr.eq.iystr) istart = 1
cx      if(istart.eq.1) then
      if(i12.eq.0) then
        ipDdc=0
        ipDdm=0
        ipIpy=0
      endif  
      iprintS=0
       
c
c     iwarnU = 0 warn user about unit issues but go on 
c     iwarnU = 1 warn user about unit issues and stop
c     iwarnDT  counter for diversion warnings
c     iwarnWT  counter for well warnings
c     iwarnISt counter for *.ipy file structure warning
c     iwarnISp counter for *.ipy file sprinkler warning
      iwarnU=0
      iwarnDT=0
      iwarnWT=0
      iwarnISt=0
      iwarnISp=0
c
c               b. Daily capability
c
c rrb 209/05/21; correction variable Mon is not set 
cx      if(i12.ge.1) then
cx       if(iday.eq.0) then
cx          fac=mthday(mon)*factor
cx        else
cx         fac=factor
cx        endif 
cx      endif     
c
c rrb 01/01/03; Recognize other baseflow types
      ibasef=0
      if(ioptio.eq.1 .or. ioptio.eq.9) then 
        ibasef=1
      endif
      
      if(ioutSM.eq.2 .or. ioutSM.eq.3) write(nlog,1323)
      
      
c     write(nlog,*) '  Mdainp; ibasef = ', ibasef
C
C-------------------------------------------------------------------
C
C------  Process 1x/run (e.g. initialize and open files)
C                     
C-------------------------------------------------------------------
C
c		Branch if reading every month
      IF(I12.EQ.12) GO TO 950
      
      iyr=0
        
c
c               1x/run Set warning on negative diversions (imports)
      do nu=1,maxuse
        iwarn(nu) = 0
      end do
c
c rrb 2006/08/01; Addition        
      do nw=1,maxdivw
        iwarnW(nw)=0
      end do  
c
c _________________________________________________________
C        1x/Run Initialize imports 1x/run
c
          DO ip=1,maxplan
            do im=1,13
              PImportX(im,ip)=0.0
            end do
          end do

          iwarnp=0
 
C
c _________________________________________________________
C        Step X; 1x/Run Open PRECIPITATION FILE
c
      write(nlog,101)
c     write(nscrn,101)
  101   format(/,72('_'),/
     1  '  Mdainp; Precipitation File (*.pra or *.prm)')

      nc = numpre
      if(numpre.eq.0 .or. numres.eq.0) nc = 1
c
c ---------------------------------------------------------
c		Set monppt a monthly ppt file (14) or annual (67)

      monppt=-1
      if(infile.eq.1) then
        ifn=14      
        rec256=filename(ifn)
        if(rec256(1:2).ne.'-1') monppt=0
        
        if(monppt.eq.-1) then
          ifn=67      
          rec256=filename(ifn)
          if(rec256(1:2).ne.'-1') monppt=1
        endif  
      endif
c
c ---------------------------------------------------------
c
      iox=0
      if(ioutPpt.eq.1) then
        write(nlog,*) ' Mdainp; iox, monppt, ifn = ', iox, monppt, ifn
        write(nlog,*) ' Mdainp; ', rec256
        write(nlog,*) ' Mdainp; ', filename(ifn)
      endif
      
      call chekpor(iin, 1, 99, monppt, ioX, nc, iystr, 
     1             imstr, 0, 2, pfacto, cyr1, maxfn, infile, idummy,
     1             nPptX, fpath1, rec256)
c
c ---------------------------------------------------------
c
      if(pfacto .gt. small) then          
        if(idummy.eq.0 .and. abs(pfacto-pfacto1).gt.small) then
          write(nlog,1640) '*.pre', pfacto, pfacto1, pfacto
          if (iwarnU.eq.1) goto 9997        
        endif
      endif  
c
c ---------------------------------------------------------
c rrb 2006/11/11; Count number of precipitation stations      
c		  to simplify control file reads
      call count(nlog, 1, numpre, iystr, 'Precipitation   ', rec256)
C
c _________________________________________________________
C        Step X; 1x/Run Open EVAPORATION FILE
c
      write(nlog,102) 
c     write(nscrn,102)
  102 format(/,72('_'),/,
     1'  Mdainp; Evaporation File (*.evm or *.eva) ')

      nc = numeva
      if(numeva.eq.0 .or. numres.eq.0) nc = 1
c
c ---------------------------------------------------------
c		Open a monthly evap file (15) or annual (66)
      if(infile.eq.1) then
        moneva=-1
        ifn=15      
        rec256=filename(ifn)
        if(rec256(1:2).ne.'-1') moneva=0
        
        if(moneva.eq.-1) then        
          ifn=66      
          rec256=filename(ifn)
          if(rec256(1:2).ne.'-1') moneva=1
        endif  
      endif
c
c ---------------------------------------------------------
c      
      iox=0
      if(ioutEV.eq.1) 
     1  write(nlog,*) ' Mdainp; moneva = ' ,moneva, ifn, rec256
      call chekpor(iin, 2, 99, moneva, ioX, nc, iystr, 
     1             imstr, 0, 1, efacto, cyr1, maxfn, infile, idummy,
     1             nEvaX, fpath1, rec256)     
      if(ioutEV.eq.1) write(nlog,*) '  Mdainp; Back from ChekPor'
c
c ---------------------------------------------------------
c rrb 2006/03/02; Treat all factors consistently
      if(efacto .gt. small) then     
        if(idummy.eq.0 .and. abs(efacto-efacto1).gt.small) then
          write(nlog,1640) '*.eva', efacto, efacto1, efacto  
          write(nlog,*) ' Mdainp; iwarnU ', iwarnU
          if (iwarnU.eq.1) goto 9997
        endif
      endif
c
c ---------------------------------------------------------
c rrb 2006/11/11; Count number of evaporation stations      
c		  to simplify control file reads
      call count(nlog, 2, numeva, iystr, 'Evaporation     ', rec256)
c
c
      if(numeva.eq.0 .and. numres.gt.0) then
        write(nlog,*) ' '
        write(nlog,*)
     1    '  Mdainp; Problem reservoir data but no evap data'
        write(nlog,*) 
     1    '  Mdainp; or the POR does not match the the control file'
        goto 9999
      endif
c
c _________________________________________________________
C        Step X; 1x/Run Open RUNOFF FILE
C
      if(ibasef.eq.0) then
        write(nlog,103)
c       write(nscrn,103)
 103    format(/,72('_'),/
     1 '  Mdainp; Base Streamflow File (*.rim or *.xbm) ')
      else
        write(nlog,113)
c       write(nscrn,103)
 113    format(/,72('_'),/
     1 '  Mdainp; Historic Streamflow File (*.rih) ')
      endif 
c
c ---------------------------------------------------------

      if(infile.eq.1) then
        ifn=16
        rec256=fileName(ifn)
      endif
c
c ---------------------------------------------------------
c
      call chekpor(iin, 3, 99, 0, ioptio, numrun, iystr, 
     1             imstr, 0, 3, rfacto, cyr1,maxfn,infile,idummy,
     1             nRihX, fpath1, rec256)
c
c ---------------------------------------------------------
c     
      if(idummy.eq.0 .and. abs(rfacto-rfacto1).gt.small) then
        write(nlog,1640) '*.rih', rfacto, rfacto1, rfacto
        if (iwarnU.eq.1) goto 9997        
      endif
     
c
c _________________________________________________________
C        Step X; 1x/Run  Open DIVERSION Demand FILE - Monthly
C
      write(nlog,104)
c     write(nscrn,104)
 104    format(/,72('_'),/
     1 '  Mdainp; Direct Flow Demand - Monthly (*.ddm) ')
     
cx      if(infile.eq.1) then
cx        ifn=17
cx        rec256=fileName(ifn)
cx      endif
      
      ifn=17
      rec256=fileName(ifn)      
      write(nlog,*) ' Mdainp; rec256 = ', rec256
      ityp=4
      
      call chekpor(
     1  iin, 4, 99, 0, ioptio, numdiv, iystr,
     1  imstr, 0, ityp, dfacto, cyr1, maxfn,
     1  infile,idummy, nDdmX, fpath1,rec256)
c     
      if(idummy.eq.0 .and. abs(dfacto-dfacto1).gt.small) then
        write(nlog,1640) '*.ddm', dfacto, dfacto1, dfacto
        if (iwarnU.eq.1) goto 9997
      endif
C
c _________________________________________________________
C        Step X; 1x/Run Open OVERRIDING FILE
C
C
cx		Move below if(Filena
cx      write(nlog,105)
cx 105   format(/,72('_'),/
cx     1  '  Mdainp; Direct Flow Demand Overwrite File - ',
cx     1   'Monthly (*.ddo) ')
c
      iin2=iin
      if(infile.eq.1) then
        ifn=19
        rec256=fileName(ifn)
        filena=rec256(1:72)
      else
        filena=filenc
        READ(IIN,'(a256)',end=926,err=928) FILENA
      endif
c
cs		Move below if(filena
cx      call putpath(maxfn, filena, fpath1)
cx      write(nlog,'(5x, a256)') filena
      if(ibasef.eq.1) goto 440 
c
      if(filena(1:2).ne.'-1') then
        write(nlog,105)
 105    format(/,72('_'),/
     1  '  Mdainp; Direct Flow Demand Overwrite File - ',
     1   'Monthly (*.ddo) ')
      
        call putpath(maxfn, filena, fpath1)
        write(nlog,'(5x, a256)') filena
      
        open(23,FILE=FILENA,STATUS='OLD',err=9997)
        write(nlog,*) '  Mdainp; Into skipn(23), filena ', filena
        call skipn(23)   
c
c              Read year type control data
        call chekts(nlog, 23, 12, c, idummy, cyr1)
      else
        idummy=1  
      endif
      
      dfactoO=c
c
c rrb 01/02/15; Allow multiple dummy files
      if(idummy.eq.1) goto 529

c     write(nlog,*) '  Mdainp; Into skipn(23)'
      call skipn(23)
      
      IDYRR=0
      NUMOVR=0

      iin2=23
  430 read(23,*,end=440,err=928) idyr

      IF(IDYR.NE.IDYRR.AND.NUMOVR.NE.0) GO TO 440
      IDYRR=IDYR
      write(nlog,*) '  Mdainp; numovr ', numovr
c     write(nscrn,,*) '  Mdainp; numovr ', numovr

      NUMOVR=NUMOVR+1
      GO TO 430
  440 IF (NUMOVR.EQ.0) GO TO 520
C
      REWIND 23     
C
  450 DO 460 ND=1,NUMOVR
      read(23,*,end=470,err=928) idyr

c
c rrb 2021/03/20; Compiler Update
cx      IF(IDYR-IYSTR) 460,510,490
      IF((IDYR-IYSTR).lt.0) goto 460
      IF((IDYR-IYSTR).eq.0) goto 510
      IF((IDYR-IYSTR).gt.0) goto 490
  460 CONTINUE
      GO TO 450
C
  470 write(nlog,480)
  480 FORMAT(/
     1 '  Mdainp; Problem',
     1 ' THE DIVERSION OVERRIDING DATA (*.ddo)',
     1 ' IS NOT IN THE SIMULATION PERIOD')
      Goto 9999
C
  490 write(nlog,500) IDYR
  500 FORMAT(/
     1 '  Mdainp; Problem',
     1 ' THE REQUESTED YEAR', I5, ' OF DIVERSION OVERRRIDING'
     1 ' DATA (*.ddo) IS NOT WITHIN THE STUDY PERIOD')
      Goto 9999
C
  510 BACKSPACE 23
      GO TO 529
C
cx520 close(23)
  520 continue
C
c _________________________________________________________
C        Step X; 1x/Run READ IN CONSTANT MONTHLY DATA - Precipitation
C
C                                        
c               moneva = 0 monthly data; 
c                      = 1 constant data
c
c rrb 2006/05/01; Correction
cr529 IF(NUMpre.EQ.0 .OR. MONEVA.EQ.0 .or. numres.eq.0) GO TO 530
  529 IF(NUMpre.EQ.0 .OR. monppt.EQ.0 .or. numres.eq.0) GO TO 530
      iin2=2
      filena='*.pra'
c     write(nlog,*) ' Mdainp; Getting constant precipitaion data'
c ---------------------------------------------------------
c
      do ipr=1,numpre
        read (1,532,end=571,err=928) cpreid(ipr),(x(i),i=1,12)
c       write(nlog,'(5x,a12,12f8.3)') cpreid(ipr),(x(i),i=1,12)
c
c rrb 2006/03/20; Adjust character string to left     
        cpreid(ipr)=adjustl(cpreid(ipr))

c
c ---------------------------------------------------------
c rrb 2006/03/02; Treat all factors consistently
        if(pfacto .gt. small) then           
          do i=1,12
            preprt(i,ipr) = x(i)*pfacto
          end do
        else
          do i=1,12
            preprt(i,ipr) = x(i)
          end do
        endif
      end do
C
      close (1)
C
C
c _________________________________________________________
C        1x/Run READ IN CONSTANT MONTHLY DATA - EVAPORATION
C
c               moneva = 0 monthly data; 
c                      = 1 constant data
  530 IF(NUMEVA.EQ.0 .OR. MONEVA.EQ.0 .or. numres.eq.0) GO TO 600
      iin2=2
      filena='*.eva'
c     write(nlog,*) ' Mdainp; Getting annlua evap data'
c ---------------------------------------------------------
      DO IEV=1,NUMEVA
        read (2,532,end=570,err=928) cevaid(iev),(x(i),i=1,12)
        if(ioutEv.eq.1) write(nlog,'(i5,5x,a12,12f8.3)')
     1    iev, cevaid(iev),(x(i),i=1,12)
c
c rrb 2006/03/20; Adjust character string to left     
        cevaid(iev)=adjustl(cevaid(iev))
c
c ---------------------------------------------------------
        
        if(efacto.gt.0) then
          do i=1,12
            evaprt(i,iev) = x(i)*efacto
          end do
        else
          do i=1,12
c
c rrb 2005/6/05/02; Correction
cr          evaprt(i,iev) = x(i)*efacto
            evaprt(i,iev) = x(i)
          end do
        endif
      end do

      close (2)
C
c _________________________________________________________
C        1x/Run READ IN Annual Diversion Demand
c           (12 constant values repeated each year) 
C
C
cx  600 write(nlog,106)
cx  106 format(/,72('_'),/
cx     1 '  Mdainp; Direct Flow Demand File - Annual (*.dda) ')
c
 600  close(55)

      if(infile.eq.1) then
        ifn=18
        rec256=fileName(ifn)
      endif
c
c ---------------------------------------------------------
c
      if(rec256(1:2).ne.'-1') then
        iin2=55
        filena='*.dda'
        
        write(nlog,106)
  106   format(/,72('_'),/
     1   '  Mdainp; Direct Flow Demand File - Annual (*.dda) ')
     
        call chekpor(
     1  iin, 55, 99, 1, ioptio, ndivin, iystr, 
     1  imstr, 0, 5, dfactoA, cyr1, maxfn, 
     1  infile,idummy,  nDdaX, fpath1, rec256)
     
        if(idummy.eq.0 .and. abs(dfacto-dfacto1).gt.small) then
          write(nlog,1640) '*.dda', dfactoA, dfacto1, dfactoA
          if (iwarnU.eq.1) goto 9997
         endif  
c
c ---------------------------------------------------------

         DO 670 ND=1,NUMDIV
           NUI=NDUSER(ND)
           NUE=NDUSER(ND+1)-1
           IF(NUI.GT.NUE) GO TO 670
           DO 660 NU=NUI,NUE
c
c               Read annual data (12 values for all years)
c
c rrb 00/06/16; Allow type 3 and 4 to be IWR data
c          IF(IDVCOM(NU).NE.2) GO TO 660
c
           if(idvcom(nu).eq.2 .or. idvcom(nu).eq.4) then
             read (55,532,end=680,err=928) cistat,(diverm(i),i=1,12)
c
c rrb 2006/03/20; Adjust character string to left     
             cistat=adjustl(cistat)
             IF(IDIVSW(ND).EQ.0) GO TO 660
             if(cistat.ne.cdivid(nd)) go to 700
c
c ---------------------------------------------------------
C
             IF(DFACTOA.gt.small) then  
               DO I=1,12
                 DIVERM(I)=DIVERM(I)/dfactoA/MTHDAY(I)
               end do
             endif
c
c ---------------------------------------------------------
C
c               Data provided as annual total demand (idvcom(nu) = 2)
             if(idvcom(nu).eq.2) then
               DO I=1,12
                 DIVER(I,NU)=DIVERM(I)
                 diverir(i,nu)=diverm(i)*(diveff(i,nu)/100.)
               end do
             endif
c
c
c ---------------------------------------------------------
c rrb 00/06/16; Data provided as annual IWR (idvcom(nu) = 4)
             if(idvcom(nu).eq.4) then
               DO Im=1,12
c
c               b. Daily capability
                 if(iday.eq.0) then
                   fac=mthday(im)*factor
                 else
                   fac=factor
                 endif      

                   if(diveff(im,nd).gt.small) then
                     diverir(im,nu) = diverm(im)  
                     DIVER(Im,NU)=DIVERM(Im)/(diveff(im,nu)/100.)
                   else
c
c rrb 00/08/02; Data check
                     if(diverm(im).gt. small) then
                     c = dfactoI*mthday(im)
                       write(nlog,757) cdivid(nd),divnam1(nd),
     1                 iyr, im, diverm(im)*c, diveff(im,nd)
                       goto 9999
                     else
                      diverir(im,nu) = 0.0
                      diver(im,nu)=0.0
                    endif
                  endif
                end do
              endif

            endif
  660     CONTINUE
  670   CONTINUE
C
        close(55)
C
        GO TO  720
C
  680   write(nlog,690)
  690   FORMAT(/
     1 '  Mdainp; Problem',
     1 ' Not Enough Data In The Annual Diversion File (*.dda)')
        Goto 9999
C
  700   write(nlog,710) cdivid(nd)
  710   FORMAT(/,
     1   '  Mdainp;',
     1   ' Problem station ',a12,' of the annual diversion ',
     1   ' file (*.dda)',/ 
     1   '          cannot be found. Check order or demand ',/ 
     1   '          type (idvcom) in *.dds')
        Goto 9999

  720   continue
c
c ---------------------------------------------------------
c     Endif for *.dda file
      endif
c
c rrb 99/09/15; Allow instream flow to provide monthly data
c               monisf = 1 annual data only; 2=monthly only; 3=both
C
c _________________________________________________________
C        1x/Run Open INSTREAM FLOW REQUIREMENT
C
c
c     write(nlog,*)  '  Mdainp; monisf= ', monisf
c        
c rrb 00/04/13; Allow ireach to control opening of file *.ifm 
c     if(monisf.ge.2) then
      if(monisf.ge.2 .or. ireach.ge.2) then 
        write(nlog,801)
c       write(nscrn,,801)
 801    format(/,72('_'),/
     1  '  Mdainp; Instream Flow Demand - Monthly (*.ifm) ')
c
c ---------------------------------------------------------

        if(infile.eq.1) then
          ifn=20
          rec256=fileName(ifn)
        endif
c        
c ---------------------------------------------------------       
c
c rrb 2005/12/16; Allow TS file units to override control file units
c       call chekpor(iin, 25, 99, 0, ioptio, numifr, iystr,
c    1               imstr, 0, 17, c, cyr1,maxfn, infile,fpath1,rec256)
        call chekpor(
     1    iin, 25, 99, 0, ioptio, numifr, iystr,
     1    imstr, 0, 17, ffactoM, cyr1,maxfn, 
     1    infile,idummy, nIfmX, fpath1, rec256)
     
        if(idummy.eq.0 .and. abs(ffactoM-ffacto1).gt.small) then
          write(nlog,1640) '*.ifm', ffactoM, ffacto1, ffactoM
          if (iwarnU.eq.1) goto 9997
        endif
        
c       write(nscrn,,*) '  Mdainp; back from checkpor for *.ifm'
c       write(nlog,*) '  Mdainp; back from chekpor for *.ifm', iystr
      endif
c        
c rrb 00/04/13; Set ireach to typical for rest of analysis 
c               now that *.ifm is opened
      if(ireach.eq.2) ireach = 0
      if(ireach.eq.3) ireach = 1

  
C
c _________________________________________________________
C        1x/Run Read INSTREAM FLOW REQUIREMENT - Annual
C
C-------------------------------------------------------------------
C
      write(nlog,107)
      write(nscrn,107)
  107 format(/,72('_'),/
     1 '  Mdainp; Instream flow Demand file - Annual (*.ifa) ')
      close(55)

      if(infile.eq.1) then
        ifn=21
        rec256=fileName(ifn)
      endif
c
c----------------------------------------------------------
c
      call chekpor(iin, 55, 99, 1, ioptio, numifr, iystr, 
     1             imstr, 0, 6, ffactoA, cyr1, maxfn, infile, idummy,
     1             nIfaX, fpath1,rec256)
     
      if(idummy.eq.0 .and. abs(ffactoA-ffacto1).gt.small) then
        write(nlog,1640) '*.ifa', ffactoA, ffacto1, FfactoA
        if (iwarnU.eq.1) goto 9997
      endif
     
      iin2=55
      filena='*.ifa'
c
c rrb 98/11/09; Branch if in baseflow mode
c rrb 01/01/03; Recognize other baseflow options   
c     if (ioptio.eq.1) goto 800 
      if (ibasef.eq.1) goto 800

c
c----------------------------------------------------------
c
c		Read annual IFS (*.ifa) data
      if(monisf.eq.1 .or. monisf.eq.3) then

        if(iouti.eq.1) write(nlog,*) ' Annual (*.ifa) ISF data'
c
c rrb 2006/06/02; Correction; Loop for maximum        
cr	  do nf=1,nisfin

c jhb need to explain this a bit more
c     the idea (i think) is to keep reading lines from the ifa file
c     until an event happens that kicks it out of this loop
c     comment() reads and skips lines that are comments and then
c       it returns when it reaches a EOF (iocode=2)
c         in which case we should jump out of this loop
c         (see code changes below)
c       or an error (iocode=3)
c         in which case we should stop the program
c       or a data line,
c         in which case comment() has backspaced the file
c         and the line is read again below
            do 730 nf=1,maxifr
c
c		Checck for comments in dat
c		Exit if EOF (2) or Error (3)
        call comment(55, nlog, iocode, nchk, 0)
c jhb this is a mistake should jump OUT of the loop
c        if(iocode.eq.2) goto 730
c     jump to newly added 740 continue statement...
        if(iocode.eq.2) goto 740
        if(iocode.eq.3) goto 928
c
        read (55,532,end=760,err=928) cistat,(diverm(i),i=1,12)
          cistat=adjustl(cistat)
c
c----------------------------------------------------------
c
c               Find station associated with this data
              ix=0
              cCallBy='Mdainp *.ifa'
              call stafind(nlog,1,1,numifr,ix,cistat,cifrid,cCallBy)

        if(ix.eq.0 .or. iifcom(ix).ne.2) then
          write(nlog,1311) cistat, 'Annual ', 0, iifcom(ix)
          goto 9999            
        endif  
c
c----------------------------------------------------------
c		Echo input after sifting through appropriate data
              if(iouti.eq.1) then
                write (nlog, 534) nf,cistat,(diverm(i),i=1,12)
              endif
c
c----------------------------------------------------------
c
c               Set flowr to correct station
c               Note, a negative is a forecast
c
c rrb 00/03/16; Carry forecast to end of year
              ifor=0
              forc1=0.0
c
c----------------------------------------------------------
c
              do i=1,12
                IF(diverm(i).GE.-1*small) then
                  flowr(i,ix)=diverm(i)
c
c rrb 2005/09/20; Save for Type 13 (La Plata Compact) and
c                 Rio Grande compact
                  flowrX(i,ix)=diverm(i)
                  forecast(i,ix)=0.0
c
c rrb 00/03/15; Carry forecast to end of year
                  if(ifor.eq.1) then
                    flowr(i,ix) = 0.0
                    forecast(i,ix)=forc1
                  endif
                else
                  flowr(i,ix)=0.0
                  forecast(i,ix)=diverm(i)
                  ifor=1
                  forc1=diverm(i)
c                 write(nlog,*) '  Mdainp; ix, forecast ',ix,forecast(i,ix)
                endif
              end do
c
c----------------------------------------------------------
c
              if(ffactoA.gt.small) then
                do i=1,12
                  flowr(i,ix)=flowr(I,ix)/ffactoA/mthday(I)
                  forecast(i,ix)=forecast(i,ix)/ffactoA/mthday(i)
                end do
              endif
c             endif
 730    continue
c jhb corrected the iocode=2 case...
 740    continue
      endif
c
c
c ---------------------------------------------------------
c
c rrb 98/12/09; Wells
 800  continue 
      close(55)
c
c _________________________________________________________
C        Setp X; 1x/Run Open WELL STRUCTURE DEMAND DATA 
C
C
      if(iwell.gt.0) then
        write(nlog,802)
c       write(nscrn,,802)
 802    format(/,72('_'),/
     1  '  Mdainp; Well Structure Demand - Monthly (*.wem) ')

        if(infile.eq.1) then
          ifn=22
          rec256=fileName(ifn)
        endif
c
c ---------------------------------------------------------
c
c rrb 2005/12/16; Allow TS file units to override control file units
c       call chekpor(iin, 9, 99, 0, ioptio, numdivw, iystr,
c     1               imstr, 0, 10, c, cyr1, maxfn, infile,fpath1,rec256)
        call chekpor(iin, 9, 99, 0, ioptio, numdivw, iystr,
     1               imstr, 0, 10, wfacto, cyr1, maxfn, infile,idummy,
     1               nWemX, fpath1, rec256)
c
c ---------------------------------------------------------
     
        if(idummy.eq.0 .and. abs(wfacto-wfacto1).gt.small) then
          write(nlog,1640) '*.wem', wfacto, wfacto1, wfacto
          if (iwarnU.eq.1) goto 9997
        endif
     
c       write(nscrn,,*) '  Mdainp; back from checkpor for *.wem'
c       write(nlog,*) '  Mdainp; back from chekpor for *.wem', iystr
      endif
C
c _________________________________________________________
C        Step X; 1x/Run Open and READ IN RETURN FLOW DELAY TABLES
C
c rrb 98/12/09; wells
c 800 write(nlog,108)
      write(nlog,108)
c     write(nscrn,,108)
  108   format(/,72('_'),/,
     1     '  Mdainp; Delay Table File (*.dly or *.urd) ')
      close(55)
      iin2=iin
      inDly=0
      filena(1:2) ='-1'
c
c ---------------------------------------------------------
      
      if(infile.eq.1) then
        ifn=23
        rec256=fileName(ifn)
        filena=rec256(1:72)
      else
        filena=filenc
        READ (IIN,'(a256)',end=926,err=928) FILENA
      endif
      if(filena(1:2).ne.'-1') inDly=1
c
c rrb 99/05/20
c ---------------------------------------------------------
c
c		Skip *.dly or *.urm read if not provided
      if(inDly.eq.0) then
        write(nlog,*) '  Mdainp; Warning no delay (*.dly or *.urd) ',
     1    'file provided'
        goto 880
      endif
      
      call putpath(maxfn, filena, fpath1)
      iin2=55
      open(55, file=filena,status='old',err=9997)

c
      write(nlog,'(5x, a256)') filena
      call skipn(55)
c
c rrb 12/18/94; Additional output
      ctot = 0.0
c
c rrb 00/02/08; Revise to handle array concerns better
c     MAXDLZ=MAXDLY+1                       
      maxdlz=maxdly
      ndlymx = 0
c
c ---------------------------------------------------------
c rrb 01/02/01; Allow return and delay data to be a fraction
c     
      if(interv.eq.-100) then
        cfac=float(iabs(interv))
c
c rrb 2021/04/18; Compiler warning
cx      cfac=amax1(cfac, 100.0)
        cfac=max(cfac, 100.0)
      else
        cfac=1.0
      endif
c
c     write(nlog,*) '  Mdainp; cfac = ', cfac      
c ---------------------------------------------------------
c
      DO 840 IDL=1,MAXDLZ
 842   read(55,*,end=880,err=880) rec12
       if(rec12(1:1).eq.'#') goto 842          

       backspace(55)

c
c rrb 03/18/96; make number of return intervals a variable
c rrb 05/28/98; allow return id to not be the array counter
       if(interv.gt.0) then
c        read (55,*,end=880,err=928) idly,(dlyrat(i,idl),i=1,interv)
         read (55,*,end=880,err=880)
     1     irtnid(idl),(dlyrat(i,idl),i=1,interv)
     
         if(ioutURM .eq.1) then
           write(nlog,*) '  Mdainp; ', 
     1     irtnid(idl), (dlyrat(i,idl), i=1,ndly(idl))        
         endif  
         
         if(ioutURM .eq.2) then
           write(nlog,*) '  Mdainp; ', irtnid(idl)
         endif
c
c rrb 99/10/16; Check for blanks
         if(irtnid(idl).eq.0) goto 880
         ndly(idl) = interv
       else
c
c
c ---------------------------------------------------------
c rrb 99/08/26; character ID
         if(interv.ne.-999) then
           read (55,*,end=880,err=880)
     1      irtnid(idl),ndly(idl), (dlyrat(i,idl),i=1,ndly(idl))
     
            if(ioutURM .eq.1) then
              write(nlog,*) '  Mdainp; ', idl, irtnid(idl), ndly(idl),
     1        (dlyrat(i,idl), i=1,ndly(idl))        
            endif
            
            if(ioutURM .eq.2) then
              write(nlog,*) '  Mdainp; ', idl, irtnid(idl), ndly(idl)
            endif
            
c
c rrb 99/10/16; Check for blanks
            if(irtnid(idl).eq.0) goto 880
          else
            read (55,829,end=880,err=880)
     1       cirtnid(idl),ndly(idl), (dlyrat(i,idl),i=1,ndly(idl))
     
           if(ioutURM .eq.1) then
             write(nlog,*) '  Mdainp; ', idl, cirtnid(idl),ndly(idl),
     1       (dlyrat(i,idl), i=1,ndly(idl))        
           endif
     
     
           if(ioutURM .eq.2) then
             write(nlog,*) '  Mdainp; ', idl, cirtnid(idl),ndly(idl)
           endif
c
c rrb 2006/03/20; Adjust character string to left     
           cirtnid(idl)=adjustl(cirtnid(idl))
c
c rrb 99/10/16; allow to work for character ID's
           irtnid(idl) = idl
c
c rrb 99/10/16; Check for blanks
           if(cirtnid(idl).eq.'            ') goto 880
         endif
       endif
c
c ---------------------------------------------------------
c
c
c rrb 2021/04/18; Compiler warning
cx     ndlymx=amax0(ndlymx,ndly(idl))
       ndlymx=max(ndlymx,ndly(idl))
c
c rrb 05/28/98; allow return id to not be the array counter
c       IF(IDLY.LE.0.OR.IDLY.GT.MAXDLY) GO TO 860
       IF(ndly(idl).gt.maxdlm) GO TO 860
c
c rrb 2021/04/18; Compiler warning
cx     ndlymx=amax0(ndlymx,ndly(idl))
       ndlymx=max(ndlymx,ndly(idl))
C
c rrb 05/28/98; allow return id to not be the array counter
c       IDLORD(IDLY)=IDL
c
c rrb 94/11/15 I/O check the total pattern equals 100%
c rrb 96/03/18 Make number of return intervals a variable
c
c ---------------------------------------------------------
c               Sum total pattern
       dlytot(idl) = 0.0
       do i=1,ndly(idl)
c
c rrb 99/11/05; Temporary Fix to get %
c rrb 01/02/01 Allow data to be entered by a fraction 
         dlyrat(i,idl)=dlyrat(i,idl) * cfac
         dlytot(idl) = dlytot(idl) + dlyrat(i,idl)
       end do
c
c
c ---------------------------------------------------------
c
  840 CONTINUE
C
      write(nlog,850) maxdly
      Goto 9999
C
  880 continue
c     NUMDLY=IDL-1,0
c
c rrb 2021/04/18; Compiler warning
cx    numdly=amax0(IDL-1,0)
      numdly=max(IDL-1,0)
      
      write(nlog,*) ' ' 
      write(Nlog,799) numdly
 799  format('  Mdainp; Number of delay tables read = ', i5)
      close(55)
c
c ---------------------------------------------------------
c               Write a summary of delay data to *.log
      if(ichk.eq.7) then

        nsum=0
        write(nlog,881)
        do idl=1,numdly
          nsum=nsum+ndly(idl)
c
c rrb; 99/08/27; Character ID
         if(interv.ne.-999) then
           write(nlog,'(i8,5x,i8,i14,1x,f10.6)')  idl, irtnid(idl),
     1                  ndly(idl), dlytot(idl)
         else
           write(nlog,'(i8,1x,a12,i14,1x,f10.6)') idl, cirtnid(idl),
     1                ndly(idl), dlytot(idl)
         endif
       end do

        write(nlog,882) nsum
 882    format(8x, 1x, 'Total       ', i14) 
      endif
      close(55)
c
c
c ---------------------------------------------------------
c               Check that we have a return table (irtnid) for
c               each one assigned to a diversion (irtndl)
c rrb 04/10/15; Identify all missing delay ID's before exiting
      iproblem=0
      do n=1,numrtn
        ifound=0
c
c rrb 99/08/26; character ID
        if(interv.ne.-999) then
          do idl=1,numdly
            if(irtndl(n).eq.irtnid(idl)) ifound=idl
          end do
        else
          do idl=1,numdly
            if(cirtndl(n).eq.cirtnid(idl)) ifound=idl
          end do
        endif 
cb
c ---------------------------------------------------------
c               !!!! From here on, the return flow pointer
c               associated with a structure (irtndl(n)) points
c               to the # of the table read (a pointer) not it's id
        if(ifound.ne.0) then
          irtndl(n)=ifound
        else

c
c rrb 99/08/26; character ID
         if(interv.ne.-999) then
           write(nlog,885) 'Diversion Return', irtndl(n)
c rrb 04/10/15; Identify all missing delay ID's before exiting
c           goto 9999
           iproblem=1
         else
           write(nlog,885) 'Diversion Return', cirtndl(n)
c rrb 04/10/15; Identify all missing delay ID's before exiting
c           goto 9999
           iproblem=1
        endif
      endif
      end do

c
c ---------------------------------------------------------
c rrb 98/12/30; Wells; Check and reassign well return tables "irtndlw"
c               to the table counter (idl)
      do n=1,numrtnw
        ifound=0
        do idl=1,numdly
          if(irtndlw(n).eq.irtnid(idl)) ifound=idl
        end do
        
        if(ifound.ne.0) then
          irtndlw(n)=ifound
        else
          write(nlog,885) 'Well Return     ', irtndlw(n)
c rrb 04/10/15;	  
c         goto 9999
          iproblem=1
        endif
      end do
c      
c ---------------------------------------------------------
c rrb 98/12/30; Wells; Check and reassign well delay tables "irtndlw2"
c               to the table counter (idl)
      do n=1,numrtnw2
        ifound=0
        do idl=1,numdly
          if(irtndlw2(n).eq.irtnid(idl)) ifound=idl
        end do
       
        if(ifound.ne.0) then
          irtndlw2(n)=ifound
        else
          write(nlog,885) 'Well Depletion  ',irtndlw2(n)
c rrb 04/10/15;
c         goto 9999
        iproblem=1
      endif
      end do
      
      
      
c      
c ---------------------------------------------------------
c rrb 2006/10/17; Reservoirs Check and reassign reservoir delay tables 
c                 "irtndlRP" to the table counter (idl)

      do n=1,numrtnRP
        ifound=0
        do idl=1,numdly
          if(irtndlRP(n).eq.irtnid(idl)) ifound=idl
        end do
        
        if(ifound.ne.0) then
          irtndlRP(n)=ifound
        else
          write(nlog,885) 'Reservoir Return', irtndlRP(n)
c rrb 04/10/15;
c         goto 9999
          iproblem=1
        endif
      end do
c
c ---------------------------------------------------------
c rrb 2006/12/27; Check and reassign plan delay tables "iprf"
c               to the table counter (idl)
c
      do n=1,numrtnPP
c
c rrb 2007/08/22; Allow 0 to indicate not used and -1 to indicate
c		  provided with operating rule      
c		  Use Plan return flow data, not iprf() from plan file
cx      if(iprf(n).eq.-1) then
          ifound=0
          do idl=1,numdly
            if(irtndlPP(n).eq.irtnid(idl)) ifound=idl
          end do  
          
           if(ifound.ne.0) then
             irtndlPP(n)=ifound
           else
             write(nlog,885) 'Plan Return     ', irtndlPP(n)
             iproblem=1
        endif  
      end do  
      
c
c _________________________________________________________
c
c		Step 18; Detailed Check for Plan Return File
      if(ioutPrf.eq.1) then
        write(nlog,14300)
        j2=0
        do np=1,nPlan
          jb=nrtnPP(np)
          je=nrtnPP(np+1)-1
          do j=jb,je
            j2=j2+1
            is = irnstaPP(j)
            write(nlog,14320) j2, pid(np), np, jb, je, j, 
     1        cstaid(is), pcttotPP(j), irtndlPP(j)            
          end do
        end do    
      endif   
14300 format(/,72('_'),/
     1 '   MdainP;',/
     1 '      J2 ID                 Np      jb      je       j',
     1 ' cstaidX      pcttotX irtndlX',/
     1 ' _______ ____________  _______ _______ _______ _______',
     1 ' ____________ _______ _______')
      
14320 format(i8, 1x,a12,1x, 4i8, 1x,a12, f8.0, i8)               
      
c
c ---------------------------------------------------------
c rrb 04/10/15; Exit if a problem with delay ID's
      if(iproblem.eq.1) goto 9999      
      
c
c ---------------------------------------------------------
c rrb 99/10/05; If a monthly model
c               Calculate div. loss, well loss & depletion salvage 
      if(iday.eq.0) then
        call Closs(0)
      endif
C
c _________________________________________________________
C        Step X; 1x/Run Reservoir min pool and target - monthly 
c        (*.tam (*.tar))
C
C
      MUNMIN=0
c
      munmin=munmin
C

cx  890 write(nlog,109)
cx     write(nscrn,,109)
cx 109  format(/,72('_'),/
cx     1 '  Mdainp; Reservoir Target Content File (*.tam (*.tar)) ')
cx
      nc = 0
      if(nrsact.eq.0 .or. numres.eq.0) nc = 1

      if(infile.eq.1) then
        ifn=24
        rec256=fileName(ifn)
      endif
c      
c ---------------------------------------------------------     
c
c rrb 2005/12/16; Allow TS file units to override control file units
c     call chekpor(iin, 77, 99, nc, ioptio, numres, iystr, 
c    1             imstr, 0, 7, c, cyr1, maxfn, infile, fpath1,rec256)
      call chekpor(iin, 24, 99, nc, ioptio, numres, iystr, 
     1             imstr, 0, 7, cfacto, cyr1, maxfn, infile,idummy,
     1             nTamX, fpath1,rec256)
c
c rrb 2006/03/02; Treat all conversion factors the same
      if(cfacto.gt.small) then     
        if(idummy.eq.0 .and. abs(cfacto-cfacto1).gt.small) then
          write(nlog,1640) '*.tam', cfacto, cfacto1, cfacto
          if (iwarnU.eq.1) goto 9997
        endif  
      endif
     
c
c ---------------------------------------------------------
c rrb 10/23/95  Check and allow the target file contains min and max 
c               or just max
      iin2=24
      filena='*.tam'
      itarx=0
c
c rrb 98/11/07; Branch if in one of several baseflow modes
      if(ibasef.ne.1) then
        if(nc.eq.0) then
c         itarx = 0
          read (24,952,end=1570,err=928) ityr, cistat
c
c rrb 2006/03/20; Adjust character string to left     
          cistat=adjustl(cistat)
c         write(nlog,*) '  Mdainp; '
c         write(nlog,952) ityr, cistat

          read (24,952,end=1570,err=928) ityr, cista2
c
c rrb 2006/03/20; Adjust character string to left     
          cista2=adjustl(cista2)

c         write(nlog,952) ityr, cistat
          backspace (24)
          backspace (24)
          if (cistat.ne.cista2) itarx = 1
        endif
      endif
C
c _________________________________________________________
C        Step X; 1x/Run Open SJRIP Sediment file (*.sjr) 
c
c rrb 00/11/11; SJRIP Sediment file (*.sjr) provided
      if(isjrip.ne.0) then
        iin2=22
        filena='*.sjr'
       
        write(nlog,805)
c      write(nscrn,,805)
 805   format(/,'  Mdainp; Annual SJRIP Sediment File (*.sjr) ')
c
c rrb 00/12/28; Set ioptio = 0 to open for all run types (baseflow, 
c               simulate, etc.)

        if(infile.eq.1) then
          ifn=30
          rec256=fileName(ifn)
        endif
c      
c ---------------------------------------------------------
c
c rrb 2005/12/16; Allow TS file units to override control file units
c		Note No update file contains only on/off information
        call chekpor(iin, 15, 99, 0,     0, 1, iystr,
     1                 imstr, 0, 20, c, cyr1, maxfn,infile,idummy,
     1                 nSjrX, fpath1,rec256)
c
c               Open output file
        if(isjrip.eq.1) then
          call namext(maxfn, filenc, 'xsj', filena)
          open(22, file=filena, status='unknown')
          call outtop(22, 1, 39)
        endif

      endif

C
c _________________________________________________________
C        1x/Run Open TIME SERIES FILE (*.tsp) or (*.ipy) 
c rrb 00/11/11; Annual time series file (*.tsp)or (*.ipy) provided
      if(itsfile.ne.0) then
        write(nlog,803)
c       write(nscrn,,803)
 803    format(/,72('_'),/
     1  '  Mdainp; Annual Time Series File (*.ipy or *.tsp) ')
c
c		Note 31 is the file name, 10 is the file number,
c		ityp=file type in Chekts
        ityp=18
        rec256=fileName(31)
c
c rrb 2011/04/03; Warn if itifile is set to expect a *.ipy file
c                 but no file is provided
        call chekpor(
     1  iin, 10, 99, 0,     0, 1, iystr,
     1  imstr, 0, ityp, c, cyr1, maxfn,
     1  infile,idummy, nIpyX, fpath1,rec256)  
     
c
c ---------------------------------------------------------
c rrb 2011/04/03; Warn if variable itifile is set to expect a *.ipy 
c                 file but no file is provided or it is set to a 
c                 value that (may be OK) works but is not documented
        iok=1
        if(itsfile.eq.-1 .or. itsfile.eq.1 .or.
     1     itsfile.eq.2  .or. itsfile.eq.10) iok=0
        if(iok.eq.1 .or. idummy.eq.1) goto 1650
c
c ---------------------------------------------------------
c		Check for old file format if not specified (nIpyX=0)
c		Note 31 (itype) is the file # in GetFn
        if(nIpyX.eq.0) then
          itype=31
c
c rrb 2008/12/01; Correction checking file 10      
c         inX=iin2
          inX=10
          call ChkVer(nlog, inX, itype, nIpyX, filena) 
        endif  
      endif
c
c _________________________________________________________
C        1x/Run Open Irrigation Water Requirement File (*.ddc or *.iwr) 
C               Note used for maximum efficiency cals only, not demand
C
C
c rrb 00/12/04; Expect *.ddc for variable efficiency
      if(ieffmax.ne.0) then
        write(nlog,804)
c       write(nscrn,,804)
 804    format(/,72('_'),/,'  Mdainp; IWR File (*.ddc) ')
c
c rrb 00/12/28; Set ioptio = 0 to open for all run types (baseflow, 
c               simulate, etc.)

          if(infile.eq.1) then
            ifn=32
            rec256=fileName(ifn)
          endif
c
c rrb 2005/12/16; Allow TS file units to override control file units
c       call chekpor(iin, 14, 99, 0,      0, 1, iystr,
c    1                imstr,  0, 19, c, cyr1, maxfn,
c    1                infile, fpath1, rec256)
        call chekpor(iin, 14, 99, 0,      0, 1, iystr,
     1                imstr, 0, 19, dfactoI,cyr1,maxfn,infile,idummy,
     1                nDdcX, fpath1, rec256)
     
        if(idummy.eq.0 .and. abs(dfactoI-dfacto1).gt.small) then 
          write(nlog,1640) '*.ddc', dfactoI, dfacto1, dfactoI
          if (iwarnU.eq.1) goto 9997
        endif  
      endif
C
c _________________________________________________________
C        1x/Run Open Soil Moisture Parameter File (*.par or *.str)
c               Note open and read here 1x/simulation for convience
c               Set using area from *.dds and *.wes and reset
c               below if variable area is read in.
C
C
c			Set IstateCU = 0 GW can have soil moisture
c					1 GW does not have a soil moisture
      istateCu=1
      if(ioutSM.eq.1) write(nlog,*) '  Mdainp; isoil = ', isoil
      if(ichk.eq.94) write(nlog,*)'  Mdainp; isoil = ', isoil
      
      isoilfn=0
      if(isoil.ne.0) then
        iin2=55
        filena='*.str'
      
        iin2=iin

        if(infile.eq.1) then
          ifn=33
          rec256=fileName(ifn)
          filena=rec256(1:72)
c
c rrb 2004/08/23; Allow StateCU structure file to be read for AWC data'
c	        if the *.par file is blank          
          if(filena(1:2).ne.'-1') then
            isoilfn=1
            write(nlog,806) 
c	          write(nscrn,,806)
 806        format(/,72('_'),/
     1      '  Mdainp; Soil Moisture Parameter File (format 1)*.par ')
          else
            isoilfn=2
            ifn=50
            rec256=fileName(ifn)
            filena=rec256(1:72)
            write(nlog,807) 
c	          write(nscrn,,807)
 807        format(/,72('_'),/
     1     '  Mdainp; Soil Moisture Parameter File (format 2) *.str')
          endif  
        else    
          READ (IIN,'(a256)',end=926,err=928) FILENA
        endif
      endif
c
      do nd=1,maxdiv
        awcr(nd)=0.0
        awcr1(nd)=0.0
        soils(nd)=0.0
      end do
c
      do nw=1,maxdivw
        awcrw(nw)=0.0
        awcrw1(nw)=0.0
        soilsw(nw)=0.0
      end do
cr   endif

      if(isoil.eq.1) then
        close(55)

        iin2=55
        call putpath(maxfn, filena, fpath1)
        open(55, file=filena, status='old',err=928)
c
c               Print to log file and skip header
        write(nlog,'(5x, a256)') filena
        call skipn(55)
c        
c               Read and process soil data 1x/year
        if(ioutsm.eq.1) then
          write(nlog,*) ' '
          write(nlog,*) ' Mdainp; numdiv, numdivw, isoilfn',
     1              numdiv, numdivw, isoilfn
          
      endif

c jhb 2014/06/26 make the do loop limit arbitrarily large (5000)
c                so it always works no matter how many records are
c                in the IPY, STR, DDC files.  This is OK, because
c                there is code below to jump out of this loop when the
c                last record is read (the end of file is reached)
c      do i=1,numdiv+numdivw
      do i=1,5000
c
c rrb 2004/08/23; Allow a StateCu structure file to provdide AWC data			
c		Note isoilfn=1 for *.par and isoilfn=2 for *.str
        if(isoilfn.eq.1) then
          read(55,'(a12, 1x, f6.1)',end=940,err=940) cistat, awcrx
        else
c
c		Read a *.str file	  
          read(55,'(a12, 71x,i4,f8.0)',end=940,err=940)
     1    cistat,ncli,awcrx
          if(ioutSM.eq.1) then
            write(nlog,*) ' Mdainp; *.str file'
            write(nlog,'(8x, i5, 1x, a12, 71x,i4,f8.0)')
     1      i, cistat,ncli,awcrx
          endif
c
c rrb 2006/05/09; Move from below
          if(cistat.eq.'            ') goto 940
     
cr          write(nlog,*) '  Mdainp; cistat,ncli, awcrx ', 
cr   1        cistat, ncli, awcrx

          do ic=1,ncli
c jhb 2016/06/26 fix this - some station ids are not integers...
c           read(55,*,end=940,err=940) ix
            read(55,*,end=940,err=940) rec256x
          end do
        endif
c
c rrb 2006/03/20; Adjust character string to left     
        cistat=adjustl(cistat)

cr        write(nlog,*)  '  Mdainp; cistat, awcrx ', cistat, awcrx
        if(cistat.eq.'            ') goto 940
c
c rrb 00/12/04; Find diversion station associated with this data
        ifound=0

        cCallBy='Mdainp D.str'
        call stafind(nlog,1,3,numdiv,ix,cistat,cdivid,cCallBy)
        if(ix.ne.0) then
          ifound=1
          awcr(ix)=awcrx * area(ix) * soild
          awcr1(ix)=awcrx
          if(ichk.eq.92) then
            write(nlog,*) '  Mdainp; ix, cdivid, area, awcr'
            write(nlog,*) '     ',ix, cdivid(ix), area(ix), awcr(ix)
          endif
        endif
c
c               IF necessary, Find well station 
        if(ifound.eq.0) then
          cCallBy='Mdainp W.str'
          call stafind(nlog,1,6,numdivw,ix,cistat,cdividw,cCallBy)
          if(ix.ne.0) then
            ifound=1
            awcrw(ix)=awcrx * areaw(ix) * soild
            awcrw1(ix)=awcrx
c
c rrb 2009/04/29; 	Do not include soil moisture on well only lands
c			to coincide with StateCU
            if(iStateCU.eq.1) then
              awcrw(ix)=0.0
              awcrw1(ix)=0.0
            endif
            if(ichk.eq.92) then
              write(nlog,*) '  Mdainp; Well     , ix, cdividw, awcr ',
     1        ix, cdividw(ix), areaw(ix), awcrw(ix)
            endif
          endif
        endif
      end do
c
c _________________________________________________________
c               1x/run Set Initial Soil Storage (soils) af at 50% and
c               Warn if soil data is not found for diversions

 940    continue
        if(ichk.eq.94) write(nlog,*)'  Mdainp; Set Diversion soilM'
        
        do nd=1,numdiv
          if(ioutSM.eq.1) then
            write(nlog,*) ' Mdainp; nd, id, acwr(nd)'          
            write(nlog,'(i5,1x,a12,20f10.2)') nd, cdivid(nd),awcr(nd) 
          endif

          if(awcr(nd).le.small) then
cr          awcr(nd) = 0.0

c rrb 2007/11/16; Warn only if acres > 0
            if(area(nd).gt.small) then
              if(ipIpy.eq.0) then
                rec40='Annual Time Series File (*.ipy or *.tsp)'
                write(nlog,1281) iyr, rec40
                ipIpy=1
              endif  
              iprints=iprints+1
              if(iprints.eq.1) write(nchk,1636)
              write(nchk,1639) iprints, -1, cdivid(nd), 'Div ',
     1        divnam1(nd), awcr(nd), area(nd)
            endif
          endif
c
c rrb 2006/09/18; Test initial soil moisture = 0
          soils(nd)=awcr(nd) * 0.5
cx        soils(nd)=awcr(nd) * 0.2
cx        write(nlog,*) ' Mdainp; Warning initial Soil M = 0.2'
cx        write(nscrn,*) ' Mdainp; Warning initial Soil M = 0.2'
c
          if(ioutSM.eq.2) write(nlog,1324)
     1    'Diversion', idyr, nd, cdivid(nd),
     1    area(nd), awcr(nd), soils(nd)
        end do
c
c _________________________________________________________
c               1x/Run Set Initial Soil Storage (soilsw) af at 50% and
c               Warn if soil data is not found for well only

        if(ichk.eq.94) write(nlog,*)'  Mdainp; Set Well soilM'
        do nw=1,numdivw
          nd=idivcow2(nw)
cr        write(nlog,*) '  Mdainp; nw, nd,', nw, nd
          if(nd.eq.0) then
            if(awcrw(nw).le.small) then
            awcrw(nw) = 0.0
c
c rrb 2007/11/16; Warn only if acres > 0
            if(areaw(nw).gt.small) then
              iprints=iprints+1
              if(iprints.eq.1) write(nchk,1636)
              write(nchk,1639) iprints,-1, cdividw(nw), 'Well',
     1            divnamw1(nw), awcrw(nw), areaw(nw)
            endif
          endif

          soilsw(nw)=awcrw(nw) * 0.5

          if(ioutSM.eq.2) write(nlog,1324)
     1        ' Well Only', idyr, nw,cdividw(nw),
     1         areaw(nw), awcrw(nw), soilsw(nw)
          endif
        end do
        close(55)

      endif
C
c _________________________________________________________
C        1x/Run Open Downstream Call Data file (*.cal) Monthly
c               Note open here and read below for monthly model
c		Open and read in Dayest for Daily model
C
C-------------------------------------------------------------------
C
      if(iday.eq.0) then
cx      write(nlog,809)
cx 809    format(/,72('_'),/
cx     1  '  Mdainp; Downstream Call File (*.cal) Monthly')

        if(infile.eq.1) then
          ifn=51
          rec256=fileName(51)
c
c		Set year type etc.
c              (nann=1 monthly, idayx=2 for downstream call data)
          nann=-1
          idayx=2
          iystr1=iyrmo(1)
          imstr=imomo(1)
          
          if(rec256(1:2).ne.'-1') then
            write(nlog,809)
 809        format(/,72('_'),/
     1      '  Mdainp; Downstream Call File (*.cal) Monthly')
c          
c		Note file has no units			
            call chekpor(iin, 90, 99, nann,      0, 1, iystr1,
     1        imstr, idayx, ifn, c, cyr1,maxfn,infile,idummy,
     1        nCalX, fpath1, rec256)
          endif
        endif

      endif
C
c _________________________________________________________
C        1x/Run Open Rio Grande Spill Data (*.rgs) Monthly
c		Note only works when infile=1
c               Note open here and read below for monthly or daily model
C
      if(infile.eq.1) then

cx	write(nlog,820)
cx 820    format(/,72('_'),/
cx     1    '  Mdainp; Rio Grande Spill file (*.rgs) Monthly')

        ifn=52
        rec256=fileName(61)
c
c		Set year type etc.
c              (nann=0 monthly, idayx=0 for monthly data)
        nann=0
        idayx=0
        iystr1=iyrmo(1)
        imstr=imomo(1)
        
        if(rec256(1:2).ne.'-1') then
          write(nlog,820)
c	        write(nscrn,820)
 820      format(/,72('_'),/
     1    '  Mdainp; Rio Grande Spill file (*.rgs) Monthly')
        
c
          call chekpor(iin, 61, 99, 0,      0, 1, iystr,
     1                imstr,  0, ifn, c, cyr1, maxfn,infile,idummy,
     1                nRgsX, fpath1, rec256)
        endif

      endif
C
c _________________________________________________________
C        1x/Run Open Rio Grande Forecast (*.rgf) Monthly
c		            Note only works when infile=1
c               Note open here and read below for monthly or daily model
C
C
      if(infile.eq.1) then

cx	write(nlog,810)
cx 810    format(/,72('_'),/
cx     1    '  Mdainp; Rio Grande Forecast file (*.rgf) Monthly')

        ifn=53
        rec256=fileName(64)
c
c		Set year type etc.
c              (nann=0 monthly, idayx=0 for monthly data)
        nann=0
        idayx=0
        iystr1=iyrmo(1)
        imstr=imomo(1)
        
        if(rec256(1:2).ne.'-1') then        
          write(nlog,810)
 810      format(/,72('_'),/
     1    '  Mdainp; Rio Grande Forecast file (*.rgf) Monthly')
        
c
c rrb 2005/12/16; Allow TS file units to override control file units
c		Note no update; file has no units
c		Note chekts knows file 52 is RioGrande_Spill_Monthly
          call chekpor(iin, 64, 99, 0,      0, 1, iystr, imstr,
     1                 0, ifn, rgfacto, cyr1, maxfn,infile,idummy,
     1                 nRgsX, fpath1, rec256)
        endif
      endif  
C
c _________________________________________________________
C rrb 2018/10/14; JMartin Inflow
C      1x/Run Open JMartin (*.jmm) Monthly
c		            Note only works when infile=1
c               Note open here and read below for monthly or daily model
C
C
      if(infile.eq.1) then
        ifn=88
        rec256=fileName(88)
        write(nlog,*) ' Mdainp; rec256 = ', rec256
        ityp=88
c
c		Set year type etc.
c              (nann=0 monthly, idayx=0 for monthly data)
        nann=0
        idayx=0
        iystr1=iyrmo(1)
        imstr=imomo(1)
        ijm=0
        
        if(rec256(1:2).ne.'-1') then   
          ijm=1     
          write(nlog,812)
 812      format(/,72('_'),/
     1    '  Mdainp; JMartin_Monthly (*.jmm)')   
c      
            call chekpor(
     1        iin, 88, 99, 0, ioptio, ijm, iystr,
     1        imstr, 0, ityp, c,      cyr1, maxfn,
     1        infile,idummy,  njmmX, fpath1,rec256)
        endif
      endif     
c      
c
      RETURN
c
c _________________________________________________________
c		Begin to read data every month      
C
C-------------------------------------------------------------------
C
C------  READ IN MONTHLY PRECIPITATION DATA FROM TAPE 1
C
C-------------------------------------------------------------------
c
  950 if(numpre.eq.0 .or. monppt.eq.1 .or. numres.eq.0) goto 1020
C
      iin2=1
      filena='*.prm'
      DO 970 IPR=1,NUMPRE

      read (1,952,end=1000,err=928) ipyr,cpreid(ipr),(x(i),i=1,12)
c
c rrb 2006/03/20; Adjust character string to left     
      cpreid(ipr)=adjustl(cpreid(ipr))
      
      if(ipyr.ne.iyr) then
        write(nlog,1308) 'Precipitation data (*.prm)',
     1        'N/A',numpre, ipr, ipyr,iyr
        goto 9999
      endif

      if(ipr.gt.maxpre) go to 980
c
c rrb 2006/03/02; Treat all factors consistently
      if(pfacto .gt. small) then                
        do i=1,12
          preprt(i,ipr) = x(i) * pfacto
        end do
      else
        do i=1,12
          preprt(i,ipr) = x(i)
        end do
      endif
  970 CONTINUE
C
      GO TO 1020
C
  980 write(nlog,990) ipr,maxpre
  990 FORMAT(/
     1 '  Mdainp; Problem',/
     1 '          The number of Precipitation stations ',I5,
     1 ' exceeds tha maximum ',I2,
     1 ' in (*.pra or *.prm)')
      goto 9999
C
 1000 write(nlog,1010)
 1010 FORMAT(/
     1 '  Mdainp; Problem. ',/
     1 '          Not enough data in the monthly precipitation ',
     1 'file *.prm')
      goto 9999
C
C-------------------------------------------------------------------
C
C------  READ IN MONTHLY PAN EVAPORATION DATA FROM TAPE 2
C
C-------------------------------------------------------------------
C
c
c rrb 2006/05/02; Correction
c1020 IF(NUMEVA.EQ.0 .OR. MONEVA.NE.0 .or. numres.eq.0) GO TO 1090
 1020 IF(NUMEVA.EQ.0 .OR. moneva.eq.1 .or. numres.eq.0) GO TO 1090
C
      if(ioutEv.eq.1)
     1  write(nlog,*) '  Mdainp; Monthly Evap numeva = ', numeva, iyr
      iin2=2
      filena='*.evm'
      DO 1040 IEV=1,NUMEVA

c
c rrb 12/27/94; I/O Flexability
c     read(2,'(a256)') recin
c     write(nlog,'(a256)') recin
      read (2,952,end=1070,err=928) ieyr, cevaid(iev),(x(i),i=1,12)
      if(ioutEv.eq.1) 
     1  write(nlog,953) iev, ieyr, cevaid(iev),(x(i),i=1,12)
c
c rrb 2006/03/20; Adjust character string to left     
      cevaid(iev)=adjustl(cevaid(iev))
                

      if(ieyr.ne.iyr) then
        write(nlog,1308) 'Evaporation data monthly (*.evm)',
     1        'N/A',numeva, iev, ieyr,iyr
        goto 9999
      endif


      if(iev.gt.maxeva) go to 1050

      if(efacto.gt.small) then
        do i=1,12
          evaprt(i,iev) = x(i) * efacto
        end do    
      else
        do i=1,12
          evaprt(i,iev) = x(i) 
        end do  
      endif

 1040 CONTINUE
C
      GO TO 1090
C
 1050 write(nlog,1060) iev,maxeva
 1060 FORMAT(/
     1 '  Mdainp; Problem'
     1 ' The number of evaporation stations ',I5,
     1 ' exceeds the maximum ',I5, ' in *.eva or *.evm')
      goto 9999
C
 1070 write(nlog,1080)
 1080 FORMAT(/
     1 '  Mdainp; Problem'
     1 ' reading the monthly evaporation file (*.evm)')
      goto 9999
C
C-------------------------------------------------------------------
C
C     READ MONTHLY RUNOFF DATA FROM TAPE 3
C
C-------------------------------------------------------------------
C
 1090 iin2=3
 
      if(ibasef.eq.0) then
        filena='*.rim or *.xbm'
      else
        filena='*.rih'   
      endif 
      
      if(ioutS.eq.1) write(nlog,*)' Mdainp; numrun = ', numrun, iyr

      DO 1130 NPx=1,NUMRUN
C
        read (3,952,end=1590,err=928) iryr,cistat,(runoff(i),i=1,12)
        if(ioutS.eq.1) write(nlog,955) ' Mdainp; ',npx,iryr,iyr,cistat
c
c rrb 2006/03/20; Adjust character string to left     
        cistat=adjustl(cistat)
          
        if(iryr.ne.iyr) then
         if(ibasef.eq.0) then
            write(nlog,1308) 'Base Streamflow (*.rim or *.xbm)',
     1        '(*.ris)',numrun, NPx, iryr,iyr
          else
            write(nlog,1308) 'Historic Streamflow (*.rih)',
     1        '(*.ris)',numrun, NPx, iryr,iyr
          endif
        goto 9999
        endif
c
c _________________________________________________________
c
c rrb 2005/10/07; Allow stream data to be provided in any order
cr	if(cistat.ne.crunid(npx)) go to 1610

        cCallBy='Mdainp *.ris'
        call stafind(nlog,1,0,numrun,ix,cistat,crunid,cCallBy)
        
cr      if(ix.eq.0) goto 1610
        if(ix.eq.0) then
          if(ibasef.eq.0) then        
            write(nlog,1308) 'Base Streamflow (*.rim or *.xbm)',
     1        '(*.ris)',numrun, NPx, iryr,iyr
          else
            write(nlog,1308) 'Historic Streamflow (*.rih)',
     1        '(*.ris)',numrun, NPx, iryr,iyr
          endif
          goto 9999
        endif  
        
c       write(nlog,*) '  Mdainp; Stream npx, ix ', npx, ix
c
c _________________________________________________________
c
c rrb 09/29/97; If total flow is provided (iopflo=1) and
c               not in baseflow mode (ioptio=1) then
c               Check for missing data -999
c rrb 98/11/09; The following is OK for BaseflowX option
c rrb 01/01/03; Recognize other baseflow options
c       if(iopflo.eq.1 .and. ioptio.ne.1) then
        iprintn=0
        if(iopflo.eq.1 .and. ibasef.ne.1) then
          do 1092 im=1,12
c
c               b. Daily capability
            if(iday.eq.0) then
              fac=mthday(im)*factor
            else
              fac=factor
            endif      
            if(abs(runoff(im)+999.0).le.0.01) then
              iprintn=1
              runoff(im)=0.0
            endif
 1092     continue
          if(iprintn.eq.1)then
            write(nlog,1622) iryr, cistat, (runoff(i), i=1,12)
cx          goto 9999
          endif
          
c         write(nlog,1169) iyrmo(12), cistat,
c    1         (runoff(i), i=1,12)          
c1169     format('  Mdainp;', i5,1x,a12, 1x, 20f8.0)        
          
        endif
C
C------  CONVERT RUNOFF DATA FROM ACRE-FEET TO CFS
C
        IF(RFACTO.gt.small) then
          DO 1100 IM=1,12
            RUNOFF(IM)=RUNOFF(IM)/RFACTO/MTHDAY(IM)
 1100     CONTINUE
        endif
C
        DO 1120 IM=1,12
          VIRINP(IM,ix)=RUNOFF(IM)
c
c rrb 00/08/17; Store total flow for use by daily model
         virinpT(im,ix) = runoff(im)
 1120   CONTINUE
C
 1130 CONTINUE
C
c rrb 11/22/95; Data is total flow (iopflo=1) or gain (iopflo=2) 
c              Note option 1 & 9 (baseflow) are always total flow
c     IF(IOPFLO.NE.1) GO TO 1220
c
c rrb 98/11/09; BaseflowX Option
c     if(iopflo.eq.1  .or. ioptio.eq.1) then
      if(iopflo.eq.1  .or. ibasef.eq.1) then
c
c _________________________________________________________
C
C------  CONVERT TOTAL VIRGIN FLOW TO REACH GAIN 
C
c
c rrb 2021/03/20; Compiler Update
cx      DO 1140 NP=1,NUMRUN
        DO 1141 NP=1,NUMRUN
          DO 1140 IM=1,12
c
c rrb 2021/03/20; Compiler Update
cx 1140     DUM(IM,NP)=VIRINP(IM,NP)
            DUM(IM,NP)=VIRINP(IM,NP)
 1140       continue
c
c rrb 2021/03/20; Compiler Update
 1141   continue
cx 
C
        DO 1160 IR=1,NUMRUN
          IRR=IRUDND(IR)
          IF(IRR.EQ.0) GO TO 1160
C
          DO 1150 IM=1,12
            dum1=dum(im,irr)
            DUM(IM,IRR)=DUM(IM,IRR)-VIRINP(IM,IR)
            
            c= RFACTO*MTHDAY(IM)         
c
c		Detailed check for Gain Calcs
cx            if(ioutGx.eq.1) then
cx              if(crunid(irr) .eq.'6400504     ') then  
cx                write(nlog,1149) iyr, im, crunid(irr), dum1*c, 
cx     1	          virinp(im,ir)*c, dum(im,irr)*c
cx 1149           format('  Mdainp;', i5, i5, 1x, a12,20f8.0) 
cx              endif
cx            endif    
 
 1150     continue
C
 1160   CONTINUE
C
       DO 1170 NP=1,NUMRUN
         DO 1168 IM=1,12
           VIRINP(IM,NP)=DUM(IM,NP)
c
c rb 2009/11/25; Detailed gain output
            if(ioutGx.eq.1) then           
              c= RFACTO*MTHDAY(IM)   
              IS=IRUSTA(NP)
              write(nlog,1161) 
     1          '  Mdainp;   ', cstaid(is), 
     1          iyr, im, np, is, virinp(im,np)*c 
            endif    
 1161    format(a12,1x,a12,1x,4i5,f8.0)              
 1168    continue
 
 1170  continue

      endif

C
C-------------------------------------------------------------------
C
C------  READ MONTHLY DIVERSION (*.ddm) FOR CURRENT YEAR FROM file 4
C
C-------------------------------------------------------------------
C
      IF(NDIVIN .EQ.0.OR.MONDIV.EQ.1) GO TO 1320
C
      iin2=4
      if(ibasef.eq.0) then
       filena='*.ddm'
      else
       filena='*.ddh'
      endif
c _________________________________________________________
c
c rrb 2005/10/07; Allow diversion data to be provided in any order
c                 Limit to one user per diversion
c     DO 1270 ND=1,NUMDIV
      if(ichk.eq.94) write(nlog,*)'  Mdainp; reading *.ddm '

      DO 1270 NDx=1,NUMDIV
c      
c rrb 2005/10/07; Limit to one user per diversion    
cr	NUI=NDUSER(NDx)
cr	NUE=NDUSER(NDx+1)-1
cr	IF(NUI.GT.NUE) GO TO 1270
cr	DO 1260 NU=NUI,NUE
cr		
c rrb 2005/10/07; Limit to one user per diversion (Moved below)
cr	  if(idvcom(nu).eq.1 .or. idvcom(nu).eq.3) then
C                 
        ioutd=0
        read(4,952,end=1280,err=928) idyr,cistat,
     1    (diverm(im),im=1,12)
     
        if(ioutd.eq.1) then
          write(nlog,*) ' '
          write(nlog,*) '  Mdainp *.ddm read'
          write(nlog,952) idyr,cistat,(diverm(im),im=1,12)
        endif
        
        if(ioutd.eq.2) then
          write(nlog,*) '  Mdainp *.ddm read ', 
     1     ndx, cistat, cdivid(ndx),idyr,iyr
        endif
c
c rrb 2006/03/20; Adjust character string to left     
        cistat=adjustl(cistat)          

        if(idyr.ne.iyr) then
          if(ibasef.eq.0) then
            write(nlog,1308) 'Diversion Demand (*.ddm)',
     1        '(*.dds or *.dst)',numdiv, NDx, idyr,iyr
          else
            write(nlog,1308) 'Historic Diversion (*.ddh)',
     1        '(*.dds or *.dst)',numdiv, NDx, idyr,iyr
          endif
          goto 9999
        endif
c
c _________________________________________________________
c
c rrb 2005/10/07; Allow diversion data (0) to be provided in any order

        cCallBy='Mdainp *.ddm'
        call stafind(nlog,1,3,numdiv,ix,cistat,cdivid,cCallBy)
        
c       write(nlog,*) '  Mdainp; Diversion ndx, ix ', ndx, ix
        if(ix.eq.0) then
          write(nlog,1310) cistat,'Diversion Demand (*.ddm)',
     1      '(*.dds)',idyr,iyr
          goto 9999
        else
          if(ioutD.eq.3) then
            write(nlog,*) ' Mdainp; Diversion in *.dds = ',ndx,cistat
          endif
        endif  
c
c rrb 2005/10/07; Limit to one user per diversion
        nu=ix
        nd=ix
          
        if(idvcom(nu).eq.1 .or. idvcom(nu).eq.3) then                    
C
          IF(dfacto.gt.small) then
            DO IM=1,12
              DIVERM(IM)=DIVERM(IM)/dfacto/MTHDAY(IM)
            end do
          endif
c
c rrb 2018/12/01         
          if(ioutd.eq.1) then
            write(nlog,*) '  Mdainp *.ddm processing ',dfacto 
            write(nlog,952) idyr,cistat,(diverm(im), im=1,12)
          endif
C
c ---------------------------------------------------------   
c                 Step Imp 1
c rrb 2019/12/29; Negative Diversion (Imports)
c                 Resolve an issue when there is a mixture
c                 of negative demands (imports) and possibly
c                 positive values and/or zero values.
c                 By setting zero values to -0.001 warning if 
          iimp=0
          do im=1,12
            if(diverm(im).lt.-0.001) iimp=1
          end do 
c
C
c ---------------------------------------------------------   
c                 Step Imp 2 At least one negative (import) found         
          if(iimp.eq.1) then
            do im=1,12
              diverm1=diverm(im)
c
C
c ---------------------------------------------------------   
c                 Step Imp 3  Check for a mixture of negatives 
c                             and positives
              if(diverm1.gt.1.0) goto 1325
c
c               Check for a mixture of negatives and zeros
c
c rrb 2020/07/27; Correction; do not set zero values to -0.001 
cx            if(diverm1.gt.-0.001 .and. diverm1.lt.0.001) then
cx              diverm(im) = -0.001
cx            endif
            end do
          endif
C
c ---------------------------------------------------------  
c                 Step Imp 4 Check for imports (negative values)
c                            by month
          DO 1250 IM=1,12
c
c rrb 11/07/95; Allow negative diversions to be imports for
c		            base flow and import calculations
c
C
c ---------------------------------------------------------  
c                 Step Imp 5 Warn imports
c rrb 2019/12/29; Revise to better identify a zero value
cx          if(diverm(im).lt.-0.01) then
            if(diverm(im).lt.-0.0009) then
              if(iwarn(nu).eq.0) then
                if(iwarnDT.eq.0) then
                  write(nchk,1241)
                endif  
                      
                if(ipDdm.eq.0) then
                  rec40='Negative Diversions (*.ddm)'
                  write(nlog,1281) iyr, rec40
                  ipDdm=1                    
                endif
                
                iwarnDT=iwarnDT+1
                iwarn(nu) = 1                      
                c = diverm(im)*dfacto*mthday(im)
                write(nchk,1242) iwarnDT, cistat, idyr, diverm(im),c 
              endif
C
c ---------------------------------------------------------  
c                 Step Imp 6 Set negative demand to import then zero, 
c                            if not in base flow mode (ibasef=0)
c rrb 11/20/95;   Set negative demand to import then zero, 
c                 if not in base flow mode (ibasef=0)
              if(ibasef.ne.1) then
c               write(nlog,1243) diverm(im), 0.0
c
c rrb 05/04/04; Check if part of a plan
                ifound=0
                do ip=1,nplan
                  if(cdivid(ix).eq. pid(ip)) ifound=ip
                end do
c
c rrb 2005/09/16; Not a fatal error, Print a warning
c               if(ifound.eq.0) goto 1305                  
                if(ifound.eq.0) then
                  if(iwarnp.eq.0) write(nlog,1306) cdivid(ix)
                  iwarnp=1
                else  
c
c			                Set import to negative demand and diversion to 0                  
                  PImportX(im,ifound) = -1.0*diverm(im)                  
                  if(ioutx.eq.1) then
                    write(nlog,*)
     1                '  Mdainp; cdivid im, ifound, PImportX = ',
     1                cdivid(ix), im, ifound, 
     1                PImportX(im,ifound)*dfacto*MTHDAY(IM)
                  endif
c
c rrb 2006/04/24; Set imports to zero if NOT in baseflow (ibasef.ne.1)
c	                diverm(im) = 0.0
                endif
               
                diverm(im) = 0.0
c
c               Endif for not in a baseflow mode (ibasef=0)
             endif  
c
c ---------------------------------------------------------
c
c               Endif for a negative diversion (import)
c
            endif
c
c rrb 00/06/16; Allow monthly iwr data
c1250       DIVER(IM,NU)=DIVERM(IM)
 1250     continue
c
c ---------------------------------------------------------
c rrb 00/06/16; Data provided as monthly total demand (idvcom(nu) = 1)
          if(idvcom(nu).eq.1) then
            do im=1,12  
              diverir(im,nu)=diverm(im)*(diveff(im,nu)/100.)
              diver(im,nu)=diverm(im)
            end do
            
            if(ioutd.eq.1) then
              write(nlog,*) '  Mdainp; diver & diverir for nu = ',nu
              write(nlog,*) '  Mdainp;', dfacto, nu, 1, mthday(1), 
     1         diver(1,nu)
              write(nlog,952) idyr,cistat,
     1                  (diver(im,nu)*dfacto*mthday(im),im=1,12)
              write(nlog,952) idyr,cistat,
     1                        (diverir(im,nu)*dfacto*mthday(im),im=1,12)
            endif
          endif
c
C
c ---------------------------------------------------------   
c rb00/06/16; Allow monthly IWR demand data
c rrb 00/06/16; Data provided as monthly IWR (idvcom(nu) = 3)
          if(idvcom(nu).eq.3) then
            do im=1,12  
              if(diveff(im,nu).gt.small) then
                diverir(im,nu)=diverm(im) 
                diver(im,nu)=diverm(im)/(diveff(im,nu)/100.)
              else 
c       
c rrb   00/08/02; Data check
                if(diverm(im).gt. small) then
                  c = dfacto*mthday(im) 
                  write(nlog,757) cdivid(nd), divnam1(nd), 
     1                        iyr, im, diverm(im)*c, diveff(im,nu)
                  goto 9999
                else
                  diverir(im,nu)=0.0 
                  diver(im,nu)=0.0
                endif
              endif
            end do
          endif
C                                                             
c --------------------------------------------------------- 
c Endif for data provided as monthly total demand (idvcom(nu) = 1)
c          
        endif
    
c         write(nlog,*) '  Mdainp *.ddm after efficiency adj'
c         write(nlog,952) idyr,cistat,
c    1                      (diver(im, nu)*dfacto*mthday(im),im=1,12)

c
c rrb 00/06/16 Add here to remove entire section related to Blaney
c              Zero data
      if(idvcom(nu).eq.5) then
        DO IM=1,12
          DIVER (IM,NU)=0.
          diverir(im,nu) = 0.0
        end do
      endif
   

C
C                                                             
c --------------------------------------------------------- 
c End of do loop to read Diversion read (do 1270) 
c1260  CONTINUE  
 1270 CONTINUE
C
      GO TO 1320
C
C
c1350 CONTINUE
C
C-------------------------------------------------------------------
C
C----- READ OVERRIDE DIVERSION DEMAND
C
C-------------------------------------------------------------------
C
c
c rrb 11/29/94 Add Monthly target content data
 1320 continue
      if(numovr.eq.0) goto 1490
c
c rrb 03/27/95; Code Addition, Diversion Override not allowed'
c               when in a base flow mode
c rrb 98/11/09; BaseflowX option
c     if(ioptio.eq.1) then
      if(ibasef.eq.1) then
        write(nlog,*)
     1    '  Mdainp; Problem.',
     2    ' Diversion override not alowed for Base Flow Mode'
        goto 9999
      endif

      iin2=23
      filena='*.ddo'
      
      DO 1460 ID=1,NUMOVR
        read(23,952,end=1470,err=928) idyr,cistat,(diverm(im),im=1,12)
c
c rrb 2006/03/20; Adjust character string to left     
        cistat=adjustl(cistat)
        
        if(idyr.ne.iyr) then
          write(nlog,1308) 'Diversion Override (*.ddo)',
     1        '(*.dds or *.dst)',numovr, id, idyr,iyr     
          goto 9999
        endif
c
c _________________________________________________________
c
c rrb 2005/10/07; Allow diversion data (0) to be provided in any order
 
        cCallBy='Mdainp *.ddo'
        call stafind(nlog,1,3,numdiv,ix,cistat,cdivid,cCallBy)
        
        if(ix.eq.0) goto 1360
c       write(nlog,*) '  Mdainp; Diversion ndx, ix ', ndx, ix



c     DO 1360 ND=1,NUMDIV
c     if(cistat.eq.cdivid(nd)) go to 1380
c1360 CONTINUE
C
cx 1380 IUSE=NDUSER(ND)
cx      IF(IUSE.LE.NDUSER(ix+1)-1) GO TO 1400
cx      write(nlog,1390) cdivid(ix)
c
c rrb 2021/04/18; Compiler warning
cx 1390 FORMAT(/
cx     1 '  Mdainp; Problem.'
cx     1 ' THE STRUCTURE ',a12,' in the diversion override file',
cx     1 ' (*.ddo) DOES NOT HAVE A USER')
cx      goto 9999
c
c
c rrb 2021/04/18; Compiler warning
cx1400 IF(dfacto.LE.small) GO TO 1420
      IF(dfacto.LE.small) GO TO 1420
      DO 1410 I=1,12
c
c rrb 2021/03/20; Compiler Update
cx 1410 DIVERM(I)=DIVERM(I)/dfactoO/MTHDAY(I)
        DIVERM(I)=DIVERM(I)/dfactoO/MTHDAY(I)
 1410 continue
c
c rrb 01/31/95; Code Addition
 1420 continue
c
      DO 1430 I=1,12
c
c rrb 2021/03/20; Compiler Update
cx 1430 DIVER(I,IUSE)=DIVERM(I)
        DIVER(I,IUSE)=DIVERM(I)
 1430 continue
 1460 CONTINUE
c
      goto 1490
C
 1470 write(nlog,1480)
 1480 FORMAT(/
     1 '  Mdainp; Problem.'
     1 ' NOT ENOUGH DATA IN DIVERSION OVERRIDE FILE (.ddo)')
      goto 9999
C
C-------------------------------------------------------------------
C
C------  READ Reservoir min pool and target contents - Monthly (*.tam)
C
C-------------------------------------------------------------------
C
c
 1490 continue
c
c rrb 03/27/95; Code Addition, Separate file for baseflow operation
c rrb 98/11/09; BaseflowX option
c     if(ioptio.eq.1) goto 1560
      if(ibasef.eq.1) goto 1560

      if (iyr.eq.iystr) iter=1

1491  iin2=24

c ---------------------------------------------------------
      filena='*.tam'
      do 1550 nm=1,numres
c
c rrb 2006/11/13; Store last years value for daily mid point calculations
c		Note set value here, before data is read, so that it
c		is set to last years value. Also year 1 is the 
c		value provided in the reservoir station file (*.res)
        if(iyr.eq.iystr) then
          resvol1(nm)=cursto(nm)
        else  
          resvol1(nm)=targetx(12,nm)
        endif 
         
c        write(Nlog,*) 
c     1   ' Mdainp; nr, cursto(nm), targetx(12,nm), resvol1(nm)',
c     1   ' Mdainp; ', nr, cursto(nm), targetx(12,nm), resvol1(nm)

        if (iyr.eq.iystr) then
c
c grb 3-19-97  allow two iterations through this section for first year
c                                                                 
c ---------------------------------------------------------
c rrb 10/23/95; Allow target to be min and max or just max
c               itarx = 0 min & max, = 1 = max only
          if(itarx.eq.0) then
            if (iter.eq.1) then
              read (24,952,end=1570,err=928)
     1          ityr, cistat, (conmin(i,nm),i=1,12)
            endif
            if (iter.eq.2) then
              read (24,952,end=1570,err=928)
     1              ityr, cistat, (conminn(i,nm),i=1,12)
            endif
c
c rrb 2006/03/20; Adjust character string to left     
            cistat=adjustl(cistat)

          else
            do i=1,12
              conmin(i,nm) = 0.0
              conminn(i,nm)=0.0
            end do
          endif
c
c ---------------------------------------------------------
c
          if (iter.eq.1) then
            read (24,952,end=1570,err=928)
     1        ityr, cista2, (targetx(i,nm),i=1,12)
c
c rrb 2006/03/20; Adjust character string to left     
            cista2=adjustl(cista2)     
          endif

          if (iter.eq.2) then
            read (24,952,end=1570,err=928)
     1        ityr, cista2, (targetn(i,nm),i=1,12)
c
c rrb 2006/03/20; Adjust character string to left     
            cista2=adjustl(cista2)
          endif

          if(iter.eq.2.and.ityr.ne.(iyr+1)) then
              write(nlog,*) '  Mdainp; Problem. ',
     1        'Res target data is out of sequence_2 ',cista2,ityr,iyr
            goto 9999
          endif

          if (iter.eq.2) then
c
c ---------------------------------------------------------
c
c rrb 2006/03/01; Make all unit conversions consistent (skip if none)
          if(cfacto.ge.small) then
            do i=1,12
              targetx(i,nm) = cfacto*targetx(i,nm)
              targetn(i,nm) = cfacto*targetn(i,nm)
              conminn(i,nm) = cfacto*conminn(i,nm)
              conmin(i,nm)  = cfacto*conmin(i,nm)
              sjtarget(i,nm)=targetx(i,nm)
            end do
          endif
        endif
      else
c
c ---------------------------------------------------------
c
c		Get data for a year not simulated
        if(cfacto.ge.small) then
          do i=1,12
            targetx(i,nm)=targetn(i,nm)*cfacto
            conmin(i,nm)=conminn(i,nm)*cfacto
            sjtarget(i,nm)=targetx(i,nm)
          end do
        else
          do i=1,12
            targetx(i,nm)=targetn(i,nm)
            conmin(i,nm)=conminn(i,nm)
            sjtarget(i,nm)=targetx(i,nm)
          end do
        endif
c
c ---------------------------------------------------------
c
        if (iyr.eq.iyend) goto 1530
c     
c                 Read min target (itarx=0) or set to 0 (itarx.ne.1)
        if(itarx.eq.0) then
          read (24,952,end=1570,err=928)
     1        ityr, cistat, (conminn(i,nm),i=1,12)
c
c rrb 2021/05/02; Runtime error tracking
          if(ioutTar.eq.1) then
            write(nlog,*)'  Mdainp_1 ', nm, numres,
     1        ityr, cistat, (conminn(i,nm),i=1,12)
          endif

c
c rrb 2006/03/20; Adjust character string to left     
          cistat=adjustl(cistat)
        else
          do i=1,12
            conminn(i,nm)=0.0
          end do
        endif
c
c ---------------------------------------------------------
c
        read (24,952,end=1570,err=928)
     1      ityr, cista2, (targetn(i,nm),i=1,12)
cc
c rrb 2021/05/02; Runtime error tracking
        if(ioutTar.eq.1) then
          write(nlog,*)'  Mdainp_2 ',nm, numres, 
     1        ityr, cistat, (conminn(i,nm),i=1,12)
        endif
     
c rrb 2006/03/20; Adjust character string to left     
        cista2=adjustl(cista2)
             
        if(ityr.ne.(iyr+1)) then
          write(nlog,*) ' '
          write(nlog,*) '  Mdainp; Problem. ',
     1        'Res target data is out of sequence_1 ',cista2,ityr,iyr
          goto 9999
        endif
        if(cfacto.ge.small) then
          do i=1,12
            targetn(i,nm)=cfacto*targetn(i,nm)
            conminn(i,nm)=cfacto*conminn(i,nm)
          end do
        endif
      endif
c ---------------------------------------------------------
      
      if(itarx.eq.0) then
        if(cistat.ne.cista2) write(nlog,1500) cistat
        if(cistat.ne.cista2) write(nscrn,1500) cistat
      else
        cistat = cista2
      endif

c
      if(cistat.eq.blank) goto 1560
c
c ---------------------------------------------------------
c               Set pointer target to reservoir (naxtnr(nm) = nr)
      do 1510 nr=1,numres
        if(cistat.eq.cresid(nr)) then
          nextnr(nm)=nr
          go to 1530
        endif
 1510   continue
c
      write(nlog,1520) cistat
      goto 9999
c
 1530 irsmin(nm)=nextnr(nm)
      itarget(nm)=nextnr(nm)
c
c
c ---------------------------------------------------------
 1550 continue
c
      nummin=numres
      if(iter.eq.1) then
c
c ---------------------------------------------------------
c rrb 99/05/19; Add code to allow 1 year of data to work
        if(iystr.eq.iyend) then
          do nm=1,numres
            do i=1,12
              targetn(i,nm)=targetx(i,nm)
              conminn(i,nm)=conmin(i,nm)
            end do
          end do
          goto 1560
        endif
        iter=2
        goto 1491
      endif

 1560 continue
c
c _________________________________________________________
c
c rrb; 99/09/15; Monthly Instream Flows
c
c               READ INSTREAM FLOW DEMANDS - Monthly

c
c rrb 00/03/06; Skip ISF data if in baseflow or baseflowx mode
c     if(ioptio.eq.1) goto 1562
      if(ibasef.eq.1) goto 1562
c
c ---------------------------------------------------------
c		Read only if demand type is 1 (monthly)
      if(iouti.eq.1) write(nlog,*) ' Mdainp; monisf, nisfinM',
     1  monisf, nisfinM   
     
      if(monisf.ge.2) then
        if(nisfinM.eq.0) goto 1562
c     
        iin2=25
        filena='*.ifm'
        if(iouti.eq.1)
     1    write(nlog,*)'  Mdainp; Monthly (*.ifm) IFS Data'
c
c rrb 2006/06/02; Correction
cr	    do nd=1,nisfin
cr	    IF(iifcom(nd).eq.1) then
      do nd=1,nisfinM
        read (25,951,end=762,err=928) idyr,cistat,
     1        (diverm(im),im=1,12),rgspilx
          cistat=adjustl(cistat) 
c
c ---------------------------------------------------------
        if(iouti.eq.1) then
          write (nlog, 536) nd,idyr,cistat,(diverm(i),i=1,12)
        endif
c
c ---------------------------------------------------------
c
        if(idyr.ne.iyr) then
          write(nlog,1308) 'Instream Demand (*.ifm)',
     1        '(*.ifs)',nisfinM, nd, idyr,iyr
          goto 9999
        endif
c
c ---------------------------------------------------------
C
c               Adjust units
        if(ffactoM.ge.small) then
          do im=1,12
            DIVERM(IM)=DIVERM(IM)/ffactoM/MTHDAY(IM)
          end do
        endif
c
c ---------------------------------------------------------
c
c rrb 2005/10/07; Allow ISF data (0) to be provided in any order

        cCallBy='Mdainp *.ifm'
        call stafind(nlog,1,1,numifr,ix,cistat,cifrid,cCallBy)

        if(ix.eq.0 .or. iifcom(ix).ne.1) then
          write(nlog,1311) cistat, 'Monthly ', iyr, iifcom(ix)
          goto 9999
        endif
c
c ---------------------------------------------------------
c rrb 2000/10/23; Handle spill for the Rio Grande
        rgspill(ix)=rgspilx
c
c
c ---------------------------------------------------------
c
c rrb 00/03/16; Carry forecast to end of year
        ifor=0
        forc1=0.0
    
        do i=1,12
          IF(diverm(i).GE.-1*small) then
            flowr(i,ix)=diverm(i)
c
c rrb 2005/09/20; Save for Type 13 (La Plata Compact) use
            flowrX(i,ix)=diverm(i)
            forecast(i,ix)=0.0
c
c rrb 00/03/15; Carry forecast to end of year
            if(ifor.eq.1) then
              flowr(i,ix) = 0.0
              forecast(i,ix)=forc1 /ffactoM/MTHDAY(i)
            endif
          else
            flowr(i,ix)=0.0
            forecast(i,ix)=diverm(i)
c
c rrb 00/03/15; Carry forecast to end of year 
            ifor=1
            forc1=diverm(i) *ffactoM*MTHDAY(I)
c                  write(nlog,*) '  Mdainp; ix, forecast ',
c    1               ix,forecast(i,ix)
         endif
       end do
c
c ---------------------------------------------------------
c               End ISF structure loop
       end do
c
c ---------------------------------------------------------
c               End Monthly ISF option
      endif
      
c
c ______________________________________________________________
c               Step X; Read Rio Grande Spill Data
c rrb 00/11/11; 
c 
 1562 continue
      ifn=61
      rec256=fileName(ifn)
      rec72=rec256(1:72)
      if(rec72(1:2).ne.'-1') then
        iin2=10
        filena = '*.rgs'
c
c		Loop for two RGDSS compact stations 
       do irg=1,nisfinM
         read (61,*,end=1286,err=928) idyr,cistat,
     1                                    (diverm(im),im=1,12)
          cistat=adjustl(cistat)     
c
c _________________________________________________________
c
          cCallBy='Mdainp *.rgs'
          call stafind(nlog,1,1,numifr,ix,cistat,cifrid,cCallBy)
          if(ix.eq.0) goto 1304
          
c        
c ---------------------------------------------------------
c     
          if(ioutRgS.eq.1)
     1      write(nlog,*) '  Mdainp; Rio Grande',
     1      idyr, cistat, (diverm(im),im=1,12)
c
c rrb 2006/03/20; Adjust character string to left     
c        
c ---------------------------------------------------------
            if(idyr.ne.iyr) then
            write(nlog,1308) 'RioGrande Spill data (*.rgs)',
     1          'N/A',nisfinM, irg, idyr,iyr
            goto 9999
          endif
c
c ---------------------------------------------------------

          rgspill(ix)=0.0
          do im=1,12
            if(diverm(im).gt.small) then
              rgspill(ix)=diverm(im)

              if(ioutRgS.eq.1)
     1          write(nlog,*) '  Mdainp Rio Grande Spill Data;',
     1          ' ix, rgspill = ', ix, cifrid(ix), rgspill(ix)
            endif
          end do
        end do
      endif    
c
c _________________________________________________________
c
c rrb; 2006/03/21; Read Rio Grande Monthly Forecast (*.rgf)
c
      if(ibasef.eq.1) goto 1564
c
c ---------------------------------------------------------
c		Only read if file is provided      
      ifn=64
      rec256=fileName(ifn)
      rec72=rec256(1:72)
      if(rec72(1:2).ne.'-1') then
c
c ---------------------------------------------------------
c		Only read only if the counter is > 0
      if(nrgfor.eq.0) goto 1564
c
        iin2=64
        filena='*.rgf'
        do nd=1,nrgfor
c         write(nlog,*) '  Mdainp;', nd, iifcom(nd)
          read (iin2,951,end=762,err=928) idyr,cistat,
     1                                    (diverm(im),im=1,12)
          if(ioutRgF.gt.0) then
            write(nlog,*) ' '
            write(nlog,951)  idyr,cistat, (diverm(im),im=1,12)
          endif  
c
c rrb 2006/03/20; Adjust character string to left     
          cistat=adjustl(cistat)     

          if(idyr.ne.iyr) then
            write(nlog,1308) 'Rio Grande Forecast (*.rgf)', 
     1          'N/A',nrgfor, nd, idyr,iyr            
            goto 9999
          endif
C
c ---------------------------------------------------------
c               Adjust units
          if(rgfacto.ge.small) then
            do im=1,12
              DIVERM(IM)=DIVERM(IM)/rgfacto/MTHDAY(IM)
            end do
          endif

c
c _________________________________________________________
c
c rrb 2005/10/07; Allow forecast data (0) to be provided in any order
c
          cCallBy='Mdainp *.rgf'
          call stafind(nlog,1,1,numifr,ix,cistat,cifrid,cCallBy)
          if(ix.eq.0) goto 1304
c
          rgspill(ix)=rgspilx
c
c ---------------------------------------------------------
c rrb 00/03/16; Carry forecast to end of year
          ifor=0
          forc1=0.0
          do i=1,12
            IF(diverm(i).GE.-1*small) then
              flowr(i,ix)=diverm(i)
c
c rrb 2005/09/20; Save for Type 13 (La Plata Compact) use
              flowrX(i,ix)=diverm(i)
              forecast(i,ix)=0.0
c
c ---------------------------------------------------------
c rrb 00/03/15; Carry forecast to end of year
              if(ifor.eq.1) then
                flowr(i,ix) = 0.0
                forecast(i,ix)=forc1 /RgFacto/MTHDAY(i)
              endif
            else
              flowr(i,ix)=0.0
              forecast(i,ix)=diverm(i)
c
c rrb 00/03/15; Carry forecast to end of year 
              ifor=1
              forc1=diverm(i) *RgFacto*MTHDAY(I) 
c             write(nlog,*) '  Mdainp; ix, forecast ',ix,forecast(i,ix)
            endif
          end do
          if(ioutRgF.gt.0) then
            write(nlog,*) ' '
            write(nlog,951)  idyr,'-1', 
     1        (forecast(i,ix)*rgfacto*mthday(i),i=1,12)
          endif  

c
c ---------------------------------------------------------
c               End Rio Grande Forecast structure loop
        end do
c
c ---------------------------------------------------------
c               End Rio Grande Forecast option
      endif
c _________________________________________________________
c
c rrb; 98/12/09; Wells
c
c               READ WELL STRUCTURE DEMANDS - Monthly (*.wem)
c _________________________________________________________
c
 1564 continue
c          
c rrb 00/04/14; Add maximum recharge rate (gwmaxr)
c     if(iwell.eq.1) then
      if(iwell.ge.1) then
        if(numdivw.eq.0) goto 1566
c
        iin2=9
        if(ibasef.eq.0) then
          filena='*.wem'
        else
          filena='*.weh'
        endif
c
c ---------------------------------------------------------
c
      if(ichk.eq.94) write(nlog,*)'  Mdainp; reading *.wem '
        icount=0
        DO ND=1,NUMDIVw
          if(ioutw.eq.1) then
            write(nlog,*) ' '
            write(nlog,'(2(a12,1x), 3i5)')
     1        '  Mdainp;   ', cdividw(nd), nd, icount, idvcomw(nd)
          endif  

c
c rrb 00/06/19; Allow IWR data and global control (idivtyp) to
c               control addition
c
c rrb 2006/05/18; Temporarily allow more data then well structures
cr	      if(idvcomw(nd).eq.1 .or. idvcomw(nd).eq.3) then
 1565    if(idvcomw(nd).eq.1 .or. idvcomw(nd).eq.3) then
           read (9,952,end=1282,err=928) idyr,cistat,
     1                                    (diverm(im),im=1,12)

c           write(nlog,*) '  Mdainp; Well demands in '
            icount=icount+1
            
            if(ioutw.eq.1) then
              write (nlog,'(3(a12,1x),3i5)')
     1          '  Mdainp;   ', cistat, cdividw(nd), nd, icount,idyr
c    1          ,(diverm(im),im=1,12)
            endif
c
c ---------------------------------------------------------
c rrb 2006/03/20; Adjust character string to left     
            cistat=adjustl(cistat)     

c
c ---------------------------------------------------------
c		Warn if unexpected data was read
            if(idyr.ne.iyr) then
              if(ibasef.eq.0) then
                write(nlog,1308) 'Well Demand (*.wem)',
     1            '(*.wes or *.wst)',numdivw, nd, idyr,iyr
              else
                write(nlog,1308) 'Historic Pumping (*.weh)',
     1            '(*.wes or *.wst)',numdivw, nd, idyr,iyr
              endif
              goto 9999
            endif
c
c _________________________________________________________
c
c rrb 2005/10/07; Allow well data (6) to be provided in any order
c		Note Istop = 0 stop if not found
c			   = 1 OK if not found
            istop=0
            
            cCallBy='Mdainp *.wem'            
            call stafind(nlog,istop,6,numdivw,ix,cistat,cdividw,cCallBy)
c
c rrb 2006/05/18; Temporarily allow more data then well structures
cr	    if(ix.eq.0) goto 1302
          if(ix.eq.0) then
            write(nlog,1312) ' Warning', cistat,iyr
            goto 1565
          endif  
c
c ---------------------------------------------------------
c               Adjust units and insure data is positive
c rrb 2006/08/01; Set negative pumping to zero
          IF(DFACTO.ge.small) then
            DO IM=1,12
              if(diverm(im).gt.smalln) then
                DIVERM(IM)=DIVERM(IM)/DFACTO/MTHDAY(IM)
              else
                if(iwarnW(ix).eq.0) then
                  if(iwarnWT.eq.0) then
                    write(nchk,1244)
                  endif  

                  iwarnWT=iwarnWT+1
                  iwarnW(ix) = 1
                  c = diverm(im)/dfacto/mthday(im)
                  write(nchk,1245) iwarnWT, cistat, idyr, c,diverm(im)
                endif
                diverm(im)=0.0
              endif
            end do
          endif

c
c ---------------------------------------------------------
c rrb 00/08/03; Test if tied to a SW structure (idivcow2() > 0
c               and expect total demand expected in *.ddm 
          if(idemtyp.ge.3 .and.idivcow2(ix).gt.0) goto 1303
c
c ---------------------------------------------------------
c               Monthly non IWR demand
c               Check for negatives and set diver to correct station
c rrb 00/06/16; Data provided as monthly total demand (idvcomw(nu) = 1)
          if(idvcomw(ix).eq.1) then
            do im=1,12
              diverirw(im,ix)=diverm(im)*(diveffw(im,ix)/100.)
              diverw(im,ix)=diverm(im) 
            end do
          endif
c
c ---------------------------------------------------------
c rrb 00/06/16; Data provided as monthly IWR (idvcomw(nu) = 3)
          if(idvcomw(ix).eq.3) then
c           write(nlog,*) '  Mdainp; idvcomw(ix) = 3'
            do im=1,12
              if(diveffw(im,ix).gt.small) then
                diverirw(im,ix)=diverm(im)
                diverw(im,ix)=diverm(im)/(diveffw(im,ix)/100.)
              else
c
c rrb 00/08/02; Data check
                if(diverm(im).gt. small) then
                  c = dfacto*mthday(im)
                  write(nlog,757) cdividw(ix),divnamw1(ix),
     1                    iyr, im, diverm(im)*c, diveffw(im,ix)
                  goto 9999
                else
                  diverirw(im,ix)=0.0
                  diverw(im,ix)=0.0
                endif
              endif
            end do
          endif
c
c ---------------------------------------------------------
c		Detailed Well Demand Output
          if(ioutw.eq.1) then
            write (nlog,'(2(a12,1x),2i5,12f8.0)')
     1         '  Mdainp;   ', cistat, idyr,idvcomw(ix),
     1        (diverw(im,ix)*dfacto*mthday(im),im=1,12)
          endif
c
c		Endif for idvcomw = 1 (total demand in *.ddm) or 
c		       idvcomw=3 (IWR deamnd in *.ddm)
        endif

c
c ---------------------------------------------------------
c               End well structure loop
      end do

      endif
c
c ---------------------------------------------------------
c               Wells End
c
 1566 continue
c
c ______________________________________________________________
c               Step X; Read Annual Time series file (*.ipy or *.tsp) 
c               for RGDSS. Note since some default data varies by month 
c               (e.g. efficiency) default data is set in bomsec
c               if itsfile = 0 and for structures not included
c               in *.ipy or *.tsp as indicated by a zero maximum efficiency
c 
      if(itsfile.ge.1) then
        iin2=10
        if(nIpyX.eq.1) then
          call getIpy2(iin2)          
        else
          call getIpy4(iin2)
        endif
      endif   
      if(ichk.eq.4) write(nlog,*) '  Mdainp; back from getipy2 or 4'
c
c
c ______________________________________________________________
c               Step X; Read SJRIP Sediment data
c rrb 00/11/11; 
c  
      if(ichk.eq.4) write(nlog,*) '  Mdainp; isjrip', isjrip
      if(isjrip.eq.1) then
        iin2=10
        filena = '*.sjr'
        read(15,'(a132)') rec132
        write(nlog,'(a132)') rec132
        backspace (15)
        read (15,*,end=1288,err=928) idyr,sjpertb
c       write(nlog,*) '  Mdainp; idyr, sjpertb', idyr, sjpertb

        if(idyr.ne.iyr) then
                write(nlog,1308) 'SJRIP data (*.ddm)',
     1        'N/A',1, 1, idyr,iyr
          goto 9999
        endif
      endif  
c
c ______________________________________________________________
c               Step X; Read Downstream Call data - Monthly
c rrb 00/11/11; 
c    
      if(ichk.eq.4) write(nlog,*) '  Mdainp; icall', icall
      if(iday.eq.0 .and. idcall.ne.0) then
        iin2=90
        filena = '*.cal'
c
c		For monthly model read daily call data and set
c              to day 1 value
        do im=1,12
c
c rrb 2005/12/16; Correct to match daily        
c933      read(90,*,end=928,err=928) icd1, rec3, icy1, rec10, dcall1
 933      read (90,*,end=520,err=520) icy1, icm1, icd1, dcall1
 
          if(icd1.ne.1) goto 933
c         call findmo(nlog, cyr1, rec3, icm1,imnum)
          dcallm(im)=dcall1          
c         write(nlog,934)  icd1, icm1, icd1, dcallm(im) 
cx 934      format(' Mdainp; Monthly Call data ', 3i8.0, f20.5)

        end do  
      endif              
c
c ______________________________________________________________
c               Step X; Read IWR (*.ddc or *.iwr) data for variable 
c rrb 00/12/04;         efficiency capability. Note read for both
c			diversions and wells
c
      if(ichk.eq.4) write(nlog,*) '  Mdainp; ieffmax', ieffmax
      if(ieffmax.ge.1) then
        iin2=14
        filena = '*.ddc'
      
        do i=1, maxuse
          idum(i) = 0
          idum2(i) = 0
        end do
c
c               Note allows variable nummber of station to be read
        nx = nduser(numdiv+1)-1+numdivw
        if(ichk.eq.94 .or. ichk.eq.4)
     1     write(nlog,*)'  Mdainp; reading *.ddc nx ' ,nx
     
c jhb 2014/06/26 make the do loop limit arbitrarily large (5000)
c                so it always works no matter how many records are
c                in the IPY, STR, DDC files.  This is OK, because
c                there is code below to jump out of this loop when the
c                last record is read (the year changes)
c        do nd=1,nx
c
c rrb 2017/12/18; Add a warning if number of records read
c                 exceeds the read limit
cx      do nd=1,5000
cx
cx rrb 2017/12/22; Revise to read an unknown number of records
cx                 until a new year is read or and end of file
cx                is encountered.
cx        ndmax=5000
cx        do nd=1,ndmax
cx
        nd=0
c        
 1700   nd=nd+1
c
 1701   read (14,951,end=1710,err=928) idyr,cistat,
     1                                  (diverm(im),im=1,12)

c       write (nlog,951) idyr,cistat,(diverm(im),im=1,12)
c       write(nlog,*) '  Mdainp IWR; idyr, iyr',idyr,iyr
c
c rrb 2006/03/20; Adjust character string to left     
        cistat=adjustl(cistat)
c
c rrb 01/08/27; Allow extra data in file
        if(idyr.lt.iyr) goto 1701
        if(idyr.gt.iyr) then
          backspace(14)
c         write(nlog,1632)
          goto 1710
        endif
c
c               Adjust units
        if(dfacto.ge.small) then
          do im=1,12
            DIVERM(IM)=DIVERM(IM)/dfacto/MTHDAY(IM)
          end do
        endif
c
c               Find diversion station associated with this data
        ix=0

        cCallBy='Mdainp D.ddc'
        call stafind(nlog,1,3,numdiv,ix,cistat,cdivid,cCallBy)
c         write(nlog,*) '  Mdainp; ix for diversion', ix      
c            
c               Set IWR data for a diversion only (diwr),
c               and for a D&W (diwr)
        if(ix.gt.0) then
          do im=1,12
            diwr(im,ix)=diverm(im)
            if(ioutd.eq.1) then
              write(nlog,*) '  Mdainp; ix, im, diwr(im,ix)'
              write(nlog,*) '  Mdainp;', ix, im, diwr(im,ix)
            endif
          end do
          idum(ix)=1

c         write(nlog,951) idyr, cistat, 
c    1      (diwr(im,ix)*dfacto*mthday(im), im=1,12)
c         write(nlog,*) ' '

        else
c
c              Find WEll ONLY associated with this data
          
          cCallBy='Mdainp W.ddc'
          call stafind(nlog,1,6,numdivw,ix,cistat,cdividw,cCallBy)
c         write(nlog,*) '  Mdainp; ix for well', ix
c
c              Set IWR data for a well
          if(ix.gt.0) then
            do im=1,12
              diwrw(im,ix)=diverm(im)
c
c rrb 2006/12/20; Reset Well IWR to data read in *.ddc
c		              Else IWR is set above to be demand * eff
c		              where eff is average in *.wes; not a sprinkler eff
              diverirw(im,ix)=diverm(im)
            end do
            idum2(ix)=1

c           write(nlog,*) '  Mdainp; ix, idum2(ix)', ix, idum2(ix)  
c           write(nlog,951) idyr, cistat,
c    1        (diwrw(im,ix)*dfacto*mthday(im),im=1,12)
c           write(nlog,*) ' '
          endif
c
c               Warn if station is not found
          if(ix.eq.0) then
            if(ipIpy.eq.0) then
              rec40='Irrigation Water Requirement (*.ddc)'
              write(nlog,1281) iyr, rec40
              ipIpy=1
            endif

            iwarnISt=iwarnISt+1
            if(iwarnISt.eq.1) write(nchk,1315) 
            write(nchk,1316) iwarnISt, cistat, iyr
c
c rrb 01/03/08; OK if not found that occurs when wells are turned off
c             goto 9999
          endif

        endif
c
c rrb 2017/12/22; Revise to read an unknown number of records
c                 until a new year is read or and end of file
c                 is encountered.
cx
cx               End monthly IWR data read
cx     end do
cx
cx rrb 2017/12/11; Warn and stop if the loop to read data is too small
cx       write(nlog,1654) ndmax
cx        goto 9999
cx
       goto 1700
c
 1710   continue


c
c _________________________________________________________
c
c               Check that all diversions have IWR data provided
c               If not, set to IWR data provided via demand
        if(ichk.eq.94 .or. ichk.eq.4)
     1     write(nlog,*)'  Mdainp; Check diversinos for iwr data'


        iprinti=0
        do nd=1,numdiv
          nui=nduser(nd) 
          nue=nduser(nd+1)-1
      
          if(nui.le.nue) then
            do nu=nui,nue
              if(idum(nu).eq.0) then
                do im=1,12
                  diwr(im,nu) = diverir(im,nu)
                end do
                if(area(nu).gt.small) then
                  if(iprinti.eq.0) then
                    write(nchk,1633)
                  endif  
                  
                  if(ipDdc.eq.0) then
                    rec40='Consumptive Water Requirement (*.ddc)'
                    write(nlog,1281) iyr, rec40
                    ipDdc=1
                  endif  
                  
                  iprinti=iprinti+1
                  write(nchk,1638) iprinti,iyr, cdivid(nu),
     1                             divnam1(nu), area(nu)
                endif

              endif
            end do
          endif
        end do
c
c ---------------------------------------------------------
c               Check that all well only lands had IWR data provided
c               Unless tied to a surface structure (idivcow2>0)
c               If not provided, set to IWR via demand and 
c               warn if area > 0
        iprintw=0
        if(ichk.eq.94 .or. ichk.eq.4)
     1    write(nlog,*) '  Mdainp; Setting IWR data for a well'
        
          do nd=1,numdivw
            if(idum2(nd).eq.0 .and. idivcow2(nd).eq.0) then
cx          if(ichk.eq.4) then
cx            write(nlog,*) ' Mdainp; Setting IWR data for a well', nd
cx            write(nlog,*) 'idum2(nd), idivcow2(nd), cdividw(nd), iyr' 
cx            write(nlog,*)  idum2(nd), idivcow2(nd), cdividw(nd), iyr 
cx          endif
              
            do im=1,12
              diwrw(im,nd)=diverirw(im,nd)
            end do
            
cx          if(ichk.eq.4) write(nlog,*) 
cx   1        ' Mdainp; nd, areaw(nd)', nd, areaw(nd), ipDdc,iprintw
     
            if(areaw(nd).gt.small) then
              if(iprintw.eq.0) then
cr              rec40='Irrig Water Requirement'
cx              if(ichk.eq.4) write(nlog,1634) iyr
                write(nchk,1634) iyr
              endif

              if(ipDdc.eq.0) then
                rec40='Consumptive Water Requirement (*.ddc)'
                write(nlog,1281) iyr, rec40
                ipDdc=1
              endif  

              iprintw=iprintw+1
cx            if(ichk.eq.4) then 
cx              write(nlog,* ) '  Mdainp; nd ', nd
cx              write(nlog,1635) iprintw, iyr, cdividw(nd), 
cx   1          divnamw1(nd), areaw(nd)
cx            endif
              
              write(nchk,1635) iprintw, iyr, cdividw(nd),
     1          divnamw1(nd), areaw(nd)
c             goto 9999
            endif
          endif
        end do
      endif
      
      
c ______________________________________________________________
c               Step X; Read JMartin Baseflow percent
c rrb 00/11/11; 
c  
      if(ichk.eq.4) write(nlog,*) '  Mdainp; ijm', ijm  
       
      if(ijm.eq.1) then
        ioutJM=0
        iin2=88
        filena = '*.jmm'
        read(88,'(a132)') rec132
        
        if(ioutJM.gt.0) write(nlog,'(a132)') rec132
        backspace (88)
        
c
c rrb 2019/06/17; Revise to read a stream ID and in JMFlow (type 54)
c                 calculate Enhanced Baseflow (pctE) based on 
c                 Baseflow (pctB = cjm(im,1)) 
cx      do i=1,2
          read(88,*,end=1288,err=928) idyr, cistat, 
     1       (cjm(im,1),im=1,12)
c
          if(ioutJM.gt.0) then
            write(nlog,*) idyr, cistat, 
     1        (cjm(im,1),im=1,12)
          endif

c
c rrb 2006/03/20; Adjust character string to left     
          cistat=adjustl(cistat)
          cjmid(1)=cistat
c
c               Check year provided is correct          
         if(idyr.ne.iyr) then
            write(nlog,1308) 'JMartin data (*.jmm)',
     1      'N/A',1, 1, idyr,iyr
            goto 9999
          endif
cx      end do
c
c               Endif for JMarting Data (ijm=1)
      endif 
      
c
c ________________________________________________________
c               Step 31; Calculate total demand for output
c rrb 00/06/16; 
c     
c --------------------------------------------------------
c               Step 31a; Diversions
      if(ichk.eq.94 .or. ichk.eq.4)
     1  write(nlog,*) '  Mdainp; Calculate total demand'

      do nd=1,numdiv
        nui = nduser(nd)
        nue=nduser(nd+1)-1
        
        if(nui.le.nue) then
          do nu=nui,nue
            do im=1,12  
              divert(im,nu) = diver(im,nu)
c
c rrb 01/04/01; Correction for skip file code
c             if(ieffmax.eq.0) then
              if(ieffmax.le.0) then
                diverirT(im,nu) = diver(im,nu) * diveff(im,nd)/100.
              else
                diverirT(im,nu) = diwr(im,nu)
              endif
            end do
          end do
        endif
      end do
c     
c --------------------------------------------------------
c               Step 31b; Wells
c
      if(iwell.ge. 1) then
        do nw=1,numdivw
          nd=idivcow2(nw)
          if(nd.gt.0) then
            nu=nduser(nd)
        
            do im=1,12
c
c               b. Daily capability
              if(iday.eq.0) then
                fac=mthday(im)*factor
              else
                fac=factor
              endif      

c
c rrb 00/07/11; Adjust well demand so that the total refelects SW efficiency
c               if in addition mode only (idemtyp=2)
c rrb 01/02/04; Adjust only if in addition mode (idemtyp=2)
              if(diveff(im,nu).gt.small .and. idemtyp.eq.2) then
                ceff = diveffw(im,nw)/diveff(im,nu)
              else
                ceff = 1.0
c               write(nlog,757)
              endif
              
              divert(im,nu) = diver(im,nu) + diverw(im,nw)*ceff
c             
c
              if(ioutD.eq.1) then
                write(nlog,*) '  Mdainp; idemtyp=2 ',
     1            im, nw, nu, diveffw(im,nw), diveff(im,nu),ceff,fac,
     1            diver(im,nu)*fac,diverw(im,nw)*fac,divert(im,nu)*fac
              endif
c
c rrb 01/04/01; Correction for skip file code
c             if(ieffmax.eq.0) then
              if(ieffmax.le.0) then
                diverirT(im,nu) = diver(im,nu)*diveff(im,nd)/100.
     1                          + diverw(im,nw)*diveffw(im,nw)/100.
              else
                diverirT(im,nu) = diwr(im,nu) + diwrw(im,nw)
              endif
            end do
          endif
        end do
      endif
c
c ________________________________________________________
c               Step 32; For type 5 Decreed Demand 
c                        If an IWR exists, then adjust
c                        SW and D&W demands to equal max (user supplied
c                        or their water right).  Note no adjustment
c                        to IWR via demand or well only lands required.
c rrb 01/02/23; 
       if(idemtyp.eq.5) then
c        ichk1=0
c        if(ichk1.eq.1) 
c    1     write(nlog,*) '  Mdainp; Adjusting demand for type 5'

         do nd=1,numdiv
           do im=1,12
c              if(ichk1.eq.1) write(nlog,*) 
c    1           '  Mdainp; ',im,nd,diver(im,nd),demcond(nd)   
c
c rrb 01/04/12; Reset to water right if IWR is > 0)
c rrb 01/04/12; Reset to demand to handle Closed Basin Pumping
c            if(diwr(im,nd).gt.small) then 
             if(diver(im,nd).gt.small) then
c
c rrb 2021/04/18; Compiler warning
cx             diver(im,nd)=amax1(demcond(nd), diver(im,nd))
               diver(im,nd)=max(demcond(nd), diver(im,nd))
               divert(im,nd)=diver(im,nd)
             endif
           end do
         end do
       endif

c
c rrb 00/07/11; Test
c     do nd=1,numdiv
c       write(nlog,759)nd,(divert(im,nd)*dfacto*mthday(im),im=1,12)
c       write(nlog,759)nd,(diverirT(im,nd)*dfacto*mthday(im),im=1,12)
c       write(nlog,*) ' '
c     end do
c
c _________________________________________________________
c               Step 33; Return
c
      if(ichk.eq.94 .or. ichk.eq.4)
     1    write(nlog,*) '  Mdainp; return'

      return

c
c _________________________________________________________
c               Error Handling
c
  570 write(nlog,580)
  580 FORMAT(/
     1 '  Mdainp; Problem.',/
     1 '          Not enough data in the monthly evaporation ',
     1 'file (*.evm)')
      Goto 9999

  571 write(nlog,581)
  581 FORMAT(/
     1 '  Mdainp; Problem.',/
     1 '          Not enough data in the annual evaporation ',
     1 'file (*.eva)')
     
      Goto 9999                                                 
      
  757 format(/
     1 '  Mdainp; Problem division by zero',/
     1 '          Demand data provided as IWR is > 0 and ', 
     1 'efficiency is < 0 for: ', /,
     1 '          ID: ' ,a12, ' Name: ', a24, ' Date: ',2i5,  
     1 ' Demand: ', f8.0, ' Efficiency: ' f8.0)
cx  758 format('  Mdainp;', 3(i5), 12f8.0) 
cx  759 format('  Mdainp;', i5, 12f8.0)
  760 write(nlog,770)
  770 FORMAT(/
     1 '  Mdainp; Problem.',/
     1 '          There is not enough data in the Annual Instream',/
     1 '          Demand File (*.ifa)')
      Goto 9999

  762 write(nlog,772) iyr
  772 FORMAT(/
     1 '  Mdainp; Problem.',/
     1 '          In year ', i5, ' Not enough data in the monthly ',/
     1 '          Instream Demand File (*.ifm)',/
     1 '          Check variable iifcom in structure file (*.ifs)')
      Goto 9999

C
cx  780 write(nlog,790)  cistat
cx  790 FORMAT(/
cx     1 '  Mdainp; Problem.'/
cx     1 '          STRUCTURE ',a12,' in the annual instream demand',
cx     1 '          file (*.ifa) cannot be found')
cx      Goto 9999

  860 if(interv.ne.-999) then
        write(nlog,870) irtnid(idl),maxdlm,ctot
      else
        write(nlog,872) cirtnid(idl), maxdlm, ctot
      endif
  870 format(/
     1 '  Mdainp; Problem.',/
     1 '          Delay table ',I10,' exceeds the maximum ',i10,/
     1 '          or exceeds 100%' f10.4, ' in *.dly or *.urm')
  872 format(/
     1 '  Mdainp; Problem.',/
     1 '          Delay table ',a12,' exceeds the maximum ',i2,/
     1 '          or exceeds 100%' f10.4, ' in *.dly or *.urm')

      goto 9999
c
c
  926 write(nlog,927) iin2, filena
  927 format(/
     1 ' Mdainp; Problem.',/
     1 '         End of file # ', i4, ' encountered',/,
     1 '         File name: ', a256)
      goto 9999
c
c
  928 write(nlog,929) iin2, filena
  929 format(/
     1 '  Mdainp; Problem reading file # ', i4,/,
     1 '          File name: ', a256)
cr   1 '          Last 2 lines read successfully are:')

c     backspace(iin2)
cr    read(iin2, '(a256)',end=926,err=926) recin
cr    write(nlog,'(a256)') recin
      goto 9999
c
c
cx  930 write(nlog,931)
cx  931 format(/
cx    1 '  Mdainp; Problem reading the delay table (*.dly or *.urm)',/
cx    1 '          Note for a simulation with wells or a daily',/
cx    1 '          time step, the control file (*.ctl) variable',/
cx    1 '          (interv) must be less than zero to indicate ',/
cx     1 '          a variable number of entries is provided')
      write(nlog,929) iin2, filena
      backspace(iin2)
      backspace(iin2)
      read(iin2, '(a256)',end=926,err=926) recin
      write(nlog,'(a256)') recin
      read(iin2, '(a256)',end=926,err=926) recin
      write(nlog,'(a256)') recin
      goto 9999
c
c
 1280 write(nlog,1290)
 1290 FORMAT(/,
     1 '  Mdainp; Problem.',
     1 ' NOT ENOUGH DATA IN DIVERSION FILE (*.ddm)',/
     1 10x,'Reconmend you check the file opened correctly in *.log',/
     1 10x,'and that the file has a demand for every diversion',/
     1 10x,'in the diversion station file')
      goto 9999
     
 1281  FORMAT(/,72('_'),/
     1  '  Mdainp; Warning See *.chk for details in year',i5,/
     1  '          Regarding file: ',a40,/
     1  '          Note only first occurance (year) is printed')

 1282 write(nlog,1283) filena
 
 1283 FORMAT(/,
     1 '  Mdainp; Problem.',
     1 ' NOT ENOUGH DATA IN WELL DEMAND (*.wem or *.weh)',/
     1 '          File Name = ', a5)
      goto 9999
      
 1286 write(nlog,1287)
 1287 FORMAT(/,
     1 '  Mdainp; Problem.'
     1 ' NOT ENOUGH DATA IN Rio Grande Spill FILE (*.rgs)')
      goto 9999
c
 1288 write(nlog,1289)
 1289 FORMAT(/,
     1 '  Mdainp; Problem.'
     1 ' NOT ENOUGH DATA IN San Juan Recovery File (*.sjr)')
      goto 9999
      
cx 1291 write(nlog,1292)
cx 1292 FORMAT(/,
cx     1 '  Mdainp; Problem.'
cx     1 ' NOT ENOUGH DATA IN Irrigation Water Requirement FILE (*.iwr)')
cx      goto 9999
c
c
cx 1301 write(nlog,1311) cistat, 'Monthly ', iyr, iifcom(nd)
 1311 FORMAT(/,'  Mdainp; Problem',
     1 ' Structure ',a12,' of the ',a8, ' ISF demand file',/
     1 10x,'In year',I5, ' is not found in file (*.ifs)',/
     1 10x,'or the data type switch = ', i5, ' is incorrect.',/
     1 10x,'Recommend you revise the *.ifs, *.ifa or *.ifm Files')
      goto 9999
c
c
cx 1302 write(nlog,1312) ' Problem', cistat,iyr
 1312 FORMAT(/,
     1 '  Mdainp; ',a8,
     1 ' Structure ',a12,' of well demand file (*.wem)',
     1 ' in year',I5, ' not found') 
      goto 9999
c
c
 1303 write(nlog,1313) idemtyp, cistat,iyr
 1313 FORMAT(/,
     1 '  Mdainp; Problem.',/
     1 '          Demand control (icondem) from control file (*.ctl)', 
     1            ' = ', i5,/
     1 '          which means the diversion demand file (*.ddm) has',/ 
     1 '          total demands for D&W structures.  Structure ',a12,/
     1 '          is tied to a SW structure in the well station file',/ 
     1 '          (*.wes) but it has data in the well demand file',/ 
     1 '          (*.wem) year',I5) 
      goto 9999
c
c
c
 1304 write(nlog,1314) cistat,iyr
 1314 FORMAT(/,
     1 '  Mdainp; Problem',
     1 ' Structure ',a12,' of Monthly Rio Grande Forecast (*.rgf)',
     1 ' in year',I5, ' not found in file (*.ifs)')
      goto 9999

c
cx1305 write(nlog,1306) cdivid(nd)
 1306 FORMAT(/,
     1 '  Mdainp; Warning. '
     1 ' Structure ID ',a12,' in the diversion file (*.ddm)',/ 
     1 10x, 'is an import but it is not in the plan file (*.pln)')
      goto 9999
 
c
c 	Div STATION IS NOT FOUND, WRITE ERROR MESSAGE AND STOP
C
 1308 FORMAT(/
     1 '  Mdainp; Problem Reading File = ', a32, /,
     1 '          The file is inconsistent with the number of',/
     1 '          stations in the Station file = ', a16,/
     1 '          Number in the station file = ', i5,/
     1 '          Number read from the Time Series = ', i5,/
     1 '          Year read = ', i5,' Year expected = ', i5)
 1310 FORMAT(/
     1 '  Mdainp; Problem ID = ',a12,' of File = ', a32, /,
     1 '          is not found in the station file = ', a8,/
     1 '          Year read = ', i5,' Year expected = ', i5)
c
 1325 write(nlog,1326) cdivid(nd)
 1326 FORMAT(/,
     1 '  Mdainp; Warning. '
     1 ' Structure ID ',a12,' in the diversion file (*.ddm)',/ 
     1 10x, 'has a combination of negative (import) and ',/
     1 10x, 'positive demands greater than zero.',/
     1 10x, 'This combination is not supported.',/
     1 10x, 'Recommend you separate imports from diversions.')
      goto 9999
c
c      
 1360 write(nlog,1370) cistat,idyr
 1370 FORMAT(/
     1 '  Mdainp; Problem.'
     1 'DIVERSION STRUCTURE ',a12,' IN OVERRIDING FILE (*.ddo)',
     1 'IS NOT IN DIVERSION STATION FILE IN YEAR ',I5)
      goto 9999
c

c
 1570 write(nlog,1580)
 1580 format(/,
     1 '  Mdainp; Problem.'
     1 ' Not enough data in reservoir target file (*.tam)')
      goto 9999
C
 1590 write(nlog,1600)
 1600 FORMAT(/,
     1 '  Mdainp; Problem.',/
     1 '          NOT ENOUGH DATA IN THE RUNOFF DATA FILE (*.rim)',/
     1 '          Check Period of Record and or Rerun Base flows')
      goto 9999
C
c rrb 2011/04/03; Warn if the annual time series switch (itsfile) is set
c                 but no annual time series file is provided.

 1650 write(nlog,1652) itsfile
 1652 FORMAT(/,
     1 '  Mdainp; Problem.',/
     1 10x, 'The variable (itsfile) specified in the control ',
     1      'file (*.ctl) = ', i5,'.' /
     1 10x, 'This value is not supported or no annual irrigation',/
     1 10x, '  practice file (*.ipy) has been provided.',/
     1 10x, 'Reconmend you revise variable itsfile or provide an',/
     1 10x, '  annual time series file (*.ipy).')
      goto 9999
c
c rrb 2017/12/11; Warn if the number of data reads is too small
cx 1654 FORMAT(/,
cx     1  72('_'),//  
cx     1 '  Mdainp;  Warning when reading *.iwr',/
cx     1 '           the number of data points read = ', i8,/
cx     1 '           that is too small.  ',/
cx     1 '           Reconmend you revise Mdainp.for')
      
C
 
 9997 write(nlog,9998) filena
 9998 format(/,'  Mdainp; Problem opening file: ', a256)
 9999 write(nscrn,*)  '  Stopped in Mdainp, see log file (*.log)'
      write(nlog,*) ' Stopped in Mdainp'
      write(nscrn,*) 'Stop 1'
      call flush (6)
      call exit(1)


      stop 
c
c _________________________________________________________
c               Formats
c
 532  format(5x,a12,12f8.0) 
 534  format(i5, 5x,a12,12f8.0) 
 536  format(i4,1x, i4,1x, a12,12f8.0) 
cx 545  format(/,
cx     1  72('_'),//  
cx     1 '  Mdainp; Problem Annual time series data (*.ipy or *.tsp)',/
cx     1 '          Well ID = ', a12,' is tied to Diversion ID = ',a12,/
cx     1 '          To do; provide data in *.ipy or *.tsp once under', 
cx     1          ' diversion ID')      
cx
cx 547  format(/,
cx     1  72('_'),//  
cx     1 '  Mdainp; Problem for Div or Well station = ', a12,/
cx     1 '          The type = 4 which indicates transmountain, but',/
cx     1 '          the max efficiency data (*.ipy or *.tsp) ',/
cx     1 '          is not 100%.',/
cx     1 '          To do: Revise type or max. efficiency')
cx
cx 550   FORMAT(16X,I8,12F8.0)


cx 828   format(a12,1x,i4,/,(10f10.6))
 829   format(a12,1x,i4,1x,(10f10.6))
cx 832   format(/,
cx     1  72('_'),//  
cx     1   '  Mdainp;',
cx     1   ' Warning in delay table file (*.dly or *.urm), the',
cx     1   ' total return for table ', i5,' =',f10.2, ' Continuing on')
 850   FORMAT(/
     1  72('_'),//  
     1   '  Mdainp; Problem'
     1   ' TOO MANY DELAY TABLES in (*.dly or *.urm),    MAXIMUM = ',I5)
 881  format(/
     1  72('_'),//  
     1        , '  Mdainp; Delay Table Summary',//
     1        , '       #       Tbl ID   # of values    Total %',/
     1        , ' _______ ____________ _____________ __________')
     
 885   format(/,
     1  72('_'),//  
     1'  Mdainp; Problem with return data for structure type ',a16,/
     1   10x, 'Could not find return or depletion table id ', i10)
     
 951  format(i4, 1x, a12, 12f8.0, 10x, f8.2)
 952  format(i4, 1x, a12, 12f8.0)
 953  format(2i4, 1x, a12, 12f8.4)
cx 954  format(i4, 1x, a12, 3f6.0, 2f8.0, f12.0, f3.0, f8.0)
 
 955  format(a12,1x, i4, 1x, i4, 1x, i4, 1x, a12)


 
 1241 format(/,
     1  72('_'),//  
     1   '  Mdainp; FYI at least one diversion is less than 0 for ',
     1   ' the following',/,
     1   10x, 'Note: OK if an import.',/
     1   10x, 'Note: In a Sumulation Mode the value is set to zero',/
     1   10x, 'Note: Only the first negative value is printed for',
     1        ' each structure.',//
     1   '    # ID           Year   Q (cfs)    Q (af)',/
     1   ' ____ ____________ ____ _________ _________')
     
 1242 format(i5,1x,a12,i5, 20f10.2)

cx 1243 format(/,
cx     1  72('_'),//  
cx     1 '  Mdainp; Warning negative demand of ', f10.2,
cx     1    ' set to' f10.2)
cx
 1244 format(/,
     1  72('_'),//  
     1   '  Mdainp; FYI at least one pumping value is less than 0 for ',
     1   ' the following',/,
     1   '          Note: Set to zero.',/
     1   '          Note: Only the first',
     1   ' negative value is printed for each structure.',//
     1   '    # ID            Year   Q (cfs)    Q (af)',/
     1   ' ____ ____________ _____ _________ _________')
 1245 format(i5,1x,a12,1x, i5, 20f10.2)

 1315 FORMAT(/,
     1  72('_'),//  
     1 '  Mdainp; Warning. ',/
     1 10x, 'The following Structures are in the Consumptive '/
     1 10x, 'Water Requirement file (*.ddc) but are not in a',/
     1 10x, 'diversion (*.dds) or a well (*.wes) station file.',/
     1 10x, 'Since it cannot be tied to a diversion or well this',/
     1 10x, 'data is not used',/     
     1 10x, 'Non Fatal Error Analysis Proceeding',//
     1   '    # ID            Year',/
     1   ' ____ ____________ _____')
     
 1316 format(i5, 1x, a12, 1x, i5)
     
cx 1318 FORMAT(/,
cx     1  72('_'),//  
cx     1 '  Mdainp; Problem',
cx     1  ' Structure ID ',a12,' in *.ipy or *.tsp has conveyance,',
cx     1  ' flood or',/ 
cx     1 10x, ' sprinkler efficiency less than 0 or greater than 1.',/ 
cx     1 10x, ' To do; Revise efficiency data.', 20f8.2)
cx
cx 1319 FORMAT(/,
cx     1  72('_'),//  
cx     1 '  Mdainp; Warning in *.ipy or *.tsp',/
cx     1 '          Sprinkler area > ground water area, ',
cx     1           ' setting sprinkler = GW area or',/
cx     1 '          GW area > Total Area ',
cx     1           ' setting GW area = Total area',//
cx     1  '    # Year ID             Spr Area   GW Area     Delta',
cx     1                          '   GW Area  Tot Area     Delta'/
cx     1  ' ____ ____ _____________ _________ _________ _________',
cx     1                          ' _________ _________ _________')
cx 
cx 1321 FORMAT(/,
cx     1  72('_'),//  
cx     1 '  Mdainp; Warning in year ',i5, ' Well Only ID ',a12,/
cx     1 10x  'in *.ipy or *.tsp has GW area ', f10.0,
cx     1 ' < total area ', f10.0,/  
cx     1 10x, 'Set GW area = Total area and moving on')
cx     
cx 1322 format(i5, i5, 1x, a12,1x 20f10.2)

 1323 FORMAT(/,
     1  72('_'),//  
     1 '  Mdainp; Soil Moisture Data',/
     1 '  Type         Year    # ID            ',
     1 '      Area     Awrc     Soils',/
     1 '  ____________ ____ ____ _____________ ',
     1 ' _________ ________ _________')
     
 1324 format(2x, a12, 1x, i5, i5, 1x, a12,1x, 20f10.2)
     
 1500   format(/,
     1 '  Mdainp;'
     1 ' Reservoir ',a12,' in Reservoir Target (*.tam) file needs two',
     1 ' values per year; reservoir min pool and max pool')
     
 1520 format(/
     1  72('_'),//  
     1   '  Mdainp; Problem'
     1   ' Reservoir ID ',a12,'in the Target file (*.tam) not found')

 1622 format( /,
     1  72('_'),//  
     1 '  Mdainp; Warning',
     1 '  Streamflow data (*.rim) has missing data (-999)',
     1 '  temporarily set to 0.0',/
     1 i4, 1x, a12, 12f8.0)
cx 1630 format(/,
cx     1  72('_'),//  
cx     1   '  Mdainp; Problem with the monthly Well demand file ',/
cx     1   '    (*.wem).  Either not enough data or too much data',/
cx     1   '    for a given year.  Note, if a well is tied to a',/
cx     1   '    diversion, the demand code (idvcomw) should be 6',/
cx     1   '    and no data is expected in the well demand file',/
cx     1   '    (*.wem).  The last record read for simulation year ',
cx     1        i5,' is:',/,a256)
cx 1631 format(/,
cx     1  72('_'),//  
cx     1   '  Mdainp; Problem with the annual time series',
cx     1   ' file (*.ipy or *.tsp)',/
cx     1   '    Either not enough data or too much data for a year',/
cx     1   '    The last record read for simulation year', i5,' is:',/
cx     1   a256)
cx 1632 format(/,
cx     1  72('_'),//  
cx     1   '  Mdainp; Warning the annual time series file ',
cx     1   '(*.ipy or *.tsp)',/
cx     1   '    has more data then wells. Moving on')
     
 1633 format(/,
     1  72('_'),//  
     1   '  Mdainp; Warning the IWR (CIR) file (*.ddc) has no data',/
     1   '          for the following DIVERSION OR D&W structure',/
     1   '          with acres > 0 ',//
     1   '     # Year Station ID   Station Name              Acres',/
     1   ' _____ ____ ____________ ________________________ _______')

 1634 format(/
     1  72('_'),//  
     1   '  Mdainp; Warning the IWR file (*.ddc) has no data for ',/
     1   '          the following WEll Only structure',/
     1   '          with acres > 0 in year ',i4,/
     1   '          Setting IWR via demand and efficiency)',// 
     1   '     # Year Station ID   Station Name               Acres',/
     1   ' _____ ____ ____________ ________________________ _______')
 1635 format(1x, i5, i5, 1x, a12, 1x, a24, 1x, f8.0)    
     
 1636 format(/,72('_'),/  
     1   '  Mdainp; Warning the StateCU parameter file (*.par or',/
     1   '          *.str) has no soil parameter data for the ',/
     1   '          following that have acres > zero.',/
     1   '          Note: The value is set to zero',/
     1   '                OK if not an agricultural structure'// 
     1   '     # Year Station ID   Type Station Name            ',
     1   '     AWC   Acres',/
     1   ' _____ ____ ____________ ____ ________________________',
     1   ' _______ _______')
     
    
 1638 format(1x, i5, i5, 1x, a12, 1x, a24, f8.0)     
 1639 format(1x, i5, i5, 1x, a12, 1x, a4, 1x, a24, f8.2, f8.0)     
 
 1640 format(/,72('-'),/
     1 '  Mdainp; WARNING FILE ', A5,/ 
     1 '          HAS A UNIT CONVERSION FACTOR         = ', F10.4,/
     1 '          WHILE THE CONTROL FILE HAS A FACTOR  = ', F10.4,/
     1 '          THE TIME SERIES DATA CONTROLS FACTOR = ', F10.4,/
     1 '          Note the above override was fully implemented',/
     1 '          in StateMod Versions 10.726 and 11.10')      
 
      end




