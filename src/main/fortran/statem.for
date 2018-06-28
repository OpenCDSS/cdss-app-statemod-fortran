
c     Last change:  RRB   9 Oct 2002    3:25 pm
c *********************************************************
c
c
c
      PROGRAM StateM
c
c       Statem; Main program for StateMod
c
c
c _________________________________________________________
c	Program Description
c
c		StateM; main program for StateMod
c _________________________________________________________
c       Update History
c
c rrb 01/01/02; Added option 10 (baseflows with variable efficiency)
c               and option 11 (baseflow with variable efficiency and
c               wells with sprinklers get used first)
c
c rrb 03/06/02; Added option to not print a log file with -version
c               option by moving the temporary log file open
c               statememtn to parse.f
c jhb 2014/07/09; version 13.00.02A
c                 interim version for testing
c                 added new op rule type 24 code from ray b
c                   (shared shortages)
c                 changed ipy (and other statecu file) reading code to
c                   allow for many more records (up to 5000) than are
c                   in the current statemod network
c                 code tweaked and updated to support modern gfortran
c                   compiler and to be platform independent
c                 code maintained in git repository and managed in
c                   eclipse/photran development environment
c jhb 2014/07/14; change diversion array size from 1530 to 3060
c                 change maxOwnD from 1531 to 3060
c                 change plan array size from 720 to 1440
c                 change plan return array size from 722 to 1444
c                 change maxsta and maxrun array size from 2500 to 5000
c jhb 2014/07/14; upgraded to version 14.00.00 after testing
c                 this starts a major new branch of the code,
c                   new compiler (gfortran), developed in eclipse/photran
c                   multiple platform compatible
c                 (runs currently in both windows and linux)
c jhb 2014/07/14; upgraded to version 14.00.01 after implementing
c                   and testing a code update that allows ISF reaches
c                   to correctly use multiple water rights in ifrrig2
c                 changed divir(i) to divir(l2,i) where i is an isf
c                   reach node index and l2 is a water right index
c                   this allowed keeping track of multiple water rights
c                   at a single node, critical to it working correctly
c jhb 2014/07/23; upgraded to version 14.00.02 after fixing
c                 several array bounds issues
c jhb 2014/07/25; upgraded to version 14.00.03
c                 removed isf overlapping reach test and warning in datinp()
c                   because it now works since multiple isf water rights
c                   are allowed at isf reach nodes
c                 changed output for an admin plan (to b68 binary and
c                   therefore the xpl, too) when the plan is being split
c                   by a type 46 op rule into other plans.  output the
c                   total supply to those plans from psuplyt()
c jhb 2014/08/19; upgraded operating rule 35 (transbasin import) behavior
c                   now the destination should be a type 11 (accounting) plan
c                   source remains the same.
c                   reuse is NOT handled by the op rule 35 anymore, but
c                   should be handled by the modeler downstream of the
c                   accounting plan
c                 also (un)fixed a problem in execut.for in the
c                   IMO variable reset logic.  a change was made previously
c                   to fix array bounds errors, but the fix broke return flow
c                   calculations.  therefore reverted code back to original.
c                   however, now have to find another way to fix array bounds
c                   problem when daily return flow patterns are used.
c jhb 2014/09/05; merged newtype35 branch into master
c                 merged newtype45rayb into master
c                 op rule 27 deliver to isf
c                 updated version to 14.01.00
c jhb 2014/09/05; tweaked or35 code in oprinp.for to account for gnu fortran to c
c                   precompiler issue - if blocks being evaluated as a single line
c                 updated version to 14.01.01
c jhb 2014/10/24; change code to prevent array bounds errors in welrig3.for and ifrrigsp.for 
c jhb 2014/10/28; change code to prevent rediversion of op rule type 24 returns/spills
c                   add new option (oprlimit=-1) to opr file input
c                   that causes the type 24 results to be frozen after
c                   reop step 1.  done with a simple change to execut.for
c                   that prevents it from calling DirectEx.for on subsequent
c                   reop loops over the water right list.
c jhb 2014/10/31; skip reading secondary records if ioprsw()=0
c                 only allow plan type 11 as a type 35 destination
c jhb 2014/11/01; Revise dimensions
c   ISF rights          241-2241
c                       because the IfrRigSP routine uses the op rule counter
c                       as an index in the divi() array.  might have to go larger later.
c jhb 2014/11/20; ver='14.01.06'
c                 Fix an array index (iscd) problem in IfrRigSP when the avail water < small
c                   and a jump to line 260 is triggered
c                 fix a oprfind call missing arg problem in oprinp.for
c                   in the op rule 7 specific code
c                 the current SP model full data set now runs to completion
c ______________________________________________________________________
c       Documentation
C
C      PROGRAM LIMITS:
C
c
c rrb 99/08/26; Rio Grande
c                                          Pre-RG    RG RG-Daily
C      STATIONS                 maxsta        777  2001  2278
C      STREAM INFLOWS           maxsta        777  2001  2278
C      PRECIP STATIONS          maxpre         35   155   180  
C      EVAPOR STATIONS          maxeva         35   155   180
C      RESERVOIRS               maxres         35   155   180
C      RESERVOIR OWNERS         maxown         50   155   180
C      RESERVOIR RIGHTS         maxrsr         80   101   251
C      DIVERSION STATIONS       maxdiv        850  1110   1500
C      DIVERSION USERS          maxuse        850  1110   1500  3060
c      Diversion Owners         maxownD      2400
C      DIVERSION RIGHTS         maxdvr       2000  3500   6500
C      DIVERSION RETURNS        maxrtn       1900  1210   2001
c
c      Max Water Rights for any structure type + 1
c                               maxwr         156   156    201
c
c      RET DELAY TABLES         maxdly        200   550   3100
c
c       Note the following dimension is a subset for return data
c         to keep 2001 size yet limit daily return and depletion 
c         arrays close to the minimum required for Rio Grande
c      Total return locations (nstrtn)maxrtnA NA   725  1725
c
c      Max delay years          maxdla               20
c      Return delay months      maxdlm         30   240
c      Return delay days        maxdld        930   930 7320 (daily)
c
c      IFR Stations             maxifr        300   230
c      IFR Rights               maxfrr        300   230
c jhb 2014/11/01; Revise dimensions
c   ISF rights          241-2241
c                       because the IfrRigSP routine uses the op rule counter
c                       as an index in the divi() array.  might have to go larger later.
c      IFR nodes within all reachs maxrea     N/A  2000
C      POWER RIGHTS (inactive)  N/A            20    20
c
C      OPERATION RIGHTS         maxopr        200   501   800
C      TOTAL of all RIGHTS      maxnwr       2290  5285  14000
c
c      Max # of well stations   maxdivw       202   502   602 402
c      Max # of well rights     maxdvrw       202  4000  7500 13110
c      Max # of well return     maxrtnw       202  10000
c
c      Number of gages per
c        base flow calculations maxbas         26    26
c      Maximum rep reservoirs   maxrep          6    25
c      Maximum res accounts dimension (maxacc) 34    34
c      Maxio; dimension of binary I/O          40    40
c      Maxfutil; maximum number of futil calls 10    10
c      Maxrg; maximum number of Rio Grande opr rules  5
c      Maximum file name length maxfn          72   256
c      Maximum # of Cu groups (se outcu)       56    56
c      Maximum # of files in the response file maxfile 60 75
c      Maximum # of operating right sub water rights (maxOprWr) 11
c      Maximum # of Plans (augmentaiton and T&C)      14
c      Max # of Reservoir Returns maxrtnRP     202  10000
c      Max # of plans tied to a use (maxplnO)  20   100 No longer used
c      Max # of plan types (maxplnT)           21   21            
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
      character getid*12, fnlog*256, rec48*48, rec256b*256,
     1 rec8*8    
c jhb 2014/07/21 initialize this variable passed to xdebug and report
      integer :: nreach=0
c
c
c _________________________________________________________
c		Step 1; Initialize
c
c		ver = xx.yy.zz; where
c		     xx is the major version 
c		     yy has new functionality
c		     zz is a bug fix
c		   For example:
c			  12 has new *.ipy file
c     
c 			11 includes carrier loss, 
c				10 includes plans
c				 9 includes wells
c				 8 includes daily capability
c				 7 includes new binary output format			
c		
c
        ver = '15.00.08'
        vdate = '2017/12/22'
c
c
c 6/20/95 Code isgi=0 for PC; isgi=1 for SGI
        isgi = 0
        igui = 0
        istop = 0
        io99=99
        nlog=99
        nchk=98
        ntmp=97
c
c _________________________________________________________
c        
C
C------  SPECIFY MAXIMUM DIMENSION OF ARRAYS
C
c
c rrb 99/08/26; Rio Grande
      maxsta=5000
      maxrun=5000
      MAXPRE= 180
      MAXEVA= 180
      MAXRAI= 180
      MAXEPT= 180
      MAXRES= 180
      MAXOWN= 251
      MAXDIV=3060
      MAXUSE=3060
      MAXRTN=3920

      maxwr =201

      MaxDly=525    
      maxdlA=20  
      maxdlAd=10
      maxdlm= 240
      maxdld= 3660    
c
c rrb 2010/09/15; Add warning if years do not coincide with
c                 maximum daily and monthly return values
      if(maxdlA*12.ne.maxdlm) then     
        write(6,330) maxdlA, maxdlM, maxdla*12, maxdlM 
        goto 9999
      endif

      if(maxdlAd*366.ne.maxdld) then      
        write(6,340) maxdlAd, maxdlD, maxdlAd*366, maxdlD
        goto 9999
      endif      
      
c
c rrb 01/10/08; New variable for return array
c		Includes returns for a diversion, well, reservoir
c		and plans
      maxrtnA= 5005

      MAXIFR=  2241
      MAXFRR=  2241
      MAXRSR=  351
      MAXDVR=  6500
      MAXPWR=  51
      MAXOPR=  3701
      maxopr2= 20
      MAXNWR=  14991
      maxbas=  440                                   
      maxacc=  39
      maxrep=  25
      maxrea=  1002
      maxio =  40
      maxdivw= 402
      maxdvrw= 13110
      maxrtnW= 8500
      maxfutil=10
      maxrg=5
      maxfn=256
      maxgrp=57
      maxfile=80
      maxownD=3060
c
      maxPlan = 1440
      maxrtnPP = 1444
cx    maxPlnO=100
c
c rrb 2015-03-24
      maxPlnT=13
      maxOprin=50
      maximprt=15
      nimport=0
      maxparm=40
      maxrtnRP=180
      maxQdiv=39
c
c	Set ndivO maximum # of values to print in *.xdd      
      ndivO=38
      nreso=29
c
c rrb 2007/02/20; Add carried to Well output            
c     nwelO=18
      nwelO=19
c
c rrb 2006/12/22; Maximum reach (maxrch) & maximum years (maxyrs)
      maxrch=150
      maxyrs=200  		
c
c rrb 2011/04/04;  Initialize some daily array counters      
      numbas=0
      numdld=0 
      numgrp=0 
      numplnt=0    
c
c ---------------------------------------------------------
c 		Check the dimension used for reporting
      if(maxdiv.gt.5000) then
        write(6,90)
 90     format('  Statem; Warning outmon & outdiv assume the maximum',
     1         ' number of diversion = 5000.  Need to revise')
        call flush(6)
        write(6,*) 'Stop 1' 
        call flush(6)
        call exit(1)
        stop 
      endif
c
c _________________________________________________________
C
C------  CHOOSE MACRO OPTION : IOPTIO
C
C      IOPTIO = 1 : Baseflow use historic data to estimate
c                   baseflows
c               2 : Simulate
c               3 : Report
c               4 : Data Check
c               5 : Print Version number
c               6 : Print Help data
c               7 : Print Update information
c               8 : SimulateX (without reports)
c               9 : BaseflowX; read baseflow (natural) at gaged locations
c                   & estimate baseflows at ungaged locations
c
c-------------------------------------------------------------------
c
c
c               Open Temp log file
c rrb 03/06/02; Made an option and moved to parse 
      filenc='Statem'
      call namext(maxfn, filenc, 'log', filena)
      open(nlog,file=filena, status='unknown')
      
      write(nlog,200) ver, vdate
c
c _________________________________________________________
c
      call parse(nlog, maxfn, ioptio, ioptio2, filenc, getid)

c     call parsgi(ioptio, ioptio2, filenc, getid)
c
crrb 99/05/20;
      call getpath(maxfn, filenc, fpath1)
      
      close (nlog)
c
c
c _________________________________________________________

c               -help option
  92  if(ioptio.eq.6) then
        write(6,200) ver, vdate
        write(6,206)
  206   format(
     1 72('_'),//
     1 '        For help with StateMod see documentation and examples')         
        goto 190
      endif
c
c
c _________________________________________________________

c               -version option
      if(ioptio.eq.5) then
        write(6,200) ver, vdate
  200   format(
     1 72('_'),//
     1 '        StateMod                       '/
     1 '        State of Colorado - Water Supply Planning Model     '//
     1 '        Version: ',a8,/,
     1 '        Last revision date: ',a10,//
     1 72('_'))     
        goto 190
      endif     
c
c
c _________________________________________________________
c		- Warrenty request      
      if(ioptio.eq.10) then
        write(nlog,200) ver, vdate
        call gnu(ioptio,nlog)
        goto 190
      endif  
c
c
c _________________________________________________________
c		- Conditions of use
      if(ioptio.eq.11) then
        write(nlog,200) ver, vdate
        call gnu(ioptio,nlog)
        goto 190
      endif  
c
c
c _________________________________________________________
c		- Contact information
      if(ioptio.eq.12) then
        write(nlog,200) ver, vdate
        call gnu(ioptio,nlog)
        goto 190
      endif  
c
c
c _________________________________________________________
c               - Test information
      if(ioptio.eq.13) then
        write(nlog,200) ver, vdate
c       write(6,*) '  Statem; ioptio2 before exit = ', ioptio2
        if(ioptio2.eq.1) then
          open(1,file='TestError', status='old',err=9999)
c         write(6,*) ' Statem; Past bad open statement'
        endif
        goto 190
      endif  
      
c
c
c _________________________________________________________
c               Open log file
c rrb 03/06/02; Allow log file to be an option
      if(ioptio2.ne.20) then
        close(nlog)
        call namext(maxfn, filenc, 'log', filena)
        open(nlog,file=filena, status='unknown')
c
c
c _________________________________________________________
c		Print heading        
        write(nlog,200) ver, vdate
        write(6,200) ver, vdate

        fnlog = filena
        write(6,80) fnlog
 80     format(/,'  Opening log file ', a256)
        call flush(6)

c
c         write(6,81) ioptio, ioptio2, getid
c         write(nlog,81) ioptio, ioptio2, getid
 81       format(2x, 'Statem.for; Ioptio, Ioptio2, Getid = ', 
     1           2i5, 1x, a12)
      endif
c
c
c _________________________________________________________
c               -update option
c
c rrb 03/06/02; Make the log file an option
c     if(ioptio.eq.7) then
      if(ioptio.eq.7 .and. ioptio2.ne.20) then
        write(6,200) ver, vdate
        write(6,212)

        write(nlog,200) ver, vdate
        write(nlog,212)
        goto 190
      endif
c
c rrb 03/06/02; Print to screen if log file option is off       
      if(ioptio.eq.7 .and. ioptio2.eq.20) then
        write(6,200) ver, vdate
        write(6,212)

c       write(6,200) ver, vdate
        write(6,212)
        write(6,211)
        write(6,210)
        write(6,208)
        write(6,207)
        write(6,213)
        write(6,203)
        write(6,204)
        write(6,205)
        goto 190
      endif
c
c		Open check file (*.chk)
      call namext(maxfn, filenc, 'chk', filena)
      open(nchk,file=filena, status='unknown')      
      write(nchk,200) ver, vdate
c
c		Open temporary file (*.tmp)
      call namext(maxfn, filenc, 'tmp', filena)
      open(ntmp,file=filena, status='unknown')      
c
c _________________________________________________________

      iback = 0
 100  if(ioptio.eq.0) then
        iback = 1
        Write(6,110)
        call flush(6)
  110   format(/,
     1           ' Option? ',
     4        //,'   [0] : STOP',
     1         /,'   [1] : Baseflow',
     2         /,'   [2] : Simulate',
     3         /,'   [3] : Report',
     4         /,'   [4] : Data Check'
     1         /,'   [5] : Version',
     1         /,'   [6] : Help',
     1         /,'   [7] : Update',
     1         /,'   [8] : SimulateX (without reports)',
     1         /,'   [9] : BaseflowX (ungaged locations only)')

c
c rrb 10/27/94 Additional Output
        write(6,*) ' '
        read (5,*,err=165) ioptio
      endif

      if(ioptio.eq.0 .or. ioptio.eq.6) goto 170
c
c _________________________________________________________
c
c 00/04/13; Baseflowx option
c     if(ioptio.gt.4 .and. ioptio.ne.8) then
      if(ioptio.gt.4 .and. ioptio.lt.8) then
c
c rrb 2008/09/16; Allow version        
cx        write(6,*) ' ** Invalid option (Version, Help, or Update)' 
cx        write(6,*) ' ** Try again **'
cx        call flush(6)
cx        ioptio = 0        
        goto 92

        if(isgi.eq.0) then
          goto 100
        else
          goto 170
        endif
      endif
c ______________________________________________________________________
c rrb 00/10/30; Add random input file capability
c rrb 02/08/08; Add *.rsp extension only if a .xxx does not exist
c     call namext(maxfn, filenc, 'rsp', filena) 
      irsp=0
      do i=1,72
        if(filenc(i:i).eq.'.') irsp=1
      end do
      if(irsp.eq.0) call namext(maxfn, filenc, 'rsp', filena) 
      rec48='Response File (*.rsp)'
      write(nlog,101) rec48, filena
 101    format(/,72('_'),/,'  StateM; ', a48,/,5x,a256)
 102    format(/,72('_'),/,'  StateM; ', a48,/,5x,a8)
      rec48='Path'
      if(fpath1(1:1) .ne. ' ') then
        write(nlog,101) rec48, fpath1
      else
        rec8='None    '
        write(nlog,102) rec48, rec8
      endif  
      open(20,file=filena,status='old',err=9997)
      call skipn(20)
      IIN=20
c	Check and if positive read all response files
c	in any order
      call Getfn(iin, nlog, infile, maxfile, 
     1    fileName, ifileNum, filetype, fileSuf)
      close(iin)
c ______________________________________________________________________
c		Set parameter values      
      call setpar(maxparm, nlog, paramd, paramr, paramw)

      GO TO (130,140,150,160,166,166,166,140,130) IOPTIO
c ______________________________________________________________________
c               Baseflow option
  130 write(nlog, 132) 'Baseflow  '
  132 format(/,72('_'),/'  Statem; Option Specified = ', a10)
      call virgen
      goto 166
c ______________________________________________________________________
c               Execute Option
  140 write(nlog, 132) 'Simulate  '
      call execut    
      goto 166
c ______________________________________________________________________
c               Report Option
  150 write(nlog, 132) 'Report    '
      call report(igui, istop, ioptio2, getid, nreach)
      goto 166
c ______________________________________________________________________
c               Check Option
  160 write(nlog, 132) 'Check     '
      call xdebug(nreach)
      goto 166
  165 write(6,*) ' ** Invalid option, try again **'
      call flush(6)
      goto 166               
c               Go back to menu if in default mode
  166 if(iback.eq.1) then
        close(20)
        ioptio = 0
        goto 100  
      endif                 
  170 write(6,180) fnlog
      call flush(6)
  180 format('  Statem; See detailed messages in file: ', a256) 
  190 write(6,*) 'Stop 0'
      call flush(6)
      call exit(0)
      stop 
c ______________________________________________________________________
c     Formats
  212   format(//
     1 ' Recent updates',/
     1 '    - 2017/12/22 (15.00.08)',/
     1 '      Revised GetIpy4 & Mdainp to read an unknown number',/
     1 '        of records to allow the *.ipy file to have ',/
     1 '        significantly more structures than those contained',/
     1 '        in the data',/     
     1 '    - 2017/12/18 (15.00.07)',/
     1 '      Revised GetIpy4 & Mdainp to read 5000 records to',/
     1 '        allow the *.ipy file to have significantly more',/
     1 '        structures than those contained in the data',/
     1 ' Recent updates',/
     1 '    - 2017/12/11 (15.00.06)',/
     1 '      Revised the following to limit screen output',/
     1 '        for Gfortran: ',/
     1 '        outdivw, outxss, outres, execut',/
     1 '      Revised the following to verify or delete',/
     1 '        edits by Jim Brannon (jhb)',/
     1 '        divcarl, divcar1, divresp2, divrplp, mdainp',/
     1 '        outmon, outpln, outplnmo, riginp, rsrspu',/
     1 '    - 2017/12/07 (15.00.05)',/
     1 '      Revised Rsrsup (type 6) to correct an array value =0',/
     1 '        and ensure the diversion limit adjustment works OK',/     
     1 '    - 2017/12/03 (15.00.04)',/
     1 '      Revised Outmon to correct a reporting ',/
     1 '        issued in *.xdd when diverting to a reservoir',/
     1 '        with losses that occurred after the edits',/
     1 '        to DivResp2 (type 27) in 15.00.03',/
     1 '      Revised DivRplP (type 28) to include consistent',/
     1 '        edits performed to DivResp2 in 15.00.03',/
      
     1 '    - 2017/10/20 (15.00.03)',/
     1 '      Revised SetPar to include River_Release',/
     1 '        River_Divert, Reservoir_Carry and ',/
     1 '        Reservoir_Loss'/
     1 '    - 2017/10/20 (15.00.02)',/
     1 '      Revised DirectWR to turn off detailed output',/
     1 '      Revised DivResp2 typo (lopr =lopr6)',/
     1 '      Revised DivResp2 to include variable Resloss',/
     1 '      Revised the following identified by a detailed',/
     1 '        compilation',/
     1 '        a) Directwr initialized ipuse',/
     1 '        b) Directwr initialized iuse2x',/
     1 '        c) Divrpl   initialized monout',/
     1 '        d) Dnmfso   initialized nlog and nlogx',/
     1 '        e) Ifrrigsp initialized idcd2',/
     1 '        f) Splatte  revised an output to be < 72 characters',/
     1 '      Revised Outmon to initialize rlossc & rlossr when',/
     1 '        when printing reservoir account data',/
     1 '      Revised Divresp2(type 27) to add loss to diversion',/
     1 '        by other (accr(4 and loss (accr(27',/

     1 '      Miscellaneous Revisions 1-10',/
     1 '    - 2015/10/18 (15.00.01)',/
     1 '      Revised qdiv reporting for Type 32, DivresR.f',/
     1 '    - 2015/10/18 (15.00.00)',/
     1 '      Copied version 14.02.27 to version 15.00.00',/
     1 '        to signify testing and comparison to historic',/
     1 '        results following the transfer to GitHub and',/
     1 '        addition of the Changed Water Right (typw 26)',/
     1 '        operating rule.',/
     1 '      Minor clean up to DirectWR (type 26), DivResP2',/
     1 '        (type 27) and DivrplP (type 28) to remove some',/
     1 '        confusing notes',/
     1 '    - 2015/10/10 (14.02.27)',/
     1 '      Revised DirectWR (type 26) to correct a problem',/
     1 '        associated with the one operation per time step',/
     1 '        implementation',/
     1 '      Revised PowSeaP (type 29) to correct a typo to qdiv',/
     1 '        that impacts reporting',/
     1 '    - 2015/10/04 (14.02.26)',/
     1 '      Revised DirectEX (type 24), DirectBy (type 25),'/ 
     1 '        DivResP2 (type 27) and DivrplP (type 28) to report'/
     1 '        water bypassed or exchanged as at the source as',/
     1 '        Carried (qdiv(38,__) that does not enter into the',/
     1 '        water budget reporting',/
     1 '      Revised PowSeaP to report water spilled from a',/
     1 '        reuse plan to a Diversion (type 4), Reuse Plan to a',/
     1 '        diversion from Tmtn and an Admin Plan (11) as a',/
     1 '        Return flow for water budget reporting',/
     1 '    - 2015/09/11 (14.02.25)',/
     1 '      Revised RsrSpu (type 6) and RsrSpuP (type 34) to ',/
     1 '        correct variable IresT1 that is used by subroutine',/
     1 '        Accou and was wrong when water is being distributed',/
     1 '        to only 1 account'
     1 '      Revised RsrSpu (type 6) to correct a problem that ',/
     1 '        did not allow the logic that limits reoperaton once',/
     1 '        per iteration to work properly',/    
     1 '    - 2015/09/06 (14.02.24)',/
     1 '      Revised DivResp2 (type 27) and DivRplP (type 28) to ',/
     1 '        correct a roundoff issue that caused an inconsistency',/
     1 '        associated with the Replacement rule (type 10)',/
     1 '        reporting between the *.xop, *.xre & *.xrp reports',/     
     1 '    - 2015/08/23 (14.02.23)',/
     1 '      Revised DivResp2 (type 27) to correct a problem ',/
     1 '        associated with porting the routine to the',/
     1 '        gfortran compiler that impacted how variable ishort',/
     1 '        was set that impacted the operation of Replace',/
     1 '        (type 10)',/
     1 '      Revised PowseaP, the Reservoir or Plan Spill operating',/
     1 '        rule to not adjust Avail at the reservoir itself',/
     1 '        if the source is a reservoir. This is consistent with',/
     1 '        a Spill to Reservoir Target operating rule (type 9)',/     
     1 '    - 2015/08/11 (14.02.22)',/
     1 '      Revised RsrSpu.f (type 6) to set qres(29,n) for all',/
     1 '        reservoir to reservoir bookovers to correct a water',/
     1 '        balance reporting problem identified in the San Juan',/
     1 '      Revised DivMulti.f (type 46) to not set qres(35,n) if',/
     1 '        the source is a Changed Water Right Plan (type 13)',/
     1 '        to correct a water balance reporting problem ',/
     1 '        identified in the North Platte Model',/
     1 '    - 2015/07/27 (14.02.21)',/      
     1 '      Revised Outmon.f to correct a problem with reporting',/
     1 '        reservoir releases from a type 27 rule by account ',/     
     1 '    - 2015/07/18 (14.02.20)',/      
     1 '      Revised Outmon.f to correct a problem with reporting',/
     1 '        reservoir releases from a type 27 rule for the ',/
     1 '        total reservoir ',/
     1 '      Revised Divres.f (type 2 & 3) to correct a problem',/
     1 '        reporting reservoir releases associated with the',/
     1 '        above edit.',/
     1 '      Revised DirectWR.f (type 26) to fix a typo to ',/
     1 '        allow only one operation per time step that was',/
     1 '        implemented in version 14.02.18',/                                
     1 '      Revised Oprinp.f to warn the user if ther are type 6',/
     1 '        operating rules that book water from a reservoir to',/
     1 '        itself then back that might cause a reoperation',/
     1 '        problem observed on the San Juan if not corrected',/
     1 '        per the documentation',/
     1 '    - 2015/07/08 (14.02.18)',/                                 
     1 '      Revised RsrSpu.f (type 6) & Bomsec.f, DaySet.f &',/
     1 '        common.inc to limit reoperation of a bookover rule)',/
     1 '        when another opr. rule has already booked water',/
     1 '        back to the original account.',/
     1 '      Revised DirectWR.f (type 26) & Execut.f to correct a ',/
     1 '        problem on how reoperation per time step is ',/
     1 '        controlled by water right icallOP(l2) not operating',/
     1 '        rule (icall26)',/
     1 '    - 2015/06/25 (14.02.17)',/                                 
     1 '      Added detailed reporting capability to subroutines:',/
     1 '        DivRes.f, ResRg1.f, RsrSpu.f, OutIchk.f, ChekRes.f,',/
     1 '        and DivCarl.f',/
     1 '        Began enhancements to control when a reservoir book',/ 
     1 '        over should be limited to one operation per time step',/
     1 '        to resolve a problem indentified in the San Juan',/        
     1 '    - 2015/06/15 (14.02.16)',/                                 
     1 '      Revised DsaMod to correct a problem that originated',/
     1 '        with porting the code to a new compiler with version',/
     1 '        14.00.00 that materalized when a direct flow water',/
     1 '        right is non-consumptive',/
     1 '    - 2015/05/06 (14.02.15)',/
     1 '        Revised DirectWr to limit % of water right left',/
     1 '        at headgate by the source structure capacity',/
     1 '    - 2015/04/07 (14.02.14)',/
     1 '      Enhanced warnings initiated in previous version',/
     1 '    - 2015/03/14 (14.02.13)',/   
     1 '      Revised Oprinp to print warnings for certain ',/
     1 '        combinations of the variable oprlimit ',/
     1 '    - 2015/02/14 (14.02.12)',/   
     1 '      Revised to correct minor issues assocaied with',/
     1 '        prior version',/
     1 '    - 2015/02/14 (14.02.11)',/   
     1 '      Revised Type 27 direct release (DivResP2.f) to correct',/
     1 '      minor problems associaed with version 14.02.10',/
     1 '    - 2015/03/07 (14.02.10)',/
     1 '      Revised Oprinp.f to allow type 27 Direct Release and',/
     1 '        type 28 Release by Exchange to allow variable',/
     1 '        Oprlimit to be 5-9 not varaible creuse when the',/
     1 '        source is a Changed Water Right Plan (type 13).',/
     1 '        Revised type 27 Direct Release (DivResP2.f) and',/
     1 '        type 28 Release by Exchange (DivRplP.f) to allow',/
     1 '        OprLimit = 5-9 and not use varaible creuse.',/
     1 '      Revised GetPln to allow a Changed Water Right Plan',/
     1 '        (type 13)',/
     1 '    - 2015/02/03 (14.02.09)',/   
     1 '      Revised Type 27 direct release (DivResP2.f) and ',/
     1 '        Type 28 release by exchange (DivRplP.f) to allow',/
     1 '        OprLimit = 1, 2, 3 and 4.  Also fixed ',/
     1 '        a problem with version 14.02.08 that did not allow ',/
     1 '        return flows to be calculated. ',/ 
     1 '    - 2015/02/03 (14.02.08)',/    
     1 '      Revised Oprinp.f and OprInOut.f to allow a Type 27 ',/
     1 '        direct release (DivResP2.f) and Type 28 release by ',/
     1 '        exchange (DivRplP.f) to read the source operating',/
     1 '        rule as variable cReuse and implemented oprlimit = 4',/
     1 '        that is a combination of types 2 and 3',/
     1 '    - 2015/01/24 (14.02.07)',/    
     1 '      Revised *.xdd reporting for a release by Divresp2',/
     1 '       (type 27) or by exchange by DivRplP (type 28)',/
     1 '        when the source is a type 26 (DirectWR)',/      
     1 '        Also, to improve readability, removed miscellaneous',/
     1 '        comments related to reporting in DirectWR, DivresP2',/
     1 '        DivRplP, DivMulti and PowseaP',/     
     1 '    - 2015/01/24 (14.02.07)',/
     1 '      Revised *.xdd to not report water taken to an admin',/
     1 '        plan by DirectWR (type 26).  Instead only report when',/
     1 '        released from an admin plan direct by Divresp2',/
     1 '       (type 27) or by exchange by DivRplP (type 28)',/      
     1 '    - 2015/01/20 (14.02.05)',/
     1 '      Revised *.xdd reporting for water taken to an admin',/
     1 '        as Carried-Exchange-Bypass only in DirectWR (type 26)',/
     1 '        and when released from an admin plan direct by',/
     1 '        Divresp2 (type 27) to by exchange by DivRplP (type 28)',/  
     1 ' Recent updates',/
     1 '    - 2015/01/16 (14.02.04)',/
     1 '      Revised DirectWR, PowseaP & DivMulti to clean up ',/
     1 '        reporting for a diversion to Admin Plan (type 11)',/ 
     1 '    - 2015/01/10 (14.02.03)',/
     1 '      Revised PowseaP (a type 29 Spill from a from an admin',/
     1 '        plan) to be reported in *.xdd as if hte diversion',/
     1 '        never occurred (e.g. From River by Other and River ',/
     1 '        Divert = 0.0',/
     1 '    - 2014/12/14 (14.02.02)',/
     1 '      Revised Oprinp.f logic used to read the Type 26',/
     1 '        destination',/    
     1 '    - 2014/12/14 (14.02.01)',/
     1 '      Revised Oprinp.f to include the following checks:',/
     1 '        1. A type 27 (from a plan direct) with an admin'/
     1 '           plan source (type 11) has oprlimit=5 and a ',/
     1 '           source water right provided in row 4',/
     1 '        2. A type 28 (from a plan exchange) with an admin'/
     1 '           plan source (type 11) has oprlimit=5 and a ',/
     1 '           source water right provided in row 4',/
     1 '        3. A type 29 (spill) rule with an admin',/
     1 '           plan source (type 11) has a spill location',/
     1 '           specified as the destination.',/  
     1 '    - 2014/11/24 (14.02.00)',/
     1 '      Added a Changed Water Right (type 26) operating rule',/
     1 '        as follows:',/
     1 '        added DirectWR.f the type 26 operating rule',/
     1 '        revised Oprinp.f to read a type 26 rule',/
     1 '        revised Execut.f to call a type 26 rule',/
     1 '        revised Setqdiv.f to process a type 26 rule',/ 
     1 '      Revised DivResP2.f (type 27), DivRplP.f (type 28) and'/,
     1 '        PowSeap.f (type 29) to report water spilled',/
     1 '        from a type 11 plan as qdiv(37,xx) and a negative',/
     1 '        diversion in outmon.f, not return flow (qdiv(36,xx)',/
     1 '      Revised Oprinp.f to llow oprlimit = 5 in order ',/
     1 '        allow the capacity to be reduced when water is',/
     1 '        released from a plan',/
     1 '      Revised DivResP2 (type 27) and DivRplP (type 28) to',/
     1 '        allow oprlimit = 5 and reduce the capacity of the',/
     1 '        source structure assigned to a changed water ',/
     1 '        right (type 26)',/
     1 '    - 2014/11/20 (14.01.06)',/
     1 '      Fixed an array index problem in IfrRigSP',/
     1 '      Fixed an opr type 7 specific code problem in OPRInp',/
     1 '    - 2014/11/01 (14.01.05)',/
     1 '      Revise ISF rights dimensions from 241 to 2241',/
     1 '        because the IfrRigSP routine uses the op rule counter',/
     1 '        as an index in the divi() array.',/
     1 '    - 2014/10/31 (14.01.04)',/
     1 '      skip reading secondary records if ioprsw=0 in opr file',/
     1 '      only allow plan type 11 as a type 35 destination',/
     1 '    - 2014/10/28 (14.01.03)',/
     1 '      added op rule type 24 reop control flag in opr file',/
     1 '         to freeze type 24 results after reop step 1',/
     1 '         to prevent rediversion of upstream returns/spills.',/
     1 '    - 2014/10/24 (14.01.02)',/
     1 '      fixed more array bounds errors that cropped up during',/
     1 '        model testing',/
     1 '    - 2014/xx/xx (14.01.01)',/
     1 '      ',/
     1 '    - 2014/09/05 (14.01.00)',/
     1 '      merged new type 35 branch into master:',/
     1 '        operating rule 35 changed to deliver from import plan',/
     1 '        (plan type 7) to accounting plan (plan type 11)',/
     1 '        WITHOUT REUSE. Modeler can split and add reuse after',/
     1 '        water is in acct plan. This will become default (only)',/
     1 '        type 35 operating mode and will be documented as such.',/
     1 '        This branch also has the return flow calculation fix,',/
     1 '        See details in code comments in statem.for.',/
     1 '      merged type 45 branch into master:',/
     1 '        changes to operating rule 45 to allow carrier water',/
     1 '        to be seen in the river.',/
     1 '      operating rule 27 delivery to isf node or reach.',/
     1 '    - 2014/08/26 (03new45)',/
     1 '      Added changes from Ray B to carrier code, type 45',/
     1 '      For testing ...',/
     1 '    - 2014/08/19 (New35Rtn)',/
     1 '      Experimental branch to change operating rule 35',/
     1 '      is now working, testing ongoing.',/
     1 '      Also (un)fixed a return flow calculation change,',/
     1 '      fix to array bounds problem with daily delay patterns',/
     1 '      broke return flow calculations. revert to original code',/
     1 '    - 2014/08/18 (.03New35)',/
     1 '      Experimental branch to change operating rule 35',/
     1 '      change it to deliver imported water to accounting plan',/
     1 '      (type 11) and skip the reuse plan,',/
     1 '      modeler can handle reuse in the acct plan if needed',/
     1 '    - 2014/07/25 (14.00.03)',/
     1 '      Allow isf reaches to overlap (partially/completely)',/
     1 '      Update admin plan output when split by type 46 op rule',/
     1 '    - 2014/07/23 (14.00.02)',/
     1 '      Fixed several array bounds issues found during testing',/
     1 '    - 2014/07/14 (14.00.01)',/
     1 '      Updated ISF reach to work with multiple water rights.',/
     1 '        This allows simulating overlapping ISF reaches.',/
     1 '    - 2014/07/14 (14.00.00)',/
     1 '      Array sizes increased:',/
     1 '        diversions from 1530 to 3060',/
     1 '        plans from 720 to 1440',/
     1 '        stations from 2500 to 5000',/
     1 '      More code changes to avoid compiler warnings',/
     1 '        and array bounds errors caused by data.',/
     1 '      Recent code changes tested extensively and deemed',/
     1 '        stable, so started a major new version - 14.',/
     1 '    - 2014/07/09 (13.00.02)',/
     1 '      StateMod code development moved to gfortran compiler,',/
     1 '        eclipse IDE with photran plugin, git version control.',/
     1 '      Now supports reading StateCU data files (IPY, etc.)',/
     1 '        with many more structures than the current network.',/
     1 '      Recent Type 24 updates allowing shortages to be shared.',/
     1 '      Many changes to avoid compiler and array bounds errors',/
     1 '        caused by unexpected data.',/
     1 '    - 2012/05/31 (13.00.01)',/
     1 '      Revised Oprinp and Divrpl (type 4) to allow variable',/
     1 '        iopdesR to be the destination type and iopdes(3,k) to',/
     1 '        be the water right limit',/
     1 '    - 2012/02/15 (13.00.00)',/
     1 '      Changed version to 13 to indicate major testing',/
     1 '        of plan operations have been completed',/
     1 '    - 2012/01/20 (12.3027)',/
     1 '      Revised Oprinp (type 27) to allow the ability to',/
     1 '        read an ISF destination',/
     1 '    - 2011/11/29 (12.3026)',/
     1 '      Revised DivResR (type 33) and DivRplR (type 34) to ',/
     1 '        Oprinp to allow an instream flow node or reach as',/
     1 '        a destination',/
     1 '      Revised Oprinp type 13, La Plata Compact, to read',/
     1 '        an ISF water right that got left out when it was ',/
     1 '        enhanced to use generic read routine in version 11.43',/
     1 '    - 2011/11/27 (12.3025)',/ 
     1 '      Revised DirectBy (type 25) to correct a problem',/
     1 '        when a carrier is specified that did not allow',/
     1 '        avail to be adjuste from the source to the carrier',/
     1 '    - 2011/11/15 (12.3024)',/
     1 '      Revised convergence logic for carrier with losses',/
     1 '        in Rivrtn and increased the maximum number of',/
     1 '        iterations allowed per time step in Execut',/
     1 '    - 2011/10/15 (12.3023)',/
     1 '      Revised DivCarL (type 45), Oprinp and SetLimit to',/
     1 '        allow a link to a Type 47 operating rule with',/
     1 '        monthly and annual limits.',/  
     1 '      Revised Mdainp.f for a warning printout',/  
     1 '    - 2011/08/22 (12.3022)',/
     1 '      Revised DirectEx (type 24) and DirectBy (type 25)',/
     1 '        to correct a reporting problem when the destination',/
     1 '        is a plan',/       
     1 '    - 2011/08/04 (12.3021)',/
     1 '      Revised DivResP2 (type 27) to handle an ISF Reach',/     
     1 '      Revised DivRplP (type 28) to handle an ISF Reach',/
     1 '      Revised OutIFR to correct a problem with the ISF',/
     1 '        reach report (*.xir)',/
     1 '    - 2011/07/28 (12.3020)',/
     1 '      Revised Oprinp to warn the user if the destination',/
     1 '        is an admin plan (type 11) and no plan spill ',/
     1 '        (type 29) operating rule has been specified',/   
     1 '      Revised DivRplP (type 28) to correct a problem ',/
     1 '        when the source plan is located downstream of',/
     1 '        the destination',/  
     1 '      Revised Oprinp and Oprfind to pass the operating',/
     1 '        rule on/off switch (ioprsw(k)) and turn off the',/
     1 '        rule if the souce is turned off',/
     1 '    - 2011/07/20 (12.3019)',/
     1 '      Revised Divcar (type 11) to correct a problem',/
     1 '        when the source is a reservoir water right',/
     1 '        and the default diversion location is specified',/
     1 '      Revised Rivrtn to to pass ndns, the',/
     1 '        # of downstream nodes, to allow ',/
     1 '        Dsamod.f to search an exchange reach',/ 
     1 '        Note this routine was missed in the ',/
     1 '        version 12.3017 update',/
     1 '    - 2011/05/31 (12.3018)',/ 
     1 '      Revised type 25 (DirectBy) & type 24 (DirectEx)',/
     1 '        to correct a problem when simulating a carrier',/ 
     1 '      Revised setloss, setcarL, & setqdivc to allow up to',/
     1 '        10 accounts (e.g. intern(3701,20))',/ 
     1 '    - 2011/05/19 (12.3017)',/ 
     1 '      Revised Common to allow up to 10 accounts (e.g.',/
     1 '        iopdes(10,3701) = iopdes(20,3701), etc.',/
     1 '      Revised Splatte.f to call Dsamod',/
     1 '      Revised RtnMax.f & RtnSecX to pass ndns, the',/
     1 '        # of downstream nodes so they can be used by',/
     1 '        Dsamod.f to search an exchange reach',/ 
     1 '      Added OutIchk and revised Resrg1 & RtnSec &',/
     1 '        Execut to enhance detailed output for ichk=4',/
     1 '        and ichk=9',/
     1 '    - 2011/05/04 (12.3016)',/ 
     1 '      Revised Type 40 (Splatte.f) to limit a diversion',/
     1 '        to the destinations decreed amount and',/
     1 '        not be called if junior to the compact',/
     1 '      Revised Execut.f for detailed output (ichk=4',/
     1 '    - 2011/04/04 (12.3015)',/ 
     1 '      Revised Mdainp.f to stop if the control variable ',/
     1 '        itsfile is set to expect a *.ipy file but a *.ipy',/
     1 '        file is not provided',/  
     1 '      Revised OutCu.f, StateM.f & Common.inc to enhance the',/
     1 '        array size reporting',/ 
     1 '      Revised Execut to only call a type 40 (Splatte) when a',/
     1 '        diversion is short or the destination is an instream',/
     1 '        flow',/       
     1 '      Revised Type 40, Splatte.f, to correct a typo that',/
     1 '        impacted return flow calculations',/  
     1 '    - 2011/02/17 (12.3014)',/ 
     1 '      Revised Carrpl.f (type 7) reporting from a reservoir',/
     1 '        to a reservoir by a carrier to correct a problem',/
     1 '        in water balance reporting',/
     1 '      Revised DivResP2, DivRplP & OutDivC to correct a ',/
     1 '        reporting problem if the destination is a reservoir',/
     1 '        or instream flow that impacted the total diversion',/
     1 '        adjustment in the water balance report',/
     1 '      Revised Outdivc.f to include structures with multiple',/
     1 '        activities at the same node',/                                   
     1 '      Revised Outbal2.f to add footnote 6 in the *.xwb ',/
     1 '        output to match the total presented in the *.xdc',/
     1 '        output. ',/
     1 '      Revised Outdivc.f to add Carrier Type A in the *.xdc ',/
     1 '        output to match the total presented in the *.xwb',/
     1 '        output. ',/ 
     1 '    - 2011/02/13 (12.3013)',/
     1 '      Finalized South Platte Compact (Ifrrigsp & Splatte)',/
     1 '    - 2011/02/02 (12.3012)',/       
     1 '      Revised PowSeaP (29) approach to spill a type 11 plan',/ 
     1 '    - 2011/02/01 (12.3011)',/
     1 '      Revised Oprinp to correct a problem when reading',/
     1 '        a source water right for a type 25 rule',/
     1 '    - 2011/01/11 (12.3010)',/
     1 '      Enhanced S Platte operation to be two separate',/
     1 '        rules; ifrrigsp.f (type 50) to store and splatte.f',/
     1 '        (type 40) to release',/  
     1 '    - 2011/01/11 (12.3009)',/
     1 '      Revised DivAlt (type 39) to correct a problem when',/
     1 '        the diversion is limited to the flow at the',/
     1 '        source water right',/
     1 '    - 2011/01/06 (12.3008)',/
     1 '      Added Splatte.f & IfrrigSp.f and edited Oprinp.f',/
     1 '        Oprinp.f, SetPlanO.f & OutplanMo.f to begin to',/
     1 '        simulate the S Platte Compact.',/
     1 '      Revised Ifrrig, Ifrrig2, to divert 0 if the ',/
     1 '        destination ISF is off.  Also revised the 12.3007',/
     1 '        edit to Powres & Powers2 if the destination ISF is',/ 
     1 '        off',/     
     1 '    - 2010/12/26 (12.3007)',/
     1 '      Revised Powres & Powres2 (type 1) to divert 0 if the ',/
     1 '        destination ISF is off',/
     1 '      Revised DivAlt (type 39) to limit diversions to the',/
     1 '        capacity of the alternate point',/
     1 '      Revised DivAlt (type 39) to not override variable',/
     1 '        idemtyp from the control file',/
     1 '    - 2010/12/05 (12.3006)',/
     1 '      Revised WelRig3P to correct a problem calculating',/
     1 '        an augmentation plans demand (pdem) when the flow',/
     1 '        from ground water is > 0 to offset a negative flow',/
     1 '      Replaced reals used as characters throughout (e.g.',/
     1 '        divnam(i,j) = divnam1(j)',/  
     1 '    - 2010/11/15 (12.3005)',/     
     1 '      Revised DivAlt (39) to allow the alternate point',/
     1 '        to be a well',/
     1 '      Created SetWel to set demand data for Divalt (39)',/
     1 '      Created SetGW to set From GW when the well depletion',/
     1 '        causes the system to go negative',/
     1 '      Revised DivCarL (45) to allow canal loss to be',/
     1 '        routed to the stream using a return flow pattern',/
     1 '        specified for a plan',/
     1 '      Revised RtnXcuP to allow plan return data to be used',/
     1 '    - 2010/11/01 (12.3004)',/     
     1 '      Revised DivCarL (45) to allow canal loss to be',/
     1 '        assigned to a recharge plan',/
     1 '      Added RtnXcuP to assign canal loss to a recharge',/
     1 '        plan',/
     1 '      Revised RtnCarry to pass iplan and call RtnXcuP',/ 
     1 '      Revised PowresP (48) and DivRplP2 (49) to shepherd',/
     1 '        a Reservoir or Plan release to a plan',/
     1 '    - 2010/10/31 (12.3003)',/      
     1 '      Revised Rivrtn to handle a depletion release',/ 
     1 '        to correct a mass balance issue on the Thorton',/
     1 '        application',/
     1 '      Revised Call Rivrtn from Directby (25), Directex (24)',/
     1 '        Divcarl (45), Divresp2 (27) and Divrplp(28)',/
     1 '      Revised type 27 DivResP2 & 28 DivRplP to correct a ',/
     1 '        problem with setting qdiv(36 (return to river)',/ 
     1 '        when making a depletion release',/
     1 '      Revised type 27 DivResP2 to correct a problem with',/
     1 '        call SetQdivC when making a depletion release',/
     1 '      Revised SetLoss, SetQdiv & SetQdivC to inlcude system',/
     1 '        loss (tranloss) at the destination, not the source',/
     1 '      Revised Outmon to report From River Loss (qres(30',/
     1 '      Revised DivResp2 (27) diversion with loss (divactL &',/
     1 '        divafL) to include system loss at the destination',/
     1 '        and reservoir loss qres(27 & qres(30)',/
     1 '      Revised PowSeaP (29) to allow a type 11 to spill',/
     1 '        consistently (at all locations)',/
     1 '      Revised Carrpl (7) to correct a water balance reporting',/
     1 '        problem with operating a carrier by exchange',/
     1 '    - 2010/10/15 (12.3002)',/  
     1 '      Revised type 32 DivResR to correct a problem with',/
     1 '        call SetQdivC',/
     1 '      Revised column headers in *.xdd as follows:',/
     1 '        From Carrier by Sto_Exc is now From Carrier by Other',/
     1 '        Water Use Total Return is now Water Use To Other',/
     1 '      Revised column headers in *.xre as follows:',/
     1 '        From River by Exc_Pln is now From River by Other',/
     1 '        From Carrier by Sto_Exc is now From Carrier by Other',/
     1 '      Revised SetPar to recognize the new column names',/
     1 '    - 2010/09/11 (12.3001)',/                          
     1 '      Revised Setqdiv when called by a type 49 rule',/
     1 '        to correct a problem with the water balance',/
     1 '      Revised Riginp to prevent reservoir rights from',/
     1 '        being turned off (it somehow had been revised to ',/
     1 '        turn off all reservoir rights)',/
     1 '      Revised GetplnW to include variable dimensions',/
     1 '      Miscellaneous clean up of compiler warnings and',/
     1 '        code comments',/
     1 '    - 2010/08/29 (12.3000)',/     
     1 '      Compiled under Lahey Professional 7.2',/
     1 '      Revised plan dimensions maxPlan 102 = 501',/
     1 '        and maxrtnPP 402 = 502',/ 
     1 '      Revised Outmon to fix a problem when reporting',/
     1 '        depletion data that showed up with an application',/
     1 '        that had data in the plan return file (*.prf)',/ 
     1 '    - 2010/02/08 (12.2931)',/     
     1 '      Revised Report to fix a problem with Call Riginp',/
     1 '        and to require a random response file (*.rsp)',/
     1 '        Revised several subroutines with array issues',/
     1 '        after compling and testing with the detailed ',/
     1 '        -trk (track) and -chk (check) compiler options on',/
     1 '    - 2010/01/25 (12.2930)',/     
     1 '      Revise Divcar, & Divcar1 to include a reoperation',/
     1 '        check. Added OutGVC to provide detailed',/
     1 '        output for the Grand Valley Check',/     
     1 '    - 2009/11/05 (12.29.29)',/     
     1 '        Revise Divcar, Divcar1, DivcarR & DsaMod to',/
     1 '        to allow CU when rtnmax is called the second time',/    
     1 '    - 2009/10/25 (12.29.27)',/     
     1 '      Revise DsaMod to reset the iterate due to return',/
     1 '        flow indicator (ireop=0)if no diversion occurrs',/
     1 '    - 2009/10/20 (12.29.26)',/
     1 '      Revised DsaMod to allow a diversion when avail',/
     1 '        is zero downstream but the diversion is ',/
     1 '        non-consumptive. Note required for the Orchard',/
     1 '        Mesa Check operation on the Colorado, etc.',/
     1 '    - 2009/08/12 (12.29.25)',/
     1 '      Revised type 11 (Divcar) to correct a problem',/
     1 '        when multiple operating rules use the same'/
     1 '        water right as a source',/ 
     1 '    - 2009/06/26 (12.29.24)',/
     1 '      Revised type 7 (carrpl) to correct another problem',/
     1 '        related to recognizing the destination operating',/
     1 '        rule may divert at a location other than the source',/
     1 '    - 2009/06/25 (12.29.23)',/
     1 '      Revised type 7 (carrpl) to recognize the destination',/
     1 '        operating rule may divert at a location other',/
     1 '        than the source (e.g. if the destination is a type',/
     1 '        11 operating rule variable ciopso(2) is not NA',/
     1 '      Revised the header on the diversion station',/
     1 '        output from From River By Exc_Pln to From',/
     1 '        River By Other to recognize this column contains',/
     1 '        diversions by an exchange, plan & carrier',/
     1 '      Revised DivcarL to adjust a carriers capacity',/
     1 '        when the source is a reservoir water right',/
     1 '        diverted at a carrier',/
     1 '      Revised Vircom, WelRig3P, and Welrig3P to fix a',/
     1 '        problem when calculating CU for well only lands',/
     1 '        with sprinklers.',/   
     1 '      Revised WelRig3P, and Welrig3P to fix a',/
     1 '        problem when simulating well pumping when the',/
     1 '        demand type = 3 (Structure Demand) where the',/
     1 '        demand for D&W lands is in *.ddm and the demand',/
     1 '        for well only lands is in *.wem',/
     1 '    - 2009/06/09 (12.29.22)',/
     1 '      Revised SetQdivC and its calling routines to correct',/
     1 '        the variables passed for local plan arrays',/
     1 '      Revised Riginp and its calling subroutines to include',/
     1 '        more information for reservoir right processing',/
     1 '    - 2009/06/01 (12.29.21)',/
     1 '      Revised type 2 ResRg1 to decrease available right',/
     1 '        to correct a problem with convergence when ',/
     1 '        operating in a daily mode',/
     1 '      Revised Dayset to recalculate a reservoir storage',/
     1 '        right when operating in a daily mode and there',/
     1 '        is no administration date set (rdate = -1)',/
     1 '      Revised DayOutR to correct a problem when printing',/
     1 '        reservoir data',/     
     1 '    - 2009/05/31 (12.29.20)',/
     1 '      Revised Mis routines to remove variable ipy that is',/
     1 '        not used',/
     1 '    - 2009/05/12 (12.29.19)',/
     1 '      Revised RivRtn to exit when the change in diversion',/
     1 '        is less than small (0.001 cfs)',/
     1 '    - 2009/05/11 (12.29.18)',/
     1 '      Revised type 24 (DirectEx) & type 25 (DirectBy)',/
     1 '        to correct a problem when the water designated for',/
     1 '        exchange or bypass is unused and is made available',/
     1 '        to the source structure',/
     1 '    - 2009/04/24 (12.29.17)',/
     1 '      Revised DayOutR to correct a problem with daily',/
     1 '        reporting of River by Priority River by Exchange',/
     1 '        and River Divert',/
     1 '      Revised Mdainp to set soil moisture = 0 for ',/
     1 '        GW only lands to match the StateCU approach',/
     1 '      Revised the baseflow output file (*.xbi) to correct',/
     1 '        the pumping value.  Note this is a correction to ',/
     1 '        the report only, not the calculations',/
     1 '      Added River Reach file (*.rir) for reach processing',/
     1 '      Revised Outrch; GetRch & Report to use the River',/
     1 '        Reach (*.rir) file.',/
     1 '      Revised Oprinp, virinp & datinp to allow the control',/
     1 '        variable isprink to equal 2 for a mutual supply',/
     1 '        approach to sprinklers',/
     1 '    - 2009/03/06 (12.29.16)',/
     1 '      Revised GetPln to require the user specify the type',/
     1 '        of recharge data specififed in a plan file (*.pln)',/
     1 '        to be Reservoir or Diversion',/
     1 '      Revised GetPln to check the type of return flow data',/
     1 '        provided for a recharge plan.',/
     1 '      Added GetPlnR to read plan to reservoir data (*.plr)',/
     1 '        to allow one plan to be tied to many recharge sites',/
     1 '    - 2009/02/02 (12.29.15)',/
     1 '      Revised type 45 rule (DivCarL) to allow a diversion',/
     1 '        to be limited by a miscellaneous diversion or ',/
     1 '        reservoir demand data',/
     1 '      Revised Bomsec to correct the Type 47 annual limit',/
     1 '        initilization',/
     1 '      Revised Oprinp to correct an initilization problem',/
     1 '        associated with a bad array size',/
     1 '      Revised the following to correct a problem with the',/
     1 '        type 45 rule (DivCarL) that caused return flows',/
     1 '        from carrier losses to be calculated twice:',/
     1 '      a) Revised DivcarL to call RtnCarry and DsaMod',/
     1 '      b) Separated SetQdivC into RtnCarry & SetQdivX in order',/
     1 '         to separate return flow calculations from diversion',/
     1 '         variable Qdiv',/
     1 '      c) Added SetQdivX to not calculate return flows from',/
     1 '         carrier losses',/
     1 '      d) Revised DsaMod to include carrier loss and special',/
     1 '         treatment when called by a type 45 (DivCarL) rule',/
     1 '    - 2009/01/15 (12.29.14)',/
     1 '      Revised OprinP, OprFind & Bomsec to allow the Type 47',/
     1 '        (Accounting Plan Limit) rule source 2 variable',/
     1 '        (iopsou(2,1) to be the month limits are reset',/
     1 '      Revised GetPln to allow a Release Limit Plan (type 12)',/
     1 '        to be initialized using the initial plan storage ',/
     1 '        varaiable in the plan data file (*.pln)',/
     1 '      Revised OutPlnMo for a Release Limit Plan (type 12)',/
     1 '        output to to print the maximum release as the annual',/
     1 '        value',/
     1 '      Revised the Type 27 (Plan to Use Direct) and the',/
     1 '        Type 28 (Plan to Use by Exchange) to be limited',/
     1 '        by the amount diverted by another operating rule',/
     1 '      Added OutRch to set default stream reaches using',/
     1 '        gage locations when the check mode is operated',/
     1 '      Added GetRch to read stream reach data when the',/
     1 '        response file contains reach data',/
     1 '      Added OutRchR to print a water balance by stream',/
     1 '        reach',/
     1 '      Enhanced OutDivC & OutWelC to process diversion',/
     1 '        and well comparisons by stream reaches.',/
     1 '      Revised Water Balance treatment of Plan Supplies',/
     1 '    - 2008/12/18 (12.29.13)',/
     1 '      Revised RivRtn to limit the maximum diversion',/
     1 '         adjustment to per iteration',/   
     1 '      Revised Rtnmax to inlcude the calling routine,',/
     1 '        calling right and a roundoff adjustment',/
     1 '    - 2008/12/15 (12.29.12)',/
     1 '      Revised type 5 (resrpl) to correct a problem with',/
     1 '        identifying when a water right is used to limit',/
     1 '        an exchange',/
     1 '    - 2008/12/11 (12.29.11)',/
     1 '      Enhanced Oprinp to handle more than one carrier when',/
     1 '        an operating rule is turned off',/
     1 '    - 2008/12/10 (12.29.10)',/
     1 '      Enhanced type 27 (DivResp2) to include an exchange',/
     1 '        (release) potential when making a release for a',/
     1 '        depletion demand',/
     1 '    - 2008/12/04 (12.29.09)',/
     1 '      Enhanced RtnMax mininmum flow calculation',/
     1 '    - 2008/12/01 (12.29.08)',/
     1 '      Revised Oprinp reuse plan check for a type 32 rule',/
     1 '      Revised type 45 carrier with loss calculation of',/
     1 '        destination capacity with loss',/
     1 '      Revised ChkVer approach to determine',/
     1 '        the Irrigation Practice File (*.ipy) version',/
     1 '    - 2008/11/24 (12.29.07)',/
     1 '      Revised SetQdivC addsjutment to diversions in a prior',/
     1 '        iteration (divmon) when a Carrier is the Source or',/
     1 '        the Carrier is the Destination',/
     1 '      Revised RivRtn exit when the last diversion from the ',/
     1 '        river is the destination (not a carrier)',/
     1 '    - 2008/11/12 (12.29.06)',/
     1 '      Enhanced GetWel & GetWel2 to check the locations',/
     1 '        of a diversion with a supplemental well are both',/
     1 '        located at the same river node',/
     1 '      Revised report list, type 12 = Stream Comparison',/
     1 '      Revised miscellaneous files related to initilizing ',/
     1 '        variables that were identified by compiling with',/
     1 '        detailed checks turned on',/
     1 '      Added Diversion to Recharge (*.dre) and Reservoir',/
     1 '        to Recharge (*.rre) to the Natural Flow calculations',/
     1 '    - 2008/10/23 (12.29.05)',/
     1 '      Enhanced RivRtn approach to adjust for immediate ',/
     1 '        return flows and reoperate as appropriate',/
     1 '    - 2008/10/21 (12.29.04)',/
     1 '      Revised Execut & Report to open selected files only',/
     1 '        when they are simulated (e.g. wells, plans, ...)',/
     1 '      Revised RivRtn to adjust for immediate return flows',/
     1 '        and reoperate as appropriate',/
     1 '    - 2008/10/15 (12.29.03)',/
     1 '      Revised RivRtn regarding the treatment of',/
     1 '        immediate return flows',/
     1 '    - 2008/10/08 (12.29.02)',/
     1 '       Revised RivRtn to skip an available flow check',/
     1 '         at the destination from a system of carriers',/
     1 '         and river returns when the last river diversion',/
     1 '         is a carrier, not the destination',/
     1 '       Revised ChkVer to correct a problem when the ',/
     1 '         operating rule file (*.opr) has no version ',/
     1 '         specified and the first record has bad data',/
     1 '       Revised WelRig3P to correct a problem reporting',/
     1 '         pumping in-priority during the current time step',/
     1 '       Revised Common, GetPlnW & Outpln to include the ',/
     1 '         decree weighted administration number for well',/
     1 '         pumping under an augmentation plan',/
     1 '    - 2008/09/23 (12.29.01)',/
     1 '       Revised GetPln to set reservoir to plan (iresP)',/
     1 '         and plan to reservoir (iplnR).',/
     1 '       Revised Sepsec to calculate seepage for one or more',/
     1 '         reservoirs and store prior iteration seepage in ',/
     1 '         variable seep1.',/
     1 '       Revised Common to include iresP, iplnR & seep1.',/
     1 '       Revised Plan to Plan direct (type 48) and Plan to ',/
     1 '         Plan Exchange (type 49) to calculate reservoir',/
     1 '         seepage when the source is a seepage plan.',/
     1 '       Revised Common, DivCar (type 11) & DivCarL (type 45)',/
     1 '         to include variable iopSouR, the source type.',/
     1 '       Revised OutWelP header regarding which plan serves',/
     1 '         a given well structure.',/
     1 '       Revised Riginp & Common to save the on/off switch.',/ 
     1 '       Revised OprFind to use above in order to allow',/
     1 '         more than one operating rule to refrence a',/
     1 '         water right.',/
     1 '    - 2008/09/15 (12.29.00)',/
     1 '        Revised DirectEx (Type 24) & DirectBy (Type 25),',/
     1 '          to correct a problem when the demand and source',/
     1 '          structure are the same',/
     1 '        Revised DivResR (Type 32), DivRplP (Type 33),',/
     1 '          PowSeaP (Type 29) & DaySet to correct a problem',/
     1 '          the volume of water in a reservoir reuse plan',/
     1 '        Revised DivAlt (Type 39) to correct a problem',/
     1 '          when the alternate point is a well structure',/
     1 '        Corrected the water balance report by revising',/
     1 '          DivResp2 (Type 27) & DivRplP (Type 28) to',/
     1 '          include carrier loss in the amount diverted',/
     1 '        Revised Oprinp to report the number of operating',/
     1 '          rules read by type',/
     1 '        Revised GetPln to report the number of plans by type',/
     1 '        Revised the version numbering to include two',/
     1 '          decimals after the main version. For example',/
     1 '          version 12.29.00 is version 12.29 with no ',/
     1 '          corrections',/
     1 '    - 2008/09/12 (12.28.09)',/
     1 '        Revised Type 24 (DirectEx) to correct a problem',/
     1 '          when the destination is a reservoir',/
     1 '        Revised the RivRtn approach to adjusting a diversion',/
     1 '          when a shortage occurrs',/
     1 '        Enhanced the file version operation by revising',/
     1 '          Oprinp & ChkVer to work when no operating rule',/
     1 '          is provided.',/
     1 '        Revised Parse to handle bad command line data better',/
     1 '        Revised OprFind to allow structure searches when a',/
     1 '          structur typs is not in the data set (e.g. search',/
     1 '          for a reservoir when no reservoir data is provided',/
     1 '    - 2008/09/08 (12.28.08)',/
     1 '        Revised RivRtn to correct a problem that occurrs',/
     1 '          when carrier and carier returns do not make flow',/
     1 '          available to a diversion',/
     1 '    - 2008/09/05 (12.28.07)',/
     1 '        Revised SetQdiv to handle a call from a Type 49',/
     1 '          Plan or Res reust to a T&C or Aug Plan',/
     1 '        Revised Oprinp to read data for Type 39 Alternate',/
     1 '          Point correctly',/
     1 '    - 2008/09/03 (12.28.06)',/
     1 '        Revised OutPlnMo to correct a Plan reporting problem',/
     1 '        Revised DirectBy (type 25) to correct an initial-',/
     1 '          ization problem',/
     1 '    - 2008/08/19 (12.28.05)',/
     1 '        Revised DivRplP (Type 28) to include canal loss',/
     1 '          in the demand calculations',/
     1 '    - 2008/08/15 (12.28.04)',/
     1 '        Revised DivResp2 (Type 27) & DivRplP (Type 28) to',/
     1 '          correct a problem with calculating the plan supply',/
     1 '          introduced with ownership capability in ver 12.28',/
     1 '        Revised DivRplP (Type 28) to correct a problem with',/
     1 '          calculating the supply available in the exchange',/
     1 '          reach',/ 
     1 '    - 2008/08/06 (12.28.03)',/
     1 '        Added a data check for Type 45 (Carrier with Loss)',/
     1 '          when a carrier is not supplied',/
     1 '        Added variable initilization to remove water right',/
     1 '          sharing capability begun in version 12.28.0',/
     1 '    - 2008/07/15 (12.28.02)',/
     1 '        Corrected a data check in DivCarL',/
     1 '    - 2008/07/08 (12.28.01)',/
     1 '        Corrected a problem in RivRtn',/     
     1 '        Revised OprInp to read carrier with loss when an',/
     1 '          operating right is off and oprloss=-1',/
     1 '        Revised OprInp to indicate a problem when the ',/
     1 '          ownership % is zero',/
     1 '    - 2008/07/03 (12.28.0)',/
     1 '        Revised OprFind, SetLoss & SetQdivC to allow carrier',/
     1 '          with loss data type to be Carrier or Return',/
     1 '        Revised type 45 Carrier with Loss by including',/
     1 '          SetLoss and Rivrtn to allow water to be ',/
     1 '          diverted to a carrier with loss, returned to',/
     1 '          the river, then rediverted to the destination',/
     1 '          or another carrier with loss',/
     1 '        Revised call to Setloss and Rivrtn by DirrectBy',/
     1 '          DirectEx, DivcarL, DivresP2, DivresR, DivRplP',/
     1 '          DivRplP2, & DivRplR',/
     1 '        Revised RtnMax to operate when maximum efficiency',/
     1 '          is turned off and remove a temporaty fix related to',/
     1 '          calculating the available flow',/
     1 '        Revised DivCarL to allow carrier with loss data',/
     1 '          by adding Call DsaMod and Call RivRtn',/
     1 '        Revised RtnMax to recognize 4 land types (SW Flood,',/
     1 '          GW Flood, SW Sprinkler and GW Sprinkler',/
     1 '        Revised RtnSec and RtnSecX to not allow any upstream',/
     1 '          returns to be available to the diversion itself',/
     1 '        Revised Oprinp to correct a problem reading carrier',/
     1 '          data without loss when the opr right is turned off',/
     1 '          Also allow the carrier to equal the source to',/
     1 '          allow losses to be calculated',/
     1 '        Revised ResrgP (Type 41) to correct a problem with',/
     1 '           identifying the destination account',/
     1 '        Implemented % ownership to type 27 (Release from Plan',/
     1 '           Direct) & type 28 (Release from Plan by Exchange)',/
     1 '    - 2008/06/04 (12.27)',/
     1 '        Revised Oprinp to correct a problem when an',/
     1 '          operating right with 12 monthly switches is',/
     1 '          turned off.',/ 
     1 '        Revised OutDivW to correct a round off issue that',/
     1 '          caused the Control Location (calling location) to',/
     1 '          be reported Capacity or Water Right Limited',/
     1 '          incorrectly (e.g. when the shortage is zero).',/
     1 '    - 2008/06/03 (12.26)',/
     1 '        Revised DivcarL (type 45), divresR (type 32) and ',/
     1 '          DivRplR (type 33) to correct a problem when calling',/
     1 '          Setloss to calculate carrier losses',/
     1 '    - 2008/05/07 (12.25)',/
     1 '        Revised Execut to make water available from reservoir',/
     1 '          seepage is available for diversion.',/
     1 '        Revised the Reoperation Rule (type 12) to insure it',/
     1 '          operates at least once per time step',/
     1 '    - 2008/04/23 (12.241)',/
     1 '        Revised DivResP2 and DivRplP to constrain releases',/
     1 '          to a T&C and Aug Plan to their demand',/
     1 '    - 2008/04/23 (12.24)',/
     1 '        Revised DirectEx to correct a problem with ',/
     1 '          calculating negative flows',/
     1 '        Revised Oprinp to correct a problem when the right',/
     1 '          operarional right is turned off',/
     1 '    - 2008/04/07 (12.23)',/
     1 '        Revised Report and added OutWelP to generate a',/
     1 '          report (*.xwp) that prints all augmentation plans',/
     1 '          serving a well structure',/
     1 '        Revised PowResP (type 48) to correct a problem',/
     1 '          when the destination is an admin plan (type 10)',/
     1 '    - 2008/03/26 (12.22)',/
     1 '        Revised the number of uses tied to a plan',/
     1 '          from 20 to 100 in StateM and Common',/
     1 '          Note uses 21-100 are still combined in ',/
     1 '          plan reporting (OutPln and OutPlnMo)',/
     1 '        Revised Oprinp and RsrSpuP (type 34) to allow',/
     1 '          transfers to be limited to the data specified',/
     1 '          in a type 47 (Plan limit) operating rule',/
     1 '        Revised Oprinp, DivresP2 (type 27) and RsrspuP',/
     1 '          (type 28) to allow an instream flow destination',/
     1 '        Revised WelRig3P approach to calculate demand to',/
     1 '          be similar to WelRig3',/
     1 '        Added DsaMod a generic Modified Direct Solution',/
     1 '          Algorythm and revised Divrig, DirectEx (type 24)',/
     1 '          and DirectBy (type 25) to call DsaMod',/
     1 '        TEMPORARILY Revised DirectBy diversion logic',/
     1 '    - 2008/03/13 (12.21)',/
     1 '        Fixed a problem with call Setlimit in operating ',/
     1 '          rule types 27 and 28 (DivResP2 and DivRplP',/
     1 '    - 2008/03/13 (12.20)',/
     1 '        Revised convergence check in replace',/
     1 '        Revised GetVer to set the default file type to unknown',/
     1 '        Revised Oprinp to be backward compatible when reading',/
     1 '          a type 10, replacement reservoir, rule by setting',/
     1 '          the default for variable ioprlim(k)=0',/
     1 '    - 2008/02/21 (12.19)',/
     1 '        Revised Oprinp to allow a type 27 and 28 operating',/
     1 '         rules to allow a structure to be be turned off',/
     1 '    - 2008/02/21 (12.18)',/
     1 '        Revised the maximum reoperation from 100 to 1000',/
     1 '    - 2008/02/14 (12.17)',/
     1 '        Revised DirectEx for a typo related to calculating',/
     1 '          the water right available for diversion',/
     1 '    - 2008/02/07 (12.16)',/
     1 '        Revised Outmon to correct a problem printing',/
     1 '          return to river under certain conditions',/
     1 '        Broke RivRtn out of DirResp2 to to simulate',/
     1 '          an Augmentation Structure by allowing a carrier',/
     1 '          to return water to the river',/
     1 '        Added RivRtn to DirectEx (24), DirectBy (25) and',/
     1 '          DivresP2 (27) and DivrplP (28)',/
     1 '        Revised all setqdiv to set source and destination',/
     1 '          data only',/
     1 '        Revised setqdivC to set carrier data only',/
     1 '        Revised SetQdiv and SetQdivC diversion & carrier',/
     1 '          location data for consistency',/
     1 '        Revised Execut, Rtnsec, Rtnsecw & Deplete to refine',/
     1 ' 	    small, non-downstream return flows and depletions',/
     1 '          for daily time step performance',/
     1 '    - 2008/01/10 (12.15)',/
     1 '        Revised DivResP2 (type 27) and Oprinp to simulate',/
     1 '          an Augmentation Structure by allowing a carrier',/
     1 '          to return water to the river',/
     1 '        To assist with water balance reporting:',/
     1 '          1.Defined qdiv(28,is) to be a Reuse or Admin ',/
     1 '            Plan Source at the Source location',/     
     1 '          2.Defined qdiv(35,is) to be a Reuse or Admin ',/
     1 '            Plan Source at the Destinaton location.',/
     1 '          3.Corrected a problem with the type 25 (bypass)',/
     1 '            plan reporting when the destination is a plan',/
     1 '          4.Revised OutBal2 and rule types 24, 25, 27, ',/
     1 '            28, 29, 46, 48, & 49, 46 accordingly',/
     1 '        Revised Virset to initialize daily soil moisture data',/
     1 '        Revised Oprinp to correct type 48 and 49 checks',/
     1 '          for correct destination and source data, AGAIN',/
     1 '    - 2008/01/03 (12.14)',/
     1 '        Revised DivresR (type 32) to correct a problem',/
     1 '          when the destination is a reservoir',/
     1 '        Revised Oprinp to correct type 48 and 49 checks',/
     1 '          for correct destination and source data',/
     1 '        Revised DirectEX (type 24) and DirectBY (25)',/
     1 '          to fix a problem when the transfer is limited',/
     1 '        Revised StateM, Outmon, Outbal2, OutdivW and',/
     1 '          OutdivC to recognize a new output variable',/
     1 '          that tracks water from a plan that was required',/
     1 '          for selected water balance calculations',/
     1 '        Revised SetQdiv and SetQdivC to include water',/
     1 '          rerturned to the river',/
     1 '    - 2008/01/02 (12.13)',/
     1 '        Revised Oprinp and OprFind to correct a problem',/
     1 '          reading and reporting a Type 46 Multiple Ownership',/
     1 '          rule.',/
     1 '        Removed Operating Rule to Plan ties from Oprinp',/
     1 '        Added SetPlanO to tie Operating Rules to Plans',/
     1 '    - 2007/12/27 (12.12)',/
     1 '        For examples ex115C112 and ex115C113',/
     1 '         1. Revised Oprinp to correct a check between a ',/
     1 '            a destination type and a reuse type (',/
     1 '        For examples ex115C132',/
     1 '         1. Revised Planeva to calculate plan evap when',/
     1 '            the reservoir is empty ',/
     1 '         2. Revised Oprinp to identify evaporation as a use',/
     1 '            when tied to a destination (ireuse)',/
     1 '        For examples ex115C14',/
     1 '         1. Removed type 26 (renamed to type 48)',/
     1 '         2. Added type 48 (PowResP) Res or Plan to Plan',/
     1 '            Direct',/
     1 '         3. Added type 49 (DivRplP2) Res or Plan to Plan',/
     1 '            Exchange',/
     1 '         4. Revised Oprinp to exclude type 26 and inlcude ',/
     1 '            type 48 and 49',/
     1 '         5. Revised GetPln to associate a plan with a ',/
     1 '            return ID in *.prf',/
     1 '         6. Revised GetSta to recognize a plan',/
     1 '    - 2007/12/06 (12.111)',/
     1 '        Revised DirectBy (type 25) to initialize the CU factor',/
     1 '        Revised Oprinp to correct a problem reading reuse',/
     1 '          data for a type 24 (Water Right Exchange) rule'/
     1 '        Added ChkPrf to check plan return flow indicator',/
     1 '          (iopsou(4,k) with the plan return file (*.prf)',/
     1 '        Revised Oprinp to call the above for operating ',/
     1 '          rule types 24, 25, 27 and 28',/
     1 '        Revised DivResP2 (type 27) & DivRplP (type 28) to',/
     1 '          include Carrier Loss',/
     1 '    - 2007/12/03 (12.10)',/
     1 '        Revised DirectEx (type 24), DirectBy (type 25)',/
     1 '          and Oprinp to not allow a transfer to use the ',/
     1 '          source structure as a carrier',/
     1 '        Revised Oprinp to allow DirectEx (type 24) & ',/
     1 '          DirectBy (type 25), DivResP2 (type 27) & ',/
     1 '          DivRplP (type 28) to include T&C CU Factors',/
     1 '        Revised Oprinp to allow DivResP2 (type 27) & ',/
     1 '          DivRplP (type 28) to allow carriers with loss',/
     1 '        Added SetTC to perform T&C conditions',/ 
     1 '    - 2007/11/27 (12.09)',/
     1 '        Revised Welrig3 to handle M&I demands (area=0 and/or',/
     1 '          efficiency=0',/
     1 '        Removed all refrences to variable icuapp in Datinp,',/
     1 '          Getipy2, Getipy4, Mdainp, Rtnsec, Rtnsecw, and',/
     1 '          Vircom. The code now initializes and uses the same',/
     1 '          variables when either a 2 or a 4 supply-irrigation',/
     1 '          data is provided in the irrigation practice file',/
     1 '          (*.ipy)',/
     1 '    - 2007/11/19 (12.08)',/
     1 '        Corrected an Index Flow (type 13) check in Oprinp',/
     1 '        Relaxed a Carrier (type 11) check in Oprinp to allow',/
     1 '          a source and destination to be the same if there',/
     1 '          is a carrier',/
     1 '    - 2007/11/13 (12.07)',/
     1 '        Revised OutDivC to not report Carrier type 1',/
     1 '          (Carrier_1) as part of the comparison',/
     1 '        Revised DivCar (type 11) reporting when a diversion',/
     1 '          right diverts to a diversion',/
     1 '        Revised GetIpy4 to allow Sprinkler Acres > GW acres ',/
     1 '          (e.g. Sprinkler Acres may be fed by Surface Water',/
     1 '        Revised DivRplP (type 28) to treat the exchange reach',/
     1 '          differently when the diversion is a Depletion',/
     1 '          Note this refines a version 12.04 edit',/    
     1 '        Revised DivResP2 (type 27) & DivRplP (type 28) to',/
     1 '          calculate a FIXED T&C obligation based on the',/
     1 '          amount diverted, not returned',/     
     1 '        Revised DivResP2 (type 27) & DivRplP (type 28), ',/
     1 '          and Oprinp to allow a MIXED  T&C oblibation ',/
     1 '          based on the amount diverted',/
     1 '        Revised Oprinp to allow the operating right file',/
     1 '          to read Format=xx to confirm the format of the file',/
     1 '    - 2007/11/04 (12.06)',/
     1 '        Revised DirectEx (type 24) and DirectBy (type 25)',/
     1 '          to correct a problem when the carrier is located',/
     1 '          at the source',/
     1 '        Revised DivMulti (type 46) to allow ownership %',/
     1 '          to be specified as a real',/
     1 '        Revised OutBal2 to include admin plans (type 11)',/
     1 '          as a supply. ',/
     1 '          Note this puts back a 12.04 edit',/     
     1 '        Revised Oprinp to allow a type 43 (In Priority',/ 
     1 '          Supply) to supply a T&C plan in addition to ',/
     1 '          an Augmentation Plan',/
     1 '        Fixed an array size problem in GetPlnW (Wells ',/
     1 '          tied to Plans)',/ 
     1 '        Revised Oprinp to allow carrier with loss data to',/
     1 '          use a free format read and (than 4 digets)',/
     1 '        Revised Oprinp type 43 from In Priority Depletion to',/
     1 '          In Priority Obligation. Note it can now be used',/
     1 '          to satisfy a T&C obligation',/
     1 '    - 2007/10/30 (12.05)',/
     1 '        Revised Oprinp for type 27 and 28 plan data checks',/
     1 '    - 2007/10/30 (12.04)',/
     1 '        Revised DivRplP (type 28) to correct a problem with',/
     1 '          Available flow when the diversion type is Depletion',/
     1 '        Revised OutBal2 to not include admin plans (type 11)',/
     1 '          plans as a supply',/
     1 '    - 2007/10/29 (12.03)',/
     1 '        Revised Replacement Rule (type 10) to allow monthly',/
     1 '          and annual release limits',/
     1 '    - 2007/10/23 (12.02)',/
     1 '  **    Note adding Release Limits is not yet complete',/
     1 '        Revised Oprinp to include Type 47 Release Limit Rule',/ 
     1 '        Revised Execut to include a Release Limit Rule',/ 
     1 '        Revised DivResP2 (type 27) to allow monthly and',/
     1 '          annual release limits',/
     1 '        Revised DivRplP (type 28) to allow monthly and',/
     1 '          annual release limits',/
     1 '        Revised DivRplP (type 28) to allow a reservoir source',/
     1 '        Revised Oprinp to check a Multi Ownership (type 46)',/
     1 '          destination is located downstream of the source',/
     1 '        Revised GetPln to check the return flow data provided',/
     1 '        Revised Outpln to correct a reporting issue with ',/
     1 '          evaporation from a plan',/     
     1 '    - 2007/09/05 (12.01)',/
     1 '  **    Version 12.xx and greater allows the irrigaiton',/
     1 '          practice file (*.ipy) to contain 2 land use types',/
     1 '          (Total GW and Total Sprinkler) or 4 land use types',/
     
     1 '          (SwFlood, SwSprinkler, GwFlood and Gw Sprinker)',/     
     1 '        Note checking for the above is not yet complete',/
     1 '        Revised the structure summary output',/
     1 '        Added GetIpy4 to allow the irrigation practice',/
     1 '          file (*.ipy) to contain 4 land use types',/
     1 '        Added GetIpy2 to allow the old 2 land use type',/
     1 '          (GW and Sprinkler) ipy file to be read',/
     1 '        Revised Mdainp and Getfn to recognize identify'/
     1 '          the correct *.ipy file type',/
     1 '        Revised the CU calculations to use appropriate data',/
     1 '          for the ipy file provided',/
     1 '        Added RtnsecC the capability to calculate a FIXED',/
     1 '          return flow pattern',/
     1 '        Revised Divresp2(type 27) and DivrplP(type 28) to',/
     1 '          use a fixed return pattern',/
     1 '        Revised Oprinp to allow a type 46 rule to have up',/
     1 '          to 5 destination plans',/
     1 '        Revised Oprinp to check all type 11 plans have',/
     1 '          Spill Operating Rule (type 29)',/
     1 '        Revised Getipy2 and GetIpy4 to convert well yield',/
     1 '          read from *.ipy in af per month from 30.0 to 30.4',/
     1 '          days per month',/
     1 '        Revised maximum number of wells from 4000 to 11000',/
     1 '        Revised maximum number of rights from 6285 to 14000')
     
c
c 
 211    format(     
     1 '    - 2007/08/28 (11.63)',/
     1 '        Added type 46, DivMulti, for Multiple Ownership',/
     1 '        Revised oprinp to read type 49, Multiple Ownership',/
     1 '        and to read efficiency for types 27 and 28',/
     1 '        Revised DivresP2 (type 27) and divRplP (type 28)',/
     1 '          to allow a T&C requirement to be calculated when',/
     1 '          water is released from a plan',/
     1 '    - 2007/07/26 (11.622)',/ 
     1 '        Revised Oprinp regarding the check to identify',/
     1 '          a T&C plan tied to both a source and demand',/
     1 '        Revised DirectEx (type 24) to operate correctly',/
     1 '          when the exchange demand is zero',/
     1 '        Revised DirectBy (type 25) to operate correctly',/
     1 '          when the bypass demand is zero',/
     1 '    - 2007/07/25 (11.621)',/ 
     1 '        Revised DirectBy (type 25) Direct Flow Bypass',/
     1 '          to correct a reporting problem in *.xop',/
     1 '    - 2007/07/09 (11.62)',/ 
     1 '        Revised DivResP2 (type 27) and DivrplP (type 28)',/
     1 '          to allow the T&C requirements associated with a',/
     1 '          transfer to be assignged at the destination',/
     1 '        Revised DirectEx (type 24) and DirectBy (type 25)',/
     1 '          to allow unused ownership to be used by the source',/
     1 '        Revised DivresP2 (type 27), DirrplP (type 28), ',/
     1 '          PowSeaP (type 29), and Oprinp to allow monthly',
     1 '          and annual limits to be adjusted based on the',/
     1 '          amount released',/
     1 '    - 2007/06/14 (11.612)',/ 
     1 '        Corrected an error in DivResP2 (type 27)',/
     1 '          associated with operating a bypass at',/
     1 '          the source location',/
     1 '        Corrected an error in Oprinp (type 24)',/
     1 '          associated with checking the location of the',/
     1 '          source and exchange point',/
     1 '    - 2007/06/14 (11.611)',/ 
     1 '        Corrected an error in DirectBy (type 24)',/
     1 '          related to call SetCarl',/
     1 '        Revised SetCArL to include loss when limiting',/
     1 '          a diverstion to a carrier capacity',/
     1 '    - 2007/06/13 (11.61)',/ 
     1 '        Corrected an error in DirectBy (type 24)',/
     1 '          related to the source demand',/
     1 '        Revised Oprinp to include additional input checks',/
     1 '          for a type 24 (direct exchange) rule',
     1 '        Revised DivresR (type 32) & DivRplR (type 33)',/
     1 '          to allow transit and carrier losses',/
     1 '        Revised DivcarL (type 45) to use generic carrier',/
     1 '          loss routines used by types 24, 25, 32 & 33.',/
     1 '    - 2007/06/05 (11.60)',/ 
     1 '        Revised DivresR, type 32 to allow a direct ',/
     1 '          release from a reservoir to a carrier',
     1 '        Revised DirectEx and DirectBy, type 24 & 25',
     1 '          to allow transit and carrier losses',/
     1 '        Revised DirectEx and DirectBy, type 24 & 25',
     1 '          reporting when water is diverted to a carrier',/
     1 '        Revised DirectBy to allow the source structure',/
     1 '          to operate as a carrier',/
     1 '        Revised DirectBy to limit capacity at the source',/
     1 '          structure to the ownership %',/
     1 '    - 2007/05/07 (11.58)',/ 
     1 '        Revised DivCar.f and DivcarL.f to correct a problem',/
     1 '          when icase = 4 (divert with return flows)',/
     1 '    - 2007/04/16 (11.57)',/ 
     1 '        Revised DivCarl.f and Oprinp.f to allow a type 45',/
     1 '          rule to allow multiple owners to share in water',/
     1 '          supplies and carrier capacity porportionally',/
     1 '    - 2007/04/10 (11.561)',/ 
     1 '        Revised Oprinp.f, type 39, input to allow the',/
     1 '          destination and alternate point to be different',/
     1 '        Revised DivAlt.f to recognize above and report',/
     1 '          accordingly',/
     1 '    - 2007/04/10 (11.56)',/ 
     1 '        Revised DivAlt.f, type 39, to correct a problem',/
     1 '          related to the number of downstream nodes',/
     1 '    - 2007/04/10 (11.553)',/ 
     1 '        Revised DivcarL.f, type 45, to calculate remaining',/
     1 '          decree correctly',/
     1 '    - 2007/04/09 (11.552)',/ 
     1 '        Revised DivcarL.f, type 45, to calculate carrier',/
     1 '          limits correctly',/
     1 '    - 2007/03=4/02 (11.551)',/ 
     1 '        Revised DivcarL.f, type 45, as follows:',/
     1 '          1. Consumptive use now reflects carrier losses',/
     1 '          2. A Diversion demand is after carrier losses',/
     1 '          3. Capacity limits are adjusted for carrier losses',/
     1 '    - 2007/03/28 (11.55)',/ 
     1 '        Added DivcarL.f, type 45, Carrier with loss',/
     1 '        Revised Oprinp.f to read type 45 that allows',/
     1 '          losses for an intermediate carrier',/
     1 '        Revised Oprfind.f to read carrier loss for intermediate',/
     1 '          carriers',/
     1 '        Revised OutBal2 to differentiate between carrier loss',/
     1 '          that may return to the system and system losses',/
     1 '          that do not return to the system',/
     1 '    - 2007/03/22 (11.54)',/ 
     1 '        Revised DirectFS.f, type 16, to correct a problem',/
     1 '          when serving more than 1 reservoir account',/
     1 '        Revised Oprinp.f for a type 11, carrier, rule',/
     1 '           to print a warning and stop if the Admin location',/
     1 '           and Destination location are the same',/     
     1 '        Cleaned up DirectEx.f, type 24, to remove refrences',/
     1 '          to an exchange point based on 11.53 updates',/
     1 '    - 2007/03/19 (11.53)',/ 
     1 '        Revised DirectFS, type 16, to limit the number of',/
     1 '          users associated with the source right to 1',/
     1 ' **     Revised DirectEx, type 24, to limit the diversion',/
     1 '          to available flow below the diversion',/
     1 '        Revised DirectEx and DirectBY, types 24 & 25, to',/
     1 '          include return flow when there is no reuse plan',/
     1 '    - 2007/03/17 (11.52)',/ 
     1 '        Revised Outbal2.f to correct the reported Loss',/
     1 '        Revised RgRg, type 17 and 18, to recognize variable',/
     1 '          mon may equal 13 when called to print annual data',/
     1 '  **    Revised DirectFs type 16 demand to reflect bypass',/
     1 '          requirement and detailed output reporting',/
     1 '        Revised DirectFs to serve multiple reservoir',/
     1 '          accounts',/
     1 '    - 2007/03/02 (11.512)',/ 
     1 '  **    Revised Oprinp.f type 13 to require a water',/
     1 '          right as source 2 and require it be controlled',/
     1 '          by the operating right (iopsou(4,k) = 1',/
     1 '    - 2007/02/28 (11.511)',/ 
     1 '        Revised Oprinp.f to relax the acount check',/
     1 '          when the source is a water right for type 16',/
     1 '         (Direct Flow Storage), type 13 (La Plata), and',/
     1 '          types 17 & 18 (RG compact)',/
     1 '    - 2007/02/28 (11.51)',/ 
     1 '        Revised Oprinp.f to correct the location check ',/
     1 '          for a type 13 (La Plata Compact) operating rule',/
     1 '  **    Revised Oprinp.f to allow the type 16 (Direct Flow',/
     1 '          Storage) operating rule data control if the source',/
     1 '          water right is left on or turned off.',/
     1 '        Revised WelRig2.f, WelrigP.f and WelRech.f to',/
     1 '          include call data',/
     1 '        Revised Outmon.f & outwr2 to include well only data',/
     1 '          and for column 33 to be salvage (rlossx2) not',/
     1 '        Revised Outmon.f & outwr2 to include well only data',/
     1 '          and for column 33 to be salvage (rlossx2) not',/
     1 '          loss (rlossx) lands. Revised OutBal2 to recognize',/
     1 '          *.xdd (*.b43) now has well only lands. Therefore',/
     1 '          no longer need to read well output',/
     1 '  **    Corrected Outmon.f to fix an error in reporting ',/
     1 '          CU shortage to the well (*.xwe) report',/ 
     1 '        Revised Outmon.f, Outwel.f, Outbal2.f, DayoutR.f',
     1 '          Daywelo.f, and common.inc to include well carrier',/
     1 '        Added WelRech.f (type 44) Recharge well to reservoir',/
     1 '        Revised WelrigP.f and WelAugP.f to set the well ID',/
     1 '          to nWE not nD for clarity',/
     1 '  **    Revised Vircom to correct pumping output to *.xbi',/
     1 '          Note this correction does not impact Naturalized',/
     1 '          results, only the pumping value reported',/
     1 '        Added OutWr2 to simplify water right output to',/
     1 '          standard reports (*.xdd, *.xwe, *.xre, *.xdy, etc.',/
     1 '        Revised Xdebug.f to call OutRch.f and print a ',/
     1 '          Preliminary Reach File (*.xrh)',/
     1 '        Revised OutDivC.f, OutDivW.f and OutCU.f to use the',/
     1 '          Reach Data file (*.rch) and summarize output by',/
     1 '          Reach',/
     1 '    - 2007/01/04 (11.50)',/ 
     1 '  **     Revised Divres.f (type 2&3), Divrpl.f (type4)',/
     1 '          Divcar.f (type 11), to allow variable efficiency',/
     1 '          Note other routines (carrpl, divcar1, divcarr, ...)',/
     1 '          already allowed variable efficiency',/
     1 '        Revised Rsrspup (type 34) to correct a problem with',/
     1 '          the water balance associated with a reservoir to',/
     1 '          reservoir transfer',/
     1 '    - 2007/01/02 (11.49)',/ 
     1 '        Revised DivCar.f (type 11) operating rule to allow',/
     1 '          canal loss to be routed to a type 8 recharge plan',/
     1 '        Revised Getpln.f and Oprinp.f to use variable',/
     1 '          psource() and PsourAcc in the plan file (*.pln)',/
     1 '          to indicate if the water source to a recharge',/
     1 '          plan (type 8) is a reservoir and its account.',/
     1 '          Note if a type 8 is not tied to a reservoir the',/
     1 '          source should be canal recharge and the plan',/
     1 '          return ID should be 1, not the default (999)',/
     1 '        Revised Mdainp.f to check the plan return ID exists',/
     1 '          in the unit response - delay file (*.urm)',/
     1 '        Revised Closs.f to include plan return ID data',/
     1 '        Revised water right output (*.xwr) to have titles',/
     1 '          on the bottom of the file',/
     1 '  **    Revised Mdainp.f to set IWR for well only lands',/
     1 '          to the value read in the Consumptive Water',/
     1 '          Requirement (*.ddc) file. This corrects a problem',/
     1 '          in reporting CU Shortage for well only lands',/
     1 '        Revised Outtbl.f to correct table headings for well',/
     1 '          return flow and depletion reporting',/
     1 '        Revised OutOpr to include the operating rules source',/
     1 '    - 2006/12/12 (11.48)',/ 
     1 '  **     Revised DivResP2, type 27, to include downstream',/
     1 '          streamflow limit (flomax)',/
     1 '        Revised Oprinp for type 4, Exchange. When the',/
     1 '          destination is a water right it no longer turns',/
     1 '          the destination right off',/
     1 '        Revised format for Well Plan data in GetPlnW.f to',/
     1 '          include structure served because some wells serve',/
     1 '          more than one structure',/
     1 '        Revised count to work with annual ppt and evap data',/
     1 '    - 2006/11/30 (11.47)',/ 
     1 '  **    Revised ResRg1.f to correct paper fill calculations',/
     1 '        Revised Oprinp.f to check Carrier (type 11) and ',/
     1 '          constrained Carrier (type 14) if source 1 and ',/
     1 '          the destination are at the same location and',/
     1 '          source 2 is blank (operate at the source location)',/
     1 '        Revised DivresP2.f to accept multiple accounts as a',/
     1 '          destination',/
     1 '    - 2006/11/27 (11.46)',/ 
     1 '  **    Revised Oprinp.f for a Bookover (type 6) to allow ',/
     1 '          iopsou(4,k) to equal 99 and to set diversion and ',/
     1 '          operational limits correctly',/
     1 '        Revised the input data used by a Carrier (Type 11) &',/
     1 '          Constrained Carrier (Type 14) operating rules.',/
     1 '          Specifically both allow the user to control when a',/
     1 '          source water right may be used by both a standard',/
     1 '          diversion and the carrier (iopsou(2,1)=0) or ',/
     1 '          by the carrier only (iopsou(2,1)=1). Also this',/
     1 '          change revised how data to provided to a ',/
     1 '          Constrained Demand (Type 14). ',/
     1 '       Revised Oprinp.f to warn the user about the new ',/
     1 '          Carrier (Type 11) & Constrained Carrier (type 14)',/
     1 '          input formats',/
     1 '    - 2006/11/22 (11.451)',/ 
     1 '        Revised Oprinp for Carrier (type 11) to control when',/
     1 '          variable iopsou(4,k) is checked as an account',/
     1 '    - 2006/11/21 (11.45)',/ 
     1 '        Revised Carrier (type 11) and Constrained Carrier',/
     1 '          (type 14) in Oprinp.f to let variable iopsou(4,k)',/
     1 '          control if the source right can be used by a direct',/
     1 '          flow and operating rule or just the operating rule',/
     1 '        Revised Constrained Carrier (type 14) Divcar1.f to',/
     1 '          be consistent with documentation regarding variable',/
     1 '          iopsou(3,k) and iopsou(4,k)',/
     1 '    - 2006/11/21 (11.44)',/ 
     1 '        Simplified the replacement reservoir output (*.xrp)',/
     1 '        Revised initilization of ishort in divres.f and',/
     1 '          divrpl.f to allow a replacement reservoir to',/
     1 '          operate correctly (when ishort is incorrect',/
     1 '          a replacement reservoir may not be called)',/
     1 '        Revised ResRg1.f paper fill operation when iressw=3',/
     1 '          to NOT STORE above target but reduce decree',/
     1 '          Note old logic allows storage above a target and',/
     1 '          required a spill',/
     1 '        Revised divResP2.f to initialize relact to zero',/
     1 '          (not -1) to correct amount printed to *.xop',/
     1 '        Revised Execut.f to clean up reoperation code',/
     1 '          (should not impact results)',/
     1 '        Revised Getchk.f to read integers as a free format',/
     1 '          real then reset to an integer',/
     1 '    - 2006/11/15 (11.43)',/ 
     1 '        Revised divres.f and divrpl.f to pass water right',/
     1 '          limits (dcrdivx and divdx) rather than common.inc',/
     1 '   **   Removed incorrect initilization of water right',/
     1 '          limit (dcrdivx) in divrpl.f when called by a ',/
     1 '          replacement reservoir',/
     1 '        Revised Oprinp.f to read Type 4 using oprfind.f ',/
     1 '          and set exchange point correctly',/
     1 '        Revised Oprinp.f to read all remaining operating ',/
     1 '          rules using oprfind.f including type 4, type 5,',/
     1 '          type 6, type 7, and type 14',/
     1 '    - 2006/11/13 (11.42)',/ 
     1 '   **   Revised Mdainp.f to store a target value from the',/
     1 '          previous year to correct October 1 daily',/
     1 '          end-of-month interpolation calculations',/
     1 '        Revised Rtnsec.f and Rtnmax.f to not use variable',/
     1 '          efficiency (ieff2=0) for a carrier',/
     1 '        Began to revise Datinp to accept new, simpler control',/
     1 '           file (*.ctl) format base on words ',/
     1 '           (e.g. Time Step = Daily Vs iday = 0)',/
     1 '        Revised Datinp to count number of climate stations to',/
     1 '           accomodate new, simpler control file',/
     1 '    - 2006/11/01 (11.41)',/ 
     1 '        Revised PutPath.f to allow file names to contain up ',/
     1 '          to 12 characters',/
     1 '        Revised Execut.f reoperation logic to identify all',/
     1 '          sources of new water',/
     1 '        Revised Rtnmax.f and RtnmaxE.f and all calling ',/
     1 '          routines to pass the variable efficiency control ',/
     1 '          (ieff2)',/
     1 '        Revised RtnMax.f to turn off variable efficiency',/
     1 '          (ieff2=0) when a structure is a carrier',/
     1 '        Revised DivCar to turn off variable efficiency',/
     1 '          (ieff2=0) for this operating rule',/
     1 '        Revised Divres.f and Carrpl.f to include reservoir',/
     1 '          to reservoir reporting (qres(29,ix) for use in ',/
     1 '          the water balance report (*.xwb)',/
     1 '        Revised Outmon.f to account for carrier losses in',/
     1 '          the water balance report (*.xwb)',/
     1 '        Revised Outmon.f and Outres.f to correct units on',/
     1 '          reservoir seepage',/
     1 '        Revised Outmon.f account for carrier losses in the',/
     1 '          reservoir reporting (*.xre)',/
     1 '    - 2006/10/24 (11.40)',/ 
     1 '        Revised CarRpl.f (type 7) to fix a problem when',/
     1 '          the destination is a reservoir with multiple',/
     1 '          accounts',/
     1 '   **   Revised DayDist.f approach to estimate daily data',/
     1 '          by connecting mid points to inlcude initial',/
     1 '          reservoir contents and data from a previous year',/
     1 '          (e.g. use Dec data to estimate Jan values',/
     1 '        Revised Putpath.f to recognize file names with 8',/
     1 '          characters if preceeded by a path',/
     1 '    - 2006/10/13 (11.39)',/      
     1 '        Revised simulate w/o reports to have less output',/
     1 '          to the screen to increase GUI performance',/
     1 '  **    Revised Seepage to be calculated at the end of a',/
     1 '          time step',/
     1 '        Revised RtnSecRP.f to store total recharge in variable',/
     1 '          pdrive',/
     1 '        Revised OutPln.f and OutPlnMo.f to print recharge to',/
     1 '          a recharge plan (type 8) output',/
     1 ' Recent updates',/
     1 '    - 2006/10/17 (11.382)',/ 
     1 '        Revised type 34, Rsrspu.f, to add an OOP bookover',/
     1 '         to the demand total (pdrive)',/
     1 '    - 2006/10/17 (11.381)',/ 
     1 '        Revised Oprinp.f to allow a type 34, Bookover with',/
     1 '          a Plan, to allow the plan data to be an OOP plan',/
     1 '        Revised type 34, Rsrspu.f, to allow the plan data',/
     1 '          to be an OOP plan at the destination',/
     1 '        Revised Sepsec.f to limit seepage to storage',/
     1 '        Revised Mdainp.f to redefine reservoir return data',/
     1 '        Revised Common.inc to include numrtnRP, the counter',/
     1 '          for reservoir returns',/
     1 '        Added GetRtnX.f to read reservoir return data',/
     1 '          with a future goal to use it for all return flow',/
     1 '          data reads (currently only used for resevoirs)',/
     1 '    - 2006/10/16 (11.38)',/ 
     1 '        Revised type 38, OOpDiv.f (OOP diversion) to correct',/
     1 '          a unit problem when the destination is a diversion',/
     1 '    - 2006/10/16 (11.37)',/ 
     1 '        Revised type 41, ResrgP.f (reservoir with special',/
     1 '          limits) to allow storage above a target that',/
     1 '          allows a plan demand to decrease and, with a',/
     1 '          release to target, results in a paper fill',/
     1 '        Revised type 38, OOpDiv.f and type 8, OopBook2.f',/
     1 '          to not track then limit amount diverted by an',/
     1 '          operating rule in variable divOpr. This limit',/
     1 '          is no longer required',/
     1 '        Revised Resrg1.f, standard reservoir storage, which',/
     1 '          was inappropriately tied to variable imonsw in ',/
     1 '          version 11.31 to determine if it is on or off.',/
     1 '          Note variable imonsw controls operating rules only',/
     1 '         (not a standard reservoir storage.',/ 
     1 '    - 2006/10/10 (11.36)',/ 
     1 '        Revised (RsrSpuP.f) to correct a Type 34, Bookover,',/
     1 '          problem related to the units of plan storage',/
     1 '    - 2006/10/09 (11.35)',/ 
     1 '        Revised (Oprinp.f) to correct testing on a Type',/
     1 '          34, Bookover, operating rule data check AGAIN',/
     1 '    - 2006/10/06 (11.34)',/ 
     1 '        Revised (Oprinp.f) to correct testing on a Type',/
     1 '          34, Bookover, operating rule data check',/
     1 '    - 2006/10/06 (11.33)',/ 
     1 '        Revised type 41 (ResRGP.f and Oprinp.f) to correct',/
     1 '          the distribution of storage to multiple OOP Plans',/
     1 '    - 2006/10/05 (11.32)',/ 
     1 '        Revised type 41 (ResRGP.f) to correct an',/
     1 '          inappropriate data check',/
     1 '    - 2006/10/03 (11.31)',/ 
     1 '        Added a type 42 (PowSeaR.f) Plan Demand Reset to',/
     1 '          allow an OOP Plan demand to be reset to zero',/
     1 '        Added a type 41 (ResRgP.f) Reservoir Storage with',/
     1 '          Special Limits to allow reservoir storage limited',/
     1 '          to the volume in one or more OOP Plans',/
     1 '        Revised the type 27 (DivResP2.f) Reservoir or Reuse',/
     1 '          Plan to Misc to allow an OOP plan to limit the',/
     1 '          transfer',/
     1 '        Revised type 34 (RsrspuP.f) Reservoir to Reservoir',/
     1 '          rule to allow an OOP plan to limit the transfer',/
     1 '        Revised type 10 (Replace.f) to remove all limits',/
     1 '          associated with a subordinated decree or an ',/
     1 '          Out-of-Priority plan demand',/
     1 '        Revised all operating rules to allow daily onoff',/
     1 '          capability',/
     1 '        Revised, DivCar,  DivCar1, DirectEx, DirectBy,',/
     1 '          DivresP, DivRes,  ResRpl,  RsrSpu,   CarRpl,',/
     1 '          OopBook2,DivRplP, DivCarR, DivResR,  DivRplR,',/
     1 '          RsrSpuP, DivImpR, and OOpDiv to allow reservoir',/
     1 '          deliveries to more than 1 account',/
     1 '        Revised Divcar, divcar1, divcar2, divcarr, divrig,',/
     1 '          rtnsec, and rtnmax  to remove variable Currtn',/
     1 '          that allows returns to the diverting node',/
     1 '        Revised OutDeb to include water right ID in the',/
     1 '          water right report (*.xwr)',/
     1 '        Revised OutBal2 to correct a problem if a bookover',/ 
     1 '          from one account to another occurred within the',/
     1 '          the same reservoir. This revision included:',/
     1 '          - Revising RsrSpu, RsrSpuP, and OopBook2 to include',/
     1 '            qres(29 that tracks the above occurance and ',/
     1 '          - Revising OutMon to redefine the variable Carry',/
     1 '            to include the above adjustment',/
     1 '    - 2006/09/15 (11.30)',/ 
     1 '        Revised Divcar.f and Outmon.f to correct carrier',/
     1 '          loss reporting',/
     1 '        Revised Divcar.f (Carrier) to allow carrier losses',/
     1 '          to be routed back to the system by calling a new ',/
     1 '          return flow routine named RtnXcu.f when the',/     
     1 '          destination is a reservoir',/
     1 '        Revised DivrigS.f (type 36 Meadow right) to not',/
     1 '          be included in reoperation check',/
     1 '        Revised Execut.f to provide more data when printing',/
     1 '          reoperation data (ichk=9)',/     
     1 '        Revised Mdainp.f to use acreage data from *.ipy when',/
     1 '          initilizing soil moisture if the *.ipy file is ',/
     1 '          provided. Note befor this edit soil moisture was',/
     1 '          initialized using data from the station files ',/
     1 '          (*.dds and *.wes). These (especilly *.wes) may not',/
     1 '          equal data in the *.ipy file if they were',/
     1 '          developed from non Gis (e.g. structure) sources',/
     1 '        Began to implement new CU calculations by:',/ 
     1 '        1. Replaced Return.f with Return2.f',/
     1 '        2. Revised Mdainp.f and Common.inc to store',/
     1 '           carrier flood, and sprinkler efficiency',/     
     1 '    - 2006/08/28 (11.29)',/ 
     1 '        Revised Oprinp.f for a carrier (Type 11) rule',/
     1 '          to leave a reservoir right on when it is',/
     1 '          a source',/
     1 '        Revised Divcar.f for potential division by 0',/
     1 '        Added type 40 rule for South Platte Compact',/
     1 '        Began type 39, Alternate Point, development',/
     1 '        Revised Closs.f to warn but continue if the',/
     1 '          total return flow is >100 but < 100.1',/
     1 '    - 2006/08/21 (11.28.0)',/ 
     1 '        Revised Common.inc and Replace.f to allow 25',/
     1 '          Replacement reservoirs',/
     1 '        Revised Oprinp.f and Divcar.f to allow a Carrier',/
     1 '          (type 11) to supply multiple reservoir accounts',/
     1 '  **    Revised Mdainp to fix a problem reading a Rio',/
     1 '          Grande spill file (*.rgs)',/          
     1 '        Revised Outdivc.f to clarify carrier treatment',/
     1 '          for a diverison comparison',/
     1 '        Revised Chekts.f to use the correct unit conversion',/
     1 '          when precip and evap data are provdied in inches',/
     1 '    - 2006/08/08 (11.27)',/ 
     1 '        Revised Oprinp.f to read variable intern(1,k)',/
     1 '          correctly when the monthly switch (dumx) is',/
     1 '          less than zero',/
     1 '    - 2006/08/08 (11.26)',/ 
     1 '        Revised Oprinp.f to eliminate redundant operating',/
     1 '          right checks for types 24, 25, 32 and 33',/
     1 '        Revised Oprinp.f, Evasec.f, and common.inc to',/
     1 '          recognize a reservoir plan may be tied to a',/
     1 '          reservoir more than once',/
     1 '        Revised DivoutC.f to fix a problem with the diversion',/
     1 '          comparison report (*.xdc) showing pumping because',/
     1 '          of adding losses to the binary file (*.b43).',/
     1 '    - 2006/08/04 (11.25)',/ 
     1 '        Revised Type 8 (OOP diversion) to allow a tie',/
     1 '          to an OOP plan ID and a type 38 (OOP diversion)',/
     1 '          rule',/
     1 '        Revised Type 10 (Replace.f & Oprinp.f) to allow',/
     1 '          a tie to an OOP plan.',/
     1 '          Note if tied to an OOP Plan source 2 is the',/
     1 '          subordinated reservoir right and reuse',/
     1 '          is the OOP Plan. Note no longer require a type 10',/
     1 '          to provide an associated operating rule so that',/
     1 '          many structures can be tied to the same OOP Plan',/
     1 '        Revised Oprinp to read opr type 1 (Res to ISF) and',/
     1 '          opr type 13 (ISF tied to an Index) using oprfind.f',/
     1 '    - 2006/07/21 (11.24)',/ 
     1 '   **   Revised Vircom.f, Virest.f and Virout.f to print',/
     1 '          Loss and Pumping to the baseflow information',/
     1 '          file (*.xbi).',/
     1 '        Revised Vircom.f to fix a problem printing from soil',/
     1 '          in the baseflow information file (*.xbi).',/
     1 '        Revised Oprinp to NOT turn off a source water',/
     1 '          right when assigned to a Type 11 (carrier) rule',/ 
     1 '        Revised Mdainp to correct a problem opening a ',/
     1 '          precipitation file when running in a baseflow mode',/
     1 '    - 2006/07/14 (11.23)',/ 
     1 '        Replaced Type 8, Bookover, (OopBook with OopBook2)',/
     1 '          and revised Oprinp to better handle a bookover',/
     1 '          from both an OOP diversion and an OOP storage',/
     1 '        Revised Mdainp to correct a bad warning regarding',/
     1 '          acrage data in *.ipy or *.tsp',/
     1 '        Added a new data check to Oprinp for a type 36',/
     1 '          (meadow right) data',/
     1 '        Revised Riginp, Bomsec & common to use variable ',/
     1 '          iResOpr as a reservoir on/off indicator so that',/
     1 '          the 1 fill calcs are OK even if a reservoir',/
     1 '          right is part of an operating rule',/
     1 '        Revised Oprinp to turn off a type 11 direct flow',/
     1 '          right if controlled by an operating rule',/ 
     1 '    - 2006/06/16 (11.22)',/ 
     1 '        Revised Carrpl to fix a reporting problem for global',/
     1 '          water balance',/
     1 '    - 2006/06/14 (11.21)',/ 
     1 '        Revised Outcu to include more significant figures',/
     1 '          on output',/
     1 '        Revised Replace to correct a problem that impacted',/
     1 '          variable divd(), the amount diverted by a direct',/
     1 '          diversion right',/
     1 '        Renamed ResOOP to OOPBook to be more descriptive',/
     1 '          (e.g. it books over OOP water)',/
     1 '        Added OOPDiv, a type 38 rule, that allows an OOP',/
     1 '          diversion to a reservoir or diversion',/
     1 '          Note this enhancement allows an OOP direct flow',/
     1 '          diversion and replaces the old approach to',/
     1 '          an OOP reservoir storage that required an OOP',/
     1 '          right in the reservoir right (*.rer) file',/
     1 '    - 2006/06/12 (11.20)',/ 
     1 '        Revised Outpln and OutplnMo to inlcude Use 10',/
     1 '          instead of delta storage for reservoir output',/
     1 '        Revised Oprinp, OutPln and OutPlnmo to allow more',/
     1 '          up to 20 Plan uses printed to the header',/
     1 '    - 2006/06/12 (11.19)',/      
     1 '        Revised Execut reoperation check to 0.01 cfs',/
     1 '       (0.61 af/mo)',/     
     1 '    - 2006/06/07 (11.18)',/   
     1 '        Revised DivResr (type 32) to report correctly',/
     1 '        BEGAN to revise OOP storage by the following',/
     1 '          four items. NOT COMPLETE',/
     1 '        1.Revised common to include a variable for a ',/
     1 '          reservoir paper fill (RitPaper)',/
     1 '        2. Revised Bomsec to initialize the paper fille',/
     1 '        3. Revised Resrg1 to limit the diversion by an OOP ',/
     1 '          right to the Seniors paper fill right',/
     1 '        4. Revised Resoop logic to limit a bookover to the',/
     1 '          Junior OOP ownership - Seniors Available Capacity',/
     1 '          (e.g. tranlim=amax1(curown(irow)-SeniorA, 0.0)',/
     1 '        Revised Datinp, Mdainp, & Common to correct a',/
     1 '          problem reading both monthly and annual instream',/
     1 '          flow data when provided in a random order',/
     1 '        Revised Bomsec to correct a problem related to',/
     1 '          the dimension of a reservoir right loop',/     
     1 '        Revised OutPln and OutplnMo to correct a problem',/
     1 '          that redefined maxres',/
     1 '        Revised Powsea to correct a problem when variable',/
     1 '          iown = 0',/
     1 '    - 2006/05/30 (11.17)',/                 
     1 '        Revised Planeva to calculate plan evaporation',/
     1 '          when the reservoir plan is a supply (operating',/
     1 '          rules type 32 and 33)',/
     1 '        Revised ChekTs for correct treatment of units',/
     1 '        assigned to an annual TS parameter (*.ipy or *.tsp)',/
     1 '    - 2006/05/24 (11.16)',/            
     1 '        Revised Oprinp to correct a problem defining data',/
     1 '          for a type 8, Out-of-Priority, Operating Rule',/
     1 '        Revised Bomsec to limit an Out-of-Priority right',/
     1 '          to the associated senior right',/
     1 '        Revised Bomsec to calculate reservoir 1 fill',/
     1 '          rule correctly if a reservoir right is off',/     
     1 '    - 2006/05/02 (11.15)',/       
     1 '        Revised Divcar to correct a problem that occurred',/
     1 '          with version 11x updates related to limiting',/
     1 '          the diversion to a water right, allow the water',/
     1 '          right to be located at the river or reservoir, ',/
     1 '          fill rate limit and carrier loss',/
     1 '        Revised daydist to include better messages',/
     1 '          for checking daily data when ichk=6',/      
     1 '        Revised Virin and Getfn to read a ',/
     1 '          StreamGage_Structure (*.rig) file',/
     1 '          to correct a problem with assigning daily',/
     1 '          estimation data for a daily baseflow run',/     
     1 '        Revised Oprinp, Execut and Common to allow both',/
     1 '          a start year and end year for operating rules',/     
     1 '        Revised Mdainp and GetFn to allow both monthly',/
     1 '          and annual evaporation and precipitation data',/  
     1 '        Revised Divres, ResOop, and ResRpl to reset the',/
     1 '          diversion to zero if less than a minimum limit',/
     1 '    - 2006/05/01 (11.14)',/     
     1 '        Revised several routines based on a detailed',/
     1 '          compilation check. Most edits were related',/
     1 '          to output reporting and correcting data types',/
     1 '          (e.g. ifix() instead of float())',/
     1 '        Revised Oprinp to correct reading of data for ',/
     1 '          a type 29 Plan Spill',/
     1 '        Revised Sepsec to do seepage calculations even if',/
     1 '          no reservoir seepage (return flows) data is provided',/
     1 '          Note if no return flow data is provided seepage',/
     1 '          is determined to be a loss',/
     1 '        Revised Outbal2 to correct the water balance',/
     1 '          reporting of reservoir seepage and loss',/     
     1 '        Began development of type 37, Augmentation Well',/
     1 '          operating rule',/
     1 '    - 2006/04/24 (11.13)',/          
     1 '        Revised DivResR (type 32 Reservoir and Reuse Direct)',/
     1 '          to include plan water in storage',/
     1 '        Revised DivRplR (type 33 Reservoir and Reuse Exch)',/
     1 '          to include plan water in storage',/
     1 '        Revised PowseaP (type 29 Plan Spill) to include',/
     1 '          plan water in storage for a reservoir plan ',/
     1 '          (plan type 3 or 5)',/
     1 '        Revised Oprinp to check a type 32 and 33 opr rules',/
     1 '          have a reservoir (type 3 or 5) plan specified',/     
     1 '        Revised Vircom to correct a problem with baseflows',/
     1 '          that occurred in Virset edits under Version 11.12',/ 
     1 '    - 2006/04/17 (11.12)',/          
     1 '        Finished enhancements to type 29 rule, Plan to Mis.',/
     1 '          that allow reservoir seepage (Recharge Pits) to',/
     1 '          be a water supply for a plan',/
     1 '        Revised Virset to initilze CU for Baseflow reporting',/
     1 '        Revised Outbal2 to include reservoir seepage in the ',/
     1 '          water balance calculations',/
     1 '        Revised Outbal2 to include reservoir seepage loss in ',/
     1 '          the water balance report',/
     1 '        Revised Outmon to include reservoir seepage loss in',/
     1 '          the binary reservior file',/
     1 '    - 2006/04/07 (11.11)',/     
     1 '        Began enhancements to allow reservoir seepage to be',/
     1 '          a source of augmentaion water. Note seepage can',/
     1 '          now be routed to any number of river nodes using',/
     1 '          any delay pattern. Previously seepage was routed',/
     1 '          downstream of the reservoir at the time it occurred',/
     1 '        Replaced Getfn, GetRes, Common to read reservoir',/
     1 '          return data.',/
     1 '        Revised Oprinp to correct the date when a diversion',/
     1 '          right is controlled by a type 16 operating rule',/     
     1 '        Revised Powsea, a type 9 opr rule to limit the',/
     1 '          minimum release to target to 0.1 af per time step',/
     1 '          This eliminates unnecessary reoperations with a',/
     1 '          reservoir forecast',/     
     1 '        Revised chekts to clarify the unit data is',/
     1 '          not used for an annual time series file (*.tsp',/
     1 '          or *.ipy)',/
     1 '        Revised evasec minimum evaporation from 0.1 af per',/
     1 '          time step to 0.001 af/time step',/
     1 '        Revised Bomsec to correct plan reporting problems',/
     1 '          for plan types greater than 3',/
     1 '    - 2006/03/22 (11.10)',/     
     1 '        Replaced WelRig with WelRigP to begin to implement',/
     1 '          well augmentation',/ 
     1 '        Revised DirectBy to allow a Plan destination',/ 
     1 '        Revised OutPlnMo and OutPln to print the amount',/
     1 '          bypassed or exchanged associatd with a T&C plan',/
     1 '        Revised OutPlnMo and OutPln to print the amount',/
     1 '          pumped associatd with a Well Aug plan and the',/
     1 '          amount pumped in priority',/
     1 '        Revised PowseaP a type 29 opr rule to allow a spill',/
     1 '          from both a plan and a reservoir. Note the',/
     1 '          reservoir must be source 1 and the plan source 2',/
     1 '          Note can release from a non reservoir plan when',/
     1 '          source 1 is a plan',/
     1 '        Revised PowseaP a type 29 opr rule to correct a ',/
     1 '          problem associated with spilling from both a plan',/
     1 '          and a reservoir',/     
     1 '        Revised Planeva to correct a problem allocating',/
     1 '          evaporation to a plan',/
     1 ' Recent updates',/
     1 '    - 2006/03/21 (11.09)',/     
     1 '        Minor cleanup to Execut, Chekts, Mdainp',/
     1 '          regarding convergence check  & processing when a',/
     1 '          time series units are inconsistent with the ,',/
     1 '          control file',/
     1 '        Revised outmon to NOT include water from soil',/
     1 '          moisture in the River Divert column of *.xdd',/
     1 '          Note the river calculations were OK, only the ',/
     1 '          reporting had a problem',/
     1 '        Corrected a reporting problem with *.xdd & *.xdy',/
     1 '          related to call reporting that occurred after',/
     1 '          adding additional data to the binary outputs',/
     1 '          in version 11.01',/
     1 '        Revised water district (*.xwd) report to include',/
     1 '          a summary of diversions and shortages',/ 
     1 '        Revised Oprinp related to reading source 2 data',/
     1 '          for a type 10 operating rule',/     
     1 '        Revised all character ID entries to be left',/
     1 '          justified',/
     1 '        Revised Divcar to limit to carrier capacity and water',/
     1 '          right when the source is a water right',/
     1 '        Revised Outbal2 to correct a problem that originated',/
     1 '          with adding loss to the reservoir balance (*.xre)',/
     1 '          in Version 11.01',/
     1 '        Added GetplnW to read plan well association data',/
     1 '    - 2006/01/23 (11.08)',/     
     1 '        Minor cleanup to Chekpor, Dayest and Daydata',/
     1 '          regarding reading and writing daily data',/
     1 '    - 2006/01/18 (11.07)',/     
     1 '        Added type 37 Opr Rule for Well Augmentation',/
     1 '        Revised Oprinp, Execut and Statem for above',/     
     1 '        Added welRigA to control a type 37 operating rule',/
     1 '    - 2006/01/18 (11.06)',/     
     1 '        Revised Outmon and Dayoutr to print the reservoir',
     1 '          number to *.b43 and *.b50.',/
     1 '        Revised Outres, Outsys, Outresc, Outbal, Dayreso',/
     1 '          to include the reservoir #',/
     1 '        Revised Outsys to print diversion binary to *.xbn',/
     1 '          and reservoir binary to *.xbr',/
     1 '        Revised Outsys to print record count to *.xbn & *.xbr',/
     1 '    - 2006/01/18 (11.05)',/     
     1 '        Add type 36 Opr Rule for seasonal (Meadow Rights)',/
     1 '        Revised Oprinp, Execut and Statem for above',/     
     1 '        Added divRigS to control a type 36 operating rule',/
     1 '    - 2006/01/04 (11.04)',/     
     1 '        Revised Divcar (type 11) and Oprinp',/
     1 '          to allow a right to be operated at its',/
     1 '          decreed location, or a reservoir or a diversion',/
     1 '        Revised PowResp (type 26), DivResP (type 27)',/ 
     1 '          DivRplP (type 28),PowSeaP (type 28), DivCarR ',/
     1 '          (type 31), DivresR (type 32), DivrplR (type 33)',/
     1 '          RsrSpuP (type 34) & DivImpR (type 35) to correct',/
     1 '          a problem related to carry over storage in a ',/
     1 '          reservoir plan',/
     1 '    - 2005/12/23 (11.03)',/     
     1 '        Revised Bintop to include ndivO, nresO, and nwelO',/
     1 '        Revised Datinp to fix a problem related to ',/
     1 '          aggressive use of units in TS files.',/
     1 '    - 2005/12/19 (11.02)',/     
     1 '        Revised Bintop to include units on output',/
     1 '        Note the above REVISES THE BINARY FILE SIZE',/
     1 '        Revised to read a RioGrande_Spill_Monthly file',/
     1 '          and use this data to adjust the demand when ',/
     1 '          a spill at Elephant Butte is indicated',/
     1 '        Revised to aggresively use units data in TS files',/
     1 '          to set StateMod conversion factors',/     
     1 '        Revised datinp to allow a # in the river station',/
     1 '          file (*.rin)',/
     1 '        Revised all binary file headers to include the ',/
     1 '          model version and output parameter types.',/
     1 '        Revised type 11, Divcar, to inlcude an additional',/
     1 '          limit on the amount carried by a diversion.',/
     1 '          note this limit is often applied when a reservoir',/
     1 '          has both a storage decree and a fill rate limit'/
     1 '        Revised Type 23, 24, 25, ... operating rights based',/
     1 '          on additional checks',/
     1 '    - 2005/11/30 (11.01)',/     
     1 '        Revised type 11, Divcar, to include carrier loss',/
     1 '        Revised all diversion and reservoir reports to',/
     1 '          include River Loss and Carrier Loss',/
     1 '        Note the above REVISES THE BINARY FILE SIZE',/
     1 '        This is why I named it version 11.xx')
c
 210     format(         
     1 '    - 2005/11/12 (10.74)',/     
     1 '        Revised reservoir plan operations to include evap',/
     1 '        Revised Execut, Bomsec, Outmon, Dayset, DirectEx,'/
     1 '          DirectBy, DivresR, DivRplR and Common for above',/     
     1 '        Revised Oprinp to correct a problem that caused ',
     1 '          a Type 8 (out-of-priority) operating right to be ',/
     1 '          turned off',/
     1 '        Enhanced Oprinp to simplify how type 8 Out-of-',/
     1 '          Priority Storage, operating rule data is read',/
     1 '    - 2005/11/11 (10.73)',/     
     1 '        Revised a type 11 (carrier) operating rule to allow',/
     1 '          the source right to be a reservoir to better handel',/
     1 '          off-channel reservoirs. Resulted in changes to ',/
     1 '          Divcar, Oprinp, and Oprfind',/
     1 '    - 2005/11/01 (10.72)',/     
     1 '        Revised Year2 to include the starting month',/
     1 '        Revised DivresR to release the Diversion ',/
     1 '          or Depletion from a reservoir',/
     1 '        Removed plan read from Datinp to GetPln',/
     1 '    - 2005/10/26 (10.71)',/     
     1 '        Revised Datinp by moving diversion station reads',/
     1 '          to separate routines (GetDiv or GetDiv2).',/ 
     1 '        Added GetDiv2 to handle new well station format',/
     1 '          (e.g. *.dds = *.dst, *.def, and *.drf)',/          
     1 '    - 2005/10/25 (10.70)',/
     1 '        Revised Bomsec to correct La Plata demand',/
     1 '          initilzation',/
     1 '    - 2005/10/22 (10.69)',/
     1 '        Revised Dapinp and Mdainp to operate if no diverions',
     1 '          are provided',/
     1 '        Revised Mdainp to allow all time series except',/
     1 '          reservoir targets to be provided in a random order',
     1 '          (random by station not year)',/
     1 '          Note cannot provide targets randomly because more',/
     1 '          than 1 year is read at a time for forecast calcs',/
     1 '        Revised datinp by moving well station reads',/
     1 '          to separte routines (GetWel or GetWel2).',/ 
     1 '        Added GetWel2 to handle new well station format',/
     1 '          (e.g. *.wes = *.wst, *.wef, *.wrf) and *.wde',/
     1 '    - 2005/10/06 (10.68)',/
     1 '        Moved the demand calculation for Opr Rule 13',/
     1 '          to Bomsec so that it is calculated based on natural',/
     1 '          flows and can be satisfied by any other operating',
     1 '          rule (e.g. Long Hollow Res)',/
     1 '    - 2005/09/21 (10.67)',/
     1 '        Revised Opr Rule 13 (La Plata Compact)',/
     1 '          to report calculated demand, not from data (*.ifa)',/
     1 '        Revised Opr Rule 1 Res to an ISF Reach',/
     1 '          to fix problems related to reach calculations',/
     1 '        Revised Mdainp regarding a plan warning',/
     1 '    - 2005/08/30 (10.66)',/
     1 '        Minor clean up to call reporting',/
     1 '     	Added detailed output to opr rule 11 (Divcar)',/
     1 '          and 14 (Divcar1)',/
     1 '    - 2005/08/29 (10.65)',/
     1 '        Added better error checking to operating rules',/
     1 '          related on monthly on/off and carriers',/
     1 '    - 2005/08/02 (10.64)',/
     1 '        Corrected error associated with 10.60 enhancement',/
     1 '          associated with reading call data',/
     1 '        Revised type 27 and 28 to allow reuse of destination',/
     1 '          water',/
     1 '        Revised old type 33 (Import ...) to Type 25',/
     1 '        Added new type 33 Reservoir with reuse to a diversion',/
     1 '          with reuse by Exchange',/
     1 '        Revised types 24 and 25 to leave source right on',/
     1 '          Note this turns back an adjustment on 6/15/2005',/
     1 '        Revised type 24 and 25 to read variable cdivtyp for',/
     1 '          Diversion or Depletion (CU)',/
     1 '        Added call reporting to *.xdd and *.cal',/
     1 '    - 2005/07/22 (10.63)',/
     1 '        Added ability to provide initial condition data',/
     1 '          to a reservoir reuse plan',/
     1 '        Added a warning to the *.log file to include an',/
     1 '          operating rule to spill a non-reservoir reuse plan',/
     1 '          if one is not provided',/
     1 '        Revised control file (Datinp) to read number of ',/
     1 '          significant figures on output',/
     1 '        Revised type 25 (DirectBy) to fix reporting problems',/
     1 '        Revised type 32 (DivresR) to limit supply based on ',/
     1 '          the source plan capacity',/
     1 '        Revised Oprinp to read a type 2 (divres) using the',/
     1 '          generic Operation right read routine OPrfind',/
     1 '        Began developing RsrspuP (type 34) bookover with reuse',/
     1 '    - 2005/06/29 (10.62)',/
     1 '        Corrected a reporting problem with type 28 (divrplP)',/
     1 '        Revised Execut to use a lower reoperation value for a',/
     1 '        daily model and now report value to *.log',/
     1 '        Revised DirectEx initilization values for diversions',/
     1 '        Added detailed check ability to DirectEx, DirectBy, ',/
     1 '        and DivrplP',/
     1 '    - 2005/06/22 (10.61)',/
     1 '        Revised type 31 (divcarr) to correct a problem',/
     1 '        related to diversion type and added detailed output',/
     1 '    - 2005/06/21 (10.60)',/
     1 '        Revised call data to free format & a 4 diget year',/
     1 '        revised dayest to use correct year for call data POR',/
     1 '    - 2005/06/15 (10.59)',/
     1 '        Revised types 24 and 25 to leave source right on',/
     1 '    - 2005/06/07 (10.58)',/
     1 '        Revised Oprinp types 25 and 31 to not check',/
     1 '        accounts if the source is a water right',/
     1 '    - 2005/05/12 (10.57)',/
     1 '        Revised DivrplP regarding release for depletion',/
     1 '    - 2005/05/12 (10.56)',/
     1 '        Corrected read of daily call data',/
     1 '        Revised Opr Rule types 24 & 25 to reoperate',/
     1 '    - 2005/04/26 (10.55)',/
     1 '        Revised Opr Rule types 24 & 25 to allow the percent',/
     1 '        ownership to be a real (not an integer).',/
     1 '    - 2005/04/04 (10.54)',/
     1 '        Revised Opr Rule types 27 & 28 to specify the type',/
     1 '        of release (Depletion or Diversion)',/
     1 '        Revised Type 23 downstream call to reoperate for ',/
     1 '        new water (reservoir releases and non ds returns',/ 
     1 '    - 2005/04/04 (10.53)',/
     1 '        Added Opr Rule type 33 TransMtn Import with Reuse',/
     1 '    - 2005/03/29 (10.52)',/
     1 '        Added Opr Rule type 31 Carrier with Reuse',/
     1 '        Miscellaneous clean up on plan operations',/
     1 '    - 2005/03/09 (10.51)',/
     1 '        Revised Opr Rule old type 23 Direct Exchange to 24 &'/ 
     1 '        old type 24 Downstream Call to 23 to simplify doc.',/
     1 '        Added StateCU_Structure to the random file option',/
     1 '        Revised the selected parameter output (*.xsp) to not',/
     1 '        allow ALL as a structure type ',/
     1 '    - 2005/02/02 (10.50)',/
     1 '        Added Opr Rule type 30 Reservoir Re Diversion',/    
     1 '    - 2005/02/01 (10.49)',/
     1 '        Corrected dead storage treatment',/
     1 '        Enhanced Opr type 23 Direct Flow Exchange',/
     1 '        and Opr Type 25 Direct Flow Bypass',/
     1 '    - 2005/01/03 (10.48)',/
     1 '        Corrected Virin for ability to read a standard',/
     1 '        and random input response file (*.rsp)',/
     1 '        Began to develop Opr Rule type 27',/
     1 '        Plan to a Div. by Exchange and Opr Rule type 28',/
     1 '        Plan to a Div direct',/
     1 '    - 2004/12/06 (10.47)',/
     1 '        Added Opr Rule type 26 Reservoir to a Plan',/
     1 '    - 2004/11/17 (10.46)',/
     1 '        Added Opr Rule type 25 Direct Flow Bypass',/
     1 '    - 2004/11/12 (10.45)',/
     1 '        Revised virin & chekpor to correct a problem with',/
     1 '        reading a response file in a random order',/
     1 '    - 2004/10/13 (10.44)',/
     1 '        Revised parse & statem to operate when the type',/
     1 '        of run (e.g. simulate, etc.) is not specified',/
     1 '    - 2004/10/13 (10.43)',/
     1 '        Revised maximum delay files from 550 to 3100',/
     1 '        Revised output format to *.xtp in dayest and outtop',/
     1 '    - 2004/09/07 (10.42)',/
     1 '        Revised to allow multiple owners for diversions',/
     1 '        Note multiple users at a strcuture are still not',/
     1 '        supported. Enhanced datinp.f, ',/
     1 '    - 2004/09/03 (10.41)',/
     1 '        Reuse of Transmountain and CU credits added by',/
     1 '        revising Divcar (type 11) to allow a reservoir ',/
     1 '        to store above its target when res type (iresse=3)',/
     1 '        else see documntation section 8.16',/
     1 '    - 2004/08/24 (10.40)',/
     1 '        Added new output data when detailed call info',/
     1 '        is requested icall=1',/
     1 '        Added a new output file *.chk for call data',/
     1 '        Added Operation Right 24 Downstream Call',/
     1 '    - 2004/08/23 (10.39)',/
     1 '        Revised to read a new AWC file format (*.str)',/
     1 '        Revised Parse to handle a command line response file',/
     1 '        with a suffix *.rsp)',/
     1 '    - 2004/06/16 (10.38)',/
     1 '        Redimensioned for Linked Model',/
     1 '    - 2004/05/06 (10.37)',/
     1 '        Revised Daysist to print total daily stream estimates',/
     1 '        (not gains) when ichk = 8',/
     1 '    - 2004/04/14 (10.36)',/
     1 '        Revised the roundoff check in evasec.f',/
     1 '    - 2003/11/03 (10.35)',/
     1 '        Revised all programs to include an exit code to',
     1        ' assist with the GUI operation',/
     1 '    - 2003/11/03 (10.34)',/
     1 '        Revised output from Bintop',/
     1 '    - 2003/10/24 (10.33)',/
     1 '        Revised riginp, common, and resrg1 to include',
     1        ' an option to store a reservoir water right in all',/
     1 '        accounts based on their ownership ratio',
     1        ' when variable iresco(2,k) = 0) ',/
     1 '        Began testing of Exit routine to Statem.f only',/     
     1 ' Recent updates',/
     1 '    - 2003/10/24 (10.32)',/
     1 '        Revised update messages to print more data per line',/
     1 ' Recent updates',/
     1 '    - 2003/10/17 (10.31)',/
     1 '        Corrected a problem to Virin that occurred',
     1        ' with the 10.30 update.',/
     1 '    - 2003/09/10 (10.30)',/
     1 '        Added getfn.f to allow a random response file read. ',/
     1 '        Revised datinp, mdainp, riginp, & virgin to allow',
     1        ' a random response file read.',/
     1 '        Revised virout to include:',/
     1 '          StreamGage_base_monthly (*.xbg) output',/
     1 '          StreamEstimate_base_monthly (*.xbe) output',/
     1 '          StreamGage_base_Daily (*.xgy) output',/
     1 '          StreamEstimate_base_Daily (*.xey) output',/
     1 '    - 2003/08/15 (10.29)',/
     1 '        Standardized comments throughout program',/
     1 '    - 2003/07/07 (10.28)',/
     1 '        Revised Ouflow to correct a problem related to',
     1        ' knowing when data is missing during a study period.',/
     1 '    - 2003/06/02 (10.27)',/
     1 '        Revised Statem and Parst to allow the log file',
     1        ' to be optional to operate with GUI on a cd.',/
     1 '    - 2003/03/03 (10.26)',/
     1 '        Revised virout and vircom.f to handle missing data',
     1        ' (-999) better; Particularly for relatively',/,
     1 '        large flows on the San Juan.',/
     1 '    - 2002/11/20 (10.25)',/
     1 '        Added detailed daily baseflow output for ichk=8',/
     1 '    - 2002/11/20 (10.24)',/
     1 '        Temporary Fix',/
     1 '        Revised Divrpl to not include variable efficiency',
     1        ' when evaluating a type 4 (exchange) operating rule.',/
     1 '    - 2002/10/28 (10.23)',/
     1 '        Revised Virset to correct where seteff.f is called',
     1        ' to correct a problem with version 10.21 in month 1.',/
     1 '        Revised oprinp, powres, powres2, divrpl, resrpl,',
     1        ' carrpl, resoop, powsea, divcar, ifrrigx, and',/
     1 '        divacr1 to allow a monthly on/off switch.',
     1        ' Revised divres to allow a release to depletion ',/
     1 '        (e.g. turned on the 10.20 edits for general use).',/

     1 '    - 2002/10/22 (10.22)',/
     1 '        Revise Oprinp and Reserpl.f to allow monthly on/off',
     1        ' switches for an operational right type 5.',/
     1 '        (res to res exchange)',/
     1 '    - 2002/10/09 (10.21)',/
     1 '        Add seteff.f to virset.f and bomsec.f so that',
     1        ' the baseflow sets default efficiency data properly.',/
     1 '        Note both baseflow and simulate use the same code.',/
     1 '    - 2002/10/08 (10.20)',/
     1 '        Test depletion option for a direct reservoir release',
     1        ' that can call divres1.f, not divres.f from execut.f.',/
     1 '        Currently not operational.',/
     1 '    - 2002/09/20 (10.19)',/
     1 '        Enhanced report.f to insure all output files',
     1        ' are closed.',/
     1 '    - 2002/09/13 (10.18)',/       
     1 '        Revised (acft) to acft to assist with GUI',
     1        ' interaction.',/

     1 '    - 2002/08/08 (10.17)',/       
     1 '        Revised Namext and Statem to allow a response file',
     1        ' with a .xxx at the end of the file.',/
     1 '        Note this allows a response file to contain any',
     1        ' suffix. Note files provided without any suffix still',/
     1 '        get .rsp added.',/
     1 '    - 2002/06/27 (10.16)',/       
     1 '        Revised Divrig, divcar, divcar1, & directex to',
     1        ' limit diversions to demand when max efficiency',/
     1 '        option is used.',/
     1 ' **     The above impacts results if max efficiency is used',
     1        ' and a diversion was initially',/
     1 '        limited by water supply, then later could divert',
     1        ' more than their demand. These conditions are',/
     1 '        considered rare but did occur with the Ym2002 data.',/
     1 '    - 2002/05/29 (10.15)',/       
     1 '        Revised Repsort printout to the log file to fix',
     1        ' the replacement reservoir rank table.',/
     1 '        Revised Divrpl to insure an exchange is at least as',
     1        ' great as the consumptive use.',/
     1 ' **     The above impacts results if a reservoir release is',
     1        ' set to offset depletion only (not diversion).',/
     1 '        Revised Outmon and DayourR to correct reporting',
     1        ' to the reservoir files (*.xre) Station Balance Total,'/
     1 '        Release, and Total Supply.',/
     1 '        Revised Outres lables as follows:',/
     1 '        Total Release = River Release',/
     1 '        Total Supply = River Diversion',/
     1 ' **     The above impacts reporting but not results.',/
     1 '    - 2002/05/09 (10.14)',/       
     1 '        Revised Outdivc, Outresc and Outwelc to pass',
     1        ' maxsta not maxuse to Average.f.',/
     1 '        The above impacts the total printed by the',
     1        ' comparison reports *.xdc, *.xrc, *.xwc.',/
     1 '        Also changed compiler to save local variables',/
     1 '    - 2002/05/07 (10.13)',/       
     1 '        For daily GUI processing of output:',/
     1 '        Revised outspd to handle streamflow,',
     1        ' revised outtop to print date, units and year type,',/
     1 '        Revised getpar to handle 5 parameters for future',
     1        ' edits and to allow outspd to operate as a command',
     1        ' line.',/

     1 '    - 2002/04/30 (10.12)',/       
     1 '        Revised getpar.f To_From_GW_Storage from',
     1        ' To/Ff_GW_Storage to operate better with the GUI.',/
     1 '    - 2002/04/02 (10.11)',/       
     1 '        Revised divrpl to correct a problem when the ',
     1        ' exchange is run with the depletion option on',/
     1 '        for certain network configurations.',/
     1 ' ***    May produce different results depending on the',
     1        ' network and exchange configuration',/
     1 '        (e.g. the depletion adjustment is downstream',
     1        ' of the exchange point (see example ex69/test2).',/
     1 '    - 2002/03/15 (10.10)',/       
     1 '        Revised divrpl to correct a problem when the ',
     1        ' exchange reservoir is downstream of the diversion',/
     1 '        doing the exchange.',/
     1 ' ***    May produce different results depending on the',
     1        ' network and exchange configuration',/
     1 '        (e.g. if the exchange node is the ',
     1        ' node with the minimum available flow).',/
     1 '    - 2002/03/01 (10.09)',/       
     1 '        Revised oprinp, return & dayest to allow ieffmax=2',
     1        ' which allows IWR data to be read and limit',/
     1 '        reservoir releases to occur only when an IWR',
     1        ' exists if the reservoir release variable is > 0.',/
     1 '        Note typically used when iday = 2 (daily demand)',
     1        ' is a running monthly total.',/
     1 '    - 2002/01/30 (10.08)',/       
     1 '        Revised evasec to handle roundoff issues better',
     1        ' when net evap is distributed to 1 account because',/
     1 '        others are full.',/
     1 '    - 2002/01/15 (10.07)',/       
     1 '        Revised virset by moving local dimension to common',
     1        ' and updated all local dimensions to use common',/
     1 '        or variable dimensions.',/
     1 ' ***    Corrects monthly baseflow report (*.xbi) problems ',
     1        ' when operated on a daily time step that occurred',/
     1 '        with the f95 update (V 10.04).',/
     
     1 '    - 2002/01/14 (10.06)',/       
     1 '        Revise parse.fto allow I/O to screen, ',
     1        ' revise baseflow output to *.log re. negative flows.',/
     1 '    - 2001/12/31 (10.05)',/       
     1 '        Added year type and units to graph reports,',
     1        ' revised dayest to not call instream flows during',/
     1 '        baseflow calculations.',
     1        ' Revised Virset to initialize CU, FromSoil, & ToSoil',/
     1 '        to correct *.xbi output for a daily baseflow run.',/
     1 ' **     Note impacts a report only, not a calcuated value',/
     1 '        Revised Virout to include positive res evap in CU.',/
     1 '    - 2001/12/27 (10.04)',/       
     1 '        Revised to operate under f95 by',
     1        ' adding variable dimension to several routines',/
     1 '        and initilization to several routines.',/
     1 '    - 2001/12/10 (10.03)',/
     1 '        Added iday=2 capability to allow the daily model',
     1        ' to use a monthly running demand.',/
     1 '        Revised outmon & dayoutr well printout to be',
     1        ' consistent for D&W structures.',/
     1 '        Revised outmon to removed redundant shortage code.',
     1        ' Revised outbal2 to correct the treatment of',/
     1 '        loss in *.xwb (*.xwb is total loss for a D&W).',/
     1 ' ***    Impacts water budget report loss column.',/

     1 '    - 2001/12/10 (10.02)',/       
     1 '        Revised outmon for a problem with 10.01 update.',/
     1 '    - 2001/12/07 (10.01)',/
     1 '        Revised carrpl to allow an exchange with a type 14', 
     1        ' operating rule and correct probs with a reservoir',/
     1 '        destination.',
     1        ' Revised outmon to correct probs with', 
     1        ' reporting carrpl results.',/
     1 '        Revised execut to call soilm and sprinkler from',
     1        ' inside the day loop to correct daily operation.',/
     1 ' ***    Impacts daily results with soil moisture.',/
     1 '        Revised mdainp and dayest to call closs so that',
     1        ' daily loss calculations are correct.')  
c
 209     format(     
     1 '    - 2001/11/03 (9.99)',/
     1 '        Revised outbal2 to insure res evap is positive when',
     1        ' calculating total cu.',/
     1 '        Revised rgrg.for for daily simulation. The',
     1        ' code now uses the average streamflow for the month',/
     1 '        to date, not a daily value for the entire month.',/
     1 '    - 2001/10/08 (9.98)',/
     1 '        Revised dimensions working toward a daily as follows:',/
     1 '        Max return tables 980 = 550.',/
     1 '        Max well return & delpetion locations 20000 = 10000.',/
     1 '        Daily return & depletions 930 = 7320 (20*366).',/
     1 '        Getrtnw local dimension revised 20000 = 10000',/
     1 '        Statem dimension sizes revised.',/
     1 '        Dayset sets returns that sum < 1% = 0%.',/
     1 '    - 2001/09/25 (9.97)',/
     1 '        Revised evasec to correct a problem when a ',
     1        ' reservoir is full.',/
     1 ' ***    Impacts results from version 9.92 on if a ',
     1        ' reservoir is full.',/
     1 '        Revised mdainp and bomsec & common to properly',
     1        ' reset default data when not provided in *.tsp.',/
     1 ' ***    Impacts results from version 9.64 on if itsfile', 
     1        ' is on, no *.tsp data is provided, and 12 different',/ 
     1 '        monthly efficiency values are provided in *.tsp.',/
     1 '    - 2001/08/31 (9.96)',/
     1 '        Revised # of files to skip for *.xsc if wells are ',
     1        ' not simulated. Added daily type 5.',/
     1 '        (estimate daily by connecting', 
     1        ' monthly end points) for reservoir use.',/
     1 '        Added capability to view call data for reservoirs', 
     1 '        and instream flows.',/

     1 '    - 2001/08/21 (9.95)',/
     1 '        Revised oprinp, divres, divrpl, & carrpl ',
     1        ' to limit reservoir releases to IWR / n ',/
     1 '        when n provided in *.opr is positive and',
     1        ' when operating with a variable efficiency.',/
     1 '    - 2001/08/21 (9.94)',/
     1 '        Revised evasec & datinp again (see 9.92) to correct',
     1        ' problem when more rain than evaporation.',/            
     1 ' ***    Impacts all prior results when rain > evaporation.',/
     1 '    - 2001/08/19 (9.93)',/
     1 '        Revised daydist & datinp to include',
     1        ' daily estimate type 4 (connect midpoints).',/
     1 '        Revised outbal2 to revise GW balance (*.xgw)', 
     1        ' recharge calculation and add loss as an output.'/
     1 '        Revised report to work from screen input', 
     1        ' by moving default read above call datinp.',/
     1 '    - 2001/08/07 (9.92)',/
     1 '        Revised datinp to test and warn or adjust Area',
     1        ' Capacity data when bad.',/
     1 '        Went back to old evapsec (fixed again 9.94)',/
     1 '    - 2001/08/03 (9.91)',/ 
     1 '        Revised evasec to correct a problem when more rain',
     1 '        than evaporation occurrs.', /
     1 '  ***   Impacts all prior results when rain > evaporation.',/ 
     1 '        Revised mdainp to accept a monthly ppt file.',
     1        ' Revised bomsec when soil moisture exceeds capacity.',/ 
     1 '        because the area gets smaller. The difference is no', 
     1        ' longer accounted as a loss to avoid problems with',/ 
     1 '        the *.xss reporting of return flows.',
     1        ' Revised outxss to correct problem reporting actual',/
     1 '        efficiency on an annual basis; No longer adjust', 
     1        ' for CU from soil moisture.',/
     1 '    - 2001/07/31 (9.90)',/
     1 '        Revised virset, vircom, virgen & common to allow',
     1        ' baseflow to track and print soil moisture and CU.',/
     1 '        Revised soilcu and soilm to allow',
     1        ' baseflow and simulate consumee water ',/
     1 '        if an IWR exists (not both an IWR and demand).',
     1        ' Revised outmon to correct divert from river by',/
     1 '        excluding CU from soil.',/
 
     1 '    - 2001/07/17 (9.89)',/
     1 '        Max dimension for daily back to 930 from 7320.',/
     1 '    - 2001/07/17 (9.88)',/
     1 '        Max dimension for daily 930 = 7320 (366*20).',/
     1 '    - 2001/06/25 (9.87)',/
     1 '        Added Direct Flow Exchange, an operating rule for a', 
     1        ' direct flow exchange (Type 23).',/ 
     1 '    - 2001/06/15 (9.86)',/
     1 '        Revised Virgen to skip the proper number of files',
     1        ' with and without wells.',/
     1 '        Revised Dayest and Daydata to print proper files',
     1        ' if in baseflow or simulate mode.',/
     1 '    - 2001/06/06 (9.85)',/
     1 '        Added daily special report (*.xds and *.xd2).',
     1        ' Added column output to special report (*.xs2).',/
     1 '        For the above revised outsp, outtop, report and parse',
     1        ' and created outspd.',/
     1 '    - 2001/05/22 (9.84)',/
     1 '        Revised report for a problem related to finding',
     1        ' the historic stream file for *.xdc',/
     1 '        and the output file for *.xsp.',/
     1 '    - 2001/04/23 (9.83)',/
     1 '        Enhanced Welrig and demand to fix problems with ',
     1        ' a type 3 demand calculation when demand data and',/ 
     1 '        IWR data are inconsistent.',
     1        ' Enhanced bomsec to warn if max eff < min eff',/
     1 '        Enhanced outxss for *.xss output issues related',
     1        ' to selected output.',/
     1 '    - 2001/04/12 (9.82)',/
     1 '        Enhanced Decreed Demand to only operate if CIR',
     1        ' data exists.',/
     1 '    - 2001/04/03 (9.81)',/
     1 '        For Simulate option, added variable units to all', 
     1        ' output files.',/
     1 '        Note for cfs output total is average.',
     1        ' For Baseflow option, began variable unit conversion.')

 208     format(
     1 '    - 2001/03/12 (9.80)',/
     1 '        Added daily baseflow capability by revising',/
     1 '        virgen, vircom, virnod, virout for daily',/
     1 '        Revised outputs for generic output unit control',/
     1 '        Revised interp to check for bad area capacity data',/
     1 '    - 2001/03/01 (9.79)',/
     1 '        Revised datinp to allow comments (#) within the',/ 
     1 '        control (*.ctl) file',/
     1 '        Revised datinp to print warning for non-downstream',/
     1 '        return locations',/
     1 '        Revised outdivc for treatment of split channels',/
     1 '        Revised outbal to print adjustment to storage',/
     1 '        Revised dayest for daily IWR data',/
     1 '        Revised dayest, dayoutr, outmon for daily checks',/
     1 '    - 2001/03/01 (9.78)',/
     1 '        Revised demand to fix demand option 4',/
     1 '        Revised datinp to allow comments in *.ctl',/
     1 '    - 2001/03/01 (9.77)',/
     1 '        Revised execut to call dayset before bomsec so', 
     1        ' monthly forecasts can be used for daily estimate',/
     1 '    - 2001/02/27 (9.76)',/
     1 '        Revised bomsec for error on setting default eff.',/   
     1 '        Revised outdivc to not print imports',/
     1 '        Revised outwr to total div and well rights',/
     1 '        Revised soilm & soulcu to use IWR',/  
     1 '    - 2001/02/26 (9.75)',/
     1 '        Added demand types 4 and 5 by revising',/
     1 '        Divrig, welrig, mdainp, bomsec, & common',/
     1 '    - 2001/02/10 (9.74)',/
     1 '        Revised execut, rtnsec and rtnsecw to improve',/
     1 '        reoperation chekcs',/
     1 '        Revised divrig, carrpl, divcar, divcar1, divcar2,'/ 
     1 '        and divrpl and rtnsec to have transmountain',/
     1 '        diversions (irturn=4) call rtnsec to allow new',/
     1 '        approach to calculate CU to operate properly',/
     1 '    - 2001/02/05 (9.73)',/
     1 '        Revised outdivc to correct carrier presentation',/
     1 '    - 2001/02/04 (9.72)',/
     1 ' ***    Revised welrig to correct total pumping error',/
     1 '        Revised outwr to print well rights for D&W',/
     1 '        Revised mdainp regardnig total demand',/
     1 '        Revised outmon return and loss to include wells',/ 
     1 '    - 2001/02/01 (9.71)',/
     1 '        Revised datinp, mdainp, and getrtnw to allow',/
     1 '        delay data to be provided as a decimal',/
     1 '        Revised divcar, divcar1, divcar2, & outmon to ',/
     1 '        fix carrier output problems',/
     1 '        Revised divrig, rtnsec and datinp to call return',/
     1 '        for transmountain diversions to get CU',/
     1 '    - 2001/01/30 (9.70)',/
     1 '        Revised welrig to limit pumping to *.wem for',/
     1 '        demand type 1.',/
     1 '        Revised outmon print of total demand and shortage',/
     1 '        including instream flow',/
     1 '    - 2001/01/27 (9.69)',/
     1 '        Revised *.xdd, *.xwe, *.xwb for Soil Moisture',/ 
     1 '        Added Structure Summary (*.xss) output',/
     1 '        Revised Rio Grande State Summary for a spill',/
     1 '    - 2001/01/18 (9.68)',/
     1 '        Added Soil Moisture Storage by adding',/
     1 '        Soilm.f and soilcu.f and editing mdainp.f',/
     1 '        datinp.f, execut.f, return.f, & outmon.f',/
     1 '    - 2001/01/15 (9.67)',/
     1 '        Additional refinement regarding Sprinkler Use',/ 
     1 '    - 2001/01/12 (9.66)',/
     1 '        Additional refinement RGRG regarding EButte Spill',/
     1 '    - 2001/01/08 (9.65)',/
     1 '        Additional refinement to SJRIP',/
     1 '        Average Efficiency updated but checking to do')

 207    format(
     1 '    - 2000/12/27 (9.64)',/
     1 '        Added variable efficiency capability by editing',/
     1 '        common.inc add dcu, dcuw, diwrreq, diwrreqw,',/ 
     1 '        ieffmax, datinp to read ieffmax',/
     1 '        mdainp reads in iwr (independent of demand IWR)',/
     1 '        bomsec initialize diwrreq, dcu, diwrreqq, dcuw',//
     1 '        rtnsec added call return',/
     1 '        rtnsecw added call return',//
     1 '        divrig break out rtnmax and calls to rtnmax',/
     1 '        to maximize diversion with variable n',/
     1 '        welrig to include variable efficiency',/
     1 '        divcar, divcar1 to include variable efficiency',/
     1 '        Note carrpl, divres, dirrpl contiue use use',/ 
     1 '        average efficiency',// 
     1 '        outmon revised to print CU calculated in return',/
     1 '        dayoutr revised to print CU calculated in return',//
     1 '        vircom revised to include variable efficiency',/
     1 '        outtbl revised to print ave and max efficiency',/
     1 '    - 2000/11/24 (9.63)',/
     1 '        Added annual time series (*.tsp) file to limit',/
     1 '        pumping to acres served by GW & capacity by year',/ 
     1 '        Edits to datinp, mdaipn, bomsec, welrig & common',/
     1 '        Added basin total capability to well comparison',/ 
     1 '        (*.xwc) and diversion comparison (*.xdc) reports',/
     1 '        Edits to average.f, outwelc.f and outdivc.f',/
     1 '    - 2000/11/11 (9.62)',/
     1 '        SJRIP operating rule #20 develped, Created sjrip.f',/
     1 '        and oprinp.f, execut.f, common.inc to handel',/
     1 '        Revised datinp.f & mdainp.f to read a time seies ', 
     1 '        file',/
     1 '        Revised interp.f to check if a value is less than',/
     1 '        lowest table value',/ 
     1 '    - 2000/10/27 (9.61)',/
     1 '        Revised rgrg.f and mdainp.f & common.inc ',/
     1 '        to allow an E Butte spill at end of forecast file',/
     1 '        Added randfn.f to begin to allow random order of',/ 
     1 '        input files',/ 
     1 '        Revised oprinp.f & oprfind.f to check for a div ',/
     1 '        or res account = 0.  Code stops and warns',/
     1 '    - 2000/10/03 (9.60)',/
     1 '        Revised rgrg.f to fix Conejos annual Nov-Dec',/
     1 '    - 2000/10/03 (9.59)',/
     1 '        Revised dimension of reservoirs to 155 and ',/
     1 '        reservoir rights to 101 for gunnison and',/ 
     1 '        colorado linkage',/
     1 '    - 2000/08/17 (9.58)',/
     1 '        Revised datinp.f and mdainp.f to continue IWR',/
     1 '        capability. Added checks when eff = 0.',/
     1 '        Fixed datinp.f for 12 efficiency values for wells',/
     1 '        Added maxfn and began edits to change file names ',/
     1 '        from 72 to 256 (not done see for getcl & open)',/
     1 '        Revised daydist for a problem with daily gain calcs',/
     1 '        Revised mdainp.f, dayest.f & common.inc to use ',/
     1 '        virinpT (total flow) for daily stream calcs',/
     1 '    - 2000/07/11 (9.57)',/
     1 '        Revised virout.f and vircom.f to relax',/ 
     1 '        identification of missing data (-999), again.',/
     1 '        Continued edits to datinp.f, outtbl.f, mdainp.f to',/ 
     1 '        finish adding ability to read demand data as IWR',/
     1 '    - 2000/07/10 (9.56)',/
     1 '        Revised virout.f and vircom.f to relax',/ 
     1 '        identification of missing data -999)',
     1 '    - 2000/06/30 (9.55)',/
     1 '        Revised datinp.f, outtbl.f, mdainp.f to continue ',/
     1 '        adding ability to read demand data as IWR',/
     1 '    - 2000/06/22 (9.54)',/
     1 '        Revised dayoutr.f regarding structure ID printed',/
     1 '        Revised daydata.f to allow less data than stations',/
     1 ' ***    Note problems with daily run (ex8) not resolved',/
     1 ' ***    Fixed  8/17/00',/
     1 '    - 2000/06/22 (9.53)',/
     1 '        Revised outrev.f and outifr.f to include include',/
     1 '        revised number of values in *.xdd.',/
     1 '        Revised demcons.f to properly treat constrained ',/
     1 '        demand mistake associated with version 9.52',/
     1 '    - 2000/06/17 (9.52)',/
     1 '        Revised datinp.f mdainp.f, welrig.f, bomsec.f,', /
     1 '        demcons.f, outmon.f, dayoutr.f & common.inc to',/ 
     1 '        control source of GW data via a control variable',/
     1 '        idemtyp (aka icondem) and to allow',/ 
     1 '        IWR data to be read for demands',/
     1 '    - 2000/06/09 (9.51)',/
     1 '        Revised rgrg.f forecast is known in October',/
     1 '        and max surplus is 150000 per yr no cumulative limit',/
     1 '    - 2000/05/20 (9.50)',/
     1 '        Revised mdainp.f to not allow *.ddh and *.weh',/
     1 '        to be added when in baseflow mode',/
     1 '        Revised outbal2.f to add pumping to stream balance',/ 
     1 '        and addl notes to *.xgw',/
     1 '    - 2000/05/03 (9.49)',/
     1 '        Revised dimension in outcu.f and fixed problem',/
     1 '        with negative flows caused by well depletions',/
     1 '        not being downstream.  Key edits add roundof.f',/
     1 '        dnmfsow.f and add roundof.f to bomsec.f, execut.f',/
     1 '        divres.f and welrig.f',/
     1 '    - 2000/05/02 (9.48)',/
     1 '        Revised dimension for max number of gw returns',/
     1 '        (maxrtnw) 9002 = 20000 and delay (maxdly) 200=980',/  
     1 '    - 2000/04/24 (9.47)',/
     1 '        Revised dimension for max number of returns',/
     1 '        (maxrtnw) 3002 = 9002 and delay (maxdly) 200=980',/  
     1 '        Corrected I/O problem in outtbl.f (check option)')
 213      format(
     1 '    - 2000/04/13 (9.46)',/
     1 '        Revised oprfind.f & oprinp.f to allow idum = -8 ',/
     1 '        or -20 for rio grande compact operatin in GUI',/
     1 '        Revised datinp.f & mdainp.f to allow ireach = 2',/ 
     1 '        or 3 to control opening of *.ifm file',/ 
     1 '        Revised chekts.f, chekpor.f, chekin.f, & mdainp.f',/
     1 '        to allow a dummy file for *.ifm and other TS files',/
     1 '        Revised datinp.f and riginp.f and common to ',/
     1 '        include sw/gw primary switch',/
     1 '        Revised statem.f & chekpor.f to resolve baseflowx',/
     1 '        concerns with Yampa (included for90 updates missed)',/
     1 '        Revised bomsec.f to correct problem with From/To Gw',/
     1 '        Revised common.inc, bomsec.f, & datinp.f to include',/
     1 '        maximum recharge from stream',/
     1 '        Revised several *.xrg output descriptions',/
     1 '    - 2000/04/04 (9.45)',/
     1 '        Revised outtbl.f to print 50 rights max for wells',/
     1 '        Revised outtbl.f to correct well printout',/
     1 '        Revised report.f to open *.xwe file',/
     1 '        Revised datinp and mdainp to allow well data type 7',/
     1 '    - 2000/03/27 (9.44)',/
     1 '        Revised rgrg.f regardung Oct and Nov-Dec',/
     1 '        and outrg.f to clean up output descriptions',/
     1 '    - 2000/03/07 (9.43)',/
     1 '        Revised rgrg.f to separate 100k surplus limit',/
     1 '        Revised execut.f & bomsec.f to correct I/O to ',/
     1 '        rio grande output (*.xrg) when start yr .ne. 1',/
     1 '        Revised outrg.f to print Compact Summary',/
     1 '    - 2000/03/07 (9.42)',/
     1 '        Revised oprinp.f to correct an array problem',/
     1 '    - 2000/03/06 (9.41)',/
     1 '        Revised mdainp.f & report.f to allow baseflow',/
     1 '        and report options to work with monthly isf data',/
     1 '    - 2000/03/03 (9.40)',/
     1 '        Revised oprinp to call oprfind for opr types',/ 
     1 '        16, 17, 18 and 19 to simplify maintenance',/,
     1 '        and replaced rgco.f with rgrg.f (one code for',/
     1 '        both RG and Conejos compact calculations',/
     1 '    - 2000/02/23 (9.39)',/
     1 '        Revised demcons to print warning when demand exceeds',/
     1 '        sum of rights or capacity',/
     1 '        Also, revised xdebug to call demcons so check option',/
     1 '        does same',/
     1 '        Revised oprinp & execut to include type 19 operating',/
     1 '        rule for split channel and ',/
     1 '        Added Divcar2 for split channel',/
     1 '    - 2000/02/21 (9.38)',/
     1 '        Revised outtbl to include minor report edits',/
     1 '        Revised oprinp, execut and added divcar2 to ',/
     1 '        allow opr type 19; split channel',/)

 203   format(
     1 '    - 2000/02/11 (9.37)',/
     1 '        Revised outresc regarding column with eom data',/
     1 '    - 2000/02/08 (9.36)',/
     1 '        Revised maximum # of returns from 2216 to 3001',/
     1 '        Revised datinp to allow sum % return > 100 and',/
     1 '        mdainp to check sum % * sum return table , 100%',/
     1 '    - 2000/02/07 (9.35)',/
     1 '        Revised maximum # of delay tables from 49 to 200',/
     1 '        Revised DFS by a carrier in oprinp and directfs by',/
     1 '        requiring the source to be an operating rule',/
     1 '        Revised outmon regarding CU for carriers and ',/
     1 '        return flows with losses',/ 
     1 '        Revised outcu to print all structures related to ',/
     1 '        9.30 edits',/
     1 '    - 2000/01/29 (9.34)',/
     1 '        Fixed directfs for a problem related to bypass %',/
     1 '    - 2000/01/26 (9.33)',/
     1 '        Reduced I/O to *.log file',/
     1 '        and increased opr right read counter for comments',/
     1 '    - 2000/01/23 (9.32)',/
     1 '        Fixed a problem in outcu related to ver 9.30 edits',/
     1 '        (diversions with baseflows were not printed)',/
     1 '        Fixed a problem in outdivw related to ver 9.30 edits',/ 
     1 '        (water rights were not printed for diversions with',/ 
     1 '        baseflows)',/       
     1 '        Fixed divcar1 call to rtnsec; a problem related to',/ 
     1 '        adding loss calculations',/
     1 '    - 2000/01/23 (9.31)',/   
     1 '        Added Stop 0 or Stop 1 and eliminated iteration',/
     1 '        printout to screen',/
     1 '    - 1999/12/29 (9.30)',/
     1 '        Revised outdivw, daydivo, outmon, dayoutr &',/
     1 '        outdivc for a new ID numbering to allow delplt',/
     1 '        to work better',/
     1 '        Also revised Average, Outdivc, Outresc and',/
     1 '        Outwelc to print partial results when some data', /
     1 '        is missing',/
     1 '    - 1999/11/11 (9.29)',/
     1 '        Revised local dimension in outwr.f to 150'/
     1 '    - 1999/11/11 (9.28)',/
     1 '        Revised local dimension in outwr.f to 100'/
     1 '    - 1999/11/11 (9.27)',/
     1 '        Revised outpltr to include well data'/
     1 '    - 1999/11/08 (9.26)',/
     1 '        Revised common.inc to allow variable depl to handle',
     1        ' 20 years',/
     1 '    - 1999/11/05 (9.25)',/
     1 '        Revised outtbl.f to print opr rules 14-16',/
     1 '        Removed misc. log output (idumx) from oprinp.f',/ 
     1 '        Mdainp.f temporarily uses pfacto to weight delay data',/ 
     1 '    - 1999/10/16 (9.24)',/
     1 '        Revised oprinp.f & directfs.f to allow operating',/
     1 '        rule type 16 (DFS) to handle a water right that',/
     1 '        diverts through a carrier',/
     1 '    - 1999/10/16 (9.23)',/
     1 '        Revised oprinp.f to skip if # in column 1',/
     1 '        Revised mdainp.f & outtbl.f to not treat delay'/
     1 '        table ID as a counter',/
     1 '    - 1999/10/05 (9.22)',/
     1 '        Revised mdainp.f + outtbl.f to calculate loss',/
     1 '        when sum of delay table .ne. 100%',/
     1 '    - 1999/09/30 (9.21)',/
     1 '        Revised outtbl.f for problem when no wells',/
     1 '    - 1999/09/22 (9.20)',/
     1 '        Revised chekpor.f & chekts.f to handle blank files',/
     1 '    - 1999/09/17 (9.19)',/
     1 '        Revised ouflow.f & outtbl.f to print correctly',/
     1 '        when the gaged data is missing',/
     1 '    - 1999/09/14 (9.18)',/
     1 '        Revised execut.f, oprinp.f & added rg.f',/
     1 '        to inlcue Rio Grande Compact',/
     1 '    - 1999/08/26 (9.17)',/
     1 '        Revised datinp.f, mdainp.f, getrtnw.f & common.inc',/
     1 '        to allow a character ID for return tables in the',/
     1 '        delay file (*.dly), the diversion station file',/
     1 '        (*.dds) and the well staion file (*.wes)',/
     1 '    - 1999/08/20 (9.16)',/
     1 '        Revised chekpor call in xdebug.f, dayest.f, datinp.f',/
     1 '        mdainp.f, virgen.f, report.f & common.inc to',/
     1 '        allow daily data to be skipped at top of file',/
     1 '    - 1999/08/10 (9.15)',/
     1 '        Revised Opr Rule 14 to allow an annual diversion',/
     1 '        limit by revising divcar1.f, bomsec.f, oprinp.f, & ',/
     1 '        common.inc.',/
     1 '        Fixed a debug printout in Divrig.f',/
     1 '        Fixed a local dimension problem in outwr.f',/)

 204   format(
     1 '    - 1999/07/19 (9.14)',/
     1 '        Revised outdivc.f to print correct average',/
     1 '        when the gaged data is missing',/
     1 '    - 1999/07/16 (9.13)',/
     1 '        Revised average.f to print total and average',/
     1 '        simulated results when the gaged data is missing',/
     1 '    - 1999/07/06 (9.12)',/
     1 '        Enhanced Opr rule 15; interruptable supply',/
     1 '        common.inc changed dumsta to a real & added demopr',/
     1 '        and idum',/
     1 '        datinp.f & outdivc.f replaced dumsta with idum',/
     1 '        bomsec.f revised the on/off switch',/
     1 '        oprinp.f revised the on/off switch',/
     1 '        intersup.f revised miscellaneous',/
     1 '    - 1999/06/29 (9.11)'/
     1 '        Finished Opr rule 15; interruptable supply',/
     1 '    - 1999/06/28 (9.10)'/
     1 '        Revised ouropr.f for Report POR .ne. Simulatino POR',/
     1 '    - 1999/06/25 (9.09)'/
     1 '        Began interruptable supply by editing outmon.f',/
     1 '        execut.f, oprinp.f, and adding intersup.f',/
     1 '    - 1999/06/23 (9.08)',/
     1 '        Revised outdivc for Reports when the',/
     1 '        Report POR .ne. Simulation POR',/
     1 '        and outdivc.f and average.f for missing (-999) data',/
     1 '    - 1999/06/22 (9.07)',/
     1 '        Reduced convergence criteria in divres.f',/
     1 '        Added additional negative checks to virnod & vircom',/
     1 '    - 1999/06/15 (9.06)',/
     1 '        Revised getpath.f and getpath.f to check filename',/,
     1 '        size is >0 and <=8 characters',/
     1 '    - 1999/05/15 (9.05)',/
     1 '        Added getpath.f and getpath.f to allow execution ',/
     1 '        from another directory by StateMod GUI, etc.',/
     1 '        Also, added check to mdainp for target data when ',/
     1 '        simulating 1 year',/
     1 '        Added chekres.for to correct a round off problem',/
     1 '        related to reservoir and account total volumes',/
     1 '    - 99/05/06 (9.04) ',/
     1 '        Added ground water balance (*.xgw) ',/
     1 '        when simulating wells',/
     1 '    - 99/03/22 (9.03) ',/
     1 '        Added year on or off capability to direct, instream',/
     1 '        power, reservoir and well water rights',/
     1 '    - 99/02/10 (9.02) ',/
     1 '        Added futile call capability',/
     1 '    - 98/11/18 (9.01) ',/
     1 '        Revised to include Wells',/
     1 '        Includes new *.xdd, etc')
 
 
 205   format(
     1 '    - 98/11/09 (8.11) ',/
     1 '        Revised to include option BaseflowX',/
     1 '        that allows natural flows at gages to be read',/
     1 '        and ungaged natural flows to be calculatd',/
     1 '    - 98/10/07 (8.10) ',/
     1 '        Revised execut.f to print iteration information',/
     1 '    - 98/09/15 (8.09) ',/
     1 '        Revised datinp.f to not allow a *.dds file to',/
     1 '          not have a river location specified',/
     1 '    - 98/08/10 (8.08) ',/
     1 '        Revised fivres.f, divrpl.f & divrig.f to improve ',/
     1 '          convergency properties related to variable ishort',/
     1 '        Revised ifrrig, ifrrig2, resrg1, powres, powres2',/,
     1 '          resrp1, rsrspu, ifrrigx, divcar, divcar1, carrpl',/
     1 '          resoop, & powsea to include variable small=0.001',/
     1 '    - 98/06/19 (8.07) ',/
     1 '        Removed the temporary fix of the dimension of intern',/
     1 '        Revised divres.for all refrences to l2 = lr',/
     1 '        Revised divrpl.f all refrences to l2 = lr',/
     1 '        Revised execut.f to remove last l2 refrence ',/
     1 '        in call divres and call divrpl',/
     1 '        Revised replace.f to remove last l2 refrence ',/
     1 '        and pass lr in the l2 position ',/
     1 '        in call divres and call divrpl',/
     1 '    - 98/06/13 (8.06) ',/
     1 '        Revised evasec.f to fix a problem',/
     1 '        related to making the model operate daily',/,
     1 '        Revised mdainp.f to allow a study period to begin',/
     1 '        later than the data provided in diversions',/
     1 '        Revised vircom.f to print the correct year to *.xbi',/
     1 '        Temporarily revised the dimension of intern ',/
     1 '    - 05/05/98 (8.05) Revised datinp to allow ',/
     1 '        daily data types -1 and 0',/
     1 '    - 05/05/98 (8.03) Revised rtnsec to allow ',/
     1 '        baseflows after daily capability was added',/
     1 '    - 03/17/98 (8.02) Revised daily capability to include',/
     1 '        daily return data',/
     1 '    - 03/09/98 (8.01) Preliminary Release Daily capability',/
     1 '    - 03/16/98 (7.27) Revised rsrsup to include a reservoir',/
     1 '        to reservoir transfer limit via the *.ddm file',/
     1 '        per Bethel email 3/16/98',/
     1 '    - 01/23/98 (7.26) ',/
     1 '      Revised oprinp.f to require type 11 and 14 opr rights ',/
     1 '        to have a water right (not a structure) as a source',/
     1 '      Revised oprinp.f to print FYI on the admin number ',/
     1 '        used when the source is a water right',/
     1 '      Revised outcu.f note regarding CU calculation comment',/
     1 '      Revised riginp.f to print FYI regarding reservoir right',/
     1 '        and first fill warning',/
     1 '    - 12/22/97 (7.25) ',/
     1 '      Revised oprinp.f to check reservoir source account data',/
     1 '    - 11/08/97 (7.24) ',/
     1 '      Enhanced reads to include err= and end= in:',/
     1 '        datinp.f, riginp.f, oprinp.f, mdainp.f, chekpor.f',/
     1 '        chekts.f, skipn.f, skip.f, outdivc.f, outresc.f,',/
     1 '        outriv.f, ouflow.f, and dattim.f',/
     1 '      Enhanced statem.f, execut.f, virgen.f, report.f ',/
     1 '        and xdebug.f regarding the banner print and ',/
     1 '        making write(99 write(io99 for checking purposes')
c206   format(
c    1 '    - 10/29/97 (7.23) '
c    1 '      Revised dimension of reprnk in common.inc to ',/
c    1 '        handle 6 replacement reservoirs',/
c    1 '    - 10/22/97 (7.22) ',/
c    1 '      Revised statem.f, common.inc & repsort.f to handle',/
c    1          ' six replacement reservoirs',/
c    1 '      Revised vircom.f to correct average printout related ',/
c    1 '        to missing data (-999) on reservoir delta storage',/
c    1 '    - 10/17/97 (7.21) ',/
c    1 '      Revised oprinp.f & resrpl fot water right constraint',/
c    1 '        on operating rule per Bethel 10/10/97 Email',/
c    1 '      Revised repsort.f for error if multiple replacement ',/
c    1 '        reservoirs & real*8 per Bethel 10/13 & 10/20/97 Email',/
c    1 '      Revised execut.f, datinp.f & common.inc to allow ',/
c    1 '        detailed calling information',/
c    1 '      Revised divres to initialize relact per Bethel 10/15 ',
c    1          'Email',/
c    1 '      Revised divrpl and divres to check for 0.01 befor',
c    1          'Call Takout per Bethel 10/17 Email',/
c    1 '      Revised divrig to include small and regarding ishort',/
c    1 '    - 09/29/97 (7.20) ',/
c    1 '      Revised vircom.f & virout.f to allow baseflow ',/
c    1 '        calculations if a gage is missing data (-999)',/
c    1 '      Revised mdainp.f to stop & warn if missing data (-999)',/
c    1 '      Revised outtop.f & virout.f to print baseflow ',/
c    1 '        data at gages only to *.xbg',/
c    1 '    - 09/24/97 (7.19) ',/
c    1 '      Revised oprinp.f & powsea.f to allow a ',/
c    1 '      target release to 1 or all accounts',/)
c     1 '    - 07/08/97 (7.18) ',/
c     1 '      Revised outcu.f, outtop.f & report.f to',/
c     1 '      include total by Water District (*.xwd) output',/
c     1 '    - 06/30/97 (7.17) ',/
c     1 '      Revised rsrspu.for per Bethel 6/30/97 to',/
c     1 '        limit bookover transfers',/
c     1 '    - 06/15/97 (7.16) ',/
c     1 '      Revised per Bethel 06/97 the following 3 items',/
c     1 '              1. resrg1.f to remove prior filling ',/
c     1 '                 constraing on OOP storage (Dillon)',/
c     1 '              2. oprinp.f, execut.f & added divcar1.f',/
c     1 '                 to provide Rule 14 which includes ',/
c     1 '                 a demand constraint (Windy Gap)',/
c     1 '              3. oprinp.f and divrpl.f to allow',/
c     1 '                 replacement of a water right in ',/
c     1 '                  addition to a structure',/
c     1 '      Revised per Bennett 06/97 for Fortran 90 ',/
c     1 '              1. formats in outbal.f, outtbl.f, ',/
c     1 '                 outsyt.f and bomsec.f',/
c     1 '              2. common.for = common.inc',/
c     1 '      Revised per Bennett 06/97 for GUI error handling ',/
c     1 '              1. outpltd.f, outpltr.f, report.f',/
c     1 '                 statem.f, virgen.f',/
c     1 '      Revised per Bennett 06/97 for Available Flow ',
c     1                  'output',/
c     1 '              1. outmon.f and outdiv.f',/
c     1 '    - 04/23/97 (7.15) ',/
c     1 '      Revised forcast in bomsec.for, mdainp.f &',
c     1 '    - 04/23/97 (7.15) ',/
c     1 '      Revised forcast in bomsec.for, mdainp.f &',
c     1 '      Revised forcast in bomsec.for, mdainp.f &',
c     1              ' common.for',/
c     1 '      Revised *.out error messages in getin.f',/
c     1 '      Revised outdeb.f to print flow gages as default',/
c     1 '      Revised outifr.f and outmon.f regardind numifr = 0',/
c     1 '      Revised divrpl.f and divres.f re Bethel comments',
c     1              ' related to repact vs divact',/
c     1 '      Revised oprint.f float = ifix',/
c     1 '      Revised common.for, added integer dumsta',/
c     1 '      Revised formats regarding ,) in datinp.f, outbal.f,',
c     1              ' outtbl.f and outsyt.f',/
c     1 '      Revised bomsec.f regarding if(rdate(nr).eq.mon =',
c     1                                 ' if(ifix(...',/
c     1 '    - 05/21/97 (7.14) ',/
c     1 '      Revised evasec.f to better handle roundoff',/
c     1 '    - 02/10/97 (7.13) ',/
c     1 '      Revised powsea.f & oprinp.f to allow ',/
c     1 '      Target release from all accounts',/
c     1 '    - 01/10/97 (7.12) ',/)
c    1 '      Revised outcu.f to print average annual cu',/
c    1 '    - 01/08/97 (7.11) ',/
c    1 '      Revised outtbl.f to print comma separated data',/
c    1 '    - 11/05/96 (7.10) ',/
c    1 '      Revised evasec.f regarding water balance evap',/
c    1 '      Revised outbal.f to add cu',/
c    1 '    - 10/28/96 (7.09) ',/
c    1 '      Revised graph files (*.xdg, *.xrg) to ',
c    1        ' include year',/
c    1 '    - 10/11/96 (7.08) ',/
c    1 '      Revised outpltr.f to handle different ',
c    1        ' reservoir types',/
c    1 '    - 10/08/96 (7.07) ',/
c    1 '      Revised dimensions of rigsor.f, somnmy.f',
c    1        ' and statem for linked model',/
c    1 '      Removed a check in oprinp.f',/
c    1 '    - 10/07/96 (7.06) ',/
c    1 '      Revised demsrc.f, statem.f and commnon.for',
c    1        ' to include larger dimensions per linked model',/
c    1 '    - 10/02/96 (7.05) ',/
c    1 '      Revised datinp.f to check returns to',
c    1        ' diverting node',/
c    1 '      Revised bomsec.f re one fill initilization',/,
c    1 '    - 08/27/96 (7.04) ',/
c    1 '      Revised outres.f re reservoir owners ',/,
c    1 '      Revised datinp.f, outsp.f, report.f,',   
c    1        ' virgen.f, xdebug.f, execut.f, outrep.f',/,
c    1 '        regarding time & unit control on *.xsp',/,
c    1 '    - 08/16/96 (7.03) ',/
c    1 '      Revised outres.f, outpltr.f & outsp.f ',
c    1        ' to fix reservoir print problems',/  
c    1 '      Revised outdeb.f to print non structure ',
c    1        ' stations (OTH)',/
c    1 '      Renamed outite.f to outsp.f for clarity',/
c    1 '      Revised log messages from getpar.f,',
c    1        ' getid.f and getin.f',/
c    1 '    - 08/14/96 (7.02) ',/
c    1 '      Revised datinp.f to check instream reach ',
c    1        ' overlap',/
c    1 '      Revised execut.f to call ifrrig2.f if ireach = 1',/
c    1 '    - 08/12/96 (7.01) ',/
c    1 '      Revised datinp.f to read ireach, code for ',/      
c    1 '        0= Phase II without instream flow reach',/
c    1 '        1= Phase III with instream flow reach',/
c    1 '      Added ifrrig2.f and powres2.f for instream reach',/
c    1 '      Revised common.for for instream reach',/
c    1 '      Added outifr to print instream reach',
c    1        ' reach details',/
c    1 '      Revised outdiv.f and outres.f to print a',
c    1        ' requested id',/
c    1 '      Revised report.f and execut.f to read a file of ',
c    1        ' requested ids to print',/
c    1 '      Broke outdiv.f into outdiv.f and outpltd.f to',
c    1        ' simplify plotting and use binary data',/,
c    1 '      Broke outres.f into outres.f and outpltr.f to',
c    1        ' simplify plotting and use binary data',/) 
c    1 '    - 07/31/96 (6.04) ',/
c    1 '    - 07/31/96 (6.04) ',/
c    1 '      Revised common.for dimension of reservoir rights',/
c    1 '    - 07/19/96 (6.03) ',/
c    1 '      Revised carrpl.f regarding reservoir destination',/
c    1 '    - 07/03/96 (6.02) ',/
c    1 '      Revised datinp.f to check network',/ 
c    1 '      Revised carrpl.f re destination is a ',
c    1        ' reservoir',/
c    1 '      Revised oprinp.f for rights turned off',
c    1        ' but part of an exchange',/
c    1 '      Revised vircom.f and virgen.f to use ',
c    1        ' a scratch file',/
c    1 '      Revised report.f, outdivc.f and outresc.f',
c    1        ' to use a scratch file',/
c    1        /
c    1 '    - 06/19/96 (6.01)',/ 
c    1 '      Revised *.f to be more generic with',
c    1        ' regard to dimensions',/
c    1 '      Revised average.f, accou.f, rigsor.f',
c    1        ' somnmy.f, vircom.f and demcons.f regarding',
c    1        ' local dimensions',/
c    1 '      Broke riginp.f into riginp.f and oprinp.f',/
c    1 '      Added special parameter output (-xsp) and ',
c    1        ' associated input file of parameters (*.out)'/
c    1 '      Revised binary output to include dimension ',
c    1        ' info for delplt to run on binary',/
c    1        /
c    1 '    - 06/06/96 (5.33) ',/
c    1 '      Revised mdainp, ... re return flow  table id(s)',/         
c    1 '                      Broke riginp.f into riginp.f & oprinp.f',/
c    1 '                      Revised open(  re error if not found',/
c    1 '                      Revised execut.f, report.f, out*.f',
c    1                      ' re additional info to binary files',/
c    1 '    - 05/28/96 (5.32) Revised parsgi.f & report for structure',
c    1                      ' list (*.xdl) output',/
c    1 '    - 05/28/96 (5.31) Revised divrpl.f, & divres.f regarding',/
c    1 '                      variable l2 in reoperation mode calls ',
c    1                      ' to rtnsec.f',/
c    1 '                      Revised divres.f re calls to rtnsec.f',/
c    1 '                      Revised common.for re variable irtnid ',
c    1        ' to allow the return Id to be a character in future',/
c    1 '    - 05/20/96 (5.30) Revised evasec.f for roundoff concerns',/
c    1 '                      Revised execut.f & rtnsec.f for',
c    1 '                      reiteration consistency',/
c    1 '                      Revised replace.f for opr right off',/
c    1 '    - 05/16/96 (5.29) Revised evasec.f for roundoff concerns',/
c    1 '                      Revised carrpl.f re reservoirs (amax1)',/
c    1 '                      Revised resrpl.f re accr(9            ',/
c    1 '                      Revised execut.f & rtnsec.f regarding',
c    1                      ' reoperation',/
c    1 '    - 05/16/96 (5.28) Revised carrpl.f to handle reservoirs',/
c    1 '    - 05/15/96 (5.27) Revised divres.f to include amax1',/
c    1 '                      Revised bomsec.f re remaining decree',/
c    1 '                      Revised getrep.f re multiple rep res',/
c    1 '                      Revised common.for re real*8 for above',/
c    1 '                      Revised evasec.f re too much rain',/
c    1 '                      Revised datinp.f to check initial cont',/
c    1 '                      Minor cosmetic changes to outres.f',
c    1 '    - 05/10/96 (5.26) Revised evasec.f in baseflow mode',/
c    1 '    - 05/08/96 (5.25) Revised outdiv.f to flush',/     
c    1 '    - 05/07/96 (5.24) Revised evasec.f & datinp.f to ',
c    1                      ' allow precip by %, storage proration or off',/
c    1 '    - 05/06/96 (5.23) Revised outdiv.f & outres.f to print ',
c    1                      ' header every year',/
c    1 '                      Removed detailed prints from misc.',/
c    1 '    - 05/01/96 (5.22) Added bookover constraint to '
c    1                      ' replacement reservoir logic',/
c    1 '                      Minor cleanup to outdeb & resrg1.f')
c
c    1 '    - 04/29/96 (5.21) Revised riginp.f for type 2 to', 
c    1                      ' reservoirs',/
c    1 '    - 04/24/96 (5.20) Revised riginp.f and divres.f',
c    1         ' for type 2 to reservoirs',/
c    1 '    - 04/22/96 (5.19) Cleaned up outtbl.f',/
c    1 '                      Revised divres.f and datinp.f to allow',
c    1         ' both monthly and intervening structure control',/
c    1 '                      Revised ifrrig.f, datinp.f, common.for',
c    1         ' takou2.f, & outdiv.f to allow an instream flow reach'/
c    1 '                      Revised execut.f, datinp.f & common.for',
c    1         ' to allow user control of reoperation (ireopx)',/
c    1 '    - 04/11/96 (5.18) Cleaned up bomsec.f and outtbl.f',/
c    1 '    - 04/10/96 (5.17) Added Out Of Priority reservoir',
c    1         ' storage code to implement Blue River Decree',/
c    1 '                      Added resOOP.f',/
c    1 '                      Revised bomsec.f, riginp.f, resrg1.f,',
c    1         ' execut.f and common.for',/
c    1 '                      Cleaned up resrpl.f',/ 
c    1 '    - 04/09/96 (5.16) Revised dimension in bomsec.f',/,
c    1 '    - 04/08/96 (5.15) Added Reservoir Paper Fill (iressw=3)',
c    1        ' to Resrg1.f',/
c    1 '                      Revised tape43, 44 & 45 to *.b43, etc.',/
c    1 '                      Revised bomsec.f & outmon.f to print',
c    1        ' remaining reservoir water right (ritrem)',/
c    1 '                      Revised outres.f to include more',/
c    1 '                      Revised outopr.f to include more',/
c    1 '                      Revised outtbl.f to include more',/
c    1 '                      Revised outbal.f to print af not kaf',)
c
c     1 '    - 03/25/96 (5.14) Revised outtbl.f to print operation',
c     1        ' informatin',/
c     1 '                      Revised common.for, outcui.f, outdivc.f,',
c     1        ' outresc.f to use memory better',/ 
c     1 '    - 03/22/96 (5.13) Revised riginp.f for type 2 Carrier',/
c     1 '    - 03/22/96 (5.12) Revised data check in Riginp.f',/ 
c     1 '    - 03/20/96 (5.11) Added *.xop to standard report',/
c     1 '    - 03/19/96 (5.10) Added reservoir evap printout (*.xev)',/
c     1 '               Added variable return pattern option',/
c     1 '    - 03/14/96 (5.09) Added operational printout (*.opr)',/
c     1 '               Target release output is now as a spill',/
c     1 '               Fixed type 3 output by account',/
c     1 '               Adjusted roundoff in riginp.f',/
c     1 '               Added operational rights on/off over time',/
c     1 '    - 03/13/96 (5.08) Revised general replacement reservoir',/
c     1 '                 (10) to handle multiple reservoirs',/
c     1 '               Added operational right output (*.xop)',/
c     1 '               Revised datinp.f to handle up to 60 returns',/
c     1 '               Revised outdeb.f when opr rights are off',/
c     1 '    - 03/07/96 (5.07) fixed bookover problem in Rsrspu.f',/
c     1 '               Added additional checks in Riginp.f'/
c     1 '               Added carrier by exchange (7) via Carrpl.f'/
c     1 '               Added general replacement (10) via Replace.f',/
c     1 '               Added exchange by 100% or consumption in ',/
c     1 '                 Divres.for and divrpl.for',/
c     1 '    - 03/01/96 (5.06) Upgrade reservoir dimension from',/
c     1 '                 40 to 50 again and fixed *.rim header',/
c     1 '    - 02/15/96 (5.05) Upgrade reservoir dimension from',
c     1        ' 40 to 50',/
c     1 '               Added opr type 5, res to res exchange',/,
c     1 '               Added opr type 13, instream flow based on',
c     1        ' an index stream flow',/,
c     1 '               Began opr type 7, res to carrier exchange',/
c     1 '               Began to include returns into type 4 res to',
c     1        ' diversion exchange',/         
c     1 '    - 02/15/96 (5.04) Upgrade river dimension from 650 to 750',/
c     1 '    - 02/12/96 (5.03) Revise instreams ifrrig.f & execut.f',/
c     1 '               Enhanced riginp.f error messages,'/
c     1 '               Added index flow per ifrrigx.f and execut.f',/    
c     1 '    - 02/06/96 (5.02) Revise rsrspu.f regarding bookover',/
c     1 '    - 01/29/96 (5.01) Revise AF/M to ACFT in outtop.f',
c     1        ' & chekts.f',/,
c     1 '               Report.f, riginp.f',
c     1        ' and datinp.f - Revised data warnings',/
c     1 '               Demcons.f - provide demand option on instream'
c     1        ' flows',/  
c     1 '    - 01/17/96 (5.00) Misc refinements'/,
c     1 '               rtnsec.f, execute.f & common.for - automatic',
c     1        ' reoperation for non downstream returns',/,
c     1 '               outres.f & outmon - print annual total',
c     1        ' and consisent reservoir output',/
c     1 '               outcu.f - prints % supplied, short',/
c     1 '               rsrsup.f - additional checks for negatives',/
c     1 '               divcar.f & divres - revised check to .01 cfs',/
c     1 '               interp.f - improved round off treatment',/       
c     1 '    - 01/02/96 (4.04) Misc refinements to bomsec.f ',
c     1        ' outtbl.f, outcu.f, outwr.f, outtop.f, datinp.f',/
c     1 '               vircom.f, outdeb.f, virout.f, riginp.f,',
c     1        ' divcar.f and outmon.f')
c
c     1 '    - 12/22/95 (4.03) Corrected dimension problems in',
c     1        ' outmon.f & riginp.f',/
c     1 '               Enhanced error messages in riginp.f and',
c     1        ' mdainp.f',/
c     1 '    - 12/19/95 (4.02) Recompiled execut.f to allow revisions',
c     1        ' under version 4.01 to work',/
c     1 '    - 12/15/95 (4.01) Revised CU report (-xcu) to be',
c     1        ' actual CU and added supply report (-xsu)',/
c     1 '               Added additional info regarding base flows ',
c     1        'to the *.log file',/
c     1 '               Revised reporting to operate for',
c     1        ' years that differ from the simulation POR',/
c     1 '               Fixed baseflows to be a function of up to ',
c     1        '26 gages',/,
c     1 '               Enhanced *.dds printout to include river ',
c     1        'and *.xre to include river name and reservoir account',/
c     1 '               Added additional warnings regarding ',
c     1        'negative streamflows',/
c     1 '    - 11/29/95 (4.00) Revised base flow operations to',
c     1        ' operate as total flow or gains, allow negative',
c     1        ' diversions to be imported water and negative',
c     1        ' results (gains)',/,
c     1 '               Added a shortage summary output (-xsh)',/,
c     1 '               Enhanced',
c     1        ' the header printed to the cu output (-xcu)',/,
c     1 '               Added an error message if the Report',
c     1        ' Option Period',
c     1        ' of Record exceeds the binary file size',/,
c     1 '               Print annual total to diversion results (xdd)',/,       
c     1 '    - 11/15/95 (3.01) Renamed *.bug to *.log.  Revised',
c     1        ' baseflows to operate as gains or total flow',/
c     1 '    - 11/13/95 (3.00) Revised divres.f, powres.f & execut.f',
c     1        ' regarding instream flows during reoperation',/
c     1 '    - 11/07/95 (2.32) Revised vircom.f & mdainp.f to allow',
c     1        ' negative diversions to equal imports',/
c     1 '    - 10/31/95 (2.31) Revised dimension for base flows, and',
c     1        ' enhanced printout to vircom.f (*.xbi)',
c     1        ' & outtbl.f (*.xtb)',/
c     1 '    - 10/27/95 (2.30) Added stream name to *.xbi and check ',
c     1 '      for negative or zero water rights',/
c     1 '    - 10/23/95 (2.29) Revised evasec to allow negative evap',/,
c     1 '    - 10/23/95 (2.28) Additional enhancement to tabular',
c     1        ' printout,',/
c     1 '          split ouflow.f into ouflow.f and outtbl.f',/,
c     1 '          revised mdainp.f to allow *.eom to contain only max',/,
c     1 '          revised datinp.f to.print warning if no evap id',/,
c     1 '    - 10/18/95 (2.27) Revised ouflow.f to print tabular'
c     1        ' summary of input data',/
c     1 '    - 10/10/95 (2.26) Cleaned up questions to screen if not',
c     1        ' in a command line option',/
c     1 '    - 10/05/95 Revised virgen.f to operate w/o reservoirs',/
c     1 '    - 9/11/95 Revised outriv.for to print return flow data',/
c     1 '    - 9/06/95 Redimensioned from 400 to 650 stations in',
c     1 '      subroutines common.for, statem.f, demcons, mdainp.f',
c     1 '      ouflow.f, outmon.f, outriv.f, and vircom.f',/
c     1 '    - 9/01/95 Added chekpor to simplify mdainp.f, virgen.f',
c     1        ' and report.f',/
c     1 '    - 9/01/95 Revised vircom.f to print cal year properly',/
c     1 '    - 8/22/95 Enhanced the river statin output ',/
c     1 '    - 8/17/95 Enabled the monthly detailed node ',
c     1        'accounting (*.xnm)',/
c     1 '    - 8/17/95 Cleaned up version and date prints in statba.f',/
c     1 '    - 8/01/95 Enhanced the -help option ',/
c     1 '    - 8/01/95 Revised open files in mdainp to allow baseflow ',
c     1        'and simulate to operate with same response file'/
c     1 '    - 8/01/95 Enhanced the reoperation initilization in ',
c     1        ' execut.for and powres.for',/
c     1 '    - 8/01/95 Enhanced the stream and diversion comparison in',
c     1        ' outdivc.for',/
c     1 '    - 7/03/95 Fixed stop message when a bad id is given',/ 
c     1 '    - 7/03/95 Added stop if div. eff is more than 100%',/
c     1 '    - 6/26/95 Fixed a problem with baseflow module related',
c     1        ' to isgi not in a calling subroutine',/
c     1 '    - 6/20/95 Code will not prompt user for bad id data ',
c     1        'for plots',/
c     1 '    - 6/20/95 Added -simulatex option to not call standard',
c     1        'reports',/ 
c     1 '    - 6/16/95 Fixed problem with base flow evap',/
c     1 '    - 6/16/95 Allow -xsc output option',/
c     1 '    - 6/16/95 Revised -version output',/
c     1 '    - 6/16/95 Added -update output for recent updates',/
c     1 '    - 6/15/95 Fixed demcons, datinp, & common to allow',
c     1        ' unconstrained demand output'/
c     1 '    - 6/12/95 Allow, but warn user if annual diversions are',
c     1        ' provided for a base flow run',/
c     1 '    - 6/12/95 Added notes to stop 0=normal, 1=problem',
c     1        ' with data, 2=problem with run time',/
c     1 '    - 6/12/95 Revised base flow to not use river station',
c     1        ' file (*.ris), but get historic id from time series',
c     1        ' file (*.rih)',/
c     1 '    - 6/12/95 Added time series header to baseflow output',/
c     1 '    - 6/12/95 Revised time series header to cu output',/
c     1 '    - 6/07/95 Added a special river output for GUI',/
c     1 '    - 6/06/95 Revised divcar and resrg1 for carrier/account',
c     1        ' distribution concern'/,
c     1 '    - 6/04/95 Revised admin number to be real*8',/
c     1 '    - 5/31/95 Revised the comparison report for stream gages',/
c     1 '    - 5/31/95 Added a -help (-h) to the command menu',/
c     1 '    - Revised base flow formula',/
c     1 '    - Actual diversion report for Consumptive Use model -xcu',/
c     1 '    - Command line arguments accept short or long names',
c     1         ' e.g. base or baseflow',/c
c     1 '    - Command line arguments implemented',/
c     1 '    - Comparison routines for diversions, reservoirs and',
c     1          ' streamflows',/
c     1 '    - Output dates reflect true year for water year output',/
c     1 '    - Additional data checking that require the last entry',/
c     1 '        of the river network file contain a blank',/
c     1 '        downstream location',/
c     1 '    - Output files have been renamed for scenario mgt')

 300  format(/,72('_'),/
     1 '  StateM; Warning an old Irrigation Practice File (*.ipy)',/
     1 '          (IrrigationPractice_Yearly) has been provided.',/
     1 '          To avoid confusion StateMod version 12.01 and',/
     1 '          greater requires the user specify if they want',/
     1 '          to provide 2 or 4 land use types.',/
     1 '          Recommend you revise your response file (*.rsp) to:',/
     1 '          IrrigationPractice_2Types_Yearly or',/
     1 '          IrrigationPractice_4Types_Yearly')

 310  format(/,72('_'),/
     1 '  StateM; Warning Two Irrigation Practice Files (*.ipy)',/
     1 '          have been provided as follows:',/
     1 '          IrrigatonPractice_2Types_Yearly and',/
     1 '          IrrigatonPractice_4Types_Yearly',/
     1 '          Recommend you revise your response file (*.rsp)',/
     1 '          and remove one ipy file')

 320  format(/,72('_'),/
     1 '  StateM; Warning number of water rights for all structures',/
     1 '          (reservoir, diversion, ISF, wells & Operating',/
     1 '          rules) ', i5, ' Exceeds the maximum ', i5,/
     1 '          Recommend you revise StateM.for and the common.inc')
 330  format(/,
     1 '  StateM; Dimension Problem',/
     1 '    The maximum years in a delay table (MAXdlA) = ', i5,' and',/
     1 '    the maximum months in a delay table (MAXdlM) = ', i5,/
     1 '    MAXdlM should be MAXdla (years) * 12 (months per yr) = ',i5,
     1      ' not ', i5,/
     1 '    Suggest you revise Statem.f and Common.inc')
 340  format(/,
     1 '  StateM; Dimension Problem',/
     1 '    The maximum years in a delay table (MAXdlA) = ', i5,' and',/
     1 '    the maximum days in a delay table (MAXdlD)  = ', i5,/
     1 '    MAXdlD should be MAXdla (years) * 366 (days per) = ',i5,
     1      ' not ', i5,/
     1 '    Suggest you revise Statem.f and Common.inc')
       
C
 9997 write(6,9998) filena
      write(nlog,9998) filena
 9998 format('  Statem; Problem opening file: ', a72)
      goto 9999

 9999 write(6,*)  '  Stopped in Statem, see statem.log or *.log'
      write(nlog,*) '  Stopped in Statem'
      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      END

