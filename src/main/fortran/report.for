c
c *********************************************************
C
      SUBROUTINE REPORT(igui, istop, ioptio2, getid, nreach)
c
c
c _________________________________________________________
c	Program Description
c
c       Report; It controls all StateMod reporting
c
c _________________________________________________________
c	Documentation
c
c  Type     Report
c     1   ' Base flow information at stream gauge locations  ',
c     2   ' Detailed node accounting                         ',
c     3   ' Water Budget                                     ',
c     4   ' Water rights list sorted by basin rank           ',
c     5   ' Water Supply Summary                             ',
c     6   ' Reservoir Graph                                  ',
c     7   ' Diversion Graph                                  ',
c     8   ' Reservoir Comparison                             ',
c     9   ' Diversion Comparison                             ',
c    10   ' Consumptive Use Summary for the CU model or other',
c    11   ' River data Summary                               ',
c    12   ' Stream Comparison                                ',
c    13   ' Standard diversion (*.xdd) and reservoir (*.xre) ',
c    14   ' Shortage Summary                                 ',
c    15   ' Structure List                                   ',
c    16   ' Selected Parameter                               ',
c    17   ' Well Graph                                       ',
c    18   ' Well Comparison                                  ',
c    19   ' Daily Selected Parameter                         ',
c    20   ' Plan                                             ',
c    21   ' Plan                                             ',
c    22   ' Well Structure                                   ',
c    23   ' Aug plan to Well Structures (*.xpw)              ', 
c    24   ' Reach Report (*.xrh)                             ',
c _________________________________________________________
c       Update History
c
c rrb 02/05/07; Added call year to get year information
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
      character cplot*12, blank*12, getid*12, ftype*5, cRch*4
      dimension idatx(3), itim1(4), itim2(4)
      character filenO*256, filenO2*256, filenOX*256, rec32*32,
     1 filena1*256
c
c _________________________________________________________
c		Step 1; Initilze
c
c      
c      
      iout=0
      maxrpt=24
      small=0.001
      filenO='NA'
      filenO2='NA'
c
c _________________________________________________________
c rrb 20100208; Simplify by requiring random file input
c		(infile=1)
      if(infile.eq.0) goto 245
c
c _________________________________________________________
c
c               Step 1; Print title and get time
      write(nlog,100) ioptio, ioptio2
c
      call dattim(idatx, itim1, isgi)
      blank = '            '
      IIN=20
c
      call namext(maxfn, filenc, 'rsp', filena) 
      open(iin,file=filena,status='old',err=230)
      call skipn(iin)                                  
c
c _________________________________________________________
c
c               Step 2; Get option type
c rrb 01/08/19; Moved the following to top before call to datinp
c
  130 if(ioptio2.gt.0) then
        iopout = ioptio2
      else
        if(isgi.eq.0) then
          Write(6,190)
          call flush(6)
          Read (5,'(i5)') ioptio2          
          write(nlog,*) '  Report from screen option = ', ioptio2
        else
          ioptio2=0
        endif

        if(ioptio2.eq.0) then
          goto 210          
        endif
      endif        

      if(ioptio2.gt.maxrpt) then
        ioptio2 = 0
        goto 130               
      endif
c
c _________________________________________________________
c
c               Step 3; Call datinp for misc data
c               Do not read station data, etc for most reports
c rrb 200809/24; Add *.xst standard report
c               inx  =   switch
c                        0-read all input data
c                        1-read only *.ctl file
c
      numstax=maxsta
      inx=0      
      CALL DATINP(IIN,inx,numstax)
c
c               Reset option that was reused in datinp
      iopout=ioptio2
c
c _________________________________________________________
c               
c               Step 4; Read in output files if appropriate
c     write(nlog,*) '  Report; ioptio2 = ', ioptio2
      cplot = getid 
c     write(nlog,*) '  Report; cplot   = ', cplot

      if(ioptio2.ne.15) then
        if(iwell.gt.0) then
          call namext(maxfn, filenc, 'b42', filena) 
          open(42,file=filena,  status='Old',access='direct',recl=92,
     1       err=230)
          call bintop(42,1,nlog,ichk)
        endif

        call namext(maxfn, filenc, 'b43', filena) 
        open(43,file=filena,  status='Old',access='direct',recl=160,
     1       err=230)
        call bintop(43,1,nlog,ichk)

        call namext(maxfn, filenc, 'b44', filena)
        open(44,file=filena,  status='Old',access='direct',recl=160,
     1       err=230)
        call bintop(44,1,nlog,ichk)

        call namext(maxfn, filenc, 'b45', filena) 
        open(45,file=filena,  status='Old',access='direct',recl=4,
     1      err=230)

        call namext(maxfn, filenc, 'b47', filena) 
        open(47,file=filena,  status='Old',access='direct',recl=4,
     1      err=230)
c
c               Rio Grande Compact
        if(irg1+irg2.ge.1) then
          call namext(maxfn, filenc, 'b66', filena) 
          open(66,file=filena,  status='Old',access='direct',recl=96,
     1      err=230)
        endif
c
c               Structure Summary
        call namext(maxfn, filenc, 'b67', filena) 
        open(67,file=filena,  status='Old',access='direct',recl=132,
     1      err=230) 
        call bintop(67,1,nlog,ichk)
c
c               Plan output
        if(nplan.gt.0) then
          call namext(maxfn, filenc, 'b68', filena) 
          open(68,file=filena,  status='Old',access='direct',recl=180,
     1      err=230) 
c         call bintop(68,1,nlog,ichk)
        endif
c
c _________________________________________________________
c
c               Step X; Read in daily reports if necessary
c
c       write(nlog,*) '  Report; iday = ', iday
        if(iday.eq.1) then
c       
c                 Open file *.b49; Binary Daily Diversion
          call namext(maxfn, filenc, 'b49', filena) 
          open(49,file=filena,  status='old',access='direct',
     1         recl=160)
          call bintop(49,1,nlog,ichk)
        
c       
c                 Open file *.b50; Binary Daily Reservoir
          call namext(maxfn, filenc, 'b50', filena)
          open(50,file=filena,  status='old',access='direct',
     1         recl=160)
          call bintop(50,1,nlog,ichk)
c       
c                 Open file *.b65; Binary Daily Wells
          if(iwell.gt.0) then
            call namext(maxfn, filenc, 'b65', filena) 
            open(65,file=filena,  status='old',access='direct',
     1         recl=92)
            call bintop(65,1,nlog,ichk)
          endif
        endif    
c      
c _________________________________________________________
c
c               Step X; Check year

        write(nlog,120)  iystr0, iyend0, iystr, iyend

        if(iystr.lt.iystr0 .or. iyend.gt.iyend0) then
          write(6, 110) 
          write(nlog,110)
          write(6, 120)  iystr0, iyend0, iystr, iyend
          write(nlog,120)  iystr0, iyend0, iystr, iyend
          goto 250
        endif
      endif                   
      
      write(nlog,100) ioptio, ioptio2
c
c _________________________________________________________
c             Open and Read Reach Reach Report File
c
c _________________________________________________________
c								Set Default Data
c rrb 2007/02/22; Allow to run without a reach report        
      nreach=1
      do is=1,maxsta
        irch(is)=1
        RchID(is)='Reach_01    '
      end do
c      
      do i=1,maxrch
        RchIDR(i)='NA'
        RchNameR(i)='NA'
        nRchTo(i)=0
        nRchEnd(i)=0
      end do
c
c _________________________________________________________
c		Warn if both a River Reach Info (*.rir)
c		and River Data (*.rch) file are not provided
      irirf=0
      irchf=0
      filena=fileName(80)
      rec256=fileName(71)
      if(filena(1:2).ne.'-1') irirf=1
      if(rec256(1:2).ne.'-1') irchf=1
      if((irirf+irchf).eq.1) goto 233
c
c _________________________________________________________
c		Get River Reach Info (*.rir)
      inf = 80
      rec256=fileName(inf)
      filena=rec256
      
      if(filena(1:2).ne.'-1') then   
cx        write(nlog,*) ' '      
cx        Write(nlog,*) ' Report; River Reach File (*.rir) ', filena
cx        Write(6,*) '  Report; River Reach File (*.rir) ', filena
        
        write(nlog,155) filena
 155    format(/,72('_'),/
     1  '  Report; River Reach File (*.rir) ',/9x,a256)
     
        nf=55
        call GetFile(
     1   nlog, nchk, iin, infile, maxfile, maxfn, 
     1   nf, irisX, numRis, fileType(inf), fileSuf(inf), 
     1   fileName(inf), fpath1, filena)          

        iget=1        
        call GetRch(nlog, iget, maxsta, maxrch, 
     1    nreach, iRch, nRchTo, nRchEnd,
     1    RchId, RchIdR, RchNameR, cstaid)     
      endif
c
c _________________________________________________________
c		Get Reach Data (*.rch)
      inf = 71
      rec256=fileName(inf)
      filena=rec256
      
      if(filena(1:2).ne.'-1') then   
        write(nlog,*) ' '      
        Write(nlog,*) ' Report; Reach Report File (*.rch) ', filena
        Write(6,*) '  Report; Reach Report File (*.rch) ', filena
        
        write(nlog,156)
 156    format(/,72('_'),/
     1  '  Report; Reach Data File (*.rch) ')
c     
        nf=55
        call GetFile(
     1   nlog, nchk, iin, infile, maxfile, maxfn, 
     1   nf, irisX, numRis, fileType(inf), fileSuf(inf), 
     1   fileName(inf), fpath1, filena)          
c
        iget=2        
        call GetRch(nlog, iget, maxsta, maxrch, 
     1    nreach, iRch, nRchTo, nRchEnd,
     1    RchId, RchIdR, RchNameR, cstaid)     
      endif
c                         
c _________________________________________________________
c
c rrb 02/04/07; Get year information
c               Step X; Get year information.
c rrb 99/08/20; Daily needs to know the year type
	    call year(iystr, iyrmo, imomo, cyr1) 
c                         
c _________________________________________________________
c
c               Step X; 1-Binary Output

      if(iopout.eq.1) then
C
        call namext(maxfn, filenc, 'xbn', filenO) 
        open(25,FILE=filenO,STATUS='Unknown')
        
        call namext(maxfn, filenc, 'xbr', filenO2) 
        open(26,FILE=filenO2,STATUS='Unknown')
C
        CALL OUTSYS(IYStr,IYEnd)
c
        write(nlog,280)
        rec32= 'Binary Output:  *.xbn and *.xbr'
        write(nlog,282) rec32
        write(6,282) rec32

        close(25)
        close(26)
        goto 210
      endif
c
c _________________________________________________________
c               
c               Step X; 2-Detailed Node Accounting
      if(iopout.eq.2) then

        call namext(maxfn, filenc, 'xnm', filenO) 
        open(16,FILE=filenO,STATUS='Unknown')
C
        CALL OUTSYT(IYStr,IYEnd)
c
        write(nlog,280)
        rec32= 'Node Accounting: *.xnm'
        write(nlog,282) rec32
        write(6,282) rec32
        close(16)
C
C------  Detailed node accounting average
        call namext(maxfn, filenc, 'xna', filenO2) 
        open(16,FILE=filenO2,STATUS='Unknown')
c
        call outsyta(iystr, iyend)
c
        write(nlog,280)
        rec32= 'Ave Node Accounting: *.xna'
        write(nlog,282) rec32
        write(6,282) rec32

        close(16)
        goto 210
      endif
c
c _________________________________________________________
c
c               Step X; 3-WATER BALANCE

      if(iopout.eq.3) then
        cplot = getid 

        call namext(maxfn, filenc, 'xwb', filenO) 
        open(27,FILE=filenO,STATUS='Unknown')
C
c rrb 99/05/06; Ground Water Balance
        call namext(maxfn, filenc, 'xgw', filenO2) 
        open(28,FILE=filenO2,STATUS='Unknown')

c       CALL OUTBAL(IYStr,IYEnd)
        call outbal2(iystr,iyend,cplot)

c
        write(nlog,280)
        rec32= 'Surface Water Balance:  *.xwb'
        write(nlog,282) rec32
        write(6,282) rec32
        
        if(iwell.gt.0) then
          rec32= 'Ground Water Balance:   *.xgw'
          write(nlog,282) rec32
          write(6,282) rec32          
        endif

        close(27)
        close(28)
        goto 210            
      endif
C
c _________________________________________________________
c
C               Step X; 4-WATER RIGHT LIST (SORTED BY RANK)

      if(iopout.eq.4) then

        call namext(maxfn, filenc, 'xwr', filenO) 
        open(11,FILE=filenO,STATUS='Unknown')

        maxwrx=maxwr
        
c
c rrb 2009/06/09; Correction                
cx        call riginp(iin, maxwrx)
        maxres1=maxres
        maxdvr1=maxdvr
        call riginp(iin, maxres1, maxdvr1)
        

        maxnwrx=maxnwr
        call rigsor(maxnwrx)
        call outdeb(0, nreach)
c
        write(nlog,280)
        rec32= 'Water Right:  *.xwr'
        write(nlog,282) rec32
        write(6,282) rec32

        close(11)
        goto 210
      endif
c
c _________________________________________________________  
c
c               Step X; 16-SELECTED Parameter Output (*.xsp)
c
      if(iopout.eq.16) then

c       write(nlog,*) '  Report;', numsta, numdiv, numres, numdivw
        call namext(maxfn, filenc, 'xsp', filenO) 
        open(21,FILE=filenO,STATUS='Unknown')

        call namext(maxfn, filenc, 'xs2', filenO2) 
        open(24,FILE=filenO2,STATUS='Unknown')
c
c
c rrb 2003/09/02; Allow random file read
        ifn = 35
        rec256=fileName(ifn)
        filena=rec256      
        write(nlog,101) filena  
c
c rrb 99/05/20
        call putpath(maxfn, filena, fpath1)
        write(nlog,101) filena

        write(nlog,'(a256)') filena
        open(22, file=filena,status='old',err=230)

        CALL OUTsp
c
        write(nlog,280)
        rec32= 'Special Parameter Rep. (Matrix): *.xsp'
        write(nlog,282) rec32
        write(6,282) rec32
c
        rec32= 'Special Parameter Rep. (Column): *.xs2'
        write(nlog,282) rec32
        write(6,282) rec32

        close(21)
        close(24)
        goto 210
      endif
c
c _________________________________________________________
c
c               Step X; 19-Daily Selected Parameter Output (*.xds)
c
      if(iopout.eq.19) then

c       write(nlog,*) '  Report;', numsta, numdiv, numres, numdivw
        call namext(maxfn, filenc, 'xds', filenO) 
        open(21,FILE=filenO,STATUS='Unknown')

        call namext(maxfn, filenc, 'xd2', filenO2) 
        open(24,FILE=filenO2,STATUS='Unknown')
c
c               Get input file of print info (*.out)
c rrb 00/03/06; Monthly ISF data (*.ifm)
        
c       if(monisf.ge.2) call skip(iin,1)
c
c
c rrb 2003/09/02; Allow random file read
        ifn = 35
        rec256=fileName(ifn)
        filena=rec256
        write(nlog,*) ' Report; *.out = ', rec256
        write(nlog,101) filena  
c
c rrb 99/05/20
        call putpath(maxfn, filena, fpath1)
        write(nlog,101) filena

c       write(nlog,'(a256)') filena
        open(22, file=filena,status='old',err=230)

        CALL OUTspd
c
        write(nlog,280)
        rec32= 'Daily Special Parameter (Matrix): *.xds'
        write(nlog,282) rec32
        write(6,282) rec32
        
        rec32= 'Daily Special Parameter (Column): *.xd2'
        write(nlog,282) rec32
        write(6,282) rec32

        close(21)
        close(24)
        goto 210
      endif

c
c _________________________________________________________
c
c               Step X; 6-RESERVOIR PLOT


      if(iopout.eq.6) then
c
c rrb 06/14/97
        nx=numres
        if(nx.eq.0) goto 232

        call namext(maxfn, filenc, 'xrg', filenO) 
        open(9,FILE=filenO2,STATUS='Unknown')
           
        cplot = getid
        if(cplot.eq.blank) then
          if(isgi.eq.0) then
            write(6,*) ' '
            write(6,*) ' Enter the reservoir ID to Plot or 0 to stop'   
            write(6,*) ' '
            call flush(6)
            read(5,'(a12)') cplot   
          else
            write(6,*) ' Cannot read id from screen, use command option'
          endif
        endif
                                 
        if(cplot.eq.blank .or. cplot.eq.'0          ') then
        else
          call outpltr(igui, istop, cplot)
          cplot = ' '
        endif
c
        close(9)
        goto 210
c
        write(nlog,280)
        rec32= 'Reservoir Plot: *.xrg'
        write(nlog,282) rec32
        write(6,282) rec32
        
      endif
c _________________________________________________________
C
c               Step X; 7-DIVERSION PLOT

      if(iopout.eq.7) then
        nx=numdiv
        if(nx.eq.0) goto 232

        call namext(maxfn, filenc, 'xdg', filenO) 
        open(9,FILE=filenO,STATUS='Unknown')

        cplot = getid
  140   if(getid.eq.blank) then
          write(6,*) ' '
          write(6,*) ' Enter the Diversion ID to Plot or 0 to stop'   
          write(6,*) ' '
          call flush(6)
          read(5,'(a12)') cplot   
          write(nlog,'(a12)') cplot
        endif
                                 
        if(cplot.eq.blank .or. cplot.eq.'0          ') then
          close(9)
c
          write(nlog,280)
          rec32= 'Diversion Plot:  *.xdg'
          write(nlog,282) rec32
          write(6,282) rec32
          goto 210
        else         
          call outpltd(igui, istop, cplot)
          cplot = ' '          
          goto 140
        endif
        
      endif
c
c
c _________________________________________________________
c
c               Step X; 17-WELL PLOT
c
      if(iopout.eq.17) then
        nx=numdivw
        if(nx.eq.0) goto 232

        call namext(maxfn, filenc, 'xwg', filenO) 
        open(9,FILE=filenO,STATUS='Unknown')

        cplot = getid
  141   if(getid.eq.blank) then
          write(6,*) ' '
          write(6,*) ' Enter the Diversion ID to Plot or 0 to stop'   
          write(6,*) ' '
          call flush(6)
          read(5,'(a12)') cplot   
          write(nlog,'(a12)') cplot
        endif
                                 
        if(cplot.eq.blank .or. cplot.eq.'0          ') then
c 
          write(nlog,280)
          rec32= 'Well Plot:  *.xwg'
          write(nlog,282) rec32
          write(6,282) rec32

          close(9)
          goto 210
        else         
c
          call outpltw(igui, istop, cplot)
          cplot = ' '
          goto 141
        endif
      endif
C
c _________________________________________________________
c
c               Step X; 8-RESERVOIR COMPARISON
c
      if(iopout.eq.8) then
c
c rrb 06/14/97
        nx=numres
        if(nx.eq.0) goto 232
c rrb 00/03/06; Monthly ISF data (*.ifm)
c       if(monisf.ge.2) call skip(iin,1)

        ifn = 25
        rec256=fileName(ifn)
        filena=rec256
c       write(nlog,*) ' Report; *.out = ', rec256
c
c               Open EOM file
      Write(nlog,*) 'EOM Content File (*.eom) '
c
      call chekpor(iin, 55, nlog, 0, ioptio, 999, iystr, 
     1             imstr, 0, 10, cfacto, cyr1, maxfn,
     1             infile, idummy, nEomX, fpath1, filena)

      if(idummy.eq.0 .and. abs(cfacto-cfacto1).gt.small) 
     1 write(nlog,1640) '*.eom', cfacto, cfacto1, cfacto
     
c
c               Open output file
        call namext(maxfn, filenc, 'xrc', filenO) 
        open(9,FILE=filenO,STATUS='Unknown')
c
c               Open temporary files
        open(1,status='scratch',access='direct',recl=52,
     1       err=250)

c       cplot=blank
        cplot=getid
        CALL OUTresc(cplot)
c
        write(nlog,280)
        rec32= 'Reservoir Comparison:  *.xrc'
        write(nlog,282) rec32
        write(6,282) rec32

        close(9)
        close(1)
        goto 210
      endif
c
c _________________________________________________________
c
c		Step X; 9-DIVERSION AND STREAMFLOW COMPARISON
c
      if(iopout.eq.9 .or. iopout.eq.12) then
c
c               Skip over right files, etc to get historic *.rih
c rrb 00/03/06; Monthly ISF data (*.ifm)
        ifn = 27
        rec256=fileName(ifn)
        filena=rec256
c
c ---------------------------------------------------------
c              Open historic streamflow data
        close(3)
cx        Write(nlog,*) '  Report; Historic Streamflow File (*.rih) '
          write(nlog,103)
 103      format(/,72('_'),/
     1    '  Report; Historic Streamflow File (*.rih) ')
c
        call chekpor(iin, 3, nlog, 0, ioptio, 999, iystr, 
     1               imstr, 0, 8, rfacto, cyr1, maxfn,
     1               infile, idummy, nRihX, fpath1, filena)

      if(idummy.eq.0 .and. abs(rfacto-rfacto1).gt.small) 
     1 write(nlog,1640) '*.rih', rfacto, rfacto1, rfacto
     
c
c ---------------------------------------------------------
c              Open historic diversion data
        ifn = 28
        rec256=fileName(ifn)
        filena=rec256

        close(4)
        
        Write(nlog,*) '  Historic Diversion File (*.ddh) '
        write(nlog,104)
 104    format(/,72('_'),/
     1    '  Report; Historic Diversion File (*.ddh) ')
        
        call chekpor(iin, 4, nlog, 0, ioptio, 999, iystr, 
     1               imstr, 0, 9, dfacto, cyr1, maxfn,
     1               infile, idummy, nDdhX, fpath1, filena)
     
      if(idummy.eq.0 .and. abs(dfacto-dfacto1).gt.small) 
     1 write(nlog,1640) '*.ddm', dfacto, dfacto1, dfacto
 
c
c ---------------------------------------------------------
c
c               Open output files
        call namext(maxfn, filenc, 'xdc', filenO) 
        open(9,FILE=filenO,STATUS='Unknown')

        call namext(maxfn, filenc, 'xsc', filenO2) 
        open(10,FILE=filenO2,STATUS='Unknown')
c
c               Open temporary files
        open(1,status='scratch',access='direct',recl=52,
     1       err=250)

        open(2,status='scratch',access='direct',recl=52,
     1       err=250)

c       cplot=blank
        cplot = getid
        CALL OUTdivc(cplot,nreach)
c
        write(nlog,280)
        rec32= 'Diversion Comparison:  *.xdc'
        write(nlog,282) rec32
        write(6,282) rec32
c        
        rec32= 'Streamflow comparison: *.xsc'
        write(nlog,282) rec32
        write(6,282) rec32
        
        close(9)
        close(10)
        close(1)
        close(2)
        goto 210
      endif

c
c _________________________________________________________
c
c               Step X; 18-Well Comparison
c
      if(iopout.eq.18) then
c
        nx=numdivw
        if(numdivw.eq.0) goto 232
c
c               Skip over right files, etc to get historic pumping
c rrb 00/03/06; Monthly ISF data (*.ifm)
        ifn = 29
        rec256=fileName(ifn)
        filena=rec256
c
c ---------------------------------------------------------
c               Open Historic Pumping
        Write(nlog,*) '  Historic Well Pumping (*.weh) '
c
c       call chekpor(iin, 55, nlog, 0, ioptio, 999, iystr, 
        call chekpor(iin, 3, nlog, 0, ioptio, 999, iystr, 
     1               imstr, 0, 11, wfacto, cyr1, maxfn,
     1               infile, idummy, nWehX, fpath1, filena)

      if(idummy.eq.0 .and. abs(wfacto-wfacto1).gt.small) 
     1 write(nlog,1640) '*.weh', wfacto, wfacto1, wfacto
c
c ---------------------------------------------------------
c               Open output file
        call namext(maxfn, filenc, 'xwc', filenO) 
        open(9,FILE=filenO,STATUS='Unknown')
c
c ---------------------------------------------------------
c               Open temporary files
        open(1,status='scratch',access='direct',recl=52,
     1       err=250)
     
c
        cplot=getid
        CALL OUTwelc(cplot, nreach)
c
        write(nlog,280)
        rec32= 'Well Comparison:  *.xwc'
        write(nlog,282) rec32
        write(6,282) rec32
        
        close(9)
        close(1)
        close(3)
        goto 210
      endif


C
c _________________________________________________________
c
c               Step X; 10-Actual Diversion output for CU model
c
      if(iopout.eq.10 .or. iopout.eq.14 .or. iopout.eq.5) then
c
c ---------------------------------------------------------
c               Open output files
        call namext(maxfn, filenc, 'xcu', filenO) 
        open(33,FILE=filenO,STATUS='Unknown')

        call namext(maxfn, filenc, 'xsh', filenO2) 
        open(34,FILE=filenO2,STATUS='Unknown')

        call namext(maxfn, filenc, 'xsu', filenOX) 
        open(35,FILE=filenOX,STATUS='Unknown')

        call namext(maxfn, filenc, 'xev', filenOX) 
        open(36,FILE=filenOX,STATUS='Unknown')

        call namext(maxfn, filenc, 'xwd', filenOX) 
        open(37,FILE=filenOX,STATUS='Unknown')
c
c ---------------------------------------------------------
c
c		Call OutCu        

        maxwx=maxdivw
        maxgrpx=maxgrp
c
c rrb 208/12/24; Revised reach approach        
cx      CALL OUTcu(maxwx,maxgrpx,nreachD,nreachW)
        CALL OUTcu(maxwx,maxgrpx,nreach)
        
        call outrev
c
        write(nlog,280)
        rec32= 'Total Consumptive Use: *.xcu'
        write(nlog,282) rec32
        write(6,282) rec32
c
        rec32= 'Total Reservoir Evap:  *.xev'
        write(nlog,282) rec32
        write(6,282) rec32
c
        rec32= 'Total Shortage:        *.xsh'
        write(nlog,282) rec32
        write(6,282) rec32
c
        rec32= 'Total Supply:          *.xsu'
        write(nlog,282) rec32
        write(6,282) rec32
c
        rec32= 'Total by WD:           *.xwd'
        write(nlog,282) rec32
        write(6,282) rec32
        
c
c ---------------------------------------------------------
c		Close files
        close(33)
        close(34)
        close(35)
        close(36)
        close(37)
        goto 210
      endif
c
c _________________________________________________________
c
c       	Step X; 11-Stream flow Information file (*.xrx)
c
      if(iopout.eq.11) then
c
c rrb 00/03/06; Monthly ISF data (*.ifm)
c       if(monisf.ge.2) call skip(iin,1)
        ifn = 27
        rec256=fileName(ifn)
        filena=rec256
c
c              Open historic streamflow data
        close(3)
        Write(6,*) '  Historic Streamflow Report: *.xrx'
        Write(nlog,*) '  Historic Streamflow Report: *.xrx'

c
        call chekpor(iin, 3, nlog, 0, ioptio, 999, iystr, 
     1               imstr, 0, 8, rfacto, cyr1, maxfn,
     1               infile, idummy, nXrxX, fpath1, filena)

      if(idummy.eq.0 .and. abs(pfacto-pfacto1).gt.small) 
     1 write(nlog,1640) '*.rih', rfacto, rfacto1, rfacto
     
c
c               Open output files
        call namext(maxfn, filenc, 'xrx', filenO) 
        open(33,FILE=filenO,STATUS='Unknown')

        call outriv
c
        write(nlog,280)
        rec32= 'River Summary: *.xrx'
        write(nlog,282) rec32
        write(6,282) rec32

        close(33)
        goto 210
      endif
C                         
c _________________________________________________________
c
c               Step X; 13-Standard Reports (*.xst)
c
      if(iopout.eq.13) then
C
        write(nlog,*) '  Report_0; Standard Report (*.xst)'
        write(6,*) '  Report_0; Standard Report (*.xst)'    
        
        call namext(maxfn, filenc, 'xdd', filenO) 
        open(33,FILE=filenO,STATUS='Unknown')
        
        call namext(maxfn, filenc, 'xre', filenO2) 
        open(34,FILE=filenO2,STATUS='Unknown')
        
        call namext(maxfn, filenc, 'xop', filenOX) 
        open(46,FILE=filenOX,STATUS='Unknown')
        
        call namext(maxfn, filenc, 'xir', filenOX) 
        open(48,FILE=filenOX,STATUS='Unknown')
        
        if(iwell.gt.0)  then
          call namext(maxfn, filenc, 'xwe', filenOX) 
          open(41,FILE=filenOX,STATUS='Unknown')
        endif
       
        if(irg1+irg2.ge.1) then
          open(41,FILE=filenOX,STATUS='Unknown') 
          call namext(maxfn, filenc, 'xrg', filenOX) 
          open(52,FILE=filenOX,STATUS='Unknown')
        endif
          
        call namext(maxfn, filenc, 'xss', filenOX)         
        open(40,FILE=filenOX,STATUS='Unknown')
        
        if(nplan.gt.0) then
          call namext(maxfn, filenc, 'xpl', filenOX) 
          open(21,FILE=filenOX,STATUS='Unknown')
        endif
        
c
c               Open daily output files 
        if(iday.eq.1) then
          call namext(maxfn, filenc, 'xdy', filenOX) 
          open(35,FILE=filenOX,STATUS='unknown')

          call namext(maxfn, filenc, 'xry', filenOX) 
          open(36,FILE=filenOX,STATUS='unknown')

          call namext(maxfn, filenc, 'xwy', filenOX)
          open(37,FILE=filenOX,STATUS='unknown')
        endif

c
c		Get water rights for header data
        write(nlog,*) '  Report_1; calling riginp'
        write(6,*) '  Report_1; calling riginp'        
        maxwrx=maxwr
c
c rrb 20100204; Correction        
cx        call riginp(iin, maxwrx)
        maxres1=maxres
        maxdvr1=maxdvr
        call riginp(iin, maxres1, maxdvr1)        
c
c               Skip over time series files, etc to get *.out file
        ioutx=0
c
c               Get input file of print info (*.out)
c rrb; 98/12/31; Wells
c rrb 00/03/06; Monthly ISF data (*.ifm)
c       if(monisf.ge.2) call skip(iin,1)

c
c rrb 2003/09/02; Allow random file read
        write(nlog,*) '  Report_2; infile = ', infile
        write(6,*) '  Report_2; infile = ', infile
        
        ifn = 35
        rec256=fileName(ifn)
        filena=rec256
        write(nlog,*) ' Report; *.out = ', rec256
C
        write(nlog,*) '  Report_3; filena = ', filena(1:5)
        write(6,*) '  Report_3; filena = ', filena(1:5)    
        
        if(filena(1:5) .eq. '     ') goto 142     
c
c rrb 99/05/20
        call putpath(maxfn, filena, fpath1)
        write(nlog,101) filena
c       write(nlog,'(a256)') filena
        open(22, file=filena,status='old',err=142)
        ioutx=1
        
 142    if(ioutx.eq.0) then
          write(nlog,*) '  Report; No *.out file found,',
     1                   ' will print all stations'
        endif
C
        write(nlog,*) '  Report_4; outres'
        write(6,*) '  Report_4; outres'    
      
        call outres
        call outdivw
        call outopr
        call outxss
c
c		Optional daily output        

        if(iday.eq.1) then
          call daydivo
          call dayreso
          call daywelo
        endif
c
c		Optional well, isf reach, riogrande and plan output
        if(iwell.eq.1)     call outwel
        if(ireach.eq.1)    call outifr
        if(irg1+irg2.ge.1) call outrg
c
c		If plans are on, get operational data        
	if(nplan.gt.0) then
          maxwrx=maxwr
          call SetPlanO      
          call outpln	
        endif
c        
        write(nlog,280)
        rec32= 'Diversion Summary:         *.xdd'
        write(nlog,282) rec32
        write(6,282) rec32
c
        rec32= 'Reservoir Summary:         *.xre'
        write(nlog,282) rec32
        write(6,282) rec32
c
        rec32= 'Operating Rule Summary:    *.xop'
        write(nlog,282) rec32
        write(6,282) rec32
c
        rec32= 'Instream Reach Summary:    *.xir'
        write(nlog,282) rec32
        write(6,282) rec32

        if(iwell.ge.1) then
          rec32= 'Well Summary:              *.xwe'
          write(nlog,282) rec32
          write(6,282) rec32
        endif

c       if(ieffmax.eq.1) then
          rec32= 'Structure Summary:         *.xss'
          write(nlog,282) rec32
          write(6,282) rec32
c       endif

        if(irg1+irg2.ge.1) then
          rec32= 'Rio Grande Compact Info:   *.xrg'
          write(nlog,282) rec32
          write(6,282) rec32
        endif
        
        if(nplan.ge.1) then
          rec32= 'Plan Summary:              *.xpl'
          write(nlog,282) rec32
          write(6,282) rec32
        endif
        

        if(iday.eq.1) then
          rec32= 'Daily Diversion output in: *.xdy'
          write(nlog,282) rec32
          write(6,282) rec32
c          
          rec32= 'Daily Reservoir output in: *.xry'
          write(nlog,282) rec32
          write(6,282) rec32
        endif

        if(iday.eq.1 .and. iwell.ge.1) then
          rec32= 'Daily Well output in:      *.xwy'
          write(nlog,282) rec32
          write(6,282) rec32
        endif
        
        close(33)
        close(34)
        close(35)
        close(36)
        close(37)
        close(40)
        close(41)
        close(46)
        close(48)
        close(52)
        goto 210
      endif
c
c _________________________________________________________                         
c               Step X; 15-Structure List

      if(iopout.eq.15) then
C
        call namext(maxfn, filenc, 'xdl', filenO) 
        open(33,FILE=filenO,STATUS='Unknown')

        call outtop(33,1,22)
        write(33,150)

        do nd=1,numdiv
          write(33,152) nd,cdivid(nd),divnam1(nd),divcap(nd)
        end do  
c        
        write(nlog,280)
        rec32= 'Diversion List:      *.xdl'
        write(nlog,282) rec32
        write(6,282) rec32

        close(33)
        goto 210
      endif
c
c _________________________________________________________                         
c               Step X; 21-Plan Output (*.xpl)

      if(iopout.eq.21) then
        write(nlog,*) ' Report; Calling plan output (outpln)'
        call namext(maxfn, filenc, 'xpl', filenO) 
        open(21,FILE=filenO,STATUS='Unknown')
c
c		Get operational right data        
        maxwrx=maxwr
c
c rrb 20100204; Correction        
cx        call riginp(iin, maxwrx)
        maxres1=maxres
        maxdvr1=maxdvr
        call riginp(iin, maxres1, maxdvr1)        
        call SetPlanO      
c
c		Print report       
        call outpln
c        
        write(nlog,280)
        rec32= 'Plan Summary:      *.xpl'
        write(nlog,282) rec32
        write(6,282) rec32
        goto 210
      endif
c
c _________________________________________________________                         
c               Step X; 22-Well Structures to Plan (*.xwp)

      if(iopout.eq.22) then
        write(nlog,*) ' Report; Calling Well Plan output (outWelP)'
        call namext(maxfn, filenc, 'xwp', filenO) 
        open(22,FILE=filenO,STATUS='Unknown')
c          
c ---------------------------------------------------------
c               Step x; Read all water rights
c
        maxwrx=maxwr
        if(ichk.eq.4) write(nlog,*) ' Report; Calling riginp'      
c
c rrb 20100204; Correction        
cx        call riginp(iin, maxwrx)
        maxres1=maxres
        maxdvr1=maxdvr
        call riginp(iin, maxres1, maxdvr1)        
        
c          
c ---------------------------------------------------------
c               Step x; Get Plan Well Data 
c			Note call after Riginp because
c                       well rights must be known 

        ifn=63
        rec256=fileName(ifn)
        filena=rec256(1:72)
      
        if(filena(1:2).eq.'-1') then
          goto 230
        else
          call GetPlnW(maxdvrW)
          if(iout.eq.1) write(nlog,*) ' Report; Out of GetPlnW'
        endif  
c          
c ---------------------------------------------------------
c               Step x; Call OutWelP for Well Plan Output
c
        
        call OutWelP
c        
        write(nlog,280)
        rec32= 'Well to Plan Summary:      *.xwp'
        write(nlog,282) rec32
        write(6,282) rec32
        
        goto 210
      endif
      
c
c _________________________________________________________                         
c               Step X; 23-Aug plan to Well Structures (*.xpw)
c !!!!!!!!!!  report never completed. Began with outWelP

      if(iopout.eq.23) then
        write(6,*) 
     1  ' Augmentation Plan to Well Report (*.xpw) is not available'
        goto 210
        
        write(nlog,*) 
     1   ' Report; Calling Aug Plan to Well output (outPlnW)'
        call namext(maxfn, filenc, 'xpw', filenO) 
        open(23,FILE=filenO,STATUS='Unknown')
c          
c ---------------------------------------------------------
c               Step x; Read all water rights
c
      maxwrx=maxwr
      if(ichk.eq.4) write(nlog,*) ' Report; Calling riginp'      
c
c rrb 20100204; Correction        
cx      call riginp(iin, maxwrx)
      maxres1=maxres
      maxdvr1=maxdvr
      call riginp(iin, maxres1, maxdvr1)      
        
c          
c ---------------------------------------------------------
c               Step x; Get Plan Well Data 
c			Note call after Riginp because
c                       well rights must be known 

        ifn=63
        rec256=fileName(ifn)
        filena=rec256(1:72)
      
        if(filena(1:2).eq.'-1') then
          goto 230
        else
          call GetPlnW(maxdvrW)
          if(iout.eq.1) write(nlog,*) ' Report; Out of GetPlnW'
        endif  
c          
c ---------------------------------------------------------
c               Step x; Call OutWelP for every Well print 
c		        an associated Aug Plan
c
        
        call OutPlnW
c        
        write(nlog,280)
        rec32= 'Aug Plan to Well:      *.xpw'
        write(nlog,282) rec32
        write(6,282) rec32
        
        goto 210
      endif
      
c
c _________________________________________________________                         
c               Step X; 24-Reach Water Balance Output (*.xrw)

      if(iopout.eq.24) then
        write(nlog,157)
 157    format(/,72('_'),/
     1  '  Report; Reach Water Balance (*.xrw) ')        
     
        call namext(maxfn, filenc, 'xrw', filenO) 
        open(22,FILE=filenO,STATUS='Unknown')
c
c		Print report     
        call OutRchR(nreach,cplot)
c        
        write(nlog,280)
        rec32= 'Reach Water Balance:      *.xrw'
        write(nlog,282) rec32
        write(6,282) rec32
        
        goto 210
      endif
     
c
c _________________________________________________________
c rrb 7/29/94 Additional Output

  210 call dattim(idatx, itim2, isgi)

      ctime1x = itim1(1)+itim1(2)/60.+itim1(3)/3600.+itim1(4)/100./3600.
      ctime2x = itim2(1)+itim2(2)/60.+itim2(3)/3600.+itim2(4)/100./3600.
      ctimed = (ctime2x-ctime1x)*60.             

c     write(nlog,501) ctimed*60.0, ctimed*60./float(iye-iys+1)
      write(nlog,220) ctimed*60.0, ctimed*60./float(iyend-iystr+1)
     
      if(filenO(1:2).ne.'NA') then
        write(6,270) filenO
        write(nlog,270) filenO
      else
        write(6,272) 
        write(nlog,272)
      endif  
      
      call flush(6)
      goto 260
c
c _________________________________________________________
c               Formats
  100 format(/, 
     1  '  Report Option:',/,
     1  10x, 'Primary report option (ioptio)   = ', i5,/
     1  10x, 'Secondary report optio (ioptio2) = ', i5)
     
  101   format(/,'  Report; Output Control File (*.out or *.xou)',/,
     1         5x, a72)
     
  110     format(/,
     1      '  Report; Report period not within simulation period')
     
  120     format(/,
     1      '  Report; Simulation period  = ', 2i5,/
     1      '          Reporting period   = ', 2i5)
     
  150   format(//,
     1    '    # ID           Name                     Cap (cfs)',/
     1    ' ---- ------------ ------------------------ ---------')
     
  152   format(i5, 1x, a12, 1x, a24, 1x, f8.0)

  180 FORMAT(7I2)
  
  190 FORMAT(//,
     1 ' Report; The report option provided (if any) cannot be found',/
     1 '         Note StateM.log contains the command provided',/
     1          ' To stop or get a report enter one of the following',/
     1          '  0 : Stop (NA)                      ',/,
     1          '  1 : Data Printed to Binary files (*.xbn, *.xbr',/,
     1          '  2 : Detailed Node Accounting (*.xnm,*.xna) ',/,
     1          '  3 : Water Balance (*.xwb, *.xgw) ',/,
     1          '  4 : Water Right List (*.xwr) '/,
     1          '  5 : Water Supply (*.xsu) ',/,
     1          '  6 : Graph Data for Reservoirs (*.xrg)',/,
     1          '  7 : Graph Data for Diversions and Gauges (*.xdg)',/,
     1          '  8 : Comparison for Reservoirs (*.xrc)',/,
     1          '  9 : Coomparison Diversion (*.xdc)',/
     1          ' 10 : Consumptive Use Model Report (*.xcu, *.xsu, ',/
     1          '      *.xsh, *.xev, *.xwd)',/
     1          ' 11 : Stream Information File Report (*.xrx)',/
     1          ' 12 : Comparison Stream (*.xsc)',/
     1          ' 13 : Standard Reports (*.xdd, *.xre, *.xop, *.xir',/
     1          '      *.xss)',/
     1          ' 14 : Shortage Summary (*.xsh)',/
     1          ' 15 : Structure List (*.xdl)',/
     1          ' 16 : Selected Parameter (*.xsp, *.xs2)',/
     1          ' 17 : Graph Data for Wells (*.xwg)',/
     1          ' 18 : Comparison for Wells (*.xwc)',/
     1          ' 19 : Daily Selected Parameter (*.xds, *.xd2)'/
     1          ' 20 : No Log (NA)'/,     
     1          ' 21 : Plan Summary (*.xpl)',/
     1          ' 22 : Well Plan Summary (*.xwp)',/
     1          ' 23 : Aug plan to Well Structures (*.xpw)',/
     1          ' 24 : Reach Report (*.xrh)',/)

  200   FORMAT(//,'  Report; OUTPUT OPTION :',/,
     1            ' 1 : Monthly',/,
     1            ' 2 : Average',/,
     1            ' 3 : Both'/)
 1640 format(/,72('-'),/
     1 '  Mdainp; WARNING FILE ', A5,/ 
     1 '          HAS A UNIT CONVERSION FACTOR         = ', F10.4,/
     1 '          WHILE THE CONTROL FILE HAS A FACTOR  = ', F10.4,/
     1 '          THE TIME SERIES DATA CONTROLS FACTOR = ', F10.4,/
     1 '          Note the above override was implemented in ',
     1            'StateMod Version 11.02')      
     
  220 format(/,'  Time to Report              = ',f8.3,' seconds  or ',
     1                                             f8.3,' seconds/year')
      
  270 format(/,
     1 '  Report; Successful Termination ',/
     1 '          Primary output in file:',/
     1 '          ',a256)
      
  272 format(/,
     1 '  Report; Requested Report unavailable')
      
  280   FORMAT(/,72('_'))
  282   Format('  Report; ', a32)
c
c _________________________________________________________
c               Error messages
c
  230 write(nlog,240) filena
  240 format('  Report; Problem opening file: ', a256)
      goto 250

  232 write(nlog,242) nx
  242 format('  Report; Problem number of structures for report =',i8)
      goto 250
      
  233 write(nlog,243) nx
  243 format(
     1 '  Report; Problem for REach Reporting boht a:',/
     1 '          River Reach (*.rir) file and a ',/
     1 '          Reach Data (*.rch) file must be provided',/
     1 '          in the response file (*.rsp).',/
     1 '          For Reach Reporting recommend you provide both',/
     1 '          For no reach processing recommend you provide none')
      goto 250
      
  245 write(nlog,246) 
  246 format(
     1 '  Report; Problem random response file (*.rsp) ',/
     1 '          format required. ',/
     1 '          Reconmned you revise response file (*.rsp)')
c
c _________________________________________________________
c               Unsuccessful termination Messages
c
  250 write(6,*)  '  Stopped in Report, see log file'
      write(nlog,*) '  Stopped in Report'
c
      call flush(6)
c
c rrb 06/14/97; Fortran GUI operation
      if(igui.eq.0) then
        write(6,*) 'Stop 1' 
        call flush(6)
        call exit(1)

        stop 
      else
        istop=1
      endif
c
 260  close(1)
      close(2)
      close(3)
      close(4)
      close(9)
      close(11)
      close(12)
      close(19)
      close(20)
      close(21)
      close(22)
      close(23)
      close(25)
      close(27)
      close(28)
      close(33)
      close(34)
      close(35)
      close(36)
      close(37)
      close(42)
      close(43)
      close(44)
      close(45)
      close(46)
      close(47)
      close(48)
      close(67)


c
c rrb 20100208; Correction
      ioptio2=0
      return
      END     
