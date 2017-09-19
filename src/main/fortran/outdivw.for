C
c *********************************************************
      subroutine outdivw
c
c
c _________________________________________________________
c	Program Description
c
c       Outdivw; Same as outdiv but includes wells
c         Generate matrix tables of diversions
c
c _________________________________________________________
c       Update History
c
c rrb 99/12/17; Add structure type to *.xdd title
c rrb 01/04/03; Add variable unit capability
c
c
c _________________________________________________________
c       Documentation
c              iplot = 0 not plotting
c              iplot = n diversion, instream or gage ID to plot
c              nid = # of ID's requested
c              ndiv = # of data values in the binary file
c              nout = output file #
c
c              idallx = 0, no *.out with requested id's provided
c                      =1, yes *.out provided
c
c rrb 99/12/24; New Id convention as follows:
c		Strtype         1-5000 = Diversion
c                               5001 - 7500 = ISF
c                               7501 - 10000 = Reservoir
c				10001 -12500 = Plan
c				12501 -15000 = Wells
c                               < -10000 = baseflow only
c                               -1* above = baseflow plus a structure 
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
c                           
c rrb 05/29/96; change to generalize dimensions
c     dimension  dat1(40), cname(6), dat1t(40)
      character  blank*12, cdx*12, ftype*24, ptype*24, ida0*12,
     1           rec24*24, rec4*4, ccallID*12, cname1*24
c
c _________________________________________________________
c		Step 1; Initilze
c
c
c		iout=1 print multiple structure warning     
     
      write(6,101) 'OutDivW '
      write(nlog,101) 'OutDivW '
 101  format(/,72('_'),/'  Subroutine ', a8)
      call flush(6)
      
c
c               Wells 
c     ndiv = 23
c     ndiv = 33
c
c rrb 2005/11/22; River Loss and Carrier Loss
c     ndiv = 35
c     ndiv = 37
c
c		Set number of output values 1x in StateM.for
c	        and store in common.inc      
      ndiv = ndivO        
      nres = nresO
      ndivw= nwelO
c
c		Set ndivP number of columns to print before 
c		    call information (control location and right)
c	 	    e.g. column of Avail Flow)	      
c		Set ndivT number of columns to print in title
c		Set ndivF number of columns to weight by factor

c		Set nrid column of rid, strucutre type 
c		Set nxstr column of xstr,number of structures
c               Set nrimcdX column of rimcdX, call location
c		Set nccallR column of call right
c		Set nrimcdX column of call location
c
c		The following are based on ndivo=37

      ndivP=29
      ndivT=31
c      
c rrb 2007/01/07; ndivO=38; new variable to track return from a plan       
c     ndivF=33      
c     nrid=34
c     nxstr=35
c     nrimcdx=36
c     nccallR=37
      
      ndivF=ndivO-4
      nrid=ndivO-3
      nxstr=ndivO-2
      nrimcdx=ndivO-1      
      nccallR=ndivO
      
      nshort=14
      
      small=0.001
      nout = 33
      nid=numsta
      blank = '            '
c     write(io99,*) '  Outdivw; numsta', numsta
c
C-------------------------------------------------------------------
C
C------  Diversion Summary 
C
C-------------------------------------------------------------------
C
      call outtop(nout,1,16)
c
c _________________________________________________________
c
c               Get requested ID's
      idallx=0
      if(ioutx.eq.1) then
        call getin(0, iid, maxsta, idallx, idtype, ftype, ptype, idreq)
      endif
c
c _________________________________________________________
c
c               Set id screen switch
      if(idallx.eq.0) then
        nid=numsta
      else
        nid=iid
      endif
c     write(99,*) '  Outdivw; ioutx, idallx, nid = ', ioutx,idallx,nid
c                      
c
      ida0 = '0'
      ip1 = 0                                         
      ix = 25
c _________________________________________________________
c
c		Get structure type
      do 190 ip=1,nid

        if(idallx.eq.0) then                           
          is=ip
        else
          call getid(idtype(ip),is,ir,ir2,idreq(ip))
c
c               Skip operational rights
          if(is.le.0) goto 190
        endif      
c       write(99,*) '  Outdivw; is = ', is
c
c _________________________________________________________
c
c rrb 10/27/94 Print Output to screen every 25 structures
        ix = ix+1
        c = float(ip)/float(numsta)*100.0
c       if(numsta.ge.25) then
c         if(ix.ge.25 .or. is.eq.nid) then
c            ix = 0
c           write(6,260) ip, numsta, c  
c            call flush(6)
c          endif
c        else
          write(6,260) ip, numsta, c  
          call flush(6)
c        endif
c
c _________________________________________________________
c
c              Get station ID (assumes one per station)
        irecs=numsta+is+numtop
        read(43,rec=irecs,err=280) (dat1(i),i=1,ndiv)
                     
cr      nd = dat1(ndiv-1) 
cr      na = dat1(ndiv)
cr      nd = dat1(ndiv-3) 
cr      na = dat1(ndiv-2)
        
        nd = dat1(nRid)
        na = dat1(nXstr)
        
        
c
c _________________________________________________________
c
c              Print header if no stream or structure at this station
        if(nd.eq.0) then
          cdx  = blank
          cdx = 'NA '
          ip1=ip1+1         
c
c _________________________________________________________
c
c              Print title to Gage or non structure Output
          write(nout,220) cunitm, headin1(1), HEADIN1(2),ip1,
     1                    cstaid(is), stanam1(is), 
     1                    cstaid(is), stanam1(is) 
          goto 140  
        endif
  
c
c _________________________________________________________
c
c              Separate single and multiple structure output
c rrb; 04/112/01
c       if(na.le.1) then
        if(na.eq.0) then
c
c _________________________________________________________
c
c rrb 99/12/22; Define baseflow only (nd<-10000 baseflow only) 
          if(nd.lt.-10000) then    
            nd1=(-1*nd)-10000
c           write(io99,*) '  Outdivw; Flo nd, nd1 = ', nd, nd1 
            cdx = crunid(nd1)
            cdx = 'Baseflow'
            ip1=ip1+1         
            is2=is
c
c rrb 2010/11/15; Update
            cname1=runnam1(nd1)        
          endif
        endif  
c
c _________________________________________________________
c
c		Define structures          
        if(na.eq.1) then
          nd1=iabs(nd)
cr        write(nlog,*) ' Outdivw; n, nd1 ', nd, nd1
c
c _________________________________________________________
c
c               Define diversion header (0<nd<5000)
          if(nd1.gt.0 .and. nd1.le.5000) then    
c           write(io99,*) '  Outdivw; Div nd, nd1 = ', nd, nd1           
            cdx = cdivid(nd1)
            ip1=ip1+1         
            is2=is
c                                 
c rrb 2010/11/15; Update          
            cname1=divnam1(nd1)   
          endif
c
c _________________________________________________________
c
c               Define instream header (5000 < nd1 < 7500)
          if(nd1.gt.5000 .and. nd1.le.7500) then
            nd1=nd1-5000
c           write(io99,*) '  Outdivw; Isf nd, nd1 = ', nd, nd1 
            cdx = cifrid(nd1)
            ip1=ip1+1         
            is2=ifrst2(nd1)
c                                 
c rrb 2010/11/15; Update          
            cname1=xfrnam1(nd1)           
          endif
c
c _________________________________________________________
c
c               Define reservoir header (7500 < nd1 < 10000)
          if(nd1.gt.7500 .and. nd1.le.10000) then
            nd1=nd1-7500
c           write(io99,*) '  Outdivw; Res nd, nd1 = ', nd, nd1
            cdx = cresid(nd1)
            ip1=ip1+1         
            is2=is
c                                 
c rrb 2010/11/15; Update          
            cname1=resnam1(nd1)            
          endif
c _________________________________________________________
c
c               Define plan header (10000 < nd1 < 12500)
          if(nd1.gt.10000 .and. nd1.le.12500) then
            nd1=nd1-10000
cr          write(io99,*) '  Outdivw; Plan nd, nd1 = ', nd, nd1
            cdx = Pid(nd1)
            ip1=ip1+1         
            is2=is
c                                 
c rrb 2010/11/15; Update          
            cname1=Pname1(nd1)                 
cx            do i=1,6
cx              cname(i)=pname(i,nd1)
cx            end do
          endif
c _________________________________________________________
c
c rrb 2007/02/21; Add wells
c               Define well header (12500 < nd1 < 15000)
          if(nd1.gt.12500 .and. nd1.le.15000) then
            nd1=nd1-12500
c           write(io99,*) '  Outdivw; Wells nd, nd1 = ', nd, nd1
            cdx = cdividw(nd1)
            ip1=ip1+1         
            is2=is
c                                 
c rrb 2010/11/15; Update          
            cname1=divnamw1(nd1)               
          endif
c
c ---------------------------------------------------------
c		End single structure IF, Then, Endif          
        endif
c
c _________________________________________________________
c
c              Multiple structures
c
        if(na.gt.1) then
c         cdx = blank
          cdx = 'Multiple'
          ip1=ip1+1         
          is2 = is
c                                 
c rrb 2010/11/15; Update          
          cname1=' '        
        endif
        
c
c _________________________________________________________
c
c               Print header
        write(nout,210) cunitm, headin1(1),HEADIN1(2),ip1,
     1              cdx, nd, ida0, cname1,
     1              cstaid(is),  stanam1(is),
     1              cstaid(is2), stanam1(is2) 
c
c _________________________________________________________
c
c              Print water right info for Total only

        nd2=iabs(nd)
        maxwrx=maxwr
c
c ---------------------------------------------------------
c		Get diversion and ISF rights
        if(nd2.gt.   0 .and. nd2.le. 7500) then
          call outwr2(maxwrx,1,is,ir,nout)
        endif
c
c ---------------------------------------------------------
c		Get reservoir rights
        if(nd2.gt.7500 .and. nd2.le.10000) then
          call outwr2(maxwrx,2,is,ir,nout)
        endif
c
c ---------------------------------------------------------
c rrb 2007/01/17; Get well rights
        if(nd2.gt.12500 .and. nd2.le. 15000) then
          call outwr2(maxwrx,6,is,nd1,nout)
        endif

c
c               Print title card once per structure
  140   continue
c  
c _________________________________________________________
c
c               Finally year loop
c 
        do 180 iy=iystr,iyend           
c
c
c               Print title card every year
cr        write(nout,230) (i, i=1,ndiv-8+2)
          write(nout,230) (i, i=1,ndivP+2)

          call year(iy, iyrmo, imomo, cyr1)
          do 150 i=1,ndiv
  150       dat1t(i) = 0.0
c _________________________________________________________
c
c		Month Loop
          do 170 im=1,12
c
            irecs=((iy-iystr0)*12+(im-1))*numsta+is+numtop
            read(43,rec=irecs,err=280) (dat1(i),i=1,ndiv)

            do 160 i=1,ndivF
              dat1(i)=dat1(i)*fmo(im)
              dat1t(i) = dat1t(i) + dat1(i)*ftot
  160       continue
c 
c _________________________________________________________
c
c		Set call location
c
c rrb 2006/03/07; Set to a variable based on column location
c           imcdX=dat1(ndiv-1)
            imcdX=dat1(nrimcdX)
            
            if(imcdX.gt.0) then
              ccallID=cstaid(imcdX)
            else
              ccallID ='NA '
            endif  
c 
c _________________________________________________________
c
c		Adjust reporting if the call is at the headgate
            if(imcdX.gt.0 .and. imcdX.eq.is) then
              ccallID='Hgate_Limit'
              dat1(ndiv)=-1.0
            endif  
c 
c _________________________________________________________
c
c		Adjust reporting if it is short but not called out
c
c rrb revise for minor roundoff
c           if(imcdX.le.0 .and. dat1(nshort).gt.small) then
            ishort=dat1(nshort)
c
c rrb 2008/06/10; Correction            
            if(imcdX.eq.0 .and. ishort.gt.0) then
              ccallID='Cap/Wr_Limit'
              dat1(ndiv)=-1.0
            endif
c
c		The following occurrs for structures with no water 
c		right (e.g. imcdx=-1; its initial value)
            if(imcdX.lt.0 .and. ishort.gt.0) then
              ccallID='NA          '
              dat1(ndiv)=-1.0
            endif
c
c _________________________________________________________
c               Print station data 
            if(isigfig.eq.0) then
              write(nout,240)   cdx, cstaid(is), iyrmo(im), xmonam(im), 
     1                        (dat1(j), j=1,ndivP), ccallID,dat1(ndiv)
c     1                         ,dat1(34), dat1(nrimcdX) 
            endif
            
            if(isigfig.eq.1) then
              write(nout,241)   cdx, cstaid(is), iyrmo(im), xmonam(im), 
     1                        (dat1(j), j=1,ndivP), ccallID,dat1(ndiv)
c     1                         ,dat1(34), dat1(nrimcdX) 
            endif
            
            if(isigfig.eq.2) then
              write(nout,242)   cdx, cstaid(is), iyrmo(im), xmonam(im), 
     1                        (dat1(j), j=1,ndivP), ccallID,dat1(ndiv)
            endif
  170     continue
c
c _________________________________________________________
c
c               Print total to *.xdd only
          write(nout,200)
          ccallID='NA '
          if(isigfig.eq.0) then
            write(nout,240)   cdx, cstaid(is), iyrmo(13), xmonam(13), 
     1                      (dat1t(j), j=1,ndivP),ccallID, -1.0
c     1                         ,dat1T(34) 
          endif
          
          if(isigfig.eq.1) then
            write(nout,241)   cdx, cstaid(is), iyrmo(13), xmonam(13), 
     1                      (dat1t(j), j=1,ndivP),ccallID, -1.0
          endif
          
          if(isigfig.eq.2) then
            write(nout,242)   cdx, cstaid(is), iyrmo(13), xmonam(13), 
     1                      (dat1t(j), j=1,ndivP),ccallID, -1.0
          endif
          write(nout,*) ' '
c
c _________________________________________________________
c
c               End Year Loop      
  180   continue
c
c               End Diversion Loop
  190 continue
c
c        Formats
  200 format(2('___________ '), 2(' ____'), 29(' _______'),
     1  ' ____________ ___________')
  210 FORMAT(    /, '   Diversion Summary ',a5, /,3x,a80,/,
     1  3X,A80,33X, 'PAGE NO. ',I3,//,
     1  '    STRUCTURE ID (0 = total)  : ',a12, 1x, i8,/,
     1  '    STRUCTURE ACCT (0 = total): ',a12,/,
     1  '    STRUCTURE NAME            : ',a24,/,
     1  '    RIVER LOCATION - FROM     : ',a12, 1x, a24,/,
     1  '    RIVER LOCATION - TO       : ',a12, 1x, a24,/)
 
c 220 FORMAT('',/,'    Gage  Summary ',a5, /,3x,20a4,/, 
  220 FORMAT(    /,'    Gage  Summary ',a5, /,3x,a80,/, 
     1  3X,a80,33X, 'PAGE NO. ',I3,//,
     1  '    RIVER LOCATION - FROM     : ',a12, 1x, a24,/, 
     1  '    RIVER LOCATION - TO       : ',A12, 1X, a24,/) 
  230   format(/,
     1  '                                         ',
     1  '                                                        ',
     1  '                                              Shortage    ',
     1  '            Water Use                        ',
     1  'Station In/Out                       ',
     1  'Station Balance     ',/,
     1  '                                      ',
     1  '  Demand              From River By           ',
     1  '           From Carrier By    ',
     1  ' Carried                ________________',
     1  ' _______________________________', 
     1  ' _______________________________________', 
     1  ' _______________________________',/
     1  '                                 ',
     1  '  _______________ _______________________________    From ', 
     1  '_______________________',
     1  ' Exchang    From   Total   Total      CU        ',
     1  '      To      To          Upstrm   Reach',
     1  '  Return    Well From/To   River   River   River   River',
     1  '   Avail Control          Control',/
     1  'Structure   River                 ',
     1  '   Total      CU Priorty Storage   Other    Loss    Well',
     1  ' Priorty   Other    Loss  Bypass   SoilM  Supply   Short',
     1  '   Short',
     1  '      CU   SoilM   Other    Loss  Inflow    Gain',
     1  '    Flow Deplete GW Stor  Inflow  Divert by Well Outflow',
     1  '    Flow Location           Right',/
     1  'ID          ID           Year   Mo      NA      NA',
     1  3('     (+)'),'     (-)', 3('     (+)'),'     (-)',
     1  '     (+)      NA      NA      NA      NA',
     1  '      NA      NA      NA      NA',
     1  3('     (+)'),'     (-)     (+)     (+)',
     1  2('     (-)'),'     (+)      NA NA                    NA',/
     1  34x, 29('    (', i2,')'),' (',i2,')                (', i2,')'/       
     1  '___________ ____________ ____ ____', 29(' _______'),
     1  ' ____________ ___________')

  240  format(2a12,i5, 2x, a3, 29f8.0, 1x, a12,f12.3,20f8.0)
  241  format(2a12,i5, 2x, a3, 29f8.1, 1x, a12,f12.3,20f8.1)
  242  format(2a12,i5, 2x, a3, 29f8.2, 1x, a12,f12.3,20f8.2)
  260  format('+', '   Printing Diversion & Stream Summary',
     1        i5,' of ', i5, '; or ',f8.0, ' % Complete')
  270  return
c
c               Error messages
  280  write(99,*) '   Outdivw; Requested data exceeds binary file size'
c
c               Error Warning
  290 write(6,300) 
      write(99,310) 
  300 format('    Stopped in Outdivw',/,
     1       '    See the *.log file')
  310 format('    Stopped in Outdivw')
      write(6,*) 'Stop 1'      
      call flush(6)
      call exit(1)


      stop 
      END
