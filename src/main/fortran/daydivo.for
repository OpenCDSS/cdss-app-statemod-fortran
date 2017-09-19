C     Last change:  RRB   8 Jan 2002   11:50 am
C
c *********************************************************
      subroutine Daydivo
c
c
c _________________________________________________________
c	Program Description
c

c       Daydivo; it prints daily diversion / stream file
c
c               Note: Similar to outdiv.for; but daily
c
c               Called by execut.for or report.for
c
c __________________________________________________________
c
c               Update History
c rrb 99/12/28; New Id convention as follows:
c                               0-5000 = reservoir
c                               5001 - 7500 = ISF
c                               7501 - 10000 = reservoir
c				10001 -12500 = Plan
c				12501 -15000 = Wells
c                               < -10000 = baseflow only
c                               -1* above = baseflow plus a structure 
c
c
c __________________________________________________________
c
c               Documentation
c
c              iplot = 0 not plotting
c              iplot = n diversion, instream or gage ID to plot
c              nid = # of ID's requested
c              ndiv = # of data values in the binary file
c              nout = output file #
c
c              idallx = 0, no *.out with requested id's provided
c                      =1, yes *.out provided
c
c
c
c _________________________________________________________
c		Dimensions
c
      include 'common.inc'
c                           
      character  blank*12, cdx*12, ftype*24, ptype*24, ccallID*12,
     1           cname1*24  

      write(6,*) ' Subroutine Daydivo'
      write(6,*) ' '             
c     write(nlog,*) ' '             
c     write(nlog,*) ' Subroutine Daydivo'
      write(nlog,*) ' Daydivo; iresop, cunitd', iresop, cunitd

      call flush(6)
c
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
c		Set nccallR column of call right
c		Set nrimcdX column of call location
c		Set nprint column of data to print (less stuff on right)
c		Set nshort column of total short
c
c		The following are based on ndivo=37

      ndivP=29
      ndivT=31
      ndivF=33
      
      nrid=34
      nxstr=35
      nrimcdx=36
      nccallR=37
      
      nshort=14
      nadj=33

      nout = 35
      nf=49
      nid=numsta
      blank = '            '
      small=0.001
c
c               Set Unit Conversion
c     cu=1.0
c     cunit='(cfs)'
c     if(iresop.eq.2) then
c       cu=factor
c     endif
c     if(iresop.eq.3) then
c       cu=factor*0.001                        
c     endif
C
C-------------------------------------------------------------------
C
C------  Diversion Summary 
C
C-------------------------------------------------------------------
C
      call outtop(nout,1,30)
c
c               Get requested ID's
      idallx=0
      if(ioutx.eq.1) then
        call getin(0, iid, maxsta, idallx, idtype, ftype, ptype, idreq)
      endif
c
c               Set id screen switch
      if(idallx.eq.0) then
        nid=numsta
      else
        nid=iid
      endif
c     write(nlog,*) '  Daydivo; idallx, nid = ', idallx, nid
c                      
      ida0 = 0
      ip1 = 0                                         
      ix = 25

      do 190 ip=1,nid

        if(idallx.eq.0) then                           
          is=ip
        else
          call getid(idtype(ip),is,ir,ir2,idreq(ip))
c
c               Skip operational rights
          if(is.le.0) goto 190
        endif      
c       write(nlog,*) '  Daydivo; is = ', is
c
c rrb 10/27/94 Print Output every 25 structures
        ix = ix+1
        c = float(ip)/float(numsta)*100.0
c
          if(ix.ge.25 .or. is.eq.nid) then
            ix = 0
            write(6,260) ip, numsta, c  
            call flush(6)
          endif
c       else
          write(6,260) ip, numsta, c  
          call flush(6)
c       endif
c
c              Get station ID (assumes one per station)
        irecs=numsta+is+numtop
c       write(nlog,*) '  Daydivo nf, irecs, ndiv', nf, irecs, ndiv
c
c rrb o1/12/01; Test running monthly demand

        read(nf,rec=irecs,err=280) (dat1(i),i=1,ndiv)
c       write(nlog,*) '  Daydivo 1', (dat1(i), i=1,ndiv)
                     
c       nd = dat1(ndiv-3) 
c       na = dat1(ndiv-2)
        
        nd = dat1(nRid)
        na = dat1(nXstr)
c
c              Print to gage file if no diversions at this station
        if(nd.eq.0) then
          cdx  = ' NA'
          ip1=ip1+1         
c
c              Print title to Gage Output
          write(nout,220) cunitd,
     1                    headin1(1), HEADIN1(2),ip1,
     1                    cstaid(is), stanam1(is), 
     1                    cstaid(is), stanam1(is)
          goto 140  
        endif
c
c _________________________________________________________
c              Separate single and multiple structure output
c rrb; 04/12/01;
c       if(na.le.1) then
        if(na.eq.0) then

c
c rrb 99/12/28; Stream gage only
          if(nd.lt.-10000) then
            nd1=(-1*nd)-10000
            cdx=crunid(nd1)
            cdx='Baseflow'
            ip1=ip1+1
            is2=is

cx            do i=1,6
cx              cname(i)=runnam(i,nd1)
cx            end do
            cname1=runnam1(nd1)
          endif
        endif  
c
c _________________________________________________________
c		Detine structures
        if(na.eq.1) then        
          nd1=iabs(nd) 
c
c _________________________________________________________
c		Diversions
c
          if(nd1.gt.0 .and. nd1.le.5000) then    
            cdx = cdivid(nd1)
            ip1=ip1+1         
            is2=is
            cname1=divnam1(nd1)
          endif
c
c _________________________________________________________
c
c               Define instream header (nd<0)
c rrb 99/12/28; New IDs
c         if(nd.lt.0) then
c           nd=-1*nd
          if(nd1.gt.5000 .and. nd1.le.7500) then
            nd1=nd1-5000

            cdx = cifrid(nd1)
            ip1=ip1+1         
            is2=ifrst2(nd1)
cx            do i=1,6
cx              cname(i)=xfrnam(i,nd1)
cx            end do
             cname1=xfrnam1(nd1)
          endif
c
c _________________________________________________________
c
c               Define reservoir header
          if(nd1.gt.7500 .and. nd1.le.10000) then
            nd1=nd1-7500
c           write(nlog,*) '  Daydivo; nd1 = ', nd1
            cdx = cresid(nd1)
            ip1=ip1+1         
            is2=is
cx            do i=1,6
cx              cname(i)=resnam(i,nd1)
cx            end do
              cname1=resnam1(nd1)
          endif
c          
c _________________________________________________________
c
c               Define plan header (10000 < nd1 < 12500)
          if(nd1.gt.10000 .and. nd1.le.12500) then
            nd1=nd1-10000
cr          write(nlog,*) '  Outdivw; Plan nd, nd1 = ', nd, nd1
            cdx = Pid(nd1)
            ip1=ip1+1         
            is2=is
            
cx            do i=1,6
cx              cname(i)=pname(i,nd1)
cx            end do
            cname1=pname1(nd1)
          endif  
c _________________________________________________________
c
c rrb 2007/02/21; Define well header (12500 < nd1 < 15000)
          if(nd1.gt.12500 .and. nd1.le.15000) then
            nd1=nd1-12500
c           write(nlog,*) '  Outdivw; Wells nd, nd1 = ', nd, nd1
            cdx = cdividw(nd1)
            ip1=ip1+1         
            is2=is
            cname1=divnamw1(nd1)
          endif
c
c		Endif for 1 structure only (na=1)		
        endif  
        
c
c _________________________________________________________
c
c              Multiple structures
       if(na.gt.1) then
          cdx = 'Multiple'
          ip1=ip1+1         
          is2 = is
cx          do i=1,6
cx            cname(i)=0
cx          end do
          cname1='Multiple'
        endif
c
c _________________________________________________________
c               Print title card once per structure
c
c ---------------------------------------------------------
c               Print header
        write(nout,210) cunitd
     1             ,headin1(1), HEADIN1(2)
     1             ,ip1 ,cdx,ida0, cname1, 
     1              cstaid(is),  stanam1(is), 
     1              cstaid(is2), stanam1(is2)
c
c              Print water right info for Total only
        maxwrx=maxwr
c        
        nd2=iabs(nd)
c
c ---------------------------------------------------------
c		Print diversion and ISF rights
        if(nd2.gt.   0 .and. nd2.le. 7500) then
          call outwr2(maxwrx,1,is,ir,nout)
        endif
c
c ---------------------------------------------------------
c		Print reservoir rights
        if(nd2.gt.7500 .and. nd2.le.10000) then
          call outwr2(maxwrx,2,is,ir,nout)
        endif        
c
c ---------------------------------------------------------
c rrb 2007/01/17; Print well rights
        if(nd2.gt.12500 .and. nd2.le. 15000) then
          call outwr2(maxwrx,6,is,nd1,nout)
        endif        
  140   continue
c  
c
c _________________________________________________________
c               Finally year loop
c 
        do iy=iystr,iyend           
          call year(iy, iyrmo, imomo, cyr1)

          do im=1,12

c
c               Print title card every month
          write(nout,230) (i,i=1,ndivT)
c
c rrb 01/12/01; Test running monthly demand
c         do i=1,ndiv
          do i=1,ndiv
            dat1t(i) = 0.0
          end do

            iox=0
            do id=1,mthday(im)
              fday=1.0
              if(iresop.eq.1 .or. iresop.eq.4) fday=float(mthday(im))

              iox=iox+1
              if(iox.ge.6) then
                write(nout,*) ' '
                iox=1
              endif
c
c             irecs=((iy-iystr0)*12+(im-1))*numsta+is+numtop
              irecs=((iy-iystr0)*12+(im-1))*numsta*31 +
     1              (id-1)*numsta+is+numtop

              read(nf,rec=irecs,err=280) (dat1(i),i=1,ndiv)
c
c rrb 02/01/08; For idaydem=1 (daily demand is a running total)
c               Do not sum column 1 (demand) and set column 12
c               (total Shortage) to the last daily entry
              ib=1
              IF(idaydem.eq.1) ib=2
c
              do i=ib,ndivF
                dat1(i)=dat1(i)*fdy(im)
c
c rrb 01/04/03; for Cfs output print average for total
                dat1t(i) = dat1t(i) + dat1(i)/fday
              end do
c
c
c rrb 02/01/08; For idaydem=1 (daily demand is a running total)
c               Do not sum column 1 (demand) and set column 12
c               (total Shortage) to the last daily entry
              IF(idaydem.eq.1) then
                dat1(1)=dat1(1)*fdy(im)

                IF(id.eq.1) then
                  dat1t(1)=dat1(1)/fday
                endif

                IF(id.eq.mthday(im)) then
cr                dat1t(12)=dat1(12)/fday
                  dat1t(nshort)=dat1(nshort)/fday
                endif
              endif
c 
c _________________________________________________________
c
c		Set call location
            imcdX=dat1(nRimcdX)
            if(imcdX.gt.0) then
              ccallID=cstaid(imcdX)
            else
              ccallID ='NA          '
            endif  
c 
c _________________________________________________________
c
c		Adjust reporting if the call is at the headgate
            if(imcdX.gt.0 .and. imcdX.eq.is) then
              ccallID='Hgate_Limit '
cr            dat1(ndiv)=-1.0
              dat1(nCcallR)=-1.0
            endif  
c 
c _________________________________________________________
c
c		Adjust reporting if it is short but not called out
cr          if(imcdX.lt.0 .and. dat1(12).gt.small) then
            if(imcdX.lt.0 .and. dat1(nshort).gt.small) then
              ccallID='Cap/Wr_Limit'
cr            dat1(ndiv)=-1.0              
              dat1(nCcallR)=-1.0
            endif  
              

c 
c _________________________________________________________
c 
c               Print station data for every node (diversion, gage
c                 reservoir, etc.)
              if(isigfig.eq.0) then
                write(nout,240) cdx, cstaid(is), iyrmo(im), xmonam(im),
     1            id,(dat1(j), j=1,ndivP), ccallID,dat1(nCcallR)      
              else
                write(nout,242) cdx, cstaid(is), iyrmo(im), xmonam(im),
     1            id,(dat1(j), j=1,ndivP), ccallID,dat1(nCcallR)      
              endif

c
c               End Day Loop
            end do
c
c 
c _________________________________________________________
c               Print monthly total 
            write(nout,200)
            if(isigfig.eq.0) then
              write(nout,240) cdx, cstaid(is), iyrmo(13), xmonam(13), 
     1                         -1, (dat1t(j), j=1,ndivP),
     1                         'NA          ',-1.0     
            else
              write(nout,242) cdx, cstaid(is), iyrmo(13), xmonam(13), 
     1                         -1, (dat1t(j), j=1,ndivP),
     1                         'NA          ',-1.0
c 242  format(2a12,i5, 2x, a3, i4, 29f8.1, 1x, a12,f8.3,i8)  
     
            endif
            write(nout,*) ' '
c
c 
c _________________________________________________________
c               End Month Loop
          end do
c
c 
c _________________________________________________________
c               End Year Loop
        end do
c
c 
c _________________________________________________________
c               End Station Loop
  190 continue
c
c 
c _________________________________________________________
c		Formats
c
  200 format(2('___________ '), 2(' ____'), ' ___',29(' _______'),
     1  ' ____________ _______')
c     
c
  210 FORMAT(/, '    Diversion Summary ',a5,/,3x,a80,/,
     1  3X,a80,33X, 'PAGE NO. ',I3,//,
     1  '    STRUCTURE ID              : ',a12,/,
     1  '    STRUCTURE ACCT (0 = total): ',i5,/,
     1  '    STRUCTURE NAME            : ',a24,/,
     1  '    RIVER LOCATION - FROM     : ',a12, 1x, a24,/,
     1  '    RIVER LOCATION - TO       : ',a12, 1x, a24,/)
c 
  220 FORMAT('',/,'    Gage  Summary ',a5,/,3x,a80,/, 
     1  3X,a80,33X, 'PAGE NO. ',I3,//,
     1  '    RIVER LOCATION - FROM     : ',a12, 1x, a24,/, 
     1  '    RIVER LOCATION - TO       : ',A12, 1X, a24,/) 
c
  230   format(/,
     1  '                                             ',
     1  '                                                        ',
     1  '                                              Shortage    ',
     1  '            Water Use                        ',
     1  'Station In/Out                       ',
     1  'Station Balance     ',/,
     1  '                                             ',
     1  '                   From River By             ',
     1  '         From Carrier By',
     1  '     Carried                ________________',
     1  ' _______________________________', 
     1  ' _______________________________________', 
     1  ' _______________________________',/
     1  '                                    ',
     1  '     Total      CU _______________________________    From ', 
     1  '_______________________',
     1  '      or    From   Total   Total      CU        ',
     1  '      To   Total          Upstrm   Reach',
     1  '  Return    Well From/To   River   River   River   River',
     1  '   Avail Control      Control',/
     1  'Structure   River                     ',
     1  '  Demand  Demand Priorty Storage   Other    Loss    Well',
     1  ' Priorty Sto_Exc    Loss Exchang   SoilM  Supply   Short',
     1  '   Short',
     1  '      CU   SoilM  Return    Loss  Inflow    Gain',
     1  '    Flow Deplete GW Stor  Inflow  Divert by Well Outflow',
     1  '    Flow Location       Right',/     
     1  'ID          ID           Year   Mo Day      NA      NA', 
     1  3('     (+)'),'(    (-)', 3('     (+)'),'(    (-)',
     1  '     (+)      NA      NA      NA      NA',
     1  '      NA      NA      NA      NA',
     1  3('     (+)'), '     (-)     (+)     (+)',
     1  2('     (-)'), '     (+)',
     1  '      NA  NA               NA',/
     1  '                                      ', 29('    (',i2,')'),
     1  ' (',i2,')            (', i2,')',/   
     1  '___________ ____________ ____ ____ ___', 29(' _______'),
     1  ' ____________ _______')

cr240  format(2a12,i5, 2x, a3, i4, 40f8.0)
  240  format(2a12,i5, 2x, a3, i4, 29f8.1, 1x, a12,f8.3,i8) 

cr242  format(2a12,i5, 2x, a3, i4, 40f8.2)
  242  format(2a12,i5, 2x, a3, i4, 29f8.1, 1x, a12,f8.3,i8)  

  260   format('+', '   Printing Diversion & Stream Summary',
     1        i5,' of ', i5, '; or ',f8.0, ' % Complete')
  270  return
c
c               Error messages
  280  write(nlog,*) '   Daydivo; Requested data exceeds binary size'
c
c               Error Warning
      write(6,300) 
      write(nlog,310) 

  300 format('    Stopped in Daydivo',/,
     1       '    See the *.log file')
  310 format('    Stopped in Daydivo')
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop 
      END
