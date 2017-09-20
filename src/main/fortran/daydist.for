c     Last change:  C    27 Apr 97    1:26 am
c
      subroutine daydist(ichk1,  ityp,iyr,monx,mon,imd,
     1                   mthday, nmax, n, iday,
     1                   qm,qdx, qd,cidy,cidx,cid1)
c
c _________________________________________________________
c	Program Description
c
c       Daydist; It distributes monthly data to daily in various ways
c                    (iday=0) set equal to monthly average value
c
c	Called by DayEst.for
c
c                    (iday=-1) divide monthly by 31
c
c                    (iday=1) set using the pattern at a daily gage
c                      The daily gage ID is the same as the ID of
c                      the gage being estimated.  
c                      Monthly total controls
c                       Datinp checks and stops if supplied wrong code
c                       for reservoirs                  
c
c                    (iday=2) set using the pattern at a daily gage
c                      The daily gage is not the same ID as the ID of
c                      the gage being estimated.  
c                      Monthly total controls.  
c                      Note does not apply to reservoir data since it
c                      is an EOM, not a volume.  Datinp checks and 
c                      stops if supplied wrong code.
c
c                    (iday=3) set using daily data (no pattern)
c                      The daily gage ID is the same as the ID of
c                      the gage being estimated.  
c                      Daily controls
c
c                    (iday=4) set using a pattern which connects
c                      average monthly data at mid point of month.
c                      Monthly total controls.
c                      Datinp checks and stops if supplied wrong code.
c                      Developed specifically for demand data
c
c                    (iday=5) set using a pattern which connects
c                      average monthly data at mid point of month.
c                      Monthly total controls.
c                      Datinp checks and stops if supplied wrong code.
c                      Developed specifically for EOM reservoir data
c
c
c       Called by dayest.for
c       Note; Streamflow must be gains because monthly data is 
c             already changed to gains
c
c __________________________________________________________
c
c               Update History
c
c rrb 01/04/02; Revised to treat reservoirs special (daily 
c               data prevails)
c rrb 01/08/08; Added type 4 (pattern via connecting mid points)
c               useful for calculated demands
c rrb 01/09/01; Added type 5 (pattern via connecting end points) 
c               useful for targets
c
c __________________________________________________________
c
c               Documentation
c 
c       ityp        = 1 streamflow
c                     2 diversions
c                     3 instream flows
c                     4 reservoirs
c                     5 N/A
c                     6 wells
c                     7 diversion consumptive irrigation requirement
c                     8 well consumptive irrigation requirement
c                     9 reservoir EOM
c
c
c       ityp        = 11 streamflow
c                     22 diversions
c                     13 instream flows
c                     14 reservoirs
c                     15 N/A
c                     16 wells
c                     17 diversion consumptive irrigation requirement
c                     18 well consumptive irrigation requirement
c                     19 reservoir EOM
c
c       nmax        = array size
c       n           = # of stations (stream, diversions, wells, etc)
c       imd         = # of days / month (from execut.for)
c       iday      0 = No daily adjustment (instream flow comes in cfs)
c                 1 = daily gage pointer for pattern is same as gage ID
c                 2 = daily gage pointer for pattern is not same as ID
c                -1 = divide monthly by # of days per month
c       qm          = monthly value
c       qd          = daily value calculated
c       qdx         = daily value read
c       qmx         = local variable used to store calculated monthly 
c                     total calculated
c       ichk1       = 0 no detailed printout
c                   = 1 detailed printout (set to 1 if ichk (*.ctl) =6)
c
c       cidy(is)    = ID to use to estimate daily data for station is
c                     Read in datinp.for
c       cidx(iss)   = ID for station with daily data.
c                     Read in daydata
c	cid1(is)    = ID of data being estimated
c
c               Parameters related to type 4 (connect midpoints)
c       mon         = month counter (Oct = 1 for WYR)
c       monx        = month id (Oct = 10 for WYR)
c
c       mthday(i)   = days per month set in datinp
c       mt          = days per month (mon)
c       mt2         = days to half month (integer math)
c
c       jb1         = beginning day of month (running for year)
c       je1         = midpoint of month (running for year)
c       jb2         = same as above
c       je2         = ending day of month (running for year)
c       x1          = for 1'st equation day at minpoint of month -1
c                     for 2'nd equation day at midpint of month
c       x2          = for 1'st equation day at minpoint of month 
c                     for 2'nd equation day at midpint of month +1
c       y1          = value at x1
c       y2          = value at x2
c       ichk4       = print detailed data for type 4 (midpoint)
c       ichk4x      = print limited data for type 4 (gives output
c                     appropriate for plotting diversions)
c       ichk5       = print detailed data for type 5 (end point)
c       ichk5x      = print limited data for type 5 (gives output
c                     appropriate for plotting diversions)
c
c __________________________________________________________
c
c               Dimensions

      dimension iday(nmax), qm(12,nmax), qd(32,nmax), qdx(32,nmax),
     1           qp(32),    mthday(12)
c
      dimension cidy(nmax), cidx(nmax), cid1(nmax),  dtype(20)

      character cidy*12, cidx*12, cidy1*12, cid1*12,
     1  dtype*55, blank*12,cidz*12, cid11*12, cchk*12

      data dtype/
     1     'Stream File - Daily (*.rid)                            ',
     2     'Direct Flow Demand File - Daily (*.ddd)                ',
     3     'Instream Flow Demand File - Daily (*.ifd)              ',
     4     'Reservoir Target File - Daily (*.tad)                  ',
     5     'NA                                                     ',
     6     'Well Demand File - Daily (*.wed)                       ',
     7     'Diversion IWR via Demand not a data file               ',
     8     'Well IWR via Demand not a daily file                   ',
     9     'Diversion IWR via IWR data - Daily (*.iwd)             ',
     1     'Well Only IWR via IWR data - Daily (*.iwd)             ',
     1     'Historic Stream File - Daily (*.rhy)                   ',
     2     'Historic Diversion File - Daily (*.dhy)                ',
     3     '                                                       ',
     4     'Reservoir EOD File - Daily (*.eoy)                     ',
     5     'NA                                                     ',
     6     'Historic Well Pumping File - Daily (*.why)             ',
     7     '                                                       ',
     8     '                                                       ',
     9     '                                                       ',
     2     '                                                       '/

c
c _________________________________________________________
c               Step 1; Initilize

      nlog=99

c     write(nlog,*) ' '
c     write(nlog,*) '  Inside daydist.for, ityp, n = ', ityp, 

      blank =''
      blank = blank                                         
c
c               Note ichk1 can be controlled by setting ichk=6 in *.ctl
c               via dayest
c		ichkX=0, no details; 1=print details
      ichkx=0
      ichk4=0
      ichk4x=0
      ichk5=0
      ichk5x=0
      cchk='374516      '

      small=0.001
      smalln=-1.*small

      fx=1.9835*float(imd)
      
      if(ichk4x.eq.1)  then
        write(nlog,*) ' Daydist; ichk4x, ityp ', ichk4x, ityp
      endif
      
      if(ichk1.eq.1 .or. ichkx.eq.1)  then
	WRITE(nlog,140) dtype(ityp)
c       write(nlog,*) '  Daydist; mthday', (mthday(i), i=1,12) 
      endif
      
cr    write(nlog,*) ' Daydist; ityp', ityp
c
c =========================================================
c               Begin station loop
      do is=1,n
	ifound=0
c
c               Initilize, particularly non days in a month to zero
	do i=1, 32
	  qd(i,is) = 0.0
	end do
	
	cid11=cid1(is)
	
c	if(ityp.eq.11) then
c	  write(nlog,*) ' Daydist;', is, cid1(is), iday(is),
c    1	   qm(mon,is), qm(mon,is)*fx
c	endif  
c
c _________________________________________________________
c               Step 2a; Type 0 Set daily to monthly average
c
	if(iday(is).eq.0) then
	  do id=1,imd
	    qd(id,is) = qm(mon,is)
	  end do
	  ifound=1

	  if(ichk1.eq.1) then
c
c rrb 2006/05/02; Additional output	  
	    write(nlog,180) iday(is), is, cid11
	  endif

	  goto 100
	endif
c
c _________________________________________________________
c               Step 2b; Type 1 or Type 2 Distribute based on another
c                        or same gage
c                        Note qm and qdx(32,is2) are average daily 
c                        (cfs)
	if(iday(is).eq.1 .or. iday(is).eq.2) then
	  cidy1=cidy(is)
	  c=1.0

	  do is2=1,n
	    if(cidy1.eq.cidx(is2)) then
c	    
c ---------------------------------------------------------
c		Set monthly factor. 
           if(abs(qdx(32,is2)).lt.small) then
             c=0.0
           else
             c=qm(mon,is)/qdx(32,is2)      
           endif  
c           
c ---------------------------------------------------------
c		Set negatives to zero unless baseflow
c		diversion (ityp=12)
           if(qdx(32,is2).lt.small .and. ityp.ne.12) c=0.0
c           
c ---------------------------------------------------------
c		Detailed Output
	      if(ichk1.eq.1) then
c
c rrb 2006/05/09; Better output	      
cr		write(nlog,182) iday(is), is, cidy1, is2, cidx(is2)
		write(nlog,182) iday(is), is, cid11, is2, cidx(is2)
		write(nlog,200) qm(mon,is), qdx(32,is2), c,
     1                          qm(mon,is)*fx, qdx(32,is2)*fx, c
	      endif
c           
c ---------------------------------------------------------
c		Calculate daily value

	      do id=1,imd
		qd(id,is)=qdx(id,is2)* c
	      end do

	      ifound=1
	      goto 100              
	    end if
	  end do
	endif
c
c _________________________________________________________
c               Step 2c; Type 3 Distribute based on same gage
c                        Note Daily data prevails
	if(iday(is).eq.3) then
	  cidy1=cidy(is)
	  c=1.0

	  do is2=1,n
	    if(cidy1.eq.cidx(is2)) then
		
	      c=1.0  
	      if(ichk1.eq.1) then
c
c rrb 2006/05/09; Better output	      	      
cr		write(nlog,183) iday(is), is, cidy1, is2, cidx(is2)
		write(nlog,183) iday(is), is, cid11, is2, cidx(is2)
		write(nlog,200) qm(mon,is), qdx(32,is2), c,
     1                          qm(mon,is)*fx, qdx(32,is2)*fx, c
	      endif

	      do id=1,imd
		qd(id,is)=qdx(id,is2)* c
	      end do

	      ifound=1
	      goto 100              
	    end if
	  end do
	endif
c
c _________________________________________________________
c               Step 2d; Type -1 Distribute based on days per month
c       write(nlog,*) '  Daydist iday(is) = ', iday(is)
	if(iday(is).eq.-1) then
	  c=float(imd)
	  do id=1,imd
	    qd(id,is) = qm(mon,is) / c
	  end do


	  if(ichk1.eq.1) then
c
c rrb 2006/05/09; Better output	      	  
cr	   write(nlog,184) iday(is), is, cidy1
	   write(nlog,184) iday(is), is, cid11
	  endif


	  ifound=1
	  goto 100
	endif

c
c _________________________________________________________
c               Step 2e; Type 4 Distribute by connecting mid points 
c                        of month

c                        Note qm and qdx(32,is2) are average daily 
c                        (cfs)
	if(iday(is).eq.4) then
	  cidy1=cidy(is)
	  c=1.0
c
c               Set miscellaneous month ID constants
          im=mon-1
	  ip=mon+1
	  mt=mthday(mon)
	  m2=mthday(mon)/2
	  rm2=float(mthday(mon))/2.0
c 
c               Get beginning and ending days for this month
	  jb1=0
	  do i=1,im
	    jb1=jb1+mthday(i)
	  end do
c 
c               Reset month-1 & month+1 for begining and ending months
	  im=amax0(1, mon-1)
	  ip=amin0(mon+1, 12)
	    
c                                                    
c               Set day loop counters
 
	  jb1=jb1+1
	  je1=jb1+m2 - 1             
c 
c               Begin calculations for first half of month
	  x1=float(jb1)-float(mthday(im))/2.0 -1.0
	  x2=float(jb1)+rm2 - 1.0
 
	  y1=qm(im,is)
	  y2=qm(i,is)
 
	  if(ichk4.eq.1) then
	    write(nlog,*) ' im,  mthday', im, mthday(im)
	    write(nlog,*) ' mon, mthday', mon, mthday(mon)
 
	    write(nlog,*) ' jb1, je1', jb1, je1
	    write(nlog,*) '  x1,  x2', x1, x2
	    write(nlog,*) '  y1,  y2', y1, y2 
	    write(nlog,*) '  y1,  y2', 
     1        y1*1.9835*float(mthday(im)), y2*fx
	  endif
c 
c               Equation for days in first half month (e.g. 1-15)
	  a=(y1-y2)/(x1-x2)
	  b=y2-a*x2
c 
c               Calculate daily pattern for first half of month
	  j1=0
	  qp(32)=0.0
 
 
	  if(ichk4.eq.1) write(nlog,110)
	  do j=jb1,je1
	    j1=j1+1
	    qp(j1)=a*float(j)+b
	    qp(32)=qp(32)+qp(j1)
	    if(ichk4.eq.1) then
	      write(nlog,120) mon, mt, m2, j, j1, a, b,qp(j1),qp(32)
	    endif
	  end do
c
c ---------------------------------------------------------------
c               Begin calculations for days in last half of month
c 
	  jb2=je1+1
	  je2=jb1+mt-1

	  x1=x2
	  x2=float(je2)+float(mthday(ip))/2.0  

	  y1=qm(i,is)
	  y2=qm(ip,is)

	  if(ichk4.eq.1) then
	    write(nlog,*) '  im,  is', im, is
	    write(nlog,*) ' jb2, je2', jb2, je2
	    write(nlog,*) '  x1,  x2', x1, x2
	    write(nlog,*) '  y1,  y2', y1, y2
	    write(nlog,*) '  y1,  y2', 
     1        y1*fx, y2*1.9835*float(mthday(ip))
	  endif
c
c               Equation for days in last half of month
	  a=(y1-y2)/(x1-x2)
	  b=y2-a*x2
c
c               Calculate daily pattern for days last half of month
	  j1=m2

	  if(ichk4.eq.1) write(nlog,110)
	  do j=jb2,je2
	    j1=j1+1
	    qp(j1)=a*float(j)+b
	    qp(32)=qp(32)+qp(j1)
	    if(ichk4.eq.1) then
	      write(nlog,120) mon, mt, m2, j, j1, a, b,qp(j1),qp(32)
	    endif
	  end do

c
c               Change total daily to average
          qp(32)=qp(32)/float(imd)
c
c               Distribute pattern to monthly total
c               Note approach preserves total but some discontinuity
c               at end points
c	    
c	    
c ---------------------------------------------------------
c	 	Set monthly factor. 
          if(abs(qp(32)).lt.small) then
            c=0.0
          else
            c=qm(mon,is)/qp(32)      
          endif  
c           
c ---------------------------------------------------------
c		Set negatives to zero unless baseflow
c		diversion (ityp=12)
          if(qp(32).lt.small .and. ityp.ne.12) c=0.0
c           
c ---------------------------------------------------------
c		Detailed output for ichk1
          if(ichk1.eq.1 .or. ichkx.eq.1) then
c
	    write(nlog,182) iday(is), is, cid11, is2, cidx(is2)
	    write(nlog,200) qm(mon,is), qp(32), c,
     1                       qm(mon,is)*fx, qp(32)*fx, c  
	  endif
c           
c ---------------------------------------------------------
c		Detailed output for ichk4
c
	  if(ichk4.eq.1) write(nlog,230) '4-MidPoint  '
	  if(ichk4x.eq.1 .and. (ityp.eq.2 .or. ityp.eq.12)) 
     1	    write(nlog,230) '4-MidPoint  '
c           
c ---------------------------------------------------------
c               Prorate daily to monthly total
          j1=jb1-1
	  qd(32,is)=0.0
	  do id=1,imd
	    qd(id,is)=qp(id)* c
	    qd(32,is)=qd(32,is)+qd(id,is)
c       
c		Summary Output for daily approach (4), simulate 
c		 diversions (2) or baseflow diversions (12)
	    if(ichk4x.eq.1 .and. (ityp.eq.2 .or. ityp.eq.12)) then
	      j1=j1+1
	      write(nlog,240) cid1(is), iyr, monx, id, j1,
     1          qm(mon,is),qp(id),qd(id,is)
	    endif
	  end do
        
	  qd(32,is)=qd(32,is)/float(imd)
c
c		Print annual total        
	  if(ichk4.eq.1 .and. (ityp.eq.2 .or. ityp.eq.12)) then
	      write(nlog,250) cid1(is), iyr, 13, -1, -1,
     1          qm(mon,is)*fx,qp(32)*fx,qd(32,is)*fx
	  endif
        
	  ifound=1
	  goto 100              
        end if
c
c _________________________________________________________
c               Step 2f; Type 5 Connecting data at the end of month
c                        Useful for targets

c                        Note qm and qdx(32,is2) are average daily 
c                        (cfs)
	if(iday(is).eq.5) then
	  cidy1=cidy(is)
	  c=1.0
c
c               Set miscellaneous month ID constants
	    im=mon-1
	    mt=mthday(mon)
c
c               Get beginning and ending days for this month
	    jb1=0
	    do i=1,im
	      jb1=jb1+mthday(i)
	    end do
c                                                    
c               Set day loop counters
	    jb1=jb1+1
	    je1=jb1+mt - 1             
c
c               Begin calculations
	    x1=float(jb1) - 1.0
	    x2=float(jb1) + mt - 1.0
c
c rrb 2006/10/23; Handle month previous better
c               Reset month-1 & month+1 for begining and ending months
	    if(im.le.0) then
              im=amax0(1, mon-1)
              call getEomX(is, ceom, cid1(is))
              y1=ceom
	      y2=qm(i,is)
	    else
              y1=qm(im,is)
	      y2=qm(i,is)
	    endif

	    if(ichk5x.eq.1 .and. cid1(is).eq.cchk) then
	      write(nlog,186) cid1(is), im, mthday(im), 
     1          mon, mthday(mon), jb1, je1, x1, x2, y1, y2
     
	    endif
c
c               Equation for entire month (e.g. 1-30)
	    a=(y1-y2)/(x1-x2)
	    b=y2-a*x2
c
c               Calculate daily pattern for entire month
	    j1=0
	    qp(32)=0.0


	    if(ichk5.eq.1) write(nlog,110)
	    do j=jb1,je1
	      j1=j1+1
	      qp(j1)=a*float(j)+b
c
c		Total if not a reservoir	      
              if(ityp.eq.4 .or. ityp.eq.14) then
	        qp(32)=qp(j1)
              else	      
	        qp(32)=qp(32)+qp(j1)
	      endif
	      
	      if(ichk5.eq.1) then
		write(nlog,120) mon, mt, m2, j, j1, a, b,qp(j1),qp(32)
	      endif
	    end do
c
c
c               Note since Targets; no need to 
c               Change total daily to average 
c               Distribute pattern to monthly total
c         qp(32)=qp(32)/float(imd)
c
c               Prorate daily to monthly total
          if(ichk5x.eq.1 .and. cid1(is).eq.cchk) 
     1     write(nlog,230) '5-EndPoint  '

	  j1=jb1-1
	  qd(32,is)=0.0
	  do id=1,imd
	    qd(id,is)=qp(id)* c
	    qd(32,is)=qd(32,is)+qd(id,is)

	    if(ichk5x.eq.1 .and. (ityp.eq.4 .or. ityp.eq.14)) then
	      if(cid1(is).eq.cchk) then
	        j1=j1+1
	        write(nlog,240) cid1(is), iyr, monx, id, j1,
     1	          qm(mon,is),qp(id),qd(id,is)
              endif
	    endif
	  end do

	  qd(32,is)=qd(32,is)/float(imd)
c
c		Print annual total (acft)
c		Note type 4 and 14 are resevoirs, so in ac-ft
	  if(ichk5.eq.1 .and. (ityp.eq.4 .or. ityp.eq.14)) then
	      write(nlog,250) cid1(is), iyr, 13, -1, -1,
     1          qm(mon,is),qp(32),qd(32,is)
	  endif

	  ifound=1
	  goto 100              
	end if
c
c _________________________________________________________
c               Step 3; Set monthly average into daily array 
c                       position 32
 100    qmx=0.0

	do id=1,imd
	  qmx=qmx+qd(id,is)
	end do
	qd(32,is)=qmx/float(imd)
c
c rrb 98/02/16; Revise monthly total;
c               Daily sum should be the same as mo total unless type 5
	if(iday(is).ne.5) then
	  qm(mon,is)=qd(32,is)              
	endif
c
c _________________________________________________________
c               Step 4; Process problems 
	if(ifound.eq.0) then
	  write(nlog,170) iyr, monx, dtype(ityp), 
     1                    is, cidy(is), cidy(is)
	  do i=1,n
	    cidz=cidx(i)
	    if(cidz(1:1).eq.' ') then
	    else
	      write(nlog,'(11x, i5, 1x, a12)') i, cidz
	    endif
	  end do
	  goto 9999
	endif
c
c =========================================================
c               End station loop
      end do
c
c _________________________________________________________
c               Step 5; Print daily data
c
c     IF(ityp.eq.11) then     
c     IF(ichk1.eq.1 .or. ityp.eq.4) then
      IF(ichk1.eq.1 .or. ichkx.eq.1) then   
        write(nlog,*) ' DayDist; Detailed data for: ', ityp, dtype(ityp)
	WRITE(nlog,150) dtype(ityp), (i, i=1,31)
	do is=1,n
	  WRITE(nlog,160) is, cid1(is), iday(is), iyr, monx, 
     1        (qd(i,is), i=1,31), qd(32,is), qd(32,is)*fx
	end do
      endif

c
c _________________________________________________________
c               Step 6; Return
      return
c
c _________________________________________________________
c               Formats
 110        format(/,'Daydist;', /, 
     1        '  mon   mt   m2    j   j1',       
     1        '           a           b      qp(j1)      qp(32)',/
     1        5(' ____'), 4(' ___________'))
 120          format(5i5, 20f12.2)

 140  FORMAT(/, 60('_'),/,'  Daydist; ', a55)
 150  FORMAT(/, '  Daydist; ', a55,/
     1          '    # ID            Type   YR  MON', 31i10,
     1          '   Ave-cfs  Total-af')
 160  FORMAT(i5, 1x, a12, 1x, 3i5, 31f10.2, f10.2, f10.0)
 170  format(/,
     1 '  Daydist; Problem in year, month ', 2i5, ' data type ', a55,/
     1 '           Station ', i5, ' uses Daily Id ', a12, /
     1 '           but it cannot find that ID for this year and',/
     1 '           month in the daily data file',//
     1 '           For Station = ', a12,/
     1 '           Available Daily distribution data is:')         
     
 180  format(/,'  Daydist; Type ',i5,' Estimating # ',i5,1x, a12,
     1         ' to Average.',
     1                 ' Monthly data controls.')
 182  format(/,'  Daydist; Type ',i5,' Estimating # ',i5,1x, a12,
     1         ' using # ',i5,1x, a12,'.',
     1                 ' Monthly data controls.')
 183  format(/,'  Daydist; Type ',i5,' Estimating # ',i5,1x, a12,
     1         ' using # ',i5,1x, a12,'.',
     1                 ' Daily data controls.')
 184  format(/,'  Daydist; Type ',i5,' Estimating # ',i5,1x, a12,
     1         ' to Total/days.',
     1                 ' Monthly data controls.')
 186  format(/,60('_'),/
     1  ' DayDist; cid1(is) = ',a12,/
     1  '          im  = ',i10,   ' mthday = ', i10,/
     1  '          mon = ',i10,   ' mthday = ', i10,/
     1  '          jb1 = ',i10,   ' je1    = ', i10,/
     1  '          x1  = ',f10.2, ' x2     = ', f10.2,/
     1  '          y1  = ',f10.2, ' y2     = ', f10.2)

 200   format(
     1 '  Daydist;             qm        qdx(32)     qm/qdx(32)',/, 
     1 '           ______________ ______________ ______________',/,
     1 2x, 'cfs' 5x, 3f15.2,/
     1 2x, ' af' 5x, 3f15.2)
c    1 10x, 3f15.2,/10x,3f15.2)  
 210   format('  Daydist; ityp, id, is, is2, qd(id,is)',
     1         4i5, f8.2)
 220   format('  Daydist; is, iday(is)', 2i5)
 230   format(/,'  Daydist; ',a12,/
     1  '                                          ',
     1  '  Cum       Ave     Daily     Daily',/
     1  '  Daydist     ID            Year  Mon  day',
     1  '  Day     Month   Pattern     Value',/
     1  ' ____________ ____________  ____ ____ ____',
     1  ' ____ _________ _________ _________')
 240   format('  DayDist cfs', 1x,a12,1x,4i5, 20f10.2)
 250   format('  DayDist af ', 1x,a12,1x,4i5, 20f10.2)

 9999 write(6,*)  '  Stopped in Daydist, see log file (*.log)'
      write(nlog,*) '  Stopped in Daydist'
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop 
c
      end


