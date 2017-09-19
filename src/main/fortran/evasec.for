
C
      SUBROUTINE EVASEC
c
c
c _________________________________________________________
c	Program Description
c
c       Evasec; It calculates net evap to be:
c               area (initial + final) / 2 * evap.
c
c               Called by: Execut and Vircom
c
c !!!!		Called just once per time step; after final reservoir
c		storage	is known; else it will keep evaporating each
c		time called
c
c               Note: +evap= more evap than rain
c                     -evap= more rain than evap
c
c               It distributes to accounts as follows:
c               pcteva(ir) 
c                     = +n by % provided
c                     = 0 prorate based on current ownership
c                     =-1 no evap to this account
c               Note if the proration fails, then excess evap
c                     is distributed to the last account first
c
c               Approach:
c               For every reservoir (nr)
c                 1. Calculate total evaporation in feet then acft
c                    (evap0)
c                 2. Average initial and final storage to get net
c                    evap in af (evap1)
c                 3. Limit evap to the toal reservoir by
c                    comparing to accounts that participate using the
c                    variable (pcteva(ir))
c                 4. Distribute to accounts
c                 5. Check if OK done
c                 6. If not OK, distribute remaining evap to
c                    last account first
c                 7. Check and return if OK or stop if a problem
c
c               Note checks in Datinp are critical to insure there
c               is not mix of percentage and 0 (porportional) data
c               provided
c                 
c
c               iout = 0 no detailed printout
c                    = n print detailed infor for reservoir n
c
c               Note since daily evap data is not accomodated
c               only impact between daily and monthly is to 
c               adjust by number of days in month (cd)
c
c _________________________________________________________
c	Update History
c
c rrb 01/01/03; Revised to recognize other baseflow options 
c               Types 1,& 9
c rrb 01/08/03; (9.91) Revised to correct problem when more rain 
c               than evap.  Problem was using variable sum not volmax
c rrb 01/09/25; (9.97) Revised to correct problem when reservoir is full
c
c _________________________________________________________
c       Documentation               
c
c               cursto(nr)      Final storage in reservoir 
c               cursto0         Initial storage in reservoir
c
c               dumx(ir)        Initial storage in account ir
c               curown(ir)      Final storage in account ir

c               ownmax(ir)      Max storage in account ir
c               accr(23,ir)     Evaporation from account ir
c
c               pcteva(ir)      Percent of evap to account ir
c
c               evap1           Initial evaporation (area*ev*ppt)
c               evap2           Evap after adjusting for account size
c               evap(nr)        Final evaporation (af)
c
c _________________________________________________________
c	Dimensions
      include 'common.inc'                      
c                           
c _________________________________________________________
c               Step 1; Initilize
      iout = 0
c     iout = 1
      if(ichk.eq.11) iout=1
      nout=0
      small=0.1
      small2=0.1
c
c rrb 2006/04/11; Revise      
cx      small=0.001
cx      small2=0.001
      
      icase=0
c
c rrb 01/01/03; Recognize other baseflow modes
      if(ioptio.eq.1 .or. ioptio.eq.9) then 
	ibasef=1
      else
	ibasef=0
      endif

c
c _________________________________________________________
c
c               Step 2: Set Daily data switches
c rrb 98/02/17; Daily option
c rrb 98/06/11; Problem c gets redefined later
c rrb 01/03/25; Daily baseflow option
c     cd=1.0
c     if(iday.eq.1 .and. ibasef.ne.1) cd=mthday(mon)

      if(iday.eq.0) then
	cd=1.0
      else
	cd=mthday(mon)
      endif
c
c _________________________________________________________
c
c               Step 3; Begin reservoir loop 
c
      do 190 nr=1,numres
c
	if(iressw(nr).eq.0) go to 190
c
c
c _________________________________________________________
c         
c               Step 4; Check reservoir accounts coming into routine
	if(ibasef.eq.0) then
          call chekres(nlog,maxres, 0,16, iyr, mon, nr, nowner, 
     1         curown, cursto, cresid)
	endif
c
c _________________________________________________________
c
c               Step 5; Calculate surface area for initial 
c                       and current storage
c
	nra=nrange(nr)                                   
c               BOM area
c
        call interp(conten(1,nr),suarea(1,nr),volint(nr),area1,nra,
     1              1, nr, cresid(nr))
c                           
c               EOM area
        call interp(conten(1,nr),suarea(1,nr),cursto(nr),area2,nra,
     1              1, nr, cresid(nr))                                
c
c _________________________________________________________
c
c               Step 6; Evaporation Calculations
c
	evap(nr)=0.
c
	ini=nevapo(nr)
	ine=nevapo(nr+1)-1
c       write(nlog,*) ' Evasec; nr, ini, ine', nr, ini, ine
c
c ---------------------------------------------------------
c
c               Step 6a; Loop for all evaporation stations

	do 110 in=ini,ine
c
c rrb 03/14/95; Code Addition, Variable evap id
	  ifind = 0
	  do icod=1,numeva
	    if (cevar(in).eq.cevaid(icod)) then
c             write(nlog,*) in, icod, nr, weigev(in), evaprt(mon,icod)
	      evap(nr)=evap(nr)+weigev(in)*evaprt(mon,icod)/100./cd
	      ifind = 1
	    endif
	  end do
	  if(ifind.eq.0) goto 200
  110   continue
C
	evap0 = evap(nr)
c
c _________________________________________________________
c
c               Step 7; Precipitation Calculations
c
	if(numpre.gt.0) then
	  rain=0.
	  ini=nprecp(nr)
	  ine=nprecp(nr+1)-1
c
c ---------------------------------------------------------
c
c               Step 7a; Loop for all precipitation stations

	  do 130 in=ini,ine
	    ifind = 0
	    do icod=1,numpre
	      if (cprer(in).eq.cpreid(icod)) then
		rain=rain+weigpr(in)*preprt(mon,icod)/100./cd
		rain0=rain
		ifind = 1
	      endif
	    END do
c
	    if(ifind.eq.0) goto 200
  130     continue
c
c         write(nlog,*) '  Evasec 1; mon, nr, evap(nr), rain'
c         write(nlog,*) '  Evasec 1;', mon, nr, evap(nr), rain
c
c ---------------------------------------------------------
c
c               Step 7b; Calculate net evaporation (ft)
c
	  evap(nr)=evap(nr)-rain
	endif
c          
c _________________________________________________________
c
c               Step 8; Calcualte Ave Area and Net Evaporation (ac-ft)
c
	area0=0.5*(area1+area2)
	evap(nr)=0.5*(area1+area2)*evap(nr)
	evap1=evap(nr)
c
c
c _________________________________________________________
c
c               Step 9; If in a baseflow mode (ioptio=1) branch
c
c rrb 01/01/03;  Recognize other baseflow modes 
	if(ibasef.eq.1) goto 170
c
c _________________________________________________________
c
c               Step 10; Distribute Evap to Total Reservoir
c
c rrb 96/05/21; Limit evap to accounts that are assigned
	iri=nowner(nr)
	ire=nowner(nr+1)-1
c
c ---------------------------------------------------------
c
c               Step 10a;  Calculate total of participating accounts 
c                          storage (sum) and available storage (sumx)
c
	sum=0.0
	sumx=0.0
	do ir=iri,ire                           
          dumx(ir) = curown(ir)        
c        
c rrb 01/08/21; Note >= since 0 means distribute to all accounts
c               porportional to their current storage
c         if(ifix(pcteva(ir)).ge.0)  sum=sum+amax1(0.,curown(ir))
	  if(ifix(pcteva(ir)).ge.0) then 
	    curown(ir) = amax1(0.0, curown(ir))
	    curown(ir) = amin1(curown(ir), ownmax(ir))
            dumx(ir) = curown(ir)

	    sum=sum+curown(ir)      
	    sumx=sumx+ownmax(ir)-curown(ir)
	  endif
	end do
c
c ---------------------------------------------------------
c
c               Step 10b; Limit evap to available storage (sum)
c                         and space (sumx)
c rrb 01/08/21; Limit total evaporation to current storage or space
c               available in accounts receiving evap
c       write(nlog,*) '  Evasec; evap(nr), sum, sumx',evap(nr),sum,sumx
c
	if(evap(nr).ge.0.0) then
	  evap(nr)=amin1(evap(nr), sum)
	else
	  evap(nr)=amax1(evap(nr), -1.0*sumx)
	endif
	evap2=evap(nr)
c
c ---------------------------------------------------------
c
c               Step 10c; Exit if no evaporation
c      
c       write(nlog,*) '  Evasec; abs(evap(nr))', abs(evap(nr))
	if(abs(evap(nr)).le.small) then
	  icase = 1
	  goto 170
	endif
c
c ---------------------------------------------------------
c
c
c               Step 10d; Exit if no storage in accounts
c                         and evap is positive
c rrb 01/09/25; Correction
c       if(sum.le.small) then
	if(sum.le.small .and. evap(nr).gt.small) then

	  evap(nr) = 0.0
	  icase = 2
	  goto 170
	endif
c
c
c ---------------------------------------------------------
c
c               Step 10e; Exit if no capacity in accounts
c                         and evap is negative
c rrb 01/09/25; Correction
c       if(sumx.le.small) then
	if(sumx.le.small .and. evap(nr).lt.small) then
	  evap(nr) = 0.0
	  icase = 3
	  goto 170
	endif

c
c ---------------------------------------------------------
c
c               Step 10f; Finally adjust total reservoir

	cursto0=cursto(nr)
	ev=evap(nr)
	cursto(nr)=cursto(nr)-ev
c
c ---------------------------------------------------------
c               
c               Step 10g; Check too much evap
	if(cursto(nr).lt.0.001) then
	  ev=ev+cursto(nr)
	  evap(nr)=ev
	  cursto(nr)=0.              
	endif
c
c ---------------------------------------------------------
c               
c               Step 10h; Check too much ppt (negative evap)
c
c rrb 01/08/03; Problem limited to amount in accounts
c               Effectively said PPt > Evap = 0
c               Check too much rain to reservoir
c       if(cursto(nr).gt.sum) then
c         ev=ev+cursto(nr)-sum
c         evap(nr)=ev
c         cursto(nr)=sum
c       endif
c
	if(cursto(nr).gt.volmax(nr)) then
	  ev=ev+cursto(nr)-volmax(nr)
	  evap(nr)=ev
	  cursto(nr)=volmax(nr)
	endif
	evap3=evap(nr)
c
c _________________________________________________________
c
c                                                  
c               Step 11; Distribute net evaporation to accounts
c        
        ev=evap(nr)
	tev=ev
	icase=4
	do 150 ir=iri,ire       
	  accr(24,ir) = 0.0
c
c               Note < since 0 implies distribute porportinally
	  if(ifix(pcteva(ir)).lt.0) goto 150
c
c               Calculate percent to this account.
c               Note if percent is 0; evap is distributed
c               porportional to ownership
	  if(pcteva(ir).gt.small) then
	    c=tev*pcteva(ir)/100.0 
	  else
	    c=tev*amax1(0.0, (curown(ir)/sum))
	  endif                  
c
c rrb 05/20/97
c         if(c.le.small) goto 150 
	  if(abs(c).le.small) goto 150

	  curown(ir)=curown(ir)-c
c
c ---------------------------------------------------------
c               
c               Step 11a; Too much Evap
c
	  if(curown(ir).le.small) then
	    c=c+curown(ir)
	    curown(ir)=0.
	  endif                 
c
c ---------------------------------------------------------
c               
c               Step 11b; Too much Rain
	  if(curown(ir).gt.ownmax(ir)) then
	    c=c+curown(ir)-ownmax(ir)
	    curown(ir)=ownmax(ir)
	  endif
	  
          ev=ev-c
	  accr(24,ir) = c
  150   continue
c
c ---------------------------------------------------------
c
c               Step 11c; If all evap is distributed exit
c
c       write(nlog,*) '  Evasec; abs(c) after 150', abs(ev) 
	if(abs(ev).le.small) then
	  go to 170
	endif
c
c _________________________________________________________
c
c              Step 12; Distribute any remaining evap sequentially 
c              from the last account backwards
	icase=5
	irr=ire+1
        if(nr.eq.nout) write(nlog,*) '  Evasec 0; ev = ', ev
	do 160 ir=iri,ire
	  irr=irr-1
c
c               Note < since 0 implies not to this account
	  if(ifix(pcteva(irr)).lt.0) goto 160

	  curown(irr)=curown(irr)-ev             
c
c ---------------------------------------------------------
c               
c               Step 12a; If done exit
c
            c2=curown(irr)-ownmax(irr)
            if(iout.eq.nr) 
     1        write(nlog,*) '  Evasec; irr, ev, c2', irr, ev, c2
     
            if(curown(irr).gt.-small.and.c2.le.small2) then
              accr(24,irr) = accr(24,irr)+ev
  	      ev=0.0
  	      icase=6
  	      goto 161
  	    endif                    
c
c ---------------------------------------------------------
c               
c               Step 12b; Too much Evap
c
	  if(curown(irr).lt.-small) then
	    accr(24,irr) = accr(24,irr)+curown(irr)+ev
	    ev=-curown(irr)
	    curown(irr)=0.
	  endif                 
c
c ---------------------------------------------------------
c               Step 12c; Too much Rain
c
	  if(curown(irr).gt.ownmax(irr)) then
	    ev1=ev
	    cr1=curown(irr)-ownmax(irr)+ev
	    accr(24,irr) = accr(24,irr)+curown(irr)-ownmax(irr)+ev
	    ev=ownmax(irr)-curown(irr)
	    curown(irr)=ownmax(irr)
	    if(iout.eq.nr) write(nlog,*) ' Evasec; irr, ev1, cr1, ev',
     1         irr, ev1, cr1, ev
	  endif
c
  160     continue
c
c ---------------------------------------------------------
c
c               Step 12c; Check for a problem with sequential
c                         allocation
 161        if(abs(ev).gt.small) then
	    write(nlog,*) ' '
            write(nlog,*) '  Evasec; Problem closure term (ev) = ', ev
            write(nlog,240) 
            write(nlog,242) nr, cresid(nr), iyr, mon, nr, icase,
     1        evap0, rain0, area0, evap1,evap2, evap3, evap(nr),sum,
     1        cursto0,  cursto(nr),ev, small
	    t1=0
	    t2=0
	    t3=0
	    t4=0
	    t5=0
	    t6=0

	    write(nlog,250)
	    do 162 ir=iri,ire                                       
	      write(nlog,260) iyr, mon, ir, icase,
     1          evap1,evap2,evap3, evap(nr),ownmax(ir),dumx(ir),
     1          curown(ir),pcteva(ir), accr(24,ir)     
              t1=evap(nr)
              t2=t2+ownmax(ir)
              t3=t3+dumx(ir)
              t4=t4+curown(ir)
              t5=t5+pcteva(ir)
              t6=t6+accr(24,ir)
  162       continue
	    write(nlog,261) iyr, mon,'Total', icase,
     1        evap1,evap2, evap3, t1, t2, t3, t4, t5, t6
	     goto 280
	  endif
c
c
c _________________________________________________________
c
c               Step 13; Check results
  170   if(iout.eq.1) then 
          if(nr.eq.1 .and. mon.eq.1) then
            write(nlog,*) '  Evasec; Detailed Evap Data for ichk = ',
     1      ichk
	    write(nlog,240)
	  endif
          write(nlog,242) nr, cresid(nr), iyr, mon, nr, icase,
     1      evap0, rain0, area0, evap1,evap2, evap3, evap(nr),sum,
     1      cursto0,cursto(nr), ev, small
c
c               Account data if not in baseflow mode
c               Note no account information when in baseflow mode
	  if(ibasef.eq.0) then
	    write(nlog,250)
	    t1=0
	    t2=0
	    t3=0
	    t4=0
	    t5=0
	    t6=0
	    do ir=iri,ire                                       
	      write(nlog,260) iyr, mon,ir, icase,
     1          evap1,evap2, evap3, evap(nr),ownmax(ir),
     1          dumx(ir), curown(ir),pcteva(ir), accr(24,ir)
                t1=t1+evap(nr)
                t2=t2+ownmax(ir)
                t3=t3+dumx(ir)
                t4=t4+curown(ir)
                t5=t5+pcteva(ir)
                t6=t6+accr(24,ir)
	    end do
	    write(nlog,261) iyr, mon,'Total', icase,
     1        evap1,evap2, evap3, t1, t2, t3, t4, t5, t6
	  endif
	endif                              
c
c
c _________________________________________________________
c
c               Step 14; Check reservoir accounts if not in baseflow
      if(ibasef.eq.0) then
        call chekres(nlog,maxres, 1,16, iyr, mon, nr, nowner, 
     1         curown, cursto, cresid)
      endif
c
c ---------------------------------------------------------
c
c               End Reservoir Loop
c
  190 continue
c
c _________________________________________________________
c
c               Step 15; Return
c
      return                               
c
c
c _________________________________________________________
c
c               Error Messages
c
  200 write(6,270) cresid(nr), cevar(in), cprer(in)
      write(nlog,270) cresid(nr), cevar(in), cprer(in)
c
      do 210 icod=1,numeva
  210   write(nlog,'(2(1x,a12))') cevar(in), cevaid(icod)
c
      do 220 icod=1,numpre
  220   write(nlog,'(2(1x,a12))') cprer(in), cpreid(icod)
      goto 280

c
c
c _________________________________________________________
c
c               Formats
c
   90 format(4i5,20f10.4)

  240 format(/,
     1  '  Evasec;                             ',       
     1  '       evap0       rain0       area0',
     1  '       evap1       evap2       evap3    evap(nr)         sum',
     1  '     cursto0      cursto          ev       small',/
     1  '   nr ID            iyr  mon Res# icase', 
     1  '         ft          ft          ac',
     1  '        acft        acft        acft        acft',
     1  '        acft        acft        acft       acft',/
     1  ' ____ ____________ ____ ____ ____ ____',
     1  12(' ___________'))
  242 format(i5, 1x, a12, 4i5, 2f12.3, 20f12.1)


  250 format(/,
     1  '  Evasec;  iyr  mon  ir icase',       
     1  '       evap1       evap2       evap3    evap(nr)      ownmax',
     1  '        dumx      curown      pcteva accr(24,ir)',/
     1  ' ________ ____ ____ ____ ____', 8(' ___________'))

  260 format(9x,4i5,4f12.3, 20f12.1)
  261 format(9x,2i5,a5,i5, 4f12.3, 20f12.1)

  270 format(' Evasec; Cannot find Evap or Ppt Id for reservoir ', a12,
     1       '         Evap id = ',a12,/
     1       '         Ppt id  = ', a12)
c
c _________________________________________________________
c
c               Step 16; Error Processing
c
  280 write(6,*) ' Stopped in Evasec, see the log file (*.log)'
      write(nlog,*)'  Stopped in Evasec'
      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)


      stop 
      END




