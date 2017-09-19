C
c *********************************************************
c
      SUBROUTINE Intersup(iw,l2,i12)
c                                                             
c
c _________________________________________________________
c	Program Description
c
c       Type 35;  It calculates an interruptable supply by
c                 allowing an instream flow (nf) to divert water that
c                 would have been divertd by a direct flow right (isr)
c                 based on the natural flow at a stream station (isx)
c
c _________________________________________________________
c       Documentation
c
c               iw  = right counter
c               l2  = operation right counter
c               i12 = switch used to check discharge switch once per
c                   = 0 called from bomsec to compare to natural flow
c                   = 1 called from execut for operation
c
c               nf  = iopdes(1,l2) Destination instream flow fr riginp
c               isx = iopsou(1,l2) Stream ID for interruptable switch
c               isr = iopsou(2,l2) Source water right fr riginp
c                     iopsou(3,l2) Stream discharge for on/off switch
c                     iopsou(4,l2) Switch to allow interruptable
c                                  transfer of diversion (0) or CU (-1)
c               divact          = resulting instream flow diversion
c               imonsw(l2,mon)  = monthly on/off switch
c
c               flowrq          = isf demand 
c               divo            = total diversion under this opr rule
c               qdiv(23, )      = exchange from an non reservoir
c                                 (e.g. interruptable supply).
c               divd            = total diversin by a right ???
c
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
      character cwhy*48
c
c
c _________________________________________________________
c
c               Step 1; Initilization - General
c
c               a. Convergence 
      small = 0.001
c
c               b. Daily capability
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif
c
c               c. Miscellaneous
      icheck  = 0
      ierror  = 0
      divact  = 0.0
      iw      = iw
c
c _________________________________________________________
c               Step 2; Set destination and source data
c               a. Instream flow destination ID (nf) and
c                  location on river (ifcd)
      nf   = iopdes(1,l2)
      ifcd = ifrsta(nf)
c
c               b. Get the source
c                  location on the river (isx) used as the
c                  on/off switch
      isx=iopsou(1,l2)
c
c               c. Get the source 
c                  water right  ID (isr)
c                  structure ID (isd)
c                  user (iuse) 
c                  location on river (iscd)
c                  number of downstream nodes (ndns)
c                  turn off diversion structure (idivsw) from this point
c                  foreward
      isr=iopsou(2,l2)
      isd=idivco(1,isr)
c
c               Note user 1 is assumed since user account
c               traditional user account (iopsou(4,l2) is used
c               to indicate diversion (0) or CU (-1)
c     iuse=nduser(isd)+idivco(2,l2)-1
      iuse=nduser(isd)
      iscd=idvsta(isd)
      ndns=ndnnod(iscd)
c
c               On/Off switch for structure with water right
c     idivsw(isd)=0

      if(icheck.ne.0) then
        write(99,*) ' ' 
        write(99,*) '  Intersup; l2                  = ', l2
        write(99,*) '  Intersup; nf                  = ', nf
        write(99,*) '  Intersup; isx  (iopsou(1,l2)  = ', isx
        write(99,*) '  Intersup; isr  (iopsou(2,l2)  = ', isr
        write(99,*) '  Intersup; isd  (idivco(1,ndr) = ', isd 
        write(99,*) '  Intersup; iscd (idvsta(nd1)   = ', iscd
      endif
c
c               d. stuff required for testing
      dcrdiv1 = dcrdiv(isr)
      divo1   = divo(l2)
      flowrq1 = flowrq(nf)
      aloifr1 = 0.0
      alodiv  = 0.0
      alodiv1 = 0.0
      divalo1 = 0.0
      pavail1 = 0.0
      divact1 = 0.0
c
c _________________________________________________________
c
c               Step 3: Check if on this month
      mon2 = imonsw(l2,mon)
c
c _________________________________________________________
c		Step X; On/Off Switch      
c
c rrb 06/01/18; Allow daily on/off switch
      if(imonsw(l2,mon).eq.0) then
        iwhy=1
        cwhy='Monthly switch Off'
        goto 100
      endif  
c
c ---------------------------------------------------------
c		For a daily model set demand for beginning of season
      if(iday.eq.1 .and. imonsw(l2,mon).gt.0) then
        if (idy.lt.imonsw(l2,mon)) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 100
        endif  
      endif  
c
c ---------------------------------------------------------
c		For a daily model set demand for end of season
      if(iday.eq.1 .and. imonsw(l2,mon).lt.0) then
        if (idy.gt.iabs(imonsw(l2,mon))) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 100
        endif  
      endif  
      
      
c
c _________________________________________________________
c
c               Step 4: Check if this is an interruptable year
c               Note i12 insures this check is done 1x/yr from bomsec
c               for the first month when an inter. supply might occurr
c               ioprsw(l2) is the opr rule on/off switch used in execut
c               idvrsw(isr) is the right on/off switch used in execut
      if(i12.eq.0) then
        if(icheck.eq.1) then
          write(99,*) '  Intersup;',dumsta(isx)*fac,float(iopsou(3,l2))
          write(99,*) '  Intersup; isx = cstaid(isx)', isx, cstaid(isx)
        endif
c
c rrb 99/07/03; River has return flows, dumsta (bomsec.f) does not
c       if(river(isx).lt.float(iopsou(3,l2))/fac) then
        if(dumsta(isx).lt.float(iopsou(3,l2))/fac) then
          ioprsw(l2) = iyr
          idvrsw(isr) = -999
        else
          ioprsw(l2)=iyr+1
          idvrsw(isr) = iyr
        endif
        goto 100
      endif
c
c _________________________________________________________
c
c               Step 5; Calculate demand for ISF
      aloifr = flowrq(nf)
      if(aloifr.lt.small) goto 100
c
c _________________________________________________________
c
c               Step 6; Calcualte water right remaining
      divalo = dcrdiv(isr)-divd(isr)
c
c _________________________________________________________
c
c               Step 7; Calcualte demand at water right location
c                       as remaining demand
c                       note demopr = divreq = diversion under
c                       this structure from bomsec.f
      alodiv1=demopr(iuse)
      if(iopsou(4,l2).ne.-1) then
c       alodiv = divreq(iuse) - divo(l2)
        alodiv = demopr(iuse)
      else
c       alodiv = divreq(iuse) - divo(l2)*(1.0+diveff(mon,isd)/100.0)
        alodiv = demopr(iuse)*(diveff(mon,isd)/100.0)
      endif
c
c _________________________________________________________
c
c               Step 8; Find downstream minimum flow
c                       available to the diversion source
c                       station (imcd) and amount avtemp(imcd)
      do is=1,numsta
        avtemp(is)=avail(is)
      end do

      call dnmfso(maxsta,avtemp,idncod,iscd,ndns,imcd)
      pavail=avtemp(imcd)
      pavail=amax1(0.0, pavail)
c
c _________________________________________________________
c
c               Step 9; Set diversion to water right (divwr) to min(
c                       diversion demand (alodiv),
c                       water right (divalo), &
c                       flow available (pavail)
c                       Adjust if this OPR rule allows the transfer of
c                       diversion (iopsou(4,l2) = 0)
c                       or CU (iopsou(4,l2) = -1)

      divwr=amin1(alodiv, divalo, pavail)
c
c rrb 99/07/06
c     if(iopsou(4,l2).eq.-1) divwr = divwr / diveff(mon,isd)/100.0
c
c _________________________________________________________
c
c               Step 10;Set actual diversion (divact) to min(
c                       instream demand (aloifr) and
c                       diversion to water right (divwr)
      divact=amin1(aloifr, divwr)
      if(divact.lt.small) goto 100
c
c _________________________________________________________
c
c               Step 11; Update
c               a. Adjust avaialble flow at ISF
      avail(ifcd) = avail(ifcd) - divact
c
c               b. Adjust ISF demand
      flowrq(nf) = flowrq(nf) - divact
c
c               c. Adjsut ISF demand by reach.
c                  Note; this allows interruptable
c                  supply to give correct results with or without
c                  ISF by reach although the interruptable supply
c                  is currently tied to a point only
      ib=ndnifb(nf)
      ie=ndnifb(nf) + ndnifs(nf) - 1
      do i=ib,ie
        florqr(i) = amax1(0.0, florqr(i) - divact)
      end do
c
c               d. Adjust diversion demand
c
c rrb 99/07/01; Do not do this to keep shortage correct for
c               source structure.  See step 7, adjustement 
c               to diversion demand
      if(iopsou(4,l2).ne.-1) then
c       divreq(iuse)=divreq(iuse)-divact
        demopr(iuse)=demopr(iuse)-divact
      else
c       divreq(iuse)=divreq(iuse)-divact*(1.+diveff(mon,isd)/100.0)
        demopr(iuse)=demopr(iuse)-divact/(diveff(mon,isd)/100.0)
      endif
c
c               e. Adjust water right counter
      divd(l2) = divd(l2) + divact
c
c               f. Adjust diversion under this OPR rule
      divo(l2) = divo(l2) + divact
c
c               g. Adjust qdiv(23, ) diversion by exchange 
c                  w/o a reservoir
      qdiv(23,ifcd)= qdiv(23,ifcd) + divact

 100  continue
c
c _________________________________________________________
c
c               Step 12; Detailed printout
      if(-iopout.eq.ifcd .or. icheck.ne.0) then
        write(99,110)
        write(99,120) iyr,  i12, mon, mon2, nf, ifcd, isx, isr, iw,
     1    iopsou(4,l2),     xfrnam1(nf),
     1    dcrdiv1*fac,      diveff(mon,isd), divo1*fac,
     1    flowrq1*fac,      aloifr1*fac,     divalo1*fac,
     1    alodiv1*fac,      pavail1*fac,     divact1*fac

        write(99,130) iyr,  i12, mon, mon2, nf, ifcd, isx, isr, iw,
     1    iopsou(4,l2),     xfrnam1(nf),
     1    dcrdiv(isr)*fac,  diveff(mon,isd), divo(l2)*fac,
     1    flowrq(nf)*fac,   aloifr*fac,      divalo*fac,
     1    alodiv*fac,       pavail*fac,      divact*fac
      endif
c
c _________________________________________________________
c
c               Step 13; Return
      if(ierror.ne.0) goto 9999
      return
c
c _________________________________________________________
c               Error warnings
 9999 write(6,150) 
      write(99,150) 
      write(6,*) 'Stop 1'
      call flush (6)
      call exit(1)


      stop 
c
c _________________________________________________________
c
c               Formats
 110    format(/,
     1    '  Intersup',
     1    '                     ISF  LOC  RIV   WR     Div-0',
     1    '                           ',
     1    '  DECREE         OPR RUL Isf_Dem ADJ DEM',
     1    ' REM RIG Div_Dem AVAIL Q  DIVERT',/,
     1    '          ',
     1    '  Yr  i12   Mo  Mo2   nf ifcd  isx  isr   iw CU-1',
     1    ' Name                      ',
     1    '  dcrdiv   Eff %    divo  flowrq  aloifr',
     1    '  divalo  alodiv  pavail  divact',/,9x,
     1    ' ---- ---- ---- ---- ---- ---- ---- ---- ---- ----',
     1    ' ------------------------- ',
     1    9(' -------'))
 120    format(
     1 '  In     ', 10i5, 1x, a24, 2x, 20f8.0)
 130  format(
     1 '  Out    ', 10i5, 1x, a24, 2x, 20f8.0)

 150  format('    Stopped in Intersup',/,
     1       '    See the *.log file')
c
c _________________________________________________________
c
      end

