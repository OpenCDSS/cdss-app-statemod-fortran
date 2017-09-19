c
C
      SUBROUTINE IFRRIGx(IW,L2,ncallx)
c                                                             
c
c _________________________________________________________
c	Program Description
c
c       Ifrrigx; It operates an instream flow as a function
c                of an index station
c                similar to ifrrig.for, but index info included
c
c _________________________________________________________
c	Update History
c
c rrb 2005/10/06; Reivsed so that demands are calculated in Bomsec.
c                 This insures demands are known irregardless of when 
c                 or how the compact demand is operated.
c
c _________________________________________________________
c	Documentation
c
c               iopdes(1,l2)    = Destination instream flow fr riginp
c               iopsou(1,l2)    = Source (index flow station) fr riginp
c               iopsou(2,l2)    = % of index flow fr riginp
c               iopsou(3,l2)    = destination water right
c               actwrq          = resulting instream flow
c               imonsw(l2,mon)  = monthly constraint for riginp 
c
c               flowrq(nf)      = demand at station nf
c		flowrX(im,nf)   = demand for month im, station nf
c				  read from *.ifa or *.ifm
c		flowr(im,nf)    = demand for month im, station nf 
c                                 (calculated in Bomsec)
c               divo            = water right used
c               qdiv            = accounting
c
c		iset            = 0 calculate demands only
c		iset            = 1 try to satisfy demands only
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
      character cwhy*48, cdestyp*12, ccarry*3, cpuse*3      
c
c _________________________________________________________
c       Step 1; Initilize & find station code of i.f.r. right
c
c		            iout = 0 no details
c		                   1 details
c                      2 summary      
      iout=0
      ioutiw=0
      ioutX=0
      
      if(ichk.eq.113) ioutX=1
      if(corid(l2).eq. ccall) ioutiw=iw
c     write(nlog,*) '  Ifrrigx; iout, ioutiw, iw, corid(l2), ccall', 
c    1  iout, ioutiw, iw, corid(l2), ccall
      
      
      iw = iw
      nf  =iopdes(1,l2)
      ifcd=ifrsta(nf)
      isx = iopsou(1,l2)
      isr = iopsou(3,l2)
      actwrq  = 0.0
 
c
c rrb 98/08/10; Convergence Update
      small = 0.001
c
c rrb 98/03/03; Daily capability
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif
      
      iwhy=-1     
      cwhy='N/A'
      cdestyp='N/A'
      ccarry='No'
      cpuse='No'
      availX=-1.      
      riverX=-1./fac
c
c
c _________________________________________________________
c       Step 2; Skip if not opearated this month
c
      mon2 = imonsw(l2,mon)
      if(mon2.eq.0) then
        iwhy=1
        cwhy='Month switch is off'
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
c _________________________________________________________
c       Step 3; Set data for printing 
c
c rrb 2011/12/05       
      cp=float(iopsou(2,l2))/100.0 
      if(cp.gt.small) riverx=flowrq(nf)/cp
      if(iout.eq.1) then
        write(nlog,*) '  IfrrigX; cp, nf, flowrq(nf)*fac, riverx*fac'
        write(nlog,*) '  IfrrigX;', cp, nf, flowrq(nf)*fac, riverx*fac
      endif
        
c
c _________________________________________________________
c       Step 3; Check available decree 
c
c rrb 2005/09/20; Do not used demand from *.ifa or *.ifm
c                 since it is calcualed on teh fly in 
c                 subroutine Bomsec.
cr    aloifr=amin1(dcrifr(isr)-divo(l2),flowrq(nf))
      if(iout.eq.1) then
        write(nlog,*) '  IfrrigX; isr, l2, dcrifr(isr), divo(l2)'
        write(nlog,*) '  IfrrigX;',isr,l2,dcrifr(isr)*fac,divo(l2)*fac
      endif
      aloifr=amax1(dcrifr(isr)-divo(l2),0.0)
c
      if(aloifr.lt.small) then
        iwhy=2
        cwhy='Remaining decree is zero'      
        goto 100
      endif  
c
c _________________________________________________________
c       Step 4; Limit based on demand, decree and available 
c	            	Note flow at the index station is flowrq(nf)
c	            	It is calcualted once in Bomsec to be the flow * cp
c               at the beginning of the time step so it does 
c               not change as the flow at the index station
c               may change with other diversions
c
cx    riverx=river(isx)
cx    avindx = float(iopsou(2,l2)) * riverx / 100.0      
cx      if(iday.eq.0) then
cx        flowrq(nf)=amin1(avindx, flowrX(mon,nf),flowrq(nf))
cx        if(ncallx.eq.0) flowr(mon,nf)= flowrq(nf)
cx      else
cx        write(nlog,*) 
cx     1   '  Ifrrigx; Stopped Daily type 13 (La Plata) must be checked'
cx      endif
      
      avindx = flowrq(nf)
      actwrq=amin1(aloifr,avail(ifcd),flowrq(nf))
c
      if(actwrq.lt.small) then
        iwhy=3
        cwhy='Demand is zero'      
        goto 100
      endif
c
c
c _________________________________________________________
c       Step 5; Appropriation possible, update available flow (avail),
c               demand (flowrq), unmet right (divo), and 
c               accounting (qdiv)
c
      iwhy=0
      avail1  = avail(ifcd)
      flowrq1 = flowrq(nf)
      divo1   = divo(l2)
c
c _________________________________________________________
c       Step 6; Update
c
      avail(ifcd)  = amax1(avail(ifcd)- actwrq, 0.0)
      flowrq(nf)   = amax1(flowrq(nf) - actwrq, 0.0)
      qdiv(14,ifcd)= qdiv(14,ifcd) + actwrq
      
c      
c _________________________________________________________
c
c	Step 7; Sum operating rule data
c
 100  divo(l2)     = divo(l2)      + actwrq
c      
c _________________________________________________________
c
c	Step 7; Instream reach data
c
      if(numifr.gt.0) then
        ib=ndnifb(nf)
        ie=ndnifb(nf) + ndnifs(nf) - 1      
        do i=ib,ie
          florqr(i)=florqr(i)-actwrq
          qdivr1=qdivr(i)
          qdivr(i)=qdivr(i)+actwrq
c         write(nlog,*) ' Ifrrigx;',
c    1     nf, i, actwrq*fac, qdivr1*fac, qdivr(i)*fac
        end do  
      endif  
c
c _________________________________________________________
c       Step 8; Detailed printout
c
c     if(-iopout.eq.ifcd) then
      ncallX=ncallX+1
      if(ioutX.eq.1 .and. iw.eq.ioutiw) then
cr      ncallX=ncallX+1
        if(ncallX.eq.1) then
          write(nlog,270) corid(l2), cdestyp, ccarry, cpuse
        else
cr        write(nlog,*) ' '
        endif  
        
        cp=float(iopsou(2,l2))
          write(99,280) iyrmo(mon),xmonam(mon),idy,
     1      iwx,mon2,nf,ifcd,isx, iopsou(2,l2),
     1      xfrnam1(nf),
     1      dcrifr(isr)*fac, riverx*fac, cp, avindx*fac, flowrq1*fac, 
     1      aloifr*fac,      avail1*fac, actwrq*fac, iwhy, cwhy
      endif
c _________________________________________________________
c       Step 9; Return
c
      return
c
c _________________________________________________________
c       Formats
c
 270   format(/, 
     1  72('_'),// 
     1  ' IfrrigX (Type 13); Operation Right ID = ', a12,
     1  ' Destination Type = ', a12,
     1  ' Carrier (Y/N) = ',a3, ' Reuse (Y/N) = ', a3/ 
     1    '                                           ',
     1    '                                    ',
     1    '  Decree  IndexQ  Factor  IndDem  Demand',
     1    '  DecAdj   Avail  Divert',/,
     1    ' In/Out    Yr   Mo  Day Iter   On   nf ifcd  isx iso2',
     1    ' Name                     ',
     1    '  dcrifr   river      cp  avindx  flowrq',
     1    ' aloifrX   avail  actwrq iwhy cwhy',/8x,
     1    ' ____ ____ ____ ____ ____ ____ ____ ____ ____',
     1    ' ________________________ ',
     1    8(' _______'),' ____ ',24('_') )
 280  format(
     1 ' IfrrigX', i5, 1x, a4, 7i5, 1x, a24, 1x, 8f8.0, i5, 1x, a24)
c
c _________________________________________________________
c       End
c
      end

