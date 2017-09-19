c
	  subroutine daymon(itype)
c
c _________________________________________________________
c	Program Description
c
c       DayMon; It allows an easy transition from a
c               monthly model to a daily model.
c               For a daily model this routine is called
c               and it sums up calculated  values that are the
c               same for a daily or monthly model.
c               After a month is complete it then sets
c               the monthly totals back to calculated values.
c
c               The routine also prints daily data and resets
c               daily return and depletion arrays
c
c               itype = 0 routine sums daily or monthly values into
c                 monthly totals
c               itype = 1 routine turns monthly totals into
c                 daily or monthly values
c
c               Called by Execut
c
c _________________________________________________________
c	Update History
c		NA
c
c__________________________________________________________
c	Dimensions
	  include 'common.inc'
c
c__________________________________________________________
c	Initlize	  
	  fac=factor
c
c		iout = 0 no details
c		iout = 1 details on plan data	  
	  iout=0
c
c =========================================================
c               Calculate monthly totals
c
	  if(itype.eq.0) then
	    rimd=float(imd)
c
c__________________________________________________________
c               Step 1; Sum river station data
	    do is=1,numsta
	      riverm(is) = riverm(is) + river(is)
	      availm(is) = availm(is) + avail(is)
	      avinpm(is) = avinpm(is) + avinp(is)
c
	      returm(is) = returm(is) + returd(ido,is)
	      deplm(is)  = deplm(is)  + depld(ido,is)

	      gw2rivm(is)= gw2rivm(is)+ gw2riv(is)
	      gw2gwm(is) = gw2gwm(is) + gw2gw(is)
c
	      do i=1,maxacc
		qdivm(i,is)=qdivm(i,is)+qdiv(i,is)
c		if(i.eq.29 .and. is.eq.23) 
c    1		  write(nlog,*) ' Daymon; is, qdivm, ',
c    1		  is,qdivm(i,is)*fac
	      end do
	    end do
c
c__________________________________________________________
c               Step 2; Sum diversion data
c
	    do nd=1,numdiv

	      if(idivsw(nd).ne.0) then
		rlossm(nd)=rlossm(nd) + rloss(nd)
		dcum(nd) =dcum(nd) + dcu(nd)
		dcutm(nd)=dcutm(nd)+ dcut(nd)
c
c rrb 01/03/07; Add Soil data
		qdivsm(nd)=qdivsm(nd) + qdivs(nd)
		qdivsom(nd)=qdivsom(nd) + qdivso(nd)

		is=idvsta(nd)
		iui=nduser(nd)
		iue=nduser(nd+1)-1
		if(iui.le.iue) then
		  do iu=iui,iue
		    divreqm(iu)=divreqm(iu)+divreq(iu)
		  end do
		endif
	      endif
	    end do
c
c__________________________________________________________
c               Step 3; Sum well data
c
	    do nw=1,numdivw
	       divreqwm(nw) = divreqwm(nw) + divreqw(nw)
	       divmonwm(nw) = divmonwm(nw) + divmonw(nw)
	       rlosswm(nw)  = rlosswm(nw)  + rlossw(nw)
	       rlossw2m(nw) = rlossw2m(nw) + rlossw2(nw)
	       rdepwm(nw)   = rdepwm(nw)   + rdepw(nw)
	       dcuwm(nw)    = dcuwm(nw)    + dcuw(nw)
	       dcutwm(nw)   = dcutwm(nw)   + dcutw(nw)
c
c rrb 01/03/07; Add Soil data
	       qdivswm(nw)=qdivswm(nw) + qdivsw(nw)
	       qdivswom(nw)=qdivswom(nw) + qdivswo(nw) 
	    end do
c
c__________________________________________________________
c               Step 4; Sum instream flow data
c
	    do nf=1,numifr
	      flowrqm(nf)=flowrqm(nf)+flowrq(nf)
	      ib=ndnifb(nf)
	      ie=ndnifb(nf)+ndnifs(nf)-1
	      do i=ib,ie
		qdivrm(i)=qdivrm(i)+qdivr(i)
	      end do
	    end do
c
c__________________________________________________________
c               Step 5; Sum reservoir data
c
c               Reservoir initial storage
c            if(idy.eq.1) then
c              do nr=1,maxres
c                volintm(nr)=volint(nr)
c              end do
c            endif
c
c               Reservoir evaporation and targets
	    do nr=1,maxres
	      evapm(nr)=evapm(nr)+evap(nr)
c
c rrb 2006/04/12; Reservoir Seepage and Loss	      
	      rlossRM(nr)=rlossRM(nr)+rlossR(nr)
	      sepactM(nr)=sepactM(nr)+sepact(nr)
c
c rrb 01/03/01; No need to sum targets
c             tarmaxm(nr)=tarmaxm(nr)+tarmax(nr)
	    end do
c
c               Reservoir accounts;
c               note (20) = initial storage & (23)=storage limit
c                 are defined on day 1 in dayset.for
	    do no=1,maxown
	      do i=1,maxacc
		qresm(i,no)=qresm(i,no)+qres(i,no)
		if(i.eq.20 .or. i.eq.23) then
		else
		  accrm(i,no)=accrm(i,no)+accr(i,no)
		endif
	      end do
	    end do
c
c__________________________________________________________
c               Step 6; Sum operating rule data
c
	    do no=1,numopr
	      divom(no)=divom(no)+divo(no)
	    end do	    
c
c__________________________________________________________
c               Step 7; Sum plan data
c
 	    do np=1,nplan
	      pdemM(np)=pdemM(np) + pdem(np)
	      pdemTm(np)=pdemTm(np) + PdemT(np)
	      
	      psuplyM(np)=psuplyM(np) + psuply(np)
	      psuplyTm(np)=psuplyTm(np) + PsuplyT(np)
	      
	      PImportM(np)=PImportM(np) + PImportT(np)
	      
	      Psto1(np) = Psto1M(np)
	      Psto2(np) = Psto2(np)
	      
	      PevapM(np) = PevapM(np) + pevap(np)	      
c
c rrb 2006/03/31; Well Augmentation Pumping in Priority          	      
	      PwellCM(np)=PwellCM(np) + PwellC(np)
	      PdriveM(np)=PdriveM(np) + Pdrive(np)
	      
              if(iout.eq.1) then
cr              write(nlog,*) '  Daymon;',
cr   1            iyr, mon, idy, np, pdemT(np)*fac, pdemtm(np)*fac
                write(nlog,*) '  Daymon; Plan Data',
     1            iyr, mon, idy, np, pImportM(np)*fac, PimportT(np)*fac
              endif
	    end do
   
c
c__________________________________________________________
c               Step 8; Print daily data
c
c           write(io99,*) ' ichk99 = ', ichk99
            maxstax=maxsta
            if(ichk99.ne.30) call dayoutr(maxstax)
c
c__________________________________________________________
c               Step 9; Initilize return array
c rrb 01/03/28; Set return values for reuse 
c
c               Test for I/O of return data
c       irecx=(ido-1)*nstrtn

	do nr=1,nstrtn
	  returd(ido,nr)=0.0
	  depld(ido,nr)=0.0
c
c               Test for I/O of return data
c         irec1=irecx+nr
c         write(78,rec=irec1) 0.0,0.0
	end do
c
c		Set Initilize plan obligation data for reuse
        do ip=1,nplan
          pobld(ido,ip)=0.0
          psupD(ido,ip)=0.0
        end do  	
c
c__________________________________________________________
c               Return

	    goto 500
	  endif
c
c =========================================================
c               Set Monthly to daily totals

c               Set monthly to daily total (For daily model) 
	if(itype.eq.1) then
	  rimd=float(imd)
c
c__________________________________________________________
c               Step 10; Set river station data  
	  do is=1,numsta
	    river(is) = riverm(is)/rimd
	    avail(is) = availm(is)/rimd
	    avinp(is) = avinpm(is)/rimd

	    gw2riv(is)= gw2rivm(is)/rimd
	    gw2gw(is) = gw2gwm(is)/rimd 

	    retur(imo,is) = returm(is)/rimd
	    depl(imo,is) = deplm(is)/rimd

	    do i=1,maxacc
c		if(i.eq.29 .and. is.eq.23)
c    1            write(nlog,*) ' Daymon; ***, is, qdivm, ',
c    1		  is,qdivm(i,is)*fac
     
	      qdiv(i,is)=qdivm(i,is)/rimd
	      
c               if(i.eq.29 .and. is.eq.23)
c    1            write(nlog,*) ' Daymon ***, is, qdiv, ',
c    1		  is,qdiv(i,is)*fac
	      
	    end do
	  end do
c
c__________________________________________________________
c               Step 11; Set diversion data
c
	  do nd=1,numdiv
	    if(idivsw(nd).ne.0) then
	      rloss(nd)=rlossm(nd)/rimd
	      dcu(nd)=dcum(nd)/rimd
	      dcut(nd)=dcutm(nd)/rimd
c
c rrb 01/03/07; Soil data
	      qdivs(nd)=qdivsm(nd)/rimd
	      qdivso(nd)=qdivsom(nd)/rimd

	      iui=nduser(nd)
	      iue=nduser(nd+1)-1
	      if(iui.le.iue) then
		do iu=iui,iue
		  divreq(iu)=divreqm(iu)/rimd
		end do
	      endif
	    endif
	  end do
c
c__________________________________________________________
c               Step 12; Set daily well data
c
	  do nw=1,numdivw
	    divreqw(nw) = divreqwm(nw)/rimd
	    divmonw(nw) = divmonwm(nw)/rimd
	    rlossw(nw)  = rlosswm(nw) /rimd
	    rlossw2(nw) = rlossw2m(nw)/rimd
	    rdepw(nw)   = rdepwm(nw)/rimd
	    dcuw(nw)    = dcuwm(nw)/rimd
	    dcutw(nw)   = dcutwm(nw)/rimd
c
c rrb 01/03/07; Soil data
	    qdivsw(nw)=qdivswm(nw)/rimd
	    qdivswo(nw)=qdivswom(nw)/rimd
	  end do
c
c__________________________________________________________
c               Step 13; Set instream flow data  
c
	  do nf=1,numifr
	    flowrq(nf)=flowrqm(nf)/rimd
	    ib=ndnifb(nf)
	    ie=ndnifb(nf)+ndnifs(nf)-1
	    do i=ib,ie
	      qdivr(i)=qdivrm(i)/rimd
	    end do
	  end do
c
c__________________________________________________________
c               Step 14; Set reservoir data
c
	  do nr=1,numres
	    volint(nr)=volintm(nr)
	    evap(nr)=evapm(nr)
	    ritremx(nr)=ritremm(nr)
c
c rrb 2006/04/12; Reservoir Seepage loss in cfs
            rlossR(nr) = rlossRM(nr)/rimd	    
c
c rrb 2006/04/18; Reservoir Seepage in af            
	    sepact(nr) = sepactM(nr)
            
c 
c rrb 01/03/01; Do not sum targets
c           tarmax(nr)=tarmaxm(nr)/rimd
	  end do
c
c               Reservoir accounts (note in ac-ft, = no / rimd)
	  do no=1,maxown
	    do i=1,maxacc
	      qres(i,no)=qresm(i,no)
	      accr(i,no)=accrm(i,no)
c             write(io99,*)' Execut i,no,accr', i,no, accr(i,no)
	    end do
	  end do
c
c__________________________________________________________
c               Step 15; Set operating rule data
c
	  do no=1,numopr
	    divo(no)=divom(no)/rimd
	  end do
c
c__________________________________________________________
c               Step 16; Set Plan data
c
	  do np=1,nplan
	    pdem(np)=pdemM(np)/rimd
	    pdemT(np)=pdemTM(np)/rimd
	    
	    psuply(np)=psuplyM(np)/rimd
	    psuplyT(np)=psuplyTM(np)/rimd
	    
	    PImport(np)=PImportM(np)/rimd	    
	    
	    Pevap(np)=PevapM(np)/rimd
c
c rrb 2006/03/31; Well Augmentation Pumping in Priority          	    
	    PwellC(np)=PwellCM(np)/rimd
	    Pdrive(np)=PdriveM(np)/rimd
	  end do
	  
c
c__________________________________________________________
c               Step 16; Return
	  goto 500
	endif

 500  RETURN

      END


