c
c _________________________________________________________
c	Update History
c

c
c _________________________________________________________
c	Documentation
c

cc     Last change:  RB   13 Feb 98    2:49 pm
c
      subroutine dayset
c
c _________________________________________________________
c	Program Description
c
c      Dayset; it sets variables used in monthly model to daily 
c               values to minimize the changes in most subroutines
c
c
c
c _________________________________________________________
c	Dimensions
      include 'common.inc'
c                           
c
c _________________________________________________________
c	Initilize
      fac=factor
      ioutR=0

c
c =========================================================
c               Initilize on day 1
c
      if(idy.eq.1) then
c
c _________________________________________________________
c               Step 1; Initilize monthly stream data
c
        do is=1,numsta
          riverm(is) = 0.0
          availm(is) = 0.0
          avinpm(is) = 0.0
          returm(is) = 0.0
          deplm(is)  = 0.0

          gw2rivm(is)= 0.0
          gw2gwm(is) = 0.0
c
          do i=1,maxacc
            qdivm(i,is) = 0.0
          end do
        end do
c
c _________________________________________________________
c               Step 2; Initilize monthly diversions
c
        do iu=1,numuse
          divreqm(iu)=0.0
        end do
c
c rrb 99/07/12; Initilize monthly total by diversion
        do nd=1,numdiv
          rlossm(nd)=0.0

          qdivsm(nd)=0.0
          qdivsom(nd)=0.0
        end do
c
c _________________________________________________________
c               Step 3; Initilize monthly wells
c
        do nw=1,numdivw
          divmonwm(nw)=0.0
          divreqw(nw)=0.0
          rlosswm(nw)=0.0
          rlossw2m(nw)=0.0
          rdepwm(nw)=0.0

          qdivswm(nw)=0.0
          qdivswom(nw)=0.0
          carryWM(nw)=0.0
        end do
c
c _________________________________________________________
c               Step 4; Initilize monthly instream flows
c
        do ni=1,numifr
          flowrqm(ni)=0.0
          ib=ndnifb(ni)
          ie=ndnifb(ni) + ndnifs(ni) - 1
c
c rrb 08/07/96 Add Instream flow reach
          do i=ib,ie
            qdivrm(i)=0.0
          end do
        end do
c
c _________________________________________________________
c               Step 5; Initilize reservoir accounts
c
        do no=1,maxown
          do i=1,maxacc
            qresm(i,no)=0.0
            accrm(i,no)=0.0
c           write(io99,*) '  Dayset; i,no,accrm', i, no, accrm(i,no)
          end do

          accrm(20,no)=curown(no)
        end do
c               Set unused decreed amount to total
c               reservoir (ritremx) and accounts accr(23,n)
        do nr=1,numres
          ritremm(nr)=0.0
c
c rrb 01/03/01; No need to sum targets
c         tarmaxm(nr)=0.0
        end do

        do l2=1,numrsr             
          nr1 = iresco(1,l2)
c
c               Do not show OOP rights as part of the total
          if(ityrsr(l2).ne.-1) then
            ritremm(nr1) = ritremm(nr1)+ritrem(l2)
          endif

          iown=nowner(nr1)+iresco(2,l2)-1
          do n=1,nrown(l2)
            n1 = iown+n-1
            accrm(23,n1) = ritrem(l2)
          end do
        end do
c
c _________________________________________________________  
c
c               Step 6; Initilize Reservoir Total
c               Note evapm is not used (it is / 31 in evasec.for)
        do nr=1,numres
          evapM(nr) = 0.0
          sepactM(nr)=0.0
          volintM(nr)=cursto(nr)
c
c rrb 2006/04/12; Reservoir Seepage Loss          
          rlossRM(nr)=0.0
        end do
c
c
c _________________________________________________________ 
c               Step 7; Initilize monthly operating Rule data
        do no=1,numopr
          divom(no)=0.0
        end do
c
c _________________________________________________________
c               Step 8; Initilize monthly cu
        do nu=1,numuse
          usemon(nu)=0.0
          dcum(nu)=0.0
          dcutm(nu)=0.0
        end do

        do nw=1,numdivw
          usemonw(nw)=0.0
          dcuwm(nw)=0.0
          dcutwm(nw)=0.0
        end do
c
c _________________________________________________________
c               Step 8; Initilize plan data on day 1x/mo
        do np=1,nplan
          pdemM(np)=0.0          
          pdemTM(np)=0.0
          
          psuplyM(np)=0.0    
          psuplyTM(np)=0.0          
          
          PImportM(np)=0.0
          PImportT(np)=0.0
          
          PevapM(np) = 0.0
          
          Psto1M(np) = Psto1(np)
c
c rrb 2006/03/31; Well Augmentation Pumping in Priority                    
          PwellCM(np) = 0.0
          PdriveM(np) = 0.0
        end do
c
c _________________________________________________________
c		Operational right maximum limit 1x/mo      
      
        do k=1,numopr
          oprmaxM(k)=oprmax(k,mon)
          if(mon.eq.1 .and. idy.eq.1) oprmaxA(k)=oprmax(k,13)
        end do
c
c =========================================================
c               End Initilization on day 1 only

      endif
c
c =========================================================
c               Begin to Initilize every day
c
c
c _________________________________________________________
c               Step 9; Initilize daily water rights 1x/day
c
      do nd=1,numfrr
        divi(nd)=0.
      end do

      do nd=1,maxrea
        do l2=1,MAXIFR
          divir(l2,nd)=0.
        end do
      end do

      do nd=1,numrsr
        divr(nd)=0.
        ritremx(nd)=0.0
      end do

      do nd=1,numdvr
        divd(nd)=0.
      end do

      do nd=1,numdvrw
        divdw(nd)=0.
      end do

      do nd=1,numopr
        divo(nd)=0.
c
c rrb 05/02/14; Plans, etc.        
        divdS(nd)=0.0
        divdE(nd)=0.0
      end do
c
c _________________________________________________________
c               Step 10; Initilize daily diversion cu 1x/day
c
        do nu=1,numuse
          dcu(nu)=0.0
          dcut(nu)=0.0
        end do
c
c _________________________________________________________
c               Step 11; Initilize daily well cu 1x/day
c
        do nw=1,numdivw
          dcuw(nw)=0.0
          dcutw(nw)=0.0
        end do 
c
c _________________________________________________________
c               Step 12; Initilize daily Instream Flows 1x/day
c
      do ni=1,numifr
c
c rrb 04/25/97; Add daily model
c       flowrq(ni)=flowr(mon,ni)
        flowrq(ni)=flowrd(idy,ni)
        ib=ndnifb(ni)
        ie=ndnifb(ni) + ndnifs(ni) - 1
c
c rrb 08/07/96; Instream flow reach
        do i=ib,ie
          florqr(i)=flowrq(ni)
          qdivr(i)=0.0
        end do
      end do
c _________________________________________________________
c               Step 13; Initilize daily demands 1x/day
c
c rrb 01/02/20; Handled in demand.for based on demand type
c     do nu=1,numuse
c
      do nd=1,numdiv
        divmon(nd)=0.
        rloss(nd) = 0.0
        qdivs(nd)=0.0
        qdivso(nd)=0.0
      end do
c
c _________________________________________________________
c               Step 14; Initilize daily Wells 1x/day
c
      do nw=1,numdivw
c
c rrb 01/02/20; Handled in demand.for based on demand type
c       divreqw(nw)=diverdw(idy,nw)
c       diwrreqw(nw)=diwrdw(idy,nw)

        divmonw(nw)=0.0
        rlossw(nw) =0.0
        rlossw2(nw)=0.0
        rdepw(nw)=0.0

        qdivsw(nw)=0.0
        qdivswo(nw)=0.0
        carryW(nw)=0.0
      end do
c
c _________________________________________________________
c               Step 15; Initilize daily demands (diversions & wells)
c                         1x/day
c
c rrb 01/12/26; Variable dimension               
c     call demand(1)
      maxdivx=maxdiv
      maxdivwx=maxdivw
      call demand(1,maxdivx,maxdivwx) 


c
c _________________________________________________________
c               Step 16; Initilize daily stream flow and
c               payback to stream 1x/day
c
      do is=1,numsta
        river(is)=0.
        avinp(is)=0.
        qtribu(is)=0.

      end do
c
      do nd=1,numdiv
        qstern(nd)=0.
      end do
c
c _________________________________________________________
c               Step 17; Compute daily Virgen Flow 1x/day
c
      do iru=1,numrun
        iss=irusta(iru)
c
c rrb; daily model
c       qtribu(iss)=qtribu(iss)+virinp(mon,iru)
c       river (iss)=river (iss)+virinp(mon,iru)
        qtribu(iss)=qtribu(iss)+virind(idy,iru)
        river (iss)=river (iss)+virind(idy,iru)
        iss=idncod(iss)

c       if(iss.eq.0) go to 370
        if(iss.ne.0) then
          ndnn=ndnnod(iss)

          do nd=1,ndnn
c
c rrb; 04/25/97; daily model
c           river (iss)=river (iss)+virinp(mon,iru)
c           avinp (iss)=avinp (iss)+virinp(mon,iru)
            river (iss)=river (iss)+virind(idy,iru)
            avinp (iss)=avinp (iss)+virind(idy,iru)
            iss=idncod(iss)
          end do
        endif
      end do
c
c _________________________________________________________
c               Step 18; Initilize daily Qstern 1x/day
c
      do nd=1,numdiv
        is=idvsta(nd)
        if(is .ne. 0) qstern(nd)=river(is)
      end do
c
c _________________________________________________________
c               Step 19; Initilize daily return flows and 
c                        depletions 1x/day
c
      do irn=1,nstrtn
        iss=istrtn(irn)
        c=returd(ido,irn)
c
c rrb 99/07/12; Daily depletions
        c1=depld(ido,irn)

        qtribu(iss) = qtribu(iss) + c - c1
        river(iss)  = river(iss)  + c - c1

c       if(ichk1.eq.1 .and. irn.eq.1) write(99, 110) mon, idy, idyt
        iss=idncod(iss)
        if(iss.ne.0) then
          ndnn=ndnnod(iss)

          do nd=1,ndnn
            river(iss) = river(iss) + c - c1
            avinp(iss) = avinp(iss) + c - c1
            iss=idncod(iss)
          end do
        endif
      end do
c
c _________________________________________________________
c               Step 20; Set GW storage and to / from river 1x/day
c		         Also set Avail = River

      call gwsub
c
c _________________________________________________________
c               Step 21; Initilize daily currtn 1x/day
c                                 
      do is=1,numsta
        currtn(is)=0.
      end do
c
c _________________________________________________________
c               Step 23; Initilize daily Qdiv 1x/day
c
      do i=1,numsta
        do j=1,maxacc
          qdiv(j,i)=0.
        end do
      end do
c
c _________________________________________________________
c               Step 24; Initilize reservoir diversions (qres)
c                        & accounts (accr) 1x/day
c               Note accr(20) is initial storage 
c 
c
c rrb 2009/6/09; Clean up
cx    do nr=1,maxres
      do nr=1,numres
c       write(io99,*) '  Dayset; nr, idy, maxres = ', nr, idy, maxres
        evap(nr)=0.0
        sepact(nr)=0.0
        sepact1(nr)=0.0
        nSepCal(nr)=0
        
c
c rrb 01/02/10; Set above in water right loop (rights <  # of res)
c       ritremx(nr)=0.0
        volint(nr)=cursto(nr)
        tarmax(nr)=targetd(idy,nr)
        rlossR(nr)=0.0
      end do
c
c               Reservoir accounts, etc
      do no=1,maxown
        do i=1,maxacc
          qres(i,no)=0.
          accr(i,no)=0.0
        end do
        accr(20,no)=curown(no)
      end do
c
c               Set unused decreed amount to total
c               reservoir (ritremx) and accounts accr(23,n)
      do l2=1,numrsr             
        nr1 = iresco(1,l2)
c
c               Do not show OOP rights as part of the total
        if(ityrsr(l2).ne.-1) then
          ritremx(nr1) = ritremx(nr1)+ritrem(l2)
        endif

        iown=nowner(nr1)+iresco(2,l2)-1
        do n=1,nrown(l2)
          n1 = iown+n-1
          accr(23,n1) = accr(23,n1) + ritrem(l2)
        end do
      end do
      
c
c _________________________________________________________
c rrb 2009/06/01; 
c		  Step 25; Initilize reservoir data when the admin date
c			is not used (rdate(nr) = -1)
c

      if(numres.gt.0) then
        DO 150 NR=1,NUMRES
          if(ioutR.eq.1) then
            write(nlog,152) nr, cresid(nr), rdate(nr)
 152        format(' DaySet; nr, cresid', i5, 1x, a12, f5.1) 
          endif
			        
c
c              Branch if this reservoir is not operated (iressw = 0)
  	  IF(IRESSW(NR).EQ.0) GO TO 150
c		
         if(ifix(rdate(nr)).lt.0) then 
c
c _________________________________________________________
c               Step 4; Initilize account storage 1x/run or Admin Date
c

	    NOI=NOWNER(NR)
	    NOE=NOWNER(NR+1)-1
  	    cursto1=0.0
	    cursto2=0.0
	    
	    DO NO=NOI,NOE
	      IF(N2OWN(NO).LE.1) THEN
	        cursto1     = cursto1 + curown(no)
	      ELSE
	        cursto2     = cursto2 + curown(no)
	      ENDIF    
	      if(ioutr.eq.1) then
	        write(nlog,*) ' DaySet; no,noi,noe,curown(no),cursto1'                         
	        write(nlog,*) ' DaySet;', no,noi,noe,curown(no), cursto1
	      endif  
	    end do
c                           
	    do iw=1,maxrsr
              tot1x(iw) = 0.0
              tot2x(iw) = 0.0
	    end do
c
c ---------------------------------------------------------
c rrb 2006/10/13; 
c               b. Initilize 1x/run or on admin date
c                  Initilize operating rule data tied to a
c	            reservoir operating date

            do k=1,numopr
              divopr(k)=0.0
            end do  
	    
c
c ---------------------------------------------------------
c               b. Initilize reservoir water right data 1x/run or
c		on Admin date
c               Loop for all water rights in priority loop
c               to set one fill constraint
c		Note ntorig is the total of all rights
c          if(ichk.eq.4) write(nlog,*)'  DaySet; 5 Reservoir rights '

  	    do 140 iw=1,ntorig
	      l1 = nwrord(1,iw)
	      l2 = nwrord(2,iw)
c                                      
c               For all reservoir rights
c rrb 04/10/96; Skip for out of priority rights (ityrsr=-1)
	      if(l1.eq.2) then
	      
c
c
c rrb 2006/05/19; Do not calculate if the right is off	            
                if(ioutr.eq.1) then
	          write(nlog,*) ' DaySet; nr,l2,irsrsw(l2),dcrres(l2)'
	          write(nlog,*) ' DaySet;',nr,l2,irsrsw(l2),dcrres(l2)
	        endif  
c
c               Check against iresopr not irsrsw.
c		  Note iResOpr is defined to equal irsrsw in riginp.
c		  It stays on even if the reservoir right
c		  is part of an operating rule which turns off the
c		  original right
	        
	        if(iResOpr(l2).eq.0) goto 140
	        if(iResOpr(l2).gt.0 .and. iyr-irsrsw(l2).lt.0) goto 140
	        if(iResOpr(l2).lt.0 .and. iyr+irsrsw(l2).gt.0) goto 140	      

	        nr1=iresco(1,l2)
c
c               We are within a reservoir loop; therefor
c               branch to avoid duplicating calculations
	        if(nr1.ne.nr) goto 140
c
c rrb 1996/04/10; Do not charge active storage to out of 
c               priority rights (ityrsr=-1)
	        if(ityrsr(l2).eq.-1) then
 		   ritrem(l2) = dcrres(l2)
		   goto 140
	        endif     

	        IF(N2FILL(L2).LE.1) THEN
		   c = dcrres(l2) - cursto1     
                   tot1x(l2) = tot1x(l2) + dcrres(l2)
                 
		   if(c.ge.-0.1) then
		     ritrem(l2) = c
                   cursto1      = amax1(cursto1 - tot1x(l2), 0.0)
		   else
		     ritrem(l2) = 0.0
		     cursto1      = cursto1 - dcrres(l2)
		   endif
	        ELSE
		   c = dcrres(l2) - cursto2  
                   TOT2x(L2) = TOT2x(L2) + DCRRES(L2)
                
		   IF(C.GE.-0.1) THEN
		     RITREM(L2) = C
                     cursto2      = amax1(cursto2 - tot2x(l2), 0.0)
		   ELSE
		     RITREM(L2) = 0.0
		     cursto2      = cursto2 - dcrres(l2)
		   ENDIF
	        ENDIF
c
c rrb 2006/06/05; Paper Fill
               RitPaper(l2)=RitRem(l2)	        
c
               if(ioutR.eq.2) then
c
c rrb 2006/07/25; Enhancement                
cr               if(nr.eq.1) write(nlog,130)
                 if(l2.eq.1) write(nlog,130)
                 write(nlog,132)  nr,cresid(nr), creswr(l2),
     1             iyrmo(mon),xmonam(mon),l1, l2, n2fill(l2),nr,nr1,
     1             rdate(nr), dcrres(l2),tot1x(l2),cursto(nr),
     1             cursto1,cursto2, ritrem(l2), RitPaper(l2),
     1             ritremx(nr)
               endif
	      endif
c  
c ---------------------------------------------------------
c		End water right loop	      
  140       continue
c  
c ---------------------------------------------------------
c		Endif for admin date off (rdate(nr) = -1)
          endif
c  
c ---------------------------------------------------------
c		End reservoir loop
  150   continue
      endif

c
c _________________________________________________________
c               Step 26; Set downstream call if on 1x/day
      if(idcall.ne.0) then
        dcall1=dcalld(idy)
c       write(nlog,*) '  DaySet; iyr, imo, idy, dcall1',
c    1    iyrmo(mon), xmonam(mon), idy, dcall1
      endif
c
c _________________________________________________________
c               Step 27; Initilize plan data 1x/day
c
        do np=1,nplan
          c = poblD(ido,np) + pfail(np)/fac
          pdem(np)= c
          pdemT(np)= c
          pevap(np)=0.0
c
c rrb 2006/03/31; Well Augmentation Pumping in Priority     
          pwellC(np)=0.0     
          pdrive(np)=0.0
c
c rrb 2005/08/05; Revise to carry over storage from a res plan
c		  Note Psuply is running total; psuplyT is total inflow	          
          c = psupD(ido,np)
          caf=c*fac
          iPtype1=iPlnTyp(np)          
          if(iPtype1.eq.3 .or. iPtype1.eq.5 .or. iPtype1.eq.9) then
            psuply(np)= psupD(ido,np)+psuply(np)
            psuplyT(np)= psupD(ido,np)
            
            psto1(np)=psto2(np) 
c
c rrb 2008/09/22; Total supply in Psto2 (ACFT) includes psuply(np)
cx          psto2(np)=psto2(np) + psupD(ido,np)*fac            
          else
            psuply(np)= psupD(ido,np)
            psuplyT(np)= psupD(ido,np)
          endif
c
c		Initilize amount in a reuse plan here since at least
c               part of it is calculated from a diversion in a prior
c               time step
c
c rrb 2006/04/18; Do not adjust for a Reservoir reuse plan since
c		  returns are in the return array
c rrb 2008/01/15; Correction
cx        if(iPtype1.ne.8 .and. iPtype1.ne.12) then        
cx          is=ipsta(np)
cx          qdiv(28,is)=psuplyT(np)
cx        endif  
          
c
c rrb		Temporarily set daily to average monthly
cr        PImport(np)=PImportD(np,nd)
          PImport(np)=PImportX(mon,np)
          PImportT(np)=0.0
        end do
c
c _________________________________________________________
c               Step 28; Initilize Call indicators 1x/day
c			Note ..L = call location
c                            ... = print indicator
            do i=1,numsta
              imcdL(i)=-1
            end do
c
c _________________________________________________________
c       	  Step 29; Initilze compact demand 1x/day if a Type 13
c		   operating rule is on.
c
      if(isetOpr(13).gt.0) then
        do k=1,numopr
          if(ityopr(k).eq.13 .and. ioprsw(k).ne.0) then
            nf  =iopdes(1,k)	
            ifcd=ifrsta(nf)          
            mon2 = imonsw(k,mon)
          
            if(mon2.eq.0) then
              cp=0.0
              flowrq(nf)=0.0
              flowrd(idy,nf)= 0.0
              avindx=0.0
              riverx=-1.0
            else
              isx = iopsou(1,k)
              riverx=river(isx)
              cp=float(iopsou(2,k))/100.0
              avindx = riverx * cp
              flowrq(nf)=avindx
              flowrd(idy,nf)=avindx
            endif

            iout=0
            ioutiw=0
      
            if(ichk.eq.113) then
              write(nlog,270) corid(k)
        
              write(nlog,280) iyrmo(mon),xmonam(mon),idy,
     1        iwx,mon2,nf,ifcd,isx, iopsou(2,k),
     1        riverx*fac, cp*100., avindx*fac,
     1        xfrnam1(nf)
            endif
          endif
        end do  
      endif  
          
 270  format(/, 
     1  ' DaySet; (Type 13); Operation Right ID = ', a12,
     1  ' Demand Calculations',/
     1    '                                           ',
     1    '                                    ',
     1    '  IndexQ  Factor  Demand',/
     1    ' DaySet    Yr   Mo  Day Iter   On   nf ifcd  isx iso2',
     1    '  riverx      cp  avindx',
     1    ' Name                     ',     /8x,
     1    ' ____ ____ ____ ____ ____ ____ ____ ____ ____',
     1    3(' _______'),' ________________________ ')
 280  format(
     1 ' DaySet ', i5, 1x, a4, 7i5, 3f8.0,1x, a24)
            
c
c _________________________________________________________
c               Step 29; Return
c                      
      return
c
c _________________________________________________________
c               Formats
c
 100  format
     1    ('  Dayset; Warning < 0 befor allocation ',
     1    ' on ',i4,1x,a3,i4,'. Set to zero from ',
     1     f10.3, ' cfs or ', f10.0, ' af at ', a24)
 110  format('  dayset;  mon  idy idyt',/,10x, 3i5)
  
 130  format(
     1 '  DaySet;   nr ID            ID_WR        ',
     1 '     iyr     mon      l1      l2  n2fill      nr     nr1',
     1 '   rdate  dcrres   tot1x  cursto cursto1 cursto2  ritrem',
     1 ' RitPapr RitremX',/
     1 ' ________ ____ ____________  ____________ ', 7(' _______'),
     1 9(' _______'))
 132  format('  DaySet ', i5,1x,a12,1x,a12,1x,i8,4x,a4,5i8, 20f8.0)
  
      end

      



