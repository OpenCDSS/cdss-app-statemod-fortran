
c
C     Last change:  RRB   9 Oct 2002    3:00 pm
c
C
      SUBROUTINE BOMSEC
c
c _________________________________________________________
c	Program Description
c
c      Bomsec; It initilizes data
c              Some once per simulation (istart.eq.1)
c	        Some once per year (if mon.eq.1)
c              Some every month
c	        Called by Execut & Vircom
c
c                           
c
c _________________________________________________________
c
c               Update history
c rrb 96/05/29; UPdated to generalize dimension statements
c rrb 00/11/24; If itsfile = 1,10 GW demand is limited to acres served
c rrb 01/08/06; If soil moisture exceeds soil moisture capacity 
c               because the area gets smaller the difference is no 
c               longer accounted as a loss to avoid problems with the 
c               *.xss reporting of return flows.
c
c rrb 00/12/09; For variable efficiency, initilize running 
c               IWR for diversion (diwrreq) and well (diwrreqw) and
c               CU from SW for Div and D&W (dcu), 
c               CU from GW for Well only (dcuw),
c               CU Total from SW only and D&W (dcut)
c _________________________________________________________
c	Documentation
c
c		ioutp = print detailed plan data
c			1=detail plus summary
c			2=summary
c		ioutR = 1 Print one fill rule detailed data
c		ioutR = 2 Print one fill rule summary #1
c		ioutR = 3 Print one fill rule summary #2 
c		ioutI = 1 Print instream flow data
c		ioutT = 1 Print reservoir target data
c		iout47= 1 Print details of operating rule 47 reset
c
c _________________________________________________________
c	Dimensions
      include 'common.inc'
      
      ioutP=0
      ioutP1=0
      ioutR=0
      ioutI=0
      ioutT=0
      iout47=0

      do i=1,maxsta
        xzero(i)=0.0
      end do

      fac = mthday(mon)*factor
      if((mon-1).gt. 0) then
        facM1=mthday(mon-1)*factor
      else
        facM1=mthday(12)*factor
      endif  
      small=0.01
c
c rrb 02/01/15; number of elements in file b66
c     nrg=22
      nrg=24
c
      istart = 0
      if(iyr.eq.iystr .and. mon.eq.1) istart = 1
      if(istart.eq.1) iprintL=0
      iprintS=0


c
c _________________________________________________________
c               Step 1; Initilize Annual limit (1x/yr) for
c                       a type 14 operating rule
c
c ---------------------------------------------------------
c rrb 99/08/09; Allow operating rule type 14 to be constrained
c               by an annual demand (divreqa)
      if(iSetOpr(14).gt.0) then
        if(mon.eq.1) then
          do k=1,numopr
	          if(ityopr(k).eq.14 .and. iopsou(2,k).ge.1) then
	            divreqa(k) = iopsou(2,k)
	          endif
	        end do
     	  endif
      endif

c
c _________________________________________________________
c               Step 1; Initilize Annual limit (1x/run) for
c                       a type 47 operating rule
c			   Note it reset on an annual basis based
c			   on the reset month specified below.
      if(iSetOpr(47).gt.0 .and. istart.eq.1) then
         do k=1,numopr
           if(ityopr(k).eq.47 .and. iopsou(1,k).ge.1) then
	           np=iopsou(1,k)
	           oprmaxA(k)=psto1(np)
	       
cx          write(nlog,*) ' Bomsec; iout47_1 ', iout47	
	           if(iout47.eq.1) then
	             write(nlog,*) ' Bomsec; iout47_1 ', iout47	       
               write(nlog,*) ' Bomsec; Type 47 data 1x per run'
               write(nlog,'(a12, 1x, 5i5, 20f8.0)') 
     1            corid(k), k, np, mon, 
     1            iyrmo(mon), imomo(mon), OprmaxA(k)
             endif
	         endif
	       end do
cx	  endif
      endif
     
c _________________________________________________________
c               Step 2; Initilize Rio Grande Compact Data 1x/run
c rrb 99/12/07; Note initilize carryover and binary input 1x per run  
c
      if(ichk.eq.94) write(nlog,*)'  Bomsec; Rio Grande Compact'

      if(irg1+irg2.ge.1) then
        if(istart.eq.1) then  
          do i=1,maxrg
c
c rrb 01/08/10; Revise Qdebtx to be initial condition of 
c               surplus/shortage (qcarry)
c          qcarry(i)=0.0
	         qcarry(i)=qdebtx(i)
	        end do

	       do iy1=iystr, iyend
	         do im=1,13
	           do n=1,maxrg 
	             irec=(iy1-iystr)*13*maxrg+(im-1)*maxrg+n
c                    write(nlog,*) '  Bomsec, irec', irec
	             write(66,rec=irec) (xzero(i), i=1,nrg)
	           end do
	         end do
	       end do
        endif
      endif
c
c
c _________________________________________________________
c               Step 3; Initilize reservoir data 1x/run
c                       or on reservoir admin date
c			See ----
c
cr    if(numres.eq.0) goto 160
      if(ichk.eq.94) write(nlog,*)'  Bomsec; 3 Reservoirs'

      if(numres.gt.0) then
        DO 150 NR=1,NUMRES
          if(ioutR.eq.1) then
            write(nlog,152) nr, cresid(nr), rdate(nr)
 152        format(' Bomsec; nr, cresid', i5, 1x, a12, f5.1) 
          endif
			        
c
c              Branch if this reservoir is not operated (iressw = 0)
  	  IF(IRESSW(NR).EQ.0) GO TO 150
c
c rrb 04/23/97
c ---------------------------------------------------------
c			Initilize reservoir data 1x/run or the admin date
c			Note GetRes adjustes input so that
c			1 in input corresponds to Jan for
c			the simulation year 
c		
         if(ifix(rdate(nr)).eq.mon .or. ifix(rdate(nr)).lt.0 
     1                      .or. istart.eq.1) then
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
	        write(nlog,*) ' Bomsec; no,noi,noe,curown(no),cursto1'                         
	        write(nlog,*) ' Bomsec;', no,noi,noe,curown(no), cursto1
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
c		   reservoir operating date

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
c          if(ichk.eq.94) write(nlog,*)'  Bomsec; 5 Reservoir rights '

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
	          write(nlog,*) ' Bomsec; nr,l2,irsrsw(l2),dcrres(l2)'
	          write(nlog,*) ' Bomsec;',nr,l2,irsrsw(l2),dcrres(l2)
	        endif  
c
c rrb 2006/06/19; Revise to check against iresopr not irsrsw.
c		  Note iResOpr is defined to equal irsrsw in riginp.
c		  It stays on even if the reservoir right
c		  is part of an operating rule which turns off the
c		  original right
cr	        if(irsrsw(l2).eq.0) goto 140
cr	        if(irsrsw(l2).gt.0 .and. iyr-irsrsw(l2).lt.0) goto 140
cr	        if(irsrsw(l2).lt.0 .and. iyr+irsrsw(l2).gt.0) goto 140	      
	        
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
c                 c = dcrres(l2) - cursto1(nr1)
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
cr                if(nr.eq.1) write(nlog,130)
                  if(l2.eq.1) write(nlog,130)
                  write(nlog,132)  nr,cresid(nr), creswr(l2),
     1              iyrmo(mon),xmonam(mon),l1, l2, n2fill(l2),nr,nr1,
     1              rdate(nr), dcrres(l2),tot1x(l2),cursto(nr),
     1              cursto1,cursto2, ritrem(l2), RitPaper(l2),
     1              ritremx(nr)
                  endif
	      endif
c  
c ---------------------------------------------------------
c		End water right loop	      
  140      continue
c  
c ---------------------------------------------------------
c		Endif for admin date (rdate(nr) and start date (istart=1)
         endif
c  
c ---------------------------------------------------------
c		End reservoir loop
  150  continue
       if(ichk.eq.94) write(nlog,*)'  Bomsec; End of Reservoir Loop'
c  
c ---------------------------------------------------------
c rrb 2006/05/24; Limit OOP right to senior right 
      if(ichk.eq.94) write(nlog,*)'  Bomsec; X OOP right ', numrsr
c
c rrb 2006/05/31; Correction causing an overflow
cr      do iw=1,ntorig
        do iw=1,numrsr
          if(ityrsr(iw).eq.-1) then
c
c rrb 2006/06/13; Warn new approach to OOP diversions
            write(nlog,785) creswr(iw)
            goto 9999
                      
cr          l3=iopid(iw)          
cr          if(l3.eq.0) then
cr            write(nlog,785) creswr(iw)
cr            goto 9999
cr          endif
cr          nrwro = iopsou(3,l3)              
cr          ritrem(iw) = amin1(ritrem(iw), ritrem(nrwro))  
c
c rrb 2006/06/05; Paper Fill
cr          ritPaper(iw)=RitRem(iw)            
          endif
        end do    
c  
c ---------------------------------------------------------
c		End Reservoir If statement
      endif
c
c _________________________________________________________
c               Step 6; Begin Monthly Reservoir Initilization
c
  160 IF(NUMRES.EQ.0) GO TO 250
      if(ichk.eq.94) write(nlog,*)'  Bomsec; 6 Monthly Reservoirs '
  
      IF(NUMMIN.EQ.0) GO TO 190
C
c		Note mummin=numres via Mdainp
      DO 180 NM=1,NUMMIN
c
c               irsmin(nm) is a pointer to a reservoir ID
	      IM=IRSMIN(NM)
	      VOLMIN(IM)=CONMIN(MON,NM)

c
c _________________________________________________________
c               Step 7; Initilize targets 1x/month
c grb 12/04/94 added variable for max reservoir contents
c
	      tarmax(im)=targetx(mon,nm)
	
c
c rrb 02/27/95; Code Addition, Target is a function of current storage
c               and forcast data (a negative target)
	      if(targetx(mon,nm) .lt. -0.1) then
c
c              Calculate months remaining + 1
	      monrem = 1
	      do 170 j=mon,12
	        if(targetx(j,nm).lt. -0.1) monrem = monrem + 1
	        if(targetx(j,nm).gt. 0.1)  inextar= targetx(j,nm)
	        if (targetx(j,nm).gt. 0.1) go to 171
  170   continue

	      do 172 j=1,12
	        if(targetn(j,nm).lt. -0.1) monrem = monrem + 1
	        if(targetn(j,nm).gt. 0.1)  inextar= targetn(j,nm)
	        if(targetn(j,nm).gt. 0.1) go to 171
  172   continue
c        
c grb 3-19-96 Set index to appropriate reservoir id
c
  171     tarmax(im) = cursto(im)-(cursto(im)+(targetx(mon,nm)*(-1.0))
     1                 - inextar)/float(monrem)
	        tarmax(im) = amax1(tarmax(im), volmin(im))
	  
          if(ioutT.eq.1) write(nlog,*) ' Bomsec; ', mon,cursto(im),
     1       targetx(mon,nm), inextar

	         targetx(mon,nm) = tarmax(im)
	        endif
	
	      if(ioutT.eq.1) then
	        write(nlog,*) ' Bomsec; mon, nm, im, cresid(im) ... '
	        write(nlog,*)  ' Bomsec; ', 
     1      mon, nm, im, cresid(im),      
     1	    tarmax(im), targetX(mon,nm)
        endif
	
c
  180 CONTINUE
C
  190 continue
  
      if(ichk.eq.94) write(nlog,*)'  Bomsec; 8 Reservoirs '
  
       DO 220 NR=1,NUMRES
C
	       IF(IRESSW(NR).EQ.0) GO TO 220
c
c _________________________________________________________
c               Step 8; Initilize reservoir and account totals 1x/mo
c

c       STOMON(NR)=0.
	      PROJTF(NR)=0.
	      REPLAC(NR)=0.
	      POWREL(NR)=0.
	      ritremx(nr) = 0.0
	      VOLINT(NR)=CURSTO(NR)
	
c
c rrb 2006/04/16; Reservoir Seepage, Seepage Loss (rlossR) and 
c                 Reservoir Seepage Plan (prRes)
        sepact(nr)=0.0
        sepact1(nr)=0
        nSepCal(nr)=0
        rlossR(nr)=0.0
c
c _________________________________________________________
c               Step 9; Initilize reservoir account data 1x/mo
c rrb 01/05/95; I/O Addition; 

	      noi=nowner(nr)
	      noe=nowner(nr+1)-1
c
	     do 210 no=noi,noe
	       do 200 i=1,maxacc
	         accr(i,no)=0.0
  200     continue
	        accr(20,no) = curown(no)
  210   continue
  220 CONTINUE
c
c rrb 04/03/96; Set unused decreed amount to total
c               reservoir (ritremx) and accounts accr(23,n)
      do 232 l2=1,numrsr             
	      nr1 = iresco(1,l2)
c
c               Do not show Out of Priority rights as part of the total
c rrb 2006/07/25; No OOP rights (by reservoir)are longer used
cr      if(ityrsr(l2).ne.-1) then
	        ritremx(nr1) = ritremx(nr1)+ritrem(l2)
cr	    endif

	     iown=nowner(nr1)+iresco(2,l2)-1
	     do 230 n=1,nrown(l2)
	       n1 = iown+n-1
c              write(nlog,*) '  Bomsec, iown, n, n1, l2',iown,n,n1,l2
	       accr(23,n1) = accr(23,n1) + ritrem(l2)
  230  continue               
  
c
c rrb 2006/07/25; Enhancement                
        if(ioutR.eq.3) then
          if(l2.eq.1) write(nlog,133)
          write(nlog,134)  nr1,cresid(nr), 
     1              iyrmo(mon),xmonam(mon), l2, n2fill(l2),nr1,
     1              rdate(nr1), dcrres(l2),cursto(nr1),
     1              ritrem(l2), ritremx(nr1)
        endif
        
  232 continue
C
C------  INITIALIZE MONTHLY OWNERSHIP
C
      DO 240 NO=1,NUMOWN
 	      QMAINS(1,NO)=0.
  240 OWNMON(NO)=0.
c
c __________________________________________________________
c               Step 10; Initilize diversion (divmon), loss (rloss),
c                       Diversion to soil moisture (qdivs) and from
c                       soil moisture (qdivso) 1x/mo
c
      if(ichk.eq.94) write(nlog,*)'  Bomsec; 10 Diversions '

  250 DO ND=1,NUMDIV
	      DIVMON(ND) = 0.0
	      rloss(nd)  = 0.0
	      qdivs(nd)  = 0.0
	      qdivso(nd) = 0.0
	
      	dIwrSF(nd) = 0.0
      	dIwrSS(nd) = 0.0
      	dIwrGF(nd) = 0.0
      	dIwrGS(nd) = 0.0
      
      	cuActSF(nd) = 0.0
      	cuActSS(nd) = 0.0
      	cuActGF(nd) = 0.0
      	cuActGS(nd) = 0.0
      end do
c
c ---------------------------------------------------------
c               10v. Initilize ... 1x/mo
      
      do no=1,numownd
        divownQ(no)=0.0
      end do  
c
c __________________________________________________________
c               Step 11; Initilize well (divmonw), loss (rlossw),
c                       Diversion to soil moisture (qdivsw) and from
c                       soil moisture (qdivswo) and carried by
c			   a well (carryW) 1x/mo

c
c rrb; Wells
      if(ichk.eq.94) write(nlog,*)'  Bomsec; 11 Wells '

      do nw=1,numdivw
	      divmonw(nw) = 0.0
	      rlossw(nw)  = 0.0
	      rlossw2(nw) = 0.0
	      rdepw(nw)   = 0.0
	      qdivsw(nw)  = 0.0
	      qdivswo(nw) = 0.0
	      carryW(nw)  = 0.0
	      
	      dIwrGFw(nw) = 0.0
	      dIwrGSw(nw) = 0.0

	      cuActGFw(nw) = 0.0
	      cuActGSw(nw) = 0.0
      end do
c
c _________________________________________________________
c               Step 12; Route any excess soil moisture ocurring 
c                       because area is reduced into loss
c                       Note although this occurrs at most 1x/yr
c                       locate here after loss (rloss) is initilized
      if(ichk.eq.94) write(nlog,*)'  Bomsec; 12 Soil Moisture '

cr    iprints=0
      rlosst=0.0
      do nd=1,numdiv
	      c=soils(nd)-awcr(nd)
c       write(nlog,*) '  Bomsec; nd,soils,acwr',nd,soils(nd),awcr(nd),c
       	if(c.gt.small) then
       	  soils1=soils(nd)
c
c rrb 01/08/06; Do not show change in soil moisture as a loss
c         rloss(nd)=c/fac
	        rloss1=c/fac
	        soils(nd)=awcr(nd)
	        if(iprintL.eq.0) then
	          write(nlog,1281) iyr, 'Soil Moisture Storage   '
                  iprintL=iprintL+1
                endif
            
	          if(iprints.eq.0) write(nchk,780)
	          iprints=iprints+1
	          write(nchk,782) iprints, cdivid(nd), 
     1      soils1, awcr(nd), rloss1*fac, 
     1      divnam1(nd)
	          rlosst=rlosst+rloss1
	        endif
      end do
      
      do nw=1,numdivw
	      nd=idivcow2(nw)
	      if(nd.eq.0) then
c       write(nlog,*) '  Bomsec; nw,soils,acwrw',
c    1                   nw,soilsw(nw),awcrw(nw),c
	      c=soilsw(nw)-awcrw(nw)
	      if(c.gt.small) then  
	        soilsw1=soilsw(nw)
c
c rrb 01/08/06; Do not show change in soil moisture as a loss 
c           rlossw(nw)=c/fac
	        rloss1=c/fac
	        soilsw(nw)=awcrw(nw)
          
	        if(iprints.eq.0) write(nlog,780)
	        iprints=iprints+1
	        write(nchk,782) iprints, cdividw(nw), 
     1        soilsw1, awcrw(nw), rloss1*fac, 
     1        divnamw1(nw) 
c
c rrb 01/09/19; Correction inclde total from well only lands
	          rlosst=rlosst+rloss1  
	        endif
	      endif
      end do
c
c               Print total soil moisture lost
cr    if(iprints.eq.0) then
      if(istart.eq.1 .and. rlossT.gt.small) then
	      write(nchk,783) rlosst*fac 
      endif   

c
c _________________________________________________________
c               Step 13; Initilize running diversion by a right 1x/mo
c grb add initialization for water right diversions
      if(ichk.eq.94) write(nlog,*)'  Bomsec; 13 Rights '

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
      end do

      do nd=1,numdvr
	      divd(nd)=0.
      end do

      do nw=1,numdvrw
	      divdw(nw)=0.
      end do

      do k=1,numopr
	      divo(k)=0.
      end do
c
c _________________________________________________________
c		Operational right maximum limit 1x/mo      
      
      do k=1,numopr      
        divdS(k)=0.0
        divdE(k)=0.0
        oprmaxM(k)=oprmax(k,mon)
        preuse(k)=0.0  
c
c rrb: 2015-07-08; Add capabiity to control the number of times
c                  an operating rule has operated per time step. 
c                  Currently used by Type 6 (bookover) and Type 26
c                  (Changed water right operation)
        icallOP(k)=0
c        
c
c 2009/01/15; Revise to allow source 2 to be the month a type 47 
c		rule is reset
        if(ityopr(k).ne.47) then        
          if(mon.eq.1) oprmaxA(k)=oprmax(k,13)
        else
          monX=iopsou(2,k)
 	        np=iopsou(1,k)
 	   
 	        if(iPlnTyp(np).ne.12) then
 	          write(nlog,*)
     1        ' Bomsec; Problem with initilizing type 47 rule ',
     1        np, iPlnTyp(np)
 	          goto 9999
 	        endif
         
          if(monX.eq.imomo(mon)) then	    
            oprmaxA(k)=oprmax(k,13)            
	          psuplyT(np) = oprmax(k,13)/fac
	          psuply(np)  = psuplyT(np)
c
c rrb 2008/01/16; Store as a volume (ac-ft)            
c	     
	          psto1(np)=oprmaxA(k)
	          psto2(np)=oprmaxA(k)
          else	     
            psuplyT1=psuplyT(np)    
            psuplyT(np)=psuply(np)*facM1/fac

            psto1(np)=psto2(np)
          endif
          
cx        write(nlog,*) ' Bomsec; iout47_2', iout47   
          if(iout47.eq.1) then
            write(nlog,*)        
            write(nlog,*) ' Bomsec; Type 47 data 1x per month'
            write(nlog,'(a12, 1x, 5i5, 20f8.0)') 
     1        corid(k), k, np, mon, 
     1        iyrmo(mon), imomo(mon), OprmaxA(k),
     1        oprmaxM(k), psto1(np), psto2(np)
          endif
        endif
      end do
c
c _________________________________________________________
c               Step 14; INITIALIZE MONTHLY DIVERSION FOR EACH USER 
c                        1x/mo
C
      if(ichk.eq.94) write(nlog,*)'  Bomsec; 14 Diversions '

      DO NU=1,NUMUSE
	      USEMON(NU)=0.
c
c rrb 00/12/09; For variable efficiency add running CU for diversions               
	      dcu(nu)=0.0
	      dcut(nu)=0.0
      end do
c
c rrb 98/12/02; Add Wells
      do nw=1,numdivw
      	usemonw(nw)=0.
c
c rrb 00/12/09; For variable efficiency add running CU for wells               
	      dcuw(nw)=0.0
	      dcutw(nw)=0.0
      end do
c     
c _________________________________________________________
c               Step 15; Initilize Instream flow demand (FLOWRQ) 1x/mo
c
      if(ichk.eq.94) write(nlog,*)'  Bomsec; 15 ISF '

      DO 320 NI=1,NUMIFR
	      FLOWRQ(NI)=FLOWR(MON,NI)
	      ib=ndnifb(ni)
	      ie=ndnifb(ni) + ndnifs(ni) - 1
c
c rrb 08/07/96 Instream Flow reach
	      do i=ib,ie
	        florqr(i)=flowrq(ni)
	        qdivr(i)=0.0
	        
	        if(ioutI.eq.1) then
	          write(nlog,*) ' Bomsec; ni, flowrq ', i,
     1	    iyrmo(mon),xmonam(mon), idy, ni, 
     1      florqr(i)*fac,flowrq(ni)*fac	   
          endif     
	      end do  
	
	      if(ioutI.eq.1) then
	        write(nlog,*) ' Bomsec; ni, flowrq Total',
     1	  iyrmo(mon),xmonam(mon), idy, ni,
     1    flowrq(ni)*fac, flowrq(ni)*fac
        endif
  320 continue
c
c _________________________________________________________
c               Step 16; Check and set default efficiency data 1x/mo
c
c rrb 02/10/09; Removed to seteff so it can be used by the
c               baseflow module also (called by virset)
      if(ichk.eq.94) write(nlog,*) ' Bomsec; Calling Seteff'  
c     write(nlog,*) ' Bomsec; Calling Seteff'   

      call seteff
      if(ichk.eq.94) write(nlog,*) ' Bomsec; Out of Seteff'
c     write(nlog,*) ' Bomsec; Out of Seteff'
cxc
cxc _________________________________________________________
cxc               Step 17; Initilize Diversion Demand (DIVREQ) 
cxc               Note may be adjusted to include well demands below
cxc		1x/mo
cxc rrb 2007/10/02; Already included in Demand
cx      DO NU=1,NUMUSE
cx	DIVREQ(NU)=DIVER(MON,NU)
cxc
cxc rrb 00/12/09; For variable efficiency add running IWR               
cx	diwrreq(nu)=diwr(mon,nu)
cxc
cxc rrb 2006/09/11; New CU Approach
cx        dIwrSF(nu)=diwrreq(nu)*AreaSF(nu)	
cx        dIwrSS(nu)=diwrreq(nu)*AreaSS(nu)	
cx        dIwrGF(nu)=diwrreq(nu)*AreaGF(nu)	
cx        dIwrGS(nu)=diwrreq(nu)*AreaGS(nu)
cxc
cx      end do
c
c _________________________________________________________
c               Step 17; Initilize well demand (divreqw) 1x/mo
c
      maxdivx=maxdiv
      maxdivwx=maxdivw
        
      if(ichk.eq.94) write(nlog,*) ' Bomsec; Calling Demand'        
      call demand(0,maxdivx,maxdivwx) 
      if(ichk.eq.94) write(nlog,*) ' Bomsec; Out of Demand'     
c
c _________________________________________________________
c               Step 18; Initilize Call indicators 1x/mo
c			Note ..L = call location
c                            ... = print indicator
      do i=1,numsta
        imcdL(i)=-1
      end do
c
c _________________________________________________________
c               Step 19; Initilize River and Avinp 1x/mo
c
      DO IS=1,NUMSTA
	      RIVER(IS)=0.
	      AVINP(IS)=0.
	      QTRIBU(IS)=0.	
      end do
C
      DO ND=1,NUMDIV
      	QSTERN(ND)=0.
      end do
c
c _________________________________________________________
c               Step 21; COMPUTE VIRGIN FLOW AT EACH STATION 1x/mo
c
      DO 370 IRU=1,NUMRUN
	     ISS=IRUSTA(IRU)
	     QTRIBU(ISS)=QTRIBU(ISS)+VIRINP(MON,IRU)
	     RIVER (ISS)=RIVER (ISS)+VIRINP(MON,IRU)
	     ISS=IDNCOD(ISS)
	     IF(ISS.EQ.0) GO TO 370
	     NDNN=NDNNOD(ISS)
       
	     DO 360 ND=1,NDNN
	       RIVER (ISS)=RIVER (ISS)+VIRINP(MON,IRU)
	       AVINP (ISS)=AVINP (ISS)+VIRINP(MON,IRU)
  360  ISS=IDNCOD(ISS)
  370 CONTINUE
C
      DO 380 ND=1,NUMDIV
	      IS=IDVSTA(ND)
c
	      if(is .eq. 0) goto 380
	      qstern(nd)=river(is)
  380 continue
c
c _________________________________________________________       
c               Step 22; Add RETURNS TO RIVER, AVINP AND QTRIB ARRAYS
c                        1x/mo
c
      DO 400 IRN=1,NSTRTN
	      ISS=ISTRTN(IRN)
c       write(nlog,*) ' Bomsec; irn, iss', irn, iss
c
	      QTRIBU(ISS)=QTRIBU(ISS)+RETUR(IMO,IRN)-depl(imo,irn)
	      RIVER (ISS)=RIVER (ISS)+RETUR(IMO,IRN)-depl(imo,irn)
	      ISS=IDNCOD(ISS)
	      IF(ISS.EQ.0) GO TO 400
	      NDNN=NDNNOD(ISS)
      
	      DO 390 ND=1,NDNN
c     
	        RIVER (ISS)=RIVER(ISS)+retur(IMO,IRN) -depl(imo,irn)
	        AVINP (ISS)=AVINP(ISS)+retur(IMO,IRN) -depl(imo,irn)
  390     ISS=IDNCOD(ISS)
  400 CONTINUE
c
c _________________________________________________________       
c               Step 23; Adjust River for ground water storage use
c                        1x/mo. 
c                        Also set Avail = River
      if(ichk.eq.94) write(nlog,*) ' Bomsec; Calling Gwsub'        
      call gwsub
      if(ichk.eq.94) write(nlog,*) ' Bomsec; Out of Gwsub'
      
c _________________________________________________________ 
c               Step 24; Set currtn 1x/mo
c
      DO IS=1,NUMSTA
	      CURRTN(IS)=0.
      end do
c
c _________________________________________________________
c               Step 25; Check for roundoff 1x/mo
c
      if(ichk.eq.94) write(nlog,*) ' Bomsec; Calling RoundOf'
      
      call roundof(river, numsta, 1, 1, nbug)
      call roundof(avinp, numsta, 1, 1, nbug)
      call roundof(avail, numsta, 1, 1, nbug)
c
      if(nbug.eq.1) then
	      write(nlog,*) '  Bomsec; Problem negative river node'
	      goto 9999
      endif
c
c _________________________________________________________
c               Step 26; Compute Seepage 1x/mo
c
c rrb 2006/04/12; Moved to Execut
cr    CALL SEPSEC             
c
c _________________________________________________________
c               Step 27; Initilize qdiv 1x/mo
c
      DO I=1,NUMSTA
	      DO J=1,maxqdiv
	        QDIV(J,I)=0.
	      end do
      end do
c
c _________________________________________________________
c               Step 28; Initilize qres 1x/mo
c
      DO nr=1,NUMRES
	      DO J=1,maxqdiv
	        QRES(J,nr)=0.
	      end do
      end do
c
c _________________________________________________________
c               Step 29; Initilize interruptable supply once per year
c rrb 99/06/29; Set opr switch and source water right switch on,
c               if appropriate
c
c rrb 99/07/01; Allow opr rule to roll into next year
      if(iSetOpr(15).gt.0) then
        do k=1,numopr
	        if(ityopr(k).eq.15 .and. ioprsw(k).ne.0) then
        
	        isr = iopsou(2,k)
	        isd = idivco(1,isr)
	        iuse= nduser(isd)
	        demopr(iuse)=divreq(iuse)
c
c               Reset for new operating rule year
	        if(imonsw(k,mon).lt.0) then
	          if(ioprsw(k).ne.0) ioprsw(k) = 1
	          if(idvrsw(isr).ne.0) idvrsw(isr) = 1
	        endif
	      endif
	      end do
c
c rrb 99/06/29; Interruptable supply initilization
c               For the first month of potential operation (imonsw()=2)
c               Check if the interruptable supply operation rule is
c               in effect (it operates based on the flow at a certain
c               point on the river)
c       write(nlog,*) ' '
c       write(nlog,*) '  Bomsec;; mon, imomo(mon), mthday(mon)',
c    1                          mon, imomo(mon), mthday(mon)
c       write(nlog,990) (dumsta(is)*fac, is=1,numsta)
c       write(nlog,990) (river(is)*fac,  is=1,numsta)
c990    format(10f10.2,/,(10f10.2))

        do k=1,numopr
          if(ityopr(k).eq.15 .and. imonsw(k,mon).eq.2) then
          
	          if(ioprsw(k).ne.0) then
              if(ichk.eq.94) write(nlog,*) ' Bomsec; Calling InterSup'
	            call intersup(0,k,0)
              if(ichk.eq.94) write(nlog,*) ' Bomsec; Out of InterSup'
            endif 
	    
	        endif
        end do
      endif  
cxc
cxc _________________________________________________________
cxc               Step 30; Detailed demand data check 1x/mo
cxc     write(nlog,*) '  Bomsecx; mon, ichk', mon, ichk
cx
cx      if(mon.eq.1) ichkx=ichk
cxc     if(mon.eq.ichkx .or. ichkx.eq.91) then
cx      if(ichkx.eq.91) then
cx	write(nlog,420) iyr, mon
cx
cx	do nd=1,numdiv
cxc         if(cdivid(nd).eq.'200631      ') then
cx	  nw=idivco2(nd)
cx	  if(nw.gt.0) then
cxc
cxc rrb 2007/10/01; Remove demspr	  
cx            effs1=effmaxs(nw)/100.0
cx            efff1=effmaxw(nw)/100.0
cx            diwrGS1=diwrGSw(nw)
cx            diwrGF1=diwrGFw(nw)
cx            divsprX=diwrGS1/EffS1
cx            divothX=diwrGF1/EffF1
cx            divreqX=divsprX+divothX
cx        
cx	  
cx	    write(nlog,*) ' '
cx	    write(nlog,430)    
cx     1      cdivid(nd), nd, nw,
cx     1      diver(mon,nd)*fac,  diveff(mon,nd),  diverir(mon,nd)*fac, 
cx     1      divert(mon,nd)*fac, diverirt(mon,nd)*fac,
cx     1      diwr(mon,nd)*fac, 
cx     1      divreq(nd)*fac,     diwrreq(nd)*fac,
cx     1      0.0,
cx     1      area(nd)
cx
cx	    write(nlog,430)
cx     1      cdividw(nw), nd, nw,
cx     1      diverw(mon,nw)*fac, diveffw(mon,nw), diverirw(mon,nw)*fac,
cx     1      divert(mon,nd)*fac, diverirt(mon,nd)*fac,
cx     1      diwrw(mon,nw)*fac,
cx     1      divreqw(nw)*fac,    diwrreqw(nw)*fac,
cxc    1      demspr(nw)*fac,
cx     1      demsprX*fac,
cx     1      areaw(nw),         areasp(nw),        areawa(nw)
cx
cx	  else
cx	    write(nlog,*) ' '
cx	    write(nlog,430) 
cx     1      cdivid(nd), nd, nw,
cx     1      diver(mon,nd)*fac,  diveff(mon,nd),  diverir(mon,nd)*fac, 
cx     1      divert(mon,nd)*fac, diverirt(mon,nd)*fac,
cx     1      diwr(mon,nd)*fac, 
cx     1      divreq(nd)*fac,     diwrreq(nd)*fac,
cx     1      0.0,
cx     1      area(nd)
cx
cx	  endif
cx	end do
cx
cx	do nw=1,numdivw
cx	  nd=idivcow2(nw)
cx	  if(nd.eq.0) then
cxc
cxc rrb 2007/10/01; Remove demspr	  
cx            effs1=effmaxs(nw)/100.0
cx            efff1=effmaxw(nw)/100.0
cx	  
cx            diwrGS1=diwrGSw(nwe)
cx            diwrGF1=diwrGFw(nwe)
cx            divsprX=diwrGS1/EffS1
cx            divothX=diwrGF1/EffF1
cx            divreqX=divsprX+divothX
cx	  
cx	    write(nlog,*) ' '
cx	    write(nlog,430)
cx     1      cdividw(nw), nd, nw,
cx     1      diverw(mon,nw)*fac, diveffw(mon,nw), diverirw(mon,nw)*fac,
cx     1      diverw(mon,nw)*fac, diverirw(mon,nw)*fac,
cx     1      diwrw(mon,nw)*fac,
cx     1      divreqw(nw)*fac,    diwrreqw(nw)*fac,
cxc    1      demspr(nw)*fac,
cx     1      demsprX*fac,
cx     1      areaw(nw),         areasp(nw),        areawa(nw)
cx	  endif
cx	end do
cx      endif
c
c _________________________________________________________
c               Step 31; Set Monthly downstream call data 1x/mo
      if(idcall.gt.0) then
        dcall1=dcallm(mon)
c       write(nlog,*) ' Bomsec; iyr, imo, idy, dcall1',
c    1    iyrmo(mon), xmonam(mon), 1, dcall1
      endif
c
c _________________________________________________________
c               Step 32; Initilize plan demand (Pdem) 1x/mo
c		Note pfail is in volume acft to maintain continuity
c		note imo is the circular pointer for future arrays
c		while mon is the month
      do np=1,nplan
        pdem(np)=pobl(imo,np) + pfail(np)/fac
        pdemT(np)=pobl(imo,np) + pfail(np)/fac
        pevap(np)=0.0
        
        pwellC(np)=0.0
        pdrive(np)=0.0
c
c rrb Revise to carry over storage for reservoir plans (type 3 or 5)		        
c     Note facm1/fac is ratio of days per month
c     Note psto1 is in acft
        iPtype1=iPlnTyp(np)
c
c ---------------------------------------------------------
c		Set supply data for 3-Reservoir Reuse,
c               5-Tmtn Reservoir Reuse, and 9-OOP Plan
        if(iPtype1.eq.3 .or. iPtype1.eq.5 .or. 
     1     iPtype1.eq.9) then
          psuply1=psuply(np)
          psuply(np)=psup(imo,np) + psuply(np)*facm1/fac
c
c rrb 2006/01/04; Correction  NO
          psuplyT(np)=psup(imo,np)          
cx        psuplyT(np)=psuply(np)  
          
          psto1(np)=psto2(np)
          
          if(ioutp.ge.1) then
            ioutp1=ioutp1+1
            if(ioutp1.eq.1) write(nlog,260)
            
            write(nlog,262)  iyr, imo, pid(np), np, iptype1, 
     1        psuply1,psuply(np), psuplyT(np), psto1(np), psto2(np) 
          endif
        endif
c
c ---------------------------------------------------------
c		Set supply data for 1-T&C, 2-Well Aug, 4-Div Reuse,
c               6-Tmtn Div Reuse, 7-Tmtn Reuse, 8-Rech Plan,
c               10-Special Well Aug, 11-Admin, 13-Changed Water Right
c rrb 2015/03/07; Add a Changed Water Right Plan (type 13)
cx   1     iPtype1.eq.10 .or.  iPtype1.eq.11)    then
        if(iPtype1.eq.1  .or.  iPtype1.eq.2 .or. iPtype1.eq.4 .or.
     1     iPtype1.eq.6  .or.  iPtype1.eq.7 .or. iPtype1.eq.8 .or.
     1     iPtype1.eq.10 .or.  iPtype1.eq.11.or. iPtype1.eq.13) then
     
          psuply(np)=psup(imo,np)
          psuplyT(np)=psup(imo,np)
          
        endif  
c
c ---------------------------------------------------------
c		Set supply data for 12-Release Limit
c rrb 2008/101/15; Moved to an operating rule loop above 
c		     This allows the reset month to be specified
c		     as a function of the operating rule (k) and
c		     because psuply is in cfs it must be adjusted
c		     for # of days per month change
cxx        if(iPtype1.eq.12) then    
cxxc
cxxc rrb 2009/01/16; Adjust for number of days in this month
cxx          monX=iopsou(2,k)
cxx          if(monX.ne.imomo(mon)) then	    
cxx		        
cxx            psuplyT1=psuplyT(np)    
cxx            psuplyT(np)=psuply(np)*facM1/fac
cxx          
cxx            write(nlog,*) ' Bomsec; setting Monthly type 47 data  af', 
cxx     1        iyrmo(mon), imomo(mon), np, fac/facM1, 
cxx     1        psuply(np)*fac, psuplyT(np)*fac, psuplyT1*fac          
cxx     
cxx            write(nlog,*) ' Bomsec; setting Monthly type 47 data cfs', 
cxx     1        iyrmo(mon), imomo(mon), np, fac/facM1, 
cxx     1        psuply(np), psuplyT(np),  psuplyT1          
cxx          endif
cxx        endif  
c
c ---------------------------------------------------------
c		Initilize amount in a reuse plan here since at least
c               part of it is calculated from a diversion in a prior
c               time step
c
c rrb 2006/04/18; Do not adjust for a Reservoir reuse plan since
c		  returns are in the return array
c rrb 2008/01/14; Correction
cx        if(iPtype1.ne.8 .and. iPtype1.ne.12) then        
cx          is=ipsta(np)        
cx          qdiv(28,is)=psuplyT(np)
cx        endif  
        
c
c ---------------------------------------------------------
c		Set imports        
        PImport(np)=PImportX(mon,np)
        PImportT(np)=0.0
c       write(nlog,*) ' Bomsec Import; np, PImport',np,PImportM(np)*fac
        
c
c ---------------------------------------------------------
c		Detalied Output
        if(ioutp.eq.1) then
         write(nlog,*) ' '
         write(nlog,*) 
     1   ' Bomsec Demand; np imo pid         pdem(np)   pdemT psuplyT' 
         write(nlog,*)
     1   ' Bomsec Demand; ', np,imo,pid(np),pdem(np)*fac,
     1    pdemT(np)*fac, psuplyT(np)
        endif  
      end do
c
c _________________________________________________________
c       	Step 33; Initilze compact demand 1x/mo if a Type 13
c               (La Plata Compact) operating rule is on.
c		Note flowrX(im,nf) is the data read from *.ifa
c
      if(isetOpr(13).gt.0) then
        do k=1,numopr
          if(ityopr(k).eq.13 .and. ioprsw(k).ne.0) then
            nf  =iopdes(1,k)	
            ifcd=ifrsta(nf)          
            mon2 = imonsw(k,mon)
          
            if(mon2.eq.0) then
              cp=0.0
              isx = iopsou(1,k)
              riverx=river(isx)              
              flowrq(nf)=0.0
              flowr(mon,nf)= 0.0
              avindx=0.0
              demmax=-1.0/fac
              demlim=-1.0/fac
            else
              isx = iopsou(1,k)
              riverx=river(isx)
              cp=float(iopsou(2,k))/100.0
              demmax = riverx * cp
c
c
c rrb 2005/10/25; Limit to demand provided in *.dda
c             flowrq(nf)=avindx
cr            demlim=flowrq(nf)
              demlim=flowrX(mon,nf)
cr              write(nlog,*) '  Bomsec; mon,nf, flowrX(mon,nf), demlim'
cr              write(nlog,*) '  Bomsec;', mon,nf, flowrX(mon,nf), demlim
              flowrq(nf)=amin1(demlim, demmax)
              flowr(mon,nf)= flowrq(nf)
              avindx=flowrq(nf)
            endif

            if(ichk.eq.113) then
              write(nlog,270) corid(k)        
              write(nlog,280) iyrmo(mon),xmonam(mon),idy,
     1        iwx,mon2,nf,ifcd,isx, iopsou(2,k),
     1        riverx*fac, cp*100., demmax*fac, demlim*fac, avindx*fac,
     1        xfrnam1(nf)
            endif
          endif
        end do  
      endif  
      
          
c
c _________________________________________________________
c               Step 34; Return
   500 RETURN
c
c _________________________________________________________
c               Formats
 130  format(
     1 '  Bomsec;   nr ID            ID_WR        ',
     1 '     iyr     mon      l1      l2  n2fill      nr     nr1',
     1 '   rdate  dcrres   tot1x  cursto cursto1 cursto2  ritrem',
     1 ' RitPapr RitremX',/
     1 ' ________ ____ ____________  ____________ ', 7(' _______'),
     1 9(' _______'))
 132  format('  Bomsec ', i5,1x,a12,1x,a12,1x,i8,4x,a4,5i8, 20f8.0)

 133  format(
     1 '  Bomsec;   nr ID           ',
     1 '     iyr     mon      l2  n2fill     nr1',
     1 '   rdate  dcrres  cursto  ritrem RitremX',/
     1 ' ________ ____ ____________ ', 5(' _______'),
     1 5(' _______'))
 134  format('  Bomsec ', i5,1x,a12,1x,i8,4x,a4,3i8, 20f8.0)
     
 260  format(/,           
     1 '  Bomsec Plan Data;  ',
     1 '    Year   Month Plan ID      ',
     1 '      np iptype1',
     1 ' psuply1  psuply psuplyT   psto1   psto2',/
     1 '  ________________',3x,
     1 2(' _______'), ' ___________ ', 7(' _______'))        
     
 262  format(
     1 '  Bomsec Plan Data;  ',2i8,1x,a12,1x,2i8, 20f8.2)
     
 270  format(/, 
     1  72('_'),//,
     1  '  Bomsec; (Type 13); Operation Right ID = ', a12,
     1  ' Demand Calculations',/
     1    '                                           ',
     1    '          ',
     1    '  IndexQ  Factor Max_Dem Ifa_Dem Com_Dem',/
     1    '  BomSec   Yr   Mo  Day Iter   On   nf ifcd  isx iso2',
     1    '  riverx  cp (%)  demmax  demlim  avindx',
     1    ' Name                     ',     /8x,
     1    ' ____ ____ ____ ____ ____ ____ ____ ____ ____',
     1    5(' _______'),' ________________________ ')
     
 280  format(
     1 '  BomSec', i5, 1x, a4, 7i5, 5f8.0,1x, a24)

 420  format('  Bomsec;  Year ', i5, ' Mon ', i3, /
     1'                      ',   
     1'  Tot_Dem      Eff   CU_Dem  Tot_Dem',   
     1'   Tot_CU      IWR  Tot_Dem   CU_Dem  Spr_Dem',      
     1'     Area  Area_SP  Area_GW',/
     1'                      ',     
     1'    diver   diveff  diverir   divert', 
     1' diverirt     diwr   divreq  diwrreq   demspr',      
     1'     area      N/A      N/A   ',/
     1'ID             Nd   Nw',    
     1'   diverw  diveffw diverirw   divert', 
     1' diverirt    diwrw  divreqw diwrreqw   demspr',    
     1'    areaw   areasp   areawa',/
     1'____________ ____ ____', 
     1' ________ ________ ________ ________', 
     1' ________ ________ ________ ________ ________', 
     1' ________ ________ ________')
 430  format(a12, 2i5, 30f9.0)

  757  format('  Bomsec; Warning canal efficiency is < 0. May ',/
     1          '          have a problem calculating total demand')
  770  format('  Bomsec; Warning (one time) maximum efficiency was',
     1        ' set to average = ', f8.2, ' for well ID ',a12) 
  780  format(/
     1  72('_'),//   
     1  '  Bomsec; Soil moisture > AWC.',/
     1  10x,'This occurs when area gets smaller over time.',/
     1  10x,'Setting soilM to AWC and balance is lost.',//
     1 '     # Structure ID  Soil (af)   AWC (af)  Lost (af) Name',/
     1 ' _____ ____________ __________ __________ __________', 24('_'))
  782  format(1x, i5, 1x, a12, 3(1x, f10.2), 1x, a24)
  783  format(1x, 'Total', 1x, 12x, 2(11x), (1x, f10.2), 1x, 24x)
  
  785  format(
     1  '  Bomsec; Problem reservoir right ', a12,/
     1  '          is an Out-of-Priority water right.',/
     1  '          Out-of-Priority diversions to a reservoir or',/
     1  '          diversion are now controlled by a type 38',/
     1  '          operating rule',/
     1  '          Reconmend you delete the OOP reservoir right',/
     1  '          and add a type 38 operating rule')

     
 1281  FORMAT(/,72('_'),/
     1  '  BomSec;  Warning See *.chk for details in year', i5,
     1  ' regarding ',a24,/
     1  '           Note only first occurance (year) is printed')


c
c _________________________________________________________
c               Error warnings
 9999 write(6,1050) 
      write(nlog,1051) 
    
 1050 format('    Stopped in Bomsec',/,
     1       '    See the *.log file')
 1051 format('    Stopped in Bomsec')
      write (6,*) 'Stop 1'
      call exit(1)
      stop 
      END
