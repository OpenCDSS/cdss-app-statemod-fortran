C     Last change:  RRB  28 May 2002    4:31 pm
c
c ---------------------------------------------------------
      subroutine divRplP2(iw,l2,divactx,ncallX)      
c
c
c _________________________________________________________
c	Program Description
c
c	DivRplP2; Type 49
c     It simulates a Reservoir or Reuse Plan or
c	    Acct Plan release to an ISF or a T&C Plan or a 
c	    Aug Plan or a Special Aug Plan by Exchange
c	    Also it can serve two T&C plans; a Total and a 
c	    secondary at the same time
c
c              Called by Execut
c _________________________________________________________
c
c       Update History
c
c
c rrb 2008/06/10; Allow return to river then diversion
c		  from a carrier again (internT = 1 Carrier, 2=Return)        
c
c rrb 2007/12/26; Copy DivRplP and edit appropriately
c
c _____________________________________________________________
c
c       Documentation
c
c
c ---------------------------------------------------------
c	VARAIABLES USED WHEN CALLED BY EXECUTE OR REPLACE 
c
c      iw                    water right order
c      l2                    order of operating rule
c      ishort                shortage indicator 0=no, 1=yes
c   
c      dcrdivx = dcrdiv(n)   water right
c      divdx = divd(n)       remaining water right amount
c   
c      divact                actual diversion
c      divactx               actual plan release from DivrplP2
c      divacty               actual diversion from DivrplP2 Note may
c                            not equal divactx if releasing for
c                            depletion
c   
c    Other Key variables
c      icx   = 6             subroutine call #
c      ieff2 = 0             always use average efficiency
c            = 1             use max efficiency if ieffmax = 1
c   
c	     internT     		      Intervening structure type
c		 	                        1 = Carrier
c		 	                        2 = River
c   
c      iopsou(1,l2)          ns 
c		                           if > 0 source reservoir
c                            if < 0 source plan
c   
c      iopsou(2,l2)          iown   NA
c      iopsou(3,l2)          T&C Plan id (for return flow obligation)
c		                             associated with a release
c      iopsou(5,l2)          1 Diversion limits are adjusted for the
c	                             operatng rule = iopsou(5,l2).
c	                           2 Release limits are imposed for the
c	                             operatng rule = iopsou(5,l2).
c      iopsou(6,l2)          Reservoir release type and efficiency
c                            0 = release to meet demand
c                            +n = release only if a CIR (IWR) exists
c                            and limit release to not exceed IWR/n,  
c                            Note nmax = min(nmax, effmax) to save 
c                            iterating
c   
c      iExPoint(l2)          exchange node
c      ireltyp               Same as iopsou(6,l2)
c   
c	     ipUse                 Reuse plan for return flows
c                            Note ipUse=ireuse(l2)
c   
c      iopdes(1,l2)          nd   destination diversion or reservoir
c      iopdes(2,l2)          iuse destination user
c   
c      iopdesr(l2)           Destination type
c		                          	2 = reservoir
c		                         	3 = diversion
c		                         	7 = plan
c   
c      ipsta(nsP) = iscd     Source Plan river station
c      idvsta(nd) = idcd     Destination diversion river station
c   
c	     idcd                  Actual Diversion location. May be one 
c                            anything
c	     idcdD			            Destintaion Diversion location on river
c	     idcdR                 Destination Reseroivr location on river
c	     idcdC                 Destination carrier location on river
c	     icdcX			            Actual Diversion location. May be one 
c                            anything BUT NOT A CARRIER
c   
c      ndnnod (idcd) = ndnD  number of nodes downstream of destination
c      ndnnod(iscd) = ndnS   number of nodes downstream of supply
c   
c      divcap(nd)            capacity
c      divmon(nd)            demand supplied this time step (grows
c                              with each diversion each iteration)
c      divreq(iuse)          demand remaining this time step
c                              (reduces with each diversion each
c                              iteration)
c   
c      divalo                allowable demand after adjusting for
c                              demand, capacity, amount already
c                              diverted this time step, water right
c                              limit, etc.
c      divact                actual diversion
c   
c	     nsP			              source is a Plan (iopsou(1,l2)<0
c	     nsR                   source is a Reservoir (iopous(1,l2>0)
c	     pavail                minimum available flow 
c   
c   
c      small                 a local limit used for convergence, etc.
c      iouta                 0 no detailed printout for avail
c                            n yes detailed printout for avail
c	     iwhy                  1 reason for no diversoin
c   
c	     ncallx		            number of times called per time step
c   
c	     ndtype 	            iopdesr(l2) Destination Type
c                           1 = 'Instrream'
c	                          2 = 'Reservoir'
c    		                    3 = 'Diversion'
c                           7 = 'Plan     '
c   
c ---------------------------------------------------------
c		Loss Data
c	     OprLoss(l2)     Transit loss (%) 
c	    		             Transit loss is a true loss no routing
c	     ioprloss        int(OprLoss) carrier loss switch
c	    		             + transit loss, maybe carrier loss
c	    		             - 0 transit loss, maybe carrier loss
c	     TranLoss    =   Transit loss (fraction)
      
c	     OprLossC(l2,i)  Conveyance loss from a carrier (%)
c	    		             Conveyance loss gets routed to system
c	     OprLost      	 conveyance loss (cfs)
c                     
c	     OprEff1       	 source Carrier Efficiency 
c                                 (1.0 - OprLoss(l2)/100)
c	     OprEffT       	 Total Carrier Efficiency 
c            
c	     effmaxT         Transit loss for a ditch 
c     
c	     ncarry          indicator at least 1 carrier
c	     ncnum           # of carriers
c
c
c ---------------------------------------------------------
c		Reporting (Outmon) Data
c       Note qdiv() is in general set in SetQdiv.  The qdiv()
c		      values set in this routine (18, 28 & 35) are related
c         to a carrier or are unique to this operating rule.
c			
c      qdiv(18,        Carrier through a structure
c      qdiv(20,        From Carrier by Storage or Exchange 
c      qdiv(26         From River by Exchange
c      qdiv(28,        Released from a reuse plan or Admin Plan
c	     qdiv(29,        Exchange from a plan
c      qdiv(30         From River from a Res or Reuse Plan 
c                         to a T&C or Aug Plan. Note non consumptive
c 	   qdiv(35        	Water with a Reuse or Admin plan source 
c			                 tracked at the destination.
c
c      qres(18		     To reservoir by exchange
c	     qres(4          To reservoir from carrier by Stor or Exch
c


c _________________________________________________________
c	Dimensions
      include 'common.inc'
      character cwhy*36, cdestyp*12, rec12*12,
     1  ccarry*3,    cpuse*3, cresid1*12,
     1  cplntyp*12,  cTandC*3, csrctyp*12,
     1  cDest*12,    cSour*12, cDest1*12, cstaid1*12

c
c _____________________________________________________________
c
c               Step 1a - Initilize general variables
c
c ---------------------------------------------------------
c               Control debut printout
c		iout=0 none, iout=1 detailed, iout=2 summary
c   outX=1 details on progress through this subroutine
      iout=0
      ioutiw=0
      ioutq=0
      ioutX=0
      
      isub=49
      
      cDest1='NA'
      
      if(ichk.eq.149) iout=2
      if(corid(l2).eq. ccall) ioutiw=iw
      
      if(iout.ge.1 .and. iw.eq.ioutiw) then  
cr        ioutX=1
      endif    
      
cx      if(iout.ge.1 .and. ncallx.eq.0) then
cx        write(nlog,102) corid(l2)
cx 102    format(/, 72('_'),/ '  DivRplP2; ID = ', a12)
cx      endif             
c
c ---------------------------------------------------------
      iouta=0
      ioff=0
      nd=0
      nr=0
      idcd=0
      idcdX=0
      imcd=0
      nsP=0
      nsR=0
      npd=0
      npd2=0
      nRiver=0
      if(iopdes(3,l2).ne.0) nPd2=iopdes(3,l2)
      
      icx=49

      small=0.001
      smalln=-1.0*small
      ishort = 0     

      
      cstaid1='NA          ' 
      
      if(iout.eq.1) then
        write(nlog,301) iyr, mon, iwx, iw, iout, ichk99
      endif
c
c ---------------------------------------------------------
c		b. Daily Capability      
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif
c
c ---------------------------------------------------------
c		c. Detailed output
      rcu=0.0
c
c rrb 2007/10/26; For ishort calculations and correct reoperation
c                 it is critical divact be < divalo when making 
c                 a quick exit
c     divact = 0.0 
      divact = -1.0/fac
      divalo = 0.0
      relact=0.0
            
      divactx=0.0
      divacty=0.0
c
c rrb 2007/10/26; Revise so call by Replace works (e.g. do not reset)
      ndx=-1
      
      pavail=-1.0/fac
      divcarry=-1.0/fac
      alocfs=-1.0/fac
      alocfs1=-1.0/fac
      divreq0=-1.0/fac
      
      avail1=-1./fac
      avail2=-1./fac
      availX=-1./fac
      qdiv29=-1./fac
      qdiv30=-1./fac
      qdiv35=-1./fac
      
      iwhy=-1
      cwhy='NA'
      cDest='-1'
      cSour='-1'
      cTandC='No'
      cpuse='No'
       
c
c _________________________________________________________
c               d. Destination Type
c     write(nlog,*) '  Divrpl; ncallx = ', ncallx
      nf=iopdes(1,l2)
      ndtype = iopdesr(l2)
      if(ndtype.eq.2) cdestyp='Reservoir'
      if(ndtype.eq.3) cdestyp='Diversion'
      if(ndtype.eq.7) cdestyp='Plan     '
      
c
c ---------------------------------------------------------
c		e. Source
      csrctyp='NA '
      ns =iopsou(1,l2)      
      
      if(ns.gt.0) then
        nsP=0
        nsR=ns
        csrctyp='Reservoir'
        cSour=cresid(nsR)
      else 
        nsP=-ns
        nsR=0
        csrctyp='Plan'
        Csour=pid(nsP)
      endif  
c
c ---------------------------------------------------------
c		f. Carrier 
      ncarry=0     
      ccarry='No'
      if(intern(l2,1).gt.0) then
        ccarry='Yes'
        ncarry=intern(l2,1)
      endif  
      
      if(ioutX.eq.1) write(nlog,*) ' DivrplP2_1; corid(l2) ',
     1    corid(l2)

c
c ---------------------------------------------------------
c rrb 2007/12/04; k. Set Transit and Carrier Loss Data      
c		Oprloss is transit loss (no returns
c		ioprLoss > 0 transit loss
c			 = 0 no transit loss
c               OprEffT  = Total Carrier Efficiency 
      call SetLoss(nlog, icx, l2, fac, 
     1 maxopr,   intern,  Oprloss, OprLossC,
     1 ioprloss, nCarry,  nRiver,  ncnum, 
     1 OprLost,  OprEff1, OprEffT, TranLoss, internT,internL,corid(l2))
      
c
c ---------------------------------------------------------
c		l. Check Avail Array
      call chekava(25, maxsta, numsta, avail)
      if(iouta.gt.0) write(nlog,*) ' DivrplP2; OK going in for ', 
     1 ' ID = ', corid(l2)
c
c _________________________________________________________
c
c		Step 2: Exit if not on this month

      if(imonsw(l2,mon).eq.0) then
        iwhy=1
        cwhy='Monthly Switch is off'
        goto 300
      endif  
c
c ---------------------------------------------------------
c		For a daily model set demand for beginning of season
      if(iday.eq.1 .and. imonsw(l2,mon).gt.0) then
        if (idy.lt.imonsw(l2,mon)) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 300
        endif  
      endif  
c
c ---------------------------------------------------------
c		For a daily model set demand for end of season
      if(iday.eq.1 .and. imonsw(l2,mon).lt.0) then
        if (idy.gt.iabs(imonsw(l2,mon))) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 300
        endif  
      endif  
c
c _________________________________________________________
c
c               Step 3 Set Source data
c
c _________________________________________________________
c               a. Source reservoir data
c
c rrb 2007/10/23; Allow source to be a reservoir      
      if(nsR.gt.0) then
        if(iressw(nsr).eq.0) ioff=2
        iscd=irssta(nsr)
        ndnS=ndnnod(iscd)
        isown=nowner(nsr)+iopsou(2,l2)-1
        
        alocfs=curown(isown)/fac
        alocfs1=alocfs
c
c ---------------------------------------------------------
c		Exit of zero
        if(alocfs.lt.small) then
          iwhy=2
          cwhy='Source Reservoir (curown) = zero'          
          goto 300
        endif
c
c ---------------------------------------------------------
c		Limit to maximum river discharge (flowmax)
        ALOCFS=AMIN1(FLOMAX(nsr)-RIVER(iscd),ALOCFS)
        alocfs = amax1(0.0,alocfs)
        alocfs2=alocfs
c
c ---------------------------------------------------------
c		Exit of zero
        if(alocfs.lt.small) then
          iwhy=3
          cwhy='Maximum river discharge  = zero'          
          goto 300
        endif
      endif
c
c _________________________________________________________
c               b. Source is a Plan
      if(nsP.gt.0) then
        IF(ifix(pon(nsp)).EQ.0) ioff=1
        iscd=Ipsta(nsp)
        ndnS=NDNNOD(iscd)      
c
c rrb 2008/09/30; If source is a recharge plan, calculate seepage
c		  and returns. Note:
c		  iall = 0 do all reservoirs, =n do reservoir n only
        if(iplntyp(nsP).eq.8) then    
c
c rrb 2009/03/10; Since many reservoirs may be tied to one plan,
c		    no way to know which reservoir needs to have seepage 
c		    so calculate for all (set iall=0)
c		    Note iplnr(np) originally calculated in GetPlnR 
c		    is not unique        
cx        iall=iplnr(nsP)
          iall=0             
          CALL SEPSEC(SeepT,iall,'DivRplP2    ')           
        endif
        
         
        ALOCFS=psuply(nsP)
        alocfs=amax1(0.0, alocfs)
        alocfs1=alocfs
        
        if(alocfs.le.small) then
          iwhy=4
          cwhy='Plan Supply = zero'
          goto 300
        endif  
      endif      
c
c _________________________________________________________
c
c		c. Exit if source is  off      
      if(ioff.gt.0) then
        iwhy=5
        cwhy='Plan or Div or Res is off'        
        write(nlog,*) ' DivrplP2; ioff', ioff
        goto 300
      endif
      
c _________________________________________________________
c
c		Step 4; Set Destination
c		4a Destination is an ISF      
      if(nf.gt.0) then
        iisf=1
        idcd=IFRSTA(NF)
        ndnD=NDNNOD(idcd)        

        idcdD=idcd
        idcdX=idcd
        
        ndX=nf
        DIVALO=FLOWRQ(NF)
        cdest=cifrid(nf)
      endif  
c _________________________________________________________
c
c		4b; Destination is a plan ID
c		stored as a negative value
      if(nf.lt.0) then
        npD=-nf
C
        idcd=ipsta(npD)
        ndnD=NDNNOD(idcd)        
        idcdP=idcd
        idcdX=idcd
        
        ndX=npD
        pdem1A=pdem(npD)
        DIVALO=pdem(npD)
        cdest=pid(npD)
      
        if(ioutX.eq.1) write(nlog,*) ' DivrplP2 In; corid(l2) ',
     1    corid(l2)
           
      endif  
c _________________________________________________________
c
c		4c; Secondary Destination is a plan ID
      if(nPd2.gt.0) then
C
        idcd=ipsta(npD2)
        ndnD=NDNNOD(idcd)        
        
        idcdX=idcd
        
        ndX=npD2
        pdem1B=pdem(npd2)
        DIVALO=amin1(divalo, pdem(nPd2))
        cdest=pid(npd2)
      endif
        
c _________________________________________________________
c
c		4d; Exit if Destination demand is 0
C
      if(nf.gt.0) then
        IF(FLOWRQ(NF).LE.small) then
          iwhy=6
          cwhy='Dest. ISF demand is zero'
          Goto 300
        endif
      endif  
      
      if(npd.gt.0) then
        IF(pdem(npD).LE.small) then
          iwhy=7
          cwhy='Dest. Total Plan Demand is zero'
          Goto 300
        endif  
      endif  
      
      if(npd2.gt.0) then        
        IF(pdem(npD2).LE.small) then
          iwhy=8
          cwhy='Dest. Second Plan Demand is zero'
          Goto 300
        endif  
      endif  
c
c _____________________________________________________________
c
c               Step 5; Destination is through a carrier
c		         Adjust diversion location
      if(intern(l2,1).gt.0) then
        IDCD=IDVSTA(ncarry)
        ndnD=NDNNOD(IDCD)
        
        idcdC=idcd        
c       idcdX=idcd
      endif
c
c _____________________________________________________________
c
c               Step 7; Exit if no demand
      if(ioutX.eq.1) write(nlog,*) ' DivrplP2_2; corid(l2) ',
     1    corid(l2)
      if(divalo.le.small) then
        iwhy=9
        cwhy='Demand or Capacity = 0'
        goto 300
      endif  
c
c
c _____________________________________________________________
c
c rrb 2007/12/04; 
c               Step 8; Process carrier limitations
c		ncarry is indicator at least 1 carrier
c		ncnum is the number of carriers
c		OprEff1 is the lost (oprlost(lw)
c		Divalo gets reduced by carrier capacity
c		DivCarry is the limitating carrier capacity
c		noprS is the structure id of the structure
c		 	that supplied water to the accounting
c		        plan that already has a capacity 
c			adjustment
      if(ncarry.gt.0) then
cx        if(lopr.gt.0) then        
cx          loprR=iopsou(1,lopr)
cx          noprS=idivco(1,loprR)        
cx        endif  
      
        call SetCarL(nlog, icx, l2, fac, 
     1    maxopr,  maxdiv, intern, OprLossC,
     1    ncarry,  ncnum,  noprS,  internT,
     1    OprEff1, DivCap, DivMon, DivCarry, Divalo)
        
        if(divcarry.lt.small) then
          iwhy=10
          cwhy='Carrier capacity (Divcarry) = 0 '
          goto 300
        endif    
      endif   
      if(ioutX.eq.1) write(nlog,*) ' DivrplP2_3; corid(l2) ',
     1    ' ncarry ', corid(l2), ncarry      
c _____________________________________________________________
c
c               Step 9 - Set Diversion (divact) and release (relact)
c       
      if(ncarry.eq.0) then      
        divact=amin1(alocfs, divalo)
      else
        divact=amin1(alocfs, divalo, divcarry)      
      endif
      divact=amax1(0.0, divact)
c
c _____________________________________________________________
c
c               Step 10 - Find minimum exchange potential
c                        in the river from the diversion
c                        node (idcd) to the Exchange point
c                        iExPoint(l2)
c
      IMCD=Idcd
      ISS=Idcd
      if(ioutq.eq.1) then
         write(nlog,*) ' DivRpl; Exchange Point = ', iss, iExPoint(l2)
      endif
c
      DO nx=1,ndnD
        if (iss.eq.iExPoint(l2)) goto 110
        IF(AVAIL(IMCD).GT.AVAIL(ISS)) IMCD=ISS
        ISS=IDNCOD(ISS)
      end do
c
  110 pavail=amax1(0.0,avail(imcd))
c _________________________________________________________
c
c 		Step 11a Calculate diversion 
      DIVACT=amin1(pavail,divalo,alocfs)
      divact=amax1(0.0,divact)
      divactx=divact
c
cr      
      relact=-divact

      if(ioutX.eq.1) then
        write(nlog,342) pavail*fac, divalo*fac, alocfs*fac, divact*fac
      endif
    
      if(divact.le.small) then
        iwhy=11
        cwhy='Exchange potential = 0'
        goto 300
      endif
c
      iwhy=0
c
c _________________________________________________________
c
c rrb 2007/12/04; Add Loss
c		Step 12; Calculate Diversion less loss (DivactL)
c			 Note:
c			 OprEffT = Total Carrier Efficiency 
      DivactL=divact*OprEffT
c
c _____________________________________________________________
c
c               Step 13; Add in Plan or Reservoir release (relact)
c
      if(ioutX.eq.1) write(nlog,*) ' DivrplP2_4; corid(l2), iscd ',
     1    ', relact ',corid(l2), iscd, relact*fac   
     
      availr=avail(iscd)
      CALL TAKOUT(maxsta,AVAIL,RIVER,AVINP,QTRIBU,IDNCOD,
     1          relact,ndnS,iscd)     
      AVAIL(iscd)=AVAILR
c
c _____________________________________________________________
c
c               Step 14; Remove destination from Avail from the
c               diversion downstream to shepherd water from other 
c               upstream uses
c
      if(ioutX.eq.1) write(nlog,*) ' DivrplP2_5; corid(l2)',
     1 ' ndnS, idcd divactL',
     1    corid(l2), ndnS, idcd, divactL*fac   
c
c     Remove from avail fron the diversion downstream (ndnD)          
      call takou2(isub, maxsta, avail ,idncod, divactL, ndnD, idcd)  
c
c rrb 2010/11/01; Correction add teh diversoin back to avail 
c                 from the exchange point downstream
      Temp = -1.*divactL
      idcE=iExPoint(l2)
      ndnE=ndnnod(idcE)
      call takou2(isub, maxsta, avail ,idncod, Temp, ndnE, idcE)  
            
      if(ioutX.eq.1) then
        write(nlog,*) ' DivrplP2_6;   corid(l2) cstaid(idcE)',
     1 '    idcE    ndnE    idcE    Temp'
        write(nlog,'(12x, a12,1x,a12,3i8,20f8.0)') 
     1    corid(l2), cstaid(idcE), idcE, ndnE, idcE, Temp*fac  
      endif       
c
c _____________________________________________________________
c
c               Step 15; Update Destination 
c
c		a. Destination is an instream flow
      if(ioutX.eq.1) write(nlog,*) ' DivrplP2_6; corid(l2) ',
     1    corid(l2),nf, npD, idcd, npd2 
      
      if(nf.gt.0) then
        FLOWRQ(NF  )=FLOWRQ(NF  )-DIVACT
      else  
c
c ---------------------------------------------------------
c		b. Destination is a plan      
        pdem(npD)=pdem(npD) - divact
        pdem2A=pdem(npD)
        if(iplntyp(npD).eq.3 .or. iplntyp(npD).eq.5) then
          psto2(npD)=amax1(psto2(npD) + divact*fac,0.0)           
        endif
c        
c rrb 2008/01/15; qdiv(35 Water with a Reuse or Admin plan source 
c			  at the destination. 
c
c rrb 2010/09/15; Accounted for under source is a plan    
cx        qdiv(35,idcd) = qdiv(35,idcd) + divact 
c
c rrb 2010/09/15; Set qdiv(30) where:
c           qdiv(30 is from River from a Res or Reuse Plan 
c           to a T&C or Aug Plan. Note non consumptive
c           plntyp=1 for aT&C and plntyp=2 for Well Augmentation
cr        if(iplntyp(npD).eq.1 .or. iplntyp(npD).eq.2) then
cr          qdiv(30,idcd)=qdiv(30,idcd) + divact
cr        endif
      
        if(npD2.gt.0) then
          pdem(npD2)=pdem(npD2) - divact
          pdem2B=pdem(npD2)
          if(iplntyp(npD2).eq.3 .or. iplntyp(npD2).eq.5) then
            psto2(npD2)=amax1(psto2(npD2) + divact*fac,0.0)           
          endif
        endif
      endif  
      
      if(ioutX.eq.1) write(nlog,*) ' DivrplP2_7; corid(l2) ',
     1    corid(l2)      
c
c _____________________________________________________________
c
c               Step 16; Update Source 
c   
c ---------------------------------------------------------
c		a. Source is a Plan
c  
      if(nsp.gt.0) then
        psuply(nsp)=amax1(0.0, psuply(nsp) +relact)
        if(iplntyp(nsp).eq.3 .or. iplntyp(nsp).eq.5) then
          psto2(nsp)=amax1(psto2(nsp)+relact*fac,0.0)                
        endif  
c
c rrb 2008/01/14; Qdiv(28 has a reuse or Admin plan source
c		  SET WHEN USED

        qdiv(28,iscd) = qdiv(28,iscd) + divact 
c
c rrb 2008/02/05; 
        qdiv(35,idcd) = qdiv(35,idcd) + divact      
        qdiv35=qdiv(35,idcd)         
      endif 
      
c      
c ---------------------------------------------------------
c		b. Source is a Reservoir
c		   Note relact and relaf are negative
      if(nsR.gt.0) then
        divaf=divact*fac
        relaf=relact*fac
        cursto(nsR)=cursto(nsR)+relaf
        curown(isown)=curown(isown)+relaf

        if(ncarry.eq.0) then
          qres(12,nsR)=qres(12,nsR)+divaf
          accr(12,isown)=accr(12,isown)+divaf  
        else        
          qres(11,nsR)=qres(11,nsR)+divaf
          accr(11,isown)=accr(11,isown)+divaf
          qdiv(18,idcd)=qdiv(18,idcd)+divaf
        endif  
c        
c               Check reservoir roundoff when exiting routine
        in=1
        isub1=19
        call chekres(nlog, maxres, in, isub1, iyr, mon, nsR,nowner,
     1               curown,cursto,cresid)
      endif         
c
c _________________________________________________________
c               Step 17; Update data to be passed out
c                        of this routine. Note
c                        divact .NE. relact if release
c                        for depletion only
c
      divactx=-relact
      divacty=divact  
c
c _________________________________________________________
c               Step 18; Update shortage (ishort) and
 300  continue
c
c _________________________________________________________
c               Step 19; Update operational right (divo)
c		to amount used from supply (relact) 
c               not amount diverted (divact)
c		Note this is required for consistency and
      divo(l2)=divo(l2)-relact
      
c
c _________________________________________________________
c rrb 2007/12/04
c               Step 20; Update Qdiv for Source and Destination

      EffmaxT1=(100.0-OprLossC(l2,1))/100.0
      
      idcd2X=idcdd
      idcd2C=idcdC
      nr2=0
      if(iout.eq.1) then
        write(nlog,*) '  DivRplP2; Call SetQdiv'
        call flush(nlog)
      endif  
            
      call SetQdiv(nlog, nCarry, nRiver,
     1  nd2, nr2, iscd, idcdX, idcdC,
     1  divact, TranLoss, EffmaxT1, OprEffT, fac, 
     1  rloss, maxsta, maxdiv, maxqdiv, qdiv, icx,
     1  internL, corid(l2))
     
      if(iout.eq.1) then
        write(nlog,*) '  DivRplP2; Call SetCarry'
        call flush(nlog)
      endif  
c
c _________________________________________________________
c rrb 2007/12/04
c               Step 21; Update Qdiv(18, ), Qdiv(32 ,) and Qdiv(20, ) 
c		   for the carrier(s)
c		   Also calculate return flows for carrier losses
c		   using the structure properties of the carrier	
c     write(nlog,*) ' DirectEx; nd, iuse', nd, iuse  
      if(ncarry.gt.0) then         
        idcd2X=idcdd
        idcd2C=idcdc
        call setQdivC(
     1    nlog, ncarry, ncnum, nd, nd2, l2, iscd, idcd2X, idcd2C,
     1    nriver, divact, TranLoss, EffmaxT1,
     1    fac, maxsta, maxdiv, maxqdiv, maxopr, 
     1    intern, idvsta, qdiv, divmon, 
c rrb 2009/06/09; Correction          
cx   1    maxrtnw, maxdivw, OprEff1, ipuse,  
     1    maxrtnPP, maxplan, OprEff1, ipuse,  
     1    pctlosPP, rlossP, oprLossc,internT,
     1    icx, corid(l2))
      endif

c
c _____________________________________________________________
c
c               Step 21 - Final printout befor exit
c
c      if(iout.ge.1) then
c      if(iout.gt.0 .and. ioutiw.eq.iw) then
       iprint=1
       
       if(iout.gt.0 .and. ioutiw.eq.iw .and. iprint.eq.1) then
c
c                a. FIND THE MIN AVAIL FLOW DOWNSTREAM (avail(imcd))
         if(idcd.gt.0) then
           iss=IDcd
           ndnDS=ndnD
           CALL DNMFSO(maxsta,avail,idncod,iSS,ndnDS,imcd)
           availX=avail(imcd)
           qdiv29=qdiv(29,idcd)
         else
         endif  
c
c ---------------------------------------------------------
c		Detailed Header            
       if(iout.ge.1 .and. iw.eq.ioutiw) then
         ncallX=ncallX+1
         if(ncallX.eq.1) then
           write(nlog,270) corid(l2), csrctyp, cdestyp, ccarry,
     1       cTandC, cpuse, cDivTyp(l2)
         endif  
       endif  
       
       if(idcd.gt.0) qdiv30=qdiv(30,idcd)        
              
         write(nlog,*) ' DivrplP2 Out; qdiv30, qdiv35 ', 
     1     qdiv30*fac,qdiv35*fac  
         if(iday.eq.0) then
           write(nlog,280) ' DivrplP2    ',
     1       iyrmo(mon),   xmonam(mon), idy,cSour,cDest,
     1       iwx, iw, l2, ndX, 
     1       alocfs1*fac, alocfs*fac,  pavail*fac,   
     1       divcarry*fac, abs(relact*fac), avail1*fac, avail2*fac,
     1       availX*fac,   divact*fac,   iwhy, cwhy
c    1       imcd, ishort, avail1*fac, qdiv29*fac,
c    1       float(iOprLim(l2)), float(iopsou(5,l2))
         else
           write(nlog,282) ' DivrplP2     ',
     1       iyrmo(mon),   xmonam(mon), idy, cSour,cDest,
     1       iwx, iw, l2, ndX, 
     1       alocfs1*fac, alocfs*fac,  pavail*fac,   
     1       divcarry*fac, abs(relact*fac), avail1*fac, avail2*fac,
     1       availX*fac,   divact*fac,  iwhy, cwhy
c    1       imcd, avail1*fac, qdiv29*fac,
c    1       float(iOprLim(l2)), float(iopsou(5,l2))
     
         endif
         if(divact*fac.gt.small .and. avail(imcd)*fac.lt.smalln)
     1   write(nlog,*) '*****' 
       endif
       
       if(idcd.gt.0) qdiv30=qdiv(30,idcd)               
cr     write(nlog,*) ' DivrplP2 Out; corid(l2),', corid(l2), qdiv30*fac      
c
c _____________________________________________________________
c               
c               Step 23 - Check Entire Avail array out
      call chekava(25, maxsta, numsta, avail)
      if(iouta.gt.0) write(nlog,*) ' DivrplP2; OK going out for ', 
     1 ' ID = ', corid(l2)

c
c _____________________________________________________________
c
c                Step 24 - Return
      return
c _________________________________________________________
c               Formats
 270    format(/, 
     1  ' DivrplP2 (Type 49); Operation Right ID = ', a12,
     1  ' Source = ', a12,         
     1  ' Destination Type = ', a12,
     1  ' Carrier (Y/N) = ',a3, ' T&C Plan (Y/N) = ',a3,
     1  ' Reuse Plan (Y/N) = ', a3,
     1  ' Diversion Type = ', a12,/   
     1  ' DivrplP2     Iyr  Imo  Day Source ID    Dest ID     ',
     1  ' Iter  iwx   l2  ndX',
     1  ' Alocfs1  Alocfs  Pavail Divcary  RelAct  Avail1  Avail2',
     1  '  AvailX  DIVACT iwhy cwhy                                ',/
c    1  ' imcd short avail  Qdiv29',/
     1  ' ___________ ____ ____ ____ ____________ ____________',
     1  ' ____ ____ ____ ____', 
     1  ' _______ _______ _______ _______ _______ _______ _______',
     1  ' _______  ______ ____ ____________________________________')
c    1  ' ____ ____ _______ _______')
 280   format(a12, i5, 1x,a4, i5, 2(1x,a12), 4i5,
     1 9f8.1, i5, 1x,a36, 2i5, 20f8.1)
     
 282   format(a12, i5, 1x,a4, i5, 2(1x,a12), 4i5,
     1 9f8.3, i5, 1x,a36, 2i5, 20f8.3)

 301   format(/,60('_'),/,
     1   '  DivrplP2;  iyr  mon iteration ', 3i5,/
     1   '  DivrplP2;   iw iout    ichk99 ', 3i5)

 342   format(
     1     '  DivrplP2; Diversion Limit;  ',
     1     '  pavail  divalo  alocfs  divact',/
     1     '                              ', 20f8.2)
c
c _____________________________________________________________
c
c		Error Processing
 9999 continue
         write(nlog,270) corid(l2), csrctyp, cdestyp, ccarry, 
     1    cTandC, cpuse, cDivTyp(l2)    
                          
         if(iday.eq.0) then
           write(nlog,280) ' DivrplP2    ', 
     1       iyrmo(mon),   xmonam(mon), idy, cSour,cDest,       
     1       iwx, iw, l2, ndX, 
     1       alocfs1*fac, alocfs*fac, divalo*fac,  pavail*fac,   
     1       divcarry*fac, abs(relact*fac), avail1*fac, avail2*fac,
     1       availX*fac,   divact*fac,  iwhy, cwhy
c    1       imcd, ishort, avail1*fac, qdiv29*fac,
c    1       float(iOprLim(l2)), float(iopsou(5,l2))
     
         else
           write(nlog,282) 'DivrplP2',
     1       iyrmo(mon),   xmonam(mon), idy, cSour,cDest,       
     1       iwx, iw, l2, ndX, 
     1       alocfs1*fac, alocfs*fac, divalo*fac, pavail*fac,   
     1       divcarry*fac, abs(relact*fac), avail1*fac, avail2*fac,
     1       availX*fac,   divact*fac,  iwhy, cwhy
c    1       imcd, avail1*fac, qdiv29*fac,
c    1       float(iOprLim(l2)), float(iopsou(5,l2))
     
         endif
 
      write(6,1050) 
      write(nlog,1051) 
      
 1050 format('    Stopped in DivrplP2',/,
     1       '    See the *.log file')
 1051 format('    Stopped in DivrplP2')
      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)
      stop 
      END
