C     Last change:  RRB  28 May 2002    4:31 pm
c     Type 28
c
c ---------------------------------------------------------
c rrb 2007/10/26; Add ability to be called by Replace
c     subroutine divrplP(iw,l2,ishort,divactx,divacty,ncallx)
c
      subroutine divRplP(iw,l2,ishort,irep,tranlim,dcrdivx,divdx,
     1     divactx,divacty,ncallX)  
c     
c
c
c _________________________________________________________
c	Program Description
c
c     DivrplP; Type 28
c              It allows a plan release to a diversion, or
c              reservoir or plan or instream flow reach by exchange 
c	       Note can exchange the diversion amount or depletion
c              amount
c
c              Called by Execut
c _________________________________________________________
c
c       Update History
c
c rrb 2015/01/20; Revised to handle releases when the water 
c     was diverted using a type 26 Changed Water right
c     This is known when source is a plan and the
c     plan type is 13  
c
c rrb 2011/08/05; Revised to allow an instream flow reach
c     Note the same logic works for both an ISF point and a reach
c
c rrb 2008/06/10; Allow return to river then diversion
c		  from a carrier again (internT = 1 Carrier, 2=Return)
c		  Also redefine nCarry (see documentation)       
c     
c rrb 2008/06/10; Allow return to river then diversion
c		  from a carrier again (internT = 1 Carrier, 2=Return)        
c
c rrb 2007/10/26; Add ability to be called by Replace
c
c rrb 2007/10/12; Revised to limit a release to an annual limit
c		  Note if iOprlim = 1 & 6 are not currently operational
c		  Note if iOprlim = 2, 4 7 or 9 release limits are adjusted
c		    for the operatng rule in iopsou(5,l2).
c		  Note if iOprlim = 3, 4, 8 or 9 diversions are limited to the 
c		    amount simulated by another operating rule iopsou(6,l2).
c     Note if IOprlim = 5 or 7-9 the source is a Changed Water 
c       Right Plan (type 13) that has special reporting capability
c                 (iopsou(5,l2).
c
c rrb 2007/08/22; Revised to allow the return flow calculation for 
c                 the T&C requirement to vary as follows:
c                 if(iopsou(4,l2) = 1 call RtnsecP for a STANDARD		  
c                 if(iopsou(4,l2) = 2 call RtnsecC for a FIXED
c		  
c rrb 2007/07/09; Revised to allow the T&C obligation to be
c		  assigned to the destination herein
c
c rrb 04/12/29; Copy divrpl. Adjust for plans by:
c	  	source is a plan not a reservoir
c	  	remove refrences to replacement reseroirs (irep>0)
c	  	remove refrences to a water right limitation
c	  	add destination is a diversion
c
c _____________________________________________________________
c
c       Documentation
c
c ---------------------------------------------------------
c	VARIABLES USED WHEN CALLED BY REPLACE (IREP>0)
c	    irep          = Called by Replace (0=no, >0=yes)
c           dcrdivx       = decree of destination diversion right
c           divdx         = diversion to date of diversion right
c
c ---------------------------------------------------------
c	VARAIABLES USED WHEN CALLED BY EXECUTE OR REPLACE 
c
c       iw              water right order
c       l2              order of operating rule
c       ishort          shortage indicator 0=no, 1=yes
c       irep            replace indicator 0=no, 1=yes
c
c       dcrdivx 	      dcrdiv(n) water right
c       divdx 		      divd(n) remaining water right amount
c
c       divact          actual diversion
c       divactx         actual plan release from divrplP
c       divacty         actual diversion from divrplP Note may
c                       not equal divactx if releasing for
c                       depletion
c
c       Other Key variables
c       icx   = 6      subroutine call #
c       ieff2 = 0      always use average efficiency
c             = 1      use max efficiency if ieffmax = 1
c       internT     =  Intervening structure type
c			                 1 = Carrier
c			                 2 = River
c
c       iopsou(1,l2)   ns 
c		                   if > 0 source reservoir
c                      if < 0 source plan
c
c       iopsou(2,l2)   iown   N/A
c       iopsou(3,l2)   T&C Plan id (for return flow obligation)
c			                  associated with a release
c
c       iopsou(5,l2) = A Plan ID containing monthly Diversion limits
c                      Read in Oprinp when Oprlimit = 2, 4 or 7
c
c       iopsou(6,l2) = A Operating rule ID whos diversion will
c                      limit diversions by this operating rule
c                      Read in Oprinp when Oprlimit = 3, 4 or 8
c
c       iopsou(7,l2) = Type 26 Operating rule that provided water to a
c                      Changed Water Right Plan (type 13)
c                      Read in Oprinp when Oprlimit = 5 or 9
c
c       ireltyp         Not operational
c                       For other operating rules this is the
c                       Reservoir release type and efficiency
c                       0 = release to meet demand
c                       +n = release only if a CIR (IWR) exists
c                       and limit release to not exceed IWR/n,  
c                       Note nmax = min(nmax, effmax) to save 
c                       iterating
c
c       iDep            Diversion type
c                       0 release to meet diversion demand
c                       1 release to meet depletion
c
c       iExPoint(l2)    exchange node
c
c	      ipUse           Reuse plan for return flows
c                       Note ipUse=ireuse(l2)
c
c       iopdes(1,l2)    nd   destination diversion or reservoir
c       iopdes(2,l2)    iuse destination user
c
c       iopdesr(l2)     Destination type   (reservoir, diversion or plan)
c
c       ipsta(nsP)     	Source Plan river station
c
c	      idcd            Actual Diversion location. May be one 
c                        a Diversion, Reservoir or Plan or Carrier

c	      idcdD	        	Destination Diversion location on river
c	      idcdR           Destination Reseroivr location on river
c	      idcdC           Destination carrier location on river
c	      idcdP           Destination Plan on river
c	      idcdX           Destination Diversion, Reservoir,
c			                  Plan. Not a CARRIER
c
c
c       ndnnod=ndnd     number of nodes downstream of destination
c			                  or first carrier
c       ndnnod(iscd) 	  ndns   number of nodes downstream of supply
c
c       divcap(nd)      capacity
c       divmon(nd)      demand supplied this time step (grows
c                         with each diversion each iteration)
c       divreq(iuse)    demand remaining this time step
c                         (reduces with each diversion each
c                         iteration)
c
c       divalo          allowable demand after adjusting for
c                         demand, capacity, amount already
c                         diverted this time step, water right
c                         limit, etc.
c       divact          actual diversion
c
c      	nsP			        source is a Plan (iopsou(1,l2)<0
c      	nsR             source is a Reservoir (iopous(1,l2>0)
c      	pavail          minimum available flow 
c
c       small           a local limit used for convergence, etc.
c       iouta           0 no detailed printout for avail
c                       n yes detailed printout for avail
c	      iwhy            1 reason for no diversoin
c
c	      ncallx		      number of times called per time step
c       
c	      ndtype         	iopdesr(l2) Destination Type
c	                      2 = 'Reservoir'
c                       3 = 'Diversion'
c                       7 = 'Plan     '
c
c ---------------------------------------------------------
c		Depletion Vs Diversion Data
c       iDep            Diversion type
c                       0 release to meet diversion demand
c                       1 release to meet depletion
c       DepFac          factor to convert from diversion to Depletion
c
c
c
c ---------------------------------------------------------
c		Loss Data
c	      OprLoss(l2)     Transit loss (%) 
c		                  	Transit loss is a true loss no routing
c	      ioprloss        int(OprLoss) carrier loss switch
c		                  	+ transit loss, maybe carrier loss
c		                 	  - 0 transit loss, maybe carrier loss
c	      TranLoss        Transit loss (fraction)
c        
c	      OprLossC(l2,i)  Conveyance loss from a carrier (%)
c		     	              Conveyance loss gets routed to system
c	      OprLost       	conveyance loss (cfs)
c             
c	      OprEff1       	source Carrier Efficiency 
c                       (1.0 - OprLoss(l2)/100)
c	      OprEffT        	Total Carrier Efficiency 
c
c       psuply(np)      Running plan returns for this month. It 
c 	                    increases or decreases based on opr rules
c       psuplyT(np)     Total monthly plan demands this month 
c                       (may increase but will not decrease based on
c                       operating rules
c        
c	      effmaxT        	Transit loss for a ditch 
c
c
c       nCarry          0 No carrier
c		                    1 No return to River, Final Destination is
c			                    from a carrier
c	                      2 Return to River, Final Destination is
c                           from a carrier
c		                    3 Return to River, Final Destination is 
c			                      from the river
c
c	      nRiver	        Indicator a release to the River
c       
c	      ncnum          # of carriers
c      
c ---------------------------------------------------------
c		Reporting (Outmon) Data
c
c       qdiv(18        Carried, Exchange or Bypass (column 11)
c                        Carrier passing thru a structure.  Note
c                        qdiv(18 is not added to TotSup1 that is used
c                        to calculate River Divert
c       qdiv(20         From Carrier by Storage, Exchange or Plan
c	      qdiv(28         Carried, Exchange or Bypass (column 11)
c                       Stored via a reuse plan in DirectEx or 
c                       DirectBy
c		    qdiv(30         From River from a Res or Reuse Plan 
c                         to a T&C or Aug Plan. Note non consumptive
c                         Note shows as a diversion in *.xdd (From River 
c                         by Storage or Other) but is not a diversion in *.xwb
c                         Non consumptive          
c                         See type 49 Divrplp2
c       qdiv(31         From River by Plan by DivResP2 or DivrplP     
c	
c       qdiv(36         Water released to the system as return flow
c                         plan types 4 & 6 reuse from a diversion or
c                         tmtn diversion
c       qdiv(38         Carried water reported as Carried, Exchange
c                         or Bypassed but not used to calculate
c                         River Divert in Outmon.f
c __________________________________________________________
c
c       qres(18         From River by Exchange to Reservoir
c       qres(4          From carrier by Storage to reservoir

c _________________________________________________________
c	Dimensions
      include 'common.inc'
      character cwhy*48, cdestyp*12, rec12*12,
     1  ccarry*3,    cpuse*3, cresid1*12,
     1  cplntyp*12,  cTandC*3, csrctyp*12, cReplace*3,
     1  cDest*12,    cSour*12, cDest1*12, cRiver*12,
     1  corid1*12, cImcdR*12, cstaid1*12

c
c _____________________________________________________________
c
c               Step 1a - Initilize general variables
c
c ---------------------------------------------------------
c               Control debut printout
c		            iout=0 none, iout=1 detailed, iout=2 summary
c		            ioutiw = water right used for detailed output
c		            ioutE = detailed output for Exchange Reach
c		            ioutA = detailed output for ChekAva
c               ioutQ = 1 Print qdiv(38
c                       2 Print qdiv(38 plus qdiv array
c
c               ioutIR= instream flow reach
c               iout26= 1 details on reporting when the original source 
c                       was a type 26 operating rule and limiting the
c                       capacity of the original source
c               iout26= 2 summary of iout26=1
c
      iout=0
      ioutiw=0
      ioutE=0
      ioutA=0
      ioutIR=0
      ioutQ=0      
      iout26=0
c    
      cDest1='NA'
      cImcdR='NA'
      
      corid1=corid(l2)
      
      if(ichk.eq.128) iout=2
      
      if(irep.eq.0) then
        if(corid1.eq. ccall) ioutiw=iw
      else
        nd=iopdes(1,l2)        
        cDest1=cdivid(nd)
c       if(cdivid(nd).eq. ccall) ioutiw=iw
c       if(corid1.eq. ccall .and. cDest1.eq.'510585      ') ioutiw=iw
        if(corid1.eq. ccall) ioutiw=iw
      endif
c
c rrb 2015/09/15; Option to print every time this operating
c                 right is called or just when the one
c                 specified as ncallx is called  
      if(iout.eq.2 .and. ioutiw.eq.iw .and. ncallx.eq.0) then  
cx    if(iout.ge.1 .and. ncallx.eq.0) then            
        write(nlog,102) corid1
 102    format(/, 72('_'),/ '  DivRplP; ID = ', a12)
      endif     
c
c rrb 2015/09/25; Control detailed output to one operating rule
c                 ioutQ = 1 Print qdiv(38
c                         2 Print qdiv(38 plus qdiv array
c
      if(iout.eq.2 .and. ioutiw.eq.iw) then 
        ioutQ=1
c       ioutQ=2
      else
        ioutQ=0
      endif 
              
c
c ---------------------------------------------------------
      ioff=0
      nd=0
      nr=0
      idcd=0
      imcd=0
      
      ndP=-1
      nsP=-1
      ipuse=-1
      lopr5=-1
      lopr6=-1
      lopr26=-1
      
      nsR=-1
      iscd=-1
      idcd=-1
      idcdX=-1
      idcdD=0
      idcdR=0
      idcdP=0
      idcdC=0
      idcdI=0
      
      nRiver=0
      
      icx=28

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
c rrb 201401/16; Print Qdiv data
      if(ioutQ.eq.2) then
        write(nlog,*) ' '
        write(nlog,*) ' DivRplP  In; Qdiv report'
        write(nlog,'(4x, 39i8)') (j, j=1,39)
        do i=1, numsta
          write(nlog,'(i5, 39f8.0)') i, (qdiv(j,i)*fac, j=1,39)
        end do
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
      iuseX=-1
      
      pavail=-1.0/fac
      divcarry=-1.0/fac
      divreq1=-1.0/fac
      alocfs=-1.0/fac
      alocfs1=-1.0/fac
      alocfsR=-1.0/fac
            
      divalox=-1.0/fac
      divreq0=-1.0/fac
      divreq1=-1.0/fac  
      
      qdiv29=-1./fac      
      qdiv31=-1./fac    
      qdiv35=-1./fac
      qdiv36=-1./fac
      
      big=99999.      
      
      OprmaxM1=-1.0
      OprmaxM2=-1.0
      
      diveff1=1.0
      
      avail1=-1./fac

      iwhy=-1
      cwhy='N/A'
      cDest='-1'
      cSour='-1'
c _________________________________________________________
c               d. Destination
c     write(nlog,*) '  DivrplP; ncallx = ', ncallx
      cdestyp='NA '
      cDest='NA'
      
      nd  =iopdes(1,l2)
c
      ndtype = iopdesr(l2)
      if(ndtype.eq.1) then
        cdestyp='ISF      '
        cDest=cifrid(nd)
      endif
      
      if(ndtype.eq.2) then
        cdestyp='Reservoir'
        cDest=cresid(nd)
      endif
      
      if(ndtype.eq.3) then
        cdestyp='Diversion'
      cDest=cdivid(nd)      
      endif
      
      if(ndtype.eq.7) then
        cdestyp='Plan     '      
        cDest=pid(nd)
      endif            
c
c ---------------------------------------------------------
c		e. Source
      csrctyp='NA '
c     nsp =iopsou(1,l2)
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
      nCarry=0
      ccarry='No'
      if(intern(l2,1).gt.0) then
        ccarry='Yes'
        ncarry=1
      endif  
c
c ---------------------------------------------------------
c		g. ReUse Plan 
c rrb 2015/02/03X; Revise to initilize when the source is a 
c                  reservoir (nsR>0) in order to allow ireuse(k) 
c                  to sometimes be the source plans operating rule
c                  when the source is a type 11 (admin plan) 
c		               ipUse = Reuse plan    
      cpuse='No'
      cplntyp='NA '
c
c rrb 2015/02/03X; Set ipUse when the source is a reservoir
      if(nsR.gt.0) then     
        ipUse=ireuse(l2)
        
        if(ipUse.gt.0) then
          cpuse='Yes'
          if(iplntyp(ipUse).ne.9) cplntyp='Reuse_Plan'
          if(iplnTyp(ipuse).eq.9) cPlnTyp='OOP_Plan'
          if(iplnTyp(ipuse).eq.11)cPlnTyp='Acct_Plan'
        endif  
      endif
c
c rrb 2015/02/03X; Set ipuse when the source is a Reuse Plan
C smalers 2017-11-06 Split the statement to ensure valid index value
C     if(nsP.gt.0 .and. iplntyp(nsP).ne.11) then
      if(nsP.gt.0) then
        if (iplntyp(nsP).ne.11) then
          ipUse=ireuse(l2)
          if(ipUse.gt.0) then
            cpuse='Yes'
            cplntyp='Reuse_Plan'
          endif
        endif
      endif
c      
c ---------------------------------------------------------
c               h. T&C Plan
c		ipTC  = T&C plan
      cTandC='No'
      ipTC=iopsou(3,l2)
      if(ipTC.gt.0) cTandC='Yes'
c
c ---------------------------------------------------------
c		            i. Set associated operating rule
c		   for carrier & capacity adjustments  
c rrb 2015/02/03X; Done below based on the value of ioprlim   
cx    lopr5=iopsou(5,l2) 
c
c ---------------------------------------------------------
c		            j. Set CU limit switch      
      rec12=cDivTyp(l2)
      iDep=0
      if(rec12(1:9).eq.'Diversion') iDep=0
      if(rec12(1:9).eq.'Depletion') iDep=1
     
c      
cr rrb 2007/08/21; Set factor for Depletion Vs Diversion
      if(iDep.eq.0) then
        DepFac=1.0
      else
        DepFac=Effmax(nd)/100.        
      endif
c
c ---------------------------------------------------------
c rrb 2005/11/27; 
c		k. Add CuFac
      CuFac=OprEff(mon,l2)/100.          
c
c ---------------------------------------------------------
c		l. Called by Replace
      if(irep.eq.0) then
        cReplace='No'
      else
        cReplace='Yes'
      endif            
c      
c ---------------------------------------------------------
c		m. Variable Efficiency
      ieff2=1
      if(ndtype.eq.3) then
        if(ieff2.eq.0) then
          diveff1=diveff(mon,iuse)/100.
        else
          diveff1=effmax(nd)/100.
        endif
      endif  
c      
c ---------------------------------------------------------
c		n. CU limit
      
      if(iDep.eq.0) then
        CuLimit=1.0
      else
        CuLimit=OprEff(mon,l2)/100.
      endif
c
c ---------------------------------------------------------
c rrb 2007/12/04; 
c		o. Set Transit and Carrier Loss Data      
c		   Oprloss is transit loss (no returns
c		   ioprLoss > 0 transit loss
c			 = 0 no transit loss
c               OprEffT  = Total Carrier Efficiency 
      call SetLoss(nlog, icx, l2, fac, 
     1 maxopr,   intern,  Oprloss, OprLossC,
     1 ioprloss, nCarry,  nRiver,  ncnum, 
     1 OprLost,  OprEff1, OprEffT, TranLoss, internT,internL,corid1)
c
c rrb 2007/01/03; Add release to river capability
       cRiver='NA          '
       if(nRiver.gt.0) cRiver=cstaid(nRiver)     
      
c
c ---------------------------------------------------------
c		p. Check Avail Array
      call chekava(18, maxsta, numsta, avail)
      if(iouta.gt.0) write(nlog,*) ' DivrplP; OK going in for ', 
     1 ' ID = ', corid1
c
c ---------------------------------------------------------
c rrb 2014/11/14; n. Set location of the operating right that
c                    provided source of water to a type 11 plan
      lopr26=0
      lr26=0
      nd26=0
      iscd26=0
      ndns26=0
c
      if(iout26.eq.1) then
        write(nlog,*) ' '
        write(nlog,*) ' DivRplP_1; l2, ioprlim(l2), iopsou(5,l2)'   
        write(nlog,*) ' DivRplP_1;', l2, ioprlim(l2), iopsou(5,l2)         
      endif
c  
c
c rrb 2015/03/07; Revise to use Oprlimit 1-9
cx      if(nsP.gt.0 .and. iplntyp(nsP).eq.11) then        
cx      lopr26=ireuse(l2)
C smalers 2017-11-06 Split the statement to ensure valid index value
C     if(nsP.gt.0 .and. iplntyp(nsP).eq.13) then
      if(nsP.gt.0) then
        if(iplntyp(nsP).eq.13) then
          lopr26=iopsou(7,l2)
          lr26=iopsou(1,lopr26)              
          nd26=idivco(1,lr26)
          iscd26=IDVSTA(nd26)          
          ndns26=NDNNOD(iscd26)
c
c rrb 2015/01/24; Additional reporting approach. 
          nsp1=iopdes(1,lopr26)
          iscd1=ipsta(nsp1)   
          iok=0
          if(lopr26.eq.0 .or. lr26.eq.0  .or. nd26.eq.0 .or.
     1     iscd26.eq.0 .or. nsp1.eq.0 .or. iscd1.eq.0) then
            iout26=1
            iok=1     
          endif       
        endif
      endif    
c   
      if(iout26.eq.1) then
        write(nlog,*) ' '         
        write(nlog,*)
     1    ' DivRplP_2;   lopr26    lr26    nd26  iscd26',
     1                '   ndns26    nsp1   iscd1  ndns26'   
        write(nlog,'(a12,8i8)')
     1    ' DivRplP_2;', lopr26, lr26, nd26, iscd26, ndns26, iscd1, ndns26
c      
        if(iok.eq.1) then
          write(nlog,*) ' Problemm with source water right reporting'
          goto 9999   
        endif   
      endif
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
c
c ---------------------------------------------------------
c 		a. Source is a plan
      if(nsP.gt.0) then
        IF(ifix(pon(nsp)).EQ.0) ioff=1
        iscd=Ipsta(nsp)
        ndns=NDNNOD(iscd) 
c
c ---------------------------------------------------------
c rrb 2008/07/03; Add % ownership
c		  Note ALOCFS=amax1(psuplyT(nsp)*OprPct1 - divo(l2))
c		  insures the % is calculated on the total amount
c		  to the plan (psuplyT), less previously diverted
c		  (divo(l2).
cx      ALOCFS=psuply(nsp)
        OprPct1=oprPct(l2)/100.0         
        ALOCFS=amax1(0.0, psuplyT(nsp)*OprPct1 - divo(l2))
c
c rrb 2008/08/15; Correction to limit by active plan storage (psuply)        
        ALOCFS=amin1(ALOCFS, psuply(nsp))
        
        if(iout.eq.1) then
          write(nlog,*) ' DivRplP; nsp, psuplyT, OprPct1, divo, alocfs'
          write(nlog,*) '         ',
     1      nsp, psuplyT(nsp)*fac, OprPct1, divo(l2)*fac, alocfs*fac
        endif
        
        alocfs=amax1(0.0, alocfs)
        alocfs1=alocfs
        
        if(alocfs.le.small) then
          iwhy=2
          cwhy='Plan Supply = zero'
          goto 300
        endif  
      endif
c
c ---------------------------------------------------------
c               b. Source reservoir data
c
c rrb 2007/10/23; Allow source to be a reservoir      
      if(nsR.gt.0) then
        if(iressw(nsr).eq.0) ioff=2
        iscd=irssta(nsr)
        ndns=ndnnod(iscd)
        isown=nowner(nsr)+iopsou(2,l2)-1
        
        alocfs=curown(isown)/fac
        alocfs1=alocfs
c
c ---------------------------------------------------------
c		Exit of zero
        if(alocfs.lt.small) then
          iwhy=3
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
          iwhy=4
          cwhy='Maximum river discharge  = zero'          
          goto 300
        endif
      endif
c
c _________________________________________________________
c               
c               Step 4; Limit to monthly and annual limits
c rrb 2007/10/23;    
c rrb 2015/02/03X; Allow type 4 that is a type 2 + type 3  
c rrb 2015/03/07; Allow ioprlim to be 1-9
cx    if(iOprLim(l2).eq.2 .and. iopsou(5,l2).gt.0) then
      if(iOprLim(l2).eq.2 .or. iOprLim(l2).eq.4 .or.
     1   iOprLim(l2).eq.7 .or. iOprLim(l2).eq.9) then
        if(iopsou(5,l2).gt.0) then
          lopr5=iopsou(5,l2)
          oprmaxM1=amin1(oprmaxM(lopr5), oprmaxA(lopr5))
c         
c rrb 2015/03/07; Revise to limit to diversions in prior iterations  
c rrb 2015/03/23; Remove since this is taken care of in SetLimit           
cx        oprmaxM1=amax1(0.0, oprmaxM1-divo(l2)*fac)          
          ALOCFS=AMIN1(ALOCFS, oprmaxM1/fac)
          alocfs3=alocfs
c          
          if(iout.eq.1) then 
            write(nlog,*) ' DivResP2; ',
     1        'lopr5, oprmaxM(),oprmaxA() divo(l2)'
            write(nlog,*) ' DivResP2;',
     1         lopr5,oprmaxM(lopr5), oprmaxA(lopr5), divo(l2)*fac
          endif
c         
          if(alocfs.le.small) then
            iwhy=5
            cwhy='Monthly or Annual Limit (OprMaxM1) = zero'          
            goto 300
          endif 
        endif 
      endif  
c
c _________________________________________________________
c               
c rrb 2009/01/15;
c               Step 4b; Limit to the amount diverted by another 
c		              operating rule
c		
c rrb 2015/02/03X; Allow type 4 that is a type 2 + type 3 
c rrb 2015/03/07; Allow ioprlim to be 1-9
cx    if(ioprlim(l2).eq.3 .and. iopsou(5,l2).gt.0) then    
      if(ioprlim(l2).eq.3 .or. ioprlim(l2).eq.4 .or.
     1   ioprlim(l2).eq.8 .or. ioprlim(l2).eq.9) then
        if(iopsou(6,l2).gt.0) then      
          lopr6=iopsou(6,l2)
          oprmaxM1=amax1(0.0, divo(lopr6)*fac)        
c        
c rrb 2015/03/02X; Revise to limit to diversions in prior iterations 
c rrb 2015/03/23; Note not handled in Setlimit since there is no 
c                 annual limit and it assumes only 1 right will be tied
c                 to this operating rule               
          oprmaxM1=amax1(0.0, oprmaxM1-divo(l2)*fac)        
          
          if(iout.eq.1) then
            write(nlog,*) ' '
            write(nlog,*) 
     1        ' DivRplP; ioprlim(l2),lopr6,divo(lopr6),divo(l2),\',
     1                   'OprmaxM1'
            write(nlog,*)
     1        ' DivRplP; ',ioprlim(l2), lopr6, divo(lopr6)*fac, 
     1                     divo(l2)*fac, OprmaxM1
          endif
                  
          ALOCFS=AMIN1(ALOCFS, oprmaxM1/fac)
          alocfs3=alocfs
          
          if(iout.eq.1) 
     1      write(nlog,*) ' DivRplP;',lopr6, oprmaxM1, 
     1        divo(lopr6)*fac, alocfs2*fac, alocfs3*fac
         
          if(alocfs.lt.small) then
            iwhy=6
            cwhy='Operating Rule Limit (OprMaxM1 or alocfs3) = 0'          
            goto 300
          endif
        endif
      endif        
c
c _________________________________________________________
c               
c rrb 2014/11/24;
c                 Step 4c; Limit to the capacity in the original source
c		
      if(lopr26.gt.0) then
        divcap1 = amax1(0.0, divcap(nd26) - divmon(nd26))     
        alocfs4=alocfs
        ALOCFS=AMIN1(ALOCFS, divcap1)
        alocfs5=alocfs
        
        if(iout26.eq.1) then
          write(nlog,*) ' '
          write(nlog,*) 
     1    ' DivRplP_3;  lopr26  alocfs4 divcap1 alocfs5'
          write(nlog,*) 
     1    ' DivRplP_3;',lopr26, alocfs4*fac, divcap1*fac, alocfs5*fac 
        endif
        
        if(alocfs.lt.small) then
          iwhy=7
          cwhy='Original source capacity = zero'          
          goto 300
        endif
      endif        
      
c
c _________________________________________________________
c
c               Step 5a; Destination (Demand = Divalo)
      nd2 = nd
      ndx = nd
c
c ---------------------------------------------------------
c		a. Destination is a Diversion      
c     write(nlog,*) '  DivrplP; nd ', nd
c     if(nd.gt.0) then
      if(ndtype.eq.3) then
        ndD=nd
        ndR=0
        ndP=0
        ndI=0
        cDest=cdivid(nd)
        if(idivsw(nd).eq.0) ioff=2
      
        IUSE=NDUSER(ND)+IOPDES(2,l2)-1
        diveff1=diveff(mon,iuse)/100.          
        iuseX=iuse        
        
        IDCD=IDVSTA(ND)
        idcdD=idcd
        NDND=NDNNOD(IDCD)
        
        idcdX=idcdD
        ndndX=NDNNOD(idcd)
        
c
c rrb 2007/12/04; Add Loss        
c       divreq1=divreq(iuse)
        divreq0=divreq(iuse)
        divreq1=divreq(iuse)/OprEffT
c
c               Determine Demand limited by supply
c
c rrb 2008/08/19; Adjust to include canal loss
cx      DIVALO=AMIN1(DIVREQ(IUSE),divcap(Nd)-divmon(nd))
        DIVALO=AMIN1(divreq1,divcap(Nd)-divmon(nd))
c
c ---------------------------------------------------------
c rrb 2007/10/26; Add ability to be called by Replace
c                 Demand for a diversion called by Replace
c                  (irep=1). Limit diversion to the remaing 
c                  decree (dcrdivx-divdx)	
        if(irep.gt.0) then  
          dcrRem=dcrdivx-divdx
          divalo=amin1(DIVALO, dcrRem)
          
          if(dcrRem.lt.small) then
            iwhy=6
            cwhy='Remaining decree (DcrRem) is 0'
            goto 300
          endif           
        endif 
c
c ---------------------------------------------------------
c		              Exit if no demand        
        if(divreq(iuse).lt.small) then
          iwhy=7
          cwhy='Destination Demand (divreq1) is 0'
          goto 300
        endif 
c
c ---------------------------------------------------------
c		              Exit if no capacity        
        if((DIVCAP(ND)-DIVMON(ND)).lt.small) then
          iwhy=8
          cwhy='Destination Capacity (divcap-divmon) is 0'
          goto 300
        endif 
c        
c ---------------------------------------------------------
c                 If release type code is on, limit the
c                 release to occur only if an IWR exists
c rrb 2015/03/07; Revised use of iopsou(6,l2).  Note this use of 
c                 iopsou(6 was never operational for a type 27 rule 
cx      ireltyp=amin0(iopsou(6,l2),ifix(effmax(nd)))
        ireltyp=0
        
c
        divmax=divreq(iuse)

        if(ireltyp.gt.0) then 
          if(diwrreq(iuse).le.small) then
            divalo=0.0
          else
            divmax = diwrreq(iuse)/(float(ireltyp)/100.0)
            divalo=amin1(divalo, divmax)
          endif  
        endif
cr
cr		Set supply based on diversion if iDep=0
c       diveff1=diveff(mon,iuse)
        if(iDep.eq.0) then
          alocfs=alocfs
        else  
          alocfs=alocfs/(diveff1)              
        endif
        
      endif
c
c ---------------------------------------------------------
c		b. Destination is a reservoir
c
c     if (nd.lt.0) then
      if(ndtype.eq.2) then
c
c rrb 2007/10/26; Revise for consistency with Oprinp and
c		  rely on variable ndtype      
c       nr=-nd
        nr=nd
        ndR=nd
        ndD=0
        ndP=0
        ndI=0
        ndX=nr
        cDest=cresid(nr)
        if(iressw(nr).eq.0) ioff=3
c     
        idcd=irssta(ndR)
        idcdR=idcd        
        ndnd=ndnnod(idcd)
        
        idcdX=idcdR        
        ndndX=NDNNOD(idcdX)
        
c
c ---------------------------------------------------------
c rrb 2006/09/25; Allow multiple accounts - Initilize
cr      irow=nowner(nr)+iopdes(2,l2)-1
        nro=1
        if(iopdes(2,l2).lt.0) then
          nro=-iopdes(2,l2)
          irow=nowner(nr)
        endif

        if(iopdes(2,l2).gt.0) then
          nro=1
          irow=nowner(nr)+iopdes(2,l2)-1
        endif
        
        iuseX=irow
c
c ---------------------------------------------------------
c rrb 2006/09/25; Allow multiple accounts - Demand
        cursa=0.0
        do n=1, nro
          n1=irow+n-1
          cursa=cursa+(ownmax(n1)-curown(n1))
        end do  
        
cr      divreq1=amin1(ownmax(irow)-curown(irow),
        divreq1=amin1(cursa,
     1                volmax(nr)-cursto(nr),
     1                tarmax(nr)-cursto(nr))/fac
        
        divreq1=amax1(0.0, divreq1)        
c
c rrb 2007/12/04; Add Loss
        divreq0=divreq1
        divreq1=divreq1/OprEffT        
        DIVALO=divreq1
        divaloX=divalo
      endif
c
c ---------------------------------------------------------
c		c. Destination is a plan (Demand = Divalo)
c
c rrb 2007/08/17; Allow a Plan Destination
      if(ndtype.eq.7) then
        ndP=nd
        ndD=0
        ndR=0
        ndI=0
        cDest=pid(ndP)
        if(ifix(pon(ndP)).eq.0) ioff=2
        
        IUSE=1
        iuseX=iuse
        
        IDCD=Ipsta(NDP)
        idcdP=idcd        
        NDND=NDNNOD(IDCD)
        
        idcdX=idcd
        ndndX=NDNNOD(idcdX)        
c
c ---------------------------------------------------------
c rrb 2008/04/23; Limit the transfer for a T&C or Aug plan 
c		  Types 1 or 2
        if(iplnTyp(ndp).eq.1 .or. iplnTyp(ndp).eq.2) then
          divreq0=pdem(ndp)
          divreq1=divreq0/OprEffT        
          DIVALO=divreq1
          divaloX=divalo        
        else                          
          divreq0=big/fac
          divreq1=big/fac/OprEffT        
          DIVALO=divreq1
          divaloX=divalo        
        endif  
      endif  
c
c ---------------------------------------------------------
c		d. Destination is an Instream Flow (Demand = Divalo)
c
c rrb 2008/03/21; Allow a Plan Destination
      if(ndtype.eq.1) then
        ndI=nd
        ndP=0
        ndD=0
        ndR=0
        cDest=cifrid(ndI) 
        if(ifrrsw(ndI).eq.0) ioff=2
        
        IUSE=1
        iuseX=iuse
        
        IDCD=ifrsta(ndI)
        idcdI=idcd
        ndnd=NDNNOD(idcd)
        
        idcdX=idcdI
        ndndX=NDNNOD(idcdX)

        divreq0=flowrq(ndI)
        divreq1=flowrq(ndI)
        DIVALO=divreq1
        divaloX=divalo   
c
c rrb 2011/08/04; Instream flow reach    
        ib=ndnifb(ndI)
        ie=ndnifb(ndI) + ndnifs(ndI) -1  
        if(ioutIR.eq.1) then
          write(nlog,*) ' DivRplP; ndI, ib, ie', ndI, ib, ie
        endif     
      endif  
c
c _____________________________________________________________
c
c               Step 6; Destination is through a carrier
c		              Adjust diversion location idcd and
c			            number of downstream nodes (ndnd) but
c			            not the actual diversion location (idcdX)
c			            or actual number of downstream nodes (ndndX)
      if(intern(l2,1).gt.0) then      
        ncar=intern(l2,1)
        idcd=idvsta(ncar)
        
        idcdC=idcd
        ndnd=ndnnod(idcdC)   
        ndndC=NDNNOD(idcdC)             
c       idcdX=idcd
c       ndndx=ndnnod(idcd)
c       write(nlog,*) ' DivRplP; ncar, idcdC, ndnd', ncar, idcdC, ndnd
      endif
c
c _________________________________________________________
c
c		Step 7. Exit if off      
      if(ioff.gt.0) then
        iwhy=9
        cwhy='Plan or Div or Res is off'        
c       write(nlog,*) ' DivrplP; ioff', ioff
        goto 300
      endif
c
c _____________________________________________________________
c
c               Step 8; Exit if no demand
      if(divalo.le.small) then
        iwhy=10
        cwhy='Demand or Capacity = 0'
        goto 300
      endif  
c
c
c _____________________________________________________________
c
c rrb 2007/12/04; 
c               Step 10; Process carrier limitations
c	              	ncarry is indicator at least 1 carrier
c	              	ncnum is the number of carriers
c	              	OprEff1 is the lost (oprlost(lw)
c	              	Divalo gets reduced by carrier capacity
c	              	DivCarry is the limitating carrier capacity
c	              	noprS is the structure id of the structure
c	              	that supplied water to the accounting
c	             	  plan that already has a capacity 
c	             		adjustment
      if(ncarry.gt.0) then
c
c rrb 2015/02/03X; Correction to specify source of lopr
cx      if(lopr.gt.0) then 
        if(lopr6.gt.0) then       
          loprR=iopsou(1,lopr6)
          noprS=idivco(1,loprR)        
        endif  
      
        call SetCarL(nlog, icx, l2, fac, 
     1    maxopr,  maxdiv, intern, OprLossC,
     1    ncarry,  ncnum,  noprS,  internT,
     1    OprEff1, DivCap, DivMon, DivCarry, Divalo)
        
        if(divcarry.lt.small) then
          iwhy=12
          cwhy='Carrier capacity (Divcarry) = 0 '
          goto 300
        endif    
      endif   
      
c _____________________________________________________________
c
c               Step 11 - Set Diversion (divact) and release (relact)
c       
      if(ncarry.eq.0) then      
        divact=amin1(alocfs, divalo)
      else
        divact=amin1(alocfs, divalo, divcarry)      
      endif
      divact=amax1(0.0, divact)
      
c     write(nlog,*) ' DivRplP;', 
c    1  alocfs*fac, divalo*fac, divcarry*fac, divact*fac
c
c _____________________________________________________________
c
c               Step 12 - Find minimum exchange potential
c                        in the river from the diversion
c                        node (idcdX) to the Exchange point
c                        iExPoint(l2)
c
      IMCD=Idcd
      ISS=Idcd
      if(ioutE.eq.1)
     1  write(nlog,*) ' DivRplP; Exchange Point iss, imcd, iExPoint = ', 
     1  iss, imcd, iExPoint(l2)
c
c rrb 2008/08/15; Revise to scan downstream of the Destination 
c		  or the Carrier (ndnd) not the destination (ndndX)
cx    DO nx=1,ndndX
      DO nx=1,ndnd
c
        if (iDep.eq.0 .and. iss.eq.iExPoint(l2)) goto 110
        
        IF(AVAIL(IMCD).GT.AVAIL(ISS)) IMCD=ISS 
        ISS=IDNCOD(ISS)
      end do
c
  110 pavail=amax1(0.0,avail(imcd))
  
      if(ioutE.eq.1)
     1  write(nlog,*) ' DivRplP; Exchange Data = ', 
     1  iexpoint(l2), idcd, ndnd, ndndX, imcd, avail(imcd)*fac  
c _________________________________________________________
c
c 		Step 12a Calculate diversion 
c		a. Based on Diversion
      if(iDep .eq. 0) then
        DIVACT=amin1(pavail,divalo,alocfs)
        divact=amax1(0.0,divact)
        divactx=divact
c       
        relact=-divact
c
        if(iout.eq.1) then
          write(nlog,342) pavail*fac, divalo*fac, alocfs*fac, divact*fac
        endif
      
        if(divact.le.small) then
          iwhy=11
          cwhy='Exchange potential = 0'
          goto 300
        endif
      endif  
c
c _________________________________________________________
c
c
c 		Step 12b Calculate diversion 
c		b. Based on Depletion

      if(iDep.eq.1) then
c
c                a. FIND THE MIN AVAIL FLOW DOWNSTREAM (avail(imcd))
        if(iouta.eq.1) 
     1   write(nlog,*) ' DivrplP', divact*fac, alocfs*fac, pavail*fac
        DIVACT=amin1(divalo,alocfs)
        divact=amax1(0.0,divact)
c
c               b. If Available flow < demand
c                  set the release (RELACT) to the difference
c                  (DIVACT - PAVAIL) or the depletion (divact*diveff)
        if(pavail .lt. divact) then
          divact=pavail          
          relact=-1.*(divact*diveff1)
        else
          relact=-1.*(divact*diveff1)
        endif
c
c               d. If iout=1 print detailed results
        if(iout.eq.1) then
          c = divact*diveff1
          write(io99,390) 2, divact*fac, pavail*fac, relact*fac, c*fac
        endif
      endif
c
      iwhy=0
c
c _________________________________________________________
c
c		Step 13; Calculate Diversion less loss (DivactL)
c			 Note: OprEffT = Total Carrier Efficiency 
      DivactL=divact*OprEffT
      
      if(divact.le.small) then
        iwhy=17
        cwhy='Available flow with River return = 0'
        goto 300
      endif
      
c
c
c _________________________________________________________
c
c               Step 14; Adjust for water dumped to the river 
c		                     by a carrier. Call RivRtn that will:
c		                     1. Adjust the diversion as necessary since
c		                        the return location may be upstream or  
c		                        downstream of the carrier diversion
c		                     2. Add the river return to Avail
c		                     3. Remove the ultimate destination from Avail
c		                     Note navail=1 allows avail to be adjusted

      if(iout.eq.1) write(nlog,*) ' DivRplP; nriver, ncarry = ',
     1   nriver, ncarry
     
      if(nRiver.gt.0) then 
c
c rrb 2010/10/15; Update to allow operationn with a depletion release
        if(idep.eq.0) then
          DepfacM=1.0
        else
          DepfacM=diveff1
        endif          
        nAvail=1  
        
        call RivRtn(
     1    icx, nriver, l2, ndtype, iscd, nd, iuse, idcd, idcdX, 
     1    fac, smallN, oprEffT, relact, adj, divact, divactL, 
     1    ncnum, nAvail, alocfsR, DepFacM, imcdR, corid1)
     
        if(imcdR.gt.0) cimcdR=cstaid(imcdR)
     
        if(divact.le. small) then
          iwhy=18
          cwhy='Available flow with River Return = zero'
          goto 300
        endif  
      endif
c
c _____________________________________________________________
c
c               Step 15; Add Plan or Reservoir release (relact)
c
      if(iout.eq.1) write(nlog,*) ' DivRplP; Takout for relact = ',
     1   iscd, relact*fac

      AVAILR=AVAIL(Iscd)
c
c rrb 2014/11/24; Add a plan release to the original source downstream
c
      if(iout26.eq.1) then
        write(nlog,*) ' '         
        write(nlog,*) ' DivRplP_4; lopr26, ndns26, iscd26'
        write(nlog,*) ' DivRplP_4; ', lopr26, ndns26, iscd26
      endif     
c      
      if(lopr26.eq.0) then      
        CALL TAKOUT(maxsta,AVAIL,RIVER,AVINP,QTRIBU,IDNCOD,
     1              relact,ndns,iscd)
      else
        call TAKOUT(maxsta,avail,river,avinp,qtribu,idncod,
     1              relact,ndns26,iscd26)     
      endif 
          
c
c rrb 2011/07/28; Allow water to be added to the source node (iscd)
c                 Different logic than Divrpl that sets 
c                 avail(iscd) = availR after water is added in and 
c                 befor returns are calculated.  
cx    avail(iscd)=availR     
c
c _____________________________________________________________
c
c               Step 16; Remove destination from Avail & River
c			                  unless it is adjusted in RivRtn (nriver>0)
c          		          nCarry = 0 No carrier
c		                           = 1 No return to River, (Final
c			                           destination is from a carrier)
      if(iout.eq.1) write(nlog,*) ' DivRplP; nriver, ncarry = ',
     1   nriver, ncarry
c
c -----------------------------------------------------------
c
c rrb 2011/08/04; Update to allow an Instream flow reach
      if(ndtype.ne.1) then
        if(nriver.eq.0) then  
          if(ncarry.eq.0) then
            CALL TAKOUT(maxsta,AVAIL,RIVER,AVINP,QTRIBU,IDNCOD,
     1              divact,ndndX,idcdX)
          else   
            CALL TAKOUT(maxsta,AVAIL,RIVER,AVINP,QTRIBU,IDNCOD,
     1              divact,ndndC,idcdC)
          endif
        endif
      endif
c
c _________________________________________________________
c rrb 2011/08/04; Update to allow a instream flow point or reach
c		            Step 17; Update to allow an Instream flow reach  
c            Note adjust avail but not river 
      if(ndtype.eq.1) then
        issr=ifrsta(ndI)
        do i=ib,ie       
          if(ioutIR.eq.1) then
            write(nlog,*) 
     1        ' DivRplP; ndI, issr, avail, divact, avail-divact' 
            write(nlog,*) ' DivRplP; ', ndI, issr, 
     1        avail(issr)*fac, divact*fac, (avail(issr)-divact)*fac
          endif
          
          avail(issr)=avail(issr)-divact
          issr=idncod(issr)      
        end do  
      endif
c      
c
c _________________________________________________________
c		            Step 18; Add return flows if a diversion     
c
c rrb 2015/02/03X; Correction
cx    if(ndtype.eq.3 .and. ipuse.eq.0) then
      if(ndtype.eq.3 .and. ipuse.le.0) then      
        CALL RTNSEC(icx,divactL,l2,IUSE,Idcd,nd,ieff2)       
      endif
c
c _________________________________________________________
c
c		            Step 19; Calculate Reuse
      if(ipUse.gt.0) then
c
c ---------------------------------------------------------
c		a. Associated Plan type is reuse (add)
        if(iplntyp(ipUse).ne.9) then
          if(ndtype.eq.3) then
            CALL RtnsecR(icx,divactL,l2,iuse,idcd,nd2,
     1         ieff2,ipUse)
          endif
c
c		Reservoir Reuse   
c
c rrb 2007/12/04; Add Loss       
          if(ndtype.eq.2) then
            psuply(ipUse)=psuply(ipUse)+divactL
            psuplyT(ipUse)=psuplyT(ipUse)+divactL
            psto2(ipUse)=psto2(ipUse)+divactL*fac   
            ipsta1=ipsta(ipUse)
          endif  
c
c		Plan type is an OOP (subtract)
          if(iplntyp(ipUse).eq.9) then
            psto2(ipUse)=amax1(0.0, psto2(ipUse)-divactL*fac)
            ipsta1=ipsta(ipUse)
            if(psto2(ipUse).le.smallN) then
              write(nlog,*) '  DivRplP; psto2(ipUse', psto2(ipUse)
              goto 9999
            endif
            if(abs(psto2(ipUse)).le.small) psto2(ipUse)=0.0  
          endif       
        endif     
      endif
c
c _____________________________________________________________
c
c               Step 20; Update Destination 
c
c ---------------------------------------------------------
c		a. Diversion Destination
c
      if(ndtype.eq.3) then
        USEMON(IUSE)=USEMON(IUSE)+divact
c
c rrb 2007/12/04; Add Loss        
c       DIVREQ(IUSE)=DIVREQ(IUSE)-divact
        DIVREQ(IUSE)=DIVREQ(IUSE)-divactL
        DIVMON(ND  )=DIVMON(ND  )+divact
      endif  
c
c ---------------------------------------------------------
c               b. Reservoir Destination

c     if(nd.lt.0) then
      if(ndtype.eq.2) then
        divaf=divact*fac
c
c rrb 2007/12/04; Add Loss        
c       cursto(nr)=cursto(nr)+divaf
        divafL=divaf*OprEffT
        cursto(nr)=cursto(nr)+divafL
c
c ---------------------------------------------------------
c rrb 2006/09/25; Allow multiple accounts - Allocate
c		            Note:
c		             iResT1=0 distributes based on ownership ratio
c		             nrown1=number of accounts in this reservoir
c		             iown = first account associated with this reservoir  
c		             icx  = subrouine calling accou.for       
c		             ia   = account to adjust
      
        nrX=ndR        
        iResT1=0
        nrown1=nro
        iownX=irow
        icx2=128
        if(ncarry.eq.0)  ia=18
        if(ncarry.ne.0)  ia=4

        cresid1=cresid(nrX)
c
c rrb 2007/12/04; Add Loss             
c       call accou(maxacc, maxown, nrX, ownmon, curown, accr, ia, 
c    1    ownmax, iownX, nrown1, cursa, divaf, iResT1, icx, cresid1)
        call accou(maxacc, maxown, nrX, ownmon, curown, accr, ia, 
     1    ownmax, iownX, nrown1, cursa, divafL, iResT1, icx2, cresid1)


c
c ---------------------------------------------------------
c		Set Carrier Data
cx rrb 2017/12/03; Simplify adding reservoir loss
      ResLoss=divafL*TranLoss
      ResLoss=amax1(0.0, divaf-divafL)
c
c rrb 2010/10/15; Correction adjust for carrier loss
c									change divaf to divafL
c
c rrb 2017/12/03; Correction for TRANSIT LOSSES (NOT CARRIER LOSSES)
cx        Note accr(4 gets set in accou.f but accr(30 (loss) does not
cx        Note qres(4 and qres(30 do not get set in accou.f
cx
cx        qres(4  From Carrier by Storage or Other to reservoir
cx        qres(18 From River by Exchange to Reservoir
cx        qres(27 From Carrier Loss
cx        qres(30 From River Loss      
cx
cx        if(ncarry.eq.0) then
cx          qres(18,nr)=qres(18,nr)+divafL
cx        else
cx          qres(4,nr)=qres(4,nr)+divafL
cx        endif  
        
        if(ncarry.eq.0) then
          qres(18,ndR)=qres(18,ndR)+divafL+ResLoss
          qres(30,ndR)=qres(30,ndR)+ResLoss
c
c rrb 2017/10/20 Addition
          accr(18,irow)=accr(18,irow)+ResLoss
          accr(30,irow)=accr(30,irow)+ResLoss
        else        
          qres(4,ndR)=qres(4,ndR)+divafL+ResLoss
          qres(27,ndR)=qres(27,ndR)+ResLoss     
c
c rrb 2017/10/20 Addition
          accr(4,irow)=accr(4,irow)+ResLoss
          accr(27,irow)=accr(27,irow)+ResLoss
        endif
            
c
c ---------------------------------------------------------
c               Check reservoir roundoff when exiting routine
        in=1
        isub=20
        call chekres(nlog, maxres, in, isub, iyr, mon, nr,nowner,
     1               curown,cursto,cresid)
     
      endif
c      
c ---------------------------------------------------------
c		c. Destination is a Plan
c rrb 2007/12/04; Add Loss
      if(ndtype.eq.7) then
        psuply(ndP)=amax1(0.0, psuply(ndP)+divactL)
        pdrive(ndP)=pdrive(ndP)+divactL
        psuplyT(ndP)=psuplyT(ndp)+divactL
 

c rrb 2008/01/15; If a T&C destination set qdiv(30 an
c		              adjustment to total diversion in outbal2
        if(iplntyp(ndP).eq.1) then
          qdiv(30,idcd)=qdiv(30,idcd)+DIVACT
       endif          
c
c rrb 2011/02/28; Correction since Qdiv was revised to
c                 only operate on diversions    
        if(iscd.ne.idcdC .and. nCarry.eq.0) then
cx      if(nCarry.eq.0) then           
          qdiv(31,idcd)=qdiv(31,idcd)+Divact  
          qdiv31 = qdiv(31,idcd)    
        endif    
      endif  

c      
c ---------------------------------------------------------
c		d. Destination is an Instream Flow
c rrb 2008/03/22; ISF Destination
      if(ndtype.eq.1) then
        flowrq(ndI)=amax1(0.0, flowrq(ndI) - divactL)
c
c rrb 2011/08/04; Instream flow reach
        do i=ib,ie
          florqr(i)=florqr(i)-divact
          qdivr1=qdivr(i)
          qdivr(i)=qdivr(i)+divact
        end do  
c
c rrb 2008/01/15; If a T&C destination set qdiv(30 an
c		      adjustment to total diversion in outbal2
c rrb 2011/08/04; Correction 
cx      if(iplntyp(ndP).eq.1) then
        qdiv(30,idcd)=qdiv(30,idcd)+DIVACT
        qdiv30=qdiv(30,idcd)        
cx      endif    
      endif   
c
c _____________________________________________________________
c
c               Step 21; Update Source 
c   
c ---------------------------------------------------------
c		a. Source is a Plan
c  
      if(nsP.gt.0) then
        psuply(nsp)=amax1(0.0, psuply(nsp) +relact)
        if(iplntyp(nsp).eq.3 .or. iplntyp(nsp).eq.5) then
          psto2(nsp)=amax1(psto2(nsp)+relact*fac,0.0)                
        endif  
c
c rrb 2014/11/24; Revise the treatment of a plan release for a type 11
cx        if(iplntyp(nsP).eq.4 .or. iplntyp(nsP).eq.6) then
c rrb 2015/10/04; Back to version befor allowing a type 13
c                 plan type for changed water rights
c                 Note this shows the plan release
c                 as a return flow to system qdiv(36
c rrb 2015/01/15
cx        if(iplntyp(nsP).eq.4 .or. iplntyp(nsP).eq.6 .or.
cx     1    iplntyp(nsP).eq.11) then  
        if(iplntyp(nsP).eq.4 .or. iplntyp(nsP).eq.6) then      
          qdiv(36,iscd)=qdiv(36,iscd) - relact
          qdiv36=qdiv(36,iscd)
        endif  
c
c rrb 2015/10/04; Correction to address type 11 before allowing a
c                 a type 13 plan type for changed water rights
        if(iplntyp(nsP).eq.11) then
          qdiv(38,iscd)=qdiv(38,iscd) - relact
          qdiv38=qdiv(38,iscd)
c
c rrb 2018/01/15; Correction store water from an admin plan (type 11) 
c                 "from plan" at the destination for water balance
          qdiv35=divact
          if(ndtype.eq.1) then
            qdiv(35,ndI) = qdiv(35,ndI) + divact
          endif
          
          if(ndtype.eq.2) then
            qdiv(35,nr) = qdiv(35,nr) + divact
          endif
          
          if(ndtype.eq.3) then
            qdiv(35,nd) = qdiv(35,nd) + divact
          endif
          
          if(ndtype.eq.7) then
            qdiv(35,ndP) = qdiv(35,ndP) + divact
          endif
         
          if(ioutQ.eq.1) then
            write(nlog,*) 
     1      ' DivRplP; nsp, iscd, cstaid, qdiv38' 
            write(nlog,*) 
     1      ' DivRplP;', nsp, iscd, cstaid(iscd), qdiv38*fac
          endif
        endif  
c
c rrb 2015/01/24; Report amount diverted as Carried... at the 
c                 the source plan node (iscd), the original
c                 source plan node befor a split (iscd1) and
c                 if not a carrier, the water right node (iscd26)
c
c                 qdiv(38 Carried water reported as Carried, Exchange
c                   or Bypassed but not used to calculaete
c                   River Divert in Outmon.f
c
c rrb 2015/03/07; Source is a Changed Water Right (type 13)   
cx      if(iplntyp(nsP).eq.11 .and. lopr26.gt.0) then 
        if(iplntyp(nsP).eq.13 .and. lopr26.gt.0) then 
        
c
          qdiv(38,iscd) = qdiv(38,iscd) - relact
          qdiv(38,iscd1)= qdiv(38,iscd1) - relact
c
          if(nd26.ne.ncar) then
            qdiv(38,iscd26)=qdiv(38,iscd26) - relact     
          endif
c                  
          if(iout26.eq.2) then
            write(nlog,*) ' '  
            write(nlog,*)          
     1        ' DivRplP_5;   lopr26   iscd   iscd1   iscd26',
     1                   '     nd26    ncar  relact qdiv(38'
             write(nlog,'(a13, 6i8, 20f8.0)')                                      
     1        ' DivRplP_5; ', lopr26, iscd, iscd1, iscd26, 
     1          nd26, ncar, relact*fac,  qdiv(38,iscd26)*fac                                       
          endif  
        endif             
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
cx        qdiv(18,idcd)=qdiv(18,idcd)+divaf
        endif  
c        
c               Check reservoir roundoff when exiting routine
        in=1
        isub=20
        call chekres(nlog, maxres, in, isub, iyr, mon, nsR,nowner,
     1               curown,cursto,cresid)
      endif         
      

c
c _________________________________________________________
c               Step 22; Update shortage (ishort) and
c                        amount associated with this operation
c                        rule (divalo)
 300  if(ndtype.eq.3 .and. divact+small.lt.divalo) ishort = 1
c
c rrb 2007/10/29; Set ishort for a quick exit. Reset initilized
c		  divact to 0 from -1
      if(divact.lt.smalln) then
        ishort=1
        divact=0.0
        relact=0.0        
      endif 
c
c _________________________________________________________
c               Step 23; Update data to be passed out
c                        of this routine. Note
c                        divact .NE. relact if release
c                        for depletion only
c
      divactx=-relact
      divacty=divact         
c     write(nlog,*) ' DivrplP;', cSour, cDest, divact*fac, divalo*fac
c
c _________________________________________________________
c               Step 24; Update operational right (divo)
c		                     to amount used from supply (relact) 
c                        not amount diverted (divact)
c		                     Note this is required for consistency and
c		                     for plan output to be correct (it uses 
c                        *.xop output)
cr    divo(l2)=divo(l2)+divact
c
c rrb 2015/09/06; TEST to keep small numbers from accumulating
      if(divactx.gt.small) then
        divo(l2)=divo(l2)-relact
      endif
c
c rrb 2015/09/06; Detailed output      
      if(iout.eq.5 .and. mon.eq.monout .and. l2.eq.18) then
        write(nlog,*) '  DivRplP; l2, ',l2, iyrmo(mon), corid1, 
     1    xmonam(mon), divact*fac, divo(l2)*fac
      endif
      
c
c _________________________________________________________
c rrb 2007/12/04
c               Step 25; Update Qdiv for Source and Destination

      EffmaxT1=(100.0-OprLossC(l2,1))/100.0
c
c rrb 2008/04/02; Correction call SetQdiv when there is a carrier 
c     if(ncarry.gt.0) then
        call SetQdiv(nlog, nCarry, nRiver,
     1    ndD, ndR, iscd, idcdX, idcdC,
     1    divact, TranLoss, EffmaxT1, OprEffT, fac, 
     1    rloss, maxsta, maxdiv, maxqdiv, qdiv, icx,
     1    internL, corid1)
c     endif
     
      if(iout.eq.1) then
        write(nlog,*) '  DirectEx; Call SetCarry'
        call flush(nlog)
      endif  
c
c _________________________________________________________
c rrb 2007/12/04
c               Step 26; Update Qdiv(18, ), Qdiv(32 ,) and Qdiv(20, ) 
c		                     for the carrier(s)
c		                     Also calculate return flows for carrier losses
c		                     using the structure properties of the carrier	
c                        write(nlog,*) ' DirectEx; nd, iuse', nd, iuse  
      if(ncarry.gt.0) then         
        call setQdivC(
     1    nlog, ncarry, ncnum, nd, ndD, l2, iscd, idcdX, idcdC,
     1    nriver, divact, TranLoss, EffmaxT1, 
     1    fac, maxsta, maxdiv, maxqdiv, maxopr, 
     1    intern, idvsta, qdiv, divmon, 
c rrb 2009/06/09; Correction          
cx   1    maxrtnw, maxdivw, OprEff1, ipuse,  
     1    maxrtnPP, maxplan, OprEff1, ipuse,  
     1    pctlosPP, rlossP, oprLossc,internT,
     1    icx, corid1)
      endif
c _________________________________________________________
c
c *************************************
c
c rrb 2014/11/24
c     Step 26a; Adjust the source capacity based on the amount released
c		   
      if(iout.eq.1) then
        write(nlog,*) ' DivRplP; l2, ioprlim, iopsou(5,l2)'   
        write(nlog,*) ' DivRplP;', l2, ioprlim(l2), iopsou(5,l2)         
      endif
c      
c *************************************
c
c rrb 2014/11/24
c     Step 26a; Adjust the amount diverted (divmon) based on 
c               the amount released which is how
c               a diversion is limited by capacity    
      if(lopr26.gt.0) then    
        if(nd26.ne.ncar) then
          divcap2=divcap(nd26) - divmon(nd26)         
          divmon(nd26) = amax1(divmon(nd26) + divact,0.0)
          divcap3=divcap(nd26) - divmon(nd26)   
        else
          divcap2=divcap(nd26) - divmon(nd26)          
          divcap3=divcap2
        endif
c
        if(iout26.eq.2) then
          write(nlog,*) ' '           
          write(nlog,*) 
     1     ' DivRPlP_6;',
     1     '  lopr26  ncarry     nd26    ncar',
     1     '  divact divcap1 divcap2 divcap3'
          write(nlog,'(1x,a12,4i8, 20f8.0)')
     1     ' DivRplP_6;', lopr26, ncarry, nd26, iscd,
     1       divact*fac, divcap1*fac, divcap2*fac, divcap3*fac
        endif        
      endif
c
c _________________________________________________________
c
c rrb 2007/07/03; 
c               Step 27; Adjust monthly or annual plan limitations
      if(iout.eq.1) write(nlog,*) ' DivRplP; ', iopsou(5,l2)
c
c rrb 2009/01/15; Revise to recognize ioprlim(l2)=3      
cx    if(iOprLim(l2).gt.1 .and. iopsou(5,l2).gt.0) then
c rrb 2015/02/03; Revise to recognize ioprlim(l2)=4
c rrb 2015/03/07; Allow Ioprlim to be 1-9
cx    if(iOprLim(l2).eq.1 .or. iOprlim(l2).eq.2) then
      if(iOprlim(l2).eq.2 .or. iOprLim(l2).eq.4 .or.
     1   iOprlim(l2).eq.7 .or. iOprLim(l2).eq.9) then
c     
        if(iopsou(5,l2).gt.0) then
      
          lopr5=iopsou(5,l2)
          ipLim=iopsou(1,lopr5)
          
cx          if(iplim.eq.5.and. divact.gt.small) 
cx     1      write(nlog,*) 'DivRpl before', 
cx     1      iplim,  psto1(ipLim), psto2(ipLim), corid1     
        
          call SetLimit(
     1      nlog, icx, lopr5, ipLim, ioprlim(l2), fac, 
     1      divact, OprmaxM(lopr5), OprMaxA(lopr5), 
     1      Oprmax(lopr5,mon), Oprmax(lopr5,13), OprmaxM1, OprmaxM2, 
     1      psto1(ipLim), psto2(ipLim), corid1)     
     
cx          if(iplim.eq.5 .and. divact.gt.small) 
cx     1      write(nlog,*) 'DivRpl after', 
cx     1      iplim,  psto1(ipLim), psto2(ipLim), corid1     
        endif
      endif
c
c_____________________________________________________________
c rrb 2007/07/09; Allow T&C return flow obligations to be assigned 
c		              at the destination (herein)

c                 Step 28; Calculate return flow obligation
c			            Note return patterns may be the default
c			            structure (iuse1) or from plan data
c			            see RtnsecP
c
c	
      if(iout.eq.1) write(nlog,*) '  DivRplP; iptc = ', iptc,
     1  Culimit, divactT*fac, rettot*fac  	
c     write(nlog,*) '  DivRplP; iptc = ', iptc	
c
c ---------------------------------------------------------
c rrb 2007/11/27; Update T&C calculations
      if(ipTC.gt.0) then 
        if(iDep.eq.0) then    
          divactT= divact
        else
          divactT= divact/DepFac
        endif  
        
        rettot = divactT *(1.0-CuFac)      
        if(iDep.eq.0) divleft=0.0
        if(iDep.eq.1) divleft=rettot
        
        call SetTC(nlog, icx, l2, ipTC, iDep, fac, 
     1    divactT, CuFac, divleft, rettot, iopsou(4,l2),
     1    pdem(ipTC), pdrive(ipTC), csour)
      endif  
c
c _____________________________________________________________
c
c               Step 29 - Final printout befor exit
c
c      if(iout.ge.1) then
c      if(iout.gt.0 .and. ioutiw.eq.iw) then
       iprint=1
       if(irep.eq.1 .and. divact.lt.small) iprint=0
c
c ---------------------------------------------------------
c		Detailed Header            
c 
cx     if(iout.ge.1 .and. iw.eq.ioutiw) then      
       if(iout.gt.0 .and. ioutiw.eq.iw .and. iprint.eq.1) then
c
c                a. FIND THE MIN AVAIL FLOW DOWNSTREAM (avail(imcd))
         if(idcd.gt.0) then
           iss=IDcd
           NDNDS=NDND
           CALL DNMFSO(maxsta,avail,idncod,iSS,ndndS,imcd)
           avail1=avail(imcd)
         endif  
         

         ncallX=ncallX+1
         if(ncallX.eq.1) then
           write(nlog,270) corid1, cdestyp, ccarry, cTandC, cpuse,
     1      cDivTyp(l2), Creplace, cRiver          
         endif  
         
       
         if(iday.eq.0) then
           write(nlog,280) '  DivrplP   ',
     1       iyrmo(mon),   xmonam(mon), idy,cSour, cDest, cImcdR,
     1       iwx, iw, l2, ndX, iuseX, iDep, ncarry, 
     1       diveff1*100., oprEffT*100,
     1       divreq1*fac,  alocfs1*fac, alocfs*fac, divaloX*fac, 
     1       pavail*fac,   OprmaxM1,    OprmaxM2, 
     1       divcarry*fac, abs(relact*fac), divact*fac,  iwhy, cwhy,
     1       imcd, ishort, qdiv31*fac
         else
           write(nlog,282) '  DivrplP   ',
     1       iyrmo(mon),   xmonam(mon), idy, cSour, cDest,
     1       iwx, iw, l2, ndX, iuseX,   iDep, ncarry,
     1       diveff1*100., oprEffT*100, 
     1       divreq1*fac,  alocfs1*fac, alocfs*fac, divaloX*fac, 
     1       pavail*fac,   OprmaxM1,    OprmaxM2, 
     1       divcarry*fac, abs(relact*fac), divact*fac,  iwhy, cwhy
     
         endif
         if(divact*fac.gt.small .and. avail(imcd)*fac.lt.smalln)
     1   write(nlog,*) ' ***** Problem' 
       endif
c
c _____________________________________________________________
c               
c               Step 30 - Check Entire Avail array out
      if(iouta.gt.0) 
     1  write(nlog,*) ' DivrplP; Calling Chekava on way out ', 
     1 ' ID = ', corid1
     
      call chekava(18, maxsta, numsta, avail)
      
      if(iouta.gt.0) write(nlog,*) ' DivrplP; OK going out for ', 
     1 ' ID = ', corid1
c
c ---------------------------------------------------------
c rrb 2014-06-15; Print Qdiv data
      if(ioutQ.eq.2) then
        write(nlog,*) ' DivRplP Out; Qdiv report'
        write(nlog,'(4x, 39i8)') (j, j=1,39)
        do i=1, numsta
          write(nlog,'(i5, 39f8.0)') i, (qdiv(j,i)*fac, j=1,39)
        end do
      endif      

c
c _____________________________________________________________
c
c                Step 31 - Return
      return
c _________________________________________________________
c               Formats
 270    format(/, 
     1  '  DivRplP (Type 28); Operation Right ID = ', a12,
     1  ' Destination Type = ', a12,
     1  ' Carrier (Y/N) = ',a3, ' T&C Plan (Y/N) = ',a3,
     1  ' Reuse Plan (Y/N) = ', a3,
     1  ' Diversion Type = ', a12,' Called by Replace = ', a3,
     1  ' Release to River = ', a12,/     
     1  '  Note: Divreq is the demand, AloCfs is the supply, '
     1     'Pavail is the available flow, Relact is the release,',
     1     ' Divact is the diversion',/
     1  '  DivRplP     Iyr  Imo  Day',
     1  ' Source ID    Dest ID      Min ID      ',
     1  ' Iter  iwx   l2 ndX iuseX iDep  nCarry',
     1  ' DivEff1 OprEffT Divreq1 Alocfs1  Alocfs divaloX  Pavail',
     1  ' Oprmax1 Oprmax2 Divcary  RelAct  DIVACT',
     1  ' iwhy cwhy                                ',/
cx   1  ' imcd  sht   avail  Qdiv29 iOprLim iopsou5',/
     1  ' ___________ ____ ____ ____',
     1  ' ____________ ____________ ____________',
     1  ' ____ ____ ____ ____ ____ ____ _______', 
     1  ' _______ _______ _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' ____ ____________________________________')
cx   1  ' ____ ____ _______ _______ _______ _______ _______ _______')
 280   format(a12, i5, 1x,a4, i5, 3(1x,a12), 6i5, i8,
     1 12f8.1, i5, 1x,a48, 2i5, 20f8.1)
     
 282   format(a12, i5, 1x,a4, i5, 2(1x,a12), 6i5,
     1 11f8.3, i5, 1x,a48, 2i5, 20f8.3)

 301   format(/,60('_'),/,
     1   '  DivrplP;  iyr  mon iteration ', 3i5,/
     1   '  DivrplP;   iw iout    ichk99 ', 3i5)

 342   format(
     1     '  DivrplP; Diversion Limit;',
     1     '  pavail  divalo  alocfs  divact',/
     1     '                          ', 20f8.2)
 360   format(
     1  'DivRplP;  Divact  Rettot DivLeft DivactE Culimit    IpTC',
     1             '     Imo    Pdem   PdemT',/
     1  '            ', 5f8.0, 2i8, 20f8.0)
     
 390   format(
     1       '  DivrplP; Release for Depletion Data;',/
     1       '                  #  divact  pavail  relact      CU',/
     1       '           ', i8, 20f8.2)
     
c
c _____________________________________________________________
c
c		Error Processing
 9999 continue
         write(nlog,270) corid1, cdestyp, ccarry, cTandC, cpuse,
     1     Creplace, cRiver
                          
         if(iday.eq.0) then
           write(nlog,280) '  DivrplP   ', 
     1       iyrmo(mon),   xmonam(mon), idy, cSour,cDest, cImcdR,
     1       iwx, iw, l2, ndX, iuseX, iDep, ncarry,
     1       diveff1*100.,  oprEffT*100,
     1       divreq1*fac,  alocfs1*fac, alocfs*fac, divaloX*fac, 
     1       pavail*fac,   OprmaxM1,    OprmaxM2, 
     1       divcarry*fac, abs(relact*fac), divact*fac,  iwhy, cwhy
cx     1       imcd, ishort, avail1*fac, qdiv29*fac,
cx     1       float(iOprLim(l2)), float(iopsou(5,l2))
     
         else
           write(nlog,282) '  DivrplP   ',
     1       iyrmo(mon),   xmonam(mon), idy, cSour,cDest,        
     1       iwx, iw, l2, ndX, iuseX, iDep, ncarry, 
     1       diveff1*100., oprEffT*100,
     1       divreq1*fac,  alocfs1*fac, alocfs*fac, divaloX*fac, 
     1       pavail*fac,   OprmaxM1,    OprmaxM2, 
     1       divcarry*fac, abs(relact*fac), divact*fac,  iwhy, cwhy
cx     1       imcd, avail1*fac, qdiv29*fac,
cx     1       float(iOprLim(l2)), float(iopsou(5,l2))
     
         endif
 
      write(6,1050) 
      write(nlog,1051) 
      
 1050 format('    Stopped in DivrplP',/,
     1       '    See the *.log file')
 1051 format('    Stopped in DivrplP')
      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)
      stop 
      END
