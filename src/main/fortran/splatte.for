c
c ---------------------------------------------------------
      subroutine SPlatte(iw, l2, l2D, ishort, nd, divactx,ncallX)
c
c
c _________________________________________________________
c	Program Description
c
c     SPlatte; Type 40
c       It releases water from the S Platte Compact plan
c         When teh destination is a diversion iopdesR=3 its by exchange
c         When the destination is an instream flow iopdesR=1 its direct
c       It allows an exchange from a plan that was filled
c       by the South Platte Compact via call IfrrigSP
c       to a diversion located upstream of the Washington County line
c       (Upstream of WD 40)
c _________________________________________________________
c
c       Called b:
c         Execut
c _________________________________________________________
c
c       Update History
c
c rrb 2011/01/02; Copied DivRplP and simplified
c rrb 2011/05/19; Revised to call Dsamod to allow the routine
c                 to account for immediate return flows
c _________________________________________________________
c       Documentation
c
c
c ---------------------------------------------------------
c	VARAIABLES USED WHEN CALLED BY EXECUTE OR REPLACE 
c
c       iw              water right order
c       l2              order of S Platte Compact operating rule
c       ishort          shortage indicator 0=no, 1=yes
c
c       divact          actual diversion
c       divactx         actual plan release from SPlatte
c       divacty         actual diversion from SPlatte Note may
c                       not equal divactx if releasing for
c                       depletion
c
c       Other Key variables
c       icx   = 6      subroutine call #
c       ieff2 = 0      always use average efficiency
c             = 1      use max efficiency if ieffmax = 1
c       internT     =  Intervening structure type
c		                   1 = Carrier
c		                   2 = River
c
c       iopsou(1,l2)   ns 
c		                   if > 0 source reservoir
c                      if < 0 source plan
c
c       iopsou(2,l2)   iown   N/A
c       iopsou(3,l2)   Source Plan (source 2 for Splatte only)
c			  associated with a release
c       iopsou(5,l2)	1 Diversion limits are adjusted for the
c		                    operatng rule = iopsou(5,l2).
c		                  2 Release limits are imposed for the
c		                    operatng rule = iopsou(5,l2).
c       iopsou(6,l2)    Reservoir release type and efficiency
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
c       ireltyp         Same as iopsou(6,l2)
c
c	      ipUse           Reuse plan for return flows
c                       Note ipUse=ireuse(l2)
c
c       iopdes(1,l2)    nd   destination diversion or reservoir
c       iopdes(2,l2)    iuse destination user
c
c       iopdesr(l2)     Destination type (reservoir, diversion or
c                        plan)
c
c       ipsta(nsP)     	iscd Source Plan river station
c
c      	idcd            Actual Diversion location. May be one 
c                          a Diversion, Reservoir or Plan or Carrier

c	      idcdD		        Destination Diversion location on river
c	      idcdR           Destination Reseroivr location on river
c	      idcdC           Destination carrier location on river
c	      idcdP           Destination Plan on river
c	      idcdX           Destination Diversion, Reservoir,
c		                    Plan. Not a CARRIER
c
c	      l2              Water right pointer
c       l2D             Destination water right pointer
c
c       ndnnod (idcd)	  ndnd  number of nodes downstream of 
c                         the destination or first carrier
c       ndnnod(iscd) 	  ndns   number of nodes downstream of supply
c       ndnE            number of nodes from diversion to 
c                       the exchange point (plan)
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
c       nsP			        source is a Plan (iopsou(1,l2)<0
c	      pavail          minimum available flow 
c
c       small           a local limit used for convergence, etc.
c       iouta           0 no detailed printout for avail
c                       n yes detailed printout for avail
c	      iwhy            1 reason for no diversoin
c
c       ncallx		      number of times called per time step
c
c	      ndtype 	=       iopdesr(l2) Destination Type
c	                        2 = 'Reservoir'
c                         3 = 'Diversion'
c                         7 = 'Plan     '
c
c ---------------------------------------------------------
c		Depletion Vs Diversion Data
c       iDep            Diversion type
c                         0 release to meet diversion demand
c                         1 release to meet depletion
c       DepFac          factor to convert from diversion to Depletion
c
c
c
c ---------------------------------------------------------
c		Loss Data
c	      OprLoss(l2)     Transit loss (%) 
c	      	              Transit loss is a true loss no routing
c	      ioprloss        int(OprLoss) carrier loss switch
c	      	                + transit loss, maybe carrier loss
c	      	                - 0 transit loss, maybe carrier loss
c	      TranLoss        Transit loss (fraction)
        
c	      OprLossC(l2,i)  Conveyance loss from a carrier (%)
c	      	              Conveyance loss gets routed to system
c	      OprLost= 	      conveyance loss (cfs)
c             
c	      OprEff1 = 	    source Carrier Efficiency 
c                        (1.0 - OprLoss(l2)/100)
c	      OprEffT = 	    Total Carrier Efficiency 
c       
c       psuply(np)      Running plan returns for this month. It 
c                         increases or decreases based on opr rules
c       psuplyT(np)     Total monthly plan demands this month 
c                        (may increase but will not decrease based on
c                         operating rules
c             
c	      effmaxT=	      Transit loss for a ditch 
c
c
c       nCarry          0 No carrier
c		                    1 No return to River, Final Destination is
c			                    from a carrier
c	                      2 Return to River, Final Destination is
c                         from a carrier
c		                    3 Return to River, Final Destination is 
c		                     from the river
c
c       nRiver	        Indicator a release to the River
c       
c	      ncnum           # of carriers
c
c
c ---------------------------------------------------------
c		Reporting (Outmon) Data
c
c       qdiv(14         Div by an instream flow (e.g. ifrrig)
c
c       qdiv(18         Carrier passing thru a structure 
c       qdiv(20         From Carrier by Storage, Exchange or Plan
c	      qdiv(28         Carried, Exchange or Bypass (column 11)
c                       Stored via a reuse or admin plan 
c		    qdiv(30         From River from a Res or Reuse Plan         
c                         to a T&C or Aug Plan or ISF. Note non 
c                         consumptive
c       qdiv(31         From the river via a reuse or Admin Plan
C 	    qdiv(35 	      Water treated as a new source (from Plan) in
c                         outmon
c       qdiv(36         Water released to the system as return flow
c                         plan types 4 & 6 reuse from a diversion or
c                         tmtn diversion

c       qres(18         From River by Exchange to Reservoir
c       qres(4          From carrier by Storage to reservoir

c _________________________________________________________
c	Dimensions
      include 'common.inc'
      character cwhy*48, cdestyp*12, rec12*12,
     1  ccarry*3,    cpuse*3, cresid1*12,
     1  cplntyp*12,  cTandC*3, csrctyp*12, cReplace*3,
     1  cDest*12,    cSour*12, cDest1*12, cRiver*12,
     1  corid1*12, cStaMin*12, cstaid1*12, cdivtyp1*12
     
c      character corid1*12, ccallby*12, cwhy*48
      character ccallby*12

c
c _____________________________________________________________
c
c               Step 1a - Initilize general variables
c
c ---------------------------------------------------------
c               Control debut printout
c		iout=0 none, iout=1 detailed, iout=2 summary
c		ioutiw = water right used for detailed output
c		ioutE = detailed output for Exchange Reach
c		ioutA = detailed output for ChekAva
      iout=0
      ioutiw=0
      ioutE=0
      ioutA=0
      icx=40
      
      nlogx=nlog
cx    nlogx=6
      
      cDest1='NA'
      cStaMin='NA'
      corid1='NA'
      ccallby='Splatte'
      if(l2.gt.0) corid1=corid(l2)
      
      if(ichk.eq.140) iout=2
      if(corid1.eq. ccall) ioutiw=iw
      ioutin=iout
c
c rrb 2011/04/11; Update to print detailed results to an ISF
c                 for testing
      if(iout.eq.2 .and. corid1.eq. 'Compact_Isf ') ioutiw=iw
      
cx      if(iout.eq.2 .and. ioutiw.eq.iw) then        
cx        write(nlogx,102) iout, ccall, corid1
cx 102    format(/, 72('_'),/ '  SPlatte; iout, ccall, corid1 = ',
cx     1   i5, 1x, 2a12)
cx      endif             
c
c ---------------------------------------------------------
      ioff=0
c
c rrb 2011/01/02; ND is passed into this routine
cx    nd=0

cx      goto 500
      idcd=0
      imcd=0
      
      ndP=-1
      nsP=-1
      
      iscd=-1
      idcd=-1
      idcdX=-1
      idcdD=0
      idcdR=0
      idcdP=0
      idcdC=0
      idcdI=0
      
      nRiver=0 
      cRiver='NA          '  
             
      imcdE=-1
      iresw=0
      icx=40

      small=0.001
      smalln=-1.0*small
      ishort = 0     

      cstaid1='NA          ' 
      
      
      if(iout.eq.1) then
        write(nlogx,301) iyr, mon, iwx, iw, iout, ichk99
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
      divact = 0.0
      divalo = 0.0
      relact=0.0
            
      divactx=0.0
c
c rrb 2007/10/26; Revise so call by Replace works (e.g. do not reset)
      ndx=-1
      iuseX=-1
      
      pavail=-1.0/fac
      pavail2=-1.0/fac
      divreq1=-1.0/fac
      alocfs=-1.0/fac
      alocfsR=-1.0/fac
            
      divreq0=-1.0/fac
      divreq1=-1.0/fac 
      divcap1=-1.0/fac
      dcrdiv1=-1.0/fac
           
      qdiv35=-1./fac
      qdiv36=-1./fac
      
      big=99999.      
      
      diveff1=-0.01
      avail1=-1./fac
      qdiv29=-1./fac
      
      iwhy=-1
      cwhy='N/A'
      cDest='-1'
      cSour='-1'
      cdestyp='NA '
c
c _________________________________________________________
c               d. Destination is a diversion (nd is passed in)
c     write(nlogx,*) '  SPlatte; ncallx = ', ncallx

      if(iopDesR(l2).eq.3) then     
cx      nd  =iopdes(1,l2)
        ndtype=3
        cdestyp='Diversion'
        cDest=cdivid(nd)
      endif 
c
c _________________________________________________________
c               d. Destination is an instream flow 
      if(iopDesR(l2).eq.1) then     
        nd  =iopdes(1,l2)
        ndtype=1
        cdestyp='Instream'
        cDest=cifrid(nd)
      endif  
c
c ---------------------------------------------------------
c		            e. Source. 
      csrctyp='NA '
      ns =iopsou(1,l2)      
      
      if(ns.le.0) then
        write(nlogx,*) '  SCompact Problem; wrong source type'
        goto 9999
      else 
        nsP=ns
        csrctyp='Plan'
        Csour=pid(nsP)
      endif  
c
c ---------------------------------------------------------
c		            f. Carrier      
      nCarry=0
      ccarry='No '
c
c ---------------------------------------------------------
c		            g. ReUse Plan  
c	  	ipUse = Reuse plan    
      ipUse=0
      cpuse='No '
      cplntyp='NA '
c      
c ---------------------------------------------------------
c               h. T&C Plan
      cTandC='No '
c
c ---------------------------------------------------------
c		            i. Set CU limit switch      
cx    rec12=cDivTyp(l2)
      rec12='Diversion'
      cdivtyp1='Diversion'
      iDep=0
      if(rec12(1:9).eq.'Diversion') iDep=0
      if(rec12(1:9).eq.'Depletion') iDep=1
     
c     
c ---------------------------------------------------------
c		            j. Set Depletion switch  
cr rrb 2007/08/21; Set factor for Depletion Vs Diversion
      if(iDep.eq.0) then
        DepFac=1.0
      else
        DepFac=Effmax(nd)/100.        
      endif
c
c ---------------------------------------------------------
c rrb 2005/11/27; 
c		            k. Add CuFac
      CuFac=OprEff(mon,l2)/100.              
c
c
c ---------------------------------------------------------
c		            l. Called by Replace
      cReplace='No'      
c ---------------------------------------------------------
c		            m. Variable Efficiency
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
c		            n. CU limit
      
      if(iDep.eq.0) then
        CuLimit=1.0
      else
        CuLimit=OprEff(mon,l2)/100.
      endif
c
c ---------------------------------------------------------
c rrb 2007/12/04; 
c		            o. Set Transit and Carrier Loss Data      
c		               Oprloss is transit loss (%)
c		               ioprLoss > 0 transit loss
c		            	          = 0 no transit loss
c                  OprEffT  = Total Carrier Efficiency 
      call SetLoss(nlogx, icx, l2, fac, 
     1 maxopr,   intern,  Oprloss, OprLossC,
     1 ioprloss, nCarry,  nRiver,  ncnum, 
     1 OprLost,  OprEff1, OprEffT, TranLoss, internT,internL,corid1)
c
c ---------------------------------------------------------
c		            p. Check Avail Array
      call chekava(18, maxsta, numsta, avail)
      if(iouta.gt.0) write(nlogx,*) ' SPlatte; OK going in for ', 
     1 ' ID = ', corid1
c
c _________________________________________________________
c
c		Step 2: Exit if not on this month
c
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
        ndnS=NDNNOD(iscd) 
c
c ---------------------------------------------------------
c
        ALOCFS=amax1(0.0, psuply(nsp))
               
        if(iout.eq.1) then
          write(nlogx,*) ' SPlatte; nsp, psuply, divo, alocfs'
          write(nlogx,*) '         ',
     1      nsp, psuply(nsp)*fac, divo(l2)*fac, alocfs*fac
        endif
               
        if(alocfs.le.small) then
          iwhy=2
          cwhy='Plan Supply = zero'
          goto 300
        endif  
      endif
     
c
c _________________________________________________________
c
c               Step 5a; Destination (Demand = Divalo)
      if(ndtype.eq.3) then
        nd2 = nd
        ndx = nd
c
c ---------------------------------------------------------
c		a. Destination is a Diversion      
        ndD=nd
        ndR=0
        ndP=0
        ndI=0
        cdivtyp1='Diversion'
        if(idivsw(nd).eq.0) ioff=2
c
c	rrb 2011/01/02; Use the l2 pointer for the destination (l2D  
c
        IUSE=NDUSER(ND)+IDIVCO(2,L2D)-1  
        
        diveff1=diveff(mon,iuse)/100.          
        iuseX=iuse        
        
        IDCD=IDVSTA(ND)
        idcdD=idcd
        NDND=NDNNOD(IDCD)
c
c rrb 2011/05/17;  Set ndnE the number of downstream nodes
c                  from the diversion to the exchange (plan) node        
        ndnE=ndnD-ndnS
        
cx       write(6,*) '  Splatte ndnD, ndnS, ndnE =', ndnD, ndnS, ndnE
        
        idcdX=idcdD
        ndndX=NDNNOD(idcd)  
c
c ---------------------------------------------------------
c		Exit if no demand  
c       
c               Add Loss        
        divreq0=divreq(iuse)
        divreq1=divreq(iuse)/OprEffT
              
        if(divreq1.lt.small) then
          iwhy=3
          cwhy='Destination Demand (divreq1) is 0'
          goto 300
        endif 
c
c ---------------------------------------------------------
c		Exit if no capacity        
        divcap1=divcap(nd) - divmon(nd)
        if(DIVCAP1.lt.small) then
          iwhy=4
          cwhy='Destination Capacity (divcap1) is 0'
          goto 300
        endif 
c
c ---------------------------------------------------------
c rrb 2011/04/25; 
c             Limit the diversion to the decree   
cx      DIVALO=AMIN1(divreq1, divcap(nd)-divmon(nd))
        dcrdiv1=dcrdiv(l2D)-divd(l2D)       
        if(dcrdiv1.lt.small) then
          iwhy=5
          cwhy='Remaining Decree (dcrdiv1) is 0'
          goto 300
        endif 
      endif      
c
c ---------------------------------------------------------
c             Determine Demand limited by capacity
      DIVALO=AMIN1(divreq1,divcap1,dcrdiv1)      
c
c ---------------------------------------------------------
c		b. Destination is an instream flow
      if(ndtype.eq.1) then      
        ndD=0
        ndR=0
        ndP=0
        ndI=nd
        
        cdivtyp1='Instream'
        diveff1=1.0          
        iuseX=iuse        
        
        IDCD=IFRSTA(ND)
        idcdD=idcd
        NDND=NDNNOD(IDCD)
        
        idcdX=idcdD
        ndndX=NDNNOD(idcd)  
c       
c               Add Loss        
        divreq0=flowrq(nd)
        divreq1=flowrq(nd)
c
c               Set demand to 99999 since it was decreased
c               when water was stored in the plan
        divreq1=99999./fac
        dcrdiv1=99999./fac
        DIVALO=divreq1  
c             
      endif      
c
c _________________________________________________________
c
c		Step 7. Exit if off      
      if(ioff.gt.0) then
        iwhy=6
        cwhy='Plan or Div or Res is off'        
c       write(nlogx,*) ' SPlatte; ioff', ioff
        goto 300
      endif
c
c _____________________________________________________________
c
c               Step 8; Exit if no demand
      if(divalo.le.small) then
        iwhy=7
        cwhy='Demand or Capacity = 0'
        goto 300
      endif  
c
      
c _____________________________________________________________
c
c               Step 11 - Set Diversion (divact) and release (relact)
c            
      divact=amin1(alocfs, divalo)
      divact=amax1(0.0, divact)
      
      if(iout.eq.1) write(nlogx,*) ' SPlatte; alocfs, divalo, divact', 
     1  alocfs*fac, divalo*fac, divact*fac
c
c _____________________________________________________________
c
c               Step 12 - If the destination is a diversion, call DsaMod
c                         that will find the minimum exchange potential
c                          in the river from the diversion node (idcd) 
c                          to the source (iscd) which is the exchange point
c
      if(ndtype.eq.3) then
        IMCD=Idcd
        ISS=Idcd
        if(ioutE.eq.1)
     1    write(6,*)
     1    ' SPlatte; Exchange Point iss, imcd, iExPoint = ', 
     1    iss, imcd, iExPoint(l2)
c  
c rrb 2011/05/17;  Call DsaMod
     
         call DsaMod(
     1    icx, ioutin, l2D, imcd, idcd, ndnE, nd, iuse, ieff2, 
     1    fac, pavail, divalo, divact, oprEffT, divactL, 
     1    iwhy, icase, ishort, iresw, cCallBy, corid1, cwhy)
     
        relact=-divact
        divactx=-relact     
      endif
c _____________________________________________________________
c
c               Step 13 - If the destination is an isf
c                         set the available flow to 9999    
c                         and calculate diversion
c      
      if(ndtype.eq.1) then
        pavail=99999./fac
        cStaMin='NA'
        imcdE=-1         
c
c ---------------------------------------------------------
c
        DIVACT=amin1(pavail,divalo,alocfs)
        divact=amax1(0.0,divact)
c               
        relact=-divact
        divactx=-relact
        
        if(iout.eq.1) then
          write(nlogx,342) pavail*fac, divalo*fac, alocfs*fac, divact*fac
        endif
        
        if(divact.le.small) then
          iwhy=8
          cwhy='Exchange potential (Pavail)= 0'
          goto 300
        endif   
c
c ---------------------------------------------------------
c               Remove the ISF from Avail
      
        if(iout.eq.1) write(nlogx,*) ' SPlatte; Takout divact = ',
     1     idcdX, divact*fac
         ix=1
        call takou2(icx, maxsta, avail ,idncod, divact, 
     1              ix, idcdX)
      endif
c
c _________________________________________________________
c
c
      iwhy=0
      
      if(divact.le.small) then
        iwhy=17
        cwhy='Available flow with River return = 0'
        goto 300
      endif
      
c
c _____________________________________________________________
c
c               Step 15; Add in Plan release (relact = -divact)
c
      if(iout.eq.1) write(nlogx,*) ' SPlatte; Takout for relact = ',
     1   iscd, relact*fac
      
      AVAILR=AVAIL(Iscd)
      CALL TAKOUT(maxsta,AVAIL,RIVER,AVINP,QTRIBU,IDNCOD,
     1            relact,ndns,iscd)
c     
c rrb 2011/01/06; Adjust the source
cx    avail(iscd)=availR     
c
c rrb 2011/05/19; Move above
cxxc _____________________________________________________________
cxxc
cxxc               Step 16; If an isf, remove diversion from Avail
cxxc          		    nCarry = 0 No carrier
cxxc		                       1 No return to River, Final Destination
cxxc			                       is from a carrier
cxx      if(iout.eq.1) write(nlogx,*) ' SPlatte; Takout divact = ',
cxx     1  idcdX, divact*fac
cxx      if(ndtype.eq.1) then
cxx        ix=1
cxx        call takou2(icx, maxsta, avail ,idncod, divact, 
cxx     1              ix, idcdX)
cxx      endif
c
c _____________________________________________________________
c
c               Step 18; Add check to see if more can be diverted
c
c rrb 2011/04/18; Add check to see if more can be diverted
      if(ndtype.eq.3) then
        IMCD=Idcd
        ISS=Idcd    
        DO nx=1,ndnd
          if (iDep.eq.0 .and. iss.eq.iExPoint(l2)) goto 120
          IF(AVAIL(IMCD).GT.AVAIL(ISS)) IMCD=ISS        
          ISS=IDNCOD(ISS)
        end do
c
  120   pavail2=amax1(0.0,avail(imcd))
        cStaMin=cstaid(imcd)  
        imcdE=imcd  
      endif   
c
c _____________________________________________________________
c
c               Step 19; Update Destination 
c
c ---------------------------------------------------------
c		a. Diversion Destination
c
      if(ndtype.eq.3) then
        USEMON(IUSE)=USEMON(IUSE)+divact
c
c rrb 2007/12/04; Add Loss        
c       DIVREQ(IUSE)=DIVREQ(IUSE)-divact    
c                           
c rrb 2011/04/04; correction 
cx      DIVREQ(IUSE)=DIVREQ(IUSE)-divactL   
        DIVREQ(IUSE)=DIVREQ(IUSE)-divact 
        DIVMON(ND  )=DIVMON(ND  )+divact
c
c rrb 2011/04/25; Limit diversion to its decree        
        divd(l2D) = divd(l2D)+divact        
      endif  
c
c
c _____________________________________________________________
c
c               Step 20; Update Source 
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
c rrb 20111/05/22; Correction qdiv(28 is carried, exchanged bypass       
cx      qdiv(28,iscd) = qdiv(28,iscd) + divact
c       qdiv(36         Water released to the system as return flow
c                         plan types 4 & 6 reuse from a diversion or
c                         tmtn diversion

c
        if(iplntyp(nsP).eq.4 .or. iplntyp(nsP).eq.6 .or.
     1     iplntyp(nsP).eq.11) then  
c
          qdiv(36,iscd)=qdiv(36,iscd) - relact
          qdiv36=qdiv(36,iscd)
        endif          
        qdiv35=qdiv(35,idcd)         
        
      endif 
c
c _________________________________________________________
c               Step 21; Update shortage (ishort) and
c                        amount associated with this operation
c                        rule (divalo)
c jhb 2014/07/13 move line label 300 down to step 28
c                to avoid occasional array bound issue when
c                idcdx is undefined
c 300  if(ndtype.eq.3 .and. divact+small.lt.divalo) ishort = 1
      if(ndtype.eq.3 .and. divact+small.lt.divalo) ishort = 1
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
c               Step 22; Update data to be passed out
c                        of this routine. Note
c                        divact .NE. relact if release
c                        for depletion only
c
      divactx=-relact
c     write(nlogx,*) ' SPlatte;', cSour, cDest, divact*fac, divalo*fac
c
c _________________________________________________________
c               Step 23; Update operational right (divo)
c		                     to amount used from supply (relact) 
c                        not amount diverted (divact)
c		Note this is required for consistency and
c		for plan output to be correct (it uses *.xop output)
cr    divo(l2)=divo(l2)+divact
      divo(l2)=divo(l2)-relact
      
c
c _________________________________________________________
c rrb 2007/12/04
c               Step 24; Update Qdiv for Source and Destination
c ---------------------------------------------------------
c               Destination is a diversion
c               qdiv(31 = From the river via a reuse or Admin Plan
      if(iopDesR(l2).eq.3) then
cx      qdiv(31,idcdX) = qdiv(31,idcdX) + divact
        qdiv(30,idcdX) = qdiv(30,idcdX) + divact
      endif
c ---------------------------------------------------------
c               Destination is an ISF     
      if(iopDesR(l2).eq.1) then
c
c rrb 2011/02/13; Show the diversion to ISF as:
c                 qdiv(31 From the river via a reuse or Admin Plan
c                 qdiv(36 return flow
c
        qdiv(31,idcdX) = qdiv(31,idcdX) + divact   
        qdiv(36,idcdX) = qdiv(36,idcdX) + divact      
      endif
      
      if(iout.eq.1) then
        write(nlogx,*) '  DirectEx; Call SetCarry'
        call flush(nlogx)
      endif  
c
      if(iout.eq.1) write(nlogx,*) ' SPlatte; ', iopsou(5,l2)
c _________________________________________________________
c
c rrb 2007/07/03; 
c               Step 27; Print results to the chk file if non-zero.
      if(divact.gt.small) then
        ncallX=ncallX+1
        if(ncallX.le.1)  write(nchk,290) corid1
        write(nchk,292) '  SPlatte   ',
     1      iyrmo(mon),   xmonam(mon), idy,cSour, cDest, cStaMin, 
     1      divact*fac
      endif

cc
c _____________________________________________________________
c
c               Step 28 - Final printout befor exit
c
c     if(iout.ge.1) then
c     if(iout.gt.0 .and. ioutiw.eq.iw) then
c jhb 2014/07/13 change line label 300 to here
c                jump to here when op rule is not active
 300  continue
      iprint=1
c
c rrb 2011/04/04; turn off detailed printout      
cx    if(divact.lt.small) iprint=0
c
c ---------------------------------------------------------
c		Detailed Header            
c 
cx    if(iout.ge.1 .and. iw.eq.ioutiw) then      
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
        if(ncallX.le.2) then
          write(nlogx,270) corid1, cdestyp, ccarry, cTandC, cpuse,
     1    cDivTyp1, Creplace, cRiver                
        endif  
         
       
        if(iday.eq.0) then        
          
          write(nlogx,280) '  SPlatte   ',
     1      iyrmo(mon),   xmonam(mon), idy,cSour, cDest, cStaMin,
     1      ncallX, iwx, iw, l2, ndX, iuseX, iDep, imcdE, ncarry,  
     1      iprint, diveff1*100., oprEffT*100,
     1      divreq1*fac,  divcap1*fac, dcrdiv1*fac, alocfs*fac, 
     1      pavail*fac,   pavail2*fac, 
     1      abs(relact*fac), divact*fac,  iwhy, cwhy
        else
          write(nlogx,282) '  SPlatte   ',
     1      iyrmo(mon),   xmonam(mon), idy, cSour, cDest, cStaMin,
     1      ncallX, iwx, iw, l2, ndX, iuseX,   iDep, imcdE, ncarry, 
     1      iprint, diveff1*100., oprEffT*100, 
     1      divreq1*fac,  divcap1*fac, dcrdiv1*fac,  alocfs*fac, 
     1      pavail*fac,   pavail2*fac, 
     1      abs(relact*fac), divact*fac,  iwhy, cwhy      
        endif
c
c rrb 2011/05/12; Correction
      endif
c
c ---------------------------------------------------------
c		Detailed Header            
c jhb 2014/07/04 added the if block so the code will run without crashing
c                need to figure out what SHOULD happen when
c                  imcd = -1 !!
        if (imcd.ge.1) then
          if(divact*fac.gt.small .and. avail(imcd)*fac.lt.smalln)
     1      write(nlogx,*) ' ***** Problem avail is less than 0'
        else
c          if(iout.eq.1) then
            write(nlogx,*)
     1      ' SPlatte; avail(imcd) error:  imcd = ',
     1      imcd
c          endif
        endif
        
c
c rrb 2011/04/18; Update
       if(ndtype.eq.3 .and. divact+small.lt.divalo) then     
         if(pavail2*fac.gt.small) 
     1     write(nlogx,*) ' ***** Problem more can be diverted',
     1     ' Min flow = ', pavail2*fac, ' Diversion = ', divact*fac,
     1     ' Unmet demand = ', divreq(iuse)*fac      
       endif
c
c _____________________________________________________________
c               
c               Step 29 - Check Entire Avail array out
      if(iouta.gt.0) then
        write(nlogx,*) ' SPlatte; Calling Chekava on way out ', 
     1   ' ID = ', corid1
      endif 
      
      call chekava(18, maxsta, numsta, avail)
      
      if(iouta.gt.0) then
        write(nlogx,*) ' SPlatte; OK going out for ', 
     1 ' ID = ', corid1
      endif
c
c _____________________________________________________________
c
c                Step 30 - Return
 500  return
c _________________________________________________________
c               Formats
 270  format(/, 
     1  '  SPlatte (Type 40); Operation Right ID = ', a12,
     1  ' Destination Type = ', a12,
     1  ' Carrier (Y/N) = ',a3, ' T&C Plan (Y/N) = ',a3,
     1  ' Reuse Plan (Y/N) = ', a3,
     1  ' Diversion Type = ', a12,' Called by Replace = ', a3,
     1  ' Release to River = ', a12,/  
     1  '  Where Divreq1 = Demand, Divcap1 = Remaining capacity,',
     1        ' Dcrdiv1 = Remaining decree, Pavail = Exchange limit,',
     1        ' Alocfs = Plan supply, Relact = Plan release, ',  
     1        ' Divact = Diversion',//   
     1  '  SPlatte     Iyr  Imo  Day',
     1  ' Source ID    Dest ID      Min ID      ',
     1  '    # Iter  iwx   l2 ndX iuseX iDep imdcE nCarry  iprint',
     1  ' DivEff1 OprEffT Divreq1 Divcap1 Dcrdiv1  Alocfs',
     1  '  Pavail Pavail2  RelAct  DIVACT',
     1  ' iwhy cwhy                                ',/
cx     1  ' imcd  sht   avail  Qdiv29 iOprLim iopsou5',/
     1  ' ___________ ____ ____ ____',
     1  ' ____________ ____________ ____________',
     1  ' ____ ____ ____ ____ ____ ____ ____ ____',
     1  ' _______ _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______ _______',
     1  ' ____ ____________________________________')
cx     1  ' ____ ____ _______ _______')

 280   format(a12, i5, 1x,a4, i5, 3(1x,a12), 8i5, 2i8,
     1 10f8.1, i5, 1x,a36, 2i5, 20f8.1)
     
 282   format(a12, i5, 1x,a4, i5, 3(1x,a12), 8i5, i8,
     1 10f8.3, i5, 1x,a36, 2i5, 20f8.3)

 290  format(/, 
     1  '  SPlatte (Type 40); Operation Right ID = ', a12,/    
     1  '  SPlatte     Iyr Imo   Day',
     1  ' Source ID    Dest ID      Min ID           DIVACT',/
     1  ' ___________ ____ ____ ____',
     1  ' ____________ ____________ ____________ __________')
     
     
 292   format(a12, i5, 1x,a4, i5, 3(1x,a12), 1x, f10.0)
 
 301   format(/,60('_'),/,
     1   '  SPlatte;  iyr  mon iteration ', 3i5,/
     1   '  SPlatte;   iw iout    ichk99 ', 3i5)

 342   format(
     1     '  SPlatte; Diversion Limit;',
     1     '  pavail  divalo  alocfs  divact',/
     1     '                          ', 20f8.2)
 390   format(
     1       '  SPlatte; Release for Depletion Data;',/
     1       '                  #  divact  pavail  relact      CU',/
     1       '           ', i8, 20f8.2)
     
c
c _____________________________________________________________
c
c		Error Processing
 9999 continue
         write(nlogx,270) corid1, cdestyp, ccarry, cTandC, cpuse,
     1     Creplace, cRiver
                          
         if(iday.eq.0) then
           write(nlogx,280) '  SPlatte   ', 
     1       iyrmo(mon),   xmonam(mon), idy, cSour,cDest, cStaMin,
     1       ncallX, iwx, iw, l2, ndX, iuseX, iDep, imcdE, ncarry,
     1       diveff1*100.,  oprEffT*100,
     1       divreq1*fac,  divcap1*fac, dcrdiv1*fac,  alocfs*fac, 
     1       pavail*fac,   pavail2*fac,
     1       abs(relact*fac), divact*fac,  iwhy, cwhy
     
         else
           write(nlogx,282) '  SPlatte   ',
     1       iyrmo(mon),   xmonam(mon), idy, cSour,cDest,        
     1       ncallX, iwx, iw, l2, ndX, iuseX, iDep, ncarry, 
     1       diveff1*100., oprEffT*100,
     1       divreq1*fac,  divcap1*fac, dcrdiv1*fac,  alocfs*fac, 
     1       pavail*fac,   pavail2*fac,
     1       abs(relact*fac), divact*fac,  iwhy, cwhy
         endif
 
      write(6,1050) 
      write(nlogx,1051) 
      
 1050 format('    Stopped in SPlatte',/,
     1       '    See the *.log file')
 1051 format('    Stopped in SPlatte')
      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)
      stop 
      END
