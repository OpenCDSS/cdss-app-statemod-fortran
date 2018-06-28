C     Last change:  RRB  22 Oct 2002    4:12 pm
c
c ---------------------------------------------------------
      subroutine DivResP2(iw,l2,ishort,irep,tranlim,dcrdivx,divdx,
     1      divactx,divacty,ncallX)
c
c _________________________________________________________
c	Program Description
c
c     DivResP2;	  Type 27 Rule: 
c                 Plan to a Reservoir, Diversion, Reservoir, 
c                 Plan or Instream flow by a direct release 
c                 via river or carrier
c		              Tied to a reuse plan (ipuse.ne.9) or
c		               a OOP Plan (ipuse.eq.9)
c	                Can release to meet a diversion or a depletion
c		              Can be called by Replace
c                 
c           	  Called by Execut
c _________________________________________________________
c
c     Update history
c
c rrb 2015/01/20; Revised to handle a type 26 Changed Water right
c
c rrb 2011/08/05; Revised to allow an instream flow reach
c     Note the same logic works for both an ISF point and a reach
c
c rrb 2008/06/10; Allow return to river then diversion
c		  from a carrier again (internT = 1 Carrier, 2=Return)
c		  Also redefine nCarry (see documentation)       
c
c rrb 2008/01/03; Add ability to release from the carrier to the
c		    river as follows:
c		  1 Use nRiver to indicate a return to the river
c		  2 Set Avtemp = Avail
c		  3 Estimate release as if the carrier were the
c		    ultimate destination
c		  4 Adjuste Avtemp
c		  5 Add in return to river
c		  6 Decrease diversion based on final destination
c		    adjust Avail
c rrb 2007/10/26; Add ability to be called by Replace
c rrb 2007/10/12; Revised to limit a release to an annual limit
c		  Note if iOprlim = 1 & 6 are not currently operational
c		  Note if iOprlim = 2, 4 7 or 9 release limits are adjusted
c		    for the operatng rule in iopsou(5,l2).
c		  Note if iOprlim = 3, 4, 8 or 9 diversions are limited to the 
c		    amount simulated by another operating rule iopsou(6,l2).
c     Note if IOprlim = 5 or 7-9 the source is a Changed Water 
c       Right Plan (type 13) that has special reporting capability
c
c rrb 2007/08/22; Revised to allow the return flow calculation for 
c     the T&C requirement to vary as follows:
c     if(iopsou(4,l2) = 1 call RtnsecP for a STANDARD		  
c     if(iopsou(4,l2) = 2 call RtnsecC for a CONSTANT
c
c rrb 2007/08/17; Revised to allow the destination to be a plan
c rrb 2007/07/09; Revised to allow the T&C obligation to be
c		  assigned to the destination herein
c rrb 2006/07/26; Copied DivresP and edited accordingly to handle
c                 a reservoir source and a OOP plan 
c ________________________________________________________
c	Documentation
c
c ---------------------------------------------------------
c	VARIABLES USED WHEN CALLED BY REPLACE (IREP>0)
c	    irep    = Called by Replace (0=no, >0=yes)
c           dcrdivx = decree of destination diversion right
c           divdx   = diversion to date of diversion right
c
c ---------------------------------------------------------
c	VARAIABLES USED WHEN CALLED BY EXECUTE OR REPLACE 
c
c        divalo       = demand
c        relalo       = available supply in plan (acft)
c        alocfs       = available supply in plan (cfs)
c        divactX      = actual amount diverted
c        divacty      = divactX used to pass out and use
c                        in SetQdivC
c        relact       = actual amount released from the plan
c     	 flomax       = maximum flow downstream of the 
c                       reservoir (e.g. current stream flow
c                       plus the reservoir release (CFS)
c                   
c        icx          = subroutine call #
c        
c        ieff2        = 0 always use average efficiency
c        ieff2        = 1 use max efficiency if ieffmax=1
c        
c        
c        ipsta(nsP)   = iscd Source Plan river station
c        
c        iscd         = source river station (Plan or Reservoir)
c
c	       idcd         = Actual Diversion location. May be 
c                             a Diversion, Reservoir or Plan or Carrier 
c	       idcdD	      = Destintaion Diversion river location 
c	       idcdR        = Destination Reservoir location on river
c	       idcdP        = Destination Plan location on river
c        
c	       idcdC        = Destination carrier location on river
c	       idcdI	      = Destination instream flow
c              
c	       idcdX        = Destination Diversion, Reservoir,
c			                   or but not a CARRIER
c	       internT     =  Intervening structure type
c	     	               	1 = Carrier
c	     	               	2 = River
c
c        iopsou(1,l2) = ns 
c			                  if > 0 source reservoir
c                       if < 0 source plan
c        iopsou(2,l2) = source reservoir account
c        iopsou(3,l2) = T&C Plan id (for return flow obligation)
c			   associated with a release
c        iopsou(4,l2) = 1, 2 or 3 call RtnsecP for the STANDARD
c			   component of a T&C obligation calculation		  
c        iopsou(4,l2) = 2 or 3call RtnsecC for the CONSTANT
c			   component of a T&C obligation calculation		  
c
c        iopsou(5,l2) = A Plan ID containing monthly Diversion limits
c                       Read in Oprinp when Oprlimit = 2, 4 or 7
c
c        iopsou(6,l2) = A Operating rule ID whos diversion will
c                       limit diversions by this operating rule
c                       Read in Oprinp when Oprlimit = 3, 4 or 8
c
c        iopsou(7,l2) = Type 26 Operating rule that provided water to a
c                       Changed Water Right Plan (type 13)
c                       Read in Oprinp when Oprlimit = 5 or 9

c	       iopdesr(l2)    = 3 destinatin is a diversion
c	        		          = 2 destination is a reservoir
c	        		          = 7 destination is a plan
c       
c	       iExPoint(l2) = exchange point
c
c        iopdes(1,l2)   nd
c	                     	if > 0 destination diversion ID 
c		                    if < 0 destination reservoir ID
c
c        iopdes(2,l2)   = destination owner 
c        ireltyp        Not operational
c                       For other operating rules this is the
c                       Reservoir release type and efficiency
c                       0 = release to meet demand
c                       +n = release only if a CIR (IWR) exists
c                       and limit release to not exceed IWR/n,  
c                       Note nmax = min(nmax, effmax) to save 
c                       iterating
c        iresw          = destination type
c                        0 diversion destination
c                        1 reservoir destination
c
c	       ipUse          = Associated Reuse plan
c                         Note ipUse=ireuse(l2)
c	       if(iplnTyp(ipUse) .ne. 9) a diversion or
c                         reservior resue plan
c	  		 if(iplnTyp(ipUse) .eq. 9) a OOP Plan
c
c        iout           = switch for detailed printout 
c                      
c        imonsw()       = monthly on off switch  
c                      
c        ires           = switch 0=diversion, 1=plan
c                      
c        l2             = order of the destination diversion
c        l2             = order of operating rule. Note
c                         when called by execute (ityopr=2 or 3); l2=l2
c                         when called by replace (ityopr=10) l2 is a 
c                         array holder
c
c	       ndtype         = iopdesr(l2) Destination Type
c		                      2 = 'Reservoir'
c            		          3 = 'Diversion'
c                         7 = 'Plan     '
c
c        nCarry         = 0 No carrier
c		                      1 No return to River, Final Destination is
c			                      from a carrier
c	                        2 Return to River, Final Destination is
c                             from a carrier
c		                      3 Return to River, Final Destination is 
c			                      from the river
c
c	       OprPct(l2)       Percent ownership of an admin plan
c	       OprPct1          same as above
c
c ---------------------------------------------------------
c		Depletion Vs Diversion Data
c           iDep          Diversion type
c                         = 0 release to meet diversion demand
c                         = 1 release to meet depletion
c           DepFac        = factor to convert from diversion to Depletion
c
c
c
c ---------------------------------------------------------
c		Loss Data
c          	OprLoss(l2)   Transit loss (%) 
c          			          Transit loss is a true loss no routin
c                         psuply(np)    Running plan returns for this month. It 
c          	              increases or decreases based on opr rules
c          	psuplyT(np)   Total monthly plan demands this month 
c                                (may increase but will not decrease based on
c                                operating rules
c          	ioprloss    	int(OprLoss) carrier loss switch
c                   			+ transit loss, maybe carrier loss
c                   			- 0 transit loss, maybe carrier loss
c          	TranLoss    	Transit loss (fraction)
           
c          	OprLossC(l2,i)Conveyance loss from a carrier (%)
c          		  	        Conveyance loss gets routed to system
c          	OprLost= 	    conveyance loss (cfs)
c                 
c          	OprEff1 =   	Source Carrier Efficiency 
c                          (1.0 - OprLoss(l2)/100)
c          	OprEffT 	    Total Carrier Efficiency 
c                 
c          	effmaxT		    Transit loss for a ditch 
c          
c           nCarry	  	  Carrier type
c                           0 No carrier
c          		              1 No return to River, Final Destination 
c                             from a carrier
c          	                2 Return to River, Final Destination is 
c                             off a carrier
c          		              3 Return to River, Final Destination is 
c                             off the river
c           
c          	nRiver	        Indicator a release to the River
c          	ncnum          	# of carriers
c
c
c ---------------------------------------------------------
c		Reporting (Outmon) Data
c
c           qdiv(18         Carrier passing thru a structure 
c           qdiv(20         From Carrier by Storage, Exchange or Plan
c		        qdiv(30         From River from a Res or Reuse Plan         
c                           to a T&C or Aug Plan. Note non consumptive
c                           See type 49 Divrplp2                      

c           qdiv(31         From River by Exc or Plan by DivresP2 or 
c                             DivrplP
c	        
c           qdiv(36         Water released to the system as return flow
c                            plan types 4 & 6 reuse from a diversion or
c                            tmtn diversion
c           qdiv(38         Carried water reported as Carried, Exchange
c                             or Bypassed but not used to calculaete
c                             River Divert in Outmon.f
c                         
c           qres(4          From Carrier by Storage or Other to reservoir
c           qres(11         From Storage to carrier
c           qres(12         From Storage to River for Use (Powres*)
c           qres(18      	  From River by Exchange to Reservoir
c           qres(27         From Carrier Loss
c           qres(30         From River Loss

c                         	
c ________________________________________________________
c	Dimensions
c
      include 'common.inc'
      character cwhy*48, cdestyp*12, csrctyp*12, cplntyp*12, 
     1  rec12*12, ccarry*3, cpuse*3, cresid1*12, cRiver*12,
     1  cTandC*3, cReplace*3, cDest*12, cSour*12,
     1  corid1*12, cCallBy*12, cImcdR*12
c _________________________________________________________
c               
c               Step 1; Initilize
c
c ---------------------------------------------------------
c		a. Miscellaneous
c		            iout=1  details
c		            iout=2  summary output
c		            iout=3  details on nRiver Adjustments
c               iout=4  details on iteration limitation
c               iout=5  details on divo(l2)
c		            iout=99 summary independent of corid
c               ioutQ = 1 Print qdiv(38
c                       2 Print qdiv(38 plus qdiv array
c
c rrb 2017/10/20 Addition
c               ioutL = 0 Do not print loss data when iout=2
c                       1 Print loss data when iout=2
c
c               ioutIR  details on instream flow reach
c
c               iout26= 1 details on reporting when the original 
c                       source was a type 26 operating rule 
c               iout26= 2 details on limiting the capacity of the 
c                       original source
c   
      iout=0
      ioutiw=0
      ioutIR=0
      
      iout26=0
      ioutQ=0
      ioutL=0
      
      lopr5=0
      lopr6=0

      loprR=0
      noprS=0
      corid1=corid(l2)
      
      monout=11   
            
      if(iout.eq.1) write(nlog,*) '  DivResP2'
            
      if(ichk.eq.127) iout=2
      if(irep.eq.0) then
        if(corid(l2).eq. ccall) ioutiw=iw
      else
        if(corid(l2).eq. ccall) ioutiw=iw
      endif 
c       
      if(iout.eq.1) then    
        write(Nlog,*)
     1    ' DivResP2; ncallx    ichk    iout  ioutiw      iw',
     1    ' corid        ccall' 
        write(nlog,'(10x,5i8,2(1x,a12))')  
     1    ncallx, ichk, iout, ioutiw, iw, corid(l2), ccall
      endif
c      
      if(iout.eq.2 .and. ioutiw.eq.iw) then
        if(ncallX.eq.0 .and. iday.eq.0) then
          write(nlog,102) corid(l2)
 102      format(/, 72('_'),/ '  DivResP2; ID = ', a12)
        endif
      endif
c
c rrb 2015/09/25; Control detailed output to one operating rule
c                 ioutQ = 1 Print qdiv(38
c                         2 Print qdiv(38 plus qdiv array
c
      if(iout.eq.2 .and. ioutiw.eq.iw) then 
        ioutQ=1
c       ioutQ=2
c
c rrb 2017/10/20; Add ioutL
        ioutL=0
      else
        ioutQ=0
      endif 
      
c
c ---------------------------------------------------------
      
      small=0.001
      smallN=-1.*small
      Round=0.1
      big=99999.
      oprPct1=1.0
      
      ishort = 0
c
c rrb 2017/10/20; Move to top
      iResT1=0
      
      iw=iw
      iowna=0
      ioff=0
      nd=0
c
c rrb; 2008/01/03; Return to river      
      nRiver=0
      imcdR=0
      cRiver='NA'
      cImcdR='NA'
      cCallBy='DivResP2    '
      
c      
c ---------------------------------------------------------
c
c               b. Daily capability
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif
c
c ---------------------------------------------------------
c               c. Detailed Output
c		   Note critical divact and relact are 
c                  initilized to zero
      iwhy=0
c
c rrb 2007/10/26; For ishort calculations and correct reoperation
c                 it is critical divact be < divalo when making 
c                 a quick exit
c     divact = 0.0          
      divact = -1.0/fac      
      
      divalo = 0.0
      relact=0.0
      cursa=0.0
      
      divactx=0.0
      divacty=0.0      
      
      divcarry=-1.0/fac
      alocfs=-1.0/fac
      alocfs1=-1.0/fac
      alocfs2=-1.0/fac
      alocfs3=-1.0/fac
      alocfsR=-1.0/fac
      
      divreq0=-1.0/fac
      divreq1=-1.0/fac
      divalox=-1.0/fac      
      pavail=-1.0/fac
      pstoX=-1/fac
      relact2=-1/fac
      
      qdiv30 = -1./fac
      qdiv31a=-1./fac
      qdiv31b=-1./fac
      qdiv35a=-1./fac
      qdiv35b=-1./fac
      qdiv36=-1./fac       
      qdiv36a=-1/fac
      qdiv36b=-1/fac
      
      OprmaxM1=-1.0
      OprmaxM2=-1.0
c
c rrb 2017/10/20; Addition
      divaf=0.0
      divafl=0.0

      iuseX=-1
      nd=-1
      
      ndP=-1
      ipUse=-1

      nsP=-1
      nsR=-1

      iscd=-1
      idcd=-1
      idcdX=-1
      idcdD=0
      idcdR=0
      idcdP=0
      idcdC=0
      idcdI=0      
      
      cwhy='NA'
c
c ---------------------------------------------------------
c		d. Destination
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
c
c rrb 2008/12/10; Correction need iuse before it was originally defined
        IUSE=NDUSER(ND)+IOPDES(2,l2)-1    
        diveff1=diveff(mon,iuse)/100.          
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
        cSour=pid(nsP)
      endif  
c
c ---------------------------------------------------------
c		f. Standard Carrier      
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
c		    ipUse = Reuse plan    
      cpuse='No'
      cplntyp='NA '
c
c rrb 2015/02/03X; Set ipUse when the source is a reservoir
      if(nsR.gt.0) then
        ipUse=ireuse(l2)
        if(ipUse.gt.0) then
          cpuse='Yes'
          if(iplntyp(ipUse).ne.9)  cplntyp='Reuse_Plan'
          if(iplnTyp(ipuse).eq.9)  cPlnTyp='OOP_Plan'
          if(iplnTyp(ipuse).eq.11) cPlnTyp='Acct_Plan'
        endif  
      endif 
c
c rrb 2015/02/03X; Set ipuse when the source is a Reuse Plan
c smalers 2017-11-06 split If statement to ensure that valid index value is used
c     if(nsP.gt.0 .and. iplntyp(nsP).ne.11) then
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
c      
c ________________________________________________________
c               h. T&C Plan
c	   	ipTC  = T&C plan
      cTandC='No'
      ipTC=iopsou(3,l2)
      if(ipTC.gt.0) cTandC='Yes'
c
c ---------------------------------------------------------
c		            i. Set associated operating rule
c		   for carrier & capacity adjustments  
c rrb 2015/02/03X; Done below based on the value of ioprlim       
cx     lopr5=iopsou(5,l2)    
c
c ---------------------------------------------------------
c		            j. CU Factor for T&C Obligation calculations
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
c rrb 2005/11/27; Add CuFac
      CuFac=OprEff(mon,l2)/100.        
c
c ---------------------------------------------------------
c		            i. Called by Replace
      if(irep.eq.0) then
        cReplace='No'
      else
        cReplace='Yes'
      endif        
c
c ---------------------------------------------------------
c rrb 2000/12/26; j. Variable efficiency consideration
      ieff2=1     
      icx=27
c
c ---------------------------------------------------------
c rrb 2007/12/04; k. Set Transit and Carrier Loss Data      
c		                Oprloss is transit loss (no returns
c		                ioprLoss > 0 transit loss
c		      	               = 0 no transit loss
c                   OprEffT  = Total Carrier Efficiency 
      call SetLoss(nlog, icx, l2, fac, 
     1 maxopr,  intern,   Oprloss, OprLossC,
     1 ioprloss, nCarry,  nRiver,  ncnum,    
     1 OprLost,  OprEff1, OprEffT, TranLoss, 
     1 internT,internL,corid(l2))
c
c rrb 2008/01/03; Add release to river capability
      if(nRiver.gt.0) cRiver=cstaid(nRiver)
c
c ---------------------------------------------------------
c               l. Check Avail array coming in
      if(iout.eq.1) then
        write(nlog,*) ' '
        write(nlog,*) ' ______________________________________'
        write(nlog,*) ' DivresP2; Calling Chekava In ', corid1
      endif  
      call chekava(17, maxsta, numsta, avail)
c
c ---------------------------------------------------------
c rrb 2008/01/08; m. Set Avtemp = Avail for Return to river
      do is=1,numsta
        avtemp(is)=avail(is)
      end do
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
        write(nlog,*) ' DivResP2_1; l2, nsP, ireuse(l2)'   
        write(nlog,*) ' DivResP2_1;', l2, nsP, ireuse(l2)         
      endif
c     
c                                      
c rrb 2015/02/03X; Revise to use Creuse 
cx      if(ioprlim(l2).eq.5 .and. iopsou(5,l2).gt.0) then
cx      lopr26=iopsou(5,l2)
c
c rrb 2015/03/07; Revise to use Oprlimit 1-9
cx    if(nsP.gt.0 .and. iplntyp(nsP).eq.11) then
cx      lopr26=ireuse(l2)    
c smalers 2017-11-06 split If statement to ensure that valid index value is used
c     if(nsP.gt.0 .and. iplntyp(nsP).eq.13) then
      if(nsP.gt.0) then
        if(iplntyp(nsP).eq.13) then
          lopr26=iopsou(7,l2)
          lr26=iopsou(1,lopr26)
          nd26=idivco(1,lr26)
          iscd26=IDVSTA(nd26)          
          ndns26=NDNNOD(iscd26)    
c
c rrb 2015/01/24; Revised reporting approach.
c                 Set iscd1 the node containing the plan
c                 the original source water right diverted to
          nsp1=iopdes(1,lopr26)
          iscd1=ipsta(nsp1)   
          iok=0
          if(lopr26.eq.0 .or. lr26.eq.0  .or. nd26.eq.0. or.
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
     1    ' DivResP2_2;   lopr26     lr26     nd26   iscd26',
     1                '   ndns26   iscd1   ndns26'   
        write(nlog,'(a12,8i8)')
     1    '  DivResP2_2;', lopr26, lr26, nd26, iscd26, ndns26, iscd1, ndns26
c      
        if(iok.eq.1) then
          write(nlog,*) ' Problem with source water right reporting'
          goto 9999   
        endif   
      endif
c        
c
c _________________________________________________________
c               Step 2; Exit if not on this month
c
      if(imonsw(l2,mon).eq.0) then
        iwhy=1
        cwhy='Monthly Switch = zero'
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
c               Step 3; Set Source Data
c        
c ---------------------------------------------------------
c               a. Source plan data
      if(nsP.gt.0) then
      
        nsr=0
        if(ifix(pon(nsp)).eq.0) ioff=1
c       write(nlog,*) ' DivResP2; nsp = ', nsp
      
        Iscd=ipsta(nsp)
        ndns=NDNNOD(IsCD)
c        
c ---------------------------------------------------------
c
c rrb 2008/07/03; Add % ownership
c		  Note ALOCFS=amax1(psuplyT(nsp)*OprPct1 - divo(l2))
c		  insures the % is calculated on the total amount
c		  to the plan (psuplyT), less previously diverted
c		  (divo(l2)).
cx      ALOCFS=psuply(nsp)
        OprPct1=oprPct(l2)/100.0         
        ALOCFS=amax1(0.0, psuplyT(nsp)*OprPct1 - divo(l2))
c
c rrb 2008/08/15; Correction to limit by active plan storage (psuply)        
        ALOCFS=amin1(ALOCFS, psuply(nsp))
        if(iout.eq.1) then
          write(nlog,*) 
     1     ' DivResP2; nsp, psuplyT, psuply, OprPct1, divo, alocfs'
          write(nlog,*) '         ',
     1      nsp, psuplyT(nsp)*fac, psuply(nsp)*fac, OprPct1, 
     1      divo(l2)*fac, alocfs*fac
        endif
        
        alocfs = amax1(0.0,alocfs)
        alocfs1= alocfs
        
        if(alocfs.lt.small) then
          iwhy=2
          cwhy='Source Plan (alocfs1) = zero'
          goto 300
        endif
      endif
c
c ---------------------------------------------------------
c               b. Source reservoir data
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
          cwhy='Source Reservoir (curown or alocfs1) = zero'          
          goto 300
        endif
c
c ---------------------------------------------------------
c rrb 2006/11/12; 
c		Limit to maximum river discharge (flowmax)
                
        ALOCFS=AMIN1(FLOMAX(nsr)-RIVER(iscd),ALOCFS)
        alocfs = amax1(0.0,alocfs)
        alocfs2=alocfs
c
c ---------------------------------------------------------
c		Exit of zero
        if(alocfs.lt.small) then
          iwhy=4
          cwhy='Maximum Available Streamflow (alocfs2) = zero'  
cx          write(nlog,*) '  DivResp2;',
cx     1     ' nsr, iscd, Flomax(nsr), river(iscd)'
cx          write(nlog,*) '  DivResp2;',
cx     1       nsr, iscd, Flomax(nsr), river(iscd), alocfs
                  
          goto 300
        endif
      endif

c
c _________________________________________________________
c               
c     Step 4a; Limit release (alocfs) to monthly and annual limits
c rrb 2015/02/03X; Allow type 4 that is a type 2 + type 3
c rrb 2015/03/07; Allow Oprlimit to be 1-9 
cx    if(ioprlim(l2).eq.2 .and. iopsou(5,l2).gt.0) then	
      if(ioprlim(l2).eq.2 .or. ioprlim(l2).eq.4 .or.
     1   ioprlim(l2).eq.7 .or. ioprlim(l2).eq.9) then      
        if(iopsou(5,l2).gt.0) then
          lopr5=iopsou(5,l2)                  
          oprmaxM1=amin1(oprmaxM(lopr5), oprmaxA(lopr5))
c         
c rrb 2015/03/07; Revise to limit to diversions in prior iterations
c  
c rrb 2015/03/23; Remove since this is taken care of in SetLimit        
cx        oprmaxM1=amax1(0.0, oprmaxM1-divo(l2)*fac)    
          ALOCFS=AMIN1(ALOCFS, oprmaxM1/fac)
          alocfs3=alocfs
c          
          if(iout.eq.1) then 
            write(nlog,*) ' DivResP2; ',
     1        'lopr5, oprmaxM(),oprmaxA() divo(l2)'
            write(nlog,*) ' DivResP2;',
     1         lopr5, oprmaxM(lopr5), oprmaxA(lopr5), divo(l2)*fac
          endif
          
          if(alocfs.lt.small) then
            iwhy=5
            cwhy='Mon or Ann Limit (OprMaxM1 or alocfs3) = zero'          
            goto 300
          endif   
        endif
      endif  
      
c
c _________________________________________________________
c               
c rrb 2009/01/153;
c     Step 4b; Limit to the amount diverted by another
c	    operating rule
c		
cx    write(nlog,*) ' DivResP2; ',ioprlim(l2)
c		                                                       
c rrb 2015/02/03X; Allow type 4 that is a type 2 + type 3 
c rrb 2015/03/07; Allow Oprlimit to be 1-9  
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
c          
          alocfs2=alocfs
          ALOCFS=AMIN1(ALOCFS, oprmaxM1/fac)
          alocfs3=alocfs
          
          if(iout.eq.1) then
            write(nlog,*) 
     1      ' DivResP2;',ioprlim(l2), lopr6, oprmaxM1, 
     1        divo(lopr6)*fac, alocfs2*fac, alocfs3*fac,
     1        divo(lopr6), fac
          endif
          
          if(alocfs.lt.small) then
            iwhy=6
            cwhy='Operating Rule Limit (OprMaxM1 or alocfs3) = zero'          
            goto 300
          endif 
        endif
      endif      
c
c _________________________________________________________
c               
c rrb 2014/11/24;
c     Step 4c; Limit to the capacity in the original source
c		
      if(lopr26.gt.0) then
        divcap1 = amax1(0.0, divcap(nd26) - divmon(nd26))     
        alocfs4=alocfs
        ALOCFS=AMIN1(ALOCFS, divcap1)
        alocfs5=alocfs
        
        if(iout26.eq.1) then
          write(nlog,*) ' '
          write(nlog,*) 
     1    ' DivResP2_3;  lopr26  alocfs4 divcap1 alocfs5'
          write(nlog,*) 
     1    ' DivResP2_3;',lopr26, alocfs4*fac, divcap1*fac, alocfs5*fac 
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
c		Set supply based on diversion if iDep=0 or 
c               depletion if iDep=1
      alocfsX=alocfs
      if(iDep.eq.0) then
        alocfs=alocfs
      else
cx      alocfs=alocfs/(diveff(mon,iuse)/100.0)
        alocfs=alocfs/diveff1
      endif
        
c        write(nlog,*) ' DivResP2; ',
c     1    '      nd    iuse     mon alocfsX  alocfs  diveff'
c        write(nlog,'(12x, 3i8, 20f8.0)') 
c     1   nd,,mon, alocfsX*fac,alocfs*fac, diveff1
c
c _________________________________________________________
c               
c     Step 5; Destination (Demand = Divalo)
      nd=iopdes(1,l2)
c
c ---------------------------------------------------------
c		a. Destination is a diversion
c
      if(ndtype.eq.3) then
        ndD=nd
        ndR=0
        ndP=0
        ndI=0
        iresw=0
        
        cDest=cdivid(nd)
        if(idivsw(nd).eq.0) ioff=2
        
        IUSE=NDUSER(ND)+IOPDES(2,l2)-1
        iuseX=iuse
        
        IDCD=IDVSTA(ND)
        idcdD=idcd
        ndnd=NDNNOD(idcd)
        
        idcdX=idcdD
        ndndX=NDNNOD(idcdX)
c
c ---------------------------------------------------------
c rrb 2007/12/04; Add Loss to Demand for a Diversion Destination        
cx      divreq1=divreq(iuse)
cx      DIVALO=AMIN1(DIVREQ(IUSE),DIVCAP(ND)-DIVMON(ND))
        divreq0=divreq(iuse)
        divreq1=divreq(iuse)/OprEffT
        DIVALO=AMIN1(divreq1,DIVCAP(ND)-DIVMON(ND))
c
c ---------------------------------------------------------
c rrb 2007/10/26; Add ability to be called by Replace
c                Demand for a diversion called by Replace
c                  (irep>0). Limit diversion to the remaining
c                  decree (dcrdivx-divdx)	
        if(irep.gt.0) then  
          dcrRem=dcrdivx-divdx
          divalo=amin1(DIVALO, dcrRem)
c
c		Exit if no remaining decree          
          if(dcrRem.lt.small) then
            iwhy=8
            cwhy='Remaining Decree (DcrRem) is 0'
            goto 300
          endif  
        endif 
c
c ---------------------------------------------------------
c		Exit if no demand        
        if(divreq(iuse).lt.small) then
          iwhy=9
          cwhy='Destination Demand (divreq1) is 0'
          goto 300
        endif 
c
c ---------------------------------------------------------
c		Exit if no capacity        
        if((DIVCAP(ND)-DIVMON(ND)).lt.small) then
          iwhy=10
          cwhy='Destination Capacity (divcap-divmon) is 0'
          goto 300
        endif 
c        
c                  If release type code is on
c                  Limit release to occur only if an IWR exists
c                  Note still releasing to meet 100% of demand
c
        if(iout.eq.1) then
          write(io99,*) ' '
          write(io99,*) ' DivResP2; nd, cDest ', nd, cDest
          write(io99,*) ' DivResP2; iopsou(6,l2)  ', iopsou(6,l2)
          write(io99,*) ' DivResP2; effmax(nd)    ', effmax(nd)
          write(io99,*) ' DivResP2; diwrreq(iuse) ', diwrreq(iuse)*fac
        endif
c
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
c
c ---------------------------------------------------------
c rrb 2007/12/04; Add Loss to Demand for a Diversion Destination 
          divalo=divalo/OprEffT          
        endif

        DIVALO=amax1(0.0,divalo)
        divaloX=divalo           
      endif  
c
c ---------------------------------------------------------
c               
c       b. Destination is a reservoir (Demand = Divalo)
      if(ndtype.eq.2) then      
c
c rrb 2007/10/26; Rely on varaible ndtype      
c       ndR=-nd
        ndR=nd
        ndD=0
        ndP=0
        ndI=0
        iresw=1
        cDest=cresid(ndR)
        
        if(iressw(ndR).eq.0) ioff=3
c
        idcd=irssta(ndR)
        idcdR=idcd
        ndnd=NDNNOD(idcd)        
        
        idcdX=idcdR
        ndndX=ndnnod(idcdX)
c
c ---------------------------------------------------------
c rrb 2006/11/29; Allow multiple destination accounts
        nro=1
        if(iopdes(2,l2).lt.0) then
          nro=-iopdes(2,l2)
          irow=nowner(ndR)
c
c rrb 2017/10/20; Correction
          iResT1=0
        endif

        if(iopdes(2,l2).gt.0) then
          irow=nowner(ndR)+iopdes(2,l2)-1
          nro=1
c
c rrb 2017/10/20; Correction
          iResT1=-1
        endif
  
        iuseX=irow

        cursa=0.0
        do n=1, nro
          n1=irow+n-1
          cursa=cursa+(ownmax(n1)-curown(n1))
        end do         
  
        if(cursa/fac.lt.small) then
          iwhy=11
          cwhy='Available Storage in accounts (cursa) is 0'
          goto 300
        endif 
c
c ---------------------------------------------------------
c       Set Reservoir demand
        divreq1=amin1(cursa,
     1                volmax(ndR)-cursto(ndR),
     1                tarmax(ndR)-cursto(ndR))/fac
        
        divreq1=amax1(0.0, divreq1)        
c
c ---------------------------------------------------------
c rrb 2007/12/04; Add Loss to Demand for a Reservoir Destination                
c       DIVALO=divreq1
        divreq0=divreq1
        DIVALO=divreq1/OprEffT
        divreq1=divalo
 
        divaloX=divalo
c        
        if((volmax(ndR)-cursto(ndR))/fac.lt.small) then
          iwhy=12
          cwhy='Available Storage in reservoir (volmax-cursto) is 0'
          goto 300
        endif 
        
        if((tarmax(ndR)-cursto(ndR))/fac.lt.small) then
          iwhy=13
          cwhy='Available Target in reservoir (tarmax-cursto) is 0'
          goto 300
        endif 
        
c
c               Check reservoir roundoff when entering routine
        in=0
        isub=19
        call chekres(nlog, maxres, in, isub, iyr, mon, ndR,nowner,
     1               curown,cursto,cresid)
        
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
        iresw=-1
        cDest=pid(ndP)
        if(ifix(pon(ndP)).eq.0) ioff=2
        
        IUSE=1
        iuseX=iuse
        
        IDCD=Ipsta(NDP)
        idcdP=idcd
        ndnd=NDNNOD(idcd)
        
        idcdX=idcdP        
        ndndX=NDNNOD(idcdX)
c
c ---------------------------------------------------------
c rrb 2007/12/04; Add Loss to Demand for a Plan Destination                
c       divreq1=big/fac     
c rrb 2008/04/23; Limit the transfer for a T&C or Aug plan 
c	    Types 1 or 2
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
c	    d. Destination is an Instream Flow (Demand = Divalo)
c
c rrb 2008/03/21; Allow an ISF Destination
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
       
cx      write(nlog,*) '  DivResP2;', ndI, flowrq(ndI)*fac  
      endif  

c
c _____________________________________________________________
c
c     Step 6; Destination is through a carrier
c		          Adjust diversion location idcd but
c		          not actual diversion location idcdX
c
c rrb 2015/01/20; Initilize ncar for capacity adjustment
      ncar=0
      if(intern(l2,1).gt.0) then
        ncar=intern(l2,1)
        idcd=IDVSTA(ncar)
        ndnd=NDNNOD(idcd)        
        
        idcdC=idcd
        ndndC=NDNNOD(idcdC)
c       idcdX=IDVSTA(ncar)
c       ndndX=NDNNOD(idcdX)
      endif
c
c
c rrb 2017/10/20; Addition
cx    if(iout.eq.1) then
      if(iout.eq.1 .or. ioutL.eq.1) then
        write(nlog,*) ' DivResP2;  ',
     1   '    iscd   idcdD   idcdR   idcdC   idcdP   idcdX    idcd'
        write(nlog,'(12x, 20i8)')
     1   iscd, idcdD, idcdR, idcdC, idcdP, idcdX, idcd
      endif  
c
c _________________________________________________________
c
c	  Step 7. Exit if off
      if(ioff.gt.0) then
        iwhy=14
        write(Nlog,*) ' DivResP2; ioff = ', ioff
        cwhy='Plan or Div or Res is off'
        goto 300
      endif
c
c _________________________________________________________
c
c     Step 8. Exit if no demand
      IF(divalo.LE.small) then
        iwhy=15
        cwhy='Demand or Capacity (divalo)=0'
        goto 300
      endif  
c
c _____________________________________________________________
c
c
c rrb 2006/10/03; 
c     Step 9; Limit based on OOP storage
c rrb 2009/06/09; Correction
      if(ipUse.gt.0) then
        if(iplntyp(ipUse).eq.9) then
          ipsta1=ipsta(ipUse)
          pstoX=psto2(ipUse)/fac
          divalo=amin1(divalo, pstoX)
          
          if(pstoX.le.small) then
            iwhy=16
            cwhy='OOP Plan Demand (PstoX) = 0'
            goto 300
          endif
        endif  
      endif
c      
c_____________________________________________________________
c rrb 2007/12/04; 
c               Step 10; Process carrier limitations
c	              	ncarry is indicator at least 1 carrier
c	              	ncnum is the number of standard carriers
c	              	OprEff1 is the lost (oprlost(lw)
c	              	Divalo gets reduced by carrier capacity
c	              	DivCarry is the limitating carrier capacity
c	              	noprS is the structure id of the structure
c	              	that supplied water to the accounting
c	              	plan that already has a capacity 
c	                adjustment
      if(ncarry.gt.0) then
c
c rrb 2015/02/03X; Correction to specify source of lopr
cx      if(lopr.gt.0) then       
        if(lopr6.gt.0) then 
c
c rrb 2017/10/20; correction
cx        loprR=iopsou(1,lopr)
          loprR=iopsou(1,lopr6)
          noprS=idivco(1,loprR)        
        endif  
      
        call SetCarL(nlog, icx, l2, fac, 
     1    maxopr,  maxdiv, intern, OprLossC,
     1    ncarry,  ncnum,  noprS,  internT,
     1    OprEff1, DivCap, DivMon, DivCarry, Divalo)
        
        if(divcarry.lt.small) then
          iwhy=17
          cwhy='Carrier capacity (Divcarry) = 0 '
          goto 300
        endif    
      endif   
c
c _________________________________________________________
c
c           **  Step 11; Set diversion (DIVACT) and
c               release (RELACT) to be the minimum of
c               suppply (ALOCFS), demand (DIVALO))
c	              and carrier limitations (DivCarry)
      if(ncarry.eq.0) then
        divact=amin1(alocfs, divalo)
      else
        divact=amin1(alocfs, divalo, divcarry)
c       write(nlog,*) ' DivResP2 2; divcarry', divcarry*fac        
      endif
      divact=amax1(0.0,divact)
c
c
c _____________________________________________________________
c
c               Step 12 - Find minimum release potential
c                        in the river from the diversion
c                        node (idcd) downstream
c
c rrb 2008/12/10; Correction Add the following to check downstream
c		              if a depletion demand is being simulated	
      if(idep.eq.0) then
        pavail=divalo
      else
        IMCD=Iscd
        ISS=Iscd
        DO nx=1,NDNs
          if (iss.eq.iExPoint(l2)) goto 110
          IF(AVAIL(IMCD).GT.AVAIL(ISS)) IMCD=ISS
          ISS=IDNCOD(ISS)
        end do
 110    pavail=amax1(0.0,avail(imcd))
c
c		Limit exchange potential to that used in a previous
c		iteration 
        pavailX=pavail
        divoX=divo(l2)/diveff1        
        pavail=amax1(0.0, pavail-divoX)
c
c rrb 20090830        
        if(iout.eq.4) then
          write(nlog,*) 
     1      ' DivResp2; Limit exchange to a previous iteration'
          write(nlog,*) 'nd, iuse, diveff1*100.,', 
     1       'divo(l2)*fac, divoX*fac, pavailX*fac, pavail*fac'
          write(nlog,*) nd, iuse, diveff1*100., 
     1        divo(l2)*fac, divoX*fac, pavailX*fac, pavail*fac
        endif
        
        if(pavail.le.small) then
          divact=0.0
          iwhy=18
          cwhy='Available flow for a dep demand (pavail) = 0'
          goto 300
        endif        
      endif
c _________________________________________________________
c
c 		Step 12; Calculate diversion 
c
c ---------------------------------------------------------
c		a. Diversion based on total diversion
      if(iDep .eq. 0) then

        DIVACT=amin1(divalo,alocfs)
        divact=amax1(0.0,divact)
        divactx=divact
c
cr        
        relact=-divact

        if(iout.eq.1) then
          write(nlog,342) pavail*fac, divalo*fac, alocfs*fac, divact*fac
        endif
      
        if(divact.le.small) then
          iwhy=19
          cwhy='Demand (divalo) or Available Flow (alocfs) = 0'
          goto 300
        endif
      endif  
c
c ---------------------------------------------------------
c		b. Diversion based on "Depletion"

      if(iout.eq.1) write(nlog,*) '  DivResP2_12; iDEP ', idep
      if(iDep .eq. 1) then

        DIVACT=amin1(pavail,divalo,alocfs)
        divact=amax1(0.0,divact)
c
c               If Available flow < demand
c               set the release (RELACT) to the difference
c               (DIVACT - PAVAIL) or the depletion (divact*diveff)
c
c rrb 2008/12/10; Correction
cx      if(pavail .lt. divact) then
        if(pavail .le. divact+small) then
          relact=divact-pavail
          relact=amax1(0.0, relact)       
          relact=amax1(relact,divact*diveff1)
          relact=-relact
        else
c               If available flow >= demand (pavail>=divact)
c               set release to the depletion (divact*diveff)
          relact=-1.*(divact*diveff1)
        endif
c
c               If iout=1 print detailed results
        if(iout.eq.1) then
          c = divact*diveff1
          write(io99,390) 2, diveff1, divact*fac, pavail*fac,
     1      relact*fac, c*fac
        endif
      endif
c
c _________________________________________________________
c
c         Step 13; Calculate Diversion less carrier loss (DivactL)
c			 Note: OprEffT = Total Carrier Efficiency 
c            TranLoss is system loss (after delivery)
c
c  rrb 2010/10/15; For water balance report system (tranloss)
c                  at the destination
cx      DivactL=divact*OprEffT  
      DivactL=divact*OprEffT * (1.0-TranLoss) 
c
c rrb 2017/10/20; Additional output
cx      if(iout.eq.1 .or. iout.eq.3) then
cx        write(nlog,*) ' DivResP2_13; ',
cx     1  divact*fac, DivactL*fac, oprefft
cx      endif
    
      if(iout.eq.1 .or. iout.eq.3 .or. ioutL.eq.1) then
        write(nlog,*) ' DivResP2_13; ',
     1  divact*fac, DivactL*fac, oprefft, (1.0-TranLoss)
      endif
c
      if(divact.le. small) then
        iwhy=20
        cwhy='Available flow without River Return (alocfs)= zero'
        goto 300
      endif  
c
c
c _________________________________________________________
c
c               Step 14; Adjust for every diversion from the 
c		                     river and water dumped to the river 
c		                     by an agumentation station (if nriver>0).
c		                     Call RivRtn that will:
c		                     1. Adjust the diversion as necessary since
c		                        the return location may be upstream or 
c                           downstream of the carrier diversion
c		                     2. Add the river return to Avail
c		                     Note navail=1 allows avail to be adjusted

      if(iout.eq.1) write(nlog,*) '  DivResP2; nriver ', nriver
      if(nRiver.gt.0) then   
c
c rrb 2010/10/15; Update to allow operation with a depletion release
       if(idep.eq.0) then
          DepfacM=1.0
        else
          DepfacM=diveff1
        endif
        nAvail=1   
c
c rrb 2010/10/15; 
cx        relact1=relact
       if(iout.eq.1) write(nlog,*) ' DivResP2_14; RivRtn_1 ', 
     1   divact*fac, relact*fac              

        
        call RivRtn(
     1    icx, nriver, l2, ndtype, iscd, nd, iuse, idcd, idcdX, 
     1    fac, smallN, oprEffT, relact, adj, divact, divactL, 
     1    ncnum, nAvail, alocfsR, DepFacM, imcdR, corid1)
     
       if(iout.eq.1) write(nlog,*) ' DivResP2_14 Back from; RivRtn_1 ', 
     1   divact*fac, relact*fac                 
           
        if(imcdR.gt.0) cImcdR= cstaid(imcdR)
c       write(nlog,*) ' DivResP2_2; avail (190)', imcdR, avail(190)*fac
     
        if(divact.le. small) then
          iwhy=21
          cwhy='Available flow with Carrier (alocfsR) = zero'
          goto 300
        endif  
      endif
c
c _________________________________________________________
c          
c               Step 15 Add Plan or Reservoir release (RELACT) to Avail
      AVAILR=AVAIL(Iscd)
c     write(nlog,*) ' DivResp2;', iscd, avail(iscd)*fac, avail(1)*fac

      if(iout.eq.1) write(nlog,*) ' DivResP2_15; RivRtn_3 ', 
     1  divact*fac, relact*fac 
            
c     if(iout.eq.2)write(nlog,*) ' DivResp2; avail_1 ',avail(1684)*fac,
c    1  ndns, iscd
c
c rrb 2014/11/24; Add a plan release to the original source downstream
c
      if(iout26.eq.1) then
        write(nlog,*) ' '         
        write(nlog,*) ' DivResp2_4; lopr26, ndns26, iscd26'
        write(nlog,*) ' DivResp2_4; ', lopr26, ndns26, iscd26
      endif     
c      
      if(lopr26.eq.0) then
        call TAKOUT(maxsta,avail,river,avinp,qtribu,idncod,
     1    relact,ndns,iscd)
      else
        call TAKOUT(maxsta,avail,river,avinp,qtribu,idncod,
     1    relact,ndns26,iscd26)     
      endif 
      
      if(iout.eq.1) then
        nchkA=90
        call chkAvail(nlog, icx, nchkA, maxsta, 
     1    AVAIL, relact, iscd, iscd, idcd, fac)
      endif         

     
      avail(iscd)=availR     
c      write(nlog,*) ' DivResp2;', iscd, avail(iscd)*fac, avail(1)*fac,
c     1  relact*fac
c
c _________________________________________________________
c          
c               Step 16 Remove destination from avail if
c			            not done in Rivrtn (nriver>0).
c         	      nCarry = 0 No carrier
c		                       1 No return to River, Final Destination
c			                       is from a carrier
c
c rrb 2011/08/04; Update to allow an Instream flow reach
      if(ndtype.eq.2 .or. ndtype.eq.3) then
 
        if(nriver.eq.0) then  
          if(ncarry.eq.0) then
            CALL TAKOUT(maxsta,AVAIL,RIVER,AVINP,QTRIBU,IDNCOD,
     1                  divact,ndndX,idcdX)        
          else   
            CALL TAKOUT(maxsta,AVAIL,RIVER,AVINP,QTRIBU,IDNCOD,
     1                 divact,ndndC,idcdC)
          endif
          
c      if(iout.eq.2)write(nlog,*) ' DivResp2; avail_3 ',avail(1684)*fac,
c     1  ndndX, idcdX

        endif
      endif
      
c      write(nlog,*) ' DivResp2;', iscd, avail(iscd)*fac, avail(1)*fac,
c     1  divactL*fac
c
c _________________________________________________________
c rrb 2011/08/04; Update to allow a instream flow point or reach
c		            Step 17; Update to allow an Instream flow reach  
c                        Note adjust avail but not river 
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
c _________________________________________________________
c
c		            Step 18; Add in return flows for a diversion     
c rrb 2007/12/04; Add Loss
c rrb 2015/03/02X Correction
cx    if(ndtype.eq.3 .and. ipuse.eq.0) then
      if(ndtype.eq.3 .and. ipuse.le.0) then       
c
c rrb 2008/09/10; Correction on return location       
cx      call rtnsec(icx,divactL,l2,iuse,IDCD,nd,ieff2)
        call rtnsec(icx,divactL,l2,iuse,IdcdX,nd,ieff2)
     
     
          if(iout.eq.1) then
          write(nlog,*) ' DivResP2 call RtnSec;',
     1     'iuse, idcdX, nd, ieff2, divactL'
          write(nlog,*) ' DivResP2 ', 
     1      iuse, idcdX, nd, ieff2, divactL*fac
     
          nchkA=91
          cx=-1.0/fac
          call chkAvail(nlog, icx, nchkA, maxsta, 
     1      AVAIL, divact, iscd, iscd, idcd, fac)
          endif           
      endif  
c     write(nlog,*) ' DivresP2_4; idcd, Avinp ', idcd, avinp(7)*fac          
c
c _________________________________________________________
c
c		            Step 19; Calculate Reuse

      if(ipUse.gt.0) then
c      
c ---------------------------------------------------------
c		a. Associated Plan type is Reuse (add)
     
        if(iplntyp(ipUse).ne.9) then
          if(ndtype.eq.3) then
c
c rrb 2007/12/04; Add Loss
c           CALL RtnsecR(icx,divact,l2,iuse,idcd,nd,
c    1         ieff2,ipUse)
            CALL RtnsecR(icx,divactL,l2,iuse,idcd,nd,
     1         ieff2,ipUse)
          endif
c
c rrb 2007/08/17; Allow a Plan destination      
          if(ndtype.eq.2) then
c         
c	               	Reservoir Reuse          
            psuply(ipUse)=psuply(ipUse)+divactL
            psuplyT(ipUse)=psuplyT(ipUse)+divactL
            psto2(ipUse)=psto2(ipUse)+divactL*fac          
            ipsta1=ipsta(ipUse)
          endif  
        endif  
c      
c ---------------------------------------------------------
c		b. Plan type is an OOP (subtract)
c		   Note source is always a reservoir     
        if(iplntyp(ipUse).eq.9) then
c
c rrb 2007/12/04; Add Loss
c         psto2(ipUse)=amax1(0.0, psto2(ipUse)-divact*fac)          
          psto2(ipUse)=amax1(0.0, psto2(ipUse)-divactL*fac)          
          ipsta1=ipsta(ipUse)
          
          if(psto2(ipUse).le. smallN) then
            write(nlog,*) '  DivResP2; psto2(ipUse)', psto2(ipUse)
            goto 9999
          endif  
          
          if(abs(psto2(ipUse)).le. small) psto2(ipUse)=0.0
        endif  
      endif
c
c _________________________________________________________
c
c               Step 20; Update destination
c
c ---------------------------------------------------------
c		a. Destination is a diversion
c
c rrb 2007/08/17; Allow a Plan destination      
c     if(nd.gt.0) then
      if(ndtype.eq.3) then
        USEMON(IUSE)=USEMON(IUSE)+DIVACT
c
c rrb 2007/12/04; Add Loss
c       DIVREQ(IUSE)=DIVREQ(IUSE)-DIVACT
        DIVREQ(IUSE)=DIVREQ(IUSE)-divactL
        DIVMON(ND  )=DIVMON(ND  )+DIVACT
      endif  
c      
c ---------------------------------------------------------
c		b. Destination is a reservoir
c
c rrb 2007/08/17; Allow a Plan destination      
c     if(nd.lt.0) then
      if(ndtype.eq.2) then
        divaf=divact*fac
c
c  rrb 2010/10/15; For water balance report system (tranloss)
c                  at the destination
cx      divafL=divaf*OprEffT        
        divafL=divaf*OprEffT * (1.0-TranLoss) 
c
c rrb 2010/10/20; Additional output
        cursto1=cursto(ndr)
        cursto(ndR)=cursto(ndR)+divafL
c
c ---------------------------------------------------------
c rrb 2006/09/25; Allow multiple accounts - Allocate
c		  Note:
c		   iResT1=0 distributes based on ownership ratio
c		   nrown1=number of accounts in this reservoir
c		   iown = first account associated with this reservoir  
c		   icx  = subroutine calling accou.for       
c		   ia   = account to adjust
      
        nrX=ndR
c
c rrb 2017/10/20; Move to top
cx      iResT1=0
        nrown1=nro
        iownX=irow
        icx2=icx+100
        if(ncarry.eq.0) ia=18
        if(ncarry.ne.0) ia=4
        cresid1=cresid(nrX)
c
c ---------------------------------------------------------
c rrb 2007/12/04; Add Loss
c                 Note the following allocates diversion after
c                 loss to the total reservoir and each account
c       call accou(maxacc, maxown, nrX, ownmon, curown, accr, ia, 
c    1    ownmax, iownX, nrown1, cursa, divaf, iResT1,icx, cresid1)
        call accou(maxacc, maxown, nrX, ownmon, curown, accr, ia, 
     1    ownmax, iownX, nrown1, cursa, divafL, iResT1,icx2, cresid1)
c
c ---------------------------------------------------------
c		  Set Reservoir Source Data
c
c rrb 2010/10/15; Adjust to add loss in the source
c                 supply (qres(30 and qres(27)
cx
cx rrb 2017/10/20; Simplify
      ResLoss=divafL*TranLoss
      ResLoss=amax1(0.0, divaf-divafL)
cx
cx
cx rrb 2017/10/20; Detailed output
        if(ioutL.eq.1) then
          write(nlog,*) ' '
          write(nlog,*) '  Divresp2; ndr, cursto1',ndr, cursto1
          write(nlog,*) '  Divresp2; ndr, cursto(ndr)',ndr, cursto(ndr)
          write(nlog,*) '  Divresp2; change ',cursto(ndr)-cursto1
          
          write(nlog,*) ' '
          write(nlog,*) '  Divresp2; divaf, divafl, tranloss, resloss'
          write(nlog,*) '  Divresp2;', divaf, divafl, tranloss, resloss
        endif
        
c
c rrb 2017/10/20; Detailed Output
        if(ioutL.eq.1) then
          write(nlog,*)' DivResP2; irow, accr(4,irow)',
     1      irow, accr(4,irow)
        endif
c
c rrb 2017/10/20; Correction
cx        Note accr(4 gets set in accou.f but accr(30 (loss) does not
cx        Note qres(4 and qres(30 do not get set in accou.f
cx
cx        qres(4  From Carrier by Storage or Other to reservoir
cx        qres(18 From River by Exchange to Reservoir
cx        qres(27 From Carrier Loss
cx        qres(30 From River Loss
cx    
cx        if(ncarry.eq.0) then
cx          qres(18,ndR)=qres(18,ndR)+divafL+ResLoss
cx          qres(30,ndR)=qres(30,ndR)+ResLoss
cx        else
cx          qres(4,ndR)=qres(4,ndR)+divafL+ResLoss
cx          qres(27,ndR)=qres(27,ndR)+ResLoss
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
c rrb 2017/10/20; Detailed Output
        if(ioutL.eq.1) then
          write(nlog,*)' DivResP2; accr(4,irow)', accr(4,irow)
        endif
c
c               Check reservoir roundoff when exiting routine
        in=1
        isub=19
        call chekres(nlog, maxres, in, isub, iyr, mon, ndR,nowner,
     1               curown,cursto,cresid)
      endif
      
      
c      
c ---------------------------------------------------------
c		c. Destination is a Plan
c rrb 2007/12/04; Add Loss
      if(ndtype.eq.7) then
        if(iout.eq.1) then
          write(nlog,*) ' DivResP2; ', ndp, divact*fac, DivactL*fac,
     1    oprefft 
        endif
        
        psuply(ndP)=amax1(0.0, psuply(ndP)+divactL)
        pdrive(ndP)=pdrive(ndP)+divactL
        psuplyT(ndP)=psuplyT(ndp)+divactL
 

c
c rrb 2008/01/15; If a T&C destination set qdiv(30 an
c		    adjustment to total diversion in outbal2
        if(iplntyp(ndP).eq.1) then
          qdiv(30,idcd)=qdiv(30,idcd)+DIVACT
          qdiv30=qdiv(30,idcd)
        endif 
c
c rrb 2011/02/28; Correction since Qdiv was rervised to
c                 only operates on diversions 
        if(nCarry.eq.0) then               
          qdiv(31,idcd)=qdiv(31,idcd)+Divact
        endif
      endif
c      
c ---------------------------------------------------------
c		d. Destination is an Instream Flow Point or Reach
c rrb 2008/08/04; ISF Destination
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
c		  adjustment to total diversion in outbal2
c rrb 2011/02/25; Correction 
cx      if(iplntyp(ndP).eq.1) then
        qdiv(30,idcd)=qdiv(30,idcd)+DIVACT
        qdiv30=qdiv(30,idcd)        
cx      endif    
      endif
c      
c _________________________________________________________
c
c               Step 20; Update Source 
c      
c ---------------------------------------------------------
c		a. Source is a Plan
      if(nsP.gt.0) then
        psuply(nsP)=amax1(0.0, psuply(nsP)+relact)
c        
        if(iplntyp(nsP).eq.3 .or. iplntyp(nsP).eq.5) then
          psto2(nsP)=amax1(psto2(nsP)+relact*fac, 0.0)                
        endif       
c 
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
          
          if(ioutQ.eq.1) then
            write(nlog,*) 
     1      ' DivRplP; nsp, iscd, cstaid, qdiv38' 
            write(nlog,*) 
     1      ' DivRplP;', nsp, iscd, cstaid(iscd), qdiv38*fac
          endif
        endif
c 
c rrb 2015/01/24; Report amount diverted as Carried, Bypassed or 
c                 Exchanged at the the source plan node (iscd),
c                 the original source plan node befor a split (iscd1)
c                 and, if not a carrier, the water right node (iscd26)
c rrb 2015/03/07; Revise to a Changed Water Right (type 13)
cx       if(iplntyp(nsP).eq.11 .and. lopr26.gt.0) then 
         if(iplntyp(nsP).eq.13 .and. lopr26.gt.0) then         
           qdiv(38,iscd) = qdiv(38,iscd) - relact  
           qdiv(38,iscd1)=qdiv(38,iscd1) - relact
           if(nd26.ne.ncar) then
             qdiv(38,iscd26)= qdiv(38,iscd26) - relact
           endif
         endif              
c                  
         if(iout26.eq.1) then
           write(nlog,*) ' '            
           write(nlog,*) 
     1       ' DivResp2_5;   lopr26    iscd   iscd1  relact qdiv(38'
           write(nlog,'(a13, 3i8, 20f8.0)') 
     1       '  DivResp2_5; ', lopr26, iscd,  iscd1, relact*fac, 
     1                   qdiv(38,iscd)*fac
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
c
c rrb 2015/07/18 Test
cx        if((iout.eq.1 .or. iout.eq.2) .and. iw.eq.ioutiw) then
cx          if(iyrmo(mon).eq. 1993) then
cx            write(nlog,*) '  DivResp2_1; ncarry, nsr, qres(11, qres(12'
cx            write(nlog,*) '  DivResp2_1;',
cx     1                       ncarry,nsr,qres(11,nsR),qres(12,nsr),divaf
cx          endif
cx        endif
        
        if(ncarry.eq.0) then
          qres(12,nsR)=qres(12,nsR)+divaf
          accr(12,isown)=accr(12,isown)+divaf 
        else        
          qres(11,nsR)=qres(11,nsR)+divaf
          accr(11,isown)=accr(11,isown)+divaf
        endif  
c
c rrb 2015/07/18 Test
cx        if((iout.eq.1 .or. iout.eq.2) .and. iw.eq.ioutiw) then  
cx        
cx          if(iyrmo(mon).eq. 1993) then
cx            write(nlog,*) '  DivResp2_2; ncarry, nsr, qres(11, qres(12'
cx            write(nlog,*) '  DivResp2_2;',
cx     1                       ncarry,nsr,qres(11,nsR),qres(12,nsr),divaf
cx          endif
cx        endif
        
c        
c               Check reservoir roundoff when exiting routine
        in=1
        isub=19
        call chekres(nlog, maxres, in, isub, iyr, mon, nsR,nowner,
     1               curown,cursto,cresid)
      endif  

c
c _________________________________________________________
c               
c               Step 21;  Update shortage switch (ishort)
c
c jhb 2014/07/13 move 300 down to step 28
c                some indices were unset (=-1) and cause array
c                bounds issues
c
c rrb 2015/08/23; Back to original
  300 if(ndtype.eq.3 .and. divact+small .lt. divalo) ishort=1  
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
c
c               Step 22; Update data to pass out and use
c                        in SetQdivC
c rrb 2007/10/26; Correction for calls by Replace
c     divactx=divact
c     divacty=divact 
      divactx=-relact       
      divacty=divact
c
c _________________________________________________________
c               
c               Step 23; Update operating rule output (DIVO)
c		Note relact is the water released (a negative value)
cx      divo(l2)=divo(l2)+divact
c
c rrb 2015/09/06; TEST to keep small numbers from accumulating
      if(divact.gt.small) then
        divo(l2)=divo(l2)-relact
      endif
c
c rrb 2015/09/06; Detailed output      
      if(iout.eq.5 .and. mon.eq.monout .and. l2.eq.18) then
        write(nlog,*) '  DivResP2; l2', l2, iyrmo(mon), corid1, 
     1    xmonam(mon), divact*fac, divo(l2)*fac
      endif
      
c
c _________________________________________________________
c rrb 2007/12/04
c               Step 24; Update Qdiv for Source and Destination

      EffmaxT1=(100.0-OprLossC(l2,1))/100.0
c
c rrb 2015/08/23; Correct problem when idcdX has not been set
      if(idcdX.gt.0) then
      
        qdiv31a=qdiv(31,idcdX)
        call setQdiv(nlog,  nCarry, nRiver,
     1    ndD, ndR, iscd,   idcdX, idcdC,
     1    divact, TranLoss, EffmaxT1, OprEffT, fac, 
     1    rloss, maxsta, maxdiv, maxqdiv, qdiv, icx,
     1    internL, corid(l2))
c
c rrb 2011/02/23; Correction; do not set destination data when
c                 it is a reservoir     
        qdiv31b=qdiv(31,idcdX)
        if(iout.eq.1) then
          write(nlog,*) '  DivResP2; Call SetQdiv qdiv31b', qdiv31b*fac
          call flush(nlog)
        endif  
      endif
c
c _________________________________________________________
c rrb 2007/12/04
c   Step 25; Update Qdiv(18, ), Qdiv(32 ,) and Qdiv(20, ) 
c		   for the carrier(s)
c		   Also calculate return flows for carrier losses
c		   using the structure properties of the carrier	
c
c smalers 2017-11-07 Steve added Jim's change
c jhb 2016/10/17 prevent the array error when iscd=-1
c                but why iscd=-1 has not been fixed
c     qdiv36a=qdiv(36, iscd)
      if(iscd.gt.0) then
        qdiv36a=qdiv(36, iscd)
      endif
c
      if(ncarry.gt.0) then      
        call setQdivC(
     1    nlog, ncarry, ncnum, nd, ndD, l2, iscd, idcdX, idcdC, 
c
c rrb 2010/10/15; Revise to allow a depletion diversion     
cx   1    nriver, divactX, TranLoss, EffmaxT1,  
     1    nriver, divact,  TranLoss, EffmaxT1,         
     1    fac, maxsta, maxdiv, maxqdiv, maxopr, 
     1    intern, idvsta, qdiv, divmon, 
     1    maxrtnPP, maxplan, OprEff1, ipuse,  
     1    pctlosPP, rlossP, oprLossc,internT, 
     1    icx, corid(l2))
      endif 
c
c rrb 2014/01/24; Check                                                                             
c rrb 2015/08/23; Correct problem when incar has not been set                
      if(ncar.gt.0) then                  
        divcap2=divcap(ncar) - divmon(ncar)
      endif 
cx      write(nlog,*) '  DivResP2_1 In; ncar, divcapX',ncar,divcapX*fac
c
c ---------------------------------------------------------
c rrb 2010/10/15; Revise to account for TranLoss (a system loss 
c                 not a carrier loss) at the destination
c                                                            
c rrb 2015/08/23; Correct problem when idcdX has not been set
      if(idcdX.gt.0) then
        if(Tranloss.gt.small) then
          if(ncarry.eq.0) then 
            qdiv(33,idcdX) = qdiv(33,idcdX) + divact*effmaxT1*Tranloss
          else
            qdiv(32,idcdX) = qdiv(32,idcdX) + divact*effmaxT1*TranLoss
          endif
        endif
      endif
c
c _________________________________________________________
c
c *************************************
c
c rrb 2014/11/24
c     Step 26a; Adjust the amount diverted (divmon) based on 
c               the amount released which is how
c               a diversion is limited by capacity 
c                                                            
c rrb 2015/08/23; Correct problem when nd26 has not been set
      if(nd26.gt.0) then            
        if(lopr26.gt.0) then
c	
c rrb 2015/01/20; Do not adjust if the source is the primary carrier	
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
     1       ' DivResP2_6;',
     1       '   lopr26  ncarry     nd26    ncar',
     1       '  divact divcap1 divcap2 divcap3'
            write(nlog,'(1x,a12,4i8, 20f8.0)')
     1       ' DivResP2_6;', lopr26, ncarry, nd26, ncar,
     1         divact*fac, divcap1*fac, divcap2*fac, divcap3*fac
          endif       
        endif
      endif  
c
c _________________________________________________________
c rrb 2007/07/03; 
c               Step 26b; Update monthly or annual plan limitations
      if(iout.eq.1) write(nlog,*) ' DivResP2; ', iopsou(5,l2)
c
c rrb 2009/01/15; Revise to recognize ioprlim(l2)=3      
cx    if(iOprLim(l2).gt.1 .and. iopsou(5,l2).gt.0) then
c rrb 2015/02/03; Revise to recognize ioprlim(l2)=4
c rrb 2015/03/07; Allow Oprlimit to be 1-9 
cx    if(iOprLim(l2).eq.1 .or. iOprlim(l2).eq.2) then
      if(iOprlim(l2).eq.2 .or. iOprlim(l2).eq.4 .or.
     1   iOprlim(l2).eq.7 .or. iOprlim(l2).eq.9) then      
c     
        if(iopsou(5,l2).gt.0) then
          lopr5=iopsou(5,l2)
          ipLim=iopsou(1,lopr5)
        
          call SetLimit(
     1      nlog, icx, lopr5, ipLim, ioprlim(l2), fac, 
     1      divact, OprmaxM(lopr5), OprMaxA(lopr5), 
     1      Oprmax(lopr5,mon), Oprmax(lopr5,13), OprmaxM1, OprmaxM2, 
     1      psto1(ipLim), psto2(ipLim), corid1)
     
        endif
      endif
      
c
c_____________________________________________________________
c rrb 2007/07/09; Allow T&C return flow obligations to be assinged 
c		  at the destination (herein)
c               Step 27; Calculate return flow obligation
c			           Note return patterns may be the default
c			           structure (iuse1) or from plan data
c			           see RtnsecP
c                iDep = 0 release to meet diversion demand

c
c	
      if(iout.eq.1) write(nlog,*) '  DivResP2; iptc, CuFac ',
     1   iptc, CuFac  	
c     write(nlog,*) '  DivResP2; iptc = ', iptc	
c
c
c ---------------------------------------------------------
c rrb 2007/11/27; Update T&C calculations
c
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
c _________________________________________________________
c
c               Step 28; Detailed output at Exit
c
c rrb 2009/05/12; Reduce output for a daily model
cx    if((iout.eq.1 .or. iout.eq.2) .and. iw.eq.ioutiw) then 
c
c rrb 2015/08/23; Back to original      
cx300  continue
      iprint=1
      if(iday.eq.1 .and. divact.lt.small) iprint=0
      
      if((iout.eq.1 .or. iout.eq.2) .and. iw.eq.ioutiw .and.
     1   iprint.eq.1) then 
c
c ---------------------------------------------------------
c		l. Detailed header      
        ncallX=ncallX+1
        if(ncallX.eq.1) then
          write(nlog,270) corid(l2), cdestyp, ccarry, cTandC, cpuse,
     1      cDivTyp(l2), Creplace, cRiver                    
        endif
        
c        write(nlog,*) ' DivResP2_2; ',lopr5, OprmaxM(), oprmaxA()        
c        write(nlog,*) ' DivResP2_2; ',lopr5,OprmaxM(lopr5),
c    1                 oprmaxA(lopr5)

cx        write(nlog,*) ' DivResP2; qdiv35a, qdiv35b, qdiv36 ipuse', 
cx     1    qdiv35a*fac, qdiv35b*fac, qdiv36*fac, ipuse
        
        write(nlog,280) '  DivResP2  ',
     1    iyrmo(mon),xmonam(mon),idy, cSour, cDest, cImcdR,
     1    iwx, iw, l2, nd, ndd, ndr, nsP,iuseX,iDep,nRiver,ncarry,
     1    alocfs1*fac, alocfs2*fac, alocfs*fac, alocfsR*fac, cursa, 
     1    divreq1*fac, OprEffT*100., DIVREQ0*fac, 
     1    Pavail*fac,  divaloX*fac, divcarry*fac,PstoX*fac,
     1    OprmaxM1,    OprmaxM2,      
     1    -1*relact*fac,divact*fac, iwhy, cwhy 
c     1    qdiv30*fac, qdiv31a*fac, qdiv31b*fac,
c     1    qdiv35a*fac, qdiv35b*fac, qdiv36*fac, qdiv36a*fac, qdiv36b*fac   
c        write(nlog,*) ' DivResP2; avail (190)', avail(190)
     
       endif
c
c _________________________________________________________
c               
c               Step 29; Check results
c
      if(iout.eq.1) then
        write(nlog,*) ' DivresP2; Calling Chekava Out ',  corid1
      endif  
      
      call chekava(17, maxsta, numsta, avail)
c _________________________________________________________
c
c               Step 30; Return

      RETURN
c
c _________________________________________________________
c               Formats
  270   format(/, 
     1   '  DivResP2 (Type 27); Operation Right ID = ', a12,
     1  ' Destination Type = ', a12,
     1  ' Carrier (Y/N) = ',a3, ' T&C Plan (Y/N) = ',a3,
     1  ' Reuse Plan (Y/N) = ', a3,
     1  ' Diversion Type = ', a12,' Called by Replace = ', a3,
     1  ' Release to River = ', a12,/
     1  '  Note: Divreq1 is demand, Divreq0 is demand with loss, ',    
     1    ' AloCfsR is allowable flow with carriers & returns',/
     1    '  DivResP2   Iyr  Imo   Idy',
     1    ' Source ID    Dest ID      Min ID        Iter    iw',
     1    '    l2    nd   ndD   ndR   nsP  iuse  iDep  nRiv ncary',
     1    ' Alocfs1 Alocfs2  Alocfs AlocfsR   Cursa DivReq1 OprEffT',
     1    ' DivReq0  Pavail',
     1    ' divaloX divCary   PstoX Oprmax1 Oprmax2  RELACT  DIVACT',
     1    '    iwhy cwhy',/
     1    ' ___________ ____ ____ ____',
     1    ' ____________ ____________ ____________ _____ _____'
     1    ' _____ _____ _____ _____ _____ _____ _____ _____ _____', 
     1    ' _______ _______ _______ _______ _______ _______ _______',
     1    ' _______ _______ _______ _______ _______ _______ _______',
     1    ' _______ _______',
     1    ' _______ ________________________')
 
 280   format(a12, i5, 1x,a4,i5, 3(1x,a12), 11i6, 16f8.0, i8, 1x,a48,
     1   20f8.0)
     
 290  format(/,
     1 '  RivRtn; Adjusting Avail',/
     1 '     i    l2  ncar nlast internT divact2',/
     1 ' _____ _____ _____ _____ _______ _______')
     
 292  format(5i6, f8.0)   
 
 342   format(
     1     '  DivresP2; Diversion Limit;',
     1     '  pavail  divalo  alocfs  divact',/
     1     '                          ', 20f8.2)
     
  360   format(/
     1  'DivResP2;  Divact  Rettot DivLeft  Divact   CuFac    IpTC',
     1           '     Imo    Pdem   PdemT',/
     1  '         ', 5f8.0, 2i8, 20f8.0)
 
  380  format(
     1       '  DivResP2; Problem with ', a12, 1x, a24,' Type = ', i5)

  390  format(
     1       '  DivResP2; Release for Depletion Data;',/
     1       '                  #',
     1       ' diveff1  divact  pavail  relact      CU',/
     1       '           ', i8, 20f8.2)
c
c               Error warnings
c _________________________________________________________
 9999 continue
      write(nlog,270) corid(l2), cdestyp, ccarry, cTandC, cpuse,
     1      cDivTyp(l2), Creplace, cRiver                    
 
      write(nlog,280) '  DivResP2_X',
     1  iyrmo(mon),xmonam(mon),idy, cSour, cDest, cImcdR,
     1  iwx, iw, l2, nd, ndd, ndr, nsP,iuseX,iDep, nRiver,
     1  alocfs1*fac, alocfs2*fac, alocfs*fac, alocfsR*fac, cursa, 
     1  divreq1*fac, oprEffT*100., DIVREQ0*fac, 
     1  Pavail*fac,  divaloX*fac, divcarry*fac,PstoX*fac,
     1  OprmaxM1,    OprmaxM2, 
     1  -1*relact*fac, divact*fac, 
     1  iwhy, cwhy
 
      write(6,1050) 
      write(99,1051) 
    
 1050 format('    Stopped in DivResP2',/,
     1       '    See the *.log file')
 1051 format('    Stopped in DivResP2')
      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)


      stop 
      
      
      
      END

