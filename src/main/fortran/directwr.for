c
c     Last change:  RRB  18 Dec 100    2:29 pm
c
      SUBROUTINE directwr(IW,L2,ISHORT,divactX,ncallX)
c _________________________________________________________
c	Program Description
c

c
c 	Type 26; Changed Water Right   
c		 1. Simulates diversion of all or part of  water 
c       right to be diverted at its original location and
c		 2. The balance of a water right to be diverted to a Plan
c
c      Approach: 
c      1. Initilize
c      2. Source is a water right (iopsou(1,l2)
c      3. Destination is a plan (iopdes(1,l2) 
c      4. Calculate the water right left at the source limited by 
c         % ownership of right, availability of water, capacity
c         and demand (STEP 7)
c      5. Calculate changed WR to the destination limited by 
c         % ownership of right, availability of water
c         and demand STEP 8-10.  Note diversion to changed WR
c         is not limited by the structure capacity; capacity 
c         adjustments occur when water is released by a type
c         27 or 28 rule.
c      6. Determine if more water can be diverted at source
c         (can occurr if the changed WR was limited for any reason)
C         STEP 16
c
c         Note
c         Only allowed to operate once per time step (see variable
c           icallOP 
c         Destination must be a type 13, Changed Water Right plan
c         Source must be a water right
c         The water right % remaining at the source and diverted 
c           has the souce capacity reduced.  The water right % 
c           that is changed and diverted to a plan does not 
c           reduce the source capacity.  Capacity limits are 
c           imposed when water is released from the plan by 
c           a direct release (type 27) or by exchange (type 28)
c		 
c _________________________________________________________
c	Update History
c
c rrb 2014-11-24;  Copied type 24 (DirectEx) and edited.
c                  Major differences are:
c                  Source must be a water right
c                  Destination must be a plan
c                  No carrier capabilities
c                  Water associated with the changed water right
c                  is diverted using the location of the source
c                  water right, not the destination (see idcd3
c                  and ndns3 in sep 8c and 12)
c _________________________________________________________
c
c               Documentation
c
c     IW : OVERALL WATER RIGHT ORDER
c     L2 : LOC. OF operation right  in opr RIGHT TABLE
c
c     lr             source water right
c     iopsou(1,l2)   source water right
c     iopsou(2,l2)   Not used (note % ownership gets transfered
c			               to oprpct in Oprinp.f)
c     iopsou(3,l2)   NA
c     iopsou(4,l2)   NA
c     oprpct(l2)     Percent of the water right to be changed WR
c	
c
c     iopdes(1,l2)   destination type
c                    if(ndtype.eq.2) cdestyp='Reservoir'
c                      if = iopdesr(l2) = 2 Reservoir
c                      if = iopdesr(l2) = 3 Diversion
c                      if = iopdesr(l2) = 7 Plan
c    
c     iopdes(2,l2)   destination account (reservoir only)
c    
c     cdivTyp        Diversion Limit transfer to 
c	 	                  Depletion limit transfer to CU
c     divmon(nd)	   Amount diverted by the source (nd) in previous
c	                   iterations (used to calculate available 
c			               capacity)
c
c     nd             source diversion ID
c     iscd           idvsta(l2) stream ID of source diversion (nd)
c     ndns           # of nodes downstream of source diversion (nd)
c     iuse           source user
c
c     nd2            destination ID code
c		
c     idcd2          Stream ID of destination plan
c     idcd2P         stream ID of destination plan
c     idcd2C         stream ID of 1'st destination carrier
c	    idcd2          Stream id of destination. May be a diversion,
c		                   reservoir, plan OR CARRIER
c     idcd2X         stream ID of destination diversion (nd2) or 
c                      reservoir or plan (NOT CARRIER)
c
c     ndns2          # of nodes downstream of destination 
c                    diversion plan
c     iuse2          destination user 
c
c     ipuse		       Reuse indicator 0=no, +=reuse plan
c
c     imcdX          pointer to avail array. 
c     imcdS          pointer to minimum flow below source
c
c     icx            subroutine call # (26) for I/O in rtnsec
c
c     ishort         code for reoperation; 0=no, 1=yes
c
c   	CuLimit        fraction to be diverted (diversion or depletion)
c
c     divactE        actual diversion at destination by changed WR
c     divact1        actual diversion at source 
c
c     divreqx        Diversion demand for types 1-3 (standard)
c
c     dcrdivS        % of Water right used at source (cfs) Set in 
C                      oprinp
c     divdS          Amount diverted at source in previous
c                    iterations (cfs)
c     dcrdiv1        Remaining water right at source (cfs)
c     dcrdivE        % of Water right used at changed WR (cfs) Set in 
C                      oprinp
c     dcrdiv2        Remaining water right at changed WR (cfs)
c
c     divcap         Structure capacity
c     divmon         Amount diverted in previous iterations
c
c     divAdd         Additional diversion by source structure 
c		                 when the changed WR is limited
c
c     idvsta(l2)     STATION WHERE DIV. RIGHT L2 IS LOCATED
c
c     idivco(1,l2)  = structure associated with water right l2
c
c     ieff2         =0 always use average efficiency
c                   =1 let ieffmax control variable efficiency 
c     ioprtn         Switch for handling return flows when
c                    multiple structures exist at 1 location
c                    currently always set to 1.0 in Datinp.f
c     iout           Switch: 0 no print; 1 print details, 2 summary
c
c     ndnr           Number of downstream nodes from return
c                    (ndnr = f (ndnnod)) 
c
c     nCarry         Number of carriers
c                    0 = No carrier
c
c     oprmaxM(l2)	   Monthly limit on changed WR	
c     oprmaxA(l2)    Annual limit on changed WR
c
c     qdiv(5, )      InBasin diversion by priority
c     qdiv(8, )      Transmountain diversion by priority
c
c     qdiv(18        Carrier passing thru a structure
c     qdiv(20        From Carrier by Storage or Exchange 

c     qdiv(26, )     From River by Exc_Pln
c     qdiv(27, )     Diversion to Carry, Exchange or Bypass
c     qdiv(28, )     Source is a reuse or admin plan
c     qdiv(38  )     Carried water reported as Carried, Exchange 
c                      or Bypassed but not used to calculate
c                      River Divert in Outmon.f
c     currtn         Immediate return to diverting node??
c     qtribu         Tributary InFlow (baseflow point)
c     qstern         Flow at diversion (used for transmtn divs)
c     small          a small value for roundoff (0.0) checking
c
c     pobl(it,ip)    T&C Obligation to a plan (ip) at time (it)
c		               	 Note calculated in RtnsecP
c
c
c ---------------------------------------------------------
c	                	Loss Data
c	    OprLoss(l2) =  Transit loss (%) 
c	   		             Transit loss is a true loss no routing
c	    ioprloss    =  int(OprLoss) carrier loss switch
c	   		             + transit loss, maybe carrier loss
c	   		             - 0 transit loss, maybe carrier loss
c	    TranLoss    =  Transit loss (fraction)
     
c	    OprLossC(l2,i)= Conveyance loss from a carrier (%)
c	   		             Conveyance loss gets routed to system
c	    OprLost= 	     conveyance loss (cfs)
c           
c	    OprEff1 = 	   source Carrier Efficiency 
c                    (1.0 - OprLoss(l2)/100)
c	    OprEffT = 	   Total Carrier Efficiency 
c           
c	    effmaxT=	     Transit loss for a ditch 
c    
c	    ncarry          indicator at least 1 carrier
c	    ncnum          # of carriers
c    
c _____________________________________________________________
c
c		Dimensions
      include 'common.inc'
      character 
     1  cwhy*48, cdestyp*12, ccarry*8, cpuse*3, csour*12,
     1  rec12*12, cTandC*3, cresid1*12, criver*12,
     1  corid1*12, cCallBy*12, ctype1*12, cImcdR*12,
     1  cwhy2*48
c
c
c_____________________________________________________________
c               Step 1; Common Initilization
c  
      corid1=corid(l2)
      cCallBy='directwr    '
c
c		       iout=0 no detials
c		            1 details
c		            2 summary 
c          ioutp=1 details of plan operation
c          ioutq=1 details of qdiv
      iout=1
      ioutP=0
      ioutQ=0   
      ioutiw=0 
c               
      if(ichk.eq.126 .and. iout.eq.0) iout=2
      if(corid(l2).eq. ccall) ioutiw=iw      
c      
      if(iout.eq.1) then
        write(nlog,*) ' '
        write(nlog,*) '  DirectWR  ',     
     1    iyrmo(mon),xmonam(mon)      
        write(Nlog,*)        
     1    ' directwr; ncallx    ichk    iout  ioutiw      iw',
     1    ' corid        ccall' 
        write(nlog,'(10x,5i8,2(1x,a12))')  
     1    ncallx, ichk, iout, ioutiw, iw, corid(l2), ccall
      endif
      
c
c ---------------------------------------------------------
c               b. Daily capability
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif      
c
c ---------------------------------------------------------
c               c. Initilize
      ieff2=1
      icx=26
      small = 0.001
      
      divact  =0.0
      divactX =0.0
      pfail1  =0.0
      ioff=0
      
      imcdX=1
      
      divaloS = -1.0/fac
      divexP  = -1.0/fac
      pavail  = -1.0/fac
      pdem1   = -1.0/fac
      pdem2   = -1.0/fac
      divCU   = -1.0/fac
      divCarry= -1.0/fac
      availX  = -1.0/fac
      alocfsR=-1.0/fac
      
      divactE= 0.0
      divact1= 0.0
      divact0=0.0
      divactL=0.0
      divactT=0.0
      divAdd=0.0
      csour='NA'
      cImcdR='NA'

      oprEffT=1.0
      oprEffTS=1.0
      
      divCap1=99999./fac
      divCap2=99999./fac
      
      ISHORT=0
      iwhy=0
      cwhy='NA'
      iwhy2=0
      cwhy2='NA'
      
      cdestyp='NA'
      ccarry='No'
      cpuse='No'
      cTandC='No'
            
      idcd2  = 0
      idcd2D = 0
      idcd2R = 0
      idcd2P = 0
      idcd2C = 0
      idcd2X = 0
      
      psto21=0.0
      psto22=0.0
c
c ---------------------------------------------------------
c                 d1. Return to River
      nriver=0
      cRiver='NA'
c
c                 d2. Diversion type
      ndtype = iopdesr(l2)
      if(ndtype.eq.7) then
        cdestyp='Plan     '
      else
        write(nlog,*) 
     1   '  Directwr; Problem a type 26 operating rule requires ',
     1     'the destination be a plan'
        goto 9999
      endif      
      np11=0
c
c ---------------------------------------------------------
c               e. Maximum changed WR limit
c		Note oprmaxM and oprmaxA are in acft      
      oprmax1=amin1(oprmaxM(l2), oprmaxA(l2))
      oprmax1=amax1(oprmax1,0.0)/fac
c
c ---------------------------------------------------------
c               f. Percent ownership      
      pctE=oprPct(l2)/100.0
      pctS=1.0-pctE
      
      if(ioutP.eq.1)
     1  write(nlog,*) ' directwr; pctE, pctS, oprmax1', 
     1  pctE, pctS, oprmax1*fac


c
c ---------------------------------------------------------
c               g. Set Carrier indicators
      nCarry=0
      if(intern(l2,1).gt.0) then
        write(nlog,*) 
     1   '  Directwr; Problem a type 26 operating rule does not ',
     1     'allow carriers'
        goto 9999
      endif
c
c ---------------------------------------------------------
c rrb 2007/06/06; 
c               h. Set Transit and Carrier Loss
      
      call SetLoss(nlog, icx, l2, fac, 
     1 maxopr,  intern, Oprloss,   OprLossC,
     1 ioprloss, nCarry, nRiver,   ncnum, 
     1 OprLost,  OprEff1, OprEffT, TranLoss, 
     1 internT,internL,corid(l2))
c
c ---------------------------------------------------------
c                 i. Check for release to river capability
      if(nRiver.gt.0) then
        write(nlog,*) 
     1   '  Directwr; Problem the type 26 operating rule does not ',
     1      'allow a release to the river'
          goto 9999      
cx        cRiver=cstaid(nRiver)
      endif
      
c
c ---------------------------------------------------------
c               i. Check Avail array coming in
      if(iout.eq.1) write(nlog,*) ' directwr; Calling Chekava In'
      call chekava(30, maxsta, numsta, avail)

c
c _________________________________________________________
c               Step 2; Set monthly on/off switch for changed WR
c                       operations (not diversion at source)
c
      if(imonsw(l2,mon).eq.0) then
        iwhy=1
        cwhy='Monthly switch Off'
        ioff=1
      endif   
c
c ---------------------------------------------------------
c		For a daily model set demand for beginning of season
      if(iday.eq.1 .and. imonsw(l2,mon).gt.0) then
        if (idy.lt.imonsw(l2,mon)) then
          iwhy=1
          cwhy='Daily switch Off'
          ioff=1
        endif  
      endif  
c
c ---------------------------------------------------------
c		For a daily model set demand for end of season
      if(iday.eq.1 .and. imonsw(l2,mon).lt.0) then
        if (idy.gt.iabs(imonsw(l2,mon))) then
          iwhy=1
          cwhy='Daily switch Off'
          ioff=1
        endif  
      endif  
c
c _________________________________________________________
c              Step 2b; Set reoperation control
c rrb 2015/10/10; Move to below Source data to allow 
c                 detailed printout
c      
c ________________________________________________________
c               Step 2c; Set T&C Plan pointer
c		                     ipTC  = T&C plan
c		                     ipUse = Reuse plan
      ipTC=iopsou(3,l2)
      if(ipTC.gt.0) then  
        write(nlog,*) 
     1   '  Directwr; Problem a type 26 operating rule does not ',
     1     'allow T&C data to be evaluated'
        goto 9999
      endif
c
c _________________________________________________________ 
c               Step 3; Set Source Data
c
      lr=iopsou(1,l2)
cr    write(nlog,*) '  directwr; l2, lr', l2, lr
      nd=idivco(1,lr)
c
c ---------------------------------------------------------      
c		              a0. Check reoperation control and exit
c                     if already operated one time
c rrb 2015/10/10; Move below to allow detailed printout
       icallOP(l2)=icallOP(l2) + 1 
c
       if(icallOP(l2).gt.1) then       
         iwhy=2
         cwhy='Reoperation not allowed'
         goto 260
       endif              
c
c ---------------------------------------------------------
c		              a1. Exit if source structure is off (iwhy=2)
      if(idivsw(nd).eq.0) then
        iwhy=2
        cwhy='Source Structure is Off'        
        goto 260
      endif
c 
      ISCD=IDVSTA(ND)
      NDNS=NDNNOD(ISCD)
      iuse=nduser(nd)
      csour=cdivid(nd)
c
c ---------------------------------------------------------
c                 a2. Set Demand at source                      
c           
c     write(nlog,*) '  directwr; iscd, ndns ', iscd, ndns
c
      if(idemtyp.le.3) then
        divreqx=divreq(iuse)
      else
        divreqx=divsw(iuse)
      endif
c
c ---------------------------------------------------------
c		              b. Set CU limit switch      
      rec12=cDivTyp(l2)
      iDep=0
      if(rec12(1:9).eq.'Diversion') then
        iDep=0
      else
c   
        write(nlog,*) 
     1   '  Directwr; Problem a type 26 operating rule requires ',
     1     'the diversion type be Diversion'
        goto 9999
cx      iDep=1           
      endif 
c
      diveff1=diveff(mon,nd)/100
c
c ---------------------------------------------------------
c		              c. Set CuLimit      
      if(iDep.eq.0) then
        CuLimit=1.0
      else
c
c ---------------------------------------------------------
c		              d. Use default or plan efficiency
c		Note pfail is in acft for continuity
        CuLimit=diveff(mon,nd)/100.        
      endif
c
c ---------------------------------------------------------
c rrb 2005/11/27; Add CuFac
      CuFac=OprEff(mon,l2)/100.          
      
c      
c ---------------------------------------------------------
c               d. Set current decree limits          
c		               note dcrdivS & dcrdivE are set in Oprinp.f
c                  l2 is a pointer to the water right
c
      dcrdiv1=amax1(0.0, dcrdivS(l2)-divdS(l2))
      dcrdiv2=amax1(0.0, dcrdivE(l2)-divdE(l2))
c
c ---------------------------------------------------------
c               e. Set current Capacity limits    
c rrb 2015/05-06/11/29; Set Capacity limit
        divCap1=divcap(nd)-divmon(nd)
        divCap2=divCap1
c
c _________________________________________________________
c               Step 4; Set destination data 
      nd2=iopdes(1,L2)
c
c _________________________________________________________ 
c               Step 4b; Set destination data 
c		                     Destination is a plan
c 
      if(ndtype.eq.7) then   
c
        nd2x=nd2
        np2=nd2x
        ndd2=0
        nr2=0
        iuse2=-1
c
c		              Exit if destination Plan (nd2) is off
        if(pOn(nd2x).le.small) then
          iwhy=4
          cwhy='Destination Plan is Off'          
          goto 260
        endif
c   
c                 idcd2 = generic destination location  
        idcd2=ipsta(nd2x)
        
        idcd2P=idcd2
        idcd2X=idcd2
        
        ndns2=ndnnod(idcd2)
        ndns2X=ndns2
        
        imcdX=idcd2

c
c                 Destination must be an administration (type 11) plan
        divreqx2=99999./fac
        np11=1
c
c *********************************************************
c rrb 2014-11-24; Key edit to put plan at source (iscd)
c                 not its location on the river when calculating
c                 supply and taking water from system
c
        idcd3=iscd
        ndns3=ndns        
c        
        if(iout.eq.1) write(Nlog,*) ' directwr; np2', pdem(np2)*fac,
     1    pdemT(np2)*fac, pdem(2)*fac, pdemT(2)*fac   
        
      endif        
c
c
c_____________________________________________________________
c               Step 5; Check flow available downstream 
c                       of the source (ndns)   
c
      CALL DNMFSO2(maxsta, AVAIL, IDNCOD, iscd,  ndns, IMCD,
     1  cCallBy)
     
      imcdX=imcd
      AvailX=avail(imcd)
c
c ---------------------------------------------------------
c               Test for a quick exit from routine (260)    
      if(avail(imcd).le.small) then
        iwhy=6
        cwhy='Available flow at Source (AvailX) = 0'
        goto 260
      endif
c
c_____________________________________________________________
c               Step 6; Check water right for a quick exit
c                       Note dcrdiv1 & 2 are set in step 1d
      divaloS=amax1(0.0, dcrdiv1+dcrdiv2)
c
c ---------------------------------------------------------
c               Test for a quick exit from routine (260)    

      if(divaloS.le.small) then
        if(iout.eq.1) then
          write(nlog,*) ' directwr; dcrdiv1+dcrdiv2,divreqX2 oprmax1'
          write(nlog,*) ' directwr;',(dcrdiv1+dcrdiv2)*fac,
     1                    divreqX2*fac, oprmax1*fac
        endif  
        
        iwhy=7
        cwhy='Remaining Decree (Dcrdiv1+Dcrdiv2)= 0'
        goto 260
      endif
c_____________________________________________________________
c               Step 7; Begin to calculate diversion at SOURCE
c                       (divalo)limited by decree (dcrdiv1),
c                       demand(divreqX) & capacity (divcap1)
c                       BUT NOT WATER SUPPLY
c                       Note divcap1 is set in step 3e
c 
c rrb 2015-05-06; Limit by capacity of total structure (divcap1             
cx    divalo=amin1(dcrdiv1, divreqX) 
      divalo=amin1(dcrdiv1, divreqX, divCap1)       
      divalo=amax1(divalo,0.0)
      
      if(iout.eq.1) then
        write(nlog,*) ' '
        write(nlog,*) ' directwr; divalo, dcrdiv1, divreqX' 
        write(nlog,*)      divalo*fac, dcrdiv1*fac, divreqX*fac 
      endif        

c
c ---------------------------------------------------------
c               a. Calculate available flow to source
c
c rrb 2014-05-15; Replace with simple downstream search and takout
       CALL DNMFSO2(maxsta,avail,IDNCOD,Iscd,NDNS,IMCD,
     1  cCallBy)
     
       divact=avail(imcd)
c
c rrb 2014-05-20; Limit the source diversion by the % of 
c                 available flow (divact*pctS) along with
c                 water right & demand (divalo)  
c                 Note will try to divert more if the 
c                 changed WR is limited in Step 15 
       divact1=amin1(divact*pctS, divalo)
       imcdS=imcd
       
       if(iout.eq.1) then
         write(nlog,*) '  '                             
         write(nlog,*) ' directwr; Step 7a'              
         write(nlog,*) ' directwr; ',                   
     1     'divact*fac, pcts, divalo*fac, divact1*fac'  
         write(nlog,*) ' directwr;',                    
     1     divact*fac, pcts, divalo*fac, divact1*fac    
       endif
            
c  
c --------------------------------------------------------- 
c               b. Remove diversion at source
       CALL TAKOUT(maxsta,AVAIL,RIVER,AVINP,QTRIBU,IDNCOD,
     1       divact1,ndns,iscd)  
c 
c                 Add in return flows
       CALL RTNSEC(icx,divact1,L2,iuse,ISCD,nd,ieff2)     
c
       if(ioutP.eq.1) then
         write(nlog,*) ' directwr_A; imcd avail, pctS, Divact1 ',   
     1    imcd, avail(imcd)*fac, pctS, divact1*fac  
       endif        
c
c                 Update remaining decree for bypass calculations
      dcrdiv1=amax1(0.0, dcrdiv1-divact1)          
c
c                 Update amount carried to limit changed WR      
      DIVMON(ND)=DIVMON(ND)+divact1      
c
c                 Update demand
      divreqX=amax1(0.0, divreqX-divact1)  
      
      if(iout.eq.1) then
        write(nlog,*) '  '                             
        write(nlog,*) ' directwr; Step 7b'              
        write(nlog,*) ' directwr; ',                   
     1    'divact*fac, pcts, divalo*fac, divact1*fac'  
        write(nlog,*) ' directwr;',                    
     1    divact*fac, pcts, divalo*fac, divact1*fac    
      endif          
c
c
c_____________________________________________________________
c               Step 8 Begin diversion at changed WR
c                      note not limited by source capacity
c
c ---------------------------------------------------------
c               8a. Exit to Source (250) if monthly switch for 
c                        changed WR is off
      if(ioff.eq.1) then
        divactE=0.0
        iwhy= 11
        cwhy='Monthly changed WR switch is off'
        goto 250
      endif  
c
c ---------------------------------------------------------
c               8b. Calculate Available flow at the Destination
c
c
c rrb 2014-11-24; Key difference from a type 24 
c                 by removing limitation associated
c                 with an exchange by calculating flow 
c                 available from the source downstream 
c                 by setting idcd3 and ndns3 to be at
c                 the source, not the plan location in 
c                 step 4b
      CALL DNMFSO2(maxsta,avail,IDNCOD,idcd3, ndns3,IMCD,
     1  cCallBy)
c     
      divact2=avail(imcd)      
c
      if(ioutP.eq.1) then
        write(nlog,*) ' directwr_B; idcd2, ndns2, avail'
        write(nlog,*) idcd2, ndns2, (avail(j)*fac, j=1,ndns2)
        write(nlog,*) ' '
        write(nlog,*) ' directwr_B; pctE, Divact2 imcd avail'   
        write(nlog,*) ' directwr_B;',
     1   pcte, divact2*fac, imcd, avail(imcd)*fac
      endif
c
c      
c ---------------------------------------------------------  
c		            8c. Destination is a Plan
c                   np11 is set in step 4b
c
      if(ndtype.eq.7) then
        divaloE=amin1(divact2, dcrdiv2, divreqX2)         
c
c               Add carrier Loss befor transfer limit      
        divaloE=divaloE/OprEffT     
c
c		            Limit to annual or monthly limit (oprmax1)        
        divaloE=amin1(divaloE, oprmax1)                 
        divaloE=amax1(0.0,divaloE)
        
        if(ioutP.eq.1) then    
          write(nlog,360)
     1      divact2*pctE*fac, dcrdiv2*fac,
     1      divreqX2*fac, pdem(np2)*fac, 
     1      oprmax1*fac, OprEffT, divaloE*fac
        endif
      endif      
c
c ---------------------------------------------------------  
c		            8d. Exit to source (250) if changed WR = 0      
      if(divaloE.le.small) then
        iwhy=13
        cwhy = 'Demand or remaining water right  = 0'
        goto 250
      endif  
c
c_____________________________________________________________
c               Step 10; Calculate amount to th changed WR (divactE)
c			                   less loss (idvactL)
      if(ioutP.eq.1) then
        write(nlog,*) ' '
        write(nlog,*) 'directwr step 10'       
      endif
      
      if(iout.eq.1) write(nlog,*) ' directwr;  ',
     1  divaloE*fac, culimit
    
      divCU=divaloE*culimit     
c
      divactE=amin1(divCU, divaloE, dcrdiv2)     
      divactE=amax1(0.0, divactE)
c
      if(iout.eq.1) then
        write(nlog,*) ' directwr;  ',
     1    ' divcu*fac, divaloE*fac, dcrdiv2*fac, divactE*fac'
        write(nlog,*) '            ',
     1    divCU*fac, divaloE*fac, dcrdiv2*fac, divactE*fac
      endif          
c
c ---------------------------------------------------------
c               10a. Adjust amount diverted at the source,
c                      (divmon), its carrier capacity 
c                      (divcap2), and bypass decree (dcrdiv2)
      dcrdiv2=amax1(0.0, dcrdiv2-divactE)      
c
c                 Allow Carrier Loss                       
      divactL=divactE*OprEffT  
c
c ---------------------------------------------------------
c		            10b Exit to source (250 if no available flow 
      if(divactE.lt.small) then
        iwhy=16
        cwhy='changed WR = 0'
        goto 250
      endif
c _________________________________________________________
c          
c               Step 12 Remove destination from avail
c		                  	if not done in Rivrtn (nriver>0)
      if(iout.eq.1) then
        write(nlog,*) ' '
        write(nlog,*) 'directwr step 12, nriver, idcd2c',
     1      nriver, idcd2c  
      endif
     
c
c
c rrb 2014-11-24; Key revision to a type 24 approach.
c                 Remove limitation associated
c                 with an exchange by calculating flow 
c                 available from the source downstream 
c                 by setting idcd3 and ndns3 to be at
c                 the source, not the plan location in 
c                 step 4b              
c
      CALL TAKOUT(maxsta,AVAIL,RIVER,AVINP,QTRIBU,IDNCOD,
     1            divactE,ndns3,idcd3)
c_____________________________________________________________
c               Step 14; Calculate return flow obligation
c			                   Note return patterns may be the default
c			                   structure (iuse) or from plan data
c			                   see RtnsecP
      if(ioutP.eq.1) then
        write(nlog,*) ' '
        write(nlog,*) 'directwr step 14'
      endif
c      
c_____________________________________________________________
c               Step 15; Double Check available flow at source 
c                        to see if more can be diverted        
 250  continue
      if(iout.eq.1) then
        write(nlog,*) ' '
        write(nlog,*) 'directwr step 15'
      endif
       
c
c rrb 2014/05/20; Check available flow from source downstream 
       CALL DNMFSO2(maxsta, AVAIL, IDNCOD, Iscd, NDNS, IMCD,
     1  cCallBy)
c
c ---------------------------------------------------------
c               15a. Exit to warning (9999) if avail is < 0
      IF(AVAIL(IMCD).le.(-1.*small)) then
        write(nlog,*) ' '
        write(nlog,*) ' directwr; Step 15'
        WRITE(nlog,310) '  directwr_2',
     1    IYR,MON,IW,NWRORD(1,IW),L2,lr, nd2, iuse2,
     1    idcd2,IMCD,divreqX2*fac, divact1*fac, divactE*fac, 
     1    divadd*fac, avail(imcd)*fac 
     
        goto 9999
      endif
c
c_____________________________________________________________
c rrb 2007/07/03
c               Step 16; Allow source structure to divert
c		                any water not used by changed WR
c
      dcrdivT=dcrdiv1
c
      if(iout.eq.1) 
     1  write(nlog,*) ' directwr; Source Use of changed WR Right',
     1  dcrdiv1*fac, dcrdiv2*fac
c
      divact0=divact1
c 
c ---------------------------------------------------------
c               16a.  Calculate additional diversion (divAdd) 
c rrb 2015/05/06; Limit diversion at source by the 
c                 remaining capacity (divCap(nd) - divmon(nd)
cx    divAdd=amin1(dcrdivT, divreqX)   
      divAdd=amin1(dcrdivT, divreqX, divCap(nd)-divmon(nd))           
c 
c ---------------------------------------------------------
c               16b. Check available flow from source downstream    
      CALL DNMFSO2(maxsta, AVAIL, IDNCOD, Iscd, NDNS, IMCD,
     1  cCallBy)
      
      pavail=avail(imcd)      
      divAdd=amin1(divAdd, pavail)
      divAdd=amax1(0.0, divAdd)
      
      divact1=divact1+divAdd
      
      if(iout.eq.1) then
        write(nlog,*) ' '
        write(nlog,*) ' directwr; Source Use of changed WR Right',
     1  divact0*fac, divact1*fac, divAdd*fac,
     1  divact1*fac, dcrdivT*fac, divreqX*fac
      endif
c 
c ---------------------------------------------------------
c		            16c. Remove additional diversion & add return
c                    flows
      if(divAdd.gt.small) then
        CALL TAKOUT(maxsta, AVAIL ,RIVER ,AVINP ,QTRIBU,IDNCOD,
     1              divAdd, NDNS,  ISCD  )                     
        CALL RTNSEC(icx,divAdd,L2,iuse,ISCD,nd,ieff2)
c
c ---------------------------------------------------------
c               16d. Adjust dimmon for capacity calculations        
        DIVMON(ND)=DIVMON(ND)+divAdd
      endif  
c
c_____________________________________________________________
c                 Step 17; Double Check available flow from
c                          destination downstream
c
      CALL DNMFSO2(maxsta, AVAIL, IDNCOD, idcd2, NDNS2, IMCD,
     1  cCallBy)
c             
c                 Allow minor roundoff
      iavail=avail(imcd)
      IF(AVAIL(IMCD).le.(-1.*small) .and. iavail.gt.-1) then
        avail(imcd)=0.0
      endif
c             
c                 Print warning if negative available flow
      IF(AVAIL(IMCD).le.(-1.*small)) then
        write(nlog,*) ' directwr; Step 17'
        
        WRITE(nlog,310) '  directwr_3',
     1    IYR,MON,IW,NWRORD(1,IW),L2,lr, nd2, iuse2,
     1    idcd2,IMCD,divreqX2*fac, divact1*fac, divactE*fac, 
     1    divadd*fac, avail(imcd)*fac 
     
        goto 9999
      endif
c
c _________________________________________________________
c                 Step 18; Update source data 
      if(idemtyp.le.3) then
        divreq(iuse)=divreq(iuse)-divact1
      else
        divreq(iuse)=amax1(0.0, divreq(iuse)-divact1)
        divsw(iuse)=divsw(iuse)-divact1
c
c                 Demand options 4 & 5               
        nw=idivco2(nd)        
        if(nw.gt.0) then
          if(ieffmax.le.0) then
            effd=diveff(mon,nd)/100.
            effw=diveffw(mon,nw)/100.
          else
            effd=effmax(nd)/100.
            effw=effmaxw(nw)/100.
          endif

          dcux=(divact1*effd)/effw
        endif
      endif
c
c		              Update diversion by this structure and user
      USEMON(iuse)=USEMON(iuse)+divact1
c
c		              Update diversion by this structure and user
c		              Note divact1 is the source diversion only
      IF(IRTURN(iuse).ne.4) then
        QDIV(5,ISCD)=QDIV(5,ISCD)+divact1
      else
        QDIV(8,ISCD)=QDIV(8,ISCD)+divact1
      endif
c _________________________________________________________
c
c                 Step 20; Update data for a plan destination 
c
      if(ndtype.eq.7) then     
c
c               Adjust plan supply for the changed WR        
        psuply(np2)=psuply(np2)+divactE * OprEffT
        psuplyT(np2)=psuplyT(np2)+divactE * OprEffT         
      endif  
c        
c _________________________________________________________
c               Step 22; Set Qdiv for the source and destination
      EffmaxT1=(100.0-OprLossC(l2,1))/100.0
c      
      if(iout.eq.1) then
        write(nlog,*) '  Directwr; qdiv(20', idcd2X, iscd, 
     1                   qdiv(20,idcd2X)*fac
        call flush(nlog)
      endif  
c
c _________________________________________________________
c               Step 24; Update reuse data
c		                     Note reusable return flows are set in 
c                        RtnsecR      
c     if(ipUse.gt.0) then
      if(nr2.gt.0 .and. ipUse.gt.0) then  
        psuply(ipUse)=psuply(ipUse)+divactL
        psuplyT(ipUse)=psuplyT(ipUse)+divactL
        
        if(ndtype.eq.2) then
          psto21=psto2(ipUse)          
          psto2(ipUse)=psto2(ipUse)+divactL*fac
          psto22=psto2(ipUse)          
        endif  
      endif      
c
c _________________________________________________________
c               Step 25; Update maximum diversion rate (Oprmax1)
c                        Note opermaxM and oprmaxA are in acft
      oprmaxM(l2)=oprmaxM(l2) - divactE*fac
      oprmaxM(l2)=amax1(oprmaxM(l2), 0.0)
      
      oprmaxA(l2)=oprmaxA(l2) - divactE*fac    
      oprmaxA(l2)=amax1(oprmaxA(l2), 0.0)
      
c
c _________________________________________________________
c		           Step 26; Set total diversion to be the amount
c		                    at the source (divact1) and the
c                       changed WR
      divactX=divact1+divactE/culimit      
c
c _________________________________________________________
c		            Step 27; Update diversion by this water right
c		                     by amount at source (divact1) and by
c		                     changed WR (divactE) befor CU adj
c                        (culimit)
      divd(lr) = divd(lr)+divactX
      divdS(l2)= divdS(l2)+divact1
c
      divdE(l2)= divdE(l2)+divactE/culimit
c      
      dcrdiv1=amax1(0.0, dcrdivS(l2)-divdS(l2))
      dcrdiv2=amax1(0.0, dcrdivE(l2)-divdE(l2))
c
      if(ioutp.eq.1) then
        write(nlog,*) ' '
        write(nlog,*) ' DrectEx;, dcrdivS(l2), dcrdivE(l2), dcrdiv1'
        write(nlog,*) ' directwr; ', dcrdivS(l2), dcrdivE(l2), dcrdiv1     
      endif 
      
c
c _________________________________________________________
c               Step 28; Update diversion by this Operating Rule
c		                     Note only show changed WR amount, not
c                        changed WR and source
      divactL=divactE*OprEffT       
      DIVO(L2)=DIVO(L2)+divactE
c
c_____________________________________________________________
c               Step 29; Print detailed results if requested
c 
  260 continue
c
c ---------------------------------------------------------
c		Detailed header      
c
c                Limit daily output to non zero values
      iprint=1
      if(iday.eq.1 .and. divactE+divact1.lt.small) iprint=0
      
      if(iout.ge.1 .and. iw.eq.ioutiw .and. iprint.eq.1) then      
        ncallX=ncallX+1
        if(ncallX.eq.1) then
          write(nlog,270) corid(l2), cdestyp, ccarry, cTandC, cpuse,
     1      cDivTyp(l2), criver
        else
c         write(nlog,*) ' '
        endif  
      
        write(nlog,280) '  DirectWR  ',     
     1    iyrmo(mon),xmonam(mon),idy, csour, cimcdR,
     1    iwx, iw,nwrord(1,iw),l2,lr,nd, iuse,
     1    nd2, ND2x,iuse2x,imcdX,imcdS, nriver, ncarry, oprEfft*100.,
     1    DIVREQx2*fac,AVAILX*fac,divaloS*fac,
     1    dcrdiv2*fac, divCU*fac,       
     1    pavail*fac,  culimit*100.,     
     1    oprmax1*fac, pfail1,
     1    divcarry*fac, divcap1,   divcap2,pdem2*fac, dcrdiv1*fac,        
     1    divact0*fac, divAdd*fac, divact1*fac,divactE*fac,
     1    (divactE+divact1)*fac, iwhy, cwhy
     
        if(iout.eq.1) then
          write(nlog,*) 'directwr; qdiv 8, 14, & 5'
          write(nlog,*)  Qdiv(8,iscd)*fac, qdiv(14,iscd)*fac, 
     1      qdiv(5,iscd)*fac
        endif


        if(iout.eq.1) then
          if(imcd.gt.0) then
            write(nlog,281) '  directwr;',
     1        imcd, divactE*fac,pavail*fac, stanam1(imcd)
          else
            write(nlog,281) '  directwr;',
     1        imcd, divactE*fac,pavail*fac
          endif
        endif  
      endif
c    
c_____________________________________________________________
c               Step 30; Set return switch (iretsw), shortage (ishort) 
c                 switch and actual diversion (divact)
      if(nd2.gt.0) then
        if(divactE.gt.small) iretsw=1
c
        if((divactE+small).lt.divaloE) ishort = 1  
c
c                 On/Off controls changed WR only
        if(ioff.eq.1) ishort=0        
      endif  
c
c _________________________________________________________
c               
c               Step 31; Set Call data at source 
c rrb 2008/06/10	      

      if(nd.gt.0) then
        ctype1='Diversion'
        call GetCall(iscd, imcdL(iscd), nd, ctype1)        
      endif  
c
c _____________________________________________________________
c               
c               Step 32 - Check Entire Avail array
c rrb 05/05/11; Check Avail going out of the routine
      call chekava(30, maxsta, numsta, avail)
c
c _____________________________________________________________
c               
c               Step 33 - Check Qdiv array
c
      if(ioutQ.eq.1) then
        write(nlog,*) ' '
        write(nlog,*) ' directwr; Qdiv report'
        write(nlog,'(4x, 39i8)') (j, j=1,39)
        do i=1, numsta
          write(nlog,'(i5, 39f8.0)') i, (qdiv(j,i)*fac, j=1,39)
        end do
      endif
      
c
c_____________________________________________________________
c               Step 34; Return
c

      RETURN
c
c_____________________________________________________________
c               Formats
c
     
  270   format(/, 
     1  '  DirectWR (Type 26); Operation Right ID = ', a12,
     1  ' Destination Type = ', a12,
     1  ' Carrier = ',a8, ' T&C Plan = ',a3,
     1  ' Reuse Plan (Y/N) = ', a3,  ' Diversion Type = ', a12,
     1  ' Release to River = ', a12,/    
     1  '  directwr    iyr mon   day',
     1  ' Source ID    Min ID      ',
     1  '     Iter     Iw  Nwrord      l2      lr      nd    iuse',
     1  '     Nd2    Nd2X  Iuse2X   ImcdX   ImcdS  nRiver  nCarry',
     1  ' OprEffT DivreqX2 AvailX DivaloS Dcrdiv2   DivCU',
     1  '  Pavail CuLimit Oprmax1  Pfail1 DivCary',
     1  ' divCap1 DivCap2   Pdem2 DcrDiv1 Divact0  DivAdd',
     1  ' Divact1 DivactE  TotDiv',
     1  '    iwhy Comment',/
     1  ' ___________ ____ ____ ____',
     1  ' ____________ ____________', 
     1  ' _______ _______ _______ _______ _______ _______ _______', 
     1  ' _______ _______ _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______ _______',
     1  ' _______ _______ _______',
     1  ' _______ __________________________')
     
c 280   FORMAT(a12, i5,1x,a4, i5, 1x, a12,13i8,23F8.0,i8,
  280   FORMAT(a12, i5,1x,a4, i5, 2(1x,a12),14i8,20F8.0,i8,
     1   1x, a48)
  281   FORMAT(a12, 143x, i8, f8.0, f8.2, 1x, a24)
  290   FORMAT(/, '  directwr   QDIV ',a12,/,16F7.1)
  300   FORMAT(/, '  directwr   QRES ',a12,/,16F7.1)
  310   FORMAT(/, '  directwr   Problem negative avail',//
     1  '  directwr    iyr  mon',
     1  '      Iw  nwrord      l2      lr     ND2   iuse2', 
     1  '   idcd2   imcd divreqX2 divact1 divactE  divAdd   avail'/  
     1  ' ___________ ____ ____', 
     1  ' _______ _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______ _______ _______',/
     1 a12, 2i5, 8i8, 20f8.0)
     
     
  320   format(/, '  directwr; avail  ',/,(10f10.2))
  330   format(/, '  directwr; river  ',/,(10f10.2))
  332   format(/, '  directwr; qtribu ',/,(10f10.2))
  334   format(/, '  directwr; qstern ',/,(10f10.2))
  340   format(/, '  directwr; Pavail, imcd, stanam ',
     1    f8.2, i8, 1x,a24)
  350   format(/, '  directwr; Problem with the changed WR reach')   
  360   format(' directwr;',
     1  '   divact2   dcrdiv2  divreqX2 pdem(np2)   oprmax1',
     1  '   OprEffT   divaloE',/' directwr;',
     1  ' --------- --------- --------- --------- ---------',
     1  ' --------- ---------',/
     1  ' directwr;', 20f10.2)  
c
c_____________________________________________________________
c               Error warnings
c
 9999 write(6,1050) 
      write(nlog,1051) 
c
c		Print results      
          write(nlog,270) corid(l2), cdestyp, ccarry, cTandC, cpuse,
     1      cDivTyp(l2), criver
      
        write(nlog,280) '  Problem   ',
     1    iyrmo(mon),xmonam(mon),idy, csour, cimcdR,
     1    iwx, iw,nwrord(1,iw),l2,lr,nd, iuse,
     1    nd2, ND2x,iuse2x,imcdX,imcdS, nriver, ncarry,oprEfft*100.,
     1    DIVREQx2*fac,AVAILX*fac,divaloS*fac,
     1    dcrdiv2*fac,  divCU*fac,     
     1    pavail*fac,   culimit*100.,     
     1    oprmax1*fac,  pfail1,
     1    divcarry*fac, divCap1,    divCap2,pdem2*fac, dcrdiv1*fac,
     1    divact0*fac,  divAdd*fac, divact1*fac, divactE*fac, 
     1    (divactE+divact1)*fac, iwhy, cwhy
      
      
 1050 format('    Stopped in directwr',/,
     1       '    See the *.log file')
 1051 format('    Stopped in directwr')
      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop 
      END
