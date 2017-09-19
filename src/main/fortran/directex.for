c
c     Last change:  RRB  18 Dec 100    2:29 pm
c
      SUBROUTINE DirectEx(IW,L2,ISHORT,divactX,ncallX)
c
c _________________________________________________________
c	Program Description
c

c
c 	Type 24; Direct Flow Exchange; It allows:  
c		 1. Part of  water right to be diverted at its
c		 original location to a diversion and 
c		 2. The balance of a water right to be 
c		 diverted upstream (exchanged) to a Diversion, 
c		 Reservoir or Plan. In general:
c
c      Approach: 
c      1. Initilize
c      2. Source is a water right (iopsou(1,l2)
c      3. Destination is a diversion, reservoir, or 
c         plan (iopdes(1,l2) 
c      4. Calculate diversion to the source limited by 
c         % ownership of right, availability of water, capacity
c         and demand STEP 7
c      5. Calculate exchange to the destination limited by 
c         % ownership of right, availability of water, capacity
c         and demand STEP 8-10
c      6. Determine if more water can be diverted at source
c         (can occurr if the exchange was limited for any reason)
C         STEP 16
c		 

c _________________________________________________________
c	Update History
c
c rrb 2014-04-26;  Revised logic to allow the source and
c       destination to shsre in shortages 
c
c
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
c     iopsou(3,l2)   T&C Plan id (for return flow obligation)
c     iopsou(4,l2)   NA
c     oprpct(l2)     Percent of the water right to be exchanged
c	
c
c     iopdes(1,l2)   destination
c                    if(ndtype.eq.2) cdestyp='Reservoir'
c                      if = iopdesr(l2) = 2 Reservoir
c                      if = iopdesr(l2) = 3 Diversion
c                      if = iopdesr(l2) = 7 Plan
c    
c     iopdes(2,l2)   destination account
c    
c     internT        Intervening structure type
c	                   1 = Carrier
c	                   2 = River
c     cdivTyp       Diversion Limit transfer to 
c	 	                  Depletion limit transfer to CU
c     divmon(nd)	  Amount diverted by the source (nd) in previous
c	                  iterations (used to calculate available 
c			              capacity)
c
c     nd             source diversion ID
c     iscd           idvsta(l2) stream ID of source diversion (nd)
c     ndns           # of nodes downstream of source diversion (nd)
c     iuse          source user
c
c     nd2            destination ID code
c		
c     nr2            iopdes(1,l2) if < 0, destination reservoir ID
c	    nd2x           iopdes(1,l2) if >10000, destination plan ID
c
c     idcd2D         stream ID of destination diversion 
c     idcd2R         stream ID of destination reservoir
c     idcd2P         stream ID of destination plan
c     idcd2C         stream ID of 1'st destination carrier
c	    idcd2          Stream id of destination. May be a diversion,
c		                   reservoir, plan OR CARRIER
c     idcd2X         stream ID of destination diversion (nd2) or 
C                      reservoir or plan (NOT CARRIER)
c
c     ndns2          # of nodes downstream of destination 
c                    diversion, reservoir, or plan
c     iuse2          destination user 
c
c	    ipuse		      Reuse indicator 0=no, +=reuse plan
c
c     imcdX          pointer to avail array. 
c	    imcdS          pointer to minimum flow below source
c
c     icx            subroutine call # (24) for I/O in rtnsec
c     IW             Global water right counter
c     L2             Operational right pointer
c     ishort         code for reoperation; 0=no, 1=yes
c
c   	CuLimit        fraction to be diverted (diversion or depletion)
c   	TcLimit        fraction to apply to T&C requirement
c
c     divactE        actual diversion at destination by exchange
c     divact1        actual diversion at source 
c     divactL	       diversion less loss
c
c     divreqx        Diversion demand for types 1-3 (standard)
c
c     dcrdivS        % of Water right used at source (cfs) Set in 
C                      oprinp
c     divdS          Amount diverted at source in previous
c                    iterations (cfs)
c     dcrdiv1        Remaining water right at source (cfs)
c     dcrdivE        % of Water right used at exchange (cfs) Set in 
C                      oprinp
c	    dcrdiv2        Remaining water right at exchange (cfs)
c
c     divcap         Structure capacity
c     divmon         Capacity diverted in previous iterations
c
c     divAdd         Additional diversion by source structure 
c		                 when the exchange is limited
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
c     nCarry         0 No carrier
c                    1 No return to River, Final Destination is
c                      from a carrier
c                    2 Return to River, Final Destination is
c                      from a carrier
c                    3 Return to River, Final Destination is 
c                      from the river
c
c     oprmaxM(l2)	   Monthly limit on exchange	
c     oprmaxA(l2)    Annual limit on exchange
c
c     qdiv(5, )      InBasin diversion by priority
c     qdiv(8, )      Transmountain diversion by priority
c
c     qdiv(18        Carrier passing thru a structure (e.g. divcar)
c     qdiv(20        From Carrier by Storage or Exchange (e.g. carrpl)

c     qdiv(26, )     From River by Exc_Pln
c     qdiv(27, )     Diversion to Carry, Exchange or Bypass
c     qdiv(28, )     Source is a reuse or admin plan
c
c     currtn         Immediate return to diverting node??
c     qtribu         Tributary InFlow (baseflow point)
c     qstern         Flow at diversion (used for transmtn divs)
c     small          a small value for roundoff (0.0) concerns
c
c     pobl(it,ip)    T&C Obligation to a plan (ip) at time (it)
c		               	 Note calculated in RtnsecP
c
c   
c     qres(4  	     From Carrier by Sto_Exchange
c     qres(18 	     From river by Exch_Plan
c
c ---------------------------------------------------------
c		Loss Data
c	 OprLoss(l2) =  Transit loss (%) 
c			Transit loss is a true loss no routing
c	 ioprloss    = int(OprLoss) carrier loss switch
c			+ transit loss, maybe carrier loss
c			- 0 transit loss, maybe carrier loss
c	 TranLoss    =  Transit loss (fraction)

c	 OprLossC(l2,i) = Conveyance loss from a carrier (%)
c			  Conveyance loss gets routed to system
c	 OprLost= 	conveyance loss (cfs)
c        
c	 OprEff1 = 	source Carrier Efficiency 
c                         (1.0 - OprLoss(l2)/100)
c	 OprEffT = 	Total Carrier Efficiency 
c        
c	 effmaxT=	Transit loss for a ditch 
c
c	 ncarry          indicator at least 1 carrier
c	 ncnum          # of carriers
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
      cCallBy='DirectEx    '
c
c		       iout=0 no detials
c		            1 details
c		            2 summary 
c          ioutp=1 details of plan operation
c          ioutq=1 details of qdiv
c          ioutC=1 details of carrier operation 
      iout=0
      ioutP=0
      ioutQ=0
      ioutC=0
      ioutiw=0 
         
      if(ichk.eq.124 .and. iout.eq.0) iout=2
      if(corid(l2).eq. ccall) ioutiw=iw
     
cx      if(iout.ge.1 .and. ncallx.eq.0) then
cx        write(nlog,102) corid(l2), iout, ioutiw, iw
cx 102    format(/, 72('_'),/ 
cx     1  '  DirectEx; ID = ', a12, 5i5)
cx      endif         
      
      
      if(iout.eq.1) then
        write(Nlog,*)
     1    ' DirectEx; ncallx    ichk    iout  ioutiw      iw',
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
      icx=24
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
      
      divCap1=0.0
      divCap2=0.0
      
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
c rrb 2008/01/25; Return to River
      nriver=0
      cRiver='NA'
c
c               d. Diversion type
      ndtype = iopdesr(l2)
      if(ndtype.eq.2) cdestyp='Reservoir'
      if(ndtype.eq.3) cdestyp='Diversion'
      if(ndtype.eq.7) cdestyp='Plan     '      
      np11=0
c
c ---------------------------------------------------------
c		            e. Maximum exchange limit
c		Note oprmaxM and oprmaxA are in acft      
      oprmax1=amin1(oprmaxM(l2), oprmaxA(l2))
      oprmax1=amax1(oprmax1,0.0)/fac
c
c ---------------------------------------------------------
c               f. Percent ownership      
      pctE=oprPct(l2)/100.0
      pctS=1.0-pctE
      
      if(ioutP.eq.1)
     1  write(nlog,*) ' DirectEx; pctE, pctS, oprmax1', 
     1  pctE, pctS, oprmax1*fac


c
c ---------------------------------------------------------
c		            g. Set Carrier indicators
      nCarry=0
      if(intern(l2,1).gt.0) then
        ccarry='Yes'
        ncarry=1
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
c rrb 2008/01/14; Add release to river capability
      if(nRiver.gt.0) cRiver=cstaid(nRiver)
      
c
c ---------------------------------------------------------
c               i. Check Avail array coming in
      if(iout.eq.1) write(nlog,*) ' DirectEx; Calling Chekava In'
      call chekava(20, maxsta, numsta, avail)

c
c _________________________________________________________
c               Step 2; Set monthly on/off switch for exchange
c                       operations (not diversion at source)
c
c rrb 04/22/96; Allow month switch
c rrb 05/11/14; On/Off impacts transfer only (also see step 11b)

      if(imonsw(l2,mon).eq.0) then
        iwhy=1
        cwhy='Monthly switch Off'
cr      goto 260
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
c ________________________________________________________
c               Step 2b; Set T&C Plan pointer
c		ipTC  = T&C plan
c		ipUse = Reuse plan
      ipTC=iopsou(3,l2)
      if(ipTC.gt.0) cTandC='Yes'
      ipUse=ireuse(l2)
      if(ipUse.gt.0) cpuse='Yes'
c _________________________________________________________ 
c               Step 3; Set Source Data
c
      lr=iopsou(1,l2)
cr    write(nlog,*) '  DirectEx; l2, lr', l2, lr
      nd=idivco(1,lr)
c
c		Exit if source structure is off (iwhy=2)
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
      
            
c     write(nlog,*) '  Directex; iscd, ndns ', iscd, ndns
c
      if(idemtyp.le.3) then
        divreqx=divreq(iuse)
      else
        divreqx=divsw(iuse)
      endif
c
c ---------------------------------------------------------
c		b. Set CU limit switch      
      rec12=cDivTyp(l2)
      iDep=0
      if(rec12(1:9).eq.'Diversion') iDep=0
      if(rec12(1:9).eq.'Depletion') iDep=1
      diveff1=diveff(mon,nd)/nd
cr
cr rrb 2005/11/14; Add TcLimit
      TcLimit= diveff(mon,nd)/100.  
      
      if(iDep.eq.0) then
        CuLimit=1.0
      else
c
c ---------------------------------------------------------
c		c. Use default or plan efficiency
c		Note pfail is in acft for continuity
        CuLimit=diveff(mon,nd)/100.        
        if(ipTC.gt.0) then
          pdem1=pdem(ipTC)        
          pfail1=pfail(ipTC)
          if(peff(mon,ipTC).gt.0.) then      
            CuLimit=peff(mon,ipTC)/100.
            TcLimit=peff(mon,ipTC)/100.
          endif  
        endif  
      endif
c
c ---------------------------------------------------------
c rrb 2005/11/27; Add CuFac
      CuFac=OprEff(mon,l2)/100.          
      
c      
c ---------------------------------------------------------
c               d. Set decree limits
c          
c		note dcrdivS & dcrdivE are set in Oprinp.f
c        l2 is a pointer to the water right
c
      dcrdiv1=amax1(0.0, dcrdivS(l2)-divdS(l2))
      dcrdiv2=amax1(0.0, dcrdivE(l2)-divdE(l2))
c
c ---------------------------------------------------------
c		e. Set Capacity limits      
c rrb 2007/11/29; Adjust capacity for amount exchanged (divdE(l2)
      divCap1=divcap(nd)-divmon(nd)
      divCap2=divCap1
c      
c ---------------------------------------------------------
c               e. Detailed output
c
      if(iout.eq.1) then
        write(nlog,*) ' '
        write(nlog,*) ' DirectEx; ipTC', ipTC              
        write(nlog,*) ' DirectEx; iprf, nd, culimit %', 
     1                 iprf(ipTC), nd, culimit*100
      endif
      
c
c _________________________________________________________
c rrb 2001/06/25; 
c               Step 4; Set destination data 
c
c ************************************************** DESTINATION

c			Destination is a diversion (ndtype=3)
      nd2=iopdes(1,L2)
c
      if(ndtype.eq.3) then
        nd2x=nd2
        ndd2=nd2
        nr2=0
        np2=0
c
c ---------------------------------------------------------
c		Exit if destination structure (nd2) is off (iwhy=3)
        if(idivsw(nd2).eq.0) then
          iwhy=3
          cwhy='Destination Structure is Off'          
          goto 260
        endif
c     
        idcd2=idvsta(nd2)
        idcd2D=idcd2
        idcd2X=idcd2
        
        ndns2=ndnnod(idcd2)
        ndns2X=ndns2
        imcdX=idcd2
c
        iuse2=nduser(nd2)+iopdes(2,l2)-1 
        iuse2X=iuse2
c
        if(idemtyp.le.3) then
cr        divreqx2=divreq(iuse2)
          divreqx2=amin1(divreq(iuse2), divcap(nd2)-divmon(nd2))
        else
cr        divreqx2=divsw(iuse2)
          divreqx2=amin1(divsw(iuse2), divcap(nd2)-divmon(nd2))
        endif
        divreqx2=amax1(0.0, divreqx2)
      endif        
      
      
c
c _________________________________________________________
c rrb 2006/04/25 
c               Step 4b; Set destination data 
c		         Destination is a plan
c 
      if(ndtype.eq.7) then   
c
c rrb 2014-06-15; Revise to use iopdesr(l2) as teh type indicator
cx      nd2x=nd2-10000
        nd2x=nd2
        np2=nd2x
        ndd2=0
        nr2=0
        iuse2=-1
c
c		Exit if destination Plan (nd2) is off
        if(pOn(nd2x).le.small) then
          iwhy=4
          cwhy='Destination Plan is Off'          
          goto 260
        endif
c     
        idcd2=ipsta(nd2x)
        idcd2P=idcd2
        idcd2X=idcd2
        
        ndns2=ndnnod(idcd2)
        ndns2X=ndns2
        
        imcdX=idcd2
c
c rrb 2007/07/09; Allow the destination to be a T&C, Well Aug
c		  Special (10) or Admin Plan (11)
        if(iplntyp(np2) .ne. 11) then
          divreqx2=amax1(0.0, pdem(np2))
          np11=0
        else
          divreqx2=99999./fac
          np11=1
        endif
        if(iout.eq.1) write(Nlog,*) ' DirectEx; np2', pdem(np2)*fac,
     1    pdemT(np2)*fac, pdem(2)*fac, pdemT(2)*fac   
        
      endif        
c
c
c _________________________________________________________
c
c               Step 4c; Set destination data 

c			Destination is a reservoir (ndtype=2)

      if(ndtype.eq.2) then
c
c rrb 2014-06-15; Revise to use iopdesr(l2) as teh type indicator
cx      nr2=-nd2
        nr2=nd2
        nd2x=nr2
        np2=0
        ndd2=0
c     
        idcd2=irssta(nr2)
        idcd2R=idcd2        
        idcd2X=idcd2
        
        ndns2=ndnnod(idcd2)
        ndns2X=ndns2

c
c ---------------------------------------------------------
c rrb 2006/09/25; Allow multiple reservoir accounts - Initilize
c		  Note use nr2 (not nr)
        nro=1
        if(iopdes(2,l2).lt.0) then
          nro=-iopdes(2,l2)
          irow=nowner(nr2)
        endif

        if(iopdes(2,l2).gt.0) then
          nro=1
          irow=nowner(nr2)+iopdes(2,l2)-1
        endif
        
        
        iuse2x=irow
        iuse2=-1
        
c
c ---------------------------------------------------------
c rrb 2006/09/25; Allow multiple reservoir accounts - Demand
        cursa=0.0
        do n=1, nro
          n1=irow+n-1
          cursa=cursa+(ownmax(n1)-curown(n1))
        end do  
c
c               b1. Set demand
c rrb 2006/09/25; Allow multiple accounts
cr      divreqx2=amin1(ownmax(irow)-curown(irow),
        divreqx2=amin1(cursa,
     1                 volmax(nr2)-cursto(nr2),
     1                 tarmax(nr2)-cursto(nr2))/fac
        divreqx2=amax1(0.0, divreqx2)
      endif  
      
c
c _____________________________________________________________
c
c               Step 4d; Destination is through a carrier
c		         Adjust diversion location (idcd2)
c			 but not the destinaion location (idcd2X)
      if(ncarry.gt.0) then
        ccarry='Yes'
        nc=intern(l2,1)
        idcd2=IDVSTA(nc)
        
        idcd2C=idcd2
c       idcd2X=idcd2        
        ndns2=NDNNOD(idcd2)
        divcarry=divcap(nc)-divmon(nc)
      endif

c
c_____________________________________________________________
c               Step 5; Check flow at source   
c
c rrb 2014-05-20; Revise to check at source
cx    CALL DNMFSO2(maxsta, AVAIL, IDNCOD, idcd2, NDNS2, IMCD,
cx   1  cCallBy)
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
c               Step 6; Check water right 
      divaloS=amax1(0.0, dcrdiv1+dcrdiv2)
c
c ---------------------------------------------------------
c               Test for a quick exit from routine (260)    

      if(divaloS.le.small) then
        if(iout.eq.1) then
          write(nlog,*) ' DirectEx; dcrdiv1+dcrdiv2,divreqX2 oprmax1'
          write(nlog,*) ' DirectEx;',(dcrdiv1+dcrdiv2)*fac,
     1                    divreqX2*fac, oprmax1*fac
        endif  
        
        iwhy=7
        cwhy='Remaining Decree (Dcrdiv1+Dcrdiv2)= 0'
        goto 260
      endif
c_____________________________________________________________
c               Step 7; Begin to calculate diversion at SOURCE
c                       (divact1)limited by decree (dcrdiv1),
c                       demand(divreqX) & capacity (divcapS)
c                       BUT NOT WATER SUPPLY
      if(divCap1.le.small) then
        iwhy=10
        cwhy='Remaining capacity at source (DivCap1) = 0' 
        goto 260
      endif
c
c rrb 2014-05-15; clean up variable names (use S for source, not Y)
c                 and correct to calculate total supply at source
c              
cx      divcapY=divcap1*pctS
cx      divalo=amin1(dcrdiv1, divreqX, divCapY)
      divcapS=divcap1*pctS
      divalo=amin1(dcrdiv1, divreqX, divCapS) 
      divalo=amax1(divalo,0.0)
      
      if(iout.eq.1) then
        write(nlog,*) ' '
        write(nlog,*) ' DirectEx; divalo, dcrdiv1, divreqX, divCapS' 
        write(nlog,*)      divalo*fac, dcrdiv1*fac, divreqX*fac, 
     1    divCapS*fac 
      endif        

c
c ---------------------------------------------------------
c                 Calculate available flow
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
c                 exchange is limited   
       divact1=amin1(divact*pctS, divalo)
       imcdS=imcd
       
       if(iout.eq.1) then
         write(nlog,*) '  '                             
         write(nlog,*) ' DirectEx; Step 7a'              
         write(nlog,*) ' DirectEX; ',                   
     1     'divact*fac, pcts, divalo*fac, divact1*fac'  
         write(nlog,*) ' DirectEX;',                    
     1     divact*fac, pcts, divalo*fac, divact1*fac    
       endif
            
c
c rrb 2015-05-15; addition  remove diversion at source
       CALL TAKOUT(maxsta,AVAIL,RIVER,AVINP,QTRIBU,IDNCOD,
     1       divact1,ndns,iscd)  
c 
c rrb 2015-05-15; Add in return flows
       CALL RTNSEC(icx,divact1,L2,iuse,ISCD,nd,ieff2)     
c
       if(ioutP.eq.1) then
         write(nlog,*) ' DirectEx_A; imcd avail, pctS, Divact1 ',   
     1    imcd, avail(imcd)*fac, pctS, divact1*fac  
       endif        
c
c rrb 2008/01/08; Update remaining decree for bypass calculations
      dcrdiv1=amax1(0.0, dcrdiv1-divact1)      
     

c rrb 2007/06/06; Update amount carried to limit Exchange      
      DIVMON(ND)=DIVMON(ND)+divact1      
      divCap2=divCap2-divact1
c
c rrb 2009/05/11; Correction - Update demand
      divreqX=amax1(0.0, divreqX-divact1)  
      
      if(iout.eq.1) then
        write(nlog,*) '  '                             
        write(nlog,*) ' DirectEx; Step 7b'              
        write(nlog,*) ' DirectEX; ',                   
     1    'divact*fac, pcts, divalo*fac, divact1*fac'  
        write(nlog,*) ' DirectEX;',                    
     1    divact*fac, pcts, divalo*fac, divact1*fac    
      endif          
c
c
c_____________________________________________________________
c               Step 8 Begin diversion at exchange
c
c ---------------------------------------------------------
c                     8a Exit to Source (250) if monthly switch for 
c                        exchange is off
      if(ioff.eq.1) then
        divactE=0.0
        iwhy= 11
        cwhy='Monthly exchange switch is off'
        goto 250
      endif  
c
c ---------------------------------------------------------
c                     8b Exit to Source (250) if T&C plan is in failure
      if(ipTC.gt.0) then
c       write(nlog,*) '  DirectEx; iptc, ipfail(iptc), pfail(iptc)'
c       write(nlog,*) '  DirectEx;', iptc, ipfail(iptc), pfail(iptc)
        if(ipfail(ipTC).eq. 1 .and. pfail(ipTC).gt.small) then
          divactE=0.0
          iwhy= 12
          cwhy='T&C Plan Failure > 0'
c
c		      Note go to 250 since source may continue to divert          
          goto 250
        endif  
      endif 
c 
c ---------------------------------------------------------
c                     8c Calculate Available flow at the Destination
c
c rrb 2014-05-15; Search from the exchange downstream
c                 since water has already been diverted from
c                 the source and removed downstream
c
c			            idcd2 is the destination location 
c                 ndns2 is the number downstream of destination     
      CALL DNMFSO2(maxsta,avail,IDNCOD,idcd2, ndns2,IMCD,
     1  cCallBy)
c
c rrb 2014-06-15; Use divact2 for excchange info                
cx    divactS=avail(imcd)
      divact2=avail(imcd)      
c
      if(ioutP.eq.1) then
        write(nlog,*) ' DirectEx_B; idcd2, ndns2, avail'
        write(nlog,*) idcd2, ndns2, (avail(j)*fac, j=1,ndns2)
        write(nlog,*) ' '
        write(nlog,*) ' DirectEx_B; pctE, Divact2 imcd avail'   
        write(nlog,*) ' DirectEx_B;',
     1   pcte, divact2*fac, imcd, avail(imcd)*fac
      endif
c
c
c ---------------------------------------------------------  
c		                 8d. Destination is a diversion
c
      if(ndtype.eq.3) then
c
c rrb 2015-05-15; correction
cx      divaloE=amin1(divactS*pctE, dcrdiv2, divreqX2,
        divaloE=amin1(divact2, dcrdiv2, divreqX2, 
     1                divcap(nd2)-divmon(nd2),
     1                divcap(nd)-divmon(nd))
c
c rrb 2006/06/06; Add carrier loss
c		  Note Oprmax1 is the diversion limit
        divaloE=divaloE/OprEffT
        divaloE=amin1(divaloE, oprmax1)                 
        divaloE=amax1(0.0,divaloE)
      endif
      
c      
c ---------------------------------------------------------  
c		                8e Destination is a Plan
c
      if(ndtype.eq.7) then
        if(np11.eq.0) then
c
c rrb 2015-05-15; correction        
cx          divaloE=amin1(divactS*pctE, dcrdiv2, divreqX2, pdem(np2),
          divaloE=amin1(divact2, dcrdiv2, divreqX2, pdem(np2),         
     1      divcap(nd)-divmon(nd))
        else
c
c rrb 2015-05-15; correction        
cx          divaloE=amin1(divactS*pctE, dcrdiv2, divreqX2,
          divaloE=amin1(divact2, dcrdiv2, divreqX2,         
     1    divcap(nd)-divmon(nd))
        endif
c
c rrb 2007/05/25; Add carrier Loss befor transfer limit      
        divaloE=divaloE/OprEffT     
c
c		Limit to annual or monthly limit (oprmax1)        
        divaloE=amin1(divaloE, oprmax1)                 
        divaloE=amax1(0.0,divaloE)
        
        if(ioutP.eq.1) then
     
          write(nlog,360)
     1      divact2*pctE*fac, dcrdiv2*fac,
     1      divreqX2*fac, pdem(np2)*fac, 
     1      oprmax1*fac, OprEffT, divaloE*fac
 360    format(' DirectEx;',
     1  '   divact2   dcrdiv2  divreqX2 pdem(np2)   oprmax1',
     1  '   OprEffT   divaloE',/' DirectEx;',
     1  ' --------- --------- --------- --------- ---------',
     1  ' --------- ---------',/
     1  ' DirectEx;', 20f10.2)
  
        endif
      endif      
c    
c ---------------------------------------------------------  
c		                8f. Destination is a reservoir      
      if(ndtype.eq.2) then
c
c rrb 2015-05-15; correction      
cx        divaloE=amin1(divactS*pctE, dcrdiv2, divreqX2)
        divaloE=amin1(divact2, dcrdiv2, divreqX2)        
c
c rrb 2006/06/06; Add carrier loss
        divaloE=divaloE/OprEffT        
        divaloE=amin1(divaloE, oprmax1)   
        
        if(iout.eq.1) write(nlog,*)       
     1    '  DirectEx; l2, mon, oprmax', l2, mon, 
     1       oprmax1*fac, divaloE*fac
c
        divaloE=amax1(0.0,divaloE)
      endif        
      
cx    write(nlog,*) ' DirectEx; Avail_0 ', (avail(i)*fac, i=1, 10)       
      
c_____________________________________________________________
c rrb 2007/06/06; 
c               Step 9; Process carrier limitations
c		ncarry is indicator at least 1 carrier
c		ncnum is the number of carriers
c		OprEff1 is the lost (oprlost(lw)
c		Divalo gets reduced by carrier capacity
c		DivCarry is the limitating carrier capacity
c		Lopr is an operating right ID with a 
c		NoprS is the structure id of the structure
c		 that supplied water to the accounting
c		 plan that already has a volumetric limit adjustment
      if(ncarry.gt.0) then
cx        if(lopr.gt.0) then        
cx          loprR=iopsou(1,lopr)
cx          noprS=idivco(1,loprR)        
cx        endif  
        norpS=0
              
        call SetCarL(nlog, icx, l2, fac, 
     1    maxopr,  maxdiv, intern, OprLossC,
     1    ncarry,  ncnum,  noprS,  internT,
     1    OprEff1, DivCap, DivMon, DivCarry, DivaloE)
      endif

c
c ---------------------------------------------------------  
c		                9b. Exit to source (250) if exchange = 0      
      if(divaloE.le.small) then
        iwhy=13
        cwhy = 'Demand, Capacity or Exchange Potential = 0'
c
        goto 250
      endif  
c
c_____________________________________________________________
c               Step 10; Calculate amount to be exchanged (divactE)
c			                   less loss (idvactL)
c			 May be a diversion, reservoir or plan
c rrb 2014-05-20; Correction
      if(ioutP.eq.1) then
        write(nlog,*) ' '
        write(nlog,*) 'DirectEx step 10'       
      endif
      
cx    divCU=divactS*pctE*culimit
      if(iout.eq.1) write(nlog,*) ' DirectEx;  ',
     1  divaloE*fac, culimit
    
      divCU=divaloE*culimit     
c
c rrb 2015-05-15; Correction      
cx    divactE=amin1(divactS*pctE*culimit, divExP, divaloE, dcrdiv2)
      divactE=amin1(divCU, divaloE, dcrdiv2)     
      divactE=amax1(0.0, divactE)
c
      if(iout.eq.1) then
        write(nlog,*) ' DirectEx;  ',
     1    ' divcu*fac, divaloE*fac, dcrdiv2*fac, divactE*fac'
        write(nlog,*) '            ',
     1    divCU*fac, divaloE*fac, dcrdiv2*fac, divactE*fac
      endif
           
c
c rrb 2007/11/29; Adjust amount diverted at the source,(divmon), its 
c		  carrier capacity (dvcap2), and bypass decree (dcrdiv2)
      divmon(nd)=divmon(nd) + divactE      
      divcap2=divcap2-divactE
      dcrdiv2=amax1(0.0, dcrdiv2-divactE)      
c
c rrb 2007/06/06; Add Carrier Loss                       
      divactL=divactE*OprEffT  
      
      if(ioutC.eq.1) then  
        write(nlog,*) 'DirectEX;  divactE*fac, OpreffT, divactL*fac'
        write(nlog,*) 'DirectEX; ', divactE*fac, OpreffT, divactL*fac
      endif
c
c ---------------------------------------------------------
c		            10b Exit to source (250 if no available flow 
      if(divactE.lt.small) then
        iwhy=16
        cwhy='Exchange = 0'
        goto 250
      endif
c
c _________________________________________________________
c
c               Step 11; Adjust for water dumped to the river 
c		                     by an carrer (if nriver>0).
c		             Call RivRtn that will:
c		             1. Adjust the diversion as necessary since
c		                the return location may be upstream or 
c		                downstream of the carrier diversion
c		             2. Add the river return to Avail
c		             3. Remove the ultimate destination from Avail
c		             Note navail=1 allows avail to be adjusted
      if(iout.eq.1) then
        write(nlog,*) ' '
        write(nlog,*) 'DirectEx step 11' 
      endif
         
      if(nRiver.gt.0) then  
        relact=0.0
        write(nlog,*) ' DirectEX; Calling RivRtn OprEfft, divactE ',
     1     oprefft, divactE*fac
c
c rrb 2010/10/15; Update to allow operation with a depletion release
       if(idep.eq.0) then
         DepfacM=1.0
       else
         DepfacM=diveff1
       endif      
             
        nAvail=1
        call RivRtn(
     1    icx, nriver, l2, ndtype, iscd, nd2, iuse2, idcd2,idcd2X, 
     1    fac, smallN, oprEffT, relact, adj, divactE, divactL, 
     1    ncnum, nAvail, alocfsR, DepfacM, imcdR, corid1)
 
        if(imcdR.gt.0) cImcdR= cstaid(imcdR)
 
        if(divactE.le. small) then
          iwhy=15
          cwhy='Available flow with River Return = zero'
          goto 250
        endif  
      endif    
c
c _________________________________________________________
c          
c               Step 12 Remove destination from avail
c		                  	if not done in Rivrtn (nriver>0)
      if(iout.eq.1) then
        write(nlog,*) ' '
        write(nlog,*) 'DirectEx step 12, nriver, idcd2c',
     1      nriver, idcd2c  
      endif
     
       if(nriver.eq.0) then
c
c rrb 2011/05/25; Correction divert from carrier if appropriate 
c                 and revise the diversion to be without loss
c                 idcd2C = stream ID of 1'st destination carrier
         if(idcd2C.eq.0) then  
           if(ioutC.eq.1) then
             write(nlog,*) ' '
             write(nlog,*) 'DirectEx; idcd2x, river(idcd2x)*fac, 
     1                    avail(idcd2x)'
             write(nlog,*) 'DirectEx;', idcd2x, river(idcd2x)*fac, 
     1                    avail(idcd2x)*fac        
           endif
c              
           CALL TAKOUT(maxsta,AVAIL,RIVER,AVINP,QTRIBU,IDNCOD,
     1       divactE,ndns2X,idcd2X)
c
c     
           if(ioutC.eq.1) then
             write(nlog,*) ' '
             write(nlog,*) 'DirectEx; idcd2, river(idcd2x)*fac, 
     1                     avail(idcd2x)'
             write(nlog,*) 'DirectEx;', idcd2x, river(idcd2x)*fac, 
     1                     avail(idcd2x)*fac 
           endif 
          
         else
            CALL TAKOUT(maxsta,AVAIL,RIVER,AVINP,QTRIBU,IDNCOD,
     1                 divactE,ndns2,idcd2)
     
         endif     
c      
       endif  
c
c_____________________________________________________________
c
c		            Step 13; DIVERSION DESTINATION add return flows
      if(iout.eq.1) then
        write(nlog,*) ' '
        write(nlog,*) ' DirectEx step 13'
        write(nlog,*) ' DirectEx; ',
     1   ' ndtype, ipuse, divactL, iuse2, idcd2, nd2, ieff2'
        write(nlog,*) ' DirectEx; ',
     1   ndtype, ipuse, divactL*fac, iuse2, idcd2,nd2,ieff2     
      endif
      
      if(ndtype.eq.3) then
        if(ipUse.le.0) then
          CALL RTNSEC(icx,divactL,L2,iuse2,idcd2,nd2,ieff2)
        else
c	         	Diversion reuse.	Note return flows are stored in Psuply
c	         	but are not added to river system. Therefore 
c	         	no need to redivert them from system        
          CALL RtnsecR(icx,divactL,L2,iuse2,idcd2,nd2,
     1         ieff2,ipUse)
        endif
      endif
c
c_____________________________________________________________
c               Step 14; Calculate return flow obligation
c			 Note return patterns may be the default
c			 structure (iuse) or from plan data
c			 see RtnsecP
c
c	
      if(ioutP.eq.1) then
        write(nlog,*) ' '
        write(nlog,*) 'DirectEx step 14'
      endif
      
      if(iout.eq.1) write(nlog,*) '  DrectEX; iptc = ', iptc	
c     write(nlog,*) '  DrectEX; iptc = ', iptc	
c
      if(ipTC.gt.0) then      
        divactT= divactE/CuLimit
        rettot = divactT *(1.0-TcLimit)
        if(iDep.eq.0) divleft=0.0
        if(iDep.eq.1) divleft=rettot
c       write(nlog,*) ' DirectEx;  tcLimit', tclimit
        
        
        call SetTC(nlog, icx, l2, ipTC, iDep, fac, 
     1    divactT, CuFac, divleft, rettot, iopsou(4,l2),
     1    pdem(ipTC), pdrive(ipTC), csour)
      endif  

c
c_____________________________________________________________
c               Step 15; Double Check available flow at source 
c                        to see if more can be diverted        
 250  continue
      if(iout.eq.1) then
        write(nlog,*) ' '
        write(nlog,*) 'DirectEx step 15'
      endif
       
c
c rrb 2015-05-20; Check available flow from source downstream 
       CALL DNMFSO2(maxsta, AVAIL, IDNCOD, Iscd, NDNS, IMCD,
     1  cCallBy)
c
c                 Exit to warning (9999) if avail is < 0
      IF(AVAIL(IMCD).le.(-1.*small)) then
        write(nlog,*) ' '
        write(nlog,*) ' DirectEx; Step 15'
        WRITE(nlog,310) '  DirectEx_2',
     1    IYR,MON,IW,NWRORD(1,IW),L2,lr, nd2, iuse2,
     1    idcd2,IMCD,divreqX2*fac, divact1*fac, divactE*fac, 
     1    divadd*fac, avail(imcd)*fac 
     
        goto 9999
      endif
c
c_____________________________________________________________
c rrb 2007/07/03
c                 Step 16; Allow source structure to divert
c		                any water not used by exchange
c
c rrb 2008/09/22; Adjust source demand nd if it is the same
c		              structure as the destination (nd2)
      if(nd.eq.nd2) divreqX=amax1(0.0, divreqX-divactE)
c
c rrb 2015-06-16; Revise to limit source to water right
c                 that remains at the source (dcrdiv1)
cx    dcrdivT=dcrdiv1+dcrdiv2
      dcrdivT=dcrdiv1
      if(iout.eq.1) 
     1  write(nlog,*) ' DirectEx; Source Use of Exchange Right',
     1  dcrdiv1*fac, dcrdiv2*fac
c
      divact0=divact1
      divAdd=amin1(dcrdivT,
     1              divreqX, divcap(nd)-divmon(nd))      
c 
c ---------------------------------------------------------
c rrb 2008/04/23; Check available flow from source downstream    
      CALL DNMFSO2(maxsta, AVAIL, IDNCOD, Iscd, NDNS, IMCD,
     1  cCallBy)
      
      pavail=avail(imcd)      
      divAdd=amin1(divAdd, pavail)
      divAdd=amax1(0.0, divAdd)
      
      divact1=divact1+divAdd
      divcapX= divcap(nd)-divmon(nd)
      
      if(iout.eq.1) then
        write(nlog,*) ' '
        write(nlog,*) ' DirectEx; Source Use of Exchange Right',
     1  divact0*fac, divact1*fac, divAdd*fac,
     1  divact1*fac, dcrdivT*fac, divreqX*fac, divcapX*fac
      endif
c 
c ---------------------------------------------------------
c
c		              Remove additional diversion
      if(divAdd.gt.small) then
        CALL TAKOUT(maxsta, AVAIL ,RIVER ,AVINP ,QTRIBU,IDNCOD,
     1              divAdd, NDNS,  ISCD  )                     
        CALL RTNSEC(icx,divAdd,L2,iuse,ISCD,nd,ieff2)
        
        DIVMON(ND)=DIVMON(ND)+divAdd
        divcap2=divcap2-divAdd
      endif  
c
c_____________________________________________________________
c                 Step 17; Double Check available flow from
c                          destination downstream
c
      CALL DNMFSO2(maxsta, AVAIL, IDNCOD, idcd2, NDNS2, IMCD,
     1  cCallBy)
c             
c rrb 2008/06/20; Allow minor roundoff
      iavail=avail(imcd)
      IF(AVAIL(IMCD).le.(-1.*small) .and. iavail.gt.-1) then
        avail(imcd)=0.0
      endif
c             
c                 Print warning if negative available flow
      IF(AVAIL(IMCD).le.(-1.*small)) then
        if(iout.eq.1) write(nlog,*) ' DirectEx; Step 17'
        
        WRITE(nlog,310) '  DirectEx_3',
     1    IYR,MON,IW,NWRORD(1,IW),L2,lr, nd2, iuse2,
     1    idcd2,IMCD,divreqX2*fac, divact1*fac, divactE*fac, 
     1    divadd*fac, avail(imcd)*fac 
     
cx        write(nlog,320) (avail(iss)*fac,iss=1,numsta)
cx        write(nlog,330) (river(iss)*fac,iss=1,numsta)
        goto 9999
      endif
c
c _________________________________________________________
c                 Step 18; Update source data ( a diversion)
      if(idemtyp.le.3) then
        divreq(iuse)=divreq(iuse)-divact1
      else
        divreq(iuse)=amax1(0.0, divreq(iuse)-divact1)
        divsw(iuse)=divsw(iuse)-divact1
c
c rrb 01/02/25; Demand options 4 & 5               
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
c
c rrb 2007/10/01; Remove DivGW                    
c         divgw(nw)=amax1(0.0, divgw(nw)-dcux)
        endif
      endif
c
c		b. Update diversion by this structure and user
c rrb 2007/06/06; Move above to account for carrier capacity limits
cx    DIVMON(ND)=DIVMON(ND)+divact1
      USEMON(iuse)=USEMON(iuse)+divact1
c
c		c. Update diversion by this structure and user
c		   Note divact1 is the source diversion
      IF(IRTURN(iuse).ne.4) then
        QDIV(5,ISCD)=QDIV(5,ISCD)+divact1
      else
        QDIV(8,ISCD)=QDIV(8,ISCD)+divact1
      endif
c
c_____________________________________________________________
c               Step 19; Update data for a diversion destination
c
      if(ndtype.eq.3) then
        if(idemtyp.le.3) then
          divreq(iuse2)=divreq(iuse2)-divactE*OprEffT
        else
          divreq(iuse2)=amax1(0.0, divreq(iuse2)-divactE*OprEffT)
          divsw(iuse2)=divsw(iuse2)-divactE * OprEffT
c
c rrb 01/02/25; Demand options 4 & 5               
          nw2=idivco2(nd2)        
          if(nw2.gt.0) then
            if(ieffmax.le.0) then
              effd=diveff(mon,nd2)/100.
              effw=diveffw(mon,nw2)/100.
            else
              effd=effmax(nd2)/100.
              effw=effmaxw(nw2)/100.
            endif

            dcux=(divactE*OprEffT*effd)/effw
c
c rrb 2007/10/01; Remove DivGW                      
c           divgw(nw2)=amax1(0.0, divgw(nw2)-dcux)
          endif
        endif
c
c		b. Update diversion by this structure and user
        DIVMON(ND2)=DIVMON(ND2)+divactE
        USEMON(iuse2)=USEMON(iuse2)+divactE
c
      endif  


c _________________________________________________________
c
c                 Step 20; Update data for a plan destination 
c
        if(ndtype.eq.7) then     
c
c rrb 2011/08/22; Correction only include amount exchanged        
cx        psuply(np2)=psuply(np2)+divactL
cx        psuplyT(np2)=psuplyT(np2)+divactL
          psuply(np2)=psuply(np2)+divactE * OprEffT
          psuplyT(np2)=psuplyT(np2)+divactE * OprEffT         
        endif  
c _________________________________________________________
c
c                 Step 21; Update data for a reservoir destination 
      if(ndtype.eq.2) then
c
c rrb 2008/09/12; Correction      
cx      divaf=divactE*fac
        divafL=divactE*fac
        cursto(nr2)=cursto(nr2)+divafL
c
c ---------------------------------------------------------
c rrb 2006/09/25; Allow multiple accounts - Allocate
c		             Note:
c		              iResT1=0 distributes based on ownership ratio
c		              nrown1=number of accounts in this reservoir
c		              iown = first account associated with this reservoir
c		              icx  = subrouine calling accou.for       
c		              ia   = account to adjust
        if(ncarry.ne.0) ia=4
        if(ncarry.eq.0) ia=18
        icx2=124
c
c rrb 2008/09/12; Correction        
cx      nrX=nd2
        nrX=nr2
        iResT1=0
        nrown1=nro
        iownX=irow
        cresid1=cresid(nrX)
c
c ---------------------------------------------------------
c              21a. Distribute to accounts     
        call accou(maxacc, maxown, nrX, ownmon, curown, accr, ia,
     1      ownmax, iownX, nrown1, cursa, divafL, iResT1, icx2, cresid1)
c
c       write(nlog,*) ' DirectEX; nr2, ncarry, divafL',
c    1    nr2, ncarry, divafL
c     
c               qres(4  From Carrier by Sto_Exchange
c               qres(18 From river by Exch_Plan
     
        if(ncarry.eq.0) then
          qres(18,nr2)=qres(18,nr2)+divafL
        else
          qres(4,nr2)=qres(4,nr2)+divafL
        endif          
c
c               21b. Check reservoir roundoff when exiting routine
        call chekres(nlog, maxres, 1, 17, iyr, mon, nr2,nowner,
     1               curown,cursto,cresid)
      endif
c      
c _________________________________________________________
c               Step 22; Set Qdiv for the source and destination
      EffmaxT1=(100.0-OprLossC(l2,1))/100.0
      
      if(iout.eq.1) write(nlog,*) ' DirectEx; call SetQivC'
      
      call SetQdiv(nlog, nCarry, nRiver,
     1  nd2, nr2, iscd, idcd2X, idcd2C,
     1  divactE, TranLoss, EffmaxT1, OprEffT, fac, 
     1  rloss, maxsta, maxdiv, maxqdiv, qdiv, icx,
     1  internL, corid(l2))
     
      if(iout.eq.1) then
        write(nlog,*) '  DirectEx; Call SetCarry'
        call flush(nlog)
      endif  
c      
c ---------------------------------------------------------
c		            Step 23; Set Qdiv for the carrier

      if(iout.eq.1) write(nlog,*) ' DirectEx; call SetQivC'
      if(ncarry.gt.0) then   
        call setQdivC(
     1    nlog, ncarry, ncnum, nd, nd2, l2, iscd, idcd2X, idcd2C,
     1    nriver, divactE, TranLoss, EffmaxT1, 
     1    fac, maxsta, maxdiv, maxqdiv, maxopr, 
     1    intern, idvsta, qdiv, divmon, 
     1    maxrtnPP, maxplan, OprEff1, ipuse,  
     1    pctlosPP, rlossP, oprLossc,internT,
     1    icx, corid(l2))
      endif
c
c _________________________________________________________
c               Step 24; Update reuse data
c rrb 2008/02/05; Correction limit to a reservoir reuse.
c		  Note reusable return flows are set in RtnsecR      
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
c			 Note opermaxM and oprmaxA are in acft
      oprmaxM(l2)=oprmaxM(l2) - divactE*fac
      oprmaxM(l2)=amax1(oprmaxM(l2), 0.0)
      
      oprmaxA(l2)=oprmaxA(l2) - divactE*fac    
      oprmaxA(l2)=amax1(oprmaxA(l2), 0.0)
      
c
c _________________________________________________________
c		           Step 26; Set total diversion to be the amount
c		                    at the source (divact1) and the Exchange
      divactX=divact1+divactE/culimit      
c
c _________________________________________________________
c		            Step 27; Update diversion by this water right
c		   by amount at source (divact1) and by amount
c		   exchanged (divactE) befor CU adj (culimit)
      divd(lr) = divd(lr)+divactX
      divdS(l2)= divdS(l2)+divact1
c
c rrb 2008/01/08; Store Additional diversion under the Exchange
c     divdE(l2)= divdE(l2)+divactE/culimit 
c rrb 2014-06-16; Do not store additional at exchange
cx    divdE(l2)= divdE(l2)+divactE/culimit + divAdd
      divdE(l2)= divdE(l2)+divactE/culimit
c
c rrb 2007/07/03; Revise remaing decree for detailed reporting
      dcrdiv1=amax1(0.0, dcrdivS(l2)-divdS(l2))
      dcrdiv2=amax1(0.0, dcrdivE(l2)-divdE(l2))
c
      if(ioutp.eq.1) then
        write(nlog,*) ' '
        write(nlog,*) ' DrectEx;, dcrdivS(l2), dcrdivE(l2), dcrdiv1'
        write(nlog,*) ' DirectEx; ', dcrdivS(l2), dcrdivE(l2), dcrdiv1     
      endif 
      
c
c _________________________________________________________
c               Step 28; Update diversion by this Operating Rule
c		         Note only show exchanged amount, not
c                        exchanged and source
c rrb 2007/07/09; Store amount exchanged less loss so
c		  Plan reporting is correct
c     DIVO(L2)=DIVO(L2)+divactE
      divactL=divactE*OprEffT       
      DIVO(L2)=DIVO(L2)+divactE
      
c     write(nlog,*) ' DirectEx; ',
c    1 divcap(nd), divmon(nd), divCap2
c
c
c_____________________________________________________________
c               Step 29; Print detailed results if requested
c 
  260 continue
c
c ---------------------------------------------------------
c		Detailed header      
c
c rrb 2009/05/11; limit daily output to non zero values
cx    if(iout.ge.1 .and. iw.eq.ioutiw) then      
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
      
        write(nlog,280) '  DirectEx  ',     
     1    iyrmo(mon),xmonam(mon),idy, csour, cimcdR,
     1    iwx, iw,nwrord(1,iw),l2,lr,nd, iuse,
     1    nd2, ND2x,iuse2x,imcdX,imcdS, nriver, ncarry, oprEfft*100.,
     1    DIVREQx2*fac,AVAILX*fac,divaloS*fac,
     1    dcrdiv2*fac, divCU*fac,       
     1    pavail*fac,  culimit*100.,     tcLimit*100.,
     1    oprmax1*fac, pfail1,
     1    divcarry*fac, divcap1,   divcap2,pdem2*fac, dcrdiv1*fac,        
     1    divact0*fac, divAdd*fac, divact1*fac,divactE*fac,
     1    
     1    (divactE+divact1)*fac, iwhy, cwhy
        if(iout.eq.1) then
          write(nlog,*) 'DirectEX; qdiv 8, 14, & 5'
          write(nlog,*)  Qdiv(8,iscd)*fac, qdiv(14,iscd)*fac, 
     1      qdiv(5,iscd)*fac
        endif


        if(iout.eq.1) then
          if(imcd.gt.0) then
            write(nlog,281) '  DirectEx;',
     1        imcd, divactE*fac,pavail*fac, stanam1(imcd)
          else
            write(nlog,281) '  DirectEx;',
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
c rrb 2014-06-16; Revise shortage check
cx      if((divactE+small).lt.divaloS*pctE) ishort = 1  
        if((divactE+small).lt.divaloE) ishort = 1  
c
c rrb 2005/11/14; On/Off controls exchange only
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
      call chekava(20, maxsta, numsta, avail)
c
c _____________________________________________________________
c               
c               Step 3 - Check Qdiv array
c
      if(ioutQ.eq.1) then
        write(nlog,*) ' '
        write(nlog,*) ' DirectEx; Qdiv report'
        write(nlog,'(4x, 39i8)') (j, j=1,39)
        do i=1, numsta
          write(nlog,'(i5, 39f8.0)') i, (qdiv(j,i)*fac, j=1,39)
        end do
      endif
      
c
c_____________________________________________________________
c               Step 38; Return
c

      RETURN
c
c_____________________________________________________________
c               Formats
c
     
  270   format(/, 
     1  '  DirectEx (Type 24); Operation Right ID = ', a12,
     1  ' Destination Type = ', a12,
     1  ' Carrier = ',a8, ' T&C Plan = ',a3,
     1  ' Reuse Plan (Y/N) = ', a3,  ' Diversion Type = ', a12,
     1  ' Release to River = ', a12,/    
     1  '  DirectEx    iyr mon   day',
     1  ' Source ID    Min ID      ',
     1  '     Iter     Iw  Nwrord      l2      lr      nd    iuse',
     1  '     Nd2    Nd2X  Iuse2X   ImcdX   ImcdS  nRiver  nCarry',
     1  ' OprEffT DivreqX2 AvailX DivaloS Dcrdiv2   DivCU',
     1  '  Pavail CuLimit TcLimit Oprmax1  Pfail1 DivCary',
     1  ' divCap1 DivCap2   Pdem2 DcrDiv1 Divact0  DivAdd',
     1  ' Divact1 DivactE  TotDiv',
     1  '    iwhy Comment',/
     1  ' ___________ ____ ____ ____',
     1  ' ____________ ____________', 
     1  ' _______ _______ _______ _______ _______ _______ _______', 
     1  ' _______ _______ _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______ _______',
     1  ' _______ _______ _______',
     1  ' _______ __________________________')
     
c 280   FORMAT(a12, i5,1x,a4, i5, 1x, a12,13i8,23F8.0,i8,
  280   FORMAT(a12, i5,1x,a4, i5, 2(1x,a12),14i8,21F8.0,i8,
     1   1x, a48)
  281   FORMAT(a12, 143x, i8, f8.0, f8.2, 1x, a24)
  290   FORMAT(/, '  DirectEx   QDIV ',a12,/,16F7.1)
  300   FORMAT(/, '  DirectEx   QRES ',a12,/,16F7.1)
  310   FORMAT(/, '  DirectEx   Problem negative avail',/
     1  '  DirectEx    iyr  mon',
     1  '      Iw  nwrord      l2      lr     ND2   iuse2', 
     1  '   idcd2   imcd divreqX2 divact1 divactE  divAdd   avail'/  
     1  ' ___________ ____ ____', 
     1  ' _______ _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______ _______ _______',/
     1 a12, 2i5, 8i8, 20f8.0)
     
     
  320   format(/, '  DirectEx; avail  ',/,(10f10.2))
  330   format(/, '  DirectEx; river  ',/,(10f10.2))
  332   format(/, '  DirectEx; qtribu ',/,(10f10.2))
  334   format(/, '  DirectEx; qstern ',/,(10f10.2))
  340   format(/, '  DirectEx; Pavail, imcd, stanam ',
     1    f8.2, i8, 1x,a24)
  350   format(/, '  DirectEx; Problem with the exchange reach')   
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
     1    pavail*fac,   culimit*100.,     tcLimit*100.,
     1    oprmax1*fac,  pfail1,
     1    divcarry*fac, divCap1,    divCap2,pdem2*fac, dcrdiv1*fac,
     1    divact0*fac,  divAdd*fac, divact1*fac, divactE*fac, 
     1    (divactE+divact1)*fac, iwhy, cwhy
      
      
 1050 format('    Stopped in DirectEx',/,
     1       '    See the *.log file')
 1051 format('    Stopped in DirectEx')
      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)


      stop 
      END








      
      
