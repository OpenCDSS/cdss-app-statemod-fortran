cc
c *********************************************************
c     Last change:  RRB  18 Dec 100    2:29 pm
c
      SUBROUTINE DirectBy(IW,L2,ISHORT,divactX,ncallx)
c
c
c _________________________________________________________
c	Program Description
c
c 	Type 25; Direct Flow Bypass
c		 It allows a water right to be diverted downstream
c		 (Bypassed) to a Diversion, Reservoir or Plan
c
c   Source 1 is a diversion water right iopsou(1
c   Destination 1 is a diversion, reservoir or plan iopdes(1
c   Source 2 can be a T&C Plan
c
c
c _________________________________________________________
c	Update History
c
c rrb 2008/06/10; Allow return to river then diversion
c		  from a carrier again (internT = 1 Carrier, 2=Return)
c		  Also redefine nCarry (see documentation)       
c
c rrb 2008/04/03; Revised to call DsaMod.
c
c rrb 2007/06/01; Revised to allow a Accounting Plan (type 11)
c		  destination
c rrb 2007/05/25; Revised to allow carrier losses
c		  NOTE THE BYPASS DEMAND IS AT THE DESTINATION
c rrb 2007/05/18; Revised to allow source to be a carrier
c		  and carrier capacity limits
c rrb 2006/09/25; Revised to allow multiple reservoir accounts
c rrb 2006/03/28; Revised to allow a plan destination
c rrb 2005/11/14; Revised so that on/off switches impact the exchange 
c               only, not the original right (see ioff)
c
c                Approach: 
c                1. Copy DirectEx and make edits 
c                2. Source is a water right (iopsou(1,l2)
c                3. Destination is a diversion (iopdes(1,l2) 
c                   or reservoir
c                4. Diversion is limited by right at source, 
c                   availability of water at source
c                5. Demand is the demand at the source.
c	            	 6. Distribute source to owners
c	            	 7. Calculate bypass potential (EP)
c	            	    If the EP > or = the owners %, bypass it
c	            	    If the EP < the owners % adjust the diversion
c	            	    at the source accordingly.
c                            8. Adjust demand at source.
c	            	 9. Adjust demand at destination or
c                               Reservoir data.
c		 
c
c		    Note may exceed demand at the source but
c                   cannot exceed the source water right
c
c _________________________________________________________
c
c               Update History
c                Note for this operating rule, the water right
c                  operates as: 
c                  1. a direct flow right and
c                  2. as an operating rule right (e.g. the right
c                     can divert at both locations.
c
c _________________________________________________________
c
c               Documentation
c
c        IW : OVERALL WATER RIGHT ORDER
c        L2 : LOC. OF operation right  in opr RIGHT TABLE
c
c        lr             source water right
c        iopsou(1,l2)   source water right
c        iopsou(2,l2)   Not used (note % ownership gets transfered
c			                  to oprpct in Oprinp.f)
c        iopsou(3,l2)   Plan id (for return flow obligation accounting)
c        iopsou(4,l2)   CU switch 
c                       0=No limit by CU, 1= limit by CU at source
c        iExPoint(l2)   bypass point (e.g. its a pointer the
c                       bypass point on the Stream
c			                  Calculated in Oprinp via call getExPt
c        iopsou(7,l2)   bypass point (e.g. its a pointer the
c
c	       oprpct(l2)     Percent of the source water right to be bypassd
c	
c
c        iopdes(1,l2)   destination diversion, plan or reservoir ID
c                       type is controlled by iopdesr() 
c        nd             source diversion ID
c        iscd           idvsta(l2) stream ID of source diversion (nd)
c        ndns           # of nodes downstream of source diversion (nd)
c        iuse          source user
c
c	       nc             Diversion for carrier #1
c
c        nd2            iopdes(1,l2) = destination ID
c                       see iopdesr() for the destination type
c
c        ndd2           iopdes(1,l2) = destination Diversion
c        np2            iopdes(1,l2) = destination plan
c        nr2            destination reservoir ID

c        idcd2          stream ID of destination diversion (nd2) or
c                         reservoir	or plan or CARRIER
c        idcd2X         stream ID of destination diversion (nd2) or 
c                         reservoir or plan (NOT CARRIER)
c        idcd2D         stream ID of destination diversion 
c        idcd2R         stream ID of destination reservoir
c        idcd2P         stream ID of destination plan
c        idcd2C         stream ID of destination carrier

c        ndns2          # of nodes downstream of destination 
c                       diversion (nd2) or reservoir or carrier
c        ndns2X         # of nodes downstream of destination 
c                       diversion (nd2) or reservoir (not a carrier)
c        iuse2          destination user 
c
c	       imcdX          pointer to avail array. It changes from 1
c                       to idcd2 (flow at destination) to imcd 
c                       (flow at the minimum location)
c
c	       internT     =  Intervening structure type
c	      	             	1 = Carrier
c	      	             	2 = River
c        icx            subroutine call # (10) for I/O in rtnsec
c        IW             Global water right counter
c        L2             Operational right pointer
c        ishort         code for reoperation; 0=no, 1=yes
c
c	       CuLimit        fraction to be diverted (diversion or depletion)
c	       TcLimit        fraction to apply to T&C requirement
c
c
c        divactB        diversion by the destination (by bypass) structure
c	       divact1        diversion at source structure
cc
c	       divreqx        Demand at source 
c        divreqx2       Demand at destination
c
c        dcrdivS        Fraction of water right available to the source
c			                  Set in oprinp
c	       divdS          Diversion at source in previous iteration (cfs)
c	       dcrdiv1        Remaining water right at source source (cfs)
c
c	       dcrdivE        Fraction of water right availabe to the ByPass
c	       divdE          Diversoin at source in previous iterations (cfs)
c	       dcrdiv2        Remaining water right at bypass (cfs)
c
c        divcap         Structure capacity
c        divmon         Capacity diverted in previous iterations
c
c        idvsta(l2)     STATION WHERE DIV. RIGHT L2 LOCATES
c
c        idivco(1,l2)  = structure associated with water right l2
c
c        ieff2         =0 always use average efficiency
c                      =1 let ieffmax control variable efficiency 
c        ioprtn         Switch for handling return flows when
c                       multiple structures exist at 1 location
c                       currently always set to 1.0 in Datinp.f
c        iout           Switch: 0 no print; 1 print details, 2 summary
c
c        ndnnod(iscd)   Number of downstream nodes
c        ndnr           Number of downstream nodes from return
c                       (ndnr = f (ndnnod)) 
c
c         nCarry         0 No carrier
c		         1 No return to River, Final Destination is
c			   from a carrier
c	                 2 Return to River, Final Destination is
c                          from a carrier
c		         3 Return to River, Final Destination is 
c			   from the river

c        qdiv(5, )      From River by priority (In Basin)
c        qdiv(8, )      From River by priority (Transmountain)
c
c        qdiv(18        Carrier passing thru a structure (e.g. divcar)
c        qdiv(20        From Carrier by Storage or Exchange (e.g. carrpl)

c        qdiv(26, )     From River by Bypass
c        qdiv(28, )     Carrier passing thru a structure by a Plan
c        qdiv(32, ) 	Carrier Loss by a carrier
c     	 qdiv(33, ) 	Carrier loss to a destination 
c
c     
c        qres(4  	From Carrier by Sto_Exchange
c        qres(18 	From river by Exch_Plan
c
c
c        currtn         Immediate return to diverting node??
c        qtribu         Tributary InFlow (baseflow point)
c        qstern         Flow at diversion (used for transmtn divs)
c        small          a small value for roundoff (0.0) concerns
c
c	       pdem(ip)       Obligation to a plan (ip)
c			                  Note calculated in RtnsecP
c
c ---------------------------------------------------------
c		Loss Data
c	       OprLoss(l2) =  Transit loss (%) 
c	     	              	Transit loss is a true loss no routing
c	       ioprloss    = int(OprLoss) carrier loss switch
c	     	              	+ transit loss, maybe carrier loss
c	     	              	- 0 transit loss, maybe carrier loss
c	       TranLoss    =  Transit loss (fraction)
         
c	       OprLossC(l2,i) = Conveyance loss from a carrier (%)
c	     	 	             Conveyance loss gets routed to system
c	       OprLost=      conveyance loss (cfs)
c              
c	       OprEff1 = 	   source Carrier Efficiency 
c                               (1.0 - OprLoss(l2)/100)
c	       OprEffT = 	   Total Carrier Efficiency 
c	       OprEffTS=     Carrier Efficiency for source 
c              
c	       effmaxT=	     Transit loss for a ditch 
c        
c	       ncarry        indicator at least 1 carrier
c	       ncnum         # of carriers
c
c _____________________________________________________________
c
c		Dimensions
      include 'common.inc'
      character 
     1  cwhy*48, cdestyp*12, ccarry*3, cpuse*3, csour*12,
     1  rec12*12, cresid1*12, cTandC*3, criver*12, 
     1  corid1*12, cCallBy*12, cstaid1*12, ctype1*12, cImcdR*12,
     1  cwhy2*48
c
c
c_____________________________________________________________
c               Step 1; Common Initilization
c
c		              iout=0 no detials
c		                   1 details
c		                   2 summary
c                      3 details for adjustment to avail for a bypass
c                 ioutX=1 detailed output specified in the control file 
c                 ioutiw=n water right for detailed output set in
c                          control file
c     write(nlog,*) ' DirectBy; nplan', nplan     

      iout=0
      ioutX=0
      ioutiw=0
      
      if(ichk.eq.125) ioutX=1
      if(corid(l2).eq. ccall) ioutiw=iw
      
      corid1=corid(l2)
      cstaid1=corid1
      cCallBy='DirectBy    '      
      
      if(corid(l2).eq. ccall) ioutiw=iw
c     iout=1
      if(ioutX.eq.1 .and. ioutiw.eq.iw .and. ncallx.eq.0) then
        write(nlog,102) corid(l2)
 102    format(/, 72('_'),/ '  DirectBy; Operating Rule ID = ', a12)
      endif         
      
      if(iout.eq.1) then
        write(Nlog,*)
     1    ' DirectBy; ncallx    ichk    iout  ioutiw      iw',
     1    ' corid        ccall' 
        write(nlog,'(10x,5i8,2(1x,a12))')  
     1    ncallx, ichk, iout, ioutiw, iw, corid(l2), ccall
      endif
      
c
c               b. Daily capability
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif

      oprEffT=1.0
      OprEffTS=1.0
      
      divact  =0.0
      divactX =0.0
      pfail1  =0.0
      ioff=0
      icase=0
      
      nr2=0
      np2=0
      
      divCap1=0.0
      divCap2=0.0
      
      ISHORT=0

      ieff2=1
      icx=25
      small = 0.001
      iscd=-1
      
      imcdX=1
      
      idcd2=0
      idcd2X=0
      idcd2D=0
      idcd2R=0
      idcd2P=0
      idcd2C=0   
c
c rrb 208/01/25; Return to River      
      nRiver=0
      criver='NA'
c
      cdestyp='NA'      
      nd2=iopdes(1,L2)

      ndtype = iopdesr(l2)
      if(ndtype.eq.2) cdestyp='Reservoir'
      if(ndtype.eq.3) cdestyp='Diversion'
      if(ndtype.eq.7) cdestyp='Plan     '      
c
c ---------------------------------------------------------
c		Output variables      
      iwhy=0
      cwhy='NA'
      iwhy2=0
      cwhy2='NA'
      cImcdR='NA'
      
      divaloS = -1.0/fac
      divByP  = -1.0/fac
      pavail  = -1.0/fac
      pdem1   = -1.0/fac
      pdem2   = -1.0/fac
      divCU   = -1.0/fac
      divCarry= -1.0/fac
      alocfsR=-1.0/fac      
      
      divactB = 0.0
c
c rrb 2008/09/03; Initilize      
      divactL = 0.0
      divact1 = 0.0
      divact0=0.0
      divMore=0.0
      divAdd=0.0
      
      
      cpuse='No'
      cTandC='No'
      csour='NA'
      nc=0
      ncnum=0
      
      if(iopsou(3,l2).gt.0) cpuse='Yes'
c
c ---------------------------------------------------------
c    Set destination type based on the value of nd2
c rrb 2011/11/27; Move above    
cx    cdestyp='NA'      
cx    nd2=iopdes(1,L2)
cx    if(nd2.gt.0 .and. nd2.lt.10000) cdestyp='Diversion'
cx    if(nd2.gt.10000) cdestyp='Plan' 
cx    if(nd2.lt.0) cdestyp='Reservoir'     
      
c
c ---------------------------------------------------------
c
c		Set monthly and annual limit (acft = cfs)      
      oprmax1=amin1(oprmaxM(l2), oprmaxA(l2))
      oprmax1=amax1(0.0, oprmax1)/fac
      
      pctE=oprPct(l2)/100.0
      pctS=1.0-pctE
      
      if(iout.eq.1) then
        write(nlog,*) '  DirectBy_0; pctE, pctS, oprmax1', 
     1    pctE, pctS, oprmax1*fac
      endif
c
c ---------------------------------------------------------
c		f. Standard Carrier   
      ncarry=0   
      ccarry='No'
      if(intern(l2,1).gt.0) then
cx        if(iout.eq.2) write(nlog,*) '  DirectBy; intern(l2,1), ', l2, 
cx     1     intern(l2,1)
        ccarry='Yes'
        ncarry=1
      endif      
c
c ---------------------------------------------------------
c rrb 2007/06/06; Set Transit and Carrier Loss

      call SetLoss(nlog, icx, l2, fac, 
     1 maxopr,   intern, Oprloss,  OprLossC,
     1 ioprloss, nCarry, nRiver,   ncnum,   
     1 OprLost,  OprEff1, OprEffT, TranLoss, 
     1 internT,  internL,corid(l2))
     
c
c rrb 2008/01/25; Return to river
      if(nriver.gt.0) cRiver=cstaid(nRiver)   
c
c ---------------------------------------------------------
c               l. Check Avail array coming in
c     if(iout.eq.1) write(nlog,*) ' DirectBy; Calling Chekava In'
      call chekava(21, maxsta, numsta, avail)
c
c _________________________________________________________
c               Step 2; Exit if not on this month (iwhy=1)
c
c rrb 04/22/96; Allow month switch
      if(imonsw(l2,mon).eq.0) then
        iwhy=1
        cwhy='Monthly Switch is Off'
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
c               Step 2b; Set Plan pointer
c		ipTC  = T&C plan
c		ipUse = Reuse plan
      ipTC=iopsou(3,l2)
      if(ipTC.gt.0) cTandC='Yes'
      ipUse=ireuse(l2)
      if(ipUse.gt.0) cpuse='Yes'
c
c ---------------------------------------------------------
c		Step 2c; Detailed header      
      if(ioutX.eq.1 .and. iw.eq.ioutiw) then      
        ncallX=ncallX+1
        if(ncallX.eq.1) then
          write(nlog,270) corid(l2), cdestyp, ccarry, cTandC, cpuse,
     1      cDivTyp(l2), pctE*100.               
        else
c         write(nlog,*) ' '
        endif  
      endif  
      
c
c _________________________________________________________ 
c               Step 3; Set Source Data
c
      lr=iopsou(1,l2)
      nd=idivco(1,lr)
c
c		Exit if source structure is off (iwhy=2)
      if(idivsw(nd).eq.0) then
        iwhy=2
        cwhy='Source Diversion is Off'        
        goto 260
      endif
c 
      ISCD=IDVSTA(ND)
      csour=cdivid(nd)
      NDNS=NDNNOD(ISCD)
      iuse=nduser(nd)
            
c     write(nlog,*) '  DirectBy_3; iscd, ndns ', iscd, ndns
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
      diveff1=diveff(mon,nd)/100.
      
c
c ---------------------------------------------------------
cr
cr rrb 2005/11/14; Add TcLimit for a source
      TcLimit= diveff(mon,nd)/100.  
      
      if(iDep.eq.0) then
        culimit=1.0
      else
c
c ---------------------------------------------------------
c		c. Use default or plan efficiency
c		Note pfail is in acft for continuity
        culimit=diveff(mon,nd)/100.
        if(ipTC.gt.0) then
          pdem1=pdem(ipTC)        
          pfail1=pfail(ipTC)
          if(peff(mon,ipTC).gt.0.) then      
            culimit=peff(mon,ipTC)/100.
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
c		note l2 is a pointer to the water right
      dcrdiv1=amax1(0.0, dcrdivS(l2)-divdS(l2))
      dcrdiv2=amax1(0.0, dcrdivE(l2)-divdE(l2))
c
c ---------------------------------------------------------
c		e. Set Capacity limits      
c rrb 2007/11/27; Adjust capacity for amount exchanged (divdE(l2)
      divCap1=divcap(nd)-divmon(nd)
      divCap2=divCap1   

c      
c ---------------------------------------------------------
c               f. Detailed output
c
      if(iout.eq.1) then
        write(nlog,*) ' '
        write(nlog,*) ' DirectBy_5; ipTC', ipTC              
        write(nlog,*) ' DirectBy_5; iprf, nd, culimit %', 
     1                 iprf(ipTC), nd, culimit*100
      endif
c
c
c _________________________________________________________
c rrb 01/06/25; 
c               Step 4a; Set destination data 
c			 Destination is a diversion
c
c rrb 2011/11/27; Revise to use ntype and iopdesr
cx    if(nd2.gt.0 .and. nd2.lt.10000) then 
      if(ndtype.eq.3) then     
        if(iout.eq.1) write(nlog,*)'  DirectBy; Diversion dest.'
      
        cdestyp='Diversion'
        ndd2=1
        nr2=0
        np2=0
c
c		Skip bypass if destination structure (nd2) is off (iwhy=3)
        if(idivsw(nd2).eq.0) then
          iwhy=3
          cwhy='Destination Diversion is Off'    
c
c		Note goto 250 since source may still divert        
c         goto 260
          goto 250                
        endif
c     
        idcd2=idvsta(nd2)        
        idcd2X=idcd2
        idcd2D=idcd2
        
        ndns2=ndnnod(idcd2)
        ndns2X=ndns2
        
        imcdX=idcd2
c
        iuse2=nduser(nd2)+iopdes(2,l2)-1 
        iuse2X=iuse2
c
        if(idemtyp.le.3) then
          divreqx2=amin1(divreq(iuse2), divcap(nd2)-divmon(nd2))
        else
          divreqx2=amin1(divsw(iuse2), divcap(nd2)-divmon(nd2))
        endif
        divreqx2=amax1(0.0, divreqx2)
      endif        
c

c
c _________________________________________________________
c rrb 2006/03/28; 
c               Step 4b; Set destination data 
c		         Destination is a plan
      nd2=iopdes(1,L2)
c 
c rrb 2006/03/29; Allow a plan destination     
c     if(nd2.gt.0) then
c rrb 2011/11/27; Revise to use ntype and iopdesr
cx    if(nd2.gt.10000) then  
      if(ndtype.eq.7) then    
        if(iout.eq.1) write(nlog,*)'  DirectBy; Plan dest.'
        cdestyp='Plan'
        ndd2=0
        nr2=0
c
c rrb 2011/11/27; Revise to use ndtype and iopdesr
cx      np2=amax0(nd2-10000,0)
        np2=nd2
        iuse2=-1
c
c		Exit if destination structure (nd2) is off
        if(pOn(np2).le.small) then
          iwhy=4
          cwhy='Destination Plan is Off'          
c
c		Note goto 250 since source may still divert        
c         goto 260
          goto 250          
        endif
c     
        idcd2=ipsta(np2)
        idcd2X=idcd2        
        idcd2P=idcd2
        
        ndns2=ndnnod(idcd2)
        ndns2X=ndns2
        
        imcdX=idcd2
c
c rrb 2007/06/01; Allow the destination to be a T&C, Well Aug
c		  or Special (10) or Admin Plan (11)
        if(iplntyp(np2) .ne. 11) then
          divreqx2=amax1(0.0, pdem(np2))
          np11=0
        else
          divreqx2=99999./fac
          np11=1
        endif
        if(iout.eq.1) write(Nlog,*)'  DirectBy; np2', pdem(np2)*fac,
     1    pdemT(np2)*fac, pdem(2)*fac, pdemT(2)*fac   
      endif        
c _________________________________________________________
c
c               Step 4c; Set destination data 
c			Destination is a reservoir
c rrb 2011/11/27; Revise to use ntype and iopdesr
cx    if (nd2.lt.0) then
      if(ndtype.eq.2) then
        if(iout.eq.1) write(nlog,*)'  DirectBy; Reservoir dest'
c
c rrb 2011/11/27; Revise to use ndtype & iopdesr()        
cx      nr2=-nd2
        nr2=nd2
        ndd2=0
        np2=0
        cdestyp='Reservoir'
c rrb 2006/09/25; Allow multiple accounts - Initilize
cr      irow=nowner(nr2)+iopdes(2,l2)-1
cr      iuse2x=irow
c     
        idcd2=irssta(nr2)
        idcd2X=idcd2        
        idcd2R=idcd2        
        
        ndns2=ndnnod(idcd2)
        ndns2X=ndns2
        
        iuse2=-1
c
c ---------------------------------------------------------
c rrb 2006/09/25; Allow multiple accounts - Initilize
c		  Note use nr2 (not nr)
        nro=1
        if(iopdes(2,l2).lt.0) then
          nro=-iopdes(2,l2)
          irow=nowner(nr2)
        endif

        if(iopdes(2,l2).gt.0) then
          irow=nowner(nr2)+iopdes(2,l2)-1
          nro=1
        endif
        
        iuse2x=irow
        
c
c ---------------------------------------------------------
c		Exit if destination structure (nd2) is off (iwhy=3)
        if(iressw(nr2).eq.0) then
          iwhy=5
          cwhy='Destination Reservoir is Off'          
c
c		Note goto 250 since source may still divert        
c         goto 260
          goto 250          
        endif
        
c
c ---------------------------------------------------------
c               b1. Set demand
c rrb 2006/09/25; Allow multiple accounts - Demand
        cursa=0.0
        do n=1, nro
          n1=irow+n-1
          cursa=cursa+(ownmax(n1)-curown(n1))
        end do  
        
cr      divreqx2=amin1(ownmax(irow)-curown(irow),
        divreqx2=amin1(cursa,
     1                 volmax(nr2)-cursto(nr2),
     1                 tarmax(nr2)-cursto(nr2))/fac
        divreqx2=amax1(0.0, divreqx2)
      endif  
c
c _____________________________________________________________
c
c               Step 4e; Destination is through a carrier
c		         Adjust diversion location to first carrier
      if(ncarry.gt.0) then
        ccarry='Yes'
        nc=intern(l2,1)
        idcd2=IDVSTA(nc)
        ndns2=NDNNOD(idcd2)        
        
        idcd2C=idcd2
c       idcd2X=idcd2
cx        if(iout.eq.2) write(nlog,*) ' DirectBy;  l2, nc,', l2, nc,
cx     1    divcap(nc)*fac, divmon(nc)*fac
        divcarry=divcap(nc)-divmon(nc)
      endif   
c
c _____________________________________________________________
c               Step 4f. Check printout 
c
c
c rrb 2009/05/11; limit daily output to non zero values
cx    if(ioutX.eq.1 .and. iw.eq.ioutiw) then      
      iprint=1
      if(iday.eq.1 .and. divactB+divact1.lt.small) iprint=0
      
      if(ioutX.eq.1 .and. iw.eq.ioutiw .and. iprint.eq.1) then      
        inout=0
        if(inout.eq.1) then
            
        write(nlog,280) '  DirectBy_1',
     1    iyrmo(mon),xmonam(mon), idy,
     1    csour,cimcdR, 
     1    iwx, iw,nwrord(1,iw),l2,lr,nd2,
     1    np2,iuse2x,imcdX,  idcd2x, idcd2C, idcd2, iscd, nriver,
     1    Divreqx*fac, DIVREQx2*fac,
     1    AVAIL(imcdX)*fac, divaloS*fac,
     1    dcrdiv2*fac, divactS*fac, divCU*fac, divByP*fac,
     1    pavail*fac,  culimit*100., oprmax1*fac, pfail1,
     1    divcarry*fac,divactB*fac, dcrdiv1*fac,  pdem1*fac,
     1    divact0*fac, divAdd*fac, divact1*fac, iwhy, cwhy
        endif
    
      endif
c
c _____________________________________________________________
c		            Step 4g; Branch to process source decree
c		                     if the destination demand for the
c                        bypass (divreqs2)  equal to zero 
c
      if(divreqx2.lt.small) then
        iwhy=7
        cwhy='Demand at destination (DivreqX2) = 0'
c
c		Note goto 250 since source may still divert        
c         goto 260
          goto 250

      endif
c
c_____________________________________________________________
c               Step 5; Begin generic water supply checks
c
      CALL DNMFSO2(maxsta, AVAIL, IDNCOD, idcd2, NDNS2, IMCD,
     1  cCallBy)
      imcdX=imcd
      availx=avail(imcd)
c             
c               Print warning if negative available flow
      IF(AVAILx.le.(-1.*small)) then
        WRITE(nlog,310) '**DirectBy_8',
     1    IYR,MON,IW,NWRORD(1,IW),L2,lr, nd2, iuse2,
     1    idcd2,IMCD,icase, divreqX2*fac, divactB*fac, avail(imcd)*fac
      endif

      if(availx.le.small) then
        iwhy=8
        cwhy='Available flow below destination (AvailX) = 0'
        goto 260
      endif
c
c_____________________________________________________________
c               Step 6; Calculate allowable diversion (divaloS)
c                       at source location based on water right
c	                      control with demand, etc later
      divaloS=amax1(0.0, dcrdiv1+dcrdiv2)
c
c		Exit if no allowable diversion 
      if(divaloS.le.small) then
        if(iout.eq.1) then
          write(nlog,*) ' DirectBy_9; dcrdiv1+dcrdiv2,divreqX2)'
          write(nlog,*) ' DirectBy_9;',(dcrdiv1+dcrdiv2)*fac,
     1                    divreqX2*fac, oprmax1*fac
        endif  
        
        iwhy=9
        cwhy='Remaining Decree (divaloS) = 0'
        goto 260
      endif
c
c
c
c_____________________________________________________________
c               Step 10 Finally calculate diversion at source (divact1)
c			limited by decree remaining, demand and capacity
      if(divCap1.le.small) then
        iwhy=10
        cwhy='Avail capacity (DivCap1) = 0' 
        goto 260
      endif
c
c_____________________________________________________________
c
      divcapY=divcap1*pctS    
      divalo=amin1(dcrdiv1, divreqX, divCapY) 
      divalo=amax1(divalo,0.0)      
      
             
      iresw=0
      call DsaMod(
     1   icx, iout, l2, imcd, iscd, ndns, nd, iuse, ieff2, 
     1   fac, pavail, divalo, divact, oprEffTS, divactL, 
     1   iwhy2, icase, ishort, iresw, cCallBy, cstaid1, cwhy2)
c
c rrb 2011/11/21; Save for printing
      pavail1=pavail
     
c
c ---------------------------------------------------------
c		Turn off iwhy & cwhy related to source diversion
      iwhy=0
      cwhy='NA'     
     
c
c rrb 2008/01/08; Update remaining decree for bypass calculations
      dcrdiv1=amax1(0.0, dcrdiv1-divact1)      
c
c
c_____________________________________________________________
c
c rrb 2007/06/06; Update amount carried to limit Exchange      
      DIVMON(ND)=DIVMON(ND)+divact1
      divCap2=divCap2-divact1
c
c rrb 2009/05/11; Correction
      divreqX=amax1(0.0, divreqX-divact1) 
c
c
c
c_____________________________________________________________
c               Step 11a Exit if monthly switch is off
      if(ioff.eq.1) then
        iwhy= 1
        cwhy='Monthly switch is off'
        goto 250
      endif  
c_____________________________________________________________
c               Step 11b Exit if T&C plan is in failure

      if(ipTC.gt.0) then
c       write(nlog,*) '  DirectBy; iptc, ipfail(iptc), pfail(iptc)'
c       write(nlog,*) '  DirectBy;', iptc, ipfail(iptc), pfail(iptc)
        if(ipfail(ipTC).eq. 1 .and. pfail(ipTC).gt.small) then
          divactB=0.0
          iwhy= 11
          cwhy='Plan Failure > 0'
c
c		Note goto 250 since source may still divert        
          goto 250
        endif  
      endif  
c
c_____________________________________________________________
c               Step 12 Check flow at the source (iscd)
c
c rrb 2008/04/03; Modified DSA Edit 5
c
c     write(nlog,*) '  DirectBy; iscd', iscd
      CALL DNMFSO2(maxsta,avail,IDNCOD,ISCD,NDNS,IMCD,
     1  cCallBy)
      if(pctE.gt.small) then
        divactS=avail(IMCD)/pctE
      else 
        divactS=avail(imcd)        
      endif
cx    write(nlog,*) '  Directby; imcd, divactS ', imcd, divactS*fac
        
      if(divactS.le.small) then
        iwhy=12
        cwhy = 'Available Flow at the Source (DivactS) = 0'
c
c		Note goto 250 since source may still divert        

        goto 250        
      endif     
c
c ---------------------------------------------------------
c		            Step 12a. Destination is a diversion (ndd2=1)
c	                        Limit to maximum bypass limit (oprmax1)
c rrb 2006/03/29; Diversion Destination
      if(ndd2.gt.0) then
        divaloB=amin1(divactS*pctE, dcrdiv2, divreqX2, 
     1                divcap(nd2) - divmon(nd2),
     1                divcap(nd)  - divmon(nd))
c
c rrb 2007/05/25; Add carrier Loss befor transfer limit      
        divaloB=divaloB/OprEffT             
        divaloB=amin1(divaloB, oprmax1)     
        divaloB=amax1(0.0, divaloB)            
        
        if(dcrdiv2.le.small) then
          iwhy=13
          cwhy = 'Carrier capacity (divCarry) = 0'
c
c		Note goto 250 since source may still divert        
          goto 250          
        endif
        
        if(divcap(nd2)-divmon(nd2).le.small) then
          iwhy=14
          cwhy = 'Remaining capacity of destination = 0'
c
c		Note goto 250 since source may still divert        
          goto 250          
        endif
        
        if(divcap(nd)-divmon(nd).le.small) then
          iwhy=15
          cwhy = 'Remaining capacity at source = 0'
c
c		Note goto 250 since source may still divert        
          goto 250          
        endif
        
        if(oprmax1.le.small) then
          iwhy=16
          cwhy = 'Monthly or Annual Limit (oprmax1) = 0'
c
c		Note goto 250 since source may still divert        
          goto 250          
        endif
          
      endif
c 
c ---------------------------------------------------------     
c		            Step 12b. Destination is a Plan (np2>0)
c                         Note np11 is a type 11 plan
c rrb 2006/03/29; Plan Destination
      if(np2.gt.0) then
        if(np11.eq.0) then
          divaloB=amin1(divactS*pctE, dcrdiv2, divreqX2, pdem(np2),
     1      divcap(nd)-divmon(nd))
        else
          divaloB=amin1(divactS*pctE, dcrdiv2, divreqX2,
     1      divcap(nd)-divmon(nd))
        endif
c
c rrb 2007/05/25; Add carrier Loss befor transfer limit      
        divaloB=divaloB/OprEffT     
c
c		Limit to annual or monthly limit (oprmax1)        
        divaloB=amin1(divaloB, oprmax1)                 
        divaloB=amax1(0.0,divaloB)
        
        if(iout.eq.1) then
          write(nlog,*) 'DirectBy; Plan Destination ',
     1      'divactS*pctE*fac, dcrdiv2*fac ',
     1      'divreqX2*fac, pdem(np2)*fac ', 
     1      'oprmax1*fac, OprEffT, divaloB*fac'
     
          write(nlog,*) 'DirectBy; Plan Destination ',
     1      divactS*pctE*fac, dcrdiv2*fac,
     1      divreqX2*fac, pdem(np2)*fac, 
     1      oprmax1*fac, OprEffT, divaloB*fac     
        endif
        
        if(oprmax1*fac.le.small) then
          iwhy=17
          cwhy = 'Monthly or Annual Limit (Oprmax1) = 0'
c
c		Note goto 250 since source may still divert        
          goto 250
        endif
        
      endif      
c 
c ---------------------------------------------------------     
c		12c. Destination is a reservoir      
c	             Limit to maximum bypass limit (oprmax1)
      if(nr2.gt.0) then
        divaloB=amin1(divactS*pctE, dcrdiv2, divreqX2)
c
c rrb 2007/05/25; Add carrier Loss befor transfer limit      
        divaloB=divaloB/OprEffT     
        
        divaloB=amin1(divaloB, oprmax1)         
        divaloB=amax1(0.0,divaloB)
c       write(nlog,*) '  DirectBy; Res Destination l2, mon, oprmax',
c    1    l2, mon, 
c    1    oprmax1*fac, divaloB*fac
c
c ---------------------------------------------------------
c		13d. Exit if bypass = 0      
        if(divaloB.le.small) then
          if(dcrdiv2.le.small) then
            iwhy=18
            cwhy = 'Carrier capacity (divCarry) = 0'
          endif
          
          if(oprmax1.le.small) then
            iwhy=19
            cwhy = 'Monthly or Annual Limit (Oprmax1) = 0'
          endif
c
c		Note goto 250 since source may still divert        
          goto 250
        endif      
      endif        
c   
c_____________________________________________________________
c rrb 2007/06/06; 
c               Step 13; Process carrier limitations
c	                ncarry is indicator at least 1 carrier
c	                ncnum is the number of carriers
c	                OprEff1 is the lost (oprlost(lw)
c	                Divalo gets reduced by carrier capacity
c	                DivCarry is the limitating carrier capacity
c	                noprS is the structure id of the structure
c                 that supplied water to the accounting
c		              plan that already has a capacity 
c			adjustment
      if(ncarry.gt.0) then
        norpS=0 
        if(iout.eq.1) then
          write(nlog,*) ' DirectBy; into Setcarl, ncarry',ncarry,
     1                  divaloB*fac
        endif
        
        call SetCarL(nlog, icx, l2, fac, 
     1    maxopr,  maxdiv, intern, OprLossC,
     1    ncarry,  ncnum,  noprS,  internT,
     1    OprEff1, DivCap, DivMon, DivCarry, DivaloB)

c       write(nlog,*) ' DirectBy; out of Setcarl, ncarry',ncarry
        if(iout.eq.1) then
          write(nlog,*) ' DirectBy; After carrier limit', 
     1    divCarry*fac, divaloB*fac
        endif
c
c ---------------------------------------------------------
c		13b. Exit if bypass = 0      
        if(divaloB.le.small) then
          iwhy=20
          cwhy = 'Carrier capacity (divCarry) = 0'
c
c		Note goto 250 since source may still divert        
          goto 250
        endif       
      endif
c
c ---------------------------------------------------------
c		13b. Exit if bypass = 0      
      if(divaloB.le.small) then
        iwhy=21
        cwhy = 'Bypass = 0'
c
c		Note goto 250 since source may still divert        
        goto 250
      endif  

c
c
c_____________________________________________________________
c rrb 01/06/25;
c               Step 14; For a Bypass find the minimum available
c                        from the source (iscd) downstream
c                        (divByP) at node imcd2
c rrb 2011/11/21; Correction for a bypass (the source is upstream)
cx    call dnmfso2(maxsta, avail, idncod, idcd2,ndns2,imcd,
cx   1  cCallBy)
      call dnmfso2(maxsta, avail, idncod, iscd, ndns, imcd,
     1  cCallBy)
     
      divByP=avail(imcd)
c        
c		Exit if no available flow
      if(divByP.lt.small) then
        iwhy=22
        cwhy='Flow at Bypass Point (DivByP) = 0'
c
c		Note goto 250 since source may still divert        
        goto 250
      endif
          

c_____________________________________________________________
c               Step 15; Calculate amount to be bypassd
      divCU=divactS*pctE*culimit
      divactB=amin1(divactS*pctE*culimit, divByP, divaloB, dcrdiv2)
      divactB=amax1(0.0, divactB)      
c
c rrb 2008/01/09; Update available flow to bypass
      divmon(nd)=divmon(nd)+divactB
      divcap2=divcap2-divactB
      dcrdiv2=amax1(0.0, dcrdiv2-divactB)      

c
c rrb 2008/01/25; Add Carrier Loss                       
      divactL=divactB*OprEffT  
c
c		Exit if no available flow 
      if(divactB.lt.small) then
        iwhy=23
        cwhy='Bypass Flow = 0'
        goto 250
      endif     
c
c
c _________________________________________________________
c
c               Step 16; Adjust for water dumped to the river 
c		                     by a carrier (if nriver>0).
c		                     Call RivRtn that will:
c		                     1. Adjust the diversion as necessary since
c		                        the return location may be upstream or 
c                           downstream of the carrier diversion
c		                     2. Add the river return to Avail
c		                     3. Remove the ultimate destination from Avail
c		                     Note navail=1 allows avail to be adjusted
cx      if(iout.eq.3) write(nlog,*) '  DivactB_1; avail(13), 49', 
cx     1  avail(13)*fac, avail(49)*fac 
      if(nRiver.gt.0) then   
        relact=0.0
c        write(nlog,*) ' DirectBy; Calling RivRtn OprEfft, divactE ',
c     1     oprefft, divactE*fac
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
     1    fac, smallN, oprEffT, relact, adj, divactB, divactL, 
     1    ncnum, nAvail, alocfsR, DepfacM, imcdR, corid1)
     
        if(imcdR.gt.0) cImcdR= cstaid(imcdR)
        
        ndnsBy=ndns-ndns2
 
        if(divactB.le. small) then
          iwhy=24
          cwhy='Available flow with River Return = 0'
          goto 250
        endif  
      endif      
      
cx      if(iout.eq.3) write(nlog,*) '  DivactB_2; avail(13), 49',
cx     1  avail(13)*fac, avail(49)*fac       
c     write(nlog,*) ' DirectBy; ', divactB*fac
c
c		Exit if no available flow 
      if(divactB.lt.small) then
        iwhy=25
        cwhy='Bypass Flow = 0'
        goto 250
      endif     

c
c _________________________________________________________
c          
c               Step 17 Remove destination from avail
c			            if not done in Rivrtn (nriver>0)
      if(nriver.eq.0) then
c
c rrb 2011/05/25; Correction divert from carrier if appropriate 
        if(idcd2C.eq.0) then
          CALL TAKOUT(maxsta,AVAIL,RIVER,AVINP,QTRIBU,IDNCOD,
     1                divactB,ndns2X,idcd2X)     
          ndnsBy=ndns-ndns2x
        else
          CALL TAKOUT(maxsta,AVAIL,RIVER,AVINP,QTRIBU,IDNCOD,
     1                divactB,ndns2,idcd2)
          ndnsBy=ndns-ndns2
        endif 
c
c               Endif for nriver       
      endif  
           
c         
c ---------------------------------------------------------
c rrb 2011/11/27; Limit a bypass to the flow at the source right
c		              to the carrier or diversion location.  This is 
c                 required if nriver is on (1) or off (0).  
c         
c ---------------------------------------------------------
      if(iout.eq.3) then
        write(nlog,*) ' '
        write(nlog,*) ' DirectBy; iscd, idcd2x, idcd2 '
        write(nlog,*) ' DirectBy;', iscd, idcd2x, idcd2                       
        write(nlog,*) ' DirectBy; ndns, ndns2, ndns2x, ndnsBy'    
        write(nlog,*) ' DirectBy;', ndns, ndns2, ndns2x, ndnsBy                     
        write(nlog,320) (avail(is)*fac, is=1,numsta)
      endif
c       
      if(ndnsBy.gt.0) then  
        iss=iscd
        do i=1,ndnsBy
          avail(iss) = avail(iss) - divactB   
          ISS=IDNCOD(ISS)            
        end do    
      endif 
c
      if(iout.eq.3) write(nlog,320) (avail(is)*fac, is=1,numsta)
       
c
c ---------------------------------------------------------
c		              Step 17a; DIVERSION DESTINATION add return flows
C		                        No reuse (ipuse=0) 
      if(ndd2.gt.0) then  
        if(ipUse.le.0) then
          CALL RTNSEC(icx,divactL,L2,iuse2,idcd2,nd2,ieff2)
        else
c         
c	              	Note return flows are stored in Psuply but
c	              	not added to river system. Therefore 
c	              	no need to redivert them from system  
          CALL RtnsecR(icx,divactL,L2,iuse2,idcd2,nd2,
     1         ieff2,ipUse)
        endif
      endif  
      
cx      write(nlog,*) '  DivactB_4; avail(13), 49', avail(13)*fac,
cx     1  avail(49)*fac         
c         
c ---------------------------------------------------------
c		            Step 17b; RESERVOIR DESTINATION (nd2<0)
c		            Note return flows are stored in Psuply but
c		            not added to river system. Therefore 
c		            no need to redivert them from system  
      if(nr2.gt.0 .and. ipUse.gt.0) then  
        ircp=ipsta(ipUse)
c
c
c rrb 2007/05/25; Adjust for transit and carrier Loss               
cx      psuply(ipUse)=psuply(ipUse)+divactB
cx      psuplyT(ipUse)=psuplyT(ipUse)+divactB
cx      psto2(ipUse)=psto2(ipUse)+divactB*fac
          
        divactL=divactB*OpreffT                        
        psuply(ipUse)=psuply(ipUse)+divactL
        psuplyT(ipUse)=psuplyT(ipUse)+divactL
        psto2(ipUse)=psto2(ipUse)+divactL*fac
          
        ipsta1=ipsta(ipUse)
      endif

c_____________________________________________________________
c                 Step 18; Calculate return flow obligation
c		            	 Note return patterns may be the default
c		            	 structure (iuse) or from plan data
c		            	 see RtnsecP
c	
      if(ipTC.gt.0) then      
        divactT= divactB/CuLimit
        rettot = divactT *(1.0-TcLimit)
        if(iDep.eq.0) divleft=0.0
        if(iDep.eq.1) divleft=rettot
        
c       write(nlog,*) ' DirectBy;  tcLimit', tclimit

        call SetTC(nlog, icx, l2, ipTC, iDep, fac, 
     1    divactT, CuFac, divleft, rettot, iopsou(4,l2),
     1    pdem(iptC), pdrive(ipTC), csour)
      endif  
      
c
c_____________________________________________________________
c rrb 2007/07/03
c               Step 19 Allow source structure to divert
c			            any water not used by the bypass
c		              Note similar to Step 10 but less limits
c		              dcrdiv1 & 2=remaining decree at source and bypass
c		              divactS = Total Supply
c                 
c		              dcrdiv1 = remaining decree at source
c		              dcrdiv2 = remaining decree at bypass
c		              dcrdivT = total remaining decree
c			
     	
 250  dcrdivT=dcrdiv1+dcrdiv2
c
c ---------------------------------------------------------
c rrb 2008/09/22; Adjust source demand nd if it is the same
c		  structure as the destination (nd2)
      if(nd.eq.nd2) divreqX=amax1(0.0, divreqX-divactB)
      
      divactS1=amax1(0.0, divactS-divact1-divactB)
      divact0=divact1
      divAdd=amin1(divactS1, dcrdivT,
     1              divreqX, divcap(nd)-divmon(nd)) 
      if(iout.eq.1) then
        write(nlog,*) ' DirectBy; ', divAdd*fac, divactS1*fac,
     1  divreqX*fac, (divcap(nd)-divmon(nd))*fac
      endif
c
c ---------------------------------------------------------
c rrb 2008/03/28; Check available flow (again) 
      CALL DNMFSO2(maxsta,avail,IDNCOD,ISCD,NDNS,IMCD,
     1  cCallBy)
      
      PAVAIL=avail(IMCD)
      divAdd=amin1(divAdd, pavail)         
      divAdd=amax1(0.0, divAdd)
      
      divact1=divact1+divAdd
c
c ---------------------------------------------------------
c     
      if(iout.eq.1) then
        write(nlog,*) ' DirectBy; Source Use of ByPass Right',
     1    dcrdiv1*fac, dcrdiv2*fac, dcrdivT*fac,
     1    divact0*fac, divAdd*fac, divact1*fac 
      endif
c
c ---------------------------------------------------------
c
      if(divAdd.gt.small) then
        CALL TAKOUT(maxsta, AVAIL ,RIVER ,AVINP ,QTRIBU,IDNCOD,
     1              divAdd, NDNS,  ISCD  )                     
        CALL RTNSEC(icx,divAdd,L2,iuse,ISCD,nd,ieff2)
        DIVMON(ND)=DIVMON(ND)+divAdd
      endif  
c
c_____________________________________________________________
c               Step 20; Double Check available flow 
c
      CALL DNMFSO2(maxsta, AVAIL, IDNCOD, idcd2, NDNS2, IMCD,
     1  cCallBy)
     
      if(iout.eq.1) write(nlog,*) '  DirectBy; Past 250', imcd
c     if(iout.eq.1) write(6,*) '  DirectBy; Past 250', imcd
      call flush(6)
      call flush(nlog)

c             
c rrb 2008/06/20; Allow minor roundoff
      iavail=avail(imcd)
      IF(AVAIL(IMCD).le.(-1.*small) .and. iavail.gt.-1) avail(imcd)=0.0
c             
c               Print warning if negative available flow      
      IF(AVAIL(IMCD).le.(-1.*small)) then
        WRITE(nlog,310) ' DirectBy_20',
     1    IYR,MON,IW,NWRORD(1,IW),L2,lr, nd2, iuse2,
     1    idcd2,IMCD,icase, divreqX2*fac, divactB*fac, avail(imcd)*fac
     
c       write(nlog,320) (avail(iss)*fac,iss=1,numsta)
c       write(nlog,330) (river(iss)*fac,iss=1,numsta)
        goto 9999
      endif
c
c _________________________________________________________
c               Step 21; Update source diversion data
      if(iout.eq.1) write(nlog,*) '  DirectBy; Update Source Data'
      call flush(nlog)

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
c ---------------------------------------------------------
c
c		b. Update diversion by this source structure and user
c
c rrb 2007/05/18; Move above to account for carrier capacity limits
cx    DIVMON(ND)=DIVMON(ND)+divact1
      USEMON(iuse)=USEMON(iuse)+divact1
c
c		c. Update diversion by this source structure and user
cx !!!! TEST
      IF(IRTURN(iuse).ne.4) then
        QDIV(5,ISCD)=QDIV(5,ISCD)+divact1
      else
        QDIV(8,ISCD)=QDIV(8,ISCD)+divact1
      endif
c
c
c_____________________________________________________________
c               Step 22a; Update destination 
c                        Destination is a Diversion
c
      if(ndd2.gt.0) then
        if(iout.eq.1) write(nlog,*) '  DirectBy; Update Div Data'
        call flush(nlog)
        
        if(idemtyp.le.3) then
          divreq(iuse2)=divreq(iuse2)-divactB*OprEffT
        else
          divreq(iuse2)=amax1(0.0, divreq(iuse2)-divactB*OprEffT)
          divsw(iuse2)=divsw(iuse2)-divactB*OprEffT 
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

            dcux=(divactB*OPrEffT*effd)/effw
c
c rrb 2007/10/01; Remove DivGW                      
c           divgw(nw2)=amax1(0.0, divgw(nw2)-dcux)
          endif
        endif
c
c ---------------------------------------------------------
c		b. Update diversion by this structure and user
c		Note includes loss
        DIVMON(ND2)=DIVMON(ND2)+divactB
        USEMON(iuse2)=USEMON(iuse2)+divactB
      endif
c
c_____________________________________________________________
c               Step 22b; Update destination 
c                        Destination is a plan
c
c ---------------------------------------------------------
c rrb 2007/05/25; Add carrier Loss (replace divactB with divactB*OPrEffT
      if(np2.gt.0) then
        if(iout.eq.1) write(nlog,*) '  DirectBy; Update Plan Data'      
        call flush(nlog)
c
c rrb 2011/08/22; Correction only include amount exchanged   
cx      psuply(np2)=psuply(np2) + DivactL
cx      psuplyT(np2)=psuplyT(np2) + divactL
        psuply(np2)=psuply(np2) + DivactB * OprEffT
        psuplyT(np2)=psuplyT(np2) + divactB * OprEffT        
      endif
c _________________________________________________________
c
c               Step 22c; Update Destination data 
c                        Destination is a Reservoir
c
c ---------------------------------------------------------
c rrb 2007/05/25; Add carrier Loss (replace divaf with divafL
      if(nr2.gt.0) then
        if(iout.eq.1) write(nlog,*) '  DirectBy; Update Res Data'
        call flush(nlog)
        divaf=divactB*fac
c
c ---------------------------------------------------------
c rrb 2007/05/25; Add carrier Loss      
cx      cursto(nr2)=cursto(nr2)+divaf
        divafL=divaf*OprEffT
        cursto(nr2)=cursto(nr2)+divafL
c
c ---------------------------------------------------------
c rrb 2006/09/25; Allow multiple accounts - Allocate
c		  Note:
c		   iResT1=0 distributes based on ownership ratio
c		   nrown1=number of accounts in this reservoir
c		   iown = first account associated with this reservoir
c		   icx  = subrouine calling accou.for       
c		   ia   = account to adjust
        if(nc.ne.0) ia=4
        if(nc.eq.0) ia=18
        icx2=125
        nrX=nd2
        iResT1=0
        nrown1=nro
        iownX=irow
        cresid1=cresid(nrX)
c
    
        call accou(maxacc, maxown, nrX, ownmon, curown, accr, ia,
     1    ownmax, iownX, nrown1, cursa, divafL, iResT1, icx2, cresid1)
c     
c               qres(4  From Carrier by Sto_Exchange
c               qres(18 From river by Exch_Plan
     
        if(nc.eq.0) then
          qres(18,nr2)=qres(18,nr2)+divafL
        else
          qres(4,nr2)=qres(4,nr2)+divafL
        endif          
        
c
c               b. Check reservoir roundoff when exiting routine
        call chekres(nlog, maxres, 1, 17, iyr, mon, nr2,nowner,
     1               curown,cursto,cresid)
      endif
c
c _________________________________________________________
c               Step 24; Update Qdiv and Carrier for all destinations
c rrb 2007/05/25; Add carrier Loss      
c
c ---------------------------------------------------------
c		Step 24a; Set Qdiv for the source and destination
      EffmaxT1=(100.0-OprLossC(l2,1))/100.0    
        
      call SetQdiv(nlog, nCarry, nRiver,
     1  nd2, nr2, iscd, idcd2X, idcd2C,
     1  divactB, TranLoss, EffmaxT1, OprEffT, fac, 
     1  rloss, maxsta, maxdiv, maxqdiv, qdiv, icx,
     1  internL, corid(l2))
     
      if(iout.eq.1) then
        write(nlog,*) ' DirectBy_1; Call SetQdivC ', divactB*fac
        call flush(nlog)
      endif  
c      
c ---------------------------------------------------------
c		Step 24b; Update Qdiv for the carrier
      if(ncarry.gt.0) then
        call SetQdivC(
     1    nlog, ncarry, ncnum, nd, nd2, l2, iscd, idcd2X,idcd2C,
     1    nriver, divactB, TranLoss, EffmaxT1, 
     1    fac, maxsta, maxdiv, maxqdiv, maxopr, 
     1    intern, idvsta, qdiv, divmon, 
c
c rrb 2009/06/09; Correction     
cx   1    maxrtnw, maxdivw, OprEff1, ipuse,
     1    maxrtnPP, maxplan, OprEff1, ipuse,        
     1    pctlosPP, rlossP, oprLossc,internT,
     1    icx, corid(l2))
      endif
     
      if(iout.eq.1) then
        write(nlog,*) ' DirectBy_2; Past SetQdivC ', divactB*fac
        call flush(nlog)
      endif  
c
c _________________________________________________________
c               Step 25; Update maximum diversion rate (Oprmax1)
      oprmaxM(l2)=oprmaxM(l2) - divactB*fac
      oprmaxM(l2)=amax1(0.0, oprmaxM(l2))
      
      oprmaxA(l2)=oprmaxA(l2) - divactB*fac    
      oprmaxA(l2)=amax1(0.0, oprmaxA(l2))
      
c
c _________________________________________________________
c		Step 26; Set total diversion
      divactX=divact1+divactB/culimit      
c
c _________________________________________________________
c		Step 27; Update diversion by this water right
c		   by amount at source (divact1) and by amount
c		   bypassd (divactB) befor CU adj (culimit)
      divd(lr) = divd(lr)+divactX
      divdS(l2)= divdS(l2)+divact1
c
c rrb 2008/01/08; Correction divAdd is additional water diverted
c		  under the exchange decree
c     divdE(l2)= divdE(l2)+divactB/culimit
      divdE(l2)= divdE(l2)+divactB/culimit+divAdd
c
c rrb 2007/07/03; Update for detailed reporting
      dcrdiv1=amax1(0.0, dcrdivS(l2)-divdS(l2))
      dcrdiv2=amax1(0.0, dcrdivE(l2)-divdE(l2))
      
c
c _________________________________________________________
c               Step 28; Update diversion by this Operating Rule
c		         Note only show bypassd amount, not
c                        bypassd and source
      divactL=divactB*OprEffT       
      DIVO(L2)=DIVO(L2)+divactB
c
c
c_____________________________________________________________
c               Step 30; Print detailed results if requested
c 
  260 continue
c     IF(-IOPOUT.eq.ISCD .or. iout.ge.1) then
      if(ioutX.eq.1 .and. iw.eq.ioutiw) then      
        write(nlog,280) '  DirectBy  ',
     1    iyrmo(mon),xmonam(mon),idy, 
     1    csour, cimcdR, 
     1    iwx, iw,nwrord(1,iw),l2,lr,nd2, 
     1    np2,iuse2x,imcdX,  idcd2x, idcd2C, idcd2, iscd, nriver,
     1    divreqx*fac, DIVREQx2*fac,divcap1*fac,
     1    AVAIL(imcdX)*fac,divaloS*fac,
     1    dcrdiv2*fac, divactS*fac, divCU*fac,       divByP*fac, 
     1    pavail1*fac,  culimit*100,     oprmax1*fac, pfail1,
     1    divcarry*fac,divactB*fac, pdem2*fac, dcrdiv1*fac,        
     1    divact0*fac, divAdd*fac, divact1*fac, iwhy, cwhy

        if(iout.eq.1) then
          if(imcd.gt.0) then
            write(nlog,281) '  DirectBy_xx',
     1        imcd, divactB*fac,pavail*fac, stanam1(imcd)
          else
            write(nlog,281) '  DirectBy_xx',
     1        imcd, divactB*fac,pavail*fac
          endif
        endif  
      endif
c    
c_____________________________________________________________
c               Step 31; Set return switch (iretsw), shortage (ishort) 
c                 switch and actual diversion (divact)
      if(nd2.gt.0) then
        if(divactB.gt.small) iretsw=1
        if((divactB+small).lt.divaloS*pctE) ishort = 1  
      endif  
c
c _________________________________________________________
c               
c               Step 32; Set Call for 
c rrb 2008/06/10	      
      if(nd.gt.0) then
        ctype1='Diversion'
        call GetCall(iscd, imcdL(iscd), nd, ctype1)        
      endif  
c
c _____________________________________________________________
c               
c               Step 33 - Check Avail going out of the routine
      if(iout.eq.1) write(nlog,*) ' DirectBy; Calling Chekava Out'
      call chekava(21, maxsta, numsta, avail)
      
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
     1  '  DirectBy (Type 25); Operation Right ID = ', a12,
     1  ' Destination Type = ', a12,
     1  ' Carrier (Y/N) = ',a3, ' T&C Plan (Y/N) = ',a3,
     1  ' Reuse Plan (Y/N) = ', a3,
     1  ' Diversion Type = ', a12, ' Ownership % ', f8.2/    
     1  '  DirectBy    iyr mon   day',
     1  ' Source ID    Min ID      ',
     1  '    Iter      Iw  nwrord      l2      lr     Nd2',
     1  '     np2  iuse2X   imcdX  idcd2X  idcd2C   idcd2    iscd',
     1  '  nRiver',
     1  ' DivReqX DivReqX2 divcap1 availX divaloS dcrdiv2',
     1  ' divactS   divCU',
     1  '  divByP  pavail culimit oprmax1  pfail1 divCarry DIVACTB',
     1  ' dcrdiv1   PdemX divact0 divAdd DivAct1    iwhy Comment',/
     1  ' ___________ ____ ____ ____',
     1  ' ____________ ____________', 
     1  ' _______ _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______ _______ _______', 
     1  ' _______ _______ _______ _______ _______ _______ _______',
     1  ' _______ _______',
     1  ' _______ _______ _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ __________________________')
     
  280   FORMAT(a12, i5,1x,a4, i5, 2(1x,a12),14i8,20F8.0,i8,1x,
     1   1x, a48)
  281   FORMAT(a12, 143x, i8, f8.0, f8.2, 1x, a24)
  290   FORMAT(/, '  DirectBy_0 QDIV ',a12,/,16F7.1)
  300   FORMAT(/, '  DirectBy_0 QRES ',a12,/,16F7.1)
  310   FORMAT(/, '  DirectBy; Problem negative avail',/
     1  '  DirectBy    iyr  mon',
     1  '      Iw  nwrord      l2      lr     ND2   iuse2', 
     1  '   idcd2    imcd   icase divreqx divactB   avail'/  
     1  ' ___________ ____ ____', 
     1  ' _______ _______ _______ _______ _______ _______', 
     1  ' _______ _______ _______ _______ _______ _______',/
     1 a12, 2i5, 9i8, 20f8.0)
     
     
  320   format(/, '  DirectBy_0; avail  ',/,(10f10.2))
  330   format(/, '  DirectBy_0; river  ',/,(10f10.2))
  332   format(/, '  DirectBy_0; qtribu ',/,(10f10.2))
  334   format(/, '  DirectBy_0; qstern ',/,(10f10.2))
  340   format(/, '  DirectBy_0; Pavail, imcd, stanam ',
     1    f8.2, i8, 1x,a24)
  350   format(/, '  DirectBy; Problem with the bypass reach')   
c
c rrb 2007/05/25; Add carrier Loss      
 400  format('  DirectBy;   i ncar',
     1          '  OprLossC  effmaxTX  effmaxT1',
     1          '    divcap   divcapX   CapRemX')         
 410  format(a10, 2i5, 20f10.0)   
c
c_____________________________________________________________
c               Error warnings
c
 9999 write(6,1050) 
      write(nlog,1051) 
c
c ---------------------------------------------------------
c		Print detailed output if a problem      
      write(nlog,270) corid(l2), cdestyp, ccarry, cTandC, cpuse,
     1      cDivTyp(l2), pctE*100.    
      
      write(nlog,280) '  DirectBy  ',
     1  iyrmo(mon),xmonam(mon),idy, csour, cimcdR,
     1  iwx, iw,nwrord(1,iw),l2,lr,nd2, 
     1  np2,iuse2x,imcdX, idcd2x, idcd2C, idcd2, iscd, nriver,
     1  divreqx*fac, DIVREQx2*fac, divcap1*fac,
     1  AVAIL(imcdX)*fac,divaloS*fac,
     1  dcrdiv2*fac, divactS*fac, divCU*fac,       divByP*fac, 
     1  pavail*fac,  culimit*100,     oprmax1*fac, pfail1,
     1  divcarry*fac,divactB*fac, pdem2*fac, dcrdiv1*fac,        
     1  divact0*fac, divAdd*fac, divact1*fac, iwhy, cwhy
      
      
 1050 format('    Stopped in DirectBy',/,
     1       '    See the *.log file')
 1051 format('    Stopped in DirectBy')
      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)


      stop 
      END



      
 
