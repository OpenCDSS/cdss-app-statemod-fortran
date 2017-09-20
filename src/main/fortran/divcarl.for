C     Last change:  RRB  21 Feb 100   11:58 am
C
      SUBROUTINE DivcarL(IW,L2,ISHORT,divactx,ncallX)
c
c _________________________________________________________
c	Program Description
c       DivcarL; It simulates a type 45 operating rule for
c                 carrier structures with Losses
c
c _________________________________________________________
c	Update History
c
c
c rrb 2009/02/04; Revised to allow a demand limit
c	         ioprlim(l2) = 0 No miscellaneous limits
c		  If ioprlim(l2)= 0 No demand limit
c		  If ioprlim(l2)= -1 The source water right
c             is shared with an operating rule ID
c				      in iopsou(5,k)
c		  If ioprlim(l2)= 2 The demand limit for a reservoir
c		    		  is stored under a reservoir ID 
c				      in iopsou(5,k)
c		  If ioprlim(l2)= 3 The demand limit for a diversion
c		    		  is stored under a diversion ID 
c				      in iopsou(5,k)
c
c rrb 2008/07/03; Remove % ownership by setting oprpct1=1.0
c		  so that MDSA can be used
c
c rrb 2008/06/10; Allow return to river then diversion
c		  from a carrier again (internT = 1 Carrier, 2=Return)  
c		  Also redefine nCarry (see documentation)       
c
c rrb 2007/04/16  Revised to allow various owners to share in 
c		  water supply and capacity
c		  in addition to decree by doing the following:
c		  1. If sharing is used iOprlimit is set to -1.
c		  2. Oprinp.f sees this and sets Iopsou(5,l2)
c		     to the first operating rule that shares water
c		  3. Also Oprinp sets oprmax(l2,mon)=-1.0
c		  4. When a shared right is first called the water
c		     available supply is stored in Oprmax(l2,mon)
c		  5. When subsequent shared rights are called, they
c		     use the supply calculated when first called
c		     not the amount that is available.
c		  6. Also capacity is shared among owners.
c		  7. Note Execut was revised to reset Oprmax(l2,mon)
c		     to -1 when the system reoperats. 
c
c		Created on 3/24/2007 from Divcar but:
c		  1. A diversion destination with loss is OK
c		  2. Carrier losses assigned based on the Carrier
c		     structure (already included in type 11)
c		  3. Application losses return based on the 
c		     diversion less carrier losses
c		  4. Ownership % applied to a water right
c		  5. Multiple carrier loss capability
c		Note if the source is a diversion structure the
c                 diversion is limited by the structure capacity
c		  that is implied by a diversion source
c		Note if the source is a diversion water right the
c                 diversion is limited by the structure capacity
c		  that is implied by a diversion source and its
c                 water right
c		Note if the source is a reservoir water right the
c                 diversion is limited by the reservoir capacity
c                 and reservoir water right
c
c		Note if the diversion has a carrier other than one
c                 that is implied the diversion is limited by the 
c                 structure capacity
c		
c _________________________________________________________
c
c               Documentation
c            
c	
c	cCallBy	= calling routine
c      icx        subtoutine call # (45)
c      IW         : OVERALL WATER RIGHT ORDER
c      L2         : LOC. OF operation right  in opr RIGHT TABLE
c      ioprtn     : Switch for handling return flows when
c                   multiple structures exist at 1 location
c                   currently always set to 1.0 in Datinp.f
c
c      IDVSTA(L2) : STATION CODE OF WHERE DIV. RIGHT L2 LOCATES
c
c      ieff2 = 0 use always average efficiency
c            = 1 use max efficiency if ieffmax = 1
c      iresw =   destination type
c              0 diversion destination
c              1 reservoir destination

c	     irit  =  0 Source is a diversion 
c      irit  =  1 Source is a diversion right 
c	     irit  = -1 Source is a reservoir right
c
c      NS1   =  iopsou(1,l2) source location
c	 	            May be adjusted if Ndloc .ne. 0
c               + = a diversion location
c               - = a water right location
c     
c      nd2   =  destination pointer
c               Initially + = diversion
c               Initially - = reservoir
c	 	            Later diversion or reservoir pointer
c     
c	     iscd  = Source location on river
c
c      idcd2   stream ID of destination. May be one of the 
c               following (diversion, reservoir or carrier)
c      idcd2D  stream ID of destination diversion 
c      idcd2R  stream ID of destination reservoir
c      idcd2P  stream ID of destination plan
c      idcd2C  stream ID of 1'st destination carrier
c      idcd2X  stream ID of destination diversion (nd2) or reservoir
c      	       or plan (NOT CARRIER)
c
c	     isDiv = Destination Diversion location on River
c	     isRes = Destination Reservoir location on River
c
c     iOpDesR(l2)  =  Destination type
c     	 3 = diversion
c     	 2 = reservoir
c     iOpSouR(l2)  =  Source type
c     	  3 = diversion structure
c     	 13 = diversion right
c     	 12 = reservoir right
c
c      iopsou(1,l2) = source water right 
c	               + = diversion
c	               - = diversion or reservoir water right
c		 
c      iopsou(2,l2) Use by Oprinp to turn on or Off the 
c		     source right
c
c	     iopsou(3,l2) = ndLoc = Administration location
c		     (The location where the source right is
c		      located for administration. e.g. an off
c                            channel reservoir right is located at a
c                            carrier on the mainstem
c		      0 = located at the source water right
c		      + = a Res right source located at the carrier
c		      - = a Div right located at the carrier
c		     NOTE reset to 0 if diversion is the source
c
c      iplan      Plan that recieves carrier losses
c                   Note iplan=ireuse(l2)
c
c      iplnTyp(ip) Plan type must be 8 (Recharge)
c	
c
c      idemtyp   Switch set in datinp via *.ctl
c                   1=do not add demand data from *.ddm and *.wem
c                   2=add demand data from *.ddm and *.wem
c                   3=total demand provided in *.ddm 
c                   4=total demand provided in *.ddm and do not
c                     limit SW demands based on other water 
c                     supplies (e.g. wells)
c                   5=same as 4 but demand is:
c                     max(input,water right)
c
c	     Caprem1  = Remaining capacity in source (ns1)
c	     Caprem2  = Remaining capacity at destination (nd2)
c	     Caprem3  = Smallest Remaining capacity in other carriers
c      
c	     Divalo   = Actual Diversion
c	     Divreqx1 = Demand
c	     Divreqx2 = Demand + Carrier Loss
c      
c	     Demand   = demand adjustment (to account for carrier
c                              loss if not administered at the river)
c      
c                 OprLimit=additional carrier limitation (cfs)
c      
c	     OprLoss(l2) = conveyance loss from source structure (%)
c	     OprLossC(l2,i) = conveyance loss from additioal 
c                                    carriers (%)
c	     OprLost= conveyance loss (cfs)
c      
c	     OprEff1 = source Carrier Efficiency 
c                             (1.0 - OprLoss(l2)/100)
c	     OprEffT = Total Carrier Efficiency 
c      
c	     effmaxT=Transit loss for a ditch 
c      
c                   dcrdiv = decreed amount 
c                   divd   = total amount diverted under this decree
c                   (dcrdiv - divd) = decree remaining
c      
c	     DcrRem1 = Initial available decree
c	     DcrRem2 = Final Available decree
c      
c	     iopsou(4,l2)= Percent of water right owned
c	     oprPct(l2)  = percent of water right owned
c      
c	     iopsou(5,l2)	    See ioprlim(k)
c      
c	     nRiver	        Indicator a release to the River
c
c      qdiv(5, ) InBasin diversion by priority
c      qdiv(8, ) Transmountain diversion by priority
c
c      qdiv(18,is) Carried through a structure
c	     qdiv(19,is) From Carrier by Priority
c      qdiv(26,is) From River by Other
c	     qdiv(32,is) From Carrier Loss 
c	     qdiv(33,is) From River loss 
c
c     
c      qres(2  From Carrier by Priority
c      qres(4  From Carrier by Sto_Exchange
c      qres(18 From river by Exch_Plan
c	     qres(27 From Carrier Loss
c      
c	     Ritrem1    Starting available reservoir capacity
c	     Ritrem2    Ending available reservoir capacity
c
c
c ---------------------------------------------------------
c		Loss Data
c	
c	     OprLoss(l2) =  Transit loss (%) 
c	     		            Transit loss is a true loss no routing
c	     ioprloss      int(OprLoss) carrier loss switch
c	     	             	+ transit loss, maybe carrier loss
c	     	             	- 0 transit loss, maybe carrier loss
c	     TranLoss       Transit loss (fraction)
c      
c	     OprLossC(l2,i) Conveyance loss from a carrier (%)
c	     	  	          Conveyance loss gets routed to system
c	     OprLost=       conveyance loss (cfs)
c                     
c	     OprEff1        source Carrier Efficiency 
c                                 (1.0 - OprLoss(l2)/100)
c	     OprEffT        Total Carrier Efficiency 
c                      
c	     effmaxT=        Transit loss for a ditch 
c      
c	     ncnum           # of carriers
c      
c	     internT         Intervening structure type
c			                 1 = Carrier
c			                 2 = River
c
c      nCarry          0 No carrier
c	                     1 No return to River, Final Destination is
c		  	                 from a carrier
c	                     2 Return to River, Final Destination is
c                       from a carrier
c	                   	 3 Return to River, Final Destination is 
c		   	                 from the river
c
c		   ipTC            T&C plan     
c		   iplan           Reuse plan   
c _________________________________________________________
c
c	Dimensions
      include 'common.inc'
c      
      character cwhy*48, cdestyp*12, ccarry*3, cpuse*3, 
     1          cSouTyp*12, cstaidX*12, cresid1*12, cplntyp*12,
     1          ctype1*12, cshare*3, cCallBy*12, criver*12, corid1*12,
     1          cDest*12, cImcdR*12	     
       
c
c _________________________________________________________
c		Step 1; Initilize
c
c		iout = 0 no details
c		       1 details
c                    2 summary      
c		       3 Rtnmax details
c		       4 super summary
c   ioutA = 1 details on adjustments to Avail
c		ioutC = 1 details on carrier loss adjusment
c   ioutZ = 2 details on subroutine logic
c
      iout=0
      ioutiw=0
      ioutA=0
      ioutC=0
cx    ioutZ=2
      ioutZ=0
      
      ifirst=0
      
      if(ichk.eq.145) iout=2
      if(corid(l2).eq. ccall) ioutiw=iw
      
      corid1=corid(l2)
      
      
c     if(iout.eq.2 .and. iw.eq.ioutiw .and. ncallX.eq.0) then
      if(iout.eq.1) then
        write(nlog,102) corid(l2), iout, ioutiw, iw
 102    format(/, 72('_'),/ 
     1  '  DivCarL; ID = ', a12, 5i5)
      endif    
c      
c               b. Miscellaneous
      cCallBy='DivCarL     '
      
      small=0.001
      smalln=-1.0*small
c
c     write(nlog,*) '  DivcarL; iout, ioutiw, iw', iout, ioutiw, iw
c
c               c. Daily capability
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif   
          
      iwhy=0
      cwhy='NA'
      icase=0
      cdestyp='NA'
      cSouTyp='NA'      
      cStaIdX='NA'
      cDest='NA'
      cImcdR='NA'
      
      ccarry='No'      
      cpuse='No ' 
      criver='NA'     
      
      availX=-1./fac
      divcap1=-1./fac
      divcapD=-1./fac
      divcapX=-1.0/fac
      divcapY=-1.0/fac
      
      capRem1=-1./fac
      capRem2=-1./fac
      capRem3=-1./fac
      
      DcrRem1=-1./fac
      DcrRem2=-1./fac
      
      CurSto1=-1.0
      CurSto2=-1.0
      imcd=-1

      divact=0.0
      divactL=0.0
      divactx=0.0
      divAf=0.0
      divAfL=0.0
      
      oprLost=0.0
      cursa=0.0
      OprEffT=1.0
      
      resLoss=0.0
      divalo=-1.0/fac
      divreqx1=-1.0/fac
      divreqx2=-1.0/fac  
c
c rrb 2009/05/14;       
      divmore=-1.0/fac
      divmore=0.0/fac
      
      alocfsR=-1.0
      
      ritrem1=-1.0/fac
      ritrem2=-1.0/fac

      OprPct1=1.0
      iresTy1=0
      
      ISHORT=0
      nd=0
      nd2=0
      nS1=0
      
      if (intern(l2,1).ne.0) ccarry='Yes'
      cshare='No '
      if(iopsou(5,l2).lt.0) cshare='Yes'
c
      lr=l2
      lopr=0
      loprR=0
      noprS=0
      nriver=0
      iresw=-1
      iuse2=-1
      
      idcd2C=0

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
        nr=-nd
        cdestyp='Reservoir'
        cDest=cresid(nr)
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
c rrb 2009/05/03; track return flow
c     write(nlog,*) ' DivcarL; ndtype, cdest ', corid1, ndtype, cdest
cx      if(cdest.eq.'0100503_I   ') iout=3           
cx      if(cdest.eq.'0103651     ') iout=3           
      
c
c ---------------------------------------------------------
c		Source      
      iopSouR1 = iopSouR(l2)
      if(iopSouR1.eq.3)  cSouTyp='Diversion'
      if(iopSouR1.eq.13) cSouTyp='Diversion_WR'
      if(iopSouR1.eq.12) cSouTyp='Reservoir_WR'

c
c
c ---------------------------------------------------------
c		Set Admin location (ndLoc), multiple 
c               Carrier code (ncarry) and structure type 
      ndLoc=iopsou(3,l2)
c
c ---------------------------------------------------------
c rrb 2010/11/01; Carrier loss to a Plan
      cplntyp='NA          '
      iplan=ireuse(l2)
      if(iplan.gt.0) then
        cpuse='Yes'
        cplntyp='Recharge    '
        if(iplntyp(iplan).ne.8) then
          write(nlog,430) iplan
          goto 9999
        endif  
      endif  
      
      
c
c _________________________________________________________
c
c               Step 2; Branch if not on this month
c
c rrb 2004/22/96; Allow month switch
      if(imonsw(l2,mon).eq.0) then
        iwhy=1
        cwhy='Monthly switch is off'
        goto 380
      endif
c
c ---------------------------------------------------------
c		For a daily model set demand for beginning of season
      if(iday.eq.1 .and. imonsw(l2,mon).gt.0) then
        if (idy.lt.imonsw(l2,mon)) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 380
        endif  
      endif  
c
c ---------------------------------------------------------
c		For a daily model set demand for end of season
      if(iday.eq.1 .and. imonsw(l2,mon).lt.0) then
        if (idy.gt.iabs(imonsw(l2,mon))) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 380
        endif  
      endif  
      
c
c ---------------------------------------------------------
c rrb 2000/12/26; Variable efficiency capability (1=on)
      ieff2=1
      icx=45

c ---------------------------------------------------------
c		Set Carrier indicators
      ncarry=0
      if(intern(l2,1).gt.0) then
        ccarry='Yes'
        ncarry=1
      endif      
c
c ---------------------------------------------------------
c rrb 2007/06/06; Set Transit and Carrier Loss
      call SetLoss(nlog, icx, l2, fac, 
     1 maxopr,   intern,  Oprloss, OprLossC,
     1 ioprloss, nCarry,  nRiver,  ncnum, 
     1 OprLost,  OprEff1, OprEffT, TranLoss, 
     1 internT,internL,corid(l2))
c
c rrb 2008/01/03; Add release to river capability
      if(ioutZ.eq.2) Write(nlog,*) ' DivCarL; nriver = ', nriver
      if(nRiver.gt.0) cRiver=cstaid(nRiver)
c
c _________________________________________________________
c               Step 3; Set source data
c
c
c               a. Source structure (NS1) and type 
c                   - negative is a water right
c                   - positive is a diversion structure
c                  Source location (iscd)
c                  Number of downstream nodes (ndns)

      NS1  =Iopsou(1,L2)
      if(ioutZ.eq.2) then
        write(nlog,*) ' DivCarL_1; NS1, iOpSouR1', NS1, iOpSouR1
      endif
c _________________________________________________________
c
c		Step 3a; Source is a diversion structure (NS1>0)
c
c rrb 2008/09/26; Update, include iopSouR1 is the destination type
cx    if(NS1.gt.0) then
      if(NS1.gt.0 .and. iOpSouR1.eq.3) then      
        if(ioutZ.eq.2) 
     1   write(nlog,*) ' DivCarL_2; Source is a diversion structure'      
c
c rrb 2007/03/27; Adjust admin location if it is the same as the source
c			 0 = located at the source water right
c			 + = a Res right source located at the carrier
c			 - = a Div right located at the carrier

        if(iabs(ndLoc).eq.ns1) ndLoc=0     
     
        iuse1=NDUSER(ns1)
        irit=0
        ISCD=IDVSTA(NS1)
        NDNS=NDNNOD(ISCD)
        cstaidx=cstaid(iscd)
c
c rrb 2006/03/20; Set carrier if source is a diversion or 
c                 diversion water right      
        ccarry='Yes'        
        cSouTyp='Diversion'
        divcap1=divcap(NS1)
c        
        ndS=ns1
c
c rrb 2014-07-29; Cleanup an unused variable
cx      iscdS=idvsta(ndS)
        iscd=idvsta(ndS)
        IuseS=NDUSER(ndS)
c        
c ---------------------------------------------------------        
c
        
        if(idivsw(NS1).eq.0) then
          iwhy=2
          cwhy='Source Structure Off'
          goto 380
        endif  
      endif
c
c _________________________________________________________
c
c		Step 3b; Source is a diversion (iopSouR1=13) water 
c                        right (NS1<0)
c
c rrb 2008/09/26; Correction, iopDesR is the destination type
cx    if(NS1.lt.0 .and. iOpDesR1.ge.0) then      
      if(NS1.lt.0 .and. iOpSouR1.eq.13) then      
        if(ioutZ.eq.2) then
          write(nlog,*) ' DivCarL_3; Source is a diversion water right',
     1    ' operated at ndLoc = ', ndLoc      
        endif
        
        IRIT=1 
        NSR=-NS1        
c
c rrb 2006/03/20; Set carrier if source is a diversion or 
c                 diversion water right      
        ccarry='Yes'       
        cSouTyp='Diversion_WR'
c
        ndS=IDIVCO(1,NSR)
c
c rrb 2014-07-29; Cleanup an unused variable        
cx      iscdS=idvsta(ndS)    
        iscd=idvsta(ndS)    
        IuseS=NDUSER(ndS)
        iuse1=iuseS
c        
c ---------------------------------------------------------        
c rrb 2007/03/27; Adjust admin location if it is the same as the source
c			 0 = located at the source water right
c			 + = located at iopsou(3,l2) a res destination
c			 - = located at iopsou(3,l2) a div destination
        if(iabs(ndLoc).eq.ndS) ndLoc=0     
        
c        
c ---------------------------------------------------------        
c
c		Right operated at source location                
        if(ndLoc.eq.0) then
          NS1=IDIVCO(1,NSR)
          
          iuse1=nduser(ns1)
          iscd=idvsta(NS1)
          NDNS=NDNNOD(ISCD)
          divcap1=divcap(NS1) 
             
          if(ioutZ.eq.2) then      
            write(nlog,*) ' DivCarL_4; ndloc, ns1, iscd', 
     1            ndloc, ns1, iscd
          endif
        endif  
c        
c ---------------------------------------------------------        
c		Reservoir Right Source operated at diversion ndloc
        if(ndloc.gt.0) then
          NS1 = ndloc
          iuse1=nduser(ns1)
          iscd=Idvsta(ns1)
          NDNS=NDNNOD(ISCD)
          
          divcap1=divcap(NS1)                    
        endif  
c
c ---------------------------------------------------------        
c		Reservoir Right source operated at diversion ndloc
        if(ndloc.lt.0) then
          NS1 = -ndloc
          iuse1=nduser(ns1)
          ISCD=Idvsta(NS1)          
          NDNS=NDNNOD(ISCD)
          
          divcap1=divcap(NS1)                    
        endif  
c
c ---------------------------------------------------------        
c		Set pointers, regardless of where it is located               
        cstaidx=cstaid(iscd)
c        
c ---------------------------------------------------------        
c
        if(idivsw(NS1).eq.0) then
          iwhy=3
          cwhy='Source Diversion Structure Off'
          goto 380
        endif          
      endif  
c
c _________________________________________________________
c
c		Step 3c; Source is a reservoir (iOpSouR1=12) water
c                        right (NS1<0)
c			Note must have a carrier (else no reason
c			for a type 11 rule)
c
c rrb 2008/09/26; Correction, iopDesR is the destination type
cx    if(NS1.lt.0 .and. iOpDesR1.lt.0) then      
      if(NS1.lt.0 .and. iOpSouR1.eq.12) then      
        if(ioutZ.eq.2) 
     1    write(nlog,*) ' DivCarL_5; Source is a reservoir water right'
        IRIT=-1
        NSR=-NS1
        cSouTyp='Reservoir_WR'

c
c		Set NS1 based on 
c                 ndLoc=0  Reservoir right location
c                       1  Reservoir location        
c			                 -1 Diversion location 
c
c rrb 2007/03/27; Adjust admin location if it is the same as the source
        if(ndLoc.eq.nsR) ndLoc=0     

c        
c ---------------------------------------------------------        
c		Res Right operated at the right location        
        if(ndLoc.eq.0) then
          NS1 = iresco(2,NSR)
          iscd = irssta(NS1)
          NDNS=NDNNOD(ISCD)          
        endif  
c        
c ---------------------------------------------------------        
c		Res Right operated at a carrier location        
        if(ndloc.gt.0) then
          NS1 = ndloc
          iuse1=NDUSER(ns1)
          iscd=idvsta(ns1)
          NDNS=NDNNOD(ISCD)
          
          idcd2C=iscd
          iuse1=NDUSER(ns1)          
        endif  
c        
c ---------------------------------------------------------        
c		Div Right operated at a diversion location        
        if(ndloc.lt.0) then
          NS1 = -ndloc
          ISCD=Idvsta(NS1)          
          NDNS=NDNNOD(ISCD)
          idcd2C=iscd
          
          iuse1=NDUSER(ns1)          
        endif  
c        
c ---------------------------------------------------------        
c		Test location        
        if(iscd.eq.0) then
          write(nlog,*) ' DivCarL; Problem with corid ', 
     1      corid(l2),ndloc, NS1, iscd
          stop 
        endif  
        NDNS=NDNNOD(ISCD)
        cstaidx=cstaid(iscd)
      endif 
      
c
c _________________________________________________________
c               Step 4a. Set Destination (nd2)
c                   - negative is a reservoir
c                   - positive is a structure
      ND  =Iopdes(1,L2)
c
c
c _________________________________________________________
c               Step 4b; Destination is a reservoir (nd2 is negative)
       if(nd.lt.0) then
         iresw=1
c
c rrb 2009/05/03; Correction         
         nd2=-nd
         nr2=nd2
         isres = irssta(nd2)    
c
c rrb 2007/06/06; Generic Loss calculations
         idcd2X=isres          
         idcd2=isres
         cDest=cresid(nr2)
        
         if(ioutZ.eq.2) write(nlog,*) ' DivCarL; nd2, nr2, iressw(nd2)',
     1   nd2, nr2, iressw(nd2)           
         if (iressw(nd2).eq.0) then
           iwhy=4
           cwhy='Destination Reservoir is Off'
           goto 380
         endif  
c
c ---------------------------------------------------------
c rrb 2006/09/25; Allow multiple accounts - Initilize
c	 	   iResTy1=0 distributes based on ownership ratio
c		   iResTy1=-1 disbribute to one account
         nro=1
         if(iopdes(2,l2).lt.0) then
           nro=-iopdes(2,l2)
           irow=nowner(nd2)
           iResTy1=0
         endif
         
         if(iopdes(2,l2).gt.0) then
           irow=nowner(nd2)+iopdes(2,l2)-1
           nro=1
           iresTy1=-1
         endif
c      
c ---------------------------------------------------------
c               Check reservoir roundoff when entering
c		Note in1=0 into a routine, 1 out of a routine
c		     sub1 = subroutine calling chekres
         in1=0
         isub1=45
         call chekres(nlog, maxres, in1, 45, iyr, mon, nr2,nowner,
     1                curown,cursto,cresid)
       endif
c
c _________________________________________________________
c
c               Step 4c. Destination is a diversion (nd is positive)
       if(nd.gt.0) then
         nd2=nd
         isdiv = idvsta(nd2) 
         iresw=0 
c
c rrb 2007/06/06; Generic Loss calculations
         idcd2X=isdiv          
         idcd2=isdiv
         cDest=cdivid(nd2)
         
         if(idivsw(nd2).eq.0) then
           iwhy=5
           cwhy='Destination diversion is Off'
           goto 380
         endif  
C      
         IUSE=NDUSER(ND2)+Iopdes(2,L2)-1
         iuse2=iuse
         if(ioutZ.eq.2) then
           write(nlog,*) ' DivCarL_6; nd, iuse, iresw ', nd,iuse,iresw
         endif
c
c        
c ---------------------------------------------------------        
c rrb 2007/03/24; Include transit losses to a diversion
         divreqx1=divreq(iuse)
         divreqx2=divreqx1/OprEffT
               
         if(DIVREQx1.LT.small) then
           iwhy=6
           cwhy='Destination Diversion Demand (Demand1) is Zero'
           goto 380
         endif 
c        
c ---------------------------------------------------------        
c        
c
c rrb 2007/04/02; Destination is a diversion
c                 Calculate destination capacity
c rrb 2008/12/02; Correction
cx       CapRem2=(divcap(nd2) - divmon(nd2))/OprEffT        
         DivCapD=divcap(nd2)
         CapRem2=(divcap(nd2) - divmon(nd2)*OprEffT)/OprEffT        
         if(ioutZ.eq.2) then
           write(nlog,*) 
           write(nlog,*) ' DivCarL_7;   nd2 DivcapD  DivMon',
     1       ' OprEffT Caprem2'
           write(nlog,'(13x,i5, 20f8.0)') 
     1       nd2, DivCapD*fac, DivMon(nd2)*fac, OprEfft, CapRem2*fac
         endif
         
         CapRem2=amax1(0.0,CapRem2)
         
         if(CapRem2.le.small) then
           iwhy=7
           cwhy='Remaining Destination Capacity (Caprem2) is Zero'
           goto 380
         endif 
       endif
c
c _____________________________________________________________
c
c rrb 2007/06/06; Add Generic Loss Calculations
c               Step 4d; Destination is through a carrier
c		         Adjust diversion location
       if(intern(l2,1).gt.0) then
         ccarry='Yes'
         nc=intern(l2,1)
c
c rrb 2008/06/29; Correction
cx       idcd2C=idcd2        
         idcd2C=IDVSTA(nc)
       endif
      
c
c _________________________________________________________
c
c               Step 5; Set up checks on available flow (avtemp)
c                 ADD RETURN FLOW FROM PREVIOUS WATER RIGHTS
C
C------  ADD RETURN FLOW FROM PREVIOUS WATER RIGHTS
C
       DO IS=1,NUMSTA
         AVTEMP(IS)=AVAIL(IS)
       end do  
c
c
c _________________________________________________________
c
c               Step 6; Check available flow at diversion (avtemp(iscd)
       AvailX=avtemp(iscd)
       IF(AVTEMP(ISCD).le.small) then
         IF(iresw.eq.0) then
           if(IRTURN(IUSE).LE.3) ISHORT=1
         endif
      
         iwhy=8
         cwhy='Available flow (AvailX) equals zero'      
         goto 380
       endif
C
c _________________________________________________________
c		Step 7; Set Destination limits

c      
c _________________________________________________________
c
c               Step 7a. Destination is a reservoir (iresw=1)
c			 Source is a diversion (irit=0) or a 
c                        diversion right (irit =1)
c			 Set Demand based on carrier loss
c                        Limit to remaining capacity (volmax-cursto)
c
c rrb 2006/09/25; Allow multiple accounts - Demand
       if (iresw.eq.1 .and. irit.ge.0) then
         cursa=0.0
         do n=1, nro
           n1=irow+n-1
           cursa=cursa+(ownmax(n1)-curown(n1))
         end do  
         
         CapRem2=cursa/fac/OprEffT
c        write(nlog,*) ' DivCarL; Caprem2', Caprem2*fac
         IF (CapRem2.lt.small) then
           iwhy=9
           cwhy='Remaininig Destination Capacity (CapRem2) = zero'            
           GOTO 380
         endif  
         
c      
c ---------------------------------------------------------
c rrb 2004/09/03; Destination is a reservoir (iresw=1)
c		Source is a diversion (irit=0)
c		Allow carrier to a reservoir even if the 
c               current storage is above the target
c		when iressw(nd2) = 3 (see *.res documentation)
         if(iressw(nr2).eq.3) then
           divalo=amin1(cursa,volmax(nd2)-cursto(nr2))/fac
         else        
           divalo=amin1(cursa,volmax(nr2)-cursto(nr2),
     1       tarmax(nr2)-cursto(nr2))/fac
         endif
c        
c ---------------------------------------------------------
c		Destination is a reservoir, Source is a Diversion right
c		Increase Demand to reflect Carrier Loss        
         divalo=amax1(0.0,divalo)
         divreqx1=divalo
         divalo=divalo/OprEffT        
         divreqx2=divalo        
         
         IF (divalo.lt.small) then
           iwhy=10
           cwhy='Res Demand (Demand1) = zero'            
           GOTO 380
         endif  
        
c        
c ---------------------------------------------------------
c		Destination is a reservoir, Source is a Diversion right
c rrb 2006/03/20; Limit by Source (implied carrier) capacity
c        divalo=amin1(divalo, divcap(NS1)-divmon(NS1))          
         divcapX=divcap(Ns1) - divmon(Ns1)
         divCapX=amax1(divCapX,0.0)
c        
c ---------------------------------------------------------
c		Destination is a reservoir, Source is a Diversion right
c rrb 2007/04/16; Allow shared capacity when iopsou(5,l2).gt.0
c 		  Note oprpct1 is set to 1.0 (ownership is not operational)
c rrb 2009/02/04; Revise to allow iopsou(5,l2) to represent other misc limits
         if(ioprlim(l2).lt.0) then
           divCapY=divcap(ns1)*Oprpct1
           if(iopsou(5,l2).gt.0) DivCapX=amin1(divCapX, divCapY)
           
           divalo=amin1(divalo, divcapX)          
           CapRem1=divcapX
           
           IF (CapRem1.lt.small) then
             iwhy=11
             cwhy='Source Capacity (CapRem1) = zero'            
             GOTO 380
           endif          
         endif
c        
c ---------------------------------------------------------
c rrb 2006/03/20; Limit by source diversion water right  
c 		  Note oprpct1 is set to 1.0 (ownership is not operational)
c        write(nlog,*) ' DivCarL; OprPct1, irit', OprPct1, irit
         if(irit.eq.1) then
           divalo1=divalo
c
c rrb 2007/03/24; Add ownership %    
c 		  Note oprpct1 is set to 1.0 (ownership is not operational)     
           DcrRem1=amin1(dcrdiv(nsr)*OprPct1, dcrdiv(nsr)-divd(nsr))         
           DcrRem1=amax1(DcrRem1, 0.0)
           divalo=amin1(divalo, DcrRem1)
           
           if(ioutZ.eq.2) then
             write(nlog,*) ' DivCarL_8;  nsr  dcrdiv OPrPct1    divd',
     1        '  DcrRem1'       
             write(nlog,'(a10,i5,20f8.0)')
     1         '  DivCarL;' , nsr, dcrdiv(nsr)*fac, OprPct1*100.,
     1            divd(nsr)*fac, DcrRem1*fac
           endif
           
           IF (DcrRem1.lt.small) then
             iwhy=12
             cwhy='Remaining source right * Ownership (DcrRem1) = zero'            
             GOTO 380
           endif            
         endif  
     
       endif  
 
c _________________________________________________________
c
c               Step 7b. Destination is a reservoir (iresw=1)
c			                   Source is a reservoir right (irig =-1)
c			                   Set Demand based on carrier loss
c                        Limit to remaining res. capacity 
c                        (volmax-cursto)
c
c rrb 2006/09/25; Revised to work with multiple reservoir
       if (iresw.eq.1 .and. irit.lt.0) then
         cursa=0.0
         do n=1, nro
           n1=irow+n-1
           cursa=cursa+(ownmax(n1)-curown(n1))
         end do  
         
         CapRem2=cursa/fac/OprEffT
         IF (CapRem2.lt.small) then
           iwhy=13
           cwhy='Remaining Destination Capacity (CapRem2) = zero'            
           GOTO 380
         endif  
        
c
c
c ---------------------------------------------------------
c rrb 2004/09/03; Allow carrier to a reservoir even if the 
c               current storage is above the target
c		when iressw(nd2) = 3 (see *.res documentation)
c		Else limit to target and destination account
         if(iressw(nd2).eq.3) then
           divalo=amin1(cursa,volmax(nr2)-cursto(nr2))/fac
         else
           divalo=amin1(cursa,volmax(nr2)-cursto(nr2),
     1     tarmax(nr2)-cursto(nr2))/fac
           divalo=amax1(0.0,divalo)
         endif
c        
c ---------------------------------------------------------
c        	Destination is a reservoir (iresw=1)
c		Source is a reservoir right (irig =-1)

         CapRem2=(volmax(nr2)-cursto(nr2))/fac/OprEffT
         IF (CapRem2.lt.small) then
           iwhy=14
           cwhy='Remaining Max Storage (CapRem2) = zero'            
           GOTO 380
         endif  
         
         if(iressw(nd2).ne.3) CapRem2=(tarmax(nr2)-cursto(nr2))/
     1         fac/OprEffT
         IF (CapRem2.lt.small) then
           iwhy=15
           cwhy='Available Target Storage (CapRem2) = zero'            
           GOTO 380
         endif  
c
c ---------------------------------------------------------
c		Destination is a reservoir
c		Increase Demand for carrier losses
c               Note impose loss AFTER limiting by the right. 
c		            This means the decreee is located at the reservoir, 
c               not the conveyance canal
         divreqx1=divalo
c
c rrb 2007/03/24; Add ownership % 
c 		  Note oprpct1 is set to 1.0 (ownership is not operational)                 
cx       ritrem1=ritrem(nsr)
         ritrem1=ritrem(nsr)*OprPct1/fac
         divalo=amin1(divalo,ritrem1)
         divalo=divalo/OprEffT
         divreqx2=divalo
         
c        write(nlog,*) 
c    1    '  DivCarL; nsr, OprPct1, ritrem(nsr), divalo, OprEffT'
c        write(nlog,*) 
c    1    '  DivCarL;' , nsr, OprPct1, ritrem(nsr), divalo, OprEffT
         
         IF (ritrem1.lt.small) then
           iwhy=16
           cwhy='Destination Res Right (RitRem1) = zero'            
           GOTO 380
         endif  
         
cr       write(nlog,*) '  DivCarL; divreqx1, divreqx2',divreqx1, divreqx2
       endif  
c
c _________________________________________________________
c
c               Step 7c. Destination is a diversion
c		                 Source is a diversion structure (irit=0)
c                    Limit allowable diversion (divalo)
c                    to destination capacity (divcap-divmon)
       IF(iresw.eq.0 .and. IRIT.EQ.0) then
c
c rrb 2008/12/02; Correction
cx       DivcapX=(divcap(nd2) - Divmon(nd2))/OprEffT
cy         DivCapD=divcap(nd2)
cy         DivCapX=(divcap(nd2) - divmon(nd2)*OprEffT)/OprEffT        
cy         DivCapX=amax1(DivCapX, 0.0)
cy         CapRem2=DivcapX
         
         CapRem2=(divcap(nd2) - divmon(nd2)*OprEffT)/OprEffT        
         CapRem2=amax1(CapRem2, 0.0)
         
         if(ioutZ.eq.2) then
           write(nlog,*) ' DivCarL_9; nd2, DivCap, CapRem2', 
     1      nd2, DivCap(nd2)*fac, CapRem2*fac
         endif
         
cy       DIVALO=AMIN1(DIVREQx2,DivCapX)
         DIVALO=AMIN1(DIVREQx2,CapRem2)
         
         
         IF (CapRem2.lt.small) then
           iwhy=17
           cwhy='Remaining Destination capacity (CapRem2) = zero'            
           GOTO 380
         endif  
        
c
c ---------------------------------------------------------
c rrb 2006/03/20; Destination is a diversion
c		  Limit by implied carrier capacity
         divCapX=divcap(NS1) - Divmon(ns1)
         divCapX=amax1(divCapX, 0.0)
c        
c rrb 2007/04/16; Allow shared capacity when iopsou(5,l2).gt.0
c 		  Note oprpct1 is set to 1.0 (ownership is not operational)
c rrb 2009/02/04; Revise to allow iopsou(5,l2) to represent other misc limits
         if(ioprlim(l2).le.0) then
           divCapY=divcap(ns1)*Oprpct1
           if(iopsou(5,l2).gt.0) DivCapX=amin1(DivCapX, DivCapY)
           
           divalo=amin1(divalo, divcapX)          
           CapRem1=DivCapX
           
           IF (CapRem1.lt.small) then
             iwhy=18
             cwhy='Source (Carrier) Capacity (CapRem1) = zero'            
             GOTO 380
           endif  
         endif
c         
c ------ ---------------------------------------------------
c           
         divalo=amax1(0.0,divalo)
         
         IF (divalo.lt.small) then
           iwhy=19
           cwhy='Div Destination demand (Demand2) = zero'            
           GOTO 380
         endif  
        
       endif
c
c _________________________________________________________
c
c               Step 7d. Destination is a diversion and 
c                        Source is a diversion water right (irit=1)
c                        Limit allowable diversion (divalo) to 
c                        destination capacity divcap(Nd2)-divmon(nd2)
c                        and water right (dcrdiv - divd)
c                        Adjust demand by OprEff but not the right
c                        This means the decree is located at the
c                        diversion not the destination canal

       IF(iresw.eq.0 .and. IRIT.EQ.1) then
c
c rrb 2008/12/02; Correction
cx       DivcapX=(divcap(nd2) - Divmon(nd2))/OprEffT
cy         DivcapX=(divcap(nd2) - Divmon(nd2)*OprEffT)/OprEffT
cy         DivCapX=amax1(DivCapX,0.0)
cy         CapRem2=DivCapX
         CapRem2=(divcap(nd2) - Divmon(nd2)*OprEffT)/OprEffT
         CapRem2=amax1(CapRem2,0.0)
         
         if(ioutZ.eq.2) then
           write(nlog,*) ' DivCarL_10;  nd2  DivCap CapRem2'
           write(nlog,'(13x, i5, 20f8.0)') 
     1      nd2, DivCap(nd2)*fac, CapRem2*fac
         endif
         
cy         DIVALO=AMIN1(DIVREQx2,DivCapX)
         DIVALO=AMIN1(DIVREQx2,CapRem2)
         
         
c
c rrb 2006/03/20; For a water right source, limit by implied carrier (NS1)
         divcapX=divcap(ns1) - divmon(ns1)
         divcapX=amax1(divcapX,0.0)
         
c
c rrb 2007/04/16; Allow shared capacity when iopsou(5,l2).gt.0
c 		  Note oprpct1 is set to 1.0 (ownership is not operational) 
c rrb 2009/02/04; Revise to allow iopsou(5,l2) to represent other misc limits        
c rrb 2009/05/26; correction
cx       if(ioprlim(k).lt.0) then
         if(ioprlim(l2).lt.0) then
           divcapY=divcap(ns1)*Oprpct1
           if(iopsou(5,l2).gt.0) divCapX=amin1(divCapX,divcapY)
           
           divalo=amin1(divalo, divcapX)
           CapRem1=divcapX
           
           if(ioutZ.eq.2) then
             write(nlog,*) ' '
             write(nlog,*) ' DivCarL_11;  ns1  iopsou',
     1        '  divalo  divcap  divmon divcapX divcapY OprPct1 CapRem1'      
           
             write(nlog,'(13x, i5,i8, 20f8.0)') 
     1         ns1, iopsou(5,l2), divalo*fac, divcap(ns1)*fac, 
     1         divmon(ns1)*fac, divcapX*fac, divcapY*fac, 
     1         Oprpct1, CapRem1*fac
           endif
           
           IF (CapRem1.lt.small) then
             iwhy=20
             cwhy='Carrier Capacity (CapRem1) = zero'            
             GOTO 380
           endif 
         endif 
        
c
c rrb 2006/03/20; Limit by source diversion water right        
c rrb 2007/03/24; Add ownership %
cx       divalo=amin1(divalo, dcrdiv(NSR)-divd(NSR))           
         DcrRem1=dcrdiv(NSR)*OprPct1-divd(NSR)
         divalo=amin1(divalo, DcrRem1)     
         if(ioutZ.eq.2) then
           write(nlog,*) 
     1      ' DivCarL_12;  nsr dcrdiv oprpct1    divd dcrrdm1'
           write(nlog,'(11x, i5, 20f8.0)')
     1       nsr, dcrdiv(nsr)*fac, oprpct1, divd(nsr)*fac, DcrRem1*fac
         endif
           
         IF (DcrRem1.lt.small) then
           iwhy=21
           cwhy='Remaining source right (DcrRem1) = zero'            
           GOTO 380
         endif  
         
c
       endif
c
c _________________________________________________________
c
c		Step 8. Process miscellaneous limitations
c rrb 2009/02/04; Revise to allow iopsou(5,k) to be miscellaneous limits
c        
c ---------------------------------------------------------
c		8a. Limit based on a reservoir target. Note the target
c		    is treated like a volume limit for this time step
         if(ioprlim(l2).eq.2) then    
           ndY=iopsou(5,l2)   
c
c rrb 2009/03/09; Correction           
cx         divCapY=amax1(0.0, tarmax(nr2)/fac-divo(l2))         
           divCapY=amax1(0.0, tarmax(ndY)/fac-divo(l2))         
           divalo=amin1(divalo, divcapY) 
           if(iout.eq.1) then   
             write(nlog,*) 
     1         ' DivCarL; iwx, ndy, iout, cresid, tarmax, divo(l2)'
             write(nlog,*) iwx, ndy, iout, cresid(ndy), tarmax(ndy),
     1         divo(l2)*fac, divalo*fac      
           endif
         
           IF (divCapY.lt.small) then
             iwhy=22
             cwhy='Misc. Reservoir Limit (DivCapY) = zero'            
             GOTO 380
           endif  
         endif                
c        
c ---------------------------------------------------------
c		8b. Limit based on a diversion demand.
         if(ioprlim(l2).eq.3) then    
           ndY=iopsou(5,l2)   
           divCapY=amax1(0.0, divreq(ndY))         
           divalo=amin1(divalo, divcapY)   
         
           IF (divCapY.lt.small) then
             iwhy=23
             cwhy='Misc. Diversion Limit (DivCapY) = zero'            
             GOTO 380
           endif  
         endif


      
      
c *********************************      
c
c rrb 2011/10/15; Add annual limit capability
c
c _________________________________________________________
c               
c               Step 8c; Limit release (alocfs) to monthly and annual
c                       limits
c rrb 2007/10/23;
c		
         if(ioprlim(l2).eq.4 .and. iopsou(5,l2).gt.0) then
           lopr=iopsou(5,l2)
           
           divCapY=amin1(oprmaxM(lopr), oprmaxA(lopr))/fac
           divalo=amin1(divalo, divCapY)
c
c rrb 2015/06/25; Make detailed output an option
           if(iout.eq.1)
     1       write(nlog,*) ' DivCarL; Monthly or annual limit ',
     1         lopr, OprmaxM(lopr), oprmaxA(lopr)
           
           if(iout.eq.1) 
     1       write(nlog,*) ' DivCarL;',lopr, oprmaxM(lopr), 
     1         oprmaxA(lopr), alocfsR*fac

           IF (divCapY.lt.small) then
             iwhy=24
             cwhy='Monthly or Annual Limit (DivCapY) = zero'            
             GOTO 380
           endif  
         endif  
      
      
      
      
c *****************************************     
       
c
c _________________________________________________________
c
c rrb 2007/6/08; 
c		Step 9. Process carrier limitations
c rrb 2008/06/10; Update
       if(ncarry.gt.0) then
         call SetCarL(nlog, icx, l2, fac, 
     1    maxopr,  maxdiv, intern, OprLossC,
     1    ncarry,  ncnum,  noprS,  internT, 
     1    OprEff1, DivCap, DivMon, DivCarry, Divalo)
       
c      
c ---- -----------------------------------------------------
       
         CapRem3=divalo      
         IF (divCarry.lt.small) then
           iwhy=25
           cwhy='Carrier Capacity (DivCarry) = zero'            
           GOTO 380
         endif  
       endif
  
c
c _________________________________________________________
c
c		Step 10. Secondary (Operating) limit 
c		Note Oprlimit is currently set to 9999. cfs in Oprinp 
       divalo=amin1(divalo,OprLimit(l2))
       divalo=amax1(0.0,divalo)
       
       IF (divalo.lt.small) then
         iwhy=26
         cwhy='Operating Limit (Oprlimt) = zero'            
         GOTO 380
       endif  
c      
c _________________________________________________________
c
c rrb 2008/06/27; Add return to river capability
c		Step 11; Call RivRtn that will:
c		1. Adjust the demand as necessary since
c		   the return location may be upstream or downstream of 
c		   the carrier diversion
c
c !!!		Note navail=0 DOES NOT allow avail to be adjusted
       if(nRiver.gt.0) then   
c
c rrb 2010/10/15; Update to allow operationn with a depletion release
         DepfacM=1.0
         relact=0.0
         DIVALOL=DIVALO*oprEffT
         nAvail=0  
c
c rrb 2014-07-27; Check Avail
         
         if(ioutA.eq.1) then
           ifirst=0
           nchkA=1
           call ChkAvail2(nlog, ifirst, icx, nchkA, maxsta, numsta, 
     1          fac, avail)
         endif         
                            
         call RivRtn(
     1     icx, nriver, l2, ndtype, iscd, nd2, iuse2, idcd2,idcd2X, 
     1     fac, smallN, oprEffT, relact, adj, DIVALO, DIVALOL, 
     1     ncnum, nAvail, alocfsR, DepFacM, imcdR, corid1)
     
         if(imcdR.gt.0) cImcdR= cstaid(imcdR)
c
c rrb 2014-07-26       
cx       if(divact.le. small) then
         if(DIVALO.le. small) then         
           iwhy=27
           cwhy='Available flow with River Return = zero'
           goto 380
         endif  
       endif   
c      
c _________________________________________________________
c
c rrb 2008/06/10; Step 12; Call MDSA that will get the maximum
c		  by including return flows as necessary
c
c !!!		  Note AVAIL gets adjusted for diversion and return flows

         if(iout.eq.3) then
           iRtn=0
           DO NR=1,NSTRTN
             IsX=ISTRTN(NR) 
             if(cstaid(isX).eq.'0102624     ') iRtn=nR
           end do         
c
c rrb 2009/05/26; Correction
           if(iRtn.gt.0) then           
             qdivX=qdiv(21,iscd) + qdiv(23,iscd) + qdiv(26,iscd) +
     1         qdiv(29,iscd) + qdiv(30,iscd) + qdiv(31,iscd) + 
     1         qdiv(34,iscd)
             
             write(nlog,*) ' '
             write(nlog,*) ' DivCarL_0; ', corid1, iRtn
             write(nlog,*) ' DivCarL_0; ', cdest, iyr, mon, imo, 
     1         iterC, iRtn, iscd, divalo*fac, 
     1         retur(imo,iRtn)*fac, qdiv(26,iscd)*fac, qdivx*fac
           endif
         endif
c
c _________________________________________________________
c	 	Step 12b Call RtnCarry & DsaMod the first time
c		          
         call DsaMod(
     1    icx, iout, l2, imcd, iscd, ndns, nd2, iuse2, ieff2, 
     1    fac, pavail, divalo, divact, oprEffT, divactL, 
     1    iwhy, icase, ishort, iresw, cCallBy, corid1, cwhy)
     
         icX=45
       
         if(iout.eq.3 .and. iRtn.gt.0) then
           write(nlog,*) ' DivCarL_1; ', cdest, iyr, mon, imo, iterC,
     1      iRtn, divalo*fac, 
     1      retur(imo,iRtn)*fac, qdiv(26,iscd)*fac
         endif
       
c      

       nd=ndS
       iterC=0
       iterMax=20
       if(ncarry.gt.0) then
         if(iout.eq.3 .and. iRtn.gt.0) then
           write(nlog,*) ' '
           write(nlog,*) ' DivCarL_2; ', cdest, iyr, mon, imo, iterC,
     1      iRtn, divalo*fac, 
     1      retur(imo,iRtn)*fac, qdiv(26,iscd)*fac
         endif
         
         call RtnCarry(
     1     nlog,    ncarry,   ncnum,    nd,       nd2, 
     1     l2,      iscd,     idcd2X,   idcd2C,   iplan,
     1     nriver, 
     1     DIVACT,  TranLoss, EffmaxT1, fac,      maxdiv, 
     1     maxqdiv, maxopr,   intern,   idvsta,   maxrtnw, 
     1     maxdivw, OprEff1,  oprLossC, internT,  icx,
     1     corid1)
     
         if(iout.eq.3 .and. iRtn.gt.0) then
           write(nlog,*) ' DivCarL_3; ', cdest, iyr, mon, imo,iterC,
     1      iRtn, divalo*fac, 
     1      retur(imo,iRtn)*fac, qdiv(26,iscd)*fac
         endif
c
c _________________________________________________________
c	 	Step 12c Begin iteraton loop (100) since the location of return
c            flows are not known au-prior
c
     
 100     iterC=iterC+1
     
         if(iout.eq.3 .and. irtn.gt.0) then
           write(nlog,*) ' '
           write(nlog,*) ' DivCarL_4; ', cdest, iyr, mon, imo, iterC,
     1      iRtn, divalo*fac, 
     1      retur(imo,iRtn)*fac,  qdiv(26,iscd)*fac
         endif
c      
c _________________________________________________________
c 
         call RtnCarry(
     1     nlog,    ncarry,     ncnum,    nd,      nd2, 
     1     l2,      iscd,       idcd2X,   idcd2C,  iplan,
     1     nriver,  
     1     DIVMORE, TranLoss,   EffmaxT1, fac,     maxdiv,
     1     maxqdiv, maxopr,     intern,   idvsta,  maxrtnw,
     1     maxdivw, OprEff1,    oprLossC, internT, icx,
     1     corid1)    
         
         if(iout.eq.3 .and. iRtn.gt.0) then
           qdivX=qdiv(21,iscd) + qdiv(23,iscd) + qdiv(26,iscd) +
     1       qdiv(29,iscd) + qdiv(30,iscd) + qdiv(31,iscd) + 
     1       qdiv(34,iscd)
              
           write(nlog,*) ' DivCarL_5; ', cdest, iyr, mon, imo, iterC,
     1      iRtn, divalo*fac, 
     1      retur(imo,iRtn)*fac, qdiv(26,iscd)*fac, qdivx*fac
         endif
      
         divalo1=divalo-divact
         if(divalo1.gt.small) then
         
           if(iout.eq.3 .and. iRtn.gt.0) then
             write(nlog,*) ' '
             write(nlog,*) ' DivCarL_6; ', cdest, iyr, mon, imo, iterC,
     1        iRtn, divalo*fac, 
     1        retur(imo,iRtn)*fac, qdiv(26,iscd)*fac
           endif
 
         
           call DsaMod(
     1       icx, iout, l2, imcd, iscd, ndns, nd2, iuse2, ieff2, 
     1       fac, pavail, divalo1, divMore, oprEffT, divMoreL, 
     1       iwhy, icase, ishort, iresw, cCallBy, corid1, cwhy)  

           if(iout.eq.3 .and. irtn.gt.0) then
             write(nlog,*) ' DivCarL_7; ', cdest, iyr, mon, imo, iterC,
     1        iRtn, divalo*fac, 
     1        retur(imo,iRtn)*fac, qdiv(26,iscd)*fac
           endif
          
           divact1=divact
           divact=amin1(divact+divMore, divalo) 
               
           divactL=divact*oprEffT
           
           if(ioutC.eq.1 .and. divmore.gt.small) then             
             if(iterC.eq.1) write(nlog,*) 
     1       ' DivCarL;  Corid1       ', 
     1       '    Iter   IterC OprEffT  divalo divact1',
     1       ' divalo1 divmore  divact'
             write(nlog,'(12x, a12, 1x, 2i8, 20f8.0)')  
     1         corid1, iwx, iterC, oprEffT, divalo*fac, divact1*fac,
     1         divalo1*fac, divmore*fac, divact*fac
           endif
         endif
         if(divmore.gt.small .and. iterC.le.iterMax) goto 100
       endif
       
       if(ioutZ.eq.2) write(nlog,*) 
     1   ' DivCarL; Back from DsaMod divalo, divact ', 
     1   divalo*fac, divact*fac
c      
c _________________________________________________________
c	 	Step 12d Exit if no available flow 
       if(divact.lt.small) then
         iwhy=28
         cwhy='Diversion (divact) = 0'
         goto 380
       endif     
c
c _________________________________________________________
c   Step 13 Call RivRtn AGAIN to allow Avail to be adjusted 
c
c rrb 2008/06/27; Add return to river capability

c !!!		 Note:
c          navail=1 allows avail to be adjusted
c          navail=2 Adjust AVAIL to be adjused for each diversion 
c                   and release except the source diversion 
c
       if(nRiver.gt.0) then  
c
c rrb 2010/10/15; Update to allow operation with a depletion release
         DepfacM=1.0             
         relact=0.0
c
c rrb 2014-07-29; Revise to not adjust the source diversion
c                 since its already done in DSAMOD
cx       nAvail=1
         nAvail=2
c
c rrb 2014-07-27; Check Avail
         if(ioutA.eq.1) then
           nchkA=2
           call ChkAvail2(nlog, ifirst, icx, nchkA, maxsta, numsta, 
     1          fac, avail) 
         endif        
         
         call RivRtn(
     1     icx, nriver, l2, ndtype, iscd, nd2, iuse2, idcd2,idcd2X, 
     1     fac, smallN, oprEffT, relact, adj, DIVACT, DIVACTL, 
     1     ncnum, nAvail, alocfsR, DepFacM, imcdR, corid1) 
c
c rrb 2014-07-27; Check Avail
         if(ioutA.eq.1) then
           nchkA=3
           call ChkAvail2(nlog, ifirst, icx, nchkA, maxsta, numsta, 
     1          fac, avail) 
         endif        
        
       endif
c
c _________________________________________________________
c          
c               Step 14; Check for shortages and 
c                        Set Qdiv for the source and destination only
       IF(IRTURN(IUSE).LE.3) ISHORT=1
c
       EffmaxT1=(100.0-OprLossC(l2,1))/100.0   
c
c      
       call SetQdiv(nlog, nCarry, nRiver,
     1   nd2, nr2, iscd, idcd2X, idcd2C,
     1   divact, TranLoss, EffmaxT1, OprEffT, fac, 
     1   rloss, maxsta, maxdiv, maxqdiv, qdiv, icx,
     1   internL, corid1)
       
       if(ioutZ.eq.2) then
         write(nlog,*) '  DivCarL; Call SetCarry'
         call flush(nlog)
       endif  
c       
c __________________________________________________________
c	 	            Step 15; Set Qdiv for the carrier and route 
c		  carrier losses
       
c      write(nlog,*) ' DivCarL; nd, iuse', nd, iuse     
       nd=ndS
       if(ncarry.gt.0) then
c
c rrb 2009/01/24; Call SetQdivX not SetQdivC because return
c		    flows from carriers have already been calculated
c		    above in RtnCarry       
cx       call setQdivC(
         call setQdivX(
     1     nlog,    ncarry,   ncnum,    nd,       nd2, 
     1     l2,      iscd,     idcd2X,   idcd2C,   nriver,
     1     divact,  TranLoss, EffmaxT1, fac,      maxsta,
     1     maxdiv,  maxqdiv,  maxopr,   intern,   idvsta, 
     1     qdiv,    divmon,   maxRtnPP, maxPlan,  OprEff1,
     1     iplan,   pctlosPP, rlossP,   oprLossC, 
     1     internT, icx,      corid1) 
       endif
       
       if(iout.eq.3 .and. iRtn.gt.0) then
           qdivX=qdiv(21,iscd) + qdiv(23,iscd) + qdiv(26,iscd) +
     1       qdiv(29,iscd) + qdiv(30,iscd) + qdiv(31,iscd) + 
     1       qdiv(34,iscd)
        
         write(nlog,*) ' DivCarL_99; ', cdest, iyr, mon, imo, iterC,
     1    iRtn, divalo*fac, 
     1    retur(imo,iRtn)*fac, qdiv(26,iscd)*fac, qdivx*fac
       endif       

c _________________________________________________________
c		            Step 16a; Check Avail from the source (iscd) 
c                         diversion (iscd) 
  
       CALL DNMFSO(maxsta, AVAIL ,IDNCOD,ISCD  ,NDNS  ,IMCD)
       availC1=avail(imcd)
       imcd1=imcd
c
c _________________________________________________________
c
c rrb 2009/01/23; 
c		            Step 16b; Check avail from the final 
c			                    destination (idcd2X)
c
       ndnsX=ndnnod(idcd2X)
       CALL DNMFSO(maxsta, AVAIL ,IDNCOD,Idcd2X ,ndnsX  ,IMCD)
       availC2=avail(imcd)
       imcd2=imcd
       
       availC=amin1(availC1, availC2)
c
c _________________________________________________________
c		            Step 16c; Check for a negative avail
       IF(AVAILC.lT.(-1.0*small)) then
         write(nlog,390) corid1, icase, iyrmo(mon),xmonam(mon),cDest, 
     1     IW,L2,iterc, IUSE,ISCD,IMCD,imcd,
     1     DivreqX1*fac,DIVACT*fac, OprLosT*fac,
     1     availx*fac, availC1*fac       
         goto 9999
       endif
c
c _________________________________________________________
c		            Step 16c; Check for a positive avail and a constrained
c		                      shortage 
       ishortC=0
       shortC=abs(divalo-divact)
       if(shortC.gt.small) ishortC=1
       
       if(ishortC.eq.1 .and. availC.gt.small) then        
         write(nlog,392) corid1, icase, 
     1     iyrmo(mon),xmonam(mon),cDest, 
     1     IW,L2,iterc, IUSE,ISCD,idcd2X, IMCD1,imcd2, ishortC,
     1     DivreqX1*fac,divalo*fac, DIVACT*fac, shortC*fac, OprLosT*fac,
     1     availx*fac, availC1*fac, availC2*fac
     
cx         write(nlog,400) (avail(i)*fac, i=1,numsta)        
         goto 9999
       endif
c _________________________________________________________
c
c               Step 17.  Update
c      
c      
c ---- -----------------------------------------------------
c               a. Update reservoir destination data
c		   Note destination uses total Efficiency
c		   OprEffT
c                qres(2  From Carrier by Priority
c                qres(4  From Carrier by Sto_Exchange
c                qres(18 From river by Exch_Plan
       if(iresw.eq.1) then
         divaf=divact*fac
         divafL=divaf*OprEffT
         resLoss=amax1(0.0, divaf-divafL)
         cursto1=cursto(nr2)
         cursto(nr2)=cursto(nr2)+divafL
         qres(2,nr2)=qres(2,nr2)+divafL
c
c rrb 2008/09/26; do not show loss at reservoir, it is with the carrier		         
c        qres(27,nr2)=qres(27,nr2)+resLoss
         
         cursto2=cursto(nr2)
c      
c ---- -----------------------------------------------------        
c                b. Update reservoir destination data
c rrb  2006/09/25; Revised to work with multiple reservoir
c	 	  Note:
c		   iresTy1 = 0 distributes based on ownership ratio
c		   iresTy1 = -1 disbritue to 1 account
c	 	   nrown1=number of accounts in this reservoir
c	 	   iown = first account associated with this reservoir        
c	 	   icx = calling routine 2=DivCarL
c	 	   ia   = account to adjust (2=From River by Storage)
         nrown1=nro
         iownX=irow
         ia=2
         cresid1=cresid(nr2)
c
c rrb 2008/09/26; Include loss. No it is with the carrier           
         call accou(maxacc, maxown, nr2, ownmon, curown, accr, ia,
     1     ownmax, iownX, nrown1, cursa, divafL, iResTy1,icx, cresid1)

cx         call accou(maxacc, maxown, nr2, ownmon, curown, accr, ia,
cx     1     ownmax, iownX, nrown1, cursa, divafL, resLoss, iResTy1,
cx     1     icx, cresid1)
c
c               c. Check reservoir roundoff when exiting routine
c		Note in1=0 into a routine, 1 out of a routine
c		     sub1 = subroutine calling chekres
        in1=1
        isub1=45
        call chekres(nlog, maxres, in1, isub1, iyr, mon, nr2,nowner,
     1               curown,cursto,cresid)
   
       endif
c      
c ---- -----------------------------------------------------
c	 	            d. Update diversion destination data
c	 	   using total carrier efficiency (OprEffT)
       if(iresw.eq.0) then    
c      
c rrb  2007/03/23; Add Carrier Loss        
cx       USEMON(IUSE)=USEMON(IUSE)+divact
cx       DIVREQ(IUSE)=DIVREQ(IUSE)-divact
         USEMON(IUSE)=USEMON(IUSE)+divact*OprEffT
         DIVREQ(IUSE)=DIVREQ(IUSE)-divact*OprEffT
c      
         if (NS1.eq.nd2) then
           DIVMON(ND2)=DIVMON(ND2)+divact
         endif  
       endif
c      
c ---- -----------------------------------------------------
c	 	            e. Update source diversion 
c rrb 2008/11/20; If the source is a carrier divmon is adjusted in
c		    SetQdivc. 
       if(irit.eq.0) then      
         DIVMON(NS1)=DIVMON(NS1)+divact
       endif
c      
c ---- -----------------------------------------------------
c	 	            f. Update source diversion water right
       if(irit.eq.1) then      
         DIVMON(NS1)=DIVMON(NS1)+divact
c      
         dcrRem1=dcrdiv(nsr)-divd(nsr)
         divd(NSR) = divd(NSR)+divact
         dcrRem2=dcrdiv(nsr)-divd(nsr)         
       endif
c      
c ---- -----------------------------------------------------
c	 	            g. Update reservoir water right
       if(irit.eq.-1) then
         ritrem1=ritrem(NSR)/fac
         ritrem(NSR)=ritrem(NSR)-divafL
         ritrem2=ritrem(NSR)/fac
c
c rrb 2009/06/23; Adjust carrier when it is the source
cx         if(iout.eq.2 .and. iw.eq.ioutiw) then
cx           write(nlog,*) ' DivcarL; ',irit, ns1, ndloc, 
cx     1      divmon(ns1)*fac,
cx     1      divact*fac, (divmon(ns1)+divact)*fac  
cx         endif    
c
c rrb 2008/06/23; Correction adjust the diversion by a
c		    carrier (divmon(ns1) when the carrier location (ns1)
c		    is the location where the source water right 
c		    is administered iabs(ndloc)
         if(iabs(ndloc).eq.NS1) then
           DIVMON(NS1)=DIVMON(NS1)+divact
         endif         
       endif  
       
c ---- -----------------------------------------------------
c                h. Update diversion by this Operating Rule
c	 	   Note includes loss
       DIVO(L2)=DIVO(L2)+DIVACT
c      
c ---- -----------------------------------------------------
c                i. UPDATE DESTINATION DEMAND   
c		      Unless the Source (nS1) is equal to 
c		      the destination (nd2)   
c   grb 1-2-96; Bypass updating of destination demand (nD2)
c                  if it is not the same as the source (nS1)
c      INODE=IDVSTA(ND2)
       isdiv=idvsta(nd2)
c      
       if (nS1.ne.nD2) then
         if (iresw.eq.0) then
           divactL=divact*OprEffT            
           DIVMON(ND2)=DIVMON(ND2)+DIVACT
         endif  
       endif    
c        
c
c _________________________________________________________ 
c		            Step 18a; Check avail from the final 
c rrb 2009/02/04; Revise to allow iopsou(5,l2) to represent other 
c		              misc limits
c	 	             j. Update miscellaneous limit
       if(ioprlim(l2).eq.3) then    
cx         write(nlog,*) ' DivCarL_2a; ', ndy, nd2, idemtyp, 
cx     1     divreq(ndy)*fac, divact*fac
         ndY=iopsou(5,l2)  
         if(ndY.ne.nd2) then  
           if(idemtyp.le.3) then
             DIVREQ(ndY)=amax1(0.0, DIVREQ(ndY)-DIVACT)
           else
             divreq(ndY)=amax1(0.0, divreq(ndY)-divact)
             divsw(ndY)=divsw(ndY)-divact 
c
c rrb 01/02/25; Demand options 4 & 5               
             nw=idivco2(ndY)
             if(nw.gt.0) then
               if(ieffmax.le.0) then
                 effd=diveff(mon,ndY)/100.
                 effw=diveffw(mon,nw)/100.
               else
                 effd=effmax(ndY)/100.
                 effw=effmaxw(nw)/100.
               endif
             endif
             dcux=(divact*effd)/effw
           endif
         endif    
cx         write(nlog,*) ' DivCarL_2b; ', ndy, nd2, idemtyp, 
cx     1     divreq(ndy)*fac, divact*fac
       
       endif    

c        
c ---------------------------------------------------------
c rrb 2011/10/15; 
c               b. Update the monthly and annual limits plus
c                  plan data
      if(iOprLim(l2).eq.4) then
        if(iopsou(5,l2).gt.0) then
          lopr=iopsou(5,l2)
          ipLim=iopsou(1,lopr)
        
          call SetLimit(
     1      nlog, icx, lopr, ipLim, ioprlim(l2), fac, 
     1      divact, OprmaxM(lopr), OprMaxA(lopr), 
     1      Oprmax(lopr,mon), Oprmax(lopr,13), OprmaxM1, OprmaxM2, 
     1      psto1(ipLim), psto2(ipLim), corid1)
     
        endif
      endif

c _________________________________________________________
c
c		            Step 19; Update Qdiv
c
c               a. Updatetransmountain (Qdiv(8,iscd))
c	 	               where iscd is the source location
c	 	               using total carrier efficiency (OprEffT)    
       IF(iresw.eq.0) then
         if(IRTURN(IUSE).EQ.4) then
cx           QDIV(8,ISCD)=QDIV(8,ISCD)+DIVACT*OprEffT
         endif
       endif
c      
c ---- -----------------------------------------------------
c                b. Update actual diversion for testing
  380  divactx = divact
c      
c ---------------------------------------------------------
c	 	             c. Update amount lost at the carrier
c	 	                diversion (nd2>) or reservoir (ndw<0)      
c	 	                Note qdiv is by stream location (cfs)
c	 	               	qres is by reservoir in (acft)
c	 	                qdiv(33,iscd)   Source Carrier loss
c	 	                Note since at carrier use TranLoss
c           
c rrb  2008/06/27; Update (add the following)
cx       OprLost=divact*TranLoss
         if(iresw.eq.0) then
cx         qdiv(33,iscd) = qdiv(33,iscd)+OprLost
         else
c      
cx         qdiv(33,iscd) = qdiv(33,iscd)+OprLost
c      
c rrb  2006/09/25; Revised to work with multiple reservoir
c	 	  Note:
c	 	   nrown1=number of accounts in this reservoir
c	 	   iown = first account associated with this reservoir        
c	 	   icx = calling routine 2=DivCarL
c	 	   ia   = account to adjust (27=From Carrier Loss)
         nrX=nd2
         nrown1=nro
         iownX=irow
         ia=27
         cresid1=cresid(nrX)
         OprLosAF=OprLost*fac
c      

         do n=1,nrown1
           n1=iownX+n-1
           if(cursa.gt.small) then
             f=(ownmax(n1)-curown(n1))/cursa
           else
             f=0.0
           endif  
           
           c=OprLosAF*f
         end do  
       endif  
c      
c __________________________________________________________
c      
c                Step 20.  Detalied output  
cx       write(nlog,*) ' DivCarl; cdest, iwx, iout, iw, ioutiw'
cx       write(nlog,*) ' DivCarl; ', cdest, iwx, iout, iw, ioutiw
       if(iout.eq.2 .and. iw.eq.ioutiw) then
         ncallX=ncallX+1
         if(ncallX.eq.1)then
           write(nlog,270) corid(l2),cdestyp, cSouTyp, ccarry, cpuse,
     1     cplntyp, cshare               
         else
c           write(nlog,*) ' '
         endif  
       
       
         write(nlog,280) '  DivCarL   ',
     1     iyrmo(mon),xmonam(mon), idy,
     1     cDest,cimcdR, 
     1     iwx, iw,nwrord(1,iw),l2,lr,
     1     NS1, Nd2,iplan,imcd, ndloc, iscd, ioprlim(l2), nRiver,
     1     availX*fac, oprlimit(l2)*fac, OprEffT*100.,
     1     divCap1*fac, DivCapX*fac, DivCapD*fac, DivcapY*fac,
     1     CapRem1*fac, CapRem2*fac, CapRem3*fac,
     1     CurSto1,     Cursto2,
     1     ritrem1*fac, ritrem2*fac, OprPct1*100., 
     1     DcrRem1*fac,  DcrRem2*fac, divreqx1*fac, OprLoss(l2),
     1     divreqx2*fac, divalo*fac,  OprLosT*fac, opreffT*100., 
     1     divactL*fac,  divactx*fac, 
     1     icase, iwhy, cwhy
       endif
c      
c ____ _____________________________________________________
c      
c                Step 21. Set switch related to reoperation
c      
       if(divact.gt.small) iretsw=1
c      
c ____ _____________________________________________________
c                
c                Step 22; Set Call 
c rrb  2008/06/10	
c rrb 2014-07-29 No need to test if not on this month and
c                iscd, etc are not set
c
       if(iwhy.ne.1) then     
         if(iresw.eq.0) then
           ctype1='Diversion'
           call GetCall(iscd, imcdL(iscd), nd2, ctype1)        
         else
           ctype1='Reservoir'
           call GetCall(iscd, imcdL(iscd), nr2, ctype1)                
         endif    
       endif
c      
c ____ _____________________________________________________
c      
c                Step 23.  Return
       
       if(iout.eq.4 .and. divactx.gt.small) then
         iouty=iouty+1
         write(nlog,420) corid1,iouty, mon,iw,iwx,l2,nd2,divactx*fac
       endif
c      
c ____ _________________________________________________________
c                
c                Step 24 - Check Entire Avail array
c rrb  2006/06/29; Check Avail going out of the routine
c rrb 2014-07-29 No need to test if not on this month and
c                idcd2, etc are not set
       
       if(iwhy.ne.1) then  
         CALL DNMFSO(maxsta, avail, IDNCOD, idcd2, ndns2, IMCD)
c
c rrb 2008/06/15; Correct to a small negative value       
c        if(avail(imcd).le.small) then
         if(avail(imcd).le.smalln) then
           write(nlog,*) ' DivCarL; Problem imcd, avail', 
     1      imcd, avail(imcd)*fac
           goto 9999
         endif  
       endif
c      
c ____ _____________________________________________________
c      
c                Step 25.  Return       
       RETURN
c
c _________________________________________________________
c
c               Formats
c
  260   format(/, 
     1  '  DivCarL (Type 45); Problem with Operation Right ID = ', a12,/
     1  '         The source is a reservoir right but no carrier is',/
     1  '         specified. Recommend you add a carrier or use a ',/
     1  '         Standard reservoir storage right, not a type 11')


  270   format(/, 
     1  '  DivCarL (Type 45); Note:',/
     1  '    Nriver  = Carrier to river (0=no, >0=yes)',/
     1  '    Divcap1 = source capacity',/
     1  '    DivCapX = source capacity remaining with loss',/
     1  '    DivcapD = destination capacity',/
     1  '    DivcapY = miscellaneous limit (see oprlimt in *.opr)',/
     1  '    CapRem1 = source capacity',/
     1  '    CapRem2 = destination capacity after loss',/
     1  '    CapRem3 = min demand or carrier capacity',//
     1  '  -Operation Right ID = ', a12,
     1  ' -Destination Type = ', a12, ' -Source Type = ', a12,
     1  ' -Carrier (Y/N) = ',a3, ' -Plan (Y/N) = ', a3, 
     1  ' -Plan Type = ', a12,' -Shared Water ', a3/
     1  '  DivCarL     iyr mon   day',
     1  ' Dest ID      Min ID      ',
     1  '    Iter      Iw  nwrord      l2      lr     NS1     Nd2',
     1  '   iplan    imcd   ndloc    iscd ioprlim  nRiver  AvailX',
     1  ' OprLimt',
     1  ' OprEffT DivCap1 DivCapX DivCapD DivCapY CapRem1 CapRem2',
     1  ' CapRem3 CurSto1 CurSto2 RitRem1 RitRem2 OprPct1 DcrRem1',
     1  ' DcrRem2 Demand1 OprLosS Demand2  Divalo OprLosT OprEffT',
     1  ' DivActL DIVACTX   icase    iwhy Comment',/
     1  ' ___________ ____ ____ ____',
     1  ' ____________ ____________', 
     1  ' _______ _______ _______ _______ _______ _______ _______',
     1  ' _______', 
     1  ' _______ _______ _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ __________________________')
  280   FORMAT(a12, i5,1x,a4, i5, 2(1x,a12),13i8,25F8.0,2i8,1x, a48)
     
  390 FORMAT(/, 
     1  '  DivCarL; Problem ID = ', a12, ' Icase = ', i5,/
     1  '           Negative available flow',/,
     1  '  IYR  MON Dest ID      ',
     1  '      IW      L2   IterC    IUSE    iscd    imcd    imcd',
     1  '    DIVREQ    Divact   OprLosT    AvailX     Avail',/
     1  ' ____ ____ _____________',
     1  ' _______ _______ _______ _______ _______ _______ _______',
     1  ' _________ _________ _________ _________ _________',/,
     1  i5,1x,a4,1x,a12,1x, 7i8, 20F10.2)
     
  392 FORMAT(/, 
     1  '  DivCarL; Problem ID = ', a12, ' Icase = ', i5,/
     1  '           Available flow but a constrained shortage ',/ 
     1  '  IYR  MON Dest ID      ',
     1  '      IW      L2   IterC    IUSE    iscd  idcd2x',
     1  '   imcd1   imcd2  ishort',
     1  '    DIVREQ    DivAlo    Divact    ShortC   OprLosT',
     1  '    AvailX   AvailC1   AvailC2'/
     1  ' ____ ____ _____________',
     1  ' _______ _______ _______ _______ _______ _______',
     1  ' _______ _______ _______',
     1  ' _________ _________ _________ _________ _________',
     1  ' _________ _________ _________',/,
     1  i5,1x,a4,1x,a12,1x, 9i8, 20F10.2)
     
  400 format(/, 
     1 '  DivCarL: avail  ',(10f10.2))
  410 format('  DivCarL: river  ',10f10.2)
  420 format('  DivCarL; ',a12,1x,6i5,20f8.0)
     
  430 FORMAT(/, '  DivCarL; Problem the plan type must be an 8',/
     1 9x, 'for recharge, not the value specified = ', i5)
c
c _________________________________________________________
c
c               Error Warnings
c
 9999 continue 
      write(nlog,270) corid(l2),cdestyp, cSouTyp, ccarry, cpuse,
     1  cplntyp, cshare     
      
      cwhy='Problem'        
      divactx=divact
     
         write(nlog,280) '  DivCarL   ',
     1     iyrmo(mon),xmonam(mon), idy,
     1     cDest,cimcdR, 
     1     iwx, iw,nwrord(1,iw),l2,lr,
     1     NS1, Nd2,iplan,imcd, ndloc, iscd, ioprlim(l2), nRiver,
     1     availX*fac, oprlimit(l2)*fac, OprEffT*100.,
     1     divCap1*fac, DivCapX*fac, DivCapD*fac, DivcapY*fac,
     1     CapRem1*fac, CapRem2*fac, CapRem3*fac,
     1     CurSto1,     Cursto2,
     1     ritrem1*fac, ritrem2*fac, OprPct1*100., 
     1     DcrRem1*fac,  DcrRem2*fac, divreqx1*fac, OprLoss(l2),
     1     divreqx2*fac, divalo*fac,  OprLosT*fac, opreffT*100., 
     1     divactL*fac,  divactx*fac, 
     1     icase, iwhy, cwhy

      write(6,1050)
      write(nlog,1051) 
      
 1050 format(
     1 '  Stopped in DivCarL',/,
     1       '    See the *.log file')
 1051 format(/, 72('_'),/
     1 '    Stopped in DivCarL')
     
      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop 
      END
