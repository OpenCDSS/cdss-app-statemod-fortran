c     Last change:  RRB  22 Oct 2002    4:12 pm
c
c
      subroutine DivResR(iw,l2,ishort,divact,ncallX)
c
c
c _________________________________________________________
c	Program Description
c
c       DivResR; Type 32
c       It simulates releases from a Reservoir and a Plan to a
c           downstream Diversion, Reservoir, or Carrier by the river
c
c           Called by Execut
c _________________________________________________________
c	Update history
c
c	2007/06/06;	Add Loss 
c	05/03/31	Copied Divres
c			Removed ability to relase for depletion	only
c                       Removed ability to be called by replacement res
c			Removed stuff related to a transfer limit 
c			Removed refrences to transmtn (IRTURN(IUSE).EQ.4)
c			Added Reservoir Reuse Plan as a source 2 option
c			
c
c  ________________________________________________________
c	Documentation
c
c           divalo = demand
c           relalo = available supply in reservoir (acft)
c           alocfsR= available supply in reservoir (cfs)
c           alocfsP= available supply in reuse plan (cfs)
c           divact = actual amount diverted
c           relace = actual amount released from the reservoir
c
c           icx   = subroutine call #
c
c           ieff2 = 0 always use average efficiency
c           ieff2 = 1 use max efficiency if ieffmax=1
c           iopsou(1,l2) = source reservoir #1 
c           iopsou(2,l2) = source reserovir #1 account
c               Note the following source 2 data is only used when
c               releases are tied to another type 6 operating rule  
c           iopsou(3,l2) = if > 0 source reservoir #2    
c           iopsou(4,l2) = if > 0 source reservoir #2 account
c
c           iopsou(5,l2) = not used
c           iopsou(6,l2) = Reservoir release type and efficiency
c                          if = 0 release to meet demand
c                          if > 0 release only if a CIR (IWR) exists
c                                 and limit release to not exceed
c                                 IWR/n,  
c                          Note nmax = min(nmax, effmax) to save 
c                                 iterating
c           ireltyp        same as iopsou(6,l2)
c
c           iopdes(1,l2) = destination (+=diversion, -=reservoir)
c           iopdes(2,l2) = destination user (account)
c
c           iout         = 0 no detailed printout
c                          1 yes detailed printout
c			                     2 yes summary
c
c           nr           = source reservoir
c           iown         = source reservoir #1 account
c
c           nrD          = destination reservoir
c           irow        = destiantion account

c           iscd        = Stream ID of Destination diversion, 
c		                       or reservoir or carrier
c           idcd2X      =  stream ID of destination diversion (nd2) 
c                        or reservoir  (NOT CARRIER)

c
c           imonsw()     = monthly on off switch  
c           iowna        = source reservoir #2 account
c
c
c           iresw         destination type
c                         0 diversion destination
c                         1 reservoir destination
c            
c           l2 - order of operating rule in opr. rule list. 
c
c           nd           = destination (+=diversion, -=reservoir)  
c           ndnd         = # of downstream nodes for reservoir #1
c
c	          ipUse        = Destination Reuse plan
c                               Note ipUse=ireuse(l2)
c	          np           = Source reuse plan
c                          Note np  =IOPSOU(1,l2)           
c
c           nr           = source reservoir #1          
c           nra          = source reservoir #2 
c
c
c	          intern(  )   = If > 0 carrier system with intervening
c                          structures
c
c		        qdiv(5	From River by Priority
c           qdiv(7   From River by Storage
c		        qdiv(18 Carrier passing through a structure
c           qdiv(20 From Carrier by Storage or Exchange (e.g. carrpl)
c           qdiv(22  From Carrier by Storage (type 3 see Divres) 
c           qdiv(26 From River by a Direct Flow Exchange
c                   or Bypass (see DirectEx and DirectBY)
c		        qdiv(28  Carried, Exchange or Bypass (column 11)
c                                Stored via a reuse plan in DirectEx or DirectBy
c		        qdiv(29  From river by exchange from a plan
c		        qdiv(30  From River by direct from a Res or Reuse Plan 
c                                to a T&C Plan. Note non consumptive
c		        qdiv(32  Carrier loss by a diversion (DivCar, DivcarL) 
c		        qdiv(33  Carrier loss to a destination (DivCar, DivcarL) 
c
c ---------------------------------------------------------
c
c           qres(8  From storage to river for use
c		        qres(22 From carrier to storage
c		        qres(25 Reservoir Seepage by Carrier Loss
c           qres(26 From river by Storage 
c
c ---------------------------------------------------------
c		Loss Data
c	         OprLoss(l2) =  Transit loss (%) 
c	        		Transit loss is a true loss no routing
c	         ioprloss    = int(OprLoss) carrier loss switch
c	        		+ transit loss, maybe carrier loss
c	        		- 0 transit loss, maybe carrier loss
c	         TranLoss    =  Transit loss (fraction)
          
c	         OprLossC(l2,i) = Conveyance loss from a carrier (%)
c	        		  Conveyance loss gets routed to system
c	         OprLost= 	conveyance loss (cfs)
c                
c	         OprEff1 = 	source Carrier Efficiency 
c                                 (1.0 - OprLoss(l2)/100)
c	         OprEffT = 	Total Carrier Efficiency 
c                
c	         effmaxT=	Transit loss for a ditch 
c         
c	         ncarry          indicator at least 1 carrier
c	         ncnum          # of carriers
c
c          idcd2X         stream ID of destination diversion (nd2) or 
c			              reservoir or plan 
c
c _________________________________________________________
c		Dimensions
      include 'common.inc'
      character cwhy*24, cdestyp*12, ccarry*3, cstaid1*12, rec12*12,
     1 cpuse*3, cresid1*12 
c
c _________________________________________________________
c               
c               Step 1; Initilize
c
c		iout = 0 no details
c		       1 details
c          2 summary   
c          3 details of plan storage   
      iout=0
      ioutX=0
      ioutIR=0
      
      if(ichk.eq.132) ioutX=2
      if(corid(l2).eq. ccall) ioutiw=iw
c
c		Override data in *.ctl      
c     iout=0
      if((ioutX.ge.1 .and. ncallx.eq.0) .or. iout.gt.0)then
        write(nlog,102) corid(l2)
 102    format(/, 72('_'),/ '  DivResR; ID = ', a12)
      endif      
      
      
c		b. Daily capability
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif
c
c               c. Miscellaneous
      small=0.001
      divact = 0.0          
      divalo = 0.0
      ishort = 0
      iw=iw
      iowna=0
      
      relact=0.0
      alocfsP=-1.0/fac
      divalo1=-1.0/fac
      divmon1=-1.0/fac
      pavail=-1.0/fac
      diveff1=-1.0/100.      
      effmax1=-1.0
      ireltyp=-1
      
      qres22a=-1.0
      qres22b=-1.0
      iwhy=0      
      
      cstaid1='N/A'
      cwhy='N/A'
      cdestyp='N/A'
      ccarry='No'
      nr=0
      nrD=0
      nRiver=0
      
      cpuse='No'
      
      ndirect=0
      rec12=cdivtyp(l2)
      if(rec12(1:6).eq.'Direct') ndirect=1
      
      lopr=0
      loprR=0
      noprS=0
c
c rrb 00/12/26; Variable efficiency consideration
      ieff2=1     
c
c rrb 01/01/17; Call number
      icx=32
c
c --------------------------------------------------------
      ndtype = iopdesr(l2)
      if(ndtype.eq.1) cdestyp='ISF Flow'
      if(ndtype.eq.2) cdestyp='Reservoir'
      if(ndtype.eq.3) cdestyp='Diversion'
      if(ndtype.eq.7) cdestyp='Plan     '  
      if(iout.eq.1) write(nlog,*) '  DivResR; ndtype', ndtype, cdestyp     
c
c ---------------------------------------------------------
c		f. Standard Carrier      
      nCarry=0
      ccarry='No'
      if(intern(l2,1).gt.0) then
        ccarry='Yes'
        ncarry=intern(l2,1)
      endif      
c
c ---------------------------------------------------------
c rrb 2007/06/06; Add Carrier Loss
      if(iout.eq.1) write(nlog,*) ' DivResR  Call SetLoss'
      
      call SetLoss(nlog, icx, l2, fac, 
     1 maxopr,   intern, Oprloss,  OprLossC,
     1 ioprloss, nCarry, nRiver, ncnum, 
c
c rrb 2008/06/03; Correction     
c    1 OprLost,  DemAdj, OprEff1, OprEffT, TranLoss)      
     1 OprLost,  OprEff1, OprEffT, TranLoss, internT,internL,corid(l2))
c
c _________________________________________________________
c		Step 1b; Check avail array coming in
      call chekava(28, maxsta, numsta, avail)
      
c
c _________________________________________________________
c               Step 1c; Branch if not on this month
c
      if(imonsw(l2,mon).eq.0) then
        iwhy=1
        cwhy='Monthly switch is off'
        goto 330
      endif
c
c ---------------------------------------------------------
c		For a daily model set demand for beginning of season
      if(iday.eq.1 .and. imonsw(l2,mon).gt.0) then
        if (idy.lt.imonsw(l2,mon)) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 330
        endif  
      endif  
c
c ---------------------------------------------------------
c		For a daily model set demand for end of season
      if(iday.eq.1 .and. imonsw(l2,mon).lt.0) then
        if (idy.gt.iabs(imonsw(l2,mon))) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 330
        endif  
      endif  
      
c      
c ________________________________________________________
c               Step 2a; Set Plan pointer for destination reuse plan
c		ipUse = Reuse plan for return flows
      ipUse=ireuse(l2)
      if(ipUse.gt.0) cpuse='Yes'
      
c      
c ________________________________________________________
c               Step 2; Set Plan pointer for source plan
c		np = Reuse plan supply      
      np=iopsou(3,l2)
c
c _________________________________________________________
c		Step X. Set CU limit switch      
      rec12=cDivTyp(l2)
      icu=0
      if(rec12(1:9).eq.'Diversion') icu=0
      if(rec12(1:9).eq.'Depletion') icu=1
      
c
c _________________________________________________________
c
c               Step 3; Set source data
c
c               a. Source reservoir #1 (nr), source account (iown),
c                  river station (iscd), & # downstream (ndnp)
      NR  =IOPSOU(1,l2)    
cr    write(Nlog,*) ' DivresR; nr ', nr       
cr    write(Nlog,*) ' DivresR; iressw(nr) ', iressw(nr)

      if(iressw(nr).eq.0) then
        iwhy=2
        cwhy='Source Reservoir is off'
        goto 330
      endif  

      IOWN=NOWNER(NR)+IOPSOU(2,l2)-1

      iscd=IRSSTA(NR)
      NDNP=NDNNOD(iscd)
      cstaid1=cstaid(iscd)
c
c rrb 01/05/15; Check reservoir roundoff when entering routine
      call chekres(nlog, maxres, 0, 21, iyr, mon, nr,nowner,
     1             curown,cursto,cresid)
c
c _________________________________________________________
c               
c               Step 4; Set destination data
c
      nd  = iopdes(1,l2)
      nd2 = iopdes(1,l2)
      ndD = 0
      ndR = 0
      ndI = 0

      if(iout.eq.1) write(nlog,*) '  DivResR; nd, nd2 = ',nd, nd2
c
c ---------------------------------------------------------
c

c               a. RESERVOIR DESTINATION (iresw=1)
c
c rrb 2008/01/08; Enhancement
c     if(nd.lt.0) then
      if(ndtype.eq.2) then
        cdestyp='Reservoir'        
        iresw=1
c
c rrb 2011/11/29; Revise to rely on ndtype
cx      nrD=-nd
        ndR=nd
        
        nrD=nd
        ndd2=0
        nr2=1        
        idcd=irssta(nrD) 
        idcd2X=idcd       
        NDND=NDNNOD(IDCD)
c
c ---------------------------------------------------------
c rrb 2006/09/25; Allow multiple accounts where if iopdes(2,l2)
c                 is less than 0 it is all accounts and if it
c                 is greater than 0 it is just one account
        nro=1 
        if(iopdes(2,l2).lt.0) then
          nro=-iopdes(2,l2)
          irow=nowner(nrD)
        endif
c
        if(iopdes(2,l2).gt.0) then
          nro=1
          irow=nowner(nrD)+iopdes(2,l2)-1
        endif
             
c
c ---------------------------------------------------------
        if (iressw(nrD).eq.0) then
          iwhy=3
          cwhy='Destination Res is off'
          goto 330
        endif  
        
        if(iout.eq.1) write(nlog,*) '  DivResR; nrd, irow = ', nrd,irow
      endif
c
c ---------------------------------------------------------
c               b. DIVERSION DESTINATION 
c
c rrb 2008/01/08; Correction when above reset nd to be positive
c     if(nd.gt.0) then
      if(ndtype.eq.3) then
        cdestyp='Diversion'  
        ndD=nd    
c
c rrb 2011/11/29; Update to ndtype         
cx      iresw=0

        ndd2=1
        nr2=0
        idcd=idvsta(nd)        
        idcdD=idcd        
        idcd2X=idcd       
        NDND=NDNNOD(IDCD)
        effmax1=effmax(nd)

        IUSE=NDUSER(ND)+IOPDES(2,l2)-1
        divreq1=divreq(iuse)
        diveff1=diveff(mon,iuse)/100.
        
        
        if(idivsw(nd).eq.0) then
          iwhy=4
          cwhy='Destination Div is off'        
          goto 330
        endif  
      endif  
c
c ---------------------------------------------------------
c rrb 2011/11/29; Update to allow an ISD destination     
c               b. DIVERSION is an ISF
c
      if(ndtype.eq.1) then
        ndI=nd
        if(ifrrsw(ndI).eq.0) ioff=2
        
        IUSE=1
        iuseX=iuse
        
        IDCD=ifrsta(ndI)
        idcdI=idcd
        idcd2X=idcd
        ndnd=NDNNOD(idcd)
        
        idcdX=idcdI
        ndndX=NDNNOD(idcd2X)

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
c ---------------------------------------------------------
c               e. Carrier system data 
 120  if(ityopr(l2).ne.10) then
        if(intern(l2,1).gt.0) then
          ncarry=intern(l2,1)
          nc=intern(l2,1)
          ccarry='Yes'
          ndc=intern(l2,1)

          idcd=idvsta(ndc)
c         idcd2X=idcd                   
          idcd2C=idcd
          ndnd=ndnnod(idcd)
c         write(nlog,*) '  DivResR; l2, idcd', l2, idcd
cr        go to 140
        endif  
      endif
c _________________________________________________________
c
c            **  Step 5; Set demand (DIVALO) when the destination
c                        is a diversion
c  
c rrb 2011/11/29; Update to ndtype
cx    if(iresw.eq.0) then
      if(ndtype.eq.3) then      
c
c               a. Set demand 
        DIVALO=AMIN1(DIVREQ(IUSE),DIVCAP(ND)-DIVMON(ND))
c       write(nlog,*) DIVREQ(IUSE)*fac, (DIVCAP(ND)-DIVMON(ND))*fac
        divalo1=DIVALO
c
c
c ---------------------------------------------------------
c
c            	b. Adjust based on release type
c		   Set based on release type
c	           ireltyp=0 demand
c		         ireltyp>0
c            ireltyp = 0 release to meet demand
c            ireltyp > 0 release only if a CIR (IWR) exists
c                        and limit release to not exceed IWR/n 
c 
        ireltyp=amin0(iopsou(6,l2),ifix(effmax(nd)))
c       write(nlog,*) ' ireltyp = ', ireltyp, 
c    1                ' diwrreq(iuse)', diwrreq(iuse)
        divmax=-1.0/fac
        if(ireltyp.gt.0) then 
          if(diwrreq(iuse).le.small) then
            divalo=0.0
          else
            divmax = diwrreq(iuse)/(float(ireltyp)/100.0)
            divalo=amin1(divalo, divmax)
          endif
        endif

        divalo=amax1(0.0,divalo)
        
c
c ---------------------------------------------------------
c rrb 01/08/23; 
c               c. Exit if no demand
        if(divalo.le.small) then
          iwhy=5
          cwhy='Demand (divalo) is zero'
          goto 330
        endif
      endif
c 
c
c _________________________________________________________
c
c            ** Step 6; Set demand (DIVALO) when the destination
c                       is a reservoir
c
c               a. Set allowable storage
c rrb 2011/11/29; Update to ndtype
cx    if(iresw.eq.1) then
      if(ndtype.eq.2) then
c
c ---------------------------------------------------------
c rrb 2006/09/25; Allow multiple accounts - Demand
        cursa=0.0
        do n=1, nro
          n1=irow+n-1
          cursa=cursa+(ownmax(n1)-curown(n1))
c         write(nlog,*) ' DivResR; n1,', n, n1,  ownmax(n1), curown(n1)
          
        end do  
      
cr      divalo=amin1(ownmax(irow)-curown(irow),
        divalo=amin1(cursa,
     1               volmax(nrD)-cursto(nrD),
     1               tarmax(nrD)-cursto(nrD))/fac

        divalo=amax1(0.0, divalo)
c
c ---------------------------------------------------------
c               b. Exit if nothing available
        if(divalo.lt.small) then
          iwhy=6
          cwhy='Destination Res is full (divalo)'
c          write(nlog,*) ' DivResR;', 
c     1     nrd, nro, cursa, volmax(nrd), tarmax(nrd)
          goto 330
        endif  
        
c
c 3/11/96 rrb;
c               b. Check Printout for reservoir demand
        if(iout.eq.1) then
          write(nlog,*) '  DivResR; nrD, irow ',
     1              'ownmax(irow)-curown(irow), ',
     1              'volmax(nrD)-cursto(nrD),  ',
     1              'tarmax(nrD)-cursto(nrD), divalo*fac'
          write(nlog,*)  nrD, irow,
     1               ownmax(irow)-curown(irow),
     1               volmax(nrD)-cursto(nrD),
     1               tarmax(nrD)-cursto(nrD), divalo
        endif
      endif
      
      
c
c _________________________________________________________
c
c           **  Step 7; Check for water available in supply reservoir
c                       RELALO (ac-ft) and ALOCFS (cfs)
c
c               a. Calculate volume in reservoir
        RELALO=AMIN1(CURSTO(NR)-VOLMIN(NR),CUROWN(IOWN))
        RELALO=AMAX1(0.0,RELALO)
c
c               b. Change units and
c                  Limit to maximum river discharge (flowmax)
        ALOCFS=RELALO/fac
cr              For now, keep supply (alocfs) independent of demand 
cr              (divalo)        
cr      ALOCFS=AMIN1(FLOMAX(NR)-RIVER(iscd),ALOCFS,DIVALO)
        ALOCFS=AMIN1(FLOMAX(NR)-RIVER(iscd),ALOCFS)
        alocfs = amax1(0.0,alocfs)
        alocfsR= alocfs
c
c               b. Exit if nothing available
        if(alocfs.lt.small) then
          iwhy=7
          cwhy='Supply Res (alocfsR)is empty'
          goto 330
        endif  
        
c rrb 2005/07/21 Limit to amount in the reuse plan (supply)        
c
c _________________________________________________________
c
c               Step 8; Set source plan data
c               a. Source plan data
c
      alocfsP = psto2(np)/fac
      if(iout.eq.3) then
        write(nlog,*) ' DivResR; np, psto2(np) ',np, psto2(np) 
      endif
      
      alocfsP = amax1(0.0,alocfsP)
            
      if(alocfsP.lt.small) then
        iwhy=8
        cwhy='Plan Supply (alocfsP) equals 0'
        goto 330
      endif
c
c _________________________________________________________
c
c               Step 9; Limit supply to the min of reservoir and plan
      alocfs=amin1(alocfsR, alocfsP)
c
c_____________________________________________________________
c
c rrb 2007/06/06; Add Carrier Loss             
c               Step 10; Process carrier limitations
      if(iout.eq.1) write(nlog,*) ' DivResR  Call SetCarL'
      call SetCarL(nlog, icx, l2, fac, 
     1 maxopr,  maxdiv, intern, OprLossC,
     1 ncarry,  ncnum,  noprS,  internT, 
     1 OprEff1, DivCap, DivMon, DivCarry, alocfs)
c
c _________________________________________________________
c
c		Step 11; Calculate diversion based on Diversion
c                        Set diversion (DIVACT) and
c                        release (RELACT) to be the minimum of
c                        reservoir (ALOCFS) and demand (DIVALO) 
      if(icu.eq.0) then
        divact=amin1(alocfs,divalo)
        divact=amax1(0.0,divact)
        relact=-divact
      endif  
c
c _________________________________________________________
c
c               Step 12b; Calculate diversion based on Depletion
c                         Set diversion (DIVACT) and
c                         to be the minimum of
c                         weighted reservoir (ALOCFS/diveff1) 
c                         and demand (DIVALO) 
c			  Set release to be demand*diveff1
      if(icu.eq.1) then
        iss=idcd        
        ndndD=ndnnod(idcd)
        
        if(iout.eq.1) write(nlog,*) ' DivResR  Call DnmFso'  
              
        call dnmfso(maxsta, avail, idncod, iss,ndndD, imcd)
        pavail=avail(imcd)
c
cr                        Recognize efficiency of use in the supply        
cr      divact=amin1(alocfs,divalo)
        divact=amin1(alocfs/diveff1,divalo)
        divact=amax1(0.0,divact)
c
c               b. If Available flow < demand
c                  set the release (RELACT) to the difference
c                  (DIVACT - PAVAIL) or the depletion (divact*diveff)
        if(pavail .lt. divact) then
c
c rrb 05/05/12; limit diversion to amount in exchange reach
c		            release to meet depletion
c		            Concern if return flow timing drives system negative
          divact=pavail          
          relact=-1.*(divact*diveff(mon,iuse)/100.)
        else
c               c. If available flow >= demand (pavail>=divact)
c                  set release to the depletion (divact*diveff)
          relact=-1.*(divact*diveff(mon,iuse)/100.)
        endif
c
c               d. If iout=1 print detailed results
        if(iout.eq.1) then
          c = divact*diveff(mon,iuse)/100.0
          write(io99,390) 2, divact*fac, pavail*fac, relact*fac, c*fac
        endif
      endif  
c
c
c _________________________________________________________
c
c               Step 12; Exit if no demand (divact <=0) or 
c
      if (divact.le.small) then
        iwhy=9
        cwhy='Demand (divact) is zero'
        goto 330
      endif  
c
      AVAILR=AVAIL(iscd)
c
c
c _________________________________________________________
c
c               Step 13; Remove diversion (DIVACT) from stream
c                        if not a direct release (ndirect.eq.0)
c rrb 2007/06/05; Check if a direct release
c rrb 2011/11/29; Allow an instream flow destination
      if(ndtype.eq.2 .or. ndtype.eq.3) then
        if(ndirect.eq.0) then
          if(iout.eq.1) write(nlog,*) ' DivResR  Call TakOut_1'      
          CALL TAKOUT(maxsta,AVAIL,RIVER,AVINP,QTRIBU,IDNCOD,
     1              DIVACT,NDND,IDCD)
c         write(nlog,*) '  DivResR; idcd,divact,relact = ',idcd,divact,relact
        endif
      endif
c
c _________________________________________________________
c
c               Step 14; Add reservoir release (RELACT) to stream
c                        if not a direct release (ndirect.eq.0)
c rrb 2007/06/05; Check if a direct release
        if(ndirect.eq.0) then
          AVAILR=AVAIL(iscd)
          call takout(maxsta,avail,river,avinp,qtribu,idncod,
     1              relact,ndnp,iscd)
          avail(iscd)=availr
c         write(nlog,*) '  DivResR; iscd,divact = ', idcd, divact, relact
        endif   
c
c _________________________________________________________
c rrb 2011/11/29; Remove ISF from avail
c		            Step 15; Remove ISF from avail 
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
c		Step 16a; Calculate diversion with loss (divactL)
c		          and lost (OprLost)
c rrb 2007/06/06; Add loss        
      divactL=divact*OprEffT          
      OprLost=divact* (1.0-OprEffT)
c
c _________________________________________________________
c
c               Step 16; Add return flows to stream
c
c rrb 2011/11/29; Update to ndtype
cx    if (iresw.eq.0 .and. ipUse.eq.0) then
      if(ndtype.eq.3 .and. ipUse.eq.0) then
        if(iout.eq.1) write(nlog,*) ' DivResR  Call RtnSec'      
        divactL=divact*OprEffT      
        call rtnsec(icx,divactL,l2,iuse,IDCD,nd,ieff2)
      endif
        
c
c _________________________________________________________

c		Plan Reuse from destination
      if(ipUse.gt.0) then
c
c rrb 2011/11/29; Update to ndtype         
cx      if(nd2.gt.0) then        
        if(ndtype.eq.3) then
          if(iout.eq.1) write(nlog,*) ' DivResR  Call RtnSecR'            
          CALL RtnsecR(icx,divactL,l2,iuse,idcd,nd2,
     1         ieff2,ipUse)
c
c rrb 04/12/30; Qdiv(28 is the carried / exchanged water
c		Note works outside river system
          ipsta1=ipsta(ipUse)
          qdiv(28,ipsta1) = psuplyT(ipUse)     
        else
c
c		Reservoir Reuse          
          psuply(ipUse)=psuply(ipUse)+divactL
          psuplyT(ipUse)=psuplyT(ipUse)+divactL
c
c rrb 2006/01/01; Correction
          if(iplntyp(ipuse).eq.3 .or. iplntyp(ipuse).eq.5) then
            psto2(ipUse)=psto2(ipUse)+divactL*fac          
          endif  
          
          ipsta1=ipsta(ipUse)
          qdiv(28,ipsta1) = psuplyT(ipUse)                 
        endif  
      endif
c
c _________________________________________________________
c
c               Step 17; Destination is a diversion update demand data
c
c
c rrb 2007/06/06; Note adjust for full amount (no loss)
c rrb 2011/11/29; Update to ndtype
cx    if(iresw.eq.0) then
      if(ndtype.eq.3) then
        USEMON(IUSE)=USEMON(IUSE)+DIVACT
        DIVREQ(IUSE)=DIVREQ(IUSE)-DIVACT
        DIVMON(ND  )=DIVMON(ND  )+DIVACT
        divmon1=divmon(nd)

C-----  UPDATE STORAGE WATER USE OF DESTINATION DEMAND AT 
c       RIVER (QDIV(7))
C       WATER CARRIED ACCOUNT (QDIV(18) OF OTHER NODES, DESTINATION 
c       STORAGE WATER USE (QDIV(20)
c              idcd  = river ID
c              inode = destination ID 
c              iresw = 1 destination is a reservoir
c                    = 0 destination is a demand
c              Do not update qdiv(20 ) from carrier by storage
c              if no intervening structures
c
c              Update to carrier from storage (Qdiv(20,inode))
c
c rrb 2007/06/06; Add Loss in SetqdivC    
c		Destination is a diversion        
        qdiv(20,idcdd)=qdiv(20,idcdd)+divactL
c
c		Destination is a carrier
        if(idcd2X.eq.idcd2C) then
          qdiv(26,idcd2X)=qdiv(26,idcd2X)+divact
          qdiv(18,idcd2X)=qdiv(18,idcd2X)+divactL  
          qdiv(33,idcd2X)=qdiv(33,idcd2X)+OprLost  
        endif  
        
        if(iout.eq.1) 
     1    write(nlog,*) '  Divresr;', idcdd, idcd, idcd2C,
     1    divact*fac, qdiv(20,idcdd)*fac, qdiv(33,idcdd)*fac,              
     1    qdiv(26,idcd)*fac, qdiv(18,idcd)*fac, qdiv(33,idcdd)*fac
      endif                           
c
c
c _________________________________________________________
c
c               Step 18; Destination is a reservoir, update 
c   
c rrb 2011/11/29; Update to ndtype
cx    if(iresw.eq.1) then
      if(ndtype.eq.2) then
        if(iout.eq.1) write(nlog,*) ' DivresR; dest reservoir', nrD
c
        divaf=divact*fac
        divafL=divaf
        cursto(nrD)=cursto(nrD)+divafL
c
c ---------------------------------------------------------
c rrb 2006/09/25; Allow multiple accounts - Allocate
c		              Note:
c		              iResT1=0 distributes based on ownership ratio
c		              nrown1=number of accounts in this reservoir
c		              iown = first account associated with this reservoir  
c		              icx  = subrouine calling accou.for       
c		              ia   = account to adjust
      
        nrX=nrD
        iResT1=0
        nrown1=nro
        iownX=irow
        icx=32
        
        if(ncarry.gt.0) then
          ia=4
        else       
c                                                                
c rrb 2010/10/15; Correction.  Note:                             
c		              qres(22 From carrier to storage                
c                 qres(26 From river by Storage to Reservoir 
cx        ia=26
          if(ndirect.eq.1) then
            ia=22
          else                     
            ia=26
          endif
        endif  
        cresid1=cresid(nrX)
c   
        if(iout.eq.1) write(nlog,*) ' DivResR  Call Accou', ncarry 
              
        call accou(maxacc, maxown, nrX, ownmon, curown, accr, ia, 
     1    ownmax, iownX, nrown1, cursa, divafL, iResT1, icx, cresid1)
        if(iout.eq.1) write(nlog,*) ' DivResR  Back from Accou',ndirect         
c        
c ---------------------------------------------------------
c
         if(iout.eq.1) write(nlog,*) ' DivResR  nrD, nd', nrD, nd, idcd
c
c rrb 2010/10/15; Correction, Destination is a reservoir        
         qdiv(20,idcd)=qdiv(20,idcd)+divactL         
        
         if(ncarry.gt.0) then
           qres(4,nrD)=qres(4,nrD)+divafL
         else 
c                   
c rrb 2010/10/15; Correction.  Note:
c		      qres(22 From carrier to storage          
c         qres(26 From river by Storage to Reservoir
cx        qres(26,nrD)=qres(26,nrD)+divafL 
          qres22a=qres(22,nrD)
          if(ndirect.eq.1) then
            qres(4,nrD)=qres(4,nrD)+divafL
          else     
            qres(26,nrD)=qres(26,nrD)+divafL             
          endif
          qres22b=qres(22,nrD)
        endif
      endif
c
c_________________________________________________________
c rrb 2011/11/29; Allow an ISF destination
c               Step 19; Destination is a ISF, update 

c		b. Update a destination ISF
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
        qdiv(30,idcd)=qdiv(30,idcd)+DIVACT
        qdiv30=qdiv(30,idcd)        
      endif           
c
c_________________________________________________________
c               Step 20; Update direct release 
c                  Set qdiv(22 ) from carrier by storage type 3, 
c                  operating rule for water balance purposes
c rrb 2011/11/29; Update
      if(ndtype.eq.2 .or. ndtype.eq.3) then
        if(ndirect.eq.1) then
c
c rrb 2010/09/15; Correction
cx        inode=idvsta(nd)    
          inode=idvsta(nrD)    
          qdiv(22,inode)=qdiv(22,inode)+divact                      
        endif

        if(ndirect.eq.0) QDIV(7,IDCD)=QDIV(7,IDCD)+DIVACT
cx      write(nlog,*) '  DivresR; ', ndirect, idcd, qdiv(7,idcd)*fac
c        
      endif 
      if(iout.eq.1) 
     1  write(nlog,*) ' DivresR; nr, relact*fac',nr,ndirect,relact*fac
c     
c _________________________________________________________
c               Step 21; Update Qdiv and Carrier for all destinations
c
c     write(nlog,*) ' DivResR; nd, iuse', nd, iuse  
      if(iout.eq.1) write(nlog,*) ' DivResR  Call SetQdivC'   
      call setQdivC(
     1    nlog,   ncarry,   ncnum,    nd,       nd2,
     1    l2,     iscd,     idcd2X,   idcd2C,   nriver,
     1    divact, TranLoss, EffmaxT1, fac,      maxsta,
     1    maxdiv, maxqdiv,  maxopr,   intern,   idvsta,
     1    qdiv,   divmon,   maxrtnPP, maxplan,  OprEff1,
     1    ipuse,  pctlosPP, rlossP,   oprLossc, internT,
     1    icx,    corid(l2))
c
      if(iout.eq.1) write(nlog,*) ' DivResR  Back from SetQdivC' 
c
c
c_________________________________________________________
c               Step 19; Update source reservoir data
      ACTACF=RELACT*fac
      CURSTO(NR  )=CURSTO(NR  )+ACTACF
      PROJTF(NR  )=PROJTF(NR  )-RELACT
      CUROWN(IOWN)=CUROWN(IOWN)+ACTACF
c
c rrb 2010/10/15; Update for a direct release
c			qres(22 is From Storage to Carrier
c
cx    QRES(8,NR)=QRES(8,NR)-ACTACF
cx    accr(8,iown) = accr(8,iown)-actacf
      if(ndirect.eq.1) then
        QRES(22,NR)=QRES(22,NR)-ACTACF
        accr(22,iown) = accr(22,iown)-actacf      
      else
        QRES(8,NR)=QRES(8,NR)-ACTACF
        accr(8,iown) = accr(8,iown)-actacf
      endif
c
c_________________________________________________________
c		           Step 20; Update source plan
      psuply(np)=amax1(psuply(np) +relact*fac, 0.0)
c
c rrb 2005/11/12;             
c
c rrb 2006/01/01; Correction for a reservoir plan
      if(iplntyp(np).eq.3 .or. iplntyp(np).eq.5) then
        psto2(np)=amax1(psto2(np)+relact*fac, 0.0)                
      endif       
c
c _________________________________________________________
c               
c               Step 21;  Set shortage switch (ishort)
  330 if((divact+small) .lt. divalo) ishort=1
c _________________________________________________________
c               
c               Step 21;  Update operating value to res release
c		               Note release does not equal diversion if the
c                  Depletion option is on (icu=1) 
      divo(l2)=divo(l2)-relact
c
c _________________________________________________________
c               
c               Step 22; Check results
c
c               a. Check that Avail flow > 0
      call chekava(28, maxsta, numsta, avail)
c
c
c               b. Check reservoir roundoff when exiting routine
      call chekres(nlog, maxres, 1, 21, iyr, mon, nr,nowner,
     1                    curown,cursto,cresid)
      if(nrD.gt.0) then 
        call chekres(nlog, maxres, 1, 21, iyr, mon, nrD,nowner,
     1                    curown,cursto,cresid)
      endif
c
c _________________________________________________________
c
c               Step 23; Return
c
c
       if(ioutX.ge.1) then
       
        if(iw.eq.ioutiw) then           
          if(ncallX.eq.0) then
            write(nlog,270) corid(l2),cdestyp,ccarry,cpuse,cDivTyp(l2)            
            ncallX=ncallX+1
          else
c           write(nlog,*) ' '
          endif  
        
cx          if(divact.gt.small) then
          write(nlog, 280)  ' DivResR_Out',
     1      iyrmo(mon),xmonam(mon),idy, cstaid1,
     1      iwx, iw,nwrord(1,iw),l2, nd, nrd, ND2, idcd, iuse,ipUse,
     1      float(iopsou(6,l2)), effmax1, float(ireltyp), 
     1      divreq1*fac,divalo1*fac, 
     1      divmax*fac, relalo, 
     1      divmon1*fac,  alocfsR*fac, alocfsP*fac, pavail*fac,
     1      abs(relact*fac), divact*fac,   iwhy, cwhy

     
cx         endif
         endif
       endif
       
  280 FORMAT(1x,a12, i5,1x,a4, i5, 1x, a12,
     1   10i8,13F8.1,i8,1x, a24, 20f8.0)
  390 format(
     1  '  divResR; Release for Depletion Data;',/
     1  '                  #  divact  pavail  relact      CU',/
     1  '           ', i8, 20f8.2)
     
c
c _________________________________________________________
c
c		Step 21; Return

      RETURN

c               Formats
  270   format(/, 
     1  '  DivResR (Type 32); Operation Right ID = ', a12,
     1  ' Destination Type = ', a12,
     1  ' Carrier (Y/N) = ',a3, ' Reuse (Y/N) = ', a3, 
     1  ' Diversion Type = ', a12/    
     1   '  DivResR      iyr mon   day ID          ',
     1   '    Iter      Iw  nwrord      l2      nd     ndr     nd2',
     1   '    idcd    iuse   iPuse',
     1   ' Eff_opr Eff_max iRelTyp DivReq1 Divalo1  DivMax',
     1   '  Relalo Divmon1 AlocfsR AlocfsP  Pavail  Relact  DIVACT',
     1   '    iwhy Comment',/
     1   '  ___________ ____ ____ ____ ____________', 
     1   ' _______ _______ _______ _______ _______ _______ _______',
     1   ' _______ _______ _______',
     1   ' _______ _______ _______ _______ _______ _______',
     1   ' _______ _______ _______ _______ _______ _______ _______',
     1   ' _______ __________________________')
     
c
c               Error warnings
c _________________________________________________________
 900  write(nlog,910) corid(l2), diveff1*100.
 910  format(/,72('_'),/,
     1  ' DivResR (Type 32); Problem with Operation Right ID = ', a12,/
     1  '         The destination efficiency = ', f8.2,/
     1  '         when operating the reservoir release as a Depletion.',/
     1 '          Reconmend you revise efficiency or operate as a ',
     1   'Diversion')
      goto 9999
      
 9999 write(6,1050) 
      write(nlog,1051) 
    
 1050 format('    Stopped in DivResR',/,
     1       '    See the *.log file')
 1051 format('    Stopped in DivResR')
      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)


      stop 
      END

