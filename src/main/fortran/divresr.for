c divresr - Type 32 operating rule
c           It simulates releases from a Reservoir and a Plan to a
c           downstream Diversion, Reservoir, or Carrier by the river.
c_________________________________________________________________NoticeStart_
c StateMod Water Allocation Model
c StateMod is a part of Colorado's Decision Support Systems (CDSS)
c Copyright (C) 1994-2021 Colorado Department of Natural Resources
c 
c StateMod is free software:  you can redistribute it and/or modify
c     it under the terms of the GNU General Public License as published by
c     the Free Software Foundation, either version 3 of the License, or
c     (at your option) any later version.
c 
c StateMod is distributed in the hope that it will be useful,
c     but WITHOUT ANY WARRANTY; without even the implied warranty of
c     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c     GNU General Public License for more details.
c 
c     You should have received a copy of the GNU General Public License
c     along with StateMod.  If not, see <https://www.gnu.org/licenses/>.
c_________________________________________________________________NoticeEnd___
c
      subroutine DivResR(iw,l2,ishort,divact,ncallX)
c
c
c _________________________________________________________
c	Program Description
c
c       DivResR; Type 32
c       It simulates releases from a Reservoir and a Plan to a
c           downstream Diversion, Reservoir, Instream Flow or 
c           Carrier by the river
c
c           Called by Execut
c _________________________________________________________
c	Update history
c
c rrb 2021/05/22; Runtime Error Tracking; 
c                   Initialize Source reservoir (nr) if
c                     the routine makes a quick exit
c
c rrb 2021/05/02; Runtime error tracking
c
c rrb 2021/04/18; Compiler warning
c
c rrb 2020/09/20; Version 16.00.41;
c                 Add to qdiv(22 similar to a type 3 to allow
c                 Outbal2 to adjust for a release To_Conduit
c                 for a diversion destination (Step 17) or
c                 a reservoir destination (Step 18)
c                 Saved as Divresr_2020-09-20.old then did
c                 clean up related to qdiv and qres reporting
c
c rrb 2020/09/06; Version 16.00.40;
c                 Revise Step 20 to handle source; not destination
c                 Specifically:
c                 if the release is a diversion or To_Stream ndirect=0
c                 If the release is direct or To_Conduit (ndirect=1) 
c                 Therefore revise in Step 17 and Step 20.
c
c  
c rrb 2020-09-06; Version 16.00.40
c                 Revise DivResR to allow destination type (iopdesr) to',/
c                 support To_Stream and To_Conduit.  Note: Oprinp',/       
c                 revises Diversion = To Stream and Divert = To_Conduit',/ 
c
c rrb 2020/07/28; Version 16.00.39;
c                 Correction; if:
c                 ndtype 2 (reservoir) then inode = irusta(nrD)
c
c rrb 2020/07/28; Version 16.00.38;
c                 Correction; if:
c                 ndtype 2 (reservoir) then inode = idvsta(nrD) and 
c                 ndtype 3 (diversi0n) then inode = idvsta(ndD) 
cx          
c	2007/06/06;	Add Loss 
c	2005/03/31	Copied Divres
c			Removed ability to release for depletion	only
c     Removed ability to be called by replacement res
c			Removed stuff related to a transfer limit 
c			Removed references to transmtn (IRTURN(IUSE).EQ.4)
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
c           iopsou(2,l2) = source reservoir #1 account
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
c           iopdesr(lr)  = Destination type
c                          To_Conduit is a direct release to a structure 
c                          To_River is a release to the river
c
c           iout         = 0 no detailed printout
c                          1 yes detailed printout
c			                     2 yes summary
c
c           nr           = source reservoir
c           iown         = source reservoir #1 account
c
c           nrD          = destination reservoir
c           irow         = destination account

c           iscd         = Stream ID of Destination diversion, 
c		                       or reservoir or carrier
c           idcd2X       = stream ID of destination diversion (nd2) 
c                          or reservoir  (NOT CARRIER)
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
c           qdiv(7  From River by Storage
c		        qdiv(18 Carrier passing through a structure
c           qdiv(20 From Carrier by Storage or Exchange (e.g. carrpl)
c           qdiv(22  From Storage to Carrier for Use 
c                    a release To_Conduit by a type 3 or 32
c                    Its used in OutBal2 to recognize a To conduit 
c                    diversion
c                    Its not used to report anything in the *.xdd 
c                    report.  Note to show From Storage by Other
c                    in *.xdd use Qdiv(20
c
c           qdiv(26 From River by a Direct Flow Exchange
c                   or Bypass (see DirectEx and DirectBY)
c		        qdiv(28 Carried, Exchange or Bypass (column 11)
c                    Stored via a reuse plan in DirectEx or DirectBy
c		        qdiv(29 From river by exchange from a plan
c		        qdiv(30 From River by direct from a Res or Reuse Plan 
c                   to a T&C Plan. Note non consumptive
c		        qdiv(32 Carrier loss by a diversion (DivCar, DivcarL) 
c		        qdiv(33 From River loss 
c		        qdiv(33  Carrier loss to a destination (DivCar, DivcarL) 
c		        qdiv(34  From River by an Out of Priority Diversion
c           
c 		      qdiv(35  Water with a Reuse or Admin plan source 
c		        	         tracked at the destination from the following:
c                      Operating rules: 26-DirectWR,
c			                 27-DivResP2,     28-DivrplP, 29-PowSeaP,
c                      35-DivImpR,
c			                 46-divMultip,    48-PowResP, 49-DivRplP2                     
c           
c		        qdiv(36  Water released to the river (report as
c			                 return flow). Set in SetQdivC, DivResp2 &
c                      PowseaP      
c           qdiv(37  Not currently used.  Was used to represent
c                      Water released to the river by a spill in 
c                      PowseaP (type 29) Report as a negative  
c                      diversion herein (OutMon) under Rivdiv   
c           qdiv(38  Carried water reported as Carried, Exchange 
c                      or Other but not used to calculate
c
c ---------------------------------------------------------
c
c           qres(4  From carrier to storage by Other
c           qres(8  From storage to river for use
c		        qres(22 From carrier to storage
c		        qres(25 Reservoir Seepage by Carrier Loss
c           qres(26 From river by Storage 
c           qres(38 Carried water reported as Carried, Exchange 
c                       or Other but not used to calculate
c                       River Divert in Outmon.f   

c
c ---------------------------------------------------------
c		Loss Data
c	         OprLoss(l2) =  Transit loss (%) 
c	        		            Transit loss is a true loss no routing
c	         ioprloss    = int(OprLoss) carrier loss switch
c	        		           + transit loss, maybe carrier loss
c	        		           - 0 transit loss, maybe carrier loss
c	         TranLoss    =  Transit loss (fraction)
          
c	         OprLossC(l2,i) = Conveyance loss from a carrier (%)
c	        		        Conveyance loss gets routed to system
c	         OprLost= 	conveyance loss (cfs)
c                
c	         OprEff1 = 	source Carrier Efficiency 
c                                 (1.0 - OprLoss(l2)/100)
c	         OprEffT = 	Total Carrier Efficiency 
c                
c	         effmaxT =	Transit loss for a ditch 
c         
c	         ncarry  =  indicator at least 1 carrier
c
c          iopdesr(lr)  = Destination type
c                          To_Conduit is a direct release to a structure 
c                          To_River is a release to the river
c          nDirect =  0 release from a reservoir To_Stream
c                       with or without a carrier 
c                     1 release from a reservoir To_Conduit)
c	         ncnum   =  # of carriers
c
c          idcd2X  =  stream ID of destination diversion (nd2) or 
c			                reservoir or plan 
c
c _________________________________________________________
c		Dimensions
      include 'common.inc'
      character cwhy*45, cdestyp*12, ccarry*3, cstaid1*12, rec12*12,
     1 cpuse*3, cresid1*12, subtypX*8 
c
c ---------------------------------------------------------
c rrb 2021/05/02; Runtime error tracking
      character cCallBy*12
      cCallBy = 'Divresr'
c
c _________________________________________________________
c               
c               Step 1; Initialize
c
c		iout = 0 no details
c		       1 details
c          2 summary   
c          3 details of plan storage   
c
c
c rrb 2021/04/18; Compiler not used or initialize
      iexit=0
      if(iexit.gt.0) goto 500

c     
      subtypX='divresr '
      iout=0
      ioutX=0
      ioutIR=0
      ioutQ=0
      ioutiw=0
c
c rrb 2021/04/18; Compiler warning
      divreq1=0.0
      divmax=0.0
      alocfsr=0.
      nro=0
      irow=0
      idcdd=0
      ib=0
      ie=0
      
      if(ichk.eq.132) ioutX=2
      if(corid(l2).eq. ccall) ioutiw=iw
      
cx      write(nlog,*) ' DivResR;',
cx     1  ichk, ioutX, corid(l2), ccall, iw, ioutiw, ncallX'
cx      write(nlog,*) ' DivResR;',
cx     1  ichk, ioutX, corid(l2), ccall, iw, ioutiw, ncallX
c
c
c --------------------------------------------------------
c		            a. Override data in *.ctl      
c     iout=0
c
c rrb 2019/08/25; Reduce detailed output
c rrb 2020/09/06; Reduce detailed reporting
cx    if((ioutX.ge.1 .and. ncallx.eq.0) .or. iout.gt.0)then
      if(ioutX.eq.2 .and. ioutiw.eq.iw .and. ncallx.eq.0) then
        write(nlog,102) corid(l2)
 102    format(/, 72('_'),/ '  DivResR; ID = ', a12)
      endif      
      
c
c --------------------------------------------------------      
c		            b. Daily capability
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif
c
c --------------------------------------------------------
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
c
c rrb 2020/09/06; use To Conduit and To Stream
cx    if(rec12(1:6).eq.'Direct') ndirect=1
      if(rec12(1:9).eq.'To_Stream')   ndirect = 0
      if(rec12(1:10).eq.'To_Conduit') ndirect = 1
      
      lopr=0
      loprR=0
      noprS=0
c
c rrb 2021/05/22; Runtime Error Tracking. Initialize 
c                 Source reservoir #1 (nr) if
c                 routine makes a quick exit          
      NR  =IOPSOU(1,l2)          
c
c rrb 00/12/26; Variable efficiency consideration
      ieff2=1     
c
c rrb 01/01/17; Call number
      icx=32
c
c --------------------------------------------------------
c               d. Diversion type
      ndtype = iopdesr(l2)
      if(ndtype.eq.1) cdestyp='ISF Flow'
      if(ndtype.eq.2) cdestyp='Reservoir'
      if(ndtype.eq.3) cdestyp='Diversion'
      if(ndtype.eq.7) cdestyp='Plan     '  
      if(iout.eq.1) write(nlog,*) '  DivResR; ndtype', ndtype, cdestyp
c
c ---------------------------------------------------------
c		            e. Standard Carrier      
      nCarry=0
      ccarry='No'
      if(intern(l2,1).gt.0) then
        ccarry='Yes'
        ncarry=intern(l2,1)
      endif      
c
c ---------------------------------------------------------
c rrb 2007/06/06; f. Carrier Loss
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
c		                Step 1b; Check avail array coming in
      call chekava(28, maxsta, numsta, avail, subtypX)
      
c
c _________________________________________________________
c                   Step 1c; Branch if not on this month
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
c		                     ipUse = Reuse plan for return flows
      ipUse=ireuse(l2)
      if(ipUse.gt.0) cpuse='Yes'
      
c      
c ________________________________________________________
c               Step 2b; Set Plan pointer for source plan
c		np = Reuse plan supply      
      np=iopsou(3,l2)
c
c rrb 2015/10/28; Add additional data check even though
c                 Oprinp.f already does this check 
      if(np.eq.0) then
        iwhy=2
        cwhy='Source Plan is equal to 0'
        goto 330
      endif  
      
c
c _________________________________________________________
c		           Step 2c. Set CU limit switch      
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

c               4a. RESERVOIR DESTINATION (iresw=1)
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
        
        if(iout.eq.1) write(nlog,*) 
     1    '  DivResR; nrd, irow = ', nrd,irow
      endif
c
c ---------------------------------------------------------
c               4b. DIVERSION DESTINATION 
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
c rrb 2011/11/29; Update to allow an ISF destination     
c               4c. DIVERSION is an ISF
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
c               4e. Carrier system data 
c
c rrb 2021/04/18; Compiler not used or initialize
cx120 if(ityopr(l2).ne.10) then
      if(ityopr(l2).ne.10) then
        if(intern(l2,1).gt.0) then
          ncarry=intern(l2,1)
          nc=intern(l2,1)
          ccarry='Yes'
          ndc=intern(l2,1)

          idcd=idvsta(ndc)
c         idcd2X=idcd                   
          idcd2C=idcd
          ndnd=ndnnod(idcd)
cr        go to 140
        endif  
      endif
      if(iout.eq.1) then
        write(nlog,*) 
     1    '  DivResR;  l2, idcd, idcd2x, idcd2C'
        write(nlog,*) 
     1    '  DivResR;',l2, idcd, idcd2x, idcd2C
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
c               a. Set diversion demand 
        DIVALO=AMIN1(DIVREQ(IUSE),DIVCAP(ND)-DIVMON(ND))
c       write(nlog,*) DIVREQ(IUSE)*fac, (DIVCAP(ND)-DIVMON(ND))*fac
        divalo1=DIVALO
c
c
c ---------------------------------------------------------
c
c            	b. Adjust diversion based on release type
c		   Set based on release type
c	           ireltyp=0 demand
c		         ireltyp>0
c            ireltyp = 0 release to meet demand
c            ireltyp > 0 release only if a CIR (IWR) exists
c                        and limit release to not exceed IWR/n 
c 
c
c rrb 2021/04/18; Compiler warning
cx      ireltyp=amin0(iopsou(6,l2),ifix(effmax(nd)))
        ireltyp=min(iopsou(6,l2),ifix(effmax(nd)))
        
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
c rrb 2001/08/23; 
c               c. Exit if no diversion demand
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
c             ** Step 6; Set demand (DIVALO) at a reservoir
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
c rrb 1996/03/11;
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
c           **  Step 7; Check for reservoir supply
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
c               a. Source plan supply data
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
c		            Step 12a; Calculate diversion based on Diversion
c                        Set diversion (DIVACT) and
c                        release (RELACT) to be the minimum of
c                        reservoir & plan(ALOCFS) and
c                        demand (DIVALO) 
      if(icu.eq.0) then
        divact=amin1(alocfs,divalo)
        divact=amax1(0.0,divact)
        relact=-divact
      endif  
c
c _________________________________________________________
c
c               Step 12b; Calculate diversion based on Depletion
c                         Set diversion (DIVACT) 
c                         to be the minimum of
c                         weighted reservoir (ALOCFS/diveff1) 
c                         and demand (DIVALO) 
c			  Set release to be demand*diveff1
      if(icu.eq.1) then
        iss=idcd        
        ndndD=ndnnod(idcd)
        
        if(iout.eq.1) write(nlog,*) ' DivResR  Call DnmFso2'  
c
c rrb 2021/05/02; Runtime error tracking              
cx      call dnmfso(maxsta, avail, idncod, iss,ndndD, imcd)
        call dnmfso2(maxsta, avail, idncod, iss,ndndD, imcd,cCallBy)
        pavail=avail(imcd)
c
cr              12b1 Recognize efficiency of use in the supply        
cr      divact=amin1(alocfs,divalo)
        divact=amin1(alocfs/diveff1,divalo)
        divact=amax1(0.0,divact)
c
c               12b2. If Available flow < demand
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
c               12bc. If available flow >= demand (pavail>=divact)
c                  set release to the depletion (divact*diveff)
          relact=-1.*(divact*diveff(mon,iuse)/100.)
        endif
c
c               12bd. If iout=1 print detailed results
        if(iout.eq.1) then
          c = divact*diveff(mon,iuse)/100.0
          write(io99,390) 2, divact*fac, pavail*fac, relact*fac, c*fac
        endif
      endif  
c
c
c _________________________________________________________
c
c               Step 12c; Exit if no demand (divact <=0)
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
c               Step 13; When ndirect=0 (To_Stream)
c                        Remove diversion (DIVACT) from stream
c                        if the destination is a diversion (ndtype=2)
c                        or reservoir (ndtype=3)
c rrb 2007/06/05; Check if a direct release
c rrb 2011/11/29; Allow an instream flow destination
      if(ndtype.eq.2 .or. ndtype.eq.3) then
        if(ndirect.eq.0) then
          if(iout.eq.1) write(nlog,*) ' DivResR  Call TakOut_1'      
          CALL TAKOUT(maxsta,AVAIL,RIVER,AVINP,QTRIBU,IDNCOD,
     1              DIVACT,NDND,IDCD)
c         write(nlog,*) 
c    1      '  DivResR; idcd,divact,relact = ',idcd,divact,relact
        endif
      endif
c
c _________________________________________________________
c
c               Step 14; When ndirect=0 (To_Stream)
c                        Add reservoir release (RELACT) to stream
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
c		           Step 16a; Calculate diversion with loss (divactL)
c		                     and lost (OprLost)
c rrb 2007/06/06; Add loss        
      divactL=divact*OprEffT          
      OprLost=divact* (1.0-OprEffT)
c
c _________________________________________________________
c
c               Step 16b; Add return flows to stream
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

c		            Step 16c; Plan Reuse from destination
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
c----------------------------------------------------------
c               Step 17a; set diversion variables
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
c		           qdiv(18 Carrier passing through a structure
c              qdiv(20 From Carrier by Storage or Exchange (e.g. carrpl)
c              qdiv(26 From River by a Other
c		           qdiv(33 From River loss from a diversion by the river
c                         
c
c
c----------------------------------------------------------
c
c            Set Qdiv for reporting to *.xdd 
c              qdiv(20 From Carrier by Storage or Exchange
c              qdiv(22 From Storage to Carrier for Use 
c                      a release To_Conduit by a type 3 or 32
c              qdiv(26 From River by a Other
c              nDirect =  0  for To_Stream = 1 for To_Conduit
c
        if(ndirect.eq.0) then
          if(ncarry.eq.0) then
            qdiv(26,idcdd)=qdiv(26,idcdd)+divactL
          else
            qdiv(20,idcdd)=qdiv(20,idcdd)+divactL        
          endif 
        else
c
c rrb 2020/09/18; Report similar to a type 3
c rrb 2020/09/20; Add to qdiv(20 and qdiv(22 similar to a type 3
c                 Note qdiv(22 will allow Outbal2 to adjust for 
c                 a release To_Conduit
c
cx        qdiv(38,idcdd) = qdiv(38,idcdd) + divactL
          qdiv(20,idcdd) = qdiv(20,idcdd) + divactL
          qdiv(22,idcdd) = qdiv(22,idcdd) + divactL
        endif      
c
c
        if(ioutQ.eq.1) then
          write(nlog,*) 
     1    ' DivResR_17; ndirect, ncarry,  idcdd,   iscd,  qdiv(20,',
     1    '  qdiv(26,  qdiv(22,  qdiv(38,   divactL' 
          write(nlog,'(a12,4i8, 20f10.0)') 
     1    ' DivResR_17;', ndirect, ncarry, idcdd, iscd, 
     1      qdiv(20,idcdd)*fac, qdiv(26,idcdd)*fac,qdiv(22,idcdd)*fac,
     1      qdiv(38,iscd)*fac, divactL*fac
        endif
        
c
c
c----------------------------------------------------------
c               Step 17c; set qdiv  when the destination is
c               the same as the carrier
c rrb 2015/10/25; Correction diversion is from a carrier (qdiv(20,xx)
        if(idcd2X.eq.idcd2C) then
cx        qdiv(26,idcd2X)=qdiv(26,idcd2X)+divact
          qdiv(20,idcd2X)=qdiv(20,idcd2X)+divact          
          qdiv(18,idcd2X)=qdiv(18,idcd2X)+divactL  
          qdiv(33,idcd2X)=qdiv(33,idcd2X)+OprLost  
        
          if(ioutQ.eq.1) 
     1      write(nlog,*) '  Divresr;', idcdd, idcd, idcd2X,
     1      divact*fac, qdiv(20,idcd2X)*fac, qdiv(33,idcd2X)*fac,
     1      qdiv(26,idcd2X)*fac, qdiv(18,idcd2X)*fac
        endif
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
c		              iown = first account for this reservoir  
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
c
        if(iout.eq.1) then
          write(nlog,*) ' DivResR  Back from Accou',ndirect         
          write(nlog,*) ' DivResR  nrD, nd', nrD, nd, idcd
        endif
c        
c ---------------------------------------------------------
c          Reservoir Destination
c            Set Qdiv for reporting to *.xdd 
c                qdiv(20 From Carrier by Storage or Exchange
c                qdiv(22 From Storage to Carrier for Use 
c                       a release To_Conduit by a type 3 or 32
c         
         qdiv(20,idcd)=qdiv(20,idcd)+divactL         
c
c rrb 2020/09/20; Add to qdiv(22 similar to a type 3 to allow
c                 Outbal to adjust for a release To_Conduit
c                 Ndirect 0 = ToStream & 1 = To Conduit
         if(ndirect.eq.1) then
           qdiv(22,idcd)=qdiv(22,idcd)+divactL
         endif
c        
c ---------------------------------------------------------
c          Reservoir Destination
c            Set Qres for reporting to *.xre 
c            qres(4  From carrier to storage by Other
c		         qres(22 From carrier to storage
      
         if(ncarry.gt.0) then
           qres(4,nrD)=qres(4,nrD)+divafL
         else 
c
c                 Ndirect 0 = ToStream & 1 = To Conduit                 
          if(ndirect.eq.1) then
            qres(4,nrD)=qres(4,nrD)+divafL
          else     
            qres(22,nrD)=qres(22,nrD)+divafL             
          endif
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
c
c_________________________________________________________
c               Step 20; Print qdiv data
c
        if(ioutQ.eq.1) then
          write(nlog,*) 
     1    ' DivResR_20; ndirect,   iscd,  qdiv(20,',
     1    '  qdiv(38,    divact' 
          write(nlog,'(a12,2i8, 20f10.0)') 
     1    ' DivResR_20;', ndirect, iscd, 
     1      qdiv(20,iscd)*fac, qdiv(38,iscd)*fac, divact*fac
        endif

c     
c _________________________________________________________
c               Step 21; Update Qdiv and Carrier for all destinations
c
c     write(nlog,*) ' DivResR; nd, iuse', nd, iuse  
c
c rrb 2015/10/28; Correction; only call if a carrier (ncarry.gt.0)
      if(ncarry.gt.0) then  
        if(iout.eq.1) write(nlog,*) ' DivResR  Call SetQdivC'  
        call setQdivC(
     1      nlog,   ncarry,   ncnum,    nd,       nd2,
     1      l2,     iscd,     idcd2X,   idcd2C,   nriver,
     1      divact, TranLoss, EffmaxT1, fac,      maxsta,
     1      maxdiv, maxqdiv,  maxopr,   intern,   idvsta,
     1      qdiv,   divmon,   maxrtnPP, maxplan,  OprEff1,
     1      ipuse,  pctlosPP, rlossP,   oprLossc, internT,
     1      icx,    corid(l2))
c       
        if(iout.eq.1) write(nlog,*) ' DivResR  Back from SetQdivC'
      endif 
c
c
c_________________________________________________________
c               Step 22; Update source reservoir data for *.xre and *.xre
      ACTACF=RELACT*fac
      CURSTO(NR  )=CURSTO(NR  )+ACTACF
      PROJTF(NR  )=PROJTF(NR  )-RELACT
      CUROWN(IOWN)=CUROWN(IOWN)+ACTACF
c
c rrb 2020/09/06; Update
cx    qdiv(20,iscd) = qdiv(20,iscd) + divact
      qdiv(38,iscd) = qdiv(38,iscd) + divact
      
        if(ioutQ.eq.1) then
          write(nlog,*) 
     1    ' DivResR_20; ndirect,   iscd,  qdiv(20,',
     1    '  qdiv(38,    divact' 
          write(nlog,'(a12,2i8, 20f10.0)') 
     1    ' DivResR_20;', ndirect, iscd, 
     1      qdiv(20,iscd)*fac, qdiv(38,iscd)*fac, divact*fac
        endif
c
c
c ---------------------------------------------------------
c rrb 2020/09/07; Correction
c               Ndirect 0 = To_Stream & 1 = To Conduit
      
      if(ndirect.eq.0) then
        QRES(8,NR)=QRES(8,NR)-ACTACF
        accr(8,iown) = accr(8,iown)-actacf
      else
        QRES(11,NR)=QRES(11,NR)-ACTACF
        accr(11,iown) = accr(11,iown)-actacf      
      endif
c
c_________________________________________________________
c		           Step 23; Update source plan
      psuply(np)=amax1(psuply(np) +relact*fac, 0.0)
c
c rrb 2006/01/01; Correction for a reservoir plan
      if(iplntyp(np).eq.3 .or. iplntyp(np).eq.5) then
        psto2(np)=amax1(psto2(np)+relact*fac, 0.0)                
      endif       
c
c _________________________________________________________
c               
c               Step 24;  Set shortage switch (ishort)
  330 if((divact+small) .lt. divalo) ishort=1
c _________________________________________________________
c               
c               Step 25;  Update operating value to res release
c		               Note release does not equal diversion if the
c                  Depletion option is on (icu=1) 
      divo(l2)=divo(l2)-relact
c
c _________________________________________________________
c               
c               Step 26; Check results
c
c               a. Check that Avail flow > 0
      call chekava(28, maxsta, numsta, avail, subtypX)
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
c               Step 27; Detailed Output
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
     1      divmon1*fac, alocfsR*fac, alocfsP*fac, pavail*fac,
     1      abs(relact*fac), divact*fac, iwhy, cwhy

     
cx         endif
         endif
       endif
       
  280 FORMAT(1x,a12, i5,1x,a4, i5, 1x, a12,
     1   10i8,13F8.1,i8,1x, a45, 20f8.0)
  390 format(
     1  '  divResR; Release for Depletion Data;',/
     1  '                  #  divact  pavail  relact      CU',/
     1  '           ', i8, 20f8.2)
     
c
c _________________________________________________________
c
c		Step 28; Return
c
c rrb 2020/08/30; TEST
cx    RETURN
 500  Return

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
c
c rrb 2021/04/18; Compiler warning
cx00  write(nlog,910) corid(l2), diveff1*100.
      write(nlog,910) corid(l2), diveff1*100.
 910  format(/,72('_'),/,
     1 ' DivResR (Type 32); Problem with Operation Right ID = ', a12,/
     1 '         The destination efficiency = ', f8.2,/
     1 '         when operating the reservoir release as a Depletion.',/
     1 '          Reconmend you revise efficiency or operate as a ',
     1 'Diversion')
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
