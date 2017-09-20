
c
c _________________________________________________________
c	Update History
c

c
c _________________________________________________________
c	Documentation
c

c
c _________________________________________________________
c	Dimensions
cc
c *********************************************************
c
      SUBROUTINE WelRech(IW,L2,ncallX)
c
c _________________________________________________________
c	Program Description
c
c
c       WelRech; Type 44.
c		 It simulates a Recharge Well diverting
c		 to a reservoir (typically a recharge reservoir)
c
c		Source 1 is a water right
c		Destination is a reservoir
c		ireuse is the plan to store depletion obligation
c                 if any
c
c
c_____________________________________________________________
c       Update History
c 
c rrb 2006/04/27; Copied WelAugP
c		  Revised accordingly
c               
c _________________________________________________________
c       Documentation
c
c        IW             Global water right ID
c        L2             LOC. OF operation right  in opr RIGHT TABLE
c        nwR            source water right
c
c        ceff           ratio of GW to SW efficicney
c
c        depx           depletion in this month by pumping
c        divact         total well pumping
c
c        divsprx        fraction of structure demand served by
c                       sprinklers
c        divothx        fraction of structure demand served by 
c                       other (non-sprinklers)
c
c        divreq         Diversion demand remaining after an iteration
c        divreqw        Well demand remaining after an iteration. Note
c                       divreqw uses well eff. for well only lands or
c                       if not adding demands (idemtyp=1)
c                       divreqw uses SW eff. for lands served by both
c                       SW & GW
c        dcrdivw(l2)    Well Decree (cfs) from Riginp
c        divdw(l2)      Well Decree diverted in previous iterations
c        divcapw(nd)    Well capacity (cfs) from Datinp
c        divmonw(nd)    Well capacity (cfs) used in previous iterations
c
c        diveff(mon,nd) Average diversion efficiency via *.dds
c        diveffw(mon,nd)Average well efficiency via *.wes
c
c        effmaxs(nd)    Maximum sprinker efficiency via *.tsp
c        effmaxw(nd)    Maximum flood efficiency via *.tsp
c
c        effd           Average diversion efficiency via *.dds
c        effa           Average well efficiency via *.wes
c
c        effs           Maximum sprinker efficiency via *.tsp
c        efff           Maximum flood efficiency via *.tsp 
c
c        iout           Switch: 0 no print; 1 yes print
c        idemtyp        Switch set in datinp via *.ctl
c                       1=do not add demand data from *.ddm and *.wem
c                       2=add demand data from *.ddm and *.wem
c                       3=total demand provided in *.ddm 
c                       4=total demand provided in *.ddm and do not
c                         limit SW demands based on other water 
c                         supplies (e.g. wells)
c                       5=same as 4 but demand is:
c                         max(input,water right)
c        idvstaw(nd)    River location of well
c        idivcow2(nd)   0=GW only structure
c                       +=diversion tied to well structure +
c        nd2            scalar for idivcow2(nd)
c        ishort         code for reoperation; 0=no, 1=yes
c
c        idivsww        on/off switch (0=off, 1=on)
c        idivcow2(nw)   SW diversion, if any, associated with well nw 
c
c	 ipAug		0=no Well Augmentation Calculations
c			1 yes Well Augmentation Calculations 
c
c        iscd           River location of well (iscd = idvstaw(nwe))
c
c        ispr           =0 use flood efficiency in rtnsecw
c                       =1 use sprinker efficiency in rtnsecw
c
c        itsfile        Switch via datinp (*.ctl)
c                       0=no GW acres by year provided
c                       1 = yes GW acres by year provided and demand
c                       is limited by amount via bomsec.f
c        iuse           User = nd for well structure
c                            = nduser(nd) for a diversion structure
c                       Note: multi user option is turned off in datinp!
c
c        nd             well ID 
c
c        ndnnod(iscd)   Number of downstream nodes
c        ndns           Number of downstream nodes from well location
c
c
c        qdiv(18  Carrier passing thru a structure 
c        qdiv(24,iscd) Pumping (diversion) by a well to a user at iscd
c        qdiv(25,iscd) Depletion (From River by Well) at river ID iscd
c
c        retx           Immediate (this day or month) return.
c                       Used for reoperation control along with
c                       variable ireop set in rtnsecw.for
c	 rlossX		Total Loss
c        small          a small value for roundoff (0.0) concerns
c
c_____________________________________________________________
c	Dimensions
c
      include 'common.inc'
      character cwhy*48, cdestyp*12, ccarry*3, cpuse*3, cstaid1*12,
     1          cidWR*12, rec12*12, cTandC*3, cidRiv*12, cidBal*12,
     1          cresid1*12, ctype1*12
c
c
c _________________________________________________________
c
c       Step 1 Common Initilization
c
c		iout = 0 No details
c		       1 Details
c                      2 Summary      
c		       3 Well Augmentation details
c		       4 Sum
c
c ---------------------------------------------------------
c		a. OutPut control
      iout=0
      ioutiw=0
      
      if(ichk.eq.144) iout=2
      if(corid(l2).eq. ccall) ioutiw=iw
c     write(Nlog,*) ' WelRech; ncallx, iout, ioutiw, iw', 
c    1                          ncallx, iout, ioutiw, iw
c
c ---------------------------------------------------------
c		b. Factor
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif
c
c ---------------------------------------------------------
c		c. Miscellaneous
      
      imcP=-1
      small = 0.001
      divact= 0.0
      divactx=0.0
      divalo = 0.0
      
      cuact=0.0
      cuact1=0.0
      cuact2=0.0
      
      rlossX=0.0
      rlossX1=0.0
      rlossX2=0.0
      totX=0.0
      rettot=0.0      
      
      retx  = 0.0      
      retx1=0.0
      retx2=0.0
c 
c rrb 00/12/26; Set variable efficiency (1=on, controlled by ieffmax)
      ieff2 =1              
c
      OprLoss1=OprLoss(l2)/100.
      OprEff1 = 1.0 - OprLoss1
      OprLosT=0.0
      ishort=0
                       
c
c ---------------------------------------------------------
c               d. Check Avail array
      call chekava(19, maxsta, numsta, avail)
c
c ---------------------------------------------------------
c               e. Initilze temp array to store current 
c		   depletions and returns
      do is=1,numsta
        avtemp(is)=0.0
      end do
      
c
c ---------------------------------------------------------
c		f. Detailed Output
      iwhy=0
      cwhy='NA'
      cdestyp='NA'
      ccarry='No'
      cpuse='No'
      cTandC='No'
      
      divcapZ=-1/fac
      dcrdivZ=-1./fac
      pdem1=-1./fac
      pdem2=-1./fac
      Avail1=-1./fac
      Avail2=-1./fac
      AvailX=-1./fac
      PwellP1=-1/fac
      PwellP2=-1/fac
      divmonx=-1/fac
      gxTot=0.0
      cidWR=crigidw(l2)
      cidRiv='NA'
      cidBal='NA'
c
c _________________________________________________________
c		Step X; On/Off Switch      
c
c rrb 06/01/18; Allow daily on/off switch
      if(imonsw(l2,mon).eq.0) then
        iwhy=1
        cwhy='Monthly switch Off'
        goto 260
      endif  
c
c ---------------------------------------------------------
c		Daily ON/Off switch start on day x
      if(iday.eq.1 .and. imonsw(l2,mon).gt.0) then
        if (idy.lt.imonsw(l2,mon)) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 260
        endif  
      endif  
c
c ---------------------------------------------------------
c		Daily On/Off Swicth end on day x
      if(iday.eq.1 .and. imonsw(l2,mon).lt.0) then
        if (idy.gt.iabs(imonsw(l2,mon))) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 260
        endif  
      endif  
c
c _________________________________________________________
c
c		Step 2; Set Source 1 a well water right
      nWR  =Iopsou(1,L2)
      nWE = idivcow(1,nWR)
      iscd=idvstaw(nWE)
      NDNS=NDNNOD(ISCD)
      imcd=idvstaw(nWE)
      cstaid1=cdividw(nwe)
      
c
c ---------------------------------------------------------
c		a. Exit if the source well is off
      if(idivsww(nWE).eq.0) then
        iwhy=2
        cwhy='Source Well Structure is off'
        goto 260
      endif  
      
c
c ---------------------------------------------------------
c		b. Exit if the source well right if off
      if(idvrsww(nwR).eq.0) then
        iwhy=3
        cwhy='Source Well Right is off'
        goto 260
      endif  
      
c      
c ________________________________________________________
c               Step 3; Set augmentation plan, if any, associatd with
c		        the recharge well pumping depletions
c		ipAug  = T&C plan
c		ipUse = Reuse plan
      ipAug=ireuse(l2)
      
      if(ipAug.gt.0) then
        cTandC='Yes'
        pobl1=pobl(imo,ipAug)
      endif  
c
c _________________________________________________________
c               Step 4; Destination a Reservoir (nr)
c
      Nr  =Iopdes(1,L2)
      iresw=1
      isres = irssta(nr)      
      cdestyp='Reservoir'
        
      if (iressw(nr).eq.0) then
        iwhy=4
        cwhy='Destination Reservoir is Off'
        goto 260
      endif  
c
c ---------------------------------------------------------
c rrb 2006/09/25; a. Allow multiple accounts - Initilize
      nro=1
      if(iopdes(2,l2).lt.0) then
        nro=-iopdes(2,l2)
        irow=nowner(nr)
      endif

      if(iopdes(2,l2).gt.0) then
        irow=nowner(nr)+iopdes(2,l2)-1
        nro=1
      endif
c      
c ---------------------------------------------------------
c		b. Check reservoir roundoff when entering
c			Note in1=0 into a routine, 1 out of a routine
c			     isub1 = subroutine calling chekres
      in1=0
      isub1=26
      call chekres(nlog, maxres, in1, isub1, iyr, mon, nr,nowner,
     1               curown,cursto,cresid)
c
c _________________________________________________________
c
c               Step 5. Destination reservoir Demand (iresw=1)
c			 Set Demand based on carrier loss
c                        Limit to remaining capacity (volmax-cursto)
c
c rrb 2006/09/25; a. Allow multiple accounts - Demand
      if (iresw.eq.1) then
        cursa=0.0
        do n=1, nro
          n1=irow+n-1
          cursa=cursa+(ownmax(n1)-curown(n1))
        end do  
        
        CapRem1=cursa/fac
        IF (CapRem1.lt.small) then
          divreqx1=CapRem1
          iwhy=5
          cwhy='Res Destination Capacity (Demand) = zero'            
          GOTO 260
        endif  
        
c
c ---------------------------------------------------------
c rrb 04/09/03; b. Allow carrier to a reservoir even if the 
c                  current storage is above the target
c		   when iressw(nd2) = 3 (see *.res documentation)
        if(iressw(nr).eq.3) then
          divalo=amin1(cursa,volmax(nr)-cursto(nr))/fac
        else        
          divalo=amin1(cursa,volmax(nr)-cursto(nr),
     1      tarmax(nr)-cursto(nr))/fac
        endif
c        
c ---------------------------------------------------------
c		c. Increase Demand to reflect Carrier Loss        
        divalo=amax1(0.0,divalo)
        divreqx1=divalo
        divalo=divalo/OprEff1
        
        divreqx2=divalo        
        
        IF (divalo.lt.small) then
          iwhy=6
          cwhy='Res Demand (Demand) = zero'            
          GOTO 260
        endif  
c      
c ---------------------------------------------------------
c		d. Exit if the demand is zero
c       
        if(divalo.lt.small) then
          iwhy=7
          cwhy='Plan Demand = 0'
          goto 260
        endif  
        
      endif  
c
c _________________________________________________________
c
c               Step 6; Check available flow at diversion (avtemp(iscd)
c		a. Check avail at well location (iscd)
      AvailX=avail(iscd)
      IF(Availx.le.small) then
        iwhy=8
        cwhy='Available flow (AvailX) equals zero'      
        goto 260
      endif
c
c ---------------------------------------------------------
c		b. Check avail downstream
      CALL DNMFSO(maxsta,Avail,IDNCOD,ISCD,NDNS,IMCD)
c
      availX=avail(imcd)
      imcdX=imcd
      IF(Availx.le.small) then
        AvailX=avtemp(iscd) 
        iwhy=9
        cwhy='Available flow (AvailX) equals zero'      
        goto 260
      endif

c
c _________________________________________________________
c
c               Step 7 Calculate pumping
c
c                 - remaining demand   (divreqx) 
c                                  aka (divreq(nd))
c                 - remaining right    (dcrdivx     - divdx)
c                                  aka (dcrdivw(l2) - divdw(l2))
c                 - remaining capacity (divcapx     - divmonx)
c                                  aka (divcapw(nd) - divmonw(nd))
      divcapx=divcapw(nWE)
      divmonx=divmonw(nWE)
      dcrdivx=dcrdivw(nWR)
      divdx=divdw(nWR)

c ---------------------------------------------------------
c		a. Exit if the remaining decree is zero

      dcrdivZ=dcrdivx-divdx
      if(dcrdivZ.le.small) then
        iwhy=10
        cwhy='Remaining Well Decree is zero'      
        goto 260
      endif  
c ---------------------------------------------------------
c		b. Exit if the remaining capacity is zero
      
      divcapZ=divcapx-divmonx
      if(divcapZ.le.small) then
        iwhy=11
        cwhy='Remaining Well Capacity is zero'      
        goto 260
      endif  
c
c ---------------------------------------------------------
c		c. Set Pumping
      DivAct=amin1(dcrdivx-divdx, divcapx-divmonx, divalo, availX)
c
c _________________________________________________________
c
c               Step 8; Take out of river for current & future times
c                        Note depletion is based on pumping and not 
c                        impacted by efficiency. 
c rrb 2006/03/21;        Note DepleteP stores the change to
c                        Avail from this well in temporary array Avtemp              
      if(ipAug.eq.0) then
        call deplete(DIVACT,depx,nWR,nWE)
      else  
        call depleteP(DIVACT,depx,nWR,nWE,ipAug)
      endif  
c
c ---------------------------------------------------------
c
c		a. Detailed Augmentation Output      
      if(iout.eq.3) then
        write(nlog,*) ' WelRech; avail after deplete'
        write(nlog,'(20f8.2)') (avail(i)*fac, i=1,numsta)
      endif  
c
c _________________________________________________________
c
c               Step 10; Add in return flows for all time steps
c		        Avail from this well in array Avtemp 
c			Note typically the return flow pattern
c			is immediate for a Recharge well.
c			However, this code allow it to be anything.             
c
cx rrb 2007/02/20; No return flows; pump to a reservoir
cx      ispr=0
cx      nd2=0
cx      if(ipAug.eq.0) then
cx        CALL RtnSecW(DIVACT,retx2,rlossX2,cuact2,
cx   1                 l2,ISCD,nwe,nd2,ieff2,ispr)
cx      else
cx        CALL RtnSecWP(DIVACT,retx2,rlossX2,cuact2,
cx   1                 l2,ISCD,nwe,nd2,ieff2,ispr,ipAug)
cx      endif
c      
c ---------------------------------------------------------
c
c		a. Detailed Output
      if(iout.eq.3) then
        write(nlog,*) ' WelRech; avail after rtnsecw'
        write(nlog,'(20f8.2)') (avail(i)*fac, i=1,numsta)
      endif  
c     write(nlog,*) ' WelRech; divact, retx = ', divact*fac,retx*fac
c
c _________________________________________________________
c
c		Step 11; Check if (Avail) was driven negative,
c		  NOTE should never occur as designed but keep
c		  logic in case something changes later.
c                 by looking at the entire array Avail, not just
c                 of the well. IF avail is <0, then
c                 (a) Find a negative avail
c                 (b) Calculate gw2riv to make avail zero
c                 (c) Route gw2riv downstream
c		
c ---------------------------------------------------------
c
c              a Find negative
      call dnmfsow(maxsta, avail, numsta, imcd)
      Avail1=-1.*avail(imcd)
      if(avail(imcd).lt.(-1.*small)) then
c
c rrb 00/05/03; Check entire array, not just downstream         
        do nx=1,numsta
          iss=nx
c
          if(avail(iss).lt.(-1.*small)) then
c
c ---------------------------------------------------------
c
c              b Calculate from GW storage gw2riv
            gx = avail(iss)
            gxTot=gxTot+gx
            
            gw2riv(iss)=gw2riv(iss) - gx
c
c ---------------------------------------------------------
c
c              c Route gw2riv downstream
            ndns1=ndnnod(iss)
            CALL TAKOUT(maxsta, AVAIL, RIVER, AVINP, QTRIBU, idncod,
     1                  gx,     ndns1, iss)
c
c		Endif for negative avail            
          endif
        end do
      
        if(iout.eq.3) then
          write(nlog,*) 
     1      ' WelRech; avail after GW adjustments gx = ',gx*fac
          write(nlog,'(20f8.2)') (avail(i)*fac, i=1,numsta)
        endif  
        
c
c		End adjustments for negative avail     
      endif
c
c _________________________________________________________

c
c              Step 12; Set Plan Obligation for CURRENT depletions 
c                       caused by this well at this time step.
c			Note array Avtemp has the current impact 
c			(depletion is a - and return flow is a +) of 
c                       this well. Therefore the current Augmentation
c                       obligation (Pobl( )) is the maximum impact
c                       downstream of the well (Balace Point)
c			Note Avtemp includes net - depletion + returns
c rrb; 2006/03/26; 
      if(ipAug.gt.0) then   
c
c rrb 2006/04/02; Plan Driver is the amount pumped
        pdrive(ipAug)=pdrive(ipAug) + divact
c
c
c ---------------------------------------------------------
c		a. Set the net plan obligation (PNetObl) to the
c                  mininum value in Avtemp that contains the impact 
c                  of this well only on the entire network.
c                  Note call dnmfso (not dnmfsoW) to get min from the
c	           well (balance point) downstream
        call dnmfso(maxsta, avtemp, idncod, iscd, ndns, imcP)
        cidBal=cstaid(imcP)
        pNetObl = -1*avtemp(imcP)
c
c ---------------------------------------------------------
c
c		b. Adjust current obligation pobl(imo, )   
        pobl1=pobl(imo,ipAug)
        pobl(imo,ipAug)=pobl(imo,ipAug) + pNetObl
        pobl2=pobl(imo,ipAug)
c
c ---------------------------------------------------------
c
c		c. Adjust current total demand PdemT()        
        pdem1=pdem(ipAug)
        pdem(ipAug)=pdem(ipAug) + pNetObl        
        pdem2=pdem(ipAug)
        
        pdemT(ipAug)=pdemT(ipAug) + pNetObl
c
c ---------------------------------------------------------
c
c		d. Detailed Well Augmentation output      
        if(iout.eq.3 .and. iw.eq.ioutiw) then
          ncallX=ncallX+1
          if(ncallX.eq.1)then
            write(nlog,262) cidWR,cdestyp
          endif  


          write(nlog,264) '  WelRech   ',
     1      iyrmo(mon),xmonam(mon), idy, iwx, cstaid1, cidRiv, cidBal,
     1      divact*fac, cuact*fac, rlossX*fac, rettot*fac, totX*fac,
     1      depX*fac,  retX*fac, (depX-retX)*fac, avail1*fac,
     1      pdem1*fac,   pNetObl*fac, pdem2*fac, 
     1      pwellP1*fac, gxTot*fac,   pwellP2*fac 
     
        endif  
      endif 
c
c
c _________________________________________________________
c
c              Step 13. Double Check available flow
c
c rrb 00/05/03; Check entire array, not just downstream               
      call dnmfsow(maxsta, avail, numsta, imcd)
      avail2=avail(imcd)
      
      IF(AVAIL(IMCD).le.(-1.*small)) then
        WRITE(nlog,310) IYR,MON,IW,NWRORD(1,IW),nwr,nwe,DIVREQx1,
     1                ISCD,ISCD,IMCD,DIVACT,avail(imcd),gw2riv(imcd)
        write(nlog,320) (avail(iss),iss=1,numsta)
        write(nlog,330) (river(iss),iss=1,numsta)
        goto 9999
      endif
c       
c
c _________________________________________________________
c
c               Step 14; Update Running Variables
c		  Note future demands for the pumping well were
c		  set in Deplete and RtnsecWP
c                 Note qdiv(25,is) is set in deplete
c                 - remaining demand   (divreqx1) 
c                                  aka (divreq(nd))
c                 - remaining right    (dcrdivx     - divdx)
c                                  aka (dcrdivw(l2) - divdw(l2))
c                 - remaining capacity (divcapx     - divmonx)
c                                  aka (divcapw(nd) - dcrdivW(nd))
c
c		a. Well data
      USEMONw(nwe)=USEMONw(nwe)+DIVACT
      DIVMONw(nwe)=DIVMONw(nwe)+DIVACT      
      divdw(l2)  =divdw(l2)  +divact  
          
      divo(l2)=divo(l2)+DIVACT
c jhb 2014/07/13 array bounds check
c                handle the case when ipAug = 0
      if (ipAug.gt.0) then
        pdem(ipAug)=pdem(ipAug)-DIVACT
      endif
      carryW(nWE) = carryW(nWE)+Divact
c
c rrb 2007/02/21; Add Wells to *.xdd        
      qdiv(18,iscd)=qdiv(18,iscd)+divact
      qdiv(24,iscd)=qdiv(24,iscd)+divact

c
c ---------------------------------------------------------
c               b. Update reservoir destination data
      if(iresw.eq.1) then
        divaf=divact*fac
        divafL=divaf*OprEff1
        cursto(nr)=cursto(nr)+divafL
        qres(2,nr)=qres(2,nr)+divaf
c
c ---------------------------------------------------------        
c               c. Distribute Diversion to accounts
c rrb 2006/09/25; Revised to work with multiple reservoir
c		  Note:
c		   iResT1=0 distributes based on ownership ratio
c		   nrown1=number of accounts in this reservoir
c		   iown = first account associated with this reservoir        
c		   icx = calling routine 2=divcar
c		   ia   = account to adjust (2=From River by Storage)
        iResT1=0
        nrown1=nro
        iownX=irow
        icx=111
        ia=2
        cresid1=cresid(nr)
c          
        call accou(maxacc, maxown, nr, ownmon, curown, accr, ia,
     1    ownmax, iownX, nrown1, cursa, divafL, iResT1,icx, cresid1)
c
c ---------------------------------------------------------        
c		d. Check reservoir roundoff when exiting routine
c			Note in1=0 into a routine, 1 out of a routine
c			     isub1 = subroutine calling chekres
        in1=1
        isub1=26
        call chekres(nlog, maxres, in1, isub1, iyr, mon, nr,nowner,
     1                     curown,cursto,cresid)

      endif
c_____________________________________________________________
c rrb 2007/02/23
c               Step 15; Identify call (note only if short)
c
      if(divreqx1-divact.gt.small) ishort=1
      if(ishort.eq.1) then
        ctype1='Well        '
        call GetCall(iscd, imcdL(iscd), nw, ctype1)        
      endif  
      
      
c _________________________________________________________
c
c               Step 15.  Detalied output
c

 260  if(iout.ge.2 .and. iw.eq.ioutiw) then
c
c ---------------------------------------------------------
c		a. Header for this time step
c
        ncallX=ncallX+1
        if(ncallX.eq.1)then
          write(nlog,270) cidWR,cdestyp, ccarry,cTandC
        else
c          write(nlog,*) ' '
        endif  
c
c ---------------------------------------------------------
c		b. Data for every time step and iteration
c
        write(nlog,280) '  WelRech   ',
     1    iyrmo(mon),xmonam(mon), idy,
     1     cstaid1,iwx, iw,nwe,nwr,nr,
     1     DIVREQx1*fac, AvailX*fac, 
     1     dcrdivZ*fac, DIVCAPz*fac,
     1     DIVMONx*fac, divdw(l2)*fac, 
     1     gw2riv(iscd)*fac, pdem1*fac, pdem2*fac,
     1     DIVACT*fac, iwhy, cwhy
     
  280     FORMAT(a12, i5,1x,a4, i5, 1x,a12, 5i8, 10F8.1,
     1    i5, 1x, a48)
     
      endif
      
c
c _________________________________________________________
c
c               Step 16; Check Avail for Roundoff issues
      call chekava(19, maxsta, numsta, avail)
      
c
      if(iout.eq.1) then
        write(nlog,*) '  WelRech; Problem at bottom',iyr,mon,iw,nwr
        write(nlog,380) crigidw(l2), namedw(l2)
        goto 9999
      endif 
c_____________________________________________________________
c               Step 17; Identify call (note only if short)
c
      if(ishort.gt.0) then
        ctype1='Well        '
        call GetCall(iscd, imcdL(iscd), nw, ctype1)        
      endif  
                                        
c
c _________________________________________________________
c
c                Step 17; Return
      RETURN
c
c _________________________________________________________
c
c                Formats
c
  200   format(/, '  WelRech; Problem with sprinkler calcs (Total)',/
     1         10x, i5, 1x, a12,/ 
     1         10x,'   isprink      effs      efff      effa' ,/
     1         10x, i10, 3f10.2,/
     1         10x,'   divsprx   divothx   divreqx1    small',/   
     1         10x,  20f20.0) 
  240   format(/,
     1         '  WelRech; mon, l2, nd,iuse,iscd,ndnsx',/,10x,20i5)
 
  250   format(/,'  WelRech (Total)',/
     1  '  WelRech; divreqx1,divcapx,divmonx,dcrdivx,divdx,',
     1           'divsprx, divothx',/,10x, 20f8.0)
  262   format(/, 
     1  '  WelRech Augmentation Summary;  Well Right ID = ', a12,
     1  ' Destination Type = ', a12,/,
     1  '  Note NetDep is independent of location while NetObl ',
     1        'is not.',/
     1  '       The critical location is at river node Bal_ID ',
     1        'located',/
     1  '       downstream of the structures location Riv_ID',
     1  //
     1  '  WelRech     iyr  mon  idy  iwx Str_ID       Riv_ID      ',
     1  ' Bal_ID      '
     1  '  Divact      CU    Loss  RetTot    TotX    Dep1    Ret1',
     1  '  NetDep  AvailX   Pdem1  NetObl   Pdem2 PwellP1   gxTot',
     1  ' PwellP2',/
     1  '____________ ____ ____ ____ ____',3(' ____________'),
     1  15(' _______'))
  
  264     FORMAT(a12, i5,1x,a4, 2i5, 3(1x,a12), 20F8.1)
  270   format(/, 
     1  '  WelRech (Type 37); Operation Right ID = ', a12,
     1  ' Destination Type = ', a12,
     1  ' Carrier (Y/N) = ',a3, ' Associated Plan (Y/N) = ', a3/    
     1  '  WelRech     iyr  mon  idy Source_ID   ',
     1  '   Iter#  Right#     Nwe     Nwr      Nr',
     1  '  Demand  AvailX DcrDivZ DivCapZ DivMonx',
     1  '   divdw  gw2riv   Pdem1   Pdem2 DIVACT',
     1  ' iwhy cwhy',/
     1  '____________ ____ ____ ____ ____________',
     1  5(' _______'), 10(' _______'),' ____', 1x, 48('_'))
     

  310   FORMAT(/,'  WelRech Print 5',6I10,F10.2,3I10,10F10.2,f20.10)
  320   format(   ' WelRech: avail  ',10f10.2)
  330   format(   ' WelRech: river  ',10f10.2)
  380   format(   '  WelRech; Problem with ', a12, 1x, a24)

c
c _________________________________________________________
c
c              Error warnings
c
 9999 write(6,340) 
      write(nlog,350) 
      call flush(6)
 340  format('    Stopped in WelRech',/,
     1       '    See the *.log file')
 350  format('    Stopped in WelRech')
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop 
      END

