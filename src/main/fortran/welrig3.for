c
c *********************************************************
c
      SUBROUTINE welrig3(IW,L2,ispruse,retx,divact,ncallX)
c
c
c _________________________________________________________
c	Program Description
c
c       Welrig3; It simulates diversions (pumping) by a well
c
c_____________________________________________________________
c
c       Update History
c
c rrb 2007/09/27; Copied WelRig2.
c		  Revised to focus on IWR as the indicator for
c		  a well demand, not total headgate demand.
c rrb 2007/11/23; Revised to handle M&I structures (Area=0 and/or n=0)
c               
c _________________________________________________________
c       Documentation
c
c        IW             Global water right ID
c        L2             LOC. OF operation right  in opr RIGHT TABLE
c        nwR            source water right
c
c        ishort         code for reoperation; 0=no, 1=yes
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
c        divgw          For options 4 or 5 same as divreqw but GW demand
c                       is limited to IWR / efficiency of gw
c        dcrdivw(l2)    Well Decree (cfs) from Riginp
c        divdw(l2)      Well Decree diverted in previous iterations
c        divcapw(nwe)   Well capacity (cfs) from Datinp
c        divmonw(nwe)   Well capacity (cfs) used in previous iterations
c
c        diveff(mon,nd) Average diversion efficiency via *.dds
c        diveffw(mon,nwe)Average well efficiency via *.wes
c
c        effmaxs(nwe)   Maximum sprinker efficiency via *.tsp
c        effmaxw(nwe)   Maximum flood efficiency via *.tsp
c
c        effd           Average diversion efficiency via *.dds
c        effa           Average well efficiency via *.wes
c
c        effs1          Maximum sprinker efficiency via *.tsp
c        efff1          Maximum flood efficiency via *.tsp 
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
c        idvstaw(nwe)    River location of well
c        idivcow2(nwe)   0=GW only structure
c                       +=diversion tied to well structure +
c        (nd2)          scalar for above
c
c        idivsww        on/off switch (0=off, 1=on)
c        idivcow2(nw)   SW diversion, if any, associated with well nw 
c        iscdx          River location of well (iscdx = idvstaw(nwe))
c
c        ispr           =0 use flood efficiency in rtnsecw
c                       =1 use sprinker efficiency in rtnsecw
c
c        ispruse        =0 called by subroutine execut
c                       =1 called by subroutine suruse for sprinkler use
c                       water first 
c
c        itsfile        Switch via datinp (*.ctl)
c                       0=no GW acres by year provided
c                       1 = yes GW acres by year provided and demand
c                       is limited by amount via bomsec.f
c        iuse           User = nd for well structure
c                            = nduser(nwe) for a diversion structure
c                       Note: multi user option is turned off in datinp!
c
c        nd             well ID 
c        nd2            0=GW only structure
c                       +=diversion tied to well structure +
c			Note nd2 = idivcow2(nwe) above
c
c        ndnnod(iscdx)  Number of downstream nodes
c        ndnsx          Number of downstream nodes (ndnsx=ndnnod(iscdx)
c
c
c        qdiv(24,iscdx) Pumping (diversion) by a well to a user at iscdx
c        qdiv(25,iscdx) Depletion (From River by Well) at river ID iscdx
c
c        retx           Immediate (this day or month) return.
c                       Used for reoperation control along with
c                       variable ireop set in rtnsecw.for
c        small          a small value for roundoff (0.0) concerns
c
c_____________________________________________________________
c	Dimensions
c
      include 'common.inc'
      character cwhy*48, cdestyp*12, ccarry*3, cpuse*3, cstaid1*12,
     1          rec12*12, cTandC*3, cidWR*12, ctype1*12, cStrOut*12
c
c
c _________________________________________________________
c       Step 1 Common Initilization
c
c		iout = 0 no details
c		       1 details
c                      2 summary      
c		       3 super detail
c		       4 selected ID

      iout=0
      ioutiw=0
      
      if(ichk.eq.206) iout=2
      if(crigidw(l2).eq. ccall) ioutiw=iw
c     write(nlog,*) ' WelRig3; ', iout, ioutiw, iw
c
c			         Detailed structure reporting         
      nwe =idivcow(1,L2)
      cstaid1='NA'      
      if(nwe.gt.0) cstaid1=cdividw(nwe)
      cStrOut='NA'
cx    cStrOut='01_AWP031   '
            
c     if(cstaid1.eq.'200631      ') iout=4

c
c		Search based on station id      
      nwe =idivcow(1,L2)
      if(cdividw(nwe).eq.ccall) ioutiw=iw     
      
c     write(Nlog,*) ' WelRig3; iout, ioutiw, crigidw(l2), ccall', 
c    1                         iout, ioutiw, crigidw(l2), ccall
        
      if(iday.eq.0) then
        f=mthday(mon)*factor
      else
        f=factor
      endif
      fac=f
      
      iwhy=0
      cwhy='NA'
      cdestyp='Diversion'
      ccarry='No'
      cpuse='No'
      cTandC='No'
      
      divact = 0.0
      divalo = 0.0
      divreqx1=0.0
      divcapZ=-1/f
      dcrdivZ=-1./f
      pdem1=-1./f
      pdem2=-1./f
      Avail1=-1./f
      Avail2=-1./f
      DivreqX=-1/f
      divreq2=-1/f
      
      cidWR=crigidw(l2)

      small = 0.001
      retx  = 0.0
      
      retx1=0.0
      retx2=0.0
c 
c rrb 00/12/26; Set variable efficiency (1=on, controlled by ieffmax)
      ieff2 =1                               
c      
c rrb 01/01/13; Set default efficiency to non sprinklers (ispr=0)
      ispr=0 
c
c               d. Check Avail array
      call chekava(19, maxsta, numsta, avail)
c
c               e. Set river avail array to temporary avtemp
      do is=1,numsta
        avtemp(is)=avail(is)
      end do
c
c _________________________________________________________
c
c		Step 2; Set Source 1 a well water right
cr    Nwr  =Iopsou(1,L2)
      Nwr  = l2
c
c _________________________________________________________
c               Step 3; Destination (nwe) a well
c     nd =Iopdes(1,L2)
      nwe =idivcow(1,L2)
      
      cstaid1=cdividw(nwe)
c
c		Set imcd to well station for detailed output
      iscd=idvstaw(nwe)
      NDNS=NDNNOD(ISCD)
      imcd=idvstaw(nwe)
c _________________________________________________________
c
c		Step 4; Exit if the structure is off (idivsww = 0)
      if(idivsww(nwe).eq.0) then
        iwhy=1
        cwhy='Well is off'
        goto 260
      endif  
c _________________________________________________________
c
c 	   	Step 5; Set Efficiency
c                 Set nd2, the diversion associated with this well
c		  Note if nd2=0 Well Only, 
c                      if ndw>0 Diversion and Well
      nd2=idivcow2(nwe)
      iuse=0
      if(nd2.gt.0) iuse=nduser(nd2)

      
      if(iout.eq.1) then
        write(nlog,*) ''
        write(nlog,*) ' ___________________________________'
        write(nlog,*) ' WelRig3; ID = ', nwe, nd2, cdividw(nwe)
      endif 
c _________________________________________________________
c
c               Step 6; Set variable data to scalars
      iscdx=idvstaw(nwe)
      ndnsx=ndnnod(iscdx)
      ndnsx=ndnsx
      divcapx=divcapw(nwe)
      divmonx=divmonw(nwe)
      dcrdivx=dcrdivw(l2)
      divdx=divdw(l2)
c
c               a. Set efficiency data
      effa=diveffw(mon,nwe)/100.0
      effS1=effmaxs(nwe)/100.0
      effF1=effmaxw(nwe)/100.0
c
c rrb 00/06/16; 
c               b. Get ratio of water use efficiency
      if(nd2.gt.0) then
        effd = diveff(mon,nd2)/100.0
        if(ieffmax.le.0) ceff = effd/effa
        if(ieffmax.eq.1) ceff = effd/efff1
      else
        effd = 0.0
        ceff = 1.0
      endif
c
c               c. Demand type
      if(idemtyp.ge.3) then
        ceff=1.0
      endif
c _________________________________________________________
c
c               Step 7; Set demand
c	
c ---------------------------------------------------------
c		Demand type 1(Historic) D&W or Well Only 
c		Note cannot use IWR in case SW was shorted or
c		Acres = 0
c
c rrb 2009/06/22; Correctin
cx    if(nd2.eq.0) AreaT=AreaGfw(nw) + AreaGsw(nw)
      if(nd2.eq.0) AreaT=AreaGfw(nw) + AreaGsw(nwe)
      if(nd2.ne.0) AreaT=AreaGf(nd2) + AreaGs(nd2)
c      
c ---------------------------------------------------------
c		Demand type 1 (Historic)
c		Well Only Structures      

      if(idemtyp.eq.1 .and. nd2.eq.0) then
        if(AreaT.gt.small) then
          divreqX=divreqw(nwe)
          divsprX=divreqX*AreaGSw(nwe)
          divothX=divreqX*AreaGFw(nwe)           
        else
          divreqX=divreqw(nwe)
          divsprX=0.0
          divothX=divreqX
        endif  
        diwrGS1=divsprX*EffS1
        diwrGF1=divothX*EffF1                
      endif
c      
c ---------------------------------------------------------
c		Demand type 1 (Historic)
c		D&W Structures      
      if(idemtyp.eq.1 .and. nd2.gt.0) then
        if(AreaT.gt.small) then
          divreqX=divreqw(nwe)
          divsprX=divreqX*AreaGS(nd2)
          divothX=divreqX*AreaGF(nd2)
        else
          divreqX=divreqw(nwe)
          divsprX=0.0
          divothX=divreqX
        endif  
        diwrGS1=divsprX*EffS1
        diwrGF1=divothX*EffF1                
      endif
c
c ---------------------------------------------------------
c		Demand type > 2 (non historic)
c		Well Only Structures      
      if(idemtyp.ge.2 .and. nd2.eq.0) then  
        if(AreaT.gt.small) then
          divreqX=divreqw(nwe)
          divsprX=divreqX*AreaGSw(nwe)
          divothX=divreqX*AreaGFw(nwe)
        else
          divreqX=divreqw(nwe)
          divsprX=0.0
          divothX=divreqX
        endif
        diwrGS1=divsprX*EffS1
        diwrGF1=divothX*EffF1                
      endif  
c
c ---------------------------------------------------------
c		Demand type > 2 (non historic)
c		D&W Structures
      if(idemtyp.ge.2 .and. nd2.ge.1) then  
        if(AreaT.gt.small) then
          divreqX=divreq(iuse)
          divsprX=divreqX*AreaGS(nd2)
          divothX=divreqX*AreaGF(nd2)          
        else  
          divreqX=divreq(nd2)
          divsprX=0.0
          divothX=divreqX
          diwrGS1=0.0
          diwrGF1=divothX*EffF1            
        endif
        diwrGS1=divsprX*EffS1
        diwrGF1=divothX*EffF1                
      endif  
      
      divreqX1=divreqX
      
      if(iout.eq.4) then
        write(nlog,*) ' WelRig3; ', iyrmo(mon),xmonam(mon),
     1   cstaid1, idemtyp, nd2, AreaT, 
     1   divreqX1*fac, divsprX*fac, divothX*fac,
     1   EffS1*100., effF1*100.
      endif
c _________________________________________________________
c
c               Step 11; Exit if no demand
      
      if(divreqx.le.small) then
        iwhy=2
        cwhy='Demand is zero'      
        goto 260 
      endif
c _________________________________________________________
c
c               Step 14; Detailed printout
      if(iout.eq.1) then
        write(nlog,250)
        write(nlog,252) cstaid1, iyrmo(mon),xmonam(mon), idy,
     1   nwe,  nd2,   diwrGF1*f,   diwrGS1*f,   efff1*100., effS1*100., 
     1               divsprX*f,   divOthX*f,   divreqX*f
      endif
c
c _________________________________________________________
c
c               Step 15 Calculate allowable pumping
c
c                 - remaining demand   (divreqx) 
c                                  aka (divreq(nwe))
c                 - remaining right    (dcrdivx     - divdx)
c                                  aka (dcrdivw(l2) - duvdw(l2))
c                 - remaining capacity (divcapx     - divmonx)
c                                  aka (divcapw(nwe) - duvninw(nwe))
      divalo=amin1(dcrdivx-divdx, divreqx, divcapx-divmonx)
      divalo=amax1(0.0,divalo)
      
      dcrdivZ=dcrdivx-divdx
      if(dcrdivZ.le.small) then
        iwhy=3
        cwhy='Remaining decree is zero'      
        goto 260
      endif  
      
      divcapZ=divcapx-divmonx
      if(divcapZ.le.small) then
        iwhy=4
        cwhy='Remaining capacity is zero'      
        goto 260
      endif  
      
c
      DIVACT=DIVALO
c
c_____________________________________________________________
c               Step 17; Find mininum downstream station from
c		         the well
c
c     write(nlog,*) ' Welrig3; Step 17'
      CALL DNMFSO(maxsta,avail,IDNCOD,ISCD,NDNS,IMCD)
      avail0=avail(imcd)
      
c
c _________________________________________________________
c
c               Step 16; Take out of river for current & future times
c               Note depletion is based on pumping and not impacted
c               by efficiency
c
      if(iout.eq.3) then
        write(nlog,*) 
        write(nlog,*) ' WelRig3; before deplete'
        write(nlog,'(20f10.0)') (avail(i)*f, i=1,numsta)
      endif  
c
c rrb 2007/10/03; Revise nd=nwe      
c     call deplete(DIVACT,depx,nwr,nd)
      call deplete(DIVACT,depx,nwr,nwe)
      
      if(iout.eq.3) then
        write(nlog,*) 
        write(nlog,*) ' WelRig3; avail after deplete'
        write(nlog,'(20f10.2)') (avail(i)*f, i=1,numsta)
      endif  
      
c
c _________________________________________________________
c               Step 17; Separate into sprinker and other use
      divsprx=amin1(divsprx, divact)
      divothx=amax1(divact-divsprx, 0.0)
c
c _________________________________________________________
c
c               Step 18; Add in return flows for all time steps
c                       once for sprinklers and once for other
c                       Note:
c			ispr=1 uses sprinker efficiency
c			ispr=0 uses flood efficiency
c  
      if(divsprx.gt.small) then
        ispr=1
c
c rrb 2007/10/03; Update nd=nwe        
c       CALL RTNSECw(divsprx,retx1,rlossX1,cuact,
c    1               nwr,ISCDx,nd,nd2,ieff2,ispr)
        CALL RTNSECw(divsprx,retx1,rlossX1,cuact,
     1               nwr,ISCDx,nwe,nd2,ieff2,ispr)
      endif

      if(divothx.gt.small) then
        ispr=0
c
c rrb 2007/10/03; Update nd=nwe        
c       CALL RTNSECw(divothx,retx2,rlossX2,cuact,
c    1               nwr,ISCDx,nd,nd2,ieff2,ispr)
        CALL RTNSECw(divothx,retx2,rlossX2,cuact,
     1               nwr,ISCDx,nwe,nd2,ieff2,ispr)
      endif
      retx=retx1+retx2

      if(iout.eq.3) then
        write(nlog,*) ' WelRig3; avail after rtnsecw'
        write(nlog,'(20f8.2)') (avail(i)*f, i=1,numsta)
      endif  
c     write(nlog,*) ' WelRig3; divact, retx = ', divact*f,retx*f
c
c _________________________________________________________
c
c               Step 19; Check if (Avail) was driven negative,
c
c rrb 00/05/03; Check entire array, not just downstream               
      call dnmfsow(maxsta, avail, numsta, imcd)
      if(imcd.gt.715) write(nlog,*) ' Welrig3; Step 19 imcd = ', imcd
c
c _________________________________________________________
c
c               Step 20; If avail <0, diverting out of priority
c                IF avail is <0, then
c                 (20a) Find a negative avail
c                 (20b) Calculate gw2riv to make avail zero
c                 (20c) Route gw2riv downstream
c                 (20d) Calculate T&C Obligation
c		
      Avail1=avail(imcd)
      if(avail(imcd).lt.(-1.*small)) then
c
c rrb 00/05/03; Check entire array, not just downstream         
        do nx=1,numsta
          iss=nx
c
c              20a Find negative
          if(avail(iss).lt.(-1.*small)) then
c
c              20b Calculate gw2riv
            gx = avail(iss)
            gw2riv(iss)=gw2riv(iss) - gx
cr          write(nlog,*) '  WelRig3; Negative Avail iss, Gx, Gw2riv',
cr   1        iss, gx*f, gw2riv(iss)*f
c
c              20c Route gw2riv downstream
            ndns1=ndnnod(iss)
            CALL TAKOUT(maxsta, AVAIL, RIVER, AVINP, QTRIBU, idncod,
     1                  gx,     ndns1, iss)
c
c		Endif for negative avail            
          endif
        end do
      
        if(iout.eq.3) then
          write(nlog,*) 
     1      ' WelRig3; avail after GW adjustments gx = ',gx*f
          write(nlog,'(20f8.2)') (avail(i)*f, i=1,numsta)
        endif  
        
c
c		End adjustments for negative avail     
      endif
c
c
c _________________________________________________________
c
c              Step 22; Double Check available flow
c
c rrb 00/05/03; Check entire array, not just downstream               
      call dnmfsow(maxsta, avail, numsta, imcd)
      if(imcd.gt.715) write(nlog,*) ' Welrig3; Step 22 imcd = ', imcd
      Avail2=avail(imcd)

c
      IF(AVAIL(IMCD).le.(-1.*small)) then
        WRITE(nlog,310) IYR,MON,IW,nwr,nwe,
     1                ISCDx,ISCDx,IMCD,divreqx*fac, DIVACT*fac,
     1                avail(imcd)*fac,gw2riv(imcd)*fac
        write(nlog,*) ' '
        write(nlog,*) ' WelRig3; avail(imcd)', avail(imcd)*fac
c       write(nlog,320) (avail(iss),iss=1,numsta)
c       write(nlog,330) (river(iss),iss=1,numsta)
        goto 500
      endif
c
c _________________________________________________________
c
c               Step 23; Update Running Variables
c                Use by type (usemonw), remaining demand (divreqw or
c                divgw), remaining capacity (divmonw), remaining right
c                (divdw). Remaining diversion by sprinklers (divspr) 
c                Note qdiv(25,is) is set in deplete
c
      USEMONw(nwe)=USEMONw(nwe)+DIVACT
      DIVMONw(nwe)=DIVMONw(nwe)+DIVACT
      
      divdw(l2)  =divdw(l2)  +divact
c
c
c ---------------------------------------------------------
c                  Demand Type 1 Historic No addition of demands

      if(idemtyp.eq.1) then
        divreqw(nwe)=divreqw(nwe)-divact
        divreq2=divreqw(nwe)
c
c               c. For a D&W, set GW supply (qdiv(24,) for output
c                  to *.xdd, even if not added
        if(nd2.gt.0) then
          qdiv(24,iscdx)=qdiv(24,iscdx)+divact
c               Adjusted in return
c         diwrreq(nd2)=diwrreq(nd2)-cuact
        else
c
c rrb 2007/02/21; Add Wells to *.xdd        
          qdiv(24,iscdx)=qdiv(24,iscdx)+divact        
c               Adjsted in return
c         diwrreqw(nwe)=diwrreqw(nwe)-cuact 
        endif
        
      endif
c
c ---------------------------------------------------------
c rrb 00/06/16; Demand type 2 Historic Sum Appraoch
c		Demand type 3 Structure Demand Appraoch
c		Demadn type 4 Supply Demand Approach
c		Demand type 5 Decreed Demand Approach
      if(idemtyp.ge.2) then
        divreqw(nwe) = divreqw(nwe)-(divact / ceff) 
        divreq2=divreqw(nwe)
        qdiv(24,iscdx)=qdiv(24,iscdx)+divact        

        if(nd2.gt.0) then
          divreq(iuse) = divreq(iuse) - (divact / ceff)
          divreq(iuse) = amax1(divreq(iuse), 0.0)
        endif
      endif
c_____________________________________________________________
c rrb 2007/02/23
c               Step XX; Identify call if Well Only
c               (note only if short)
c
      if(divreqx1-divact.gt.small) ishort=1
      if(ishort.eq.1 .and. nd2.eq.0) then
        ctype1='Well        '
        call GetCall(iscd, imcdL(iscd), nw, ctype1)        
      endif  
      
c
c _________________________________________________________
c
c               Step 24.  Detalied output
c

 260  continue
      if(iout.ge.1 .and. iw.eq.ioutiw) then
        ncallX=ncallX+1
        if(ncallX.eq.1)then
          write(nlog,270) cidWR,cdestyp, idemtyp
        else
c          write(nlog,*) ' '
        endif  

cx        write(nlog,*) '  Welrig3  ', imo, qdiv(25, 246)*f,
cx     1    depl(imo,104)*f   

        write(nlog,280) '  WelRig3   ',
     1    iyrmo(mon),xmonam(mon), idy,
     1    cstaid1,iwx, iw,nwrord(1,iw),l2,nwr, nwe,
     1     DIVREQx*f, Avail0*f, Avail1*f, avail2*f,
     1     dcrdivZ*f, DIVCAPz*f,
     1     DIVMONx*f, divdw(l2)*f, 
     1     gw2riv(iscdx)*f, pdem1*f, pdem2*f, 
     1     divreq2*f, divact*f, iwhy, cwhy
  280     FORMAT(a12, i5,1x,a4, i5, 1x,a12, 6i8, 13F8.1,
     1    i5, 1x, a48)
     
      endif
c
c _________________________________________________________
c
c               Step 25; Check Avail for Roundoff issues
      call chekava(19, maxsta, numsta, avail)
      
c
cx      if(iout.eq.1) then
cx        write(nlog,*) '  WelRig3; Problem at bottom',iyr,mon,iw,nwr
cx        write(nlog,380) crigidw(l2), namedw(l2)
cx        goto 500
cx      endif                                   
c
c _________________________________________________________
c
c                Step 26; Return
cx    if(ichk.eq.94) write(nlog,*) ' WelRig3; Exiting'
c

      RETURN
c
c _________________________________________________________
c
c                Formats
c
  200   format(/, '  WelRig3; Problem with sprinkler calcs (Total)',/
     1         10x, i5, 1x, a12,/ 
     1         10x,'   isprink      effs1      efff1      effa' ,/
     1         10x, i10, 3f10.2,/
     1         10x,'   divsprx   divothx   divreqx     small',/   
     1         10x,  20f20.0) 
  240   format(/,
     1         '  WelRig3; mon, l2, nwe,iuse,iscdx,ndnsx',/,10x,20i5)
 
  250   format(/,'  WelRig3; Demand Data',/
     1  '  WelRig3; ID            iyr  mon  idy   nd  nd2',
     1  '   diwrGF1   diwrGS1     efff1     effS1', 
     1  '   divsprX   divOthX   divreqX')
  252   format( 11x,a12, i5, a5, 3i5, 20f10.0)

  270   format(/, 
     1  '  WelRig3;  Well Right ID = ', a12,
     1  ' Destination Type = ', a12,' Demand Type = ', i5,//
     1  '  WelRig3     iyr  mon  idy Str_ID      ',
     1  '     iwx      iw  nwrord      l2     NWR      ND',
     1  ' DIVREQx  Avail0  Avail1  Avail2 DcrDivZ DivCapZ DivMonx',
     1  '   divdw  gw2riv   Pdem1   Pdem2 Divreq2  DIVACT',
     1  '  iwhy cwhy',/
     1  '____________ ____ ____ ____ ____________',
     1  6(' _______'), 13(' _______'),' ____', 1x, 48('_'))
     

  310   FORMAT(/,
     1  '  WelRig3; Problem negative avail',/
     1  '  WelRig3;  iyr  mon   iw  nwr   nd iscd iscd imcd',
     1  '   divreqx    divact     avail    gw24iv',/
     1  '  ________ ____ ____ ____ ____ ____ ____ ____ ____',
     1  ' _________ _________ _________ _________',/   
     1  '  WelRig3;', 8I5,20F10.2)
     
  320   format(   ' WelRig3: avail  ',10f10.2)
  330   format(   ' WelRig3: river  ',10f10.2)
  380   format(   '  WelRig3; Problem with ', a12, 1x, a24)

c
c _________________________________________________________
c
c              Error warnings
  500   continue
c       if(ncallX.eq.1)then
          write(nlog,270) cidWR,cdestyp
c       endif  
        cwhy='Problem'

        write(nlog,280) '  WelRig3   ',
     1    iyrmo(mon),xmonam(mon), idy,
     1    cstaid1,iwx, iw,nwrord(1,iw),l2,nwr, nwe,
     1     DIVREQx*f, Avail0*fac, Avail1*f, avail2*f,
     1     dcrdivZ*f, DIVCAPz*f,
     1     DIVMONx*f, divdw(l2)*f, 
     1     gw2riv(iscdx)*f, pdem1*f, pdem2*f, 
     1     divreq2*f, divact*f, iwhy, cwhy
 
c
c --------------------------------------------------------- 
      write(6,340) 
      write(nlog,350) 
      call flush(6)
 340  format('    Stopped in WelRig3',/,
     1       '    See the *.log file')
 350  format('    Stopped in WelRig3')
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop 
      END

