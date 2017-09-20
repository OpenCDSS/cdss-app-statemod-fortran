c
c *********************************************************
c
        subroutine rtnmax(
     1   iter, iuse, iri, ire, iscd, ndns, 
     1   small, pavail, ieff2, ioutZ, cCallBy, corid1)
c
c _________________________________________________________
c	Program Description
c
c       Rtnmax; it calculates maximum diversion allowable
c               (pavail) with returns in a given reach using 
c               IF ieffmax=1 a  maximum  efficiency approach 
c               If ieffmac=0 or 2 a constant efficiency approach.
c
c               Note it is used instead of rtnsec because it 
c               calculates a maximum, not actual. Also it
c		does not adjust Avail, the available water supply
c
c       Called by: Divcar, Divcar1, DivcarR & DsaMod
c _________________________________________________________
c	Called By: 
c   DivCar.for,  Divcar1.for, DivCarL.for, DivCarR.for, DIVRIG.FOR 
c   DivRigS.for, DsaMod.for,  OopDiv.for,  RTNMAX.FOR,  RTNMAXE.FOR
c   RtnSecX.for, TAKOU2.FOR 
c _________________________________________________________
c	Update History
c
c
c rrb 2008/06/24; Revised to recognize 4 land use types
c		
c
c _________________________________________________________
c       Documentation
c
c               iter = iteration (ranges from 1 to 2)
c                      1 efficiency = variable (additional CU possible)
c                      2 efficiency = 0% (no additional CU)
c               iuse = diversion user
c               iri  = return flow pointer to beginning index
c               ire  = return flow pointer to ending index
c               iscd = diversion station (iscd = idvsta(nd)       
c               ieffmax = variable efficiency 0=off, 1=on  
c               iday = daily model switch

c               ndns = number of downstream nodes from diversion 
c               small= small convergecy number passed into the routine
c		smallX=  small convergence number used herein
c
c
c               ndnr = number of downstream nodes from return location
c               avwret=temporary array of return %
c               avtemp=temporary array of available flow (avail)
c		
c
c               AreaSF= Area SW Flood (fraction)
c               AreaSF= Area SW Sprinkler (fraction)
c               AreaSF= Area GW Flood (fraction)
c               AreaSF= Area GW Sprinkler (fraction)
c
c		dumx  = temproray array of return in cfs
c		effF  = Efficiency Flood (fraction)
c		effS  = Efficiency Sprnkler (fraction)
c
c		foret = return flow fraction
c               pavail=amount available                
c
c _________________________________________________________
c	Dimensions
c
       include 'common.inc'
       character corid1*12, cCallBy*12
c
c _________________________________________________________
c		Step 1; Initilze
c
c		iout=0 no details
c		iout=1 details
c		iout=2 detail on plan data      
c		iout=3 summary for weighted efficiency 
c
c		ioutL=1 details on return flow loop
cx    iout=3
cx    iout=0
cx    iout=ioutZ  

      isub=-2
      iout=0
      ioutL=0  
c
c ---------------------------------------------------------      
      if(iout.gt.0 .and. iter.eq.1) then
        write(nlog,320) cdivid(iuse), iout, 
     1    iyrmo(mon), xmonam(mon), idy

      endif  
      
      smallX=small
      smallX=0.00000001
      smallY=0.001
      pavail=0.0
c                                                          
c ---------------------------------------------------------
c               Daily capability
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif   
c
c ---------------------------------------------------------
c		Set effective efficiency (EffF1 and effS1)
c		as carrier * on farm
      effC1=effC(iuse)
      effF1=effF(iuse)*effC1
      effS1=effS(iuse)*effC1
            
c
c _________________________________________________________
c
c               Step 1; Calculate % return (foret)
c               	Note ieffmax = 0 variable efficiency OFF
c               	Note ieffmax = 1 variable efficiency ON
c
c
c ---------------------------------------------------------
c rrb 2006/10/31; Use average efficiency (ieff2=0) 
c		  if a structure is a carrier only
c rrb 2006/11/01; Turn Off, control by calling routine
c rrb 2006/11/03; Turned back on, critical for baseflow calculations
      itest=0
      if(itest.eq.0) then
        if(irturn(iuse).eq.3) then
          ieff2=0
          if(iout.eq.1) then
            write(nlog,330) cdivid(iuse), iuse, iuse, 
     1        irturn(iuse), diveff(mon,iuse)
          endif
        endif
      endif
c 
c ---------------------------------------------------------    
c rrb 2006/10/31; Use average efficiency (ieff2=0) 
c		  if a structure is a carrier only
      if(ieffmax.le.0 .or. ieffmax.eq.2 .or. ieff2.eq.0) then
        effx=diveff(mon,iuse)
        FORET=1.0-DIVEFF(mon,IUSE)/100.0
      else
c
c ---------------------------------------------------------
c rrb 2008/06/24; Update for 4 land use types . Note:
c		AreaSF, etc are fractions     
cx      effx=effmax(iuse)
cx      foret=1.0-effmax(iuse)/100.0
c               Step 1x; Adjust efficiency to 100 and 
c               fraction foret to 1.00 for second iteration
c		if using a maximum efficiency approach
        foret=1.0 - (AreaSF(iuse)*effF1+AreaSS(iuse)*effS1+
     1        AreaGF(iuse)*effF1+AreaGS(iuse)*effS1)     
c
c ---------------------------------------------------------
c rrb 2008/06/25; Use Old approach for M&I structures (area=0        
        AreaT=AreaSf(iuse) + AreaSs(iuse)+ AreaGf(iuse) + AreaGs(iuse)     
        if(AreaT.lt.smallX) then 
          effx=effmax(iuse)
          FORET=1.0-effmax(IUSE)/100.0
        endif  
c        
c ---------------------------------------------------------     
c		Adjust efficiency for iteration 2
        effx=foret*100.0
        if(iter.eq.2) then
          effx=100.0
          foret = 1.0
        endif          
      endif 
c      
c ---------------------------------------------------------  
c		Set total IWR   
      diwrz1=diwrsf(iuse)+diwrss(iuse)+diwrgf(iuse)+diwrgs(iuse)
      
c
c ---------------------------------------------------------
c		Detailed Output      
      if(iout.ge.1) then
        write(nlog,346)     
        write(nlog,'(9x, 5i8, 20f8.3)')
     1    iter, iri, ire, ieffmax, ieff2, AreaT, 
     1    effx, foret, effmax(iuse)
     
        write(nlog,348)
     
        write(nlog,'(9x, 3i8, 20f8.3)')
     1    iter, iuse, iuse, 
     1    effC1, effF1,effS1, foret, 
     1    AreaSF(iuse), AreaSS(iuse), AreaGF(iuse), AreaGS(iuse),
     1    diwrz1*fac,   diwrsf(iuse)*fac, diwrss(iuse)*fac, 
     1    diwrgf(iuse)*fac, diwrgs(iuse)*fac

      endif
c
c _________________________________________________________
c
c               Step 2; Set temporary array of return fraction
c               (avwret) and available flow (avtemp)
c
      DO IS=1,NUMSTA
        AVWRET(IS)=0.
        dumx(is)=0.0
        avtemp(is)=avail(is)
      end do
c
c ---------------------------------------------------------
c		Detailed Output
      call dnmfso(maxsta, AVTEMP, idncod, iscd, ndns, imcd)
      if(iout.eq.1)
     1  write(nlog,*) '  Rtnmax-1 ; imcd, avail', imcd,avail(imcd)*fac
c
c _________________________________________________________
c
c               Step 3; STEP THROUGH RETURN FLOWS FOR CURRENT DIVERSION

      DO 180 IRT=IRI,IRE
c
        IRCD =IRNSTA(IRT)
        NDNR =NDNNOD(IRCD)
        idly=irtndl(irt)
c
c _________________________________________________________
c
c               Step 4; Set delay for month or day (facdly)
c
c rrb 97/10/15; Daily model 
        if(iday.eq.0) then
          FACDLY=DLYRAT(1,IDLY)
        else
          facdly=dlyratd(1,idly)
        endif
c
c _________________________________________________________
c
c               Step 5; Set return fraction (RET)
c
        RET=PCTTOT(IRT)*FACDLY/10000.
c
c _________________________________________________________
c
c               Step 6; Set return fraction (Avwret) for 
c                       all downstream nodes
c
c rrb 2007/11/24; Roundoff concern; ignore if the return < .001
        if(ret.gt.smallX) then
          ISS=IRCD          
          DO  NS=1,NDNR
            AVWRET(ISS)=AVWRET(ISS)+RET
            ISS=IDNCOD(ISS)
          end do
c
c rrb 2008/01/24; Enhancement at the diversion (iscd) eff 100%            
            avwret(iscd)=1.0
        endif
  180 CONTINUE

c _________________________________________________________
c
c               Step 7; COMPUTE THE AMOUNT FOR THE CURRENT DIVERSION
c 
c    
      ifound=0      
      ISS=ISCD
      if(iout.eq.1) write(nlog,*) ' RtnMax; iscd, ndns', iscd, ndns
      
      DO 200 NS=1,NDNS
        dumx(iss)=1.0e6      
        avtemp1=avtemp(iss)      
        c0=(1.0-AVWRET(ISS)*foret)
c        
c ---------------------------------------------------------
c		Set dumx = avtemp/return fraction
        IF(c0.gt.smallX) then
          ifound=ifound+1       
          dumx(ISS)=AVTEMP(ISS)/(1.0-AVWRET(ISS)*foret)
        endif  
c        
c ---------------------------------------------------------
c		Detailed Output        
        if(iout.ge.1 .and. ioutL.eq.1) then
          if(ns.eq.1) write(nlog,310)
          write(nlog,'(15x, 5i8, 20f12.2)')
     1      ns, ifound, ndns, iscd, iss, 
     1      avtemp1*fac, avwret(iss), c0, dumx(iss)*fac,
     1      foret     
        endif          
  200 ISS=IDNCOD(ISS)
     
c
c ---------------------------------------------------------
c		Set diversion node (iscd) to available flow
c		Note this is critical if, for some reason,
c		returns get sent upstream
      dumx(ISCD)=AVTEMP(ISCD)
c
c _________________________________________________________
c
c               Step 8; FIND THE MIN flow in dumx
      if(ifound.gt.0) then
        if(iout.eq.1) write(nlog,*) ' RtnMax; iscd, ndns', iscd, ndns
        CALL DNMFSO(maxsta,dumx,IDNCOD,ISCD,NDNS,IMCD)
        PAVAIL=dumx(IMCD)
        pavail=amax1(0.0,dumx(imcd))
c        
c ---------------------------------------------------------
c		Detailed Output        
        if(iout.ge.1) then
          write(nlog,*) ' Rtnmax; ifound    imcd  pavail'
          write(nlog,'(8x, 2i8, 20f8.0)') ifound, imcd, pavail*fac
        endif  
      else
        pavail=avtemp(iscd)
c        
c ---------------------------------------------------------
c		Detailed Output        
        if(iout.ge.1) then
          write(nlog,*) ' Rtnmax; ifound iscd, pavail'
          write(nlog,'(8x, 2i8, 20f8.0)') ifound, iscd, pavail*fac
        endif  
      endif
      pavail1=pavail
c
c _________________________________________________________
c		Step 9; Check if the linear estimate for max return
c		        (pavail, which was calcuated above)
c		         is less than what will really occurr
c			       when limited by 4 land use types and
c			       4 IWR demands.
c		         Note call rtnsecX & dnmfso in order to
c			       not adjust any final values
c        
c ---------------------------------------------------------
c		Detailed Output        
      if(iout.ge.1) then
        write(nlog,*) ' RtnMax; pavail', imcd, pavail1*fac, pavail*fac
      endif  
c
c _________________________________________________________
c		Step X; Get min flow downstream
      call dnmFso(maxsta, AVAIL, idncod, iscd, ndns, imcd)  
      call dnmFso(maxsta, AVTemp, idncod, iscd, ndns, imcdx)
c     write(nlog,*) ' RtnMax; xxx', avail(imcd)*fac, avtemp(imcdx)*fac  
      
      avail1=avail(imcd)
      imcd1=imcd
      
      icx=100
      do is=1,maxsta
        avtemp(is)=avail(is)
      end do
        
      l2=-1      
      imcd1=imcd
      avtemp1=avtemp(imcd)
c
c _________________________________________________________
c		Step X; Add returns from pavail to avtemp
c
      if(iout.eq.1) write(nlog,*)  'RtnMax calling; iuse, iscd',
     1  iuse, iscd
c
c rrb 2011/05/19; Revise to include ndns (# of downstream nodes)
c                 so it can be used to search an exchange reach;
c                 instead of the entire downstream array
cx    CALL RtnSecX(icx,pavail,L2,iuse,iscd,iuse,ieff2,
cx   1     retTot, ret1, corid1)
      CALL RtnSecX(icx,pavail,L2,iuse,iscd,ndns,iuse,ieff2,
     1     retTot, ret1, corid1)

      imcd2=imcd
      avtemp2=avtemp(imcd)      
c
c _________________________________________________________
c		Step X; Remove pavail from avtemp        
      Call takou2(isub,maxsta, avtemp, idncod, pavail, ndns, iscd)
      imcd3=imcd
      avtemp3=avtemp(imcd)        
c
c _________________________________________________________
c		Step X; Get min downstream flow in avtemp
      call dnmFso(maxsta, avtemp, idncod, iscd, ndns, imcd)  
      imcd4=imcd
      
      avtemp4=avTemp(imcd)
      if(avtemp4.le. small) then
        pavail=pavail+avtemp4
      endif
      
c
c _________________________________________________________
c		Step X; Detailed Output              
      if(iout.eq.1 .or. iout.eq.3) then
        write(nlog,340) iter, ieff2, ieffmax, iscd, ndns, 
     1    imcd1,imcd2,imcd3,imcd4
      
        write(nlog,*) ' '
        write(nlog,342) iter,
     1    avail1*fac, avtemp1*fac, avtemp2*fac, avtemp3*fac, 
     1    avtemp4*fac, pavail1*fac, pavail*fac, avwret(imcd1),
     1    dumx(imcd1), retTot*fac, ret1*fac
      endif 
c _________________________________________________________
c
c               Step 10; Return
      return
c
c _________________________________________________________
c
c               Formats
 310  format(/,'  Rtnmax; loop ', 
     1      '      ns  ifound    ndns    iscd     iss', 
     1      '     avtemp1      avwret          c0        dumx',
     1      '       foret')
     
 320  format(/,60('_'),/
     1  '  RtnMax: Details for Diversion ', a12, ' iout = ', i5,
     1  1x, i5, 1x, a4, i5)
 330  format(/,
     1  '  RtnMax; FYI structure ID ', a12,' nd = ', i5, ' iuse = ',i5,/
     1  '          is a carrier because irturn(iuse) = ',i5,/
     1  '          Therefore average efficiency = ', f8.2, ' is used')

 340  format(/,
     1  '  RtnMax;      iter     ieff2   ieffmax      iscd      ndns',
     1  '     imcd1     imcd2     imcd3     imcd4',/
     1  '  _______ _________ _________ _________ _________ _________',
     1  ' _________ _________ _________ _________',/
     1  '  RtnMax;', 9i10, 20f10.3)       
 342  format(/,
     1  '  RtnMax;      iter',
     1  '    Avail1   Avtemp1   Avtemp2   AvTemp3   Avtemp4   Pavail1',
     1  '    Pavail    AvWret      dumx    retTot      ret1',/
     1  '  _______ _________',
     1  ' _________ _________ _________ _________ _________ _________',
     1  ' _________ _________ _________ _________ _________',/
     1  '  RtnMax;', i10, 20f10.3)       
     
 344  format(/,72('_'),//,        
     1  '  RtnMax; Warning IWR>0 for iteration 2.',/
     1  '          Called by: ', a12, ' for right ', a12,
     1  ' IWR (ac-ft) = ',f10.3)        
      
 346   format(/,        
     1  '  RtnMax;    iter     iri     ire ieffmax   ieff2   AreaT',
     1  '    effx   foret  effmax') 

 348  format(/,        
     1  '  RtnMax;    iter      nd    iuse   effC1   effF1   effS1',
     1  '   Foret  AreaSF  AreaSS  areaGF  AreaGS  diwrz1',
     1  '  diwrSF  diwrSS  diwrGF  diwrGS') 

      end
