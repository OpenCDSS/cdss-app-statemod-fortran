c
      Subroutine DsaMod(
     1   icx, ioutIN, l2, imcd, iscd, ndns, nd, iuse, ieff2, 
     1   fac, pavail,  divalo, divact, oprEffT, divactL, 
     1   iwhy, icase, ishort,  iresw, cCallBy, corid1, cwhy)
c
c _____________________________________________________________
c	Program Description
c	    It calcualtes the Modified Direct Solution Algorythm  
c	    Specifically it esitmates the diversion (divact) and
c	    minimum available flow (pavail) given the demand (divalo)	
c
c _____________________________________________________________
c	Update History
c
c      2009/01/24; Revise to exit before trying to divert more 
c	       icase=65 when called by divCarL (icx=45) since it is known
c	       carrier loss returns (if any) will be added in DivCarL.
c
c      2009/01/23; Revise to include divactL (diversion less loss)
c	       and to calculate returns based on divactL. Note
c	       the diversions are still based on divact. Also
c	       when divactL < divact return flows may be underestimated.
c
c	2008/04/02 Copied from DivRig.for
c
c _____________________________________________________________
c
c	Documentation
c	    icx	= calling routine (100 + opr rule or 200 + std rule)
c	    icxM	= called by DsaMod
c	    iout	= output control
c	    l2	= water right pointer
c	    imcd	= location of minimum flow (result)
c	    iscd	= diversion location
c	    ndns	= # of downstream nodes
c	    nd	= diversion pointer
c	    iuse	= diversion user (nd=iuse)
c     ieff2  = 0 always use average efficiency
c            = 1 let ieffmax control variable efficiency 
c
c     AreaSF = Area SW Flood (fraction)
c     AreaSF = Area SW Sprinkler (fraction)
c     AreaSF = Area GW Flood (fraction)
c     AreaSF = Area GW Sprinkler (fraction)  
c    	Area   = Total Area of ditch (acres)
c    
c    	diwrSF = IWR of Sprinkler Flood land (acft)
c    
c    	pavail	= minimum available flow (result)
c    	divalo	= demand limited by capacity, & decree
c    	divact	= diversion
c    	divactL= diversion less loss
c    	OprEffT= diversion efficiency (diversion less loss)
c    
c    	iwhy	= code of why a diversion = 0
c    	icase	= type of diversion
c    
c    	iresw   = 0 diversion
c	             1 reservoir
c     ishort  = shortage code for reoperation; 0=no, 1=yes
c	    cCallBy = calling routine
c	    corid1	= calling water right
c	    cwhy	= description of why a diversion = 0
c     icu     = 0 non consumptive
c                 1 consumptive
c
c_____________________________________________________________
c    	Dimensions
      include 'common.inc'
      character 
     1  cwhy*48,    rec12*12,   cCallBy*12, corid1*12, cCallBy2*12
c     
c_____________________________________________________________
c               Step 1; Initilize
c
c		iout=0 No details
c		    =1 details
c		    =2 summary
c		    = ioutIN, controlled by calling program
c
c		  iout6 = details of icase = 5
c
c		  ioutZ =0 NO details from Rtnsec
c		      >0 Details from Rtnsec if iout=1
c	  	Do not control detail from the calling program (ioutIN)
c     iout=ioutIN
c     ioutZ=0
      iout=0
      iout6=0
      ioutZ=ioutIN  
c
c rrb Turn the following on to see details when the check
c     option is on for a specified operating rule          
cx    if(ioutZ.gt.0) iout=2
      iout6=0
      
      icxM=-1
c     icxM=icx
      nout=0      
      cCallBy2='DsaMod      '
      icase=0
c
c rrb 2009/05/26; Correction
      if(iresw.eq.0) then
        iri=nrtn(iuse)
        IRE=NRTN(IUSE+1)-1
      else
        iri=-1
        ire=-1
      endif
      
      small=0.001
      smallN=-1.0*small
      Short=0.0
      divmore=-1.0/fac
      pavail=0.0
      
      divact=0.0
      divactL=0.0
      
      diwrz1=0.0
      diwrz2=0.0
      diwrz3=0.0
      
      iwhy=0
      cwhy='NA'
c_____________________________________________________________
c               Step 2; Find mininum downstream flow (pavail)
c          
cx      if(iout.eq.1) write(nlog,*) '  DsaMod; iscd, ndns,     ', 
cx     1  iscd, ndns
     
      CALL DNMFSO2(maxsta,avail,IDNCOD,ISCD,NDNS,IMCD,
     1  cCallBy2)

      Pavail=avail(IMCD)

      IF(iout.eq.1) then
        write(nlog,*) 
        write(nlog,*) 
        
     1   '___________________________________________________________'
        write(nlog,*) ' DsaMod; Step 2a '
        write(nlog,*) '           ','corid1       iscd ndns',
     1    '  pavail  qtribu  qstern     fac',
     1    '  divalo  divact divactL'
         
        write(nlog,'(12x,a12,2i5, 20f8.0)')
     1    corid1, iscd, ndns, pavail*fac, qtribu(iscd), qstern(nd),fac, 
     1    divalo*fac, divact*fac, divactL*fac
      endif
c
c
c rrb 2008/06/30; Include reservoirs (iresw=1)    
      if(iresw.eq.0) then
        IF(IRTURN(IUSE).EQ.4) Pavail=AMIN1(avail(IMCD),QSTERN(ND))
      else
        Pavail=avail(IMCD)
      endif  
      
      pavail=amax1(0.0,pavail)
c
c ---------------------------------------------------------
c
c rrb 2008/04/23; Quick Exit if available flow or diversion = 0 

c rrb 2009/10/20; revise to simulate if pavail = 0 but the
c                 diversion (iretsw=0) is non consumptive (icu=0)
      icu=1
c jhb 2014/07/13 put a check for the iuse (diversion user index)value
c                to prevent array bounds problem
c                ALSO change iresw to iretsw (typo?)
c      if(iresw.eq.0 .and. diveff(mon,iuse).lt.small) icu=0
      if (iuse.ge.1) then
        if(iretsw.eq.0 .and. diveff(mon,iuse).lt.small) icu=0
      else
        if(iretsw.eq.0) icu=0
      endif

cx    if(pavail.lt.small) then
      if(pavail.lt.small .and. icu.eq.1) then
        iwhy=1
        cwhy='DsaMod Available flow (pavail) is zero' 
        if(iuse.gt.0.and.iout.eq.2) write(nlog,*) ' DsaMod;', 
     1   iretsw, mon, iuse, diveff(mon,iuse), icu
        goto 220
      endif  
      
      if(divalo.lt.small) then
        iwhy=2
        cwhy='DsaMod Available diversion (divalo) is zero'
        goto 220
      endif  
c
c_____________________________________________________________
c               Step 3; Determine diversion 
c
c
c ---------------------------------------------------------
c               Step 3a 
c               Case 1 Destination is a reservoir (iresw=1)
c		     Diversion with No return flow adjustment possible 
c                 (irturn=4=transmountain) or no returns (iri<ire)
c
      if(iresw.eq.1) then
        icase=1
        
        DIVACT=AMIN1(Pavail,DIVALO)
        divact=amax1(0.0,divact)
c
c rrb 2009/01/23; Calculate carrier loss for return calculations        
        divactL=divact*OprEffT
c

        CALL TAKOUT(maxsta, avail ,RIVER ,AVINP ,QTRIBU,IDNCOD,
     1              DIVACT, NDNS,  ISCD     )
        GOTO 220
      endif
c
c ---------------------------------------------------------
c               Step 3a 
c               Case 1 Diversion with No return flow adjustment possible 
c                 (irturn=4=transmountain) or no returns (iri<ire)
c
      if(irturn(iuse).eq.4) then
        icase=1
        
        DIVACT=AMIN1(Pavail,DIVALO)
        divact=amax1(0.0,divact)
c
c rrb 2009/01/23; Calculate carrier loss for return calculations        
        divactL=divact*OprEffT        

        CALL TAKOUT(maxsta, avail ,RIVER ,AVINP ,QTRIBU,IDNCOD,
     1              DIVACT, NDNS,  ISCD     )
        QSTERN(ND)=QSTERN(ND)-DIVACT
c
c rrb 01/02/01; Call rtnsec who calls return to calculate CU,
c               reduce IWR and loss (as appropriate)
c rrb 2009/01/23; Calcualte returns for diversion less loss
cx      CALL RTNSEC(icxM,DIVACT,L2,IUSE,ISCD,nd,ieff2)
        CALL RTNSEC(icxM,DIVACTL,L2,IUSE,ISCD,nd,ieff2)
        
        IF(iout.eq.1) then
          write(nlog,*) ' DsaMod; icase  = ', icase
          write(nlog,*) ' DsaMod; divact = ', divact*fac
        endif
        GOTO 220
      endif
c
c ---------------------------------------------------------
c               Step 3b
c               Case 2 Can divert 100% w/o any return flow additions
c                 since (divalo) > available (pavail)
c
      if(pavail.ge.divalo) then 
        icase=2
        DIVACT=DIVALO
c
c rrb 2009/01/23; Calculate carrier loss for return calculations        
        divactL=divact*OprEffT

        IF(iout.eq.1) then
          write(nlog,*) ' DsaMod; icase  = ', icase
          write(nlog,*) ' DsaMod; divact = ', divact*fac
        endif

        CALL TAKOUT(maxsta, avail ,RIVER ,AVINP ,QTRIBU,IDNCOD,
     1              DIVACT, NDNS,  ISCD     )
c
c rrb 2009/01/23; Calcualte returns for diversion less loss
cx      CALL RTNSEC(icxM,DIVACT,L2,IUSE,ISCD,nd,ieff2)
       CALL RTNSEC(icxM,DIVACTL,L2,IUSE,ISCD,nd,ieff2)
        GOTO 220
      endif
c
c ---------------------------------------------------------
c               Step 4 Account for return flows
c
c
c rrb 2006/10/31; Check avail
      if(iout.eq.1) write(nlog,*) '  Dsamod; iscd, ndns ', iscd, ndns
      call rtnmax(1, iuse, iri, ire, iscd, ndns, small, pavail, 
     1  ieff2, ioutZ, cCallBy, corid1)
c
      if(pavail.lt.small) then
        iwhy=5
        cwhy='DsaMod Available downstream flow (pavail) = 0'
        goto 220
      endif  
c
c
c rrb 2009/01/23; Calculate carrier loss for return calculations        
      pavailL=pavail*oprEffT
cx    if(pavail.ge.divalo) then    
c
c ---------------------------------------------------------
c               Step 4a
c               Case 3 Divert 100% with return flows
      if(pavailL.ge.divalo) then    
        icase=3
        DIVACT=DIVALO
c
c rrb 2009/01/23; Calculate carrier loss for return calculations        
        divactL=divact*OprEffT
        
        CALL TAKOUT(maxsta, avail ,RIVER ,AVINP ,QTRIBU,IDNCOD,
     1              DIVACT, NDNS,  ISCD     )                     
c
c rrb 2009/01/23; Calcualte returns for diversion less loss
cx      CALL RTNSEC(icxM,DIVACT,L2,IUSE,ISCD,nd,ieff2)
        CALL RTNSEC(icxM,DIVACTL,L2,IUSE,ISCD,nd,ieff2)
        
        IF(iout.eq.1) then
          write(nlog,*) ' DsaMod; icase  = ', icase
          write(nlog,*) ' DsaMod; divact = ', divact*fac
        endif
        
        GOTO 220
      endif
c
c ---------------------------------------------------------
c               Step 4b
c               Case 4 Divert < 100% with return flow additions
c
c
c
c rrb 2009/01/23; Calculate carrier loss for return calculations        
      pavailL=pavail*oprEffT
cx    if(pavail.lt.divalo) then
      if(pavailL.lt.divalo) then
        icase=4
cx      divact=pavail
        divact=pavailL
        divact1=divact
c
c rrb 2009/01/23; Calculate carrier loss for return calculations        
        divactL=divact*OprEffT
        
c
c rrb 2006/10/31; Check avail for detailed output
        call dnmfso2(maxsta, avail ,idncod,iscd  ,ndns  ,imcd,
     1       cCallBy2)
        avail1=avail(imcd)
        imcd1=imcd

        CALL TAKOUT(maxsta, avail ,RIVER ,AVINP ,QTRIBU,IDNCOD,
     1              DIVACT, NDNS,  ISCD  )
c
c rrb 2009/01/23; Calcualte returns for diversion less loss
cx      CALL RTNSEC(icxM,DIVACT,L2,IUSE,ISCD,nd,ieff2)
        CALL RTNSEC(icxM,DIVACTL,L2,IUSE,ISCD,nd,ieff2)
c
c rrb 2006/10/31; Check avail for detailed output
        call dnmfso2(maxsta, avail ,idncod,iscd  ,ndns  ,imcd,
     1       cCallBy2)
c
c rrb 2008/12/17     
        pavail=avail(imcd)
        avail2=avail(imcd)
        imcd2=imcd
        
        Short=amax1(0.0, divalo-divact)
        
        IF(iout.eq.1) then
          write(nlog,*) ' DsaMod; icase  = ', icase
          write(nlog,*) ' DsaMod; divact = ', divact*fac
        endif
     
        if(iout.eq.3) then
          nout=nout+1
          if(nout.eq.1) write(nlog,260)
          write(nlog,262) 
     1      nout, ccallBy, corid1, 
     1      iyrmo(mon),xmonam(mon), idy,
     1      iwx, icase, l2, nd, iuse, iscd, imcd1, imcd2, 
     1      avail1*fac, avail2*fac, 
     1      Short*fac, divact*fac, divactL*fac
        endif
         
        if(Short.lt.small) then
          goto 220          
        endif
     
        if(pavail.lt.small) then
          goto 220          
        endif
      endif  
c
c ---------------------------------------------------------
c rrb 2009/01/24; 
c		 Step 3f; Exit for type 45 (DivCarL) since it is known:
c	         returns will be increased by carrier losses (if any)
c		  in DivCarL.
c
      if(icx.eq.45) then
        icase=5

        IF(iout.eq.1) then
          write(nlog,*) ' DsaMod; icase  = ', icase
          write(nlog,*) ' DsaMod; divact = ', divact*fac
        endif
        
        goto 220
      endif
      
c
c ---------------------------------------------------------
c
c rrb 2008/06/16; Update
c              Step 3f; Check if more can be diverted since
c		            constrained demand (Short) >small
c			          and avail(imcd) > small		          
c		            Should only occurr if rtnmax is more 
c			          than anticipated
      iterD=0
      iterMax=25
      facIter=0.8
      
 100  icase=6
      iterD=iterD+1
      
      dIWRZ1=     dIwrSf(iuse)*AreaSf(iuse)  
      diwrz1=diwrz1+dIwrss(iuse)*Areass(iuse)
      diwrz1=diwrz1+dIwrgf(iuse)*Areagf(iuse)
      diwrz1=diwrz1+dIwrgs(iuse)*Areags(iuse)
      
      IF(iout6.eq.1) then
         write(nlog,290)  
         write(nlog,292) 
     1    corid1, iyrmo(mon),xmonam(mon), 
     1    idy,iwx, iterD, imcd, ieffmax, 
     1    short*fac, diwrz1, pavail*fac, divmore*fac
      endif
c
c		Calculate IWR, note:
c		  diwrSF is in ac-ft
c		  areaSF is a fraction       
      availZ=avail(imcd)      
c
c ---------------------------------------------------------
c		Find amount available for diversion with no CU
c rrb 2009/11/01l Revise to allow some CU to occurr by 
c                 setting iter=2
cx    call rtnmax(2, iuse, iri, ire, iscd, ndns, 
      call rtnmax(1, iuse, iri, ire, iscd, ndns, 
     1  small, pavail, ieff2, ioutZ, cCallBy, corid1)
     
      dIWRZ2=     dIwrSf(iuse)*AreaSf(iuse)   
      diwrz2=diwrz2+dIwrss(iuse)*Areass(iuse) 
      diwrz2=diwrz2+dIwrgf(iuse)*Areagf(iuse) 
      diwrz2=diwrz2+dIwrgs(iuse)*Areags(iuse) 
      pavailL=pavail*oprEffT
      
      divmore=amin1(short, pavailL) 
      
      divmore=amax1(0.0, divmore) * facIter
      divmoreL=divmore*oprEffT
      
      if(divmore.le.small) goto 220
      

      IF(iout6.eq.1) then
        RETA=divmore - availZ        
         write(nlog,292) 
     1    corid1, iyrmo(mon),xmonam(mon), 
     1    idy,iwx, iterD, imcd, ieffmax, 
     1    short*fac, diwrz2, pavail*fac, divmore*fac
      endif
         

c     write(nlog,*) ' DsaMod iteration 2; pavail', pavail*fac 
c
c rrb 2008/12/17; Update
cx    if(pavail.gt.small) then      
      if(divmore.gt.small) then      
        availA=avail(222)     
        CALL TAKOUT(maxsta,avail,RIVER,AVINP,QTRIBU,IDNCOD,
     1              divmore,NDNS, ISCD )
     
     
cx        IF(iout6.eq.1) then
cx          write(nlog,*) ' DsaMod 6f-X; ',
cx     1    '  icase    diwrsf    diwrss    diwrgf    diwrgs     diwrz1'
cx          write(nlog,'(14x,i8, 20f10.3)')
cx     1      icase, diwrsf(iuse), diwrss(iuse), diwrgf(iuse), 
cx     1      diwrgs(iuse), diwrz1
cx        endif
        
        availB=avail(220)
        
c
c rrb 2009/01/23; Calcualte returns for diversion less loss
cx      CALL RTNSEC(icxM,divmore,L2,IUSE,ISCD,nd,ieff2)
        CALL RTNSEC(icxM,divmoreL,L2,IUSE,ISCD,nd,ieff2)
          
        dIWRZ3=     dIwrSf(iuse)*AreaSf(iuse)   
        diwrz3=diwrz3+dIwrss(iuse)*Areass(iuse) 
        diwrz3=diwrz3+dIwrgf(iuse)*Areagf(iuse) 
        diwrz3=diwrz3+dIwrgs(iuse)*Areags(iuse) 
        
        IF(iout6.eq.1) then
         write(nlog,292) 
     1    corid1, iyrmo(mon),xmonam(mon), 
     1    idy,iwx, iterD, imcd, ieffmax, 
     1    short*fac, diwrz3, pavail*fac, divmore*fac
        endif
          
          
        availC=avail(220)
        retB=availC-availB
                      
cx        if(iout6.eq.1) then
cx          write(nlog,*) ' DsaMod 6f-5;',
cx     1   '   icase    availA    AvailB    AvailC      RetB'
cx        
cx          write(nlog,'(14x, i8, 20f10.3)')
cx     1    icase, availA*fac, availB*fac, availC*fac, retB*fac
cx        endif
        
        divactZ=divact
        divact=divact+divmore
c
c rrb 2009/01/23; Calculate carrier loss for return calculations        
        divactL=divact*OprEffT
        
        
cx        IF(iout6.eq.1) then
cx          write(nlog,*) ' DsaMod 6f-6; ',
cx     1      '  icase   divactZ   divmore    divact'
cx          write(nlog,'(14x, i8, 20f10.2)')
cx     1      icase, divactZ*fac, divmore*fac, divact*fac
cx        endif
c
c _________________________________________________________
c		
c rrb 2008/06/24; Step X; Iterate if necessary
        CALL DNMFSO(maxsta, avail, IDNCOD, ISCD, NDNS, IMCD)
        Short=amax1(0.0, divalo-divact)
        if(Short.gt.small .and. avail(imcd).gt.small .and.
     1     iterD.lt.iterMax) goto 100
        
c _________________________________________________________
c		Step X; Final checks
c rrb 2008/06/24; Check if demand exists yet avail > 0
        if(Short.gt.small .and. avail(imcd).gt.small) then
          write(nlog,*) ' '
          write(nlog,*) '  DsaMod; Warning_2 ',
     1       'Short   = ', Short*fac, 
     1       'Avail   = ', avail(imcd)*fac,
     1       'Iscd    = ', iscd,
     1       'ndns    = ', ndns,
     1       'Imcd    = ', imcd,
     1       'Diwrz1  = ', diwrZ1,
     1       'Diwrz2  = ', diwrZ2,
     1       'Diwrz3  = ', diwrZ3
cx          stop
        endif
c
c ---------------------------------------------------------
c rrb 2007/10/05; TEMPORARY CORRECTION find negative (avail(imcd)
c		  and add into system 
        CALL DNMFSO(maxsta, avail, IDNCOD, ISCD, NDNS, IMCD)
        
        IF(avail(IMCD).le.smallN) then
c
c rrb 2008/12/19; Reset Avail if the negative value is small)
cx        if(avail(imcd)+small .ge. smallN) then
          if(abs(avail(imcd)) .le. small*10.) then
            Adjust=avail(imcd)
            CALL TAKOUT(maxsta,avail,RIVER,AVINP,QTRIBU,IDNCOD,
     1                Adjust,NDNS, ISCD )
     
            write(nlog,300) corid1, adjust, adjust*fac
            goto 220
          endif
                  
        
          write(nlog,*) ' '
          write(nlog,*) ' DsaMod; Warning negative flow = ', 
     1      Avail(imcd), 'cfs. Following are details'
     
          write(nlog,*) ' DsaMod; Warning ',
     1     ' Iscd Imcd   l2 icase',
     1     '   availZ     DivactZ     Short    Pavail',
     1     '   Divmore    Divact',
     1     '    avail  avail-cfs'
     
          write(nlog,'(18x, 4i5, 7f10.2, f10.4)') 
     1      Iscd,        Imcd,        l2, icase, 
     1      availZ*fac,  divactZ*fac, short*fac, 
     1      pavail*fac,  divMore*fac, divact*fac, avail(imcd)*fac,
     1      avail(imcd)
        
c
c rrb 2008/12/19; Reset Avail after the warning data
          Adjust=avail(imcd)
          CALL TAKOUT(maxsta,avail,RIVER,AVINP,QTRIBU,IDNCOD,
     1              Adjust,NDNS, ISCD )
        
c
c rrb 2007/11/24; Addition
c
c rrb 23008/06/10; Correction diwrSF is in ft    

          Area1=area(iuse)
          dIWRZ3=     dIwrSf(iuse)*AreaSf(iuse)   
          diwrz3=diwrz3+dIwrss(iuse)*Areass(iuse) 
          diwrz3=diwrz3+dIwrgf(iuse)*Areagf(iuse) 
          diwrz3=diwrz3+dIwrgs(iuse)*Areags(iuse) 
          
          write(nlog,*) ' DsaMod;  diwrZ1= ',diwrz1
          write(nlog,*) ' DsaMod;  diwrZ2= ',diwrz2
          write(nlog,*) ' DsaMod;  diwrZ3= ',diwrz3
          write(nlog,*) ' DsaMod;  Change = ',diwrz3-diwrZ1
          
          write(nlog,*) ' DsaMod;  divmore = ',divmore*fac
          write(nlog,*) ' DsaMod;  pavail = ', pavail*fac
          write(nlog,*) ' DsaMod;  divact = ', divact*fac
c
c rrb 2009/06/17; Allow to continue if < 10 cfs
          if(abs(adjust).lt.10.0) then
            write(nlog,*) ' DsaMod; Proceeding on after correcting ',
     1         ' for the negative flow.' 
            goto 220        
          endif  
          goto 9999
c
c		Endif for negative avail                
        ENDIF
c             
c		Endif for pavail.gt.small              
      endif  

c_____________________________________________________________
c
c		Step 4; Set ishort
      IF(IRTURN(IUSE).LE.3 .and. (divact+small).lt.divalo) ISHORT=1
c
c_____________________________________________________________
c               Step 5; Update available flow at diversion station
c                 for Cases 2, 3A & 3b 
cf
c
c_____________________________________________________________
c               Step 6; Double Check available flow
c
  220 continue
c
c rrb 2009/10/25; Revise to reset the iterate due to return 
c                 flows (ireop=0)if no diversion (divact<0)
      if(divact.lt.small) ireop=0
        
      CALL DNMFSO2(maxsta, avail, IDNCOD, ISCD, NDNS, IMCD,
     1  cCallBy2)

      IF(iout.eq.1) then
        write(nlog,*) ' DsaMod; Step 8'
        write(nlog,*) ' DsaMod; icase  = ', icase
        write(nlog,*) ' DsaMod; avail  = ', avail(imcd)*fac
        
      endif

c             
c               Print warning if negative available flow
      IF(avail(IMCD).le.(-1.*small)) then
        write(nlog,390) cCallBy, corid1, 
     1    iyrmo(mon),xmonam(mon), icx, 
     1    L2,IUSE, ISCD,IMCD,ieff2, icase, 
     1    Divalo*fac, divact1*fac, divmore*fac, DIVACT*fac, 
     1    divactL*fac, avail(imcd), avail(imcd)*fac
     
cxx        goto 9999
      endif
c
c_____________________________________________________________
c               Step 7; Detailed Output
      
      if(iout.ge.2) then
        if(iretsw.eq.1) then
          divcap1=divcap(nd)*fac
          divmon1=divmon(nd)*fac
        else
          divcap1=-1.
          divmon1=-1.
        endif
        
        write(nlog,270) corid1,cCallBy
        write(nlog,280) '  DsaMod    ',
     1    iyrmo(mon),xmonam(mon), idy, iwx, corid1,
     1    icase, l2, ND, IUSE, iscd, imcd, iresw, 
     1    Divalo*fac,    avail1*fac, avail2*fac, CURRTN(ISCD)*fac, 
     1    DIVCAP1,   DIVMON1,  divd(l2)*fac,    
     1    pavail*fac,     Short*fac, divact*fac,  divactL*fac,
     1    iwhy, cwhy
      endif
        
c
c_____________________________________________________________
c               Step 8; Return
      
      return 
c
c_____________________________________________________________
c               Formats
c
  260   format(/, '  DsaMod Check',/ 
     1  ' nout cCallBy      Source ID     iyr  mon  Day',
     1  '  Iter icase    l2    nd  iuse  iscd imcd1 imcd2',
     1  '  avail1  avail2   Short  divact divactL',/
     1  ' ____ ____________ ____________ ____ ____ ____',
     1  ' _____ _____ _____ _____ _____ _____ _____ _____',
     1  ' _______ _______ _______ _______ _______')
     
  262   FORMAT(i5, 1x,a12, 1x,a12, i5,1x,a4,i5, 
     1         8i6,20F8.0)
     
  270   format(/, 
     1  '  DsaMod; Right ID = ', a12, ' Called by ', a12,/
     1  '  DsaMod      iyr  mon  Day Iter Source ID    ',
     1  '   icase      l2      nd    iuse    iscd    imcd   iresw',
     1  '  Divalo  avail1  avail2  currtn  divcap  divmon    divd',
     1  '  pavail   Short  divact divactL iwhy cwhy',/
     1  ' ___________ ____ ____ ____ ____ ____________ ', 
     1  ' _______ _______ _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ ____ ____________')
     
  280   FORMAT(a12, i5,1x,a4,2i5,1x, a12,1x,7i8, 11F8.0, i5, 1x, a48)
  
  290   format(/,                                    
     1   ' DsaMod 6f-1 ID      ',                   
     1   '  iyr  mon  idy iwx iterD imcd',               
     1   ' ieffmax   short  diwrz1  pavail divmore')
      
  292   format(10x,a12,i5,1x,a4,i4, 3i5,i8,20f8.0)       
  300   format(/,            ' '
     1  ' DsaMod; FYI Adjusting avail for roundoff.',
     1  ' Water Right = ', a12, ' Adjustment = ', f10.4,
     1  ' cfs or ', f10.4, ' acft')

  390 FORMAT(/, 72('_'),/
     1  '  DsaMod; Warning negative avail',/
     1  '          Called by = ', a12, ' Right = ', a12, /,
     1  '  IYR  MON  icx   L2 IUSE iscd imcd   ieff2   icase',
     1  '    Divalo   DivAct1   DivMore    DivAct   DivActL',
     1  '  avail cfs  avail af',/
     1  ' ____ ____ ____ ____ ____ ____ ____ _______ _______',
     1  ' _________ _________ _________ _________ _________',
     1  ' _________ _________',/,
     1  i5,1x,a4, 5i5, 2i8, 20F10.2)

c
c_____________________________________________________________
c               Error warnings
c
 9999 continue
      write(nlog,*) ' DsaMod; Error Reporting'
      write(nlog,270) corid1,cCallBy
     
      cwhy='DsaMod Warning'
      write(nlog,280) 'DsaMod      ',
     1    iyrmo(mon),xmonam(mon), idy, iwx, corid1,
     1    icase, l2, ND, IUSE, iscd, imcd, iresw, 
     1    Divalo*fac,    avail1*fac, avail2*fac, CURRTN(ISCD)*fac, 
     1    DIVCAP(ND)*fac, DIVMON1*fac,  divd(l2)*fac,    
     1    pavail*fac,     Short*fac, divact*fac, divactL*fac, 
     1    iwhy, cwhy

      write(6,1050) 
      write(nlog,1051) 
      
 1050 format('    Stopped in DsaMod',/,
     1       '    See the *.log file')
 1051 format('    Stopped in DsaMod')
      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)


      stop 
      
      end
