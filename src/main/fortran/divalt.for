c
c _________________________________________________________
c
c     Last change:  RRB  18 Dec 100    2:29 pm
c
      SUBROUTINE DivAlt(IW,L2,ISHORT,divactx,ncallx)
c
c _________________________________________________________
c	Program Description
c
c	DivAlt; Type 39
c		It calculates a diversion at an alternate point
c		Destination is a diversion or well
c		Source 1 is a water right
c		Source 2 is the Alternate Point location (well or diversion)
c		Note:	if(iopsou(4,l2)=0 the diversion is not limited by flow 
c                     at the decreed location 
c                  =1 the diversion is limited by flow 
c                     at the decreed location 
c
c _________________________________________________________
c	Update History
c
c rrb 2010/11/15; More corrections and refinements
c rrb 2008/09/19; Miscellaneous corrections, primarly related
c		              to alternate points = wells
c rrb 2006/06/23; Copy DivRigS (standard right with seasonal
c                 limitations.
c
c_____________________________________________________________
c
c       Documentation
c
c      icx             subroutine call # (5)
c      IW              Global water right ID
c      L2              Operational right pointer
c                      
c	     ndr             Source water right pointer
c	     nd1             Source diversion pointer
c                      
c      nd              Destination structure pointer
c      iuse            Diversion user
c                      
c	     nr              Reservoir pointer (0)
                       
c      ishort          code for reoperation; 0=no, 1=yes
c                      
c      divactx         actual diversion
c                      
c      divreq          Diversion demand for types 1-3 (standard)
c                      
c      divsw           SW demand for demand types 4 & 5 
c                      
c      divreqx         Set to divreq or divsw based on demand type
c                      
c      dcrdiv          Decree (cfs)
c      divd            Decree diverted in previous iterations
c      divcap          Structure capacity
c      divmon          Amount diverted in previous iterations
c                      
c      idvsta(l2)      STATION WHERE DIV. RIGHT L2 LOCATES
c                      
c      ieff2           = 0 always use average efficiency
c                      = 1 let ieffmax control variable efficiency 
c                      
c      imd             Number of days this month from Execut.for
c      ioprtn          
c      iout            Switch: 0 no print; 1 yes print
c      idvsta(nd)      Diversion station
c      iscd            Decreed Diversion station (iscd = idvsta(nd))
c                      
c      ndnnod(iscd)    Number of downstream nodes
c      ndns            Number of downstream nodes from decree
c                      (ndns=ndnnod(iscd))
c      ndnd            Number of downstream nodes from the destination
c                      (ndnd=ndnnod(idcd))
c      ndnr            Number of downstream nodes from return
c                      (ndnr = f (ndnnod)) 
c
c
c      qdiv(5         Total diversion from river by priority 
c                     (e.g. divcar)
c      qdiv(8         Total diversion from transmtn (e.g. divcar)
c      qdiv(18        Carrier passing thru a structure (e.g. divcar)
c      qdiv(19        From Carrier by Priority (e.g. divcar)
c      qdiv(20        From Carrier by Storage or Exchange or Alt Point
c      qdiv(24        Pumping (Well) at river ID 
c      qdiv(26        From River by a Direct Flow, Exchange, or 
c                        Bypass (see DirectEx and DirectBY)
c                        or Carrier with loss (DirectL) or an alternate 
c                        point (DivAlt)c
c      currtn         Immediate return to diverting node??
c      small          a small value for roundoff (0.0) concerns
c
c      DivownP(n)     Ownership fraction
c	     DivnamO(n)     Owner name
c	     NdOwn(n)       Pointer to ID of owner
c	     DivOwn(n)      Diversion by owner n
c      
c	     imcd1          minimum flow location
c
c      ilimit         iopsou(2,l2) 0 do not limit based on flow at 
c                       the decreed locatoin, 1 do limit
c
c _____________________________________________________________
c	Dimensions
c
      include 'common.inc'
      character ctype1*12, corid1*12
      character cwhy*45, cdestyp*12, ccarry*3, cpuse*3, cidvri*12
c
c_____________________________________________________________
c               Step 1; Common Initilization
c		iout = 0 no details
c		       1 details
c          2 summary      
      iout=0
      ioutiw=0
      isub=39
      
      corid1=corid(l2)
      
      if(ichk.eq.139) iout=2
      if(corid(l2).eq. ccall) ioutiw=iw
      
c     if(iout.ge.1 .and. ncallx.eq.0) then
      if(iout.eq.1) then
        write(nlog,102) corid(l2), iout, ioutiw, iw
 102    format(/, 72('_'),/ 
     1  '  DivAlt; ID = ', a12, 5i5)
      endif         

      divact = 0.0
      divactx= 0.0
      divalo = 0.0
c     availd = availd
      ISHORT=0
      imcd1=0
      icall=39     
c
c 		Use maximum efficiency if ieffmax=1     
      ieff2=1
      
      icx=39
      fac = mthday(mon) * factor
      small = 0.001
c
c rrb 2006/01/18            
      iwhy=0
      cwhy='NA'
      
      nAPtype=0
      cpuse='No '
      ccarry='No'
      cdestyp='Diversion'      
      if (intern(l2,1).ne.0) ccarry='Yes'
      iuse=0
      nr=0
      nd2=0
      
      pavail=-1/fac
      divreqX=-1.0/fac
      RitRem1=-1.0/fac
      DivCap1=-1.0/fac
      Avail1=-1.0/fac
      Caprem1=-1.0/fac
      CapremAP=-1.0/fac
      imcd=-1
      
c
c_____________________________________________________________
c               Step 2; Set source water right (ndr) associated
c                       with a diversion
c
      Ndr  =Iopsou(1,L2)
      ND1=IDIVCO(1,NDR)
      iscd=idvsta(nd1)
      NDNS=NDNNOD(ISCD)
      ilimit=iopsou(4,L2)
      iup=0
c
c_____________________________________________________________
c               Step 3; Set Alternate Point Location Data
c                (iscd), etc.
      nAP = iopsou(3,l2)
      if(nAP.gt.0) then
        isAP= idvsta(nAP)
        ndAP= NDNNOD(isAP)
        nAPtype=3
        ctype1='Diversion'        
      endif  
      
      if(nAP.lt.0) then      
        nAP=iabs(nAP)
        isAP= idvstaw(nAP)
        ndAP= NDNNOD(isAP)
        nAPtype=6
        ctype1='Well     '
      endif  
      
      if(nAPtype.eq.0) then
        write(nlog,310) nAPtype
        goto 9999
      endif 
      
c
c _________________________________________________________
c		Step 4; On/Off Switch      
c
c rrb 06/01/18; Allow daily on/off switch
      if(imonsw(l2,mon).eq.0) then
        iwhy=1
        cwhy='Monthly switch Off'
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
c_____________________________________________________________
c               Step 5a; Set destination data 
c
c ------------------------------------------------------------
c               a) Destination is a diversion
c rrb 2010/11/15; Correction
      idest=iopdesr(l2)
      if(idest.eq.3) then
        cdestyp='Diversion   '
        nd=iopdes(1,l2)
        nw=0
        nd2=0
       
        idcd=idvsta(nd)
        Ndnd=NDNNOD(idcd)   
        IUSE=NDUSER(ND)+IDIVCO(2,nd)-1 
        
        if(idivsw(nd).eq.0) then
          iwhy=2
          cwhy='Destination Diversion is off'
          goto 260
        endif         
      endif
c
c ------------------------------------------------------------
c               b) Destination is a well
c rrb 2010/11/15; Destination cannot be a well (checked in oprinp)
c      
cx      if(idest.eq.6) then
cx        cdestyp='Well        '      
cx        nd=0
cx        nw=iopdes(1,l2)
cx        nd2=idivcow2(nw)  
cx             
cx        idcd=idvstaw(nw)
cx        Ndnd=NDNNOD(idcd)
cx        
cx        if(idivsww(nw).eq.0) then
cx          iwhy=2
cx          cwhy='Destination Well is off'
cx          goto 260
cx        endif         
cxc      
cx        IUSE=nw 
cx      endif
c      
c
c ---------------------------------------------------------
c               c. Detailed Output
c
c
      if(iout.eq.1) then      
        write(nlog,100)  iyrmo(mon), xmonam(mon), cdestyp, iopout,
     1                   iscd, idest, nd, nw, nAPtype, iuse, ilimit
     
 100    format(
     1    /, 80('_'), /,
     1    '  DivAlt_1;     iyr     mon Cdestyp       iopout',
     1    '    iscd idest      nd      nw nAPtype    iuse  ilimit',/,
     1   11x, i8, 4x, a4, 1x,a12, 20i8)
      endif
c
c_____________________________________________________________
c               Step 6; Set demand (divreqX)
c     write(nlog,*) ' DivAlt; #1a'

      call SetDem(isub, nd, nw, nd2, iuse, fac, 
     1    effa, effS1, effF1, AreaT, divreqX, divSprX, divOthX,
     1    diwrGS1, diwrGF1, ncallX)      
c
c ------------------------------------------------------------
c               c) Exit if the destination demand is zero    
        
      if(divreqx.lt.small) then
        iwhy=3
        cwhy='Destination Diversion Demand (Demand) is zero'
        goto 260
      endif  
c     write(nlog,*) ' DivAlt; #1b'        
c
c_____________________________________________________________
c               Step 7; Check and Set capacity of destination
c rrb 2008/09/19; Correction
      if(nd.gt.0) CapRem1= divcap(nd)-divmon(nd)    
c
c rrb 2010/11/15; Destination cannot be a well
cx    if(nw.gt.0) CapRem1= divcapW(nw)-divmonW(nw)
      if(CapRem1.le.small) then
        iwhy=4
        cwhy='Destination Capacity (CapRem1) is zero'
        write(nlog,*) ' DivAlt; ',nAPtype, Nap, Caprem1*fac
        goto 260
      endif          
      
c_____________________________________________________________
c               Step 8; Set Source Decree
c
c     write(nlog,*) ' DivAlt; #3a ndr ', ndr

      RitRem1=dcrdiv(ndr)-divd(ndr)
      if(RitRem1.le.small) then
        iwhy=5
        cwhy='Remaining source decree (RitRem1) is zero'
        goto 260
      endif  
c
c_____________________________________________________________
c               Step 9; Initilize avtemp
c
       DO IS=1,NUMSTA               
         AVTEMP(IS)=AVAIL(IS)       
       end do                       
c
c_____________________________________________________________
c               Step 10; Limit flow at the source (water right)
c                        location if ilimit > 0
c

      if(ilimit.eq.0) then
        pavail=9999./fac
        avail1=9999./fac
      else
c
c ---------------------------------------------------------
c               a) Set flow at source water right headgate
c       write(nlog,*) ' DivAlt; #4a iscd, iuse ', iscd, iuse
        Avail1=avtemp(iscd)
c
c ---------------------------------------------------------
c               b)Find mininum downstream flow from 
c		              the decreed location      
        CALL DNMFSO(maxsta,AVTEMP,IDNCOD,iscd,ndns,IMCD)
        PAVAIL=AVTEMP(IMCD)
        pavail=amax1(0.0,pavail)     
cx      write(nlog,*) '  DivAlt; Source', nd, iscd, pavail*fac       
c
c ---------------------------------------------------------
c rrb 2011/01/11; Correction 
c               c) Determine if the source water right is 
c                  upstream of the alternate point 
        iup=0
        iss=iscd
        do is=1,ndns-1
          if(iss.eq.isAP) iup=1
          iss=idncod(iss)
        end do  
      endif
c
c ---------------------------------------------------------
c              c) Exit if available flow is zero     
      if(Avail1.le.small) then
        IF(IRTURN(IUSE).LE.3) ISHORT=1
        iwhy=6
        cwhy='Avail flow at the source right (Avail1) =0'
        goto 260
      endif 
      
      if(pavail.le.small) then
        iwhy=7
        cwhy='Avail flow downstream of the Source (Pavail) =0'
        goto 260
      endif
             
c
c_____________________________________________________________
c               Step 11; Check flow at the Alternate Point
c
c ------------------------------------------------------------
c               a) Check flow at the Diversion Alterante Point (isAP)
      if(nAPtype.eq.3) then
        Pavail2=avtemp(isAP)
cx      write(nlog,*) '  DivAlt; Alt Point', nd, isAP, pavail2*fac
      
        if(Pavail2.le.small) then
          IF(IRTURN(IUSE).LE.3) ISHORT=1
          iwhy=8
          cwhy='Available flow at Alt Point (Pavail2) is zero'
          goto 260
        endif
c
c ---------------------------------------------------------
c               b) Check flow downstream of the diversion Alternate Pt

        CALL DNMFSO(maxsta,AVTEMP,IDNCOD,isAP,ndAP,imcdAP)

        
        pavail2=AVTEMP(imcdAP)
        Pavail2=amax1(0.0,pavail2)      
        
cx      write(nlog,*) ' DivAlt; #6a imcdD ', imcdAP, pavail2*fac         
        if(pavail2.le.small) then
          iwhy=9
          cwhy='Avail downstream of the Alt Point (Pavail2) = zero'
          goto 260
        endif
c
c ---------------------------------------------------------
c rrb 2010/12/26; Enhancement
c               c) Check capacity at diversion Alternate Pt
        CapRemAP= divcap(nAp)-divmon(nAp)    
        if(CapRemAP.le.small) then
          iwhy=4
          cwhy='Alternate Point Capacity (CapRemAP) is zero'
cx        write(nlog,*) ' DivAlt; ',nAPtype, Nap, CapremAP*fac
          goto 260
        endif              
      endif
c
c ---------------------------------------------------------
c               d) Set flow for a Well Alternate Point
c
      if(nAPtype.eq.6) then
        pavail2=9999./fac
c
c rrb 2010/12/26; Enhancement
        CapRemAP= divcapw(nAp)-divmonw(nAp)    
        if(CapRemAP.le.small) then
          iwhy=4
          cwhy='Alternate Point Capacity (CapRemAP) is zero'
cx        write(nlog,*) ' DivAlt; ',nAPtype, Nap, CapremAP*fac
          goto 260
        endif                
      endif
c
c_____________________________________________________________
c               Step 12; Calculate allowable diversion (divalo)
c			 RitRem = source right remaining
c			 DivreqX = Demand
c			 CapRem1 = remaining capacity at diversion
c      CapRemAP= remaining capacity at alternate point
c			 Avail1  = Available flow at source right
c			 Pavail  = Available flow downstream 
c	 			         of source right
c			 Pavail2 = Available flow downstream 
c				         of Alernate Point
c
      divalo=amin1(RitRem1, divreqx, CapRem1, CapRemAP,
     1             Avail1, Pavail, Pavail2)
      
      divact=amax1(0.0,divalo)
      IF((divact+small).lt.divreqX) ISHORT=1
c
c_____________________________________________________________
c               Step 15; Alternate Point is a diversion
c                 Adjust river from the alternate point (isAP)
c                 and calculate returns from the destination (nd)
      if(nAPtype.eq.3) then      
        CALL TAKOUT(maxsta, AVAIL ,RIVER ,AVINP ,QTRIBU,IDNCOD,
     1              DIVACT, ndAP,  isAP  )
c
c                 Calculate returns based on the diversion (nd)
c                 located at the alternate point (isAP)
        CALL RTNSEC(icx,DIVACT,L2,IUSE,isAP,nd,ieff2)
      endif
c
c_____________________________________________________________
c               Step 16; Alternate Point is a Well
c                 Adjust river for depletions from the alternate 
c                 point (isAP) and calculate returns from the 
c                 destination (nd) 
      nd2=0
      if(nAptype.eq.6) then            
        call deplete(divact, depx, l2, isAP)
c
c                 Calculate returns based on the diversion (nd)
c                 located at the alternate point (isAP)
        CALL RTNSEC(icx,DIVACT,L2,IUSE,isAP,nd,ieff2)
      endif  
c
c _________________________________________________________
c
c               Step 19; If the diversion is limited by the flow
c                        at the destination (ilimit=1) and the 
c                        destination is upstream of the alternate
c                        point (iup-=1), adjust available flow
c                        at the destination
      if(ilimit.eq.1 .and. iup.eq.1) then
        avail(idcd)=avail(idcd)-divact
      endif
c_____________________________________________________________
c               Step 18; Find any negative values in Avail
c			 May occur when the Alt Point is a well. Note:
c			 istop =  0 DO NOT STOP if a negative is found
c		                  1 DO STOP if a negative is found
      istop=0      
      call chekav2(
     1  icall, maxsta, numsta, istop, fac, AVAIL, IMCD, AvMin)
        AvailC2=avail(imcd)   
      
c
c _________________________________________________________
c
c               Step 19; If the destination is a Well (nw>0) and
c		              avail <0, diverting out of priority
c                 IF avail is <0, then
c                   a Find a negative avail
c                   b Calculate gw2riv to make avail zero
c                   c Route gw2riv downstream
c                   d Calculate T&C Obligation
c		
      if(nAPtype.eq.6 .and. availC2.lt.(-1.*small)) then
        call setGW (isub, small, fac, corid1)
      endif
c
c_____________________________________________________________
c               Step 20; UPDATE 
c
c ---------------------------------------------------------
c		           a) Update the Destination diversion
c                 qdiv(20 From Carrier by Storage, Exchange or Alt Point   
c
      if(nd.gt.0) then
        if(idemtyp.le.3) then
          DIVREQ(IUSE)=DIVREQ(IUSE)-DIVACT
        else
          divreq(iuse)=amax1(0.0, divreq(iuse)-divact)
          divsw(iuse)=divsw(iuse)-divact 
c
c rrb 01/02/25; Demand options 4 & 5               
          nw2=idivco2(nd)
          if(nw2.gt.0) then
            if(ieffmax.le.0) then
              effd=diveff(mon,nd)/100.
              effw=diveffw(mon,nw2)/100.
            else
              effd=effmax(nd)/100.
              effw=effmaxw(nw2)/100.
            endif
     
            dcux=(divact*effd)/effw
          endif
c    
          USEMON(IUSE)=USEMON(IUSE)+DIVACT 
        endif
        
        DivMon(nd) = DivMon(nd) + Divact 
        qdiv(20,idcd)=qdiv(20,idcd)+divact
      endif
c
c ---------------------------------------------------------
c		           b) Update the destination Well 
c rrb 2010/11/15; Destination cannot be a well 
cx    if(nw.gt.0) then
cx      DivMonW(nw) = DivMonW(nw) + Divact
cx      divreqw(nw) = divreqw(nw) - divact    
cx    endif
c
c ---------------------------------------------------------
c		           c) Update the alternate point
      if(nAPtype.eq.3) then
        divmon(nAp)= DivMon(nAP) + divact
      else
        divmonw(nAP)= DivMonW(nAP) + divact 
      endif
c rc
c
c ---------------------------------------------------------
c		           d) Update the source water right & this
c		              operating rules water right
      divd(ndr) = divd(ndr)+divact
c
c
c ---------------------------------------------------------
c		           e) Adjust Qdiv variables at a diversion Alternate Point
c                 qddiv(18 Carrier passing thru a structure (e.g. divcar)
c                 qdiv(26  From River by an Alternate Point, plus
      if(nAPtype.eq.3) then
        qdiv(18,isAP)=qdiv(18,isAP)+divact        
        qdiv(26,isAP)=qdiv(26,isAP)+divact
      endif  
c
c ---------------------------------------------------------
c		           f) Adjust Qdiv variables at the Alternate Point
c                 when the alternate point is a well (Naptype=6)
c                 qdiv(18 Carrier passing thru a structure (e.g. divcar)
c                 qdiv(24  Pumping (Well) at river ID 
      if(nAPtype.eq.6) then
        qdiv(18,isAP)=qdiv(18,isAP)+divact
        qdiv(24,isAP)=qdiv(24,isAP)+divact
        divmonW(nw)=divmonW(nw)+divact
        carryW(nw)=carryW(nw)+divact
      endif
c 
c ---------------------------------------------------------
c		           g) Update diversion by this Operating Rule
      DIVO(L2)=DIVO(L2)+DIVACT
c    
c ---------------------------------------------------------
c              h) Set return switch (iretsw) & the
c		  shortage (ishort) switch 
c
  260 continue
      if(divact.gt.small) iretsw=1
      if((divact+small).lt.divreqX) ishort = 1
c
c_____________________________________________________________
c               Step X; Print detailed results if requested
c       
 380  divactx=divact     
      if(iout.ge.1 .and. iw.eq.ioutiw) then
        ncallX=ncallX+1
        if(ncallX.eq.1)then
          write(nlog,270) corid(l2),ctype1,cdestyp,ccarry,cpuse
        else
c          write(nlog,*) ' '
        endif  
        
        write(nlog,280) '  DivAlt      ', cstaid(iscd),
     1    iyrmo(mon),xmonam(mon),
     1    idy, iw, iwx, nAp, nd, nw, nd2, imcd, imcd1, 
     1    idcd, isAP, iup, ilimit,
     1    DIVREQx*fac, CapRem1*fac, CapRemAP*fac, Ritrem1*fac, 
     1    AVAIL1*fac,  pavail*fac,  pavail2*fac,  divact*fac,
     1    iwhy, cwhy     
      endif
c
c_____________________________________________________________
c               Step 24; Identify call (note only if short)
c
      if(ishort.gt.0 .and. nd.gt.0) then
        call GetCall(isAP, imcdL(isAP), nAP, ctype1)        
      endif  
c
c_____________________________________________________________
c               Step 24; Return
c
      RETURN
c
c_____________________________________________________________
c               Formats
c
  270   format(/, 
     1  '  DivAlt (Type 39); Operation Right ID = ', a12,
     1  ' Alternate Point Type = ', a12, ' Diversion Type = ', a12,
     1  ' Carrier (Y/N) = ',a3, ' Reuse Plan (Y/N) = ', a3,/
     1  '  DivAlt       ID           ',
     1  '  iyr  mon  day   iw iter  nAP   nd   nw  nd2 imcd imc1',
     1  ' idcd isAP  iup  ilimit',
     1  '    Demand   CapRem1  CapRemAP   RitRem1    Avail1    Pavail',
     1  '   Pavail2    DIVACT iwhy cwhy',/
     1  ' _____________ ____________ ',
     1  ' ____ ____ ____ ____ ____ ____ ____ ____ ____ ____ ____',
     1  ' ____ ____ ____ _______',
     1  ' _________ _________ _________ _________ _________ _________',
     1  ' _________ _________ ____',  45('_'))
     
  280   FORMAT(a14,1x, a12, 1x, i5,1x,a4,12i5,i8,
     1    8F10.0,i5,1x,a45)                    
     
  290   FORMAT(/, '  DivAlt; QDIV ',a12,/,16F7.1)
  300   FORMAT(/, '  DivAlt; QRES ',a12,/,16F7.1)
  310   format(/',  DivAlt; Problem the alternate point type = ', i5)
c
c_____________________________________________________________
c               Error warnings
c
 9999 write(6,1050) 
      write(nlog,1051) 
      
 1050 format('    Stopped in DivAlt',/,
     1       '    See the *.log file')
 1051 format('    Stopped in DivAlt')
      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)
      stop 
      END
