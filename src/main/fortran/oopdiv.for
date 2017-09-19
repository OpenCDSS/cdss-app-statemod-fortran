c
c
      SUBROUTINE OopDiv(IW,L2,ISHORT,divactX,ncallx)
c
c
c _________________________________________________________
c	Program Description
c
c 	Type 38
c	OopDiv  Out-of-Priority diversion (not storage)
c               via the upstream storage statute
c		
c
c	Source 1 is the senior (subordinated) reservoir water right
c	Source 2 is the junior diversion or reservoir right diverting
c                out of priority
c
c	Updates:
c rrb 2006/05/26; Copied DirectBy and revised as appropriate
c
c                Approach: 
c                1. Copy DirectBy and make edits 
c
c _________________________________________________________
c	Update History
c                Note for this operating rule, the water right
c                  operates as: 
c
c _________________________________________________________
c
c      Documentation
c
c        IW : OVERALL WATER RIGHT ORDER
c        L2 : LOC. OF operation right  in opr RIGHT TABLE
c
c        lrS            source water right
c        iopsou(1,l2)   source 1 Senior reservoir water right
c        iopsou(2,l2)   Not used
c        iopsou(3,l2)   source 2 Junior diversion or reservoir
c                       water right
c
c	 oprpct(l2)     Percent of the source water right to be bypassd
c	
c
c        iopdes(1,l2)   if > 0 diversion ID 
c        iopdes(2,l2)   destination owner 1
c
c        iopdes(1,l2)   if < 0 destination reservoir ID 
c        iopdes(2,l2)   destination reservoir account

c        nd             source diversion ID
c        iscd           idvsta(l2) stream ID of source water right
c        ndns           # of nodes downstream of source diversion (nd)
c        iuse1          source user
c
c        nDes           destination diversion ID
c        nRes           destination reservoir ID

c        isDes          stream ID of destination diversion (nDes) or reservoir
c                       or carrier
c        ndns2          # of nodes downstream of destination 
c                       diversion (nDes) or reservoir
c        iuseD          destination user 
c
c	 imcdX          pointer to avail array. It chnages from 1 (to allow
c                       debug printout to isDes (flow at destination) to
c                       imcd flow at the minimum location
c
c        icx            subroutine call # (38) for I/O in rtnsec
c        IW             Global water right counter
c        L2             Operational right pointer
c        ishort         code for reoperation; 0=no, 1=yes
c
c	 CuLimit        fraction to be diverted (diversion or depletion)
c	 TcLimit        fraction to apply to T&C requirement
c
c	 divaloS        Allowable diversion 
c	 divaloS1       Remaining decree at source
c	 divaloD	Remaining decree at destinaion
c	 divact         actual diversion at source 
c
c        divreqx        Diversion demand for types 1-3 (standard)
c
c        dcrdivS        Water right at source  (cfs) Set in oprinp
c	 divdS          Amount diverted at source in previous iterations (cfs)
c	 dcrdiv1        Remaining water right used by source for 
c                         source (cfs)
c
c	 dcrdivE        Water right at bypass (cfs) Set in oprinp
c	 divdE          Amount diverted at source in previous iterations (cfs)
c	 dcrdiv2        Remaining water right at bypass (cfs)
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
c
c	 ieffmax       =0 use average efficiency
c		       =1 use maximum efficiency
c
c        ioprtn         Switch for handling return flows when
c                       multiple structures exist at 1 location
c                       currently always set to 1.0 in Datinp.f
c        iout           Switch: 0 no print; 1 print details, 2 summary
c
c        ndnnod(iscd)   Number of downstream nodes
c        ndnr           Number of downstream nodes from return
c                       (ndnr = f (ndnnod)) 
c        qdiv(5, )      InBasin diversion by priority
c        qdiv(8, )      Transmountain diversion by priority
c
c        qdiv(18        Carrier passing thru a structure (e.g. divcar)
c        qdiv(20        From Carrier by Storage or Exchange (e.g. carrpl)

c        qdiv(26, )     Diversion by Bypass
c        qdiv(27, )     Diversion to Carry, Exchange or Bypass
c        qdiv(28, )     Stored via a reuse plan  
c
c	 qdiv(34, )     Diversion by an Out of Priority Diversion

c
c        currtn         Immediate return to diverting node??
c        qtribu         Tributary InFlow (baseflow point)
c        qstern         Flow at diversion (used for transmtn divs)
c        small          a small value for roundoff (0.0) concerns
c
c	 pdem(ip)       Obligation to a plan (ip)
c			Note calculated in RtnsecP
c
c	 ritPapS        Paper fill at source 
c                         (currently just use ritrem()
c	 ritPapD        Paper fill at reservoir destination
c                         (currently just use ritrem()
c
c        qres(4  From carrier by Storage, Exchange, Plan, etc.
c        qres(18,ix)    From River by bypass to Reservoir
c
c	 iP		Plan ID
c
c	 DivOpr		Toal amount diverted by this Opr Rule
c			Paid back by OopBook2. 
c                       Note this value is in acft since it 
c			cululates from one month to the next
c _____________________________________________________________
c	Dimensions
c
      include 'common.inc'
      character 
     1  cwhy*48, cdestyp*12, ccarry*3, cpuse*3, cstaid1*12,
     1  rec12*12, cidvri*12, cresid1*12, cCallBy*12, corid1*12
c
c
c_____________________________________________________________
c               Step 1; Common Initilization
c
c		iout=0 no detials
c		     1 details
c		     2 summary
c		    99 summary without limit to ccall

      iout=0
      ioutiw=0
      
      cCallBy='OopDiv      '
      corid1=corid(l2)
      
      if(ichk.eq.138) iout=2
      if(corid(l2).eq. ccall) ioutiw=iw
c
c ---------------------------------------------------------
c               b. Daily capability
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif

      divact  =0.0
      divactX =0.0
      divaf   =0.0
      pfail1  =0.0
      ioff=0
      
      imcdX=1
      
      divaloS = -1.0/fac
      divaloS1= -1.0/fac
      divaloD = -1.0/fac
      
      divByP  = -1.0/fac
      pavail  = -1.0/fac
      pdem1   = -1.0/fac
      pdem2   = -1.0/fac
      divCU   = -1.0/fac
      divCarry= -1.0/fac
      
      rettot  = 0.0
      dcrdiv1=-1.0/fac
      dcrdiv2=-1.0/fac
      oprmax1=-1.0/fac
c      
c rrb 2006/10/16; Remove divopr limit tied to the amount diverted
c                 by a type 38 (OopDiv.f) operating rule
cr    divOpr1 = divOpr(l2)
      divOpr1 = -1.0
      divOpr2 = divOpr1
      
      cidvri=corid(l2)
      ritPapS=-1.0
      ritPapD=-1.0
      
      cpuse='No'
      cstaid1='NA'
      nc=0
      
      ccarry='No'
      if(intern(l2,1).gt.0) ccarry='Yes'      
c
c ---------------------------------------------------------
c		Set destination data (a ditch or a reservoir)
      cdestyp='NA'      
      nDes=0
      nRes=0
      
      nDes=iopdes(1,L2)
      if(nDes.gt.0) then
        cdestyp='Diversion'
      endif
      
      if(nDes.lt.0) then
        nRes=-nDes
        cdestyp='Reservoir'     
      endif  
c
c ---------------------------------------------------------
c		Detailed header      
      if(iout.ge.1 .and. iw.eq.ioutiw) then      
        ncallX=ncallX+1
        if(ncallX.eq.1) then
          write(nlog,270) corid(l2), cdestyp, ccarry, cpuse,
     1      ropnk(l2)            
        else
          write(nlog,*) ' '
        endif  
      endif  
c      
c ---------------------------------------------------------
c		Miscellaneous      
      ISHORT=0
      iwhy=0
      cwhy='NA'
      icu=0
      culimit=1.0

c
c 		Use maximum efficiency if ieffmax=1     
      ieff2=1
      icx=38
      small = 0.0001
      iscd=-1
      
c
c _________________________________________________________
c               Step 2; Exit if not on this month
c
c rrb 04/22/96; Allow month switch
      if(imonsw(l2,mon).eq.0) then
        iwhy=1
        cwhy='Monthly Switch is Off'
        ioff=1
        goto 260
      endif  
c
c ---------------------------------------------------------
c		For a daily model set demand for beginning of season
      if(iday.eq.1 .and. imonsw(l2,mon).gt.0) then
        if (idy.lt.imonsw(l2,mon)) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 260
        endif  
      endif  
c
c ---------------------------------------------------------
c		For a daily model set demand for end of season
      if(iday.eq.1 .and. imonsw(l2,mon).lt.0) then
        if (idy.gt.iabs(imonsw(l2,mon))) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 260
        endif  
      endif  
      
       
c      
c ________________________________________________________
c               Step 2b; Set Plan pointer
c		iP  = Plan pointer
      iP=ireuse(l2)
      rec12=cDivTyp(l2)
      icu=0
      if(rec12(1:9).eq.'Diversion') icu=0
      if(rec12(1:9).eq.'Depletion') icu=1
      
c
c _________________________________________________________ 
c               Step 3; Set Source 1 Data (a reservoir water right)
c
      lrS=iopsou(1,l2)
      nrS=iresco(1,lrS)
c
c		Exit if source structure is off
      if(iressw(nrS).eq.0) then
        iwhy=2
        cwhy='Source Reservoir is Off'        
        goto 260
      endif
c 
      ISCD=Irssta(nrS)
      cstaid1=cresid(nrS)
      NDNS=NDNNOD(ISCD)
c
c		Set paper fill to remaining decree
      RitPapS=ritrem(lrS)      
c
c _________________________________________________________ 
c               Step 4; Set Source 2
c                Destination is a diversion,
c		 Source 2 is a diversion right diverting OOP
c
      if(nDes.gt.0) then
        lrDes=iopsou(3,l2)
        lrRes=0
        if(idvrsw(lrDes).eq.0) then
          iwhy=3
          cwhy='Source 2 (Div Destination Right) is Off'        
          goto 260          
        endif
      endif
c
c ---------------------------------------------------------
c		Set Source 2
c		Destination is a reservoir 
c		Source 2 is a reservoir right
        
      if(nRes.gt.0) then
        lrRes=-iopsou(3,l2)  
        if(iout.eq.1)
     1    write(nlog,*) ' Divoop; nres, lrres', nres, lrres      
        lrDes=0
c
c		Set paper fill to remaining decree
c rrb 2006/07/26; Revise based on decree until booked over
cr      RitPapD=ritrem(lrRes)       
c  
c rrb 2006/10/16; Remove divopr limit tied to the amount diverted
c                 by a type 38 (OopDiv.f) operating rule       
cr      RitPapD=dcrres(lrRes)-divopr(l2)/fac
        RitPapD=dcrres(lrRes)
        if(irsrsw(lrRes).eq.0) then
          iwhy=4
          cwhy='Source 2 (Res Destination Right) is Off'        
          goto 260
        endif          
      endif  
c
c _________________________________________________________
c rrb 01/06/25; 
c               Step 4; Set destination Demand, etc.
c			 Destination is a diversion
      if(nDes.gt.0) then      
        cdestyp='Diversion'
        nDesx=nDes
        nRes=0
c
c ---------------------------------------------------------
c		a. Exit if destination structure (nDes) is off 
c		   Destination is a diversion
        if(idivsw(nDes).eq.0) then
          iwhy=5
          cwhy='Destination Diversion is Off'          
          goto 260
        endif
c
c ---------------------------------------------------------
c		b. Set Pointers
c		   Destination is a diversion    
        isDes=idvsta(nDes)
        isDesd=isDes
        
        ndns2=ndnnod(isDes)
        imcdX=isDes
c
        iuseD=nduser(nDes)+iopdes(2,l2)-1 
        iuseDX=iuseD
c
c ---------------------------------------------------------
c		c. Set Demand        
c		   Destination is a diversion    
        if(idemtyp.le.3) then
          divreqx2=amin1(divreq(iuseD), divcap(nDes)-divmon(nDes))
        else
          divreqx2=amin1(divsw(iuseD),divcap(nDes)-divmon(nDes))
        endif
        divreqx2=amax1(0.0, divreqx2)
c
c ---------------------------------------------------------
c		d. Set Plan data
c		   Destination is a diversion    
        TcLimit= diveff(mon,nDes)/100.        
        if(icu.eq.0) then
          culimit=1.0
        else
c
c		Use default or plan efficiency
c		Note pfail is in acft for continuity
          culimit=diveff(mon,nDes)/100.
          if(iP.gt.0) then
            pdem1=pdem(iP)        
            pfail1=pfail(iP)
            if(peff(mon,iP).gt.0.) then      
              culimit=peff(mon,iP)/100.
              TcLimit=peff(mon,iP)/100.
            endif  
          endif  
        endif  
      endif
c
c _________________________________________________________
c
c               Step 5; Set destination data 
c			Destination is a reservoir
      if (nRes.gt.0) then
        nDesx=nRes
c
c -------------------------------------------------------
c		a. Exit if destination structure (nDes) is off 
        if(iressw(nDesx).eq.0) then
          iwhy=6
          cwhy='Destination Reservoir is Off'          
          goto 260
        endif
c
c ---------------------------------------------------------
c rrb 2006/09/25; Allow multiple accounts - Initilize
cr      irow=nowner(nRes)+iopdes(2,l2)-1

        nro=1
        if(iopdes(2,l2).lt.0) then
          nro=-iopdes(2,l2)
          irow=nowner(nres)
        endif

        if(iopdes(2,l2).gt.0) then
          nro=1
          irow=nowner(nres)+iopdes(2,l2)-1
        endif        
        iuseDx=irow
c        
c ---------------------------------------------------------     
        isDes=irssta(nRes)
        isDesr=isDes        
        ndns2=ndnnod(isDes)
        imcdX=isDes
        
        iuseD=-1
c
c ---------------------------------------------------------
c		c. Set Demand   
c rrb 2006/09/25; Allow multiple accounts - Demand
        cursa=0.0
        do n=1, nro
          n1=irow+n-1
          cursa=cursa+(ownmax(n1)-curown(n1))
        end do  
     
cr      divreqx2=amin1(ownmax(irow)-curown(irow),
        divreqx2=amin1(cursa,
     1                 volmax(nRes)-cursto(nRes),
     1                 tarmax(nRes)-cursto(nRes))/fac
        divreqx2=amax1(0.0, divreqx2)
c               Step X; Set plan data
c
c ---------------------------------------------------------
c		d. Set Plan data
        TcLimit=1.0
        culimit=1.0
      endif 
c
c _____________________________________________________________
c
c               Step 6; Destination is through a carrier
c		        Adjust diversion location
      if(intern(l2,1).gt.0) then
        ccarry='Yes'
        nc=intern(l2,1)
        isDes=IDVSTA(Nc)
        isDesc=isDes
        ndns2=NDNNOD(isDes)
        divcarry=divcap(nc)-divmon(nc)
      endif
c
c _____________________________________________________________
c               Step 7; Check printout 
c
c     IF(-IOPOUT.eq.ISCD .or. iout.ge.1) then
      if((iout.ge.1 .and. iw.eq.ioutiw).or. iout.eq.99) then      
            
        write(nlog,280) '  OopDiv_In ',
     1    iyrmo(mon),xmonam(mon), idy,
     1    cstaid1,iwx, iw,nwrord(1,iw),l2,lrS,nDes,
     1    nDesx,iuseDx,imcdX, DIVREQx2*fac,
     1    AVAIL(imcdX)*fac, divaloS1*fac, divaloD*fac,
     1    dcrdiv2*fac, divCU*fac, RitPapS, RitPapD,
     1    pavail*fac,  culimit*100., oprmax1*fac, pfail1,
     1    divcarry*fac,divact*fac, dcrdiv1*fac,  pdem1*fac,
     1    divOpr1, divact*fac, iwhy, cwhy
    
      endif
c
c _____________________________________________________________
c		Step 8; Exit if demand equal to zero 
c
      if(divreqx2.lt.small) then
        iwhy=7
        cwhy='Demand (divreqx2) = 0'
        goto 260
      endif
c
c_____________________________________________________________
c               Step 9; Begin generic water supply checks
c		From the destination (isDes) 
c
      CALL DNMFSO(maxsta, AVAIL, IDNCOD, isDes, NDNS2, IMCD)
      imcdX=imcd
c             
c               Print warning if negative available flow
      IF(AVAIL(IMCD).le.(-1.*small)) then
        WRITE(nlog,310) '**OopDiv_8',
     1    IYR,MON,IW,NWRORD(1,IW),L2,lrS, nDes, iuseD,
     1    isDes,IMCD,divreqX2*fac, divact*fac, avail(imcd)*fac
      endif

      if(avail(imcd).le.small) then
        iwhy=8
        cwhy='Available flow (availX) = 0'
c       write(nlog,320) (avail(iss)*fac,iss=1,numsta)        
        goto 260
      endif
c
c_____________________________________________________________
c               Step 10; Calculate Source 1 available decree and 
c                 paper fill. Note units ofdivaloS are cfs
      divaloS1=amin1(ritrem(lrS), RitPapS)/fac
      divaloS=amin1(ritrem(lrS), RitPapS)/fac
      if(iout.eq.1) then
        write(nlog,*) ' OOpdiv; ritrem(lrs), ritPapS'
        write(nlog,*) ' OOpdiv;', ritrem(lrs), ritPapS
      endif  
c
c		Exit if no allowable diversion 
      if((ritrem(lrS)/fac).le.small) then
        iwhy=9
        cwhy='Remaining Source Right (DivaloS) = 0'
        goto 260
      endif  
      
      if((RitPapS/fac).le.small) then
        iwhy=10
        cwhy='Remaining Paper Fill Right (RitPapS) = 0'
        goto 260
      endif  
c
c_____________________________________________________________
c               Step 10; Calculate available destination decree
c
c ---------------------------------------------------------
c		Destination is a diversion
      if(nDes.gt.0) then
c
c rrb 2006/10/13; Correction reservoir water rights are in acft        
cr      divaloS=amin1(dcrdiv(lrDes)-divd(lrDes), divaloS)
cr      if((dcrdiv(lrDes)-divd(lrDes)).le.small) then
c
c rrb 2006/10/16; Diversion units are in cfs
cr      divaloD=(dcrdiv(lrDes)-divd(lrDes))/fac
        divaloD=dcrdiv(lrDes)-divd(lrDes)
        divaloS=amin1(divaloD, divaloS)
        if(divaloD.le.small) then
          iwhy=11
          cwhy='Destination Diversion Right (DivaloD) = 0'
          goto 260
        endif  
      endif

c
c ---------------------------------------------------------
c		Destination is a reservoir      
      if(nRes.gt.0) then
c
c rrb 2006/06/26; Correction      
cr      divaloS=amin1(dcrres(lrRes)/fac, RitPapS/fac, divaloS)
c
c rrb 2006/07/26; Revise based on decree until booked over
cr      divaloS=amin1(ritrem(lrRes)/fac, RitPapD/fac, divaloS)
        divaloS=amin1(RitPapD/fac, divaloS)
        
        if(iout.eq.1)
     1    write(nlog,*) ' OOpdiv; Destination divalos',
     1      lrRes, RitPapD, divalos*fac
        
c
c rrb 2006/07/26; Revise based on decree until booked over
cr      if((ritrem(lrRes)/fac).le.small) then
        if((ritpapD/fac).le.small) then
          iwhy=12
          cwhy='Destination Reservoir Paper Fill (RitPapD) = 0'
          goto 260
        endif  
        
        if((RitPapS/fac).le.small) then
          iwhy=13
          cwhy='Source Reservoir Paper Fill (RitPapS) = 0'
          goto 260
        endif          
      endif
      
      
c
c_____________________________________________________________
c               Step 12; Find mininum downstream available flow
c                       from destination location (isDes)
      
      CALL DNMFSO(maxsta,avail,IDNCOD,isDes,NDNS2,IMCD)
      PAVAIL=avail(IMCD)
      pavail=amax1(0.0,pavail)   
      pavail1=pavail   
c
c rrb 2006/08/08; Enhancement
      divact=amin1(divact, pavail)
      divact=amax1(divact, 0.0)      
c      
c		Exit if no available flow 
      if(pavail.le.small) then
        iwhy=13
        cwhy='Available Flow (AvailX) = 0'
        goto 260
      endif   
c
c_____________________________________________________________
c               Step 13	Destination is a reservoir, 
c		set diversion to available flow, decree or demand      
      if(nRes.gt.0) then
        if(iout.eq.1)      
     1    write(nlog,*) ' OOpdiv; pavail, divaloS, divreqx2',
     1     pavail*fac, divaloS*fac, divreqx2*fac

        divact=amin1(pavail, divaloS, divreqx2)
        divact=amax1(divact, 0.0)
        goto 220
      endif    
c
c_____________________________________________________________
c               Step 14; Destination is a diversion
c               Divert 100% since (divaloS) > available (pavail)
c
      if(pavail.ge.divaloS) then 
        divact=amin1(pavail, divaloS, divreqx2)
        GOTO 220
      endif
c
c ---------------------------------------------------------
c               
c               a. Destination is a diversion
c                  Divert 100% with return flow addition
      iri=nrtn(iuseD)
      IRE=NRTN(iuseD+1)-1
c
c rrb 2006/08/07; Enhancement      
      pctE=oprPct(l2)/100.0
      pctS=1.0-pctE
      if(pctS.le.small) goto 220

c
      call rtnmaxE(1, iuseD, iri, ire, isDes, ndns2, small, 
     1     pctS, pavail, ieff2, cCallBy, corid1)
c
c		Divert 100% with return flow        
      if(pavail.ge.divaloS) then    
         divact=amin1(pavail, divaloS, divreqx2)  
         divact=amax1(0.0, divact)
         GOTO 220
      endif
c
c rrb 2006/08/08; Enhancement Nothing more available to divert
c     write(nlog,*) '  OopDiv; pavail1, pavail', pavail1*fac, pavail*fac
      if(abs(pavail1-pavail).le.small) then    
         divact=amin1(pavail, divaloS, divreqx2)  
         divact=amax1(0.0, divact)
         GOTO 220
      endif
c
c ---------------------------------------------------------
c              
c               b. Destination is a diversion
c                  Divert < 100% with return flow additons
c
      if(pavail.lt.divaloS) then
        divact=pavail 
        divact1=divact
        if(divact.lt.small) goto 260
c
c rrb 00/12/20;               
c ---------------------------------------------------------
c              	c. Check if more can be diverted
c
        if(ieffmax.eq.1) then 
          call dnmfso(maxsta, avail ,idncod,isDes  ,ndns2, imcd)
          if(avail(imcd).gt.small) then
          
c
c rrb 2006/08/07; Enhancement      
            pctE=oprPct(l2)/100.0
            pctS=1.0-pctE
            if(pctS.le.small) goto 220
          
            call rtnmaxE(2, iuseD, iri, ire, isDes, ndns2, small, 
     1           pctS, pavail,ieff2, cCallBy, corid1)
c
c rrb 02/06/27; Limit additional diversion to demand, etc.
            divmore=amin1(divaloS-divact, pavail-divact)
            divmore=amax1(0.0, divmore)
c            
c            write(nlog,*)
c     1       '  OopDiv_11 pcts, pavail1, pavail, divact1,',
c     1       ' divaloS, divmore, divact, effmax', 
c     1       pcts, pavail1*fac, pavail*fac, divact1*fac, 
c     1       divaloS*fac, divmore*fac,divact*fac, effmax(iuseD)

            if(divmore.gt.small) then
              divact=divact+divmore
c              write(nlog,*) '  OopDiv_11 iteration 2; divact',
c     1          divact*fac  
            endif  
          endif
        endif
        
        divact=amin1(divact, divaloS, divreqx2)  
        
      endif
c
c
c_____________________________________________________________
c      
c		Step 15; Exit if no diversion at the source 
  220 continue
      if(divact.le.small) then
        iwhy=14
        cwhy='Available Flow = 0' 
        goto 260
      endif
c
c_____________________________________________________________
c
c		Step 16; Process carrier limitations
      do i=1,10
        if (intern(l2,i).gt.0) then
          intvn=intern(l2,i)
          if (divact.gt.(divcap(intvn)-divmon(intvn))) then
c
            divact=amax1(0.0, divcap(intvn)-divmon(intvn))
            divcarry=divact
          endif  
        endif  
      end do  
c
c_____________________________________________________________
c               Step 17 Take out of river from destination

      CALL TAKOUT(maxsta, AVAIL ,RIVER ,AVINP ,QTRIBU,IDNCOD,
     1              divact, Ndns2,  isDes)                     
c
c_____________________________________________________________
c               Step 18 Add in return flows
c
      if(nDes.gt.0) then
        CALL RTNSEC(icx,divact,L2,iuseD,isDes,nDes,ieff2)
      endif  
c
c _________________________________________________________

c		Step 19; Out of Priority Plan (similar to a reservoir)
      if(iP.gt.0) then
        pdrive(iP)=pdrive(iP)+divact
c
        psuply(iP)=psuply(iP)+divact
        psuplyT(iP)=psuplyT(iP)+divact
        psto2(iP)=psto2(iP)+divact*fac          
        ipsta1=ipsta(iP)
      endif  
c
c ---------------------------------------------------------
c		Detailed Plan output

      if(iout.eq.1) then
        write(nlog,360) divact*fac, rettot*fac,
     1    divact*fac, culimit*100, iP, 
     1    imo, pdem(iP)*fac, pdemT(iP)*fac     
      endif
c
c_____________________________________________________________
c               Step 20; Double Check available flow from the
c                        destination (isDes) downstream
c
 250  CALL DNMFSO(maxsta, AVAIL, IDNCOD, isDes, NDNS2, IMCD)
c             
c               Print warning if negative available flow
      IF(AVAIL(IMCD).le.(-1.*small)) then
        WRITE(nlog,310) cidvri,
     1    IYR,MON,IW,NWRORD(1,IW),L2,lrS, nDes, iuseD,
     1    isDes,IMCD,divreqX2*fac, divact*fac, avail(imcd)*fac
     
     
cr        write(nlog,320) (avail(iss)*fac,iss=1,numsta)
cr        write(nlog,330) (river(iss)*fac,iss=1,numsta)
        goto 9999
      endif
c
c _________________________________________________________
c               Step 21; Update source 1 data (reservoir right)
c rrb 2006/06/16; Store paper fill as remaining decree     
cx    ritPaper(lrS)=ritPaper(lrS)-divact*fac
      ritrem(lrS)=ritRem(lrs)-divact*fac
c
c_____________________________________________________________
c               Step 22; Update destination 
c                        Destination is a diversion
c
c rrb 2006/03/29; Plan Destination
c     if(nDes.gt.0) then
      if(nDes.gt.0 .and. nDes.lt.10000) then
        if(idemtyp.le.3) then
          divreq(iuseD)=divreq(iuseD)-divact
        else
          divreq(iuseD)=amax1(0.0, divreq(iuseD)-divact)
          divsw(iuseD)=divsw(iuseD)-divact 
c
c rrb 01/02/25; Demand options 4 & 5               
          nw2=idivco2(nDes)        
          if(nw2.gt.0) then
            if(ieffmax.le.0) then
              effd=diveff(mon,nDes)/100.
              effw=diveffw(mon,nw2)/100.
            else
              effd=effmax(nDes)/100.
              effw=effmaxw(nw2)/100.
            endif

            dcux=(divact*effd)/effw
c
c rrb 2007/10/01; Remove DivGW          
c           divgw(nw2)=amax1(0.0, divgw(nw2)-dcux)
          endif
        endif
c
c ---------------------------------------------------------
c		b. Update diversion by this structure and user
        divaf=divact*fac

        DIVMON(nDes)=DIVMON(nDes)+divact
        USEMON(iuseD)=USEMON(iuseD)+divact
cx      divd(lrDiv) = divd(lrDiv)+divact
        divd(lrDes) = divd(lrDes)+divact
        
c
c ---------------------------------------------------------
c
c               c. Update diversion and carrier arrays
c                  
        if(nc.eq.0) then
          qdiv(34,isDes)=qdiv(34,isDes)+divact
        else
          qdiv(18,isDes)=qdiv(18,isDes)+divact
          qdiv(20,isDesd)= qdiv(20,isDesd) + divact
        endif
      endif  

c_____________________________________________________________
c               Step 23; Update destination 
c                        Destination is a plan
c
c rrb 2006/03/29; Plan Destination
c rrb 2006/06/07; Plan Destination not operational
c     if(nDes.gt.0) then
cr    if(nDes.gt.10000) then
cr      pdem(nDesx) = pdem(nDesx)-divact
cr
cr        if(nc.eq.0) then
cr          qdiv(26,isDes)=qdiv(26,isDes)+divact  
cr        else
cr          qdiv(26,isDes)=qdiv(26,isDes)+divact  
cr          qdiv(18,isDes)=qdiv(18,isDes)+divact
cr          qdiv(20,isDesd)= qdiv(20,isDesd) + divact          
cr        endif
cr      endif  

c _________________________________________________________
c
c               Step 24; Update Destination data 
c                        Destination is a Reservoir
      if(nRes.gt.0) then
        divaf=divact*fac
        cursto(nRes)=cursto(nRes)+divaf
c
c ---------------------------------------------------------
c rrb 2006/09/25; Allow multiple accounts - Allocate
c		  Note:
c		   iResT1=0 distributes based on ownership ratio
c		   nrown1=number of accounts in this reservoir
c		   iown = first account associated with this reservoir  
c		   icx  = subrouine calling accou.for       
c		   ia   = account to adjust
cr      curown(irow)=curown(irow)+divaf
      
        nrX=nres
        iResT1=0
        nrown1=nro
        iownX=irow
        icx=138
        if(nc.eq.0) then        
          ia=28
        else
          ia=4
        endif
        cresid1=cresid(nrX)
c        
        call accou(maxacc, maxown, nrX, ownmon, curown, accr, ia, 
     1    ownmax, iownX, nrown1, cursa, divaf, iResT1, icx, cresid1)
        
c
c ---------------------------------------------------------
c 
c rrb 2006/07/25; Update decree when diversion occurrs 
c		  to limit to total
c
c rrb 2006/07/26; Revise based on decree until booked over
cr       ritRem(lrRes)=ritRem(lrRes)-divaf
c
        if(nc.eq.0) then
          qres(28,nRes)=qres(18,nRes)+divaf
cr        accr(28,irow)=accr(18,irow)+divaf        
        else
          qres(4,nRes)=qres(4,nRes)+divaf
cr        accr(4,irow)=accr(4,irow)+divaf
          qdiv(18,isDes)=qdiv(18,isDes)+divact
        endif          
c
c               b. Check reservoir roundoff when exiting routine
        call chekres(nlog, maxres, 1, 8, iyr, mon, nRes,nowner,
     1               curown,cursto,cresid)
      endif
      
c
c _________________________________________________________
c               Step 25; Update diversion by this Operating Rule
      DIVO(L2)=DIVO(L2)+divact
c
c ---------------------------------------------------------
c		Store amount diverted Note keep in acft since it 
c		cumulates from one month to the next)      
c
c  
c rrb 2006/10/16; Remove divopr limit tied to the amount diverted
c                 by a type 38 (OopDiv.f) operating rule
cr    divOpr(l2)=divOpr(l2)+divaf
cr    divOpr2=divOpr(l2)
cr    write(nlog,*) ' OopDiv; l2, divopr', l2, divopr(l2)
c
c _________________________________________________________
c               Step 26; Update carrier structures
c
      do i=1,10
        if (intern(l2,i).gt.0) then          
          intvn=intern(l2,i)          
          divmon(intvn)=divmon(intvn)+divact
          inode=idvsta(INTVN)
        endif
      end do  
      
c
c_____________________________________________________________
c               Step 27; Print detailed results if requested
c 
  260 continue
      divactX=divact
      
      if((iout.ge.1 .and. iw.eq.ioutiw) .or. iout.eq.99) then      
        write(nlog,280) '  OopDiv_Out',
     1    iyrmo(mon),xmonam(mon),idy, cstaid1,
     1    iwx, iw,nwrord(1,iw),l2,lrS,nDes, 
     1    nDesx,iuseDx,imcdX,
     1    DIVREQx2*fac,AVAIL(imcdX)*fac,divaloS1*fac, divaloD*fac,
     1    dcrdiv2*fac, divCU*fac,       RitPapS, RitPapD,
     1    pavail*fac,  culimit*100,     oprmax1*fac, pfail1,
     1    divcarry*fac,divact*fac, dcrdiv1*fac,     pdem2*fac,   
     1    divOpr2,  divact*fac, iwhy, cwhy

        if(iout.eq.1) then
          if(imcd.gt.0) then
            write(nlog,281) '  OopDiv_xx',
     1        imcd, divact*fac,pavail*fac, stanam1(imcd)
          else
            write(nlog,281) '  OopDiv_xx',
     1        imcd, divact*fac,pavail*fac
          endif
        endif  
      endif
c    
c_____________________________________________________________
c               Step 28; Set return switch (iretsw), shortage (ishort) 
c                 switch and actual diversion (divact)
      if(nDes.gt.0) then
        if(divact.gt.small) iretsw=1
        if((divact+small).lt.divaloS*pctE) ishort = 1  
      endif  
c
c _____________________________________________________________
c               
c               Step 29 - Check Entire Avail array
c rrb 05/05/11; Check Avail going out of the routine
      call chekava(23, maxsta, numsta, avail)
      
c
c_____________________________________________________________
c               Step 30; Return
c
      RETURN
c
c_____________________________________________________________
c               Formats
c
  270   format(/, 
     1  '  OopDiv (Type 38); Operation Right ID = ', a12,
     1  ' Destination Type = ', a12,
     1  ' Carrier (Y/N) = ',a3, ' Reuse Plan (Y/N) = ', a3,
     1  ' Admin # = ', f15.5,/    
     1  '  OopDiv      iyr mon   day Source ID   ',
     1  '    Iter      Iw  nwrord      l2     lrS    nDes',
     1  '   nDesX  iuseDX', 
     1  '   imcdX divreqX2 availX divaloS divaloD  dcrdiv2  divCU',
     1  ' RitPapS RitPapD pavail culimit oprmax1  pfail1 divCarry',
     1  '  divact dcrdiv1   PdemX divOprX  divact    iwhy',
     1  ' Comment',/
     1  ' ___________ ____ ____ ____ ____________', 
     1  ' _______ _______ _______ _______ _______ _______',
     1  ' _______ _______', 
     1  ' _______ _______ _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______ _______',
     1  ' __________________________')
     
  280   FORMAT(a12, i5,1x,a4, i5, 1x, a12,9i8,18F8.0,i8,1x,
     1   1x, a48)
  281   FORMAT(a12, 143x, i8, f8.0, f8.2, 1x, a24)
  290   FORMAT(/, '  OopDiv_0 QDIV ',a12,/,16F7.1)
  300   FORMAT(/, '  OopDiv_0 QRES ',a12,/,16F7.1)
  310   FORMAT(/, '  OopDiv_0 Problem negative avail',/
     1  '  OopDiv    iyr  mon',
     1  '      Iw  nwrord      l2     lrS     nDes  iuseD', 
     1  '   isDes    imcd divreqx divact   avail'/  
     1  ' ___________ ____ ____', 
     1  ' _______ _______ _______ _______ _______ _______', 
     1  ' _______ _______ _______ _______ _______',/
     1 a12, 2i5, 8i8, 20f8.0)
     
     
  320   format(/, '  OopDiv_0; avail  ',/,(10f10.2))
  330   format(/, '  OopDiv_0; river  ',/,(10f10.2))
  332   format(/, '  OopDiv_0; qtribu ',/,(10f10.2))
  334   format(/, '  OopDiv_0; qstern ',/,(10f10.2))
  340   format(/, '  OopDiv_0; Pavail, imcd, stanam ',
     1    f8.2, i8, 1x,a24)
  350   format(/, '  OopDiv; Problem with the bypass reach')   
  360   format(
     1  'OopDiv_19;  divact  rettot  divact culimit   iP',
     1             '      imo    pdem    pdemT',/
     1  '            ', 5f8.0, 2i8, 20f8.0)

c
c_____________________________________________________________
c               Error warnings
c
 9999   continue
      
        write(nlog,280) '  OopDiv_Out',
     1    iyrmo(mon),xmonam(mon),idy, cstaid1,
     1    iwx, iw,nwrord(1,iw),l2,lrS,nDes, 
     1    nDesx,iuseDx,imcdX,
     1    DIVREQx2*fac,AVAIL(imcdX)*fac,divaloS1*fac,divaloD*fac,
     1    dcrdiv2*fac, divCU*fac,       RitPapS, RitPapD,
     1    pavail*fac,  culimit*100,     oprmax1*fac, pfail1,
     1    divcarry*fac,divact*fac, dcrdiv1*fac,     pdem2*fac,   
     1    divOpr2,  divact*fac, iwhy, cwhy

       write(6,1050) 
      write(nlog,1051) 
      
 1050 format('    Stopped in OopDiv',/,
     1       '    See the *.log file')
 1051 format('    Stopped in OopDiv')
      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)


      stop 
      END








      
      
