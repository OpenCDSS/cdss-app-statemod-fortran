C     Last change:  RRB  21 Feb 100   11:58 am
C
      SUBROUTINE DivCar(IW,L2,ISHORT,divactx,ncallX)
c
c _________________________________________________________
c	Program Description
c
c       Divcar; It simulates a type 11 operating rule for
c                 carrier structures
c		              Note if the source is a diversion structure the
c                 diversion is limited by the structure capacity
c		              that is implied by a diversion source
c		              Note if the source is a diversion water right the
c                 diversion is limited by the structure capacity
c		              that is implied by a diversion source and its
c                 water right
c		              Note if the source is a reservoir water right the
c                 diversion is limited by the reservoir capacity
c                 and reservoir wate right
c
c		              Note if the diversion has a carrier other than one
c                 that is implied the diversion is limited by the 
c                 structure capacity
c		
c
c
c _________________________________________________________
c
c       Update History
c rrb 2006/12/27; Revised to allow carrier losses to be routed
c		              to a recharge plan
c rrb 2006/09/25; Revised to allow carrier losses to be routed
c		              back to the stream when the destination is a
c		              reservoir. Note if the destination is a diversion
c		              losses are handled via standard efficiency calcs.
c rrb 2006/09/15; Revised to allow carrier losses to be routed
c		              back to the stream.
c rrb 2006/08/18; Revised to work with multiple reservoir
c                 destination accounts
c rrb 2006/03/20; Corrected to limit to carrier capacity when the
c                 source is a water right and the carrier limitation
c		              is implied.
c                 Revised to limit to water right decree when the
c                 source is a diversion water right.
C     
c rrb 2006/01/12; Revise to allow a right to limit at the 
c		              destination for use in South Platte for off
c		              channel reservoirs		
c rrb 2005/11/22; Revised to use iposou(3,lr) to indicate the 
c		              location where water is diverted (e.g. the
c		              source water right is an off-channel reservoir but
c		              water is diverted at a carrier on the mainstem)
c rrb 2005/11/15; Revised to allow conveyance loss (oprloss %). OprLoss
c		              Note if diverted through a carrier, Oprloss:
c		              1. Causes the destination demand to increase 
c		              2. Does not increase the decreed amount 
c		              3. Does not increase the carriers capacity
c		              4. Charges the 
c		
c		    
c rrb 2005/11/10; Revised to allow source 1 to be a reservoir water
c                 right to better handle off channel reservoirs
c
c rrb 01/02/01; Added call to rtnsec for transmountain diversions                
c               to calculate Cu, etc.
c
c rrb 00/02/21; Documented
c rrb 98/03/03; Daily capability added
c
c rrb 00/12/26; For variable efficiency capability replaced lots of 
c               code that appears in many different subroutines
c               with the following:
c               1. Calculate the max available with returns 
c                  (pavail) by calling rtnmax
c
c               2. If pavail.ge.divalo they get what they want.
c                  a. Set the diversion (divact) to demand (divalo)
c                  b. Take out of stream (call takout)
c                  c. Calculate returns (call rtnsec)
c                  d. Exit
c
c               3. If pavail.lt.divalo they are limited to less than
c                     what they want at a maximum efficiency.
c                  a. Set the diversion (divact) to available (pavail)
c                  b. Take out of stream (call takout)
c                  c. Calculate returns (call rtnsec)
c                  d. Check if more can be diverted by operating
c                     at less than maximum efficiency
c                  e. If no more is availble exit.
c
c                  f. If more is available then
c                     f1. Finding max available with returns
c                         (pavail) by calling rtnmax
c                     f2. Take out of stream (call takout)
c                     f3. Calculate returns (call rtnsec)
c rrb 01/12/26; Revised to operate when iuse is undefined under f95
c rrb 02/06/27; Revised logic for maximum efficiency to limit
c               diversion to demand via divalo
c
c rrb 96/03/13; Initilize divact, send returns to bottom & set divo
c
c rrb 01/02/28; Revised logic when destination is a reservoir since
c               variable iuse does not make sense and causes problems
c               in rtnsec
c               
c
c _________________________________________________________
c
c               Documentation
c
c               icx        subtoutine call # (2)
c               IW         : OVERALL WATER RIGHT ORDER
c               L2         : LOC. OF operation right  in opr RIGHT TABLE
c               ioprtn     : Switch for handling return flows when
c                            multiple structures exist at 1 location
c                            currently always set to 1.0 in Datinp.f
c
c               IDVSTA(L2) : STATION CODE OF WHERE DIV. RIGHT L2 LOCATES
c
c               ieff2 = 0 use always average efficiency
c                     = 1 use max efficiency if ieffmax = 1
c               iresw =   destination type
c                       0 diversion destination
c                       1 reservoir destination

c		irit  =  0 Source is a diversion 
c               irit  =  1 Source is a diversion right 
c		irit  = -1 Source is a reservoir right

c               NS1   =  iopsou(1,l2) source pointer
c                        + = diversion
c                        - = water right
c
c               nd2   =  destination pointer
c                        Initially + = diversion
c                        Initially - = reservoir
c			 Later diversion or reservoir pointer
c
c		iopSouR1  =  iopSouR(l2)
c                        source right type
c			 + = diversion
c			 - = reservoir
c               iopsou(1,l2) = NSR =  source water right 
c			 + = diversion
c			 - = reservoir
c               iopsou(2,l2) Use by Oprinp to turn on or Off the 
c			     source right
c
c		iopsou(3,l2) = ndLoc = diversion location
c			 0 = located at the source water right
c			 + = located at iopsou(3,l2) a reservoir location
c			 - = located at iopsou(3,l2) a diversion location
c		iopsou(4,l2) not used
c
c               ipUse      Plan that recieves carrier losses
c                          Note ipUse=ireuse(l2)
c
c	        iplnTyp(ipUse) Plan type must be 8 (Recharge)
c
c               OprLimit=additional carrier limitation (cfs)
c
c		OprLoss= conveyance loss (%)
c		OprLosT= conveyance loss (cfs)
c               dcrdiv = decreed amount 
c               divd   = total amount diverted under this decree
c               (dcrdiv - divd) = decree remaining
c                       (ndnr = f (ndnnod)) 
c
c               qdiv(5, )  From River by priority
c               qdiv(8, )  From River by priority Transmountain 
c
c               qdiv(18    Carrier passing thru a structure (e.g. divcar)
c               qdiv(19    From Carrier by Priority (e.g. divcar)
c               qdiv(28, ) Stored via a reuse plan  
c		qdiv(32, ) Carrier Loss for a diversion
c
c		qres(2,    From carrier by Priority 
c		qres(25, ) Reservoir Seepage by Carrier Loss
c		qres(27, ) Carrier Loss for a reservoir
c
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
c      
      character 
     1 cwhy*48, cDesTyp*12, ccarry*3, cpuse*3, 
     1 cSouTyp*12, cstaidX*12, cresid1*12, cplntyp*12,
     1 cCallBy*12, corid1*12
c     
c _________________________________________________________
c               Step 1; Initilize
c               a. Convergence
      small=0.001
c
c               b. Miscellaneous
c
c		iout = 0 no details
c		       1 details
c                      2 summary      
c		       3 Rtnmax details
c		       4 super summary
c
      iout=0
      ioutiw=0      
      ioutZ=0
      ioutwr=7
      cCallBy='DivCar      '
      
 
      
      if(ichk.eq.111) iout=2
      if(corid(l2).eq. ccall) ioutiw=iw
      corid1=corid(l2)
      
      if(iout.eq.2 .and. ioutiw.eq.iw) then
        ioutZ=1
        if(ncallX.eq.0) then
          write(nlog,*) ' '
          write(nlog,*) ' ___________________________'
cx        write(nlog,*) ' DivCar; l2 = ', l2
          write(nlog,*) ' DivCar; ',iyrmo(mon),xmonam(mon), idy,iwx,
     1      corid1 
        endif       
      else
        ioutZ=0
      endif                

      if(iout.eq.1)
     1  write(nlog,*) '  Divcar; iout, ioutiw, iw', iout, ioutiw, iw
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
      ccarry='No'      
      cpuse='No '      
      
      availX=-1./fac
      divcap1=-1./fac
      capRem1=-1./fac
      capRem2=-1./fac
      capRem3=-1./fac
      
      DcrRem=-1./fac
      imcdX=-1

      divact=0.0
      divactx=0.0
      cursa=0.0
      divaloS=-1.0/fac
      divreqx1=-1.0/fac
      divreqx2=-1.0/fac 
      dcrdivX=-1.0/fac 
      
      ritrem1=-1.0/fac
      ritrem2=-1.0/fac
      
      iResTy1=0
      ISHORT=0
      NS1=0
      nd2=0
      
      if (intern(l2,1).ne.0) ccarry='Yes'
c
      lr=l2
c
      cdestyp='NA'      
      ndtype = iopdesr(l2)
      if(ndtype.eq.2) cdestyp='Reservoir'
      if(ndtype.eq.3) cdestyp='Diversion'
      if(ndtype.eq.7) cdestyp='Plan     '      
      
c
c rrb 2008/09/26; Include source type varaible
      cSouTyp='NA'
      iopSouR1 = iopSouR(l2)
      if(iopSouR1.eq.3)  cSouTyp='Diversion'
      if(iopSouR1.eq.13) cSouTyp='Diversion_WR'
      if(iopSouR1.eq.12) cSouTyp='Diversion_WR'
c
c rrb 2005/11/15; Add conveyance loss      
      OprLoss1=OprLoss(l2)/100.
      OprEff1 = 1.0 - OprLoss1
      OprLosT=0.0
c
c ---------------------------------------------------------
c rrb 2006/12/27; Carrier loss to a Plan
      cpuse='No'
      cplntyp='NA          '

      ipUse=ireuse(l2)
      if(ipUse.gt.0) then
        cpuse='Yes'
        cplntyp='Recharge    '
        if(iplntyp(ipUse).ne.8) then
          write(nlog,430) ipUse
          goto 9999
        endif  
      endif  
      
c
c _________________________________________________________
c
c               Step X; Branch if not on this month
c
c rrb 04/22/96; Allow month switch
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
c rrb 00/12/26; Variable efficiency capability (1=on)
c rrb 2006/10/31; Variable efficiency capability off for an
c		  operating rules(1=on)
c     ieff2=1
c
c rrb 2007/01/04; Use maximum efficiency if ieffmax=1     
c     ieff2=0
      ieff2=1
c
c rrb TEST
cx    ieff2=0      
      icx=2
c
c _________________________________________________________
c               Step 2; Set source data
c
c
c               a. Source structure (NS1) and type 
c                   - negative is a water right
c                   - positive is a diversion structure
c                  Source location (iscd)
c                  Number of downstream nodes (ndns)

      NS1  =Iopsou(1,L2)
c     NsrTyp =iopsou(2,l2)
c
c rrb 2008/09/26; Correction
cx    iopDesR1=iopDesR(l2)
      iopSouR1=iopSouR(l2)
      ndLoc=iopsou(3,l2)
      ncarry=intern(l2,1)
      
      if(iout.eq.1) then
        write(nlog,*) ' Divcar; NS1, iopSouR1 ncarry', 
     1  NS1, iopSouR1  , ncarry
      endif
c _________________________________________________________
c
c		Step 2a; Source is a diversion structure
c
c rrb 2008/09/26; Update, include iopSouR1 is the destination type
cx      if(NS1.gt.0) then
        if(NS1.gt.0 .and. iopSouR1.eq.3) then
        if(iout.eq.1) 
     1   write(nlog,*) ' Divcar; Source is a diversion structure'      
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
c ---------------------------------------------------------        
c
c rrb 2006/09/15; To calculate returns associated with carrier losses
c		  Set return flow data for source structure 
c		  Note assumes 1 user per structure       
        ndS=ns1
        iscdS=idvsta(ndS)
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
c		Step 2b; Source is a diversion (iOpDesR1=13) water 
c                        right (NS1<0)
c
c rrb 2008/08/26; Correction
cx    if(NS1.lt.0 .and. iOpDesR1.ge.0) then      
      if(NS1.lt.0 .and. iopSouR1.eq.13) then      
cr      iout=1
        if(iout.eq.1)
     1    write(nlog,*) ' Divcar; Source is a diversion water right',
     1    ' operated at ndLoc = ', ndLoc      
        IRIT=1 
        NSR=-NS1        
c
c rrb 2006/03/20; Set carrier if source is a diversion or 
c                 diversion water right      
        ccarry='Yes'       
        cSouTyp='Diversion_WR'
c
c		Set NS1 based on the water right location (ndLoc)
c                 ndLoc=0  Reservoir right location
c                       1  Reservoir location        
c			-1 Diversion location 
c        
c ---------------------------------------------------------        
c
c rrb 2006/09/15; To calculate returns associated with carrier losses
c		  Set return flow data for source structure 
c		  Note assumes 1 user per structure       
        ndS=IDIVCO(1,NSR)
        iscdS=idvsta(ndS)        
        IuseS=NDUSER(ndS)
c        
c ---------------------------------------------------------        
c
c		Right operated at source right location                
        if(ndLoc.eq.0) then
          NS1=IDIVCO(1,NSR)
          iscd=idvsta(NS1)
c
c rrb 2006/03/20; Track capacity when implied (diversion or diverion WR)          
          divcap1=divcap(NS1)          
cr        write(nlog,*) ' Divcar; ndloc, ns1, iscd', ncloc, ns1, iscd          
        endif  
c
c		Right operated at a destination (reservoir) location        
        if(ndloc.gt.0) then
          NS1 = ndloc
          ISCD=IRSSTA(NS1)          
        endif  
c
c		Right operated at a diversion location        
        if(ndloc.lt.0) then
          NS1 = -ndloc
          ISCD=Idvsta(NS1)          
c
c rrb 2006/03/20; Track capacity when implied (diversion or diverion WR)          
          divcap1=divcap(NS1)                    
        endif  
               
        NDNS=NDNNOD(ISCD)
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
c		Step 2c; Source is a reservoir (iopSouR1=12) water
c                        right (NS1<0)
c			Note must have a carrier (else no reason
c			for a type 11 rule)
c
c rrb 2008/08/26; Correction
cx    if(NS1.lt.0 .and. iOpDesR1.lt.0) then      
      if(NS1.lt.0 .and. iOpSouR1.eq.12) then      
      if(iout.eq.1) then
        write(nlog,*) ' Divcar; Source is a res water right',-1*ns1
      endif
      
      IRIT=-1
      NSR=-NS1
      cSouTyp='Reservoir_WR'

c
c		              Set NS1 based on 
c                 ndLoc=0  Reservoir right location
c                       1  Reservoir location        
c			-1 Diversion location 
c
c		Right operated at the right location        
        if(ndLoc.eq.0) then     
c
c rrb 2011/07/20; Correction iresco(1,nsr) is the reservoir #
cx        NS1 = iresco(2,NSR)
          NS1 = iresco(1,NSR)
          iscd = irssta(NS1)
cx          write(nlog,*) ' Divcar; at right location, ns1, iscd', 
cx     1      ns1, iscd
        endif  
c
c		Right operated at a reservoir location        
        if(ndloc.gt.0) then
          NS1 = ndloc
          ISCD=IRSSTA(NS1)          
        endif  
c
c		Right operated at a diversion location        
        if(ndloc.lt.0) then
          NS1 = -ndloc
          ISCD=Idvsta(NS1)          
        endif  
        
        if(iscd.eq.0) then
          write(nlog,*) ' Divcar; Problem with corid ', corid(l2),ndloc,
     1     NS1, iscd
          stop 
        endif  
        NDNS=NDNNOD(ISCD)
        cstaidx=cstaid(iscd)

c
c		Warn if no carrier to a reservoir        
        if(ncarry.le.0) then
          write(nlog,260) corid(l2)
          goto 9999
        endif  
      endif 
c
c _________________________________________________________
c               Step 3a. Set Destination (nd2)
c                   - negative is a reservoir
c                   - positive is a structure
      ND2  =Iopdes(1,L2)
      iresw=0
c
c
c _________________________________________________________
c               Step 3b; Destination is a reservoir (nd2 is negative)
      if(nd2.lt.0) then
        iresw=1
        nd2=-nd2
        nr2=nd2
        isres = irssta(nr2)      
        cdestyp='Reservoir'
        
        if (iressw(nr2).eq.0) then
          iwhy=4
          cwhy='Destination Reservoir is Off'
          goto 380
        endif  
c
c ---------------------------------------------------------
c rrb 2006/09/25; Allow multiple accounts - Initilize
        nro=1
        if(iopdes(2,l2).lt.0) then
          nro=-iopdes(2,l2)
          irow=nowner(nr2)
          iresTy1=0
        endif

        if(iopdes(2,l2).gt.0) then
          irow=nowner(nr2)+iopdes(2,l2)-1
          nro=1
          iresTy1=-1
        endif
c      
c ---------------------------------------------------------
c               Step X; Check reservoir roundoff when entering
c			Note in1=0 into a routine, 1 out of a routine
c			     isub1 = subroutine calling chekres
        in1=0
        isub1=11
        call chekres(nlog, maxres, in1, isub1, iyr, mon, nr2,nowner,
     1               curown,cursto,cresid)
      endif
c
c _________________________________________________________
c
c               Step 3c. Destination is a diversion (nd2 is positive)
      if(iresw.eq.0) then
        cdestyp='Diversion'
        isdiv = idvsta(nd2)  
        if(idivsw(nd2).eq.0) then
          iwhy=5
          cwhy='Destination diversion is Off'
          goto 380
        endif  
C
        IUSE=NDUSER(ND2)+Iopdes(2,L2)-1
      endif
c
c _________________________________________________________
c
c               Step 4; Destination is a Diversion
c			Set demand (divreq) and
c                       capacity limit at destination
c                       (divcap-divmon .le. small)
c
cx120 continue
c     write(nlog,*) '  Divcar; iresw, nd2, iuse', iresw,nd2,iuse
c
      IF(iresw.eq.0) then        
        if(DIVREQ(IUSE).LT.small) then
          divreqx1=divreq(iuse)
          iwhy=6
          cwhy='Destination Diversion Demand (Demand1) is Zero'
          goto 380
        endif 
        
        CapRem2=divcap(nd2)-divmon(nd2)        
        if((divcap(nd2)-divmon(nd2)).le.small) then
          iwhy=7
          cwhy='Destination Diversion Capacity (Caprem2) is Zero'
          goto 380
        endif 
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
      IF(iresw.eq.0) then
      else
c      
c rrb 2006/09/28; Not required with 1 structure per station      
cz        AVTEMP(ISCD)=AVTEMP(ISCD)+CURRTN(ISCD)
cz        availX=avtemp(iscd)
cz        imcdX=iscd
      endif  
c
c _________________________________________________________
c
c               Step 6; Check available flow at diversion (avtemp(iscd)
      IF(AVTEMP(ISCD).le.small) then
c 
        IF(iresw.eq.0) then
          if(IRTURN(IUSE).LE.3) ISHORT=1
        endif

        iwhy=8
        cwhy='Available flow (AvailX) equals zero'      
        goto 380
      endif
C
c _________________________________________________________
c		Step X; Set return flow data
      if(iresw.eq.0) then
        iri=nrtn(iuse)
        ire=nrtn(iuse+1)-1
      else
        iuse=0
        iri=0
        ire=0
      endif
c      
c _________________________________________________________
c
c               Step 7a. Destination is a reservoir (iresw=1)
c			                   Source is a diversion (irit=0) or a 
c                        diversion right (irit =1)
c			                   Set Demand based on carrier loss
c                        Limit to remaining capacity (volmax-cursto)
c
c rrb 2006/09/25; Allow multiple accounts - Demand
      if (iresw.eq.1 .and. irit.ge.0) then
        cursa=0.0
        do n=1, nro
          n1=irow+n-1
          cursa=cursa+(ownmax(n1)-curown(n1))
        end do  
        
        CapRem1=cursa/fac
        IF (CapRem1.lt.small) then
          iwhy=9
          cwhy='Res Destination Capacity (CapRem1) = zero'            
          GOTO 380
        endif  
        
c
c ---------------------------------------------------------
c rrb 04/09/03; Allow carrier to a reservoir even if the 
c               current storage is above the target
c		when iressw(nd2) = 3 (see *.res documentation)
        if(iressw(nr2).eq.3) then
          divalo=amin1(cursa,volmax(nr2)-cursto(nr2))/fac
        else        
          divalo=amin1(cursa,volmax(nr2)-cursto(nr2),
     1      tarmax(nr2)-cursto(nr2))/fac
     
          TarMax1=Tarmax(nr2)-cursto(nr2)        
          IF (TarMax1.lt.small) then
cx          write(nlog,*) ' DivCar; Target constraint',
cx   1        nr2, tarmax(nr2), cursto(nr2)
            iwhy=10
            cwhy='Reservoir Target (TarMax1) = zero'            
            GOTO 380
          endif       
        endif
        
        VolMax1=volmax(nd2)-cursto(nr2)        
        IF (VolMax1.lt.small) then
          iwhy=11
          cwhy='Reservoir Capacity (VolMax1) = zero'            
          GOTO 380
        endif      
c
c rrb 2006/03/20; Limit by implied carrier capacity
        divalo=amin1(divalo, divcap(NS1)-divmon(NS1))          
        CapRem1=divcap(NS1)-divmon(NS1)       
        
        IF (CapRem1.lt.small) then
          iwhy=12
          cwhy='Carrier Capacity (CapRem1) = zero'            
          GOTO 380
        endif  
c        
c ---------------------------------------------------------
c rrb 2006/03/20; Limit by source diversion water right        
        if(irit.eq.1) then
          divalo1=divalo
          DcrRem=dcrdiv(nsr)-divd(nsr)
          divalo=amin1(divalo, dcrdiv(NSR)-divd(NSR))
          IF (DcrRem.lt.small) then
            iwhy=13
            cwhy='Source right (DcrRem) = zero'            
            GOTO 380
          endif            
        endif  
c        
c ---------------------------------------------------------
c		Destination is a reservoir
c		Increase Demand to reflect Carrier Loss        
        divalo=amax1(0.0,divalo)
        divreqx1=divalo
        divalo=divalo/OprEff1
        
        divreqx2=divalo        
        
        IF (divalo.lt.small) then
          iwhy=14
          cwhy='Res Demand (Demand1) = zero'            
          GOTO 380
        endif  
        
      endif  
 
c _________________________________________________________
c
c               Step 7b. Destination is a reservoir (iresw=1)
c			                  Source is a reservoir right (irig =-1)
c			                  Set Demand based on carrier loss
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
        
        CapRem1=cursa/fac
        IF (CapRem1.lt.small) then
          iwhy=15
          cwhy='Res Destination Capacity (CapRem1) = zero'            
          GOTO 380
        endif  
        
c
c
c ---------------------------------------------------------
c rrb 04/09/03; Allow carrier to a reservoir even if the 
c               current storage is above the target
c		when iressw(nd2) = 3 (see *.res documentation)
c		Else limit to target and destination account
        if(iressw(nd2).eq.3) then
          divalo=amin1(cursa,volmax(nr2)-cursto(nr2))/fac
        else
          divalo=amin1(cursa,volmax(nr2)-cursto(nr2),
     1    tarmax(nr2)-cursto(nr2))/fac
c
c rrb 2006/08/18; Redundant and overrides ability to divert
c		  to multiple accounts     
cx        divalo=amin1(divalo, (ownmax(irow)-curown(irow))/fac)     
          divalo=amax1(0.0,divalo)
        endif
c        
c ---------------------------------------------------------
c        
        CapRem2=(volmax(nd2)-cursto(nd2))/fac
        IF (CapRem2.lt.small) then
          iwhy=16
          cwhy='Available Max Storage (CapRem2) = zero'            
          GOTO 380
        endif  
        
        if(iressw(nd2).ne.3) CapRem2=(tarmax(nd2)-cursto(nd2))/fac
        IF (CapRem2.lt.small) then
          iwhy=17
          cwhy='Available Target Storage (CapRem2) = zero'            
          GOTO 380
        endif  
        
c
c ---------------------------------------------------------
c		            Destination is a reservoir
c		            Increase Demand for carrier losses
c               Note impose loss AFTER limiting by the right. 
c		            This means the decreee is located at the reservoir, 
c               not the conveyance canal
        divreqx1=divalo
        ritrem1=ritrem(nsr)
        divalo=amin1(divalo,ritrem(NSR)/fac)
        divalo=divalo/OprEff1
        divreqx2=divalo
        
        IF (ritrem1.lt.small) then
          iwhy=16
          cwhy='Destination Res Right (RitRem1) = zero'            
          GOTO 380
        endif  
        
cr      write(nlog,*) '  Divcar; divreqx1, divreqx2',divreqx1, divreqx2
      endif  
c
c _________________________________________________________
c
c               Step 7c. Destination is a diversion
c		                     Source is a diversion (irit=0)
c                        Limit allowable diversion (divalo)
c                        to destination capacity (divcap-divmon)
      IF(iresw.eq.0 .and. IRIT.EQ.0) then
        DIVALO=AMIN1(DIVREQ(IUSE),DIVCAP(ND2)-DIVMON(ND2))
        CapRem2=divcap(nd2)-divmon(nd2)        
        
        IF (CapRem2.lt.small) then
          iwhy=17
          cwhy='Div Destination capacity (CapRem2) = zero'            
          GOTO 380
        endif  
        
c
c ---------------------------------------------------------
c rrb 2006/03/20; Destination is a diversion
c		  Limit by implied carrier capacity
        divalo=amin1(divalo, divcap(NS1)-divmon(NS1))          
        CapRem1=divcap(NS1)-divmon(NS1)       
        
        IF (CapRem1.lt.small) then
          iwhy=18
          cwhy='Carrier Capacity (CapRem1) = zero'            
          GOTO 380
        endif  
c        
c ---------------------------------------------------------
c          
        divalo=amax1(0.0,divalo)
        divreqx1=divalo
        divreqx2=divalo        
        
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
c			                   Adjust demand by OprEff but not the right
c                        This means the decree is located at the diversion
c                        not the destination
c               canal

      IF(iresw.eq.0 .and. IRIT.EQ.1) then
        DIVALO=AMIN1(DIVREQ(IUSE),DIVCAP(ND2)-DIVMON(ND2))
        CapRem2=divcap(nd2)-divmon(nd2)        
c
c rrb 2006/03/20; For a water right source, limit by implied carrier (NS1)
        divalo=amin1(divalo, divcap(NS1)-divmon(NS1))
        CapRem1=divcap(NS1)-divmon(NS1)
        
        IF (CapRem1.lt.small) then
          iwhy=20
          cwhy='Carrier Capacity (CapRem1) = zero'            
          GOTO 380
        endif      
c
c rrb 2006/03/20; Limit by source diversion water right  
        ritrem1=dcrdiv(NSR)-divd(NSR)     
        divalo=amin1(divalo, dcrdiv(NSR)-divd(NSR))           
        divreqx1=divalo
        divreqx2=divalo 
c
c ---------------------------------------------------------
        
        IF (divalo.lt.small) then
          iwhy=21
          cwhy='Source right (RitRem1) = zero'            
          GOTO 380
        endif  
        
c       write(nlog,*) ' Divcar; divreqx1 ', divreqx1*fac       
      endif
c
c _________________________________________________________
c
c		Step 8. Carrier Limitations (Does not include those
c		        implied by a water right source)
c     write(nlog,*) ' DivCar; divalo', divalo*fac
      CapRemX=99999./fac
      do 180 i61=1,10
        if (intern(l2,i61).eq.0) goto 190
        intvn=intern(l2,i61)
c
c rrb 2006/03/20; Track capacity limits        
        CapRemX=amin1((divcap(intvn)-divmon(intvn)), CapRemX)
        if (divalo.LE.(divcap(intvn)-divmon(intvn))) goto 180 
        divalo=divcap(intvn)-divmon(intvn)
  180 continue
  190 continue
  
      CapRem3=CapRemX
      
      IF (divalo.lt.small) then
        iwhy=22
        cwhy='Carrier Capacity (CapRem3) = zero'            
        GOTO 380
      endif  
  
c
c _________________________________________________________
c
c		Step 9.	Secondary (Operating) limit  
      divalo=amin1(divalo,OprLimit(l2))
      divalo=amax1(0.0,divalo)
      
      IF (divalo.lt.small) then
        iwhy=23
        cwhy='Operating Limit (Oprlimt) = zero'            
        GOTO 380
      endif  
      
c
c _________________________________________________________
c
c               Step 10; Set available flow (pavail)
c
c               FIND DOWNSTREAM MINIMUM FLOW STATION
      CALL DNMFSO(maxsta,AVTEMP,IDNCOD,ISCD,NDNS,IMCD)
      
cx      write(nlog,*)
cx    1  ' Divcar; iscd, ndns, imcd, iresw, avtemp(imcd)*fac'
cx      write(nlog,*)
cx     1  '         ',iscd, ndns, imcd, iresw, avtemp(imcd)*fac
c
      availX=avtemp(imcd)
      imcdX=imcd
      
C
      PAVAIL=AVTEMP(IMCD)
c
c rrb 05/07/11 Detailed output      
      divaloS=pavail      
c
      IF(iresw.eq.0) then
c       write(nlog,*) ' Divcar; nd2, iuse, irturn(iuse)'
c       write(nlog,*) ' Divcar;', nd2, iuse, irturn(iuse)
        if(IRTURN(IUSE).EQ.4) PAVAIL=AMIN1(AVTEMP(IMCD), QSTERN(ND2))
c
c rrb 05/07/11 Detailed output      
        divaloS=pavail              
      endif
c
c _________________________________________________________
c
c               Step 11; Set actual diversion (divact)
c 
c
  200 IF(iresw.eq.0) then
        if(IRI.LE.IRE.AND.IRTURN(IUSE).NE.4) GOTO 210
      endif
c
c _________________________________________________________
c
c               Step X; Case 1 Destination is a Reservoir
c			            or a Diversion with no return flows
      
      icase=1
      DIVACT=AMIN1(PAVAIL,DIVALO)
      divact=amax1(0.0, divact)
c
      availX=pavail      
       
      if(iout.eq.1) then
        write(nlog,*) '  Divcar Case 1; pavail, divalo, divact'
        write(nlog,*) pavail*fac, divalo*fac, divact*fac
      endif
      
      IF (DIVACT.LT.small) then
        iwhy=24
        cwhy='Available Flow (AvailX) = zero'            
        GOTO 380
      endif  
c      
c ---------------------------------------------------------
c		            Step 12; Destination is a diversion
c  	            	Calculate return flows using divert-loss
c		              Note Destination is a diversion
c		
      if(iresw.eq.0) then
        CALL RTNSEC(icx,Divact,L2,IUSE,ISCD,nd2,ieff2)
      endif
c
      CALL TAKOUT(maxsta, AVAIL ,RIVER ,AVINP ,QTRIBU,IDNCOD,
     1            DIVACT, NDNS,  ISCD  )
c
C                        
      IF(iresw.eq.0) then
        if(IRTURN(IUSE).NE.4) GOTO 290
      endif

      if (iresw.eq.0) QSTERN(ND2)=QSTERN(ND2)-DIVACT
      GOTO 300
c
c _________________________________________________________
c
c               Step X; Case 2 Can divert 100%
c                   Allowable flow (pavail) > adj demand (divalo)
c
  210 continue
c     write(nlog,*)' Divcar 210; pavail, divalo', pavail*fac,divalo*fac
      IF(PAVAIL.ge.DIVALO) then
        icase=2
      
C
C------  AVAILABLE FLOW IS GREATER THAN ALLOWABLE DIVERSION 
C 
        DIVACT=DIVALO
c
c rrb 04/21/96
        divact=amax1(0.0, divact)
        availX=pavail
        
        IF (DIVACT.LT.small) then
          iwhy=25
          cwhy='Available flow (AvailX) = zero'            
          GOTO 380
        endif      

        CALL TAKOUT(maxsta, AVAIL ,RIVER ,AVINP ,QTRIBU, idncod,
     1            DIVACT,NDNS,  ISCD   )
c
c		Destination is a diversion.
c		Calculate return flows
        if(iresw.eq.0) then
          CALL RTNSEC(icx,Divact,L2,IUSE,ISCD,nd2,ieff2) 
        endif  
      
        GOTO 290
      endif  
c
c _________________________________________________________
c
c               Step X; Case 3 Limited diversion
c                   Consider immediate return flow
c		                when operating at maximum efficiency 
c

      call rtnmax(1, iuse, iri, ire, iscd, ndns, small, pavail, 
     1  ieff2, ioutZ, cCallBy, corid1)
     
      if(ioutZ.eq.1) then
        write(nlog,*) ' Divcar after RtnMax_1; '
        write(nlog,*) ' Divcar;        iuse   ieff2  pavail  divalo'
        write(nlog,'(12x, 2i8, 20f8.0)') 
     1   iuse, ieff2, pavail*fac, divalo*fac
       endif
c
      IF(PAVAIL.ge.DIVALO) then
        icase=3
        DIVACT=DIVALO
        availX=pavail
        
        IF (DIVACT.LT.small) then
          iwhy=26
          cwhy='Available flow (AvailX) = zero'            
          GOTO 380
        endif  
     
        CALL TAKOUT(maxsta, AVAIL ,RIVER, AVINP, QTRIBU, idncod,
     1            DIVACT,NDNS,  ISCD )
c
c		       Destination is a diversion.
c		       Calculate return flows
        if(iresw.eq.0) then
          CALL RTNSEC(icx,Divact,L2,IUSE,ISCD,nd2,ieff2)        
        endif

        GOTO 290
      endif
c
c _________________________________________________________
c
c               Step X; Case 4 AVAILABLE FLOW IS less than ALLOWABLE 
c                        DIVERSION with returns
c
      icase=4
      DIVACT=PAVAIL
      availX=pavail
      IF (DIVACT.LT.small) then
        iwhy=27
        cwhy='Available flow (AvailX) = zero'            
        GOTO 380
      endif
c
c rrb 2006/10/31; Check avail
      if(ioutZ.eq.1) then
        call dnmfso(maxsta, avail ,idncod,iscd  ,ndns  ,imcd)
        write(nlog,*) ' '
        write(nlog,*) ' DivCar; Before Takout ',
     1  ' iuse, ieff2, imcd, avail  = ', 
     1    iuse, ieff2, imcd, avail(imcd)*fac
      endif     
      
      CALL TAKOUT(maxsta, AVAIL ,RIVER ,AVINP ,QTRIBU,IDNCOD,
     1            DIVACT, NDNS,  ISCD  )
c
c
c		Destination is a diversion.
c		Calculate return flows
      if(iresw.eq.0) then
c
c rrb 2006/10/31; Check avail
        if(ioutZ.eq.1) then
          call dnmfso(maxsta, avail ,idncod,iscd  ,ndns  ,imcd)
          write(nlog,*) ' '
          write(nlog,*) ' DivCar; Before RtnSec, ',
     1      'iuse, ieff2, imcd, avail  = ', 
     1       iuse, ieff2, imcd, avail(imcd)*fac
        endif     
        
        CALL RTNSEC(icx,Divact,L2,IUSE,ISCD,nd2,ieff2)
c
c rrb 2006/10/31; Check avail
        if(ioutZ.eq.1) then
          call dnmfso(maxsta, avail ,idncod,iscd  ,ndns  ,imcd)
          write(nlog,*) ' '
          write(nlog,*) ' DivCar; After RtnSec,',
     1      'iuse, ieff2, imcd, avail  = ', 
     1       iuse, ieff2, imcd, avail(imcd)*fac
        endif     
      endif  
c
c ---------------------------------------------------------
c rrb 00/12/26;              
c              11f; Destination is a diversion.
c 		              Check if more can be diverted
c                   if operating at less than max efficiency
cr      if(ieffmax.eq.1) then 
        if(iresw.eq.0 .and. ieffmax.eq.1) then 
          call dnmfso(maxsta, avail ,idncod,iscd  ,ndns  ,imcd)
          

c
c ---------------------------------------------------------
c
          if(avail(imcd).gt.small) then
c rrb 2009/11/01l Revise to allow some CU to occurr by 
c                 setting iter=2
cx          call rtnmax(2, iuse, iri, ire, iscd, ndns,     
            call rtnmax(1, iuse, iri, ire, iscd, ndns, 
     1       small, pavail, ieff2, ioutZ, cCallBy, corid1)
     
            if(ioutZ.eq.1) then
              write(nlog,*) ' Divcar after RtnMax_2; ',
     1        'iuse, ieff2, pavail, divalo, imcd, avail(imcd) ', 
     1        iuse, ieff2, pavail*fac, divalo*fac, 
     1        imcd, avail(imcd)*fac          
c             write(nlog,*) '  Divcar; iteration 2; pavail', pavail*fac
            endif
c
c rrb 02/06/27; Limit additional diversion to demand, etc.
            divmore=amin1(divalo-divact, pavail)
            pavail=amax1(0.0, divmore)
            divaloS=pavail      

            if(pavail.gt.small) then
              CALL TAKOUT(maxsta,AVAIL,RIVER,AVINP,QTRIBU,IDNCOD,
     1                    pavail,NDNS, ISCD  )

c
c
c rrb 2007/045/07; Correction, calculate returns baased on addition (pavail)		
c             CALL RTNSEC(icx,divact,L2,IUSE,ISCD,nd2,ieff2)
              CALL RTNSEC(icx,pavail,L2,IUSE,ISCD,nd2,ieff2)
              divact=divact+pavail
              if(ioutZ.eq.1) then     
                write(nlog,*) '  Divcar; iteration 2; pavail',pavail*fac               
                write(nlog,*) '  Divcar; iteration 2; divact',divact*fac 
              endif 
            endif  
          endif
        endif

c
      IF(IRTURN(IUSE).LE.3) ISHORT=1
c _________________________________________________________
c
c               Step 12.  Update

  290 IF(IOPRTN.EQ.0) GOTO 300
c
c ---------------------------------------------------------
c               
c      
c rrb 2006/09/28; Not required with 1 structure per station      
cr    TEMP=AVAIL(ISCD)
cr    CURRTN(ISCD)=CURRTN(ISCD)+AMIN1(0.,TEMP)
cr    AVAIL (ISCD)=AMAX1(0.,TEMP)
c
c ---------------------------------------------------------
c
  300 continue
  
c
c _________________________________________________________
c
c		Step X; Calcualte return flows associated with 
c		   carrier loss to a Reservoir (iresw=1)
      if(iresw.eq.1) then
        OprLosT=divact*OprLoss1
        if(OprLosT.gt.small) then
          if(ipUse.eq.0) then
            CALL RtnXcu(icx,OprLosT,L2,IuseS,IscdS,ndS)
          else
            ip1=ipUse
c
c rrb 2008/09/29; TURN OFF FOR TESTING            
cx            CALL RtnSecRP(ip1, ip1, OprLosT, 
cx     1        pctlosPP(ip1), rlossP(ip1))
          endif
          
        endif  
      endif  
c
c _________________________________________________________
c		Step X; Check Avail  
  
      CALL DNMFSO(maxsta, AVAIL ,IDNCOD,ISCD  ,NDNS  ,IMCD)

      IF(AVAIL(IMCD).lT.(-1.0*small)) then
c
c
c _________________________________________________________
c rrb 2007/10/05; TEMPORARY CORRECTION find negative (avail(imcd)
c		  and add into system 
c rrb 2008/02/01     
cx      goto 9999
        Adjust=avail(imcd)
        CALL TAKOUT(maxsta,AVAIL,RIVER,AVINP,QTRIBU,IDNCOD,
     1             Adjust,NDNS, ISCD )

         write(nlog,390) corid(l2), iyrmo(mon),xmonam(mon),cstaidx,
     1      icase, IW,L2,IUSE,ISCD,IMCDx,imcd,
     1     DivreqX1*fac,DIVACT*fac, OprLosT*fac,
     1     availx*fac, avail(imcd)*fac, adjust*fac
      
         divactx = divact      
         goto 9999
         
         iout=2
         ioutiw=iw
      endif
c
c ---------------------------------------------------------
c               a. Update reservoir destination data
      if(iresw.eq.1) then
        divaf=divact*fac
        divafL=divaf*OprEff1
        
        cursto(nr2)=cursto(nr2)+divafL
        qres(2,nr2)=qres(2,nr2)+divafL
c
c ---------------------------------------------------------        
c               b. Distribute Diversion to accounts
c rrb 2006/09/25; Revised to work with multiple reservoir
c		  Note:
c		   iresTy1 = 0  distribute based on ownership ratio
c		   iresTy1 = -1 distribute to 1 account
c		   nrown1=number of accounts in this reservoir
c		   iown = first account associated with this reservoir        
c		   icx = calling routine 2=divcar
c		   ia   = account to adjust (2=From River by Storage)
        nrX=nd2
        nrown1=nro
        iownX=irow
        icx=111
        ia=2
        cresid1=cresid(nrX)
c          
        call accou(maxacc, maxown, nrX, ownmon, curown, accr, ia,
     1    ownmax, iownX, nrown1, cursa, divafL, iresTy1,icx, cresid1)
c
c               b. Check reservoir roundoff when exiting routine
        call chekres(nlog, maxres, 1, 11, iyr, mon, nd2,nowner,
     1               curown,cursto,cresid)        
      endif
c
c ---------------------------------------------------------
c		c. Update diversion destination data
      if(iresw.eq.0) then      
        USEMON(IUSE)=USEMON(IUSE)+divact
        DIVREQ(IUSE)=DIVREQ(IUSE)-divact
c
        if (NS1.eq.nd2) then
          DIVMON(ND2)=DIVMON(ND2)+divact
        endif  
      endif
c
c ---------------------------------------------------------
c		d. Update source diversion 
      if(irit.eq.0) then      
        DIVMON(NS1)=DIVMON(NS1)+divact
      endif
c
c ---------------------------------------------------------
c		e. Update source diversion water right
      if(irit.eq.1) then      
        DIVMON(NS1)=DIVMON(NS1)+divact
c
c rrb 2006/03/20; Limit by source diversion water right        
        divd(NSR) = divd(NSR)+divact
        ritrem2=dcrdiv(NSR)-divd(NSR)
      endif
c
c ---------------------------------------------------------
c		f. Update reservoir water right
      if(irit.eq.-1) then
        ritrem1=ritrem(NSR)
        ritrem(NSR)=ritrem(NSR)-divafL
        ritrem2=ritrem(nsr)
      endif  
      
c
c ---------------------------------------------------------
c               g. Update carrier through a structure (qdiv(18,n))
c                  if the source is not the carrier (iscdx.ne.iscd)
cr rrb 2005/12/21; Skip if ncarry is not defined (no carrier)
      if(ncarry.gt.0) then
        ISCDx=IDVSTA(ncarry)
c       write(nlog,*) 'Divcar;', iscdX, iscd
        if(iscdX.ne.iscd) then      
          QDIV(18,ISCD)=QDIV(18,ISCD)+DIVACT
        endif
c
c rrb 2006/01/12; Correction when the source is a diversion or
c                 a diversion water right        
      else
c
c rrb 2007/11/18; Correction; adjust a diversion right also      
c       if(irit.ge.0) then
        if(irit.ne.0) then
          QDIV(18,ISCD)=QDIV(18,ISCD)+DIVACT      
        endif  
      endif  
      
c
c ---------------------------------------------------------
c               h. Update Total Diversion (qdiv5,n)

      QDIV(5,ISCD)=QDIV(5,ISCD)+DIVACT
c
c ---------------------------------------------------------
c               i. Update this diversion water right
c rrb 2009/08/12; Remove, already adjusted above (see e.) 
cx    IF (IRIT.EQ.1) DIVD(NSR)=DIVD(NSR)+DIVACT
c
c ---------------------------------------------------------
c               j. Update diversion by this Operating Rule
      DIVO(L2)=DIVO(L2)+DIVACT
c
c ---------------------------------------------------------
c               k. Update intervening Structures
c		   Does not include implied carriers
      do i11=1,10
        if (intern(l2,i11).eq.0) goto 350
        intvn=intern(l2,i11)
        divmon(intvn)=divmon(intvn)+divact
        inode=idvsta(INTVN)
        qdiv(18,inode)=qdiv(18,inode)+divact
      end do  
  350 continue
c
c ---------------------------------------------------------
c               l. UPDATE DESTINATION DEMAND      
c   grb 1-2-96; Bypass updating of destination demand 
c                 if same as diverting struc.
      INODE=IDVSTA(ND2)
c
      if (NS1.ne.nd2) then
c
c ---------------------------------------------------------
c               m. Update carrier used by a structure (qdiv(19,x))
c                  and destination demand (divmon)
        if (iresw.eq.0) then
          qdiv(19,inode)=qdiv(19,inode)+divact
          DIVMON(ND2)=DIVMON(ND2)+DIVACT
        endif  
      endif    
c
c ---------------------------------------------------------
c               n. Update transmountain (Qdiv(8,n))

      IF(iresw.eq.0) then
        if(IRTURN(IUSE).EQ.4) then
          QDIV(8,ISCD)=QDIV(8,ISCD)+DIVACT
        endif
      endif
c
c ---------------------------------------------------------
c               o. Update actual diversion for testing
  380 divactx = divact
      OprLosT=divact*OprLoss1
c
c ---------------------------------------------------------
c		p. Update amount lost at the destination
c		   diversion (nd2>) or reservoir (ndw<0)      
c		   Note qdiv is by stream location
c			qres is by reservoir in ac-ft   
c _________________________________________________________
c
c               Step 13.  Detalied output
      if(iout.ge.2 .and. iw.eq.ioutiw) then
        ncallX=ncallX+1
        if(ncallX.eq.1)then
          write(nlog,270) corid(l2),cdestyp, cSouTyp, ccarry, cpuse,
     1  cplntyp               
        else
c          write(nlog,*) ' '
        endif  
      
      
      
        write(nlog,280) 'DivCar    ',
     1    iyrmo(mon),xmonam(mon), idy,
     1    cstaidx,iwx, iw,nwrord(1,iw),l2,lr,
     1    NS1, Nd2,ipuse,imcdX, ndloc, iscd,
     1    availX*fac, oprlimit(l2)*fac,
     1    divCap1*fac, CapRem1*fac, CapRem2*fac, CapRem3*fac,
     1    ritrem1, ritrem2, DcrRem, divreqx1*fac, OprLoss(l2),
     1    divreqx2*fac, divaloS*fac, OprLosT*fac, divactx*fac, 
     1    icase, iwhy, cwhy
      endif
c
c _________________________________________________________
c
c               Step 14.  Return
c
      if(iout.eq.4 .and. divactx.gt.small) then
        iouty=iouty+1
        write(nlog,420) corid1,iouty, mon,iw,iwx,l2,nd2,divactx*fac
      endif
      
      RETURN
c
c _________________________________________________________
c
c               Formats
c
  260   format(/, 
     1  '  DivCar (Type 11); Problem with Operation Right ID = ', a12,/
     1  '         The source is a reservoir right but no carrier is',/
     1  '         specified. Recommend you add a carrier or use a ',/
     1  '         Standard reservoir storage right, not a type 11')

  270   format(/, 
     1  '  DivCar (Type 11); Operation Right ID = ', a12,
     1  ' Destination Type = ', a12, ' Source Type = ', a12,
     1  ' Carrier (Y/N) = ',a3, ' Plan (Y/N) = ', a3, 
     1  ' Plan Type = ', a12,/
     1  '  DivCar      iyr mon   day River ID    ',
     1  '    Iter      Iw  nwrord      l2      lr     NS1     Nd2',
     1  '   ipUse   imcdX   ndloc    iscd  AvailX OprLimt Capact1',
     1  ' CapRem1',
     1  ' CapRem2 CapRem3 RitRem1 RitRem2  DcrRem Demand1',
     1  ' OprLosS Demand2 DivaloS OprLosT DIVACTX   icase    iwhy',
     1  ' Comment',/
     1  ' ___________ ____ ____ ____ ____________', 
     1  ' _______ _______ _______ _______ _______ _______ _______', 
     1  ' _______ _______ _______ _______ _______ _______ _______',
     1  ' _______',
     1  ' _______ _______ _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______ _______',
     1  ' __________________________')
  280   FORMAT(a12, i5,1x,a4, i5, 1x, a12,11i8,15F8.0,2i8,1x, a48)
     
  390 FORMAT(/, 72('_'),/
     1  ' DIVCAR; Problem negative avail for ID = ', a12,
     1  ' TEMPORARY FIX'/ 
     1  '  IYR  MON ID           ',
     1  '   icase      iw      l2    IUSE    iscd   imcdx    imcd',
     1  '    DIVREQ    Divact   OprLosT    AvailX     Avail    Adjust',/
     1  ' ____ ____ _____________',
     1  ' _______ _______ _______ _______ _______ _______ _______',
     1  ' _________ _________ _________ _________ _________ _________',/
     1  i5,1x,a4,1x,a12,1x, 7i8, 20F10.2)
  400 format(' divcar: avail  ',10f10.2)
  410 format(' divcar: river  ',10f10.2)
  420 format(' Divcar; ',a12,1x,6i5,20f8.0)
     
  430 FORMAT(/, ' Divcar; Problem the plan type must be an 8',/
     1 9x, 'for recharge, not the value specified = ', i5)
c
c _________________________________________________________
c
c               Error Warnings
c
 9999 continue 
      write(nlog,270) corid(l2),cdestyp, cSouTyp, ccarry, cpuse,
     1  cplntyp     
      
      cwhy='Problem'        
      write(nlog,280) 'DivCar    ',
     1    iyrmo(mon),xmonam(mon), idy,
     1    cstaidx,iwx, iw,nwrord(1,iw),l2,lr,
     1    NS1, Nd2,ipuse,imcdX, ndloc, iscd,
     1    availX*fac, oprlimit(l2)*fac,
     1    divCap1*fac, CapRem1*fac, CapRem2*fac, CapRem3*fac,
     1    ritrem1, ritrem2, DcrRem, divreqx1*fac, OprLoss(l2),
     1    divreqx2*fac, divaloS*fac, OprLosT*fac, divactx*fac, 
     1    icase, iwhy, cwhy

      write(6,1050)
      write(nlog,1051) 
      
 1050 format('    Stopped in Divcar',/,
     1       '    See the *.log file')
 1051 format('    Stopped in Divcar')
      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)


      stop 
      END



