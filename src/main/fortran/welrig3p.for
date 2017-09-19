c
c *********************************************************
c
      SUBROUTINE welrig3P(IW,L2,ispruse,retx,divact,ncallX)
c
c
c _________________________________________________________
c	Program Description
c
c       WelRig3P; It simulates diversions (pumping) by a well
c                and ties future depletions to a T&C Plan
c
c_____________________________________________________________
c       Update History
c
c rrb 2007/10/01; Revised to focus on IWR as the indicator for
c		  a well demand, not total headgate demand.
c		  Copied Welrig3. Added data related to ipAug
c		  from WelRigP
c _________________________________________________________
c       Documentation
c
c        IW             Global water right ID
c        L2             LOC. OF operation right  in opr RIGHT TABLE
c        nwR            source water right


c
c	       AvailP1	    	Min Available flow downstream of plan BEFOR
c	       		              any depletion or return calculations
c	       AvailP2	    	Min Available flow downstream of plan AFTER
c	       		              any depletion or return calculations
c	       Avail1		      Min Available flow in network BEFOR
c	       		              any depletion or return calculations
c	       AvailP2	    	Min Available flow in network AFTER
c	       		              any depletion or return calculations
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
c        dcrdivw(l2)   Well Decree (cfs) from Riginp
c        divdw(l2)     Well Decree diverted in previous iterations
c        divcapw(nwe)    Well capacity (cfs) from Datinp
c        divmonw(nwe)    Well capacity (cfs) used in previous iterations
c
c        diveff(mon,nwe) Average diversion efficiency via *.dds
c        diveffw(mon,nwe)Average well efficiency via *.wes
c
c        effmaxs(nwe)    Maximum sprinker efficiency via *.tsp
c        effmaxw(nwe)    Maximum flood efficiency via *.tsp
c
c        effd           Average diversion efficiency via *.dds
c        effa           Average well efficiency via *.wes
c
c        effs1           Maximum sprinker efficiency via *.tsp
c        efff1           Maximum flood efficiency via *.tsp 
c
c        iout           Switch: 0 no print; 1 yes print
c        idemtyp        Switch set in datinp via *.ctl
c                       1=Historic, well demands are in file *.wem for
c				                  both D&W and Well Only lands. Do not add
c                       2=Historic Sum, well demands are in file *.wem 
c				                  for both D&W and Well Only lands. Add them
c				                  to get total demand
c                       3=Structure Demand well demands are in file 
c			                    *.ddh D&W lands and in file *.weh for Well 
c                           Only lands. Do not add
c                       4=Same as 3 but the demand equals their decree.
c        idvstaw(nwe)   River location of well
c        idivcow2(nwe)  0=GW only structure
c                       +=diversion tied to well structure +
c        nd2            scalar for idivcow2(nwe)
c        ishort         code for reoperation; 0=no, 1=yes
c

c
c        idivsww        on/off switch (0=off, 1=on)
c        idivcow2(nwe)  SW diversion, if any, associated with well nw 
c
c	       ipAug	        0=no Well Augmentation Calculations
c		           	        1 yes Well Augmentation Calculations 
c	       depx		        Depletion at this time step
c	       retx		        Return at this time step
c	       depX-retX      Net depletion this time step
c
c        iscdx          River location of well (iscdx = idvstaw(nwe))
c
c        ispr           =0 use flood efficiency in rtnsecw
c                       =1 use sprinker efficiency in rtnsecw
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
c
c        ndnnod(iscdx)  Number of downstream nodes
c        ndnsx          Number of downstream nodes (nwensx=ndnnod(iscdx)
c
c	       pwellC		      Pumping in priority
c
c        qdiv(24,iscdx) Pumping (diversion) by a well to a user at iscdx
c        qdiv(25,iscdx) Depletion (From River by Well) at river ID iscdx
c
c        retx           Immediate (this day or month) return.
c                       Used for reoperation control along with
c                       variable ireop set in rtnsecw.for
c	       rlossX		      Total Loss
c        small          a small value for roundoff (0.0) 
c
c_____________________________________________________________
c	Dimensions
c
      include 'common.inc'
      character cwhy*45, cdestyp*12, ccarry*3, cpuse*3, cstaid1*12,
     1          cidWR*12, rec12*12, cTandC*3, cidRiv*12, cidBal*12,
     1          ctype1*12,cStrOut*12, pid1*12
c
c
c _________________________________________________________
c
c       Step 1 Common Initilization
c
c           		iout = 0 no details
c           		       1 details
c                      2 less detail, 3 less detail, 4 less detail     
      iout=0
      ioutiw=0  
c	
c		  ioutA = 0 no details of well agumentation
c		          1 details of well agumentation
      ioutA=0
c
c     ioutP = 0 do not print data for plan # ioutP
c             x do print data for plan pointer ioutP
      ioutP=0
   
      
      if(ichk.eq.206) iout=2
      if(crigidw(l2).eq. ccall) ioutiw=iw
c
c ---------------------------------------------------------
      if(iout.eq.1 .and. ioutiw.eq.iw) then
        write(nlog,*) ' '
        write(nlog,*) ' _____________________________________'
        write(Nlog,*) ' WelRig3P: iout, ioutiw, iw, crigidw(l2), ',
     1   'cstaid1, ccall' 
        write(nlog,*) '          ',iout, ioutiw, iw, crigidw(l2), 
     1   cstaid1, ccall
      endif
      
c
c			         Detailed structure reporting         
      nwe =idivcow(1,L2)
      cstaid1='NA'      
c
c		Print detailed data for this well Structure
c		not water right      
      if(nwe.gt.0) cstaid1=cdividw(nwe)
      
      if(cstaid1.eq.ccall) ioutiw=iw
      cStrOut='NA'
cx    cStrOut='01_AWP031   '
c
c		Detailed check      
      ioutX=0
cx    if(cstaid1.eq.'64_AWP007') ioutX=1         

        
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
      
      
      divmonx=0.0
      divmon2=0.0
      
      
      AvailP1=-1.0/f
      AvailP2=-1.0/f
      gw2riv1=0.0
      gxTot=0.0
      pWellC1=0.0
      pWellC2=0.0
      
      cidRiv='NA'
      cidBal='NA'
      pid1='NA'
      
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
      pdemT1=-1/f
      pdemT2=-1/f
      
      
      cidWR=crigidw(l2)

      small = 0.001
      smalln=-1.0*small
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
        avtemp(is)=0.0
      end do
c
c _________________________________________________________
c
c		Step 2; Set Source 1 a well water right
      Nwr  = l2
      
c      
c ________________________________________________________
c rrb 2007/10/01;
c               Step 2b; Set Source 2 is a pointer
c	            	ipAug  = T&C plan
c		            ipUse = Reuse plan
      ipAug=0
      ipAug=iplanw(l2)      
      if(ipAug.gt.0) then
        cTandC='Yes'
        pobl1=pobl(imo,ipAug)
        pid1=pid(ipAug)
      endif  
c
c _________________________________________________________
c               Step 3; Destination (nwe) a well
      nd =idivcow(1,L2)
      nwe =idivcow(1,L2)
c
c		Set imcd to well station for detailed output
      iscd=idvstaw(nwe)
      NDNS=NDNNOD(ISCD)
      imcd=idvstaw(nwe)
      
      cIdRiv=cstaid(iscd)
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
        write(nlog,*) ' ______________________________________________'
        write(nlog,*) ' WelRig3P: ID = ', nd, nd2, cdividw(nwe)
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
c ---------------------------------------------------------
c               a. Set efficiency data
      effa=diveffw(mon,nd)/100.0
      effs1=effmaxs(nwe)/100.0
      efff1=effmaxw(nwe)/100.0
c
c	
c ---------------------------------------------------------
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
c --------------------------------------------------------- 
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
      if(nd2.eq.0) AreaT=AreaGfw(nwe) + AreaGsw(nwe)
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
c
c rrb 2010/2/05; Correction        
cx        divreqX=divreqw(new)
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
          divreqX=divreq(iuse)
          divsprX=0.0
          divothX=divreqX
          diwrGS1=0.0
          diwrGF1=divothX*EffF1            
        endif

        diwrGS1=divsprX*EffS1
        diwrGF1=divothX*EffF1          
      endif  
      
      divreqX1=divreqX
c	
c ---------------------------------------------------------
c      
      if(iout.eq.1 .and. ioutiw.eq.iw .or. iout.eq.4) then
         write(nlog,*) ' '
         write(nlog,*) 
     1    ' WelRig3P:    iyr  mon ',
     1   'cstaid1      ityp  nwe  nd2     AreaT', 
     1   '  divreqX1   divsprX   divothX',
     1   '     EffS1     effF1   AreaGfw   AreaGsw'
     
        write(nlog,'(a12,1x,i5,2x,a3,1x, a12,3i5,20f10.2)')
     1    ' WelRig3P: ', iyrmo(mon),xmonam(mon),
     1   cstaid1, idemtyp, nwe, nd2, AreaT, 
     1   divreqX1*fac, divsprX*fac, divothX*fac,
     1   EffS1*100., effF1*100., AreaGfw(nwe), AreaGsw(nwe)
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
     1   nd,  nd2,   diwrGF1*f,   diwrGS1*f,   efff1*100.,  effS1*100., 
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
c                                  aka (divcapw(nwe) - divmonw(nwe))
      divalo1=divalo
      divalo=amin1(divreqx, dcrdivx-divdx, divcapx-divmonx)
      divalo=amax1(0.0,divalo)
      
      if(ioutX.eq.1) then
        write(nlog,*) ' '
        write(nlog,*) ' WelRig3P; 1 ', cstaid1, nwe, 
     1    divalo1*fac, divreqx*f, (dcrdivx-divdx)*f, 
     1   (divcapX-divmonX)*f, divalo*fac
      endif      
      
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
        write(nlog,*) ' WelRig3P; before deplete'
        write(nlog,'(20f8.2)') (avail(i)*f, i=1,numsta)
      endif  
c	
c ---------------------------------------------------------
c      
      if(ipAug.eq.0) then      
        call deplete(DIVACT,depx,nwr,nd)
      else  
        iscdp=ipsta(ipAug)
        ndnsP=Ndnnod(IscdP)
       
        CALL DNMFSO(maxsta,avail,IDNCOD,IscdP,NndsP,ImcdP)
        availP1=avail(imcdP)
        call depleteP(DIVACT,depx,nwr,nwe,ipAug)
      endif  
c	
c ---------------------------------------------------------
c              
      if(iout.eq.3) then
        write(nlog,*) ' WelRig3P: avail after deplete'
        write(nlog,'(20f8.2)') (avail(i)*f, i=1,numsta)
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
c                       Note ispr=1 uses sprinker efficiency
c                       in subroutine return.f
c
c		Note RtnSecWP adjusts both Avail and Avtemp       
c  

      if(iout.eq.1 .and. ioutiw.eq.iw) then      
        write(nlog,*) ' '
        write(nlog,*) ' _____________________________________'
        write(nlog,'(a12,i5,2x,a3,4i5,2(1x,a12),20f10.2)') 
     1   ' WelRig3P; ', iyrmo(mon),xmonam(mon), idy,
     1    l2, iplanw(l2), ipaug, cstaid1, crigidw(l2), dcrdivw(l2),
     1    divact*fac, divsprX*fac
      endif           
c	
c ---------------------------------------------------------
c      
      if(divsprx.gt.small) then
        ispr=1
      
        if(iout.eq.1 .and. ioutiw.eq.iw) then
          write(nlog,*) 
          write(nlog,*) ' WelRig3P; Calling RtnsecW_ Sprinklers',
     1     ispr, ipAug, divact*fac, divsprx* fac, divothx*fac
        endif      
        
        if(ipAug.eq.0) then
          CALL RtnSecW(divsprx,retx1,rlossX1,cuact1,
     1                 l2,ISCDx,nwe,nd2,ieff2,ispr)
        else
          CALL RtnSecWP(divsprx,retx1,rlossX1,cuact1,
     1                 l2,ISCDx,nwe,nd2,ieff2,ispr,ipAug)
        endif
        
      endif
c	
c ---------------------------------------------------------
c      

      if(divothx.gt.small) then
        ispr=0

        if(iout.eq.1 .and. ioutiw.eq.iw) then
          write(nlog,*) 
          write(nlog,*) ' WelRig3P; Calling RtnsecW_ Non Sprinklers',
     1     ispr, ipAug, divact*fac, divsprx*fac, divothx* fac
        endif      
       
        if(ipAug.eq.0) then
          CALL RtnSecW(divothx,retx2,rlossX2,cuact2,
     1                 l2,ISCDx,nwe,nd2,ieff2,ispr)
        else
          CALL RtnSecWP(divothx,retx2,rlossX2,cuact2,
     1                 l2,ISCDx,nwe,nd2,ieff2,ispr,ipAug)
        endif
     
      endif
c	
c ---------------------------------------------------------
c         
      retx=retx1+retx2
      cuact=cuact1+cuact2
      rlossX=rlossX1+rlossX2
      retTot=divact-cuact-rlossX
      totX=cuact+rlossX+retTot
c	
c ---------------------------------------------------------
c      
      if(iout.eq.3) then
        write(nlog,*) ' WelRig3P: avail after rtnsecw'
        write(nlog,'(20f8.2)') (avail(i)*f, i=1,numsta)
      endif  
c     write(nlog,*) ' WelRig3P: divact, retx = ', divact*f,retx*f
c
c ---------------------------------------------------------
c rrb 2009/04/28; Check
      if(cstaid1.eq.cStrOut) then
        ncallX=ncallX+1

        if(ncallX.eq.1) write(nlog,991) 
c       write(nlog,991) 
 991    format(/, '  WelRig3P; where retX is the immediate return',/      
     1    '  WelRig3P   cstaid1        iyr   mo  nwe Iter   Iw',
     1    ' dcrdivX  divact   cuact  rettot  rlossx    totx    retX',/
     1    '____________ ____________ _____ ____ ____ ____ ____',
     1    ' _______ _______ _______ _______ _______ _______ _______')
     
        write(nlog,'(13x, a12,1x, i5, 2x,a3, 3i5, 20f8.0)')
     1    cstaid1, iyrmo(mon),xmonam(mon), nwe, iwx, iw,
     1    dcrdivx*fac, divact*fac, cuact*fac,
     1    rettot*fac,  rlossx*fac, totx*fac, retX*fac
      endif
c
c _________________________________________________________
c
c               Step 19; Check if (Avail) was driven negative,
c
c rrb 00/05/03; Check entire array, not just downstream               
      call dnmfsow(maxsta, avail, numsta, imcd)
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
        gw2riv1=avail(imcd)
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
            gxTot=gxTot+gx            
            gw2riv(iss)=gw2riv(iss) - gx            
c       
cr          write(nlog,*) '  WelRig3P: Negative Avail iss, Gx, Gw2riv',
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
c	
c ---------------------------------------------------------
c            
        if(iout.eq.3) then
          write(nlog,*) 
     1      ' WelRig3P: avail after GW adjustments gx = ',gx*f
          write(nlog,'(20f8.2)') (avail(i)*f, i=1,numsta)
        endif  
        
c
c		End adjustments for negative avail     
      else
cr      write(nlog,*) ' WelRig3P: Positive Avail imcd, avail(imcd)',
cr   1  imcd, avail(imcd)*f        
      endif
c
c _________________________________________________________

c
c              Step 21; Set Plan Obligation for CURRENT depletions 
c                       caused by this well at this time step.
c			                  Note array Avtemp has the current impact 
c			                  (depletion is a - and return flow is a +) of 
c                       this well. Therefore the current Augmentation
c                       obligation (Pobl( )) is the maximum impact
c                       downstream of the well (Balance Point)
c rrb; 2006/03/26; 
      if(ipAug.gt.0) then   
c
c rrb 2006/04/02; Plan Driver is the amount pumped
        pdrive1=pdrive(ipAug)
        pdrive(ipAug)=pdrive(ipAug) + divact
        pdrive2=pdrive(ipAug)
c
        iscd=idvstaw(nwe)
        NDNS=NDNNOD(ISCD)
c
c ---------------------------------------------------------
c		a. Set the net plan obligation (PNetObl) to the
c                  mininum value in Avtemp that contains the impact 
c                  of this well only on the entire network.
c                  Note call dnmfso (not dnmfsoW) to get min from the
c	           well (balance point) downstream
        call dnmfso(maxsta, avtemp, idncod, iscd, ndns, imcP)
        cidBal=cstaid(imcP)
c
c rrb 2008/03/31; Correction (minimum may not be a depletion)        
        pNetObl = amin1(avtemp(imcP), 0.0)
c
c rrb 2010/12/05; Correction the net obligation cannot be an increase        
cx      if(pNetObl.lt.smalln) pNetObl=-1*avtemp(imcP)
        if(pNetObl.lt.smalln) then
          PNetObl=-1*avtemp(imcP)        
          pNetObl=amax1(pNetObl,0.0)
        else
          pNetObl=0.0
        endif
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
        pdemT1=pdemT(ipaug)    
c
c rrb 2010/12/05; Correction. Include amount this well drove
c                 the system negative (gxTot)
c rrb 2010/12/05; Back to original  
        pdemT(ipAug)=pdemT(ipAug) + pNetObl   
cx      pdemT(ipAug)=pdemT(ipAug) + pNetObl - gxTot
        pdemT2=pdemT(ipAug)
c
c ---------------------------------------------------------
c rrb 2006/03/31; 
c		d. Calculate pumping in Priority
c		       Note gxTot is the amount (if any) this well
c          drove the system negative this time step
c		       Therefore if pNetObl +(-gxTot) is > 0 the well 
c          is in priority
        pwellC1=pwellC(ipAug)
        ppri=amax1(pNetObl + gxTot, 0.0)
        pwellC(ipAug)=pwellC(ipAug) + ppri
        pwellC2=pwellC(ipAug)
c
c ---------------------------------------------------------
c		e. Adjust running demand        
        pdem1=pdem(ipAug)
        pdem(ipAug)=pdem(ipAug) + pNetObl - ppri   
c
c rrb 2010/12/05; Correction. gxTot is already included
c                 in pNetObl          
cx      pdem(ipAug)=pdem(ipAug) - gxTot
        pdem2=pdem(ipAug)
        
c
c ---------------------------------------------------------
c
c		e. Detailed Well Augmentation output      
        if(ioutA.eq.1 .and. (iw.eq.ioutiw .or. ioutP.eq.ipAug)) then
          ncallX=ncallX+1
          if(ncallX.eq.1)then
            write(nlog,262) cidWR,cdestyp
          endif  
c
c ---------------------------------------------------------
c
          write(nlog,264) '  WelRig3P-Now   ',
     1      iyrmo(mon),xmonam(mon), idy, iwx, 
     1      cstaid1, cidRiv, cidBal, pid1, ipAug,
     1      divact*fac,  pDrive1*fac,pDrive2*fac, cuact*fac, 
     1      rlossX*fac,  rettot*fac, totX*fac,
     1      depX*fac,    retX*fac, (depX-retX)*fac,
     1      availP1*fac, availP2*fac, avail1*fac,avail2*fac,
     1      pdem1*fac,   pNetObl*fac, gxtot*fac, ppri*fac, 
     1      pdem2*fac,   pDemT1*fac,  pdemT2*fac, 
     1      pwellC1*fac, pwellC2*fac 
     
        endif  
      endif 
      
c
c
c _________________________________________________________
c
c              Step 22; Double Check available flow
c
c rrb 00/05/03; Check entire array, not just downstream               
      call dnmfsow(maxsta, avail, numsta, imcd)
      Avail2=avail(imcd)

c
      IF(AVAIL(IMCD).le.(-1.*small)) then
        WRITE(nlog,310) IYR,MON,IW,nwr,nd,
     1                ISCDx,ISCDx,IMCD,divreqx*fac, DIVACT*fac,
     1                avail(imcd)*fac,gw2riv(imcd)*fac
        write(nlog,*) ' '
        write(nlog,*) ' WelRig3P; avail(imcd)', avail(imcd)
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
      
      divmon1=divmonw(nwe)
      DIVMONw(nwe)=DIVMONw(nwe)+DIVACT
      divmon2=divmonw(nwe)
      
      divdw(l2)  =divdw(l2)  +divact
      
      if(ioutX.eq.1) then
        write(nlog,*) ' WelRig3P; 2 ', cstaid1, nwe, iwhy,
     1   idemtyp, divmon1*f, divmon2*f, divact*f
      endif

c
c
c rrb 00/06/16; 
c ---------------------------------------------------------
c 		b. Global control on demand options
c                  No addition of demands

      divreqw1=divreqw(nwe)
      
      if(idemtyp.eq.1) then
        divreqw(nwe)=divreqw(nwe)-divact
c
c               c. For a D&W, set GW supply (qdiv(24,) for output
c                  to *.xdd, even if not added
        if(nd2.gt.0) then
          iuse=nduser(nd2)
          qdiv(24,iscdx)=qdiv(24,iscdx)+divact
c          
c               Adjusted in return
c         diwrreq(nd2)=diwrreq(nd2)-cuact
        else
c
c rrb 2007/02/21; Add Wells to *.xdd        
          qdiv(24,iscdx)=qdiv(24,iscdx)+divact 
c                 
c               Adjusted in return
c         diwrreqw(nwe)=diwrreqw(nwe)-cuact 
        endif
      endif
c
c ---------------------------------------------------------
c rrb 00/06/16; 
c            	d. Expect total demand to be in *.ddm if not GW only
      if(idemtyp.ge.2) then
c
c rrb 2009/06/19; Update
cx      divreqw(nwe) = divreqw(nwe)-(divact / ceff) 
        divreqw(nwe) = amax1(0.0, divreqw(nwe)-divact)
c
c rrb 2007/02/21; Add Wells to *.xdd        
        qdiv(24,iscdx)=qdiv(24,iscdx)+divact        

        if(nd2.gt.0) then
          iuse=nduser(nd2)
          divreq(iuse) = amax1(0.0, divreq(iuse)-divact)
c
c rrb 2009/06/19; Update
          diwrGS(nd2) = amax1(0.0, diwrGS(nd2) - divact)          
c
c rrb 2009/06/19; Update
        else
cx        divreqw(nwe) = amax1(0.0, divreqw(nwe)-divact)        
cx        cdiwrGSw(nwe) = amax1(0.0, diwrGSw(nwe) - divact)    
cx        diwrGFw(nwe) = amax1(0.0, diwrGFw(nwe) - divact)    
        endif
      endif
      
      if(ioutX.eq.1) then
        write(nlog,*) ' WelRig3P; 3 ', cstaid1, nwe, nd2, 
     1   idemtyp, divreqw1*f, divact*f, divreqw(nwe)*f, 
     1   diwrGSw(nwe)*f, diwrGFw(nwe)*f
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
          write(nlog,270) cidWR,cdestyp
        else
c          write(nlog,*) ' '
        endif  


        write(nlog,280) '  WelRig3P   ',
     1    iyrmo(mon),xmonam(mon), idy,
     1    cstaid1,iwx, iw,nwrord(1,iw),l2,nwr, nd, ipaug,
     1     DIVREQx*f, Avail0*f, Avail1*f, avail2*f,
     1     dcrdivZ*f, DIVCAPz*f,
     1     DIVMONx*f, divmon2*fac, divdw(l2)*f, 
     1     pNetObl*f, gxTot*f, pWellC1*f, pWellC2*f, pdem1*f, pdem2*f, 
     1     divact*f, iwhy, cwhy
  280     FORMAT(a12, i5,1x,a4, i5, 1x,a12, 7i8, 16F8.1,
     1    i5, 1x, a45)
     
      endif
c
c _________________________________________________________
c
c               Step 25; Check Avail for Roundoff issues
      call chekava(19, maxsta, numsta, avail)
      
c
cx      if(iout.eq.1) then
cx        write(nlog,*) '  WelRig3P; Problem at bottom',iyr,mon,iw,nwr
cx        write(nlog,380) crigidw(l2), namedw(l2)
cx        goto 500
cx      endif                                   
c
c _________________________________________________________
c
c                Step 26; Return
      RETURN
c
c _________________________________________________________
c
c                Formats
c
  200   format(/, '  WelRig3P; Problem with sprinkler calcs (Total)',/
     1         10x, i5, 1x, a12,/ 
     1         10x,'   isprink      effs1      efff1      effa' ,/
     1         10x, i10, 3f10.2,/
     1         10x,'   divsprx   divothx   divreqx     small',/   
     1         10x,  20f20.0) 
  240   format(/,
     1         '  WelRig3P; mon, l2, nd,iuse,iscdx,ndnsx',/,10x,20i5)
 
  250   format(/,'  WelRig3P Demand Data',/
     1  '  WelRig3P: ID            iyr  mon  idy   nd  nd2',
     1  '   diwrGF1   diwrGS1     efff1     effS1', 
     1  '   divsprX   divOthX   divreqX')
  252   format( 11x,a12, i5, a5, 3i5, 20f10.0)

  262   format(/, 
     1  '  WelRigP Augmentation Summary;  Well Right ID = ', a12,
     1  ' Destination Type = ', a12,/,
     1  '  Note NetDep is independent of location while NetObl ',
     1        'is not.',/
     1  '       The critical location is at river node Bal_ID ',
     1        'located',/
     1  '       downstream of the structures location Riv_ID',
     1  //
     1  '  WelRigP         iyr  mon  idy  iwx Str_ID       ',
     1  'Riv_ID       Bal_ID       PID            ipAug',
     1  '  Divact pDrive1 pDrive2      CU',
     1  '    Loss  RetTot    TotX    Dep1    Ret1',
     1  '  NetDep AvailP1 AvailP2  Avail1  Avail2',
     1  '   Pdem1  NetObl   GxTot    PPri   Pdem2',
     1  '  PdemT1  PdemT2 pwellC1 pwellC2',/
     1  '________________ ____ ____ ____ ____',4(' ____________'),
     1  25(' _______'))
     
  264   FORMAT(a16, i5,1x,a4, 2i5, 4(1x,a12), i8, 40F8.1)

  270   format(/, 
     1  '  WelRig3P:  Well Right ID = ', a12,
     1  ' Destination Type = ', a12,//
     1  '  WelRig3P    iyr  mon  idy Str_ID      ',
     1  '     iwx      iw  nwrord      l2     NWR      ND   IpAug',
     1  ' DivReqx2 Avail0  Avail1  Avail2 DcrDivZ DivCapZ DivMonx',
     1  ' DivMon2   divdw pNetObl   gxTot pWellC1 pWellC2   Pdem1',
     1  '   Pdem2  DIVACT iwhy cwhy',/
     1  '____________ ____ ____ ____ ____________',
     1  7(' _______'), 16(' _______'),' ____', 1x, 45('_'))
     

  310   FORMAT(/,
     1  '  WelRig3P: Problem negative avail',/
     1  '  WelRig3P:  iyr  mon   iw  nwr   nd iscd iscd imcd',
     1  '   divreqx    divact     avail    gw24iv',/
     1  '  ________ ____ ____ ____ ____ ____ ____ ____ ____',
     1  ' _________ _________ _________ _________',/   
     1  '  WelRig3P:', 8I5,20F10.2)
     
  320   format(   ' WelRig3P: avail  ',10f10.2)
  330   format(   ' WelRig3P: river  ',10f10.2)
  380   format(   '  WelRig3P: Problem with ', a12, 1x, a24)

c
c _________________________________________________________
c
c              Error warnings
  500   continue
c       if(ncallX.eq.1)then
          write(nlog,270) cidWR,cdestyp
c       endif  
        cwhy='Problem'

        write(nlog,280) '  WelRig3P   ',
     1    iyrmo(mon),xmonam(mon), idy,
     1    cstaid1,iwx, iw,nwrord(1,iw),l2,nwr, nd,
     1     DIVREQx*f, Avail0*f, Avail1*f, avail2*f,
     1     dcrdivZ*f, DIVCAPz*f,
     1     DIVMONx*f, divmon2*fac, divdw(l2)*f, 
     1     pNetObl*f, gxTot*f, pWellC1*f, pWellC2*f, pdem1*f, pdem2*f, 
     1     divact*f, iwhy, cwhy
c
c --------------------------------------------------------- 
      write(6,340) 
      write(nlog,350) 
      call flush(6)
 340  format('    Stopped in WelRig3P',/,
     1       '    See the *.log file')
 350  format('    Stopped in WelRigP3')
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop 
      END

