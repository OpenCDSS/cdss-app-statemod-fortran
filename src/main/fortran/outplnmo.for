c
c *********************************************************
c
      SUBROUTINE outplnMo
c
c
c _________________________________________________________
c	Program Description
c
c       outplnMo; It prints plan data to a binary file
c
c _________________________________________________________
c       Update History
c	2006/04/06	Revised to use plan to operating rule
c		        arrays iplnoprE, iplnoprS, 
c                       iplnoprR and iplnoprU
c
c _________________________________________________________
c       Documentation
c
c     	iplntyp	Plan type 1 for T&C,              2 for Well_Aug, 
c                         3 Reuse_Reservoir,      4 Reuse_Diversion, 
c                         5 Reuse_Reservoir_Tmn,  6 Reuse_Diversion_Tmn
c		                  	  7 TransMtn Import       8 Recharge 
c		                  	  9 Out-of-Priority      10 Special Augmentation
c		                  	 11 Accounting Plan      12 Release_Limit
c				
c
c	iox     	= counter for plan output sources and uses
c	ird     	= counter for re-diversion by a type 1
c	maxTC		= max number of T&C outputs before failure data
c		  	    note more than max is stored in last plan
c	iplnoprE(np,nop)= Ties evaporation for a plan to a operating rule
c	pwellC		= Pumping (depletion) in priority
c
c      psuply(np)    Running plan volume for this month. It 
c	                increases or decreases based on opr rules
c	psuplyT(np)   Total monthly plan volume this month 
c                      (may increase but will not decrease based on
c                      operating rules
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
      character ftype*24, parType*24, cfail1*3, cfail2*3
c
c _________________________________________________________
c
c              Step 1; Initilize
c
c		ioutP=0 No details
c		ioutP=1 Details
c		ioutP=11 Details on operating rule 11
      ioutP=0
      small=0.1
      fac=mthday(mon)*factor
      
      if(ioutP.eq.1) then
        write(6,101) 'OutPlnMo  ', nplan
        write(nlog,101) 'OutPlnMo  ', nplan
      endif
 101  format(/,72('_'),/'  Subroutine ', a8, ' Number of plans = ',i4)
c
c
c		Set Max for various plan types
c rrb 2006/09/28; Revised to allow 20 outputs
cr    maxTc=13
cr    maxAug=9
cr    maxResP=12
cr    maxResPX=12
      
      maxTc=28
      maxAug=24
      maxResP=22
      maxResPX=22
      maxRch=23
      
      iox=0
      ior=23
c
c _________________________________________________________ 
c
c               Step 2. Process each plan
      do 170 np=1,nplan
        
        cfail1='Off'
        if(ipfail(np).eq.1) cfail1 = ' On'
        cfail2 = 'Off'
            
        do i=1,40
          dat2(i)=0.0
        end do
c
c ---------------------------------------------------------
c rrb 2006/03/23; Set column with total for various types
cr      if(iplntyp(np).le.2) then
        mTot=maxResP
        if(iplntyp(np).eq.1 .or. iplntyp(np).eq.2 .or.
     1    iplntyp(np).eq.10) mTot=MaxAug
        if(iplntyp(np).eq.8) mTot=maxRch
c
c _________________________________________________________ 
c 		Step 3; Set demand or supply based on type
c		Note iplntyp=1 for T&C, 2 for well augmentation, 3 Reuse_Reservoir,
c                           4 Reuse_Diversion, 5 Reuse_Reservoir_Tmtn
c                           6 Reuse_Diversion_Tmtn
        ifound=0
        IF(pon(np).gt.0) then
          is=ipsta(np)
          if(ioutP.eq.1) then 
            write(nlog,*) ' '
            write(nlog,*) ' OutPlnMo_top; ', np, is, pid(np), cstaid(is)
          endif
          
c
c ---------------------------------------------------------
c rrb 2006/04/02; Print Plan Driver		                    
          IF(iplntyp(np).eq.1 .or. iplntyp(np).eq.2 .or.
     1       iplntyp(np).eq.10) then
            ifound=1
            iox=2
            dat2(1)=Pdrive(np)
            dat2(2)=pdemT(np)
c           write(nlog,*)'  OutPlnMo;', np, pid(np), pdrive(np)*fac
          endif
c
c ---------------------------------------------------------
c		Type 3 or 5 Reuse to a reservoir (use = demand)            
c
c rrb 2006/06/07; Add OOP plan
          if(iplntyp(np).eq.3 .or. iplntyp(np).eq.5) then
            ifound=1
            iox=1
c
c rrb 2008/01/14; Qdiv(28 is from a reuse or admin plan            
cx          dat2(1) = qdiv(28,is)        
            dat2(1) = psuplyT(np)  
          endif  
c
c ---------------------------------------------------------
c		Type 4 or 6 Reuse to a diversion
          if(iplntyp(np).eq.4 .or. iplntyp(np).eq.6) then
            ifound=1
            iox=1
c
c rrb 2008/01/14; Qdiv(28 is from a reuse or admin plan            
cx          dat2(1) = qdiv(28,is)
            dat2(1) = psuplyT(np)  
          endif
c
c ---------------------------------------------------------
c		Type 7 Transmtn Import
          if(iplntyp(np).eq.7) then
            iox=1
            ifound=1
            dat2(1) = qdiv(18,is)
          endif
c
c ---------------------------------------------------------
c		Type 8 Recharge Plan
          if(iplntyp(np).eq.8) then
            iox=2
            ifound=1
            dat2(1) = pdrive(np)
            dat2(2) = psuplyT(np)
cx            write(nlog,*) ' OutPlnMo; ', 
cx     1       np, pid(np), pdrive(np)*fac, psuplyT(np)*fac
          endif
c
c ---------------------------------------------------------
c		Type 9 OOP Plan          
          if(iplntyp(np).eq.9) then
            iox=1
            ifound=1
            dat2(1)=Pdrive(np)
          endif
c
c ---------------------------------------------------------
c		Type 11 Admin Plan
          if(iplntyp(np).eq.11) then
            ifound=1
            iox=1
c
c rrb 2008/01/14; Qdiv(28 is from a reuse or admin plan            
cx          dat2(1) = qdiv(28,is)
            dat2(1) = psuplyT(np)
            if(ioutP.eq.11) then
              write(nlog,*) ' OutPlnMo_X; ', 
     1         np, pid(np), psuplyT(np)*fac
            endif
         endif
c
c ---------------------------------------------------------
c		Type 12 Release Limit Plan          
c			Note psuplyT(np) is the limit at the
c                       beginning of time step
          if(iplntyp(np).eq.12) then
            iox=1
            ifound=1
c
c rrb 2008/01/16; Store as a volume (ac-ft)            
cx            dat2(1)=psuplyT(np)
            dat2(1)=psto1(np)/fac       
cx            
cx            write(nlog,*) '  OutPlnMo; setting psuplyT for type 12', 
cx     1        np, psuplyT(np)*fac
          endif
        
c
c ---------------------------------------------------------
c
c		Warn if not found
          if(ifound.eq.0) then
            write(nlog,260) np, iplntyp(np), pid(np)
            goto 9999
          endif  
          
        endif
c
c _________________________________________________________
c
c		Step 4; Find all supplies and uses from the operating rules
c		        Unless it is to a reservoir with 100% reuse
c                       (iplntyp(np).eq.4) then it is handled above 
c
c rrb 2006/06/12; Add maximum plan counter             
cr      do 130 nop=1,10
cx      write(nlog,*) '  OutPlnMo; maxplnU, mtot ', maxplnU, mtot
        do 130 nop=1,maxplnU
          iP1=iPlnTyp(np)          
        
        
c
c rrb 2008/08/09; Evap is a function of operating rule        
cr        kE=iplnoprE(np)
          kE=iplnoprE(np,nop)
          kS=iplnoprS(np,nop)
          kR=iplnoprR(np,nop)
          kU=iplnoprU(np,nop)
          kO=iplnoprO(np,nop)
          kP=iplnoprP(np,nop)
c
c --------------------------------------------------------
c
c		Source is Well pumping in priority
          if(kS.eq.-1) then
            iox=iox+1        
            iox=amin0(iox,mTot-1)            
            dat2(iox)  = dat2(iox)  + pwellC(np)
            dat2(mTot) = dat2(mTot) + pwellC(np)
          endif    
c
c --------------------------------------------------------
c
c		Other operating rule sources
          if(kS.gt.0) then
            iox=iox+1
            iox=amin0(iox,mTot-1)                        
            dat2(iox)  = dat2(iox)  + divo(kS)
            dat2(mTot) = dat2(mTot) + divo(kS)
          endif
c
c --------------------------------------------------------
c
c		Re diversion operating rules
          if(kR.gt.0) then
cr          iox=iox+1
            ior=ior+1
            ior=amin0(ior,23)                        
            dat2(ior)  = dat2(ior)  + divo(kR)
            dat2(24)   = dat2(24) + divo(kR)
          endif  
c
c --------------------------------------------------------
c
c		Use operating rules 
c		Note associated with plan Types 3-7 and 11
          if(kU.gt.0 .and. kP.eq.0) then
            iox=iox+1
            iox=amin0(iox,mTot-1)                        
            dat2(iox) = dat2(iox) + divo(kU)
            dat2(mTot)  = dat2(mTot)    + divo(kU)
            if(ioutP.eq.11 .and. iplntyp(np).eq.11) then
             write(nlog,*) ' '
             write(nlog,*) ' OutplnMo_1; Use; np, iox, KU, divo(ku)*fac'
             write(nlog,*) ' OutplnMo_1; Use',  np,iox,KU,divo(ku)*fac
            endif
          endif  
c
c --------------------------------------------------------
c
c		Multiple Owners (opr type 46)
          if(kU.gt.0 .and. kP.gt.0) then
            iox=iox+1
            iox1=iox
            iox=amin0(iox,mTot-1) 
cx          write(nlog,*) '  OutPlnMo; iox1, mtot iox ', iox1, mtot, iox
c
c rrb 2008/09/03; Correction the following overrides data used later            
c           is=ipsta(kP)
            IS1=ipsta(kP)
c
c rrb 2008/01/14; Qdiv(28 is from a reuse or admin plan
c jhb 2014/07/25 for a plan being split into other plans (op rule type 46)
c                the better values to output in the b68 binary and therefore the *.xpl
c                are the total supplies to each of the other plans, psuplyt
c            dat2(iox)  = dat2(iox)  + qdiv(28,is1)
c            dat2(mTot) = dat2(mTot) + qdiv(28,is1)
c            dat2(iox)  = dat2(iox)  + psuply(kp)
c            dat2(mTot) = dat2(mTot) + psuply(kp)
            dat2(iox)  = dat2(iox)  + psuplyt(kp)
            dat2(mTot) = dat2(mTot) + psuplyt(kp)
            
            if(ioutP.eq.1 .or. 
     1        (ioutP.eq.11 .and. iplntyp(np).eq.11)) then
              write(nlog,*) '  '
              write(nlog,*) '   OutplnMo_2; Use, np, kP, is, is1,',
     1           ' psuplyT'
              write(nlog,*) '   OutplnMo_2; Use',np, kP, is, is1, 
     1          psuplyT(kp)*fac
            endif
          endif            
c
c --------------------------------------------------------
c
c		Out of Priority operating rule sources
          if(kO.gt.0) then
            iox=iox+1
            iox=amin0(iox,mTot-1)                        
            dat2(iox)  = dat2(iox)  + divo(kO)
            dat2(mTot) = dat2(mTot) + divo(kO)
          endif
          
c
c
c --------------------------------------------------------
c		Evaporation
c		Note associated with plan Types 3 and 5			
          if(kE.gt.0) then
            iox=iox+1
            iox=amin0(iox,maxresP-1)
            dat2(iox)=dat2(iox)+Pevap(np)    
            dat2(maxresP)=dat2(maxresP)+Pevap(np)  
          endif  
c
c		End operating right loop
  130   continue
c
c _________________________________________________________ 
c
c		Step 7; Shortage for a T&C and Well Augmentation
c                   Note do not add pfail(np) to pfail(np) since
c                   it is embedded in the demand via bomsec              
c
        if(iplntyp(np).eq.1) then
          dat2(mTot-1) = dat2(2) - dat2(mTot)
          dat2(mTot)=dat2(mTot) + dat2(mTot-1)          
        endif

        if(iplntyp(np).eq.2 .or. iplntyp(np).eq.10) then
          dat2(mTot-1) = dat2(2) - dat2(mTot)
          dat2(mTot)=dat2(mTot) + dat2(mTot-1)
        endif
c
c _________________________________________________________ 
c
c		Step 8; Set failure data for all       
c		Note pfail is stored as a volume to maintain
c		continuity
        if(iplntyp(np).eq.1) pfail(np) = dat2(maxTC+3)*fac

        if(ipfail(np).gt.0 .and. pfail(np) .gt. small) then
          cfail2 = ' On'
        else
          cfail2 = 'Off'
          pfail(np)=0.0
        endif  
c
c _________________________________________________________ 
c
c		Step 9; Print results to binary file
     
        IREC=(IYR-IYSTR)*12*Nplan+(MON-1)*Nplan + np
c        
c --------------------------------------------------------
c
c		a. T&C Plans        
        if(iplntyp(np).eq.1) then
          write(68,rec=irec) pid(np), cstaid(is),
     1      iyrmo(mon), xmonam(mon), (dat2(i),i=1,maxTC),
     1      cfail1, cfail2, pfail(np)     
        endif
c        
c --------------------------------------------------------
c
c		b. Well Augmentation        
        if(iplntyp(np).eq.2 .or. iplntyp(np).eq.10) then
cr        write(nlog,*) ' OutPlnMo; irec', irec
        
          write(68,rec=irec) pid(np), cstaid(is),
     1      iyrmo(mon), xmonam(mon), (dat2(i),i=1,maxAug),
     1      cfail1, cfail2, pfail(np)
        endif
c        
c --------------------------------------------------------
c
c		c. Reservoirs  and OOP Plans   
c
c rrb 2006/06/07; Add OOP plans
        if(iplntyp(np).eq.3 .or. iplntyp(np).eq.5 .or.
     1     iplntyp(np).eq.9) then
     
          write(68,rec=irec) pid(np), cstaid(is),
     1      iyrmo(mon), xmonam(mon), (dat2(i),i=1,maxresP),
     1      psto1(np)/fac, psto2(np)/fac, pfail(np)        
     
          if(ioutP.eq.1) write(nlog,*) '  OutPlnMo; ',
     1      pid(np), cstaid(is),
     1      iyrmo(mon), xmonam(mon), (dat2(i),i=1,maxresP),
     1      psto1(np), psto2(np), pfail(np)        
     
        endif
c
c --------------------------------------------------------
c		d. Non Reservoirs
c        
        if(iplntyp(np).eq.4 .or. iplntyp(np).eq.6 .or.
     1     iplntyp(np).eq.7 .or. iplntyp(np).eq.11.or.
     1     iplntyp(np).eq.12) then      
        
          write(68,rec=irec) pid(np), cstaid(is),
     1      iyrmo(mon), xmonam(mon), (dat2(i),i=1,maxresPX),
     1      cfail1, cfail2, pfail(np)
c
c rrb 2008/09/02   
          if(ioutP.eq.11 .and. iplntyp(np).eq.11) then 
            write(nlog,*) ' OutPlnMo; ', is, np, pid(np), cstaid(is),
     1      iyrmo(mon), xmonam(mon), (dat2(i),i=1,maxresPX),
     1      cfail1, cfail2, pfail(np)
          endif
        endif
c
c --------------------------------------------------------
c		d. Recharge
c        
        if(iplntyp(np).eq.8) then
        
          write(68,rec=irec) pid(np), cstaid(is),
     1      iyrmo(mon), xmonam(mon), (dat2(i),i=1,maxrch),
     1      cfail1, cfail2, pfail(np)
        endif
        
c
c
c --------------------------------------------------------
c
c               End plan loop
  170   continue
c
c _________________________________________________________
c
c               Step 10; Return
c
      return
c
c _________________________________________________________
c
c               Formats
c
 
  260 format(/,
     1 ' OutPlnMo; Problem cannot find a supply or demand for ',/
     1 '           Plan number ', i5,' Plan Type ', i5,' Plan ID ',a12)
    
  500 format( ' Stopped in OutPlnMo, see log file (*.log)')
c
c _________________________________________________________
c
c               Formats
  
 9999 write(6,500)
      write(nlog,500)
      write(6,*) 'Stop 1'
      call flush (6)
      call exit(1)      

      stop 
      END
