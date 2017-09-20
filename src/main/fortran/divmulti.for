c *********************************************************
c
      SUBROUTINE DivMulti(IW,L2,ncallX)
c
c
c _________________________________________________________
c	Program Description
c
c       DivMulti; Type 46.
c		 It simulates multiple ownership
c		 Currently transfers water from an admin plan
c		 to one or more admin plans.
c
c		Source 1 is an Admin Plan
c		Destination(s) 1-n are Admin Plans
c
c		Future enhancement apply to a water right
c
c
c_____________________________________________________________
c
c       Update History
c 
c rrb 2006/04/27; Copied WelAugP
c		  Revised accordingly
c               
c _________________________________________________________
c
c       Documentation
c	
c       IW              Global water right ID
c       L2              LOC. OF operation right  in opr RIGHT TABLE
c       nwR             source water right
c                       
c       ceff            ratio of GW to SW efficicney
c                       
c       depx            depletion in this month by pumping
c       divact          total well pumping
c                       
c       divsprx         fraction of structure demand served by
c                       sprinklers
c       divothx         fraction of structure demand served by 
c                       other (non-sprinklers)
c                       
c       divreq          Diversion demand remaining after an iteration
c       divreqw         Well demand remaining after an iteration. Note
c                       divreqw uses well eff. for well only lands or
c                       if not adding demands (idemtyp=1)
c                       divreqw uses SW eff. for lands served by both
c                       SW & GW
c       dcrdivw(l2)     Well Decree (cfs) from Riginp
c       divdw(l2)       Well Decree diverted in previous iterations
c       divcapw(nd)     Well capacity (cfs) from Datinp
c       divmonw(nd)     Well capacity (cfs) used in previous iterations
c                       
c       diveff(mon,nd)  Average diversion efficiency via *.dds
c       diveffw(mon,nd) Average well efficiency via *.wes
c                       
c       effmaxs(nd)     Maximum sprinker efficiency via *.tsp
c       effmaxw(nd)     Maximum flood efficiency via *.tsp
c                       
c       effd            Average diversion efficiency via *.dds
c       effa            Average well efficiency via *.wes
c                       
c       effs            Maximum sprinker efficiency via *.tsp
c       efff            Maximum flood efficiency via *.tsp 
c                       
c       iout            Switch: 0 no print; 1 yes print
c       idemtyp         Switch set in datinp via *.ctl
c                       1=do not add demand data from *.ddm and *.wem
c                       2=add demand data from *.ddm and *.wem
c                       3=total demand provided in *.ddm 
c                       4=total demand provided in *.ddm and do not
c                         limit SW demands based on other water 
c                         supplies (e.g. wells)
c                       5=same as 4 but demand is:
c                         max(input,water right)
c       idvstaw(nd)     River location of well
c       idivcow2(nd)    0=GW only structure
c                       +=diversion tied to well structure +
c       nd2             scalar for idivcow2(nd)
c       ishort          code for reoperation; 0=no, 1=yes
c                       
                        
c                       
c       idivsww         on/off switch (0=off, 1=on)
c       idivcow2(nw)    SW diversion, if any, associated with well nw 
c                       
c	      ipAug		        0=no Well Augmentation Calculations
c			                  1 yes Well Augmentation Calculations 
c                       
c       iscd            River location of source plan 
c       	           	 (Iscd=ipsta(nsp)
c                       
c       ispr            =0 use flood efficiency in rtnsecw
c                       =1 use sprinker efficiency in rtnsecw
c                       
c       itsfile         Switch via datinp (*.ctl)
c                       0=no GW acres by year provided
c                       1 = yes GW acres by year provided and demand
c                       is limited by amount via bomsec.f
c       iuse            User = nd for well structure
c                            = nduser(nd) for a diversion structure
c                       Note: multi user option is turned off in datinp!
c                       
c       nd              well ID 
c                       
c       ndnnod(iscd)    Number of downstream nodes from source
c       ndns            Number of downstream nodes from well location
c                       
c       qdiv(18  	      Carrier passing thru a structure 
c       qdiv(24,iscd) 	Pumping (diversion) by a well to a user at iscd
c       qdiv(25,iscd) 	Depletion (From River by Well) at river ID iscd
c	      qdiv(28        	Carried, Exchange or Bypass (column 11)
c                      	Source is a reuse or Admin Plan
c		    qdiv(30         From River from a Res or Reuse Plan 
c                        to a T&C or Aug Plan. Note non consumptive
c       qdiv(31        	From River by Exc or Plan
c	
c 	    qdiv(35        	Water with a Reuse or Admin plan source 
c			                  tracked at the destination.
c       qdiv(38         Carried water not used in any calculations
c                       to report River Divert
c	
c       retx            Immediate (this day or month) return.
c                       Used for reoperation control along with
c                       variable ireop set in rtnsecw.for
c	      rlossX		      Total Loss
c       small           a small value for roundoff (0.0) concerns
c
c_____________________________________________________________
c	Dimensions
c	
      include 'common.inc'
      character cwhy*48, cdestyp*12, ccarry*3, cpuse*3, cstaid1*12,
     1          rec12*12, cTandC*3, cidRiv*12, cidBal*12,
     1          cresid1*12
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
      
      if(ichk.eq.146) iout=2
      if(corid(l2).eq. ccall) ioutiw=iw
      
      if(iout.ge.1 .and. ncallx.eq.0) then
        write(nlog,102) corid(l2)
 102    format(/, 72('_'),/ '  DivMulti; ID = ', a12)
      endif             
      
c     write(Nlog,*) ' DivMulti; ncallx, iout, ioutiw, iw', 
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
      divactT=0.0
      divalo = 0.0
      
c 
c rrb 00/12/26; Set variable efficiency (1=on, controlled by ieffmax)
      ieff2 =1              
c
      ishort=0
                       
c
c ---------------------------------------------------------
c               d. Check Avail array
      call chekava(19, maxsta, numsta, avail)
c
c ---------------------------------------------------------
c               e. Initilze temp array to store current 
c		               depletions and returns
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
      cstaid1='NA'
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
c		Daily On/Off Switch end on day x
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
c		Step 2; Set Source 1 a plan
      nsP  =Iopsou(1,L2)
      Iscd=ipsta(nsp)
      ndns=NDNNOD(IsCD)
      imcd=ipsta(nsP)
      cstaid1=pid(nsP)
      ALOCFS=psuply(nsP)
 
c
c ---------------------------------------------------------
c		a. Exit if the plan is off
      if(pon(nsP).eq.0) then
        iwhy=2
        cwhy='Source Plan is off'
        goto 260
      endif  
c
c ---------------------------------------------------------
c		b. Exit if supply is zero
      if(alocfs.lt.small) then
        iwhy=3
        cwhy='Source Plan = zero'
        goto 260
      endif
c
c      
c _________________________________________________________
c               Step 3; Add supply to the river downstream of the source        
      divAdd=alocfs*(-1.0)
      
cx      write(nlog,*) ' '
cx      write(nlog,*) ' DivMulti; ',iyrmo(mon),xmonam(mon), iscd, ndns
cx      write(nlog,*) ' DivMulti; X Avail(80) = ', 0.0, Avail(80)*Fac
       
c
c rrb 2014/11/24; Transfer water by carrier, not the river      
cx      CALL TAKOUT(maxsta, AVAIL ,RIVER ,AVINP ,QTRIBU,IDNCOD,
cx     1              divAdd, NDNS,  ISCD  )  
c
c      write(nlog,*) ' DivMulti; Y Avail(80) = ', 
c      1  DivAdd*fac, Avail(80)*Fac
c      
c _________________________________________________________
c               Step 3; Allocate Supplies to each plan
      cdestyp='Plan     '
c
      ndes=int(oprlimit(l2))
      divactT=0.0
      n1=0
      n2=0
      do n=1,ndes
        n1=n2+1
        n2=n1+1
        
        ndP  =Iopdes(n1,L2)
        idcdD=ipsta(ndP)
        ndnsD=NDNNOD(idcdD)
c     
c
c rrb 2007/08/17; Limit to ownership %
        pct=ropdes(l2,n2)/100.0
        divact=alocfs*Pct
        divactT=divactT+divact
c
c		    qdiv(30         From River from a Res or Reuse Plan 
c                        to a T&C or Aug Plan. Note non consumptive
c 	    qdiv(35        	Water with a Reuse or Admin plan source 
c			                  tracked at the destination.
c       qdiv(38         Carried water not used in any calculations
c                       to report River Divert
c
c
        if(iplntyp(ndP).ne.11) then
          qdiv(35,idcdD) = qdiv(35,idcdD) + divact
        endif
c
c rrb 2008/01/15; If a T&C destination set qdiv(30 an
c		  adjustment to total diversion in outbal2
        if(iplntyp(ndP).eq.1) then
          qdiv(30,idcdD)=qdiv(30,idcdD)+divact
        endif         
        pdrive(ndP) =pdrive(ndP) +divact
        psuply(ndP) =psuply(ndP) +divact
        psuplyT(ndP)=psuplyT(ndp)+divact
      enddo
c      
c
c
c _________________________________________________________
c
c              Step 4. Double Check available flow
c
c rrb 00/05/03; Check entire array, not just downstream               
      call dnmfsow(maxsta, avail, numsta, imcd)
      avail2=avail(imcd)
      
      IF(AVAIL(IMCD).le.(-1.*small)) then
        write(nlog,318) imcd, avail(imcd)*fac 
        write(nlog,320) (avail(iss),iss=1,numsta)
        write(nlog,330) (river(iss),iss=1,numsta)
        goto 9999
      endif
c       
c
c _________________________________________________________
c
c               Step 14; Update Source a plan
c			 Note do not set psuplyT, it is total inflow
      psuply1=psuply(nsP)
      psuply(nsP)=amax1(0.0, psuply(nsP)-divactT)
c      
c _________________________________________________________
c               
c               b; Update operating rule output (DIVO)
      divo(l2)=divo(l2)+divactT
      
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
          write(nlog,270) corid(l2),cdestyp, ccarry,cTandC
        else
c          write(nlog,*) ' '
        endif  
c
c ---------------------------------------------------------
c		b. Data for every time step and iteration
c
        write(nlog,280) '  DivMulti  ',
     1    iyrmo(mon),xmonam(mon), idy,
     1     cstaid1, iwx, iw, nsP, ndes,
     1     alocfs*fac, (iopdes(n,l2), ropdes(l2,n+1), n=1,10,2),
     1     divactT*fac, iwhy, cwhy
     
  280     FORMAT(a12, i5,1x,a4,i5, 1x,a12, 4i8, F8.1,
     1    5(i8, f8.2), f8.1, i5,1x,a48)
     
      endif
      
c
c _________________________________________________________
c
c               Step 16; Check Avail for Roundoff issues
      call chekava(46, maxsta, numsta, avail)
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
  270   format(/, 
     1  '  DivMulti (Type 47); Operation Right ID = ', a12,
     1  ' Destination Type = ', a12,
     1  ' Carrier (Y/N) = ',a3, ' Associated Plan (Y/N) = ', a3/    
     1  '  DivMulti    iyr  mon  idy Source_ID   ',
     1  '   Iter#  Right#     nsP    ndes',
     1  '  AloCfs iopdes1       % iopdes3       % iopdes5       %',
     1  ' iopdes7       % iopdes9       %  DIVACT',
     1  ' iwhy cwhy',/
     1  '____________ ____ ____ ____ ____________',
     1  5(' _______'), 11(' _______'),' ____', 1x, 48('_'))
     
  318   format(/, 72('_'),/
     1  '  DivMulti; Problem Negative Available Flow ',/
     1  '            Imcd = ',i5,' Avail (af) = ',f10.3)
  320   format(/, '  DivMulti; avail  ',/,(10f10.2))
  330   format(/, '  DivMulti; river  ',/,(10f10.2))
     
c
c _________________________________________________________
c
c              Error warnings
c
 9999 write(nlog,270) corid(l2),cdestyp, ccarry,cTandC
         write(nlog,280) '  DivMulti   ',
     1    iyrmo(mon),xmonam(mon), idy,
     1     cstaid1,iwx, iw, nsP, ndes,
     1     alocfs*fac, (iopdes(n,l2), ropdes(l2,n+1), n=1,10,2),
     1     divactT*fac, iwhy, cwhy
c    

      write(6,340) 
      write(nlog,350) 
      call flush(6)
 340  format('    Stopped in DivMulti',/,
     1       '    See the *.log file')
 350  format('    Stopped in DivMulti')
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop 
      END

