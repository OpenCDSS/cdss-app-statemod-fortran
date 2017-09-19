c
c *********************************************************
c
      SUBROUTINE WelPrity(iw,l2,ncallX)
c
c _________________________________________________________
c	Program Description
c
c       WelPrity; Type 43
c		 It simulates in-priority supply for delayed
c		 delpletions associated with pumping. 
c                Note current time step depletions
c		 are accounted as they occur in WelRigP.f
c
c_____________________________________________________________
c       Update History
c
c rrb 2006/11/16; Copied WelRigP and revised to include a T&C plan
c               
c _________________________________________________________
c       Documentation
c
c        IW             Global water right ID
c        L2             LOC. OF operation right  in opr RIGHT TABLE
c        iout           Switch: 0 no print; 1 yes print
c
c	 ip		Well Augmentation pointer
c
c        iscdx          River location of well (iscdx = idvstaw(nd))
c	 imcd           River location of minimum flow
c
c        ndnnod(iscdx)  Number of downstream nodes
c        ndnsx          Number of downstream nodes (ndnsx=ndnnod(iscdx)
c
c	 PdemX		Pumping in priority
c
c	 Pdem(ip)       Running Augmentation requirement
c	 Pdem1		Initial Augmentation requirement 
c	 Pdem2		Final Augmentation requirement 
c
c        divo(l2) )     Pumping in priority
c	 PwellP1	Initial pumping in priority
c	 pwellP2	Final pumping in priority
c
c        small          a small value for roundoff (0.0) concerns
c
c_____________________________________________________________
c	Dimensions
c
      include 'common.inc'
     
      character cwhy*48, cdestyp*12, ccarry*3, cpuse*3, 
     1 cRelTyp*12, cReplace*3, 
     1 cStaId1*12, cRivId*12, cMinId*12, cDesId*12
c
c _________________________________________________________
c       Step 1 Common Initilization
c
c		iout = 0 No details
c		       1 Details
c                      2 Summary      
c		      99 Summary independent of ccall
      iout=0
      ioutiw=0
      
      if(ichk.eq.143) iout=2
      if(corid(l2).eq. ccall) ioutiw=iw
      
c     write(Nlog,*) ' WelPrity; ichk, iout, ioutiw, iw', 
c    1                          ichk, iout, ioutiw, iw
      
c
c ---------------------------------------------------------
c		Factor      
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif
      small = 0.001

c
c ---------------------------------------------------------
c		Title data      
      iwhy=0
      cwhy='NA'
      cdestyp='Plan'
      ccarry='No'
      cpuse='No'
      cRelTyp='NA'
      cReplace='NA'
      cStaId1=corid(l2)
      cMinId='NA'
      cRivId='NA'
c
c ---------------------------------------------------------
c		OUtput data      
      Avail1=-1./fac
      PdemX=-1/fac
      pdem1=-1./fac
      pdem2=-1./fac
      PwellP1=-1/fac
      PwellP2=-1/fac
      imcd=0
c
c ---------------------------------------------------------
c               Check Avail array
      call chekava(19, maxsta, numsta, avail)
c
c _________________________________________________________
c               Step 3; Set Destination (ip) a plan
      ip =iopdes(1,L2)
      if(ip.eq.0) then
        write(Nlog,300) cStaId1, ip
        goto 9999
      endif
      
      iscd=ipsta(ip)
      ndns=ndnnod(iscd)
      
      cDesID=Pid(ip)
      cRivId=cStaId(iscd)
c _________________________________________________________
c
c		Step 4; Exit if the plan is off (idivsww = 0)
      if(pon(ip).eq.0) then
        iwhy=1
        cwhy='Well Augmentation Plan is off'
        goto 250
      endif  
c
c
c _________________________________________________________
c
c              Step 5. Check available flow
c 		       Note Check entire array, not just downstream               
      call dnmfsow(maxsta, avail, numsta, imcd0)
      avail0=avail(imcd0)
      
      IF(AVAIL0.le.(-1.*small)) then
        WRITE(99,310) cstaid1, iyrmo(mon),xmonam(mon), idy, imcd0,avail0
        goto 9999
      endif
      
c      
c _________________________________________________________
c
c		Check available flow downstream of the source
      call dnmfso(maxsta, avail, idncod, iscd, ndns, imcd)
      cMinId=cstaid(imcd)
      avail1=avail(imcd)
      IF(AVAIL1.le.small) then
        iwhy=2
        cwhy='Available flow (avail1) = zero'
        goto 250
      endif
c      
c _________________________________________________________
c
c		Step 6; Determine if past depletions or T&C requirements
c		         are in priority
c			 Note do not adjust the avail array because
c			 these impacts were taken out of the system
c      at the beginning of the time step
c			 Note do not adjust pdemT or pobl since
c			 these are subtractions from the current demand
      if(avail1.gt.small) then
        pdemX=amin1(pdem(ip), avail1)
        pdemX=amax1(pdemX,0.0)
c
c ---------------------------------------------------------
c		Exit if demand is null        
        IF(pdemX.le.small) then
          iwhy=3
          cwhy='Plan demand (pdemX) = zero'
          goto 250
        endif
c
c ---------------------------------------------------------
c
c		a. Adjust current demand Pdem() by the 
c                  amount diverted in priority        
        pdem1=pdem(ip)
        pdem(ip)=pdem(ip) - pdemX
        pdem2=pdem(ip)
c
c ---------------------------------------------------------
c		b. Store past depletions in priority under divo
c		   the operating rule diversion
        pwellP1=divo(l2)
        divo(l2)=divo(l2)+pdemX
        pwellP2=divo(l2)
        
      endif  
c
c ---------------------------------------------------------
c
c		c. Detailed Well Augmentation output      
 250    continue
c 
c     if(iout.eq.99 .and. demX.lt.small) iout=98
      if((iout.eq.2 .and. iw.eq.ioutiw) .or. iout.ge.99) then      
          ncallX=ncallX+1        
          if(ncallX.eq.1)then
            write(nlog,270) corid(l2),cdestyp,ccarry,cpuse,
     1        cRelTyp, Creplace
          endif

        write(nlog,280) '  WelPrity-Past  ',
     1    iyrmo(mon),xmonam(mon), idy, cDesId, cRivId, cMinId, 
     1    iwx, l2, ip, imcd, 
     1    avail1*fac,  pdem1*fac,  pdemX*fac, pdem2*fac, 
     1    pwellP1*fac, pdemX*fac, pwellP2*fac, pDemX*fac,
     1    iwhy, cwhy     
      endif  
c       
     
  280 FORMAT(a12, 1x, i5,1x,a4, i5, 1x,a12, 1x,a12, 1x,a12,
     1 4i5, 8F8.1, i5, 1x, a48)
      
c
c _________________________________________________________
c
c                Step 27; Return
      RETURN
c
c _________________________________________________________
c
c                Formats
c

  270   format(/, 
     1  '  WelPrity; Right ID = ', a12,
     1  ' Destination Type = ', a12,
     1  ' Carrier (Y/N) = ',a3, ' Reuse Plan (Y/N) = ', a3,
     1  ' Release Type = ', a12,' Called by Replace = ', a3,/  
     1  '  WelPrity     iyr  mon  idy',
     1  ' Plan_ID      Riv_ID       Min_ID      ',
     1  '  iwx   l2   ip imcd',
     1  '  Avail1   Pdem1   PdemX   Pdem2',
     1  ' PwellP1   PdemX  PwellP2  PdemX',
     1  ' iwhy cwhy',/
     1  ' ____________ ____ ____ ____',
     1  ' ____________ ____________ ____________',
     1  4(' ____'), 8(' _______'),' ____', 1x, 48('_'))
     

  300   FORMAT(/,
     1 '  WelPrity; Problem with plan ID (ip)',/
     1 '            Opr Id = ', a12, ' ip = ', i5)
     
  310   FORMAT(/,'  WelPrity; Problem Avail1 is negative',/
     1   ' Opr Id = ', a12, i5,1x,a4, i5, ' imcd = ', i5,
     1   ' Avail1 = ', 20f8.2)
c
c _________________________________________________________
c
c              Error warnings
c
 9999 write(6,340) 
      write(99,350) 
      call flush(6)
 340  format('    Stopped in WelPrity',/,
     1       '    See the *.log file')
 350  format('    Stopped in WelPrity')
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop 
      END

