c
C
      SUBROUTINE OopBook2(IW,L2,divactx,ncallx)        
c
c		      
c
c _________________________________________________________
c	Program Description
c
c       OopBook2; Type 8.
c               It simulates an Out of Priority Bookover
c               It is the same as OopBook but it was adjusted to 
c		add the second of the following two applications
c		1. When the destination (iopdes(1,_)) >0
c		   The destination is a reservoir account
c                  Source 1 (iopsou(1,_) is an OOP reservoir account
c		   The plan (ireuse() is the OOP plan
c		   The junior OOP (junior) water right is in intern(l2,1)
c		2. When the destination (iopdes(1,_)) < 0
c		   The destination is an OOP plan
c                  Source 1 (iosou(1,_) may be blank or it may
c                  be an 'associated' OOP reservoir account
c		   The plan (ireuse() is the OOP plan
c		   Note for this case the -1*iopdes(1,_) = ireuse()
c		   Any associated reservoir storage is in 
c                  reservoir intern(l2,1) account intern(l2,2), 
c                  reservoir intern(l2,3) account intern(l2,4), etc)
c
c	Note on July 19, 2006 OopBook2 fully replaced the functionality 
c       of the original type 8 rule controlled by OopBook (e.g. it simply
c       has more capability).
c
c       The type 8 operating rule pays back any storage or diversions
c       that occurred out-of-priority under a type 38 (OopDiv) 
c       operating rule that were tied to the same OOP Plan ID.
c
c       When properly applied, water stored or diverted out-of-priority
c       is kept in a OOP Plan. 

c	If the destination is a OOP plan and the volume of water stored
c       in the OOP Plan exceeds the remaining capacity of the subordinated
c       (senior) reservoir, water is paid back to the OOP Plan.
c
c	If the destination is a Reservoir and the volume of water stored
c       in the OOP Plan exceeds the remaining capacity of the subordinated
c       (senior) reservoir, water is booked to another reservoir account
c
c	By having a bookover to both a resrvoir and plan depend on the amount
c	stored in an OOP Plan, the user has the flexability to control
c	the priority of when water stored or diverted at several locations
c       is booked over.
c
c       If the destination is a reservoir and the subordinated (senior)
c       reservoir does not fill then a type 2 operating right is
c       typically used to pay back the OOP storage by transfering
c       water from the out of priority reservoir account to the senior reservoir.
c
c       If the destination is a plan and the subordinated (senior)
c       reservoir does not fill then a type XX operating right is
c       typically used to pay back the OOP Plan by transfering
c       water from another reservoir to the senior reservoir.
c
c _________________________________________________________
c       Update History
c
c rrb 2006/07/17; Copy OopBook and revise accordingly
c
c _________________________________________________________
c       Documentation
c
c	iout =  0 no detailed printout
c       	1 yes detailed printout
c
c	resavl  	available in reservoir
c	resalo  	available in account
c		
c ---------------------------------------------------------
c	nd 		iopdes(1,
c         	 	If>0 the destination is a reservoir 
c		 	If<0 the destination is a plan
c	idow            The destination reservoir account
c	ndwr		The destination (junior) water right
c
c ---------------------------------------------------------
c	nr		iopsou(1,
c		 	If>0 the OOP reservoir
c		 	If=0 there is no OOP reservoir account
c       irow            the OOP reservoir account
c		
c ---------------------------------------------------------
c	nrwr		iopsou(3,
c		 	The subordinated (senior) reservoir right
c	nSenior		The subordinated (senior) reservoir
c	SeniorA		Avaliable storage in the subordinated (senior) 
c                       decree
c
c	divOpr		Amount current owed by an associated
c		        OOP diversion or reservoir 
c			Note store in acft since it cululates
c			from one month to the next
c	
c ---------------------------------------------------------
c	ip		ireuse(
c			The OOP plan pointer
c		        Note if the destination is a plan then
c			ireuse( is the same as iopdes(1, 
c ---------------------------------------------------------
c
c
c       qres(29,nr)     Amount diverted within the same reservoir
c		        used in Outbal2 for for basin balance
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
      character cwhy*48, cdestyp*12, ccarry*3, cpuse*3, cstaid1*12,
     1          rec12*12,cSouTyp*12, cresid1*12      
c
c
c _________________________________________________________
c
c               Step 1 Initilize
c
c		iout=1 details
c		iout=2 summary

      iout=0
      ioutiw=0
      
      if(ichk.eq.108) iout=2
      if(corid(l2).eq. ccall) ioutiw=iw
      
      if(iout.eq.1) write(nlog,*) '  OopBook2.for;'      
      
      if(iout.ge.1 .and. iw.eq.ioutiw) then      
        if(NcallX.eq.0) then
          write(nlog,270) corid(l2),cdestyp, cSouTyp, ccarry, cpuse
          ncallX=ncallX+1
        endif  
      endif  
      
c
c rrb 98/03/03; Daily capability
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif
      
      divaf   = 0.0                   
      divactx = -1./fac
      caprem  = -1/fac
      relaf   = -1.0
      tarcon  = -1.0
      tranlim = -1.0
      resavl  = -1.0
      resalo  = -1.0
      seniorA = -1.0
      psuply1 = -1.0
      psuply2 = -1.0
      divOpr1 = -1.0
      
      cdestyp='NA'
      cstaid1='NA'
      cSouTyp='Reservoir'
      ccarry='No'      
      cpuse='No '      
c
      SMALL =0.001
      BIG=99999.
      
      cwhy='NA'
      iwhy=0
c
c _________________________________________________________
c
c               Step 2; Allow monthly on/off switch
      if(imonsw(l2,mon).eq.0) then
        iwhy=1
        cwhy='Month Off'      
        goto 200
      endif  
c
c ---------------------------------------------------------
c		For a daily model set demand for beginning of season
      if(iday.eq.1 .and. imonsw(l2,mon).gt.0) then
        if (idy.lt.imonsw(l2,mon)) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 200
        endif  
      endif  
c
c ---------------------------------------------------------
c		For a daily model set demand for end of season
      if(iday.eq.1 .and. imonsw(l2,mon).lt.0) then
        if (idy.gt.iabs(imonsw(l2,mon))) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 200
        endif  
      endif  
      
c
c _________________________________________________________
c
c               Step 3; Set source data

      NR  =IOPSOU(1,L2)
      cstaid1=cresid(nr)
c
c ---------------------------------------------------------      
c rrb 2006/07/16; Allow a Plan destination
c		a. Source 1 is a reservoir
      if(nr.gt.0) then
        IF(IRESSW(NR).EQ.0) then
          iwhy=2
          cwhy='Source Reservoir is Off'      
          Goto 200
        endif  
                
        IROW=NOWNER(NR)+IOPSOU(2,L2)-1
        ISCD=IRSSTA(NR)
        ndnr = ndnnod(iscd)        
        curown1=curown(irow)               
c
c ---------------------------------------------------------      
c               a. Roundoff check Source Reservoir
        in1=0
        isub=8
        call chekres(nlog,maxres, in1, isub, iyr, mon, nr,nowner,
     1               curown,cursto,cresid)
      endif  
c
c ---------------------------------------------------------      
c
c rrb 2006/07/16; Allow a Plan destination
c		b. Source 1 is blank (not a reservoir)
      if(nr.eq.0) then
        irow=0
        curown1=-1.0
      endif  
c
c _________________________________________________________
c
c               Step 4; Set subordinated (senior) right data        
        
      nrwr=iopsou(3,l2)
      nSenior=iresco(1,nrwr)
c
c rrb 2006/07/25; Revise limit to be based on senior decree (dcrres)
c		  not available storage      
cr    SeniorA=volmax(nSenior) - CURSTO(nSenior)
      SeniorA=dcrres(nrwr) - CURSTO(nSenior)
      SeniorA=amax1(SeniorA, 0.0)
c
c		Set paper fill to remaining decree
      ritpap1=ritrem(nrwr)
      ritrem1=ritrem(nrwr)     
c      
c ________________________________________________________
c               Step 5; Set Plan Pointer
c		iP  = Plan pointer
      iP=ireuse(l2)
      if(ip.le.0) goto 510
c
c rrb 2006/10/04; For an OOP plan all data is in Psto2      
cr    psuply1=psuply(iP)*fac
      psuply1=psto2(ip)
c
c _________________________________________________________
c
c               Step 6; Set desitnation data
c
      ND  =IOPDES(1,L2)
c
c rrb 2006/07/16; Allow a Plan destination
c
c ---------------------------------------------------------      
c		a. Destination is a reservoir
      if(nd.gt.0) then
        ndP=0
        cdestyp='Reservoir'
      
        IF(IRESSW(ND).EQ.0) then
          iwhy=3
          cwhy='Destination Resrvoir is Off'      
          Goto 200
        endif  
      
        IDCD=IRSSTA(ND)
cr      IDOW=NOWNER(ND)+IOPDES(2,L2)-1
c
c ---------------------------------------------------------      
c               a. Roundoff check Destinatino Reservoir
        in1=0
        isub=8
        call chekres(nlog,maxres, in1, isub, iyr, mon, nr,nowner,
     1               curown,cursto,cresid)

c
c ---------------------------------------------------------
c rrb 2006/09/25; Allow multiple accounts - Initilize
        nro=1
        if(iopdes(2,l2).lt.0) then
          nro=-iopdes(2,l2)
          idow=nowner(nd)
        endif

        if(iopdes(2,l2).gt.0) then
          nro=1
          idow=nowner(nd)+iopdes(2,l2)-1
        endif
        
        
      endif  
c
c ---------------------------------------------------------      
c rrb 2006/07/16; Allow a Plan destination
c		b. Destination is a plan
      if(nd.lt.0) then
        ndP=-1*iopdes(1,l2)
        if(ndP.ne.iP) then
          write(nlog,*) '  OopBook2; Problem destination plan ID does ',
     1      'not equal the plan ID', ndp, ip
          goto 9999
        endif 
        
        cdestyp='Plan'
        IDOW=0
        IDCD=ipsta(iP)
     
        IF(ifix(pon(iP)).EQ.0) then
          iwhy=4
          cwhy='Destination Plan is Off'      
         Goto 200
        endif  
        
      endif  
      
c
c _________________________________________________________
c
c               Step 7; If the destination is a reservoir
c                       Set OOP (Junior) right
c
c rrb 2006/10/03; Correction
cr    if(nr.gt.0) then
      if(nd.gt.0) then
        ndwr=intern(l2,1)
        ritremD=ritrem(ndwr)
      else
        ndwr=-1
        ritremD=-1
      endif
      
c
c _________________________________________________________
c
c               Step 8; Detailed Output
c
      if(iout.ge.1 .and. iw.eq.ioutiw) then
c        if(NcallX.eq.0) then
c          write(nlog,270) corid(l2),cdestyp, cSouTyp, ccarry, cpuse
c          ncallX=ncallX+1
c        endif  
        
        WRITE(nlog,280) 'In  ',iyrmo(mon),xmonam(mon),idy, cstaid1,
     1    iwx, IW,L2, nr, NR,  irow,  nd, idow, nrwr, ndwr, ip, 
     1    curown1, SeniorA, ritrem1, divOpr1,
     1    ritremD, psuply1, tranlim, tarcon,
     1    resavl,  resalo,  caprem,  relaf, divactx*fac, divaf,
     1    iwhy, cwhy  
  280 format(
     1 4x,a4,i5,2x,a3, 1x,i4, a12,1x, 11i5, 14f10.0, i5, 1x,a48)
        
      endif
c
c _________________________________________________________
c
c               Step 9; Calculate transfer limit based
c		     	 on the amount in the Plan (total for all OOP
c			 activities associated with this reservoir)
c rrb 2006/10/04; For an OOP plan all data is in Psto2      
cr    psuply1=psuply(iP)*fac
      psuply1=psto2(ip)
      
      tranlim=amax1(psuply1-SeniorA, 0.0)
      trancfs=tranlim/fac        
c
      IF(trancfs.LE.small) then
        iwhy=6
        cwhy='OOP plan is less than the available Seniors storage'
        Goto 200
      endif  
c
c _________________________________________________________
c
c               Step 10; Set Source 1 (reservoir) limit if any
c rrb 2006/07/16; Allow a Plan destination
c
c ---------------------------------------------------------
c
c		a. Source 1 is a reservoir
c		   NOte irow is the source (OOP) reservoir account
c			idow is the destination reservoir account
c			nd is the destination reservoir
      if(nr.gt.0) then
        RESAVL=CUROWN(IROW)
        tarcon=tarmax(nd)-cursto(nd)       
cr      RESALO=OWNMAX(IDOW)-CUROWN(IDOW)
c
c ---------------------------------------------------------
c rrb 2006/09/25; Allow multiple accounts - Demand
        cursa=0.0
        do n=1, nro
          n1=idow+n-1
          cursa=cursa+(ownmax(n1)-curown(n1))
        end do        
        resalo=cursa
      endif
c
c ---------------------------------------------------------
c
c		b. Source 1 is a reservoir or blank (a Plan)
c		   Limit to amount owed by the associaetd OOP diversion
c		   (stored in intern(l2,2) as a negative
c rrb 2006/07/26; Correction
      if(nr.le.0) then
        resavl=BIG
        resalo=BIG
        tarcon=BIG
      endif  
c
c ---------------------------------------------------------
c		c. Limit to amount diverted by a type 38 (OopDiv.f)
c		   operating rule
c rrb 2006/10/16; Remove divopr limit
cr    nX=intern(l2,2)
cr    if(nX.gt.0) then
cr      divOpr1=divOpr(nx)
cr      resavl=amin1(resavl, psuply1, divOpr(nX))
cr      resavl=amax1(resavl, 0.0)        
c
c       write(nlog,260) ' OopBook2;  ',nX, divOpr(nX)
cr    
cr      IF(resavl.LE.small) then
cr        iwhy=7
cr        cwhy='OOP Demand less OOP Reservoir storage is zero'
cr        Goto 200
cr      endif  
cr    endif  
c      
c ---------------------------------------------------------
c
c		d. Set source limits
      
      RESALO=AMAX1(RESALO,0.)
      RALCFS=RESALO/fac      
      RAVCFS=RESAVL/fac
      
      IF(RALcfs.LE.small) then
        iwhy=8
        cwhy='Destination capacity is zero'
        Goto 200
      endif  

      IF(RAVcfs.LE.small) then
        iwhy=9
        cwhy='Source capacity is zero'
        Goto 200
      endif  
c
c rrb 2006/05/23; Clean up
      caprem=amin1(ralcfs, ravcfs)    
      caprem=amax1(caprem, 0.0)
c
c _________________________________________________________
c
c               Step 11; Calculate Book Over
c
c rrb 2006/05/22; Test
      divact=amin1(caprem,trancfs)
      divact=amax1(0.0,divact)
      DIVAF=DIVACT*fac
c
      IF(DIVACT.LE.small) then
        iwhy=10
        cwhy='Diversion = 0'      
        Goto 200
      endif  
c
c _________________________________________________________
c
c               Step 12; UPDATE VARABLES
c rrb 2006/07/16; Allow a Plan destination
c
c ---------------------------------------------------------
c		a. Adjust source 1, a reservoir
      if(nr.gt.0) then
        CURSTO(NR  )=CURSTO(NR  )-DIVAF
        CUROWN(IROW)=CUROWN(IROW)-DIVAF
        QRES (22,NR)=QRES (22,NR)+DIVAF
        accr(22,irow) = accr(22,irow)+divaf        
      endif  
c      
c ---------------------------------------------------------
c		b. Adjust the OOP (junior) right
c rrb 2006/07/25; Correction. Adjust when stored OOP to 
c		  insure the OOP is .le. the junior decree
c
c rrb 2006/07/26; Adjust when booked over, not when diverted
      if(nd.gt.0) then
        ritrem(ndwr) = amax1(ritrem(ndwr) - divaf, 0.0)      
      endif  
c
c ---------------------------------------------------------
c		c. Adjust the destination reservoir  
      if(nd.gt.0) then
        CURSTO(ND  )=CURSTO(ND  )+DIVAF
cr      CUROWN(IDOW)=CUROWN(IDOW)+DIVAF        
        QRES  (4,ND)=QRES  (4,ND)+DIVAF
cr      accr(4,idow)  = accr(4,idow)+divaf
c
c ---------------------------------------------------------
c rrb 2006/09/25; Allow multiple accounts - Allocate
c		  Note:
c		   iResT1=0 distributes based on ownership ratio
c		   nrown1=number of accounts in this reservoir
c		   iown = first account associated with this reservoir  
c		   icx  = subrouine calling accou.for       
c		   ia   = account to adjust
      
        nrX=nd
        iResT1=0
        nrown1=nro
        iownX=idow
        icx=108
        ia=4
        cresid1=cresid(nrX)
c        
        call accou(maxacc, maxown, nrX, ownmon, curown, accr, ia, 
     1    ownmax, iownX, nrown1, cursa, divaf, iResT1, icx, cresid1)
        
      endif  
c
c ---------------------------------------------------------
c rrb 2006/10/03; 
c		Store amount diverted within the same reservoir
c		for basin balance
      if(nd.eq.nr) then
        qres291=qres(29,nd)
        qres(29,nd)=qres(29,nd) + divaf      
        if(iout.eq.1) then
          write(nlog,*) ' OopBook2; cresid, iyrmo(mon),xmonam(mon),',
     1     'nd, nr, qres1, divaf, qres ' 
     
          write(nlog,*) ' OopBook2; ',
     1      cresid(nr), iyrmo(mon),xmonam(mon), nd, nr, 
     1      qres291, divaf, qres(29,nr)
        endif
      endif
      
c
c ---------------------------------------------------------
c
c		d. Adjust the destination Plan
      if(iP.gt.0) then
c      
c rrb 2006/10/04; For an OOP plan all data is in Psto2      
cr      psuply(iP)=psuply(iP)-divact
cr      psuplyT(iP)=psuplyT(iP)-divact
        
        psto2(iP)=amax1(0.0, psto2(iP)-divaf)
        ipsta1=ipsta(iP)
c        
c rrb 2006/10/04; For an OOP plan all data is in Psto2      
cr      psuply2=psuply(ip)*fac
        psuply2=psto2(ip)
      endif  
c
c ---------------------------------------------------------
c
c		f. Adjust the operating rule diversion
  200 divo(l2)=divo(l2)+(divaf/fac)
c  
c rrb 2006/10/16; Remove divopr limit tied to the amount diverted
c                 by a type 38 (OopDiv.f) operating rule
cr    if(nX.gt.0) then
cr      divOpr(nX)=divOpr(nX) - divaf
cr      divOpr2=divOpr(nX)
cr    endif  
c
      divactx=divaf/fac
      if(divactx.lt.small) divactx=0.0  
c
c _________________________________________________________
c
c               Step 13; Detailed Output
c
      if(iout.ge.1 .and. iw.eq.ioutiw) then
        WRITE(nlog,280) 'Out ',iyrmo(mon),xmonam(mon),idy,cstaid1,
     1    iwx, IW, L2, nr, NR,  irow,  nd, idow, nrwr, ndwr, ip, 
     1    curown1,  SeniorA, ritrem1, DivOpr1,
     1    ritremD,  psuply1, tranlim, tarcon,
     1    resavl,   resalo,  caprem*fac, relaf, divactX*fac, divaf,
     1    iwhy, cwhy
      endif
c
c _________________________________________________________
c
c               Step 14; Roundoff check, 
c
c ---------------------------------------------------------
c		a. Source reservoir
      if(nr.gt.0) then
        in1=1
        isub=8
        call chekres(nlog,maxres, in1, isub, iyr, mon, nr,nowner,
     1               curown,cursto,cresid)
        endif
c
c ---------------------------------------------------------
c		a. Destination reservoir
      if(Nd.gt.0) then
        in1=1
        isub=8
        call chekres(nlog,maxres, in1, isub, iyr, mon, nr,nowner,
     1               curown,cursto,cresid)
        endif
     
c
c _________________________________________________________
c
c               Step 15; Return
c
      return
c
c _________________________________________________________
c
c               Formats
c
 260    format(a12, 1x, i5, f8.0)         
 270  format(/, 72('_'),/
     1 '  OopBook2; Out-of-Priority Storage (Type 8)',/
     1 '          Operation Right ID = ', a12,
     1 ' Destination Type = ', a12, ' Source Type = ', a12,
     1 ' Carrier (Y/N) = ',a3, ' Reuse Plan (Y/N) = ', a3//
     1 '                                      ',
     1         '                                        ',
     1 '       OOP    Senior    Senior       Opr    Junior  OOP Plan',
     1 '  Transfer    Target Available Available Available',/                              ',/
     1 '                                      ',
     1         '                                        ',
     1 '   Storage Available     Right     Limit     Limit    Demand',
     1 '     Limit     Limit    in_Res   in_Acct   Minimum     Spill',
     1 '   DIVACTx  BookOver',/
     1 '          iyr  mon  day Source ID     iter   iw   l2',
     1         '   nr   nr irow   nd idow nrwr ndwr   ip',
     1 '    curown   SeniorA   ritremS   DivOpr1   ritremD   psuply1',
     1 '   tranlim    tarcon    resavl    resalo    caprem     RELAF',
     1 '   DIVACTx     DIVAF',
     1 ' iwhy Comment',/
     1 8x, 3(' ____'), ' ____________ ',
     1 14(' _________'), ' ____ ',24('_'))
c
c _________________________________________________________
c               Error warnings
c
 510  write(nlog,512) 
 512  format(
     1 '  OopBook2; Problem a Type 8 Operating Rule must be tied to',/
     1 '            an Out-of-Priority Plan',/
     1 '            Recommend you revise the operating rule ',
     1             '(*.opr) file')
      goto 9999

 9999 write(6,1050) 
      write(99,1051) 
    
 1050 format('    Stopped in OopBook2',/,
     1       '    See the *.log file')
 1051 format('    Stopped in OopBook')
      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)
c
c _________________________________________________________
c		Stop

      stop 
      END
