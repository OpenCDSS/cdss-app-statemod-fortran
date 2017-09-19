c
c *********************************************************
c
      subroutine powres2(iw,l2,divact,ncallx)
c
c
c _________________________________________________________
c	Program Description
c
c       Powres2; It simulates a reservoir release to an instream
c                flow reach. Similar to Powres.
c
c _________________________________________________________
c       Update History
c
c rrb 1996/03/13; Initilize divact, send returns to bottom & set divo
c rrb 1996/08/07; Instream Flow reach included by reoperating instream
c                 right to insure whole reach is available to releases
c rrb 2002/10/25; Allow monthly on/off switch
c rrb 2005/09/21; Adjusted to handle reach data better
c
c _________________________________________________________
c       Documentation
c
c       iw                      water right loop counter
c       l2                      instream right counter
c       nf=iifrco(l2)           instrem structure counter
c       ifcd=ifrsta(nf)         river location of instream structure
c       dcrifr(l2)              water right from riginp.f
c
c       divi(l2)                amount diverted by this right 
c       divir(i)                amount diverted by this right 
c                                 at reach node i
c
c       flowrq(nf)              demand remaining this time step
c       florqr(i)               demand remaining at reach node i
c
c       qdiv(14,ifcd)           instream diversion at river node ifcd
c       qdivr(i)                instream diversion at reach node i
c
c       ndns = ndnifs(nf)       number of downstream nodes in the reach
c       ndnifb(nf)              beginning counter for storing reach info
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
      character cwhy*48, cdestyp*12, ccarry*3, cpuse*3     
c
c
c _________________________________________________________
c
c		Step 1; Initilize
      iw = iw
      iout=0
      ioutiw=0
      
      if(ichk.eq.101) iout=1
      if(corid(l2).eq. ccall) ioutiw=iw
c     write(nlog,*) ' PowRes2; ichk, ioutiw, iw',ichk,ioutiw,iw    
      
c
c rrb 98/03/03; Daily capability
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif
      
      iwhy=0      
      cwhy='N/A'
      cdestyp='N/A'
      ccarry='No'
      cpuse='No'

      ipuse=-1
      imcdx=-1
      divreqx2=-1./fac
      divalos=-1./fac
      divactX=-1./fac
      availX=-1./fac 
      divact = 0.0
      
      mon2 = imonsw(l2,mon)  
      small=0.001
c
c _________________________________________________________
c
c		Step 2; Check Monthly on/off switch
      if(imonsw(l2,mon).eq.0) then
        iwhy=1
        cwhy='Monthly switch is off'
        goto 130
      endif
c
c ---------------------------------------------------------
c		For a daily model set demand for beginning of season
      if(iday.eq.1 .and. imonsw(l2,mon).gt.0) then
        if (idy.lt.imonsw(l2,mon)) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 130
        endif  
      endif  
c
c ---------------------------------------------------------
c		For a daily model set demand for end of season
      if(iday.eq.1 .and. imonsw(l2,mon).lt.0) then
        if (idy.gt.iabs(imonsw(l2,mon))) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 130
        endif  
      endif  
      
c
c _________________________________________________________
c
c		Step 3; Set source (reservoir) data
c
      NR  =IOPSOU(1,L2)
      IF(IRESSW(NR).EQ.0) then
        iwhy=2
        cwhy='Reservoir is off'
        Goto 130
      endif  
      IOWN=NOWNER(NR)+IOPSOU(2,L2)-1
      ISCD=IRSSTA(NR)
      NDNS=NDNNOD(ISCD)
c
c ---------------------------------------------------------
c rrb 2006/08/19; Check reservoir data going in. Note:
c		in1=0 into a subroutine      
c		isub1=subroutine calling
      in1=0
      isub1=24
      call chekres(nlog, maxres, in1, isub1, iyr, mon, nr,nowner,
     1                     curown,cursto,cresid)     
c
c _________________________________________________________
c
c		Step 4; Set destination (isf) data
c
      NF  =IOPDES(1,L2)
      IFCD=IFRSTA(NF)
c
c rrb 2010/12/26; Exit if the ISF is off
      if (IFRRSW(nf).eq.0) then
        iwhy=3
        cwhy='Instream Destination is off'        
        goto 130
      endif 
c    
      ib=ndnifb(nf)
      ie=ndnifb(nf) + ndnifs(nf) - 1      
      
      IF(FLOWRQ(NF).LE.small) then
        iwhy=4
        cwhy='Instream Demand is zero'
        Goto 130
      endif  
c
c _________________________________________________________
c
c		Step 5; Set demand data
c
      DIVALO=FLOWRQ(NF)
      divreqx2=divalo
c
c _________________________________________________________
c
c		Step 6; Set supply from reservoir
c
      RESAVL=AMIN1(CURSTO(NR)-VOLMIN(NR),CUROWN(IOWN))
      RESAVL=AMAX1(0.,RESAVL)
      
      RAVCFS=RESAVL/fac
      divaloS=ravcfs
      IF(RAVCFS.LE.small) then
        iwhy=5
        cwhy='Reservoir supply is zero'
        Goto 130
      endif  
c
c _________________________________________________________
c
c		Step 7; Set maximum release
c
      FLOAVL=AMAX1(FLOMAX(NR)-RIVER(IFCD),0.)
      IF(FLOAVL.LE.small) then
        iwhy=6
        cwhy='Available River Flow is zero'
        Goto 130
      endif  
c
c _________________________________________________________
c
c		Step 8; Calculate release
c
cr 2005/09/21; Simplify logic
cr    IF(FLOAVL.LE.RAVCFS) GO TO 100
cr    DIVACT=AMIN1(RAVCFS,DIVALO)
cr    GO TO 110
cr100 DIVACT=AMIN1(FLOAVL,DIVALO)
cr110 TEMP=-DIVACT
       
      if(FLOAVL.LE.RAVCFS) then
        divact=AMIN1(FLOAVL,DIVALO)
      else
        divact=AMIN1(RAVCFS,DIVALO)
      endif  
      divactX=divact
      temp=-DIVACT
c
c _________________________________________________________
c
c		Step 9; Adjust river and avail from Res down. Note:
c			Avail at RES avail(iscd) is not adjusted
c			Avail at ISF avail(ifcd) is not adjusted
      availr=avail(iscd)
      CALL TAKOUT(maxsta, AVAIL ,RIVER ,AVINP ,QTRIBU,IDNCOD,
     1            TEMP  , NDNS,  ISCD  )
      avail(iscd)=availr
c
c
c _________________________________________________________
c
c		Step 10; Adjust avail in the isf reach
c     AVAIL (IFCD)=AVAIL (IFCD)-DIVACT
      issr=ifrsta(nf)
      do i=ib,ie             
cr        write(nlog,*) ' Powres2; isf reach ', nf, issr, avail(issr),
cr     1    avail(issr)-divact
        avail(issr)=avail(issr)-divact
        issr=idncod(issr)      
      end do  
c
c _________________________________________________________
c
c		Step 11; Adjust running variables
  120 RELAF=DIVACT*fac
c
c		a. Reservoir
      CURSTO(NR  )=CURSTO(NR  )-RELAF
      PROJTF(NR  )=PROJTF(NR  )+DIVACT
      CUROWN(IOWN)=CUROWN(IOWN)-RELAF
c     IF(IOWNA.NE.IOWN) QMAINS(2,IOWNA)=QMAINS(2,IOWNA)-RELAF
c
c		b. Demand
      FLOWRQ(NF)=FLOWRQ(NF)-DIVACT
c
c		c. Stream
      QRES(12,NR)=QRES(12,NR)+RELAF
      QDIV(15,IFCD)=QDIV(15,IFCD)+DIVACT
c
c		d. Accounts
      accr(12,iown) = accr(12,iown)+relaf
c
c		e. Operating Rule
 130  divo(l2)=divo(l2)+divact
c
c		f. Instream reach data
      do i=ib,ie
        florqr(i)=florqr(i)-divact
        qdivr1=qdivr(i)
        qdivr(i)=qdivr(i)+divact
c       if(i.eq.51 .or. i.eq.52)
c    1    write(nlog,*) ' Powres2;', i, qdivr1*fac, qdivr(i)*fac
      end do  
        
c rrb 08/07/96; Instream Flow reach 
c               Adjust the instream reach demand (florqr) 
c               at most upstream node (ndnifb(nf))
c
c               reoperat the instream flow rights associated with
c               the destination instream structure to insure the 
c               whole instream reach can use reservoir releases
c               Note; ndnifs(nf) is the number of subreaches for this 
c                                instream flow
c                     numfrr is the number of instream rights
c                     iifrco is the instream id for right kk
c                     iifrsw is the on/off switch for right kk
c                     nfr=ndnifb(nf) is the first subreach
cr    nfr=ndnifb(nf)
cr   florqr(nfr)=amax1(0.0, florqr(nfr)-divact)
c
c rrb 08/16/96 Instream Flow reach
cr    qdivr(nfr)=amin1(qdivr(nfr)+divact, flowr(mon,nf))
cr      if(ndnifs(nf).gt.1) then
cr        do 140 kk=1,numfrr
cr          if(nf.eq.iifrco(kk).and. iifrsw(kk).ne.0) then
cr            CALL IFRRIG2(IW,kk,ncallx)
cr          endif  
cr 140    continue
cr      endif     
c      
c _________________________________________________________
c
c               Step 12.  Detalied output
c

      if(iout.eq.1 .and. iw.eq.ioutiw) then
c     if(iout.eq.1) then
        ncallX=ncallX+1
        if(ncallX.eq.1)then
          write(nlog,270) corid(l2),cdestyp,ccarry,cpuse
        else
c          write(nlog,*) ' '
        endif  
      
        write(nlog,280) 'PowRes2     ',
     1    iyrmo(mon),xmonam(mon), idy,
     1    cstaid(iscd),iwx, iw,nwrord(1,iw),l2,l2,
     1    Nf,ipuse,imcdX, availX*fac, DIVREQx2*fac, 
     1    divaloS*fac, -1.0, divactx*fac, iwhy, cwhy
      endif
c
c ---------------------------------------------------------
c rrb 2006/08/19; Check reservoir data going in. Note:
c		in1=1 out of subroutine      
c		isub1=subroutine calling
      in1=1
      isub1=24
      call chekres(nlog, maxres, in1, isub1, iyr, mon, nr,nowner,
     1                     curown,cursto,cresid)     
      
c      
c _________________________________________________________
c
c               Formats
    
  270   format(/, 
     1  ' PowRes2 (Type 1); Operation Right ID = ', a12,
     1  ' Destination Type = ', a12,
     1  ' Carrier (Y/N) = ',a3, ' Reuse Plan (Y/N) = ', a3/
     1  ' PowRes2      iyr  mon  day ID          ',
     1  '    Iter      Iw  nwrord      l2      l2      nF   ipUse', 
     1  '   imcdX  availX demandX divaloS psuplyT divactX',
     1  '    iwhy Comment',/
     1  ' ___________ ____ ____ ____ ____________', 
     1  ' _______ _______ _______ _______ _______ _______ _______', 
     1  ' _______ _______ _______ _______ _______ _______',
     1  ' _______ __________________________')     
  280   FORMAT(a12, i5,1x,a4, i5, 1x, a12,8i8,5F8.1,i8,1x, a48)
c      
c _________________________________________________________
c
c		Return
      RETURN
      END



