c
c _________________________________________________________
c	Update History
c

c
c _________________________________________________________
c	Documentation
c

c
c _________________________________________________________
c	Dimensions
cC     Last change:  RRB  22 Oct 2002    4:12 pm
c
c
      subroutine DivImpR2(iw,l2,ishort,divact,ncallX)
c
c _________________________________________________________
c	Program Description
c
c
c       DivImpR2; the NEW type 35
c       It handles diversions from a diversion (import) to a
c         diversion, reservoir, ACCOUNTING PLAN, or carrier
c         with reuse
c       Called by Execut
c _________________________________________________________
c
c 	Update history
c
c	05/03/31	Copied Divres
c			Removed ability to relase for depletion	only
c                       Removed ability to be called by replacement res
c			Removed stuff related to a transfer limit 
c			Removed refrences to transmtn (IRTURN(IUSE).EQ.4)
c			Added Reservoir Reuse Plan as a source 2 option
c       2014/07/31 jhb  copied DivImpR
c                       first step - add acct plan (type 11) as destination
c                       second step - remove reuse (type 5,6 plan) when dest = plan
c                       third (maybe) - make dest=plan and no reuse the only options
c                       
c
c  ________________________________________________________
c	Documentation
c
c           divalo = demand
c           relalo = available supply in reservoir (acft)
c           alocfs = available supply in reservoir (cfs)
c           divact = actual amount diverted
c           relace = actual amount released from the reservoir
c
c           icx   = subroutine call # (32)
c
c           ieff2 = 0 always use average efficiency
c           ieff2 = 1 use max efficiency if ieffmax=1
c           iopsou(1,l2) = source reservoir #1 
c           iopsou(2,l2) = source reserovir #1 account
c               Note the following source 2 data is only used when
c               releases are tied to another type 6 operating rule  
c           iopsou(3,l2) = if > 0 source reservoir #2    
c           iopsou(4,l2) = if > 0 source reservoir #2 account
c
c           iopsou(5,l2) = not used
c           iopsou(6,l2) = Reservoir release type and efficiency
c                          if = 0 release to meet demand
c                          if > 0 release only if a CIR (IWR) exists
c                                 and limit release to not exceed
c                                 IWR/n,  
c                          Note nmax = min(nmax, effmax) to save 
c                                 iterating
c           ireltyp        same as iopsou(6,l2)
c
c           iopdes(1,l2) = destination (+=diversion, -=reservoir)
c           iopdes(2,l2) = destination user (account)
c
c           iout         = 0 no detailed printout
c                          1 yes detailed printout
c
c           iown         = source reservoir #1 account
c           iscd         = reservoir #1 river station
c
c           imonsw()     = monthly on off switch  
c           iowna        = source reservoir #2 account
c

c           irow         = destination user (account)
c
c           ires         = switch 0=diversion, 1=reservoir
c            
c           l2 - order of operating rule in opr. rule list. 
c
c           nd           = destination (+=diversion, -=reservoir)  
c           ndnd         = # of downstream nodes for reservoir #1
c
c
c	    intern(  )   = If > 0 carrier system with intervening
c                          structures
c
c        qdiv(18        Carrier passing thru a structure (e.g. divcar)
c        qdiv(20        From Carrier by Storage or Exchange (e.g. carrpl)
c        qdiv(28,       Stored via a reuse plan  
c        qdiv(31,       Diversion from ReUse plan to a Res or Diversion
c
c           qres(4,ix)   = From Carrier by Storage to Reservoir
c           qres(8,ix)   = Reservoir Storage to Trans Mountain Carrier
c           qres(9,ix)   = Reservoir Storage to Carrier??
c           qres(26,ix)  = Reservoir Storage to River
c           qres(11,ix)  = Reservoir Storage to Carrier
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
      character cwhy*24, cdestyp*12, ccarry*3, cresid1*12
c
c _________________________________________________________
c               Step 1; Initilize
c
      iout=0
      if(ichk.eq.135) iout=2
      if(corid(l2).eq. ccall) ioutiw=iw
      
      if(iout.ge.1 .and. ncallx.eq.0) then
        write(nlog,102) corid(l2), iout, ioutiw, iw
 102    format(/, 72('_'),/ 
     1  '  DivImpR; ID = ', a12, 5i5)
      endif         
      

      
c		b. Daily capability
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif
c
c               c. Miscellaneous
      small=0.001
      divact = 0.0          
      divalo = 0.0
      relact=0.0
      ishort = 0
      iw=iw
      iowna=0
      
      iwhy=0
      cwhy='N/A'
      cdestyp='N/A'
      ccarry='No'
      nc=0
      nd=0
      ns=0
      iuse=0
      diwrreqX=-1.0/fac
      divreqX=-1.0/fac
      divmax=-1.0/fac      
      divmonX=-1.0/fac
      effmax1=-1.0
      divcapX=-1.0
      ireltyp=0
      
c
c rrb 00/12/26; Variable efficiency consideration
      ieff2=1     
c
c rrb 01/01/17; Call number
      icx=32
cr    write(nlog,*) '  DivImpR iwx', iwx
c
c		Step 1b; Check avail array
      call chekava(2, maxsta, numsta, avail)
c
c _________________________________________________________
c               Step 2; Branch if not on this month
c
      if(imonsw(l2,mon).eq.0) then
        iwhy=1
        cwhy='Monthly switch is off'
        goto 330
      endif  
c
c ---------------------------------------------------------
c		For a daily model set demand for beginning of season
      if(iday.eq.1 .and. imonsw(l2,mon).gt.0) then
        if (idy.lt.imonsw(l2,mon)) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 330
        endif  
      endif  
c
c ---------------------------------------------------------
c		For a daily model set demand for end of season
      if(iday.eq.1 .and. imonsw(l2,mon).lt.0) then
        if (idy.gt.iabs(imonsw(l2,mon))) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 330
        endif  
      endif  
      
      
c      
c ________________________________________________________
c               Step 3; Set Plan pointer for Destination Reuse plan
      ipUse=ireuse(l2)
c
c _________________________________________________________
c
c               Step 4; Set source data (a transmountain import plan)
c
      np  =IOPSOU(1,l2)    
      iscd=ipsta(np)
      NDNs=NDNNOD(iscd)

      alocfs = PImport(np)
      relalo=alocfs
c
c               b. Exit if off      
      if(Pon(np).eq.0) then
        iwhy=2
        cwhy='Source Plan (import) is off'
        goto 330
      endif  
c
c               c. Exit if nothing available
      if(alocfs.lt.small) then
        iwhy=3
        cwhy='Import is zero'
        goto 330
      endif  
c
c _________________________________________________________
c               
c               Step 5; Set destination data
c
      nd  = iopdes(1,l2)
cr    write(nlog,*) '  Divers; l2, ityopr = ', ityopr(l2)    
c
c ---------------------------------------------------------
c
c               a. Destination is a reservoir (nd<0 & iresw=1)
      if(nd.lt.0) then
        cdestyp='Reservoir'        
        iresw=1
        nd=-nd
        idcd=irssta(nd)        
        NDND=NDNNOD(IDCD)
c
c ---------------------------------------------------------
c rrb 2006/09/25; Allow multiple accounts - Initilize
cr      irow=nowner(nd)+iopdes(2,l2)-1  

        nro=1
        if(iopdes(2,l2).lt.0) then
          nro=-iopdes(2,l2)
          irow=nowner(nd)
        endif

        if(iopdes(2,l2).gt.0) then
          nro=1
          irow=nowner(nd)+iopdes(2,l2)-1
        endif
              
c
c ---------------------------------------------------------
c
        if (iressw(nd).eq.0) then
          iwhy=4
          cwhy='Destination Res is off'
          goto 330
        endif  
c
c       write(nlog,*) '  DivImpR; irow = ', irow
cr      go to 120      
      endif
c
c ---------------------------------------------------------
c               b. Destination is a diversion (nd>0 & iresw=0)
      if(nd.gt.0) then
        cdestyp='Diversion'              
        iresw=0
        idcd=idvsta(nd)        
        NDND=NDNNOD(IDCD)
        

        IUSE=NDUSER(ND)+IOPDES(2,l2)-1
        divreqX=divreq(iuse)
        
        if(idivsw(nd).eq.0) then
          iwhy=5
          cwhy='Destination Div is off'        
          goto 330
        endif  
      endif  
c
c ---------------------------------------------------------
c               e. Carrier system data 
 120  if(ityopr(l2).ne.10) then
cr      if(intern(l2,1).eq.0) go to 130
        if(intern(l2,1).gt.0) then
          ccarry='Yes'
          ndr=intern(l2,1)

          idcd=idvsta(ndr)  
          ndnd=ndnnod(idcd)
c         write(nlog,*) '  DivImpR; l2, idcd', l2, idcd
cr        go to 140
        endif  
      endif
c
c
c _____________________________________________________________
c               f. Check printout 
c
      IF(-IOPOUT.eq.ISCD .or. iout.ge.1) then
        if(iwx.eq.1) then
          write(nlog,270) cdestyp, ccarry
cr      else
cr        write(nlog,*) ' '
        endif  
      endif  

c _________________________________________________________
c
c            **  Step 6; Set demand (DIVALO) when the destination
c                        is a diversion
c        
      if(iresw.eq.0) then
c
c ---------------------------------------------------------
c               a. Diversion demand 
        DIVALO=AMIN1(DIVREQ(IUSE),DIVCAP(ND)-DIVMON(ND))
        divcapX=DIVCAP(ND)-DIVMON(ND)
        divmonX=divMon(nd)
c
c

c
c            Adjust based on release type
c		Set based on release type
c		ireltyp=0 demand
c		ireltyp>0
c               ireltyp = 0 release to meet demand
c               ireltyp > 0 release only if a CIR (IWR) exists
c                           and limit release to not exceed IWR/n  
        effmax1=effmax(nd)
        ireltyp=amin0(iopsou(6,l2),ifix(effmax(nd)))

        if(ireltyp.gt.0) then 
          diwrreqX=diwrreq(iuse)        
          if(diwrreq(iuse).le.small) then
            divalo=0.0
          else
            divmax = diwrreq(iuse)/(float(ireltyp)/100.0)
            divalo=amin1(divalo, divmax)
          endif
        endif

        divalo=amax1(0.0,divalo)
c
c rrb 01/08/23; Exit if no demand
        if(divalo.le.small) then
          iwhy=6
          cwhy='Demand is zero'
          goto 330
        endif
      endif
c 
c
c ---------------------------------------------------------
c
c               b. Reservoir Demand
      if (iresw.eq.1) then
c
c ---------------------------------------------------------
c rrb 2006/09/25; Allow multiple accounts - Demand
        cursa=0.0
        do n=1, nro
          n1=irow+n-1
          cursa=cursa+(ownmax(n1)-curown(n1))
        end do  
              
cr      divalo=amin1(ownmax(irow)-curown(irow),
        divalo=amin1(cursa,
     1         volmax(nd)-cursto(nd),tarmax(nd)-cursto(nd))/fac
        divalo=amax1(0.0, divalo)
c
c ---------------------------------------------------------
c               Exit if nothing available
        if(divalo.lt.small) then
          iwhy=7
          cwhy='Destination Res is full'
          goto 330
        endif  
        
c
c ---------------------------------------------------------
c               Check Prinout for reservoirs
        if(iout.eq.1) then
          write(nlog,*) '  DivImpR; nd, irow ',
     1              'ownmax(irow)-curown(irow), ',
     1              'volmax(nd)-cursto(nd),  ',
     1              'tarmax(nd)-cursto(nd), divalo*fac'
          write(nlog,*)  nd, irow,
     1               ownmax(irow)-curown(irow),
     1               volmax(nd)-cursto(nd),
     1               tarmax(nd)-cursto(nd), divalo
        endif
      endif
c
c
c _________________________________________________________
c
c               Step 7; Limit release (ALOCFS) to capacity of
c                       intervening structures
c                       if required for type 2 & 3 only
cr    if(ityopr(l2).ne.10) then
        do 150 i61=1,10
c
          if (intern(l2,i61).eq.0) go to 160
          intvn=intern(l2,i61)

  150     alocfs=amin1(alocfs,(divcap(intvn)-divmon(intvn))) 
          alocfs=amax1(0.0,alocfs)
  160   continue
cr    endif
c
c _________________________________________________________
c
c           **  Step 8; Set diversion (DIVACT) and
c               release (RELACT) to be the minimum of
c               import water (ALOCFS) and demand (DIVALO) 
      divact=amin1(alocfs,divalo)
      divact=amax1(0.0,divact)
      relact=-divact
c
c
c _________________________________________________________
c
c               Step 9; Exit if no demand (divact <=0) or 
c
      if (divact.le.small) then
        iwhy=8
        cwhy='Demand is zero'
        goto 330
      endif  
c
c
c _________________________________________________________
c
c               Step 10; Add import (RELACT) to stream
c
      iwhy=-1
      cwhy='N/A'
      AVAILR=AVAIL(iscd)
      call takout(maxsta,avail,river,avinp,qtribu,idncod,
     1            relact,ndns,iscd)
      avail(iscd)=availr
c
c _________________________________________________________
c
c               Step 11 Remove diversion (DIVACT) from stream

      CALL TAKOUT(maxsta,AVAIL,RIVER,AVINP,QTRIBU,IDNCOD,
     1            DIVACT,NDND,IDCD)
c     write(nlog,*) '  DivImpR; idcd,divact,relact = ',idcd,divact,relact
c     write(nlog,*) '  DivImpR; iscd,divact = ', idcd, divact, relact
c
c
c _________________________________________________________
c
c               Step 12; Add return flows to stream
c
      if (iresw.eq.0 .and. ipUse.eq.0) then
        call rtnsec(icx,divact,l2,iuse,IDCD,nd,ieff2)
      endif
        
c
c _________________________________________________________

c		Step 13; Calculate reuse   
      if(ipUse.gt.0) then
        if(nd.gt.0) then            
          CALL RtnsecR(icx,divact,l2,iuse,idcd,nd,
     1         ieff2,ipUse)
c
c rrb 04/12/30; Qdiv(28 is the carried / exchanged water
c		Note works outside river system
          ipsta1=ipsta(ipUse)
          qdiv(28,ipsta1) = psuplyT(ipUse)     
        else
c
c		Reservoir Reuse          
cr        ircp=ipsta(ipUse)
          psuply(ipUse)=psuply(ipUse)+divact
          psuplyT(ipUse)=psuplyT(ipUse)+divact
c
c rrb 2006/01/01; Correction
          if(iplntyp(ipuse).eq.3 .or. iplntyp(ipuse).eq.5) then
            psto2(ipUse)=psto2(ipUse)+divact*fac          
          endif  
          
          ipsta1=ipsta(ipUse)
          qdiv(28,ipsta1) = psuplyT(ipUse)                 
        endif  
      endif
c
c _________________________________________________________
c
c               Step 14; Update storage, demand, etc.
c
c ---------------------------------------------------------
c               a. Destination is a reservoir, update storage data
      if(iresw.eq.1) then
c
        divaf=divact*fac
        cursto(nd)=cursto(nd)+divaf
c
c ---------------------------------------------------------
c rrb 2006/09/25; Allow multiple accounts - Allocate
c		  Note:
c		   iResT1=0 distributes based on ownership ratio
c		   nrown1=number of accounts in this reservoir
c		   iown = first account associated with this reservoir  
c		   icx  = subrouine calling accou.for       
c		   ia   = account to adjust
c
cr      curown(irow)=curown(irow)+divaf      
        nrX=nd
        iResT1=0
        nrown1=nro
        iownX=irow
        icx=2
        if(intern(l2,1).gt.0) then
          ia=2
        else
          ia=26
        endif  
        cresid1=cresid(nrX)
c        
        call accou(maxacc, maxown, nrX, ownmon, curown, accr, ia, 
     1    ownmax, iownX, nrown1, cursa, divaf, iResT1, icx, cresid1)
        
c
c ---------------------------------------------------------
c
        if(intern(l2,1).gt.0) then
c
c                  qres(4,ix) =  From Carrier by Storage to Reservoir
          qres(4,nd)=qres(4,nd)+divaf
cr        accr(4,irow)=accr(4,irow)+divaf
        else           
c
c               From Reservoir Storage to River
          qres(26,nd)=qres(26,nd)+divaf
cr        accr(26,irow)=accr(26,irow)+divaf
        endif
cr      go to 260
      endif
c
c ---------------------------------------------------------
c               b. Destination is a diversion update demand data
c
      if(iresw.eq.0) then
        USEMON(IUSE)=USEMON(IUSE)+DIVACT
        DIVREQ(IUSE)=DIVREQ(IUSE)-DIVACT
        DIVMON(ND  )=DIVMON(ND  )+DIVACT
c
c ---------------------------------------------------------
c               c. From Carrier Qdiv(20
c
        if(intern(l2,1).ne.0) then
          inode=idvsta(nd)
          qdiv(20,inode)=qdiv(20,inode)+divact               
        endif                                             
c
c ---------------------------------------------------------
c               d. From River by Exch or Plan Qdiv(31 
cr      QDIV(7,IDCD)=QDIV(7,IDCD)+DIVACT
        qdiv(31,idcd)=qdiv(31,idcd)+divact
      endif
c
c ---------------------------------------------------------
c               e.  Carrier passing throught a structure Qdiv(18
      do i11=1,10
c
        if (intern(l2,i11).eq.0) go to 282
        intvn=intern(l2,i11)
     
        divmon(intvn)=divmon(intvn)+divact
        inode=idvsta(INTVN)
        qdiv(18,inode)=qdiv(18,inode)+divact
      end do  
  282 continue
c
c ---------------------------------------------------------
c               i. Adjust source (import) data
      PImport(np)=amax1(0.0, PImport(np)-divact)
      PImportT(np)=PImportT(np)+divact
c
c rrb 2006/01/01; Correction for a reservoir plan
      if(iplntyp(np).eq.3 .or. iplntyp(np).eq.5) then
        psto2(np)=amax1(psto2(np)-divact*fac, 0.0)                
      endif  
      
      qdiv(18,iscd)=qdiv(18,iscd)+divact
c
c _________________________________________________________
c               
c               Step 23;  Set shortage switch (ishort)
  330 if((divact+small) .lt. divalo) ishort=1
c  
c ---------------------------------------------------------
c               h. Update operating value
      divo(l2)=divo(l2)+divact
c
c _________________________________________________________
c               
c               Step 15; Check results
c
c               a. Check that Avail flow > 0
      call chekava(2, maxsta, numsta, avail)
c
c               b. Detailed Check
c
       if(iout.eq.2) then
         write(nlog, 280)  ' DivImpR    ',
     1    iyrmo(mon),xmonam(mon),idy, cstaid(iscd),
     1    iwx, iw,nwrord(1,iw),l2,ns, ND,iuse,ipUse,
     1      float(iopsou(6,l2)), effmax1, float(ireltyp), 
     1      divreqX*fac,divcapX, diwrreqX*fac, 
     1      divmax*fac, relalo*fac, 
     1      divmonX*fac, divact*fac, iwhy, cwhy
       endif
  280   FORMAT(a12, i5,1x,a4, i5, 1x, a12,
     1   8i8,10F8.1,i8,1x, a24)
c
c _________________________________________________________
c
c               Step 16; Return
      RETURN

c               Formats
  270   format(/, 
     1    ' DivImpR (Type 32) Destination Type = ', a12,
     1    1x,a3,' Carrier'/
     1  ' DivImpR      iyr mon   day ID          ',
     1  '    Iter      Iw  nwrord      l2      ns      nd',
     1  '    iuse   ipUse',
     1  ' Eff_opr Eff_max iRelTyp  Demand Cap-cfs     IWR',
     1  '  Divmax  Relalo  Divmon  DIVACT    iwhy',
     1  ' Comment',/
     1  ' ___________ ____ ____ ____ ____________', 
     1  ' _______ _______ _______ _______ _______ _______',
     1  ' _______ _______',
     1  ' _______ _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' __________________________')
     
c
c               Error warnings
c _________________________________________________________
 9999 write(6,1050) 
      write(nlog,1051) 
    
 1050 format('    Stopped in DivImpR',/,
     1       '    See the *.log file')
 1051 format('    Stopped in DivImpR')
      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)


      stop 
      END

