
c
c _________________________________________________________
c	Documentation
c

c
c _________________________________________________________
c	Dimensions
C
      SUBROUTINE DirectFS(iw,l2,ishort,divactx,ncallx)
c
c
c _________________________________________________________
c	Program Description
c
c       Type 16. Direct Flow Storage
c       Directfs; Direct flow storage opeating rule
c              Operating rule type 16
c                Source is a water right (iopsou(1,l2)
c                Destination is a reservoir (iopdes(1,l2), 
c                  account (iopdes(2,l2)
c                Storage is limited by right, availability of water,
c                  irrigation season (demand is non zero),
c                  exchange potential diversion to reservoir,
c                  and % that can be stored (iopsou(4,l2))
c
c                Note for this operating rule, the water right
c                  operates as:
c                  1. a direct flow right and
c                  2. as an operating rule right.
c
c
c _________________________________________________________
c	Update History
c
c
c rrb 99/11/02; Revised to allow destination water right to be
c               tied to a carrier
c rrb 2006/03/08; Revised to serve more than 1 account
c
c _________________________________________________________
c
c      Documentation
c
c               IW : OVERALL WATER RIGHT ORDER
c               L2 : LOC. OF operation right  in opr RIGHT TABLE
C
c               ndr   = iopsou(1,l2) = source water right
c                       iopsou(2,l2) = user
c                       iopsou(4,l2) = maximum diversion % (1-bypass)
c                       iopsou(5,l2) = structure with demand when the
c                                      source is a carrier.  Note for
c                                      a carrier it may not be the
c                                      structure where the right is at
c               nd1   = source diversion ID
c               iscd  = idvsta(l2) stream ID of source diversion (nd1)
c               ndns  = # of nodes downstream of source diversion (nd
c               iuse  = source user (used to get demand data)
c
c               nd2   = iopdes(1,l2) = destination reservoir ID
c               iown  = iopdes(2,l2) = destination reservoir account ID
c               iscd2 = stream ID of destination reservoir (nd2)
c               ndns2 = # of nodes downstream of destination res nd2)
c
c               intern(k,i) = intervening structure(1)
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
      character cwhy*48, cdestyp*12, ccarry*3, cpuse*3, cstaid1*12,
     1          rec12*12, cTandC*3, cdivtyp1*12, cresid1*12
c
c _________________________________________________________
c
c               Step 1; Initilize
c		iout=0 no detials
c		     1 details
c		     2 summary
      iout=0
      ioutiw=0
      
      if(ichk.eq.116) iout=2
      if(corid(l2).eq. ccall) ioutiw=iw
c     write(nlog,*) '  Directfs; ichk, corid(l2), iout, ccall'
c     write(nlog,*) '  Directfs;', ichk, corid(l2), iout, ccall
c
c               a. Convergence
      small=0.001
c
c               b. Daily capability

      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif
c               c. Miscellaneous
      divact=0.0
      divalo=0.0
      bypass=0.0
      ISHORT=0

      divd1=-1./fac
      divcap1=-1./fac
      pavail=-1./fac
      
      cursto1=-1.
      volmax1=-1.
      tarmax1=-1.
      curown1=-1.
      ownmax1=-1.
      divRes1=-1.
      
      divact1=0.0
      bypass1=0.0
      
      iwhy=0
      cwhy='NA'
      cdestyp='NA'
      ccarry='No'
      cpuse='No'
      cTandC='No'
      cdivtyp1='Diversion'
      
      
c
c _________________________________________________________
c
c               Step 2; Set source data
c
c               a. Source structure (nd1) a water right
c                  Source location (iscd)
c                  Number of downstream nodes (ndns)
      ndr =iopsou(1,l2)

c
c rrb 00/01/28; Revised carrier treatment
c     nd1 =idivco(1,ndr)    
      if(iopsou(5,l2) .eq. 0) then
        nd1 = idivco(1,ndr)    
      else
        nd1 = iopsou(5,l2)
      endif
c
c               Return if source structure is off
      if(idivsw(nd1).eq.0) then
        iwhy=1
        cwhy='Source structure is off'
        goto 500
      endif

      iscd=idvsta(nd1)
c     ndns=ndnnod(iscd)
c
c rrb 2007/03/20; Variable iopsou(2,l2) is used to turn
c		  the source right on/off in Oprinp
c		  Therefore Estiamte the user is account 1
c		  
c     iuse=nduser(nd1)+iopsou(2,l2)-1
      iuse=nduser(nd1)
      divd1=dcrdiv(ndr) - divd(ndr)
      divcap1=divcap(nd1)-divmon(nd1) 
c
c rrb 99/11/02; Set data when the source structure is tied to 1 carrier

      if(intern(l2,1).gt.0) then
        ndc   = intern(l2,1)
c
c rrb 00/01/28; Not required since oprinp searches for a opr right 
c       iscdc = idvsta(ndc)
      endif
      
      DivMax=float(iopsou(4,l2))/100.0
c
c rrb 2007/03/08
      if(DivMax.lt.small) then
        iwhy=1
        cwhy='Maximum Diversion % is zero'
        goto 500
      endif
c
c _________________________________________________________
c
c               Step 3; Set destination data (a reservoir)         
c
      nd2  =iopdes(1,L2)
      iscd2=irssta(nd2)
      ndns2=ndnnod(iscd2)
      if (iressw(nd2).eq.0) goto 500
      
cr    iown=nowner(nd2)+iopdes(2,l2)-1
      cursto1=cursto(nd2)
      volmax1=volmax(nd2)
      tarmax1=tarmax(nd2)
cr      curown1=curown(iown)
cr      ownmax1=ownmax(iown)
c
c ---------------------------------------------------------
c rrb 2007/03/08; Allow multiple accounts - Initilize
cr      irow=nowner(nd)+iopdes(2,lr)-1

        nro=1
        if(iopdes(2,l2).lt.0) then
c
c rrb Correction
c         nro=-iopdes(2,lr)
          nro=-iopdes(2,l2)
          iown=nowner(nd2)
        endif

        if(iopdes(2,l2).gt.0) then
          nro=1
          iown=nowner(nd2)+iopdes(2,l2)-1
        endif
c
c
c ---------------------------------------------------------
c               b. Check Prinout for reservoirs
        if(iout.eq.1) then
          write(nlog,*) '  Divres; nd, iown ',
     1              'ownmax(iown)-curown(iown), ',
     1              'volmax(nd2)-cursto(nd2),  ',
     1              'tarmax(nd2)-cursto(nd2), divalo*fac'
          write(nlog,*)  nd, iown,
     1               ownmax(iown)-curown(iown),
     1               volmax(nd2)-cursto(nd2),
     1               tarmax(nd2)-cursto(nd2), divalo
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
        goto 500
      endif
c
c ---------------------------------------------------------
c		For a daily model set demand for beginning of season
      if(iday.eq.1 .and. imonsw(l2,mon).gt.0) then
        if (idy.lt.imonsw(l2,mon)) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 500
        endif  
      endif  
c
c ---------------------------------------------------------
c		For a daily model set demand for end of season
      if(iday.eq.1 .and. imonsw(l2,mon).lt.0) then
        if (idy.gt.iabs(imonsw(l2,mon))) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 500
        endif  
      endif  
      
c
c
c _________________________________________________________
c
c               Step 5; Set allowable diversion (divalo)
c
c               a. Limit allowable diversion to irrigation season
c                    (months with a demand)
      if(diver(mon,iuse) .le. small) then
        iwhy=3
        cwhy='Demand (Diver) is zero (non irrigation season)'
        goto 500
      endif
c
c               b. Limit to remaining diversion capacity
c                   (divcap(nd1)-divmon(nd1))
c rrb 2007/03/08; Should be in cfs and reduced by factor
cx    divcap2=(divcap(nd1)-divmon(nd1))/fac
      divcap2=divcap(nd1)-divmon(nd1)
      divalo =amax1(divcap2, 0.0)
      if(divalo .le. small) then
        iwhy=4
        cwhy='Remaining Capacity (DivCap) is zero'
      endif  
c
c rrb 99/11/02;
c               c. Limit to carrier capacity (ID = ndc)
      if(intern(l2,1).gt.0) then
c      
c rrb 2007/03/08; Should be in cfs
cr      divcapc2=(divcap(ndc)-divmon(ndc))/fac
        divcapc2= divcap(ndc)-divmon(ndc)
        divalo  = amin1(divcap2,divcapc2)
        divalo  = amax1(divalo, 0.0)
        if(divalo .le. small) then
          iwhy=5
          cwhy='Carrier Capacity (DivCapc2) is zero'
        endif  
      endif
c
c               d. Limit by decree * max DFS amount %
c
c rrb 00/01/27; Correction on Bypass treatment
c               Adjust after limited by flow (Step 7)
c     iopsou(4,l2)=100.
c     divdec1=(dcrdiv(ndr)-divd(ndr))*DivMax
      divdec1=dcrdiv(ndr)-divd(ndr)
      divalo =amin1(divalo, divdec1)
      divalo =amax1(divalo, 0.0)
      if(divalo .le. small) then
        iwhy=6
        cwhy='Remaining decree (DecRem) is zero'        
      endif  
c
c               e. Limit to remaining reservoir capacity
c                   (volmax-cursto) and target (tarmax-cursto)
      divres1=amin1(volmax(nd2)-cursto(nd2),
     1              tarmax(nd2)-cursto(nd2))/fac
      divalo=amin1(divalo, divres1)
      divalo=amax1(divalo, 0.0)
      if(divalo .le. small) then
        iwhy=7
        cwhy='Available reservoir capacity (Divres1) is zero'
      endif  
c
      
c
c ---------------------------------------------------------
c               f. Limit demand by space in account
c rrb 2007/03/08; Allow multiple accounts - Demand
      cursa=0.0
      do n=1, nro
        n1=iown+n-1
        cursa=cursa+(ownmax(n1)-curown(n1))
      end do  
      
      curown1=cursa
      ownmax1=cursa
      divown1=cursa/fac
c
c rrb 2007/03/08; Increase demand by DivMax (factor 0-100%)      
c     divalo =amin1(divalo,divown1)
      divalo =amin1(divalo,divown1)/DivMax
      divalo =amax1(divalo, 0.0)
      
      if(divalo .le. small) then
        iwhy=8
        cwhy='Reservoir Account capacity (Curown1) is zero'
      endif  
c
c _________________________________________________________
c
c               Step 6; Set available flow (pavail)
c                       to be the minimum from the destination
c                       reservoir (iscd2 & ndns2) down since we
c                       are exchanging the water upstream
c
      do is=1,numsta
        avtemp(is)=avail(is)
      end do

      CALL DNMFSO(maxsta,AVTEMP,IDNCOD,ISCD2,NDNS2,IMCD)
      pavail=avtemp(imcd)
c
c _________________________________________________________
c
c               Step 7; Set actual diversion (divact)
c                       Include limit for bypass requirement
c
c
c rrb 00/01/27; Correction on Bypass treatment
c     divact=amin1(pavail,divalo)
cr    divact=(amin1(pavail,divalo)) * DivMaxfloat(iopsou(4,l2))/100.0
      divact=(amin1(pavail,divalo)) * DivMax
      divact=amax1(divact, 0.0)

      if (divact.lt.small) then
        iwhy=9
        cwhy='Available Flow (Pavail) is zero'
        goto 500
      endif
c
c _________________________________________________________
c
c               Step 8; Remove from stream starting at the destination
c                       reservoir (ndns2, iscd2)
c
      CALL TAKOUT(maxsta,AVAIL ,RIVER ,AVINP ,QTRIBU,IDNCOD,
     1            DIVACT,NDNS2, ISCD2 )
c
c _________________________________________________________

c               Step 9; Adjust avail for the bypass amount
c                       from the destination reservoir to the
c                       DFS diversion or it's carrier
c
cr    bypass = divact*(1.0 - float(iopsou(4,l2))/100.)
      bypass = divact*(1.0 - DivMax)
      
      iss=iscd2
      avail(iss)=avail(iss) - bypass
      iss=idncod(iss)
c
c rrb 99/11/02; Allow source to be at a carrier
      isx=iscd
c
c rrb 00/01/28; Revised carrier treatment
c     if(intern(l2,1).gt.0) isx=iscdc
c
      if(iss.ne.0 .and. ndns2.gt.1) then
        ifound=0
        do nd=1,ndns2-1
          if(ifound.eq.0) then
            avail(iss)=avail(iss)-bypass
c
c rrb 99/11/02; Allow source to be at a carrier
c           if(iss.eq.iscd) ifound=1
            if(iss.eq.isx)  ifound=1
          endif
          iss=idncod(iss)
        end do
      endif
c _________________________________________________________
c
c               Step 10.  Check and Update
c
c               a. Check minimum
      CALL DNMFSO(maxsta, AVAIL, IDNCOD, ISCD2, NDNS2, IMCD)
      IF(AVAIL(IMCD).lT.(-1.0*small)) then
        write(nlog,100)
        write(nlog,110) (avail(iss),iss=1,numsta)
        write(nlog,120) (river(iss),iss=1,numsta)
        goto 9999
      endif
c
c               b. Update reservoir data
      divaf=divact*fac
      cursto(nd2)  =cursto(nd2)+divaf
      qres(3,nd2)  =qres(3,nd2)+divaf
cr      curown(iown) =curown(iown)+divaf
cr      accr(3,iown) =accr(3,iown)+divaf
      
c
c ---------------------------------------------------------
c rrb 2006/09/25; Allow multiple accounts - Allocate
c		  Note:
c		   iResT1=0 distributes based on ownership ratio
c		   nrown1=number of accounts in this reservoir
c		   iown = first account associated with this reservoir  
c		   icx  = subrouine calling accou.for       
c		   ia   = account to adjust
      
      nrX=nd2
      iResT1=0
      nrown1=nro
      iownX=iown
      icx=116
      ia=3
        
      cresid1=cresid(nrX)
      call accou(maxacc, maxown, nrX, ownmon, curown, accr, ia, 
     1    ownmax, iownX, nrown1, cursa, divaf, iResT1, icx, cresid1)
c
c ---------------------------------------------------------
c               c. Update diversion by this water right
c                  include factor for diversion limit %
cr    DIVD(NDR)=DIVD(NDR)+DIVACT / (float(iopsou(4,l2))/100.)
      DIVD(NDR)=DIVD(NDR)+DIVACT / DivMax
c
c ---------------------------------------------------------
c               d. Update diversion by this Operating Rule
      DIVO(L2)=DIVO(L2)+DIVACT
c
  500 divactx = divact
c
c _________________________________________________________
c
c               Step 11.  Check results
c
c
c          Detailed printout
      if(iout.ge.1 .and. iw.eq.ioutiw) then   
        ncallX=ncallX+1                   
        if(ncallX.eq.1) then
          write(nlog,270) corid(l2), cdestyp, ccarry, cTandC, cpuse,
     1      cDivTyp1
        else
          write(nlog,*) ' '
        endif  
        
        write(nlog,280)'    In    ',    
     1    iyrmo(mon),xmonam(mon), idy,                
     1    imonsw(l2,mon), ndr, nd1, iuse, iscd,
     1    nd2, iscd2, imcd, iopsou(4,l2),
cr     
cr        write(nlog,*)  
     1    diver(mon,iuse)*fac, divd1*fac,   divcap1*fac,
     1    cursto1,    volmax1,    tarmax1,    divRes1, 
     1    curown1,    ownmax1,
     1    pavail*fac, divact1*fac, bypass1*fac, 
     1    (divact1+bypass1), iwhy, cwhy

        write(nlog,280) '    Out   ',
     1    iyrmo(mon),xmonam(mon), idy,                     
     1    imonsw(l2,mon), ndr, nd1, iuse, iscd,
     1    nd2, iscd2, imcd, iopsou(4,l2), 
     1    diver(mon,iuse)*fac, (dcrdiv(ndr)-divd(ndr))*fac,
     1    (divcap(nd1)-divmon(nd1))*fac,
     1    cursto(nd2), volmax(nd2), tarmax(nd2), divRes1*fac,
     1    curown1,   ownmax1,
     1    pavail*fac, divact*fac, bypass*fac, (divact+bypass)*fac,
     1    iwhy, cwhy
      endif
      
  270   format(/, 
     1  '  DirectFS (Type 16); Operation Right ID = ', a12,
     1  ' Destination Type = ', a12,
     1  ' Carrier (Y/N) = ',a3, ' T&C Plan (Y/N) = ',a3,
     1  ' Reuse Plan (Y/N) = ', a3,
     1  ' Diversion Type = ', a12,/    
     1  '  Directfs',
     1  '   Yr   Mo  Day MoOn  ndr  nd1 iuse iscd  nd2 isd2 imcd',
     1  ' Max%',
     1  '   Diver  DecRem  DivCap  ResVol  ResMax  TarMax DivRes1',
     1  ' AccVol',
     1  '  AccMax  Pavail  Divact  Bypass   Total  iwhy cwhy',
     1   /,10x,
     1  12(' ____'), 13(' _______'),' ____',1x,48('_'))
      
c
c _________________________________________________________
c
c               Step 12.  Return

      RETURN
c
c               Formats
c _________________________________________________________
c
 100  format(  '  DirectFS; Problem avail is negative')
 110  format(  '  DirectFS: avail  ',10f10.2)
 120  format(  '  DirectFS: river  ',10f10.2)
c130  format(/,'  Directfs',
c    1  '   Yr   Mo MoOn  ndr  nd1 iuse iscd  nd2 isd2 imcd Max% iwhy',
c    1  '   Diver  DecRem  DivCap  ResVol  ResMax  TarMax  AccVol',
c    1  '  AccMax  Pavail  Divact  Bypass',
c    1   /,10x,
c    1  13(' ____'), 11(' _______'))
 280  format(
     1 a10, i5,1x,a4,10i5,13f8.0, i5,1x,a48)

c
c _________________________________________________________
c
c               Error Warnings
c
 9999 write(6,510)
      write(nlog,520) 

 510  format('    Stopped in DirectFS',/,
     1       '    See the *.log file')
 520  format('    Stopped in DirectFS')
      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop 
      END



