c resrpl - Type 5 operating rule, reservoir to reservoir exchange
c_________________________________________________________________NoticeStart_
c StateMod Water Allocation Model
c StateMod is a part of Colorado's Decision Support Systems (CDSS)
c Copyright (C) 1994-2021 Colorado Department of Natural Resources
c 
c StateMod is free software:  you can redistribute it and/or modify
c     it under the terms of the GNU General Public License as published by
c     the Free Software Foundation, either version 3 of the License, or
c     (at your option) any later version.
c 
c StateMod is distributed in the hope that it will be useful,
c     but WITHOUT ANY WARRANTY; without even the implied warranty of
c     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c     GNU General Public License for more details.
c 
c     You should have received a copy of the GNU General Public License
c     along with StateMod.  If not, see <https://www.gnu.org/licenses/>.
c_________________________________________________________________NoticeEnd___
c
      subroutine resrpl(iw,l2,divactx,ncallX)
c
c
c _________________________________________________________
c	Program Description
c
c       Resrpl; It performs a reservoir to reservoir exchange
c       Type 5. Reservoir storage by Exchange with a reservoir
c
c _________________________________________________________
c       Update History
c
c rrb 2021/05/02; Runtime error tracking
c
c rrb 2021/04/18; Compiler warning
c
c rrb 2020-04-03; Add qdiv(38 to .xdd reporting to include
c                 From Storage to River for Exchange in column
c                 Carried, Exchange, Other
c       qdiv(38,ipcd) = qdiv(38,ipcd) + stocfs
c
c
c rrb 2020-04-03; Correct *.xre reporting  to both
c                 From Storage to River for Use and
c                 From Storage to River for Exchange only
cx      qres(9,nr)=qres(9,nr)+actacf
cx      accr(9,iown)=accr(9,iown)+actacf
c
c rrb 2002/10/22; Allow monthly on/off switch (imonsw)
c
c rrb 1996/03/13; initialize divact, send returns to bottom & set divo
c
c _________________________________________________________
c       Documentation
c
c      nr    = replacement reservoir
c      iown  = replacement reservoir account
c      ipcd  = replacement reservoir river station
c      ndnp  = replacement reservoir downstream info
c
c      nrd   = destination reservoir
c      iownd = destination reservoir account
c      ircd  = destination reservoir river station
c      ndnr  = destination reservoir downstream info
c
c      qdiv(38 Carried water reported as Carried, Exchange 
c              or Other at source but not used to calculate
c              River Divert in Outmon.f  
c
c	     qres(3  From River by Other (e.g. Exchange, etc.)
c      qres(21 From Storage for Exchange at Source


c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
      character cresid1*12, cwrid*12
      character cwhy*50, cdestyp*12, ccarry*3, cpuse*3, cstaid1*12
c
c ---------------------------------------------------------
c rrb 2021/05/02; Runtime error tracking
      character cCallBy*12
      cCallBy = 'Resrpl'      
c
c _________________________________________________________
c		Step 1; Initialize
c
c
c rrb 2021/04/18; Compiler warning
      iownd=0
      fac=0.0
c		           iout=0 No details
c		           iout=1 Details
c		           iout=2 Summary
c		           iout=99 Summary independent of ichk
cx      if(ichk.eq.4) write(nlog,*) ' Resrpl; Type 5 Processing ', 
cx     1  corid(l2)
      iout=0
      ioutiw=0
      ioutZ=0
      
      if(ichk.eq.105) iout=2
      if(corid(l2).eq. ccall) ioutiw=iw
      
cxc     write(nlog,*) ' ResRp; ', iout ioutiw, ccall, corid(l2)
cx      if(iout.eq.2 .and. ioutiw.eq.iw) then
cx        ioutZ=1
cx        write(nlog,*) ' '
cx        write(nlog,*) ' ___________________________'
cx        write(nlog,*) ' ResRpl; l2 = ', l2
cx        write(nlog,*) ' ResRpl; ',iyrmo(mon),xmonam(mon), idy,iwx
cx      else
cx        ioutZ=0
cx      endif                
      

c grb dummy variable iw initialization
      iw=iw
      cwrid=corid(l2)
c
      divactx=0.0
      ACTACF=0.0
      
      divact=0.0      
      resavl=-1.0
      divreq1=-1.0
      tarcon=-1.0
      stoalo=-1.0
      stocfs=0.0
      ritrem1=-1.0/fac
      pavail=-1.0/fac
      
      
      cdestyp='N/A'
      ccarry='No'
      cpuse='No'
      cstaid1='N/A'
      cwhy='N/A'
      imcdx=-1
      iuse2x=-1
      lr=l2
      iwhy=0
      
c
c rrb 98/08/10; Convergence Update
      small=0.001
c
c rrb 98/03/03; Daily capability
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif
c
c rrb 02/10/22; Allowm monthly on/off switch
      if(imonsw(l2,mon).eq.0) then
        iwhy=1
        cwhy='Monthly on off switch is off'
        goto 120
      endif  
c
c _________________________________________________________
c
c        Step 1; Set Source Data

      NR  =IOPSOU(1,L2)
C
      IF(IRESSW(NR).EQ.0) then
        iwhy=1
        cwhy='Source Reservoir is off'
        Goto 120
      endif  
C
      IOWN=NOWNER(NR)+IOPSOU(2,L2)-1
      IPCD=IRSSTA(NR)
      NDNP=NDNNOD(IPCD)
c
c ---------------------------------------------------------
c rrb 2006/10/02; Source Reservoir Roundoff check; cin=0 Into
c     write(nlog,*) ' Resrpl Source In; nr, cWrID', nr, cWrID
      in=0
      isub=5
      call chekres(io99,maxres, in, isub, iyr, mon, nr, nowner,
     1                    curown,cursto,cresid)
      
c
c _________________________________________________________
c
c        Step 2; Set Destination reservoir info; 
c           res. code, associated owner and station code
C
      nrd  =iopdes(1,L2)
      cdestyp='Reservoir   '
C
      if(iressw(nrd).eq.0) then
        iwhy=2
        cwhy='Destination Reservoir is off'
        goto 120
      endif  
C
      ircd  = irssta(nrd)
      ndnr  = ndnnod(ircd)   
c
c ---------------------------------------------------------
c rrb 2006/09/25; Allow multiple accounts - Initialize
cr    iownd = nowner(nrd)+iopdes(2,L2)-1
      nro=1
      if(iopdes(2,l2).lt.0) then
        nro=-iopdes(2,l2)
        iownd=nowner(nrd)
      endif

      if(iopdes(2,l2).gt.0) then
        nro=1
        iownd=nowner(nrd)+iopdes(2,l2)-1
      endif
c
c ---------------------------------------------------------
c rrb 2006/10/02; Destination Roundoff check; cin=0 Into
c     write(nlog,*) ' Resrpl Destination In; nrd, cWrID', nrd, cWrID
      in=0
      isub=5
      call chekres(io99,maxres, in, isub, iyr, mon, nrd, nowner,
     1                    curown,cursto,cresid)
                                            
c
c _________________________________________________________
c
c      Step 3. Allowable replacement
C
C------  FIND ALLOWABLE REPLACEMENT FROM CURRENT OWNERSHIP
C
      RPLALO=AMIN1(CUROWN(IOWN),CURSTO(NR)-VOLMIN(NR))
      RPLALO=AMAX1(0.,RPLALO)
c
c rrb 98/03/03; Daily capability
c     ALOCFS=RPLALO/f
      ALOCFS=RPLALO/fac
      if(alocfs.lt.small) then
        iwhy=3
        cwhy='Destination storage available (alocfs) = zero'
        goto 120
      endif  
c
c ---------------------------------------------------------        
      if((FLOMAX(NR)-RIVER(IPCD)).lt.small) then
        iwhy=4
        cwhy='Maximum reservoir release = zero'
        goto 120
      endif  
c
      ALOCFS=AMIN1(FLOMAX(NR)-RIVER(IPCD),ALOCFS)
      alocfs = amax1(alocfs, 0.0)
c
c _________________________________________________________
c
c      Step 4; Check available flow by putting release
c		into a temp array, then checking
c		downstream of the destination
C
C------  FILL THE TEMPORARY AVAILABLE FLOW
C
      ISS=ircd
      DO 100 IS=1,NDNr
        AVTEMP(ISS)=AVAIL(ISS)
c
c rrb 2021/03/20; Compiler Update
cx  100 ISS=IDNCOD(ISS)
        ISS=IDNCOD(ISS)
  100 continue
C
      ISS=IPCD
      DO 110 IS=1,NDNP
        AVTEMP(ISS)=AVAIL(ISS)+ALOCFS
c
c rrb 2021/03/20; Compiler Update
cx  110 ISS=IDNCOD(ISS)
        ISS=IDNCOD(ISS)
  110 continue
C
C------  Find minimum flow available to the destination
c
c rrb 2021/05/02; Runtime error tracking
cx    CALL DNMFSO(maxsta, avtemp, idncod, ircd, ndnr, imcd)
      CALL DNMFSO2(maxsta,avtemp, idncod, ircd, ndnr, imcd,cCallBy)
      imcdX=imcd
C                                      
c
c     PAVAIL=AVTEMP(IMCD)
      PAVAIL=amin1(AVTEMP(IMCD),alocfs)
c     if(pavail.le.0.01) goto 120
      if(pavail.le.small) then
        iwhy=5
        cwhy='Available flow (pavail) = 0'
        goto 120
      endif  
c
c
c _________________________________________________________
c
c
c      Step 5; Calculate demand at destination
C
C------  CHECK AVAILABLE WATER AT CURRENT STATION
C
c     IF(AVAIL(IRCD).LT.0.00001) Goto 120
      avail1=avail(ircd)
      IF(AVAIL(IRCD).LT.small) then
        iwhy=6
        cwhy='Available flow at destination (avail1) = 0'
        Goto 120
      endif
C
C------  FIND REMAINING RESERVOIR CAPACITY
C
      CAPREM=VOLMAX(NRd)-CURSTO(NRd)
C
C------  FIND THE ALLOWABLE STORAGE
C grb 12-04-94  also include check to see max target not exceeded
      tarcon=tarmax(nrd)-cursto(nrd)
c
c ---------------------------------------------------------
c rrb 2006/09/25; Allow multiple accounts - Demand
      cursa=0.0
      do n=1, nro
        n1=iownd+n-1
        cursa=cursa+(ownmax(n1)-curown(n1))
      end do  
      stoalo=amin1(caprem,tarcon,cursa)
c
c rrb 2008/12/15; Additional output information      
      stoalo=amax1(stoalo,0.0)
      stocfs=stoalo/fac      
      if(stocfs.le.small) then  
        iwhy = 7
        cwhy = ' Reamining Dest Capacity or target (stoalo) = zero' 
        goto 120
      endif
c _________________________________________________________
c
c
c grb 97/10/16; Add logic to consider a water right in the exchange
c rrb 2008/12/15; Revise to recognize additional uses of iopdesr
c		    Note iopdesr = 2 for a reservoir and = 12 for a 
c		    reservoir right
cx    if(iopdesr(l2).gt.0) then
      if(iopdesr(l2).eq.12) then
        iwatrig=iopdesr(l2)
        stoalo=amin1(stoalo,ritrem(iwatrig))
        ritrem1=ritrem(iwatrig)
      endif
      stoalo=amax1(stoalo, 0.0)
C
C------  CONVERT AF TO CFS
c
c rrb 98/03/03; Daily capability
c     stocfs=stoalo/f
      stocfs=stoalo/fac
      if(stocfs.le.small) then  
        iwhy = 8
        cwhy = ' Remaining Water Right (ritrem1) = zero' 
        goto 120
      endif
c
c _________________________________________________________
c
c
c       Step 6; Calculate allowable exchange
      stocfs = amin1(stocfs, pavail)
      stocfs = amax1(stocfs, 0.0)
c
c
c _________________________________________________________
c
c      Step 7; Remove diversion (amount stored)
      AVAILR=AVAIL(IPCD)

      CALL TAKOUT(maxsta, avail ,river ,avinp ,qtribu,idncod,
     1            stocfs, ndnr,  ircd)
c
c _________________________________________________________
c
c      Step 8; Add in replacement (amount released)

      repact=-stocfs
      call takout(maxsta, avail ,river ,avinp ,qtribu,idncod,
     1            repact, ndnp,  ipcd)
      AVAIL(IPCD)=AVAILR

c
c _________________________________________________________
c
c
c      Step 9; Update replacement reservoir info
c
c rrb 98/03/03; Daily capability
c     ACTACF=stocfs*f
      ACTACF=stocfs*fac
      accr(21,iown) = accr(21,iown) + actacf
      qres(21,nr)   = qres(21,nr)   + actacf
c
      cursto(nr  )=cursto(nr  )     - actacf
      replac(nr  )=replac(nr  )     + repact
c
      curown(iown)=curown(iown)     - actacf
c
c rrb 2020-04-03; Add qdiv(38 to .xdd reporting to include
c                 From Storage to River for Exchange in column
c                 Carried, Exchange, Other
       qdiv(38,ipcd) = qdiv(38,ipcd) + stocfs
c
c rrb 2020-04-03; Correct *.xre reporting  to both
c                 From Storage to River for Use and
c                 From Storage to River for Exchange only
cx      qres(9,nr)=qres(9,nr)+actacf
cxc
cxc rrb 04/20/96; problem with *.xre
cxc     accr(9,iown)=qres(9,iown)+actacf
cx      accr(9,iown)=accr(9,iown)+actacf
c
c _________________________________________________________
c
c
c      Step 10; Update destination reservoir info 
c
cr    accr(3,iownd) = accr(3,iownd) + actacf
      qres(3,nrd )  = qres(3,nrd)   + actacf

      cursto(nrd )=cursto(nrd )     + actacf
cr    curown(iownd)=curown(iownd)   + actacf
c
c ---------------------------------------------------------
c rrb 2006/09/25; Allow multiple accounts - Allocate
c		  Note:
c		   iResT1=0 distributes based on ownership ratio
c		   nrown1=number of accounts in this reservoir
c		   iown = first account associated with this reservoir  
c		   icx  = subrouine calling accou.for       
c		   ia   = account to adjust
      
      nrX=nrd
      iResT1=0
      nrown1=nro
      iownX=iownd
      icx=105
      ia=3
      cresid1=cresid(nrX)
c        
      call accou(maxacc, maxown, nrX, ownmon, curown, accr, ia, 
     1    ownmax, iownX, nrown1, cursa, actacf, iResT1, icx, cresid1)
      
c
c _________________________________________________________
c		
 120  divactx=stocfs
c
c rrb 2006/05/02; Convergence 
      if(divactx.le.small) divactx=0.0
 
      divo(l2) = divo(l2) + stocfs
c
c _________________________________________________________
c		Detailed header      
      if(iout.eq.99 .and. divactX.lt.small) iout=98
      if((iout.eq.2 .and. iw.eq.ioutiw) .or. iout.ge.99) then      
        ncallX=ncallX+1
        if(ncallX.eq.1) then
          write(nlog,270) corid(l2), cdestyp, ccarry, cpuse          
        endif  
  
        write(nlog,280) ' ResRpl     ', iyrmo(mon),xmonam(mon), idy,
     1    cstaid1,iwx, iw,nwrord(1,iw),l2,lr, Nrd,iuse2x,imcdX, 
     1    StoAlo,  RplAlo, tarcon,pavail*fac, ritrem1*fac,
     1    divactX*fac, iwhy, cwhy
      endif
     
  270   format(/, 
     1  ' ResRpl (Type 6); Operation Right ID = ', a12,
     1  ' Destination Type = ', a12,
     1  ' Carrier (Y/N) = ',a3, ' Reuse (Y/N) = ', a3/    
     
     1  ' RsrSpu       iyr  mon  day ID           ',
     1  '    Iter     Iw  nwrord      l2      lr    Nrd  iuse2X', 
     1  '   imcdX  StoAlo  RplAlo  TarCon  Pavail RitRem1  Divact',
     1  '    iwhy Comment',/
     1  ' ___________ ____ ____ ____ ____________', 
     1  ' _______ _______ _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______ _______ _______',
     1  ' __________________________')
     
  280 FORMAT(a12, i5,1x,a4, i5, 1x, a12,8i8,6F8.0,i8, 1x, a50)
      

c
c ---------------------------------------------------------
c rrb 99/05/10; Source Roundoff check cin=1 Into

c     write(nlog,*) ' Resrpl Source Out; nr, cWrID ',
c    1  nr, cWrID, ACTACF
      in=1
      isub=5
      call chekres(io99,maxres, in, isub, iyr, mon, nr,nowner,
     1                    curown,cursto,cresid)
     
c
c ---------------------------------------------------------
c     write(nlog,*) ' Resrpl Destination Out; nrd, cWrID ',
c    1  nrd, cWrID, ACTACF
      in=1
      isub=5
      call chekres(io99,maxres, in, isub, iyr, mon, nrd,nowner,
     1                    curown,cursto,cresid)
c
c _________________________________________________________
c

      RETURN        
      END



