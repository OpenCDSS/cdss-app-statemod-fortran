c divimpr - Type 35 operating rule, which handles diversions from a diversion (import) to a
c           diversion, reservoir, or carrier with reuse
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
      subroutine DivImpR(iw,l2,ishort,divact,ncallX)
c
c _________________________________________________________
c	Program Description
c
c
c       DivImpR; Type 35
c       It simulates imports that are provided as a source
c           to the operating rule as a negative diversion (import)
c           whose ID is also a type 7 (import) plan. 
c           It has code to deliver water to a diversion, reservoir,
c           carrier or plan with reuse.  However as of version 
c           16.00.44 subroutine Oprinp.f requires the destination
c           be a type 11 administration plan.
c
c           Called by Execut
c _________________________________________________________
c
c 	Update history
c
c rrb 2020/11/20; Version 16.00.44
c                 Revised DivImpr (type 35) to increment qdiv(30',/
c                   "From River by Other" but is not summed in the
c                   water budget (*.xwb) in Step 14-d

c rrb 2020/11/15; Version 16.00.44
c                 Revised DivImpr (type 35) to increment qdiv(31',/
c                   "From River by Other" in Step 14-d and other 
c                   miscellaneous edits
c
c rrb 2020/10/27; Version 16.00.44
c                 Revised DivImpr (type 35) to not increment qdiv(31',/
c                   to help resolve double accounting of Divert',/
c                   in water budget (*.xwb)',/ 
c
c
c rrb 2020/07/28; Version 16.00.38  
c                 Added detailed output when iout=1
c                 Revised to remove references
c                 to iuse when the source is a plan (iopdesr=7)
c
c	rrb 05/03/31	Copied Divres
c			            Removed ability to release for depletion	only
c                 Removed ability to be called by replacement res
c			            Removed stuff related to a transfer limit 
c			            Removed references to transmtn (IRTURN(IUSE).EQ.4)
c			            Added Reservoir Reuse Plan as a source 2 option
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
c           icx   = subroutine call # (35)
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
c	          intern(  )   = If > 0 carrier system with intervening
c                          structures
c
c           qdiv(18        Carried, Exchange or Bypass
c                            Carrier passing thru a structure.
c           qdiv(20        From Carrier by Storage or Exchange 
c           qdiv(28,       Stored via a reuse plan  
c           qdiv(31,       Diversion from ReUse plan to a Res or 
c                            Diversion
c           qdiv(35         Water with a Reuse or Admin plan source 
c                            tracked at the destination & reported as
c                            from Plan in water balanace (OutTbl2)

c           qres(4,ix)   = From Carrier by Storage to Reservoir
c           qres(26,ix)  = Reservoir Storage to River
c           qres(11,ix)  = Reservoir Storage to Carrier
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
      character cwhy*24, cdestyp*12, ccarry*3, cresid1*12, subtypX*8
c
c rrb 2018/08/05; Update
      character cpuse*3, cSouTyp*12, cplntyp*12
      
c
c _________________________________________________________
c               Step 1; Initialize
c
      subtypX='divimpr'
      
      iout=0
      if(ichk.eq.135) iout=2
      if(corid(l2).eq. ccall) ioutiw=iw
c
c rrb 2020/07/28; Detailed output         
      if(iout.eq.1) then
        write(nlog,*) ' '
        write(nlog,102) corid(l2), iout, ioutiw, iw

 102    format(/, 72('_'),/ 
     1  '  DivImpR; ID = ', a12, 5i5)        
      endif    
c      
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
c rrb 2018/03/05; Correction
      cpuse='No'
      ipUse=ireuse(l2)
      if(ipUse.gt.0) cpuse='Yes'
      
c
c rrb 00/12/26; Variable efficiency consideration
      ieff2=1     
c
c rrb 01/01/17; Call number
c rrb 2018/03/05; Correction
cx    icx=32
      icx=35
cr    write(nlog,*) '  DivImpR iwx', iwx
c
c		Step 1b; Check avail array
      call chekava(2, maxsta, numsta, avail, subtypX)

c
c rrb 2020/07/28; Include source type in detailed reporting
      cplntyp='NA          '
      cSouTyp='NA'
      iopSouR1 = iopSouR(l2)
      if(iopSouR1.eq.3)  cSouTyp='Diversion'
      if(iopSouR1.eq.13) cSouTyp='Diversion_WR'
      if(iopSouR1.eq.12) cSouTyp='Diversion_WR'
      if(iopSouR1.eq.11) cSouTyp='Admin  Plan  '
      if(iopSouR1.eq.7)  cSouTyp='Import Plan  '
c
c rrb 2020/11/15; Include destination type
      iopDesR1 = iopDesR(l2)
      if(iopDesR1.eq.3)  cDesTyp='Diversion'
      if(iopDesR1.eq.2 ) cDesTyp='Reservoir'
      if(iopDesR1.eq.7)  cDesTyp='Plan  '
c
c rrb 2020/07/28; Detailed output               
      if(iout.eq.1) then
        write(nlog,*)'  DivImpr; cSouTyp = ', cSouTyp
        write(nlog,*)'  DivImpr; cDesTyp = ', cDesTyp
      endif
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
c                       Note Oprinp.f checks and requires the source
c                       be a type 7 plan
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
c               5-a. Destination is a reservoir (nd<0 & iresw=1)
      if(nd.lt.0) then
        cdestyp='Reservoir'        
        iresw=1
        nd=-nd
        idcd=irssta(nd)        
        NDND=NDNNOD(IDCD)
c
c ---------------------------------------------------------
c rrb 2006/09/25; Allow multiple accounts - Initialize
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
c jhb 2014/08 add logic for destination = plan type 11 (accounting plan)
c     nd is the index of the destination structure in the list of structures
c       but to figure out which type it is takes some work as follows:
c     destination is a reservoir, iopdesr(l2) = 2
c       nd < 0 when destination is a reservoir id that is found in the list of reservoirs
c       nd = -(reservoir index)
c     destination is a diversion, iopdesr(l2) = 3
c       nd > 0 when destination is a diversion id that is found in the list of diversions
c       nd = diversion index
c     destination is a plan, iopdesr(l2) = 7
c       nd > 0, when destination is a plan id that is found in the list of plans
c       nd = plan index
c ---------------------------------------------------------
c
c               5-b Destination is a plan or diversion      
      if(nd.gt.0) then
c
c ---------------------------------------------------------
c               
c               5-c Destination is a plan or diversion
        if(iopdesr(l2).eq.7) then
c         
          cdestyp='Plan     '
          iresw=0
          idcd=ipsta(nd)
          NDND=NDNNOD(IDCD)
c
c rrb 2020/07/28; A plan does not have multiple users
cx        IUSE=NDUSER(ND)+IOPDES(2,l2)-1
c
c rrb 2018/03/02; Control output
cx        write(nlog,*) '  DivImpR; destination is plan!'
          if(iout.eq.1) write(nlog,*)'  DivImpR; destination is plan!'
c         check that the destination plan is on
          if(pOn(nd).le.small) then
            iwhy=5
            cwhy='Destination Plan is Off'
            goto 330
          endif
        else
c ---------------------------------------------------------
c               5-d Destination is diversion
c                   (nd>0 & iopdesr(l2)=3), set iresw=0
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
      endif
c
c rrb 2020/07/28; Additional output
      if(iout.eq.1) write(nlog,*)'  DivImpr; cDesTyp = ', cDesTyp
c
c ---------------------------------------------------------
c               5-e. Carrier system data 
c jhb 2014/08 obviously, ityopr(l2) = 35, so...
c 120  if(ityopr(l2).ne.10) then
 120   continue
cr    if(intern(l2,1).eq.0) go to 130
      if(intern(l2,1).gt.0) then
        ccarry='Yes'
        ndr=intern(l2,1)

        idcd=idvsta(ndr)  
        ndnd=ndnnod(idcd)
c       write(nlog,*) '  DivImpR; l2, idcd', l2, idcd
cr      go to 140
      endif  
c
c ---------------------------------------------------------
c               5-f. Check printout 
c
c rrb 2018/03/05; Correction
      if(iout.ge.1 .and. iw.eq.ioutiw) then      
        ncallX=ncallX+1
        if(ncallX.eq.1) then
          write(nlog,270) corid(l2),cdestyp, cSouTyp, ccarry, cpuse,
     1                    cplntyp
        else
cx        write(nlog,*) ' '
        endif 
      endif 
cx    
cx       end of updates

c ---------------------------------------------------------
c jhb 2014/08 add logic for destination = plan type 11 (accounting plan)
c             check for iopdesr(l2) = 7
c ---------------------------------------------------------
c
c            **  Step 6; Set demand (DIVALO) when the destination
c                        is a diversion
c jhb 2014/08            or a plan
c        
      if(iresw.eq.0) then
c
c ---------------------------------------------------------
c               6-a Destinaiton is a plan
   
        if(iopdesr(l2).eq.7) then
c ---------------------------------------------------------
c          from DirectBy for a plan destination
c            note that "np2" there is the same as "nd" here
c ---------------------------------------------------------
c          idcd2=ipsta(np2)
c          idcd2X=idcd2
c          idcd2P=idcd2
c          ndns2=ndnnod(idcd2)
c          ndns2X=ndns2
c          imcdX=idcd2
c          if(iplntyp(np2) .ne. 11) then
c            divreqx2=amax1(0.0, pdem(np2))
c            np11=0
c          else
c            divreqx2=99999./fac
c            np11=1
c          endif
c          if(iout.eq.1) write(Nlog,*)'  DivimpR; np2', pdem(np2)*fac,
c     1      pdemT(np2)*fac, pdem(2)*fac, pdemT(2)*fac
c ---------------------------------------------------------
c         essentially, the above says treat a type 11 as if it has infinite demand,
c         so that it can take all the water delivered
c         do the same here since it (currently) MUST be a type 11 plan
c ---------------------------------------------------------
          divreqx2=99999./fac
c          pdem1A=pdem(nd)
c          DIVALO=pdem(nd)
          DIVALO=divreqx2
        else
c ---------------------------------------------------------
c               6-b. Destination is a Diversion
          DIVALO=AMIN1(DIVREQ(IUSE),DIVCAP(ND)-DIVMON(ND))
          divcapX=DIVCAP(ND)-DIVMON(ND)
          divmonX=divMon(nd)
c
c            Adjust based on release type
c		  Set based on release type
c		  ireltyp=0 demand
c		  ireltyp>0
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
c rrb 01/08/23; Exit if no demand
          if(divalo.le.small) then
            iwhy=6
            cwhy='Demand is zero'
            goto 330
          endif
        endif
      endif
c 
c
c ---------------------------------------------------------
c
c               6-c. Destination is a Reservoir
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
c               Step 7; Carrier provided.
c                       Limit release (ALOCFS) to capacity of
c                       intervening structures
cr    if(ityopr(l2).ne.10) then
      do 150 i61=1,10
        if (intern(l2,i61).eq.0) go to 160
        intvn=intern(l2,i61)
        alocfs=amin1(alocfs,(divcap(intvn)-divmon(intvn)))
  150 continue
      alocfs=amax1(0.0,alocfs)
  160 continue
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
c rrb 2020/11/22; Detailed output         
      if(iout.eq.1) then     
        write(nlog,*) '  DivImpr; iyrmo(mon),xmonam(mon),idy, ',
     1              'cstaid(iscd), iscd, availr*fac, avail(iscd)*fac'
        write(nlog,*) '  DivImpr;', iyrmo(mon),xmonam(mon),idy, 
     1               cstaid(iscd), iscd, availr*fac, avail(iscd)*fac
      endif
c
c _________________________________________________________
c
c               Step 11 Remove diversion (DIVACT) from stream

      AVAILR=AVAIL(idcd)
      CALL TAKOUT(maxsta,AVAIL,RIVER,AVINP,QTRIBU,IDNCOD,
     1            DIVACT,NDND,IDCD)
c
c rrb 2020/11/22; Detailed output         
      if(iout.eq.1) then     
        write(nlog,*) ' '
        write(nlog,*) '  DivImpR; idcd, divact, availR, avail(idcd)'
        write(nlog,*) '  DivImpR;',idcd, divact*fac,
     1                   availR*fac, avail(idcd)*fac
      endif
c
c _________________________________________________________
c
c               Step 12; Add return flows to stream

      if (iresw.eq.0 .and. ipUse.eq.0) then
c jhb 2014/08 put stub of code in place for plan destinations
        if(iopdesr(l2).eq.7) then
c         don't have return flows from a type 11 plan...
        else
c         it's a diversion, so it has return flows...
          call rtnsec(icx,divact,l2,iuse,IDCD,nd,ieff2)
        endif
      endif
c
c _________________________________________________________

c		            Step 13; Calculate reuse
c jhb 2014/08 note that ipUse should equal 0 if NA is entered
c             in the reuse plan field when the destination is a type 11 plan
      if(ipUse.gt.0) then
c
c ---------------------------------------------------------
c               13a Diversion reuse
        if(nd.gt.0) then            
          CALL RtnsecR(icx,divact,l2,iuse,idcd,nd,
     1         ieff2,ipUse)
c
c rrb 04/12/30; Qdiv(28 is the carried / exchanged water
c		Note works outside river system
          ipsta1=ipsta(ipUse)
c
c rrb 2011/11/15; qdiv(28 is not used since 2010
cx        qdiv(28,ipsta1) = psuplyT(ipUse)     
        else
c
c
c ---------------------------------------------------------
c		            13-b Reservoir Reuse          
cr        ircp=ipsta(ipUse)
          psuply(ipUse)=psuply(ipUse)+divact
          psuplyT(ipUse)=psuplyT(ipUse)+divact
c
c rrb 2006/01/01; Correction
          if(iplntyp(ipuse).eq.3 .or. iplntyp(ipuse).eq.5) then
            psto2(ipUse)=psto2(ipUse)+divact*fac          
          endif  
          
          ipsta1=ipsta(ipUse)
c
c rrb 2011/11/15; qdiv(28 is not used since 2010
cx          qdiv(28,ipsta1) = psuplyT(ipUse)                 
        endif  
      endif
c
c _________________________________________________________
c
c               Step 14; Update storage, demand, etc.
c
c ---------------------------------------------------------
c               14-a. Destination is a reservoir, update storage data
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
c               Endif for a Reservoir Destination
      endif
c
c ---------------------------------------------------------
c               14-b. Destination is a diversion or Plan update
c
      if(iresw.eq.0) then
c jhb 2014/08 put stub of code in place for plan destinations
c
c ---------------------------------------------------------
c               14-b1. Destination is a plan Update
        if(iopdesr(l2).eq.7) then
c
c rrb 2020/07/28; A plan (iopdesr=7) does not have a use
cx        USEMON(IUSE)=USEMON(IUSE)+DIVACT
          if(iout.eq.1) write(nlog,*) '  DivimpR; Update Plan Data'
          call flush(nlog)
          psuply(nd)=psuply(nd) + divact
          psuplyT(nd)=psuplyT(nd) + divact
        else
c
c ---------------------------------------------------------
c               14-b2. Destination is a diversion Update
          USEMON(IUSE)=USEMON(IUSE)+DIVACT
          DIVREQ(IUSE)=DIVREQ(IUSE)-DIVACT
          DIVMON(ND  )=DIVMON(ND  )+DIVACT
        endif
c
c rrb 2020/11/15; Add endif for reservoir update
      endif
c
c ---------------------------------------------------------
c               14-c. Destinaion has a Carrier update
      if(intern(l2,1).ne.0) then
        inode=idvsta(nd)
        qdiv(20,inode)=qdiv(20,inode)+divact               
      endif                                             
c
c ---------------------------------------------------------
c               14-d. Destination is a reservoir, diversion or plan
c                     update qdiv(31 From "River by Other"
c rrb 2020/10/27; Revised DivImpr (type 35) to not increment qdiv(31
c                 to help resolve double accounting of Divert',/
c                 in water budget (*.xwb)',/ 
c rrb 2020/11/15; Revise Divimpr (type 35) to increment qdiv(31
c                 "From River by Other" to resolve mass balance
cx        qdiv(31,idcd)=qdiv(31,idcd)+divact
c rrb 2020/11/20; Revise Divimpr (type 35) to increment qdiv(30
c                 "From River by Other" in *.xdd but not summed
c                 in *.xwb
      qdiv(31,idcd)=qdiv(31,idcd)+divact
cx    qdiv(30,idcd)=qdiv(30,idcd)+divact
c
c rrb 2020/11/15; Move endif for a reservoir to step 14-b
cx    endif
c
c ---------------------------------------------------------
c               14-e. Carrier adjust carrier thru a structure 
c                     Qdiv(18
      do i11=1,10
        if (intern(l2,i11).eq.0) go to 282
        intvn=intern(l2,i11)
        divmon(intvn)=divmon(intvn)+divact
        inode=idvsta(INTVN)
        qdiv(18,inode)=qdiv(18,inode)+divact
      end do  
  282 continue
c
c ---------------------------------------------------------
c               14-f. Source plan Update
      PImport(np)=amax1(0.0, PImport(np)-divact)
      PImportT(np)=PImportT(np)+divact
c
c rrb 2006/01/01; Correction for a reservoir plan
      if(iplntyp(np).eq.3 .or. iplntyp(np).eq.5) then
        psto2(np)=amax1(psto2(np)-divact*fac, 0.0)                
      endif  
c
c rrb 2018/03/05; Set qdiv(35 and qdiv(18 where:
c               qdiv(35  From Plan for water balance reporting
c               qdiv(18  Carried, Exchange or Bypass (Carrier
c                        passing thru a structure. 

      qdiv(35,iscd)=qdiv(35,iscd)+divact
cx      
cx      end of update
c
      qdiv(18,iscd)=qdiv(18,iscd)+divact
c
c _________________________________________________________
c               
c               Step 15;  Update shortage and opr rule results
c ---------------------------------------------------------
c               15-a Set shortage switch (ishort)
  330 if((divact+small) .lt. divalo) ishort=1
c  
c ---------------------------------------------------------
c               15-b. Update operating results (divo)
      divo(l2)=divo(l2)+divact
c
c _________________________________________________________
c               
c               Step 16; Check results
c
c ---------------------------------------------------------
c               16-a. Check that Avail flow > 0
      call chekava(2, maxsta, numsta, avail, subtypX)
c
c ---------------------------------------------------------
c               16-b. Detailed Check
c rrb 2018/02/05; correction
cx    if(iout.eq.2) then
      if(iout.ge.1 .and. iw.eq.ioutiw) then  
c
c
c ---------------------------------------------------------
c               16-c. Print detail header 
c rrb 2020/07/19; Update to include header
        ncallX=ncallX+1
        if(ncallX.eq.1)then
c
c rrb 2020/07/27; Print more info to detailed check
cx        write(nlog,270) corid(l2), cdestyp, ccarry, cpuse    
          write(nlog,270) corid(l2),cdestyp, cSouTyp, ccarry, cpuse,
     1                    cplntyp                
        else
c          write(nlog,*) ' '
        endif  
c
c ---------------------------------------------------------
c               16-d. Print detailed results 
c                          
        write(nlog, 280)  ' DivImpR    ',
     1   iyrmo(mon),xmonam(mon),idy, cstaid(iscd),
     1   iwx, iw,nwrord(1,iw),l2,ns, ND,iuse,ipUse,
     1     float(iopsou(6,l2)), effmax1, float(ireltyp), 
     1     divreqX*fac,divcapX, diwrreqX*fac, 
     1     divmax*fac, relalo*fac, 
     1     divmonX*fac, divact*fac, iwhy, cwhy
      endif
c
c _________________________________________________________
c
c               Step 17; Return
      RETURN

c               Formats
  270   format(/, 
     1  '  DivImpR (Type 35); Operation Right ID = ', a12,
     1  ' Destination Type = ', a12, ' Source Type = ', a12,
     1  ' Carrier (Y/N) = ',a3, ' Plan (Y/N) = ', a3, 
     1  ' Plan Type = ', a12,/
cx     1    ' DivImpR (Type 35) Destination Type = ', a12,
cx     1    1x,a3,' Carrier'/
cx     1    ' Carrier (Y/N) = ',a3, ' Reuse Plan (Y/N) = ', a3/      
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
  280   FORMAT(a12, i5,1x,a4, i5, 1x, a12,
     1   8i8,10F8.1,i8,1x, a24)
     
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

