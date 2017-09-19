c
C     Last change:  RRB  22 Oct 2002    4:12 pm
c
c
      subroutine divresP(iw,l2,ishort,divact,ncallX)
c
c
c _________________________________________________________
c	Program Description
c
c     DivresP; Type 27 Rule: 
c              ReUse plan to a diversion or reservoir 
c              By a direct release via river or carrier
c	        Note can release to meet a diversion or a depletion
c
c           	Called by Execut
c _________________________________________________________
c
c	Update history
c rrb04/12/30; Copied Divres and edited accordingly
c ________________________________________________________
c	Documentation
c
c           divalo = demand
c           relalo = available supply in plan (acft)
c           alocfs = available supply in plan (cfs)
c           divact = actual amount diverted
c           relace = actual amount released from the plan
c
c           icx   = subroutine call #
c
c           ieff2 = 0 always use average efficiency
c           ieff2 = 1 use max efficiency if ieffmax=1
c           iopsou(1,l2) = source plan #1 
c           iopsou(2,l2) = source reservoir #1 account
c	          iopsou(3,l2) = exchange point
c
c           iopdes(1,l2) nd if > 0 destination diversion ID 
c           iopdes(2,l2)    destination owner 
c
c           iopdes(1,l2) nr if < 0 destination reservoir ID 
c           iopdes(2,l2) iuse or irow destination reservoir account
c
c           icu           Diversion type
c                         = 0 release to meet diversion demand
c                         = 1 release to meet depletion
c
c
c           iopsou(5,l2) = Not used
c           iopsou(6,l2) = plan release type and efficiency
c                          if = 0 release to meet demand
c                          if > 0 release only if a CIR (IWR) exists
c                                 and limit release to not exceed
c                                 IWR/n,  
c                          Note nmax = min(nmax, effmax) to save 
c                                 iterating
c           ireltyp      = Same as iopsou(6,l2)
c
c	          ipUse        = Destination Reuse plan
c                          Note ipUse=ireuse(l2)
c	          np           = Source reuse plan
c                          Note np  =IOPSOU(1,l2)           
c
c
c           iout         = switch for detailed printout 
c
c           iscd         = source plan river station
c
c           imonsw()     = monthly on off switch  

c           irow         = destination user (account)
c
c           ires         = switch 0=diversion, 1=plan
c            
c           l2 - order of the destination diversion
c           l2 - order of operating rule in opr. rule list. Note
c                when called by execute (ityopr=2 or 3); l2=l2
c                when called by replace (ityopr=10) l2 is a array 
c                holder

c
c           nd           = destination (+=diversion, -=plan)  
c           ndnd         = # of downstream nodes for plan #1
c
c           nr           = source plan #1          
c           nra          = source plan #2 
c
c
c           qdiv(18        Carrier passing thru a structure 
c           qdiv(20        From Carrier by Storage, Exchange or Plan
c           qdiv(28        Carried or exchanged water
c	    qdiv(29        Exchange from a plan
c           qdiv(31        Direct Diversion from ReUse plan to a
c                            Reservoir  or Diversion (DivresP)
c           qres(18        From River by Exchange to Reservoir
c           qres(4         From carrier by Storage to reservoir
c ________________________________________________________
c	Dimensions
c
      include 'common.inc'
      character cwhy*24, cdestyp*12, rec12*12, ccarry*3, cpuse*3,
     1 cresid1*12, cTandC*3
c
c _________________________________________________________
c               Step 1; Initilize
c
      iout=0
      ioutiw=0
      
      if(ichk.eq.127) iout=2
      if(corid(l2).eq. ccall) ioutiw=iw
      
      
c               a. Convergence
      small=0.001
c
c               b. Daily capability
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif
c
c               c. Miscellaneous
      divact = 0.0          
      divalo = 0.0
      relact=0.0
      ishort = 0
      iw=iw
      iowna=0
      irep=0
      ioff=0
      ncarry=0
      nd=0
      nd2=0
      ndX=0
      nr=0
      
      iwhy=-1
      divaloX=-1.0/fac
      divcarry=-1.0/fac
      alocfs=-1.0/fac
      alocfs1=-1.0/fac
      divalox=-1.0/fac
      pavail=-1.0/fac
      
      cwhy='N/A'
      pavail=-1.0/fac      
      
      cpuse='No'
      ipUse=ireuse(l2)
      if(ipUse.gt.0) cpuse='Yes'

      cdestyp='NA '
      nd  =iopdes(1,l2)
      if(nd.gt.0) cdestyp='Diversion'
      if(nd.lt.0) cdestyp='Reservoir'
      
      
      ccarry='No'
      if(intern(l2,1).gt.0) ccarry='Yes'
      

c
c rrb 00/12/26; Variable efficiency consideration
      ieff2=1     
      icx=4
      
c
c               d. Check Avail array
      call chekava(17, maxsta, numsta, avail)
c      
c ________________________________________________________
c               Step 2b; Set Reuse Plan pointer
c		ipUse = Reuse plan
      ipTC=iopsou(3,l2)
      if(ipTC.gt.0) cTandC='Yes'
      ipUse=ireuse(l2)
      if(ipUse.gt.0) cpuse='Yes'
c
c _________________________________________________________
c
c		Detailed header      
      if(iout.ge.1 .and. iw.eq.ioutiw) then      
        ncallX=ncallX+1
        if(ncallX.eq.1) then
          write(nlog,270) corid(l2), cdestyp, ccarry, cpuse          
        else
          write(nlog,*) ' '
        endif  
      endif  
      
c
c _________________________________________________________
c               Step 2; Branch if not on this month
c
      if(imonsw(l2,mon).eq.0) then
        iwhy=1
        cwhy='Monthly Switch = '
        goto 300
      endif  
c
c _________________________________________________________
c
c               Step 3; Set source data a plan
c               a. Source plan data
      np  =IOPSOU(1,l2)           
      if(ifix(pon(np)).eq.0) ioff=1
c     write(nlog,*) ' DivresP; np = ', np

      IsCD=ipsta(np)
      ndns=NDNNOD(IsCD)
c
      ALOCFS=psuply(np)
      alocfs = amax1(0.0,alocfs)
      alocfs1= alocfs
            
      if(alocfs.lt.small) then
        iwhy=2
        cwhy='Plan Supply = 0'
        goto 300
      endif
c
c _________________________________________________________
c		Step X. Set CU limit switch      
      rec12=cDivTyp(l2)
      icu=0
      if(rec12(1:9).eq.'Diversion') icu=0
      if(rec12(1:9).eq.'Depletion') icu=1

c
c _________________________________________________________
c               
c               Step 4a; Destination is a diversion
c                        Set demand, etc.
      nd  =iopdes(1,l2)
      nd2 =nd
      ndX=nd
c
      if(nd.gt.0) then
        cdestyp='Diversion'
        iresw=0
        if(idivsw(nd).eq.0) ioff=2
        
        IUSE=NDUSER(ND)+IOPDES(2,l2)-1
        iuseX=iuse
        
        IDCD=IDVSTA(ND)
        idcdd=idcd
        NDND=NDNNOD(IDCD)
        divreq1=divreq(iuse)
        
        DIVALO=AMIN1(DIVREQ(IUSE),DIVCAP(ND)-DIVMON(ND))
c        
c                  If release type code is on
c                  Limit release to occur only if an IWR exists
c                  Note still releasing to meet 100% of demand
c
        if(iout.eq.1) then
          write(io99,*) ' '
          write(io99,*) ' DivresP; iopsou(6,l2)  ', iopsou(6,l2)
          write(io99,*) ' DivresP; effmax(nd)    ', effmax(nd)
          write(io99,*) ' DivresP; diwrreq(iuse) ', diwrreq(iuse)*fac
        endif

        ireltyp=amin0(iopsou(6,l2),ifix(effmax(nd)))
c
        divmax=divreq(iuse)

        if(ireltyp.gt.0) then 
          if(diwrreq(iuse).le.small) then
            divalo=0.0          
          else
            divmax = diwrreq(iuse)/(float(ireltyp)/100.0)
            divalo=amin1(divalo, divmax)
          endif
        endif

        DIVALO=amax1(0.0,divalo)
        divaloX=divalo        
cr
cr		Set supply based on diversion if icu=0 or 
c               depletion if icu=1
        diveff1=diveff(mon,iuse)
cr      if(iopsou(4,l2).ge.0) then
        if(icu.eq.0) then
          alocfs=alocfs
        else  
          alocfs=alocfs/(diveff(mon,iuse)/100.0)              
        endif
        
      endif  
c
c _________________________________________________________
c               
c               Step 4b; Destination is a reservoir
c
      if (nd.lt.0) then
        nr=-nd
        ndX=nr
        cdestyp='Reservoir'
        
        if(iressw(nr).eq.0) ioff=3
        
c     
        idcd=irssta(nr)
        idcdr=idcd
        ndnd=ndnnod(idcd)
c
c ---------------------------------------------------------
c rrb 2006/09/25; Allow multiple accounts - Initilize
cr      irow=nowner(nr)+iopdes(2,l2)-1
cr      iuseX=irow

        nro=1
        if(iopdes(2,l2).lt.0) then
          nro=-iopdes(2,l2)
          irow=nowner(nr)
        endif

        if(iopdes(2,l2).gt.0) then
          irow=nowner(nr)+iopdes(2,l2)-1
          nro=1
        endif
        
        iuseX=irow
c
c ---------------------------------------------------------
c rrb 2006/09/25; Allow multiple accounts - Demand
        cursa=0.0
        do n=1, nro
          n1=irow+n-1
          cursa=cursa+(ownmax(n1)-curown(n1))
        end do         
        
c
c               a. Set demand
cr      divreq1=amin1(ownmax(irow)-curown(irow),
        divreq1=amin1(cursa,
     1                volmax(nr)-cursto(nr),
     1                tarmax(nr)-cursto(nr))/fac
        
        divreq1=amax1(0.0, divreq1)        
        DIVALO=divreq1
        divaloX=divalo
      endif
c
c _____________________________________________________________
c
c               Step 5; Destination is through a carrier
c		         Adjust diversion location
      if(intern(l2,1).gt.0) then
        ncarry=intern(l2,1)
        IDCD=IDVSTA(ncarry)
        idcdc=idcd
        NDND=NDNNOD(IDCD)
        divcarry=divcap(ncarry)-divmon(ncarry)        
      endif
c
c
c _________________________________________________________
c               Step 6. Check Printout on input
      if(iout.ge.1 .and. iw.eq.ioutiw) then                
          write(nlog,280) '  DivresP_In',
     1    iyrmo(mon),xmonam(mon),idy, iwx, iw,l2,ndX,iuseX,icu,
     1    alocfs1*fac, alocfs*fac, DIVREQ1*fac, Pavail*fac, 
     1    divaloX*fac, divcarry*fac,relact*fac, divact*fac, 
     1    iwhy, cwhy
     
     
     
      endif
c
c _________________________________________________________
c
c		Step 7. Exit if off      
      if(ioff.gt.0) then
        iwhy=3
        write(Nlog,*) ' DivresP; ioff = ', ioff
        cwhy='Plan or Div or Res is off'
        goto 300
      endif
c
c _________________________________________________________
c
c               Step 8. Exit if no demand
      IF(divalo.LE.small) then
        iwhy=4
        cwhy='Demand or Capacity =0'
        goto 300
      endif  
c
c _____________________________________________________________
c
c               Step 9; Process carrier limitations
      do i=1,10
        if (intern(l2,i).gt.0) then
          intvn=intern(l2,i)
          if (divalo.gt.(divcap(intvn)-divmon(intvn))) then
            divalo=divcap(intvn)-divmon(intvn)
            divcarry=divalo
c           write(nlog,*) ' DivresP 1; divcarry', divcarry*fac
          endif  
        endif  
      end do       
c
c _________________________________________________________
c
c           **  Step 10; Set diversion (DIVACT) and
c               release (RELACT) to be the minimum of
c               (water in plan (ALOCFS), demand (DIVALO))
c	        and carrier limitations
      if(ncarry.eq.0) then
        divact=amin1(alocfs, divalo)
      else
        divact=amin1(alocfs, divalo, divcarry)
c       write(nlog,*) ' DivresP 2; divcarry', divcarry*fac        
      endif
      divact=amax1(0.0,divact)
c
c
c _____________________________________________________________
c
c               Step 11 - Find minimum exchange potential
c                        in the river from the diversion
c                        node (idcd) to the Exchange point
c                        iopsou(3,l2)
c
      IMCD=Iscd
      ISS=Iscd
c
      DO nx=1,NDNs
        if (iss.eq.iopsou(3,l2)) goto 110
        IF(AVAIL(IMCD).GT.AVAIL(ISS)) IMCD=ISS
        ISS=IDNCOD(ISS)
      end do
c
  110 pavail=amax1(0.0,avail(imcd))
c _________________________________________________________
c
c 		Step 12a; Calculate diversion based on "Diversion"
      if(icu .eq. 0) then

        DIVACT=amin1(divalo,alocfs)
        divact=amax1(0.0,divact)
        divactx=divact
c
cr        
        relact=-divact

        if(iout.eq.1) then
          write(nlog,342) divalo*fac, alocfs*fac, divact*fac
        endif
      
        if(divact.le.small) then
          iwhy=5
          cwhy='Exchange potential = 0'
          goto 300
        endif
      endif  
c
c _________________________________________________________
c
c
c 		Step 12b Calculate diversion based on "Depletion"

      if(icu .eq. 1) then

        DIVACT=amin1(pavail,divalo,alocfs)
        divact=amax1(0.0,divact)
c
c               b. If Available flow < demand
c                  set the release (RELACT) to the difference
c                  (DIVACT - PAVAIL) or the depletion (divact*diveff)
        if(pavail .lt. divact) then
          relact=divact-pavail
          relact=amax1(0.0, relact)       
          relact=amax1(relact,(divact*diveff(mon,iuse)/100.))
          relact=-relact
        else
c               c. If available flow >= demand (pavail>=divact)
c                  set release to the depletion (divact*diveff)
          relact=-1.*(divact*diveff(mon,iuse)/100.)
        endif
c
c               d. If iout=1 print detailed results
        if(iout.eq.1) then
          c = divact*diveff(mon,iuse)/100.0
          write(io99,390) 2, divact*fac, pavail*fac, relact*fac, c*fac
        endif
      endif

      iwhy=0
c
c _________________________________________________________
c
c               Step 13; Add plan release (RELACT) to stream
c		        Note plan was not added to the stream earlier
c
      AVAILR=AVAIL(IsCD)
      call takout(maxsta,avail,river,avinp,qtribu,idncod,
     1            relact,ndns,iscd)
      avail(iscd)=availr     
c
c _________________________________________________________
c
c               Step 14; Remove diversion or storage (DIVACT)

      CALL TAKOUT(maxsta,AVAIL,RIVER,AVINP,QTRIBU,IDNCOD,
     1            DIVACT,NDND,IDCD)
c
c		Step 14b; Add in return flows for a diversion     
c
c rrb 2005/08/02; Allow reuse on return flows
cr    if(nd.gt.0) then
      if(nd.gt.0 .and. ipuse.eq.0) then
        call rtnsec(icx,divact,l2,iuse,IDCD,nd,ieff2)
      endif  
c
c _________________________________________________________

c		Step 14c; Destination ReUse Plan
      if(ipUse.gt.0) then
        if(nd2.gt.0) then            
          CALL RtnsecR(icx,divact,l2,iuse,idcd,nd2,
     1         ieff2,ipUse)
c
c rrb 04/12/30; Qdiv(28 is the carried / exchanged water
c		Note works outside river system
          ipsta1=ipsta(ipUse)
          qdiv(28,ipsta1) = psuplyT(ipUse)     
        else
c
c		Reservoir Reuse          
          psuply(ipUse)=psuply(ipUse)+divact
          psuplyT(ipUse)=psuplyT(ipUse)+divact
c
c rrb 2006/01/01; Correction
          psto2(ipUse)=psto2(ipUse)+divact*fac          
          ipsta1=ipsta(ipUse)
          qdiv(28,ipsta1) = psuplyT(ipUse)                 
        endif  
      endif
c
c _________________________________________________________
c
c               Step 15a; Update destination is a diversion

c
      if(nd.gt.0) then
        USEMON(IUSE)=USEMON(IUSE)+DIVACT
        DIVREQ(IUSE)=DIVREQ(IUSE)-DIVACT
        DIVMON(ND  )=DIVMON(ND  )+DIVACT
        
        if(ncarry.eq.0) then
          qdiv(31,idcdd)=qdiv(31,idcdd)+divact  
        else
c
c rrb 2005/12/07; Revise Note:
c		qdiv(18 (carried exchange or Bypass)
c        	qdiv(20 (from Carrier by Plan)       
c		qdiv(31 (from River by Plan
c         qdiv(20,idcdc)=qdiv(20,idcdc)+divact  
          qdiv(31,idcdc)=qdiv(31,idcdc)+divact  
          qdiv(18,idcdc)=qdiv(18,idcdc)+divact
          
          qdiv(20,idcdd)=qdiv(20,idcdd)+divact
        endif  
      endif  

      
c _________________________________________________________
c
c               Step 15b; Update destination is a reservoir
      if(nd.lt.0) then
        divaf=divact*fac
        cursto(nr)=cursto(nr)+divaf
cr      curown(irow)=curown(irow)+divaf
c
c ---------------------------------------------------------
c rrb 2006/09/25; Allow multiple accounts - Allocate
c		  Note:
c		   iResT1=0 distributes based on ownership ratio
c		   nrown1=number of accounts in this reservoir
c		   iown = first account associated with this reservoir  
c		   icx  = subrouine calling accou.for       
c		   ia   = account to adjust
      
        nrX=nr
        iResT1=0
        nrown1=nro
        iownX=irow
        icx=127
        if(ncarry.eq.0) ia=18
        if(ncarry.ne.0) ia=4
        cresid1=cresid(nrX)
c        
        call accou(maxacc, maxown, nrX, ownmon, curown, accr, ia, 
     1    ownmax, iownX, nrown1, cursa, divaf, iResT1,icx, cresid1)
c          
c ---------------------------------------------------------
cr rrb 05/02/16; Standard treatment of reservoirs from *.xdd
cr               is DO NOT report reservoir diversion in *.xdd
cr		 unless a carrier
        if(ncarry.eq.0) then
          qres(18,nr)=qres(18,nr)+divaf
cr        accr(18,irow)=accr(18,irow)+divaf 
        else        
          qres(4,nr)=qres(4,nr)+divaf
cr        accr(4,irow)=accr(4,irow)+divaf
c
c rrb 2005/12/07; Correction          
c         qdiv(29,idcd)=qdiv(29,idcd)+divact       
c
c rrb 2005/12/07; Do not report reservoir information in *.xdd
c         qdiv(31,idcd)=qdiv(31,idcd)+divact       
          qdiv(18,idcd)=qdiv(18,idcd)+divact
        endif  
c
c ---------------------------------------------------------
c               b. Check reservoir roundoff when exiting routine
        call chekres(nlog, maxres, 1, 19, iyr, mon, nr,nowner,
     1               curown,cursto,cresid)
      endif
c      
c _________________________________________________________
c
c               Step 16; Update Source Plan (note relact is negative)
cr    psuply(np)=amax1(0.0, psuply(np)-divact)
      psuply(np)=amax1(0.0, psuply(np)+relact)
c
c rrb 2006/01/01; Correction for a reservoir plan
      if(iplntyp(np).eq.3 .or. iplntyp(np).eq.5) then
        psto2(np)=amax1(psto2(np)+relact*fac, 0.0)                
      endif  
c      
c _________________________________________________________
c
c               Step 17; Update data to pass out (someday)
      divactx=relact
      divacty=divact
      
c
c _________________________________________________________
c               
c               Step 18;  Update shortage switch (ishort)
c
  300 if(nd.gt.0 .and. divact+small .lt. divalo) ishort=1
c
c _________________________________________________________
c               
c               Step 19; Update operating rule output (DIVO)
c		Note relact is negative
cr    divo(l2)=divo(l2)+divact
      divo(l2)=divo(l2)-relact
c
c _________________________________________________________
c               Step 20; Update carrier structures
c
      do i=1,10
        if (intern(l2,i).gt.0) then          
          intvn=intern(l2,i)
          
          divmon(intvn)=divmon(intvn)+divact
          inode=idvsta(INTVN)
        endif
      end do  
c
c _________________________________________________________
c               
c               Step 21; Check results
c
      call chekava(17, maxsta, numsta, avail)
c
c _________________________________________________________
c
c               Step 22; Detailed output at Exit
c
      if(iout.ge.1 .and. iw.eq.ioutiw) then      
       
          write(nlog,280) '  DivresP_Ou',
     1    iyrmo(mon),xmonam(mon),idy, iwx, iw, l2, ndX,iuseX,icu,
     1    alocfs1*fac, alocfs*fac, DIVREQ1*fac, Pavail*fac, 
     1    divaloX*fac, divcarry*fac,-1*relact*fac, divact*fac, 
     1    iwhy, cwhy
     
       endif
       
c _________________________________________________________
c
c               Step 23; Return
      RETURN
c
c _________________________________________________________
c               Formats
 270    format(/,
     1    '  DivresP (Type 27); Operation Right ID = ', a12,
     1    ' Destination Type = ', a12,
     1    ' Carrier (Y/N) = ',a3, ' Reuse Plan (Y/N) = ', a3/    
     1    '  DivresP     Iyr  Imo  Idy Iter   iw   l2 ndX iuseX  iCU',
     1    ' Alocfs1  Alocfs Divreq1  Pavail',
     1    ' divaloX divCary  RELACT  DIVACT',
     1    '    iwhy cwhy',/
     1    ' ___________ ____ ____ ____ ____ ____ ____ ____ ____ ____', 
     1    ' _______ _______ _______ _______',
     1    ' _______ _______ _______ _______',
     1    ' _______ ________________________')
 280   format(a12, i5, 1x,a4, 7i5, 8f8.1, i8, 1x,a24)
 
 342   format(
     1     '  DivrplP; Diversion Limit;',
     1     '  pavail  divalo  alocfs  divact',/
     1     '                          ', 20f8.2)
 
  380  format(
     1       '  DivresP; Problem with ', a12, 1x, a24,' Type = ', i5)

  390  format(
     1       '  DivresP; Release for Depletion Data;',/
     1       '                  #  divact  pavail  relact      CU',/
     1       '           ', i8, 20f8.2)
c
c               Error warnings
c _________________________________________________________
 9999 write(6,1050) 
      write(99,1051) 
    
 1050 format('    Stopped in DivresP',/,
     1       '    See the *.log file')
 1051 format('    Stopped in DivresP')
      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)


      stop 
      END

