c
C
      SUBROUTINE OopBook(IW,L2,divactx,ncallx)        
c
c		      
c
c _________________________________________________________
c	Program Description
c
c       OopBook; Type 8.
c               It simulates an Out of Priority Bookover
c               It is similar to rsrspu.for
c               Generic routine developed for Blue River Decree
c               between Dillon and Green Mountain
c
c       The type 8 operating rule works in concert with an out-of-
c       priority reservoir right to allow a Junior to store upstream
c       (OOP) storage and subsequent bookover to occur by a junior
c       reservoir with respect to a senior reservoir. When properly
c       applied, water stored by the junior reservoir out of priority
c       is kept in a separate reservoir account, typically called an
c       OOP account. Once the volume of water stored in the OOP
c       account exceeds the remaining capacity of the senior
c       reservoir, water is booked into an active
c       account within the junior reservoir. If the senior
c       reservoir does not fill then a type 2 operating right is
c       typically used to transfer the water from the out of priority
c       reservoir to the senior reservoir.
c
c	1. Water is diverted into an OOP account by an OOP right  
c		by the standard reservoir diversion routine (Resrg1)
c	2. Note Resrg1 limits water diverted to the OOP to not
c	   exceed the junior decree amount	
c	3. If water in the OOP account is greater than the senior
c		decree, this routine (OopBook) books it into an active
c               account and decreases the junior's decree
c	4. If more water is in teh OOP account than is available to 
c		the junior decree spill. Note this should only occurr
c		when hte one fill rule limitations through things out
c		of balance
c
c _________________________________________________________
c       Update History
c
c rrb 98/03/03; Daily Capability added
c
c rrb 02/10/25; Allow monthly on/off switch
c
c rrb 2006/05/22; Move Spill Calculations below transfer calculations
c rrb 2006/06/05; Revise to include paper fill and limit based on
c		  the senior storage; not the senior right
c rrb 2006/06/13; Revise to recognize bookover adjusts a type 9
c                 Out-of-Priority Reservoir Plan
c
c
c
c _________________________________________________________
c       Documentation
c               iout = 0 no detailed printout
c                      1 yes detailed printout
c
c		nr	source reservoir pointer
c		nd      destination reservoir pointer
c		irow    source account
c		nrwr    senior water right (iopsou(3 )
c		ndwr    junior right diverting out of priority
c                       NOT the OOP right, the one diverting OOP
c		resavl  available in reservoir
c		resalo  available in account
c		
c
c
c
c _________________________________________________________
c	Dimensions

      include 'common.inc'
      character cwhy*48, cdestyp*12, ccarry*3, cpuse*3, cstaid1*12,
     1          rec12*12,cSouTyp*12      
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
      
      if(iout.eq.1) write(nlog,*) '  OopBook.for;'      
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
      
      cdestyp='Reservoir'
      cSouTyp='Reservoir'
      ccarry='No'      
      cpuse='No '      
c
c rrb 98/08/10; Convergence Update
      small =0.001
      
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
c _________________________________________________________
c
c               Step 3; FIND THE CODE OF SOURCE NODE (RESERVOIR)
c
      NR  =IOPSOU(1,L2)
      IF(IRESSW(NR).EQ.0) then
        iwhy=2
        cwhy='Source Reservoir is Off'      
        Goto 200
      endif  
      IROW=NOWNER(NR)+IOPSOU(2,L2)-1
      ISCD=IRSSTA(NR)
      ndnr = ndnnod(iscd)
      nrwr=iopsou(3,l2)
      nSenior=iresco(1,nrwr)
      SeniorA=volmax(nSenior) - CURSTO(nSenior)
c
c		Set paper fill to remaining decree
      ritpap1=ritrem(nrwr)
      
c
c _________________________________________________________
c
c               Step 4; FIND THE CODE OF DESTINATION NODE (RESERVOIR)
c
      ND  =IOPDES(1,L2)
      IF(IRESSW(ND).EQ.0) then
        iwhy=3
        cwhy='Destination Resrvoir is Off'      
        Goto 200
      endif  
      
      IDOW=NOWNER(ND)+IOPDES(2,L2)-1
      IDCD=IRSSTA(ND)
      idcd = idcd
      ndwr=intern(l2,1)
c
c rrb 2006/05/23; Used on output only
cr    caprem=2000000.
      if(iout.ge.1 .and. iw.eq.ioutiw) then
        if(NcallX.eq.0) then
          write(nlog,270) corid(l2),cdestyp, cSouTyp, ccarry, cpuse
          ncallX=ncallX+1
        endif  
        
        WRITE(nlog,280) 'In  ',iyrmo(mon),xmonam(mon),idy,
     1    iwx, IW,L2, NR,  irow,  nd, idow, nrwr, ndwr,
     1    curown(irow),SeniorA, ritrem(nrwr), RitPap1,
     1    ritrem(ndwr), tranlim, tarcon,
     1    resavl, resalo, caprem, relaf, divactx*fac, divaf,
     1    iwhy, cwhy     
      endif
c      
c ________________________________________________________
c               Step 2b; Set Source Plan pointer
c		iP  = Plan pointer
      iP=ireuse(l2)
c
c _________________________________________________________
c
c               Step 5; Spill if amount in the OOP account
c                       exceeds the amount available to the
c                       destination right (ndwr)
c rrb 2006/05/23; This should never happen because of limits
c		  added to Resrg1.for		
c
      goto 100
      if (curown(irow).gt.ritrem(ndwr)) then
        relcfs=-1.0*(curown(irow)-ritrem(ndwr))/fac
        relaf =-1.0*relcfs*fac
        availr = avail(iscd)
        call takout(maxsta,avail,river,avinp,qtribu,idncod,
     1              relcfs,ndnr, iscd)        
        avail(iscd) = availr

        cursto(nr  )=cursto(nr  )-relaf
        curown(irow)=curown(irow)-relaf
C                                 
c               Put spilled water into spill account
c               powrel in cfs; accr in af
        powrel(nr)=powrel(nr)-relcfs
        accr(19,irow) = accr(19,irow)+relaf
        divactx = -relcfs
c
c rrb 2006/05/23; Exit now based on relcfs as opposed to         
c		  below when curown(irow)-ritrem(nrwr) will be = 0
        IF(relcfs.lt.small) then
          iwhy=4
          cwhy='OOP account is spilling'
          Goto 200
        endif  
      
      endif
 100  continue     
c
c _________________________________________________________
c
c               Step 6; Limit transfer to amount in source account 
c                       (irow) and available to senior right (nrwr)    
c rrb 2006/06/05; Revise to limit to senior storage, not senior right
cr    tranlim=amax1(curown(irow)-ritrem(nrwr), 0.0)
      tranlim=amax1(curown(irow)-SeniorA, 0.0)
c
      tranlim=amax1(tranlim, 0.0)
      
      trancfs = tranlim/fac
c
c rrb 2006/05/22; Test      
      IF(trancfs.LE.small) then
        iwhy=5
c       cwhy='OOP storage is less than the senior '
        cwhy='OOP storage is less than the senior available'
        Goto 200
      endif  
      
c
c _________________________________________________________
c
c               Step 7; CALCULATE VOLUME AVAILABLE FROM RESERVOIRS
c
      if(nd.eq.nr) then
        RESAVL=CUROWN(IROW)
        RESALO=OWNMAX(IDOW)-CUROWN(IDOW)
        tarcon=tarmax(nd)-cursto(nd)        
      else
        RESAVL=AMAX1(AMIN1(CURSTO(NR)-VOLMIN(NR),CUROWN(IROW)),0.)
        tarcon=tarmax(nd)-cursto(nd)
        RESALO=AMIN1(OWNMAX(IDOW)-CUROWN(IDOW),tarcon)
      endif
      RESALO=AMAX1(RESALO,0.)
c
      RALCFS=RESALO/fac
      
      RAVCFS=RESAVL/fac
      
      IF(RALcfs.LE.small) then
cr      write(nlog,*) '  OopBook; idow, curown(idow)', idow,curown(idow)
        iwhy=6
        cwhy='Destination capacity is zero'
        Goto 200
      endif  

      IF(RAVcfs.LE.small) then
cr        write(nlog,*) ' OopBook; irow, curown(irow)',irow, curown(irow)
      
        iwhy=7
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
c               Step 8; CALCULATE ACTUAL DIVERSION
c
c rrb 2006/05/22; Test
      divact=amin1(ravcfs,ralcfs,caprem,trancfs)

      divact=amax1(0.0,divact)
C
c     IF(DIVACT.LE.0.00001) Goto 200
      IF(DIVACT.LE.small) then
        iwhy=8
        cwhy='Diversion = 0'      
        Goto 200
      endif  
C
c
c rrb 98/03/03; Daily capability
c     DIVAF=DIVACT*MTHDAY(MON)*FACTOR
      DIVAF=DIVACT*fac
c
c _________________________________________________________
c
c               Step 9; UPDATE VARABLES
C
      CURSTO(NR  )=CURSTO(NR  )-DIVAF
      CUROWN(IROW)=CUROWN(IROW)-DIVAF
C
      CURSTO(ND  )=CURSTO(ND  )+DIVAF
      CUROWN(IDOW)=CUROWN(IDOW)+DIVAF

      QRES  (4,ND)=QRES  (4,ND)+DIVAF
      QRES (22,NR)=QRES (22,NR)+DIVAF
c
      accr(4,idow)  = accr(4,idow)+divaf
      accr(22,irow) = accr(22,irow)+divaf
c
c	 Adjust the junior right
cr    ritrem(ndwr) = ritrem(ndwr) - divaf
      ritrem(ndwr) = amax1(ritrem(ndwr) - divaf, 0.0)
      
c
c _________________________________________________________

c		Step 19; Out of Priority Plan (similar to a reservoir)
      if(iP.gt.0) then
        psuply(iP)=psuply(iP)-divaf
        psuplyT(iP)=psuplyT(iP)-divaf
        psto2(iP)=psto2(iP)-divaf
        ipsta1=ipsta(iP)
      endif  
      
  200 divo(l2)=divo(l2)+(divaf/fac)
c
c rrb 2006/05/02; Convergence
      divactx=divaf/fac
      if(divactx.lt.small) divactx=0.0  
c
c _________________________________________________________
c
c               Step 10; Detailed Output
c
      if(iout.ge.1 .and. iw.eq.ioutiw) then
        WRITE(nlog,280) 'Out ',iyrmo(mon),xmonam(mon),idy,
     1    iwx, IW, L2, NR,  irow,  nd, idow, nrwr, ndwr, 
     1    curown(irow),SeniorA, ritrem(nrwr), RitPap1,
     1    ritrem(ndwr),  tranlim, tarcon,
     1    resavl, resalo, caprem*fac, relaf, divactX*fac, divaf,
     1    iwhy, cwhy
      endif
c
c _________________________________________________________
c
c               Step 11; Roundoff check
      call chekres(io99,maxres, 1, 8, iyr, mon, nr,nowner,
     1                    curown,cursto,cresid)
c
c _________________________________________________________
c
c               Step 12; Return
c
      return
c
c _________________________________________________________
c
c               Formats
c
  270 format(/, 72('_'),/
     1 '  OopBook; Out-of-Priority Storage (Type 8)',/
     1 '          Operation Right ID = ', a12,
     1 ' Destination Type = ', a12, ' Source Type = ', a12,
     1 ' Carrier (Y/N) = ',a3, ' Reuse Plan (Y/N) = ', a3//
     1 '  OopBook;                            ',
     1 '                              ',
     1 '       OOP    Senior    Senior    Senior    Junior  Transfer',
     1 '    Target',
     1 ' Available Available Available                              ',/
     1 '                                      ',
     1 '                              ',
     1 '   Storage Available     Right PaperRigh     Limit     Limit',
     1 '    in_Res   in_Acct   Minimum     Spill   DIVACTx  BookOver',/
     1 '          iyr  mon  day iter   iw   l2',
     1 '   NR irow   nd idow nrwr ndwr',
     1 '    curown   SeniorA   ritremS  RitPaper   ritremD   tranlim',
     1 '    tarcon',
     1 '    resavl    resalo    caprem     RELAF   DIVACTx     DIVAF',
     1 ' iwhy Comment',/
     1 8x, 12(' ____'), 13(' _________'), ' ____ ',24('_'))
  280 format(
     1 4x,a4,i5,2x,a3, 10i5, 13f10.0, i5, 1x,a48)
c
c _________________________________________________________
c
      END
