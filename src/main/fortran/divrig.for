c
      SUBROUTINE DIVRIG(IW,L2,ISHORT,divactx,ncallx)
c
c _________________________________________________________
c	Program Description
c
c		It calculates a standard diversion		
c _________________________________________________________
c      Update History
c
c rrb 1997/10/15; Added daily model capability (see facdly) &
c               Miscellaneous clean up
c rrb 2000/12/02; Miscellaneous clean up (added steps)
c rrb 2000/12/09; Added variable efficiency capability by revising
c               rtnsec and adding rtnmax
c rrb 2000/12/18; Removed duplicate logic included in rtnmax
c rrb 2000/12/26; Added ieff2 to call rtnsec for variable efficiency
c               control (e.g. divrig.f can use variable n, divres.f 
c               cannot)
c rrb 2001/02/23; Added demand types 4 & 5 that does not allow SW
c               demands to be constrained by supplies from other 
c               sources (e.g. wells)
c rrb 2002/06/27; Revised logic for maximum efficiency to limit
c               diversion to demand via divalo
c rrb 2004/09/08; Add ability to distribute a diversion to owners
c		based on ownership fraction
c
c rrb 2005/07/22; Began to add call information
c
c_____________________________________________________________
c	Documentation
c
c        icx            subroutine call # (5)
c        IW             Global water right ID
c        L2             Direct Diversion water right ID
c        ishort         code for reoperation; 0=no, 1=yes
c
c        divactx        actual diversion
c
c        divreq         Diversion demand for types 1-3 (standard)
c
c        divsw          SW demand for demand types 4 & 5 
c
c        divreqx        Set to divreq or divsw based on demand type
c
c        dcrdiv         Decree (cfs)
c        divd           Decree diverted in previous iterations
c        divcap         Structure capacity
c        divmon         Capacity diverted in previous iterations
c
c        idvsta(l2)     STATION WHERE DIV. RIGHT L2 LOCATES
c
c        ieff2          =0 always use average efficiency
c                       =1 let ieffmax control variable efficiency 
c	  idemtyp	   Switch for demand data type 
c                       (See Section 7.10 of Doc)
c			   1 Historic Demand Approach
c				demands for structures with both SW 
c				and GW rights are provided in a
c				separate file (e.g. *.ddm & *.wem) 
c				and are not added 
c				(i.e. SW shortages cannot be
c				supplied by GW & visa versa)
c		          2 Historic Sum Demand Approach
c				demands for structures with both SW &
c				GW rights are provided separately
c				(i.e. the *.ddm and *.wem files
c				are added. Demands can be supplied
c				by SW or GW)
c			   3 Structure Demand Approach
c				demands for structures with both SW 
c				and GW rights are provided 
c				in one file, the direct diversion 
c				demand file (e.g. *.ddm).Demands 
c				for well only lands are provided
c				in the well demand file (*.wem)
c				Demands can be supplied by SW or GW).
c			   4 Supply Demand Approach
c				Same as 3 but the surface water may be 
c				diverted up to their demand even
c				if a CIR does not exist.
c				See Section 7.10for a detailed
c				discussion.
c			   5 Decreed Demand Approach
c				Same as 4 but the Decreed Demand
c				Approach is used. See Section 7.10 
c				for additional discussion.
c        imd            Number of days this month from Execut.for
c        ioprtn
c        iout           Switch: 0 no print; 1 yes print
c        idvsta(nd)     Diversion station
c        iscd           Diversion station (iscd = idvsta(nd))
c
c        ndnnod(iscd)   Number of downstream nodes
c        ndns           Number of downstream nodes from diversion
c                       (ndns=ndnnod(iscd))
c        ndnr           Number of downstream nodes from return
c                       (ndnr = f (ndnnod)) 
c
c        iuse           Diversion user
c
c        qdiv(5, )      InBasin diversion by priority
c        qdiv(8, )      Transmountain diversion by priority
c
c        currtn         Immediate return to diverting node??
c        qtribu         Tributary Flow ???
c        qstern         ???
c        small          a small value for roundoff (0.0) concerns
c
c        DivownP(n)     Ownership fraction
c	 DivnamO(n)     Owner name
c	 NdOwn(n)       Pointer to ID of owner
c	 DivOwn(n)      Diversion by owner n
c
c	 imcd           Call information
c
c _____________________________________________________________
c	Dimensions
c
      include 'common.inc'
      character ctype1*12
      character cwhy*48, cdestyp*12, ccarry*3, cpuse*3, 
     1 cRelTyp*12, cReplace*3,
     1 OkIN*3, OkOut*3,  cCallBy*12, corid1*12
c
c_____________________________________________________________
c               Step 1; Common Initilization
c
c 
c ---------------------------------------------------------
c		Detailed Reporting
c		  iout=0 no details
c		  iout=1 details
c		  iout=2 summary
c		  iout=3 details of ieff2, etc
c
c		  ioutwr=details on water right ioutwr
c
c		  ioutIN=0 no details from DsaMod
c		        2 yes details from DsaMod
      iout=0
      ioutiw=0  
      ioutIN=0
      ioutwr=0
      ioutwr=516
      ioutwr = 380
      iresw=0
      
      if(crigid(l2).eq. ccall) ioutiw=iw
c
c rrb 2009/05/31; Detailed output for structure pointer 4     
      
      ND  =IDIVCO(1,L2)
      if(nd.eq.4) then
cx        ichk=203
cx        ioutiw=iw
      endif    
      
cx    write(nlog,*) ' Divrig; ichk, ioutiw, iw', ichk, ioutiw, iw  
      
      if(ichk.eq.203 .and. ioutiw.eq.iw) then
        iout=2
        ioutIN=2
c
c rrb 2015/06/15; Additional detailed output        
        if(ncallx.eq.0) then
          write(nlog,*) ' DivRig_1; ccall iw, ioutiw', ccall, iw, ioutiw
          write(nlog,*) ' DivRig_1; nd, idivsw(nd)', nd, idivsw(nd)   
        endif     
      endif        
      
      cCallBy='DivRig      '
      corid1=crigid(l2)

cx    if(ichk.eq.94) write(nlog,*) ' DivRig; Entering'
c
c
c ---------------------------------------------------------
c rrb 2006/10/31; Daily capability
c     f = mthday(mon) * factor
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif   
c
c ---------------------------------------------------------
c		Miscellaneous Initilization
c
      cdestyp='Diversion'
      ccarry='No'
      cpuse='No'
      creltyp='Diversion'
      creplace='No '
      divact = 0.0
      divalo = 0.0
      divaloX= 0.0
      
      oprEffT=1.0
      
      pavail=0.0/fac            
      avail0=-1.0/fac
      divactx=0.0
      ISHORT=0
      imcd=-1
      
      cwhy='NA'
      iwhy=0
      icase=0
c
c ---------------------------------------------------------
c rrb 00/12/26; Variable efficiency controlled by ieffmax
      ieff2=1
c
c ---------------------------------------------------------
c rrb 01/01/17; Subroutine Call ID
      icx=5
c
c ---------------------------------------------------------
      small = 0.001
      smalln=small*(-1.0)
c
c ---------------------------------------------------------
c rrb 2006/10/31; Check avail coming in
      OkIn=' -1'
      OkOut=' -1'
      call chekava(24, maxsta, numsta, avail)      
      OkIn='Yes'
c
c_____________________________________________________________
c               Step 2; Set Destination Data
c
      ND  =IDIVCO(1,L2)
c
c rrb 2015/06/15; Additional detailed output  
      if(ichk.eq.203 .and. ioutiw.eq.iw) then      
        if(ncallx.eq.0) then
          write(nlog,*) ' DivRig_2; ' 
          write(nlog,*) ' DivRig_2; nd, idivsw(nd)', nd, idivsw(nd)   
        endif   
      endif  
      
c
      if(idivsw(nd).eq.0) then
        iwhy=1
        cwhy='Diversion is off'
        goto 260
      endif  
c
      IUSE=NDUSER(ND)+IDIVCO(2,L2)-1

c
c_____________________________________________________________
c               Step 3; Set Source Data
c
      ISCD=IDVSTA(ND)
      NDNS=NDNNOD(ISCD)
      avail0=avail(iscd)
c     if(nd.eq.2) write(nlog,*) ' Divrig; nd, iscd, iwx, imcdL(iscd)', 
c    1 nd, iscd, iwx, imcdL(iscd)
c
      IF(-IOPOUT.eq.ISCD .or. iout.eq.1) then
        write(nlog,100)  iyrmo(mon), xmonam(mon), iuse, iopout,
     1                   iscd, idemtyp, irturn(iuse)
 100    format(
     1    /, 80('_'), /,
     1    '  Divrig;     iyr     mon    iuse  iopout',
     1             '    iscd idemtyp  irturn',/,
     1   9x, i8, 4x, a4, 5i8)
      endif

c
c_____________________________________________________________
c               Step 4; Set current diversion based on demand type
c			   idemtyp (see documenation above)
c
      if(idemtyp.le.3) then
        divreqx=divreq(iuse)
      else
        divreqx=divsw(iuse)
      endif
c
c_____________________________________________________________
c               Step 6; Check remaining demand and return if zero
c
      if(divreqx.lt.small.or.(dcrdiv(l2)-divd(l2)).le.small.or.
     1   divcap(nd)-divmon(nd).le.small) then
        iwhy=2
        cwhy='Demand or capacity is zero'
        goto 260
      endif  
c
c_____________________________________________________________
c               Step 7; Begin generic water supply checks
c
      DO IS=1,NUMSTA
        AVTEMP(IS)=AVAIL(IS)
      end do
c
c
c ---------------------------------------------------------
c               Set STARTING AND ENDING INDEX OF RETURN FLOW STATIONS
      iri=nrtn(iuse)
      IRE=NRTN(IUSE+1)-1
c
c_____________________________________________________________
c               Step 8; Case 1 No water available at headgate location

      if(avtemp(iscd).le.small) then
        IF(IRTURN(IUSE).LE.3) ISHORT=1
        iwhy=3
        cwhy='Available flow at diversion is zero'   
        imcd=iscd     
        goto 260
      endif
c
c_____________________________________________________________
c               Step 9; Calculate allowable diversion (divalo)
      
      divalo=amin1(dcrdiv(l2)-divd(l2),divreqx, 
     1       divcap(nd)-divmon(nd))
      divalo=amax1(0.0,divalo)

      IF(-IOPOUT.eq.ISCD .or. iout.eq.1) then
        write(nlog,*) ' Divrig; Step 9 divalo = ', divalo*fac
      endif
c      
c_____________________________________________________________
c               Step 10; Call DsaMod to calculate diversion 
c			    limited by water supply
c     
      iresw=0 
cx    write(nlog,*)'  Divrig; corid1, iscd, ndns ', corid1,iscd,ndns
      call DsaMod(
     1   icx, ioutIN, l2, imcd,  iscd, ndns, nd, iuse, ieff2, 
     1   fac, pavail, divalo, divact, oprEffT, divactL, 
c
c rrb; 2016/06/15; Keep iwhy unique to this routine (dont
c                  reset in dsamod)
cx   1   iwhy, icase, ishort, iresw, cCallBy, corid1, cwhy)
     1   iwhy2, icase, ishort, iresw, cCallBy, corid1, cwhy)     
c
c_____________________________________________________________
c               Step 11; UPDATE MONTHLY DIVERSION FOR EACH USER
c
c
c rrb 01/02/23; Demand options 4 & 5
      if(idemtyp.le.3) then
        DIVREQ(IUSE)=DIVREQ(IUSE)-DIVACT
      else
        divreq(iuse)=amax1(0.0, divreq(iuse)-divact)
        divsw(iuse)=divsw(iuse)-divact 
c
c rrb 01/02/25; Demand options 4 & 5               
        nw=idivco2(nd)
        if(nw.gt.0) then
          if(ieffmax.le.0) then
            effd=diveff(mon,nd)/100.
            effw=diveffw(mon,nw)/100.
          else
            effd=effmax(nd)/100.
            effw=effmaxw(nw)/100.
          endif

          dcux=(divact*effd)/effw
        endif
      endif
c
      USEMON(IUSE)=USEMON(IUSE)+DIVACT 
      DIVMON(ND  )=DIVMON(ND  )+DIVACT
      divd(l2) = divd(l2)+divact
c
c
      IF(IRTURN(IUSE).ne.4) then
        QDIV(5,ISCD)=QDIV(5,ISCD)+DIVACT
      else
        QDIV(8,ISCD)=QDIV(8,ISCD)+DIVACT
      endif
c
c_____________________________________________________________
c               Step 12; Print detailed results if requested
c 
  260 continue
c
      if(iout.ge.1 .and. iw.eq.ioutiw) then
         ncallX=ncallX+1       
         if(ncallX.eq.1 .or. ioutIN.ge.1)then
           write(nlog,270) crigid(l2),cdestyp,ccarry,cpuse,
     1       cRelTyp, Creplace
         endif  
      
      
        NR=IRSORD(1,ISCD)
        write(nlog,280) '  Divrig Out ',
     1    iyrmo(mon),xmonam(mon), idy, iwx, corid1,
     1    iw, nwrord(1,iw), l2, nr, IRSORD(2,ISCD), ND, IUSE, imcd, 
     1    DIVREQx*fac,    AVAIL0*fac, CURRTN(ISCD)*fac, 
     1    DIVCAP(ND)*fac, DIVMON(ND)*fac, divd(l2)*fac, pavail*fac,
     1    divact*fac, iwhy, cwhy
     
        IF(IRSORD(2,ISCD).ne.0) then
          write(nlog,300) corid1,(qres(i,nr  ),i=1,16)
        endif
      endif
c    
c_____________________________________________________________
c               Step 13; Set return switch (iretsw), shortage (ishort) 
c                 switch and actual diversion (divact)
c

      if(divact.gt.small) iretsw=1
      if((divact+small).lt.divalo) ishort = 1
      divactx=divact
c
c _________________________________________________________
c               Step 14; Distribute to Owners
      no=ndown(nd+1)-ndown(nd)
      if(no.gt.1 .and. divact.gt.small) then
        call AccDiv(nlog, maxownd, nd, divact, ndown, divownP,
     1    divownQ, f, iopout, iscd, iyrmo(mon), xmonam(mon), divnamo)    
      endif
c
c_____________________________________________________________
c               Step 15; Identify call (note only if short)
c
      if(nd.gt.0) then
        ctype1='Diversion'
        call GetCall(iscd, imcdL(iscd), nd, ctype1)        
      endif  

cx    if(ichk.eq.4) write(nlog,*) ' DivRig; Exiting'

c
c_____________________________________________________________
c               Step 16; Return
      RETURN
c
c_____________________________________________________________
c               Formats
c
  270   format(/, 
     1  '  DivRig; Right ID = ', a12,
     1  ' Destination Type = ', a12,
     1  ' Carrier (Y/N) = ',a3, ' Reuse Plan (Y/N) = ', a3,
     1  ' Release Type = ', a12,' Called by Replace = ', a3,/  
     1  '  DivRig      iyr  mon  Day Iter Source ID    ',
     1  '      Iw  nwrord      l2      nr  irsord      nd    iuse', 
     1  '    imcd divreqx  avail0  currtn  divcap  divmon    divd',
     1  '  pavail  divact iwhy cwhy',/
     1  ' ___________ ____ ____ ____ ____ ____________ ', 
     1  ' _______ _______ _______ _______ _______ _______ _______', 
     1  ' _______ _______ _______ _______ _______ _______ _______',
     1  ' _______ _______ ____ ____________')
  280   FORMAT(a12, i5,1x,a4,2i5,1x, a12,1x,8i8,8F8.0, i5, 1x, a45)
  290   FORMAT(/, '  Divrig; QDIV ',a12,/,16F7.1)
  300   FORMAT(/, '  Divrig; QRES ',a12,/,16F7.1)
  310   FORMAT(/, '  Divrig Print 5',I10,6x,a4,4i10,
     1               F10.2,3I10,F10.2, f20.10)
  320   format(/, '  Divrig: avail  ',/,(10f10.2))
  330   format(/, '  Divrig: river  ',/,(10f10.2))
  340   format(/, '  Divrig; Pavail, imcd, stanam ', f8.2, i8, a24)
  390 FORMAT(/, '  DivRig; Problem negative avail for Icase = ', i5,/,
     1  '  IYR  MON ID              IW   L2 IUSE iscd imcd ieff2',
     1  '   DIVREQ   DivAct1   DivMore    DivAct',
     1  '  Avail cfs  Avail af',/
     1  ' ____ ____ _____________ ____ ____ ____ ____ ____ ____',
     1  ' _________ _________ _________ _________',
     1  ' _________ _________',/,
     1  i5,1x,a4,1x,a12,1x, 6i5, 20F10.2)
  
c
c_____________________________________________________________
c               Error warnings
c
 9999 continue
      write(nlog,270) crigid(l2),cdestyp,ccarry,cpuse,
     1  cRelTyp, Creplace
     
      cwhy='Problem'
      write(nlog,280) '  Divrig Out ',
     1    iyrmo(mon),xmonam(mon), idy, iwx, corid1,
     1    iw, nwrord(1,iw), l2, nr, IRSORD(2,ISCD), ND, IUSE, imcd, 
     1    DIVREQx*fac,    AVAIL(ISCD)*fac, CURRTN(ISCD)*fac, 
     1    DIVCAP(ND)*fac, DIVMON(ND)*fac,  divd(l2)*fac,    
     1    pavail*fac,     divact*fac,  iwhy, cwhy
 
      write(6,1050) 
      write(nlog,1051) 
      
 1050 format('    Stopped in Divrig',/,
     1       '    See the *.log file')
 1051 format('    Stopped in Divrig')
      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)
c
c_____________________________________________________________
c               Stop
      stop 
      END

