c
c *********************************************************
c
      SUBROUTINE RTNSEC(icx, DIVACT,L2,IUSE,IDCD,nd,ieff2)
c
c
c
c _________________________________________________________
c	Program Description
c
c       Rtnsec; It calculates return flows from a diversion
c        that are non-consumed.
c		     It calls return to calculate CU, IWR remaining,
c		     amount returns and actual efficiency.
c
c	 Called by VirCom, DsaMod, DirectEx, ...
c	 Note when called by Vircom idcd=0 and ioptio=1 or 9
c	      which do not allow adjustments to River
c	      and avail to be performed
c _________________________________________________________
c       Update History
c
c rrb; 95/03/18; removed redundant refrence to over 50 tables
c                saved as old code f031596.zip
c rrb; 98/03/17; Revised to handle daily operation
c rrb; 00/12/09; Revised to handle variable efficiency
c
cc _________________________________________________________
c       Called By:
c CARRPL.for,  DEPLETE.FOR,  DepleteP.for, DirectBy.for, DirectEx.for, 
c DivAlt.for,  DivCar.for,   Divcar1.for,  DIVCAR2.FOR,  DivCarR.for,  
c DivImpR.for, DivMulti.for, DIVRES.FOR,   DivResP.for,  DivResP2.for, 
c DivresR.for, DIVRIG.FOR,   DivRigS.for,  Divrpl.for,   divrplP.for,
c divrplR.for, DsaMod.for,   EXECUT.for,   ifrrigSP.for, OopDiv.for,
c return2.for, RTNMAX.FOR,   RTNMAXE.FOR,  RTNSEC.FOR,   RtnsecC.for, 
c RtnsecP.for, RtnsecR.FOR,  RtnsecRP.for, RTNSECW.FOR,  RtnSecWP.for,
c RtnSecX.for, RtnXcu.FOR,   SEPSEC.FOR,   Splatte.for,  Vircom.for, 
c WelAugP.for, WelRech.for,  WelRig3.FOR,  WelRig3P.FOR 
c
c _________________________________________________________
c       Documentation
c               icx    =      subroutine called by
c                             1=carrpl, 2=divcar, 3=divcar1, 4=divres
c                             5=divrig, 6=divrpl, 7=vircom,  8=divcar2,
c                             9=directEx, 10=directBy, 11=divcarR, 
c                             33=DivImpR, 38=OOPDiv2, 39=DivAlt, 145=divcarl,
c                             -1 = DsaMod
c               divact =      diversion
c               l2     =      water right counter unless called by
c                             divres, divrpl, or directex then its 
c                             the opr. counter
c               iuse   =      diversion user
c               idcd   =      river station ID where the diversion
c                             is located.  
c			     *** Note set to 0 for baseflow operation
c               nd     =      diversion ID  
c               ieff2  = 0    use average efficiency for calculations      
c                             std for calls by operation rules 
c                             (e.g. divres)
c               ieff2  = 1    use maximum efficiency for calculations
c                             std for calls by divrig if ieffmax=1
c
c               avinp       = flow upstream of a node 
c               const       = return flow to a given location
c               currtn      = current return at receiving node??   
c                             const = rettot*pcttot(irn)/10000
c
c               dlyrat(im,idly) =  % return in month im for table idly
c               dlyratd(id,idly)=  % return in day id for table idly
c               diveff( )   = diversion efficiency  
c
c               divchk      = maximum diversion or immediate return
c                             before reoperation is required (cfs)
c
c               iout        = 0 no detailed output, 1 yes detailed output
c               ichkwr      = water right pointer (l2) for detailed 
c                             printout when iout=2

c               imd    =      days this month from execut.for
c               idy    =      day of month
c               imo         = from execut via common block
c                             circular monthly counter
c               idy         = from execut vial common block
c                             daily counter
c               ido         = from execut via common block
c                             circular daily counter
c
c               interv = +n = number of returns for all patterns
c                      = -n = variable number of returns per pattern
c
c               irnsta(irn) = ircd = return location 
c               idncod(ircd)= iscd = river location just downstream
c			      of the return location (ircd)
c
c               irnsta(irn) = ircd = return location node
c               irnord(ircd)= set in datinp.  Return pointer associated
c                             with River node ircd
c
c               irtndl(irn) = idly = return table
c
c               ireop       = reoperation code if returns are not
c                             downstream 0=no, 1=yes
c
c               mon         = from execut via common block
c                             monthly counter
c               ndly(n)     = # of returns for pattern n
c               ndlymx      = max # of returns for any pattern
c                             from mdainp.for for a monthly model
c                             from dayest.for for a daily model
c
c               nrtn(iuse)  =  irni = beginning # of return locations
c               nrtn(iuse+1)=  irne = ending # of return locations
c
c               pcttot(irn) = percent return to a given location
c               rettot      = total return flow
c                             rettot = divact*(1-diveff()/100)    
c               ret         = return flow to a location in a month
c                             ret = const * dlyrat(imo,idly)
c               ret1        = return in this time step (cfs)
c
c               retur(kk,iord) = future returns in month kk at node iord
c                               note kk is a circular pointer based on max
c                               return interval e.g if max is 5, then
c                               at time 1 kk=1-5 at time 2 kk=2-4,1, etc.
c               returnd(kk,iord)=same as above but for daily
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
c
c
c _________________________________________________________
c
c               Step 1 - Initilize
c
c     write(6,*)    '  Rtnsec;'
c     write(nlog,*) '  Rtnsec;'
c
c		iout=0 no detailed checks
c		iout=1 details for immediate returns 
c		iout=2 details for immediate and future returns
c
c		ichkwr details for water right (l2) ichkwr 
c			when iout=2
      iout = 0
      ichkD = 0
      ichkWR = 0
cx      iout = 2
cx      ichkD = 7
cx      ichkWR = 16
cx    if(nd.eq.ichkD .or. l2.eq.ichkWR) iout=3
      ion=0
      
 
      l2 = l2
      ireop=0
      iprintr=0

      ret1=0.0
      rett=0.0
      
      small=0.001    
      smalln=-1.0*small  
c
c               c. Daily capability
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif   
c      
      if(iout.ge.3) 
     1  write(nlog,350) cdivid(nd), mon, idy,  
     1    divact*fac, l2, nd, icx, ieff2
 350    format(/, 72('_'),/, 
     1  '  Rtnsec; ID          ',
     1  '  Mon  idy  divact   l2   nd  icx ieff2', /
     1  '          ',a12, 2i5, f8.0, 20i5)
c
c grb 1/16/96   Initialize source id downstream station (idwn) and
c               number of nodes downstream from that station (ndnnt)
c rrb 01/01/15; Check for zero because of call from vircom.for
      if(idcd.gt.0) then
        idwn=idncod(idcd)
        ndnnt=ndnnod(idwn)
      endif
c
c ---------------------------------------------------------
c 		Check ID
      if(nd.le.0 .or. iuse.le.0) then
        write(nlog,*) '  Rtnsec; ', cdivid(nd)
        write(nlog,*) '  Rtnsec; nd, iuse, mon, icx', nd,iuse,mon,icx
        goto 9999
      endif
      
c
c ---------------------------------------------------------
c rrb 2006/09/05; Use average efficiency (ieff2=0) 
c		  if a structure is a carrier only
c rrb 2006/11/01; Turn Off, control by calling routine
c rrb 2006/11/03; Turned back on, critical for baseflow calculations 
c rrb 2009/05/28; Leave this on. Note by setting ieff2=0 the CU
c		    is not limited to the CU demand (iwr) data
      if(irturn(iuse).eq.3) then
        ieff2=0
        if(iout.ge.3) then
          write(nlog,330) cdivid(nd), nd, iuse, 
     1      irturn(iuse), diveff(mon,iuse)
        endif
      endif  
c
c
c _________________________________________________________
c

c               Step 2 - Calculate total return (rettot)
c                        and loss (rloss)
c
c               Calculate return, etc using ave efficiency (diveff)
c               or maximum efficiency (effmax) based on switchs 
c               (ieffmax and ieff2)
c
c rrb 2006/08/29; Add new CU (call return2)
        nw1=idivco2(nd)		
        itype=1     
        cx=0.0   
        ispr=-1  
c
c rrb 2007/11/27; Use Old approach for M&I        
        AreaT=AreaSf(nd) + AreaSs(nd)+ AreaGf(nd) + AreaGs(nd)
c        
        if(AreaT.lt.small) then 
          if(iout.eq.1) write(nlog,*) ' Rtnsec; calling Return; ', 
     1          'divact, AreaT', divact*fac, areaT
          call return(
     1     nlog, nd, nw1,       ieffmax,      ieff2,      divact, 
     1     diveff(mon,iuse),    effmax(iuse), fac,        rettot, 
     1     diwrreq(nd),         dcut(nd),     dcu(nd),    cuact,
     1     isoil, soils(nd),    awcr(nd),     qdivs(nd), 
     1     ichk,  cdivid(nd))
        else                 
          if(iout.eq.1) write(nlog,*) ' Rtnsec; calling Return2; ', 
     1         'divact, AreaT', divact*fac, areaT
          call return2(
     1      nlog, iyr, mon, 
     1      nd,    nw1,         ieffmax,      ieff2,      divact, 
     1      itype, effC(nd),    effF(nd),     effS(nd), 
     1      AreaSf(nd),         AreaSs(nd),   AreaGf(nd), AreaGs(nd),
     1      Area(nd),
     1      dIwrSF(nd),         dIwrSS(nd),   dIwrGF(nd), dIwrGS(nd),  
     1      cuactSF(nd),        cuactSS(nd),  cuactGF(nd),cuactGS(nd),     
     1      cx,                 ispr,
     1      diveff(mon,iuse),   effmax(iuse), fac,        rettot, 
     1      diwrreq(nd),        dcut(nd),     dcu(nd),    cuact,
     1      isoil, soils(nd),   awcr(nd),     qdivs(nd), 
     1      ichk,  cdivid(nd),  ccall)
c           write(nlog,*) ' Rtnsec; back from  Return2'
        endif
     

c
c rrb 2000/01/24
      if(nd.le.0 .or. nd.gt.maxdiv) goto 9999

c               Calculate Loss
      rloss(nd)=rloss(nd)+rettot*pctlos(nd)/100.0
c
c               Set return flow loop data
c rrb 01/02/01; Allow a call to return for transmountain               
      IRNI=NRTN(IUSE)
      IRNE=NRTN(IUSE+1)-1
      if(iout.eq.3) then
        write(nlog,*) ' '
        write(nlog,340)
     1    ieff2, ieffmax, iuse, irni, irne, divact*fac, rettot*fac 
      endif
      
      IF(IRNI.GT.IRNE) goto 500

c
c _________________________________________________________
c
c               Step 3 - Loop for number of return flow locations

      DO 150 IRN=IRNI,IRNE
        IRCD=IRNSTA(IRN)
        ISCD=IDNCOD(IRCD)
c
c _________________________________________________________
c
c
c               Step 4 - Test if a return flow location is
c                        downstream of the source node,
c                        if not downstream, may need to reoperate
c                        (ireop=1).
c                        Skip if idcd = 0, to allow vircom.for to work
        if(idcd.gt.0) then
          idwn=idncod(idcd)
          do 100 nst1=1,ndnnt
            if (ircd.eq.idwn) goto 110
  100       idwn=idncod(idwn)
c
            ireop=1
  110     continue
        endif
c
c
c _________________________________________________________
c
c               Step 5 - Calculate return to location irn (const)
c                        and delay table (idly)


        CONST=RETTOT*PCTTOT(IRN)/10000.
        idly=irtndl(irn)
c
c rrb; 980503; Must set befor branch for baseflows (ioptio.eq.1)
        NDNN=NDNNOD(ISCD)
        IORD=IRNORD(IRCD)
c
c
c rrb 2009/05/31; Move below calculation of ret
c               If in baseflow mode (ioptio.eq.1)
c               Branch around adjustments to current month
cx      if(ioptio.eq.1 .or. ioptio.eq.9) goto 130 
c
c
c _________________________________________________________
c
c               Step 6 - Calculate return to location irn
c                        in month 1 (ret) by delay table dlyrat

c rrb 97/10/10; Daily Model test, set daily return by / # of days
        if(iday.eq.0) then
          RET=CONST*DLYRAT(1,IDLY)
        else
          ret =const*dlyratd(1,idly)
        endif
c
c rrb 01/02/10; Enhance reoperation consideration
        ret1=ret

        if(iout.eq.1) then
          write(nlog,*) ' '
          write(nlog,180)
     1      iday, l2, imd, ircd, divact*fac,
     1      const*dlyrat(1,idly)*fac,ret*fac
        endif
c
c
c rrb 2009/05/31; Moved from above to calculaet ret
        if(ioptio.eq.1 .or. ioptio.eq.9) goto 130 
c
c
c _________________________________________________________
c
c               Step 7 - Adjust avail and river at return location
c                        month 1.  Note do not adjust AVINP, flow
c                        into (upstream) of river node.
c
c rrb 2008/06/25; Do not allow upstream return flows
c		  to increase the flow at the diversion
cx        avail(ircd)=avail(ircd)+ret
          if(ircd.ne.idcd) avail(ircd)=avail(ircd)+ret
          RIVER(IRCD)=RIVER(IRCD)+RET
c
c
          IF(ISCD.LE.0) GO TO 130
c
c _________________________________________________________
c
c               Step 8 - Adjust avail, river & avinp for all
c                        downstream nodes in month 1.  Note
c                        do adjust avinp; the flow into
c                        (upstream of river node).

          ISS=ISCD

          DO 120 NST=1,NDNN
c
c rrb 2008/06/25; Do not allow upstream return flows
c		  to increase the flow at the diversion          
cx          AVAIL(ISS)=AVAIL(ISS)+RET
            if(iss.ne.idcd) AVAIL(ISS)=AVAIL(ISS)+RET
            
            RIVER(ISS)=RIVER(ISS)+RET
            AVINP(ISS)=AVINP(ISS)+RET
  120     ISS=IDNCOD(ISS)

c
c _________________________________________________________
c
c
c               Step 9 - Calculate future returns (retur())
c                        Note, they get added downstream at the
c                        beginning of each month in
c                        bomsec.for for a monthly model and in
c                        dayset.for for a daily model


  130   IM=0
        IEND=IMO+ndly(idly)-1
c        
c ---------------------------------------------------------        
        if(iout.ge.3) then
          write(nlog,*) ' '
          write(nlog,*) ' Rtnsec; ret', ret*fac
        endif
c
c ---------------------------------------------------------
c rrb 98/03/17; Monthly return capability

        if(iday.eq.0) then
          DO K=IMO,IEND
            IM=IM+1
c
c               Adjust monthly model for # of days in a month
            imx = mon+im-1
c
c rrb 99/08/27; Rio Grande size
c           if(imx.gt.24) imx = imx-24
c           if(imx.gt.12) imx = imx-12
            ixe=imx/12+1

            do ix=1,ixe
              if(imx.gt.12) imx=imx-12
            end do

            c  = float(mthday(mon))/float(mthday(imx))

            dlyrat0=dlyrat(im,idly)
            dlyrat1=dlyrat(im,idly)*c
            ret=const*dlyrat(im,idly)*c
            rett=rett+ret
            KK=K
c
c ---------------------------------------------------------
c               Check for wrap around
            IF(K.GT.ndlymx) then
              KK=K-ndlymx
            endif


c           write(nlog,*) ' Rtnsec: kk, iord ', kk, iord
c           write(6,*) ' Rtnsec: kk, iord ', kk, iord

            RETUR(KK,IORD)=RETUR(KK,IORD)+RET
c

c ---------------------------------------------------------
c
            if(iout.ge.3) then
              if(iprintr.eq.0) write(nlog,300)
              write(nlog,310)  nd, iyr, mon, im, imo, imx, irn, kk, 
     1          rettot*fac, ret*fac, dlyrat0, dlyrat1, 
     1          divact*fac, retur(kk,iord)*fac, rett*fac
     
              iprintr=1
            endif  
    
 300  format(/
     1 '  Rtnsec;  Return Flow Summary',/
     1 '           Note: mon = Current time step (month),',/ 
     1 '                 im  = Future time steps',/
     1 '                 imx = Month used to adjust for day per month',/
     1 '   nd  iyr  mon   im  imo  imx  irn   kk',
     1 '    rettot       ret   dlyrat0   dlyrat1    divact     retur',
     1 '      rett',/
     1 ' ____ ____ ____ ____ ____ ____ ____ ____',
     1 ' _________ _________ _________ _________ _________ _________',
     1 ' _________')     
 310  format(8i5, 20f10.2)


            if(iout.eq.1) then
              if(k.eq.imo) write(nlog,160)
              write(nlog,172)  l2, iord, mon, idy, imo, iend, k,
     1          im,imx, kk, ndlymx, mthday(imx)
            endif
c
c _________________________________________________________
c               End Loop for number of return flow time intervals
          end do
c
c               Print results for detailed checking
          if(iout.eq.1) then
            write(nlog,318) mon, imo
            write(nlog,320) 1, ndlymx, 
     1                     (retur(k,iord)*fac, k=1,ndlymx)
          endif
            
c
c		End month if statement          
        end if

c
c rrb 98/03/17; Daily return capability
c ---------------------------------------------------------
        if(iday.eq.1) then
          id=0
          iend=ido+ndly(idly)-1

          do k=ido,iend 
            id=id+1

            ret=const*dlyratd(id,idly)
c
c               Check for wrap around
            kk=k
            if(k.gt.ndlymx) then
              kk=k-ndlymx
            endif
            
            returd(kk,iord)=returd(kk,iord)+ret
c
c               End Loop for number of return flow time intervals
          end do
c
c               Print results for detailed checking
          if(iout.eq.1 .and. l2.eq.ichkwr) then
              write(nlog,*) '  Rtnsec Daily Return Flows; ido, ndlymx',
     1          ido, ndlymx
              write(nlog,'(10f8.2)') (returd(k,iord), k=1,ndlymx)
          endif

        endif
c
c               End Loop for number of return flow locations
  150 CONTINUE
c
c _________________________________________________________
c
c               Step 10; Reset reoperation loop check (divchkR) based on amount
c
        divchkR=ret1   
c

c
c rrb 2008/01/23; Enhancement to handle zero values better
c       if(ret1.le.divchk) ireop=0 
        c=ret1-divchk
        if(c.lt.smalln) ireop=0
c
c rrb 2011/05/12; correction
cx	      if(ret1.gt.divchk) then        
cx          ireop=1
cxcx        write(nlog,*) '  Rtnsec; reoperate by return, nd '
cxcx        write(nlog,*) '  Rtnsec:',nd, cdivid(nd), ret1*fac,divchk*fac
cx        endif
c
c _________________________________________________________
c
c               Step 11; Return
 500  RETURN
c
c
c _________________________________________________________
c
c               Formats

 160  format('     Rtnsec;   l2 iord  mon  idy  imo iend',
     1                   '    k   im  imx   kk',
     1                   ' ndlymx mdhday(imx)')
 170  format('     Rtnsec;   l2 iord  mon  idy  ido iend',
     1                   '    k   id   kk ndlymx')
 172  format(12x, 20i5)

 180  format('  Rtnsec; iday   l2  imd ircd divact    retM    retD',/,
     1             9x, 4i5, 10f8.2)
 190  format('     Rtnsec; Avail = ', i4, 10f12.0,/,(25x10f12.0))
     
 318  format('  Rtnsec; Return Array (retur(k,iord) for ',
     1 'month (mon) = ',i5, ' pointer (imo) = ', i5)
 320  format('  From ', i5, ' To ', i5, 1000f8.2)             
 
 330  format(/,60('_'),/
     1  '  RtnSec; FYI structure ID ', a12,' nd = ', i5, ' iuse = ',i5,/
     1  '          is a carrier because irturn(iuse) = ',i5,/
     1  '          Therefore average efficiency = ', f8.2, ' is used')

 340  format(/,60('_'),/
     1  '  RtnSec;     ieff2   ieffmax      iuse      irni      irne',
     1           '    divact    rettot',/
     1  '  _______ _________ _________ _________ _________ _________',
     1  ' _________ _________',/
     1  '  RtnSec;', 5i10, 20f10.2)       
     
c
c
c _________________________________________________________
c
c               Error Processing
 9999  write(nlog,900) icx, nd
 900  format('  Rtnsec; Problem when called by routine # ', i5,/
     1       ' nd = ', i15)
                                      
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop
      END





