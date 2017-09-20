c
c *********************************************************
c
c rrb 2011/05/19; Revise to include ndns (# of downstream nodes)
c                 so it can be used to search an exchange reach;
c                 instead of the entire downstream array
cx   SUBROUTINE RtnSecX(icx, DIVACT,L2,IUSE,IDCD,nd,ieff2, 
cx   1  retTot, ret1, corid1)
      SUBROUTINE RtnSecX(icx, DIVACT,L2,IUSE,IDCD,ndnS,nd,ieff2, 
     1  retTot, ret1, corid1)
     
c
c
c _________________________________________________________
c	Program Description
c
c     RtnSecX; It estimates return flows for the current month
c		  in order to adjust AvTemp, not Avail
c _________________________________________________________
c       Update History
c
c rrb 2008/06/25; Revise to not allow upstream returns to be 
c		   available to the diverting node (idcd)
c rrb 2008/01/29; Copy RtnSec 
c		   Remove future returns
c		   Set variables *(nd) to *X so they
c		     are not updated.
c		   Replaced Avail with AvTemp
c
c _________________________________________________________
c       Called By RtnMax via DsaMod
c
c _________________________________________________________
c       Documentation
c
c               icx    =      subroutine called by
c                             1=carrpl, 2=divcar, 3=divcar1, 4=divres
c                             5=divrig, 6=divrpl, 7=vircom,  8=divcar2,
c                             9=directEx, 10=directBy, 11=divcarR, 
c                             33=DivImpR, 38=OOPDiv2, 39=DivAlt
c			                        100=Rtnmax
c               divact =      diversion
c               l2     =      water right counter unless called by
c                             divres, divrpl, or directex then its 
c                             the opr. counter
c               iuse   =      diversion user
c               idcd   =      river station ID where the diversion
c                             is located.  Note set to 0
c                             for baseflow operation
c               ndnS   =      # of nodes downstream of idcd.  Note this
c                             value is typically the number to the
c                             end of the network.  But when called by
c                             Splatte via dsamod & rtnmax it is the
c                             # of nodes to the exchange point
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
c               iout       = 0 no detailed output, 1 yes detailed output
c               ichkwr      = water right ID for detailed printout

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
c			                        of the return location (ircd)
c
c               irnord(ircd)= Return pointer associated
c                             with River node ircd (set in datinp)  
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
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
      character corid1*12
c
c
c _________________________________________________________
c
c               Step 1 - Initilize
c
c     write(6,*)    '  RtnSecX;'
c     write(nlog,*) '  RtnSecX;'
c
c		iout=0 no detailed checks
c		iout=1 details for immediate returns 
c		iout=2 details for immediate and future returns
c		iout=3 summary for return flow data
c   iout=4 details on number of nodes in exchange reach
c
c		ichkwr details for water right ichkwr
      iout = 0
      ichkwr = 5
      ion=0
      
      isub=-3
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
c ---------------------------------------------------------
c rrb 2011/05/19; Update to to pass ndnS, the # downstream
c                 to an exchange point   
       ndnn=NDNNOD(idcd)
c
c rrb 2015/10/04; Print warning when iout=4
       if(iout.eq.4) then
         if(ndnn.ne.ndnS) write(nlog,*) '  RtnsecX_1; Warning ',
     1     ' icx, ndnn, ndnS', icx, ndnn, ndnS
       endif
c
cx     CALL DNMFSO(maxsta, avtemp, IDNCOD, idcd, ndnn, IMCD)
       CALL DNMFSO(maxsta, avtemp, IDNCOD, idcd, ndnS, IMCD)
       
       
       imcd1=imcd
       avtemp1=avtemp(imcd)
c      
      if(iout.ge.1) write(nlog,350) mon, idy, divact*fac
c350    format(/, 72('_'),/, '  RtnSecX; Mon, idy = ', 2i5, f8.0)
 350    format(/, '  RtnSecX; Mon, idy = ', 2i5, f8.0)
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
        write(nlog,*) ' RtnSecX; ', cdivid(nd), corid1
        write(nlog,*) ' RtnSecX; nd, iuse, mon, icx', nd,iuse,mon,icx
        goto 9999
      endif
      
c
c ---------------------------------------------------------
c rrb 2006/09/05; Use average efficiency (ieff2=0) 
c		  if a structure is a carrier only
      if(irturn(iuse).eq.3) then
        ieff2=0
        if(iout.eq.1) then
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
c --------------------------------------------------
c rrb 2008/01/29;Revise from Rtnsec
        diwrreqX=diwrreq(nd)
        dcutX=dcut(nd)
        dcuX=dcu(nd)
        awcrX=awcr(nd)
        soilsX=soils(nd)
c
c rrb 2009/05/26; Correction qdivsX is an array        
        qdivsX2=qdivs(nd)
c
c __________________________________________________
c rrb 2007/11/27; Use Old approach for M&I        
        AreaT=AreaSf(nd) + AreaSs(nd)+ AreaGf(nd) + AreaGs(nd)
c        
        if(AreaT.lt.small) then 
          if(iout.eq.1) write(nlog,*) ' RtnSecX; calling Return; ', 
     1          'divact, AreaT', divact*fac, areaT



          call return(
     1     nlog, nd, nw1,       ieffmax,      ieff2,      divact, 
     1     diveff(mon,iuse),    effmax(iuse), fac,        rettot, 
     1     diwrreqX,            dcutX,        dcuX,       cuact,
     1     isoil,               soilsX,       awcrX,      qdivsX2, 
     1     ichk,  cdivid(nd))
        else   
c
c __________________________________________________
c  
c rrb 2007/11/27; New approach for Agricultural lands
          if(iout.eq.1) write(nlog,*) ' RtnSecX; calling Return2; ', 
     1         'divact, AreaT', divact*fac, areaT

c
c rrb 2008/01/29;Revise from Rtnsec
          dIwrSFX = dIwrSF(nd)
          dIwrSSX = dIwrSS(nd)
          dIwrGFX = dIwrGF(nd)
          dIwrGSX = dIwrGS(nd)
  
          cuactSFX = cuactSF(nd)
          cuactSSX = cuactSS(nd)
          cuactGFX = cuactGF(nd)
          cuactGSX = cuactGS(nd)    

          call return2(
     1      nlog, iyr, mon, 
     1      nd,    nw1,         ieffmax,      ieff2,      divact, 
     1      itype, effC(nd),    effF(nd),     effS(nd), 
     1      AreaSf(nd),         AreaSs(nd),   AreaGf(nd), AreaGs(nd),
     1      Area(nd),
     1      dIwrSFX,            dIwrSSX,      dIwrGFX,    dIwrGSX,     
     1      cuactSFX,           cuactSSX,     cuactGFX,   cuactGSX,        
     1      cx,                 ispr,
     1      diveff(mon,iuse),   effmax(iuse), fac,        rettot, 
     1      diwrreqX,           dcutX,        dcuX,       cuact,
     1      isoil, soilsX,      awcrX,        qdivsX2,    
     1      ichk,  cdivid(nd),  ccall)
c           write(nlog,*) ' RtnSecX; back from  Return2'
        endif
     

c
c rrb 2000/01/24
      if(nd.le.0 .or. nd.gt.maxdiv) goto 9999

c               Calculate Loss
c
c rrb 2008/01/29;Revise from Rtnsec
cx    rloss(nd)=rloss(nd)+rettot*pctlos(nd)/100.0
    

c
c               Set return flow loop data
c rrb 01/02/01; Allow a call to return for transmountain               
      IRNI=NRTN(IUSE)
      IRNE=NRTN(IUSE+1)-1
      if(iout.eq.1) then
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
c                        Note NDNN is the # of nodes downstream of 
c                        the return location

        CONST=RETTOT*PCTTOT(IRN)/10000.
        idly=irtndl(irn)
c
c rrb; 980503; Must set befor branch for baseflows (ioptio.eq.1)
        NDNN=NDNNOD(ISCD)
        IORD=IRNORD(IRCD)

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
c _________________________________________________________
c
c               Step 7 - Adjust avail and river at return location
c                        month 1.  Note do not adjust AVINP, flow
c                        into (upstream) of river node.
c
c rrb 2008/01/29;Revise from Rtnsec
cx        avail(ircd)=avail(ircd)+ret
cx        RIVER(IRCD)=RIVER(IRCD)+RET
c
c rrb 2008/06/25; Do not allow upstream return flows
c		  to increase the flow at the diversion
cx        avtemp(ircd)=avtemp(ircd)+ret
          if(ircd.ne.idcd) avtemp(ircd)=avtemp(ircd)+ret


c
c              The following will never occur if returns
c              are not allowed to go to the diverting node
c      

          IF(ISCD.LE.0) GO TO 150
c
c _________________________________________________________
c
c               Step 8 - Adjust AvTemp for all
c                        downstream nodes in month 1. 

          ISS=ISCD

          DO 120 NST=1,NDNN
c
c rrb 2008/06/25; Do not allow upstream return flows
c		  to increase the flow at the diversion
cx          avtemp(iss)=avtemp(iss)+ret
            if(iss.ne.idcd) avtemp(iss)=avtemp(iss)+ret
  120     ISS=IDNCOD(ISS)

          if(iout.eq.2) then
            write(nlog,*) ' '
            write(nlog,*) ' RtnSecX; ret', ret*fac
c           write(nlog,190) 3, (avtemp(is)*fac,is=1,numsta)
          endif
 150    continue         
c _________________________________________________________
c
c               Step 9; As a Check
c			Subtract diversion and find the minimum
c			in Avtemp
c
cx       ndnn=NDNNOD(idcd)
cx       Call takou2(isub,maxsta, avtemp, idncod, divact, ndnn, idcd)
c
c ---------------------------------------------------------
c rrb 2011/05/19; Update to to pass ndnS, the # downstream
c                 to an exchange point   
       ndnn=NDNNOD(idcd)
c
c rrb 2015/10/04; Print warning when iout=4       
       if(iout.eq.4) then
         if(ndnn.ne.ndnS) write(nlog,*) '  RtnsecX_2; Warning ',
     1     ' icx, ndnn, ndnS', icx, ndnn, ndnS
       endif
c
cx     CALL DNMFSO(maxsta, avtemp, IDNCOD, idcd, ndnn, IMCD)
       CALL DNMFSO(maxsta, avtemp, IDNCOD, idcd, ndnS, IMCD)       
       retmin=avtemp(imcd)
       if(iout.ge.3) then
         write(nlog,*) ' '
         write(nlog,*) 
     1    ' RtnsecX;',
     1    '    idcd    ndnS   imcd1    imcd',
     1    ' avtemp1  avtemp  divact  retTot  retMin'
     
         write(nlog,'(10x, 4i8, 20f8.2)') 
     1    idcd, ndnS, imcd1, imcd, avtemp1*fac, 
     1    avtemp(imcd)*fac, divact*fac, rettot*fac, retmin*fac 
       endif  

c _________________________________________________________
c
c               Step 11; Return
 500  RETURN
c
c
c _________________________________________________________
c
c               Formats

 160  format('     RtnSecX;   l2 iord  mon  idy  imo iend',
     1                   '    k   im  imx   kk',
     1                   ' ndlymx mdhday(imx)')
 170  format('     RtnSecX;   l2 iord  mon  idy  ido iend',
     1                   '    k   id   kk ndlymx')
 172  format(12x, 20i5)

 180  format('  RtnSecX; iday   l2  imd ircd divact    retM    retD',/,
     1             9x, 4i5, 10f8.2)
 190  format('     RtnSecX; Avail = ', i4, 10f12.0,/,(25x10f12.0))

 320  format('  From ', i5, ' To ', i5, 1000f8.2)             
 
c330  format(/,60('_'),/
 330  format(/,
     1  '  RtnSecX; FYI structure ID ', a12,' nd = ', i5, ' iuse = ',i5,/
     1  '          is a carrier because irturn(iuse) = ',i5,/
     1  '          Therefore average efficiency = ', f8.2, ' is used')

c340  format(/,60('_'),/
 340  format(/,
     1  '  RtnSecX;     ieff2   ieffmax      iuse      irni      irne',
     1             '  divact    rettot',/
     1  '  _______ _________ _________ _________ _________ _________',
     1  ' _________ _________',/
     1  '  RtnSecX;', 5i10, 20f10.2)       
     
c
c
c _________________________________________________________
c
c               Error Processing
 9999  write(nlog,900) icx, nd
 900  format('  RtnSecX; Problem when called by routine # ', i5,/
     1       ' nd = ', i15)
                                      
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop
      END





