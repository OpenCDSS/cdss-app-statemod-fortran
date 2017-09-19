c
c *********************************************************
c
      SUBROUTINE RTNxCU(icx, RetTot,L2,IUSE,IDCD,nd)
c
c
c
c _________________________________________________________
c	Program Description
c
c       RtnXcu; It calculates return flows for water lost
c		    during conveyance (RetTot).
c		    Same as rtnsec but it does not call return
c		    that calculates CU, return flows, etc.
c		    Instead RetTot is provided as an input to the routine
c
c		Called by DivCar & DivCarL
c _________________________________________________________
c       Update History
c
c rrb; 2006/09/15; Copied RtnSec and revised accordingly
c _________________________________________________________
c       Documentation
c               icx    =      subroutine called by
c                             1=carrpl, 2=divcar, 3=divcar1, 4=divres
c                             5=divrig, 6=divrpl, 7=vircom,  8=divcar2,
c                             24=directEx, 25=directBy, 11=divcarR, 
c                             33=DivImpR, 38=OOPDiv2, 39=DivAlt,
c			      45=DivCarL
c               RetTot =      diversion
c               l2     =      water right counter unless called by
c                             divres, divrpl, or directex then its 
c                             the opr. counter
c               iuse   =      diversion user
c               idcd   =      river station ID where the diversion
c                             is located.  Note set to 0
c                             for baseflow operation
c               nd     =      diversion ID  
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
c               irnsta(irn) = ircd = return location node
c               irnord      = set in datinp.  refers to river node??
c
c               idncod(ircd)= iscd = # of downstream nodes from ircd
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
c
      include 'common.inc'
c
c
c _________________________________________________________
c
c               Step 1 - Initilize
c
c     write(6,*)    '  RtnXcu;'
c     write(nlog,*) '  RtnXcu;'
c
c		iout=0 no detailed checks
c		iout=1 details for returns 
      iout = 0
      ichkwr = -1
      l2 = l2
      ireop=0
      iprintr=0

      ret1=0.0
      rett=0.0

      if(iday.eq.0) then
        fac=factor*mthday(mon) 
      else  
        fac=1.0
      endif
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
c		Check ditch ID      
      if(nd.le.0 .or. iuse.le.0) then
        write(nlog,*) '  RtnXcu;', cdivid(nd)
        write(nlog,*) '  RtnXcu; nd, iuse, mon, icx', nd,iuse,mon,icx
        goto 9999
      endif

c               Calculate Loss
      rloss(nd)=rloss(nd)+rettot*pctlos(nd)/100.0
c
c               Set return flow loop data
c rrb 01/02/01; Allow a call to return for transmountain               
      IRNI=NRTN(IUSE)
      IRNE=NRTN(IUSE+1)-1
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
c               If in baseflow mode (ioptio.eq.1)
c               Branch around adjustments to current month
c       IF(IOPTIO.EQ.1) GO TO 130
        if(ioptio.eq.1 .or. ioptio.eq.9) goto 130 
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

cr      if(iout.ge.1 .and. l2.eq.ichkwr) then
        if(iout.ge.1) then
          write(nlog,*) ' '
          write(nlog,180)
     1      iday, l2, imd, RetTot*fac,const*dlyrat(1,idly)*fac,ret*fac
     
c         write(nlog,190) 1, (avail(is)*fac,is=1,numsta)
        endif
c
c
c _________________________________________________________
c
c               Step 7 - Adjust avail and river at return location
c                        month 1.  Note do not adjust AVINP, flow
c                        into (upstream) of river node.

          avail(ircd)=avail(ircd)+ret
          RIVER(IRCD)=RIVER(IRCD)+RET
c
C
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
            AVAIL(ISS)=AVAIL(ISS)+RET
            RIVER(ISS)=RIVER(ISS)+RET
            AVINP(ISS)=AVINP(ISS)+RET
  120     ISS=IDNCOD(ISS)

          if(iout.ge.1 .and. l2.eq.ichkwr) then
            write(nlog,*) ' '
            write(nlog,*) ' RtnXcu; ret', ret*fac
            write(nlog,190) 3, (avail(is)*fac,is=1,numsta)
          endif
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
c rrb 98/03/17; Monthly return capability
c ---------------------------------------------------------
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

            ret=const*dlyrat(im,idly)*c
            rett=rett+ret
            KK=K
c
c               Check for wrap around
            IF(K.GT.ndlymx) then
              KK=K-ndlymx
            endif


c           write(nlog,*) ' RtnXcu: kk, iord ', kk, iord
c           write(6,*) ' RtnXcu: kk, iord ', kk, iord

              RETUR(KK,IORD)=RETUR(KK,IORD)+RET
c
c
            if(iout.eq.1) then
              if(iprintr.eq.0) write(nlog,300)
              is=istrtn(iord)
              write(nlog,310)  iyr, mon, imo, nd, kk, iord,is, 
     1          RetTot*fac, retur(kk,iord)*fac, rett*fac, 
     1          (retur(kk,iord)-rett)*fac
              iprintr=1
            endif


            if(iout.eq.1 .and. l2.eq.ichkwr) then
              if(k.eq.imo) write(nlog,160)
              write(nlog,172)  l2, iord, mon, idy, imo, iend, k,
     1          im,imx, kk, ndlymx, mthday(imx)
            endif
c
c               End Loop for number of return flow time intervals
          end do
c
c               Print results for detailed checking
c         if(iout.eq.1 .and. l2.eq.ichkwr) then
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
              write(nlog,*) '  RtnXcu Daily Return Flows; ido, ndlymx',
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
c               Step 10; Reset reoperation loop based on amount
        divchkr=ret1   
c
        if(ret1.le.divchk) then
          ireop=0
        else
c         ireop=1
c         write(nlog,*) '  RtnXcu; reoperate by return, nd '
c         write(nlog,*) '  RtnXcu:',nd, cdivid(nd)
        endif
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

 160  format('     RtnXcu;   l2 iord  mon  idy  imo iend',
     1                   '    k   im  imx   kk',
     1                   ' ndlymx mdhday(imx)')
 170  format('     RtnXcu;   l2 iord  mon  idy  ido iend',
     1                   '    k   id   kk ndlymx')
 172  format(12x, 20i5)

 180  format(/, 72('_'),/
     1'  RtnXcu; iday   l2  imd  RetTot    retM    retD',/,
     1 9x, 3i5, 10f8.2)
 190  format('     RtnXcu; Avail = ', i4, 10f12.0,/,(25x10f12.0))
c 
 300  format('  RtnXcu; Return Data',//
     1 '  iyr  mon  imo   nd   kk iord   is    RetTot     retur',
     1 '      rett retur-rett',/
     1 ' ____ ____ ____ ____ ____ ____ ____ _________ _________',
     1 ' _________ _________')
     
 310  format(7i5, 20f10.2)
 318  format(/,'  RtnXcu; Return Array (retur(k,iord) for ',
     1 ' month (mon) = ',i5, ' pointer (imo) = ', i5)
 320  format('  From ', i5, ' To ', i5, 1000f8.2)             
  
 330  format(
     1  '  RtnXcu; FYI structure ID ', a12,' nd = ', i5, ' iuse = ',i5,/
     1  '          is a carrier because irturn(iuse) = ',i5,/
     1  '          Therefore average efficiency = ', f8.2, ' is used')
c
c
c _________________________________________________________
c
c               Error Processing
 9999  write(nlog,900) icx, nd, iuse
 900  format('  RtnXcu; Problem when called by routine # ', i5,/
     1       '          nd = ', i5, ' iuse = ', i5)
                                      
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop
      END





