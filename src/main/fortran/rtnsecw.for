C
c *********************************************************
c
      SUBROUTINE RTNSECw(DIVACT,retx,rlossX,cuact,
     1                   L2,IDCD,nw,nd,ieff2,ispr)
c
c
c
c _________________________________________________________
c	Program Description
c
c       Rtnsecw; Calculate return flows for wells.
c                Similar to rtnsec but changed the following for wells
c                ireop =1 always reoperate if immediate return
c                         exceeds limit (divchk from execut & common)
c                retx  = volume returned in current day or month
c                cuact = volume consumed in current day or month
c                nrtnw Vs nrtn = wells
c                diveffw Vs diveff = wells
c                pcttotw Vs pcttot = wells
c                irnstaw Vs irnsta = wells
c
c _________________________________________________________
c       Update History
c
c rrb 95/03/18; removed redundant refrence to over 50 tables
c                saved as old code f031596.zip
c rrb 98/03/17; Revised to handle daily operation
c rrb 00/12/26; Added variable efficiency capability by
c               adding call return and ieff2
c
c _________________________________________________________
c       Documentation
c
c               divact =      diversion
c               l2     =      water right counter unless called by
c                             divres or divrpl, then its the opr. 
c                             counter
c               nw     =      well structure associted with this right
c               nd     =      associated diversion with this right
c
c               idcd   =      # of downstream nodes.  Note set to 0 for
c                             baseflow calculations
c
c               imd    =      days this month from execut.for
c               ioptio =      Option 1=baseflow, 2=data check, 
c                               3=simulate, ..
c               idy    =      day of month
c
c               interv = +n = number of returns for all patterns
c                      = -n = variable number of returns per pattern
c               ispr  =       0 use flood efficiency
c                             1 use sprinkler efficiency
c
c               ndly(n)     = # of returns for pattern n
c               ndlymx      = max # of returns for any pattern
c                             from mdainp.for for a monthly model
c                             from dayest.for for a daily model
c                            
c               nrtnw(nw)   =  irni = beginning # of return locations
c               nrtnw(nw+1) =  irne = ending # of return locations
c
c               irnstaw(irn)= ircd = return location node
c               idncod(ircd)= iscd = # of downstream nodes from ircd
c
c               irtndl(irn) = idly = return table
c
c               ireop       = reoperation code if returns are not
c                             downstream 0=no, 1=yes
c
c               mon         = from execut via common block
c                             monthly counter
c               imo         = from execut via common block
c                             circular monthly counter
c               idy         = from execut vial common block
c                             daily counter
c               ido         = from execut via common block
c                             circular daily counter
c
c               diveffw( )  = diversion efficiency
c               rettot      = total return flow
c                             rettot = divact*(1-diveff()/100)
c               rlossw( )   = loss
c
c               pcttotw(irn) = percent return to a given location
c               const       = return flow to a given location
c                             const = rettot*pcttot(irn)/10000
c
c               dlyrat(im,idly) =  % return in month im for table idly
c               dlyratd(id,idly)=  % return in day id for table idly
c               ret         = return flow to a location in a month
c                             ret = const * dlyrat(imo,idly)
c               retx        = total return for current time step
c
c               return(kk,iord) = future returns in month kk at node iord
c                               note kk is a circular pointer based on max
c                               return interval e.g if max is 5, then
c                               at time 1 kk=1-5 at time 2 kk=2-4,1, etc.
c               returnd(kk,iord)=same as above but for daily
c
c               irnord      = set in datinp.  refers to river node??
c
c               currtn      = current return at receiving node??
c               avinp       = flow upstream of a node
c
c               ichk        = global check switch
c               ichk1        = 0 no detailed output, 1 yes detailed output
c               ichkwr      = water right ID for detailed printout
c                           
c
c
c _________________________________________________________
c	Dimensions

      include 'common.inc'
c
c __________________________________________________________  
c
c               Step 1 - Initilize
c
c     write(6,*)    '  Rtnsecw;'
c     write(nlog,*) '  Rtnsecw;'

      iprintr=0
      ichk1 = 0
      ichkwr = 0
      
      iout=0
      iw=nw
      ioutiw=-1
      
      IRNI=NRTNw(nw)
      IRNE=NRTNw(nw+1)-1

      if(iday.eq.0) then
        fac=factor*mthday(mon)
      else
        fac=factor
      endif
c
c	
      iout=0
cx    if(ichk.eq.206) iout=2
c
c rrb 2009/06/17; Revise when called by Vircom, l2=0
      if(l2.ne.0) then
        if(crigidw(l2).eq. ccall) ioutiw=nw
      else
        if(cdividw(nw).eq.ccall) ioutiw=nw
        if(nw.eq.44) ioutiw=nw
      endif

      if(iout.eq.2 .and. ioutiw.eq.iw) then
        write(nlog,*) 
        write(nlog,*) ' RtnsecW;  cdividw, iyr, mon, nw, nd,', 
     1   ' ispr irni, irne, divact' 
        write(nlog,*) ' RtnsecW; ',cdividw(nw),iyr, mon, nw, nd, 
     1    ispr, irni, irne, divact*fac
      endif     
      
cx    write(nlog,*) ' RtnSecW; ', cdividw(nw), iout, nw,ioutiw
      
      l2 = l2
      ireop=0
      retx=0.0
      ret1=0.0
      
      small=0.001
      smalln=-1*small

c     diveffx=diveffw(mon,nw)/100.0

      if(iday.eq.0) then
        fac=factor*mthday(mon)
      else
        fac=factor
      endif

      IRNI=NRTNw(nw)
      IRNE=NRTNw(nw+1)-1
      IF(IRNI.GT.IRNE) goto 9999
c
c __________________________________________________________
c
c               Step 2 - Total return (rettot)
c                        and loss (rlossw)
c
c rrb 00/12/26; Variable efficiency capability
c     RETTOT=DIVACT*(1.-DIVEFFx)
c
c               Calculate return (rettot), using ave efficiency 
c               (diveffw) or maximum efficiency (effmaxw) based 
c               on switchs (ieffmax and ieff2)
c
c               Well (effmaxw) or Sprinkler (effmaxs) efficiency
      if(ispr.eq.0) effmaxx=effmaxw(nw)
      if(ispr.eq.1) effmaxx=effmaxs(nw)
      
      if(iout.eq.2 .and. ioutiw.eq.iw) then
        write(nlog,*) ' Rtnsecw; itype, ispr, effmaxx, iw, nd, AreaT = ' 
        write(nlog,*) '          ', itype, ispr, effmaxx, iw, nd, AreaT
      endif
c
c __________________________________________________________
c
c               Step 3a - Old Approach to calculate CU, etc. 
c rrb 00/12/28; Pass IWR for lands with wells only (nd=0)
c rrb 2006/08/2006; New CU
c
c rrb 2007/11/23; Use old approach for M&I lands (area=0)
      if(nd.eq.0) AreaT=AreaGfw(nw) + AreaGsw(nw)
      if(nd.ne.0) AreaT=AreaGf(nd) + AreaGs(nd)
c

      if(AreaT.lt.small) then
        if(nd.eq.0) then
c        
c ---------------------------------------------------------      
c		Old CU Approach when AreaT<0 Well only
          call return(
     1      nlog, nd, nw,      ieffmax,   ieff2,    divact, 
     1      diveffw(mon,nw),   effmaxx,   fac,      rettot, 
     1      diwrreqw(nw),      dcutw(nw), dcuw(nw), cuact,
     1      isoil, soilsw(nw), awcrw(nw), qdivsw(nw), 
     1      ichk,  cdividw(nw))
        else
c
c        
c ---------------------------------------------------------      
c		Old CU Approach when AreaT<0  D&W lands
c rrb 00/12/28; Pass IWR for lands with diversions and wells 
c               as a total (nd>0)
          call return(
     1      nlog, nd, nw,      ieffmax,   ieff2,    divact, 
     1      diveffw(mon,nw),   effmaxx,   fac,      rettot, 
     1      diwrreq(nd),       dcut(nd),  dcuw(nw), cuact, 
     1      isoil, soils(nd),  awcr(nd),  qdivsw(nw), 
     1      ichk,  cdivid(nd))
        endif
      endif
c
c __________________________________________________________
c
c               Step 3b - NEW Approach to calculate CU, etc. 
c rrb 2007/08/06; New CU
c rrb 2007/11/23; Limit New CU approach to irrigated lands
      if(AreaT.gt.small) then
        if(nd.eq.0) then
c
c ---------------------------------------------------------      
c		New CU Approach When Area > 0, Well Only

          itype=3
          call return2(
     1      nlog, iyr, mon, 
     1      nd, nw,             ieffmax,    ieff2,       divact, 
     1      itype, effCw(nw),   effFw(nw),  effSw(nw), 
     1      dum1,               dum1,       AreaGfw(nw), AreaGsw(nw),
     1      Areawa(nw),     
     1      dum1,               dum1,       dIwrGFw(nw), dIwrGSw(nw), 
     1      dum1,               dum1,       cuactGFw(nw),cuactGSw(nw),          
     1      divcapw(nw),        ispr,
     1      diveffw(mon,nw),    effmaxx,    fac,         rettot, 
     1      diwrreqw(nw),       dcutw(nw),  dcuw(nw),    cuact,
     1      isoil, soilsw(nw),  awcrw(nw),  qdivsw(nw), 
     1      ichk,  cdividw(nw), ccall)    
        else
c
c ---------------------------------------------------------      
c		NEW CU Approach when Area > 0, D&W Lands
c rrb 00/12/28; Pass IWR for lands with diversions and wells 
c               as a total (nd>0)
     
          itype=2
          call return2(        
     1      nlog, iyr, mon,
     1      nd, nw,       ieffmax,          ieff2,       divact, 
     1      itype, effC(nd),    effF(nd),   effS(nd), 
     1      dum1,               dum1,       AreaGf(nd),  AreaGs(nd), 
     1      area(nd),
     1      dum1,               dum1,       dIwrGF(nd),  dIwrGS(nd),   
     1      dum1,               dum1,       cuactGF(nd), cuactGS(nd),               
     1      divcapw(nw),        ispr,      
     1      diveffw(mon,nw),    effmaxx,    fac,         rettot, 
     1      diwrreq(nd),        dcut(nd),   dcuw(nw),    cuact,
     1      isoil, soils(nd),   awcr(nd),   qdivsw(nw), 
     1      ichk,  cdividw(nw), ccall)    
     
        endif
      endif
c
c 

      rlossw(nw)=rlossw(nw)+rettot*pctlosw(nw)/100.0
      rlossX=rlossw(nw)
c
c __________________________________________________________  
c
c               Step 3 - Return Flow Loop
c                Returns to location irn (const) by delay table (idly)

      DO 150 IRN=IRNI,IRNE
        IRCD=IRNSTAw(IRN)
        ISCD=IDNCOD(IRCD)
        CONST=RETTOT*PCTTOTw(IRN)/10000.
        idly=irtndlw(irn)
c
        NDNN=NDNNOD(ISCD)
        IORD=IRNORD(IRCD)
c
c
c __________________________________________________________  
c
c               Step 4 - Baseflow Branch around current month adj.
c
c rrb 01/01/03; Recognize other baseflow types
c       IF(IOPTIO.EQ.1) GO TO 130
        if(ioptio.eq.1 .or. ioptio.eq.9) goto 130 
c
c __________________________________________________________  
c
c               Step 5 - Calculate return to location irn
c                        in month 1 (ret) by delay table dlyrat
c rrb 97/10/10; Daily Model test, set daily return by / # of days
        if(iday.eq.0) then
          RET=CONST*DLYRAT(1,IDLY)
        else
          ret =const*dlyratd(1,idly)
        endif
c
c _________________________________________________________
c rrb 01/02/10; Reoperation refinement               
        ret1=ret

        if(ichk1.eq.1 .and. l2.eq.ichkwr) then
          write(nlog,*) ' '
          write(nlog,180)
     1      iday, l2, imd, divact, const*dlyrat(1,idly),ret
          write(nlog,190) 1, (avail(is),is=1,numsta)
        endif
c
c _________________________________________________________
c
c               Step 6 - Store immediate return (retx) and_
c                        set reoperation switch (ireop)
c rrb 2009/04/28; Correcton retX is only return for month 
c                 there is only 1 return pattern 
cx      retx=ret
        retx=retX+ret
        ireop=1
c
c _________________________________________________________
c
c               Step 7 - Adjust avail and river in month 1
c                       
        avail(ircd)=avail(ircd)+ret
        river(ircd)=river(ircd)+ret
        if(ichk1.eq.1 .and. l2.eq.ichkwr) then
           write(nlog,190) 2, (avail(is),is=1,numsta)
        endif
c
c
        IF(ISCD.LE.0) GO TO 130
c
c __________________________________________________________  
c
c               Step 8 - Adjust avail, river & avinp for all
c                        downstream nodes in month 1
        ISS=ISCD

        DO NST=1,NDNN
          AVAIL(ISS)=AVAIL(ISS)+RET
          RIVER(ISS)=RIVER(ISS)+RET
          AVINP(ISS)=AVINP(ISS)+RET
          ISS=IDNCOD(ISS)
        end do
        if(ichk1.eq.1) then
c          write(nlog,190) 3, (avail(is),is=1,numsta)
        endif
c
c __________________________________________________________  
c
c               Step 9 - Calculate future returns (retur())

c                        Note, they get added downstream at the
c                        beginning of each month in
c                        bomsec.for for a monthly model and in
c                        dayset.for for a daily model
  130   IM=0
        IEND=IMO+ndly(idly)-1
c
c __________________________________________________________  
c
c               Step 9.1 - Monthly return capability

        if(iday.eq.0) then
          DO K=IMO,IEND
            IM=IM+1
c
c               Adjust monthly model for # of days in a month
            imx = mon+im-1
c
            ixe=imx/12+1
            
            do ix=1,ixe
              if(imx.gt.12) imx=imx-12
            end do

            c  = float(mthday(mon))/float(mthday(imx))

            ret=const*dlyrat(im,idly)*c
            KK=K
c
c               Check for wrap around
            IF(K.GT.ndlymx) then
              KK=K-ndlymx
            endif
c
c           write(nlog,*) ' Rtnsec: kk, iord ', kk, iord
c           write(6,*) ' Rtnsec: kk, iord ', kk, iord

            RETUR(KK,IORD)=RETUR(KK,IORD)+RET

            if(ichk1.eq.1) then
              if(iprintr.eq.0) write(nlog,300)
              rett=0.0
              write(nlog,310)  iyr, mon, imo, nw, kk, iord,
     1          retur(kk,iord), rett, retur(kk,iord)-rett
              iprintr=1
            endif

            if(ichk1.eq.1 .and. l2.eq.ichkwr) then
              if(k.eq.imo) write(nlog,160)
              write(nlog,172)  l2, iord, mon, idy, imo, iend, k,
     1          im,imx, kk, ndlymx, mthday(imx)
              endif
c
c               End Loop for number of return flow time intervals
          end do
c
c               Print results for detailed checking
          if(ichk1.eq.1 .and. l2.eq.ichkwr) then
              write(nlog,'(10f8.2)') (retur(k,iord), k=imo,ndlymx)
              write(nlog,'(10f8.2)') (retur(k,iord), k=1,imo-1)
          endif
        end if

c
c __________________________________________________________  
c
c               Step 9.2 - Daily return capability
c
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

            if(ichk1.eq.1 .and. l2.eq.ichkwr) then
              if(k.eq.ido) write(nlog,170)
              write(nlog,172)  l2, iord, mon, idy, ido, iend, k, id,
     1          kk, ndlymx
            endif
c
c               End Loop for number of return flow time intervals
          end do
c
c               Print results for detailed checking
          if(ichk1.eq.1 .and. l2.eq.ichkwr) then
              write(nlog,*) '  Rtnsecw; ido, ndlymx', ido, ndlymx
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
c rrb 2008/01/23; Enhancement to handle zero values better
        c=ret1-divchk
        if(c.lt.smalln) then
          ireop=0 
        else
cx          if(l2.eq.8747) then
cx          write(nlog,*) ' RtnsecW;', l2, ireop, ret1*fac, divchk*fac,
cx     1      c*fac
cx          endif
        endif  
c
c __________________________________________________________  
c
c               Step 10 - Return
c
 9999 RETURN
c
c __________________________________________________________  
c
c               Formats
c
 160  format('    Rtnsecw;   l2 iord  mon  idy  imo iend',
     1                   '    k   im  imx   kk',
     1                   ' ndlymx mdhday(imx)')
 170  format('    Rtnsecw;   l2 iord  mon  idy  ido iend',
     1                   '    k   id   kk ndlymx')
 172  format(12x, 20i5)

 180  format('    Rtnsecw; iday   l2  imd  divact    retM    retD',/,
     1             12x, 3i5, 10f8.2)
 190  format('    Rtnsecw; Avail = ', i4, 10f8.2,(/,25x10f8.2))
 300  format('  Rtnsecw; Well return flow Matrix check',//
     1 '  iyr  mon  imo   nd   kk iord     retur      rett     delta',/
     1 ' ____ ____ ____ ____ ____ ____ _________ _________ _________')
 310        format(6i5, 20f10.2)
                                       
      END





