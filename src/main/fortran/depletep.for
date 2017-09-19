C
      SUBROUTINE depleteP(DIVACT,depx,L2,nd,ipTC)
c
c _________________________________________________________
c	Program Description
c
c       DepletP; It calculates well depletion
c                Same As Deplete but adjusted to calculate 
c                obligations associated with future delpetions
c		 (T&C obligations)
c
c		In summary added the following
c		1. For current time step, store depletions in AvTemp
c                  as a negative
c		2. For future time steps store depletion obligation
c                  (T&C Olbigations) in pobl( ) or poblD()
c
c
c _________________________________________________________
c	Update History
c		NA
c
c _________________________________________________________
c	Documentation
c
c               divact =      pumped water
c               l2     =      water right counter
c               nd     =      well structure associated with this right 
c
c               imd    =      days this month from execut.for
c               ioptio =      1=baseflow, 2=data check, 3=simulate, ..
c               idy    =      day of month
c
c               interv = +n = number of depletions for all patterns    
c                      = -n = variable number of depletions per pattern
c               ndly(n)     = # of depletions for pattern n
c               ndlymx      = max # of depletions for any pattern
c                             from mdainp.for for a monthly model
c                             from dayest.for for a daily model
c                          
c               nrtnw2(nd)  =  irni = begin of depletion location
c               nrtnw2(nd+1)=  irne = end of depletion location
c
c               irnstaw2(irn) = ircd = depletion location node
c               idncod(ircd)= iscd = # of downstream nodes from ircd
c
c               irtndlw2(irn) = idly = depletion table
c
c               ireop       = reoperation code if depletions are not
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
c               deptot      = total depletion flow
c                             deptot = divact*(1-diveff()/100)
c
c               pcttotw2(irn) = percent depletion to a given location
c               pctlosw2(nd)  = percent salvaged
c               const         = depletion flow to a given location
c                               const = deptot*pcttot(irn)/10000
c
c               dlyrat(im,idly) =  % depletion in month im, table idly
c               dlyratd(id,idly)=  % depletion in day id, table idly
c               dep           = depletion flow to a location in a month
c                               dep = const * dlyrat(imo,idly)
c               depx          = total depletion for current time step
c
c               depl(kk,iord) = future depletions in month kk, node iord
c                               note kk is a circular pointer based on 
c                               max depletion interval e.g if max is 5, 
c                               then at time 1 kk=1-5 at time 2 
c                               kk=2-4,1, etc.
c               depld(kk,iord)=same as above but for daily
c
c               irnord      = set in datinp.  refers to river node??
c
c               currtn      = current depletion at receiving node??
c               avinp       = flow upstream of a node
c
c               iout        = 0 no detailed output, 1 yes detailed output
c               ioutwr      = water right ID for detailed printout
c
c               rlossw2(nd) = Salvage (pumping - sum of depletions)
c
c               pobl(kk,ipTC) =future returns in month kk for 
c                               plan ipTC
c                               note kk is a circular pointer that
c                               is a function of the  maximum
c                               return interval e.g if max is 5, then
c                               at time 1 kk=1-5 at time 2 kk=2-4,1, 
c                               etc.
c               poblD(kk,ipTC)=same as above but for daily
c
c
c _________________________________________________________
c
c               Dimensions
c
      include 'common.inc'
      character cstaid1*12, cStrOut*12
c
c ___________________________________
c               Step 1 - Initilize
      iout = 0
      ioutwr = 0
      l2 = l2
      ireop=0
      depx=0.0

c
c				       Detalied data for a seleted structure      
      cstaid1=cdividw(nd)
cx    cStrOut='01_AWP031   '
      cStrOut='NA'
      if(cstaid1.eq.cStrOut) then
        iout=2
        ioutwr=l2
        write(nlog,*) ' DepleteP; Detailed data for ', cstaid1,iout
      endif            
c
c		b. Units
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif

c
c               Set depletion location counter
      IRNI=nrtnw2(nd)
      IRNE=nrtnw2(nd+1)-1

      IF(IRNI.GT.IRNE) goto 9999
c
c _________________________________________________________
c               Step 2 - Set total depletion (deptot)
c                        and salvage (rlossw2)
      deptot=DIVACT
      rlossw2(nd)=rlossw2(nd)+divact*pctlosw2(nd)/100.0
c     write(io99,*) '  DepleteP; mon, nd, rlossw2, pctlosw2'
c     write(io99,*)  mon, nd, rlossw2(nd), pctlosw2(nd)
c
c ___________________________________
c               Step 3 - Loop for number of depletion locations
      DO 150 IRN=IRNI,IRNE
        IRCD=IRNSTAw2(IRN)
        ISCD=IDNCOD(IRCD)
c
c ___________________________________
c               Step 4 - Calculate depletion to location irn (const)
c                        and delay table (idly)
        CONST=deptot*pcttotw2(IRN)/10000.
c       write(io99,*) '  DepleteP; deptot, pcttotw2(irn), const' 
c       write(io99,*)     deptot, pcttotw2(irn), const
        idly=irtndlw2(irn)
c
        NDNN=NDNNOD(ISCD)
        IORD=IRNORD(IRCD)
c
c rrb 01/01/03; Recognize other baseflow options               
c       IF(IOPTIO.EQ.1) GO TO 130
        if(ioptio.eq.1 .or. ioptio.eq.9) goto 130 
c
c ___________________________________
c               Step 5 - Calculate depletion to location irn
c                        in month 1 (dep) by delay table dlyrat
c rrb 97/10/10; Daily Model, set daily depletion by / # of days
        if(iday.eq.0) then
          dep=CONST*DLYRAT(1,IDLY)
        else
          dep =const*dlyratd(1,idly)
        endif

        if(iout.ge.2 .and. l2.eq.ioutwr) then
          write(nlog,180) irn 
          write(nlog,182) 'Current ',
     1      iday, iyr, mon, l2, imd, idly, iord, mon, mon, mon,
     1      divact*fac, pcttotw2(IRN), dlyrat(1,idly),dep*fac
        endif

c
c ___________________________________
c               Step 6 - Store immediate depletion to stream (depx) &
c                        set depletion code ireop for reoperation
        qdiv(25,ircd) = qdiv(25,ircd)+dep
c
c rrb 99/08/16; Store river depletion by this well for well output
        rdepw(nd) = rdepw(nd) + dep
c
c rrb 2009/04/29; Revise to recognize more than one 
c								  immediate return pattern
cx      depx=dep
        depx=depx+dep
        ireop=1          
c
c ___________________________________
c               Step 7 - Adjust avail and river at depletion location
c                        month 1 
        avail(ircd)=avail(ircd)-dep
        river(ircd)=river(ircd)-dep
c
c rrb 2006/03/23; Well Augmentation
        avtemp(ircd)=avtemp(ircd)-dep        
        if(iout.eq.1 .and. l2.eq.ioutwr) then
           write(io99,190) 2, (avail(is),is=1,numsta)
           write(io99,192) 2, (river(is),is=1,numsta)
           write(io99,194) 2, (avtemp(is),is=1,numsta)
        endif
c
c
        IF(ISCD.LE.0) GO TO 130
c
c ___________________________________
c               Step 8 - Adjust avail, river & avinp for all
c                        downstream nodes in month 1
        ISS=ISCD

        DO NST=1,NDNN
          AVAIL(ISS)=AVAIL(ISS)-dep
          RIVER(ISS)=RIVER(ISS)-dep
          AVINP(ISS)=AVINP(ISS)-dep
c
c rrb 2006/03/23; Well Augmentation
          avtemp(iss)=avtemp(iss)-dep                  
          ISS=IDNCOD(ISS)
        end do

        if(iout.eq.1 .and. l2.eq.ioutwr) then
           write(io99,190) 3, (avail(is),is=1,numsta)
           write(io99,192) 3, (river(is),is=1,numsta)
           write(io99,192) 3, (avtemp(is),is=1,numsta)
        endif

c
c ___________________________________
c               Step 9 - Calculate future depletions (depl())
c                        Note, they get added downstream at the
c                        beginning of each month in
c                        bomsec.for for a monthly model and in
c                        dayset.for for a daily model
  130   IM=0
        IEND=IMO+ndly(idly)-1
c
c _________________________________________________________
c
c               Monthly depletion capability
c ________________________________________
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

            dep=const*dlyrat(im,idly)*c
            KK=K
c
c               Check for wrap around
            IF(K.GT.ndlymx) then
              KK=K-ndlymx
            endif
c
c           write(io99,*) ' DepleteP: kk, iord', kk, iord
c           write(6,*) ' DepleteP: kk, iord', kk, iord

            depl(KK,IORD)=depl(KK,IORD)+dep
c
c rrb 2006/03/23; Well Augmentation for future but not current month
            if(kk.ne.imo) then
              pobl(kk,ipTC)=pobl(kk,ipTC) + dep
            endif  
            
            if(iout.ge.2 .and. l2.eq.ioutwr) then
              write(nlog,182) 'Future  ',
     1         iday, iyr, mon, l2, imd, idly, iord, im, k, kk,
     1         divact*fac, pcttotw2(IRN), dlyrat(im,idly),dep*fac
            endif

            if(iout.eq.1 .and. l2.eq.ioutwr) then
              if(k.eq.imo) write(io99,160)
              write(io99,172)  l2, iord, mon, idy, imo, iend, k,
     1          im,imx, kk, ndlymx, mthday(imx)
            endif
c
c               End Loop for number of depletion flow time intervals
          end do
c
c               Print Monthly results for detailed checking
          if(iout.eq.2 .and. l2.eq.ioutwr) then
              write(nlog,*) ' '
              write(nlog,*) '  Deplete; cstaid1, iord, imo, ndlymx' 
              write(nlog,*) '         ',cstaid1, iord, imo, ndlymx          
              write(io99,'(10f8.2)') (depl(k,iord), k=imo,ndlymx)
              write(io99,'(10f8.2)') (depl(k,iord), k=1,imo-1)
          endif
        end if

c
c _________________________________________________________
c
c               Daily depletion capability
c
        if(iday.eq.1) then
          id=0
          iend=ido+ndly(idly)-1

          do k=ido,iend 
            id=id+1

            dep=const*dlyratd(id,idly)

c           write(io99,*) 
c    1        '  DepleteP; const, id, idly, dlyratd(id,idly), dep'
c           write(io99,*) 
c    1          const, id, idly, dlyratd(id,idly), dep
c
c               Check for wrap around
            kk=k
            if(k.gt.ndlymx) then
              kk=k-ndlymx
            endif

            depld(kk,iord)=depld(kk,iord)+dep

c           if(iprintd.eq.0) write(io99,300)
c           write(io99,302)  iyr, mon, imo, ido, idy, nd, kk, iord,
c    1        depld(kk,iord), rett, depld(kk,iord)-rett
c           iprintd=1
c
c rrb 2006/03/23; Well Augmentation for future but not current month
            if(kk.ne.ido) then
              poblD(kk,ipTC)=poblD(kk,ipTC)+dep
            endif  
c
c               Print results for detailed checking
            if(iout.eq.1 .and. l2.eq.ioutwr) then
              write(nlog,*) '  DepleteP; ido, ndlymx, kk', ido, ndlymx
              write(nlog,'(i5,10f8.2)') kk, poblD(kk,ipTC)*fac
            endif    


            if(iout.eq.1 .and. l2.eq.ioutwr) then
              if(k.eq.ido) write(io99,170)
              write(io99,172)  l2, iord, mon, idy, ido, iend, k, id,
     1                         kk, ndlymx
            endif
            
            
c
c               End Loop for number of depletion flow time intervals
          end do
c
c               Print Daily results for detailed checking
          if(iout.eq.2 .and. l2.eq.ioutwr) then
              write(nlog,*) ' '
              write(nlog,*) '  Deplete; cstaid1 iord ido, ndlymx' 
              write(nlog,*) '         ',cstaid1,iord,ido, ndlymx          
              write(io99,'(10f8.2)') (depld(k,iord), k=1,ndlymx)
          endif

        endif
c
c               End Loop for number of depletion flow locations
  150 CONTINUE
C
c _________________________________________________________
c
c               Return
 9999 RETURN
c
c _________________________________________________________
c
c               Formats
 160  format('    DepleteP;   l2 iord  mon  idy  imo iend',
     1                   '    k   im  imx   kk',
     1                   ' ndlymx mdhday(imx)')
 170  format('    DepleteP;   l2 iord  mon  idy  ido iend',
     1                   '    k   id   kk ndlymx')
 172  format(12x, 20i5)

 180  format(/
     1 '    Deplete; Time Step. Location & Table Data = ',i5,/
     1 12x,9x,' iday  iyr  mon   l2  imd idly iord   im    k   kk', 
     1 '  divact     pct  dlyrat     dep',
     1             12x, a8,1x, 10i5, 20f8.2)
     
 182  format(12x, a8,1x, 10i5, 20f8.2)
     
 190  format('    DepleteP; Avail  = ', i4, 10f8.2,(/,25x10f8.2))
 192  format('    DepleteP; River  = ', i4, 10f8.2,(/,25x10f8.2))                                       
 194  format('    DepleteP; Avtemp = ', i4, 10f8.2,(/,25x10f8.2))                                       
 300  format('  DepleteP; Well depletion check',//
     1 '  iyr  mon  imo  ido  idy   nd   kk iord',     
     1 '      Depl      rett     delta',/
     1 ' ____ ____ ____ ____ ____ ____ ____ ____', 
     1 ' _________ _________ _________')
 302  format(8i5, 20f10.2)
 
 310  format(/'  RtnsecP; Plan Obligation Summary',/
     1 '  iyr  mon  imo ipln   kk      pobl      rett',
     1                          '    rettot     delta',/
     1 ' ____ ____ ____ ____ ____ _________ _________',
     1                          ' _________ _________')
 312  format(5i5, 20f10.2)

      END





