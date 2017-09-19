c
c _________________________________________________________
c	Dimensions
c
      SUBROUTINE deplete(DIVACT,depx,L2,nd)
c
c
c _________________________________________________________
c	Program Description
c
c       Deplete;It calculates well depletion
c
c rrb 2008/01/23;Revise reoperation switch (ireop) to not reoperate
c		 if the depletion less than a specified amount (divchk)
c
c               Similar to rtnsecw but changed the following for wells
c                ireop =1 always reoperate due to depletions
c                depx  = volume depleted in current day or month
c                qdivx = depletion to 
c                ndtyp = +n to well structure -n to a diversion
c                nrtnw2 Vs nrtn = wells
c                diveffw Vs diveff = wells
c                pcttotw2 Vs pcttot = wells
c                irnstaw Vs irnsta = wells
c rrb 00/12/26;  Note since depletion is based on what
c                is pumped this routine is not impacted by variable
c                efficiency capability
c
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
c _________________________________________________________
c
c               Dimensions
c
      include 'common.inc' 
      character cstaid1*12, cStrOut*12
c
c ___________________________________
c               Step 1 - Initilize
c							iout=1 details
c							iout=2 less detail (includes current and future arrays)
c							iout=3 less detail (w/o future arrays
c             ioutD=1 details for a given water right in the control file
      ioutD = 0
      iout =0
      ioutwr = 0     
      cstaid1=cdividw(nd)
c
c				       Detalied data for a seleted structure   
      cStrOut= 'NA'
c     cstrOut= '01_AWP031   '  
      cstrOut= '6400503     '

      if(cstaid1.eq.cStrOut) then
        iout=3
        ioutwr=l2
        write(nlog,*) ' Deplete; Detailed data for ', cstaid1, iout
      endif      
      
      l2 = l2
      ireop=0
      depx=0.0
      small=0.001
      smalln=-1.0*small

c
c               b. Daily capability
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif

c     iprintd=0
c     rett=0
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
c     write(nlog,*) '  Deplete; mon, nd, rlossw2, pctlosw2'
c     write(nlog,*)  mon, nd, rlossw2(nd), pctlosw2(nd)
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
c       write(nlog,*) '  Deplete; deptot, pcttotw2(irn), const' 
c       write(nlog,*)     deptot, pcttotw2(irn), const
        idly=irtndlw2(irn)
c
        NDNN=NDNNOD(ISCD)
        IORD=IRNORD(IRCD)
c
c ---------------------------------------------------------
c		            If in Baseflow mode branch around adjustements to 
c							  the stream system, etc.
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

        if(ioutD.eq.1) then
          if(iout.ge.2 .and. l2.eq.ioutwr) then
            write(nlog,180) irn 
            write(nlog,182) 'Current ',
     1        iday, iyr, mon, l2, imd, idly, iord, mon, mon, mon,
     1        divact*fac, pcttotw2(IRN), dlyrat(1,idly),dep*fac
          endif
        endif
c
c ___________________________________
c               Step 6 - Store immediate depletion to stream (depx) &
c                        set depletion code ireop for reoperation
        qdiv(25,ircd) = qdiv(25,ircd)+dep
c
c rrb 99/08/16; Store river depletion by this well for well output
        rdepw(nd) = rdepw(nd) + dep
        depx=dep

c
c rrb 2008/01/23; Enhancement to handle small values better
c       ireop=1          
        c=depx-divchk
        if(c.lt.smalln) then
          ireop=0 
        else
          ireop=1
cx          if(l2.eq.8747) then
cx          write(nlog,*) ' Deplete;', l2, ireop, depx*fac, divchk*fac,
cx     1      c*fac
cx          endif
        endif  
c
c ___________________________________
c               Step 7 - Adjust avail and river at depletion location
c                        month 1 
        avail(ircd)=avail(ircd)-dep
        river(ircd)=river(ircd)-dep
        if(iout.eq.1 .and. l2.eq.ioutwr) then
           write(nlog,190) 2, (avail(is),is=1,numsta)
           write(nlog,192) 2, (river(is),is=1,numsta)
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
          ISS=IDNCOD(ISS)
        end do


        if(iout.eq.1 .and. l2.eq.ioutwr) then
           write(nlog,190) 3, (avail(is),is=1,numsta)
           write(nlog,192) 3, (river(is),is=1,numsta)
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
c rrb 00/02/24; Rio Grande size
c           if(imx.gt.24) imx = imx-24
c           if(imx.gt.12) imx = imx-12
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
c           write(nlog,*) ' Deplete: kk, iord', kk, iord
c           write(6,*) ' Deplete: kk, iord', kk, iord

            depl(KK,IORD)=depl(KK,IORD)+dep

            if(iout.ge.2 .and. l2.eq.ioutwr) then
              write(nlog,182) 'Future  ',
     1         iday, iyr, mon, l2, imd, idly, iord, im, k, kk,
     1         divact*fac, pcttotw2(IRN), dlyrat(im,idly),dep*fac
            endif
c
c               End Loop for number of depletion flow time intervals
          end do
c
c               Print monthly results for detailed checking
          if(iout.eq.2 .and. l2.eq.ioutwr) then
              write(nlog,*) ' '
              write(nlog,*) '  Deplete; cstaid1, iord, imo, ndlymx' 
              write(nlog,*) '         ',
     1         iyrmo(mon),xmonam(mon),cstaid1, iord, imo, ndlymx
     
              write(nlog,*) '  Deplete;',  imo, ndlymx
              write(nlog,'(10f8.2)') (depl(k,iord)*fac, k=imo,ndlymx)
              
              write(nlog,*) '  Deplete;',  1, imo
              write(nlog,'(10f8.2)') (depl(k,iord)*fac, k=1,imo-1)
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

c           write(nlog,*) 
c    1        '  Deplete; const, id, idly, dlyratd(id,idly), dep'
c           write(nlog,*) 
c    1          const, id, idly, dlyratd(id,idly), dep
c
c               Check for wrap around
            kk=k
            if(k.gt.ndlymx) then
              kk=k-ndlymx
            endif

            depld(kk,iord)=depld(kk,iord)+dep

c           if(iprintd.eq.0) write(nlog,300)
c           write(nlog,310)  iyr, mon, imo, ido, idy, nd, kk, iord,
c    1        depld(kk,iord), rett, depld(kk,iord)-rett
c           iprintd=1


            if(iout.eq.1 .and. l2.eq.ioutwr) then
              if(k.eq.ido) write(nlog,170)
              write(nlog,172)  l2, iord, mon, idy, ido, iend, k, id,
     1                         kk, ndlymx
            endif
c
c               End Loop for number of depletion flow time intervals
          end do
c
c               Print daily results for detailed checking
          if(iout.eq.2 .and. l2.eq.ioutwr) then
              write(nlog,*) ' '
              write(nlog,*) '  Deplete; cstaid1, iord, ido, ndlymx' 
              write(nlog,*) '         ',
     1          iyrmo(mon),xmonam(mon),cstaid1, iord, ido, ndlymx
              write(nlog,'(10f8.2)') (depld(k,iord), k=1,ndlymx)
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
 160  format('    Deplete;   l2 iord  mon  idy  imo iend',
     1                   '    k   im  imx   kk',
     1                   ' ndlymx mdhday(imx)')
 170  format('    Deplete;   l2 iord  mon  idy  ido iend',
     1                   '    k   id   kk ndlymx')
 172  format(12x, 20i5)

 180  format(/
     1 '    Deplete; Time Step. Location & Table Data = ',i5,/
     1 12x,9x,' iday  iyr  mon   l2  imd idly iord   im    k   kk', 
     1 '  divact     pct  dlyrat     dep',
     1             12x, a8,1x, 10i5, 20f8.2)
     
 182  format(12x, a8,1x, 10i5, 20f8.2)
     
 190  format('    Deplete; Avail = ', i4, 10f8.2,(/,25x10f8.2))
 192  format('    Deplete; River = ', i4, 10f8.2,(/,25x10f8.2))                                       
 300  format('  Deplete; Well depletion check',//
     1 '  iyr  mon  imo  ido  idy   nd   kk iord',     
     1 '      Depl      rett     delta',/
     1 ' ____ ____ ____ ____ ____ ____ ____ ____', 
     1 ' _________ _________ _________')
 310        format(8i5, 20f10.2)

      END





