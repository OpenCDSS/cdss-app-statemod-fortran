c
c *********************************************************
c
      SUBROUTINE RtnsecP(icx, iplan, iuse, l2, rettot, divLeft,
     1  retX1, pdriveX)
c
c
c
c _________________________________________________________
c	Program Description
c
c     RtnsecP; It calculates future return obligations for plans
c		  based on unused portion of diversion (rettot)
c
c		  icx =    calling routine icx = type
c                                 24=directEx,  25=directBY, 
c                                 27=DivResP2, 38=OopDiv
c		  iplan	    Total plan pointer
c		  iplan1    Return Location plan pointer
c		  iuse	    return flow pointer
c		  l2        water right pointer
c		  rettot    total return flow over all time
c		  divLeft   diversion left at headgate
c		  	        = 0 for a diversion type = Diversion 
c		  	        = rettot for a diversion type = Depletion
c		  retX1     immediate (this time step) return flow		  
c		  pdriveX   Total plan driver
c _________________________________________________________
c       Update History
c
c rrb 2004/11/30; Copy Rtnsec and edit appropriately
c rrb 2005/01/25; Add ability to use return table specified in the
c		 plan data if iprf(iplan).gt.0
c rrb 2005/11/14; Revised to reflect amount left at diversion (divLeft)
c rrb 2007/11/28; Revise to skip return % < 0 (if <0 they refer
c		  to a FIXED return pattern
c rrb 2007/12/26; Track return locations under separate T&C Plans
c
c _________________________________________________________
c     Documentation
c
c     icx    =      subroutine called by
c                   1=carrpl, 2=divcar, 3=divcar1, 4=divres
c                   5=divrig, 6=divrpl, 7=vircom,  8=divcar2,
c                   9=directEx, 10=directBY, 11=welrig
c     l2     =      water right counter unless called by
c                   divres, divrpl, or directex then its 
c                   the opr. counter
c     iuse   =      diversion user
c     idcd   =      river station ID where the diversion
c                   is located.  Note set to 0
c                   for baseflow operation
c     nd     =      diversion ID  
c
c     const       = return flow to a given location
c
c     dlyrat(im,idly) =  % return in month im for table idly
c     dlyratd(id,idly)=  % return in day id for table idly
c     diveff( )   = diversion efficiency  
c
c
c     iout        = 0 no detailed output, 1 yes detailed output
c     ichkwr      = water right ID for detailed printout

c     imd    =      days this month from execut.for
c     idy    =      day of month
c     imo         = from execut via common block
c                   circular monthly counter
c     idy         = from execut vial common block
c                   daily counter
c     ido         = from execut via common block
c                   circular daily counter
c
c     interv = +n = number of returns for all patterns
c            = -n = variable number of returns per pattern
c
c     irnsta(irn) = ircd = return location node
c     irnord      = set in datinp.  refers to river node??
c
c     irtndl(irn) = idly = return table
c
c     ireop       = reoperation code if returns are not
c                   downstream 0=no, 1=yes
c
c     mon         = from execut via common block
c                   monthly counter
c     ndly(n)     = # of returns for pattern n
c     ndlymx      = max # of returns for any pattern
c                   from mdainp.for for a monthly model
c                   from dayest.for for a daily model
c
c     nrtn(iuse)  =  irni = beginning # of return locations
c     nrtn(iuse+1)=  irne = ending # of return locations
c
c     pcttot(irn) = percent return to a given location
c     ret         = return flow to a location in a month
c                   ret = const * dlyrat(imo,idly)
c     ret1        = return in this time step
c
c     pobl(kk,iplan) =future returns in month kk for 
c                     plan iplan
c                     note kk is a circular pointer that
c                     is a function of the  maximum
c                     return interval e.g if max is 5, then
c                     at time 1 kk=1-5 at time 2 kk=2-4,1, 
c                     etc.
c     poblD(kk,iplan)=same as above but for daily
c
c		  pdem(np)       =running demand for this month. It 
c		  	              increases or decreases based on opr 
c                                 rules
c		  pdemT(np)      =total demand this month (may increase 
c                                 but will not decrease based on
c                                 operating rules
c		  pdemTM(np)     =same as above but track for daily analysis
c     
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
c     write(6,*)    ' RtnsecP;'
c     write(nlog,*) ' RtnsecP;'
cc
c		iout = 1 print detailed data
c		       2 print summary return data for current time 
c		       3 print summary data for current and future time
c		       4 print details on Standard and Fixed table types
c		       5 print details on tracking return locations 
c                        under separate T&C Plans
c   ioutP = Plan pointer for detailed data

      iout = 0
      ioutP = 0
      ichkwr = 1
      l2 = l2
      ireop=0
      iprintr=0
      ipid=0
      

      ret1=0.0
      rett=0.0
      pct1=0.0
      small=0.001
      smalln=-1*small
      retX1=0.0
      

      if(iday.eq.0) then
        fac=factor*mthday(mon) 
      else  
        fac=1.0
      endif
c
c _________________________________________________________
c
c               Step 2 - Loop for number of return flow locations
c
c		Allow plan return flow data
      if(iprf(iplan).eq.999) then
        IRNI=NRTN(IUSE)
        IRNE=NRTN(IUSE+1)-1
c       IF(IRNI.GT.IRNE) goto 500
      else
        irni=nrtnPP(iplan)
        irne=nrtnPP(iplan+1)-1
      endif    
c
c ---------------------------------------------------------
c		Detailed Output
      
      if(iout.eq.1 .and. iplan.eq.ioutP) then
        write(nlog,100) iplan, iprf(iplan), irni, irne, 
     1    rettot*fac
      endif
 100    format(/, 72('_'),/ '  RtnsecP;',/
     1  '   iplan    iprf    irni    irne  rettot',/ 4i8, 20f8.1)    
c
c ---------------------------------------------------------
c rrb 2007/08/22; Enhance Performance      
      if(rettot.lt.small) goto 500
c _________________________________________________________
c
c               Step 3 - Calculate return to location irn (const)
c                        and delay table (idly). Note even though
c			 the location is not needed; this calculation 
c                        is required to get the proper return table ID
c
      DO 150 IRN=IRNI,IRNE

        if(iprf(iplan).eq.999) then
          CONST=RETTOT*PCTTOT(IRN)/10000.
          idly=irtndl(irn)
          pct1=pcttot(IRN)          
        else
          idly=irtndlPP(irn)
          CONST=RETTOT*pcttotPP(IRN)/10000.
          pct1=pcttotPP(IRN)
        endif  
c
c ---------------------------------------------------------
c rrb 2007/11/28
c		Skip if the percent is NEGAITVE since that 
c		indicates the pattern is not for a STANDARD return
        if(pct1.lt.smalln) then
          if(iout.eq.4) write(nlog,*)
     1      ' RtnSecP; Standard Return skipping ', irn, pct1
          goto 150
        endif  
c
c _________________________________________________________
c
c               Step 4 - Calculate return flows 
c                        in month 1 or day 1 (ret) by delay table dlyrat
c			 Adjust demand (pdem) for this month
c
        if(iday.eq.0) then
          RET=CONST*DLYRAT(1,IDLY)
          dlyrat1=dlyrat(mon,idly)          
        else
          ret =const*dlyratd(1,idly)
          dlyrat1=dlyratd(1,idly)
        endif
c
c		Note pdem is current demand (may go up or down)
c		     pdemT() is total demand this day or month 
c		             (can only go up)
          
cr      pdem(iplan) =pdem(iplan) + ret
cr      pdemT(iplan)=pdemT(iplan) + ret
c
c		Adjust return for water left at headgate

        pdem1=pdem(iplan)
        retX= amax1 (0.0, pdem(iplan)+ret-DivLeft)
c
c ---------------------------------------------------------
c		Set Total Plan Data (iplan)       
        pdem(iplan) =retX        
        pdemT1=pdemT(iplan)
        retX= amax1 (0.0, pdemT(iplan)+ret-DivLeft)        
        pdemT(iplan)=retX
        retX1=retX1+ret
        if(iout.eq.1) write(nlog,*) ' RtnsecP; retX1 ', retX1*fac
c
c ---------------------------------------------------------
c rrb 2007/12/26; Set return location plan data (ipID) if 
c		  not the same as the Total Plan
c       IPID=IrnstaPP(IRN)
        IPID=irnPlan(irn)
        
        if(iplan.ne.ipid) then
          pdem(ipID) = pdem(ipID) + ret        
          pdemT1=pdemT(ipID)
          pdemT(ipID)= pdemT(ipID) + ret
          pdrive(ipiD) = pdriveX
          if(iout.eq.5) then
            if(iprintr.eq.0) write(nlog,304)
            iprintr=iprintr+1
            write(nlog,312)  iyr, mon, mon, iplan, ipid, irn, kk,
     1       RetX*fac
          endif
        endif  
c        
c ---------------------------------------------------------
c		Detailed Output
        if((iout.ge.1 .and.iout.le.3) .and. ioutP.eq.iplan) then
          if(iprintr.eq.0) write(nlog,302)
          iprintr=iprintr+1
c
          write(nlog,312)  iyr, mon, mon, iplan, ipid, irn, 1, 
     1    rettot*fac, pct1, ret*fac, dlyrat1, DivLeft*fac, retx*fac, 
     1    pdem1*fac, Pdem(iplan)*fac, PdemT1*fac, PdemT(iplan)*fac
        endif
     
 302  format(/'  RtnsecP; Plan Obligation Summary',/
     1 '  iyr  mon  mon ipln ipid irn   kk',
     1 '    RetTot    PctTot       Ret   DlyRat1   DivLeft      Retx',
     1 '     Pdem1     Pdem2    PdemT1    PdemT2',/
     1 ' ____ ____ ____ ____ ____ ____ ____',
     1 ' _________ _________ _________ _________ _________ _________',
     1 ' _________ _________ _________ _________')

 304  format(/'  RtnsecP; Plan Obligation Summary',/
     1 '  iyr  mon  mon ipln ipid  irn   kk      RetX',/
     1 ' ____ ____ ____ ____ ____ ____ ____ _________')
 312  format(7i5, 20f10.2)
c
c _________________________________________________________
c
c               Step 5 - Calculate future return obligations (pobl1)
  130   IM=0
        IEND=IMO+ndly(idly)-1
c
c _________________________________________________________
c
c 		Step 5a - Monthly returns
        if(iday.eq.0) then
          DO K=IMO,IEND
            IM=IM+1
c
c               Adjust monthly model for # of days in a month
            imx = mon+im-1
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
c
c rrb 2007/12/26; Track T&C oblibations by location
            pobl(kk,iplan)=pobl(kk,iplan) + ret
            if(ipid.ne.iplan) then
              pobl(kk,ipid)=pobl(kk,ipid) + ret
            endif
            
c
            if(iout.eq.3) then
              if(iprintr.eq.0) write(nlog,300)
              write(nlog,310)  iyr, mon, imo, iplan, kk, 
     1          pobl(kk,iplan)*fac, rett*fac, rettot*fac,
     1          (rett-rettot)*fac
              iprintr=1
            endif
c
c               End Loop for number of return flow time intervals
          end do
c
c               Print results for detailed checking
          if(iout.eq.3 .and. l2.eq.ichkwr) then
              write(nlog,318)
              write(nlog,320) 1, imo-1,
     1                        (pobl(k,iplan)*fac, k=1,imo-1)
              write(nlog,320) imo, ndlymx, 
     1                       (pobl(k,iplan)*fac, k=imo,ndlymx)
          endif
        end if

c _________________________________________________________
c
c rrb 98/03/17; Daily return capability
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

            poblD(kk,iplan)=poblD(kk,iplan)+ret
c
c rrb 2007/12/26; Track T&C oblibations by location
            if(ipid.ne.iplan) then
              poblD(kk,ipid)=poblD(kk,ipid) + ret
            endif
c
c               End Loop for number of return flow time intervals
          end do
c
c               Print results for detailed checking
          if(iout.eq.2 .and. l2.eq.ichkwr) then
              write(nlog,*) '  RtnsecP; ido, ndlymx', ido, ndlymx
              write(nlog,'(10f8.2)') (poblD(k,iplan)*fac, k=1,ndlymx)
          endif
        endif
c
c               End Loop for number of return flow locations
  150 CONTINUE
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

 300  format(/'  RtnsecP; Plan Obligation Summary',/
     1 '  iyr  mon  imo ipln   kk      pobl      rett',
     1                          '    rettot     delta',/
     1 ' ____ ____ ____ ____ ____ _________ _________',
     1                          ' _________ _________')
 310  format(5i5, 20f10.2)
 
 318  format(/, '  RtnsecP; Obligation array')

 320  format('  From ', i5, ' To ', i5, 10f8.2)             
c
c
c _________________________________________________________
c
c               Error Processing
 200  write(nlog,210) icx, iplan
 210  format('  RtnsecP; Problem when called by routine # ', i5,/
     1       ' iplan = ', i15)
                                      
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop
      END





