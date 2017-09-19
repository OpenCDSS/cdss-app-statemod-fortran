c
c *********************************************************
c
      SUBROUTINE RtnsecC(icx, iplan, iuse, l2, divactT, divLeft,
     1  retX2, pdriveX)
c
c
c
c _________________________________________________________
c	Program Description
c
c       RtnsecC; It calculates FIXED future return obligations
c                for a plans. Note:
c		    It turns off future returns when a zero value 
c		      occurrs after a non-zero value
c		    It operates on the amount diverted (divactT) not
c		      unused water like most return calculations
c		    The fixed return pattern assumes the first return
c		      value in the return pattern corresponds to 
c		      January (calendar year calculations), the next
c		      to February, etc.
c		    A fixed return pattern is given a negative % value 
c
c		    icx      calling routine icx = type
c                                   24=directEx,  25=directBY, 
c                                   27=DivResP2, 38=OopDiv
c		    iplan	   plan pointer
c		    iuse	   return flow pointer
c		    l2       water right pointer
c		    divactT  diversion this time step
c		    divLeft  diversion left at headgate
c		    	         = 0 for a diversion type = Diversion 
c		    	         = divactT for a diversion type = Depletion
c		    RetX2    immediate return flow
c		    pdriveX  Total plan drivec
c
c _________________________________________________________
c
c       Update History
c rrb 2007/11/28; Revise to skip return % > 0 (if <0 they refer
c		  to a FIXED return pattern)
c rrb 2007/09/25; Revise to allow returns to extend up
c		  to 12 months of the following year
c		  Note it will stop when a zero
c		  value is encountered after a non-zero value
c rrb 2007/08/22; Copy RtnsecP and edit. In summary edits
c		  replace circular array that varies with time
c		  to a fixed fraction for a given month.
c
c _________________________________________________________
c        Documentation
c               icx    =      subroutine called by
c                             1=carrpl, 2=divcar, 3=divcar1, 4=divres
c                             5=divrig, 6=divrpl, 7=vircom,  8=divcar2,
c                             9=directEx, 10=directBY, 11=welrig
c               l2     =      water right counter unless called by
c                             divres, divrpl, or directex then its 
c                             the opr. counter
c               iuse   =      diversion user
c               idcd   =      river station ID where the diversion
c                             is located.  Note set to 0
c                             for baseflow operation
c               nd     =      diversion ID  
c
c               const       = return flow to a given location
c
c               dlyrat(im,idly) =  % return in month im for table idly
c               dlyratd(id,idly)=  % return in day id for table idly
c               diveff( )   = diversion efficiency  
c
c
c               iout        = 0 no detailed output, 1 yes detailed output
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
c               ret         = return flow to a location in a month
c                             ret = const * dlyrat(imo,idly)
c               ret1        = return in this time step
c
c               pobl(kk,iplan) =future returns in month kk for 
c                               plan iplan
c                               note kk is a circular pointer that
c                               is a function of the  maximum
c                               return interval e.g if max is 5, then
c                               at time 1 kk=1-5 at time 2 kk=2-4,1, 
c                               etc.
c               poblD(kk,iplan)=same as above but for daily
c
c		pdem(np)       =running demand for this month. It 
c				increases or decreases based on opr 
c                               rules
c		pdemT(np)      =total demand this month (may increase 
c                               but will not decrease based on
c                               operating rules
c		pdemTM(np)     =same as above but track for daily analysis
c
c
c _________________________________________________________
c	Dimensions

      include 'common.inc'
c
c
c _________________________________________________________
c
c               Step 1 - Initilize
c
c     write(6,*)    ' RtnsecC;'
c     write(nlog,*) ' RtnsecC;'
cc
c
c ---------------------------------------------------------
c		iout = 1 print detailed data
c		       2 summary data for current time step
c		       3 print summary data for current and future time
c		       4 print detials on Standard and Fixed table types
      iout = 0
      ichkwr = 0
      l2 = l2
      ireop=0
      iprintr=0
      ioff=0
      im=0
      imx=0
      ipid=0

      ret1=0.0
      rett=0.0
      pct1=0.0
      
      retX2=0.0
      
c
c rrb; 2007/09/21; Revise to handle up to 12 months
c		   Make small very small for daily checks.
      small=0.00001
c
c ---------------------------------------------------------

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
        IF(IRNI.GT.IRNE) goto 500
      else
        irni=nrtnPP(iplan)
        irne=nrtnPP(iplan+1)-1
      endif    
c
c ---------------------------------------------------------
c		Check the number of patterns specified
      ic=irne-irni+1
      if(ic.le.0) then
        write(nlog,300) ic
        goto 900
      endif         
      
c
c ---------------------------------------------------------
c		Detailed Output
      if(iout.eq.1) write(nlog,100) iplan, iprf(iplan), irni, irne,
     1  divactT*fac  
 100    format(/, 72('_'),/ '  RtnsecC;',/
     1  '   iplan    iprf    irni    irne  divactT',/ 4i8, 20f8.1)    
      
c
c ---------------------------------------------------------
c rrb 2007/08/22; Enhance Performance      
      if(divactT.lt.small) goto 500
c _________________________________________________________
c
c               Step 3 - Calculate return to location irn (const)
c                        and delay table (idly). Note even though
c			 the location is not needed; this calculation 
c                        is required to get the proper return table ID
c
      DO 150 IRN=IRNI,IRNE

        
        if(iprf(iplan).eq.999) then
          idly=irtndl(irn)
          CONST=divactT*pcttot(IRN)/10000.
          pct1=pcttot(IRN)
        else
          idly=irtndlPP(irn)
          CONST=divactT*pcttotPP(IRN)/10000.
          pct1=pcttotPP(IRN)
        endif  
c
c ---------------------------------------------------------
c rrb 2007/11/28
c		Skip if the percent is positive since that 
c		indicates the pattern is not for a FIXED return
c		Adjust the data to a positive value
        if(pct1.gt.small) then
          if(iout.eq.4)  write(nlog,*) 
     1     ' RtnSecC; FIXED Return skipping ', irn, pct1
          goto 150
        else
          pct1=-1.*pct1
          const=-1.0*const
        endif
c
c
c _________________________________________________________
c
c               Step 4 - Calculate return flows (ret) in month 1 
c                          or day 1 (ret) by delay table dlyrat
c			 Adjust demand (pdem) for this month
c			 Note dlyrat is a fixed return based on the
c                          month or day
c
c
        if(iday.eq.0) then
          RET=CONST*DLYRAT(mon,IDLY)
          dlyrat1=dlyrat(mon,idly)
        else
          ret =const*dlyratd(idy,idly)
          dlyrat1=dlyrat(mon,idly)
        endif
        dlyrat0=dlyrat(mon,idly)
        
        rett=rett+ret
        
c
c		Note pdem is current demand (may go up or down)
c		     pdemT() is total demand this day or month 
c		             (can only go up)
c
c		Adjust return for water left at headgate
        pdem1=pdem(iplan)
        retX= amax1 (0.0, pdem(iplan)+ret-DivLeft)
        pdem(iplan) =retX
        
        pdemT1=pdemT(iplan)
        retX= amax1 (0.0, pdemT(iplan)+ret-DivLeft)        
        pdemT(iplan)=retX
        
        retX2=retX2+ret
c
c ---------------------------------------------------------
c rrb 2007/12/26; Set return location plan data (ipID) if 
c		  not the same as the Total Plan
c       IPID=IrnstaPP(IRN)
        IPID=irnPlan(irn)
        
        if(iplan.ne.ipid) then
          pdem(ipID) =pdem(ipID) + ret        
          pdemT1=pdemT(ipID)
          pdemT(ipID)=pdemT(ipID) + ret
          pdrive(ipiD) = pdriveX
        endif  
c
c ---------------------------------------------------------
c       
        
        if(iout.ge.1 .and. iout.le.3) then
          if(iprintr.eq.0) write(nlog,302)
          iprintr=iprintr+1
c
c         write(nlog,*) ' '
          im=mon
          write(nlog,312)
     1      iplan, ipID, iyr, mon, im, imo, imx, irn, 1, 0,
     1      divactT*fac, pct1, ret*fac, dlyrat0, dlyrat1, 
     1      DivLeft*fac, retx*fac, pdem1*fac, Pdem(iplan)*fac, -1.0
          write(nlog,*) ' '
     
        endif
        
        
c
c _________________________________________________________
c
c               Step 5 - Calculate future return obligations (pobl1)
c rrb 2007/09/21; Extend up to 12 months
        IM=0
        IEND=IMO+ndly(idly)-1
c
c _________________________________________________________
c
c 		Step 5a - Monthly returns
        if(iday.eq.0) then
c
c rrb 2007/08/22; Loop through this month to end of year        
c rrb 2007/09/21; Extend up to 12 months
c         do k=mon+1,12
c           im=k
c           c = 1.0
          
          ioff=0
          iNonZero=0
          do k=imo,iend
            im=im+1
c
c               Adjust monthly model for # of days in a month
            imx = mon+im-1
            
            ixe=imx/12+1
            do ix=1,ixe
              if(imx.gt.12) then
                imx=imx-12
c               write(nlog,*) ' Adjusting imx ', 
c    1           imx, mthday(mon), mthday(imx)
              endif                  
            end do
c
c		Adjust for fixed month            
            if(im.gt.12) then
              im=1
              write(nlog,*) ' Adjusting im ', im
            endif
            

            c  = float(mthday(mon))/float(mthday(imx))
c
c ---------------------------------------------------------
c rrb 2007/09/21; Extend up to 12 months
c 		Turn off remainder of year when a small value is encountered 
            dlyrat0=dlyrat(imx,idly)
            if(dlyrat0.gt.small) inonZero=1
            if((dlyrat0.lt.small.and.iNonZero.eq.1).or.ioff.eq.1) then
              ioff=1
              c=0.0
            endif  
c
c rrb 2007/09/24; Adjust for Constant Return            
c           ret=const*dlyrat(im,idly)*c
c           dlyrat0=dlyrat(im,idly)
c           dlyrat1=dlyrat(im,idly)*c

            ret=const*dlyrat(imx,idly)*c
            dlyrat1=dlyrat(imx,idly)*c

            
            rett=rett+ret
            KK=K
c
c               Check for wrap around
            IF(K.GT.ndlymx) then
              KK=K-ndlymx
            endif
c
            pobl(kk,iplan)=pobl(kk,iplan) + ret
c
c ---------------------------------------------------------
c rrb 2007/12/26; Track T&C oblibations by location
            if(ipid.ne.iplan) then
              pobl(kk,ipid)=pobl(kk,ipid) + ret
            endif
            
c
            if(iout.eq.3) then
              if(iprintr.eq.0) write(nlog,302)
              iprintr=iprintr+11
c
c rrb 2007/08/22; Revise to use a FIXED return percent
              write(nlog,312)  
     1          iplan, ipID, iyr, mon, im, imo, imx, irn, kk, ioff, 
     1          divactT*fac, pct1, ret*fac, dlyrat0, dlyrat1, 
     1          DivLeft*fac,   retx*fac, 
     1          pdem1*fac, Pdem(iplan)*fac, pobl(kk,iplan)*fac
            endif
c
c               End Loop for number of return flow time intervals
          end do
c
c               Print results for detailed checking
          if(iout.eq.2 .and. l2.eq.ichkwr) then
              write(nlog,318)
c
              write(nlog,320) 1, mon,
     1                        (pobl(k,iplan)*fac, k=mon,12)
          endif
        end if

c _________________________________________________________
c
c rrb 98/03/17; Daily return capability
c
        if(iday.eq.1) then
          ioff=0
          id=0
          c=1.0
c          
c rrb 2007/09/21; Extend up to 12 months
c         iend=365
c         do k=idy+1,iend
c           id=k
          id=idy-1

          iend=ido+365-1          
          do k=ido,iend
            id=id+1
c
c ---------------------------------------------------------
c rrb 2007/09/21; Extend up to 12 months
c 		Turn off remainder of year when a small value is encountered            
            if(dlyratd(id,idly).lt.small .or. ioff.eq.1) then
              ioff=1
              c=0.0
            endif  
c
            ixe=id/365+1
            do ix=1,ixe
              if(id.gt.365) then
                id=id-365
              endif
            end do

            ret=const*dlyratd(id,idly)*c
c
c               Check for wrap around
            kk=k
            if(k.gt.ndlymx) then
              kk=k-ndlymx
            endif

            poblD(kk,iplan)=poblD(kk,iplan)+ret
c
c ---------------------------------------------------------
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
              write(nlog,*) '  RtnsecC; ido, ndlymx', ido, ndlymx
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
 300  format(/
     1 '  RtnSecC; Problem',/
     1 '           For a Mixed Return Pattern two and only two',/
     1 '           return tables are allowed. Number provided = ', i5)
     
 302  format(/
     1 '  RtnsecC; Plan Obligation Summary',/
     1 '           Note: mon = Current time step (month),',/ 
     1 '                 im  = Future time steps',/
     1 '                 imo = Circular counter for return array',/     
     1 '                 imx = Month used to adjust days per month',/
     1 'iplan  ipid  iyr  mon   im  imo  imx  irn   kk ioff',
     1 '   divactT    Pcttot      Ret   DlyRat0    DlyRat1   DivLeft',
     1 '      Retx     Pdem1     Pdem2      Pobl',/
     1 ' ____ ____ ____ ____ ____ ____ ____ ____ ____ ____',
     1 ' _________ _________ _________ _________ _________ _________',
     1 ' _________ _________ _________ _________')
    
 312  format(10i5, 20f10.2)
 
     
     
 318  format(/, '  RtnsecC; Obligation array')

 320  format('  From ', i5, ' To ', i5, 10f8.2)             
c
c
c _________________________________________________________
c
c               Error Processing
 900  write(nlog,910) icx, iplan
 910  format('  RtnsecC; Problem when called by routine # ', i5,/
     1       ' iplan = ', i15)
                                      
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop
      END





