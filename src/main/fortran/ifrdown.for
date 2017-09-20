C
      SUBROUTINE IfrDown(IwTemp,L2Temp,l1,fx,ncloc,dcallx)
c
c
c _________________________________________________________
c	Program Description
c
c       IfrDown; Type 23
c               It calculates instream flow for a downstream
c               call. Note same as ifrrig2.for except it is
c               called for a variable admin date.
c
c	Called by Execut
c ---------------------------------------------------------
c
c	iwTemp = water right counter
c	iwx    = iteration counter
c	l2Temp = pointer to downstream call water right
c	il     = Calling location from Execute
c		1=ISF
c		2=reservoir
c		3=direct diversion
c		6=well
c	       99=before moving to new time step
c	dcall1 = admin number at this time step 
c		 set in bomsec for monthly and dayset for daily
c	dcallx = admin number it was comapred to befor calling
c       ncloc  = calling location
c                1 = before type 1 instream flow
c                2 = before type 2 instream reservoir
c                3 = before type 3 diversion
c                5 = before type 5 opr right
c                6 = before type 6 well 
c _________________________________________________________
c
c       Update History
c rrb 01/29/95 Simplified logic per R. Bethel & R. Bennett
c rrb 04/22/96 Added instream reach capabilities by minimum 
c rrb 08/07/96 Instream Flow reach by reach node
c _________________________________________________________
c
c       Documentation
c
c       l2Temp                  Pointer for 
c			             downstream call operation right
c       iwTemp                  Temporary water right counter
c       nf=iifrco(l2Temp)       instream structure pointer
c       ifcd=ifrsta(nf)         river location of instream structure
c       dcrifr(l2Temp)          water right from riginp.f
c
c       divi(l2Temp)            amount diverted by this right 
c       divir(l2Temp,i)         amount diverted by this right
c                               at reach node i
c
c       flowrq(nf)              demand remaining this time step
c       florqr(i)               demand remaining at reach node i
c
c       qdiv(14,ifcd)           instream diversion at river node ifcd
c       qdivr(i)                instream diversion at reach node i
c
c       ndns = ndnifs(nf)       number of downstream nodes in the reach
c       ndnifb(nf)              beginning counter for storing reach info
c
c _________________________________________________________
c	Dimensinos
      include 'common.inc'
      character cwhy*48
      real*8 dcallx
c
c _________________________________________________________
c
c
c               Initilize
c
c		iout = detailed output
c		     = 0 None
c		     = 1 summary
      iout=0      
      if(ichk.eq.123) iout=1
      
      IwTemp = iwTemp

      nf=iopdes(1,l2Temp)
      ifcd=ifrsta(nf)                         
      f = mthday(mon) * factor              
      actmin = 99999. 
      actwrq = 0.0
      iss=ifrsta(nf)
c      
c
c  test      
c     iopout= -1*ifcd
      if(-iopout.eq.ifcd) then
        write(nlog,*) ' '
        write(nlog,*) '  IfrDown; ncloc, iwTemp, l2Temp ' 
        write(nlog,*) '  IfrDown; ',ncloc, iwTemp, l2Temp
      endif  
 
      ib=ndnifb(nf)
      ie=ndnifb(nf) + ndnifs(nf) - 1
c
c rrb 98/08/10; Convergence Update
      small=0.001
      
c      
c
c               Detailed printout
      if(-iopout.eq.ifcd) then
          write(nlog,*) ' '
          write(nlog,*) '  IfrDown; iopout, ifcd ', iopout, ifcd
          write(nlog,*) '  IfrDown; iwTemp, l2Temp, nf, ib, ie ',
     1      iwTemp,l2Temp,nf,ib,ie
c         write(nlog,'(10f8.0)') (avail(j)*f, j=1,numsta)
      endif
c
c _________________________________________________________
c
c               Step X; Branch if not on this month
c
c rrb 04/22/96; Allow month switch
      if(imonsw(l2Temp,mon).eq.0) then
        iwhy=1
        cwhy='Monthly switch is off'
        goto 500
      endif
c
c ---------------------------------------------------------
c		For a daily model set demand for beginning of season
      if(iday.eq.1 .and. imonsw(l2Temp,mon).gt.0) then
        if (idy.lt.imonsw(l2Temp,mon)) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 500
        endif  
      endif  
c
c ---------------------------------------------------------
c		For a daily model set demand for end of season
      if(iday.eq.1 .and. imonsw(l2Temp,mon).lt.0) then
        if (idy.gt.iabs(imonsw(l2Temp,mon))) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 500
        endif  
      endif  
      
c
c               Instream Reach Loop
      do 200 i=ib,ie             
c
c _________________________________________________________
c
c               1. Initilize
        actwrq  = 0.0
        actwrq1 = 0.0
        aloifr1 = 0.0
c
c _________________________________________________________
c
c               2. Demand
c 		Demand is via *.dda or *.ddm
        aloifr=amin1(dcrifr(l2Temp)-divir(l2Temp,i),florqr(i))
        aloifr=florqr(i)
        if(aloifr.lt.small) goto 100
c
c _________________________________________________________
c
c               3. Available Flow
        avail1 = avail(iss)   
        actwrq=amin1(aloifr,avail(iss))
        actwrq=avail(iss)
        if(actwrq.lt.small) goto 100
c
c _________________________________________________________
c
c               4. Update Avail Array
        avail(iss) = avail(iss)-actwrq
c
c _________________________________________________________
c
c               5. Update 
        flowrq1 = florqr(i)    
        divi1 = divir(l2Temp,i)
        florqr(i) = florqr(i) - actwrq
        qdivr1=qdivr(i)
        divir(l2Temp,i) = divir(l2Temp,i)   + actwrq
        qdivr(i) = qdivr(i)   + actwrq
cr      write(nlog,*) ' IfrDown;', i, qdivr1*fac, qdivr(i)*fac
        
c
c _________________________________________________________
c
c               6. Identify minimum
 100    if(actwrq.lt.actmin) then
          actmin = actwrq
          imcd = iss
        endif
c                                                
c _________________________________________________________
c
c               7. Detailed printout
        if(-iopout.eq.ifcd) then
          write(nlog,110) 
          write(nlog,120)
     1      iyr,mon,i,nf,ifcd,imcd,iss,
     1      xfrnam1(nf),
     1      dcrifr(l2Temp)*f,  divi1*f,  flowrq1*f, aloifr1*f,
     1      avail1*f, actwrq1*f

          write(nlog,130)
     1      iyr,mon,i,nf,ifcd,imcd,iss,
     1      xfrnam1(nf), 
     1      dcrifr(l2Temp)*f,  divir(l2Temp,i)*f, florqr(i)*f, aloifr*f,
     1      avail(iss)*f, actwrq*f
        endif
c
c _________________________________________________________
c
c               8. Identify next downstream river node
        iss=idncod(iss)
 200  continue
c                                   
c _________________________________________________________
c
c               9. Set structure data to minimum
      flowrq(nf)   = flowrq(nf)    - actmin
      divi(l2Temp)     = divi(l2Temp)      + actmin
      qdiv(14,ifcd)= qdiv(14,ifcd) + actmin
c
c rrb Test                
cr    write(nlog,*) 'IFrdown; qdiv(14,17)', qdiv(14,17)
c
c
c              10. Update diversion by this Operating Rule
      Divo(l2Temp)=divo(l2Temp)+actmin

c _________________________________________________________
c
c              11. Print min results
      if(-iopout.eq.ifcd) then
        write(nlog,*) ' '
        write(nlog,140)
     1      iyr,mon,i,nf,ifcd,imcd,imcd,
     1      xfrnam1(nf),
     1      dcrifr(l2Temp)*f,  divi(l2Temp)*f, flowrq(nf)*f, aloifr*f,
     1      avail(imcd)*f, actmin*f

c       write(nlog,'(10f8.0)') (avail(j)*f, j=1,numsta)
      endif
     
      if(iout.eq.1) then
        write(nlog,300) iyrmo(mon), xmonam(mon), idy, iwtemp, iwx, 
     1    l2temp, l1, dcall1, dcallx, divo(l2temp)*fx
 300    format(/,'  IfrDown; for ', i5, 1x, a4, 1x, i3,/
     1   '    IwTemp                  = ', i8,/
     1   '    Iteration               = ', i8,/ 
     1   '    L2Temp                  = ', i8,/
     1   '    Calling location (l1)   = ', i8,/
     1   '    Downstream call Admin # = ', f8.0,/
     1   '    Called prior to Admin # = ', f8.0,/     
     1   '    Total Diverted (af)     = ', f8.0)     
      endif
c _________________________________________________________
c
c
c               Formats                         
 110  format(/,'  IfrDown ',
     1    '   Yr   Mo Rech   nf ifcd imcd  iss',
     1    ' Name                     ',
     1    '  dcrifr    divi  flowrq  aloifr   avail',
     1    '  actwrq',/10x, 
     1    ' ---- ---- ---- ---- ---- ---- ----',
     1    ' ------------------------ ',
     1    6(' -------'))

 120  format(
     1 '  In      ', 7i5, 1x, a24, 2x, 20f8.0)
 130    format(
     1 '  Out     ', 7i5, 1x, a24, 2x, 20f8.0)
 140    format(
     1 '  Min     ', 7i5, 1x, a24, 2x, 20f8.0)
c _________________________________________________________
c
 500  return
      end





