c
c *********************************************************
c
C     Last change:  C    20 May 97    0:00 am
C
      SUBROUTINE IFRRIG(IW,L2,ncallx)
c
c _________________________________________________________
c	      Program Description
c
c _________________________________________________________
c
c       Update History
c       Copied Ifrig.for and made appropriate edits
c
c _________________________________________________________
c	      Documentation
c
c
c _________________________________________________________
c	      Dimensions
c
      include 'common.inc'
      character ctype1*12  
      character cwhy*48, cdestyp*12, ccarry*3, cpuse*3          
c
c _________________________________________________________
c
c

c		iout = 0 no details
c		       1 details
c                      2 summary  
      isub=201    
      iout=0
      ioutiw=0

      iout=0      
      if(ichk.eq.201) iout=2
      if(cisfwr(l2).eq. ccall) ioutiw=iw      
      
      if(iout.eq.2 .and. ioutiw.eq.iw) then
        write(nlog,*) '  Ifrrig; ', iout, cisfwr(l2), ccall
      endif
c      
c        find station code of i.f.r. right
      nf  =iifrco(l2)
      ifcd=ifrsta(nf)
      ishort=0
         
      cwhy='N/A'
      cdestyp='N/A'
      ccarry='No'
      cpuse='No'
      availX=-1.    
      mon2 = imonsw(l2,mon)    
c
c rrb 98/08/10; Convergence Update
      small=0.001
c
c               Additional information for detailed printout
      f = mthday(mon) * factor                       
      actwrq  = 0.0
      actwrq1 = 0.0
c
c               Check demand
      aloifr1 = 0.0
      aloifr=amin1(dcrifr(l2)-divi(l2),flowrq(nf))
      if(aloifr.lt.small) goto 100
c
c rrb 2011/02/06; Exit if the ISF is off
      if (IFRRSW(nf).eq.0) then
        iwhy=4
        cwhy='Instream Structure is off'        
        goto 100
      endif        
c
c               Check flow
c rrb 04/22/96; Instream Flow reach capability
c     actwrq=amin1(aloifr,avail(ifcd))
      ndns=ndnifs(nf)
      call dnmfso(maxsta, avail, idncod, ifcd, ndns, imcd)
      avail1 = avail(imcd)   
      actwrq=amin1(aloifr,avail(imcd))
c
c		Set shortage
      if(actwrq.lt.aloifr-small) ishort=1      

      if(-iopout.eq.ifcd) then
          write(nlog,*) ' '
          write(nlog,'(10f8.0)') (avail(i)*f, i=1,numsta)
      endif

c     if(actwrq.lt.(.01)) goto 100
      if(actwrq.lt.small) goto 100
c
c rrb 04/22/96; Instream Flow reach capability
      call takou2(isub, maxsta, avail ,idncod, actwrq, ndns, ifcd)
c
c               Update available flow (avail),
c               demand (flowrq), unmet right (divi), and 
c               accounting (qdiv)
c
c     avail1       = avail(ifcd)   
      flowrq1      = flowrq(nf)    
      divi1        = divi(l2)      

c
c rrb 04/22/96; Instream Flow reach capability
c     avail(ifcd)  = avail(ifcd)   - actwrq
      flowrq(nf)   = flowrq(nf)    - actwrq
      divi(l2)     = divi(l2)      + actwrq
      qdiv(14,ifcd)= qdiv(14,ifcd) + actwrq

cx    write(nlog,*) '  Ifrrig', nf, flowrq(nnf)*fac, actwrq*fac      
c                                                

 100  continue
c
c_____________________________________________________________
c               Step 13; Identify call (note only if short)
c
      if(ishort.gt.0 .and. nf.gt.0) then
        ctype1='InstreamFlow'
cr      call GetCall(ifcd, imcdI(nf), nf, ctype1)        
        call GetCall(ifcd, imcdL(ifcd), nf, ctype1)        
      endif  
c 
c_____________________________________________________________
c               Step 13; Detailed printout
c     if(-iopout.eq.ifcd) then
      if(iout.eq.2 .and. iw.eq.ioutiw) then
        ncallX=ncallX+1
        if(ncallX.eq.1) then
          write(nlog,270) corid(l2), cdestyp, ccarry, cpuse
        else
          write(nlog,*) ' '
        endif  
           
      
        write(nlog,510) 
        write(nlog,520)
     1    iyr,mon,mon2,nf,ifcd,imcd,
     1    xfrnam1(nf),
     1    dcrifr(l2)*f,  divi1*f,  flowrq1*f, aloifr1*f,
     1    avail1*f, actwrq1*f

        write(nlog,530)
     1    iyr,mon,mon2,nf,ifcd,imcd,
     1    xfrnam1(nf),
     1    dcrifr(l2)*f,  divi(l2)*f, flowrq(ifcd)*f, aloifr*f,
     1    avail(imcd)*f, actwrq*f

        write(nlog,'(10f8.0)') (avail(i)*f, i=1,numsta)
      endif
      
c
c _________________________________________________________
c
c		Return
      return
c
c _________________________________________________________
c
c		Formats      
 510    format(' Ifrrig  ',
     1    '   Yr   Mo   On   nf ifcd imcd Name                     ',
     1    '  dcrifr    divi  flowrq  aloifr   avail',
     1    '  actwrq',/9x, 
     1    ' ---- ---- ---- ---- ---- ------------------------ ',
     1    6(' -------'))
     
 270   format(/, 
     1  ' Ifrrig (Type 1); Operation Right ID = ', a12,
     1  ' Destination Type = ', a12,
     1  ' Carrier (Y/N) = ',a3, ' Reuse (Y/N) = ', a3/ 
     1    '                                           ',
     1    '                          ',
     1    '  Decree Decree2  Demand     Min',
     1    '   Avail  Divert',/,
     1    ' In/Out    Yr   Mo   On ifcd imcd',
     1    ' Name                     ',
     1    '  dcrifr    divi  flowrq  aloifr',
     1    '   avail  actwrq',/8x,
     1    ' ____ ____ ____ ____ ____',
     1    ' ________________________ ',
     1    6(' _______'))     
 520    format(
     1 '  In    ', 5i5, 1x, a24, 2x, 20f8.0)
 530    format(
     1 '  Out   ', 5i5, 1x, a24, 2x, 20f8.0)
      
      
      end


