c
C
      SUBROUTINE IFRRIG2(IW,L2,ncallx)
c
c
c _________________________________________________________
c	Program Description
c
c       Ifrrig2; It calculates instream flow for a reach
c               Similar to ifrrig
c _________________________________________________________
c       Update History
c
c rrb 01/29/95 Simplified logic per R. Bethel & R. Bennett
c rrb 04/22/96 Added instream reach capabilities by minimum 
c rrb 08/07/96 Instream Flow reach by reach node
c _________________________________________________________
c
c       Documentation
c
c       iw                      water right loop counter
c       l2                      instream right counter
c       nf=iifrco(l2)           instrem structure counter
c       ifcd=ifrsta(nf)         river location of instream structure
c       dcrifr(l2)              water right from riginp.f
c
c       divi(l2)                amount diverted by this right 
c       divir(l2,i)             amount diverted by this right
c                                 at reach node i
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
c	Dimensions
      include 'common.inc'
      character ctype1*12    
      character cwhy*48, cdestyp*12, ccarry*3, cpuse*3, cstaid1*12
c
c _________________________________________________________
c		Step 0; Initilize
c		iout = Detailed output based on control data
c		       0 no details
c		       1 details
c                      2 summary      
c		ioutX = Detailed output based on compiling code
c		       0 no details
c		       1 summary
      iout=0
      ioutiw=0
      ioutX=0
      
      if(ichk.eq.201) iout=2      
      if(cisfwr(l2).eq. ccall) ioutiw=iw      

      if((iout.eq.2 .and. iw.eq.ioutiw) .or. ioutX.eq.1) then
        write(nlog,*) '  Ifrrig2; ', iout, cisfwr(l2), ccall
        ncallX=ncallX+1
        if(ncallX.eq.1) then
          write(nlog,270) corid(l2), cdestyp, ccarry, cpuse
        else
          write(nlog,*) ' '
        endif  
      endif  

      
      iw = iw
      ishort=0
      iwhy=0
      cwhy='NA'
      
      cstaid1=cisfwr(l2)
      
      cwhy='NA'
      cdestyp='NA'
      ccarry='No'
      cpuse='No'
      availX=-1.  
      mon2 = imonsw(l2,mon)
      
      nf  =iifrco(l2)
      ifcd=ifrsta(nf)                         
      f = mthday(mon) * factor              
      actmin = 99999. 
      actwrq = 0.0
      iss=ifrsta(nf)

      ib=ndnifb(nf)
      ie=ndnifb(nf) + ndnifs(nf) - 1
c
c rrb 98/08/10; Convergence Update
      small=0.001
c
c               Detailed printout
      if(-iopout.eq.ifcd) then
          write(99,*) ' '
          write(99,*) '  Ifrrig2; iw, l2, nf, ib, ie ',iw,l2,nf,ib,ie
c         write(99,'(10f8.0)') (avail(j)*f, j=1,numsta)
      endif
c
c rrb 2011/02/06; Exit if the ISF is off
      if (IFRRSW(nf).eq.0) then
        iwhy=4
        cwhy='Instream Structure is off'        
        goto 130
      endif        
c
c               Instream Reach Loop
      do 200 i=ib,ie             
c
c _________________________________________________________
c
c               Step 1. Initilize
        actwrq  = 0.0
        actwrq1 = 0.0
        aloifr1 = 0.0
c
c _________________________________________________________
c
c               Step 2. Demand
        aloifr=amin1(dcrifr(l2)-divir(l2,i),florqr(i))
        if(aloifr.lt.small) then
          iwhy=1
          cwhy='Decree or Demand=0'
          goto 100
        endif  
c
c _________________________________________________________
c
c               Step 3. Available Flow
        avail1 = avail(iss)   
        actwrq=amin1(aloifr,avail(iss))
        if(actwrq.lt.small) then
          iwhy=2
          cwhy='Available flow (avail) = 0'
          goto 100
        endif
c
c _________________________________________________________
c
c               Step 4. Update Avail Array
        avail(iss) = avail(iss)-actwrq
c
c _________________________________________________________
c
c               Step 5. Update 
        flowrq1 = florqr(i)    
        divi1 = divir(l2,i)
        florqr(i) = florqr(i) - actwrq
        qdivr1=qdivr(i)
        divir(l2,i) = divir(l2,i)   + actwrq
        qdivr(i) = qdivr(i)   + actwrq
cr      write(nlog,*) ' Ifrrig2;', i, qdivr1*fac, qdivr(i)*fac
c
c _________________________________________________________
c
c               Step 6. Identify minimum
 100    if(actwrq.lt.actmin) then
          actmin = actwrq
          imcd = iss
        endif
c                                                
c _________________________________________________________
c
c               Step 7. Detailed printout
c       if((iout.eq.2 .and. iw.eq.ioutiw) .or. ioutX.eq.1) then
        if(iout.eq.1) then
          write(nlog,520)
     1      iyrmo(mon),xmonam(mon), iday, iwx,
     1      i,nf,ifcd,imcd,iss,
     1      cstaid1,
     1      dcrifr(l2)*f,  divi1*f,  flowrq1*f, aloifr1*f,
     1      avail1*f, actwrq1*f, iwhy, cwhy

          write(nlog,530)
     1      iyrmo(mon),xmonam(mon), iday, iwx, 
     1      i,nf,ifcd,imcd,iss,
     1      cstaid1,
     1      dcrifr(l2)*f,  divir(l2,i)*f, florqr(i)*f, aloifr*f,
     1      avail(iss)*f, actwrq*f, iwhy, cwhy
        endif
c
c _________________________________________________________
c
c               Step 8. Identify next downstream river node
        iss=idncod(iss)
 200  continue
c                                   
c _________________________________________________________
c
c               Step 9. Set structure data to minimum
      flowrq(nf)   = flowrq(nf)    - actmin
      divi(l2)     = divi(l2)      + actmin
      qdiv(14,ifcd)= qdiv(14,ifcd) + actmin
c      
cx    write(nlog,*) '  Ifrrig2; nf, flowrq(nf)', nf, flowrq(nf)*fac
c        
c_____________________________________________________________
c
c		Step 10. Set shortage
 130  if(actmin.lt.aloifr-small) ishort=1              
                         
c
c_____________________________________________________________
c               Step 11; Identify call (note only if short)
c
      if(ishort.gt.0 .and. nf.gt.0) then
        ctype1='InstreamFlow'      
        call GetCall(ifcd, imcdL(ifcd), nf, ctype1)        
      endif  
c
c _________________________________________________________
c
c              Step 12. Print min results
c
c     if(-iopout.eq.ifcd) then
      if((iout.eq.2 .and. iw.eq.ioutiw) .or. ioutX.eq.1) then
cx      ncallX=ncallX+1
cx      if(ncallX.eq.1) then
cx        write(nlog,270) corid(l2), cdestyp, ccarry, cpuse
cx      else
cx        write(nlog,*) ' '
cx      endif  
        
c       write(nlog,*) ' '
        write(nlog,540)
     1      iyrmo(mon),xmonam(mon), iday, iwx,
     1      mon2,nf,ifcd,imcd,imcd,
     1      cstaid1,
     1      dcrifr(l2)*f,  divi(l2)*f, flowrq(nf)*f, aloifr*f,
     1      avail(imcd)*f, actmin*f, iwhy, cwhy

c       write(nlog,'(10f8.0)') (avail(j)*f, j=1,numsta)
      endif
c _________________________________________________________
c
c
c               Formats                         
     
 270   format(/, 
     1  ' Ifrrig2 (Type 1); Operation Right ID = ', a12,
     1  ' Destination Type = ', a12,
     1  ' Carrier (Y/N) = ',a3, ' Reuse (Y/N) = ', a3/ 
     1    ' In/Out    Yr   Mo  Day  Iwx    #   nf ifcd imcd  iss',
     1    ' ID           ',
     1    '  dcrifr    divi  flowrq  aloifr',
     1    '   avail  actmin    iwhy cwhy',/8x,
     1    9(' ____'),  ' ____________ ',
     1    6(' _______')' _______ ', 45('_'))
     
     

 520  format(
     1 '  In    ', i5, 2x,a3, 7i5, 1x,a12,1x, 6f8.0, i8, 1x, a45)
 530    format(
     1 '  Out   ', i5, 2x,a3, 7i5, 1x,a12,1x, 6f8.0, i8, 1x,a45)
 540    format(
     1 '  Min   ', i5, 2x,a3, 7i5, 1x,a12,1x, 6f8.0, i8, 1x,a45)
c _________________________________________________________
c
      return
      end





