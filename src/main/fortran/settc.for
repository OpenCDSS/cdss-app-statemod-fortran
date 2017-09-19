       Subroutine SetTC(nlog, icx, l2, ipTC, icu, fac, 
     1 divactT, CuFac, divleft, rettot, iopsou4,
     1 pdemX, pdriveX, cstaid1)
     
       character cstaid1*12
c
c _________________________________________________________
c		Documentation
c	     nlog	       log file #     
c	     icx	       ID of Subroutine call     
c	     l2	         Water right
c	     ipTC	       T&C Plan 
c	     icu	       0=Diversion, 1=Depletion
c	     fac	       factor cfs = af
c	     divactT	   diversion amount
c	     CUFac	     CU Factor (fraction)
c	     Divleft     
c	     RetTot	     Total Return
c	     Iopsou4     iopsou(4,l2)
c                  1 STANDARD return for Return
c		               2 FIXED return for a diversion        
c                  3 MIXED return for a diversion         		
c	     pdemX	      pdem(ipTC)
c	     cstaid1     water right calling this routine	  
c
c _________________________________________________________
c rrb 2007/11/27; Update T&C calculations

      iout=0
c      
c _________________________________________________________
c		Set Rettot and divLeft      
      rettot = divactT *(1.0-CuFac)      
      if(icu.eq.0) divleft=0.0
      if(icu.eq.1) divleft=rettot
c        
c _________________________________________________________
c		Set demand (pdem), driver (pdriveX), etc        
      pdem2=pdemX
      pdriveX=pdriveX + divactT
c
c _________________________________________________________
c		Standard Return Component
      if(iopsou4 .ge. 1 .and. iopsou4 .le.3) then          
        call rtnsecP(icx, ipTC, ipTC, l2, RETTOT, divLeft, retX1, 
     1    pdriveX)
      endif  
c _________________________________________________________
c		Fixed Return Component
      if(iopsou4.ge.2 .and. iopsou4.le.3) then
        divleft=0.0
        call rtnsecC(icx, ipTC, ipTC, l2, DIVACTT, divLeft, retX2,
     1    pdriveX)
      endif
      
c _________________________________________________________
c		Detailed Output
      if(iout.eq.1) then
        write(nlog,200) cstaid1, icx, ipTC, 
     1   divactT*fac, CuFac*100.0, rettot*fac,
     1   divLeft*fac, retX1*fac, retX2*fac, pdemX*fac,
     1   pdriveX*fac     
     
      endif
      
c _________________________________________________________
c		Formats
  200 format(
     1  '  SetTC; Cstaid      ',
     1  '     icx    ipTC DivactT   CuFac  Rettot',
     1  ' DivLeft   RetX1   RetX2   PdemX PdriveX',/
     1  '         ', a12, 2i8, 20f8.0)
      return
      end
