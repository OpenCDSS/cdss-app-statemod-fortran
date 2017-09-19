

      subroutine SetLimit(
     1    nlog, icx, lopr, ipLim, ioprlimX, fac,
     1    divact,  OprmaxMX,  OprMaxAX, 
     1    OprmaxX, Oprmax13, OprmaxM1, OprmaxM2, 
     1    psto1x,  psto2X, coridX)
c
c
c rrb 2009/01/23; Revise to operate on storage (psto2), not 
c	plan supply (psuplyX)
c _________________________________________________________      
c		Documentation
c	   SetLimit adjusts monthly and annual release limits 
c		 for operating rule (lopr)
c
c   Called by: DivResp2; DivCarL; DivRplP; & RsrSpuP
c	
c       If iOprLimX     = 1 limits are are increased
c          iOprLimX     = 2 limits are decreased and
c                           if(iplim>0) plan supplies are decreased
c       nlog            = log file
c       icx             = calling routine (100+opr rule type)
c       lopr            = associated operating rule with limits
c       iplim           = associated plan (if any)
c       ioprlimX        = 1 limits are increased
c                       = 2 limits are decreased and 
c                           if(iplim>0) plan supplies are decreased
c       fac             = factor cfs to af/mo
c       
c       divact          = diversion (cfs)
c       oprmaxMX        = running monthly limit this time step initial
c       oprmaxAX        = running annual limit this time step initial
c
c       oprmaxX         = total monthly limit (value read) 
c       oprmax13        = total annual limit (value read)
c
c       oprmaxM1        = monthly or annual limit initial
c       oprmaxM2        = monthly or annual limit adjusted

     
      character coridX*12
c
c _________________________________________________________      
      iout=0
      small=0.001
c
c _________________________________________________________      
c               Step 2; Adjust monthly or annual diversion limits UP
        
      if(iOprLimX.eq.1 .and. lopr.gt.0) then
        oprmaxM1=oprmaxMX
        oprmaxMX=oprmaxMX + divact*fac
        oprmaxMX=amin1(oprmaxMX, oprmaxX)
        OprmaxM2=oprmaxMX

        oprmaxA1=oprmaxAX
        oprmaxAX=oprmaxAX + divact*fac
        oprmaxAX=amin1(oprmaxAX, oprmax13) 
        
        if(iout.eq.1 .and. abs(divact).gt.small) then
          write(nlog,*) 
     1      ' SetLimit; Adjusting Monthly and Annual Limits UP'
          write(nlog,'(a12, 1x,a12,1x, 4i5, 20f8.0)') 
     1      ' SetLimit; ',coridX, icx, ioprLimX, lopr, iplim,
     1        oprmaxM1, divact*fac, oprmaxMX,
     1        OprmaxA1, OprmaxAX, psto1X, psto2X
        endif       
      endif
c
c _________________________________________________________
c
c               Step 3; Adjust monthly or annual release limits DOWN
c
c rrb 2011/10/15; Allow a type 4
cx    if(iOprLimX.eq.2 .and. lopr.gt.0) then
      if((iOprLimX.eq.2 .or. iOprLimX.eq.4).and. lopr.gt.0) then
      
cx      oprmaxM1=amin1(oprmaxMX, oprmaxAX)
cx      oprmaxMX=amax1(oprmaxMX - divact*fac, 0.0)
        oprmaxM1=oprmaxMX
        oprmaxMX=amax1(oprmaxMX - divact*fac, 0.0)
        
        oprmaxA1=oprmaxAX
        oprmaxAX=amax1(oprmaxAX - divact*fac, 0.0)
        
        OprmaxM2=amin1(oprmaxMX, oprmaxAX)
c
c _________________________________________________________      
c
c		Step 4; Tie to plan for reporting
c		Note OprmaxA(lopr) = psuply(iplim)
        if(iplim.gt.0) then
c
c rrb 2009/01/23; Revise to operat on storage only         
cx          psuply1=psuplyX
cx          psuplyX=amax1(psuplyX - divact, 0.0)
cx          psuply2=psuplyX        
          
          psto2X=psto2X-divact*fac          
        endif 
        
        if(iout.eq.1 .and. abs(divact).gt.small) then
          write(nlog,*) ' '
          write(nlog,*) 
     1      ' SetLimit; Adjusting Monthly and Annual Limits Down'
          write(nlog,'(a12, 1x,a12,1x, 4i5, 20f8.0)') 
     1      '  SetLimit; ',coridX, icx, ioprlimX, lopr,iplim, 
     1      oprmaxM1, divact*fac, oprmaxMX, 
     1      OprmaxA1, OprmaxAX,
     1      psto1X, psto2X                
        endif
      endif
c
c _________________________________________________________      
c
c		Step 5; Return      
      return 
      end
