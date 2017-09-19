cc
c *********************************************************
c     Last change:  RRB  18 Dec 100    2:29 pm
c
      SUBROUTINE SetCarL(nlog, icx, l2, fac, 
     1 maxopr,  maxdiv, intern, OprLossC,
     1 ncarry,  ncnum,  noprS,  internT,
     1 OprEff1, DivCap, DivMon, DivCarry, DivaloX)
c
c
c _________________________________________________________
c	Program Description
c
c		It calculates the limiting carrier capacity (DivCarry)
c		and its impact on the demand (divaloX) for 
c		various operating rules
c _________________________________________________________
c		Update History
c
c rrb 2008/06/10; Allow return to river then diversion
c		  from a carrier again (internT = 1 Carrier, 2=Return) 
c _________________________________________________________
c		Documentation
c
c		ncarry is indicator at least 1 carrier
c		ncnum is the number of carriers (set in SetLoss)
c		OprEff1 is the lost (oprlost(lw)
c		Divalo gets reduced by carrier capacity
c		DivCarry is the limitating carrier capacity
c
c		noprS is the structure id of the structure
c		  that supplied water to the accounting
c		  plan that already has a capacity adjustment     
c
c _________________________________________________________
c	Dimensions
c
      dimension 
     1 intern(maxopr,20), OprlossC(maxopr,20), internT(maxopr,20),
     1 divcap(maxdiv), divmon(maxdiv) 
c
c
c
c _________________________________________________________
c		Step 1;  Initilize
c
      iout=0
      ifound=0
c
c rrb 2007/05/25; Add carrier Loss (throughout this section)     
      DivaloX1=DivaloX
      DivCarry=DivaloX
      CapRemX=99999./fac
            
      if(ncarry.gt.0) then      
        EffmaxT1=OprEff1
        do i=1,ncnum
          ncar=intern(l2,i)
c
c rrb 2008/06/10; Allow return to river then diversion
c		  from a carrier again (internT = 1 Carrier, 2=Return)        
          if(internT(l2,i).eq.1) then
c
c rrb 2008/06/11; Note when ncar>0 the structure type must be
c		  a Carrier (checked in Oprfind)       
c		  Note internT = 1 = Carrier & 2=Return)        
            if (ncar.gt.0 .and. ncar.ne.noprS) then
      
              divcap1=amax1((divcap(ncar)/EffmaxT1)-divmon(ncar), 0.0)
              divcap1=amax1((divcap(ncar)-divmon(ncar))/EffmaxT1, 0.0)
              CapRemX=amin1(divcap1, CapRemX)
            
              if (divaloX.gt.divcap1) then
                divaloX=amax1(0.0, divcap1)
                divcarry=divaloX
              endif  
            
              effmaxTx=effmaxT1
              EffmaxT1=EffmaxT1*(100.0 - OprlossC(l2,i))/100.0
            endif
c
c ---------------------------------------------------------
c		Check calculations            
            if(iout.eq.1) then            
              ifound=1
              if(i.eq.1) write(nlog,200)
              write(nlog,210)
     1          '  SetCarL;  ',icx, ncarry, noprS, i, ncar,
     1          internT(l2,i),
     1          OprLossC(l2,i), effmaxTX*100.,effmaxT1*100.,
     1          divcap(ncar), divcap1*fac, CapRemX*fac,
     1          divmon(ncar)*fac, DivaloX1*fac, DivaloX*fac         
            endif
          endif  
        end do  
      endif
c
c ---------------------------------------------------------
c		Check calculations            
      if(ifound.eq.0) then
        if(iout.eq.1) then  
          i=1          
          if(i.eq.1) write(nlog,200)
          write(nlog,210)
     1      '  SetCarL;  ',icx, ncarry, noprS, i, ncar
        endif
      endif            
      
c
c _________________________________________________________
c		Step 3; Return
      return 
c
c _________________________________________________________
c		Formats    
c
c rrb 2007/05/25; Add carrier Loss      
 200  format(/, 72('_'),/
     1  '  SetCarL;  ',
     1  '     icx  ncarry   noprS       i    ncar internT',
     1  '  OprLossC  effmaxTX  effmaxT1',
     1  '    divcap   divcap1   CapRemX    Divmon  DivaloX1   DivaloX',/
     1  ' ___________', 6(' _______'), 9(' _________'))         
 210  format(a12, 6i8, 20f10.0)   
      
      end
