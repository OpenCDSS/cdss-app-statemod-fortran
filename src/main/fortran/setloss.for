c
c *********************************************************
c     Last change:  RRB  18 Dec 100    2:29 pm
c
      SUBROUTINE SetLoss(nlog, icx, l2, fac, 
     1 maxopr,   intern,  Oprloss, OprLossC,
     1 ioprloss, nCarry,  nRiver,  ncnum, 
     1 OprLost,  OprEff1, OprEffT, TranLoss, internT, internL, corid1)
c
c
c _________________________________________________________
c	Program Description
c
c		It initilizes loss data for various operating rules
c
c _________________________________________________________
c		Update History
c
c
c rrb 2008/06/10; Allow return to river then diversion
c		  from a carrier again (internT = 1 Carrier, 2=Return)        

c 2008/01/04; Added nRiver to indicate a carrier release to river
c
c _________________________________________________________
c		Documentaiton
c
c	     OprLoss(l2) =  Transit loss (%) 
c	    		            Transit loss is a true loss no routing
c	     ioprloss    = int(OprLoss) carrier loss switch
c	                		+ transit loss, maybe carrier loss
c	                		- 0 transit loss, maybe carrier loss
c	     intern(l2,i)   Intervening structure ID
c	    	            	+ = diversion pointer
c	    	            	- = stream pointer
c	     internT(l2,i)  Intervening structure type
c	                		1 = Carrier
c	                		2 = River
c	     internL        Last Intervening Structure Type
c	                		1 = Carrier
c	                		2 = River
c	     TranLoss    =  Transit loss (fraction)
c	     OprLossC(l2,i) Conveyance loss from a carrier (%)
c	              		  Conveyance loss gets routed to system
c	     OprLost= 	    conveyance loss (cfs)
c                     
c	     OprEff1 = 	    source Carrier Efficiency 
c                                 (1.0 - OprLoss(l2)/100)
c	     OprEffT = 	    Total Carrier Efficiency 
c            
c	     effmaxT=	      Transit loss for a ditch 
c     
c	     nCarry         Indicator at least 1 carrier
c	     nRiver		      Indicator release to the River
c	     ncnum          # of intervening structures (Carriers & Returns)
c	     corid1         Operating rule ID
c
c _________________________________________________________
c	Dimensions
c     
c rrb 2011/05/31; Revise dimension to 20
cx    dimension Oprloss(maxopr), intern(maxopr,10), internT(maxopr,10), 
cx   1 OprlossC(maxopr,10)
      dimension Oprloss(maxopr), intern(maxopr,20), internT(maxopr,20), 
     1 OprlossC(maxopr,20)

      character corid1*12
c
c
c _________________________________________________________
c		Step 1;  Initilize
      iout=0
      
      ioprloss=int(OprLoss(l2))
c
c rrb 2008/06/10; 
      ncar=intern(l2,1)
      
      if(ncar.gt.0) nCarry=1
      nRiver=0
      OprLost=0.0
      small=0.001
      internL=0      
      
      if(ioprloss.gt.0) then
        TranLoss=OprLoss(l2)/100.
      else
        TranLoss=0.0  
      endif
c
c rrb 2010/10/15; Correction for a water balance
c                 report system (tranloss)at the destination;
c                 not befor water is carried. 
cx    OprEff1 = 1.0 - TranLoss   
      OprEff1 = 1.0  
      OprEffT = OprEff1
c
c
c _________________________________________________________
c		Step 2; Carrier Losses and return to river
      ncnum=0
      if(ncarry.gt.0) then
        do i=1,10
c         if(iout.eq.1) write(nlog,*) ' SetLoss; ',l2, i, 
c    1      intern(l2,i), internT(l2,i), OprlossC(l2,i)
     
          ncar=intern(l2,i)
          
c
c rrb 2008/06/10; Allow return to river then diversion
c rrb 2008/06/11; Note when ncar>0 the structure type internT()
c		  must be a Carrier (checked in Oprfind)       
c		  Note internT = 1 = Carrier & 2=Return)        
c
          if(internT(l2,i).eq.1) then
            ncnum=ncnum+1
            EffmaxT1=(100.0 - OprLossC(l2,i))/100.0
            OprEffT=OprEffT*EffmaxT1
            internL=1
          endif
          
          if(internT(l2,i).eq.2) then  
            nRiver=ncar            
            ncnum=ncnum+1
            internL=2
          endif
c
c ---------------------------------------------------------
c		Detailed Output
          if(iout.eq.1) then
            if(i.eq.1) write(nlog,200)
            if(ncar.gt.0) write(nlog,210) 
     1        '  SetLoss;  ', icx,  l2,  nCarry, nRiver, internL,ncnum,
     1         intern(l2,i), internT(l2,i), 
     1         OprLost, OprEff1, OprEffT, TranLoss
          endif            
        end do
      endif
c
c ---------------------------------------------------------
c rrb 2007/12/05; Warn if losses exceed 100%
      if(oprEffT.le.small) then
        write(nlog,910) corid1, OprEffT
        goto 900
      endif      
c
c rrb 2008/06/10; Set carrier type Ncarry
c                 0 No carrier
c		              1 No return to River, Final Diversion from carrier
c	                2 Return to River, Final Diversion from a carrier
c		              3 Return to River, Final Diversion from the river
      ncarry1=ncarry
c
c rrb 2008/06/29; Correction      
cx      if(ncarry.gt.0 .and. internL.eq.0) ncarry=1
cx      if(ncarry.gt.0 .and. internL.eq.1) ncarry=2
cx      if(ncarry.gt.0 .and. internL.eq.2) ncarry=3
      
      if(ncarry1.gt.0 .and. nriver.eq.0 .and. internL.eq.1) ncarry=1
c
c rrb 2014-07-29; Revise to allow return to river to work in Type 45 (DivCarL)  
cx      if(ncarry1.gt.0 .and. nriver.gt.0 .and. internL.eq.1) ncarry=2
cx     if(ncarry1.gt.0 .and. nriver.gt.0 .and. internL.eq.2) ncarry=3
      if(ncarry1.gt.0 .and. nriver.gt.0 .and. internL.eq.1) ncarry=3
      if(ncarry1.gt.0 .and. nriver.gt.0 .and. internL.eq.2) ncarry=2
            
      if(iout.ge.1) then
        write(nlog,*) ' SetLoss; ncarry1 internL  ncarry'
        write(nlog,'(10x, 20i8)') ncarry1, internL, ncarry
      endif  
            
c
c _________________________________________________________
c		Step 3; Return
      return 
      
      
c
c _________________________________________________________
c		Formats      
 200  format(/,72('_'),/ 
     1 '  SetLoss;  ',
     1 '       icx        l2    nCarry    nRiver    internL     ncnum',
     1 '    intern   internT   OprLost   OprEff1    OprEffT  TranLoss',/
     1 12x, 11(' _________'))
 210  format(a12,8i10, 20f10.2)
c
c_____________________________________________________________
c               Error warnings
c
 900  write(6,920) 
      write(nlog,930)  
 910  format(
     1 '  SetLoss; Problem with operating rule ', a12,/
     1 '           The net carrier efficiency = ', f10.3,/
     1 '           Reconmend you revise losses or turn off',/
     1 '           the operating rule (delivery will be zero')     
 920  format('    Stopped in SetLoss',/,
     1       '    See the *.log file')
 930  format('    Stopped in SetLoss')
      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)
      end
