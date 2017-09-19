c
c *********************************************************

        Subroutine SetQdivX(
     1    nlog,    ncarry,   ncnum,     nd,      nd2,
     1    l2,      iscd,     idcdX,     idcdC,   nriver, 
     1    divactX, TranLoss, EffmaxT1,  fac,     maxsta, 
     1    maxdiv,  maxqdiv,  maxopr,    intern,  idvsta, 
     1    qdiv,    divmon,   maxRtnPP,   maxPlan, Opreff1, 
     1    ipuse,   pctlosPP,  rlossP,  oprLossC,  internT, 
     1    icx,      corid1)
c
c
c _________________________________________________________
c	Program Description
c
c	  SetQdivX, same as SetQidvC (It sets qdiv data for carriers and 
c	  return to River) but it DOES NOT calculate return flows 
c	  from any carrier losses
c
c	  Note:
c	    1. If the source is also a carrier it is handled 100%
c	    	in the Calling Program and SetQdiv.
c	    2. If the destination is the a carrier, it is handled
c		100% in SetQdiv
c	    3. If the carrier is a return to river (ncar=intern(l2,i)<0    
c		nothing occurrs
c
c _________________________________________________________
c	Documentation
c
c	        nCarry   0 No carrier
c			 1 No return to River, Final Destination from a carrier
c	        	 2 Return to River, Final Destination from a carrier
c		 	 3 Return to River, Final Destination from the river
c		ncnum= number of carriers
c		nd   = source diversion
c		nd2  = destination diversion, reservoir, or plan
c
c		oprloss1 = transit efficiency (set in SetLoss)
c		iscd = diversion location
c               idcdX = stream ID of destination diversion (nd2) or reservoir
c			 or plan (NOT CARRIER)
c               idcdC = stream ID of destination carrier 
c
c		divactX=amount diverted (cfs)
c
c		qdiv(5	From River by Priority
c		qdiv(18 Carrier passing through a structure
c
c               qdiv(20 From Carrier by Storage or Exchange
c		qdiv(32 From Carrier Loss
c
c		qdiv(26 From River by Exc_Pln (Exc_Pln)
c
c		qdiv(31 From River by Sto/Exc/Plan by type 27 or 28
c
c		qdiv(32  From Carrier loss from a carrier (DivCar, DivcarL) 
c		qdiv(33  From River loss from a diversion by the river
c                        (DivCar, DivcarL, DirectBy, DirectEx) 
c
c		qdiv(36  Water released to the river (report as
c			 return flow)
c
c _________________________________________________________
c	Dimensions
c
        dimension idvsta(maxdiv)
        dimension qdiv(maxqdiv, maxsta)
        dimension intern(maxopr,10), internT(maxopr,10)
        dimension pctlosPP(maxRtnPP)
        dimension rlossP(maxPlan)
        dimension oprlossC(maxopr,10)
        dimension divmon(maxdiv)
        character corid1*12
c
c _________________________________________________________
c		Step 1; Initilize        
c		iout=0 No detailed output
c                   =1 Details
        iout=0
        
        small=0.001
        
        if(iout.eq.1) then
c         write(nlog,250) 
          write(nlog,*) ' '
          write(nlog,*) ' SetQdivX; icx, ncarry ncnum nd',
     1      icx,ncarry,ncnum,nd
          call flush(nlog)          
        endif  
        
        if(ncarry.eq.0) goto 150
c
c _________________________________________________________
c		Step 2; Set Loss data
        
        OprLos1=divactX*TranLoss
        OprLos2=(divactX-Oprlos1)*(1.0-EffmaxT1)
        OprLos3=OprLos1+OprLos2
        
        divactC=divactX*OprEff1
c
c _________________________________________________________
c		Step 3; Set Carrier Data
c			Note nlast=0 no prior return to River
c			     ilast=1 prior return to River
        nlast=0
        do i=1,ncnum
          OprLosT=0.0          
          icase=0
          ncar=intern(l2,i)     
c
c rrb 2007/01/03; Allow intern(l2,i)<0 to be a river ID            
c
c rrb 2008/06/10; Revise code (internT = 1 Carrier, 2=Return) 
          if(internT(l2,i).eq.1) then   
c
c rrb 2008/11/20; Correction. When the Carrier is the Source or
c		    the Carrier is the Destination divmon
c		    gets adjusted in the calling program.
c		    Therefore copy to case 3
cx          divmon(ncar)=divmon(ncar)+divactC     
            EffmaxT1=(100.0-OprLossC(l2,i))/100.0
          
            inode=IDVSTA(ncar)
            OprLosT=divactC*(1.0-effmaxT1)
c
c _________________________________________________________
c		Case 1; Carrier is the Source 
c		Note diversion from river (source data)
c		 is set in SetQdiv 
            if(inode.eq.iscd) then
              icase=1            
              goto 100
            endif  
c
c _________________________________________________________
c		Case 2 Carrier is the Destination (diversion from the river)
C		Note idcdX is the final destination (NOT THE CARRIER)
c
            if(inode.eq.idcdX) then
               icase=2
               goto 100
            endif  
c
c _________________________________________________________
c		Case 3 Simple Carrier
c rrb 2008/02/04;	Set Loss             
c		qdiv(18 Carrier passing through a structure
c		qdiv(31 From River by Sto/Exc/Plan by type 27 or 28
c		qdiv(33 From River loss 
c               qdiv(20 From Carrier by Storage or Exchange
c		qdiv(32 From Carrier Loss
c

            icase=3
c
c rrb 2008/11/20; Moved from above to only occurr for case 3            
            divmon(ncar)=divmon(ncar)+divactC                 
            if(nlast.eq.0) then
              qdiv(18,inode)=qdiv(18,inode)+ divactC - oprlosT
              qdiv(31,inode)=qdiv(31,inode)+ divactC 
              qdiv(33,inode)=qdiv(33,inode)+ OprLosT
            else
c
c rrb 2008/06/29; Clarify the source for type 45 (DivcarL)            
cx            qdiv(20,inode)=qdiv(20,inode)+ divactC 
              ndest=20
cx	      if(icx.eq.45) ndest=19	  	          
              qdiv(ndest,inode)=qdiv(ndest,inode)+ divactC 
              
              qdiv(18,inode)=qdiv(18,inode)+ divactC - oprlosT
              qdiv(32,inode)=qdiv(32,inode)+ OprLosT
            endif  
c
c _________________________________________________________
c		Step 4;	Calculate return flows from transit losses
c		Intvn = diversion ID or Use, Inode=stream location     

 100        if(OprLosT.gt.small) then
cx              if (corid1(1:11).eq.'OpThBurl.01') then 
cx                write(nlog,*) 'SetQdivX; ', corid1
cx                write(nlog,*) '  ncar, inode, DivactX, OprLosT'
cx                write(nlog,*) '  ',  ncar, inode,  
cx     1            DivactX*fac, OprLosT*fac
cx              endif
c
c rrb 2009/01/24; Remove the following which calculates return flows
c		    from carrier losses (the only difference between
c		    SetQdivX and SetQdivC
cx              CALL RtnXcu(icx,OprLosT,L2,ncar,Inode,ncar)
            endif  
c          
c		  Define Carried water for next carrier
            divactC=divactC*EffmaxT1        
            nlast=1
          endif
c
c _________________________________________________________
c		Step 5;	Set Releases back to the River
          
          if(internT(l2,i).eq.2) then
            icase=4
            inode=intern(l2,i)
            qdiv(36,inode)=qdiv(36,inode) + divactC
            nlast=0
c           write(nlog,*) ' SetQdivX; qdiv(36', i, l2,inode,divactC*fac
          endif  
c
c
c ---------------------------------------------------------
c		Detailed output
          if(iout.eq.1) then
            if(i.eq.1) write(nlog,200)
            write(nlog,210) icx, corid1, 
     1        i, icase, ncar, inode, iscd, idcdX, idcdC,
     1        divactX*fac, OprEff1*100., divactC*fac,  
     1        EffmaxT1*100, OprlosT*fac,
     1        qdiv(31,inode)*fac, qdiv(33,inode)*fac,
     1        qdiv(20,inode)*fac, qdiv(32,inode)*fac
            call flush(nlog)
          endif  
        end do  
c
c _________________________________________________________
c		Step 5; Return        
 150    return
c
c _________________________________________________________
c		Formats        
 200   format(
     1 '  SetQdivX; ',
     1 '  icx CorID1      ',
     1 '         i     icase      ncar     inode      iscd',
     1 '     idcdX     idcdC   divactX   oprEff1   divactC',
     1 '   EffmaxT1   OprlosT   qdiv(31   qdiv(33   qdiv(20',
     1 '   qdiv(32',/
     1 ' ___________ ____ ____________', 16(' _________'))
 210   format(12x, i5,1x,a12, 7i10, 20f10.0)    
 250   format(/,72('_'))       
        end
