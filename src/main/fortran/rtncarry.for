c
c *********************************************************
c
c rrb 2010/11/01; Revise to pass iplan
        Subroutine RtnCarry(
     1    nlog, ncarry, ncnum, nd, nd2, 
     1    l2, iscd, idcdX,idcdC,iplan,
     1    nriver, divactX, TranLoss, EffmaxT1, fac, 
     1    maxdiv, maxqdiv, maxopr, intern, idvsta, 
     1    maxrtnw, maxdivw,  Opreff1,  oprLossC, internT, 
     1    icx, corid1)
c
c _________________________________________________________
c	Program Description
c
c	  It calculates return flows for Carrier Losses
c
c
c _____________________________________________________________
c	Update	History
c
c
c      2009/01/24; Copied SetQdivC and removed refrences to qdiv
c
c
c _____________________________________________________________
c	Documentation
c
c      2009/01/24; Copied SetQdivC and removed refrences to qdiv
c
c	  Note:
c	       nCarry  = 0 No carrier
c			             1 No return to River, Final Destination from a carrier
c	            	   2 Return to River, Final Destination from a carrier
c		 	             3 Return to River, Final Destination from the river
c		     ncnum    = number of carriers
c		     nd       = source diversion
c		     nd2      = destination diversion, reservoir, or plan
c
c		     oprloss1 = transit efficiency (set in SetLoss)
c		     iscd     = diversion location
c        idcdX    = stream ID of destination diversion (nd2) or reservoir
c			              or plan (NOT CARRIER)
c        idcdC    = stream ID of destination carrier 
c
c		     divactX  =amount diverted (cfs)
c
c _____________________________________________________________
c	Dimensions
c     
        dimension idvsta(maxdiv)
        dimension intern(maxopr,10), internT(maxopr,10)
        dimension oprlossC(maxopr,10)
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
          write(nlog,*) ' RtnCarry; icx, ncarry ncnum nd',
     1      icx,ncarry,ncnum,nd
          call flush(nlog)          
        endif  
        
        if(ncarry.eq.0) goto 150
c
c _________________________________________________________
c		Step 2; Set Loss data
c
c rrb 2010/10/15; Correction TranLoss (system loss) occurrs
c                 at the destination        
cx      OprLos1=divactX*TranLoss
        OprLos1=0.0
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
          if(internT(l2,i).eq.1) then   
            EffmaxT1=(100.0-OprLossC(l2,i))/100.0
            inode=IDVSTA(ncar)
            OprLosT=divactC*(1.0-effmaxT1)
c
c _________________________________________________________
c		Case 1; Carrier is the Source 
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
c			Calculate return flows from transit losses
c		       Intvn = diversion ID or Use, Inode=stream location     
            icase=3
 100        if(OprLosT.gt.small) then
c
c rrb 2010/11/01; Add call loss to a plan capability
cx         CALL RtnXcu(icx,OprLosT,L2,ncar,Inode,ncar)
            if(iplan.eq.0) then            
              CALL RtnXcu(icx,OprLosT,L2,ncar,Inode,ncar)
            else
              call RtnXcuP(icx, OprLosT,L2,ncar,Inode,ncar,iplan)
            endif
              
cx              if (corid1(1:11).eq.'OpThBurl.01') then 
cx                write(nlog,*) 'RtnCarry; ', corid1
cx                write(nlog,*) '  ncar, inode,  DivactX, OprLosT'
cx                write(nlog,*) '  ',  ncar, inode,  
cx     1            DivactX*fac, OprLosT*fac
cx              endif
              
            endif  
c          
c		  Define Carried water for next carrier
            divactC=divactC*EffmaxT1        
            nlast=1
          endif
c
c ---------------------------------------------------------
c		Detailed output
          if(iout.eq.1) then
            if(i.eq.1) write(nlog,200)
            write(nlog,210) icx, corid1, 
     1        i, icase, ncar, inode, iscd, idcdX, idcdC,
     1        divactX*fac, OprEff1*100., divactC*fac,  
     1        EffmaxT1*100, OprlosT*fac
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
     1 '  RtnCarry; ',
     1 '  icx CorID1      ',
     1 '         i     icase      ncar     inode      iscd',
     1 '     idcdX     idcdC   divactX   oprEff1   divactC',
     1 '   EffmaxT1   OprlosT',/
     1 ' ___________ ____ ____________', 12(' _________'))
 210   format(12x, i5,1x,a12, 7i10, 20f10.0)    
 250   format(/,72('_'))       
        end
