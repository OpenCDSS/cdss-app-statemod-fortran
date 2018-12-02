c setqdiv - sets Qdiv for destinations from various operating rules
c           and the source only if it is equal to the destination
c           (e.g.  only when iscd .eq. idcdX).
c           IT DOES NOTHING FOR CARRIERS.
c_________________________________________________________________NoticeStart_
c StateMod Water Allocation Model
c StateMod is a part of Colorado's Decision Support Systems (CDSS)
c Copyright (C) 1994-2018 Colorado Department of Natural Resources
c 
c StateMod is free software:  you can redistribute it and/or modify
c     it under the terms of the GNU General Public License as published by
c     the Free Software Foundation, either version 3 of the License, or
c     (at your option) any later version.
c 
c StateMod is distributed in the hope that it will be useful,
c     but WITHOUT ANY WARRANTY; without even the implied warranty of
c     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c     GNU General Public License for more details.
c 
c     You should have received a copy of the GNU General Public License
c     along with StateMod.  If not, see <https://www.gnu.org/licenses/>.
c_________________________________________________________________NoticeEnd___

        subroutine SetQdiv(nlog, nCarry, nRiver,
     1    nd2, nr2, iscd, idcdX, idcdC,
     1    divactX, TranLoss, EffmaxT1, OprEffT, fac, 
     1    rloss, maxsta, maxdiv, maxqdiv, qdiv, icx, 
     1    internL, corid1)
c     
c
c _________________________________________________________
c	Program Description
c
c		It sets Qdiv for destinations from various operating rules
c      and the source only if it is equal to the destination
c      (e.g.  only when iscd .eq. idcdX) See step 3
c		IT DOES NOTHING FOR CARRIERS
c		Note source is only set when the destination 
c		  is a carrier.
c		Note idcdX is the destination location 
c		  e.g. (a diversion, reservoir, instream flow,
c		  or plan. But not a carrier)
c
c
c__________________________________________________________
c	Documentation
c
c		nlog     log file
c
c	  nCarry   0 No carrier
c			       1 No return to River, Final Destination from a carrier
c	        	 2 Return to River, Final Destination from a carrier
c		 	       3 Return to River, Final Destination from the river
c
c		nd2      +n Destination is a diversion
c            0  Destination is not a diversion
c		         
c		nr2      +n Destination is a reservoir)
c            0 Destination is not a reservoir
c            
c		iscd     Source location
c	  idcdX =  Destination Diversion, Reservoir,
c			       or Plan but not a CARRIER
c   idcdC =  stream ID of the first carrier
c		idcdC    0 if no carrier
c
c		divactX  amount bypassed (cfs)
c		divact1  amount bypassed that is transit lost (cfs)
c		divactT  amount bypassed that is delivered (less
c 		        transit and carrier lost (cfs))
c		TranLoss Transit loss (fraction)
c
c	  internL  Last Intervening Structure Type
c		 	       1 = Carrier
c			       2 = River
c
c		effMaxT1 Efficiency of first carrier (fraction)
c		OprEffT  
c		rloss    Canal Loss
c
c		qdiv(5	 From River by Priority
c		qdiv(18  Carrier passing through a structure
c   qdiv(19  From Carrier by Priority (e.g. divcar)
c            
c   qdiv(20  From Carrier by Other (Storage, Exchange or Changed)
c		qdiv(32  From Carrier Loss
c
c		qdiv(26  From River by Exc_Pln (Exc_Pln)
c		qdiv(30  From River by a direct diversion or exchange
c            to a T&C or well Aug Plan. Note non consumptive
c
c		qdiv(31 From River by Sto/Exc/Plan by type 27 or 28
c		qdiv(33 From River Loss
c   qdiv(38 Carried water reported as Carried, Exchange 
c            or Bypassed but not used to calculate
c            River Divert in Outmon.f   

c
c
c__________________________________________________________
c	Dimensions
c
        dimension qdiv(maxqdiv, maxsta), rloss(maxdiv)
        character corid1*12
c        
c__________________________________________________________
c		Step 1; Initilze
c
c		iout=0 No details
c		iout=1 details
c		iout=2 summary
c
        iout=0
cx      if(corid1(1:10) .eq.'5036680.75') iout=1
        if(iout.eq.1) write(nlog,*) ' SetQdiv; ', qdiv(31,idcdX)*fac
        
        small=0.001
        
        if(iout.eq.1) then
          write(nlog,250)
          write(nlog,*) 
     1     ' SetQdiv;       icx  ncarry    iscd   idcdX   idcdC'
          write(nlog,'(12x,20i8)') icx, ncarry, iscd, idcdX, idcdC
          call flush(nlog)          
        endif  
        
        if(divactX.lt.small) goto 100
c        
c rrb 2010/10/15; Correction for a water balance
c                 report system (tranloss) at the destination;
c                 not befor water is carried.     
cx      OprLos1=divactX*TranLoss        
        OprLos1=0.0
        OprLos2=(divactX-Oprlos1)*(1.0-EffmaxT1)
        OprLos3=OprLos1+OprLos2
        
        DivactT=divactX*oprEffT
        divact1=divactX-OprLos1
c        
        icase=0            
c
c__________________________________________________________
c   Step 2: Initize Source Data based on type of opr rule
c 
c rrb 2008/06/29; Set Source based on type of opr rule 
c		              Moved qdiv(5 ) from calling program
c                 
c		              qdiv(26 From River by Exc_Pln (Exc_Pln)
c		              qdiv(30 From River direct by a Res or Reuse Plan 
c                          to a T&C Plan. Note non consumptivec
c		              qdiv(31 From River by Sto/Exc/Plan by type 27 or 28
c                 
c		              Note For type 24 or 25  Nsou=26
c                          type 26        Nsou=20
c		                       type 27 or 28  Nsou=31
c                          type 40        Nsou=31
c                          type 45        Nsou=5
c                          type 49        Nsou=29
c                 
c ---------------------------------------------------------
        nSou=0
        if(icx.eq.24) nSou=26
        if(icx.eq.25) nSou=26
c
c rrb 2014-11-24; Set control for type 26 Changed WR 
c rrb 2014-01-16; Note type 26 (DirectWR) no longer calls
c                 this subrouting
        if(icx.eq.26) nSou=20
c
        if(icx.eq.27) nSou=31
        if(icx.eq.28) nSou=31
        if(icx.eq.40) nSou=31
        if(icx.eq.45) nSou=26
c
c rrb 2010/09/15 Revise to qdiv(30)
cr        if(icx.eq.49) nSou=26
        if(icx.eq.49) nSou=30
c
c rrb 2014-11-24
        if(iout.eq.1) then
          write(nlog,250)
          write(nlog,*) 
     1     ' SetQdiv;       icx  ncarry    iscd   idcdX   idcdC    nSou'
          write(nlog,'(12x,20i8)') icx, ncarry, iscd, idcdX,idcdC, nSou
          call flush(nlog)          
        endif  
                
        if(nsou.eq.0) goto 400
c
c _________________________________________________________
c   Step 3; Set Source (iscd) data only if iscd .eq. idcdC
c
c		    qdiv(5  From River by Priority
c       qdiv(26 From River by Exc_Pln (Exc_Pln) type other
c	      qdiv(31 From River by Sto/Exc/Plan by type 27 or 28
c		    qdiv(33 From River Loss
c
c		    qdiv(18 Carrier passing through a structure
c       qdiv(19 From Carrier by Priority (e.g. divcar, divcarL)
c       qdiv(20 From Carrier by Storage or Exchange
c
c
c__________________________________________________________
c
c   Step 4; Set Source data qdiv(__,iscd) when same as the destination
c
c rrb 2011/02/25; Upgrade to not adjust diversions to a reservoir
c                 in the *.xdd reporting       
c                 isccd = source node
c                 idcdC = carrier node (0 if no carrier)
cx      if(iscd.eq.idcdC) then
cx      if(iscd.eq.idcdC .and. nr2.eq.0) then
        if(iscd.eq.idcdC .and. nd2.ne.0) then        
          qdiv(nsou,iscd) = qdiv(nsou,iscd)+divactX 
          rloss(iscd)     = rloss(iscd) + OprLos1    
          qdiv(33,iscd)   = qdiv(33,iscd)+OprLos3                 
          qdiv(18,iscd)   = qdiv(18,iscd)+divactX - OprLos3
        endif
c
c
c__________________________________________________________
c
c   Step 5; Set Destination data qdiv(__,idcdX)
c        
c ---------------------------------------------------------
c
c		Case 1; Source is the Carrier, set Destination Data (idcdX)
c	            qdiv(31 From River by Sto/Exc/Plan by type 27 or 28
c             qdiv(20 From Carrier by Storage or Exchange
c		        Set destination. Note, ncarry:
c			        = 0 No carrier
c			          1 No return to River, Final Destination from a carrier
c	        	    2 Return to River, Final Destination from a carrier
c		 	          3 Return to River, Final Destination from the river
c
        if(iscd .eq. idcdC) then   
          icase=1     
          if(nCarry.le.2) then
            qdiv(20,idcdX) = qdiv(20,idcdX)+divactT
          endif
          
          if(nCarry.eq.3) then
            qdiv(31,idcdX) = qdiv(31,idcdX)+divactT
            if(iout.eq.1) write(nlog,*) ' Outdiv_1 ', qdiv(31,idcdX)*fac
          endif
          goto 100
        endif
c        
c ---------------------------------------------------------
c
c		Case 2; No Carrier Set Destination (idcdX)
c
        if(iscd.ne.idcdC .and. nCarry.eq.0) then
          icase=2
c
c rrb 2011/02/25; Do not add to destination if not a diversion
          if(nD2.ne.0) then
            qdiv(nsou,idcdX) = qdiv(nSou,idcdX)+divactX 
          endif
          goto 100
        endif  
c        
c ---------------------------------------------------------
c
c		Case 3; Yes Carrier Set Destination (idcdX) when
c			1. The source is not the Carrier and
c			2. A carrier exists
c
c		Set destination. Note:
c			 ncarry  0 No carrier
c			         1 No return to River, Final Destination from a carrier
c	        	   2 Return to River, Final Destination from a carrier
c		 	         3 Return to River, Final Destination from the river
c
c               qdiv(19 From Carrier by Priority (e.g. divcar)
c               qdiv(20 From Carrier by Storage or Exchange
c               qdiv(31 From River by Sto/Exc/Plan by type 27 or 28
c
	      ndest=20
        if(nCarry.le.2) then
          icase=3    
c
c rrb; Clarify the source from a type 45 as From Carrier by Priority (10)          
cx        qdiv(20,idcdX) = qdiv(20,idcdX)+divactT
          qdiv(ndest,idcdX) = qdiv(20,idcdX)+divactT
          goto 100
        endif
          
        if(nCarry.eq.3) then
          icase=4
          qdiv(31,idcdX) = qdiv(31,idcdX)+divactT
          if(iout.eq.1) write(nlog,*) ' Outdiv_2 ', qdiv(31,idcdX)*fac          
          goto 100
        endif
c
c__________________________________________________________
c
c		Step 5; Detailed Output        
 100    if(iout.ge.1)  then        
          write(nlog,200)
          write(nlog,210) icx, corid1,
     1     icase, nCarry, iscd, idcdX, idcdC, nRiver, 
     1     divactX*fac, TranLoss*100, EffmaxT1*100., divact1*fac,
     1     oprEffT*100, divactT*fac, qdiv(30,idcdX)*fac, 
     1     qdiv(31,idcdX)*fac
          call flush(nlog)
        endif
c
c__________________________________________________________
c
c		Step 6; Return             
        return
c
c _________________________________________________________
c		Formats        
 200    format(/,
     1   '  SetQdiv;  icx CorID       ',
     1   '   icase  nCarry    iscd   idcdX   idcdC  nRiver',
     1   '   divactX  TranLoss  EffmaxT1   divact1',
     1   '   OprEffT   divactT   Qdiv(30   Qdiv(31',/
     1   ' _________ ____ ____________', 6(' _______'), 7(' _________'))
 210    format(    
     1   '  SetQdiv;', i5, 1x, a12, 6i8, 20f10.0)
 250   format(/,72('_'))       
c
c _________________________________________________________
c
c		Error tracking
 400  write(nlog, 410) nsou
 410  format(
     1 '  SetQdiv; Problem type of source not defined, nsou =',i5)
      goto 900
     
 900  write(6,910)
      write(nlog,910) 
      
 910  format('    Stopped in SetQdiv',/,
     1       '    See the *.log file')
 920  format('    Stopped in SetQdiv')
      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)
      
      stop
      end  
