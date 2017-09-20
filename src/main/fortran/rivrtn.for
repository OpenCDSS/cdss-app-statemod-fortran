c      
c
 	Subroutine RivRtn(
     1    icx, nriver, l2, ndtype, iscd, nd, iuse, idcd, idcdX, 
     1    fac, smallN, oprEffT, relact, adj, divact, divactL, 
     1    ncnum, nAvail, alocfsR, DepfacM, imcdR, corid1)
c     
c		   It calculates return to river.
c		   It is used to similate an augmentation station 
c		   (e.g. a structure that diverts then immediately 
c		   returns water to river for diversion at another
c		   structure (destination)) or another carrier
c		   Note it operates on AVTEMP; not avail because
c		   the ultimate destination at idcd may be located upstream
c		   or downstream of the return to the river.
c      
c		   Genreal approach:
c		   1. Adjust AVTEMP for every release, diversion and return
c		      (Steps 1-5)
c		   2. Search for minimum at every diversion from the river
c		      (e.g. divert to a carrier or the destination) (Step 6)
c		   3. If the minimum is negative; reduce the diversion
c		      and release accordingly (Step 7)
c		   4. if(navail.eq.1) Adjust AVAIL for each diversion 
c         and release (Step 8)
c      5. if(navail.eq.2) Adjust AVAIL for each diversion 
c         and release except the source diversion (used by DivcarL)
c      
c
c
c _________________________________________________________
c
c               Update History
c
c rrb 2008/10/23; Revised to readjust when a shortage
c		  occurrs, even if adj > 0
c
c rrb 2008/10/15; Revised to not adjust the adjustment by
c		  the efficiency of water use, already
c		  included in the calculation of Avail.
c
c rrb 2008/09/12; Revised calculation of adjustmnet factor and
c		  to allow reoperation with an adjusted diversion
c		  up to 3 times 
c
c rrb 2008/09/08; Corrected to catch a bad setup that can result in 
c		  no water available to the destination (idcdX)
c
c rrb 2008/06/10; Allow return to river then diversion
c		  from a carrier again (internT = 1 Carrier, 2=Return)        
c
c _________________________________________________________
c
c               Documentation
c	
c	  icx		    this routines ID
c	  nriver		return to river indicator (0=no, +=river id)
c	  l2		    Operational right pointer
c	  ndtype  	destination type (3=diversion, 2=reservoir, 7=plan)
c	  iscd	    source location
c                 	
c	  nd		    destination ID
c	  iuse	    destination user
c	  idcd	    first destination location (may be a carrier)
c	  idcdX	    final destination location (not a carrier)
c          
c   DepfacM   Monthly efficiency
c             =1.0 for a diversion release
c             =diveff(nd,mon) for a depletion release
c	  fac		    factor cfs to acft
c	
c	  smallN		small negative number
c	  effX		  weighted efficiency based on land use
c	  oprefft 	
c	  relact		release
c	  adj		    adjustment to diversion
c	  divact  	Final diversion
c	  divactL		Final diversion less loss
c	  internT   Intervening structure type
c			        1 = Carrier
c			        2 = River
c   ncnum     # of intervening structures (Carriers & Returns)
c
c	  nAvail		Avail Adjustment control (allows this routine 
c	            to be used to identify a carrier limit w/o 
c	            adjusting avail (e.g. see DivCarL). 
c	            0 Do not adjust the array Avail
c	            1 Do adjust the array Avail             
c   nlast     indicator where the last carrier discharges water
c             0 = river
c             1 = carrier
c
c	  imcdR		  River station pointer of min flow
c	  alocfsR   Min flow at river station imcdR
c	  cImcd     River Station ID of min flow
c   
c	  corid1  	Calling operating right
c
c _____________________________________________________________
c
      include 'common.inc'
      dimension adjOut(500)
      character 
     1  cwhy*48, cdestyp*12, ccarry*8, cpuse*3, csour*12,
     1  rec12*12, cTandC*3, cresid1*12, criver*12, 
     1  corid1*12, cimcd*12

c _________________________________________________________      
c
c	              Step 1; Initilize
c		            iout = 1 details
c		            iout = 2 summary
c		            iout = 3 summary & details on last diversion
c		            iout = 4 summary & details on adjustments to avtemp
c		            iout = 5 summary & details on adjustments to AVAIL
c
c
      isub=-1
c     iout=4
cx    iout=5
      iout=0
      
cx     if(corid1(1:9) .eq. 'RkyMtn.07')   iout=4

      if(iout.ge.1) write(nlog,210) 'RivRtn      ',corid1,iout,navail,
     1 relact*fac, divact*fac    
 210  format(//, 60('_'),/, 2(2x, a12), /
     1 '    iout   = ', i5, /
     1 '    navail = ', i5, ' where:',/
     1 '                 0 Do not adjust the array Avail',/
     1 '	               1 Do adjust the array Avail',/
     1 '    Relact = ', f8.0,/
     1 '    Divact = ', f8.0)
c     
      iterX=0
      iterMax=500
      
      if(iterMax.gt.500) then
        write(nlog,*) ' RivRtn; Warning itermax dimension exceeded = ',
     1    itermax
        goto 999
      endif
      
      do i=1,iterMax
        adjOut(i)=0.0
      enddo  
      
      small=0.001
      smalln=-1.0*small
      
      divact0=-1./fac
      divactL0=-1./fac
      
      divact1=divact
      divactL1=divactL
      relact1=relact
      alocfsR=-1.0/fac
      ret1=0.0/fac
      adjChk=0.0
      imcdR=0
      cImcd='NA'
      effX=1.0
c
c rrb 2011/10/15
      adjpct=0.8
      adjpct2=0.6
      
      
 100  iterX=iterX+1
      adj=0.0
      adj0=-1.0/fac
      adj1=-1.0/fac
      adj2=-1.0/fac
      adj3=-1.0/fac
      adj4=-1.0/fac
      adj5=-1.0/fac
      adj6=-1.0/fac
      
      nchkA=0
      ieff2=1
      effX=1.0
      short=0.0
c
c rrb 2014-07-27; Go straight to adjust avail if called the 
c                 second time by DivCarL (may work for all
c                 calls but no time to verify)
      if(navail.eq.2) goto 400      
c
c _________________________________________________________      
c
c               Step 2; Set AVTEMP = Avail for Return to river
      do is=1,numsta
        AVTEMP(is)=avail(is)
        dumx(is)=0.0
      end do        
c

      if(iout.eq.1) then
        nchkA=1
        call chkAvail(nlog, icx, nchkA, maxsta, 
     1    AVTEMP, 0.0, idcdX, nRiver, idcd, fac)
      endif
c     
c _________________________________________________________      
c
c               Step 3; Adjust AvTem for Plan or Reservoir 
c                       release (RELACT) 
c                  
      availR=AVTEMP(Iscd)
      ndns=ndnnod(iscd)  
      call takou2(isub, maxsta,AVTEMP,idncod,relact,ndns,iscd)  
      AVTEMP(iscd)=availR     
c
      if(iout.eq.1) then
        nchkA=2 
        call chkAvail(nlog, icx, nchkA, maxsta,       
     1    AVTEMP, relact, idcdX, nRiver, idcd, fac)    
      endif
      
c
c _________________________________________________________      
c
c rrb 2008/06/10; 
c               Step 4; For each diversion from the river
c		                    and return to the river adjust AVTEMP
c		                    Note nlast = indicator water has returned 
c                       to the river
c
      divact2=divact
      nlast=0
      
      do i=1,ncnum
        ncar=intern(l2,i)
 
c
c ---------------------------------------------------------
c		            Step 4a; Adjust AVTEMP for each diversion to a carrier
c               (InternT()=1
      
        if(internT(l2,i).eq.1) then
          if(nlast.eq.0) then
            idcd1=idvsta(ncar)
            ndnd1=NDNNOD(idcd1)
            call takou2(isub, maxsta,AVTEMP,idncod,divact2,ndnd1,idcd1)
            nlast=1   
c            
            if(iout.eq.1) then
              idcd0=amax0(idcd1-1,1)
              idcd2=idcd1+1
c
              nchkA=3 
              call chkAvail(nlog, icx, nchkA, maxsta, 
     1          AVTEMP, divact2, idcd0, idcd1, idcd, fac)
            endif
                     
          endif  
             
          Eff1=(100.0 - OprLossC(l2,i))/100.0
          divact2=divact2 * Eff1
        endif  

c
c ---------------------------------------------------------
c		            Step 4b; Adjust AVTEMP for water returned to the river 
c                        (InternT()=2)
        if(internT(l2,i).eq.2) then             
          RelRiv=-1.0*divact2     
          idcd1=ncar
          ndnd1=ndnnod(ncar)        
          nlast=0
          call takou2(isub, maxsta,AVTEMP,idncod,RelRiv,ndnd1,idcd1)
c
c ---------------------------------------------------------
c		            Step 4c; Detalied output at 3 locations          
          if(iout.eq.1) then
            idcd0=amax0(idcd1-1,1)
            idcd2=idcd1+1
c
            nchkA=4 
            call chkAvail(nlog, icx, nchkA, maxsta, 
     1        AVTEMP, RelRiv, idcd0, idcd1, idcd, fac)
          endif
        endif
c
c		End of carrier loop (i=1,ncnum)        
      end do
      
c
c _________________________________________________________      
c
c		            Step 5; Adjust AVTEMP for return flows from final 
c                       destination if it is a diversion 
c                       (ndtype=3)
c
      if(ndtype.eq.3) then		        
c
c		Initilize Dumx to Avatemp before return adjustments      
c		so they can be removed if the diversion gets adjusted
        do is=1,numsta
          dumx(is)=avtemp(is)
        end do
c
c rrb 2011/07/14; Correction to allow the edit for version 12.3017 
c                 to work properly       
cx      CALL RtnSecX(icx,divactL,L2,iuse,idcdX,nd,ieff2, 
        ndndX=NDNNOD(idcdX)
        CALL RtnSecX(icx,divactL,L2,iuse,idcdX,ndndX,nd,ieff2,
     1    retTot, ret1, corid1)
     
        if(ret1.gt.small) then
          effX=1.0-ret1/divactL
          effX=amin1(1.0, effX)
        else
          effX=1.0
        endif

c
c		Store currrent returns in dumx 
c		so they can be removed if the diversion gets adjusted
        do is=1,numsta
          dumx(is)=avtemp(is)- dumx(is)
        end do        
c     
c        
        if(iout.eq.1) then 
          nchkA=5        
          call chkAvail(nlog, icx, nchkA, maxsta, 
     1        AVTEMP, ret1, idcd0, idcdX, idcd, fac)
        endif     
      endif
c
c _________________________________________________________      
c
c rrb 2008/10/21; 
c		            Step 6; Find minimum flow downstream of the demand
      ndndX=NDNNOD(idcdX)  
          
      if(iout.eq.1) write(nlog,*) '  RivRtn_1; ndndX, idcdX',
     1   ndndX, idcdX 
      CALL DNMFSO(maxsta, AVTEMP, IDNCOD, idcdX, ndndX, IMCD)
      adj=avtemp(imcd)      
      adj0=adj
c      
c
c _________________________________________________________      
c
c		            Step 7; Find minimum flow in AVTEMP downstream of
c		                    all river diversions (internT = 1) 
c
        
      imcd=0
      do i=1,ncnum
        if(internT(l2,i).eq.1) then
          ncar=intern(l2,i)
          idcd1=idvsta(ncar)
          ndnd1=NDNNOD(idcd1) 
          
          CALL DNMFSO(maxsta, AVTEMP, IDNCOD, idcd1, ndnd1, IMCD)
c
          if(AVTEMP(imcd).lt. Adj) then
            Adj=AVTEMP(imcd)
            Adj1=adj
          endif
c
          if(iout.eq.1) write(nlog,*) 
     1    '  RivRtn_2; ndnd1, idcd1, ncar i imcd, AVTEMP(imcd), Adj',
     1      ndndX, idcdX, ncar, i, imcd, AVTEMP(imcd)*fac, Adj*fac  
          
        endif
      end do  
c
c _________________________________________________________      
c
c		            Step 8a; If the system is negative; reduce the
c		                     diverson and release accordingly
c 
c     adjChk=adj-smallN
      adjChk=adj
      if(adj.lt.smallN) then
        adj2=adj
c
c ---------------------------------------------------------
c 		Calculate adjustment.
c		Note effX is the actual efficiency with carrier loss

        if(effX.gt.small) then
          adj3=adj/(1.0-(1.0-effX))
c
c rrb 2008/12/17; Limit the adjustment to 80% 
c		    Necessary to converge; especailly when the calling
c		    right amy change between iterations
c
c rrb 2011/10/15;
cx        adj3=amax1(adj3, -1.0*divactL*0.8)   
          adj3=amax1(adj3, -1.0*divactL*adjpct)               
          adj=adj3
        endif
c
c ---------------------------------------------------------
c rrb 2008/10/20; Rejust if adjustment is too large
        if(divactL+adj3.lt.small) then
          adj=adj2
        endif  
        
        adjOut(iterX)=adj
        
        divact0=divact
        divactL0=divactL
c
c ---------------------------------------------------------
c
c		Re Adjust based on the diversion divactL &
c		number of iterations
c        
        divactL=amax1(0.0, divactL0+adj)                  
        
        if(divactL.gt.small .and. iterX.le.5) then
          divactL=divactL*(1.0+(1.0-effX))
        else
cx        divactL=amax1(0.0, divactL0+ adj)
cx        divactL=amax1(0.0, divactL0+ adj*(1.0+EffX))
          divactL=amax1(0.0, divactL0+ adj*(1.2))
          if(divactL.lt.small) divactL=0.0
        endif        
        
        divact=divactL/OprEffT
c        
c rrb 2010/10/15; Account for a depletion release                    
cx        relact=-1.*divact        
        relact=-1.*(divact*DepfacM)
        short=divact1-divact      
        alocfsR=divact
        
        if(imcd.gt.0) then
          imcdR=imcd
          cImcd=cstaid(imcdR)
        endif
        
        if(iout.eq.4) then
          if(iterX.eq.1) write(nlog,220) 
          write(nlog,221)
     1      iterX, 1, imcdR, cImcd, 
     1      adjChk, divact1*fac, divactL1*fac,Ret1*fac, effX*100.,  
     1      adj0*fac, adj1*fac, adj2*fac, adj3*fac, adj4*fac, 
     1      adj5*fac, adj6*fac, adj*fac,
     1      DivactL0*fac, DivactL*fac, Divact0*fac, Divact*fac, 
     1      relact*fac, short*fac        
        endif        
        
      endif
c
c _________________________________________________________      
c
c rrb 2008/10/23; 
c		            Step 8b; If Avtemp (adj is positive 
c		            	and short increase the diversion & release
c		            	If positive but not short, exit
c
      if(adj.gt.smallN) then
        adjX=adj
        short=divact1-divact
        
        if(short.gt.small .and. adj.gt.small) then
          divact0=divact
          divactL0=divactL
c          
c rrb 2008/12/17; Limit the adjustment to 80% 
c		    Necessary to converge; especailly when the calling
c		    right may change between iterations
c
c rrb 2011/10/15;
cx        adj=adj*0.8
          adj=adj*adjPct
          adj4=adj
          divactL=amin1(divactL1, divactL+adj)
        
          adjOut(iterX)=adj
          divact=divactL/OprEffT
c          
c rrb 2010/10/15; Account for a depletion release               
cx        relact=-1.*divact 
          relact=-1.*(divact*DepfacM)         
        endif  
        
        alocfsR=divact
      
        if(imcd.gt.0) then
          imcdR=imcd
          cImcd=cstaid(imcdR)
        endif
        
        if(iout.eq.4) then
          if(iterX.eq.1) write(nlog,220) 
          write(nlog,221)
     1      iterX, 2, imcdR, cImcd, 
     1      adjChk, divact1*fac, divactL1*fac,Ret1*fac, effX*100.,  
     1      adj0*fac, adj1*fac, adj2*fac, adj3*fac, adj4*fac, 
     1      adj5*fac, adj6*fac, adj*fac,
     1      DivactL0*fac, DivactL*fac, Divact0*fac, Divact*fac, 
     1      relact*fac, short*fac        
        endif        
c
c _________________________________________________________
c
c	            Step 9; Check for an exit 
c rrb 2008/12/08; Compare to adjX, whici is adj before the factor (adjpct)
        if(adjX.le.small .or. short.le.small) goto 300
c
c rrb; 2009/05/12; Exit after 10 iterations if there is just a 
c                  small change (cx)
        cx = abs(adjout(iterX) - adjout(iterX-1))
        if(cx.lt.small .and. iterX.gt.10) goto 300      
        
      endif  
c
c
c _________________________________________________________      
c
c		            Step 10;	Return to 100 to recalculate diversion
c		                      if the maximum # of iterations is not 	
c		                      exceeded and the diversion > zero
      
      if(iterX.le.iterMax) then
c
c rrb 2011/10/15; Update
        if(iterX.gt.100) then
          adjPct=adjPct2
        endif
        goto 100
      else  
        goto 900     
      endif          
      
c
c _________________________________________________________      
c rrb 2008/09/08; 
c		            Step 11; Correction to catch a bad setup that 
c                        can result in no water available to
c		                      the destination (idcdX)
c rrb 2008/10/08; Correction, add if(nlast ... since the only reason
c		  to check the final destination (idcdX) is if the
c		  final diversion is from the river
 300  if(nlast.eq.0) then   
        cx=divact
        
        divact0=divact
        divact=amin1(divact, avtemp(idcdX))
        divactL=divact*OprEffT
c        
c rrb 2010/10/15; Account for a depletion release               
cx      relact=-1.* divact    
        relact=-1.* (divact*DepfacM)  
c
c rrb 2008/12/17; revise adj... for reporting        
cx      adj2=divact-cx
        adj=divact-cx
        adj5=adj
c       
        if(adj.lt.small) then      
          if(effX.gt.small) then
            adj=adj/(1.0-(1.0-effX))           
            adj6=adj
c           write(nlog,*) ' RivRtn; Adj data 2 ',adj5*fac, effX, 
c    1        adj6*fac, adj*fac
          endif
           
          adjOut(iterX)=adj
           
          if(iout.eq.4) then
            if(iterX.eq.1) write(nlog,220) 
            write(nlog,221)
     1        iterX, 3, idcdX, cstaid(idcdX), 
     1        adjChk, divact1*fac, divactL1*fac, Ret1*fac,effX*100., 
     1        adj0*fac, adj1*fac, adj2*fac, adj3*fac, adj4*fac, 
     1        adj5*fac, adj6*fac, adj*fac, 
     1        DivactL0*fac, DivactL*fac,  Divact0*fac, Divact*fac,
     1        relact*fac, short*fac   
          endif
c
c rrb 2008/11/24; Exit if adjustment is zero and shortage is zero
          if(adj.lt.small .and. short.lt.small) goto 400		        
        
c       
c	  	Return to 100 to recalculate diversion
c	  	if the maximum # of iterations is not exceeded		
          if(iterX.le.iterMax) then
            goto 100
          else
            goto 900
          endif          
        endif  
      endif


c
c _________________________________________________________      
c		
c               Step 12; Exit if nAvail = 0 (do not adjust AVAIL)      
 400  if(nAvail.eq.0) goto 250
c
c _________________________________________________________      
c
c		Step 13; Finally adjust AVAIL for each diversion and release      
      if(iout.eq.1 .or. iout.eq.5) then
        write(nlog,*) ' '
        write(nlog,*) ' Begin to adjust Avail, nAvail, divact = ',
     1    nAvail, nlast, divact*fac
      endif  
      
      divact2=divact
      nlast=0
      do i=1,ncnum
        ncar=intern(l2,i)
        
c
c ---------------------------------------------------------
c		            Step 14; Adjust AVAIL for each diversion
c		            from the river to a Carrier (e.g. do not adjust
c		            if the final destination is from the river
c		            that adjustment occurrs in step in the calling routine
      
        if(internT(l2,i).eq.1) then
          if(nlast.eq.0) then
            idcd1=idvsta(ncar)
            ndnd1=NDNNOD(idcd1)
c
c rrb 2014-07-28; Clean up debug printout
            idcdA=amax0(idcd1-1,1)
            idcdB=idcd1
            idcdC=idcd1+1

c rrb 2014-07-12 TEST by skipping the source carrier since it
c                was set in DivCarL
cx              call takout(maxsta,avail,river,avinp,qtribu,idncod,
cx     1          divact2, ndnd1,idcd1)

            if(navail.eq.1) then
              call takout(maxsta,avail,river,avinp,qtribu,idncod,
     1          divact2, ndnd1,idcd1)
            endif
            
            if(navail.eq.2 .and. i.ge.2) then
              call takout(maxsta,avail,river,avinp,qtribu,idncod,
     1          divact2, ndnd1,idcd1)
            endif
             
            if(iout.eq.1 .or. iout.eq.5) then
              nchkA=50             
              call chkAvail(nlog, icx, nchkA, maxsta, 
     1        AVAIL, divact2, idcdA, idcdB, idcdC, fac)
            endif            
            
            nlast=1            
          endif
            
          
          Eff1=(100.0 - OprLossC(l2,i))/100.0
          divact2=divact2 * Eff1

        endif  

c
c ---------------------------------------------------------
c		            Step 15; Adjust AVAIL for water returned to the river 
        if(internT(l2,i).eq.2) then        
          RelRiv=-1.0*divact2
          idcd1=ncar
          ndnd1=ndnnod(ncar)        
          nlast=0
c
c rrb 2014-07-28; Clean up debug printout
            idcdA=amax0(idcd1-1,1)
            idcdB=idcd1
            idcdC=idcd1+1
          
c
          call takout(maxsta,avail,river,avinp,qtribu,idncod,
     1      RelRiv, ndnd1,idcd1)
          
          if(iout.eq.1 .or. iout.eq.5) then
            nchkA=51
            call chkAvail(nlog, icx, nchkA, maxsta, 
     1        AVAIL, RelRiv, idcdA, idcdB, idcdC, fac)
          endif         
        endif
c
c        
        if(iout.eq.3) then
          if(i.eq.1) write(nlog,290) 
          write(nlog,292)
     1      i, l2, ncar, nlast, internT(l2,i), divact2*fac
        endif
      end do
c
c _________________________________________________________      
c
c		            Step 16;	If nlast=0 (the last carrier was to a 
c		                      return to river) adjust Avail for the 
c		            	        final diversion from the stream       
   
      if(nlast.eq.0) then   
c
c rrb 2008/07/08; Correction      
cx      ndnd1=ndnnod(ncar) 
        ndnd1=ndnnod(idcdX) 
        if(iout.eq.5) then       
         write(nlog,*) ' RivRtn_1; nlast, ncar, idcdX, ... = ', 
     1    nlast, ncar, idcdX , ndnd1, divactL*fac
        endif
     
        CALL TAKOUT(maxsta,AVAIL,RIVER,AVINP,QTRIBU,IDNCOD,
     1            divactL,ndnd1,idcdX)
     
c        write(nlog,*) ' RivRtn_2; nlast, ncar, idcdX = ', 
c     1   nlast, ncar, idcdX , ndnd1, divactL*fac
       endif        
c
c _________________________________________________________      
c
c		            Step 17; Detailed Output
c
 250  if(iout.ge.1) then
        write(nlog,270) iterX
        write(nlog, 280) '  RivRtn;   ', 
     1    iyrmo(mon),xmonam(mon), idy, iterX,
     1    icx, iscd, idcdX, nriver, ncnum, l2, idcd, imcdR, nAvail,
     1    divact1*fac, divactL1*fac, relact1*fac, 
     1    divact*fac, divactL*fac, AVTEMP(imcd)*fac,
     1    (adjOut(i)*fac, i=1,5), cImcd
        write(nlog,*) ' RivRtn max iteration = ', iterX 
      endif
c
c _________________________________________________________      
c
c		Formats
 220    format(/,  
     1  ' RivRtn; Diversion Adjstment',/
     1  '   Note cImcd is the River ID with the min flow', /
     1  '        AdjChk is the convergence check in cfs',/
     1  '   iterX   Print   imcdR cImcd           AdjChk',
     1  ' Divact1 DivactL1   Ret1    effX',
     1  '    adj0    adj1    adj2    adj3    adj4',
     1  '    adj5    adj6     adj',
     1  ' DivactLO DivactL Divact0  Divact Relact',
     1  '   Short'/
     1  ' _______ _______ _______ ____________ _________',
     1  ' _______ _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______')
     
 221    format(3i8,1x,a12, f10.4, 20f8.2)             
     
 260  format(/,
     1'  RivRtn; Problem with operating rule ',a12,/
     1'          Either the maximum iteration is exceeded or',/
     1'          the minimum available flow is less than zero',/
     1'          iterX = ', i8, ' Max iterations = ', i8, /,
     1'          Adjx  = ',f12.5,' Short = ', f12.5,' Small = ', f12.5,/
     1'          Adjx  = ',f12.5,' Short = ', f12.5,' Small = ', f12.5,/
     1'          Divact = ', f8.2, ' AvMin = ', f8.2, ' imcd = ',i5)
     
 270  format(/
     1'  RivRtn; Summary Output. Note:',/
     1'    Divact1  = Initial Carrier Diversion',/
     1'    DivactL1 = Initial Destination Diversion',/
     1'    Relact   = Plan or Reservoir Release',/
     1'    Adj      = Diversion Adjustment',/
     1'    Divact   = Final Carrier Div',/
     1'    DivactL  = Final Destination Diversion',/
     1'    IterX =    ', i5,//
     1'  RivRtn      iyr mon   day',
     1'   iterX     icx    iscd   idcdX  nriver    ncnum     l2',
     1'    idcd   imcdR  nAvail',
     1'   divact1  divactL1    relact    divact   divactL',
     1'    AVTEMP   AdjOut1   AdjOut2   AdjOut3   AdjOut4   AdjOut5'
     1' cImcd'/
     1' ___________ ____ ____ ____',
     1' _______ _______ _______ _______ _______ _______ _______',
     1' _______ _______ _______',
     1' _________ _________ _________ _________ _________ _________',
     1' _________ _________ _________ _________ _________',
     1' ___________')
 
 280  FORMAT(a12, i5,1x,a4, i5, 10i8, 11F10.2, 1x, a12)
     
 290  format(/,
     1 '  RivRtn; Adjusting Avail',/
     1 '     i    l2  ncar nlast internT divact2',/
     1 ' _____ _____ _____ _____ _______ _______')
     
 292  format(5i6, f8.0)   
c _________________________________________________________      
c
c		Return            
      return
c _________________________________________________________      
c
c		Error Processing
c		Note istop=0 do not stop
 900  istop=0
      call ChekAv2(icx, maxsta, numsta, istop, fac, AVTEMP, imcd, avMin)
      
      write(nlog,260) corid1, iterX, iterMax, 
     1  Adjx, short, small,
     1  Adjx*fac, short*fac, small*fac,
     1  Divact*fac, AvMin*fac, imcd
c
c rrb 2010/09/15; Correction     
cr    i1=amax1(iterX-10, 1) 
      i1=max0(iterX-10, 1)
      do i=i1,iterX-1
        write(nlog,*) i, adjout(i)*fac
      end do
      
      write(nlog,270) iterX
        write(nlog, 280) '  RivRtn;   ', 
     1    iyrmo(mon),xmonam(mon), idy, iterX,
     1    icx, iscd, idcdX, nriver, ncnum, l2, idcd, imcdR, nAvail,
     1    divact1*fac, divactL1*fac, relact*fac, 
     1    divact*fac, divactL*fac, AVTEMP(imcd)*fac,
     1    (adjOut(i)*fac, i=1,5), cImcd

 999  write(6,1050) 
      write(99,1051)      
      
c
c rrb Remove comment indicator to TEST what happens when the
c            system is allowed to keep operating
cx   RETURN
    
 1050 format(/, 72('_'),/
     1 '  Stopped in RivRtn',/,
     1 '  See the *.log file')
 1051 format(/, 72('_'),/
     1 '  Stopped in RivRtn')
      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)     
      stop
      end
