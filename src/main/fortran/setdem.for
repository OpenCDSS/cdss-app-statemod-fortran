c
c
       SUBROUTINE SetDem(isub, nd, nw, nd2, iuse, fac, 
     1  effa, effS1, effF1, AreaT, divreqX, divSprX, divOthX,
     1  diwrGS1, diwrGF1, ncallX)
c
c _________________________________________________________
c     SetDem; It Sets demand data for a diversion
c     or well.  Note hte well may operate independently (nd2=0)
c     or as part of a diversion system (nd2>0) 
c
      include 'common.inc'
      character cstaid1*12
c

c
c _________________________________________________________
c	Program Description
c
c     SetDem; it sets well demand depending on the type of
c     demand specifieid (idemtyp) and if the well is tied
c     to a diversion (nd2>0) or not (nd2=0)
c _________________________________________________________
c
c		  Step 1; Initilize
c

      iout=0
      small=0.001
c _________________________________________________________
c
c		  Step 2; Set demand for a diversion
c               
      if(nd.gt.0) then
        cstaid1=cdivid(nd)
        if(idemtyp.le.3) then
          divreqx=divreq(iuse)
        else
          divreqx=divsw(iuse)
        endif
        goto 500
      endif
c _________________________________________________________
c		  Step 3; Set Demand for a well
c         
      cstaid1=cdividw(nw)      
      effa=diveffw(mon,nw)/100.0
      effS1=effmaxs(nw)/100.0
      effF1=effmaxw(nw)/100.0
c
c rrb 00/06/16; 
c               b. Get ratio of water use efficiency
      if(nd2.gt.0) then
        effd = diveff(mon,nd2)/100.0
        if(ieffmax.le.0) ceff = effd/effa
        if(ieffmax.eq.1) ceff = effd/efff1
      else
        effd = 0.0
        ceff = 1.0
      endif
c
c               c. Demand type
      if(idemtyp.ge.3) then
        ceff=1.0
      endif
c _________________________________________________________
c
c               Step 7; Set demand
c	
c ---------------------------------------------------------
c		Demand type 1(Historic) D&W or Well Only 
c		Note cannot use IWR in case SW was shorted or
c		Acres = 0
c
c rrb 2009/06/22; Correctin
cx    if(nd2.eq.0) AreaT=AreaGfw(nw) + AreaGsw(nw)
      if(nd2.eq.0) AreaT=AreaGfw(nw) + AreaGsw(nw)
      if(nd2.ne.0) AreaT=AreaGf(nd2) + AreaGs(nd2)
c      
c ---------------------------------------------------------
c		Demand type 1 (Historic)
c		Well Only Structures      

      if(idemtyp.eq.1 .and. nd2.eq.0) then
        if(AreaT.gt.small) then
          divreqX=divreqw(nw)
          divsprX=divreqX*AreaGSw(nw)
          divothX=divreqX*AreaGFw(nw)           
        else
          divreqX=divreqw(nw)
          divsprX=0.0
          divothX=divreqX
        endif  
        diwrGS1=divsprX*EffS1
        diwrGF1=divothX*EffF1                
      endif
c      
c ---------------------------------------------------------
c		Demand type 1 (Historic)
c		D&W Structures      
      if(idemtyp.eq.1 .and. nd2.gt.0) then
        if(AreaT.gt.small) then
          divreqX=divreqw(nw)
          divsprX=divreqX*AreaGS(nd2)
          divothX=divreqX*AreaGF(nd2)
        else
          divreqX=divreqw(nw)
          divsprX=0.0
          divothX=divreqX
        endif  
        diwrGS1=divsprX*EffS1
        diwrGF1=divothX*EffF1                
      endif
c
c ---------------------------------------------------------
c		Demand type > 2 (non historic)
c		Well Only Structures      
      if(idemtyp.ge.2 .and. nd2.eq.0) then  
        if(AreaT.gt.small) then
          divreqX=divreqw(nw)
          divsprX=divreqX*AreaGSw(nw)
          divothX=divreqX*AreaGFw(nw)
        else
          divreqX=divreqw(nw)
          divsprX=0.0
          divothX=divreqX
        endif
        diwrGS1=divsprX*EffS1
        diwrGF1=divothX*EffF1                
      endif  
c
c ---------------------------------------------------------
c		Demand type > 2 (non historic)
c		D&W Structures
      if(idemtyp.ge.2 .and. nd2.ge.1) then  
        if(AreaT.gt.small) then
          divreqX=divreq(iuse)
          divsprX=divreqX*AreaGS(nd2)
          divothX=divreqX*AreaGF(nd2)          
        else  
          divreqX=divreq(nd2)
          divsprX=0.0
          divothX=divreqX
          diwrGS1=0.0
          diwrGF1=divothX*EffF1            
        endif
        diwrGS1=divsprX*EffS1
        diwrGF1=divothX*EffF1                
      endif  
      
      divreqX1=divreqX
c _________________________________________________________
c	    Step X; Detailed output
c      
 500  if(iout.eq.1) then
        write(nlog,*) ' SetDem; ', iyrmo(mon),xmonam(mon),
     1   cstaid1, idemtyp, nw, nd, nd2, iuse, AreaT, 
     1   divreqX*fac, divsprX*fac, divothX*fac,
     1   EffS1*100., effF1*100.
      endif
c _________________________________________________________
c     Return
c      
      return
      end
