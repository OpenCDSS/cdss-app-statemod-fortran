c
C     Last change:  RRB   8 Jan 2002    5:38 pm
      subroutine demand(ityp,maxdivx,maxdivwx)
c
c
c _________________________________________________________
c	Program Description
c

c       Demand; It:
c               1. Calculates total demand and IWR demand
c                  based on how data is provided (Total or IWR).
c               2. Sets sprinkler demand
c
c _________________________________________________________
c
c       Update History
c rrb 01/04/13; Revised to test for zero efficiency for Closed Basin
c rrb 01/12/17; Revised to allow daily model to operate using a
c               running monthly demand.  Approach:
c                 idaydem controls 0=off, 1=on
c                 idaydem is read as iday in datinp and set
c                 Define new varaible divreqd (daily demand)
c
c                 if daily and idaydem is on and day = 1
c                   diverx = diverd(32,nd)*mthday(mon) Monthly value
c                   diverd = diverd(idy,nd) no change
c
c                 if daily and idaydem is off
c                   diverx = diverd(idy,nd) no change
c                   diverd = diverd(idy,nd) no change
c
c                 if monthly
c                   diverx = diver(mon,nd) no change
c                   diverd = diver(mon,nd) no change
c                
c
c               ityp=0 called by bomsec (Monthly)
c               ityp=1 called by dayset (Daily)
c
c Diversion Inputs
c	 diver(mon,nd)			Diversion Demand (Mdainp *.ddm)
c	 diverir(mon,nd)		Diversion IWR (Mdainp *.ddm)
c	 diwr(mon,nd)			Diversion IWR (Mdainp *.ddc)
c	 diwrX(nd)			Diversion IWR (Demand *.ddm or *.ddc)
c	
c Diversion Outputs
c	 divreq(nd) = diverx(nd) = diver(mon,nd) 	Diversion Demand
c	 diwrreq(nd)= diwrX(nd)  = Diverir(mon,nd) or diwr(mon,nd)  IWR
c	 dIwrSF(nd)			IWR Surface Water Flood
c	 dIwrSS(nd)			IWR Surface Water Sprinkler
c	 dIwrGF(nd)			IWR Ground Water Flood
c	 dIwrGS(nd)			IWR Ground Water Sprinkler

c
c Well Inputs
c	 diverW(mon,nw)			Well Demand (Mdainp *.ddm)
c	 diverirW(mon,nw)		Well IWR (Mdainp *.ddm)
c	 diwrW(mon,nw)			Well IWR (Mdainp *.ddc)
c	 diwrWX(nw)			Well IWR (Demand *.ddm or *.ddc)
c	
c Well Outputs
c	 divreqW(nw) = diverx(nw) = diver(mon,nw) 	Well Demand
c	 diwrreqW(nw)= diwrX(nw)  = Diverir(mon,nw) or diwr(mon,nd)  Well IWR
c	 dIwrGFW(nw)			IWR Ground Water Flood
c	 dIwrGSW(nw)			IWR Ground Water Sprinkler
c	
c _________________________________________________________
c
c       Dimensions
        include 'common.inc'
        dimension  divreqd(maxdivx)
        Character rec12*12
c
c _________________________________________________________

c rrb 01/12/18; Daily printout
      if(iday.eq.0) then
        fac = mthday(mon)*factor
      else
        fac = factor
      endif

      small=0.001
c
c		iout=0 No detailed output
c		     1 Detailed output      
      iout=0
      ioutX=iout
c     if(ichk.eq.5) iout=1
      if(ichk99.eq.5) iout=1
      if(mon.eq.1) ichkx=ichk      
      if(ichkx.eq.91) iout=1
      

c     write(nlog,*) '  Demand; iout ', iout
      iprintd=0

c
c _________________________________________________________
c               Step 1; Initilize Diversion Data based on
c               time step
c
c ---------------------------------------------------------
c               Step 1a; Monthly called by bomsec
      if(ityp.eq.0) then
c
c ---------------------------------------------------------
c		Set Scalers for Diversions      
        idy=0
        do nd=1,numdiv
          diverx(nd)=diver(mon,nd)
c
c rrb 01/12/17; Allow daily model to use monthly demand
          divreqd(nd)=diverx(nd)
c
c rrb 01/04/23; Set to IWR data when ieffmax is on  
          if(ieffmax.le.0) then
            diwrx(nd) =diverir(mon,nd)              
          else
            diwrx(nd) =diwr(mon,nd)
c           write(io99,*) '  Demand 1; nd, diwrx(nd)',nd,diwrx(nd)*fac
          endif

c         write(io99,*) ' Demand monthly; nd, diverx(nd)',nd,diverx(nd)
        end do
c
c ---------------------------------------------------------
c		Set Scalers for Wells
        do nw=1,numdivw
          diverwx(nw)=diverw(mon,nw)   
c
c rrb 01/04/23; Set to IWR data when ieffmax is on
          if(ieffmax.le.0) then
            diwrwx(nw)=diverirw(mon,nw)
          else
            diwrwx(nw)=diwrw(mon,nw)         
          endif
          
          rec12=cdividw(nw)        
cx         if(rec12.eq.'CBPAGGZ22   ') then
cx           write(nlog,*) ' Demand; CBPAGGZ22 @ Step1', nw, 
cx    1       ieffmax, diverirw(mon,nw)*fac, diwr(mon,nw)*fac
cx         endif            
cx         write(io99,*) '  Demand monthly; nw, diverwx(nw)',nw,diverwx(nw)
        end do

      endif
c
c ---------------------------------------------------------               
c               Step 1b; Daily called by dayset
      if(ityp.eq.1) then
c      
c ---------------------------------------------------------
c		Set Scalers for Diversions
        do nd=1,numdiv
c         write(io99,*) '  Demand; nd, idy', nd,idy
c
c rrb 01/12/01; Allow daily model to use monthly demand
c               Note if idaydem =1 (on):
c               On day 1 set to monthly demand (e.g.
c                 diverd(32,nd)*mthday(mon))
c               On day 2-n set to old value which
c                 got reduced by divrig, etc.
c         diverx(nd)=diverd(idy,nd) 

          divreqd(nd)=diverd(idy,nd)
          if(idaydem.eq.0) then
            diverx(nd)=diverd(idy,nd)
          else
            if(idy.eq.1) then
              diverx(nd)=diverd(32,nd)*float(mthday(mon))
            else
              diverx(nd)=divreq(nd)
            endif
          endif

c
c rrb 01/04/23; Set to IWR data when ieffmax is on
c         diwrx(nd) =diwrd(idy,nd)
          if(ieffmax.le.0) then
            diwrx(nd) = diverird(idy,nd)
          else
            diwrx(nd) =diwrd(idy,nd)
          endif                         
        end do
c
c ---------------------------------------------------------
c		Set Scalers for Wells
        do nw=1,numdivw
          diverwx(nw)=diverdw(idy,nw)
c
c rrb 01/04/23; Set to IWR data when ieffmax is on
c         diwrwx(nw)=diwrdw(idy,nw)
          if(ieffmax.le.0) then
            diwrwx(nw)= diveridw(idy,nw)
          else
            diwrwx(nw)=diwrdw(idy,nw)
          endif
        end do

      endif
c _________________________________________________________
c               Step 2; Initilize Diversion Demand (DIVREQ) 
c		Note Same calculations for Monthly or Daily
c               Note may be adjusted to include well demands below
c		Note numuse is number of diversion users
c      
      DO nd=1,NUMUSE
        divreq(nd)=diverx(nd)
        diwrreq(nd)=diwrx(nd)
        divsw(nd)=divreq(nd)
c
c rrb 01/12/17; Allow daily model to use monthly demand
c               Not used for monthly
        divreqd(nd)=divreqd(nd) 
c
c rrb 2006/09/11; New CU Approach,
c		Note AreaSF(nd) is a fraction
        dIwrSF(nd)=diwrreq(nd)*AreaSF(nd)	
        dIwrSS(nd)=diwrreq(nd)*AreaSS(nd)	
        dIwrGF(nd)=diwrreq(nd)*AreaGF(nd)	
        dIwrGS(nd)=diwrreq(nd)*AreaGS(nd)        
      end do

c
c _________________________________________________________
c               Step 3; Initilize well demand (divreqw) 
c                       and adjust diversion demand based on
c                       how well data is provided
c rrb 01/01/15;         Note for sprinkler demand (isprink>0), 
c                       datinp.f checks that we have appropriate data
      do nw=1,numdivw
c
c ---------------------------------------------------------
c               Initilize
c
        ca=1.0
        nd=idivcow2(nw)   

        if(ieffmax.le.0) then
          effw=diveffw(mon,nw)*0.01
          effs1=diveffw(mon,nw)*0.01
        else
          effw=effmaxw(nw)*0.01
          effs1=effmaxs(nw)*0.01
        endif
c
c _________________________________________________________
c		Step 3a Historic Demand Approach (idemtyp=1)
c		data in ddm and *.wem, do not add
        if(idemtyp.eq.1) then
          nd=idivcow2(nw)   
c          
c ---------------------------------------------------------
c 		Well Only Lands (nd=0)
          if(nd.eq.0) then
            divreqw(nw) =diverwx(nw)
            diwrreqw(nw)=diwrwx(nw)
            
            rec12=cdividw(nw)        
            
cx            if(rec12.eq.'CBPAGGZ22   ') then
cx              write(nlog,*) ' Demand; CBPAGGZ22 @ 3b1', nw, 
cx     1         divreqw(nw)*fac, diwrreq(nw)*fac
cx            endif  
          else  
c
c ---------------------------------------------------------
c		D&W Lands (nd>0)
            divreqw(nw) =diverwx(nw)
            diwrreqw(nw)=diwrreq(nd)* (AreaGF(nd) + AreaGS(nd))
            ca = 1.0
            if(itsfile.eq.1 .or. itsfile.eq.10) then
              izero=0
              call coeffa(areawa(nw), area(nd), small, ca, 
     1                    izero, ioutX, io99, cdivid(nd)) 
            endif
            diwrreqw(nw)=diwrwx(nd)* ca
          endif
        endif
        
c
c _________________________________________________________
c               Step 3b; Historic Sum Approach (idemtyp=2)
        if(idemtyp.eq.2) then
          nd=idivcow2(nw)
c          
c ---------------------------------------------------------
c 		D&W Lands (nd>0)
          if(nd.gt.0) then
            iuse=nduser(nd)
            if(diveff(mon,iuse).gt.0.0001) then
              ceff = diveffw(mon,nw)/diveff(mon,iuse)
            else
              ceff = 0.0
              write(io99,100)
            endif
c
            divreqd(iuse) = divreqd(iuse) + diverwx(nw) * ceff
            diwrreq(iuse) = diwrreq(iuse) + diwrwx(nw) 
            divsw(iuse)   = divreq(iuse)
            diwrx(iuse)   = diwrx(iuse) + diwrwx(nw)*ceff
            
c
c rrb 00/11/18; Limit well demand to fraction of Total area (area(nd))
c               Note if D&W, divreqw uses SW efficiency
c               Note if Well only, divreqw uses GW efficiency
c
            ca = 1.0
            if(itsfile.eq.1 .or. itsfile.eq.10) then
              izero=0
              call coeffa(areawa(nw), area(nd), small, ca, 
     1                    izero, ioutX, io99, cdivid(nd)) 
            endif
            divreqw(nw) = divreqd(iuse) * ca
            diwrreqw(nw)= diwrx(iuse) * ca     
          else
c          
c ---------------------------------------------------------
c               Well Only (nd2=0)
            divreqw(nw) = diverwx(nw) 
            diwrreqw(nw)= diwrwx(nw)
          endif
        endif
c _________________________________________________________
c               Step 3c; Structure Demand Approach (idemtyp=3)
c		Demand in *.ddm for D&W or *.wem for Well only
        if(idemtyp.ge.3) then
          nd = idivcow2(nw)
c          
c ---------------------------------------------------------
c               D&W Structures (Demand in *.ddm)
          if(nd.gt.0) then
            iuse=nduser(nd)
            divreq(iuse) = divreq(iuse)
c
c rrb 01/12/17; Allow daily model to use monthly demand
            divreqd(iuse)=divreqd(iuse)
            divsw(iuse)=divreq(iuse)
c
c rrb 00/11/18; Limit well demand to fraction of Total area (area(nd))
c           divreqw(nw) = 0.0 
c               Note if D&W, divreqw uses SW efficiency
c               Note if Wel only, divreqw uses GW efficiency 
            if(itsfile.eq.1 .or. itsfile.eq.10) then
              izero=0
              call coeffa(areawa(nw), area(nd), small, ca, 
     1                    izero, ioutX, io99, cdivid(nd)) 
            endif
c
c rrb 01/12/17; Allow daily model to use monthly demand
c           divreqw(nw) = divreq(iuse) * ca
            divreqw(nw) = divreqd(iuse) * ca    
            diwrreqw(nw)= diwrx(iuse) * ca     
          else
c          
c ---------------------------------------------------------
c               Well Only Structures
c
            divreqw(nw) = diverwx(nw)
            diwrreqw(nw)= diwrwx(nw)
          endif  
        endif
c
c          
c ---------------------------------------------------------
c rrb 2006/09/11; New CU
c                Step 4; Set IWR for gw lands irregardless of method
c		Note AreaGFw(nw) is a fraction
          
        dIWRGFw(nw)=diwrreqw(nw)*AreaGFw(nw)
        dIwrGSw(nw)=diwrreqw(nw)*AreaGSw(nw) 
        
        rec12=cdividw(nw)        
cx        if(rec12.eq.'CBPAGGZ22   ') then
cx          write(nlog,*) ' Demand; CBPAGGZ22 ', nw, 
cx     1     diwrreqw(nw)*fac, AreaGFw(nw), dIWRGFw(nw)*fac
cx        endif
c
c ---------------------------------------------------------
c               End well loop
          
      end do     
c
c      
c _________________________________________________________
c rrb 01/12/17; Print all data
      if(iout.eq.1) then
        if(iprintd.eq.0) write(io99,130) iyrmo(mon), xmonam(mon), 
     1    idy, ityp

        do nd=1, numdiv
          nw=idivco2(nd)

          iprintd=iprintd+1
c
c             Print D&W lands
          if(nw.gt.0) then
            write(io99,132)
     1      iprintd, idaydem, idemtyp, nd, nw, cdivid(nd), 
     1      areasp(nw),       areawa(nw),      area(nd), 
     1      effmaxs(nw),      effmaxw(nw),     diwrx(nd)*fac,               
     1      divreqw(nw)*fac,  
     1      divsw(nd)*fac,    divreq(nd)*fac,  divreqd(nd)*fac,
     1      diwrreq(nd)*fac,
     1      divnam1(nd)
          else
c
c             Print Div only lands
            write(io99,132)
     1      iprintd, idaydem, idemtyp, nd,  0, cdivid(nd), 
     1      areasp(nd),       areawa(nd),      areawa(nd), 
     1      effmaxs(nd),      effmax(nd),      diwrx(nd)*fac, 
     1      0.0,             
     1      divsw(nd)*fac,    divreq(nd)*fac,  divreqd(nd)*fac,
     1      diwrreq(nd)*fac,
     1      divnam1(nd)
          endif
        end do
        
c
c             Print Well only lands
        do nw=1,numdivw
          nd = idivcow2(nw)
          if(nd.eq.0) then
            iprintd=iprintd+1
c
c             Print Well Only lands
            write(io99,132)
     1      iprintd, idaydem, idemtyp, 0,  nw, cdividw(nw), 
     1      areasp(nw),       areawa(nw),      areawa(nw), 
     1      effmaxs(nw),      effmaxw(nw),     diwrwx(nw)*fac, 
     1      divreqw(nw)*fac, 
     1      0.0,              0.0,             0.0,
     1      0.0,
     1      divnamw1(nw)
          endif
        end do 
      endif
c
c _________________________________________________________
c               Step 4; Return
      return
c
c _________________________________________________________

  100  format(
     1 '  Demand; Warning canal efficiency is < 0. May ',/
     1 '          have a problem calculating total demand')
  130  format(/, '  Demand; Demand data for Div, D&W & Well Only',   
     1 ' Year ', i5, ' Month ', a4, ' Day ', i5,/
     1 '          Called by ', i5, ' where 0=bomsec & 1=dayest',/
     1 '    # Idem DTyp   Nd   Nw ID          ',
     1 '  AreaSP  AreaGW AreaTot effmaxs effmaxw     IWR', 
     1 ' divreqw   divsw  divreq divreqd diwrreq',
     1 ' Name', /,
     1 ' ____ ____ ____ ____ ____ ____________',
     1 ' _______ _______ _______ _______ _______ _______', 
     1 ' _______ _______ _______ _______ _______ _______ _______',
     1 1x,24('_'))
  132  format(5i5, 1x, a12, 11f8.0, 1x, a24)

c9999 Stop

      end
