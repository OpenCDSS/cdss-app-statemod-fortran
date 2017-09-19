c *********************************************************
C
      subroutine soilcu2(io99,  nd, nw, small, fac, divreq, diwrreq,
     1                  soils, dcux,  divact, ichk,cdividx)
c
c
c _________________________________________________________
c	Program Description
c
c       SoilCu2; It calculates soil Moisture Use
c		 for 4 land use types.
c                Called by Soilm.f
c               
c               Note datip checks if isoil.ne.1 or itsfile.ne.1 or
c               ieffmax.ne.1 
c               Also oprinp checks if isoil=1 but no operating rule provided 
c
c _________________________________________________________
c       Documentation
c               io99    = output file number
c               nd      = diversion ID
c               nw      = well ID
c               small   = a small number
c               fac     = unit conversion  cfs to af
c
c               divreq  = demand (cfs)
c               diwrreq = IWR demand (cfs)
c               soils   = soil moisture Storage (af)
c               dcux    = CU (cfs)
c               divact  = diversion to soil (cfs)
c       
c               ichk    = detailed output control
c               cdividx = Diversion or Well ID
c
c rrb 01/07/31; Revised to allow as a function of IWR only 
c               not IWR and demand
c
c _________________________________________________________
c	Dimensions
c
c               Step 1; Initilize
      character cdividx*12

c
c _________________________________________________________
c               Step 1; Initilize
c     write(io99,*) '  Into SoilCU for nd, nw', nd, nw

      iout=0
      if(ichk.eq.92) iout=1

      divact=0.0

      soils1=soils
      divreq1=divreq
      diwrreq1=diwrreq
      dcu1=dcux

      if(soils.gt.small) then
c
c _________________________________________________________
c
c               Step 2; Diversion is min demand, IWR demand,
c                       soil moisture (cfs)
c rrb01/02/27; Limit Soil Cu to demand, IWR and soil moisture
c       divact = amin1(divreq,soils/fac)
c
c rrb 01/07/31; Limit Soil CU to IWR and soil moisture only
c               e.g. Soil CU is OK if an IWR but no demand
c                    key for historic operation
c       divact = amin1(divreq,diwrreq,soils/fac)
        divact = amin1(diwrreq,soils/fac)

        divact = amax1(divact, 0.0)
c
c _________________________________________________________
c
c               Step 3; Update Soil Moisture (af)
        soils=soils-divact*fac
c
c _________________________________________________________
c
c               Step 4; Update Demand (cfs)
c
c rrb 01/07/31; Limite Soil CU to IWR and soil moisture only 
c       divreq=divreq-divact
        divreq=amax1(divreq-divact, 0.0)
c
c _________________________________________________________
c
c               Step 5; Update IWR Demand (cfs)
        diwrreq=diwrreq-divact
c
c _________________________________________________________
c               Step 5; Update CU (cfs)
        dcux=dcux+divact

      endif
c
c _________________________________________________________
c
c               Step 6; Check
      if(iout.eq.1) then
        write(io99,100) cdividx, nd, nw
        write(io99,110) divreq1*fac, diwrreq1*fac, soils1, dcu1*fac,
     1    divact*fac,   divreq*fac,  diwrreq*fac,  soils,  dcux*fac
       endif     
c
c _________________________________________________________
c
c               Step 7; Return
c
      RETURN
c
c _________________________________________________________
c
c               Formats
c
 100  format(/,
     1 '  SoilCU; for ID = ', a12, ' Div# = ', i5, ' Well# = ', i5,/,
     1 '   Dem-1   IWR-1 Soils-1    CU-1  Divact   Dem-2   IWR-2',
     1 ' Soils-2    CU-2'/
     1 ' _______ _______ _______ _______ _______ _______ _______',
     1 ' _______ _______')
 110  format(20f8.0)
c
c _________________________________________________________
c
c               Print warning
c 
c9999 write(6,*) '  Stopped in SoilCU, see the log file (*.log)'
c     write(99,*) '  Stopped in SoilCU'
c     write(6,*) 'Stop 1' 
c     call flush(6)
c     call exit(1)

c     stop 
      end
     
