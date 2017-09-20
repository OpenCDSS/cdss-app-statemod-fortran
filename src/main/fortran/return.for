c
c *********************************************************
c
c
       subroutine return(
     1   nlog,   nd,     nw,   ieffmax, ieff2,divact,
     1   effave, effmax, fac,  rettot,
     1   diwrx,  dcux,   dcus, cuact,
     1   isoil,  soils,  awcr, qdivs,
     1   ichk,   cdividx)
c
c
c _________________________________________________________
c	Program Description
c
c     Return; it calculates returns (rettot), IWR remaining
c            (diwrx), and actual CU (dcux) given actual
c             diversion (divact) and efficiecny (eff).  
c             Note efficiency varies from average to maximum
c             based on switches ieffmax and ieff2
c _________________________________________________________
c       Update History
c
c rrb 02/03/01; Revised to adjust diwrx (IWR remaining) so that
c               IWR data can limit a',/
c               reservoir releases to occur only when an IWR',/
c               exists if the reservoir release variable is > 0',/
c
c _________________________________________________________
c       Documentation
c
c
c               nlog    log file #
c               nd      diversion ID
c               nw      well ID
c
c               eff     efficiency (average or maximum)
c
c               ieffmax .ne.1 use average efficiency
c                       =1 use maximum efficiency
c               ieff2   =0 use average efficiency always (e.g. calls by
c                          operating rules
c               ieff2   =1 use maximum efficiency for non operating rules
c                          only (e.g. calls by divrig if ieffmax=1)
c               divact  actual diversion
c               effave  average monthly efficiency from datinp (*.dds)
c               effmax  maximum efficiency from mdainp (*.tsp)
c               fac     converts cfs to af
c
c               rettot  return flow    
c               diwrx   irrigation water requirement going out of sub
c               dcux    running total consumptive use for structure
c                       Note if a D&W it is SW + GW
c               dcus    running total CU by source (SW or GW)
c
c               soils   soil moisture storage (af)
c               awcr    available water capacity (af)
c               qdivs   diversion to soil moisture (cfs)
c
c               ichk    detailed output control
c               cdividx Diversion or Well ID (print divesion for D&W)
c
c               iout    0=print detailed results
c                       1=do not print
c               cumax   maximum CU (function of max eff)
c               cuact   actual CU
c
c
c _________________________________________________________
c	Dimensions
c
       character cdividx*12
c
c _________________________________________________________
c		Step 1; Initilze
       iout=0
       
       if(ichk.ge.90 .and. ichk.le.100) iout=1
cx       if(cdividx.eq.'72_ADC051   ') iout=1
cx       if(cdividx.eq.'500731      ') iout=1
cx       if(cdividx.eq.'01_AWP031   ') iout=1
cx       if(cdividx.eq.'0100513     ') iout=1


       diwrx1=diwrx
       soils1=soils
       qdivs=0.0
       cuact=0.0
       rettot=0.0
c       small=0.001
       small=0.005
c
c _________________________________________________________
c
c               Step 1; Calculate maximum CU (cumax) based on
c                       efficiency approach when variable
c                       efficiency is on (ieffmax=1)
c			ieff2 = 0 use Average efficiency always
c
       if(ieffmax.le.0 .or. ieffmax.eq.2 .or. ieff2.eq.0) then
         eff=effave
         cumax=divact*eff/100.0
         cuact=cumax
       else
         eff=effmax
         cumax=divact*eff/100.0
         cuact=amin1(cumax,diwrx)
       endif
c
c _________________________________________________________
c
c               Step 2; Soil Moisture
c
      if(isoil.eq.1) then
        c1 = amax1(0.0, cumax-cuact)
        if(c1.gt.small) then

          c2=amax1(0.0, awcr-soils)
          if(c2.gt.small/fac) then
            qdivs=amin1(c1*fac, c2)/fac
            soils=soils+qdivs*fac
          endif
        endif
      endif
c _________________________________________________________
c
c               Step 3; Calculate return flow (rettot)
c

       rettot = divact-cuact-qdivs
       rettot = amax1(rettot, 0.0)
c
c               Step 4; Adjust irrigation water requirement
c                       Note not used if ieffmax .ne. 1 so set to -1)
c rrb 02/03/01; Revise to adjust if ieffmax = 2
c ________________________________________________________

c      if(ieffmax.le.0 .or. ieffmax.eq.2 .or. ieff2.eq.0) then
       if(ieffmax.le.0 .or. ieff2.eq.0) then       
         diwrx=-1./fac
         diwrx1=-1./fac
       else
         diwrx = diwrx - cuact
         diwrx = amax1(diwrx, 0.0)
       endif
c
c _________________________________________________________
c
c               Step 4; Total CU
c

       dcux=dcux+cuact
       dcus=dcus+cuact
c
c _________________________________________________________
c
c               Step 6; Calculate actual efficiency (effact)
c
       if(divact.gt.small) then
         effact=(cuact+qdivs)/divact*100
       else
         effact=0.0
       endif
c
c _________________________________________________________
c
c               Step 7; Check    
c

       delta=abs(divact-cuact-rettot-qdivs)

       if(iout.eq.1 .or. delta.gt.small) then
         write(nlog,100) cdividx, nd, nw, ieffmax, ieff2
         write(nlog,110) 
     1     divact*fac, effave,    effmax,    eff,        diwrx1*fac,
     1     cumax*fac,  cuact*fac, dcus*fac,  rettot*fac, effact, 
     1     dcux*fac,   diwrx*fac, soils1,    qdivs*fac,  soils, 
     1     delta*fac
         write(nlog,*)
     1     divact*fac, effave,    effmax,    eff,        diwrx1*fac,
     1     cumax*fac,  cuact*fac, dcus*fac,  rettot*fac, effact,
     1     dcux*fac,   diwrx*fac, soils1,    qdivs*fac,  soils,
     1     delta*fac

         if(delta.gt.small) then
           write(nlog,120) delta
           goto 9999
         endif
       endif
c
c _________________________________________________________
c
c               Step 8 ; Return
c

       return
c
c _________________________________________________________
c
c               Formats
c
 100   format(/,
     1 '  Return; Detailed outut because iout > 0 or delta > 0',/
     1 '          ID = ', a12, ' Div# = ', i5, ' Well# = ', i5,/,
     1 '          Note ieffmax = ', i5, ' and ieff2 = ', i5,/
     1 '  Return  Divert  EffAve  EffMax     Eff   IWR-1   CUmax',
     1 '   CUact    CUsx',
     1 '  Return  EFFact   TotCU   IWR-2  Soil-1  ToSoil   Soil2',
     1 '   Delta',/
     1 ' _______ _______ _______ _______ _______ _______ _______', 
     1 ' _______ _______',
     1 ' _______ _______ _______ _______ _______ _______ _______',
     1 ' _______')
 110   format('  Return',20f8.2)
 120   format('  Return; Problem delta see above = ', f10.4)
c
c _________________________________________________________
c               Error warnings
 9999 write(6,200) 
      write(99,210) 
 200  format('    Stopped in Return',/,
     1       '    See the *.log file')
 210  format('    Stopped in Return')
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop 
      END

