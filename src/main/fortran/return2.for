c
c *********************************************************
c
       subroutine Return2(
     1   nlog,   iyr,    mon,  
     1   nd,     nw,   ieffmax, ieff2,  divact,
     1   itype,  effC,   effF,   effS,    
     1   AreaSF, AreaSS, AreaGF, AreaGS, area1,
     1   dIwrSF, dIwrSS, dIwrGF, dIwrGS,
     1   cuactSF1, cuactSS1, cuactGF1, cuactGS1,
     1   divcapw,ispr,
     1   effave, effmax, fac,    rettot,
     1   diwrx,  dcux,   dcus,   cuact,
     1   isoil,  soils,  awcr,   qdivs1,
     1   ichk,   cdividx,ccall)
c
c
c _________________________________________________________
c	Program Description
c
c     Return2; it calculates returns (rettot), IWR remaining
c            (diwrx), and actual CU (dcux) given actual
c             diversion (divact) and efficiecny (eff).
c	      Same as Return.f but revised to treat the following
c             uniquely:
c		1. Surface Water Flood Irrigated
c		2. Surface Water Sprinkler Irrigated
c             Note efficiency varies from average to maximum
c             based on switches ieffmax and ieff2
c     CALLED BY: Rtnsec and RtnsecW
c _________________________________________________________
c       Update History
c
c rrb 2006/08/31; Revised Return to include new CU approach
c
c _________________________________________________________
c       Documentation
c
c               nlog    log file #
c               nd      diversion ID
c               nw      well ID
c
c		itype   1 New CU Approach SW Only
c		itype   2 New CU Approach D&W Structure
c		itype   3 New CU Approach Well Only Structure
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
c
c               effC	Carrier Efficiency
c               effF	Flood Efficiency
c		effS	Sprinkler Efficiency
c		
c		FOR A GIVEN CALL (SURFACE WATER OR GROUND)
c		AreaF	Fraction of land Flood irrigated
c		AreaS	Fraction of land Sprinkler irrigated
c
c               fac     converts cfs to af
c
c               rettot  return flow    
c               diwrT   irrigation water requirement for this time step
c			(does not get adjusted as water is supplied)
c               diwrx   irrigation water requirement going out of sub
c               dcux    running total consumptive use for structure
c                       Note if a D&W it is SW + GW
c               dcus    running total CU by source (SW or GW)
c
c               soils   soil moisture storage (af)
c               awcr    available water capacity (af)
c               qdivs   diversion to soil moisture this iteration (cfs)
c               qdivs1  total diversion to soil moisture this time step (cfs)
c
c               ichk    detailed output control
c               cdividx Diversion or Well ID (print divesion for D&W)
c
c               iout    0=print detailed results
c                       1=do not print
c               cumax   maximum CU (function of max eff)
c               cuact   actual CU
c
c		ispr    -1 SW supply
c			0  GW supply Non Sprinkler
c			1  GW supply Sprinkler
c			
c
c
c _________________________________________________________
c	Dimensions
c		
       dimension ctype(5)
       character cdividx*12, ctype*16, ccall*12
       
       ctype(1)='Diversion       '
       ctype(2)='Diversion & Well'
       ctype(3)='Well Only       '
       ctype(4)='NA              '
       ctype(5)='NA              '
c
c _________________________________________________________
c		Step 1; Initilize
c		iout=0 no details
c		    =1 details
c		    =2 summary (divert, CU, Soil, Return)
c		    =3 StateCU format
       iout=0
       
c
c							Detailed printout for a selected structure       
cx       if(cdividx.eq.'01_AWP022   ') iout=3
cx       if(nw.eq.44) iout=3
       
       if(ichk.ge.90 .and. ichk.le.100) iout=1
       
       if(iout.gt.0) then
         write(nlog,*) ' '
         write(nlog,*)' ___________________________________________'
         write(nlog,*) ' Return2; iout, ID = ', 
     1     iout, cdividX
       endif         
        

       diwrx1=diwrx
       soils1=soils

       qdivs=0.0
       rettot=0.0
       
       ToFarm=1.0/fac
       
       cumax=0.0
       cuact=0.0
       delta=0.0
       
       cumaxSF=0.0
       cumaxSS=0.0
       cumaxGF=0.0
       cumaxGS=0.0
       
       cuactSF=0.0
       cuactSS=0.0
       cuactGF=0.0
       cuactGS=0.0
       
       qdivsSF=0.0
       qdivsSS=0.0
       qdivsGF=0.0
       qdivsGS=0.0
       
       dIwrSF1=dIwrSF
       dIwrSS1=dIwrSS
       dIwrGF1=dIwrGF
       dIwrGS1=dIwrGS
              
       small=0.001
c
c		Calculate area from fraction data
       AreaSF1=AreaSF*Area1
       AreaSS1=AreaSS*Area1
       AreaGF1=AreaGF*Area1
       AreaGS1=AreaGS*Area1
      
c
c _________________________________________________________
c
c               Step 1; Calculate maximum CU (cumax) based on
c                       Average efficiency approach when variable
c                       efficiency is off (ieffmax=0 or 2) or 
c			always using average efficiency (ieff2 = 0)
c
       if(ieffmax.le.0 .or. ieffmax.eq.2 .or. ieff2.eq.0) then
c        write(nlog,*) ' Return2; ieff2=0'
cx       DivactT=Divact
         eff=effave
         cumax=divact*eff/100.0
         cuact=cumax
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
c ________________________________________________________
c
         if(ieffmax.le.0 .or. ieff2.eq.0) then       
           diwrx=-1./fac
           diwrx1=-1./fac
         else
           diwrx = diwrx - cuact
           diwrx = amax1(diwrx, 0.0)
         endif
         goto 50  
       endif

c
c _________________________________________________________
c
c		New CU approach Surface Water Only (itype=1)
c ---------------------------------------------------------
c		Surface Water Supply (diversion)
c		  Apply Canal Loss
c		  Spread to all 4 land use types
c	          Note AreaT should equal 1.0, data is initilized
c		  to zero above
       if(itype.eq.1) then
         ToFarm=divact*effc
         AreaT=AreaSF+AreaSS+AreaGF+AreaGS

         cuMaxSF=ToFarm*AreaSF*effF
         cuActSF=amin1(dIwrSF, cuMaxSF)
         dIwrSF=dIwrSF-cuActSF         
         cuActSF1=cuActSF1+CuActSF
         
         cuMaxSS=ToFarm*AreaSS*effS
         cuActSS=amin1(dIwrSS, cuMaxSS)       
         dIwrSS=dIwrSS-cuActSS
         cuActSS1=cuActSS1+CuActSS
       
         cuMaxGF=ToFarm*AreaGF*effF
         cuActGF=amin1(dIwrGF, cuMaxGF)
         dIwrGF=dIwrGF-cuActGF
         cuActGF1=cuActGF1+CuActGF
         
         cuMaxGS=ToFarm*AreaGS*effS
         cuActGS=amin1(dIwrGS, cuMaxGS)
         dIwrGS=dIwrGS-cuActGS
         cuActGS1=cuActGS1+CuActGS
         
         cuMax=cuMaxSF+cuMaxSS+cuMaxGF+cuMaxGS
         cuAct=cuActSF+cuActSS+cuActGF+cuActGS
         cuActT=cuActSF1+cuActSS1+cuActGF1+cuActGS1
         
       endif
c
c
c _________________________________________________________
c
c		New CU approach D&W or Well Only (itype=2 or 3)
c		Ground Water by Sprinkler Supply (ispr=1) 
       if(itype.gt.1 .and. ispr.eq.1) then

         ToFarm=divact
         AreaT=AreaGF+AreaGS
c
c		Spread pumping capacity to Srinkler Lands Preferentially
cx              cuMaxGS=amin1(dIwrGS, ToFarm*effS, divcapw*effS)
         cuMaxGS=amin1(dIwrGS, ToFarm*effS)     
         cuActGS=amin1(dIwrGS, cuMaxGS)
         dIwrGS=dIwrGS-cuActGS
         cuActGS1=cuActGS1+cuActGS
         
         cuMax=cuMaxGS+cuMaxGF
         cuAct=cuActGS+cuActGF
       endif
       
c
c _________________________________________________________
c
c		New CU approach D&W or Well Only (itype=2 or 3)
c		Ground Water without  Sprinklers
       if(itype.gt.1 .and. ispr.eq.0) then

         ToFarm=divact
cx       AreaT=AreaFF+AreaFS
         AreaT=AreaGF+AreaGS

cx              cuMaxGF=amin1(dIwrGF, ToFarm*effF, divcapw*effF)
         cuMaxGF=amin1(dIwrGF, ToFarm*effF)
         cuActGF=amin1(dIwrGF, cuMaxGF)
         dIwrGF=dIwrGF-cuActGF
         cuActGF1=cuActGF1+cuActGF
         
         cuMax=cuMaxGS+cuMaxGF
         cuAct=cuActGS+cuActGF
       endif
       
c
c _________________________________________________________
c
c               Step 2; Add to Soil Moisture (if excess)
c		        Note ItypSM=0 treat soil as a single bucket
c			     (e.g. do not limit to soil under each
c			     land type)
c
       if(isoil.eq.1) then
c
c rrb 2008.09/07; Correction       
c        itypSM=0
         itypSM=0
         call SoilIN(nlog, itypSM, awcr, soils, AreaSF, AreaT, 
     1     cumaxSF, cuactSF, qdivsSF, fac, cdividx, ccall)
     
         call SoilIN(nlog, itypSM, awcr, soils, AreaSS, AreaT, 
     1     cumaxSS, cuactSS, qdivsSS, fac, cdividx, ccall)
     
         call SoilIN(nlog, itypSM, awcr, soils, AreaGF, AreaT, 
     1     cumaxGF, cuactGF, qdivsGF, fac, cdividx, ccall)
     
         call SoilIN(nlog, itypSM, awcr, soils, AreaGS, AreaT, 
     1     cumaxGS, cuactGS, qdivsGS, fac, cdividx, ccall)
c
         qdivS=qdivsSF+qdivsSS+qdivsGF+qdivsGS
         qdivS1=qdivS1 + qdivS
       endif  
         
c _________________________________________________________
c
c               Step 3; Calculate return flow (rettot)
c

       rettot = divact - cuact - qdivs
       rettot = amax1(rettot, 0.0)
       
       if(iout.ge.2) then
         delta=divact-cuact-qdivs-rettot
         write(nlog,*) ' '
         write(nlog,*) 
     1     ' Return2;   divact   cuact   qdivs  rettot  delta'
         write(nlog,'(10x, 20f8.2)')
     1     divact*fac, cuact*fac, qdivs*fac, rettot*fac,delta*fac
        endif
c
c               Step 4; Adjust irrigation water requirement
c                       Note not used if ieffmax .ne. 1 so set to -1)
c rrb 02/03/01; Revise to adjust if ieffmax = 2
c ________________________________________________________
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
  50   continue
c      write(nlog,*) ' Return2; at 50'
  
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
c		Output like StateCU       
c

       if(AreaT.gt.small) delta=abs(divact-cuact-rettot-qdivs)
       if(iout.eq.3) then
         write(nlog,150) cdividx, ctype(itype), nd, nw, ieffmax, ieff2
         
         cIwr=dIwrSF1+ dIwrSS1+ dIwrGF1+ dIwrGS1
         cIwrT=dIwrSF+ dIwrSS+ dIwrGF+ dIwrGS

         ShortSF=diwrSF1-cuactSF
         ShortSS=diwrSS1-cuactSS
         ShortGF=diwrGF1-cuactGF
         ShortGS=diwrGS1-cuactGS
         ShortT=ShortSF+ShortSS+ShortGF+ShortGS
         
         
         write(nlog,160) iyr, mon, cdividx, ispr,
     1     divact*fac,  ToFarm*fac,
     1     AreaSF1,     AreaSS1,AreaGF1,AreaGS1,Area1, 
     1     dIwrSF1*fac, dIwrSS1*fac, dIwrGF1*fac,dIwrGS1*fac,cIwr*fac,
     1     cuactSF*fac, cuactSS*fac, cuactGF*fac,cuactGS*fac,cuact*fac,
     1     soils1,      qdivs*fac, soils, delta*fac,
     1     shortSF*fac, ShortSS*fac, ShortGF*fac, ShortGS*fac, 
     1     ShortT*fac,
     1     cuactSF1*fac,cuactSS1*fac, cuactGF1*fac,cuactGS1*fac,
     1     cuactT*fac,
     1     dIwrSF*fac, dIwrSS*fac, dIwrGF*fac,dIwrGS*fac,cIwrT*fac,
     1     dcuX*fac, dcuS*fac, rettot*fac
     

         if(delta.gt.small) then
           write(nlog,120) delta*fac
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
     1 '  Return2; for ID = ', a12, ' Type = ', a16,
     1 ' Div # = ', i5,' Well # = ', i5,
     1 ' ieffmax = ', i5, ' and ieff2 = ', i5,/
     1 ' Year  Mon ID          ','    ispr', 
     1 '  Divert  ToFarm  EffAve    EffC    EffF    EffS',
     1 '  AreaSF  AreaSS  AreaGF  AreaGS   AreaT',
     1 '  dIwrSF  dIwrSS, dIwrGF, dIwrGS,  dIwrT',
     1 '   IWR-1   CUmax   CUact',
     1 '  Return  EffMax  EffAct   TotCU   IWR-2',
     1 '  Soil-1  ToSoil   Soil2   Delta   ',/
     1 ' ____ ____ ____________',' _______',
     1 ' _______ _______ _______ _______ _______ _______',
     1 ' _______ _______ _______ _______ _______',
     1 ' _______ _______ _______ _______ _______',
     1 ' _______ _______ _______',
     1 ' _______ _______ _______ _______ _______',
     1 ' _______ _______ _______ _______')
 110   format(2i5, 1x, a12, i8, 2f8.0, 4f8.2, 5f8.3, 20f8.0)
 120   format('  Return2; Problem delta see above = ', f10.4)
 
 150   format(/,
     1 '  Return2; for ID = ', a12, ' Type = ', a16,
     1 ' Div # = ', i5,' Well # = ', i5,
     1 ' ieffmax = ', i5, ' and ieff2 = ', i5,/
     1 ' Year  Mon ID          ','    ispr', 
     1 '  Divert  ToFarm',
     1 '  AreaSF  AreaSS  AreaGF  AreaGS   AreaT',
     1 ' dIwrSF1 dIwrSS1 dIwrGF1 dIwrGS1    CIwr',
     1 ' cuactSF cuactSS cuactGF cuactGS   CUact',
     1 '  Soil-1  ToSoil   Soil2   Delta',
     1 ' ShortSF ShortSS ShortGF ShortGS  ShortT'     
     1 ' cuactSF cuactSS cuactGF cuactGS   CUact',
     1 '  dIwrSF  dIwrSS  dIwrGF  dIwrGS,  cIwrT'
     1 '    dcuS    dcuX  RetTot',//
     1 ' ____ ____ ____________',' _______',
     1 ' _______ _______ _______ _______ _______ _______',
     1 ' _______ _______',
     1 ' _______ _______ _______ _______ _______',
     1 ' _______ _______ _______ _______ _______',
     1 ' _______ _______ _______ _______ _______',
     1 ' _______ _______ _______ _______ _______',
     1 ' _______ _______ _______ _______ _______',
     1 ' _______ _______ _______ _______ _______ _______')
     
 160   format(2i5, 1x, a12, i8, 2f8.2, 5f8.2, 5f8.2, 5f8.2, 30f8.2)
 
c
c _________________________________________________________
c               Error warnings
 9999 write(6,200) 
      write(99,210) 
 200  format('    Stopped in Return2',/,
     1       '    See the *.log file')
 210  format('    Stopped in Return2')
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop 
      END

