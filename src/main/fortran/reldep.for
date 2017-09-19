c
c**********************************************************      
c     
c
      Subroutine RelDep(nlog,  iyr, mon, iw, iwx, cidvri,cdivid1,
     1   maxsta, numsta, avail, avtemp, idncod,
     1   fac, ieffmax, ieff2, 
     1   effave1, effmax1, facdly, diwrreq1,
     1   idcd, ndnD, idcS, ndnS, divact, RELACT)
c     
c
c _________________________________________________________
c	Program Description
c
c		A simpler approach to make a reservoir release to
c                meet a depletion only. It lets the Avail array 
c                tell us if the location and amount of return flows
c                is critical when releasing to meet a depletion.  
c                It works as follows:
c
c		1; Check avail is OK
c		2; Set avail to avtemp
c		3; Estimate release to be CU at max efficiency 
c		4; Adjust avtemp for diversion
c		5; Adjust avtemp for release
c		6; Check avtemp for negatives
c		7; If Avtemp >=0, Done (released to meet depletion)
c		8; If Avtemp <0, the location or timing of return flows
c		   is important. Therefore set additional release by the
c                  negative amount and adjust Avtemp
c	        9; Check Avtemp for negatives
c	       10; Print details, if requested
c              11; If Avtemp >=0, Done (released to meet depletion)
c	       11; If Avtemp <0, Huston we have a problem.

c _________________________________________________________
c	Dimensions
c
      dimension avail(maxsta), avtemp(maxsta),IDNCOD(maxsta)
      character cidvri*12,cdivid1*12
c
c _________________________________________________________
c		Step 1; Initilze
c      
c		iout=1 details
c		iout=2 summary details
      iout=1
      relact1=relact  
      relact2=0.0
      relact3=0.0
      smalln=-1.*0.001
c
c _________________________________________________________
c
c		Step 1; Check avail is OK (stop if AVAIL<0 (istop=1))
      istop=1		
      call chekav2(22, maxsta, numsta, istop, fac, AVAIL, IMCD, AvMin1)
c
c _________________________________________________________
c
c		Step 2; Set avail to avtemp      
      do i=1,numsta
        avtemp(i)=avail(i)
      end do
c
c _________________________________________________________
c        
c		Step 3; Estimate release to be diversion less immediate 
c                 return irregardless of where the return comes in
      if(ieffmax.le.0 .or. ieffmax.eq.2 .or. ieff2.eq.0) then
        eff=effave1
        cumax=divact*eff/100.0
        cuact=cumax
      else
        eff=effmax1
        cumax=divact*eff/100.0
        cuact=amin1(cumax,diwrreq1)
      endif
      
c     FORET=1.0-eff/100.
c     RET=divact*foret*FACDLY/100.

      RET=(divact-cuact)*FACDLY/100.

      retN=-1.0*ret
      relact2=-1*(divact-ret)
      
      if(iout.eq.1) then
        write(nlog,*) 
     1  ' RelDep; ieffmax, ieff2, eff, diwrreq1, cuact,',
     1  ' facdly, ret, relact2'
        write(nlog,*) 
     1  ' RelDep;', ieffmax, ieff2, eff, diwrreq1*fac,cuact*fac,
     1  facdly, ret*fac, relact2*fac
       endif
      
c
c _________________________________________________________
c
c		Step 4; Adjust avtemp for diversion
c		Remove diversion (divact) at diversion location (iscD)
c		and all downstream nodes (ndnD)        
      call takou2(maxsta, AVTEMP, idncod, divact, ndnD, idcD)
c
c _________________________________________________________
c
c		Step 5; Adjust avtemp for release
c		Add reservoir release (relact) a negative
c               at source location (iscS)and downstream nodes (ndnS)                
      call takou2(maxsta, AVTEMP, idncod, relact2, ndnS, idcS)
c
c _________________________________________________________
c
c		Step 5; Adjust avtemp for return flow
c		Assume it occurrs at the diversion point (ndnD, idcD)
      call takou2(maxsta, AVTEMP, idncod, RETN, ndnD, idcD)
c
c _________________________________________________________
c
c		Step 6; Check Avtemp for negatives
      istop=0
      call chekav2(2, maxsta, numsta, istop, fac, AVTEMP, IMCD, AvMin2)
c _________________________________________________________
c      
c		Step 7; If Avtemp >=0, Done (released to meet depletion)
        
      if(avMin2.gt.-1*smalln) goto 200
c      
c _________________________________________________________
c
c		Step 8; If Avtemp <0, the location of returns
c		   is important. Therefore set additional release by the
c                  negative amount and adjust Avtemp.
      relact3=AvMin2
      call takou2(maxsta, AVTEMP, idncod, relact3, ndnS, idcS)
c
c _________________________________________________________
c
c	        Step 9; Check Avtemp
      istop=0
      call chekav2(2, maxsta, numsta, istop, fac, AVTEMP, IMCD, AvMin3)
c
c		Step 10; Set new release      
 200  relact=relact2+relact3
c _________________________________________________________
c
c		Step 10; Print details

      if(iout.eq.1) then
        write(nlog,250)  cidvri, cdivid1,iyr, mon, iw, iwx, 
     1    divact*fac, relact1*fac, Eff,  diwrreq1*fac, Ret*fac,
     1    relact2*fac, AvMin2*fac, relact3*fac, RELACT*FAC
      endif
      
      if(iout.ge.1) then
        write(nlog,260)  cidvri, cdivid1, iyr, mon, iw, iwx, 
     1    divact*fac, RELACT*FAC
      endif
c
c		Check results for reasonableness      
      if(abs(relact) .gt. abs(relact1)) then
        write(nlog,270) relact1, relact
        goto 9999
      endif        
c
c _________________________________________________________
C      
C		Step 11; If Avtemp >=0, Done (released to meet depletion)
        
      if(AvMin3.gt.smalln) goto 500
c
c _________________________________________________________
C      
C		Step 12; If Avtemp < Problem
      write(nlog,520) imcd, avMin3, avMin3*fac
      goto 9999
c _________________________________________________________
c
c		Return	
 500  return
 
c _________________________________________________________
c
c		Error Processing
c
      
 9999 write(nlog,510)
      stop
c _________________________________________________________
c
c		Formats      
c

 250  format(/, 72('_'),/
     1 '  RelDep; ID          ID             Yr   Mo   Iw  Iwx',
     1 '    Divact   Relact1       Eff  DiwrReq1    Ret',
     1 '   Relact2   Avmin2    Relact3    RELACT',/       
     1 '          ', 2a12, 4i5,20f10.2)
 260  format(/, 72('_'),/
     1 '  RelDep; ID          ID             Yr   Mo   Iw  Iwx',
     1 '    Divact    RELACT',/       
     1 '          ', 2a12, 4i5,20f10.2)
     
 270  format(/, 72('_'),/
     1 '  RelDep; Problem releasing more than diverted',/
     1 '  RelDep; ID             Yr   Mo   Iw  Iwx',
     1 '   Relact1    RELACT',/       
     1 '          ', a12, 4i5,20f10.2)
     
 510  format('    Stopped in RelDep',/,
     1       '    See the *.log file')
 520  format(
     1 '    Stopped in RelDep',/
     1 13x, 'Negative avail at node ', i5, '. Avail = ', f10.3,' cfs',
     1 f10.3, ' af')
      end
