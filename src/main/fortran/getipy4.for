c _________________________________________________________
c
      subroutine GetIpy4(iin2)
c
c _________________________________________________________
c	Program Description
c
c		GetIpy4; it reads a *.ipy file with two land use types
c			Extracted from Mdainp.for on 9/4/2007.
c			Replaced with GetIpy4 on 9/4/2007, saved
c			for backward compatibility
c
c _________________________________________________________
c
c	Update History
c
c rrb 2007/9/04; Separated from Mdainp
c _________________________________________________________
c	Documentaiton
c		iwarnISp counter for *.ipy file sprinkler warning
c               itspd=0 no *.tsp (*.ipy) data for a diversion
c                    =1 yes *.tsp (*.ipy) rdata for a diversion
c               itspw=0 no *.tsp (*.ipy) data for a well
c                    =1 yes *.tsp (*.ipy) data for a well
c
c		ireset=0 If Sprinkler Acres > GW Acres 
c			 DO NOT reset Sprikler Acres = GW Acres
c			 (e.g. Sprinkler Acres can be Surface Water)
c		       1 If Sprinkler Acres > GW Acres 
c			 DO reset Sprikler Acres = GW Acres
c			 (e.g. Sprinkler Acres must be GW Acres)
c
c		ioutA  =1 print Area data 
c		ioutEf =1 print Eff data 
c		ioutSM =1 print Soil Moisture data
c
c _________________________________________________________
c	Dimensions
      include 'common.inc'
      dimension x(12), y(12), iwarnw(maxdivw)
      character rec3*3, rec40*40, rec12*12, rec12b*12, cCallBy*12

c
      character cistat*12, cista2*12, blank*12, recin*256, rec72*72
c
c _________________________________________________________
c
c		Step 1; Initilize      

      filena = '*.ipy'
      ipIpy=1
      iwarnISt = 0
      iout=0
      
      ioutA=0
      ioutEf=0 
      ioutSM=0
c
      nprint=0
      ireset=0  
      
      small=0.01
      cCallBy='GetIpy4     '

      
      if(iout.gt.0) write(nlog,540)

c
c ---------------------------------------------------------	      
c rrb 01/09/25; Set *.ipy or *.tsp data code for setting defaults 
c               in bomsec
      do nd=1,numdiv
        itspd(nd)=0
        AreaSF(nd)=0.0
	      AreaSS(nd)=0.0
	      AreaGF(nd)=0.0
	      AreaGS(nd)=0.0
	      effC(nd)=0.0
	      effF(nd)=0.0
	      effS(nd)=0.0
      enddo
      
      do nw=1,numdivw
        itspw(nw)=0
	      AreaGFw(nw)=0.0
	      AreaGSw(nw)=0.0
	      effCw(nw)=0.0
	      effFw(nw)=0.0
	      effSw(nw)=0.0
      enddo
c
c _________________________________________________________
c
c		Step 2 Read data
c               Read numdiv+numdivw since file has diversions and wells
c
      if(ichk.eq.4 .or. iout.eq.1) then
         write(nlog,*)'  GetIpy4; reading *.ipy ' ,
     1              numdiv, numdivw, numdiv+numdivw,  maxdivw
      endif

c jhb 2014/06/26 make the do loop limit arbitrarily large (5000)
c                so it always works no matter how many records are
c                in the IPY, STR, DDC files.  This is OK, because
c                there is code below to jump out of this loop when the
c                last record is read (the year changes)
C          do nd = 1, numdiv+numdivw
	      do nd = 1, 5000
	        ifound=0
          read(10,954,end=1700,err=928) idyr,cistat, (x(i), i=1,3),
     1	      AreaSF1, AreaSS1, AreaGF1, AreaGS1, Qmax, gwmode1, Atot
     
          AreaSF1=amax1(AreaSF1, 0.0)
          AreaSS1=amax1(AreaSS1, 0.0)
          AreaGF1=amax1(AreaGF1, 0.0) 
          AreaGS1=amax1(AreaGS1, 0.0)
          Qmax=amax1(qmax, 0.0) 
          gwmode1=amax1(gwmode1, 0.0) 
          Atot=amax1(Atot, 0.0)
          
          if(iout.eq.1) write(nlog,*) ' '
          if(iout.eq.1) write(nlog,*) '  GetIpy4; nd ', nd
          if(iout.eq.1) write(nlog,954) idyr, cistat, (x(i), i=1,3),
     1	      AreaSF1, AreaSS1, AreaGF1, AreaGS1, Qmax, gwmode1, Atot           
c
c rrb 2006/03/20; Adjust character string to left     
          cistat=adjustl(cistat)
c
c ---------------------------------------------------------	      
c               Check year (OK if more data than expected)
	       if(idyr.ne.iyr) then  
	         backspace(10)
c          write(nlog,1632)
	         goto 1700
	       endif
c
c _________________________________________________________
c
c		Step 3; Find diversion if any
c
	       call stafind(nlog,1,3,numdiv,ix,cistat,cdivid,cCallBy)
	       ndx=ix             
	       if(iout.eq.1) write(nlog,*) '  GetIpy4; ix', ix
c
c
c _________________________________________________________
c
c		Step 4; Set Diversion data
c		SW only or SW and GW conjunctive
c               Set transit efficiency (effmaxt), ditch on-farm 
c               efficiency (effmaxd) and system efficiency (effmax)
	       if(ix.ne.0) then
	         AreaSF(ix)=AreaSF1
	         AreaSS(ix)=AreaSS1
	         AreaGF(ix)=AreaGF1
	         AreaGS(ix)=AreaGS1
c       
c		     RESET total area to data in *.ipy
                 Area(ix)=AreaSF1+AreaSS1+AreaGF1+AreaGS1	    
	         ifound=1
	         nw=idivco2(ix)
c
c rrb 01/09/25; Set *.tsp (*.ipy) data code for setting defaults
c               in bomsec
	         itspd(ix)=1
	         if(nw.gt.0) itspw(nw)=1
           
	         effmaxt(ix)=x(1)*100.0
	         effmaxd(ix)=x(2)*100.0
	         effmax(ix)=x(1)*x(2)*100.0
c          write(nlog,*) '  GetIpy4; ix, effmax(ix)', ix, effmax(ix)
c            
c rrb 01/04/25; Miscellaneous checks related to efficiency
	         c1 = effmaxt(ix)-100.0
	         c2 = effmaxd(ix)-100.0
	         if((effmax(ix).lt.small) .or. 
     1              (c1.gt.small) .or. (c2.gt.small)) then
	           write(nlog,1318) cdivid(ix), effmaxt(ix), effmaxd(ix) 
	           goto 9999
	         endif                    
c
c ---------------------------------------------------------	      

c rrb; 01/02/01; Insure efficiency is set properly if a transmountain
c                diversion (irturn=4)
	         if(irturn(ix).eq.4) then 
	           do i=1,12
		           c = abs(effmax(ix)-100.0)
		           if(c .gt. small) then
		             write(nlog,547) cdivid(ix)
c                      effmax(ix) = 100.0
		            goto 9999
		           endif
	           end do
	         endif
c
c	      
c ---------------------------------------------------------	      
c
c               Set Annual total acres
	        if(itsfile.eq.6 .or. itsfile.eq.10) then
	          area(ix)=atot
c
c rrb 01/02/08; Adjust SoilM capacity based on new acreage
c               Note Bomsec handles any loss of soil moisture               
		        awcr(ix)=awcr1(ix) * area(ix) * soild
		
c
c ---------------------------------------------------------	      
c rrb 2006/09/18; Revise soil moisture to let area in *.ipy control
           if(iyr.eq.iystr) then
             soils(ix)=awcr(ix) * 0.5
c        
	           if(ioutSM.eq.3) write(nlog,1324)
     1         'Diversion   ', iyr, ix, cdivid(ix), 
     1          area(ix), awcr(ix), soils(ix)
             endif  
	         endif        
c
c	      
c ---------------------------------------------------------	      
c          
c	 	Set total SW and GW
c		Change AreaSF, etc to a fraction
            Asw=AreaSF1+AreaSS1
            Agw=AreaGF1+AreaGS1
            
            
            if(Atot.gt.0) then
              AreaGS(ix)=AreaGS(ix)/Atot
              AreaSS(ix)=AreaSS(ix)/Atot
            
              AreaGF(ix)=AreaGF(ix)/Atot
              AreaSF(ix)=AreaSF(ix)/Atot
              AtotX=1.0
           
            else
              AreaGS(ix)=0.0
              AreaSS(ix)=0.0
            
              AreaGF(ix)=0.0
              AreaSF(ix)=0.0 
              AtotX=0.0           
            endif
c	      
c ---------------------------------------------------------	      
c            
            if(ioutA.gt.0) then 
              if(nprint.eq.0) write(nlog,1680)
              nprint=nprint+1
              rec12='Diversion   '
                       
              write(nlog,1682) nprint, nd, ix, iyr, cistat, rec12,
     1          Asw, Agw, Atot, 
     1          AreaSF(ix), AreaSS(ix), AreaGF(ix), AreaGS(ix),
     1          (AreaSF(ix)+ AreaSS(ix)+ AreaGF(ix)+ AreaGS(ix)),
     1          (AtotX-AreaSF(ix)-AreaSS(ix)-AreaGF(ix)-AreaGS(ix))
            endif
c	      
c ---------------------------------------------------------	      
c		Set Efficiency and maximum efficiency data	    
            effC(ix)=x(1)
            effF(ix)=x(2)
            effS(ix)=x(3)
              
            eff1=effmax(ix)
            effN1=(AreaSF(ix)*effF(ix)+AreaSS(ix)*effS(ix)+
     1        AreaGF(ix)*effF(ix)+AreaGS(ix)*effS(ix))*100.0
            effN2=effN1*effC(ix)
c        
c	  	Do not reset if equal to zero (area = 0)              
            if(effN2.gt.small) effmax(ix)=effN2
            
            if(ioutEf.gt.0) then 
              if(nd.eq.1) write(nlog,1684)
              rec12='Diversion   '                         
              write(nlog,1686) nd, ix, iyr, cistat, rec12, 
     1          Eff1, effN1, effN2, effmax(ix), Effmax(ix)-Eff1
            endif 
c              
c ---------------------------------------------------------
c		Endif processing a diversion (ix.ne.0)            
	        endif

c
c _________________________________________________________
c
c		Step 5; Set Well Data
c               Find well information associated with this data 
c
c rrb 01/03/08; Correction
          if(iout.eq.1) write(nlog,*)'  GetIpy4; Process wells A',ix
	        if(iwell.ge.1) then
	         call stafind(nlog,1,6,numdivw,ix,cistat,cdividw,cCallBy)
	         
          if(iout.eq.1) write(nlog,*)'  GetIpy4; Process wells B',ix
c	      
c ---------------------------------------------------------	      
c
c rrb 01/02/23; Check if data is potentially provided for both a 
c               Diversion and Well when really the same structure  
c               Note code expects data under SW id as the prefernce
	         if(ix.gt.0) then
	           ifound=ifound+2  
c
c rrb 01/09/25; Set *.tsp (*.ipy) data code for setting defaults 
c               in bomsec
	           itspw(ix)=1
c
c	      
c ---------------------------------------------------------	      
c               Warn if data is provided for both a diversion and 
c               well when really same structure
	           ndx=idivcow2(ix)      
	           if(ndx.gt.0) then
		            if(cdividw(ix).ne.cdivid(ndx)) then
		              write(nlog,545) cdividw(ix), cdivid(ndx)
		              goto 9999
		            endif
	           endif
	         else
c
c	      
c ---------------------------------------------------------	      
c rrb 01/04/23; Warning if not a diversion or a well
c            ix=idivco2(ndx)
             if(iout.eq.1) write(nlog,*) '  Getipy4; ifound', ifound
             
	           if(ifound.eq.0) then
               if(ipIpy.eq.0) then
                 rec40='Annual Time Series File (*.ipy or *.tsp)'		    
                 write(nlog,1281) iyr, rec40
                 ipIpy=1                  
                endif                  		  
	           
  	            iwarnISt=iwarnISt+1
  	           	if(iwarnISt.eq.1) write(nchk,1315) 
  	             	write(nchk,1316) iwarnISt, cistat, iyr
 	             else
 	             	ix=idivco2(ndx)
 	             endif
 	           endif
c
c	      
c ---------------------------------------------------------	      
c               Well Only
c               Finally set area served by GW for year (areawa),
c               annual GW capacity (divcapwa), flood efficiency
c               (effmaxw), sprinkler efficiency (effmaxs), and
c               sprinkler switch (igwmode)
c               Note most are OK to set now and let control switch 
c               take precedence later
      if(iout.eq.1) write(nlog,*) '  GetIpy4; Well only ix', ix
	    if(ix.ne.0) then
c        ifound=1
c
c	      
c ---------------------------------------------------------	      
c               Warn and reset if area sprinklers > area wells
c rrb 2006/08/31; Do not reset based on CU approach
c
c rrb 2007/09/04; Allow 4 land use types to be read
        areaw(ix)=areaGF1 + AreaGS1
        areawa(ix) = AreaGF1 + AreaGS1
        areasp(ix) = AreaSS1 + AreaGS1
c
c rrb 2007/11/16; Allow ireset to control acreage reset		      
	      if(ireset.eq.1) then
	        if((areasp(ix)-areawa(ix)) .gt. small) then
	          if(iwarnISp.eq.0) then
cr                  write(nlog,1281) iyr, rec40
	            write(nchk,1319)
	          endif  
	       
            if(ipIpy.eq.0) then
               rec40='Annual Time Series File (*.ipy or *.tsp)'
	             write(nlog,1281) iyr, rec40
              ipIpy=1		    
            endif  
	       
	       
	          iwarnISp=iwarnISp+1
	          write(nchk,1322) iwarnISp, idyr, cdividw(ix), 
     1              areasp(ix), areawa(ix), areasp(ix)-areawa(ix),
     1              areawa(ix), areaw(ix), areawa(ix)-areaw(ix)                
               
	          areasp(ix) = areawa(ix)
	        endif 
c
c         Endif for ireset
	      endif
	      
c	      
c ---------------------------------------------------------	      
c
c rrb 2006/08/29; New CU Approach, set efficiencies
        effCw(ix)=x(1)
        effFw(ix)=x(2)
        effSw(ix)=x(3)
              
c
c rrb 2006/08/29; New CU Approach, set areas
c		  Note put sprinklers on GW preferentially
c		            
c       Agw=x(4)
c       Asp=x(5)
c       Atot=x(8)
             
        if(Atot.gt.0) then
          AreaGSw(ix)=AreaGS1              
          AreaGFw(ix)=AreaGF1
c        
c		  Change to fraction     
          AreaGSw(ix)=AreaGSw(ix)/Atot              
          AreaGFw(ix)=AreaGFw(ix)/Atot
          AtotX=1.0           
        else
          AreaGSw(ix)=0.0              
          AreaGFw(ix)=0.0
          AtotX=0.0           
        endif
        
        if(ioutA.gt.0) then 
          if(nprint.eq.0) write(nlog,1680)
               
               ndx=idivcow2(ix)      
	        if(ndx.gt.0) then
	          rec12='Div & Well  '
  	        else
	          rec12='Well Only   '
	        endif  
	         
	        nprint=nprint+1                       
            write(nlog,1682) nprint, nd, ix, iyr, cistat, rec12,
     1        Asw, Agw, Atot, 
     1        0.0, 0.0, AreaGFw(ix), AreaGSw(ix),
     1        (AreaGFw(ix)+ AreaGSw(ix)),
     1        (AtotX-AreaGFw(ix)-AreaGSw(ix))
        endif
c	      
c ---------------------------------------------------------	      
c
c               Set Annual GW capacity (divcapw and divcapwa)
c               using a 30 day month (conservative)
	      if(itsfile.eq.2 .or. itsfile.eq.10) then
c
c rrb 2007/10/03; Revise to match StateDMI	      
c       	divcapwa(ix) = qmax/1.9835/30.0
          divcapwa(ix) = qmax/1.9835/30.4
          divcapw(ix)=divcapwa(ix) 
	      endif
c
c               Isprink is a global control that helps
c               with baseflow calculations
c               GW Mode (igwmode)    0=no Sprinkler, 
c                 1=Maximum Supply, 2= Mutual Supply
        if((isprink.ge.1) .and. 
     1   (itsfile.eq.5 .or. itsfile.eq.10)) then
         igwmode(ix) = ifix(gwmode1)
        else
          igwmode(ix) = 0
        endif
c
c               Set annual flood (effmaxw) and sprinker (effmaxs)
c               efficency
	      effmaxw(ix)=x(2)*100.0
	      effmaxs(ix)=x(3)*100.0

c
c rrb; 01/02/01; Insure efficiency is set properly if a transmountain
c                well (irturnw=4)
	      if(irturnw(ix).eq.4) then 
		      do i=1,12
		        c = abs(effmaxw(ix)-100.0) + abs(effmaxs(ix)-100.0)
		        if(c .gt. small) then
		          write(nlog,547) cdividw(ix)
c                         effmaxw(ix) = 100.0
c                         effmaxs(ix) = 100.0
		          goto 9999
		        endif
		      end do
	      endif
c
c ---------------------------------------------------------
c               Set Annual total acres
	      if(itsfile.eq.6 .or. itsfile.eq.10) then
cx		    areaw(ix)=areaGF1+areaGS1
c
c               Warn if total area exceeds GW area for a well
c
c rrb 2006/07/14; Correction
cr		    if(ifound.eq.2 .and. areawa(ix).lt.areaw(ix)) then
	  	    if(ifound.eq.2 .and. areawa(ix).gt.areaw(ix)) then
          
          if(iwarnISp.eq.0) then
c	  	      rec40='Annual Time Series File'
c                       write(nlog,1281) iyr, rec40
	  	      write(nchk,1319)
	  	    endif  
	  	   
          if(IpIpy.eq.0) then
            rec40='Annual Time Series File (*.ipy or *.tsp)'		                  
	  	      write(nlog,1281) iyr, rec40
            IpIpy=1		    
          endif  
		     
         
          iwarnISp=iwarnISp+1
		      write(nchk,1322) iwarnISp, idyr, cdividw(ix), 
     1      areasp(ix), areawa(ix), areasp(ix)-areawa(ix),
     1      areawa(ix), areaw(ix), areawa(ix)-areaw(ix)                
          
		      areawa(ix)=areaw(ix)
		    endif  
c
c ---------------------------------------------------------
c rrb 01/02/08; Set Soil capacity based on new acreage
c               Note Bomsec handles any loss of soil moisture               
		    awcrw(ix)=awcrw1(ix) * areaw(ix) * soild
c

c ---------------------------------------------------------	      
c rrb 2006/09/18; Revise soil moisture to let area in *.ipy control
        if(iyr.eq.iystr) then
          soilsw(ix)=awcrw(ix) * 0.5
c       
	        if(ioutSM.eq.3) write(nlog,1324)
     1     'Well        ', iyr, ix, cdividw(ix), 
     1      areaw(ix), awcrw(ix), soilsw(ix)
          endif  
		    
	      endif
         
c          write(nlog,*) '  GetIpy4;',ix, effmaxw(ix), effmaxs(ix)
	        c1 = effmaxw(ix)-100.0
	        c2 = effmaxs(ix)-100.0
	        if((effmaxw(ix).lt.small).or.(effmaxs(ix).lt.small).or.
     1          (c1.gt.small) .or. (c2.gt.small)) then
	          write(nlog,1318) cdividw(ix), effmaxw(ix), effmaxs(ix) 
	          goto 9999
	          endif
	        endif

c
c         Endif well only
	      endif
	      
c
c _________________________________________________________
c
c		Step 6; End diversion + well loop
        if(iout.eq.1) write(nlog,*)'  GetIpy4; end do nd'   , nd  
	    end do 
c
c _________________________________________________________
c		  Return
	
 1700 if(iout.eq.1) write(nlog,*) '  GetIpy4; returning'
      return
c
c _________________________________________________________
c               Formats
 540  format(/,72('_'),/,
     1 '  GetIpy4; Detailed Results from reading *.ipy file with',/
     1 '           SW-Flood, SW-Sprinkler, GW-Flood, and GW-Sprinkler')

 545  format(/,
     1  72('_'),//  
     1 '  GetIpy4; Problem Annual time series data (*.ipy or *.tsp)',/
     1 '          Well ID = ', a12,' is tied to Diversion ID = ',a12,/
     1 '          To do; provide data in *.ipy or *.tsp once under', 
     1          ' diversion ID')      

 547  format(/,
     1  72('_'),//  
     1 '  GetIpy4; Problem for Div or Well station = ', a12,/
     1 '          The type = 4 which indicates transmountain, but',/
     1 '          the max efficiency data (*.ipy or *.tsp) ',/
     1 '          is not 100%.',/
     1 '          To do: Revise type or max. efficiency')

c 954  format(i4, 1x, a12, 3f6.0, 2f8.0, f12.0, f3.0, f8.0)
  954  format(i4, 1x, a12, 3f6.0, 4f8.0, f12.0, f3.0, f8.0)
     
 1281  FORMAT(/,72('_'),/
     1  '  GetIpy4; Warning See *.chk for details in year',i5,/
     1  '          Regarding file: ',a40,/
     1  '          Note only first occurance (year) is printed')

 1315 FORMAT(/,
     1  72('_'),//  
     1 '  GetIpy4; Warning. ',/
     1 10x, 'The following Structures are in the annual time series',/ 
     1 10x, 'file (*.ipy or *.tsp) but are not in a diversion',/
     1 10x, '(*.dds) or a well (*.wes) station file. Since it ',/
     1 10x, 'cannot be tied to a diversion or well this data is ',/
     1      'not used',/
     1   '    # ID            Year',/
     1   ' ____ ____________ _____')
     
 1316 format(i5, 1x, a12, 1x, i5)
       
 1318 FORMAT(/,
     1  72('_'),//  
     1 '  GetIpy4; Problem',
     1  ' Structure ID ',a12,' in *.ipy or *.tsp has conveyance,',
     1  ' flood or',/ 
     1 10x, ' sprinker efficiency less than 0 or greater than 1.',/ 
     1 10x, ' To do; Revise efficiency data.', 20f8.2)

 1319 FORMAT(/,
     1  72('_'),//  
     1 '  GetIpy4; Warning in *.ipy or *.tsp',/
     1 '          Sprinker area > ground water area, ',
     1           ' setting sprinkler = GW area or',/
     1 '          GW area > Total Area ',
     1           ' setting GW area = Total area',//
     1  '    # Year ID             Spr Area   GW Area     Delta',
     1                          '   GW Area  Tot Area     Delta'/
     1  ' ____ ____ _____________ _________ _________ _________',
     1                          ' _________ _________ _________')     
     
 1322 format(i5, i5, 1x, a12,1x 20f10.2)
 
 1324 format(2x, a12, 1x, i5, i5, 1x, a12,1x, 20f10.2)
     
 1632 format(/,
     1  72('_'),//  
     1   '  GetIpy4; Warning the annual time series file ',
     1   '(*.ipy or *.tsp)',/
     1   '    has more data then wells. Moving on')
c     
 1680  format(/, 72('_'),/
     1 '  GetIpy4; Area Report for 4 Supply-Irrigation Approach',
     1 ' (ipyX=4)', /  
     1 '    #   nd   ix  iyr ID           Type        ',
     1 '     Asw     Agw    Atot',
     1 '  AreaSF  AreaSS  AreaGF  AreaGS     Sum   Delta',/
     1 ' ____ ____ ____ ____ ____________ ____________',
     1 ' _______ _______ _______',
     1 ' _______ _______ _______ _______ _______ _______')
c     
 1682  format(4i5, 1x,a12, 1x,a12, 3f8.0, 20f8.3)
c     
 1684  format(/, 
     1 '  GetIpy4; Efficiency Report for 4 Supply-Irrigation Approach',
     1 ' (ipyX=4)', /  
     1 '          Note Diversions only, Wells do not need adjustment',/
     1 '   nd   ix  iyr ID           Type        ',
     1 '    Eff1   effN1   effN2  effmax   Delta',/
     1 ' ____ ____ ____ ____________ ____________',
     1 ' _______ _______ _______ _______ _______')
     
c     
 1686  format(3i5, 1x,a12, 1x,a12, 20f8.3)
c
c _________________________________________________________
c               Error Processing
c
c
  928 write(nlog,929) iin2, filena
  929 format(/
     1 '  GetIpy4; Problem reading file # ', i4,/,
     1 '          File name: ', a256)
 
 9999 write(6,*)  '  Stopped in GetIpy4, see log file (*.log)'
      write(nlog,*) ' Stopped in GetIpy4'
      write(6,*) 'Stop 1'
      call flush (6)
      call exit(1)



      stop 
      end
