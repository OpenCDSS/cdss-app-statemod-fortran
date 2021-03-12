c datinp - reads in constant (non time series) data
c_________________________________________________________________NoticeStart_
c StateMod Water Allocation Model
c StateMod is a part of Colorado's Decision Support Systems (CDSS)
c Copyright (C) 1994-2021 Colorado Department of Natural Resources
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
c
c	Update History
c

C     Last change:  RRB  16 Oct 99   12:18 pm
c
C
      SUBROUTINE DATINP(IIN, inx, numstax)
c
c _________________________________________________________
c	Program Description
c
c
c       Datinp; it reads in constant (non time series) data
c
c _________________________________________________________
c
c               Documentation
c
c               iin  =   response file #
c               inx  =   switch
c                        0-read all input data
c                        1-read only *.ctl file
c
c               ichk =  0 do not print detail;
c                       -n do at river id ichk,
c                       + print detailed information (datinp)
c			                  1 Network and downstream data (datinp)
c                       4 Calls from Execut (execut)
c                       5 Demand & sprinkler (demand)
c                       6 Daily data (daydist)
c                       7 Return flow data (closs via mdainp or datest)
c                       8 Print detailed daily baseflow data to *.xtp
c                       9 Reoperation information from Execut
c		                   10 Details on reading operating right data 
c		                   11 Details regarding evaporation calculations
c                      20 Override daily ID data for testing (datinp)
c		                   21 Print binary header data
c                      24 Detailed results of opr rule 24 (downstream
c                         call)
c                      25 Limit output to daily baseflow (*.xbx) to 
c		                      stream ID in variable ccall		
c                      30 Do not print any daily binary results 
c                         see outmon call dayoutr.
c
c                      90 Return flow detailed water use data (return)
c
c                      91 Print detailed demand data (Bomsec) and well
c                           water right info (welrig)
c                      92 Print detailed soil moisture data (soilm)
c                      99 Enter water right id for call information
c                     100+n Print water right n (see *.xwr for a number)  
c
c
c _________________________________________________________ 
c rrb 6/15/95 Demand Control 
c               icondem= 0 constrain demand to water rights
c                        1 no constraint and for D&W do not add ddm 
c                          and wem
c                        2 no constraint and for D&W add ddm and wem
c                        3 no constraint and for D&W expect demands  
c                          in *.ddm
c                        4 same as 3 but do not limit demand from SW to 
c                          IWR supplied by other sources (e.g. wells)
c                        5 same as 4 but demand is water right if
c                          there is a non zero demand
c
c rrb 00/06/19; Add demand control as a global option (default is 1)
c _________________________________________________________
c               Instream Flow Switch
c               ireach  = 0 No instream flow reaches
c                         1 with reaches
c                         2 no reaches but open monthly *.ifm file
c                         3 with reaches and open monthly *.ifm file

c
c		nrgfor   # of Rio Grande forecast statins to read 
c                          in Mdainp
c		nisfinA  # of annual ISF values stations 
c                          to read in Mdainp
c		nisfinM  # of monthly ISF values stations 
c                          to read in Mdainp
c _________________________________________________________
c               Reoperation control
c               ireopx  = 0 do reoperate, 
c                         1 do not, 
c                         -n reoperate when > n  
c
c
c _________________________________________________________
c               Wells - Maximum recharge (payback) rate
c
c               gwmaxrc = 0.0 or greater
c
c _________________________________________________________
c
c               Update History
c
c rrb 05/08/22; Remove reservoir read to getres.for
c rrb 04/09/07; Revise diversions to allow multiple owners
c		ndown. Note multiple users are still not
c		allowed.
c rrb 03/08/18; Revise to allow random file read
c
c rrb 99/09/15; Revised instream flows to allow monthly data
c rrb 00/05/30; Revised to allow Irrigation Water Req. data for
c               demand information
c rrb 00/11/10; Revised to read itsfile; annual time series code
c rrb 00/12/04; Revised to add ieffmax; variable efficiency code
c               (0=no, 1=yes)
c
c
c _________________________________________________________
c	Dimensions
c

      include 'common.inc'
c
      DIMENSION ITEMP(numstax)

      dimension mthd(12), xmon(12)
C
      character ch3*3, ch4*4, blank*12, crtnid*12, xmon*4,
     1          recin*256, cgoto2*12, cx*12, rec4*4, rec24*24,
     1          rec1*1, rec12*12, 
     1          rec32*32, rec132*132, rec80*80

      write(6,*) ' Subroutine Datinp'
c
c__________________________________________________________
c               Initialize
c
c		Details:
c		  ioutC = Control Data (*.ctl)
c     ioutS = River Station (*.ris)		
c     ioutI = Instream Flow (*.ifs)
c     ioutN = 1 Network Data (*.rin)
c		        = 2 Network plus idncod and ndnnod
      iout=0
      ioutS=0
      ioutN=0
      ioutC=0
      ioutI=0
      
      small = 0.001
      small2= 0.002
      smalln = -1.0*small
      smalln = smalln

      blank = '            '                       
c ---------------------------------------------------------
c               Initialize return pointer here so
c		  resevoir returns can be read in GetRes
c		  as well as Getdiv and Getwel.     

      numrtn=0
      NSTRTN=0
      numdiv=0
      numres=0
      numifr=0
      numdivw=0
      numsta=0
      nplan=0
      nx=0      
      
      do is=1,maxsta
        irnord(is)=0
      end do  
c
c rrb 2006/11/08;  Move initialization to top for new control file read
c		   (e.g. getctlC)      
      ireach = 0 
      icondem = 0
      iopout = 0
      ioprtn = 1
      nwrite = 0
      nrvrep = 0
      ibrrep = 999999
      ireopx = 0        
      icall=0

      iwell = 0
      gwmaxrc = 0.0
      isjrip = 0
      itsfile = 0
      ieffmax = 0
      isprink = 0
      soild = 0.0
      isigfig = 0
      interv = -1
 
      factor=1.9835     
      rfacto=1.9835
      dfacto=1.9835
      ffacto=1.0
      cfacto=1.0
      efacto=1.0
      pfacto=1.0
      wfacto=1.0
      
      rfacto1=rfacto
      dfacto1=dfacto
      ffacto1=ffacto
      cfacto1=cfacto
      efacto1=efacto
      pfacto1=pfacto
      wfacto1=dfacto
c
c _________________________________________________________
c               Read Daily Switch               
      iday=0
      idaydem=0
      
c
c _________________________________________________________
c
c               Read Control Data (*.ctl)
c
  110 write(nlog,101)
      write(6,101)
  101 format(/,72('_'),/
     1 '  Datinp; Control File (*.ctl) ')
      call flush(6)
      
c
c rrb 2008/02/22; Read new or old response file formats (infile)
c		  add path (fpath1), open file (filena), get
c		  number of stations (numopr, if 0) and get 
c		  version number (ioprX)
c		  inf = filename
c		  nf = file #
      inf=2
      nf=1
      call GetFile(
     1   nlog, nchk, iin, infile, maxfile, maxfn, 
     1   nf, ictlX, numCtl, fileType(inf), fileSuf(inf), 
     1   fileName(inf), fpath1, filena)
c
c _________________________________________________________
c
c		Determine file type (old or new)      
      itestCtl=0
      READ(1,'(a12)',end=926,err=928) rec12
      rec12=adjustl(rec12)
      if(rec12(1:5) .eq.'Title') itestCtl=1
      backspace(1)
c
c _________________________________________________________
c               Read Control File
c		iok=0 Not OK if not found
c		iok=1 OK if not found
c		
      if(itestCtl.eq.1) then
        iok=0
        call GetCtlC(nlog, 1, iok,3,'Title_1                  ',  
     1    i,  r, cx, headin1(1))
     
        call GetCtlC(nlog, 1, iok,3,'Title_2                  ',  
     1    i,  r, cx, headin1(2))
     
        call GetCtlC(nlog, 1, iok,0, 'Starting_Year           ',  
     1    iystr,  r, cx, rec80)
        
        call GetCtlC(nlog, 1, iok,0, 'Ending_Year             ',      
     1    iyend,  r, cx, rec80)
c        
        call getctlC(nlog, 1, iok,2, 'Time_Step               ',       
     1    iday, r, cx, rec80)
     
        call GetCtlC(nlog, 1, iok,2, 'Year_Type               ',      
     1      i, r, cx, rec80)
        cyr1=cx(1:5)        
        cyr1=adjustr(cyr1)
          
        call GetCtlC(nlog, 1, iok,2, 'Output_Units            ',     
     1    iresop, r, cx, rec80)
        
        if(iok.eq.-1) goto 120
        
        call GetCtlC(nlog, 1, iok,2, 'Streamflow_Type         ',  
     1    iopflo, r, cx, rec80)
c
c		Read ok if not found
c		iok=0 Not OK if not found
c		iok=1 OK if not found
        iok=1        
        call getctlC(nlog, 1, iok,2, 'Demand_Type             ',      
     1    icondem, r, cx, rec80)
        call getctlC(nlog, 1, iok,0, 'Reoperation_Control     ',
     1    ireopx, r, cx, rec80)
c        
        call getctlC(nlog, 1, iok,0, 'Detailed_Output         ',  
     1     ichk, r, cx, rec80)
     
        call getctlC(nlog, 1, iok,0, 'Call_Switch             ',       
     1    icall, r, cx, rec80)
        call getctlC(nlog, 1, iok,2, 'Call_ID                 ',           
     1    i, r, ccall, rec80)
        ccall=adjustl(ccall)
          
        if(iok.eq.-1) goto 120       
c          
        call getctlC(nlog, 1, iok,0, 'Annual_Time_Series      ', 
     1    itsfile, r, cx, rec80)
        call getctlC(nlog, 1, iok,0, 'Variable_Efficiency     ',
     1    ieffmax,r,cx, rec80)
        call getctlC(nlog, 1, iok,0, 'Sprinkler_Approach      ', 
     1    isprink,r,cx, rec80)              
        call getctlC(nlog, 1, iok,1, 'Soil_Moisture_Depth     ',
     1    i, soild, cx, rec80)
        call getctlC(nlog, 1, iok,0, 'Significant_Figures     ',
     1    isigfig,r,cx, rec80)
          
        if(iok.eq.-1) goto 120       
c
c		Read ok if not found
c		iok=0 Not OK if not found
c		iok=1 OK if not found
	      iok=1        
        
        call getctlC(nlog, 1, iok,0, 'Instream_Flow_Type      ',
     1    ireach, r, cx, rec80)
        call getctlC(nlog, 1, iok,0, 'Well_Control            ',  
     1    iwell, r, cx, rec80)
        call getctlC(nlog, 1, iok,1, 'Maximum_Stream_Loss     ',
     1    i,gwmaxrc,cx, rec80)      
        call getctlC(nlog, 1, iok,0, 'San_Juan_RIP            ',   
     1    isjrip, r,cx, rec80)
      
      else
c
c _________________________________________________________
c               Old Control File Format
C
c _________________________________________________________
c               Read Headings from control file
        DO j=1,2
          READ(1,950,end=926,err=928) HEADIN1(J)
          write(nlog,950) HEADIN1(J)
        end do
      
        call getctl(1, 0, 0, iystr,  r, cx)
        call getctl(1, 0, 0, iyend,  r, cx)
        call getctl(1, 0, 0, iresop, r, cx)
        call getctl(1, 0, 0, moneva, r, cx)
        call getctl(1, 0, 0, iopflo, r, cx)
c
c _________________________________________________________
c               Read precipitation and delay controls

        call getctl(1, 0, 0, numpre, r, cx)
        call getctl(1, 0, 0, numeva, r, cx)
        call getctl(1, 0, 0, interv, r, cx)    
c
c _________________________________________________________
c               Read Factors
        call getctl(1, 0, 1, i, factor, cx)
        call getctl(1, 0, 1, i, rfacto, cx)
        call getctl(1, 0, 1, i, dfacto, cx)
        call getctl(1, 0, 1, i, ffacto, cx)
        call getctl(1, 0, 1, i, cfacto, cx)
        call getctl(1, 0, 1, i, efacto, cx)
        call getctl(1, 0, 1, i, pfacto, cx)
c 
c               Set Year and month type
        call getctl(1, 0, 2, i, r, cx)
          cyr1=cx(1:5)
c
c		Demand Type          
        call getctl(1, 1, 0, icondem, r, cx)
c
c		Detailed Check
        call getctl(1, 1, 0, ichk, r, cx)
c
c		Reoperation control        
        call getctl(1, 1, 0, ireopx, r, cx)
c       write(nlog,*) ' DAtainp; ireopx = ', ireopx
c
c		Instream Flow Reach        
        call getctl(1, 1, 0, ireach, r, cx)
c
c		Call Data        
        call getctl(1, 1, 0, icall, r, cx)
        call getctl(1, 1, 2, i, r, ccall)
          ccall=adjustl(ccall)       
c
c		Daily Switch          
        call getctl(1, 1, 0, iday, r, cx)
c
c		Well Switch
        call getctl(1, 1, 0, iwell, r, cx)
c
c		Max Recharge rate
        call getctl(1, 1, 1, i, gwmaxrc, cx)      
c
c		 San Juan RIP control
        call getctl(1, 1, 0, isjrip, r, cx)      
c         write(nlog,*) ' Datinp; '
c
c		Annual Time Series Control
        call getctl(1, 1, 0, itsfile, r, cx)
c
c		Variable Efficiency        
        call getctl(1, 1, 0, ieffmax, r , cx)
c
c		Sprinkler Control        
        call getctl(1, 1, 0, isprink, r, cx)
c
c		Soil Moisture        
        call getctl(1, 1, 1, i, soild, cx)
c
c		Significant Figure        
        call getctl(1, 1, 0, isigfig, r, cx)
      
        rfacto1=rfacto
        dfacto1=dfacto
        ffacto1=ffacto
        cfacto1=cfacto
        efacto1=efacto
        pfacto1=pfacto
        wfacto1=dfacto
      endif
      
 120  continue     
c
c rrb 2008/09/25; Warn if iyend < iystr
      if(iyend-iystr. lt. 0) then
        write(6, 1510) iystr, iyend
        write(nlog, 1510) iystr, iyend
        goto 9999
      endif

c
c		Set constant year variables      
      call year2(nlog, mthday, xmon, xmonam, cyr1, imstr)
c
c		Set annual year variables      
      call year(iystr,iyrmo,imomo, cyr1)
c
c _________________________________________________________
c               Set Output Unit Conversions
c               CFS
c
c ---------------------------------------------------------
c               Set Output/Input Unit Conversions (e.g. for baseflow)
      cunitdX='  CFS'
      
      if(iresop.eq.1) then
c
c rrb 2006/03/03; Why not      
cr      cunitm= '(cfs)'
cr      cunitd= '(cfs)'
        cunitm= '  CFS'
        cunitd= '  CFS'
        cunitm2='  CFS'
        cunitd2='  CFS'

        ftot=1.0/12.0

        do i=1,12
          fmo(i)=1.0
          fdy(i)=1.0
          faf(i)=factor*mthday(i)
        end do
      endif
c
c               Af
      if(iresop.eq.2) then
c
c rrb 2006/03/03; Why not            
cr      cunitm=' (af)'
cr      cunitd=' (af)'
        cunitm=' ACFT'
        cunitd=' ACFT'
        
        cunitm2=' ACFT'
        cunitd2=' ACFT'

        ftot=1.0
        do i=1,12
          fmo(i)=factor*mthday(i)
          fdy(i)=factor
          faf(i)=factor*mthday(i)
        end do
      endif

      if(iresop.eq.3) then
c
c rrb 2006/03/03; Why not                  
cr      cunitm='(kaf)'
cr      cunitd='(cfs)'
        cunitm= '  KAF'
        cunitd= '  CFS'
        cunitm2='  KAF'
        cunitd2='  CFS'

        ftot=1.0
        do i=1,12
          fmo(i)=factor*mthday(i)*.001
          fdy(i)=1.0
          faf(i)=factor*mthday(i)
        end do
      endif

      if(iresop.eq.4) then
        cunitm= ' ACFT'
        cunitd= '  CFS'
        cunitm2=' ACFT'
        cunitd2='  CFS'

        ftot=1.0
        do i=1,12
          fmo(i)=factor*mthday(i)
          fdy(i)=1.0
          faf(i)=factor*mthday(i)
        end do
      endif

      if(iresop.eq.5) then
        cunitm= '  CMS'
        cunitd= '  CMS'
        cunitm2='  CMS'
        cunitd2='  CMS'
        ftot=1.0
        do i=1,12
          fmo(i)=factor
          fdy(i)=factor
          faf(i)=factor*mthday(i)
        end do
      endif
c
      idemtyp = amax0(1,icondem)
c
c _________________________________________________________
c               Check option
      ichk99=ichk
      if(ichk.eq.1) ioutN=1
c
c     write(nlog,*) ' '
      if(ireopx.gt.0)
     1  write(nlog,*)'  ** Datinp; Warning reoperation off'

 160  write(nlog,159)
 159  format(/,72('_'),/ '  Datinp; ',/)
      if(iday.eq.0)  then
        write(nlog,*)'  ** Monthly Model                            **'
      else
        write(nlog,*)'  ** Daily Model                              **'
c
c rrb 01/12/17; Daily using monthly SW demand
        if(iday.eq.2) then
          iday=1
          idaydem=1
        write(nlog,*)'     Daily SW demands use the monthly total   **'
        endif
      endif

      if(iresop.eq.1) 
     1  write(nlog,*)'  ** Output Unit: cfs                         **'
      if(iresop.eq.2) 
     1  write(nlog,*)'  ** Output Unit: af                          **'
      if(iresop.eq.3) 
     1  write(nlog,*)'  ** Output Unit: kaf                         **'
      if(iresop.eq.4)
     1  write(nlog,*)'  ** Output Unit: cfs daily & af monthly      **'
      if(iresop.eq.5) 
     1  write(nlog,*)'  ** Output Unit: cms                         **'
        
c
c _________________________________________________________
c               Well Swicth

 162  if(iwell.le.0)  then                 
        if(iwell.eq.0) 
     1  write(nlog,*)'  ** Wells                                Off **'
        if(iwell.lt.0)
     1  write(nlog,*)'  ** Wells                            Skipped **'

        if(idemtyp.ge.6) then
         write(nlog,15) idemtyp
         goto 9999
        endif
      endif

      if(iwell.gt.0) then
        if(iwell.eq.1) then
        write(nlog,*)'  ** Wells                                 On **'
        write(nlog,*)'  ** Maximum Recharge (1)                 Off **'
        endif
        if(iwell.eq.2) then
        write(nlog,*)'  ** Wells                                 On **'
        write(nlog,*)'  ** Maximum Constant Recharge (2)         On **'

        endif
        if(iwell.eq.3) then     
        write(nlog,*)'  ** Wells                                 On **'
        write(nlog,*)'  ** Variable Maximum Recharge (3)         On **'

        endif

        if(idemtyp.eq.1) then
         write(nlog,10)
 10      format(     
     1     '   ** Demand type = 1 (Historical Approach)    **',/ 
     1     '      For D&W structures demands not added      ',/
     1     '      (Diversion are in *.ddm, wells in *.wem)  ')
        endif

        if(idemtyp.eq.2) then
         write(nlog,11) 
 11      format(     
     1     '   ** Demand type = 2 (Historical Sum Approach)**',/  
     1     '      fOR d&w STRUCTURES DEMANDS ARE ADDED       ',/ 
     1     '      (DEMANDS EQUAL *.DDM + *.WEM)              ')                  
        endif

        if(idemtyp.eq.3) then
         write(nlog,12)
 12      format(     
     1     '   ** Demand type = 3 (Structure Dem. Approach)**',/ 
     1     '      For D&W structures demands not added        ',/
     1     '      (Expected demands in *.ddm to be total)    ') 
        endif

        if(idemtyp.eq.4) then
         write(nlog,13)
 13      format(     
     1     '   ** Demand type = 4 (Supply Demand Approach) **',/     
     1     '      For D&W structures demands not added       ',/
     1     '      (Expected demands in *.ddm to be total)    ',/
     1     '      and SW demands are not limtied by other    ',/
     1     '      water supplies (e.g. wells)                ')
        endif

        if(idemtyp.eq.5) then
         write(nlog,14)
 14      format(     
     1     '   ** Demand type = 5 (Decreed Demand Approach) **',/     
     1     '      For D&W structures demands not added       ',/
     1     '      (Expected demands in *.ddm to be total) &  ',/
     1     '      SW demands are not limtied by other        ',/
     1     '      water supplies (e.g. wells) &              ',/
     1     '      non diversion with a nonzero CIR are set   ',/
     1     '      the sum of their surface water rights      ')
        endif

        if(idemtyp.ge.6) then
         write(nlog,15) idemtyp
 15      format(     
     1     '   ** Demand type =', i2,
     1                           '   is not supported     **')
         goto 9999
        endif
      endif
c        
c rrb 00/04/13; Allow ireach=2 or 3 to control opening of file *.ifm  
c               Note ireach gets reset to 1 or 2 in mdainp after  
c               *.ifm is opened
      if(ireach.eq.0 .or. ireach.eq.2)
     1  write(nlog,*)'  ** Instream flow with reaches           Off **'
      if(ireach.eq.1 .or. ireach.eq.3)
     1  write(nlog,*)'  ** Instream flow with reaches            On **'

      if(ichk.lt.0) iopout = ichk
      if(ichk.lt.0)  then
        write(nlog,*)'  ** Check information at river node = ',
     1    ichk,' **'
      endif

      if(icall.gt.0)  then
        write(nlog,*)'  ** Detailed call information             On **'
        write(nlog,*)'  ** Detalied call ID            ',ccall,   ' **'
      else
        write(nlog,*)'  ** Detailed call information            Off **'
      endif
c
c _________________________________________________________
c               SJRIP Sediment File Switch               
 166  if(isjrip.eq.0) then
        write(nlog,*)'  ** SJRIP Sediment file (*.sjr)          Off **'
      endif
      if(isjrip.eq.-1) then
        write(nlog,*)'  ** SJRIP Sediment file (*.sjr)      Skipped **'
      endif        
      if(isjrip.eq.1) then
        write(nlog,*)'  ** SJRIP Sediment file (*.sjr)           On **'
      endif                         
c
c _________________________________________________________
c               Annual Time series Switch               


 164  if(itsfile.eq.0) then
        write(nlog,*)'  ** Annual time series (*.tsp) file      Off **'
      endif
      if(itsfile.eq.-1) then
        write(nlog,*)'  ** Annual time series file (*.tsp)  Skipped **'
      endif        
      if(itsfile.ge.1) then
        write(nlog,*)'  ** Annual time series file (*.tsp)       On **'
      endif
c
c _________________________________________________________
c               Variable efficiency switch
c

 165  if(ieffmax.eq.0) then
        write(nlog,*)'  ** Variable efficiency                  Off **'
      endif
      if(ieffmax.eq.-1) then
        write(nlog,*)'  ** Variable efficicney              Skipped **'
      endif        
      if(ieffmax.eq.1) then
        write(nlog,*)'  ** Variable efficiency                   On **'
      endif
      if(ieffmax.eq.2) then
        write(nlog,*)'  ** Variable efficiency         Read but Off **'
      endif
c
c _________________________________________________________
c               Sprinkler switch (isprink)
c               0=No, 1=Yes Max Supply, 2=Mutual Supply
c               Note the above work in conjunction with gwmode
c               in *.tsp
c

 167  if(isprink.le.0) then
        write(nlog,*)'  ** Sprinklers                           Off **'
        isprink=0
      endif

      if(isprink.eq.1) then
        write(nlog,*)'  ** Sprinklers for Max Supply             On **'
      endif

      if(isprink.eq.2) then
        write(nlog,*)'  ** Sprinklers for Mutual Supply          On **'
      endif
c
c _________________________________________________________
c               Soil Moisture switch (isoil)
c               Soild>0 = constant depth (ft)
c
      if(ioutC.eq.1)
     1  write(nlog,*) '  Datinp; soild, small = ', soild, small

 168  isoil=int(soild)
      if(ioutC.eq.1)
     1  write(nlog,*) '  Datinp; isoil = ', isoil 

      if(soild.le.-1.0*small) isoil=-1
      if(soild.gt.small) isoil=1
      if(ioutC.eq.1)
     1  write(nlog,*) '  Datinp; isoil = ', isoil

      if(isoil.eq.0) then
        write(nlog,*)'  ** Soil Moisture                        Off **'
      endif
      if(isoil.eq.-1) then
        write(nlog,*)'  ** Soil Moisture                    Skipped **'
      endif                                                         
      if(isoil.eq.1) then
        write(nlog,*)'  ** Soil Moisture                         On **'
      endif
c
c _________________________________________________________
c               Significant figure switch (isigfig)
c               isigfig=0 no sig figs; 1= 1 sig fig
c
      write(nlog,*)'  ** Significant figures on output         ',
     1   isigfig,'**'
c
c _________________________________________________________
c
c               Echo input control
      iecho=1
      if(ichk.ge.1) iecho=1

      if(iecho.eq.1) then
        do j=1,2
          write(nlog,950) headin1(j)
        end do

        write(nlog,111) IYSTR ,IYEND ,IRESOP,MONEVA,IOPFLO,
     1    NUMPRE, NUMEVA, INTERV,
     1    factor,rfacto,dfacto,ffacto,cfacto,
     1    efacto,pfacto
 111    format(
     1    '  Datinp; ',/
     1    '    iystr      = ', i8,/            
     1    '    iyend      = ', i8,/            
     1    '    iresop     = ', i8,/            
     1    '    moneva     = ', i8,/       
     1    '    iopflo     = ', i8,/       
     1    '    numpre     = ', i8,/       
     1    '    numeva     = ', i8,/       
     1    '    interv     = ', i8,/     
     1    '    factor     = ', f8.3,/            
     1    '    rfacto (1) = ', f8.3,/            
     1    '    dfacto (1) = ', f8.3,/            
     1    '    ffacto (1) = ', f8.3,/       
     1    '    cfacto (1) = ', f8.3,/       
     1    '    efacto (1) = ', f8.3,/       
     1    '    pfacto (1) = ', f8.3,/
     1    ' (1) Note an input value of 0 is effectively equal to 1.0')

      write(nlog,112)
     1  cyr1,    icondem, ichk, ireopx, ireach,icall,ccall,
     1  iday,    idaydem, iwell, gwmaxrc, isjrip, itsfile, 
     1  ieffmax, isprink, soild, isoil, isigfig
 112  format(
     1    '  Datinp; ',/          
     1    '    cyr1       = ', a5,/
     1    '    icondem    = ', i8,/
     1    '    ichk       = ', i8,/
     1    '    ireopx     = ', i8,/
     1    '    ireach     = ', i8,/
     1    '    icall      = ', i8,/
     1    '    ccall      = ', a12,/
     1    '    iday       = ', i8,/
     1    '    idaydem    = ', i8,/
     1    '    iwell      = ', i8,/
     1    '    gwmaxrc    = ', f8.2,/
     1    '    isjrip     = ', i8,/
     1    '    itsfile    = ', i8,/
     1    '    ieffmax    = ', i8,/
     1    '    isprink    = ', i8,/
     1    '    soild      = ', f8.2,/
     1    '    isoil      = ', i8,/
     1    '    isigfig    = ', i8)
      endif
c
c _________________________________________________________
c               Check Variable Efficiency Data
c rrb 00/12/09; Warn if maximum efficiency expected but no data
      if(ieffmax.eq.1 .and. itsfile.le.0) then
        write(nlog,1430)
        goto 9999
      endif

c
c _________________________________________________________
c               Check Sprinkler Data
c               Warn if isprink=2 during simulation
c rrb 2009/04/16; Revise to allow isprink=2 to indicate a mutual
c		    approach
cx      if(isprink.eq.2 .and. (ioptio.eq.2 .or. ioptio.eq.8)) then
cx        write(nlog,1490)
cx        goto 9999
cx      endif
c
c rrb 00/12/09; Warn if maximum efficiency is on but not provided
      if(isprink.ge.1 .and. (ieffmax.le.0 .or. itsfile.le.0)) then
        write(nlog,*)'  Datinp; For the sprinkler options'
        write(nlog,*)'          Max supply (isprink=1) or '
        write(nlog,*)'          Mutual supply (isprink=2)'
        write(nlog,*)'          Max eff should be on (ieffmax =1) &'
        write(nlog,*)'          Time series file on (itsfile>0)'
        goto 9999
      endif
c
c rrb 00/12/09; Warn if maximum efficiency expected but not provided
      if(isprink.ge.1 .and. iwell.le.0)  then
        write(nlog,*)'  Datinp; For the sprinkler options'
        write(nlog,*)'          Max supply (isprink=1) or '
        write(nlog,*)'          Mutual supply (isprink=2)'
        write(nlog,*)'          Wells should be on  (iwell =1 )'
        goto 9999
      endif
c
c _________________________________________________________
c rrb 00/12/09; Check Soil Moisture Data
c 
      if(isoil.ge.1 .and. (ieffmax.ne.1 .or. itsfile.le.0)) then
        write(nlog,*)'  Datinp; For soil moisture on (isoil   =1)'
        write(nlog,*)'          Max eff should be on (ieffmax =1) &'
        write(nlog,*)'          Time series file on  (itsfile >0)'
        goto 9999
      endif
c
c _________________________________________________________
c rrb 01/04/12; Check Demand type
c 
      if(idemtyp.ge.4 .and. ieffmax.ne.1) then
        write(nlog,*)'  Datinp; For demand type = 4 or 5'
        write(nlog,*)'          Max eff should be on (ieffmax =1)'
        goto 9999
      endif

c
c
c _________________________________________________________
c               Check delay table interval data 
c rrb 98/03/20; Daily constraint in # of daily intervals
c rrb 99/08/13; Additional check
c     if(iday.eq.1 .and. interv.gt.0) then
      if(interv.gt.0 .and. (iday.eq.1 .or.iwell.ge.1)) then
        write(nlog,161)
        goto 9999
      endif

      close (1)
c
c               Return if only wanted to read control file
      if(inx.eq.1) goto 925
c
c               CALL SYSTEM TIME AND DATE FOR REPORT HEADINGS.
      IF(IOPTIO.NE.3) GOTO 170
c
c_________________________________________________________
c               Read River Network (*.rin) 
c
  170 write(nlog,102)
      write(6,102)
  102 format(/,72('_'),/
     1 '  Datinp; River Network File (*.rin)')
      iin2=iin
c
c rrb 2008/02/22; Read new or old response file formats (infile)
c		  add path (fpath1), open file (filena), get
c		  number of stations (numopr, if 0) and get 
c		  version number (ioprX)
c		  inf = filename
c		  nf = file #
      inf=3
      nf=2
      call GetFile(
     1   nlog, nchk, iin, infile, maxfile, maxfn, 
     1   nf, irinX, numRin, fileType(inf), fileSuf(inf), 
     1   fileName(inf), fpath1, filena)
C
      MAXSTB=MAXSTA+1
      is1=1
      if(ioutN.eq.1 .or. ichk.gt.0) write(nchk,1001)
      
c     write(nlog,*) '  Datinp; ioutN ', ioutN      
      DO 180 IS=1,MAXSTB
c
c _________________________________________________________      
c rrb 2004/12/14; Allow blanks if a # in column 1 
c		Note iocode=0 = Comment (#)
c                           1 = Data
c			    2 = EOF
c                           3 = Error                    
        call comment(2, nlog, iocode, nchk, 0)
        if(iocode.eq.0) goto 180
        if(iocode.eq.2) goto 190
        if(iocode.eq.3) goto 928
                
        read (2,1000,end=190,err=928)
     1    cstaid(is1),rec24, cstadn(is1),gwmaxr(is1)
     

cx        write(nlog,1000) 
cx     1    cstaid(is1),rec24, cstadn(is1),gwmaxr(is1)
        
c     
c ---------------------------------------------------------
c		Store name as a real for historical consistency
        stanam1(is1)=rec24
cx        j2=0
cx        do j=1,6
cx          j1=j2+1
cx          j2=j1+3
cx          rec4 = rec24(j1:j2)
cx          read(rec4,'(a4)') stanam(j,is1)
cx        end do  
c
c ---------------------------------------------------------
c rrb 10/7/94 Allow blank data at end of file
        if (cstaid(is1) .eq. blank) goto 190
     
        if(ioutN.eq.1 .or. ichk.gt.0) write(nchk,1002) is1, 
     1    cstaid(is1),stanam1(is1),cstadn(is1),gwmaxr(is1)
     
        if(iwell.eq.2) gwmaxr(is1) = gwmaxrc
c       write(nlog,*) '  Datinp, is, gwmaxr', is, gwmaxr(is)

c
c               Check for connection problems
        if(cstaid(is1).eq.cstadn(is1)) then
          write(6,1010) cstaid(is1), cstadn(is1)
          write(nlog,1010) cstaid(is1), cstadn(is1)
          goto 9999
        endif
        
        is1=is1+1     
        
  180 CONTINUE
C
      write(nlog,1020) MAXSTA
      write(6,1020) maxsta
      Goto 9999
C
C------  SET NUMSTA TO NUMBER OF STATIONS READ
C
  190 NUMSTA=IS1-1          
      write(nlog,192) numsta
  192 format(/,      
     1  '  Datinp; Number of Network Stations (*.rin) = ', i5)
  
c
c               Check that last station is blank to avoid a 
c               problem in the return flow algorythm
      if(cstadn(numsta).ne.blank) then
        write(6,1460)
        write(nlog,1460)
        goto 9999
      endif
C
      close (2)
C
C------  FIND THE DOWNSTREAM STATION CODE OF EACH STATION
C
      DO 220 IS=1,NUMSTA
      DO 200 NS=1,NUMSTA
      if(cstadn(is).eq.cstaid(ns)) goto 210
  200 CONTINUE
      IDNCOD(IS)=0
      Goto 220
  210 IDNCOD(IS)=NS
  220 CONTINUE
C
C------  BUILD TREE STRUCTURE
C
      DO 230 IS=1,NUMSTA
        NDNNOD(IS)=0
  230 CONTINUE

      DO 260 IS=1,NUMSTA
        ISS=IS 
c
c rrb 06/28/96; Check for a network looping onto itself
        do 240 isx=1,numsta
  240   dumsta(isx) = 0.0
  
  250   NDNNOD(IS)=NDNNOD(IS)+1
          ISS=IDNCOD(ISS)
c
c rrb 06/28/96; Check for a network looping onto itself
c       IF(ISS.NE.0) Goto 270
          if(iss.ne.0) then
            if(dumsta(iss).gt.0.01) then
              write(nlog,1400) cstaid(iss)
              goto 9999
            endif      

            dumsta(iss) = 1.0
            goto 250
          endif
  260 CONTINUE                           
c              
c               Print tree structure information
c       write(nlog,*) '  Datinp; ioutN @ line 1053 ', ioutN
        if(ioutN.eq.1 .or. ioutN.eq.2 .or. ichk.eq.1) then
          write(nchk,1030)
          do 270 is=1,numsta
            write(nchk,1040) is, cstaid(is), cstadn(is), 
     1                    idncod(is), ndnnod(is)
  270     continue
        endif
c _________________________________________________________
c
c
c               Read Reservoir Station data (*.res)
c
c rrb 2008/02/22; Read new or old response file formats (infile)
c		  add path (fpath1), open file (filena), get
c		  number of stations (numopr, if 0) and get 
c		  version number (ioprX)
c		  inf = filename
c		  nf = file #
      write(6,103)
      write(nlog,103)
  103 format(/,72('_'),/
     1 '  Datinp; Reservoir Station File (*.res)')
      iin2=iin

      inf=11
      nf=3
      call GetFile(
     1   nlog, nchk, iin, infile, maxfile, maxfn, 
     1   nf, iresX, numRes, fileType(inf), fileSuf(inf), 
     1   fileName(inf), fpath1, filena)

      if(iout.eq.1) write(nlog,*) '  Datinp; iresX ', iresX     
      if(iresX.le.1) then
        call GetRes(IIN, inx, numstax)
      else
        call GetRes2(IIN, inx, numstax)        
      endif      

c
c _________________________________________________________
c               Step X; Read Diversion Station Data (*.dds)
c
c      
c ---------------------------------------------------------      
c
c rrb 2008/02/22; Read new or old response file formats (infile)
c		  add path (fpath1), open file (filena), get
c		  number of stations (numopr, if 0) and get 
c		  version number (ioprX)
c		  inf = filename
c		  nf = file #
      write(6,104)
      write(nlog,104)
  104 format(/,72('_'),/
     1 '  Datinp; Diversion Station File (*.dds)')
      iin2=iin

      inf=5
      nf=4
      call GetFile(
     1   nlog, nchk, iin, infile, maxfile, maxfn, 
     1   nf, iddsX, numDds, fileType(inf), fileSuf(inf), 
     1   fileName(inf), fpath1, filena)
     
      if(iDdsX.le.1) then
        call  GetDiv(IIN, inx, numstax)
      else
        call GetDiv2(IIN, inx, numstax)        
      endif      
c
c _________________________________________________________
c		INITIALIZE RETURN FLOW ARRAY
C
  730   continue
      if(ichk.eq.4) write(nlog,*) 
     1  'Datinp initialize monthly return flow arrays'
     
        DO IS=1,maxsta
          DO IM=1,maxdlm
            RETUR(IM,IS)=0.
          end do
        end do
c
c _________________________________________________________
c
c rrb 97/10/15; Initialize daily return array
      if(ichk.eq.4) write(nlog,*) 
     1  'Datinp initialize daily return flow arrays'
     
        if(iday.eq.1) then
c
c rrb 2006/05/01; Correction        
cr        do is=1,nstrtn
          do is=1,maxsta
            do idy=1,maxdld
              returd(idy,is) = 0.0
            end do
          end do
        endif  
c
c _________________________________________________________
c
c		INITIALIZE Plan arrays
      if(ichk.eq.4) write(nlog,*) 
     1  'Datinp initialize MONTHLY plan arrays'

      do ip=1,maxplan
        do im=1,maxdlm
          pobl(im,ip)=0.0
          psup(im,ip)=0.0
        end do
      end do
c
c		Initialize daily plan arrays
      if(iday.eq.1) then
        if(ichk.eq.4) write(nlog,*) 
     1    'Datinp initialize DAILY plan arrays'
c
c rrb 2006/05/09; correction     
cr      do ip=1,nplan
        do ip=1,maxplan
          do idy=1,maxdld
            pobld(idy,ip)=0.0
            psupd(idy,ip)=0.0
          end do  
        end do            
      endif
c
c		Initialize reuse plan array
c		Note ipnode(i)>0 means the river node has a reuse plan
      do ns=1,maxsta
        ipnode(ns)=0
      end do	
c
c __________________________________________________________
c               Step X; Read River Station (*.ris) 
c
      write(nlog,105)
      write(6,105)
  105   format(/,72('_'),/,'  Datinp; River Station File (*.ris)')
      iin2=iin

c
c rrb 2008/02/22; Read new or old response file formats (infile)
c		  add path (fpath1), open file (filena), get
c		  number of stations (numopr, if 0) and get 
c		  version number (ioprX)
c		  inf = filename
c		  nf = file #
      inf=4
      nf=55
      call GetFile(
     1   nlog, nchk, iin, infile, maxfile, maxfn, 
     1   nf, irisX, numRis, fileType(inf), fileSuf(inf), 
     1   fileName(inf), fpath1, filena)
c


      DO 750 IS=1,NUMSTA
  750 ISTARU(IS)=0
C
      MAXRUO=MAXRUN+1
C
      DO 780 NP=1,MAXRUO
c
c rrb 04/25/97; Daily model (next 9 lines (up to ... goto 790)
c               idays  = 0 = set monthly to daily average,
c                        1 = daily gage monthly controls
c                        2 = reference gage monthly controls
c                        3 = daily gage daily rules
c                       -1 = divide by # of days
c
c               Read *.ris stream inflow data
        read(55,1312,end=790,err=928)
     1    crunid(np),rec24,cgoto, crunidy(np)
     
        if(ioutS.eq.1) write(nlog,1314)
     1    np, crunid(np),rec24,cgoto, crunidy(np)
c
c ---------------------------------------------------------
c		Store name as a real divnam for historical consistency
        Runnam1(np)=rec24
        
cx        j2=0
cx        do j=1,6
cx          j1=j2+1
cx          j2=j1+3
cx          rec4 = rec24(j1:j2)
cx          read(rec4,'(a4)') runnam(j,np)
cx        end do  
     
c
c
c rrb 2006/03/20; Adjust character string to left     
        crunid(np)=adjustl(crunid(np))
        crunidy(np)=adjustl(crunidy(np))
        
        idays(np)=0
        if(crunidy(np).ne.blank) idays(np)=2
        if(crunidy(np).eq.crunid(np)) idays(np)=1
        if(crunidy(np).eq.'-1          ') idays(np)=-1
        if(crunidy(np).eq.'0           ') idays(np)=0
        if(crunidy(np).eq.'3           ') then
          idays(np)=3
          crunidy(np)=crunid(np)
        endif
c
c rrb 01/08/08; Add type 4 (pattern is via connecting mid points)
        if(crunidy(np).eq.'4           ') then
          idays(np)=4
          crunidy(np)=crunid(np)
        endif

c
c       write(nlog,*) '  Datinp; np, idays', np, idays(np)
c
        if(crunid(np).eq.blank) goto 790
C
C------  FIND RUNOFF STATION INDEX IN XSTAID ARRAY
C
        DO 760 IS=1,NUMSTA
          if(cstaid(is).eq.cgoto) goto 770
  760   CONTINUE
C
        write(nlog,1320) cgoto
c
c rrb 10/27/94 Additional Output
        write(6,1320) cgoto
        Goto 9999
C
  770   IRUSTA(NP)=IS
        ISTARU(IS)=NP
  780 CONTINUE
C
      write(nlog,1330) MAXRUN
c
c rrb 10/27/94 Additional Output
      write(6,1330) maxrun
      Goto 9999
C
  790 NUMRUN=NP-1
      write(nlog,791) numrun
  791 format(/,
     1  '  Datinp; Number of river stations (*.ris) = ', i5)
  
C
      close (55)
      DO 820 NP=1,NUMRUN
        IRUDND(NP)=0
        ISCD=IRUSTA(NP)
        ISS=IDNCOD(ISCD)
        IF(ISS.EQ.0) Goto 820 

c       if(iss.eq.0) goto 9441

        NDNS=NDNNOD(ISS)
        DO 800 ND=1,NDNS
          IF(ISTARU(ISS).NE.0) Goto 810
          iss = idncod(iss)
  800   CONTINUE
        Goto 820

c       goto 9441

  810   IRUDND(NP)=ISTARU(ISS)
c
  820 CONTINUE
c
c _________________________________________________________
c
c               Read Instream Flow Station (*.ifs) 
c

  830 write(6,107)
      write(nlog,107)
  107   format(/,72('_'),/,
     1 '  Datinp; Instream Flow Station File (*.ifs) ')
      iin2=iin
c
c ---------------------------------------------------------
c rrb 2008/02/22; Read new or old response file formats (infile)
c		  add path (fpath1), open file (filena), get
c		  number of stations (numopr, if 0) and get 
c		  version number (ioprX)
c		  inf = filename
c		  nf = file #
      inf=7
      nf=55
      call GetFile(
     1   nlog, nchk, iin, infile, maxfile, maxfn, 
     1   nf, iifsX, numIfs, fileType(inf), fileSuf(inf), 
     1   fileName(inf), fpath1, filena)
C
      MAXIFS=MAXIFR+1
      do is=1,maxsta
        idum(is) = 0
      end do
C
c rrb 99/09/15; Set instream flow data type variables
      itmpi1=0
      itmpi2=0
      nisfinA=0
      nisfinM=0
      nrgfor=0

      DO 890 NF=1,MAXIFS
        read (55,1340,end=900,err=928)
     1    cifrid(nf),rec24,cgoto,
     1    IFRRSW(NF), crtnid,
     1    cifridy(nf), iifcom(nf)
     
       if(ioutI.eq.1) then
         write(nlog,*)  ' Datinp; Instream data ', nf
         write(nlog,*) 
     1    cifrid(nf),rec24,cgoto,
     1    IFRRSW(NF), crtnid,
     1    cifridy(nf), iifcom(nf)        
       endif
c
c ---------------------------------------------------------
c		Store name as a real divnam for historical consistency
        xfrnam1(nf)=rec24     
c
c rrb 2006/03/20; Adjust character string to left     
        cifrid(nf)=adjustl(cifrid(nf))
        cifridy(nf)=adjustl(cifridy(nf))
c
c rrb 04/25/97; Set daily data code; 0 = read daily; 1 = use monthly
        idayi(nf) = 0
        if(cifridy(nf).ne.blank) idayi(nf)=2
        if(cifridy(nf).eq.cifrid(nf)) idayi(nf)=1
        if(cifridy(nf).eq.'-1          ') idayi(nf)=-1
        if(cifridy(nf).eq.'0           ') idayi(nf)=0
        if(cifridy(nf).eq.'3           ') then
          idayi(nf)=3
          cifridy(nf)=cifrid(nf)
        endif
c
c rrb 01/08/08; Add type 4 (pattern is via connecting mid points)
        if(cifridy(nf).eq.'4           ') then
          idayi(nf)=4
          cifridy(nf)=cifrid(nf)
        endif

c
c rrb 10/7/94 Allow blank data at end of file
        if(cifrid(nf).eq.blank) goto 900
c
c rrb 99/09/15; Set data time step code to annual (2) for backward
c               compatibility
        if(iifcom(nf).eq.0) iifcom(nf)=2
c
c rrb 2006/06/02; Correction        
cr      if(iifcom(nf).le.2) nisfin=nisfin+1
        if(iifcom(nf).eq.1) nisfinM=nisfinM+1
        if(iifcom(nf).eq.2) nisfinA=nisfinA+1
        if(iifcom(nf).eq.1) itmpi1=2
        if(iifcom(nf).eq.2) itmpi2=1
c
c rrb 2006/03/21; Read RG forcast from separate file        
        if(iifcom(nf).eq.3) nrgfor=nrgfor+1
C
C------  FIND I.F.R. STATION INDEX IN XSTAID ARRAY
C   
        DO IS=1,NUMSTA
          if(cstaid(is).eq.cgoto) goto 860
        end do
C
c        Exit if statoin was not found
        write(nlog,1350) cgoto, crtnid
        write(6,1350) cgoto, crtnid
        Goto 9999
C
  860   IFRSTA(NF)=IS            
c
c rrb 2005/07/25; Store structure type at this river node
        istrtype(is)=5000+nf   
       
c
c _________________________________________________________
c               Step X; Instream flow reach
c
c               Loop to find downstream river node (if provided)
c               ndnifs(nf) is the number of downstream node in reach
c               ndnifb(nf) is the beginning counter for that reach
c               ifrst2(nf) = downstream reach id
        if(ioutI.eq.1) write (nlog,*) ' Datinp; crtnid ', crtnid
        
        if(crtnid.eq.blank .or. crtnid.eq.cgoto) then
          ndnifs(nf) = 1
          ifrst2(nf) = is                                 
c
c rrb 08/14/96 Check for Instream Flow Station Overlap
c jhb 2014/07/24 experimental branch isfoverlap
c                remove this check, might work now with overlapping reaches
c                because v14.00.02 now allows multiple isf rights per node
c          if(idum(is).ne.0) then
c              write(nlog,1370) cifrid(nf), cifrid(idum(is))
c              goto 9999
c          endif
          idum(is) = nf
          goto 880
        else       
c        
c rrb 00/04/13; Allow ireach to control opening of file *.ifm
c         if(ireach.eq.0) then
          if(ireach.eq.0 .or. ireach.eq.2) then
            write(nlog,1360) cgoto, ireach
            goto 9999   
          endif

          iss=is 
          ndns = ndnnod(iss)
          do 870 nd=1,ndns      
            if(ioutI.eq.1) then
              write(nlog,*) ' Datinp;, nd, iss, idum(iss)'    
              write(nlog,*) ' Datinp;', nd, iss, idum(iss)                
            endif
c
c rrb 08/14/96 Check for Instream Flow Reach Overlap
c jhb 2014/07/24 experimental branch isfoverlap
c                remove this check, might work now with overlapping reaches
c                because v14.00.02 now allows multiple isf rights per node
c            if(idum(iss).ne.0) then
c              write(nlog,1370) cifrid(nf), cifrid(idum(iss))
c              goto 9999
c            endif
            idum(iss) = nf

            if(crtnid.eq.cstaid(iss)) then
              ndnifs(nf) = nd
              ifrst2(nf) = iss
              goto 880
            endif
  870     iss=idncod(iss)

          write(nlog,1350) cgoto, crtnid
          goto 9999  
        endif
c
  880   if(nf.eq.1) then
          ndnifb(nf) = 1
        else
          ndnifb(nf) = ndnifb(nf-1) + ndnifs(nf-1)
        endif
c
c               Detailed Check
c       if(nf.eq.1) then
c         write(nlog,*) '  Datinp; Instream Flow Reach Data'
c         write(nlog,*) '      nf  ndnifs  ndnifb'
c       endif
c       write(nlog,'(3i8)') nf, ndnifs(nf), ndnifb(nf)                   
c
c               Check dimension
        numrea=ndnifb(nf)+ndnifs(nf)
        if(ndnifb(nf)+ndnifs(nf).gt.maxrea) then
          write(nlog,1380) maxrea
          goto 9999
        endif

  890 CONTINUE
C
      write(nlog,1390) MAXIFR
      write(6,1390) maxifr
      Goto 9999
C
  900 NUMIFR=NF-1
      write(nlog,792) numifr
  792 format(/,
     1  '  Datinp; Number of instream flow stations (*.ifs) = ', i5)
  
c
c rrb 99/09/15; Set code for mix of data provided
c               monisf = 1 annual only, 2=monthly only; 3=both
      monisf=itmpi1+itmpi2
cr    write(nlog,*) '  Datinp; monisf = ', monisf

 902  close(55)
c
c _________________________________________________________
c               Step X; Read Well Station Data (*.wes)
c
c ---------------------------------------------------------      
c
c rrb 2008/02/22; Read new or old response file formats (infile)
c		  add path (fpath1), open file (filena), get
c		  number of stations (numopr, if 0) and get 
c		  version number (ioprX)
c		  inf = filename
c		  nf = file #
      write(6,108)
      write(nlog,108)
  108   format(/,72('_'),/,
     1 '  Datinp; Well Station File (*.wes) ')
     
      iin2=iin
      inf=9
      nf=55
      call GetFile(
     1   nlog, nchk, iin, infile, maxfile, maxfn, 
     1   nf, iwesX, numWes, fileType(inf), fileSuf(inf), 
     1   fileName(inf), fpath1, filena)
     
      if(iwesX.le.1) then
        call GetWel(IIN, inx, numstax)
      else
        call GetWel2(IIN, inx, numstax)        
      endif      
c
c _________________________________________________________
c               Step X; Read Plan data (*.pln)
  901 iprintdp=0
c
c
c rrb 2008/02/22; Read new or old response file formats (infile)
c		  add path (fpath1), open file (filena), get
c		  number of stations (numopr, if 0) and get 
c		  version number (ioprX)
c		  inf = filename
c		  nf = file #
      write(6,109)
      write(nlog,109)
  109   format(/,72('_'),/,
     1 '  Datinp; Plan Station File (*.pln) ')
     
      inf=53
      nf=55
      call GetFile(
     1   nlog, nchk, iin, infile, maxfile, maxfn, 
     1   nf, iplnX, numPln, fileType(inf), fileSuf(inf), 
     1   fileName(inf), fpath1, filena)
     
      if(ichk.eq.4) write(nlog,*) ' Datinp; Calling GetPln'
     
      call getpln(numstax)
    
c _________________________________________________________
c
c		Step X; Read River Gage file (*.rig)
c			(infile=1)
      iriver=0
        
      write(6,113)
      write(nlog,113)
  113   format(/,72('_'),/,
     1 '  Datinp; River Gage File (*.rig) ')
     
      inf=68
      nf=68
      call GetFile(
     1   nlog, nchk, iin, infile, maxfile, maxfn, 
     1   nf, iplnX, numRig, fileType(inf), fileSuf(inf), 
     1   fileName(inf), fpath1, filena)
     
      if(ichk.eq.4) write(nlog,*) ' Datinp; Calling GetRig'


cx        if(infile.eq.1) then
cx          ifn=68
cx          rec256=fileName(ifn)
cx          
cx          if(rec256(1:2).ne.'-1') then
cx            iriver=1        
cx            
cx            rec48= ' Datinp; River Gage Structure (*.rig)'            
cx            write(nlog,100) rec48
cx            write(nlog,'(5x, a256)') rec256            
cx            
cx            iin2=68
cx            filena = rec256
cx            call putpath(maxfn, filena, fpath1)
cx            
cx            open(68, file=filena,status='old',err=928)
cx            call skipn(68)
cx          endif
cx        endif  
        if(numrig.gt.0) call GetRig
c
c _________________________________________________________
c               Step X; Network Checks
c
c rrb 10/02/95; check for more than one final outlet

        if(iout.eq.1) write(nlog,*) ' Datinp; network checks'
  909   ix = 0
        do 910 is=1,numsta
          if (idncod(is).eq.0) ix = ix + 1
  910   continue                        

        if(ix.gt.1) then
          write(nchk,911)
          ix=0
          do is=1,numsta
            if(idncod(is).eq.0) then
              ix=ix+1
              write(nchk,'(2x, i5, 1x,i8,1x,a24,20i8)')
     1        ix, is,stanam1(is), idncod(is), ndnnod(is)
            endif
          end do
        endif
c
c _________________________________________________________
c
c               Print general information to log file
        if(iout.eq.1) write(nlog,*) ' Datinp; General Information'

        ncom = numsta - numrun-numdiv-numifr-numres-numdivw - nplan

        write(nlog,1120) numrun, numdiv, numres, numifr,
     1                   numdivw, numdivwS, numdivwX, nplan, 
     1                   ncom, numsta
c
c _________________________________________________________
c               Step X; Check return locations that cause reoperation
c rrb 01/03/08; 
      if(iout.eq.1) write(nlog,*) ' Datinp; Return Checks'
      if(ichk.eq.4) write(nlog,*) ' Datinp; Check return flows'
      
      iprintx=0
      do nd=1, numdiv
        ndx=0
        idcd=idvsta(nd) 

        irni=nrtn(nd)
        irne=nrtn(nd+1) - 1

        do irn=irni,irne
          ircd=irnsta(irn)
          if(ircd.eq.0) then
            write(nlog,*) '  Datinp; nd, idcd, irni, irne, ircd'
            write(nlog,*) '        ',nd, idcd, irni, irne, ircd
          endif
          
          iscd=idncod(ircd)

          if(idcd.gt.0) then
            iok=0
            idwn=idncod(idcd)
            if(idwn.gt.0) then
              ndnnt=ndnnod(idwn)
            else      
              is=idvsta(nd)      
              write(nlog,1492) cstaid(is)
              goto 9999
            endif

            do n=1,ndnnt
              if(ircd.eq.idwn) iok=1
              idwn=idncod(idwn)
            end do

            if(iok.eq.0) then
              rec32='Upstream Return Flows' 
              if(iprintx.eq.0) write(nlog,1281) rec32
c
c rrb 2018/04/08; Reduce output to *.log              
cx            if(iprintX.eq.0) write(nlog,1285) 
              if(iprintx.eq.0) write(nchk,1285) 

              if(ndx.eq.0) then
                iprintx=iprintx+1    
c
c rrb 2018/04/08; Reduce output to *.log              
cx                write(nlog,1284) 
cx     1            iprintx, cdivid(nd),  divnam1(nd),
cx     1            cstaid(idcd), cstaid(ircd)  
     
                write(nchk,1284) 
     1            iprintx, cdivid(nd),  divnam1(nd),
     1            cstaid(idcd), cstaid(ircd)  
                ndx=nd
              endif
            endif
          endif
        end do
      end do
c
c _________________________________________________________
c               Step X; For testing; override daily ID data
      if(ichk.eq.20) then
        write(nlog,*) '  Datinp; Warning daily ID data overridden'
        do n=1,numres
          idayr(n)=0
        end do

        do n=1,numdiv
          idayd(n)=0
        end do

        do n=1, numifr
          idayi(n)=0
        end do

        do n=1, numdivw
          idaydw(n)=0
        end do

        do n=1, numrun
          idays(n)=0
        end do
      endif
c
c _________________________________________________________
c               Step X; Return
c
c 925 stop
  925 continue
      if(ichk.eq.4) write(nlog,*) ' Datinp; return'
      RETURN
c
c               Error Handling
c _________________________________________________________
c
  926 write(nlog,927) iin2, filena
  927 format(/,72('_'),/
     1 ' Datinp; Problem. End of file # ', i4, ' encountered',/,
     1 '   File name: ', a256)
      goto 9999
c
  928 write(nlog,929) iin2, filena
  929 format(/,72('_'),/
     1 ' Datinp; Problem reading file # ', i4,/,
     1       '   File name: ', a256,/
     1       '   Problem record (next line):')

      backspace(iin2)
      read(iin2, '(a256)',end=926,err=926) recin
      write(nlog,'(a256)') recin
      goto 9999
c
c               Formats
c ___________________________________________________
  161  format(/,72('_'),/       
     1    '  Datinp; Problem daily or well option is on.',/
     1    '          These reqiure the variable interv from the *.ctl',/
     1    '          file be a negative number',/
     1    '          Note a negative indicates a variable number of',/
     1    '          values are provided in the delay (*.dly) file'/
     1    '          a -999 allows a character format',/
     1    '          a -100 expects data as a decimal' )
     
  544  format(/,72('_'),/       
     1   '  Datinp; Warning well id ', a12,
     1    ' has idvcomw = ', i8, ' which is no longer supported. ',/
     1          '          It has been reset to 1 since ichk = 2')
     
  545  format(/,72('_'),/       
     1 '  Datinp; Problem well id ', a12,
     1    ' has idvcomw = ', i8, ' which is not supported.')
     
  546  format(/,72('_'),/
     1 '  Datinp; Problem in *.wes well id ', a12,
     1          ' has idvcomw = ', i4, /
     1 '          But in *.ctl the variable icondem (idemtyp) = ',i4,
     1          ' which is inconsitant.')
     
  547  format(/,72('_'),/
     1 '  Datinp; Problem for Div or Well station = ', a12,
     1          ' type = 4 which indicates transmountain, but',/
     1 '          efficiency is not 100%.',
     1 '          To do; Change type or efficiency data to 100%')

  758  format(/,72('_'),/
     1  '  Datinp; Problem Multi User no longer supported', 
     1 ' but specified in *.dds for ID: ',a12)  
     
  903 format(/,72('_'),/
     1 '  Datinp; Warning well control switch = ', i5,' (on)',/ 
     1 '          but no well station data was not provided.',/
     1 '          Setting well control switch = 0 and proceeding')
     
  911  format(/,72('_'),/
     1 '  Datinp;  Warning more than one final outlet',/
     1 '           Will treat tributary like a futile call',//
     1 '           See column To node = 0 in the following table',/
     1 '      #  Fr node Fr Name                  To node  # down',/
     1 '  _____ ________ ________________________ _______ _______')

  930  FORMAT(A256)
  940  format(4x, a256)
  950  FORMAT(a80)
  960  format(i8)
  970  FORMAT(F8.5)
  972  FORMAT(F8.3)

  980  FORMAT(12I4)
  990  format(/,72('_'),/
     1  '  Datinp; Problem. Year type must be CYR, WYR or IYR',/
     1  '     Not ', a5)
     
 1000  format(a12,a24,a12, 1x, 12x, 1x, f8.0)
 1001  format(/,72('_'),/
     1  '  Datinp; Network file (*.rin)',//,
     1  '    # ID          Name                    DownStream   ',
     1  'GWMax    Comment',/  
     1  ' ___exb__________eb______________________eb__________ex',
     1  'b______exb__________________e')

 1002  format(i5, 1x, a12,a24,a12, 1x, f8.0)
 
 1010  format(/,72('_'),/
     1  '  Datinp; Problem. Network file (*.rin),',
     1    ' station ', a12, ' goes to ', a12)
     
 1020  FORMAT(/,72('_'),/
     1 ' Datinp; Problem. Too Many Stations, Maximum = ',I5)
 1021  FORMAT(/,72('_'),/
     1  ' Datinp; Problem.',
     1  ' Too many return stations, max = ', i5)

 1030  format(/,72('_'),/
     1'  Datinp; River Network Summary',//
     1  '  Is cstaid       cstand         idncod  ndnnod',/
     1  '____ ___________ ____________    _______ _______')
     
 1040  format(i4, 2(1x, a12), 1x, 2i8)
 
 1050  format(24x,4f8.0,4i8)
 
 1060  FORMAT(/,72('_'),/
     1  ' Datinp; Problem.',
     1  ' Reservoir ',a12,' not found in river station file')
     
 1070  FORMAT(/,72('_'),/
     1  ' Datinp; Problem. TOO MANY RESERVOIR OWNERSHIPS ',I5,
     1  '  MAXIMUM = ',I5)
     
 1080  FORMAT(12x,a12,3f8.0,i8)
 

     
 1120  format(/72('_'),/
     1  '  Datinp; River Node Summary',//
     1  '  Number of Inflow nodes                    = ',i5,/
     1  '  Number of Diversion nodes                 = ',i5,/
     1  '  Number of Reservoir nodes                 = ',i5,/
     1  '  Number of Minimum Instream nodes          = ',i5,/
     1  '  Number of Well nodes Total                = ',i5,/
     1  '  Number of Supplemental Well nodes = ',i5,/
     1  '  Number of Sole Source Well nodes  = ',i5,/
     1  '  Number of Plans                           = ',i5,/
     1  '  Number of Other nodes (1)                 = ',i5,/
     1  '  _________________________________________________',/
     1  '  Total of River nodes                      = ',i5,//,
     1  '  (1) May be negative if multiple activities occur at the',
     1  '  same node',//)
 1130  FORMAT(/,
     1  '  Datinp; Problem. Too MANY EVAPO. COMBINATIONS ',I5,
     1  '     MAXIMUM = ',I5)
 1140  format(/,
     1 '  Datinp; Warning Res. ', i4, ' has no evap data')
 1150  FORMAT(24x, a12,F8.0)
 1160  FORMAT(/,
     1  ' Datinp; Problem. Too MANY RAINFALL COMBINATIONS ',I5,
     1  '   MAXIMUM = ',I5)
 1170  FORMAT(24X,8F8.0)
 1180  FORMAT(/,
     1  ' Datinp; Problem. Too MANY RESERVOIRS,    MAXIMUM = ',I5)
 1190  format(/,
     1   ' Datinp: Problem',/
     1   '         Area capacity curve for reservoir ', a12, /
     1   '         is not increasing',//
     1   '            Entry       Value',/
     1   '         ________ ___________', /,
     1   9x, i8, f12.4,/, 9x, i8, f12.4,//
     1   '         Problem could be number of accounts specified',/
     1   '         is not consistent with the ownership data')
 1191  format(/,
     1   ' Datinp: Warning',/
     1   '         Capacity in area-capacity curve for reservoir ',a12,/
     1   '         entry ', i5, ' is not increasing.',/
     1   '         Value adjusted by ',f8.4,//
     1   '            Entry       Value   Adj Value',/
     1   '         ________ ___________ ___________', /,
     1   9x, i8, f12.4, f12.4,/, 9x, i8, f12.4, f12.4)
 1192  format(/,
     1   ' Datinp: Warning',/
     1   '         Area in area-capacity curve for reservoir ',a12,/
     1   '         entry ', i5,' is not increasing. ',/
     1   '         Value adjusted by ', f8.4,//
     1   '            Entry       Value   Adj Value',/
     1   '         ________ ___________ ___________', /,
     1   9x, i8, f12.4, f12.4,/, 9x, i8, f12.4, f12.4)

 1210  FORMAT(
     1  '  Datinp; Problem for diversion id = ', a12,/
     1  '          in the diversion station file (*.dds) ',/
     1  '          the river location (cgoto) = ', a12,/
     1  '          cannot be found in the network file (*.rin)',/
     1  '          (If the id looks ok, Check to be sure it',/
     1  '          does not contain a tab character')
 1212  FORMAT(
     1  '  Datinp; Problem for well id = ', a12,/
     1  '          in the well station file (*.wes) ',/
     1  '          the river location (cgoto) = ', a12,/
     1  '          cannot be found in the network file (*.rin)',/
     1  '          (If the id looks ok, Check to be sure it',/
     1  '          does not contain a tab character')
 1220  FORMAT(/,
     1  '  Datinp; Problem. Too MANY DIVERSION USERS      ',I5,
     1  '  MAXIMUM = ',I5)
c1232  FORMAT(12X,24x,a12, 2I8,2F8.0,i8)
 1232  format(12x,24x,a12, 3i8, 2f8.0, i8, f8.0)
 
 1238  format(/,72('_'),/ 
     1  '  Datinp; Warning, for baseflow calculations reset control',/
     1  '          variable icondem (idemtyp) = 1 to insure no ',/
     1  '          adjustments to historic diversion and well',/ 
     1  '          pumping data')
     
 1239  format(/,72('_'),/
     1  '  Datinp; Warning, in baseflow therefore ID ', a12, 'has had',/
     1  '          its demand type (idvcomw) reset to 1.  Recall',/
     1  '          the baseflow mode reads *.weh in lie of *.wem')
 1240  format(/,72('_'),/
     1  '  Datinp; Warning, Annual diversions not used in base',
     1  ' flow calcs'/)

 1241  format(/,72('_'),/
     1  '  Datinp; Problem, In Baseflow mode yet diversion code',
     1           ' in *.dds says IWR data provided (idvcom = 3).', 
     1           ' for ID = ', a12,/
     1  '          Set to 1 to avoid having historic diversion', 
     1           ' data weighted by efficiency.')
     
 1242  format(/,72('_'),/
     1  '  Datinp; Warning, Wells must have a return & depletion',
     1  ' location',/ '          Check ID ', a12 )
     
 1243  format(/,72('_'),/
     1  '  Datinp; Problem, In Baseflow mode yet diversion code',/
     1  '          in *.wes says *.ddc data provided (idvcomw = 3).',/ 
     1  '          Stopped to avoid having historic pumping',/, 
     1  '          data weighted by efficiency.  To do: revise *.wes')
     
 1250  FORMAT(/,72('_'),/
     1  ' Datinp; Problem. Too MANY RETURN FLOW STATIONS, MAX = ',I5)
     
 1255 format(/,72('_'),/
     1 '  Datinp; Problem with Well Station ', a12,/
     1 10x,'The augmentation plan code (planw) = ', a12,/
     1 10x,'But this plan is type ', i4,/
     1 10x,'(Note a well agumentation plan should be a type 2)')
     
cr 1256 format(/,72('_'),/
cr     1 '  Datinp; FYI at least one Well Station (e.g.) ', a12,/
cr     1 9x,' is tied to an augmentation plan (planw) = ', a12,
cr     1     ' plan pointer = ', i5)
     
 1257 format(/,72('_'),/,
     1 '  Datinp; Well Station augmentation plan ties',/
     1          'plan (planw) = ', a12,
     1     ' plan pointer = ', i5)
cr     1 ' Well_ID     Aug_ID        Tie Note',/
cr     1 ' ___________ ___________ _____ ____________')
     
 1258 format(i5, 1x, a12, 1x, a12, 1x, i5, 1x, a12)       
     
 1260  format(36x,a12,f8.0,i8)
 1262  format(36x,a12,f8.0,a12)
     
 1280  FORMAT(/,72('_'),/
     1  '  Datinp; Problem.'
     1  ' Station ',a12,' of diversion station file',
     1  ' has return flows going back to the diverting node',
     1  ' at ', a12)
     
 1281  FORMAT(/,72('_'),/
     1  '  Datinp; Warning See *.chk for details regarding: ',a32)
     
 1283  FORMAT(/,72('_'),/
     1  '  Datinp; Problem the following structure(s) ',
     1    'return water upstream',//
     1  '    # Str ID       Str Name                ', 
     1  ' Located at   Returns to',/ 
     1  ' ____ ____________ ________________________', 
     1  ' ____________ ____________')
     
 1284  format(i5,1x, a12, 1x, a24, 1x, a12, 1x a12)
     
 1285  FORMAT(/,72('_'),/
     1 '  Datinp; Warning the following structure(s) return water',/
     1 10x,'to at least one non-downstream node. Only the first',/ 
     1 10x,'non-downstream occurance is printed per structure.',/
     1 10x,'Note, a non-downstream return is OK but it can cause',/ 
     1 10x,'the system to reoperate and impact performance if',/
     1 10x,'the immediate (current month or days) return flow',/
     1 10x,'exceeds the prescribed tolerance (see Performance Info',/
     1 10x 'in the *.log file)'//
     1  '    # Str ID       Str Name                ', 
     1  ' Located at   Returns to',/ 
     1  ' ____ ____________ ________________________', 
     1  ' ____________ ____________')

 
 1292  FORMAT(/,72('_'),/
     1 '  Datinp; Problem station ',a12,
     1 ' of the Direct Diversion Station File (*.dds)',/
     1  10x, ' has the following return data:',/
     1  10x, ' Total of all returns = ', f8.2)
     
 1300  FORMAT(/,72('_'),/
     1 ' Datinp; Problem.',
     1 ' Too MANY DIVERSION PROJECTS,     MAXIMUM = ',I5)
     
 1302  FORMAT(/,72('_'),/
     1 ' Datinp; Problem. Too MANY Well Stations,     MAXIMUM = ',I5)

c rrb 04/25/97; Daily model
 1312  FORMAT(a12, a24, a12, 1x, a12)
 1314  FORMAT(i5, 1x, a12, a24, a12, 1x, a12)
 
 1320  FORMAT(/,72('_'),/
     1 '  Datinp; Problem. ',
     1  ' STATION ',a12,' OF *.ris file not found in',
     1 ' the river network (*.rin) file')
     
 1322  FORMAT(/,72('_'),/
     1  '  Datinp; Warning Well structure not tied to a SW structure',/
     1  '          and not specified as N/A or a SW structure cannot',/ 
     1  '          be found in the diversion station file (*.dds).',/
     1  '          Treating these as well only structures',//
     1  '  Well ID      Well Name                SW ID',/ 
     1  '  ____________ ________________________ ____________')
     
 
 1330  FORMAT(/,72('_'),/
     1 '  Datinp; Problem. ',
     1  ' Too MANY INPUT VIRGIN FLOWS,   MAXIMUM = ',I5)
c
c rrb 99/09/15; Allow isf to have monthly data
 1340  FORMAT(a12,A24,a12,i8,1x,a12,1x,a12,i8)
 
 1350  FORMAT(/,72('_'),/
     1  '  Datinp; Problem. Station ',a12,
     1     ' or downstream station ', a12,/
     1  10x, 'of the Instream (*.ifs) station file cannot be found',/
     1  10x, 'or is not located downstream of the upstream station')

 1360  format(/,72('_'),/
     1  '  Datinp; Warning.',
     1  ' Instream station ',a12,' is specified to be',/,
     1  19x, 'a reach but control variable ireach = ', i5,/,19x,
     1  'Check documentation and revise to be consistent')
     
 1370  format(/,72('_'),/
     1 '  Datinp; Warning. ',
     1 ' Instream flow points or reaches ',a12,' and ',a12,/
     1  18x, ' overlap before the downstream point is found'/
     1  18x, ' Recommend you check your isf station (*.ifs) file',/
     1  18x, ' and or your river network (*.rin) file')
     
 1380     format('  Datinp; too many instream reach values, max = ',i5)
 
 1390  FORMAT(/,72('_'),/
     1  '  Datinp; Problem.',
     1  ' Too MANY .REAM FLOW REQ.,    MAXIMUM = ',I5)
     
 1400  format(/,72('_'),/
     1  '  Datinp; Problem the network loops onto itself',/
     1  '          Check the *.rin file ID = ', a12)
     
 1402  format(/,72('_'),/
     1 '  Datinp; Problem res. ID ', a12, ' has a daily ID = ', a12,/
     1 '          which says monthly data controls.  This approach ',/
     1 '          is not supported since it is EOM data.',/
     1 '          To do: set daily ID to type 0 (average) or ', 
     1           'type 3 (daily controls)')

 1410  write(nlog,1420) filena
 1420  format(/,72('_'),/
     1'  Datinp; Problem opening file: ', a256)
     
 1430  format(/,72('_'),/
     1 '  Datinp; Problem max eff is on (ieffmax =1)',/
     1 '          but no max eff data file (itsfile <= 0)')
 1460  format(/,72('_'),/
     1 ' Datinp; Problem',
     1 ' The last downstream station should be blank')
     
 1470  format(/,72('_'),/
     1   ' Datinp; Problem',
     1   ' two reservoirs at the same river node',/
     1   '         Reservor IDs = ', a12, 1x, a12)
     
 1490   format(/,72('_'),/
     1  '  Datinp; Problem sprinkler option (isprink) = 2',/
     1  '          is not supported during simulation'/
     1  '          Set isprink=0 or 1')
     
 1492 format(/72('_'),/
     1  '  Datinp; Problem ndnnt=0 (number of downstream nodes = 0)'/
     1  '          at river node ', a12,' To fix:',/
     1  '          1. Check that it supposed to have a downstream ',/
     1  '             node that goes nowhere',/
     1  '          2. If OK, revise *.rin to have its downstream ',/
     1  '              node go to a blank just like the last node',/
     1  '              in the network')
     
 1500   format(/,72('_'),/
     1  '  Datinp; Problem diversion ownership is less than 100%',
     1           ' for ID: ', a12, /
     1  20(i5, ' Owner Name = ', a24,
     1  ' Percent = ', f10.0,/))
     
 1510   format(/,72('_'),/
     1  '  Datinp; Problem starting year ', i4, ' is less than',/
     1  '          the ending year ', i4,' in the control (*.ctl)',/
     1  '          file. recommend you revise your control data.')

     

       goto 9999
c _________________________________________________________
c
c               Error Tracking


 9999 write(6,1440) 
      write(nlog,1450) 
      call flush(6)
 1440 format('  Stopped in Datinp, see the log file (*.log)')
 1450 format('  Stopped in Datinp')
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop 
      END
c
c *********************************************************
c
      subroutine getctl(nf, iok, ityp, i, r, c)
c
c       Getctl; A generic control data reader
c               nf=file number
c               iok     0 not OK if not found
c                       1 OK if not found
c               itype   0 integer
c                       1 = real
c                       2=character
c               i = integer value
c               r = real value
c               c = character value
c
      character cx*12, c*12

      iout=0
      i=0
      r=0.0
      c=' '
c
c _________________________________________________________
c               Read data based on type

c100  read(nf,'(a12)',end=110,err=110) cx
 100  read(nf,*,end=110,err=110) cx
c     write(99,*) '  Getctl; iok, cx = ', iok, cx
      if(cx(1:1).eq.'#') goto 100

      backspace(nf)

      if(ityp.eq.0) then      
        read(nf,*,end=110,err=110) ri
        i=ifix(ri)
        if(iout.eq.1) write(99,*) '  GetCtl; Integer i = ', i
        goto 120
      endif

      if(ityp.eq.1) then
        read(nf,*,end=110,err=110) r
        if(iout.eq.1) write(99,*) '  GetCtl; Real r = ',r
        goto 120
      endif

      if(ityp.eq.2) then
        read(nf,'(a12)',end=110,err=110) c
        if(iout.eq.1) write(99,*) '  GetCtl; Character c = ',c
        goto 120
      endif
c
c _________________________________________________________
c               Check if eof is OK
 110  if(iout.eq.1) write(99,*) ' GetCtl; iok = ',iok
      if(iok.eq.0) goto 200
c
c _________________________________________________________
c               Return
 120  return
c
c _________________________________________________________
c               Error Processing

 200  write(6,210) 
      write(99,220) 
      call flush(6)
 210  format('  Stopped in Getctl')
 220  format(72('_'),/
     1 '  Getcl; Stopped in Getctl, see the log file (*.log)')
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop 
      END


