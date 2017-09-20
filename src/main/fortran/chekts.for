

c
c *****************************************************
c
      subroutine chekts(nlog, nf, ityp, coeff, idummy, cyr1)
c
c
c _________________________________________________________
c	Program Description
c
c       Chekts; It checks that a time series data file
c               has the right year type for the simulation.
c
c
c _________________________________________________________
c	Update History
c		NA
c
c _________________________________________________________
c	Documentation
c
c _________________________________________________________
c	Dimensions
c

      dimension dtype(80), cunitX(10)
      character cyr1*5, cyr*5, dtype*45, cunit*5, recin*132,
     1  filena*256, cyr1X*5, rec72*72, cunitX*5, 
     1  recout*132,
     1  rec1a*1, rec1b*1, rec1c*1

      data dtype/
     1     'Evaporation File (*.eva)                     ',
     2     'Precipitation File (*.pre)                   ',
     3     'Base Streamflow File - Monthly (*.rim)       ',
     4     'Direct Flow Demand File - Monthly (*.ddm)    ',
     5     'Direct Flow Demand File - Annual (*.dda)     ',
     6     'Instream Flow Demand File - Annual (*.ifa)   ',
     7     'Reservoir Target File - Annual (*.tar)       ',
     8     'Historic Streamflow File - Monthly (*.rih)   ',
     9     'Historic Diversion File - Monthly (*.ddh)    ',     
     1     'Well Demand File - Monthly (*.wem)           ',
c     
     1     'Historic Pumping File - Monthly (*.weh)      ',
     2     'Direct Flow Demand Override File - (*.ddo)   ',
     3     'Base Streamflow File - Daily (*.rid)         ',
     4     'Direct Flow Demand File - Daily (*.ddd)      ',
     5     'Instream Flow Demand File - Daily (*.ifd)    ',
     6     'Reservoir Target File - Daily (*.tad)        ',
     7     'Instream Flow Demand File - Monthly(*.ifm)   ',
     8     'Time Series File - Annual (*.tsp or *.ipy)   ',
     9     'Consumptive Water Req. - Monthly (*.iwr)     ',
     2     'SJRIP Sedimentation - Annual (*.sjr)         ',
c     
     1     'Soil Moisture Parameter - (*.par)            ',
     2     'Consumptive Water Req. - Daily (*.iwd)       ',
     3     'Well Demand File - Daily (*.wed)             ',
     4     'Reservoir End of Day File - Daily (*.eod)    ',
     5     'Reservoir End of Month File - Monthly (*.eom)',
     6     'StreamEstimate_Coefficients',
     7     'StreamGage_Historic_Monthly',
     8     'Diversion_Historic_Monthly',
     9     'Well_Historic_Monthly',           
     3     'SanJuanRecovery',
c     
     1     'IrrigationPractice_Yearly',
     2     'ConsumptiveWaterRequirement_Monthly',
     3     'SoilMoisture',                    
     4     'GeographicInformation',
     5     'OutputRequest',                   
     6     'Stream_Base_Daily',
     7     'Diversion_Demand_Daily',
     8     'Instreamflow_Demand_Daily',
     9     'Well_Demand_Daily',
     4     ' ',
c
     1     'ConsumptiveWaterRequirement_Daily', 
     2     'DelayTable_Daily',
     3     'StreamGage_Historic_Daily',
     4     'Diversion_Historic_Daily',
     5     'Well_Historic_Daily',
     6     'Reservoir_Historic_Daily',               
     7     'StreamEstimate_Station',
     8     'StreamEstimate_Base_Monthly',
     9     'StreamEstimate_Base_Daily',     
     5     'StateCU_Structure',
c     
     1     'DownStream_Call (*.cal)',
     2     'RioGrande_Spill_Monthly (*.rgs)',
     3     'RioGrande_Forecast_Monthly (*.rgf)',      
     4     ' ',
     5     ' ',
     6     ' ',
     7     ' ',
     8     ' ',
     9     ' ',
     6     ' ',
c    
     1     ' ',
     2     ' ',
     3     ' ',
     4     ' ',
     5     ' ',
     6     ' ',
     7     ' ',
     8     ' ',
     9     ' ',
     7     ' ',
c    
     1     ' ',
     2     ' ',
     3     ' ',
     4     ' ',
     5     ' ',
     6     ' ',
     7     'Reservoir_To_Recharge_Monthly (*.rre)',
     8     'Diversion_To_Recharge_Monthly (*.dre)',
     9     ' ',
     8     ' ' /
     
     
      data cunitX/
     1 ' AF/M', ' ACFT', '   AF', '  CFS', '   FT', '   IN', '   NA',
     1 ' ', ' ', ' '/
    
c
      iout=0
      ierror = 0
      ibm = 0
      iby = 0
      idummy=0
      iwarnU=0
      
      ilength=0
      
      cyr = '     '
      iin2=nf
      coeff=-1.
      small=0.001
      smalln=-1*small

      filena(1:45)=dtype(ityp)
      
      if(iout.eq.1) then
        read(nf,'(a72)',end=38) rec72
        write(nlog,*) '  Chekts nf, rec72 ', nf, rec72
 38     backspace(nf)
      endif  
      
      write(nlog, 150)
 150  format(/,'  Chekts')     
c
c rrb 2006/03/01; Back to fixed format read and trim blanks from 
c                 characters
      read(nf,100,end=40,err=928) ibm, iby, iem, iey, cunit, cyr     
      write(nlog,100) ibm, iby, iem, iey, cunit, cyr
      if(iout.eq.1) write(nlog,*) ' Chekts; cunit ', cunit

c
c              Assume a blank file if month, year and year type are zero
 40   if(ibm+iby.eq.0 .and. cyr.eq.'     ') then
        write(nlog,130) dtype(ityp)
        idummy=1
        close(nf)
        goto 50
      endif
c
c _________________________________________________________
c		Check the year type and month are consistent 
c     write(nlog,*) ' Chekts; cyr = ', cyr
      cyr=adjustl(cyr)
c     write(nlog,*) ' Chekts; cyr = ', cyr
      recin(1:5) = cyr
      
      
      cyr1X=cyr1
c     write(nlog,*) ' Chekts; cyr1X = ', cyr1X      
      cyr1X=adjustl(cyr1X)
      write(nlog,*) ' Chekts; cyr1X = ', cyr1X
c
c		Adjust to upper case
      call AdjCase(nlog, recin, recout, 5, 2)      
      cyr=recout(1:5)
      
      if(cyr1X.ne. cyr) ierror = 1
c
c rrb 2007/09/04; Accept new ipy file format (without month)		      
      if(ityp.eq.18) then    
      else  
      
        if(cyr(1:3).eq.'CYR' .and. ibm.ne.1)  ierror = 1
        if(cyr(1:3).eq.'WYR' .and. ibm.ne.10) ierror = 1
        if(cyr(1:3).eq.'IYR' .and. ibm.ne.11) ierror = 1

        if(cyr(1:3).eq.'CYR' .and. iem.ne.12) ierror = 1
        if(cyr(1:3).eq.'WYR' .and. iem.ne.9)  ierror = 1
        if(cyr(1:3).eq.'IYR' .and. iem.ne.10) ierror = 1
      endif  
c
c _________________________________________________________
c		Set the coefficient based on the units provided
c               and what StateMod uses internally (cfs for most
c		acft for storage)
      coeff=-1.
      cunit=adjustl(cunit)
      recin(1:5)=cunit
c
c		Adjust to upper case
      call AdjCase(nlog, recin, recout, 5, 2)
      cunit=recout(1:5)
      
c
c		The following recognizes time series files
c		often do not include per month      
      if(cunit(1:4).eq. 'AF/M') coeff = 1.9835
      if(cunit(1:4).eq. 'ACFT') coeff = 1.9835
      if(cunit(1:4).eq. 'AF  ') coeff = 1.9835
c
c		Treat monthly storage terms uniquely
c		itype  7 = *.tar (Tartget Monthly)
c		itype 16 = *.tad (Target Daily)
c		itype 24 = *.eod (End of Day)
c		itype 25 = *.eom (End of Month)    
c
c rrb 2006/03/21; Add Rio Grande Forecast (a storage term)
      if(ityp.eq. 7 .or. ityp.eq.16 .or.
     1   ityp.eq.24 .or. ityp.eq.25) then
         if(cunit(1:4).eq. 'ACFT') coeff = 0.0
         if(cunit(1:4).eq. 'AF  ') coeff = 0.0
         isto=1
      endif  
      
c
c rrb 2005/12/23; Do not adjust if in CFS since
c                 Daily are always cfs and
c	          monthly are adjusted by /c/mthday
      if(cunit(1:3).eq. 'CFS')  coeff = 0.0
      if(cunit(1:2).eq. 'FT')   coeff = 0.0
      if(cunit(1:2).eq. 'IN')   then
c
c rrb 2006/08/14; Correction      
cr      coeff = 1.0/12.0
        coeff = 1.0/12.0
        ilength = 1
      endif  
      if(cunit(1:2).eq. 'NA')   coeff = 0.0
c
c _________________________________________________________
c rrb 2006/05/30; Clean up *.tsp or *.ipy unit treatment
      if(ityp.eq.18) then      
cx      write(nlog,146) cunit, ityp
        coeff=0.0
      endif  
c      
c
c _________________________________________________________
c		Exit if bad units or no units found      
c rrb 2006/05/30; Clean up
cr    if(coeff.lt.0.0) goto 930
      if(coeff.lt.smallN) goto 930
      
c
c _________________________________________________________
c rrb 2006/04/10; Clarify *.tsp or *.ipy unit treatment
      if(ityp.eq.18) then      
        write(nlog,146) cunit, ityp
      else  
        if(abs(coeff).lt.small) then
          write(nlog,140) cunit, ityp
        else
          if(ilength.eq.0) then
            cex=500.0/coeff/30.
            write(nlog,142) cunit, ityp, coeff, cunit, cex, 'CFS'
          else
            cex=500.0*coeff
            write(nlog,144) cunit, ityp, coeff, cunit, cex, 'FT'
          endif  
        endif
      endif
      
      
 140    format(
     1   '  Chekts; Units provided = ', a5, ' for Chekts file type ',
     1              i4,/
     1   '          StateMod (internally) uses CFS for flow,',/
     1   '          FT for length, and AF for storage.',/
     1   '          Therefore a data value of 500 is 500 for internal',
     1             ' calculations')
 142    format(
     1   '  Chekts; Units provided = ', a5, ' for file type ', i4,/
     1   '          Unit Conversion Factor = 1.0 divided by ', f8.4,/
     1   '          divided by the number of days in a month.', /
     1   '          StateMod (internally) uses CFS for flow,',/
     1   '          FT for length, and AF for storage.',/
     1   '          Therefore a data value of 500 ',a4, 
     1             ' is ', f12.4, ' ', a4, /
     1   '          for internal calculations (assuming a 30 day',
     1             ' month)')
 144    format(
     1   '  Chekts; Units provided = ', a5, ' for file type ', i4,/
     1   '          Unit Conversion Factor = ', f8.4,/
     1   '          StateMod (internally) uses CFS for flow,',
     1             ' FT for length, and AF for storage',/
     1   '          Therefore a data value of 500 ',a4,
     1            ' is ', f12.4, ' ', a4, /
     1   '          for internal calculations (assuming a 30 day',
     1            ' month)')
 146    format(
     1   '  Chekts; Units provided = ', a5, ' for file type ', i4,/
     1   '          This file has data with various units included',/
     1   '          to conicide with the same input file to StateCU.',/
     1   '          Therefore StateMod ignors the unit type provided',/
     1   '          and adjusts units internally to coincide with',/
     1   '          the file specifications.')
     
c
c _________________________________________________________
c		Print error messages

      if(ierror.eq.1) then
        write(nlog,110) dtype(ityp)
        write(nlog,120) ibm, iby, iem, iey, cunit, cyr, cyr1
        goto 9999
      endif                  

      call skipn(nf)
c      
c _________________________________________________________
c		Return
   50 return
c      
c _________________________________________________________
c		Error Handling
c
c rrb 97/11/02; Error Handling
  926 write(nlog,927) iin2, filena
  927 format(' Chekts.f; End of file # ', i4, ' encountered',/,
     1       '   File name: ', a256)
      goto 9999
c
  928 write(nlog,929) iin2, filena
  929 format('  Chekts.f; Problem reading file # ', i4,/,
     1       '    File name: ', a256,/
     1       '    Problem record (next line):')
      backspace(iin2)
      read(iin2, '(a132)',end=926,err=926) recin
      write(nlog,'(a132)') recin
      goto 9999
     
  930 write(nlog,931) iin2, filena, cunit, (cunitX(i), i=1,8)
  931 format(' Chekts.f; Problem reading file # ', i4,/,
     1       '   File name: ', a256,/
     1       '   The unit provided is not recognized',/
     1       '   Recommend you check the unit type specified',/
     1       '   and the data file for correct format. (Note the',/
     1       '   data can be upper or lower case)',/
     1       '   Unit provided:',/
     1           2x,a5,//
     1       '   Recognized unit types are (caps only):',
     1        /,10(2x,a5,/))
      goto 9999

c
c _________________________________________________________
c               Formats
  100 format(i5, 1x, i4, 5x, i5, 1x, i4, a5, a5)
  110 format('  Chekts; Year type is does not match control data in ',/,
     1       '          file ', a45)
  120 format(
     1 '  Beginning month: ' ,i5,/
     1 '  Beginning year:  ', i5,/
     1 '  Ending month:    ', i5,/
     1 '  Ending year:     ', i5,/
     1 '  Unit:              ', a5,/
     1 '  Year type read:    ', a5,/
     1 '  Year expected:     ', a5)
  130 format('  Chekts; FYI, A dummy (blank) file determined for ',/
     1       '          file: ', a45)
c
c _________________________________________________________
c               Exit message

 9999 write(6,*)  '  Stopped in Chekts, see log file (*.log)'
      write(nlog,*) '  Stopped in Chekts'
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop 
      end




