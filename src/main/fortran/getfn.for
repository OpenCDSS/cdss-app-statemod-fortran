c
       subroutine Getfn(iin, nlog, infile, maxfile, 
     1   fileName, ifileNum, filetype, fileSuf)
c     
c
c _________________________________________________________
c	Program Description
c
c      Getfn; Subroutine that reads in files in any order         
c              and parses thru them to get actual file name
c               Tyical input is *.rsp = FileName
c
c	      Called by Statem.for
c
c _________________________________________________________
c	Update History
c	2006/01/01; Add Network so it can be inlcuded in *.rsp file
c                   for GUI (*.net)
c	2006/03/21; Add Plan Well for well augmentation data (*.plw)
c	2006/03/21; Add Rio Grande Forecast (*.rgf)
c
c _________________________________________________________
c
c	Documentation
c
c               iin = response file number
c               infile = 0 standard sequential input
c                       = 1 random input
c               maxfile = max number of files in *.rsp
c
c               filena = first file read
c               fileName = vector of random input file names
c               ifileNum = vector of random input file #'s
c
c              Tyical input is: 'Response = Filename.rsp'
c
c _________________________________________________________
c	Dimensions
       dimension
     1   ifileNum(80), fileName(80), 
     1   filetypX(80), fileType(80),
     1   fileID(80), ifileNx(80), fileSuf(80)

       character filena*256,
     1   filetype*40, filetypX*40, FileName*256, FileId*5,
     1   fileT1*40,   fileT2*40, fileN1*256, fileSuf*5

       data fileID/
     1  '*.rsp', '*.ctl', '*.rin', '*.ris', '*.dds',
     1  '*.ddr', '*.ifs', '*.ifr', '*.wes', '*.wer',
     
     1  '*.res', '*.rer', '*.opr', '*.pre', '*.eva', 
     1  '*.rim', '*.ddm', '*.ddo', '*.dda', '*.ifm', 
     
     2  '*.ifa', '*.wem', '*.dly', '*.tam', '*.eom', 
     1  '*.rib', '*.rih', '*.ddh', '*.weh', '*.sjr',
     
     3  '*.ipy', '*.ddc', '*.par', '*.gis', '*.out',
     1  '*.rid', '*.ddd', '*.ifd', '*.wed', '*.tad',
     
     4  '*.ddx', '*.dld', '*.riy', '*.ddy', '*.wey',
     1  '*.eoy', '*.rbs', '*.ri?', '*.ri?', '*.str',
     
     5  '*.cal', '*.dum', '*.pln', '*.dst', '*.def',
     1  '*.drf', '*.wst', '*.wef', '*.wrf', '*.wde',
     
     6  '*.rgs', '*.net', '*.plw', '*.rgf', '*.rrf',
     1  '*.evm', '*.prm', '*.rig', '*.rie', '*.prf',
     
     7  '*.rch', '*.rst', '*.row', '*.rev', '*.rpp',
     1  '*.rac', '*.rre', '*.dre', '*.plr', '*.rir'/
     
       data filetypX/
     1  'Response',
     2  'Control',
     3  'River_Network',
     4  'StreamGage_Station',
     5  'Diversion_Station', 
     6  'Diversion_Right',
     7  'Instreamflow_Station', 
     8  'Instreamflow_Right',
     9  'Well_Station',      
     1  'Well_Right',
     
     1  'Reservoir_Station',
     2  'Reservoir_Right',
     3  'Operational_Right',
     4  'Precipitation_Monthly',
     5  'Evaporation_Monthly',
     6  'Stream_Base_Monthly',
     7  'Diversion_Demand_Monthly',
     8  'Diversion_DemandOverride_Monthly',
     9  'Diversion_Demand_AverageMonthly',     
     2  'Instreamflow_Demand_Monthly',
     
     1  'Instreamflow_Demand_AverageMonthly',
     2  'Well_Demand_Monthly',
     3  'DelayTable_Monthly',
     4  'Reservoir_Target_Monthly',        
     5  'Reservoir_Historic_Monthly',
     6  'StreamEstimate_Coefficients',
     7  'StreamGage_Historic_Monthly',
     8  'Diversion_Historic_Monthly',
     9  'Well_Historic_Monthly',                
     3  'San_Juan_Recovery',
     
     1  'IrrigationPractice_Yearly',
     2  'ConsumptiveWaterRequirement_Monthly',
     3  'SoilMoisture',                    
     4  'GeographicInformation',
     5  'OutputRequest',                   
     6  'Stream_Base_Daily',
     7  'Diversion_Demand_Daily',
     8  'Instreamflow_Demand_Daily',
     9  'Well_Demand_Daily',
     4  'Reservoir_Target_Daily',
     
     1  'ConsumptiveWaterRequirement_Daily', 
     2  'DelayTable_Daily',
     3  'StreamGage_Historic_Daily',
     4  'Diversion_Historic_Daily',
     5  'Well_Historic_Daily',
     6  'Reservoir_Historic_Daily',               
     7  'StreamEstimate_Station',
     8  'StreamEstimate_Base_Monthly',
     9  'StreamEstimate_Base_Daily',     
     5  'StateCU_Structure',
     
     1  'DownStream_Call',
     2  'Dummy',
     3  'Plan_Data', 
     4  ' ',
     5  'Diversion_Efficiency',
     6  'Diversion_Return',
     7  ' ',
     8  'Well_Efficiency',
     9  'Well_Return',     
     6  'Well_Depletion',     
     
     1  'RioGrande_Spill_Monthly', 
     2  'Network', 
     3  'Plan_Wells',     
     4  'RioGrande_Forecast_Monthly', 
     5  'Reservoir_Return',
     6  'Evaporation_Annual', 
     7  'Precipitation_Annual',
     8  'RiverGage_Structure', 
     9  'RiverEstimate_Structure', 
     7  'Plan_Return',
     
     1  'Reach_Data', 
     2  ' ',
     3  'Reservoir_Owners',
     4  'Reservoir_Evaporation',     
     5  'Reservoir_Precipitation', 
     6  'Reservoir_Area-Capacity',
     7  'Reservoir_Historic_To_Recharge_Monthly',
     8  'Diversion_Historic_To_Recharge_Monthly',
     9  'Plan_Recharge',
     8  'River_Reach'/
     
c
c		Set file number (ifileNx)
c		Use -1 if not currently used 
         data ifileNx/
     1   20, 1,  2, 55,  4,  3, 55,  1,  55, 3,
     1   3,  2, 55,  2,  3,  4, 55, 23, 23, 25,
     2  55,  9, 55, 24, 55, 56,  3,  4,  9, 22, 
     3  10, 14, 55, -1, 22, 81, 82, 83, 84, 85, 
     4  86, 87, 81, 82, 84, 85, -1, -1, -1, 55,
     5  90, -1, 55, 56, 56, 56, 57, 58, 59, 60,
     6  61, -1, 55, 64, 55,  2,  1, 68, 55, 55,
     7  55,  3, 55, 55, 55, 55, 77, 78, 55, 55/
c
c _______________________________________________________ 
c
c               Step 1; Initilize

c
c               iout = 0 no details
c               iout = 1 details
c               iout = 2 summary of files read
c		        iout = 3 summary plus list file types
       iout= 0
       infile=0
       iok=0
       
       nfsize=256
       do i=1,maxfile
         filename(i) = '-1'
         ifilenum(i) = 0
         filetype(i) = '-1'
         fileSuf(i)  = '-1'
       end do
       write(nlog,*) ' Getfn; maxfile = ', maxfile  
       if(iout.eq.1) write(nlog,*) '  Getfn; iin, maxfile = ',
     1   iin, maxfile

c
c _______________________________________________________ 
c
c               Step 2; Random file, read in other files and store
       j1=0
       jp=0

       ! jhb May 2014
       ! summarize the logic
       ! the idea is to loop continuously,
       !   skipping comment lines (and now blank lines), until:
       ! 1. the max possible number of file types has been read
       !    (handled by the max loop index)
       ! 2. end of file is reached, exit gracefully by jumping out of loop
       ! 3. read error - exit abruptly by stopping the program
       do i=1,maxfile
         ! simplify the logic here and make it a bit more
         ! portable across different compilers and platforms
         ! read the line and use the iostatus arg
  100    read(iin,'(a256)',err=9999, end=500) filena
         ! if debugging details are on then read the next
         ! line in the response file and then
         ! output it to the log file
         if(iout.eq.1) then
           write(nlog,*) '  Getfn; i=', i
           ! don't need the next line
           !read(iin,'(a256)',err=9999, end=500) filena
           write(nlog,*) '  Getfn 1; filena, ', filena           
           ! don't need the next line
           !backspace(iin)
         endif
         if (len_trim(trim(filena)).eq.0) then
           ! it's a blank line, which we should skip
           goto 100
         else
           ! it's a non-blank line
           ! check the first char for # comment char
           if (filena(1:1).eq.'#') then
             ! it's a comment line, skip it
             goto 100
           else
             ! don't need the next line
             !call skipn(iin)
             ! don't need the next line
             !read(iin,'(a256)',err=9999, end=500) filena
             ! if debugging details are on then output to the log file
             if(iout.eq.1) write(nlog,*) '  Getfn 2; filena, ', filena
             call getName(i, nlog,nfsize,ifound, filena,fileT1,fileN1)
c
c ---------------------------------------------------------
c               Check for a blank file and/or Sequential input
             if(ifound.eq.0) then
               if(j1.eq.0) write(nlog,300)
               goto 500
             else
               if(j1.eq.0) write(nlog,310)
               if(j1.eq.0 .and. iout.ge.1) write(nlog,202)
             endif
c _________________________________________________________
c
c               Find File Type
             ifound=0
             do j=1,maxfile
               if(fileT1.eq.filetypX(j)) then
                 ifound=1
                 j1=j1+1
                 ifileNum(j) = ifileNx(j)
                 fileName(j) = FileN1
                 fileType(j) = filetypX(j)
                 fileSuf(j) = fileID(j)
cr               if(iout.ge.2) then
                 if(iout.ge.2.and. ifilenum(j).ne.0) then
                   jp=jp+1
                   if(j.eq.4 .or. j.eq.5 .or. j.eq.11) write(nlog,*) ' '
                   if(j.eq.9 .or. j.eq.7 .or. j.eq.13) write(nlog,*) ' '
                   if(j.eq.32 .or. j.eq.53) write(nlog,*) ' '
                   write(nlog,204)  jp, j, ifileNx(j), fileid(j),
     1              ifileNum(j), fileT1, fileN1
                 end if
               endif
             end do
c
c ---------------------------------------------------------
c rrb 2009/03/18; Stop and warn if a file is read but not found
             if(ifound.eq.0) then
               write(nlog,1480) fileT1
               iok=1
               goto 500
             endif

             if(i.eq.1 .and. ifound.eq.0) then
               write(nlog,302)
               goto 500
             else
               infile=1
             endif
c            if(i.gt.1 .and. ifound.eq.0) goto 9998
           endif
         endif
       end do
c
c _______________________________________________________ 
c
c               Step 3; List file types
 500   continue
c      iout=3
       if(iout.eq.3) then
         write(nlog,312)
         write(nlog,203)
         
         do j=1,maxfile
           write(nlog,204)  j, j, ifileNx(j), fileid(j),
     1       ifilenum(j), filetypX(j)
           if(i.eq.4  .or. i.eq.10 .or. i.eq.14) write(nlog,*) ' '
           if(i.eq.17 .or. i.eq.22 .or. i.eq.26) write(nlog,*) ' '
           if(i.eq.30 .or. i.eq.22 .or. i.eq.26) write(nlog,*) ' '
           if(i.eq.32 .or. i.eq.53) write(nlog,*) ' '
     
         enddo
       endif
c
c _______________________________________________________ 
c
c rrb 2009/03/18; Stop if a file is not found
       if(iok.eq.1) goto 1000
c
c _______________________________________________________ 
c
c rrb 2005/10/14; Separate station files
c
c		Check only 1 well station file is provided
       if(ifilenum(9) .gt. 0 .and. ifilenum(57) .gt. 0) then
         write(nlog,*) ' Getfn; 9, 57', ifilenum(9), ifilenum(57)
         fileT1=filetypX(9)
         fileT2=filetypX(57)
         goto 9997
       endif  
c       
c		Check only 1 diversion station file is provided
       if(ifilenum(5) .gt.0 .and. ifilenum(54) .gt. 0) then
         write(nlog,*) ' Getfn; 5, 54', ifilenum(5), ifilenum(54)
         fileT1=filetypX(5)
         fileT2=filetypX(54)
         goto 9997
       endif  
c
c _______________________________________________________ 
c
c               Formats

 200   format(/,
     1 '  Getfn; File # to open = ', i5, /
     1 '         Type           = ', a40,/
     1 '         Name           = ', a256)
 202   format(/,
     1 '  Getfn;',/
     1 '    #    j Fn_1  FnID  Fn_2',
     1 ' Type                                    ', ' Name',/
     1 ' ____ ____ ____ _____ _____',
     1 ' ________________________________________', ' ',  72('_'))
 203   format(/,
     1 '  Getfn;',/
     1 '    #  Fn#  FnID   Fn#',
     1 ' Type                                    ',/
     1 ' ____ ____ _____ _____',
     1 ' ________________________________________')
 204   format(3i5, 1x, a5, 1x, i5, 1x, a40, 1x, a256)
 300   format(/,72('_'),/'  Getfn; Sequential input from *.ctl #1')
 302   format(/,72('_'),/'  Getfn; Sequential input from *.ctl #2')
 310   format(/,72('_'),/'  Getfn; Random input from *.ctl' )
 312   format(/,72('_'),/'  Getfn; Recognized file types for *.ctl' )        
 
 510   return

c
c _________________________________________________________
c
c               Error Tracking
c
 9997 write(99,1470) fileT1, fileT2
      write(6,1440)
      goto 1000
      
 9998 write(99,1460) fileT1,
     1 1, 'Operational_Right', 'Operational_Right_1', 
     1    'Operational_Right_2',
     1 2, 'IrrigationPractice_Yearly', 'IrrigationPractice_Yearly_1',
     1    'IrrigationPractice_Yearly_2',
     1 3, 'Diversion_Station', 'Diversion_Station_1',
     1    'Diversion_Station_2',
     1 4, 'Well_Station',      'Well_Station_1',     
     1    'Well_Station_2',
     1 5, 'Reservoir_Station', 'Reservior_Station_1',
     1    'Reservoir_Station_2'
 
      write(6,1440)
      goto 1000


 9999 write(99,1450) 
      write(6,1440)
      goto 1000

 1440 format(
     1 '  Stopped in getfn, see the log file (*.log)')
 1450 format(
     1 '  Stopped in getfn; Problem reading response (*.rsp) file')
     
 1460 format(/, 72('_'),//
     1 '  Stopped in getfn; File Type not found ', a40,/,
     1 '  Note the following have been revised to identify two',/
     1 '  versions are supported. Recommend you upgrade to the',/
     1 '  new version or select the version that is consistent',/
     1 '  with your data',//, '   ', 
     1 '                             ',
     1 '                 Revised Name',
     1 '                 Revised Name',/, '  #', 
     1 '                     Old Name',
     1 '                   Old Format',
     1 '                   New Format',/
     1 ' __', 3(' ____________________________'),/
     1 20(i3,3(1x,a28),/)) 
     
 1470 format(/,
     1 72('_'),/
     1 '  Stopped in Getfn; Problem cannot provide both',/
     1 '  a standard station file = ', a40,/
     1 '  and a new station file  = ', a40,/
     1 '  Reconmend you pick one and remove the other')
     
 1480 format(/,
     1 72('_'),/
     1 '  Stopped in Getfn;',/
     1 '  Problem cannot find file = ', a40,/
     1 '  Reconmend you revise the response file (*.rsp)')

c
c _________________________________________________________
c               Error Processing

 1000 write(6,1010) 
      write(99,1020) 
      call flush(6)
 1010 format('  Stopped in GetFn')
 1020  format(72('_'),/
     1 '  GetFn; Stopped in GetFn, see the log file (*.log)')
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop 
      END      
      
c
c *********************************************************
c
       subroutine getName(i, nlog, nfsize, ifound,
     1   filena, fileT1, fileN1)

       character filena*256, fileN1*256, fileT1*40

       FileT1=' '
       FileN1=' '
       iout=0
c _______________________________________________________ 
c
c               Step 1; Find file type
c
       ifound=0
       do j=1,nfsize
         if(ifound.eq.0 .and. filena(j:j).eq.'=') then
           ifound=1
           fileT1=filena(1:j-1)
           j1=j+1
           j2=j1
         endif
       end do

c      write(nlog,*) 'j1,j2 ', j1, j2

       if(ifound.eq.0) goto 500

       if(iout.eq.1) then
         write(nlog,*) ' '
         write(nlog,100) i, fileT1
       endif
c _______________________________________________________ 
c
c               Step 2; Find first character of file name
       ifound=0
       do k=j1,nfsize
         if(ifound.eq.0) then
           if(filena(k:k).ne.' ') then
             ifound=1
c            write(nlog,*) ' k=', k
           else
             j2=j2+1
           endif
         endif
       end do
       j1=j2
c      write(nlog,*) 'j1,j2 ', j1, j2
cx

cx
c _______________________________________________________ 
c
c               Step 3; Find file name ending location
       ifound=0
       do k=j1,nfsize
         if(ifound.eq.0) then
           if(filena(k:k+1).eq.'  ') then
             ifound=1
c            write(nlog,*) ' k=', k
           else
             j2=j2+1
           endif
         endif
       end do
c      write(nlog,*) 'j1,j2 ', j1, j2
c
c _______________________________________________________ 
c
c               Step 3c; Set file name to scalar (rec256)
       fileN1 = filena(j1:j2-1)

       if(iout.eq.1) then
         write(nlog,120) i, fileT1, fileN1
       endif
c
c _______________________________________________________ 
c
c               Step 4; Return
 500  return
c
c               Formats
c _______________________________________________________ 
 100  format(
     1 '  GetName; File # Read = ', i5,/,
     1 '           Type        = ', a40)
 120  format(
     1 '  GetName; File # Read = ', i5,/,
     1 '           Type        = ', a40,/,
     1 '           Name        = ', a256)
      stop 
      END

