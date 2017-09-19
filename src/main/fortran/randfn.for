c
c *********************************************************
c
c
       subroutine randfn(iin, nlog, iinput, filena, fileran)
c
c
c _________________________________________________________
c	Program Description
c
c      Randfn; It reads in files in any order         
c              and parses thru them to get actual file name
c              and store results in variable fileran
c              Note files are ordered the same as the StateMod
c              documentaion
c
c              Tyical input is: '*.rsp = FileName.rsp'
c              or               'Response: Filename.rsp'
c
c _________________________________________________________
c       Documentation
c               iin = response file number
c               nlog = log output file number
c               iinput = 0 standard sequential input
c                        1 random input
c               filena = first file read
c               fileran = vector of random input files
c
c
c _________________________________________________________
c	Dimensions
c
       dimension fileran(100), fileID(100), fileId2(100)
       character filena*256, fileran*72, fileid*5, fileid2*40,
     1  rec5*5, rec40*40, rec72*72
      
       data fileID/
     1  '*.rsp', '*.ctl', '*.rin', '*.ris', 
     1  '*.dds', '*.ddr', '*.ifs', '*.ifr',
     1  '*.wes', '*.wer', '*.res', '*.rer',
     1  '*.opr', '*.pre', '*.eva', 
     1  '*.rim', '*.ddm', '*.dda', '*.ddo',
     1  '*.ifm', '*.ifa', '*.wem', '*.dly',
     1  '*.tar', 
     1  '*.eom', '*.rib', '*.rih', '*.ddh',
     1  '*.weh',
     1  '*.sjr', '*.tsp', '*.ddc', '*.par',
     1  '*.gis', '*.out',
     1  '*.rid', '*.ddd', '*.ifd', '*.wed', '*.tad',
     1  '*.ddx', '*.dld',
     1  '*.riy', '*.ddy', '*.wey', '*.eoy',
     1  '     ', '     ', '     ', '*.str',
     1  '*.dum', 49*'    '/

       data fileID2/
     1  'Response',
     2  'Control',
     3  'River_Network',
     4  'River_Station',
     5  'Diversion_Station',     'Diversion_Right',
     7  'InstreamFlow_Station',  'InstreamFlow_Right',
     9  'Well_Station',          'Well_Right',
     1  'Reservoir_Station',     'Reservoir_Right',
     3  'Operational_Right',
     4  'Precipitation',
     5  'Evaporation',
     6  'Streamflow_Monthly',
     7  'DirectFlow_Demand_Monthly',
     8  'DirectFlow_Demand_Annual',
     9  'DirectFlow_Override',
     2  'InstreamFlow_Demand_Monthly',
     1  'InstreamFlow_Demand_Annual',
     2  'Well_Demand_Monthly',
     3  'Delay_Table_Monthly',
     4  'Reservoir_Target_Monthly',

     5  'Historic_Reservoir_EOM_Monthly',
     6  'Base_Streamflow',
     7  'Historic_Streamflow_Monthly',
     8  'Historic_Diversion_Monthly',
     9  'Historic_Well_Pumping_Monthly',

     3  'San_Juan_Recovery',
     1  'CU_Time_Series',
     2  'Consumptive_Water_Requirement_Monthly',
     3  'Soil_Moisture',

     4  'Geographic_Information',
     5  'Output_Request',

     6  'Streamflow_Daily',
     7  'Diversion_Demand_Daily',
     8  'InstreamFlow_Demand_Daily',
     9  'Well_Demand_Daily',
     4  'Reservoir_Target_Daily',
     1  'Consumptive_Water_Requirement_Daily', 
     2  'Delay_Table_Daily',
     3  'Historic_Streamflow_Daily',
     4  'Historic_Diversion_Daily',
     5  'Historic_Well_Pumping_Daily',
     6  'Historic_Reservoir_EOM_Monthly',          
     7  '     ',
     8  '     ', 
     9  '     ', 
     5 'StateCU_Structure',
     1 'Dummy',
     1  49*'    '/
c
c _________________________________________________________
c               Step 1. Read first file and check for a random read
c
       maxfn=1000
       iinput=0
       iout=0
c
       read(iin, '(a256)', err=9999, end=500) filena
       if(iout.eq.1) write(nlog,'(a256)') filena

       do i=1,maxfn
         rec5=filena(1:5)
         rec40=filena(1:40)
         do j=1,40
           if(rec40(j:j).eq.':') rec40(j:j) = ' '
         end do

         if(rec5.eq.fileid(i) .or. rec40.eq.fileid2(i)) then
	   iinput=1
c          fileran(i) = filena(1:72)
	 endif
       end do
c
c               Print to log file
       write(nlog,*) ' ' 
       if(iinput.eq.0) then
         write(nlog,*) '  ** Randfn; Sequential input from *.ctl' 
       else  
         write(nlog,*) '  ** Randfn; Random input from *.ctl'         
c
c               Step 2; Random file, read in all files and store
c _______________________________________________________
         rewind(iin)
	 do 10 ii=1,maxfn
 11        read(iin, '(a256)', err=9999, end=500) filena
           if(filena(1:1) .eq. '#') goto 11

           if(iout.ge.2) write(nlog,'(a256)') filena


	   rec5=filena(1:5)

           rec40=filena(1:40)
           do j=1,40
             if(rec40(j:j).eq.':') rec40(j:j) = ' '
           end do

	   ifound = 0
	   do i=1,maxfn
             if(rec5.eq.fileid(i) .or. rec40.eq.fileid2(i)) then
                if(iout.ge.2) then
                  write(nlog,106) 1, i, fileid(i), fileid2(i), filena
                endif

	       ifound=1
c
c               Step 3; Find character '=' or ':'
c _______________________________________________________ 
               ifound2=0
               do j=1,256
                 if(filena(j:j).eq.'=' .or. filena(j:j).eq.':') then
		   j1=j
		   j2=j
                   ifound2=1
                 endif
               end do

               if(ifound2.eq.0) then
                 write(nlog,110)  i, fileid(i)
                 goto 500
               endif

c
c                      Step 3x; Find first non blank after '='
               ifound3=0
               do k=j1+1,256
                 if(filena(k:k).eq.' ' .and. ifound3.eq.0) then
                   j2=j2+1
                 else
                   ifound3=1
                 endif
               end do
c
c                       Step 3c; Set file name to scalar (rec72)
	       rec72=''     
               do m=1, 72
                 j2=j2+1
                 rec72(m:m) = filena(j2:j2)
c
c                       Find end file based on 2 blank characters
                 if(filena(j2:j2+1) .eq. '  ') then
c
c                       Step 3c; Finally set file name to vector of 
c                                file names (fileran())
                    fileran(i) = rec72
                    if(iout.ge.2) then
                      write(nlog,106) 2, i, fileid(i),fileid2(i),
     1                     fileran(i)
                    endif
                    goto 10
                 endif
               end do
	     endif
c
c               End found file type loop
	   end do
           if(ifound.eq.0) write(nlog,120) filena
c
c               End file name loop
 10      continue                    
       endif

c _______________________________________________________ 
c
c               Step 4; Check
c500  if(iout.eq.1) then
 500  write(nlog,102)
      do i=1,maxfn
        write(nlog,104) i, fileid(i), fileid2(i), fileran(i)
      end do
c     endif
c _______________________________________________________ 
c
c               Step 5; Return
      return
c _______________________________________________________ 
c
c               Formats
 100  format(
     1 '  Randfn; ',i5, ' File # ', i5, ' Type = ', a5,
     1  ' Description : ', a40, ' Name ', a72)
 102  format(
     1 '  Randfn; Random file read results',//
     1 '    # Type  Description                            ',1x, 
     1 'Name ',/,
     1 ' ____ _____ _______________________________________',1x,
     1  72('_'))
 104  format(i5, 1x, a5, 1x, a40, 1x, a72)
 106  format(2i5, 1x, a5, 1x, a40, 1x, a72)
 110  format(
     1 '  Randfn; File # ', i5, ' type ', a5, 'needs an = or a :')
 120  format(
     1 '  Randfn; Warning file type not found for:', a72,/)
c _______________________________________________________ 
c
c               Error Tracking

 9999 write(6,1440) 
      write(nlog,1450) 
      call flush(6)
 1440 format('  Stopped in Randfn, see the log file (*.log)')
 1450 format('  Stopped in Randfn')
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop 
      END

