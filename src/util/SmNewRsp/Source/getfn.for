c
c *********************************************************
c
       subroutine Getfn(iin, nlog, infile, maxfile, 
     1   fileName, ifileNum)
c     
c      Getfn; Subroutine that reads in files in any order         
c              and parses thru them to get actual file name
c               Typical input is *.rsp = FileName
c
c _________________________________________________________
c
c      Documentation
c
c               iin = response file number
c               infile = 0 standard sequential input
c                       = 1 random input
c               maxfile = max number of files in *.rsp

c               filena = first file read
c               fileran = vector of random input files
c              Typical input is: '*.rsp = .rsp'
c              or               'Response = Filename.rsp'
c
       dimension ifileNum(55), fileName(55), FileType(55),
     1           fileID(55), ifileNx(55)

       character filena*256,
     1   fileType*40, FileName*256, FileId*5,
     1   fileT1*40,   fileN1*256
     
      
       data fileID/
     1  '*.rsp', '*.ctl', '*.rin', '*.ris', '*.dds', 
     1  '*.ddr', '*.ifs', '*.ifr', '*.wes', '*.wer',
     
     1  '*.res', '*.rer', '*.opr', '*.pre', '*.eva', 
     1  '*.rim', '*.ddm', '*.dda', '*.ddo', '*.ifm',
     
     1  '*.ifa', '*.wem', '*.dly', '*.tar', '*.eom',
     1  '*.rib', '*.rih', '*.ddh', '*.weh', '*.sjr', 
     
     1  '*.tsp', '*.ddc', '*.par', '*.gis', '*.out',
     1  '*.rid', '*.ddd', '*.ifd', '*.wed', '*.tad',
     
     1  '*.ddx', '*.dld', '*.riy', '*.ddy', '*.wey',
     1  '*.eoy', '*.rbs', '*.rbm', '*.rbd', '*.str', 
     
     1  '*.dum', '     ', '     ', '     ', '     '/

       data fileType/
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
     6  'StreamGage_Base_Monthly',
     7  'Diversion_Demand_Monthly',
     8  'Instreamflow_Demand_AverageMonthly',
     9  'Diversion_DemandOverride_Monthly',
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
     3  'SanJuanRecovery',
     1  'IrrigationPractice_Yearly',
     2  'ConsumptiveWaterRequirement_Monthly',
     3  'SoilMoisture',                    
     4  'GeographicInformation',
     5  'OutputRequest',                   
     6  'StreamGage_Base_Daily',
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
     1  'Dummy',
     1   ' ', ' ', ' ', ' '/
c
c
         data ifileNx/
     1   20,  1,  2, 55,  4,  3, 55, 1, 55, 3, 3, 2, 55,
     1   1,   2,  3,
     1   4,  55, 23, 
     1   23, 55,
     1   9,
     1   55,
     1   77,  0,
     1   70,  3,  4,  9,
c
c               San Juan = 15, *.tsp = 10, *.ddc=14, *.par = 55
     1   15, 10, 14, 55,
c
c               Gis = 0?, *.out=22
     1   0, 22,
c
c       *.rid = 81 *.ddd=82, *.wed=84, *.eod=85
     1   81, 82, 84, 85,
c
c       *.ri?=86 *.ri?=87 *.ri?=88 *.str=89
     1  86, 87, 88, 55,     
     1   12*0/     ! Add enough zeros to add up to ifileNx dimension
c
c _______________________________________________________ 
c
c               Step 1; Initialize

       infile=0
       nfsize=256
       iout= 0
c
c _______________________________________________________ 
c
c               Step 2; Random file, read in other files and store
c
       j1=0
       do i=1,maxfile
         call skipn(iin)
         read(iin,'(a256)',err=9999, end=500) filena
         if(iout.eq.1) write(nlog,*) ' Getfn; filena, ', filena
         call getName(i, nlog,nfsize,ifound, filena,fileT1,fileN1)
c
c
c               Check for a blank file and/or Sequential input
         if(ifound.eq.0) then
           if(j1.eq.0) write(nlog,300)        
           goto 500
         else
           if(j1.eq.0) write(nlog,310)
           if(iout.ge.1) write(nlog,202)
         endif

c
c               Find File Type
         ifound=0

         do j=1,maxfile
           if(fileT1.eq.FileType(j)) then
             ifound=1
             j1=j1+1
c            write(nlog,*) ' Getfn; j = ', j
             ifileNum(j) = ifileNx(j)
             fileName(j) = FileN1

             if(iout.ge.2) then
               write(nlog,204)  j1, ifileNx(j), fileid(j),
     1          fileT1, fileN1
             end if
           endif
         end do

         if(i.eq.1 .and. ifound.eq.0) then
           write(nlog,302)        
           goto 500
         else
           infile=1
c          if(i.eq.1) write(nlog,310) 
         endif  
         if(i.gt.1 .and. ifound.eq.0) goto 9998

c        if(i.eq.1 .and. ifound.eq.1) write(nlog,310)
       end do
c
c _______________________________________________________ 
c
c               Step 3; List file types
 500   if(iout.eq.2) then
         write(nlog,312)
         write(nlog,203)
         do j=1,maxfile
           write(nlog,204)  j1, ifileNx(j), fileid(j),
     1       fileType(j)
         enddo
       endif
c
c _______________________________________________________ 
c
c               Formats

 200   format(/,
     1 '  Getfn; File # to open = ', i5, /
     1 '         Type           = ', a40,/
     1 '         Name           = ', a256)
 202   format(//
     1 '    #  Fn#  FnID Type                                    ',
     1 ' Name',/
     1 ' ____ ____ _____ ________________________________________',
     1 ' ',  72('_'))
 203   format(//
     1 '    #  Fn#  FnID Type                                    ',/
     1 ' ____ ____ _____ ________________________________________')
 204   format(2i5, 1x, a5, 1x, a40, 1x, a256)
 300   format(/,'   ** Getfn; Sequential input from *.ctl #1')
 302   format(/,'   ** Getfn; Sequential input from *.ctl #2')
 310   format(/,'   ** Getfn; Random input from *.ctl' )
 312   format(/,'   ** Getfn; Recognized file types for *.ctl' )        
 510   return

c
c               Error Tracking
c _________________________________________________________
 9998 write(99,1460) fileT1
      write(6,1440)
      goto 1000


 9999 write(99,1450) 
      write(6,1440)
      goto 1000

 1440 format(
     1 '  Stopped in getfn, see the log file (*.log)')
 1450 format(
     1 '  Stopped in getfn; Problem reading response (*.rsp) file')
 1460 format(
     1 '  Stopped in getfn; File Type not found ', a40)

 1000 write(6,*) 'Stop 1'
      call flush(6)
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
