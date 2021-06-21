c
c *********************************************************
c
       subroutine Getfn2(iin, nlog, nout, infile, maxfile)
c     
c      Getfn2; Subroutine that reads in an
c              old StateMod response file and prints a
c              new StateMod response file
c
c _________________________________________________________
c
c	Documentation
c
c               iin = response file number
c               infile = 0 standard sequential input
c                       = 1 random input
c               maxfile = max number of files in *.rsp

c               filena = first file read
c               fileran = vector of random input files
c              Tyical input is: '*.rsp = .rsp'
c              or               'Response = Filename.rsp'
c
       dimension ifileNum(55), fileName(55), FileType(55),
     1           fileID(55), fileId2(55), ifileNx(55)

       character filena*256,
     1   fileType*40, FileName*256, FileId*5, FileId1*5, fileId2*5,
     1   fileT1*40,   fileN1*256, rec132*132

cx
       data fileID/
     1  '*.rsp', '*.ctl', '*.rin', '*.ris', '*.dds', 
     6  '*.ddr', '*.ifs', '*.ifr', '*.wes', '*.wer',
     
     1  '*.res', '*.rer', '*.opr', '*.pre', '*.eva', 
     6  '*.rim', '*.ddm', '*.ddo', '*.dda', '*.ifm',
     
     2  '*.ifa', '*.wem', '*.dly', '*.tar', '*.eom',
     6  '*.rib', '*.rih', '*.ddh', '*.weh', '*.sjr',
     
     3  '*.tsp', '*.ddc', '*.par', '*.gis', '*.out',
     6  '*.rid', '*.ddd', '*.ifd', '*.wed', '*.tad',
     
     4  '*.ddx', '*.dld', '*.riy', '*.ddy', '*.wey',
     6  '*.eoy', '*.rbs', '*.dum', '     ', '     ',
     
     5  '     ', '     ', '     ', '     ', '     '/
c
c               Alias (old names)
c               rim = xbm
c               out = xou
c               ddc = iwr
c               ddx = iwd
c               riy = rhy
c               dhy = ddy
c               why = wey
c               gis = gvp

       data fileID2/
     1  '*.rsp', '*.ctl', '*.rin', '*.ris', '*.dds',
     6  '*.ddr', '*.ifs', '*.ifr', '*.wes', '*.wer',
     
     2  '*.res', '*.rer', '*.opr', '*.pre', '*.eva', 
     6  '*.xbm', '*.ddm', '*.ddo', '*.dda', '*.ifm',
     
     3 '*.ifa', '*.wem', '*.dly',  '*.tar', '*.eom',
     6 '*.rib', '*.rih', '*.ddh',  '*.weh', '*.sjr',
     
     4 '*.tsp', '*.iwr', '*.par',  '*.gvp', '*.out',
     6 '*.rid', '*.ddd', '*.ifd',  '*.wed', '*.tad',
     
     5  '*.iwd', '*.dld', '*.rhy', '*.dhy', '*.why',
     6  '*.eoy', '*.rbs', '*.dum', '     ', '     ',
     
     6  '     ', '     ', '     ', '     ', '     '/

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
     3  'SanJuanRecovery',
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
     8  'Dummy',
     9   ' ',
     5   ' ',
     1   ' ', ' ', ' ', ' ', ' '/

c
c _______________________________________________________ 
c
c               Step 1; Initilize

       infile=0
       nfsize=256
c
c		iout = 1 detailed output       
c		       2 summary
       iout= 2
       icomment=0
       FileN1=' '

       write(nlog,300)
       write(nout,320)


c
c _______________________________________________________ 
c
c               Step 2; Random file, read in other files and store
c
       j1=0
c      do 100 i=1,maxfile
       do 100 i=1,1000
c
c		Copy comment cards if icomment .ne. 0
         if(icomment.eq.0) then
           call skipn(iin)
         else  
           read(iin,'(a132)',err=9999, end=500) rec132
           if(rec132(1:1).eq.'#') then
             write(nout,'(a132)') rec132         
             goto 100
           else
             backspace(iin)
           endif
         endif  
         
         
         read(iin,'(a256)',err=9999, end=500) filena
         if(iout.eq.1) write(nlog,*) ' Getfn2; filena = ', filena
         call getID(i, nlog,nfsize,ifound, filena,fileID1,fileN1)
c        write(nlog,*) '  Getfn2; ifound', ifound , fileid1

c
c
c               Check for a blank file and/or Sequential input
         if(ifound.eq.0) then
           if(j1.eq.0) write(nlog,310)        
           goto 500
         else
           if(j1.eq.0 .and. iout.ge.2) write(nlog,202)
         endif

c
c               Find File Type
         ifound=0

         do j=1,maxfile
           if(fileId1.eq.FileID(j) .or. fileid1.eq.fileid2(j)) then
             ifound=1
             j1=j1+1
c            write(nlog,*) ' Getfn2; j = ', j
             fileName(j) = FileN1
c
c		Skip if equal to *.dum
             if(j.ne.48) then
               write(nout,206)  fileType(j), fileN1
             endif  
 206         format(a40, '= ', a256)
             if(iout.eq.2) then
               write(nlog,204) j1, fileid1, filetype(j), fileN1
             endif
           endif
         end do
c
c               Print file type if a problem
         if(ifound.eq.0) then
           write(nlog,310) fileID1
         endif
 100   continue        
c
c _______________________________________________________ 
c
c               Formats

 200   format(/,
     1 '  Getfn2; File # to open = ', i5, /
     1 '         Type           = ', a40,/
     1 '         Name           = ', a256)
 202   format(//
     1 '    #  FnID Type                                    ',
     1 ' Name',/
     1 ' ____ _____ ________________________________________',
     1 ' ',  72('_'))
 204   format(i5, 1x, a5, 1x, a40, 1x, a256)
 300   format(/,'   ** Getfn2; Building a New StateMod response file')
 310   format(/,'   ** Getfn2; Problem reading response file type ', a5)
 320   format(
     1  '#',/
     1  '# StateMod Response File',/
     1  '#',/
     1  '# Type', 35x, ' Name'/,
     1  '# ', 38('_'), 2x,72('_'))

c
c               Error Tracking
c _________________________________________________________
 9998 write(nlog,1460) fileId1
      write(6,1440)
      goto 1000


 9999 write(nlog,1450) 
      write(6,1440)
      goto 1000

 1440 format(
     1 '  Stopped in getfn2, see the log file (*.log)')
 1450 format(
     1 '  Stopped in getfn2; Problem reading response (*.rsp) file')
 1460 format(
     1 '  Stopped in getfn2; File ID not found ', a5)

 1000 write(6,*) 'Stop 1'
      call flush(6)
 500  return
      stop
      END
c
c *********************************************************
c
       subroutine getID(i, nlog, nfsize, ifound,
     1   filena, fileID1, FileN1)

       character filena*256, fileN1*256, fileID1*5
c
c _______________________________________________________ 
c
c               Step 1; Find file type

       FileN1=' '
       FileId1='*.    '
       iout=2
c
       ifound=0
       do j=1,nfsize-1
         j1=j+1
c rrb 05/01/10; allow relative paths
c        if(ifound.eq.0 .and. filena(j:j).eq.'.') then
         if(ifound.eq.0 .and. filena(j:j).eq.'.' .and.
     1      filena(j:j1).ne.'..' .and. filena(j:j1).ne.'.\') then
           ifound=1
           fileN1=filena(1:j+4)
           fileId1(3:5)=filena(j+1:j+3)
           j1=j+1
           j2=j1
         endif
       end do

       if(ifound.eq.0) goto 500

       if(iout.eq.1) then
         write(nlog,*) ' '
         write(nlog,100) i, fileId1, fileN1
       endif
c
c               Step 4; Return
 500  return
c
c               Formats
c _______________________________________________________ 
 100  format(
     1 '  GetID;   File #         = ', i5,/,
     1 '           File ID        = ', a5,/
     1 '           File Name      = ', a256)

      stop
      END

