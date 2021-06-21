      subroutine smfcNew(nlog, maxnf, iprint, maxsta, 
     1  iday, infile, ifile, fext, ext, desc, fpath)
c
c Note the following character statements are different
c than those in StateMod file getfn.for  (two dimensions)    
c _________________________________________________________
c rrb 2007/12/18; Variables for Random Response File
c
c rrb 2011/08/18; updated to include new files (*.rre, *.dre,
c                 *.plr and *.rir by changng dimension to 80
c                 and replacing dimension statments to match
c                 getfn.for from ./StateM    
       dimension
     1   ifileNum(2,80), fileName(2,80), fileType(2,80),
     1   filetypX(80),   fileID(80), ifileNx(80)
 
       character filena*256,
     1   filetype*40, filetypX*40, FileName*256, FileId*5,
     1   fileT1*40,   fileT2*40, fileN1*256, Fileid1*5, 
     1   filrsp*72,   filetyp1*40
     
       character  fext*4,    fext1*1  
       character  ext(50)*4,  desc(50)*24,  fpath(2)*72 
c
c rrb 2011/08/18; copy data from statemod getfn.for     
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
     4  'Precipitation_Annual',
     5  'Evaporation_Annual',
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
     6  'Evaporation_Monthly', 
     7  'Precipitation_Monthly',
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
                                    
      
c
c _________________________________________________________
c rrb 2007/12/18; Get and Open comparison file 1 
c		  and determine type of response file
c		  If the new (random) file is provided
c		  read each and store in fileName
c
c		Read response file 1   
      iin=1      
      maxfile=80
      nout=0
      
c
c rrb 2011/08/18; Initilize nout (output counter)
      nout=0    
        
      call skipn(5,0)
      read(5,'(a72)', end=1110,err=1120) filrsp
      
      call getpath(nlog, filrsp,fpath(1))

c
c ---------------------------------------------------------
c
c rrb 2008/02/02; Update   
c rrb 2012/03/08; Update to return from opentop2 if *.rsp is not provided
      it=1
      if(fext.ne.'.rsp') then
        write(nlog,*) ' SmFcNew; Opening file = ', 11, filrsp
        open(11, file=filrsp, status='old', err=1120)
c
c rrb 2012/03/08; Initilize infile (new format) so that SmFcOld is
c                 not called in SmFc when a response file (*.rsp) is
c                 not specified.
        infile=1	        
        it=0
      endif  

       
      if(fext.eq.'.rsp') then
        write(nlog,*) ' '
        write(nlog,*) ' SmFcNew; Opening file = ', iin, filrsp
        open(iin, file=filrsp, status='old', err=1120)
        
        nf=1
        call Getfn(nlog, nf, iin, infile, maxfile, 
     1    fileName, ifileNum, filetype, fileID, filetypX)
        close(iin)

        write(nlog,*) ' '      
        write(nlog,*) ' SmFcNew; infile = ', infile
        if(infile.eq.0) then
          write(nlog,*) ' Problem file type not found = ', infile
          goto 1120
        endif  
c
c rrb 2012/03/05 Allow a non response file to be read
      endif
c           
c
c _________________________________________________________
c rrb 2007/12/18; Get and Open file 2 
c		              and determine type of response file
c		              If the new (random) file is provided
c		              read each and store in fileName
      
      call skipn(5,0)
      read(5,'(a72)', end=1110,err=1120) filrsp
      call getpath(nlog, filrsp,fpath(2))
      
      
      if(fext.ne.'.rsp') then
        write(nlog,*) ' SmFcNew; Opening file = ', 12, filrsp
        open(12, file=filrsp, status='old', err=1120)
      endif  
c
c ---------------------------------------------------------
c		Read response file 2  
      nf=2    
      if(fext.eq.'.rsp') then      
        write(nlog,*) ' '
        write(nlog,*) ' SmFcNew; Opening Response file = ', iin, filrsp
        open(iin, file=filrsp, status='old', err=1120)
        call Getfn(nlog, nf, iin, infile, maxfile, 
     1    fileName, ifileNum, filetype, fileID, filetypX)
        close(iin)

        write(nlog,*) ' '      
        write(nlog,*) ' SmFcNew; infile = ', infile
        if(infile.eq.0) goto 500
      endif
c                                                                       
c _________________________________________________________
c               Control file (*.ctl)                                    
	    nf=2
	    if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
	
        call opentop2(nlog, nf, maxnf, it, ifound,
     1    fileID(nf), filetypX(nf), fpath, filename)     
        write(nlog,*) ' SmFcNew; ifound ', ifound
        
        if(ifound.eq.1) then
          nout=nout+1
          call comp1(nlog, nout, maxnf,iprint, fileid(nf))
	        ifile=ifile+1
	      endif  
	    endif
c                                                                       
c _________________________________________________________
c               River Network (*.rin)                                   

   	  nf=3
   	  if(fext.eq.'.rsp' .or. fext.eq.'.rin') then
          call opentop2(nlog, nf, maxnf, it, ifound,
     1      fileID(nf), filetypX(nf), fpath, filename)     

         write(nlog,*) ' SmFcNew; ifound ', ifound        
          if(ifound.eq.1) then 
            write(nlog,*) ' SmFcNew; calling compID ', ifound
            nout=nout+1           
            call compID(nlog, nout, maxnf,maxsta,iprint,0,fileid(nf))
	          ifile=ifile+1
    	  endif
    	endif
c                                                                       
c _________________________________________________________
c               Reservoir Stations (*.res)                              

	    nf=11
	    if(fext.eq.'.rsp' .or. fext.eq.'.res') then
          call opentop2(nlog, nf, maxnf, it, ifound,
     1      fileID(nf), filetypX(nf), fpath, filename)     
     
          if(ifound.eq.1) then 
            nout=nout+1           
            call compSTA(nlog, nout, maxnf,maxsta,iprint,0, fileid(nf))
	        ifile=ifile+1
	      endif
	    endif
c                                                                       
c               Diversion Stations (*.dds)                              
c -------------------------------------------                           
	    nf=5
	    if(fext.eq.'.rsp' .or. fext.eq.'.dds') then
          call opentop2(nlog, nf, maxnf, it, ifound,
     1      fileID(nf), filetypX(nf), fpath, filename)     

          if(ifound.eq.1) then  
            nout=nout+1           
            call compSTA(nlog, nout, maxnf,maxsta,iprint,2, fileid(nf))
            ifile=ifile+1
          endif  
	    endif
c                                                                       
c               River Stations (*.ris)                                  
c -------------------------------------------                           
	    nf=4
	    if(fext.eq.'.rsp' .or. fext.eq.'.ris') then
          call opentop2(nlog, nf, maxnf, it, ifound,
     1      fileID(nf), filetypX(nf), fpath, filename)     

          if(ifound.eq.1) then 
            nout=nout+1           
	        call compID(nlog, nout, maxnf,maxsta,iprint,0, fileid(nf))
	        ifile=ifile+1
	      endif
	    endif
c                                                                       
c               Instream Stations (*.ifs)                               
c -------------------------------------------                           
	    nf=7
	    if(fext.eq.'.rsp' .or. fext.eq.'.ifs') then
          call opentop2(nlog, nf, maxnf, it, ifound, 
     1      fileID(nf), filetypX(nf), fpath, filename, fileid(nf))     

          if(ifound.eq.1) then
            nout=nout+1           
      	    call compID(nlog, nout, maxnf,maxsta,iprint,0, fileid(nf))
      	    ifile=ifile+1
      	  endif
      	endif
c
c               Well Stations (*.wes)
c -------------------------------------------                           
	     nf=9
c
c rrb 03/01/30
      if(fext.eq.'.rsp' .or. fext.eq.'.wes') then
        call opentop2(nlog, nf, maxnf, it, ifound,
     1    fileID(nf), filetypX(nf), fpath, filename)     
c
        if(ifound.ge.1) then
          nout=nout+1         
          call compSTA(nlog, nout, maxnf,maxsta,iprint,2, fileid(nf))
          ifile=ifile+1
        endif
      endif  
c                                                                       
c               Instream rights (*.ifr)                                 
c -------------------------------------------                           
	    nf=8
	    if(fext.eq.'.rsp' .or. fext.eq.'.ifr') then
        call opentop2(nlog, nf, maxnf, it, ifound,
     1    fileID(nf), filetypX(nf), fpath, filename)     

        if(ifound.eq.1) then
          nout=nout+1 
          call compID(nlog, nout, maxnf,maxsta,iprint,0, fileid(nf))
          ifile=ifile+1
        endif
      endif
c                                                                       
c               Reservoir rights (*.rer)                                
c -------------------------------------------                           
	    nf=12
	    if(fext.eq.'.rsp' .or. fext.eq.'.rer') then
          call opentop2(nlog, nf, maxnf, it, ifound,
     1      fileID(nf), filetypX(nf), fpath, filename)     

        if(ifound.eq.1) then 
          nout=nout+1        
          call compID(nlog, nout, maxnf,maxsta,iprint,0, fileid(nf))
          ifile=ifile+1
        endif
      endif
c                                                                       
c               Diversion rights (*.ddr)                                
c -------------------------------------------                           
	    nf=6
	    if(fext.eq.'.rsp' .or. fext.eq.'.ddr') then
        call opentop2(nlog, nf, maxnf, it, ifound,
     1    fileID(nf), filetypX(nf), fpath, filename)     

        if(ifound.eq.1) then
          nout=nout+1        
      	  call compID(nlog, nout, maxnf,maxsta,iprint,0, fileid(nf))
      	  ifile=ifile+1
      	endif
	    endif
c                                                                       
c               Operation rights (*.opr)                                
c -------------------------------------------                           
	    nf=13
	    if(fext.eq.'.rsp' .or. fext.eq.'.opr') then
        call opentop2(nlog, nf, maxnf, it, ifound,
     1    fileID(nf), filetypX(nf), fpath, filename)     
c
        if(ifound.eq.1) then
          nout=nout+1        
          call compSTA(nlog, nout, maxnf,maxsta,iprint,3, fileid(nf))
	        ifile=ifile+1
	      endif
	    endif
c
c               Well right (*.wer)
c -------------------------------------------                           
	    nf=10
c
      if(fext.eq.'.rsp' .or. fext.eq.'.wer') then
        call opentop2(nlog, nf, maxnf, it, ifound,
     1    fileID(nf), filetypX(nf), fpath, filename)     
c

        if(ifound.eq.1) then
c
c rrb 2011/09/14; Add option to skip well rights                
          nskipW=0
	        if(nskipW.eq.1) then         
	          write(26,300)	        
 300        format(/,
     1      '  SmFcNew; FYI The well right file (*.wer) is not being',/
     1      '  compared because the same ID may be associated with',/
     1      '  two or more records.  Recommend you do a visual check')
          else
            nout=nout+1        
            call compID(nlog, nout, maxnf,maxsta,iprint,0, fileid(nf))
	          ifile=ifile+1
	        endif
	      endif
      endif
c                                                                       
c               Precipitation (*.pre)                                   
c -------------------------------------------                           
	    nf=14
	    if(fext.eq.'.rsp' .or. fext.eq.'.pre') then
         call opentop2(nlog, nf, maxnf, it, ifound,
     1     fileID(nf), filetypX(nf), fpath, filename)     

         if(ifound.eq.1) then
          nout=nout+1         
	        call compT(nlog, nout, maxnf,maxsta,iprint,0, fileid(nf))
	        ifile=ifile+1
	      endif
	    endif
c                                                                       
c               Evaporation (*.eva)                                     
c -------------------------------------------                           
	    nf=15
	    if(fext.eq.'.rsp' .or. fext.eq.'.eva') then
        call opentop2(nlog, nf, maxnf, it, ifound,
     1    fileID(nf), filetypX(nf), fpath, filename)     

        if(ifound.eq.1) then 
          nout=nout+1        
      	  call compID(nlog, nout, maxnf,maxsta,iprint,0, fileid(nf))
      	  ifile=ifile+1
      	endif
      endif
c                                                                       
c               River Baseflows (*.rim)
c -------------------------------------------                           
	    nf=16
	    if(fext.eq.'.rsp' .or. fext.eq.'.rim') then
        call opentop2(nlog, nf, maxnf, it, ifound,
     1    fileID(nf), filetypX(nf), fpath, filename)     
	
        if(ifound.eq.1) then
          nout=nout+1        	
          call compT(nlog, nout, maxnf,maxsta,iprint,0, fileid(nf))
          ifile=ifile+1
        endif
      endif
c                                                                       
c               Diversion Demand-Monthly (*.ddm)                        
c -------------------------------------------                           
	    nf=17
	    if(fext.eq.'.rsp' .or. fext.eq.'.ddm') then
          call opentop2(nlog, nf, maxnf, it, ifound,
     1      fileID(nf), filetypX(nf), fpath, filename)     

        if(ifound.eq.1) then
          nout=nout+1        
          call compT(nlog, nout, maxnf,maxsta,iprint,0, fileid(nf))
          ifile=ifile+1
        endif
      endif
c                                                                       
c               Diversion Override (*.ddo)                              
c -------------------------------------------                           
	    nf=18
	    if(fext.eq.'.rsp' .or. fext.eq.'.ddo') then
        call opentop2(nlog, nf, maxnf, it, ifound,
     1    fileID(nf), filetypX(nf), fpath, filename)     

        if(ifound.eq.1) then
           nout=nout+1       
          call compID(nlog, nout, maxnf,maxsta,iprint,0, fileid(nf))
          ifile=ifile+1
        endif
      endif
c                                                                       
c               Diversion Demand-Annual (*.dda)                         
c -------------------------------------------                           
	    nf=19
	    if(fext.eq.'.rsp' .or. fext.eq.'.dda') then
        call opentop2(nlog, nf, maxnf, it, ifound,
     1    fileID(nf), filetypX(nf), fpath, filename)     
     
        if(ifound.eq.1) then
          nout=nout+1        
          call compID(nlog, nout, maxnf,maxsta,iprint,0, fileid(nf))
          ifile=ifile+1
        endif
      endif
c
c               Instream Demand-Monthly (*.ifm)
c -------------------------------------------                           
	    nf=20
c
c rrb 03/01/30
      if(fext.eq.'.rsp' .or. fext.eq.'.ifm') then
        call opentop2(nlog, nf, maxnf, it, ifound,
     1    fileID(nf), filetypX(nf), fpath, filename)     

        if(ifound.eq.1) then
          nout=nout+1        
          call compT(nlog, nout, maxnf,maxsta,iprint,0, fileid(nf))
          ifile=ifile+1
        endif
      endif
c
c               Instream Demand-Annual (*.ifa)                          
c -------------------------------------------                           
	    nf=21
	    if(fext.eq.'.rsp' .or. fext.eq.'.ifa') then
        call opentop2(nlog, nf, maxnf, it, ifound,
     1    fileID(nf), filetypX(nf), fpath, filename)     

        if(ifound.eq.1) then
          nout=nout+1       
          call compT(nlog, nout, maxnf,maxsta,iprint,0, fileid(nf))
          ifile=ifile+1
        endif
      endif
c
c               Well Demand (*.wem)
c -------------------------------------------                           
	    nf=22
        if(fext.eq.'.rsp' .or. fext.eq.'.wem') then
          call opentop2(nlog, nf, maxnf, it, ifound,
     1      fileID(nf), filetypX(nf), fpath, filename)     
  	  
        if(ifound.eq.1) then 
          nout=nout+1        
          call compT(nlog, nout, maxnf,maxsta,iprint,0, fileid(nf))
      	  ifile=ifile+1
        endif
	    endif
c                                                                       
c               Delay Table (*.dly or *.urm)                                     
c -------------------------------------------                           
	    nf=23
c 
c rrb 2011/09/04; Update
cx    if(fext.eq.'.rsp' .or. fext.eq.'.dly') then
      if(fext.eq.'.rsp' .or. fext.eq.'.dly' .or. 
     1  fext.eq.'.urm') then	    
        call opentop2(nlog, nf, maxnf, it, ifound,
     1    fileID(nf), filetypX(nf), fpath, filename)     
	
        if(ifound.eq.1) then
          nout=nout+1 
c
c S Platte       
cx        call compSTA(nlog, nout, maxnf,maxsta,iprint,1, fileid(nf))
          call compURM(nlog, nout, maxnf,maxsta,iprint,1, fileid(nf))
         
          ifile=ifile+1
        endif
      endif
c                                                                       
c               Reservoir Target (*.tar)                                
c -------------------------------------------                           
	    nf=24
	    if(fext.eq.'.rsp' .or. fext.eq.'.tar') then
        call opentop2(nlog, nf, maxnf, it, ifound,
     1    fileID(nf), filetypX(nf), fpath, filename)     
	
        if(ifound.eq.1) then
          nout=nout+1        
	       call compT(nlog, nout, maxnf,maxsta,iprint,1, fileid(nf))
        endif
c
c             Check max (is=-1)
        if(ifound.eq.1) then
          nout=nout+1        
      	  call compT(nlog, nout, maxnf,maxsta,iprint,-1, fileid(nf))
      	  ifile=ifile+1
      	endif
      endif
c
c               San Juan Sediment File (*.sjr)
c -------------------------------------------
	    nf=30
c
      if(fext.eq.'.rsp' .or. fext.eq.'.sjr') then
          call opentop2(nlog, nf, maxnf, it, ifound,
     1      fileID(nf), filetypX(nf), fpath, filename)     
	  
          if(ifound.eq.1) then
          nout=nout+1          
            call compT(nlog, nout, maxnf,maxsta,iprint,0, fileid(nf))
	        ifile=ifile+1
	      endif
      endif
c
c               Annual Time Series File (*.ipy)
c -------------------------------------------                           
	    nf=31
      if(fext.eq.'.rsp' .or. fext.eq.'.tsp') then
        call opentop2(nlog, nf, maxnf, it, ifound,
     1    fileID(nf), filetypX(nf), fpath, filename)     
  	  
c
c rrb 03/01/30
        if(ifound.eq.1) then
          nout=nout+1        
          call compT(nlog, nout, maxnf,maxsta,iprint,0, fileid(nf))
	        ifile=ifile+1
        endif
	    endif
c
c               Annual Water Requirement (IWR) File (*.ddc)
c -------------------------------------------                           
	    nf=32
c
      if(fext.eq.'.rsp' .or. fext.eq.'.iwr') then
        call opentop2(nlog, nf, maxnf, it, ifound,
     1    fileID(nf), filetypX(nf), fpath, filename)     
	  
        if(ifound.eq.1) then
          nout=nout+1        
          call compT(nlog, nout, maxnf,maxsta,iprint,0, fileid(nf))
          ifile=ifile+1
        endif
      endif
c
c               Soil Parameter Data (*.par)
c -------------------------------------------                           
	    nf=33
c
      if(fext.eq.'.rsp' .or. fext.eq.'.par') then
        call opentop2(nlog, nf, maxnf, it, ifound,
     1    fileID(nf), filetypX(nf), fpath, filename)     
	  
        if(ifound.eq.1) then
          nout=nout+1        
          call compID(nlog, nout, maxnf,maxsta,iprint,0, fileid(nf))
          ifile=ifile+1
        endif
      endif
c
c               Reservoir EOM (*.eom)                                   
c -------------------------------------------                           
	    nf=25
	    if(fext.eq.'.rsp' .or. fext.eq.'.eom') then
        call opentop2(nlog, nf, maxnf, it, ifound,
     1    fileID(nf), filetypX(nf), fpath, filename)     
	
        if(ifound.eq.1) then
          nout=nout+1        
          call compT(nlog, nout, maxnf,maxsta,iprint,0, fileid(nf))
          ifile=ifile+1
        endif
      endif
c                                                                       
c               River Baseflow Info (*.rib)                             
c -------------------------------------------                           
	    nf=26
	    if(fext.eq.'.rsp' .or. fext.eq.'.rib') then
        call opentop2(nlog, nf, maxnf, it, ifound,
     1    fileID(nf), filetypX(nf), fpath, filename)     
	
        if(ifound.eq.1) then
          nout=nout+1        
          call compSTA(nlog, nout, maxnf,maxsta,iprint,0, fileid(nf))
          ifile=ifile+1
        endif
      endif
c                                                                       
c               Historical Streamflow (*.rih)                           
c -------------------------------------------                           
	    nf=27
	    if(fext.eq.'.rsp' .or. fext.eq.'.rih') then
        call opentop2(nlog, nf, maxnf, it, ifound,
     1    fileID(nf), filetypX(nf), fpath, filename)     
	
        if(ifound.eq.1) then
          nout=nout+1        
     	    call compT(nlog, nout, maxnf,maxsta,iprint,0, fileid(nf))
     	    ifile=ifile+1
     	  endif
     	endif
c                                                                       
c               Historical Diversions (*.ddh)                           
c -------------------------------------------                           
	    nf=28
	    if(fext.eq.'.rsp' .or. fext.eq.'.ddh') then
        call opentop2(nlog, nf, maxnf, it, ifound,
     1      fileID(nf), filetypX(nf), fpath, filename)     
	
        if(ifound.eq.1) then
          nout=nout+1        
	        call compT(nlog, nout, maxnf,maxsta,iprint,0, fileid(nf))
	        ifile=ifile+1
	      endif
	    endif
c
c               Historical Well Pumping (*.weh)
c -------------------------------------------                           
	    nf=29
      if(fext.eq.'.rsp' .or. fext.eq.'.weh') then
        call opentop2(nlog, nf, maxnf, it, ifound,
     1      fileID(nf), filetypX(nf), fpath, filename)     
c
          if(ifound.eq.1) then      
            nout=nout+1          
            call compT(nlog, nout, maxnf,maxsta,iprint,0, fileid(nf))
	         ifile=ifile+1
	       endif
      endif
c  
c _________________________________________________________ 
c rrb 2012/03/08; Update for new files                                                                    
c               GIS Data (*.cal)
	    nf=34
	    if(fext.eq.'.rsp' .or. fext.eq.'.gis') then             
        call opentop2(nlog, nf, maxnf, it,ifound, 
     1    fileID(nf), filetypX(nf), fpath, filename)     
	
        if(ifound.ge.1) then
          nout=nout+1        
          write(nlog,510) fext, adjustl(filetypX(nf))
          write(26,510) fext, adjustl(filetypX(nf))          
	        ifile=ifile+1
	      endif  
	    endif
c     
c _________________________________________________________                                                                  
c               Output Control (*.out)
c 
	    nf=35
	    if(fext.eq.'.rsp' .or. fext.eq.'.out') then 
        call opentop2(nlog, nf, maxnf, it,ifound, 
     1      fileID(nf), filetypX(nf), fpath, filename) 
           
        if(ifound.eq.1) then
	        nskipO=1     
	        if(nskipO.eq.1) then         
	          write(26,310)	        
 310        format(/,
     1      '  SmFcNew; FYI The output file (*.out) is not being',/
     1      '  compared because the same ID may be associated with',/
     1      '  two or more records and it is not a data input file ',/
     1      '  (it is an output control file)',/)
          else            
            nout=nout+1        
            call compID(nlog, nout, maxnf,maxsta,iprint,1,fileid(nf))
	          ifile=ifile+1
	        endif
	      endif
	    endif
c  
c _________________________________________________________ 
c rrb 2011/08/18; Update for new files                                                                    
c               StateCU Station file (*.str)
	    nf=50
	    if(fext.eq.'.rsp' .or. fext.eq.'.str') then             
        call opentop2(nlog, nf, maxnf, it,ifound, 
     1    fileID(nf), filetypX(nf), fpath, filename)     
	
        if(ifound.ge.1) then
          nout=nout+1        
          call compSTA(nlog, nout, maxnf,maxsta,iprint,2, fileid(nf))
	        ifile=ifile+1
	      endif  
	    endif
c  
c _________________________________________________________ 
c rrb 2012/03/08; Update for new files                                                                    
c               Call Data (*.cal)
	    nf=51
	    if(fext.eq.'.rsp' .or. fext.eq.'.cal') then             
        call opentop2(nlog, nf, maxnf, it,ifound, 
     1    fileID(nf), filetypX(nf), fpath, filename)     
	
        if(ifound.ge.1) then
          nout=nout+1        
          write(nlog,510) fext, adjustl(filetypX(nf))
          write(26,510) fext, adjustl(filetypX(nf))          
	        ifile=ifile+1
	      endif  
	    endif	    
c  
c _________________________________________________________                                                                      
c               Plan Stations (*.pln)
c 
	    nf=53
	    if(fext.eq.'.rsp' .or. fext.eq.'.pln') then             
        call opentop2(nlog, nf, maxnf, it,ifound, 
     1    fileID(nf), filetypX(nf), fpath, filename)     
	
        if(ifound.ge.1) then 
          nout=nout+1        
          call compSTA(nlog, nout, maxnf,maxsta,iprint,2, fileid(nf))
	        ifile=ifile+1
	      endif  
	    endif
c     
c _________________________________________________________                                                                   
c               Rio Grande Spill (*.rgs)                           
c                   
	    nf=61
	    if(fext.eq.'.rsp' .or. fext.eq.'.rgs') then
        call opentop2(nlog, nf, maxnf, it, ifound,
     1    fileID(nf), filetypX(nf), fpath, filename)     
	
        if(ifound.eq.1) then
          nout=nout+1        
     	    call compT(nlog, nout, maxnf,maxsta,iprint,0, fileid(nf))
     	    ifile=ifile+1
     	  endif
     	endif
c  
c _________________________________________________________ 
c rrb 2012/03/08; Update for new files                                                                    
c               Network File (*.net)
	    nf=62
	    if(fext.eq.'.rsp' .or. fext.eq.'.net') then             
        call opentop2(nlog, nf, maxnf, it,ifound, 
     1    fileID(nf), filetypX(nf), fpath, filename)     
	
        if(ifound.ge.1) then
          nout=nout+1        
          
          write(nlog,510) fext, adjustl(filetypX(nf))
          write(26,510) fext, adjustl(filetypX(nf))           
	        ifile=ifile+1
	      endif  
	    endif	
c
c _________________________________________________________ 
c               Plan Well (*.plw)
c                           
	    nf=63
c
      if(fext.eq.'.rsp' .or. fext.eq.'.plw') then
        call opentop2(nlog, nf, maxnf, it, ifound,
     1    fileID(nf), filetypX(nf), fpath, filename)     
	  
        if(ifound.eq.1) then
          nout=nout+1        
          call compID(nlog, nout, maxnf,maxsta,iprint,0, fileid(nf))
          ifile=ifile+1
        endif
      endif	    
c  
c _________________________________________________________ 
c rrb 2011/08/18; Update for new files                                                                    
c               Reservoir Return (*.rrf)
	    nf=65
	    if(fext.eq.'.rsp' .or. fext.eq.'.rrf') then             
        call opentop2(nlog, nf, maxnf, it,ifound, 
     1    fileID(nf), filetypX(nf), fpath, filename)     
	
        if(ifound.ge.1) then
          nout=nout+1        
          call compSTA(nlog, nout, maxnf,maxsta,iprint,2, fileid(nf))
	        ifile=ifile+1
	      endif  
	    endif
c  
c _________________________________________________________ 
c rrb 2012/03/08; Update for new files                                                                    
c               Monthly Evaporation (*.evm)
c
	    nf=66
	    if(fext.eq.'.rsp' .or. fext.eq.'.evm') then             
        call opentop2(nlog, nf, maxnf, it,ifound, 
     1    fileID(nf), filetypX(nf), fpath, filename)     
	
          if(ifound.eq.1) then
            nout=nout+1          
            call compT(nlog, nout, maxnf,maxsta,iprint,0, fileid(nf))
	         ifile=ifile+1        
	        
	      endif	  
	    endif
c  
c _________________________________________________________ 
c rrb 2012/03/08; Update for new files                                                                    
c               Monthly PPt (*.prm)
c
	    nf=67
	    if(fext.eq.'.rsp' .or. fext.eq.'.prm') then             
        call opentop2(nlog, nf, maxnf, it,ifound, 
     1    fileID(nf), filetypX(nf), fpath, filename)     
	
          if(ifound.eq.1) then
            nout=nout+1          
            call compT(nlog, nout, maxnf,maxsta,iprint,0, fileid(nf))
	         ifile=ifile+1        
	        
	      endif	  
	    endif	    
c 
c _________________________________________________________                                                                      
c               Plan Return (*.prf)
c 
	    nf=70
	    if(fext.eq.'.rsp' .or. fext.eq.'.prf') then             
        call opentop2(nlog, nf, maxnf, it,ifound, 
     1    fileID(nf), filetypX(nf), fpath, filename)     
	
        if(ifound.ge.1) then
          nout=nout+1        
          call compSTA(nlog, nout, maxnf,maxsta,iprint,2, fileid(nf))
	        ifile=ifile+1
	      endif	  
	    endif
c  
c _________________________________________________________ 
c rrb 2012/03/08; Update for new files                                                                    
c               Reach Data (*.rch)
	    nf=71
	    if(fext.eq.'.rsp' .or. fext.eq.'.rch') then             
        call opentop2(nlog, nf, maxnf, it,ifound, 
     1    fileID(nf), filetypX(nf), fpath, filename)     
	
        if(ifound.ge.1) then
          nout=nout+1        
          call compSTA(nlog, nout, maxnf,maxsta,iprint,2, fileid(nf))
	        ifile=ifile+1
	      endif  
	    endif
c  
c _________________________________________________________ 
c rrb 2011/08/18; Update for new files                                                                    
c               Reservoir historic to recharge (*.rre)
c
	    nf=77
	    if(fext.eq.'.rsp' .or. fext.eq.'.rre') then             
        call opentop2(nlog, nf, maxnf, it,ifound, 
     1    fileID(nf), filetypX(nf), fpath, filename)     
	
          if(ifound.eq.1) then
            nout=nout+1          
            call compT(nlog, nout, maxnf,maxsta,iprint,0, fileid(nf))
	         ifile=ifile+1        
	        
	      endif	  
	    endif
c  
c _________________________________________________________ 
c rrb 2011/08/18; Update for new files                                                                    
c               Plan Recharge (*.dre)
	    nf=78
	    if(fext.eq.'.rsp' .or. fext.eq.'.dre') then             
        call opentop2(nlog, nf, maxnf, it,ifound, 
     1  fileID(nf), filetypX(nf), fpath, filename)     
	
        if(ifound.eq.1) then
          nout=nout+1        
          call compT(nlog, nout, maxnf,maxsta,iprint,0, fileid(nf))
	        ifile=ifile+1
	      endif  
	    endif
c  
c _________________________________________________________ 
c rrb 2011/08/18; Update for new files                                                                    
c               Plan Recharge (*.plr)
	    nf=79
	    if(fext.eq.'.rsp' .or. fext.eq.'.plr') then             
        call opentop2(nlog, nf, maxnf, it,ifound, 
     1    fileID(nf), filetypX(nf), fpath, filename)     
	
        if(ifound.ge.1) then
          nout=nout+1        
          call compSTA(nlog, nout, maxnf,maxsta,iprint,2, fileid(nf))
	        ifile=ifile+1
	      endif  
	    endif
c  
c _________________________________________________________ 
c rrb 2011/08/18; Update for new files                                                                    
c               River Reach (*.rir)
	    nf=80
	    if(fext.eq.'.rsp' .or. fext.eq.'.rir') then             
        call opentop2(nlog, nf, maxnf, it,ifound, 
     1    fileID(nf), filetypX(nf), fpath, filename)     
	
        if(ifound.ge.1) then
          nout=nout+1        
          call compSTA(nlog, nout, maxnf,maxsta,iprint,2, fileid(nf))
	        ifile=ifile+1
	      endif  
	    endif
c
c
c _________________________________________________________
c		Return if iday = 0	
c       if(iday.eq.0) goto 500
c
c _________________________________________________________ 
c               Daily Streamflow (*.rid)
c 
	    nf=36
      if(fext.eq.'.rsp' .or. fext.eq.'.rid') then
        call opentop2(nlog, nf, maxnf, it, ifound,
     1    fileID(nf), filetypX(nf), fpath, filename)     
  	  
         if(ifound.eq.1) then
          nout=nout+1         
	        call compT(nlog, nout, maxnf,maxsta,iprint,0, fileid(nf))
	        ifile=ifile+1
	      endif
      endif
c
c               Daily Demand (*.ddd)
c -------------------------------------------
	    nf=37
      if(fext.eq.'.rsp' .or. fext.eq.'.ddd') then
          call opentop2(nlog, nf, maxnf, it, ifound,
     1      fileID(nf), filetypX(nf), fpath, filename)     
  	  
         if(ifound.eq.1) then
          nout=nout+1         
	        call compT(nlog, nout, maxnf,maxsta,iprint,0, fileid(nf))
	        ifile=ifile+1
	      endif
      endif
c
c               Daily ISF Demand (*.ifd)
c -------------------------------------------
	    nf=38
  	  if(fext.eq.'.rsp' .or. fext.eq.'.ifd') then
          call opentop2(nlog, nf, maxnf, it, ifound,
     1      fileID(nf), filetypX(nf), fpath, filename)     
  	  
         if(ifound.eq.1) then
          nout=nout+1         
	        call compT(nlog, nout, maxnf,maxsta,iprint,0, fileid(nf))
	        ifile=ifile+1
	      endif
      endif
c
c               Daily Well Demand (*.wed)
c -------------------------------------------
	    nf=39
c
 	    if(fext.eq.'.rsp' .or. fext.eq.'.wed') then
          call opentop2(nlog, nf, maxnf, it, ifound,
     1      fileID(nf), filetypX(nf), fpath, filename)     
  	  
        if(ifound.eq.1) then
          nout=nout+1        
          call compT(nlog, nout, maxnf,maxsta,iprint,0, fileid(nf))
	        ifile=ifile+1
	      endif
      endif
c
c               Daily Res target (*.tad)
c -------------------------------------------
	    nf=40
  	  if(fext.eq.'.rsp' .or. fext.eq.'.tad') then
           call opentop2(nlog, nf, maxnf, it, ifound,
     1        ext(nf), desc(nf), fpath, filename)     
  	  
        if(ifound.eq.1) then
          nout=nout+1        
	       call compT(nlog, nout, maxnf,maxsta,iprint,0, fileid(nf))
	       ifile=ifile+1
	      endif
      endif
c
c               Daily Delay Table (*.dld)
c -------------------------------------------
	    nf=42
  	  if(fext.eq.'.rsp' .or. fext.eq.'.dld') then
         call opentop2(nlog, nf, maxnf, it, ifound,
     1        ext(nf), desc(nf), fpath, filename)     
  	  
        if(ifound.eq.1) then
          nout=nout+1        
  	      call compSTA(nlog, nout, maxnf,maxsta,iprint,1, fileid(nf))
	        ifile=ifile+1
	      endif
      endif
c
c               Daily IWR (*.ddx)
c -------------------------------------------
	      nf=41
c
        if(fext.eq.'.rsp' .or. fext.eq.'.ddx') then
          call opentop2(nlog, nf, maxnf, it, ifound,
     1      fileID(nf), filetypX(nf), fpath, filename)     
  	  
c
         if(ifound.eq.1) then
          nout=nout+1         
           call compT(nlog, nout, maxnf,maxsta,iprint,0, fileid(nf))
	        ifile=ifile+1
	      endif
      endif
c
c               Daily Historic Stream (*.riy)
c -------------------------------------------
	     nf=43
      if(fext.eq.'.rsp' .or. fext.eq.'.rih') then
        call opentop2(nlog, nf, maxnf, it, ifound,
     1    fileID(nf), filetypX(nf), fpath, filename)     
  	  
        if(ifound.eq.1) then
          nout=nout+1        
	       call compT(nlog, nout, maxnf,maxsta,iprint,0, fileid(nf))
	       ifile=ifile+1
	      endif
      endif
c
c               Daily Historic Diversion (*.ddy)
c -------------------------------------------
	    nf=44
    	if(fext.eq.'.rsp' .or. fext.eq.'.ddy') then
        call opentop2(nlog, nf, maxnf, it, ifound,
     1    fileID(nf), filetypX(nf), fpath, filename)     
  	  
        if(ifound.eq.1) then
          nout=nout+1        
	       call compT(nlog, nout, maxnf,maxsta,iprint,0, fileid(nf))
	       ifile=ifile+1
	      endif
      endif
c
c               Daily Historic Well Pumping (*.wey)
c -------------------------------------------
	    nf=45
c
  	  if(fext.eq.'.rsp' .or. fext.eq.'.wey') then
        call opentop2(nlog, nf, maxnf, it, ifound,
     1    fileID(nf), filetypX(nf), fpath, filename)     
  	  
        if(ifound.eq.1) then
          nout=nout+1        
          call compT(nlog, nout, maxnf,maxsta,iprint,0, fileid(nf))
	       ifile=ifile+1
	      endif
      endif
c
c               Daily Historic Reservoir EOY (*.eoy)
c -------------------------------------------
	    nf=46
       if(fext.eq.'.rsp' .or. fext.eq.'.eoy') then
         call opentop2(nlog, nf, maxnf, it, ifound,
     1     fileID(nf), filetypX(nf), fpath, filename)     
  	  
         if(ifound.eq.1) then
          nout=nout+1         
	        call compT(nlog, nout, maxnf,maxsta,iprint,0, fileid(nf))
	        ifile=ifile+1
	      endif
      endif

c
c _________________________________________________________ 
c               Output File (*.x**)
c 
	    fext1=fext(2:2)
	    if(fext1.eq.'x' .or. fext1.eq.'X') then
c
c rrb 2011/08/18; Initilize infile (new format) and nout 
c                 (output counter)
        infile=1	    
	      fileid1='Output File'
	      write(6,200) fext
	      write(nlog,200) fext	      
 200    format('  SmFcNew; Processing an output file ',a4)
 
        nout=nout+1        
	      call compOut(nlog, nout, maxnf, iprint, fext)  
	      ifile=ifile+1
	    endif
	
 400	return
	    stop 0
	    
 510      format(/,
     1	    '  Warning file comparison cannot be performed ',
     1      'for file type = ', a4,', ', a40,/
     1      '  Because it is not ID specific')

c
c _________________________________________________________
 1110 write(6,*)  '  SmFcNew; Unsuccessful termination, see smfc.log'
      write(nlog,*)'  SmFcNew; Unsuccessful termination, see smfc.log'
	    goto 500
     
 1120 write(6,*) ' '
	    write(6,*) '  SmFcNew: Problem opening file ', filrsp
	    write(nlog,*) ' '
	    write(nlog,*) '  SmFcNew: Problem opening file ', filrsp
	    goto 500

     
c
c _________________________________________________________
 500  call flush(6)
	    stop 1
	    end                                                             
