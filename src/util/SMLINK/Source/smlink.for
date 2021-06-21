c
c
C     Last change:  RB   27 Jun 98   12:10 pm
C                                                                       
C      Smlink.f 
c              Code to generate a linked file for up to 5               
c              Statemod input files
c
c _________________________________________________________
c	Version Descriptions
c       1.01 (05/31/96) handles time series with different years
c       1.03 (06/28/96) handles *.out and *.gis & *.dly is file 1
c       1.04 (12/06/96) added starib for big *.rib files
c       1.05 (98/06/29) Added command lines to delete, etc.
c            (98/10/21) Revised input from file 5 to 95 to 
c                       work for 5 files
c       1.06 (00/10/03) Added logic to handle *.ifm which lead 
c                       to misc edits to subroutine timec
c	1.07 (04/06/16) Added logic to read instream data type 
c		        *.rsp and change file names to lower case
c	2.01 2007/07/10 Revised to read new (command line) 
c                         response file format (nver >= 10)
c			Revised file size to 250 for new 
c			  operational right information
c			Allow comments in operational right file
c			Revised to allow 1 or more files to not exist
c			  in the files to be linked
c			Added Well Station, Right, Demand, &
c			  Historic Pumping 
c			Added Plan Station and Return Flow
c			Added Irrigation Practice (*.ipy)
c			Added Consumptive Water Requirement (*.ddc)
c			Added San Juan Release
c			Added Daily Data
c
c !!!   See subroutines parse, getpath and putpath for
c       Conversion to work
c
c               nd         = # of command line edits
c               fndel()*72 = file name to be edited
c               iddel()*12 = ID to be edited
c               ndel()     = file # to be edited
c               idtyp()    = Edit type 1=delsta, 2=addrec, 3=repzero
c
c		maxnf  Number of basins to link (input files
c		       to read
c
c _________________________________________________________
c rrb 2007/07/10; Update
       dimension
     1   ifileNum(5,80), fileName(5,80), fileType(5,80)
       character 
     1   filetype*40, FileName*72, rec5*5

        dimension ndel(200), idtyp(200)
        character filenc*72,  filout*72, fpath(5)*72,
     1            fname(5)*72,ext(80)*4,  desc(80)*40,
     1            iddel(200)*12, fndel(200)*72, recnew(200)*160,
     1            vdate*10, cifm*1
        data fndel/200*' '/

        data ext/
     1  '.rsp', '.ctl', '.rin', '.ris', '.dds',
     1  '.ddr', '.ifs', '.ifr', '.wes', '.wer',
     
c    1  '.res', '.rer', '.opr', '.pre', '.eva',
     1  '.res', '.rer', '.opr', '.prm', '.evm',      
     1  '.rim', '.ddm', '.ddo', '.dda', '.ifm', 
     
     2  '.ifa', '.wem', '.dly', '.tam', '.eom', 
     1  '.rib', '.rih', '.ddh', '.weh', '.sjr',
     
     3  '.ipy', '.ddc', '.par', '.gis', '.out',
     1  '.rid', '.ddd', '.ifd', '.wed', '.tad',
     
     4  '.ddx', '.dld', '.riy', '.ddy', '.wey',
     1  '.eoy', '.rbs', '.ri?', '.ri?', '.str',
     
     5  '.cal', '.dum', '.pln', '.dst', '.def',
     1  '.drf', '.wst', '.wef', '.wrf', '.wde',
     
     6  '.rgs', '.net', '.plw', '.rgf', '.rrf',
c    1  '.evm', '.prm', '.rig', '.rie', '.prf',
     1  '.eva', '.pra', '.rig', '.rie', '.prf',
     
     
     7  '.rch', '.rst', '.row', '.rev', '.rpp',
     1  '.rac', '.rre', '.dre', '.plr', '.rir'/
     
        data desc/
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
c               Initilize
c
       ver = 3.01
       vdate = '2011/11/08'
       
       nver=11
       iderr=0  
       maxnf=5
       iwarn=0
       nlog=99
c
c rrb 2011/08/29; Update       
       maxfile=80       
       iout=0
c                                                                       
c
c _________________________________________________________
c               Open files                                              
       open(99,file='smlink.log', status='unknown')
       open(98,file='smlink.dim', status='unknown')
c                                                                       
c _________________________________________________________
c               Get control (.rsp) file
       call parse(filenc)                     
c
c
c _________________________________________________________
c               Check for simple -version options
       if(filenc(1:2).eq. '-v') then
         write(6,120) ver, vdate
         call flush(6)
         goto 112
       endif
c
c
c _________________________________________________________
c               Check for simple -help option
       if(filenc(1:2).eq. '-h') then
         write(6,130)
         call flush(6)
         goto 112
       endif

       open(95, file=filenc, status='old')                               
c                                                                       
c _________________________________________________________
c               Get output file name                                    
c       write(6,*) ' Gettin output file name'
       call skipn(95,0)  
       read(95,'(a72)',end=150,err=150) filout
       ix = 0                                                           
       do 100 i=1,72
         if(filout(i:i).ne.' ') ix=ix+1                                 
  100  continue                                                         
c
c _________________________________________________________
c rrb 2011/09/24; Get file control variables
       call skipn(95,0)  
       read(95,*,end=148,err=148) nEva, nPre, nStr, nIpy, 
     1   nDdc, nDly, nUnit
       write(nlog,*) ' Constant files',
     1    neva, nPre, nStr, nIpy, nDdc, nDly, nUnit
c
c _________________________________________________________
c                                                                       
c               Get response file names                                 
       call opener(maxnf,fpath)
       write(nlog,*) ' SmLink; Number of Files to Link (maxnf) = ', 
     1   maxnf         
c _________________________________________________________
c               Determine if files have a .rim or not
c
c rrb 04/06/16; Read from control file (file 5 may be used already)
c      write(6,*) '  Are Monthly ISF files (*.ifm) included (y or n)'
c      read(5,'(a1)') cifm
cx      read(95,'(a1)') cifm
       cifm='y'
       write(nlog,*) ' SmLink; Monthly ISF files Provided = ', cifm
       if(cifm.eq.'Y') cifm = 'y'
       if(cifm.eq.'N') cifm = 'n'       
c
c _________________________________________________________
c                                                                       
c               Get commands for editing
        call getcom(nd,fndel,idtyp,iddel,recnew)
c
c _________________________________________________________
c rrb 2007/07/10; Update
c		  Read file names for every basin to be linked
c		  Note reads new, command line, response file
         do m=1,maxnf 
           iin=m
           call Getfn(iin, nlog, infile, maxfile, m, 
     1     fileName, ifileNum, filetype)        
         end do
c
c _________________________________________________________
c                                                                       
c               Response file (*.rsp)                                   
        nf=1        
        nfin=1
c        if(iout.eq.1) then
c          write(nlog,*) '  SmLink; Process response file (*.rsp)'
c        endif
c
c
c rrb 2007/07/11; Update
        write(nlog,*) '  Smlink; nver = ', nver
        if(nver.lt.10) then
          call opentop(nlog, nver, nfin,
     1      iwarn,ix,maxnf,0,filout,ext(nf),desc(nf),
     1      fpath,fname, filename)
          call rsp(ix, filout, ext, desc) 
        else
          filout(ix+1:ix+4) = ext(nf) 
          write(nlog,*) '  Smlink; ', nf, filout                                                 
          open(97, file=filout, status='unknown')  
          write(97,146) ext(nf), desc(nf), filout                                         
        endif
c                                                                       
c
c _________________________________________________________
c               Control file (*.ctl) 2                             

        nf=nf+1
        nfin=nf
        call opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1    fpath,fname, filename)
        call control(maxnf,moneva,iybeg,iyend,nUnit,filout)
c                                                                       
c
c _________________________________________________________
c               River Network (*.rin) 3                                  
        nf=nf+1
        nfin=nf
        call opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1    fpath,fname, filename)
        call getfnX(maxnf,nd,ndel,fndel,fname)
        call rin(nlog,maxnf,iderr,nd,ndel,idtyp,iddel,filout,recnew)
c
c _________________________________________________________
c               River Stations (*.ris) 4                                 
        nf=nf+1
        nfin=nf
        call opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1    fpath,fname, filename)      
        call getfnX(maxnf,nd,ndel,fndel,fname)
        call station(nlog,nfin,maxnf,iderr,0,3,1,nd,ndel,
     1               idtyp,iddel,filout,recnew)
c                                                                       
c
c _________________________________________________________
c               Reservoir Stations (*.res)   
c
c rrb 2011/09/17; Read later                           
cx        nf=nf+1
cx        nfin=nf
cx        call opentop(nlog, nver, nfin,
cx     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
cx     1    fpath,fname, filename)
cx        call getfnX(maxnf,nd,ndel,fndel,fname)
cx        call station(nlog,nfin,maxnf,iderr,0,3,1,nd,ndel,
cx     1               idtyp,iddel,filout,recnew)
c                                                                       
c
c _________________________________________________________
c               Diversion Stations (*.dds) 5                             
        nf=nf+1
        nfin=nf
        call opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1    fpath,fname, filename)
        call getfnX(maxnf,nd,ndel,fndel,fname)
c
c rrb 98/10/20; test problem when n = 100%
c       call station(nlog,nfin,maxnf,iderr,0,3,1,nd,ndel,
        call station(nlog,nfin,maxnf,iderr,0,2,1,nd,ndel,
     1               idtyp,iddel,filout,recnew)
c
c rrb 98/06/29
c        do i=1,nd
c          write(99,112) i,fndel(i),i, iddel(i),i,ndel(i)
c        end do
c 112    format('  Main2    fndel  ',i5,' = ', a72,/,
c     1         '           iddel  ',i5,' = ', a12,/,
c     1         '            ndel  ',i5,' = ', i5)
c       goto 160
c                                                                       
c                                                                       
c
c _________________________________________________________
c rrb 2011/09/19 Add diversion rights
c               Diversion rights (*.wer) 6                                
        nf=nf+1
        nfin=nf
        call opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1    fpath,fname, filename)       
        call getfnX(maxnf,nd,ndel,fndel,fname)
        call station(nlog,nfin,maxnf,iderr,0,3,1,nd,ndel,
     1               idtyp,iddel,filout,recnew)

c                                                                       
c
c _________________________________________________________
c               Instream Stations (*.ifs) 7                              
        nf=nf+1
        nfin=nf
        call opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1    fpath,fname, filename)      
        call getfnX(maxnf,nd,ndel,fndel,fname)
        call station(nlog,nfin,maxnf,iderr,0,3,1,nd,ndel,
     1               idtyp,iddel,filout,recnew)
c                                                                       
c
c _________________________________________________________
c               Instream rights (*.ifr) 8                                
        nf=nf+1
        nfin=nf
        call opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1    fpath,fname, filename)       
        call getfnX(maxnf,nd,ndel,fndel,fname)
        call station(nlog,nfin,maxnf,iderr,0,3,1,nd,ndel,
     1               idtyp,iddel,filout,recnew)
c                                                                       
c
c _________________________________________________________
c rrb 2007/07/11; Update
c               Well Stations (*.wes) 9                              
        nf=nf+1
        nfin=nf
        call opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1    fpath,fname, filename)      
        call getfnX(maxnf,nd,ndel,fndel,fname)
        call station(nlog,nfin,maxnf,iderr,0,3,1,nd,ndel,
     1               idtyp,iddel,filout,recnew)
c                                                                       
c
c _________________________________________________________
c rrb 2007/07/11; Update
c               Well rights (*.wer) 10                                
        nf=nf+1
        nfin=nf
        call opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1    fpath,fname, filename)       
        call getfnX(maxnf,nd,ndel,fndel,fname)
        call station(nlog,nfin,maxnf,iderr,0,3,1,nd,ndel,
     1               idtyp,iddel,filout,recnew)
c                                                                       
c
c _________________________________________________________
c               Reservoir station (*.res) 11                               
        nf=nf+1
        nfin=nf
cx       write(nlog,*) ' SmLink; processing 11 *.res' , nf, desc(nf)                    
        call opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1    fpath,fname, filename)
        call getfnX(maxnf,nd,ndel,fndel,fname)
        call station(nlog,nfin,maxnf,iderr,0,3,1,nd,ndel,
     1               idtyp,iddel,filout,recnew)
c                                                                       
c
c _________________________________________________________
c               Reservoir rights (*.rer) 12  
                            
        nf=nf+1
        nfin=nf
cx        write(nlog,*) ' SmLink; processing 12 *.rer' , nf, desc(nf)             
        call opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1    fpath,fname, filename)
        call getfnX(maxnf,nd,ndel,fndel,fname)
        call station(nlog,nfin,maxnf,iderr,0,3,1,nd,ndel,
     1               idtyp,iddel,filout,recnew)
c                                                                       
c
c _________________________________________________________
c               Operation rights (*.opr)  13   
                          
        nf=nf+1
        nfin=nf
cx        write(nlog,*) ' SmLink; processing 13 *.opr', nf, desc(nf)        
        call opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1    fpath,fname, filename)
        call getfnX(maxnf,nd,ndel,fndel,fname)
        call station(nlog,nfin,maxnf,iderr,0,3,1,nd,ndel,
     1               idtyp,iddel,filout,recnew)
c                                                                       
c
c _________________________________________________________
c               Precipitation (*.prm) 14
c               Note see file 67 for *.pra                                  
        nf=nf+1
        nfin=nf
        if(moneva.eq.0) then
          if(nEva.eq.0) then
            write(99,141)
            write(26,141)
          endif
                
          call opentop(nlog, nver, nfin,
     1      iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1      fpath,fname, filename)        
          call timec(nlog,maxnf,iderr,0,iybeg,iyend,nd,ndel,
     1               nver,nfin,idtyp,iddel,filout,filename)
        endif
c                                                                       
c
c _________________________________________________________
c               Evaporation Monthly (*.evm) 15
c               Note see file 66 for *.eva
c
c rrb 2011/10/15; Correction moneva=0=monthly data, moneva=1=constant data
cx      write(nlog,*) '  Smlink; moneva ', moneva
c
        nf=nf+1
        nfin=nf
        if(moneva.eq.0) then                                   
          if(nEva.eq.0) then
            write(99,140)
            write(26,140)
          endif

 
          call opentop(nlog, nver, nfin,
     1      iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1      fpath,fname, filename)
          call timec(nlog,1,iderr,0,iybeg,iyend,nd,ndel,
     1               nver,nfin,idtyp,iddel,filout,filename)
        endif
c                                                                       
c
c _________________________________________________________
c               River Baseflows (*.rim) 16
        nf=nf+1
        nfin=nf
        call opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1    fpath,fname, filename)
c
c rrb 98/07/28
        call getfnX(maxnf,nd,ndel,fndel,fname)
c
c rrb; Test
        call timec(nlog,maxnf,iderr,0,iybeg,iyend,nd,ndel,
     1             nver,nfin,idtyp,iddel,filout,filename)
c                                                                       
c
c _________________________________________________________
c               Diversion Demand-Monthly (*.ddm)  17                      
        nf=nf+1
        nfin=nf
        call opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1    fpath,fname, filename)
c
c rrb 98/07/28
        call getfnX(maxnf,nd,ndel,fndel,fname)
        call timec(nlog,maxnf,iderr,0,iybeg,iyend,nd,ndel,
     1             nver,nfin,idtyp,iddel,filout,filename)
c                                                                       
c
c _________________________________________________________
c               Diversion Override (*.ddo) 18                              
        nf=nf+1
        nfin=nf
        call opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1    fpath,fname, filename)
        call getfnX(maxnf,nd,ndel,fndel,fname)
        call station(nlog,nfin,maxnf,iderr,1,17,1,nd,ndel,
     1               idtyp,iddel,filout,recnew)
c                                                                       
c
c _________________________________________________________
c               Diversion Demand-Annual (*.dda) 19                        
        nf=nf+1
        nfin=nf
        call opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1    fpath,fname, filename)
        call getfnX(maxnf,nd,ndel,fndel,fname)
        call station(nlog,nfin,maxnf,iderr,1,17,1,nd,ndel,
     1               idtyp,iddel,filout,recnew)

c                                                                       
c
c _________________________________________________________
c rrb 00/09/14; Add ability for monthly ISF 20
c               Instream Flow Demand-Monthly (*.ifm)                        
        if(cifm.eq.'y') then
          nf=nf+1
          nfin=nf
          call opentop(nlog, nver, nfin,
     1      iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1      fpath,fname, filename)
          call getfnX(maxnf,nd,ndel,fndel,fname)
          call timec(nlog,maxnf,iderr,0,iybeg,iyend,nd,ndel,
     1               nver,nfin,idtyp,iddel,filout,filename)
        endif
c                                                                       
c
c _________________________________________________________
c               Instream Demand-Annual (*.ifa)  21                        
        nf=nf+1
        nfin=nf
        call opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1    fpath,fname, filename)
        call getfnX(maxnf,nd,ndel,fndel,fname)
        call station(nlog,nfin,maxnf,iderr,1,17,1,nd,ndel,
     1               idtyp,iddel,filout,recnew)
c                                                                       
c
c _________________________________________________________
c rrb 2007/07/11; Update
c               Well Demand-Monthly (*.wem)   22                     
        nf=nf+1
        nfin=nf
        call opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1    fpath,fname, filename)
c
c rrb 98/07/28
        call getfnX(maxnf,nd,ndel,fndel,fname)
        call timec(nlog,maxnf,iderr,0,iybeg,iyend,nd,ndel,
     1             nver,nfin,idtyp,iddel,filout,filename)
     
c                                                                       
c
c _________________________________________________________
c               Delay Table (*.dly or *.urm)    23                                 
        nf=nf+1
        nfin=nf
        call opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1    fpath,fname, filename)
     

        call getfnX(maxnf,nd,ndel,fndel,fname)
c
c rrb 2011/09/24; Add ability to combine *.dly files       
        if(nDly.eq.0) then
          write(99,145)
          write(26,145)        
          call station(nlog,nfin,1,iderr,0,12,1,nd,ndel,
     1                 idtyp,iddel,filout,recnew)
        else
          call station(nlog,nfin,maxnf,iderr,0,12,1,nd,ndel,
     1                 idtyp,iddel,filout,recnew)                  
        endif
c                                                                       
c
c _________________________________________________________
c               Reservoir Target (*.tar or *.tam) 24                                
        nf=nf+1
        nfin=nf
        call opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1    fpath,fname, filename)
c
c rrb 98/07/28
        call getfnX(maxnf,nd,ndel,fndel,fname)
        call timec(nlog,maxnf,iderr,1,iybeg,iyend,nd,ndel,
     1             nver,nfin,idtyp,iddel,filout,filename)
c                                                                       
c
c _________________________________________________________
c               Reservoir EOM (*.eom) 25                                  

        nf=nf+1
        nfin=nf
        call opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1    fpath,fname, filename)
c
c rrb 98/07/28
        call getfnX(maxnf,nd,ndel,fndel,fname)
        call timec(nlog,maxnf,iderr,0,iybeg,iyend,nd,ndel,
     1             nver,nfin,idtyp,iddel,filout,filename)
c
c
c _________________________________________________________
c               River Baseflow Info (*.rib) 26                            
        nf=nf+1
        nfin=nf
        call opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1    fpath,fname, filename)
c       call getfnX(maxnf,nd,ndel,fndel,fname)
c       call station(nlog,nfin,maxnf,iderr,0,3,1,nd,ndel,
c    1    iddel,filout,recnew)
        call starib(maxnf,iderr,3,filout)
c                                                                       
c
c _________________________________________________________
c               Historical Streamflow (*.rih)  27                         
        nf=nf+1
        nfin=nf
        call opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1    fpath,fname, filename)
c
c rrb 98/07/28
        call getfnX(maxnf,nd,ndel,fndel,fname)
        call timec(nlog,maxnf,iderr,0,iybeg,iyend,nd,ndel,
     1             nver,nfin,idtyp,iddel,filout,filename)

         
c                                                                       
c
c _________________________________________________________
c               Historical Diversions (*.ddh)  28                         
        nf=nf+1
        nfin=nf
        call opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1    fpath,fname, filename)
c
c rrb 98/07/28
        call getfnX(maxnf,nd,ndel,fndel,fname)
        call timec(nlog,maxnf,iderr,0,iybeg,iyend,nd,ndel,
     1             nver,nfin,idtyp,iddel,filout,filename)
c                                                                       
c
c _________________________________________________________
c rrb 2007/07/11; Update
c               Historical Well Pumping (*.weh)  29                         
        nf=nf+1
        nfin=nf
        call opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1    fpath,fname, filename)
c
c rrb 98/07/28
        call getfnX(maxnf,nd,ndel,fndel,fname)
        call timec(nlog,maxnf,iderr,0,iybeg,iyend,nd,ndel,
     1             nver,nfin,idtyp,iddel,filout,filename)
c
c _________________________________________________________
c rrb 2007/07/11; Update
c               San Juan Release (*.sjr)  30                             
        nf=nf+1
        nfin=nf
        call opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1    fpath,fname, filename)      
        call getfnX(maxnf,nd,ndel,fndel,fname)
        call station(nlog,nfin,maxnf,iderr,0,3,1,nd,ndel,
     1               idtyp,iddel,filout,recnew)
c
c _________________________________________________________
c rrb 2007/07/11; Update
c               Irrigation Practice (*.ipy)  31                             
        nf=nf+1
        nfin=nf
        call opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1    fpath,fname, filename)

        call getfnX(maxnf,nd,ndel,fndel,fname)
c
c ---------------------------------------------------------
c rrb 2011/09/24; Add ability to skip file compare and simply copy
c                 the first file to the output file        
        if(nIpy.eq.0) then
          write(99,143)
          write(26,143)
        
          i3=12
          rec5='*.ipy'
          call FnCopy(nlog, maxnf, iderr, i3, rec5, filout) 
        else
          call timec(nlog,maxnf,iderr,0,iybeg,iyend,nd,ndel,
     1             nver,nfin,idtyp,iddel,filout,filename)
        endif
c
c _________________________________________________________
c rrb 2007/07/11; Update
c               Consumptive Water Requirement (*.ddc)   32                        
        nf=nf+1
        nfin=nf
        call opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1    fpath,fname, filename)
c
c rrb 98/07/28
        call getfnX(maxnf,nd,ndel,fndel,fname)
c
c ---------------------------------------------------------
c rrb 2011/09/24; Add ability to skip file compare        
        if(nDdc.eq.0) then
          write(99,144)
          write(26,144)
          i3=12
          rec5='*.ddc'
          call FnCopy(nlog, maxnf, iderr, i3, rec5, filout) 
        else        
          call timec(nlog,maxnf,iderr,0,iybeg,iyend,nd,ndel,
     1             nver,nfin,idtyp,iddel,filout,filename)
        endif
c
c _________________________________________________________
c rrb 2007/07/11; Update
c               StateCU Parameter File (*.par)    33                           
        nf=nf+1
        nfin=nf
        call opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1    fpath,fname, filename)      
        call getfnX(maxnf,nd,ndel,fndel,fname)
        call station(nlog,nfin,maxnf,iderr,0,3,1,nd,ndel,
     1               idtyp,iddel,filout,recnew)
     
c                                                                       
c
c _________________________________________________________
c               GIS (*.gis)      34                                       
        nf=nf+1
        nfin=nf
        iwarn=0
        call opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1    fpath,fname, filename)
        if(iwarn.eq.1) call gis(maxnf,2)
c                                                                       
c
c _________________________________________________________
c               Output (*.out)   35                                          
        nf=nf+1
        nfin=nf
        iwarn=0
c       call opentop(nlog, nver, nfin,
c    1    0,ix,maxnf,1,filout,ext(nf),desc(nf),
c    1    fpath,fname, filename)
        call opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1    fpath,fname, filename)
c       write(99,144)
c       write(26,144)
        if(iwarn.eq.1) call gis(maxnf,3)
c
c rrb Add daily files here as necessary
c _________________________________________________________
c		Daily Stream (*.rid) 36
         nf=nf+1
c _________________________________________________________
c		Daily Diversion demand (*.ddd) 37
         nf=nf+1
c _________________________________________________________
c		Daily ISF demand (*.ifd) 38
         nf=nf+1
c _________________________________________________________
c		Daily Well demand (*.wed)    39    
         nf=nf+1
c _________________________________________________________
c		Daily Reservoir targets (*.tad) 40
         nf=nf+1
c _________________________________________________________
c		Daily IWR (*.ddx) 41
         nf=nf+1
c _________________________________________________________
c		Daily delay (*.dld or urd) 42
         nf=nf+1
c _________________________________________________________
c		Daily historic streamflow (*.riy) 43
         nf=nf+1
c _________________________________________________________
c		Daily historic diversion (*.ddy) 44
         nf=nf+1
c _________________________________________________________
c		Daily historic pumping (*.wey) 45
         nf=nf+1
c _________________________________________________________
c		Daily reservoir EOD (*.eoy) 46
         nf=nf+1
c _________________________________________________________
c		*.rbs 47
         nf=nf+1
c _________________________________________________________
c		*.ri? 48
         nf=nf+1
c _________________________________________________________
c		*.ri? 49
         nf=nf+1
c _________________________________________________________
c rrb 2007/07/11; Update
c               StateCU Structure File (*.str)   50                            
        nf=nf+1
        nfin=nf
        call opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1    fpath,fname, filename)      
        call getfnX(maxnf,nd,ndel,fndel,fname)
        
c
c ---------------------------------------------------------
c rrb 2011/09/24; Add ability to skip file compare  and copy file 1 to the output      
        if(nStr.eq.0) then
          write(99,142)
          write(26,142)
          
          i3=12
          rec5='*.str'
          call FnCopy(nlog, maxnf, iderr, i3, rec5, filout) 
        else            
          call staStr(maxnf, iderr, 3, filout)
        endif
     
c
c _________________________________________________________
c rrb 2007/07/11; Update
c               Call Data (*.cal) 51                              
        nf=nf+1
        nfin=nf
        call opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1    fpath,fname, filename)      
        call getfnX(maxnf,nd,ndel,fndel,fname)
        call station(nlog,nfin,maxnf,iderr,0,3,1,nd,ndel,
     1               idtyp,iddel,filout,recnew)
c
c _________________________________________________________
c rrb 2007/07/11; Update 52
c		Dummy (*.dum)
         nf=nf+1
c
c _________________________________________________________
c rrb 2007/07/11; Update
c               Plan Data (*.pln)  53                             
        nf=nf+1
        nfin=nf
        call opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1    fpath,fname, filename)      
        call getfnX(maxnf,nd,ndel,fndel,fname)
        call station(nlog,nfin,maxnf,iderr,0,3,1,nd,ndel,
     1               idtyp,iddel,filout,recnew)
c     
c _________________________________________________________
c rrb 2007/07/11; Skip separate diversion station, diversion efficiency
c		  and diversion return flow, etc. 54-62
c     
         nf=nf+9
c
c _________________________________________________________
c rrb 2007/07/11; Update
c               Plan Well Data (*.plw)    63                           
        nf=nf+1
        nfin=nf
        call opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1    fpath,fname, filename)      
        call getfnX(maxnf,nd,ndel,fndel,fname)
        call station(nlog,nfin,maxnf,iderr,0,3,1,nd,ndel,
     1               idtyp,iddel,filout,recnew)
c
c _________________________________________________________
c rrb 2007/07/11; Update
c               Reservoir Return Flow (*.rrf)   64                            
        nf=nf+1
        nfin=nf
        call opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1    fpath,fname, filename)      
        call getfnX(maxnf,nd,ndel,fndel,fname)
        call station(nlog,nfin,maxnf,iderr,0,3,1,nd,ndel,
     1               idtyp,iddel,filout,recnew)
     
c
c _________________________________________________________
c               Evaporation Annual (*.eva) 66
c
c rrb 2011/10/15; Correction moneva=0=monthly data, moneva=1=constant data
cx      write(nlog,*) '  Smlink; moneva ', moneva
        nf=nf+2
        nfin=nf
        if(moneva.eq.1) then                                    
          if(nEva.eq.0) then
            write(99,140)
            write(26,140)
          endif

          call opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1    fpath,fname, filename)
c   
          call getfnX(maxnf,nd,ndel,fndel,fname)
          call station(nlog,nfin,1,iderr,1,17,1,nd,ndel,
     1                 idtyp,iddel,filout,recnew)
        endif
        
c
c _________________________________________________________
c               Precipitation Annual (*.pra) 67
c
c rrb 2011/10/15; Correction moneva=0=monthly data, moneva=1=constant data
cx      write(nlog,*) '  Smlink; moneva ', moneva
        nf=nf+1
        nfin=nf
        if(moneva.eq.1) then                                   
          if(nPre.eq.0) then
            write(99,141)
            write(26,141)
          endif

          call opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1    fpath,fname, filename)
c   
          call getfnX(maxnf,nd,ndel,fndel,fname)
          call station(nlog,nfin,1,iderr,1,17,1,nd,ndel,
     1                 idtyp,iddel,filout,recnew)
        endif
        
c     
c _________________________________________________________
c rrb 2007/07/11; Add separate river gage data, etc. 68 - 69
c     
cx      nf=nf+4
        nf=nf+2
c
c _________________________________________________________
c rrb 2007/07/11; Update
c               Plan Return Flow (*.prf)   70                            
        nf=nf+1
        nfin=nf
        call opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1    fpath,fname, filename)      
        call getfnX(maxnf,nd,ndel,fndel,fname)
        call station(nlog,nfin,maxnf,iderr,0,3,1,nd,ndel,
     1               idtyp,iddel,filout,recnew)
c
c _________________________________________________________
c rrb 2007/07/11; Update
c               Reach Data (*.rch)   71                            
        nf=nf+1
        nfin=nf
        call opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1    fpath,fname, filename)      
        call getfnX(maxnf,nd,ndel,fndel,fname)
        call station(nlog,nfin,maxnf,iderr,0,3,1,nd,ndel,
     1               idtyp,iddel,filout,recnew)
c
c
c     
c _________________________________________________________
c rrb 2011/08/29; Skip separate diversion station, diversion efficiency
c		  and diversion return flow, etc. 72 - 76
c     
         nf=nf+5
                                                                       
c
c _________________________________________________________
c rrb 2011/08/29; Update
c               Reservoir_Historic_To_Recharge_Monthly (*.rre)   77                        
        nf=nf+1
        nfin=nf
        call opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1    fpath,fname, filename)
c
c rrb 98/07/28
        call getfnX(maxnf,nd,ndel,fndel,fname)
        call timec(nlog,maxnf,iderr,0,iybeg,iyend,nd,ndel,
     1             nver,nfin,idtyp,iddel,filout,filename)
c
c _________________________________________________________
c rrb 2011/08/29; Update
c               Diversion_Historic_To_Recharge_Monthly (*.dre)  78                         
        nf=nf+1
        nfin=nf
        call opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1    fpath,fname, filename)
c
c rrb 98/07/28
        call getfnX(maxnf,nd,ndel,fndel,fname)
        call timec(nlog,maxnf,iderr,0,iybeg,iyend,nd,ndel,
     1             nver,nfin,idtyp,iddel,filout,filename)
     
c
c _________________________________________________________
c rrb 2007/07/11; Update
c               Plan recharge (*.plr)  79                             
        nf=nf+1
        nfin=nf
        call opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1    fpath,fname, filename)      
        call getfnX(maxnf,nd,ndel,fndel,fname)
        call station(nlog,nfin,maxnf,iderr,0,3,1,nd,ndel,
     1               idtyp,iddel,filout,recnew)
c
c _________________________________________________________
c rrb 2007/07/11; Update
c               River Reach (*.rir)   80                            
        nf=nf+1
        nfin=nf
        call opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,1,filout,ext(nf),desc(nf),
     1    fpath,fname, filename)      
        call getfnX(maxnf,nd,ndel,fndel,fname)
        call station(nlog,nfin,maxnf,iderr,0,3,1,nd,ndel,
     1               idtyp,iddel,filout,recnew)
c
c
c _________________________________________________________
c               Print global message regarding duplicate id's
 110    write(6,*) ' '
        if(nEva.eq.0) write(6,140)
        if(nPre.eq.0) write(6,141)
        if(nStr.eq.0) write(6,142)
        if(nIPY.eq.0) write(6,143)
        if(nDdc.eq.0) write(6,144)
        if(ndly.eq.0) write(6,145)
        
        write(6,144)
c
c               Print warning if edits were not implemented
        iedit = 0
        ioe=0
        do i=1,nd
          if(idtyp(i).ge.0) then
            if(ioe.eq.0)
     1        write(6,*) '  Main; Warning edits incomplete, see *.log'

            write(99,*) '  Main; Warning, edit not executed for: '
            write(99,111)  i,fndel(i),i,iddel(i),i,recnew(i)
            iedit=1
            ioe=1
          endif
        end do
c
c               Print termination notes
        if(iedit.eq.0) then
          write(6,*)  ' '
          write(99,*) ' ' 
          write(6,*)  '  All edits implemented'
          write(99,*) '  All edits implemented'
          call flush(6)
        endif

        if(iderr.eq.0) then
          write(6,*)  '  Successful termination'
          write(6,*)  '  No duplicate ID problems'
          write(99,*) '  Successful termination'
          write(99,*) '  No duplicate ID problems'

          write(99,*) ' '
          call flush(6)
        else
          write(6,*)  ' '
          write(6,*)  '  Warning; duplicate id(s) in input files'
          write(6,*)  '  Otherwise a successful termination'
          write(99,*) ' '
          write(99,*) '  Warning; duplicate id(s) in input files'
          write(99,*) '  Otherwise a successful termination'
        endif

  112   call flush(6)
        stop 0
c
c               Formats
  111   format('  Main     fndel  ',i5,' = ', a72,/,
     1          '           iddel  ',i5,' = ', a12,/,
     1          '           recnew ',i5,' = ', a160)
  120   format(
     1 ' _______________________________________________________'//
     1 '        SmLink                       '/
     1 '        State of Colorado - Statemod Input File Linker  '//
     1 '        Version: ',f5.2,/,
     1 '        Last revision date: ',a10,//
     1 ' _______________________________________________________')
  130   format(
     1 ' _______________________________________________________'//
     1 '        SmLink                       '/
     1 '        State of Colorado - Statemod Input File Linker  '//
     1 '        For help, see manpage (man smlink)',/
     1 ' _______________________________________________________')
  140   format('#  !!!  Caution, evap (*.evm or *.eva) for',
     1  ' file 1 used for all basins')
  141   format('#  !!!  Caution, precip (*.prm or *.pra) for',
     1  ' file 1 used for all basins')     
  142   format('#  !!!  Caution, structure (*.str) for',
     1  ' file 1 used for all basins')
  143   format('#  !!!  Caution, irrig practice (*.ipy) for',
     1  ' file 1 used for all basins')     
  144   format('#  !!!  Caution, irrig demand (*.ddc) for',
     1  ' file 1 used for all basins')
  145   format('#  !!!  Caution, delay (*.dly or *.urm) for',
     1  ' file 1 used for all basins')    
  146   format(                                                         
     1  '#',/
     1  '# StateMod Response File',/
     1  '#',/                                                           
     1  '# *', a4, 1x, a40,/,                                           
     1  '# Linked file name: ', 1x, a72,/                                
     1  '#',/
     1  '# Type                                    Name',/
     1  '# ______________________________________  ',
     1  '________________________________________')
  
     

c
c               Warning messages
  148   write(99,*) '  Problem reading response file file codes'
  150   write(99,*) '  Problem reading output file in main'
  160   write(6,*)  '  Unsuccessful termination, see smlink.log'
        write(99,*) '  Unsuccessful termination, see smlink.log'
        call flush(6)
        stop 1
        end                                                             
c                                                                       
c ************************************************************          
c                                                                       
c               blank; it checks the first n lines of a file            
c                      for blanks                                       
c                      ib = 0, blank; 1 = not blank                     
        subroutine blank(rec1, n, ib)                                   
        character rec1*250
                                                                        
        ib = 0                                                          
        do 100 i=1,n                                                    
          if(rec1(i:i) .ne. ' ') ib = 1                                 
  100   continue                                                        
        return                                                          
        end                                                             
c                                                                       
c ************************************************************          
c                                                                       
c               checkc; it check the first n characters of a            
c                       control file for consistency                    
        subroutine checkc(filout, rec1, rec2, n, n1, n2)                        
        character filout*72, rec1*250, rec2*250 
                                                                        
        do 110 i=1,n                                                    
          if(rec1(i:i) .ne. rec2(i:i)) then                             
            write(99,100) filout, n1, rec1, n2, rec2 
           goto 500
c          stop                                                        
         endif         
                                                         
  110   continue     
  500   write(6,*) '  Checkc; Problem see log (*.log) file'
        call flush(6)                                                     
        return     
c
c _________________________________________________________
c Formats
  100       format(/,                                                    
     1      ' Checkc; Problem inconsistent control (*.ctl) data ', 
     1        'between input files. File 1 is used in the output',/, 
     1      ' Output File = ', a72,/,                                   
     1      ' Record ', i3,'  = ', a250,/,
     1      ' Record ', i3,'  = ', a250)                                                      
        end
c                                                                       
c ************************************************************          
        subroutine chekfn(nlog,filrsp)
c
c               It checks the name for long path names
c               that cannot be bound into an executable
        character filrsp*72, x*1
c     
c !!!           PC Specific                
        x='\'
c     
c !!!           SGI Specific                
c       x='/'

        do 110 i=1,72
          if(filrsp(i:i).eq.x) then
            ii=0
            do j=i+1,72
              if(filrsp(j:j).ne.' ') ii=ii+1
              if(filrsp(j:j).eq.x) then
                if(ii.le.9) then
                  goto 110
                else
cx                write(nlog,140) filrsp
cx                goto 120
                endif
              endif
            end do
          endif
  110   continue

        return               

  120   write(99,130) filrsp
  130   format(/,'  Chekfn; Problem code is limited to path names',
     1                    ' of 8 characters or less',/,
     1           '          filrsp = ',a72,/
     1           '  Stopped in Checkfn.for')
  140   format(/,'  Chekfn; Warning code may be limited to path names',
     1                    ' of 8 characters or less',/,
     1           '          filrsp = ',a72)
        write(6,*) '  Problem; Stopped in Checkfn.for, see *.log'
        stop
        end
c
c ************************************************************
c
c               checkid; it checks that an Id has not been
c                 inadvertenly duplicated in the linked network
        subroutine checkid(n, filout, id, iderr)
        dimension id(7500)
        character id*12, idx*12, filout*72
 
c       write(99,*) ' n = ', n 
        maxdel=10
        nlist=1
        k=0
        
        do 100 i=1,n                                                    
          idx= id(i)                                                    
                                                                        
          do 100 j=1,n                                                  
            if(j.ne.i .and. idx .eq. id(j)) then 
              k=k+1                         
              filout=filout                                               
              write(99,110) k, filout, idx, id(j)
c             write(6,110)  k, filout, idx, id(j)

              iderr=1
cx            k=k+1
cx              if(nlist.eq.1) then
cx                do k=1,n
                  write(99,120) idx
cx                end do
cx              endif  
c             stop 
c
c rrb 2011/09/19; Limit to 10 stations (temporarily)
              if(k.ge.maxDel) then
                write(99,130) maxDel
                goto 500
              endif                                                       
            endif                                                         
 100      continue                                                        
 500      return                                                          
c
c               Formats
  110   format(                                                     
     1      ' CheckID; Problem ', i5, ' Duplicate id ', /,                                
     1      '   Output File = ', a72,/,                                 
     1      '   ID 1        = ', a12,/,                                 
     1      '   ID 2        = ', a12)
c    1      '  Stopped in Checkid.for')
  120   format('-DELSTA(         ,',a12, ')')
  130   format(/,'  Warning; only first ', i5, ' matches printed')  

        end   
        
c
c ************************************************************
c
c               checkX; it checks that an Id has not been
c                 inadvertenly duplicated in the linked network
        subroutine checkX(n, filout, id, idfn, iderr)
        dimension id(7500), idfn(7500)
        character id*12, idx*12, filout*72
 
c       write(99,*) ' n = ', n 
        maxdel=10
        nlist=1
        k=0
        
        do 100 i=1,n                                                    
          idx= id(i)
          idfx=idfn(i)                                                    
                                                                        
          do 100 j=1,n                                                  
            if(j.ne.i .and. idx .eq. id(j) .and. idfX.ne.idfn(j)) then 
              k=k+1                         
              filout=filout                                               
              write(99,110) k, filout, idx, idfX, id(j), idfn(j)
c             write(6,110)  k, filout, idx, id(j)

              iderr=1
cx            k=k+1
cx              if(nlist.eq.1) then
cx                do k=1,n
                  write(99,120) idx
cx                end do
cx              endif  
c             stop 
c
c rrb 2011/09/19; Limit to 10 stations (temporarily)
              if(k.ge.maxDel) then
                write(99,130) maxDel
                goto 500
              endif                                                       
            endif                                                         
 100      continue                                                        
 500      return                                                          
c
c               Formats
  110   format(                                                     
     1      ' CheckID; Problem ', i5, ' Duplicate id ', /,                                
     1      '   Output File = ', a72,/,                                 
     1      '   ID 1        = ', a12, ' fn ', i5,/,                                 
     1      '   ID 2        = ', a12, ' fn ', i5)
c    1      '  Stopped in Checkid.for')
  120   format('-DELSTA(         ,',a12, ')')
  130   format(/,'  Warning; only first ', i5, ' matches printed')  

        end                                                             
        
                                                                  
c                                                                       
c ************************************************************          
c                                                                       
        subroutine control(maxnf,moneva,iybeg,iyend,nunit,filout)
c
c       approach 
c       1. Read and write control from first file to new control file
c       2. Check and warn when control data is different than that
c          specified in file 2
c
c rrb 2011/09/17; Update                                                   
cx      dimension rec(20)  
        dimension rec(50), recx(50)         
        character rec*250, rec1*250, filout*72, rec8*8, recx*250, 
     1            recU*1
c                                                                       
c               Title                                                   
        nf=10                                                           
        do 100 i=1,maxnf                                                
          nf1 = nf+i                                                    
          call skipn(nf1,0) 
          call skipx(nf1,2)                                             
  100   continue                                                        
        write(26,110)                                                   
  110   format(' Linked Model Simulation',/)                            
                                                                        
c                                                                       
c               Other Control Data based on file 1                      
        nf = 11      
        j2=0
c
c rrb 2011/09/17; Update                                                   
cx      do 120 i=1,20 
        do 120 i=1,50                                                  
          read(nf,'(a250)',end=130,err=170) rec1 
          j2=j2+1
c                                         
          if(i.eq.1) then
            rec8=rec1(1:8)
            read(rec8,'(i8)') iybeg
          endif

          if(i.eq.2) then
            rec8=rec1(1:8)
            read(rec8,'(i8)') iyend
          endif
c
c               Adjust output units (1=cfs, 2=af, 3=kaf)
cx        if(i.eq.3) rec1(1:8) = '       3'

          if(i.eq.3)  then
            if(nunit.eq.1) rec8='       1'
            if(nunit.eq.2) rec8='       2'
            if(nunit.eq.3) rec8='       3'
          endif
        
          if(i.eq.4) then
            rec8=rec1(1:8)
            read(rec8,'(i8)') moneva
          endif
c
c              Print data from control data in file 1 to
c              the output control file
          write(26,'(a250)') rec1
c
c rrb 2011/09/17; Save then remove data past column 12
          do k=13,250
cx            recx(k:k)=rec1(k:k)
cx            rec1(k:k)=' '
          end do
c
c                 Store data from file 1 in rec          
          rec(i) = rec1 

  120   continue
c                                                                       
c             Check files for consistency     
c
c rrb 2011/09/17; revise to read more than 17 values in *.ctl                    
cx130   if(i.lt.16) goto 170
  130   nf = 10                                                         
        do 140 j=1,j2                                                   
          do 140 i=2,maxnf                                              
          nf1 = nf+i                                                    
          read(nf1,'(a250)',end=150,err=170) rec1       
c
c _________________________________________________________
c               Adjust parameter iresop to print in Kaf
          if(j.eq.3)  rec1(1:8) = '       3'
c
c _________________________________________________________
c rrb 2011/09/17; Remove data past column 12
cx          do k=13,250
cx            rec1(k:k)=' '
cx          end do          
c _________________________________________________________
c                 compare files
          call checkc(filout, rec(j), rec1, 8, 1, i)                          
  140   continue                                                        
c                                                                       
c _________________________________________________________
c              Close input and output file 
c rrb 2011/09/17; Simplify                             
cx150   if(j.lt.j2) goto 170
  150   continue
        do 160 nf=11,10+maxnf                                           
          close(nf)                                                     
  160   continue                         
        close(26)                                                       
        return                             
c
c               Error Messages              
  170   write(99,*) '  Problem reading file ', nf,' in control.f',j,j2
        write(6,*)  '  Unsuccessful termination, see smlink.log'
        write(99,*) '  Unsuccessful termination, see smlink.log'
        call flush(6)
        stop 1
        end                                                             
c                                                                       
c ************************************************************          
        subroutine gis(maxnf, nx)
        character rec1*250
             
        ix=0
  100   ix=ix+1
        nf=10
        do 110 i=1,maxnf 
          nf1=nf+i
          call skipn(nf1,26)
  110   continue
                 
        nf=10
        do 130 i=1,maxnf
          nf1=nf+i
  120     read(nf1,'(a250)',end=130,err=150) rec1
          if(rec1(1:1).ne. '#') then
            write(26,'(a250)') rec1
            goto 120
          else
            backspace(nf1)
          endif
  130   continue
c
c               Repeat for vector files
  140   if(ix.le.nx-1) goto 100
c                                                                       
c              Close output file                                        
        close(26)                                                       
        return                                                          
c
c               Warning Messages
  150   write(99,*) '  Problem with gis file in gis.f'
        write(6,*)  '  Unsuccessful termination, see smlink.log'
        write(99,*) '  Unsuccessful termination, see smlink.log'
        call flush(6)
        stop 1
        end                                                             
c                                                                       
c ************************************************************          
        subroutine opener(maxnf,fpath)
        dimension fpath(5)
        character filrsp*72, fpath*72
        character recin*232, recout*232
        recin=' '
        recout=' '
        filrsp=' '

        nfin = 0 
        do nf=1,maxnf
          call skipn(95,0)
          read(95,'(a72)',end=110,err=120) filrsp
c
c rrb 04/06/16; Change the case 
c           1 caps to lower case
c			      0 (not 1) lower case to upper
          nlog=99
          recin(1:72)=filrsp
          nin=232
          ntype=0
          call AdjCase(nlog, recin, recout, nin, ntype)          
          filrsp=recout(1:72)
                    
          if(filrsp(1:3).eq.'   ') goto 110
c
c rrb 98/06/29
          if(filrsp(1:1).eq.'#' .or. filrsp(1:1).eq.'-') goto 110
          call chekfn(nlog,filrsp)

          open(nf, file=filrsp, status='old',err=120)                           
          call skipn(nf,0)
          nfin = nfin+1
          call getpath(filrsp,fpath(nf))
          write(99,140) nfin, filrsp
        end do

  110   if(nfin.le.maxnf) then
c
c rrb 98/06/29 & 98/10/21
          if(nfin.lt.maxnf) backspace(95)
          maxnf = nfin
        else
          write(6,*) '  Problem, maximum # of files = 5'
          write(99,*) '  Problem, maximum # of files = 5'
          call flush(6)
          goto 130
        endif
                                     
        return                                                          
c
c               Warning Messages
  120   write(99,*) ' '
        write(99,*) '  Opener: Problem opening file ', filrsp
  130   write(6,*)  '  Unsuccessful termination, see smlink.log'
        write(99,*) '  Unsuccessful termination, see smlink.log'
  140   format('  Opener, Opened file ', i5,' ', a72)
        call flush(6)
        stop 1
        end                                                             
c                                                                       
c ************************************************************          
        subroutine opentop(nlog, nver, nfin,
     1    iwarn,ix,maxnf,it,filout,extx,descx,
     1    fpath, fname, filename)
c
c		     It opens the linked output file and
c		     Opens each file to be linked     
c
c		     nver=0 Read fixed format response file
c		          1 Read commmand line response file
c		     nfin   For nver=1, the file type to open
c		     it  =0 Open output file only
c		     it   1 Open output file and input files
c		     maxnf  Number of basins to link (input files
c		            to read
c
        dimension fpath(5), fname(5)
        character filout*72, filin*72, extx*4, descx*40,
     1            fpath*72,  fname*72
        character recin*232, recout*232
        dimension filename(5,80)
        character filename*72
c
c _________________________________________________________
c		Initilize        
        recin=' '
        recout=' '
        filin=' '

        iout=0
        ifound=0
c
c _________________________________________________________
c		Print to log                                                                        
        write(6,100) extx, descx, nfin       
        write(nlog,110) extx, descx, nfin       
  100   format(   ' OpenTop; Processing ', a4, '; ', a40,' nfin = ',i5)
  110   format(/, '_____________________',/
     1            ' OpenTop; Processing ', a4, '; ', a40,' nfin = ',i5)
c
c _________________________________________________________
c		Open output file                                                                        
        filout(ix+1:ix+4) = extx                                        
        open(26, file=filout, status='unknown')                         
c                                                                       
c _________________________________________________________
c               Print header to output file                                          
        write(26,130) extx, descx, filout                               
        write(nlog,130) extx, descx, filout                               
c                               
c
c _________________________________________________________
c               Open input files
        if(it.eq.1) then                                                
          do 120 nf=1,maxnf
            fname(nf)= ' '
            nf1=nf+10

c
c ---------------------------------------------------------
c rrb 2007/07/11; Accomodate Version 10 and greater            
            if(nver.lt.10) then
              read(nf,'(a72)',end=150,err=160) filin
            else
              filin=filename(nf,nfin)
            endif
            
            if(iout.eq.1) 
     1       write(nlog,*) ' Opentop; nf, nf1, nver, filin ',
     1         nf, nf1, nver, filin
c
c _________________________________________________________
c rrb 04/06/16; Make lower case
            nlog=99
            recin(1:72)=filin
            nin=232
            ntype=0
            call AdjCase(nlog, recin, recout, nin, ntype)          
            filin=recout(1:72)
            
            fname(nf)=filin
            write(26,140) nf, filin
            write(nlog,140) nf, filin
c
c ---------------------------------------------------------
c rrb 2007/07/11; Allow a file to not be included		            
            if(filin(1:2).ne.'-1') then
              call putpath(filin,fpath(nf))
              open(nf1, file=filin, status='old', err=160)                         
c
c ---------------------------------------------------------
c rrb 2007/07/11; Update
c		For version 10 and greater print to response file
c		only if a file exists                 
              ifound=ifound+1
              if(nver.gt.10 .and. ifound.eq.1) then
                write(97,142) descx, filout              
              endif  
            endif  
  120     continue                                                      
        endif                                                           
                                                                        
  130   format(                                                         
     1  '#',/                                                           
     1  '# *', a4, 1x, a40,/,                                           
     1  '# Linked file name: ', 1x, a72)                                
  140   format(                                                         
     1  '# Source file : ', i4, 1x, a72)               
  142   format(a40, '= ', a72)                    

        iwarn=1
        return                                                          
c
c               Error Messages              
  150   write(99,152) nf, filin
  152   format('  Opentop; Problem reading input file ', i5,/
     1         '  Opentop; filin = ', a72)
        write(6,*)  '  Opentop; Problem, see smlink.log'
        write(99,*) '  Opentop; Problem, see smlink.log'
        write(99,*) ' '
        call flush(6)
        stop 1
  160   write(99,162) nf, filin
  162   format('  Opentop; Warning cannot read input file ', i5,/
     1         '  Opentop; filin = ', a72,/
     1         '  Opentop; Maybe OK if *.gis or *.out')
        write(6,*)  '  Opentop; Warning, see smlink.log'
        write(99,*) '  Opentop; Warning, see smlink.log'
        write(99,*) ' '
        call flush(6)
c
c rrb; Continue on if iwarn = 0 (typically for *.out and *.gis)               
        if(iwarn.eq.0) return
        stop 1

        end                                                             
c                                                                       
c ************************************************************          
        subroutine getpath(filrsp,fpath1)
c
c               It finds the path for an input file
        character filrsp*72, fpath1*72, x*1
c     
c !!!           PC Specific                
        x='\'
c     
c !!!           SGI Specific                
c       x='/'
        fpath1=' '
        iout=0

        do i=1,72
          ii=73-i
          if(filrsp(ii:ii).eq.x) then
            do j=1,ii
              fpath1(j:j) = filrsp(j:j)
            end do
            goto 120
          endif
        end do
            

  120   if(iout.eq.1) write(99,130) filrsp, fpath1
  130   format(/,
     1         '  Getpath results; ',/,
     1         '           filrsp  ',a72,/,
     1         '           fpath   ',a72)
        return    
        end
c                                                                       
c ************************************************************          
        subroutine putpath(filrsp,fpath1)
c
c               It adds a path for an iput file if none provided
        character filrsp*72, fpath1*72, filrsp1*72, x*1
c
c     
c !!!           PC Specific                
        x='\'
c     
c !!!           SGI Specific                
c       x='/'
        icheck=0                        
        filrsp1=filrsp
c
c               Check to see if a path was provided
        do 100 i=1,72
          ii=73-i
          if(filrsp(ii:ii).eq.x) goto 130
  100   continue              
c
c               Add path
        do 120 i=1,72
          if(fpath1(i:i).ne.' ') then
            filrsp(i:i) = fpath1(i:i)
          else         
            ii=i-1
            do 110 j=1,72
              if(filrsp1(j:j).ne.' ') then
                ii=ii+1                
                if(ii.gt.72) goto 140
                filrsp(ii:ii) = filrsp1(j:j)
              else
                goto 130
              endif
  110       continue
          endif
  120   continue
        goto 140

  130   if(icheck.eq.1) write(99,150)  filrsp1, fpath1, filrsp
        return      
                 
  140   write(6,*) '  Problem with putpath, see *.log'
        write(99,150)  filrsp1, fpath1, filrsp
  150   format('  Putpath results; ',/,
     1         '           filrsp1 ',a72,/,
     1         '           fpath1  ',a72,/
     1         '           filrsp  ',a72)
        end
c                                                                       
c ************************************************************          
c       Parse, it parses thru the command line to find request          
c                                                                       
c               filenc  = base file name                                
c                                                                       
        subroutine parse(filenc)                                        
        character comP*127, filenc*72                                
c
c               Initilize
        filenc = ' '                                                    
c     
c !!!           PC Specific
c               Get command line data                                   
        call getcl(comP)                                            
c                                                                       
c !!!           SGI specific                                            
c       comP = ' '                                                   
c       narg = 1                                                        
c       call getarg (narg, comP)                                     
c                                                                       
c               Find control file name (command is packed to left)      
c ----------------------------------------------------------------      
        do 100 i=1,127                                                  
          if(comP(i:i) .ne. ' ') then                                
            filenc(i:i) = comP(i:i)                                  
          else                                                          
            goto 110                                                    
          endif                                                         
  100   continue                                                        
                                                                        
  110   if(i.eq.1) filenc = 'smlink.rsp'
        write(99,120) filenc                                            
  120   format(' Input file name = ', a72)                              
        return                                                          
        end                                                             
c                                                                       
c ************************************************************          
        subroutine rin(nlog, maxnf,iderr,nd,ndel,idtyp,
     1    iddel,filout,recnew) 
        dimension id(7500), nin(5), ndel(200), idtyp(200),iddel(200),
     1            recnew(200)
        character filout*72, rec1*250, id*12, ifx*1, rec3*3, iddel*12,
     1            recnew*160, recout*250
c                                                                       
c               Initilize
c               iout=2 detailed info regarding ID's     
        iout=0                                         
        nf = 10      
        rec3='   '
                                                           
        do 100 i=1,maxnf                                                
          call skipn(nf+i,26)
  100     nin(i) = 0                                                    
c                                                                       
c               Print network                                           
        n = 0  
        nf=11                                                 
  110   nf1=nf-10       
        if(iout.eq.2) write(99,*) ' Rin; Processing file ', nf1                                                
  120   read(nf,'(a250)',end=140,err=180) rec1  
c
c rrb 2011/09/20; Adjust the case  and store in recout
        ntype=0
        call AdjCase(nlog, rec1, recout, 232, ntype)        
cxx        rec1=recout       
        
        call blank(rec1, 12, ib) 
        
        if(iout.eq.1) write(99,*) ' Rin;  ', rec1                          
c
c rrb 2011/10/15; Compare to the record with the adjusted case
        rec3(1:3) = rec1(37:39)
        rec3(1:3) = recout(37:39)
c
c rrb 2011/09/24; Update           
c        if(rec3.eq.'End' .or. rec3.eq.'END' .or. rec3.eq.'end') then
         if(rec3.eq.'   ') then
          if(iout.eq.2) write(nlog,*)' Rin; End = ', rec1
          goto 140
        endif
        
        if(ib.ne.0) then
c
c               Check for an id indicator in first 3 columns
          call blank(rec1, 3, ib2)
          if(ib2.ne.0) then                   
            n = n+1
c
c rrb 2011/10/15; Store ID with recout; the one with the case adjusted
cx          id(n) = rec1(1:12)  
            id(n) = recout(1:12)
            nin(nf1) = nin(nf1) + 1
c
c rrb 98/06/29; Delete desired stations
            call delsta(nf1,idel,n,nin(nf1),nd,ndel,0,
     1                  idtyp,nlog,0,iddel,id(n))
          endif

          if(iout.eq.2) then
            write(99,*) '  Rin; nf, n, idel, id(n) ',nf, n, idel, id(n)
          endif

          if(idel.eq.0) write(26,'(a250)') rec1       
          goto 120                                                      
        endif                                                           
c                                                                       
c                      End of file                                      
  140   if(n.eq.0) goto 180
        write(ifx,'(i1)') nf1
c
c rrb 2011/10/15;  Upper Case
        rec1(37:48) = 'END_FILE_?  '                                    
        rec1(13:36) = 'END_FILE_?              '                        
        rec1(46:46) = ifx                                               
        rec1(22:22) = ifx                                               
                                                                        
        write(26,'(a250)') rec1 
        n=n+1                                                           
        id(n) = rec1(1:12)                                            
        nin(nf1) = nin(nf1)+1
c
c                       Check for an add record by file
        call addrec(nf1,idel,n,nin(nf1),nd,ndel,0,idtyp,iddel,id,recnew)
c    
c _________________________________________________________                                                                   
c               Check for another file                                  
        nf=nf+1                                                         
        if(nf.le.10+maxnf) goto 110                                     

c                                                                       
c              Check id's for duplicates                                
        call checkid(n, filout, id, iderr)
c                                                                       
c              Print processing results                                 
        write(99,160) filout, (nin(j),j=1,maxnf), n                     
        write(98,160) filout, (nin(j),j=1,maxnf), n                     
  160   format(a72,/,2x, 20i5)                                          
c                                                                       
c              Close input and output file                              
        do 170 nf=11,10+maxnf                                           
          close(nf)                                                     
  170   continue                                                        
        close(26)                                                       
        return                                                          
c
c               Error Messages              
  180   write(99,*) '  Rin; Problem reading network file ', nf
        write(99,*) '    or number of cards read: ', n
        write(6,*)  '  Unsuccessful termination, see smlink.log'
        write(99,*) '  Unsuccessful termination, see smlink.log'
        call flush(6)
        stop 1
        end                                                             
c                                                                       
c ************************************************************          
        subroutine rsp(ix, nver, filout, ext, desc)  
c
c rrb 2011/08/29; Correction                         
        dimension ext(80), desc(80)                                     
        character filout*72, ext*4, desc*40
                                                                        
                                                                        
        write(26,100)                                                   
  100   format(                                                         
     1  '# Name' 66x, 'Description (optional)',/                        
     1  '# ',10('_'), 62x, 10('_'))                                     
                                                                        
        do 110 i=2,26                                                   
          filout(ix+1:ix+4) = ext(i)                                    
          write(26,120) filout, desc(i)                                 
  110   continue                                                        
  120   format(a72, a40)                                                
c                                                                       
c              Close output file                                        
        close(26)                                                       
        return                                                          
        end                                                             
c                            
c ************************************************************
c
c               skipn it skips any number of comment cards identified
c               as a '#' in column 1 of a file
c
        subroutine skipn(nf,io)
        character rec1*144
c                 
c              Check first record and store for use on this file
        n=0
  100   read(nf,'(a144)',end=110,err=110) rec1
        n=n+1
c
        if(rec1(1:1).eq.'#') then  
          if(io.ne.0) write(io,'(a144)') rec1
          goto 100
        else
          if(n.ge.1) backspace(nf)
        endif        

  110   return
        end
c                            
c ************************************************************
c
c               skipx it skips x lines from an input file
c
        subroutine skipx(nf,n)
        character rec1*1
c                 
c              Check first record and store for use on this file
        do 100 i=1,n
  100   read(nf,'(a1)',end=110) rec1
        rec1 = rec1
  110   return
        end
c                            
c ************************************************************
        subroutine station(nlog,nfin,maxnf,iderr,ia,i3,ichk,
     1                     nd,ndel,idtyp,iddel,filout,recnew)
c
c               i3        = columns to check for ID
c               ia          Time series control 1=annual data
c               ichk    1 = Check ID,
c                       0 = Do not Check id (temporarily off for *.ddr)
c               nd        = Number of command file edits
c               ndel()    = File number to be edited
c               iddel()*12= ID to be edited
c               idtyp()   = Edit type 1=delsta, 2=addrec, 3=repzero
c               filout    = Output file         
c
c rrb 2011/09/24; Revise to store idfn(file #) and not check if the same file
c
        dimension id(7500), nin(5), ndel(200), idtyp(200),
     1            iddel(200),recnew(200), idfn(7500)
        character filout*72, rec1*250, id*12, iddel*12, recnew*160
        character recin*232, recout*232, rec12*12, recIdum*8, recTyp*8,
     1            recSou2*8 
c
c               Initilize
        n = 0                 
        nf = 10
        ioutD=0
        ioutX=0
        idum=0
        
cx      if(nfin.eq.13) ioutX=1  
cx      if(ioutX.eq.1) write(nlog,*) ' Station; ioutX', ioutX      

        do 100 i=1,maxnf
          call skipn(nf+i,26)
  100     nin(i) = 0

  110   nf=nf+1
        nf1= nf-10
c
c _________________________________________________________
c               Time series control for annual data  (ia=1)
        if(ia.eq.1) then
          if(nf1.eq.1) then
            read(nf,'(a250)',end=130,err=160) rec1
            write(26,'(a250)') rec1
          else
            read(nf,'(a250)',end=130,err=160) rec1
          endif
        endif
c
c _________________________________________________________
c		Read station
  120   read(nf,'(a250)',end=130,err=160) rec1
c
c rrb 2007/07/11; Allow a comment card (#)
        if(rec1(1:1).eq.'#') then
          write(26,'(a250)') rec1
          goto 120
        endif  
        
c       if(ioutX.eq.1) write(99,*) ' Station; rec1 = ', rec1
           
        call blank(rec1, 96, ib)
c
c _________________________________________________________
c               Check for an id indicator in first 3 columns or 12
 
        if(ib.eq.1) then
c
          call blank(rec1,i3, ib2)
c
c ---------------------------------------------------------
c rrb 2011/09/09; Special check for monthly on / off data in an 
c                 operating rule file (stored from prior record read 
c                 Do not use; does not handle ype 27 with iopsou4>0
cx          if(nfin.eq.13 .and. idum.ge.12) then
cx            ib2=0
cx            idum=0
cx          endif
                                      
          if(ib2.ne.0) then                   
            n = n+1     
            nin(nf1) = nin(nf1) + 1
            if(i3.ne.17) then
              id(n) = rec1(1:12)
            else
              id(n) = rec1(6:17)
            endif 
c
c rrb 2011/09/24; Add file id so we only check different files  
            idfn(n)=nf1        
c
c _________________________________________________________
c rrb 2007/07/11; Update by adjusting the case of the 
c		  So edit commands work with operating 
c		  rule file (*.opr)
            rec12=id(n)
            recin(1:12)=rec12
            ninX=12
            ntype=0
            call AdjCase(nlog, recin, recout, ninX, ntype)          
            id(n)=recout(1:12)
c
c _________________________________________________________
c rrb 2007/07/11; Control detailed checking by data type
c		             13 = operational rights

           if(ioutX.eq.1) then
c            write(99,*) ' Station; nfin, filout',nfin, filout
             write(99,*) ' Station; nf, n, id(n)', nf, n, id(n)
           endif
c
c _________________________________________________________
c rrb 98/06/29; Delete desired stations
            call delsta(nf1,idel,n,nin(nf1),nd,ndel,0,
     1                  idtyp,nlog,ioutD,iddel,id(n))
c
c ---------------------------------------------------------
c rrb 2011/09/09; Special check for monthly on / off data in an 
c                 operating rule file used to check the next 
c                 record read
c                 Do not use; does not handle ype 27 with iopsouu4>0  
cx            idum=0
cx            if(nfin.eq.13) then
cx              rec8=rec1(65:72)
cx              read(rec8,'(f8.0)',err=122) dum
cx              idum=dum
cxc             write(nlog,*) ' Station rec8, idum = ', rec8, idum
cx              idum=iabs(idum)
cx              if(idum.ge.12) ib2=0
cx            endif
            
          endif
c
c               Print a non station record
  122     if(idel.eq.0) write(26,'(a250)') rec1
          goto 120
        endif
c
c _________________________________________________________
c               Add stations as necessary note the ID must be -addstr
  130   call addrec(nf1,idel,n,nin(nf1),nd,ndel,0,idtyp,iddel,id,recnew)
c
c _________________________________________________________
c               Check for other stations
        if(nf1.lt.maxnf) goto 110
c
c _________________________________________________________
c              Print processing results
        write(99,140) filout, (nin(j),j=1,maxnf), n
        write(98,140) filout, (nin(j),j=1,maxnf), n                     
  140   format(a72,/,2x, 20i5)
c
c _________________________________________________________
c              Check id's for duplicates
c rrb 98/06/29; Temporarily do not print duplicates for *.ddr
 
        if(ichk.eq.1) then
c
c rrb 2011/09/24; Add input file to avoid checking ID's from
c                 the same file (vip for well rights, & stations
cx        call checkid(n, filout, id, iderr)
          call checkX(n, filout, id, idfn, iderr)
        else
          write(99,*) '  ** Warning not checking duplicates in file ', 
     1      filout
        endif
c
c _________________________________________________________
c              Close input and output file
        do 150 nf=11,10+maxnf
          close(nf)      
  150   continue                                             
        close(26)
c
c _________________________________________________________

        return
c
c
c _________________________________________________________
c               Error Messages              
  160   write(99,*) '  Staton; Problem reading station file ', nf
        write(6,*)  '  Unsuccessful termination, see smlink.log'
        write(99,*) '  Unsuccessful termination, see smlink.log'
        call flush(6)
        stop 1
        end
c                            
c ************************************************************
        subroutine delsta(nf,idel,n,nin1,nd,ndel,iy1,idtyp,
     1    nlog,iout,iddel,id1)
        character id1*12, iddel(200)*12
        dimension ndel(200),idtyp(200)
c
        idel=0
        
        iout=0
cx      if(id1.eq."0200834_I   ") iout = 1
cx      if(id1.eq."0203858     ") iout = 1
        if(iout.eq.1) write(nlog,*) '  DelSta; id1 ', id1
c
c _________________________________________________________
        i1=0
        do i=1,nd        
cx         if(iout.eq.1) then
cx           write(99,111) i, nf, ndel(i), idtyp(i),iddel(i),id1
cx  111      format('  Delsta ', 4i5, 2(1x,a12))
cx         endif
c
c _________________________________________________________

          if(iabs(idtyp(i)).eq.1) then
            if(nf.eq.ndel(i) .and. id1.eq.iddel(i)) then
              if(idtyp(i).eq.1) write(99,100) id1, nf
              
              idel=1
              n=n-1
              nin1=nin1-1
              id1=' '
              idtyp(i)=-1
              i1=i
            end if
          end if
        end do
        
       if(iout.eq.1 .and. i1.gt.0) then
         write(nlog,*) '  DelSta; ',nf, i1, idtyp(i1) , idel
       endif
c
c _________________________________________________________
        return
c
c _________________________________________________________
 100    format('  ** Delsta;  Deleting  ID ', a12,
     1         ' From Source File ', i5,' in all years ')
        end


c        
c ************************************************************
        subroutine repzero(nf,idel,n,nin1,nd,ndel,iy1,idtyp,
     1             iddel,id1,rec1)
        character id1*12, iddel(200)*12, rec1*250, rec0*8
        dimension ndel(200),idtyp(200)

        iout=0
        idel=0
        rec0='    0.00'
c
c _________________________________________________________

        do i=1,nd
          if(iout.eq.1) then
            write(99,111) i, nf, ndel(i), idtyp(i), id1, iddel(i)
 111        format('  Repzero ', 4i5, 2(1x,a12))
          endif

          if(iabs(idtyp(i)).eq.3) then
            if(nf.eq.ndel(i) .and. id1.eq.iddel(i)) then

              if(idtyp(i).eq.3) write(99,100) id1, nf
              idtyp(i)=-3
c
c
c _________________________________________________________
c                       Replace data with zero's
              ke=17
              do k=1,12
                kb=ke+1
                ke=kb+7
                rec1(kb:ke)=rec0
              end do

              rec1(ke+1:ke+2) = '  '
              kb=ke+3
              ke=kb+7
              rec1(kb:ke)=rec0
              if(iout.eq.1) write(99,'(a250)') rec1
            end if
          end if
        end do
c
c _________________________________________________________
        return
c
c _________________________________________________________
 100    format('  ** Repzero; Replacing ID ', a12,
     1         ' From Source File ', i5,' in all years with zeros')
        end

c
c ************************************************************
        subroutine addrec(nf,iadd,n,nin1,nd,ndel,iy1,
     1             idtyp,iddel,id,recnew)
        character iddel(200)*12, recnew*160, id*12, rec160*160
        dimension ndel(200), idtyp(200), recnew(200), id(7500)

        iadd=0
        iout=0
c
c _________________________________________________________

        do i=1,nd
          if(iout.eq.1 .and. idtyp(i).eq.2) then
            write(99,111) i, nf, ndel(i), idtyp(i), iddel(i)
          endif
c
c _________________________________________________________

          if(nf.eq.ndel(i) .and. iabs(idtyp(i)).eq.2) then
            write(26,'(a160)') recnew(i)
            if(idtyp(i).eq.2) write(99,100) recnew(i)

            iadd=1
            n=n+1
            nin1=nin1+1
c           id1=' '
            idtyp(i)=-2
c
c
c _________________________________________________________
c               Set ID
            rec160=recnew(i)
            id(n)=rec160(1:12)
          end if
        end do
c
c _________________________________________________________
        return
c
c _________________________________________________________
 100    format('  ** Addrec; Adding the following record ',
     1         a160)
 111    format('  Addrec ', 4i5, 2(1x,a12))    
        end
c                            
c ************************************************************
        subroutine staRib(maxnf, iderr, i3, filout)
c
c               Similar to station, but handles larger record
c               size for *.rib file.  Also checks number for 
c               poetntial dimension problems; 
c               Note max dimension = 15 = (350 - 28)/21
        dimension id(7500), nin(5)
c       character filout*72, rec1*250, id*12
        character filout*72, rec1*250, id*12, recid*250, rec8*8
c
c _________________________________________________________
c
c               Initilize
        n = 0                 
        nf = 10                    
        ndim=350

        do 100 i=1,maxnf
          call skipn(nf+i,26)
  100     nin(i) = 0

  110   nf=nf+1
        nf1= nf-10
c
c _________________________________________________________

  120   read(nf,'(a350)',end=130,err=160) rec1
c
c rrb 98/06/29
c       write(99,'(a350)') rec1
c
c
c _________________________________________________________
c               Check dimension (it is 28+21*gages)
        rec8=rec1(21:28)
        read(rec8,'(i8)') n
        if(28+21*n.gt.ndim) goto 170
c
c
c _________________________________________________________
c               Check for blank card
        recid=rec1(1:250)
        call blank(recid, 96, ib)
c
c _________________________________________________________
 
        if(ib.eq.1) then
c
c               Check for an id indicator in first 3 columns or 12
          call blank(recid,i3, ib2)
          if(ib2.ne.0) then                   
            n = n+1     
            nin(nf1) = nin(nf1) + 1
            if(i3.ne.17) then
              id(n) = recid(1:12)
            else
              id(n) = recid(6:17)
            endif
          endif
c

          write(26,'(a350)') rec1
          goto 120
        endif
c
c _________________________________________________________
c
c               Check for other stations
  130   if(nf1.lt.maxnf) goto 110
c
c
c _________________________________________________________
c              Print processing results
        write(99,140) filout, (nin(j),j=1,maxnf), n
        write(98,140) filout, (nin(j),j=1,maxnf), n                     
  140   format(a72,/,2x, 20i5)
c
c
c _________________________________________________________
c              Check id's for duplicates
        call checkid(n, filout, id, iderr)
c
c
c _________________________________________________________
c              Close input and output file
        do 150 nf=11,10+maxnf
          close(nf)      
  150   continue
        close(26)
c
c _________________________________________________________

        return
c
c _________________________________________________________
c
c               Error Messages              
  160   write(99,*) '  Problem reading station file ', nf
        write(6,*)  '  Unsuccessful termination, see smlink.log'
        write(99,*) '  Unsuccessful termination, see smlink.log'
        call flush(6)     
        goto 180
  170   write(99,*) '  Problem reading station file ', nf
        write(99,*) '  Number of gages = ', n
        write(99,*) '  Max number of =   ', (ndim-28)/21
        write(99,*) '  Dimension of ', ndim, ' exceeded'
        write(99,*) '  Note max dimension approx = 28 + 21 * gages'
        write(6,*)  '  Unsuccessful termination, see smlink.log'
        write(99,*) '  Unsuccessful termination, see smlink.log'
        call flush(6)     
        goto 180

  180   stop 1
        end
c                            
c ************************************************************
c
        subroutine timec(nlog,maxnf,iderr,idtar,iybeg,iyend,nd,ndel,
     1                   nver,nfin,idtyp,iddel,filout,filename)  
     
c
c rrb 2011/09/24; Revise to store idfn(file #) and not check if the same file
c
c
c
c               idtar = 0 standard
c                     = 1 target file, two id's per station
c               iyz   = 0 first year
c                     = 1 years n
c               ib    = 0 blank data (end of file)
c                     = 1 non blank data
c               idy   = number of values read
c               nin(i)= number read from file i
c               ninx(i)= same as nin but not reset for a blank file
c               idtyp = 1 = delsta, 2=adddrec, 3=repzero
c
c		nver  = StateMod version #
c		nfin  = data type being processed
c		filename = filename
c
        dimension nin(5), iymax(5), iymin(5), idtyp(200), id(7500),
     1            ndel(200), iddel(200), ninx(5), idfn(7500)
        character filout*72, rec1*250, rec4*4, id*12, iddel*12,
     1            recout*250
        
        dimension filename(5,80)
        character filename*72, filenam1*72
        
c
c               Initilize
c       n=0
        nx=0                    
        iyz=0
        idy=0    
        nf=10
        iout=0
c
c _________________________________________________________
        
        do i=1,maxnf
          call skipn(nf+i,26)                           
          iymax(i) = 0  
          iymin(i) = 9999
          nin(i) = 0
c
c rrb 04/06/16; Initilize
          ninx(i) = 0          
        end do
c
c
c _________________________________________________________

c               Year Loop begins here
  110   n=0
        nf=11
        nf1=nf-10
        do i=1,maxnf
          nin(i)=0
        end do
c
c _________________________________________________________
c rrb 2007/07/11: Allow one or more files to not have data        
        if(nver.ge.10) then
          do i=1,maxnf
            filenam1=filename(nf1,nfin)
            if(filenam1(1:2).eq.'-1') then
              nf=nf+1
              nf1=nf-10
            else
              goto 111  
            endif
          end do
c
c		Exit if no files are found          
          goto 158 
        endif
c
c
c _________________________________________________________
c               Read and write time control (card 1)
 111    if(iyz.eq.0) then
          read(nf,'(a250)',end=180,err=180) rec1
c         write(99,'(a250)') rec1

          call blank(rec1, 12, ib)
c
c               Write non blank data
          if(ib.eq.1) then
            write(26,'(a250)') rec1
          else
            write(99,*) '  Warning Dummy file ', nf, ' in time.f'
            write(99,*) '   Note a dummy is OK if not the first file'
            close(nf)
            goto 170
          endif
        endif
c
c
c _________________________________________________________
c               Read first year of data to get the year from file 1 (11) 
        iprnt = 0
 1      read(nf,'(a250)',end=140,err=180) rec1
c       write(99,'(a250)') rec1
c
c ---------------------------------------------------------
c rrb 2011/09/20; Adjust the case for command processing (-delsta, etc).
        ntype=0
        recout=rec1                
c
c rrb 2011/10/15; correction adjust case thru column 17 for a timeseries
c       call AdjCase(nlog, rec1, recout,12,ntype) 
        call AdjCase(nlog, rec1, recout,17,ntype) 
c
c rrb 2011/15/10; keep adjusted case in recout
cx      rec1=recout

        call blank(rec1, 12, ib)
        if(ib.eq.1) then
c
c               Non Blank data get year
          idy=idy+1
          rec4=rec1(1:4)
          read(rec4,'(i4)',err=190) iy     
c
c rrb 06/12/96
          if(iy.lt.iybeg) then
            if(iprnt.eq.0) write(99,155) iybeg, iyend, iy
            iprnt=1
            goto 1
          endif

          iymax(nf1) = max0(iymax(nf1),iy)
          iymin(nf1) = min0(iymin(nf1),iy) 

c         if(iyz.eq.0) then
c
c               Check if a target file (idtar=1), with 2 lines per id
            if(idtar.eq.0) then 
              n = n+1
              nin(nf1) = nin(nf1)+1     
c
c rrb 2011/10/15; Store adjusted character string in recout      
cx            id(n) = rec1(6:17)
              id(n)= recout(6:17)
c
c rrb 2011/09/24; Add file id so we only check different files               
              idfn(n) = nf1                 
              call delsta(nf1,idel,n,nin(nf1),nd,ndel,iy1,
     1                    idtyp,nlog,0,iddel,id(n))    
c
c rrb 2011/10/15; Correction (preserve variable idel if = 1
              if(idel.eq.0) then     
                call repzero(nf1,idel,n,nin(nf1),nd,ndel,iy1,
     1                     idtyp,iddel,id(n),rec1)
              endif
            else
c
c rrb 99/01/18; Problem with # of target stations
c             if(idy.eq.2) then
              if(idy.ge.2) then
                idy=0
                n=n+1
                nin(nf1) = nin(nf1)+1
c
c rrb 2011/10/15; Store adjusted character string in recout      
cx              id(n) = rec1(6:17)
                id(n)= recout(6:17)                
c
c rrb 2011/09/24; Add file id so we only check different files               
                idfn(n) = nf1                  
                call delsta(nf1,idel,n,nin(nf1),nd,ndel,iy1,
     1                      idtyp,nlog,0,iddel,id(n))
c
c rrb 2011/10/15; Correction (preserve variable idel if = 1
                if(idel.eq.0) then         
                  call repzero(nf1,idel,n,nin(nf1),nd,ndel,iy1,
     1                      idtyp,iddel,id(n),rec1) 
                endif
cx              if(id(n).eq.'1886_CALL   ') then
cx                write(nlog,*) '  Timec; rec1 ', rec1
cx              endif
              endif
            endif
c
c               Print record (first year)
            if(idel.eq.0) write(26,'(a250)') rec1 
cx          if(idel.eq.0) write(nlog,'(a250)') rec1 
            if(iout.eq.1 .and. idel.eq.1) then
              write(nlog,*) '  TimeC; Deleting ', rec1
            endif
c         endif
        else
c
c rrb 05/31/96
          goto 140
        endif
c
c
c _________________________________________________________
c              Get time series data years n+1-n, files 1-n
        iprnt = 0
c
c               Begin station loop
  120   read(nf,'(a250)',end=130,err=180) rec1
  
c
c ---------------------------------------------------------
c rrb 2011/09/20; Adjust the case  
        ntype=0
c
c rrb 2011/10/15; correction adjust case thru column 17 for a timeseries
c       call AdjCase(nlog, rec1, recout,12,ntype) 
        call AdjCase(nlog, rec1, recout,17,ntype) 
c
c rrb 2011/10/15; Keep unadjusted record in rec1
cx      rec1=recout 
              
                
c       write(99,'(a250)') rec1
c       write(99,*) '  Timec; past 120 read for file ', nf

        call blank(rec1, 12, ib)
 
        if(ib.eq.1) then
c
c rrb 98/06/29
          idy=idy+1
          rec4=rec1(1:4)
          read(rec4,'(i4)',err=190) iy1
c
c rrb 06/12/96
          if(iy1.lt.iybeg) then
            if(iprnt.eq.0) write(99,155) iybeg, iyend, iy1
            iprnt=1
            goto 120
          endif

          iymax(nf1) = max0(iymax(nf1),iy1)
          iymin(nf1) = min0(iymin(nf1),iy1)

c
c                       New Year
          if(iy1.eq.iy) then
            if(idtar.eq.0) then
c
c                      Non Target file
              n=n+1
              nin(nf1)=nin(nf1)+1
c
c rrb 2011/10/15; Store adjusted character string in recout      
cx            id(n) = rec1(6:17)
              id(n)= recout(6:17)              
c
c rrb 2011/09/24; Add file id so we only check different files               
              idfn(n) = nf1  
                          
              call delsta(nf1,idel,n,nin(nf1),nd,ndel,iy1,
     1                    idtyp,nlog,0,iddel,id(n))
c
c rrb 2011/10/15; Correction (preserve variable idel if = 1
              if(idel.eq.0) then     
                call repzero(nf1,idel,n,nin(nf1),nd,ndel,iy1,
     1                     idtyp,iddel,id(n),rec1)
              endif
            else
c
c                       Target File
c
c rrb 99/01/18; Problem with # of target stations
c             if(idy.eq.2) then
              if(idy.ge.2) then
                idy=0
                n=n+1
                nin(nf1) = nin(nf1)+1
c
c rrb 2011/10/15; Store adjusted character string in recout      
cx              id(n) = rec1(6:17)
                id(n)= recout(6:17)
c
c rrb 2011/09/24; Add file id so we only check different files               
                idfn(n) = nf1   
                               
                call delsta(nf1,idel,n,nin(nf1),nd,ndel,iy1,
     1                      idtyp,nlog,0,iddel,id(n))
c
c rrb 2011/10/15; Correction (preserve variable idel if = 1
                if(idel.eq.0) then         
                  call repzero(nf1,idel,n,nin(nf1),nd,ndel,iy1,
     1                      idtyp,iddel,id(n),rec1)
                endif
              endif

            endif
c
c               Print Record (year n+1)                      
            if(idel.eq.0) write(26,'(a250)') rec1
            
            if(iout.eq.1 .and. idel.eq.1) then
              write(nlog,*) '  TimeC; Deleting ', rec1
            endif            
c
c               Return for another station                          
            goto 120
c
c               A New Year
          else
            backspace(nf)
            nf=nf+1
            nf1=nf-10
c
c               Branch for next year
            if(nf1.gt.maxnf) then
              iyz=1
              goto 110
            else
c
c               Skip header and time control on files 2-n
              if(iyz.eq.0) then 
                read(nf,'(a250)',end=130,err=180) rec1
c               call blank(rec1, 12, ib)
c               if(ib.eq.1) idum(nf1) = 1
              endif
c             write(99,*) '  Timec; past time check'
              goto 120
            endif
          endif                
c          
c rrb 00/09/14 Blank ID check if a dummy file
        else
          if(nin(nf1) .eq. 0) then
c           write(99,*) '  Timec; No ID's found; dummy file = ', nf1
c
c rrb 00/10/03; Blank ID store count data in a variable that 
c               is not initilzed.  Cannot come up with something more
c               elegant at this time
            nx = n
            do i=1,maxnf
              ninx(i)=nin(i)
            end do

            backspace(nf)
            nf=nf+1
            nf1=nf-10
            if(nf1.gt.maxnf) then
              iyz=1
              goto 110
            else
              goto 120
            endif
          endif
        endif
c
c
c _________________________________________________________
c              Handle end of file considerations
  130   nf=nf+1        
        nf1=nf-10
c
c rrb 98/06/29;
        idy=0
        if(nf.le.10+maxnf) goto 120
c
c
c rrb 2011/09/24; Add input file to avoid checking ID's from
c                 the same file (vip for well rights, & stations
c
c                 Check id's for duplicates
cx140   call checkid(n, filout, id, iderr)
  140   call checkX(n, filout, id, idfn, iderr)
c
c rrb 98/08/05;
        nlist=1
        if(iderr.eq.1 .and. nlist.eq.0) then
          write(nlog,*) '  TimeC; Duplicate ID found, List follows'
          do i=1,n
            write(99,'(i5, 1x, a12)') i, id(i)
          enddo
        endif
c
c              Check years for control/file mismatch
        ie1=0
        ie2=0
        do 152 nf1=1,maxnf
c
c rrb 00/09/14; Handle file with no data 
          if(iymin(nf1).eq.9999) then
c           write(99,*) '  Timec; Warning file ', nf1, ' is a dummy'
            goto 151
          endif

          if(iymin(nf1).gt.iybeg) ie1 = 1
          if(iymax(nf1).lt.iyend) ie1 = 1

          if(iymin(nf1).ne.iybeg) ie2 = 1
          if(iymax(nf1).ne.iyend) ie2 = 1

 151      continue
          if(iout.eq.1)
     1      write(99,*) '  Timec; nf1, ie1, iymin, iybeg', 
     1                            nf1, ie1, iymin(nf1), iybeg
 152    continue                          
c
c _________________________________________________________

        if(ie1.eq.1) then
          write(99,*)   ' '          
          write(99,*)   ' Timec; Problem starting and ending years are' 
          write(99,*)   '   inconsistent with the control data'
          write(99,*)   '   Control data: ',iybeg, iyend
          write(99,*)   ' '          
c
c _________________________________________________________

          do 154 nf1=1,maxnf
 154        write(99,*) '  Data for File: ',nf1, iymin(nf1), iymax(nf1)
          goto 200      
        endif

        if(ie2.eq.1) then
          write(99,155) iybeg, iyend
 155      format(/,
     1           ' Timec; Warning starting or ending years are',/ 
     1           '   inconsistent with the control data',/
     1           '   Continuing on with smaller data set',/
     1           '   Control data =            ',2i5, /
     1           '   Beginning year of data = ', i5)

          do 156 nf1=1,maxnf
 156        write(99,*) '  Data for File: ',nf1, iymin(nf1), iymax(nf1)
        endif
c
c
c _________________________________________________________
c              Print processing results
c
 158    continue
c rrb 00/10/03; Sometimes updated other times not if a file is missing       
        nx = amax0(nx,n)
        do j=1, maxnf
          ninx(j) = amax0(nin(j), ninx(j))
        end do

        write(99,150) filout, (ninx(j),j=1,maxnf), nx
        write(98,150) filout, (ninx(j),j=1,maxnf), nx                     
  150   format(a72,/,2x, 20i5)
c
c              Close input and output file
        do 160 nf=11,10+maxnf
          close(nf)      
  160   continue
  170   close(26)
c
c _________________________________________________________

        return
c
c _________________________________________________________
c
c               Error Messages              
  180   write(99,*) '  Timec, Problem reading file ', nf
        write(99,*) '  Sometimes OK if you save on a PC with one change'
        goto 200

  190   write(99,*) '  Problem reading year in timec.f from file ', nf
        goto 200

  200   write(6,*)  '  Timec; Problem, see smlink.log'
        write(99,*) '  Timec; Problem, see smlink.log'
        call flush(6)
        stop 1
        end
c                            
c ************************************************************
        subroutine getcom(nd,fndel,idtyp,iddel,recnew)
        dimension  fndel(200), idtyp(200), iddel(200), recnew(200)
        character command*232, iddel*12, recnew*160, fndel*72,
     1            rec7*7, rec1*1, recin*232, recout*232

c
c               idtyp()    = Edit type 1=delsta, 2=addrec, 3=repzero
c _________________________________________________________
        nd=0
        iout=0
        
        do i=1,200*2
          idtyp(i)=0
          read(95,'(a232)',end=150,err=150) command
c
c
c _________________________________________________________
c rrb 04/06/16; Change case of edit commands
          nlog=99
          recin=command    
c
          nin=232    
          recout=recin  
                  
          ntype=0
          call AdjCase(nlog, recin, recout, nin, ntype)          
          command=recout
          
          rec1=command(1:1)
          rec7=command(1:7)
c
          if(rec1.eq.'-') then
            nd=nd+1
            if(nd.gt.200) goto 160

            idtyp(nd)=-1
            if(rec7.eq.'-DELSTA' .or. rec7.eq.'-delsta') idtyp(nd)=1
            if(rec7.eq.'-ADDREC' .or. rec7.eq.'-addrec') idtyp(nd)=2
            if(rec7.eq.'-REPZER' .or. rec7.eq.'-repzer') idtyp(nd)=3
c
c rrb 2011/09/24; Add new code to process an add record
cx          if(idtyp(nd).eq.1 .or. idtyp(nd).eq.3) then
              call getcom2(idtyp(nd),fndel(nd),iddel(nd),
     1                 recnew(nd),command)
     
            if(iout.eq.1 .and. idtyp(nd).eq.2) then
              write(nlog,*) '  GetCom; recnew(nd)', nd, recnew(nd)
            endif
          endif
        end do
 150    write(99,*) ' Getcom; Number of Edits (nd) = ', nd
c
c _________________________________________________________
 
        return
c
c _________________________________________________________
 160    write(6,*) '  Getcom; Problem see *.log'
        write(99,*) ' Getcom; Problem with dimension size, nd = ',nd
        stop
        end
c                            
c ************************************************************
        subroutine getcom2(idtyp1,fndel1,iddel1,recnew1,command)
        character command*232, iddel1*12, fndel1*72,recnew1*160,
     1            rec1*1
c
c           idtyp1 = 1 for -delsta'
c           idtyp1 = 2 for -addrec'
c           idtyp1 = 3 for -repzer
c
c _________________________________________________________

        ifd=1
        iid=1
        fndel1=' '
        iddel1=' '
        recnew1=' '

        iout=0
        n1=72
        n2=160
        nt=n1+n2
cx        write(99,*)' Getcom2; nt ', nt, command
cx        write(6,*) ' Getcom2; nt ', nt, command
c
c _________________________________________________________

        do i=1,nt
          if(command(i:i).eq.'(') then
c
c _________________________________________________________
c                               Find replacement File
            i1=i
            do j=1,nt-i
              i1=i1+1
              if(i1.gt.232 .or. j.gt.72) goto 120

              rec1=command(i1:i1)
              if(rec1.eq.',') ifd=0
              
              if(ifd.eq.1) then
                fndel1(j:j) = command(i1:i1)
              else
c
c _________________________________________________________
c                 Find replacement ID (f not an
c                   -addrec command
                iid=1
                i2=i1
                do k=1,12
                  i2=i2+1
                  rec1=command(i2:i2)
c
c rrb 2011/09/24; Correction to get an add record
cx                if(rec1.eq.')' .or. rec1.eq.',') iid=0
                  if(rec1.eq.')' .or. rec1.eq.',' .or. 
     1               idtyp1.eq.2) iid=0                  
                  
                  if(iid.eq.1) then
                    iddel1(k:k) = command(i2:i2)
                  else
c
c _________________________________________________________
c                               Find replacement string
                    iid=1               
                    i3=i2
c
c rrb 2011/09/24; Update for a add string                    
                    if(idtyp1.eq.2) i3=i2-1
                    
c                    write(99,*) ' GetCom2; n2, i3, n2+i3', n2, i3, n2+i3
c                    write(6,*) ' GetCom2; n2, i3, n2+i3', n2, i3, n2+i3
                    do l=1,n2
                      i3=i3+1
                      rec1=command(i3:i3)
                      if(rec1.eq.')') iid=0
                      if(iid.eq.1) then
                        recnew1(l:l) = command(i3:i3)
                      endif
                    end do
                    goto 100
                    
                  endif
                end do
                
              endif
            end do
          end if
        end do
c
c _________________________________________________________

 100    if(iout.eq.1) write(99,110) command,idtyp1,
     1                              fndel1,iddel1,recnew1
 110    format(/,
     1         '  Getcom2 command = ', a232,/
     1         '           idtyp  = ', i4,/
     1         '           fndel1 = ', a72,/,
     1         '           iddel1 = ', a12,/
     1         '           recnew1= ', a160)
c
c _________________________________________________________
        return
c
c _________________________________________________________
 120    write(6,*) '  Getcom2; Problem see *.log'
        write(99,122) command
 122    format('  Getcom2; ',
     1        'Problem with replacement commands',/
     1    11x,'Note the data separator should be a comma',/
     1    11x,' command = ', a232)
        stop 1
        end
        
c                            
c ************************************************************
        subroutine getfnX(maxnf,nd,ndel,fndel,fname)
c
c               If the file name (fname) is to be edited
c                 (.eq. fndel) set the file pointer (ndel)
c                 to the edit counter (j)
        dimension  ndel(200)
        character  fndel(200)*72, fname(5)*72
c
c _________________________________________________________

        do j=1,200
          ndel(j)=0
          do i=1,maxnf
c           write(99,*) 'getfnX i, j, fndel(j),fname(i)',
c    1       i,j,fndel(j),fname(i)
            if(fndel(j).eq.fname(i)) then
              ndel(j)=i
c             write(99,*) '** getfnX i, j, ndel(j)', i,j,ndel(j)
            endif
          end do
        end do
c
c _________________________________________________________
        return
        end

c
c *********************************************************
c 
      subroutine AdjCase(nlog, recin, recout, nin, ntype)
c
c		It adjusts the case of a character string      
c
c		nin = # of records (72)
c		ntype = 1 caps to lower case
c			      0 (not 1) lower case to upper
c
      dimension cap(26), lc(26)
      character cap*1, lc*1
      character recin*232, recout*232
      
      data cap/'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K',
     1         'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 
     1         'W', 'X', 'Y', 'Z'/
      data lc /'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 
     1         'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
     1         'w', 'x', 'y', 'z'/
c
c _________________________________________________________
      
      iout=0
      recout=recin
c
c		Caps to lower case
      if(ntype.eq.1) then
        do i=1,nin
          do j=1,26     
            if(recout(i:i) .eq. cap(j)) recout(i:i) = lc(j)
          end do          
        end do
      else
        do i=1,nin
          do j=1,26     
            if(recout(i:i) .eq. lc(j)) recout(i:i) = cap(j)
          end do          
        end do        
      endif  
c
c _________________________________________________________
      
      if(iout.eq.1) then
        write(nlog,100) recin, recout
 100    format(/,' AdjCase;  recin  = ', a250,/
     1           '           recout = ', a250)
      endif
      return
      end     
c                            
c ************************************************************
        subroutine staStr(maxnf, iderr, i3, filout)
c
c               Similar to station, but handles a StateCU structure
c		            structure filw with "extra data" on lines 2-n
c               Note max dimension = 15 = (350 - 28)/21
        dimension id(7500), nin(5)
c       character filout*72, rec1*250, id*12
        character filout*72, rec1*250, id*12, recid*250, rec8*8,
     1    rec3*3  
c
c _________________________________________________________
c
c               Initilize
        n = 0                 
        nf = 10                    
        ndim=350

        do 100 i=1,maxnf
          call skipn(nf+i,26)
  100     nin(i) = 0

  110   nf=nf+1
        nf1= nf-10
c
c _________________________________________________________

  120   read(nf,'(a250)',end=130,err=160) rec1
c
c rrb 98/06/29
c       write(99,'(a250)') rec1
c
c _________________________________________________________
c               Check for blank card
        recid=rec1(1:250)
        call blank(recid, 96, ib)
c
c _________________________________________________________
 
        if(ib.eq.1) then
c
c               Check for an id indicator in first 3 columns or 12
          call blank(recid,i3, ib2)
          if(ib2.ne.0) then                   
            n = n+1     
            nin(nf1) = nin(nf1) + 1
            if(i3.ne.17) then
              id(n) = recid(1:12)
            else
              id(n) = recid(6:17)
            endif
          endif
c

          write(26,'(a250)') rec1
c
c _________________________________________________________
c rrb 2007/07/11; Update
c		Read "Extra Data" on lines 2-n          
          rec3=rec1(85:87)
          read(rec3,'(i3)') nextra
          do i=1,nextra
            read(nf,'(a250)',end=130,err=160) rec1
            write(26,'(a250)') rec1
          end do
c _________________________________________________________
c		Get another station
          goto 120
        endif
c
c _________________________________________________________
c
c               Check for other stations
  130   if(nf1.lt.maxnf) goto 110
c
c
c _________________________________________________________
c              Print processing results
        write(99,140) filout, (nin(j),j=1,maxnf), n
        write(98,140) filout, (nin(j),j=1,maxnf), n                     
  140   format(a72,/,2x, 20i5)
c
c
c _________________________________________________________
c              Check id's for duplicates
        call checkid(n, filout, id, iderr)
c
c
c _________________________________________________________
c              Close input and output file
        do 150 nf=11,10+maxnf
          close(nf)      
  150   continue
        close(26)
c
c _________________________________________________________

        return
c
c _________________________________________________________
c
c               Error Messages              
  160   write(99,*) '  StaStr; Problem reading station file ', nf
        write(6,*)  '  Unsuccessful termination, see smlink.log'
        write(99,*) '  Unsuccessful termination, see smlink.log'
        call flush(6)     
        goto 180
        
  170   write(99,*) '  StaStr; Problem reading station file ', nf
        write(99,*) '  Number of gages = ', n
        write(99,*) '  Max number of =   ', (ndim-28)/21
        write(99,*) '  Dimension of ', ndim, ' exceeded'
        write(99,*) '  Note max dimension approx = 28 + 21 * gages'
        write(6,*)  '  Unsuccessful termination, see smlink.log'
        write(99,*) '  Unsuccessful termination, see smlink.log'
        call flush(6)     
        goto 180

  180   stop 1
        end


c                            
c ************************************************************
        subroutine FnCopy(nlog, maxnf, iderr, i3, rec5, filout)
c
c               Similar to station, but it simply copies one file 
c               to the output file
        dimension id(7500), nin(5)
c       character filout*72, rec1*250, id*12
        character filout*72, rec1*250, id*12, recid*250, rec8*8,
     1    rec3*3, rec5*5 
c
c _________________________________________________________
c
c               Initilize
        n = 0                 
        nf = 10                    
        ndim=350
        nout=0
        nrec=0
        
        if(nout.eq.1) write(nlog,*) '  FnCopy; rec5 = ', rec5

        do 100 i=1,maxnf
          call skipn(nf+i,26)
  100     nin(i) = 0

  110   nf=nf+1
        nf1= nf-10
c
c _________________________________________________________

  120   read(nf,'(a250)',end=130,err=160) rec1    
        nrec=nrec+1
c
        write(26,'(a250)') rec1
c
c _________________________________________________________
c		Get another station
        goto 120
c
c _________________________________________________________
c		Print exit information
        
        
  130   continue
        write(99,140) filout
        write(98,140) filout                   
  140   format(a72) 
    
        write(99,132) rec5, nrec
  132   format(
     1  ' FnCopy; Copied first file successfuly. Fn = ', a5,/
     1  '         Number of records copied = ', i8)
c
c
c _________________________________________________________
c              Close input and output file
        do 150 nf=11,10+maxnf
          close(nf)      
  150   continue
        close(26)
c
c _________________________________________________________

        return
c
c _________________________________________________________
c
c               Error Messages              
  160   write(99,*) '  FnCopy; Problem reading station file ', nf
        write(6,*)  '  Unsuccessful termination, see smlink.log'
        write(99,*) '  Unsuccessful termination, see smlink.log'
        call flush(6)     
        goto 180

  180   stop 1
        end
