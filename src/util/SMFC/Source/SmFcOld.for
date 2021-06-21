      subroutine smfcOld(nlog, maxnf, iprint, maxsta, ifile,
     1  iwell,   ireach, isjrip, itsfile,
     1  ieffmax, isoil,  iday, fext, ext, desc, fpath)
      
c _________________________________________________________
c rrb 2007/12/18; Variables for Random Response File
c rrb 2011/08/18; Increase dimension from 75 to 80   
c
       dimension
     1   ifileNum(2,80), fileName(2,80), fileType(2,80),
     1   filetypX(80),   fileID(80), ifileNx(80)

       character filena*256,
     1   filetype*40, filetypX*40, FileName*256, FileId*5,
     1   fileT1*40,   fileT2*40, fileN1*256,
     1   filrsp*72
     
       character  fext*4,    fext1*1  
       character  ext(50)*4,  desc(50)*24,  fpath(2)*72 
                                                       
c                                                                       
c _________________________________________________________
c               Response file (*.rsp)                                   
       nf=1     
       nout=0                                                       
       call opener(nlog, maxnf,fpath,iall)
c                                                                       
c _________________________________________________________
c               Control file (*.ctl)                                    
	    nf=nf+1
	    if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
	
          call opentop(nlog, ix,maxnf,iall,1,filout,
     1        ext(nf),desc(nf),fpath)
	      call comp1(nlog, nout, maxnf,iprint)
	      ifile=ifile+1
	    endif
c                                                                       
c _________________________________________________________
c               River Network (*.rin)                                   

	    nf=nf+1
	    if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
	        write(nlog,*) ' SmFcOld; calling Opentop and CompID'
          call opentop(nlog, ix,maxnf,iall,1,filout,
     1       ext(nf),desc(nf),fpath)
          
    	  call compID(nlog, nout, maxnf,maxsta,iprint,0,ext(nf))
    	  ifile=ifile+1
    	endif
c                                                                       
c _________________________________________________________
c               Reservoir Stations (*.res)                              

	    nf=nf+1
	    if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
          call opentop(nlog, ix,maxnf,iall,1,filout,
     1     ext(nf),desc(nf),fpath)
     
	      call compSTA(nlog, nout, maxnf,maxsta,iprint,0,ext(nf))
	      ifile=ifile+1
	    endif
c                                                                       
c               Diversion Stations (*.dds)                              
c -------------------------------------------                           
	    nf=nf+1
	    if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
          call opentop(nlog, ix,maxnf,iall,1,filout,
     1     ext(nf),desc(nf),fpath)
     
          call compSTA(nlog, nout, maxnf,maxsta,iprint,2,ext(nf))
	      ifile=ifile+1
	    endif
c                                                                       
c               River Stations (*.ris)                                  
c -------------------------------------------                           
	    nf=nf+1
	    if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
          call opentop(nlog, ix,maxnf,iall,1,filout,
     1     ext(nf),desc(nf),fpath)

    	  call compID(nlog, nout, maxnf,maxsta,iprint,0,ext(nf))
    	  ifile=ifile+1
    	endif
c                                                                       
c               Instream Stations (*.ifs)                               
c -------------------------------------------                           
	    nf=nf+1
	    if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
          call opentop(nlog, ix,maxnf,iall,1,filout,
     1      ext(nf),desc(nf),fpath)

	      call compID(nlog, nout, maxnf,maxsta,iprint,0,ext(nf))
	      ifile=ifile+1
	    endif
c
c               Well Stations (*.wes)
c -------------------------------------------                           
	    nf=nf+1
c
c rrb 03/01/30
c       IF(iwell.eq.1) then
        IF(iwell.ne.0) then
          if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
            call opentop(nlog, ix,maxnf,iall,iwell,
     1             filout,ext(nf),desc(nf),fpath)
c
c rrb 03/01/30
            if(iwell.ge.1) then
              call compSTA(nlog,nout,   maxnf,maxsta,iprint,2,ext(nf))
            endif
	         ifile=ifile+1
	       endif
       endif
c                                                                       
c               Instream rights (*.ifr)                                 
c -------------------------------------------                           
	    nf=nf+1
	    if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
          call opentop(nlog, ix,maxnf,iall,1,filout,
     1      ext(nf),desc(nf),fpath)

	      call compID(nlog, nout,  maxnf,maxsta,iprint,0,ext(nf))
	      ifile=ifile+1
	    endif
c                                                                       
c               Reservoir rights (*.rer)                                
c -------------------------------------------                           
	    nf=nf+1
	    if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
          call opentop(nlog, ix,maxnf,iall,1,filout,
     1      ext(nf),desc(nf),fpath)

	      call compID(nlog,nout,  maxnf,maxsta,iprint,0,ext(nf))
	      ifile=ifile+1
	    endif
c                                                                       
c               Diversion rights (*.ddr)                                
c -------------------------------------------                           
	    nf=nf+1
	    if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
          call opentop(nlog, ix,maxnf,iall,1,filout,
     1      ext(nf),desc(nf),fpath)

	      call compID(nlog, nout, maxnf,maxsta,iprint,0,ext(nf))
	      ifile=ifile+1
	    endif
c                                                                       
c               Operation rights (*.opr)                                
c -------------------------------------------                           
	    nf=nf+1
	    if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
          call opentop(nlog, ix,maxnf,iall,1,filout,
     1      ext(nf),desc(nf),fpath)
c
c rrb 01/12/24; revise to compsta to allow more than 1 line per input
c         call compID(nlog, nout, maxnf,maxsta,iprint,0,ext(nf))
          call compSTA(nlog, nout, maxnf,maxsta,iprint,3,ext(nf))
	      ifile=ifile+1
	    endif
c
c               Well right (*.wer)
c -------------------------------------------                           
	    nf=nf+1
c
c rrb 03/01/30
c       IF(iwell.eq.1) then
        IF(iwell.ne.0) then
    	  if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
            call opentop(nlog, ix,maxnf,iall,iwell,
     1             filout,ext(nf),desc(nf),fpath)
c
c rrb 03/01/30
            if(iwell.ge.1) then
              call compID(nlog, nout, maxnf,maxsta,iprint,0,ext(nf))
            endif
	        ifile=ifile+1
	      endif
        endif
c                                                                       
c               Precipitation (*.pre)                                   
c -------------------------------------------                           
	    nf=nf+1
	    if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
          call opentop(nlog, ix,maxnf,iall,1,filout,
     1      ext(nf),desc(nf),fpath)

	      call compT(nlog, nout, maxnf,maxsta,iprint,0,ext(nf))
	      ifile=ifile+1
	    endif
c                                                                       
c               Evaporation (*.eva)                                     
c -------------------------------------------                           
	    nf=nf+1
	    if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
          call opentop(nlog, ix,maxnf,iall,1,filout,
     1      ext(nf),desc(nf),fpath)

	      call compID(nlog, nout,  maxnf,maxsta,iprint,0,ext(nf))
	      ifile=ifile+1
	    endif
c                                                                       
c               River Baseflows (*.rim)
c -------------------------------------------                           
	    nf=nf+1
	    if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
          call opentop(nlog, ix,maxnf,iall,1,filout,
     1      ext(nf),desc(nf),fpath)

	      call compT(nlog, nout,  maxnf,maxsta,iprint,0,ext(nf))
	      ifile=ifile+1
	    endif
c                                                                       
c               Diversion Demand-Monthly (*.ddm)                        
c -------------------------------------------                           
	    nf=nf+1
	    if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
          call opentop(nlog, ix,maxnf,iall,1,filout,
     1      ext(nf),desc(nf),fpath)

	      call compT(nlog, nout,  maxnf,maxsta,iprint,0,ext(nf))
	      ifile=ifile+1
	    endif
c                                                                       
c               Diversion Override (*.ddo)                              
c -------------------------------------------                           
	    nf=nf+1
	    if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
          call opentop(nlog, ix,maxnf,iall,1,filout,
     1      ext(nf),desc(nf),fpath)

	      call compID(nlog, nout,  maxnf,maxsta,iprint,0,ext(nf))
	      ifile=ifile+1
	    endif
c                                                                       
c               Diversion Demand-Annual (*.dda)                         
c -------------------------------------------                           
	    nf=nf+1
	    if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
          call opentop(nlog, ix,maxnf,iall,1,filout,
     1      ext(nf),desc(nf),fpath)

     	  call compID(nlog, nout,  maxnf,maxsta,iprint,0,ext(nf))
     	  ifile=ifile+1
     	endif
c
c               Instream Demand-Monthly (*.ifm)
c -------------------------------------------                           
	    nf=nf+1
c
c rrb 03/01/30
      IF(ireach.ge.2) then
	      if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
            call opentop(nlog, ix,maxnf,iall,1,filout,
     1        ext(nf),desc(nf),fpath)

	      call compT(nlog, nout,  maxnf,maxsta,iprint,0,ext(nf))
	      ifile=ifile+1
	      endif
      endif
c
c               Instream Demand-Annual (*.ifa)                          
c -------------------------------------------                           
	    nf=nf+1
	    if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
          call opentop(nlog, ix,maxnf,iall,1,filout,
     1      ext(nf),desc(nf),fpath)

	      call compT(nlog, nout,  maxnf,maxsta,iprint,0,ext(nf))
	      ifile=ifile+1
	    endif
c
c               Well Demand (*.wem)
c -------------------------------------------                           
	    nf=nf+1
c
c rrb 03/01/30; rrb
c     IF(iwell.eq.1) then
      IF(iwell.ne.0) then
  	    if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
          call opentop(nlog, ix,maxnf,iall,iwell,
     1           filout,ext(nf),desc(nf),fpath)
c
c rrb 03/01/30
            if(iwell.ge.1) then
              call compT(nlog, nout,  maxnf,maxsta,iprint,0,ext(nf))
            endif
	          ifile=ifile+1
          endif
	    endif
c                                                                       
c               Delay Table (*.dly)                                     
c -------------------------------------------                           
	    nf=nf+1
	    if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
          call opentop(nlog, ix,maxnf,iall,1,filout,
     1      ext(nf),desc(nf),fpath)

    	  call compSTA(nlog, nout,  maxnf,maxsta,iprint,1,ext(nf))
    	  ifile=ifile+1
    	endif
c                                                                       
c               Reservoir Target (*.tar)                                
c -------------------------------------------                           
	    nf=nf+1
	    if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
          call opentop(nlog, ix,maxnf,iall,1,filout,
     1      ext(nf),desc(nf),fpath)

c         itest=1
c         if(itest.eq.0) then
c
c               Check min (is=1)
	          call compT(nlog, nout,  maxnf,maxsta,iprint,1,ext(nf))
c         endif
c
c               Check max (is=-1)
c         itest=1
c         if(itest.eq.0) then
	       call compT(nlog, nout,  maxnf,maxsta,iprint,-1,ext(nf))
c              endif
	       ifile=ifile+1
	     endif
c
c               San Juan Sediment File (*.sjr)
c -------------------------------------------
	    nf=nf+1
c
c rrb 03/01/30
c       IF(isjrip.eq.1) then
        IF(isjrip.ne.0) then
	  if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
            call opentop(nlog, ix,maxnf,iall,isjrip,
     1             filout,ext(nf),desc(nf),fpath)
c
c rrb 03/01/30
            if(isjrip.ge.1) then
              call compT(nlog, nout,  maxnf,maxsta,iprint,0,ext(nf))
            endif
	    ifile=ifile+1
	  endif
        endif
c
c               Annual Time Series File (*.tsp)
c -------------------------------------------                           
	nf=nf+1
c
c rrb 03/01/30
c       IF(itsfile.ge.1) then
        IF(itsfile.ne.0) then
  	  if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
            call opentop(nlog, ix,maxnf,iall,itsfile,
     1            filout,ext(nf),desc(nf),fpath)
c
c rrb 03/01/30
            if(itsfile.ge.1) then
              call compT(nlog, nout,  maxnf,maxsta,iprint,0,ext(nf))
            endif
	    ifile=ifile+1
          endif
	endif
c
c               Annual Water Requirement (IWR) File (*.iwr)
c -------------------------------------------                           
	nf=nf+1
c
c rrb 03/01/30
c       IF(ieffmax.ge.1) then
        IF(ieffmax.ne.0) then
	  if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
            call opentop(nlog, ix,maxnf,iall,ieffmax,
     1             filout,ext(nf),desc(nf),fpath)
c
c rrb 03/01/30
            if(ieffmax.ge.1) then
              call compT(nlog, nout,  maxnf,maxsta,iprint,0,ext(nf))
            endif
	    ifile=ifile+1
	  endif
        endif
c
c               Soil Parameter Data (*.par)
c -------------------------------------------                           
	nf=nf+1
c
c rrb 03/01/30
c       IF(isoil.ge.1) then
        IF(isoil.ne.0) then
	  if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
            call opentop(nlog, ix,maxnf,iall,isoil,
     1             filout,ext(nf),desc(nf),fpath)
c
c rrb 03/01/30
            if(isoil.ge.1) then
              call compID(nlog, nout,  maxnf,maxsta,iprint,0,ext(nf))
            endif
	    ifile=ifile+1
	  endif
        endif
c
c               Reservoir EOM (*.eom)                                   
c -------------------------------------------                           
	nf=nf+1
	if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
          call opentop(nlog, ix,maxnf,iall,1,filout,
     1      ext(nf),desc(nf),fpath)

	  call compT(nlog, nout,  maxnf,maxsta,iprint,0,ext(nf))
	  ifile=ifile+1
	endif
c                                                                       
c               River Baseflow Info (*.rib)                             
c -------------------------------------------                           
	nf=nf+1
	if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
          call opentop(nlog, ix,maxnf,iall,1,filout,
     1      ext(nf),desc(nf),fpath)

	  call compSTA(nlog, nout,  maxnf,maxsta,iprint,0,ext(nf))
	  ifile=ifile+1
	endif
c                                                                       
c               Historical Streamflow (*.rih)                           
c -------------------------------------------                           
	nf=nf+1
	if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
          call opentop(nlog, ix,maxnf,iall,1,filout,
     1      ext(nf),desc(nf),fpath)

	  call compT(nlog, nout,  maxnf,maxsta,iprint,0,ext(nf))
	  ifile=ifile+1
	endif
c                                                                       
c               Historical Diversions (*.ddh)                           
c -------------------------------------------                           
	nf=nf+1
	if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
          call opentop(nlog, ix,maxnf,iall,1,filout,
     1     ext(nf),desc(nf),fpath)

	  call compT(nlog, nout,  maxnf,maxsta,iprint,0,ext(nf))
	  ifile=ifile+1
	endif
c
c               Historical Well Pumping (*.weh)
c -------------------------------------------                           
	nf=nf+1
c
c rrb 03/01/30
c       IF(iwell.ge.1) then
        IF(iwell.ne.0) then
	  if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
            call opentop(nlog, ix,maxnf,iall,iwell,
     1             filout,ext(nf),desc(nf),fpath)
c
c rrb 03/01/30
            if(iwell.ge.1) then
              call compT(nlog, nout,  maxnf,maxsta,iprint,0,ext(nf))
            endif
	    ifile=ifile+1
	  endif
        endif
c
c               GIS (*.gis)                                             
c -------------------------------------------                           
	nf=nf+1
	if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
          call opentop(nlog, ix,maxnf,iall,1,filout,
     1     ext(nf),desc(nf),fpath)
	  call compID(nlog, nout,  maxnf,maxsta,iprint,0,ext(nf))
	  ifile=ifile+1
	endif
c                                                                       
c               Output Control (*.out)
c ------------------------------------------- 
	nf=nf+1 
	if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then             
          call opentop(nlog, ix,maxnf,iall,1,filout,
     1      ext(nf),desc(nf),fpath)

	  call compID(nlog, nout,  maxnf,maxsta,iprint,1,ext(nf))
	  ifile=ifile+1
	endif
c
c               Daily Streamflow (*.rid)
c -------------------------------------------
	nf=nf+1
        IF(iday.eq.1) then
  	  if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
            call opentop(nlog, ix,maxnf,iall,1,filout,
     1        ext(nf),desc(nf),fpath)

	    call compT(nlog, nout,  maxnf,maxsta,iprint,0,ext(nf))
	    ifile=ifile+1
	  endif
        endif
c
c               Daily Demand (*.ddd)
c -------------------------------------------
	nf=nf+1
        IF(iday.eq.1) then
  	  if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
            call opentop(nlog, ix,maxnf,iall,1,filout,
     1        ext(nf),desc(nf),fpath)

	    call compT(nlog, nout,  maxnf,maxsta,iprint,0,ext(nf))
	    ifile=ifile+1
	  endif
        endif
c
c               Daily ISF Demand (*.ifd)
c -------------------------------------------
	nf=nf+1
        IF(iday.eq.1) then
  	  if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
            call opentop(nlog, ix,maxnf,iall,1,filout,
     1        ext(nf),desc(nf),fpath)

	    call compT(nlog, nout,  maxnf,maxsta,iprint,0,ext(nf))
	    ifile=ifile+1
	  endif
        endif
c
c               Daily Well Demand (*.wed)
c -------------------------------------------
	nf=nf+1
c
c rrb 03/01/30
c       IF(iday.eq.1 .and. iwell.ge.1) then
        IF(iday.eq.1 .and. iwell.ne.0) then
  	  if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
            call opentop(nlog, ix,maxnf,iall,iwell,
     1       filout,ext(nf),desc(nf),fpath)
c
c rrb 03/01/30
            if(iwell.ge.1) then
              call compT(nlog, nout, maxnf,maxsta,iprint,0,ext(nf))
            endif
	    ifile=ifile+1
	  endif
        endif
c
c               Daily Res target (*.tad)
c -------------------------------------------
	nf=nf+1
        IF(iday.eq.1) then
  	  if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
            call opentop(nlog, ix,maxnf,iall,1,filout,
     1        ext(nf),desc(nf),fpath)

	    call compT(nlog, nout,maxnf, maxsta,iprint,0,ext(nf))
	    ifile=ifile+1
	  endif
        endif
c
c               Daily Delay Table (*.dld)
c -------------------------------------------
	nf=nf+1
        IF(iday.eq.1) then
  	  if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
            call opentop(nlog, ix,maxnf,iall,1,filout,
     1        ext(nf),desc(nf),fpath)

  	    call compSTA(nlog, nout, maxnf,maxsta,iprint,1,ext(nf))
	    ifile=ifile+1
	  endif
        endif
c
c               Daily IWR (*.iwd)
c -------------------------------------------
	nf=nf+1
c
c rrb 03/01/30
c       IF(iday.eq.1 .and. ieffmax.ge.1) then
        IF(iday.eq.1 .and. ieffmax.ne.0) then
  	  if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
            call opentop(nlog, ix,maxnf,iall,ieffmax,
     1         filout,ext(nf),desc(nf),fpath)
c
c rrb 03/01/30
            if(ieffmax.ge.1) then
              call compT(nlog, nout, maxnf,maxsta,iprint,0,ext(nf))
            endif
	    ifile=ifile+1
	  endif
        endif
c
c               Daily Historic Stream (*.rih)
c -------------------------------------------
	nf=nf+1
        IF(iday.eq.1) then
  	  if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
            call opentop(nlog, ix,maxnf,iall,1,filout,
     1        ext(nf),desc(nf),fpath)

	    call compT(nlog, nout, maxnf,maxsta,iprint,0,ext(nf))
	    ifile=ifile+1
	  endif
        endif
c
c               Daily Historic Diversion (*.ddy)
c -------------------------------------------
	nf=nf+1
        IF(iday.eq.1) then
  	  if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
            call opentop(nlog, ix,maxnf,iall,1,filout,
     1        ext(nf),desc(nf),fpath)

	    call compT(nlog, nout, maxnf,maxsta,iprint,0,ext(nf))
	    ifile=ifile+1
	  endif
        endif
c
c               Daily Historic Well Pumping (*.wey)
c -------------------------------------------
	nf=nf+1
c
c rrb 03/01/30
c       IF(iday.eq.1 .and. iwell.eq.1) then
        IF(iday.eq.1 .and. iwell.ne.0) then
  	  if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
            call opentop(nlog, ix,maxnf,iall,iwell,
     1             filout,ext(nf),desc(nf),fpath)
c
c rrb 03/01/30
            if(iwell.ge.1) then
              call compT(nlog, nout, maxnf,maxsta,iprint,0,ext(nf))
            endif
	    ifile=ifile+1
	  endif
        endif
c
c               Daily Historic Reservoir EOY (*.eoy)
c -------------------------------------------
	nf=nf+1
        IF(iday.eq.1) then
  	  if(fext.eq.'.rsp' .or. fext.eq.ext(nf)) then
            call opentop(nlog, ix,maxnf,iall,1,filout,
     1        ext(nf),desc(nf),fpath)
	    call compT(nlog, nout, maxnf,maxsta,iprint,0,ext(nf))
	    ifile=ifile+1
	  endif
        endif

c
c               Output File (*.x**)
c ------------------------------------------- 
	    nf=nf+1
	    fext1=fext(2:2)
	    if(fext1.eq.'x' .or. fext1.eq.'X') then
	      write(6,200) fext
	      write(nlog,200) fext	      
 200    format(' SmFcOld; Processing an output file ',a4)
 
        call opentop(nlog, ix,maxnf,iall,i,filout,
     1      ext(nf),desc(nf),fpath)
        nout=nout+1

	      call compOut(nlog, nout, maxnf,iprint,ext(nf))  
	      ifile=ifile+1
	    endif

      return
      end