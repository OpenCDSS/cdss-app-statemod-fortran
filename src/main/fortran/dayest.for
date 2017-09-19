
C     Last change:  C    19 May 97   11:55 pm
c
      subroutine dayest(iin,i12)
c
c _________________________________________________________
c	Program Description
c        Dayest; it opens daily files and reads them 1x/month
c
c             Dayest.for; Read and/or Estimate daily data
c             When ii2.eq.0; Called by Execute or Virgen
c                            1x per simulation
c             When ii2.gt.0; Called by Execute or Vircom
c                            1x per month
c             Note the daily delay and daily IWR data files are 
c               opened in a different order when called by baseflow 
c               Vs simulate.  This allows same variables to be used
c               by both (e.g. streamflow is baseflow in simulate mode 
c               and streamflow is historic in baseflow mode)
c
c       For simulate, called by Execut
c	For Baseflow called by Virgen 1x/simulation and 
c           Vircom every month
c
c __________________________________________________________
c
c       Update History
c
c rrb 01/02/01; Revise read file numbers 
c rrb 01/02/19; Add Demand type checks 
c               Add daily consumptive req. file read
c rrb 04/05/06; Make detailed streamflow output total not gains
c
c
c __________________________________________________________
c
c       Documentation                       
c
c       iin         = response file #
c       i12         = 0 open files and check POR
c                     1 read data
c       idaily      = 0 N/A monthly model
c                 1 = daily gage id for pattern
c                -1 = divide by # of days per month
c
c	ichk6=ichk from control file
c       ichk6       = 0 no detailed printout in daydist
c                   = 1 detailed printout in daydist
c
c	ichk8=ichk from control file
c       ichk8       = 0 no detailed streamflow output
c                     1 detailed streamflow output
c
c       isim        = 0 non simulate (baseflow) option
c                     1 simulate option
c
c
c _________________________________________________________
c	Dimensions
c
      INCLUDE 'common.inc'
      character rec3*3
c
c rrb 02/01/15; Dimension clean up (to common)
c     dimension dumd(32,2001)
c
c
c _________________________________________________________
c		Initilzie
c
c     write(6,*) '  Dayest; iin, i12', iin, i12
c
c
c		iout = 1 print downstream call data
      iout=0
      ichk6=0
      ichk8=0
      if(ichk.eq.6) then
        ichk6=1
        ichk8=1
      endif  

      if(ichk.eq.8) ichk8=1

      small=0.01
c

c
c               Set file type ID depending on simulate or baseflow mode 
c		Note the following is only used for detailed reporting
      if(ioptio.eq.2 .or. ioptio.eq.8) then
	isim=1

	ityps=1
	itypd=2
	itypr=4
	itypw=6
      else
	isim=0

	ityps=11
	itypd=12
	itypr=14
	itypw=16
      endif

      if(ioptio.eq.1 .or. ioptio.eq.9) then
	ibasef=1
      else
	ibasef=0
      endif
c
c ==========================================================
c               Once per simulation 
c               Open files, check POR, read daily delay data
       if(i12.eq.0) then
c
c __________________________________________________________
c                Step 1; If in baseflow Mode
c                        Open Daily Return patterns         
	 if(isim.eq.0) then
          write(nlog,102) 
 102      format(/, 72('_'),/    
     1    ' Dayest; Opening Daily Return Data (*.dld)')
           if(infile.eq.1) then
             ifn= 42
             filena=fileName(ifn)
           else
             read (iin,'(a72)',end=510,err=510) filena
           endif
c
c rrb 99/05/20
	   call putpath(maxfn, filena, fpath1)
	   open(87, file=filena,status='old',err=510)
c
	   write(io99,'(5x, a72)') filena
	   call skipn(87)
c
c __________________________________________________________
c                Step 2; If in Baseflow mode
c                        Open Daily IWR (ddc) data         
           
	   if(ieffmax.ne.0) then

             if(infile.eq.1) then
               ifn= 41
               filena=fileName(ifn)
             endif
             
             write(nlog,104) 
 104         format(/, 72('_'),/    
     1       ' Dayest; Opening Daily Return Data  IWR (*.iwd)')
	     nx=numdiv+numdivw
c
	     ioptiox=2
	     call chekpor(iin, 88, 99, -1, ioptiox, nx, iyrmo(1),  
     1                    imstr, iday, 22, c, cyr1, maxfn,
     1                    infile, idummy, nDdcX, fpath1, filena)
	   endif

	 endif
c            
c __________________________________________________________ 
c                Step 1; Open Daily Base Streamflow (simulate)
c                        or Daily historic streamflow (baseflow)

	 write(io99,*) ' '
	 if(ibasef.eq.0) then
           if(infile.eq.1) then
             ifn= 36
             filena=fileName(ifn)
           endif

	   write(io99,*) ' Dayest; Opening daily streamflow (*.rid)'
	 else
           if(infile.eq.1) then
             ifn= 43
             filena=fileName(ifn)
           endif
           write(nlog,106) 
 106       format(/, 72('_'),/    
     1     ' Dayest; Opening Daily Historic Streamflow (*.riy)')
	 endif
c
	 ioptiox=2
	 write(nlog,*) ' Dayest; numrun', numrun
	 call chekpor(iin, 81, 99, -1, ioptiox, numrun, iyrmo(1),
     1                imstr, iday, 13, c, cyr1, maxfn,
     1                infile, idummy, nRiyX, fpath1, filena)
c
c __________________________________________________________ 
c                Step 2; Open Daily Diversion Demands        
	 write(io99,*) ' '
	 if(ibasef.eq.0) then
           if(infile.eq.1) then
             ifn=37
             filena=fileName(ifn)
           endif
           
          write(nlog,108) 
 108      format(/, 72('_'),/    
     1    ' Dayest; Opening Daily Demands (*.ddd)')
	 else
           if(infile.eq.1) then
             ifn=44
             filena=fileName(ifn)
           endif
           
          write(nlog,111) 
 111      format(/, 72('_'),/    
     1    ' Dayest; Opening Daily Historic Diversions (*.ddy)')
	 endif
c
	 ioptiox=2
	 call chekpor(iin, 82, 99, -1, ioptiox, numdiv, iyrmo(1),
     1                imstr, iday, 14, c, cyr1, maxfn,
     1                infile, idummy, nDdyX, fpath1, filena)
c
c __________________________________________________________
c                Step 3; Open Daily Instream Flow demand (simulate)        
	 if(isim.eq.1) then
           if(infile.eq.1) then
             ifn=38
             filena=fileName(ifn)
           endif

	   write(io99,*) ' '
	   write(io99,*) ' Dayest; Opening Daily InstreamFlow (*.ifd)'
	   ioptiox=2

	   call chekpor(iin, 83, 99, -1, ioptiox, numifr, iyrmo(1),
     1                imstr, iday, 15, c, cyr1, maxfn,
     1                infile, idummy, nIfdX, fpath1, filena)
	 endif
c
c __________________________________________________________ 
c                Step 4; Open Daily Well Demands (simulate)         
c                        or daily historic diversions (baseflow)
	 if(iwell.ne.0) then
	   write(io99,*) ' '
	   if(ibasef.eq.0) then
             if(infile.eq.1) then
               ifn=39
               filena=fileName(ifn)
             endif
             write(nlog,112) 
 112      format(/, 72('_'),/    
     1    ' Dayest; Opening Daily Well Demand (*.wed)')
           else
             if(infile.eq.1) then
               ifn=45
               filena=fileName(ifn)
             endif   
             write(nlog,114) 
 114         format(/, 72('_'),/    
     1      ' Dayest; Opening Daily Historic Well Pumping (*.wey)')
           endif
c
           ioptiox=2
	   call chekpor(iin, 84, 99, -1, ioptiox, numdivw, iyrmo(1),
     1                  imstr, iday, 23, c, cyr1, maxfn,
     1                  infile, idummy, nWeyX, fpath1, filena)

	 endif
c
c __________________________________________________________
c                Step 5; Open Daily Reservoir Targets (simulate)
c                        or daily EOD contents (baseflow)
	 write(io99,*) ' '
	 if(ibasef.eq.0) then
           if(infile.eq.1) then
             ifn=40
             filena=fileName(ifn)
           endif
          write(nlog,116) 
 116      format(/, 72('_'),/    
     1    ' Dayest; Opening Daily Reservoir Targets (*.tad)')
	 else
           if(infile.eq.1) then
             ifn=46
             filena=fileName(ifn)
           endif
           write(nlog,118) 
 118      format(/, 72('_'),/    
     1    ' Dayest; Opening Daily Historic Resrvoir EOD (*.eoy)')
	 endif 
c
	 ioptiox=2
	 call chekpor(iin, 85, 99, -1, ioptiox, numres, iyrmo(1),
     1                imstr, iday, 16, c, cyr1, maxfn,
     1                infile, idummy, nTadX, fpath1, filena)
c
c __________________________________________________________
c                Step 6; Open Daily Reservoir Evaporation 
c        write(io99,*) ' '
c        write(io99,*) ' Dayest; Opening Daily Res. Evap. (*.evd)'
c        call chekpor(iin, 86, 99, -1, ioptio, numres, iyrmo(1),
c                     imstr, iday, 16, c, cyr1, nEvdX, maxfn, fpath1)
c
c __________________________________________________________
c                Step 7; If in simulate Mode
c                        Open Daily Return patterns         
	 if(isim.eq.1) then
           if(infile.eq.1) then
             ifn=42
             filena=fileName(ifn)
           else
             read (iin,'(a72)',end=510,err=510) filena
           endif
          write(nlog,121) 
 121      format(/, 72('_'),/    
     1    ' Dayest; Opening Daily Return Data (*.dld)')

c
c rrb 99/05/20
	   call putpath(maxfn, filena, fpath1)
	   open(87, file=filena,status='old',err=510)
c
	   write(io99,'(5x, a72)') filena
	   call skipn(87)                     
c
c __________________________________________________________
c                Step 8; If in baseflow mode
c                        Open Daily IWR (ddx?) data         
	   if(ieffmax.ne.0) then
             if(infile.eq.1) then
               ifn=41
               filena=fileName(ifn)
             endif
            write(nlog,122) 
 122        format(/, 72('_'),/    
     1    ' Dayest; Opening Daily IWR Data (*.iwd)')

	     nx=numdiv+numdivw
c
	     ioptiox=2
	     call chekpor(iin, 88, 99, -1, ioptiox, nx, iyrmo(1),  
     1                    imstr, iday, 22, c, cyr1, maxfn,
     1                    infile, idummy, nDdxX, fpath1, filena)
	   endif
	 endif     
c
c __________________________________________________________
c               Step 9; Read Daily Return Flows (*.dld)
c                    1x per simulation)
	 ctot = 0.0
	 ndlymx = 0
	 numdld = 0
	 iprint = 0
	 cout = 0.0
c
	 if(interv.eq.-100) then
	   cfac=float(iabs(interv))
	   cfac=amax1(cfac,100.0)
	 else
	   cfac=1.0
	 endif
c
c ----------------------------------------------------------
c               Step 9a; Begin return flow loop
c
	 do idl=1,maxdly+1
	   if(interv.gt.0) then
	     write(io99,120)
	     goto 9999
	   else
	     read(87,*,end=100,err=510)
     1       irtnid(idl),ndly(idl), (dlyratd(i,idl),i=1,ndly(idl))

	     ndlymx=amax0(ndlymx,ndly(idl))
	     numdld=ndlymx
	     if(ndly(idl).gt.maxdld) then
	       write(io99,110) irtnid(idl),maxdly
	       goto 9999
	     endif
c
c ----------------------------------------------------------
c               Step 9b; Return Flow - adjust units and total
c
c
c rrb 01/11/30; Initilize 
	     dlytot(idl)=0.0 
c
c rrb 01/11/30; Test set day 1 = 0
c            dlyratd(1,idl)=0.0 

	     ctot = 0.0  

	     do i=1,ndly(idl)
	       dlyratd(i,idl)=dlyratd(i,idl)*cfac
	       dlytot(idl)=dlytot(idl) + dlyratd(i,idl)
	       ctot = ctot + dlyratd(i,idl)
	     end do
c
c rrb 01/10/08; Reset to 0 if the sum is less than 1%
	     iset=0
	     if(iset.eq.1) then
	       if(ctot.lt.1.0) then
		 if(iprint.eq.0) write(io99,160)

		 iprint=iprint+1
		 cout=cout+ctot

		 write(io99,161) iprint, irtnid(idl),ctot, cout
		 ndly(idl)=1
		 dlyratd(1,idl) = 0.0
		 dlytot(idl) = 0.0
	       endif
	     endif
c
	   endif
	 end do
c
c ----------------------------------------------------------
c               Step 9c; Return Flow - warn if dimension exceeded
c
	 write(io99,110) maxdly
	 goto 9999
c
c
c ----------------------------------------------------------
c               Step 9d; Return Flow - set delay counter and close

  100    numdly=idl-1
	 close (87)
c
c
c ----------------------------------------------------------
c               Step 9e; Return Flow - calculate loss
c rrb 01/11/30; Calculate loss data based on daily returns, etc.
	 write(io99,*) ' Dayest; calling closs'
	 call closs(1)
c
c
c ----------------------------------------------------------
c               Step 9e; Open Downstream Call file
cr    if(idcall.ne.0) then
	write(nlog,809)
	Write(6,809)
 809    format(/,'  Mdainp; Downstream Call File (*.cal) Daily')

        if(infile.eq.1) then
          ifn=51
          rec256=fileName(ifn)
        endif
c
c		Set year type, etc
c              (nann=-1 for daily, idayx=2 for downstream call)
        nann=-1
        idayx=2
        iystr1=iyrmo(1)
c
c rrb 05/06/21; revise to use correct year type        
        call chekpor(iin, 90, 99, nann,      0, 1, iyrmo(1),  
     1                imstr, idayx, ifn, c, cyr1, maxfn,
     1                infile, idummy, nCalX, fpath1, rec256)
c
c _________________________________________________________
c               End of once per initilization
c
c rrb 03/01/16; Provide header for detailed baseflow output
         if(ichk8.eq.1) then
cr         call outtop(96, -1, 48)
           call outtop(96, -2, 48)
         endif                 

	 goto 500

       endif
c
c =========================================================
c               Daily processing 1x/month
      if(i12.ne.0) then
c
c __________________________________________________________
c               Step 10; Daily Streamflow
	if(numrun.gt.0) then
c         ichk6 = 1
	  if(ibasef.eq.0) nfx=1
	  if(ibasef.eq.1) nfx=8
          call daydata(ichk6,81,nfx,maxsta,numrun,nlog,
     1      iyrmo(mon),imomo(mon), imd, virindx, crunidx)
c
c rrb 00/08/17; Moved to here, above gain calculations after editing
c               mdainp to store total flow (virinpT), 
c               not just gains (virinp)
	  call daydist(ichk8, ityps,iyrmo(mon),imomo(mon),mon,imd,
     1         mthday, maxsta, numrun, idays,
     1         virinpT,virindx,virind,crunidy,crunidx,crunid)
c
c ----------------------------------------------------------
c               Step X; Print Daily Streamflow Total
 
          if(ichk8.eq.1) then
            c = faf(mon)
            do np=1,numrun
c
c rrb 2004/05/06; Need station ID            
              write(96,780) iyrmo(mon), imomo(mon),
     1          crunid(np), (virind(id,np), id=1,31), virind(32,np)*c
c780          format(i5,  i5, 1x, a12, 31f8.2, f10.0)
 780          format(i4,  i4, 1x, a12, 31f8.2, f10.0)
            end do
          endif
c
c               Note calculate gains if data is total flow (iopflo=1) 
c               or we are in a base flow option (ioptio=1) which
c               always reads total flow
c
c ----------------------------------------------------------
c               Step 10a; Streamflow - Calculate gains 

c         if(iopflo.eq.1 .or. ioptio.eq.1 .or. ioptio.eq.9) then
	  if(iopflo.eq.1 .or. isim.eq.0) then

c
	    do np=1,numrun
	      do id=1,32
		dumd(id,np)=virind(id,np)
	      end do
	    end do

	    do ir=1,numrun
	      irr=irudnd(ir)
	      if(irr.ne.0) then
		do id=1,32
		  dumd(id,irr)=dumd(id,irr)-virind(id,ir)
		end do
	      endif
	    end do
c
	    do np=1,numrun
	      do id=1,32
		virind(id,np)=dumd(id,np)
	      end do
	    end do
	  endif
c	  
c
c ----------------------------------------------------------
c               Step 10b; Streamflow Gains - Check
cr 04/05/06; Moved above to print total not gains
cr 
cr          if(ichk8.eq.1) then
cr            c = faf(mon)
cr            do np=1,numrun
c
c rrb 2004/05/06; Need station ID            
cr              write(96,780) iyrmo(mon), imomo(mon),
cr     1          crunid(np), (virind(id,np), id=1,31), dumd(32,np)*c
cr 780          format(i5,  i5, 1x, a12, 31f8.2, f8.2)
cr            end do
cr          endif
        endif
c
c
c __________________________________________________________
c               Step 11; Diversion Demands
	if(numdiv.gt.0) then
	  if(ibasef.eq.0) nfx=2
	  if(ibasef.eq.1) nfx=9

          call daydata(ichk6,82,nfx,maxdiv,numdiv,nlog,
     1      iyrmo(mon),imomo(mon), imd, diverdx, cdividx)
c
c ---------------------------------------------------------
c               Step 11a; Diversions - Process input options
c rrb 01/02/19; Add demand options
	  do nd=1,numdiv
c
c ---------------------------------------------------------  
c               Step 11b; Diversions - input as demand idvcom=1
c
	    if(idvcom(nd).eq.1) then
	      do i=1,32
		diverd2(i,nd)=diverdx(i,nd)*diveff(mon,nd)/100.
	      end do
	    endif
c
c ---------------------------------------------------------  
c               Step 11c; Diversions - input as IWR idvcom=1
c                                                 
	    if(idvcom(nd).eq.3) then
	      do i=1,32
		diverd2(i,nd)=diverdx(i,nd)
		diverdx(i,nd)=diverdx(i,nd)/(diveff(mon,nd)/100.)
	      end do                        
	    endif
	  end do
c
c ---------------------------------------------------------  
c               Step 11d; Diversions - distribute daily af demands
c
	  call daydist(ichk6,itypd,iyrmo(mon),imomo(mon),mon,imd,
     1      mthday,maxdiv, numdiv,idayd,
     1      diver,diverdx,diverd,cdividy,cdividx,cdivid)
c
c ---------------------------------------------------------  
c               Step 11e; Diversion IWR - distribute daily IWR based
c                         on demand, not IWR data (see Step 16)
c
	  call daydist(ichk6,7,iyrmo(mon),imomo(mon),mon,imd,
     1      mthday, maxdiv, numdiv, idayd,
     1      diverir,diverd2,diverird,cdividy,cdividx,cdivid)
	endif
c
c __________________________________________________________
c               Step 12; Instream Flow Demand
c rrb 01/12/28; Simulate only
c       if(numifr.gt.0) then        
        if(isim.eq.1 .and. numifr.gt.0) then
          call daydata(ichk6,83,3,maxifr,numifr,nlog,
     1      iyrmo(mon),imomo(mon), imd, flowrdx, cifridx)
	  call daydist(ichk6,3,iyrmo(mon),imomo(mon),mon,imd,
     1      mthday, maxifr, numifr, idayi,
     1      flowr,flowrdx,flowrd,cifridy,cifridx,cifrid)
	endif
c
c __________________________________________________________
c               Step 13; Well Demands
	if(iwell.gt.0 .and. numdivw.gt.0) then
	  if(ibasef.eq.0) nfx=4
	  if(ibasef.eq.1) nfx=10

          call daydata(ichk6,84,nfx,maxdivw,numdivw,nlog,
     1      iyrmo(mon),imomo(mon), imd, diverdxw, cdividxw)
c
c ---------------------------------------------------------
c               Step 13a; Well Demands - Process input options
c rrb 01/02/19; Add demand options
	  do nd=1,numdivw
c
c ---------------------------------------------------------  
c               Step 13b; Well Demands - input as demand idvcom=1
c
	    if(idvcomw(nd).eq.1) then
	      do i=1,32
		diverd2(i,nd)=diverdxw(i,nd)*diveffw(mon,nd)/100.
	      end do
	    endif
c
c ---------------------------------------------------------  
c               Step 13c; Well Demands - input as IWR idvcom=1
c                                                 
	    if(idvcomw(nd).eq.3) then
	      do i=1,32
		diverd2(i,nd)=diverdxw(i,nd)
		diverdxw(i,nd)=diverdxw(i,nd)/(diveffw(mon,nd)/100.)
	      end do                        
	    endif
	  end do
c
c ---------------------------------------------------------  
c               Step 13d; Well Demands - distribute daily af demands
c

	  call daydist(ichk6,itypw,iyrmo(mon),imomo(mon),mon,imd,
     1      mthday, maxdivw,numdivw,idaydw,
     1      diverw,diverdxw,diverdw,cdividyw,cdividxw,cdividw)
c
c ---------------------------------------------------------  
c               Step 13e; Well IWR based on demand - distribute 
c                         daily IWR based on demand (not IWR data)
c
	  call daydist(ichk6,8,iyrmo(mon),imomo(mon),mon,imd,
     1      mthday, maxdivw, numdivw, idaydw,
     1      diverw,diverd2,diveridw,cdividyw,cdividxw,cdividw)

	endif
c
c __________________________________________________________
c               Step 14; Reservoir Target
	if(numres.gt.0) then
          call daydata(ichk6,85,5,maxres,numres,nlog,
     1      iyrmo(mon),imomo(mon), imd, targex2, cresidx)

	  if(isim.eq.1) then    
	    call daydist(ichk6,itypr,iyrmo(mon),imomo(mon),mon,imd,
     1        mthday,maxres, numres,idayr,
     1        targetx,targex2,targetd,cresidy,cresidx,cresid)
	  else
	    call daydist(ichk6,itypr,iyrmo(mon),imomo(mon),mon,imd,
     1        mthday,maxres, numres,idayr,
     1        resvol,targex2,targetd,cresidy,cresidx,cresid)
	  endif

c
c __________________________________________________________
c               Step 15; Daily Evaporation; not used / 31 in evasec.for
c         do i=1,numeva
c           idaye(i)=-1
c         end do
c         call daydist(ichk6,4,iyrmo(mon),imomo(mon),mon,imd,
c     1     mthday, maxeva, numeva, idaye,
c     1     evaprt,evapdx,evapd,cevaidy,cevaidx,cevaid)
	endif
c
c __________________________________________________________
c               Step 16;  Consumptive (IWR) demand 
c                         Note includes both diversion and well data
c                         Note storing in diversion array diverdx, 
c                         this may be a problem
c rrb 02/03/04; Revise to allow ieffmax=2
c       if(ieffmax.eq.1) then
        if(ieffmax.ge.1) then
	  nx=numdiv+numdivw
	  if(nx.gt.maxdiv) then
            write(nlog,770) numdiv, numdivw, nx, maxdiv
	    goto 9999
	  endif
c
c rrb 01/12/17; Correction
c         call daydata(ichk6,88,11,maxdiv,numdiv,nlog,
c    1      iyrmo(mon),imomo(mon), imd, diverdx, cdividx)

          call daydata(ichk6,88,11,maxdiv,nx,nlog,
     1      iyrmo(mon),imomo(mon), imd, diverdx, cdividx)
c
c ----------------------------------------------------------
c               Step 16a; Distribute daily Diversion (IWR) demand

	  call daydist(ichk6,9,iyrmo(mon),imomo(mon),mon,imd,
     1      mthday, maxdiv, numdiv, idayd,
     1      diwr,diverdx,diwrd,cdividy,cdividx,cdivid)
c
c ----------------------------------------------------------
c               Step 16a; Distribute daily Well (IWR) Demand 

	  call daydist(ichk6,10,iyrmo(mon),imomo(mon),mon,imd,
     1      mthday, maxdivw, numdivw, idaydw,
     1      diwrw,diverdx,diwrdw,cdividyw,cdividxw,cdividw)

	endif
c
c ________________________________________________________
c               Step 18; Calculate total demand for output
c     
c --------------------------------------------------------
c               Step 18a; Diversions 
	do nd=1,numdiv
	  do i=1,32  
c           write(nlog,*) '  Dayest;', nd, i, diverd(i,nd)
	    divertd(i,nd) = diverd(i,nd)
c
c rrb 01/04/01; Correction for -1 data code
c           if(ieffmax.eq.0) then
	    if(ieffmax.le.0) then
	      diveriTd(i,nd) = diverd(i,nd) * diveff(mon,nd)/100. 
	    else
	      diveriTd(i,nd) = diwrd(i,nd) 
	    endif
	  end do
	end do
c     
c --------------------------------------------------------
c               Step 18b; Wells
c
	if(iwell.ge. 1) then
	  do nw=1,numdivw        
	    nd=idivcow2(nw)
	    if(nd.gt.0) then

	      do i=1,32
c
c rrb 00/07/11; Adjust well demand so that total refelct SW efficiency
c               if in addition mode only (idemtyp=2)
c rrb 01/02/04; Adjust only if in addition mode (idemtyp=2)
		if(diveff(mon,nd).gt.small .and. idemtyp.eq.2) then
		  ceff = diveffw(mon,nw)/diveff(mon,nd)
		else
		  ceff = 1.0
c                 write(nlog,757)
		endif

		divertd(i,nd) = divertd(i,nd) + diverdw(i,nw)*ceff
c
c rrb 01/04/01; Correction for -1 data code
c               if(ieffmax.eq.0) then
		if(ieffmax.le.0) then
		  diveriTd(i,nd) = diverd(i,nd)*diveff(mon,nd)/100. 
     1                             + diverdw(i,nw)*diveffw(mon,nw)/100.
		else
		  diveriTd(i,nd) = diwrd(i,nd) + diwrdw(i,nw)
		endif
	      end do
c             write(nlog,758) nw, nd,
c    1          (divert(mon,nd)*dfacto*mthday(mon),mon=1,12)
c             write(nlog,758) nw, nd,
c    1          (diverirT(mon,nd)*dfacto*mthday(mon), mon=1,12)
c             write(nlog,*) ' '

	    endif
	  end do
	endif
c
c rrb 00/07/11; Test
c       do nd=1,numdiv
c         write(nlog,759)nd,(divert(mon,nd)*dfacto*mthday(mon),mon=1,12)
c         write(nlog,759)nd,(diverirT(mon,nd)*dfacto*mthday(mon),mon=1,12)
c         write(nlog,*) ' '
c       end do
c
c _________________________________________________________
c               Step 19; Set Monthly diversion demands to daily totals
c
	do nd=1,numdiv
c         write(nlog,*) '  Dayest;', nd, diver(mon,nd), diverd(32,nd)
	  diver(mon,nd)=diverd(32,nd)
	  diverir(mon,nd)=diverird(32,nd)
	  diwr(mon,nd)=diwrd(32,nd)
c
	  divert(mon,nd)=divertd(32,nd)
	  diverirt(mon,nd)=diveritd(32,nd)
	end do
c
c _________________________________________________________
c               Step 20; Set Monthly well demands to daily totals
c
	do nw=1,numdivw
	  diverw(mon,nw)=diverdw(32,nw)
c
	  if(ieffmax.le.0) then
	    diverirw(mon,nw)=diveridw(32,nw)
	  else
	    diverirw(mon,nw)=diwrdw(32,nw)
	  endif
	end do
c
c _________________________________________________________
c               Step 21; Read Daily Call data for 1 month
c		Note idcall =0 unless a type 23 downstream call 
c                 operational right is on
c     write(nlog,*) '  Dayest; idcall = ', idcall
      if(idcall.ne.0) then
	iin2=90
	filena = '*.cal'
        do id=1,31
c
c rrb 05/06/21; Revise to free format and 4 diget year        
c933      read (90,932,end=520,err=520) icd1, rec3, icy1, rec10, dcall1
          read (90,*,end=520,err=520) icy1, icm1, icd1, dcall1
 
          if(id.ne.icd1) then
            backspace(90)
            goto 22
          endif  
            
cr        call findmo(nlog, cyr1, rec3, icm1,imnum)
          dcalld(id)=dcall1
          
c         write(nlog,934)  icy1, icm1, icd1, dcall1
        end do  
c        
        if(iout.eq.1 .or. ichk6.eq.1) then
          write(nlog,932) ' Daily Call File (*.cal)             file 90'
          write(nlog,933) (i, i=1,31)
          write(nlog,934) 1, 3, icy1, icm1, (dcalld(id), id=1,31)
 932      format(/,60('_'),/'  Dayest; for ',a45)
 933      format(/,
     1    '  Dayest;    ID Type   YR  MON', 31i10)
 934      format(
     1    10x, 4i5, 31f10.2, f10.2, f10.0)
        endif
      endif              


c
c _________________________________________________________
c               Step 22; End of Daily Initilization

 22	goto 500
      endif
c
c __________________________________________________________
c               Step 21; Return
 500  return
c
c __________________________________________________________
c               Step 22; Error Processing
  510 write(nlog,150) filena
      goto 9999
  520 write(nlog,929) iin2, filena
      goto 9999
 
c
c __________________________________________________________
c               Formats
  110 format(
     1 '  Dayest;'
     1 ' Too many Daily delay tables in (*.dld),    Maximum = ',I5)
  120 format(
     1 '  Dayest;'
     1 ' The variable interv from the control file (*.ctl)',
     1 ' must be a negative value and data in both the monthly',/,10x,
     1 ' and daily delay tables must be formatted accordingly')
  130 format(/,
     1 '  Dayest;',
     1 ' Warning in the Daily delay table file (*.dld), the',
     1 ' total return for table ', i5,' = ',f10.2,' not 100.00')

  150 format(
     1 '  Dayest;'
     1 ' Problem opening or reading the Daily delay file ',/,
     1 10x, a72,/,10x,
     1 ' read from the response (*.rsp) file')
  160 format(
     1 '  Dayest; FYI return tables ' 
     1 ' reset to 0.0 becuse their sum is < 1%',/
     1 '    #      ID   Sum %    CumSum %',/
     1 ' ____ _______ _______ ___________')
  161 format(i5, i8, f8.2, f12.2)
  757 format(
     1 '  Dayest; Problem division by zero',/
     1 '          Demand data provided as IWR is > 0 and ', 
     1 'efficiency is < 0 for: ', /,
     1 '          ID: ' ,a12, ' Name: ', a24, ' Date: ',2i5,  
     1 ' Dayest: ', f8.0, ' Efficiency: ' f8.0)
  758 format('  Dayest;', 2(i5), 12f8.0) 
  759 format('  Dayest;', i5, 12f8.0)
  770 format(
     1  '  Dayest; Problem diversion array is too small for',
     1  ' IWR data.  To do make new variable'/
     1  '          numdiv = ', i5, ' numdivw = ', i5,
     1           ' total = ', i5, ' Dimension = ', i5)
     
  929 format(/
     1 '  DayEst; Problem reading file # ', i4,/,
     1 '          File name: ', a256,/
     1 '          Problem record (next line):')


 9999 write(6,1440)
      write(nlog,1450) 
      
 1440 format('  Stopped in Dayest, see the log file (*.log)')
 1450 format('  Stopped in Dayest')
      write(6,*) ' Stop 1'
      call flush(6)
      call exit(1)

      stop 
      end


