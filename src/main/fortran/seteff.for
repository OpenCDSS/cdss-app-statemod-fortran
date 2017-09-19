c
c *********************************************************
c
      subroutine seteff
c
c
c _________________________________________________________
c	Program Description
c
c       Seteff; It sets efficiency data
c               Called from bomsec and virset
c               Removed from Bomsec so that both the
c               baseflow and simulate modules can use the
c               same code.
c
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
      character rec32*32
c
c _________________________________________________________
c
c               Step 1; Initilize
c
      istart=0
      iout=0
      
      if(iout.gt.0 .or. ichk.eq.94) then
        write(nlog,*) ' SetEff Detailed Output'
        write(6,*) ' SetEff'
      endif      
c
c rrb 2006/10/20; Daily output control
cr    if(iyr.eq.iystr .and. mon.eq.1) istart = 1
      if(iyr.eq.iystr .and. mon.eq.1 .and. idy.eq.1) istart = 1
c     WRITE(nlog,*) '  Seteff; istart = ', istart

      if(istart.eq.1) iprintd=0
c
c		Set efficiency warning output to 1%      
      warneff=1.0
c _________________________________________________________
c
c               Step 2; Check and set default efficiency data
c


      if(itsfile.eq.1 .or. itsfile.eq.10) then
c
c               Note if data is provided in *.tsp it is set in mdainp
      else
	do nd=1,numdiv
	  effmax(nd) = diveff(mon,nd)
	  effmaxd(nd)= diveff(mon,nd)
	  effmaxt(nd)= 100.0
	end do

	do nw=1,numdivw
	  effmaxw(nw) = diveffw(mon,nw)
	  effmaxs(nw) = diveffw(mon,nw)
	end do
      endif
c
c
c _________________________________________________________
c
c               Step 3; Set default diversion data if none
c                       provided in *.tsp
      do nd=1,numdiv
c
c               itspd = 0 no *.tsp data
c                     = 1 yes *.tsp data
	if(itspd(nd).eq.0) then
	  effmax(nd) = diveff(mon,nd)  
	  effmaxd(nd)= diveff(mon,nd)
	  effmaxt(nd)= 100.0
c          
c               Print 1x per year
         if(istart.eq.1 .and. ieffmax.eq.1) then

c         if(ieffmax.eq.1) then
            if(iprintd.eq.0) then
              rec32='Max Diversion Efficiency'
              write(nlog,1281) iyr, rec32
              write(nchk,760) (xmonam(n), n=1,12)
c             write(nlog,*) '  Bomsec; istart, ieffmax', istart, ieffmax
            endif
            iprintd=iprintd+1
	    write(nchk,762) iprintd,cdivid(nd),divnam1(nd),
     1                      (diveff(n,nd), n=1,12)
	  endif
        endif
      end do 
c
c _________________________________________________________
c
c               Step 4; Set default well data if none provided
c                       in *.tsp
      do nw=1,numdivw
c
c rrb 01/09/25; Correction (worked on 1st time step but then 
c               effmaxw(nw) is set so it sets all data to month 1 data
	if(itspw(nw).eq.0) then
	  effmaxw(nw) = diveffw(mon,nw)
	  effmaxs(nw) = diveffw(mon,nw)
	  areawa(nw)  = 0.0
	  areasp(nw)  = 0.0
	  divcapwa(nw)=divcapw(nw)
	  igwmode(nw) = 0
c
c
c               Test print 1x/ simulation
	  if(istart.eq.1 .and. ieffmax.eq.1) then 
c         if(ieffmax.eq.1) then
	    if(iprintd.eq.0) then
	      rec32='Max Well Efficiency'
	      write(nlog,1281) iyr, rec32
	      write(nchk,760) (xmonam(n),n=1,12)
	    endif  
	    iprintd=iprintd+1
	    write(nchk,762) iprintd,cdividw(nw),divnamw1(nw),
     1                      (diveffw(n,nw), n=1,12)
	  endif
	endif
      end do 
c
c rrb 01/04/25; Check for ave efficiency < max efficiency in year 1
c
c rrb 2006/06/07; Reduce warnings
cr    if(iyr.eq.iystr) then
      if(istart.eq.1 .and. ieffmax.eq.1) then 
cr	if(mon.eq.1) iprintd=0
	do nd=1,numdiv
	  c=diveff(mon,nd) - effmax(nd)
c
c rrb 01/08/25; Limit output to 1 %
	  if(c .gt. warneff) then
	    if(iprintd.eq.0) then
	      rec32='Max Vs Ave Div Efficiency'	    
              write(nlog,1281) iyr, rec32
	      write(nchk,764)
	    endif  
	    iprintd=iprintd+1
	    write(nchk,766) iprintd,cdivid(nd), '  Div', mon,
     1        diveff(mon,nd), effmax(nd),c, divnam1(nd)
	  endif
	end do

	do nw=1,numdivw
	  c=diveffw(mon,nw) - effmaxs(nw)
c
c rrb 01/08/25; Limit output to 1 %
	  if(c .gt. warneff) then
	    if(iprintd.eq.0) then
	      rec32='Max Vs Ave Well Efficiency'	    	    
              write(nlog,1281) iyr, rec32
	      write(nchk,764)
	    endif  
	    iprintd=iprintd+1
	    write(nchk,766) iprintd,cdividw(nw), ' Well', mon,
     1        diveffw(mon,nw), effmaxs(nw),c, divnamw1(nw)
	  endif
	end do
      endif
c
c _________________________________________________________
c
c               Formats
  760  format(/
     1  72('_'),//   
     1 '  SetEff; FYI the following structures have no data provided',/
     1 10x,'in the annual time series file (*.tsp)',/ 
     1 10x, 'Setting max efficiency, sprinker area, etc. to default',/,
     1 10x, 'Note OK if the structure is non agricultural',/
     1 10x, 'Note this warning is printed only 1x per simulation', //,
     1 '                                            ',12('  EffMax'),/
     1 '     # Structure ID Name                    ',12(4x, a4),/  
     1 ' _____ ____________ ________________________',12(' _______'))
  762  format(1x, i5, 1x, a12, 1x, a24, 12f8.2)

  764  format(/
     1  72('_'),//     
     1 '  SetEff; FYI the following structures have ',/
     1 10x,'(ave efficiency in *.dds) < (max efficiency ',
     1 'in *.tsp)',/
     1 10x,'where max efficiency = flood for diversions',/
     1 10x,'and max efficiency = sprinkler for wells',/ 
     1 10x, 'This can result in a total demand being satisfied ',/ 
     1 10x,'while the CU demand is not',/
     1 10x, 'Note this warning is printed for each month in ', 
     1 'year 1 only', //,
     1 '     # Structure ID Type  Mon  AveEff  MaxEff   Delta Name',/
     1 ' _____ ____________ ____ ____ _______ _______ _______ ', 
     1 '________________________')
  766  format(1x, i5, 1x, a12, a5, i5, 3f8.2, 1x, a24)
     
 1281  FORMAT(/,72('_'),/
     1  '  SetEff;  Warning See *.chk for details in year', i5,
     1  ' regarding: ',a32,/
     1  '           Note only first occurance (year) is printed')

c
c _________________________________________________________
c
c               Return
      return
      end

