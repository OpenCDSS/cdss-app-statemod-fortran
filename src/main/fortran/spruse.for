c
C
      subroutine spruse(iw,l2,divact,ncallX)
c
c
c _________________________________________________________
c	Program Description
c
c       Spruse; It simulates a Type 21 Operating Rule for Sprinkler Use
c
c	 CAlled by Execut
c               
c       Note datip checks if isprink.ne.1 or itsfile.ne.1 or
c               ieffmax.ne.1 iwell.ne.1
c       Also oprinp checks if isprink=1 but no operating rule provided 
c
c _________________________________________________________
c       Documentation
c
c               iw= global water right counter
c               l2 = operation right counter
c               divact = amount diverted under this operation right
c
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
      character cwhy*48
c
c _________________________________________________________
c
c               Step 1; Initilize
      iout=0
      if(ichk.eq.121) iout=1

      iprint=0
      small = 0.001
c
c rrb 98/03/03; Daily capability
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif
c
c _________________________________________________________
c
c               Step X; Branch if not on this month
c
c rrb 04/22/96; Allow month switch
      if(imonsw(l2,mon).eq.0) then
        iwhy=1
        cwhy='Monthly switch is off'
        goto 500
      endif
c
c ---------------------------------------------------------
c		For a daily model set demand for beginning of season
      if(iday.eq.1 .and. imonsw(l2,mon).gt.0) then
        if (idy.lt.imonsw(l2,mon)) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 500
        endif  
      endif  
c
c ---------------------------------------------------------
c		For a daily model set demand for end of season
      if(iday.eq.1 .and. imonsw(l2,mon).lt.0) then
        if (idy.gt.iabs(imonsw(l2,mon))) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 500
        endif  
      endif  
      
c
c _________________________________________________________
c
c               Step 2; Loop for all well rights
      do 200 k=1, numdvrw
        divact=0.0
c
c               Get well ID
        nw = idivcow(1,k)
c
c               Skip if this well or well right  is off
        if(idivsww(nw).eq.0 .or. idvrsww(k).eq.0) goto 200
c
c               Skip if sprinkler acreage is small
        if(areasp(nw).lt.small) goto 200
c
c               Insure we have sprinker efficiency data
        if(effmaxs(nw).lt.small) then
          write(io99,120) cdividw(nw)
          goto 9999
        endif           
c
c               Operate if Maximum supply is on for this Well
        if(igwmode(nw).eq.1) then
c
c _________________________________________________________
c
c               Step 3; Call welrig like any other well right
c                       Note welrig uses ispruse to know
c                       demand is limited to area with sprinklers
c
          ispruse=1
c
c rrb 2006/04/12; Add ability to simulate T&C for an aug Plans
cr         call welrig(iw,k,ispruse,retx,divact)
            if(iplanwOn.eq.0) then
	      call welrig3(iw,k,ispruse,retx,divact,ncallX)
	    else
	      call Welrig3P(iw,k,ispruse,retx,divact,ncallX)
	    endif  
          
c
c _________________________________________________________
c
c               Step 4; Store water controlled by this opr right
c
          divo(l2)=divo(l2)+divact
c
c               Endif for igwmode = 1
        endif

c
c _________________________________________________________
c
c               Step 5; Print detailed results
        if(iout.eq.1) then
          if(iprint.eq.0) write(io99,100) 
          write(io99,110) nw, k, igwmode(nw), divreqw(nw)*fac, 
     1      divact*fac, divo(l2)*fac
c         iprint=1
        endif

 200  continue
c
c _________________________________________________________
c
c               Step 6; Return
c
 500  RETURN
c
c _________________________________________________________
c
c               Formats
c
 100  format(/,'  SprUse;',/,
     1 '      nw       k igwmode divreqw  divact    divo',/
     1 ' _______ _______ _______ _______ _______ _______')
 110  format(3i8, 20f8.0)
 120  format('  SprUse; Problem simulating baseflow with sprinkler', 
     1  ' use first (isprink=1)',/ 
     1 9x,'but no sprinkler eff data for id ', a12)
c
c _________________________________________________________
c
c               Print warning
c 
 9999 write(6,*) '  Stopped in SprUse, see the log file (*.log)'
      write(99,*) '  Stopped in SprUse'
      write(6,*) 'Stop 1' 
      call flush(6)
      call exit(1)

      stop 
      end
     
