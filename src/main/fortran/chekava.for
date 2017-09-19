C
C-------------------------------------------------------------------
C
      SUBROUTINE chekava(icall, maxsta, numsta, AVAIL)
c
c
c _________________________________________________________
c	Program Description
c
c       Chekava; it checks array Avail by finding the
c                mininum value and warning if < 0.
c
c
c _________________________________________________________
c	Update History
c		NA
c
c _________________________________________________________
c
c	Documentaion
c		icall	calling routine
c		maxsta	dimension for maximum number of stations
c               numsta  number of downstream stations
c		avail   array to check for min value
c               Avmin   minimum value in array
c
c
c _________________________________________________________
c	Dimensions
c

      DIMENSION AVAIL(maxsta)
      dimension subtyp2(30)
      character subtyp2*8
      data subtyp2/
     1  'Powres',   'Divres',   'Divres2', 'Divrpl',  'Resrpl',
     1  'Rsrspu',   'Carrpl',   'Resoop',  'Powsea',  'Replace',
     1  'Divcar',   'Reoper',   'Ifrrigx', 'Divcar1', 'Sjrip',
     1  'Evasec',   'DivResP2', 'DivRplP', 'Welrig',  'DirectEx',
     1  'DirectBy', 'PowResP',  'OopDiv',  'Divrig',  'DivrplP2',
     1  'DivCarL',  'RivRtn',   'DivRplR', 'DivResR ',       ' '/
c
c _________________________________________________________
c
c               Step 1; Check array avail
c
      iout=0

      nlog=99
      smalln = -0.001     
      ax = 99999.9
      fx=1.9835*31.
      
      do ix=1,numsta
        ax1=ax
        ix1=ix
        ax=amin1(ax, avail(ix))
        if(ax.lt.ax1) ix1=ix
        if(ax.lt.smalln) goto 9999
      end do
      
      if(iout.eq.1) then
        write(nlog,1054) subtyp2(icall), ix1, ax, ax*fx
      endif
c
c _________________________________________________________
c               Return
c
      return
c
c _________________________________________________________
c               Formats
c
      
 1050 format('    Stopped in ChekAva',/,
     1       '    See the *.log file')
 1051 format('    Stopped in ChekAva')
 1052 format(/,'    ChekAva; ', 
     1    'Checking Avail going into or out of routine ', a8,/
     1 13x, 'Negative avail at node ', i5, '. Avail = ', f10.3,' cfs',
     1 f10.3, ' af')
     
 1054 format(/,'    ChekAva; ', 
     1    'Checking Avail going into or out of routine ', a8,/
     1 13x, 'No negative avail. Minimum at node ', i5, 
     1 '. Avail = ', f10.3,' cfs', f10.3, ' af')
c
c _________________________________________________________
c               Error Processing
c
 9999 write(6,1050) 
      write(nlog,1052) subtyp2(icall), ix, ax, ax*fx
      write(nlog,1051)

      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)
      stop


      end

