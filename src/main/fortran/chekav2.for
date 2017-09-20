C
      SUBROUTINE chekav2(
     1 icall, maxsta, numsta, istop, fac, AVAIL, IMCD, AvMin)
c
c
c _________________________________________________________
c	Program Description
c
c       Chekav2; Similar to Chekava.
c                It checks teh entire Avail array by finding the
c                mininum value and warning if < 0.
c		 But instead of stopping, it returns the most 
c                negative value. 
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
c
c	       istop   0 DO NOT STOP if a negative is found
c		        1 DO STOP if a negative is found
c             numsta  number of downstream stations
c		avail   array to check for min value
c		imcd    pointer to min value
c             Avmin   minimum value in array
c
c _________________________________________________________
c
c		Dimensions

      DIMENSION AVAIL(maxsta)
      dimension subtyp2(30)
      character subtyp2*8
      data subtyp2/
     1 'Powres',    'Divres',   'Divres',  'Divrpl', 'Resrpl',
     1 'Rsrspu',    'Carrpl',   'Resoop',  'Powsea', 'Replace',
     1 'Divcar',    'Reoper',   'Ifrrigx', 'Divcar1','Sjrip',     
     1  'Evasec',   'DivResP2', 'DivRplP', 'Welrig', 'DirectEx',
     1  'DirectBy', 'PowResP',  'OopDiv',  'Divrig', 'DivrplP2',
     1  'DivCarL',  'RivRtn',   'DivAlt',  ' ',      ' '/     
c
c _________________________________________________________
c
c               Step 1; Check entire array avail
c
      nlog=99
      smalln = -0.001     
      AvMin = 99999.
      IMCD=0
      
      do ix=1,numsta
        if (avail(ix).lt.AvMin) then
          AvMin=avail(ix)
          imcd=ix
        endif  
      end do    
c
c	
      if(istop.eq.1 .and. AvMin.lt.smalln) goto 9999
c
c _________________________________________________________
c               Return
c
      return
c
c _________________________________________________________
c               Error Processing
c
 9999 write(6,1050) 
      write(nlog,1052) subtyp2(icall), ix, AvMin, AvMin*fac
      write(nlog,1051)

      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)
c _________________________________________________________
c               Stop
      stop
c
c _________________________________________________________
c               Formats
c
      
 1050 format('    Stopped in ChekAv2',/,
     1       '    See the *.log file')
 1051 format('    Stopped in ChekAv2')
 1052 format(/,'    ChekAva2; ', 
     1    'Checking Avail going into or out of routine ', a8,/
     1 13x, 'Negative avail at node ', i5, '. Avail = ', f10.3,' cfs',
     1 f10.3, ' af')
c _________________________________________________________
c               End
c
      end

