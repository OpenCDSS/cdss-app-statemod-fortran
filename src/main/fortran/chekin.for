c *****************************************************
c
      subroutine chekin(iin, nf, itype, iystr, cyr1)
c
c
c _________________________________________________________
c	Program Description
c
c       Chekin; it checks that a time series file has
c       data appropriate for the study period.
c

c
c _________________________________________________________
c	Update History
c		NA
c
c
c _________________________________________________________
c	Documentation
c
c
c _________________________________________________________
c	Dimensions
c
      character dtype*45, filena*256, cyr1*5
      dimension dtype(10)
      data dtype/
     1     'Evaporation File (*.eva)                     ',
     2     'Precipitation File (*.pre)                   ',
     3     'Base Streamflow File (*.rib)                 ',
     4     'Direct Flow Demand File - Monthly (*.ddm)    ',
     5     'Direct Flow Demand File - Annual (*.dda)     ',
     6     'Instream Flow Demand File (*.ifa)            ',
     7     'Reservoir Target Content File (*.tar)        ',
     8     'Historic Streamflow File (*.rih)             ',
     9     'Historic Diversion File (*.ddh)              ',
     1     '                                             '/

c
      iout=0
      
      if(iout.eq.1) write(99,*) ' Chekin; nf, itype ', nf, itype
      Write(99,'(a45)') dtype(itype)
 100  read (iin,'(a256)') filena
      if(filena.eq.'                    ') go to 100
      open(nf, file=filena,status='old',err=9997)
c
      write(99,'(5x, a256)') filena
      call skipn(nf)
c
c              Read year type control data          
      call chekts(nf,itype, c, idummy, cyr1)
      call skipn(nf)
c
 150  READ (nf,*,END=170) IRYR
C
      if(iryr-iystr) 150,210,190
c
 170  write(99,180) dtype(itype)
      WRITE(6,180) dtype(itype)
 180  format(' Chekin; Problem the ', a45, 1x,
     1 ' data is not within the simulation period')
      goto 9999
 190  write(99,200) dtype(itype)
      WRITE(6,200) dtype(itype)
 200  format(' Chekin; Problem the requested starting year of',
     1 ' simulation is not within the ', a45, ' data file')
      goto 9999
c
c              First year found
 210  continue
      backspace(nf)
      return

 9997 write(99,9998) filena
 9998 format('  Chekin; Problem opening file: ', a256)
 9999 write(6,*) '  Stopped in Chekin; see log file (*.log)'
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop 
      end


