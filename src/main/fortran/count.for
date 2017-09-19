c
c *********************************************************      
      subroutine count(nlog,nf, inX, iystr, ctype, rec256)
c
c _________________________________________________________
c	Program Description
c
c
c		It counts the number of entries in a time series
c		then rewinds the file and skips POR record      
c      
c _________________________________________________________
c	Update History
c		NA
c
c _________________________________________________________
c	Documentation
c		NA
c _________________________________________________________
c	Dimensions
c
      character recA*12, recB*12, ctype*16, rec256*256
      
      iout=0
      inX=0
      if(iout.eq.1) then
        write(nlog,*) '  Count;'
        read(nf,'(a256)',end=100,err=100) rec256
        write(nlog,*) rec256
        backspace(nf)
      endif 
       
      read(nf,'(5x,a12)',end=100,err=100) recA    
      inX=1  
      do i=1,1000
        read(nf,'(5x,a12)',end=100,err=100) recB
        if(recA.eq.recB) goto 100
        inX=inX+1
      end do
      
 100  if(iout.eq.1) write(nlog,200) inX, ctype, rec256 
      rewind(nf)
c
c		Skip header      
      call skipn(nf)
c
c		Skip year type control      
      read(nf,'(5x,a12)',end=500,err=500) recA    
c _________________________________________________________
c
c               Parse thru data to get time of interest
c rrb 2006/12/12; Read fixed format to allow an annual file with ityr=0
c90   read (nf,*,end=500,err=500) ityr
 90   read (nf,'(i5)',end=500,err=500) ityr
      if(iout.eq.1) write(nlog,*) ' Count, ityr', ityr
c
c
c rrb 2006/12/12; Allow an annual file with ityr=0
      if(ityr.eq.0) goto 110
      if((ityr-iystr).lt.0) goto 90
      if((ityr-iystr).gt.0) goto 120

 110  backspace(nf)
      goto 500
c
c		Return     
 500  return

 112  format(
     1  '  Count; FYI, A dummy (blank) file determined for ',/
     1  '         file: ', a256)
     
 200  format(
     1 '  Count; Number of stations = ', i5,/     
     1 '         File type          = ', a16,/
     1 '         File name          = ', a256)
     
  120 write(nlog,130) rec256, iystr, ityr
  130 format(
     1  '  Count; Problem with standard monthly file:',/,
     1  '         ', a256,/
     1  '           The simulation start year ', i5,/
     1  '           is outside the period of record provided  ', i5)
     
      goto 9999
c
c _________________________________________________________
c		Error Processing      
 9999 write(6,*)  '  Stopped in Count, see log file (*.log)'
      write(99,*) '  Stopped in Count'
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop 
      end
