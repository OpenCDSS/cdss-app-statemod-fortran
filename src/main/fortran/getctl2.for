c
c *********************************************************
c
      subroutine getctl2(nf, iok, ityp, i, r, c)
c
c       Getctl; A generic control data reader
c               nf=file number
c               iok     0 not OK if not found
c                       1 OK if not found
c               itype   0 integer
c                       1 = real
c                       2=character
c               i = integer value
c               r = real value
c               c = character value
c

      character cx*12, c*12

      iout=0
      i=0
      r=0.0
      c=' '
c
c _________________________________________________________
c               Read data based on type

c100  read(nf,'(a12)',end=110,err=110) cx
 100  read(nf,*,end=110,err=110) cx
c     write(99,*) '  Getctl; iok, cx = ', iok, cx
      if(cx(1:1).eq.'#') goto 100

      backspace(nf)

      if(ityp.eq.0) then
        read(nf,*,end=110,err=110) i
        if(iout.eq.1) write(99,*) '  GetCtl; Integer i = ', i
        goto 120
      endif

      if(ityp.eq.1) then
        read(nf,*,end=110,err=110) r
        if(iout.eq.1) write(99,*) '  GetCtl; Real r = ',r
        goto 120
      endif

      if(ityp.eq.2) then
        read(nf,'(a12)',end=110,err=110) c
        if(iout.eq.1) write(99,*) '  GetCtl; Character c = ',c
        goto 120
      endif
c
c _________________________________________________________
c               Check if eof is OK
 110  if(iout.eq.1) write(99,*) ' GetCtl; iok = ',iok
      if(iok.eq.0) goto 200
c
c _________________________________________________________
c               Return
 120  return

c
c _________________________________________________________
c               Error Processing

 200  write(6,210) 
      write(99,220) 
      call flush(6)
 210  format('  Stopped in Getctl')
 220  format(72('_'),/
     1 '  Getcl; Stopped in Getctl, see the log file (*.log)')
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop 
      END


