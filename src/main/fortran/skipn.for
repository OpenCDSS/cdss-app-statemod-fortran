c                            
c ************************************************************
c
c
        subroutine skipn(nf)
c
c
c _________________________________________________________
c	Program Description
c
c       Skipn; It skips any number of comment cards identified
c               as a '# '  from a data file
c _________________________________________________________
c	Dimensions
c
         character rec1*1, recx*1, recin*256, filena*256, rec132*132
c
c _________________________________________________________
c
c                 
c		iout=0 no details
c			1 details
c			2 summary
        iout=0
        if(iout.eq.1) write(nlog,*) '  Skipn, nf', nf
        recx = ' '
        iin2=nf
        filena='N/A, in skipn.for'
        nlog=99
c
c _________________________________________________________
c
c              Check first record and store for use on this file
c
c100    read(nf,500,end=926,err=928) rec1
 100    read(nf,'(a132)',end=926,err=928) rec132
        rec1=rec132(1:1)
        if(iout.eq.1) write(nlog,*) ' Skipn; rec132 = ', rec132
        if(iout.eq.1) write(nlog,*) ' Skipn; rec1 = ', rec1
c       write(io99,500) rec1
c
        if(rec1.eq.'#') then
          recx = rec1
        else
          backspace(nf)
          goto 999
        endif        
c
c _________________________________________________________
c
c              Check subsequent records
c
c110    read(nf,500,end=926,err=928) rec1
 110    read(nf,'(a132)',end=926,err=928) rec132
        rec1=rec132(1:1)
        if(iout.eq.1) write(nlog,*) ' Skipn; rec132 = ', rec132
        if(iout.eq.1) write(nlog,*) ' Skipn; rec1 = ', rec1
c       write(io99,500) rec1
c
        if(rec1.eq.recx) then
          goto 110
        else
          backspace(nf)
        endif        

 500    format(a1)
 999    return

c
c rrb 97/11/02; Error Handling
  926 if(iout.ge.1) write(nlog,927) iin2, filena
  927 format('  Skipn.f; End of file # ', i4, ' encountered',/,
     1       '    File name: ', a256,/
     1       '    May be OK if a dummy file read')
      goto 999
c
  928 write(nlog,929) iin2, filena
  929 format(' Skipn.f; Problem reading file # ', i4,/,
     1       '   File name: ', a256,/
     1       '   Problem record (next line):')

      backspace(iin2)
      read(iin2, '(a256)',end=926,err=926) recin
      write(nlog,'(a256)') recin
      goto 9999


 9999 write(6,9997)
      write(nlog,9998)
 9997 format('  Stopped in Skipn, see the log file (*.log)')
 9998 format('  Stopped in Skipn')
      write(6,*) 'Stop 1' 
      call flush(6)
      call exit(1)

      stop 
      end





