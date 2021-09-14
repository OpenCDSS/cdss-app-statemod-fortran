C     Last change:  RRB  28 Aug 2002    4:00 pm
c                            
c ************************************************************
c
c       skipn - skips any number of comment cards identified
c               as a '#' in column 1 from a data file (nf)
c
        subroutine skipn(nf)
        character rec1*1, recx*1, recin*256, filena*256
c                 
        recx = ' '
        iin2=nf
        filena='N/A, in skipn.for'
        io99=99
        iout=0
c
c       Check first record and store for use on this file.
 100    read(nf,500,end=926,err=928) rec1
        if(iout.eq.1) write(io99,500) rec1
c
        if(rec1.eq.'#') then
          recx = rec1
        else
          backspace(nf)
          goto 999
        endif        
c                                                                
c       Check subsequent records.
 110    read(nf,500,end=926,err=928) rec1
        if(iout.eq.1) write(io99,500) rec1
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
  926 write(io99,927) iin2, filena
  927 format('  Skipn.f; End of file # ', i4, ' encountered',/,
     1       '    File name: ', a256,/
     1       '    May be OK if a dummy file read')
      goto 999
c
  928 write(io99,929) iin2, filena
  929 format(' Skipn.f; Problem reading file # ', i4,/,
     1       '   File name: ', a256,/
     1       '   Problem record (next line):')

      backspace(iin2)
      read(iin2, '(a256)',end=926,err=926) recin
      write(99,'(a256)') recin
      goto 9999


 9999 write(6,9997)
      write(99,9998)
 9997 format('  Stopped in Skipn, see the log file (*.log)')
 9998 format('  Stopped in Skipn')
      write(6,*) 'Stop 1' 
      call flush(6)
      stop 
      end
