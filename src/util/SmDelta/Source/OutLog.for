c                            
c ************************************************************
c
        subroutine outlog

         include 'SmDelta.inc'
c
c               Print input to log file
c       write(99,*) ' '
        if(ibin.eq.0) write(99,*) '  Binary file determined'
        if(ibin.eq.1) write(99,*) '  ASCII file determined'
        write(99,100) filen(ifx), iwelld, iwellr, iwellw,
     1                ftype(ifx), ptype(ifx), ip, itime(ifx)
  100   format(
     1      ' Outlog; ',/
     1      '   File:                        ', a72,/
     1      '   Diversion file version (1)   ', i2,/
     1      '   Reservoir file version (1)   ',  i2, /
     1      '   Well file version (1)        ', i2,/
     1      '   File Type:                   ', a24,/
     1      '   Parameter:                   ', a24,/
     1      '   Parameter ID:                ', i2,/,
     1      '   Time requested:              ', a12,/
     1      ' ',/
     1      '   (1) where:',/
     1      '       Version 1 = Original ',/
     1      '               2 = With wells (10x)',/
     1      '               3 = With Loss (11x)')

        do 120 i=1,iid
          write(99,110) idreq(i)
  110     format(
     1      '   Id requested                 ', /
     1      '     where 0=all:               ',a12)
  120   continue
        return
        end
