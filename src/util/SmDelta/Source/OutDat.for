c                            
c ************************************************************
c
        subroutine outdat

         include 'SmDelta.inc'
c
c rrb 2019/08/25; Documentation
c        ir = 0 Single:     Print results for one file
c        ir = 1 Multiple;   Print multiple results (do not allow 
c                           different locations in each)
c        ir = 2 Difference; Print difference of two files allow 
c                           different locations in each)
c        ir = 3 Merge;      Merge two files
c        ir = 4 DiffX;      Print difference of two files allow 
c                           different locations in each)
c        ir = 5 Diff2;      Print same as 2 but print data plus 
c                           difference
c
c rrb 2019/08/11; Revise dimension from 2018 to 5001

         character idx*12, nameo*144
         dimension ifound(5001), delx(10)
         data ifound/5001*0/
         ichk=0
c
         write(99,102)
  102    format(/, 60('_'),/
     1   'OutDat Results',/)
c
c
c ----------------------------------------
c rrb 02/08/09; Print title
       Write(7,'(a72)') ctitle
c
c _________________________________________________________
c               Print results for one file                           
       if(ir.eq.0) then

         call titleo(itime(1), ptype(1), filen(1), nameo)
         write(7,100) 1, 1, nameo
         if(iout.eq.1) write(99,100) 1, 1, nameo
  100    format(2i4,/,'Single      ', /, a144)
         
c        write(99,*) ' isx(1), ry2(1)', isx(1), ry2(1)

         do 110 i=1,isx(1)         
          if(idz(1,i) .eq. '            ') goto 110
            write(7,260) idz(ifx,i), namex(ifx,i), ctypex(ifx,i),
     1                   dels(ifx,i)/ry2(1)
            if(iout.eq.1) then
              write(99,260) idz(ifx,i), namex(ifx,i),ctypex(ifx,i),
     1                      dels(ifx,i)/ry2(1)
            endif
  110    continue
       endif
c
c _________________________________________________________
c
c               Print multiple results
c               (do not allow different locations in each)

       if(ir.eq.1) then
         if(ifx.lt.2) goto 280

         write(7,120) ifx, ifx
         write(99,120) ifx, ifx
  120   format(2i4,/,'Multiple    ')
         do i=1,ifx
           call titleo(itime(i), ptype(i), filen(i), nameo)
           write(7,'(a144)') nameo
           write(99,'(a144)') nameo
         end do

         do 150 i=1,isx(1)
           if(idz(1,i) .eq. '            ') goto 150
c
c rrb 99/12/28; Insure consistency between data
c               Allow random order
           idx = idz(1,i)
           delx(1) = dels(1,i)
c
c rrb 00/01/14; rrb
           do 148 j=2,ifx 
             do k=1,isx(j)
               if(idx.eq.idz(j,k)) then
                 delx(j) = dels(j,k)
                 goto 148
               endif
             end do
c
c               Problem station not found
           goto 270
  148    continue
         write(7,260) idz(1,i),namex(1,i),ctypex(1,i), 
     1                      (delx(j)/ry2(j), j=1,ifx)
         if(iout.eq.1) then
           write(99,260) idz(1,i),namex(1,i),ctypex(1,i),
     1                      (delx(j)/ry2(j), j=1,ifx)
         endif
  150    continue
       endif
c
c _________________________________________________________
c               Print difference of two files
c               (ir = 2  allow different locations in each)
c               (ir = 4  allow different locations in each)
c               (ir = 5  same as 2 but print data plus difference)
c ----------------------------------------
       if(ir.eq.2 .or. ir.eq.4 .or. ir.eq.5) then
         if(ifx.lt.2) goto 280

         if(ir.eq.2) then
           write(7,160) 1, 2
           write(99,160) 1, 2
  160      format(2i4,/,'Difference  ')
         endif
         if (ir.eq.4) then
           write(7,170) 1, 2
           write(99,170) 1, 2
  170      format(2i4,/,'Diffx       ')
         endif
         if (ir.eq.5) then
           write(7,172) 1, 2
           write(99,172) 1, 2
  172      format(2i4,/,'Diff2       ')
         endif

         do 180 i=1,2
           call titleo(itime(i), ptype(i), filen(i), nameo)
           write(7,'(a144)') nameo
           write(99,'(a144)') nameo
  180    continue
c
c rrb 02/01/03; Debug ID information
c        ichk=1
         if(ichk.eq.1) then
           write(99,*) '  SmDelta; File 1 ID information:'
           do i=1,isx(1)
             write(99,'(i5,1x, a12)') i, idz(1,i)
           end do

           write(99,*) ' '
           write(99,*) '  SmDelta; File 2 ID information:'
           do i=1,isx(2)
             write(99,'(i5,1x, a12)') i, idz(2,i)
           end do
         endif
c        ichk=0

         do 200 i=1,isx(1)                                           
           if(idz(1,i) .eq. '            ')  goto 200

           do 190 j=1,isx(2)
c
c               Print data at same location
             if(idz(1,i).eq.idz(2,j).and.ftype(1).eq.ftype(2)) then
               ifound(j) = 1
                 IF(ir.ne.5) then
                   write(7,260) idz(1,i),namex(1,i),ctypex(1,i),
     1                 dels(1,i)/ry2(1)-dels(2,j)/ry2(2)
                   if(iout.eq.1) then
                     write(99,260) idz(1,i),namex(1,i),ctypex(1,i),
     1                 dels(1,i)/ry2(1)-dels(2,j)/ry2(2)
                  endif
                 else
                   write(7,260) idz(1,i),namex(1,i),ctypex(1,i),
     1               dels(1,i)/ry2(1), dels(2,j)/ry2(2),
     1               dels(1,i)/ry2(1)-dels(2,j)/ry2(2)
                   if(iout.eq.1) then
                     write(99,260) idz(1,i),namex(1,i),ctypex(1,i),
     1                 dels(1,i)/ry2(1), dels(2,j)/ry2(2),
     1                 dels(1,i)/ry2(1)-dels(2,j)/ry2(2)
                   endif
                 endif

                 goto 200
             endif 
  190      continue  
c
c 
c               For the Difference Option (ir=2)
c               Print data in file 1 without a match in file 2
c --------------------------------------------------------------
c rrb 2019/08/25; Print warning for all checks (ir=2,4 & 5)
cx           if(ir.eq.2) then
             write(99,*) ' Data in file 1 without a match in file 2'
             write(7,260) idz(1,i),namex(1,i), ctypex(1,i),
     1                    (dels(1,i)-0.0)/ry2(1)
             if(iout.eq.1) then
               write(99,260) idz(1,i),namex(1,i),ctypex(1,i),
     1                       (dels(1,i)-0.0)/ry2(1)
             endif
cx           endif
  200    continue
c
c               For the Difference Option (ir=2)
c               Print data in file 2 without a match in file 1
c --------------------------------------------------------------
c rrb 2019/08/25; Print warning for all checks (ir=2,4 & 5)
cx         if(ir.eq.2) then
           write(99,*) ' Data in file 2 without a match in file 1'
           do 210 i=1,isx(2)
             if(ifound(i).eq.0 .and. idz(2,i).ne.'            ') then
               write(7,260) idz(2,i),namex(2,i),ctypex(2,i),
     1                     (0.0-dels(2,i))/ry2(2)
               if(iout.eq.1) then
                 write(99,260) idz(2,i),namex(2,i),ctypex(2,i),
     1                        (0.0-dels(2,i))/ry2(2)
               endif
             endif                       
  210      continue
         endif
cx       endif
c
c _________________________________________________________
c               Merge two or more separate files

       if(ir.eq.3) then                                       
         if(ifx.lt.2) goto 280
c
c rrb 99/11/24; Revision
c        write(7,220) ifx, ifx
         write(7,220) 1, ifx
         write(99,220) 1, ifx

  220    format(2i4,/,'Merge  ')
         do 230 i=1,ifx
           call titleo(itime(i), ptype(i), filen(i), nameo)
           write(7,'(a144)') nameo
           write(99,'(a144)') nameo

  230  continue

         do 250 j=1,ifx
           do 240 i=1,isx(j)
c
c rrb 99/12/28
c            if(idz(ifx,i) .ne. '            ') then
             if(idz(j,i)   .ne. '            ') then      
               write(7,260) idz(j,i),namex(j,i),ctypex(j,i),
     1                      dels(j,i)/ry2(j)
               if(iout.eq.1) then
                 write(99,260) idz(j,i),namex(j,i),ctypex(j,i),
     1                       dels(j,i)/ry2(j)
               endif
             endif
  240      continue
  250    continue
       endif
c
c _________________________________________________________
c		Return
       return
c
c rrb 2019/08/11; Add operating rule type
cx  260  format(a12,',', a24, ',',20(f10.0,','))                 
  260  format(a12,',', a24, ',', a2, ',', 20(f10.0,','))                 
  
  270  write(99,272) idx, 1, i, idz(1,i)
       write(99,274) (j, k, idz(j,k), k=1,isx(j))
  272  format(   
     1         '  Outdat; Problem inconsisent files',/
     1         '          indicated by different id information ',/,
     1         '          idx      = ', 11x,     a12,/
     1         '          idz(j,i) = ', 2i5, 1x, a12,/)

  274  format ('          idz(j,k) = ', 2i5, 1x, a12,/
     1        ('          idz(j,k) = ', 2i5, 1x, a12))
       goto 290
  280       write(99,*) 
     1        '  Outdat; Problem only 1 input file provided',
     1        '    for a Multiple, Merge, Difference or Diffx request'
       goto 290

  290  write(6,*) '  Outdat; Problem see log file'
       write(6,*) 'Stop 1'
       call flush(6)
       stop 
       end
