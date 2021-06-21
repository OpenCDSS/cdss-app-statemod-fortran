c                            
c ************************************************************
        subroutine datres
c
         include 'SmDelta.inc'

         character  cmon*3,    name24*24, rec14*14, 
     1            rec72*72,  
     1            ids*12,    
     1            recmo*3,   recyr*4

       ry2(ifx) = 0.0
       is = 0  
       ids = '999999999999'
c
c rrb 99/12/03
       if(iwellr.eq.1) nres = 19
       if(iwellr.eq.2) nres = 21
c
c -----------------------------------------
c
c               Get data
  100    read(1,'(a72)',end=150,err=150) rec72

         rec14 = rec72(5:18)
         if(rec14.eq.'RESERVOIR NAME')  name24=rec72(32:55)
c
c               Screen non data
         if(rec72(19:20).ne.'19') goto 100
c        write(nlog,*) ' past 19'
c
c               Screen Reservoirs for annual data and selected year
         recmo = rec72(25:27)
         if(iyx.le.1 .and. recmo.ne.'TOT') goto 100
c        write(nlog,*) ' past tot'
c
c               Screen for total account (0)
         if(rec72(17:17).ne.'0') goto 100
c        write(nlog,*) ' past account 0'
c
c               Screen Reservoirs for selected year
         recyr = rec72(19:22)
         read(recyr,'(i4)') iy
         if(iyx.ge.1 .and. iy.ne.iyreq) goto 100
c        write(99,*) ' past year'
c
c               Screen Reservoir for selected month
         if(iyx.eq.2 .and. recmo.ne.imreq) goto 100
c        write(99,*) ' past month'
c
c               Screen for ID, as necessary 
         rec12=rec72(1:12)

         if(idallx.eq.1) then
           do 110 i=1,iid
             if(idreq(i).eq.rec12) goto 120
  110      continue
           goto 100
         endif

c        write(99,*) ' past id'
  120    if(rec12.eq.ids) then
           ry2(ifx) = ry2(ifx)+1
         else
           is = is+1          
           ry2(ifx) = 1
           write(6,130)  ifx, is, rec12
           if(iout.eq.1) write(99,130)  ifx, is, rec12

  130      format('+',' Processing file & station ', 2i5, 1x, a12)
           call flush(6)
c          write(99,*) ' '
         endif
c
         backspace(1)
c
c               Reservoir
         read(1,140,end=150,err=150) ids,ia,iy,cmon,(x1(m),m=1,nres)
c        write(99,140) ids, ia, iy, cmon, (x1(m),m=1,nres)
  140    format(a12, 2i5, 2x, a3, 30f8.0)
         ia = ia
         cmon=cmon

         dels(ifx,is) = dels(ifx,is) + x1(ip)      
c        write(99,*) is, dels(is)

         namex(ifx,is) = name24
         idz(ifx,is) = ids                      
c
c               Loop for average
         goto 100

  150    if(is.eq.0) goto 160
         return

c
c               Error message
  160    write(99,170) 
  170    format(
     1     '   Problem reading reservoir data',/,
     1     '   Check your file(s) exist and are not empty',/
     1     '   Check your requested id exist')
         write(6,180) fillog
         write(99,180) fillog
  180    format('  Datres - Unsuccessful termination, see ', a72)
         write(6,*) 'Stop 1'
         call flush(6)
         stop 
         end
