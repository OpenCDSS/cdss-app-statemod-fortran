c                            
c ************************************************************
        subroutine datdiv
c
c rrb 99/12/29; Revised to handle instream flows
c
       include 'SmDelta.inc'

       character  cmon*3,    name24*24, rec14*14, 
     1            rec72*72,  rec2*2,
     1            ids*12,    idr*12,
     1            recmo*3,   recyr*4,   rec8*8

        iout=1
        ry2(ifx) = 0.0
        is = 0  
        ifound = 0
        ids = '999999999999'
        name24='NA'
c
c rrb 99/12/03
        if(iwelld.eq.1) nstr=17
c       if(iwelld.eq.2) nstr=22
        if(iwelld.eq.2) nstr=27
c
c rrb 2006/11/22; Version 11x
        if(iwelld.eq.3) nstr=29

c       write(nlog,*) ' iyx, iyreq, imreq', iyx, iyreq, imreq
c
c -----------------------------------------
c
c               Get data
         iyread = 0
c 
c rrb 99/12/28; screen for diversions
c 100    read(1,'(a72)',end=150,err=150) rec72
   90    read(1,'(a72)',end=150,err=150) rec72 
c        write(nlog,'(a72)') rec72
c         
c rrb 99/12/22; Test for a diversion (abs (nd) 0<x<5000)
         rec14 = rec72(5:18)  
         if(rec14.ne.'STRUCTURE ID (') goto 90 
         
         rec8=rec72(46:53)
         read(rec8,'(i8)') nd
         nd1=iabs(nd)
         if(idiv.eq.0 .and. nd1.gt.5000) goto 90
         if(idiv.eq.3 .and. (nd1.lt.5000 .or. nd1.gt.7500)) goto 90
         
         if(iout.eq.1) write(nlog,*) ' DatStr; nd1 = ', nd1
         
c
c rrb 99/12/29; Count # found
         ifound=ifound+1
c rrb 02/01/03; No longer required for a diversion
c        ifounds=0

  100    read(1,'(a72)',end=150,err=150) rec72 
         rec14 = rec72(5:18)
c
c _________________________________________________________
c		Check for new structure
c rrb 99/12/29; Screen for a new structure and type diversion
         if(rec14.eq.'STRUCTURE ID (') then  
           rec8=rec72(46:53)
           read(rec8,'(i8)') nd
           nd1=iabs(nd)
c
c               Screen for diversion (idiv=0) or instream flow (idiv=3)
           if(idiv.eq.0 .and. nd1.gt.5000) goto 90
           if(idiv.eq.3 .and. (nd1.lt.5000 .or. nd1.gt.7500)) goto 90
           ifound=ifound+1
         endif
c
c               Check to see if it is a printout
c               where no structures or stream gages exist 
c               Note, RIVER LOCATION 
c               appears twice at the top per structure
c
c               Set Structure Name
         if(rec14.eq.'STRUCTURE NAME')  name24=rec72(32:55)
         if(iout.eq.1) write(nlog,*) ' DatStr; Name24 = ', name24
              
c
         
c _________________________________________________________
c        
c               Screen non data
c rrb 2006/11/22; Update
c        if(rec72(26:27).ne.'19') goto 100
         rec2=rec72(26:27)
         iyok=0
         if(rec2.eq.'19') iyok=1
         if(rec2.eq.'20') iyok=1
         if(iyok.eq.0) goto 100
         
         
         
c
c               Screen for annual data
         recmo = rec72(32:34)
         if(iyx.le.1 .and. recmo.ne.'TOT') goto 100
c
c               Screen for selected year
         recyr = rec72(26:29)
         read(recyr,'(i4)') iy
         if(iyx.ge.1 .and. iy.ne.iyreq) goto 100
c
c               Screen for selected month
         if(iyx.eq.2 .and. recmo.ne.imreq) goto 100

         iyread = iyread+1
c
c               Screen for ID, as necessary
         rec12=rec72(1:12)
         if(rec12.eq.'            ') goto 100
c        if(idreq(1).ne.'0           ') then
         if(idallx.eq.1) then
           do 110 i=1,iid
             if(idreq(i).eq.rec12) goto 120
  110      continue
           goto 90
         endif                   
c
  120    if(rec12.eq.ids) then
           ry2(ifx) = ry2(ifx)+1
         else
           is = is+1          
           ry2(ifx) = 1
           write(6,130)  ifx, is, rec12
           if(iout.eq.1) write(nlog,130) ifx, is, rec12
  130      format('+',' Processing file & station ', 2i5, 1x, a12)
           call flush(6)
c          write(nlog,*) ' '
         endif
c
         backspace(1)
c
c               Diversion or Instream Flow
         read(1,140,end=150,err=150) ids,idr,iy,cmon,(x1(m),m=1,nstr)
c        write(nlog,140) ids, idr, iy, cmon, (x1(m),m=1,nstr)
  140    format(2a12, i5, 2x, a3, 30f8.0)
         idr = idr

         cmon=cmon

         dels(ifx,is) = dels(ifx,is) + x1(ip)      
c        write(nlog,*) '  Datdiv, is, ifx, dels', is, dels(ifx,is)

         namex(ifx,is) = name24
         idz(ifx,is) = ids

c        write(nlog,*) ' '
c        write(nlog,*) 'datdiv; idz(ifx,is) ', idz(ifx,is)
c
c               Get more data
         goto 100
  150    if(is.eq.0) goto 160
c
c               Print number found
         if(idiv.eq.0) write(nlog,152) ifound
         if(idiv.eq.3) write(nlog,153) ifound 

         return
c
c               Formats and Error Processing
c ___________________________________________________________
  152    format(' # of Diversions:     ', i5)
  153    format(' # of Instream Flows: ', i5)
  160    write(nlog,170) iyreq, imreq, iyread
  170    format(
     1     '   Problem reading diversion data',/,
     1     '   Check your file(s) exist and are not empty',/
     1     '   Check your requested id exist',/
     1     '   Requested year and month: ', i4, 1x, a3,/
     1     '   Number of records passing year & month constraint:',i5,/
     1     '   Requested ID (if not all):')
c        if(idreq(1).ne.'0           ') then
         if(idallx.eq.1) then
           write(nlog,180) (idreq(i), i=1,iid)
         endif
  180    format((6x, a12))

         write(6,190) fillog
         write(nlog,190) fillog
  190    format('  Datdiv - Unsuccessful termination, see ', a72)
         write(6,*) 'Stop 1'
         call flush(6)
         stop 
         end
