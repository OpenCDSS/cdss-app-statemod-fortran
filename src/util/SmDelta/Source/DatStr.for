c       
c ************************************************************
        subroutine datstr
c
c rrb 99/22/99; Revised to better locate stream gages using
c               new data printed to *.xdd on structure ID line
c rrb 99/12/28; Revised to screen for diversions only (exclude
c               instream flows and reservoirs
c rrb 99/12/29; Revised to handle StreamID
c
c
c		iyx=1 Ave
c		iyx=2 Year
c		iyx=3 Year and Month   
c
c		idallx=0 print all ID's
c		idallx=1 print 1 ID     

c
         include 'SmDelta.inc'

        character cmon*3,    name24*24, rec14*14, 
     1            rec72*72,  rec2*2, 
     1            ids*12,    idr*12,
     1            recmo*3,   recyr*4, rec8*8

        iout=1
        ry2(ifx) = 0.0
        is = 0  
        ifound=0
c
c rrb 99/12/03
        if(iwelld.eq.1) nstr=17
        if(iwelld.eq.2) nstr=27
c
c rrb 2006/11/22; Version 11x
        if(iwelld.eq.3) nstr=29

c       ids = '999999999999'
        idr = '999999999999'
c
c -----------------------------------------
c
c               Get data
c rrb 99/12/22
c 100    read(1,'(a72)',end=150,err=150) rec72
   90    read(1,'(a72)',end=150,err=150) rec72  
         if(iout.eq.2) write(nlog,*) ' DatStr; rec72_90 = ', rec72
c         
c rrb 99/12/22; Test for a stream gage (nd is less than zero)
         rec14 = rec72(5:18)  
         if(rec14.ne.'STRUCTURE ID (') goto 90 
c
c _________________________________________________________
c               Found Structure ID, set nd
         if(iout.eq.2) write(nlog,*) ' DatStr; rec14 = ', rec14
         
         rec8=rec72(46:53)
         read(rec8,'(i8)') nd
         if(nd.ge.0) goto 90
         if(iout.eq.1) write(nlog,*) ' DatStr; nd = ', nd
         
c
c rrb 99/12/29; StreamID option
c		Note idiv=5 is a stream ID
         rec1=rec72(33:33)
c
c rrb 2006/11/22; UPdate         
c        if(idiv.eq.5 .and. rec1.ne.'0') goto 90
         iok=0
         if(idiv.eq.5 .and. rec1.eq.'0') iok=1
         if(idiv.eq.5 .and. rec1.eq.'B') iok=1
         if(iok.eq.0) goto 90
         
         if(iout.eq.1) write(nlog,*) ' DatStr; rec1 = ', rec1

         ifound=ifound+1
c
c _________________________________________________________
c               Found a stream gage or StreamID
c
c rrb 2006/11/22; Skip Structure account
  100    read(1,'(a72)',end=150,err=150) rec72
         rec14 = rec72(5:18)  
         if(rec14.ne.'STRUCTURE NAME')  goto 100
         if(iout.eq.2) write(nlog,*) ' DatStr_100; rec72 = ', rec72
c
c
c               Set structure name
         name24=rec72(32:55)
         if(iout.eq.1) write(nlog,*) ' DatStr; Name24 = ', name24
c
c _________________________________________________________
c               Found an ID and Name
  110    read(1,'(a72)',end=150,err=150) rec72
         rec14 = rec72(5:18)  
c
c _________________________________________________________
c		Check for a new ID (indicated by Structure ID)
c rrb 99/12/29; Screen for a new structure and type streamflow (nd<0)
         if(rec14.eq.'STRUCTURE ID (') then  
           rec8=rec72(46:53)
           read(rec8,'(i8)') nd
           if(nd.ge.0) goto 90
c
c rrb 99/12/29; StreamID option
           rec1=rec72(33:33)
           if(idiv.eq.5 .and. rec1.ne.'0') goto 90
           ifound=ifound+1
         endif

c _________________________________________________________
c        
c               Screen non data
c rrb 2006/11/22; Update
         rec2=rec72(26:27)
         iyok=0
         if(rec2.eq.'19') iyok=1
         if(rec2.eq.'20') iyok=1
         if(iyok.eq.0) goto 110
c
c _________________________________________________________
c               Found data, Check for total or year of interest
c
c               Screen Stream for annual data
         recmo = rec72(32:34)
         if(iout.eq.2) write(nlog,*) ' DatStr; recmo = ', recmo
         if(iyx.le.1 .and. recmo.ne.'TOT') goto 110
         if(iout.eq.1) write(nlog,*) ' DatStr; recmo = ', recmo
c
c
c               Screen Stream for selected year
         recyr = rec72(26:29)
         read(recyr,'(i4)') iy
         if(iout.eq.1) write(nlog,*) ' DatStr; iy = ', iy
         if(iyx.ge.1 .and. iy.ne.iyreq) goto 110
         
c
c               Screen Diversions for selected month
         if(iyx.eq.2 .and. recmo.ne.imreq) goto 110
         if(iout.eq.1) write(nlog,*) ' DatStr; recmo = ', recmo
c
c _________________________________________________________
c               Found time Check for ID, as necessary
         rec12=rec72(13:24)                  
         if(iout.eq.1) write(nlog,*) ' DatStr; ID = ', rec12
c        if(iout.eq.1) write(nlog,*) ' DatStr; idallx = ', idallx

         if(idallx.eq.1) then
           do i=1,iid
             if(idreq(i).eq.rec12) goto 120
           end do
c
c rrb 99/12/22
c          goto 100
           goto 90
         endif
c
c
c _________________________________________________________
c               Found ID process this gage
  120    if(rec12.eq.idr) then
           ry2(ifx) = ry2(ifx)+1
         else
           is = is+1          
           ry2(ifx) = 1
           write(6,130)  ifx, is, rec12
           if(iout.eq.1) write(nlog,130)  ifx, is, rec12

  130      format('+',' Processing file & station ', 2i5, 1x, a12)
           call flush(6)
c          write(nlog,*) ' '
         endif
c
         backspace(1)
c
c               StreamGage
         read(1,140,end=150,err=150) ids,idr,iy,cmon,(x1(m),m=1,nstr)
  140    format(2a12, i5, 2x, a3, 30f8.0)
         ids = ids

         cmon=cmon

         dels(ifx,is) = dels(ifx,is) + x1(ip)      
c        write(nlog,*) is, dels(ifx,is)

         namex(ifx,is) = name24
         idz(ifx,is) = idr                      
c        write(nlog,142) ifx, is, idz(ifx,is) 
c 142    format('  Datstr; ifx, is, idz(ifx,is)', 2i5,1x,a12)

c
         goto 110
  150    if(is.eq.0) goto 160
c
c               Print number found
         write(nlog,152) ifound

         return
c
c               Formats and Error Processing
c _________________________________________________________
  152    format(' # of Stream Gages:       ', i5)
c
c               Error message
  160    write(nlog,170) 
  170    format(
     1     '   Problem reading stream data',/,
     1     '   Check your file(s) exist and are not empty',/
     1     '   Check your requested id exist')
         write(6,180) fillog
         write(nlog,180) fillog
  180    format('  Datstr - Unsuccessful termination, see ', a72)
         write(6,*) 'Stop 1'
         call flush(6)
         stop 
         end
