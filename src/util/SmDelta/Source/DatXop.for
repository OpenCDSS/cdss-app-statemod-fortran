c                            
c ************************************************************
        subroutine datXop
c
c 	It reads an operating rule summary (*.xop) file
c
c               ifx     file counter
c               iyx     year code; 
c                       0=average annual total, 
c                       1=specified year total,
c                       2=specified year and month
c		            ip      Element of interest (13=annual total)
c		            is 	Structure counter
c
c Updates
c
c rrb 2020/12/29 Correction to screen and process Average data
c                that did not work previously
c

       include 'SmDelta.inc'

       character  cmon*3,    name24*24, rec14*14, 
     1            rec72*72,  rec2*2,
     1            ids*12,    idr*12,
     1            recmo*3,   recyr*4,   rec8*8,
     1            ctype*2
c
c		              iout=0 no details
c		              iout=1 details
c		              iout=2 summary
c                 iout=3
c                 iout=4 echo data read
        iout=2
        nstr=13
        ip=13
        cmon='NA'
        
        ry2(ifx) = 0.0
        is = 0  
        ifound = 0
        ids = '999999999999'
        name24='NA'
        
        write(nlog,*) ' ifx, iyx, iyreq, ' 
        write(nlog,'(20i5)') ifx, iyx, iyreq
c
c _________________________________________________________
c		Step 1; Find Structure ID
   90    read(1,'(a72)',end=150,err=150) rec72  
         if(iout.eq.4) write(nlog,*) ' DatXop; rec72_90 = ', rec72
c         
c rrb 99/12/22; Test for an ID
         rec2 = rec72(2:3)  
         if(rec2.ne.'ID') goto 90 
c
c _________________________________________________________
c               Step 2; Set Structure ID
         
         rec12=rec72(7:18)
         ifound=ifound+1
c
c _________________________________________________________
c               Step 2; Set Structure Name
c
c rrb 2019/07/30; Correction to match current file format
cx       name24=rec72(27:50)
         name24=rec72(33:56)
c
c _________________________________________________________
c               Step 2b; Set Operating Rule type
         ctype=rec72(71:72)
c
c rrb 2019/07/30; Correction to match current file format
c _________________________________________________________
c		Step 3; Get Data
         iyread = 0

  100    read(1,'(a72)',end=150,err=150) rec72 
cx       if(iout.eq.4) write(nlog,*) '  DatXop; ', rec72
c        
c               Screen non data
         rec2=rec72(1:2)
         iyok=0
         if(rec2.eq.'19') iyok=1
         if(rec2.eq.'20') iyok=1
c ---------------------------------------------------------
c rrb 2020/12/29; Correction to read average
c               Screen for average data
cx       if(iyok.eq.0) goto 100
         if(iyx.ge.1 .and. iyok.eq.0) goto 100
c
c ---------------------------------------------------------
c rrb 2020/12/29; Correction to read average
c               Screen for average data
         recmo = rec72(1:3)
         if(iyx.eq.0 .and. recmo.ne.'AVG') goto 100

c ---------------------------------------------------------
c               Screen for selected year
c                   iyx=0=average annual total, 
c                       1=specified year total,
c                       2=specified year and month
c
c rrb 2020/12/29; Correction add average
         if(iyx.eq.1) then
           recyr = rec72(1:4)
           read(recyr,'(i4)') iy
           if(iyx.ge.1 .and. iy.ne.iyreq) goto 100
         endif

         iyread = iyread+1
c
c ---------------------------------------------------------
c               Screen for ID, as necessary
         if(rec12.eq.'            ') goto 100
         if(idallx.eq.1) then
           do 110 i=1,iid
             if(idreq(i).eq.rec12) goto 120
  110      continue
           goto 90
         endif                   
c
c ---------------------------------------------------------
c               Count years in file ifx
  120    if(rec12.eq.ids) then
           ry2(ifx) = ry2(ifx)+1
         else
c
c rrb         
           ids=rec12
           is = is+1          
           ry2(ifx) = 1
           write(6,130)  ifx, is, rec12
           if(iout.eq.2) write(nlog,130) ifx, is, rec12
  130      format('+',' Processing file & station ', 2i5, 1x, a12)
           call flush(6)
c          write(nlog,*) ' '
         endif
c
c ---------------------------------------------------------
c               Read Operational Right Output
         backspace(1)
c
c rrb 2020/12/29; Correction allow average year
cx       read(1,140,end=150,err=150) iy,(x1(m),m=1,nstr)
         read(1,140,end=150,err=150) recyr,(x1(m),m=1,nstr)
         if(iyx.gt.1) then
           read(recyr,'(i4)') iy
         else
           iy = -1
         endif
c
c rrb 2019/08/25; Detailed output by year        
cx       if(iout.eq.2)  then 
         if(iout.eq.4) then
           write(nlog,140) recyr, (x1(m),m=1,nstr)
         endif
cx140    format(i5, 13f8.0)
  140    format(a4, 1x, 13f8.0)

         cmon=cmon
c
c ---------------------------------------------------------
c               Set Data, ID and Name
         dels(ifx,is) = dels(ifx,is) + x1(ip)   
c
c rrb 2019/08/25; Detailed output by year        
         if(iout.eq.4) then 
           if(iyread.le.0) then  
             write(nlog,*) 
     1         '  DatXop;    # Name                    ',
     1         '   iy   is       x   dels' 
           endif
           
           write(nlog,'(a12,1x, i5,1x, a24,1x, 2i5, 20f8.0)')
     1       '  DatXop;   ', iyread, name24, iy, is, x1(ip), 
     1       dels(ifx,is)
         endif

         namex(ifx,is) = name24
         idz(ifx,is) = ids
c
c rrb 2019/08/11; Store opr type
         ctypeX(ifx,is) = ctype

c        write(nlog,*) ' '
c        write(nlog,*) '  DatXop; idz(ifx,is) ', idz(ifx,is)
c
c               Get more data
cx         write(nlog,*) ' '
cx         write(nlog,*) ' DatXop; is = ', is
         goto 90
  150    if(is.eq.0) goto 160
c
c               Print number found
         write(nlog,152) ifound

         return
c
c               Formats and Error Processing
c ___________________________________________________________
  152    format(' # of Operational Rights: = ', i5)
  160    write(nlog,170) iyreq, imreq, iyread
  170    format(
     1     '   Problem reading operational right data',/,
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
  190    format('  DatXop - Unsuccessful termination, see ', a72)
         write(6,*) 'Stop 1'
         call flush(6)
         stop 
         end
