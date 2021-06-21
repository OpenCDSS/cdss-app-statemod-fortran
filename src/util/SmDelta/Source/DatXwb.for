c                            
c ************************************************************
        subroutine datXwb
c
c 	It reads a Water Budget summary (*.Xwb) file
c
c               ifx     file counter
c               iyx     year code; 
c                       0=average annual total, 
c                       1=specified year total,
c                       2=specified year and month
c		ip      Element of interest (13=annual total)
c		is 	Structure counter

       include 'SmDelta.inc'
       dimension y1(100)
       character  cmon*3,    name24*24, rec14*14, 
     1            rec72*72,  rec2*2,    
     1            ids*12,    idr*12,
     1            recmo*3,   recyr*4,   rec8*8
c
c		iout=0 no details
c		iout=1 details
c		iout=2 summary
        iout=2
c
       write(nlog,*) ' '
 	     write(nlog,*) 'DatXwb; Statemod Version = ', iver        
        nstr=21
        ip=16
        idS=rec12
        is=1
        
        ry2(ifx) = 0.0
        ifound = 0
        ids = '999999999999'
        name24='NA'
c
        cmon='NA'
        rec12='Basin_Total '
        name24='Basin_Total             '
        
c       write(nlog,*) ' iyx, iyreq, imreq', iyx, iyreq, imreq
c
c _________________________________________________________
c		Step 3; Get Data
         iyread = 0

  100    read(1,'(a72)',end=150,err=150) rec72 
         if(iout.eq.1) write(nlog,*) '  DatXwb; ', rec72
c        
c               Screen non data
         rec2=rec72(2:3)
         iyok=0
         if(rec2.eq.'19') iyok=1
         if(rec2.eq.'20') iyok=1
         if(iyok.eq.0) goto 100
c
c               Screen for annual data
         recmo = rec72(7:9)
         if(iyx.le.1 .and. recmo.ne.'Tot') goto 100
         
c
c ---------------------------------------------------------
c               Screen for selected year
c                   iyx=0=average annual total, 
c                       1=specified year total,
c                       2=specified year and month
         recyr = rec72(2:5)
         read(recyr,'(i4)') iy
         if(iyx.ge.1 .and. iy.ne.iyreq) goto 100

         iyread = iyread+1
c
c ---------------------------------------------------------
c               Count years in file ifx
         ry2(ifx) = ry2(ifx)+1
c
c rrb         
         write(6,130)  ifx, is, rec12
         if(iout.ge.1) write(nlog,130) ifx, is, rec12
  130    format('+',' Processing file & station ', 2i5, 1x, a12)
         call flush(6)
c        write(nlog,*) ' '
c
c ---------------------------------------------------------
c               Read Water Budget Output
         backspace(1)
         read(1,'(a72)') rec72
         if(iout.eq.1) write(nlog,'(a72)') rec72
         
         backspace(1)
         read(1,140,end=150,err=150) iy,cmon, (x1(m),m=1,nstr)
c        read(1,*,end=150,err=150) iy,cmon, (x1(m),m=1,nstr)

         if(iout.eq.1) then
           write(nlog,140) iy, cmon, (x1(m),m=1,nstr)
cx           do m=1,nstr
cx             write(nlog,'(i5, f12.0)') m, x1(m)
cx           end do
         endif
  140    format(i5,1x,a3,1x,40f12.0)
c
c ---------------------------------------------------------
c rrb 2007/03/17; Adjust to equal version 11 output
c		  with reservoir seepage
         if(iver.le.10) then
           y1(11)=0.0
           do i= 11,nstr
             y1(i+1)=x1(i)
           end do  
           do i=11,nstr
             x1(i)=y1(i)
           end do
         endif    

         cmon=cmon
c
c ---------------------------------------------------------
c               Set Data, ID and Name
c
c		For *.xwb store all elements
cx       dels(ifx,is) = dels(ifx,is) + x1(ip)      
cx       namex(ifx,is) = name24
cx       idz(ifx,is) = ids

         rec12='Column_xx  '
         do m=1,nstr        
           dels(ifx,m) = dels(ifx,m) + x1(m)      
           write(rec2,'(i2)') m
           
           rec12(8:9)=rec2
           idz(ifx,m) = rec12
           namex(ifx,m) = rec12
           
           if(iout.eq.1) 
     1      write(nlog,*) '  DatXwb;, m, idz, dels', 
     1        m,idz(ifx,m), dels(ifx,m)
         end do  


c
c               Get more data
         goto 100
  150    if(is.eq.0) goto 160
         is=nstr
         ifound=nstr
         write(nlog,*) ' DatXwb; is ', is
c
c		Set parameters
         nameX(ifx,1)='Stream_Inflow'         
         nameX(ifx,2)='Return'         
         nameX(ifx,3)='From/To_GWStor'         
         nameX(ifx,4)='From_Soil'         
         nameX(ifx,5)='From_Plan'         
         nameX(ifx,6)='Total_Inflow'         
         nameX(ifx,7)='Divert'         
         nameX(ifx,8)='From_River_By_Well'         
         nameX(ifx,9)='Well_Depletion'         
         nameX(ifx,10)='Reservoir_Evaporation'         

         nameX(ifx,11)='Reservoir_Seepage'         
         nameX(ifx,12)='Stream_Outflow'         
         nameX(ifx,13)='Reservoir_Change'         
         nameX(ifx,14)='To_SoilM'         
         nameX(ifx,15)='SoilM_Change'         
         nameX(ifx,16)='Total_Outflow'         
         nameX(ifx,17)='Delta'         
         nameX(ifx,18)='CU'         
         nameX(ifx,19)='Loss'         
         nameX(ifx,20)='Pumping'         
         nameX(ifx,21)='Salvage'         
c
c               Print number found
         write(nlog,152) ifound

         return
c
c               Formats and Error Processing
c ___________________________________________________________
  152    format(' # of Water Budget Entries: ', i5)
  160    write(nlog,170) iyreq, imreq, iyread
  170    format(
     1     '   Problem reading Water Budget data',/,
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
  190    format('  DatXwb - Unsuccessful termination, see ', a72)
         write(6,*) 'Stop 1'
         call flush(6)
         stop 
         end
