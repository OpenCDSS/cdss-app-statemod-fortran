c                            
c ************************************************************
       subroutine datbin
c
c               Subroutine reads binary diversion or stream data
c
c               ndiv = # of elements in binary file (23)
c               numsta = # of river nodes in output file
c
c               idall  = 0 get all stations
c                      = n counter of requested id's
c               isy    = 0, switch to count number of years of data
c                           first time thru structure 1
c               is1    = 1-n counter of structures read
c
c               idiv = 0 diversion requested
c                      1 streamgage (baseflow points) requested
c                      2 reservoir requested
c                      3 instream flow
c                      4 well requested
c                      5 streamID (ID = "0...."
c
c
      include 'SmDelta.inc'

      ifound2=0
      ry2(ifx) = 0.0
c
c rrb 99/12/03
      if(iwelld.eq.1) then
        ndiv = 23
      else
c
c rrb 02/01/03; Binary file size
        ndiv = 33
      endif

      iresop=2
      cu = 1.9835       
      isy=0
      isx(ifx) = 0

c     if(idreq(1).ne.'0           ') then
      if(idallx.eq.1) then
        idall=iid
        nid=iid
      else
        idall=0
        nid=numsta
      endif
c
c     write(99,*) ' '
c     write(99,*) '  Datbin; idallx, nid, numsta = ', idallx,nid,numsta
c
c               Screen for year as necessary
c ---------------------------------------
      call scryr(ichk, iystr, iyend, iystr0, iyend0, iyreq, imreq,
     1           fillog)
c
c               Get data
c ---------------------------------------

      do 200 n=1,nid
        isx(ifx) = isx(ifx) + 1

c
c               Find station ID or river ID as necessary 
c ---------------------------------------   
        if(idall.eq.0) then                           
          call getsta(n,is1,ifound)
c         write(99,*) '  Datbin; ifound = ',ifound

          if(ifound.eq.0)  goto 200
          ifound2=ifound2+1
        else
          call getid(n,is1)
        endif
c
c               Finally get data

        write(6,150) ifx, is1, idz(ifx,n)
c       if(iout.eq.1) write(99,150) ifx, is1, idz(ifx,n)
  150   format('+',' Processing file & station ', 2i5, 1x, a12)
c
        if(iout.eq.1) write(99,250) ifx, is1, idz(ifx,n)
  250   format('  Datbin;  Processing file & station ', 2i5, 1x, a12)

        do 190 iy=iystr,iyend
          if(isy.eq.0) ry2(ifx) = ry2(ifx) + 1

          do 160 i=1,ndiv
            dat1t(i) = 0.0
  160     continue

            do 180 im=1,12
              cx = cu         
              if(iresop.ne.1) cx=cu*mthday(im)
           

              irecs=((iy-iystr0)*12+(im-1))*numsta+is1+numtop
    
              read(43,rec=irecs,err=210) (dat1(i),i=1,ndiv)
c             write(99,*) is, iy, im, ndiv, (dat1(i)*cx, i=1,ndiv)

              do 170 i=1,ndiv-2
                dat1(i)=dat1(i)*cx
                dat1t(i) = dat1t(i) + dat1(i)
  170         continue

            dels(ifx,n) = dels(ifx,n) + dat1(ip)      
c           write(99,*) ' is1, iy, im, ip, dat1(ip)'
c           write(99,*)   is1, iy, im, ip, dat1(ip)
  180       continue
c           write(99,*) ' Datbin ifx, n, dels(ifx,n)', dels(ifx,n)
c           write(99,*) ' '
  190     continue
          isy=isy+1
  200   continue                                      
c  
c               Print number found
         if(idiv.eq.0) write(99,151) ifound2
         if(idiv.eq.1) write(99,152) ifound2
c        if(idiv.eq.2) write(99,153) ifound2          
         if(idiv.eq.3) write(99,154) ifound2 
         if(idiv.eq.4) write(99,155) ifound2          
         if(idiv.eq.5) write(99,156) ifound2 

         return
c
c               Formats and Error Processing
c ___________________________________________________________
  151   format(' # of Diversions:         ', i5)
  152   format(' # of Stream Gages        ', i5)
  153   format(' # of Reservoirs          ', i5)
  154   format(' # of InstreamFlows       ', i5)  
  155   format(' # of Wells               ', i5)  
  156   format(' # of StreamID            ', i5)  

  210   write(99,*) '  Datbin; problem reading binary data'
  212   write(6,220) fillog
        write(99,220) fillog
  220   format('  Datbin; Unsuccessful termination, see ', a72)
        write(6,*) 'Stop 1'
        call flush(6)
        stop 
        end

