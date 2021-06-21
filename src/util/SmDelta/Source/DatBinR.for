C
c *********************************************************
       subroutine datbinr
c
c         Get binary reservoir data
c                             
c
      include 'SmDelta.inc'
c
c               nres = # of output values in file, 
c
c rrb 99/12/03
      if(iwellr.eq.1) then
        nres = 21
      else
        nres = 24
      endif

      nrsactx=nrsact+numown
c     write(99,*) 'nrsact, numown, nrsactx'
c     write(99,*) nrsact, numown, nrsactx

      ir1 = 0
      isy=0
      iresop=2
      cu=1.9835               
      isx(ifx) = 0     
      ry2(ifx) = 0                          
c
c               Set id screen switch
c     if(idreq(1).ne.'0           ') then
      if(idallx.eq.1) then
        idall =iid
        nid=iid
        nr1 = 0
      else
        idall=0
        nid=numres
      endif
c
c               Screen for year as necessary
c ---------------------------------------
      call scryr(ichk, iystr, iyend, iystr0, iyend0, iyreq, imreq,
     1           fillog)
c
c               Selected ID Loop
      do 140 n=1,nid
        isx(ifx) = isx(ifx) + 1
c
c               Screen for ID
        if(idall.eq.0) then
          nr=n
          ir1=ir1+1
          nr1=nr
          idz(ifx,nr) = cresid(nr)
          namex(ifx,nr) = resnam(nr)
c         write(99,*) ' idall, nr, ir1, nr1', idall, nr, ir1, nr1
        else      
          call getid(n, nr) 
          ir1=nowner(nr) + nr - 1
          nr1=nr1+1
c         write(99,*) ' idall, nr, ir1, nr1', idall, nr, ir1, nr1
        endif
          
        if(iressw(nr).eq.0) goto 140

          write(6,100) ifx, nr1, idz(ifx,nr1)
          if(iout.eq.1) write(99,100) ifx, nr1, idz(ifx,nr1)

  100     format('+',' Processing file & station ', 2i5, 1x, a12)
          call flush(6)
c              
          do 130 iy=iystr,iyend
            if(isy.eq.0) ry2(ifx) = ry2(ifx)+1
        
            do 120 im=1,12
c
              irecr=((iy-iystr0)*12+(im-1))*nrsactx+ir1+numtop
              read(44,rec=irecr,err=160) (dat1(i),i=1,nres)
c             write(99,*) (dat1(i),i=1,nres)
                     
              ida  = dat1(nres-1) 
              nacc = dat1(nres)

              cx = cu         
              if(iresop.ne.1) cx=cu*mthday(im)
              do 110 i=1,nres-2
                dat1(i)=dat1(i)*cx
  110         continue
c             write(99,*) (dat1(i),i=1,nres)
c
c               Set requested parameter 
              if(ip.ge.2 .and. ip.le.13) then
                dels(ifx,nr1) = dels(ifx,nr1) + dat1(ip)
              endif
c
c               End Year and Month Loop      
  120       continue
c
c               Set requested parameters for non summing data
c               1 = initial content, 14 = eom content
            if(ip.eq.1 .or. ip.ge.14) then
              dels(ifx,nr1) = dels(ifx,nr1) + dat1(ip)  
            endif
  130     continue
          isy = isy+1
c
c               Skip over subaccounts
            if(nacc-ida.gt.1) ir1 = ir1 + nacc-ida -1
c
c               End Reservoir Loop
  140   continue    
c
c               End Requested ID loop
  150 continue
      return
c
c               Error Messages
c ------------------------------------------------------------


  160 write(6,*) '  Datbinr; Problem see the log file'
      write(99,*) '  Datbinr; Problem reading binary data'
      write(6,*) 'Stop 1'
      call flush(6)
      STOP 
      END           




