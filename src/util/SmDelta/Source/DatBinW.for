C
c *********************************************************
       subroutine datbinw
c
c         Get binary well data
c                             
c
      include 'SmDelta.inc'
c
c               nres = # of output values in file,
c
c               Step 1; Initilize
c _________________________________________________________
c
      nwel= 18
c     write(99,*) 'nrsact, numown, nrsactx'
c     write(99,*) nrsact, numown, nrsactx

      iw1 = 0
      isy=0
      iresop=2
      ifound2=0
      cu=1.9835               
      isx(ifx) = 0     
      ry2(ifx) = 0                          
c
c _________________________________________________________
c
c               Step 2; Set id screen switch
c     if(idreq(1).ne.'0           ') then
c     write(99,*) ' Datbinw; idallx = ', idallx

      if(idallx.eq.1) then
        idall =iid
        nid=iid
      else
        idall=0
        nid=numdivw
      endif
c
c     write(99,*) ' Datbinw; idall = ', idall
c
c _________________________________________________________
c
c               Step 3; Screen for year as necessary
c
      call scryr(ichk, iystr, iyend, iystr0, iyend0, iyreq, imreq,
     1           fillog)
c
c _________________________________________________________
c
c               Step 4; Selected ID Loop
      do 140 n=1,nid
        isx(ifx) = isx(ifx) + 1
c
c _________________________________________________________
c
c               Step 5; Screen for ID
        if(idall.eq.0) then
c
c               Well data is by well not stream
c         call getsta(n,is1,ifound)
c         if(ifound.eq.0) goto 150

          nw=n
          iw1=iw1+1
          idz(ifx,nw)=cdividw(nw)
          namex(ifx,nw)=divnamw(nw)
c         write(99,*) ' Datbinw; idall, nw ', idall, nw

          ifound2=ifound2+1
        else
          call getid(n,iw1)
        endif
          

          write(6,100) ifx, nw, idz(ifx,nw)
          if(iout.eq.1) write(99,100) ifx, nw, idz(ifx,nw)
  100     format('+',' Processing well file & station ', 2i5, 1x, a12)
          call flush(6)
c
c _________________________________________________________
c
c               Step 6; Year Loop
c
          do 130 iy=iystr,iyend
            if(isy.eq.0) ry2(ifx) = ry2(ifx)+1

            do i=1,nwel
              dat1t(i)=0.0
            end do
c
c _________________________________________________________
c
c               Step 7; Month Loop
            do 120 im=1,12
c
              cx = cu         
              if(iresop.ne.1) cx=cu*mthday(im)
              irecw=((iy-iystr0)*12+(im-1))*numdivw+iw1+numtop

              read(42,rec=irecw,err=160) (dat1(i),i=1,nwel)
c             write(99,*) (dat1(i),i=1,nwel)
                     
              do i=1,nwel
                dat1(i)=dat1(i)*cx
                dat1t(i)=dat1t(i) + dat1(i)
              end do
c             write(99,*) (dat1(i),i=1,nwel)
c _________________________________________________________
c
c               Step 8; Set requested parameter
c
c             WRITE(99,*) '  datbinw; ip, cx, dat1(ip)',ip,cx,dat1(ip)
              dels(ifx,n) = dels(ifx,n) + dat1(ip)
c
c _________________________________________________________
c
c               Step 9; End Year and Month Loop      
  120       continue
  130     continue
          isy = isy+1
c
c _________________________________________________________
c
c               Step 10; End Well Loop
  140   continue    
c
c _________________________________________________________
c
c               Step 11; End Requested ID loop
  150 continue

      write(99,155) ifound2
      return
c
c _________________________________________________________
c
c               Error Messages

  155 format('   # of Wells               ', i5)
  160 write(6,*) '  Datbinr; Problem see the log file'
      write(99,*) '  Datbinr; Problem reading binary data'
      write(6,*) 'Stop 1'
      call flush(6)

      STOP 
      END           

cd ..