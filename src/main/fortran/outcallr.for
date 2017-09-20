c
c
      SUBROUTINE OutCallR
      
c
c _________________________________________________________
c	Program Description
c
c
c	OutCallR; It prints unique call locations
c
c	imcd1= calling location (<=0 if none)
c	imcdP1= indicator =0 not printed yet, 1 already printed
c	cid1 = calling structure ID
c	ctype1=shorted structure type (Diversion, Instream, Res, etc.)
c	cname1=calling structure name
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
      character cstaidX*12, cstaNamX*24

c
c _________________________________________________________
c		ioutF = 1 print header once per simulation
c		ioutF = 0 Print header once per month      
      ioutF=1

      cstaidX='NA'
      cstaNamX='NA'
      
      do i=1,numsta
        idum(i)=0
      end do
      
      idy1=idy
      if(iday.eq.0) idy1=1
      
      if(ioutF.eq.0) then
        ioutcR=ioutcR+1            
        if(ioutcR.eq.1) write(53,100) 
      else
        if(iyr.eq.iystr .and. mon.eq.1) write(53,100)
      endif
      
c
c _________________________________________________________
            
      iprint=0
      je=0
      do is=1,numsta
        imcd1=imcdL(is)
c
c		Determine if there is a downstream call
c		Note if imcd1=is, the min flow is at the headgate        
cr      if(imcd1.gt.0) then
        if(imcd1.gt.0 .and. imcd1.ne.is) then
c
c		Determine if already printed
          ifound=0
          do j=1,je        
            if(idum(j).eq.imcd1) ifound=1
          enddo  
          
          if(ifound.eq.0) then
            iprint=iprint+1
            je=je+1
            idum(je)=imcd1
            write(53,110) 
     1      iyrmo(mon),xmonam(mon), idy1,imcdL(is), 
     1      cstaid(imcd1), ccallR(is),stanam1(imcd1)
          endif
        endif
      end do 
c
c _________________________________________________________
c		Print every month, even if no call
      if(iprint.eq.0) then
        write(53,110) 
     1  iyrmo(mon),xmonam(mon), idy1,-1, 
     1  cstaidX, -1., cstanamX
      endif
c
c _________________________________________________________
      
      return
c
c _________________________________________________________
 100  format(/
     1  ' Call Summary',/
     1  ' Note: The calling right is estiamted to be the most senior ',/
     1  '       water right at the calling location that is shorted.',/
     1  '       If none is reported (NA) it typically occurrs when a',/
     1  '       structure is diverting at a carrier location, not its',/
     1  '       physical location',//
     1  ' OutCallR    Year Mon   Day Imcd ',
     1  'Call Location  Call Right Call Location Name',/
     1  ' ___________ ____ ____ ____ ____ ',      
     1  '_____________  __________ ',24('_'))
 110  format(' OutCallR   ',i5,1x,a4, i5, i5,1x,
     1 a12, 1x, f12.4, 1x, a24)
      end
      
