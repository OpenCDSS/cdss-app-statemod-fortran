c
c
      SUBROUTINE OutCall(imcd1, nx, ctype1)        
      
c
c	GetCall; It estimates the call when a right is shorted
c
c	imcd1= calling location (<=0 if none)
c	imcdP1= indicator =0 not printed yet, 1 already printed
c	cid1 = calling structure ID
c	cdr1 = calling right
c	ctype1=shorted structure type (Diversion, Instream, Res, etc.)
c	cname1=calling structure name
c
c _________________________________________________________
c
      include 'common.inc'
      dimension cname1(4), imcdP(100)
      character ctype1*12, cid1*12, commentX*32, ccallID*12,
     1   ccallWR*12, cdru*4
c
c		iout = 0 Do not print if no call
c		       1 Print status for every structure
c		iouthg=0 Do not print if call is at structure headgate
c		       1 Print if call is at structure headgage
      iout=0      
      iouthg=0
      
      iouthg1=0
      ioutc=ioutc+1
      imcdX=imcd1
      small=0.001
      
      commentX='NA'
      cdr1=-1.0
c
c               b. Daily capability
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif
c _________________________________________________________
c
c		Determine if avail is limiting       
      avail1=avail(imcd1)
      if(imcd1.le.0) then
        commentX='Flow is not limiting'
      else  
        commentX='Downstream Flow is limiting'
      endif  
c
c _________________________________________________________
c		Detailed output      
      if(ioutc.eq.1) write(nlog,92)      
c
c _________________________________________________________
c		Set called structure ID      
      if(ctype1(1:3).eq.'Div') then
        cid1=cdivid(nx)
      endif  
        
      if(ctype1(1:3).eq.'Ins') then
        cid1=cifrid(nx)
      endif  
        
      if(ctype1(1:3).eq.'Res') then
        cid1=cresid(nx)
      endif  
c
c _________________________________________________________        
c
c		Set calling structure ID
c
c		istrtype = structure type (div, isf, res)
c		ccallID  = calling structure ID
c		cdr1     = calling right 
c		cdru     = calling right units
c
c		imcd1    = -1 no call
c                        = +n calling point on the river from
c                          the first time this structure gets
c                          shorted (from Getcall) 
      ccallID='NA'
      cdrT=0.0
      
      if(imcd1.gt.0 .and. imcdP(imcd1).eq.0) then                     
c
c		Check if already printed
        if(imcdP(imcd1).lt.0) then  
          imcdP(imcd1)=1    
c
        
          istrtyp1=istrtype(imcd1)
          RivPri  = qdiv(8,imcd1) + qdiv(14,imcd1) + 
     1              qdiv(5,imcd1) - qdiv(16,imcd1) 
c
c rrb 2010/09/15; Correction     
cdr        CarPri  = qdiv(16,is) + qdiv(19,is)
          CarPri  = qdiv(16,imcd1) + qdiv(19,imcd1)
          DivPri  = RivPri+CarPri
c
c		Calling right is a diversion, find structure and right        
          if(istrtyp1.le.5000) then
            nd=istrtyp1
            ccallID=cdivid(istrtyp1)
            cdru='cfs '          
            do k=1,numdvr
              if(idivco(1,k).eq.nd) then
                cdrT=cdrT+dcrdiv(k)
                if(cdrT.ge.DivPri) cdr1=dcrdiv(k)
              endif  
            end do  
          endif
c
c		Calling right is a ISF, find structure and right        
          if(istrtyp1.gt.5000 .and. istrtyp1.le.7500) then
            nf=istrtyp1-5000
            ccallID=cifrid(nf)
            cdru='cfs '          
            do k=1,numifr
              if(iifrco(k).eq.nf) then
                cdrT=cdrT+dcrdiv(k)
                if(cdrT.ge.DivPri) cdr1=dcrdiv(k)            
              endif  
            end do  
          endif
c
c		Calling right is a reservoir, find structure and right      
          if(istrtyp1.gt.7500) then
            nr=istrtyp1-7500
            ccallID=cresid(nr)
            cdru='acft'                    
            do k=1,numrsr
              if(iresco(1,k).eq.nr) then
                cdrT=cdrT+dcrdiv(k)
                if(cdrT.gt.DivPri) cdr1=dcrres(k)
              endif  
            end do  
          endif
c
c		
          if(ccallID.eq.cid1) then
            iouthg1=1
            commentX='Headgate Flow is limiting'
          endif  
c
          if(iouthg1.eq.iouthg) then
            write(nlog,112) 
     1      iyrmo(mon),xmonam(mon), idy, iwx, imcd1, 
     1      ccallID, cdr1, cdru
cr          write(nlog,110) 
cr   1      iyrmo(mon),xmonam(mon), idy, iwx, imcd1, 
cr   1      ctype1, cid1, ccallID, cdr1, cdru, avail1*fac, commentX 
          endif
c
c		Detailed printout for all structures
        else
          if(iout.eq.1) then
            write(nlog,112) 
     1      iyrmo(mon),xmonam(mon), idy, iwx, imcd1, 
     1      'NA          ', -1.0, cdru
cr          write(nlog,110) 
cr   1      iyrmo(mon),xmonam(mon), idy, iwx, imcd1, 
cr   1      ctype1, cid1, 'NA          ', -1.0, cdru, avail1*fac,
cr   1      commentX
          endif
        endif  
      endif  
      
      return
  90  format(/
     1  ' OutCall; Call Details',//
     1  ' Note: The calling right is estiamted to be the most senior ',/
     1  '       water right at the calling location that is shorted.',/
     1  '       If none is reported (NA) it typically occurrs when a',/
     1  '       structure is diverting at a carrier location, not its',/
     1  '       physical location.',/
  
     1  ' OutCall     iyr  mon   day Iter Imcd ',
     1  'Str Type     Str ID       Calling ID   Calling Rgt Unit',
     1  '    Avail Comment',/
     1  ' ___________ ____ ____ ____ ____ ____ ',      
     1  '___________  ___________  ___________  ___________ ____',
     1  ' ________ _______________________________')
 110  format(' OutCall    ',i5,1x,a4, i5, i5, i5,1x,
     1 a12, 1x,a12, 1x,a12, f12.4, 1x,a4, 1x,f8.1, 1x,a32)
  92  format(/
     1  ' OutCall     iyr  mon   day Iter Imcd ',
     1  'Calling ID   Calling Rgt Unit',/
     1  ' ___________ ____ ____ ____ ____ ____ ',      
     1  '___________  ___________ ____')
 112  format(' OutCall    ',i5,1x,a4, i5, i5, i5,1x,
     1 a12, f12.4, 1x,a4)
      end
      
