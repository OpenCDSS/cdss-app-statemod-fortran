c
c
      SUBROUTINE OutCallS(imcd1, nx, ccallR1, ctype1, ccallID)        
      
c
c _________________________________________________________
c	Program Description
c
c	GetCallS; It estimates the calling water right (ccallR1)
c                 when a structure is shorted. Note the shortage
c                 is controlled at river location imcd1
c
c
c _________________________________________________________
c	Documentation
c
c	imcd1 = calling location on the river(<=0 if none)
c       nx    = shorted structure pointer (div, isf, or res)
c	ccallR1=calling right amount
c	ctype1= shorted structure type (Diversion, Instream, Res, etc.)
c	ccallID=calling structure ID
c
c	cid1 =  shorted structure ID 
c	istrtype = structure type (div, isf, res)
c	cdru     = calling right units
c
c _________________________________________________________
c	Dimensions
      include 'common.inc'
      character ctype1*12, cid1*12, commentX*32, ccallID*12,
     1   cdru*4
c
c _________________________________________________________
c		Step 1; Initilze
c
c
c		iout = 0 Do not print details
c		       1 Print details
      iout=0
      
      small=0.001
c
c               b. Daily capability
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif
      
      commentX='NA '
      ccallID='NA '
      cdru='NA  '          
      cid1='NA'
      
      ccallR1=-1.0
      avail1=-1.0/fac
      cdrT=0.0      
      istrtyp1=0
      ioutCS=0
      
      if(iout.eq.1) then
        write(nlog,*) '  OutCallS; imcd1, nx, ctype1'
        write(nlog,*) imcd1, nx, ctype1   
      endif
      
c _________________________________________________________
c
c		Determine if avail is limiting    
      
      if(imcd1.gt.0) avail1=avail(imcd1)
      if(imcd1.le.0) then
        commentX='Flow is not limiting'
      else  
        commentX='Downstream Flow is limiting'
      endif  
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
        
      if(ctype1(1:3).eq.'Wel') then
        cid1=cdividw(nx)
      endif 
       
c
c _________________________________________________________        
c
c		Set calling structure ID
c
      
        if(imcd1.gt.0) then                     
c
          cdrT=0
          istrtyp1=istrtype(imcd1)
          RivPri  = qdiv(8,imcd1)  + qdiv(14,imcd1) + 
     1              qdiv(5,imcd1)  - qdiv(16,imcd1) 
          CarPri  = qdiv(16,imcd1) + qdiv(19,imcd1)
          DivPri  = RivPri+CarPri
c
c _________________________________________________________
c		Calling right is a diversion, find structure and right        
          if(istrtyp1.gt.0 .and. istrtyp1.le.5000) then
            nd=istrtyp1
            cdru='cfs '          
            do k=1,numdvr
              if(idivco(1,k).eq.nd) then
                cdrT=cdrT+dcrdiv(k)
                if(cdrT.ge.DivPri) then
                  ccallR1=dcrdiv(k)
                  ccallID=cdivid(nd)
                endif  
              endif  
            end do  
          endif
c
c _________________________________________________________
c		Calling right is a ISF, find structure and right        
          if(istrtyp1.gt.5000 .and. istrtyp1.le.7500) then
            nf=istrtyp1-5000
            cdru='cfs '          
            do k=1,numifr
              if(iifrco(k).eq.nf) then
                cdrT=cdrT+dcrdiv(k)
                if(cdrT.ge.DivPri) then
                  ccallR1=dcrdiv(k)            
                  ccallID=cifrid(nf)
                endif                  
              endif  
            end do  
          endif
c
c _________________________________________________________
c		Calling right is a reservoir, find structure and right      
          if(istrtyp1.gt.7500 .and. istrtyp1.le.10000) then
            nr=istrtyp1-7500
            cdru='acft'                    
            do k=1,numrsr
              if(iresco(1,k).eq.nr) then
                cdrT=cdrT+dcrdiv(k)
                if(cdrT.gt.DivPri) then
                  ccallR1=dcrres(k)
                  ccallID=cresid(nr)
                endif
              endif  
            end do  
          endif
c
c _________________________________________________________
c		Calling right is a Well
c rrb 2007/02/23; Add well
          if(istrtyp1.gt.12500 .and. istrtyp1.le.15000) then
            nw=istrtyp1-12500
            cdru='acft'                    
            do k=1,numdvrw
              if(idivcow(1,k).eq.nw) then
                cdrT=cdrT+dcrdivw(k)
                if(cdrT.gt.DivPri) then
                  ccallR1=dcrdivw(k)
                  ccallID=cdividw(nr)
                endif
              endif  
            end do  
          endif
          
c
c _________________________________________________________
c		Identify if the min flow is at the structure headgate
c		
          if(ccallID.eq.cid1) then
            commentX='Headgate Flow is limiting'
          endif  
c
c		Detailed printout for structures with a call
          if(iout.eq.1) then
            if(ioutcS.eq.0) write(nlog,90)                
            ioutcS=ioutcS+1
            
            write(nlog,110) 
     1      iyrmo(mon),xmonam(mon), idy-1, iwx, nx, imcd1, 
     1      ctype1, cid1, ccallID, ccallR1, cdru, avail1*fac, commentX 
          endif
c
c		Detailed printout for structures w/o a call
        else
          if(iout.eq.1) then
            if(ioutcS.eq.0) write(nlog,90)                
            ioutcS=ioutcS+1

            write(nlog,110) 
     1      iyrmo(mon),xmonam(mon), idy-1, iwx, nx, imcd1, 
     1      ctype1, cid1, 'NA          ', -1.0, cdru, avail1*fac,
     1      commentX
          endif
        endif  

 500  return
  90  format(/
     1  ' OutCallS    iyr  mon   day Iter   nx Imcd ',
     1  'Str Type     Str ID       Calling ID   Calling Rgt Unit',
     1  '    Avail Comment',/
     1  ' ___________ ____ ____ ____ ____ ____ ____ ',      
     1  '___________  ___________  ___________  ___________ ____',
     1  ' ________ _______________________________')
 110  format(' OutCallS   ',i5,1x,a4, i5, i5, i5,i5, 1x,
     1 a12, a12, 2x,a12, f12.4, 1x,a4, 1x,f8.1, 1x,a32)
  92  format(/
     1  ' OutCallS    iyr  mon   day Iter   nx Imcd ',
     1  'Calling ID   Calling Rgt Unit',/
     1  ' ___________ ____ ____ ____ ____ ____ ____ ',      
     1  '___________  ___________ ____')
 112  format(' OutCallS   ',i5,1x,a4, i5, i5, i5,i5,1x,
     1 a12, f12.4, 1x,a4)
      end
      
