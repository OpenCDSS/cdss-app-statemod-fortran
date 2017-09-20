c
      SUBROUTINE GetCall(iscd, imcd1, nx, ctype1)        
c
c
c _________________________________________________________
c	Program Description
c
c	GetCall; It estimates the call when a right is shorted
c _________________________________________________________
c	Documentation
c
c	iscd = diversion location
c	imcd1= calling location (0 if not equal to zero)
c	imcdX= indicator from a previous iteration
c	         0 calling right has never been set
c		>0 calling right has been set previously
c	cid1 = calling structure ID
c	ctype1=calling type (Diversion, Instream, Reervoir, etc.)
c	cname1=calling structure name
c
c _________________________________________________________
c	Dimensions
      include 'common.inc'
      dimension cname1(4)
      character ctype1*12, cid1*12, commentX*24
c
c		iout = 0 no details
c		       x details for diversion ID, instream ID,
c			  reservoir ID, ... = ioutnd      
      iout=0
      ioutnd=17
      imcd0=imcd1
      imcdX=imcd1
      small=0.001
      
      commentX='N/A'
c
c               b. Daily capability
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif
      
c
c _________________________________________________________
c		Get min flow downstream      
c
      NDNS=NDNNOD(ISCD)      
      CALL DNMFSO(maxsta,avail,IDNCOD,ISCD,NDNS,IMCD1)
c
c _________________________________________________________
c
c		Determine if avail is limiting       
      avail1=avail(imcd1)
      if(avail1.gt.small) then
        imcd1=0  
c
c rrb 2008/06/16; Correction
cx        imcdX=0    
        commentX='Avail is not limiting'
        goto 100
      endif  
c
c _________________________________________________________
c
c		Reset only first time non zero to keep senior call
      if(imcdX.lt.0) then
        imcd1=imcd1      
        commentX='Avail is limiting 00'
        goto 100
      endif
      
      if(imcdX.gt.0) then
        imcd1=imcdX      
        commentX='Avail is limiting 01'
        goto 100
      endif
c
c _________________________________________________________
c		Detailed output      
 100  continue
cx    if(iout.eq.1) then       
      if(iout.eq.1 .and. nx.eq.ioutND) then       
        ioutcX=ioutcX+1      
        if(ioutcX.eq.1) write(nlog,90)
      
        if(ctype1(1:3).eq.'Div') cid1=cdivid(nx)
        if(ctype1(1:3).eq.'Res') cid1=cresid(nx)
        if(ctype1(1:3).eq.'Isf') cid1=cifrid(nx)
c        
        if(imcd1.gt.0) then             
          write(nlog,110) 
     1    iyrmo(mon),xmonam(mon), idy, iwx, nx, imcd0, imcdX, imcd1,
     1    cid1, ctype1, cstaid(imcd1), avail1*fac, commentX 
        else        
          write(nlog,110) 
     1    iyrmo(mon),xmonam(mon), idy, iwx, nx, imcd0, imcdX, imcd1, 
     1    cid1, ctype1, 'N/A         ', avail1*fac, commentX
        endif
      endif
      
      return
  90  format(/
     1  '  GetCall     yr  mon    day  Iter    nx Imcd0 ImcdX Imcd1',
     1  ' Str ID      Type        Call ID        Avail ',
     1  'Comment',/
     1  ' ___________ ____ ____ _____ _____ _____ _____ _____ _____ ',      
     1  '___________ ___________ ___________ ________ ',
     1  '_______________________')
 110  format('  GetCall   ',i5,1x,a4, 6i6, 1x,
     1 a12, a12, a12, f8.1, 1x, a24)
      end
      
