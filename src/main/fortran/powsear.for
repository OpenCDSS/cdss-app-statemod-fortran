C
c *********************************************************
c
      subroutine PowSeaR(iw,l2,ncallX)
c
c
c _________________________________________________________
c	Program Description
c
c       PowseaR; It simulates a type 42 operating rule that
c               Resets a plan demand to zero
c
c _________________________________________________________
c	Documentation
c	
c	iopsou(1, ) < 0 Source 1 is a planc
c	npS > 0 	Source 1 or Source 2 plan pointer
c		
c _________________________________________________________
c       Update History
c
c rrb 2006/10/02; Copy PowSeaP and update accordingly
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
      character cwhy*48, cdestyp*12, ccarry*3, cpuse*3, cstaid1*12,
     1          rec12*12, cTandC*3
      
c
c _________________________________________________________
c
c		iout = 0 No detailed printout
c		     = 2 Summary printout
      iout=0
      if(ichk.eq.4) then
        write(nlog,*) ' PowseaR; Type 42'
        iout=1
      endif 
      
      if(ichk.eq.142) iout=2
      if(corid(l2).eq. ccall) ioutiw=iw
c
      divact = 0.0
      iw = iw
      
      small=0.001
c
c rrb 98/03/03; Daily capability
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif
      
      iwhy=0
      cwhy='NA'
      cdestyp='NA'
      ccarry='No'
      cpuse='No'
      cTandC='No'
      cstaid1='NA'
      
      npS=0

      ravcfs=-1./fac     
c
c _________________________________________________________
c
c rrb 02/10/25; Allow monthly on/off switch
      if(imonsw(l2,mon).eq.0) then
        iwhy=1
        cwhy='Monthly switch is off'
        goto 120
      endif  
c
c ---------------------------------------------------------
c		For a daily model set demand for beginning of season
      if(iday.eq.1 .and. imonsw(l2,mon).gt.0) then
        if (idy.lt.imonsw(l2,mon)) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 120
        endif  
      endif  
c
c ---------------------------------------------------------
c		For a daily model set demand for end of season
      if(iday.eq.1 .and. imonsw(l2,mon).lt.0) then
        if (idy.gt.iabs(imonsw(l2,mon))) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 120
        endif  
      endif  
      
c      
c _________________________________________________________
c
c		Step 2; Set Source Data
c               Find reservoir (nr), owner (iown), river location (iscd)
c               and # of downstream nodes (ndns)
      NR  =IOPSOU(1,L2)
      if(nr.lt.0) npS=-nr      
c
c ---------------------------------------------------------
c
c		2b; Exit if source 1 plan is off            
      IF(npS.gt.0 .and. pon(npS).LE.small) then
        iwhy=2
        cwhy='Source 1 Plan is off'                  
        Goto 120
      endif  
      
c _________________________________________________________
c
c		Step 3; Set Plan Source 1 Data
      if(npS.gt.0) then
        iscd=ipsta(npS)
        NDNS=NDNNOD(ISCD)        
        cstaid1=pid(npS)        
        
        psuply1=psto2(npS)/fac
        iplntyp1=iplntyp(npS)
        
        ravcfs=amax1(0.0, psuply1)
c
c ---------------------------------------------------------        
c        
c		3b; Exit if Plan Source 1 is zero
        IF(ravcfs.LE.small) then
          iwhy=3
          cwhy='Source 1 Plan Supply is zero'        
          Goto 120        
        endif  
        if(iout.eq.1) write(nlog,*) '  PowSeaR; nr, npS, ravcfs',
     1    nr, npS, ravcfs*fac
        
      endif  
c _________________________________________________________
c
c		Step 4; Set release
      divact=amax1(0.0, ravcfs)
c _________________________________________________________
c
c		Step 5; Reduce plan by amount released       
c
      if(npS.gt.0) then
        psto2(npS)=amax1(0.0, psto2(npS) - divact*fac)
        psuply(npS)=amax1(0.0, psuply(npS) - divact)
      endif  
C
c _________________________________________________________
c
c		Step 6; Update 
 120  divo(l2)=divo(l2)+divact
c _________________________________________________________
c
c		Step 7;  Detailed Output
      if(iout.eq.1 .or. (iout.le.2 .and. iw.eq.ioutiw)) then
        ncallX=ncallX+1
        if(ncallX.eq.1)then
          write(nlog,270) corid(l2), cdestyp, ccarry, cTandC, cpuse,
     1      cDivTyp(l2)
        endif  
        
        write(nlog,280) '  PowSeaR   ', iyrmo(mon),xmonam(mon), idy,
     1    cstaid1, iwx, iw, l2,nr, npS,          
     1    ravcfs*fac, divact*fac,  iwhy, cwhy

      endif
c _________________________________________________________
c
c		Step 8; Return
      RETURN
c
c _________________________________________________________
c
c               Formats
 270  format(/, 
     1  '  PowSeaR (Type 29); Plan Reset',/
     1  '          Operation Right ID = ', a12,
     1  ' Destination Type = ', a12,
     1  ' Carrier (Y/N) = ',a3, ' T&C Plan (Y/N) = ',a3,
     1  ' Reuse Plan (Y/N) = ', a3,
     1  ' Diversion Type = ', a12/    
     1  '  PowSeaR     iyr mon   day ID          ',
     1  ' Iter   Iw   l2   nr  npS', 
     1  '  RavCfs  DIVACT',
     1  '    iwhy Comment',/
     1  ' ___________ ____ ____ ____ ____________', 
     1  ' ____ ____ ____ ____ ____', 
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ __________________________')
      
  280   FORMAT(a12, i5,1x,a4, i5, 1x, a12, 5i5, 2f8.1,
     1   i8, 1x, a48)
c
c
c _________________________________________________________
c
c               Print warning
 9999 write(6,*) '  Stopped in PowSeaR, see the log file (*.log)'
      write(nlog,*) '  Stopped in PowSeaR'
      write(6,*) 'Stop 1' 
      call flush(6)
      call exit(1)

      stop 
      end
     
