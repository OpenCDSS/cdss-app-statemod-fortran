c
      Subroutine GetRig
c
c		Read river gage (*.rig) file
c
c _________________________________________________________
c
c               Documentation
      
      include 'common.inc'
      character blank*12, rec12*12
c
c 		iout=1 details
c	  	iout=2 summary
      iout=0
      blank='            '
      nGage=0
      iin2=68
      
      write(6,10)
      write(nlog,10)
  10  format(/,72('_'),/,
     1 '  GetRig; River Gage File (*.rig) ')        
c      
c _________________________________________________________
c
c		Step 8; Read RiverGage Structure file (*.rig).
c          
c
      nGage=0
      if(iout.eq.1) write(nlog,*) '  GetRig; maxrun', maxrun
      DO 250 NP=1,maxrun+1
c          
c ---------------------------------------------------------
c 		c. Allow comments in the file       
        call comment(68, nlog, iocode, nchk, 0)     
        if(iocode.eq.2) goto 260
        if(iocode.eq.3) goto 928          
c
c ---------------------------------------------------------
c               d. Read *.rig RiverGage Structure data
        read(68,280,end=260,err=928)
     1    crunidG(np), runnamG(np)
     
        if(iout.eq.1) write(nlog,281) np,     
     1    crunidG(np), runnamG(np)
c
c ---------------------------------------------------------
c		e. Done reading if a blank ID
        if(crunidG(np).eq.blank) goto 260     
c
c ---------------------------------------------------------
c 		f. Adjust character string to left     
        crunidG(np)=adjustl(crunidG(np))

c
c ---------------------------------------------------------
c               Insure it's in the network, find ID, etc.
        rec12=crunidG(np)
        ifound=0
        do is=1,numsta
          if(cstaid(is).eq.rec12) then
            nGage=nGage+1
            irustaG(nGage)=is
            crunidG(nGage) = rec12
            runnamG(nGage) = runnamG(np)            
        
            if(iout.ge.1) then
              write(nlog,'(a8,2i4,1x,a12,1x,a12,i5)') '  GetRig;', 
     1        nGage, is, crunidG(nGage)
            endif          
            goto 250
          endif  
        end do
c            
c		Exit if not found  
        if(ifound.eq.0) goto 390
c
  250 CONTINUE
c      
c _________________________________________________________
  
  260 continue
      write(nlog,262) nGage
  262 format(/,72('_'),/    
     1  '  GetRig; Number of Stream Gages    = ', i5)
      return
c      
c _________________________________________________________
  280 FORMAT(a12, a24, a12, 1x, a12)
  281 FORMAT(i5,1x, a12, a24, a12, 1x, a12)
c      
c _________________________________________________________
  
  390 write(nlog,392) rec12
      write(6,392) rec12
  392 format(
     1  'GetRig; station ',a12,' of historic stream file (*.rih)',/
     1  '       or RiverGage Station file (*.rig)',/
     1  '       not found in the network file')
      goto 9999   
      
  928 write(nlog,929) iin2, filena
  929 format(' GetRig.f; Problem reading file # ', i4,/,
     1       '   File name: ', a256,/
     1       '   Problem record (next line):')
      goto 9999
c      
c _________________________________________________________
c      
 9999 write(6,*) '  Stopped in Virin, see the log file (*.log)'
      write(nlog,*)'  Stopped in Virin'
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)
      
      end
