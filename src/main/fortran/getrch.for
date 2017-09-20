c
c *********************************************************
C
      SUBROUTINE GetRch(nlog, iget, maxsta, maxrch, 
     1  nreach, iRch, nRchTo, nRchEnd,
     1  RchId, RchIdR, RchNameR, cstaid)
c
c _________________________________________________________
c	Program Description
c
c       GetRch; Get Reach Data
c
c	 Called by Report.f and Virin.f
c
c _________________________________________________________
c	Documentation
c
c 	iget   = 1 Get River Reach Info
c	         = 2 Get River Data Info			
C
c	Itype           Type used by GetID 
c                       -3=Diversion Only
c                       6=Well
c
c	RchidDr		Diversion Reach ID
c	RchidDd		Diversion Reach
c _________________________________________________________
c	Dimensions
c
      dimension 
     1 irch(maxsta), Rchid(maxsta), cstaid(maxsta),
     1 RchidR(maxrch), RchNameR(maxrch),
     1 nRchTo(maxrch), nRchEnd(maxrch), RchTo(maxrch) 
     
      character rec1*1, rec12*12,  recID*12, rec24*24, rec132*132,
     1 Staid*12, Staname*24, RchID*12, RchidX*12, cstaid*12,
     1 RchNameX*24, RchidR*12, RchNameR*24,
     1 RchTo*12
     
c
c
c _________________________________________________________
c		Initilize
c
c				iout =1 details
c				iout =2 summary
c				iout =3 super summary
      iout=3
      
      write(6,10) iget
      write(nlog,10) iget
  10  format(/,72('_'),/,
     1 '  GetRch; River Reach File (*.rch); iget = ', i5)             
 
c
c _________________________________________________________
c					Reach River Reach data (iget = 1)
      if(iget.eq.1) then
c
c _________________________________________________________
c		  	Initilize River Reach data
        do i=1,maxsta
          irch(i)=1
          RchID(i)='Reach_01    '
        end do
        
        do i=1,maxrch
          nRchTo(i)=0
          nRchEnd(i)=0
          RchIDR(i)='NA'
          RchNameR(i)='NA'
          RchTo(i)='END'
        end do
c
c _________________________________________________________
c		       Read reach data
        rec1=' '
        rec12=' '
        ng=0
        do i=1,maxrch
 100      read(55,*,end=110, err=110) rec1
          if(iout.eq.1) write(nlog,*) rec1
          if(rec1.eq.'#') goto 100
          
          backspace(55)
          read(55,*) rec12
          if(iout.eq.1) write(nlog,*) rec12
cx          if(rec12.eq.'Reach_Data  ') goto 100
cx          if(rec12.eq.'Node_Data   ') goto 110
          
          backspace(55)
          ng=ng+1
          read(55,*)
     1    RchidR(ng), RchNameR(ng), RchTo(ng), rec24, StaiD
     
          call getid(4, is1, ir1, ir2, StaID)      
          nRchEnd(ng)=is1
               
          if(iout.eq.1) write(nlog,*) ng, RchidR(ng), RchNameR(ng), 
     1      RchTo(ng), rec24, StaID,nRchEnd(ng)
        end do
c
c		Set the number of gages to be ng-1 since the last one
c		read was the end      
 110    nreach=ng      
        write(nlog,111) nreach
 111    format(/,
     1    '  GetRch; Number of reaches (*.rch) = ', i5)      
c
c _________________________________________________________
c
c		Set connectivity data

        if(iout.eq.3) write(nlog,210)
        
        do ng=1,nreach
          RchidX=RchTo(ng)
          do i=1,nreach+1
            if(rchidX.eq.RchidR(i)) then
              nRchTo(ng)=i
            end if  
          end do
          
          if(iout.eq.3) write(nlog,211) ng,
     1      RchidR(ng), RchNameR(ng), RchTo(ng), 
     1      nRchEnd(ng), nRchTo(ng)
          
        end do
         
        
        if(iout.eq.1 .or. iout.eq.2) write(nlog,220)
c
c _________________________________________________________
c 		Done reading River Reach Info Exit via goto 200       
        goto 200   
      endif
      
c
c _________________________________________________________
c
c		Get Node Data
      if(iget.eq.2) then
        if(iout.eq.1) then
          write(nlog,*) ' GetRch; iget = 2'
        endif
        
        is=0
        nreach=0
 120    read(55,*,end=190,err=190) rec1
        if(iout.eq.1) write(nlog,*) 'GetRch; ', rec1
        if(rec1.eq.'#') goto 120
        backspace(55)
        
        if(iout.eq.1) then
          read(55,'(a132)',end=190,err=190) rec132
          write(nlog,*) rec132
          backspace(55)
        endif
        
        is=is+1
        read(55,*,end=200,err=200) StaID, RchNameX, iRchX, RchIdX
        if(iout.eq.1) write(nlog,130) StaID, RchNameX, iRchX, RchIdX
c
c ---------------------------------------------------------
c		    Get stream Id; irregardless of how data is printed      
        call getid(4, is1, ir1, ir2, StaID) 
             
        if(is1.eq. 0)  goto 520
      
c
c ---------------------------------------------------------
c		      Set reach ID and name for every stream node       
        irch(is1)=irchX
        Rchid(is1)=RchidX
c       RchName(is1)=RchNameX
        nreach=amax0(nreach,iRchX)
c
c ---------------------------------------------------------
c		      Set reach id and name for every reach 
c		      Based on the first entry found     
        rec12=RchidR(irchX)
        if(rec12(1:2).eq.'NA') then
          RchidR(irchX)=RchidX
          RchNameR(irchX)=RchNameX      
        endif
        
        if(iout.eq.1 .or. iout.eq.2) write(nlog,230) is1, cstaid(is1), 
     1    irch(is1), Rchid(is1)
c
c ---------------------------------------------------------
c		Get next reach value
        goto 120
      endif
      
 190  write(nlog,192) is
 192    format(/,
     1    '  GetRch; Number of nodes (is) = ', i5)      

 200  if(nreach.gt.maxrch) goto 510
      close(55)
c          
c _________________________________________________________
c 
      return 
c
c _________________________________________________________
c
c		Formats 
 130  format(a12, 1x,a24, 1x,i8, 1x,a12)
 210  format(
     1 '    # Reach ID     Reach Name               ',
     1 'ReachTo       nrchend   rchto',/
     1 ' ____ ____________ ________________________ ',
     1 '____________ ________ _______')
    
 211  format(i5, 1x, a12, 1x, a24, 1x, a12, 1x, 2i8)
 
 220  format(/,72('_'),/
     1  '  GetRch; Reach Report ',//
     1  '    # ID              n Reach ID',/
     1  ' ____ ____________ ____ ____________')
 230  format(i5, 1x,a12, i5, 1x,  a12)
c
c _________________________________________________________
c
c               Error Tracking

 510  write(nlog,512) nreach, maxrch
 512  format(
     1 ' GetRch; Problem the number of reaches = ', i5,/
     1 '         Exceeds the maximum = ', i5,/
     1 '         Reconmend you revise the reach data (*.rch)')
      goto 900
 
 520  write(nlog,522) staid
 522  format(
     1 ' GetRch; Problem reach ID    = ', a12,/
     1 '         Cannot be found in the river network file (*.rin)',/
     1 '         Reconmend you revise the reach data (*.rch) or',/
     1 '         the river network (*.rin) file')
      goto 900
 900  write(6,910) 
      write(nlog,920) 
      call flush(6)

 910  format('  Stopped in GetRch, see the log file (*.log)')
 920  format(/72('_'),/,'  GetRch; Stopped in GetRch')
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)
     
      stop     
      end
