c
        subroutine average(maxsta,iopt,iopt2, im,iy,nyr1,
     1                     f,c1,c2,c3,c4,c5,ave, avet, 
     1                     iz, maxrch, maxyrs, ave3D) 
c
c _________________________________________________________
c	Program Description
c
c               It keeps a running total for 5 elements in
c               an array ave       
c
c
c _________________________________________________________
c	Update History
c
c rrb 99/12/27; Revised to process if some data is missing
c rrb 00/11/27; Revised to include basin total (avet)
c rrb 2006/12/22; Revised to include reach (zone) balance 
c               Ave3D(iz,iy,i)
c		where iz=# of zones, iy=year, i=element
c
c _________________________________________________________
c	Documentation
c
c
c               iopt = 0 initilize 
c                      1 total
c                      -1 Basin Total
c               iopt2  0 do not calculate basin total (avet)
c                      1 do calcualte basin total (avet)
c                      Note the above is required since streamflow
c                      and diversions are processed simotaneously
c               im   = for iopt = 0,  array size (numres, numdiv, etc.)
c                      for iopt = 1,  month
c
c               iy   = year to begin storing the annual total in
c                      array elements 6...n
c               nyr1 = counter for missing data by year
c               f    = factor to convert data 
c               c1...= data to total
c               ave  = Ditch results 
c                      ave(1-13, 1-4) = ditch averages by month for elements 1-4
c                      ave(1-4, 5-ny+5) ditch totals by year
c               avet = Basin results (similar to above)
c               iz   = Reach #
c               maxrch=dimension of reaches
c               maxyrs=dimension of years
c               ave3d(iz,iy,i) = total for zone iz, year iy, month i
c                           
c

c
c _________________________________________________________
c	Dimensions
c
c
        dimension ave(13,maxsta), avet(13,maxsta), c(5),
     1  ave3d(maxrch,maxyrs,13)                 

c       ldim=2500
c _________________________________________________________
c                           
c               Step 1; Check for local dimensions
c       if(maxsta.gt.ldim) then
c         write(99,*) '  Average: Local dimension exceeded '
c         write(99,*) '           local, maxsta ', ldim, maxsta
c         goto 130
c       endif
c       write(99,*) '  Average:', maxsta, iopt, iopt2

c _________________________________________________________
c 
c               Step 2; Initilize basin total data
        if(iopt.eq.-1) then
          do i=1,13
            do j=1,maxsta
              avet(i,j) = 0.0 
            end do
          end do
c
c		Step 2b; Initilize ave3d           
c rrb 2006/12/22; Add average 3D (zone, year, element)
          do i=1,13
            do j=1,maxyrs
c
c rrb 20100208; Correction            
cx            do iz=1,maxrch
              do k=1,maxrch              
                ave3d(k,j,i)=0.0
              end do  
            end do
          end do
          goto 500
        endif
c _________________________________________________________
c 
c               Step 3; Initilize structure data
        if(iopt.eq.0) then
          do i=1,13
            do j=1,maxsta
              ave(i,j) = 0.0 
            end do
          end do
          goto 500
        endif
c
c _________________________________________________________
c               Step 4; Process Data
c       if(iopt.eq.1) then
          n2 = iy+5
          if(n2.gt.maxsta) then
            write(99,*) '  Average: Local dimension exceeded '
            write(99,*) '           local, n2 ', 850, n2
            goto 130
          endif

          c(1) = c1*f
          c(2) = c2*f
          c(3) = c3*f
          c(4) = c4*f
          c(5) = c5*f
c
c _________________________________________________________
c               Step 4a; Annual totals
c rrb 99/12/27; set annual totals to -999 if any data is missing
          if(im.eq.12) then
            if(nyr1.lt.12) then
              ave(1,n2) = -999.0
              ave(2,n2) = -999.0
              ave(3,n2) = -999.0
              ave(4,n2) = -999.0
c
c rrb 00/11/29; Calculate Basin Total
              if(iopt2.eq.1) then
                avet(1,n2) = -999.0
                avet(2,n2) = -999.0
                avet(3,n2) = -999.0
                avet(4,n2) = -999.0
c
c		3D (Reach) Total 
c rrb 20100208; Correction
                if(iz.gt.0) then               
                  ave3d(iz,iy,1)=-999.0
                  ave3d(iz,iy,2)=-999.0
                  ave3d(iz,iy,3)=-999.0
                  ave3d(iz,iy,4)=-999.0     
                endif           
              endif
            endif
            nyr1=0
          endif
c               
          do 110 i=1,5
c
c rrb 99/06/23; Check for -999
c rrb 99/12/27; Do not count (even simulated) if data is missing
            if(abs(c(1)+999.0) .lt. 0.01) then
            else
c               
c _________________________________________________________
c               Step 4b; Total for average monthly
c
              if(abs(ave(im,i)+999.0).gt.0.01) then
                ave(im,i)  = ave(im,i)  + c(i)
                if(iopt2.eq.1) avet(im,i) = avet(im,i) + c(i)
              endif
c               
c _________________________________________________________
c               Step 4c; Total for annual total
c
              if(abs(ave(i,n2)+999.0).gt.0.01) then
                ave(i,n2)  = ave(i,n2) + c(i)
                if(iopt2.eq.1) then
                  avet(i,n2) = avet(i,n2) + c(i)
c
c		Reach total 
c rrb 20100208; Correction
                  if(iz.gt.0) then                  
                    ave3d(iz,iy,i)=ave3D(iz,iy,i) + c(i)
                  endif
                endif
              endif
            endif
 110      continue

c     write(99,*)' Average; ',iz, iy, (avet(i,n2), i=1,5)
c     write(99,*)' Average; ',iz, iy, (ave3D(iz,iy,i), i=1,5)
c _________________________________________________________
c
c		Return
  500  return



  130 write(6,*)  '  Stopped in Average, see the log file (*.log)'
      write(99,*) '  Stopped in Average'
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)
      stop 
      END

