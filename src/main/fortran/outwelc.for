C
c *********************************************************

      subroutine outwelc(cplot,nreach)
c
c
c _________________________________________________________
c	Program Description
c
c       Outwelc; It prints a table of well pumping
c                Historic Vs Calculated
c
c _________________________________________________________
c       Documentation
c
c              cplot = n diversion, instream or gage ID to plot
c              cplot = '           ' prints mon, ann, ave & basin data
c              cplot = 'Annual     ' prints annual,ave & basin data
c              cplot = 'BasinTotal' prints basin data only
c              now   = output file number
c              idum2 = pointer to well ID
c              ndivw = # of output values in the binary file
c              iresop= 1=cfs, 2=acft, 3=Kaf
c              diverm() = historic pumping
c              dat1()   = simulaetd well data
c              dum(1-13,1-4) Average well data for month 1-13 for
c                            elements 1-4
c              dum(1-4, 5-5+ny) Annual well data for elements 1-4
c                            for years 5 - 5+ny     
c
c _________________________________________________________
c	Dimensions
c 
      include 'common.inc'
      dimension nmo(13), ihis(1)
      character blank*12, cplot*12, cistat*12, recin*256,
     1 rec12*12
c
c _________________________________________________________
c	Initilize
c
      iout=1
c      
c     write(6,*) ' '
      write(6,*) ' Subroutine Outwelc'
      write(6,*) ' '
      write(99,*) ' Outwelc; cplot = ', cplot, 
     1  ' # of reaches = ',nreach
c             
c               Step 1; Initilize
c _________________________________________________________
c
c
c
c-------------------------------------------------------------------
c		Open and Print header to Report File
      if(cplot.eq.'Report      ') then
        call namext(maxfn, filenc, 'Ywc', filena) 
        open(116,file=filena,status='unknown')
        call outtop(116,1,58)        
        write(116,374)        
      endif  
c
c               nsup = column of binary data with total pumping
      nsup = 3
c
c		now  = output file #      
      now  = 9
c     ndiv = 23
c     ndivw=18
c
c		Set number of output values 1x in StateM.for
c	        and store in common.inc      
      ndiv = ndivO        
      nres = nresO
      ndivw= nwelO
      
      numdivh=0
c     numhis=0
      blank = '            '

      d1=0.0
      d2=0.0
      d3=0.0
      d4=0.0
      d5=0.0
c
      do iw=1,numdivw
        idum2(iw) = 0
      end do
c
c     cu=1.0
c     if(iresop.eq.2) cu=factor
c     if(iresop.eq.3) cu=factor*0.001                        
c
c _________________________________________________________
c
c               Step 2; Process & Store Historic Pumping in Temp File
c
      if(numdivw.eq.0) then
        Write(io99,*) ' No Active Wells'
        Goto 500
      endif
      
      is=0
      iin2=4
      filena='*.weh'
  100 read (3,390,end=140,err=928) idyr,cistat,(diverm(im),im=1,12)
c
c rrb 2006/01/20; Well data should never be negative
      do im=1,12
        diverm(im)=amax1(diverm(im),0.0)
      end do
        
      is=is+1
      write(1,rec=is) idyr, (diverm(im),im=1,12)

      if(numdivh.eq.0) idyr1 = idyr

      if(idyr1 .ne. idyr) then
        goto 100
      else
        numdivh = numdivh + 1
c
c               Find corresponding id
        do n=1,numdivw
          if(cistat.eq.cdividw(n)) then
            idum2(n) = numdivh
            if(iout.eq.1) then
              write(nlog,*) ' OutwelC; ',n, cistat, idum2(n)
            endif  
            goto 100
          endif
        end do
        write(io99,430) cistat,idyr
        go to 100
      endif
c
c
  140 continue
c 
c               Initilize Total Data
      call average(maxsta,-1,1,im,iy1,nyr1,
     1               1.0,d1,d2,d3,d4,d5,dum,dum2,
     1             iz, maxrch, maxyrs, dum3D)
c
c _________________________________________________________
c
      do 230 is=1,numdivw
c
c ---------------------------------------------------------
c rrb 2007/01/24; Set Reach ID (RchidWR)
c rrb 2008/12/24; revise reach approach
cx        iz=nreach+1
cx        do ix=1,nreach
cx          if(RchidWR(ix).eq. RchidW(is)) then
cx            iz=ix
cx            if(iout.eq.1) then
cx              write(nlog,*) ' OutwelC; ', 
cx     1          cdividw(is), is, iz, RchidWR(iz) 
cx            endif  
cx          endif  
cx        end do             
        iss=idvstaw(is)
        iz=irch(iss)
      
c
c               Step 3; Print Title
c _________________________________________________________
c 
c               Initilize structure data
c       call average(maxsta,0,1,numres,0,nyr1,
        call average(maxsta,0,1,im,iy1,nyr1,
     1               1.0,d1,d2,d3,d4,d5,dum,dum2,
     1             iz, maxrch, maxyrs, dum3D)
c
c               Print to Screen
        c = float(is)/float(numdivw)*100.0
        write(6,150) cunitm, is, numdivw, c
        call flush(6)
c
c              Get station ID (assumes one per station)
        irecs=numdivw+is+numtop
        read(42,rec=irecs,err=450) (dat1(i),i=1,ndivw)
        
        if(iout.eq.1) then
c         write(io99,*) (dat1(i), i=1,ndivw)
          write(io99,*) '   OutdivC;  is, idum2(is) ', is, idum2(is)
        endif
c
c              Print to gauge file if no diversions at this station
c ----------   Gage data
        if(idum2(is).eq.0) goto 230
c            
c rrb 00/11/27; Limit output to basin total      
        if(cplot.eq.'BasinTotal  ' .or. 
     1     cplot.eq.'Annual      ') then
        else
c
c               Print title
          write(now,310)
        endif
c
c              Print water right info for Total only
        maxwrx=maxwr
c        
c rrb 2007/01/17; Print summary of rights only              
c        
c       if(cplot.eq.blank) call outwr(maxwrx,1, is, is, now)
        if(cplot.eq.blank) call outwr2(maxwrx,1, is, is, now)
c 
c rrb 99/12/28; Missing data
        do im=1,13
          nmo(im)=0
        end do
        nyr1 = 0
        iy1  = 0                     
        
        do 180 iy=iystr,iyend
          iy1 = iy1+1
          call year(iy, iyrmo, imomo, cyr1)
c
c               Read historic well pumping from temp file
          irecd=(iy-iystr0)*numdivh+idum2(is)

          read(1,rec=irecd,err=460) idyr, (diverm(im), im=1,12)
          if(iout.eq.1) then
            write(io99,*) '  Outwelc; Historic ', (diverm(im), im=1,12)
          endif
c
c               Read simulated well data
          do 180 im=1,12
c
            irecs=((iy-iystr0)*12+(im-1))*numdivw+is+numtop
            read(42,rec=irecs,err=460) (dat1(i),i=1,ndivw)
            if(iout.eq.1) then
              write(io99,*) '  OutwelC; Est. ',(dat1(i),i=1,ndivw)
            endif

c           cx = cu         
c           if(iresop.ne.1) cx=cu*mthday(im)
           
            do i=1,ndivw
c             dat1(i)=dat1(i)*cx
              dat1(i)=dat1(i)*fmo(im)
            end do
c
c rrb 99/12/28; Missing data
            if(abs(diverm(im)+999.0) .lt. 0.01) then
              c = -999.0
              cp = -999.0
              nmo1=0
            else
              c = diverm(im)-dat1(nsup)
              cp = 0.
              if(diverm(im).gt.0.001) cp = (c/diverm(im))*100.0
              nmo1=1
              nyr1=nyr1+1
              nmo(im)=nmo(im)+1
            endif
c _________________________________________________________
c 
c               Print structure monthly data
c            
c rrb 00/11/27; Limit output to basin total      
            if(cplot.eq.'BasinTotal  ' .or. 
     1         cplot.eq.'Annual      ') then
            else
                write(now,350)   cdividw(is), iyrmo(im), xmonam(im), 
     1                           diverm(im), dat1(nsup), c, cp,
     1                           divnamw1(is), iz, nmo1
            endif
c
c               Total for average monthly, average annual and
c                         annual total

            call average(maxsta, 1, 1, im, iy1, nyr1,  
     1                   1.0,diverm(im),dat1(nsup),c,d4,d5,dum,dum2,
     1             iz, maxrch, maxyrs, dum3D)

c
c        End Year and Month Loop      
  180   continue
        
c 
c _________________________________________________________
c               Print structure annual totals
c rrb 00/11/27; Limit output to basin total      
          if(cplot.eq.'BasinTotal  ') then
          else 
c           write(now,*) ' '
c           write(now,*) ' Total by Year'
c
c               Print title
            write(now,310)

          endif

          iy5 = 5
          do iy=iystr,iyend
            iy5 = iy5+1

            dum(4,iy5) = 0.0
            if(dum(1, iy5) .gt. 0.001) then
              dum(4, iy5) = dum(3, iy5) / dum(1,iy5)*100.0
            endif

            nyr2=1
            if(abs(dum(1,iy5) + 999.0) .lt. 0.01) nyr2=0
c
c               Print standard ASCII output
c rrb 00/11/27; Limit output to basin total      
            if(cplot.eq.'BasinTotal  ') then
            else
                write(now,350)   cdividw(is),  iy, xmonam(13),
     1                          (dum(j,iy5), j=1,4),
     1                          divnamw1(is), iz, nyr2
            endif
          end do
c _________________________________________________________
c
c               Calculate Annual average
          iy5=5
          do iy=iystr, iyend
            iy5=iy5+1
            if(abs(dum(1,iy5)+999.0) .gt. 0.01) then
              nmo(13) = nmo(13) + 1
              dum(13,1) = dum(13,1) + dum(1,iy5)
              dum(13,2) = dum(13,2) + dum(2,iy5)
              dum(13,3) = dum(13,3) + dum(3,iy5)
c
c               Basin total

              dum2(13,1) = dum2(13,1) + dum(1,iy5)
              dum2(13,2) = dum2(13,2) + dum(2,iy5)
              dum2(13,3) = dum2(13,3) + dum(3,iy5)

            endif
          end do
c
c               Print Structure averages (month and year) 
c _________________________________________________________
c            
c rrb 00/11/27; Limit output to basin total      
          if(cplot.eq.'BasinTotal  ') then 
          else 
c           write(now,*) ' '
c           write(now,*) ' Average by Month and Year'
c
c               Print title
            write(now,310)
          endif

          ry = (iyend-iystr+1)
          if(ry.lt.0.01) goto 230

          do im=1,13                                    
c            
c rrb 00/11/27; Limit output to basin total      
            if(cplot.eq.'BasinTotal  ') then 
            else 
              if(im.eq.13) write(now,*) ' '           
            endif
c                         
c rrb 99/12/28; Missing data
            ryx = nmo(im)
            if(nmo(im).eq.0) then
              ryx = 1.0
              dum(im,1) = -999.0
              dum(im,2) = -999.0
              dum(im,3) = -999.0
            endif

            dum(im,4) = 0.0
            if(dum(im,1) .gt. 0.001) then
              dum(im,4) = dum(im,3) / dum(im,1)*100.0*ry
            endif
c            
c rrb 00/11/27; Limit output to basin total      
            if(cplot.eq.'BasinTotal  ') then 
            else
c 
c               Print average output for diversions
              write(now,370)   cdividw(is), xmonam(im), 
     1                        (dum(im,j)/ryx, j=1,4),
     1                        divnamw1(is), iz, nmo(im)
            endif
          end do
c       
c
c        End Well Station Loop
c-------------------------------------------------------------------

  230 continue
c _________________________________________________________   
c 
c               Step 11; Print annual totals for a Reach Data
          ry = (iyend-iystr+1)
          nyr3=ry
c
c		Reach Loop
          do iz=1,nreach
c
c ---------------------------------------------------------
c               Print title
            write(now,310)
            
            do j=1,4
              dum3(iz,j)=0.0
            end do            
            iy1=0
c            
c ---------------------------------------------------------
c		Year Loop
            do iy=iystr,iyend
              iy1=iy1+1
              dum3D(iz,iy1,4) = 0.0
              if(dum3D(iz,iy1,1) .gt. 0.001) then
                dum3D(iz,iy1,4) = dum3D(iz,iy1,3)/dum3D(iz,iy1,1)*100.0
              endif

              nyr2=1
              if(abs(dum3D(iz,iy1,1) + 999.0) .lt. 0.01) nyr2=0
c
c ---------------------------------------------------------
c               Print standard ASCII output
                write(now,351)  iz, iy, xmonam(13),
     1                        (dum3D(iz,iy1,j), j=1,4), iz, nyr2
c
c		Calculate average          
                do j=1,4
                  dum3(iz,j)=dum3(iz,j) + dum3D(iz,iy1,j)/ry
                end do  
            end do
c
c ---------------------------------------------------------
c		Print average per Reach (zone)            
            write(now,373)           
            dum3(iz,4) = 0.0
            if(dum3(iz,1) .gt. 0.001) then
              dum3(iz,4) = dum3(iz,3)/dum3(iz,1)*100.0
            endif
            write(now,371)  iz, xmonam(13),
     1                      (dum3(iz,j), j=1,4), iz, nmo(13)
c
c ---------------------------------------------------------
c		Print average per Reach to Report 
            if(cplot.eq.'Report      ') then
cx            write(116,375) RchidWR(iz),(dum3(iz,j), j=1,4),RchidWR(iz)
              write(116,375) Rchid(iz),(dum3(iz,j), j=1,4),Rchid(iz)
            endif
     
          end do  
c _________________________________________________________   
c 
c               Print Basin annual totals
c
c               Print title
          write(now,310)
c         write(now,*) ' '
c         write(now,*) ' Basin Total by Year'

          iy5 = 5
          do iy=iystr,iyend
            iy5 = iy5+1

            dum2(4,iy5) = 0.0
            if(dum2(1, iy5) .gt. 0.001) then
              dum2(4, iy5) = dum2(3, iy5) / dum2(1,iy5)*100.0
            endif

            nyr2=1
            if(abs(dum2(1,iy5) + 999.0) .lt. 0.01) nyr2=0
c
c               Print standard ASCII output
              write(now,352)  iy, xmonam(13),
     1                        (dum2(j,iy5), j=1,4), -1, nyr2
          end do

c
c _________________________________________________________   
c               Print Total Basin Averages (monthly and annual)

c         write(now,*) ' '
c         write(now,*) ' Basin Total by Month and Year'
c
c               Print title
          write(now,310) 

          ry = (iyend-iystr+1)
          if(ry.lt.0.01) goto 500

          do im=1,13                                    
            if(im.eq.13) write(now,*) ' '           
c                         
c rrb 99/12/28; Missing data
            ryx = nmo(im)
            if(nmo(im).eq.0) then
              ryx = 1.0
              dum2(im,1) = -999.0
              dum2(im,2) = -999.0
              dum2(im,3) = -999.0
            endif

            dum2(im,4) = 0.0
            if(dum2(im,1) .gt. 0.001) then
              dum2(im,4) = dum2(im,3) / dum2(im,1)*100.0*ry
            endif
c 
c               Print average data
              write(now,372)   xmonam(im), 
     1                        (dum2(im,j)/ryx, j=1,4), iz, nmo(im)
          end do
c
c ---------------------------------------------------------
c		Print Basin Total average to Report 
            if(cplot.eq.'Report      ') then
              write(116,375) 'Basin Total ',(dum2(13,j)/ryx, j=1,4),
     1          'Basin Total '
            endif

  500 close(116)
      return

c
c        Formats
C-------------------------------------------------------------------
  150   format('+', '   Printing Well Comparison ',a5,
     1              i5,' of ', i5, '; or ',f8.0, ' % Complete')
  170       format(' is = ', i5, ' cdivid = ', a12)
  270  format(2a12,i5, 2x, a3, 20f8.0)
  280  format(2(a12,','),i5,',', 2x, a3, ',', 20(f8.0,','))

  300  format(a12,',', i5,',', 2x, a3, ',', 20(f8.0,','))

  310   format(/,
     1  'Structure                    Gauged    Est.   Delta   Delta',/
     1  'ID            Acc Year   Mo Pumping Pumping Pumping   Pump%',
     1  ' Name',19x, '     Reach    #'/
     1  '___________  ____ ____ ____ _______ _______ _______ _______ ',
     1  25('_'), ' _______ ____')
     

     
  350  format(a12,           5x,i5,2x,a3,4f8.0,1x, a24, 1x, i8,i5)
  351  format('Reach   ', i3,6x,i5,2x,a3,4f8.0,1x, 24x, 1x, i8, i5)
  
  352  format('Basin Total ',5x,i5,2x,a3,4f8.0,1x, 24x, 1x, i8, i5)
  360  format(a12,',', 5x,',',i5,',', 2x, a3, ',', 4(f8.0,','),
     1        1x, a24, ',', 1x, i8, ',' i5, ',')  
  362  format(12x,',', 5x,',',i5,',', 2x, a3, ',', 4(f8.0,','),
     1        1x, 24x, ',', 1x, i8, ',' i5, ',')  

  370  format(a12,           5x,'  Ave',2x,a3,4f8.0,1x,a24,1x,i8,i5)
  371  format('Reach   ', i3,6x,'  Ave' 2x,a3,4f8.0,1x,24x,1x,i8,i5)  
  372  format('Basin Total ',5x,'  Ave',2x,a3,4f8.0,1x,24x,1x,i8,i5)
  373  format(
     1  '___________  ____ ____ ____',
     1  ' _______ _______ _______ _______ ',
     1  24('_'), ' ________ ____')
  
  374   format(/,
     1  'River       ,    Gauged,      Est.,     Delta,     Delta,'/,
     1  'Reach       ,   Pumping,   Pumping,   Pumping,  Pumping %,',
     1  '                   Name')
     
  375  format(a12,',', 4(f10.0,','), 1x,a24)     
  
  380  format(a12,',', 5x,',', '  Ave,',2x, a3, ',', 4(f8.0,','),
     1 1x, a24, ',', 1x, i5, ',')  
c
c _________________________________________________________
c
c               Error Messages
c
  390       format(i4, 1x, a12, 12f8.0)

c
  430 FORMAT(/, '  Outwelc: Warning',/
     1  '    Station ',a12,' of Historic Well Pumping file (.weh)',/
     1  '    not found in well station file (*.wes) in year ', i5,/
     1  '    OK, moving on')

  450 write(6,*)  '   Outwelc; Request exceeds binary file 42 size'
      write(io99,*) '   Outwelc; Request exceeds binary file 42 size'
      goto 9999
  460 write(6,*)  '   Outwelc; Request exceeds binary file 1 size'
      write(io99,*) '   Outwelc; Request exceeds binary file 1 size'
      goto 9999
c
c
c
c _________________________________________________________
c
c       Error Handling
  926 write(io99,927) iin2, filena
  927 format(' outwelc.f; End of file # ', i4, ' encountered',/,
     1       '   File name: ', a256)
      goto 9999
c
  928 write(io99,929) iin2, filena
  929 format(' outwelc.f; Problem reading file # ', i4,/,
     1       '   File name: ', a256,/
     1       '   Problem record (next line):')

      backspace(iin2)
      read(iin2, '(a256)',end=926,err=926) recin
      write(io99,'(a256)') recin
      goto 9999

 9999 write(6,*) '  Stopped in Outwelc, see the log file (*.log)'
      write(io99,*)'  Stopped in Outwelc'
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

c
c _________________________________________________________
c
      stop 
      END      
