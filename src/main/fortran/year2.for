c
c *********************************************************
c
        subroutine year2(nlog, mthday, xmon, xmonam,  cyr1, imstr)
c
c _________________________________________________________
c	Program Description
c
c
c       Year2; It sets selected data associated with a year type 
c _________________________________________________________
c	Dimensions
c
      dimension
     1 xmon(12),   mthd(12), 
     1 xmonam(14), mthday(12)   
      character cyr1*5, xmon*4, xmonam*4
c
c _________________________________________________________
c		Step 1; Initilize        
c
      xmon(1)  = 'JAN '
      xmon(2)  = 'FEB '
      xmon(3)  = 'MAR '
      xmon(4)  = 'APR '
      xmon(5)  = 'MAY '
      xmon(6)  = 'JUN '
      xmon(7)  = 'JUL '
      xmon(8)  = 'AUG '
      xmon(9)  = 'SEP '
      xmon(10) = 'OCT '
      xmon(11) = 'NOV '
      xmon(12) = 'DEC '
      
      xmonam(13) = 'TOT '
      xmonam(14) = 'AVE '

      ierror=1
      do j=1,12
        mthd(j) = 31
      end do   
  
      mthd(2) = 28
      mthd(9) = 30
      mthd(4) = 30
      mthd(6) = 30
      mthd(11) = 30
        
c
c _________________________________________________________
c
c		Calendar Year       
      if(cyr1 .eq. '  CYR') then
        j1 = 0
        ierror = 0
        imstr=1   
        do j=1,12
          j1 = j1+1
          if(j1.gt.12) j1 = 1
          mthday(j) = mthd(j1)
          xmonam(j) = xmon(j1)
        end do
      endif
c
c		Water Year
      if(cyr1 .eq. '  WYR') then
        j1 = 9
        ierror = 0
        imstr=10
        do j=1,12
          j1 = j1+1
          if(j1.gt.12) j1 = 1
          mthday(j) = mthd(j1)
          xmonam(j) = xmon(j1)
        end do
      endif
c
c		Irrigation Year
      if(cyr1 .eq. '  IYR') then
        j1 = 10
        ierror = 0
        imstr=11

        do j=1,12
          j1 = j1+1
          if(j1.gt.12) j1 = 1
          mthday(j) = mthd(j1)
          xmonam(j) = xmon(j1)
        end do
      endif
c
c _________________________________________________________
c
      if(ierror.eq.1) then
        write(6,990) cyr1
        goto 9999
      endif
c
c _________________________________________________________
c

        return
        
  990  format(/,
     1  '  Datinp; Problem. Year type must be CYR, WYR or IYR',/
     1  '     Not ', a5,/
     1  '          Also if reading old control format it must',/
     1  '          be right justified (e.g. xxCYR)')
 9999  stop
        
        end



