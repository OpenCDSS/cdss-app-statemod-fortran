c
c *********************************************************
c 
      subroutine AdjCase(nlog, recin, recout, nin, ntype)
c
c _________________________________________________________
c	Program Description
c		It changes characters from upper case to lower
c		case or visa versa
c
c
c _________________________________________________________
c	Update History
c
c		NA
c
c _________________________________________________________
c	Documentation
c
c
c	nlog   = log file number
c	recin  =  input record
c      recout = output record
c	nin    = record size
c	ntype  = 1 caps to lower case
c	         2 lower case to capitol
c
c
c _________________________________________________________
c	Dimensions
c
      dimension cap(26), lc(26)
      character cap*1, lc*1
      character recin*132, recout*132
      
      data cap/'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K',
     1         'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 
     1         'W', 'X', 'Y', 'Z'/
      data lc /'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 
     1         'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
     1         'w', 'x', 'y', 'z'/
      
      iout=0
      recout=recin
c
c		Caps to lower case
      if(ntype.eq.1) then
        do i=1,nin
          do j=1,26     
            if(recout(i:i) .eq. cap(j)) recout(i:i) = lc(j)
          end do          
        end do
      else
        do i=1,nin
          do j=1,26     
            if(recout(i:i) .eq. lc(j)) recout(i:i) = cap(j)
          end do          
        end do        
      endif  
      
      if(iout.eq.1) then
        write(nlog,100) recin, recout
 100    format(' AdjCase;   recin = ', a72,/
     1         '           recout = ', a72)
      endif
      return
      end     
          
