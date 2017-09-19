c
      SUBROUTINE GetEomX(nr, c, cid1)
c
c
c _________________________________________________________
c	Program Description
c
c		Called by Daydist to get reservoir storage
c		  from previous year. Note:
c		  For baseflows resvol1 is set in Virin 
c		  For simulate resvol1 is set in Bomsec
c
c _________________________________________________________
c	Dimensions
c

      include 'common.inc'      
      character cid1*12
c
c _________________________________________________________
c		Step 1; 
c
      
      iout=0
      c=resvol1(nr)
      if(iout.eq.1) write(nlog,*) ' GetEomX; ', cid1, iyr, mon, nr, c
      return
      end
     
