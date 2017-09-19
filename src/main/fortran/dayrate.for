c
        subroutine DayRate(idayX, idly, facdly)
c
c _________________________________________________________
c	Program Description
c
c       DayRate; It sets daily factor to limit code size
c
c _________________________________________________________
c	Dimensions
        
        include 'common.inc'
c       include 'daily.inc'
c
c _________________________________________________________
c
               
        if(idayX.eq.0) then
          FACDLY=DLYRAT(1,IDLY)
        else
          facdly=dlyratd(1,idly)
        endif
        return 
        end
