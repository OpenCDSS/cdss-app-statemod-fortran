c
c *********************************************************
c
        subroutine year(iyr, iyrmo, imomo, cyr1)
c
c
c _________________________________________________________
c	Program Description
c
c       Year; It sets data associated with a year type
c _________________________________________________________
c	Dimensions
c
        dimension iyrmo(13), imomo(13)
        character cyr1*5
c
c _________________________________________________________
c
c               Calendar Year (Jan - Dec
        do j=1,13
          iyrmo(j) = iyr
          imomo(j) = j
        end do
c
c _________________________________________________________
c
c               Water Year (Oct - Sept)

        if(cyr1 .eq. '  WYR') then
          do j=1,3
            iyrmo(j) = iyr-1
            imomo(j) = j+9
          end do

          do j=4,12
            imomo(j)=j-3
          end do
        endif
c
c _________________________________________________________
c
c               Irrigation Year (Nov - Dec)

        if(cyr1 .eq. '  IYR') then
          do j=1,2
            iyrmo(j) = iyr-1
            imomo(j) = j+10
          end do

          do j=3,12
            imomo(j) = j-2
          end do
        endif
c
c _________________________________________________________
c

        return
        end



