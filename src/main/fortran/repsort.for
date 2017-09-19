c
c *********************************************************
c
          subroutine repsort(n, maxrep, ireprnk, reprnk, reprnkx)
c
c _________________________________________________________
c	Program Description
c
c       Repsort; It does a simple sort of replacement reservoir info
c		Called by GetRep.f that is called by Oprinp.f 
c
c
c _________________________________________________________
c	Dimensions
c
          dimension reprnk(maxrep), ireprnk(maxrep), temp(maxrep)
          real*8 reprnk, reprnkx, temp, c
c
c _________________________________________________________
c
c               Step 1; Initilize

          iout=0
          nlog=99
c
c _________________________________________________________
c
c               Step 2; Check local dimensions

          if(n.gt.maxrep) then
            write(nlog,*) '  Repsort; Local Dimension Exceeded'
            write(nlog,*) '  Repsort; maxrep = ', maxrep
            write(nlog,*) '  Revise common.inc, statem.f & repsort.f'
            goto 9999
          endif
c
c _________________________________________________________
c
c               Step 3; Sort

          do i=1,n
            temp(i) = reprnk(i)
          end do
                         
          imx = 1          
          c  = temp(imx)

          do 120 i1=1,n
            do 110 i2=1,n
              if(temp(i2).lt.c) then
                imx = i2
                c = temp(i2)
              endif
 110         continue
c
            ireprnk(i1) = imx
            temp(imx) = 9999999.
            c = 9999999.
 120      continue                                       
c
c _________________________________________________________
c
c               Step 4; Store minimum Admin # of replacement
c                       reservoirs
          reprnkx = reprnk(ireprnk(1))
c
c _________________________________________________________
c
c               Step 5; Print results

          if(iout.ge.1) then
            write(nlog,129)

            do i=1,n
              n1=ireprnk(i)
              write(nlog,133) i, reprnk(n1), ireprnk(i), reprnkx
            end do
          endif
c
c _________________________________________________________
c
c               Step 6; Return
          return
c
c _________________________________________________________
c
c               Step 7; Error Processing

 9999     write(6,*)  '  Stopped in Repsort, see the log file (*.log)'
          write(99,*) '  Stopped in Repsort'
          write(6,*) 'Stop 1' 
          call flush(6)
          call exit(1)

c
c _________________________________________________________
c
c               Formats
 129      format(/,'  Repsort; sort results',/
     1      10x, ' Rank         Admin # Rep#     Min Admin #',/
     1      10x, ' ____ _______________ ____ _______________')
 133      format('  Repsort;',i5, f16.5, i5, f16.5)

          stop 
          end

