c                            
c ************************************************************
       subroutine getsta(n,is1,ifound)
c
c               Get station id for a given river id
        include 'SmDelta.inc'

        is1=n                  
        ifound=0
c
c               Find corresponding diversion 
          if(idiv.eq.0) then
            do i=1,numdiv
              if(idvsta(i).eq.n) then
                idz(ifx,is1) = cdivid(i)
                namex(ifx,is1) = divnam(i)
                ifound=1
              endif
            end do
          endif
c
c               Find corresponding instream flow
          if(idiv.eq.3) then
            do i=1,numifr
              if(ifrsta(i).eq.n) then
                idz(ifx,is1) = cifrid(i)
                namex(ifx,is1) = xfrnam(i)
                ifound=1
              endif
            end do  
          endif
c
c               Find corresponding stream gage
          if(idiv.eq.1) then
            do i=1,numrun
              if(irusta(i).eq.n) then
                idz(ifx,is1) = crunid(i)
                namex(ifx,is1) = runnam(i)
                ifound=1
c               write(99,122) ifx, is1, idz(ifx,is1) 
c 122           format('  Getsta; ifx, is1, idz(ifx,is1)', 2i5,1x,a12)
              endif
            end do  
          endif
c
c               Find corresponding well
          if(idiv.eq.4) then
              write(99,*) ' '
              write(99,*) ' Getsta; numdivw=', numdivw
            do i=1,numdivw
              write(99,*) ' Getsta; idvstaw(i) = ', n, idvstaw(i)
c
c rrb 02/01/07; Wells are by well not stream station
c             if(idvstaw(i).eq.n) then
              if(i.eq.n) then

                write(99,*) ' Getsta; cdividw(i) = ', cdividw(i)

                idz(ifx,is1) = cdividw(i)
                namex(ifx,is1) = divnamw(i)
                ifound=1
              endif
            end do
          endif

c
c               Find corresponding streamID
          if(idiv.eq.5) then
            do i=1,numrun
              if(irusta(i).eq.n) then
                rec12 = crunid(i)
                rec1  = rec12(1:1)
                if(rec1.eq.'0') then
                  idz(ifx,is1) = crunid(i)
                  namex(ifx,is1) = runnam(i)
                  ifound=1
c                 write(99,122) ifx, is1, idz(ifx,is1) 
c 122             format('  Getsta; ifx, is1, idz(ifx,is1)', 2i5,1x,a12)
                endif
              endif
            end do  
          endif

        return
        end
