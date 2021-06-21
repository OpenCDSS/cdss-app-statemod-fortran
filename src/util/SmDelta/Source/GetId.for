c                            
c ************************************************************
       subroutine getid(n, is1)
c
c               For diversions, instream flows and stream gages,
c                 It gets the river node for a given requested ID
c               For a reservoir,
c                 It gets the reservoir ID requested 
c               idreq =  requested id
c               n     =  requested station counter
c               is1   =  For diversions, instream flows and stream
c                          gages = river location
c                        For reservoirs = reservoir requested
         include 'SmDelta.inc'
c
c               Find corresponding diversion or instream flow
          if(idiv.eq.0) then
            do 112 i=1,numdiv
              if(idreq(n).eq.cdivid(i)) then
                is1=idvsta(i)
                idz(ifx,n) = cdivid(i)
                namex(ifx,n) = divnam(i)
                goto 140
              endif
  112       continue
          endif
c
c               Find corresponding instream flow
          if(idiv.eq.3) then
            do 122 i=1,numifr
              if(idreq(n).eq.cifrid(i)) then
                is1=ifrsta(i)
                idz(ifx,n) = cifrid(i)
                namex(ifx,n) = xfrnam(i)
                goto 140
              endif
  122       continue
          endif
c
c               Find corresponding stream gage
          if(idiv.eq.1) then
            do 132 i=1,numrun
              if(idreq(n).eq.crunid(i)) then
                is1=irusta(i)
                idz(ifx,n) = crunid(i)
                namex(ifx,n) = runnam(i)
                goto 140
              endif
  132       continue
          endif
c
c               Find corresponding streamID
          if(idiv.eq.5) then
            do i=1,numrun
              if(idreq(n).eq.crunid(i)) then
                rec12=crunid(i)
                rec1=rec12(1:1)
                if(rec1.eq.'0') then
                  is1=irusta(i)
                  idz(ifx,n) = crunid(i)
                  namex(ifx,n) = runnam(i)
                  goto 140
                endif
              endif
            end do
          endif

c
c               Find corresponding reservoir
          if(idiv.eq.2) then
            do 134 i=1,numres
              if(idreq(n).eq.cresid(i)) then
                is1=i
                idz(ifx,n) = cresid(i)
                namex(ifx,n) = resnam(i)
                goto 140
              endif
  134       continue
          endif
c          
          write(99,*) '  Problem cannot find requested id ', idreq(n)
          goto 212
  140   return

  212   write(6,220) fillog
        write(99,220) fillog
  220   format('  Getid; Unsuccessful termination, see ', a72)
        write(6,*) 'Stop 1'
        call flush(6)
        stop 
        end
