c                            
c ************************************************************
c
       subroutine getsta(idiv,n,is1,ifound,idreqx)
c
c
c _________________________________________________________
c	Program Description
c
c       GetSta; It gets the structure ID (idreqx)
c               and structure ID pointer (ifound)
c               For a given data type (idiv) and river location id (n)
c
c _________________________________________________________
c	Documentation
c
c               idiv = data type
c                      0 diversion 
c                      1 stream 
c                      3 instream flow
c                      6 well 
c               n = river ID 
c               is1 = river ID
c               ifound = 0 not found
c                        x found at structure x
c               idreqx = structure ID found
c
c _________________________________________________________
c	Dimensions
c
        include 'common.inc'
        character idreqx*12
c
c _________________________________________________________
c
        iout=0

        is1=n                  
        ifound=0         
        idreqx=' '
c
c _________________________________________________________
c               Find corresponding diversion
        if(idiv.eq.0) then
          do i=1,numdiv
            if(idvsta(i).eq.n) then
              idreqx=cdivid(i)
              ifound=i
            endif
          end do  
        endif
c
c _________________________________________________________
c               Find corresponding instream flow river station
        if(idiv.eq.3) then
          do i=1,numifr
            if(ifrsta(i).eq.n) then
              idreqx=cifrid(i)
              ifound=i
            endif
          end do  
        endif
c
c _________________________________________________________
c               Find corresponding stream gage
        if(idiv.eq.1) then
          do i=1,numrun
            if(irusta(i).eq.n) then
              idreqx=crunid(i)
              ifound=i
            endif
          end do  
        endif
c
c _________________________________________________________
c               Find well station
        if(idiv.eq.6) then
          do i=1,numdivw
            if(idvstaw(i).eq.n) then
              idreqx=cdividw(i)
              ifound=i
            endif
          end do
        endif
c
c _________________________________________________________
c               Find a plan station
        if(idiv.eq.7) then
          do i=1,nplan
            if(ipsta(i).eq.n) then
              idreqx=pid(i)
              ifound=i
            endif
          end do
        endif
c
c _________________________________________________________
          
        if(iout.eq.1) then
          write(io99,*)  '  Getsta;  idiv,n,is1,ifound,idreqx'
          write(io99,*)  '        ', idiv,n,is1,ifound,idreqx
        endif

        return
        end






