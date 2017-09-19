c
c *********************************************************
c
      subroutine outwr(maxwrx, itype, is, nid, nf)
c
c
c _________________________________________________________
c	Program Description
c
c       Outwr; It finds all water rights associated with a structure
c
c _________________________________________________________
c       Documentation                                                                 
c              maxwrx  max # of water rights per structure
c              itype   type of water right sort
c                      1=direct or instream flow
c                      2=storage (reservoir),
c                      6=well
c              is      river node
c              nid     station ID, currently only used for wells but
c                        it has potential for future use when several
c                        structures (users) exist at the same stream
c                        node
c              nf      output file
c              kstr    code for multiple structures at this river node
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'

      dimension rdvnk1(maxwrx)
      dimension dcrdivX(maxwrx), idvrswX(maxwrx),
     1          idivcoX(maxwrx), divcapX(maxwrx)
      dimension cstr(maxwrx),    dcrtyp(maxwrx),
     1          titlwr(11)

      character titlwr*32, cstr*12, dcrtyp*6
      data titlwr/      
     1          '    # of Rights (max = 50)    : ',
     1          '    Structure ID              : ',
     1          '    Capacity (cfs or af)      : ',
     1          '    Administration Number     : ',
     1          '    On/Off Switch (1=on)      : ',
     1          '    Owner (0 = InStream)      : ',
     1          '    Decree Type               : ',
     1          '    Decreed amount (cfs)      : ',
     1          '    Decreed amount (af@30)    : ',
     1          '    Decreed amount (af@31)    : ',
     1          '    Decreed amount (af)       : '/
c
c _________________________________________________________
c		Step 1; Initilze

      k1=0
      kdiv=0
      kisf=0
      kwel=0
      kres=0

      ndx=0
      nw=0

      dt = 0.0
      dd = 0.0
      dw = 0.0
c
c rrb 00/04/04; Limit output
c     maxwr = 150
      maxwr = 50
c      write(nf,*) '  Outwr; itype, is, nf ', itype, is, nf
c      write(nf,*) '  Outwr; numdvr ', numdvr
c
c
      if(itype.eq.1) then
c
c _________________________________________________________
c              Identify all direct flow rights
        kdiv=0
        do k=1,numdvr
          nd = idivco(1,k)
c         write(99,*) '  Outwr;  k, nd, idvsta(nd), is ' 
c         write(99,*) '       ', k, nd, idvsta(nd), is

          if(idvsta(nd).eq.is) then             
            ndx=nd
c           write(99,*) ' Outwr; Found one'
            k1=k1+1
            kdiv=k1
c
c rrb 99/08/10; Dimension check
c rrb 00/04/03; Warn but go on
c           if(k1.gt.maxwr) goto 9999 
            if(k1.gt.maxwr) then
              write(io99,*) '  Outwr; FYI all direct rights not printed'
              k1=maxwr
              goto 100
            endif

c           cstr(k1)    = cdivid(nd)
            call backfil( cdivid(nd), cstr(k1))

            divcapX(k1) = divcap(nd)
            rdvnk1(k1)  = rdvnk(k)
            dcrtyp(k1)  = '   Div'
            dcrdivX(k1) = dcrdiv(k)
            idvrswX(k1) = idvrsw(k)
c
c rrb 04/09/07; For a diversion revise the number of owners 
c               to be the number of owners
c           idivcoX(k1) = idivco(2,k)
            idivcoX(k1) = ndown(nd+1)-ndown(nd)
     
            dt = dt + dcrdivX(k1)
            dd = dt
          endif
        end do
c
c _________________________________________________________
c              Identify all instream rights
        kisf=0
        do k=1,numfrr
          nd = iifrco(k)

          if(ifrsta(nd).eq.is) then
            k1=k1+1
            kisf=k1
c
c rrb 99/08/10; Dimension check
c rrb 00/04/04; Warn but go on
c           if(k1.gt.maxwr) goto 9999
            if(k1.gt.maxwr) then
              write(io99,*) '  Outwr; FYI all ISF rights not printed'
              k1=maxwr
              goto 100
            endif

c           cstr(k1)    = cifrid(nd)
            call backfil( cifrid(nd), cstr(k1))

            divcapX(k1) = -1.
            rdvnk1(k1)  = rfrnk(k)
            dcrtyp(k1)  = '   Isf'
            dcrdivX(k1) = dcrifr(k)
            idvrswX(k1) = iifrsw(k)
            idivcoX(k1) = 0
     
            dt = dt + dcrdivX(k1)
          endif
        end do

c
c _________________________________________________________
c               Find wells tied to diversion structures
c rrb 01/02/04; Simplify and fix problmes related to station location
c
        kwel=0
        if(ndx.gt.0) nw=idivco2(ndx)
        if(nw.gt.0) then 

          do k=1,numdvrw
            nx = idivcow(1,k)
c
c rrb 01/02/04; Simplify and fix problems related to station location
            if(nx.eq.nw) then
c           if(idvcomw(nx).eq.6) then
c             nd=idivcow2(nx)
c             if(idvsta(nd).eq.is) then  
              k1=k1+1
              kwel=kwel+1
c
c rrb 99/08/10; Dimension check
c rrb 00/04/04; Warn but go on
c             if(k1.gt.maxwr) goto 9999
              if(k1.gt.maxwr) then
cx              write(io99,*) '  Outwr; FYI all well rights not printed'
                k1=maxwr
                goto 100
              endif

              cstr(k1)    = cdividw(nx)
              call backfil( cdividw(nx), cstr(k1))

              divcapX(k1) = divcapw(nx)
              rdvnk1(k1)  = rdvnkw(k)
              dcrtyp(k1)  = '   Wel'
              dcrdivX(k1) = dcrdivw(k)
              idvrswX(k1) = idvrsww(k)
              idivcoX(k1) = idivcow(2,k)

              dt = dt + dcrdivX(k1)
              dw = dw + dcrdivX(k1)
            endif
          end do
        endif
c
c               End itype = 1 (Diversion or ISF)
      endif
c
c _________________________________________________________
c              Identify all storage rights
c rrb 98/12/02; Wells
      if(itype.eq.2) then
        kres=0
        do k=1,numrsr
          nd = iresco(1,k)

c         write(99,*) ' Outrep; k, nd, irssta(nd)', k, nd, irssta(nd)
          if(irssta(nd).eq.is) then             
            k1=k1+1
            kres=k1
c
c rrb 99/08/10; Dimension check
c rrb 00/04/04; Warn but go on
c           if(k1.gt.maxwr) goto 9999
            if(k1.gt.maxwr) then
              write(io99,*) '  Outwr; FYI all res rights not printed'
              k1=maxwr
              goto 100
            endif

c           istr(k1)    = xresid(nd)
c           cstr(k1)    = cresid(nd)
            call backfil( cresid(nd), cstr(k1))

            dcrtyp(k1)  = '   Sto'
            divcapX(k1) = volmax(nd)
            rdvnk1(k1)  = rrsnk(k)
            dcrdivX(k1) = dcrres(k)
            idvrswX(k1) = irsrsw(k)
            idivcoX(k1) = iresco(2,k)

            dt = dt + dcrdivX(k1)
          endif  
        end do

      endif
c
c _________________________________________________________
c              Identify well rights to well structures only
      if(itype.eq.6) then
c       write(io99,*) '  Outwr; nf = ', nf
        kwel=0
        do k=1,numdvrw
          nd = idivcow(1,k)
c
c               Insure we have the proper well at river station is
c                 not really an issue for diversions and reservoirs
c         write(99,*) ' Outrep; k, nd, irssta(nd)', k, nd, irssta(nd)
c         if(idvstaw(nd).eq.is) then 
          if(nd.eq.nid) then
            k1=k1+1
            kwel=k1
c
c rrb 99/08/10; Dimension check
c rrb 00/04/04; Warn but go on
c           if(k1.gt.maxwr) goto 9999
            if(k1.gt.maxwr) then
cx            write(io99,*) '  Outwr; FYI all well rights not printed'
              k1=maxwr
              goto 100
            endif


c           cstr(k1)    = cdividw(nd)
            call backfil( cdividw(nd), cstr(k1))

            divcapX(k1) = divcapw(nd)
            dcrtyp(k1)  = '   Wel'
            rdvnk1(k1)  = rdvnkw(k)
            dcrdivX(k1) = dcrdivw(k)
            idvrswX(k1) = idvrsww(k)
            idivcoX(k1) = idivcow(2,k)

            dt = dt + dcrdivX(k1)
            dw = dw + dcrdivX(k1)
          endif
        end do
      endif

c
c _________________________________________________________
c rrb 01/02/27; Calculate total by type
c
c rrb 01/02/27; Total Diversions
        if(kdiv.gt.0) then
          k2=k1+1
          dcrtyp(k2) = 'TotDiv'
          dcrdivX(k2)= dd
        endif
c
c rrb 01/02/27; Total ISF
        if(kisf.gt.0) then
          k2=k1+1
          dcrtyp(k2) = 'TotIsf'
          dcrdivX(k2)= dt
        endif          
c
c rrb 01/02/27; Total Wells
        if(kwel.gt.0) then   
          k2=k1+1
          if(kdiv.gt.0) k2=k2+1
          dcrtyp(k2) = 'TotWel'
          dcrdivX(k2)= dw
        endif
c
c rrb 01/02/27; Total Reservoirs
        if(kres.gt.0) then
          k2=k1+1
          dcrtyp(k2) = 'TotRes'
          dcrdivX(k2)= dt
        endif

c
c _________________________________________________________
c               Finally Print results
      
 100  if(k1.gt.0) then                                                  
        c1=factor*30
        c2=factor*31
        write(nf,130) titlwr(1), k1
        write(nf,132) titlwr(2), (cstr(k),k=1,k1)
        write(nf,150) titlwr(3), (divcapX(k),k=1,k1)
        write(nf,131) titlwr(4), (rdvnk1(k),k=1,k1)
        write(nf,130) titlwr(5), (idvrswX(k),k=1,k1)
        write(nf,130) titlwr(6), (idivcoX(k),k=1,k1)
c
c               Print non reservoirs
        if(itype.ne.2) then
          write(nf,133) titlwr(7), (dcrtyp(k),k=1,k2), 'TotAll'
          write(nf,140) titlwr(8), (dcrdivX(k),k=1,k2),    dt
          write(nf,150) titlwr(9), (dcrdivX(k)*c1,k=1,k2), dt*c1
          write(nf,150) titlwr(10), (dcrdivX(k)*c2,k=1,k2), dt*c2
        else
c
c               Print reservoir
          write(nf,133) titlwr(7), (dcrtyp(k),k=1,k2), 'TotAll'
          write(nf,150) titlwr(11), (dcrdivX(k),k=1,k2),    dt
        endif
        write(nf,*) ' '

      endif

      if(nf.eq.21) then
        write(22,160) k1
        write(22,170) (cstr(k), k=1,k1)
      endif

      Return
c _________________________________________________________
c
c               Formats
c
  130 format(a32, 2x, 15i12,     /, (34x, 15i12)     )
  131 format(a32, 2x, 15(f12.5), /, (34x, 15(f12.5)) )
  132 format(a32, 2x, 15a12,     /, (34x, 15a12)     )
  133 format(a32, 2x, 15(6x,a6), /, (34x, 15(6x,a6)) )
  140 format(a32, 2x, 15f12.2,   /, (34x, 15(f12.2)) )
  150 format(a32, 2x, 15f12.0,   /, (34x, 15(f12.0)) )
  160 format(/,'# of ,Right,s =     ,', i8)
  170 format(  'Struc,ture ,ID  =   ,', 15(a12,','), 
     1                           /, (34x, 15(a12,',')) )
c
c _________________________________________________________
c
c
c               Error Warnings
c

c9999 write(6,180) 
c     write(99,190) maxwr
c     call flush(6)
 180  format('    Stopped in Outwr',/,
     1       '    See the *.log file')
 190  format('    Stopped in Outwr; Local dimension ', i5, ' exceeded')
c     write(6,*) 'Stop 1' 
c     call flush(6)
c     call exit(1)

c     stop 
      END
