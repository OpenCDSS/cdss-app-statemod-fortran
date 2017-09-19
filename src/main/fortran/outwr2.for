c
c *********************************************************
c
      subroutine Outwr2(maxwrx, itype, is, nid, nf)
c
c
c _________________________________________________________
c	Program Description
c
c       Outwr2; It finds all water rights associated with a structure
c		Same as OutWr but prints a summary, not every right
c		Since Outwr stops at 50 and the total is incorrect
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

      character titlwr*32, cstr*12, dcrtyp*6, titlwr2*32
c
c _________________________________________________________
c	Initilze

      k1=0
      kdiv=0
      kwel=0
      kres=0

      ndx=0
      nw=0

      dt = 0.0
      dd = 0.0
      dw = 0.0
      divcap1=0.0
      divcapW1=0.0
      
      dcrdiv1=0.0
      dcrdivW1=0.0
      dcrdivR1=0.0
      
c
c rrb 00/04/04; Limit output
c     maxwr = 150
      maxwr = 50
c      write(nf,*) '  Outwr2; itype, is, nf ', itype, is, nf
c      write(nf,*) '  Outwr2; numdvr ', numdvr
c
c
      if(itype.eq.1) then
c
c _________________________________________________________
c              Identify all direct flow rights
        do k=1,numdvr
          nd = idivco(1,k)
c         write(99,*) '  Outwr2;  k, nd, idvsta(nd), is ' 
c         write(99,*) '       ', k, nd, idvsta(nd), is

          if(idvsta(nd).eq.is) then             
            ndx=nd
c           write(99,*) ' Outwr2; Found one'
c rrb 20067/01/17; Outwr2 revision
c           k1=k1+1
            k1=1
            kdiv=kdiv+1

            divcap1 = divcap(nd)
            dcrdiv1 = dcrdiv1+ dcrdiv(k)
c
          endif
        end do
c
c _________________________________________________________
c              Identify all instream rights
        do k=1,numfrr
          nd = iifrco(k)

          if(ifrsta(nd).eq.is) then
c rrb 20067/01/17; Outwr2 revision
c           k1=k1+1
            k1=1
            kdiv=kdiv+1
            
            divcap1 = 0.
            dcrdiv1 = dcrdiv1+dcrifr(k)
          endif
        end do

c
c _________________________________________________________
c               Find wells tied to diversion structures
c rrb 01/02/04; Simplify and fix problmes related to station location
c
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
c rrb 20067/01/17; Outwr2 revision
c
c             k1=k1+1
              k1=1
              kwel=kwel+1

              divcapW1 = divcapw(nx)
              dcrdivW1 = dcrdivW1+dcrdivw(k)
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
        do k=1,numrsr
          nd = iresco(1,k)

c         write(99,*) ' Outrep; k, nd, irssta(nd)', k, nd, irssta(nd)
          if(irssta(nd).eq.is) then             
c rrb 20067/01/17; Outwr2 revision
c           k1=k1+1
            k1=1
            kres=kres+1
c
c rrb 99/08/10; Dimension check
c rrb 00/04/04; Warn but go on
c           if(k1.gt.maxwr) goto 9999
            if(k1.gt.maxwr) then
              write(io99,*) '  Outwr2; FYI all res rights not printed'
              k1=maxwr
              goto 100
            endif

            divcapR1    = volmax(nd)
            dcrdivR1    = dcrdivR1+ dcrres(k)
          endif  
        end do

      endif
c
c _________________________________________________________
c              Identify well rights to well structures only
      if(itype.eq.6) then
c       write(io99,*) '  Outwr2; nf = ', nf
        do k=1,numdvrw
          nd = idivcow(1,k)
c
c               Insure we have the proper well at river station is
c                 not really an issue for diversions and reservoirs
c         write(99,*) ' Outrep; k, nd, irssta(nd)', k, nd, irssta(nd)
c         if(idvstaw(nd).eq.is) then 
          if(nd.eq.nid) then
c rrb 20067/01/17; Outwr2 revision
c           k1=k1+1
            k1=1
            kwel=kwel+1
c
c rrb 99/08/10; Dimension check
c rrb 00/04/04; Warn but go on
c           if(k1.gt.maxwr) goto 9999
            if(k1.gt.maxwr) then
cx            write(io99,*) '  Outwr2; FYI all well rights not printed'
              k1=maxwr
              goto 100
            endif
c           write(io99,*) '  Outwr2; Wells ',  nd, k,
c    1        dcrdivw(k), dcrdivW1            
            divcapW1    = divcapw(nd)
            dcrdivW1    = dcrdivW1 + dcrdivw(k)
c           write(io99,*) '  Outwr2; Wells ',  nd, k,
c    1        dcrdivw(k), dcrdivW1
          endif
        end do
      endif

c
c _________________________________________________________
c rrb 01/02/27; Calculate total by type

c
c _________________________________________________________
c               Finally Print results
      
 100  if(k1.gt.0) then                                                  
        c1=factor*30
        c2=factor*31
c
c ---------------------------------------------------------        
c               Print diversions and ISF's data
c       if(itype.ne.2) then
        if(itype.eq.1) then
          write(nf,180) 
     1         1, divcap1,  divcap1*c1, divcap1*c2,
     1      kdiv, dcrdiv1,  dcrdiv1*c1, dcrdiv1*c2, 
     1         1, divcapW1, divcapW1*c1, divcapW1*c2,
     1      kwel, dcrdivW1, dcrdivW1*c1, dcrdivW1*c2 
        endif
c
c ---------------------------------------------------------        
c               Print reservoir data
        if(itype.eq.2) then
          write(nf,200) 
     1      1, divcapR1, kres, dcrdivR1
        endif
c
c ---------------------------------------------------------        
c		Print Well header        
        if(itype.eq.6) then
          write(nf,190) 
     1         1, divcapW1, divcapW1*c1, divcapW1*c2,
     1      kwel, dcrdivW1, dcrdivW1*c1, dcrdivW1*c2 
        endif
c
c ---------------------------------------------------------        
        
        write(nf,*) ' '

      endif
c      
c _________________________________________________________
c		Return

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
  180 format(/
     1 '    STRUCTURE DATA            : ',
     1 '    #       cfs     af@30     af@31',/
     1 '                                ',
     1 '_____ _________ _________ _________',/
     1 '      Diversion Capacity      : ', i5, 3f10.0,/
     1 '      Diversion Rights        : ', i5, 3f10.0,/
     1 '      Well Capacity           : ', i5, 3f10.0,/
     1 '      Well Rights             : ', i5, 3f10.0)
  190 format(/
     1 '    STRUCTURE DATA            : ',
     1 '    #       cfs     af@30     af@31',/
     1 '                                ',
     1 '_____ _________ _________ _________',/
     1 '      Well Capacity           : ', i5, 3f10.0,/
     1 '      Well Rights             : ', i5, 3f10.0)
  200 format(/
     1 '    STRUCTURE DATA            :     #        af',/
     1 '                                _____ _________',/
     1 '      Capacity                : ', i5, f10.0,/
     1 '      Reservoir Rights        : ', i5, f10.0)
c
c _________________________________________________________
c
c
c               Error Warnings
      END
