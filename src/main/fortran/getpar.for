c
c ************************************************************
c
        subroutine getpar(itype, ip, ptypex)
c
c
c _________________________________________________________
c	Program Description
c
c       Getpar; It gets a structure type and parameter value 
c
c _________________________________________________________
c       Documentation               
c
c               itype = 0 diversion, instream flow, other (*.b43, *.xdd)
c                       1 stream gage (*.b43, *.xdd)
c                       2 reservoir (*.b44, *.xre)
c                       6 well (*.b43, *.xwe)
c                      99 help
c
c _________________________________________________________
c	Dimensions
       include 'common.inc'

       dimension  ptype(10)
       character  ptypex*24, ptype*12
                                                             
       data ptype/
     1 'Diversion  ',  'StreamGage  ', 'Reservoir   ',
     1 ' ',            ' ',            'Well        ',
     1 ' ',            ' ',            ' ', ' '/
c
c
c _________________________________________________________
c		Step 1; Initilize

         io99=99
c rrb 2005/11/22; River Loss and Carrier Loss
c     ndiv = 27
c     nres=21
      ndiv = 37         
      nres=26
      ndivw=18
         
      nx=amax0(ndiv, nres, ndivw)
c
c               Process diversion or streamGage type
      do 100 i=1,nx
        if(itype.le.1 .and. ptypex.eq.paramd(i)) then
          ip = i
          goto 110
        endif        
c
c               Process reservoir type
        if(itype.eq.2 .and. ptypex.eq.paramr(i)) then
          ip = i
          goto 110
        endif
c
c               Process well type
        if(itype.eq.6 .and. ptypex.eq.paramw(i)) then
          ip = i
          goto 110
        endif 
c
c               Print available parameters
        if(itype.eq.99) then           
          write(io99,140) (j, paramd(j), j=1,ndiv) 
          write(io99,141) (j, paramr(j), j=1,nres) 
          write(io99,142) (j, paramw(j), j=1,ndivw)
          call flush(6)
          goto 110
        endif
  100   continue
        goto 120

c 110   write(io99,*) '  Getpar, ptypex, ip', ptypex, ip
  110   return
c
c               Error Messages
  120    write(io99,*) ' ' 
         write(io99,*) '   Getpar; For type = ', ptype(itype+1)
         write(io99,*) '           Parameter not found = ', ptypex
         write(io99,140) (j, paramd(j), j=1,ndiv) 
         write(io99,141) (j, paramr(j), j=1,nres)
         write(io99,142) (j, paramw(j), j=1,ndivw)
         write(6,*) '  Stopped in Getpar, see the log file (*.log)'
         write(io99,*) '  Stopped in Getpar'
  140    format(/,
     1     ' Available diversion or streamflow parameters:',/
     1     (2x, i5, 1x, a24))
  141    format(/,
     1     ' Available reservoir parameters:',/
     1     (2x, i5, 1x, a24))
  142    format(/,
     1     ' Available well parameters:',/
     1     (2x, i5, 1x, a24))

         write (6,*) 'Stop 1'
         call flush(6)
         call exit(1)


         stop 
         end

