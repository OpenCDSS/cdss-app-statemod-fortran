c
        subroutine getrep(n,k)
c
c _________________________________________________________
c	Program Description
c
c       Getrep; It processes replacement reservoir information
c               For a type 10 operating right
c
c _________________________________________________________
c	Documentation
c
c       
c               irepn         = number of replacement reservoirs
c               irepk(lx)     = replacement reservoirs operation ID
c               ireprnk(lx)   = rank of replacement reservoir
c               irepnr(lx)    = replacement reservoir pointer
c               irepown(lx)   = replacement reservoir owner
c               irepexp(n,nd) = Exchange point
c                               (n=rep reservoir, nd = diversion)
c                                0=N/A (direct supply)
c                                +=river ID of exchange point
c               ireptyp(lx)   = Replacement type from datinp.for
c                                0,1=100% replacement
c                                -1=depletion replacement
c                                -999 no replacement
c               reprnk(lx)    = Replacement reservoir admin #
c               reprnkx       = Most senior replacement reservoir admni #
c
c               iout          = 0 no detailed printout
c                               1 yes detailed printout
c
c _________________________________________________________
c	Dimensions
c
        include 'common.inc'
c
c _________________________________________________________
c
c               Step 1; Initilize

        iout = 0                                    
        if(iout.ge.1) write(io99,*)
     1     '  Getrep; Replacement reservior provided', n,k

        irepk(n)   = k
        irepnr(n)  = iopsou(1,k)
        irepown(n) = iopsou(2,k)
        irepMax(n) = iopsou(5,k)
        irepLim(n) = ioprlim(k)
c _________________________________________________________
c
c               Step 2; Determine rank of replacement reservoirs
c                       using a simple sort
        reprnk(n)  = ropnk(k)
        ireprnk(n) = n
        reprnkx    = reprnk(n)

        if(n.gt.1) then
          call repsort(n,maxrep,ireprnk,reprnk,reprnkx)          
        endif
c
c
c _________________________________________________________
c
c               Step 3; Determine if every structure would be served 
c                       by a direct replacement or exchange
        if(iout.eq.1) write(nlog,*) '  GetRep; numdiv ', numdiv
        do 150 nd=1,numdiv
          idcdd=idvsta(nd)

          idcdr=irssta(iopsou(1,k))
          ndndr=ndnnod(idcdr)
          issr=idncod(idcdr)
c
c               Error with network
          if(issr.eq.0.or.ndndr.le.1) goto 180
c
c               Search every river node downstram of the reservoir
c               to see if it can be served directly
          if(iout.eq.1) write(nlog,*) '  GetRep; ndndr-1', ndndr-1
          do 100 ndr=1,ndndr-1
            if(issr.eq.idcdd) then
              irepexp(n,nd) = 0
              goto 150
            endif
 100      issr=idncod(issr)                        
c
c               Could not be served directly
c               Must be served by an exchange
c               Find the exchange point
         if(iout.ne.0) then
           write(io99,119) nd, cdivid(nd), idcdd
         endif

         ndndd=ndnnod(idcdd)
         issr=idncod(idcdr)
         issd=idcdd
c
c               Error with network
         if(issd.eq.0.or.ndndd.le.1) goto 180

         if(iout.eq.1) write(nlog,*) '  GetRep; ndndr-1', ndndr-1
         do 130 ndr=1,ndndr-1
           issd=idcdd
           do 120 ndd=1,ndndd
             if(issr.eq.issd) then
c              iExPoint(k)=issr
               iRepExp(n,nd) = issr
               go to 150
             endif
 120       issd=idncod(issd)
 130     issr=idncod(issr)
 
         if(iout.ne.0) then
           write(io99,119) nd, cdivid(nd), issd
         endif

         goto 200


 150    continue
c
c _________________________________________________________
c
c               Step 4; Print results
        if(iout.eq.1) write(99,220)

        do 160 nd=1,numdiv
          n1 = irepexp(n,nd)
          if(iout.eq.1) then
            if(n1.eq.0) then
              write(99,230) n, nd,divnam1(nd),irepexp(n,nd)
            else
              write(99,230) n, nd,divnam1(nd),irepexp(n,nd), 
     1                   stanam1(n1)
            endif
          endif
 160    continue
        goto 500
c
c _________________________________________________________
c
c               Step 5; Error Processing

 180    write(99,190) crigid(k), idvsta(nd), iopsou(1,k)
        goto 9999

 200    write(99,210) crigid(k), idvsta(nd), iopsou(1,k)
        goto 9999
c
c rrb 2011/10/15; Revise to warn but not stop if a replacement
c                 reservoir is used but a structure cannot
c                 be served because it may be OK (e.g. Laramie 
c                 River off the Yampa)
 9999   continue
cx        write(6,*) '  Getrep; Stopped because of bad data' 
cx        write(6,*) '          See the log file'       
cx        write(99,*) '  Getrep; Stopped because of bad data'
cx        write(6,*) 'Stop 1'
cx        call flush(6)
cx        call exit(1)
cx       stop

c
c _________________________________________________________
c
c               Step 6; Return

 500  return
c
c _________________________________________________________
c
c               Formats
 119    format('  Getrep; nd, cdivid, idcdd = ', i5,1x,a12,1x,i5)

 190    format(
     1  '  GetRep; Warning',
     1  ' Operation Right ', i12,' has a problem;'/,                      
     1  ' Destination = ', i12, ' or source = ', a12, 
     1  ' have a problem related to the network')

 210    format(
     1  '  GetRep; Warning',
     1  ' Operation Right ', a12,/,                      
     1  ' Destination = ', i12, ' and source = ', i12, 
     1  ' cannot be served directly or by exchange',/
     1  ' May be OK if a diversion is not served by',/
     1  ' a replacement reservoir in *.dds')
     
 220    format(/,72('_'),/'  Getrep;',/
     1  '    n   nd  Diversion Name',11x,
     1        'exp Exchange Pt Name',/   
     1  ' ____ ____ ', 24('_'), ' ____ ', 24('_'))

 230    format(' GerRep; ', 2i5, 1x, a24,  i5, 1x, a24)

      stop
      END                                                               

