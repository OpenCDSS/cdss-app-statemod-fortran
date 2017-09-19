c
c
c *********************************************************
      subroutine dayreso
c
c
c _________________________________________________________
c	Program Description
c         Dayreso; It prints daily reservoir data by account
c
c         Called by execut or report
c
c _________________________________________________________
c
c	Documentation
c               ir   = reservoir counter (active or inactive)
c               idat = reservoir account counter (active or inactive)
c               ir1  = reservoir account+1 (active) e.g. a reservoir
c                      with 2 accounts will have 3 ir values 1=total 
c                      and 2 for each account
c
c _________________________________________________________
c
c	Dimensions
c
      include 'common.inc'
      character ownnamx*12, ftype*24, ptype*24
c                
c
c _________________________________________________________
c
c	Initilise
c           
      write(6,*) ' Subroutine Dayreso'
      write(6,*) ' '
c                                 
c
c		Set number of output values 1x in StateM.for
c	        and store in common.inc      
      ndiv = ndivO        
      nres = nresO
      ndivw= nwelO
c
c rrb 2006/01/21; Columns of data impacted by a new file size		
c               nbom = column of BOM data (1)
c		nemo = column of EOM data (16)
c		ntar = column of Tartet data (17)
c		none = column of One fill or BOM limit (18)
c		nidr = columns of irdr (account))
c		naccX = columns of nacc ( )
c		nresX = columns to adjust by factor
c               nresY = columns to print   

      nBom=1       
      nEom=16
      nEva=14
      nTar=17
      nOne=18
      
      nidr=nres-2
      naccX=nres-1
c
c rrb 2006/06/13; Add Reservoir Seepage Loss      
cr    nresX=nres-3
cr    nresY=nres-5
      nresX=nres-4
      nresY=nres-6
      
      

      nf=36
c     write(6,*) 'Dayreso top nf', nf
C
C-------------------------------------------------------------------
C
C------  Reservoir Summary with accounts
C
C-------------------------------------------------------------------
C
      call outtop(nf,1,31)
c     write(6,*) ' Dayreso nf after outtop', nf

      nrsactx=nrsact+numown
      if(numres.eq.0 .and. nrsact.eq.0) THEN
        Write(nf,*) ' NO ACTIVE RESERVOIR FOR THE CURRENT JOB'
        Goto 220
      endif
c
c               Get requested ID's
      idallx=0
      if(ioutx.eq.1) then
        call getin(2, iid, maxsta, idallx, idtype, ftype, ptype, idreq)
      endif
c
c               Set ID screen switch
      if(idallx.eq.0) then
        nid = numres
      else          
        nid=iid
      endif
C
c              
      ip1 = 0
      ir1 = 0
      ir  = 0
c
c              Set parameters for a plot or non plot retrevial
      idat = 0                     

      do 180 ip=1,nid
c       if(idtype(ip).ne.2) goto 140

        if(idallx.eq.0) then
          ir=ir+1
          is=irssta(ir)
        else
          call getid(2, is, ir, ir2, idreq(ip))
c
c               Skip if not a reservoir
          if(is.le.0) goto 180
          ir1=0
          idat=0
          do j=1,ir-1               
            idat = idat + nowner(j+1) - nowner(j) 
            if(iressw(j).ne.0) 
     1        ir1 = ir1 + nowner(j+1) - nowner(j) + 1
          end do
        endif
c
c rrb 03/08/96; Allow a reservoir to be turned off
        if(iressw(ir).eq.0) then
          idat = idat + nowner(ir+1) - nowner(ir)
          goto 180      
        endif
c
        c = float(ir)/float(numres)*100.0
c       write(6,110) ir, numres, c
        call flush(6)
c
c              Print Header
        ida0 = 0
  120   ip1 = ip1 + 1                 

        ir1=ir1+1
c
c               Get Owner name
        if(ida0.eq.0) then
          ownnamx='Total       '
          c = volmax(ir)
        else                  
          idat = idat+1
          ownnamx=ownnam(idat)
          c = ownmax(idat)
        endif
c
c               Print header
c       write(6,*) '  Dayreso befor headin nf = ', nf 
          write(nf,190) cunitd,
     1                  headin1(1), headin1(2),ip1,
     1                  cresid(ir), resnam1(ir),
     1                  ida0, c, ownnamx, 
     1                  stanam1(is)
c
c              Print water right info for Total reservoir only
        maxwrx=maxwr
c        
c rrb 2007/01/17; Print summary of rights only              
c        
c       if(ida0.eq.0) call outwr(maxwrx, 2, is, ir, nf)
        if(ida0.eq.0) call outwr2(maxwrx, 2, is, ir, nf)
c
        do iy=iystr,iyend

          do im=1,12
c
c              Print title every month
c           write(6,*) '  Dayreso befor year nf=', nf
            write(nf,200) (i, i=1,nresY)
            call year(iy, iyrmo, imomo, cyr1)

            iox=0
            do i=1,nres
              dat2t(i) = 0.0
            end do
            dat2t(ntar) = -1.0
            dat2t(none) = -1.0
c
            do id=1,mthday(im)

              fday=1.0
              if(iresop.eq.1 .or. iresop.eq.4) fday=float(mthday(im))

              iox=iox+1
              if(iox.ge.6) then
                write(nf,*) ' '
                iox=1
              endif

c             cx = cu
c
c               Daily Model capabiity
c             if(iresop.ne.1) cx=cu*mthday(im)
c

c             irecr=((iy-iystr0)*12+(im-1))*nrsactx+ir1+numtop
c             irecs=((iy-iystr0)*12+(im-1))*numsta*31 + 
c    1                (id-1)*numsta+is+numtop
              irecr=((iy-iystr0)*12+(im-1))*nrsactx*31 + 
     1                (id-1)*nrsactx+ir1+numtop

c
c -----------------------------------------------------------------------------
cx              if(id.eq.1) then
cx                write(nlog,*) ' DayResO; ', ir, ir1, 
cx     1           iyrmo(im), xmonam(im), id, irecr, cresid(ir)
cx              endif

 100          format(
     1          '  dayreso;   iy   im   id  ir1  ip1 irecr',/,
     1          '  dayreso;',20i5)
              read(50,rec=irecr) (dat2(i),i=1,nres)
                     
              ida  = dat2(nidr) 
              ida0 = ida + 1
              nacc = dat2(naccX)
c
c               Annual total, except for initial and ending stroage
              do i=1,nresX
                dat2(i)=dat2(i)*fdy(im)       
                
cr              if(i.eq.1 .or. i.eq.14 .or. i.eq.15 .or. i.eq.16) then
                if(i.eq.nbom .or. i.eq.neva .or. 
     1             i.eq.neom .or. i.eq.ntar .or. i.eq.none) then
                else
                  dat2t(i) = dat2t(i) + dat2(i)/fday
                endif
              end do
         
              if(id.eq.1) dat2t(1) = dat2(1)
              if(id.eq.mthday(im)) dat2t(14) = dat2(14)
c 
c               Print total station output
              if(isigfig.eq.0) then
                write(nf,210) cresid(ir), ida, iyrmo(im), xmonam(im),
     1                        id, (dat2(j), j=1,nresY)
              else
                write(nf,212) cresid(ir), ida, iyrmo(im), xmonam(im),
     1                        id, (dat2(j), j=1,nresY)
              endif
c
c               End day loop
              end do
c
c               Print monthly total
              write(nf,160)
              if(isigfig.eq.0) then
                write(nf,210) cresid(ir), ida, iyrmo(13), xmonam(13), 
     1                      -1, (dat2t(j), j=1,nresY)
              else
                write(nf,212) cresid(ir), ida, iyrmo(13), xmonam(13), 
     1                      -1, (dat2t(j), j=1,nresY)
              endif
c
c               End Month Loop      
          end do
c
c rrb 01/15/95; Annual Total
c
c               End Year Loop
        end do
c
c               Account Loop
c       write(io99,*) ' '
c       write(io99,*) '  dayreso; nacc, ida', nacc, ida
        if(nacc-ida.gt.1) goto 120       
c
c               End Reservoir Loop
  180 continue

c
c        Formats
c __________________________________________________________
  110   format('+', '   Printing Reservoir Summary',i5,' of ', i5,
     1              '; or ',f8.0, ' % Complete', i5)
  160   format('___________  ____ ____ ____ ___', 23(' _______'))
  190 FORMAT('',/,'    Reservoir Summary ',a5,/,3x,a80,/,
     1       3X,A80,53X, 'PAGE NO. ',I3,//,
     1          '    RESERVOIR ID              : ',a12,/,
     1          '    RESERVOIR NAME            : ',a24,/,
     1          '    RESERVOIR ACCOUNT & AMOUNT: ',i2, 1x, f8.0,
     1              '; where account 0 is the total',/
     1          '    RESERVOIR OWNER           : ',a12,/, 
     1          '    RIVER LOCATION            : ',a24,/) 
  200   format(// 
     1  '                                                        ',
     1  '                                                    ',
     1  'From Storage to'
     1  58x, ' Station Balance',/
     1  '                                                 From River',
     1  ' by              From Carrier by             ___________',
     1  '____________                                ',
     1  ' Targt_0     BOM ', 39('_'),/
     1  '                               ',
     1  ' Initial _______________________________',
     1  ' _______________________   Total',
     1  '   River   River Carrier   Total          Seep &     EOM',
c    1  '  Stor_n  Decree   River   Total   Total   River   River',/
     1  '  Stor_n  Decree   River   River   River   River   River',/
     1  'Reservoir                      ',
     1  ' Storage Priorty Storage Exc_Pln    Loss Priorty Sto_Exc',
     1  '    Loss  Supply',
     1  ' For Use For Exc for Use Release    Evap   Spill Content',
c    1  '   Limit   Limit  Inflow Release  Supply by Well Outflow',/
     1  '   Limit   Limit  Inflow Release  Divert by Well Outflow',/
     1  'ID            Acc Year   Mo Day      NA'
     1  3('     (+)'), '     (-)', 2('     (+)'), '     (-)',
     1  '      NA',
     1  3('     (-)'), '      NA',
     1  2('     (-)'), '      NA      NA      NA',
     1  2('     (+)'),  2('     (-)'), '      NA',/,
     1  '                               ', 23('    (',i2,')'),/
     1  '___________  ____ ____ ____ ___', 23(' _______'))

  210  format(a12,2i5, 2x, a3, i4, 30f8.0)
  212  format(a12,2i5, 2x, a3, i4, 30f8.1)

  220  return
       END
