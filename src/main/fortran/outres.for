c
c *********************************************************
c
      SUBROUTINE OUTres
c
c
c _________________________________________________________
c	Program Description
c
c       Outres; It prints reservoir data *.xre by reservoir then account
c
c _________________________________________________________
c       Documentation
c
c               ir   = reservoir counter (active or inactive)
c               idat = reservoir account counter (active or inactive)
c               ir1  = reservoir account+1 (active) e.g. a reservoir
c                      with 2 accounts will have 3 ir values 1=total 
c                      and 2 for each account
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
      character ownnamx*12, ftype*24, ptype*24
c
c _________________________________________________________
c
c     	iout =	1 details
c		2 summary
      iout=0
      write(6,101) 'OutRes  '
      write(nlog,101) 'OutRes  '
 101  format(/,72('_'),/'  Subroutine ', a8)
c     
c		Set number of output values 1x in StateM.for
c	        and store in common.inc      
      ndiv = ndivO        
      nres = nresO
      ndivw= nwelO
      
      if(nresO.ne.29) then
        write(nlog,900) 'Reservoirs', 29, nresO
        goto 9999
      endif
      
c
c rrb 2006/01/21; Columns of data impacted by a new file size		
c               nbom = column of BOM data (1)
c		nemo = column of EOM data (16)
c		ntar = column of Tartet data (17)
c		none = column of One fill or BOM limit (18)
c		nidr = columns of irdr (account))
c		naccX = columns of nacc ( )
c		nresX = columns to adjust by factor
c               nresY = columns to print  ndivO-6   
      nBom=1 
      nEom=16
      nTar=17
      nOne=18
      
      nidr=nres-2
      naccX=nres-1
c
c rrb Add reservoir seepage loss to right side     
c     nresX=nres-3
c     nresY=nres-4
      nresX=nres-4
c rrb 2--6/10/27; Correction 
      nresX=nres-3      
c
c rrb test      
      nresY=nres-6
      
      nf  = 34
c              
      ip1 = 0
      ir1 = 0
      ir  = 0
      
C
C-------------------------------------------------------------------
C
C------  Reservoir Summary with accounts
C
C-------------------------------------------------------------------
C
      call outtop(nf,1,17)

      nrsactx=nrsact+numown
      if(numres.eq.0 .and. nrsact.eq.0) THEN
        Write(nf,*) ' NO ACTIVE RESERVOIR FOR THE CURRENT JOB'
        Goto 500
      endif
c
c               Get requested ID's
      idallx=0
      if(iout.eq.1) then
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
      idat = 0                     

      do 180 ip=1,nid

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
          do 100 j=1,ir-1               
            idat = idat + nowner(j+1) - nowner(j) 
            if(iressw(j).ne.0) 
     1        ir1 = ir1 + nowner(j+1) - nowner(j) + 1
  100     continue
        endif
c
c rrb 03/08/96; handle a reservoir turned off
        if(iressw(ir).eq.0) then
          idat = idat + nowner(ir+1) - nowner(ir)
          goto 180      
        endif
c
c rrb 10/27/94 Additional Output
        c = float(ir)/float(numres)*100.0
        write(6,110) ir, numres, c
        call flush(6)
  110   format('+', '   Printing Reservoir Summary',i5,' of ', i5,
     1              '; or ',f8.0, ' % Complete', i5)
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
          write(nf,190) cunitm, headin1(1), headin1(2),ip1,
     1                  cresid(ir), resnam1(ir),
     1                  ida0, c, ownnamx, 
     1                  stanam1(is)
c
c              Print water right info for Total reservoir only
        maxwrx=maxwr
c        
c rrb 2007/01/17; Print summary of rights only              
c        
c       if(ida0.eq.0) call outwr(maxwrx,2, is, ir, 34)
        if(ida0.eq.0) call outwr2(maxwrx,2, is, ir, 34)
c
        do 170 iy=iystr,iyend
c
c              Print title every year
c
c rrb 2006/01/20; Additional data at end (nr)
cr        write(nf,200) (i, i=1,nres-3)
          write(nf,200) (i, i=1,nresY)
          write(nf,220)
          call year(iy, iyrmo, imomo, cyr1)
c
c rrb 01/15/95 Initilize annual total
c               Note 15 = target, 16 = one fill limit
c               Note ntar = target, none = one fill limit
          do 130 i=1,nres
            dat2t(i) = 0.0
  130     continue
c
c rrb 2006/01/20; Correction  
c         dat2t(15) = -1.0
c         dat2t(16) = -1.0
          dat2t(nTar) = -1.0
          dat2t(nOne) = -1.0

          do 150 im=1,12
            fac=mthday(im)*factor
          
c
            irecr=((iy-iystr0)*12+(im-1))*nrsactx+ir1+numtop
c           write(nlog,*) ' Outres; irecr = ', irecr
            read(44,rec=irecr) (dat2(i),i=1,nres)
c
c rrb 2006/01/20; New inforamtion at end (nr)                     
c           ida  = dat2(nres-1) 
c           ida0 = ida + 1
c           nacc = dat2(nres)
            
            ida  = dat2(nidr) 
            ida0 = ida + 1
            nacc = dat2(naccX)
            
        if(iout.eq.2) then
c         write(nlog,*) ' '        
          write(nlog,123)
          write(nlog,124) cresid(ir), irecr, ir, nacc, ida,dat2(25)*fac
 123      format(
     1    '  Outres; ID             ',
     1    ' irecr      ir     acc    ridr    Loss')
 124      format(
     1    '          ', a12,1x, 4i8, 20f8.0)
        endif
            

c
c               Annual total, except for initial and ending stroage
c               Note outmon changes all data to cfs (even initial storage)
c
c rrb 2006/01/20; Additional data (nr) at end
c           do 140 i=1,nres-2
            do 140 i=1,nresX
              dat2(i)=dat2(i)*fmo(im)
c
c rrb 2006/01/21; Move column dependent data to top of file              
c             if(i.eq.1 .or. i.eq.14 .or. i.eq.15 .or. i.eq.16) goto 140
              if(i.eq.nBom .or. i.eq.nEom .or. 
     1           i.eq.nTar .or. i.eq.nOne) goto 140
c
c rrb 01/04/03; Adjust annual total based on units 
c               Note ftot=1.0 for af output and 1/12 for cfs output 
              dat2t(i) = dat2t(i) + dat2(i)*ftot

  140       continue          
            if(im.eq.1) dat2t(1) = dat2(1)
c
            if(im.eq.12) dat2t(neom) = dat2(neom)
c 
c _________________________________________________________
c               Print total station output
            if(isigfig.eq.0) then            
              write(nf,210) cresid(ir), ida, iyrmo(im), xmonam(im), 
     1                      (dat2(j), j=1,nresY)
            endif
            
            if(isigfig.eq.1) then            
              write(nf,211) cresid(ir), ida, iyrmo(im), xmonam(im), 
     1                      (dat2(j), j=1,nresY)
            endif
            
            if(isigfig.eq.2) then            
              write(nf,212) cresid(ir), ida, iyrmo(im), xmonam(im), 
     1                      (dat2(j), j=1,nresY)
            endif
c
c               End Month Loop      
  150     continue
c
c _________________________________________________________
c rrb 01/15/95; Annual Total
            write(nf,220)
  
            if(isigfig.eq.0) then              
              write(nf,210) cresid(ir), ida, iyrmo(13), xmonam(13), 
     1                      (dat2t(j), j=1,nresY)
            endif
            
            if(isigfig.eq.1) then                          
              write(nf,211) cresid(ir), ida, iyrmo(13), xmonam(13), 
     1                      (dat2t(j), j=1,nresY)
            endif
            
            if(isigfig.eq.2) then                          
              write(nf,212) cresid(ir), ida, iyrmo(13), xmonam(13), 
     1                      (dat2t(j), j=1,nresY)
            endif
            
            write(nf,*) ' '
c
c               End Year Loop
  170   continue
c
c        Account Loop
        if(iout.eq.2) then
          write(nlog,*) ' Outres; nacc, ida, nacc-ida'
          write(nlog,*) ' Outres;', nacc, ida, nacc-ida
        endif
        
        if(nacc-ida.gt.1) goto 120
               
c
c        End Reservoir Loop
  180 continue
c
c _________________________________________________________
c
c
c        Formats
c _________________________________________________________
  190 FORMAT(/,'   Reservoir Summary ',a5,/,3x,a80,/,
     1       3X,A80,53X, 'PAGE NO. ',I3,//,
     1          '    RESERVOIR ID              : ',a12,/,
     1          '    RESERVOIR NAME            : ',a24,/,
     1          '    RESERVOIR ACCOUNT & AMOUNT: ',i2, 1x, f8.0,
     1              '; where account 0 is the total',/
     1          '    RESERVOIR OWNER           : ',a12,/, 
     1          '    RIVER LOCATION            : ',a24,/) 
  200   format( 
     1  '                                                    ',
     1  '                                                    ',
     1  'From Storage to',
     1  64x, 'Station Balance'/
     1  '                                             From River',
     1  ' by              From Carrier by             ___________',
     1  '____________                                ',
     1  ' Targt_0     BOM ',39('_'),/
     1  '                           ',
     1  ' Initial _______________________________',
     1  ' _______________________   Total',
     1  '   River   River Carrier   Total          Seep &     EOM',
c    1  '  Stor_n  Decree   River   Total   Total   River   River',/
     1  '  Stor_n  Decree   River   River   River   River   River',/
     1  'Reservoir                  ',
     1  ' Storage Priorty Storage   Other    Loss Priorty   Other',
     1  '    Loss  Supply',
     1  ' For Use For Exc for Use Release    Evap   Spill Content',
c    1  '   Limit   Limit  Inflow Release  Supply by Well Outflow',/
     1  '   Limit   Limit  Inflow Release  Divert by Well Outflow',/
     1  'ID            Acc Year   Mo      NA'
     1  3('     (+)'), '     (-)', 2('     (+)'), '     (-)',
     1  '      NA',
     1  3('     (-)'), '      NA',
     1  2('     (-)'), '      NA      NA      NA',
     1  2('     (+)'),  2('     (-)'), '      NA',/,
     1  27x, 23('    (', i2,')'))
  210  format(a12,2i5, 2x, a3, 35f8.0)
  211  format(a12,2i5, 2x, a3, 35f8.1)
  212  format(a12,2i5, 2x, a3, 35f8.2)
  220 format('___________  ____ ____ ____', 23(' _______'))

  900  format(
     1 ' OutRes;  Problem key output variables for ', a10, 
     1 ' were developed',/
     1 10x,'Based on ', i5, ' output variables but ndivO = ', i4,/
     1 10x,'Reconmend you revise OutBal2 appropriately')
  
c
c _________________________________________________________
c

  500 return
 9999 write(6,*)
      write(nlog,*) '  Stopped in OutRes'
      write(6,*) 'Stop 1' 
      call flush(6)
      call exit(1)

      stop 
  
       END
