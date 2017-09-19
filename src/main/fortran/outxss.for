C
c *********************************************************
      subroutine Outxss
c
c
c _________________________________________________________
c	Program Description
c
c       Outxss; It prints a Structure Summary Output (*.xss)
c               Note:
c               Reads binary output (file *.b67) from outmon.f
c               Prints to file 40
c
c _________________________________________________________
c	Update History
c
c rrb 2007/09/06; Revised format with acreage data		
c
c _________________________________________________________
c       Documentation
c               iplot  = 0 not plotting
c               iplot  = n diversion, instream or gage ID to plot
c               nid    = # of ID's requested
c               nstr   = # of data values in the binary file
c
c               nout   = output file # (40)
c
c               ioutx  = 0, no *.out file provided (print all)
c                      = 1, a *.out file provided see idallx 
c               idallx = 0, print all
c                      = 1, print ID's provided in *.out
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
      character  cdx*12,   ida0*12, ftype*24, ptype*24, cname1*24
c
c _________________________________________________________
c               Step 1; Print to Screen
c
      iout=0
      write(6,101) 'OutXss  '
      if(ichk.eq.94) write(nlog,101) 'OutXss  '
 101  format(/,72('_'),/'  Subroutine ', a8)
      call flush(6)
c
c _________________________________________________________
c               Step 2; Initilize      
c
c rrb 2007/09/06; Revised format
c     nss = 31
      nss = 33
      nout = 40
      iw=0

      small=0.001
      
c
c rrb 2007/10/05; Basin Total      
      do im=1,13
        do i=1,nss
          dum(im,i) = 0.0
        end do
      end do      
c
c               Set Unit Conversion
c     cu=1.0
c     cunit='(cfs)'
c     if(iresop.eq.2) then
c       fac=factor
c       cunit=' (af)'
c     endif
c     if(iresop.eq.3) then
c       fac=factor*0.001                        
c       cunit='(kaf)'
c     endif
c
c _________________________________________________________
c               Step 3; Print top of file

      call outtop(nout,1,40)
c
c _________________________________________________________
c               Step 4; Get requested ID's
      idallx=0
      if(ioutx.eq.1) then
        call getin(7, iid, maxsta, idallx, idtype, ftype, ptype, idreq) 
c       write(io99,*) '  Outxss; iid, idallx ',  iid, idallx
      endif
c
c _________________________________________________________
c               Step 5; Set id screen switch
c               Note numdxw = # of diversions and wells
      if(idallx.eq.0) then
        nid=numdxw
      else
        nid=iid
      endif
c _________________________________________________________
c               Step 6; Set loop constants
c
c     write(99,*) '  Outxss; idallx, nid = ', idallx, nid
c                      
      ida0 = '0           '
      ip1 = 0                                         
      ix = 25
c
c _________________________________________________________
c               Step 7; Process Structures (Diversions and Well Only)
      do 190 ip=1,nid
c
c               Set page counter
        ip1=ip1+1
c
c _________________________________________________________
c               Step 9; Print Output every 25 structures
        ix = ix+1
        c = float(ip)/float(numdxw)*100.0

        if(numdivw.ge.25) then
          if(ix.ge.25 .or. iw.eq.nid) then
            ix = 0
            write(6,260) iw, numdxw, c  
            call flush(6)
          endif
        else
          write(6,260) iw, numdxw, c  
          call flush(6)
        endif
c
c _________________________________________________________
c               Step 10; If processing all records read
c               top of Binary File to get header data
c

        if(idallx.eq.0) then
          iw=ip

          irecs=numdxw+iw+numtop
          if(iout.eq.1) write(io99,*) '  Outxss; irecs = ', irecs
          
          read(67,rec=irecs,err=280) (dat1(i),i=1,nss)
          if(iout.eq.1) write(io99,*) '  Outxss; 1 ', (dat1(i),i=1,nss)
c  
          nwx=int(dat1(nss))
          ndx=int(dat1(nss-1))
          idx=int(dat1(nss-2))
c         write(io99,*) '  Outxss; nwx, ndx, idx', nwx, ndx, idx
        endif
c
c _________________________________________________________
c               Step x; Selected output
c
        if(idallx.eq.1) then
c
c               Find a diversion
          call getid(-3,is,iw,iw2,idreq(ip))
          if(is.gt.0) then
            ndx=iw
            idx=2
          else
c
c rrb 01/04/24; Search on well only lands (type 7)
c           call getid(6,is,iw,iw2,idreq(ip))
            call getid(7,is,iw,iw2,idreq(ip))   
            if(is.le.0) goto 190
            nwx=iw
            idx=3
            iw=numdiv+iw2
c
c               Skip if user requested a D&W by specifying a Wel ID
            if(idivcow2(nwx).gt.0) then
              write(io99,282) cdividw(nwx)
              goto 190
            endif

          endif
        endif
c       write(io99,*) '  Outxss ip, ndx, nwx, iw', ip, ndx, nwx, iw

c
c _________________________________________________________
c               Step 11; Set ID and Name 
c
c               11a; Diversion or D&W
        if(idx.le.2) then

          is=idvsta(ndx)
          cdx = cdivid(ndx)           
            cname1=divnam1(ndx)
        else
c
c _________________________________________________________
c               11b; Well only

          is=idvstaw(nwx)
          cdx= cdividw(nwx)
            cname1=divnamw1(nwx)
        endif
c
c _________________________________________________________
c               Step 12; Print header
        write(nout,210) cunitm,
     1    headin1(1), HEADIN1(2),ip1,
     1    cdx,ida0, cname1
c
c
c _________________________________________________________
c               Step 13; Print water right info
        maxwrx=maxwr
        if(idx.le.2) then
c        
c rrb 2007/01/17; Print summary of rights only              
c         call outwr(maxwrx, 1, is, iw, nout)
          call outwr2(maxwrx, 1, is, ndx, nout)
        else
c        
c rrb 2007/01/17; Print summary of rights only              
c         call outwr(maxwrx, 6, is, iw,  nout)
          call outwr2(maxwrx, 6, is, nwx, nout)
        endif
c
c _________________________________________________________
c               Step 15; Loop for every year
        do 180 iy=iystr,iyend           
c
c
c _________________________________________________________
c               Step 15; Print title card every year
          write(nout,100) (i, i=1,30)

          call year(iy, iyrmo, imomo, cyr1)
          do i=1,nss
            dat1t(i) = 0.0
          end do
c
c _________________________________________________________
c               Step 16; Loop for every month
c
          do im=1,12
c
c
c _________________________________________________________
c               Step 17; Read monthly data

            irecs=((iy-iystr0)*12+(im-1))*numdxw+iw+numtop
c           write(io99,*) '  Outxss; iy, im, numtop, iw,irecs = '  
c           write(io99,*) '  Outxss;', iy, im, numtop, iw,irecs             
            read(67,rec=irecs,err=280) (dat1(i),i=1,nss)
c           write(io99,*) '  Outxss 2;', (dat1(i),i=1,nss)
c
c _________________________________________________________
c               Step 18; Convert units and total

            do i=1,nss-3
c             dat1(i)=dat1(i)*cx
              dat1(i)=dat1(i)*fmo(im) 
c
c rrb 01/04/03; Adjust annual total based on units 
c               Note ftot=1.0 for af output and 1/12 for cfs output
c             dat1t(i) = dat1t(i) + dat1(i) 
              dat1t(i) = dat1t(i) + dat1(i)*ftot
c
c rrb 2007/10/05; Add Basin Total
              dum(im,i)=dum(im,i) + dat1(i)
              dum(13,i)=dum(13,i) + dat1(i)	              
            end do
c
c               Do not total acres (1-5), efficiency data (8, 9, 11),  
c               capacity (19) and soil storage (25).  
c               Note actual efficiency is calculated as an annual 
c               value below
            do i=1,5
              dat1t(i)=dat1(i)
c
c rrb 2007/10/05; Add Basin Total
              dum(im,i)=dat1(i)
              dum(13,i)=dat1(i)	                            
            end do
            
            dat1t(8) =dat1(8)
            dat1t(9) =dat1(9)
            dat1t(11) =dat1(11)
            dat1t(19)=dat1(19)              
            dat1t(25)=dat1(25)
c
c rrb 2007/10/05; Add Basin Total
            dum(im,8) =dat1(8)
            dum(im,9) =dat1(9)
            dum(im,11)=dat1(11)
            dum(im,19)=dat1(19)              
            dum(im,25)=dat1(25)

            dum(13,8) =dat1(8)
            dum(13,9) =dat1(9)
            dum(13,11)=dat1(11)
            dum(13,19)=dat1(19)              
            dum(13,25)=dat1(25)
c
c _________________________________________________________
c               Step 19; Print station data
            if(isigfig.eq.0) then
              write(nout,110)   cdx, iyrmo(im), xmonam(im),
     1                        (dat1(j), j=1,nss-3)
            else
              write(nout,112)   cdx, iyrmo(im), xmonam(im),
     1                        (dat1(j), j=1,nss-3)
            endif
          end do
c
c _________________________________________________________
c               Step 20; Print Annual total
c
c               Recalculate actual efficiency data for SW
c                SWeff (17) = SWCU (13) + ToSoil (14) - Fr soil(27)
c
c ---------------------------------------------------------
c rrb 2007/09/06; New format
cx          if(dat1t(6).gt.small) then   
cx            dat1t(14)=(dat1t(10) + dat1t(11) - dat1t(26))  
cx     1                /dat1t(6)*100.0
cx          else
cx            dat1t(14) = 0.0
cx          endif
          
          if(dat1t(10).gt.small) then   
            dat1t(17)=(dat1t(13) + dat1t(14) - dat1t(27))  
     1                /dat1t(10)*100.0
          else
            dat1t(17) = 0.0
          endif
c
c ---------------------------------------------------------
c rrb 2007/09/06; New format
c               Recalculate actual efficiency data for GW
c                GWeff (23) = GWCU (19) + ToSoil (20)
cx          if(dat1t(15).gt.small) then
cx            dat1t(23) = (dat1t(19) + dat1t(20)) / dat1t(15) * 100.
cx          else
cx            dat1t(23)=0.0
cx          endif
          
          if(dat1t(18).gt.small) then
            dat1t(24) = (dat1t(20) + dat1t(21)) / dat1t(18) * 100.
          else
            dat1t(24)=0.0
          endif

          write(nout,120)
          if(isigfig.eq.0) then
            write(nout,110)   cdx, iyrmo(13), xmonam(13), 
     1                      (dat1t(j), j=1,nss-3)
          else
            write(nout,112)   cdx, iyrmo(13), xmonam(13), 
     1                      (dat1t(j), j=1,nss-3)
          endif

          write(nout,*) ' '
c
c
c _________________________________________________________
c               Step 21; End Year Loop      
  180   continue
c
c
c _________________________________________________________
c               Step 22; End Structure  Loop
  190 continue
c
c		Print Monthly Basin Total  
      do im=1,12
      end do
          if(dum(13,10).gt.small) then   
            dum(13,17)=(dum(13,13) + dum(13,14) - dum(13,27))  
     1                /dum(13,10)*100.0
          else
            dum(13,17) = 0.0
          endif
c
c ---------------------------------------------------------
          
          if(dum(13,18).gt.small) then
            dum(13,24) = (dum(13,20) + dum(13,21)) / dum(13,18) * 100.
          else
            dum(13,24)=0.0
          endif

          write(nout,120)
          if(isigfig.eq.0) then
            write(nout,110)   cdx, iyrmo(13), xmonam(13), 
     1                      (dum(13,j), j=1,nss-3)
          else
            write(nout,112)   cdx, iyrmo(13), xmonam(13), 
     1                      (dum(13,j), j=1,nss-3)
          endif
      
      
c
c _________________________________________________________
c               Step 23; Return

      return
c
c
c _________________________________________________________
c               Formats
cx 100  format(
cx     1'                                   Area          ',
cx     1'     Demand                                  Surface Water',      
cx     2'      ',
cx     1'                                              Ground Water',   
cx     1'            ',
cx     1'                                    Consumptive Use     ',/
cx     1'                        _______________________ _______',
cx     1'________',
cx     1'  _____________________________________________________',
cx     1'_________________ ',
cx     1'_______________________________________________________',
cx     1'________________',
cx     1'    Soil _______________________   Total',/
cx     1'ID            Year  Mon',  
cx     1'  Sprink      GW   Total',
cx     1'   Total      CU  Divert  ConEff ConLoss  MaxEff',   
cx     1'   To CU To Soil  Return',
cx     1'    Loss  ActEff Pumping Capacty',  
cx     1'  FldEff  SprEff   To CU To Soil',
cx     1'  Return    Loss  ActEff Storage   SW&GW    Soil   Total',
cx     1'  Return',/
cx     1  23x,28('      NA'),/
cx     1  23x, 28('    (', i2,')'),/
cx     1'____________  ____ ____', 28(' _______')) 
     
 100  format(
     1'                                          Area          ',
     1'             Demand      Max Efficiency      ',
     1'                      Surface Water          ',
     1'                                    Ground Water    ',
     1'                              Consumptive Use     ',/
     1'                        _______________________________________',
     1' _______________ _______________',
     1'  _____________________________________________',
     1'_________________ ',
     1'_______________________________________________________',
     1'    Soil _______________________   Total     IWR',/
     1'ID            Year  Mon',  
c    1'  Sprink      GW   Total',
     1'  SW Fld  SW Spr  GW Fld  GW Spr   Total',
c    1'   Total      CU',
     1'   Total      CU  FldEff  SprEff',
c    1'  Divert  ConEff ConLoss  MaxEff',   
     1'  Divert  ConEff ConLoss',
     1'   To CU To Soil  Return',
     1'    Loss  ActEff Pumping Capacty',  
c    1'  FldEff  SprEff   To CU To Soil',
     1                '   To CU To Soil',
     1'  Return    Loss  ActEff Storage   SW&GW    Soil   Total',
     1'  Return   Short',/
     1  23x,30('      NA'),/
     1  23x, 30('    (', i2,')'),/
     1'____________  ____ ____', 30(' _______')) 

  110 format(a12, 1x, i5, 2x, a3, 30f8.0)
  112 format(a12, 1x, i5, 2x, a3, 30f8.1)
  120 format('____________', 2(' ____'), 30(' _______'))

  200 format(2('___________ '), 2(' ____'), 10(' _______'))
  210 FORMAT('',/, '   Structure Summary ',a5,/,2x,a80,/,
     1  2X,a80,33X, 'PAGE NO. ',I3,//,
     1  '    STRUCTURE ID (0 _ total)  : ',a12,/,
     1  '    STRUCTURE ACCT (0 _ total): ',a12,/,
     1  '    STRUCTURE NAME            : ',a24,/)
 
  260   format('+', '   Printing Structure Summary (*.xss)',
     1        i5,' of ', i5, '; or ',f8.0, ' % Complete')
c
c
c _________________________________________________________
c               Error messages
  280  write(99,*)
     1  '   Outxss; Problem binary file size exceeded'
  282  format(
     1  '  Outxss; Warning requested output for well ID ', a12,
     1     ' but it is a D&W structure.',/
     1  '  To fix use D&W ID but for now this ID has been skipped')
c
c               Error Warning
  290 write(6,300) 
      write(99,310) 
      call flush(6)
  300 format('    Stopped in Outxss',/,
     1       '    See the *.log file')
  310 format('    Stopped in Outxss')
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

c _________________________________________________________

      stop 
      END


