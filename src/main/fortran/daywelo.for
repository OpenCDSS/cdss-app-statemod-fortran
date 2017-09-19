c _________________________________________________________
c	Dimensions
cC     Last change:  C    20 May 97    0:04 am
C
c *********************************************************
      subroutine daywelo
c
c
c _________________________________________________________
c	Program Description
c
c      Daywelo; it prints daily well structure output
c
c rrb 2007/02/21; Add Well Carrier
c
c _________________________________________________________
c      Documentation
c
c              iplot  = 0 not plotting
c              iplot  = n diversion, instream or gage ID to plot
c              nid    = # of ID's requested
c              ndivw  = # of data values in the binary well file
c              nout   = output file #
c
c              ioutx  = 0, no *.out file provided (print all)
c                     = 1, a *.out file provided see idallx 
c              idallx = 0, print all
c                      =1, print some provided in *.out
c

c
c _________________________________________________________
c	Update History
c		NA
c
      include 'common.inc'
c                           
      character  cdx*12,   ida0*12, ftype*24, ptype*24, cname1*24

      write(6,*) ' Subroutine Daywelo'
      write(6,*) ' '             
c     write(io99,*) ' '             
c     write(io99,*) ' Subroutine Daywelo'
      call flush(6)
c
c rrb 01/02/20; Update for new output
c     ndivw = 10
c     ndivw = 18
      nout  = 37
c
c		Set number of output values 1x in StateM.for
c	        and store in common.inc      
      ndiv = ndivO        
      nres = nresO
      ndivw= nwelO
      
      
      nid=numdivw
c
c               Set Unit Conversion
c     cu=1.0
c     cunit='(cfs)'
c     if(iresop.eq.2) then
c       cu=factor
c       cunit=' (af)'
c     endif
c     if(iresop.eq.3) then
c       cu=factor*0.001                        
c       cunit='(kaf)'
c     endif
c
c-------------------------------------------------------------------
C
C------  Well Summary 
C
C-------------------------------------------------------------------
C
      call outtop(nout,1,35)
c
c               Get requested ID's
      idallx=0
      if(ioutx.eq.1) then
        call getin(6, iid, maxsta, idallx, idtype, ftype, ptype, idreq)
c       write(io99,*) '  Daywelo; iid, idallx ',  iid, idallx
      endif
c
c               Set id screen switch
      if(idallx.eq.0) then
        nid=numdivw
      else
        nid=iid
      endif
c
c     write(io99,*) '  Daywelo; idallx, nid = ', idallx, nid
c                      
      ida0 = '0           '
      ip1 = 0                                         
      ix = 25

      do 190 ip=1,nid
      if(idtype(ip).eq.6 .or. idallx.eq.0) then
        if(idallx.eq.0) then
          iw=ip
          is=idvstaw(iw)
        else
          call getid(idtype(ip),is,iw,iw2,idreq(ip))
        endif
c
c       write(io99,*) '  Daywelo; is = ', is
c
c rrb 10/27/94 Print Output every 25 structures
        ix = ix+1
c       c = float(ip)/float(numdivw)*100.0
        c = float(iw)/float(numdivw)*100.0

        if(numdivw.ge.25) then
          if(ix.ge.25 .or. iw.eq.nid) then
            ix = 0
            write(6,260) iw, numdivw, c  
            call flush(6)
          endif
        else
          write(6,260) iw, numdivw, c  
          call flush(6)
        endif
c
c              Get station ID (assumes one per station)
        irecs=numdivw+iw+numtop
        read(65,rec=irecs,err=280) (dat1(i),i=1,ndivw)
c       write(io99,*) '  Daywelo;'
c       write(io99,*) (dat1(i),i=1,ndivw)
c
        cdx = cdividw(iw)
        ip1=ip1+1         
c
        cname1=divnamw1(iw)
c
c               Print header
        write(nout,210) cunitd,
     1    headin1(1), HEADIN1(2),ip1,
     1    cdx,ida0, (cname(i),i=1,6) 
c
c              Print water right info
        maxwrx=maxwr
c        
c rrb 2007/01/17; Print summary of rights only              
c       call outwr(maxwrx, 6, is, iw, nout)
        call outwr2(maxwrx, 6, is, iw, nout)
c
c               Finally year loop
c-------------------------------------------------------------------
        do iy=iystr,iyend           
          call year(iy, iyrmo, imomo, cyr1)

          do im=1,12
c
c               Print title card every month
c rrb 2007/02/21; Add well carrier
            write(nout,230) (i,i=1,19)
            do i=1,ndivw
              dat1t(i) = 0.0
            end do

            iox=0
            do id=1,mthday(im)
              fday=1.0
              if(iresop.eq.1 .or. iresop.eq.4) fday=float(mthday(im))

              iox=iox+1
              if(iox.ge.6) then
                write(nout,*)
                iox=1
              endif
c
              irecs=((iy-iystr0)*12+(im-1))*numdivw*31 +
     1             (id-1)*numdivw+iw+numtop
c             write(io99,*) '  Daywelo; iy, im, id, irecs = ',
c    1          iy, im, id, irecs

              read(65,rec=irecs,err=280) (dat1(i),i=1,ndivw)

c             cx = cu         
           
              do i=1,ndivw
c               dat1(i)=dat1(i)*cx
                dat1(i)=dat1(i)*fdy(im)

                dat1t(i) = dat1t(i) + dat1(i)/fday
              end do
c 
c               Print well station data
              write(nout,240)   cdx, cstaid(is), iyrmo(im), xmonam(im),
     1                          id, (dat1(j), j=1,ndivw)
c
c               End day loop
            end do
c
c               Print monthly total
            write(nout,200)
            write(nout,240) cdx, cstaid(iw), iyrmo(im), xmonam(13),
     1                      -1, (dat1t(j), j=1,ndivw)
            write(nout,*) ' '                    
c
c               End month loop
          end do
c
c               Print annual total
c         write(nout,200)
c         write(nout,240)   cdx, cstaid(is), iyrmo(13), xmonam(13), 
c    1                      -1, (dat1t(j), j=1,ndivw)

          write(nout,*) ' '
c
c               End Year Loop
        end do
c
c               End Diversion Loop
      endif
  190 continue
c
c        Formats
  200 format(2('___________ '), 2(' ____'), ' ___', 19(' _______'))

  210 FORMAT(
     1  '',/, '    Well Water Only Summary - Daily',a5,/,3x,a80,/,
     1  3X,A80,33X, 'PAGE NO. ',I3,//,
     1  '    STRUCTURE ID (0 _ total)  : ',a12,/,
     1  '    STRUCTURE ACCT (0 _ total): ',a12,/,
     1  '    STRUCTURE NAME            : ',a24,/)

  230   format(/,                                            
     1  2(12x),2(5x), 4x,
     1  '     Demand              Water  Supply          ',
     1  '      Short     ',
     1  '                 Water Use              ',
     1  '               Water Source             ',/

     1  2(12x),2(5x),4x,    
     1  ' _______________ _______________________________',
     1  ' _______________',
     1  ' _______________________________________',
     1  ' _______________________________________',/

     1  '                                      ',
     1  '   Total      CU    From    From    From   Total',   
     1  '   Total      CU',   
     1  '   Total      To   Total                   Total'
     1  '    From    From    From    From   Total',/

     1  'Structure   River                     ',
     1  '  Demand  Demand    Well      SW    Soil  Supply',   
     1  '   Short   Short',      
     1  '      CU    Soil  Return    Loss Carried     Use',   
     1  '   River  GwStor Salvage    Soil  Source',/

     1  'ID          ID           Year   Mo Day',
     1  '     N/A     N/A     (+)     (+)     (+)     N/A',
     1  '     N/A     N/A',     
     1  '     (+)     (+)     (+)     (+)     (+)     (+)',
     1  '     N/A',
     1  '     (+)     (+)     (+)     N/A',/
     1  38x, 19('    (', i2,')'),/
     1   2('___________ '), 2(' ____'), ' ___', 18(' _______'))


  240  format(2a12,i5, 2x, a3, i4, 20f8.0)
  250  format( a12,i5, 2x, a3, i4, 20f8.0)
  260   format('+', '   Printing Diversion & Stream Summary',
     1        i5,' of ', i5, '; or ',f8.0, ' % Complete')
  270  return
c
c               Error messages
  280  write(io99,*) 
     1  '   Daywelo; Requested data exceeds binary file size'
c
c               Error Warning
  290 write(6,300) 
      write(io99,310) 

  300 format('    Stopped in Daywelo',/,
     1       '    See the *.log file')
  310 format('    Stopped in Daywelo')
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)


      stop 
      END


 

