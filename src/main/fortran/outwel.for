C
c *********************************************************
c
      subroutine outwel
c
c
c _________________________________________________________
c	Program Description
c
c       Outwel; It prints a matrix output of wells
c
c _________________________________________________________
c       Documentation
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
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
c                           
c rrb 05/29/96; change to generalize dimensions
c     dimension  dat1(40), dat1t(40)
      character  cdx*12,   ida0*12, ftype*24, ptype*24, cname1*24
c
c _________________________________________________________
c		Step 1; Initilze
      write(6,101) 'OutWel  '
      write(nlog,101) 'OutWel  '
 101  format(/,72('_'),/'  Subroutine ', a8)
      call flush(6)
c
c		Test reach (zone) balance
      nz=3       
c
c		Set number of output values 1x in StateM.for
c	        and store in common.inc      
      ndiv = ndivO        
      nres = nresO
      ndivw= nwelO
      
      nout = 41
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
C
C-------------------------------------------------------------------
C
C------  Well Summary 
C
C-------------------------------------------------------------------
C
      call outtop(nout,1,33)
c
c               Get requested ID's
      idallx=0
      if(ioutx.eq.1) then
        call getin(6, iid, maxsta, idallx, idtype, ftype, ptype, idreq)
c
c       write(nlog,*) '  Outwel; iid, idallx ',  iid, idallx
      endif
c
c               Set id screen switch
      if(idallx.eq.0) then
        nid=numdivw
      else
        nid=iid
      endif

c     write(nlog,*) '  Outwel; idallx, nid = ', idallx, nid
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
c       write(nlog,*) '  Outwel; is = ', is
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
        read(42,rec=irecs,err=280) (dat1(i),i=1,ndivw)
c
            cdx = cdividw(iw)
            ip1=ip1+1         
            cname1=divnamw1(iw)
c            
c               Print header
        write(nout,210) cunitm,
     1    headin1(1), HEADIN1(2),ip1,
     1    cdx,ida0, cname1 
c
c              Print water right info
        maxwrx=maxwr
c        
c rrb 2007/01/17; Print summary of rights only              
c       call outwr(maxwrx, 6, is, iw, nout)
        call outwr2(maxwrx, 6, is, iw, nout)
c
c               Print title card once per structure
  140   continue

c  
c               Finally year loop
c-------------------------------------------------------------------
c 
        do 180 iy=iystr,iyend           
c
c               Print title card every year
          write(nout,230) (i, i=1,19)

          call year(iy, iyrmo, imomo, cyr1)
          do 150 i=1,ndivw
  150       dat1t(i) = 0.0

          do 170 im=1,12
c
            irecs=((iy-iystr0)*12+(im-1))*numdivw+iw+numtop
            read(42,rec=irecs,err=280) (dat1(i),i=1,ndivw)

c           cx = cu         
c           if(iresop.ne.1) cx=cu*mthday(im)
           
            do i=1,ndivw
c             dat1(i)=dat1(i)*cx
              dat1(i)=dat1(i)*fmo(im)
c
c rrb 01/04/03; Adjust annual total based on units 
c               Note ftot=1.0 for af output and 1/12 for cfs output
c             dat1t(i) = dat1t(i) + dat1(i)
              dat1t(i) = dat1t(i) + dat1(i)*ftot
            end do
c 
c _________________________________________________________
c               Print station data for a diversion or gage            
            if(isigfig.eq.0) then
              write(nout,240)   cdx, cstaid(is), iyrmo(im), xmonam(im),
     1                        (dat1(j), j=1,ndivw)
            endif
            
            if(isigfig.eq.1) then
              write(nout,241)   cdx, cstaid(is), iyrmo(im), xmonam(im),
     1                        (dat1(j), j=1,ndivw)
            endif
            
            if(isigfig.eq.2) then
              write(nout,242)   cdx, cstaid(is), iyrmo(im), xmonam(im),
     1                        (dat1(j), j=1,ndivw)
            endif
  170     continue
c
c _________________________________________________________
c               Print total
          write(nout,200)
          
          if(isigfig.eq.0) then
            write(nout,240)   cdx, cstaid(is), iyrmo(13), xmonam(13), 
     1                      (dat1t(j), j=1,ndivw)
          endif
          
          if(isigfig.eq.1) then
            write(nout,241)   cdx, cstaid(is), iyrmo(13), xmonam(13), 
     1                      (dat1t(j), j=1,ndivw)
          endif
          
          if(isigfig.eq.2) then
            write(nout,242)   cdx, cstaid(is), iyrmo(13), xmonam(13), 
     1                      (dat1t(j), j=1,ndivw)
          endif

          write(nout,*) ' '
c
c               End Year Loop      
  180   continue
c
c               End Diversion Loop
      endif
  190 continue
c
c
c _________________________________________________________
c
c        Formats
c
  200 format(2('___________ '), 2(' ____'), 18(' _______'))
  210 FORMAT('',/, '   Well Water Only Summary ',a5,/,3x,a80,/,
     1  3X,a80,33X, 'PAGE NO. ',I3,//,
     1  '    STRUCTURE ID (0 _ total)  : ',a12,/,
     1  '    STRUCTURE ACCT (0 _ total): ',a12,/,
     1  '    STRUCTURE NAME            : ',a24,/)
 
  230   format(/,                                            
     1  2(12x),2(5x),
     1  '     Demand              Water  Supply          ',
     1  '      Short     ',
     1  '                 Water Use              ',
     1  '               Water Source             ',/

     1  2(12x),2(5x),    
     1  ' _______________ _______________________________',
     1  ' _______________',
     1  ' _______________________________________________',
     1  ' _______________________________________',/

     1  '                                  ',
     1  '   Total      CU    From    From    From   Total',   
     1  '   Total      CU',   
     1  '   Total      To   Total                   Total'
     1  '    From    From    From    From   Total',/

     1  'Structure   River                 ',
     1  '  Demand  Demand    Well      SW    Soil  Supply',   
     1  '   Short   Short',      
     1  '      CU    Soil  Return    Loss Carried     Use',   
     1  '   River  GwStor Salvage    Soil  Source',/

     1  'ID          ID           Year   Mo',
     1  '     N/A     N/A     (+)     (+)     (+)     N/A',
     1  '     N/A     N/A',     
     1  '     (+)     (+)     (+)     (+)     (+)     N/A',
     1  '     (+)     (+)     (+)     (+)     N/A',/
     1  34x, 19('    (', i2,')'),/
     1   2('___________ '), 2(' ____'), 19(' _______'))
  240  format(2a12,i5, 2x, a3, 20f8.0)
  241  format(2a12,i5, 2x, a3, 20f8.1)
  242  format(2a12,i5, 2x, a3, 20f8.2)
  260   format('+', '   Printing Diversion & Stream Summary',
     1        i5,' of ', i5, '; or ',f8.0, ' % Complete')
  270  return
c
c               Error messages
  280  write(nlog,*) '   Outwel;',
     1  ' Requested data exceeds binary file size'
c
c               Error Warning
  290 write(6,300) 
      write(nlog,310) 
      call flush(6)
  300 format('    Stopped in Outwel',/,
     1       '    See the *.log file')
  310 format('    Stopped in Outwel')
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

c
c _________________________________________________________
c

      stop 
      END


