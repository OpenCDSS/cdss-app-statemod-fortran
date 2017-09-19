
c
c _________________________________________________________
c	Update History
c

c
c _________________________________________________________
c	Documentation
c

c
c _________________________________________________________
c	Dimensions
cC     Last change:  C    20 May 97    0:06 am
C
c *********************************************************

      subroutine outpltd(igui, istop, cplot)
c
c
c _________________________________________________________
c	Program Description
c
c       Outpltd; It generates a plot file of diversion, instream or
c              stream gage data
c
c _________________________________________________________
c       Documentation
c              iplot = 0 not plotting
c              iplot = n diversion, instream or gage ID to plot
c
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
      character  cplot*12            
c
c _________________________________________________________
c		Step 1; Initilze
c                           
                           
      write(6,*) ' Subroutine Outpltd'
      write(6,*) ' '             
      call flush(6)
      
c
c rrb 2005/11/29; River and Carrier Loss
c     ndiv = 37
c
c		Set number of output values 1x in StateM.for
c	        and store in common.inc      
      ndiv = ndivO        
      nres = nresO
      ndivw= nwelO

C
C-------------------------------------------------------------------
C
C------  Diversion Summary 
C
C-------------------------------------------------------------------
c
c              Find diversion ID
        do 100 id=1,numdiv
          if(cdivid(id).eq.cplot) then
            is = idvsta(id)
            goto 140
          endif
  100   continue
c
c              Find instream ID
        do 110 id=1,numifr
          if(cifrid(id).eq.cplot) then
            is = ifrsta(id)
            goto 140
          endif
  110   continue
c
c              Find gage ID
        do 120 id=1,numsta
          if(cstaid(id).eq.cplot) then
            is = id
            goto 140
          endif
  120   continue


        write(6,130) cplot
        write(99,130) cplot
  130   format('  Outdiv: Diversion id ', a12, ' not found')
        call flush(6)
        goto 230
c
c              Print title 
  140 write(9,190) iystr, iyend, cunitm2, cyr1
c
c               Get Data
c-------------------------------------------------------------------

      do 180 iy=iystr,iyend
        call year(iy, iyrmo, imomo, cyr1)

        do 150 i=1,ndiv
          dat1t(i) = 0.0
  150   continue

        do 170 im=1,12
c
          irecs=((iy-iystr0)*12+(im-1))*numsta+is+numtop
          read(43,rec=irecs,err=220) (dat1(i),i=1,ndiv)

c         cx = cu         
c         if(iresop.ne.1) cx=cu*mthday(im)
           
          do 160 i=1,ndiv-2
c           dat1(i)=dat1(i)*cx
            dat1(i)=dat1(i)*fmo(im)
            dat1t(i) = dat1t(i) + dat1(i)
  160     continue
c 
c               Print station data for a diversion or gage
          write(9,200) cplot, cstaid(is), iyrmo(im), xmonam(im), 
     1                 (dat1(j), j=1,ndiv-6)
  170   continue
c
c               End Year Loop      
  180   continue
c
c _________________________________________________________
c
c
c        Formats
  190   format(i5,',', i5,',', a5,',',a5,','/
     1  '            ,            ,',
     1  '     ,     ,        ,        ,        ,',
     1  '        ,        ,        ,        ,        ,        ,',
     1  '   WATER, USE    ,        ,        ,        ,'
     1  '   STATI,ON BALAN,CE      ,        ,        .        ,',
     1  '        ,        ,'/

     1  '            ,            ,',
     1  '     ,     ,        ,        ,        , From Ri,ver By  ,',
     1  '            ,        ,    From, Carrier, By    ,        ,',
     1  '        ,',
     1  '        ,        ,        ,',
     1  ' _______,________,________,________,________,________,'
     1  ' _______,________,________,________,________,',
     1  ' _______,________,________,________,________,',/

     1  'Structure   ,River       ,',
     1  '     ,     ,   Total,      CU, _______,________,',
     1  '________,    From, _______,________, Carried,    From,',
     1  '   Total,',
     1  '   Total,      CU,',
     1  '        ,      To,   Total,        ,  Upstrm,   Reach,',
     1  '  Return,',
     1  '    Well,',
     1  ' To/From,   River,   River,   River,   River,   Avail,',/

     1  'ID          ,ID          , Year,   Mo,',
     1  '  Demand,  Demand, Priorty, Storage, Exc_Pln,    Loss,',
     1  '    Well, Priorty, Sto_Exc,    Loss',
     1  '   Water,    Soil,  Supply,   Short,   Short,',
     1  '      CU,    Soil,  Return,    Loss,  Inflow,    Gain,',
     1  '    Flow,',
     1  ' Deplete,',
     1  ' GW Stor,  Inflow,  Divert, by Well, Outflow,    Flow,',/
     1  2('___________ ,'),2(' ____,'), 30(' _______,'))
  200  format(2(a12,','),i5,',', 2x, a3, ',', 30(f8.0,','))
  210  return
  220  write(6,*)  '   Outdiv; Requested data exceeds binary file size'
       write(99,*) '   Outdiv; Requested data exceeds binary file size'
c
c _________________________________________________________
c
c
c               Error Warning
  230 write(6,240) 
      write(99,250) 
      call flush(6)
  240 format('    Stopped in Outpltd',/,
     1       '    See the *.log file')
  250 format('    Stopped in Outpltd')
      if(igui.eq.0) then
        write(6,*) 'Stop 1' 
        call flush(6)
        call exit(1)

        stop 
      else
        stop 2
        return
      endif
c
c _________________________________________________________
c
      
      END
