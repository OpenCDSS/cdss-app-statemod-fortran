
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
c
c *********************************************************
c
      SUBROUTINE Outpltr(igui, istop, cplot)
c
c
c _________________________________________________________
c	Program Description
c
c       Outpltr; It generates a matrix tables of reservoirs by account
c
c _________________________________________________________
c       Documentation
c                             
c              iplot = n reservoir ID to plot
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
      character cplot*12
c
c _________________________________________________________
c		Step 1; Intitlize
c

      write(6,*) ' Subroutine Outplr'
      write(6,*) ' '
c                                 
c     nres = 26
c
c		Set number of output values 1x in StateM.for
c	        and store in common.inc      
      ndiv = ndivO        
      nres = nresO
      ndivw= nwelO

      
      nrsactx=nrsact+numown
C
C-------------------------------------------------------------------
C
C------  Reservoir Summary with accounts
C
C-------------------------------------------------------------------
C
c
c              Get ID for a plot
      ir1 = 0
      do 100 ir=1,numres             
	if(cresid(ir).eq.cplot) goto 120
	if(iressw(ir).ne.0) ir1 = ir1 + nowner(ir+1) - nowner(ir) + 1
  100 continue

      write(9,110) cplot
  110 format('  Outplr: Reservoir id ', a12, ' not found')
      goto 200
c
c               Check if a reservoir is turned off
  120 if(iressw(ir).eq.0) then
	write(99,*) '  Outplr; Reservoir ID ', cplot, ' is turned off'
	goto 200      
      endif
		
  130 ir1=ir1+1
c
c              Print title
      write(9,180) iystr, iyend, cunitm2, cyr1
c
      do 170 iy=iystr,iyend
	call year(iy, iyrmo, imomo, cyr1)
c
c rrb 01/15/95 Initilize annual total
c               Note 15 = target, 16 = one fill limit
	do 140 i=1,nres
	  dat2t(i) = 0.0
  140   continue
	dat2t(15) = -1.0
	dat2t(16) = -1.0

	do 160 im=1,12
c         cx = cu         
c         if(iresop.ne.1) cx=cu*mthday(im)
c
	  irecr=((iy-iystr0)*12+(im-1))*nrsactx+ir1+numtop
	  read(44,rec=irecr) (dat2(i),i=1,nres)
		     
	  ida  = dat2(nres-1) 
	  nacc = dat2(nres)
c
c               Annual total, except for initial and ending stroage
	  do 150 i=1,nres-2
c           dat2(i)=dat2(i)*cx           
	    dat2(i)=dat2(i)*fmo(im)           
	    
	    if(i.eq.1 .or. i.eq.14 .or. i.eq.15 .or. i.eq.16) goto 150
	    dat2t(i) = dat2t(i) + dat2(i)
  150     continue          
	  if(im.eq.1) dat2t(1) = dat2(1)
	  if(im.eq.12) dat2t(14) = dat2(14)
c 
c               Print total station output
	  write(9,190)   cresid(ir), ida, iyrmo(im), xmonam(im), 
     1                   (dat2(j), j=1,nres-3)
c
c               End Month Loop      
  160   continue
c
c               End Year Loop
  170 continue
c
c        Account Loop
	if(nacc-ida.gt.1) goto 130       
c
c
c _________________________________________________________
c
c
c        Formats
c 180   format(       
  180   format(i5,',', i5,',',a5,',', a5,',',/       
     1  '            ,     ,     ,     ,',
     1  '        ,        ,        ,        ,        ,        ,',
     1  '        ,     Fro,m Storag,e to    ,        ,        ,',
     1  '        ,        ,        ,        ,        ,     Sta,',
     1  'tion  Da,ta      ,        ,',/
     1  '            ,     ,     ,     ,',
     1  '        ,        , From River, by     ,        ',
     1  ' From Ca,rrier by,    ',
     1  '    ,     _______,________,________,        ,        ,',
     1  '        ,        , Targt_0,     BOM, _______,________,'
     1  '________,________,________,',/
     1  'Reservoir   ,     ,     ,     ,',
     1  ' Initial, _______,________,________, _______,________,',
     1  '   Total,   River,   River, Carrier,   Total,        ,',
     1  '  Seep &,     EOM,  Stor_n,  Decree,   River,   Total,',
     1  '   Total,   River,   River,',/
     1  'ID          ,  Acc, Year,   Mo,',
     1  ' Storage, Priorty, Storage, Exc_Pln,    Loss, Priorty,',
     1  ' Sto_Exc,    Loss,',
     1  '  Supply, For Use, For Exc, for Use, Release,    Evap,',
     1  '   Spill, Content,   Limit,   Limit,  Inflow, Release,',
     1  '  Supply, by Well, Outflow,',/
     1  '____________, ____, ____, ____,', 23(' _______,'))

  190  format(a12,',', 2(i5,','), 2x, a3, ',', 30(f8.0,','))
       return
c
c
c _________________________________________________________
c

c               Error Warning
  200 write(6,210) 
      write(99,220) 
      call flush(6)
  210 format('    Stopped in Outplr',/,
     1       '    See the *.log file')
  220 format('    Stopped in Outplr')
      if(igui.eq.0) then
	write(6,*) 'Stop 1' 
	call flush(6)
        call exit(1)

	stop 
      else
	istop=2
	return
      endif
c
c _________________________________________________________
c

      END
