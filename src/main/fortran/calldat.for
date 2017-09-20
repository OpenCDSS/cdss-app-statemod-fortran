

c *********************************************************
c
      subroutine CallDat(itype, l1, l2, icallx, ishort, fx)
c      
c
c _________________________________________________________
c	Program Description
c
c  		CallDat; it prints call data at every iteration 
c               if requested from the control file
c
c		Called from Execut if icall from *.ctl is = 1
c
c
c _________________________________________________________
c	Documentation
c
c             itype = -1 print header once per simulation
c    		      itype = 0 print header every month
c             icallx = switch to print the first time a right
c                        is called per month only.
c             ccall = water right id to print from datinp
c
c _________________________________________________________
c	Update History
c
c		NA
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
      
      nchkx=nlog
      iout=0
c      
c _________________________________________________________
c
c		Print header once per simulation
      if(itype.eq.-1) then
        write(nchkx,100) ccall
        goto 500
      endif  
c      
c _________________________________________________________
c
c		Print header once per month
      if(itype.eq.0) then
        write(nchkx,110) ccall
        goto 500
      endif  
c
c _________________________________________________________
c               Print call data for a direct diversion
	    if(l1.eq.3 .and. crigid(l2).eq.ccall) then
	      icallx=icallx+1
	      ND  =IDIVCO(1,L2)
	      ISCD=IDVSTA(ND)
	      NDNS=NDNNOD(ISCD)
        call dnmfso(maxsta,avail,idncod,iscd,ndns,imcd)	      
        bypass=amax1(river(iscd)-avail(imcd), 0.0)

        if(iout.eq.1) then
          write(nchkx,*) ' CallDat; cridid(l2), ccall', 
     1      l2, crigid(l2), ccall
        endif
	      
	      if(idivsw(nd).eq.0 .or. idvrsw(l2).eq.0) then	      
		      write(nchkx,324)icallx,iyrmo(mon), xmonam(mon), idy,
     1              crigid(l2), iwx, 0.0,
     1              river(iscd)*fx, avail(imcd)*fx, bypass*fx,     
     1              'Diversion    '
		      goto 500
	      endif


	      if(icall.gt.1 .and. icallx.eq. 1) write(io99,*) ' '
	      if (avail(iscd).lt.(.01)) then
		      if(ishort.eq.0) then
		        write(nchkx,325)icallx,iyrmo(mon), xmonam(mon), idy,
     1                crigid(l2), iwx, divmon(nd)*fx,
     1                river(iscd)*fx, avail(imcd)*fx, bypass*fx,          
     1                'Diversion    '
          
		      else
		        write(nchkx,326)icallx,iyrmo(mon), xmonam(mon), idy,
     1                crigid(l2), iwx, divmon(nd)*fx,
     1                river(iscd)*fx, avail(imcd)*fx, bypass*fx,          
     1                'Diversion    '
		      endif
	      else
		      if(avail(imcd).le.(.01)) then                           		
		        write(nchkx,327)icallx, iyrmo(mon), xmonam(mon), idy,
     1                crigid(l2), iwx, divmon(nd)*fx,
     1                river(iscd)*fx, avail(imcd)*fx, bypass*fx,          
     1                'Diversion    ',
     1                cstaid(imcd),stanam1(imcd)
		      else
		        write(nchkx,325)icallx, iyrmo(mon), xmonam(mon), idy,
     1                 crigid(l2), iwx, divmon(nd)*fx,
     1                 river(iscd)*fx, avail(imcd)*fx, bypass*fx,          
     1                 'Diversion    '
		      endif
	      endif
	    endif


c
c _________________________________________________________
c               Print call data for an instream flow
c jhb 2014/07/07 array index checking
        if(l2.ge.1 .and. l2.le.241) then
	    if(l1.eq.1 .and. cisfwr(l2).eq.ccall) then
	      icallx=icallx+1
	      nf  = iifrco(L2)
	      ISCD=ifrsta(nf)
	      NDNS=NDNNOD(ISCD)
              call dnmfso(maxsta,avail,idncod,iscd,ndns,imcd)	      
              bypass=amax1(avail(imcd)-river(iscd), 0.0)
	      
	      if(ifrrsw(nf).eq.0 .or. iifrsw(l2).eq.0) then	        
		      write(nchkx,324)icallx, iyrmo(mon), xmonam(mon), idy, 
     1            cisfwr(l2), iwx, qdiv(14,iscd)*fx,
     1            river(iscd)*fx, avail(imcd)*fx, bypass*fx,
     1            'ISF         '
		      goto 500
	      endif


	      if(icall.gt.1 .and. icallx.eq. 1) write(io99,*) ' '

	      
	      if (avail(iscd).lt.(.01)) then
		      if(ishort.eq.0) then
                  write(nchkx,325)icallx,iyrmo(mon), xmonam(mon), idy,
     1              cisfwr(l2), iwx, qdiv(14,iscd)*fx,
     1              river(iscd)*fx, avail(imcd)*fx, bypass*fx,          
     1              'ISF         '
		      else
		       write(nchkx,326)icallx,iyrmo(mon), xmonam(mon), idy,
     1              cisfwr(l2), iwx, qdiv(14,iscd)*fx,
     1              river(iscd)*fx, avail(imcd)*fx, bypass*fx,          
     1              'ISF         '
		      endif
	      else
		      if (avail(imcd).le.(.01)) then
		        write(nchkx,327)icallx, iyrmo(mon), xmonam(mon), idy,
     1              cisfwr(l2), iwx, qdiv(14,iscd)*fx,
     1              river(iscd)*fx, avail(imcd)*fx, bypass*fx,          
     1              'ISF         ',
     1              cstaid(imcd),stanam1(imcd)

		      else
		         write(nchkx,325)icallx,iyrmo(mon), xmonam(mon), idy,
     1              cisfwr(l2), iwx, qdiv(14,iscd)*fx, 
     1              river(iscd)*fx, avail(imcd)*fx, bypass*fx,          
     1              'ISF         '
		      endif
	      endif
	    endif
	    endif


c

c _________________________________________________________
c               Print call data for a reservoir right
c               Note concept of ishort not applied to resrvoirs
c           write(io99,*) '  Execut; l1, l2, creswr ccall', 
c    1        l1, l2, cresid(l2), ccall

c jhb 2014/07/07 array index checking
        if(l2.ge.1 .and. l2.le.351) then
	    if(l1.eq.2 .and. creswr(l2).eq.ccall) then 
c             write(nchkx,*) '  Execut; l1, ccall', l1, ccall
	      icallx=icallx+1
	      nr  =iresco(1,L2)
	      ISCD=irssta(nr)
	      NDNS=NDNNOD(ISCD)
              call dnmfso(maxsta,avail,idncod,iscd,ndns,imcd)	      
              bypass=amax1(avail(imcd)-river(iscd), 0.0)	      	        
	      
	      if(iressw(nr).eq.0) goto 500
	      if(iressw(nr).eq.0 .or. irsrsw(l2).eq.0) then
	        write(nchkx,324)icallx,iyrmo(mon), xmonam(mon), idy,
     1            creswr(l2), iwx, 0.0,
     1            river(iscd)*fx, avail(imcd)*fx, bypass*fx,     
     1            'Reservoir   '
		      goto 500
	      endif


	      if(icall.gt.1 .and. icallx.eq. 1) write(io99,*) ' '
	      if (avail(iscd).lt.(.01)) then
		      write(nchkx,326)icallx, iyrmo(mon), xmonam(mon), idy,
     1            creswr(l2), iwx, qres(1,nr),
     1            river(iscd)*fx, avail(imcd)*fx, bypass*fx,          
     1            'Reservoir   '
	      else
		      if (avail(imcd).le.(.01)) then
		        write(nchkx,327)icallx,iyrmo(mon), xmonam(mon), idy,
     1              creswr(l2), iwx, qres(1,nr), 
     1              river(iscd)*fx, avail(imcd)*fx, bypass*fx,          
     1              'Reservoir   ',
     1              cstaid(imcd),stanam1(imcd)
		      else
		        write(nchkx,325)icallx, iyrmo(mon), xmonam(mon), idy,
     1              creswr(l2), iwx, qres(1,nr), 
     1              river(iscd)*fx, avail(imcd)*fx, bypass*fx,          
     1              'Reservoir   '
		      endif
	      endif
	    endif
	    endif
c
c _________________________________________________________
c		Formats	    
 100  format('#',/,
     1 '#  Execut; FYI Detailed call information requested ',
     1          'at right ',a12,/
     1 '#  The following are noted:',/
     1 '#    1. This report assumes a diversion is not limited',
     1     ' by a physical or legal constraint at the structure',/
     1 '#    2. A call is estimated to occur when a diversion is',
     1     ' limited by the available flow at the structure',/
     1 '#       itself or downstream.',/
     1 '#    3. The calling structure is estimated to be the',
     1     ' first location downstream of the diversion.',/
     1 '#       where flow is limiting. Other locations with',
     1     ' limiting flow may occur downstream.',/
     1 '#    4. Outflow is the amount of water physically passing',
     1     ' the diverting structure.',/
     1 '#    5. Avail is the minimum amout of water available ',
     1     ' downstream of the structure. Note the minimum may',/
     1 '#    be associated with a diversion and/or natural',
     1     ' stream losses',/
     1 '#    6. Bypass is the estimated flow required to pass ',
     1      ' downstream. It is calulated as max (outflow-avail,0).',/     
     1 '#    7. Day = 1 for a monthly model',/
     1 '#')
 110  format(/,72('_'),/
     1 '#',/
     1 '# CallDat; Call data for a right ',a12,/,
     1 '#',/,
     1 '#', 9x, '    # Year  Mon  Day Right        Iter   Divert af',
     1 '  Outflow af    Avail af   Bypass_af ',
     1 ' Str_Type     Sta_ID       Sta_Name                ',
     1 ' Comment',/
     1 '#', 9x, ' ____ ____ ____ ____ ____________ ____',
     1 4(' ___________'), 2(1x,' ___________'), 2x,24('_'),
     1 1x,25('_'))
     
 324  format(10x, 2i5, 1x,a4, i5,1x, a12, i5, 4f12.2, 1x,2(1x,a12),
     1  1x,a24,
     1  ' Not called out. The structure or right is not on')
     
 325  format(10x, 2i5, 1x,a4, i5, 1x, a12, i5, 4f12.2, 1x,2(1x,a12),
     1  1x,a24,' Not called out and not shorted')
     
 326  format(10x, 2i5, 1x,a4, i5,1x, a12, i5, 4f12.2, 1x,2(1x,a12),
     1  1x,a24,' Not called out but shorted due to a flow',
     1  ' limitation at the structure itself')
     
 327  format(10x, 2i5, 1x,a4, i5,1x, a12, i5, 4f12.2, 1x,2(1x,a12),
     1  1x,a24,' Called out and limited by flow at station ') 
     

c _________________________________________________________
 500        return 
            end
