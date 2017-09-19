c
c *********************************************************
c
       subroutine outcu(maxwx,maxgrpx,nreach)
c
c
c _________________________________________________________
c	Program Description
c
c       Outcu; It Prints average cu data
c               Code to read and print total cu (-xcu) to 33
c               Code to read and print total shortage (-xsh) to 34
c               Code to read and print total supply (-xsu) to 35
c               Code to calculate and print total by WD (-xwd) to 37
c
c _________________________________________________________
c       Documentation
c               maxwx = max number of well diversions (maxdivw)
c               maxgrpx = max # of Cu groups (maxgrp)
c
c               ndiv = # of values in binary diversion output
c               nsup = location of total supply for -xsh
c               ncu  = location of consumptive use for -xcu
c               nsht = location of shortage for -xsh
c               dum  = average annual shortage (shorta)
c               dum2 = average annual supply (suplya)
c               maxwd = maximum # of water district groups
c
c		dumng = CU by group (water district)
c		dumngS = Short by group (water district)
c		dumngD = Diversion by group (water district)
c
c rrb 99/12/24; New Id convention as follows:
c                               0-5000 = diversion
c                               5001 - 7500 = ISF
c                               7501 - 10000 = reservoir
c                               < -10000 = baseflow only
c                               -1* above = baseflow plus a structure 
c
c _________________________________________________________
c	Dimensions
c
       include 'common.inc'
       dimension cu(13),     cut(13),
     1           short(13),  shortt(13),
     1           supply(13), suplyt(13)
c
       dimension
     1           dumave(13,maxgrpx),  dummin(13,maxgrpx),
     1           dummax(13,maxgrpx),
     1           dumaveD(13,maxgrpx), dumaveS(13,maxgrpx),
     1           iymin(maxgrpx),      iymax(maxgrpx),
     1           dumng(13,maxgrpx),   dumngS(13,maxgrpx),
     1           dumngD(13,maxgrpx),
     1           cgroup(maxgrpx)
c
       dimension inegw(maxwx), dumw(13,maxwx), dum2w(13,maxwx),
     1           dum3w(13,maxwx)

       CHARACTER idx*2, cdivid2*12, cgroup*2, rec48*48, RchidRX*12
c
c _________________________________________________________
c
c
c		iout=1 print ID information
       iout=2       
       if(iout.ge.1) write(nlog,300)
c
c		Initilize a reach that may not be found
c rrb 2008/12/24; Update reach processing

cx       nreachX=nreachD
cx       RchidDR(nreachD+1) = 'Reach_NA    '
       nreachX=nreach
       
       
       ncu  = 16
       nsht = 14
       nsup = 13
       
cx     nrid = 34
cx     nxstr= 35

       nrid = ndivO-3
       nxstr= ndivO-2

c      ndivw= 18
       ncuw = 9
       nshtw= 7
       nsupw= 3
       maxwd = maxgrpx
c
c		Set number of output values 1x in StateM.for
c	        and store in common.inc      
      ndiv = ndivO        
      nres = nresO
      ndivw= nwelO
      
      ndivP=29
      ndivT=31
      
       
c
c               Check local dimensions
       nd1 = numdiv+numdivw+1
       if(nd1.gt.maxdiv) goto 260

       nd2=502
       if(nd2.lt.numdivw) goto 262 
c               Set desired units
c      fac=1.0
c      if(iresop.eq.2)  fac=factor
c      if(iresop.eq.3)  fac=factor*0.001                        
c
c               Initilize average annual arrays
      do n=1,numdiv+1
        do im=1,13
          dum(im,n) = 0.0
          dum2(im,n) = 0.0
          dum3(im,n) = 0.0
        end do
      end do
c
c rrb 99/01/23; Initilize Wells
      do n=1,numdivw+1
        do im=1,13
          dumw(im,n) = 0.0
          dum2w(im,n)=0.0
          dum3w(im,n)=0.0
        end do
      end do
c
c rrb 07/08/97; Initilize ave, max and min group arrays
      do n=1,maxwd
        iymax(n) = 0
        iymin(n) = 0
      
        do im=1,13
          dumave(im,n) = 0.0
          dummax(im,n) = 0.0
          dummin(im,n) = 99999999.
          dumaveS(im,n)= 0.0
          dumaveD(im,n)= 0.0
        end do
      end do
c
c Initilize ineg and inegw
        do i=1,numdiv
          ineg(i)=0
        end do
        
        do i=1,numdivw
          inegw(i)=0
        end do
c
c rrb 20100205; Initilize diversion Groups        
      	do n=1,maxgrpx	
      	  do im=1,13	     
      	    dumng(im,n) = 0.0
      	    dumngS(im,n) = 0.0
      	    dumngD(im,n) = 0.0
      	  end do
        end do        
c
c                       Find diversion groups
    	ng = 0
    	do nd=1,numdiv
	      IF(ineg(nd).eq.0) then
	       cdivid2 = cdivid(nd)
	       idx = cdivid2(1:2)
	       ng=ng+1
	       cgroup(ng) = idx
        
	       do nd2=nd,numdiv
	         cdivid2 = cdivid(nd2)
	         if(ineg(nd2).eq.0 .and. idx.eq. cdivid2(1:2)) THEN
		         ineg(nd2)=ng
	         endif
	       end do
c
c rrb; 99/01/23; Find wells in diversion groups
	        do nd2=1,numdivw
	          cdivid2 = cdividw(nd2)
	          if(inegw(nd2).eq.0 .and. idx.eq. cdivid2(1:2)) THEN
		          inegw(nd2)=ng
	          endif
	        end do
	      endif
	    end do
c
c                       Scan wells for diversion groups
	    do nd=1,numdivw
	     IF(inegw(nd).eq.0) then
	       cdivid2 = cdividw(nd)
	       idx = cdivid2(1:2)
	       ng=ng+1
	       cgroup(ng) = idx
     
	       do nd2=nd,numdivw
	         cdivid2 = cdividw(nd2)
	         if(inegw(nd2).eq.0 .and. idx.eq. cdivid2(1:2)) THEN
		         inegw(nd2)=ng
	         endif
	       end do
	      endif
	    end do

      NUMgrp=ng
     	ng1=ng+1
     	IF(ng1.gt.maxwd) GOTO 280
 
    	do im=1,13
    	  dummin(im,ng1) = 0.       
    	end do
c 
c
      if(numdiv.eq.0) then
      	 write(6,*)  'No active diversions for the current job'
      	 write(33,*) 'No active diversions for the current job'
      	 write(34,*) 'No active diversions for the current job'
      	 write(37,*) 'No active diversions for the current job'
      	 goto 240
      else
      	 call outtop(33,0,10)
      	 call outtop(34,0,14)                  
      	 call outtop(35,0, 5)	 
      	 
      	 write(37,350) 'Consumptive Use by Reach        '
      	 call outtop(37,0,28)
      		       
      	 write(6,*) ' '
      endif        
c 
       do 190 iy=iystr, iyend
       	 write(6,130) iy
  130    format(
     1   '+', '   Printing CU, Shortage & Supply Data for Year ', i5)
c
	    do 140 im=1,13
	      cut(im)    = 0.0
	      shortt(im) = 0.0
	      suplyt(im) = 0.0
c
c rrb 20100205; Correction	   
cx	   do ig=1,ng
	       do ig=1,ng1	   
	         dumng(im,ig) = 0.0
	         dumngS(im,ig) = 0.0
	         dumngD(im,ig) = 0.0
	       end do
  140  continue
c
c _________________________________________________________
c		Process Diversions
	    do 170 ndx=1,numdiv
c
c ---------------------------------------------------------
c rrb 2007/01/24; Set Ditch Reach ID (RchidWR)
cx           iz=nreachD+1
cx           do ix=1,nreachD
cx             if(RchidDR(ix).eq. RchidD(ndx)) then
cx               iz=ix
cx               if(iout.eq.1) then
cx                  write(nlog,*) ' OutCU; ', 
cx     1            cdivid(ndx), ndx, iz, RchidDR(iz) 
cx               endif  
cx             endif  
cx           end do   
cx           if(iz.gt.nreachD) nreachX=nreachD+1          
c
c rrb 2008/12/24; Update to reach processing
        is=idvsta(ndx)
        iz=irch(is)           
	 
c
c ---------------------------------------------------------
c                       Skip if not an annual diversion
c rrb 00/02/01; Include all structures
c          if(idvcom(ndx).ne.1) goto 170
	      is = idvsta(ndx)
	      cu(13) = 0.0
	      short(13) = 0.0
	      supply(13) = 0.0
c
c ---------------------------------------------------------
	      do 150 im=1,12
	        irecs=((iy-iystr0)*12+(im-1))*numsta+is+numtop
	        read(43,rec=irecs,err=250) (dat1(i), i=1,ndiv)
c       
c                        Process diversions only
c                          nd .eq. 0 = gauge location
c                          na .gt. 1 = multiple diversion location
	        nd = dat1(nrid)
	        na = dat1(nxstr) 
c 
c rrb 00/01/24; New ID convention (see top)
c            if(nd.le.0) goto 170
	        ndid=iabs(nd)
         
	        if(ndid.gt.5000) goto 170
	        if(na.gt.1) goto 170
c
c                       Change units as appropriate
c            cx = fac         
c            if(iresop.ne.1) cx=fac*mthday(im)
c
c                       Store the cu
c        cu(im)     = dat1(ncu)  * cx
	       cu(im)     = dat1(ncu)  * fmo(im)
        
	       cu(13)     = cu(13)   + cu(im)*ftot
c
c rrb 01/04/03; Add ftot for cfs output
c               note in datinp ftot = 1.0 for af and 1/12 for cfs output
	       cut(im)      = cut(im)      + cu(im)    
	       cut(13)      = cut(13)      + cu(im)*ftot
        
	       dum3(im,ndx) = dum3(im,ndx) + cu(im)
	       dum3(13,ndx) = dum3(13,ndx) + cu(im)*ftot   
        
	       dum3(im,nd1) = dum3(im,nd1) + cu(im)
	       dum3(13,nd1) = dum3(13,nd1) + cu(im)*ftot   
c
c                       Store the shortage
c            short(im)  = dat1(nsht) * cx
	       short(im)  = dat1(nsht) * fmo(im)
         
	       short(13)  = short(13) + short(im)*ftot
         
	       shortt(im)  = shortt(im)  + short(im)
	       shortt(13)  = shortt(13)  + short(im)*ftot
         
	       dum(im,ndx) = dum(im,ndx) + short(im)
	       dum(13,ndx) = dum(13,ndx) + short(im)*ftot
         
	       dum(im,nd1) = dum(im,nd1) + short(im)
	       dum(13,nd1) = dum(13,nd1) + short(im)*ftot
c
c                       Store the supply
c            supply(im)   = dat1(nsup) * cx
	       supply(im)   = dat1(nsup) * fmo(im)
         
	       supply(13)   = supply(13)   + supply(im)*ftot
         
	       suplyt(im)   = suplyt(im)   + supply(im)
	       suplyt(13)   = suplyt(13)   + supply(im)*ftot
         
	       dum2(im,ndx) = dum2(im,ndx) + supply(im)
	       dum2(13,ndx) = dum2(13,ndx) + supply(im)*ftot
         
	       dum2(im,nd1) = dum2(im,nd1) + supply(im)
	       dum2(13,nd1) = dum2(13,nd1) + supply(im)*ftot
c
c ---------------------------------------------------------
c rrb 07/08/97;        Store data by group
c rrb 2007/01/24;
c	       ig=ineg(ndx)
         ig=iz
	       
	       dumng(im,ig) = dumng(im,ig) + cu(im)
	       dumng(13,ig) = dumng(13,ig) + cu(im)*ftot
	       
	       dumngS(im,ig) = dumngS(im,ig) + short(im)
	       dumngS(13,ig) = dumngS(13,ig) + short(im)*ftot
	       
	       dumngD(im,ig) = dumngD(im,ig) + supply(im)
	       dumngD(13,ig) = dumngD(13,ig) + supply(im)*ftot
c
c                      End Month Loop
  150  continue
c
c
c ---------------------------------------------------------
c                      Print one year of data for a given station
	     cp = (supply(13) + short(13))
	     if(cp.gt.0.001) then
	       csh = short(13) / cp * 100.0
	       csu = supply(13) / cp * 100.0
	       itype = 1
	     else
	       csh =   0.0
	       csu = 100.0
	       itype = 0
	     endif
c          if(iout.eq.1) write(io99,*) '  Outcu; ndid, is', ndid, is
c	   
c ---------------------------------------------------------
	   
	     if(isigfig.eq.0) then
	       write(33,160) iy, cdivid(ndid), (cu(im), im=1,13),     
     1           csu, divnam1(ndid), itype, demsrc(ndid)
	       write(34,164) iy, cdivid(ndid), (short(im), im=1,13), 
     1           csh, stanam1(is)
	       write(35,164) iy, cdivid(ndid), (supply(im), im=1,13), 
     1           csu, stanam1(is)
             else
	       write(33,162) iy, cdivid(ndid), (cu(im), im=1,13),     
     1           csu, divnam1(ndid), itype, demsrc(ndid)
	       write(34,166) iy, cdivid(ndid), (short(im), im=1,13), 
     1           csh, stanam1(is)
	       write(35,166) iy, cdivid(ndid), (supply(im), im=1,13), 
     1         csu, stanam1(is)
        endif
c
c                      End Diversion Loop
  170 continue
c
c _________________________________________________________
c		Well Processing
c                       Note if tied to a structure (idivcow2.ne.0)
c                       then cu is reported under the strucure
	    do nw=1,numdivw
c             nwx=numdiv+nw
	      if(idivcow2(nw).eq.0) then
c
c rrb 2008/12/24; Revise reach approach	   
cxc
cxc ---------------------------------------------------------
cxc rrb 2007/01/31; Tie a Well ID to a Diversion Reach
cx
cx             iz=nreachD+1
cx             do ix=1,nreachD
cx               if(RchidDR(ix).eq. RchidW(nw)) then
cx                 iz=ix
cx                 if(iout.eq.1) then
cx                    write(nlog,*) ' OutCU; ', 
cx     1              cdividW(nw), ndx, iz, RchidWR(iz) 
cx                 endif  
cx               endif  
cx             end do    
cx             if(iz.gt.nreachD) nreachX=nreachD+1                                
            is=idvstaw(nw)
            iz=irch(is)	   
	     cu(13) = 0.0
	     short(13) = 0.0
	     supply(13) = 0.0
	     is=idvstaw(nw)
	     ig=inegw(nw)
c
	     do im=1,12
c
c rrb 12/12/95; Variable report year and date stamp
	       irecs=((iy-iystr0)*12+(im-1))*numdivw+nw+numtop
	       read(42,rec=irecs,err=250) (dat1(i), i=1,ndivw)
c
c                       Change units as appropriate
c              cx = fac         
c              if(iresop.ne.1) cx=fac*mthday(im)
c
c                       Store the cu
c              cu(im)     = dat1(ncuw)  * cx
	       cu(im)     = dat1(ncuw)  * fmo(im)

	       cu(13)     = cu(13)   + cu(im)*ftot

	       cut(im)      = cut(im)       + cu(im)    
	       cut(13)      = cut(13)       + cu(im)*ftot    
	       
	       dum3w(im,nw)  = dum3w(im,nw) + cu(im)   
	       dum3w(13,nw)  = dum3w(13,nw) + cu(im)*ftot   

	       dum3(im,nd1)  = dum3(im,nd1) + cu(im)   
	       dum3(13,nd1)  = dum3(13,nd1) + cu(im)*ftot
c
c                       Store the shortage
c              short(im)   = dat1(nshtw) * cx
	       short(im)   = dat1(nshtw) * fmo(im)

	       short(13)   = short(13)   + short(im)*ftot

	       shortt(im)   = shortt(im)   + short(im)
	       shortt(13)   = shortt(13)   + short(im)*ftot

	       dumw(im,nw)  = dumw(im,nw)  + short(im)
	       dumw(13,nw)  = dumw(13,nw)  + short(im)*ftot

	       dum(im,nd1)  = dum(im,nd1)  + short(im)
	       dum(13,nd1)  = dum(13,nd1)  + short(im)*ftot
c
c                       Store the supply
c              supply(im)   = dat1(nsupw) * cx
	       supply(im)   = dat1(nsupw) * fmo(im) 
	       supply(13)   = supply(13)   + supply(im)*ftot

	       suplyt(im)    = suplyt(im)    + supply(im)
	       suplyt(13)    = suplyt(13)    + supply(im)*ftot

	       dum2w(im,nw)  = dum2w(im,nw)  + supply(im)
	       dum2w(13,nw)  = dum2w(13,nw)  + supply(im)*ftot

	       dum2(im,nd1)  = dum2(im,nd1)  + supply(im)
	       dum2(13,nd1)  = dum2(13,nd1)  + supply(im)*ftot
c
c rrb 07/08/97;        Store data by group
	       dumng(im,ig) = dumng(im,ig) + cu(im)
	       dumng(13,ig) = dumng(13,ig) + cu(im)*ftot
	     
               dumngS(im,ig) = dumngS(im,ig) + short(im)
	       dumngS(13,ig) = dumngS(13,ig) + short(im)*ftot
	     
	       dumngD(im,ig) = dumngD(im,ig) + supply(im)
	       dumngD(13,ig) = dumngD(13,ig) + supply(im)*ftot
	       
c
c                      End Month Loop
	     end do
c
c                      Print one year of data for a given well station
	     cp = (supply(13) + short(13))
	     if(cp.gt.0.001) then
	       csh = short(13) / cp * 100.0
	       csu = supply(13) / cp * 100.0
	       itype = 1
	     else
	       csh =   0.0
	       csu = 100.0
	       itype = 0
	     endif
             if(isigfig.eq.0) then
	       write(33,160) iy, cdividw(nw), (cu(im), im=1,13),     
     1           csu, divnamw1(nw), itype, demsrcw(nw)
	       write(34,164) iy, cdividw(nw), (short(im), im=1,13), 
     1           csh, stanam1(is)
	       write(35,164) iy, cdividw(nw), (supply(im), im=1,13), 
     1           csu, stanam1(is)
             else
	       write(33,162) iy, cdividw(nw), (cu(im), im=1,13),     
     1           csu, divnamw1(nw), itype, demsrcw(nw)
	       write(34,166) iy, cdividw(nw), (short(im), im=1,13), 
     1           csh, stanam1(is)
	       write(35,166) iy, cdividw(nw), (supply(im), im=1,13), 
     1           csu, stanam1(is)
             endif
c
c                        End Well IF
	      endif
c                        End Well Loop
	    end do
c
c                       Print total for shortage & supply (skip CU)
	    cp =  (suplyt(13) + shortt(13)) 
	    if(cp.gt.0.001) then
	      csh = shortt(13) / cp * 100.
	      csu = suplyt(13) / cp * 100.
	    else
	      csh =   0.0
	      csu = 100.0
	    endif
c
c rrb 01/10/96; Skip annual data for CU in order to insure no impact
c               to cu model
c rrb 99/01/23; Added back in
c rrb 2006/06/14; Allow more sig figs
         if(isigfig.eq.0) then
           write(33,180) iy, (cut(im),    im=1,13), csu
	        write(34,180) iy, (shortt(im), im=1,13), csh
	        write(35,180) iy, (suplyt(im), im=1,13), csu
	      else
          write(33,182) iy, (cut(im),    im=1,13), csu
	        write(34,182) iy, (shortt(im), im=1,13), csh
	        write(35,182) iy, (suplyt(im), im=1,13), csu
	      endif  
c
c rrb 07/08/97;        Store ave, min and max by group
     	 do ig=1,ng
     	   do im=1,13
     	     dumave(im,ig) = dumave(im,ig) + dumng(im,ig)
     	     dumaveS(im,ig)= dumaveS(im,ig)+ dumngS(im,ig)
     	     dumaveD(im,ig)= dumaveD(im,ig)+ dumngD(im,ig)
c
c rrb test
           if(im.eq.1 .and. ig.eq.1 .and. dumave(im,ig).gt.0.01) then
             write(nlog,*) 'Outcu; im, ig, dumave',im,ig,dumave(im,ig)
           endif 	     
	      end do

	      IF(dumng(13,ig).LT. dummin(13,ig)) then
	        iymin(ig) = iy
	        do im=1,13
	          dummin(im,ig) = dumng(im,ig)
	        end do
	      endif
        
	      IF(dumng(13,ig).GT. dummax(13,ig)) then
	        iymax(ig) = iy
	        do im=1,13
	          dummax(im,ig) = dumng(im,ig)
	        end do
	      endif
	    end do
c
c                      End Year Loop
  190  continue   
c
c _________________________________________________________
c
c
c                      Print Average Annual for cu, shortage & supply
     	write(33,200) iystr, iyend
     	write(34,200) iystr, iyend
     	write(35,200) iystr, iyend
 200  format(//,' Average from ',2i5,/)
     
     	ry = iyend-iystr+1
c
c ---------------------------------------------------------
c                     Print Diversion Average Annual 

 	    do 220 nd=1,numdiv
	       is = idvsta(nd)
	       cp = (dum2(13,nd) + dum(13,nd))
	       if(cp.gt.0.001) then
	         csh = dum(13,nd) / cp * 100.
	         csu = dum2(13,nd) / cp * 100.
	       else
	         csh =   0.0
	         csu = 100.0
	       endif
           
         if(isigfig.eq.0) then
	        write(33,210) cdivid(nd), (dum3(im,nd)/ry, im=1,13), 
     1                        csu, stanam1(is)          
	        write(34,210) cdivid(nd), (dum(im,nd)/ry, im=1,13), 
     1                      csh, stanam1(is)          
	        write(35,210) cdivid(nd), (dum2(im,nd)/ry, im=1,13), 
     1                        csu, stanam1(is)          
              else
	        write(33,212) cdivid(nd), (dum3(im,nd)/ry, im=1,13), 
     1                        csu, stanam1(is)          
	        write(34,212) cdivid(nd), (dum(im,nd)/ry, im=1,13), 
     1                      csh, stanam1(is)          
	        write(35,212) cdivid(nd), (dum2(im,nd)/ry, im=1,13), 
     1                     csu, stanam1(is)          
         endif
  220   continue       
c
c
c ---------------------------------------------------------
c                     Print Well Average Annual 
	do nw=1,numdivw
c
c		Process if well only (idivcow2(nw) = 0
	    if(idivcow2(nw).eq.0) then
	      is = idvstaw(nw)
	      cp = (dum2w(13,nw) + dumw(13,nw))
	      if(cp.gt.0.001) then
	        csh = dumw(13,nw) / cp * 100.
	        csu = dum2w(13,nw) / cp * 100.
	      else
	        csh =   0.0
	        csu = 100.0
	      endif
            
            if(isigfig.eq.0) then
	      write(33,210) cdividw(nw), (dum3w(im,nw)/ry, im=1,13), 
     1                      csu, stanam1(is)          
	      write(34,210) cdividw(nw), (dumw(im,nw)/ry, im=1,13), 
     1                      csh, stanam1(is)          
	      write(35,210) cdividw(nw), (dum2w(im,nw)/ry, im=1,13), 
     1                      csu, stanam1(is)
             else
	      write(33,212) cdividw(nw), (dum3w(im,nw)/ry, im=1,13), 
     1                      csu, stanam1(is)          
	      write(34,212) cdividw(nw), (dumw(im,nw)/ry, im=1,13), 
     1                      csh, stanam1(is)          
	      write(35,212) cdividw(nw), (dum2w(im,nw)/ry, im=1,13), 
     1                      csu, stanam1(is)
             endif
	      endif
	    end do       
c
c _________________________________________________________
c
c
c               Print Average Total

       cp = (dum2(13,nd1) + dum(13,nd1))
       if(cp.gt.0.001) then
	       csh = dum(13,nd1) / cp * 100.
	       csu = dum2(13,nd1) / cp * 100.
       else
	       csh =   0.0
	       csu = 100.0
       endif
c
       if(isigfig.eq.0) then
         write(33,230) (dum3(im,nd1)/ry, im=1,13), csu
         write(34,230) (dum(im,nd1)/ry,  im=1,13), csh
         write(35,230) (dum2(im,nd1)/ry, im=1,13), csu
       else
         write(33,232) (dum3(im,nd1)/ry, im=1,13), csu
         write(34,232) (dum(im,nd1)/ry,  im=1,13), csh
         write(35,232) (dum2(im,nd1)/ry, im=1,13), csu
       endif  
c
c _________________________________________________________
c
c
c rrb 07/08/97;        PRINT group (water district) data
       ng=nreachX       
       do ig=1,ng
	       do im=1,13
	         dumave(im,ng1) = dumave(im,ng1) + dumave(im,ig)
	         dummin(im,ng1) = dummin(im,ng1) + dummin(im,ig)
	         dummax(im,ng1) = dummax(im,ng1) + dummax(im,ig)
	       end do
c
c rrb 2008/12/24; Update to reach data	 
cx         if(iz.eq.1) then
cx           RchIdRX=RchidDR(ig)
cx         else
cx           RchIdRX=RchidWR(ig)
cx         endif 
         RchidRX=Rchid(iz) 
	 
c
         if(isigfig.eq.0) then
cx	   write(37,310) 'Ave ',cgroup(ig), 'Ave ',
cx     1                   (dumave(im,ig)/ry, im=1,13)
cx	   WRITE(37,314) 'Min ',cgroup(ig), iymin(ig), 
cx     1                   (dummin(im,ig), im=1,13)
cx	     WRITE(37,314) 'Max ',cgroup(ig), iymax(ig), 
cx     1                   (dummax(im,ig), im=1,13)
c
	      write(37,310) 'Ave ',RchIdRX,
     1                   (dumave(im,ig)/ry, im=1,13)     
c	      WRITE(37,314) 'Min ',RchIdRX,
c    1                   (dummin(im,ig), im=1,13)
c	      WRITE(37,314) 'Max ',RchIdRX,
c    1                   (dummax(im,ig), im=1,13)
c	      WRITE(37,*) ' '
	    else 
	      write(37,312) 'Ave ',RchIdRX,
     1                   (dumave(im,ig)/ry, im=1,13)
c	      WRITE(37,316) 'Min ',RchIDRX, 
c     1                   (dummin(im,ig), im=1,13)
c	      WRITE(37,316) 'Max ',RchidRX, 
c     1                   (dummax(im,ig), im=1,13)
c	      WRITE(37,*) ' '	 
	    endif
c
c		End Reach (group) loop	 
       end do
c
c		Total
       write(37,322)
       if(isigfig.eq.0) then
         write(37,330) 'Ave ', (dumave(im,ng1)/ry, im=1,13)
c         write(37,330) 'Min ', (dummin(im,ng1),    im=1,13)
c         write(37,330) 'Max ', (dummax(im,ng1),    im=1,13)
       else
         write(37,332) 'Ave ', (dumave(im,ng1)/ry, im=1,13)
c         write(37,332) 'Min ', (dummin(im,ng1),    im=1,13)
c         write(37,332) 'Max ', (dummax(im,ng1),    im=1,13)
       endif
c
c _________________________________________________________       
c       	Supply Summary
c
       write(37,350) 'Supply Summary by Reach         '
       call outtop(37,0,52)
       
       ng=nreachX       
       do ig=1,ng
c
c rrb 2008/12/24; Revised reach approach       
cx       RchIdRX=RchidDR(ig)
         RchIdRX=Rchid(ig)
         
	       do im=1,13
	         dumaveD(im,ng1) = dumaveD(im,ng1) + dumaveD(im,ig)
	       end do
c
         if(isigfig.eq.0) then
	         write(37,310) 'Ave ',RchIdRX, 
     1                   (dumaveD(im,ig)/ry, im=1,13)
         else
	         write(37,312) 'Ave ',RchIdRX, 
     1                   (dumaveD(im,ig)/ry, im=1,13)
         endif
c	 WRITE(37,*) ' '
       end do

       write(37,322)
       if(isigfig.eq.0) then
         write(37,330) 'Ave ', (dumaveD(im,ng1)/ry, im=1,13)
       else
         write(37,332) 'Ave ', (dumaveD(im,ng1)/ry, im=1,13)
       endif
       
c
c _________________________________________________________       
c		Shortage by Group (water district)'
       write(37,350) 'Shortage by Reach              '
       call outtop(37,0,53)
       
       ng=nreachX
       do ig=1,ng
c
c rrb 2008/12/24; Update to reach data	 
cx       RchIdRX=RchidDR(ig)
         RchidRX=Rchid(ig)
         
	      do im=1,13
	        dumaveS(im,ng1) = dumaveS(im,ng1) + dumaveS(im,ig)
	      end do
c
         if(isigfig.eq.0) then
	         write(37,310) 'Ave ',RchIdRX, 
     1                   (dumaveS(im,ig)/ry, im=1,13)
         else
	         write(37,312) 'Ave ',RchIdRX, 
     1                   (dumaveS(im,ig)/ry, im=1,13)
         endif
c	 WRITE(37,*) ' '
       end do

       write(37,322)
       if(isigfig.eq.0) then
         write(37,330) 'Ave ', (dumaveS(im,ng1)/ry, im=1,13)
       else
         write(37,332) 'Ave ', (dumaveS(im,ng1)/ry, im=1,13)
       endif
c
c _________________________________________________________
c
c
c               Return
  240  return
c
c
c _________________________________________________________
c
c               Formats
  160      format(i4, 1x, a12, 13f8.0, f8.2, 1x, a24, i8, f8.0)
  162      format(i4, 1x, a12, 13f8.1, f8.2, 1x, a24, i8, f8.1)

  164      format(i4, 1x, a12, 13f8.0, f8.2, 1x, a24, 1x, a24) 
  166      format(i4, 1x, a12, 13f8.1, f8.2, 1x, a24, 1x, a24) 
  
  180  format(
     1 '____ ____________', 14(' _______'),/
     1 i4, ' Total       ', 13f8.0, f8.2/)
     
  182  format(
     1 '____ ____________', 14(' _______'),/
     1 i4, ' Total       ', 13f8.1, f8.2/)
     
  210      format('Ave ', 1x, a12, 13f8.0, f8.2, 1x, a24,1x,a24)
  212      format('Ave ', 1x, a12, 13f8.1, f8.2, 1x, a24,1x,a24)
     
  230  format(
     1 '____ ____________', 14(' _______'),/
     1 'Ave ', ' Total       ', 13f8.0, f8.2/)
     
  232  format(
     1 '____ ____________', 14(' _______'),/
     1 'Ave ', ' Total       ', 13f8.1, f8.2/)
     
  300  format(/,72('_'),/, '  OutCU; CU Report ')

cx310  format(a4, 1x, a2, 1x, a4, 5x, 13f8.0)
c 312  format(a4, 1x, a2, 1x, a4, 5x, 13f8.1)
  310  format(a4, 1x, a12,            13f8.0)
  312  format(a4, 1x, a12,            13f8.1)
  
c 314  format(a4, 1x, a2, 1x, i4, 5x, 13f8.0)
c 316  format(a4, 1x, a2, 1x, i4, 5x, 13f8.1)
  314  format(a4, 1x, a12,            13f8.0)
  316  format(a4, 1x, a12,            13f8.1)
  
  322  format(
     1 '____ ____________', 13(' _______')) 
  330  format(
     1 a4, ' Total       ', 13f8.0)
  332  format(
     1 a4, ' Total       ', 13f8.1)
  350  format('# ', /, '# ', a32)
     
c
c _________________________________________________________
c
c
c               Error Message
  250  write(6,*)  '   Outcu; Requested data exceeds binary file size'
       write(99,*) '   Outcu; Requested data exceeds binary file size'
       goto 9999

  260  write(6,270) maxdiv, nd1
  270  format('  Outcu; Dimension problem for storing average'/,
     1        '         Dimension = ', i5, ' Need = ', i5) 
       GOTO 9999

  262  write(6,272) maxdivw, nd2
  272  format('  Outcu; Dimension problem for wells'/,
     1        '         Dimension = ', i5, ' Need = ', i5) 
       GOTO 9999

c
  280  write(6,290) maxwd, ng1
  290  format('  Outcu; Dimension problem for water district',
     1          'groups',/,
     1        '         Dimension = ', i5,' Need = ', i5)
       goto 9999

 9999 write(6,1440) 
      write(io99,1440) 
      call flush(6)
 1440 format('  Stopped in Outcu, see the log file (*.log)')
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

c
c _________________________________________________________
c

      stop 
      end
