c
       Subroutine daydata(ichk1,nf, nfx, nmax, nin, nlog, 
     1          iy1, im1, imd, q, cid)
c
c
c _________________________________________________________
c	Program Description
c
c       Daydata;               
c             It reads daily data ID (cid) and flow (q)
c             Called by dayest
c             Called by dayest
c
c _________________________________________________________
c	Update History
c		NA
c __________________________________________________________
c
c      Documenation
c
c               imd=number of days this month
c               nf = file #
c               nfx = file type (see datat statement)
c               nmax = max array size for variable dimension
c               nin = number of stations, etc
c               nlog = debug file #
c               iy1 = year
c               im1 = month
c               imd = day
c               q(i,j) = value on day i, station j
c               cid = station ID
c
c               iwarn
c
c _________________________________________________________
c	Dimensions
c
      dimension q(32,nmax)
      character cid*12, dtype*45 
      dimension cid(nmax), dtype(12)
      data dtype/
     1     'Daily stream file (*.rid)             file 81',
     1     'Daily demand file (*.ddd)             file 82',
     1     'Daily instream flow file (*.ifd)      file 83',
     1     'Daily well demand data (*.wed)        file 84', 
     1     'Daily reservoir target file (*.tad)   file 85',
     
     1     'Daily reservoir evaporation (*.evd)   file 86',
     1     'Daily delay table data (*.dld)        file 87', 
     1     'Daily historic stream file (*.riy)    file 81',
     1     'Daily historic diversion file (*.ddy) file 82',
     1     'Daily well pumping file (*.wey)       file 84',
     
     1     'Daily consumptive IWR (*.ddy)         file 88',     
     1     'Daily historic reservoir EOD (*.eoy)  file 85'/                                             '/

c
c __________________________________________________________
c
c               Step 1; Initilize
c
c rrb 00/06/22; Allow less daily values than stations
       igot1 = 0
       iwarn=1
       
       
       iout=0
       if(iout.ge.1) ichk1=1
c      nfx=nf-80

       fx=1.9835

       if(ichk1.eq.1) write(nlog,440) dtype(nfx), (i,i=1,31)

       do j=1,nin
c
c __________________________________________________________
c
c               Step 2; Read daily data

	 read(nf,110,end=400,err=400) iy, im, cid(j), (q(i,j), i=1,31)
         if(ichk1.eq.1) write(nlog,110) iy, im, cid(j), (q(i,j), i=1,31)
         cid(j)=adjustl(cid(j))
c
c rrb 99/08/19; Check for a blank read
c rrb 00/06/22; Allow less daily values than stations
c        if(iy.eq.0) goto 400
	 if(iy.eq.0 .and. igot1.ge.1) goto 500
c
c               Check if less data than stations
c               if yes, backspace and reset last read 
	 if(iy.ne.iy1 .or. im.ne.im1) then
	   backspace(nf)
c
c rrb 00/08/05; Reset last read since it is really next month
	   do i=1,31
	     q(i,j) = 0.0
	   end do
c          nin=i-1
	   goto 500
	 endif

	 igot1=igot1+1

	 tot=0.0
	 do i=1,imd
	   tot=tot+q(i,j)
	 end do
c
c               Keep total as an average even for reservoir data
         q(32,j)=tot/imd
c
c               Check daily data
	 if(ichk1.eq.1) then
	   write(nlog,160) cid(j), j, iy, im, (q(i,j), i=1,32),
     1     q(32,j)*fx*float(imd)	   	   
 	   
	 endif

       end do
       goto 500
c
c
 400   if(igot1.ge.1) then
	 goto 500
       else
	 if(iwarn.eq.0) then
	   write(nlog,410) dtype(nfx)
c          write(6,410) dtype(nfx)
	 endif
	 iwarn=1
c
c rrb 01/02/10; Allow no daily data               
c        goto 9999
	 goto 500
       endif
c
c __________________________________________________________
c
c               Return

 500   return
c
c __________________________________________________________
c
c               Formats

 110   format(i4, i4, 1x, a12, 31f8.2, f8.0) 
 160   FORMAT(a12, 3x,3i5, 31f10.2, f10.2, f10.0) 
 410   format('  Daydata; Warning, ran out of daily data for: ',a45,
     1        '  Moving on')
 420   format('  Daydata;      nf',     
     1   '     nin       j      iy     iy1      im     im1   igot1',/
     1   10x, 20i8)

 430  format('  Daydata;', 32('        ',i2),/,10x, 32f10.2)
 440  format(/,60('_'),/'  Daydata; for ',a45,//,
     1'ID                 #   YR  MON', 31i10,'   Ave-cfs  Total-af',/
     1'____________   _____ ____ ____', 33(' _________'))
 
 510  format('  Stopped in Daydata, see the log file (*.log)')
 520  format('  Stopped in Daydata')

c
c __________________________________________________________
cc               Error Message
c
c9999 write(6,510) 
c     write(nlog,520) 
c     call flush(6)
c     write(6,*) 'Stop 1'
c     call flush(6)
c     call exit(1)

c     stop 
      END
