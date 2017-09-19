C
c *********************************************************
c
      SUBROUTINE OUTrev
c
c
c _________________________________________________________
c	Program Description
c
c       Outrev; It prints reservoir evaporation (*.xev) for CU anlysis
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
c                           
c
c _________________________________________________________
c		Step 1; Initilze

      write(6,*) ' Subroutine Outrev'
      write(6,*) ' '
c                                 
c     cu=1.0
c     if(iresop.eq.2) cu=factor
c     if(iresop.eq.3) cu=factor*0.001                        
c
c               nres = # of output values in file, 
c               nev = location of evap data
c
c rrb 00/06/22; Correction
c     nres = 21
c     nev = nres-9
c
c rrb 2005/11/29; River and Carrier Loss
c     nres = 24
c     nev = 12
      nres = 26
      nev = 14
C
      nrsactx=nrsact+numown
      if(numres.eq.0) then
         write(36,*) 'No active reservoirs for the current job'
      else
        call outtop(36,0,21)                  
      endif
c              
c     ir1 = 0

      do 220 iy=iystr,iyend
      ir1 = 0

      do 190 ir=1,numres
        if(iressw(ir).eq.0) goto 190

        ir1=ir1+1
          dat2t(13) = 0.0
c
c
c               Month Loop
          do 180 im=1,12
c           cx = cu         
c           if(iresop.ne.1) cx=cu*mthday(im)

c
c rrb 12/12/95; Variable report year for reservoirs
c
c rrb 06/06/96; Include header information
c           irecr=((iy-iystr0)*12+(im-1))*nrsactx+ir1
            irecr=((iy-iystr0)*12+(im-1))*nrsactx+ir1+numtop
            read(44,rec=irecr,err=250) (dat2(i),i=1,nres)
                     
            ida  = dat2(nres-1) 
            nacc = dat2(nres)
             
c           write(99,*) ' Outrep2; ida, nacc', ida, nacc
c             dat2t(im) = dat2(nev)*cx
              dat2t(im) = dat2(nev)*fmo(im)
              dat2t(13) = dat2t(13) + dat2t(im)
c
c               End Month Loop      
  180       continue
c 
c               Print one reservoir one year
           write(36,182) iy, cresid(ir), (dat2t(im), im=1,13), 
     1                         resnam1(ir)
  182      format(i4, 1x, a12, 13f8.0,       9x, a24)
c
c               Skip over subaccounts
          if(nacc-ida.gt.1) ir1 = ir1 + nacc-ida -1
c
c               End Reservoir Loop
  190     continue

c               End Year Loop
  220 continue
c
  300  return
c
c
c _________________________________________________________
c
c               Error Messages
c
  250  write(6,*)  '   Outrev; Requested data exceeds binary file size'
       write(99,*) '   Outrev; Requested data exceeds binary file size'
      goto 370
c
  370 write(6,*) 'Stop 1' 
      call flush(6)
      call exit(1)

c
c _________________________________________________________
c
      stop
      END

