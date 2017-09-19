
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
cC     Last change:  C    20 May 97    0:07 am
C
c *********************************************************

      SUBROUTINE OUTresc(cplot)
c
c
c _________________________________________________________
c	Program Description
c
c       Outresc; It prints a comparison of reservoir data
c                historic vs simulated
c
c _________________________________________________________
c      Update history
c
c rrb 99/12/27; Revised to handle variable number of missing values
c
c _________________________________________________________
c      Documentation
c               nres = # of output values in file, 
c               neom = location of EOM data
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
c                           
      dimension nmo(13)
c                           
      character cplot*12, blank*12, cistat*12, recin*256

c _________________________________________________________
c
c                                 
c               Step 1 Initilize
c

c     write(6,*) ' '
      write(6,*) ' Subroutine Outresc'
      write(6,*) ' '
c      
c		Test reach (zone) balance
      nz=3 
      
c
c-------------------------------------------------------------------
c		Open and Print header to Report File
      if(cplot.eq.'Report      ') then
        call namext(maxfn, filenc, 'Yrc', filena) 
        open(114,file=filena,status='unknown')
        call outtop(114,1,57)        
        write(114,241)        
      endif  

      blank = ' '
c
c
c		Set number of output values 1x in StateM.for
c	        and store in common.inc      
      ndiv = ndivO        
      nres = nresO
      ndivw= nwelO
c
c rrb 2006/01/21; Columns of data impacted by a new file size		
c               nbom = column of BOM data (1)
c		nemo = column of EOM data (16)
c		ntar = column of Tartet data (17)
c		none = column of One fill or BOM limit (18)
c		nidr = columns of irdr (account))
c		naccX = columns of nacc ( )
c		nresX = columns to adjust by factor
c               nresY = columns to print     
      nBom=1 
      nEom=16
      nTar=17
      nOne=18
      
      nidr=nres-2
      naccX=nres-1
      nresX=nres-3
      nresY=nres-4

      nf = 9       
      numresh=0
      nrsactx=nrsact+numown

      d1=0.0
      d2=0.0
      d3=0.0
      d4=0.0
      d5=0.0
c
      do 90 ir=1,numres
        idum(ir) = 0
 90   continue
c
      if(numres.eq.0 .and. nrsact.eq.0) THEN
        Write(nf,*) ' NO ACTIVE RESERVOIR FOR THE CURRENT JOB'
        Goto 290
      endif
c
c _________________________________________________________
c
c               Step 2; Get historic EOM data

c     write(io99,*) '  Outresc; gettin historic EOM data'
c     write(6,*) '  Outresc; gettin historic EOM data' 

      nd=0
      iin2=55
      filena='*.eom'
  100 read(55,210,end=120,err=928) iryr,cistat,(diverm(im),im=1,12)
c     write(io99,210) iryr,cistat,(diverm(im),im=1,12) 
      nd=nd+1
c     write(io99,*) '  Outresc; writing EOM data to file 1'
c     write(6,*) '  Outresc; writing EOM data to file 1'

      write(1,rec=nd) iryr, (diverm(im), im=1,12)


      if(numresh.eq.0) iryr1=iryr

      if(iryr1.ne.iryr) then
        goto 100
      else           
        numresh=numresh+1
c
c               Find corresponding ID
        do 110 ir=1,numres
          if(cistat.eq.cresid(ir)) then
            idum(ir) = numresh
            goto 100
          endif
  110   continue
        goto 300
      endif
c
c _________________________________________________________
c
c
c               Step 3; Get Simulated Results
  120 ip = 0
      ir1 = 0
c     write(io99,*) '  Outresc; calling average'
c     write(6,*) '  Outresc; calling average'

c 
c               Initilize Basin Total data
c      call average(maxsta,-1,1,numres,0,nyr1,
       call average(maxsta,-1,1,im,iy1,nyr1,
     1              1.0,d1,d2,d3,d4,d5,dum,dum2,
     1             iz, maxrch, maxyrs, dum3D)
c 
c               Begin Reservoir Station loop
c     write(io99,*) '  Outresc; beginning reservoir loop'
c     write(6,*) '  Outresc; beginning reseroir loop'

      do 200 ir=1,numres
        if(iressw(ir).eq.0) goto 200
c 
c               Initilize structure average
c      call average(maxsta,0,1,numres,0,nyr1,
       call average(maxsta,0,1,im,iy1,nyr1,
     1              1.0,d1,d2,d3,d4,d5,dum,dum2,
     1             iz, maxrch, maxyrs, dum3D)
c
c rrb 10/27/94 Additional Output
        c = float(ir)/float(numres)*100.0
        write(6,130) ir, numres, c
        call flush(6)
  130   format('+', '   Printing Reservoir Comparison',i5,' of ', i5,
     1              '; or ',f8.0, ' % Complete', i5)
c
c              Print Header
        ida0 = 0
  140   ip = ip + 1
        if(cplot.eq.blank) then
          write(nf,220) headin1(1), headin1(2),ip,
     1                  cresid(ir), resnam1(ir), ida0
        endif

        ir1=ir1+1
c
c              Step 4; Print title
        if(cplot.eq.blank) then
          write(nf,230)
        else
          write(nf,240)
        endif
c              
c rrb 99/12/27
        do im=1,13
          nmo(im)=0
        end do
        nyr1=0
        iy1 = 0

        do 170 iy=iystr,iyend
        iy1 = iy1+1
        call year(iy, iyrmo, imomo, cyr1)
c
c _________________________________________________________
c
c
c               Get Historic EOM data
c
c rrb 99/06/23; iystr0 is the simulation period
c       irech=(iy-iystr0)*numresh+idum(ir)
        irech=(iy-iystr)*numresh+idum(ir)
        read(1,rec=irech,err=310) iryr, (diverm(im), im=1,12)

c
c
c               Month Loop
          do 160 im=1,12
            im1 = (iy - iystr)*12 + im
c           cx = fac         
c           if(iresop.ne.1) cx=fac*mthday(im)
c
            irecr=((iy-iystr0)*12+(im-1))*nrsactx+ir1+numtop
            read(44,rec=irecr) (dat2(i),i=1,nres)
                     
            ida  = dat2(nidr) 
            ida0 = ida + 1
            nacc = dat2(naccX)
             
c           write(99,*) ' Outrep2; ida, nacc', ida, nacc
            do i=1,nresX
              dat2(i)=dat2(i)*fmo(im)
            end do

c
c rrb 99/12/27; Handle missing historic data
            if(abs(diverm(im)+999.0).lt.0.01) then
              c = -999.0
              cp = -999.0
              nmo1=0
            else
c
c               Calculate delta                  
              c = diverm(im)-dat2(neom)
              cp = -1.
              if(diverm(im).gt.0.001) cp = (c/diverm(im))*100.0
              nmo1=1
              nyr1=nyr1+1
              nmo(im)=nmo(im)+1
            endif
c 
c               Print total station output
            if (cplot.eq.blank) then
              write(nf,250)   cresid(ir), ida, iyrmo(im), xmonam(im), 
     1                        diverm(im), dat2(neom), c, cp,
     1                        resnam1(ir), nmo1
            else
              write(nf,260)   cresid(ir), ida, iyrmo(im), xmonam(im), 
     1                        diverm(im), dat2(neom), c, cp, nmo1
            endif         
c
c               Total for average monthly, average annual and
c                         annual total
            call average(maxsta, 1, 1, im, iy1, nyr1,  
     1                   1.0,diverm(im),dat2(neom),c,d4,d5,dum,dum2,
     1             iz, maxrch, maxyrs, dum3D)
c
c               End Year and Month Loop      
  160       continue
  170     continue
c
c               Skip over subaccounts
          if(nacc-ida.gt.1) ir1 = ir1 + nacc-ida -1
c 
c               Print annual total
          write(nf,*) ' '
          write(nf,*) ' Average Monthly by Year'

          iy5 = 5
          do 180 iy=iystr,iyend
          iy5 = iy5+1

          dum(4,iy5) = -1.0
          if(dum(1, iy5) .gt. 0.001) then
            dum(4, iy5) = dum(3, iy5) / dum(1,iy5)*100.0*12.0
          endif

            nyr2=1
            c=12.0
            if(abs(dum(1,iy5) + 999.0) .lt.0.01) then 
              nyr2=0
              c=1.0
            endif

            if (cplot.eq.blank) then
              write(nf,250)   cresid(ir), ida, iy, xmonam(14),
     1                        (dum(j,iy5)/c, j=1,4),
     1                        resnam1(ir), nyr2
            else
              write(nf,260)   cresid(ir), ida, iy, xmonam(14),
     1                        (dum(j,iy5)/c, j=1,4), nyr2
            endif              
  180     continue
c
c rrb 99/12/27; Calculate average annual for years with 
c               no missing data only
          iy5=5
          do iy=iystr, iyend
            iy5=iy5+1
            if(abs(dum(1,iy5)+999.0) .gt. 0.01) then
              nmo(13) = nmo(13) + 1
              dum(13,1) = dum(13,1) + dum(1,iy5)
              dum(13,2) = dum(13,2) + dum(2,iy5)
              dum(13,3) = dum(13,3) + dum(3,iy5)
            endif
          end do
c
c _________________________________________________________
c               Print average                   

          write(nf,*) ' '
          write(nf,*) ' Average Monthly by Month and Year'
c
c rrb 99/12/27; 
          ry = (iyend-iystr+1)
          if(ry.lt.0.01) goto 200

          do 190 im=1,13                                    
c
c rrb 99/12/27; 
            ryx=nmo(im)
            im1 = im
            if(im.eq.13) then
              write(nf,*) ' '
c
c rrb 99/12/28;
              ryx = ryx*12
              im1 = im1+1
            endif
c
c rrb 99/12/27; 
            if(nmo(im).eq.0) then
              ryx=1.0
              dum(im,1) = -999.0
              dum(im,2) = -999.0
              dum(im,3) = -999.0
            endif

            dum(im,4) = -1.0
            if(dum(im,1) .gt. 0.001) then
              dum(im,4) = dum(im,3) / dum(im,1)*100.0*ryx
            endif
c 
c               Print average output
            if (cplot.eq.blank) then
              write(nf,270)   cresid(ir), ida, xmonam(im1),
     1                        (dum(im,j)/ryx, j=1,4),
     1                        resnam1(ir), nmo(im)
            else
              write(nf,280)   cresid(ir), ida, xmonam(im1), 
     1                        (dum(im,j)/ryx, j=1,4),
     1                        resnam1(ir), nmo(im)
            endif              
  190     continue
          write(114,271) cresid(ir),(dum(13,j)/ryx,j=1,4), resnam1(ir)
  
c
c               End Reservoir Loop
  200 continue
c
c
c _________________________________________________________
c
c               Formats
  210     format(I4, 1X, A12, 12f8.0)
  220 FORMAT('',/,'    Reservoir Summary',/,3x,a80,/,
     1       3X,a80,53X, 'PAGE NO. ',I3,//,
     1          '    RESERVOIR ID  : ',a12,/,
     1          '    RESERVOIR NAME: ',a24,/,
     1          '    RESERVOIR ACCT: ',I8, ' (Account 0 is the ',
     1       'total)',/)
  230   format(/,
     1  'Structure                    Gauged    Est.   Delta   Delta',/
     1  'ID            Acc Year   Mo     EOM     EOM     EOM   EOM %',
     1  ' Name',25x, '#',/
     1  '___________  ____ ____ ____ _______ _______ _______ _______ ',
     1  25('_'), " ____")

  240   format(/,
     1  'Structure  ,      ,     ,     ,  Gauged,    Est.,',
     1                                 '   Delta,   Delta',/
     1  'ID         ,   Acc, Year,   Mo,     EOM,     EOM,',
     1                                 '     EOM,   EOM % Name',/
     1  '___________  ____ ____ ____ _______ _______ _______ _______ ',
     1  25('_'), " ____")
     
  241   format(/,
     1  'Structure  ,  Gauged,    Est.,   Delta,     Delta,',/
     1  'ID         ,     EOM,     EOM,     EOM,     EOM %,   Name')

  250  format(a12,2i5, 2x, a3, 4f8.0,1x,a24, 1x, i5)
  260  format(a12,',', 2(i5,','), 2x, a3, ',', 4(f8.0,','), i5, ',')  
  270  format(a12,i5,'  Ave' 2x, a3, 4f8.0,1x,a24, 1x, i5)
  271  format(a12,',' 4(f8.0,1x,','), a24)
  280  format(a12,',', (i5,','), '  Ave,',2x, a3, ',', 4(f8.0,','),
     1        1x,a24, ',', 1x, i5, ',')  
c
  290  return
c
c _________________________________________________________
c
c
c               Error Messages

  300 write(6,*) ' Outresc; problem finding reservoir ', cistat
      goto 9999

  310 write(6,*) ' Outresc; problem reading temp reservoir file '
      goto 9999
c
c _________________________________________________________
c
c
  926 write(io99,927) iin2, filena
  927 format(' Outresc.f; End of file # ', i4, ' encountered',/,
     1       '   File name: ', a256)
      goto 9999
c
  928 write(io99,929) iin2, filena
  929 format(' Outresc.f; Problem reading file # ', i4,/,
     1       '   File name: ', a256,/
     1       '   Problem record (see next line):')

      backspace(iin2)
      read(iin2, '(a256)',end=926,err=926) recin
      write(io99,'(a256)') recin
      goto 9999

 9999 write(6,*) '  Stopped in Outresc, see the log file (*.log)'
      write(io99,*)'  Stopped in Outresc'
      write(6,*) 'Stop 1' 
      call flush(6)
      call exit(1)

c
c _________________________________________________________
c
      stop 
      END      
