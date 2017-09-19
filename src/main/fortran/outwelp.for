C     Last change:  C    20 May 97    0:04 am
C
c *********************************************************
c
      subroutine OutWelP
c
c       OutWelP; It prints for every well structure 
c	         every Aug plan with well pumping 
c
c _________________________________________________________
c
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
c		qt    = total monthly pumping by a ditch in a month
c		qp(i) = monthly pumping by a ditch associated with plan i
c
c		qtA   = total annual pumping by a ditch in a month
c		qpA(i)= annual pumping by a ditch associated with plan i
c
c		qtAA(im) = Average total basin pumping in month im
c		qpAA(im) = Average basin pumping to a plan in month im
c		qxAA(im) = Aveage basin pumping not to a plan in month im
c
c		iplan     = number of plans tied to this well
c		cwellP(i) = augmentation plan ID associated with this well
c		nplanP(i) = plan ID associated with this well
c
c		nwellW(k) = well structure associated with well right k
c		nwellP(k) = plan associated with well right k,
c _________________________________________________________
c
c
      include 'common.inc'
c                           
      dimension   
     1  cwellP(10), nplanP(10), nwellW(maxdvrw), nwellP(maxdvrw),
     1  qp(11),  qpa(11), qpAA(13), qxAA(13), qtAA(13)

      character   cdx*12,   ida0*12, ftype*24, ptype*24,
     1  cwellP*12, cwellP1*12, cname1*24
     
      data qpAA/13*0.0/, qxAA/13*0.0/, qtAA/13*0.0/
c
c _________________________________________________________
c		Step 1; Initilize
      write(6,101) 'OutWelP  '
      write(nlog,101) 'OutWelP  '
 101  format(/,72('_'),/'  Subroutine ', a8)
      call flush(6)
c
c		iout=1 details
c		iout=2 summary     
      iout=2
c
c		Test reach (zone) balance
      nz=3       
c
c		Set number of output values 1x in StateM.for
c	        and store in common.inc      
      ndiv = ndivO        
      nres = nresO
      ndivw= nwelO
      
      nout = 22
      nid=numdivw
      
      ndim=0
      max1=1000
      max2=10
      max3=max2+1
      small=0.001
      
      do iy=1,1000
        do im=1,13
          dum(im,iy)=0.0
          dum2(im,iy)=0.0
          dum3(im,iy)=0.0          
        end do
      end do          
c
c _________________________________________________________
C
c		Step 2; Print header
      call outtop(nout,1,61)
c
c _________________________________________________________
c               Step 3; Get requested ID's
      idallx=0
      if(ioutx.eq.1) then
        call getin(6, iid, maxsta, idallx, idtype, ftype, ptype, idreq)
c
c       write(nlog,*) '  OutWelP; iid, idallx ',  iid, idallx
      endif
c
c
c
c _________________________________________________________
c               Step 4; Set id screen switch
      if(idallx.eq.0) then
        nid=numdivw
      else
        nid=iid
      endif

c     write(nlog,*) '  OutWelP; idallx, nid = ', idallx, nid
c                      
      ida0 = '0           '
      ip1 = 0                                         
      ix = 25
c
c
c
c _________________________________________________________
c		Step 5; Loop for well ID of interest
      do 190 ip=1,nid
      if(idtype(ip).eq.6 .or. idallx.eq.0) then

        if(idallx.eq.0) then
          iw=ip
          is=idvstaw(iw)
        else
          call getid(idtype(ip),is,iw,iw2,idreq(ip))
        endif
        
        if(iout.eq.1) then
          write(nlog,*) ' '
          write(nlog,*) ' Processing well ', nid, iw, cdividw(iw)
        endif
        
c
c
c
c _________________________________________________________
c 		Step 6; Print Output every 25 structures
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
c _________________________________________________________
c              Step 7; Get station ID (assumes one per station)
        irecs=numdivw+iw+numtop
        read(42,rec=irecs,err=310) (dat1(i),i=1,ndivw)
c
        cdx = cdividw(iw)
        ip1=ip1+1         
c
        cname1=divnamw1(iw)
c
c _________________________________________________________
c		Step 8; Print header
        write(nout,210) cunitm,
     1    headin1(1), HEADIN1(2),ip1,
     1    cdx,ida0, cname1

c
c
c _________________________________________________________
c		Step 9; Print water right info
        maxwrx=maxwr
        call outwr2(maxwrx, 6, is, iw, nout)
c  
c _________________________________________________________
c		Step 10; Get every water right associated with this 
c			 and its associated aug plan
        nWR=0
        iplan=0
        ipn=0  
c
c rrb 2010/02/08; Correction        
        do i=1,10
          cwellP(i) = '          NA'        
        end do
c        
c rrb 2010/02/08; Enhancement
        do i=1,maxdvrw 
          nwellP(i) = 0
          nwellW(i)= 0
        end do          
c ---------------------------------------------------------
c            
        do k=1,numdvrw
          iw1=idivcow(1,k)
          ndimX=max1
          
c         write(nlog,*) ' OutWelP; ',k, iw, iw1
          if(iw1.eq.iw) then
            nWR=nWR+1
            if(nWR.gt.max1) goto 300 
            nwellW(nWR)=k
c            
c ---------------------------------------------------------
c            
c		Get every plan associated with this well
            ipX=iplanw(k)
c
c rrb 20100208; Correction
            if(ipx.gt.0) then                        
              cwellP1=pid(ipX)
              ndimX=max2
c           
c		  First plan tied to this well            
              if(iplan.eq.0) then
                iplan=1
                ipn=1
                if(iplan.gt.max2) goto 300
                
                cwellP(iplan)= pid(ipX)
                nplanP(iplan)= ipX
                nwellP(k)=ipn
              else  
c           
c		  More than one plan tied to this well
c		  Determine if it is the same as a prior plan            
                ipn=0
                do i=1,iplan
                  if(cwellP1.eq.cwellP(i)) then
                    ipn=i
                    nwellP(k)=ipn                  
                  endif  
                enddo
c           
c		  More than one plan tied to this well
c		  Not the same as a prior plan
c		                
                if(ipn.eq.0) then
                  iplan=iplan+1
                  if(iplan.gt.max2) goto 300
                  
                  ipn=iplan
                  nwellP(k)=ipn                  
                  cwellP(iplan)= pid(ipX)
                  nplanP(iplan)= ipX
                endif
              endif  
c              
c --------  -------------------------------------------------
c              
              if(iout.eq.1) then
                if(nwr.eq.1) write(nlog,220)
                write(nlog,222)
     1          nwr, iw,cdx, k,crigidw(k),ipX, iplan, 
     1          ipn, cwellP(ipn), nwellP(ipn)
              endif
            endif
          endif  
        end do 
        
        if(iout.ge.2) then
          if(iw.eq.1) write(nlog,224)
           write(nlog,226)iw, cdx, iplan
        endif 
c
c ---------------------------------------------------------
c               Print title card every year
        if(iout.eq.1) write(nlog,*) '  OutWelP; iplan = ', iplan
        if(iplan.gt.0) write(nout,277)
c
c ---------------------------------------------------------
c               Print plans associated with this Well Structure
        do i=1,iplan
          i1=nplanP(i)
          write(nout,278) i, cwellP(i), NameP(i1)
        end do          
c
c _________________________________________________________
c  
c               Step 11; Finally year loop
c 
        iy1=0
        do 180 iy=iystr,iyend           
          iy1=iy1+1
c
c               Print title card every year
          write(nout,270) '   Pump', 'Total', (i, i=1,12) 


          call year(iy, iyrmo, imomo, cyr1)
          do i=1,ndivw
            dat1t(i) = 0.0
          end do
          
          qtA=0.0
            
          do i=1,max3
            qpA(i)=0.0
          enddo
          
c
c
c _________________________________________________________
c		Step 12; Read binary well output
          do 170 im=1,12
c
            irecs=((iy-iystr0)*12+(im-1))*numdivw+iw+numtop
            read(42,rec=irecs,err=310) (dat1(i),i=1,ndivw)
            if(iout.eq.1) write(nlog,*) ' OutWelP; ',
     1       (dat1(i)*fac,i=1,ndivw)

            fac=fmo(im)
c           cx = cu         
c           if(iresop.ne.1) cx=cu*mthday(im)
           
c
c _________________________________________________________
c		Step 13; Distribute to water rights and plans
c !!!! Assume rights are sorted by priority		

            qt=dat1(3)
            qt1=qt
            qtA=qtA+qt*fac
            
            
            dum(im,iy1)=dum(im,iy1) + qt*fac
            dum(13,iy1)=dum(13,iy1) + qt*fac
            
            qtAA(im) = qtAA(im) + qt*fac
            qtAA(13) = qtAA(13) + qt*fac
            
            do i=1,max3
              qp(i)=0.0
            enddo
            
            do n=1,nwr
              k=nwellW(n)
              q1=amin1(qt, dcrdivw(k))
              qt=qt-q1
              
              ipn=nwellP(k)
              ipX=iplanw(k)
              
c
c rrb 2008/04/11; Correction
c             if(ipn.gt.0) then
              if(ipX.gt.0) then
                qp(ipn)=qp(ipn) + q1              
                qp(max3)=qp(max3) + q1
              
                qpA(ipn)=qpA(ipn) + q1*fac
                qpA(max3)=qpA(max3) + q1*fac
                
                dum2(im,iy1) = dum2(im,iy1) + q1*fac
                dum2(13,iy1) = dum2(13,iy1) + q1*fac
                
                qpAA(im) = qpAA(im) + q1*fac
                qpAA(13) = qpAA(13) + q1*fac
              else
                if(qt.gt.small) then
                  write(nlog,228) cdx, qt*fac, iyr
                  
                  dum3(im,iy1) = dum3(im,iy1) + q1*fac
                  dum3(13,iy1) = dum3(13,iy1) + q1*fac
                  
                  qxAA(im) = qxAA(im) + q1*fac
                  qxAA(13) = qxAA(13) + q1*fac
                endif
              endif  
            end do              
            
            if(isigfig.eq.0) then
              write(nout,274)   cdx, cstaid(is), iyrmo(im), xmonam(im),
     1                        qt1*fac, (qp(i)*fac,i=1,max3)
            else
              write(nout,275)   cdx, cstaid(is), iyrmo(im), xmonam(im),
     1                        qt1*fac, (qp(i)*fac,i=1,max3)
            endif
     
  170     continue
c
c               Print total
          write(nout,272)
          write(nout,274)   cdx, cstaid(is), iyrmo(13), xmonam(13), 
     1                      qtA,  (qpA(i),i=1,max3)

          write(nout,*) ' '
c
c _________________________________________________________
c
c               End Year Loop      
  180   continue
c
c
c _________________________________________________________
c               End Diversion Loop
      endif
  190 continue
c
c _________________________________________________________
c		Step 14; Print Basin Total
c
c ---------------------------------------------------------
c               Print header
        cdx='Basin Total '
        write(nout,210) cunitm,
     1    headin1(1), HEADIN1(2),0,
     1    cdx ,0

     
        do i=1,10
          cwellP(i) = 'NA'        
          dumx(i)=0.0
        end do
        
        cwellP(1)   = 'Yes'
        cwellP(2)   = 'No'
c
c _________________________________________________________
c               14a; Print basin total - monthly

        iy1=0
        ry1=iyend-iystr+1
        do iy=iystr,iyend           
          iy1=iy1+1
          call year(iy, iyrmo, imomo, cyr1)
c
c ---------------------------------------------------------
c               Print title card every year
c         write(nout,240) (adjustR(cwellp(i)),i=1,10), (i, i=1,12) 
          write(nout,270) '   Pump', 'Total', (i, i=1,12) 
c
c ---------------------------------------------------------
c               Print monthly basin total
          do im=1,12
            write(nout,274)   cdx, cdx, iyrmo(im), xmonam(im),
     1        dum(im,iy1), dum2(im,iy1), dum3(im,iy1),
     1        (dumx(i), i=1,8), dum(im,iy1)
          end do
c
c ---------------------------------------------------------
c               Print annual basin total
          write(nout,272)
          write(nout,274)   cdx, cdx, iyrmo(13), xmonam(13), 
     1      dum(13,iy1), dum2(13,iy1), dum3(13,iy1),
     1      (dumx(i), i=1,8), dum(13,iy1)
          
        end do
        
        
        
c
c _________________________________________________________
c               14b; Print basin total - annual

        iy1=0
        ry1=iyend-iystr+1
c
c ---------------------------------------------------------
c               Print title card once
        write(nout,210) cunitm,
     1    headin1(1), HEADIN1(2),0,
     1    cdx ,0
     
        write(nout,277)     
        write(nout,278) 1, 'NA          ',
     1    'With a Plan             '
        write(nout,278) 2, 'NA          ',
     1    'Without a Plan          '

        write(nout,270) '   Pump', 'Total', (i, i=1,12) 
        
        do iy=iystr,iyend           
          iy1=iy1+1
          call year(iy, iyrmo, imomo, cyr1)
c
c ---------------------------------------------------------
c               Print basin total annual
          write(nout,274)   cdx, cdx, iyrmo(13), xmonam(13), 
     1      dum(13,iy1), dum2(13,iy1), dum3(13,iy1),
     1      (dumx(i), i=1,8), dum(13,iy1)          
        end do
c
c ---------------------------------------------------------
c               Print basin total Average
        c1=1.0/ry1
        write(nout,272)
        write(nout,276)   cdx, cdx, xmonam(13), 
     1    qtAA(13)*c1, qpAA(13)*c1, qxAA(13)*c1, 
     1    (dumx(i), i=1,8), qtAA(13)*c1        
        
        
        
c _________________________________________________________
c
c               14c; Print Average Monthly
        cdx='Basin Total '
c
c ---------------------------------------------------------
c               Print title card 
        write(nout,210) cunitm,
     1    headin1(1), HEADIN1(2),0,
     1    cdx ,0
     
        write(nout,277)     
        write(nout,278) 1, 'NA          ',
     1    'With a Plan             '
        write(nout,278) 2, 'NA          ',
     1    'Without a Plan          '
     
    
        write(nout,270) '   Pump', 'Total', (i, i=1,12) 
        
c        
c ---------------------------------------------------------
c
c               Print Average Monthly basin total
        do im=1,12
          write(nout,276)   cdx, cdx, xmonam(13), 
     1      qtAA(im)*c1, qpAA(im)*c1, qxAA(im)*c1, 
     1      (dumx(i), i=1,8), qtAA(im)*c1
        end do
c
c ---------------------------------------------------------
c               Print Average Annual Basin Total
        write(nout,272)
        write(nout,276)   cdx, cdx, xmonam(13), 
     1    qtAA(13)*c1, qpAA(13)*c1, qxAA(13)*c1, 
     1    (dumx(i), i=1,8), qtAA(13)*c1
  
c
c _________________________________________________________
c		Step 15; Return
      return
c
c
c _________________________________________________________
c
c        Formats
c
  200 format(2('___________ '), 2(' ____'), 12(' ____________'))
c 210 FORMAT('',/, '   Well Water Only Summary ',a5,/,3x,a80,/,
  210 FORMAT('',/, '   Well Augmentation Plan Summary ',a5,/,3x,a80,/,
     1  3X,a80,33X, 'PAGE NO. ',I3,//,
     1  '    STRUCTURE ID (0 _ total)  : ',a12,/,
     1  '    STRUCTURE ACCT (0 _ total): ',a12,/,
     1  '    STRUCTURE NAME            : ',a24,/)
     
  220 format(/,
     1 '    # Well Well_ID      Righ Right_Id      ipX ipln   ip',
     1 ' Plan_ID       nWellP',/
     1 ' ____ ____ ____________ ____ ____________ ____ ____ ____',
     1 ' ___________ ________')   
     
  222 format(i5, i5,1x,a12, i5,1x,a12, i5, i5, i5, 1x, a12, 1x, i8) 
  
  224 format(/,
     1 '    # Well_ID        Plans',/
     1 ' ____ ____________ _______')
     
  226 format(i5, 1x,a12, i8)
  
  228 format(/, 
     1 '  OutWelP; Warning Well ID = ', a12, ' is not tied to a plan',/
     1 '           But pumping = ', f8.0, ' in ', i5)
 
  240  format(2a12,i5, 2x, a3, 20(1x,f12.0))
  242  format(2a12,  '  Ave', 2x, a3, 20(1x,f12.0))
  
  250  format(a12, i5, 2x, a3, 20f8.0)
  260   format('+', '   Printing Well Augmentation Plan Summary',
     1        i5,' of ', i5, '; or ',f8.0, ' % Complete')
     
  270   format(/,
     1  '                                            ',
     1  'Plan Supplies                     ',/
     1  '                                    ',a7,
     1  ' _________________________________________',
     1  '______________________________________________',/
     1  'Well         River                    ',a5,
     1  '  Plan 1  Plan 2  Plan 3  Plan 4  Plan 5',
     1  '  Plan 6  Plan 7  Plan 8  Plan 9 Plan 10',
     1  '   Total',/
     1  'ID           ID           Year   Mo     N/A',
     1  '     (+)     (+)     (+)     (+)     (+)',
     1  '     (+)     (+)     (+)     (+)     (+)',
     1  '     NA',/
     1  '                                   ', 12('    (',i2,')'),/
     1  '____________ ____________ ____ ____ _______',      
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______ _______')
c     
c    1  '________________________________________',
c    1  '________________________________________',     
c    1  ' Plan 11 Plan 12 Plan 13 Plan 14 Plan 15',
c    1  ' Plan 16 Plan 17 Plan 18 Plan 19 Plan 20',
c    1  '     (+)     (+)     (+)     (+)     (+)',
c    1  '     (+)     (+)     (+)     (+)     (+)',          
c    1  ' _______ _______ _______ _______ _______',
c    1  ' _______ _______ _______ _______ _______',
c    1  '                                   ', 22('    (',i2,')'),/
c
  272   format(
     1  '____________ ____________ ____ ____ _______',      
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______ _______')

  274   format(a12,1x,a12, i5,2x,a3,40f8.0)
  275   format(a12,1x,a12, i5,2x,a3,40f8.1)
  276   format(a12,1x,a12, '  Ave',2x,a3,40f8.0)
  277   format(/,'    PLAN DATA')
  
  278   format('    Plan ',i2,  '   ID = ', a12, 1x, ' Name = ',a24)
c
c _________________________________________________________
c               Error messages
  300  write(nlog,302) ndimX
  302  format(/, 72('_'),/
     1  '   OutWelP; Problem dimension exceeded = ', i5)
       goto 400
     
  310  write(nlog,312)
  312  format(/, 72('_'),/
     1  '   OutWelP; Requested data exceeds binary file size')
       goto 400
c
c               Error Warning
  400 write(6,410) 
      write(nlog,420) 
      call flush(6)
  410 format('    Stopped in OutWelP',/,
     1       '    See the *.log file')
  420 format('    Stopped in OutWelP')
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

c
c _________________________________________________________
c

 500  stop 
      END


