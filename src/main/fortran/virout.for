C
      SUBROUTINE VIROUT(ityp,irec)
c
c
c _________________________________________________________
c	Program Description
c
c       Virout; It prints baseflow (virgin) results
c
c               Called from vircom
c               ityp = 1; Monthly & Daily
c                         Print *.xbi (70) baseflow and
c                         and monthly scratch (75) 
c               ityp = 2; Monthly & Daily
c                         Print *.xbm (72) monthly baseflows &
c                         *.xbg (71) baseflows at stream gages
c                         *.xbe (76) baseflows at stream estimates
c               ityp = 3; Daily
c                         Print Stream_base *.xby (73),
c                               StreamGage_Base *.xgy (79),
c                               StreamEstimate_Base *.xey (80)
c               ityp = 4; Monthly & Daily
c                         Print *.xbi (70) negative summary
c               ityp = 5; Daily
c                         Print *.xbx (74) daily baseflow info and
c                         daily scratch file (74)
c
c
c _________________________________________________________
c       Update History
c
c rrb 03/03/03; Revised -999 checks for roundoff issue that showed
c               up on relatively large flows. Changed from a real
c               comparison to an integer comparison
c
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
c
      dimension temp(20), tempa(20), tempm(20), zero(20)
      character cunit*5, ctime*3
c
c _________________________________________________________
c               Step 1; Initilize
      iout=0
      
      if(iout.eq.1 .or. ichk.eq.4) then
        write(6,*)    ' Virout; iyr, ityp ', iyr, ityp
        write(nlog,*) ' Virout; iyr, ityp ', iyr, ityp
      endif

      smallx = 0.1
      nout=17

      if(iday.eq.0) then 
        cunit=cunitm2
        ctime='mon'
      else
        cunit=cunitd2
        ctime='day'
      endif
c
c               Set factor for average monthly results
c               iresop = 1 = cfs 4 = cfs af mix
      if(iresop.eq.1 .or. iresop.eq.4) then
        do i=1,12
c         fxm(i)=fdy(i)
        end do 
      else
c       rid=1.0
        do i=1,12
c         fxm(i)=fmo(i)
        end do 
      endif

      do i=1,14
        zero(i)=0.0
      end do
      

c
c _________________________________________________________
c
c               Step 2; Monthly Base flow information file (*.xbi)
c                       Called 1x/month for monthly and daily model
c                       from vircom
      if(ityp.eq.1) then
        if(iday.eq.0) then
c         fx=fmo(mon)
          fx=faf(mon)
        else
c         fx=fdy(mon)
          fx=faf(mon)
        endif

        do irn=1,numrun
          is=irusta(irn)   
          x=temph(is)
          xx=tempno(is)
c
c rrb 00/07/10; Round off concern
          rc=abs(tempq(is)*fx+999.)
          ic = ifix((tempq(is)*fx-0.5))

          if(iout.eq.1) then
            write (99,302)
     1      iyrmo(mon), xmonam(mon), mthday(mon), cstaid(is), rc, ic
          endif
          if(ic.eq.-999) then
            x  = -999./fx
            xx = -999./fx
          endif
c
c               Monthly baseflow information (*.xbi) & Scratch file
          write(70,300)
     1      iyrmo(mon),    xmonam(mon),  mthday(mon), cstaid(is),
     1      tempq(is)*fx,  tempi(is)*fx,
     1      tempd(is)*fx,  tempr(is)*fx,  tempw(is)*fx,
     1      temps(is)*fx,  tempe(is)*fx,  x*fx, xx*fx,
     1      tempts(is)*fx, tempfs(is)*fx, tempcu(is)*fx,
     1      templ(is)*fx,  tempp(is)*fx,  tempDre(is)*fx, 
     1      tempUse(is)*fx,tempRre(is)*fx,
     1      stanam1(is)

          irec=irec+1
          write(75,rec=irec) 
     1      tempq(is)*fx,   tempi(is)*fx,
     1      tempd(is)*fx,   tempr(is)*fx,   tempw(is)*fx,
     1      temps(is)*fx,   tempe(is)*fx,   x*fx, xx*fx,
     1      tempts(is)*fx,  tempfs(is)*fx,  tempcu(is)*fx, 
     1      templ(is)*fx,   tempp(is)*fx,   tempDre(is)*fx, 
     1      tempUse(is)*fx, tempRre(is)*fx
     
        end do
c
c rrb 2006/03/02; Treat monthly and daily *.xbi the same        
cr      if(iday.eq.0 .and. numrun.gt.1) write(70,*) ' '
        goto 500
      endif
c
c _________________________________________________________
c
c               Step 3; Monthly baseflows (*.xbm) files 71,
c                       at gages (*.xbg) 72 and estimates (*.xbe) 76
c                       Called 1x per simulation from vircom
      if(ityp.eq.2) then
c
c              Station loop
        do is=1,numsta

c
c               Print both gages (ibout=1) and baseflows (ibout=2)
          if(ibout(is).ge.1) then
            ytot=0.0
            ytotN=0.0
            do IM=1,12
              fx=faf(im)
c             QHISTO(IM,is)=QHISTO(IM,is)*f(im)
              QHISTO(IM,is)=QHISTO(IM,is)*fx
              

c
c rrb 00/07/10; Round off concern
c             if(abs(qhisto(im,is)+999.).lt.0.001 .or. 
c    1                    ytot.lt.-0.01) then
c
c rrb 03/03/03; Roundoff OK since not a unit conversion issue
              if(abs(qhisto(im,is)+999.).lt.smallx.or. 
     1                    ytot.lt.-0.01) then
                ytot = -999.0
              else
                YTOT=YTOT+QHISTO(IM,is)
              endif
c
c rrb 2009/06/15; Store negative baseflows              
              dum3(im,is)=dum3(im,is)*fx
              ytotN=ytotN+dum3(im,is)
            end do
c
c               Print *.xbm (file 72)
            write(72,180) iyr, cstaid(iS),(qhisto(i,IS),i=1,12), YTOT
c
c               Print *.xbg (file 71) for stream gages only
c               Print *.xbg (file 76) for stream estimates only
c                                 
            if(ibout(is).eq.1) then
              write(71,180) iyr, cstaid(iS),(qhisto(i,IS),i=1,12), YTOT
c
c rrb 2009/06/15
c		Print negative flow summary      
cx            write(nlog,180) iyr, cstaid(iS),(dum3(i,IS),i=1,12), YTOTn                                  
              write(81,180) iyr, cstaid(iS),(dum3(i,IS),i=1,12), YTOTn              
            else  
              write(76,180) iyr, cstaid(iS),(qhisto(i,IS),i=1,12), YTOT
            endif

          endif
c
        end do
        goto 500
      endif
c
c _________________________________________________________
c
c               Step 4; Daily baseflows (*.xby) file 73
c                       Called 1x/day from vircom
      if(ityp.eq.3) then
        fx=faf(mon)
        do is=1,numsta
c
c               Average in virset(3,irec) for gages & baseflows
c         qhisto(mon,is)=qhistod(32,is)/rid
          if(ibout(is).ge.1) then 
            write(73, 350) iyrmo(mon), imomo(mon), cstaid(is), 
     1        (qhistod(i,is), i=1,31), qhistod(32,is)*fx
          endif
c
c               Print StreamGage_Base_Daily *.xgy (file 79)
c               Print StreamEstimate_Base_Daily *.xey (file 80)
c
          if(ibout(is).eq.1) then
            write(79, 350) iyrmo(mon), imomo(mon), cstaid(is), 
     1        (qhistod(i,is), i=1,31), qhistod(32,is)*fx
          else    
            write(80, 350) iyrmo(mon), imomo(mon), cstaid(is), 
     1        (qhistod(i,is), i=1,31), qhistod(32,is)*fx
          endif


        end do
        goto 500
      endif     
c
c _________________________________________________________
c
c               Step 5; Monthly baseflow summary (*.xbi) file 70
c                       Called 1x per simulation from vircom
c                       For both daily and monthly models
      if(ityp.eq.4) then                       
        ifound=0
        rn = iyend-iystr+1
        iin2=75
        filena='Scratch'
c
c rrb 01/12/31
        ixt=0
        c1t=0.0
        c2t=0.0
        ctt=0.0

        write(nlog,410) cunit, ctime, ctime, ctime

        do 460 irn=1,numrun
          write(70,120) cunit,iystr, xmonam(1), iyend, xmonam(12) 
          write(70,121) 'ACFT', (i, i=1,nout+1)
          
          is  = irusta(irn)
c
c ---------------------------------------------------------
c
c               Step 5a; Initilize
          do j=1,nout
            tempa(j) = 0.0
          end do

          do 430 im=1,12
            do j=1,nout
              tempm(j) = 0.0
            end do

            if(iday.eq.0) then
              imd=1
            else
              imd=mthday(im)
            endif

            do idy=1,imd  
              do iy=iystr, iyend
c
c ---------------------------------------------------------
c
c               Step 5b; Read from scratch file in acft
                if(iday.eq.0) then
                  irec=((iy-iystr)*12+(im-1))*numrun+irn
                else
c                 idy=1
                  irec=((iy-iystr)*12+(im-1))*numrun*31+
     1                 (idy-1)*numrun+irn
                endif
                
                read(75,rec=irec,err=928) (temp(j), j=1,nout)
c               write(Nlog,*) ' Virout In; 13, temp(13)', 13, temp(13)
                
                do j=1,nout
c
c rrb 10/21/97; Check for negative data on annual average for only
c               Gaged flow (1), total base flow (8) and 
c			base flow w/o negatives (9)
                  if((abs(temp (j)+999.).lt.smallx) .or.
     1               (abs((tempa(j)/rn)+999.).lt.smallx)) then
                    if(j.eq.1 .or. j.eq.8 .or. j.eq.9) then
                      tempm(j) = -999.*rn             
                      tempa(j) = -999.*rn
                    else
                      tempm(j)=tempm(j) + temp(j)
                      tempa(j)=tempa(j) + temp(j)
                    endif

                  else
                    tempm(j) = tempm(j) + temp(j)
                    tempa(j) = tempa(j) + temp(j)
                  endif
                end do
c
c               End year loop
              end do
c
c               End day loop (may be set to 1)
            end do
c
c               Print monthly average     
            write(70,420)  xmonam(im),
     1        cstaid(is), (tempm(j)/rn,j=1,nout),stanam1(is)
c
c               End month loop
  430     continue
c
c _________________________________________________________
c
c               Step 6; Print Negative GAGE Summary
c
          write(70,*) ' '
c
c rrb 2006/07/31; Add Loss          
c         write(70,440) cstaid(is), (tempa(j)/rn, j=1,12), 
          write(70,440) cstaid(is), (tempa(j)/rn, j=1,nout), 
     1                     stanam1(is)
c
c ---------------------------------------------------------
c
c               Step 6a; Print negative flow summary at gages
c                        (ineg.lt. 0)
          if(ineg(is).lt.0) then        
            ifound=ifound+1
            c1 = qneg(is)/float(ineg(is))*(-1.0) 
            c2 = c1
            if(iopflo.eq.1) c2 = 0.0
            ix = iabs(ineg(is))
c
c rrb 01/12/30; Sum
            c1t=c1t+c1
            c2t=c2t+c2
            ctt=ctt-qneg(is)
            ixt=ixt+ix

            write(nlog,450) ifound,
     1        cstaid(is), stanam1(is), ix, 
     1        c1, c2, float(ineg(is))*c1, is
c
c rrb 2006/08/18; Print none if appropriate
          else
            ifound=ifound+1
            write(nlog,450) ifound,
     1          cstaid(is), stanam1(is), ineg(is), 
     1          0.0, 0.0, 0.0, is
     
          endif
          
  460   continue  

cx        if(ifound.eq.0) then
c         write(nlog,*) ' None Found'
cx          write(nlog,452) 0, 0.0, 0.0, 0.0          
cx        else
          if(ixt.gt.0) then           
            c1a=ctt/float(ixt)
          else
            c1a=0.0
          endif    
          write(nlog,452) ixt, c1a, c2t, ctt
cx      endif
c
c ---------------------------------------------------------
c
c               Step 6b; Print negative BASEFLOW summary
c                        at baseflows (ineg .gt.0)
        write(nlog,470) cunit, ctime, ctime, ctime
        ifound=0
        ifound1=0
        ixt=0
        c1t=0.0
        c2t=0.0
        ctt=0.0

        do is=1,numsta
cr        if(ineg(is).gt.0) then
          if(ineg(is).gt.0) then
          
            ifound=ifound+1
            c = qneg(is)/float(ineg(is))
c
c rrb 01/12/30; Sum
            c1t=c1t+c
            c2t=c2t+0.0
            ctt=ctt+qneg(is)
            ixt=ixt+iabs(ineg(is))

            write(nlog,450) ifound,
     1          cstaid(is), stanam1(is), ineg(is), 
     1          c, 0.0, float(ineg(is))*c, is
c
c rrb 2006/08/18; Print none if appropriate
          else
            if(ibout(is).eq.2) then
              ifound=ifound+1
              write(nlog,450) ifound,
     1          cstaid(is), stanam1(is), ineg(is), 
     1          0.0, 0.0, 0.0, is
            endif          		     
          endif
        end do

cr        if(ifound.eq.0) then
c         write(nlog,*) ' None Found'
cr          write(nlog,452) 0, 0.0, 0.0, 0.0                    
cr        else
          if(ixt.gt.0) then
            c1a=ctt/float(ixt)
          else
            c1a=0
          endif    
          write(nlog,452) ixt, c1a, c2t, ctt
cr        endif


        goto 500
      endif
c
c _________________________________________________________
c 
c               Step 7; Daily Base flow information file (*.xbx)
c                       Called 1x/day from vircom
      if(ityp.eq.5) then
c
c rrb 012/01/14; Initilize
        fx=faf(mon)

c
c rrb 2005/03/02; Print heading to *.xbx in vircom like monthly        
cr      if(idy.eq.1) then
cr        write(74,120) cunit,iystr, xmonam(1), iyend, xmonam(12),
cr   1       (i, i=1,13)
cr      endif

        do irn=1,numrun
          is=irusta(irn)   

          x = tempq(is)-tempi(is)+tempd(is)
     1       -tempr(is)+tempw(is)+temps(is)+tempe(is)
          xx = amax1(0.0,x)
          ic = ifix((tempq(is)*fx-0.5))
          if(ic.eq.-999) then
            x  = -999./fx
            xx = -999./fx
          endif
c
c               Daily baseflow information (74=*.xbx) & 
c               Scratch file (75)
c rrb 01/04/03; *.xbx in cfs (fx=1.0)
          fx=1.0
          iprint=0
          if(ichk.ne.25) iprint=1
          if(ichk.eq.25 .and. cstaid(is).eq. ccall) iprint=1
c         write(nlog,*) 'Virout; ', ichk, cstaid(is), ccall
          if(iprint.eq.1) then
            write(74,301)
     1        iyrmo(mon),   xmonam(mon),  idy, cstaid(is),
     1        tempq(is)*fx,   tempi(is)*fx,
     1        tempd(is)*fx,   tempr(is)*fx, tempw(is)*fx,
     1        temps(is)*fx,   tempe(is)*fx, x*fx, xx*fx,
     1        tempts(is)*fx,  tempfs(is)*fx, tempcu(is)*fx,
     1        templ(is)*fx,   tempp(is)*fx, tempDre(is)*fx, 
     1        tempUse(is)*fx, tempRre(is)*fx, 
     1        stanam1(is)
          endif

          fx=factor
          irec=irec+1
          write(75,rec=irec) 
     1      tempq(is)*fx,   tempi(is)*fx,
     1      tempd(is)*fx,   tempr(is)*fx, tempw(is)*fx,
     1      temps(is)*fx,   tempe(is)*fx, x*fx, xx*fx,
     1      tempts(is)*fx,  tempfs(is)*fx, tempcu(is)*fx,
     1      templ(is)*fx,   tempp(is)*fx, tempDre(is)*fx, 
     1      tempUse(is)*fx, tempRre(is)*fx 
        end do
c
c               Fill array with 31 values per month
        idx=31-mthday(mon)
        if(idy.eq.mthday(mon) .and. idx.gt.0) then
c         write(nlog,*) ' Virout;',  idy, mthday(mon)
          do ix=mthday(mon)+1,idx
            do is=1,numrun
              irec=irec+1
              write(nlog, 110) iyrmo(mon), imomo(mon), idy,
     1                       irec
              write(75,rec=irec) (zero(i), i=1,nout)
            end do
          end do
        endif

        goto 500
      endif
c
c _________________________________________________________
c
c               Step 7; Return

  500 RETURN
c
c _________________________________________________________
c               Formats
c

  110   format(' Virout; Year  Mon  Day Irec',/,9x, 4i5)
  120   format(///,' Naturalized Flow Estimate Information ', a5,
     1   ' From ', i5, 1x, a4, ' To ', i5, 1x, a4,/          
     1   ' Note: Annual Average Base Flows have negatives set to zero',/
     1   '       Divert includes diversions from all sources ',
     1          '(priority, storage, exchange, etc.) and uses '
     1          '(irrigate, storage, recharge, ...)',/
     1   '       Return includes returns from diversions & wells',/
     1   '       Wel Dep includes immediate and lagged depletions',/
     1   '       CU does include net reservoir evaporation',/
     1   '       Note: Ground Water storage to maintain streamflow ',
     1          'at or greater than zero (To_From_GW_Stor) ',
     1          'is not included',/)
     
cx  121   format(/,'Naturalized Flow Information ', a4,/ 
cx     1   '                           ',
cx     1   '    Gauged    Import    Divert    Return      Well',
cx     1   '     Delta       Net        To     Total   w/o (-)',' |',
cx     1   '        To      From          '/
cx     1   'Year  Mon  Day River ID    ',
cx     1   '      Flow       (-)       (+)       (-)   Dep (+)',
cx     1   '   Sto (+)   Evp (+)   Rch (+) Base Flow Base Flow',' |',
cx     1   '     SoilM     SoilM        CU      Loss   Pumping',
cx     1   ' River Name',/,
cx     1   27x, 10('      (', i2,')'),' |',
cx     1   5('      (', i2,')'), ' (', i2,')', /
cx     1   3('____ '), '____________', 10(' _________'),  
cx     1   ' |',5(' _________'),1x, 24('_'))  
     
  121   format(/,'Naturalized Flow Information ', a4,/ 
     1   '                           ',
     1   '    Gauged    Import    Divert    Return      Well',
     1   '     Delta       Net     Total   w/o (-)',' |',
     1   '        To      From                              ',
     1   '    Divert    Divert Reservoir'/
     1   'Year  Mon  Day River ID    ',
     1   '      Flow       (-)       (+)       (-)   Dep (+)',
     1   '   Sto (+)   Evp (+) Base Flow Base Flow',' |',
     1   '     SoilM     SoilM        CU      Loss   Pumping',
     1   '   To Rech    To Use   To Rech River Name',/,
     1   27x, 9('      (', i2,')'),' |',
     1   8('      (', i2,')'), ' (', i2,')', /
     1   3('____ '), '____________', 9(' _________'),  
     1   ' |',8(' _________'),1x, 24('_'))         
     
  180   FORMAT(i4,1x, a12,14F8.0, '  Base Flow')
  300   format(i4, 1x,a4, i5, 1x, a12, 9f10.0, ' |',8f10.0,1x, a24)
  301   format(i4, 1x,a4, i5, 1x, a12, 9f10.2, ' |',8f10.2,1x, a24)
  302   format(i4, 1x,a4, i5, 1x, a12, f10.3, i10)
  350   format(2i4, 1x, a12, 31f8.2, f8.0)

  420   format(' Ave', 1x,a4,   6x, a12, 9f10.0, ' |', 
     1   8f10.0,  1x, a24)
  440   format(
     1   3('____ '), '____________', 9(' _________'),  
     1   ' |',7(' _________'),1x, 24('_'),/
     1   'Ave ', 1x,'Tot '6x, a12, 9f10.0, ' |',
     1   8f10.0, 1x, a24)

  410   format(//,72('_'),/                                      
     1   '  Virout; Negative Flows at GAGE Summary ',a5,'/',a3,/
     1   '    Note: Count is the # of months for a monthly model and',/
     1   '             the # of days for a daily model and ',/
     1   '           Est is the average negative flow estimate ',/
     1   '             on a monthly basis (af/mo) or a daily basis',
     1            ' (af/day)',/
     1   '           Adj is the adjusted value printed to results',/
     1   '           Total is the total adjustment (abs(# * Est)',/
     1   '                                                ',
     1   '              Est         Adj       Total       River',/
     1   '    # ID           Name                        Count ',
     1   '      af/',a3,'      af/',a3,'        acft          ID',/
     1   ' ____ ____________ ________________________ ________ ',
     1   4(' ___________'))
  450   format(i5, 1x, a12, 1x, a24, 1x, i8, 1x, 3f12.2, i12)
  452   format(
     1   ' ____ ____________ ________________________ ________',
     1   1x,4(' ___________'),/
     1   5x, 1x, 'Total Ave   ', 1x, 24x, 1x, i8, 1x, 3f12.2)

  470   format(//,72('_'),/                                                                            
     1   ' Virout; Negative Flows at BASEFLOW LOCATIONS Summary ',a5,
     1   '/', a3,/
     1   '   Note: Count is the # of months for a monthly model and',/
     1   '           the # of days for a daily model and ',/
     1   '         Est is the average negative flow estimate ',/
     1   '           on a monthly basis (af/mo) or a daily basis',
     1            ' (af/day)',/
     1   '           Total is the total adjustment (abs(# * Est)',/
     1   '                                                     ',
     1   '         Est         Adj       Total       River',/
     1   '    # ID           Name                        Count  ',
     1   '     af/',a3,'      af/',a3,'        acft          ID',/
     1   ' ____ ____________ ________________________ ________',1x
     1   4(' ___________'))

c
c _________________________________________________________
c
c               Error Processing
  928 write(nlog,929) iin2, filena
  929 format('  Virout; Problem reading file # ', i4,
     1       '   File name: ', /,a256)
      goto 9999
 9999 write(6,*) '  Stopped in Virout, see the log file (*.log)'
      write(nlog,*) '  Stopped in Virout'
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop 


      END




