c
c *********************************************************
c
      subroutine sjrip(iw,l2,isjon,divact)
c
c
c _________________________________________________________
c	Program Description
c
c       Sjrip; It simulates operating rule type 20
c              San Juan RIP operation of Navajo Reseroir
c              per Draft Sjrip Hydrology Model Documentation 
c              (3/24/00)
c               Approach:
c                 1. Copied powsea (type 9)
c                 2. Adjusted accordingly
c
c
c _________________________________________________________
c       Documentation
c
c               imonsw(k,im) = switch 0=off, 1=on    
c               isjon        = counter to keep track of when 
c                              the routine is called
c                              0=no operation yet calculate demand, 
c                              1=yes operation
c
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'       
      dimension sjrelm(13)
      character cwhy*48

c
c _________________________________________________________
c
c               Step 1; Initilize
c
c               Temporary 
      iout=0
      if(ichk.eq.120) iout=1

      sjevap = 0.0


      divact = 0.0
      iw = iw
c
c rrb 98/08/10
      small=0.001
      small2=0.1
c
c rrb 98/03/03; Daily capability
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif
c
c              Find reservoir (nr), owner (iown), river location (iscd)
c                and # of downstream nodes (ndns)
      NR  =IOPSOU(1,L2)
      IOWN=NOWNER(NR)+IOPSOU(2,L2)-1
      ISCD=IRSSTA(NR)
      NDNS=NDNNOD(ISCD)
c
c _________________________________________________________
c
c               Step 2; Return based on imonsw() monthly on off
c                       switch and if already operated (isjon)
c
c _________________________________________________________
c
c               Step X; Branch if not on this month
c
c rrb 04/22/96; Allow month switch
      if(imonsw(l2,mon).eq.0) then
        iwhy=1
        cwhy='Monthly switch is off'
        goto 150
      endif
c
c ---------------------------------------------------------
c		For a daily model set demand for beginning of season
      if(iday.eq.1 .and. imonsw(l2,mon).gt.0) then
        if (idy.lt.imonsw(l2,mon)) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 150
        endif  
      endif  
c
c ---------------------------------------------------------
c		For a daily model set demand for end of season
      if(iday.eq.1 .and. imonsw(l2,mon).lt.0) then
        if (idy.gt.iabs(imonsw(l2,mon))) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 150
        endif  
      endif  
      
c _________________________________________________________
c
c               Step 3; Set San Juan Specific parameters
c
c               Branch around all release calculations if 
c               no demand (already satisfied in a prior iteration)
      if(isjon.gt.0) then
        if(abs(sjrelm(mon)-small2) .gt.small2) then
          isjon=isjon+1
c         write(io99,*) sjrelm(mon), small2, sjrelm(mon)-small2
          goto 100
        else
          goto 120
        endif
      endif
c
c               Set counter
      isjon=isjon + 1 
c
c               Initilize annual release totals 
      if(iyr-iystr.eq.0) then
        sjrel0 = 0.0
        sjrel1 = 0.0
        sjrel2 = 0.0
        sjrel3 = 0.0
      endif
c
c               Update annual release totals
      if(iyr-iystr.ge.1) then
        sjrel3 = sjrel0 + sjrel2
        sjrel2 = sjrel0 + sjrel1
        sjrel1 = sjrel0
        sjrel0 = 0.0   
      endif
c
c               sjspill = Probable Spill (0=no,1=yes)
c               isjpertb   = Pertebation 0=no, 1=yes
c
c               Available water variables
c                 sjavail    = available Water
c                 cursto(nr) = current storage
c                 irsmin(nr) = pointer to target data
c                 sjtarget( ) = forecast through July, a negative value,
c                              via *.tar
c                 sjrela     = average release (600 cfs) via *.opr
c                 sjmina     = min release via *.opr
c                 sjrel      = calculated release (annual)
c                 sjrelm(im) = calculated release (monthly)
c
c               Available Space Variables
c                 sjspace    = available space
c                 volamx(nr) = maximum storage via *.res
c                 deadst(nr) = dead storage via *.res
c                 cursto(nr) = current storage
c                 sjtar7     = July target 
c
c               Probable Spill variables
c                 sjspill    = probable spill
c                 irsmin(nr) = pointer to target data
c                 sjtarget( ) = forecast through July, a negative value,
c                              via *.tar
c                 sjrela     = average release (600 cfs) via *.opr  
c                 sjspace    = available space
c                 sjevap     = estimated evap
c                 volmax(nr) = reservoir capacity
c                 deadst(nr) = dead storage
c
c _________________________________________________________
c
c               Step 3a Calculate available water
c
      nrx = irsmin(nr)
      sjavail = cursto(nr) - sjtarget(mon, nrx) - sjrela*fac - sjmina
      if(iout.eq.1) then
        write(io99,*) ' ' 
        write(io99,230) 'sjrela', iyrmo(mon), xmonam(mon),0,sjrela,fac
        write(io99,230) 'sjavail', iyrmo(mon), xmonam(mon), 1, sjavail, 
     1    cursto(nr), sjtarget(mon, nrx), sjrela*fac, sjmina
      endif
c
c rrb   Store in 1000 af for integer math
      isjavail = ifix(sjavail/1000.)
c
c _________________________________________________________
c
c               Step 3b; Calculate available space
c
      if(cyr1 .eq. '  CYR') nx= 7
      if(cyr1 .eq. '  WYR') nx=10
      if(cyr1 .eq. '  IYR') nx= 9
      sjtar7=sjtarget(nx,nrx)

      sjspace = volmax(nr) - deadst(nr) - cursto(nr) - sjtar7
      if(iout.eq.1) then
        write(io99,230) 'sjspace',iyrmo(mon), xmonam(mon), 2, sjspace, 
     1    volmax(nr), deadst(nr), cursto(nr), sjtar7 
      endif
c
c _________________________________________________________
c
c               Step 3c; Estimate evaporation
c               TEmporarily set to 0.0
      sjevap = 0.0
c
c _________________________________________________________
c
c               Step 3d; Calculate probable spill (cannont be < 0)
c
      sjspill  = -1*sjtarget(mon,nrx) - (sjspace + sjrela*fac + sjevap)
      sjspill  = amax1(0.0, sjspill)
      if(iout.eq.1) then
        write(io99,230) 'sjspill',iyrmo(mon), xmonam(mon), 3, sjspill,
     1    sjtarget(mon,nrx), sjspace, sjrela*fac, sjevap
      endif
      isjspill = ifix(sjspill)
c
c _________________________________________________________
c
c               Step 3e; Calcualte available water + spill
c
      sjavsp  = sjavail +  sjspill
      if(iout.eq.1) then
        write(io99,230) 'sjavsp',iyrmo(mon), xmonam(mon), 3, sjavail, 
     1                   sjspill
      endif
c
c _________________________________________________________
c
c               Step 3f; Set maximum release and full hydrograph
c
      sjmax = 344000.
      sjfull= 344000.

      isjpertb = ifix(sjpertb)
c
c _________________________________________________________
c
c       Step 3g; Process per SJRIP logic
c       write(io99,*) ' Sjrip; Avail 0 <= 114000', isjavail


      case1x: select case (isjavail)
c
c _________________________________________________________
c
c               Mode 1 (avail < 114,000)

        case (:114) case1x
c         write(io99,*) ' Sjrip; Avail <= 114000', sjavail
          isjmode=1
          sjRel=0.0
          call sjripmo(io99, isjmode, sjrel, sjrelm, cyr1)
          goto 100
c
c _________________________________________________________
c
c               Modes 2 - 13 (avail > 114000)
c
        case default case1x
          case2x: select case (isjspill) 
c
c               Release Mode 2-5 (avail > 114,000 & no probable spill
c                 and yes or no pertubation
            case (0) case2x
c
c               Mode 2 (avail > 114,000, No spill, yes pertubation
              if(isjpertb.eq.1) then
                isjmode=2
                sjrel = 114000.
                call sjripmo(io99, isjmode, sjrel, sjrelm, cyr1)
                goto 100
              else
c
c               Mode 3 (avail > 114,000, No spill, no pertubation,
c                 and Misc flow conditions
                if(sjrel3.lt.344000.0 .and. sjrel1.lt.166000.0) then
                  isjmode=3
                  sjrel= 114000.
                  call sjripmo(io99, isjmode, sjrel, sjrelm, cyr1)
                  goto 100
                endif
c
c               Mode 4 (avail > 114,000, No spill, no pertubation,
c                 and Misc flow conditions
                if(sjrel3.lt.344000.0 .and. sjrel1.ge.166000.0) then
                  isjmode=4
                  sjrel= 0.
                  call sjripmo(io99, isjmode, sjrel, sjrelm, cyr1)
                  goto 100
                endif
c
c               Mode 5 (avail > 114,000, No spill, no pertubation,
c                 and Misc flow conditions
                if(sjrel3.ge.344000.0 .and. sjrel2.ge.0.0) then
                  isjmode=5
                  sjrel= 0.
                  call sjripmo(io99, isjmode, sjrel, sjrelm, cyr1)
                  goto 100
                endif
              endif
c
c               Print Warning if logic through
              write(io99,*) ' Sjrip; Problem with logic at #1'
              goto 9999
c
c
c _________________________________________________________
c
c               Modes 6 - 13 (avail > 114000 and yes spill)

            case default case2x 
c
c               Mode 6 (avail > 114,000, Yes spill, Spill > 344,000
              if(sjspill.ge.344000.0) then
                isjmode=6
                sjrel= sjfull
                call sjripmo(io99, isjmode, sjrel, sjrelm, cyr1)
                goto 100
              else
c
c               Mode 7 (avail > 114,000, Yes spill, Spill < 344,000,
c                 and spill Misc flow conditions              
                if(sjavsp.ge.344000.0 .and. sjrel3.lt.344000) then
                  isjmode=7
                  sjrel = sjfull
                  call sjripmo(io99, isjmode, sjrel, sjrelm, cyr1)
                  goto 100
                endif
c
c               Mode 8 (avail > 114,000, Yes spill, Spill < 344,000,
c                 and Misc flow conditions              
                if(sjavsp.ge.344000.0 .and. sjrel3.ge.344000.0 .and.
     1             sjrel1.lt.166000.0) then
                  isjmode=8
                  sjrel = 166000.0
                  call sjripmo(io99, isjmode, sjrel, sjrelm, cyr1)
                  goto 100
                endif
c
c               Mode 9 (avail > 114,000, Yes spill, Spill < 344,000,
c                 Yes pertebation and Misc flow conditions              
                if(sjavsp.ge.344000.0 .and. sjrel3.ge.344000.0 .and.
     1             sjrel1.ge.166000.0 .and. isjpertb.eq.1) then
                  isjmode=9
                  sjrel = 166000.0
                  call sjripmo(io99, isjmode, sjrel, sjrelm, cyr1)
                  goto 100
                endif
c
c               Mode 10 (avail > 114,000, Yes spill, Spill < 344,000,
c                 and Misc flow conditions              
                if(sjavsp.ge.344000.0 .and. sjrel3.ge.344000.0 .and.
     1             sjrel1.ge.166000.0 .and. isjpertb.eq.0) then
                  isjmode=10
                  sjrel = 114000.0
                  call sjripmo(io99, isjmode, sjrel, sjrelm, cyr1)
                  goto 100
                endif
c
c               Mode 11 (avail > 114,000, Yes spill, Spill < 344,000,
c                 and Misc flow conditions              
                if(sjavsp.lt.344000.0 .and. sjrel3.lt.344000.0) then
                  isjmode=11
                  sjrel = sjmax
                  call sjripmo(io99, isjmode, sjrel, sjrelm, cyr1)
                  goto 100
                endif
c
c               Mode 12 (avail > 114,000, Yes spill, Spill < 344,000,
c                 and Misc flow conditions              
                if(sjavsp.lt.344000.0 .and. sjrel3.ge.344000.0 .and.
     1             sjrel1.ge.166000.0) then  
                  isjmode=12
                  sjrel = 114000.0
                  call sjripmo(io99, isjmode, sjrel, sjrelm, cyr1)
                  goto 100
                endif
c
c               Mode 13 (avail > 114,000, Yes spill, Spill < 344,000,
c                 and Misc flow conditions              
                if(sjavsp.lt.344000.0 .and. sjrel3.ge.344000.0 .and.
     1             sjrel1.lt.166000.0) then  
                  isjmode=13
                  sjrel = 114000.0
                  call sjripmo(io99, isjmode, sjrel, sjrelm, cyr1)
                  goto 100
                endif
              endif
c
c               Print Warning if logic through
              write(io99,*) ' Sjrip; Problem with logic at #2'
              goto 9999                                      
          end select case2x
      end select case1x  

 100  continue 
c
c _________________________________________________________
c
c               Step 3h; Set monthly release

      resval = sjrelm(mon)
c
c               Detailed output then exit when monthly demand = 0
      if(resval.lt.small2) then
        if(isjon.eq.1) write(22,200)   
        write(22,210) iyrmo(mon), xmonam(mon), sjavail, sjspill, 
     1    sjavsp, sjpertb, sjrel3, sjrel1, sjrel2, sjrel, sjrelm(mon),
     1    resval, cursto(nr), isjmode, isjon
        goto 120
      endif
c
c
c _________________________________________________________
c
c               Step 4; Limit release based on volume available
c                       in reservoir (cursto(nr) or account (curown() 

c
c rrb 02/05/97; Allow target release from all accounts
      if(iopsou(2,l2).gt.0) then
        resval=amax1(amin1(resval, cursto(nr), curown(iown)),0.)
      else
        resval=amax1(amin1(resval, cursto(nr)), 0.0)
      endif
c
c _________________________________________________________
c
c               Step 5; Calculate release in cfs, return if small
c
      RAVCFS=RESVAL/fac                   
      IF(RAVCFS.LE.small) Goto 120
c
c _________________________________________________________
c
c               Step 6; Calculate flow available (floavl) to be
c                       max release (flomax) - flow in river (river).
c                       Return if small

                
      FLOAVL=AMAX1(FLOMAX(NR)-RIVER(ISCD),0.)
      IF(FLOAVL.LE.small) Goto 120
c _________________________________________________________   
c
c               Step 7; Determine release (divact) in cfs based on
c                       limiting amount (release or volume)
      IF(FLOAVL.LE.RAVCFS) then
        divact=floavl
      else
        divact=ravcfs
      endif
c
c _________________________________________________________
c
c               Step 8; Add release to downstream & reservoir
c
      TEMP=-DIVACT
      CALL TAKOUT(maxsta, AVAIL ,RIVER ,AVINP ,QTRIBU,IDNCOD,
     1            TEMP  , NDNS,  ISCD)
      AVAIL (ISCD)=AVAIL (ISCD)-DIVACT
c
c _________________________________________________________
c
c               Step 9; REDUCE RESERVOIR STORAGE BY RELEASE (divact)
c                       and set miscellaneous 
c
      RELAF=DIVACT*fac
      cursto1=cursto(nr)
      CURSTO(NR) = CURSTO(NR)-RELAF
      PROJTF(NR) = PROJTF(NR)+DIVACT
C
      POWREQ(NR) = POWREQ(NR)-RELAF
      POWREL(NR) = POWREL(NR)+DIVACT
c
c               Detailed output
      if(isjon.eq.1) write(22,200)

      write(22,210) iyrmo(mon), xmonam(mon), sjavail, sjspill, 
     1  sjavsp, sjpertb, sjrel3, sjrel1, sjrel2, sjrel, sjrelm(mon),
     1  relaf,  cursto(nr), isjmode, isjon
c
c               Update release totals and monthly demand (sjrelm())
      sjrel0 = sjrel0 + relaf
      sjrelm(mon) = amax1(sjrelm(mon)-relaf, 0.0)
c
c _________________________________________________________
c
c               Step 10; Distribute release to accounts
c rrb 02/05/97; Distribute to account assigned (iopsou>0) or
c               based on current storage in all accounts
c               (iopsou=0)
      if(iopsou(2,l2).gt.0) then
        CUROWN(IOWN)=CUROWN(IOWN)-RELAF
        accr(19,iown) = accr(19,iown) + relaf
      else
        ct=0.0
        iown=nowner(nr)-1
        if(iopsou(2,l2).eq.0) then
          nrown1=nowner(nr+1)-nowner(nr)
        else
c          nrown1=abs(iopsou(2,l2))
          write(99,111) iopsou(2,l2)
          goto 9999
        endif

        if(iout.eq.1) then        
          write(99,*) ' '
          write(99,*)'  Sjrip; nr iown nowner(nr+1) nowner(nr) nrown1'
          write(99,*) nr, iown, nowner(nr+1), nowner(nr), nrown1
          write(99,*)'  Sjrip; n1, relaf, curown(n1), cursto1, c, ct'
        endif
c
        do n=1,nrown1
          n1=iown + n
          c=(curown(n1)/cursto1)*relaf
          curown(n1)=curown(n1)-c
          accr(19,n1) = accr(19,n1) + c
          ct=ct+c
          if(iout.eq.2) then
            write(99,*) '  Sjrip;'
            write(99,'(i5,10f8.0)') n1, relaf, curown(n1),
     1            cursto1, c, ct
          endif
        end do

        if(ABS(ct-relaf).gt.small2) then
          write(6,*) 'Sjrip; Problem in allocating target release '
          write(6,*) '        to accounts ct, relaf, ct-relaf, small2'
          write(6,*)          ct, relaf, ct-relaf, small2

          write(99,*) 'Sjrip; Problem in allocating target release '
          write(99,*) '        to accounts ct, relaf, ct-relaf, small2'
          write(99,*)          ct, relaf, ct-relaf, small2
          goto 9999
        endif
      endif
c
c _________________________________________________________
c
c               Step 11; Set data for operation file (*.xop) output

 120  divo(l2)=divo(l2)+divact
c
c _________________________________________________________
c
c               Step 12; Check for roundoff issues
c
c rrb 99/05/10; Roundoff check (note 15 is subroutine type for chekres
      call chekres(io99,maxres, 1, 15, iyr, mon, nr,nowner,
     1                    curown,cursto,cresid)
c
c _________________________________________________________   
c
c               Step 13; Return

 150  RETURN
c
c _________________________________________________________   
c
c               Formats
c
c
 111   format('  Sjrip; Can only make a target release', /,
     1           'to 1 or all accounts. iopsou(2,l2) = ', i5)

 200   format(//' San Juan RIP Rule Decision Tree Data ',/
     1  '          ',
     1  '                        Avail+    Pertu-   Release',
     1  '   Release   Release',/
     1  ' Year  mon',
     1  '     Avail     Spill     Spill    bation   3-years',
     1  '    1-year   2-years AnnDemand MonDemand    MonRel',     
     1  '   EndStor      Mode   Counter',/
     1  '  (1)  (2)',
     1  '       (3)       (4)       (5)       (6)       (7)',
     1  '       (8)       (9)      (10)      (11)      (12)',      
     1  '      (13)      (14)      (15)'/
     1  ' ____ ____',
     1  ' _________ _________ _________ _________ _________',
     1  ' _________ _________ _________ _________ _________', 
     1  ' _________ _________ _________')
 210  format(i5,1x, a4, 11f10.0, 2i10)
 230  format('  Sjrip;', a8, i5, 1x, a4, i5, 20f10.0)
c
c _________________________________________________________
c               Error Trackingc

 9999 write(6,*) '  Stopped in Sjrip, see the log file (*.log)'
      write(99,*) '  Stopped in Sjrip'
      write(6,*) 'Stop 1' 
      call flush(6)
      call exit(1)

      stop 
      end
c
c **************************************
c
        Subroutine sjripmo(io99, isjmode, sjrel, sjrelm, cyr1)
        dimension sjrelm(13)
        character cyr1*5

        iout=0

        do i=1,12
          sjrelm(i) = 0.0
        end do

        if(cyr1.eq.'  CYR') then
          ia=4
          im=5
          ij=6
        endif

        if(cyr1.eq.'  WYR') then
          ia = 7
          im = 8
          ij = 9
        endif
        
        if(cyr1.eq.'  IYR') then
          ia = 6
          im = 7
          ij = 8
        endif      
c
c               0 Hydrograph
        if(isjmode.eq.1  .or. isjmode.eq.4  .or. isjmode.eq.5) then
          sjrelm(ia) = 0.0
          sjrelm(im) = 0.0
          sjrelm(ij) = 0.0
          goto 100
        endif
c
c               114,000 Hydrograph
        if(isjmode.eq.2  .or. isjmode.eq.3  .or. isjmode.eq.10 .or.
     1     isjmode.eq.12 .or. isjmode.eq.13) then
          sjrelm(ia) = 0.0
          sjrelm(im) = 26381.
          sjrelm(ij) = 114000. - sjrelm(im) + 1.0
          goto 100
        endif
c
c               166,000 Hydrograph
        if(isjmode.eq. 8 .or. isjmode.eq. 9) then
          sjrelm(ia) = 0.0
          sjrelm(im) = 61290.
          sjrelm(ij) = 166000. - sjrelm(im) + 1.0
          goto 100
        endif
c
c               236,000 Hydrograph
        if(isjmode.eq. 8 .or. isjmode.eq. 9) then
          sjrelm(ia) = 0.0
          sjrelm(im) = 87474.
          sjrelm(ij) = 236000. - sjrelm(im) + 1.0
          goto 100
        endif
c
c               344,000 Hydrograph
        if(isjmode.eq. 6 .or. isjmode.eq. 7 .or. isjmode.eq.11) then
          sjrelm(ia) = 3174.
          sjrelm(im) = 163440.
          sjrelm(ij) = 344000. - sjrelm(ia) - sjrelm(im) + 1.0
          goto 100
        endif

 100    continue
c
c _________________________________________________________
c               Calculate Annual total
        do i=1,12
          sjrelm(13) = sjrelm(13) + sjrelm(i)
        end do
c
c _________________________________________________________
c               Print results

        if(iout.eq.1) then
          write(io99,110) cyr1, (sjrelm(i), i=1,13), sjrel,
     1         sjrelm(13) - sjrel
        endif
c
c _________________________________________________________
c               Return
        return
c _________________________________________________________
c               Formats
 110    format('  sjripmo for year type ', a5,/,12f8.0, 20f10.0)
c
c _________________________________________________________
c               End
        end




     
