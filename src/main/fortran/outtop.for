c
c *********************************************************
c
       subroutine outtop(nout, itype, nx)
c
c
c _________________________________________________________
c	Program Description
c
c       Outtop; It prints the header to a file
c
c
c _________________________________________________________
c       Documentation
c               nout  = output file #
c               itype = 0 monthly time series header
c                      -1 daily time series header
c                       1 non time series output
c                       2 non time series output with time stamp
c                         for daily outspd
c               nx    = output ID (e.g. *.xbi, see below)
c                      Code to stamp info on output
c
c _________________________________________________________
c       Update History
c
c rrb 02/045/07; Revised non time series header (itype=1)
c                to include POR and unit information to satisfy
c                the GUI need to process daily (*.xds via outspd.f)       
c
c
c _________________________________________________________
c	Dimensions
c
       include 'common.inc'
c
c rrb 7/29/94 Additional Output
       dimension idatx(3), itim2(4)
       character cunit*5
       dimension want2(70), titleh(70)
       character want2*10,  titleh*51, cstep*8
       character cyr*5, dtype*45 

        data want2/
     1     '*.xbi     ', '*.xna     ', '*.xwb     ', 
     1     '*.xwr     ', '*.xsu     ',
     1     '*.xrg     ', '*.xdg     ', '*.xrc     ', 
     1     '*.xdc     ', '*.xcu     ', '*.xrx     ',
     1     '*.xsc     ', '*.xst     ', '*.xsh     ', 
     1     '*.xbm     ', '*.xdd     ', '*.xre     ',
     1     '*.xbn     ', '*.xnm     ', '*.xop     ',
     1     '*.xev     ', '*.xdl     ', '*.xcb     ',
     1     '*.xcd     ', '*.xci     ', '*.xsp     ',
     1     '*.xir     ', '*.xwd     ', '*.xbg     ',
     1     '*.xdy     ', '*.xry     ', '*.xrp     ',
     1     '*.xwe     ', '*.xgw     ', '*.xwy     ',
     1     '*.xrg     ', '*.xbi     ', '*.xbm     ',
     1     '*.xsj     ', '*.xss     ', '*.xby     ',
     1     '*.xby     ', '*.xbx     ', '*.xbx     ',
     1     '*.xs2     ', '*.xds     ', '*.xd2     ',
     1     '*.xtp     ', '*.xbr     ', '*.xpl     ',
     1     '*.xca     ', '*.xwd     ', '*.xwd     ',
     1     '*.Ydc     ', '*.Ysc     ', '*.Ywb     ',
     1     '*.Yrc     ', '*.Ywc     ', '*.Ycu     ',
     1     '*.xrh     ', '*.xwp     ', '*.xpw     ',
     1     '*.tmp     ', '*.xgn     ', '          ',
     1     '          ', '          ', '          ',
     1     '          ', '          '/

        data titleh/
     1   ' Base flow information at stream gauge locations  ',
     2   ' Detailed node accounting (Summary)               ',
     3   ' Water Budget                                     ',
     4   ' Water rights list sorted by basin rank           ',
     5   ' Water Supply Summary                             ',
     6   ' Reservoir Graph                                  ',
     7   ' Diversion Graph                                  ',
     8   ' Reservoir Comparison                             ',
     9   ' Diversion Comparison                             ',
     1   ' Consumptive Use Summary for the CU model or other',
     1   ' River data Summary                               ',
     2   ' Stream Comparison                                ',
     3   ' Standard diversion (*.xdd) and reservoir (*.xre) ',
     4   ' Shortage Summary                                 ',
     5   ' Base Flow Results - Monthly                      ',
     6   ' Diversion Summary                                ',
     7   ' Reservoir Summary                                ',
     8   ' Binary File Listing (*.xdd)                      ',
     9   ' Detailed node accounting                         ',
     2   ' Operational Right Diversion Summary              ',
     1   ' Reservoir Evaporation                            ',
     2   ' Diversion Structure List                         ',
     3   ' Base Flow Check                                  ',
     4   ' Diversion Demand Check (SW diversions only)      ',
     5   ' Instream Flow Check                              ',
     6   ' Special Parameter (Matrix)                       ',
     7   ' Instream Reach Data                              ',
     8   ' CU Summary by Water District                     ',
     9   ' Base Flow at Stream Gages                        ',
     3   ' Daily Diversion Summary                          ',
     1   ' Daily Reservior Summary                          ',
     2   ' Replacement Reservoir Detail                     ',
     3   ' Well Structure Summary                           ',
     4   ' Ground Water Budget                              ',
     5   ' Daily Well Structure Summary                     ',
     6   ' Rio Grande Compact                               ',
     7   ' Baseflow data at gauges via BaseflowX option     ',
     8   ' Baseflows Gaged and Ungaged via BaseflowX option ',
     9   ' San Juan RIP                                     ',
     4   ' Structure Summary                                ',
     1   ' Baseflow Results - Daily                         ',
     2   ' Baseflow results - Daily via BaseflowX option    ',
     3   ' Baseflow Information - Daily                     ',     
     4   ' Baseflow Information via Baseflowx - Daily       ',
     5   ' Special Parameter (Column)                       ',
     6   ' Daily Special Parameter (Matrix)                 ',
     7   ' Daily Special Parameter (Column)                 ',
     8   ' Temporary Data File                              ',
     9   ' Binary File Listing (*.xre)                      ',
     5   ' Plan',
     1   ' Call Data File',
     2   ' Supply Summary by Water District                 ',
     3   ' Shortage Summary by Water District               ',
     4   ' Stream Comparison Report                         ',
     5   ' Diversion Comparison Report                      ',
     6   ' Water Budget Report                              ',
     7   ' Reservoir Comparison Report                      ',
     8   ' Well Comparison Report                           ',
     9   ' Consumptive Use Report                           ',
     6   ' Reach Report Data                                ',
     1   ' Well Plan Summary                                ', 
     2   ' Augmentation Plan Well Summary                   ', 
     3   ' Temporary                                        ',
     4   ' Unadjusted BaseFlow',
     1    ' ', ' ', ' ', ' ', ' ', ' '/
c
c _________________________________________________________
c		Step 1; Initilze
c
       iout=0
       
       if(iout.eq.1) write(99,*) '  Outtop; nout', nout
c       

       if(itype.eq.0)  cunit=cunitm

       if(itype.eq.-1) cunit=cunitd
c
c rrb 2006/02/27; Establist a unit for standard outputs for baseflow       
       if(itype.eq.-2) cunit=cunitdX
       
       if(iday.eq.0)   cstep='Monthly '
       if(iday.eq.1)   cstep='Daily   '
c
c _________________________________________________________
c               Get date and time
       call dattim(idatx, itim2, isgi)
c
c               Get proper dates on output
         im1 = 1
         im2 = 12                
         iy1 = iystr

         if(cyr1.eq.'  WYR') then
           im1 = 10
           im2 = 9  
           iy1 = iystr - 1
         endif                            

         if(cyr1.eq.'  IYR') then
           im1 = 11
           im2 = 10      
           iy1 = iystr - 1
         endif
c
c _________________________________________________________
c               Print Monthly time series header
         if(itype.eq.0) then
           write(nout,160) 
     1                want2(nx), titleh(nx),
     1                headin1(1), headin1(2),
     1                ver, vdate, 
     1                idatx(3), idatx(2), idatx(1), (itim2(j), j=1,3),
     1                cstep,
     1                (xmonam(j), j=1,12),
     1                im1, iy1, im2, iyend, cunit, cyr1 
         endif
c
c _________________________________________________________
c               Print Daily time series header
         if(itype.le.-1) then
           write(nout,161) 
     1                want2(nx), titleh(nx),
     1                headin1(1), headin1(2),
     1                ver, vdate, 
     1                idatx(3), idatx(2), idatx(1), (itim2(j), j=1,3),
     1                cstep,
     1                (j, j=1,31),
     1                im1, iy1, im2, iyend, cunit, cyr1 
         endif

c
c _________________________________________________________
c               Print non time series header
         if(itype.eq.1) then
           write(nout,162) 
     1                want2(nx), titleh(nx),
     1                headin1(1), headin1(2),
     1                ver, vdate, 
     1                idatx(3), idatx(2), idatx(1), (itim2(j), j=1,3),
     1                cstep
c          write(nout,100) ibm, iby, iem, iey, cunit, cyr
cx         write(nout,100) imomo(1), iyrmo(1), imomo(12), iyend,
cx   1       cunitd2, cyr1
         endif
c
c _________________________________________________________
c               Print non time series header with time stamp
         if(itype.eq.2) then
           write(nout,162) 
     1                want2(nx), titleh(nx),
     1                headin1(1), headin1(2),
     1                ver, vdate, 
     1                idatx(3), idatx(2), idatx(1), (itim2(j), j=1,3),
     1                cstep
c          write(nout,100) ibm, iby, iem, iey, cunit, cyr
           write(nout,100) imomo(1), iyrmo(1), imomo(12), iyend,
     1       cunitd2, cyr1
         endif
c
c                      Formats
 100  format(
     1  '#',/
     1  '# MoB  YrB       MoE  YrE Unit YTyp',/
     1  '# ___ ____      ____ ____ ____ ____',/
     1  i5, 1x, i4, 5x, i5, 1x, i4, a5, a5)
 160    format(
     1   '# ',/,
     1   '# ',80('_'),/,
     1   '#',/
     1   '# ',a10, 1x, a50,/
     1   '#',/, 
     1   '# ', a80,/,
     1   '# ', a80,/,
     1   '#',/, 
     1   '# Statemod Version:  ',a8,
     1   ' (',a10,')',/,
     1   '# Run date and time: ', i5, 2('/',i2), 1x,
     1                                 2(i2,':'), i2,/,
     1   '# Time Step:         ', a8,/
     1   '# ',/
     1   '# ',80('_'),/,
     1   '# Yr Station ID  ',12(4X,a4),'   TOTAL',/,
     1   '#___ ____________',13(' _______'),/
     1   i5,'/',i4,'  -  ',i5,'/',i4,a5,a5)
 161    format(
     1   '# ',/,
     1   '# ',80('_'),/,
     1   '# ',a10, 1x, a50,/
     1   '#',/, 
     1   '# ', a80,/,
     1   '# ', a80,/,
     1   '#',/, 
     1   '# Statemod Version:  ',a8,
     1   ' (',a10,')',/,
     1   '# Run date and time: ', i5, 2('/',i2), 1x,
     1                                 2(i2,':'), i2,/,
     1   '# Time Step:         ', a8,/
     1   '# ',/
     1   '# ',80('_'),/,
     1   '# Yr  Mo Station ID  ',31('    (',i2,')'),'     TOTAL',/,
     1   '#___ ___ ____________',31(' _______'),' _________'/
     1   i5,'/',i4,'  -  ',i5,'/',i4,a5,a5)

 162    format(
     1   '#',/, 
     1   '# ',80('_'),/, 
     1   '# ',a10, 1x, a50, /, 
     1   '#',/, 
     1   '# ', a80, /,
     1   '# ', a80, /,
     1   '#',/, 
     1   '# Statemod Version: ',a8,
     1   ' Date = ',a10,')',/,
     1   '# Run date:         ', i3, 2('/',i2), 1x,
     1                                 2(i2,':'), i2,/,
     1   '# Time Step:         ', a8,/
     1   '# '/,
     1   '# ',80('_'))
c  10/1951  -      9/1991 AF/M  WYR     
c
c _________________________________________________________
c
        return
        end


