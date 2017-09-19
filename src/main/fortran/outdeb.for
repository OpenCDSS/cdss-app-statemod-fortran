c
c *********************************************************
C
      SUBROUTINE OUTDEB(idat,nreach)
c
c
c _________________________________________________________
c	Program Description
c
c       Outdeb; Print data check information
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'

      real*8 rrtemm
      character cu*1, namez*24, cix*12, cix2*12, rec3*3, cwrid*12,
     1 rec12*12, cname1*24, cname2*24
c
c _________________________________________________________
c		Step 1; Initilze

      iout=0
      if(iout.eq.1) then
        WRITE(nlog,430) ntorig
        write(nlog,442)
      endif
      
C
C-------------------------------------------------------------------
C
C------  WATER RIGHT RANK
C
C-------------------------------------------------------------------
C
      call outtop(11,1,4)     
      WRITE(11,430) ntorig
c
c               Print notes at top
      write(11,450)      
      write(11,440)
      
      nw2=0
c
      DO 150 IW=1,NTORIG
c
        cix = '-1          '
        cix2= '-1          '
        cWrId='-1          '
              
        dcx = -1.0
        cu  = ' '
        rrtemm = -1.0
        inoff = -1
        icname=1

        cname1=""
        cname2=""

        L1=NWRORD(1,IW)
        L2=NWRORD(2,IW)
        ityp1=L1
        
C                       
c              Instream 
        if(l1.eq.1) then
          rrtemm=rFRNK(L2)
          Inoff=IIFRSW(L2)
          nd = iifrco(l2)
          cix = cifrid(nd)
          dcx = dcrifr(l2)
          cu = 'c'
          namez = namei(l2)
          cname1=xfrnam1(nd)
          cWrId=cisfwr(l2)
          Goto 148
        endif
C          
c              Reservoir
        if(l1.eq.2) then
          rrtemm=rRSNK(L2)
          Inoff=IRSRSW(L2)
          nd = iresco(1,L2)
          cix = cresid(nd)
          dcx = dcrres(l2)
          namez = namer(l2)
          cu = 'a'

          cname1=resnam1(nd)
          cWrId=creswr(l2)          
          Goto 148
        endif
C          
c              Direct Flow
        if(l1.eq.3) then
c         write(99,'(i4, 2x, f20.5)') iw, rdvnk(l2)
          rrtemm=rDVNK(L2)
          Inoff=IDVRSW(L2)               
          cu = 'c'
          nd = idivco(1,l2)
          cix = cdivid(nd)
          dcx = dcrdiv(l2)
          namez = named(l2)
          cname1=divnam1(nd)
          cWrId=crigid(l2)          
          Goto 148
        endif
C      
c              Power     
        if(l1.eq.4) then
          rrtemm=rPWNK(L2)
          Inoff=IPOWSW(L2)
          cu = 'c'
          Goto 148
        endif
C                     
c              Operational Right
        if(l1.eq.5) then
          icname=0
          rrtemm=rOPNK(L2)
          Inoff=IOPRSW(L2)
          cu = 'x' 
          namez = nameo(l2)
          nd = iopdes(1,l2)
          cWrId=corid(l2)
          ityp1=100+ityopr(l2)

c
c               Branch occurs when right is turned off
          if(nd.eq.0) goto 148
c
c              Instream  operational right
          if(ityopr(l2).eq.1)  then
            cix = cifrid(nd)
            icname=1
             cname1=xfrnam1(nd)
          endif
c
c              Direct operational right
          if(ityopr(l2).eq.2)  then
            if(nd.gt.0) then
              cix = cdivid(nd)
              icname=1
              cname1=divnam1(nd)
            endif

            if(nd.lt.0) then
              cix = cdivid(-nd)
              icname=1
              cname1=resnam1(-nd)
            endif
          endif

          goto 148
        endif
c      
c rrb 98/11/25; Wells     
        if(l1.eq.6) then
          rrtemm=rdvnkw(L2)
          Inoff=idvrsww(L2)               
          cu = 'c'
          nd = idivcow(1,l2)
c
c               Well right tied to a well structure             
          cix = cdividw(nd)
          cWrId=crigidw(l2)
          cname1=divnamw1(nd)

c
c               Well right tied to a diversion
          nw2=idivcow2(nd)
          if(nw2.gt.0) then
            cix2 = cdivid(nw2)
            cname2=divnam1(nw2)
          end if

          dcx = dcrdivw(l2)
          namez = namedw(l2)
c         write(99,*) '  Outdeb; nd = ', nd 
          Goto 148
        endif
C
 148    nx = nwrord(1,iw)
        if(iw.ge.2) then
          n0 = nwrord(1,iw-1)
          if(nx.eq.5 .and. n0.ne.nx) write(11,*) ' '
          if(n0.eq.5 .and. n0.ne.nx) write(11,*) ' '
        endif
        
        if(iout.eq.1) 
     1    WRITE(nlog,422)  l1,l2,IW,cWrId,ityp1,rrtemm,
     1                   Inoff, cix, cix2, dcx, cu,
     1                   namez
c
c               Correct problem with cname 
        if(icname.eq.1) then
          if(nw2.eq.0) then
            WRITE(11,420)  IW,cWrId,ityp1,rrtemm,
     1                     Inoff, cix, cix2, dcx, cu,
     1                     namez, cname1
          else
            WRITE(11,420)  IW,cWrId,ityp1,rrtemm,
     1                     Inoff, cix, cix2, dcx, cu,
     1                     namez, cname1, cname2,
     1                     l1,l2
          endif
        else
          WRITE(11,420)  IW,cWrId,ityp1,rrtemm,
     1                   Inoff, cix, cix2, dcx, cu,
     1                   namez

        endif
        if(iout.eq.1) 
     1    WRITE(nlog,422)  l1,l2,IW,cWrId,ityp1,rrtemm,
     1                   Inoff, cix, cix2, dcx, cu,
     1                   namez
  150 CONTINUE
c
c               Print footnotes at bottom
c     write(11,450)
c
c               Branch if only requesting water right dat
      if(idat.eq.0) goto 500
C
C-------------------------------------------------------------------
C
C------  Preliminary Output Request File (*.xou)
C
C-------------------------------------------------------------------
C
        do i=1,maxsta
          dumsta(i) = 0
        end do

        write(21,470)                   
        rec3='DIV'
        do nd=1,numdiv
          dumsta(idvsta(nd)) = 1
          write(21,460) cdivid(nd), divnam1(nd), rec3, 0
        end do

        rec3='ISF'
        do nd=1,numifr
          dumsta(ifrsta(nd)) = 1
          write(21,460) cifrid(nd), xfrnam1(nd), rec3, 0
        end do

        rec3='RES'
        do nd=1,numres
          dumsta(irssta(nd)) = 1
          write(21,460) cresid(nd), resnam1(nd), rec3, 0
        end do

        rec3='WEL'
        do nd=1,numdivw
          dumsta(idvstaw(nd)) = 1
          write(21,460) cdividw(nd), divnamw1(nd), rec3, 0
        end do

        rec3='FLO'
        do nd=1,numrun
          dumsta(irusta(nd)) = 1
          write(21,460) crunid(nd), runnam1(nd), rec3, 1
        end do

        rec3='OTH'
        do nd=1,numsta
          if(dumsta(nd).eq.0) 
     1      write(21,460) cstaid(nd), stanam1(nd), rec3, 0
        end do

        rec3='OPR'
        do nd=1,numopr
          write(21,462) corid(nd), nameo(nd), rec3, 0
        end do
        write(21,460) '-999        '
        
C
C-------------------------------------------------------------------
C
C------  Dimension Data
C
C-------------------------------------------------------------------
C
                            
      Write(99,190)
  190 format('',///,10x,'Dimension Allocation of StateMod',//,
     1       '  Comment                                  ',
     1       '    MAXIMUM         USED        DELTA',/
     1       '  __________________________________________',
     1       '  __________________________________',/)
C
      Write(99,200) MAXSTA,NUMSTA,maxsta-numsta
  200 FORMAT('   Stations (MAXsta, NUMSTA);              ',I11,2I13)
C
      Write(99,270) MAXRUN,NUMRUN,maxrun-numrun
  270 FORMAT('   Streamflow gages (MAXrun, NUMRUN);      ',I11,2I13)
C
      Write(99,271) MAXrch,nreach,Maxrch-nreach
  271 FORMAT('   Stream Reaches (MAXrch, nreach) (1);    ',I11,2I13)
C
c
c rrb 2011/04/04; Update
c     Write(99,272) MAXbas,maxbas, maxbas-maxbas
      Write(99,272) MAXbas,numbas, maxbas-numbas
  272 FORMAT('   Gages for baseflows (MAXbas)(1);        ',I11,2I13)
C
c ---------------------------------------------------------------------
c  
c      write(99,490)
c
      Write(99,220) MAXDIV,NUMDIV,maxdiv-numdiv
  220 FORMAT('   Diversion stations (MAXdiv, NUMDIV);    ',I11,2I13)

      Write(99,350) MAXUSE,NDIVIN,maxuse-ndivin
  350 FORMAT('   Diversion demands (MAXuse, NDIVIN);     ',I11,2I13)
C
      Write(99,320) MAXUSE,NUMUSE,maxuse-numuse
  320 FORMAT('   Diversion owners (MAXuse, NUMUSE);      ',I11,2I13)
C
      Write(99,300) MAXDVR,NUMDVR,maxdvr-numdvr
  300 FORMAT('   Diversion rights (MAXdvr, NUMDVR);      ',I11,2I13)
C
      Write(99,230) MAXownD,NUMownD,MaxOwnD-numownD
  230 FORMAT('   Diversion accounts (MAXownD, NUMownD);  ',I11,2I13)
C
      Write(99,233) MAXRTN,NUMRTN,maxrtn-numrtn
  233 FORMAT('   Div. Return Locations (MAXrtn, NUMRTN); ',I11,2I13)
c
c ---------------------------------------------------------------------
c  
c      write(99,490)
c
      Write(99,210) MAXRES,NUMRES,maxres-numres
  210 FORMAT('   Reservoirs stations (MAXres, NUMRES);   ',I11,2I13)
c
      Write(99,211) MAXRES,NRSACT,maxres-nrsact
  211 FORMAT('   Res stations & accts (MAXres, NRSACT);  ',I11,2I13)
C
      Write(99,310) MAXOWN,NUMOWN,maxown-numown
  310 FORMAT('   Reservoir accounts (MAXown, NUMOWN);    ',I11,2I13)
C
      Write(99,290) MAXRSR,NUMRSR,maxrsr-numrsr
  290 FORMAT('   Reservoir rights (MAXrsr, NUMRSR);      ',I11,2I13)
C
      Write(99,291) MAXRtnrp,NUMrtnrp,maxrtnrp-numrtnrp
  291 FORMAT('   Res. return flows (MAXrtnRP, numrtnRP); ',I11,2I13)
c
cx   Write(99,212) MAXREp, irepn, maxrep-irepn
      Write(99,212) MAXREp, maxrep, maxrep-maxrep
  212 FORMAT('   Replacement Reservoirs (MAXrep, irepn); ',I11,2I13)
c
c ---------------------------------------------------------------------
  
c      write(99,490)
c
      Write(99,280) MAXIFR,NUMIFR,maxifr-numifr
  280 FORMAT('   Instream stations (MAXifr, NUMIFR);     ',I11,2I13)
C
      Write(99,282) MAXFrR,NUMFRR,maxfrr-numfrr
  282 FORMAT('   Instream rights (MAXfrr, NUMFRR);       ',I11,2I13)
C
      Write(99,287) MAXrea,NUMrea,0
  287 FORMAT('   Instream nodes / reach (MAXrea,NUMrea); ',I11,2I13)

c ---------------------------------------------------------------------
  
c      write(99,490)
c
      Write(99,281) MAXdivw,NUMdivw,maxdivw-numdivw
  281 FORMAT('   Well stations (MAXdivw, NUMdivw);       ',I11,2I13)
C
      Write(99,283) MAXdvrw,NUMdvrw,maxdvrw-numdvrw
  283 FORMAT('   Well rights (MAXdvrw, NUMdvrw);         ',I11,2I13)
C
      Write(99,231) MAXRTNw,NUMRTNw,maxrtnw-numrtnw
  231 FORMAT('   Well Return Locations (MAXrtnw,NUMrtnw);',I11,2I13)
C
      Write(99,232) MAXRTNw,NUMRTNw2,maxrtnw-numrtnw2
  232 FORMAT('   Well Dep Locations (MAXrtnw,NUMRTNw2);  ',I11,2I13)
c
c ---------------------------------------------------------------------
c  
c      write(99,490)
c
      Write(99,284) MAXplnt, Maxplnt, 0
  284 FORMAT('   Plan types (MAXplnt, NUMplnt) (1);      ',I11,2I13)
C
      Write(99,285) MAXplan,NUMplan,maxplan-numplan
  285 FORMAT('   Plan stations (MAXplan, Numplan);       ',I11,2I13)
C
      Write(99,286) MAXrtnpp, numrtnpp, maxrtnpp-numrtnpp
  286 FORMAT('   Plan Returns (MAXrtnpp, NUMrtnpp);      ',I11,2I13)
c
c --------------------------------------------------------------------- 
c      write(99,490)
C                 
      Write(99,240) MAXDLY,NUMDLY,maxdly-numdly
  240 FORMAT('   Delay Tables (MAXdly, NUMDLY);          ',I11,2I13)
C

c     Write(99,241) MAXDLa,Ndlymx/12,maxdla-ndlymx/12
c 241 FORMAT('   Delay values years  (MAXdla, NDLYMX/12);',I11,2I13)
 
      Write(99,242) MAXDLm,Ndlymx,maxdlm-ndlymx
  242 FORMAT('   Delay values months  (MAXdlm, NDLYMX);  ',I11,2I13)
                        
      nx=ndlymx/12*366
cx    Write(99,243) MAXDLd,nx,maxdld-nx                       
cx243 FORMAT('   Delay values days  (MAXdld, NUMdld);    ',I11,2I13)  
      Write(99,243) MAXDLd,nx,maxdld-nx                       
  243 FORMAT('   Delay values days  (MAXdld, NUMdld) (1);',I11,2I13)    
cc
c ---------------------------------------------------------------------
c  
c      write(99,490)
c
      Write(99,250) MAXPRE,NUMPRE,maxpre-numpre
  250 FORMAT('   Precip (MAXpre, NUMPRE);                ',I11,2I13)
C
      Write(99,260) MAXEVA,NUMEVA,maxeva-numeva
  260 FORMAT('   Evap (MAXeva, NUMEVA);                  ',I11,2I13)
C
c ---------------------------------------------------------------------
  
c      write(99,490)
C
      Write(99,340) MAXOPR,NUMOPR,maxopr-numopr
  340 FORMAT('   Operation rights (MAXopr, NUMOPR);      ',I11,2I13)
c
      Write(99,360) MAXNWR,NWRTOT,maxnwr-nwrtot
  360 FORMAT('   Total structure rights (MAXnwr, NWRTOT);',I11,2I13)

      Write(99,370) MAXNWR,NTORIG,maxnwr-ntorig
  370 FORMAT('   Total str + opr rights (MAXnwr, NTORIG);',I11,2I13)
C
      Write(99,361) MAXgrp, NUMgrp, maxgrp-numgrp
  361 FORMAT('   Total CU groups (MAXgrp, NUMgrp) (1);   ',I11,2I13)     
  
      write(99,362) 
  362 format(/,
     1 '   (1) Dimension used may be reported as 0 if an ',
     1        'option (e.g. daily model) or',/
     1 '       report (e.g. Cu Report) is not used.')

c
c ---------------------------------------------------------------------
c  
c      write(99,490)

  420 format(i8, 1x,a12,1x, i8, 4x,f12.5, i8, 2x,a12, 1x,a12, f12.3, 
     1       1x, a1,1x, a24, 2x,a24, 2x,a24, 20i4)
  422 format(3i8,1x,a12,1x, i8, 4x,f12.5, i8, 2x, a12, 1x, a12, f12.3, 
     1       1x,a1, 1x,a24, 2x,a24, 2x,a24, 20i4)
c
c
c _________________________________________________________
c

c              Formats
  430 FORMAT(
     1   '#',/,
     1   '# *.xwr; Water Right Information',/,
     1   '#        Number of rights = ', i10)
  440  FORMAT('#', /, '#',/,
     1 '#   Rank ID               Type         Admin #  On/Off ',
     1 ' Str ID #1    Str ID #2   ',
     1 '        Amount',
     1 ' Right Name                Str Name #1',
     1 '               Str Name #2',/
     1 '#    (1) (2)               (3)             (4)     (5)',
     1 '  (6)          (7)                    (8) (9)',
     1 '                       (10)                       (11)',/
     1 '#_______ _____________ _______ _______________ _______ ',
     1 ' ____________ ____________   ___________',
     1   1x, 24('_'), 2x, 24('_'), 2x, 24('_'),/)
  442  FORMAT('#', /, '#',/,
     1 '#    I1      I2     Rank ID               Type         Admin #',
     1 '  On/Off ',
     1 ' Str ID #1    Str ID #2   ',
     1 '        Amount',
     1 ' Right Name                Str Name #1',
     1 '               Str Name #2',/
     1 '#    (1)     (2)     (3) (4)               (5)             (6)',
     1 '     (7) (8)          (9)           (10)',/
     1 '#_______ _______ _______ _____________ _______ _______________',
     1 ' _______ ',
     1 ' ____________ ____________   ___________',
     1   1x, 24('_'), 2x, 24('_'), 2x, 24('_'),/)
  450 format('#',/,'# ', 120('_'),/,2('#',/), '# Where:',/
     1 '#  1. Rank           = Water right basin rank',/
     1 '#  2. Type           = Water right type',/
     1 '#                   1=Instream,',/
     1 '#                   2=Reservoir,',/
     1 '#                   3=Diversion,',/
     1 '#                   4=Power,',/
     1 '#                   5=Operational,',/
     1 '#                   6=Well,',/
     1 '#  3. Admin #        = Administration Number',/
     1 '#  4. On/Off         = On or Off switch',/
     1 '#     Note: Certain operating rules may cause a structure to',/
     1 '#           be turned off since if it is controlled by an',/
     1 '            operating rule',/
     1 '#                   0=off',/
     1 '#                   1=on',/
     1 '#                  +n=begin in year n',/
     1 '#                  -n=stop in year n',/
     1 '#  5. Str Id #1      = Primary structure for this right',/,
     1 '#  6. Str Id #2      = Secondary structure for this right ',
     1                        '(-1=N/A)',
     1 '#  7. Amount         = Decreed capacity & unit (c=CFS, a=AF)',/,
     1 '#  8. Right Name     = Water right name',/,
     1 '#  9. Str Name #1    = Primary structure for this right',/
     1 '# 10. Str Name #2    = Secondary structure for this right ',
     1                        '(-1=N/A)')
 460  format(a12, 1x, a24, 1x, a3, 1x, i5)
 462  format(a12, 1x, a24, 1x, a3, 1x, i5)
 470  format(
     1 '#',/
     1 '# *.xou; Preliminary station request file',/
     1 '#',/
     1 '# Type (e.g. Diversion, StreamGage, Reservoir, InstreamFlow ', 
     1    'or Well)',/
     1 '# ',/
     1 'Diversion',/
     1 '#',/
     1 '# Parameter (e.g. Total_Supply, Sim_EOM, River_Outflow, ...)',/ 
     1 '# ',/
     1 'Total_Supply',/
     1 '#',/
     1 '# ID Name Type and Print Code (0=no, 1=yes);',/
     1 '# Note: id = All prints all',/
     1 '#       id = -999 = stop',/ 
     1 '#       default is to turn on all stream gages (FLO)',/
     1 '#',/
     1 '# ID         Name                     Typ OnOff  ',/
     1 '#b----------eb-----------------------eb-exb---e')
 480  format(
     1 '#',/
     1 '# *.xrh (*.rch); Preliminary reach ID',/
     1 '#',/
     1 '# ID         Name                     Typ  Zone',/
     1 '#b----------eb-----------------------eb-exb---e')
 490  format(' ',/,80('_'),/)
c
c _________________________________________________________
c

 500  RETURN
      END
