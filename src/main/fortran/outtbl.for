c
c *********************************************************
c                                                              
      SUBROUTINE OUTTBL(NUMHIS)
c
c
c _________________________________________________________
c	Program Description
c
c       Outtbl; It prints summary tables of input data
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
       dimension
     1 oprtyp(50),   cdem(5)

        character
     1 recres*1,     cdemx*8,      cdem*8, 
     1 namez*12,     name1*12,     name2*12,  blank*12,
     1 oprtyp*30,    cix2*12,      cname1*24, cname2*24
c
        data oprtyp/
     1     'Res to Instream by River      ', 
     2     'Res to Diversion by River     ', 
     3     'Res to Diversion by Carrier   ',
     4     'Res to Diversion by Exchange  ', 
     5     'Res to Reservoir by Exchange  ',
     6     'Res to Reservoir by Bookover  ',
     7     'Res to Carrier by Exchange    ',
     8     'Out of Priority Bookover      ',
     9     'Res to River by Target        ',
     1     'General Replacement Reservoir ',
     1     'Carrier                       ',
     2     'Reoperation                   ',
     3     'Instream by Index Flow        ',
     4     'Carrier with Diversion Limit  ',
     5     'Interruptable Supply          ',
     6     'Direct Flow Storage           ',
     7     'Rio Grande Compact Rio Grande ',
     8     'Rio Grande Compact Conejos    ',
     9     'Split Channel Operation       ',
     2     'San Juan RIP Operation        ',
     1     'Wells with Sprinkler          ',
     2     'Soil Moisture                 ',
     3     'Downstream Call               ',
     4     'Direct Flow Exchange          ',
     5     'Direct Flow Bypass            ',
     6     'Various to a T&C or Aug Plan  ',
     7     'Reuse Plan to Various Direct  ',
     8     'Reuse Plan to Various Exchange',
     9     'Reuse Plan Spill              ',
     3     'Reservoir ReDiversion         ',
     1     'Carrier with Reuse            ',
     2     'Various with Reuse Direct     ',
     3     'Various with Reuse Exchange   ',
     4     'Reservoir to Reservoir Transfr',
     5     'Import with Reuse             ',
     6     'Seasonal Operation            ',
     7     'Well Augmentation to a Plan   ',
     8     'Out of Priority Diversion     ',
     9     'Alternate Point               ',
     4     'South Platte Compact          ',
     1     'Storage with Special Limits   ',
     2     'Plan Reset                    ',
     3     'In Priority Well Depletion    ',
     4     'Recharge Well                 ',
     5     'Carrier with Transit Loss     ',
     6     'Multiple Ownership            ',
     7     'Administration Plan Limits    ',
     8     'Plan or Reservoir to Plan Dir ',
     9     'Plan or Reservoir to Plan Exc ',
     5     ' '/

        data cdem /
     1     ' Mon-Tot', ' Ann-Tot', ' Mon-Iwr', ' Ann-Iwr', '    Zero'/
c                Similar to outsyt but it prints average
c _________________________________________________________
c

        blank = '            '
        small = 0.001
c
c rrb 2008/05/19; irpt=0 original operating fule format
c		  irpt=1 similar to original but includes Source ID
c		  irpt=2 smaller version for reports        
        irpt=2
c
c               Lines per page (lpp)
        lpp=60          
        rn = iyend - iystr +1
c
c _________________________________________________________
c
c
c               a. Print river network summary

        write(19,90) numdiv, numres, numifr, numdivw, nplan, numhis,
     1               numdiv+ numres+ numifr+ numdivw+ nplan+ numhis
c  90    format(''/,
   90    format(   /,
     1   'Table 4.1.1a',/
     1   'River Network Elements',//
     1   ' Type           ,  Number',/
     1   ' ______________ ,________ ',/
     1   ' Diversion      ,',i8,/
     1   ' Reservoirs     ,',i8,/
     1   ' Instream Flow  ,',i8,/
     1   ' Wells          ,',i8,/     
     1   ' Plans          ,',i8,/     
     1   ' Stream Flow    ,',i8,/
     1   ' ______________ ,________,',/
     1   ' Total          ,',i8)  

c
c _________________________________________________________
c
c
c               b. Print base flow data comparison
         write(19,100) iystr, iyend
c 100    format(''/,
  100    format(   /,
     1   'Table 4.1.4a',/
     1   'Stream Flow Comparison',/
     1   'Average (af/yr), ',i4, ',-,', i4,//
     1   '    #, ID          ,  Name                   ,',
     1   '   BaseFlow,   Historic, Difference',/,
     1   '  ___, ____________, ________________________,',
     1   ' __________, __________, __________')
              
         npp=0
         do 120 nh=1,numhis
c
           npp=npp+1
           if(npp.gt.lpp) then
             npp=1
             write(19,100) iystr, iyend
           endif

c          iss=ihisid(nh) 
           iss=ifix(dummy(nh,4)) 
c              
c rrb 99/09/17; Check for missing data
           if(abs(dum3(13,iss)+999.0) .lt. 0.01) then
             c = -999.0*rn
             dum3(13,iss)=dum3(13,iss)*rn
           else
             c = dum2(13,iss) - dum3(13,iss)
           endif

           write(19,110) nh, cstaid(iss),stanam1(iss),
     1                   dum2(13,iss)/rn, dum3(13,iss)/rn, c/rn
  110      format(1x, i4,',',1x, a12,',', 1x, a24,',', 
     1            2(1x,f10.0,','), 1x,f10.0)
  120    continue
c
c _________________________________________________________
c
c
c               c. Diversion Structure Summary
         write(19,130) iystr, iyend
c 130    format('',/,  
  130    format(    /,  
     1   'Table 4.2.1a',/
     1   'Diversion Structure Summary',/
     1   'Average ',i4, '-', i4,//
     1   '    #, ID          ,',
     1   ' Name                    ,',
     1   ' Location                ,',
     1   ' Cap-cfs,',
     1   ' Area-ac,',
     1   ' EffAve%,',
     1   ' EffMax%,',
     1   '  Dem-af, DemType,  On/Off',/,
     1   ' ____, ____________,',
     1   ' ________________________,',
     1   ' ________________________,',6(' _______,'), ' _______')
c
         npp=0
         do 160 nd=1,numdiv
c
c               Print header
           npp=npp+1
           if(npp.gt.lpp) then
             npp=1
             write(19,130) iystr, iyend
           endif
c
c               Calculate average efficiency
           divefcA = 0.0    
           ce = 0.0
           do 140 im=1,12
             if(diveff(im,nd).gt.0.0) then
               divefcA = divefcA+diveff(im,nd)
               ce = ce+1.0
             endif
  140      continue
           if(ce.gt.0) divefcA = divefcA/ce
c
c rrb 00/06/30; Print demand type
           do i=1,5
             if(idvcom(nd).eq.i) cdemx=cdem(i)
           end do

           iss =idvsta(nd)
c
c rrb 00/12/20; Print maximum efficiency (effmax)
           write(19,150) nd, cdivid(nd),divnam1(nd),
     1                   stanam1(iss), divcap(nd), area(nd),
     1                   divefcA,effmax(nd), 
     1                   dummy(iss,1)/rn, cdemx,idivsw(nd)
  150      format(1x, i4,',', 1x, a12,',', 1x, a24,',', 1x, a24,',', 
     1            5(f8.0,','),a8, ',', i8)
  160    continue
c
c _________________________________________________________
c
c
c               d. Print diversion return flow summary

         write(19,170)
c 170    format('',/,
  170    format(/,
     1   'Table 4.2.1b',/
     1   'Diversion Return Flow Summary',//,
     1   '     , Structure:  ,                         ,',
     1         ' Returns To: ,                         ,',
     1   '        ,        ,',/
     1   '    #, ID          , Name                    ,',
     1         ' ID          , Name                    ,',
     1   '       %,   Table, Sum Tbl,   Net %, Cum Net',/
     1   ' ____, ____________, ________________________,',
     1         ' ____________, ________________________,',
     1   ' _______, _______, _______, _______, _______')
            
      npp=0
      do 190 nd=1,numdiv
        if(idivsw(nd).eq.0) go to 190
c
        nui=nrtn(nd)
        nue=nrtn(nd+1)-1
        if(nui.gt.nue) go to 190
        ct=0.0
c
          do 180 n=nui,nue
            is =irnsta(n)                                   
c
c rrb 99/10/16; Correction
c           irid = irtnid(irtndl(n))
            irid = irtndl(n)
            npp=npp+1
            ct=ct+pcttot(n)*dlytot(irid)/100.0

            if(n.eq.nui) then
c
c               Print header
              if(npp.gt.lpp) then
                npp=1
                write(19,170)
              endif
c
c rrb 99/10/05; Allow sum of table to be < 100.
              write(19,200) nd, cdivid(nd), divnam1(nd),
     1          cstaid(is), stanam1(is),pcttot(n),
     1          irtnid(irid),
     1          dlytot(irid), pcttot(n)*dlytot(irid)/100.0, ct
            else
              write(19,210)
c
c rrb 99/10/05; Allow sum of table to be < 100.
c
c    1          cstaid(is), stanam1(is),pcttot(n), irid,
     1          cstaid(is), stanam1(is),pcttot(n),
     1          irtnid(irid),
     1          dlytot(irid), pcttot(n)*dlytot(irid)/100., ct
            endif
  180     continue
c
c               Print Loss
          if(pctlos(nd).gt.small) then
            ct=ct+pctlos(nd)
            write(19,212) 'Loss        ', pctlos(nd),ct
          endif

c         write(19,*) ' '
  190   continue
  200   format(
     1         i5,',', 1x, a12,',', 1x, a24,',', 
     1         1x, a12,',', 1x, a24,',', f8.2,',', i8,',',
     1         f8.2, ',', f8.2,',',f8.2)
  210   format(5x ',',     13x,',',     25x,',',
     1         1x, a12,',', 1x, a24,',', f8.2,',', i8,','
     1         f8.2, ',', f8.2,',',f8.2)
  212   format(5x ',',     13x,',',     25x, ',',
     1         1x, a12,',', 1x, 24x,',', 8x, ',', 8x,','
     1         8x,',', f8.2,','f8.2)
c
c
c _________________________________________________________
c
c               e. Print Direct Flow Rights
         write(19,220) 
c 220   format('',/,
  220   format(/,
     1   'Table 4.2.5a',/
     1   'Diversion Water Right Summary (cfs)',//
     1   '    #, ID          ,', 
     1   ' Name                    ,',
     1   ' Admin #     ,     Decree, Cum Decree,  On/Off  Right #',/
     1   ' ____, ____________,',
     1   ' ________________________,',
     1   ' ____________, __________, __________, _______, _______')
 
      npp=0
      do 260 nd=1,numdiv
        nx = 0
        tot=0.0
        do 250 k=1,numdvr
          nd1 = idivco(1,k)
          if(nd.eq.nd1) then
            npp=npp+1
            tot=tot+dcrdiv(k)
            if(nx.eq.0) then
c
c               Print header
              if(npp.gt.lpp) then
                npp=1
                write(19,220)
              endif
              nx = 1
              write(19,230)  nd, cdivid(nd),divnam1(nd),
     1                        rdvnk(k), dcrdiv(k), tot, idvrsw(k),npp
  230          format(1x, i4,',', 1x, a12,',', 1x, a24,',', 
     1                1x, f12.5,',', 1x, f10.2,',', f10.2,',',i8,',',i8)
            else
               write(19,240) rdvnk(k), dcrdiv(k), tot, idvrsw(k),npp
  240          format(    5x,',',     13x,',',     25x,',',
     1                1x, f12.5,',', 1x, f10.2,',',f10.2,',',i8,',',i8)
            endif
          endif            
  250   continue
  260 continue
c
c _________________________________________________________
c
c
c               f. Print reservoir summary
  269    write(19,270) 
c 270    format('',/,
  270    format(    /,
     1   'Table 4.3.1a',/
     1   'Reservoir Structure Summary',//
     1   '    #, ID          ,', 
     1   ' Name                    ,',
     1   ' Location                ,',
     1   '   Cap. (af),',
     1   '    # Owners,  On/Off,',/
     1   ' ____, ____________,',
     1   ' ________________________,',
     1   ' ________________________,',2(' ___________,'), ' _______')

         npp=0
         do 290 nr=1,numres
c
c               Print header
           npp=npp+1
           if(npp.gt.lpp) then
              npp=1
              write(19,270)
           endif

           iss =irssta(nr)
           n = nowner(nr+1) - nowner(nr)
           write(19,280) nr, cresid(nr),resnam1(nr),
     1                   stanam1(iss),
     1                   volmax(nr), n, iressw(nr)
  280      format(1x, i4,',', 1x, a12,',', 1x, 
     1           2(a24,',', 1x), f11.0,',', 1x,i11,',',i8)
  290    continue
c
c               Print Reservoir Rights
         write(19,300)
  300    format(
c    1   '',/,       
     1   //,
     1   'Table 4.3.5a',/
     1   'Reservoir Water Right Summary (af)',//
     1   '    #, ID          ,', 
     1   ' Name                    ,',
     1   ' Admin #     ,     Decree,       Fill,  On/Off  Right #',/
     1   ' ____, ____________,',
     1   ' ________________________,',
     1   ' ____________, __________, __________, _______, _______')
  310    format(/,
     1   'Table 5.5.', i2,'b',/
     1   'Operating Rules for ID = ', a12,/
     1   'Reservoir Water Right Summary (af)',//
     1   '    #, ID          ,', 
     1   ' Name                    ,',
     1   ' Admin #     ,     Decree,       Fill,  On/Off',/
     1   ' ____, ____________,',
     1   ' ________________________,',
     1   ' ____________, __________, __________, _______')


      npp=0
      do 360 nr=1,numres
        nx = 0
        tot=0
        do 350 k=1,numrsr
          nr1 = iresco(1,k)
          if(nr.eq.nr1) then
            npp=npp+1
            tot=tot+dcrres(k)
            if(nx.eq.0) then
c
c               Print header
              if(npp.gt.lpp) then
                npp=1
                write(19,300) 
              endif         

              nx = 1
              write(19,320) nr, cresid(nr),resnam1(nr),
     1                      rrsnk(k), dcrres(k), n2fill(k), irsrsw(k)
  320         format(1x, i4,',', 1x, a12,',', 1x, a24,',', 
     1               1x,f12.5,',', 1x,f10.2,',', 1x, i10,',',i8,',',i8)
            else
              write(19,330) rrsnk(k), dcrres(k), n2fill(k), irsrsw(k)
  330         format(    5x,',',     13x,',',     25x,',',       
     1               1x,f12.5,',', 1x,f10.2,',', 1x, i10,',',i8,',',i8)
  340         format(1x, i4,',',     13x,',',     25x,',',
     1               1x,f12.5,',', 1x,f10.2,',', 1x, i10,',',i8,',',i8)
            endif
          endif            
  350    continue
  360  continue
c
c _________________________________________________________
c
c
c               g. Print instream summary
         write(19,370) iystr, iyend
c 370    format('',/,
  370    format(/,
     1   'Table 5.4.1a',/
     1   'Instream Flow Structure Summary',/
     1   'Average (af/yr) ,',i4, ',-,', i4,//
     1   '    #, ID          ,', 
     1   ' Name                    ,',
     1   ' Location                ,',
     1   '  Dem-af, DemType,  On/Off,',/
     1   ' ____, ____________,',
     1   ' ________________________,',
     1   ' ________________________,',2(' _______,'), ' _______')

         npp=0
         do 390 nf=1,numifr
c
c               Print header
            npp=npp+1
            if(npp.gt.lpp) then
              npp=1
              write(19,370)
           endif
c
c
c rrb 00/06/30; Print demand type
           do i=1,5
             if(iifcom(nf).eq.i) cdemx=cdem(i)
           end do

           iss =ifrsta(nf)
           write(19,380) nf, cifrid(nf),xfrnam1(nf),
     1                   stanam1(iss),
c    1                   toti(iss)/rn, ifrrsw(nf)
     1                   dummy(iss,2)/rn, cdemx, ifrrsw(nf)
  380      format(1x, i4,',', 1x, a12,',', 
     1            1x, 2(a24,',', 1x), f7.0,',',a8, ',', i8)
  390    continue
c
c _________________________________________________________
c
c
c               h. Print Instream Rights
         write(19,400)
  400    format(
c    1   '',/,       
     1   //,
     1   'Table 5.4.3a',/
     1   'Instream Flow Water Right Summary (cfs)',//
     1   '    #, ID          ,', 
     1   ' Name                    ,',
     1   '      Admin #,     Decree, Cum Decree   On/Off, Right #',/
     1   ' ____, ____________,',
     1   ' ________________________,',
     1   ' ____________, __________, __________, _______, _______')

      npp=0
      do 440 nf=1,numifr
        nx = 0
        tot=0.0
        do 430 k=1,numfrr
          nf1 = iifrco(k)
          if(nf.eq.nf1) then
            npp=npp+1
            tot=tot+dcrifr(k)
            if(nx.eq.0) then
c
c               Print header
              if(npp.gt.lpp) then
                npp=1
                write(19,400)
              endif
              nx = 1
               write(19,410) nf, cifrid(nf),xfrnam1(nf),
     1                       rfrnk(k), dcrifr(k), tot, iifrsw(nf),npp
  410          format(1x, i4,',', 1x, a12,',', 1x, a24,',', 
     1                1x, f12.5,',', 1x, f10.2,',', f10.2,',',i8,
     1                ',',i8)
            else
               write(19,420) rfrnk(k), dcrifr(k), tot, iifrsw(nf),npp
  420          format(    5x,',',     13x,',',     25x,',',
     1                1x, f12.5,',', 1x, f10.2,',', f10.2, ',',i8, 
     1                ',',i8)
            endif
          endif            
  430   continue
  440 continue
c
c _________________________________________________________
c
c
c               i. Print Well Station Summary
         if(iwell.eq.0) goto 448
         icount=0
         
         write(19,132) iystr, iyend
c 132    format('',/,  
  132    format(    /,  
     1   'Table 5.5.1a',/
     1   'Well Structure Summary',/
     1   'Average ',i4, '-', i4,//
     1   '    #,',
     1   ' Type,',
     1   ' ID / Div ID ,',
     1   ' Name / Div ID           ,',
     1   ' Location                ,',
     1   ' Cap-cfs,',
     1   ' AreaTot,',
     1   ' AreaSpr,',
     1   ' EffAve%,',
     1   ' EffFld%,',
     1   ' EffSpr%,',
     1   '  Dem-af, DemType,  On/Off',/,
     1   ' ____,',
     1   ' ____,',
     1   ' ____________,',
     1   ' ________________________,',
     1   ' ________________________,',
     1    8(' _______,'), ' _______')    
c
         npp=0
c
c rrb 00/04/03; Correction
c        do nd=1,numdiv
         do nd=1,numdivw
c
c               Print header
           npp=npp+1
           if(npp.gt.lpp) then
             npp=1
             write(19,132) iystr, iyend
           endif
c
c               Calculate average efficiency
           divefcA = 0.0    
           ce = 0.0
           do im=1,12
             if(diveffw(im,nd).gt.0.0) then
               divefcA = divefcA+diveffw(im,nd)
               ce = ce+1.0
             endif
           end do

           if(ce.gt.0) divefcA = divefcA/ce
c
c
c rrb 00/06/30; Set demand type
           do i=1,5
             if(idvcomw(nd).eq.i) cdemx=cdem(i)
           end do

           iss =idvstaw(nd)

c
c               Well right tied to a diversion
           nw2=idivcow2(nd)

           if(nw2.gt.0) then
             cix2 = cdivid(nw2)
             cname2=divnam1(nw2)
           else
             icount=icount+1  
c            write(nlog,*) ' Outtbl; Well Only ', icount, cdividw(nd)
           end if
           
c             
c rrb 00/12/20; Print maximum efficiency
           write(19,152) nd, nw2, cdividw(nd),divnamw1(nd),
     1                   stanam1(iss),divcapw(nd), areaw(nd),
     1                   areasp(nd), divefcA,effmaxw(nd), effmaxs(nd),
     1                   dummy(iss,5)/rn,cdemx,idivsww(nd), icount, 
     1                   idvcomw(nd)
c
c rrb 2006/05/18; Do not print assocaited diversion structure
cr         if(nw2.gt.0)  write(19,153)  cix2, cname2
  152      format(1x, i4,',',i4,',',
     1            1x, a12,',', 1x, a24,',', 1x, a24,',',
     1            7(f8.0,','),a8, ',', 3i8)
  153      format(1x, 4x,',',
     1            1x, a12,',', 1x, a24,',', 1x, 24x,',',
     1            8(8x,','), 8x)
         end do
c
c _________________________________________________________
c
c
c               j. Print Well return flow summary
         write(19,172)
c 172    format('',/,
  172    format(    /,
     1   'Table 5.5.1b',/
     1   'Well Return Flow Summary',//,
     1   '     , Structure:  ,                         ,',
     1         ' Returns To: ,                         ,',
     1   '        ,        ,        ,        ,         ',/
     1   '    #, ID          , Name                    ,',
     1         ' ID          , Name                    ,',
     1   '       %,   Table, Sum Tbl,   Net %,   Cum %,',/
     1   ' ____, ____________, ________________________,',
     1         ' ____________, ________________________,',
     1   ' _______, _______, _______, _______, ________')
            
      npp=0
c
c rrb 2006/12/20; Correction       
c     do nd=1,numdiv
      do nd=1,numdivw
        if(idivsww(nd).gt.0) then
c
          nui=nrtnw(nd)
          nue=nrtnw(nd+1)-1
          if(nui.le.nue) then
          ct=0.0
c
            do n=nui,nue
c
c rrb 00/04/24; Correction
c             is = irnsta(n)
              is = irnstaw(n)
c
c rrb 99/10/16; Corection
c             irid = irtnid(irtndlw(n))
              irid = irtndlw(n)
              npp=npp+1
              ct=ct+pcttotw(n)*dlytot(irid)/100.0

              if(n.eq.nui) then
c
c               Print header
                if(npp.gt.lpp) then
                  npp=1
                  write(19,172)
                endif
                write(19,200) nd, cdividw(nd), divnamw1(nd),
     1            cstaid(is), stanam1(is),pcttotw(n),
     1            irtnid(irid),
c
c rrb 99/10/05; Allow sum of table to be < 100.
     1            dlytot(irid), pcttotw(n)*dlytot(irid)/100.0, ct
              else
                write(19,210)
     1            cstaid(is), stanam1(is),pcttotw(n), 
     1            irtnid(irid),
c
c rrb 99/10/05; Allow sum of table to be < 100.
     1            dlytot(irid), pcttotw(n)*dlytot(irid)/100.0, ct
              endif
            end do
c
c               Print Well Loss
            if(pctlosw(nd).gt.small) then
              ct=ct+pctlosw(nd)
              write(19,212) 'Loss        ', pctlosw(nd), ct
            endif
c           write(19,*) ' '
          endif
        endif
      end do
c
c _________________________________________________________
c
c
c               k. Print Well Depletion Summary
         write(19,173)
c 173    format('',/,
  173    format(    /,
     1   'Table 5.5.1c',/
     1   'Well Depletion Summary',//,
     1   '     , Structure:  ,                         ,',
     1         ' Depletes:   ,                         ,',
     1   '        ,        ,',/
     1   '    #, ID          , Name                    ,',
     1         ' ID          , Name                    ,',
     1   '       %,   Table, Sum Tbl,   Net %',/
     1   ' ____, ____________, ________________________,',
     1         ' ____________, ________________________,',
     1   ' _______, _______, _______, _______')
            
      npp=0
      ct=0.0

      do nd=1,numdivw
        if(idivsww(nd).gt.0) then
c
          nui=nrtnw2(nd)
          nue=nrtnw2(nd+1)-1

          if(nui.le.nue) then

            ct=0.0
            do n=nui,nue
c
c rrb 00/04/24; Correction
c             is =irnsta(n)
              is =irnstaw2(n)
c
c rrb 99/10/16; Correction
c             irid = irtnid(irtndlw2(n))
              irid = irtndlw2(n)
              npp=npp+1
              ct=ct+pcttotw2(n)*dlytot(irid)/100.0

c             write(io99,*) '  Outtbl; nd,is,n,irid',nd,is,n,irid
              if(n.eq.nui) then
c
c               Print header
                if(npp.gt.lpp) then
                  npp=1
                  write(19,173)
                endif
c
c rrb 99/10/05; Allow sum of table to be < 100.
                write(19,200) nd, cdividw(nd), divnamw1(nd),
     1            cstaid(is), stanam1(is),pcttotw2(n),
     1            irtnid(irid),
     1            dlytot(irid), pcttotw2(n)*dlytot(irid)/100.0, ct
              else
                write(19,210)
     1            cstaid(is), stanam1(is),pcttotw2(n),
     1            irtnid(irid),
c
c rrb 99/10/05; Allow sum of table to be < 100.
     1            dlytot(irid), pcttotw2(n)*dlytot(irid)/100.0, ct
              endif
            end do
c
c               Print Well Salvage
            if(pctlosw2(nd).gt.small) then
              ct=ct+pctlosw2(nd)
              write(19,212) 'Salvage     ', pctlosw2(nd), ct
            endif
c           write(19,*) ' '
          endif
        endif
      end do
c
c
c _________________________________________________________
c

c               l. Print Well Right Summary
         write(19,222)
c 222    format('',/,
  222    format(/,
     1   'Table 5.5.5a',/
     1   'Well Water Right Summary (cfs)',//
     1   '    #, ID          ,', 
     1   ' Name                    ,',
     1   ' Admin #     ,     Decree, Cum Decree,  On/Off, Right #',/
     1   ' ____, ____________,',
     1   ' ________________________,',
     1   ' ____________, __________, __________, _______, _______')
 
      npp=0
      do nd=1,numdivw
        nx = 0
        tot=0.0
        do k=1,numdvrw
          nd1 = idivcow(1,k)
          if(nd.eq.nd1) then
            npp=npp+1
            tot=tot+dcrdivw(k)
            if(nx.eq.0) then
c
c               Print header
              if(npp.gt.lpp) then
                npp=1
                write(19,222)
              endif
              nx = 1
              write(19,230)  nd, cdividw(nd),divnamw1(nd),
     1                        rdvnkw(k), dcrdivw(k), tot, idvrsww(k),npp
            else
               write(19,240) rdvnkw(k), dcrdivw(k), tot, idvrsww(k),npp
            endif
          endif
        end do
      end do
c
c _________________________________________________________
c
c
c               m. Print base flow data
  448    write(19,450) iystr, iyend
c 450    format(''/,
  450    format(   /,
     1   'Table N/A',/
     1   'Base Flow Summary',/
     1   'Average (af/yr) ,',i4, ',-,', i4,//
     1   '    #, ID          , Name                    ,',
     1   '       Gain, Total Flow',/,
     1   ' ____, ____________, ________________________,',
     1   ' __________,','  __________')

         npp=0
         do 470 np=1,numrun
c
c               Print header
           npp=npp+1
           if(npp.gt.lpp) then
             npp=1
             write(19,450) iystr, iyend
           endif

           iss =irusta(np)
           write(19,460) np, crunid(np),runnam1(np),
c    1                 totv(iss)/rn, dum2(13,iss)/rn
     1                 dummy(iss,3)/rn, dum2(13,iss)/rn
  460      format(1x, i4,',', 1x, a12,',', 1x, a24,',', 
     1            2(1x, f10.0,','))
  470    continue

c
c _________________________________________________________
c
c
c               n. Print base flow data by month
c 480    format(''/,
  480    format(   /,
     1   'Table N/A',/
     1   'Stream Flow Comparison',/
     1   'Average (af/yr) ',i4, '-', i4,//
     1   '    #, ID          , Name                    ,  Mon,',
     1   '   BaseFlow,   Historic Adjustment',/,
     1   ' ____, ____________, ________________________, ____,',
     1   ' __________, __________, __________')
                         
         npp=0
         do 510 nh=1,numhis
c          write(19,410) iystr, iyend
c          iss=ihisid(nh) 
           iss=ifix(dummy(nh,4)) 
c
c               Print header
           npp=npp+1
           if(npp.gt.lpp) then
              npp=1
              write(19,170)
           endif
           do 500 im=1,13
             c = dum2(im,iss) - dum3(im,iss)
c             write(19,412) nh, cstaid(iss),stanam1(iss),
c     1                   xmonam(im),
c     1                   dum2(im,iss)/rn, dum3(im,iss)/rn, c/rn
  490        format(1x, i4,',', 1x, a12,',', 1x, a4,',', 
     1              1x, a24,',', 2(1x, f10.0,','), 1x, f10.0)
  500      continue
  510    continue
c


c
c _________________________________________________________
c
c               o. Print plan data
         write(19,482) (i, plntypX(i), i=1,12)
  482    format(   /,
     1   'Table N/A',/
     1   'Plan Data Summary where:',/    
     1   12(' Type ', i5 ' = ', a25,/),/
     1   '    #, ID          , Name                    ,',
     1   ' Type, Source',/
     1   ' ____, ____________, ________________________,',
     1   ' ____, ____________')
                         
         npp=0
         do np=1,nplan
           write(19,484) np, pid(np), pname1(np), iplnTyp(np), 
     1       psource(np)
  484        format(1x, i4,',', 1x, a12,',', 1x, a24,',', 
     1              i5, ',',1x, a12)
         end do
c

c _________________________________________________________
c
c
c               o. Print operational right information
        name1 = blank
        n = 0
        it = 0

        do 560 k=1,numopr       
        
          namez = corid(k)
          name2 = blank
c
c               See if a new or continuing operating rule
          do 520 i=1,12
            if(namez(i:i).eq.'.') goto 530
            name2(i:i) = namez(i:i)       
  520     continue

  530     if(name2.ne.name1) then
            name1 = name2
            n = 0
            it=it+1
c
c               Print reservoir owner data if appropriate
            if(ityopr(k).gt.10 .or. ioprsw(k).eq.0) then
              write(19,600)
              recres='a'
            else   
c
c rrb 2009/03/31; Limit reservoir data to report type 0 (original)	            
              if(irpt.eq.0) then    
                recres='c'
                nr=iopsou(1,k)
                ji=nowner(nr)
                je=nowner(nr+1) -1
C                  
                write(19,570) it, name1
                ctot = 0.0
                j1 = 0
                do 540 j=ji,je
                  j1=j1+1
                  write(19,580) 
     1              resnam1(nr),j1, ownnam(j), ownmax(j)
                    ctot =ctot+ownmax(j) 
  540           continue
                if(je.gt.ji) write(19,590) ctot
c           
c                   Print Reservoir Rights
c               it=it+1
                write(19,310) it, name1
                nx = 0
                do 550 kr=1,numrsr
                  nr1 = iresco(1,kr)
                  if(nr.eq.nr1) then
                    if(nx.eq.0) then
                      nx = nx+1
                      write(19,320) nx, cresid(nr),
     1                  resnam1(nr),
     1                  rrsnk(kr), dcrres(kr), n2fill(kr), 
     1                  irsrsw(kr)
                    else       
                      nx = nx+1
                      write(19,340) nx, rrsnk(kr), dcrres(kr), 
     1                  n2fill(kr), irsrsw(kr)
                    endif
                  endif            
  550           continue
              endif

            endif
c
c               Finally print operating rule info
c           it=it+1
            if(irpt.eq.0) write(19,610) it, recres, name1
            if(irpt.eq.1) write(19,611) it, recres, name1
            if(irpt.eq.2) write(19,612) it, recres, name1
          endif  
c
          n=n+1      
          itx = ityopr(k)
          write(nlog,*) n, nameo(k), ityopr(k)
          
c
c ---------------------------------------------------------
c               Destination is a instream flow
          if(itx.eq.1 .or. itx.eq.15 .or. itx.eq.17) then 
            nf=iopdes(1,k)
c
c rrb 2008/05/19; New format for report            
            if(irpt.eq.0) then
              write(19,630) n, nameo(k), ropnk(k), ioprsw(k), 
     1          cifrid(nf), xfrnam1(nf), 
     1          itx, oprtyp(itx)
            endif 
            
            if(irpt.eq.1) then
              write(19,631) n, nameo(k), ropnk(k), ioprsw(k), 
     1          cifrid(nf), ciopsoX(1,k), 
     1          itx, oprtyp(itx)
            endif 
            
            if(irpt.eq.2) then
              write(19,632) n, nameo(k), ropnk(k), 
     1          cifrid(nf), xfrnam1(nf), itx
            endif
            goto 560
          endif
c
c ---------------------------------------------------------
c               Destination is a diversion or reservoir
          if(itx.eq.2 .or. itx.eq.3 .or. itx.eq.4 .or. itx.eq.11 .or.
     1       itx.eq.14 .or. itx.eq.45) then
c           write(nlog,*) ' Outtbl;', k, itx, iopdes(1,k)
            if(iopdes(1,k).gt.0) then                       
              nd = iopdes(1,k)
c
c rrb 2008/05/19; New format for report            
              if(irpt.eq.0) then              
                write(19,630) n, nameo(k), ropnk(k), ioprsw(k), 
     1          cdivid(nd), divnam1(nd), 
     1          itx, oprtyp(itx)
              endif 
            
              if(irpt.eq.1) then
              write(19,631) n, nameo(k), ropnk(k), ioprsw(k), 
     1          cdivid(nd), ciopsoX(1,k), 
     1          itx, oprtyp(itx)
              endif 
            
              if(irpt.eq.2) then     
                write(19,632) n, nameo(k), ropnk(k), 
     1          cdivid(nd), divnam1(nd), itx
              endif 
            else
c
c rrb 2008/05/19; New format for report 
              nr=-1*iopdes(1,k)
              if(nr.gt.0) then
                if(irpt.eq.0) then            
                  write(19,630) n, nameo(k), ropnk(k), ioprsw(k), 
     1            cresid(nr), resnam1(nr), 
     1            itx, oprtyp(itx)
                endif 
               
                if(irpt.eq.1) then
                  write(19,631) n, nameo(k), ropnk(k), ioprsw(k), 
     1            cresid(nr), ciopsoX(1,k), 
     1            itx, oprtyp(itx)
                endif 
               
                if(irpt.eq.2) then     
                  write(19,632) n, nameo(k), ropnk(k), 
     1            cresid(nr), resnam1(nr), itx
                endif
              endif
            endif
            goto 560
          endif
c
c ---------------------------------------------------------
c               Destination is a reservoir
          if(itx.eq. 5 .or. itx.eq.6 .or. itx.eq.16) then
            nr=iopdes(1,k)
c
c rrb 2008/05/19; New format for report            
            if(irpt.eq.0) then            
              write(19,630) n, nameo(k), ropnk(k), ioprsw(k), 
     1        cresid(nr), resnam1(nr), 
     1        itx, oprtyp(itx)
              endif 
            
              if(irpt.eq.1) then
              write(19,631) n, nameo(k), ropnk(k), ioprsw(k), 
     1          cresid(nr), ciopsoX(1,k), 
     1          itx, oprtyp(itx)
              endif 
            
              if(irpt.eq.2) then    
              write(19,632) n, nameo(k), ropnk(k), 
     1        cresid(nr), resnam1(nr), itx
            endif
            goto 560
          endif
c
c ---------------------------------------------------------
c               Destination is a diversion or reservoir or plan
c		 based on iopdesr(k)=3
          if(itx.ge.23) then
            if(iopdesr(k).eq.1) then                       
              ni=iopdes(1,k)
c
c rrb 2008/05/19; New format for report            
              if(irpt.eq.0) then              
                write(19,630) n, nameo(k), ropnk(k), ioprsw(k), 
     1          cifrid(ni), xfrnam1(ni), 
     1          itx, oprtyp(itx)
              endif 
            
              if(irpt.eq.1) then
              write(19,631) n, nameo(k), ropnk(k), ioprsw(k), 
     1          cifrid(ni), ciopsoX(1,k), 
     1          itx, oprtyp(itx)
              endif 
            
              if(irpt.eq.2) then    
                write(19,632) n, nameo(k), ropnk(k),
     1          cifrid(ni), xfrnam1(ni), itx
              endif
            endif
            
            if(iopdesr(k).eq.2) then                       
              nr=iopdes(1,k)
c
c rrb 2008/05/19; New format for report            
              if(irpt.eq.0) then              
                write(19,630) n, nameo(k), ropnk(k), ioprsw(k), 
     1          cresid(nr), resnam1(nr), 
     1          itx, oprtyp(itx)
              endif 
            
              if(irpt.eq.1) then
              write(19,631) n, nameo(k), ropnk(k), ioprsw(k), 
     1          cresid(nr), ciopsoX(1,k), 
     1          itx, oprtyp(itx)
              endif 
            
              if(irpt.eq.2) then    
                write(19,632) n, nameo(k), ropnk(k), 
     1          cresid(nr), resnam1(nr), itx
              endif
            endif

            if(iopdesr(k).eq.3) then                       
              nd = iopdes(1,k)
c
c rrb 2008/05/19; New format for report            
              if(irpt.eq.0) then              
                write(19,630) n, nameo(k), ropnk(k), ioprsw(k), 
     1          cdivid(nd), divnam1(nd), 
     1          itx, oprtyp(itx)
              endif 
            
              if(irpt.eq.1) then
              write(19,631) n, nameo(k), ropnk(k), ioprsw(k), 
     1          cdivid(nd), ciopsoX(1,k), 
     1          itx, oprtyp(itx)
              endif 
            
              if(irpt.eq.2) then    
                write(19,632) n, nameo(k), ropnk(k), 
     1          cdivid(nd), divnam1(nd), itx
              endif
            endif
            
            if(iopdesr(k).eq.6) then                       
              nw=iopdes(1,k)
c
c rrb 2008/05/19; New format for report            
              if(irpt.eq.0) then              
                write(19,630) n, nameo(k), ropnk(k), ioprsw(k), 
     1          cdividW(nw), divnamW1(nw), 
     1          itx, oprtyp(itx)
              endif 
            
              if(irpt.eq.1) then
                write(19,631) n, nameo(k), ropnk(k), ioprsw(k), 
     1          cdividw(nw), ciopsoX(1,k), 
     1          itx, oprtyp(itx)
              endif 
            
              if(irpt.eq.2) then    
                write(19,632) n, nameo(k), ropnk(k),
     1          cdividW(nw), divnamW1(nw), itx
              endif
            endif
            
            if(iopdesr(k).eq.7) then 
c
c rrb 2009/05/26; Correction            
              np=iopdes(1,k)
              np=abs(iopdes(1,k))
c
c rrb 2008/05/19; Revise to allow different methods of assigning ID                                  
              if(np.gt.10000) np=np-10000
c
c rrb 2008/05/19; New format for report            
              if(irpt.eq.0) then              
                write(19,630) n, nameo(k), ropnk(k), ioprsw(k), 
     1          pid(np), pname1(np), 
     1          itx, oprtyp(itx)
              endif 
            
              if(irpt.eq.1) then
              write(19,631) n, nameo(k), ropnk(k), ioprsw(k), 
     1          pid(np), ciopsoX(1,k), 
     1          itx, oprtyp(itx)
              endif 
            
              if(irpt.eq.2) then    
                write(19,632) n, nameo(k), ropnk(k),
     1          pid(np), pname1(np), itx
              endif
            endif
            goto 560
          endif
c
c ---------------------------------------------------------
c               Destination is other (e.g. 9)         
          if(irpt.eq.0) then
            write(19,630) n, nameo(k), ropnk(k), ioprsw(k),             
     1      ciopdeX(1,k), 'NA                      ',      
     1      itx, oprtyp(itx)
          endif 
            
          if(irpt.eq.1) then
            write(19,631) n, nameo(k), ropnk(k), ioprsw(k), 
     1        ciopdeX(1,k), ciopsoX(1,k), 
     1        itx, oprtyp(itx)
          endif 
            
          if(irpt.eq.2) then    
            write(19,632) n, nameo(k), ropnk(k), 
     1      ciopdeX(1,k), 'NA              ', itx
          endif
          goto 560
         
  560   continue
c
c _________________________________________________________
c
c       Formats

c 570     format('',/,
  570     format(/,
     1   'Table 5.5.', i2,'a',/
     1   'Operating Rules for ID = ', a12,/
     1   'Reservoir Account Summary',//
     1   'Reservoir               ,  Acct,',
     1   ' Owner       , Cap (af)',/
     1   '________________________, _____,',
     1   ' ____________, ________')
  580     format(a24,',', 1x, i5,',', 1x, a12,',', 1x, f8.0)
  590     format(
     1   '________________________, _____,',
     1   ' ____________, ________'/,
     1   'Total (1)               ,      ,', 
     1   '             ,', 1x, f8.0, /
     1   '(1) May exceed reservoir capacity if accounts share the ',
     1        'same space')

c 600   format('')
  600   format(1x)
  610   format(/, 
     1   'Table 5.5.', i2,a1,/
     1   'Operating Rules for ID = ', a12,//
     1   '     ,                         ,        Admin,  1=On,',  
     1   ' Destination ,  Destination            ,      ,',31x/
     1   '    #, System                  ,       Number, 0=Off,',  
     1   ' ID          ,  Name                   ,  Type,',
     1   ' Description',/
     1   '_____, ________________________, ____________, _____,',  
     1   ' ____________, ________________________, _____,',
     1   31('_'))
  
  611   format(/, 
     1   'Table 5.5.', i2,a1,/
     1   'Operating Rules for ID = ', a12,//
     1   '     ,                         ,        Admin,  1=On,',  
     1   ' Destination , Source      ,      ,',31x/
     1   '    #, System                  ,       Number, 0=Off,',  
     1   ' ID          , ID          ,  Type,',
     1   ' Description',/
     1   '_____, ________________________, ____________, _____,',  
     1   ' ____________, ____________, _____,',
     1   31('_'))
     
  612   format(/, 
     1   'Table 5.10.', i2,a1,/
     1   'Operating Rules for ID = ', a12,//
     1   '     ,                         ,        Admin,',
     1   ' Destination , Destination ,  '/
     1   '    #, System                  ,       Number,',
     1   ' ID          , Name            ,   Type',/
     1   '_____, _______________________, _____________,',
     1   '____________ , ________________, ______')
     
     
  630   format(i5,',', 1x,a24,',', 1x,f12.5,',', 1x,i5,',', 
     1         1x,a12,',', 1x,a24,',',1x,i5,',', 1x,a30)
     
  631   format(i5,',', 1x,a24,',', 1x,f12.5,',', 1x,i5,',', 
     1         1x,a12,',', 1x,a12,',',1x,i5,',', 1x,a30)
     
  632   format(i5,',', 1x,a24,',', 1x,f12.5,',',  
     1         1x,a12,',', 1x, a16,',', 1x,i5)
c
c _________________________________________________________
c
             
       return
      END       





