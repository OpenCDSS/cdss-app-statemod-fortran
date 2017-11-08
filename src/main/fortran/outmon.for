c *********************************************************
C
      SUBROUTINE OUTMON(numstax)
c      
c
c _________________________________________________________
c	Program Description
c
c       Outmon; It prints binary output
c               Note this routine prints well information 
c                 for D&W structures only, not well only structures.
c
c _________________________________________________________
c       Documentation
c
c               file 42 *.xwe   wells 
c               file 43 *.xdd   diversions & stream 
c               file 44 *.xre   reservoirs
c               file 45 *.xop   operating rules 
c               file 47 *.xir   instream flow
c               file 67 *.xss   structure summary
c
c               ofl(is)         river outflow to diversion output
c               oflx(is)        river outflow to reservoir output
c               oflz(is)        adj river outflow for min downstream 
c                               calcuations and carrier calculations
c               avt             min flow downstream (limits diversion)
c               AvFlow          available flow
c               gw2riv(is)      Water from ground water storage to River
c               gw2gw(is)       Water from river to ground water storage
c               dep(is)         depletion caused by future depletions
c               shortx(is)      constrained (total) shortage
c               rid             counter used to identify structure type
c                               used by outdiv, outdivw, outdivc,
c                                       daydivo, dayoutr and the post 
c                                       processor delplt
c               qdiv(1   Total demand
c               qdiv(2   In-basin demand (w/o transmountain)
c               qdiv(3   Not used but calculated to be total demand
c               qdiv(4   Not used but refrenced
c               qdiv(5   Total diversion from river by priority 
c                        (e.g. divcar)
c               qdiv(6   Not used but refrenced
c               qdiv(7   From River by Storage
c               qdiv(8   Total diversion from transmtn (e.g. divcar)
c               qdiv(9   Refrenced in outmon but not set!!!
c               qdiv(10  From river by Storage Transmountain (e.g. 
c                         divers, carrpl)
c               qdiv(11  Remaining demand (shortage)
c               qdiv(12  ISF demand set in outmon
c               qdiv(13  Set in outmon but not used
c               qdiv(14  Div by an instream flow (e.g. ifrrig)
c               qdiv(15  Reservoir to power (e.g. powres) or
c                        Reservoir to a T&C plan (Type 49 powresP)
c                        or Res to Res by a carrier
c                        Note shows as a diversion in *.xdd (From River 
c                        by Storage) but is not a diversion in *.xwb
c                        Non consumptivec               
c               qdiv(16  Used to remove carrier water from 
c			                   total diversion (qdiv(5))
c               qdiv(17  Not used
c               qdiv(18  Carried, Exchange or Bypass (column 11)
c                        Carrier passing thru a structure.  Note
c                        qdiv(18 is not added to TotSup1 that is used
c                        to calculate River Divert
c               qdiv(19  From Carrier by Priority (e.g. divcar)
c               qdiv(20  From Carrier by Storage or Exchange.  Note:
c                        qdiv(20 gets added to TotSup1 that is used
c                        to calculate River Divert
c               qdiv(21  From River by Exchange from a reservoir (e.g.
c		                     carrpl)
c               qdiv(22  From Storage to Carrier for Use 
c               qdiv(23  From River by Exchange via an interruptable 
c                        supply
c               qdiv(24  Pumping (Well) at river ID 
c               qdiv(25  Depletion at river ID
c               qdiv(26  From River by a Direct Flow, Exchange, or 
c                        Bypass (see DirectEx and DirectBY)
c                        or Carrier with loss (DirectL) or an alternate 
c                        point (DivAlt) or by a changed water right 
c                        (Directwr)
c               qdiv(27  Other source via a Direct Flow Exchange
c                        or Bypass (see DirectEx and DirectBY)
c		            qdiv(28  Carried, Exchange or Bypass (column 11)
c                        Released from a reuse plan or Admin plan
c		            qdiv(29  From river by exchange from a plan
c		            qdiv(30  From River from a Res or Reuse Plan 
c                        to a T&C or Aug Plan. Note non consumptive
c                        Note shows as a diversion in *.xdd (From River 
c                        by Storage or Other) but is not a diversion in *.xwb
c                        Non consumptive          
c                        See type 49 Divrplp2
c               qdiv(31  From River by Plan by DivresP2 or DivrplP
c		            qdiv(32  From Carrier loss from a carrier (DivCar, DivcarL) 
c		            qdiv(33  From River loss from a diversion by the river
c                        (DivCar, DivcarL, DirectBy, DirectEx) 
c		            qdiv(34  From River by an Out of Priority Diversion
c              
c 		          qdiv(35  Water with a Reuse or Admin plan source 
c		            	         tracked at the destination from the following:
c                          Operating rules: 26-DirectWR,
c			                     27-DivResP2,     28-DivrplP, 29-PowSeaP,
c			                     46-divMultip,    48-PowResP, 49-DivRplP2
c              
c		            qdiv(36  Water released to the river (report as
c			                     return flow). Set in SetQdivC, DivResp2 &
c                          PowseaP      
c               qdiv(37  Not currently used.  Was used to represent
c                          Water released to the river by a spill in 
c                          PowseaP (type 29) Report as a negative  
c                          diversion herein (OutMon) under Rivdiv   
c               qdiv(38  Carried water reported as Carried, Exchange 
c                          or Bypassed but not used to calculate
c                          River Divert in Outmon.f   
c  
c		            ClossDC From Carrier Loss
c		            ClossDR From River Loss
c
c
c ______________________________________________
c             
c	            	qres(1  From river by Priority (ResRg1 or resRg1P)
c	            	qres(2  From carrier to Storage by Priority
c	              qres(3
c               qres(4  From carrier to storage by Other
c               qres(8  From storage to river for use
c               qres(9  From storage to carrier for non Tmtn.
c               qres(11 From storage to carrier
c               qres(12 From storage to River for Use (Powres*)
c               qres(18 From river by Exchange
c               qres(21 From storage for Exchange
c		            qres(22 From storage to carrier
c		            qres(25 Reservoir Seepage by Carrier Loss
c               qres(26 From river by Storage to Reservoir
c               qres(27 Carrier Loss (DivCar)
c		            qres(28 From River by an Out of Priority Diversion
c		            qres(29 Amount Booked over in the same reservoir (Rsrspu)
c		               used in outbal2 to adjust for a 
c			             diversion already included in reservoir storage
c               qres(30 From River Loss
c
c
c		            RclossC Loss to a reservoir by a carrier
c		            RclossR Loss to a reservoir by the river
c
c _________________________________________________________
c       Update History
c
c rrb 99/06/12; Adjusted for qdiv(23 exchange by interruptable supply
c rrb 99/12/24; New Id convention as follows:
c                                  1  -  5000 = diversion
c                               5001  -  7500 = ISF
c                               7501  - 10000 = reservoir
c                                  0  -     0 = well only 
c                               < -10000 = baseflow only
c                               -1* above = baseflow plus a structure 
c rrb 96/05/29; change to generalize dimensions
c rrb 00/07/03; For d&W structures added adjustment to 
c               demand (demcon) based on supply to meet IWR demand
c rrb 01/01/24; Calculate and add IWR demand (diwrz), and 
c               IWR short (shortiw).  Also add loss (rloss) to output.
c
c rrb 01/12/07; Station balance total supply (column 19)
c               should not include water diverted
c               from carrier by priority (qres(2,nr)
c               from carrier by storage (qres(4,nr)
c rrb 2007/02/23; Revised *.xdd to include well only in the balance
c		             Revised *.xdd output column 33 = rlossw2 (salvage)
c
c
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
c                           
      dimension cstr(20)
      character cstr*12, ccallID*12, ctype1*12
C
      DIMENSION
     1  VIR(numstax),     RET(numstax),     GAI(numstax),
     1  OFL(numstax),     AVT(numstax),     TEMP3(numstax)
     
      dimension
     1  shortx(numstax),  oflx(numstax),    oflz(numstax),
     1  dep(numstax),     rlossx(numstax),  totothw(numstax),
     1  demx(numstax),    diwrz(numstax),   shortiw(numstax), 
     1  qtosoil(numstax), qfrsoil(numstax), rlossx2(numstax)
c
c _________________________________________________________
c               Step 1; Initilize
c
c	     iout=1 details on reservoir output
c	     iout=2 summary details on reservoir output
c	     iout=3 details on *.b43
c      iout=4 print warning for multiple structures at a node
c	     iout=5 print details about variable carry
c      iout=6 print data to *.b43 to *.log
c      ioutQ  print details of River Divert and qdiv calcs
c	     ioutRtn = 1 Print return flow details by node & month
c		             2 Print return flow details by month 
c
      iout=0
      ioutRtn=0
      ioutQ=0
   
      
      if(iout.eq.2) then
          write(nlog,122) mon
 122      format(/, ' Outmon; mon = ',i5)
      endif  
        
      
      maxstr=20     
      small = 0.001
      smalln=-1.0*small
      irecmax=0
c
c ---------------------------------------------------------       
c
c rrb 01/03/30; Units
c               fx (fmo) = units for output (cfs, af, kaf, or cms)
c               fy (faf) = unit conversion af to cfs for a day
c               fz (faf) = unit conversion cfs to af/mo 
c               Monthly model fx=fy.  Daily model fx .ne. fy
      fac=MTHDAY(MON)*factor
      fx=fmo(mon)
      fz=faf(mon)
c
c
c ---------------------------------------------------------       
c rrb 99/12/23; Check ID labeling used by delplt               
      iok=0
      if(numdiv.gt.5000) iok=1
      if(numifr.gt.2500) iok=1
      if(numres.gt.2500) iok=1
      if(iok.eq.1) then
        write(io99,*) numdiv, 5000, numifr, 2500, numres, 2500
        goto 9999
      endif
c
c ---------------------------------------------------------       

      DO IS=1,NUMSTA
        VIR(IS)=0.
        GAI(IS)=0.
        OFL(IS)=0.
        avt(is)=0.
        temp3(is)=0.
        
        RET(IS)=0.
        dep(is)=0.
        rlossx(is)=0.0
c
c rrb 2007/02/23; Add salvage to *.xdd output        
        rlossx2(is)=0.0
c
c rrb 00/07/11; IWR considerations
        shortx(is)=0.0
        oflx(is)=0.
        oflz(is)=0.
c
c rrb 01/01/24; Soil, etc
        demx(is)    =0.0
        diwrz(is)   =0.0
        shortiw(is) =0.0
        qtosoil(is) =0.0
        qfrsoil(is) =0.0
      end do
C
cx              write(nlog,*) ' Outmon; qdiv29, qdiv30', 
cx     1          qdiv(26,179)*fac,qdiv(29,179)*fac, qdiv(30,179)*fac   
c
c _________________________________________________________
c               Step 2; Diversion and Stream output (*.xdd)

      DO IS=1,NUMSTA
        QDIV(4,IS)=QDIV(5,IS)+QDIV(6,IS)+QDIV(8,IS)+QDIV(9,IS)+
     1             QDIV(16,IS)
      end do
c
c _________________________________________________________
c               Step 3; Diversion Calculations
C
      DO 150 ND=1,NUMDIV
        totothw(nd)=0.0
        IS =IDVSTA(ND)

c
c rrb 01/02/04; Include loss by wells
        nw=idivco2(nd)    
        if(nw.eq.0) then
          rlossx(is)=rloss(nd)  
        else
          rlossx(is)=rloss(nd)+rlossw(nw)
c
c rrb 2007/02/23; Add salvage to *.xdd          
          rlossx2(is)=rlossw2(nw)
        endif
C
        IF(IDRGST(ND).NE.0.AND.IRSORD(2,IS).NE.0) GO TO 150
C
        IF(IDIVSW(ND).EQ.0) GO TO 150
C

        IUI=NDUSER(ND)
        IUE=NDUSER(ND+1)-1
C
        IF(IUI.GT.IUE) GO TO 150
c
c rrb 00/06/16; Adjust to include total demand (divert()) from mdainp

        DO 140 IU=IUI,IUE
c
c rrb 01/01/24; Soil Moisture, etc.
          if(iu.ne.nd) then
            write(io99,510) iu, nd
            goto 9999
          endif

          demx(is)  = divert(mon,iu)
          diwrz(is) = diverirt(mon,iu)
          if(nw.eq.0) then
            qtosoil(is) = qdivs(iu)
            qfrsoil(is) = qdivso(iu)
          else
            qtosoil(is) = qdivs(iu) + qdivsw(nw)
            qfrsoil(is) = qdivso(iu) + qdivswo(nw)
          endif 
c
c
          shortiw(is) = amax1(0.0, shortiw(is) + diverirt(mon,iu) 
     1                - dcut(iu)) 

          IF(DIVERt(MON,IU).LE.0.00001) GO TO 140
          QDIV(1,IS)=QDIV(1,IS)+DIVERt(MON,IU)
          IF(IRTURN(IU).EQ.4) GO TO 120
          QDIV(2,IS)=QDIV(2,IS)+DIVERt(MON,IU)
          GO TO 130
  120     QDIV(3,IS)=QDIV(3,IS)+DIVERt(MON,IU)
  130     QDIV(11,IS)=QDIV(11,IS)+DIVREQ(IU)
  140   CONTINUE

  150 CONTINUE             
C

c
c _________________________________________________________
c               Step 4; Return flow & Depletion calculations
c     write(nlog,*) ' Outmon; iyr, mon, nstrtn', iyr, mon, nstrtn
c
c rrb 10/09/01; Test
cx             write(nlog,*) ' Outmon; Test_Set_1', is, np, nstrtn,
cx      1         iplntyp(np), cstaid(is), imo, qdiv(25, 246)*fac,
cx      1         depl(imo,104)*fac                 
cx 
      DO NR=1,NSTRTN  
  
        IS=ISTRTN(NR)
        ret1=ret(is)
c
        RET(IS)=Ret(is) + RETUR(IMO,NR)
        ret2=ret(is)
c
c rrb 10/09/01; Version 12.3000 CORRECTION       
c       dep(is)=depl(imo,nr)   
        dep(is)=dep(is) + depl(imo,nr)           
c
      end do
c
c
c _________________________________________________________
c               Step 5; Gain calculations

      DO NP=1,NUMRUN
        IS=IRUSTA(NP)
        GAI(IS)=GAI(IS)+VIRINP(MON,NP)
      end do
c
c _________________________________________________________
c        
c		Step 6; ISF calculations
      DO 190 NF=1,NUMIFR
        IF(IFRRSW(NF).EQ.0) GO TO 190
        IS=IFRSTA(NF)
        demx(is)=demx(is) + flowr(mon,nf)

        qdiv(11,is)=qdiv(11,is)+flowrq(nf)
        QDIV(12,IS)=FLOWR(MON,NF)
  190 CONTINUE
c
c _________________________________________________________
c               Step 7; Plan calculations
c		  NOte qdiv(11 is remaing demand
c		       qdiv(12 is total demand
c
c     write(nlog,*) ' Outmon; nplan',  nplan
c     write(6,*) ' Outmon; nplan',  nplan
      DO  np=1,nplan
c
c rrb 04/12/28; Reuse Plan capability      
c		Note iplntyp=1 for T&C, 2 for well augmentation, 3 CU reuse
c               4 for Tmtn reuse
        IF(pon(np).gt.0) then
          is=ipsta(np)
c smalers 2017-11-07 Add checks to avoid array index out of bounds
         
	  if(is.gt.0) then
            IF(iplntyp(np).eq.1 .or. iplntyp(np).eq.2) then
              demx(is)=demx(is) + pdemT(np)        
              qdiv(11,is)=qdiv(11,is)+Pdem(np)
              QDIV(12,IS)=pdemT(np)
            endif
c
c rrb 2006/04/18; do not adjust for reservoir recharge since it
c		  is included in return flow          
            
            if(iplntyp(np).ge.3 .and. iplntyp(np).ne.8) then
c             write(nlog,*) ' Outmon; Plan data for node (is) = ', is
              qdiv(11,is)=qdiv(11,is)+Psuply(np)
cr            demx(is)=demx(is) + PsuplyT(np)        
              QDIV(12,IS)=psuplyT(np)
            endif
          endif  
        endif
      end do    
c
c
c _________________________________________________________
c               Step 8; Well Only Calculations 
c               Note this routine calculates data for Well Only lands
        
        if(iwell.ge.1) then  
          do 197 nw=1,numdivw
            nd=idivcow2(nw)
c
c
c rrb 01/01/24; Soil Moisture, etc (See not above regarding 
c               Well only structures
c
c               For Well Only Lands (nd.eq.0)
            if(nd.eq.0) then
              is = idvstaw(nw)
c
c rrb 01/01/24; Soil Moisture, etc 
c               Note included here for emphasis but all terms
c               are treated as a total under diversions

              c = diverw(mon,nw)
c
c rrb 00/12/19; Variable eficiency calculate supply as IWR   
              supplyx = divmonw(nw) + qdivswo(nw)
              shorty  = amax1(diverw(mon,nw)-supplyx, 0.0)
c
c rrb 00/08/02; Express demand (demcon) using average SW efficiency
c               Note Mdainp checks & warns if 
c               demand > 0 and efficiency = 0                 
c               
              if(diveffw(mon,nw).gt.small) then
                demcon(mon,is) = usemonw(nw) + 
     1                    (shorty / (diveffw(mon,nw)/100.))
c
c rrb 00/07/11; Express shortage (shortx) using SW efficiency
                shortx(is) = shorty / (diveffw(mon,nw)/100.)
              else
                demcon(mon,is) = c
                shortx(is) = c - usemonw(nw)
              endif
              diwrz(is) = diverirw(mon,nw)
              demx(is)  = diverw(mon,nw)
              shortiw(is)=diverirw(mon,nw) - dcutw(nw)
              rlossx(is) =rlossx(is) + rlossw(nw)            
              rlossx2(is) =rlossx2(is) + rlossw2(nw)

c             write(io99,191) is, c*fz, demcon(mon,is)*fz, 
c    1           diverirt(mon,nd)*fz, supplyx*fz, shorty*fz
            endif
 197      continue         
        endif
        
c
c _________________________________________________________
c               Step 9; River Station calculations
c
c     write(nlog,*) ' Outmon; numsta',  numsta
c     write(6,*) ' Outmon; numsta',  numsta
      DO 200 IS=1,NUMSTA
c
c
c rrb 2008/02/02; Adjust returns (Ret)for Return to River by a
c         plan types 4 & 6 Reuse from a diversion & tmtn diversion
        RET(IS)=Ret(is) + qdiv(36,is)
c
c rrb 01/05/95; I/O Addition Constrained shortage

        c = demcon(mon,is)+qdiv(11,is)-qdiv(1,is)-qdiv(12,is)

        OFL(IS)=RIVER(IS)              
        oflx(is)=river(is)              
        oflz(is)=river(is)
c
c rrb 98/12/22; Adjust "Well Depletion" to not include current month
c               which is shown under "River by Well" (qdiv(25,is)
c rrb 99/06/12; qdiv(23 is now exchange by interruptable supply
        dep(is)=dep(is)-qdiv(25,is)
c
c
c rrb 01/05/95; I/O Addition  irsord(2,is) is the reservoir number
        IF(IRSORD(2,IS).ne.0) then 
          OFLz(IS)=AVINP(IS)+VIR(IS)+GAI(IS)+RET(IS)-QDIV(5,IS)
     1        -QDIV(6,IS)-QDIV(8,IS)-QDIV(9,IS)-QDIV(7,IS)-QDIV(10,IS)
     1        +gw2riv(is)-gw2gw(is)-dep(is) 

        endif
        
  200 CONTINUE
C
      DO 210 IS=1,NUMSTA
  210   AVT(IS)=AVAIL(IS)
  
  
c
c _________________________________________________________
c               Step 10; Reservoir Calculations
c
      DO 220 NR=1,NUMRES
        IF(IRESSW(NR).EQ.0) GO TO 220
        IS=IRSSTA(NR)
        AVT(IS)=AMIN1(AVT(IS),OFLz(IS))
c
c rrb 2010/10/15; Include loss at the destination        
        rlossX(is)=rlossX(is) + (qres(27,nr) + qres(30,nr))/fac
  220 CONTINUE
c
c _________________________________________________________
c               Step 11; Summary Calculations
c
c rrb 06/06/96; Inlcude header information
c     IREC=(IYR-IYSTR)*12*NUMSTA+(MON-1)*NUMSTA+1    
      IREC=(IYR-IYSTR)*12*NUMSTA+(MON-1)*NUMSTA+ numtop    
C      
      IF(IOPOUT.Gt.0) then
        WRITE(99,230) IYR,MON
  230   FORMAT(' QDIV ',2I10)
      endif
C
c
C
cr    write(nlog,*) ' Outmon; near do 290 qdiv29, qdiv30', 
cr  1   qdiv(26,179)*fac,qdiv(29,179)*fac, qdiv(30,179)*fac   
c
c _________________________________________________________
c               Step 12; Print data by Stream
c
      DO 290 IS=1,NUMSTA
      
        rimcdX=-1.0
        ccallR(is)=-1.0
        ccallID='N/A'
      
        IRECS=IREC+IS
        NDNN =NDNNOD(IS)
        CALL DNMFSO(maxsta, AVT, IDNCOD, IS, NDNN, IM)
c rrb 01/10/95; Additional Output                                
c              Subtract qdiv(16,is) to remove carrier water totaled
c                in qdiv(5,is)       
        RivPri  = qdiv(8,is)  + qdiv(14,is) + qdiv(5,is) - qdiv(16,is) 
c
c rrb 2015/01/16; Test
c ______________________________________________________________
        if(ioutQ.eq.1) then
          write(nlog,*) ' '
          write(nlog,*) 'Outmon;   is',
     1      '    RivPri =  qdiv(8 + qdiv(14  + qdiv(5 - qdiv(16'
         
          write(nlog,'(a8, i5, 20f10.0)')
     1    ' Outmon;', is, RivPri*fac, qdiv(8,is)*fac, qdiv(14,is)*fac, 
     1     qdiv(5,is)*fac, qdiv(16,is)*fac
        endif  
c
c rrb 01/12/07; Use Abs to get correct output from carrpl 
        RivSto  = amax1(0.0, 
     1          qdiv(10,is) + qdiv(15,is) + qdiv(7,is) - qdiv(21,is))
c
c rrb 99/06/25; Interruptable supply addition
c               Note qdiv(21) is exchange via a reservoir
c                    qdiv(23) is exchange via interruptable supply 
c                    qdiv(26) from river by exchange by direct flow Exchnage
c rrb 05/02/08;      qdiv(29  is exchange from a plan
c                    qdiv(30  is direct from a plan or Res to a T&C
c                    qdiv(31  From the river via a reuse or Admin Plan
c		                 qdiv(34  is from river by an OOP diversion

c
c rrb 2006/06/07; Add from OOP (qdiv(34)
        RivExPl = qdiv(21,is) + qdiv(23,is) + qdiv(26,is) + 
     1            qdiv(29,is) + qdiv(30,is) + qdiv(31,is) +qdiv(34,is) 
c
        
        well = qdiv(24,is)
c
c rrb 98/12/22; current impact on river is part of dep(is) (depl())
c rrb 99/06/12; qdiv(23 is now exchange by interruptable supply
c       RivWel= qdiv(23,is)
        RivWel= qdiv(25,is)

        CarPri  = qdiv(16,is) + qdiv(19,is)
        CarStoEx  = qdiv(20,is)
c
c rrb 2005/11/29; River and Carrier Loss        
        CLossDC=qdiv(32,is)
        ClossDR=qdiv(33,is)
c
c rrb 2004/09/21; Set Carried to carried water (qdiv(18 )or 
c               exchanged out of a structure (qdiv(27 )
c rrb 04/12/28; or from a reuse or admin plan (qdiv(28
c       Carried  = qdiv(18,is)
c       Carried  = qdiv(18,is) + qdiv(27,is)  
c
c rrb 2010/10/9; Revised approach to Plan Accounting 
cx        Carried  = qdiv(18,is) + qdiv(27,is) + qdiv(28,is) 
cx        CarriedX = qdiv(18,is)               + qdiv(28,is)
        Carried  = qdiv(18,is) + qdiv(27,is) 
        CarriedX = qdiv(18,is)               
c
cx	if(carried.gt.0.001) then
cx          write(nlog,*) '  Outmon; is, carried', is, carried*fac        
cx          write(nlog,*) '  Outmon; 18,27,28', qdiv(18,is)*fac,
cx     1      qdiv(27,is)*fac, qdiv(28,is)*fac
cx        endif  
         
        TotSup1  = RivPri + RivSto + RivExPl + CarPri + well + CarStoEx
     1           + qfrsoil(is)-CarriedX - ClossDC - ClossDR 
c
c rrb 2015/01/16; Test
c ______________________________________________________________
        if(ioutQ.eq.1) then
          write(nlog,*) ' '
          write(nlog,*) 'Outmon;   is',
     1      '   Totsup1 =  RivPri  + RivSto + RivExPl  + CarPri',
     1      '    + well +CarStoEx  +qfrsoil -CarriedX - ClossDC',
     1      ' - ClossDR'
          
          write(nlog,'(a8, i5, 20f10.0)')
     1    ' Outmon;', is, Totsup1*fac, RivPri*fac,
     1       RivSto*fac, RivExPl*fac, CarPri*fac, well*fac,
     1       CarStoEx*fac, qfrsoil(is)*fac, 
     1       CarriedX*fac, ClossDC*fac, ClossDR*fac        
        endif
c ______________________________________________________________
 
                 
        TotSup1  = amax1(0.0, TotSup1)        
        TotSup(is)=TotSup1-qfrsoil(is)
        
c
c              Find diversion(s) at this station
c              Assumes one per station
        UseCu = 0.0
        retx  = 0.0
        rid   = 0.0                              
        ris   = 0.0
        k1    = 0

c
c rrb 99/12/22; Find a baseflow point (surrogate for a gage)
c               Note if this point is also a diversion, etc that
c               ID takes precedence.
        do n=1,numrun
          if(irusta(n).eq.is) then
            rid=-1*(10000.0+float(n))
            ris=rid
          endif
        end do

c
c _________________________________________________________
c
c              Step 13; Diversion id (if it exists)
c              Assumes one structure per river station
c                      Note diveff(im,nd) = 0 for instream
c                           totothw(nd) is used by well output
        nw=0
        do 250 nd=1,numdiv
          if(idvsta(nd).eq.is) then
            nw=idivco2(nd)
            if(k1.eq.0) then   
              UseCu=dcut(nd) 
              totothw(nd)=TotSup1-qfrsoil(is)
            endif
            rid = nd
c
c rrb 99/12/23; identify if a diversion plus a gage
            if(rid.gt.0 .and. ris.lt.-10000.0) rid=-1.0*rid

            k1 = k1+1
            k1=amin0(maxStr,k1)
            cstr(k1) = cdivid(nd)
c
c __________________________________________________
c rrb 2005/07/22; Get call data for a Diversion or Carrier
            ctype1='Diversion'
            call OutCallS(imcdL(is), nd, ccallR(is), ctype1, ccallID)        
            rimcdX=imcdL(is)            
c           write(nlog,*) '  Outmon; is, rimcdX', is, rimcdX
          endif
  250   continue   
c
c _________________________________________________________
c
c               Calculate Total Return (retx) = Total Supply
c                 (TotSup1) - CU (UseCu) - loss (rlossx(is))
c                 - To Soil (qtosoil) - carried water (qdiv(18,is)
c
c rrb 2005/11/29; Correction; TotSup1 is already adjusted for qdiv(18,is)
c rrb 2006/03/18; The following maintains mass balance in the global
c                 water budget (*.xwb)
c rrb 2007/12/03; Correction. Note budget is OK
c       retx = TotSup1-UseCu-rlossx(is) - qtosoil(is) - qdiv(18,is)
        retx = TotSup1-UseCu-rlossx(is) - qtosoil(is)
cr        if(retx.lt.smalln) then
cr          write(nlog,*) ' Outmon; Warning ret is less than zero' 
cr          write(nlog,*) ' Outmon; im, is, TotSup1, usecr, rloss(is)',
cr     1     ' qtosoil(is), qdiv(18,is)'
cr     
cr          write(nlog,*) im, is,TotSup1,usecr,rloss(is)*fac,
cr     1     qtosoil(is)*fac,qdiv(18,is)*fac
cr        endif  

        retx = amax1(0.0, retx)
c
c rrb; 98/12/14; Wells
        GwSto= gw2riv(is)-gw2gw(is)
        RivIn = avinp(is)+gai(is)+ret(is)+gw2riv(is)-gw2gw(is)
     1                   -dep(is)
     
        CarryY=CarPri + CarStoEx - Carried - ClossDC
        RivDiv = TotSup1 - CarryY - well + ClossDR - qfrsoil(is) 
        RivDiv=amax1(RivDiv, 0.0)
c
c rrb 2015/01/16; Test
c ______________________________________________________________
        if(ioutQ.eq.1) then
          write(nlog,*) 'Outmon;   is',
     1      '    RivDiv = TotSup1  - CarryY    - well + ClossDR ',
     1      ' - qfrsoil(is'
         
          write(nlog,'(a8, i5, 20f10.0)')
     1      ' Outmon;', is, RivDiv*fac, TotSup1*fac, CarryY*fac,
     1       well*fac, ClossDR*fac, qfrsoil(is)*fac
        endif

c ______________________________________________________________
        
c
c rrb 2014/11/24; Adjust the diversion to remove a plan release qdiv(37
c                 qdiv(37  Water released to the river by a spill in 
c                          PowseaP (type 29)
c rrb 2015/01/16; Do not adjust for qdiv(37 since diversions to
c                 admin plans are not used in calculating
c                 River Divert  
cx        RivDiv1=RivDiv
cx        RivDiv=Rivdiv-qdiv(37,is) 
c
c rrb 2015/01/16; Adjust the amount carried to include water diverted
c                 to an Admin Plan by a type 26 rule after
c                 River Divert has been calculated
        Carried=Carried+qdiv(38,is)         
c
c _________________________________________________________
c
c		Step 14; Reservoir ID (if it exists)
c rrb 01/15/95; Re calculate River Divert (RivDiv) if a reservoir
c		            made a release at this location
c               Store resid even for diversion printout
        do 252 nr=1,numres
          if(irssta(nr).eq.is) then
c
c rrb 99/08/13; Adjust diversion to reflect a reservoir release
            RivDiv = RivIn - ofl(is) - RivWel
c
            rid = nr+7500
c
c rrb 99/12/23; identify if a reservoir plus a gage
            if(rid.gt.0 .and. ris.lt.-10000.0) rid=-1.0*rid

            k1 = k1+1
            k1=amin0(maxStr,k1)            
            cstr(k1) = cresid(nr)
c
c __________________________________________________
c rrb 2005/07/22; Get call data for a reservoir
            ctype1='Reservoir'
            call OutCallS(imcdL(is), nr, ccallR(is), ctype1, ccallID)        
            rimcdX=imcdL(is)            
          endif          
  252   continue

c
c              (im is the node that limits the diversion)
        AvFlow = avt(IM)
c
c _________________________________________________________
c
c              Step 15; Instream id (if it exists)
        do 260 nf=1,numifr
          if(ifrsta(nf).eq.is) then
            rid = 5000.0+nf          
c
c rrb 99/12/23; identify if an ISF plus a gage
            if(rid.gt.0 .and. ris.lt.-10000.0) rid=-1.0*rid

            k1 = k1+1
            k1=amin0(maxStr,k1)                        
            cstr(k1) = cifrid(nf)
c
c __________________________________________________
c rrb 2005/07/22; Get call data for an ISF
            ctype1='InstreamFlow'
c           write(nlog,*) '  Outmon; instream flow'
            call OutCallS(imcdL(is), nf, ccallR(is),ctype1,ccallID)
            rimcdX=imcdL(is)            
          endif
  260   continue
c
c _________________________________________________________
c
c              Step 16; Find Plan id (if it exists)
c     write(nlog,*) ' Outmon; is, nplan',  is, nplan
c     write(6,*) ' Outmon; nplan',  is, nplan

        do np=1,nplan
          if(ipsta(np).eq.is) then
            rid = 10000.0+np         
c           write(nlog,*) ' Outmon; np, rid, ris', np, rid, ris
c
c 		Identify if a Plan plus a gage
            if(rid.gt.0 .and. ris.lt.-10000.0) rid=-1.0*rid

            k1 = k1+1
            k1=amin0(maxStr,k1)                        
            cstr(k1) = Pid(np)
c
c 		Adjust diversion to include spill from a plan (a negative value)
            RivDiv = RivIn - ofl(is) - RivWel
cx             write(nlog,*) 
cx     1        'Outmon; np, is, cstaid(is), RivDiv, Rivin, ofl' 
cx             write(nlog,*) 
cx     1        'Outmon;', np, is, cstaid(is), RivDiv*fac, Rivin*fac,
cx     1                      ofl(is)*fac      
          endif
        end do  
c
c _________________________________________________________
c
c rrb 01/03/05; Step 17; Well Only
c               Reassign ID if a well only structure
c
c              Find a well only id (if it exists)
c     write(nlog,*) ' Outmon; is, numdivw',  is, numdivw
c     write(6,*) ' Outmon; numdivw',  is, numdivw
        do nw=1,numdivw
          nd=idivcow2(nw)
          
          if(nd.eq.0) then
            if(idvstaw(nw).eq.is) then
              useCU= dcutw(nw) 
c
              retx = TotSup1-UseCu-rlossx(is)-qtosoil(is)
              retx = amax1(retx, 0.0)
              rid = 12500+nw
c
              if(rid.gt.0 .and. ris.lt.-12500.0) rid=-1.0*rid
              k1 = k1+1
c
c rrb 2009/02/23; Correction              
              k1=amin0(maxStr,k1)                        
              cstr(k1) = cdividw(nw)
c
c __________________________________________________
c rrb 2005/07/22; Get call data for a well
             ctype1='Well        '
             call OutCallS(imcdL(is), nw, ccallR(is), ctype1, ccallID)        
             rimcdX=imcdL(is)            
           endif
         endif
        end do
c
c
c _________________________________________________________
c              Identify multiple structures at this river node

        xstr = min0(1,k1)
        do 270 k=2,k1
          if(cstr(1).ne.cstr(k)) xstr = xstr+1
          if(iout.eq.4) then
            write(nlog,*) ' Outmon; Warning multiple structure output'
            write(nlog,*) '         at river node ', cstaid(is) 
          endif  
  270   continue
c _________________________________________________________
c
c               Step 18; Print  *.xdd
c
c rrb 2005/11/29; Add qdiv(32,is) and Qdiv(33,is) for River and Canal Loss
        
        shortx(is) = amax1(demx(is)-TotSup1, 0.0)
c
c rrb 2008/01/15; In Outbal, adjust diversion for qdiv(30 (Plan to a T&C)    
        cx=qdiv(15,is)+qdiv(30,is)
c
c        
        write(43,rec=irecs)
     1    demx(is),  diwrz(is),   RivPri,     RivSto,      RivExPl,
     1    ClossDR,   well,        CarPri,     CarStoEx,    ClossDC, 
     1    Carried,   qfrsoil(is), TotSup1,    shortx(is),  shortiw(is), 
     1    UseCu,     qtosoil(is), retx,       rlossx(is),  avinp(is),  
     1    gai(is),   ret(is),     dep(is),    GwSto,       RivIn,  
     1    RivDiv,    RivWel,      ofl(is),    AvFlow,      qdiv(14,is), 
     1    cx,        qdiv(22,is), rlossx2(is),qdiv(35,is), rid,
     1    xstr,      rimcdX,      ccallR(is)
     
c
c rrb 2011/02/25; Print *.b43 to *.log
        if(iout.eq.6) then
          write(nlog,*) '  Outmon; iout=6 ', iyrmo(mon),xmonam(mon),
     1      is, cstaid(is), 
     1      demx(is)*fac,  diwrz(is)*fac,   RivPri*fac,  
     1      RivSto*fac,      RivExPl*fac
cx     1    ClossDR,   well,        CarPri,     CarStoEx,    ClossDC, 
cx     1    Carried,   qfrsoil(is), TotSup1,    shortx(is),  shortiw(is), 
cx     1    UseCu,     qtosoil(is), retx,       rlossx(is),  avinp(is),  
cx     1    gai(is),   ret(is),     dep(is),    GwSto,       RivIn,  
cx     1    RivDiv,    RivWel,      ofl(is),    AvFlow,      qdiv(14,is), 
cx     1    cx,        qdiv(22,is), rlossx2(is),qdiv(35,is), rid,
cx     1    xstr,      rimcdX,      ccallR(is)     
        endif
c
c rrb 2006/03/17; Additional output
            tempx=qdiv(14,is)+qdiv(15,is)+qdiv(30,is)- qdiv(22,is)
            if(iout.eq.3 .and. abs(tempx).gt.0.001) 
     1        write(io99,266) 99, is, rid, xstr,
     1        qdiv(14,is), qdiv(15,is), qdiv(30,is), 
     1        qdiv(22,is)
 266  format(' OutMon Diversion adjustment;',
     1 ' #, is, rid, xstr, qdiv(14,is) qdiv(15,is),qdiv(30,is),',
     1 ' qdiv(22,is) = ', 2i5, 20f12.4)   
     
c        
c
c               Debug printout
        IF(IOPOUT.gt.0) then
          write(99,280) is,cstaid(is),ccallID, RimcdX, ccallR(is)
  280     FORMAT(i5,1x, a12,1x,a12, 16F7.2)
        endif
C
c ---------------------------------------------------------
c
c               End station loop for diversion output
  290 CONTINUE
c
c rrb 01/03/28; Moved to execut.for
c     DO NR=1,NSTRTN
c       RETUR(IMO,NR)=0.
c       depl(imo,nr)=0.0
c     end do
c
c _________________________________________________________
c               Step 19; Reservoir Output (*.xre)
c
c     write(nlog,*) ' Outmon; numres',  numres
c     write(6,*) ' Outmon; numres',  numres
      IF(NUMRES.EQ.0.OR.NRSACT.EQ.0) goto 470 
      DO 350 NR=1,NUMRES
C
        IF(IRESSW(NR).EQ.0) GO TO 350

        QRES(5,NR)=PROJTF(NR)+REPLAC(NR)
C
        NOI=NOWNER(NR)
        NOE=NOWNER(NR+1)-1
C
        IF(IOPOUT.Gt.0) then
          WRITE(99,320)IYR,MON,NR,IRSSTA(NR),NOI,NOE,
     1                 (CUROWN(NO),NO=NOI,NOE)
  320     FORMAT(' OUTMON; Reservoir Account Info ',6I7,10F9.0)
        endif

  350 CONTINUE
c
c rrb 01/12/26; nx is not defined first time through
      nx = nrsact+numown
      ireca=(iyr-iystr)*12*nx   +(mon-1)*nx+ numtop
c     nx = nrsact+numown
c
      IF(IOPOUT.Gt.0) then
        WRITE(99,360) IYR,MON
  360   FORMAT(' QRES ',2I10)
      endif
C
      if(iout.eq.1) then
        write(nlog,*) '  Outmon; i, nowner(i)'
        do i=1,numres
          write(nlog,*) '  Outmon;', i, nowner(i)
        end do  
        i1=numres+1
        write(nlog,*) '  Outmon;', i1, nowner(i1)
      endif  
      

      DO 460 NR=1,NUMRES
        ridr=0
        IF(IRESSW(NR).EQ.0) GO TO 460
        IS=IRSSTA(NR)
        
        REvap     =EVAP  (NR)/FACTOR/MTHDAY(MON)
        RSeep     =SEPACT(NR)/FACTOR/MTHDAY(MON)
        RSto1     =volint(nr)/factor/mthday(mon)
C
        DO NO=1,3
          TEMP3(NO)=0.
        end do
C
        NOI=NOWNER(NR)
        NOE=NOWNER(NR+1)-1
        DO NO=NOI,NOE
          IO=NO-NOI+1
          TEMP3(IO)=QMAINS(2,NO)/FACTOR/MTHDAY(MON)
        end do
c
c ---------------------------------------------------------
c		Change units to cfs except for Qres(5,nr)
        DO J=1,4
          QRES(J,NR)=QRES(J,NR)/FACTOR/MTHDAY(MON)
        end do
c
        DO J=6,maxacc
          QRES(J,NR)=QRES(J,NR)/FACTOR/MTHDAY(MON)
        end do
        
        RRivPri  = qres(1,nr)      
c
c rrb 2006/06/07; Add from OOP (qres(28,nr)) 		                  
        RRivExc  = qres(3,nr) + qres(18,nr) + qres(28,nr)
        RCarPri  = qres(2,nr)
        RCarSto  = qres(4,nr)
        RRivSto  = qres(26,nr)
        RClossC  = qres(27,nr)
c
c  rrb 2010/10/15; For water balance report system (tranloss)
c                  at the destination        
cx      RClossR  = 0.0
        RClossR  = qres(30,nr)
        RSto2    = cursto(nr)/fz
        
C
c       CARRY(NR)=OFL(IS)-(RRivPri+QRES(3,NR)+QRES(16,NR)+
c    1            QRES(18,NR))
c
c rrb 2006/10/03; Not used. Replace with carried used by Outbal
cr      CARRY(NR)=OFLz(IS)-(RRivPri+QRES(3,NR)+QRES(16,NR)+
cr   1            QRES(18,NR))
C
        AVT(IS)=AVAIL(IS)
        NDNN=NDNNOD(IS)
        CALL DNMFSO(maxsta, AVT, IDNCOD, IS, NDNN, IM)
c
c rrb 2006/10/03; Not used. Replace with carried used by Outbal
c		Note qres(29,nr) is amount transfered between
c		accounts in the same reservoir in Rsrspu.for
cr      CARRY(NR)=CARRY(NR)-AVT(IM)
c
c rrb 2006/10/27; Correction carry does not include loss
cr      carry1= RCarPri + RCarSto + RcLossC
        carry1= RCarPri + RCarSto - RcLossC
        
cx      carry(nr)= amax1(0.0, carry1-qres(29,nr))
        carry(nr)= carry1-qres(29,nr)
        
cr      if(iout.eq.5) then
        if(iout.eq.5 .and. qres(29,nr).gt.small) then
          write(nlog,*) ' OutMon; cresid   iyrmo  xmonam',
     1     '      nr  carry1   carry qres(29'
          write(nlog,*) ' OutMon; ', cresid(nr),iyrmo(mon),xmonam(mon),
     1      nr, carry1*fac, carry(nr)*fac, qres(29,nr)*fac 
        endif
c        
        SPILL(NR)=AVT(IM)
        n1=nowner(nr)
        n2=nowner(nr+1)-1
        acc= n2-n1+2
c
c rrb 2005/12/09; River and Carrier Loss
        RTotSup = RRivPri + RCarPri  + RCarSto + RRivExc +
     1            RRivSto - RClossC - RClossR
c
c rrb 03/14/96; Show powrel (release due to targets) as Seep and Spill
c rrb 2015/07/18; Correction
cx        RStoUse= qres(8,nr) + qres(9,nr)  + qres(12,nr) -
cx     1           qres(21,nr)- qres(11,nr)
        RStoUse= qres(8,nr) + qres(9,nr)  + qres(12,nr) - qres(21,nr)
     
        RStoExc  = qres(21,nr)
        RStoCar = qres(11,nr) + qres(16,nr) + qres(17,nr) + qres(22,nr)
        RTotRel = RStoUse      + RStoExc        + RStoCar
        

c rrb 03/14/96; Show powrel (release due to targets) as Seep and Spill
c rrb 2006/12/27; Add seepage from carrier loss
c       RSpill = RSeep +powrel(nr)
        RSpill = RSeep +powrel(nr)+qres(25,nr)
        tot16 = ritremx(nr)/fz
c
c rrb; 98/12/14; Wells
c       tot17 = avinp(is) + vir(is)    + gai(is)      + ret(is)                
        tot17 = avinp(is) + vir(is)    + gai(is)      + ret(is) 
     1                    + gw2riv(is) - gw2gw(is) - dep(is)
c
c rrb 99/08/16; Add stream data to include river by well, plus
c rrb 02/05/23; Tot18x (Station river release) should be
c               Total release (RTotRel) -  From Storage to Carrier
c               (RStoCar) + Seep and Spill (RSpill)
c       tot18x=RTotRel
c
c rrb 2006/10/27; Do not include seepage in release
cr      tot18x=RTotRel - RStoCar + RSpill
        tot18x=RTotRel - RStoCar + powrel(nr)

c
c rrb 01/12/07; Station balance total supply (column 19)
c               should not include water diverted
c               from carrier by priority (qres(2,nr)
c               from carrier by storage (qres(4,nr)
c       tot19x=RTotSup-qres(2,is) - qres(4,is)
c rrb 2006/10/27; Correction adjust for carrier loss
cr      tot19x=RTotSup-RCarPri - RCarSto 
        tot19x=RTotSup - (RCarPri + RCarSto - RclossC)

        tot20x=qdiv(25,is)
C
        ireca=ireca+1
        rnr=float(nr)
c _________________________________________________________
c
c		Step 20; Print Reservoir
        write(44,rec=ireca) 
     1    RSto1,    RRivPri,       RRivSto,  RRivExc,  RClossR,
     1    RCarPri,  RCarSto,       RclossC,  RTotSup,  RStoUse,
     1    RStoExc,  RStoCar,       RTotRel,  REvap,    RSpill,
     1    RSto2,    tarmax(nr)/fz, tot16,    tot17,    tot18x,
     1    tot19x,   tot20x,        oflx(is), carry(nr),rlossR(nr),
     1    RSeep,    ridr,          acc,      rnr
     
        if(iout.eq.2) then  
          write(nlog,123) ireca, nr,int(acc),int(ridr),cresid(nr)
 123      format('  Outmon; ireca, nr acc ridr, ID', 4i5, 1x, a12)
        endif
          
     
        if(iout.eq.1) then
          write(nlog,*) ' Outmon; Reservoir and account', nr, 0,           
     1    RSto1,    RRivPri,       RRivSto,  RRivExc,  RClossR,
     1    RCarPri,  RCarSto,       RclossC,  RTotSup,  RStoUse,
     1    RStoExc,  RStoCar,       RTotRel,  REvap,    RSpill,
     1    RSto2,    tarmax(nr)/fz, tot16,    tot17,    tot18x,
     1    tot19x,   tot20x,        oflx(is), carry(nr),rlossR(nr),
     1    ridr,     acc,           rnr
        endif
     
c
c              Calculalte limit on storage for the entire reservoir
c                by subtracting the amount in each account
        tot15 = volmax(nr)
        do n=n1,n2
          tot15=tot15 - accr(20,n)
        end do
c
c _________________________________________________________
c
c              Step 21; Print Reservoir account data
        do 450 n=n1,n2                                       
c
c rrb 2006/01/21; Do not adjust acc; it is constnat for a reservior 
c                 ridr is hte account number        
cx        acc=n
c
c              Initilize
          do  i=1,maxacc
            accr(i,n) = accr(i,n)/fz
          end do
c
          ridr   = ridr  + 1.0
          ireca  = ireca + 1
 
          RRivPri=accr(1,n)
          RRivSto=accr(26,n)
          RCarPri=accr(2,n)
          RCarSto=accr(4,n)
c
c rrb 2006/06/07; Add from OOP 		          
          RRivExc = accr(3,n) + accr(18,n) + accr(28,n)
c
c rrb 2006/09/15; Correction          
          RTotSup = RRivPri + RCarPri  + RCarSto + RRivExc +
     1             RRivSto - RClossC - RClossR
c          
c rrb 2015/07/27; Correction
cx          RStoUse= accr(8,n) + accr(9,n)  + accr(12,n) - accr(21,n) -
cx     1            accr(11,n)          
          RStoUse= accr(8,n) + accr(9,n)  + accr(12,n) - accr(21,n)
     
                            
          RStoExc  = accr(21,n)
c
          RStoCar = accr(11,n) + accr(22,n)
          RTotRel = RStoUse     + RStoExc + RStoCar
          RSpill = accr(25,n) + accr(19,n)
          RSeep  = accr(25,n)
          
          RClossR=0.0
          RClossC=accr(27,n)
          RSto1=accr(20,n)
          REvap=accr(24,n)
          RSto2=curown(n)/fz
c
c rrb 2006/01/21; Account Output
          rnr=float(nr)
          write(44,rec=ireca) 
     1      RSto1,        RRivPri,    RRivSto,    RRivExc,  RClossR,
     1      RCarPri,      RCarSto,    RClossC,    RTotSup,  RStoUse, 
     1      RStoExc,      RStoCar,    RTotRel,    REvap,    RSpill,
     1      RSto2,        tot15/fz,   accr(23,n), tot17,    tot18x,
     1      tot19x,       tot20x,     oflx(is),   carry(nr),rlossR(nr),
     1      Rseep,        ridr,       acc,        rnr
     
          if(iout.eq.2) then
            write(nlog,123) ireca, nr,int(acc),int(ridr),cresid(nr)
          endif
     
          if(iout.eq.1) then
            write(nlog,*) ' Outmon; Reservoir and account', nr, n, 
     1      RSto1,        RRivPri,    RRivSto,    RRivExc,  RClossR,
     1      RCarPri,      RCarSto,    RClossC,    RTotSup,  RStoUse, 
     1      RStoExc,      RStoCar,    RTotRel,    REvap,    RSpill,
     1      RSto2,        tot15/fz,   accr(23,n), tot17,    tot18x,
     1      tot19x,       tot20x,     oflx(is),   carry(nr),rlossR(nr),
     1      Rseep, 
     1      ridr,         acc,          rnr
          endif
     
  450   continue
C
        IF(IOPOUT.Gt.0) then
          write(99,280) nr,cstaid(is),(qres(j,nr),j=1,18)
        endif
C
  460 CONTINUE        
c
c _________________________________________________________
c               Step 25; Print Operating Rule Data (*.xop)
c
  470 continue
c     write(nlog,*) ' Outmon; numopr',  numopr
c     write(6,*) ' Outmon; numopr',  numopr
  
      do i=1,numopr
c
        IREC=(IYR-IYSTR)*12*NUMopr+(MON-1)*NUMopr + i
        write(45,rec=irec) divo(i)                
      end do
c
c _________________________________________________________
c               Step 26; Print Instream Flow Reach Data
c
c     write(nlog,*) ' Outmon; numifr',  numifr
c     write(6,*) ' Outmon; numifr',  numifr

      if(numifr.gt.0) then

cr      ib=ndnifb(nf)
cr      ie=ndnifb(nf) + ndnifs(nf) - 1      

        n=ndnifb(numifr)+ndnifs(numifr)-1
c       write(99,*) '  Outmon; n = ', n
        do i=1,n
          IREC=(IYR-IYSTR)*12*NUMrea+(MON-1)*NUMrea + i
          
          write(47,rec=irec) qdivr(i)
c         if(i.eq.51 .or.i.eq.52) 
c    1      write(nlog,*) ' Outmon;', i, qdivr(i)*fac
        end do
      endif
c
c _________________________________________________________
c               Step 27; Print Well Report (*.xwe)
c
c     write(nlog,*) ' Outmon; numdivw',  numdivw
c     write(6,*) ' Outmon; numdivw',  numdivw

      if(numdivw.gt.0) then
c
c rrb 01/12/26; Revise to work under f95
        irecw=(iyr-iystr)*12*numdivw +(mon-1)*numdivw+ numtop

        do nw=1,numdivw
          is=idvstaw(nw)
          nd=idivcow2(nw)
          irecw=irecw+1
c
c ---------------------------------------------------------
c               Step 6a Well Report Well only structures
c
          if(nd.eq.0) then
            totx=divmonw(nw) + 0.0 + qdivswo(nw)  

            shrtx=amax1(diverw(mon,nw)-totx, 0.0)
c
c rrb 01/12/04; Soil moisture is already incuded in dcutw
c           shrtcu=diverirw(mon,nw) - dcutw(nw) - qdivswo(nw)
            shrtcu=diverirw(mon,nw) - dcutw(nw) 

            rety=divmonw(nw) - dcuw(nw)   - qdivsw(nw) - rlossw(nw) -
     1           carryW(nw)        
            gwx=divmonw(nw)  - rdepw(nw)  - rlossw2(nw)

            totox=dcutw(nw)  + qdivsw(nw) + rety  + rlossw(nw) +
     1            carryW(nw)
            tots=rdepw(nw)   + gwx        + rlossw2(nw) + qdivswo(nw)


            write(42,rec=irecw) 
     1        diverw(mon,nw), diverirw(mon,nw), divmonw(nw),
     1        0.0,            qdivswo(nw),      
     1        totx,           shrtx,          shrtcu,          
     1        dcutw(nw),      qdivsw(nw),  
     1        rety,           rlossw(nw),     carryW(nw), 
     1        totox,          rdepw(nw),  
     1        gwx,            rlossw2(nw),    qdivswo(nw),    tots

          else
c
c ---------------------------------------------------------
c               Step 6b Well Report D&W structures
c

            divx=totothw(nd)-divmonw(nw) 
            totx=divmonw(nw) + divx + qdivso(nd)

            shrtx=amax1(divert(mon,nd) - totx, 0.0)
c
c rrb 01/12/04; Soil moisture is already incuded in dcut(nd)
c           shrtcu=diverirt(mon,nd)- dcut(nd) - qdivso(nd)
            shrtcu=diverirt(mon,nd)- dcut(nd)        

            rety=divx        - dcu(nd)   - qdivs(nd)  - rloss(nd) +
     1           divmonw(nw) - dcuw(nw)  - qdivsw(nw) - rlossw(nw)-
     1           carryW(nw)
            gwx= divmonw(nw) - rdepw(nw) - rlossw2(nw)

            totox=dcut(nd)  + qdivs(nd)  + qdivsw(nw) +  rety +
     1            rloss(nd) + rlossw(nw) + carryW(nw)
            tots= divx      + rdepw(nw)  + gwx  + rlossw2(nw) +
     1            qdivso(nd)

            write(42,rec=irecw) 
     1        divert(mon,nd), diverirt(mon,nd),     divmonw(nw),  
     1        divx,           qdivso(nd),
     1        totx,           shrtx,                shrtcu,          
     1        dcut(nd),       qdivs(nd)+qdivsw(nw),
     1        rety,           rloss(nd)+rlossw(nw), carryW(nw),
     1        totox,          divx+rdepw(nw),
     1        gwx,            rlossw2(nw),          qdivso(nd),
     1        tots
          endif
        end do
      endif
c
c _________________________________________________________
c               Task 28; Print Structure Summary (*.b67, *.xss)
c rrb 01/01/20; 
c rrb 2007/09/18; Move to separate routine so it can be called by 
c		  Vircom
      call outXssMo(numstax)

c
c _________________________________________________________
c
c               Task 29; Print Plan Data
      call outplnMo
      
c
c _________________________________________________________
c
c               Task 29; Print Detailed return data
      
      
         if(ioutRtn.ge.1) then
          write(nlog,520) 
            rett=0.0
            dept=0.0
            iprint=0
          
          do is=1,numsta
            c=abs(ret(is)) + abs(dep(is))
            if(c.gt.small .or. is.eq.numsta) then
              nout=nout+1
              retT=retT+ret(is)
              depT=depT+dep(is)
            
              if(ioutRtn.eq.1) then
                write(nlog,522) iyrmo(mon), xmonam(mon), iprint,
     1          is,cstaid(is), ret(is)*fac, dep(is)*fac, 
     1          retT*fac, depT*fac
              endif
            endif
          end do          
          
          write(nlog,'(a9,i5,1x,a4, i5,6x,a12,16x,20f8.0)')           
     1      '  OutMon;',iyrmo(mon),xmonam(mon),iprint,
     1      'Total       ', retT*fac, depT*fac
        endif


c     write(io99,*) '  OutMon; irecmax for *.xss = ', irecmax
c
c _________________________________________________________
c               Task 30; Return
c
      RETURN
c
c _________________________________________________________
c               Formats
c
 191  format('  Outmon; Adjusting demand at river node ', i8, 
     1       ' from ', f8.0, ' to ', 20f8.0)

 500  format(
     1  '  Outmon; Problem the numbers used to define structure type',/
     1  '          exceeded ',/
     1  '          numdiv = ', i5, ' but sized to ', i5,/
     1  '          numisf = ', i5, ' but sized to ', i5,/
     1  '          numres = ', i5, ' but sized to ', i5)
 510  format(
     1  '  Outmon; Problem, user ID (iu) does not equal Ditch ID (nd)',
     1 /,'          iu = ', i5, ' nd = ', i5)

 520        format(/, '  OutMon; Return & Depletion Detail',/
     1        '  OutMon; Year  Mon    #   is ID         ',
     1        '     Ret     Dep    RetT    depT') 
     
 522        format('  OutMon;', i5, 1x, a4, 2i5,1x,a12,20f8.0) 
 
 9999 write(6,1050)
      write(io99,1051) 
c
c _________________________________________________________
c               Error Processing
 1050 format('    Stopped in Outmon',/,
     1       '    See the *.log file')
 1051 format('    Stopped in Outmon')
      write(6,*) 'Stop 1' 
      call flush(6)
      call exit(1)

      stop 

      END
