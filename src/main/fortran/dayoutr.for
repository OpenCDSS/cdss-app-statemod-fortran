c
c _________________________________________________________
c	Dimensions
cC     Last change:  RRB   8 Jan 2002    5:23 pm
C
      subroutine dayoutr(maxstax)
c
c
c _________________________________________________________
c	Program Description
c
c
c       Dayoutr; It prints daily binary data
c                Same as outmon, but revised for daily data
c
c                Called by daymon.for
c
c _________________________________________________________
c	Update History
c
c               Update History
c rrb 99/12/28; New Id convention as follows:
c                               0-5000 = reservoir
c                               5001 - 7500 = ISF
c                               7501 - 10000 = reservoir
c				10001 -12500 = Plan
c                               < -10000 = baseflow only
c                               -1* above = baseflow plus a structure 
c rrb 00/07/03; For D&W structures added adjustment to
c               demand (demcon) based on supply to meet  demand
c rrb 00/12/26; Added variable efficiency capability
c               ieffmax = 1 (variable efficiency used along with new CU 
c               ieffmax = 2 average efficiency used but new CU approach 
c rrb 01/12/04; Revise River Divert (RivDiv) to not include CU from
c               soil (qfrsoil(is))
c rrb 01/12/04; Revised printout regarding demands and shortages
c               Set demcon(mon,is)   = demx(is)
c               Set diwrmon,nd)      = diwrz(is)
c               Set diverirt(mon,iu) = diveritd(idy,iu)
c               Set diverw(mon,nw)   = diverdxw(idy),nw)
c               Set diverirw(mon,nw) = diverd2(idy,nw)
c               Set divert(mon,nd)   = divertd(idy,nd)
c               Set diverirt(mon,nd) = diveritd(idy,nd)
c rrb 2007/02/23; Revised *.xdy to include well only in the balance
c		  Revised *.xdy output column 33 = rlossw2 (salvage)
c
c _________________________________________________________
c
c       Documentation
c                       
c               file 49 *.xdy   Daily diversion and stream
c               file 50 *.xry   Daily reservoir
c               file 65 *.xwe   Daily well
c
c
c               ofl(is)         river outflow to diversion output
c               oflx(is)        river outflow to reservoir output
c               oflz(is)        adj river outflow for min downatream 
c                               calcuations and carrier calculations
c               avt             min flow downstream (limits diversion)
c               tot18           available flow
c
c

c
c _________________________________________________________
c
c               Dimensions
C
      include 'common.inc'
      dimension cstr(20)
      character cstr*12, ccallID*12, ctype1*12
c
      dimension
     1  ret(maxstax),      gai(maxstax),     ofl(maxstax),
     1  avt(maxstax),      temp3(maxstax)
      dimension
     1  shortx(maxstax),   oflx(maxstax),    oflz(maxstax),
     1  dep(maxstax),      rlossx(maxstax),  rlossx2(maxstax),
     1  totothw(maxstax),  shortiw(maxstax), demx(maxstax),
     1  diwrz(maxstax),    qtosoil(maxstax), qfrsoil(maxstax)
c
c 
c _________________________________________________________
c               Step 1; Initilize
c
      small = 0.001
      if(iresop.eq.2) then
        fz=fdy(mon)  
      else
        fz=faf(mon)
      endif
c
c
c		Set number of output values 1x in StateM.for
c	        and store in common.inc      
      ndiv = ndivO        
      nres = nresO
      ndivw= nwelO
      
      do i=1,maxio
        dat1(i)=0.0
      end do

      do is=1,numsta
        gai(is)=0.
        ofl(is)=0.
        avt(is)=0.
        temp3(is)=0.
        ret(is)=0.
        dep(is)=0.
        rlossx(is)=0.
c
c rrb 2007/02/23; Add Salvage to *.xdy        
        rlossx2(is)=0.0
        shortx(is)=0.
        oflx(is)=0.
        oflz(is)=0.
        demx(is)=0.
        diwrz(is)=0.0
        shortiw(is)=0.0
        qtosoil(is)=0.0
        qfrsoil(is)=0.0
c
c rrb 01/12/28; Add temporary running monthly demand
        tempd(is)=0.0
      end do
c
c _________________________________________________________
c               Step 2; Daily Diversion & Stream Output (*.xdy)
c
      do is=1,numsta
        qdiv(4,is)=qdiv(5,is)+qdiv(6,is)+qdiv(8,is)+qdiv(9,is)+
     1             qdiv(16,is)
      end do
c
c
c _________________________________________________________
c		Diversion Processing
      do 150 nd=1,numdiv
        totothw(nd)=0.0
        is =idvsta(nd)
c
c rrb 01/12/28; Add temporary running monthly demand
        tempd(is)=diverx(nd)
c        
c               Loss by wells
        nw=idivco2(nd)
        if(nw.eq.0) then
          rlossx(is)=rloss(nd)
        else
          rlossx(is) =rloss(nd)+rlossw(nw)
c
c rrb 2007/02/23; Add salvage to *.xdd          
          rlossx2(is)=rlossw2(nw)          
        endif

        if(idrgst(nd).ne.0.and.irsord(2,is).ne.0) go to 150
        if(idivsw(nd).eq.0) go to 150
c
        iui=nduser(nd)
        iue=nduser(nd+1)-1
c
c rrb 00/06/16; Adjust to include total demand (divert()) from datinp
c !!!!! rrb Warning not yet included, no variable for daily total demand
        if(iui.gt.iue) go to 150
        do 140 iu=iui,iue
c
c rrb 01/02/19; Use daily values
c         demx(is)=divert(mon,iu)
c         diwrz(is)=diverirt(mon,iu)c
c rrb 02/01/08; Adjust demand if idaydem = 1 (option to use monthly
c               running demand is on)
          IF(idaydem.eq.0) then
            demx(is)=divertd(idy,iu)
          else
            demx(is)=diverx(iu)
          endif

          diwrz(is)=diveritd(idy,iu)

          if(nw.eq.0) then
            qtosoil(is)=qdivs(iu)
            qfrsoil(is)=qdivso(iu)
          else
            qtosoil(is)=qdivs(iu) + qdivsw(nw)
            qfrsoil(is)=qdivso(iu) + qdivswo(nw)
          endif
c
c rrb 01/12/04; Use daily values
c         shortiw(is)=amax1(0.0, shortiw(is) + diverirt(mon,iu)
c    1               - dcut(iu))
          shortiw(is)=amax1(0.0, shortiw(is) + diveritd(idy,iu)
     1               - dcut(iu))


          if(diverd(idy,iu).gt.0.0001) then
            qdiv(1,is)=qdiv(1,is)+diverd(idy,iu)
            if(irturn(iu).ne.4) then
              qdiv(2,is)=qdiv(2,is)+diverd(idy,iu)
            else
              qdiv(3,is)=qdiv(3,is)+diverd(idy,iu)
            endif
            qdiv(11,is)=qdiv(11,is)+divreq(iu)
          endif
  140   continue
  150 continue
c
c _________________________________________________________
c               Return
      do nr=1,nstrtn
        is=istrtn(nr)
        ret(is)=returd(ido,nr)
        dep(is)=depld(ido,nr)
      end do
c
c _________________________________________________________
c               Gain
      do np=1,numrun
        is=irusta(np)
        gai(is)=gai(is)+virind(idy,np)
      end do
c
c _________________________________________________________
c               Instream
      do nf=1,numifr
        if(ifrrsw(nf).ne.0) then
          is=ifrsta(nf)
          demx(is)=demx(is) + flowrd(idy,nf)
          
          qdiv(12,is)=flowrd(idy,nf)
          qdiv(11,is)=qdiv(11,is)+flowrq(nf)
        endif
      end do
c
c _________________________________________________________
c               Step X; Plans
      DO  np=1,nplan
        IF(pon(np).gt.0) then
          IS=Ipsta(np)
          if(iplntyp(np).eq.1 .or. iplntyp(np).eq.2) then
            demx(is)=demx(is) + pdemT(np)          
          
            qdiv(11,is)=qdiv(11,is)+Pdem(np)
            QDIV(12,IS)=pdemT(np)
            
c           write(nlog,*)
c    1      '  Outmon; is, np, demx(is)', is, np, demx(is)*fac
          endif
          
          if(iplntyp(np).ge.3) then
            qdiv(11,is)=qdiv(11,is) + Psuply(np)
            qdiv(12,is)=qdiv(12,is) + psuplyT(np)
          endif  
        endif
      end do    
c
c _________________________________________________________
c               Step 8; Well Only Calculations 
c               Note this routine calculates data for Well Only lands
        
        if(iwell.ge.1) then  
          do nw=1,numdivw
            nd=idivcow2(nw)
            if(nd.eq.0) then
              is = idvstaw(nw)
              c = diverw(mon,nw)
              supplyx = divmonw(nw) + qdivswo(nw)
              shorty  = amax1(diverw(mon,nw)-supplyx, 0.0)
c
c rrb 00/08/02; Express demand (demcon) using average SW efficiency
c               Note Mdainp checks & warns if 
c               demand > 0 and efficiency = 0                 
c               
              if(diveffw(mon,nw).gt.small) then
                demcon(mon,is) = usemonw(nw) + 
     1           (shorty / (diveffw(mon,nw)/100.))
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

c             write(nlog,191) is, c*fz, demcon(mon,is)*fz, 
c    1           diverirt(mon,nd)*fz, supplyx*fz, shorty*fz
            endif
          end do
        endif
c
c _________________________________________________________
c               River Station Data
      do is=1,numsta
c

        ofl(is)=river(is)
        oflx(is)=river(is)              
        oflz(is)=river(is)              
c
c rrb 98/12/22; Adjust "Well Depletion" to not include current month
c               which is shown under "river by Well" (qdiv(25,is)
c rrb 99/06/12; qdiv(23 is now exchange by interruptable supply
c       dep(is)=dep(is)-qdiv(23,is)
        dep(is)=dep(is)-qdiv(25,is)

        if(irsord(2,is).ne.0) then
          oflz(is)=avinp(is)+gai(is)+ret(is)-qdiv(5,is)
     1        -qdiv(6,is)-qdiv(8,is)-qdiv(9,is)-qdiv(7,is)-qdiv(10,is)
     1        +gw2riv(is)-gw2gw(is)-dep(is)

        endif
      end do
c
c _________________________________________________________
c               Available Flow, adjusted for reservoirs
      do is=1,numsta
        avt(is)=avail(is)
      end do

      do nr=1,numres
        if(iressw(nr).gt.0) then
          is=irssta(nr)
          avt(is)=amin1(avt(is),oflz(is))
        endif
      end do  
c
      if(iopout.gt.0) then
        write(nlog,230) iyr,mon
      endif
c
c     irec=(iyr-iystr)*12*numsta+(mon-1)*numsta+ numtop
      irec=((iyr-iystr)*12+(mon-1))*numsta*31 + 
     1              (idy-1)*numsta+numtop
c
c _________________________________________________________
c               Finally Daily printout
      do 290 is=1,numsta
        rimcdX=-1
        ccallR(is)=-1.0        
      
        irecs=irec+is
        ndnn =ndnnod(is)
        call dnmfso(maxsta,avt,idncod,is,ndnn,im)
c
        RivPri  = qdiv(8,is)  + qdiv(14,is) + qdiv(5,is) - qdiv(16,is)
        
        RivSto  = qdiv(10,is) + qdiv(15,is) + qdiv(7,is) - qdiv(21,is) 
c
        RivExPl  = qdiv(21,is) + qdiv(23,is) + qdiv(26,is) +
     1          qdiv(29,is) + qdiv(30,is) + qdiv(31,is)
c
c rrb 99/04/18; Add Wells
        Well = qdiv(24,is)
        RivWel= qdiv(25,is)
c
        CarPri= qdiv(16,is) + qdiv(19,is)
        CarStoEx=qdiv(20,is)
c
c rrb 2005/11/29; River and Carrier Loss        
        CLossDC=qdiv(32,is)
        ClossDR=qdiv(33,is)
c        
        Carried  = qdiv(18,is) + qdiv(27,is) + qdiv(28,is)     
        CarriedX = qdiv(18,is)               + qdiv(28,is)
c
        TotSup1  = RivPri + RivSto + RivExPl + CarPri + Well + CarStoEx
     1           +qfrsoil(is) - CarriedX   - ClossDC - ClossDR
        TotSup1  = amax1(0.0, TotSup1)
c
c _________________________________________________________
c              find diversion(s) at this station
c              assumes one per station
        UseCU = 0.0
        retx  = 0.0
        rid   = 0.0   
        k1    = 0
c
c rrb 2006/01/19; Update        
        ris   = 0.0                           
c
c _________________________________________________________
c rrb 99/12/28; Find a baseflow point (surrogate for a gage)
        do n=1,numrun
          if(irusta(n).eq.is) then
            rid=-1*(10000.0+float(n))
            ris=rid
          endif
        end do
c
c _________________________________________________________
c              find diversion id (if it exists)
c              assumes one structure per river station
c                      note diveff(im,nd) = 0 for instream
        nw=0
        do 250 nd=1,numdiv
          if(idvsta(nd).eq.is) then
            nw=idivco2(nd)
            if(k1.eq.0) then
              UseCU=dcut(nd)
              totothw(nd)=TotSup1-qfrsoil(is)
            endif
            rid = nd
c
c rrb 99/12/28; Identify if a diversion plus a gage
            if(rid.gt.0 .and. ris.lt.-10000) rid=-1.0*rid

            k1 = k1+1
            cstr(k1) = cdivid(nd)
c
c __________________________________________________
c rrb 2005/07/22; Get call data for a diversion
            ctype1='Diversion'
            call OutCallS(imcdL(is), nd, ccallR(is), ctype1, ccallID)        
            rimcdX=imcdL(is)            
            
          endif
  250   continue   
c
c               Calculate Total return (retx) = Total Supply
c                 (TotSup1) - CU (UseCU) - loss (rlossx) 
c                 - To Soil (qtosoil) - carried water (qdiv(18,is)
c
c rrb 2005/11/29; TotSup1 is already adjusted for qdiv(18,is)
c       retx = TotSup1-UseCU -rlossx(is) - qtosoil(is) - qdiv(18,is)
        retx = TotSup1-UseCU-rlossx(is) - qtosoil(is) 
        retx = amax1(0.0, retx)
c
c rrb; 99/04/14; Wells
        tot14x=gw2riv(is)-gw2gw(is)
        RivIn = avinp(is)+gai(is)+ret(is)+gw2riv(is)-gw2gw(is)
     1                    -dep(is)

c

c rrb 2005/11/29; River and Carrier Loss
c       RivDiv = TotSup1 - CarPri - CarStoEx - Well - qfrsoil(is) 
c
c rrb 2009/05/01
cx        RivDiv = TotSup1 - CarPri - CarStoEx - Well - qfrsoil(is) 
cx     1          + ClossDC + ClossDR 
cx        
        CarryY=CarPri + CarStoEx - Carried - ClossDC             
        RivDiv = TotSup1 - CarryY - well + ClossDR - qfrsoil(is)
c
c _________________________________________________________
c
c		Find a reservoir ID (if it exists)
c rrb 01/15/95; re calculate diversion if a reservoir
c               store resid even for diversion printout
        do nr=1,numres
          if(irssta(nr).eq.is) then
c
            RivDiv = RivIn - ofl(is) - RivWel
c
            rid = nr+7500
            if(rid.gt.0 .and. ris.lt.-10000.0) rid=-1.0*rid
            k1 = k1+1
            cstr(k1) = cresid(nr)
c
c __________________________________________________
c rrb 2005/07/22; Get call data for a reservoir
            ctype1='Reservoir'
            call OutCallS(imcdL(is), nr, ccallR(is), ctype1, ccallID)        
            rimcdX=imcdL(is)            
            
          endif
        end do  

c
c              (im is the node that limits the diversion)
        tot18 = avt(im)
c
c _________________________________________________________
c              Find instream id (if it exists)
        do nf=1,numifr
          if(ifrsta(nf).eq.is) then
            rid = 5000.0 + nf
c
c rrb 99/12/28; Identify if a ISF plus a gage
            if(rid.gt.0 .and. ris.lt.-10000.0) rid=-1.0*rid

            k1 = k1+1
            cstr(k1) = cifrid(nf)
c
c __________________________________________________
c rrb 2005/07/22; Get call data for an ISF
            ctype1='InstreamFlow'
            call OutCallS(imcdL(is), nf, ccallR(is),ctype1,ccallID)
            rimcdX=imcdL(is)            
            
          endif
        end do  
c
c _________________________________________________________
c
c              Find Plan id (if it exists)
        do np=1,nplan
          if(ipsta(np).eq.is) then
            rid = 10000.0+np         
c           write(nlog,*) ' Outmon; np, rid', np, rid
c
c 		Identify if an Plan plus a gage
            if(rid.gt.0 .and. ris.lt.-10000.0) rid=-1.0*rid

            k1 = k1+1
            cstr(k1) = Pid(np)
c
c		Adjust diversion to include a spill from a plan
c		(a negative value)            
            RivDiv = RivIn - ofl(is) - RivWel
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
              retx = TotSup1-UseCu-rlossx(is)-qtosoil(is)-qdiv(18,is)
              retx = amax1(retx, 0.0)
              rid = 12500+nw
c
c rrb 99/12/23; identify if a well plus a gage
c rrb 2007/02/21; Add wells
              if(rid.gt.0 .and. ris.lt.-12500.0) rid=-1.0*rid
              k1 = k1+1
             cstr(k1) = cdividw(nw)
           endif
         endif
        end do
           
                               
c
c _________________________________________________________
c              Process multiple structures at this river node
        xstr = min0(1,k1)
        do k=2,k1
          if(cstr(1).ne.cstr(k)) xstr = xstr+1
        end do
c rrb 04/09/21;	For total shortage subtract water exchanged 
c               out of a structure (qdiv(27  NO
c       shortx(is)=amax1(demx(is)-TotSup1-qdiv(27,is),0.0)
        shortx(is)=amax1(demx(is)-TotSup1,0.0)
c
c rrb 01/12/10; Test printout to determine why monthly total shortage
c               < daily daily total shortage.  Conclusion because
c               daily can be negative due to soil moisture
          itest=0
          if(itest.eq.1) then
            ndx=ifix(rid)
            ndx=iabs(ndx)
            if(ndx.eq.51) then
              if(idy.eq.1) then
                xt=0.0
                yt=0.0
                write(nlog,*) ' '
                write(nlog,*) '  Dayoutr cdivid ', cdivid(ndx)
              endif

              x=demx(is) - TotSup1
              if(x.le.-0.00001) yt=yt+abs(x)
              xt=xt+x
c             write(nlog,101) mon, idy, demx(is),   TotSup1,   x,xt,yt
              write(nlog,101) mon, idy, demx(is)*fz,TotSup1*fz,
     1           x*fz, xt*fz, yt*fz
 101          format('  Dayoutr;', 2i5, 20f8.2)
            endif
          endif
          
c
c _________________________________________________________
c
c		Print Daily Binary Diversions, etc (*.b49)
c rrb 2005/11/29; Stream and Carrier Loss        
c		Binary Diversinos, etc.
c       write(nlog, *) ' DayoutR; Direct', iyr, mon, idy, irec, irecs
        write(49,rec=irecs)
     1    demx(is), diwrz(is),   RivPri,     RivSto,     RivExPl, 
     1    ClossDR,  Well,        CarPri,     CarStoEx,   ClossDC,
     1    Carried,  qfrsoil(is), TotSup1,     shortx(is), shortiw(is), 
     1    UseCU,    qtosoil(is), retx,       rlossx(is), avinp(is), 
     1    gai(is),  ret(is),     dep(is),    tot14x,     RivIn, 
     1    RivDiv,   RivWel,      ofl(is),    tot18,      qdiv(14,is), 
c
c rrb 2007/02/23; Use column 33 to be salvage (rlossx2)     
c    1    qdiv(15,is)+qdiv(30,is), qdiv(22,is),rlossx(is), 
     1    qdiv(15,is)+qdiv(30,is), qdiv(22,is),rlossx2(is), 
     1    rid,      xstr,  
     1    rimcdX,   ccallR(is)

                 
c     
c               debug printout
        if(iopout.gt.0) then
c         write(nlog,280) is,cstaid(is),(qdiv(j,is),j=1,ndiv)
        endif
c
c               end station loop for diversion output
  290 continue
c
c               The following changed to daily &
c               moved to execut.for
c     do nr=1,nstrtn
c       retur(imo,nr)=0.0
c       returd(idyt,nr)=0.
c     end do
c
c               Print 31 days/month for all months to simplify I/O
c               Note if < 31 dayx/month print all zeros
      idx=31-mthday(mon)

      if(idy.eq.mthday(mon) .and. idx.gt.0) then
c       write(nlog,*) '  DayoutR;',  idy, mthday(mon), idx
        do ix=mthday(mon)+1,idx
          do is=1,numsta
            irecs=irecs+1
            write(nlog, 110) iyrmo(mon), imomo(mon), idy,
     1                       irec, irecs
            write(49,rec=irecs) (dat1(i), i=1,ndiv)
          end do
        end do
      endif
c
c _________________________________________________________
c
c               Step 3; Daily Reservoir Output (*.xry)
c
      if(numres.eq.0.or.nrsact.eq.0) goto 470
c
      do nr=1,numres
        if(iressw(nr).gt.0) then
          qres(5,nr)=projtf(nr)+replac(nr)
          noi=nowner(nr)
          noe=nowner(nr+1)-1
          if(iopout.gt.0) then
            write(nlog,320)iyr,mon,nr,irssta(nr),noi,noe,
     1                   (curown(no),no=noi,noe)
          endif
        endif
      end do
c
      if(iopout.gt.0) then
        write(nlog,360) iyr,mon
      endif
c
      nx = nrsact+numown

c     ireca=((iyr-iystr)*12 +(mon-1))*nx+ numtop
      irec= ((iyr-iystr)*12 +(mon-1))*nx*31 +
     1      (idy-1)*nx + numtop
      ireca=irec

      do 460 nr=1,numres
c
c rrb 2009/06/01; Move below to operate after the iressw(nr) = 0 check
cx      ireca=ireca+1

        ridr = 0
        if(iressw(nr).eq.0) go to 460
        is=irssta(nr)
c
c rrb 2009/06/01; Moved from above to operate after iressw(nr) = 0 check
        ireca=ireca+1
        
        REvap     =evap  (nr)/fz
        RSeep     =sepact(nr)/fz
        RSto1     =volint(nr)/fz
c      
        do no=1,3
          temp3(no)=0.
        end do
c
        noi=nowner(nr)
        noe=nowner(nr+1)-1
        do no=noi,noe
          io=no-noi+1  
          temp3(io)=qmains(2,no)/fz 
        end do
c
        do j=1,4
          qres(j,nr)=qres(j,nr)/fz
        end do
c
        do j=6,maxacc
          qres(j,nr)=qres(j,nr)/fz
        end do
        
        RRivPri = qres(1,nr)
        RRivExc = qres(3,nr) + qres(18,nr) + qres(28,nr)
        RCarPri = qres(2,nr)
        RCarSto = qres(4,nr)
        RRivSto = qres(26,nr)
        RClossC = qres(27,nr)
        RClossR = 0.0
        RSto2   = cursto(nr)/fz
c
        carry(nr)=oflz(is)-(RRivPri+qres(3,nr)+qres(16,nr)
     1            +qres(18,nr))
        avt(is)=avail(is)

        ndnn=ndnnod(is)
        call dnmfso(maxsta,avt,idncod,is,ndnn,im)
c
c rrb 2009/05/01; Simplify        
cx      carry(nr)=carry(nr)-avt(im)
        carry1 = RCarPri + RCarSto - RClossC
        carry(nr)=carry1-qres(20,nr)
        
        spill(nr)=avt(im)

        n1=nowner(nr)
        n2=nowner(nr+1)-1
        acc  = n2-n1+2

        RTotSup = RRivPri + RCarPri  + RCarSto + RRivExc +
     1            RRivSto - RClossC - RClossR
        RStoUse  = qres(8,nr) + qres(9,nr)  + qres(12,nr) -
     1          qres(21,nr)- qres(11,nr)
        RStoExc  = qres(21,nr)
        RStoCar = qres(11,nr) + qres(16,nr) + qres(17,nr) + qres(22,nr)
        RTotRel = RStoUse        + RStoExc        + RStoCar
c
c rrb 2009/05/10; Update        
cx      RSpill = RSeep +powrel(nr)
        RSpill = RSeep +powrel(nr)+qres(25,nr)
        RBomDec = ritremx(nr)/fz
c
c rrb 99/08/16; Add Wells.
cx      tot17 = avinp(is)  + gai(is) + ret(is)
        RRivIn = avinp(is)  + gai(is) + ret(is)
     1                     + gw2riv(is) - gw2gw(is) - dep(is)
c
c rrb 99/08/16; Add Wells
c rrb 02/05/23; Tot18x (Station river release) should be
c               Total release (RTotRel) -  From Storage to Carrier
c               (RStoCar) + Seep and Spill (RSpill)
c       tot18x=RTotRel
cx        tot18x=RTotRel - RStoCar + RSpill
        RRel=RTotRel - RStoCar + RSpill
c
c rrb 02/05/24; Station balance shuld not include water diverted
c               by a carrier to the reservoir
c       tot19x=RTotSup
c rrb 2009/05/01
cx      tot19x=RTotSup-RCarPri - RCarSto
cx      tot19x = RTotSup - (RCarPri + RCarSto - RClossC)
        RRivDiv = RTotSup - (RCarPri + RCarSto - RClossC)

        RRivWel=qdiv(25,is)
        RTarLim=tarmax(nr)/fz
c
c		Binary Reservoir        
        rnr=float(nr)
c
c ---------------------------------------------------------        
cx        if(idy.eq.1) then
cx          write(nlog,*) ' DayOutR; ', nr, iyrmo(mon), xmonam(mon), 
cx     1    idy, ireca, cresid(nr)
cx        endif
        
        write(50,rec=ireca)
     1    RSto1,      RRivPri,    RRivSto,    RRivExc,   RClossR,
     1    RCarPri,    RCarSto,    RClossC,    RTotSup,   RStoUse, 
     1    RStoExc,    RStoCar,    RTotRel,    REvap,     RSpill,
     1    RSto2,      RTarLim,    RBomDec,    RRivIn,    RRel, 
     1    RRivDiv,    RRivWel,    oflx(is),   carry(nr), rlossR(nr),
     1    RSeep,      ridr,       acc,        rnr
c
c              calculalte limit on storage per account
        RTarLim = volmax(nr)
        do n=n1,n2
          RTarLim=RTarLim - accr(20,n)
        end do
c
c              print account data
        do n=n1,n2
          do i=1,maxacc
            accr(i,n) = accr(i,n)/fz
          end do

          ridr   = ridr  + 1
          ireca  = ireca + 1
          
          RRivPri = accr(1,n)
          RRivSto = accr(26,n)
          RCarPri = accr(2,n)
          RCarSto = accr(4,n)
c
          RRivExc  = accr(3,n) + accr(18,n) + accr(28,n)
          RTotSup  = RRivPri + RCarPri + RCarSto + RRivExc +
     1               RRivSto - RClossC - RClossR     
          RStoUse  = accr(8,n) + accr(9,n)  + accr(12,n) - accr(21,n) -
     1               accr(11,n)                                 
          RStoExc  = accr(21,n)
c
          RStoCar = accr(11,n) + accr(22,n)
          RTotRel = RStoUse       + RStoExc + RStoCar
          RSpill  = accr(25,n) + accr(19,n)
          RSeep=accr(25,n)
          
          RCLossR=0.0
          RCLossC= accr(27,n)
          RSto1=accr(20,n)
          REvap=accr(24,n)
          RSto2=curown(n)/fz
          RBomDec=accr(23,n)

c         write(nlog, 100) iyrmo(mon), imomo(mon), idy, 
c    1                  nr, n, irec, ireca
          rnr=float(nr)
          write(50,rec=ireca) 
     1      Rsto1,   RRivPri,  RRivSto,    RRivExc,   RClossR,
     1      RCarPri, RCarSto,  RClossC,    RTotSup,   RStoUse,   
     1      RStoExc, RStoCar,  RTotRel,    REvap,     Rspill,  
     1      RSto2,   RTarLim/fz,RbomDec,   RTarLim,   RRel,
     1      RRivDiv, RRivWel,  oflx(is),   carry(nr), rlossR(nr),
     1      Rseep,   ridr,     acc,        rnr
c
c               End account loop
        END do
c
        if(iopout.gt.0) then
          write(99,280) nr,cstaid(is),(qres(j,nr),j=1,18)
        endif
c
c               End reservoir loop
  460 continue
c
c               Print 31 days/month for all months to simplify I/O
c               Note for months with days < 31 days fill with zeros
      idx=31-mthday(mon)

      if(idy.eq.mthday(mon) .and. idx.gt.0) then
        do ix=mthday(mon)+1,31
          do is=1,nx
            ireca=ireca+1
c           write(nlog, 100) iyrmo(mon), imomo(mon),
c    1        ix, -1, is-1, irec, ireca
            write(50,rec=ireca) (dat1(i), i=1,nres)
          end do
        end do
      endif
c
c 
c _________________________________________________________
c
c               Step 4; Daily Operating Rule Output (*.opy)
c               !!! Not operational for daily 
c
  470 continue
c     do i602=1,numopr
c       divox=divo(i602)*fz
c       write(45) divox
c     end do
c
c _________________________________________________________
c               Step 5; Daily Instream Flow Reach Output (*.ify)
c               !!! Not operational for daily
c
c     if(numifr.gt.0) then
c       n=ndnifb(numifr)+ndnifs(numifr)-1
c       write(nlog,*) '  DayoutR; n = ', n
c       do i=1,n
c         write(47) qdivr(i)
c       end do
c     endif
c
c 
c _________________________________________________________
c               Step 6; Daily Well Output (*.xwy)
c
      if(numdivw.gt.0) then
        irec=((iyr-iystr)*12+(mon-1))*numdivw*31 + 
     1                (idy-1)*numdivw+numtop

        do nw=1,numdivw
          irecs=irec+nw
          is=idvstaw(nw)
          nd=idivcow2(nw)
c
c ---------------------------------------------------------
c               Step 6a; Well only Structures 
c
          if(nd.eq.0) then
            totx=divmonw(nw) + 0.0 + qdivswo(nw)
c
c rrb 01/12/04; Daily values
c           shrtx=amax1(diverw(mon,nw)-totx, 0.0)
            shrtx=amax1(diverdxw(idy,nw)-totx, 0.0)
c
c rrb 01/12/04; Daily values & Soil moisture are already in dcutw
c           shrtcu=diverirw(mon,nw) - dcutw(nw) - qdivswo(nw)
            shrtcu=diverd2(idy,nw)  - dcutw(nw)

            rety=divmonw(nw) - dcuw(nw)  - qdivsw(nw) - rlossw(nw) -
     1           carryW(nw)        
c
c rrb 01/03/01; Consistent with monthly (outmon)
            gwx=divmonw(nw)  - rdepw(nw) - rlossw2(nw) 

            totox=dcutw(nw) + qdivsw(nw) + rety + rlossw(nw) +
     1            carryW(nw)            
            tots=rdepw(nw)  + gwx        + rlossw2(nw) + qdivswo(nw)
c
c rrb 01/12/04; Binary Daily values 
            write(65,rec=irecs)
     1        diverdxw(idy,nw),diverd2(idy,nw), divmonw(nw),
     1        0.0,            qdivswo(nw),
     1        totx,           shrtx,          shrtcu,
     1        dcutw(nw),      qdivsw(nw),   
     1        rety,           rlossw(nw),     carryW(nw),
     1        totox,          rdepw(nw),    
     1        gwx,            rlossw2(nw),    qdivswo(nw), 
     1        tots
          else
c
c ---------------------------------------------------------
c               Step 6b; D&W Structures 
c
            divx=totothw(nd)-divmonw(nw)
            totx=divmonw(nw) + divx + qdivso(nd)
c
c rrb 01/12/04; Daily Values
c           shrtx=amax1(divert(mon,nd)  - totx, 0.0)
            shrtx=amax1(divertd(idy,nd)  - totx, 0.0)

c
c rrb 01/12/04; Daily values & soil CU is already included in dcut(nd)
c           shrtcu=diverirt(mon,nd) - dcut(nd) - qdivso(nd)
            shrtcu=diveritd(idy,nd) - dcut(nd)

            rety=divx        - dcu(nd)   - qdivs(nd)  - rloss(nd) +
     1           divmonw(nw) - dcuw(nw)  - qdivsw(nw) - rlossw(nw) -
     1           carryw(nw)
            gwx=divmonw(nw)  - rdepw(nw) - rlossw2(nw)

            totox= dcut(nd)  + qdivs(nd) + qdivsw(nw) + rety  + 
     1             rloss(nd) + rlossw(nw) +
     1             carryW(nw)     
            tots = divx      + rdepw(nw) + gwx  + rlossw2(nw) + 
     1             qdivso(nd)

            write(65,rec=irecs)
     1        divertd(idy,nd),diveritd(idy,nd),     divmonw(nw),  
     1        divx,           qdivso(nd),   
     1        totx,           shrtx,                shrtcu,
     1        dcut(nd),       qdivs(nd)+qdivsw(nw), 
     1        rety,           rloss(nd)+rlossw(nw), carryW(nw),          totox,
     1        divx+rdepw(nw), gwx,          
     1        rlossw2(nw),    qdivso(nd),           tots

          endif
c
c               Print 31 days/month for all months to simplify I/O
c               Note for months with dayx<31 print all zeros
          idx=31-mthday(mon)

          if(idy.eq.mthday(mon) .and. idx.gt.0) then
            do i=1,ndivw
              dat1(i) = 0.0
            end do

c           write(nlog,*) '  DayoutR;',  idy, mthday(mon), idx
            do ix=mthday(mon)+1,idx
              do is=1,numdivw
                irecs=irecs+1
c               write(nlog, 110) iyrmo(mon), imomo(mon), idy,
c    1                           irec, irecs
                write(65,rec=irecs) (dat1(i), i=1,ndivw)
              end do
            end do
          endif
        end do
      endif
c
c _________________________________________________________
c               Step 7; Return
      return
c 
c _________________________________________________________
c               Formats
c
 100      format(' DayoutR; Res  ',
     1                 '  iyr  mon  idy   nr    n irec ireca',/
     1           ' DayoutR; Res  ', 20i5)
 110      format(' DayoutR; Div  ',
     1                 '  iyr  mon  idy irec irecs',/
     1           ' DayoutR; Div  ', 20i5)
 120      format(' DayoutR; Well ',
     1                 '  iyr  mon  idy irec irecs',/
     1           ' DayoutR; Well ', 20i5)

  230   format(' qdiv ',2i10)
  280   format(i5,a12,16f7.2,/,10x,16f7.2)
  320 format(' DayoutR; reservoir account info ',6i7,10f9.0)
  360   format(' qres ',2i10)
      end
