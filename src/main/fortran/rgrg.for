c rgrg - simulates the  Rio Grande Compact for both the Rio Grande and Conejos River
c_________________________________________________________________NoticeStart_
c StateMod Water Allocation Model
c StateMod is a part of Colorado's Decision Support Systems (CDSS)
c Copyright (C) 1994-2021 Colorado Department of Natural Resources
c 
c StateMod is free software:  you can redistribute it and/or modify
c     it under the terms of the GNU General Public License as published by
c     the Free Software Foundation, either version 3 of the License, or
c     (at your option) any later version.
c 
c StateMod is distributed in the hope that it will be useful,
c     but WITHOUT ANY WARRANTY; without even the implied warranty of
c     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c     GNU General Public License for more details.
c 
c     You should have received a copy of the GNU General Public License
c     along with StateMod.  If not, see <https://www.gnu.org/licenses/>.
c_________________________________________________________________NoticeEnd___

      SUBROUTINE Rgrg(IW,L2,IRG,nrg,icarry,irgout)
c
c
c _________________________________________________________
c       Program Description
c
c       Rgrg; It simulates the  Rio Grande Compact for both the
c             Rio Grande and Conejos River
c
c _________________________________________________________
c       Update History
c
c rrb 99/09/09; Operating rule type 17; Rio Grande for Rio Grande
c rrb 00/06/09; Revise forecast in Oct is known and maximum surplus
c               is limited to 150,000 / yr and no accumulated limit
c rrb 00/10/06; Revised qnov and qdec to be an array and calculated
c               in correct location for Conejos.  Fixed problem with
c               annual Conejos qnov+qdec only.  Note must be an array 
c               because of when output is printed.
c rrb 00/10/23; Added rgspill(irg) to include historic Elephant Butte
c               spills
c rrb 01/08/10; Revised use of variables qdebt and qdebtx to be year
c               when debt payback begins and initial condition of debt
c               respectively
c rrb 01/08/28; Began to address daily operation
c               Added daily demand flowrd(idy,nf)
c rrb 01/11/14; Set avtemp = river or riverm/days per month              
c               to operate properly with both monthly and daily
c               time step                                      
c
c
c _________________________________________________________
c       Documentation
c               When called from execute.f within water right loop
c               (icarry=0 and irgout=0)
c                - calculate compact demand and adjust
c                  to reflect water diverted in previous iteration.
c               When called from execute.f at end of water right loop
c               (icarry=0 and irgout=1)
c                - print monthly results
c               When called from execute.f at end of monthly loop
c               (icarry=1 and irgout=1)
c                - print annual results
c
c               When monthly (iday=0) avtemp = river
c               When daily (iday=1) avtemp = riverm/rimd (ave to date)
c
c               Variables (cfs):
c               avtemp          = For monthly model 
c                                 avtemp=river
c                                 For a daily model
c                                 avtemp=riverm/ridy (average to date)
c               irg             = 1 for Rio Grande
c                               = 2 for Conejos
c               icarry          = code to set carry over data
c                               = 0 during water right loop
c                               = 1 after water right loop is complete
c               ichk1           = 0 debug off; 1=on
c               ichk2           = For ichk1 = 1, 0= print both, 
c                                                1 print RG only
c                                                2 print Conejos Only
c               irgout          = 0 do not print (e.g. during wr loop)
c                               = 1 print end of wr loop (month) or year

c
c               imonsw(l2,mon)  = monthly constraint fr oprinp *.opr
c
c               
c               nrg             = # of times routine is called per time step

c               flowr(mon,nf)   = demand (need) for this month
c               flowrq(nf)      = remaining demand for this month (gets reduced each
c                                 time this routine operates via an iteration)
c                                 set in bomsec.for to forecast but reset to 0 in this routin
c               flowrq1         = demand satisfied so far this iteration
c
c               ifcd            = river location of demand (RG @ Labatos)
c
c               ntbl            = # of entries in delivery table
c               divo            = water right used
c               qdiv            = accounting
c
c               qrg             = Rio Grande index flow
c               qrgd            = Rio Grande delivery
c
c               qindex(i,irg)   = flow at index
c               qtodate         = sum of index for forecast season 4-9
c
c               qdel(i,irg)     = flow at compact station (RG - Conejos)
c               xtodate         = sum of delivery - forecast season 4-9
c
c               qcbp            = closed basin flow
c               qnative         = estimated native inflow (Nov - Dec)
c               qndsc           = flow Norton Drain South Channel
c               qpaper          = paper allocation
c               qcarry(irg)     = cumulative carry over (+=surplus
c                                 -=shortage) (acft)
c
c               qobl            = obligation = f(adj. index)
c               qneed           = Demand for remaining compact season 
c                                 (imon through and incluing Oct)
c                                 gets adjusted down based on actual 
c                                 diversions in a prior iteration
c               qneed1          = Same as above but not adjusted per 
c                                 iteration
c               curtail         = cutrailment % (calculated FYI, not used by code)
c
c               qdebt           = Year when payback of surplus occurs
c               idebt           = Same as above but an integer
c               qdebtx          = Initial condition of debt 
c                                 (set in bomsec)
c               qsurplus        = Maximum surplus per year (90k for RG,
c                                 60k for Conejos)
c               rgspill         = Historic spill information.  Integer               
c                                 is month spilled, decimal is % Co
c                                 (for prorating surplus)
c
c               aloifr          = demand after being adjusted for 
c                                 prior iterations
c               aloifr1         = maximum demand in any iteration               
c               actwrq          = actual diversion     
c               actwrq1         = maximum diversion in any iteration
c
c               demcon(imon,ifcd)=demand printed to *.xdd
c
c               iopdes(1,l2)    = Destination instream flow 
c                                 fr oprinp *.opr           
c
c               FOR RIO GRANDE (IRG=1)
c ____________________________________
c
c               is1=iopsou(1,l2)= Source (index flow station)
c                                 RG nr Labatos
c               is2=iopsou(3,l2)= Source (index flow station) 
c                                 Conejos nr La Sauses
c               is3=iopsou(8,l2)= Source (index flow station) 
c                                 Closed Basin to RG af/yr              
c               is4=iopsou(10,l2)=Source (index flow station) 
c                                 Norton Drain South af/yr
c
c               FOR CONEJOS (IRG=2)
c _________________________________
c
c               is1=iopsou(1,l2)= Source (index flow station)
c                                 Conejos at Magote
c               is2=iopsou(3,l2)= Source (index flow station) 
c                                 Los Pinos nr Ortiz
c               is3=iopsou(5,l2)= Source (index flow station) 
c                                 San Antonio at Ortiz
c               iopsou(8,l2)    = Source (index flow station) 
c                                 Closed Basin to RG af/yr              
c               iopsou(10,l2)   = Source (index flow station) 
c                                 Norton Drain South af/yr
c
c _________________________________________________________
c       Dimensions
c
      include 'common.inc'
c
      character cwhy*48
      dimension qnov(2), qdec(2), qneed1(2), aloifr1(2)
      dimension qrg(31), qrgd(31), qcod(31),   f(12), qsurplus(2)
      data qrg/
     1       0.,   50000.,  100000.,  150000.,  200000., 
     1  250000.,  300000.,  350000.,  400000.,  450000.,
     1  500000.,  550000.,  600000.,  650000.,  700000.,
     1  750000.,  800000.,  850000.,  900000.,  950000.,
     1 1000000., 1050000., 1100000., 1150000., 1200000.,
     1 1250000., 1300000., 1350000., 1400000., 1450000.,  1500000./
c
      data qrgd/
     1       0.,       0.,       0.,       0.,   60000., 
     1   65000.,   75000.,   86000.,   98000.,  112000.,
     1  127000.,  144000.,  162000.,  182000.,  204000.,
     1  229000.,  257000.,  292000.,  335000.,  380000.,
     1  430000.,  485000.,  540000.,  590000.,  640000.,
     1  690000.,  740000.,  790000.,  840000.,  840000.,  840000./
c
      data qcod/
     1       0.,       0.,       0.,   20000.,   45000., 
     1   75000.,  109000.,  147000.,  188000.,  232000.,
     1  278000.,  326000.,  376000.,  426000.,  476000.,
     1  476000.,  476000.,  476000.,  476000.,  476000.,
     1  476000.,  476000.,  476000.,  476000.,  476000.,
     1  476000.,  476000.,  476000.,  476000.,  476000.,  476000./
c
c _________________________________________________________
c
c               Step 1 Initialize
c
c               a. Miscellaneous control data                   
      iout=0
      if(ichk.eq.118) iout=1
c               ichk1           = 0 debug off; 1=on
c               ichk2           = For ichk1 = 1, 0= print both, 
c                                                1 print RG only
c                                                2 print Conejos Only

      ichk1 = 0
      ichk2 = 0
      if(ichk.eq.118) ichk1=1

      if(ichk1.eq.1) then
cx      WRITE(nlog,*) '  Rgrg.for iyr, mon, idy ', iyr, mon, idy
      endif

      nrg   = nrg+1
      iw    = iw

      nf    = iopdes(1,l2)
      ifcd  = ifrsta(nf)

      is1   = iopsou(1,l2)
      is2   = iopsou(3,l2)
      is3   = iopsou(5,l2)

      actwrq  = 0.0
      actwrq1 = qdiv(14,ifcd)
      small   = 0.00001
      flowrq1 = 0.0
      idebt=ifix(qdebt(irg))
c
c rrb 00/11/02; Need to save between iterations like qneed1             
      if(nrg.eq.1) then
        qneed1(irg) = 0.0
        aloifr1(irg) = 0.0
      endif
      ntbl=31
c
c rrb 00/06/31; Maximum annual surplus if 150,000 for State
      qsurplus(1) = 90000.
      qsurplus(2) = 60000.
c
c               b. Month imon = mon except for last call per year
      imon=mon
c
c rrb 01/11/14; Set for daily
      rimd=float(idy)
      if(icarry.eq.1) then 
        imon=12
        rimd=31.0
      endif
c
c               c. Flowrq, the cumulative delivery variable
c                  Note in Bomsec it is set to the forecast (a - #)
c                  from here forward it is cumulative delivery to 
c                  compact by Opr rule up to this iteration.
      flowrq(nf)=amax1(flowrq(nf),0.0)
c
c               d. Set units for daily model
      do i=1,12
        f(i)=factor* float(mthday(i))
      end do  
      fac=factor*float(mthday(imon))
c
c rrb 01/11/14; Set generic stream variable for daily or monthly model
      if(iday.eq.0) then
        do is=1,numsta
          avtemp(is)=river(is)
        end do
      else
        do is=1,numsta
          avtemp(is)=riverm(is)/rimd
        end do
      endif
c
c _________________________________________________________
c
c               Step X; Branch if not on this month
c
c rrb 2004/22/96; Allow monthly on/off switches
c rrb 2007/03/12; Correction this routine is called when
c		mon=13 to print annual data
      if(mon.le.12) then
        if(imonsw(l2,mon).eq.0) then
          iwhy=1
          cwhy='Monthly switch is off'
          goto 380
        endif
c
c ---------------------------------------------------------
c		For a daily model set demand for beginning of season
        if(iday.eq.1 .and. imonsw(l2,mon).gt.0) then
          if (idy.lt.imonsw(l2,mon)) then
            iwhy=1
            cwhy='Daily switch Off'
            goto 380
          endif  
        endif  
c
c ---------------------------------------------------------
c		For a daily model set demand for end of season
        if(iday.eq.1 .and. imonsw(l2,mon).lt.0) then
          if (idy.gt.iabs(imonsw(l2,mon))) then
            iwhy=1
            cwhy='Daily switch Off'
            goto 380
          endif  
        endif  
      endif
      
c
c _________________________________________________________      
c
c               Step 2; Set index flow for this month
c                  Note this value is saved here for use next month
c                  since the current month is part of the forecast.  
c                  Also, set qnov and qdec for annual printout.
      if(irg.eq.1) then
c
c rrb 01/11/14; Test average flow to date
c       qindex(imon,irg)=river(is1)
        qindex(imon,irg)=avtemp(is1)

c
c rrb 01/11/14; Test
c       write(nlog,*) 'Rgrg; ', iyr, mon, idy, -1,qindex(imon,irg)*fac
c    1    river(is1)*fac
c
c rrb 01/11/14; Test average flow to date  
c       if(imon.eq.11) qnov(irg)=river(is1)*fac
c       if(imon.eq.12) qdec(irg)=river(is1)*fac

        if(imon.eq.11) qnov(irg)=avtemp(is1)*fac
        if(imon.eq.12) qdec(irg)=avtemp(is1)*fac
      endif

      if(irg.eq.2) then
        if(imon.lt.4 .or. imon.gt.10) then
c
c rrb 01/11/14; Test average flow to date
c         qindex(imon,irg)= float(iopsou(2,l2)) * river(is1)
          qindex(imon,irg)= float(iopsou(2,l2)) * avtemp(is1)
        else
c
c rrb 01/11/14; Test average flow to date
c         qindex(imon,irg)= float(iopsou(2,l2)) * river(is1) +
c    1                      float(iopsou(4,l2)) * river(is2) +
c    1                      float(iopsou(6,l2)) * river(is3)
          qindex(imon,irg)= float(iopsou(2,l2)) * avtemp(is1) +
     1                      float(iopsou(4,l2)) * avtemp(is2) +
     1                      float(iopsou(6,l2)) * avtemp(is3)

c
c rrb 00/10/06; Error impacts annual at Conejos only moved below if
c         if(imon.eq.11) qnov(irg)=qindex(imon,irg)*fac
c         if(imon.eq.12) qdec(irg)=qindex(imon,irg)*fac
        endif
c
c rrb 00/10/06; Error impacts annual at Conejos only moved below if
        if(imon.eq.11) qnov(irg)=qindex(imon,irg)*fac
        if(imon.eq.12) qdec(irg)=qindex(imon,irg)*fac
      endif
c
c _________________________________________________________      
c
c               Step 3; Set index flow for future months
c
      do i=imon+1,12
        qindex(i,irg)=0.0
      end do
c
c               a. Months 10 - 12 to average values when no actual
c                  value exists.
c                  Note qindex(n,1)=rio grande & qindex(n,2)=conejos
      if(irg.eq.1) then
        if(imon.lt.10) qindex(10,irg) = 30000./f(10)
        if(imon.lt.11) qindex(11,irg) = 12500./f(11)
        if(imon.lt.12) qindex(12,irg) = 12500./f(12)
      endif

      if(irg.eq.2) then
        if(imon.lt.10) qindex(10,irg) = 8000./f(10)
        if(imon.lt.11) qindex(11,irg) = 4000./f(11)
        if(imon.lt.12) qindex(12,irg) = 4000./f(12)
      endif

c
c _________________________________________________________      
c
c
c               Step 4; Calculate index flow to date (to previous 
c                       month) for forecast period (Apr - Sept)
      qtodate = 0.0
      if(imon.ge.4) then
        i2=min0(imon-1, 9)
        do i=4,i2
          qtodate = qtodate + qindex(i,irg)*f(i)
        end do
        qtodate = qtodate/fac
      endif
c
c _________________________________________________________      
c
c               Step 5; Delivery to date
c
c ---------------------------------------------------------
c
c               a. Begin Actual delivery to date as RG @ Labatos - 
c                  Conejos @ La Sauses
c
      if(irg.eq.1) then
c
c rrb 01/11/14; Test
c       qdel(imon,irg) = float(iopsou(2,l2)) * river(ifcd) +
c    1                   float(iopsou(4,l2)) * river(is2)
        qdel(imon,irg) = float(iopsou(2,l2)) * avtemp(ifcd) +
     1                   float(iopsou(4,l2)) * avtemp(is2)

        qdel(imon,irg) = amax1(0.0, qdel(imon,irg))
c       x=float(iopsou(2,l2))*river(ifcd)*fac
c       y=float(iopsou(4,l2))*river(is2)*fac
c       write(nlog,*) '  Rgrg; imon, nrg, x,y, qdel(imon,irg)', imon,  
c    1                nrg, x, y, qdel(imon,irg)*fac
      endif
c
c ---------------------------------------------------------
c
c               b. Begin actual delivery to date as Conejos @ La Sauses
c
      if(irg.eq.2) then
c
c rrb 01/11/14; Test
c       qdel(imon,irg) = float(iopsou(2,l2))* river(ifcd)
        qdel(imon,irg) = float(iopsou(2,l2))* avtemp(ifcd)
        qdel(imon,irg) = amax1(0.0, qdel(imon,irg))
      endif
c
c ---------------------------------------------------------
c               c. Delivery for future months
c
      do i=imon+1,12
        qdel(i,irg)=0.0
      end do
c
c ---------------------------------------------------------
c
c               d. Delivery to date (xtodate) during 
c                  forecast season (4-9)
      xtodate = 0.0
      if(imon.ge.4) then
c
        do i=4,imon-1
          xtodate = xtodate + qdel(i,irg)*f(i)
c         write(nlog,*) '  Rg; i, qdel, xtodate ',i,qdel(i,irg),xtodate
        end do
        xtodate = xtodate/fac
      endif
c
c _________________________________________________________
c
c               Step 6; Set Closed basin and Norton drain                  
c                  qcbp, from closed basin is 1600/mo*remaining mon
c                  qndsc, Norton south channel is an annual adjustment
      qcbp    = (float(iopsou(8,l2))/12.0)/fac * (float(12-imon+1))
      qndsc   = float(iopsou(10,l2))/fac
      qcbp1   = (float(iopsou(8,l2)))/fac
c
c
c _________________________________________________________
c
c               Step 7; Set Native and Paper 
c                  qnative, is native delivery for Nov - Dec for all 
c                    months except 11 & 12 where it is adjusted      
c                  qpaper, is the 5,000 credit per river basin

      if(irg.eq.1) then
        qnative1= 27000./fac
        qnative = 27000./fac
        qpaper  = 5000./fac
      endif

      if(irg.eq.2) then
        qnative1= 6000./fac
        qnative = 6000./fac
        qpaper  = 5000./fac
      endif  
c
c rrb 00/03/16; Keep native constant regardless of month              
c     if(imon.eq.11) qnative = qnative - qdel(11,irg)
c     if(imon.eq.12) qnative = 0.0  
c
c _________________________________________________________
c
c               Step 7; Set Native and Paper 
c                  qnative, is native delivery for Nov - Dec for all 
c                    months except 11 & 12 where it is adjusted      
c                  qpaper, is the 5,000 credit per river basin
c
c
c _________________________________________________________
c
c               Step 8; Branch if not on this month
c
      if(imonsw(l2,imon).eq.0) goto 500
c
c
c _________________________________________________________
c
c               Step 9 Set Forecast = a negative demand
c                    Note it is set to flowr() because that does not
c                    get reduced over time
c
c rrb 00/06/09; Forecast is known if October
      if(mon.le.9) then
        fore1 = amax1(-1.0 * forecast(imon,nf), 0.0)
      else
        fore1 = qtodate
      endif
c     write(nlog,*) '  Rgrg; nf, fore1, ', nf, fore1*fac
c     write(nlog,*) '  rg; nf, fore1 = ', nf, fore1
c
c               Allow compact demands to be calculated outside the 
c                 forecast season (4-9)
c     if((fore1).lt.small) goto 100
c
c
c _________________________________________________________
c
c               Step 10 Calculate Adjusted Forecast
c                  = forecast - flow to date for forecast months (4-n)
c 
      adjfor = fore1 - qtodate
      adjfor = amax1(0.0, adjfor)
c
c _________________________________________________________
c
c               Step 11 Calculate Index Supply
c
c               a.adjusted supply + index to date + Oct - Dec estimate
c                 note the following calculation skips current month, 
c                 the one we are calculating a need for
      qsupply = adjfor*fac
      do i=1,12
        if(i.ne.imon) qsupply=qsupply+qindex(i,irg)*f(i)
      end do
      qsupply=qsupply/fac
c
c               b. adjust for current month if outside the 
c                  forecast season
      if(imon.le.3 .or. imon.ge.10)  qsupply=qsupply+qindex(imon,irg)
c
c  _________________________________________________________
c
c               Step 12; Calculate Annual Obligation (qobl)
c
      c=qsupply*fac
c
c rrb 01/12/26; Pass debug info
c     if(irg.eq.1) call interp(qrg,qrgd,c,qobl,ntbl)
c     if(irg.eq.2) call interp(qrg,qcod,c,qobl,ntbl)
      if(irg.eq.1) call interp(qrg,qrgd,c,qobl,ntbl,
     1                         3, l2, corid(l2))
      if(irg.eq.2) call interp(qrg,qcod,c,qobl,ntbl,
     1                         3, l2, corid(l2)) 
      qobl=qobl/fac
c
c  _________________________________________________________
c
c               Step 13 Calculate need (qneed) for forecast 
c                      season or if Oct for later this month
c
c                a. Need is obligation - closed basin - native - norton
c                   drain south - paper credit - surplus (qcarry)
c rrb 01/08/10; Revised treatment of surplus/shortage by year
c        write(nlog,*) '  Rgrg; idebt, iyr', idebt, iyr
      if(iyr.ge.idebt) then
        qneed=qobl - qcbp - qnative - qndsc-qpaper-qcarry(irg)/f(imon)
      else
        qneed=qobl - qcbp - qnative - qndsc-qpaper
      endif
      qneed=qneed*fac
c
c                b. Need is qneed - supply to date by RG - Conejos
      do i=1,imon-1
        qneed=qneed-qdel(i,irg)*f(i)
      end do
c
c rrb 00/11/02; Correction?
c     qneed1(irg)=amax1(0.0, qneed/fac)
      qneed1(irg)=amax1(0.0, qneed/fac, qneed1(irg))

c     if(imo.ge.10 .and. irg.eq.2) then
c       write(nlog,*) '  Rgrg; irg,nrg,qneed1 = ',irg,nrg,qneed1(irg), 
c    1    qneed1(irg)*fac
c     endif
c
c rrb 00/03/18; Adjust based on maximum debt
c rrb 01/08/10; Revise use of variable qdebt (idebt) to be year
c               month when payback occurs
c     qneed=amax1(0.0, (qneed-qdebt(irg))/f(imon)) 
      qneed=amax1(0.0, (qneed)/f(imon)) 

      c1 = float(10-imon+1)
      c1 = amax1(c1, 1.0)
c
c               nrg = # of times called this time step
      IF(nrg.eq.1) then
        flowrq1=0.0
      else
        flowrq1=(flowr(imon,nf) - flowrq(nf)) * c1
      endif

      qneed =qneed  - flowrq1
      qneed =amax1(0.0,qneed)
c
c  _________________________________________________________
c
c               Step 14; Adjust Annual Obligation (qobl) and need for
c                       a spill at Elephant Butte
c
c rrb 00/10/23; Adjust obligation if Elephant Butte Spilled

      nrgspil = ifix(rgspill(nf))
c     write(nlog,*) iyr, mon, idy, irg, rgspill(nf), nrgspil
      if(nrgspil.gt.0 .and. mon.ge.nrgspil) then
        qobl = 0.0
        qneed=0.0
        qneed1(irg)=0.0
c       write(nlog,*) iyr, mon, idy, irg, rgspill(nf), nrgspil
      endif

c
c _________________________________________________________
c
c               Step 15 Calculate Demand (aloifr)
c
c               a. Demand (aloifr) is the (qneed) per month  
c                  thru and including OCT per month or one if
c                  outside the forecast season

      aloifr=qneed/c1
c
c rrb 00/11/02; Store maximum demand               
      aloifr1(irg)=amax1(aloifr1(irg), aloifr)
      demcon(imon,ifcd) = aloifr1(irg)
      flowr(imon,nf)    = aloifr1(irg)
c
c               b. Store maximum demand for *.xdd printout
      IF(nrg.eq.1) then
        flowrq(nf)        = aloifr
c
c rrb 01/08/28; Store daily demand
        flowrd(idy,nf)    = aloifr
      else
      endif
c
c     if(imo.ge.10 .and. irg.eq.2) then         
c       write(nlog,*) '  Rgrg; irg,nrg,aloifr1= ',irg,nrg,aloifr1(irg),
c    1    aloifr1(irg)*fac
c     endif

c
c _________________________________________________________
c
c               Step 16 Check Demand
c
      if(aloifr.lt.small) goto 100
c
c _________________________________________________________
c
c               Step 17; Calculate Compact delivery (actwrq) to be
c               min flow in river (avail) & demand (aloifr)
c               
c               a. Compact delivery (actwrq) is the
c                  min flow in river (avail) since non consumptive
c                  and demand (aloifr)
      actwrq=amin1(avail(ifcd),aloifr)

      if(actwrq.lt.small) goto 100
c
c _________________________________________________________
c
c               Step 18; Update
c
c              a. Note avail  =  available flow,
c                      flowrq =  cumulative diversion, 
c                      divo   = diversion by an opr rule, and
c                      qdiv   = priority diversion 
c     avail1       = avail(ifcd)
      avail(ifcd)  = avail(ifcd)   - actwrq
      flowrq(nf)   = flowrq(nf)    - actwrq
      qdiv(14,ifcd)= qdiv(14,ifcd) + actwrq
      actwrq1      = qdiv(14,ifcd)
c
c
 100  divo(l2)     = divo(l2)      + actwrq
c                                                
c _________________________________________________________
c
c               Step 19 Prepare Output
c               

        qjanmar=0.0
        m1=min0(3,imon-1)
        do i=1,m1
          qjanmar=qjanmar+qdel(i,irg)*f(i)
        end do
        qjanmar=qjanmar/fac
c
c rrb 01/08/10; Revised treatment of surplus/shortage by year
        if(iyr.ge.idebt) then 
          tot = qjanmar + xtodate + qneed1(irg) + qcbp + qnative +
     1          qndsc   +  qpaper + qcarry(irg)/f(imon)
        else
          tot = qjanmar + xtodate + qneed1(irg) + qcbp + qnative +
     1          qndsc   +  qpaper
        endif

        curtail = 0.0
        c=adjfor+qindex(10,irg)
c
c rrb 00/03/18; Include maximum debt in need calculations 
c       IF(c.gt.small .and. idebt.eq.0) 
        IF(c.gt.small) curtail = qneed1(irg) / c * 100.0
        curtail=amin1(curtail, 100.0)
c
c _________________________________________________________
c
c               Step 20 Setup for Printing monthly Results to Binary
c
c rrb 00/03/06; Print after last iteration every month
        if(icarry.eq.0 .and. irgout.eq.1) then
c
c               Adjust surplus or shortage one time (not each 
c               iteration) in the month that it occurs
          if(nrgspil.gt.0 .and. mon.eq.nrgspil) then     
c
c               If in debt it is wiped out.
            if (qcarry(irg).le.0.1) then
              qcarry(irg) = 0.0
            else
c
c               If in surplus adjust by % Colorado
              c = rgspill(nf)-float(nrgspil)
              qcarry(irg)=qcarry(irg) * c
            endif
c
c rrb 01/01/12; Adjust total based on adjusted carryover 
            tot = qjanmar + xtodate + qneed1(irg) + qcbp + qnative +
     1        qndsc   +  qpaper + qcarry(irg)/f(imon)

          endif
c
c _________________________________________________________
c
c               Step 21; Print to Binary file
c
          irec=(iyr-iystr)*13*numrg + (imon-1)*numrg +irg
c
          write(66,rec=irec) fore1*fac,
     1    qindex(1,irg)*f(1)+qindex(2,irg)*f(2)+qindex(3,irg)*f(3),
     1    qtodate*fac, adjfor*fac,  qindex(10,irg)*f(10),
     1    qindex(11,irg)*f(11)+qindex(12,irg)*f(12),qsupply*fac,
     1    qobl*fac,
     1    qjanmar*fac,
     1    xtodate*fac, qneed1(irg)*fac,  qcbp*fac,    
     1    qnative*fac, qndsc*fac,        qpaper*fac, 
     1    qcarry(irg),
     1    tot*fac,     aloifr1(irg)*fac, actwrq1*fac, 
     1    qdel(imon,irg)*fac,   curtail, qdebt(irg), rgspill(nf)

c
c _________________________________________________________                                                
c
c               Step 22; Setup for Detailed Output
c
c         write(nlog,*) 'Rgrg After monthly print ', ichk1, ichk2
          if(ichk1.eq.1 .and. (ichk2.eq.irg .or. ichk2.eq.0)) then
c         if(imon.ge.10) then
c         write(nlog,*) 'Rgrg After monthly print, imon = ', imon 
c         if(iw.eq.1 .and. imon.eq.1) write(nlog,110)
c
c ---------------------------------------------------------
c               a. Title
          write(nlog,110)
c
c ---------------------------------------------------------
c               b. Supply
          write(nlog,120) irg, nrg, iyr,imon,idy,fore1,
     1      qindex(1,irg)+qindex(2,irg)+qindex(3,irg),
     1      qtodate, adjfor,  qindex(10,irg),
     1      qindex(11,irg)+qindex(12,irg),qsupply
c
c ---------------------------------------------------------
c               c. Delivery
          write(nlog,130) irg, nrg, iyr, imon, idy, qobl,
     1      qjanmar,
     1      xtodate, qneed, qcbp, qnative, qndsc, qpaper, qcarry(irg), 
     1      flowrq1, tot,   qdel(imon,irg), aloifr, actwrq,
     1      (flowr(imon,nf)-flowrq(nf)), curtail
          write(nlog,*) ' '
        endif



      endif
c                                                
c _________________________________________________________
c
c               Step 22; At end of year (icarry=1)
c               Set Carry over and print EOY stuff
c      
c rrb 00/03/06; 
c     if(icarry.eq.1 .and. imon.eq.12) then
      if(icarry.eq.1 .and. irgout.eq.1) then       
c
c ---------------------------------------------------------
c               a. Calculate actual delivery (qdel1)
        qdel1=0.0
        qindex1=0.0

        do i=1,12
          qindex1 = qindex1+qindex(i,irg)*f(i)
          qdel1   = qdel1+qdel(i,irg)*f(i)
        end do
        qindex1=qindex1/fac
        qdel1=qdel1/fac
c
c ---------------------------------------------------------
c               b. Calculate carryover
c rrb 00/06/09; Annual surplus is limited to 150,000 for state
        qcarry1 = qcarry(irg)
c
c rrb 00/06/09; Annual surplus is limited to 150,000 for state
c       qcarry(irg) = qcarry(irg) + (qdel1 - (qobl - qpaper))*fac
        qcarry(irg) = qcarry(irg) +
     1                amin1(qsurplus(irg), (qdel1-(qobl-qpaper))*fac)
c
c
c rrb 00/06/09; Cumulative surplus is not limited
c       if(qcarry(irg).gt.qsurplus(irg)) qcarry(irg) = qsurplus(irg)
c
c ---------------------------------------------------------
c               c. Adjust carryover based on discharge 
c                  to get wiped out if a spill is expected 
c                  (actual delivery (qdel1) > spill indicator (qdebtx)
c rrb 01/08/10; Revised use of variables qdebt and qdebtx
c       if(qdel1 .gt. qdebtx(irg)/fac)  qcarry(irg) = 0.0
c
c ---------------------------------------------------------
c               d. Adjust carryover based on spill data
        if(nrgspil.gt.0) then
c
c               If in debt it is wiped out.
          if (qcarry1.le.0.1) then
            qcarry(irg) = 0.0
          else
c
c               If in surplus from past year it
c               is adusted on the month it occurred (above)
            qcarry(irg) = qcarry1
          endif
        endif
c                                                
c _________________________________________________________
c
c               Step 23; Print output at end of year (icarry=1)
c
c               Detailed I/O for Checking
cx 380    continue
 
        if(ichk1.eq.1 .and. (ichk2.eq.irg .or. ichk2.eq.0)) then
          if(imon.ge.10) then
          write(nlog,*) '  Rgrg before annual print ; Month = ', imon
          write(nlog,140) irg, nrg, iyr,imon,idy,
     1      qcarry1, qindex1, qobl, qpaper, qobl-qpaper, qdel1,
     1      qcarry(irg)
          endif
        endif
c
c               Note annual curtailment is done in outrg where
c               annual totals are known
        curtail=-1.
c
c               Use imon not imon-1 since end of year but imon = 12
c       irec=(iyr-iystr)*13*numrg + (imon-1)*numrg + irg
        irec=(iyr-iystr)*13*numrg + (imon  )*numrg + irg
c
c       write(nlog,*) '  Rgrg EOY (iyr-iystr), numrg, (imon-1), irg'
c       write(nlog,*)  (iyr-iystr), numrg, (imon-1), irg 
c       write(nlog,*) '  Rgrg EOY, irec = ', irec    

        write(66,rec=irec) -1.,
     1    qindex(1,irg)*f(1)+qindex(2,irg)*f(2)+qindex(3,irg)*f(3),
     1    qtodate*fac,   0.,    qindex(10,irg)*f(10), 
     1    (qnov(irg)+qdec(irg)),          qindex1*fac,
     1    qobl*fac,   
     1    qjanmar*fac,
     1    xtodate*fac,   -1.,   qcbp1*fac,   
     1    qnative1*fac,         qndsc*fac,  qpaper*fac, 
     1    qcarry(irg),
     1    tot*fac,       -1.,   -1.,        qdel1*fac, 
     1    curtail,       qdebt(irg),        rgspill(nf)
        goto 500
      endif
c                                                
c ________________________________________________________
c
c               Step 24; Print Detailed Results to Log
c
 380  continue

      if(ichk1.eq.1 .and. (ichk2.eq.irg .or. ichk2.eq.0)) then
        if(imon.ge.10) then
c       if(iw.eq.1 .and. imon.eq.1) (99,110)

c               a. Title
        write(nlog,110)
c
c               b. Supply
        write(nlog,120) irg, nrg, iyr,imon,idy,fore1,
     1    qindex(1,irg)+qindex(2,irg)+qindex(3,irg),
     1    qtodate, adjfor,  qindex(10,irg),
     1    qindex(11,irg)+qindex(12,irg),qsupply
c
c               c. Deliver
        write(nlog,130) irg, nrg, iyr, imon, idy, qobl,
     1    qjanmar,
     1    xtodate, qneed, qcbp, qnative, qndsc, qpaper, qcarry(irg), 
     1    flowrq1, tot,   qdel(imon,irg), aloifr, actwrq,
     1    (flowr(imon,nf)-flowrq(nf)), curtail
        write(nlog,*) ' '
        endif
      endif
c
c ________________________________________________________
c
c               Formats
c
 110  format(/,' Rio Grande Compact',/,
     1    '    Where irg is 1 for Rio Grande and irg=2 for Conejos,',/
     1    '          * is an estimate for forecast season (Apr-Sept)',
     1             ' or actual thereafter, and',/
     1    '          Apr-Now is Apr to current month - 1 for',
     1             ' forecast season or Apr - Sept therafter',/
     1    '          AdjFor is adjusted forecast for forecast season',
     1             ' or zero therafter',/,
     1    '          Divert-1 is total diversion from previous', 
     1             ' iterations * months to end of forecast season',//
     1    ' Supply    Irg  Nrg   Yr   Mo  Day',
     1    '   Forcast   Jan-Mar   Apr-Now    AdjFor',
     1    '      Oct*  Nov-Dec*     Total',/
     1    ' Deliver   Irg  Nrg   Yr   Mo  Day',
     1    '     Oblig   Jan-Mar   Apr-Now    Needed',
     1    '       CBP  Nov-Dec*    Norton     Paper',
     1    '   Surplus  Divert-1     Total      Flow Demand/mo',
     1    '    Divert SumDivert % Curtail',/
     1    ' ________ ____ ____ ____ ____ ____',
     1    ' _________ _________ _________ _________',
     1    ' _________ _________ _________ _________',
     1    ' _________ _________ _________ _________ _________',
     1    ' _________ _________ _________')
 120  format(' Supply  ', 5i5, 20f10.3)
 130  format(' Deliver ', 5i5, 20f10.3)
 140  format(//,
     1    'Carryover  Irg  Nrg   Yr   Mo  Day',   
     1    ' Surplus-1 Adj Index     Oblig     Paper Net Oblig',
     1    '  Delivery   Surplus',/
     1    ' ________ ____ ____ ____ _____ ____',
     1    ' _________ _________ _________ _________ _________',
     1    ' _________ _________'/
     1       'Carryover', 5i5, 20f10.0)   
c
c ________________________________________________________
c
c               Step 25 Return
c 

 500  return
      end

