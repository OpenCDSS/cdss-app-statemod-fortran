c JMFlow - Type 54 operating rule,
c            It simulates the Baseflow and Enhanced Baseflow percents
c            for the Arkansas River at Las Animas gage that are used
c            by a John Martin Storage (type 53) operating rule
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
c
      SUBROUTINE JMFlow(IW,L2,ncallX,
     1             pctB, pctE, aveB, aveE, rday1, rday2)
c
c _________________________________________________________
c	Program Description
c
c  JMFlow; Type 54.
c		 It simulates the Baseflow and Enhanced Baseflow percents
c      for the Arkansas River at Las Animas gage that are used
c      by a John Martin Storage (type 53) operating rule
c
c_____________________________________________________________
c
c       Update History
c
c
c ---------------------------------------------------------
c rrb 2015/08/15; Runtime error initialization related to not saving
c                   local variables with Gfortran compilation moved
c                   from here (JMFlow) to execut
c 
c rrb 2020/04/26; Revise Winter Storage season to go 
c                 from 3/15 to 3/31.  Therefore:
c                 Step 9.8 is for 3/1 - 3/15 and
c                 Step 9.9 is for 3/16 to 3/31
c
c rrb 2019/06/17; For a monthly model, revised *.jmm to be the 
c                 Baseflow % and calculate the Enhanced % herein
c                 Also added a check that the monthly data ID is
c                 the same as the source 1 ID
c
c rrb 2018/08/18; Copied JmStore.for and revised
c   
c _________________________________________________________
c
c       Documentation
c	
c               IW         Global water right ID
c               L2         LOC. OF operation right  in opr RIGHT TABLE
c                          
c               ns         = iopsou(1,l2) Stream gage (Arkansas R at LAas Animas)
c               ns2        = iopsou(3,l2) Stream gage (Purgatoire R at Las Animas)

c               ndR        Destination reservoir ID
c               pct   		 Percent of gage to divert
c
c		            qres(18    From river by other
c
c_____________________________________________________________
c	      Dimensions
c	
      include 'common.inc'
      character cwhy*48, cdestyp*12, ccarry*3, cpuse*3, cstaid1*12,
     1          rec12*12, cTandC*3, cidRiv*12, cidBal*12,
     1          cresid1*12, cCallby*12, csour1*12, csour2*12
c
c
c _________________________________________________________
c
c               Step 1 Common Initialization
c
c
c rrb 2021/04/18; Compiler warning
      iexit=0
      if(iexit.gt.0) goto 500
      ccarry=' '
      cdestyp=' '
      cidbal=' '
      cidriv=' '
      cpuse=' '
      cresid1=' '
      cstaid1=' '
      ctandc=' '
      rec12=' '
c
c ---------------------------------------------------------
c
      cCallBy='JMFlow    '      

c
c ---------------------------------------------------------
c		       a. OutPut control
c		            ioutX = 0 No details
c		                    1 Details
c                       2 Std output each time called      
c                       3 Monthly Baseflow and Enhanced results
      iout=0
      ioutX=0
      ioutiw=0
c
c          b. Quick exit for checking      
cx      goto 500
      
      ncallX=ncallX+1

      
      if(ichk.eq.154) iout=2
      if(corid(l2).eq. ccall) ioutiw=iw
c
      if(ioutX.ge.1 .and. ncallx.eq.1 .and. idy.eq.1) then
        write(nlog,102) corid(l2)
 102    format(/, 72('_'),/ ' JMFlow; ID = ', a12)
      endif             
      
c
c ---------------------------------------------------------
c		       b. Factor
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif
c
c ---------------------------------------------------------
c		       c. Miscellaneous
      
      imcP=-1
      small = 0.001
      smalln=-1*small
      
      ishort=0                          
c
c ---------------------------------------------------------
c		       d. Detailed Output
      iwhy=0
      ilogic=0
      cwhy='NA'     
      csour1='NA' 
      csour2='NA'
cx      flow1=-1/fac  
c
c ---------------------------------------------------------
c rrb 2015/08/15; Runtime error initialization related to not saving
c                   local variables with Gfortran compilation moved
c                   from here (JMFlow) to execut
c          e. Initialize percentage and average once per year
c
cx       if(mon.eq.1 .and. idy.eq.1 .and. icallOP(l2).eq.0) then
cx        pctb=0.0
cx        pcte=0.0
cx        aveB=0.0
cx        aveE=0.0
cx        rday1=0.0
cx        rday2=0.0
cxc        write(nlog,*) '  JMFlow; initializing', mon, idy, icallop(l2)
cxc        write(nlog,*) '  JMFlow; iyr ', iyr, mon, idy, numsta
cx      endif
c
c ____________________________________________________
c          f. Limit to one call per time step
c rrb 2018/07/29 
      icallOP(l2)=icallOP(l2) + 1
c
      if(icallOP(l2).gt.1) then
        iwhy=1
        cwhy='Limited to one call per time step'
        goto 300
      endif 
       
                  
c _________________________________________________________
c		            Step 2; Get flow at stream gage (availG)
c     
      ns  = iopsou(1,l2)
      csour1=cstaid(ns)
      Flow1= avail(ns)   
      
c     write(nlog,*) '  JMFlow; iyr ', iyr, mon, idy, numsta      
cx      if(ioutX.eq.1) then
cx        if(mon.eq.2 .and. idy.eq.22) then
cx          do is=1,numsta
cx          write(nlog,*) '  JMFlow; for day 22 ', iyr, mon, iday, is,
cx     1                   cstaid(is), fac
cx          write(nlog,'(20f8.2)')
cx     1                   avail(is),  avail(is)*fac, avail(is)*fac*30,
cx     1                   flow1,      flow1*fac,     flow1*fac*30
cx          end do
cx        endif
cx      endif
c     
c _________________________________________________________
c
c		            Step 2.5; Adjust for year type
c
c              TEST data is in water year
      if(cyr1.eq.'  WYR') monx=mon+9
      if(cyr1.eq.'  IYR') monx=mon+10
 
      if(ioutX.eq.1) then
        write(nlog,*) ' ' 
        write(nlog,*) '  JmFlow;    mon monx  idy    pctB    pctE'
        write(nlog,'(a12,3i5, 2f8.0)') '   JmFlow;  ',
     1       mon, monx, idy, pctB, pctE
      endif
c      
c
c _________________________________________________________
c		            Step 3; Check for On/Off Switches      
c
c ---------------------------------------------------------
c		       Monthly ON/Off switch

      ioff=0
      if(imonsw(l2,mon).eq.0) then
        iwhy=1
        cwhy='Monthly switch Off'
        ioff=1
      endif  
c
c ---------------------------------------------------------
c		       Daily ON/Off switch start on day x
      if(idy.eq.1 .and. imonsw(l2,mon).gt.0) then
        if (idy.lt.imonsw(l2,mon)) then
          iwhy=1
          cwhy='Daily switch Off'
          ioff=1
        endif  
      endif  
c
c ---------------------------------------------------------
c		       Daily On/Off Switch end on day x
      if(idy.eq.1 .and. imonsw(l2,mon).lt.0) then
        if (idy.gt.iabs(imonsw(l2,mon))) then
          iwhy=1
          cwhy='Daily switch Off'
          ioff=1
        endif  
      endif  
c
c ---------------------------------------------------------
c               Step 4. Exit if system is off
      if(ioff.eq.1) then
        if(ioutX.eq.1) write(nlog,*) '  JMFlow; ioff, imonsw ',
     1     ioff, imonsw(l2,mon)
        ilogic=0
        pctb=0.0
        pcte=0.0
        if(iday.eq.0) then
          AveB=-1.0/fac
          AveE=-1.0/fac
          flow1=-1.0/fac
        else
          AveB=-1.0
          AveE=-1.0
          flow1=-1.0
        endif
        goto 300
      endif  
c _________________________________________________________
c		            Step 4x; monthly model
c rrb 2018/11/17; Add monthly model option
      if(iday.eq.0) then
c
c ---------------------------------------------------------
c rrb 2019/06/17 
c               Check monthly stream ID is same as the source 1 Id
c               Note oprinp.f checks both type 53 & 54 have same 
c               source 1 ID
        if(cjmID(1) .ne. ciopsoX(1,l2)) then
cx        write(nlog,*) ijm, cjmID(1), ciopsoX(1,l2)        
          write(nlog,310) ijm, cjmID(1), ciopsoX(1,l2)
          goto 9999
        endif     
c
c rrb 2019-06-17; Read only pctB and set pctE to be 100 - pctB when
c                 the value provided is for Baseflow is 0
cx      pctB=cjm(mon,1)
cx      pctE=cjm(mon,2)
cx
        pctB=cjm(mon,1)        
        if(pctB.gt.small) then
          pctE=amax1(100.0 - pctB, 0.0)
          cjm(mon,2) = pctE

          AveB=-1.0/fac
          AveE=-1.0/fac
c
c               Detailed monthly results        
          if(ioutX.eq.1 .or. ioutX.eq.3) then
            write(nlog,*) 
     1        '  JMFlow;  iyr  iyr  mon  idy',
     1        '   cjm_1   cjm_2    pctB    pctE'
            write(nlog,'(a10, 4i5, 20f8.0)')      
     1        '   JMFlow; ', iyr , iyr, mon, idy,
     1            cjm(mon,1), cjm(mon,2), pctB, pctE
          endif
c
c               Skip daily calculaitons          
          goto 300
        else
c
c               Print warning
          write(nlog,320) ijm, pctB
          goto 9999
        endif
         
c
c ---------------------------------------------------------
c               Endif for a monthly model               
      endif
      
     
c _________________________________________________________
c             Step 5; Set Winter Storage Season
      ibm0=iopdes(1,l2)
      ibd0=iopdes(2,l2)
      iem0=iopdes(3,l2)
      ied0=iopdes(4,l2)
c
c ---------------------------------------------------------
c            Check for years wrapping into next year
      if(iem0.lt.ibm0) iem0=iem0+12
c     
c _________________________________________________________
c             Step 6; Set 'Baseflow' period data
      ibm1=iopdes(5,l2)
      ibd1=iopdes(6,l2)
      iem1=iopdes(7,l2)
      ied1=iopdes(8,l2)
c     
c _________________________________________________________
c
c		            Step 7; Set 'Enhanced Baseflow' Period data
      ibm2=iopdes(9,l2)
      ibd2=iopdes(10,l2)
      iem2=iopdes(11,l2)
      ied2=iopdes(12,l2)
      
c     
c _________________________________________________________
c
c		            Step 8; Check months and days provided are 
C                       within current logic
      iprob=0
      if(iem0.lt.ibm0) iprob=1
      if(iem1.lt.ibm1) iprob=2
      if(iem2.lt.ibm2) iprob=3
      
      if(iem0.le.12) then
        if(iem1.lt.iem0) iprob=4
        if(iem2.lt.iem0) iprob=5
      endif
      
      if(iem0.eq.ibm0 .and. ied0.lt.ibd0) iprob=6
      if(iem1.eq.ibm1 .and. ied1.lt.ibd1) iprob=7
      if(iem2.eq.ibm2 .and. ied2.lt.ibd2) iprob=8
      
      if(iem0.eq.iem1 .and. ied0.lt.ied1) iprob=9
      if(iem0.eq.iem2 .and. ied0.lt.ied2) iprob=10
      
      if(cyr1.eq.'  CYR') iprob=11
     
      if(iprob.gt.0) then
        if(iem0.gt.12) iem0=iem0-12
        write(nlog,330) iprob,
     1    ibm0, ibd0, iem0, ied0, 
     1    ibm1, ibd1, iem1, ied1,
     1    ibm2, ibd2, iem2, ied2
     
        write(6,330) iprob, 
     1    ibm0, ibd0, iem0, ied0, 
     1    ibm1, ibd1, iem1, ied1,
     1    ibm2, ibd2, iem2, ied2
     
        goto 9999
      endif
c     
c _________________________________________________________
c
c		            Step 9; Adjust for year type
c
c              TEST data is in water year  
c rrb 2018/12/03: Move above for detailed output reporting    
cx      if(cyr1.eq.'  WYR') monx=mon+9
cx      if(cyr1.eq.'  IYR') monx=mon+10      
cx 
cx      if(ioutX.eq.1) then
cx        write(nlog,*) ' ' 
cx        write(nlog,*) '  JmFlow;    mon monx  idy pctB pctE'
cx        write(nlog,'(a12,3i5, 2f8.0)') '  JmFlow;   ',
cx     1       mon, monx, idy, pctB, pctE
cx      endif
      
     
      
c
c ---------------------------------------------------------
c               9.1 Set values for times less than the start      
      if(monx.lt.ibm0 .or. monx.lt.ibm1) then
        ilogic=1
        pctb=0.0
        pcte=0.0
        goto 300
      endif
c
c ---------------------------------------------------------
c               9.2 Set values for first month (11/1) and days less 
c                  than the start of averaging for conservation (11/8) 
c                  (11/1 - 11/7)     
      if(monx.eq.ibm0 .and. idy.lt.ibd1) then
        ilogic=2
        pctb=100.0
        pcte=0.0
        goto 300
      endif
c
c ---------------------------------------------------------
c               9.3 Calculate baseflow average for first period and
c                   Set percent for this time period and
c                   (11/8 - 11/14)
c                 Count on System to initialize base and idyb to zero 
c                   and store them locally
cx      if(imb1.ne.ime1) goto 9999
cx
      if(monx.ge.ibm1 .and. idy.ge.ibd1) then
        if(monx.le.iem1 .and. idy.le.ied1) then
          ilogic=3
          AveB=AveB+flow1
          rday1=rday1+1
          
          if(idy.eq.ied1) AveB = AveB/rday1
                 
          pctb=100.0
          pcte=0.0
          goto 300
        endif
      endif      
c
c ---------------------------------------------------------
c               9.4 Find days gt period 1 and lt than period 2
c                 11/15 to 11/21.  
c                 Also set ropdes(l2,3 & 4) to a flow 
c                 time when AveB is unknown
c
c                 Count on System to initialize
c                 base and idyb to zero and 
c                 store them locally
      if(monx.ge.ibm1 .and. idy.gt.ibd1) then
        if(monx.le.iem2 .and. idy.lt.ibd2) then     
          ilogic=4
c
c rrb 2018/2/03; Correction
          if(flow1.le.AveB) then            
            pctb=100.0
            pcte=0.0
          else
            pctb=AveB/Flow1*100
            pcte=100 - pctb
          endif          
          goto 300
        endif
      endif
c
c ---------------------------------------------------------
c               9.5 Find days to calculate period 2
c                 11/22 to 11/28.  
c                 Also set ropdes(l2,3) & (l2,4) to a flow 
c                 time when AveB is unknown
c
c                 Count on System to initialize
c                 base and idyb to zero and 
c                 store them locally
      if(monx.ge.ibm2 .and. idy.ge.ibd2) then
        if(monx.le.iem2 .and. idy.le.ied2) then
          ilogic=5
c
c ----------------------------------------------------
c         TEST logic by adjusting enhanced flow * factor
cx        AveE=AveE+flow1*2
          facX=1.0
          AveE=AveE+flow1*facX
          rday2=rday2+1        
c
c rrb 2018/2/03; Correction
cx          pctb=100.0
cx          pcte=0.0
 
          if(flow1.le.AveB) then            
            pctb=100.0
            pcte=0.0
          else
            pctb=AveB/Flow1*100
            pcte=100.0 - pctb
          endif          
c        
          if(idy.eq.ied2) then
            AveE = AveE/rday2
cx            write(nlog,'(a24, 2f8.0)')
cx     1       '  JMFlow; AveB, AveE', AveB*fac, AveE*fac
cx            
            if(AveB.ge.AveE) then
              pctb=100.0
              pcte=0.0
            else        
              pctb=AveB/AveE*100.
              pcte=100.0-pctb
            endif
          endif
cx            write(nlog,'(a30, 4f8.0)')
cx     1       '  JMFlow; AveB, AveE, pctb, pcte',
cx     1         AveB*fac, AveE*fac, pctb, pcte
          goto 300
        endif
      endif
c
c ---------------------------------------------------------
c               9.6 Set rest of days in November using the last pctb
c                  11/29 to 11/30
      if(monx.eq.iem2 .and. idy.gt.ied2) then
        ilogic=6
      
        pctb=pctb
        pcte=100.0 - pctb
        goto 300
      endif
c
c
c
c ---------------------------------------------------------
c               9.7 Set days in next Months
c                  Dec - Feb
      if(monx.gt.ibm0 .and. monx.lt.iem0) then
        ilogic=7
        pctb=pctb
        pcte=100.0 - pctb
        goto 300
      endif
c
c ---------------------------------------------------------
c               9.8 Set days in last Month 
c                  3/1 - 3/15
c rrb 2020/04/26; Allow Winter Storage season to go to 3/31
c                 therefor set end to 3/15 (value not read in)
cx    if(monx.eq.iem0 .and. idy.le.ied0) then
      if(monx.eq.iem0 .and. idy.le.15) then
        ilogic=8
        pctb=pctb
        pcte=100.0 - pctb
        goto 300
      endif
c
c ---------------------------------------------------------
c               9.9 Set days in last Month (3/15 - 3/31)
c                  3/16 - 3/31
c rrb 2020/04/26; Allow Winter Storage season to go to 3/31
c                 therefor set end to 3/15 (value not read in)
cx    if(monx.eq.iem0 .and. idy.gt.ied0) then
cx        ilogic=9
cx        pctb=0.0
      if(monx.eq.iem0 .and. idy.gt.15) then
        ilogic=9
        pctb=100.0
        pcte=0.0
        goto 300
      endif
c
c
c ---------------------------------------------------------
c               9.10 Set days off in any Months past the last
c                    even if not turned off with monthly switch
      if(monx.gt.iem0) then
        ilogic=10      
        pctb=0.0
        pcte=0.0
        goto 300
      endif
        
c     
c _________________________________________________________
c      
c                Step 10; Goto point if a:
c                  - Monthly model or
c                  - Monthly on/off switches are off
 300  continue
c     
c _________________________________________________________
c      
c                Step 11; Set ropdes for Arkansas River data 
c                         used or printed in JMStore (type 53)
c 
      ropdes(l2,1) = pctb
      ropdes(l2,2) = pcte
      ropdes(l2,3) = aveB
      ropdes(l2,4) = aveE
c _________________________________________________________   
c  rrb 2018/12/15; Store Purgatoire flow in ropdes(l2,5)
c                Step 12; Set ropdes for Purgatoire River date
c                         used or printed in JMStore type 53)
      ns2 = iopsou(3,l2)
      rs2 = float(iopsou(4,l2))
      if(ns2.gt.0) then
        csour2=cstaid(ns2)
        Flow2= avail(ns2)  
        ropdes(l2,5) = Flow2*rs2/100. 
       
        if(ioutX.eq.1) then
        write(nlog,*) 
     1    '  JMFlow;  iyr  iyr  mon  idy  ns2 csour2      ',
     1    '     rs2   flow2  ropdes'
        write(nlog,'(a10, 5i5, 1x, a12, 20f8.0)')      
     1    '   JMFlow; ', iyr , iyr, mon, idy, ns2, csour2,
     1      rs2,flow2*fac, ropdes(l2,5)*fac
        endif
      endif  
c _________________________________________________________
c      
c                Step 13; Set divo equal to the baseflow % to print
c                         in the operating rule file (*.xop)
      divo(l2)=pctb/fac
      
cx      write(nlog,*) '  JMFlow ; pctb, fac, pctb', pctb, fac,pctb/fac
c _________________________________________________________
c
c                Step 14; Detailed output if requested

      if((iout.ge.2 .and. iw.eq.ioutiw) .or. ioutX.eq.2) then
c
c ---------------------------------------------------------
c		            a. Header for this time step
c
        if(ncallX.eq.1 .and. idy.eq.1)then
          if(iday.eq.0 .and. mon.eq.1) then
            write(nlog,*) ncallx, idy, iday, mon
            write(nlog,270) corid(l2), 'Monthly ', 'AF/M'
          endif
          if(iday.eq.1) then
            write(nlog,270) corid(l2), 'Daily   ',  'cfs '
           endif
        else
c          write(nlog,*) ' '
        endif  
c
c ---------------------------------------------------------
c		            b. Detailed output
c
        con=flow1*pctb/100.
        oth=flow1*pcte/100.
        
        if(iday.eq.0) then
          write(nlog,280) csour1, csour2, iyrmo(mon),xmonam(mon), 
     1     idy, mon, monx, ilogic,
     1     flow1*fac, AveB*fac, AveE*fac, rday1, rday2, 
     1     pctb, pcte, con*fac, oth*fac, Flow2*fac, iwhy, cwhy
        else
          write(nlog,280) csour1, csour2, iyrmo(mon),xmonam(mon), 
     1     idy, mon, monx, ilogic,
     1     flow1, AveB, AveE, rday1, rday2, 
     1     pctb, pcte, con, oth, flow2, iwhy, cwhy
        endif
      endif  
      
cx280  FORMAT(a12, i5,1x,a4,4i5, 7f8.0, i5,1x, a48)
           
c
c ---------------------------------------------------------
c 						  Step 15; Return  
c 
cx500 RETURN
  500 Continue
  
c_______________________________________________________________________
c rrb 2020/06/03; Check avail
c		            Check Avail for Roundoff
cx      write(nlog,*) ' JmFlow; Calling RoundOf'
        call roundof(avail, numsta, 9, 15, nbug)
      RETURN
  
c
c _________________________________________________________
c
c                Formats
c
  270  format(/, 
     1  ' JMFlow (Type 53); Operation Right ID = ', a12,/
     1  ' ',a8, ' Model - Units are ', a4,/
     1  ' Source_1    Source_2     iyr  mon  idy  mon monx ilog',
     1  '   Flow1    Base   Ebase   rday1   rday2    pctb    pcte',
     1  '  ConSto  OthSto   Flow2 iwhy cwhy',/
     1  '____________ ___________ ____ ____ ____ ____ ____ ____',
     1  10(' _______'),' ____', 1x, 48('_'))
     
  280  FORMAT(a12,1x,a12, i5,1x,a4,4i5, 10f8.0, i5,1x, a48)
c
c _________________________________________________________
c
c              Error warnings
c
 9999  continue
c
 310   format('  JMFlow; Monthly mode with code (ijm)= ',i5,/
     1        10x, 'The John Martin Flow (*.jmm) ID        = ', a12,/
     1        10x, 'does not equal the type 54 source 1 ID = ', a12,/
     1        10x, 'Recommend you revise the input data')

 320   format('  JMFlow; Monthly mode with code (ijm)= ',i5,/
     1        10x, 'The Baseflow percent specified must be '
     1             ' greater than zero',/
     1        10x, 'But the value provided      = ',f8.2,/
     1        10x, 'Recommend you confirm the Monthly JMartin',
     1             'file (*.jmm) has been provided or if the baseflow',/
     1        10x, '% is less than or equal to zero in a month',
     1             'when the operating rule monthly switch is on')
 
 330   format('  JMFlow; Daily mode with code = ',i5,/
     1        10x, 'This routine requires a WYR or IYR ',
     1        'year type and ',/
     1        10x, 'assumes the starting and ending period of ',
     1             'operation occur in the same year ',/
     1        10x, 'and the starting and ending baseflow and ',
     1             'enhanced baseflow averaging periods increase',/ 
     1        10x, 'For example if the beginning month',
     1             ' and ending month are the same',/
     1        10x, 'the beginning day must be less than',
     1             ' the ending day',/
     1        10x, 'Period of operation     = ',2i3,' - ', 2i3,/
     1        10x, 'Baseflow Average Period = ',2i3,' - ', 2i3,/
     1        10x, 'Enhanced Average Period = ',2i3,' - ', 2i3,/     
     1        10x, 'Recommend you revise operating rule type 54 input')
     
cx     
cx        write(nlog,280) csour1, 
cx     1     iyrmo(mon),xmonam(mon), idy, mon, monx, ilogic,
cx     1     flow1*fac, AveB*fac, AveE*fac, rday1, rday2, 
cx     1     pctb, pcte, iwhy, cwhy
cx     
      
c    
      write(6,340) 
      write(nlog,350) 
      call flush(6)
 340  format('    Stopped in JMFlow',/,
     1       '    See the *.log file')
 350  format('    Stopped in JMFlow')
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop 
      END
