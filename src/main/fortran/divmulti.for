c divmulti - Type 46 operating rule.
c            It simulates multiple ownership.
c            Currently transfers water from an admin plan to one or more admin plans.
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
      SUBROUTINE DivMulti(IW,L2,ncallX)
c
c
c _________________________________________________________
c	Program Description
c
c    DivMulti; Type 46.
c		 It transfer water from:
c      1) an Admin Plan (type 11) or Changed Water Right Plan (type 13)
c         to one or more other plan types or 
c      2) a WWSP-Supply plan (type 14) to one 
c         or more WWSP-User plans (type 15).
c
c    For a WWSP-Supply (14) plan, it has special code to check
c      if a divert to irrigate that is part of the total supply
c      exceeds the allocation percentage.  see Steps 4a & 4b.
c
c		 Source 1 is an Admin Plan (type 11) or WWSP Supply Plan (type 14)
c
c
c_____________________________________________________________
c
c       Update History
c
c
c rrb 2021/04/18; Compiler warning
c  
c rrb 2020/05/31; Revise the correction to psuply (ndP) 
c                 to emphasize this only occurs for a plan
c                 type 14(a WWSP plan).  Specifically psuplyX can only
c                 be > 0 for a type 14 WWSP Plan
c rrb 2020/05/29; Revise to correct a problem identified on SPlatte
c
c rrb 2019/08/25; Revised psuplyD=psupDD(nDp) not psuplyD=psuply(ndP)
c                    to insure its the amount taken to direct diversion
c                    in DivCarl.for and limit to only occur for a 
c                    WWSP-user (type 15) near step 5d
c rrb 2019/07/14; 1. Move initialization from step 3 to step 1.5
c                    to facilitate detailed output
c                 2. Correction for type 14 and 15 (again) in step 4
c                    (nr 364)
c                 3. Added psuplyX to store cumulative divert to 
c                    irrigate in step 4a for use in step 9 (see below)
c                 4. Detailed output when direct diversion > allocation
c                    at bottom of step 4a when ioutW=1.
c                 5. Corrected pctX % when direct divert > allocation
c                    in step 4b
c                 6. Revised test for WWSP in step 5b
c                 7. Added step 9 to reset WWSP-Supply for direct 
c                    diversion and note that the WWSP-User is reset
c                    in BomSec.f after the split has occurred and
c                    results are printed
c                 8. Initialized psuplyDT to zero to correct a problem
c                    in years 2-n
c
c rrb 2019/04/20; Revised to recognize a WWSP Supply plan is a
c                   type 14 and a WWSP User Plan is a type 15
c
c rrb 2019/04/10; Revised to work for all plans (not just a WWSP Supply
c                   (type 14) plan
c rrb 2018/10/11; Revised to include divoWWX(k)that indicates a 
c                   WWSP Supply plan has been allocated
c rrb 2018/10/11; Revised to reallocate supplies if a user has
c                   taken more direct flow than their percent allotment
c rrb 2018/10/07; a.	It ties a plan to an operating rule for Plan Reporting
c 
c rrb 2018/08/30; If the source is a WWSP Supply plan (type 14) adjust
c                   to allow destination % to include direct irrigation
c                   to a WWSP User Plan (type 15)
c                  
c rrb 2006/04/27; Copied WelAugP
c		  Revised accordingly
c               
c _________________________________________________________
c
c       Documentation
c	
c       IW              Global water right ID
c       L2              LOC. OF operation right  in opr RIGHT TABLE
c       nwR             source water right
c      
c       alocfs          Source 1 plan supply = psuply(nsP)                  
c       divact          total well pumping
c                                             
c       iscd            River location of source plan 
c       	           	 (Iscd=ipsta(nsp)
c       ndes            Number of destinations(step 4a)
c                                            
c       ndnnod(iscd)    Number of downstream nodes from source
c       ndns            Number of downstream nodes from well location
c
c       nsP             Pointer to supply plan
c       ndP             Pointer to destination plan
c
c       PctX            Percent used
c       pct1            Original percent for a WWSP Plan
c       pct2            Adjusted percent for a WWSP Plan if the 
c                         allocation is exceeded  because of a 
c                         direct diversion
c
c       psupDD          Set in DivcarL. Amount taken by direct diversion
c       psuplyD         initialized to zero and for a WWSP-Supply (14)
c                         the amount taken  by direct diversion   
c       psuplyX         Total of all direct diversions (psuplyD) for use
c                         in Step 9.
c       psuplyDT        Total of direct diversion > their allocation
c
c       ropdes(l2,n)    Percent allocation to a destination
c
c                       
c       qdiv(18  	      Carrier passing thru a structure 
c       qdiv(24,iscd) 	Pumping (diversion) by a well to a user at iscd
c       qdiv(25,iscd) 	Depletion (From River by Well) at river ID iscd
c	      qdiv(28        	Carried, Exchange or Bypass (column 11)
c                      	Source is a reuse or Admin Plan
c		    qdiv(30         From River from a Res or Reuse Plan 
c                        to a T&C or Aug Plan. Note non consumptive
c       qdiv(31        	From River by Exc or Plan
c			                  tracked at the destination.
c       qdiv(38         Carried water not used in any calculations
c                       to report River Divert
c	
c       small           a small value for roundoff (0.0) concerns
c
c       divoww(np,no)   It ties a plan (np) to an operating rule (no)
c                       for detailed WWSP Plan Reporting
c_____________________________________________________________
c	Dimensions
c	
      include 'common.inc'
      dimension pctX(40), pct1(40), pct2(40), inegP(40)
      character cwhy*48, cdestyp*12, ccarry*3, cpuse*3, cstaid1*12,
     1          rec12*12, cTandC*3, cidRiv*12, cidBal*12,
     1          cresid1*12, subtypX*8
c
c
c _________________________________________________________
c
c               Step 1 Common Initialization
c
c
c rrb 2021/04/18; Compiler warning
      cidbal=' '
      cidriv=' '
      cresid1=' '
      rec12=' '
      psuplyd=0.0
      
c		         iout = 0 No details
c		                1 Details
c                   2 Summary      
c		                3 Well Augmentation details
c		                4 Sum   
c            ioutP = 1 Print details of plan allocation
c            ioutX = 1 Print details if allocation exceeds supply & is 
c                      getting adjusted
c            ioutW = 1 Print warning if allocation exceeds the supply
c                      and is getting adjusted
c
c ---------------------------------------------------------
c		a. OutPut control
      subtypX='divmulti'
      
      iout=0
      ioutP=0
      ioutiw=0    
      ioutX=0
      ioutW=1
      
      if(ichk.eq.146) iout=2
      if(corid(l2).eq. ccall) ioutiw=iw
c
c rrb 2018/04/08; Reduce output to *.log      
cx      if(iout.ge.1 .and. ncallx.eq.0) then
cx        write(nlog,102) corid(l2)
cx 102    format(/, 72('_'),/ '  DivMulti; ID = ', a12)
cx      endif             
      
c     write(Nlog,*) ' DivMulti; ncallx, iout, ioutiw, iw', 
c    1                          ncallx, iout, ioutiw, iw
c
c ---------------------------------------------------------
c		b. Factor
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif
c
c ---------------------------------------------------------
c		c. Miscellaneous
      
      imcP=-1
      divact= 0.0
      divactT=0.0
      divalo = 0.0
      small = 0.001
      smalln=-1*small
c
c rrb 2018/10/14; Set variable to print a warning in *.xww
c rrb 2018/10/21; Set in Bomsec with divoWW(i,l2)
cx      divoWWX(l2)=0.0    
c      
c 
c rrb 00/12/26; Set variable efficiency (1=on, controlled by ieffmax)
      ieff2 =1              
c
      ishort=0
                       
c
c ---------------------------------------------------------
c               d. Check Avail array
      call chekava(19, maxsta, numsta, avail, subtypX)
c
c ---------------------------------------------------------
c               e. Initialize temp array to store current 
c		               depletions and returns
      do is=1,numsta
        avtemp(is)=0.0
      end do
      
c
c ---------------------------------------------------------
c		f. Detailed Output
      iwhy=0
      cwhy='NA'
      cdestyp='NA'
      ccarry='No'
      cpuse='No'
      cTandC='No'
      cstaid1='NA'
c
c _________________________________________________________
c
c rrb 2019/07/14-1; Move here from step 3 to facilitate detailed output    

c		            Step 1.5; Set Source 1 an accounting plan (type 11), 
c                         changed water right plan (type 13) or a 
c                         WWSP-Supply plan (type 14) plan 
      nsP  =Iopsou(1,L2)
      Iscd=ipsta(nsp)
      ndns=NDNNOD(IsCD)
      imcd=ipsta(nsP)
      cstaid1=pid(nsP)
c      
      alocfs=psuply(nsP)
c
c rrb 2019/07/14-1; Check     
      if(ioutP.eq.1 .or. ioutX.eq.1) then
        write(nlog,*) ' '
        write(nlog,*) ' -----------------------------------'
        write(nlog,*) '  DivMulti; ', 
     1    iyrmo(mon), xmonam(mon), nsp, alocfs*fac
      endif
c
c _________________________________________________________
c		            Step 2; On/Off Switch      
c
c rrb 06/01/18; Allow daily on/off switch
      if(imonsw(l2,mon).eq.0) then
        iwhy=1
        cwhy='Monthly switch Off'
        goto 260
      endif  
c
c ---------------------------------------------------------
c		Daily ON/Off switch start on day x
      if(iday.eq.1 .and. imonsw(l2,mon).gt.0) then
        if (idy.lt.imonsw(l2,mon)) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 260
        endif  
      endif  
c
c ---------------------------------------------------------
c		Daily On/Off Switch end on day x
      if(iday.eq.1 .and. imonsw(l2,mon).lt.0) then
        if (idy.gt.iabs(imonsw(l2,mon))) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 260
        endif  
      endif  
c
c _________________________________________________________
c
c		            Step 3; Set Source 1 a WWSP Supply (type 14) plan, 
c                       set SUPPLY (alocfs) 
c 
c rrb 2019/07/14-1; Move above for detailed reports
cx      nsP  =Iopsou(1,L2)
cx      Iscd=ipsta(nsp)
cx      ndns=NDNNOD(IsCD)
cx      imcd=ipsta(nsP)
cx      cstaid1=pid(nsP)
cxc      
cx      alocfs=psuply(nsP)
cxc
cxc rrb 2019/07/14; Test     
cx      if(ioutP.eq.1 .or. ioutX.eq.1) then
cx        write(nlog,*) '  DivMulti; ', 
cx     1    iyrmo(mon), xmonam(mon), nsp, alocfs*fac
cx      endif
c
c ---------------------------------------------------------
c		            a. Exit if the supply (nsP) plan is off
      if(pon(nsP).eq.0) then
        iwhy=2
        cwhy='Source Plan is off'
        goto 260
      endif  
c
c ---------------------------------------------------------
c		            b. Exit if supply is zero
      if(alocfs.lt.small) then
        iwhy=3
        cwhy='Source Plan = zero'
        goto 260
      endif
c _________________________________________________________
c rrb 2018/10/11
c *********************************************************
c               Step 4a; For a WWSP User Plan destination
c                        do a preliminary allocation
c                        to see if their direct diversions exceeded
c                        their percent allotment
      cdestyp='Plan     '
c
      ndes=int(oprlimit(l2))
c
c rrb Revised to make the destination a WWSP User plan (type 15)        
cx    if(iplntyp(nsP).eq.14) then
c
c rrb 2019/07/14; Check
      if(ioutP.eq.1) then
        write(nlog,*) ' '
        write(nlog,*) '  DivMulti_0, nsp, iplntyp(nsP)', 
     1    nsp, iplntyp(nsP)
      endif
c
c ---------------------------------------------------------
c               Step 4a1: Confirm the source is a WWSP-Supply (type 14) 
c rrb 2019/07/14-2; Correction; The source should be a WWSP-Supply (14)
c                 plan and move initialization outside the plan type
c                 check to allow code to work with other plan types
c                  
cx    if(iplntyp(nsP).eq.15) then
cx      
cx      divactT=0.0
cx      psuplyD=0.0     
cx      psuplyX=0.0
cx      pctPT = 0.0
cx      inegPT=0
cx      
cx      do n=1,maxopr2 
cx        inegP(n)=0
cx        pct1(n)=0.0
cx        pct2(n)=0.0
cx        pctX(n)=0.0
cx      end do
        
      divactT=0.0
      psuplyD=0.0     
      psuplyX=0.0
      pctPT = 0.0
      inegPT=0
c
c rrb 2019/07/14-8; Correction initialize psuplyDT
      psuplyDT=0.0
       
      do n=1,maxopr2 
        inegP(n)=0
        pct1(n)=0.0
        pct2(n)=0.0
        pctX(n)=0.0
      end do
        
c
c
c
c ---------------------------------------------------------
c               Step 4a2; Loop to see if any destination accounts (ndP)
c                         divert more to irrigate than their % allocation 
c
c               pct1()   = Original percentage
c               pct2()   = Revised percentage for users being adjusted
c               pctPT    = Total percentage for users that do not take
c                            more than their allotment as direct irrig.
c               psupDD  = Supply by direct diversion in DivCarl.for
c               psuplyD = psuply(ndP) supply by direct diversion   
c               psuplyX = total of psuplyD (all direct diversions)
c               psuplyDT = Total of diversions greater than their 
c                            allotment
      if(iplntyp(nsP).eq.14) then
        n1=0
        n2=0        
        do n=1,ndes     
          
          
          n1=n2+1
          n2=n1+1
        
          ndP  =Iopdes(n1,L2)
          pctX(n2)=ropdes(l2,n2)/100.0 
          pct1(n2)=pctX(n2) 
          pct2(n2)=pctX(n2)
                    
          pctPT=pctPT+pctX(n2) 
c
c rrb 2019/08/25; Replace psuply(ndP) with psupDD(nd) set in 
c                 DivCarL.for diversion to irrigate by a 
c                 WWSP user (ndP)         
cx        psuplyD=psuply(ndP)
          psuplyD=psupDD(ndP)/fac  
c
c rrb 2019/07/14-3; Correction set psuplyX to be the sum for all
c                 direct diversions for use in year 2-n operations
          psuplyX=psuplyX+psuplyD   
          
          divactX=alocfs*pctX(n2)
          divact1=alocfs*pctX(n2)-psuplyD
          
          if(ioutX.eq.1) then
            write(nlog,*) 
     1        '  DivMulti;   ndp    n   n1   n2 psuplyD  psuply'
            write(nlog,'(a12,4i5,20f8.0)')
     1        '  DivMulti; ',ndp, n, n1, n2, psuplyD*fac, 
     1           psuply(ndp)*fac
          endif
          
c
c ---------------------------------------------------------
c               Step 4a3; Direct irrigation exceeds the allocation 
c                                
          if(divact1.lt.smalln) then
            inegPT=inegPT+1
            inegP(n2)=1
            pctPT=pctPT-pctX(n2)
            psuplyDT=psuplyDT+divact1
          endif
c
c ---------------------------------------------------------
c rrb 2019/07/14-4;
c               Step 4a4; Print warning to screen and log
          if(ioutX.eq.1 .or. ioutW.eq.1) then
            if(divact1.lt.smalln) then

cx            write(nlog,*) ' DivMulti_X', im, iyrmo(mon), xmonam(mon)
              write(nlog,340) iyrmo(mon), xmonam(mon), pid(ndP), 
     1          alocfs*fac, pctX(n2)*100, alocfs*pctX(n2)*fac,
     1          psuplyD*fac, divact1*fac  
             
              write(6,*) im, iyrmo(mon), xmonam(mon)
              write(6,340) iyrmo(mon), xmonam(mon), pid(ndP), 
     1          alocfs*fac, pctX(n2)*100, alocfs*pctX(n2)*fac, 
     1          psuplyD*fac, divact1*fac 
            endif
          endif         
          
        end do
c
c _________________________________________________________
c rrb 2018/10/11
c               Step 4b1; For a WWSP User Plan (type 15) with divert
c                         to irrigate greater than their allotment 
c                         (over allocation)
        if(ioutP.eq.1) then
          write(nlog,*) '  DivMulti_2; l2, inegPT, divoWWX(l2)'
          write(nlog,*) '  DivMulti_2:', l2, inegPT, divoWWX(l2)
        endif
c
c ---------------------------------------------------------
c rrb 2018/10/14; 
c              Step 4b2; Set divoWWX to warn users in the WWSP report (*.xww)
c                        and loop to adjust for over allocation
c
        if(inegPT.gt.0) then        
          divoWWX(l2)=1.0
          n1=0
          n2=0
          do n=1,ndes
            n1=n2+1
            n2=n1+1
            
            ndP  =Iopdes(n1,L2)
c
c rrb 2019/08/25; Replace psuply(ndP) with psupDD(nd) set in 
c                 DivCarL.for a WWSP user (ndP)         
cx          psuplyD=psuply(ndP) 
            psuplyD=psupDD(ndP)/fac
            divact1=alocfs*pctX(n2)-psuplyD
c
c ---------------------------------------------------------
c               Reset percentage allotment (pctX) for those that
c                did not take too much by direct irrigation
c                and leave % alone if they did
c                Note psuplyDT is < 0 and divact1 is < 0
                  
            if(inegP(n2).eq.0) then
              pct2(n2) = pct1(n2) + pct1(n2)/pctPT * (psuplyDT/alocfs)
              pctX(n2) = pct2(n2)
            else    
              pct2(n2) = pct1(n2) - divact1/alocfs
cx              write(nlog,*) 
cx     1          ' Divmulti; CHECK n2, pat(n2)', n2, pct2(n2)
c
c rrb 2019/07/14-5; Correction              
cx            pctX(n2) = pctX(n2)
              pctX(n2) = pct2(n2)
           endif
c
c
c ---------------------------------------------------------
c							  Detailed Output            
            if(ioutX.eq.1) then
              divact2=alocfs*pct2(n2)
            
              write(nlog,*) ' '
              write(nlog,*) 'DivMulti_3; %s are getting adjusted'
              write(nlog,*)
     1          '  DivMulti;    n   n1   n2 ineg',
     1          ' divact1 psuplyDT alocfs psuplyD divact2',
     1          '   pctPT  pct1    pct2    pctX divoWWX'
     
              write(nlog,'(a12, 4i5, 5f8.0, 20f8.4)')
     1          '  DivMulti;', n, n1, n2, inegP(n2),
     1             divact1*fac, psuplyDT*fac, alocfs*fac, 
     1             psuplyD*fac, divact2*fac,
     1             pctPT, pct1(n2), pct2(n2), pctX(n2),divoWWX(l2) 
            endif
c
c               end do for number of destinations
          end do    
c
c
c               Endif for inegPT > 0 (when WWSP-Supply (type 14) checks  
        endif 
c
c               Endif for WWSP-Supply (type 14) checks
      endif   
c      
c _________________________________________________________
c               Step 5; Allocate Supplies to each plan for all plan types
      cdestyp='Plan     '
c
      ndes=int(oprlimit(l2))
      divactT=0.0
      n1=0
      n2=0
c _________________________________________________________
c               Step 5a; Loop for number of destinations
      do n=1,ndes
        n1=n2+1
        n2=n1+1
c
c rrb 2007/08/17; Limit to ownership %
c      
c _________________________________________________________
c               Step 5b; 
c rrb 2007/10/11; Reset percentage allotment for a WWSP plan supply
c                 when divert to irrigate is greater than allotment
c rrb 2019/04/20; Revised to recognize a WWSP user Plan is a type 15
cx      if(iplntyp(nsP).ne.14) then
c
c rrb 2019/07/14-6; Test for a positive condition
cx        if(iplntyp(nsP).ne.15) then
cx          Pct=ropdes(l2,n2)/100.0 
cx        else  
cx          Pct=pctX(n2)
cx        endif        
        if(iplntyp(nsP).eq.14) then        
          Pct=pctX(n2)
        else
          Pct=ropdes(l2,n2)/100.0 
        endif
c        
        ndP  =Iopdes(n1,L2)
        idcdD=ipsta(ndP)
        ndnsD=NDNNOD(idcdD)
c
        
        alocfs1=alocfs
c
c      
c _________________________________________________________
c               Step 5c; Adjust if a T&C plan (type 1) destination
c rrb 2008/01/15; If a T&C destination set qdiv(30 and
c		              adjustment to total diversion in outbal2
c                 Note a T&C destination does not store 
c                 data for a later month.  therefore do not
c                 adjust psto2
        if(iplntyp(ndP).eq.1) then
          divact1=alocfs*Pct
          divact=alocfs*Pct
          divactT=divactT+divact
c
c               Update destination plan (ndP)                
          pdrive(ndP) =pdrive(ndP) +divact
          psuply(ndP) =psuply(ndP) +divact
          psuplyT(ndP)=psuplyT(ndp)+divact
          qdiv(30,idcdD)=qdiv(30,idcdD)+divact
        endif
c
c      
c _________________________________________________________
c               Step 5d; Adjust destination plan totals 
c rrb 2018/09/22; Include divert to irrigate (psuplyD) that is 
c                 initialized to 0 or set if the source is a 
c                 WWSP-Supply plan (14)
C                 ndp = destination plan (User)
c                 nsp = source plan (Supply)  
c
c rrb 2019/04/10; Correction to work for all plan types
cx        if(iplntyp(ndP).eq.15) then  
cx                
          divact1=alocfs*Pct
c
c 2019/07/14; Correction to work for all plan types where
c             PsuplyD is already initialized or set if it
c             the source is a WWSP-Supply (14) plan  
c             Do not adjust divact for psuplyD; that is taken
c             care of by the variable Pct  
cx        psuplyD=0.0
cx        psuplyD=psuply(ndP) 
c 
c rrb 2019/08/25; Correction
          if(iplntyp(ndP).eq.15) then        
            psuplyD=psupDD(ndP)/fac
          else
            psuplyD=0.0
          endif
cx                   
          divact=alocfs*Pct-psuplyD
          divactT=divactT+divact
          
          iok=0
          if(divact.lt.smalln) then
            iok=0
            divactT=divactT-divact
            divactX=divact
            divact=0.0
          endif
c      
c _________________________________________________________
c               Step 5e; Adjust Destination (ndP)
c rrb 2018/09/30; Destination (User Plan) 
C                 ndp = destination plan (User)
c                 Adjust supply
        
          psuply1=psuply(ndp)
          psuply(ndP) = psuply(ndP) +divact
          psuply2=psuply(ndp)
          
          psuplyT1=psuplyT(ndP)
          psuplyT(ndP)= psuplyT(ndp)+divact   
           
          psuplyT2=psuplyT(ndP) 
              
          psto2(ndP)  = psto2(ndP)+divact*fac 
c
c rrb 2019/08/25; Optional detailed output
c
c rrb 2020/05/29; Check           
        if(ioutX.eq.1) then
            write(nlog,*) ' '
            write(nlog,*) ' ___________________________________'
            write(nlog,*) 
     1       ' DivMulti_4; Source Plan pid(nsp) = ', pid(nsP)           
            write(nlog,*) 
     1       ' DivMulti_4; pid(ndP)           n  alocfs     pct',
     1          ' psuply1  divact psuplyD psuply2',
     1          ' psup_T1 psup_T2'
            write(nlog,'(a13, 1x, a12, i8, 20f8.0)') 
     1       '  DivMulti_4;', pid(ndP), n, alocfs*fac, pct*100., 
     1          psuply1*fac, divact*fac, 
     1          psuplyD*fac, psuply2*fac, psuplyT1*fac, psuplyT2*fac
c
c rrb 2020/05/29; Check           
        endif
c          
c      
c _________________________________________________________
c               Step 5f; Adjust Supply
c
c rrb 2018/09/22; Supply (Source Plan)          
C                 ndp = destination plan (User)
c                 nsp = source plan (Supply)  
          psuply(nsP) = psuply(nsP)-divact
c
c rrb 2018/10/08; Correction a release is not a negative supply
cx        psuplyT(nsP)= psuplyT(nsP)-divact       
          
          psto2(nsP)  = psto2(nsP)-divact*fac          
c
c               Detailed Output          
          if(ioutP.eq.1) then
            write(nlog,*) ' '
            write(nlog,*) 
     1       '  DivMulti_5; alocfs1     pct divact1 psuplyD  divact'
            write(nlog,'(a12, 20f8.0)') 
     1       '  DivMulti_5;', alocfs1*fac,pct*100, divact1*fac,
     1          psuplyD*fac,divact*fac
          endif
c          
c _________________________________________________________
c rrb 2018/10-07; 
c               Step 5g; Set detailed operating rule data (divoWW()
c                 for WWSP reporting
c                 Note printed in Outmon.  Read in OutWW
          divoWW(n,l2) = divoWW(n,l2) + divact
          if(ioutP.eq.1) then
            write(nlog,*) ' '
            write(nlog,*) 
     1       '  DivMulti_6; n, l2, divoWW(n,l2)', n, l2, 
     1          divoWW(n,l2)*fac
          endif      
c          
c      
c _________________________________________________________
c               Step 5h; Exit if negatives.  Currently turned off
c                        in step 5d after adding logic to
c                        remove over allocation from other users
          if(iok.eq.1) goto 9999
c
c rrb 2019/04/10; Correction to work for all plan types
cx        endif      
c
c ---------------------------------------------------------
c                End destination loop (n=1, ndes)
      enddo
c
cc _________________________________________________________
c
c rrb 2019/07/14-7; 
c              Step 9 Set psuply(nsP) for the WWSP-Supply (type 14)
c                     to remove any direct irrigation
c                     Note psuply(ndP) the WWSP-User (15) is reset in
c                     Bomsec after results have been printed.
c  
c rrb 2020/05/31; Revise to emphasize this only occurs for a plan
c                 type 14(a WWSP plan).  Specifically psuplyX can onl
c                 be > 0 for a type 14 WWSP Plan
      if(iplntyp(nsP).eq.14) then
        psuply(nsP) = amax1(0.0, psuply(nsP)-psuplyX)
      endif
c
c
c _________________________________________________________
c
c              Step 10. Double Check available flow
c
c rrb 00/05/03; Check entire array, not just downstream               
      call dnmfsow(maxsta, avail, numsta, imcd)
      avail2=avail(imcd)
      
      IF(AVAIL(IMCD).le.(-1.*small)) then
        write(nlog,318) imcd, avail(imcd)*fac 
        write(nlog,320) (avail(iss),iss=1,numsta)
        write(nlog,330) (river(iss),iss=1,numsta)
        goto 9999
      endif
c       
c
c _________________________________________________________
c
c               Step 11; Update operating rule output (DIVO)
      divo(l2)=divo(l2)+divactT
      
c _________________________________________________________
c
c               Step 12.  Detailed output
c

 260  if(iout.ge.2 .and. iw.eq.ioutiw) then
c
c ---------------------------------------------------------
c		a. Header for this time step
c
        ncallX=ncallX+1
        if(ncallX.eq.1)then
          write(nlog,270) corid(l2),cdestyp, ccarry,cTandC
        else
c          write(nlog,*) ' '
        endif  
c
c ---------------------------------------------------------
c		b. Data for every time step and iteration
c
        write(nlog,280) '  DivMulti  ',
     1    iyrmo(mon),xmonam(mon), idy,
     1     cstaid1, iwx, iw, nsP, ndes,  
     1     alocfs*fac,  divactT*fac, psuplyD*fac,
     1     (iopdes(n,l2), ropdes(l2,n+1), n=1,10,2),
     1     divactT*fac, iwhy, cwhy
     
  280     FORMAT(a12, i5,1x,a4,i5, 1x,a12, 4i8, 3F8.1,
     1    5(i8, f8.2), f8.1, i5,1x,a48)
     
      endif
      
c
c _________________________________________________________
c
c               Step 13; Check Avail for Roundoff issues
      call chekava(46, maxsta, numsta, avail, subtypX)
c
c _________________________________________________________
c
c               Step 14; Return
      RETURN
c
c _________________________________________________________
c
c                Formats
c
  270   format(/, 
     1  '  DivMulti (Type 46); Operation Right ID = ', a12,
     1  ' Destination Type = ', a12,
     1  ' Carrier (Y/N) = ',a3, ' Associated Plan (Y/N) = ', a3/    
     1  '  DivMulti    iyr  mon  idy Source_ID   ',
     1  '   Iter#  Right#     nsP    ndes',
     1  '  alocfs  divact psuplyD',
     1  ' iopdes1       % iopdes3       % iopdes5       %',
     1  ' iopdes7       % iopdes9       %  DIVACT',
     1  ' iwhy cwhy',/
     1  '____________ ____ ____ ____ ____________',
     1  7(' _______'), 11(' _______'),' ____', 1x, 48('_'))
     
  318   format(/, 72('_'),/
     1  '  DivMulti; Problem Negative Available Flow ',/
     1  '            Imcd = ',i5,' Avail (af) = ',f10.3)
  320   format(/, '  DivMulti; avail  ',/,(10f10.2))
  330   format(/, '  DivMulti; river  ',/,(10f10.2))
  340   format(//,
     1  '  DivMulti; Warning:',/
     1  '    Year ', i5, ' Month ', a4,' WWSP User Plan ID = ', a12,/
     1  '    Diverted more to irrigate than its allotment',/
     1  '      Total Allotment    = ', f8.0,/
     1  '      Percent            = ', f8.0,/
     1  '      Allotment to user  = ', f8.0,/
     1  '      Divert to Irrigate = ', f8.0,/
     1  '      Over allotment     = ', f8.0, /
     1  '    Simulation is continuing by removing the over allotment',/
     1  '    from other users based on their percent allotment.'/
     1  '    Recommend you verify the over allocation is correct.')
c
c _________________________________________________________
c
c              Error warnings
c
 9999 write(nlog,270) corid(l2),cdestyp, ccarry,cTandC
         write(nlog,280) '  DivMulti   ',
     1    iyrmo(mon),xmonam(mon), idy,
     1     cstaid1,iwx, iw, nsP, ndes,  
     1     alocfs*fac,  divactT*fac, psuplyD*fac,
     1     (iopdes(n,l2), ropdes(l2,n+1), n=1,10,2),
     1     divactT*fac, iwhy, cwhy
c    

      write(6,500) 
      write(nlog,510) 
      call flush(6)
 500  format('    Stopped in DivMulti',/,
     1       '    See the *.log file')
 510  format('    Stopped in DivMulti')
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop 
      END

