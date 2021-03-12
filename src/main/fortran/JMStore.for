c JMSTORE - Type 53 operating rule,
c      It simulates the storage of Arkansas River at Las Animas and
c      Purgatoire River at Las Animas in the JMartin
c      Conservation Storage Account and Other Storage Account.
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
      SUBROUTINE JMStore(IW,L2,ncallX)
c
c
c _________________________________________________________
c	Program Description
c
c  JMStore; Type 53.
c		 It simulates the storage of Arkansas River at Las Animas and
c      Purgatoire River at Las Animas in the JMartin
c      Conservation Storage Account and Other Storage Account
c      based on:
c       Flow at a Arkansas River at Las Animas and
c       Flow at the Purgatoire River at Las Animas.
c       A coefficient provided by a type 54 operating rule that is
c         the percent of Arkansas River flows to store in Conservation
c         Storage and Other Storage.
c       A coefficient provided by variable iopsou(4,l2) that is 
c         the percent of the Purgatoire River flows to store in
c         Conservation storage.
c       Available storage in a reservoir account (AvailD)
C       Available storage in a reservoir limited by the target 
c          content(AvailR)
c       Minimum flow downstream (AvailX)
c
c       Approach:
c         Begin Destination Loop
c           For destination 1 (conservation)
c             Divert from Arkansas River to Conservation
c           For destination 2 (Other)
c             Divert from Arkansas to Other
c             Divert from Purgatoire to Conservation
c         End destination loop 
c
c
c
c
c_____________________________________________________________
c
c       Update History
c
c
c rrb 2020/07/15; 2020/07/15 (16.00.35)
c                 Revise the Available flow printed to *.xjm
c                 (divoWWX(l2) to be the min flow downstream from 
c                 JMartin reservoir not the flow of the Ark @ LA
c
c rrb 2020/06/09  Revised JMStore (type 53) logic to correct ',/
c                 approach to determine if there is a loosing',/
c                 reach from JMartin (confluence of Ark & Purg',/
c                 Also deleted several sections that are not used',/
c
c rrb 2020/06/03; Revised logic to correct Purgatoire operation
c                 to operate after the Arkansas has diverted
c                 to both Conservation (1) and Other (2)
c                 Note the approach used only works because
c                 the Purgatoire only goes to Conservation (1)
c
c rrb 2020/05/22; Revised to store Purgatoire in Conservation
c                 pool, not Other pool
c                 Fixed initialization of divactT=0 (see Step 1c)
c
c rrb 2020/04/19; Revision to handle small roundoff             
cx                Change: if(AvailX .gt. avail(ns)) then
c                 To:     if((AvailX + small) .gt. avail(ns)) then
c 
c rrb 2018/12/15; Revised to include both Arkansas River and
c                 Purgatoire River in one pass to simplify
c                 the passing of data in variable ropdes( )
c 
c rrb 2018/10/27; Revised to allow a shortage caused by a 
c                 loosing reach to be evenly shared between
c                 Arkansas River and Purgatoire River
c                 using variable pctS = (Ark Q) / (Ark Q + Purg Q)
c                 Resulted in calling Purgatoire after the Arkansas
c                 is done (e.g. when the # destinations (n) = 2 not 1
c
c rrb 2018/08/18; Copied divmultR.for and revised
c		           
c _________________________________________________________
c
c       Documentation
c	
c               
c               IW         Global water right ID
c               L2         LOC. OF operation right  in opr RIGHT TABLE
c  
c               divoWW(i,l2) Variable used to store detailed operating
c                            rule data where i is consistent with 
c                            detailed reporting in subroutine OutJM.f  
c
c               iloose     = 0 Not a loosing reach downstream of the res 
c                            1 A loosing reach downstream of the 
c                              reservoir 
c
c               iopdes(1,l2)=  Destination 1 reservoir pointer
c               iopdes(2,l2)=  Destination 1 account pointer
c               iopdes(3,l2)=  Destination 2 reservoir pointer
c               iopdes(4,l2)=  Destination 2 account pointer
c              
c               nopr       = iopsou(5,l2), a type 54 operating rule 
c                            with % data to allocate flow at the 
c                            Ark @ LA gage to Conservation and Other
c                            storage.
c               ns         Source gage ID Arkansas
c               ns2        source gage ID Purgatoire
c               ndR        Destination reservoir ID
c               nx         Pointer used to store diversion data in
c                            variable divoWW (nx=6) as follows 
c                            Conservation storage = divoww(nx+1=7,l2) & 
c                            Other storage = divoww(nx+2=8,l2)
c               pct   		 Percent of gage to store in an account. Note:
c                            - pct for the Arkansas River is provided by
c                                  an operating rule ID (variable 
c                                  iopsou(5,l2)) 
c                            - pct for the Purgatoire River is provided 
c                                  by variable iopdes(4,l2)
c               pctS       Fraction to adjust Arkansas flows when 
c                          there is a loosing reach downstream.
c               pctS2      Fraction to adjust Purgatoire flows when
c                          there is a loosing reach downstream.
c
c               AvailX     Min flow downstream of diversion
c               AvailG1    Initial gaged flow
c               AvailG     Initial gaged flow * pct * pctS
c               AvailD     Available storage in a reservoir account
c               AvailR     Available storage in a reservoir
c                          
c
c_____________________________________________________________
c	      Dimensions
c	
      include 'common.inc'
      character cwhy*48, cdestyp*12, ccarry*3, cpuse*3, cstaid1*12,
     1          rec12*12, cTandC*3, cidRiv*12, cidBal*12,
     1          cresid1*12, cresid2*12, cCallby*12, csour1*12, 
     1          csour2*12,  subtypX*8
c
c
c _________________________________________________________
c
c               Step 1 Common Initialization
c
c
c ---------------------------------------------------------
c
      cCallBy='JmStore    '   
      subtypX='JMStore '   
c
c ---------------------------------------------------------
c		       a. OutPut control
c		            ioutX = 0 No details
c		                    1 Details
c                       2 Std output each time called
c               ioutA = 0 No details on variable avail from Chekav3
c                       1 Details     
c     
      iout=0
      ioutX=0
      ioutiw=0
      ioutA=0
c
c          b. Quick exit for checking      
cxx      goto 500
      
      ncallX=ncallX+1
      
      if(ichk.eq.153) iout=2
      if(corid(l2).eq. ccall) ioutiw=iw
c
      if(iout.ge.1 .and. ncallx.eq.1) then
        write(nlog,102) corid(l2)
 102    format(/, 72('_'),/ ' JMStore; ID = ', a12)
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
      
      divact =0.0
      divact1=0.0
      divact2=0.0
c
c rrb 2020/05/22; Correction
cx    civactT=0.0
      divactT=0.0
      
      divAF=0.0
      divAF1=0.0
      divAF2=0.0
      
      pct=-1.
      
      availX  =-1./fac
      availX1 =-1./fac
      availX2 =-1./fac
      availG1 =-1./fac
      availG  =-1./fac
      availB  =-1./fac
      availD  =-1./fac
      availJM =-1./fac
      
      cursto1=-1.
      cursto2=-1.
c
      ishort=0                      
c
c ---------------------------------------------------------
c		       f. Detailed Output
      iwhy=0
      cwhy='NA'
      cdestyp='NA'
      ccarry='No'
      cpuse='No'
      cTandC='No'
      cstaid1='NA'
      cdestyp='Reservoir'      
      
      cresid1='NA'
      csour1='NA'
      csour2='NA'
      iacct=0
c
c ---------------------------------------------------------
c rrb 2018/12/16; Set avtemp check impact to array avail      
      
      ioutC=1
      idcd=0
      
        
c
c _________________________________________________________
c		            Step 2; Check for On/Off Switches      
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
      if(iday.eq.1 .and. imonsw(l2,mon).gt.0) then
        if (idy.lt.imonsw(l2,mon)) then
          iwhy=1
          cwhy='Daily switch Off'
          ioff=1
        endif  
      endif  
c
c ---------------------------------------------------------
c		       Daily On/Off Switch end on day x
      if(iday.eq.1 .and. imonsw(l2,mon).lt.0) then
        if (idy.gt.iabs(imonsw(l2,mon))) then
          iwhy=1
          cwhy='Daily switch Off'
          ioff=1
        endif  
      endif  
c
c ____________________________________________________
c               Step 3; Check reoperation control and exit
c               if already operated once this time step
       icallOP(l2)=icallOP(l2) + 1
c     
       if(icallOP(l2).gt.1) then
         iwhy=2
         cwhy='Limited to one call per time step'
         ioff=1
       endif      
c
c ---------------------------------------------------------
c               Step 4. Exit if system is off
      if(ioff.eq.1) then
        divact=0.0
        divact1=0.0
        divact2=0.0
        divactT=0.0
        divaF=0.0        
        goto 300
      endif    
c          
c ---------------------------------------------------------
c               Step 4.1 Check Avail array coming in
      if(ioutX.eq.1) then
        write(nlog,*) ' ' 
        write(nlog,*) '  JMStore_2; Call chekava'
      endif
      
      call chekava(21, maxsta, numsta, avail, subtypX)      
c     
c _________________________________________________________
c
c		            Step 5; Determine minimum flow downstream
c                       Note this occurs outside the 
c                       number of destination loop to 
c                       ensure shortages are shared equally
c     
c
c rrb 2018/12/15; Revise to include source 5 as the Type 54 operating rule ID
cx    nopr = iopsou(3,l2)
      nopr = iopsou(5,l2)
c          
c ---------------------------------------------------------
c               Step 5.1 Set Primary source (ns = Arkansas)
      ns  = iopsou(1,l2)
      csour1=cstaid(ns)       
      iscd=ns        
      ndns=ndnnod(iscd)
c
c ---------------------------------------------------------
c rrb 2019/10/27; 
c               Step 5.2 Initialize source 2 (ns2 = Purgatoire)
c                 here to allow a loosing reach to be determined 
c                 in step 7.1     
      ns2  = iopsou(3,l2)
      csour2=cstaid(ns2) 
c          
c ---------------------------------------------------------
c               Step 5.3; Calculate minimum downstream flow
c                 from the primary source (Ark @ LA)
     
      CALL DNMFSO2(maxsta, AVAIL, IDNCOD, iscd, NDNS, IMCD,
     1  cCallBy)
         
      imcdX=imcd
      availX=avail(imcd)
      
      if(availX.le.small) then
        iwhy=3
        cwhy='Available flow below stream gage (AvailX) = 0'
        goto 300
      endif
c        
c _________________________________________________________
c		            Step 6; Set Variables divoWW() for detailed reporting
c                       from variables (ropdes) set in a type 54
c                       operating rule.  Note column 1 of *.xjm is 
c                       divowwX (available flow) therefore the divoww 
c                       pointers are 1 less than the column of output
c                       in *.xjm, Also, nx is where Purgatoire data
c                       begins to be printed. Other values are:
c         NA = Available Flow
c         1=Ark flow,     2= ave baseflow    3=ave enhanced flow, 
c         4=%_base,       5=%_enhan          6=Pur flow, 
c         7=Cons storage, 8 = Other storage
c
      nx=6

      divoWW(1,l2) = avail(ns)
      
      divoWW(2,l2) = ropdes(nopr,3)/fac
      divoWW(3,l2) = ropdes(nopr,4)/fac                         
      divoWW(4,l2) = ropdes(nopr,1)/fac
      divoWW(5,l2) = ropdes(nopr,2)/fac 
c
c rrb 2018/12/15; Set Purgatoire River flow
      divoWW(6,l2) = ropdes(nopr,5)     
      divoWW(7,l2) = 0.0 
      divoWW(8,l2) = 0.0
      divoWW(9,l2) = 0.0  
c
c rrb 2019/10/28; Store available flow 
c rrb 2020/07/15; Move to step 7.2 so that the Available flow 
c                 printed to *.xjm (divoWWX(l2) is the min flow
c                 downstream from the reservoir 
cx    divoWWX(l2)  = availX
cx    write(nlog,*) '  JMStore;', l2, divoWWX(l2)*fac
c _________________________________________________________
c		            Step 7; Determine flow at Stream gage (AvailG)
c
c                       Note this occurs outside the 
c                       number of destination loop to 
c                       ensure shortages are shared equally
c  
c rrb; 2019-10-27; Revise to recognize available flow is 
c                  Limited to max available downstream 
cx    availG1 = avail(ns)
      availG1 = availX     
c
c
c          
c ---------------------------------------------------------
c rrb 2020/06/09;
c               Step 7.1; Calculate minimum downstream flow from 
c                 the destination reservoir (after Ark & Purgatorie
c                 converge to determine if there is a loosing reach
      ndR=iopdes(1,l2)
      iscdR=irssta(ndR)
      ndnsR=ndnnod(iscdR)
     
      CALL DNMFSO2(maxsta, AVAIL, IDNCOD, iscdR, ndnsR, IMCD,
     1  cCallBy)
         
      imcdR=imcd
      availY=avail(imcdR)
c
c ---------------------------------------------------------
c rrb 2020/07/15; 
c               Step 7.2 Set Available flow printed to *.xjm 
c                 (divoWWX(l2) to be the min flow downstream
c                 from the reservoir
      divoWWX(l2) = avail(imcdR)

cx    if(ioutX.eq.1) 
      write (nlog,*) '  JMStore; availY = ', availY*fac
      
      if(availY.le.small) then
        iwhy=3
        cwhy='Available flow below destination (AvailY) = 0'
        goto 300
      endif

c --------------------------------------------------=------
c               Step 7.3; Determine if there is a loosing reach
c                         If it is, reduce flow available at 
c                         each gage porportionally
c                         Note small = 0.001 and
c                         rlossX is > 0 for a loosing reach
c
      rtot=avail(ns) + avail(ns2)
      rlossX = rtot - AvailY
      
c       
      if(rlossX.lt.small) then
        availG1 = avail(ns)
        availG2 = avail(ns2)
      else
        availG1 = avail(ns)  - rlossX * avail(ns)/rtot
        availG2 = avail(ns2) - rlossX * avail(ns2)/rtot
      endif
      
cx      if(ioutX.eq.1) then
        write(nlog,*) ' '
        write(nlog,*) '  JMStore_3; ns, ns2, AvailX,',
     1               ' avail(ns), availG1, avail(ns2), availG2',
     1               ' rtot, rlossX,  '
        write(nlog,*) '  JMStore_3;', ns, ns2, AvailX*fac,
     1                avail(ns)*fac,  availG1*fac,
     1                avail(ns2)*fac, availG2*fac
     1                rtot*fac, rlossX*fac  
cx      endif
c
c _____________________________________________________________
c rrb 2020/06/03; Check avail array
      ifirst=0
      icx=53
      
      nchkA=1
      
cx      if(ioutX.eq.1) then
        call ChkAvail2(nlog, ifirst, icx, nchkA, maxsta, numsta, 
     1     fac, avail)
cx      endif 
c
c     
c *********************************************************
c               Step 8; Begin loop for number of destinations
c
c
c rrb 2018/12/15; Revise because oprlimit is +1 to include
c                 the operating rule data
      ndes=int(oprlimit(l2))
      ndes=ndes-1
      ndes=amax0(1, ndes)

      n1=0
      n2=0        
        do n=1,ndes
          n1=n2+1
          n2=n1+1
c          
          divact=0.0
          divact1=0.0
          divact2=0.0
          
          divAF=0.0        
          divAF1=0.0
          divAF2=0.0
c
          iwhy=0
          cwhy='NA'
c
c _________________________________________________________
c
c		            Step 9; For the Arkansas River determine the
c                       percent of flow at Stream gage (availG1)  
c                       to store in this account (availG1*pct/100)
c                       NOTE nopr is the operating rule ID used
c                       to calculate the % of Arkansas River to store
c
          pct =ropdes(nopr,n) 
          availG = availG1 * pct/100 
c
          if(availG.le.small) then
            iwhy=4
            cwhy='Gaged flow * pct (AvailG) for this time = 0'
          endif
                 
          if(ioutX.eq.1) then
            write(nlog,*) ' '
            write(nlog,*) '  JMStore_4;',
     1        '    n   n1   n2   ix ndes nopr',
     1        '     pct AvailG1  AvailG'
            write(nlog,'(1x,a12,6i5, 20f8.0)') 
     1        '  JMStore_4;', n, n1, n2, ix, ndes, nopr, 
     1        pct, availG1*fac, availG*fac
          endif
c     
c _________________________________________________________
c
c		            Step 10; Determine space in dest account (availD - cfs)
c                        Note:
c                        iopdes(2,l2)=  Destination 1 account pointer
c                        iopdes(4,l2)=  Destination 2 account pointer
c                
          ndR  =Iopdes(1,l2)
          cresid1=cresid(ndR)
          iacct=iopdes(n2,l2)
          
          idcd=irssta(ndr)        
          ndnd=ndnnod(idcd)
          
          irowD  =nowner(ndR)+iopdes(n2,l2)-1          
          curown1=curown(irowD)         
          
          availD=amax1(ownmax(irowD)-curown1, 0.0)/fac
          
          if(availD.le.small) then
            iwhy=5
            cwhy='Available space in res account is zero'
          endif        
c     
c _________________________________________________________
c
c		            Step 11; Determine space in reservoir (availR- cfs)
c              
          cursto1=curSto(ndR)
          curstoV=volmax(ndR)-cursto1  
          curstoT=tarmax(ndR)-cursto1
          
          availR=amin1(curstoV, curstoT)          
          availR=amax1(availR, 0.0)/fac
          
          if(availR.le.small) then
            iwhy=6
            cwhy='Available space in res storage or target is zero'
          endif        
c     
c _________________________________________________________
c
c		            Step 12; Determine diversion amount from Ark River 
c 
          divact  = amin1(availX, availD, availR, availG)
          divact1 = divact
c
c rrb 2019/10/27; Store total diversion in divactT          
          divactT = divactT+divact
          divAF   = divact1*fac   
          divAF1  = divAF                       
c _________________________________________________________
c
c               Step 13; Remove diversion (DIVACT) from the Arkansas
c                        river from avail & stream
c                        AT THE RESERVOIR LOCATION (IDCD & NDND)
c                        iscd=stream gage ID
c                        ndns = # of downstream nodes from gage
c                        idcd = reservoir location
c                        ndnd = # of downstream nodes from reservoir
c
c
c ---------------------------------------------------------
c		            Check Detailed results of variable avail with diversion
          if(ioutX.eq.1) then
            write(nlog,*) '  JmStore_6; call chekav3'
          endif
c
          icode=1 
          call chekav3(icode, ioutA, maxsta, numsta, avtemp, 
     1             AVAIL, idcd,  nlog,   fac,    idy, 
     1             iyrmo(mon), xmonam(mon), ccallBy, cstaid, stanam1)
c     
          CALL TAKOUT(maxsta,AVAIL,RIVER,AVINP,QTRIBU,IDNCOD,
     1                divact,ndnd,idcd)
c     
          icode=2
          call chekav3(icode, ioutA, maxsta, numsta, avtemp, 
     1             AVAIL, idcd,  nlog,   fac,    idy, 
     1             iyrmo(mon), xmonam(mon), ccallBy, cstaid, stanam1)
c
c rrb 2019/10/27        
          if(ioutX.eq.1) then
            write(nlog,*) ' '
            write(nlog,*) '  JmStore_5;  idcd,availX divact'
            write(nlog,*) '  JmStore_5;',idcd,availX*fac, divact*fac
          endif
          
          
     
c     
c *********************************************************                
c
c               Step 13.5;
c rrb 2020/06/03; Correction.  Moved here from below Purgatoire
c               (Step 15); Update destination reservoir & account
          curown1=curown(irowD)      
          cursto1=cursto(ndR)
          
          curown(irowD)= curown(irowD) + divAF
          cursto(ndR) = cursto(ndR) + divAF
          
          curown2=curown(irowD)
          cursto2= cursto(ndR)  
c
c
c ---------------------------------------------------------
c
c               Step 13.6
c rrb 2020/06/03; Correction. Moved here from below Purgatoire
c               (Step 18) to keep Arkansas and Purgatoire separate
c
          divoWW(nx+n,l2) = divoWW(nx+n,l2) + divact
          if(ioutX.eq.1) then
            write(nlog,*) ' '
            write(nlog,*)'  JMStore_11;  n, nx, l2, divoWW(nx+n,l2)' 
            write(nlog,*)'  JMStore_11;',n, nx, l2, divoWW(nx+n,l2)*fac
          endif      

c   
c *********************************************************                
c
c _________________________________________________________
c
c               Step 14.1; 
c                 If n=2 account for Purgatoire after adjusting
c                 availD (available in Destination),
c                 AvailR (available in reservoir)
c
c rrb 2019/10/27; Calculate storage at purgatoire when n = 2
c                 (storage in conservation) to allow a loosing 
c                  reach to be accmondated
          if(n.eq.2) then
            if(ioutX.gt.0) then
              write(nlog,*) ' '
              write(nlog,*) ' __________________________________'
              write(nlog,*) '  JMStore_7; Purgatoire n = ', n
            endif
c            
            availD2=amax1(0.0, availD-divact)
            availR2=amax1(0.0, availR-divact)
                     
            ns2  = iopsou(3,l2)
            pct2 = iopsou(4,l2)
                       
            csour2=cstaid(ns2) 
c
c rrb 2020/06/03; Revise to be unique to Purgatoire
cx          iscd=ns2        
cx          ndns=ndnnod(iscd)

            iscd2=ns2        
            ndns2=ndnnod(iscd2)
             
            if(ioutX.eq.1) then
              write(nlog,*) ' '
              write(nlog,*) '  JMStore_8;  ns2, pct2, cstaid'
              write(nlog,*) '  JMStore_8;',ns2,pct2,cstaid(ns2)
            endif
c
c ---------------------------------------------------------
c		            Step 14.3; 
c                 Determine diversion amount 
c                 Note do not need flow at diversion
c                 availG since its included 
c
c rrb 2020-06-09; Correction include AvailG2 that includes
c                 an adjustment for a loosing reach
c
cx          divact2 = amin1(availX2, availD, availR)
            availG  = availG2 * pct2/100.
            
            divact2 = amin1(availG, availD, availR)
            
            divact2 = amax1(divact2, 0.0)
            
            divact  = divact1+divact2
c
c rrb 2019/10/27; Store total diversion in divactT          
            divactT = divactT+divact2
c
c   
c *********************************************************                
c rrb 2020/06/03; Correction to separate Purgatoire
            divAF2 = divact2*fac
            
            divAF  =  divact1*fac + divact2*fac  
c 
            if(ioutX.eq.1) then   
              write(nlog,*) ' '                 
              write(nlog,*) '  JMStore_9;',
     1          ' AvailX2  availD  availR divact2'
              write(nlog,'(2x, a12, 20f8.0)') ' JMStore_9;', 
     1            AvailX2*fac, availD*fac, availR*fac, divact2*fac 
            endif                         
c
c ---------------------------------------------------------
c		            Step 14.4; 
c                 Remove diversion (DIVACT) from the Purgatoire
c                 river from avail & stream
c                 AT THE RESERVOIR LOCATION (IDCD & NDND)
c                 iscd=stream gage ID
c                 ndns = # of downstream nodes from gage
c                 idcd = reservoir location
c                 ndnd = # of downstream nodes from reservoir
c
            if(ioutX.eq.1) then
              write(nlog,*) '  JmStore_10; call chekav3'
            endif
            
            icode=1
            call chekav3(icode, ioutA, maxsta, numsta, avtemp, 
     1             AVAIL, idcd,  nlog,   fac,    idy, 
     1             iyrmo(mon), xmonam(mon), ccallBy, cstaid, stanam1)
c
            CALL TAKOUT(maxsta,AVAIL,RIVER,AVINP,QTRIBU,IDNCOD,
     1                  divact2,ndnd,idcd)
c          
            icode=2
            call chekav3(icode, ioutA, maxsta, numsta, avtemp, 
     1             AVAIL, idcd,  nlog,   fac,    idy, 
     1             iyrmo(mon), xmonam(mon), ccallBy, cstaid, stanam1)
c
c ---------------------------------------------------------
c     
c               Step 14.5
c rrb 2020/06/03; Revise to adjust Purgatoire within Purg. section
c                 Update destination reservoir & account
c                 Note use iopdes(1,l2) and iopdes(2,l2) knowing the
c                 first destination reservoir and account for the 
c                 the Arkansas River is to Conservation

            ndR2  =Iopdes(1,l2)
            cresid2=cresid(ndR2)
                                               
            irowD2 = nowner(ndR2) + iopdes(2,l2) - 1
            
            curown1p=curown(irowD2)      
            cursto1p=cursto(ndR2)
c
c rrb 2020-06-05; Correction for Purgatoire            
cx          curown(irowD2)= curown(irowD2) + divAF
cx          cursto(ndR2) = cursto(ndR2) + divAF

            curown(irowD2)= curown(irowD2) + divAF2
            cursto(ndR2) = cursto(ndR2) + divAF2
            
            curown2p=curown(irowD2)
            cursto2p= cursto(ndR2)      
            
            if(ioutX.eq.1) then
              write(nlog,*) 
     1        '  JMStore; ndR2, irowD2, curown1p, divAF2, curown2p'
              write(nlog,*) 
     1        '  JMStore;',ndR2, irowD2, curown1p, divAF2, curown2p
            endif
            
c
c ---------------------------------------------------------
c
c               Step 14.6
c rrb 2020/06/03; Correction. Moved here from below Purgatoire
c                 Step 18 to keep Arkansas and Purgatoire separate
c                 where nx=6 (flow in Purgatoire and nx=7 corresponds 
c                 storage in the Conservation Pool in *.xjm
            divoWW(nx+1,l2) = divoWW(nx+1,l2) + divact2          
 
            if(ioutX.eq.1) then
              write(nlog,*) ' '
              write(nlog,*)'  JMStore_11;  n, nx, l2, divoWW(nx+1,l2)' 
              write(nlog,*)'  JMStore_11;',n,nxl2,divoWW(nx+1,l2)*fac
            endif      
c
c ---------------------------------------------------------
c
c               Step 14.7  Endif for including Purgatoire after
c                          n=2 for the Arkansas           
          endif
c _________________________________________________________
c
c               Step 16; Update reservoir report values
c               ndR      destination reservoir
c               irowD    destination reservoir account
c               qres(18  From river by other
c
          QRES(18,ndR)=QRES(18,ndR) + divAF
          accr(18,irowD)  = accr(18,irowD) + divAF
c
c rrb 2018/11/17; Adjust qdiv to get correct reporting at
c                 the diversion JMartin (idcd)         
          qdiv(26,idcd)=qdiv(26,idcd) + divact
c
c _________________________________________________________
c
c               Step 17; Check destination storage has not 
c                        gone above capacity        
          if(curown(irowD).gt.ownmax(irowD)) then
            write(nlog,250) curown(irowD), ownmax(irowD)
 250        format(/, ' JmStore_11b;  ', 
     1            'Warning destination account storage   = ',f8.0,/
     1       10x, ' is greater than the accoount capacity = ',f8.0,/
     1       10x, 'Excess storage is left at source.')
            goto 9999
          endif    
c    
c _________________________________________________________
c               
c               Step 18; Update operating rule output (DIVO)
c                        Note nx=6; to correspond to specific
c                        column of output in *.xjm
          divo(l2)=divo(l2)+divAF/fac
c _________________________________________________________
c
c               Step 19; Call Chekres for Roundoff issues
          call chekres(io99,maxres, 1, 53, iyr, mon, nsR, nowner,
     1               curown,cursto,cresid)  
c
c _________________________________________________________
c
c               Step 20; Detailed output within destination loop 
c 
  260     continue        
          if((iout.ge.2 .and. iw.eq.ioutiw) .or. ioutX.ge.1) then
c
c ---------------------------------------------------------
c		            a. Header for this time step
c
            if(ncallX.eq.1 .and. n.eq.1)then
              write(nlog,270) corid(l2),cdestyp, ccarry,cTandC
            else
c              write(nlog,*) ' '
            endif  
c
c ---------------------------------------------------------
c		            b. Detailed output
c
            write(nlog,280) ' JMStore    ',
     1         iyrmo(mon),xmonam(mon), idy, csour1, csour2, cresid1,
     1         iacct, iwx, iw, n, ns, ndr, irowd, nopr, imcdX, 
     1         pct,  pctS*100., pctS2*100.,
     1         AvailX*fac, AvailG1*fac, AvailG*fac, AvailD*fac,
     1         AvailR*fac,  curown1, curown2, divact1*fac, availX2*fac,
     1         divact2*fac, divact*fac, divactT*fac, iwhy, cwhy
          endif
c
c *********************************************************
c
c               Step 21; End destination loop

        end do  
c_______________________________________________________________________
c rrb 2020/06/03; Check avail
c		            Step 22; Check Avail for Roundoff
        if(ioutX.eq.1) then
          write(nlog,*) ' '
          write(nlog,*)'  JmStore_13; Calling RoundOf'
        endif
        
        call roundof(avail, numsta, 8, 22, nbug)
c
c _________________________________________________________
c
c               Step 23; Normal exit with monthly switches on

        goto 500
 
c
c _________________________________________________________
c
c                Step 24; Exit point if monthly switches are off
 300    continue

        if((iout.ge.2 .and. iw.eq.ioutiw) .or. ioutX.eq.1) then
c
c ---------------------------------------------------------
c		            a. Header for this time step
c
          if(ncallX.eq.1)then
            write(nlog,270) corid(l2),cdestyp, ccarry,cTandC
          else
c            write(nlog,*) ' '
          endif  
c
c ---------------------------------------------------------
c		            b. Detailed output
c
          write(nlog,280) ' JMStore    ',
     1       iyrmo(mon),xmonam(mon), idy, csour1, csour2, cresid1,
     1        iacct, iwx, iw, n, ns, ndr, irowd, nopr, imcdX, 
     1        pct,  pctS*100., pctS2*100.,
     1        availX*fac, availG1*fac,  availG*fac,  availD*fac,
     1        availR*fac, curown1, curown2, divact1*fac,availX2*fac, 
     1        divact2*fac, divact*fac, divactT*fac, iwhy, cwhy   
     
        endif       
      
c
c ---------------------------------------------------------
c 						  Step 25; Return  
c 
  500 RETURN
c
c _________________________________________________________
c
c                Formats
c
  270  format(/, 
     1  ' JmStore (Type 53); Operation Right ID = ', a12,
     1  ' Destination Type = ', a12,
     1  ' Carrier (Y/N) = ',a3, ' Associated Plan (Y/N) = ', a3,/  
     1  ' JMStore      iyr  mon  idy',
     1  ' Source_1    Source_2    Dest_1      ',
     1  '   iacct     iwx      iw       n      ns     ndr',
     1  '   irowd    nopr   imcdX     pct    pctS   pctS2',
     1  '  AvailX AvailG1  AvailG  AvailD  AvailR curown1',
     1  ' curown2 divact1 AvailX2 divact2  divact divactT iwhy cwhy',/
     1  '____________ ____ ____ ____',
     1  3(' ___________'), ' ',
     1  9(' _______'), 15(' _______'),' ____', 1x, 48('_'))
     
  280  FORMAT(a12, i5,1x,a4,i5, 1x,3a12, 9i8, 15f8.0, i5,1x, a48)
  
  290  format(/,'JMStore; Detailed results of avail where idcd = ',i5,/
     1 '  iyr  mon  idy    # River ID    Riv Name                ',
     1 '  avtemp  divert   avail',/
     1 ' ____ ____ ____ ____ ___________ ________________________',
     1 ' _______ _______ _______')  
        
  292  format(i5,1x,a4,i5, i5,1x,a12, a24, 20f8.0)
                 
c
c _________________________________________________________
c
c              Error warnings
c
 9999  write(nlog,270) corid(l2),cdestyp, ccarry,cTandC
 
          write(nlog,280) ' JMStore    ',
     1       iyrmo(mon),xmonam(mon), idy, csour1, csour2, cresid1,
     1        iacct, iwx, iw, ns, ndr, irowd, nopr, imcdX,
     1        pct, pctS*100., pctS2*100., 
     1        AvailX*fac, AvailG1*fac,  AvailG*fac,  AvailD*fac,
     1        AvailR*fac, curown1, curown2,  divact1*fac, availX2*fac, 
     1        divact2*fac, divact*fac, divactT*fac iwhy, cwhy
      
c    
      write(6,340) 
      write(nlog,350) 
      call flush(6)
 340  format('    Stopped in JMStore',/,
     1       '    See the *.log file')
 350  format('    Stopped in jMStore')
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop 
      END

