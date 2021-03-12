c setplano - ties a plan to an operating rule for plan reporting
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
        Subroutine SetPlanO
c		
c	It ties a plan to an operating rule for Plan Reporting
c	Called by Execut.f
c
c _________________________________________________________
c
c
c       Update History
c
c rrb 2019/11/17; Allow multiple type 12 (release limit) plans 
c
c rrb 2019/05/26; Revise to allow a multi-split (DivMulti) (opr 46)
c                 operating rule to have a source 1 equal a WWSP
c                 Supply Plan (type 14) or WWSP User Plan (type 15)
c
c rrb 2019/04/29; Corrected plan type 11 or 13 and operating rule
c                   46 (see step 3d)
c rrb 2019/04/27; Corrected MaxplnU to equal maxP1
c rrb 2019/04/20; Revised to recognize a WWSP Supply plan is a
c                   type 14 and a WWSP User Plan is a type 15
c rrb 2008/01/02; Removed from Oprinp
c
c _________________________________________________________
c
c	Documentation
c	      iplnoprE(np,iop) = Evap Opr rule for a plan(np) & output(iop) 
c	      iplnoprS(np,iop) = Source Opr rule for a plan(np) & output(iop)
c       iplnoprR(np,iop) = Res Re-Diversion Opr rule for plan(np) 
c                          & output(iop)
c       iplnoprO(np,iop) = OOP Opr rule for a plan(np) & output(iop) 
c       iplnoprU(np,iop) = Use Opr rule for a plan(np) & output(iop) 
c       iplnoprP(np,iop) = Multiple Use Plan for plan(np) & output(iop) 
c
c       maxplnU          = maximum number of outputs for any plan
c
c       iplntyp	Plan type=  1 for T&C,              2 for Well_Aug, 
c                        =  3 Reuse_Reservoir,      4 Reuse_Diversion, 
c                        =  5 Reuse_Reservoir_Tmn,  6 Reuse_Diversion_Tmn
c                        =  7 TransMtn Import       8 Recharge 
c                        =  9 Out-of-Priority      10 Special Augmentation
c                        = 11 Accounting Plan      12 Release_Limit
c                        = 13 Changed Water Right  14 WWSP Supply 
c                        = 15 WWSP User
c
c
c _________________________________________________________
c
      include 'common.inc'

      real*8 rtem
c
      character*12 cidvri,  ciopde,  ciopso1, ciopso2, ciopso3,
     1             ciopso4, ciopso5, blank,   czero,  cx
      character recin*256, creuse*12, rec12*12, rec2*2,
     1          csource*12, cdest*12, rec72*72, rec1*1

c
c _________________________________________________________
c		Step 0; Print Subroutine Name      
      write(nlog,102)
c     write(6,102)      
  102   format(/,72('_'),/,
     1 '  SetPlanO; Set Plan to Operational Rule Data')
c
c _________________________________________________________
c		            Step 1; Initialize Plan to Operating Rule ties
c
c               iout   = 0 detailed output
c                        2 summary of results
c               iout12 = 0 no details when plan type = 12
c                        1 details when plan type = 12
      iout=0
      iout12=0  
      maxPlnU=0
      
      do np=1,nplan
c
c 2009/06/09; correction
cx      do k=1,maxplnO
        do k=1,maxopr
          iplnoprE(np,k)=0
          iplnoprS(np,k)=0
          iplnoprR(np,k)=0
          iplnoprO(np,k)=0
          iplnoprU(np,k)=0            
          iplnoprP(np,k)=0            
        end do
      end do
c
c _________________________________________________________
c		            Step 2; Plan Loop
        
      do np=1,nplan
        iop=0
c
c rrb; 2018/08/28; WWSP Reporting Allow columns 1-10 to be sources and
c                  columns 11-20 to be uses
        iopX=10
        iopS=0
        iP1=iPlnTyp(np) 
c
        maxP1=0
c
        if(iout.eq.1) then
          write(nlog,*) ' SetPlanO;',  np, pid(np), iP1
        endif 
c
c ---------------------------------------------------------
c		Identify source 1 a Current priority diversion
c		  for well augmentation iplntyp=2
c
c		            Set Source Type for a T&C Plan (1), Well Aug Plan (2), 
c		            or Special Plan (10)
        if(iP1.eq.1 .or. iP1.eq.2 .or. iP1.eq.10) then
          iop=iop+1
          iplnoprS(np,iop)=-1
        endif            
c
c _________________________________________________________
c		            Step 3; Operating Rule Loop
        do k=1,numopr       
c
c ---------------------------------------------------------
c	
c               Step 3a; PLAN IS a 3 or 5 REUSE 
C                        Set Plan Evaporation for a Res Reuse Plan (3) 
c	                       or Tmtn Res Reuse Plan (5)
c                
c	                Operating rule type 24 or 25
c	                Plan Evaporation associated with a Plan (ireuse) and 
c                 a reservoir destination (iopdes(1,k)<0
          if(iP1.eq.3 .or. iP1.eq.5) then      
            if(ireuse(k).eq.np .and. iopdes(1,k).lt.0) then
c
c rrb 2006/08/09; Store by opr rule              
cr            iplnoprE(np)=k
              iop=iop+1
              iplnoprE(np,iop)=k
              maxP1=maxP1+1
c              
              if(iout.eq.1) write(nlog,300) np, k
            endif  
          endif
c
c --------------------------------------------------------- 
c               Step 3b; Plan is a type 1, 2 or 10 (destination or
c                        secondary destination) & various opr rules
c               PLAN IS A DESTINATION AND TYPE 1, 2 OR 10
c		            Set Source when the destination is a T&C (1) or 
c		              Well Aug Plan (2) or Special Plan (10)
c		              Note do not include Accounting Plan (type 11)
c		              It diverts water
        if(iP1.eq.1 .or. iP1.eq.2 .or. iP1.eq.10) then
          if(ciopdeX(1,k).eq.Pid(np)) then               
            iop=iop+1
            iplnoprS(np,iop)=k
            maxP1=maxP1+1                
          endif 
c
c		          Add secondary destination               
          if(ciopdeX2(k).eq.Pid(np)) then               
            iop=iop+1
            iplnoprS(np,iop)=k
            maxP1=maxP1+1                
          endif 
        endif
c
c ---------------------------------------------------------
c               Step 3c; PLAN IS a 3 or 8 SOURCE 1 and various opr rules
c
c		            Set Use when Source 1 is a Res Reuse Plan (3-6), 
c		              Tmtn Plan (7), or Recharge Plan (8)
c
        if(iP1.ge.3 .and. iP1.le.8) then
            if(ciopsoX(1,k).eq.Pid(np)) then
              iop=iop+1
              iplnoprU(np,iop)=k
              maxP1=maxP1+1                
            endif                
        endif  
c
c ---------------------------------------------------------
c               Step 3d; PLAN IS type 11 or 13 and various opr rules
c                        TYPE 11 (ACCOUNTING) OR 
C                        TYPE 13 (CHANGED WATER RIGHT)
c rrb 2008/01/02; Set Use when Source 1 is an Accounting Plan (11)
c rrb 2015/03/07; Allow a Changed Water Right Plan (type 13)
cx        if(iP1.eq.11) then  
        if(iP1.eq.11 .or. iP1.eq.13) then
c
c rrb 2019/04/27; Correction based on Version 15
          if(ityopr(k).ne.46) then
          
            if(ciopsoX(1,k).eq.Pid(np)) then
              iop=iop+1
              iplnoprU(np,iop)=k
              maxP1=maxP1+1                
              ndP=0
c            
              if(iout.eq.1)write(nlog,310) np, iop, ndP, pid(np)  
            endif

          endif 
c
c rrb 2019/04/27; Add the following that got deleted when WWSP Supply 
c                 (type 14) and WWSP User (type 15) were added
c rrb 2008/01/02; Add for Type 46 (Multiple Sources) 
          if(ityopr(k).eq.46) then                  
            ndes=ioprlim(k)
            n1=0
            n2=0
c
c rrb 2019/04/29; If the source is this plan, initialize every destination
            if(ciopsoX(1,k).eq.Pid(np)) then
              do i=1,ndes   
                n1=n2+1
                n2=n1+1
              
cx                write(nlog,*) ' SetPlanO_13c; i, ciopdex(n1,k), pid(np)'
cx                write(nlog,*) ' SetPlanO_13c;', i, ciopdex(n1,k), pid(np)
c                  
                  npD=iopdes(n1,k)
                  iop=iop+1
                  iplnoprU(np,iop)=k
                  iplnoprP(np,iop)=npD
                  maxP1=maxP1+1              
c              
                  if(iout.eq.1) then
                    write(nlog,*) ' SetPlanO; np, k, iop ', np, k, iop
                    write(nlog,*) ' SepPlanO;',
     1                            iplnoprU(np,iop), iplnoprP(np,iop)
                    write(nlog,320) 
     1              np, iop, ndP, pid(np), pid(npD)  
                  endif 
              end do    
            endif
c
c               Endif for opr type 46   
          endif
c
c               Endif for iP1 = 11 or 13   
        endif
c
c
c ---------------------------------------------------------
c               Step 3e; PLAN IS type 14 (WWSP Supply) & OPR RULE 29
C rrb 2018/10/19; Update for type 29 WWSP Spill
c rrb 2019/04/20; Recognize type 14 is WWSP Supply and 15 is WWSP User
        if(iP1.eq.14 .and. ityopr(k).eq.29) then
          if(ciopsoX(1,k).eq.Pid(np)) then
            iop=iop+1
            iplnoprU(np,iop)=k
            maxP1=maxP1+1                
            ndP=0
c         
            if(iout.eq.1)write(nlog,310) np, iop, ndP, pid(np)  
          endif
        endif
c
c
c ---------------------------------------------------------
c               Step 3f; Plan is type 15 (WWSP User) and opr is 29 (spill)
cc rrb 2019/04/20; Revised to recognize a WWSP Supply plan is a
c                   type 14 and a WWSP User Plan is a type 15

        if(iP1.eq.15 .and. ityopr(k).eq.29) then
          if(ciopsoX(1,k).eq.Pid(np)) then
            iop=iop+1
            iplnoprU(np,iop)=k
            maxP1=maxP1+1                
            ndP=0
c         
            if(iout.eq.1)write(nlog,310) np, iop, ndP, pid(np)  
          endif
        endif

c
c ---------------------------------------------------------
c               Step 3g; PLAN IS type 14 (WWSP Supply), source 1 and
c                        OPR RULE TYPE 46
c rrb 2008/01/02; Add for Type 46 (Multiple Sources)
c                 This section sets water delivered to a destination by a 
c                 type 46 (split operating rule) or type 53 (WWSP Plan)
c                 This takes care of WWSP type 14 plan
c                 uses by operating rule 46 (supply)
c rrb 2018/09/11; Correction
c rrb 2018/09/22; Add WWSP Supply Plan (type 14)
c rrb 2019/05/26; Add WWSP User Plan (type 15)
c
cx      if(iP1.eq.14) then
        if(iP1.eq.14 .or. ip1.eq.15) then
          if(iout.eq.1) then
            write(nlog,*) ' SetPlanO; np, k, ip1, ityopr(k)',
     1        np, k,ip1, ityopr(k)
          endif   

          if(ityopr(k).eq.46) then
            if(ciopsoX(1,k).eq.Pid(np)) then                             
              ndes=ioprlim(k)
              n1=0
              n2=0
              do i=1,ndes                
           
                n1=n2+1
                n2=n1+1
                
                npD=iopdes(n1,k)
                iop=iop+1
                
                iplnoprU(np,iop)=k
                iplnoprP(np,iop)=npD                 
                maxP1=maxP1+1              
c
                if(iout.eq.1) then
                  write(nlog,320)np, iop, npD, pid(np), pid(npD)
                endif   
              end do  
c
c             Endif for operating rule with source 1 = a type 14 plan
            endif  
c
c             Endif for Opr type type 46
          endif 
c
c rrb 2019/04/20; Keep type 14 or ro 15 and opr rule 46 separate
c               Endif for Type 14
        endif         
c
c ---------------------------------------------------------
c               Step 3h; PLAN IS type 14 (WWSP Supply), source 1 and
c                        OPR RULE 52 
c                        This takes care of WWSP type 14 plan
c                        uses by operating rule 52 (destination)
c rrb 2018/12/08; Allow a type 52 to be associated with a type 14 plan
c                   WWSP Plan (type 14) and operating rule type 52 
c                   Split Source to Users

c rrb 2019/04/20; Keep type 14 and opr rule 52 separate
        if(iP1.eq.14) then
          if(iout.eq.1) then
            write(nlog,*) ' SetPlanO; np, k, ip1, ityopr(k)',
     1        np, k,ip1, ityopr(k)
          endif   
c
          if(ityopr(k).eq.52) then
            ndes=ioprlim(k)
            n1=0
            n2=0
            do i=1,ndes                        
              n1=n2+1
              n2=n1+1                  
              if(ciopdeX(n1,k).eq.Pid(np)) then                             
               
                npD=ioppln(n1,k)
                iop=iop+1
                
                iplnoprU(np,iop)=k
                iplnoprP(np,iop)=npD               
                maxP1=maxP1+1              
c             
                if(iout.eq.1) then
                  write(nlog,320)np, iop, ndP, pid(np), pid(npD)
                endif 
              endif
            end do  

c             Endif for Opr type type 52
          endif   
c
c rrb 2019/04/20; Keep type 14 and opr rule 52 separate
c               Endif for Type 14
        endif
c          
c
c ---------------------------------------------------------
c
c               Step 3i; PLAN IS type 15 (WWSP User), source 5 and
c                        Opr Rule 45
c rrb 2018/09/23; Allow a WWSP-Supply by a type 45 direct diversion
c rrb 2019/04/20; Keep WWSP-User (type 15), creuse and opr rule 45
c                 separate
cx      if(iP1.eq.14) then
        if(iP1.eq.15) then
c
          if(iout.eq.1) then
            write(nlog,*) ' SetPlanO; np, k, ip1, ityopr(k)',
     1        np, k,ip1, ityopr(k)
          endif   
c          
          if(ityopr(k).eq.45) then
c             Identify a type 45 with a WWSP User plan as 
c               variable ciopsoX5(k) that diverts to irrigate
c
            if(ciopsoX5(k).eq.Pid(np)) then                        
              iop=iop+1
              iplnoprU(np,iop)=k
              maxP1=maxP1+1                
              ndP=0              
cx              write(nlog,*) 'SetPlano; ',
cx     1          ityopr(k), np, iop, ndP, pid(np)  
c
c             Endif for CiopsoX5
            endif
c
c             Endif for Opr type type 45
          endif
c
c             Endif for plan type 15 (WWSP User)                       
        endif  
c
c ---------------------------------------------------------
c rrb 2019/04/27; 
c               Step 3j; PLAN IS type 14 (WWSP Supply) variable creuse
c                        Opr Rule 45
c rrb 2019/04/27; Note no reason to add to WWSP Supply because
c                 sources are not reported in *.xpl
cx        if(iP1.eq.14) then
cx          if(ityopr(k).eq.45) then
cx            if(creuseX(k).eq.Pid(np)) then   
cx                        
cx              iop=iop+1
cx              iplnoprU(np,iop)=k
cx              maxP1=maxP1+1                
cx              ndP=0              
cxcx              write(nlog,*) 'SetPlano; ',
cxcx     1          ityopr(k), np, iop, ndP, pid(np)  
cx            endif
cxc
cxc             Endif for Opr type type 45
cx          endif
cxc
cxc             Endif for plan type 14 (WWSP Supply)                       
cx        endif            
c
c ---------------------------------------------------------
c               Step 3k; PLAN IS A type 3, 4, 5, 6, 7, 8, or 15 source 2
c                 This takes care of WWSP User Plan (type 15)
c                 used by operating rules 27 (exchange), 28 (bypass)
C                 29 (spill), 29 (reset) and 34 (bookover with plan)
cx        if(iP1.ge.3 .and. iP1.lt.9) then  
c
c rrb 2019/04/20; Recognize WWSP Source (type 14) and WWSP User (type 15)   
cx        if(iP1.eq.3 .or. iP1.eq.4 .or. iP1.eq.5 .or.
cx   1       ip1.eq.6 .or. iP1.eq.7 .or. iP1.eq.8 .or.
cx   1       ip1.eq.14) then  
        if(iP1.eq.3 .or. iP1.eq.4 .or. iP1.eq.5 .or.
     1     ip1.eq.6 .or. iP1.eq.7 .or. iP1.eq.8 .or.
     1     ip1.eq.15) then  
c
c rrb 2007/12/06; Correction            
c         if(ciopsoX(1,k).eq.Pid(np)) then             
          if(ciopsoX2(k).eq.Pid(np)) then             
            iop=iop+1
            iplnoprU(np,iop)=k
            maxP1=maxP1+1
                            
            if(iout.eq.1) then
              write(nlog,*)' SetPlanO; np, k, ciopsoX2(k)'
              write(nlog,*)' SetPlanO;', np, k, ciopsoX2(k)
            endif  
          endif
        endif  
c
c ---------------------------------------------------------
c               Step 3l; PLAN IS A TYPE 9 (OOP), Creuse FOR OPERATING 
C               RULES 8 (OOP Bookover) & 27 (res or plan direct)
c
        if(iP1.eq.9) then
c		          For an OOP Plan and opr rule type 8 (OOP BOOKOVER) or 27
          if(ityopr(k).eq.8 .or. ityopr(k).eq.27) then  
            if(creuseX(k).eq.Pid(np)) then             
              iop=iop+1
              iplnoprO(np,iop)=k
              maxP1=maxP1+1                
              if(iout.eq.1) then
                write(nlog,*)' SetPlanO; np, k, ireuse(k)'
                write(nlog,*)' SetPlanO;', np, k, ireuse(k)
              endif  
            endif
          endif  
c
c ---------------------------------------------------------
c               Step 3m; PLAN IS A Type 9 SOURCE 2 FOR OPERATING RULE 34
c rrb 2006/10/03; For an OOP Plan and a Type 34              
          if(ityopr(k).eq.34) then  
            if(ciopsoX2(k).eq.Pid(np)) then             
              iop=iop+1
              iplnoprO(np,iop)=k
              maxP1=maxP1+1                
              if(iout.eq.1) then
                write(nlog,*)' SetPlanO; np, k, ciopsoX2(k)'
                write(nlog,*)' SetPlanO;', np, k, ciopsoX2(k)
              endif  
            endif
          endif  
              
c
c ---------------------------------------------------------
c               Step 3n; PLAN IS type 9, SOURCE 1 FOR OPERATING RULE 42
c rrb 2006/10/02; For an OOP plan and Type 42              
          if(ityopr(k).eq.42) then  
            if(ciopsoX(1,k).eq.Pid(np)) then             
              iop=iop+1
              iplnoprO(np,iop)=k
              maxP1=maxP1+1                
              if(iout.eq.1) then
                write(nlog,*)' SetPlanO; np, k, ireuse(k)'
                write(nlog,*)' SetPlanO;', np, k, ireuse(k)
              endif  
            endif
c
c           Endif for plan type 9
          endif  
c
c ---------------------------------------------------------
c               Step 3o; PLAN IS type 9, MULTIPLE DESTINATIONS FOR 
c                        OPR RULE 41
c rrb 2006/10/06;	For an OOP plan and type 41, tie to many plans                
          if(ityopr(k).eq.41) then  
            do i=1,10
              npx=intern(k,i)
              if(npx.eq.np) then                
                iop=iop+1
                iplnoprO(np,iop)=k
                maxP1=maxP1+1                
                if(iout.eq.1) then
                  write(nlog,*)' SetPlanO; Type 41 np,k,intern(k)'
                  write(nlog,*)' SetPlanO; Type 41 ',np,k,ireuse(k)
                endif  
              endif  
            end do
          endif  
c              
c ---------------------------------------------------------               
c		Endif for iP1.eq.9
        endif  
c
c ---------------------------------------------------------
c               Step 3p; PLAN IS SOURCE 2, FOR A Opr Rule 30 
c                        (WELL AUGMENTATION) 
c		Reservoir Re-Diversion (Type 30 and Source 2 is a plan) 
        if(iP1.le.2) then
          if(ciopsoX2(k).eq.Pid(np) .and. ityopr(k).eq.30) then
            iop=iop+1
            iplnoprR(np,iop)=k                
            maxP1=maxP1+1                
          endif
        endif
c
c ---------------------------------------------------------
c               Step 3q; PLAN IS A Type 12, SOURCE 5 (RELEASE LIMIT) 
c                        for various operating rules
c		Release Limit (type 12)
        if(iP1.eq.12) then
          if(ciopsoX5(k).eq.Pid(np)) then         
            iop=iop+1
            iplnoprU(np,iop)=k
            maxP1=maxP1+1    
          endif
c
c rrb 2019/11/17; Allow multiple type 12 (release limit) plans 
          if(ciopsoX6(k).eq.Pid(np)) then
            iop=iop+1
            iplnoprU(np,iop)=k
            maxP1=maxP1+1    
          endif
c
c rrb 2019/11/17; test output
          if(iout12.eq.1) then
            write(nlog,*)' SetPlanO; ip1, k, np, Pid(np) iplnoprU'
            write(nlog,*)' SetPlanO;',ip1,k,np,Pid(np),iplnoprU(np,iop)
          endif  
        endif     
c
c ---------------------------------------------------------
c		          Step 4; Store the maximum number of uses for all plan            
c 
c rrb 2008/03/31; Adjust maximum  maxplnO (100)
c rrb 2009/06/09; Correction
cx            if(maxP1.gt.maxplnO) then
cx              write(nlog,900) maxPlnO
cx              goto 9999
cx            endif
cx            maxplnU=amax0(maxplnO,maxP1)     
       
                
          if(maxP1.gt.maxopr) then
            write(nlog,900) maxp1, maxPlnU, maxOpr
            goto 9999
          endif
c
c rrb 2019/04/27; Correction
cx        maxplnU=amax0(maxopr,maxP1)  
          maxplnU=amax0(maxplnU,maxP1)     
c
c ---------------------------------------------------------
c		End Operating rule loop           
        end do
c
c ---------------------------------------------------------
c		End plan loop          
      end do
c
c _________________________________________________________
c rrb 2019/04/27;
c		            Step 5; Detailed Output (same as available in Outpln.for)

      if(iout.ge.1) then
        do np=1,nplan
          write(nlog,400) np, pid(np), iplntyp(np), plntypC(np), maxplnU
          write(nlog,410) (i, i=1,60)
          
          write(nlog,420) 'iplnoprE', (iplnoprE(np,nop), nop=1,maxplnU)
          write(nlog,420) 'iplnoprS', (iplnoprS(np,nop), nop=1,maxplnU)
          write(nlog,420) 'iplnoprR', (iplnoprR(np,nop), nop=1,maxplnU)
          write(nlog,420) 'iplnoprO', (iplnoprO(np,nop), nop=1,maxplnU)
          write(nlog,420) 'iplnoprU', (iplnoprU(np,nop), nop=1,maxplnU)            
          write(nlog,420) 'iplnoprP', (iplnoprP(np,nop), nop=1,maxplnU) 
        end do
      endif        
   
 400  format(/,  
     1  'SetPlanO; Detailed output for Plan # = ', i5,
     1  ' ID = ', a12, ' Type = ',i5, ' Name = ', a25, 
     1  ' maxplnU = ', i5)
 410  format(/, 12x, 60('  ', i3),/
     1          12('_'), 60(' ____'))      
 420  format(a12, 100i5)
c
c _________________________________________________________
c		            Step 6; Return
        
      return
c
c _________________________________________________________
c		Formats

 300  format(/, 
     1 ' SetPlanO_1; np, k ', 2i5)

 310  format(/, 
     1 ' SetPlanO_1;   np  iop  ndP pid(np)',/
     1 ' SetPlanO_1;',3i5, 1x, a12)
        
 320  format(/, 
     1 '  SetPlanO_2;   np  iop  ndP pid(np)      pid(npD)',/   
     1 '  SetPlanO_2;',3i5, 2(1x,a12))
c      
c _________________________________________________________
c		Problems
     
 900  format(/,72('_'),/,
     1 '  SetPlanO; Problem number of plans to a use = ', i5,/
     1 '    that exceeds the dimension (maxOpr)      = ', 2i5,/, 
     1 '  Recommend you revise StateM and Common')

c      
c _________________________________________________________
c
c               Warn and Stop
      
 9999 write(6,1440) 
      write(nlog,1450) 
      call flush(6)
 1440 format(/, 72('_'),/,
     1 ' SetPlanO;  Stopped in SetPlanO, see the log file (*.log)')
 1450 format(/, 72('_'),/,' SetPlanO; Stopped in SetPlanO')
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)
      
      stop
      end
