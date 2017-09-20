c
c ---------------------------------------------------------
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
c		Step 1; Initilize Plan to Operating Rule ties
c
      iout=0
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
c		Step 2; Plan Loop
        
      do np=1,nplan
        iop=0
        iopS=0
        iP1=iPlnTyp(np)          
        maxP1=0
          
c
c ---------------------------------------------------------
c		Identify source 1 a Current priority diversion
c		  for well augmentation iplntyp=2
c
c		Set Source Type for a T&C Plan (1), Well Aug Plan (2), 
c		  or Special Plan (10)
        if(iP1.eq.1 .or. iP1.eq.2 .or. iP1.eq.10) then
          iop=iop+1
          iplnoprS(np,iop)=-1
        endif            
c
c _________________________________________________________
c		Step 3; Operating Rule Loop
        do k=1,numopr
c
c ---------------------------------------------------------
c	 Set Plan Evaporation for a Res Reuse Plan (3) or
c	   Tmtn Res Reuse Plan (5)
c
c	  Operating rule type 24 or 25
c	  Plan Evaporation associated with a Plan (ireuse) and 
c   a reservoir destination (iopdes(1,k)<0
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
c		Set Source when the destination is a T&C (1) or 
c		  Well Aug Plan (2) or Special Plan (10)
c		  Note do not include Accounting Plan (type 11)
c		  It diverts water
          if(iP1.eq.1 .or. iP1.eq.2 .or. iP1.eq.10) then
            if(ciopdeX(1,k).eq.Pid(np)) then               
              iop=iop+1
              iplnoprS(np,iop)=k
              maxP1=maxP1+1                
            endif 
c
c		Add secondary destination               
            if(ciopdeX2(k).eq.Pid(np)) then               
              iop=iop+1
              iplnoprS(np,iop)=k
              maxP1=maxP1+1                
            endif 
          endif
c
c ---------------------------------------------------------
c		Set Use when Source 1 is a Res Reuse Plan (3-6), 
c		  Tmtn Plan (7), or Recharge Plan (8)
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
c rrb 2008/01/02; Set Use when Source 1 is an Accounting Plan (11)
c                 or Changed Water Right Plan (13)
c rrb 2015/03/07; Allow a Changed Water Right Plan (type 13)
cx        if(iP1.eq.11) then  
          if(iP1.eq.11 .or. iP1.eq.13) then
          if(iout.eq.1) write(nlog,*) '  SetPlanO', ip1, np, pid(np)
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
cxc                                                     
cxc rrb 2011/01/06; Add for Type 40 (South Platte compact)
cxc                 The destination is the plan ID
cx            if(ityopr(k).eq.40) then                  
cx              if(ciopsoX(1,k).eq.Pid(np)) then
cx                iop=iop+1
cx                iplnoprU(np,iop)=k
cx                maxP1=maxP1+1                
cx                ndP=0
cxc              
cx                if(iout.eq.1) write(nlog,310) np, iop, ndP, pid(np)   
cx              endif
cx            endif                 
c
c rrb 2008/01/02; Add for Type 46 (Multiple Sources) 
            if(ityopr(k).eq.46) then                  
              ndes=ioprlim(k)
              n1=0
              n2=0
              do i=1,ndes                
                if(ciopsoX(i,k).eq.Pid(np)) then
                  n1=n2+1
                  n2=n1+1
                  
                  npD=iopdes(n1,k)
                  iop=iop+1
                  iplnoprU(np,iop)=k
                  iplnoprP(np,iop)=npD
                  maxP1=maxP1+1              
c              
                  if(iout.eq.1) write(nlog,320) 
     1              np, iop, ndP, pid(np), pid(npD)   
                endif
              end do  
            endif     
          endif  
c
c ---------------------------------------------------------
c		Source 2 is a Plan, Set Use
          if(iP1.ge.3 .and. iP1.lt.9) then  
c
c rrb 2007/12/06; Correction            
c           if(ciopsoX(1,k).eq.Pid(np)) then             
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
c		Plan ID is associated with an OOP Reservoir
c
          if(iP1.eq.9) then
c
c ---------------------------------------------------------
c		For an OOP Plan and type 8 or 27
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
            endif  
c
c ---------------------------------------------------------
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
c		Release Limit (type 12)
          if(iP1.eq.12) then
            if(ciopsoX5(k).eq.Pid(np)) then
              iop=iop+1
              iplnoprU(np,iop)=k
              maxP1=maxP1+1                
            endif
          endif            
c
c ---------------------------------------------------------
c		Store the maximum number of uses for a plan            
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
            maxplnU=amax0(maxopr,maxP1)       
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
c		Step ; Return
        
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
     1 ' SetPlanO_2;   np  iop  ndP pid(np)      pid(npD)',/   
     1 ' SetPlanO_2;',3i5, 2(1x,a12))
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
