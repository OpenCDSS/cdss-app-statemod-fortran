c
C
      SUBROUTINE PlanEva
c
c
c _________________________________________________________
c	Program Description
c
c       PlanEva; It distributes reservoir evaporation to a 
c                reservoir plan
c
c               Called by: Execut
c
c _________________________________________________________
c	Update History
c
c rrb 2006/04/05; Corrected a problem associated with assigning
c                   the correct reservoir ID and
c		  Revised common and bomsec to initilize
c                   iplnopr() that ties a plan to an operating rule
c _________________________________________________________
c       Documentation               
c
c       curown(ira)     Storage in account ira (ac-ft)
c
c       accr(23,ira)    Evaporation from account ira (ac-ft)
c
c	psuply(np)      Plan supply (cfs)
c	pevap(np)	Plan evaporation (cfs)
c
c	iplnopr(np)     Ties a operating rule to a plan
c	        	Defined 1x/run in Bomsec
c       iout            0 no detailed printout
c                       n print detailed infor for reservoir n
c
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'                      
c                           
c _________________________________________________________
c
c               Step 1; Initilize
      iout = 0

      small=0.001
c
c               b. Daily capability
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif
c                           
c _________________________________________________________
c
c   		Step 2; Allocate res evaporation to plan types 3 
c                       and 5 where Type 3=Reuse_Reservoir and 
c                       Type 5=Reuse_Reservoir_Tmtn   
c
c rrb 2006/08/09; Set code to only let evap be calculated 1x per plan   
      do 100 np=1,nplan
        if(Pon(np).eq.0) goto 100
        if(iplnTyp(np).eq.3 .or. iplntyp(np).eq.5) then
          do kx=1,numopr
            k=iplnoprE(np,kx)
            if(k.gt.0) then
c                           
c _________________________________________________________
c
c 		Step 3; Find tie between plan reservoir in 
c                       operating rule info
c		        Note ireuse(k) indicates reuse and 
c                       iopdes<0 indicates a reservoir
              ir=0
c
c ---------------------------------------------------------
c		Operating Rule type 24, 25, 27 & 28
c		Destination is a reservoir and the Reuse ID
c               is a Reservoir Plan
c
c rrb 2006/08/09; Add type 27 and 28
              if(ityopr(k).eq.24 .or. ityopr(k).eq.25 .or.
     1           ityopr(k).eq.27 .or. ityopr(k).eq.28) then          
                irX=iopdes(1,k)
                if(irX.ne.0) then
                  ir=-1*irX
                  ira=NOWNER(ir)+IOPDES(2,k)-1
                endif  
              endif  
c
c ---------------------------------------------------------
c rrb 2006/05/30; Operating Rule type 32 or type 33
c                 Include reservoir evap when a source.   
c		  Note source 1 is a reservoir and 
c		  source 2 is a reservoir plan       
c
c rrb 2006/08/09; Add type 26
              if(ityopr(k).eq.32 .or. ityopr(k).eq.33 .or.
     1           ityopr(k).eq.26) then          
                irX=iopsou(1,k)
                if(irX.ne.0) then
                  ir=irX
                  ira=NOWNER(ir)+IOPSOU(2,k)-1
                endif
              endif  
c
c ---------------------------------------------------------
c rrb 2006/05/30; Operating Rule type 38
c                 Include reservoir evap when a source.   
c		  Note source 1 is a reservoir and 
c		  source 2 is a reservoir plan       
              if(ityopr(k).eq.38) then          
                irX=iopdes(1,k)
                if(irX.ne.0) then
                  ir=-1*irX
                  ira=NOWNER(ir)+IOPSOU(2,k)-1
                endif
              endif  
c
c ---------------------------------------------------------
c		Stop if the source is not a type 27 and a 
c		reservoir was not found
c
              if(ir.eq.0) then
                goto 100
              else
                if(ir.eq.0) then
                  write(nlog,220) corid(k), ityopr(k), pid(np), ir
                  goto 9999
                endif
              endif
c
c rrb 2006/04/04; Correction              
cr            ira=iopdes(2,k)
cr            ira=NOWNER(ir)+IOPSOU(2,k)-1
c                           
c _________________________________________________________
c		Step 4; Initilze
c		Note  evap and Psuply are in ac-ft
c                    +evap= more evap than rain
c                    -evap= more rain than evap

              Evap0=abs(accr(24,ira))
              Evap1=accr(24,ira)
           
              Psuply1=psto2(np)
              Psuply2=Psto2(np)
              curown1=curown(ira)
c                       
c ---------------------------------------------------------
c		5a; Net evap is zero              
              if(evap0.lt.small) then              
                Pevap1=0.0
                goto 110
              endif
c                           
c ---------------------------------------------------------
c		5b; Case 2 account is empty              
              if(curown(ira).lt.small) then
c
c rrb 2007/12/27; Correction              
c               Pevap1=amax1(Psuply1-Evap1, 0.0)
                Pevap1=amin1(Psuply2, Evap1)
                Pevap1=amax1(Pevap1,0.0)
                Psuply2=amax1(Psuply1-Pevap1, 0.0)
                goto 110
              endif
c        
c ---------------------------------------------------------                   
c		5c; Prorate based on ownership 
c		    Note reservoir has already evaporated so
c		    add evap to storage to get proper ratio
              curown2=curown(ira)
              curown2=curown(ira) + Evap1                        
              if(curown2.gt.small) then
                c = amin1(Psuply1/curown2, 1.0)              
                Pevap1 = accr(24,ira)*c
                Psuply2 = amax1(Psuply2 - Pevap1, 0.0)
                goto 110
              endif
c                                           
c _________________________________________________________
c
c		Step 6; Detailed output   
c
 110          if(iout.eq.1) then
                write(nlog,200)
                write(nlog,210)      
     1            iyrmo(mon),xmonam(mon),idy, Pid(np),
     1            np, k, ir, ira, 
     1            curown1, Psuply1, evap1, pevap1, Psuply2
              endif
c                           
c _________________________________________________________
c
c		Step 7; Update
c                   Note change units to cfs (except storage)             
c
c rrb 2007/12/27; move from below
              Pevap(np)=Pevap1/fac
              Psuply(np)=Psuply(np)- Pevap(np)
              Psto2(np)=Psuply2
c
c rrb 2005/05/30; Correction; Move below do after storage calculations          
cx            Pevap(np)=Pevap1/fac
            endif 
c          
c		End operating rule LOOP
          end do           
c          
c		End reuse and reservoir IF
        endif
c
c		End plan LOOP        
 100  continue
c
c _________________________________________________________
c
c		Step 8; Return      
      return
c
c _________________________________________________________
c
c               Error Messages
c
c
c
c _________________________________________________________
c
c               Formats
 200  format(/, 72('_'),/ '  PlanEva;',/
 
 
     1 ' Year  Mon  Day Plan ID     ',
     1 '   np    k   ir  ira curown1 Psuply1   evap1   Pevap Psuply2',/
     1 ' ____ ____ ____ ____________',
     1 ' ____ ____ ____ ____ _______ _______ _______ _______ _______')
 210  format(i5, 2x, a3, i5, 1x, a12, 4i5, 20f8.1)    
 220  format(/,72('_'),/
     1 ' PlanEva;',/
     1 'Problem with Operating right ID = ', a12, ' Type = ',i2,/      
     1 '          Reservoir plan ',a12,
     1 '          it is not tied to a reservor, nr = ', i5,/
     1 '          Reconmend you revise the plan type or ',/
     1 '          turn the plan off if not used')
 
c
c _________________________________________________________
c
c               Step 16; Error Processing
c
 9999 write(6,*) ' Stopped in PlanEva, see the log file (*.log)'
      write(io99,*)' Stopped in PlanEva'
      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)


      stop 
      END




