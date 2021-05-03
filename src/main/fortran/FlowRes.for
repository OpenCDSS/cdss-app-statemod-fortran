c FlowRes - Type 51 Flow Reservoir Control.  
c
c           Initially developed for the Trinidad project 
c           operation in Colorado's Arkansas River Basin
c           but may have generic applications.
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
C     Last change:  
c
c ---------------------------------------------------------
      subroutine FlowRes(iw,l2,iflow,ncallX)
c
c _________________________________________________________
c	Program Description
c
c     FlowRes;	  Type 51 Flow Reservoir Control.  
c
c                 Initially developed for the Trinidad project 
c                 operation in Colorado's Arkansas River Basin
c                 but may have generic applications.  The subroutine
c                 does the following:
c
c                 1) If the operating is active this time step,
c                     this call (limited to 1 per iteration,  etc.)
c                     Initializes the control to be on (iflow=1)
c                 2) Search all reservoir accounts to 
c                    determine if any have 0 storage 
c                 3) If they all have storage exit with iflow=1 (ON)
c                 4) If one has no storage, exits with iflow=0 (OFF)
c
               
c                 Note:
c                   iflow is initialized in Execut to be zero
c                   iflow is only set herein when the operating rule
c                     is active this time step,
c                 
c           	    Called by Execute 
c _________________________________________________________
c
c     Update history
c
c rrb 2021/04/18; Compiler warning
c
c rrb 2018/07/15; Created by editing DivResP2
c ________________________________________________________
c	Documentation
c
c        iflow          = 1 Project is operating, for Trinidad all 
c                           account storages are > 0
c                         0 Project is not operating
c                         
c
c	       iopdesr(l2)    = 3 destination is a diversion
c	        		          = 2 destination is a reservoir
c	        		          = 7 destination is a plan
c       
c        iopdes(1,l2)   nd
c	                     	if > 0 destination diversion ID 
c		                    if < 0 destination reservoir ID
c
c        iopdes(2,l2)   = destination owner 
c
c        iout           = switch for detailed printout 
c                      
c        imonsw()       = monthly on off switch  
c                      
c        ires           = switch 0=diversion, 1=plan
c                      
c        l2             = order of the destination diversion
c        l2             = order of operating rule. Note
c                         when called by execute (ityopr=2 or 3); l2=l2
c                         when called by replace (ityopr=10) l2 is a 
c                         array holder
c
c	       ndtype         = iopdesr(l2) Destination Type
c                         1 = 'ISF'
c		                      2 = 'Reservoir'
c            		          3 = 'Diversion'
c                         7 = 'Plan     '
c
c
c ---------------------------------------------------------                         	
c ________________________________________________________
c	Dimensions
c
      include 'common.inc'
      character cwhy*48,    corid1*12, cCallBy*12
      character cdestyp*12, ccarry*3,  cpuse*3,   cstaid1*12,
     1          rec12*12,   cTandC*3,  cidRiv*12, cidBal*12,
     1          cresid1*12, creplace*3,criver*3, subtypX*8
c      
c _________________________________________________________
c               
c               Step 1; Initialize
c
c ---------------------------------------------------------
c
c rrb 2021/04/18; Compiler warning
      irep=0
      ioutiw=0
      divact=0.0
      iprob=0
      rec12=' '
      cidbal=' '
      cidriv=' '
      cstaid1=' '
      if(iprob.gt.0) goto 9999
c
c               a. Diversion Type
      ndtype = iopdesr(l2)
c
c ---------------------------------------------------------
c
c      		      b. Miscellaneous
c   
      subtypX='flowres '
      iout=0
      ioutX=0
      
      corid1=corid(l2)
      iwhy=0
      cwhy='NA'
      cwhy='Project is operating'
            
      if(ioutX.eq.1) then
        write(nlog,*) ' '
        write(nlog,*) ' FlowRes'     
      endif 
c
c ---------------------------------------------------------
c
c               c. Detailed Report Control

      if(ichk.eq.151) iout=2
      
      if(irep.eq.0) then
        if(corid(l2).eq. ccall) ioutiw=iw
      else
        if(corid(l2).eq. ccall) ioutiw=iw
      endif 
c
c ---------------------------------------------------------
c               d. Detailed Output
      if(ioutX.eq.1) then    
        write(Nlog,*)
     1    ' FlowRes; ncallx    ichk    iout  ioutiw      iw',
     1    ' corid        ccall' 
        write(nlog,'(9x,5i8,2(1x,a12))')  
     1    ncallx, ichk, iout, ioutiw, iw, corid(l2), ccall
      endif
c      
      if(iout.eq.2 .and. ioutiw.eq.iw) then
        if(ncallX.eq.0 .and. iday.eq.0) then
          write(nlog,102) corid(l2)
 102      format(/, 72('_'),/ '  FlowRes; ID = ', a12)
        endif
      endif
   
c
c ---------------------------------------------------------
c               e. Misc
     
      small=0.001
      smallN=-1.*small
      Round=0.1
      big=99999.

      cCallBy='FlowRes    '
      
      iwhy=-1
      ccarry='No'
      cTandC='No'
      cpuse='No'
      cdestyp='NA'
      creplace='No'
      cRiver='No'
      
c      
c ---------------------------------------------------------
c
c               f. Daily capability
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif
c
c ---------------------------------------------------------
c               g. Check Avail array coming in
      if(ioutX.eq.1) then
        write(nlog,*) ' '
        write(nlog,*) ' ______________________________________'
        write(nlog,*) ' FlowRes; Calling Chekava In ', corid1
        write(nlog,*) ' '
      endif  
      call chekava(51, maxsta, numsta, avail, subtypX)
        
c
c _________________________________________________________
c               Step 2; Exit if not on this month
c
      if(imonsw(l2,mon).eq.0) then
        iwhy=1
        cwhy='Monthly Switch = zero'
        goto 300
      endif  
c
c ---------------------------------------------------------
c		For a daily model set demand for beginning of season
      if(iday.eq.1 .and. imonsw(l2,mon).gt.0) then
        if (idy.lt.imonsw(l2,mon)) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 300
        endif  
      endif  
c
c ---------------------------------------------------------
c		For a daily model set demand for end of season
      if(iday.eq.1 .and. imonsw(l2,mon).lt.0) then
        if (idy.gt.iabs(imonsw(l2,mon))) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 300
        endif  
      endif 
c
c ____________________________________________________
c               Step 3; Check reoperation control and exit
c                     if already operated once this time step
c rrb 2018/07/29 
       icallOP(l2)=icallOP(l2) + 1
c
       if(icallOP(l2).gt.1) then
         iwhy=1
         cwhy='Limited to one call per time step'
         goto 310
       endif            

c       
c _________________________________________________________
c               Step 4; Determine if any account has zero storage
c
c               b. Destination is a reservoir (Demand = Divalo)
      if(ndtype.eq.2) then
c        
c                  iflow = 1 Project is operating
c                          0 Project is not operating
c                            e.g. Trinidad all accounts are > 0
c
        iflow=1
      
        cdestyp='Reservoir   '
        ndes=int(oprlimit(l2))
c  
c ---------------------------------------------------------       
c				  	Loop for number of reservoir accounts
        n1=0
        n2=0
      
        do n=1,ndes
          n1=n2+1
          n2=n1+1
        
          nR  =Iopdes(n1,L2)
          nRa =Iopdes(n2,L2)          
          irow=nowner(nR)+nRa-1  
               
          cresid1=cresid(nR)
          cursto1=curown(irow)
           
          if(cursto1.lt.small) then
            iflow=0
            iwhy=3
            cwhy='Reservoir account storage is < 0'
          endif        
c         
c ---------------------------------------------------------       
c				  	Check results
          if(ioutX.eq.1) then
            write(nlog,*) ' FlowRes;   iyr, mon, ',
     1        'ndes,  n1,  n2,  nR, nRa,irow, iflow, cursto1'
            write(nlog,'(a12, i5, 1x, a4, 7i5, f8.0)')
     1        '  FlowRes;  ',
     1        iyrmo(mon),xmonam(mon),
     1        ndes, n1, n2, nR, nRa, irow, iflow, cursto1  
          endif
          
        enddo
        goto 300
      endif
c       
c _________________________________________________________
c               Step 5; Save as comments in code 
c                       in case plans are added later

cxxxxc
cxxxxc ---------------------------------------------------------
cxxxxc		c. Destination is a plan (Demand = Divalo)
cxxxxc
cxxxxc rrb 2007/08/17; Allow a Plan Destination
cxxxx      if(ndtype.eq.7) then
cxxxx        ndP=nd
cxxxx        ndD=0
cxxxx        ndR=0
cxxxx        ndI=0
cxxxx        iresw=-1
cxxxx        cDest=pid(ndP)
cxxxx        if(ifix(pon(ndP)).eq.0) ioff=2
cxxxx        
cxxxx        IUSE=1
cxxxx        iuseX=iuse
cxxxx        
cxxxx        IDCD=Ipsta(NDP)
cxxxx        idcdP=idcd
cxxxx        ndnd=NDNNOD(idcd)
cxxxx        
cxxxx        idcdX=idcdP        
cxxxx        ndndX=NDNNOD(idcdX)
cxxxxc
cxxxxc ---------------------------------------------------------
cxxxxc rrb 2007/12/04; Add Loss to Demand for a Plan Destination                
cxxxxc       divreq1=big/fac     
cxxxxc rrb 2008/04/23; Limit the transfer for a T&C or Aug plan 
cxxxxc	    Types 1 or 2
cxxxx        if(iplnTyp(ndp).eq.1 .or. iplnTyp(ndp).eq.2) then
cxxxx          divreq0=pdem(ndp)
cxxxx          divreq1=divreq0/OprEffT        
cxxxx          DIVALO=divreq1
cxxxx          divaloX=divalo        
cxxxx        else                  
cxxxx          divreq0=big/fac
cxxxx          divreq1=big/fac/OprEffT        
cxxxx          DIVALO=divreq1
cxxxx          divaloX=divalo        
cxxxx        endif
cxxxx      endif  
c
c Step X Check for detailed output before returning
c
c      
  300  continue
c
c       
c _________________________________________________________
c               Step 6; Detailed Output
c
 310  iprint=1
      if(iday.eq.1 .and. divact.lt.small) iprint=0
      
      if((iout.eq.1 .or. iout.eq.2) .and. iw.eq.ioutiw .and.
     1   iprint.eq.1) then 
c
c ---------------------------------------------------------
c		            l. Detailed header      
        ncallX=ncallX+1
        if(ncallX.eq.1) then
          write(nlog,270) corid(l2), cdestyp, ccarry, cTandC, cpuse,
     1      cDivTyp(l2), Creplace, cRiver  
        else
          if(ncallX.gt.1) then
            if(ncallX.eq.2) write(nlog,*) ' '
            write(nlog,272) corid(l2), cwhy, icallop(l2) 
            goto 500
          else
            write(nlog,*) ' '  
          endif                
        endif
c
c ---------------------------------------------------------
c		            2. Detailed results  
        n1=0
        n2=0
      
        do n=1,ndes
          n1=n2+1
          n2=n1+1
        
          nR  =Iopdes(n1,L2)
          nRa =Iopdes(n2,L2) 
          
          irow=nowner(nR)+nRa-1 
          cursto1=curown(irow)
   
          write(nlog,280) ' FlowRes    ', 
     1      iyrmo(mon),xmonam(mon),idy, nr, nra, 
     1      Resnam1(nR), cresid(nR), nRa, cursto1, 
     1      icallop(l2), iflow, iwhy, cwhy
     
        end do
      endif
c
c _________________________________________________________
c               
c               Step 7; Check results
c rrb 2018-02-11; Check
c
 500  if(ioutX.eq.1) then
         write(nlog,*) ' '
        write(nlog,*) ' FlowRes; Calling Chekava Out ',  corid1
      endif  
      
      call chekava(51, maxsta, numsta, avail, subtypX)
c      
c ---------------------------------------------------------        
c               b; Update operating rule output (DIVO)
c
      divo(l2)=float(iflow)/fac
c _________________________________________________________
c
c               Step 8; Return

      RETURN
c
c _________________________________________________________
c               Formats
  270   format(/, 
     1  '  FlowRes (Type 51); Operation Right ID = ', a12,
     1  ' Destination Type = ', a12,
     1  ' Carrier (Y/N) = ',a3, ' T&C Plan (Y/N) = ',a3,/
     1  '                    ', ' Reuse Plan (Y/N) = ', a3,
     1  ' Diversion Type = ', a12,' Called by Replace = ', a3,
     1  ' Release to River = ', a12,/
     1  '                    ',
     1  ' icallOP = Number of calls this time step;  ',
     1  ' iflow = Project is operating (1) or 0=not operating (0)',
     1     //
     1    ' FlowRes      Iyr  Imo  Idy   nR  nRa',
     1    ' Reservoir Name           Res_ID        Account',
     1    '  Cursto icallOP   iflow iwhy cwhy',/
     1    ' ___________ ____ ____ ____ ____ ____',
     1    ' ________________________ ____________ ________',
     1    ' _______ _______ _______ ____ _________________________')
c
 272   format(
     1  ' FlowRes (Type 51); Operation Right ID = ', a12,
     1    1x, a48, ' Number of calls = ', i5)

 280     format(a12, i5, 1x,a4, 3i5, 1x,
     1     a24,1x, a12,1x, i8, f8.0, i8, i8, i5, 1x, a48)     
c
c               Error warnings
c _________________________________________________________
 9999 continue
      write(nlog,270) corid(l2), cdestyp, ccarry, cTandC, cpuse,
     1      cDivTyp(l2), Creplace, cRiver                    
 
      write(nlog,280) '  FlowRes_X',
     1  iyrmo(mon),xmonam(mon),idy, icallop(l2), iflow, iwhy, cwhy
 
      write(6,1050) 
      write(99,1051) 
    
 1050 format('    Stopped in FlowRes',/,
     1       '    See the *.log file')
 1051 format('    Stopped in FlowRes')
      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop 
    
      END

