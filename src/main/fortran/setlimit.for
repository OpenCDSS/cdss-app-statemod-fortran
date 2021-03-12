c setlimit - adjusts monthly and annual release limits for operating rule (lopr)
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
      subroutine SetLimit(
     1    nlog, icx, lopr, ipLim, ioprlimX, fac,
     1    divact,  OprmaxMX,  OprMaxAX, 
     1    OprmaxX, Oprmax13, OprmaxM1, OprmaxM2, 
     1    psto1x,  psto2X, coridX)
c _________________________________________________________
c	Program Description
c
c      SetLimit; It adjusts monthly and annual release limits 
c		             for an operating rule (lopr)
c	        Called by DivResP2 (type 27), DivRplP (type 28), 
c                   RsrspuP (type 34) and DivCarL (type 45),
c
c _________________________________________________________
c	Update History
c
c
c rrb 2019/09/07; Revised to allow iOprlimX to be 14
c
c rrb 2009/01/23; Revise to operate on storage (psto2), not 
c                	plan supply (psuplyX)
c _________________________________________________________      
c		Documentation
c	
c       nlog            = log file
c       icx             = calling routine (100+opr rule type)
c       lopr            = associated operating rule with limits
c       iplim           = associated plan (if any).  If > 0 it
c                         adjusts psto2x when iOprLimX .ne.1
c
c       iOprLimX        = 1 limits are are increased.  Note this
c                           option is only operational for RsrSpuP
c                           (type 34)
c       iOprLimX        = 2,4, 7, 9 & 14 limits are decreased and
c                           if(iplim>0) plan supplies are decreased
c
c       fac             = factor cfs to af/mo
c       
c       divact          = diversion (cfs)
c       oprmaxMX        = running monthly limit this time step initial
c       oprmaxAX        = running annual limit this time step initial
c
c       oprmaxX         = total monthly limit (value read) 
c       oprmax13        = total annual limit (value read)
c
c       oprmaxM1        = monthly or annual limit initial
c       oprmaxM2        = monthly or annual limit adjusted

     
      character coridX*12
c
c _________________________________________________________      
      iout=0
      small=0.001


      if(iout.eq.1 .and. abs(divact).gt.small) then
          write(nlog,*) ' '
          write(nlog,*) 
     1      ' SetLimit; Adjusting Monthly and Annual Limits'
          write(nlog,*)
     1      ' SetLimit;      coridX        icx ioprLimt   lopr',
     1      '   iplim'     
          write(nlog,'(a12, 1x,a12,1x, 4i8, 20f10.0)') 
     1      ' SetLimit; ',coridX, icx, ioprLimX, lopr, iplim
      endif
c
c _________________________________________________________      
c               Step 2; Adjust monthly or annual diversion limits UP
        
      if(iOprLimX.eq.1 .and. lopr.gt.0) then
        oprmaxM1=oprmaxMX
        oprmaxMX=oprmaxMX + divact*fac
        oprmaxMX=amin1(oprmaxMX, oprmaxX)
        OprmaxM2=oprmaxMX

        oprmaxA1=oprmaxAX
        oprmaxAX=oprmaxAX + divact*fac
        oprmaxAX=amin1(oprmaxAX, oprmax13) 
c
c rrb 2015/02/03; Correction
        if(iplim.gt.0) then
          psto2X=psto2X+divact*fac          
        endif 
        
        
        if(iout.eq.1 .and. abs(divact).gt.small) then
          write(nlog,*) 
     1      ' SetLimit; Adjusting Monthly and Annual Limits UP'
          write(nlog,*)
     1      ' SetLimit;      coridX        icx ioprLimt   lopr',
     1      '   iplim  oprmaxM1    divact  oprmaxMX',
     1              '  OprmaxA1  OprmaxAX    psto1X    psto2X'

     
          write(nlog,'(a12, 1x,a12,1x, 4i8, 20f10.0)') 
     1      ' SetLimit; ',coridX, icx, ioprLimX, lopr, iplim,
     1        oprmaxM1, divact*fac, oprmaxMX,
     1        OprmaxA1, OprmaxAX, psto1X, psto2X
        endif       
      endif
c
c _________________________________________________________
c
c               Step 3; Adjust monthly or annual release limits DOWN
c
c rrb 2011/10/15; Allow a type 4 by a type 45 rule
cx    if(iOprLimX.eq.2 .and. lopr.gt.0) then
c
c rrb 2015/03/23; Allow Oprlimit to range from 1-9
cx    if((iOprLimX.eq.2 .or. iOprLimX.eq.4).and. lopr.gt.0) then
c rrb 2019/09/07; Allow OprliomX to be 14 
cx      if((iOprLimX.eq.2 .or. iOprLimX.eq.4 .or.
cx     1    iOprlimX.eq.7 .or. iOprLimX.eq.9).and. lopr.gt.0) then  
     
      if((iOprLimX.eq.2 .or. iOprLimX.eq.4 .or.
     1    iOprlimX.eq.7 .or. iOprLimX.eq.9 .or.
     1    iOprlimX.eq.14).and. lopr.gt.0) then       
cx      oprmaxM1=amin1(oprmaxMX, oprmaxAX)
cx      oprmaxMX=amax1(oprmaxMX - divact*fac, 0.0)
        oprmaxM1=oprmaxMX
        oprmaxMX=amax1(oprmaxMX - divact*fac, 0.0)
        
        oprmaxA1=oprmaxAX
        oprmaxAX=amax1(oprmaxAX - divact*fac, 0.0)
        
        OprmaxM2=amin1(oprmaxMX, oprmaxAX)
c
c _________________________________________________________      
c
c		Step 4; Tie to plan for reporting
c		Note OprmaxA(lopr) = psuply(iplim)
c
        if(iplim.gt.0) then
c
c rrb 2009/01/23; Revise to operat on storage only         
cx          psuply1=psuplyX
cx          psuplyX=amax1(psuplyX - divact, 0.0)
cx          psuply2=psuplyX        
          
          psto2X=psto2X-divact*fac          
        endif 
        
        if(iout.eq.1 .and. abs(divact).gt.small) then
          write(nlog,*) ' '
          
          write(nlog,*) 
     1      ' SetLimit; Adjusting Monthly and Annual Limits Down'
          write(nlog,*)
     1      ' SetLimit;      coridX        icx ioprLimt   lopr',
     1      '   iplim  oprmaxM1    divact  oprmaxMX',
     1              '  OprmaxA1  OprmaxAX    psto1X    psto2X'
          write(nlog,'(a12, 1x,a12,1x, 4i8, 20f10.0)') 
     1      '  SetLimit; ',coridX, icx, ioprlimX, lopr,
     1      iplim, oprmaxM1, divact*fac, oprmaxMX, 
     1      OprmaxA1, OprmaxAX, psto1X, psto2X                
        endif
      endif
c
c _________________________________________________________      
c
c		Step 5; Return      
      return 
      end
