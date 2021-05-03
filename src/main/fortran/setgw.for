c setgw - sets demand data for a diversion (well?)
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

        subroutine SetGw(isub, small, fac, corid1)
c
c _________________________________________________________
c      Program Description
c            SetGW; It Sets to GW when a well depletion drives available
c            flow (Avail) negative
c                                                                 
c_________________________________________________________________
c                                                                 
c       Update History                                            
c                                                                 
c rrb 2021/04/18; Compiler warning                                
c _____________________________________________________________
c	      Dimensions
c
      include 'common.inc'        
      character corid1*12
c _____________________________________________________________
c  
c rrb 2021/04/18; Compiler warning
      corid1=corid1
      
      iout=1
c rrb 00/05/03; Check entire array, not just downstream         
      do nx=1,numsta
        iss=nx
        ndns1=ndnnod(iss)
c
c ---------------------------------------------------------
c              a Find negative
        if(avail(iss).lt.(-1.*small)) then
c
c ---------------------------------------------------------
c              b Calculate gw2riv
          gx = avail(iss)
          gw2riv(iss)=gw2riv(iss) - gx
c
c ---------------------------------------------------------
c              c Route gw2riv downstream
          CALL TAKOUT(maxsta, AVAIL, RIVER, AVINP, QTRIBU, idncod,
     1                gx,     ndns1, iss)            
        endif
      end do
c
c ---------------------------------------------------------
c              d Detailed Output      
        if(iout.eq.1) then
          write(nlog,*) 
     1      ' SetGW; Avail after GW adjustments gx = ',gx*fac
          write(nlog,'(20f8.2)') (avail(i)*fac, i=1,numsta)
        endif  
c
c ---------------------------------------------------------
c              d Double check the entire Avail Array
c		 Note: 
c		 istop =  0 DO NOT STOP if a negative is found
c		          1 DO STOP if a negative is found
        istop=0      
        call chekav2(
     1   icall, maxsta, numsta, istop, fac, AVAIL, IMCD, AvMin)
        AvailC2=avail(imcd)   
c
c_____________________________________________________________
c		           Step 3; Print warning if negative available flow
      IF(AVAILC2 .le.(-1.*small)) then
        write(nlog,*) ' SetGW; problem with operating rule ',
     1    ' isub =', isub
cx        WRITE(nlog,*) IYRmo(mon),xmonam(MON),IW,NWRORD(1,IW),L2,
cx     1                IUSE,DIVREQx*fac,
cx     1                ISCD,ISCD,IMCD,DIVACT*fac, avail(imcd)*fac
        write(nlog,320) (avail(iss)*fac,iss=1,numsta)
        write(nlog,330) (river(iss)*fac,iss=1,numsta)
        
        goto 9999           
      endif
c
c_____________________________________________________________
c              Formats
c        
        
cx310 FORMAT(/, '  DivAlt Print 5',I10,6x,a4,4i10,
cx   1             F10.2,3I10,F10.2, f20.10)
cx    
  320 format(/, '  SetGW: avail  ',/,(10f10.2))
  330 format(/, '  SetGW: river  ',/,(10f10.2))        
c
c_____________________________________________________________
c              Return
      return
c
c_____________________________________________________________
      
 9999 write(6,1050) 
      write(nlog,1051) 
      
 1050 format('    Stopped in SetGW',/,
     1       '    See the *.log file')
 1051 format('    Stopped in SetGW')
      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)


      stop 
      END      
