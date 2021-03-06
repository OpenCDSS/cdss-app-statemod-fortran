c rigsor - sorts water rights by priority
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

      SUBROUTINE RIGSOR(maxnwrx)
C
c
c _________________________________________________________
c	     Program Description
c
c       Rigsor; It sorts water rights by priority
c
c_________________________________________________________________
c                                                                 
c      Update History                                            
c                                                                 
c rrb 2021/04/18; Compiler warning                                
c _________________________________________________________
c	     Dimensions
c
      include 'common.inc'
      real*8 rwr
      dimension rwr(maxnwrx)
      DIMENSION IWR(maxnwrx),KWR(maxnwrx),LWR(maxnwrx)  
c                                       
c rrb 2021/04/18; Compiler warning      
cx    INTEGER*4 I4
c
c _________________________________________________________
c
c               Step 1a - Initialize
c
c rrb 2021/04/18; Compiler warning
      iprob=0                 
      if(iprob.gt.0) goto 9999
c
C-------------------------------------------------------------------
c                                
c rrb 2021/04/18; Compiler warning
cx    I4(I)=I
c     ldem=5285
c                           
c rrb 06/18/96; Check for local dimensions
c
c rrb 99/08/27; Note maxsta*3 is used to allow data 
c               from the equivalence statement to wrap into
c               first 3 arrays of dummy that is dimensioned
c               based on maxsta size.
c               Note ntorig is total rights from riginp.f
c     if(maxnwr.gt.ldem .or. maxnwr.gt.maxsta*3) then
c     if(maxnwr.gt.ldem .or. ntorig.gt.maxsta*3) then
c       write(99,*) '  Rigsor; Local dimension exceeded'
c       write(99,*) '          local, maxnwr, maxsta*3 = ', 
c    1                         ldem,  maxnwr, maxsta*3
c       write(99,*) '  Revise common.for, statem.f & somnmy'
c       goto 9999
c     endif
c                           
c rrb 06/18/96; Check for local dimensions
c     if(ntorig.gt.ldem) then
c       write(99,*) '  Rigsor; Local dimension exceeded'
c       write(99,*) '          local, ntorig = ', ldem, ntorig
c       write(99,*) '  Revise common.for, statem.f & somnmy'
c       goto 9999
c     endif
C
C-------------------------------------------------------------------
C
C------  FIND THE PRIORITY ORDER OF ALL WATER RIGHTS
C
C-----------------------------------------------------------------------
C
      I=NUMFRR
      J=NUMRSR+I
      K=NUMDVR+J
      L=NUMPWR+K
c
c rrb 98/11/18; Wells
      m=numopr+L

      if(numfrr.gt.0) then
        DO IW=1,NUMFRR
          KWR(IW  )=1
          LWR(IW  )=IW
          Rwr(IW  )=rFRNK(IW)
        end do
      end if
C
      IF(NUMRSR.gt.0) then
        DO IW=1,NUMRSR
          KWR(IW+I)=2
          LWR(IW+I)=IW
          Rwr(IW+I)=rRSNK(IW)
        end do
      end if
C
      IF(NUMDVR.gt.0) then
        DO IW=1,NUMDVR
          KWR(IW+J)=3
          LWR(IW+J)=IW
          Rwr(IW+J)=rDVNK(IW)
        end do
      end if
C
      IF(NUMPWR.gt.0) then
        DO IW=1,NUMPWR
          KWR(IW+K)=4
          LWR(IW+K)=IW
          Rwr(IW+K)=rPWNK(IW)
        end do
      endif
C
      IF(NUMOPR.gt.0) then
        DO IW=1,NUMOPR
          KWR(IW+L)=5
          LWR(IW+L)=IW
          Rwr(IW+L)=rOPNK(IW)
        end do
      endif
C
c rrb 98/11/18; Wells
      if(numdvrw.gt.0) then
        DO IW=1,numdvrw
          KWR(IW+M)=6
          LWR(IW+M)=IW
          Rwr(IW+M)=rdvNKw(IW)
        end do
      end if
C
c     write(6,*) 'ENTER INTO SORTING ROUTINE'
C
      CALL SOMNMY(Rwr,IWR,NTORIG,maxnwr)
C
c     write(6,*) 'OUT FROM SORTING ROUTINE'
C                      
c rrb 2021/04/18; Compiler warning
cx310 DO 320 IW=1,NTORIG 
      DO 320 IW=1,NTORIG  
      M=IWR(IW)
      NWRORD(1,IW)=KWR(M)
      NWRORD(2,IW)=LWR(M)
  320 CONTINUE
c
c _________________________________________________________
c
c
      RETURN
c      
c rrb 2021/04/18; Compiler warning
c 330 write(6,*) '  Stopped in Rigsor, see the log file (*.log)'
 9999 write(6,*) '  Stopped in Rigsor, see the log file (*.log)'
      write(99,*) '  Stopped in Rigsor'
      write(6,*) 'Stop 1'       
      call flush(6)
      call exit(1)

c
c _________________________________________________________
c
      stop 
      END




