c somnmy - sorts water rights
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

      SUBROUTINE SOMNMY(Rwr,L,N,maxnwr)
c
c
c _________________________________________________________
c	Program Description
c
c       Somnmy; It sorts water rights
c               Called by rigsor.f                                                             
c_________________________________________________________________
c                                                                 
c       Update History                                            
c
c rrb 2021/04/18; Compiler warning
c _________________________________________________________
c	Dimensions
c
      real*8 rwr
      DIMENSION Rwr(maxnwr), L(maxnwr)
c
c _________________________________________________________
c		Step 1; Initialize  
c
c rrb 2021/04/18; Compiler warning
      iprob=0                 
      if(iprob.gt.0) goto 9999

      DO 10 I=1,N
c
c rrb 2021/03/20; Compiler Update
cx   10 L(I)=I
        L(I)=I
   10 continue
   
      M=N
   20 M1=M-1
c
c rrb 2021/03/20; Compiler Update
cx      IF(M1) 60,60,30
      if(m1.le.0) goto 60
c
c rrb 2021/04/18; Compiler warning      
cx 30 DO 50 I=1,M1 
      DO 50 I=1,M1   
      II=N-I+1
      J=L(II)
      K=L(II-1)

      if(rwr(j)-rwr(k) .lt. -0.000001) then
        l(ii)=k
        l(ii-1)=j
        m=i
      endif

   50 CONTINUE
c
c rrb 2021/03/20; Compiler Update
cx      IF(M-M1) 20,20,60
      IF((M-M1).le.0) goto 20
   60 RETURN
c
c _________________________________________________________
c  
 9999 write(6,*) '  Stopped in Somnmy, see the log file (*.log)'
      write(99,*) '  Stopped in Somnmy'
      write(6,*) 'Stop 1' 
      call flush(6)
      call exit(1)

      stop 
      END

