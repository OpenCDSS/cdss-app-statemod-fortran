c somnmy - sorts water rights
c_________________________________________________________________NoticeStart_
c StateMod Water Allocation Model
c StateMod is a part of Colorado's Decision Support Systems (CDSS)
c Copyright (C) 1994-2018 Colorado Department of Natural Resources
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
c
c _________________________________________________________
c	Dimensions
c
      real*8 rwr
      DIMENSION Rwr(maxnwr), L(maxnwr)
c
c _________________________________________________________
c		Step 1; Initilize
      DO 10 I=1,N
   10 L(I)=I
      M=N
   20 M1=M-1
      IF(M1) 60,60,30
   30 DO 50 I=1,M1
      II=N-I+1
      J=L(II)
      K=L(II-1)

      if(rwr(j)-rwr(k) .lt. -0.000001) then
        l(ii)=k
        l(ii-1)=j
        m=i
      endif

   50 CONTINUE
      IF(M-M1) 20,20,60
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

