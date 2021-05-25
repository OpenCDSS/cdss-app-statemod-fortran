c dnmfso2 - finds minimum flow at station and downstream
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

      SUBROUTINE DnmFso2(maxsta, AVAIL, IDNCOD,ISCD,NDNS,IMCD,
     1  cCallBy)
c
c
c _________________________________________________________
c       Program Description
c
c       DnmFso2; It finds the minimum flow at station iscd
c                and downstream.
c                Same as DnmFso but it includes cCallBy
c
c _________________________________________________________
c
c               Update history
c
c rrb 2021/05/02; Runtime error tracking
c
c
c _________________________________________________________
c
c               Dimensions
      DIMENSION AVAIL(maxsta),IDNCOD(maxsta)
      character cCallBy*12
c
c rrb 2021/05/02; Runtime error tracking
      iprob=0
      if(iprob.eq.1) goto 9999
C
C-------------------------------------------------------------------
C
C------  FIND THE DOWNSTEAM MINIMUM FLOW NODE OF STATION ISCD
C
C-------------------------------------------------------------------
C
      iout=0
cx    if(cCallBy.eq.'IfrRigSP    ') iout=1
      if(iout.eq.1) write(99,*) ' DnmFso2; In, maxsta, iscd, ndns ', 
     1  maxsta, iscd, ndns
     
      IMCD=ISCD
      ISS=ISCD
      
      if(iout.eq.1) write(99,*) ' DnmFso2; iscd, imcd, iss, ndns', 
     1              iscd, imcd, iss, ndns
      DO ND=1,NDNS
        if(iout.eq.1) then
          write(99,*) ' DnmFso2;   nd iscd ndns  iss imcd' 
          write(99,'(a10,1x,20i5)') 
     1             '  DnmFso2; ',nd, iscd, ndns, iss,imcd
        endif
c
c rrb 2021/05/02; Runtime error tracking        
cx      if(iss.eq.0 .or. imcd.eq.0) goto 9999
        if(iss.eq.0 .or. imcd.eq.0) then
          write(99,*) ' DnmFso2; Problem with iss or imcd ', 
     1     cCallBy, iss, imcd
cx        goto 9999
          goto 500
        endif
cx
        IF(AVAIL(IMCD).GT.AVAIL(ISS)) IMCD=ISS
        ISS=IDNCOD(ISS)
      end do
      if(iout.eq.1) write(99,*) '  DnmFso2; Out imcd ', imcd
c
c rrb 2021/05/02; Runtime error tracking        
cx    RETURN
 500  RETURN
c
c_____________________________________________________________
c               Error warnings
c
 9999 continue
      write(99,300) cCallBy, nd, iscd, ndns, imcd, iss
  300 FORMAT(/, 72('_'),/
     1  '  DnmFso2; Problem undefined data when called by ',a12/
     1  '             nd iscd ndns imcd  iss' /,
     1  '         ', 20i5)
 
      write(6,310) 
      write(99,320) 
      
 310  format('    Stopped in DnmFso2',/,
     1       '    See the *.log file')
 320  format('    Stopped in DnmFso2')
      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)


      stop
      END
