c dnmfso2 - finds the minimum flow at station iscd and downstream.
c           Same as DnmFso but it includes cCallBy.
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
      SUBROUTINE DnmFso2(maxsta, AVAIL ,IDNCOD,ISCD,NDNS,IMCD,
     1  cCallBy)
c
c _________________________________________________________
c
c       Program Description
c       DnmFso2; It finds the minimum flow at station iscd
c               and downstream.
c		    Same as DnmFso but it includes cCallBy
c
c ________________________________________________________
c       Update History
c
c rrb 2021/04/18; Compiler warning
c

      DIMENSION AVAIL(maxsta),IDNCOD(maxsta)
      character cCallBy*12
c
c ________________________________________________________
c       Initilize
C
C-------------------------------------------------------------------
C
C------  FIND THE DOWNSTEAM MINMUM FLOW NODE OF STATION ISCD
C
C-------------------------------------------------------------------
C
      iout=0
      if(iout.eq.1) write(99,*) '  DnmFso2; In, maxsta, iscd, ndns ', 
     1  maxsta, iscd, ndns

c jhb 2014/06/26 temporary fix...
      if(ndns.lt.0 .or. ndns.gt.maxsta) then
        write(99,*) '  DnmFso2; ndns has invalid value. ndns = ', ndns
        imcd=iscd
        return
      endif
     
      IMCD=ISCD
      ISS=ISCD
      
      DO ND=1,NDNS
        if(iss.eq.0 .or. imcd.eq.0) goto 9999
        IF(AVAIL(IMCD).GT.AVAIL(ISS)) IMCD=ISS
        ISS=IDNCOD(ISS)
      end do
      if(iout.eq.1) write(99,*) '  DnmFso2; Out imcd ', imcd
C
      RETURN
      
      
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
