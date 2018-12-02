c dnmfsow - It is similar to dnmfso but it searches all array elements, not just downstream.
c           Used for wells since depletions are often not downstream.
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

      SUBROUTINE DNMFSOw(maxsta, AVAIL ,numsta, IMCD)
c
c _________________________________________________________
c	Program Description
c
c       Dnmfsow; It is similar to dnmfso but it searches
c               all array elements, not just downstream
c               Used for wells since depletions are often
c               not downstream
c
      dimension avail(maxsta)
C
C-------------------------------------------------------------------
C
C------  FIND THE MINIMUM FLOW IN THE NETWORK
C
C-------------------------------------------------------------------
C
      IMCD=1
      
      DO ND=1,numsta
        IF(AVAIL(IMCD).GT.AVAIL(nd)) IMCD=nd
      end do
C
      RETURN
      END
