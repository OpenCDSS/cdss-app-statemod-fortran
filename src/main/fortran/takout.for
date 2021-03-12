c takout - subtracts divact from 
c          Avail and River at diverting node (iscd) and from
c          Avail, River and Avinp at downstream nodes (iss).
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

      SUBROUTINE TAKOUT(maxsta, AVAIL ,RIVER ,AVINP ,QCHECK,IDNCOD,
     1                  DIVACT, NDNS  ,ISCD)
c
c
c _________________________________________________________
c	Program Description
c
c       Takout; It subtracts divact from 
c               Avail and River at diverting node (iscd) and from
c               Avail, River and Avinp at downstream nodes (iss)
c
c
c _________________________________________________________
c       Documentatoin
c
c               avail is water available for diversion
c               river is wet water at this node
c               avinp is wet water upstream of this node
c
c
c _________________________________________________________
c	Dimensions
c
      DIMENSION AVAIL(maxsta), RIVER(maxsta), AVINP(maxsta),
     1          QCHECK(maxsta),IDNCOD(maxsta)

c
c _________________________________________________________
c		Step 1; Initialize
      nlog=99
      iout=0
      if(iout.eq.1) then
        write(nlog,*) ' '
        write(nlog,*) ' Takout; iscd, ndns', iscd, ndns
      endif
c
c  Adjust avail and river at the diversion itself        
      ISS=ISCD
      AVAIL(ISS)=AVAIL(ISS)-DIVACT
      RIVER(ISS)=RIVER(ISS)-DIVACT
c
c  Adjusst avail, river and avinp at every downstream node
      ISS=IDNCOD(ISS)
      IF(ISS.EQ.0.OR.NDNS.LE.1) goto 500
      
      DO ND=1,NDNS-1
        if(iout.eq.1) write(nlog,*) 
     1    ' Takout; nd, ndns-1, iss', nd, ndns-1, iss
c      
        if(iss.eq.0) then
          write(nlog,*) '  Takout; Warning iscd, ndns, iss = ',
     1       iscd, ndns, iss
          goto 500
        endif
      
        AVAIL(ISS)=AVAIL(ISS)-DIVACT
        RIVER(ISS)=RIVER(ISS)-DIVACT
        AVINP(ISS)=AVINP(ISS)-DIVACT
        
        ISS=IDNCOD(ISS)
      end do
C
C
 500  RETURN
c
c _________________________________________________________
c
c	 Error Tracking
 1000 write(6,1050) 
      write(nlog,1051) iss
      
 1050 format(
     1  '    Stopped in Takout',/,
     1  '    See the *.log file')
 1051 format(
     1  '    Stopped in Takout; iss, iscd = ', i5,/
     1  '      This can happen when the number of downstream nodes',/
     1  '      (ndns) is incorrect for this river node (iscd)')
      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)        
      END
