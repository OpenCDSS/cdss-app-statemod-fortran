c takou2 - takes water from the available flow array
c          It is similar to takout, but for instream flows do not adjust river and avinp.
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
      SUBROUTINE TAKOU2(isub, maxsta, AVAIL ,IDNCOD,DIVACT,
     1                  NDNS  , ISCD)
c
c
c _________________________________________________________
c	Program Description
c
c       Takou2; It takes water from the available flow array
c               It is similar to takout, but used when the
c               destination is an instream flow
c               or T&C plan (type 1) or Aug plan(type 2)
c               It adjusts avail but not river and avinp. 
c
c _________________________________________________________
c	Dimensions
c
      DIMENSION AVAIL(maxsta),IDNCOD(maxsta)
c
c _________________________________________________________
c
c  Step 1; Initialize
      iout=0
      nlog=99
      fac=1.9835 
      
      ISS=ISCD
      AVAIL(ISS)=AVAIL(ISS)-DIVACT
      ISS=IDNCOD(ISS)
      
      if(iout.eq.2) then        
        write(nlog,*) '  Takou2 isub, iscd, iss, ndns, divact',
     1      isub, iscd, iss, ndns, divact*fac     
        endif      
      IF(ISS.EQ.0.OR.NDNS.LE.1) goto 500
c
c _________________________________________________________
c
c  Step 2; Route downstream
      
      do ND=1,NDNS-1
        if(iout.eq.1) then        
          write(nlog,*) '  Takou2 nd, iss, ndns-1, divact',
     1      nd, iss, ndns-1, divact*fac     
        endif
c        
cx      if(iss.eq.0) goto 1000
        if(iss.eq.0) then
          write(nlog,*) 
     1     '  Takou2; Warning isub, iscd, ndns, iss, nd = ', 
     1     isub, iscd, ndns, iss, nd
          write(nlog,*) '  Note -1 is rivrtn, -2 is rtnmax',
     1      ' 48 id powresP, 49 is divrplp2, 201 is ifrrigx'
cx          goto 500
        endif        
        
        
        AVAIL(ISS)=AVAIL(ISS)-DIVACT
        ISS=IDNCOD(ISS)
      end do
c
c _________________________________________________________
c
c	 Step 3; Return

 500  RETURN
c
c _________________________________________________________
c
c	 Error Tracking
 1000 write(6,1050) 
      write(nlog,1051) isub, iss
      
 1050 format(
     1  '    Stopped in Takou2',/,
     1  '    See the *.log file')
 1051 format(
     1  '    Stopped in Takou2; '/
     1  '      Called by subroutine ',i5,' iss = ', 2i5,/
     1  '      where +n is the subroutine number and ',/
     1  '      201 is ifrrig, -1 is RivRtn and -2 is RtnMax',/
     1  '      This can happen when the number of downstream nodes',/
     1  '      (ndns) is incorrect for this river node (iscd)')
      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)      
      END
