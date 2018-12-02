c dnmfso - finds the minimum flow at station iscd and downstream
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

      SUBROUTINE DNMFSO(maxsta, AVAIL ,IDNCOD,ISCD,NDNS,IMCD)
c
c
c _________________________________________________________
c	Program Description
c
c       Dnmfso; It finds the minimum flow at station iscd
c               and downstream
c
c _________________________________________________________
c	Dimensions
c
c
      DIMENSION AVAIL(maxsta),IDNCOD(maxsta)

C
C-------------------------------------------------------------------
C
C------  FIND THE DOWNSTEAM MINMUM FLOW NODE OF STATION ISCD
C
C-------------------------------------------------------------------
C
      iout=0
c
c rrb 2017/10/20; Variables used but not set      
      nlog=99
      nlogx=99

      if(iout.eq.1) write(99,*) '  DnmFso; In, maxsta, ndns, iscd ', 
     1  maxsta, ndns, iscd

c jhb 2014/06/26 temporary fix...
      if(ndns.lt.0 .or. ndns.gt.maxsta) then
        write(99,*) '  DnmFso; ndns has invalid value. Exiting routine.'
        imcd=iscd
        return
      endif

      IMCD=ISCD
      ISS=ISCD
      iout1=1
      if(iss.eq.0 .or. imcd.eq.0) goto 9999      
      iout1=2
        
      DO ND=1,NDNS
        if(iss.eq.0 .or. imcd.eq.0) goto 9999
        imcd1=imcd
        IF(AVAIL(IMCD).GT.AVAIL(ISS)) IMCD=ISS
        
        if(iout.eq.1) write(99,*) '  DnmFso; ',
     1    nd, imcd1, imcd, iss, avail(imcd1),avail(iss)
     
        ISS=IDNCOD(ISS)
      end do
      if(iout.ge.1) write(99,*) '  DnmFso; Out iscd, imcd ',
     1   iscd, imcd, avail(imcd)
C
      RETURN
      
      
c
c_____________________________________________________________
c               Error warnings
c
 9999 continue
      write(99,300) iout1, nd, iscd, ndns, imcd, iss
  300 FORMAT(/, 72('_'),/
     1  '  DnmFso; Problem undefined data',/
     1  '         iout1   nd iscd ndns imcd  iss' /,
     1  '         ', 20i5)
  302 FORMAT(/, 72('_'),/
     1  '  DnmFso; Problem undefined data',/
     1  '         iout1   nd iscd ndns imcd  iss')     
  304 FORMAT('         ', 20i5)
     
      IMCD=ISCD
      ISS=ISCD
      write(nlog,302)
      DO ND=1,NDNS
        write(99,304) iout1, nd, iscd, ndns, imcd, iss      
cx      if(iss.eq.0 .or. imcd.eq.0) goto 9999
c jhb 2014/07/04 added the if block so the code will run without crashing
c                need to figure out what SHOULD happen when
c                  imcd < 1 or iss < 1 !!
        if (iss.ge.1) then
          if (IMCD.ge.1) then
            IF(AVAIL(IMCD).GT.AVAIL(ISS)) IMCD=ISS
          else
c          if(iout.eq.1) then
            write(nlogx,*)
     1      ' dnmfso; avail(IMCD) error:  IMCD = ',
     1      IMCD
c          endif
          endif
          ISS=IDNCOD(ISS)
        else
c         if(iout.eq.1) then
            write(nlogx,*)
     1        ' dnmfso; avail(iss) error:  iss = ',
     1        iss
c          endif
        endif

      end do 
      
      write(6,310) 
      write(99,320) 
      
 310  format('    Stopped in DnmFso',/,
     1       '    See the *.log file')
 320  format('    Stopped in DnmFso')
      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)


      stop 
      END      
