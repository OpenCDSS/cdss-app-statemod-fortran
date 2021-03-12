c oprdown - determines if a structure located at iscd
c           is downstream of a structure loacted at iss with ndns2 downstream nodes
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
        subroutine oprdown(nlog, maxsta, ndns2, iss, iscd, idncod, 
     1    cidvri, csource, cdest)
c
c _________________________________________________________
c	Program Description
c
c
c               Oprdown; it determines if a structure located  at iscd
c                        is downstream of a structure loacted at iss
c                        with ndns2 downstream nodes
c
c _________________________________________________________
c	Documetaiton
c               	iss	Source location
c               	iscd	Destination location
c
c _________________________________________________________
c	Dimensions
c
        dimension idncod(maxsta)
        character cidvri*12, csource*12, cdest*12
c
c _________________________________________________________
c		Step 1; Initialize        
        iout=0
        iss1=iss
        itype=1
c
c		Set data for debug output        
        nssour= iss1
        nsdest= iscd

        do is=1,ndns2
          if(iout.eq.1) then
            write(nlog,*) '  Oprdown; ',is, iss, iscd, ndns2
          endif
          if(iss.eq.iscd) goto 500
          iss=idncod(iss)
        end do
c
c		Problem not found 
        write(nlog,160) cidvri, 
     1    csource, nssour, cdest, nsdest
        
        goto 9999
c
c               Return
 500  if(iout.eq.1) then
        if(itype.eq.1) write(nlog,170) cidvri, 
     1    csource, nssour, cdest, nsdest
        if(itype.eq.0) write(nlog,172) cidvri,
     1    csource, nssour, cdest, nsdest
      endif  
      return
c
c               Formats
c _________________________________________________________
  150 format('  Oprdown; PROBLEM see the log file (*.log)')
  160 format(/,
     1 '  OprDown; Problem with operational right ', a12,/
     1 11x,'The destination (reservoir, diversion, plan, ',/
     1 11x,'river return, etc.)' ,/
     1 11x,'IS NOT LOCATED PROPERLY from the source',/
     1 11x,'For an exchange the destination should be upstream',/
     1 11x,'For a direct release or bypass the destination ',
     1     'should be downstream.',/
     1 11x,'Source River ID and pointer (iss1)      = ', a12, 1x, i5,/
     1 11x,'Destination River ID and pointer (iscd) = ', a12, 1x, i5)
     
  170 format(/,
     1 '  OprDown; RESULTS for operational right ', a12,/
     1 11x,'The destination (reservoir or diversion)' ,/
     1 11x,'IS LOCATED PROPERLY to be served directly',/
     1 11x,'(e.g the destination is downstream from the source)',/
     1 11x,'Source River ID and pointer (iss1)      = ',a12, 1x, i5,/
     1 11x,'Destination River ID and pointer (iscd) = ',a12, 1x, i5)
  172 format(/,
     1 '  OprDown; RESULTS for operational right ', a12,/
     1 11x,'The destination (reservoir or diversion)' ,/
     1 11x,'IS LOCATED PROPERLY to be served by exchange',/
     1 11x,'(e.g. the destination is upstream from the source)',/
     1 11x,'Source River ID and pointer (iss1)      = ',a12, 1x, i5,/
     1 11x,'Destination River ID and pointer (iscd) = ',a12, 1x, i5)
c
c               Error Processing
c _________________________________________________________
 9999 write(6,150)
      write(nlog,150)
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop 
c
c _________________________________________________________
c
      end
