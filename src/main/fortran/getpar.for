c getpar - gets a structure type and parameter value 
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

        subroutine getpar(itype, ip, ptypex)
c
c
c _________________________________________________________
c	Program Description
c
c       Getpar; It gets a structure type and parameter value 
c
c_________________________________________________________________
c
c       Update History
c
c rrb 2021/04/18; Compiler warning
c _________________________________________________________
c       Documentation               
c
c               itype = 0 diversion, instream flow, other (*.b43, *.xdd)
c                       1 stream gage (*.b43, *.xdd)
c                       2 reservoir (*.b44, *.xre)
c                       6 well (*.b43, *.xwe)
c                      99 help
c
c _________________________________________________________
c	Dimensions
       include 'common.inc'

       dimension  ptype(10)
       character  ptypex*24, ptype*12
                                                             
       data ptype/
     1 'Diversion  ',  'StreamGage  ', 'Reservoir   ',
     1 ' ',            ' ',            'Well        ',
     1 ' ',            ' ',            ' ', ' '/
c
c
c _________________________________________________________
c		Step 1; Initialize

         io99=99
c rrb 2005/11/22; River Loss and Carrier Loss
c     ndiv = 27
c     nres=21
      ndiv = 37         
      nres=26
      ndivw=18
c
c rrb 2021/04/18; Compiler warning
cx    nx=amax0(ndiv, nres, ndivw)       
      nx=max(ndiv, nres, ndivw)
c
c               Process diversion or streamGage type
      do 100 i=1,nx
        if(itype.le.1 .and. ptypex.eq.paramd(i)) then
          ip = i
          goto 110
        endif        
c
c               Process reservoir type
        if(itype.eq.2 .and. ptypex.eq.paramr(i)) then
          ip = i
          goto 110
        endif
c
c               Process well type
        if(itype.eq.6 .and. ptypex.eq.paramw(i)) then
          ip = i
          goto 110
        endif 
c
c               Print available parameters
        if(itype.eq.99) then           
          write(io99,140) (j, paramd(j), j=1,ndiv) 
          write(io99,141) (j, paramr(j), j=1,nres) 
          write(io99,142) (j, paramw(j), j=1,ndivw)
          call flush(6)
          goto 110
        endif
  100   continue
        goto 120

c 110   write(io99,*) '  Getpar, ptypex, ip', ptypex, ip
  110   return
c
c               Error Messages
  120    write(io99,*) ' ' 
         write(io99,*) '   Getpar; For type = ', ptype(itype+1)
         write(io99,*) '           Parameter not found = ', ptypex
         write(io99,140) (j, paramd(j), j=1,ndiv) 
         write(io99,141) (j, paramr(j), j=1,nres)
         write(io99,142) (j, paramw(j), j=1,ndivw)
         write(6,*) '  Stopped in Getpar, see the log file (*.log)'
         write(io99,*) '  Stopped in Getpar'
  140    format(/,
     1     ' Available diversion or streamflow parameters:',/
     1     (2x, i5, 1x, a24))
  141    format(/,
     1     ' Available reservoir parameters:',/
     1     (2x, i5, 1x, a24))
  142    format(/,
     1     ' Available well parameters:',/
     1     (2x, i5, 1x, a24))

         write (6,*) 'Stop 1'
         call flush(6)
         call exit(1)


         stop 
         end

