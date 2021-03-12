c getid - gets the station pointer (ir1) and its location on the river pointer (is1)
c         for a given structure type (idiv) and a given structure id (direqx).
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

       subroutine getid(idiv, is1, ir1, ir2, idreqx)
c
c
c _________________________________________________________
c	Program Description
c
c       Getit; It gets the station pointer (ir1) and
c               its location on the river pointer (is1)
c               For a given structure type (idiv) and
c                 a given structure id (direqx) 
c
c _________________________________________________________
c	Documentation
c               idiv  =  type switch 
c                       -1=unknown
c                        0=diversion or instream flow, 
c                        1=stream gage, 
c                        2=reservoir
c                        3=operational right
c                        4=other (no diversion, instream, gage, res)
c                        6=well
c                        7=well only for *.xss
c                        8=plan
c                       -2=instream flow only
c                       -3=diversion only
c               is1   =  River id of structure,
c                        Note is1 = 0 if not of requested type
c               ir1   =  Reservoir id or well id
c               ir2   =  Counter for *.xss related to well only
c               idreqx =  requested id
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
        character idreqx*12
c
c
c _________________________________________________________
c		Step 1; Initialize
c

c		iout =1 details
        iout = 0
        is1=0
        ir1=0
        ir2=0

        if(iout.eq.1) then
          write(nlog,*) '  Getid; idiv  = ', idiv
          write(nlog,*) '  Getid; idreqx = ', idreqx
        endif
c
c               Find corresponding diversion or instream flow
        if(idiv.eq.0 .or. idiv.eq.-1 .or. idiv.eq.-3) then
          do i=1,numdiv
            if(idreqx.eq.cdivid(i)) then
              ir1=i
              is1=idvsta(i)
              goto 140
            endif
          end do
        endif
c
c               Find corresponding instream flow
        if(idiv.eq.0 .or. idiv.eq.-1 .or. idiv.eq.-2) then
          do i=1,numifr
            if(idreqx.eq.cifrid(i)) then
              ir1=i
              is1=ifrsta(i)
              goto 140
            endif
          end do
        endif
c
c               Find corresponding stream gage
        if(idiv.eq.1 .or. idiv.eq.-1) then
          do i=1,numrun
            if(idreqx.eq.crunid(i)) then
              ir1=i
              is1=irusta(i)
              goto 140
            endif
          end do
        endif
c
c               Find corresponding other (river) id
        if(idiv.eq.0 .or. idiv.eq.4 .or. idiv.eq.-1) then
          do i=1,numsta
            if(idreqx.eq.cstaid(i)) then
              ir1=i
              is1=i
              goto 140
            endif
          end do
        endif
c
c               Find corresponding reservoir 
        if(idiv.eq.2 .or. idiv.eq.-1) then
          do i=1,numres
            if(idreqx.eq.cresid(i)) then
              ir1=i
              is1=irssta(ir1)
              goto 140
            endif
          end do
        endif
c
c               Find corresponding operational right
        if(idiv.eq.3 .or. idiv.eq.-1) then
          do i=1,numopr
            if(idreqx.eq.corid(i)) then
              ir1=i
              is1=-1
              goto 140
            endif
          end do
        endif
c
c               Find corresponding well
        if(idiv.eq.6 .or. idiv.eq.-1) then
          do i=1,numdivw
            if(idreqx.eq.cdividw(i)) then
              ir1=i
              is1=idvstaw(i)
              goto 140
            endif
          end do
        endif
c
c rrb 01/04/24; Find corresponding well only
        if(idiv.eq.7) then
          i2=0
          do i=1,numdivw
            nd=idivcow2(i)
            if(nd.eq.0) i2=i2+1
            if(idreqx.eq.cdividw(i)) then 
              ir1=i
              is1=idvstaw(i)
              ir2=i2                      
              goto 140
            endif
          end do
        endif
c
c rrb 01/04/24; Find corresponding plan
        if(idiv.eq.8) then
          do i=1,nplan
            if(idreqx.eq.pid(i)) then 
              ir1=i
              is1=ipsta(i)
              goto 140
            endif
          end do
        endif

c        
 140    if(iout.eq.1) write(nlog,150) idreqx, is1, ir1
        return
c
c               Formats
 150    format('  Getid; For requested ID = ', a12,
     1      ' Stream counter = ', i5, ' and Structure counter = ', i5)
c                            
c
c 160 write(6,*) '  Stopped in Getid, see the log file (*.log)'
c     write(nlog,*) '  Stopped in Getid'
c     write (6,*) 'Stop 1'
c     call flush(6)
c     call exit(1)

c     stop 
      END
