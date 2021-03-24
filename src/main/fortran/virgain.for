c virgain - For gain flows (iopflo=2) it revises flow at gages
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

      subroutine virgain
c
c
c _________________________________________________________
c	Program Description
c
c       Virgain; For gain flows (iopflo=2) it revises flow at gages
c                Note for iopflo=2 at this point
c                qhisto is gain at all points
c                except gauges.  Therefor store total at gages in
c                array virinp, then calculate gain at gage
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc' 
          
c     fac = mthday(mon)*factor
      iout=0
c
      if(iout.gt.0 .or. ichk.eq.4) then
        write(nlog,*) ' VirGain'
        write(6,*) ' VirGain'
      endif
      
      do is=1,numsta
        flowx(is)=0.
      end do
c
c
c _________________________________________________________
c
c               Store total flow at gauges (qhisto) into virinp
      do iru=1,numrun
        iss=irusta(iru)
        virinpx(iss) = qhistox(iss)
        qhistox(iss) = 0.0
      end do
c
c
c _________________________________________________________

c               Calculate total flow based on gain calculations
      do is=1,numsta
        iss=is
c       ndns=ndnnod(iss)
        do nd=1,ndns
          flowx(iss)=flowx(iss)+qhistox(is)
          iss=idncod(iss)
        end do
      end do
c
c
c _________________________________________________________
c
c rrb 11/22/95; calculate gain at gages
      do 390 iru=1,numrun
        is=irusta(iru)
        qhistox(is) = virinpx(is) - flowx(is)
c
c
c _________________________________________________________
c
c rrb 11/14/95; move gain at gages to downstream gages only
        iss=is
        ndns=ndnnod(iss)
        do 380 nd=1,ndns     
c
c
c _________________________________________________________
c
c               Determine if a gage
          ix = 0                         
          do 370 iru2=1,numrun 
            isg=irusta(iru2)
            if(iss.eq.isg) ix = 1
  370     continue
c
c
c _________________________________________________________
c
c               Adjust total flow downstream
          if(ix.eq.1) then
            flowx(iss) = flowx(iss) + qhistox(is)
          endif            

c
c rrb 2021/03/20; Compiler Update
cx380     iss=idncod(iss)
          iss=idncod(iss)
  380   continue
  390 continue

      return
      end
