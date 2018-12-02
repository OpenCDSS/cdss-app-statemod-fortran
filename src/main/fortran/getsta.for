c getsta - gets the structure ID (idreqx) and structure ID pointer (ifound)
c          for a given data type (idiv) and river location id (n)
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

       subroutine getsta(idiv,n,is1,ifound,idreqx)
c
c
c _________________________________________________________
c	Program Description
c
c       GetSta; It gets the structure ID (idreqx)
c               and structure ID pointer (ifound)
c               For a given data type (idiv) and river location id (n)
c
c _________________________________________________________
c	Documentation
c
c               idiv = data type
c                      0 diversion 
c                      1 stream 
c                      3 instream flow
c                      6 well 
c               n = river ID 
c               is1 = river ID
c               ifound = 0 not found
c                        x found at structure x
c               idreqx = structure ID found
c
c _________________________________________________________
c	Dimensions
c
        include 'common.inc'
        character idreqx*12
c
c _________________________________________________________
c
        iout=0

        is1=n                  
        ifound=0         
        idreqx=' '
c
c _________________________________________________________
c               Find corresponding diversion
        if(idiv.eq.0) then
          do i=1,numdiv
            if(idvsta(i).eq.n) then
              idreqx=cdivid(i)
              ifound=i
            endif
          end do  
        endif
c
c _________________________________________________________
c               Find corresponding instream flow river station
        if(idiv.eq.3) then
          do i=1,numifr
            if(ifrsta(i).eq.n) then
              idreqx=cifrid(i)
              ifound=i
            endif
          end do  
        endif
c
c _________________________________________________________
c               Find corresponding stream gage
        if(idiv.eq.1) then
          do i=1,numrun
            if(irusta(i).eq.n) then
              idreqx=crunid(i)
              ifound=i
            endif
          end do  
        endif
c
c _________________________________________________________
c               Find well station
        if(idiv.eq.6) then
          do i=1,numdivw
            if(idvstaw(i).eq.n) then
              idreqx=cdividw(i)
              ifound=i
            endif
          end do
        endif
c
c _________________________________________________________
c               Find a plan station
        if(idiv.eq.7) then
          do i=1,nplan
            if(ipsta(i).eq.n) then
              idreqx=pid(i)
              ifound=i
            endif
          end do
        endif
c
c _________________________________________________________
          
        if(iout.eq.1) then
          write(io99,*)  '  Getsta;  idiv,n,is1,ifound,idreqx'
          write(io99,*)  '        ', idiv,n,is1,ifound,idreqx
        endif

        return
        end






