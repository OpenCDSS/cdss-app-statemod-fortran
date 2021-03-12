c backfil - backfills blanks to the unused portion of character string
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

        subroutine backfil(recin, recout)
c
c _________________________________________________________
c       Program Description
c               It backfills blanks to the unused portion of a
c               character string
c
c               recin character string in
c               recout character string out
c
c
c _________________________________________________________
c       Update History
c       NA
c
c _________________________________________________________
c       Documentation
c
c
c _________________________________________________________
c       Dimensions
cc
        character recin*12, recout*12

        i1=0
        j=0

        recout=' '
        do i=1,12
          i1=12-i+1
c         write(99,110) i1, recin   

          if(recin(i1:i1).ne.' ') then
c           write(99,110) 0, recin

            do j=1,i1
              j1=12-j+1
              recout(j1:j1) = recin(i1:i1)
              i1=i1-1
            end do
c           write(99,110) 1, recin
c           write(99,110) 2, recout
            goto 100 
          endif
        end do 

 100    return
 110    format('  Backfil;', i5, a12)
        end
