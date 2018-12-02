c namext - puts an extension (filext) on a name (filenc).
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

        subroutine namext(maxfn, filenc, filext, fileno)
c
c
c _________________________________________________________
c	Program Description
c
c       Namext, it puts an extension (filext) on a name (filenc).
c               Output is fileno
c
c _________________________________________________________
c
c       Update History
c
c rrb 02/08/08; Allow the response file name to include a .xxx
c rrb 00/08/04; Added maxfn and revised file length from 72 to 256
c
c _________________________________________________________
c	Dimensions
        character filenc*256, filext*3, fileno*256
c
c _________________________________________________________
c		Step 1; Initilize
        iout=0
        nlog=99
        fileno = ' '
        if(iout.eq.1) write(nlog,*) ' Namext; In' 

        do 100 i=1,maxfn
c rrb 02/08/08: Allow the response file name to include a .xxx
c         if(filenc(i:i) .eq. ' ') then
          if(filenc(i:i) .eq. ' ' .or. filenc(i:i).eq.'.') then
            fileno(i:i) = '.'
            fileno(i+1:i+3) = filext
c           write(nlog,'(2x, a256)') filenc
c           write(nlog,'(2x, a256)') fileno
            goto 500       
          else
            fileno(i:i) = filenc(i:i)
          endif
 100    continue
 500    if(iout.eq.1) then
          write(nlog,110) filenc, filext, fileno
        endif
c
c _________________________________________________________
c

        return
c
c _________________________________________________________
c
c       Formats

 110    format(/, '  Namext; File in, extension, and file out',/,
     1           '  In:  ', a256,/,'  Ext: ', a3,/,'  Out: ',a256)
c
c _________________________________________________________
c

        end

