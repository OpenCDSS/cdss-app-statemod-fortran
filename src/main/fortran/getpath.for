c getpath - finds the path for an input file
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

        subroutine getpath(maxfn, filrsp,fpath1)
c
c
c _________________________________________________________
c	Program Description
c
c       Getpath; It finds the path for an input file
c
c _________________________________________________________
c
c       Update History               
c
c rrb 00/08/04; Added maxfn and revised file name from 72 to 256
c
c _________________________________________________________
c	Dimensions
        character filrsp*256, fpath1*256, x*1
c jhb added precomiler macro to determine the path separator
c rrb 2014-11-24 
cx #if __unix__
cx      character(len=1), parameter :: path_sep='/'
cx#elif _WIN32
      character(len=1), parameter :: path_sep='\'
cx#else
cx      character(len=1), parameter :: path_sep='/'
cx#endif
c     
c
c _________________________________________________________
c		Step 1; Initilize
c

c !!!           PC Specific                
        x='\'
c     
c !!!           SGI Specific                
c       x='/'
c jhb added precomiler macro to determine the path separator
        x = path_sep
        fpath1=' '
c
c		iout =	0 no details        
c			1 details
c			2 summary
        iout=2
        if(iout.eq.1) write(99,110)

        do i=1,maxfn
          ii=maxfn+1-i
          if(filrsp(ii:ii).eq.x) then
            do j=1,ii
              fpath1(j:j) = filrsp(j:j)
            end do
            goto 120
          endif
        end do
c
c rrb 99/06/15; Check to see if filename > 8 characters or < 0
  120   j=0
        j1=0
        do i=ii+1,maxfn
          if(filrsp(i:i).eq.'.')  j1=1
          if(j1.eq.0 .and. filrsp(i:i).ne.' ') j=j+1
        end do
c       write(6,*) ' Getpath; ', ii,j
c       stop
c
c rrb 00/08/05; Allow one character name
c rrb 00/08/05; Allow up to 16 character names
c       if(j.lt.1 .or. j.gt.8) then
        if(j.lt.0 .or. j.gt.16) then
          write(6,*) ' Getpath; Problem with filename, see *.log',ii,j
          write(6,130) filrsp, fpath1
          goto 999
        endif

        if(iout.eq.2) write(99,130) filrsp, fpath1
        
  110   format(/,72('_'),/ '  Getpath; ')
        
  130   format(/,72('_'),/
     1   '  Getpath; ',/
     1   '    File Name:       ',a256,/,
     1   '    Path:            ',a256)

        return
 999    write(6,*) 'Stop 1'
        call flush(6)
        call exit(1)

        stop
        end



