c getver - checks a file for version number and skips up to 1000 other header cards      
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
      subroutine GetVer(nlog, iin2, nfmt, filena)
c
c        nlog = log file
c        iin2 = input file #
c        nfmt = format #
c        filena = filename
c      
      
      character rec132*132, filena*256, rec1*1, rec18*18
      
      iout=0  
c
c rrb 2008/03/12; Set the default to unknown          
c     nfmt=1
      nfmt=0
      
      if(iout.eq.1) write(nlog,*) ' GetVer; iin2, filena ',iin2, filena 

      
 100  read(iin2, '(a132)',end=300, err=300) rec132
        if(iout.eq.1) write(nlog,*) rec132
        
        if(rec132(1:1).ne. '#') then
          backspace(iin2)
          goto 300
        endif
          
        if(rec132(1:18).eq. '#FileFormatVersion') then
          backspace(iin2)
          read(iin2,*,end=400,err=400) rec18, nfmt
        endif
        
        if(rec132(1:19).eq. '# FileFormatVersion') then
          backspace(iin2)
          read(iin2,*,end=400,err=400) rec1, rec18, nfmt
        endif
      goto 100  
      
 300  if(iout.ge.1) then
        write(nlog,310) filena, nfmt
 310  format(/,
     1 '  GetVer; File name    = ', a256,/
     1 '          File version =', i2) 
      endif  
      
      return
      
c
c _________________________________________________________      
c     Error Processing
  400 write(nlog,410) 
  410 format(/,
     1 72('_'),/
     1 '  GetVer; Problem reading version number',/ 
     1 '    Recommend you check the format looks like the',
     1    ' following (note no =)',/,
     1 '    #FileFormatVersion 2 or # FileFormatVersion')
      goto 9999      
c      
c _________________________________________________________      
      
 9999 write(6,900)
      write(nlog,900) 
 900  format(/, 72('_'), /,
     1'  GetVer;  Stopped in GetVer, see the log file (*.log)') 
     
      write(6,*) 'Stop 1'
      call flush (6)
      call exit(1)

      stop       
      end
