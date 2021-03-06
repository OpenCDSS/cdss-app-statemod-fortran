c outpltw - generates a plot file of well data
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

      subroutine outpltw(igui, istop, cplot)
c
c
c _________________________________________________________
c	    Program Description
c
c       Outpltw; It generates a plot file of well data
c
c _________________________________________________________
c     Update History
c
c
c rrb 2021/04/18; Miscellaneous updates to compile without warnings
c
c _________________________________________________________
c	     Documentation
c
c              iplot = 0 not plotting
c              iplot = n diversion, instream or gage ID to plot
c
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
      character  cplot*12            
c
c _________________________________________________________
c		Step 1; Initialize
c
c rrb 2021/04/18; Miscellaneous updates to compile without warnings
      is=0
                           
      write(6,*) ' Subroutine Outpltw'
      write(6,*) ' '             
      call flush(6)
      
c
c
c		Set number of output values 1x in StateM.for
c	        and store in common.inc      
      ndiv = ndivO        
      nres = nresO
      ndivw= nwelO
      
C
C-------------------------------------------------------------------
C
C------  Well Summary 
C
C-------------------------------------------------------------------
c
c              Find well ID
      nw=0
      do id=1,numdivw
        if(cdividw(id).eq.cplot) then
          nw = id
          is=idvstaw(nw)
        endif
      end do

      if(nw.eq.0) then
        write(io99,*) '  Otupltw; Problem well ID not found ',cplot
        goto 230
      endif
c
c              Print title 
c
c rrb 2021/04/18; Compiler warning
cx140 write(9,190) iystr, iyend, cunitm2, cyr1
      write(9,190) iystr, iyend, cunitm2, cyr1
c
c               Get Data
c-------------------------------------------------------------------

      do 180 iy=iystr,iyend
        call year(iy, iyrmo, imomo, cyr1)

        do i=1,ndivw
          dat1t(i) = 0.0
        end do

        do 170 im=1,12
c
c         irecs=((iy-iystr0)*12+(im-1))*numsta+is+numtop
          irecs=((iy-iystr0)*12+(im-1))*numdivw+nw+numtop
          read(42,rec=irecs,err=220) (dat1(i),i=1,ndivw)
          write(io99,*) (dat1(i),i=1,ndivw)

c         cx = cu         
c         if(iresop.ne.1) cx=cu*mthday(im)
           
          do 160 i=1,ndivw
c           dat1(i)=dat1(i)*cx
            dat1(i)=dat1(i)*fmo(im)
            dat1t(i) = dat1t(i) + dat1(i)
  160     continue
c 
c               Print well data 
          write(9,200) cplot, cstaid(is), iyrmo(im), xmonam(im), 
     1                 (dat1(j), j=1,ndivw)
  170   continue
c
c               End Year Loop      
  180   continue
c
c rrb 2021/04/18; Clean up  
        return
c
c
c _________________________________________________________
c
c
c        Formats
  190   format(i5,',', i5,',',a5,',',a5,','/
c    1  '            ,            ,       ,   ,       ,  Structu,',
c    1  're Data ,        ,     Wel,l Water ,Use     ,    Well,',
c    1  ' Water S,ource   ,',/
c    1  'Structure   ,River       ,       ,   ,_______,_________,',
c    1  '________,________, _______,________,________, _______,',
c    1  '________,________,',/
     1  '            ,            ,       ,   ,',
     1  '   Total,      CU,    From,    From,    From,   Total,',
     1  '   Total,      CU,',   
     1  '   Total,      To,   Total,         ,  Total,',
     1  '    From,    From,    From,    From,   Total,',/

     1  'ID          ,ID          , Year,   Mo,',  
     1  '  Demand,  Demand,    Well,      SW,    Soil,  Supply,',
     1  '   Short,   Short,',      
     1  '      CU,    Soil,  Return,    Loss,     Use,',
     1  '   River,  GwStor, Salvage,    Soil,  Source,',/
     1   2('___________ ,'), 2(' ____,'), 18(' _______,'))
  200  format(2(a12,','),i5,',', 2x, a3, ',',20(f8.0,','))
c
c rrb 2021/04/18; Compiler warning & clean up
cx210  return
c
  220  write(6,*)  '   Outpltw; Requested data .gt. binary file size'
       write(99,*) '   Outpltw; Requested data .gt. binary file size'
c
c               Error Warning
  230 write(6,240) 
      write(99,250) 
      call flush(6)
  240 format('    Stopped in Outpltw',/,
     1       '    See the *.log file')
  250 format('    Stopped in Outpltw')
      if(igui.eq.0) then
        write(6,*) 'Stop 1' 
        call flush(6)
        call exit(1)

        stop 
      else
        istop=2
        return
      endif
c
c _________________________________________________________
c
      
      END
