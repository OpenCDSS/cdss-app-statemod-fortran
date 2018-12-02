c license - write the license/copyright notice
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

      subroutine license(out)

      implicit none
      integer out
      write(out,100)
 100  format(
     1'StateMod Water Allocation Model',/
     1'StateMod is a part of Colorado''s Decision Support Systems ',
     +'(CDSS)',/
     +'Copyright (C) 1994-2018 Colorado Department of Natural ',
     +'Resources',/
     +'',/
     +'StateMod is free software:  you can redistribute it and/or',/
     +'    modify it under the terms of the GNU General Public License '
     +'as published by',/
     +'    the Free Software Foundation, either version 3 of the '
     +'License, or',/
     +'    (at your option) any later version.',/
     +'',/
     +'StateMod is distributed in the hope that it will be useful,',/
     +'    but WITHOUT ANY WARRANTY; without even the implied '
     +'warranty of',/
     +'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  ',
     +'See the',/
     +'    GNU General Public License for more details.',/
     +'',/
     +'    You should have received a copy of the GNU General ',
     +'Public License',/
     +'    along with StateMod.  If not, see ',
     +'<https://www.gnu.org/licenses/>.',/
     +72('_'),//)
      end
