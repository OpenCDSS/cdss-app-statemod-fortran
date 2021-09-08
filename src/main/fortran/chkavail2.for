c chkavail2 - prints the entire avail array
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

       subroutine ChkAvail2(nlog, ifirst, icx, nchkA, maxsta, numsta, 
     1   fac, avail)
c
c _________________________________________________________
c	Program Description
c
c       Chekavail2; It prints the contents of array Avail for
c                   detailed checking
c
c _________________________________________________________
c	Update History
c		NA
c
c _________________________________________________________
c
c	Documentation
c		nlog    output file #
c   ifirst  counter for printing header
c   icx    	calling routine
c   nchkA   calling location in routine ics
c		maxsta	dimension for maximum number of stations
c   numsta  number of stations
c
c		avail   available flow array 
c   fac     factor to convert to af/mo
c
c _______________________________________________________     
     
       dimension avail(maxsta)

       ifirst=ifirst+1
       if(ifirst.eq.1) write(nlog,200) icx, nchkA, (n, n=1,20)

       write(nlog,210) ifirst, icx, nchkA, (avail(n)*fac, n=1,numsta) 
     
 200   format(/,
     1 ' ChkAvail2; Called by subroutine ', i5,' nchkA = ', i5,/,
     1 '    #  icx     nchkA', 20('  Avail-',i2),/
     1 ' ____  ___ _________', 20(' _________'))
 210   format(2i5, i10, 20f10.2(/,20x10f8.2))

       return
       stop
       end
