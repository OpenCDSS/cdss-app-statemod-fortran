c chkavail
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
       subroutine ChkAvail(nlog, icx, nchkA, maxsta, 
     1   avail, q, n1, n2, n3, fac)
     
       dimension avail(maxsta)

       iout=0
c      nchkA=nchkA+1
cx     if(nchkA.eq.1) write(nlog,200) icx, nchkA
       if(iout.eq.1) write(nlog,200) icx, nchkA

       write(nlog,210) nchkA, q*fac, n1, avail(n1)*fac, n2, 
     1  avail(n2)*fac, n3, avail(n3)*fac

 200   format(/,
     1 ' ChkAvail; Called by subroutine ', i5,' nchkA = ', i5,/
     1 '    #    Divert   N1  Avail(1)   N2  Avail(2)   N3  Avail(3)',/
     1 ' ____ _________ ____ _________ ____ _________ ____ _________')
 210   format(20(i5, f10.2))

       return
       stop
       end
