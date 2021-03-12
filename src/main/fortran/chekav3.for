c chekav3 - prints the results of a diversion on the array avail.
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
C
      SUBROUTINE chekav3(icode,  ioutA,   maxsta,  numsta, avtemp, 
     1                   AVAIL,  idcd,    nlog,    fac,    idy1, 
     1                   iyrmo1, xmonam1, ccallby, cstaid, stanam1)
c
c
c _________________________________________________________
c	Program Description
c
c       Chekav3; it prints the results of a diversion
c                on the array avail.
c
c       icode = 1; set array avtemp
c               2; compare avail to avtemp
c
c _________________________________________________________
c	              Update History
c		2018/12/16; Created to track the impact of a John Martin Flow
c               (type 53) impact on variable avail.
c _________________________________________________________
c
c	              Documentation
c		  icode	       1; set array avtemp to avail
c                  2; compare avail to avtemp
c     ioutA        0 do not print results
c                  1 do print results
c		  maxsta	     dimension for maximum number of stations
c     numsta       number of downstream stations
c     avtemp       available flow prior to a diversion
c		  avail        available flow
c
c
c _________________________________________________________
c	              Dimensions
c
      DIMENSION AVAIL(maxsta), avtemp(maxsta), cstaid(maxsta),
     1          stanam1(maxsta)
c
c rrb 2018/07/29; Replace subtyp2 with a scalar passed in
      character ccallby*12, xmonam1*4, cstaid*12, stanam1*24
c
c _________________________________________________________
c
c               Step 1; Check array avail
c
      iout=0   
c
c _________________________________________________________
c               Step 2; Set Avtemp
      if(icode.eq.1) then
        do ix=1,numsta
          avtemp(ix) = avail(ix)
        end do
        goto 500 
      endif

c _________________________________________________________
c               Step 3; Print diversion impact on avail 
      if(icode.eq.2) then
c
c rrb 2020/06/07; Additional output
cx      if(ioutA.gt.0) then
        if(ioutA.gt.0.or.iout.gt.0) then
          write(nlog,200) ccallby, idcd     
        
          do ix=1,numsta
            c = avtemp(ix) - avail(ix)
            write(nlog,210) iyrmo1,xmonam1, idy1, ix, cstaid(ix), 
     1        stanam1(ix), avtemp(ix)*fac, c*fac, avail(ix)*fac  
c
c rrb 2019/10/27; Update Avtemp
            avtemp(ix) = avail(ix)
          end do 
          goto 500
        endif  
      endif
c
c _________________________________________________________
c               Step 4; Return
c
  500  return
c
c _________________________________________________________
c               Formats
c
      
  200  format(/,'  ChekAv3; Change of avail from subroutine ', 
     1 a12, ' where idcd = ',i5,/
     1 '  iyr  mon  idy    # River ID    Riv Name                ',
     1 '  avtemp  divert   avail',/
     1 ' ____ ____ ____ ____ ___________ ________________________',
     1 ' _______ _______ _______')  
        
  210  format(i5,1x,a4,i5, i5,1x,a12, a24, 20f8.0)

c
c _________________________________________________________
c               Error Processing
c
      stop
      end

