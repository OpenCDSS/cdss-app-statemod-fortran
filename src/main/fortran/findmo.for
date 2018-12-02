c findmo - returns a month pointer (icm1) and 
c          month number (imnum) for a given year type (cyr1)
c          and month descriptor (rec3).
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

       subroutine findmo(nlog,cyr1,rec3,icm1,imnum)
c
c
c _________________________________________________________
c	Program Description
c
c		It returns a month pointer (icm1) and 
c               month number (imnum) for a give year type (cyr1)
c		and month descriptor (rec3)
c
c		nlog = log file number
c               cyr1 = year type (CYR, WYR, IYR)
c		rec3 = month name (Oct, Nov)
c		icm1 = month pointer (oct=1 for water year)        
c		imnum  = month number (jan=1)        
c _________________________________________________________
c	Dimensions		
        character cyr1*5, rec3*3
        iout=0
c
c _________________________________________________________
c
c		Set true month identifiers to imnum        
          if(rec3.eq.'Jan') imnum=1
          if(rec3.eq.'Feb') imnum=2
          if(rec3.eq.'Mar') imnum=3
          if(rec3.eq.'Apr') imnum=4
          if(rec3.eq.'May') imnum=5
          if(rec3.eq.'Jun') imnum=6
          if(rec3.eq.'Jul') imnum=7
          if(rec3.eq.'Aug') imnum=8
          if(rec3.eq.'Sep') imnum=9
          if(rec3.eq.'Oct') imnum=10
          if(rec3.eq.'Nov') imnum=11
          if(rec3.eq.'Dec') imnum=12
       
c
c _________________________________________________________
c		Set Calendar year month pointers      
        if(cyr1.eq.'  CYR') then
          if(rec3.eq.'Jan') icm1=1
          if(rec3.eq.'Feb') icm1=2
          if(rec3.eq.'Mar') icm1=3
          if(rec3.eq.'Apr') icm1=4
          if(rec3.eq.'May') icm1=5
          if(rec3.eq.'Jun') icm1=6
          if(rec3.eq.'Jul') icm1=7
          if(rec3.eq.'Aug') icm1=8
          if(rec3.eq.'Sep') icm1=9
          if(rec3.eq.'Oct') icm1=10
          if(rec3.eq.'Nov') icm1=11
          if(rec3.eq.'Dec') icm1=12
        endif
c
c _________________________________________________________
c		Set Water year month pointers      
c        
        if(cyr1.eq.'  WYR') then
          if(rec3.eq.'Jan') icm1=4
          if(rec3.eq.'Feb') icm1=5
          if(rec3.eq.'Mar') icm1=6
          if(rec3.eq.'Apr') icm1=7
          if(rec3.eq.'May') icm1=8
          if(rec3.eq.'Jun') icm1=9
          if(rec3.eq.'Jul') icm1=10
          if(rec3.eq.'Aug') icm1=11
          if(rec3.eq.'Sep') icm1=12
          if(rec3.eq.'Oct') icm1=1
          if(rec3.eq.'Nov') icm1=2
          if(rec3.eq.'Dec') icm1=3
        endif
c
c _________________________________________________________
c		Set Irrigation year month pointers      
c       
        if(cyr1.eq.'  IWR') then
          if(rec3.eq.'Jan') icm1=3
          if(rec3.eq.'Feb') icm1=4
          if(rec3.eq.'Mar') icm1=5
          if(rec3.eq.'Apr') icm1=6
          if(rec3.eq.'May') icm1=7
          if(rec3.eq.'Jun') icm1=8
          if(rec3.eq.'Jul') icm1=9
          if(rec3.eq.'Aug') icm1=10
          if(rec3.eq.'Sep') icm1=11
          if(rec3.eq.'Oct') icm1=12
          if(rec3.eq.'Nov') icm1=1
          if(rec3.eq.'Dec') icm1=2
        endif
        
        if(iout.eq.1) write(nlog,100) cyr1, rec3, imnum, icm1
 100    format('  Findmo; Year type ', a5, ' Month ', a3, 
     1  ' Month # ', i3, ' Month Pointer ', i3)       
        return
        end	
