c oprinout - prints results of reading data in Oprinp
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
      Subroutine OprinOut(nlog, maxopr, k,
     1    ityopr1, cidvri,
     1    ciopde,  iopdes1, iopdes2,
     1    ciopso1, iopSou1, iopsou2,
     1    ciopso2, iopsou3,iopsou4, creuse, ireuse1,
     1    oprloss1, oprlimit, iopSou5, iopSou6, iopsou7,
     1    cdivtyp1, intern, cntern, cAssoc, cAssoc2, cAssoc3)
c
c _________________________________________________________
c       Program Description
c
c       OprinOut; It prints results of reading data in Oprinp.f
c
c
c_________________________________________________________________
c
c       Update History
c
c rrb 2021/04/18; Compiler warning
c _________________________________________________________        
c       Dimensions
c
      dimension intern(maxopr,10), cntern(10)
      character*12 cidvri,  ciopde,  ciopso1, ciopso2, ciopso3,
     1             ciopso4, ciopso5, cntern,  creuse, cdivtyp1,
     1             cAssoc,  cAssoc2, cAssoc3
c
c _________________________________________________________
c
c
c rrb 2021/04/18; Compiler warning
      ciopso3=' '
      ciopso4=' '
      ciopso5=' '
c      
c       Step 1; Print operational rule data
c
        write(nlog,2020) ityopr1, cidvri, ityopr1,    
     1    ciopde, iopdes1,  iopdes2, 
     1    ciopso1, iopSou1, iopsou2,
     1    ciopso2, iopsou3, iopsou4, creuse, ireuse1,
     1    oprloss1,oprlimit,
     1    cAssoc,  iopSou5, cAssoc2, iopSou6, cAssoc3,
     1    iopSou7, cdivtyp1
        
        do i=1,10
          if(intern(k,i).ne.0) then
            write(nlog,2021) cntern(i), intern(k,i)
          endif  
        end do
c
c _________________________________________________________        
        return
c
c _________________________________________________________        
        
 2020   format(/,60('_'),/
     1  '  OprinOut; Detailed output for operating rule type ', i5,/
     1  10x, 'ID and Type       = ', a12, i5,/
     1  10x, 'Destination       = ', a12, i5,/        
     1  10x, 'Destination Acct  = ', 12x, i5,/             
     1  10x, 'Source 1          = ', a12, i5,/
     1  10x, 'Source 1 Acct     = ', 12x, i5,/             
     1  10x, 'Source 2          = ', a12, i5,/
     1  10x, 'Source 2 Acct     = ', 12x, i5,/             
     1  10x, 'cReuse            = ', a12, i5,/
     1  10x, 'Loss              = ', 12x, f6.0,/
     1  10x, 'OprLimit          = ', 12x, f6.0,/
     1  10x, 'Associated Rule 1 = ', a12, i5,/
     1  10x, 'Associated Rule 2 = ', a12, i5,/ 
     1  10x, 'Associated Rule 3 = ', a12, i5,/              
     1  10x, 'Diversion type    = ', a12)
                                
 2021   format(                 
     1  10x, 'Carrier           = ', a12, i5)
c
c _________________________________________________________        
      stop
      end
