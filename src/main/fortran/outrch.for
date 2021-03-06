c outrch - print preliminary river data (22) based on River Reach data (*.rir)
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
       subroutine OutRch(nreach)   
c
c
c _________________________________________________________
c	Program Description
c
c      OutRch; Print Preliminary River Data (22)
c		           based on River Reach data (*.rir)
c
c	             Called by Xdebug.f     
c
c
c_________________________________________________________________
c
c               Update History
c
c rrb 2021/04/18; Compiler warning
c _________________________________________________________
c	              Documentation
c	        Rchid(is) Reach ID for station is
c _________________________________________________________
c	              Dimensions
c
      include 'common.inc'
      
       character rec1*1, rec2*2, rec12*12,  
     1   crch4*4, rchIdX*12, rchIdY*12      
C
c _________________________________________________________ 
c                                  
c rrb 2021/04/18; Compiler warning 
      rchidy=' '

c		Step 1; Initialize Reach Data
      iout=0
      write(nlog,*) ' OutRch;  numsta, nreach', numsta, nreach
      cRch4='Str '
      if(nreach.gt. maxrch) goto 920
      
c
c _________________________________________________________
c		Step 2; Set reach based on stream gage locations
      write(22,200)
      
      do ng=1,nreach
        if(iout.eq.1) write(nlog,*) 'OutRch;  ng = ', ng                
        write(nlog,201) ng, rchIdR(ng), rchNameR(ng)
        write(22,201) ng, rchIdR(ng), rchNameR(ng)
      end do
c
c _________________________________________________________
c		Step 4; Initialize all nodes to the default reach (ngageX)
c			 this is required for the last few
c			 nodes that do not have a downstream gage
      write(22,202)
      do is=1,numsta
        iRch(is)=nreach
        RchID(is)=rchIdR(nreach)
      end do      
c
c _________________________________________________________
c		Step 4; Assign reach to each stream node (is) 
      rchIdX=RchidR(nreach)
     
      do 100 is=1,numsta
        
        NDNS=NDNNOD(IS)    
        ISS=IS
        if(iout.eq.1) then
          write(nlog,*) ' OutRch; cstaid = ', is, cstaid(is), ndns
        endif
        
        DO n=1,NDNS
          rec12=cstaid(iss)
          
          do ng=1,nreach
              IF(nRchEnd(ng).eq.iss) then            
            
              if(iout.eq.1) then
                write(nlog,*) ' '
                write(nlog,*) ' OutRch; ', is, rec12, ng, RchIDR(ng)
              endif
              
              if(ng.le.9) then
                write(rec1,'(i1)') ng
                rchIdX(7:7) = '0'
                rchIdX(8:8) = rec1
              else              
                write(rec2,'(i2)') ng
                rchIdX(7:8) = rec2
              endif
              
              iRch(is)=ng
              RchID(is)=rchIdX
              
              write(22,230)  cstaid(is),RchNameR(ng),
     1          iRch(is),RchID(is),is
     
              if(iout.eq.1) then
                write(nlog,230)cstaid(is),RchNameR(ng),
     1          iRch(is),RchID(is),is
              endif
              
              goto 100
            endif
          end do  
          
          ISS=IDNCOD(ISS)
        end do    
        write(22,230)  cstaid(is),RchNameR(ng),
     1          iRch(is),RchID(is),is
        if(iout.eq.1) then
          write(nlog,230)cdivid(is),RchNameR(ng),
     1      iRch(is),RchID(is),is
        endif
 100  continue
c
c _________________________________________________________
c 					Return
      return

c
c _________________________________________________________
c 		Formats      
 200  format(
     1 '#',/
     1 '# ;',/
     1 '# Reach data (*.xrh & *.rch) based on the following data',/
     1 '# Provided in the River Reach File (*.rir)',/
     1 '# ',/
     1 '#    # Reach ID     Gage at end of Reach',/
     1 '# ____ ___________  _______________________')
     
 201  format('#',i5,1x, a12, 1x, a24)     
 202  format(
     1 '#',/
     1 '# Str ID       Reach Name                    Rch # ',
     1 'Reach ID          #',/
     1 '# ____________ __________________________ ________ ',
     1 '______________ ____')
     
cx 204  format(
cx     1 '#',/
cx     1 '#                                         ',
cx     1 'Goes To        Goes To                    ',/
cx     1 '# Reach ID     Reach Name                 ',
cx     1 'Reach ID       Reach Name                 ',
cx     1 'At Stream ID',/
cx     1 '# ____________ __________________________ ',
cx     1 '______________ __________________________ ',
cx     1 '______________',/
cx     1 'Reach_Data')
cx
cx 206  format(2('"',a12,'"',1x '"',a24,'"',1x), '"',a12,'"')      
cx     
cx 210  format(    
cx     1 a12, 1x, a12, 1x, a24)
cx
cx 220  format(
cx     1 '#',/  
cx     1 '# ID           Name                       Rch ID   ',
cx     1 'Reach Name',/
cx     1 '#------------exb------------------------exb------ex',
cx     1 'b-----------e')
cx     
 230  format(
     1 '"', a12, '"', 1x, '"',a24,'"', 1x,i8, 1x,'"',a12'"',i5)    
c
c _________________________________________________________
c
c               Error Tracking

c910  write(nlog,911) cdivid(nd) 
c911  format(/, 72('_'),/,
c    1 ' OutRch; Problem cannot locate Ditch ID ',a12,/
c    1 '         in a reach',/
c    1 '         Recommend you review the Reach data in *.rch')
c     goto 9990
      
 920  write(nlog,921) maxrch, nreach
 921  format(/, 72('_'),/,
     1 ' OutRch; Problem the # of reaches             = ', i5,/
     1 '         is less than the number of gages + 1 = ', i5,/
     1 '         Recommend you revise the reach dimension')
      goto 9990
      
 9990 write(6,9991) 
      write(nlog,9992) 
      call flush(6)
 9991 format('  Stopped in OutRch, see the log file (*.log)')
 9992 format(/72('_'),/,'  OutRch; Stopped in GetRch')
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)
     
      stop     
      end
