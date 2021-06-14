c outrev - prints reservoir evaporation (*.xev) for CU anlysis
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
      SUBROUTINE OUTrev
c
c
c _________________________________________________________
c	Program Description
c
c       Outrev; It prints reservoir evaporation (*.xev) for CU anlysis
c _________________________________________________________
c
c       Update History
c
c
c rrb 2021/04/18; Miscellaneous updates to compile without warnings
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
c                           
c
c _________________________________________________________
c		Step 1; Initialize

      write(6,*) ' Subroutine Outrev'
      write(6,*) ' '
c
c rrb 2021/04/18; Compiler warning
      iexit=0
      if(iexit.gt.0) goto 300
c
c 2019/03/27; Add detailed output for checking
c             iout=0 no details
c                  1 details      
      iout=0
c                                 
c     cu=1.0
c     if(iresop.eq.2) cu=factor
c     if(iresop.eq.3) cu=factor*0.001                        
c
c               nres = # of output values in file, 
c               nev = location of evap data
c
c rrb 00/06/22; Correction
c     nres = 21
c     nev = nres-9
c
c rrb 2005/11/29; River and Carrier Loss
c     nres = 24
c     nev = 12
c
c rrb 2019/03/27; Update # of values in reservoir output file
cx    nres = 26
      nres = nresO
      if(nresO.ne.29) then
        write(nlog,260) 'Reservoirs', 29, nresO
        goto 9999
      endif
      
      
      nev = 14
      nrsactx=nrsact+numown
c
c rrb 2019/03/25; Check
      if(iout.eq.1) then
        write(nlog,*) '  OutRev; nrsact, numown, nrsactx'
        write(nlog,*) '  OutRev;', nrsact, numown, nrsactx
      endif
            
      if(numres.eq.0) then
         write(36,*) 'No active reservoirs for the current job'
      else
        call outtop(36,0,21)                  
      endif

      do 220 iy=iystr,iyend
c
c rrb 2019/03/25; Initialize to 1
cx    ir1 = 0
      ir1 = 0

        do 190 ir=1,numres
          if(iressw(ir).eq.0) goto 190
          ir1=ir1+1
c      
          dat2t(13) = 0.0
c
c
c               Month Loop
          do 180 im=1,12
c           cx = cu         
c           if(iresop.ne.1) cx=cu*mthday(im)

c
c rrb 12/12/95; Variable report year for reservoirs
c
c rrb 06/06/96; Include header information
c           irecr=((iy-iystr0)*12+(im-1))*nrsactx+ir1            
            irecr=((iy-iystr0)*12+(im-1))*nrsactx+ir1+numtop
c
c rrb 2019/03/25; check
            if(iout.eq.1) then
              write(nlog,*) '  Outrev;',
     1          cresid(ir), ir, nrsactx, nowner(ir+1), nowner(ir),
     1          ir1, irecr  
            endif              
            
            read(44,rec=irecr,err=250) (dat2(i),i=1,nres)
c
c rrb 2019/03/26; Update for additional rows in reservoir output                    
cx            ida  = dat2(nres-1) 
cx            nacc = dat2(nres)
c
c rrb 2021/04/18; Compiler warning
cx          ida  = dat2(nres-2) 
cx          nacc = dat2(nres-1)
            ida  = nint(dat2(nres-2)) 
            nacc = nint(dat2(nres-1))
            
c           write(99,*) ' Outrep2; ida, nacc', ida, nacc
c             dat2t(im) = dat2(nev)*cx
              dat2t(im) = dat2(nev)*fmo(im)
              dat2t(13) = dat2t(13) + dat2t(im)
c
c               End Month Loop      
  180       continue
c 
c               Print one reservoir one year
           write(36,182) iy, cresid(ir), (dat2t(im), im=1,13), 
     1                         resnam1(ir)
  182      format(i4, 1x, a12, 13f8.0,       9x, a24)
c
c               Skip over subaccounts
          ir0=ir1
          if(nacc-ida.gt.1) ir1 = ir1 + nacc-ida -1
c
c rrb 2019/03/25; check
          if(iout.eq.1) then
            write(nlog,*) '  OutRev;   ir, nres, ir0, nacc, ida, ir1'
            write(nlog,*) '  OutRev;', ir, nres, ir0, nacc, ida, ir1
          endif
c
c               End Reservoir Loop
  190     continue

c               End Year Loop
  220 continue
c
  300  return
c
c
c _________________________________________________________
c
c               Error Messages
c
  250  write(6,*)  '   Outrev; Requested data exceeds binary file size'
       write(99,*) '   Outrev; Requested data exceeds binary file size'
      goto 9999
      
  260  format(
     1 ' OutRev; Problem key output variables for ', a10, 
     1 ' were developed',/
     1 10x,'Based on ', i5, ' output variables but ndivO = ', i4,/
     1 10x,'Recommend you revise OutRev appropriately')
c
 9999 write(6,*) 'Stop 1' 
      call flush(6)
      call exit(1)

c
c _________________________________________________________
c
      stop
      END

