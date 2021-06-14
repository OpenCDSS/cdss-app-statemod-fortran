c OutJM - prints John Martin 1980 Operating Plan Results
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
      SUBROUTINE outJM
c
c _________________________________________________________
c	              Program Description
c
c       OutJM; It prints John Martin 1980 Operating Plan Results
c _________________________________________________________
c
c               Approach
c       Copy outWW and revise to do the following.
c       For every year and month loop through the *.opr file and 
c         find a type 53 entry
c       If found, read detailed *.opr file
c       Print JMartin results to 3 files where 
c       File 1 = Initial Allocation to Conservation and Other
c                from Arkansas River and Purgatoire
c       File 2 = Allocation of Conservation (Section II) Account
c       File 3 = Allocation of Other (Section III) Account
c _________________________________________________________
c
c               Update History
c
c rrb 2021/04/18; Compiler warning
c
c 2020/07/28; Revised a typo to the Section III
c               reporting by setting 68.8% to 68.6%          

c 2020/07/23; Revised to calculate total & annual average
c               available flow
c
c 2019/10/28; Revise to include Available Flow in output
c
c 2018/11/17  Created outJM.for
c _________________________________________________________
c
c               Documentation
c
c       coridR(i)  = Subreport title for subreport i
c       coridO(i,j)= Operating rule ID to report for subreport i
c                    element j
c       ioutJM     = detailed print control
c       jout1      = columns of data to print from each type 52 opr rule
c                    equal to oprlimit(i) 
c                    Note jout1 = 8 if it is a type 53 opr rule 
c                    
c       krpt       = report counter
c
c       maxopr2    = number of entries in detailed *.opr file 
c                    set in Statem to 40
c       numopr     = number of operating rights in *.opr file
c                    set in Statem to 3701
c       nomax      = number of columns of output in a subreport
c       nrs        = # of subreports (2)
c
c       rx         = available flow cfs
c       rx1        = available flow af/mo 
c       rx1T       = annual sum of rx1
c       rx1TA      = annual average of rx1
c
c _________________________________________________________
c	              Local Dimensions
c
      include 'common.inc'
      character coridR*12,  coridO*12 
c
      dimension 
     1  dat2A(40), coridR(5), coridO(5,20) 
c
c rrb 2018/10/07; Define variables for monthly average
      dimension divm(13),divt(13)
      data divm/13*0.0/, divt/13*0.0/
c
c _________________________________________________________
c
c               Step 1; Initialize
c
c rrb 2021/04/18; Compiler warning
      nomax=0
c
c rrb 2021/04/18; Compiler warning
      iexit=0
      if(iexit.gt.0) goto 500
c		
c		            ioutJM = 1 detailed output level 1 (less)
c                        2 detailed output level 2 (more)
      ioutJM = 0
c
c              File numbers (nf1, nf2 & nf3)
      nf1=30
c     nf1=99
            
      maxResP=19
      nmax=maxopr2/2
c
      write(6,101) 'OutJM   '
      write(nlog,101) 'OutJM   '
 101  format(/,72('_'),/'  Subroutine ', a8)


c
      small = 0.001
      smalln=-1*small 
c
c rrb 2018/12/08; TEMPORARILY Hard wire report names
c                 Long range plan is to read from file *.rpt
c
c ---------------------------------------------------------             
c rrb 2019/06/16; Initialize 
c
      do j1=1,5
        coridR(j1) = ' '     
        do j2=1,20
          coridO(j1,j2) = ' '
        end do
      end do
c ---------------------------------------------------------             
c rrb 2018/12/08; Set main report name 
c      
      coridR(1) = 'JMartinR-Con'
      coridR(2) = 'JMartinR-Oth'
c
c ---------------------------------------------------------             
c rrb 2018/12/08; Set operating rules in Sub-report 1  
      coridO(1,1) = 'JM-SecII    ' 
      coridO(1,2) = 'JM-SecII-A  ' 
      coridO(1,3) = 'JM-SecII-B  ' 
c
c ---------------------------------------------------------             
c rrb 2018/12/08; Set operating rules in Sub-report 2  
      coridO(2,1) = 'JM-SecII    ' 
      coridO(2,2) = 'JM-SecIII-A ' 
      coridO(2,3) = 'JM-SecIII-B '
      coridO(2,4) = 'JM-SecIII-C '
      coridO(2,5) = 'JM-SecIII-D '   
c     
c     coridO(2,6) = 'JM-SecIII-E '
c     coridO(2,7) = 'JM-SecIII-F '
c
c
c rrb 2018/12/08; Warn user this file is currently Hard Wired
c                 To the Operating rule ID's in the operating
c                 rule file (*.opr) 
      write(nlog,340) 
     1  coridR(1), (coridO(1,j1), j1=1,3),
     1  coridR(2), (coridO(2,j2), j2=1,7)
c
c _________________________________________________________
c
c               Step 2; Print output file Banner
      call outtop(nf1, 1, 66)
c
c
c ---------------------------------------------------------
c               Initialize number of reports (nrs) & report # (krpt)      
      nrs=2
      krpt=0
c
c
c *********************************************************
c _________________________________________________________
c               Step 3; Repeat logic to print second report     
 90   krpt=krpt+1
      do i=1,40
        dat2A(i)=0.0
      end do   
c
c rrb 2020/07/23; Initialize annual average available flow for 
c                 each report 
      rx1TA=0.0    
c
c _________________________________________________________
c               Step 4; Year Loop
c   
        ry=0
        DO 120 IY=IYSTR,IYEND
          ry=ry+1.0
          
          call year(iy, iyrmo, imomo, cyr1)
          do i=1,40
            dat2T(i)=0.0
          end do
c
c rrb 2020/07/23; Initialize total available flow for this
c                 year and report
          rx1T=0.0       
c
c _________________________________________________________
c
c		            Step 5; Month Loop              
          DO 110 IM=1,12
          
            if(ioutJM.ge.1) then
              write(nlog,*) ' '
              write(nlog,*) '  OutJM; ', iyrmo(im), xmonam(im)
            endif
            
            do j=1,40
              dat2(j)=0.0
            end do
c
            fac=fmo(im) 
            
c _________________________________________________________
c
c		            Step 6; Loop for every operating right
            
            j1=0            
            n=0

            nomax=0
c
c rrb 2019/10/28
            rx=0.0
            rx1=0.0
           
            do 100 k=1, numopr 
              if(ioutJM.eq.2) then
                write(nlog,*) '  OutJM; k ityopr(k)', k, ityopr(k)
              endif   
c
c ---------------------------------------------------------
c rrb 2018/12/08; Loop to search for a operating rule ID
c                 and store number of values to report 
c                 (jout1=oprlimit(k) unless its a type 53 rule, 
c                  then set jout1 = 8
              m1=0 
              jout1=0
              
              do m=1,20               
                if(corid(k) .eq. coridO(krpt,m)) then
                  m1=k
c
c                 Set the number of columns to print to oprlimit(k)
c                 unless it is a type 53, then set to 8.
c                                   
c
c rrb 2021/04/18; Compiler warning
cx                jout1=oprlimit(k)
                  jout1=nint(oprlimit(k))
                  if(ityopr(k).eq.53) jout1=8
                  
                  nomax=nomax+jout1                  
c                                    
                  if(ioutJM.ge.1) then
                    write(nlog,*) ' '
                    write(nlog,*) ' OutJM;    ',
     1                ' krpt    k    m  m1 jout1 corid(k)    ',
     1                ' coridO(krpt,m)'
                    write(nlog,'(a12, 5i5,20(1x,a12))') ' OutJM;    ',
     1                krpt, k, m, m1, jout1, corid(k),coridO(krpt,m)
                  endif                
                endif
              end do
c
c _________________________________________________________
c
c rrb 2018/10/07; Step 7 Found an operating rule to print
c                        Read detailed operating rule output from
c                        array divoWW                  
c
                if(ioutJM.ge.1) write(nlog,*) ' '
                
                if(m1.gt.0) then
                n=n+1
                irec1=((iy-iystr0)*12*numopr)+((im-1)*(numopr))+k
                read(39,rec=irec1) (dat1(j), j=1,maxopr2), rx
c
c rrb 2019/10/28; Include Available Flow in output
              if(corid(k).eq. 'JM-SecII    ') rx1=rx*fac
                
                if(ioutJM.ge.1) then             
                  write(nlog,*) ' OutJM;        k    m   m1',
     1              '      rx     rx1'
                  write(nlog,'(a12,3i5,20f8.0)')
     1              '  OutJM;    ', k, m, m1, rx*fac, rx1 
                endif   
c
c rrb 2019/10/28  Detailed Output                
                if(ioutJM.ge.2) then
                  write(nlog,'(2i5, 20f8.0, 20f8.0)') n, jout1, 
     1             (dat1(j)*fac, j=1, nomax), rx, rx1
                endif
c
c ---------------------------------------------------------             
c rrb 2018/11/23; Loop for number of destinations in the operating
c                 rule file (jout1=oprlimit(k)) unless its a type 53 
c                 operating rule when jout1=8
                do j=1,jout1
                  j1=j1+1
c
c ---------------------------------------------------------             
c               Check that j1 array is not exceeded                  
                  if(j1.gt.maxopr2) then  
                    write(nlog,330) 
                    goto 900
                  endif                  
c
c ---------------------------------------------------------             
c               Finally adjust units   
                  dat2(j1)=dat1(j)*fac
c   
c ---------------------------------------------------------             
c							     Calculate annual total (dat2T) & ave annual (dat2A)              
                  dat2T(j1) = dat2T(j1) + dat2(j1)
                  dat2A(j1) = dat2A(j1) + dat2(j1) 
                end do           
c              
c ---------------------------------------------------------
c                 Endif for a operating rule to be reported
              endif                
c              
c ---------------------------------------------------------
c                 End Operating Right loop
  100       continue
                    
c _________________________________________________________ 
c               Step 8; Print 1 month of data
c
c ---------------------------------------------------------
c               Print annual title once per year per plan    
            if(im.eq.1) then
              nomax1=nomax+1
              if(krpt.eq.1) write(nf1,230) coridR(krpt),(i, i=1,nomax1)
              if(krpt.eq.2) write(nf1,250) coridR(krpt),(i, i=1,nomax1)
            endif
c
c ---------------------------------------------------------
c rrb 2020/07/23; Sum the annual and annual average values for
c                 each month and report
            rx1T=rx1T+rx1
            rx1TA=rx1TA + rx1

            if(isigfig.eq.0) then
              write(nf1,260) 
     1        iyrmo(im), xmonam(im), rx1, (dat2(i), i=1,nomax)
            endif
                        
            if(isigfig.eq.1) then
              write(nf1,262) 
     1        iyrmo(im), xmonam(im), rx1, (dat2(i), i=1,nomax)
             endif
            
            if(isigfig.eq.2) then
              write(nf1,264) 
     1        iyrmo(im), xmonam(im), rx1, (dat2(i), i=1,nomax)
            endif
c
c ---------------------------------------------------------
c		f.          Detailed output     
        if(ioutJM.eq.2) then
          write(nlog,*) ' '
          write(nlog,'(a25,1x,i5, 40f8.0)') 
     1         '  outJM (12); np,dat2;', np, rx1, rx1T, rx1TA,
     1         (dat2(i), i=1,nomax)
        endif
c
c ---------------------------------------------------------
c               End Month loop
  110     continue
c
c ---------------------------------------------------------
c		            Step 9; Write annual total once per year per plan  
c
c rrb 2019/10/28; Include Available Flow in output
cx        rx1=-1.

          if(krpt.eq.1) write(nf1,232) 
          if(krpt.eq.2) write(nf1,252) 
c
c rrb 2020/07/23; Revise the following to print rx1t (not rxt)          
          if(isigfig.eq.0) then
            write(nf1,260) 
     1        iyrmo(im), xmonam(im), rx1T, (dat2T(i), i=1,nomax)
          endif
          
          if(isigfig.eq.1) then
            write(nf1,262) 
     1        iyrmo(im), xmonam(im), rx1T, (dat2T(i), i=1,nomax)
          endif
          
          if(isigfig.eq.2) then
            write(nf1,264) 
     1        iyrmo(im), xmonam(im), rx1T, (dat2T(i), i=1,nomax)
          endif 
c
c ---------------------------------------------------------
c               End Year loop
  120   continue
c
c
c _________________________________________________________
c
c		           Step 10; Calculate & print average annual total
        do i=1,nomax
          dat2A(i) = dat2A(i)/ry
        end do 
c
c rrb 2020/07/30; Calculate average annual Available flow 
        rx1TA = rx1TA/ry
c
c _________________________________________________________
c
c			         Step 11; Print Annual Average  
c
c rrb 2020/07/23; Revise the following to print rx1TA, not rx1
c
        write(nf1,*) ' '        
        if(krpt.eq.1) write(nf1,232) 
        if(krpt.eq.2) write(nf1,252) 
        
        if(isigfig.eq.0) then
          write(nf1,260) 
     1      0, 'AVE', rx1TA, (dat2A(i), i=1,nomax)
        endif
c        
        if(isigfig.eq.1) then
          write(nf1,262) 
     1      0, 'AVE', rx1TA, (dat2A(i), i=1,nomax)
        endif         
c        
        if(isigfig.eq.2) then
          write(nf1,264) 
     1      0, 'AVE', rx1TA, (dat2A(i), i=1,nomax)
        endif         
c
c _________________________________________________________
c
c               Step 12; Go Back to do second report
       if(ioutJM.eq.2) then
         write(nf1,*) 'OutJM; krpt & nrs = ', krpt, nrs
         write(nlog,*) 'OutJM; krpt & nrs = ', krpt, nrs
       endif
c
c *********************************************************
       if(krpt.lt.nrs) goto 90    
cc
c _________________________________________________________
c
c               Step 13; Return
c
  500   return
c
c _________________________________________________________
c
c               Formats
c   
  230   format(/,
     1  ' JMartin 1980 Storage Program Section II Conservation ',a12,/
     1  '             Avail     Ark                                ',
     1  '    Purg Conserv   Other',
     1  '  Col_II  Kan_II Ft Bent  Keesee',
     1  '   Amity   Lamar    Hyde  Manvel XY&Gram Buffalo SStubbs'/,         
     1  ' Year   Mo',
     1  '    Flow    Flow    AveB    AveE    PctB    PctE',
     1  '    Flow   Total   Total (60.0%) (40.0%)',
     1  ' ( 9.9%) ( 2.3%) (49.5%)  (19.8%) ( 1.3%)( 2.4%) ( 5.1%)',
     1  ' ( 8.5%)  (1.2%)',/
     1  10x, 20('    (', i2,')'),/
     1  ' ____ ____', 20(' _______'))

  232  format( 
     1  ' ____ ____', 20(' _______'))
     
c
c 2020/07/28; Revised a typo to the Section III
c               reporting by setting 68.8% to 68.6%          
  250   format(/,
     1  ' JMartin 1980 Storage Program Section III Other ',a12,/     
     1  '             Avail     Ark                                ',
     1  '    Purg Conserv   Other T-Loss+    WWSP',
     1  '  T-Loss COl III  Kan II Ft Bent  Keesee   Amity',
     1  '   Lamar    Hyde  Manvel XY&Gram Buffalo SStubbs',/
     1  ' Year   Mo',
     1  '    Flow    Flow    AveB    AveE    PctB    PctE',
     1  '    Flow   Total   Total',
     1  ' (35.0%) (65.0%)  (1.7K) (68.6%) (31.4%)',
     1  ' ( 9.9%) ( 2.3%) (49.5%) (19.8%) ( 1.3%)',
     1  ' ( 2.4%) ( 5.1%) ( 8.5%)  (1.2%)',/
     1  10x, 23('    (', i2,')'),/     
     1  ' ____ ____',
     1  ' _______ _______ _______ _______ ______ _______',
     1  ' _______ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______')

c
c Summer allocation percents
cx     1  ' (19.6%) ( 4.6%) ( 0.0%) (39.2%) ( 2.6%) ( 4.8%)',
cs     1  ' (10.1%) (16.8%) ( 2.4%)',/

  252  format( 
     1  ' ____ ____', 22(' _______'))

  260   format(i5, 2x, a3, 40f8.0) 
  262   format(i5, 2x, a3, 40f8.1) 
  264   format(i5, 2x, a3, 40f8.2) 
               
cx320   format(/,'  OutJM; Reading Binary Operating Rule file'/
cx   1              ' outJM (8a); np, pID,  ifound, k, ityopr iop',
cx   1              ' i46, p46',/, a14, 1x,a12, 1x, 6i5,20f8.0)

  330  format(/,'  OutJM; Problem array size maxopr2 = 40 exceeded'/
     1          '  Recommend you revise Statem and common.inc')
 
 340  format(/'  OutJM;',/
     1  '    Warning the *.xjm output file was designed',/
     1  '    to report the John Martin 1980 Operating Plan',/
     1  '    results using specific values for the operating rule',/
     1  '    IDs.  A future enhancement may allow different IDs to',/
     1  '    to be provided in a report file (*.rpt).',//
     1  '    For Report: ', a12,/
     1  '    The Operating Rule IDs should be:',/
     1       3(16x,a12,/),/,
     1  '    For Report: ', a12,/
     1  '    The Operating Rule IDs should be:',/
     1       20(16x, a12,/))
c
c _________________________________________________________ 
c        Error Messages
c
  900 write(6,*)  '  Stopped in outJM; see the log file (*.log)'
      write(nlog,*) '  Stopped in outJM'                        
      write(6,*) 'Stop 1' 
      call flush(6)
      call exit(1)

      stop 
      END
