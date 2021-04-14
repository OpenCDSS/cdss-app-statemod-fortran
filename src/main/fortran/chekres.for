c chekres - checks reservoir storage total vs account totals
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
       subroutine chekres(nlog, maxres, in, isub, iyr, mon, nr,
     1                    nowner,curown,cursto,cresid)
c
c _________________________________________________________
c       Program Description
c
c
c       Chekres; it checks reservoir storage total vs account totals
c
c_______________________________________________
c
c       Update History
c
c rrb 99/05/10; Additional Check
c rrb 02/01/15; Dimension clean up
c
c _____________________________________________________________
c
c       Documentation
c             small1 = small value used for roundoff issues (1.0 af)
c             small2 = small value used for roundoff issues (0.1 af)
c                icx = calling routine (100 + opr rule or 200 + std rule)
c_______________________________________________
c
c       Dimensions
c
        dimension nowner(maxres), curown(maxres), cursto(maxres),
     1            cresid(maxres)
c
c rrb 2018/07/29; Revise to include up to 60 operating rules
cx      dimension subtyp(50)
        dimension subtyp(60)
        character cresid*12, cin*6, subtyp*12
        data subtyp/
     1    'Powres', 'Divres',  'Divres',   'Divrpl',   'Resrpl',
     1    'Rsrspu', 'Carrpl',  'OopBook2', 'Powsea',   'Replace',
     1    'Divcar', 'Reoper',  'Ifrrigx',  'Divcar1',  'Sjrip',
     1    'Evasec', 'DirectEx','ResRg1P',  'DivResP2', 'DivrplP',
     1    'DivresR','DivrplR', 'ResRg1 ',  'PowRes2',  'ResRgP',
     1    'WelRech',' ',       ' ',        ' ',        ' ',
     1    ' ',      ' ',       ' ',        ' ',        ' ',
     1    ' ',      ' ',       ' ',        ' ',        ' ',
     1    ' ',      ' ',       ' ',        ' ',        'DivCarL',
     1    ' ',      ' ',       'PowResP    ','DivRplP2 ',' ',
     1    'FlowRes','DivMultR','WWSP ',    ' ',        ' ',     
     1    ' ',      ' ',       ' ',        ' ',        ' '/

c _________________________________________________________
c              Step 1; Initialize
c
        iout=1
c
c rrb 2018/07/29; Check for operating rule limit
        if(maxoprin.gt.60) goto 900
                
c
c rrb 2015/07/20; Test        
cx      small2=0.1
        small2=10.0
        if(in.eq.0) cin='Into  '
        if(in.eq.1) cin='Out of'
        
        if(nr.gt.maxres) then
          write(nlog,*) '  Chekres; Problem nr < maxres', nr, maxres
          goto 9999
        endif 

        if(nr.gt.0) then
          iri=nowner(nr)
          ire=nowner(nr+1)-1
c
c rrb 2020/07/28; Additional detailed output        
        if(iout.eq.1) then
          write(nlog,*) ' '
          write(nlog,*) ' ChekRes; iyr, mon, nr, cresid(nr),iri,ire'
          write(nlog,*) ' ChekRes;', iyr, mon, nr, cresid(nr),iri,ire
        endif

          sum=0.0
          do ir=iri,ire
            sum=sum+amax1(0.,curown(ir))  
c
c rrb 2015/06/25; Additional output for checking
            if(iout.eq.1) then
              write(nlog,*) '  Chekres: cresid(nr), nr, ir,',
     1              ' curown(ir), sum'   
              write(nlog,*) '  Chekres: ', cresid(nr), nr, ir, 
     1                curown(ir), sum   
            endif                      
          end do
c
c rrb 05/21/97; Handle roundoff concerns between main reservoir
c               and accounts.  If within 1 acft, set total to
c               sum of accounts
          x=abs(sum-cursto(nr))
          if(x.lt.1.0) cursto(nr)=sum

          x=abs(sum-cursto(nr))
          
          if(iout.eq.1) then
            if(in.eq.0) write(nlog,102)
            write(nlog,100) ' FYI    ',cin, subtyp(isub), x, 
     1        iyr, mon, nr, cresid(nr), sum, cursto(nr), x
          endif
         
c
c               Problem
          if(x.gt.small2) then     
            ! TODO smalers 2021-04-07 print out specific message during troubleshooting
            ! - use 5-digit label starting with 99 for troubleshooting
            if(iout.eq.1) then
              write(nlog,99001) x, small2
99001           format(/,' Checkres; Problem x (',f12.2,
     +          ') > small2(',f12.2,')')
            endif
            write(nlog,100) 'Problem ', cin, subtyp(isub), 
     1        x, iyr, mon, nr, cresid(nr), sum, cursto(nr), x

            write(nlog,110)
            do ir=iri,ire
              write(nlog,120) ir, curown(ir)
            end do
            write(nlog,130) sum
            write(nlog,131) cursto(nr)
            write(nlog,132) sum-cursto(nr)
c
            write(nlog,*) '  Stopped in Chekres, see log file (*.log)'
            write(6,*)    '  Stopped in Chekres, see log file (*.log)'
            write(6,*) 'Stop 1'
            call flush(6)
            call exit(1)

            stop 
          endif
        endif


 100    format(/,
     1 '  Chekres; ',a8, 1x, a6, ' subroutine ', a12,/
     1 '           Delta = ', f10.3,//
     1 '  Chekres; Input data:',/
     1 '  Iyr  Mon   Nr Cresid               Sum      Cursto',
     1 '       Delta',/
     1 ' ---- ---- ---- ------------',3(' -----------'),/,
     1   i5, i5, i5, 1x, a12, 20f12.3)
 102   format(/, 80('_'))    
 110   format(//,
     1  '   ir  cursto(ir)',/
     1  ' ____ ___________')
 120   format(i5, f12.2)
 130   format(' ____ ___________',/
     1        'T_Sum', f12.2)
 131   format('T_Res', f12.2)
 132   format(' ____ ___________',/
     1        'Delta', f12.2)
     

        return
c
c _________________________________________________________
c
c               Print warning
 900  write(nlog,910) 
      write(nlog,920) 
      call flush(6)
 910  format('    Stopped in Chekres',/,
     1       '    See the *.log file')
 920  format('    Stopped in Chekres; number of operating rules > 60')
 
 9999 write(6,*) '  Stopped in Chekres, see the log file (*.log)'
      write(99,*) '  Stopped in Chekres'
      write(6,*) 'Stop 1' 
      call flush(6)
      call exit(1)

      stop 
      
        
      end
