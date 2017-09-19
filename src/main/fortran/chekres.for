c
       subroutine chekres(nlog, maxres, in, isub, iyr, mon, nr,
     1                    nowner,curown,cursto,cresid)
c
c _________________________________________________________
c	Program Description
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
c_______________________________________________
c
c       Dimensions
c
        dimension nowner(maxres), curown(maxres), cursto(maxres),
     1            cresid(maxres)
        dimension subtyp(50)
        character cresid*12, cin*6, subtyp*12
        data subtyp/
     1    'Powres', 'Divres',  'Divres',   'Divrpl',   'Resrpl',
     1    'Rsrspu', 'Carrpl',  'OopBook2', 'Powsea',   'Replace',
     1    'Divcar', 'Reoper',  'Ifrrigx',  'Divcar1',  'Sjrip',
     2    'Evasec', 'DirectEx','ResRg1P',  'DivResP2', 'DivrplP',
     1    'DivresR','DivrplR', 'ResRg1 ',  'PowRes2',  'ResRgP',
     3    'WelRech',' ',       ' ',        ' ',        ' ',
     1    ' ',      ' ',       ' ',        ' ',        ' ',
     4    ' ',      ' ',       ' ',        ' ',        ' ',
     1    ' ',      ' ',       ' ',        ' ',        'DivCarL',
     5    ' ',      ' ',       'PowResP    ','DivRplP2   ',' '/

c _________________________________________________________
c		Step 1; Initilize
c
        iout=0
        small2=0.1
        if(in.eq.0) cin='Into  '
        if(in.eq.1) cin='Out of'
        
        if(nr.gt.maxres) then
          write(nlog,*) '  Chekres; Problem nr < maxres', nr, maxres
          goto 9999
        endif  

        if(nr.gt.0) then
          iri=nowner(nr)
          ire=nowner(nr+1)-1

          sum=0.0
          do ir=iri,ire
            sum=sum+amax1(0.,curown(ir))
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
     1 '  Chekres; ',a8, 1x, a6, ' subrountine ', a12,/
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
 9999 write(6,*) '  Stopped in Chekres, see the log file (*.log)'
      write(99,*) '  Stopped in Chekres'
      write(6,*) 'Stop 1' 
      call flush(6)
      call exit(1)

      stop 
        
      end
