c
c *********************************************************
c
        subroutine parse(nlog, maxfn, ioptio, ioptio2, filenc, getid)   
c        
c
c _________________________________________________________
c	Program Description
c
c       Parse, it parses thru the command line to find request
c
c _________________________________________________________
c       Update History
c
c rrb 01/01/02; Added option 10 (baseflows with variable efficiency)
c               and option 11 (baseflow with variable efficiency and
c               wells with sprinklers get used first)
c
c rrb 02/05/07; Added gettyp and getpar for daily plotting capability
c
c rrb 03/06/02; Revise to recognize -NoLog as a seconary
c               option and to print to a log file only if
c               -NoLog is off
c rrb 05/01/06  Add plan output type 21
c
c               ioptio  = primary option type
c               ioptio2 = secondary option type
c               filenc  = base file name
c
c
c _________________________________________________________
c	Dimensions
c
        dimension want(15), want2(25), wantx(15), titleh(25)
c
c rrb 00/08/04; Revise maximum command line length
        character command*127, want*12, want2*12, rec12*12, filenc*256,
     1            getid*12,  wantx*10, titleh*50,
     1            gettyp*12, getpar*12, rec1*1

        data want/
     1     '-baseflow   ', '-simulate   ',
     1     '-report     ', '-check      ', '-version    ',
     1     '-help       ', '-update     ', '-simulatex  ', 
     1     '-baseflowx  ', '-warranty   ', '-conditions ',
     1     '-contact    ', '-test       ', '            ',
     1     'N/A         '/
        data wantx/
     1     '-base       ', '-sim        ',
     1     '-rep        ', '-chk        ', '-v          ',
     1     '-h          ', '-up         ', '-simx       ', 
     1     '-basex      ', '-w          ', '-c          ',
     1     '-cx         ', '-t          ', '            ',
     1     'N/A         '/
        data want2/
     1     '-xbn      ', '-xnm      ', '-xwb      ',
     1     '-xwr      ', '-xsu      ',
     1     '-xrg      ', '-xdg      ', '-xrc      ', 
     1     '-xdc      ', '-xcu      ', 
     1     '-xrx      ', '-xsc      ', '-xst      ', 
     1     '-xsh      ', '-xdl      ', 
     1     '-xsp      ', '-xwg      ', '-xwc      ', 
     1     '-xds      ', '-NoLog    ',
     1     '-xpl      ', '-xwp      ',
     1     '-xpw      ', '-xrw      ', 'NA'/

        data titleh/
     1   ' Base flow information at stream gauge locations  ',
     2   ' Detailed node accounting                         ',
     3   ' Water Budget                                     ',
     4   ' Water rights list sorted by basin rank           ',
     5   ' Water Supply Summary                             ',
     6   ' Reservoir Graph                                  ',
     7   ' Diversion Graph                                  ',
     8   ' Reservoir Comparison                             ',
     9   ' Diversion Comparison                             ',
     1   ' Consumptive Use Summary for the CU model or other',
     1   ' River data Summary                               ',
     2   ' Stream Comparison                                ',
     3   ' Standard diversion (*.xdd) and reservoir (*.xre) ',
     4   ' Shortage Summary                                 ',
     5   ' Structure List                                   ',
     6   ' Selected Parameter                               ',
     7   ' Well Graph                                       ',
     8   ' Well Comparison                                  ',
     9   ' Daily Selected Parameter                         ',
     2   ' NoLog                                            ',
     1   ' Plan                                             ',
     2   ' Well Structure to Plan                           ',
     3   ' Plan to Well Structure                           ',
     4   ' Reach Water Balance Report                       ',          
     5   'NA'/

c _________________________________________________________
c
c               Step 1; Initilize
c
c		iout = 	0 no details
c			1 details
c			2 summary
        iout = 2
        
        if(iout.eq.1) write(99,*) '  Parse'
c
c               Get command line data
        nin = 25
c
c rrb 00/08/04; Maximum command length
        maxcl = 127
c       maxcl = 256
c
c               Maximum want size (a12)
        maxwant=12
        nwant=15
c
c _________________________________________________________
c
c               Step 2; Get Command Line
c
        call getcl(command)
c
c rrb 00/08/04; File length limit
c rrb 03/06/02; Print at bottom if NoLog option is not on
        if(iout.eq.1 .or. iout.eq.2) then
          write(6,100) command
          write(99,100) command
          write(99,*) ' '
        endif
c
c               Find control file name, use statem as a default
        filenc = 'statem' 
c
c               Initilize
        ioptio  = 0
        ioptio2 = 0
        getid  = ' '
        gettyp = ' '
        getpar = ' '
        ii = 0
c
c _________________________________________________________
c
c               Step 3; Find control file name
c                       (command is packed to left)
c
c rrb 2008/09/16; Allow operation without a control file namd
        if(command(1:1) .ne. '-') then
          filenc = ' '
          do i=1,maxcl
c
c rrb 2004/08/23; Allow a full response name with .rsp
c           if(command(i:i) .ne. ' ') then
            if(command(i:i) .ne. ' ' .and. command(i:i).ne.'.') then
              filenc(i:i) = command(i:i)
              ii = i
            else
              goto 110
            endif
          end do
        endif
c
c _________________________________________________________
c
c               Step 4; Get the primary option type (if any)
c
  110   if(iout.eq.1) write(nlog,*) ' Parse; filenc = ', filenc
        rec12 = ' '
        do i=ii+1,maxcl
          if(command(i:i).eq. '-') then
c
c               Option provided, store command
            j1 = 0
c           do j=i,i+9
            do j=i,i+maxwant-1
              j1 = j1+1
              if(command(j:j).eq.' ')  goto 140
              rec12(j1:j1) = command(j:j)
            end do  
          endif
        end do
c
c               Option provided, get type
  140   if(iout.eq.1) write(99,*) ' Parse; Option 1 = ', rec12
c
c ---------------------------------------------------------
c rrb 2008/09/10; Revise to handle bad data better  
        if(rec12.eq.'            ') then
          write(99,*) ' Parse; Problem no option provided'
c         goto 500
          goto 400
        endif
        
        if(rec12.ne.'            ') then
          do i=1,nwant
            if(rec12.eq.want(i) .or. rec12.eq.wantx(i)) ioptio = i 
          end do                       
c         write(99,*) ' Parse; ioptio = ', ioptio   
c
c _________________________________________________________
c
c               Step 5; Get the secondary option type (e.g. -xst if any)
c
          rec12 = ' '
          ii = j
          do i=ii+1,maxcl
            if(command(i:i).eq. '-') then
c
c               Option provided, store command
              j1 = 0
c             do j=i,i+9
              do j=i,i+maxwant-1
                j1 = j1+1
                if(command(j:j).eq.' ')  goto 160
                rec12(j1:j1) = command(j:j)
                if(iout.eq.1) write(99,*) ' Parse; rec12 = ', rec12
              end do
            endif
          end do

  160     if(iout.eq.1) write(99,*) ' Parse; Option 2 = ', rec12

  
          if(rec12.ne.'          ') then
            if(iout.eq.1) write(99,*) ' Parse; ioptio2 = ', ioptio2
            
            do i=1,nin
              if(rec12.eq.want2(i)) ioptio2 = i
            end do  
c
c               -xsc (12) is the same report routine as -xdc (9)
c               -xsh (14) is the same report routine as -xcu (10)
c               -xsu ( 5) is the same report routine as -xcu (10)
            if(ioptio2.eq.12) ioptio2=9 
            if(ioptio2.eq.14) ioptio2=10
            if(ioptio2.eq. 5) ioptio2=10
            
            if(iout.eq.1) write(99,*) ' Parse; ioptio2 = ', ioptio2
c
c rrb 2003/10/27; Store test number if option 2 is not already found
            if(ioptio2.eq.0) then
              j0=j1-1
              rec1=rec12(j0:j0)
              if(iout.eq.1) write(99,*) ' Parse; rec12 = ', rec12
              if(iout.eq.1) write(99,*) ' Parse; rec1 = ', rec1
              read(rec1, *,end=161,err=161) ioptio2
              if(iout.eq.1) write(99,*) ' Parse; ioptio2 = ', ioptio2
            endif
              
          endif
c
c
c _________________________________________________________
c
c               Step 7; Get the station to plot
c
 161      rec12 = ' '
          ii = j
          do i=ii+1,maxcl
            if(command(i:i).eq. '-') then
c
c               Option provided, store command
              j1 = 0
c
c 02/05/09; Skip the - for getid
c             do j=i,i+12
              do j=i+1,i+13
                j1 = j1+1
                if(command(j:j).eq.' ')  goto 162
                rec12(j1:j1) = command(j:j)
              end do
            endif
          end do

  162     if(rec12.ne.'          ') then
            getid = rec12
          endif
c
c _________________________________________________________
c
c               Step 8; Get the data type (e.g. diversion)
c                       currently used by daily *.xds only
c
          rec12 = ' '
          ii = j
          do i=ii+1,maxcl
            if(command(i:i).eq. '-') then
c
c               Option provided, store command
              j1 = 0
              do j=i,i+12
                j1 = j1+1
                if(command(j:j).eq.' ')  goto 164
                rec12(j1:j1) = command(j:j)
              end do
            endif
          end do

  164     if(rec12.ne.'          ') then
            gettyp = rec12
          endif
c
c _________________________________________________________
c
c               Step 9; Get the data type (e.g. diversion)
c                       currently used by daily *.xds only
c
          rec12 = ' '
          ii = j
          do i=ii+1,maxcl
            if(command(i:i).eq. '-') then
c
c               Option provided, store command
              j1 = 0
              do j=i,i+12
                j1 = j1+1
                if(command(j:j).eq.' ')  goto 166
                rec12(j1:j1) = command(j:j)
              end do
            endif
          end do

  166     if(rec12.ne.'          ') then
            getpar = rec12
          endif
c
c             Endif for option provided
        endif

c
c _________________________________________________________
c
c               Step 10;  Print help data
c
  190   if(ioptio.eq.6) then
c         open(99,file='statem.log', status='unknown')
          write(99,*) ' '
          write(99,*) ' Primary options are:'

          do i=1,nwant
            write(99,192) want(i), wantx(i)
          end do

          write(99,*) ' '
          write(99,*) ' Secondary options are:'
          do i=1,nin
            write(99, '(i5, 2x, a10, a50)') i,want2(i), titleh(i)
          end do
c
c rrb 03/06/02; Print only if -NoLog option is not on
          if(ioptio2.ne.20) then
            write(99,*) ' '
            write(99,*) ' Primary options are:'

            do i=1,nwant
              write(99,192) want(i), wantx(i) 
            end do

            write(99,*) ' '
            write(99,*) ' Secondary options are:'

            do i=1,nin
              write(99,'(i5, 2x, a10, a50)') i,want2(i), titleh(i)
            end do
          endif
        endif
c
c       Get file name if not in version, help or update mode
c       and not provided
        if(ioptio.le.4 .or. ioptio.ge.8) then
          if(filenc.eq.' ') then
            write(99,*) 'Enter base file name (statem, yampa, etc)'
            write(99,*) ' '
            read(5,'(a256)') filenc
          endif
        endif

c
c _________________________________________________________
c
c               Step 11; Print results
       if(iout.eq.1 .or. iout.eq.2) then
c        write(99,100) command
c
c rrb 03/06/02; Print only if -NoLog option is not on
         if(ioptio2.ne.20) then
c          write(99,100) command
           write(99,*) ' '
           if(ioptio2.eq.0) ioptio2=nin
           if(ioptio.eq.0) ioptio=nwant
           write(99,200) filenc,want(ioptio), want2(ioptio2),
     1       getid, gettyp, getpar
     
           if(ioptio2.eq.nin) ioptio2=0
           if(ioptio.eq.nwant) ioptio=0
         endif
       endif
c
c _________________________________________________________
c
c               Step 12; Close temporary log file
c
c rrb 03/06/02; Close temporary log file (if used)
c      if(ioptio2.ne.20) close (99)
c
c _________________________________________________________
c
c               Step 13; Return
c
 400    if(iout.eq.1) write(99,*) ' Parse; Return'
        return
c
c _________________________________________________________
c
c               Formats
c
  100   format('  Parse; Command line argument: ',/, 2x, a127)
  101   format(i1)
  
  192  format(2x, a10, ' or ', a10)
  200  format('  Parse Results:',/
     1 '  Response File Name:      ',a256,/
     1 '  Primary Option:          ',a12,/
     1 '  Secondary Option:        ',a12,/
     1 '  Station (if any):        ',a12,/
     1 '  Data type (if any):      ',a12,/
     1 '  Parameter type (if any): ',a12)
     
c
c _________________________________________________________
c               Error Processing

 500  write(6,510) 
      write(99,520) 
      call flush(6)
 510  format('  Stopped in Getctl')
 520  format(72('_'),/
     1 '  Parse; Stopped in Parse, see the log file (*.log)')
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop 
c _________________________________________________________
c
      END     



