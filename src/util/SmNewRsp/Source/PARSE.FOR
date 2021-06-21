C     Last change:  RRB  28 Aug 2002    3:46 pm
c
        subroutine parse(maxcl, ioptio, ioptio2,
     1             filenx, filelog, filenc)
c        
c       Parse, it parses thru the command line to find
c       1. Response file name
c       2. Request (e.g. - Version -Run, etc.
c
c       Note option to read more than 2 parameters exists but not used.
c
c
c _________________________________________________________
c               Variable Documentaiton
c
c       maxcl = maximum file length (e.g. 127)
c       ioptio = primary option (result)
c       ioptio2 = secondary option (result)
c       filenx  = default file name (*.rsp)
c       filelog = log file name (*.log)
c       filec   = response file name (result)
c
c
        dimension want(15), want2(20), wantx(15), titleh(20)
        
c
c
c _________________________________________________________
c		Dimensions, etc. for Parse
        character filenc*127,  filenx*127,  filelog*72
c
c _________________________________________________________
c		Local dimensions, etc.        
        character command*127, want*12, want2*12, rec12*12, 
     1            getid*12,    wantx*10, titleh*50,
     1            gettyp*12,   getpar*12

        data want/
     1     '-version    ', '-run        ', '-help       ',
     1     '-           ', '-           ', '-           ',
     1     '-           ', '-           ', '-           ', 
     1     '-           ', '-           ', '-           ',
     1     '-           ', '-           ', '-N/A        '/
        data wantx/
     1     '-v          ', '-r          ', '-h          ',
     1     '-           ', '-           ', '-           ',
     1     '-           ', '-           ', '-           ', 
     1     '-           ', '-           ', '-           ',
     1     '-           ', '-           ', '-N/A        '/
        data want2/
     1     '-         ', '-         ',
     1     '-         ', '-         ', '-         ',
     1     '-         ', '-         ', '-         ', 
     1     '-         ', '-         ', '-         ',
     1     '-         ', '-         ', '-         ',
     1     '-         ', '-         ', '-         ',
     1     '-         ', '-         ', '          '/

        data titleh/
     1   ' Version                                          ',
     2   ' Run                                              ',
     3   ' Help                                             ',
     4   '                                                  ',
     5   '                                                  ',
     6   '                                                  ',
     7   '                                                  ',
     8   '                                                  ',
     9   '                                                  ',
     1   '                                                  ',
     1   '                                                  ',
     2   '                                                  ',
     3   '                                                  ',
     4   '                                                  ',
     5   '                                                  ',
     6   '                                                  ',
     7   '                                                  ',
     8   '                                                  ',
     9   '                                                  ',
     2   '                                                  '/

c _________________________________________________________
c
c               Step 1; Initilize
c
        iout = 1
        nin = 20
        ioptio=15
        ioptio2=15
c
c               Maximum want size
c       maxwant=10
        maxwant=12
c
c _________________________________________________________
c
c               Step 2; Get Command Line
c
        call getcl(command)
c
        if(iout.eq.1) then
c         write(6,100) command
          write(99,100) command
          write(99,*) ' '
        endif
c
c		Check for no command line provided
        if(command(1:2) .eq. '  ') then
          write(99,210) 
          write(6,210) 
          goto 900
        endif
                
c
c               Initilize
        getid  = ' '
        gettyp = ' '
        getpar = ' '
        ii = 0
c
c _________________________________________________________
c
c               Step 3; Get control file name
c                       (command is packed to left)
c                       use 'modbal' as a default
        filenc = filenx
c
        if(command(1:1) .ne. '-') then
          filenc = ' '
          do i=1,maxcl
            if(command(i:i) .ne. ' ') then
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
c               Step 4; Get option 1 run type (if any)
c
  110   rec12 = ' '
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
  140   if(rec12.ne.'            ') then
          do i=1,11
            if(rec12.eq.want(i) .or. rec12.eq.wantx(i)) ioptio = i 
          end do                       
c         write(6,*) ' Parse; ioptio = ', ioptio   
c
c _________________________________________________________
c
c               Step 5; Get option 2 run subtype (e.g. -xst if any)
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
              end do
            endif
          end do

  160     if(rec12.ne.'          ') then
            do i=1,nin
              if(rec12.eq.want2(i)) ioptio2 = i
            end do  
          endif
c
c
c _________________________________________________________
c
c               Step 6; Get option 3, the station to plot
c
          rec12 = ' '
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
c               Step 7; Get option 4 data type (e.g. diversion)
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
c               Step 9;  Print help data
c
  190   if(ioptio.eq.3) then
c         write(6,*) ' '
c         write(6,*) ' Primary options are:'
          write(99,*) ' '
          write(99,*) ' Primary options are:'

          do i=1,10
c           write(6,192) want(i), wantx(i)
            write(99,192) want(i), wantx(i) 
          end do

c         write(6,*) ' '
c         write(6,*) ' Secondary options are:'
          write(99,*) ' '
          write(99,*) ' Secondary options are:'

          do i=1,nin
c           write(6, '(i5, 2x, a10, a50)') i,want2(i), titleh(i)
            write(99,'(i5, 2x, a10, a50)') i,want2(i), titleh(i)
          end do
        endif
c
c       Get file name if not in version, help or update mode
c       and not provided
        if(ioptio.le.4 .or. ioptio.ge.8) then
          if(filenc.eq.' ') then
c           write(6,*) 'Enter base file name (statem, yampa, etc)'
c           write(6,*) ' '
            read(5,'(a127)') filenc
          endif
        endif
c
c _________________________________________________________
c
c               Step 10; Print results
       if(iout.eq.1) then
         write(99,200) filenc, want(ioptio), want2(ioptio2),
     1     getid, gettyp, getpar
       endif
c
c _________________________________________________________
c
c               Step 11; Return
c
        return
c
c _________________________________________________________
c
c               Formats
c
  100   format('  Parse; Command line arguement: ',/, a127)
  192   format(2x, a10, ' or ', a10)
  200  format('  Parse Results:',/
     1 '  Response File:           ',a72,/
     1 '  Primary Option:          ',a12,/
     1 '  Secondary Option:        ',a12,/
     1 '  Station (if any):        ',a12,/
     1 '  Data type (if any):      ',a12,/
     1 '  Parameter type (if any): ',a12)
 210   format('  Parse; Problem no command line arguement provided',/
     1        '         See *.log')    
 900    stop    
        end


