c parse - parses thru the command line to find request, for gfortran
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
        ! The 'common.inc' common data are not included so must pass values
        ! back to the calling code.

        ! subroutine parse(nlog, maxfn, ioptio, ioptio2, filenc, getid)   ! smalers, 2021-06-14, maxfn not used
        subroutine parse(nlog, ioptio, ioptio2, filenc, getid,
     +                   iystrCli, iyendCli)
c        
c
c _________________________________________________________
c       Program Description
c
c       Parses the command line, using code that is compatible with gfortran.
c       Parsing occurs by searching the entire command line for
c       recognized substrings.
c       Each option rescans the entire command line,
c       which is different than parsing approaches for other languages.
c
c _________________________________________________________
c       Update History
c
c smalers 2021-06-14 Add -iystr Year and -iyend Year options.
c
c rrb 2021/04/18; Miscellaneous updates to compile without warnings
c
c rrb 2001/01/02; Added option 10 (baseflows with variable efficiency)
c               and option 11 (baseflow with variable efficiency and
c               wells with sprinklers get used first).
c
c rrb 2002/05/07; Added gettyp and getpar for daily plotting capability.
c
c rrb 2003/06/02; Revise to recognize -NoLog as a secondary
c               option and to print to a log file only if
c               -NoLog is off.
c rrb 2005/01/06  Add plan output type 21a.
c jhb 2005/06/14  Convert to the gfortran method of accessing command
c               line arguments.  Convert the command line into a
c               string starting at the 1st (not 0th) argument
c               to reuse as much of the old code as possible
c               use functions get_command, get_command_argument
c
c               ioptio  = primary option type
c               ioptio2 = secondary option type
c               filenc  = base file name without '.rsp' extension
c
c
c _________________________________________________________
c       Dimensions
c
        dimension want(15), want2(25), wantx(15), titleh(25)
c
c rrb 00/08/04; Revise maximum command line length
        character command*512, want*12, want2*12, rec12*12,
     1            filenc*256, getid*12,  wantx*12, titleh*50,
     1            gettyp*12, getpar*12, rec1*1

        ! Variables for the new command line argument functions.
        character(len=512) :: cmdline
        integer iarg
        ! smalers 2021-09-12 increase size because passing rsp file with full path
        !character(len=100) :: dummy
        !character(len=100) :: arg
        character(len=256) :: dummy
        character(len=256) :: arg
        integer iarglen
        integer istatus

        logical foundPrimary
        logical foundSecondary

        ! Start and end of simulation, saved in common block for check in 'datinp'.
        integer iystrCli, iyendCli

        ! Control whether new parsing, which has flexible order.
        logical doFlexibleOrder
        character(len=20) :: clog    ! Text after --log=

        ! The following are primary command line arguments using long names.
        ! The number and order should agree with 'wantx' below.
        data want/          ! Interactive menu 0 is 'Stop'
     1     '-baseflow   ',  ! Interactive menu 1
     1     '-simulate   ',  ! Interactive menu 2
     1     '-report     ',  ! Interactive menu 3
     1     '-check      ',  ! Interactive menu 4
     1     '-version    ',  ! Interactive menu 5
     1     '-help       ',  ! Interactive menu 6
     1     '-update     ',  ! Interactive menu 7
     1     '-simulatex  ',  ! Interactive menu 8
     1     '-baseflowx  ',  ! Interactive menu 9
     1     '-warranty   ',
     1     '-conditions ',
     1     '-contact    ',
     1     '-test       ',
     1     '            ',
     1     'N/A         '/
        ! The following are primary command line arguments using short names.
        ! The number and order should agree with 'want' above.
        data wantx/         ! Interactive menu 0 is 'Stop'
     1     '-base       ',  ! Interactive menu 1
     1     '-sim        ',  ! Interactive menu 2
     1     '-rep        ',  ! Interactive menu 3
     1     '-chk        ',  ! Interactive menu 4
     1     '-v          ',  ! Interactive menu 5
     1     '-h          ',  ! Interactive menu 6
     1     '-up         ',  ! Interactive menu 7
     1     '-simx       ',  ! Interactive menu 8
     1     '-basex      ',  ! Interactive menu 9
     1     '-w          ',
     1     '-c          ',
     1     '-cx         ',
     1     '-t          ',
     1     '            ',
     1     'N/A         '/
        ! The following are secondary command line arguments using short names.
        ! The number and order should agree with 'titleh' below.
        ! See the 'report.for' code for interactive prompts that should match these reports.
        data want2/
     1     '-xbn      ',  ! Interactive menu 1
     1     '-xnm      ',  ! Interactive menu 2
     1     '-xwb      ',  ! Interactive menu 3
     1     '-xwr      ',  ! Interactive menu 4
     1     '-xsu      ',  ! Interactive menu 5
     1     '-xrg      ',  ! Interactive menu 6
     1     '-xdg      ',  ! Interactive menu 7
     1     '-xrc      ',  ! Interactive menu 8
     1     '-xdc      ',  ! Interactive menu 9
     1     '-xcu      ',  ! Interactive menu 10
     1     '-xrx      ',  ! Interactive menu 11
     1     '-xsc      ',  ! Interactive menu 12
     1     '-xst      ',  ! Interactive menu 13, "xst" = standard reports (multiple reports)
     1     '-xsh      ',  ! Interactive menu 14
     1     '-xdl      ',  ! Interactive menu 15
     1     '-xsp      ',  ! Interactive menu 16
     1     '-xwg      ',  ! Interactive menu 17
     1     '-xwc      ',  ! Interactive menu 18
     1     '-xds      ',  ! Interactive menu 19
     1     '-NoLog    ',  ! Interactive menu 20
     1     '-xpl      ',  ! Interactive menu 21
     1     '-xwp      ',  ! Interactive menu 22
     1     '-xpw      ',  ! Interactive menu 23
     1     '-xrw      ',  ! Interactive menu 24
     1     'NA'/

        ! The following are secondary command line arguments using descriptions.
        ! The number and order should agree with 'want2' above.
        ! The information is printed when -help argument is given.
        data titleh/
     1   ' Base flow information at stream gauge locations  ',  ! Interactive menu 1
     2   ' Detailed node accounting                         ',  ! Interactive menu 2
     3   ' Water Budget                                     ',  ! Interactive menu 3
     4   ' Water rights list sorted by basin rank           ',  ! Interactive menu 4
     5   ' Water Supply Summary                             ',  ! Interactive menu 5
     6   ' Reservoir Graph                                  ',  ! Interactive menu 6
     7   ' Diversion Graph                                  ',  ! Interactive menu 7
     8   ' Reservoir Comparison                             ',  ! Interactive menu 8
     9   ' Diversion Comparison                             ',  ! Interactive menu 9
     1   ' Consumptive Use Summary for the CU model or other',  ! Interactive menu 10
     1   ' River data Summary                               ',  ! Interactive menu 11
     2   ' Stream Comparison                                ',  ! Interactive menu 12
     3   ' Standard diversion (*.xdd) and reservoir (*.xre) ',  ! Interactive menu 13
     4   ' Shortage Summary                                 ',  ! Interactive menu 14
     5   ' Structure List                                   ',  ! Interactive menu 15
     6   ' Selected Parameter                               ',  ! Interactive menu 16
     7   ' Well Graph                                       ',  ! Interactive menu 17
     8   ' Well Comparison                                  ',  ! Interactive menu 18
     9   ' Daily Selected Parameter                         ',  ! Interactive menu 19
     2   ' NoLog                                            ',  ! Interactive menu 20
     1   ' Plan                                             ',  ! Interactive menu 21
     2   ' Well Structure to Plan                           ',  ! Interactive menu 22
     3   ' Plan to Well Structure                           ',  ! Interactive menu 23
     4   ' Reach Water Balance Report                       ',  ! Interactive menu 24
     5   'NA'/

c _________________________________________________________
c
c
c
c rrb 2021/04/18; Compiler not used or initialize
      ! maxfn = maxfn  ! smalers 2021-06-14, maxfn not used
      j1 = 0
      j = 0

      ! smalers 2021-06-14 enable flexible command line order to allow new options
      ! - this is now the default
      ! - Lahey 'parse.for' may not have new options unless code is updated consistently
      doFlexibleOrder = .TRUE.
c
c     Step 1; Initialize
c
c rrb 2021/04/18; Compiler not used
      ! Set iexit to 1 to skip parsing, used for troubleshooting to exit now.
      iexit = 0
      if(iexit.gt.0) goto 500
c
c     iout = logging level
c            0 no details
c            1 details (use for debugging)
c            2 summary
      iout = 1
        
      if(iout.eq.1) write(nlog,*) 'Parse; parsing command line'
c
c     Get command line data
      nin = 25
c
c rrb 00/08/04; Maximum command length
      maxcl = 512
c
c     Maximum want size (a12)
      maxwant=12
      nwant=15

c     Initialize

      ioptio  = 0   ! Primary option, will cause interactive prompt if not specified.
      ioptio2 = 0   ! Secondary option, optional, such as for report type.
      getid  = ' '  ! Station identifier for reporting.
      gettyp = ' '  ! Data type for reporting.
      getpar = ' '  ! Parameter type for reporting.
      filenc = ' '  ! Response file name, will cause interactive prompt if not specified.

      if ( doFlexibleOrder .eqv. .TRUE. ) then
        write(nlog,*) '  Parsing command line using flexible order.'
        ! Parse with flexible order introduced in version 16.00.48.
        ! This fully uses modern Fortran command line parsing.
        iarg = 0  ! Argument 0 is the program name.
        do
          ! Increment the argument being processed.
          iarg = iarg + 1
          call get_command_argument(iarg, arg, iarglen, istatus)
          if ( iarglen == 0 ) then
            ! No more arguments.
            exit
          endif
          ! Have an argument to interpret.
          arg = trim(arg)
          write(nlog,*) '  arg: "', arg, '"'

          ! See if a primary option by checking the option array.
          ! Check the short and long option names.
          foundPrimary = .FALSE.
          do i=1,nwant
            if(arg.eq.trim(want(i)) .or. arg.eq.trim(wantx(i))) then
              ! Primary option was found.
              ioptio = i
              if(iout.eq.1) write(nlog,*) '    ioptio = ', ioptio
              foundPrimary = .TRUE.
              exit
            endif
          end do
          if ( foundPrimary .eqv. .TRUE. ) then
            ! No need to check more options.
            cycle
          endif

          ! See if a secondary option.
          foundSecondary = .FALSE.
          do i=1,nin
            if(arg.eq.trim(want2(i))) then
              ! Secondary option was found.
              ioptio2 = i
              ! -xsc (12) is the same report routine as -xdc (9)
              ! -xsh (14) is the same report routine as -xcu (10)
              ! -xsu ( 5) is the same report routine as -xcu (10)
              if(ioptio2.eq.12) ioptio2 = 9
              if(ioptio2.eq.14) ioptio2 = 10
              if(ioptio2.eq. 5) ioptio2 = 1 0
 
              if(iout.eq.1) then
                write(nlog,*) '    ioptio2 = ', ioptio2
              endif
              foundSecondary = .TRUE.
              exit
            endif
          end do
          if ( foundSecondary .eqv. .TRUE. ) then
            ! No need to check more options.
            cycle
          endif

          ! See if other specific options.

          if ( arg == '-iystr' ) then
            iarg = iarg + 1
            call get_command_argument(iarg, arg, iarglen, istatus)
            arg = trim(arg)
            write(nlog,*) '    arg: ', arg
            if ( istatus .ne. 0 ) then
                write(6,*) '-iystr is missing year YYYY'
                write(nlog,*) '    -iystr is missing year YYYY'
            else
              read(arg,*,iostat=istat) iystrCli
              if ( istat == 0 ) then
                if ( iout == 1 ) then
                  write(nlog,*) '    iystr from command line; ',iystrCli
                endif
              else
                write(6,*) '-iystr uses invalid year: ', arg
                write(nlog,*) '    -iystr uses invalid year: ', arg
              endif
            endif
            ! Go to the next argument.
            cycle

          else if ( arg .eq. '-iyend' ) then
            iarg = iarg + 1
            call get_command_argument(iarg, arg, iarglen, istatus)
            arg = trim(arg)
            write(nlog,*) '    arg: ', arg
            if ( istatus .ne. 0 ) then
                write(6,*) '-iyend is missing year YYYY'
                write(nlog,*) '    -iyend is missing year YYYY'
            else
              read(arg,*,iostat=istat) iyendCli
              if ( istat == 0 ) then
                if ( iout == 1 ) then
                  write(nlog,*) '    iyend from command line; ',iyendCli
                endif
              else
                write(6,*) '-iyend uses invalid year: ', arg
                write(nlog,*) '    -iyend uses invalid year: ', arg
              endif
            endif
            ! Go to the next argument.
            cycle

          else if ( arg .eq. '-station' ) then
            iarg = iarg + 1
            call get_command_argument(iarg, arg, iarglen, istatus)
            arg = trim(arg)
            write(nlog,*) '    arg: ', arg
            if ( istatus .ne. 0 ) then
                write(6,*) '-station is missing id'
                write(nlog,*) '    -station is missing id'
            else
              getid = trim(arg(1:12))
              if ( iout == 1 ) then
                write(nlog,*) '    station id from command line; ',getid
              endif
            endif
            ! Go to the next argument.
            cycle

          ! TODO smalers 2021-06-14 not sure if this is the argument - documentation is lacking
          else if ( arg .eq. '-datatype' ) then
            iarg = iarg + 1
            call get_command_argument(iarg, arg, iarglen, istatus)
            arg = trim(arg)
            write(nlog,*) '    arg: ', arg
            if ( istatus .ne. 0 ) then
                write(6,*) '-datatype is missing type'
                write(nlog,*) '    -datatype is missing type'
            else
              gettyp = trim(arg(1:12))
              if ( iout == 1 ) then
                write(nlog,*) '    data type from command line; ',gettyp
              endif
            endif
            ! Go to the next argument.
            cycle

          ! TODO smalers 2021-06-14 not sure if this is the argument - documentation is lacking
          else if ( arg .eq. '-parameter' ) then
            iarg = iarg + 1
            call get_command_argument(iarg, arg, iarglen, istatus)
            arg = trim(arg)
            write(nlog,*) '    arg: ', arg
            if ( istatus .ne. 0 ) then
                write(6,*) '-parameter is missing type'
                write(nlog,*) '    -parameter is missing type'
            else
              getpar = trim(arg(1:12))
              if ( iout == 1 ) then
                write(nlog,*) '    parameter from command line; ',getpar
              endif
            endif
            ! Go to the next argument.
            cycle

          ! New logging option as of 17.x for more flexible troubleshooting, for example:
          !   --log=ioutp=1
          else if ( (iarglen > 6) .AND. (arg(1:6) == '--log=' )) then
            ! Use the original argument length below to get the substring.
            clog=arg(7:(len(arg)-6))
! TODO smalers 2021-09-12 Comment out since don't need for SmNewRsp.
!           call logsetlevel(clog,ierror)
!           if ( ierror > 0 ) then
!             write(nlog,*)
!    +        'Error in syntax (should be similar to --log=ioutp=1): ',
!    +        arg
!           endif

          ! Other dash option is an error if not recognized above.

          else if ( arg(1:1) .eq. '-' ) then
            write(6,*) 'Unrecognized option: ', arg
            write(nlog,*) '    Unrecognized option: ', arg
            cycle

          ! Anything else is assumed to be the response file name.
          ! Detect space or period.
          ! Filename 'filenc' does NOT contain the extension.
          ! Extension '.rsp' is ignored if specified (will be appended later).
          else if ( arg(1:1) .ne. '-' ) then
            ! Not a dash option so assume the response file.
            ipos = index(arg,".rsp")
            if ( ipos > 0 ) then
              ! Remove the extension.
              filenc = trim(arg(1:(ipos - 1)))
            else
              ! No extension
              filenc = trim(arg)
            endif
            if ( iout == 1 ) then
              write(nlog,*) '    response file without .rsp; ', filenc
            endif
            ! Skip to the next parameter.
            cycle
          endif

        end do ! End looping through arguments

        ! Additional defaults
        if ( filenc .eq. '' ) then
          ! Traditional default.
          ! filenc = 'statem'
          ! TODO smalers 2021-06-14 seems to work better if no default because prompt for name.
        endif

      else
        ! Parse the old way where order is required and code is difficult to understand.
        write(nlog,*) '  Parsing command line using fixed order.'
c
c _________________________________________________________
c
c               Step 2; Get Command Line
c
        !call getcl(command)
        ! Convert to gfortran method to create command arg string.
        ! First get the entire command line.
        call get_command(cmdline)
        if(iout.eq.1)
     +    write(nlog,*) ' Command line including program: ', cmdline
        ! Next get the command line minus the program name
        ! by finding the length of the program name - 0th arg.
        iarg = 0
        call get_command_argument(iarg,dummy,iarglen,istatus)
        command = cmdline(iarglen+2:511)
        if(iout.eq.1) then
          write(nlog,*) ' Command line without program: ', command
          write(nlog,*) ' value: ', dummy
          write(nlog,*) ' iarglen: ', iarglen
          write(nlog,*) ' istatus: ', istatus
        endif
c
c rrb 00/08/04; File length limit
c rrb 03/06/02; Print at bottom if NoLog option is not on
        if(iout.eq.1 .or. iout.eq.2) then
          write(6,100) command
          write(nlog,100) command
          write(nlog,*) ' '
        endif
c
c       Find response file name, use 'statem' as a default.
        filenc = 'statem' 
c
c       Initialize
        ii = 0  ! Character position in the command line.
c
c _________________________________________________________
c
c               Step 3; Find response file name
c                       (command is packed to left)
c
c rrb 2008/09/16; Allow operation without a control file name
c rrb 2019/01/31; Detailed output
c       if(iout.eq.1) write(nlog,*) ' Parse; 0, command(1:1) ',
c    1                0, command(1:1)
     
        if(command(1:1) .ne. '-') then
          ! No dash so assume the response file name.
          ! Can be before or after other options.
          filenc = ' '
          do i=1,maxcl
            if(iout.eq.1) then
              ! Log the character being processed.
              write(nlog,20) i, i, command(i:i)
20            format('  Parse; command(',i2,':',i2,') = ',a1)
            endif

            ! Detect space or period.
            ! Filename 'filenc' does NOT contain the extension.
            ! Extension '.rsp' is ignored if specified (will be assumed later).
            if(command(i:i) .ne. ' ' .and. command(i:i).ne.'.') then
              filenc(i:i) = command(i:i)
              ii = i
            else
              ! Done processing filename.
              goto 110
            endif
          end do
        endif
c
c _________________________________________________________
c
c               Step 4; Get the primary option type (if any)
c
  110   if(iout.eq.1) write(nlog,*)
     1  ' Parse; filenc (filename without extension) = ', filenc
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
  140   if(iout.eq.1) write(nlog,*) ' Parse; Option 1 = ', rec12
c
c ---------------------------------------------------------
c rrb 2008/09/10; Revise to handle bad data better  
        if(rec12.eq.'            ') then
          write(nlog,*) ' Parse; No primary option provided.'
          write(nlog,*) ' Parse; Will prompt user with menu.'
c         goto 500
          goto 400
        endif
        
        if(rec12.ne.'            ') then
          do i=1,nwant
            if(rec12.eq.want(i) .or. rec12.eq.wantx(i)) ioptio = i
          end do
c         write(nlog,*) ' Parse; ioptio = ', ioptio
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
                if(iout.eq.1) write(nlog,*) ' Parse; rec12 = ', rec12
              end do
            endif
          end do

  160     if(iout.eq.1) write(nlog,*) ' Parse; Option 2 = ', rec12

  
          if(rec12.ne.'          ') then
            if(iout.eq.1) write(nlog,*) ' Parse; ioptio2 = ', ioptio2
            
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
            
            if(iout.eq.1) write(nlog,*) ' Parse; ioptio2 = ', ioptio2
c
c rrb 2003/10/27; Store test number if option 2 is not already found
            if(ioptio2.eq.0) then
              j0=j1-1
              rec1=rec12(j0:j0)
              if(iout.eq.1) write(nlog,*) ' Parse; rec12 = ', rec12
              if(iout.eq.1) write(nlog,*) ' Parse; rec1 = ', rec1
              read(rec1, *,end=161,err=161) ioptio2
              if(iout.eq.1) write(nlog,*) ' Parse; ioptio2 = ', ioptio2
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
c               Step 9; Get the parameter (e.g. diversion)
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
c         Endif for command line option provided.
        endif

        endif ! iFlexibleOrder

        ! Below here is responding to values parsed above.

c
c _________________________________________________________
c
c               Step 10;  Print help data
c
c
c rrb 2021/04/18; Compiler not used
cx190   if(ioptio.eq.6) then
        if(ioptio.eq.6) then
  
c         open(nlog,file='statem.log', status='unknown')
          write(nlog,*) ' '
          write(nlog,*) ' Primary options are:'

          do i=1,nwant
            write(nlog,192) want(i), wantx(i)
          end do

          write(nlog,*) ' '
          write(nlog,*) ' Secondary options are:'
          do i=1,nin
            write(nlog, '(i5, 2x, a10, a50)') i,want2(i), titleh(i)
          end do
c
c rrb 03/06/02; Print only if -NoLog option is not on
          if(ioptio2.ne.20) then
            write(nlog,*) ' '
            write(nlog,*) ' Primary options are:'

            do i=1,nwant
              write(nlog,192) want(i), wantx(i)
            end do

            write(nlog,*) ' '
            write(nlog,*) ' Secondary options are:'

            do i=1,nin
              write(nlog,'(i5, 2x, a10, a50)') i,want2(i), titleh(i)
            end do
          endif
        endif
c
c       Get file name if not in version, help or update mode and not provided.
        if(ioptio.le.4 .or. ioptio.ge.8) then
          if( filenc.eq.' ') then
            ! The filename was not entered on the command line so prompt for it.
            write(6,*)
     1     'Enter base file name without .rsp (statem, yampa, etc)'
            write(nlog,*) ' '
            read(5,'(a256)') filenc
          endif
        endif

c
c _________________________________________________________
c
c               Step 11; Print results
       if(iout.eq.1 .or. iout.eq.2) then
c        write(nlog,100) command
c
c rrb 03/06/02; Print only if -NoLog option is not on
         if(ioptio2.ne.20) then
c          write(nlog,100) command
           write(nlog,*) ' '
           if(ioptio2.eq.0) ioptio2=nin
           if(ioptio.eq.0) ioptio=nwant
           write(nlog,200) filenc, want(ioptio), want2(ioptio2),
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
c      if(ioptio2.ne.20) close (nlog)
c
c _________________________________________________________
c
c               Step 13; Return
c
 400    if(iout.eq.1) write(nlog,*) ' Parse; Return'
        return
c
c _________________________________________________________
c
c               Formats
c
  100   format('  Parse; Command line: ',/, 2x, a256)
c
c rrb 2021/04/18; Compiler not used
cx101   format(i1)
  
  192  format(2x, a12, ' or ', a12)
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
      write(nlog,520)
      call flush(6)
 510  format('  Stopped in Parse')
 520  format(72('_'),/
     1 '  Parse; Stopped in Parse, see the log file (*.log)')
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop
c _________________________________________________________
c
      END
