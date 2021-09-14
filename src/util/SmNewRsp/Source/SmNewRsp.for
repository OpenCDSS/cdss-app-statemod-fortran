c
c
c       SmNewRsp; It reads a sequential StateMod input file
c               and prints it as a random input file
       dimension ifileNum(50), fileName(50), FileType(50),
     1           fileID(50), ifileNx(50)

       character filena*256,
     1   fileType*40, FileName*256, FileId*5,
     1   fileT1*40,   fileN1*256

       character ver*24, vdate*10
c
c      Data for parse     
       character filenx*127, filelog*127, filenc*127
        ! Start and end of simulation, saved in common block for check in 'datinp'.
        integer iystrCli, iyendCli
        character getid*12

        ver='1.2.0'
        vdate='2021-09-12'

        iin=20
        nout=8
        nlog=99
        maxfile = 50
        open(nout, file='SmNewRsp.out', status='unknown')
        open(nlog, file='SmNewRsp.log', status='unknown')
        
        write(nout,530) ver, vdate
        write(nlog,530) ver, vdate
c
c       Get old StateMod response file from the command line        
        maxcl=127
        filenx='-1'
        filelog='SmNewRsp.log'
! smalers 2021-09-12 update for latest parse routine.
!       call parse(maxcl, ioptio, ioptio2,
!    1             filenx, filelog, filenc)
        call parse(nlog, ioptio, ioptio2, filenc, getid,
     +                   iystrCli, iyendCli)

        ! The value of filenc will be the response file without the extension.
        filenc = trim(filenc) // '.rsp'
c
c       Open old response file.
        if(filenc(1:2).eq.'-1') then
          write(6,520)
          write(nlog,520) 
          goto 900     
        endif
        write(6,*) 'Opening old rsp file: ', filenc
        write(nlog,*) 'Opening old rsp file: ', filenc
        open(iin, file=filenc, status='old', iostat=ierror)
        if ( ierror .ne. 0 ) then
          write(6,*) 'Error opening response file'
          write(nlog,*) 'Error opening response file'
          call exit(1)
        endif
        
c       Make new response file.
        call getfn2(iin, nlog, nout, infile, maxfile)
        write(6,510)
 510    format(
     1    ' SmNewRsp; Successful Execution',/
     1    '         New StateMod response file = SmNewRsp.out',/
     1    '         Log file = SmNewRsp.log')
 520    format(
     1    ' SmNewRsp; Problem',/
     1    '         Response file not found',/
     1    '         To execute the program type SmNewRsp responsefile')
 530    format(
     1 '# _______________________________________________________'/
     1 '#',/     
     1 '#  SmNewRsp',/
     1 '#  Create a new variable format StateMod',/
     1 '#  response file from an old format)',/
     1 '#  State of Colorado'/
     1 '#  Version: ',a24,/,
     1 '#  Last revision date: ',a10,/
     1 '#',/
     1 '# _______________________________________________________')
     
 900    stop
        end
