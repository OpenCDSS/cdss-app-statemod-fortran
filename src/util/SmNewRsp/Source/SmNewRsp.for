c
c
c       SmNewRsp; It reads a sequential StateMod input file
c               and prints it as a random input file
       dimension ifileNum(50), fileName(50), FileType(50),
     1           fileID(50), ifileNx(50)

       character filena*256,
     1   fileType*40, FileName*256, FileId*5,
     1   fileT1*40,   fileN1*256
c
c		Data for parse     
       character filenx*127, filelog*127, filenc*127, vdate*10

        ver=1.1
        vdate='2005/01/10'

        iin=20
        nout=8
        nlog=99
        maxfile = 50
        open(nout, file='SmNewRsp.out', status='unknown')
        open(nlog, file='SmNewRsp.log', status='unknown')
        
        write(nout,530) ver, vdate
        write(nlog,530) ver, vdate
c
c		Get old StateMod response file from the command line        
        maxcl=127
        filenx='-1'
        filelog='SmNewRsp.log'
        call parse(maxcl, ioptio, ioptio2,
     1             filenx, filelog, filenc)
c
c		Open old response file
        if(filenc(1:2).eq.'-1') then
          write(6,520)
          write(nlog,520) 
          goto 900     
        endif
        open(iin,  file=filenc,  status='old')
        
c
c		Make new response file
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
     1 '#        SmNewRsp (create a new, variable format StateMod',/
     1 '#                response file from an old format)',/                                          '/
     1 '#        State of Colorado'/
     1 '#        Version: ',f5.2,/,
     1 '#        Last revision date: ',a10,/
     1 '#',/
     1 '# _______________________________________________________')
     
 900    stop
        end
