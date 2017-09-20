C     Lastd change:  RRB  16 Oct 99   12:18 pm
c
C
      SUBROUTINE GetRes2(IIN, inx, numstax)
c
c       GetRes2; it reads in reservoir data
c
c _________________________________________________________
c
c               Documentation
c
c               iin  =   response file #
c               inx  =   switch
c                        0-read all input data
c                        1-read only *.ctl file
c
c               ichk =  0 do not print detail;
c                       -n do at river id ichk,
c                       + print detailed information (GetRes2)
c                       4 Calls from Execut (execut)
c                       5 Demand & sprinkler (demand)
c                       6 Daily data (daydist)
c                       7 Return flow data (closs via mdainp or datest)
c                       8 Print detailed daily baseflow data to *.xtp
c                       9 Reoperation information from Execut
c		       10 Details on reading operating right data 
c                      20 Override daily ID data for testing (GetRes2)
c                      24 Detailed results of opr rule 24 (downstream
c                         call)
c                      30 Do not print any daily binary results 
c                         see outmon call dayoutr.
c
c                      90 Return flow detailed water use data (return)

c                      91 Print detailed demand data (Bomsec) and well
c                           water right info (welrig)
c                      92 Print detailed soil moisture data (soilm)
c                      99 Enter water right id for call information
c                     100+n Print water right n (see *.xwr for a number)  
c
c _________________________________________________________
c
c               Update History
c
c rrb 2006/04/04; Add ability to read Reservoir Return flow file for
c                 seepage fate
c rrb 04/09/07; Revise diversions to allow multiple owners
c		ndown. Note multiple users are still not
c		allowed.
c rrb 03/08/18; Revise to allow random file read
c
c rrb 99/09/15; Revised instream flows to allow monthly data
c rrb 00/05/30; Revised to allow Irrigation Water Req. data for
c               demand information
c rrb 00/11/10; Revised to read itsfile; annual time series code
c rrb 00/12/04; Revised to add ieffmax; variable efficiency code
c               (0=no, 1=yes)
c
c _________________________________________________________

      include 'common.inc'
c
      DIMENSION ITEMP(numstax)

      dimension mthd(12), xmon(12)
C
      character ch3*3, ch4*4, blank*12, crtnid*12, xmon*4,
     1          recin*256, cgoto2*12, cx*12, rec4*4, rec24*24,
     1          rec1*1, rec12*12, rec32*32,
     1          filenaR*256
     

      write(6,*) ' Subroutine GetRes2'
c
c _________________________________________________________
c
c               Read Reservoir Station (*.res) 
c
c		iout=1 print reservoir details
c		    =2 print reservoir return flow details
      iout=0

      NUMRES=0
      NUMOWN=0
      NUMRAI=0
      NUMEPT=0
      iprintr=0
      iprintr2=0
      blank = '            '                       
      
      small = 0.001
      small2= 0.002
      smalln = -1.0*small
      smalln = smalln
      
      do i=1,maxrtnRP
        nrtnRP(i)=0
      end do  
c
c rrb 2008/09/30; Initilze reservoir to plan, etc      
      do i=1,maxres
        iresP(i)=0
c
c rrb 2009/05/21; Initilze Id, etc used by Bintop             
        cresid(i)='NA          '
        irssta(i)=0
        iressw(i)=0
        nowner(i)=0
        resnam1(i)='NA'        
      end do      
C
c
c _________________________________________________________
c
c		Open input file
  280 write(nlog,103)
      write(6,103)
  103   format(/,
     1  '  GetRes2; Reservoir Station File (*.res) ')
      iin2=iin

      if(infile.eq.1) then
        ifn=11
        rec256=fileName(ifn)
        filena=rec256(1:72)
c       write(nlog,*) ' GetRes2; fileName = ', fileName(ifn)
c       write(nlog,*) ' GetRes2; filena   = ', filena
      else   
        filena=filenc
        READ(IIN,930,end=926,err=928) FILENA
      endif
c
c		Allow no file to be provided
      if(filena(1:2).eq.'-1') then
        write(nlog,*) ' GetRes2; FYI no reservoir data provided'
        numres=0
        goto 520
      endif
c
c _________________________________________________________
c
c		Add path to input file
      
      call putpath(maxfn, filena, fpath1)
      open(3, file=filena,status='old',err=1410)
      iin2=3
c
c rrb 10/7/94 Comment
      write(nlog,940) filena
      call skipn(3)
c
c _________________________________________________________
C
      MAXRET=MAXRES+1
C
      DO 400 NR=1,MAXRET
        read(3,1310,end=430,err=928)
     1  cresid(nr),rec24,cgoto,
     1  iressw(nr),rdate(nr),cresidy(nr)

        if(iout.eq.1) then
          write(nlog,1310)
     1    cresid(nr),rec24,cgoto,
     1    iressw(nr),rdate(nr),cresidy(nr) 
        endif
c
c rrb 2006/03/20; Adjust character string to left     
        resnam1(nr)=adjustl(rec24)
        cresid(nr)=adjustl(cresid(nr))
        cresidy(nr)=adjustl(cresidy(nr))
        cgoto=adjustl(cgoto)        
c
c rrb 2006/12/29; Set reservoir name to a real for old code        
cx        j2=0
cx        do j=1,6
cx          j1=j2+1
cx          j2=j1+3
cx          rec4 = rec24(j1:j2)
cxc         read(rec4,*) divnam(j,nd)
cx          read(rec4,'(a4)') resnam(j,nr)
cx        end do  
        
c
c		Exit if blank
        if(cresid(nr).eq.blank) goto 430
c
c rrb 04/25/97; Daily model 0 implies daily = monthly
        idayr(nr)=0
        if(cresidy(nr).ne.blank) idayr(nr)=2
        if(cresidy(nr).eq.cresid(nr)) idayr(nr)=1
        if(cresidy(nr).eq.'-1          ') idayr(nr)=-1
        if(cresidy(nr).eq.'0           ') idayr(nr)=0
        if(cresidy(nr).eq.'3           ') then
          idayr(nr)=3
          cresidy(nr)=cresid(nr)
        endif
c
c rrb 01/08/08; Add type 4 (pattern is via connecting mid points)
        if(cresidy(nr).eq.'4           ') then
          idayr(nr)=4
          cresidy(nr)=cresid(nr)
        endif
c
c rrb 01/08/08; Add type 5 (pattern is via connecting end points)
        if(cresidy(nr).eq.'5           ') then
          idayr(nr)=5
          cresidy(nr)=cresid(nr)
        endif
c
c rrb 01/04/02; Reservoir data allow daily to prevail since 
c               monthly data is an EOM value, not a volume
c               Therefore stop if sample gage is provided.
        if(iday.eq.1) then
          if(idayr(nr).eq.1 .or. idayr(nr).eq.2) then
            write(nlog,1402) cresid(nr), cresidy(nr)
            goto 9999
          endif
        endif
c
c rrb 12/12/94; Code Enhansement;     
c               Set default administration date to Nov 1
        if(rdate(nr).eq.0) rdate(nr)=11
 
        ch4 = xmonam(1)
        if(ch4(1:1) .ne. ' ') then
          ch3 = ch4(1:3)
        else
          ch3 = ch4(2:4)       
        endif        
        if(ch3.eq.'JAN') ix = 0
        if(ch3.eq.'OCT') ix = 9
        if(ch3.eq.'NOV') ix = 10

        if(rdate(nr).gt.0) then
          rdate(nr)=  rdate(nr) - ix
          if(rdate(nr).lt.0) rdate(nr) = rdate(nr) +12
        endif
c
c _________________________________________________________

        read(3,1050,end=430,err=928)  
     1    volmin(nr),volmax(nr),
     2    flomax(nr),deadst(nr),nowner(nr+1),
     3    nevapo(nr+1),nprecp(nr+1),nrange(nr)
c
c		Echo input        
        if(iout.eq.1) then
          write(nlog,1050)
     1    volmin(nr),volmax(nr),
     2    flomax(nr),deadst(nr),nowner(nr+1),
     3    nevapo(nr+1),nprecp(nr+1),nrange(nr)
        endif
C
C------  FIND RESERVOIR STATION INDEX IN XSTAID ARRAY
C
        DO 290 IS=1,NUMSTA
          if(cstaid(is).eq.cgoto) goto 300
  290   CONTINUE
C
C------  IF STATION IS NOT FOUND, WRITE ERROR MESSAGE AND STOP
C
        Write(6,1060) cgoto
        write(nlog,1060) cresid(nr), cgoto
        Goto 9999
C       
  300   IRSSTA(NR)=IS
        if(iout.eq.1) write(nlog,*) '  GetRes2; nr, is ', nr, is
c
c rrb 2005/07/25; Store structure type at this river node
        istrtype(is)=7500+nr     
C
        NUMOWN=NUMOWN+NOWNER(NR+1)
C
cr      IF(NUMOWN.LE.MAXOWN) Goto 310
        IF(NUMOWN.gt.MAXOWN) then
          write(nlog,1070) NUMOWN,MAXOWN
          write(6,1070) numown,maxown
          Goto 9999
        endif
c
c _________________________________________________________
          
C
C------  READ BEGINNING OWNERSHIPS AND COMPUTE CURRENT STORAGE
C
c rrb 05/02/01; Correction problems when dead storage > 0
cr310   CURSTO(NR)=DEADST(NR)
C
        JI=NUMOWN-NOWNER(NR+1)+1
        JE=NUMOWN
C
        sum=0.0        
        ctot = 0.0
        iwarnx = 0
        cursto(nr)=0.0
        if(iout.eq.1) write(nlog,*) 
     1    ' GetRes2; Reading ownership data', ji, je
        DO 320 J=JI,JE
c
c rrb 01/10/95; 
          read(3,1080,end=926,err=928)
     1    ownnam(j), ownmax(j),curown(j),pcteva(j),n2own(j)
          if(iout.eq.1) then
            write(nlog,1080)
     1      ownnam(j), ownmax(j),curown(j),pcteva(j),n2own(j)
          endif
c 
C             
          CURSTO(NR)=CURSTO(NR)+CUROWN(J)
          ctot = ctot+ownmax(j)                       
          if(ifix(pcteva(j)).ge.0)  sum=sum+amax1(0.,pcteva(j))
          if(abs(pcteva(j)).lt.small) iwarnx=1
c
c rrb 04/15/96 Check initial contents of an account 
c              do not exceed capacity
          if(ownmax(j)-curown(j).lt.-0.001) then
            write(nlog,1100) cresid(nr), resnam1(nr),
     1        ownmax(j), curown(j)
            goto 9999
          endif
        
  320   CONTINUE
c  
c
c rrb 11/29/94 Check ownership does not exceed the storage
      if(abs(volmax(nr)-ctot).gt. 0.01) then 
        rec32='Reservoir Accounts'
        if(iprintr.eq.0) write(nlog,1281) rec32
        if(iprintr.eq.0) write(nchk,1092)
        iprintr=iprintr+1
        write(nchk,1094) iprintr, cresid(nr), resnam1(nr),
     1    volmax(nr),  ctot, ctot-volmax(nr)
      endif
c
c rrb 04/15/96 Check initial contents do not exceed capacity
      if(volmax(nr)-cursto(nr).lt.-0.001) then
cr        rec32='Reservoir Area Capacity'
cr        if(iprintr.eq.0) write(nlog,1281) rec32
      
        write(nchk,1100) cresid(nr), resnam1(nr),
     1                volmax(nr), cursto(nr)
        goto 9999
      endif
c
c rrb 04/07/96 Check precip assignment is ok               
      if(sum.gt.0.001 .and. sum.lt.100.0) then
        write(6,1110) cresid(nr), sum
        write(nlog,1110) cresid(nr), sum
        goto 9999
      endif
c
c rrb 2006/05/30; Additional check
      if(sum.gt.(100.0+small)) then
        write(6,1110) cresid(nr), sum
        write(nlog,1110) cresid(nr), sum
        goto 9999
      endif          
      
c
c rrb 01/08/21; Check than there is not a mix of % data and 
c               porportinal (pcteva()=0)
      if(sum.gt.small .and. iwarnx.eq.1) then
        write(6,1110) cresid(nr), sum
        write(nlog,1110) cresid(nr), sum
        goto 9999
      endif

C
c		Remove dead storage from the reservoir and
c               last account
c
cr    IF(IRESSW(NR).NE.2) Goto 330
      IF(IRESSW(NR).eq.2) then
        CURSTO(NR)=CURSTO(NR)-DEADST(NR)
        OWNMAX(JE)=OWNMAX(JE)-DEADST(NR)
        CUROWN(JE)=CUROWN(JE)-DEADST(NR)
      endif  
C
  330 NUMEPT=NUMEPT+NEVAPO(NR+1)
C
      IF(NUMEPT.LE.MAXEPT) Goto 340
C
      write(nlog,1130) NUMEPT,MAXEPT
c
      write(6,1130) numept,maxept
      Goto 9999
C
  340 JI=NUMEPT-NEVAPO(NR+1)+1
      JE=NUMEPT         
      if(nevapo(nr+1).eq.0) then               
        write(nlog,1140) cresid(nr)
      endif
C
c
c _________________________________________________________
c
c		Read evap or net evap data
      read(3,'(a256)') recin
      if(iout.eq.1) write(nlog,*) ' GetRes2; Evap Data Read = ', recin
      backspace 3
      
      if(iout.eq.1) write(nlog,*) 
     1  '  GetRes2; Evaporation read; ', ji,je

      DO 350 J=JI,JE
        read(3,1150,end=926,err=928) cevar(j),weigev(j)
        if(iout.eq.1) write(nlog,1150) cevar(j),weigev(j)
  350 CONTINUE
C
      NUMRAI=NUMRAI+NPRECP(NR+1)
C
cx     IF(NUMRAI.LE.MAXRAI) Goto 360
cx
cx     write(nlog,1160) NUMRAI,MAXRAI
cx
cx     write(6,1160) numrai,maxrai
cx     Goto 9999
cx     
      IF(NUMRAI.gt.MAXRAI) then
        write(nlog,1160) NUMRAI,MAXRAI
        write(6,1160) numrai,maxrai
        Goto 9999
      endif  
      
C
  360 JI=NUMRAI-NPRECP(NR+1)+1
      JE=NUMRAI
c
c _________________________________________________________
C
c		Read Precipitation data
      if(iout.eq.1) write(nlog,*) 
     1  '  GetRes2; Precipitation read; ', ji,je
      DO 370 J=JI,JE
        read(3,1150,end=926,err=928) cprer(j),weigpr(j)
        if(iout.eq.1) write(nlog,1150) cevar(j),weigev(j)        
  370 CONTINUE
c
c _________________________________________________________
c
c		READ RESERVOIR AREA/CAPACITY CURVE
      NRA=NRANGE(NR)
      
      do 380 irg=1,nra
        READ(3,1170,end=926,err=928)
     1   CONTEN(IRG,NR),SUAREA(IRG,NR),sepage(irg,nr)
        sepcon(irg,nr) = conten(irg,nr)
c       write(nlog,*) '  GetRes2;', nr, conten(irg,nr), suarea(irg,nr)
  380 continue
c
c rrb 11/21/94 Data check
c     write(nlog,*) ' Checking reservoir ', nr
      do 390 irg=2,nra
        ir0=irg-1
c
c rrb 01/08/08; Check for errors in reservoir input data
        if((conten(irg,nr) - conten(ir0,nr)) .lt. smalln) then
           write(nlog,1190) cresid(nr),
     1            ir0, conten(ir0,nr), irg, conten(irg,nr)
           Goto 9999
        endif

        if((suarea(irg,nr) - suarea(ir0,nr)) .lt. smalln) then
           write(nlog,1190) cresid(nr), 
     1           ir0, suarea(ir0,nr), irg, suarea(irg,nr)
           Goto 9999
        endif

c
c rrb 01/08/07; Correct small errors on the fly 
        if((conten(irg,nr) - conten(ir0,nr)) .lt. small) then
          rec32='Reservoir Area Capacity'
          if(iprintr.eq.0) write(nlog,1281) rec32
        
          write(nchk,1191) cresid(nr), irg, small2,
     1                     ir0, conten(ir0,nr),  conten(ir0,nr),
     1                     irg, conten(irg,nr),  conten(ir0,nr)+ small2
          conten(irg,nr) = conten(ir0,nr) + small2
          sepcon(irg,nr) = conten(irg,nr)
        endif
c
c
c _________________________________________________________

  390 continue
C
c
c _________________________________________________________

  400 CONTINUE
C
c rrb 10/27/94 Additional Output
      write(nlog,1180) MAXRES
      write(6,1180) maxres                       
      Goto 9999
C
C------  SET NUMRES TO NUMBER OF RESERVOIRS READ.
C
c 430 numres=NR-1
  430 numres=amax0(NR-1,0)
      write(nlog,432) numres
  432 format('  GetRes2; Number of Reservoirs = ', i5)    
c
c _________________________________________________________
c

c              Check to insure only one reservoir per node
      do 440 i=1,numres
        do 440 j=1,numres          
          if(i.eq.j) goto 440
          if(irssta(i).eq.irssta(j)) then
            write(nlog,1470) cresid(i), cresid(j)
            goto 9999
          endif
  440 continue
c
c _________________________________________________________
c
c		Close input file
      close (3)
c
c _________________________________________________________
C
C		CHECK IF THERE IS ANY ACTIVE RESERVOIR
C
      NRSACT=0
C
      IF(NUMRES.EQ.0) Goto 520
C
c
c _________________________________________________________
c
c		Initilize selected variables
      DO 450 NR=1,NUMRES
      IF(IRESSW(NR).NE.0) NRSACT=NRSACT+1
  450 CONTINUE
C
      DO 460 IS=1,NUMSTA
      IRSORD(1,IS)=0
  460 IRSORD(2,IS)=0
C
      NRSDUM=0
      DO 470 NR=1,NUMRES
        IS=IRSSTA(NR)
        IRSORD(1,IS)=NR
        IF(IRESSW(NR).EQ.0) Goto 470
        NRSDUM=NRSDUM+1
        IRSORD(2,IS)=NRSDUM
  470 CONTINUE
C
      NOWNER(1)=1
      DO NR=1,NUMRES
        NOWNER(NR+1)=NOWNER(NR+1)+NOWNER(NR)     
cr      if(iout.eq.1) write(nlog,*) '  GerRes; ', nr+1, nowner(nr+1)
      end do
c
c		
      NEVAPO(1)=1
      DO NR=1,NUMRES
        NEVAPO(NR+1)=NEVAPO(NR+1)+NEVAPO(NR)
c       write(nlog,*) '  GetRes2;', nr, nevapo(nr), nevapo(nr+1)
      end do
C
      NPRECP(1)=1
      DO NR=1,NUMRES
        NPRECP(NR+1)=NPRECP(NR+1)+NPRECP(NR)
      end do
C
      DO NO=1,NUMOWN
        QMAINS(2,NO)=0.
      end do
      
      
c      
c _________________________________________________________
c
c		Step X; Get Reservoir return flow data (*.rrf)
c		        NOte: Originally developed for Recharge Plans
c			but works generically to route seepage
c                       to the stream as a return flow 
c
c 		Open file
      inR=0
      numrtnRP=0  
              
      rec256=fileName(65)
      filenaR=rec256(1:56)
      if(filenaR(1:2).ne.'-1') then
        write(nlog,104)
        write(6,104)
  104   format(/,72('_'),/
     1  '  GetRes2; Reservoir Station Return File (*.rrf) ')
      
        inR=55
        write(nlog,940) 'Reservoir Return Flow (*.rrf) ', filenaR
c       write(nlog,*) '  GetRes2; inR', inR
        call putpath(maxfn, filenaR, fpath1)
        open(inR, file=filenaR,status='old',err=1412)
        call skipn(inR)                
      endif      
c
c _________________________________________________________
c
c		Step X; IF a file is provided (inR>0) get 
c                       Return Flow data

      if(inR.gt.0) then
   
        
        if(iout.eq.2)
     1    write(nlog,*) '  GetRes2; Calling getRtnX for Res. Returns'
c
c _________________________________________________________
c
c		Step X; Get return data for every reservoir
c			Note ityp=0 is for return flows
c			istrTyp=2 for a reservoir
        ityp=0
        istrTyp=2
        call getrtnX(
     1    nlog,      nchk,     iprintx,  istrTyp,  
     1    maxrtnA,   maxrtnRP, maxsta,   maxres,   nstrtn,
     1    in1,inR,   nw,ji,je, iin2,     numsta,   
     1    ityp,      iloss,    interv,
     1    pcttotRP,  pctlosRP, irtndlRP, irnordRP, irnstaRP,
     1    numrtnRP,  nrtnRP,   numRES,   istrtn,
     1    ndnnod,    idncod,   cgoto,    ResNam1,
     1    cresid,    cstaid,   cstadn,   filenaR)
       endif
c
c rrb 2007/08/27; Move to GetRtnX
c
c _________________________________________________________
c
c               Step X; Redefine # of returns (nrtnRP) and 
c			Set Order and check array size limits
cx      nrtnRP(1)=1
cx      do nr=1,numRes
cx        nrtnRP(nr+1) =nrtnRP(nr+1) +nrtnRP(nr)
cx      end do
cxC
cx      DO IS=1,NUMSTA
cx        ITEMP(IS)=0
cx      end do
cxC
cx
cx      DO Nx=1,numrtnRP
cx        IS=irnstaRP(Nx)
cx        ITEMP(IS)=1
cx      end do
cxC
cxc rrb 98/12/11; Set order
cxc               Not return type specific, build on diversions and wells
cxc     NSTRTN=0
cx      DO IS=1,NUMSTA
cx        IF(ITEMP(IS).ne.0) then
cx          NSTRTN=NSTRTN+1
cx          istrtn(nstrtn)=is
cx          irnord(is)=nstrtn
cx          iprintr2=iprintr2+1
cxc          write(nlog,*) '  GetRes2; nr, is, nstrtn, irnord(is), cstaid' 
cxc          write(nlog,*) '  GetRes2;',iprintr2, is, nstrtn, irnord(is),
cxc     1      cstaid(is)
cx        endif
cx      end do
cxc
cxc rrb01/10/08; Check dimension all return flow data, where all
cxc		includes diversions, wells and reservoirs
cx      if(nstrtn.gt.maxrtnA) then
cx        write(nlog,1021) nstrtn, maxrtnA
cx        goto 9999
cx      endif
cxc
cxc _________________________________________________________
cxc
cxc		Step X; Detailed Check for Reservoir Return File
cx      if(iout.eq.2) then
cx        write(nlog,1430)
cx        j2=0
cx        do nr=1,numres
cx          j1=nrtnRP(nr+1)
cx          do j=1,j1
cx            j2=j2+1
cx            write(nlog,1432) nr, j2, nrtnRP(nR+1), pcttotRP(j2)
cx          end do
cx        end do    
cx      endif  

      
c
c _________________________________________________________
c
c		Step X; Close Reservoir Return File
      close(inR)     
c
c _________________________________________________________
c
c		Step X; Return 
 520  return
c
c _________________________________________________________
      
  926 write(nlog,927) iin2, filena
  927 format(/,72('_'),/
     1  ' GetRes2; Problem. End of file # ', i4, ' encountered',/,
     1  '         File name: ', a256)
      goto 9999
c
  928 write(nlog,929) iin2, filena
  929 format(/,72('_'),/
     1' GetRes2; Problem reading file # ', i4,/,
     1       '   File name: ', a256,/
     1       '   Problem record (next line):')

      backspace(iin2)
      read(iin2, '(a256)',end=926,err=926) recin
      write(nlog,'(a256)') recin
      goto 9999
      
 1410 write(nlog,1420) filena
 1420 format(/,72('_'),/
     1 '  GetRes2; Problem opening file: ', a256)
      goto 9999
      
 1412 write(nlog,1420) filenaR
      goto 9999
c
c _________________________________________________________
c		Formats

  930  FORMAT(A256)
  940  format(4x, a256)
     
 1021  FORMAT(/,72('_'),/
     1  ' GetRes2; Problem.',
     1  ' Number of return stations = ',i5, 
     1  9x, 'Exceeds the dimension = ', i5,/
     1  ' Reconmend you revise the common block size')
  
 1050  format(24x,4f8.0,4i8)
 1060  FORMAT(/,72('_'),/
     1  ' GetRes2; Problem.',/
     1  '   Reservoir ',a12,' not found in the network (*.rin) file',
     1  '   at location ', a12)
     
 1070  FORMAT(/,72('_'),/
     1  ' GetRes2; Problem',/
     1  '   TOO MANY RESERVOIR OWNERSHIPS ',I5,' MAXIMUM = ',I5)
 
 1080  FORMAT(12x,a12,3f8.0,i8)

 1100  FORMAT(/,72('_'),/
     1  '  GetRes2;  Problem',/
     1  '    Reservoir ID             ',a12,/
     1  '    Name                             ', a24,/
     1  '    Has a total or account volume of ', f12.0,/,
     1  '    but initial content of           ', f12.0)
     
 1110  FORMAT(/,
     1   '  GetRes2; Warning Reservor ID ',a12,/
     1   '          Has a precip type total = ', f12.2,/,
     1   '          Note: Total of non negatives should be 0 for',/
     1   '          proration or 100% for percentage approach.',/
     1   '          Also cannot mix approaches (prorate and %)',/
     1   '          at one reservoir')
     
 1130  FORMAT(/,72('_'),/
     1  '  GetRes2; Problem',/
     1  '    Too MANY EVAPO. COMBINATIONS ',I5,' MAXIMUM = ',I5)
     
 1140  format(/,
     1 '  GetRes2; Warning Reservoir ID ', a12, ' has no evap data')
     
 1150  FORMAT(24x, a12,F8.0)
 
 1160  FORMAT(/,72('_'),/
     1  ' GetRes2; Problem',/
     1  '   Too MANY RAINFALL COMBINATIONS ',I5,' MAXIMUM = ',I5)
     
 1170  FORMAT(24X,8F8.0)
 
 1180  FORMAT(/,72('_'),/
     1  ' GetRes2; Problem '/
     1  '   Too MANY RESERVOIRS, MAXIMUM = ',I5)
     
 1190  format(/,72('_'),/
     1   ' GetRes2: Problem',/
     1   '         Area capacity curve for reservoir ', a12, /
     1   '         is not increasing',//
     1   '            Entry       Value',/
     1   '         ________ ___________', /,
     1   9x, i8, f12.4,/, 9x, i8, f12.4,//
     1   '         Problem could be number of accounts specified',/
     1   '         is not consistent with the ownership data')
     
 1191  format(/,
     1   '  GetRes2; Warning',/
     1   '    Capacity in area-capacity curve for reservoir',/
     1   '   ',a12,' entry ', i5, ' is not increasing.',/
     1   '    Value adjusted by ',f8.4,//
     1   '            Entry       Value   Adj Value',/
     1   '         ________ ___________ ___________', /,
     1   9x, i8, f12.4, f12.4,/, 9x, i8, f12.4, f12.4)
     
 1092  FORMAT(/,
     1  '  GetRes2; Warning',/
     1  '    Reservoir with volume < total accounts',/
     1  '    (May be ok if tied to refill accounts)'// 
     1  '      # ID           Name                          Volume',  
     1  '      Owners       Delta',/
     1  '  _____ ____________ ________________________ ___________', 
     1  ' ___________ ___________')
     
 1094  format(2x, i5, 1x, a12, 1x, a24, 3f12.0)
      
 1281  FORMAT(/,
     1  '  GetRes2; Warning See *.chk for details regarding: ',a24)

 1310  FORMAT(a12,a24,a12,i8,f8.0,1x,a12)
 
 1402  format(/,72('_'),/
     1 '  GetRes2; Problem res. ID ', a12, ' has a daily ID = ', a12,/
     1 '          which says monthly data controls.  This approach ',/
     1 '          is not supported since it is EOM data.',/
     1 '          To do: set daily ID to type 0 (average) or ', 
     1           'type 3 (daily controls)')
 1430 format(/,
     1 '  GetRes2; Reservoir Return Flow Details',/
     1 '      nr      J2   Nrtnx pcttotw',/
     1 ' _______ _______ _______ _______')
     
 1432 format(3i8, f8.0)               
     
     
 1470  format(/,72('_'),/
     1   ' GetRes2; Problem',
     1   ' two reservoirs at the same river node',/
     1   '         Reservor IDs = ', a12, 1x, a12)
c
c               Error Tracking
c _________________________________________________________

 9999 write(6,9440) 
      write(nlog,9450) 
      call flush(6)
 9440 format(/,72('_'),/
     1  ' GetRes2; Stopped in GetRes2, see the log file (*.log)')
 9450 format('  Stopped in GetRes2')
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop 
      END
