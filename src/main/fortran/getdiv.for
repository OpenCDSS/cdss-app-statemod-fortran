c
       Subroutine GetDiv(IIN, inx, numstax)
c
c
c _________________________________________________________
c	Program Description
c
c       GetDiv; it reads in diversion station data
c
c _________________________________________________________
c
c       Documentation
c
c               iin  =   response file #
c
c _________________________________________________________
c
c       Update History
c
c rrb 2005/10/24; Separated from Datinp
c
c _________________________________________________________
c	Dimensions
      include 'common.inc'
c
      DIMENSION ITEMP(numstax), ctemp(numstax)

      dimension x(12)
C
      character rec3*3, blank*12, crtnid*12,
     1          recin*256, cgoto2*12, cx*12, rec4*4, rec24*24,
     1          rec1*1, rec12*12,ctemp*24, rec32*32
c     
c _________________________________________________________
c
c		Step 1; Initilize
c		0=no details
c		1= print details
c		2=print return flow details
c		3=printe diversion summary
c		99=print new input format (*.str, *.eff, *.rtn)
      iout=0
      ioutRF=0
c
c rrb 2006/04/12; Initilize at top of Datinp so that Reservoir 
c		  return data can be read befor GetDiv 
c                 and this value is not reset to zero
cr    NUMRTN=0
      NDIVIN=0
      NUMUSE=0
      numOwnD=0
      ITMPT1=0
      ITMPT2=0    
      iwarnx=0
      iloss=0
      iprintx=0
      iprintd=0 
      small=0.001
      blank = '            '                       
      irturn3=0
      
      if(iout.gt.0) write(nlog,*) ' GetDiv; iout = ', iout
      
      iin2=4
c _________________________________________________________
c
c		Step 2; Open new files 
      if(iout.eq.99) then
        call namext(maxfn, filenc, 'dst', filena) 
        open(56,FILE=filena,STATUS='Unknown')
        call outtop(56,1,48)
        
        call namext(maxfn, filenc, 'def', filena) 
        open(57,FILE=filena,STATUS='Unknown')
        call outtop(57,1,48)
        
        call namext(maxfn, filenc, 'drf', filena) 
        open(58,FILE=filena,STATUS='Unknown')
        call outtop(58,1,48)
      endif  

c      
c _________________________________________________________
c
c
c     DO 650 ND=1,MAXDIW
      do 650 nd=1,maxdiv-1
c
c rrb 2009/05/21; Initilize
        idivco2(nd)=0      
        read(4,1200,end=660,err=928)
     1    cdivid(nd),rec24,cgoto,
     1    idivsw(nd),divcap(nd), ix, ireptyp(nd),
     1    cdividy(nd)
c
c rrb 2006/03/20; Adjust character string to left     
        cdivid(nd)=adjustl(cdivid(nd))
        cdividy(nd)=adjustl(cdividy(nd))     
     
        if(iout.eq.1) write(nlog,1200)
     1    cdivid(nd),rec24,cgoto,
     1    idivsw(nd),divcap(nd), ix, ireptyp(nd),
     1    cdividy(nd)

        ctemp(nd)=cdividy(nd)
        
c
c ---------------------------------------------------------
c		Store name as a real divnam for historical consistency
c
        divnam1(nd)=rec24
        
cx        j2=0
cx        do j=1,6
cx          j1=j2+1
cx          j2=j1+3
cx          rec4 = rec24(j1:j2)
cx          read(rec4,'(a4)') divnam(j,nd)
cx        end do  
c      
c _________________________________________________________
c
c
c rrb; 04/25/97; Daily model -1 implies / day per month,
c                0=same as average monthly
c               -1=divide monthly by 31
c                1=use a daily pattern of this ID, Monthly controls
c                2=use a daily pattern of another, ID Monthly controls
c                3=usd daily data for this ID, Daily controls
        idayd(nd)=0
        if(cdividy(nd).ne.blank) idayd(nd)=2
        if(cdividy(nd).eq.cdivid(nd)) idayd(nd)=1
        if(cdividy(nd).eq.'-1          ') idayd(nd)=-1
        if(cdividy(nd).eq.'0           ') idayd(nd)=0
        if(cdividy(nd).eq.'3           ') then
          idayd(nd)=3
          cdividy(nd)=cdivid(nd)
        endif
c
c rrb 01/08/08; Add type 4 (pattern is via connecting mid points)
        if(cdividy(nd).eq.'4           ') then
          idayd(nd)=4
          cdividy(nd)=cdivid(nd)
        endif
c
c _________________________________________________________
c rrb 02/17/95; Code Addition; Allow multiple owners.
c		Note multi users are still not being supported
c               assume 1 user, revised read with variable ix
c       if(ix.gt.1) then
c         write(nlog,758) cdivid(nd)
c         goto 9999
c       endif
c
        nduser(nd+1) = 1        
        ndOwn(nd+1) = ix
        numOwnd=numOwnd+ndOwn(nd+1)
        
        noi=numOwnd-ndOwn(nd+1)+1
        noe=numOwnd
        no=noi
        
        if(ix.gt.1) then
          write(nlog,*) '  GetDiv; FYI multiple owners for',
     1    ' Diversion ', cstaid(nd), ' ', rec24   
     
          sum=0.0  
          do no=noi,noe
            read(4,*,end=660,err=928)  divnamO(no), divOwnP(no)
            write(nlog,*) '  GetDiv; ',divnamo(no), divownp(no)
            sum=sum+divownp(no)
            divOwnP(no)=divOwnP(no)/100.0
          end do  
          
          if(sum.ne.100) then
            write(nlog,1500) cdivid(nd), 
     1       (no-noi+1, divnamo(no), divownp(no)*100., no=noi,noe)
            goto 9999
          endif
        else
          divnamO(no) = rec24
          divOwnP(no) = 100.0
        endif
c
c
c _________________________________________________________
c              Allow blank data at end of file
        if(cdivid(nd).eq.blank) goto 660
c rrb 98/09/15; This lead to a problem when bad data was provided
c               since multi user capability is not recommended, remove
        if(cgoto.eq.blank) then
          write(nlog,1210)  cdivid(nd), cgoto
          write(6,1210)     cdivid(nd), cgoto
cx        Goto 9999
        endif
c
c _________________________________________________________
c		FIND DIVERSION STATION ID
C
        DO 530 IS=1,NUMSTA
          if(cstaid(is).eq.cgoto) goto 540
  530   CONTINUE
C
C------  IF STATION IS NOT FOUND, WRITE ERROR MESSAGE AND STOP PROGRAM
C
        write(nlog,1210) cdivid(nd), cgoto
        write(6,1210)    cdivid(nd), cgoto
        Goto 9999
C
  540   IDVSTA(ND)=IS
c
c rrb 2005/07/25; Store structure type at this river node
        istrtype(is)=nd   
c
c
c _________________________________________________________
c               Print Diversion information
c       write(nlog,*) ' GetDiv; iout=', iout
        if(iout.eq.3) then
          write(99,541) nd, cdivid(nd), is
  541     format('  GetDiv; nd, cdivid(nd), is', i5, 1x, a12, i5)
        endif
c
c _________________________________________________________
c
c		Set user data
        IF(NDUSER(ND+1).EQ.0) Goto 650
        NUMUSE=NUMUSE+NDUSER(ND+1)
        IF(NUMUSE.LE.MAXUSE) Goto 560
        write(nlog,1220) NUMUSE,MAXUSE
        write(6,1220) numuse,maxuse
        Goto 9999
  560   NUI=NUMUSE-NDUSER(ND+1)+1
        NUE=NUMUSE
c
c _________________________________________________________
c		READ IN USERS' DATA
c rrb 04/09/07; Still limited to 1 user per structure
c		even though multiple owners are now OK
C
C
        DO 640 NU=NUI,NUE
          Read(4,1230,end=926,err=928)
     1      USENAM1(NU),IDVCOM(NU),NRTN(NU+1),
     1      divefc(nu),AREA(NU),IRTURN(NU), ix
c
c rrb 2007/10/05; Insure area > 0
          area(nu)=amax1(area(nu),0.0)    
c               
          if(iout.eq.1 .or. ioutRF.eq.1) write(nlog,1232)
     1      nu, USENAM1(NU),IDVCOM(NU),NRTN(NU+1),
     1      divefc(nu),AREA(NU),IRTURN(NU), ix
     
     
          demsrc(nu)=float(ix)
          if(irturn(nu).eq.3) irturn3=1
c
c _________________________________________________________
c 		Read 12 monthly efficiency values
          if(divefc(nu) .lt. -0.001) then
            read(4,*,end=926,err=928) (diveff(i,nu), i=1,12)
            if(iout.eq.1) write(nlog,*) (diveff(i,nu), i=1,12)
          else
            do 570 i=1,12
              diveff(i,nu) = divefc(nu)
  570       continue
          endif
c
c
c _________________________________________________________
c		Insure efficiency is set properly if Tmtn (irturn=4)
          if(irturn(nu).eq.4) then 
            do i=1,12
              c = diveff(i,nu)-100.0
              if(c .gt. small) then
                write(nlog,547) cdivid(nd)
c               diveff(i,nu) = 100.0
                goto 9999
              endif
            end do
          endif
c
c _________________________________________________________
c
c		Set demand code
c
c------  IDVCOM(K) = 1 : Monthly (12*years) from *.ddm
c                  = 2 : Annual (12 values) from *.dda
c                  = 3 : Monthly (12*years) IWR provided
c                  = 4 : Annual (12 values) IWR provided
c                  = 5 : demand is set to 0 by program
c                  = 6 : demand for a D&W structure is in *.ddm
c
c rrb 6/12/95; Baseflow warnings or resets
          if(ioptio.eq.1 .or. ioptio.eq.9) then 
c
c               Reset annual data to zero
            if(idvcom(nu).eq.2.or.idvcom(nu).eq.4)then
              idvcom(nu) = 5
              write(nlog,1240)
            endif
c 
c rrb 00/05/30; Problem if baseflow mode & code says monthly IWR data 
c               is provided
            if(idvcom(nu).eq.3) then
              write(nlog,1241) cdivid(nd)
              idvcom(nu) = 1
c             goto 9999
            endif
c          
c               Reset control variable to insure no addition, etc.
            if(idemtyp.ne.1) then
              idemtyp=1
              write(nlog,1238)
            endif

          endif
c
c
c _________________________________________________________
c
c		STORE DIVERSION CODE FOR WHICH DEMAND IS TO BE INPUT
c
c rrb 00/05/30; Allow IWR data
c         IF(IDVCOM(NU).GT.2) Goto 580
          IF(IDVCOM(NU).eq.5) Goto 580
C   
          NDIVIN=NDIVIN+1
C
          ITMPT1=1
          if(iout.eq.1) then
            write(nlog,*) '  GetDiv; nu, idvcom(nu), nrtn(nu+1),interv'
            write(nlog,*) '  GetDiv;', nu,idvcom(nu),nrtn(nu+1),interv
            write(nlog,*) '  GetDiv; numrtn', numrtn
          endif  
c
c
c _________________________________________________________
c
          IF(idvcom(nu).eq.1 .or. idvcom(nu).eq.3) itmpt2=2
cr        Goto 580
C
  580     IF(NRTN(NU+1).EQ.0) Goto 640
C
          NUMRTN=NUMRTN+NRTN(NU+1)
          IF(NUMRTN.LE.MAXRTN) Goto 590
C
          write(nlog,1250) MAXRTN
          write(6,1250) maxrtn
          Goto 9999
C
c
c _________________________________________________________
c
c		Read return flow information
  590     JI=NUMRTN-NRTN(NU+1)+1
          JE=NUMRTN
          
          DO 620 J=JI,JE
c
c rrb 99/08/26; Character ID
            if(interv.ne.-999) then
              Read(4,1260,end=926,err=928)  crtnid,pcttot(j),irtndl(j)
              if(iout.eq.1) write(nlog,1260)crtnid,pcttot(j),irtndl(j)
            else
              Read(4,1262,end=926,err=928)  crtnid,pcttot(j),cirtndl(j)
              if(iout.eq.1) write(nlog,1262)crtnid,pcttot(j),cirtndl(j)
            endif
c           write(nlog,*) 
c     1       '  GetDiv;   nd, nrtn,    j, pcttot, irtndl'
c           write(nlog,'(8x, 3i6, 2f8.0)')
c     1       nd, nrtn(nu+1), j, pcttot(j), irtndl(j)
c
c               Find river station pointer
            DO 600 IS=1,NUMSTA -1
              if(cstaid(is).eq.crtnid) goto 610
  600       CONTINUE
C
C------  IF STATION NOT FOUND, WRITE ERROR MESSAGE AND STOP
C
            write(nlog,1270) crtnid, cdivid(nd), divnam1(nd)
            write(6,1270) crtnid, cdivid(nd), divnam1(nd)
            Goto 9999
C
  610       IRNSTA(J)=IS
c
c rrb 11/15/94 Data check
            if(cstadn(is).eq.blank) then
              write(nlog,1270) crtnid, cdivid(nd), divnam1(nd)
              write(6,1270)  crtnid, cdivid(nd), divnam1(nd)
              goto 9999
            endif

c
c _________________________________________________________
c
c 		Check if the diversion returns go to the diversion 
c           if(crtnid.eq.cgoto) write(nlog,1280) crtnid
            iss=is 
            ndns = ndnnod(iss)
            do 618 n=1,ndns           
              if(cstaid(iss).eq.cgoto) then
                rec32='Return Flows                    '
                if(iprintx.eq.0) write(nlog,1281) rec32
                if(iprintx.eq.0) write(nchk,1283)
                iprintx=iprintx+1
                write(nchk,1284) 
     1            iprintx, cdivid(nd), divnam1(nd),
     1            cgoto, crtnid
                iwarnx=1
              endif
  618       iss=idncod(iss)
c _________________________________________________________
c		End loop to read return flow data
  620     CONTINUE
c
c _________________________________________________________
c		End loop for users (currently limited to 1)
  640   CONTINUE
c
c _________________________________________________________
c		End diversion read
  650 CONTINUE
C
c
c _________________________________________________________
c		Warn if dimension is exceeded
      write(nlog,1300) MAXDIV
      write(6,1300) maxdiv
      Goto 9999
C
  660 close (4)
  
c
c rrb 09/31/96; Check regarding return flows upstream
c rrb 00/03/07; Allow simulation to proceed with warning
c     if(iwarnx.eq.1) goto 9999
      iwarnx=iwarnx

      NUMDIV=amax0(ND-1,0)
      numdxw=numdiv
      
      write(nlog,211) numdiv
  211 format(/,
     1  '  GetDiv; Number of Diversions       = ', i5)
  
c
c _________________________________________________________
c		Print Warning about efficiency for a carrier
      if(irturn3.eq.1) then
        rec32='Carrier Efficiency              '
        write(nlog,1281) rec32
      
        write(nchk,330) (i, i=1,12)
        
        n=0
        do Nd=1,NUMdiv
          n1=nduser(nd+1)
          n2=nduser(nd+1)-1        
          
c         do nu=n1,n2            
            nu=nd
            if(irturn(nu).eq.3) then
              n=n+1
              write(nchk,332) n, cdivid(nd), nd, nu, 
     1        irturn(nu), divefc(nu), (diveff(im,nu),im=1,12)                
            endif
c         end do
        end do
      endif    
c
c _________________________________________________________
c               Step X; Assign diversion type (mondiv)
c
c        MONDIV = 0 : NO DIVERSION DATA WILL BE READ IN
c               = 1 : CONSTANT MONTHLY DIV DEMAND TO BE INPUT 1x/sim
c               = 2 : MONTHLY DIV DEMAND TO BE INPUT EVERY YEAR
c               = 3 : CASES 1 & 2 EXIST IN THE SYSTEM
c
      MONDIV=ITMPT1+ITMPT2
C
c _________________________________________________________
c		Reset diversion user and diversion owner arrays
c rrb 04/09/07; Allow multiple structure owners	
      NDUSER(1)=1
      ndown(1)=1
      DO ND=1,NUMDIV
        NDUSER(ND+1)=NDUSER(ND+1)+NDUSER(ND)
        ndown(nd+1)=Ndown(nd+1)+ndown(nd)
      end do
c
c _________________________________________________________
c		Reset return flow array
      NRTN(1)=1
      DO NU=1,NUMUSE
        NRTN(NU+1)=NRTN(NU+1)+NRTN(NU)
      end do
C
cr    IF(NUMRTN.EQ.0) Goto 730
      IF(NUMRTN.gt.0) then
C
c _________________________________________________________
c		Set temp array in Bomsec
        DO IS=1,NUMSTA
          ITEMP(IS)=0
        end do
C
        DO NR=1,NUMRTN
          IS=IRNSTA(NR)
          ITEMP(IS)=1
        end do
C
cr      NSTRTN=0
        DO 710 IS=1,NUMSTA
c
c rrb 2006/04/12; Initilize at top of Datinp so that Reservoir 
c		  return data can be read befor GetDiv 
c                 and this value is not reset to zero                
cr        IRNORD(IS)=0
          IF(ITEMP(IS).EQ.0) Goto 710
          NSTRTN=NSTRTN+1
          ISTRTN(NSTRTN)=IS
          IRNORD(IS)=NSTRTN
          iprintd=iprintd+1
          
          if(iout.eq.2) then
            if(iprintd.eq.1) write(nlog,300)            
            write(nlog,302) nstrtn, is, irnord(is), cstaid(is)
          endif
          
 710    continue
      endif 
c
c _________________________________________________________
c rrb 01/10/08; Dimension check for return data
      if(nstrtn.gt.maxrtnA) then
        write(nlog,1021) maxrtnA
        goto 9999
      endif
c
c _________________________________________________________      
c
c		Step X; Print new format if iout = 99
      if(iout.eq.99) then      
        do nd=1,numdiv
          ns=idvsta(nd)
c
c		Station (*.dst) #1
          write(56,1201)
     1      cdivid(nd),divnam1(nd),cstaid(ns),
     1      idivsw(nd),divcap(nd), 1, ireptyp(nd),
     1      ctemp(nd)
 1201  format(a12,' "', a24,'" ', a12,i8,f8.0,2i8,1x,a12)
c
c		Station (*.dst) #2  
          ix=ifix(demsrc(nd))
          write(56,1231)
     1      USENAM1(Nd),IDVCOM(Nd),NRTN(Nd+1),
     1      divefc(nd),AREA(Nd),IRTURN(Nd),
     1      ix
 1231  FORMAT(12X,' "',a24,'" ',12x, 2I8,F8.2,f8.0,3I8)
c
c _________________________________________________________
c		Diversion Efficiency (*.def)     
c
          if(divefc(nd) .lt. -0.001) then
            write(57,'(a12, 20f8.0)') cdivid(nd),(diveff(i,nd), i=1,12)
          endif  
c
c _________________________________________________________
c
c		Returns (*.drf)        
          j1=nrtn(nd)
          j2=nrtn(nd+1)-1
          j3=0
          rec3='Rtn'
          write(58,'(a1)') '#'
          do j=j1, j2
            j3=j3+1
            write(58, '(a12,1x,a12,f8.4,1x, i8,1x, a3, i2)')
     1        cdivid(nd), cstaid(irnsta(j)), pcttot(j),irtndl(j),
     1        rec3, j3
          end do
        end do
        close(56)
        close(57)
        close(58)
        
      endif
c      
c _________________________________________________________      
c
c		Return
cr    write(nlog,*) ' '
c
c		Case 1; evap is zero

      return
c      
c _________________________________________________________      
c
c		Error Handling
  926 write(nlog,927) iin2, filena
  927 format(' GetDiv; Problem. End of file # ', i4, ' encountered',/,
     1       '   File name: ', a256)
      goto 9999
      
  928 write(nlog,929) iin2, filena
  929 format(' GetDiv; Problem reading file # ', i4,/,
     1       '   File name: ', a256,/
     1       '   Problem record (next line):')

cr      backspace(iin2)
cr      read(iin2, '(a256)',end=926,err=926) recin
cr      write(nlog,'(a256)') recin
cr      goto 9999
     
 1410  write(nlog,1420) filena
 1420  format('  GetDiv; Problem opening file: ', a256)
       goto 9999      
      
c
c _________________________________________________________
c		Formats      
 300  format(
     1  '  GetDiv;      nstrtn      is  irnord crunid',/
     1  ' ____________ _______ _______ _______ ____________')
     
 302  format('  GetDiv;   ', 1x, 3i8, 1x, a12)

 330  format(/, 
     1  '  GetDiv; FYI the folloinw structures are carriers',/
     1  '          and therefore always use average efficiency',//
     1  '    # ID             nd iuse  irturn',
     1  '      DivAve', 9('DivEff_',i1), 3(' DivEff_',i2),/
     1  ' ____ ____________ ____ ____ _______',13(' _________'))
 332  format(i5, 1x, a12, 2i5, i8, 13f10.2)
 
  547  format(/,72('_'),/
     1 '  GetDiv; Problem for Div or Well station = ', a12,
     1          ' type = 4 which indicates transmountain, but',/
     1 '          efficiency is not 100%.',
     1 '          To do; Change type or efficiency data to 100%')
  930  FORMAT(A256)
  
  940  format(/, 72('_'), /,
     1 '  GetDiv; Diversion Station ', a24,/,4x, a256)
     
 1021  FORMAT(/,72('_'),/
     1  ' GetDiv; Problem.',
     1  ' Too many return stations, max = ', i5)

 1200  format(a12,a24,a12,i8,f8.0,2i8,1x,a12)

 1202  format(a12,a24,a12,i8,f8.0,1x,a12,1x,f12.5,1x,a12)      
 1210  FORMAT(/,72('_'),/
     1  '  GetDiv; Problem for diversion id = ', a12,/
     1  '          in the diversion station file (*.dds) ',/
     1  '          the river location (cgoto) = ', a12,/
     1  '          cannot be found in the network file (*.rin)',/
     1  '          (If the id looks ok, Check to be sure it',/
     1  '          does not contain a tab character')
 
 1220  FORMAT(/,72('_'),/
     1  '  GetDiv; Problem. Too MANY DIVERSION USERS      ',I5,
     1  '  MAXIMUM = ',I5)
 
 1230  FORMAT(1x, 12X,a24,12x, 2I8,F8.2,f8.0,3I8)
 1232  FORMAT(' GetDiv; Return Flow ',
     1  i5, 1x, 12X,a24,12x, 2I8,F8.2,f8.0,3I8)
 
 1238  format(/,
     1  '  GetDiv; Warning, for baseflow calculations reset control',/
     1  '          variable icondem (idemtyp) = 1 to insure no ',/
     1  '          adjustments to historic diversion and well',/ 
     1  '          pumping data')
 
 1239  format(/,
     1  '  GetDiv; Warning, in baseflow therefore ID ', a12, 'has had',/
     1  '          its demand type (idvcomw) reset to 1.  Recall',/
     1  '          the baseflow mode reads *.weh in lie of *.wem')
 1240  format(/,
     1  '  GetDiv; Warning, Annual diversions not used in base',
     1  ' flow calcs'/)

 1241  format(/,72('_'),/
     1  '  GetDiv; Problem, In Baseflow mode yet diversion code',
     1           ' in *.dds says IWR data provided (idvcom = 3).', 
     1           ' for ID = ', a12,/
     1  '          Set to 1 to avoid having historic diversion', 
     1           ' data weighted by efficiency.')
     
 1242  format(/,
     1  '  GetDiv; Warning, Wells must have a return & depletion',
     1  ' location',/ '          Check ID ', a12 )
     
 1243  format(/,72('_'),/
     1  '  GetDiv; Problem, In Baseflow mode yet diversion code',/
     1  '          in *.wes says *.ddc data provided (idvcomw = 3).',/ 
     1  '          Stopped to avoid having historic pumping',/, 
     1  '          data weighted by efficiency.  To do: revise *.wes')
     
 1250  FORMAT(/,72('_'),/
     1  ' GetDiv; Problem. Too MANY RETURN FLOW STATIONS, MAX = ',I5)
     
 1255 format(/,72('_'),/
     1 '  GetDiv; Problem with Well Station ', a12,/
     1 10x,'The augmentation plan code (planw) = ', a12,
     1 10x,'But this plan is type ', i4,/
     1 10x,'(Note a well agumentation plan should be a type 2)')
     
 1256 format(/,
     1 '  GetDiv; FYI at least one Well Station (e.g.) ', a12,/
     1 9x,' is tied to an augmentation plan (planw) = ', a12,
     1     ' Plan Pointer = ', i5)
     
 1257 format(/,
     1 '  GetDiv; Warning Well Station ', a12,/
     1 9x,' IS NOT TIED to an augmentation plan (planw) = ', a12,
     1     ' plan pointer = ', i5)
     
 1260  format(36x,a12,f8.0,i8)
 1262  format(36x,a12,f8.0,a12)
 1270  FORMAT(/,72('_'),/
     1  '  GetDiv: Problem.',
     1  ' Return flow to: ',a12,' from Diversion:',/
     1  '          ID: ',a12, ' Named: ', a24,/
     1  '          was not found or is not allowed at the last',/
     1  '          river station.  For the latter case, add one',/
     1  '          additional river node to the *.rin file')
 1280  FORMAT(/,72('_'),/
     1  '  GetDiv; Problem.'
     1  ' Station ',a12,' of diversion station file',
     1  ' has return flows going back to the diverting node',
     1  ' at ', a12)
     
 1281  FORMAT(/,
     1  '  GetDiv; Warning See *.chk for details regarding: ', a32)
     
 1282  FORMAT(/,
     1  '  GetDiv; Warning the following diversion has a return ',/
     1  '          location that is available to the diverter:',/
     1  '          Diversion ID:                  ',a12,/, 
     1  '          Diversion name:                ',a24,/, 
     1  '          Located at river ID:           ',a12,/,  
     1  '          Has a return flow to river ID: ',a12)
 1283  FORMAT(/,72('_'),/
     1  '  GetDiv; Problem the following structure(s) ',
     1    'return water upstream',//
     1  '    # Str ID       Str Name                ', 
     1  ' Located at   Returns to',/ 
     1  ' ____ ____________ ________________________', 
     1  ' ____________ ____________')
 1284  format(i5,1x, a12, 1x, a24, 1x, a12, 1x a12)
 1285  FORMAT(/,
     1 '  GetDiv; Warning the following structure(s) return water',/
     1 10x,'to at least one non-downstream node. Only the first',/ 
     1 10x,'non-downstream occurance is printed per structure.',/
     1 10x,'Note, a non-downstream return is OK but it can cause',/ 
     1 10x,'the system to reoperate and impact performance if',/
     1 10x,'the immediate (current month or days) return flow',/
     1 10x,'exceeds the prescribed tolerance (see Performance Info',/
     1 10x 'in the *.log file'//
     1  '    # Str ID       Str Name                ', 
     1  ' Located at   Returns to',/ 
     1  ' ____ ____________ ________________________', 
     1  ' ____________ ____________')

 1291  format(2x, i5, 1x, a12, 1x, a24, 1x,3f8.0)
 1292  FORMAT(/,72('_'),/
     1 '  GetDiv; Problem station ',a12,
     1 ' of the Direct Diversion Station File (*.dds)',/
     1  10x, ' has the following return data:',/
     1  10x, ' Total of all returns = ', f8.2)
     
 1300  FORMAT(/,72('_'),/
     1 ' GetDiv; Problem.',
     1 ' Too MANY DIVERSION PROJECTS,     MAXIMUM = ',I5)      
     
 1500   format(/,72('_'),/
     1  '  GetDiv; Problem diversion ownership is less than 100%',
     1           ' for ID: ', a12, /
     1  20(i5, ' Owner Name = ', a24,
     1  ' Percent = ', f10.0,/))
c
c _________________________________________________________
c
c               Error Tracking

 9999 write(6,1440) 
      write(nlog,1450) 
      call flush(6)
 1440 format('  Stopped in GetDiv, see the log file (*.log)')
 1450 format(/72('_'),/,'  GetDiv; Stopped in GetDiv')
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)
     
      stop     
      end
