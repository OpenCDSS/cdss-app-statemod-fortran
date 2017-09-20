c
       Subroutine GetDiv2(IIN, inx, numstax)
c
c
c _________________________________________________________
c	Program Description
c
c       GetDiv2; it reads in diversion station data
c
c _________________________________________________________
c
c      Documentation
c
c	iin  		response file #
c	inS		Response file number
c	inE		Efficiency Fiel number
c	inR		Return Flow File number
c	inC		Indicates which file is currently being read
c
c       ireptyp(nd)	0 or -999 = off
c                       1 = on, with 100% replacment
c                      -1 = on, depletion replacement
c	itmpt1		switch used to calculate Mondiv
c	itmpt2		switch used to calculate Mondiv
c
c       MONDIV 	        0 NO DIVERSION DATA WILL BE READ IN
c               	1 CONSTANT MONTHLY DIV DEMAND TO BE INPUT 1x/sim
c               	2 MONTHLY DIV DEMAND TO BE INPUT EVERY YEAR
c               	3 CASES 1 & 2 EXIST IN THE SYSTEM
c       NDIVIN          0 No diversion demand TS to read in Mdainp
c                       >0 Number of diversion TS to read in Mdainp
c	numdiv	 	Number of diversion stations read
C
c
c _________________________________________________________
c
c	Update History
c
c rrb 2005/10/24; Separated from Datinp
c
c _________________________________________________________
c	Dimensions

      include 'common.inc'
c
      DIMENSION ITEMP(numstax)
      dimension x(12)
C
      character ch3*3, ch4*4, blank*12, crtnid*12,
     1          cgoto2*12, cx*12, rec4*4, rec24*24,
     1          rec1*1, rec12*12,
     1          filenaS*256, filenaE*256, filenaR*256, 
     1          cistat*12
     
      character cyrX*5, cunit*5, recin*132, recout*132
c
c__________________________________________________________
c
c               Step 1; Initilize
c		iout =  0 No details
c		ioutR = 0 No details for return flows
c		ioutE = 0 No detials for efficiency
      iout=0
      ioutR=0
      ioutE=0
      iin2=iin
C
c rrb 2006/04/12; Initilize at top of Datinp so that Reservoir 
c		  return data can be read (befor Getdiv2 
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
c     iprintd=0 
      small=0.001
      blank = '            '                       
      do nd=1,maxdiv
        iwarn(nd)=0
      end do

      inS=54     
c
c__________________________________________________________
c
c               Step 2; Open Diversion Station File (*.dst)
cx      rec256=fileName(54)
cx      filenaS=rec256(1:72)
cx      if(filenaS(1:2).ne.'-1') then
cx        inS=54
cx        write(nlog,940) 'New (*.dst)             ', filenaS          
cx        in1=inS
cxc       write(nlog,*) '  GetDiv2; inS', inS      
cx        call putpath(maxfn, filenaS, fpath1)
cx        open(inS, file=filenaS,status='old',err=1410)
cx        call skipn(inS)                
cx      endif  
c
c__________________________________________________________
c
c               Step 3; Open Diversion Efficiency File (*.def)
c
c rrb 2005/10/14; Separate efficiency, return and delay files
      rec256=fileName(55)
      filenaE=rec256(1:72)
      if(filenaE(1:2).ne.'-1') then
        inE=55          
        write(nlog,940) 'Efficiency (*.def)      ', filenaE          
c       write(nlog,*) '  GetDiv2; inE', inE      
        call putpath(maxfn, filenaE, fpath1)
        open(inE, file=filenaE,status='old',err=1410)
        call skipn(inE)                
      endif  
c
c__________________________________________________________
c
c               Step 4; Open Diversion Station Return Flow File (*.drf)
c
c rrb 2005/10/14; Allow separate return file      
      rec256=fileName(56)
      filenaR=rec256(1:56)
      if(filenaR(1:2).ne.'-1') then
        inR=59
        write(nlog,940) 'Return Flow (*.drf)     ', filenaR
c       write(nlog,*) '  GetDiv2; inR', inR
        call putpath(maxfn, filenaR, fpath1)
        open(inR, file=filenaR,status='old',err=1410)
        call skipn(inR)                
      endif      
c__________________________________________________________
c
c               Step 5; Exit if no station file is be provided
c
c rrb 2005/10/14; Separate station files
cx      if(inS.eq.0) then
cx        write(nlog,*) ' GetDiv2; FYI no diversion data provided'
cx        numdiv=0
cx        goto 730
cx      endif
c
c__________________________________________________________
c
c               Step 6; Read diversion station file (*.dds)
c
        inC=inS
      do 650 nd=1,maxdiv-1
 100    read(inS,*,end=210,err=210) cistat
        if(iout.eq.1) write(nlog,*) '  GetDiv2; Station for ', cistat
        if(cistat(1:1) .eq. '#') goto 100
        backspace(inS)
      
cx      read(inS,*,end=210,err=928)      
cx     1    cdivid(nd),rec24,cgoto,
cx     1    idivsw(nd),divcap(nd), ix, ireptyp(nd),
cx     1    cdividy(nd),
cx     1    rec24,IDVCOM(nd),NRTN(nd+1),
cx     1    divefc(nd),AREA(nd),IRTURN(nd),
cx     1    ix

        read(inS,*,end=210,err=928)     
     1    cdivid(nd),rec24,cgoto,
     1    idivsw(nd),divcap(nd), ix, ireptyp(nd),
     1    IDVCOM(nd), divefc(nd),AREA(nd),IRTURN(nd),
     1    ix,  cdividy(nd)

c
c rrb 2007/10/05; Insure area > 0
          area(nd)=amax1(area(nd),0.0)    
c
c rrb 2006/03/20; Adjust character string to left     
          cdivid(nd)=adjustl(cdivid(nd))
          cdividy(nd)=adjustl(cdividy(nd))     
          demsrc(nd)=float(ix)
          
c
c _________________________________________________________
c              Step 7; Allow blank data at end of file
        if(cdivid(nd).eq.blank) goto 210
        if(cgoto.eq.blank) then
          write(nlog,1210)  cdivid(nd), cgoto
          write(6,1210)     cdivid(nd), cgoto
          Goto 9999
        endif
c
c__________________________________________________________
c
c               Step 8; Store name as a real divnam for historical consistency
cx        j2=0
cx        do j=1,6
cx          j1=j2+1
cx          j2=j1+3
cx          rec4 = rec24(j1:j2)
cxc         read(rec4,*) divnam(j,nd)
cx          read(rec4,'(a4)') divnam(j,nd)
cx        end do  
c
c__________________________________________________________
c
c               Step 9; Daily model switches
c               -1 implies / day per month,
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
c
c rrb           Step 10; Allow multiple owners.
c		Note multi users are still not being supported
c               assume 1 user, revised read with variable ix
c
        nduser(nd+1) = 1        
        ndOwn(nd+1) = ix
        numOwnd=numOwnd+ndOwn(nd+1)
        
        noi=numOwnd-ndOwn(nd+1)+1
        noe=numOwnd
        no=noi
c
c _________________________________________________________
c
c		Step 11; Set river location of the diversion
C
        call stafind(nlog,0,0,numsta,is,cgoto,cstaid)            
        if(is.eq.0) goto 1404
        
        IDVSTA(ND)=IS
        istrtype(is)=nd   
c
c
c _________________________________________________________
c
c		Step 12; Print Diversion information
        if(iout.ne.0) then
c         write(99,541) nd, cdivid(nd), is
  541     format('  GetDiv2; nd, cdivid(nd), is', i5, 1x, a12, i5)
        endif
c _________________________________________________________
c
c		Step 14; Set user data
        IF(NDUSER(ND+1).EQ.0) Goto 650
        NUMUSE=NUMUSE+NDUSER(ND+1)
c       IF(NUMUSE.LE.MAXUSE) Goto 560
        IF(NUMUSE.gt.MAXUSE) then
          write(nlog,1220) NUMUSE,MAXUSE
          write(6,1220) numuse,maxuse
          Goto 9999
        endif
  560   NUI=NUMUSE-NDUSER(ND+1)+1
        NUE=NUMUSE
c
c _________________________________________________________
c
c		Step 15a; Set demand code
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
            if(idvcom(nd).eq.2.or.idvcom(nd).eq.4)then
              idvcom(nd) = 5
              write(nlog,1240)
            endif
c 
c rrb 00/05/30; Problem if baseflow mode & code says monthly IWR data 
c               is provided
            if(idvcom(nd).eq.3) then
              write(nlog,1241) cdivid(nd)
              idvcom(nd) = 1
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
c		Step 15b; STORE DIVERSION CODE FOR WHICH DEMAND 
c                         IS TO BE INPUT
c
c rrb 00/05/30; Allow IWR data
cr        IF(IDVCOM(nd).eq.5) Goto 580
          IF(IDVCOM(nd).eq.5) Goto 650
C   
          NDIVIN=NDIVIN+1

c
c _________________________________________________________
c
c 		Step 15c; Allow IWR data  
          ITMPT1=1
          IF(idvcom(nd).eq.1 .or. idvcom(nd).eq.3) itmpt2=2
c
c _________________________________________________________
c
c		Step 16; End diversion Station read
  650 CONTINUE
C
c
c _________________________________________________________
c
c		Step 17; Check diversion dimension
      write(nlog,1300) MAXDIV
      write(6,1300) maxdiv
      Goto 9999
      
C
c
c _________________________________________________________
c
c		Step 18; Initilize warning variable
  210 numdiv=amax0(nd-1,0)
      write(nlog,211) numdiv
  211 format(/,
     1  '  GetDiv2; Number of Diversions       = ', i5)
  
c          
c _________________________________________________________
c
c		Step 11; Read effieciency in new station format (*.def)              
      if(inE.gt.0) then
c
c ---------------------------------------------------------
c		Read units and year.check they are consistent        
        read(inE,*,end=300,err=928) cunit, cyrX
        cyrX=adjustl(cyrX)
        recin(1:5) = cyrX
c
c		Adjust to upper case
        call AdjCase(nlog, recin, recout, 5, 2) 
        cyrX=recout(1:5)
                 
        if(cyrX.ne.cyr1) then
          write(nlog,1290) cyrX, cyr1
          goto 9999
        endif
c
c ---------------------------------------------------------
c		Read Efficiency Data        
        inC=inE          
        Do n=1,maxdiv-1
 220      read(inE,*,end=300,err=928) cistat
        if(ioutE.eq.1) write(nlog,*)'  GetDiv2; Efficiency for ',cistat
 
          if(cistat(1:1).eq.'#') goto 220
          backspace(inE)
        
          read(inE,*,end=300,err=928) cistat, (x(i), i=1,12)
c
c rrb 2006/03/20; Adjust character string to left     
          cistat=adjustl(cistat)          
c
c          
c _________________________________________________________
c
c		Step 11a; Tie to structure
c			  Set to year type ALWAYS READ AS CALENDAR YEAR
          call stafind(nlog,0,3,numdiv,ix,cistat,cdivid)            
          if(ix.eq.0) goto 1400
          
          iwarn(ix)=1
	  do im=1,12
c	  
c		Adjust based on year type ALWAYS READ AS CALENDAR YEAR
	    j=imomo(im)
	    diveff(im,ix)=x(j)
            if(ioutE.gt.0) write(nlog,*) 'GetDiv2; ',im, j,diveff(im,ix)
          end do  
	end do  
c          
c _________________________________________________________
c
c		Step 11b; Set to average if not found      
 300    DO n=1,numdiv
          if(iwarn(n).eq.0) then
            do i=1,12
              diveff(i,n) = divefc(n)
            end do
          endif
c          
c _________________________________________________________
c
c		Step 11c; Detailed Check        
          if(ioutE.gt.0) then
            write(nlog,*) '  GetDiv2; Efficiency by model year type; ',
     1        'n, (diveff(i,n) ', 
     1         n, (diveff(i,n), i=1,12)
          endif          
        end do  
	
      endif
c _________________________________________________________
c
c		Step 12a; Get Return flow data in New format (*.drf)
c		Note ityp=0 for diversion return flows
      if(inR.gt.0) then
        inC=inR
        if(ioutR.gt.0)
     1    write(nlog,*) ' GetDiv2; Calling getRtn for Returns'
        ityp=0
c
c rrb 99/08/26; Character ID - added interv and cirtndlw
        call getrtn  (maxrtn,  maxsta,   maxdiv,   maxdvr,
     1               in1,inR,   
     1               nd,ji,je, nlog,     iin2,     numsta,
     1               ityp,     iloss,    interv,
     1               pcttot,   pctlos,   irtndl,  irnsta,
     1               numrtn,   nrtn,     numdiv,
     1               ndnnod,   idncod,   cgoto,    divnam1,
     1               cdivid,   cstaid,   cstadn,   filenaR,
     1               cirtndl)
c
c _________________________________________________________
c
c               Step 12b; Reset return flow array (nrtn) and 
c			Set Order and check array size limits
C
        nrtn(1)=1
        do nd=1,numdiv
          n1=nrtn(nd+1)
          nrtn(nd+1) =nrtn(nd+1) +nrtn(nd)
          if(ioutR.gt.0)
     1      write(nlog,*) 'GetDiv2; nd, n1, nrtn(nd), nrtn(nd+1) ', 
     1      nd, n1, nrtn(nd), nrtn(nd+1)
        end do
     
        
      endif
c
      numdxw=numdiv
c
c _________________________________________________________
c               Step 13; Assign diversion type (mondiv)
c
c        MONDIV = 0 : NO DIVERSION DATA WILL BE READ IN
c               = 1 : CONSTANT MONTHLY DIV DEMAND TO BE INPUT 1x/sim
c               = 2 : MONTHLY DIV DEMAND TO BE INPUT EVERY YEAR
c               = 3 : CASES 1 & 2 EXIST IN THE SYSTEM
c
      MONDIV=ITMPT1+ITMPT2
C
c _________________________________________________________
c		
c 		Step 14; Reset diversion user and diversion owner arrays
c rrb 04/09/07; Allow multiple structure owners	
      NDUSER(1)=1
      ndown(1)=1
      DO ND=1,NUMDIV
        NDUSER(ND+1)=NDUSER(ND+1)+NDUSER(ND)
        ndown(nd+1)=Ndown(nd+1)+ndown(nd)
      end do
C
c _________________________________________________________
c
c		Step 16; Set stream (istrtn) and order (irnord)
      if(numrtn.gt.0) then
        DO IS=1,NUMSTA
          ITEMP(IS)=0
        end do
C
        DO NR=1,NUMRTN
          IS=IRNSTA(NR)
          ITEMP(IS)=1
        end do
C
        NSTRTN=0
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
 710    continue
      endif
c
c _________________________________________________________
c
c 		Step 17a; Check return data array
      if(nstrtn.gt.maxrtn) then
        write(nlog,1021) maxrtn
        goto 9999
      endif
c
c _________________________________________________________
c
c		Step 17b; Check efficiency for a Tmtn (irturn=4)
         do nd=1,numdiv
           if(irturn(nd).eq.4) then 
             do i=1,12
               c = diveff(i,nd)-100.0
               if(c .gt. small) then
                 write(nlog,547) cdivid(nd)
c                diveff(i,nu) = 100.0
                 goto 9999
               endif
             end do
          endif
        end do  
      
c      
c _________________________________________________________
c
c		Step 18; Close Files
 730  if(inE.ne.0) close (inE)
      if(inR.ne.0) close (inR)
      close (inS)
c      
c _________________________________________________________      
c
c		Step 19; Return
      return
c      
c _________________________________________________________      
c
c		Error Handling
  926 write(nlog,927) inC, filena  
  927 format(/, 72('_'),/
     1  ' GetDiv2; Problem. End of file # ', i4, ' encountered',/,
     1  '   File name: ', a256)
      goto 9999
      
  928 write(nlog,929) inC, filena
  
  929 format(/, 72('_'),/
     1  ' GetDiv2; Problem reading file # ', i4,/,
     1  '   File name: ', a256,/
     1  '   Problem record (next line):')

      backspace(inC)
      read(inC, '(a132)',end=926,err=926) recin
      write(nlog,'(a132)') recin
      goto 9999
      
 1400 write(nlog,1402) cistat
 1402 FORMAT(/, 72('_'), /,
     1 '  GetWel2;; Problem.'
     1 ' Structure ',a12,' of diversion efficiency file (*.def)',
     1 ' not found in diversion station file') 
      goto 9999
      
 1404 write(nlog,1402) cistat
 1406 FORMAT(/, 72('_'), /,
     1 '  GetWel2;; Problem.'
     1 ' Structure ',a12,' of diversion station file (*.dst)',
     1 ' not found in river station file') 
      goto 9999
     
 1410  write(nlog,1420) filena
 1420  format('  GetDiv2; Problem opening file: ', a256)
       goto 9999      
c
c _________________________________________________________
c
c		Formats      
  547  format(/, 72('_'), /,
     1 '  GetDiv2; Problem for Div station = ', a12,
     1          ' type = 4 which indicates transmountain, but',/
     1 '          efficiency is not 100%.',
     1 '          To do; Change type or efficiency data to 100%')
  930  FORMAT(A256)
      
  940  format(/, 72('_'), /,
     1 '  GetDiv2; Diversion Station ',a24,/,4x, a256)
  
 1021  FORMAT(/, 72('_'), /,
     1  ' GetDiv2; Problem.',
     1  ' Too many return stations, max = ', i5)

 1200  format(a12,a24,a12,i8,f8.0,2i8,1x,a12)

 1202  format(a12,a24,a12,i8,f8.0,1x,a12,1x,f12.5,1x,a12)      
 
 1210  FORMAT(
     1  '  GetDiv2; Problem for diversion id = ', a12,/
     1  '          in the diversion station file (*.dds) ',/
     1  '          the river location (cgoto) = ', a12,/
     1  '          cannot be found in the network file (*.rin)',/
     1  '          (If the id looks ok, Check to be sure it',/
     1  '          does not contain a tab character')
     
 1220  FORMAT(/, 72('_'), /,
     1  '  GetDiv2; Problem. Too MANY DIVERSION USERS      ',I5,
     1  '  MAXIMUM = ',I5)
     
 1230  FORMAT(12X,a24,12x, 2I8,2F8.0,3I8)

 1232  format(12x,24x,a12, 3i8, 2f8.0, i8, f8.0)
 
 1238  format(/, 72('_'), /,
     1  '  GetDiv2; Warning, for baseflow calculations reset control',/
     1  '          variable icondem (idemtyp) = 1 to insure no ',/
     1  '          adjustments to historic diversion and well',/ 
     1  '          pumping data')

 1240  format(/, 72('_'), /,
     1  '  GetDiv2; Warning, Annual diversions not used in base',
     1  ' flow calcs'/)

 1241  format(/, 72('_'), /,
     1  '  GetDiv2; Problem, In Baseflow mode yet diversion code',
     1           ' in *.dds says IWR data provided (idvcom = 3).', 
     1           ' for ID = ', a12,/
     1  '          Set to 1 to avoid having historic diversion', 
     1           ' data weighted by efficiency.')
     
 1250  FORMAT(/, 72('_'), /,
     1  ' GetDiv2; Problem. Too MANY RETURN FLOW STATIONS, MAX = ',I5)
     
 1260  format(36x,a12,f8.0,i8)
 
 1262  format(36x,a12,f8.0,a12)
 
 1270  FORMAT(/, 72('_'), /,
     1  '  GetDiv2: Problem.',
     1  ' Return flow to: ',a12,' from Diversion:',/
     1  '          ID: ',a12, ' Named: ', a24,/
     1  '          was not found or is not allowed at the last',/
     1  '          river station.  For the latter case, add one',/
     1  '          additional river node to the *.rin file')
     
 1280  FORMAT(/, 72('_'), /,
     1  '  GetDiv2; Problem.'
     1  ' Station ',a12,' of diversion station file',
     1  ' has return flows going back to the diverting node',
     1  ' at ', a12)
     
 1282  FORMAT(/, 72('_'), /,
     1  '  GetDiv2; Warning the following diversion has a return ',/
     1  '          location that is available to the diverter:',/
     1  '          Diversion ID:                  ',a12,/, 
     1  '          Diversion name:                ',a24,/, 
     1  '          Located at river ID:           ',a12,/,  
     1  '          Has a return flow to river ID: ',a12)
     
 1283  FORMAT(/, 72('_'), /,
     1  '  GetDiv2; Problem the following structure(s) ',
     1    'return water upstream',//
     1  '    # Str ID       Str Name                ', 
     1  ' Located at   Returns to',/ 
     1  ' ____ ____________ ________________________', 
     1  ' ____________ ____________')
     
 1284  format(i5,1x, a12, 1x, a24, 1x, a12, 1x a12)
 
 1285  FORMAT(/, 72('_'), /,
     1 '  GetDiv2; Warning the following structure(s) return water',/
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

 1290  FORMAT(
     1 '  GetDiv2; Problem the Diversion Efficiency file (*.def)',/
     1 '    year type does not match the simulation year type',/
     1 '    Recommend you revise the following to match',/
     1 '    ', a5, 1x, a5)
     
 1291  format(2x, i5, 1x, a12, 1x, a24, 1x,3f8.0)
 
 1292  FORMAT(
     1 '  GetDiv2; Problem station ',a12,
     1 ' of the Direct Diversion Station File (*.dds)',/
     1  10x, ' has the following return data:',/
     1  10x, ' Total of all returns = ', f8.2)
     
 1300  FORMAT(/, 72('_'), /,
     1 ' GetDiv2; Problem.',
     1 ' Too MANY DIVERSION PROJECTS,     MAXIMUM = ',I5)      
     
 1500   format(/, 72('_'), /,
     1  '  GetDiv2; Problem diversion ownership is less than 100%',
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
 1440 format('  Stopped in GetDiv2, see the log file (*.log)')
 1450 format('  Stopped in GetDiv2')
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)
     
      stop     
      end
