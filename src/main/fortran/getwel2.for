c
c *********************************************************
C
      SUBROUTINE GetWel2(IIN, inx, numstax)
c
c
c _________________________________________________________
c	Program Description
c
c       GetWel2; it reads in well station data
c
c _________________________________________________________
c
c	Documentation
c
c               iin  =   response file #
c
c _________________________________________________________
c
c      Update History
c
c rrb 2005/10/14; Separated from Datinp and allowed revised file format
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
c
      DIMENSION ITEMP(numstax)

      dimension mthd(12), xmon(12), x(12)
C
      character ch2*2, ch3*3, ch4*4, blank*12, crtnid*12, xmon*4,
     1          cgoto2*12, cx*12, rec4*4, rec24*24,
     1          rec1*1, rec12*12, 
c
c rrb 2005/10/14; Separate return and depletion files     
     1          filenaS*256, filenaE*256, filenaR*256, filenaD*256,
     1          cistat*12
     
      character cyrX*5, cunit*5, recin*132, recout*132, cCallBy*12
c__________________________________________________________
c
c               Step 1; Initilize
c		Details iout = well data
c rrb 2005/10/14; Separate files
      write(nlog,90)
 90   format(/, '  Subroutine GetWel2')
c


      blank = '            '                       

      iout=0
      inS=0
      inE=0
      inR=0
      inD=0
      in1=0
      inC=0
      numdivwS=0
      numdivwX=0
      ibad=0
      ndemW=0
      cCallBy='GetWel2     '

      
c
c__________________________________________________________
c
c		Step 2; Exit if wells are turned off
      if(iwell.eq.0) goto 925
c
c__________________________________________________________
c
c               Step 3; Open Well Station File (*.wes)
c rrb; 98/11/18; Wells
      iprintdw=0
      iin2=iin

cx      rec256=fileName(57)
cx      filenaS=rec256(1:72)
cx      if(filenaS(1:2).ne.'-1') then
cx        inS=57
cx        write(nlog,940) 'New (*.wst)             ', filenaS          
cx        in1=inS
cxc       write(nlog,*) '  GetWel2; inS', inS      
cx        call putpath(maxfn, filenaS, fpath1)
cx        open(inS, file=filenaS,status='old',err=1410)
cx        call skipn(inS)                
cx      endif  
c
c__________________________________________________________
c
c               Step 4; Open Well Efficiency File (*.wef)
c
c rrb 2005/10/14; Separate efficiency, return and delay files
      rec256=fileName(58)
      filenaE=rec256(1:72)
      if(filenaE(1:2).ne.'-1') then
        inE=58          
        write(nlog,940) 'Efficiency (*.wef)      ', filenaE          
c       write(nlog,*) '  GetWel2; inE', inE      
        call putpath(maxfn, filenaE, fpath1)
        open(inE, file=filenaE,status='old',err=1410)
        call skipn(inE)                
      endif  
c
c__________________________________________________________
c
c               Step 5; Open Well Station Return Flow File (*.wrf)
c
c rrb 2005/10/14; Allow separate return file      
      rec256=fileName(59)
      filenaR=rec256(1:72)
      if(filenaR(1:2).ne.'-1') then
        inR=59
        write(nlog,940) 'Return Flow (*.wrf)     ', filenaR
c       write(nlog,*) '  GetWel2; inR', inR
        call putpath(maxfn, filenaR, fpath1)
        open(inR, file=filenaR,status='old',err=1410)
        call skipn(inR)                
      endif      
      
c
c__________________________________________________________
c
c               Step 6; Open Well Station Depletion File (*.wde)
        
      rec256=fileName(60)
      filenaD=rec256(1:72)
      if(filenaD(1:2).ne.'-1') then
        inD=60
        write(nlog,940) 'Depletion (*.wde)       ', filenaD          
c       write(nlog,*) '  GetWel2; inD', inD      
        call putpath(maxfn, filenaD, fpath1)
        open(inD, file=filenaD,status='old',err=1410)
        call skipn(inD)        
      endif  
c
c__________________________________________________________
c
c               Step 7; Exit if no station file is be provided
c rrb 2005/10/14; Separate station files
      if(inS.eq.0) then
        write(nlog,*) ' GetWel2; FYI no well data provided'
        numdivw=0
        goto 925
      endif
c
c__________________________________________________________
c
c               Step 8; Read Station data
c
      numrtnw=0
      numrtnw2=0
      iloss=0
      iprimary=0
C
      DO 200 nw=1,maxdivw+1
c      
c _________________________________________________________
c
c		Step 8a; Read New Well Station format (*.wst)
        inC=inS        
 100    read(inS,*,end=210,err=210) cistat
        if(iout.gt.0) write(nlog,*) '  GetWel2; Ret or Dep for ', cistat
 
        if(cistat(1:1) .eq. '#') goto 100
        backspace(inS)
c
c		Remove plan from read        
cx        read(inS,*,end=210,err=925) cdividw(nw),divnamw1(nw),cgoto,
cx     1    idivsww(nw),divcapw(nw), cdividyw(nw), primary(nw), rec12,
cx     1    cgoto2, idvcomw(nw), nrtnw(nw+1), nrtnw2(nw+1),
cx     1    divefcw(nw), areaw(nw),   irturnw(nw), demsrcw(nw)
     
        read(inS,*,end=210,err=925) cdividw(nw),divnamw1(nw),cgoto,
     1    idivsww(nw), divcapw(nw), primary(nw), cgoto2, 
     1    idvcomw(nw), divefcw(nw), areaw(nw),   irturnw(nw), 
     1    demsrcw(nw), cdividyw(nw)


c
c rrb 2009/06/18; Count number of demands to read in Mdainp  
c		    Note Idemtyp = icondem from datinp.for
c			  IF idemtyp = 1 Historic
c                                   2 Historic Sum
c                                   3 Structure Demand (D&W's have data in *.ddm)
c					 4 Supply Demand (see documentation)
c					 5 Decreed Demand (see documentation)
c			  if idvcomw = 1 Monthly demand provided
c					 2 NA
c					 3 Monthly IWR provided
c					 4 NA
c                                   5 Set to zero
c                                   6 D&W structures have demands in *.ddm
c		       
        if(idemtyp.eq.1 .and. idvcomw(nw).le.3) ndemW=ndemW+1
        if(idemtyp.eq.2 .and. idvcomw(nw).le.3) ndemW=ndemW+1
        if(idemtyp.ge.3 .and. idvcomw(nw).le.3) ndemW=ndemW+1
     
        if(iout.eq.1) then
          write(nlog,*) ' GetWel Demand Data     nw   ndemW',
     1     ' idemtyp idvcomw  cdividw'
          write(nlog,'(a24,4i8,1x,a12)') 
     1     ' GetWel Demand Data     ', nw, ndemW,
     1     idemtyp, idvcomw(nw), cdividw(nw)
        endif

c rrb 2007/10/05; Insure area > 0
c rrb 2007/11/16; Correction
c       areaw(nw)=amax1(area(nw),0.0)       
        areaw(nw)=amax1(areaw(nw),0.0)       
     
        if(iout.gt.0) then
          rec12='NA          '
          write(nlog,*) '  GetWel2; ' 
          write(nlog,*) cdividw(nw),divnamw1(nw),cgoto,
     1      idivsww(nw),divcapw(nw), cdividyw(nw), primary(nw),
     1      rec12,
     1      cgoto2, idvcomw(nw), nrtnw(nw+1), nrtnw2(nw+1),
     1      divefcw(nw), areaw(nw),   irturnw(nw), demsrcw(nw)
        endif
c
c rrb 2006/03/20; Adjust character string to left     
        cdividw(nw)=adjustl(cdividw(nw))
        cdividyw(nw)=adjustl(cdividyw(nw))        
c
c__________________________________________________________
c
c               Step 8; Store name as a real divnam for historical consistency
        rec24=divnamw1(nw)
cx        j2=0
cx        do j=1,6
cx          j1=j2+1
cx          j2=j1+3
cx          rec4 = rec24(j1:j2)
cx          read(rec4,'(a4)') divnamw(j,nw)
cx        end do  
        

     
        if(rec12(1:1).ne. ' ') then
          planw(nw)= rec12
        else  
          planw(nw)= 'N/A'
        endif  
c      
c _________________________________________________________
c
c		Step 8c; Set daily ID
c
c rrb; 04/25/97; Daily model -1 implies / day per month,
c                0=same as average monthly
c               -1=divide monthly by 31
c                1=use a daily pattern of another diversion
c                2=use a daily pattern of this diversion
        idaydw(nw)=0
        if(cdividyw(nw).ne.blank)          idaydw(nw)=2
        if(cdividyw(nw).eq.cdividw(nw))    idaydw(nw)=1
        if(cdividyw(nw).eq.'-1          ') idaydw(nw)=-1
        if(cdividyw(nw).eq.'0           ') idaydw(nw)=0
        if(cdividyw(nw).eq.'3           ') then
          idaydw(nw)=3
          cdividyw(nw)=cdividw(nw)
        endif
        
        if(cdividyw(nw).eq.'4           ') then
          idaydw(nw)=4
          cdividyw(nw)=cdividw(nw)
        endif

        ix=idint(primary(nw))
        if(ix.gt.0)                        iprimary = 1
c
c _________________________________________________________
c
c		Step 8d; Allow blank data at end of file
        if(cdividw(nw).eq.blank) goto 210

        if(cgoto.eq.blank) then
          write(nlog,1212)  cdividw(nw), cgoto
          write(6,1212)     cdividw(nw), cgoto
          Goto 9999
        endif
c
c      
c _________________________________________________________
c
c		Step 8e; Locate well in network
        ifound=0
        do IS=1,NUMSTA
          if(cstaid(is).eq.cgoto) ifound=is 
        end do
C
C------  IF STATION IS NOT FOUND, WRITE ERROR MESSAGE AND STOP PROGRAM
C
        if(ifound.eq.0) then
          write(nlog,1212) cdividw(nw), cgoto
          write(6,1212)    cdividw(nw), cgoto
          Goto 9999
        endif
C
        is=ifound
        idvstaw(nw)=ifound
C
C------  Idvcomw(nw)  = 1 : Monthly (12*years) from *.wem
C                     = 2 : Annual (12 values) used every year from *.wea
C                     = 3 : Monthly IWR data
C                     = 4 : Annual (12 values) IWR data
C                     = 5 : Demand is set to 0
c                     = 6 : Demand in *.ddm
c      
c _________________________________________________________
c
c		Step 8f; If simulating, warn annual data is not 
c                        supported for wells
        if(ioptio.eq.2 .or. ioptio.eq.8) then
          if(idvcomw(nw).eq. 2 .or. idvcomw(nw).eq.4)  then
            write(nlog,545) cdividw(nw), idvcomw(nw)
            goto 9999
          endif
c
c               Warn type 6 data provided but control variable not set
          if(idvcomw(nw).eq. 6 .and. idemtyp.le.2)  then
            write(nlog,546) cdividw(nw), idvcomw(nw), idemtyp
            goto 9999
          endif
        endif
c      
c _________________________________________________________
c
c		Step 8f; For baseflow mode (1, 9) reset some data
        if(ioptio.eq.1 .or. ioptio.eq.9) then
c
c               Reset annual data (2 or 4) to zero (type 5)
          if(idvcomw(nw).eq.2 .or. idvcomw(nw).eq.4) then
            idvcomw(nw) = 5
            write(nlog,1240)
          endif
c
c               Reset type 3 (IWR) data to type 1
          if(idvcomw(nw).eq.3 .or. idvcomw(nw).eq.6) then
            idvcomw(nw) = 1
            write(nlog,1239) cdividw(nw)
          endif         
c          
c               Reset control variable to insure no addition, etc.
          if(idemtyp.ne.1) then
            idemtyp=1
            write(nlog,1238)
          endif
        endif
c      
c _________________________________________________________
c
c		Step 8g; STORE DIVERSION CODE 
        IF(idvcomw(nw).le.4) then
c         nwelin=nwelin+1
          ITMPT1=1
          IF(idvcomw(nw).eq.2 .or. idvcomw(nw).eq.4) itmpt2=2
        endif
c
c      
c _________________________________________________________
c		Step 8h; Check for no return or depletion data
        IF(nrtnw(nw+1).eq.0 .or. nrtnw2(nw+1).eq.0) then
          write(nlog,1242) cdividw(nw)
          write(6,1242)    cdividw(nw)
          goto 9999
        endif
c
c _________________________________________________________
c
c		Step 8i; Check if demand is tied to a direct diversion
        ifound=0
        idivcow2(nw)=0

        do n=1,numdiv
          if(cdivid(n).eq.cgoto2) ifound=n
        end do
c
c               Set well to ditch and ditch to well pointers
        if(ifound.gt.0) then
          idivcow2(nw)=ifound
          idivco2(ifound)=nw 
          numdivwS=numdivwS+1
c
c ---------------------------------------------------------
c rrb 2008/10/27; Check that a supplemental well is located at the same
c		  structure as the diversion, else reporting has probs.
            isw=idvstaw(nw)
            isd=idvsta(ifound)
               
            if(idvstaw(nw) .ne. idvsta(ifound)) then
              write(nlog,1290) cdividw(nw), cstaid(isw), 
     1          cdivid(ifound), cstaid(isd)
              ibad=1
            endif
          
        endif
c
c
        if(ifound.eq.0) then 
          ch2=cgoto2(1:2)
          ch3=cgoto2(1:3)
          if(ch3.eq.'N/A' .or. ch2.eq.'NA') then
            numdxw=numdxw+1
            numdivwX=numdivwX+1
            
          else
            if(iprintdw.eq.0) write(nlog,1322)  
            iprintdw=1   
            write(nlog,1323) cdividw(nw), divnamw1(nw),cgoto2
c           goto 9999
          endif
        endif
c
c _________________________________________________________
c
c		Step 8j; End Well Station Loop
  200 CONTINUE
c
c
c _________________________________________________________
c
c		Step 9; Print dimension error (expected to branch out of
c               loop by reaching end of file or reading a blank ID
      write(nlog,1302) maxdivw
      Goto 9999
C
c
c _________________________________________________________
c
c		Step 10; Initilize warning variable
  210 numdivw=nw-1
      write(nlog,1270) numdivwS, numdivwX, numdivw
  
      do nw=1,numdivw
        iwarn(nw)=0
      end do
c          
c _________________________________________________________
c
c		Step 11; Read effieciency in new station format (*.wef)              
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
          write(nlog,1292) cyrX, cyr1
          goto 9999
        endif
c
c ---------------------------------------------------------
c		Read Well Efficiency Data      
        inC=inE          
        Do nw=1,maxdivw+1
 220      read(inE,*,end=300,err=928) cistat
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
          call stafind(nlog,0,6,numdivw,ix,cistat,cdividw,cCallBy)            
          if(ix.eq.0) goto 1400
          
          iwarn(ix)=1
	  do im=1,12
c	  
c		Adjust based on year type ALWAYS READ AS CALENDAR YEAR
	    j=imomo(im)
	    diveffw(im,ix)=x(j)
c           if(iout.gt.0) write(nlog,*) 'GetWel2; ',im, j,diveffw(im,ix)
          end do  
	end do  
      endif
c
c          
c _________________________________________________________
c
c		Step 11b; Set to average if not found      
 300  DO nw=1,numdivw
        if(iwarn(nw).eq.0) then
          do i=1,12
            diveffw(i,nw) = divefcw(nw)
          end do
        endif
c
c          
c _________________________________________________________
c
c		Step 11c; Detailed Check        
        if(iout.gt.0) then
          write(nlog,*) '  GetWel2; Efficiency by model year type; ',
     1      'nw, (diveffw(i,nw) ', 
     1       nw, (diveffw(i,nw), i=1,12)
	endif          
      end do  
c
         
c _________________________________________________________
c
c		Step 12; Get Return flow data in New format (*.wrf)
      if(inR.gt.0) then
c       write(nlog,*) '  GetWel2; calling getRtnW2 for Returns'
        ityp=0
c
c rrb 99/08/26; Character ID - added interv and cirtndlw
        call getrtnW2(
     1    nlog, nchk, iprintx,        
     1    maxrtnw,   maxsta,   maxdivw,  
     1    in1,inR,   nw,ji,je, iin2,     numsta,
     1    ityp,      iloss,    interv,
     1    pcttotw,   pctlosw,  irtndlw,  irnstaw,
     1    numrtnw,   nrtnw,    numdivw,
     1    ndnnod,    idncod,   cgoto,    divnamw1,
     1    cdividw,   cstaid,   cstadn,   filenaR)
       endif
c
c          
c _________________________________________________________
c		Step 13; Get Depletion data in New format (*.wde)
      if(inD.gt.0) then
        ityp=1
c       write(nlog,*) '  GetWel2; calling getRtnW2 for Depletions'
        
c
c rrb 99/08/26; Character ID - added interv and cirtndl2
        call getrtnW2(
     1    nlog, nchk, iprintx,        
     1    maxrtnw,   maxsta,   maxdivw,  
     1    in1, inD,  nw,ji,je, iin2,     numsta,
     1    ityp,      iloss,    interv,
     1    pcttotw2,  pctlosw2, irtndlw2, irnstaw2,
     1    numrtnw2,  nrtnw2,   numdivw,
     1    ndnnod,    idncod,   cgoto,    divnamw1,
     1    cdividw,   cstaid,   cstadn,   filenaD)
      endif
c
c _________________________________________________________
c
c               Step 14; Redefine # of returns (nrtnw) and 
c                       # of depletion (nrtnw2) arrays
c			Set Order and check array size limits
C
      nrtnw(1)=1
      nrtnw2(1)=1
      do nw=1,numdivw
        nrtnw(nw+1) =nrtnw(nw+1) +nrtnw(nw)
        nrtnw2(nw+1)=nrtnw2(nw+1)+nrtnw2(nw)
      end do
C
      DO IS=1,NUMSTA
        ITEMP(IS)=0
      end do
C
      DO NR=1,NUMRTNw
        IS=irnstaw(NR)
        ITEMP(IS)=1
      end do
c
c rrb 98/12/22; Well depletion
      DO NR=1,NUMRTNw2
        IS=irnstaw2(NR)
        ITEMP(IS)=1
      end do
C
c rrb 98/12/11; Set order
c               Not well specific, build on diversions
c     NSTRTN=0
      DO IS=1,NUMSTA
        IF(ITEMP(IS).ne.0) then
          NSTRTN=NSTRTN+1
           istrtn(nstrtn)=is
           irnord(is)=nstrtn
        endif
      end do
c
c rrb01/10/08; New dimension check for return data
      if(nstrtn.gt.maxsta) then
        write(nlog,1021) maxsta
        goto 9999
      endif
c      
c _________________________________________________________
c		Step 15; Close Files
      if(inE.ne.0) close (inE)
      if(inR.ne.0) close (inR)
      if(inD.ne.0) close (inD)
      close (inS)
        
c
c _________________________________________________________
c               Step 16; Return
c
  925 continue
c
c rrb 2008/10/27;
      if(ibad.eq.1) goto 9999      
     
      RETURN
c
c               Error Handling
c _________________________________________________________
c
  926 write(nlog,927) inC, filena
  927 format(' GetWel2; Problem. End of file # ', i4, ' encountered',/,
     1       '   File name: ', a256)
      goto 9999
c
  928 write(nlog,929) inC, filena
  929 format(' GetWel2; Problem reading file # ', i4,/,
     1       '   File name: ', a256,/
     1       '   Problem record (next line):')

      backspace(inC)
      read(inC, '(a132)',end=926,err=926) recin
      write(nlog,'(a132)') recin
      goto 9999
c
c               Formats
c ___________________________________________________
  545  format(/,72('_'),/,
     1 '  GetWel2; Problem well id ', a12,
     1    ' has idvcomw = ', i8, ' which is not supported.')
     
  546  format(/,72('_'),/,
     1 '  GetWel2; Problem in *.wes well id ', a12,
     1          ' has idvcomw = ', i4, /
     1 '          But in *.ctl the variable icondem (idemtyp) = ',i4,
     1          ' which is inconsitant.')
     
  547  format(/,72('_'),/,
     1 '  GetWel2; Problem for Div or Well station = ', a12,
     1          ' type = 4 which indicates transmountain, but',/
     1 '          efficiency is not 100%.',
     1 '          To do; Change type or efficiency data to 100%')

  930  FORMAT(A256)
  940  format(/,
     1 '  GetWel2; Well Station ',a24,/,4x, a256)
     
 1021  FORMAT(/,72('_'),/,
     1  ' GetWel2; Problem.',
     1  ' Too many return stations, max = ', i5)
     
 1203  format(a12,a24,a12,i8,f8.0,1x,a12,1x,f12.5,1x,a12)      
     
 1212  FORMAT(/,72('_'),/,
     1  '  GetWel2; Problem for well id = ', a12,/
     1  '          in the well station file (*.wes) ',/
     1  '          the river location (cgoto) = ', a12,/
     1  '          cannot be found in the network file (*.rin)',/
     1  '          (If the id looks ok, Check to be sure it',/
     1  '          does not contain a tab character')
 1230  FORMAT(12X,a24,12x, 2I8,2F8.0,I8)
 1232  format(12x,24x,a12, 3i8, 2f8.0, i8, f8.0)

 1242  format(/,
     1  '  GetWel2; Warning, Wells must have a return & depletion',
     1  ' location',/ '          Check ID ', a12 )
 1243  format(/,
     1  '  GetWel2; Problem, In Baseflow mode yet diversion code',/
     1  '          in *.wes says *.ddc data provided (idvcomw = 3).',/ 
     1  '          Stopped to avoid having historic pumping',/, 
     1  '          data weighted by efficiency.  To do: revise *.wes')
 1250  FORMAT(/,72('_'),/,
     1  ' GetWel2; Problem. Too MANY RETURN FLOW STATIONS, MAX = ',I5)
     
 1255 format(/,72('_'),/,
     1'  GetWel2; Problem with Well Station ', a12,/
     1 10x,'The augmentation plan code (planw) = ', a12,
     1 10x,'But this plan is type ', i4,/
     1 10x,'(Note a well agumentation plan should be a type 2)')
     
 1256 format(/,
     1 '  GetWel2; FYI at least one Well Station (e.g.) ', a12,/
     1 9x,' is tied to an augmentation plan (planw) = ', a12,
     1     ' plan pointer = ', i5)
     
 1257 format(/,
     1 '  GetWel2; Warning Well Station ', a12,/
     1 9x,' IS NOT TIED to an augmentation plan (planw)',/
     1    ' Plan ID = ', a12,' plan pointer = ', i5,/
     1    ' If OK enter N/A or NA as the plan ID')
     
 1260  format(36x,a12,f8.0,i8)
 
 1262  format(36x,a12,f8.0,a12)

 1292  FORMAT(
     1 '  GetWel2; Problem the Well Efficiency file (*.wef)',/
     1 '    year type does not match the simulation year type',/
     1 '    Recommend you revise the following to match',/
     1 '    ', a5, 1x, a5) 
     
 1290  FORMAT(/,
     1 '  GetWel; Problem. When a well is tied to a diversion, ',/
     1 '          they must be located at the same river ID',/
     1 '          Well Station ', a12,' is located at river ID: ',a12,/
     1 '          Div Station  ', a12,' is located at river ID: ',a12,/
     1 '          Reconmend you revise the location data or',/
     1 '          the variable that indicates they are tied (idvcow2)')
     
 
 1302  FORMAT(/,72('_'),/,
     1 ' GetWel2; Problem. Too MANY Well Stations,     MAXIMUM = ',I5)

 1310  FORMAT(a12,a24,a12,i8,f8.0,1x,a12)
c
c rrb 04/25/97; Daily model
 1312  FORMAT(a12, a24, a12, 1x, a12)
 
 1322  FORMAT(/,
     1  '  GetWel2; Warning Well structure not tied to a SW structure',/
     1  '          and not specified as N/A or a SW structure cannot',/ 
     1  '          be found in the diversion station file (*.dds).',/
     1  '          Treating these as well only structures',//
     1  '  Well ID      Well Name                SW ID',/ 
     1  '  ____________ ________________________ ____________')
     
 1323  format(/,
     1  'GetWel2; ', 2x, a12, 1x, a24, 1x, a12)    
c
 1238  format(/,
     1  '  DetWel2; Warning, for baseflow calculations reset control',/
     1  '          variable icondem (idemtyp) = 1 to insure no ',/
     1  '          adjustments to historic diversion and well',/ 
     1  '          pumping data')
     
 1239  format(/,
     1  '  DetWel2; Warning, in baseflow therefore ID ', a12, 'has had',/
     1  '          its demand type (idvcomw) reset to 1.  Recall',/
     1  '          the baseflow mode reads *.weh in lie of *.wem')
     
 1240  format(/,
     1  '  DetWel2; Warning, Annual diversions not used in base',
     1  ' flow calcs'/)
 
 1270 format(/,
     1 '  GetWel; Number of Supplemental Wells = ', i5,/
     1 '          Number of Sole Source Wells  = ', i5,/
     1 '          Number of Wells Total        = ', i5)
       goto 9999
c
c               Error Tracking
c _________________________________________________________
      
 1410  write(nlog,1420) filena
 1420  format('  GetWel2; Problem opening file: ', a256)
      goto 9999      
      
 1400 write(nlog,1402) cistat
 1402 FORMAT(/,72('_'),/,
     1 '  GetWel2;; Problem.'
     1 ' Structure ',a12,' of well efficiency file (*.wef)',
     1 ' not found in well station file') 
      goto 9999
      

 9999 write(6,1440) 
      write(nlog,1450) 
      call flush(6)
 1440 format('  Stopped in GetWel2, see the log file (*.log)')
 1450 format('  Stopped in GetWel2')
 
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop 
      END



