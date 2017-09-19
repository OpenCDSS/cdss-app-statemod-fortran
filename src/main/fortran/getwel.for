
c
      SUBROUTINE GetWel(IIN, inx, numstax)
c
c
c _________________________________________________________
c	Program Description
c
c       GetWel; it reads in well station data
c
c _________________________________________________________
c	Documentation
c
c               iin  =   response file #
c
c _________________________________________________________
c	Update History
c
c rrb; 2005/10/14; Separated from Datinp
c
c _________________________________________________________
c	Dimensions
      include 'common.inc'
c
      DIMENSION ITEMP(numstax), cgotoX(numstax), cgotoX2(numstax),
     1          divefcY(numstax)
c
      dimension mthd(12), xmon(12), x(12)
C
      character ch2*2, ch3*3, ch4*4, blank*12, crtnid*12, xmon*4,
     1          recin*256, cgoto2*12, cx*12, rec4*4, rec24*24,
     1          rec1*1, rec12*12,
c
c rrb; 2005/10/14; Separate return and depletion files     
     1          filenaS*256, filenaE*256, filenaR*256, filenaD*256,
     1          cistat*12, rec3*3, cgotoX*24, cgotoX2*24
c     
c__________________________________________________________
c               Initilize

C		0=no details
c		1=detailed of data read
c		2=summary of data read
c		99=Print new format
c     iout=99
      iout=0

cr    write(nlog,*) ' Subroutine GetWel'
c      
      if(iout.gt.0) write(nlog,*) ' GetWel; iwell = ', iwell


      blank = '            '                       
      iprintX=0
      small=0.001
      numdivw=0
      numdivwS=0
      numdivwX=0
      ibadS=0
      ibadR=0
      inC=55
      

      if(iwell.eq.0) goto 925
      iprintdw=0
      write(nlog,106)
  106 format(/,72('_'),
     1 /,'  GetWel; Well Station File (*.wes) ')
      iin2=iin


cx      if(infile.eq.1) then
cx        ifn=9
cx        rec256=fileName(ifn)
cx        filena=rec256(1:72)
cxc       write(nlog,*) ' GetWel; fileName = ', fileName(ifn)
cxc       write(nlog,*) ' GetWel; filena   = ', filena
cx      else   
cx        filena=filenc
cx        READ (IIN,930,end=926,err=928) FILENA
cx      endif
cxc
cxc		Allow no file to be provided
cx      if(filena(1:2).eq.'-1') then
cx        write(nlog,*) ' GetWel; FYI no well data provided'
cx        goto 925
cx      endif
      
      
      if(iwell.eq.-1) goto 925
cxc
cxc rrb;1999/05/20
cxc     write(nlog,940) '(*.wes)                 ', filena                
cx      call putpath(maxfn, filena, fpath1)
cx      open(55, file=filena,status='old',err=1410)
cx      iin2=55
cx      inC=55
cx      
cx      call skipn(55)
c
c _________________________________________________________
c
c		Open files to print new format      
C
      
      if(iout.eq.99) then
        call namext(maxfn, filenc, 'wst', filena) 
        open(56,FILE=filena,STATUS='Unknown')
        call outtop(56,1,48)
        
        call namext(maxfn, filenc, 'wef', filena) 
        open(57,FILE=filena,STATUS='Unknown')
        call outtop(57,1,48)
        
        call namext(maxfn, filenc, 'wrf', filena) 
        open(58,FILE=filena,STATUS='Unknown')
        call outtop(58,1,48)

        call namext(maxfn, filenc, 'wde', filena) 
        open(59,FILE=filena,STATUS='Unknown')
        call outtop(59,1,48)
      endif  
        
C
      numrtnw=0
      numrtnw2=0
      iloss=0
      iprimary=0
      ndemW=0
C
      DO 652 nw=1,maxdivw+1
c
c		Note iocode 1 = Data, 2 = EOF, 3 = Error                    
        call comment(55, nlog, iocode, nchk, 0)
        if(iocode.eq.2) goto 662
        if(iocode.eq.3) goto 928      
c
c               Read Well Station info
        READ(55,1202,end=662,err=1420)
     1    cdividw(nw),rec24,cgoto,
     1    idivsww(nw),divcapw(nw), cdividyw(nw), primary(nw),
     1    rec12

     
          cistat=cdividw(nw)
          
c
c ---------------------------------------------------------
c		Store name as a real divnam for historical consistency
c
        divnamw1(nw)=rec24
        
        j2=0
cx        do j=1,6
cx          j1=j2+1
cx          j2=j1+3
cx          rec4 = rec24(j1:j2)
cx          read(rec4,'(a4)') divnamw(j,nw)
cx        end do  
cx     
        if(iout.eq.1) write(nlog,1203) nw, 
     1    cdividw(nw),rec24,cgoto,
     1    idivsww(nw),divcapw(nw), cdividyw(nw), primary(nw),
     1    rec12
c
c rrb; 2006/03/20; Adjust character string to left     
          cdividw(nw)=adjustl(cdividw(nw)) 
          cdividyw(nw)=adjustl(cdividyw(nw))    
     
          cgotoX(nw)=cgoto
c
c rrb; 2004/25/97; Daily model -1 implies / day per month,
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
c
c rrb; 2001/08/08; Add type 4 (pattern is via connecting mid points)
        if(cdividyw(nw).eq.'4           ') then
          idaydw(nw)=4
          cdividyw(nw)=cdividw(nw)
        endif


        ix=idint(primary(nw))
        if(ix.gt.0)                        iprimary = 1
c
c              Allow blank data at end of file
        if(cdividw(nw).eq.blank) goto 662

        if(cgoto.eq.blank) then
          write(nlog,1212)  cdividw(nw), cgoto
          write(6,1212)     cdividw(nw), cgoto
          Goto 9999
        endif
c
c               Locate well in network
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
c        if(ichk.ne.0) then
c          write(99,543) nw, cdividw(nw), is
c  543     format('  GetWel; nw, cdividw(nw), is', i5, 1x, a12, i5)
c        endif
c
c               Read User Data
        READ(55,1232,end=926,err=928)
     1      cgoto2, idvcomw(nw), nrtnw(nw+1), nrtnw2(nw+1),
     1      divefcX, areaw(nw),   irturnw(nw), demsrcw(nw)
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
c        
c rrb; 2007/10/05; Insure area > 0
c
c rrb; 2007/11/16; Correction
c       areaw(nw)=amax1(area(nw),0.0)       
        areaw(nw)=amax1(areaw(nw),0.0)       

        if(iout.eq.1) write(nlog,1232)
     1      cgoto2, idvcomw(nw), nrtnw(nw+1), nrtnw2(nw+1),
     1      divefcX, areaw(nw),   irturnw(nw), demsrcw(nw)
c       
     
        if(cgoto2(1:3).eq. 'N/A') cgoto2(1:3) = 'NA ' 
c
c rrb; 2006/03/20; Adjust character string to left     
        cgoto2=adjustl(cgoto2)         
        
        cgotoX2(nw)=cgoto2
        divefcY(nw)=divefcX

c
c               Read monthly efficiency data
        if(divefcX .lt. -0.001) then
          read(55,*,end=926,err=928) (diveffw(i,nw), i=1,12)
        else
          do i=1,12
            diveffw(i,nw) = divefcX
          end do
        endif
c
c rrb; 2001/02/01; Insure efficiency is set properly if Tmtn (irturnw=4)
        if(irturnw(nw).eq.4) then 
          do i=1,12
            c = diveffw(i,nw)-100.0
            if(c .gt. small) then
              write(nlog,547) cdividw(nw)
c             diveffw(i,nw) = 100.0
              goto 9999
            endif
          end do
        endif
C
C------  Idvcomw(nw)  = 1 : Monthly (12*years) from *.wem
C                     = 2 : Annual (12 values) used every year from *.wea
C                     = 3 : Monthly IWR data
C                     = 4 : Annual (12 values) IWR data
C                     = 5 : Demand is set to 0
c                     = 6 : Demand in *.ddm

c
c               If simulating, warn annual data is not supported for wells
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
c rrb; 2000/05/30; For baseflow mode (1, 9) reset some data
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

C
C------  STORE DIVERSION CODE FOR WHICH DEMAND IS TO BE INPUT
        IF(idvcomw(nw).le.4) then
c         nwelin=nwelin+1
          ITMPT1=1
          IF(idvcomw(nw).eq.2 .or. idvcomw(nw).eq.4) itmpt2=2
        endif
c
c               Check for no return or depletion data
        IF(nrtnw(nw+1).eq.0 .or. nrtnw2(nw+1).eq.0) then
          ibadR=ibadR+1
          if(ibadR.eq.1) write(nlog,1241)
          write(nlog,1242) ibadR, cdividw(nw)
c         write(6,1242)    cdividw(nw) 
cx        goto 9999
        endif
c
c               Check if demand is tied to a direct diversion structure
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
c rrb; 2008/10/27; Check that a supplemental well is located at the same
c		  structure as the diversion, else reporting has probs.
            isw=idvstaw(nw)
            isd=idvsta(ifound)
               
            if(idvstaw(nw) .ne. idvsta(ifound)) then
              write(nlog,1290) cdividw(nw), cstaid(isw), 
     1          cdivid(ifound), cstaid(isd)
              ibadS=ibadS+1
            endif
          endif
c
c
          if(ifound.eq.0) then 
            ch3=cgoto2(1:3)
            if(ch3.eq.'NA ') then
              numdxw=numdxw+1
              numdivwX=numdivwX+1
            else
              if(iprintdw.eq.0) write(nlog,1281)              
              if(iprintdw.eq.0) write(nchk,1322)  
              iprintdw=1   
              write(nchk,1323) cdividw(nw), divnamw1(nw), cgoto2
c             goto 9999
            endif
          endif
c
c               Get Return flow data
        numrtnw=numrtnw+nrtnw(nw+1)
        IF(numrtnw.gt.maxrtnw) then
          write(nlog,1250) maxrtnw
          write(6,1250) maxrtnw
          Goto 9999
        endif

        JI=NUMRTNw-NRTNw(nw+1)+1
        JE=NUMRTNw
        ityp=0
        if(iout.eq.1) write(nlog,*) '  GetWel; calling getrtnw 1'
c
c  rrb; 1999/08/26; Character ID - added interv and cirtndlw
        call getrtnw(
     1    nlog, nchk, iprintx,        
     1    maxrtnw,   maxsta,   maxdivw,  maxdvrw, 
     1    nw,ji,je,  iin2,     numsta,
     1    ityp,      iloss,    interv,
     1    pcttotw,   pctlosw,  irtndlw,  irnstaw,
     1    ndnnod,    idncod,   cgoto,    divnamw1,
     1    cdividw,   cstaid,   cstadn,   filena,
     1    cirtndlw)
     
        if(iout.eq.1) write(nlog,*) '  GetWel; back from getrtnw 1'     
c
c               Get Depletion data
        numrtnw2=numrtnw2+nrtnw2(nw+1)
        IF(numrtnw2.gt.maxrtnw) then
          write(nlog,1250) maxrtnw
          write(6,1250) maxrtnw
          Goto 9999
        endif

        JI=NUMRTNw2-NRTNw2(nw+1)+1
        JE=NUMRTNw2
        ityp=1
c
c rrb; 2099/08/26; Character ID - added interv and cirtndl2
        if(iout.eq.1) write(nlog,*) '  GetWel; calling getrtnw 2'
        call getrtnw(
     1    nlog, nchk, iprintx,                
     1    maxrtnw,   maxsta,   maxdivw,  maxdvrw,
     1    nw,ji,je,  iin2,     numsta,
     1    ityp,      iloss,    interv,
     1    pcttotw2,  pctlosw2, irtndlw2, irnstaw2,
     1    ndnnod,    idncod,   cgoto,    divnamw1,
     1    cdividw,   cstaid,   cstadn,   filena,
     1    cirtndl2)
c
c               End Well Station Loop
        if(iout.eq.1) write(nlog,*) '  GetWel; 652 continue nw', nw
  652 CONTINUE
c
c               Print dimension error (expected to branch out of
c               loop by reaching end of file or reading a blank ID
      write(nlog,1302) maxdivw
      Goto 9999
C
  662 close (55)
c
c _________________________________________________________
c               Step X; Redefine # of returns (nrtnw) and 
c                       # of depletion (nrtnw2) arrays
      numdivw=nw-1
      write(nlog,1270) numdivwS, numdivwX, numdivw, ndemW
      
C
      nrtnw(1)=1
      nrtnw2(1)=1
      do nw=1,numdivw
        nrtnw(nw+1) =nrtnw(nw+1) +nrtnw(nw)
        nrtnw2(nw+1)=nrtnw2(nw+1)+nrtnw2(nw)
      end do
c
c _________________________________________________________
c               Step X; Check number of returns arrays and set order
C
      DO IS=1,NUMSTA
        ITEMP(IS)=0
      end do
C
c		WEll returns
      DO NR=1,NUMRTNw
        IS=irnstaw(NR)
        ITEMP(IS)=1
      end do
c
c rrb; 1998/12/22; Well depletion
      DO NR=1,NUMRTNw2
        IS=irnstaw2(NR)
        ITEMP(IS)=1
      end do
C
c rrb; 1998/12/11; Count and set order
c     NSTRTN=0
      DO IS=1,NUMSTA
        IF(ITEMP(IS).ne.0) then
          NSTRTN=NSTRTN+1
           istrtn(nstrtn)=is
           irnord(is)=nstrtn
        endif
      end do
c
c rrb; 2001/10/08; Check
      if(nstrtn.gt.maxrtnA) then
        write(nlog,1021) maxrtnA
        goto 9999
      endif
c
c _________________________________________________________      
c
c		Step X; Print new format if iout = 99
      if(iout.eq.99) then      
        do nw=1,numdivw
c
c		Station (*.wst) 
          rec12='NA          '    
          write(56,1204)
     1      cdividw(nw),divnamw1(nw), cgotoX(nw),
     1      idivsww(nw),divcapw(nw), cdividyw(nw), primary(nw),
     1        cgotoX2(nw), idvcomw(nw), -1, -1,
     1        divefcY(nw), areaw(nw),   irturnw(nw), demsrcw(nw)
 1204  format(a12,' "',a24,'" ',a12,i8,f8.0,1x,a12,1x,f12.5,1x,
     1 a12, 3i8, 2f8.0, i8, f8.0)
     
c1204  format(a12,' "',a24,'" ',a12,i8,f8.0,1x,a12,1x,f12.5,1x,a12)           
c1232  format(12x,24x,a12, 3i8, 2f8.0, i8, f8.0)
 
     
c
c		Efficiency (*.wef)     
          write(57,'(a12, 20f8.0)')cdividw(nw),(diveffw(i,nw), i=1,12)
c
c		Returns (*.wrf)        
          j1=nrtnw(nw)
          j2=nrtnw(nw+1)-1
          j3=0
          rec3='Rtn'
          write(58,'(a1)') '#'
          do j=j1, j2
            j3=j3+1
            write(58, '(a12,1x,a12,f8.4,1x, i8,1x, a3, i2)')
     1        cdividw(nw), cstaid(irnstaw(j)),pcttotw(j),irtndlw(j),
     1        rec3, j3
          end do
c
c		Depletions (*.wde)        
          j1=nrtnw2(nw)
          j2=nrtnw2(nw+1)-1
          j3=0
          rec3='Dep'
          write(59,'(a1)') '#'          
          do j=j1,j2
            j3=j3+1
            write(59, '(a12,1x,a12,f8.4,1x,i8, 1x, a3, i2)')
     1        cdividw(nw), cstaid(irnstaw2(j)),pcttotw2(j),irtndlw2(j),
     1        rec3, j3
          end do
        end do
        close(56)
        close(57)
        close(58)
        close(59)
      endif  
c
c rrb; 2008/10/27;
 925  continue
c
c rrb; 2009/03/12; Allow no returns
cx    if(ibadS.gt.0) goto 9999      
c
c rrb; 2009/03/13; Back to original
      if(ibadS.gt.0 .or. ibadR.gt.0) goto 9999 
           
c
c _________________________________________________________
c               Step X; Return
c
      RETURN
c _________________________________________________________
c
c               Error Handling

c
  926 write(nlog,927) 55, filena
  927 format(' GetWel; Problem. End of file # ', i4, ' encountered',/,
     1       '   File name: ', a256)
      goto 9999
c
  928 write(nlog,929) 55, filena
  929 format(' GetWel; Problem reading file # ', i4,/,
     1       '   File name: ', a256,/
     1       '   Problem record (next line):')

      backspace(inC)
      read(55, '(a256)',end=926,err=926) recin
      write(nlog,'(a256)') recin
      goto 9999
c
c               Formats
c ___________________________________________________
  545  format(/,'  GetWel; Problem well id ', a12,
     1    ' has idvcomw = ', i8, ' which is not supported.')
  546  format(/,
     1 '  GetWel; Problem in *.wes well id ', a12,
     1          ' has idvcomw = ', i4, /
     1 '          But in *.ctl the variable icondem (idemtyp) = ',i4,
     1          ' which is inconsitant.')
  547  format(/,
     1 '  GetWel; Problem for Div or Well station = ', a12,
     1          ' type = 4 which indicates transmountain, but',/
     1 '          efficiency is not 100%.',
     1 '          To do; Change type or efficiency data to 100%')

  930  FORMAT(A256)
  940  format(/, 72('_'), /,
     1 '  GetWel; Well Station ', a24,/,4x, a256)
  
 1021  FORMAT(/,
     1  ' GetWel; Problem.',
     1  ' Too many return stations, max = ', i5)
 1202  format(a12,a24,a12,i8,f8.0,1x,a12,1x,f12.5,1x,a12)           
 1203  format(i5, 1x, a12,a24,a12,i8,f8.0,1x,a12,1x,f12.5,1x,a12)
     
 1212  FORMAT(
     1  '  GetWel; Problem for well id = ', a12,/
     1  '          in the well station file (*.wes) ',/
     1  '          the river location (cgoto) = ', a12,/
     1  '          cannot be found in the network file (*.rin)',/
     1  '          (If the id looks ok, Check to be sure it',/
     1  '          does not contain a tab character')
 1230  FORMAT(12X,a24,12x, 2I8,2F8.0,I8)
 1232  format(12x,24x,a12, 3i8, 2f8.0, i8, f8.0)

c
 1238  format(/,
     1  '  GetWel; Warning, for baseflow calculations reset control',/
     1  '          variable icondem (idemtyp) = 1 to insure no ',/
     1  '          adjustments to historic diversion and well',/ 
     1  '          pumping data')
 1239  format(/,
     1  '  GetWel; Warning, in baseflow therefore ID ', a12, 'has had',/
     1  '          its demand type (idvcomw) reset to 1.  Recall',/
     1  '          the baseflow mode reads *.weh in lie of *.wem')
 1240  format(/,
     1  '  GetWel; Warning, Annual diversions not used in base',
     1  ' flow calcs'/)
c     
 1241  format(/,
     1  '  GetWel; Warning, Wells without a return & depletion',
     1  ' location',/,
     1  '    # Well ID     ',/
     1  ' ____ ____________')
 1242  format(i5, 1x, a12)  

 1243  format(/,
     1  '  GetWel; Problem, In Baseflow mode yet diversion code',/
     1  '          in *.wes says *.ddc data provided (idvcomw = 3).',/ 
     1  '          Stopped to avoid having historic pumping',/, 
     1  '          data weighted by efficiency.  To do: revise *.wes')
 1250  FORMAT(/,
     1  ' GetWel; Problem. Too MANY RETURN FLOW STATIONS, MAX = ',I5)
 1255 format(/'  GetWel; Problem with Well Station ', a12,/
     1 10x,'The augmentation plan code (planw) = ', a12,
     1 10x,'But this plan is type ', i4,/
     1 10x,'(Note a well agumentation plan should be a type 2)')
 1256 format(/'  GetWel; FYI at least one Well Station (e.g.) ', a12,/
     1 9x,' is tied to an augmentation plan (planw) = ', a12,
     1     ' plan pointer = ', i5)
 1257 format(/'  GetWel; Warning Well Station ', a12,/
     1 9x,' IS NOT TIED to an augmentation plan (planw)',/
     1    ' Plan ID = ', a12,' plan pointer = ', i5,/
     1    ' If OK enter NA as the plan ID')
     
 1260  format(36x,a12,f8.0,i8)
 1262  format(36x,a12,f8.0,a12)
 
 1270 format(/,
     1 '  GetWel; Number of Supplemental Wells    = ', i5,/
     1 '          Number of Sole Source Wells     = ', i5,/
     1 '          Number of Wells Total           = ', i5,/
     1 '          Number with demand data (*.wem) = ', i5)     
 
     
 1281  FORMAT(/,
     1  '  GetWel; Warning See *.chk for details.')
     
 1290  FORMAT(/,
     1 '  GetWel; Problem. When a well is tied to a diversion, ',/
     1 '          they must be located at the same river ID',/
     1 '          Well Station ', a12,' is located at river ID: ',a12,/
     1 '          Div Station  ', a12,' is located at river ID: ',a12,/
     1 '          recommend you revise the location data or',/
     1 '          the variable that indicates they are tied (idvcow2)')
     
 1302  FORMAT(/,
     1 ' GetWel; Problem. Too MANY Well Stations,     MAXIMUM = ',I5)

 1310  FORMAT(a12,a24,a12,i8,f8.0,1x,a12)
c
c rrb; 2004/25/97; Daily model
 1312  FORMAT(a12, a24, a12, 1x, a12)
 1322  FORMAT(/,
     1  '  GetWel; Warning Well structure not tied to a SW structure',/
     1  '          and not specified as NA or a SW structure cannot',/ 
     1  '          be found in the diversion station file (*.dds).',/
     1  '          Treating these as well only structures',//
     1  '  Well ID      Well Name                SW ID',/ 
     1  '  ____________ ________________________ ____________')
 1323  format(2x, a12, 1x, a24, 1x, a12)    
     

       goto 9999
c
c _________________________________________________________
c               Error Tracking
      
 1400 write(nlog,1402) cistat,iyr
 1402 FORMAT(/,
     1 '  GetWel;; Problem.'
     1 ' Structure ',a12,' of well efficiency file (*.wef)',
     1 ' not found in well station file') 
     
      numdivw=nw-1
      write(nlog,1270) numdivwS, numdivwX, numdivw, ndemW
     
      goto 9999
      
 1410 write(nlog,1412) filena
 1412 format('  GetWel; Problem opening file: ', a256)
      goto 9999      
      
      
 1420  write(nlog,1422) filena
 1422  format('  GetWel; Problem reading file: ', a256)
       numdivw=nw-1
       write(nlog,1270) numdivwS, numdivwX, numdivw, ndemW
 
      goto 9999      
c      
c _________________________________________________________
c		Print stop information
 9999 write(6,1440) 
      write(nlog,1450) 
      call flush(6)
 1440 format('  Stopped in GetWel, see the log file (*.log)')
 1450 format('  Stopped in GetWel')
 
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop 
      END



