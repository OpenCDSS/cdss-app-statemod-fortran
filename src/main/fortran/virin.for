c
c *********************************************************
c
      subroutine virin(ityp, iin)
c
c
c _________________________________________________________
c	Program Description
c
c       Virin; It is called 1x per simulation
c                 and once per month.
c                 When called 1x per simulation it redefines 
c                 diversion and stream data to be historic so 
c                 mdainp will continue to read each month.
c                 When called 1x per month, reads reservoir 
c                 eom data
c
c _________________________________________________________
c       Documentation
c               ityp = 0 called 1x per simulation
c               ityp = 1 called 1x per year
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
      dimension x(12)      
      character cistat*12, blank*12, czero*12, recin*256, rec48*48,
     1  cCallBy*12 
c jhb 2014/06/26 set these as local static variables...
c      integer, save :: numRre = 0
c      integer, save :: numDre = 0
c jhb 2014/07/08 remove the save command because we are using a global
c                static variable compiler switch -fno-automatic
      integer :: numRre = 0
      integer :: numDre = 0
c
c _________________________________________________________
c		Step 1; Initilize   
c		ioutR= Reservoir details
c		ioutRe=Diversion to Recharge details
c		ioutRe2=Reservoir recharge data
      iout=0
      ioutG=0
      ioutG1=0
      ioutNG=0
      ioutB=0   
      ioutR=0
      ioutRe=0
      ioutRe2=0
      cCallBy='Virin       '
      blank = '            '
      czero = '0           '
      if(iout.gt.0 .or. ioutRe.gt.0) then      
        write(nlog,*) ' Virin'
        write(6,*) ' Virin'
      endif
c
c =========================================================
c               Called 1x per simulation
      if(ityp.eq.0) then
        do is=1,numsta
          ibout(is)=0
          istaru(is)=0
        end do
        iin2=iin
        filena='*.rsp'
c
c _________________________________________________________
c		Initilize Recharge Data 
c       idvRre is reservoir to recharge
c		idvDre is diversion to recharge      
        do nr=1,numres
          idvRre(nr)=0
          do im=1,13
            RrechM(im,nr)=0.0
          end do
        end do
        do nd=1,numdiv
          idvDre(nd)=0
          do im=1,13
            DrechM(im,nd)=0.0
            DuseM(im,nd)=0.0
          end do
        end do
c
c _________________________________________________________
c
c		Step 1; Open historic EOM data (*.eom)
        rec48='EOM Content File (*.eom) - Monthly'
        write(nlog,100) rec48

        if(infile.eq.1) then
          ifn=25
          rec256 = fileName(ifn)
        endif
c
c rrb 3002/10/17; Correction (add else)
c rrb 2004/11/09; Correction need to open file above
        IF(NUMRES.EQ.0.OR.NRSACT.EQ.0) then
        else
          call chekpor(iin, 25, 99, 0, 0, 999, iystr, 
     1               imstr, 0, 25, c, cyr1, maxfn,
     1               infile, idummy, nEomX, fpath1, rec256)
        endif
c
c _________________________________________________________
c
c rrb 2008/10/28;
c		Step 2; Open historic Reservoir to Recharge (*.rre)
c
cx        rec48='Reservoir to Recharge File (*.rre) - Monthly'
cx        write(nlog,100) rec48

        rreFacto=0.0                   
        rec256(1:2) = '-1'
        
        if(infile.eq.1) then
          ifn=77
          rec256 = fileName(ifn)
        endif
c
        if(rec256(1:2).ne.'-1') then        
          rec48='Reservoir to Recharge File (*.rre) - Monthly'
          write(nlog,100) rec48
        
          call chekpor(iin, 77, 99, 0, 0, 999, iystr, 
     1               imstr, 0, 77, RreFacto, cyr1, maxfn,
     1               infile, idummy, nRre, fpath1, rec256)
c
c ---------------------------------------------------------
c rrb 2008/10/31
c		        istop=1 OK if not found
c			 itype=2 reservoir
          istop=1
          itype=2
          
          nno=0
          numRre=0 
          if(ioutRe2.eq.1) write(nlog,294) 'Reservoir to Recharge   '
c
c rrb 2008/11/05; Read mumsta values to allow extra data in the file          
c         DO NR=1,NUMRES
          DO NR=1,maxsta          
            read(77,*,end=90,err=928) iryr, cistat
            if(iryr.eq.iystr) then
              numRre=numRre+1
c
              call stafind(nlog,istop, itype, numres, nx, 
     1          cistat, cresid, cCallBy)
              idvRre(numRre)=nx             
c
c ---------------------------------------------------------
c			Warn but Allow data not in the system
c jhb 2016/06/26 have to handle nx=0 below!!
c              if(ioutRe2.eq.1) write(nlog,'(12x,3i8,1x,2a12)')
c     1             numRre, nr, nx, cistat, cresid(nx)
              if(ioutRe2.eq.1) then
                if(nx.eq.0) then
                  write(nlog,'(12x,3i8,1x,a12)')
     1             numRre, nr, nx, cistat
                else
                  write(nlog,'(12x,3i8,1x,2a12)')
     1             numRre, nr, nx, cistat, cresid(nx)
                endif
              endif
              if(nx.eq.0 .and. ioutRe2.eq.0) then            
                nno=nno+1
                if(nno.eq.1) write(nlog,290) rec48, 'Reservoir   '
                write(nlog,292) nno, cistat 
              endif
            endif  
c
c ---------------------------------------------------------
c			Exit if all stations have been read
            if(iryr.gt.iystr) goto 90            
          end do
c
c ---------------------------------------------------------
c			Warn and exit if dimension exceeded
          goto 320          
          
        else
c          write(nlog,*) ' '
c          write(nlog,*) ' Virin;  No Reservoir to Recharge (*.rre) ',
c     1                  'data provided'
          goto 93
        endif
c
c ---------------------------------------------------------
c		Reset file at beginning year        
 90     rewind(77)
        call skipn(77)
 92     read (77,*,end=90,err=928) iryr
        if(iryr.ne.iystr) goto 92
        backspace(77)

 93     write(nlog,*) ' '
        write(nlog,*) ' Virin;  Number of Reservoir to Rech = ', 
     1   numRre, ' Factor = ', rreFacto
c
c
c _________________________________________________________
c
c rrb 2008/10/28;
c		Step 3; Open Historic Diversion to Recharge (*.dre)
c
cx        rec48='Diversion to Recharge File (*.dre) - Monthly'
cx        write(nlog,100) rec48  
c              
        dreFacto=0.0
        rec256(1:2) = '-1'
        
        if(infile.eq.1) then
          ifn=78
          rec256 = fileName(ifn)
        endif
c
        if(rec256(1:2).ne.'-1') then
          rec48='Diversion to Recharge File (*.dre) - Monthly'
          write(nlog,100) rec48                
        
          call chekpor(iin, 78, 99, 0, 0, 999, iystr, 
     1               imstr, 0, 78, dreFacto, cyr1, maxfn,
     1               infile, idummy, nDre, fpath1, rec256)
c
c ---------------------------------------------------------
c rrb 2008/10/31
c		Find number of diversion to recharge stations and ID
c		        istop=1 OK if not found
c						itype=3 diversion	
          istop=1
          itype=3
          
          nno=0
          numDre=0            
          if(ioutRe.eq.1) write(nlog,294) 'Diversion to Recharge   '
          
c
c rrb 2008/11/05; Read maxsta values to allow extra data in the file          
cx        DO nd=1,numdiv
          DO nd=1,maxsta
            read(78,*,end=94,err=928) iryr, cistat
            if(iryr.eq.iystr) then
              numDre=numDre+1
c
              call stafind(nlog,istop, itype, numdiv, nx, 
     1          cistat, cdivid, cCallBy)     
              idvDre(numDre)=nx

c jhb 2016//06/26 have to handle nx=0 below
c              if(ioutRe.eq.1) write(nlog,'(12x,3i8,1x,2a12)')
c     1             numdre, nd, nx, cistat, cdivid(nx)
              if(ioutRe.eq.1) then
                if(nx.eq.0) then
                   write(nlog,'(12x,3i8,1x,a12)')
     1             numdre, nd, nx, cistat
                else
                   write(nlog,'(12x,3i8,1x,2a12)')
     1             numdre, nd, nx, cistat, cdivid(nx)
                endif
              endif
              if(nx.eq.0 .and. ioutRe.eq.0) then
                nno=nno+1
                if(nno.eq.1) write(nlog,290) rec48, 'Diversion   '                              
                write(nlog,292) nno, cistat
              endif              
            endif
c
c ---------------------------------------------------------
c			Exit if all stations have been read
c            
            if(iryr.gt.iystr) goto 94            
          end do
c
c ---------------------------------------------------------
c			Warn and exit if dimension exceeded
          goto 320          
        else
c          write(nlog,*) ' '
c          write(nlog,*) ' Virin;  No Diversion to Recharge (*.dre) ',
c     1                  'data provided'
          goto 97
        endif
        
c
c ---------------------------------------------------------
c		Reset file at beginning year        
 94     rewind(78)
        call skipn(78)
 96     read(78,*,end=90,err=928) iryr
        if(iryr.ne.iystr) goto 96
        backspace(78)
        
 97     write(nlog,*) ' '
        write(nlog,*) ' Virin;  Number of Diversions to Rech = ',
     1      numDre, ' Factor = ', dreFacto
c
c _________________________________________________________
c rrb 02/27/95; 
c		Step 3; Open file for Base flow estimates (*.rib) 
c                 at non-gauges.
c                 Insure the base flow option is set to total
c                 Since historic data is always total
c                 iopflo:  1=total; 2=gains
c
c rrb 11/22/95; keep this info; consider always total in mdainp
c       iopflo = 1
c
        rec48=' Virin; Base Flow File (*.rib) - NA'
        write(nlog,100) rec48
 100    format(/,72('_'),/,'  Virin; ', a48)
c
         iin2=iin
         filena='*.rsp'
c        write(nlog,*) '  Virin; infile = ', infile

         if(infile.eq.1) then
           ifn=26
           rec256 = fileName(ifn)
           filena = rec256
cr         write(nlog,*) ' Virin; fileName(ifn) = ', rec256
         else
           read (iin,'(a256)',end=926,err=928) filena
cr         write(nlog,*) filena
         endif
         
c
c ---------------------------------------------------------
c rrb 99/05/20
c
c		Allow no file to be provided
        if(filena(1:2).eq.'-1') then
          write(nlog,*) ' Virin; FYI no baseflow estimates (*.rib) ',
     1                  'data provided'
          nbaset=0
          goto 171
        endif
c         
c ---------------------------------------------------------

        iin2=56
        filena=filena
c
c rrb 2006/05/08; Correction
        call putpath(maxfn, filena, fpath1)
        open(56, file=filena, status='old',err=928)        
        write(nlog,'(5x, a256)') filena
c       write(6,'(5x, a256)') filena
        call skipn(56)
c         
c ---------------------------------------------------------
c 
c              Step 4; Read node data for base flow estimate 
  120   i = 0
  130   i = i+1
c
c rrb 2006/03/07; Allow comments in the file  
        call comment(56, nlog, iocode, nchk, 0)
        if(iocode.eq.2) goto 170
        if(iocode.eq.3) goto 928
        
        read(56,140,end=170,err=928)   cgagen(i), mbase(i),
     1            (cnodm(j,i), cupper(j,i),j=1,mbase(i))
     
        if(ioutNG.eq.1) write(nlog,142) i, cgagen(i), mbase(i),
     1            (cnodm(j,i), cupper(j,i),j=1,mbase(i))
  140   format(a12, 8x, i8, 26(f8.0,1x,a12))
  142   format(i5, 1x, a12, 8x, i8, 26(f8.0,1x,a12))
c
c rrb 2006/03/07; Allow comments in the file       
        call comment(56, nlog, iocode, nchk, 0)     
        if(iocode.eq.2) goto 170
        if(iocode.eq.3) goto 928
c
c              Allow a blank data file                   
        if(cgagen(i).eq.blank .or. cgagen(i).eq.czero) goto 170
                
        read(56,150,end=170,err=928)  pf, nbase(i),  
     1            (cnode(j,i), cgagex(j,i), j=1,nbase(i))
c
c rrb 01/03/13;  Find river node to print
        cistat=cgagen(i)
        do is=1,numsta
          if(cstaid(is).eq.cistat) then
            ibout(is)=2
c           write(nlog,*) ' Virin; cgagen ',  cstaid(is)
          endif
        end do                                

cr      write(nlog,152) i, cgagen(i), nbase(i), c, cgageb(i), 
cr   1            (cnode(j,i), cgagex(j,i), j=1,nbase(i))
  150   format(12x, f8.0, i8, 26(f8.0,1x,a12))
  152   format(i5,7x, f8.0, i8, 26(f8.0,1x,a12))
c
c ---------------------------------------------------------
c          
c              Check dimension
        if(nbase(i).gt.maxbas) then
          write(6,*) 
     1  ' Virin; Warning, dimension exceeded, max= ',maxbas
          goto 9999
        endif
c
c ---------------------------------------------------------
c                              
c              Allow one value per month
        if(pf.lt.0) then
          read(56,*,end=170,err=928) (coeff(j,i), j=1,12)
        else
          do 160 j=1,12
            coeff(j,i) = pf
  160     continue
        endif       
        goto 130
c 
  170   nbaset = i-1
  171   close(3)
  
        write(nlog,172) nbaset
  172    format('  Virin; ',
     1    'Number of Non Gage (Base) river stations (*.rib) = ',i5)
  
  
c
c _________________________________________________________
c
c		Step 5; Open historic streamflow data
c
c
        rec48=' Virin; Historic Streamflow (*.rih) - Monthly'        
        write(nlog,100) rec48

        if(infile.eq.1) then
          ifn=27
          rec256=fileName(ifn)
        endif

        call chekpor(iin, 3, 99, 0, 0, 999, iystr, 
     1             imstr, 0, 8, c, cyr1, maxfn,
     1             infile, idummy, nRihX, fpath1, rec256)

c _________________________________________________________
c
c		Step 6; Determine if a RiverGage (*.rig) file 
c		        is provided. Must be random input file
c rrb 2009/04/26; 
c			(infile=1)
        iriver=0
        rec256(1:2) = '-1'
        
        if(infile.eq.1) then
          ifn=68
          rec256=fileName(ifn)
          
cr        write(nlog,*) ' Virin; fileName(ifn) = ', rec256
        
          if(rec256(1:2).ne.'-1') then
            iriver=1        
            
            rec48= ' Virin; River Gage Structure (*.rig)'            
            write(nlog,100) rec48
            write(nlog,'(5x, a256)') rec256            
            
            iin2=68
            filena = rec256
            call putpath(maxfn, filena, fpath1)
            
            open(68, file=filena,status='old',err=928)
            call skipn(68)
          endif
        endif  
        
        if(iday.eq.1 .and. iriver.eq.0) then
          write(nlog,*) ' Virin; Problem Daily Baseflow',
     1      'Calculations require a RiverGage Structure file (*.rig)'
          goto 9999
        endif  
        
        if(ioutG.eq.1) 
     1   write(nlog,*) '  Virin; iday, iriver', iday, iriver
  
c
c _________________________________________________________
c
c		Step 7; Get historic stream gauge ID's, by going thru
c               one year of historic stream flow file
c               Only used if in monthly mode (iday=0) and
c               a RiverGage Structure file is not provided
c               (iriver.eq.0).
 
        if(iday.eq.0 .and. iriver.eq.0) then
c
c
c ---------------------------------------------------------
c		Read year 1        
          iin2=3
          filena='*.rih'
          read (3,270,end=370,err=928) iryr1, cistat
          backspace (3)
c
c ---------------------------------------------------------
c		Read historic stream (*.rih)
          do 230 np=1,maxrun+1
            read (3,270,end=370,err=928) iryr, cistat
            if(iryr1 .eq. iryr) then
c
c
c ---------------------------------------------------------
c               Insure it's in the network, find ID, etc.
              do 190 is=1,numsta
                if(cstaid(is).eq.cistat) go to 210
  190         continue
c
c ---------------------------------------------------------
c		Exit if not found  
              goto 390
c
c ---------------------------------------------------------
c		Set stream data
  210         irusta(np)=is
              istaru(is)=np
              crunid(np) = cistat
              ibout(is)=1
c             write(nlog,*) ' Virin; crunid is,', is, cstaid(is) 
              goto 230
            else                     
              do 220 i=1,np
                backspace(3)
  220         continue
              goto 240
            endif     
  230     continue   
c
c ---------------------------------------------------------
c		Exit if dimension exceeded
          Goto 930
c
c ---------------------------------------------------------
c
c		Set and print station count
  
  240     numrunG = np-1
          write(nlog,241) numrunG
  241     format(
     1    '  Virin; Number of historic stream gages (from *.rih) = ',
     1    i5)
c
c rrb 2006/05/09; Redefine number of stations to be gages only  
          numrun=numrunG
        endif
c
c ********************** Start new
c
c _________________________________________________________
c
c		Step 8; Read RiverGage Structure file (*.rig).
c		        Note this file is required if in daily mode.
c			to avoid mix up with river station data
c			read in datinp (e.g. crunid, numrun, etc.)

c rrb 99/05/20
        if(iriver.eq.1) then
c          
c ---------------------------------------------------------
c               a. Initilize daily ID pointer
c        
cr        do is=1,numrun
          do is=1,maxsta
            idays(is)=0
            crunidy(is)='NA'
          end do
c          
c ---------------------------------------------------------
c               b. Read RiverGage Structure (*.rig) 
c
          numrunG=0
          if(ioutg.eq.1) write(nlog,*) '  Virin; maxrun', maxrun
          DO 250 NP=1,maxrun+1
c          
c ---------------------------------------------------------
c 		c. Allow comments in the file       
            call comment(68, nlog, iocode, nchk, 0)     
            if(iocode.eq.2) goto 260
            if(iocode.eq.3) goto 928          
c
c ---------------------------------------------------------
c               d. Read *.rig RiverGage Structure data
            read(68,280,end=260,err=928)
     1        crunid(np), runnam1(np), cgoto, crunidy(np)
     
            if(ioutG.eq.3) write(nlog,281) 
     1        np,     
     1        crunid(np), runnam1(np), cgoto, crunidy(np)
c
c ---------------------------------------------------------
c		e. Done reading if a blank ID
            if(crunid(np).eq.blank) goto 260     
c
c ---------------------------------------------------------
c 		f. Adjust character string to left     
            crunid(np)=adjustl(crunid(np))
            crunidy(np)=adjustl(crunidy(np))
            
cxxxxxxxxxxxxxxx
c
c
c ---------------------------------------------------------
c               Insure it's in the network, find ID, etc.
            cistat=crunid(np)
            do is=1,numsta
              if(cstaid(is).eq.cistat) go to 212
            end do
c            
c		Exit if not found  
            goto 390
c
c ---------------------------------------------------------
c		Set stream data
  212       irusta(np)=is
            istaru(is)=np
            crunid(np) = cistat
            ibout(is)=1
c           write(nlog,*) ' Virin; crunid is,', is, cstaid(is) 

c
c ---------------------------------------------------------
c		h. Set daily ID        
c               idays  = 0 = set monthly to daily average,
c                        1 = daily gage monthly controls
c                        2 = refrence gage monthly controls
c                        3 = daily gage daily rules
c                       -1 = divide by # of days
            if(ioutG.eq.1)
     1       write(nlog,*) ' Virin; np, crunidy', np, crunidy(np)
            idays(np)=0
            if(crunidy(np).ne.blank)          idays(np)=2
            if(crunidy(np).eq.crunid(np))     idays(np)=1
            if(crunidy(np).eq.'-1          ') idays(np)=-1
            if(crunidy(np).eq.'0           ') idays(np)=0
            if(crunidy(np).eq.'3           ') then
              idays(np)=3
              crunidy(np)=crunid(np)
            endif
            
            if(crunidy(np).eq.'4           ') then
              idays(np)=4
              crunidy(np)=crunid(np)
            endif
            
            write(nlog,*) ' Virin; idays(np) 4', idays(np)
            
            if(ioutG.eq.3) then
               ioutG1=ioutG1+1
               write(nlog,'(a8,2i4,1x,a12,1x,a12,i5)') '  Virin;', 
     1            ioutG1, np, crunid(np), crunidy(np), idays(np)
            endif
c
  250     CONTINUE
c
c ---------------------------------------------------------
c		i. Exit if dimension exceeded
          Goto 930
c
c ---------------------------------------------------------
c		j. Set and print station count
  260     numrunG=NP-1
          close (68)
          write(nlog,261) numrunG
  261   format(
     1  '  Virin; Number of historic stream gages (from *.rig) = ', 
     1  i5)  
c
c rrb 2006/05/09; Redefine number of stations to be gages only  
          numrun=numrunG
        endif  
  
c **********************  End New
c
c
c _________________________________________________________
c
c		Step 9; Redefine the network stuff for gauges
        do 244 np=1,numrun
          irudnd(np)=0
          iscd=irusta(np)
          iss=idncod(iscd)
          if(iss.eq.0) go to 244 

          ndns=ndnnod(iss)
          do 242 nd=1,ndns
            if(istaru(iss).ne.0) go to 243
            iss = idncod(iss)
  242     continue
          go to 244

  243     irudnd(np)=istaru(iss)
c
  244   continue
c
c _________________________________________________________
c
c		Step 10; Open historic diversion data (*.ddh)
c               Note because the file number for historic
c               diversions (.ddh) is 4, the same the file number
c               used to read demand data in Mdainp the
c               historic diversions are read as demands in mdainp
c
        close(4)
cx        write(nlog,*) ' '   
cx      Write(nlog,*) ' Virin; Historic diversions (*.ddh) - Monthly'
        rec48='Historic diversions (*.ddh) - Monthly'
        write(nlog,100) rec48        
c
c ---------------------------------------------------------
c		a. Get file name
        if(infile.eq.1) then
          ifn=28
          rec256=fileName(ifn)
c         write(nlog,*) ' Virin; fileName(ifn) = ', rec256
        endif
c
c ---------------------------------------------------------
c		b. Open and check POR
        call chekpor(iin, 4, 99, 0, 0, 999, iystr, 
     1    imstr, 0, 9, c, cyr1, maxfn,
     1    infile, idummy, nDdhX, fpath1, rec256)
c
c _________________________________________________________
c
c		Step 11; Open Histiric pumping data as file 9 (type 11)   
c
        if(iwell.ne.0) then
          close(9)
cr        write(nlog,*) ' '           
cr        Write(nlog,*) ' Virin; Historic pumping (*.weh) - Monthly'
          rec48='Historic pumping (*.weh) - Monthly'
          write(nlog,100) rec48
c
c ---------------------------------------------------------
c		a. Get file name
          if(infile.eq.1) then
            ifn=29
            rec256=fileName(ifn)
c           write(nlog,*) ' Virin; fileName(ifn) = ', rec256
          endif
c
c ---------------------------------------------------------
c		b. Open and check POR
          call chekpor(iin, 9, 99, 0, 0, 999, iystr, 
     1               imstr, 0, 11, c, cyr1, maxfn,
     1               infile, idummy, nWehX, fpath1, rec256)
        endif
      endif
c
c =========================================================
c               Called 1x per year
      if(ityp.eq.1) then
c
c _________________________________________________________
c		Initilize Recharge Data 
      do nr=1,numres
        do im=1,13
          RrechM(im,nr)=0.0
        end do
      end do
      
      do nd=1,numdiv
        do im=1,13
          DrechM(im,nd)=0.0
          DuseM(im,nd)=0.0
        end do
      end do          
c
c _________________________________________________________
c
c		Step 12; Read EOM data (*.eom)
C
        IF(NUMRES.EQ.0.OR.NRSACT.EQ.0) GO TO 490
C
        iin2=55
        filena='*.eom'
        DO NR=1,NUMRES
C
          read (25,270,end=330,err=928) 
     1      iryr,cistat,(x(i),i=1,12)
          if(ioutR.eq.1)
     1      write(nlog,270) iryr,cistat,(x(i),i=1,12)

          if(iryr.ne. iyr) then
            write(nlog,1308) 'Reservoir EOM data (*.eom)',
     1        '(*.res)',iryr,iyr          
cr          write(6,*) ' Virin; year problem with eom data', iryr, iyr
            goto 9999
          endif
c
c
c ---------------------------------------------------------
c		Find station for this data
	        call stafind(nlog,1,3,numres,ix,cistat,cresid,cCallBy)
          if(ix.eq.0) then
            write(nlog,1310) cistat,'Reservoir EOM (*.eom)',
     1        '(*.res)',iryr,iyr
            goto 9999
          endif  
C
c ---------------------------------------------------------
c		Store last years value for daily mid point calcs
          if(iyr.eq.iystr) then
            resvol1(ix)=cursto(ix)
          else
            resvol1(ix)=resvol(12,ix)	           
          endif
C
c ---------------------------------------------------------
c		CONVERT EOM CONTENT DATA TO AC-FT IF NECESSARY
C
          DO IM=1,12
            RESVOL(IM,ix)=x(IM)*CFACTO
          end do
        end do
c
c _________________________________________________________
c
c		Step 13; Read Reservoir to Recharge data (*.rre)
C
        IF(NUMRES.EQ.0.OR.NRSACT.EQ.0) GO TO 490
        rec256 = fileName(77)
        if(rec256(1:2).eq.'-1') goto 490
C
        iin2=55
        filena='*.rre'
        DO nRre=1,numRre
          read(77,270,end=332,err=928) 
     1      iryr,cistat,(x(i),i=1,12)
          if(ioutRe2.eq.1)
     1      write(nlog,270) iryr,cistat,(x(i),i=1,12)
          if(iryr.ne.iyr) then
            write(nlog,1308) 'Reservoir Recharge data (*.rre)',
     1        '(*.res)',iryr,iyr          
            goto 9999
          endif
c
c ---------------------------------------------------------
c		CONVERT seepage data to cfs & store in RrechM
C
c         write(nlog,*) ' Virin; Res Rech, ', nRre, ix, idvRre(nRre)
          
          ix=idvrre(nRre)
          if(ix.gt.0) then
            DO IM=1,12
              RrechM(IM,ix)=RrechM(im,ix) + x(IM)/rreFacto/mthday(im)
            end do
          endif
          
        
        end do
        if(ioutRe2.eq.1) then
          do nr=1,numres
            write(nlog,*) ' Virin; Res to Rech',nr,RrechM(1,nr)*fac        
          end do
        endif          
c
c _________________________________________________________
c
c		Step 14; Read Diversion to Recharge data (*.dre)
C
 490    continue
c
c rrb 2008/12/09; Initilize diversion to use (Duse) = total diversion (diver)
        do nd=1,numdiv
          do im=1,12
            DuseM(im,nd)=diver(im,nd)
          end do
        end do
        
                  
        IF(NUMdiv.EQ.0 .or. numDre.eq.0) GO TO 500
        rec256 = fileName(78)
        if(rec256(1:2).eq.'-1') goto 500
C
        iin2=78
        filena='*.dre'
        DO nDre=1,numDre
C
          read(78,270,end=334,err=928) 
     1      iryr,cistat,(x(i),i=1,12)
          if(ioutRe.eq.1) 
     1      write(nlog,270) iryr,cistat,(x(i),i=1,12)

          if(iryr.ne.iyr) then
            write(nlog,1308) 'Drain Recharge data (*.dre)',
     1        '(*.res)',iryr,iyr          
            goto 9999
          endif
c
c ---------------------------------------------------------
c		CONVERT Diversion to Recharge data to cfs
C
          ix=idvDre(nDre)
          if(ix.gt.0) then
            DO IM=1,12
              DrechM(IM,ix)=DrechM(im,ix) + x(IM)/dreFacto/mthday(im)
c
c rrb 2008/12/09; Add diversion to use              
              DuseM(im,ix)=amax1(Diver(im,ix)- DrechM(im,ix), 0.0)
            end do
          endif
        end do
c
c		Check
        if(ioutRe.eq.1) then
          imx=2
          fac=mthday(imx)*factor        
          do nd=1,numdiv
            write(nlog,*) ' Virin; Div to Rech for FEB', iryr, imx,nd,
     1        cdivid(nd), 
     1        Diver(imx,nd)*fac,  DrechM(imx,nd)*fac, DuseM(imx,nd)*fac        
          end do
        endif  

        
c
c ---------------------------------------------------------
c		End of ityp=1 (called 1x/year if statement
      endif
      
c
c _________________________________________________________
c               Step 14; Return
  500 return
c
c _________________________________________________________
c               Formats
  270 FORMAT(i4, 1x, a12, 12F8.0)

  280 FORMAT(a12, a24, a12, 1x, a12)
  281 FORMAT(i5,1x, a12, a24, a12, 1x, a12)
  283 FORMAT(a12, a24, a12, 1x, a12)
  284 FORMAT(i5, 1x, a12, a24, a12, 1x, a12)
  
  290 format(/,72('_'),/
     1'  Virin;    Warning the following stations in the ',
     1'            ',a48,/
     1'            were not found in the ',a12, ' station file.',/
     1'            Data is being ignored.',//
     1'    # ID',/
     1' ____ ____________')
     
  292 format(i5,1x,a12)
  
  294 format(/,72('_'),/ '  Virin;    ',a24,/
     1  '  Virin;    ',
     1  '       #       n      nx ID          cistat')
  
c
c _____________________________________________________________
c               Error 
  320 write(nlog,322) maxsta
  322 format('  Virin; Dimension exceeded = ', i5)    
      goto 9999

  330 WRITE(nlog,331)
      write(6,331)
  331 FORMAT(' NOT ENOUGH DATA IN THE EOM CONTENT DATA FILE')
      goto 9999
      
  332 WRITE(nlog,333)
      write(6,333)
  333 FORMAT(
     1 ' NOT ENOUGH DATA IN THE Reservoir to Recharge file (*.rre)')
      goto 9999
      
  334 WRITE(nlog,335)
      write(6,335)
  335 FORMAT(
     1  ' NOT ENOUGHT DATA IN THE Diversion to Recharge file (*.dre)')
      goto 9999

  350 write(nlog,360) cistat,iyr,cresid(nr)
      write(6,360) cistat,iyr,cresid(nr)
  360 FORMAT(
     1 ' STATION ',a12,' OF EOM. FILE IN ',I5,' IS DIFFERENT than'/,
     1 ' STATION ',a12,' IN RESERVOIR STATION FILE (*.res)')
      goto 9999

  370 write(6,380)
      write(nlog,380)
  380 format(' not enough data in the historic stream flow file') 
      goto 9999

  390 write(nlog,392) cistat
      write(6,392) cistat
  392 format(
     1  'Virin; station ',A12,' of historic stream file (*.rih)',/
     1  '       or RiverGage Station file (*.rig)',/
     1  '       not found in the network file')
      goto 9999  
      
  394 write(nlog,396) cistat
      write(6,396) cistat
      
  396 format(
     1  'Virin; station ',A12,' of river gage file (*.rig)',/
     1  '       not found in the river station file (*.ris)')
      goto 9999  
c
c rrb 97/11/02; Error Handling
  926 write(nlog,927) iin2, filena
  927 format(' Virin.f; End of file # ', i4, ' encountered',/,
     1       '   File name: ', a256)
      goto 9999
c
  928 write(nlog,929) iin2, filena
  929 format(' Virin.f; Problem reading file # ', i4,/,
     1       '   File name: ', a256,/
     1       '   Problem record (next line):')

      backspace(iin2)
      read(iin2, '(a256)',end=926,err=926) recin
      write(nlog,'(a256)') recin
      goto 9999

 930  write(nlog,931) MAXRUN     
 931  FORMAT(/,72('_'),/
     1 '  Virin; Problem. ',
     1  ' Too many stream gages,   MAXIMUM = ',I5)
      

 9999 write(6,*) '  Stopped in Virin, see the log file (*.log)'
      write(nlog,*)'  Stopped in Virin'
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)
      
 1308 FORMAT(
     1 '  Virin;  Problem Reading File = ', a32, /,
     1 '          The file is inconsistent with the number of',/
     1 '          stations in the Station file = ', a16,/
     1 '          Year read = ', i5,' Year expected = ', i5)
 1310 FORMAT(
     1 '  Virin;  Problem ID = ',a12,' of File = ', a32, /,
     1 '          is not found in the station file = ', a8,/
     1 '          Year read = ', i5,' Year expected = ', i5)


      stop 
      END   
