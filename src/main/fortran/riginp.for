c
      SUBROUTINE RIGINP(IIN, maxres1, maxdvr1)
c
c
c _________________________________________________________
c	Program Description
c
c       Riginp; It reads in water right data
c
c _________________________________________________________
c       Update History
c
c 2008/09/22;   Store on/off switch read (e.g. idvrsw1 not idvrsw) in 
c               order to allow more than one operating rule
c	        to refrence a water right

c 2003/08/18;   Revise to allow random file read
c 2003/10/24;   Revise to allow iresco(2,k) = 0 to allow
c               a reservoir right to be distributed porportional
c               to the ownership of all accounts in that reservoir.
c               Therefor:
c               if iresco(2,k) = +n only account n gets filled.
c               if iresco(2,k) = -n, accounts 1-n get filled
c                 porportional to the available storage in each account
c               if iresco(2,k) = 0, all accounts in this reservoir get
c                 filled porportional to the ownership in each account
c
c _________________________________________________________
c       Documentation
c
c               iin    = response (*.rsp) file #
c               maxwrx = max # of water rights for any structure
c                        type + 1 
c
c _________________________________________________________
c	Dimensions
c

      include 'common.inc'
      real*8 rtem
c
c rrb 2009/06/09; Correction      
cx    dimension iwr(maxwrx), jwr(maxwrx), kwr(maxwrx)
      dimension iwr(maxres1), jwr(maxdvr1), kwr(maxdvr1)
      character*12 cifrri,cirsid,cidvri,
     1             blank, czero, rec12
      character rec32*32
c
c _________________________________________________________
c
                                                                      
      write(6,*) ' Subroutine Riginp'
c
c		ioutR	= 1 print reservoir read
c		ioutR   = 2 print reservior right distribution data
c               ioutD = 1 print *.ddr read      
      ioutR=0
      ioutD=0
      ioutW=0
      ioutI=0
      
      iprintd=0
      blank = '            '                                            
      czero = '0           '                                            
      istop=0
c
c               Step 1; Read ISF rights (*.ifr)
c _________________________________________________________
c
      write(nlog,101) iin,infile, nchk
      write(6,101) iin, infile, nchk
 101  format(/,72('_'),/,
     1 '  Riginp; Instream Flow Right File (*.ifr) ', 20i5)
      iin2=iin
      nchk=98


      if(infile.eq.1) then
        ifn=8
        rec256=fileName(ifn)
        filena=rec256(1:72)
c       write(nlog,*) ' Riginp; fileName = ', fileName(ifn)
c       write(nlog,*) ' Riginp; filena   = ', filena
      else
        filena=filenc
        READ(IIN,450,err=926,end=928) FILENA
      endif
c
c		Allow no data to be read
      if(filena(1:2).eq.'-1') then
        write(nlog,*) ' Ringip; FYI no instream flow rights provided'
        numfrr=0
        goto 131
      endif

      call putpath(maxfn, filena, fpath1)
      open(2, file=filena,status='old',err=430)

      iin2=2
      write(nlog,460) filena                                              
      call skipn(2)                                                     
C                                                                       
      MAXFRS=MAXFRR+1
C                                                                       
      DO 120 K=1,MAXFRS     
c
c _________________________________________________________      
c rrb 2004/12/14; Allow a # in column 1 for comments
c		Note iocode 1 = Data, 2 = EOF, 3 = Error                    
        call comment(2, nlog, iocode, nchk, 0)
        if(iocode.eq.2) goto 130
        if(iocode.eq.3) goto 928
                                                  
        read(2,470,end=130,err=928)
     1    cifrri, namei(k),  cgoto, rtem,  dcrifr(k), iifrsw(k)
        iifrsw1(k)=iifrsw(k)
     
        if(ioutI.eq.1) write(nlog,472) k,
     1    cifrri, namei(k),  cgoto, rtem,  dcrifr(k), iifrsw(k)
c
c rrb 2006/03/20; Adjust character string to left     
        cifrri=adjustl(cifrri)  
        cgoto=adjustl(cgoto)   

        if(cifrri.eq.blank) goto 130
c
c rrb 01/09/01; Store ID for call reporting
        cisfwr(k)=cifrri

        if(dcrifr(k).lt.0.001) then 
          rec32='Instream Flow Rights'
          if(iprintd.eq.0) write(nlog,1281) rec32
          if(iprintd.eq.0) write(nchk,491)
          iprintd=iprintd+1     
          write(nchk,495) iprintd, cifrri, dcrifr(k), ' Isf'
          dcrifr(k)  = 0.00     
        endif   
C                                                                       
        DO 100 NF=1,NUMIFR      
        if(cifrid(nf).eq.cgoto) goto 110
  100   CONTINUE
C                                                                       
C------  IF STATION NOT FOUND, WRITE ERROR MESSAGE AND STOP             
C
        write(nlog,500) cIFRRI
        goto 9999
C                                                                       
  110   IIFRCO(K)=NF  
C                                                                       
        rfrnk(k)=rtem 
C                                                                       
        IF(IFRRSW(NF).EQ.0) then
          IIFRSW(K)=0 
          iifrsw1(k)=0
        endif  
c
c              End Instream Flow Diversion Right Loop
  120 CONTINUE                                                          
      write(nlog,510) MAXFRR                                              
      goto 9999                                                         
C                                                                       
  130 NUMFRR=K-1
      write(nlog,630) 'InstreamFlow', numfrr                                                        
  
  131 close(2)                                                          
c      
c _________________________________________________________
c               Step 2; Read Reservoir rights (*.rer)
c
      DO 140 NR=1,NUMRES                                                
  140   IWR(NR)=0
C                                                                       
      write(nlog,102)
      write(6,102)
      
  102   format(/,72('_'),/,
     1 '  Riginp; Reservoir Right File (*.rer) ')
      iin2=iin

      if(infile.eq.1) then
        ifn=12
        rec256=fileName(ifn)
        filena=rec256(1:72)
        if(ioutR.eq.1) then
          write(nlog,*) ' Riginp; fileName = ', fileName(ifn)
          write(nlog,*) ' Riginp; filena   = ', filena
        endif
      else

        filena=filenc
        READ (IIN,450,end=926,err=928) FILENA
      endif
c
c
      if(filena(1:2).eq.'-1') then
        write(nlog,*) ' Ringip; FYI no reservoir right provided'
        numres=0
        goto 280
      endif

      call putpath(maxfn, filena, fpath1)
      open(2, file=filena,status='old',err=430)
      iin2=2
      write(nlog,460) filena                                              
      call skipn(2)                                                     
C                                                                       
      IF(NUMRES.EQ.0) GOTO 280                                         
      NUMRSR=0                                                          
C
      DO 150 NO=1,NUMOWN
  150 NRIGOW(NO+1)=0
C
      DO 160 NR=1,NUMRES
  160 NRIGRS(NR+1)=0
C
      MAXRSS=MAXRSR+1
C
      iprintr=0
      iprintr2=0
      DO 210 K=1,MAXRSS
c
c _________________________________________________________      
c rrb 2004/12/14; Allow a # in column 1 for comments
c		Note iocode 1 = Data, 2 = End, 3 = Error                    
        call comment(2, nlog, iocode, nchk, 0)
        if(iocode.eq.2) goto 220
        if(iocode.eq.3) goto 928
      
        read (2,470,end=220,err=928)
     1    cirsid,namer(k),cgoto,
     1    rtem, dcrres(k),irsrsw(k),iresco(2,k),
     1    ITYRSR(K),n2fill(k),copid(k)
     
        irsrsw1(k)=irsrsw(k)
        
        
        if(ioutR.eq.1) write(99,*) '  Riginp; ',k, 
     1      cirsid,namer(k),cgoto,
     1      rtem, dcrres(k),irsrsw(k),iresco(2,k),
     1     ITYRSR(K),n2fill(k),copid(k)

        if(ioutR.eq.1) write(99,472) '  Riginp; ',k, 
     1      cirsid,namer(k),cgoto,
     1      rtem, dcrres(k),irsrsw(k),iresco(2,k),
     1     ITYRSR(K),n2fill(k),copid(k)
c
c rrb 2006/06/19; Set iResOpr equal to irsrsw
c		  It stays on even if the reservoir right
c		  is part of an operating rule which turns off the
c		  original right. Used in Bomsec to set 1 fill limit
       iResOpr(k)=irsrsw(k)     
c
c ---------------------------------------------------------
c rrb 2006/03/20; Adjust character string to left     
        cirsid=adjustl(cirsid)  
        cgoto=adjustl(cgoto)
        copid(k)=adjustl(copid(k))
c
c ---------------------------------------------------------
c		Exit if a blank ID                                                                        
        if(cirsid.eq.blank) goto 220
        creswr(k) = cirsid
c
c ---------------------------------------------------------
c rrb 04/01/96; Check available reservoir types
        if(ityrsr(k).gt.1 .or. ityrsr(k).lt.-1 .or. 
     1    ityrsr(k).eq.0) then
          write(nlog,520) cirsid, ityrsr(k)
          goto 9999
        endif
c
c		Print warning if < 0    
c
c rrb 2010/09/11; Correction when commented out all reservoirs are set to 0                                                                   
cxxx        if(dcrres(k).lt.0.001) then 
        if(dcrres(k).lt.0.001) then
          rec32='Reservoir Rights'
          if(iprintd.eq.0) write(nlog,1281) rec32
          if(iprintd.eq.0) write(nchk,491)
          iprintd=iprintd+1
          write(nchk,495) iprintd, cirsid, dcrres(k), ' Res'
          dcrres(k)  = 0.00 
cxxx        endif
        endif
C                       
c ---------------------------------------------------------
c		Locate on network                                                
        DO 170 NR=1,NUMRES
          if(cresid(nr).eq.cgoto ) goto 180
  170   CONTINUE
C                                                                       
C------  IF RESERVOIR NOT FOUND, WRITE ERROR MESSAGE AND STOP           
        write(nlog,530) cgoto
        do nr=1,numres
          write(nlog,'(i5, 1x, a12)') nr, cresid(nr)
        end do
        goto 9999
C                                                                       
  180   IRESCO(1,K)=NR
c
c               Set default reservoir right data
        nrown(k)=1
        irestyp(k) = 1
c
c ---------------------------------------------------------
c rrb 2006/05/24; Check if the number of accounts served by this
c               reservoir right is wrong
        ia1=nowner(nr+1)-nowner(nr)
        ia2=abs(iresco(2,k))
        if(ia1.lt.ia2) then
          write(nlog,485) cresid(nr), ia1, cirsid, ia2
          goto 9999
        endif          
c
c ---------------------------------------------------------
c rrb 20066/05/24; Check an OOP right has a operating right ID 
        rec12=copid(k)
        if(ityrsr(k).eq.-1 .and. rec12(1:1).eq.' ') then
          write(nlog,486) cresid(nr), cirsid, copid(k)
          goto 9999
        endif
         
c ---------------------------------------------------------
c rrb 01/05/95; Multiple owners for 1 reservoir right
c              if iresco(2,k) < 0, then distribute porportional
c              to available space.
c              nrown(k) = # of owners beginning with owner 1
        ichk1=0
        ione=iresco(2,k)
        if(iresco(2,k).lt.0) then
c
          ione=99
          nrown(k) = -1*iresco(2,k)
c         if(ichk1.eq.1) write(nlog,*) '  Riginp; nrown(k) = ', nrown(k)
          iresco(2,k) = 1
          irestyp(k) = -1
        endif
c
c ---------------------------------------------------------
c rrb 2003/10/24; Multiple owners for 1 reservoir right
c              if iresco(2,k) = 0, then distribute porportional
c              to ownership 
c              nrown(k) = # of owners beginning with owner 1
        ichk1=0
        if(iresco(2,k).eq.0) then
c         ichk1=1
          ione=99
          nrown(k) = nowner(nr+1) - nowner(nr)
          if(ichk1.eq.1) write(nlog,*) '  Riginp; nrown(k) = ', nrown(k)
          iresco(2,k) = 1
          irestyp(k) = 0
        endif
c
c ---------------------------------------------------------
c rrb 01/23/97; Warning if:
c               a right is on (irsrsw(k) = 1) &
c               a right is a first fill (n2fill(k) =1) &
c               a right goes to 1 account (ichk1.eq.0).gt.0) &
c               it is not an OOP right (ityrsr.eq.1) &
c               a resevoir has multiple accounts (nowner(nr+1) - (nr)),
c                          Print warning
        if(ioutR.eq.2) write(nchk,*) ' Riginp; ',
     1    irsrsw(k), n2fill(k), ione, ityrsr(k),nowner(nr+1)-nowner(nr)
        if(irsrsw(k).eq.1  .and. n2fill(k).eq.1 .and. ione.ne.99 .and.
     1    ityrsr(k).eq.1 .and. nowner(nr+1)-nowner(nr).gt.1) then
          rec32='Reservoir Rights'
          if(iprintr.eq.0) write(nlog,1281) rec32
          if(iprintr.eq.0) write(nchk,483)
          if(iprintr.ne.nr) then
            iprintr=nr
            iprintr2=iprintr2+1
            write(nchk,484) iprintr2, cresid(nr), cirsid, iresco(2,k),
     1                      nowner(nr+1)-nowner(nr)
          endif
        endif
c                                                          
c ---------------------------------------------------------             
c rrb 01/10/95;
        IF(IRESSW(NR).EQ.0) then
          IRSRSW(K)=0
          irsrsw1(k)=0
        endif
C                                                                       
        rrsnk(k)=rtem
C                                                                       
C------  CHECK OWNER'S CODE SPECIFIED (IRESCO(2,K))                     
        NO=NOWNER(NR+1)-NOWNER(NR)
        IF(IRESCO(2,K).LE.NO) GOTO 190
c                                                                       
c rrb 10/7/94 Comment                                                   
        write(nlog,540) IRESCO(2,K),NO
        do ix=1,numres
          write(nlog,*) ix, cresid(ix), nowner(ix+1)
        end do  
        goto 9999
C                                                                       
C                                 
  190   continue                                   
  200   NO=NOWNER(NR)+IRESCO(2,K)-1     
        NRIGOW(NO+1)=NRIGOW(NO+1)+1
        NRIGRS(NR+1)=NRIGRS(NR+1)+1
C 
        IF(IRSRSW(K).NE.0 .AND. IWR(NR).EQ.0) IWR(NR)=K  
        JWR(K) =NRIGOW(NO+1)
        KWR(K) =NRIGRS(NR+1)
c
c ---------------------------------------------------------
c              End Reservoir Right Loop
  210 continue                                                      
      write(nlog,550) MAXRSR                                              
      goto 9999                                                         
C                                                                       
  220 NUMRSR=K-1
      write(nlog,630) 'Reservoir   ', numrsr                                                        
C                                                                       
      NRIGRS(1)=1                                                       
      DO 230 NR=1,NUMRES                                                
  230 NRIGRS(NR+1)=NRIGRS(NR+1)+NRIGRS(NR)                              
C                                                                       
      NRIGOW(1)=1                                                       
      DO 240 NO=1,NUMOWN                                                
  240 NRIGOW(NO+1)=NRIGOW(NO+1)+NRIGOW(NO)                              
C                                                                       
      DO 250 IW=1,NUMRSR                                                
  250 LAST(IW)=0
C
      DO 260 NR=1,NUMRES
        K=IWR(NR)
c
c rrb 7/29/94 Execution Problem
      if(k.eq.0) goto 260
        LAST(K)=1
  260 CONTINUE
C
      DO 270 K=1,NUMRSR
        NR=IRESCO(1,K)
        NO=NOWNER(NR)+IRESCO(2,K)-1
        IW=NRIGOW(NO)+JWR(K)-1
        IRIGOW(IW)=K
C                                                                       
        IW=NRIGRS(NR)+KWR(K)-1
        IRIGRS(IW)=K
  270 CONTINUE                                                          
C                                                                       
  280 close(2)                                                          
c  
c _________________________________________________________
c
c               Step 3; Read direct diversion rights (*.ddr)
c
      write(nlog,103)
      write(6,103)
 103    format(/,72('_'),/,
     1 '  Riginp; Direct Diversion Right File (*.ddr) ')
      iin2=iin

      if(infile.eq.1) then
        ifn=6
        rec256=fileName(ifn)
        filena=rec256(1:72)
c       write(nlog,*) ' Riginp; fileName = ', fileName(ifn)
c       write(nlog,*) ' Riginp; filena   = ', filena
      else

        filena=filenc
        READ (IIN,450,end=926,err=928) FILENA
      endif
c
c rrb 99/05/20
c
c		Allow no data to be read
      if(filena(1:2).eq.'-1') then
        write(nlog,*) ' Ringip; FYI no diversion right provided'
        numdvr=0
        goto 361
      endif

      call putpath(maxfn, filena, fpath1)
      open(3, file=filena,status='old',err=430)
      iin2=3
      write(nlog,460) filena                                              
      call skipn(3)                                                     
c
c rrb 98/12/17; not used
c     DO 290 NU=1,NUMUSE                                                
c 290 NRIGUS(NU+1)=0                                                    
C                                                                       
      MAXDVS=MAXDVR+1                                                   
      iprintd=0
C                                                                       
      DO 340 K=1,MAXDVS     
      
c
c _________________________________________________________      
c rrb 2004/12/14; Allow a # in column 1 for comments
c		Note iocode 1 = Data, 2 = End, 3 = Error                    
        call comment(3, nlog, iocode, nchk, 0)
        if(iocode.eq.2) goto 350
        if(iocode.eq.3) goto 928
                   
        READ(3,470,END=350,err=928)
     1    cidvri,named(k),cgoto, rtem, dcrdiv(k),idvrsw(k)
     
        idvrsw1(k)=idvrsw(k)
          
        if(ioutD.eq.1) write(nlog,472) k, 
     1    cidvri,named(k),cgoto, rtem, dcrdiv(k),idvrsw(k)
c
c rrb 2006/03/20; Adjust character string to left     
        cidvri=adjustl(cidvri)  
        cgoto=adjustl(cgoto)    
c                                                                       
        if(cidvri.eq.blank .or. cidvri.eq.czero) goto 350
c                                                                       
c              Set data not currently used                              
        idivco(2,k) = 1
        if(dcrdiv(k).lt.0.001) then
c
c rrb; Reduce I/O
          rec32= 'Diversion Rights'
          if(iprintd.eq.0) write(nlog,1281) rec32
          if(iprintd.eq.0) write(nchk,491)
          iprintd=iprintd+1
          write(nchk,495) iprintd, cidvri, dcrdiv(k), ' Div'
          dcrdiv(k)  = 0.00
        endif
C                                                                       
c grb - store right id into array crigid                                
        crigid(k)=cidvri
                                                                        
        DO 300 ND=1,NUMDIV
          if(cdivid(nd).eq.cgoto) goto 310
  300   CONTINUE
        write(nlog,560) cGOTO
        goto 9999
C
  310   IDIVCO(1,K)=ND
C
        rdvnk(k)=rtem
C
C------  CHECK USER'S CODE
        IF(IDVRSW(K).EQ.0) GOTO 340 
C                                                                       
        IF(IDIVSW(ND).NE.0) Goto 320
        IDVRSW(K)=0
        Goto 340                                                        
        
  320   NU=NDUSER(ND+1)-NDUSER(ND)
        IF(NU.GE.IDIVCO(2,K)) Goto 330
C                                                                       
        write(nlog,570) IDIVCO(2,K),K,NU,ND
        goto 9999
C
  330   NU=NDUSER(ND)+IDIVCO(2,K)-1
        IRIGUS(K)=NU
c
c rrb 98/12/17; Not used
c       NRIGUS(NU+1)=NRIGUS(NU+1)+1
c                                 
c              End Direct Diversion Right Loop                        
  340 CONTINUE                                                          
      write(nlog,580) MAXDVR                                              
      goto 9999                                                         
C                                                                       
  350 NUMDVR=K-1                                                        
      write(nlog,630) 'Diversion   ', numdvr
  
c
c rrb; 98/12/17; Not used
c     NRIGUS(1)=1                                                       
c     DO 360 NU=1,NUMUSE                                                
c 360 NRIGUS(NU+1)=NRIGUS(NU+1)+NRIGUS(NU)
c
  361 close(3)                                                          
c      
c _________________________________________________________
c
c               Step 4; Read other rights (not active)
c
      numpwr = 0
      NWRTOT=NUMFRR+NUMRSR+NUMDVR+NUMPWR
c _________________________________________________________
c
c               Step 5: Read Well Rights (*.wer)
c
      if(iwell.eq.0) goto 901
      write(nlog,104)
      write(6,103)
 104  format(/,72('_'),/,
     1 '  Riginp; Well Right File (*.wer) ')
      iin2=iin


      if(infile.eq.1) then
        ifn=10
        rec256=fileName(ifn)
        filena=rec256(1:72)
c       write(nlog,*) ' Riginp; fileName = ', fileName(ifn)
c       write(nlog,*) ' Riginp; filena   = ', filena
      else

        filena=filenc
        READ (IIN,450,end=926,err=928) FILENA
      endif
      if(iwell.eq.-1) goto 901
c
c		Allow no data to be read
      if(filena(1:2).eq.'-1') then
        write(nlog,*) ' Ringip; FYI no reservoir right provided'
        numdvrw=0
        goto 901
      endif
      
c
c rrb 99/05/20

      call putpath(maxfn, filena, fpath1)
      open(3, file=filena,status='old',err=430)
      iin2=3
      write(nlog,460) filena                                              
      call skipn(3)                                                     
c                                                                       
      iprintw=0
      DO 342 K=1,maxdvrw+1                                                
c      
c rrb 00/02/24; Correction
c       READ(3,470,END=350,err=928)
c
c _________________________________________________________      
c rrb 2004/12/14; Allow a # in column 1 for comments
c		Note iocode 1 = Data, 2 = End, 3 = Error                    
        call comment(3, nlog, iocode, nchk, 0)
        if(iocode.eq.2) goto 352
        if(iocode.eq.3) goto 928

        READ(3,470,END=352,err=928)
     1    cidvri,namedw(k),cgoto, rtem, dcrdivw(k),idvrsww(k)
     
        idvrsww1(k)=idvrsww(k)
     
        if(ioutW.eq.1) write(nlog,472) k, 
     1    cidvri,namedw(k),cgoto, rtem, dcrdivw(k),idvrsww(k)
c   
c
c rrb 2006/03/20; Adjust character string to left     
        cidvri=adjustl(cidvri)  
        cgoto=adjustl(cgoto)
                                                                            
        if(cidvri.eq.blank .or. cidvri.eq.czero) goto 352
c                                                                       
        idivcow(2,k) = 1
        if(dcrdivw(k).lt.0.001) then
          rec32= 'Well Rights'
          if(iprintw.eq.0) write(nlog,1281) rec32
          if(iprintw.eq.0) write(nchk,491)
          iprintw=iprintw+1
          write(nchk,495) iprintw, cidvri, dcrdivw(k), ' Wel'
          dcrdivw(k)  = 0.00
        endif

        crigidw(k)=cidvri
c
c               Tie a well right to a well structure
        ifound=0
        DO ND=1,numdivw
          if(cdividw(nd).eq.cgoto) then
            ifound=nd
          endif  
          if(ioutW.eq.1 .and. cgoto(1:8).eq.'6400511M') then
            write(nlog,'(i5,1x,a12,1x,a12,i5)') 
     1      nd, cdividw(nd), cgoto, ifound 
          endif
        end do
c
        if(ifound.eq.0) then
          write(nlog,562) cGOTO
c
c rrb get all stations with problems
c         goto 9999
          istop=1
          goto 342
        endif
c
        nd=ifound
        idivcow(1,K)=nd
c       write(nlog,*) '  Riginp; k, nd', k, nd
c
c rrb 00/04/07; Primary option
        if(iprimary.gt.0) rtem=rtem-primary(nd)

        rdvnkw(k)=rtem
C
c               Check if right (idvrsww(k)) is turned off
        if(idvrsww(k).eq.0) goto 342 
c
c               Turn off right if well structure (idivsww(nd)) is off
        if(nd.gt.0) then
          if(idivsww(nd).eq.0) then
            idvrsww(k)=0
          endif
c
c               Turn off right if diversion structure is off
          nx=idivcow2(nd)
          if(nx.gt.0) then
            if(idivsw(nx).eq.0) then
              idvrsww(k)=0
            endif
          endif
          goto 342
        endif
c
c               Do not include user info for wells
c        NU=NDUSER(ND+1)-NDUSER(ND)
c        IF(NU.GE.iwelco(2,K)) Goto 330
c
c  330   NU=NDUSER(ND)+iwelco(2,K)-1
        IRIGUSw(K)=nd
c                                 
c              End Well Right Loop                        
  342 CONTINUE                                                          
      write(nlog,582) maxdvrw                                              
      goto 9999                                                         
c                                                                       
  352 numdvrw=K-1 
      write(nlog,630) 'Well        ', numdvrw                                                        
  
      if(istop.eq.1) goto 9999
c                                                                       
 901  close(3)

      NWRTOT=nwrtot+numdvrw
c
c _________________________________________________________
c
c               Step 6; Read Operational Rights (.opr)
c
      if(ichk.eq.4) write(nlog,*) ' Riginp; Calling oprinp'
      CALL OPRINP(IIN)
      if(ichk.eq.4) write(nlog,*) ' Riginp; Out of Oprinp'
      
      NTORIG=NWRTOT+NUMOPR
c
c _________________________________________________________
c
c               Step 7; Check all Direct Diversions have a right
c
      iprints=0
      do 380 nd=1,numdiv
        do k=1,numdvr
c
c rrb 00/01/28; Additional check
c         if(idivco(1,k).eq.nd .and. idvrsw(k).ne.0) goto 380
          if(idivco(1,k).eq.nd .and. idvrsw(k).ne.0 .and.
     1       rdvnk(k).lt. 99998.9) goto 380
        end do
        
      rec32='Diversion Rights'
      if(iprints.eq.0) write(nlog,1281) rec32
      if(iprints.eq.0) write(nchk,496) 99998.9
      iprints=iprints+1     
      write(nchk,497) iprints, cdivid(nd), divnam1(nd),' Div'
  380 continue                                                          
c
c _________________________________________________________
c
c               Step 8; Check all Instream Flows have a right
c
      do 400 nf=1,numifr                                                
        do k=1,numfrr                                               
          if(iifrco(k).eq.nf .and. iifrsw(k).ne.0) goto 400             
        end do
      
      if(iprints.eq.0) write(nchk,496) 99998.9
      iprints=iprints+1     
      write(nchk,497) iprints,cifrid(nf),xfrnam1(nf),' Isf'                  
  400 continue                                                          
c
c
c _________________________________________________________
c
c               Step 9; Check all Reservoirs have a right
c
      do 420 nr=1,numres
        do 410 k=1,numrsr
          if(iresco(1,k).eq.nr .and. irsrsw(k).ne.0) goto 420
  410   continue

      if(iprints.eq.0) write(nchk,496) 99998.9
      iprints=iprints+1     
      write(nchk,497) iprints,cresid(nr),resnam1(nr),' Res'
  420 continue
c
c _________________________________________________________       
c
c               Step 10; Print general information to log file
c
c rrb 98/11/18; Wells
c     write(nlog,600) numdvr,  numrsr, numfrr, numopr, ntorig
      write(nlog,600) numdvr,  numrsr, numfrr, numopr,
     1                numdvrw, ntorig
c
c rrb 99/08/27
      if (ntorig.gt. maxnwr) then
        write(nlog,602) ntorig, maxnwr
        goto 430
      endif
c                                                                       
c _________________________________________________________
c
c               Step 11; Return
      RETURN            
c
c _________________________________________________________
c
c               Error Handling
  926 write(nlog,927) iin2, filena
  927 format(/,' Riginp; Problem. End of file # ', i4, ' encountered',
     1       '   File name: ', /,a256)
      goto 9999
c
  928 write(nlog,929) iin2, filena, k, cidvri
  929 format(/
     1 ' Riginp.f; Problem reading file # ', i4,
     1 '           File name:   ' a256,/
     1 '           Data record: ',i5,/
     1 '           Last ID read: ', a12)
c    1 '   Problem is the record following record:')

      backspace(iin2)
      backspace(iin2)
      read(iin2, '(a256)',end=926,err=926) recin
      write(nlog,'(a256)') recin
      goto 9999


  430 write(nlog,610) filena
      goto 9999
      
 9999 write(6,620)
      write(nlog,*) '  Stopped in Riginp'
      write(6,*) 'Stop 1' 
      call flush(6)
      call exit(1)

      stop 
c
c
c _________________________________________________________
c
c            Formats
  450 FORMAT(A256)                                                       
  460 format(4x, a256)                                                   
  470 format(a12,a24,a12,f16.0,f8.0,4i8,a12)
  472 format(i5, 1x, a12,a24,a12,f16.0,f8.0,4i8,a12)
c 480 format(a12,a24,a12,f16.0,f8.0,i8, 3(1x,a12,i8), 20i8)             
  483 format(/,
     1  72('_'),//       
     1    '  Riginp; Warning the following reservoir',
     1    ' has at least one right tied to an account with multiple'
     1    ' accounts.',/  
     1    '    When the one fill rule is implemented on the admin ', 
     1        'date, water is allocated from the reservoir to water ',/ 
     1    '    rights by priority. This might cause this account to ', 
     1        'be unable to fill. To correct, allow all accounts to',/
     1    '    fill proportionally and use an operating rule to book ', 
     1        'water from other accounts into a senior account.',//
     1    '    # Res ID       Right ID     Account Tot Acc',/
     1    ' ___exb__________exb__________exb______eb______e')
  484 format(i5, 1x, a12, 1x, a12, 2i8)
  
  485 format(/,
     1  72('_'),//       
     1    '  Riginp; Problem; a reservoir right is tied to more',/
     1    '          accounts than the resrvoir has',/
     1    '          Resrvoir ID = ',a12, 'has ', i5 ' accounts',/
     1    '          Right    ID = ',a12, 'has ', i5 ' accounts')
     
  486 format(/,
     1  72('_'),//       
     1    '  Riginp; Problem; an OOP reservoir right must have',/
     1    ' the variable copid tied to an opeating rule ID',/
     1    '          Resrvoir ID = ',a12, ' Right    ID = ',a12,/
     1    ' has Operating rule ID (copid) = ', a12)

  490 format(/,
     1  72('_'),//     
     1  '  Riginp; Warning',
     1  ' Instream right ', a12, ' has a decree of ', f16.5,            
     1  ' in *.ifr Proceeding on')
     
  491 format(/,
     1  72('_'),//     
     1  '  Riginp; Warning. Water right with a zero decree',/
     1  '  (may be OK, just FYI)',//
     1  '    # Right ID               Decree Type',/
     1  ' ___exb__________exb______________exb__e')
  495 format(i5, 1x, a12, 1x, f16.5, 1x, a4)
  
  496 format(/,
     1  72('_'),//       
     1  '  Riginp; Warning. Structure with no water rights',/,
     1  '  or free water rights only (admin # .gt. ', f8.1,')',/
     1  '  (may be OK if very junior or controlled by an opr right)',//
     1  '    # Structure ID Name                     Type',/
     1  ' ___exb__________exb______________________exb__e')
  497 format(i5, 1x, a12, 1x, a24, 1x, a4)

  492 format(/,
     1  72('_'),//       
     1  '  Riginp; Warning. ',
     1  ' Direct Diversion right ', a12, ' has a decree of ', f16.5,            
     1  ' in *.ddr. Proceeding on')
     
  493 format(i5, 1x, a12, f8.2)
     
  494 format(/,
     1  72('_'),//       
     1  '  Riginp; Warning. ',
     1  ' Reservoir right ', a12, ' has a decree of ', f16.5,            
     1  ' in *.ddr. Proceeding on')                                      

  500 FORMAT(/,
     1  72('_'),//       
     1  '  Riginp; Warning',
     1  '  STRUCTURE ',a12,' OF IFR RIGHT FILE (*.ifr)',/
     1  '          NOT FOUND IN INSTREAM STATION FILE (*.ifs)')
  510 FORMAT(/,        
     1  72('_'),//                                                          
     1  '  Riginp; Warning',
     1  ' TOO MANY I.F.R RIGHTS in *.ifr, MAXIMUM = ',I5)
     
  520 format('  Riginp; Warning reservoir right ', a12,
     1       ' has an undefined type = ',i8) 
  530 FORMAT(/,        
     1  72('_'),//                                                          
     1  '  Riginp; Warning'
     1  ' RESERVOIR ',a12,' in the RESERVOIR RIGHT FILE (*.rer)',/
     1  '          NOT FOUND IN RESERVOIR DATA FILE (*.res)',//,
     1  ' Available Reservoir IDs are:',/
     1  '    #           ID',/
     1  ' ___exb__________e')
     
  540 FORMAT(/,                                                           
     1  72('_'),//       
     1  '  Riginp; Warning',
     1  ' OWNERS CODE ',I8,' IN RES RIGHT FILE (*.rer) EXCEEDS THAT (',
     1    I8,') IN THE RESERVOIR DATA FILE (*.res)')
     
  550 FORMAT(/,
     1  72('_'),//       
     1  '  Riginp; Warning',
     1  ' TOO MANY RES. RIGHTS in *.rer,    MAXIMUM = ',I5)
     
  560 FORMAT(/,
     1  72('_'),//       
     1  '  Riginp; Warning',
     1  ' STRUCTURE ',a12,' OF THE DIVERSION RIGHT FILE (*.ddr)',/
     1  '          NOT FOUND IN DIVERSION STATION FILE (*.dds)')                        
  562 FORMAT(/,
     1  72('_'),//       
     1  '  Riginp; Warning',
     1  ' structure ',a12,' of the well right file (*.wer)',/
     1  '          NOT FOUND in the Well Station FILE (*.wes)')                        
     
  570 FORMAT(                                                           
     1  72('_'),//       
     1  '  Riginp; Warning',                                            
     1  ' USERS CODE ',I3,' IN ',I4,'TH DIV RIGHT FILE (*.rer)'         
     1  ' EXCEEDS THAT (',I2,') OF THE ',I4,'TH DIV DITCH in *.res)')   
     
  580 FORMAT(/,
     1  72('_'),//       
     1  '  Riginp; Warning',
     1  '  TOO MANY DIV. RIGHTS in *.ddr,    MAXIMUM = ',I5)

  582 FORMAT(/,
     1  72('_'),//       
     1  '  Riginp; Warning',
     1  '  TOO MANY WELL RIGHTS in *.wer,    MAXIMUM = ',I5)
     
  590 format(/,
     1  72('_'),//       
     1  '  Riginp; Warning',
     1  ' Structure ', a12,1x,a24,
     1  ' has NO WATER RIGHT or NO ACTIVE WATER RIGHT',/, 10x,
     1  ' May be OK if controlled by an operational right')             
     
  600 format(/,
     1  72('_'),//       
     1  '  Rininp; Water Right Summary',//
     1  '  Number of Diversion Rights       = ',i5,/
     1  '  Number of Reservoir Rights       = ',i5,/
     1  '  Number of Instream Rights        = ',i5,/
     1  '  Number of Operational Rights     = ',i5,/
     1  '  Number of Well Rights            = ',i5,/
     1  '  ________________________________________',/
     1  '  Total Water Rights               = ',i5,//)
     
  602 format(/,
     1  72('_'),//       
     1  '  Riginp; Problem number of total rights = ', i5,/
     1  '          Exceeds dimension (maxnwr)     = ', i5)
  610 format(/,
     1  72('_'),//       
     1  '  Riginp; Problem opening file: ', a256)
     
  620 format(/
     1  72('_'),//       
     1  '  Stopped in Riginp, see the log file (*.log)')

  630 format(
     1  '  Rininp; FYI Number of ', a12, ' Rights = ', i5)
     
 1281  FORMAT(/,72('_'),/
     1  '  RigInp; Warning See *.chk for details regarding: ', a32)
     
c
c _________________________________________________________
c

      END                                                               




