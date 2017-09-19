C
c *********************************************************
c
      SUBROUTINE RsrspuP(IW,L2,ncallX)
c
c
c _________________________________________________________
c	Program Description
c
c       RsrspuP; Type 34
c		It does Reservoir to Reservoir (Bookover) with Reuse
c		It can constrain the bookover by:
c		  1. The demand of a diversion structure, and
c		  2. The amount diverted by another operating rule,
c		  3. The amount in an OOP Plan, and
c		  4. A carriers capacity
c
c rrb 2008/04/01; Added capability to limit based on limits 
c		  specified in another operating rule = iopsou(5,k)

c _________________________________________________________
c	Documentation
c
c	iopsou(1,l2)    source reservoir pointer
c	iopsou(2,l2)    source reservoir account pointer
c
c	iopsou(3,l2)	Source 2 pointer. Based on iopsou(4,l2
c			Diversion pointer that limits bookover
c			Operating rule pointer that limits bookover
c			Plan pointer that limits the bookover
c	iopsou(4,l2)    Source 2 (iopsou(3,l2) type indicator
c			3=diversion
c			7=plan
c			14=operating rule
c       iopsou(5,l2)    Operating rule with monthly and annual 
c			diverison limit data
c
c	iopdes(1,l2)    destination reservoir pointer
c	iopdes(2,l2)    destination reservoir account pointer
c
c       qres(29,nr)     Amount diverted within the same reservoir
c		        used in Outbal2 for for basin balance
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
      character cwhy*48, cdestyp*12, ccarry*3, cpuse*3, cstaid1*12,
     1 cresid1*12
c
c _________________________________________________________
c
c		Step 1; Detailed Output control and header
c		iout=0 no details
c		     1 details
c		     2 summary
c		     3 qres(29 details
c		    99 summary independent of ccall

      iout=0
      ioutiw=0
      
      if(ichk.eq.134) iout=2
      if(corid(l2).eq. ccall) ioutiw=iw
      
c     write(nlog,*) ' RseSpuP; ioprlim ',ioprlim(l2)
      
c
c _________________________________________________________
c
c		Step 1; Initilize
c
      small = 0.001
      nr=0
      iexit=0
c
c rrb 98/03/03; Daily capability
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif
      
      divact=0.0
      divaf=0.0
      
      resavl=-1.0
      tarcon=-1.0
      divreq1=-1.0/fac
      
      big=99999.
      
      caprem=big/fac
      capremD=-1.0/fac
      capremO=-1.0/fac
      capremP=-1.0/fac
      capremL=-1.0/fac
      qres291=-1.0
      qres292=-1.0
            
      cdestyp='N/A'
      ccarry='No'
      cpuse='No'
      cstaid1='N/A'
      
      iwhy=0
      cwhy='N/A'
      
      imcdx=-1
      iuse2x=-1
      lr=l2
      
      nLimD=0
      nLimO=0
      nLimP=0

c
c _________________________________________________________
c
c		Step 2; Check for monthly on switch
c
      if(imonsw(l2,mon).eq.0) then
        iwhy=1
        cwhy='Monthly switch is off'
        goto 140
      endif  
c
c ---------------------------------------------------------
c		For a daily model set demand for beginning of season
      if(iday.eq.1 .and. imonsw(l2,mon).gt.0) then
        if (idy.lt.imonsw(l2,mon)) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 140
        endif  
      endif  
c
c ---------------------------------------------------------
c		For a daily model set demand for end of season
      if(iday.eq.1 .and. imonsw(l2,mon).lt.0) then
        if (idy.gt.iabs(imonsw(l2,mon))) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 140
        endif  
      endif  
      
c
c _________________________________________________________
c
c		Step 3; FIND Source 1 data (a RESERVOIR)
      NR  =IOPSOU(1,L2)
      ISCD=IRSSTA(NR)
      cstaid1=cresid(nr)
      IROW=NOWNER(NR)+IOPSOU(2,L2)-1

c ---------------------------------------------------------
      IF(IRESSW(NR).EQ.0) then
        iwhy=2
        cwhy='Source Reservoir is off'
        goto 140
      endif  
c
c _________________________________________________________
c
c		Step 4; FIND DESTINATION DATA (a RESERVOIR)
c
      ND  =IOPDES(1,L2)
      IDCD=IRSSTA(ND)
      cdestyp='Reservoir'
      qres291=qres(29,nd)
c
c ---------------------------------------------------------
      
      IF(IRESSW(ND).EQ.0) then
        iwhy=3
        cwhy='Destination Reservoir is off'
        goto 140
      endif  
C
c
c ---------------------------------------------------------
c rrb 2006/09/25; Allow multiple destination accounts - Initilize
cr    IDOW=NOWNER(ND)+IOPDES(2,L2)-1
      nro=1
      if(iopdes(2,l2).lt.0) then
        nro=-iopdes(2,l2)
        idow=nowner(nd)
      endif

      if(iopdes(2,l2).gt.0) then
        nro=1
        idow=nowner(nd)+iopdes(2,l2)-1
      endif
      
c
c _________________________________________________________
c
c		Step 5; Set carrier data 
c                       Treated as pipe outside the river system
c
      if(intern(l2,1).gt.0) then
        ccarry='Yes'
        nc=intern(l2,1)
        divcarry=divcap(nc)-divmon(nc)
        
        if(divcarry.lt.small) then
          iwhy=4
          cwhy='Carrier Limit equals zero' 
          Goto 140
        endif  
        
      endif
c      
c ________________________________________________________
c               Step 6; Set Reuse Plan pointer
c		ipUse = Reuse plan
      ipUse=ireuse(l2)
      if(ipUse.gt.0) cpuse='Yes'
c     if(iout.eq.1) write(nlog,*) '  RsrSpuP; ipUse = ', ipUse      
c
c _________________________________________________________
c
c		Step 7; Limit to the diversion demand (3) specified 
c		  as source 2 (iopsou(3,l2)>0)
c
      if(iopsou(4,l2).eq.3) then
        np=iopsou(3,l2)
        nLimD=np
        capremD=amin1(divcap(np)-divo(l2), divreq(np)-divo(l2))        
        caprem =amin1(caprem, capremD)
        
        if(capremD.lt.small) then
          iwhy=6
          cwhy='Diversion Demand (CapremD) Limit equals zero' 
          iexit=1
cx        Goto 140
        endif  
        
      endif
c
c _________________________________________________________
c
c		Step 8; Limit to an operating rule (14) diversion 
      if(iopsou(4,l2).eq.14) then
c
c rrb 2008/03/26; Correction not a negative in Oprinp      
cx      np=-iopsou(3,l2)
        np=iopsou(3,l2)
        nlimO=np
        capremO=divo(np)-divo(l2)
        caprem =amin1(caprem, capremO)
        
        if(capremO.lt.small) then
          iwhy=6
          cwhy='Operating Rule (CapremO) Limit equals zero' 
          iexit=1
cx        Goto 140
        endif  
      endif
c
c _________________________________________________________
c
c rrb 2006/10/03; Addition
c		Step 9; Limit to an OOP plan volume 
      if(iopsou(4,l2).eq.7) then
        np=iopsou(3,l2)
        nlimP=np
        capremP=psto2(nP)/fac
        caprem =amin1(caprem, capremP)
        
        if(capremP.lt.small) then
          iwhy=7
          cwhy='Plan Limit (CapremP) equals zero' 
          iexit=1
cx          Goto 140
        endif  
      endif
      
c
c _________________________________________________________
c               
c rrb 2008/04/01; 
c               Step 10; Limit to monthly and annual limits
c		provided in an operating rule
c		
c     write(nlog,*) ' RsrSpuP; ',ioprlim(l2), iopsou(5,l2)
      lopr=iopsou(5,l2)

      if(lopr.gt.0) then
        capremL=amin1(oprmaxM(lopr), oprmaxA(lopr))/fac
        caprem =amin1(caprem, capremL)
        
        if(capremL.lt.small) then
          iwhy=8
          cwhy='Volume Limit (capremL) = zero'          
          iexit=1
cx          goto 140
        endif
        
      endif     
c
c _________________________________________________________
c
c		Step 10b; Exit if any limits are = 0
        
      if(iout.eq.1) then
        write(nlog,*) '  RsrspuP; Div Limit; ', 
     1   'np, divcap-divo, divreq-divo'
        write(nlog,*) '  RsrspuP; Div Limit; ',
     1    np, divcap(np)-divo(l2), divreq(np)-divo(l2)
c
        write(nlog,*) ' '
        write(nlog,*) '  RsrspuP; Opr Limit; np, divo(np), divo(l2)'
        write(nlog,*) '  RsrspuP;', np, divo(np), divo(l2)
c
        write(nlog,*) ' '
        write(nlog,*) '  RsrspuP; Plan Limit; np, divo(np), divo(l2)'
        write(nlog,*) '  RsrspuP;', np, divo(np), divo(l2)
c
        write(nlog,*) ' '
        write(nlog,*) 
     1    '  RsrspuP; Volume Limit; l2, OprmaxM(l2), oprmaxA(l2)'
        write(nlog,*) 
     1   '  RsrspuP; Volume Limit;',l2, OprmaxM(l2), oprmaxA(l2)
      endif
      if(iexit.eq.1) goto 140
c
c _________________________________________________________
c
c		Step 11; CALCULATE VOLUME AVAILABLE FROM RESERVOIRS
C
      if(nr.eq.nd) then
        RESAVL=CUROWN(IROW)
      else
        RESAVL=AMAX1(AMIN1(CURSTO(NR)-VOLMIN(NR),CUROWN(IROW)),0.)
      endif
      
      RAVCFS=RESAVL/fac      
      if(ravcfs.lt.small) then
        iwhy=9
        cwhy='Available reservoir or account equals zero' 
        Goto 140
      endif  
c
c _________________________________________________________
c
c		Step 12; Destination is a Reservoir
c			 Check target
c
C grb 12-04-94; Include a check to see max target not exceeded
      tarcon=tarmax(nd)-cursto(nd)
c
c ---------------------------------------------------------
c rrb 2006/09/25; Allow multiple accounts - Demand
      cursa=0.0
      do n=1, nro
        n1=idow+n-1
        cursa=cursa+(ownmax(n1)-curown(n1))
      end do       
      
      if(nr.eq.nd) then
cr      RESALO=OWNMAX(IDOW)-CUROWN(IDOW)
        RESALO=cursa
      else
cr      RESALO=AMIN1(OWNMAX(IDOW)-CUROWN(IDOW),tarcon)
        RESALO=AMIN1(cursa,tarcon)
      endif
      
      RESALO=AMAX1(RESALO,0.)
c               
      RALCFS=RESALO/fac
      if(ralcfs.lt.small) then
        iwhy=10
        cwhy='Available account or target equals zero' 
        Goto 140
      endif  
      
c
c _________________________________________________________
c
c		Step 13; CALCULATE ACTUAL DIVERSION
c
c rrb 2008/03/26; Revised to recognize caprem is initilized to BIG
cx      if (iopsou(3,l2).ne.0) then
cx        DIVACT=AMIN1(RAVCFS,RALCFS,CAPREM)
cx      else
cx        divact=amin1(ravcfs,ralcfs)
cx      endif  
cx
      DIVACT=AMIN1(RAVCFS,RALCFS,CAPREM)
C
      IF(DIVACT.LE.small) then
        iwhy=11
        cwhy='Diversion or Opr Rule Capacity = zero' 
        Goto 140
      endif  
c
      DIVAF=DIVACT*fac
      
c
c
c _________________________________________________________
c		Step 14; Adjust Reservoir Reuse          
      if(ipUse.gt.0) then
        psuply(ipUse)=psuply(ipUse)+divact
        psuplyT(ipUse)=psuplyT(ipUse)+divact
c
c rrb 2006/01/01; Correction
c rrb 2006/10/17; Enhancement
cr      if(iplntyp(ipUse).eq.3 .or. iplntyp(ipUse).eq.5) then
        if(iplntyp(ipUse).eq.3 .or. iplntyp(ipUse).eq.5 .or.
     1     iplntyp(ipUse).eq.9 .or. iplntyp(ipUse).eq.10) then
          psto2(ipUse)=psto2(ipUse)+divact*fac          
          pdrive(iPUse)=pdrive(iPUse)+divact
        endif  
        
        ipsta1=ipsta(ipUse)
        qdiv(28,ipsta1) = psuplyT(ipUse)               
      endif  
      
c
c _________________________________________________________
c
c		Step 15; UPDATAE variables
c
c ---------------------------------------------------------
c		a. Source Reservoir
      CURSTO(NR  )=CURSTO(NR  )-DIVAF
      CUROWN(IROW)=CUROWN(IROW)-DIVAF
      accr(22,irow) = accr(22,irow)+divaf
C
c
c ---------------------------------------------------------
c		b. Destination Reservoir
c rrb 2006/09/25; Allow multiple accounts - Allocate
c		  Note:
c		   iResT1=0 distributes based on ownership ratio
c		   nrown1=number of accounts in this reservoir
c		   iown = first account associated with this reservoir  
c		   icx  = subroutine calling accou.for       
c		   ia   = account to adjust
cr    CUROWN(IDOW)=CUROWN(IDOW)+DIVAF
cr    accr(4,idow)  = accr(4,idow)+divaf

      CURSTO(ND)=CURSTO(ND)+DIVAF      
      nrX=nd
      iResT1=0
      nrown1=nro
      iownX=idow
      icx=134
      ia=4
      cresid1=cresid(nrX)
c        
      call accou(maxacc, maxown, nrX, ownmon, curown, accr, ia, 
     1    ownmax, iownX, nrown1, cursa, divaf, iResT1, icx, cresid1)
      
c
c ---------------------------------------------------------
c rrb 2006/10/02 
c		c. Update Demand limitation
cr    if (nd.ne.nr) DIVMON(NP  )=DIVMON(NP  )+DIVACT
      if (nLimD.gt.0) DIVMON(NP  )=DIVMON(NP  )+DIVACT
c      
c ---------------------------------------------------------
c rrb 2006/10/02 
c		d. Update Plan limitation
      if (nLimP.gt.0) psto2(np)=psto2(np) - divaf
c
c ---------------------------------------------------------
c		e. Update Reservoir Details
      QRES(4,ND)=QRES(4,ND)+DIVAF
      QRES(22,NR)=QRES(22,NR)+DIVAF
      
c
c ---------------------------------------------------------
c		f. Update operating rule diversion
      divo(l2)=divo(l2)+(divaf/fac)
c
c ---------------------------------------------------------
c rrb 2006/10/03; 
c		g. Store amount diverted within the same reservoir
c		for basin balance
c rrb 2007/01/02; Revise to be reservoir to reservoir (not necessarily
c		  the same reservoir by removing if(nd.eq.nr)
cz      if(nd.eq.nr) then
        qres291=qres(29,nd)
        qres(29,nd)=qres(29,nd) + divaf      
        qres292=qres(29,nd)
        
cx        if(iout.eq.99) then
cx          write(nlog,*) ' RsrSpuP; cresid, iyrmo(mon),xmonam(mon),',
cx     1     'nd, nr, qres1, divaf, qres291, qres(29,nr) ' 
     
cx          write(nlog,*) ' RsrSpuP; ',
cx     1      cresid(nr), iyrmo(mon),xmonam(mon), nd, nr, 
cx     1      qres291, divaf, qres291, qres(29,nr)
cx        endif
cz      endif
c
c _________________________________________________________
c
c rrb 2007/07/03; 
c               h. Adjust monthly or annual plan limitations
c		   Note ioprlim(l2) = 2 from Oprinp
      if(iout.eq.1) write(nlog,*) ' RsrSpuP; ', ioprlim(l2)
      
      if(lopr.gt.0) then
        ipLim=iopsou(1,lopr)        
        call SetLimit(
     1    nlog, icx, lopr, ipLim, ioprlim(l2), fac, 
     1    divact, OprmaxM(lopr), OprMaxA(lopr), 
     1    Oprmax(lopr,mon), Oprmax(lopr,13), OprmaxM1, OprmaxM2, 
     1    psto1(ipLim), psto2(ipLim), corid(l2))
      endif
c
c _________________________________________________________
c
c		Step 16; Detailed Check
 140  continue     
      if(iout.eq.99 .and. divact.lt. small) iout=98      
      if((iout.ge.1 .and. iw.eq.ioutiw) .or. iout.ge.99) then        
        ncallX=ncallX+1
        if(ncallX.eq.1) then
          write(nlog,270) corid(l2), cdestyp, ccarry, cpuse          
        endif  
      
        write(nlog,280) ' RsrSpuP;   ', iyrmo(mon),xmonam(mon), idy,
     1    cstaid1,iwx, iw,nwrord(1,iw),l2,lr, ND,nr, iuse2x,imcdX,
     1    qres291, qres292,  
     1    ResAvl, tarcon, capremD*fac, capremO*fac, capremP*fac, 
     1    capremL*fac, caprem*fac, divact*fac, iwhy, cwhy
      endif
c
c _________________________________________________________
c
c		Step 17; Roundoff check
c
      if(nr.gt.0) then
        call chekres(io99,maxres, 1, 6, iyr, mon, nr,nowner,
     1               curown,cursto,cresid)
      endif
c
c _________________________________________________________
c
c		Step 18; Return
      return
c
c _________________________________________________________
c
c		Formats

  280 FORMAT(a12, i5,1x,a4, i5, 1x,
     1      a12,9i8,10F8.0,i8, 1x, a48)
     
  270   format(/, 
     1  ' RsrspuP (Type 34); Operation Right ID = ', a12,
     1  ' Destination Type = ', a12,
     1  ' Carrier (Y/N) = ',a3, ' Reuse (Y/N) = ', a3/    
     
     1  ' RsrSpuP      iyr  mon  day Source ID   ',
     1  '    Iter      Iw  nwrord      l2      lr      Nd      nr',
     1  '  iuse2X   ImcdX qres291 qres292  ResAvl  TarCon CapremD',
     1  ' CapremO CapremP CapremL  Caprem Divact',
     1  '    iwhy Comment',/
     1  ' ___________ ____ ____ ____ ____________', 
     1  ' _______ _______ _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ __________________________')

      END
