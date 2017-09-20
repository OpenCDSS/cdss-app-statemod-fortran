c
c *********************************************************
c
      SUBROUTINE RSRSPU(IW,L2,ncallX)
c
c
c _________________________________________________________
c	Program Description
c
c       Rsrspu; Type 6 Reservoir to Reservoir (Bookover)
c		It can constrain the bookover by a carrier capacity
c		and a demand specified in the diversion demand file 
c               (*.ddm)
c
c
c _____________________________________________________________
c	Update History
c
c      2015/07/08; Revised to control reoperation if
c                  an operating rule is provided as a limit in
c                  iopsou(3) (e.g. ity = 2) and that
c                  operating rule has been called at least
c                  once in this time step.  Implemented to 
c                  control bookover operatoins after water is
c                  booked out then back in when iopsou(3,l2)
c                  is an operating rule specified and oprlimit(l2)
c                  is 1.
c
c _________________________________________________________
c
c		Documentation
c
c	       iopsou(1,l2)   source reservoir pointer
c        nr             source reservoir pointer
c	       iopsou(2,l2)   source reservoir account pointer
c        
c	       iopsou(3,l2)	  0 not used
c                       1 Diversion pointer that limits bookover
c	       		            2 Operating rule pointer that limits the bookover  
c 
c        ity            1 Same as above for iopsou(3,l2) = 1        
c	       ity            2 Same as above for iopsou(3,l2) = 2
c
c	       iopsou(4,l2)	  0 not used
c	       		            99 indicates iopsou(3,l2) is a diversion
c        
c	       iopdes(1,l2)   destination reservoir pointer
c        nd             destination reservoir  pointer
c	       iopdes(2,l2)   destination reservoir account pointer
c                       If iopdes(2,l2)>0 bookover to one account
c                       if iopdes(2,l2)<0 bookover to first n accounts
c        nro            # of accounts to bookover into
c        nbook          = 1 bookover to 1 account
c                       = 2 bookover to first n accounts
c
c        oprCall(l2)    = counts number of times this operating
c                         rule has been called.  Used with oprlimit(l2)
c                         is an operatng rule and oprlimit = 1
c                         to not allow a bookover once another 
c                         operating rule specified as iopsou(3,l2)
c                         has operated.
c                         
c        ioprlim(l2)    = 0 not used
c                       = 1 limit reoperation of operating rule l2
c                         when the operating rule associated with
c                         ID = ciopsoX2(l2) has been operated 
c
c        iresT1	          Type of account distribution in Accou
c			                  = 0 Ownership Ratio 
c                       = 1 Available Space 
c                       = -1 One Account  
c
c        qres(4          From carrier to storage by Other
c		     qres(22         From storage to carrier
c        qres(29,nr)     Amount diverted within the same reservoir
c		                     used in Outbal2 for for basin balance
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
      character cwhy*48, cdestyp*12, ccarry*3, cpuse*3, cstaid1*12,
     1          cresid1*12  
c _________________________________________________________
c
c		Step 1; Initilize
c
c
c _________________________________________________________
c
c		Step 1; Detailed Output control and header
c		iout=0 No details
c		iout=1 Details
c		iout=2 Summary
c		iout=3 Qres(29 details
c		iout=99 summary indenpendent of ioutiw
      iout=0
      ioutiw=0
c
c rrb 2016-/06/25; Additional outut  
      if(ichk.eq.4) iout=2 
c         
      if(ichk.eq.106) iout=2
      if(corid(l2).eq. ccall) ioutiw=iw
c
c rrb 2015/06/25; Additional output control for debugging
c                 when iout=2 and ioutiw=iw.  Note:   
c                 ioutX = 1 misc source & destination detail 
c                 ioutx = 2 focused source & destination detail
      ioutX=0
      if(iout.eq.2 .and. ioutiw.eq.iw) then
        ioutX=0  
        write(io99,*) ' '    
        write(io99,*) '________________________________'
        write(io99,*) 'Rsrspu; iout, iw, ioutX', iout, iw, ioutX
      endif
c     
      divaf = 0.0
      small = 0.001
      nr=0
c
c rrb 98/03/03; Daily capability
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif
c
      divact=0.0      
      resavl=-1.0
      divreq1=-1.0
      tarcon=-1.0
      qres291=-1.0
      qres292=-1.0
      resalo=-1.0
      
      cdestyp='NA'
      ccarry='No'
      cpuse='No'
      cstaid1='NA'
      
      iwhy=0
      cwhy='N/A'
      ity=0
      lr=l2
      
      
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
c rrb 2015/07/08; Add capability to not operate any more
c                 this time step based on user data
       if(icallOP(l2).ge.1 .and. ioprlim(l2).eq.1) then
         iwhy=2
         cwhy='Reoperation limited to once per time step'
         goto 140
       endif    
c
c _________________________________________________________
c
c		Step 3; FIND Source data (a RESERVOIR)
c
      NR  =IOPSOU(1,L2)
c     write(io99,*) '  Rsrspu; nr = ', nr
c     write(6,*) '  Rsrspu; nr = ', nr

      IF(IRESSW(NR).EQ.0) then
        iwhy=2
        cwhy='Source Reservoir is off'
        goto 140
      endif  
      
C
      IROW=NOWNER(NR)+IOPSOU(2,L2)-1
      ISCD=IRSSTA(NR)
      cstaid1=cresid(nr)       
c
c _________________________________________________________
c
c		Step 4; FIND DESTINATION DATA (a RESERVOIR)
c
      ND  =IOPDES(1,L2)
      cdestyp='Reservoir'
      qres291=qres(29,nd)
      
      IF(IRESSW(ND).EQ.0) then
        iwhy=3
        cwhy='Destination Reservoir is off'
        goto 140
      endif  

      IDCD=IRSSTA(ND)
cr    IDOW=NOWNER(ND)+IOPDES(2,L2)-1
c
c rrb 2015/09/09; Detailed output
      if(ioutx.eq.1) then
        write(nlog,*) 
     1    'RsrSpu; Step 3;   l2   nr   nd  iopsou cstaid1'
        write(nlog,'(a15,1x, 3i5, i8, 1x, a12)') 
     1   ' RsrSpu; Step 3;', l2, nr, nd, iopsou(2,l2), cstaid1        
      endif

c
c ---------------------------------------------------------
c rrb 2006/09/25; Allow multiple accounts - Initilize      
c rrb 2015/06/25; If iopdes(2,l2)>0 bookover to one account
c                 if iopdes(2,l2)<0 bookover to first n accounts
c
      nro=1
c
c rrb 2015/06/25      
      nbook=1
      if(iopdes(2,l2).lt.0) then
c
c rrb 2015/06/25; 
        nbook=2      
        nro=-iopdes(2,l2)
        idow=nowner(nd)
c
c rrb 2015/09/11; Correction
        iresT1=0
      endif

      if(iopdes(2,l2).gt.0) then
        nro=1
        idow=nowner(nd)+iopdes(2,l2)-1
c
c rrb 2015/09/11; Correction
        iresT1=-1        
      endif
      
c
c _________________________________________________________
c
c		Step 5; Determine if limited by an
c           Opr right or diversion 
c		        Note 0 = none
c		            <0 = opr right or diversion limit
c
c rrb 2006/11/27; Revise to allow output
c     caprem=2000000
      caprem=1000000/fac
      if(iopsou(3,l2).ge.0) go to 110
      
      ity=-(iopsou(3,l2)-((iopsou(3,l2)/10)*10))
      write(99,*) 'ity',ity
c _________________________________________________________
c
c		Step 6; Diversion Limit (ity=1)      
      if(ity.eq.1) then
        NP  =-iopsou(3,l2)/10
c
c  grb following line added 6-29-97 to allow for switch to use ddm file for a
c  structure as the necessary contents in the source before a bookover is made
c
c rrb 2006/11/27; Simplify by doing check here and storing as caprem 
cx      if (iopsou(4,l2).eq.99) goto 110
c
c
c ---------------------------------------------------------
c		6a. Diversion Limit (ity=1) for Capacity
c     
c rrb 2006/11/27; Limit by capacity only (e.g. not demand (divreq(np))
cr      caprem=amin1(divcap(np)-divo(l2), divreq(np)-divo(l2))
        caprem=divcap(np)-divo(l2)
        caprem=amax1(0.0, caprem)
c
c rrb 2006/11/27; Addition      
        if(caprem.le.small) then
          iwhy=4
          cwhy='Diversion Capacity Limit (Caprem) is zero'
          write(nlog,*) ' Rsrspu; ', divcap(np), divo(l2), caprem
          Goto 140
        endif  
c
c ---------------------------------------------------------
c		6b. Diversion Limit (ity=1) for Demand
c rrb 2006/11/27; Simplify by doing check here and storing as caprem 
        if(iopsou(4,l2).eq.99) then
          caprem=amin1(caprem,divreq(np))
          divreq1=divreq(np)
c        
          if(divreq1.le.small) then
            iwhy=5
            cwhy='Diversion limit (DivReq1) is zero'
            Goto 140
          endif          
        endif
c       go to 110
      endif  
c
c _________________________________________________________
c
c		Step 7; Source 2 operational right limit (ity.eq.2)
c100  np=-(iopsou(3,l2)/10)
c
      if(ity.eq.2) then   
        np=-(iopsou(3,l2)/10)
        caprem=divo(np)-divo(l2)
        caprem=amax1(caprem, 0.0)
c
c rrb 2006/11/27; Addition      
        if(caprem.le.small) then
          iwhy=6
          cwhy='Operational Limit (Caprem) is zero'
          Goto 140
        endif 
      endif
c
c _________________________________________________________
c
c		Step 8; CALCULATE VOLUME AVAILABLE FROM SOURCE ACCOUNT
C
  110 if(nr.eq.nd) then
        RESAVL=CUROWN(IROW)  
      else
        RESAVL=AMAX1(AMIN1(CURSTO(NR)-VOLMIN(NR),CUROWN(IROW)),0.)
      endif
c
c rrb 2015/06/25; Additional output control for debugging
      if(ioutX.eq.1) then
        write(nlog,*) ' '
        write(nlog,*) 'RsrSpu_Step 8;    nr   nd irow  RESAVL'
        write(nlog,'(a15, 3i5, 20f8.0)') 
     1                ' RsrSpu_Step 8; ', nr, nd, irow, RESAVL 
        write(nlog,*) ' '
      endif
c
c rrb 2006/11/27; Addition      
      if(resavl/fac.le.small) then
        iwhy=8
        cwhy='Reservoir Source (ResAvl) is zero'
        Goto 140
      endif  
      
c
c _________________________________________________________
c
c		Step 9; Limit based on a Diversion demand
c
c rrb 2006/11/27: Move above and store as caprem
cr    if(iopsou(4,l2).eq.99) then
cr
cr      resavl=amin1(resavl,(divreq(np)*fac))
cr      resavl=amax1(resavl,0.0)
cr      divreq1=divreq(np)
cr      
cr      if(resavl/fac.le.small) then
cr        iwhy=9
cr        cwhy='Diversion limit (DivReq1) is zero'
cr        Goto 140
cr      endif          
cr    endif
c
c _________________________________________________________
c
c		Step 10; Calculate Destination Space Available
c
C grb 12-04-94; Include a check to see max target not exceeded
      tarcon=tarmax(nd)-cursto(nd)
c
c ---------------------------------------------------------
c rrb 2006/09/25; Allow multiple accounts for Destination
      cursa=0.0
      do n=1, nro
        n1=idow+n-1
        cursa=cursa+(ownmax(n1)-curown(n1))
c
c rrb 2015/06/25; Additional output control for debugging
        if(ioutX.eq.1) then
          cx=ownmax(n1)-curown(n1)
          write(nlog,*) 'RsrSpu_Step 10;  nro    n   n1',
     1                  '  ownmax  curown      cx   cursa'
          write(nlog,'(a16, 3i5, 20f8.0)')
     1      ' RsrSpu_Step 10;', nro, n, n1,
     1                    ownmax(n1), curown(n1), cx, cursa    
        endif    
      end do    
          
      if(nr.eq.nd) then
        resalo=cursa
      else
        RESALO=AMIN1(cursa,tarcon)
      endif
      RESALO=AMAX1(RESALO,0.)
      
      if(resalo/fac.le.small) then
        iwhy=10
        cwhy='Destination Available (ResAlo) is zero'
        Goto 140
      endif  
c
c rrb 2015/06/25; Additional output control for debugging
      if(ioutX.eq.1) then
        write(nlog,*) ' '
        write(nlog,*) 'RsrSpu_Step 10b;    nr, nd, irow, RESALO'
        write(nlog,*) 'RsrSpu_Step 10b; ', nr, nd, irow, RESALO 
        write(nlog,*) ' '
      endif
c               
      RAVCFS=RESAVL/fac
      RALCFS=RESALO/fac
c
c _________________________________________________________
c
c		Step 11; CALCULATE ACTUAL DIVERSION
c
  120 DIVACT=AMIN1(RAVCFS,RALCFS,CAPREM)
C
c		The followig exit should never occur
c     IF(DIVACT.LE.0.00001) Goto 140
      if(resavl/fac.le.small) then
        iwhy=11
        cwhy='ResAvl, DivReq1 or ResAvl is zero'
        Goto 140
      endif  
c
c rrb 98/03/03; Daily capability
c     DIVAF=DIVACT*MTHDAY(MON)*FACTOR
      DIVAF=DIVACT*fac
c
c _________________________________________________________
c
c		Step 15; UPDATAE variables

c
c rrb 2015/06/25; Additional output
      if(ioutX.eq.2) then
        write(nlog,*)' '
        write(nlog,*)
        write(nlog,*)
     1    'RsrSpu_Step 15a;  nro   divaf',
     1    '   nr  cursto irow curownS   nd  cursto idow curownD'
        write(nlog,'(1x, a16, 20(i5,f8.0))') 
     1    'RsrSpu Step 15a;', nro, divaf,
     1     nr, cursto(nr), irow, curown(irow), nd, cursto(nd),  
     1     idow, curown(idow) 
      endif

c
      CURSTO(NR  )=CURSTO(NR  )-DIVAF
      CUROWN(IROW)=CUROWN(IROW)-DIVAF
C
      CURSTO(ND  )=CURSTO(ND  )+DIVAF
c
c
c rrb 2015/09/09; Note the following has been commented
c                 out for a long time so that subroutine
c                 Accou can be used to allocate water to
c                 accounts.
cr    CUROWN(IDOW)=CUROWN(IDOW)+DIVAF
c
c rrb 2015/06/25; Additional output
      if(ioutX.eq.2) then
        write(nlog,*)' '
        write(nlog,*)
        write(nlog,*)
     1    'RsrSpu_Step 15b;  nro   divaf',
     1    '   nr  cursto irow curownS   nd  cursto idow curownD'
        write(nlog,'(1x, a16, 20(i5,f8.0))') 
     1    'RsrSpu Step 15b;', nro, divaf, 
     1     nr, cursto(nr), irow, curown(irow), nd, cursto(nd),  
     1     idow, curown(idow) 
      endif
c
c ---------------------------------------------------------
c rrb 2006/09/25; Allow multiple accounts - Allocate
c		  Note:
c		   iResT1 =  distributes based on ownership ratio
c		   nrown1 =  number of accounts in this reservoir
c		   iown   =   first account associated with this reservoir  
c		   icx    =   subrouine calling accou.for       
c		   ia     =   account to adjust
      
      nrX=nd
c
c rrb 2015/09/11; Correction set IresT1 above based on 
c                 number of accounts  
cx    iResT1=0
      nrown1=nro
      iownX=idow
      icx=106
      ia=4
      cresid1=cresid(nrX)
c        
      call accou(maxacc, maxown, nrX, ownmon, curown, accr, ia, 
     1  ownmax, iownX, nrown1, cursa, divaf, iResT1, icx, cresid1)
c
c rrb 2015/06/25; Additional output
      if(ioutX.eq.2) then
        write(nlog,*)' '
        write(nlog,*)
     1    'RsrSpu_Step 15c;  nro   divaf',
     1    '   nr  cursto irow curownS   nd  cursto idow curownD'
        write(nlog,'(1x, a16, 20(i5,f8.0))') 
     1    'RsrSpu Step 15c;', nro, divaf,
     1     nr, cursto(nr), irow, curown(irow), nd, cursto(nd),  
     1     idow, curown(idow) 
      endif
     
     
c
c ---------------------------------------------------------
C grb 1-26-95   Update carrier diversions if not same res transfer
c     DIVMON(NP  )=DIVMON(NP  )+DIVACT
      if (nd.ne.nr) DIVMON(NP  )=DIVMON(NP  )+DIVACT
c
c        nd = destination
c        nr = source
c        qres(4          From carrier to storage by Other
c		     qres(22         From storage to carrier
c
      QRES(4,ND)=QRES(4,ND)+DIVAF
      QRES(22,NR)=QRES(22,NR)+DIVAF
      
cr    accr(4,idow)  = accr(4,idow)+divaf
      accr(22,irow) = accr(22,irow)+divaf
c
c rrb 2015/09/11; Additional output
      if(ioutX.eq.2) then
        write(nlog,*) ' '
        write(nlog,*) 'RsrSpu_1; l2, divo(l2)', l2, divo(l2)*fac
      endif
c
      divo(l2)=divo(l2)+(divaf/fac)
c      
      if(ioutX.eq.2) 
     1 write(nlog,*) 'RsrSpu_2; l2, divo(l2)', l2, divo(l2)*fac      
c
c ---------------------------------------------------------
c rrb 2006/10/03; 
c		Store amount booked over within the same reservoir
c		for basin balance
c rrb 2015/08/11; Revise to set qres(29 for all reservoir to
c                 reservoir bookovers to correct a reporting
c                 problem in the San Juan
cx      if(nd.eq.nr) then
        qres291=qres(29,nd)
        qres(29,nd)=qres(29,nd) + divaf 
        qres292=qres(29,nd)     
        if(iout.eq.3 .and. qres291.gt.small) then
          write(nlog,*) ' RsrSpu; cresid, iyrmo(mon),xmonam(mon),',
     1     'nd, nr, qres1, divaf, qres(29 ' 
     
          write(nlog,*) ' RsrSpu; ',
     1      cresid(nr), iyrmo(mon),xmonam(mon), nd, nr, 
     1      qres291, divaf, qres(29,nr)
        endif
cx      endif
c
c _________________________________________________________
c
c		Step 16; Detailed Check
 140  continue     
C
C     WRITE(6,150) IYR,MON,IW,L2,NR,QRES(16,NR),QRES(11,NR),
C    1             ND,QRES(2,ND),QRES(4,ND),ISCD,QDIV(16,ISCD),
C    2             IDCD,QDIV(16,IDCD)
c
c ---------------------------------------------------------
c		Detailed header      
      if(iout.eq.99 .and. divact.lt. small) iout=98
      if((iout.eq.2 .and. iw.eq.ioutiw) .or. iout.ge.99) then      
        ncallX=ncallX+1
        if(ncallX.eq.1 .or. ioutX.eq.2) then
          write(nlog,270) corid(l2), cdestyp, ccarry, cpuse          
        endif  
  
        write(nlog,280) ' RsrSpu     ', iyrmo(mon),xmonam(mon), idy,
     1    cstaid1,iwx, iw,nwrord(1,iw),l2,lr, ND,nr, ity,
     1    qres291, qres292, ResAvl,  divreq1, CapRem*fac, resAlo,
     1    divact*fac, iwhy, cwhy
        endif
c
c _________________________________________________________
c
c		Step 17; Roundoff check
c
      if(nr.gt.0) then
        call chekres(io99,maxres, 1, 6, iyr, mon, nr,nowner,
     1               curown,cursto,cresid)
c
c rrb 2015/06/25; Additional output
        if(ioutX.eq.1) then
          write(nlog,*)' '
          write(nlog,*)
          write(nlog,*)
     1      'RsrSpu_Step 15d;  nro   divaf',
     1      '   nr  cursto irow curownS   nd  cursto idow curownD'
          write(nlog,'(1x, a16, 20(i5,f8.0))') 
     1      'RsrSpu Step 15d;', nro, divaf,
     1       nr, cursto(nr), irow, curown(irow), nd, cursto(nd),  
     1       idow, curown(idow) 
        endif    
      endif
c
c _________________________________________________________
c
c rrb 2015/07/08
c		Step 18; Check for switch to not operate water right k
c            if water right l2 has operated and
c            the operating right ID of l2 is specified
c            as ciopsoX2 for water right k based on user
c            supplied data
c rrb 2015/09/11; Correction; should not require the rule
c                 used to monitor reoperation (ioprlim(l2)) 
c                 have data required to turn itself off.  
cr      if(ioprlim(l2).eq.1) then
        do k=1,numopr
          if(corid(l2) .eq. ciopsoX2(k)) then
c
c rrb 2015/07/18; Correction 
c rrb 2015/09/11; Back to original     
cx          icallOP(k) = icallOP(k) + 1
cx          icallOP(l2) = icallOP(l2) + 1 
            icallOP(k) = icallOP(k) + 1               
c             
            if(ioutX.eq.1) then
              write(nlog,*)
     1          ' RsrSpu; Switch to not allow reoperation on'              
              write(nlog,*) 
     1          ' RsrSpu; k, l2, corid(k), corid(l2),
     1            ciopsoX2(k), icallOP(l2)'
              write(nlog,*) 
     1          ' RsrSpu;', k, l2, corid(k), corid(l2),
     1            ciopsoX2(k), icallOP(l2)  
            endif
          endif
        end do 
cr      endif         
c
c _________________________________________________________
c
c		Step 19; Return
      return
c
c _________________________________________________________
c
c		Formats

  150 FORMAT(/,' RsrSpu ',4I5,2(I5,2F8.2),2(I5,F8.2))
     
  270   format(/, 
     1  ' Rsrspu (Type 6); Operation Right ID = ', a12,
     1  ' Destination Type = ', a12,
     1  ' Carrier (Y/N) = ',a3, ' Reuse (Y/N) = ', a3/    
     
     1  ' RsrSpu       iyr  mon  day Source ID    ',
     1  '    Iter      Iw  nwrord      l2      lr      Nd      nr',
     1  '     ity qres291 qres292  ResAvl DivReq1  CapRem',
     1  '  ResAlo  Divact    iwhy Comment',/
     1  ' ___________ ____ ____ ____ ____________ ', 
     1  ' _______ _______ _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ __________________________')
     
  280 FORMAT(a12, i5,1x,a4, i5, 1x, a12,1x, 8i8,7F8.0,i8, 1x, a48)

      END
