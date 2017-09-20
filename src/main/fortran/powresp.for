c
c *********************************************************
c
      subroutine PowResP(iw,l2,divact,ncallX)
c
c
c _________________________________________________________
c	Program Description
c
c	Type 48
c      PowResP; It simulates a Reservoir or ReUse Plan or
c    	 Acct Plan release to an ISF or a T&C Plan or a 
c    	 Aug Plan or a Special Aug Plan
c    	 Also it can serve two T&C plans; a Total and a 
c    	   secondary at the same time
c    	 
c    	 if iopdes (nf) > 0 destination is an ISF
c    	 if iopdes (nf) < 0 destination is a plan
c      
c    	 if iopsou (nr) > 0 source is a reservoir
c    	 if iopdes (nr) < 0 source is a plan
c
c
c _________________________________________________________
c	Documenatation
c
c      qdiv(15        Reservoir release to a plan or power
c      qdiv(18        Carrier passing thru a structure 
c      qdiv(20        From Carrier by Storage, Exchange or Plan
c      qdiv(28        Carried or exchanged water
c
c      qdiv(30        Plan release to a T&C plan or power
c      qdiv(31        Direct Diversion from ReUse plan to a
c	
c 	   qdiv(35 	      Water with a Reuse or Admin plan source 
c			                 tracked at the destination.
c	     qdiv(36  	    Water released to the river (report as
c			                return flow)


c      qres(1         From river by Priority
c      qres(2  	      From carrier by Priority
c      qres(3          
c      qres(4  	      From carrier by Storage to reservoir
c      qres(8  	      From reservoir to carrier for Tmtn
c      qres(9  	      From reservoir to carrier for non Tmtn.
c      qres(11 	      From reservoir to carrier
c      qres(12        From storage to River for Use (Powres*)
c      qres(18 	      From river by Exchange to Reservoir
c      qres(21 	      From storage for Exchange
c	     qres(22 	      From storage to carrier
c      qres(26 	      From river by Storage to Reservoir
c      qres(27 	      Carrier Loss (DivCar)
c
c _________________________________________________________
c       Update History
c
c rrb 2007/12/26; Revised to allow two destination plans
c rrb 2006/12/19; Recogize a type 10 plan is admnistrative
c		  Therefore set supply to a big number
c rrb 2004/11/29; Edit Powres.
c                 Basically treat a plan source like a reservoir
c                 Note since reuse plan inflows are never diverted 
c                 from the river they do not get release from the 
c		  river herein.
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
      character cwhy*48, cdestyp*12, rec12*12, ccarry*3, cpuse*3,
     1          csource*10, cstaid1*12     
c
c _________________________________________________________
c
c		Step 1; Initilize
      iout=0
      ioutiw=0
      
      if(ichk.eq.148) iout=2
      if(corid(l2).eq. ccall) ioutiw=iw
c
c ---------------------------------------------------------      
cx    if(iout.ge.1 .and. ncallx.eq.0) then
      if(iout.ge.1 .and. ioutiw.eq.iw .and. ncallX.eq.0) then
        write(nlog,102) corid(l2)
 102    format(/, 72('_'),/ '  PowResP; ID = ', a12)
      endif 
            
      isub=48  
      iw = iw
      iisf=0
c
c ---------------------------------------------------------
c rrb 98/03/03; Daily capability
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif
c
c ---------------------------------------------------------      
      np=0
      npD=0
      npD2=0
      if(iopdes(3,l2).ne.0) nPd2=iopdes(3,l2)
      
      divact=0.0
      divalo=0.0
      
      cursto1=-1.0
      curown1=-1.0
      river1=-1.0/fac
      iwhy=0
      cwhy=''
      cwhy(1:2)='NA'
c
c ---------------------------------------------------------
c rrb 98/08/10; Convergence Update
      small = 0.001
      divo1=divo(l2)
      pdem1A=-1./fac
      pdem2A=-1./fac
      
      pdem1B=-1./fac
      pdem2B=-1./fac
      
      avail1=-1./fac
      avail2=-1./fac
      
      qdiv35=-1./fac
      qdiv15=-1./fac
c
c ---------------------------------------------------------
c               d. Check Avail array coming in
c     write(nlog,*) ' PowresP; Calling Chekava going in ',corid(l2)
      call chekava(22, maxsta, numsta, avail)      

      NR  =IOPSOU(1,L2)
      if(nr.gt.0) csource='Reservoir '      
      if(nr.lt.0) csource='Plan      '      
c
c ---------------------------------------------------------      
      cpuse='No'
      ipUse=ireuse(l2)
      if(ipUse.gt.0) cpuse='Yes'

      cdestyp='NA '
      nd  =iopdes(1,l2)
c
c ---------------------------------------------------------      
      ndtype = iopdesr(l2)
      if(ndtype.eq.1) cdestyp='ISF         '
      if(ndtype.eq.2) cdestyp='Reservoir   '
      if(ndtype.eq.3) cdestyp='Diversion   '
      if(ndtype.eq.7) cdestyp='Plan        '
c
c ---------------------------------------------------------
c               Set carrier indicator (currently not set up 
c               to allow a carrier    
      ccarry='No'
      if(intern(l2,1).gt.0) ccarry='Yes'
c
c ---------------------------------------------------------
c rrb 02/10/25; Allow monthly on/off switch
      if(imonsw(l2,mon).eq.0) then
        iwhy=1
        cwhy='Month switch is off'
        goto 130
      endif  
c
c ---------------------------------------------------------
c		For a daily model set demand for beginning of season
      if(iday.eq.1 .and. imonsw(l2,mon).gt.0) then
        if (idy.lt.imonsw(l2,mon)) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 130
        endif  
      endif  
c
c ---------------------------------------------------------
c		For a daily model set demand for end of season
      if(iday.eq.1 .and. imonsw(l2,mon).lt.0) then
        if (idy.gt.iabs(imonsw(l2,mon))) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 130
        endif  
      endif  
      
c _________________________________________________________
c
c		Step 2; FIND THE SOURCE DATA
C
c ---------------------------------------------------------
c
c		Step 2a Source is a Reservoir
      if(nr.gt.0) then
        np=0
        cstaid1=cresid(nr)
        IF(IRESSW(NR).EQ.0) then
          iwhy=2
          cwhy='Source reservoir is off'
          Goto 130
        endif  
c        
        IOWN=NOWNER(NR)+IOPSOU(2,L2)-1
        ISCD=IRSSTA(NR)
        NDNS=NDNNOD(ISCD)
      endif
c _________________________________________________________
c
c		Step 2b Source is a Plan
      if(nr.lt.0) then
        np=-nr
        cstaid1=pid(np)
        nr=0
        IF(pon(np).LE.small) then
          iwhy=3
          cwhy='Source plan is off'
          Goto 130
        endif  
C
        iscd=ipsta(np)
        NDNS=NDNNOD(ISCD)        
        psuply1=psuply(np)
c
c rrb 2008/09/30; If source is a recharge plan, calculate seepage
c		  and returns. Note:
c		  iall = 0 do all reservoirs, =n do reservoir n only
        if(iplntyp(np).eq.8) then    
c
c rrb 2009/03/10; Since many reservoirs may be tied to one plan,
c		    no way to know which reservoir needs to have seepage 
c		    so calculate for all (set iall=0)
c		    Note iplnr(np) originally calculated in GetPlnR 
c		    is not unique        
cx        iall=iplnr(np)    
          iall=0
          CALL SEPSEC(SeepT,iall,'PowResP     ')           
        endif
      endif  
c _________________________________________________________
c
c		Step 3; Destination data
c	                Note if nf > 0 it is an instream flow
c	                Note if nf < 0 it is a plan
C
      ndtype = iopdesr(l2)
      NF  =IOPDES(1,L2)
      
c _________________________________________________________
c
c		Step 3a Destination is an ISF      
c     if(nf.gt.0) then
      if(ndtype.eq.1) then
        NF  =IOPDES(1,L2)
        iisf=1
        idcd=IFRSTA(NF)
        NDNSd=NDNNOD(idcd)        
C
c       IF(FLOWRQ(NF).LE.0.00001) Goto 130
        IF(FLOWRQ(NF).LE.small) then
          iwhy=4
          cwhy='Destination ISF is off'
          Goto 130
        endif
        DIVALO=FLOWRQ(NF)
      endif  
c _________________________________________________________
c
c		Step 3b; Destination is a plan
c			 stored as a negative value
c     if(nf.lt.0) then
      if(ndtype.eq.7) then
c       npD=-nf
        npD=IOPDES(1,L2)
        
C
        idcd=ipsta(npD)
        NDNSd=NDNNOD(idcd)        
        pdem1A=pdem(npD)
        DIVALO=pdem(npD)
cx        write(nlog,*) ' PowResP; pdem1a, divalo pdemT',
cx     1    pdem1a*fac, divalo*fac, pdemT(npD)*fac
                
        IF(pdem(npD).LE.small) then
          iwhy=5
          cwhy='Total Plan Demand is zero'
          Goto 130
        endif  
      endif  
c _________________________________________________________
c
c		Step 3c; Secondary Destination is a plan ID
      if(nPd2.gt.0) then
C
        idcd=ipsta(npD2)
        NDNSd=NDNNOD(idcd)        
        pdem1B=pdem(npd2)
        DIVALO=amin1(divalo, pdem(nPd2))
cx      write(nlog,*) ' PowResP; pdem1b, divalo', pdem1b*fac, divalo*fac
        
        IF(pdem(npD2).LE.small) then
          iwhy=5
          cwhy='Secondary Plan Demand is zero'
          Goto 130
        endif  
      endif  
c _________________________________________________________
c
c		Step 4; CALCULATE VOLUME AVAILABLE
c			from a reservoir or plan
C
c		4a; Reservoir Supply
      if(nr.gt.0) then
        RESAVL=AMIN1(CURSTO(NR)-VOLMIN(NR),CUROWN(IOWN))
        RESAVL=AMAX1(0.,RESAVL)
        RAVCFS=RESAVL/fac
        
        cursto1=cursto(nr)
        curown1=curown(iown)
c
        river1=river(idcd)
        FLOAVL=AMAX1(FLOMAX(NR)-RIVER(iscd),0.)
        IF(FLOAVL.LE.small) then
          iwhy=6
          cwhy='Water supply is zero'
          Goto 130
        endif          
      else
c
c		4b; Plan Supply
c rrb 2006/12/19; Type 10 plan has an administrative supply
c		  Set supply to demand (divalo)
        
        if(iplntyp(np).eq.10) psuply(np)=divalo
        ravcfs=psuply(np)  
        floavl=ravcfs
      endif
c
c
c		4d; Exit if zero              
      IF(RAVCFS.LE.small) then
        iwhy=7
        cwhy='Source Plan supply is zero'        
        Goto 130
      endif  
c
c _________________________________________________________
c
c		Step 6; CALCULATE Supply AS THE MINIMUM OF
c               DEMAND (DIVALO), STORAGE (RAVCFS) AND
C               OUTLET CAPACITY (FLOAVL)

      DIVACT=AMIN1(Divalo, ravcfs, floavl)
cx      write(nlog,*) '  PowResP; ', 
cx     1  divact*fac,  divalo*fac, ravcfs*fac, floavl*fac
c _________________________________________________________
c
c		Step 7; Add Reservoir or Plan releases downstream
c		        Warning reuse plan return flows
c			   were never diverted from the system
C
c		a. Reservoir source
      if(nr.gt.0) then
        availr=avail(iscd)
        TEMP=-DIVACT
        CALL TAKOUT(maxsta, AVAIL ,RIVER ,AVINP ,QTRIBU,IDNCOD,
     1              TEMP  , NDNS,  ISCD)
c
c rrb 05/03/29; Avail supply at the reservoir (iscd) has not increased      
        avail(iscd)=availr
      endif  
c
c ---------------------------------------------------------
c		b. Plan source Note do not add if:
c			   the plan is a recharge (type 8)
c		     or adminnistrative (type 10)
c		     site since it shows up in return flows   
c 
c rrb 201002/5; Correction add np check  
      if(np.gt.0) then 
        iskip=0
        if(iplntyp(np).eq.8 .or. iplntyp(np).eq.10) iskip=1
c 
c rrb 201002/5; Correction add np check above
cx      if(np.gt.0 .and. iskip.eq.0) then
        if(iskip.eq.0) then        
          availr=avail(iscd)
          TEMP=-DIVACT
          CALL TAKOUT(maxsta, AVAIL ,RIVER ,AVINP ,QTRIBU,IDNCOD,
     1              TEMP  , NDNS,  ISCD)
c
c rrb 05/03/29; Avail supply at the plan (iscd) has not increased.      
          avail(iscd)=availr
        endif  
      endif
c
c _____________________________________________________________
c
c               Step 8; Remove destination from Avail to 
c		              shepherd water from other upstream uses
c			         Note since ndown1 = 1; it only adjusts 
c			         availat node idcd.  Also since it uses takou2
c                        only avail gets adjusted (not river & avinp)
c rrb 2008/04/10; Correction; only shepherd water if water has 
c                 been released from a reservoir
      if(nr.gt.0 .or. (np.gt.0 .and. iskip.eq.0)) then
c
c rrb 2010/11/01; Correction to shepherd water from the source
c                 to destination    
cx      ndown=1
cx      call takou2(isub, maxsta, avail, idncod, divact, ndown, idcd)
        ndown1=1
        Avail1=avail(idcd)
        call takou2(isub, maxsta, avail, idncod, divact, ndown1, idcd)
        Avail2=avail(idcd)
      endif

c _________________________________________________________
c
c		Step 9; Update supply variables
      RELAF=DIVACT*fac
C
      if(nr.gt.0) then
c      
cr        write(nlog,*) ' PowresP; 1, RRivpri', qres(1,1)*fac 	    
        cursto1=cursto(nr)
        CURSTO(NR  )=CURSTO(NR  )-RELAF
        PROJTF(NR  )=PROJTF(NR  )+DIVACT
C
        CUROWN(IOWN)=CUROWN(IOWN)-RELAF
        QRES(12,NR)=QRES(12,NR)+RELAF
        accr(12,iown) = accr(12,iown)+relaf
        
        QDIV(15,idcd)=QDIV(15,idcd)+DIVACT    
        qdiv15=qdiv(15,idcd)    
        curown2=curown(iown)
        cursto2=cursto(nr)        
      endif  
c
c ---------------------------------------------------------
c		Source is a plan      
      if(np.gt.0) then
c
        psuply(np)=psuply(np) - divact  
c
c rrb 2006/01/01; Correction for a reservoir plan
        if(iplntyp(np).eq.3 .or. iplntyp(np).eq.5) then
          psto2(np)=amax1(psto2(np)- divact*fac,0.0)                
        endif  
c
c rrb 2008/01/14; Qdiv(28 & qdiv(35 have a reuse or Admin plan source
        qdiv(28,iscd) = qdiv(28,iscd) + divact
c        
c rrb 2008/01/15; qdiv(35 Water with a Reuse or Admin plan source 
c			  at the destination.     
c        
c rrb 2010/09/15; Do not set qdiv(35 (From Plan in *.xwb)
c		              if the destination plan is a
c                 type 2 well aug or type 10 (administrative)     
cx        if(iplntyp(npD).ne.10) then   
        if(iplntyp(npD).ne.2 .and. iplntyp(npD).ne.10) then   
          qdiv(35,idcd) = qdiv(35,idcd) + divact             
          qdiv35=qdiv(35,idcd)
        endif
c
c rrb 2008/01/15; If a T&C or Aug Plan destination set qdiv(30 From River 
c                 by Exch or plan in outmon
c rrb 2010/09/15; Add iplntyp.eq.2 (well aug)
cx      if(iplntyp(npD).eq.1) then    
        if(iplntyp(npD).eq.1 .or. iplntyp(npD).eq.2) then   
          qdiv(30,idcd)=qdiv(30,idcd)+DIVACT
          
cx        write(nlog,*) ' PowResP; ', idcd, qdiv(30,idcd)*fac
        endif         
      endif  
c
c _________________________________________________________
c
c		Step 10; Update destination variables
c
c ---------------------------------------------------------
c		a. Destination is an instream flow
      if(ndtype.eq.1) then
        FLOWRQ(NF  )=FLOWRQ(NF  )-DIVACT
      endif  
c
c ---------------------------------------------------------
c		b. Destination is a plan      
      if(ndtype.eq.7) then
        pdem(npD)=pdem(npD) - divact
        pdem2A=pdem(npD)
 

        if(iplntyp(npD).eq.3 .or. iplntyp(npD).eq.5) then
          psto2(npD)=amax1(psto2(npD) + divact*fac,0.0)           
        endif
c
c     Adjsut if there is a secondary plan (npD2.gt.0)                  
        if(npD2.gt.0) then
          pdem(npD2)=pdem(npD2) - divact
          pdem2B=pdem(npD2)
          if(iplntyp(npD2).eq.3 .or. iplntyp(npD2).eq.5) then
            psto2(npD2)=amax1(psto2(npD2) + divact*fac,0.0)           
          endif
        endif
      endif  
C
c _________________________________________________________
c
c		Step 11; Update reuse for potential pay back
c                       (reservoir re diversion). Note:
c                       when nr>0 the source is a reservoir and 
c                       when ndtype=7 the destination is a plan
      if(nr.gt.0 .and. ndtype.eq.7) then
        preuse1=preuse(l2)
        Preuse(l2) = Preuse(l2) + divact
        if(iout.eq.1)
     1    write(nlog,*) ' PowResP; l2, preuse', l2, preuse1*fac,
     1    preuse(l2)*fac
      endif  
C
c _________________________________________________________
c
c		Step 12; Update operating rule variables

 130  divo(l2)=divo(l2)+divact
      divo2=divo(l2)
cr      write(nlog,*) ' PowresP; 2, RRivpri', qres(1,1)*fac 	  
 
c 
c
c _________________________________________________________
c
c		Step 13; Check printout
        
      if(iout.ge.1 .and. iw.eq.ioutiw) then      
        ncallX=ncallX+1
        if(ncallX.eq.1) then
          if(nr.gt.0)
     1      write(nlog,270) corid(l2), csource, cdestyp, ccarry, cpuse
          if(nr.le.0)
     1      write(nlog,272) corid(l2), csource, cdestyp, ccarry, cpuse
        endif  
        
cx         write(nlog,*) ' PowResP; qdiv15, qdiv35 ', 
cx     1     qdiv15*fac,qdiv35*fac          
c
c               13a. Check Printout source reservoir
        if(nr.gt.0) then        
          write(nlog,280) '  PowResP   ',
     1      iyrmo(mon),xmonam(mon), idy, iwx, cstaid1,
     1      np, nr, nf, npD,     
     1      cursto2, volmin(nr), cursto1-volmin(nr), curown2,
     1      flomax(nr), river1*fac, flomax(nr)-river1, divalo*fac, 
     1      divo1*fac, divo2*fac, pdem1A*fac, pdem1B*fac, 
     1      pdem2A*fac, pdem2B*fac, avail1*fac, avail2*fac, divact*fac, 
     1      iwhy, cwhy
     
        else
c
c               13b. Check Printout source plan
c         write(nlog,142) 
cx          if(iwhy.eq.0) then
            write(nlog,282) '  PowResP   ',
     1        iyrmo(mon), xmonam(mon), idy, iwx, cstaid1,
     1        np, nr, nf, npD,
     1        psuply1*fac, divalo*fac, divo1*fac,  divo2*fac, 
     1        pdem1A*fac,  pdem1B*fac, pdem2A*fac, pdem2B*fac, 
     1        psuply(np),  avail1*fac, avail2*fac, divact*fac, 
     1        iwhy, cwhy     
           endif
        endif
cx      endif
 280  FORMAT(a12, i5,1x,a4, 2i5, 1x, a12,4i5, 17f12.2, i5, 1x, a48) 
 282  FORMAT(a12, i5,1x,a4, 2i5, 1x, a12,4i5, 12f12.2, i5,1x, a48) 
c
c               d. Check Avail array going out
c     write(nlog,*) ' PowresP; Calling Chekava at exit ',corid(l2)
      call chekava(22, maxsta, numsta, avail)      
c     write(nlog,*) ' PowresP; After Chekava ',corid(l2)
        
c
c _________________________________________________________
c
      RETURN
c
c_____________________________________________________________
c               Formats

 270    format(/,
     1    '  PowResP (Type 48); Operation Right ID = ', a12,
     1    ' Source = ', a8,
     1    ' Destination Type = ', a12,
     1    ' Carrier (Y/N) = ',a3, ' Reuse Plan (Y/N) = ', a3/         
     1    '   PowResP    Iyr Mon   Day Iter Source_ID   ',
     1    '   Np   Nr   Nf  npD',
     1    '     CurStoX      VolMin Net_Storage     CurOwnX',
     1    '      FloMax      River1   Net_River      Divalo',      
     1    '       Divo1       Divo2      Pdem1A      Pdem1B',
     1    '      Pdem2A      Pdem2B      Avail1      Avail2',
     1    '      DIVACT iwhy Comment'/
     1    ' ___________ ____ ____ ____ ____ ____________', 
     1    ' ____ ____ ____ ____',
     1    ' ___________ ___________ ___________ ___________',          
     1    ' ___________ ___________ ___________ ___________',
     1    ' ___________ ___________ ___________ ___________',
     1    ' ___________ ___________ ___________ ___________',
     1    ' ___________ ____ ________________________') 
     
 272    format(/,
     1    '  PowResP (Type 48); Operation Right ID = ', a12,
     1    ' Source = ', a8,
     1    ' Destination Type = ', a12,
     1    ' Carrier (Y/N) = ',a3, ' Reuse Plan (Y/N) = ', a3/    
     1    '  PowResP     Iyr Mon   Day Iter Source_ID   ',
     1    '   Np   Nr   Nf  npD',     
     1    '     Psuply1      Divalo       Divo1       Divo2',
     1    '      Pdem1A      Pdem1B      Pdem2A      Pdem2B',
     1    '      Psuply      Avail1      Avail2      DIVACT',
     1    ' iwhy Comment'/
     1    ' ___________ ____ ____ ____ ____ ____________', 
     1    ' ____ ____ ____ ____',     
     1    ' ___________ ___________ ___________ ___________',
     1    ' ___________ ___________ ___________ ___________',
     1    ' ___________ ___________ ___________ ___________',
     1    ' ____ ________________________')

 
c
c_____________________________________________________________
c               Error warnings
c
 9999 write(6,1050) 
      write(nlog,1051) 
      
 1050 format('    Stopped in PowResP',/,
     1       '    See the *.log file')
 1051 format('    Stopped in PowResP')
      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)
      stop      
      
      END
