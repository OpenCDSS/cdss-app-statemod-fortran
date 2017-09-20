c
c *********************************************************
c
			SUBROUTINE VIRCOM(iin, i12, numstaX)
C
c
c _________________________________________________________
c	Program Description
c
C       Vircom; it COMPUTES VIRGIN FLOWS FROM HISTORICAL DATA
c
c       Called once per year from Virgen.for
c
c_______________________________
c       Update History
c rrb 2008/10/31; Included to recharge. Note it is part of
c	           total diversion, therefore must be removed
c		    to irrigate
c rrb 2003/03/03  Revised roundoff issues related to -999
c rrb 2001/07/30; Refined soil moisture capability
c rrb 2001/03/12; Added daily capability
c rrb 2000/12/26; Added variable efficiency capability to:
c                 call rtnsec, rtnsecw, deplete
c
c _________________________________________________________
c       Documentation
c
c               iin    = response file # (20)
c               i12    = control (12)
c               iopflo = 1 calculate total flow at inflow points
c                      = 2 calculate gain at inflow points
c                        Note: for option 2, need to redefine flow
c                        at gauges and allow loosing (negative) flows
c                        For option 1 do not allow negative flows
c               ineg   = 0 naturalized flow is not negative
c                        -n # of negative naturalized flow occurances at a gage
c                        +n # of negative naturalized flow occurances at a
c                           nongage
c               iday   = 0 monthly model
c                        1 daily model
c
c             Prior to virnod.for
c               virinp = historic gain at gages (In mdainp
c                        the total flow is read and changed to gain)
c               qhisto = total base flow at gages (sum of virinp plus
c                        diversion - import - returns - delta storage)
c
c               After virnod.for when iopflo=2, the following is done
c                        to calculate gain at a gage
c               virinp = total base flow at gages (storing qhisto)
c               qhisto = gain at all inflow points except gages
c               flowx  = total base flow at all stream nodes
c
c		tempq 	 Gaged flow
c		tempi	 Import
c		tempd	 Diversion
c		tempr	 Return Flow
c		tempw	 Well Depletion
c		temps	 Reservoir Storage Change
c		tempe	 Reservoir Evaporation
c		tempRre Reservoir to Recharge 
c		tempDre Diversion to Recharge 
c		tempUse Diversion to Use (non recharge)
c		tempts	 To Soil Moisture
c		tempfs	 From Soil Moisture
c		tempcu	 CU
c		templ   Loss
c		tempp   Pumping
c		
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
      dimension dumA(13), demspr(maxDivw)
      DIMENSION                                            
     1  RET(numstax),     DEP(numstax)
c     
c
c _________________________________________________________
c		Step 1 - Initilize
c
c      
c		iout = 1 details
c		ioutCU=1 details on CU
c		ioutCU=2 details plus summary on CU
c		ioutSP=1 details on Sprinkler Demand
c		ioutRe=1 details on diversion to recharge
c		ioutRe2=1 details on reservoir recharge
c		ioutD =1 details on a particular ditch
c		ioutRtn=1 details on return flows by stream ID & month
c		        2 details on return flows by month 
c             Warning this output includes off-channel returns
c			        in the total
c                     
      iout=0
      ioutCU=0
      ioutSP=0
      ioutRe=0
      ioutRe2=0
      ioutPU=0   
      ioutD=0 
      ioutRtn=0
      
cx    ichk=4
       
c
c rrb 2009/056/15;  pass numstax in subroutine call
cx    numstax=maxsta
      
      nout=17
      
      if(iout.gt.0 .or. ichk.eq.4) then
        write(nlog,*) ' Vircom; ', numstaX
        write(6,*) ' Vircom ', numstaX
      endif
      
c
c rrb 01/01/17; Call number
      icx=7
      
      small=0.001
      smalln=-1.*small
      smallx=0.1
      
      totA=0.0
      do im=1,13
      	dumx(im)=0.0
      
      	do nd=1,numdiv
         dum(im,nd)=0.0
      	  dum2(im,nd)=0.0
      	end do
      end do
c
c ---------------------------------------------------------
c		Initilize in year 1 only      
      if(iyr.eq.iystr) then
        irec=0
        do is=1,numsta
          ineg(is) = 0
        end do
        
        do im=1,13
          dumA(im)=0.0
        end do  
      endif   
      
      		
c
c ---------------------------------------------------------
c		Get calendar year data
      call year(iyr, iyrmo, imomo, cyr1)
      
      write(70,120) iyrmo(1), xmonam(1), iyrmo(12), xmonam(12)
      
      if(iday.eq.1) then
        write(74,120) iyrmo(1), xmonam(1), iyrmo(12), xmonam(12)
      endif
c
c
c =========================================================
c               Month Loop
c

C      write(nlog,262) iyrmo(mon)
      write(nlog,262) iyr
      DO 340 MON=1,12
c
c rrb 01/12/28; Print in day loop
c       write(nlog,260) iyrmo(mon), xmonam(mon), idy
        fac = mthday(mon)*factor    
        noutSP=0

c
c _________________________________________________________
c
c               Step 2; Set data based on time step
c               iday   = 0 monthly model
c                        1 daily model

      if(iday.eq.0) then
        imd=1
        fx=mthday(mon)*factor
        if(ichk.eq.4) write(nlog,*) ' Vircom; Step 2 Call Virset(0)'
        call virset(0)
      else
        imd=mthday(mon)
        fx=factor
        call dayest(iin,i12)
c
c rrb 2006/10/20; Print header to daily naturalized flow information (*.xbx)
        write(74,121) ' CFS', (i, i=1,nout+1)
        write(70,121) 'ACFT', (i, i=1,nout+1) 
      endif

c
c =========================================================
c               DAILY LOOP
c
      do 330 idy=1,imd
c       write(nlog,*) ' '
c       write(6,*) '  Vircom; month & day', mon, idy
        write(6,260) iyrmo(mon), xmonam(mon), idy
        call flush(6)

        ido=ido+1
        if(ido.gt.ndlymx) ido=1
c
c ---------------------------------------------------------
c		Set daily data	  
c               iday   = 0 monthly model
c                        1 daily model

      if(iday.eq.1) then
        call virset(1)
        if(ichk.eq.4) write(nlog,*) ' Vircom; back from Virset(1)' 
      endif
c
c rrb 00/07/10; Test
c         write(nlog,*) mon, mthday(mon)

      IMO=IMO+1         
      IF(IMO.GT.ndlymx) IMO=1
c
c ___________________________________________________________________
c
c               Step 3 - Historic Flow
c               Note data is changed to gains in mdainp based on iopflo
c
      if(ichk.eq.4) write(nlog,*) ' Vircom; Step 3 Historic Flow'

      DO IRU=1,NUMRUN
        ISS=IRUSTA(IRU)
c      
        NDNS=NDNNOD(ISS)
        DO ND=1,NDNS
          qhistox(ISS)=qhistox(ISS)+virinpx(IRU)
          tempq(iss) = qhistox(iss)
          ISS=IDNCOD(ISS)
        end do
      end do
c
c !!!! Begin Naturalized Flow X (Option 9) major adjustments here
      if(ioptio.ne.9) then
c
c ___________________________________________________________________
c
c               Step 4 - Diversions
c
      if(ichk.eq.4) write(nlog,*) ' Vircom; Step 4 Diversions'
c     write(nlog,*) ' Vircom; Step 4 Diversions ', numdiv

      DO 170 ND=1,NUMDIV
        IF(IDIVSW(ND).EQ.0) GO TO 170
        NUI=NDUSER(ND)
        NUE=NDUSER(ND+1)-1
        if(ioutRe.eq.1) write(nlog,*) ' Vircom; ',
     1        iyr, mon, cdivid(nd), nd, nui, nue
        
        IF(NUI.GT.NUE) GO TO 170
        ISS=IDVSTA(ND)
C     
        DO NU=NUI,NUE
c
c rrb 2008/12/10; Revise to account for recharge and use        
cx        IF(diverx(NU).GT.0.00001) then 
          IF(diverx(NU).GT.smalln) then 
            divacux(ISS)=divacux(ISS)+diverx(NU)
c
c rrb 2008/10/29; Include Diversion to Recharge & Use		
            dumR(iss)=dumR(iss)+Drech(nu)
            dumU(iss)=dumU(iss)+Duse(nu)
            
            if(ioutRe.eq.1) write(nlog,*) ' Vircom; ',
     1        iyr, mon, cdivid(nd), nd, nu, iss, 
     1        Drech(nu)*fac, tempDre(iss)*fac,
     1        Duse(nu)*fac, tempUse(iss)*fac            
          else
c
c		Negatives are imports          
            dumz(iss)=dumz(iss)-diverx(nu)
cr           write(nlog,*) ' Vircom; Imports',
cr   1         iyrmo(mon), xmonam(mon), 
cr   1        idy, nd, cdivid(nd), diverx(nu)
          endif
        end do
 170  continue
c
c ---------------------------------------------------------
c              Step 4b Route Diversions Downstream
      DO IS=1,NUMSTA
        ISS=IS
        NDNS=NDNNOD(ISS)
        
        DO ND=1,NDNS
          qhistox(ISS)=qhistox(ISS)+divacux(IS)
          tempd(iss) = tempd(iss)+divacux(is)
          qhistox(iss)=qhistox(iss)-dumz(is)
          tempi(iss) = tempi(iss)+dumz(is)
          tempDre(iss)=tempdre(iss)+dumR(is)
          tempUse(iss)=tempUse(iss)+dumU(is)
          ISS=IDNCOD(ISS)
        end do
      end do
c
c ___________________________________________________________________
c
c               Step 5 - Special Case Wells  with Sprinklers used first
c			    Calculate CU, returns & depletions for wells 
c			    with sprinklers if igwmode=1
c
c                        Note 1) must call before diversions since
c                          returns are a function of IWR which is 
c                          satisfied by sprinklers first
c                        Note 2) datinp.f checks if isprink=1 then
c                         ieffmax=1, itsfile>=1 & iwell=1
c                        Note 3) Mdainp insures we have efficiency,
c                          area, etc to avoid division by zero
      if(isprink.eq.1) then
        do nw=1,numdivw
c
c               Process if structure is on, Max supply is on, and 
c                 has sprinkler acres
          if(idivsww(nw).gt.0 .and. igwmode(nw).eq.1 .and. 
     1                areasp(nw).gt.small) then
          
          if(diverwx(nw).gt.small) then
c
c                       Calculate and pass pumping by sprinklers only
c                       Note D&W (nd) is important since we need IWR
              nd = idivcow2(nw)
              if(nd.eq.0) then
                izero=0
                call coeffa(areasp(nw),areawa(nw), small, cs,
     1                                  izero, iout, nlog,cdividw(nw))
    		      	demspr(nw)= diwrreqw(nw) * cs / 
     1                                 (effmaxs(nw)/100.0)
              else
                izero=0
                call coeffa(areasp(nw),area(nd), small, cs,
     1                                  izero, iout, nlog,cdivid(nd))
                demspr(nw)= diwrreq(nd) * cs / 
     1                             (effmaxs(nw)/100.0) 
              endif
c
c ---------------------------------------------------------
c
              if(ioutSP.eq.1) then
                noutSP=noutSP+1
                if(noutSP.eq.1) write(nlog,130)
              	
                cd=-1.0
                if(nd.gt.0) cd=diwrreq(nd)				      	
                write(nlog,132) noutSP, iyr, mon, 1, cdividw(nw), 
     1            nw, nd, cs, effmaxs(nw),
     1            demspr(nw)*fac, diwrreqw(nw)*fac, cd*fac
              endif
c
c ---------------------------------------------------------
c			Sprinklers First Calculate pumping by sprinklers
c
              divact=amin1(demspr(nw), diverwx(nw))
              demspr(nw)=divact
              
              if(divact.gt.small) then
                nd=idivcow2(nw)
c
c ---------------------------------------------------------	      
c			Note ieff2=1 lets variable efficiency be 
c                          controlled by ieffmax		  		      
                ieff2=1
                ispr =1
                call rtnsecw(divact,retx,rlossX,cuact,
     1               l2,0,nw,nd,ieff2,ispr)
              endif
            endif
          endif
        end do
      endif
c
c ___________________________________________________________________
c
c               Step 6 - Diversion Return Flows, CU, etc.
c
c               Diversion returns
      if(ichk.eq.4) write(nlog,*) ' Vircom; Step 6 Div Returns'

      DO 210 ND=1,NUMDIV
      	IF(IDIVSW(ND).EQ.0) GO TO 210
c
        NUI =NDUSER(ND)
        NUE =NDUSER(ND+1)-1
        IF(NUI.GT.NUE) GO TO 210
C
        DO NU=NUI,NUE
          IF(diverx(NU).gt.small) then
c
c ---------------------------------------------------------	      
c rrb 2008/11/03
c                Adjust total diversion by the amount to recharge.
cx          Done in Virset
cx          DIVACT=diverx(NU)   
cx          DIVACT=amax1(0.0, diverx(NU) - Drech(nu))   
            DIVACT=Duse(nu)
c
c ---------------------------------------------------------	      
c           Note ieff2=1 lets variable efficiency be 
c                          controlled by ieffmax		  
            ieff2=1  
c                
c                       Note for isprink=1 and ieffmax = 1
c                       IWR for this ditch may have been adjusted
c                       down for sprinkler well use in step 5
            l2=0
            
            if(ioutD.eq.1) then
              if(cdivid(nd). eq.'0100503_I   ' .or. 
     1          cdivid(nd).eq.'0100503_D' ) then                  
                write(nlog,*) ' '              
                write(nlog,*) ' Vircom; cdivid(nd) ',iyr,mon,cdivid(nd)
                write(nlog,*) ' Vircom; Calling Rtnsec',nu,divact*fac,
     1            diverx(NU)*fac, Drech(nu)*fac, Duse(nu)*fac   
           
                write(nlog,*) ' Vircom; nd, nui, nue, nu',
     1                  nd, nui, nue, nu
                write(nlog,*) ' Vircom; cdivid(nd) ', cdivid(nd)
              endif  
            endif
     
            call rtnsec(icx,divact,l2,nu,0,nd,ieff2)
            if(ichk.eq.4) write(nlog,*) ' Vircom; Back from Rtnsec'
          endif
        end do
  210 CONTINUE
c
c ___________________________________________________________________
c
c               Step 7 - Well Return Flows
c                       Note all impacts associated with or without 
c                       variable efficiency are handled in subroutine
c                       return.
c			Note idivsww is a well on/off switch
c
      if(ichk.eq.4) write(nlog,*) ' Vircom; Step 7 Well Returns'

      do 220 nw=1,numdivw
cx
cx rrbX      
cx        if(nw.eq.44) then
cx          write(nlog,*) ' Vircom;', iyrmo(mon), xmonam(mon), idy,
cx     1      nw, isprink, igwmode(nw),
c     1      idivsww(nw),diverwx(nw)*fac
cx        endif
        
        if(idivsww(nw).eq.0 .or. diverwx(nw).lt.small) goto 220
c	      
c ---------------------------------------------------------	      
c 			Note ieff2=1 lets variable efficiency be 
c                          controlled by ieffmax		  	       
        ieff2=1
        nd=idivcow2(nw)
c
c _________________________________________________________
c
c               Step 7a Standard naturalized flow
c                    Sprinkler switch off (isprink=0) or
c                    GW mode off (igwmode=0)
        if(isprink.eq.0 .or. igwmode(nw).eq.0) then
          divact=diverwx(nw)
          ispr=0
          call rtnsecw(divact,retx,rlossX,cuact,
     1                           l2,0,nw,nd,ieff2,ispr)
          goto 220
        endif
c
c _________________________________________________________
c
c               Step 7b Maximum Supply Mode
c			   Calculate CU, returns & depletions for 
c			   GW without sprinklers
c
c			   Naturalized flow with variable efficiency and
c                       Sprinkler lands served before SW diversions)
        if(isprink.eq.1 .and. igwmode(nw).eq.1) then
          nd = idivcow2(nw) 
c
c                       Lands served by sprinkler handled in Step 5
c                       (befor diversions).  Therefore adjust demand but 
c                       do not call rtnsecw
          divact=diverwx(nw)
c
c                       Lands served by non sprinkler
          divact=amax1(divact-demspr(nw), 0.0)
c
c ---------------------------------------------------------
c
          if(ioutSP.eq.1) then
            noutSP=noutSP+1
            if(noutSP.eq.1) write(nlog,130)
          
            write(nlog,132) noutSP, iyr, mon, 0, cdividw(nw), 
     1                 nw, nd, cs, effmaxs(nw),
     1                 demspr(nw)*fac, diwrreqw(nw)*fac, cd*fac		
          endif
    
c
c ---------------------------------------------------------
c			Max Supply Approach Pumping by non sprinklers
          if(divact.gt.small) then
            ispr=0
            call rtnsecw(divact,retx,rlossX,cuact,
     1                  l2,0,nw,nd,ieff2,ispr)
          endif
          
          goto 220
        endif
c
c _________________________________________________________
c
c               Step 7c Naturalized flow with variable efficiency and
c                       Mutual Supply Model (Sprinkler lands served 
c                       after SW diversions
c rrb 2009/04/16; Revise to allow isprink=2 to indicate a mutual supply
cx      if(isprink.eq.1 .and. igwmode(nw).eq.2) then
        if((isprink.eq.1 .and. igwmode(nw).eq.2) .or. isprink.eq.2) then
c
c                       Calculate pumping by sprinklers only
c                       Note D&W (nd) is important since IWR is used
          nd = idivcow2(nw)
c
c rrbX; 2009/06/17; Correction          
c rrb           if(nd.eq.0) then
c rrb             izero=0
c rrb             call coeffa(areasp(nw), areawa(nw), small, cs,
c rrb      1        izero, iout, nlog, cdividw(nw))
c rrb             demspr(nw)= diwrreqw(nw) * cs / (effmaxs(nw)/100.0) 
c rrb             
c rrb           else
c rrb             izero=0
c rrb             call coeffa(areasp(nw),area(nd), small, cs,
c rrb      1        izero, iout, nlog, cdivid(nd))
c rrb             demspr(nw)= diwrreq(nd) * cs / (effmaxs(nw)/100.0) 
c rrb           endif               
c rrb           
c rrb           demspr(nw)=amin1(demspr(nw), diverwx(nw)) 
c rrb c
c rrb c ---------------------------------------------------------
c rrb c        Calculate Pumping by Sprinklers
c rrb c rrb 2006/09/13	New CU Approach
c rrb           if(nd.gt.0) then
c rrb            demspr(nw)=amin1(demspr(nw),dIwrGS(nd)/effS(nd))
c rrb           else  
c rrb            demspr(nw)=amin1(demspr(nw),dIwrGSw(nw)/effSw(nw))
c rrb           endif
c
c rrbX; 2009/06/17; Correction
          
          if(nd.gt.0) then
            demspr(nw)=diverwx(nw) * AreaGS(nd)
          else  
            demspr(nw)=diverwx(nw) * AreaGSw(nw)
          endif
c
c ---------------------------------------------------------
c		Detailed output
          if(ioutSP.eq.1) then
            noutSP=noutSP+1
            if(noutSP.eq.1) write(nlog,130)
            write(nlog,132) noutSP, iyr, mon, 1, cdividw(nw), 
     1        nw, nd, cs, effmaxs(nw),
     1        demspr(nw)*fac, diwrreqw(nw)*fac, cd*fac		
          endif
c
c ---------------------------------------------------------
c
c			Mutual Approach Calculate Pumping by Sprinkers
          if(demspr(nw).gt.small) then
           ispr=1
           divact=demspr(nw)
           call rtnsecw(divact,retx,rlossX,cuact,
     1       l2,0,nw,nd,ieff2,ispr)
          endif
c
c ---------------------------------------------------------
c
c
c			Mutual Approach calculate pumping by non sprinkler
          divact=amax1(diverwx(nw)-demspr(nw), 0.0)
          if(divact.gt.small) then
            ispr=0
            call rtnsecw(divact,retx,rlossX,cuact,
     1        l2,0,nw,nd,ieff2,ispr)
          endif
c          
          
          goto 220
        endif
 220  continue         
c
c ___________________________________________________________________
c
c               Step 8 - Reservoir Return Flows (Seepage)
 
c
c rrb 2008/11/03; 
      if(ichk.eq.4) write(nlog,*) ' Vircom; Step 8 Res Returns'


      do nr=1,numres
       
        iplan=iresP(nr)         
        iscd=irssta(nr)          
        seepcfs=Rrech(nr)
        if(ioutRe2.eq.1)
     1   write(nlog,*) ' Vircom; Res Seepage;', nr, nplan, iplan, iscd, 
     1   seepcfs*fac, pctlosRP(nr), rlossR(nr)*fac
          
c
c _________________________________________________________
c 		Call RetnSecRP to to calculate seepage
        if(seepcfs.gt.small) then
          CALL RtnSecRP(iplan, nr, seepcfs, 
     1      pctlosRP(nr), rlossR(nr))
        if(ichk.eq.4) write(nlog,*) ' Vircom; Back from RtnSecRP'
        endif
      end do      
 
 
c
c ___________________________________________________________________
c
c               Step 9 - Route Return flows (diversions, wells & reservoirs)
c                        downstream
c
c ---------------------------------------------------------
c
c               Step 9a; Route monthly returns downstream
c
      if(ichk.eq.4) write(nlog,*) ' Vircom;  Step 9 Route Returns'

      if(iday.eq.0) then
        irecx=(imo-1)*nstrtn
        irecx=irecx
c
c rrb 2009/05/27; Detailed output        
        do irn=1,numsta
          ret(irn)=0.0
          dep(irn)=0.0
        end do
       
        DO IRN=1,NSTRTN
          ISCD=ISTRTN(IRN)
          NDNS=NDNNOD(ISCD)
c
c rrb 2009/05/27; Detailed output
          ret(iscd)=ret(iscd) + retur(imo,irn)   
          dep(iscd)=depl(imo,irn)
C         
          DO NST=1,NDNS
            rtnacux(ISCD)=rtnacux(ISCD)+RETUR(IMO,IRN)
            ISCD=IDNCOD(ISCD)
          end do
c         
          RETUR(IMO,IRN)=0.
c
c __________________________________________________
c rrb 01/03/28; Set return values for reuse 
c               Note daily is set in daymon

c               irec1=irecx+irn
c               write(78,rec=irec1) 0.0,0.0
        end do
      endif
c
c ---------------------------------------------------------
c
c               Step 9b; Route daily returns downstream
c

       if(iday.eq.1) then
        irecx=(ido-1)*nstrtn
        irecx=irecx
       
       	DO IRN=1,NSTRTN
            ISCD=ISTRTN(IRN)
            NDNS=NDNNOD(ISCD)
C         
          DO NST=1,NDNS
            rtnacux(ISCD)=rtnacux(ISCD)+returd(ido,irn)
            ISCD=IDNCOD(ISCD)
          end do
C
          returd(ido,irn)=0.
c
c __________________________________________________
c rrb 01/03/28; Set return values for reuse 
c               Note daily is set in daymon

c              irec1=irecx+irn
c              write(78,rec=irec1) 0.0,0.0
        end do
      endif  
c
c ---------------------------------------------------------
c
c               Step 9c; Adjust downstream nodes for returns
      DO IS=1,NUMSTA
        ISS=IS
        qhistox(ISS)=qhistox(ISS)-rtnacux(IS)
        tempr(iss) = tempr(iss) + rtnacux(is)
      end do
c
c ___________________________________________________________________
c
c               Step 10 - Well Depletion
c               Note: Returns from wells were included above)
c
c ---------------------------------------------------------
c
c               Step 10a; Calculate well depletions
      if(ichk.eq.4) write(nlog,*) ' Vircom;  Step 10 Well Depl'

      if(iwell.gt.0) then
        do nd=1,numdivw
          if(idivsww(nd).gt.0) then
         
            if(diverwx(nd).gt.small) then
              divact=diverwx(nd)
c
c rrb 00/12/26; Note deplete is based on pumping, therefore
c               not impacted by variable efficiency capability

              call deplete(divact,depx,l2,nd)
            endif
          endif
        end do
c
c ---------------------------------------------------------
c
c               Step 10b; Route monthly depletions downstream
        if(iday.eq.0) then
          DO IRN=1,NSTRTN
             ISCD=ISTRTN(IRN)
             NDNS=NDNNOD(ISCD)
C           
            DO NST=1,NDNS
              depacu(ISCD)=depacu(ISCD)+depl(IMO,IRN)
              ISCD=IDNCOD(ISCD)
            end do
C           
            depl(IMO,IRN)=0.
          end do
        endif
c
c ---------------------------------------------------------
c
c               Step 10c; Route daily depletions downstream
        if(iday.eq.1) then
          DO IRN=1,NSTRTN
             ISCD=ISTRTN(IRN)
             NDNS=NDNNOD(ISCD)
C           
            DO NST=1,NDNS
              depacu(ISCD)=depacu(ISCD)+depld(ido,irn)
              ISCD=IDNCOD(ISCD)
            end do
c           
            depld(ido,IRN)=0.
          end do
        endif
c
c ---------------------------------------------------------
c
c               Step 10d; Adjust downstream nodes for depletions
c
        DO IS=1,NUMSTA
          ISS=IS
          qhistox(ISS)=qhistox(ISS)+depacu(IS)
          tempw(iss) = tempw(iss) + depacu(is)
        end do
      endif
c
c ___________________________________________________________________
c
c               Step 11 - Reservoir Storage Change
c		Note delst() is set to cfs from acft
c
c           IF(NUMRES.EQ.0.OR.NRSACT.EQ.0) GO TO 280
      if(ichk.eq.4) write(nlog,*) ' Vircom;  Step 11 Res Storage'

      if(numres.gt.0) then
c
          DO NR=1,NUMRES
           VOLINT(NR)=CURSTO(NR)
           delst(nr) = (resvolx(nr) - volint(nr))/fx
           
           CURSTO(NR)=resvolx(NR)
         
           if(ioutRe.eq.1) then
           if(idy.eq.1 .and. nr.eq.1) then
             write(nlog,110) 
           endif
           
           if(cresid(nr).eq.'6403551     ') then
           write(nlog,111) iyrmo(mon), xmonam(mon), idy, nr,
     1       cresid(nr), fx,resvolx(nr),volint(nr),
     1       delst(nr)*fx 
           endif
          endif  
        end do
      endif  
c
c ___________________________________________________________________
c
c               Step 12 - Account for Reservoir Storage, Evaporation &
c        Seepage
c
      if(ichk.eq.4) write(nlog,*) ' Vircom;  Step 12 Res evap'

      if(numres.gt.0) then  
        call evasec
c       
        DO 270 NR=1,NUMRES        
            IF(IRESSW(NR).EQ.0) GO TO 270
            ISCD=IRSSTA(NR)
            NDNS=NDNNOD(ISCD)
            ISS=ISCD
c         
c	  	      	Route delta storage, evap, & seepage downstream
          DO NST=1,NDNS
c       
c rrb   2008/10/29; Include to Recharge		
            qhistox(iss)=qhistox(iss)+evap(nr)/fx+delst(nr)
            tempe(iss) = tempe(iss) + evap(nr)/fx
            temps(iss) = temps(iss) + delst(nr)
c       
c rrb   2008/10/29; Include Reservoir to Recharge		
             tempRre(iss)=tempRre(iss)+Rrech(nr)
        
c                   if(nr.eq.2) then
c                    write(nlog,112) nr, iss, delst(nr)*fx, temps(iss)*fx
c                   endif
        
            ISS=IDNCOD(ISS)
          end do
  270   CONTINUE
      endif
c
c ___________________________________________________________________
c
c               Step 13 - Account for Soil Moisture
c                       Note does not impact naturalized flow calculations
c                       directly simply add for output
      if(ichk.eq.4) write(nlog,*) ' Vircom;  Step 13 Soil Moisture'

      if(isoil.eq.1 .and. ieffmax.eq.1) then
c
c
c               Call soil moisture to use soil moisture                       
        iw=1
        l2=1
        call soilm(iw,l2,divx)
c              
c                 Total soil moisture for all diversions by stream 
        do nd=1, numdiv
          iss=idvsta(nd)
          qdivsx(iss) = qdivsx(iss) + qdivs(nd)
          qdivsox(iss)= qdivsox(iss) + qdivso(nd)
        end do
c              
c                 Total soil moisture for all wells by stream 
        do nw=1, numdivw
         iss=idvstaw(nw)
c
c rrb 2006/07/31; Correction
          nd=idivcow2(nw)
          if(nd.eq.0) then
            qdivsx(iss) = qdivsx(iss) + qdivsw(nw)
            qdivsox(iss)= qdivsox(iss) + qdivswo(nw)
          endif  
        end do
c
c ---------------------------------------------------------
c               Route downstream
       do is=1,numsta
        iss=is
        ndns=ndnnod(iss)
        
        do n=1,ndns
            tempts(iss)=tempts(iss)+qdivsx(is)
            tempfs(iss)=tempfs(iss)+qdivsox(is)
            iss=idncod(iss)
        end do
       end do
      endif
c
c ___________________________________________________________________
c
c               Step 14 - Sum CU for all diversions
c                       Note does not impact naturalized flow calculations
c                       directly simply add for output
c               Total CU for all Div and D&W structures by stream 
c               Note dcut = CU from SW, GW & Soil for a Div or D&W
c                    dcutw= CU from GW and Soil for a Well Only
c
c ---------------------------------------------------------
c
      if(ichk.eq.4) write(nlog,*) ' Vircom;  Step 14 CU'

      tot=0.0
     
      do nd=1, numdiv
        iss=idvsta(nd)
        qcux(iss) = qcux(iss) + dcut(nd)
c
c rrb 2006/07/31; Add Loss		
        qloss(iss)=qloss(iss) + rloss(nd)	    
c
c		Total for detailed output            
        tot=tot+dcut(nd)*fac
        totA=totA+dcut(nd)*fac
        dum(mon,nd)=dcut(nd)*fac
        dum(13,nd)=dum(13,nd) + dcut(nd)*fac
        
        dumx(mon)=dumx(mon) + dcut(nd)*fac
        dumx(13)=dumx(13) + dcut(nd)*fac
        
        dumA(mon)=dumA(mon) + dcut(nd)*fac
        dumA(13)=dumA(13) + dcut(nd)*fac
c
c rrb 2006/08/03; Detailed output
        if(ioutCU.eq.1) then
          write(nlog,308) ' Diversion', nd, cdivid(nd), 
     1     iyr, mon, dcut(nd)*fac, tot, totA
        endif
      end do
c
c ---------------------------------------------------------
c            
c               Total CU for all Well Only by stream 
c								Total Pumping for all wells
      ioutPuC=0
      do nw=1, numdivw
c
c rrb 2007/02/05; Correction	  
        iss=idvstaw(nw)
        
        nd=idivcow2(nw)
c
c rrb 2006/09/13; Correction	    
cx	    if(nd.ne.0) then
        if(nd.eq.0) then
          iss=idvstaw(nw)
          qcux(iss) = qcux(iss) + dcutw(nw)
c
c rrb 2006/07/31; Add Loss and pumping		
          qloss(iss)=qloss(iss) + rlossW(nw)	      
c
c		Total for detailed output              
          tot=tot+dcutw(nw)*fac
          totA=totA+dcutw(nw)*fac
          dum2(mon,nw)=dcutw(nw)*fac
          dum2(13,nw)=dum2(13,nw) + dcutw(nw)*fac
          	
          dumx(mon)=dumx(mon) + dcutw(nw)*fac
          dumx(13)=dumx(13) + dcutw(nw)*fac
          
          dumA(mon)=dumA(mon) + dcutw(nw)*fac
          dumA(13)=dumA(13) + dcutw(nw)*fac
          		
c
c rrb 2006/08/03; Detailed output              
          if(ioutCU.eq.1) then                
            write(nlog,308) ' Well', nw, cdividw(nw), 
     1        iyr, mon, dcutw(nd)*fac, tot, totA            
          endif							
        endif
     
c
c rrb 2009/04/28; Correction diverwx should not be added
cx     qpump(iss)=qpump(iss) + diverwx(nw)
       qpump(iss)=qpump(iss) + divmonw(nw)
       
       if(ioutPu.eq.1 .and. divmonw(nw).gt.small) then
         jioutPuC=ioutPuC+1
         if(ioutPuC.eq.1) write(nlog,*) ' Vircom; Pumping Details'
         write(nlog,'(a12,1x, 4i5, 2(1x,a12), 20f8.0)')
     1       ' VirCom;    ',
     1       iyrmo(mon), imomo(mon), nw, iss, cstaid(iss), 
     1       cdividw(nw), divmonw(nw)*fx, diverwx(nw)*fx,
     1       qpump(iss)*fx
       endif
      end do
c
c ---------------------------------------------------------
c xxx
c rrb 02/01/07; Total CU for reservoir evap.
      do nr=1,numres
        if(iressw(nr).gt.0) then
          iss=irssta(nr)
cxx           if(evap(nr).gt.small) then
          qcux(iss) = qcux(iss) + evap(nr)/fx
cxx            endif
        endif
      end do
c
c ---------------------------------------------------------
c               Route downstream
      do is=1,numsta
        iss=is
        ndns=ndnnod(iss)
        
        do n=1,ndns
          tempcu(iss)=tempcu(iss)+qcux(is)
c
c rrb 2006/07/31; Add Loss and pumping		
          templ(iss)=templ(iss)+qloss(is)
          tempp(iss)=tempp(iss)+qpump(is)
          iss=idncod(iss)
        end do
       end do
     
c ---------------------------------------------------------
c
c rrb 98/11/07; End naturalized flowX Edits (Still Print *.xbi file)
      endif
c
c ___________________________________________________________________
c
c               Step 15 - Print Daily base flow information (*.xbx)
c                         and daily scratch (74)

      if(iday.eq.1) then
      if(ichk.eq.4) write(nlog,*) ' Vircom; Step 15 Call Virout(5)'
        call virout(5,irec)
      endif
c
c _________________________________________________________
c
c               Step 16; Store negative data for both monthly and daily
c               and data about loosing reaches (negatives)
c               For gain approach, allow to stay
c               For total flow approach, set to zero
      if(ichk.eq.4) write(nlog,*) ' Vircom; Step 16 Negative Processing'
      do 310 iru=1,numrun
        iss = irusta(iru)
        temph(iss)=qhistox(iss)
        tempno(iss)=qhistox(iss)
c
c                       Set Missing Data to -999 regardless of what
c                       the naturalized flows were estimated to be.
c rrb; 00/07/10; round off concern
c             if(iopflo.eq.1.and.
c    1           abs(tempq(iss)*fac+999.).lt.0.001)  then

c
c rrb 03/03/03; Revise roundoff again
c             if(iopflo.eq.1.and.
c    1          abs(tempq(iss)*fx+999.).lt.smallx) then
           
            ic = ifix((tempq(iss)*fx-0.5))
            if(iopflo.eq.1.and. ic.eq.-999) then
              qhistox(iss) = -999./fx
              goto 310
            endif
c
c                       Identify negative naturalized flow at gages
            if(qhistox(iss).lt.-0.001) then
              ineg(iss) = ineg(iss) - 1
c
c                       Store negatives by units
c                       Monthly = af/mo; Daily = af/day
c               qneg(iss) = qneg(iss) + qhistox(iss)*fx
               if(iday.eq.0) then
                 qneg(iss) = qneg(iss) + qhistox(iss)*fx
               else
                 qneg(iss) = qneg(iss) + qhistox(iss)*factor
               endif
               
c               write(nlog,*)'  Vircom;', mon, iss,
c    1                     ineg(iss),qhistox(iss)*fx
c
c rrb 95/01/02; No negatives allowed when in a total flow mode 
          if(iopflo.eq.1) then
          	qhistox(iss) = 0.0
          	tempno(iss) = 0.0
          endif
        endif
c
c rrb 99/06/22; Additional check against negatives.
c               Note by doing here only count if < -0.001
        if(iopflo.eq.1) qhistox(iss)=amax1(0.0,qhistox(iss))
  310   continue
c     endif
c
c ___________________________________________________________________
c
c               Step 17 - Ungaged Base Flows
      if(ichk.eq.4) write(nlog,*) ' Vircom; Step 17 Unaged flow'
      
      call virnod
c
c ___________________________________________________________________
c
c               Step 18 - Gain Adjustment (if required)
c
c rrb 11/22/95; For gain flows (iopflo=2) Revise flow at gages
c               Note for iopflo=2 at this point
c               qhisto is gain at all points
c               except gauges.  Therefor store total at gages in
c               array virinp, then calculate gain at gage
c
      if(ichk.eq.4) write(nlog,*) ' Vircom; Step 18 Gain Adjustment'

      if(iopflo.eq.2) then                                      
        call virgain
      endif
c     
c                 Calculate monthly totals for daily model
      if(iday.eq.1) then
        call virset(2)
c       write(nlog,*) ' Vircom; back from Virset(2)'
      endif
c
c               End day loop
  330 continue
c
c __________________________________________________________
c
c               Step 19; Print daily results and
c                        set monthly running totals to standard 
        if(iday.eq.1) then
          if(ichk.eq.4) write(nlog,*) ' Vircom; Step 19a Call Virout(3)'
          call virset(3)
c         write(nlog,*) ' Vircom; back from Virset(3)'
          
          call virset(5)
c         write(nlog,*) ' Vircom; back from Virset(5)'
          
          
          call virout(3,irec)
          call virout(1,irec)
        endif
c                               
c               Print monthly naturalized flow information (*.xbi) and
c               Set monthly data for annual printout
        if(iday.eq.0) then
        if(ichk.eq.4) write(nlog,*) ' Vircom; Step 19b Call Virout(1)'
        
          write(70,121) 'ACFT', (i, i=1,nout+1)
          call virout(1,irec)

          if(ichk.eq.4) write(nlog,*) ' Vircom; Back from Virout(1)'
          call virset(4)
          if(ichk.eq.4) write(nlog,*) ' Vircom; Back from Virset(4)'
          
c
c rrb 2007/09/18; Print Monthly Structure Summary Binary          
          call outXssMo(numstax)
          if(ichk.eq.94) write(nlog,*) ' Vircom; Back from OutXssMo'
       
        endif          
c
c __________________________________________________________
c               End month loop 
        if(ichk.eq.4) write(nlog,*) ' Vircom; End month loop'
        
        if(ioutRtn.ge.1) then
          write(nlog,520) 
            rett=0.0
            dept=0.0
            iprint=0
          
          do is=1,numsta
            c=abs(ret(is)) + abs(dep(is))
            if(c.gt.small .or. is.eq.numsta) then
              iprint=iprint+1
              retT=retT+ret(is)
              depT=depT+dep(is)
              
              if(ioutRtn.eq.1) then            
                write(nlog,522) iyrmo(mon), xmonam(mon), iprint, is,
     1          cstaid(is),ret(is)*fac, dep(is)*fac,retT*fac,depT*fac
              endif
            endif

          end do          
          
          write(nlog,'(a9,i5,1x,a4, i5,6x,a12,16x,20f8.0)') 
     1      '  VirCom;',iyrmo(mon),xmonam(mon),iprint,
     1      'Total       ', retT*fac, depT*fac
        endif
c
c __________________________________________________________
c               End month loop 

  340 continue
    
      if(ioutcu.ge.1) then
        write(nlog,304) (ix,ix=1,12)
        
        do nd=1,numdiv
        	write(nlog,305) nd, iyr, cdivid(nd),   'Diversion    ',
     1      (dum(im,nd), im=1,13)
        end do  
        
        do nw=1,numdivw
          write(nlog,305) nw, iyr, cdividw(nw),  'Well        ',
     1      (dum2(im,nw), im=1,13)
        end do  
c
c		Annual Total        
        write(nlog,303)
        write(nlog,305) -1, iyr, 'Basin Total ', 'Basin Total ',
     1      (dumx(im), im=1,13)
      endif  
      
c
c __________________________________________________________
c
c               Step 20; Once per Year, print monthly results (*.xbm)
c
      call virout(2,irec)
c
c ___________________________________________________________________
c
c               Step 21 - For last year of simulation
c                         Print Average naturalized flow info and
c                         Print negative flow summary
c
      if(iyr.eq.iyend) then
c
c		Annual Total        
        if(ioutCU.ge.1) then
          write(nlog,303)
          
          ry=iyend-iystr+1
          write(nlog,305) -1, -1, 'Basin Ave   ', 'Basin Ave   ',
     1      (dumA(im)/ry, im=1,13)
        endif
      
        call virout(4,irec)
c
c rrb 2007/09/18; Print Structure Summary ASCII for all years
        call outXss
      
      
      endif
c
c ___________________________________________________________________
c
c               Step 22 - Return
c
      RETURN
c
c
c ___________________________________________________________________
c               Formats
c
  110   format(/,
     1  '  Vircom; Year  Mon  idy   nr ID           ',
     1  '        fx   resvolx    volint     delst',/
     1  ' ________ ____ ____ ____ ____ ____________ ',
     1  4(' _________'))
  111   format(9x,i5, 1x,a4, 2i5, 1x,a12,1x, 20f10.2)
  
  112   format(
     1  '  Vircom;   nr iss      delst     temps',/
     1      9x,2i5,20f10.2)
     
  120   format(///,' Naturalized Flow Estimate Information',
     1   ' From ', i5, 1x, a4, ' To ', i5, 1x, a4,/     
     1   ' Note: Annual Average Base Flows have negatives set to zero',/
     1   '       Divert includes diversions from all sources ',
     1          '(priority, storage, exchange, etc.) and uses '
     1          '(irrigate, storage, recharge, ...)',/
     1   '       Return includes returns from diversions & wells',/
     1   '       Wel Dep includes immediate and lagged depletions',/
     1   '       CU does include net reservoir evaporation',/
     1   '       Note: Ground Water storage to maintain streamflow ',
     1          'at or greater than zero (To_From_GW_Stor) ',
     1          'is not included',/)     
     
 130  format(/, '  Vircom; Sprinkler Demand Data',/
     1 '    # Year  Mon Ispr Id             nw   nd      cs effmaxs',
     1 '    demspr  diwrreqW   diwrreq',/
     1 ' ____ ____ ____ ____ ____________ ____ ____ _______ _______',
     1 ' _________ _________ _________')
 132  format(4i5, 1x, a12, 2i5, 2f8.2, 20f10.0)     
     
 121  format(/,'Naturalized Flow Information ', a4,/ 
     1   '                           ',
     1   '    Gauged    Import    Divert    Return      Well',
     1   '     Delta       Net     Total   w/o (-)',' |',
     1   '        To      From                              ',
     1   '    Divert    Divert Reservoir'/
     1   'Year  Mon  Day River ID    ',
     1   '      Flow       (-)       (+)       (-)   Dep (+)',
     1   '   Sto (+)   Evp (+) Base Flow Base Flow',' |',
     1   '     SoilM     SoilM        CU      Loss   Pumping',
     1   '   To Rech    To Use   To Rech River Name',/,
     1   27x, 9('      (', i2,')'),' |',
     1   8('      (', i2,')'), ' (', i2,')', /
     1   3('____ '), '____________', 9(' _________'),  
     1   ' |',8(' _________'),1x, 24('_'))
     
     
 260  format('+', ' Vircom; Year ', i5, ' Month  ', a4, ' Day ', i5)
 262  format(' Vircom; Year ', i5)
 
 303  format(
     1 ' ____ ____ ____________  ____________ ',13(' _______'))
   
 304  format(/, '  Vircom; Detailed CU (from SW, GW and Soil)',/
     1 '    # Year ID            Type         ',12(i8),'   Total',/
     1 ' ____ ____ ____________  ____________ ',13(' _______'))
   
 305  format(2i5, 2(1x,a12,1x), 20f8.0)    
 
 306  format(/, '  Vircom; Detailed CU Report',/
     1 '  Type           # ID           Year  Mon',
     1 '              Cu',
     1 '           CuMon',
     1 '           CuAnn',/
     1 ' ___________ _____ ____________ ____ ____',
     1 3(' _______________'))
 308  format(a12, 1x, i5, 1x, a12, 2i5, 20f16.2)     
 
 520        format(/, '  Vircom; Return & Depletion Detail',/
     1        '  VirCom; Year  Mon    #   is ID         ',
     1        '     Ret     Dep    RetT    depT') 
     
 522        format('  VirCom;', i5, 1x, a4, 2i5,1x,a12,20f8.0)       
      
      END                     



