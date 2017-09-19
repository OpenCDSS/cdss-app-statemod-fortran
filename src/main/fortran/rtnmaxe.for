c
c *********************************************************
c
        subroutine RtnmaxE(
     1   iter, iuse, iri, ire, iscd, ndns, 
     1   small, pctS, pavail, ieff2, cCallBy, corid1)
c
c
c _________________________________________________________
c	Program Description
c
c
c       RtnmaxE; it calculates maximum diversion allowable
c               (pavail) with returns in a given reach using a
c               maximum  efficiency approach. Note:
c		1. Same as rtnmax but it allows a percent of the
c                  source to be included (pctS)
c               2. It is used instead of rtnsec because it 
c                  calculates a maximum, not actual.
c
c		
c
c       Called by: DirectBy, DirectEx, and OOPDiv the        
c               only operating routines that allow a variable 
c               efficiency
c
c _________________________________________________________
c	Update History
c
c
c rrb 04/12/28; Revise to allow return flow reuse by not making it 
c		available to downstream users (see iplan) 
c		
c
c _________________________________________________________
c       Documentation
c
c               iter = iteration (ranges from 1 to 2)
c               iuse = diversion user
c               iri  = return flow pointer to beginning index
c               ire  = return flow pointer to ending index
c               iscd = diversion station (iscd = idvsta(nd)       
c               ndns = number of downstream nodes from diversion 
c               small= small convergecy number
c               pavail=amount available 
c
c               ieffmax = variable efficiency 0=off, 1=on  
c               iday = daily model switch
c               ndnr = number of downstream nodes from return location
c               avwret=temporary array of return %
c               avtemp=temporary array of available flow (avail)
c
c		foret = return flow fraction
c               ipnode(ns)=reuse occurs at node ns 
c               
c
c _________________________________________________________
c	Dimensions
c
        include 'common.inc'
        character cCallBy*12, corid1*12
c
c _________________________________________________________
c		Step 1; Initilze

        f=factor*mthday(mon)
        f=f
c
c		iout = detailed output
c		ioutP=detailed plan output
        iout=0
        ioutp=0
c
c _________________________________________________________
c
c               Step 1; Calculate % return (foret)
c

        if(ieffmax.le.0 .or. ieffmax.eq.2) then
          FORET=1.0-DIVEFF(mon,IUSE)/100.0
c         write(nlog,*) '  RtnmaxE, ieffmax, foret', ieffmax, foret
        else
          foret=1.0-effmax(iuse)/100.0
c         write(nlog,*) '  RtnmaxE, ieffmax, foret', ieffmax, foret, 
c    1                   effmax(iuse)
        endif
c
c               Step 1x; Adjust efficiency to 0 for second iteration
        if(iter.eq.2) foret = 1.0
c
c _________________________________________________________
c
c               Step 2; Set temporary array of return % avwret and 
c               available flow avtemp
c
        DO IS=1,NUMSTA
          AVWRET(IS)=0.
          avtemp(is)=avail(is)
        end do
c
c rrb; 02/07/01; Test currtn impact
c       call dnmfso(maxsta, avtemp, idncod, iscd, ndns, imcd)
c       write(nlog,*) '  RtnmaxE-1 ; avail', avail(imcd)*f

        if(ioprtn.eq.0 .or. irturn(iuse).eq.4) then
        else
          avtemp(iscd)=avtemp(iscd)+currtn(iscd)
        endif
c
c rrb 02/07/01
c       write(nlog,*) '  RtnmaxE-2; avail', avail(imcd)*f

c       write(nlog,*) '  RtnmaxE; avtemp #1 for iuse = ', iuse
c       write(nlog,*) (avtemp(i)*f, i=1,numsta)
c
c _________________________________________________________
c
c               Step 3; STEP THROUGH RETURN FLOWS FOR CURRENT DIVERSION
c

c
        DO 180 IRT=IRI,IRE
c
          IRCD =IRNSTA(IRT)
          NDNR =NDNNOD(IRCD)
          idly=irtndl(irt)
c
c rrb 04/12/28; Add reuse capability
          if(ipnode(ircd).gt.0) then
            iplan=ipnode(ircd)
            if(iout.eq.1) then
              write(nlog,*) ' '
              write(nlog,*) ' RtnmaxE; Plan data for user (iuse)    = ', 
     1          iuse, ' node (ircd) = ', ircd, ' plan (iplan) = ',iplan
            endif
            goto 180            
          endif  
c
c _________________________________________________________
c
c               Step 4; Set delay for month or day 1
c
c rrb 97/10/15; Daily model 
          if(iday.eq.0) then
            FACDLY=DLYRAT(1,IDLY)
          else
            facdly=dlyratd(1,idly)
          endif
c
c _________________________________________________________
c
c               Step 5; Set return fraction
c
c		Chagne for rtnsecE
c         RET=PCTTOT(IRT)*FACDLY/10000.
          RET=PCTTOT(IRT)*FACDLY/10000.*pctS
          
c
c _________________________________________________________
c
c               Step 6; Set return fraction for all downstream nodes
c
          ISS=IRCD
          DO  NS=1,NDNR
            AVWRET(ISS)=AVWRET(ISS)+RET
            ISS=IDNCOD(ISS)
          end do
  180   CONTINUE

c       write(nlog,*) ' '
c       write(nlog,*) '  RtnmaxE; avwret #1 (fraction) for iuse = ',iuse
c       write(nlog,*) (avwret(i), i=1,numsta)
c       write(nlog,*) '  RtnmaxE; avtemp #1 (af) for iuse = ',iuse
c       write(nlog,*) (avtemp(i)*f, i=1,numsta)
c _________________________________________________________
c
c               Step 7; COMPUTE THE AMOUNT FOR THE CURRENT DIVERSION
c 

        ISS=ISCD
        DO 200 NS=1,NDNS
          IF(ABS(AVWRET(ISS)*FORET-1.).LE.small) GO TO 190
          AVWRET(ISS)=AVTEMP(ISS)/(1.0-AVWRET(ISS)*FORET)
          GO TO 200      
  190     AVWRET(ISS)=1.0E10
c
  200   ISS=IDNCOD(ISS)
        
        AVWRET(ISCD)=AVTEMP(ISCD)

c       write(nlog,*) '  RtnmaxE; avwret #2 (af) for iuse = ', iuse
c       write(nlog,*) (avwret(i)*f, i=1,numsta)

c
c _________________________________________________________
c
c               Step 8; FIND THE MIN return downstream
c 

        CALL DNMFSO(maxsta,AVWRET,IDNCOD,ISCD,NDNS,IMCD)

c       write(nlog,*)  '  RtnmaxE; imcd = ', imcd
c
        PAVAIL=AVWRET(IMCD)

c       write(nlog,*) '  RtnmaxE, pavail = ',  pavail*f

        pavail=amax1(0.0,pavail)
        if(iout.eq.1) then
          write(nlog,*) '  RtnmaxE, ieffmax, pavail', ieffmax, pavail*f
        endif  
c
c _________________________________________________________
c
c               Step 9; Return
c
        return
        end
