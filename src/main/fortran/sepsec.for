c
c *********************************************************
c
      SUBROUTINE SEPSEC(SeepT, iall, cSub)
c
c _________________________________________________________
c	Program Description
c	
c       Sepsec; It calculates reservoir seepage.
c
c 		Note it uses a counter (nsepcal(nr) to limit
c		seepage to 1) Once per time step or 2) to
c		the change in seepage between iterations within
c		the same time step.  Without this counter
c		a resevoir will keep seeping each time called
c
c
c _________________________________________________________
c	Update	History
c
c	2006/04/11 Revised to handle recharge pits, etc.
c		     by providing ability to route seepage to
c 		     a plan and river based on return flow data.
c		     SEE CALL RTNSECRP()
c		   Also treat as a loss if return flow data is not
c		     provided.
c		   Also define seepage to be af/month. Therefore
c		     divide by number of days for a daily model
c		   Also correct seepage treatment that removed it
c		     from the stream
c		
c _________________________________________________________
c
c	Documentation
c      SEPACT(NR)      Seepage for reservoir nr this iteration
c	     sepact1(nr)     Seepage for reservoir nr prior time step
c	     iall            0 do all reservoirs at once
c	                     1 do reservoir iall only
c	     csub            Subroutine that called this routine
c      nSepCal(nr)     Counter to limit seepage calculations
c                      to once per time step or for change in 
c			                 seepage within a time step
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
c      
      dimension cursto1(555), sepact0(555)
      character cSub*12, cProg*12
c
c _________________________________________________________
c		Step 1; Initilize
c
c		iout=1 details
c		     2 summary
      iout=0
      iwhy=0
      
      ioutR=10
      iprintRP=0
      
      if(iday.eq.0) then
        fac=mthday(mon)*factor
        fday=1.0        
      else
        fac=factor
        fday=mthday(mon)
      endif
      
      small=0.0001
      seepT=0.0
      seep=0.0
      cProg='  SepSec'
c _________________________________________________________
c
C
c rrb 2008/09/30; Allow this routine to be called multiple times
cx    DO 150 NR=1,NUMRES
      if(iall.eq.0) then
        nrb=1
        nre=numres
      else
        nrb=iall
        nre=iall
      endif
      
      if(iout.eq.1) then
        write(nlog,*) ' '      
        write(nlog,*) '______________________________________________'
        write(nlog,*) ' SepSec; nrb, nre, iwhy', nrb, nre, iwhy
      endif
      
      DO 150 NR=nrb, nre
        nSepCala=nSepCal(nr)
        nSepCal(nr)=nSepCal(nr) + 1
        
        nSepCalb=nSepCal(nr)
        cursto1(nr)=cursto(nr)
        sepact0(nr)=sepact1(nr)
C
        IF(IRESSW(NR).EQ.0) then
          if(iout.eq.1. and. ioutR.eq.nr) iwhy=1
          GO TO 150
        endif
C
C------  COMPUTE ACTUAL SAEEPAGE
C
        NRA=NRANGE(NR)
c _________________________________________________________
c
C
        CALL INTERP(SEPCON(1,NR),SEPAGE(1,NR),CURSTO(NR),
     1              Seep,NRA, 2, nr, cresid(nr))
c
c ---------------------------------------------------------
c rrb 2008/09/30; Allow this routine to be called multiple times
        seep1=seep
        if(nSepCal(nr).gt.1) then
          seep=amax1(0.0, seep - Sepact1(nr))
        endif

        IF(seep.LT.small) then
          if(iout.eq.1. and. ioutR.eq.nr) then
            iwhy=2
            write(nlog,*) ' SepSec; iwhy=2 ', 
     1        iwhy, nr, seep1, Sepact1(nr), seep
          endif
          GO TO 150
        endif
c
        seep = seep/fday
        seepT=seepT+seep
c
c rrb 2006/10/17; Correction
        seep=amin1(cursto(nr), seep)        
        cursto1(nr)=cursto(nr)
        CURSTO(NR)=CURSTO(NR)-seep
        SepAct(nr) = SepAct(nr)+Seep
C
c _________________________________________________________
c
c		Distribute to accounts
        IRI=NOWNER(NR)
        IRE=NOWNER(NR+1)-1

        SeepT=Seep

        SUM=0.
        DO IR=IRI,IRE
          SUM=SUM+AMAX1(0.,CUROWN(IR))
        end do
c
c _________________________________________________________
c              Distribute seepage based on current ownership
c
        IF(SUM.LE.small) go to 130
        
        DO 120 IR=IRI,IRE
          C=SeepT*AMAX1(0.,CUROWN(IR))/SUM
          IF(CUROWN(IR).LE.small) GO TO 120
          CUROWN(IR)=CUROWN(IR)-C
          IF(CUROWN(IR).GE.0.) GO TO 110
          C=C+CUROWN(IR)
          CUROWN(IR)=0.
  110     SeepT=SeepT-C
          accr(25,ir) = c
  120   CONTINUE
c _________________________________________________________
c              Distribute remaining seepage based on availablity
c
  130   IF(SeepT.LE.small) go to 150
        
        IRR=IRE+1
        DO 140 IR=IRI,IRE
          IRR=IRR-1
          IF(CUROWN(IRR).LE.small) GO TO 140
          CUROWN(IRR)=CUROWN(IRR)-SeepT

          IF(CUROWN(IRR).GE.0.) then
            accr(25,irr) = accr(25,irr) + seepT
            GO TO 150
          endif                     

          accr(25,irr) = accr(25,irr)+seepT+curown(irr)
          SeepT=-CUROWN(IRR)
          CUROWN(IRR)=0.
  140   CONTINUE

        CUROWN(IRI)=CUROWN(IRI)-SeepT
        accr(25,iri) = accr(25,iri) + SeepT
c
c _________________________________________________________
c
c rrb 2006/04/12; Reservoir Seepage Return Flow Calculations
c		Return Flow calculations
  150 CONTINUE
c
c _________________________________________________________
c		Calculate reservoir return flows
cx    do nr=1,numres

      do nr=nrb,nre
       
        iplan=iresP(nr)         
        iscd=irssta(nr)          
        seep1=sepact(nr)
        seep0=seep1
        
        if(nSepCal(nr).gt.1) then
          seep1=amax1(0.0, sepact(nr) - Sepact1(nr))
        endif

c
c _________________________________________________________
c 		Call RetnSecRP to accumulate seepage to a plan
c		     or loss as appropriate          
        
        if(iout.eq.1 .and. ioutR.eq.nr) then
          write(nlog,*) ' SepSec; befor RtnSecRP', 
     1      iwhy, nr, nSepCal(nr), 
     1      seep0, sepact(nr), sepact1(nr), seep1
        endif   
        
        if(seep1.gt.small) then
          seepcfs=seep1/fac          
          CALL RtnSecRP(iplan, nr, seepcfs, 
     1      pctlosRP(nr), rlossR(nr))
        else
          if(iout.eq.1. and. ioutR.eq.nr) iwhy=3
        endif
        
        if(iout.eq.1 .and. ioutR.eq.nr) then
          write(nlog,*) ' '
          write(nlog,*) ' SepSec; after RtnSecRP', 
     1      iwhy, nr, nSepCal(nr), 
     1      seep0, sepact(nr), sepact1(nr), seep1
        endif        
c
c		Set sepact1 to current seepage
        sepact1(nr)=sepact(nr)          
      end do      
      
c
c _________________________________________________________
c
c		Detailed output   
      
      if(iout.ge.1) then
        
        if(iprintRP.eq.0) then
           write(nlog,270) cSub, iall, iplan
          iprintRP=iprintRP+1
        endif
c        
        do nr=nrb,nre              
          c=sepact(nr)-rlossR(nr)*fac  
c        
c rrb 20 08/09/29; Test          
          if(iout.ge.1 .and. nr.eq.ioutR) then
            iplan1=iplan
            
            if(iplan.gt.0) then
              write(nlog,280) cProg, iyrmo(mon),xmonam(mon), idy, 
     1        cresid(nr),pid(iplan1), iwx,nr, iplan1,iwhy, 
     1        cursto1(nr), cursto(nr), 
     1        sepact0(nr), sepact(nr), sepact1(nr), c, rlossR(nr)*fac,
     1        psuply(iplan1)*fac, psuplyT(iplan1)*fac,
     1        pdrive(iplan1)*fac
            else
              write(nlog,280) cProg, iyrmo(mon),xmonam(mon), idy,
     1        cresid(nr),'NA          ', iwx,nr, iplan1, iwhy,
     1        cursto1(nr), cursto(nr), 
     1        sepact0(nr), sepact(nr), sepact1(nr), c, rlossR(nr)*fac,
     1        -1.0, -1.0, -1.0
            endif
          endif
        end do
      endif  
c
c _________________________________________________________
c               Return
c
      RETURN
c
c _________________________________________________________
c		Formats
 270  format(/, '  SepSec; Called by: ',a12, ' with Iall =', i5,
     1  ' and Iplan = ', i5/     
     1  '  Called By   iyr mon   day Res ID       Plan ID     ',
     1  '  iwx   nr ipln iwhy',
     1  '   CurSto1   CurSto2   SepAct0    SepAct   SepAct1',
     1  '   Tot Ret  Tot Loss    Psuply   PsuplyT    Pdrive',/
     1  ' ___________ ____ ____ ____ ____________ ____________',
     1  ' ____ ____ ____ ____',
     1  ' _________ _________ _________ _________ _________',
     1  ' _________ _________ _________ _________ _________')
 280  format(a12, i5, 1x, a4,i5, 1x,a12, 1x, a12, 4i5, 20f10.1)     
      
      END
