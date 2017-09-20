c
c *********************************************************
c
      SUBROUTINE IfrRigSP(IW,L2,ISHORT,divactX,ncallx)
c
c
c _________________________________________________________
c	Program Description
c
c 	Type 50; South Platte Compact to a Plan 
c		 Similar to DirectBy but it allows the source to be
c		 an instream flow.  Also the diversion occurrs at the
c    destination, not the source right location (effectively
c    reassigns the water right to the destination)
c
c		Source 1 is a ISF water right iopsou(1
c		Destination 1 is a plan
c
c _________________________________________________________
c
c	Update History
c		   Copied DirectBY; revised to handle an instream flow
c		     source and simplify for an ISF source
c
c _________________________________________________________
c
c      Documentation
c
c        IW : OVERALL WATER RIGHT ORDER
c        L2 : LOC. OF operation right  in opr RIGHT TABLE
c
c        lr             source water right pointer
c        iopsou(1,l2)   source water right
c        iopsou(2,l2)   Not used (note % ownership gets transfered
c			                  to oprpct in Oprinp.f)
c        iopsou(3,l2)   Plan id (for return flow obligation accounting)
c        iopsou(4,l2)   CU switch 
c                       0=No limit by CU, 1= limit by CU at source
c        iExPoint(l2)   bypass point (e.g. its a pointer the
c                       bypass point on the Stream
c			   Calculated in Oprinp via call getExPt
c        iopsou(7,l2)   bypass point (e.g. its a pointer the
c
c	       oprpct(l2)     Percent of the source water right to be bypassd
c	
c
c        iopdes(1,l2)   if > 0 and < 10000 destination diversion ID 
c        iopdes(1,l2)   if > 10000 destination plan ID 
c        iopdes(2,l2)   destination owner 
c
c        iopdes(1,l2)   if < 0 destination reservoir ID 
c        iopdes(2,l2)   destination reservoir account

c        nd             source diversion ID
c        iscd           idvsta(l2) stream ID of source diversion (nd)
c        ndns           # of nodes downstream of source diversion (nd)
c        iuse          source user
c
c	       nc             Diversion for carrier #1
c
c        nd2            iopdes(1,l2) = if>0 and < 10000, destination 
c                         diversion ID
c        ndd2           iopdes(1,l2) = if<10000, destination Diversion
c			                    np2 = iopdes(1,l2)
c        np2            iopdes(1,l2) = if>10000, destination plan
c			                    np2 = iopdes(1,l2) - 10000
c        nr2            iopdes(1,l2) = if>0, destination reservoir ID

c        idcd2          stream ID of destination diversion (nd2) or reservoir
c		                    	or plan or CARRIER
c        idcd2X         stream ID of destination diversion (nd2) or reservoir
c			                    or plan (NOT CARRIER)
c        idcd2D         stream ID of destination diversion 
c        idcd2R         stream ID of destination reservoir
c        idcd2P         stream ID of destination plan
c        idcd2C         stream ID of destination carrier
c        ndns2          # of nodes downstream of destination 
c                       diversion (nd2) or reservoir
c        iuse2          destination user 
c
c	       imcdX          pointer to avail array. It chnages from 1 (to allow
c                       debug printout to idcd2 (flow at destination) to
c                       imcd flow at the minimum location
c
c	       internT        Intervening structure type
c		      	            1 = Carrier
c		      	            2 = River
c                          icx            subroutine call # (10) for I/O in rtnsec
c                          IW             Global water right counter
c                          L2             Operational right pointer
c                          ishort         code for reoperation; 0=no, 1=yes
c
c	       CuLimit        fraction to be diverted (diversion or depletion)
c	       TcLimit        fraction to apply to T&C requirement
c
c
c        divactB        diversion by the destination (by bypass) structure
c	       divact1        diversion at source structure
cc
c	       divreq1        Demand at source 
c
c        dcrdivS        Fraction of water right available to the source
c			                  Set in oprinp
c	       divdS          Amount diverted at source in previous iterations (cfs)
c	       dcrdiv1        Remaining water right at source source (cfs)
c        
c	       dcrdivE        Fraction of water right availabe to the ByPass
c	       divdE          Amount diverted at source in previous iterations (cfs)
c	       dcrdiv2        Remaining water right at bypass (cfs)
c
c        divcap         Structure capacity
c        divmon         Capacity diverted in previous iterations
c
c        idvsta(l2)     STATION WHERE DIV. RIGHT L2 LOCATES
c
c        idivco(1,l2)  = structure associated with water right l2
c
c        ieff2         =0 always use average efficiency
c                      =1 let ieffmax control variable efficiency 
c        ioprtn         Switch for handling return flows when
c                       multiple structures exist at 1 location
c                       currently always set to 1.0 in Datinp.f
c        iout           Switch: 0 no print; 1 print details, 2 summary
c
c        ndnnod(iscd)   Number of downstream nodes
c        ndnr           Number of downstream nodes from return
c                       (ndnr = f (ndnnod)) 
c
c        nCarry         0 No carrier
c		                    1 No return to River, Final Destination is
c			                    from a carrier
c	                      2 Return to River, Final Destination is
c                         from a carrier
c		                    3 Return to River, Final Destination is 
c			                    from the river

c        qdiv(5, )      From River by priority (In Basin)
c        qdiv(8, )      From River by priority (Transmountain)
c        qdiv(14 )      Div by an instream flow (e.g. ifrrig)
c
c        qdiv(18        Carrier passing thru a structure (e.g. divcar)
c        qdiv(20        From Carrier by Storage or Exchange (e.g. carrpl)

c        qdiv(26, )     From River by Other
c        qdiv(28, )     Carrier passing thru a structure by a Plan
c        qdiv(32, ) 	  Carrier Loss by a carrier
c	       qdiv(33, ) 	  Carrier loss to a destination 
c
c     
c        qres(4  	From Carrier by Sto_Exchange
c        qres(18 	From river by Exch_Plan
c
c
c        currtn         Immediate return to diverting node??
c        qtribu         Tributary InFlow (baseflow point)
c        qstern         Flow at diversion (used for transmtn divs)
c        small          a small value for roundoff (0.0) concerns
c
c	       pdem(ip)       Obligation to a plan (ip)
c			                  Note calculated in RtnsecP
c
c_______________________________________________________________________
c	Dimensions
      include 'common.inc'
      character 
     1  cwhy*48, cdestyp*12, ccarry*3, cpuse*3, csour*12,
     1  rec12*12, cresid1*12, cTandC*3, criver*12, 
     1  corid1*12, cCallBy*12, cstaid1*12, ctype1*12, cImcdR*12,
     1  cwhy2*48
c_______________________________________________________________________
c               Step 1; Common Initilization
c		  iout=0 no detials
c		     1 details
c		     2 summary
c     write(nlog,*) ' IfrRigSP; nplan', nplan     
      isub=50
      iout=0
      ioutiw=0
      if(ichk.eq.150) iout=2
      if(corid(l2).eq. ccall) ioutiw=iw
      corid1=corid(l2)
      cstaid1=corid1
      cCallBy='IfrRigSP    '      
cx      if(iout.ge.1 .and. ioutiw.eq.iw .and. ncallx.eq.0) then
cx        write(nlog,102) corid(l2), iout, ioutiw, iw
cx 102    format(/, 72('_'),/ '  IfrRigSP; ID = ', a12, 5i5)
cx      endif         
cx      
      if(iout.eq.1) then
        write(Nlog,*)
     1    ' IfrRigSP; ncallx    ichk    iout  ioutiw      iw',
     1    ' corid        ccall' 
        write(nlog,'(10x,5i8,2(1x,a12))')  
     1    ncallx, ichk, iout, ioutiw, iw, corid(l2), ccall
      endif
c_______________________________________________________________________
c               b. Daily capability
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif
c_______________________________________________________________________
      ioff=0
      icase=0      
      nr2=0
      np2=0      
      ISHORT=0
      ieff2=1
      icx=25
      small = 0.001
      iscd=-1      
      imcdX=1 
      nRiver=0
      criver='NA'
c_______________________________________________________________________
      ndtype = iopDesR(l2)
      if(ndtype.eq.2) cdestyp='Reservoir'
      if(ndtype.eq.3) cdestyp='Diversion'
      if(ndtype.eq.7) cdestyp='Plan     '      
c_______________________________________________________________________
c		Output variables      
      iwhy=0
      cwhy='NA'
      iwhy2=0
      cwhy2='NA'
      cImcdR='NA'
c_______________________________________________________________________
      divalo  = -1.0/fac
      divByP  = -1.0/fac
      pavail  = -1.0/fac
      pdem1   = -1.0/fac
      pdem2   = -1.0/fac
      divCU   = -1.0/fac
      divCarry= -1.0/fac
      alocfsR =-1.0/fac      
      dcrdiv1=-1.0/fac
      divreq1=-1.0/fac
      psuply1=-1/fac
      oprEffT =1.0
      OprEffTS=1.0
c_______________________________________________________________________
      divactB = 0.0     
      divact1 = 0.0
      divact0=0.0
      divMore=0.0
      divAdd=0.0
      divact  =0.0
      divactX =0.0
      pfail1  =0.0
c_______________________________________________________________________
      cpuse='No'
      cTandC='No'
      csour='NA'
      nc=0
      ncnum=0
c_______________________________________________________________________
      if(iopsou(3,l2).gt.0) cpuse='Yes'
      cdestyp='NA'      
      nd2=iopdes(1,L2)
      if(iopDesR(l2).eq.7) cdestyp='Plan' 
c_______________________________________________________________________
c		f. Standard Carrier   
      ncarry=0   
      ccarry='No'
c_______________________________________________________________________
c               l. Check Avail array coming in
c     if(iout.eq.1) write(nlog,*) ' IfrRigSP; Calling Chekava In'
      call chekava(21, maxsta, numsta, avail)
c_______________________________________________________________________
c               Step 2; Exit if not on this month (iwhy=1)
c		2a; Monthly switch off
      if(imonsw(l2,mon).eq.0) then
        iwhy=1
        cwhy='Monthly Switch is Off'
        ioff=1
      endif   
c_______________________________________________________________________
c		2b; For a daily model set demand for beginning of season
      if(iday.eq.1 .and. imonsw(l2,mon).gt.0) then
        if (idy.lt.imonsw(l2,mon)) then
          iwhy=1
          cwhy='Daily switch Off'
          ioff=1
        endif  
      endif  
c_______________________________________________________________________
c		2c; For a daily model set demand for end of season
      if(iday.eq.1 .and. imonsw(l2,mon).lt.0) then
        if (idy.gt.iabs(imonsw(l2,mon))) then
          iwhy=1
          cwhy='Daily switch Off'
          ioff=1
        endif  
      endif  
c_______________________________________________________________________
c		Step 3; Set Source Data (an instream flow right)
      if(iopSouR(l2).ne.11) then
        goto 9999
      else
        lr=iopsou(1,l2)
        nd=iifrco(lr)
        csour=cifrid(nd)            
c       write(nlog,*) '  IfrRigSP_3; iscd, ndns ', iscd, ndns
c_______________________________________________________________________
c		a. Set remaining decree & exit if zero
        dcrdiv1=amax1(0.0, dcrifr(lr) -divo(l2))
        if(dcrdiv1.le.small) then
          iwhy=2
          cwhy='Remaing decree (dcrdiv1) = 0'          
          goto 260          
        endif         
c_______________________________________________________________________
c		b. Exit if source structure is off (iwhy=2)
        if(ifrrsw(nd).eq.0) then
          iwhy=3
          cwhy='Source ISF Right is Off'        
          goto 260
        endif
      endif
c_______________________________________________________________________
c               Step 4; Set destination data 
c		         Destination is a plan
      np2=iopdes(1,L2)
      if(iopDesR(l2).ne.7) then
        goto 9999
      else
        if(iout.eq.1) write(nlog,*)'  IfrRigSP; Plan dest.'
        cdestyp='Plan'
        ndd2=0
        nr2=0
        iuse2=-1
        idcd=ipsta(np2)
c_______________________________________________________________________
c    b; Set the diversion location equal to the destination location
        iscd=idcd
        NDNS=NDNNOD(ISCD)  
c       write(nlog,*) '  IfrRigSP_3; iscd, ndns ', iscd, ndns
c_______________________________________________________________________
c    b; Set demand & Exit if zero
        divreq1=amax1(0.0,flowrq(nd))
        if(iout.eq.1) then
          write(Nlog,*)' IfrRigSP; nd, np2, flowrq', 
     1      nd, np2, flowrq(nd)
        endif        
        if(divreq1.lt.small) then
          iwhy=4
          cwhy='Demand at destination (divreq1) = 0'
          goto 260
        endif    
c_______________________________________________________________________
c		c. Exit if destination structure (np2) is off
        if(pOn(np2).le.small) then
          iwhy=5
          cwhy='Destination Plan is Off'          
          goto 260          
        endif
      endif             
c_______________________________________________________________________
c               Step 5; Check available flow
c			                  from the source down (iscd)
      CALL DNMFSO2(maxsta, AVAIL, IDNCOD, iscd, NDNS, IMCD,
     1  cCallBy)
      imcdX=imcd
      pavail=avail(imcd)
c               Print warning if negative available flow
      IF(pavail.le.(-1.*small)) then
        WRITE(nlog,310) '**IfrRigSP_8',
     1    IYR,MON,IW,NWRORD(1,IW),L2,lr, nd2, iuse2,
     1    idcd2,IMCD,icase, divreq1*fac, divact*fac, avail(imcd)*fac
      endif
      if(pavail.le.small) then
        iwhy=6
        cwhy='Available flow below destination (AvailX) = 0'
        goto 260
      endif
c_______________________________________________________________________
c               Step 6; Calculate allowable diversion (divalo)
c                       at source location based on water right
c	                control with demand, etc later
      divalo=amin1(divreq1, dcrdiv1, pavail)
      divalo=amax1(0.0, divalo)
      divactX=amax1(divalo,0.0)       
c_______________________________________________________________________
c		Turn off iwhy & cwhy related to source diversion
      iwhy=0
      cwhy='NA'     
c_______________________________________________________________________
c               Step 17 Remove destination from avail & River
c                       just like a diversion by calling takout
c                       which adjusts avail and river
        CALL TAKOUT(maxsta, avail ,RIVER ,AVINP ,QTRIBU,IDNCOD,
     1              DIVACTX, NDNS,  ISCD     )
c_______________________________________________________________________
c               Step 20; Double Check available flow 
c jhb try to prevent array out of bounds error on avail(imcd)
c     turns out idcd2 is undefined.  probably should be iscd as before
c      CALL DNMFSO2(maxsta, AVAIL, IDNCOD, idcd2, NDNS2, IMCD,
c     1  cCallBy)
      CALL DNMFSO2(maxsta, AVAIL, IDNCOD, iscd, NDNS2, IMCD,
     1  cCallBy)
      call flush(6)
      call flush(nlog)
c rrb 2008/06/20; Allow minor roundoff
      iavail=avail(imcd)
      IF(AVAIL(IMCD).le.(-1.*small) .and. iavail.gt.-1) avail(imcd)=0.0
c               Print warning if negative available flow      
      IF(AVAIL(IMCD).le.(-1.*small)) then
        WRITE(nlog,310) ' IfrRigSP_20',
     1    IYR,MON,IW,NWRORD(1,IW),L2,lr, nd2, iuse2,
     1    idcd2,IMCD,icase, divreq1*fac, -1., avail(imcd)*fac
c       write(nlog,320) (avail(iss)*fac,iss=1,numsta)
c       write(nlog,330) (river(iss)*fac,iss=1,numsta)
        goto 9999
      endif
c_______________________________________________________________________
c		Step x; Update 
c_______________________________________________________________________
c               Step 22a; Update Source data
c                         qdiv(26, ) From River by Other
      flowrq(nd)   = flowrq(nd)    - divactX
      divi(l2)     = divi(l2)      + divactX
c rrb 2011/05/22; set qdiv(31, From River by Other
c                 (a reuse or Admin Plan) and show as a diversion
c                 in *.xwb
c                 Note iscd = idcd
cx    qdiv(26,iscd)= qdiv(26,iscd)  + divactX  
cx    qdiv(30,iscd)= qdiv(30,iscd) + divactX  
      qdiv(31,iscd)= qdiv(31,iscd) + divactX
c_______________________________________________________________________
c               Step 22b; Update destination 
c                        Destination is a plan
      if(iout.eq.1) write(nlog,*) '  IfrRigSP; Update Plan Data'      
      call flush(nlog)
      psuply(np2)=psuply(np2) + divactX
      psuplyT(np2)=psuplyT(np2) + divactX
      psuply1=psuplyT(np2)
c_______________________________________________________________________
c               Step 22b; Operating Rule
      DIVO(L2)=DIVO(L2)+divactX
c_______________________________________________________________________
c               Step 30; Print detailed results if requested
  260 continue
      iprint=1
cx    if(divactX.lt.small) iprint=0
      if(iout.ge.1 .and. iw.eq.ioutiw .and. iprint.eq.1) then  
        ncallX=ncallX+1
        if(ncallX.eq.1) then
          write(nlog,270) corid(l2), cdestyp, ccarry, cTandC, cpuse,
     1      cDivTyp(l2)                    
        endif  
        write(nlog,280) '  IfrRigSP  ',
     1    iyrmo(mon),xmonam(mon),idy, csour, 
     1    iwx, iw,l2,lr,np2,imcdX, 
     1    divreq1*fac, divalo*fac, dcrdiv1*fac, pavail*fac, 
     1    psuply1*fac, divactX*fac, iwhy, cwhy
  270   format(/, 
     1  '  IfrRigSP (Type 25); Operation Right ID = ', a12,
     1  ' Destination Type = ', a12,
     1  ' Carrier (Y/N) = ',a3, ' T&C Plan (Y/N) = ',a3,
     1  ' Reuse Plan (Y/N) = ', a3,
     1  ' Diversion Type = ', a12/    
     1  '  IfrRigSP    iyr mon   day Source ID   ',
     1  '    Iter      Iw      l2      lr     np2   imcdX',
     1  ' divreq1  divalo dcrdiv1  pavail Psuply1 DivActX',
     1  '    iwhy Comment',/
     1  ' ___________ ____ ____ ____ ____________', 
     1  ' _______ _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______ _______',
     1  ' _______ __________________________')
  280   FORMAT(a12, i5,1x,a4, i5, (1x,a12),6i8,6F8.0,i8,
     1   1x, a48)
      endif
c_______________________________________________________________________
c               Step 32; Set Call for 
c rrb 2008/06/10
c jhb 2014/11/20 add check for case where pavail < small causes a jump
c                to 260 and iscd is not a valid value for an array index
c      if(nd.gt.0) then
      if(nd.gt.0.and.iscd.gt.0) then
        ctype1='Instream'
        call GetCall(iscd, imcdL(iscd), nd, ctype1)        
      endif  
c_______________________________________________________________________
c               Step 33 - Check Avail going out of the routine
      if(iout.eq.1) write(nlog,*) ' IfrRigSP; Calling Chekava Out'
      call chekava(21, maxsta, numsta, avail)
c_______________________________________________________________________
c               Step 34; Return
      RETURN
c_______________________________________________________________________
c               Formats
  281   FORMAT(a12, 143x, i8, f8.0, f8.2, 1x, a24)
  290   FORMAT(/, '  IfrRigSP_0 QDIV ',a12,/,16F7.1)
  300   FORMAT(/, '  IfrRigSP_0 QRES ',a12,/,16F7.1)
  310   FORMAT(/, '  IfrRigSP; Problem negative avail',/
     1  '  IfrRigSP    iyr  mon',
     1  '      Iw  nwrord      l2      lr     ND2   iuse2', 
     1  '   idcd2    imcd   icase divreq1 divactB   avail'/  
     1  ' ___________ ____ ____', 
     1  ' _______ _______ _______ _______ _______ _______', 
     1  ' _______ _______ _______ _______ _______ _______',/
     1 a12, 2i5, 9i8, 20f8.0)
  320   format(/, '  IfrRigSP_0; avail  ',/,(10f10.2))
  330   format(/, '  IfrRigSP_0; river  ',/,(10f10.2))
  332   format(/, '  IfrRigSP_0; qtribu ',/,(10f10.2))
  334   format(/, '  IfrRigSP_0; qstern ',/,(10f10.2))
  340   format(/, '  IfrRigSP_0; Pavail, imcd, stanam ',
     1    f8.2, i8, 1x,a24)
  350   format(/, '  IfrRigSP; Problem with the bypass reach')   
c rrb 2007/05/25; Add carrier Loss      
 400  format('  IfrRigSP;   i ncar',
     1          '  OprLossC  effmaxTX  effmaxT1',
     1          '    divcap   divcapX   CapRemX')         
 410  format(a10, 2i5, 20f10.0)   
c
c_____________________________________________________________
c               Error warnings
c
 9999 write(6,1050) 
      write(nlog,1051) 
c
c ---------------------------------------------------------
c		Print detailed output if a problem      
      write(nlog,270) corid(l2), cdestyp, ccarry, cTandC, cpuse,
     1      cDivTyp(l2)                    
      
      write(nlog,280) '  IfrRigSP  ',
     1    iyrmo(mon),xmonam(mon),idy, csour, 
     1    iwx, iw,l2,lr,np2,imcdX, 
     1    divreq1*fac, divalo*fac, dcrdiv1*fac, pavail*fac, 
     1    psuply1*fac, divactX*fac, iwhy, cwhy
      
      
 1050 format('    Stopped in IfrRigSP',/,
     1       '    See the *.log file')
 1051 format('    Stopped in IfrRigSP')
      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)


      stop 
      END



      
      
