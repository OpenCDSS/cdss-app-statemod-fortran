c
c *********************************************************
C     Last change:  C    20 May 97    0:03 am

      subroutine outbal2(iys,iye,cplot)
c
c
c _________________________________________________________
c	Program Description
c
c       Outbal2; It prints a water balance
c                Same as outbal; but this one includes a GW balance
c
c ____________________________________________________
c       Update History
c
c rrb 2007/02/23; Revised to recognize *.b43 or *.xdd now has
c		  well only lands reported. Also redefined 
c		  column 33 to be salvage. Therefore no longer
c		  need to read the well file (*.xwe). 
c rrb 2007/02/20; Add Carrier to Well Output
c rrb 2006/04/18; Add Reservoir Seepage
c rrb 2005/11/29; Add River dat1(6) and Carrier dat1(10) Loss
c rrb 1999/02/20; Futile call capability, allow 100
c                outflows (one main 99 futile call tribs)
c
c ____________________________________________________
c       Documentation
c
c              dat1(1)  = total demand
c           ** dat1(2)  = IWR demand
c
c              dat1(3)  = from river by priority for 
c                         direct, instream, to carrier
c              dat1(4)  = from river by storage
c              dat1(5)  = from river by exchange
c           xx dat1(6)  = from river loss
c              dat1(7)  = From Well
c
c              dat1(8)  = from carrier priority via priority
c              dat1(9)  = from carrier via storage
c	    xx dat1(10) = from carrier loss
c              dat1(11)  = carried (pass thru) water
c
c           ** dat1(12) = from soil
c              dat1(13) = total supply
c              dat1(14) = total short (cu (divert - all future returns)
c           ** dat1(15) = IWR short
c              dat1(16) = CU from diversion and D&W (not wells)
c           ** dat1(17) = To soil
c              dat1(18) = To Other
c           ** dat1(19) = Loss (lost to system)
c
c              dat1(20) = Upstream flow
c              dat1(21) = Gain 
c              dat1(22) = return flow
c              dat1(23) = well deplete
c              dat1(24) = from/to GW Stor
c
c              dat1(25) = Total Inflow
c              dat1(26) = River Divert
c              dat1(27) = River by well (current month like a diversion
c              dat1(28) = River (Total) Outflow
c              dat1(29) = Avail flow
c
c              dat1(30) = From river to ISF by priority
c              dat1(31) = From river to ISF by storage or
c                         From river to Plan by storage or a reuse plan
c              dat1(32) = From carrier by storage by operation type 3
c              dat1(33) = Salvage
c	             dat1(34) = Source is a reuse or admin plan	  
c              dat1(35) = rid
c              dat1(36) = xstr
c	             dat1(37) = calling ID
c	             dat1(38) = calling right
c
c	             AdjDc    = adjustment from Diversion Carrier (*.xdd)
c	             AdjRc    = adjustment from a Reservoir Carrier (*.xre)
c              AdjRr    = adjustment from a Reservoir Release (*.xre)
c	             AdjP     = plan adjustment (*.xdd)
c
c rrb 2008/01/11; New Id convention as follows:
c		           Strtype (istr1) 1-5000 = Diversion
c                               5001 - 7500 = ISF
c                               7501 - 10000 = Reservoir
c				                        10001 -12500 = Plan
c				                        12501 -15000 = Wells
c                               < -10000 = baseflow only
c                               -1* above = baseflow plus a structure 
c
c ____________________________________________________
c	Dimensions
c
      include 'common.inc'
      dimension ioutx(100)
      character cplot*12
c
c ____________________________________________________
c
c		Step 1; Initilize
c
c		iout=0 no details
c		iout =1 print all adjustments to balance
c		iout =2 print Carrier to Storage adjustments to balance
c		ioutD=1 print total diversion data
c		ioutP=1 print plan data
c		ioutI=1 print inflow data
c   ioutS=1 print to storage data 
c
      iout=0
      
      ioutD=1
      ioutI=0
      ioutP=0
      ioutS=0

      nout=0
      noutD=0
      small=0.01
      ry=iye-iys+1      
      
      write(6,220)
      write(nlog,220) 
c
c-------------------------------------------------------------------
c		Open and Print header to Report File
      if(cplot.eq.'Report      ') then
        call namext(maxfn, filenc, 'Ywb', filena) 
        open(113,file=filena,status='unknown')
        call outtop(113,1,56)        

        call namext(maxfn, filenc, 'Ycu', filena) 
        open(115,file=filena,status='unknown')
        call outtop(115,1,59)                
        write(115,331)
      endif  
c
c
c		Set number of output values 1x in StateM.for
c	        and store in common.inc      
      ndiv = ndivO        
      nres = nresO
      ndivw= nwelO
c
c		Set ndivP number of columns to print before 
c		    call information (control location and right)
c	 	    e.g. column of Avail Flow)	      
c		Set ndivT number of columns to print in title
c		Set ndivF number of columns to weight by factor
c		Set nrid column of rid, strucutre type 
c		Set nxstr column of xstr,number of structures
c		Set nccallR column of call right
c		Set nrimcdX column of call location
c		Set nprint column of data to print (less stuff on right)
c		Set nshort column of total shortage
c
c		The following are based on ndivo=38
c		They must be revised whenever ndivO is revised
      if(ndivO.ne.38) then
        write(nlog,900) 'Diversions', 38, ndivO
        goto 9999
      endif
      
      if(nresO.ne.29) then
        write(nlog,900) 'Reservoirs', 29, nresO
        goto 9999
      endif
      
      ndivP=29
      ndivT=31      
c      
c rrb 2007/01/07; ndivO=38; new variable to track return from a plan       
c     ndivF=33      
c     nrid=34
c     nxstr=35
c     nrimcdx=36
c     nccallR=37
      
      ndivF=ndivO-4
      nrid=ndivO-3
      nxstr=ndivO-2
      nrimcdx=ndivO-1      
      nccallR=ndivO
      
      nshort=14
      nadj=33
      ndelta=17
c
c rrb 2006/01/21; Columns of data impacted by a new Reservoir file size		
c               nbom = column of BOM data (1)
c		nemo = column of EOM data (16)
c		ntar = column of Tartet data (17)
c		none = column of One fill or BOM limit (18)
c		nidr = columns of irdr (account))
c		naccX = columns of nacc ( )
c		nresX = columns to adjust by factor
c               nresY = columns to print     
c
c		nCar2 = column of Carry in reservoir output
c		
c		The follwoing are based on nres0=28
      nBom=1 
      nEva=14
c
c rrb 2006/06/30; Add reservoir seepage            
cr    nSep=13
      nSep=26
c
c rrb 2010/09/15; Add reservoir release
      nRel=13      
      nEom=16
      nTar=17
      nOne=18
      nLos=25
c
      nCar2=24
      
      nidr=nres-2
      naccX=nres-1
      nresX=nres-3
      nresY=nres-4
      
      
      
      iw=27

      AdjDc=0.0
      AdjRc=0.0 
      AdjRr=0.0
      AdjP=0.0
      AdjTp=0.0
      AdjTot=0.0
      AdjTs=0.0
      
c ____________________________________________________
c
c rrb 2009/01/06;  Step 1b. Initilize adjustment averages
        AdjDcT=0.0
        AdjRcT=0.0
        AdjRrT=0.0
        AdjPt=0.0
        AdjtpT=0.0
        AdjTsT=0.0
        AdjSiT=0.0
        AdjTot=0.0      
      
      call outtop(iw,1,3)
c
c rrb 99/05/06; Ground Water balance
c         nb = # of columns in stream system blance output,
c         ng = # of columns in gw balance
      nb=21
      ng=13

      igw=28
      if(iwell.gt.0) call outtop(igw,1,34)
c
c ____________________________________________________
c rrb 10/31/96;
c              Step 1b. Initilize average annual arrays
      do 100 n=1,nb+ng
        do 100 im=1,13
          dum(im,n) = 0.0
  100 continue
c
c ____________________________________________________
c              Step 2. Find outlet station 
      ix=0
      do is=1,numsta
        if(idncod(is).eq.0) then
          ix=ix+1
          ioutx(ix) = is
        endif
      enddo
c
c ____________________________________________________
c              Step 3; Print header if annual data only                        
c               
      if(cplot.eq.'Annual      ') then
        write(iw,250) cunitm, (i,i=1,nb)
        write(iw,260)
      endif
c
c rrb 2007/12/07; Print without monthly titles
        write(ntmp,250) cunitm, (i,i=1,nb)
        write(ntmp,260)                
c      
c ____________________________________________________
c
c		Step 4; Begin year loop
      do 210 iy=iys,iye
        noutD=0
        call year(iy, iyrmo, imomo, cyr1)
c
c rrb 10/27/94 Additional Output
        write(6,130) iy
        call flush(6)
  130   format('+', ' Processing Year = ', i5)
c
c ____________________________________________________
c
c               Step 5; Print header once per year           
        if(cplot.eq.'Annual      ') then
        else
          write(iw,250) cunitm, (i,i=1,nb)
          write(iw,260) 
        endif
c
c ____________________________________________________
c
c rrb 99/05/06; 
c		Step 6; Ground Water balance (*.xgw)
        if(iwell.gt.0) then
c
c               Annual Total output
          if(cplot.eq.'Annual      ') then
            else
            write(igw,255) cunitm, (i,i=1,ng)
            write(igw,265)
          endif
        endif
c
c ____________________________________________________
c
c		Step 7; Initilze annual totals

        tint   = 0.0
        trett  = 0.0
        tgwint = 0.0
        tfrsmt = 0.0
c
c rrb 05/02/18; Plans        
        tplant = 0.0
        tswoutt= 0.0
        
        tswint = 0.0

        tdivt  = 0.0
        twellt = 0.0
        tdept  = 0.0
        tevapt = 0.0
        tsept  = 0.0
        
        toutt  = 0.0
        tstot  = 0.0
        ttosmt = 0.0
        tdelsmt= 0.0

        delt   = 0.0

        tcut   = 0.0
        tlosst = 0.0
        tsalt  = 0.0

c
c rrb 99/05/06; Ground Water Balance
        trecht=0.0
        tpumpt = 0.0        
        tcuxt  = 0.0
        tgwoutt= 0.0
c ____________________________________________________
c
c		Step 9; Monthly loop
        do 200 im=1,12
          nout=0
          if(iout.ge.1) write(nlog,268)
c
c         fac=mthday(im)*factor
          fac=fmo(im)
          tin  = 0.0
          tret = 0.0
          tgwin= 0.0
          tfrsm= 0.0
c
c rrb 05/02/18; Plans        
          tplan= 0.0          
          tswin= 0.0

          tdiv = 0.0
          twell= 0.0
          tdep = 0.0
          tevap= 0.0
          tsep = 0.0
          tout = 0.0
          tsto = 0.0
          ttosm= 0.0
          tdelsm=0.0
          tswout=0.0

          del  = 0.0

          tcu  = 0.0
          tloss= 0.0
          tsal = 0.0
c
c rrb 99/05/06; Ground Water Balance
          tcux = 0.0
          tpump= 0.0
c
c rb 2006/10/27; Diversion Adjustments		          
          AdjDc=0.0
          AdjRc=0.0
          AdjP=0.0
          AdjTp=0.0
          AdjTS=0.0
          
cx          AdjTpT=0.0
cx          AdjTsT=0.0
cx          AdjRcT=0.0
cx          AdjTot=0.0          
c
c ____________________________________________________
c
c		Step 10; Process Station (*.b43, *.xdd) loop
          do 150 is=1,numsta
c
            irecs=((iy-iystr0)*12+(im-1))*numsta+is+numtop
c
            read(43,rec=irecs,err=270) (dat1(i),i=1,ndiv)
c           write(nlog,*) '  Outbal2; 43', (dat1(i), i=1,ndiv)
c
c ---------------------------------------------------------
c		Set structure type
c                               1-5000 = Diversion
c                               5001 - 7500 = ISF
c                               7501 - 10000 = Reservoir
c				                        10001 -12500 = Plan
c				                        12501 -15000 = Wells
            istr=0
            istrT=0
            istr=iabs(istrtype(is))
            if(istr.ge.1     .and. istr.le.5000)  istrT=3
            if(istr.ge.7501  .and. istr.le.10000) istrT=2
            if(istr.ge.10001 .and. istr.le.12500) istrT=7
            if(istr.ge.12501 .and. istr.le.15000) istrT=6

c rrb 2005/11/29; Carrier loss, etc
c           do i=1,ndiv-2
cr          do i=1,ndiv-4

            do i=1,ndivF
              dat1(i)=dat1(i)*fac
            end do
c
c                                                           
c			The following are based on ndivO=37 or 38 or 39
            tin  = tin   + dat1(21)
            tret = tret  + dat1(22)
            tgwin= tgwin + dat1(24)
            tfrsm= tfrsm + dat1(12)
            tpump=tpump+dat1(7)
c
c
c ---------------------------------------------------------
c			Print detailed inflow data            
            if(ioutI.ge.1 .and. is.eq.1) write(nlog,290)
            if(ioutI.ge.1 .and. (abs(dat1(21)).gt.small .or. 
     1       abs(dat1(24)).gt.small)) then
              if(is.eq.1) write(nlog,290)
              write(nlog,291) iyrmo(im),xmonam(im), 
     1         is, cstaid(is), dat1(21), tin, dat1(24), tgwin
            endif
c
c ---------------------------------------------------------
c rrb 2008/01/15; Store From Plan in dat1(34)
c			Note for detailed output there is no adjustment
c			therefore do not add dat1(34) to AdjTot
c
c rrb 2010/09/15; Add back in
cx          tplan=0.0                    
cx          AdjTp=tplan                  
cx          AdjTpT=AdjTpT+tplan          
            tplan=tplan+dat1(34)
            AdjTp=dat1(34)                 
            AdjTpT=AdjTpT+dat1(34)
cx
c rrb 2010/09/15; Detailed output for plan adjustment
c                 Note dat1(34 = qdiv(35 diversion from a reuse
c                 or admin plan source            
cx          if(dat1(34).gt.small .and. iout.ge.1) then
            if(dat1(34).gt.small .and. (iout.ge.1.or.ioutP.ge.1)) then
               nout=nout+1
               write(nlog,269) nout, iyrmo(im), xmonam(im),
     1         'Adding to Plan Data     ', cstaid(is),
     1         is, is, istr, AdjTp, AdjTpT, AdjTot     
            endif
            
            tswin= tin + tret + tgwin + tfrsm + tplan

c
c ---------------------------------------------------------
c	       The following are based on ndivO=39
c              Total diversion = direct from river by priority (3)
c                + river by storage (4) + river by exchange (5)
c                - nonCU by priority (30) - NonCU by storage (31)
c                + carrier by storage (32)
            tdiv = tdiv + dat1(3)  + dat1(4)  + dat1(5) 
     1                  + dat1(32) - dat1(30) - dat1(31)
     
            c =           dat1(3)  + dat1(4)  + dat1(5) 
     1                  + dat1(32) - dat1(30) - dat1(31)
     
            c1=           dat1(3)  + dat1(4)  + dat1(5) 
            c2=           dat1(32) - dat1(30) - dat1(31)
c
c ---------------------------------------------------------              
c rrb 2008/01/11; If the structure is a reservoir (istrT=2)
c                 adjust diversion information in *.xdd
            if(istrT.eq.2) then
c
c		To Storage adjustment
              AdjTs=dat1(3) + dat1(4)  + dat1(5)            
c
              tdiv=tdiv-AdjTs
              
              if (AdjTs.gt.small) then
                AdjTsT = AdjTsT + AdjTs
                AdjTot = AdjTot - AdjTs
              
                if(iout.ge.1 .or. ioutS.eq.1) then
                  nout=nout+1
                  ir1=istr-7500
                  write(nlog,269) nout, iyrmo(im), xmonam(im),
     1              'Adj Div to Storage      ' , cstaid(is),
     1              is, ir1, istr, AdjTs, AdjTsT, AdjTot
                endif
              endif
c
c		End to storage adjustment              
            endif
c
c ---------------------------------------------------------              
c
c rrb 2011/02/25; TEST
cx          if(ioutD.eq.1) then
            if(ioutD.eq.1 .and. istrT.ne.3) then   
              noutD=noutD+1
              nd=istrtype(is)
                if(noutD.eq.1) write(nlog,300)
                  write(nlog,310) iyrmo(im),xmonam(im), 
     1              is, istrtype(is), cstaid(is),c, c1, c2, tdiv,
     1              dat1(30), dat1(31), dat1(32), 0.0
 300          format(/,
     1        '   Yr  Mon   is istrtyp ID          ',
     1        '       c      c1      c2    tdiv',
     1        ' dat1_30 dat1_31 dat1_32 Fr Plan',/
     1        ' ____ ____ ____ _______ ____________',
     1        ' _______ _______ _______ _______',
     1        ' _______ _______ _______ _______')           
 310          format(i5, 1x, a4, i5, i8, 1x, a12, 20f8.0)    
            endif
              
c
c ---------------------------------------------------------
c		Store Diversion Carrier Adjustment
cx            AdjDc=dat1(30)+dat1(31)-dat1(32)            
cx            AdjDct=AdjDct+AdjDc
cx            
cx            if (abs(AdjDc).gt.0.001) then
cx              AdjTot=AdjTot+AdjDc
cx              if(iout.eq.1) then
cx                nout=nout+1
cx                write(nlog,269) nout, iyrmo(im), xmonam(im),
cx     1              'Adj Div to Carrier      ', cstaid(is),
cx     1             is, istr, AdjDc, AdjDcT, AdjTot
cx              endif  
cx            endif
c
c		The following are based on ndivO=38
            twell= twell+ dat1(27)
            tdep = tdep + dat1(23)

            tcu  = tcu  + dat1(16)
            ttosm= ttosm+ dat1(17)            
            tdelsm=tdelsm-dat1(17)+dat1(12)
c
c rrb 99/05/06; Ground Water Balance
            tcux = tcu
c
c rrb 2007/03/28; Carrier losses are not a loss to system
c		  Note if total returns are < 100 then
c		  it shows up ind dat1(19) system loss
cx          tloss= tloss+ dat1(6) + dat1(10) + dat1(19)
            tloss= tloss+ dat1(19)
            tsal = tsal + dat1(33)
c
c rrb; 99/02/20; Futile call capability (multiple outlets)
c           if(is .eq. ix) tout = dat1(28)
            do i=1,ix
              if(ioutx(i).eq.is) tout=tout+dat1(28)
            end do
c
  150     continue
c
c ____________________________________________________
c
c		Step 11; Reservoir (*.xre) data Loop

          if(numres.eq.0.or.nrsact.eq.0) go to 190

          nrsactx=nrsact+numown             
          irt = 0

          do 180 ir=1,numres
            if(iressw(ir).eq.0) go to 180
  160       irt = irt+1
c
c ---------------------------------------------------------
c							 Read Reservoir data
            irecr=((iy-iystr0)*12+(im-1))*nrsactx+irt+numtop
            read(44,rec=irecr,err=270) (dat2(i),i=1,nres)
c           write(nlog,*) '  Outbal2, Res', (dat2(i), i=1,nres)
c
c ---------------------------------------------------------
c              Skip account data
            ida = dat2(nidR)
            if(ida.ne.0) goto 160

c           write(nlog,'(20f8.2)') (dat2(i),i=1,nres)
c
c ---------------------------------------------------------
c		Adjust units
            do i=1,nresX
              dat2(i)=dat2(i)*fac
            end do
c
c ---------------------------------------------------------
c rrb 2006/04/18; Add seepage and seepage loss

            tsep   = tsep  + dat2(nSep)
            tloss  = tloss + dat2(nLos)
            tevap  = tevap + dat2(nEva)            
            tsto   = tsto  + dat2(nEom) - dat2(nBom)
c
c _________________________________________________________
c              Step 11a. Adjust diversion to storage
c              by carrier dat2(nCar2) which is already included in the
c              reservoir delta storage term
c
c rrb 2008/01/08; Test. Back to original
            AdjRc=dat2(nCar2)
            tdiv = amax1(0.0,tdiv-AdjRc)
c
c		Store carrier adjustment data            
            if (abs(AdjRC).gt.small) then
              AdjRcT=AdjRcT+AdjRc
              AdjTot=AdjTot-AdjRc
              is=irssta(ir)      
                     
              if(iout.ge.1) then
                nout=nout+1
                write(nlog,269) nout, iyrmo(im), xmonam(im),
     1              'Adj Res to Carrier      ', cstaid(is),
     1              is, ir, istr, AdjRc, AdjRcT, AdjTot
              endif
            endif
c
c _________________________________________________________
c rrb 2010/09/15; Update
crrb c              Step 11b. Adjust diversion for release from storage
crrb c              which is not part of the reservoir storage change
crrb c              (e.g. when delsto = 0)
crrb             tsto1 = dat2(nEom) - dat2(nBom)            
crrb             if(abs(tsto1).le.small) then
crrb               AdjRr=dat2(nrel)
crrb               if(AdjRr.gt.small) then            
crrb                  tdiv = amax1(0.0,tdiv-AdjRr)
crrb               endif             
crrb c
crrb c		Store Reservoir release adjustment            
crrb               if(AdjRr.gt.small) then
crrb                 AdjRrT=AdjRrT+AdjRr
crrb                 AdjTot=AdjTot-AdjRr
crrb                 is=irssta(ir)      
crrb                      
crrb                 if(iout.ge.1) then
crrb                   nout=nout+1
crrb                   write(nlog,269) nout, iyrmo(im), xmonam(im),
crrb      1                'Adj for Res Release     ', cstaid(is),
crrb      1                is, ir, istr, AdjRr, AdjRrT, AdjTot
crrb                 endif 
crrb               endif             
crrb             endif
c ____________________________________________________
c
c               Step 11c Add reservoir evap to cu for global 
c                        balance (tcu) but not to gw balance
c                        calculations (tcux)
c rrb 01/11/03; Insure it is a positive evaporatoin
            tcu  = tcu + amax1(dat2(nEva), 0.0)
c
  180     continue
c
c
c		Exit if no reservoirs
  190   continue
c
c
c _______________________________________________
c               Step 12; Print Water Balance

        del =  tin  + tret  + tgwin + tfrsm + tplan
     1       - tdiv - twell - tdep - tevap - tsep - tout  - tsto 
     1       - ttosm- tdelsm 

        tswout=tdiv + twell + tdep + tevap + tsep + tout  + tsto 
     1       + ttosm+ tdelsm 

        tint   = tint   + tin
        trett  = trett  + tret
        tgwint = tgwint + tgwin
        tfrsmt = tfrsmt + tfrsm
c
c rrb 05/02/18; Plans        
        tplant = tplant + tplan        
        tswint = tint   + trett + tgwint + tfrsmt + tplant

        tdivt  = tdivt  + tdiv
        twellt = twellt + twell
        tdept  = tdept  + tdep
        tevapt = tevapt + tevap
        tsept  = tsept  + tsep
        toutt  = toutt  + tout
        tstot  = tstot  + tsto

        ttosmt = ttosmt + ttosm
        tdelsmt= tdelsmt+ tdelsm
        tswoutt= tdivt  + twellt + tdept + tevapt + tsept +
     1           toutt  + tstot  + ttosmt + tdelsmt

        delt   = delt   + del

        tcut   = tcut   + tcu
        tlosst = tlosst + tloss
        tsalt  = tsalt  + tsal

        tcuxt  = tcuxt  + tcux
c
c ____________________________________________________
c
c               Step 13; Average
        dum(im,1) = dum(im,1) + tin
        dum(im,2) = dum(im,2) + tret
        dum(im,3) = dum(im,3) + tgwin
        dum(im,4) = dum(im,4) + tfrsm
c
c rrb 05/02/18; Plans        
        dum(im,5) = dum(im,5)+ tplan                
        dum(im,6) = dum(im,6) + tswin

        dum(im,7) = dum(im,7) + tdiv
        dum(im,8) = dum(im,8) + twell
        dum(im,9) = dum(im,9) + tdep
        dum(im,10)= dum(im,10)+ tevap
        dum(im,11)= dum(im,11)+ tsep
        dum(im,12)= dum(im,12)+ tout
        dum(im,13)= dum(im,13)+ tsto

        dum(im,14)= dum(im,14)+ ttosm
        dum(im,15)= dum(im,15)+ tdelsm
        dum(im,16)= dum(im,16)+ tswout
        dum(im,17)= dum(im,17)+ del

        dum(im,18)= dum(im,18)+ tcu
        dum(im,19)= dum(im,19)+ tloss
        dum(im,20)= dum(im,20)+ tpump
        dum(im,21)= dum(im,21)+ tsal
c
c ____________________________________________________
c
c		Step 14; Ground Water Balance
c rrb 99/05/06; Include Ground Water Balance
c rrb 01/08/19; Replace from river by well (twell) with pumping (tpump)
c               Leave loss and include on right side as an outflow
c               Recharge = diversion + pump - CU - Soil Change (tdelsm)
c               + Plan change (tplan)
c
c rrb 05/02/18; Plans        
c       trech   = tdiv   + tpump  - tcux  - tdelsm
        trech   = tdiv   + tpump  - tcux  - tdelsm 
        tgwinx  = trech  + twell  + tdep

        tgwout  = tpump  + tret   + tloss  + tgwin
        tgwdel  = tgwinx - tgwout

        tpumpt  = tpumpt + tpump
c
c rrb 99/05/06; Ground Water balance
c rrb 01/08/19; Replace from river by well (twell) with pumping (tpump)
c               Note no adjustment for loss
c       trecht  = tdivt  + twellt - tcuxt - tlosst
c
c rrb 05/02/18; Plans        
cx      trecht  = tdivt  + tpumpt - tcuxt - tdelsmt 
cx      tgwinxt = trecht + twellt + tdept 
cx      tgwoutt = tpumpt + trett  + tloss + tgwint
        
        trecht  = trecht + trech
        tgwinxt = tgwinxt + tgwinx
        tgwoutt = tgwoutt + tgwout
        

        dum(im,nb+1)= dum(im,nb+1) + trech
        dum(im,nb+2)= dum(im,nb+2) + twell
        dum(im,nb+3)= dum(im,nb+3) + tdep
        dum(im,nb+4)= -1.0
        dum(im,nb+5)= dum(im,nb+5) + tgwinx

        dum(im,nb+6)= dum(im,nb+6) + tpump
        dum(im,nb+7)= dum(im,nb+7) + tret
        dum(im,nb+8)= dum(im,nb+8) + tloss
        dum(im,nb+9)= dum(im,nb+9) + tgwin
        dum(im,nb+10)= -1.0
        dum(im,nb+11)=dum(im,nb+11)+ tgwout
        dum(im,nb+12)=dum(im,nb+12)+ tgwdel
        dum(im,nb+13)=dum(im,nb+13)+ tsal
c
c
c ____________________________________________________
c		Step 15; Annual average
        dum(13,1) = dum(13,1) + tin
        dum(13,2) = dum(13,2) + tret
        dum(13,3) = dum(13,3) + tgwin
        dum(13,4) = dum(13,4) + tfrsm
c
c rrb 05/02/18; Plans                
        dum(13,5)=  dum(13,5)+ tplan        
        dum(13,6) = dum(13,6) + tswin

        dum(13,7) = dum(13,7) + tdiv
        dum(13,8) = dum(13,8) + twell
        dum(13,9) = dum(13,9) + tdep
        dum(13,10)= dum(13,10)+ tevap
        dum(13,11)= dum(13,11)+ tsep
        dum(13,12)= dum(13,12)+ tout
        dum(13,13)= dum(13,13)+ tsto

        dum(13,14)= dum(13,14)+ ttosm
        dum(13,15)= dum(13,15)+ tdelsm
        dum(13,16)= dum(13,16)+ tswout
        dum(13,17)= dum(13,17)+ del

        dum(13,18)= dum(13,18)+ tcu
        dum(13,19)= dum(13,19)+ tloss
        dum(13,20)= dum(13,20)+ tpump
        dum(13,21)= dum(13,21)+ tsal

c
c rrb 99/05/06; Ground Water balance.  Note recharge is diversion (tdiv)
        dum(13,nb+1)= dum(13,nb+1) + trech
        dum(13,nb+2)= dum(13,nb+2) + twell
        dum(13,nb+3)= dum(13,nb+3) + tdep
        dum(13,nb+4)= -1.0
        dum(13,nb+5)= dum(13,nb+5) + tgwinx

        dum(13,nb+6)= dum(13,nb+6) + tpump
        dum(13,nb+7)= dum(13,nb+7) + tret
        dum(13,nb+8)= dum(13,nb+8) + tloss
        dum(13,nb+9)= dum(13,nb+9) + tgwin
        dum(13,nb+10)= -1.0
        dum(13,nb+11)=dum(13,nb+11)+ tgwout
        dum(13,nb+12)=dum(13,nb+12)+ tgwdel
        dum(13,nb+13)=dum(13,nb+13)+ tsal
c
c _______________________________________________
c               Step 16; Print Monthly Balance
c               
        if(cplot.eq.'Annual      ') then
        else
          if(isigfig.eq.0) then                      
            write(iw,230) iyrmo(im),xmonam(im),
     1      tin,    tret,   tgwin,  tfrsm,  tplan,
     1      tswin,  tdiv,   twell,  tdep,   tevap, tsep,
     1      tout,   tsto,   ttosm,  tdelsm, tswout, 
     1      del,    tcu,    tloss,  tpump,  tsal
          endif
          
          if(isigfig.eq.1) then
            write(iw,2301) iyrmo(im),xmonam(im),
     1      tin,    tret,   tgwin,  tfrsm,  tplan,
     1      tswin,  tdiv,   twell,  tdep,   tevap, tsep,
     1      tout,   tsto,   ttosm,  tdelsm, tswout, 
     1      del,    tcu,    tloss,  tpump,  tsal
          endif
          
          if(isigfig.eq.2) then
            write(iw,2302) iyrmo(im),xmonam(im),
     1      tin,    tret,   tgwin,  tfrsm,  tplan,
     1      tswin,  tdiv,   twell,  tdep,   tevap, tsep,
     1      tout,   tsto,   ttosm,  tdelsm, tswout, 
     1      del,    tcu,    tloss,  tpump,  tsal
          endif
         
        endif
c
c _______________________________________________
c               Step 17; Print Monthly Ground Water Balance
c rrb 99/05/06; Ground Water balance.  Note recharge is diversion (tdiv)
c               + pumping (twell) - CU w/o reservoir evap (tcux).
c               Note, immediate returns are in tret
        if(iwell.gt.0) then
c
c               Print Monthly GW balance
          if(cplot.eq.'Annual      ') then
          else
            if(isigfig.eq.0) then
              write(igw,230) iyrmo(im),xmonam(im),
     1        trech,   twell,  tdep,   -1.0,   tgwinx,
     1        tpump,   tret,   tloss, tgwin,  -1.0,   
     1        tgwout,tgwdel,tsal
            endif
            
            if(isigfig.eq.1) then
              write(igw,2301) iyrmo(im),xmonam(im),
     1        trech,   twell,  tdep,   -1.0,   tgwinx,
     1        tpump,   tret,   tloss, tgwin,  -1.0,   
     1        tgwout,tgwdel,tsal
            endif
            
            if(isigfig.eq.2) then
              write(igw,2302) iyrmo(im),xmonam(im),
     1        trech,   twell,  tdep,   -1.0,   tgwinx,
     1        tpump,   tret,   tloss, tgwin,  -1.0,   
     1        tgwout,tgwdel,tsal
            endif
          endif
        endif
c
c ________________________________________________________
c               Step 18; End month loop

  200   continue
c
c _______________________________________________
c               Step 19; Print annual total - Stream Balance
c
        if(cplot.eq.'Annual      ') then
        else
          write(iw,260)
        endif
c
        if(isigfig.eq.0) then
          write(iw,230) iyrmo(13),'Tot ',
     1      tint*ftot,    trett*ftot,   tgwint*ftot, tfrsmt*ftot, 
     1      tplant*ftot, 
     1      tswint*ftot,  tdivt*ftot,   twellt*ftot, tdept*ftot,
     1      tevapt*ftot,  tsept*ftot,
     1      toutt*ftot,   tstot*ftot,   ttosmt*ftot,  tdelsmt*ftot,
     1      tswoutt*ftot,
     1      delt*ftot,    tcut*ftot,    tlosst*ftot,  tpumpt*ftot,  
     1      tsalt*ftot
        endif
        
        if(isigfig.eq.1) then
          write(iw,2301) iyrmo(13),'Tot ',
     1      tint*ftot,    trett*ftot,   tgwint*ftot, tfrsmt*ftot, 
     1      tplant*ftot, 
     1      tswint*ftot,  tdivt*ftot,   twellt*ftot, tdept*ftot,
     1      tevapt*ftot,  tsept*ftot,
     1      toutt*ftot,   tstot*ftot,   ttosmt*ftot,  tdelsmt*ftot,
     1      tswoutt*ftot,
     1      delt*ftot,    tcut*ftot,    tlosst*ftot,  tpumpt*ftot,  
     1      tsalt*ftot
        endif
        
        if(isigfig.eq.2) then
          write(iw,2302) iyrmo(13),'Tot ',
     1      tint*ftot,    trett*ftot,   tgwint*ftot, tfrsmt*ftot, 
     1      tplant*ftot, 
     1      tswint*ftot,  tdivt*ftot,   twellt*ftot, tdept*ftot,
     1      tevapt*ftot,  tsept*ftot,
     1      toutt*ftot,   tstot*ftot,   ttosmt*ftot,  tdelsmt*ftot,
     1      tswoutt*ftot,
     1      delt*ftot,    tcut*ftot,    tlosst*ftot,  tpumpt*ftot,  
     1      tsalt*ftot
        endif
     
c
c _______________________________________________
c               Step 20; Print annual total - Ground Balance
        tgwdelt = tgwinxt - tgwoutt
        if(iwell.gt.0) then
          if(cplot.eq.'Annual      ') then
          else
            write(igw,265)
          endif
          
          if(isigfig.eq.0) then
            write(igw,230) iyrmo(13),'Tot ',
     1      trecht*ftot,  twellt*ftot, tdept*ftot,  -1.0, tgwinxt*ftot,
     1      tpumpt*ftot,  trett*ftot,  tlosst*ftot, tgwint*ftot, 
     1      -1.0, tgwoutt*ftot, 
     1      tgwdelt*ftot, tsalt*ftot
          endif
          
          if(isigfig.eq.1) then
            write(igw,2301) iyrmo(13),'Tot ',
     1      trecht*ftot,  twellt*ftot, tdept*ftot,  -1.0, tgwinxt*ftot,
     1      tpumpt*ftot,  trett*ftot,  tlosst*ftot, tgwint*ftot, 
     1      -1.0, tgwoutt*ftot, 
     1      tgwdelt*ftot, tsalt*ftot
          endif
          
          if(isigfig.eq.2) then
            write(igw,2302) iyrmo(13),'Tot ',
     1      trecht*ftot,  twellt*ftot, tdept*ftot,  -1.0, tgwinxt*ftot,
     1      tpumpt*ftot,  trett*ftot,  tlosst*ftot, tgwint*ftot, 
     1      -1.0, tgwoutt*ftot, 
     1      tgwdelt*ftot, tsalt*ftot
          endif
        endif
c
c ________________________________________________________
c               Step 21; End year loop

  210 continue
  

c
c _______________________________________________
c               Step 22; Print Average - Stream Balance
      
      write(iw,250) cunitm, (i,i=1,nb)
      write(iw,260)      
      do im=1,12
        if(isigfig.eq.0) then
          write(iw,232) xmonam(im),
     1                  (dum(im,n)/ry, n=1,nb)
        endif
      
        if(isigfig.eq.1) then
          write(iw,2321) xmonam(im),
     1                  (dum(im,n)/ry, n=1,nb)        
        endif
      
        if(isigfig.eq.2) then
          write(iw,2322) xmonam(im),
     1                  (dum(im,n)/ry, n=1,nb)        
        endif
      end do
      
      
c
c ________________________________________________________
c               Step 23; Print Average Annual plus footnotes
      write(iw,260)
      if(isigfig.eq.0) then
        write(iw,232)  'Tot ', (dum(13,n)*ftot/ry, n=1,nb) 
c
c rrb 2011/02/17; Correction to match *.xdc 
cx      write(iw,280) AdjTot*ftot/ry, (dum(13,7)+AdjTot)*ftot/ry                          
        write(iw,280) -1.*AdjTot*ftot/ry, (dum(13,7)-AdjTot)*ftot/ry        
        write(iw,282) AdjTsT*ftot/ry, AdjDcT*ftot/ry, AdjRcT*ftot/ry,
     1                AdjRrT*ftot/ry,
     1                AdjPT*ftot/ry, -1.*AdjTot*ftot/ry
      endif
      
      if(isigfig.eq.1) then
        write(iw,2321)  'Tot ', (dum(13,n)*ftot/ry, n=1,nb)                        
        write(iw,2801) AdjTot*ftot/ry,(dum(13,7)+AdjTot)*ftot/ry
        
        write(iw,282) AdjTsT*ftot/ry, AdjDcT*ftot/ry, AdjRcT*ftot/ry,
     1                AdjRrT*ftot/ry,        
     1                AdjPT*ftot/ry, -1.*AdjTot*ftot/ry
      endif
      
      if(isigfig.eq.2) then
        write(iw,2322)  'Tot ', (dum(13,n)*ftot/ry, n=1,nb)                        
        write(iw,2802) AdjTot*ftot/ry,(dum(13,7)+AdjTot)*ftot/ry
        
        write(iw,282) AdjTsT*ftot/ry, AdjDcT*ftot/ry, AdjRcT*ftot/ry,
     1                AdjRrT*ftot/ry,        
     1                AdjPT*ftot/ry, -1.*AdjTot*ftot/ry
      endif
c
c _______________________________________________
c               Step 22X; Print Average - Stream Balance Report
c		and CU comparison Report
c
c		Elements 1-11      
      if(cplot.eq.'Report      ') then
        nb1=11
        write(113,251) cunitm, (i,i=1,nb1)
        
        do im=1,12
            write(113,234) xmonam(im), (dum(im,n)/ry, n=1,nb1)
        end do
        
        write(113,234)  'Tot ', (dum(13,n)*ftot/ry, n=1,nb1)                   
c       
c	  	Elements 12-21      
        nb1=12
        write(113,252) cunitm, (i,i=nb1,nb)
c       write(113,262)            
        
        do im=1,12
            write(113,234) xmonam(im), (dum(im,n)/ry, n=nb1,nb)
        end do
        
c       write(113,262)
        write(113,234)  'Tot ', (dum(13,n)*ftot/ry, n=nb1,nb)  
c
c		Print CU comparison 
        ca=(dum(13,18) -  dum(13,10))*ftot/ry
        cr=dum(13,10)*ftot/ry            
        ct=dum(13,18)*ftot/ry            
        
        write(115,375)  'Ag. CU      ', -1., ca, -1., -1.
        write(115,375)  'Res. CU     ', -1., cr, -1., -1.
        write(115,375)  'Total CU    ', -1., ct, -1., -1.
       endif                 
c
c _________________________________________________________               
c               Step 23; Print Average - Ground Balance
c rrb 99/05/06; 
      if(iwell.gt.0) then
        write(igw,255) cunitm, (i,i=1,ng)
        write(igw,265)
        do im=1,12
          if(isigfig.eq.0) then        
            write(igw,232) xmonam(im),
     1                (dum(im,n)/ry, n=nb+1,nb+ng)
          endif
          
          if(isigfig.eq.1) then
            write(igw,2321) xmonam(im),
     1                (dum(im,n)/ry, n=nb+1,nb+ng)
          endif
          
          if(isigfig.eq.2) then
            write(igw,2322) xmonam(im),
     1                (dum(im,n)/ry, n=nb+1,nb+ng)
          endif
          
        end do
      endif
c
c ________________________________________________________
c               Step 25; Print Ground Water Balance
      if(iwell.gt.0) then
        write(igw,265)
        write(igw,232)  'Tot ',
     1                (dum(13,n)*ftot/ry, n=nb+1,nb+ng)                   
        write(igw,263)
      endif
c
c ________________________________________________________
c               Step 26; Print Delta to screen and *.log
c
c rrb 2006/05/30; Correction
cr    write(6,265) delt*ftot
cr    write(nlog,265) delt*ftot
      write(6,256) dum(13,ndelta)*ftot/ry
      write(nlog,256) dum(13,ndelta)*ftot/ry
c
c ____________________________________________________
c		Step 27; Return
      close(113)
      close(115)
      return
        
c
c ________________________________________________________
c               Formats
  220 format(/,72('_'),/'  Subroutine Outbal2')
 
  230 format(i5, 1x, a4, 30f12.0)
 2301 format(i5, 1x, a4, 30f12.1)
 2302 format(i5, 1x, a4, 30f12.2)

  232 format('Ave  ', 1x, a4, 30f12.0)
 2321 format('Ave  ', 1x, a4, 30f12.1)
 2322 format('Ave  ', 1x, a4, 30f12.2)
 
  234 format('Ave  ,', 1x, a4,',', 10(f10.0,','), f10.0)
 2341 format('Ave  ,', 1x, a4,',', 10(f10.1,','), f10.1)
 2342 format('Ave  ,', 1x, a4,',', 10(f10.2,','), f10.2)
 
  236 format('Ave  ', 1x, a4, 30f12.0)
 2361 format('Ave  ', 1x, a4, 30f12.1)
 2362 format('Ave  ', 1x, a4, 30f12.2)
c
c
c 240 format(/,30x,'***  Stream Water Balance ', a5,' ***')


  250 FORMAT(/,30x, ' Water Budget ', a5,//
     1 '          ',
     1 '      Stream                 From/To        From        From',
     1 '       Total              From River        Well   Reservoir',
     1 '   Reservoir      Stream   Reservoir          To       SoilM',
     1 '       Total                                    '/
     1 ' Year   Mo',
     1 '      Inflow      Return   GWStorage       SoilM    Plan (5)',
     1 '      Inflow  Divert (6)     by Well   Depletion Evaporation',
     1 '     Seepage     Outflow      Change       SoilM      Change',
     1 '     Outflow       Delta      CU (1)    Loss (2) Pumping (3)',
     1 ' Salvage (4)',/, 
     1 '          ',
     1 '         (+)         (+)         (+)         (+)         (+)',
     1 '          NA',
     1 '         (-)         (-)         (-)         (-)',
     1 '         (-)         (-)         (-)         (-)         (-)',
     1 '          NA          NA          NA          NA          NA',
     1 '          NA'/
     1 10x,
     1 21('        (', i2,')'))
     
  251 FORMAT(/,30x, ' Water Budget Report Format', a5,//
     1 '     ,     ,',
     1 '    Stream,          ,   From/To,      From,      From,',
     1 '     Total,          ,  Fr River,      Well, Reservoir,',
     1 ' Reservoir,',/
     1 ' Year,   Mo,',
     1 '    Inflow,    Return, GWStorage,     SoilM,      Plan,',
     1 '    Inflow,    Divert,   by Well, Depletion,      Evap,',
     1 '   Seepage,',/, 
     1 '     ,     ,',
     1 11('        ', i2,','))
     
  252 FORMAT(/,30x, ' Water Budget Report Format', a5,//
     1 '     ,     ,',
     1 '    Stream, Reservoir,        To,     SoilM,',
     1 '     Total,          ,          ,          ,          ,',
     1 '          ',/
     1 ' Year,   Mo,',
     1 '   Outflow,    Change,     SoilM,    Change,',
     1 '   Outflow,     Delta,        CU,      Loss,   Pumping,',
     1 '   Salvage,',/, 
     1 '     ,     ,',
     1 10('        ', i2,','))

  255 FORMAT(/,30x, ' Ground Water Budget ', a5,// 
     1 '          ',
     1 '              From River        Well       Other       Total',
     1 '       Total                               To/Fr       Other',       
     1 '       Total',
     1 '                        ',/
     1 ' Year   Mo',
     1 ' Recharge(1)     by Well   Depletion  Inflows(2)      Inflow',
     1 '     Pumping      Return        Loss   GwStorage Outflows(3)',     
     1 '     Outflow    Delta(4)  Salvage(5)',/
     1 '          ',
     1 '         (+)         (+)         (+)         (+)          NA',
     1 '         (-)         (-)         (-)         (-)         (-)',         
     1 '          NA          NA         NA',/,
     1  10x, 13('        (', i2,')'))

  260 format(
     1 ' ____ ____',
     1 ' ___________ ___________ ___________ ___________ ___________',
     1 ' ___________ ___________ ___________ ___________ ___________',
     1 ' ___________ ___________ ___________ ___________ ___________',
     1 ' ___________ ___________ ___________ ___________ ___________',
     1 ' ___________')

  261 format(
     1 ' ____, ____,',
     1 ' _________, _________, _________, _________, _________,',
     1 ' _________, _________, _________, _________, _________,',
     1 ' _________')
     
  262 format(
     1 ' ____, ____,',
     1 ' _________, _________, _________, _________, _________,',
     1 ' _________, _________, _________, _________, _________')

  265 format(
     1 ' ____ ____ ___________ ___________ ___________ ___________',
     1           ' ___________ ___________ ___________ ___________',
     1           ' ___________ ___________ ___________ ___________', 
     1           ' ___________')

  280 format(82x, f12.0,' (6,7)',/ 82x, ' ___________',/,82x, f12.0,/)
 2801 format(82x, f12.1,' (6,7)',/ 82x, ' ___________',/,82x, f12.1,/)
 2802 format(82x, f12.2,' (6,7)',/ 82x, ' ___________',/,82x, f12.2,/)
 
  
  282 format(
     1  '  Note: (1) Consumptive Use (CU) = Diversion ',
     1              '(Divert) * Efficiency + From Well * Efficiency ',/
     1  '           + max (Resevoir Evaporation (Evap), 0.0).'/,
     1  '        (2) Loss is not part of the Stream Water Balance. ',/
     1  '            It is the portion of a diversion, well pumping ',
     1              'and reservoir seepage that does not return to ',
     1              'the stream.',/,
     1  '        (3) Pumping is not part of the Stream Balance.',/
     1  '            Its impact on the stream is included in the ', 
     1              'From River by Well and Well Depletion columns.',/
     1  '        (4) Salvage is not part of the Stream Water Balance.',/
     1  '            It is the portion of well pumping that does not ',
     1              'impact the stream.',/
     1  '        (5) From Plan is water from a non-reservoir reuse ',
     1              'plan (type 4) or an accounting plan (type 12).',/
     1  '            Both a non-reservoir reuse and an accounting ',
     1              'plan are water supplies in the water balance',/
     1  '            because they store return flows which would have ',
     1              'returned to the system if they were not assigned ',
     1              'to a plan.',/
     1  '        (6) Divert does not include diversions by an '
     1              'instream flow or a T&C plan. ',/
     1  '            To avoid double accounting with reservoir ',
     1              'storage it has been reduced as follows:',/
     1  '              Diversion to Storage                      ',
     1                   f8.0,' af/yr',/
     1  '              Diversion to Carrier                      '
     1                   f8.0,' af/yr',/
     1  '              Reservior to Carrier                      '
     1                   f8.0,' af/yr',/     
     1  '              Reservior Release when storage change = 0 '
     1                   f8.0,' af/yr',/     
     1  '              Plan Carrier                              '
     1                   f8.0,' af/yr',/     
     1  '              Total                                     ',
     1                   f8.0,' af/yr',/
     1  '        (7) Divert does include diversions by a '
     1                 'administration plan.  This may be tracked ',/
     1  '            at a later date but it will require the ',
     1                 'TsTool, a time series processor, be updated') 

  263 format(/,'  Note: (1) Recharge = Divert + Pumping',
     1         ' - CU - Soil Moisture Change.',
     1         ' Recharge and CU are for both surface and ground',
     1         ' water. CU does not include reservoir evaporation.',/
     1         '        (2) Other Inflows to ground water not',
     1         ' modeled include natural stream loss, precipitation ',
     1         ' recharge, boundary inflow, etc.',/
     1         '        (3) Other Outflows from ground water not',
     1         ' modeled include natural stream gain, boundary outflow,',
     1         ' CU by native species, etc.',/
     1         '        (4) Delta is Total Inflow - Total Outflow but', 
     1         ' remember Other Inflows and Other Outflows are not', 
     1         ' included. Also it takes some time before return',/
     1         '            flows & depletions impact the system',/        
     1         '        (5) Salvage is not part of the Ground Water ',
     1         'Balance because it is a net change from non benefical',
     1         ' (e.g. Native ET, etc.) to Consumptive Use')
 256  format(/72('_'),/
     1 '  Outbal2; Average Inflow - Outflow = ', f8.0)          
 266  format(' Outbal2 Diversion adjustment;',
     1          ' #, dat1(30), dat1(31), dat1(32) = ',2i5, 20f8.1)  
 268  format(/,' Outbal2; Balance adjustment;',/
     1 '    # Year  Mon Type                     River ID       is ',
     1 'ip/ir istr     AdjXX      AdjT    AdjToT',/
     1 ' ____ ____ ____ ________________________ ____________ _____',
     1 ' ____ ____ _________ _________ _________')

 269  format(2i5,1x,a4,1x, a24, 1x,a12, 1x,3i5, 20f10.1) 
 
 290  format(/,' Outbal2; Inflow Data;',/
     1 ' Year  Mon   is River ID      Inflow  In Tot',
     1 '   GW In  GW Tot',/
     1 ' ____ ____ ____ ____________ _______ _______',
     1 ' _______ _______')
 291  format(i5, 1x, a4, i5, 1x, a12, 1x, 20f8.0)    
 
  331 format(/,
     1  'Item       ,   StateCU,  StateMod,     Delta,   Delta %')
  375  format(a12,',', 3(f10.0,','), f10.0)
     
     
 900  format(
     1 ' Outbal2; Problem key output variables for ', a10, 
     1 ' were developed',/
     1 10x,'Based on ', i5, ' output variables but ndivO = ', i4,/
     1 10x,'Reconmend you revise OutBal2 appropriately')
c
c ____________________________________________________
c
c       Error Processing

  270 write(6,272)
  272 format('   Outbal2; Requested data exceeds binary file size')
      write(nlog,272) 
      goto 9999

 9999 write(6,*)
      write(nlog,*) '  Stopped in OutBal2'
      write(6,*) 'Stop 1' 
      call flush(6)
      call exit(1)

      stop 
c
c ____________________________________________________
c
      end






