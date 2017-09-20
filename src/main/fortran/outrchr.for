c *********************************************************
C
      SUBROUTINE outRchR(nreach,cplot)
c
c	Called by Report
C
c      outRchR; Print a water balance report by reach (*.xrh)
c
c	RchidD		Diversion Reach ID
c
c ____________________________________________________
c
c       Update History
c
c rrb 2008/12/20; Copy OutBal2 and add reach data. Specifically:
c		1. Add a reach do loop (240)
c		2. For a reach:
c		   Use the data in the reach input file (*.rch) to 
c		   exclude data not within a reach.
c		   Revise the approach to calculate stream inflow.
c		   Revise the Carrier to Storage adjustments
c
c ____________________________________________________
c
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
c              dat1(18) = Total Return
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
c	       dat1(34)	= Source is a reuse or admin plan	  
c              dat1(35) = rid
c              dat1(36) = xstr
c	       dat1(37) = calling ID
c	       dat1(38) = calling right
c
c	       AdjDc    = adjustment to Diversion Carrier (*.xdd)
c	       AdjRc    = adjustment to Reservoir Carrier (*.xre)
c	       AdjP     = plan adjustment (*.xdd)
c
c rrb 2008/01/11; New Id convention as follows:
c		Strtype (istr1) 1-5000 = Diversion
c                               5001 - 7500 = ISF
c                               7501 - 10000 = Reservoir
c				10001 -12500 = Plan
c				12501 -15000 = Wells
c                               < -10000 = baseflow only
c                               -1* above = baseflow plus a structure 

c ____________________________________________________
c
c
      include 'common.inc'
      dimension ioutx(100), iinx(100)
cx      1 nRchTo(150), nRchEnd(150)
      character cplot*12
c
c ____________________________________________________
c
c		Step 1; Initilize
c
c	          	 iout        =0  no details
c	          	 iout        =1  print all adjustments to balance
c	          	 iout        =2  print Carrier to Storage adjustments to balance
                       
c	          	 ioutR       =1  print detailed reservoir adjustment data
c	          	 ioutD       =1  print total diversion data
c	          	 ioutIn      =1 print detailed reach inflow data
c	          	 ioutOut     =1 print detailed reach outflow data
c	          	 ioutRe      =1 Print detailed reach processing
c	          	 ioutS       =1 print summary data by reach to *.log
c	          	 ioutIZ      =reach to print detailed data
      iout=0
      ioutR=0
      ioutD=0
      ioutIn=0
      ioutOut=0
      ioutRe=0
      ioutS=1
      
      ioutIZ=2
      
      write(6,10)
      write(nlog,10)
  10  format(/,72('_'),/,
     1 '  OutRchR; Reach Water Balance Report (*.xrw) ')             
c
c		Set reach To and Reach End for Lower South Platte      
cx      nRchTo(1) = 2
cx      nRchTo(2) = 3
cx      nRchTo(3) = 4
cx      nRchTo(4) = 9
cx      nRchTo(5) = 9
cx      nRchTo(6) = 9
cx      nRchTo(7) = 9
cx      nRchTo(8) = 9
cx      nRchTo(9) = 0
cx      
cx      nRchEnd(1) = 168
cx      nRchEnd(2) = 179
cx      nRchEnd(3) = 189
cx      nRchEnd(4) = 221
cx      nRchEnd(5) = 230
cx      nRchEnd(6) = 234
cx      nRchEnd(7) = 240
cx      nRchEnd(8) = 248
cx      nRchEnd(9) = 250
      
      nout=0
      small=0.01
      ry=iyend-iystr+1      
      
      riz=float(nreach)
      write(6,220) nreach
      write(nlog,220) nreach
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
      
      
      
      now=22

      
      call outtop(now,1,3)
c
c rrb 99/05/06; Ground Water balance
c         nb = # of columns in stream system balance output,
c         nb1= nb+1 for reach ID
c         ng = # of columns in gw balance
      nb=21    
      nb1=nb+1
      ng=13
c
c rrb 2008/101/05; Remove GW balance report by reach
cxx   igw=28
cxx   if(iwell.gt.0) call outtop(igw,1,34)
c
c ____________________________________________________
c rrb 2008/01/05; Add Reach Data. Note nreach+1 is the total
c              Step X; Loop for each reach
      RchidR(nreach+1) = 'Total       '
      RchNameR(nreach+1)= 'Total                   '
      
      do 240 iz=1,nreach+1   
        riz=float(iz)
          
c
c		Initilize reach in and out
        do ix=1,100
          ioutX(ix)=0
          iinX(ix)=0
        end do  
      
c ____________________________________________________
c rrb 10/31/96;
c              Step 1b. Initilize average annual arrays
        do n=1,nb+ng
          do im=1,13
            dum(im,n) = 0.0
          end do
        end do
c ____________________________________________________
c
c rrb 2009/01/06;  Step 1b. Initilize adjustment averages
        AdjDcT=0.0
        AdjRcT=0.0
        AdjPt=0.0
        AdjtpT=0.0
        AdjTsT=0.0
        AdjSiT=0.0
        AdjTot=0.0
        AdjGain=0.0
c
c ____________________________________________________
c              Step 2a. Find inflow stations to this reach 
        
        inx=0
        ifound=0
        do i=1,nreach
c         write(nlog,*) ' OutRchR; i,iz,inx,nRchTo',i,iz,inx,nRchTo(i)
          if(nRchTo(i).eq.iz) then
            inx=inx+1
            iinX(inx)=nRchEnd(i)
            
            if(ioutIn.eq.1) then
              write(nlog,*) ' '
              write(nlog,*) ' OutRchR Inflow; ',
     1        'iz, i, inx, iinX', iz,i,inx,iinX(inx) 
            endif
            ifound=1
          endif
        end do
        
        if(ioutIn.eq.1 .and. ifound.eq.0) then
          write(nlog,*) ' OutRchR; No inflows found for reach ', iz, inX
        endif
c
c ____________________________________________________
c              Step 2b. Find outlet station for a reach
        ix=0
        if(iz.le.nreach) then
          ix=1
          ioutx(ix) = nRchEnd(iz)
        endif
        if(ioutOut.eq.1) then
          write(nlog,*) ' OutRchR Outflow; iz, ix, ioutx(ix)',
     1      iz, ix, ioutx(ix)      
        endif
c
c ____________________________________________________
c              Step 2c. Find outlet station for the total system
c			as reaches with no downstream node
        if(iz.gt.nreach) then 
          ix=0
          do is=1,numsta
            if(idncod(is).eq.0) then
              ix=ix+1
              ioutx(ix) = is
            endif          
          end do
        endif
c
c ---------------------------------------------------------
c		Summary output to *.log
      
        if(ioutRe.eq.1) write(nlog,*) ' OutRchR; Processing Reach = ', iz
        
        if(ioutS.eq.1 .and. iz.eq.ioutIz) then
          write(nlog,250) cunitm,0,RchidR(nreach+1),RchNameR(nreach+1),
     1      (i,i=1,nb1) 
          write(nlog,260) 
        endif
        
        if(ioutS.eq.1 .and. iz.eq.nreach+1) then
          write(nlog,260) 
        endif
        
c
c ____________________________________________________
c              Step 3; Print header if annual data only                        
c               
        if(cplot.eq.'Annual      ') then
          write(now,250) cunitm,iz,RchidR(iz),RchNameR(iz), (i,i=1,nb1)
          write(now,260)
        endif
c
c rrb 2007/12/07; Print without monthly titles
        write(ntmp,250) cunitm,iz,RchidR(iz),RchNameR(iz), (i,i=1,nb1)
        write(ntmp,260)                
c      
c ____________________________________________________
c
c		Step 4; Begin year loop
        do 210 iy=iystr,iyend
          call year(iy, iyrmo, imomo, cyr1)
c
c rrb 10/27/94 Additional Output
          write(6,130) iy
          call flush(6)
  130     format('+', ' Processing Year = ', i5)
c
c ____________________________________________________
c
c               Step 5; Print header once per year           
          if(cplot.eq.'Annual      ') then
          else
            write(now,250) cunitm,iz,RchidR(iz),RchNameR(iz),(i,i=1,nb1)
            write(now,260) 
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
          tgaint = 0.0

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
c           fac=mthday(im)*factor
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
            tgain= 0.0
c
c rrb 99/05/06; Ground Water Balance
            tcux = 0.0
            tpump= 0.0
c
c rrb 2006/10/27; Diversion Adjustments		          
            AdjDc=0.0
            AdjRc=0.0
            AdjP=0.0
            AdjTp=0.0
            AdjTS=0.0
            AdjSi=0.0
c
c ____________________________________________________
c
c		Step 10; Process Station (*.b43, *.xdd) loop
            do 150 is=1,numsta
c         
              irecs=((iy-iystr0)*12+(im-1))*numsta+is+numtop
              
              if(ioutRe.eq.1) then
                write(nlog,*) ' OutRchR; iz, is, iz1 ', 
     1            cstaid(is), iz, is, iz1, irecs
                write(nlog,*) ' OutRchR; irecs ', 
     1            irecs, iy-iystr0, im-1, numsta, numtop
              endif
c         
              read(43,rec=irecs,err=270) (dat1(i),i=1,ndiv)
c             write(nlog,*) '  outRchR; 43', (dat1(i), i=1,ndiv)
c
c ---------------------------------------------------------
              istr=0
              istrT=0
              istr=iabs(istrtype(is))
              if(istr.ge.1     .and. istr.le.5000)  istrT=3
              if(istr.ge.7501  .and. istr.le.10000) istrT=2
              if(istr.ge.10001 .and. istr.le.12500) istrT=7
              if(istr.ge.12501 .and. istr.le.15000) istrT=6

              do i=1,ndivF
                dat1(i)=dat1(i)*fac
              end do
c
c ---------------------------------------------------------
c rrb 2009/01/05; Set Reach Inflow as the outflow from
c		    an upstream reach
c             write(nlog,*) '  OutRchR; inX = ', inX
              if(inX.gt.0) then
                do i=1,inX
                  if(iinX(i).eq.is .and. iz.le.nreach) then
                    tin=tin+dat1(28)                
                  
                   if(ioutIn.eq.1 .and. iz.eq.ioutIZ) then
                     write(nlog,*) ' '
                     write(nlog,*) ' OutRchR Sum Inflow; ',
     1               'iy, im, iz, is, i, inX, iinX '
                     write(nlog,*) ' OutRchR Sum Inflow; ',
     1               iy, im, iz, is, i, inx, iinX(i), tin
                   endif
                  endif                  
                end do
              endif
c
c
c ---------------------------------------------------------
c		     Set Reach Outflow
c rrb; 2009/04/22; Correction Move here to process a station (is) that
c		     is not in the reach of interest
cx              do i=1,ix
cx                if(ioutx(i).eq.is) tout=tout+dat1(28)
cx              end do              
c
c ---------------------------------------------------------
c rrb 2009/01/05; Skip if not in this reach
              iz1=iRch(is)
              if(iz1.ne.iz .and. iz.le.nreach) goto 150
c
c ---------------------------------------------------------
c 		    Add gains to stream inflow
c			The following are based on ndivO=37 or 38
              tgain=tgain  + dat1(21)    
              AdjGain=AdjGain + dat1(21)
              
              tin1=tin
              tin  = tin   + dat1(21)
              
              if(ioutIn.eq.99 .and. iz.eq.ioutIz) then
                write(nlog,*) ' '
                write(nlog,*) ' OutRchR Tin & Gain; ',
     1           'iy, im, iz, is, cstaid(is), tin1, tgain, tin'   
                write(nlog,*) ' OutRchR Tin & Gain;', 
     1            iy, im, iz, is, cstaid(is), tin1, tgain, tin   
              endif
           
              tret = tret  + dat1(22)
              tgwin= tgwin + dat1(24)
              tfrsm= tfrsm + dat1(12)
              tpump=tpump+dat1(7)
c
c ---------------------------------------------------------
c rrb 2008/01/15; Store From Plan in dat1(34)
c			Note for detailed output there is no adjustment
c			therefore do not add dat1(34) to AdjTot
c rrb 2008/12/30; From Plan is included in to diversion			
cx            tplan=tplan+dat1(34)              
cx            AdjTp=dat1(34)                 
cx            AdjTpT=AdjTpT+dat1(34)
              
              AdjTp=tplan
              AdjTpT=AdjTpT+tplan
              
              
              if(dat1(34).gt.small .and. iout.ge.1) then
                 nout=nout+1
                 write(nlog,269) nout, iyrmo(im), xmonam(im),
     1           'Adding to Plan Data     ', cstaid(is),
     1           iz, is, is, AdjTp, AdjTpT, AdjTot     
              endif
              
              tswin= tin + tret + tgwin + tfrsm + tplan

c
c ---------------------------------------------------------
c	       The following are based on ndivO=39
c              Total diversion = direct from river by priority (3)
c                + river by storage (4) + river by exchange (5)
c                - instream by priority (30) - instream by storage (31)
c                + carrier by storage (32)
              tdiv = tdiv + dat1(3)  + dat1(4)  + dat1(5) 
     1                    + dat1(32) - dat1(30) - dat1(31)
            
              c =           dat1(3)  + dat1(4)  + dat1(5) 
     1                    + dat1(32) - dat1(30) - dat1(31)
            
              c1=           dat1(3)  + dat1(4)  + dat1(5) 
              c2=           dat1(32) - dat1(30) - dat1(31)
c
c ---------------------------------------------------------              
c rrb 2008/01/11; To Storage Adjustment
              if(istrT.eq.2) then
                c=0.0
                c1=0.0
                c2=0.0
c             
c		  To Storage adjustment
                AdjTs=dat1(3) + dat1(4)  + dat1(5) 
                
c             
                tdiv=tdiv-AdjTs
                
                if (AdjTs.gt.small) then
                  AdjTsT = AdjTsT + AdjTs
                  AdjTot = AdjTot - AdjTs
                
                  if(iout.ge.1) then
                    nout=nout+1
                    ir1=istr-7500
                    write(nlog,269) nout, iyrmo(im), xmonam(im),
     1                'Adj Div to Storage      ' , cstaid(is),
     1                iz, is, ir1, AdjTs, AdjTsT, AdjTot
                  endif
                endif
c             
c		  End to storage adjustment              
              endif
            
c             if(ioutD.eq.1 .and. c.gt.small) then
              if(ioutD.eq.1) then
                nd=istrtype(is)
                  if(is.eq.1) write(nlog,300)
                  write(nlog,310) iyrmo(mon),xmonam(mon), 
     1              is, istrtype(is), cstaid(is),c1, c2, c, tdiv,
     1              dat1(30), dat1(31), dat1(32), 0.0
 300            format(/,
     1          '   Yr  Mon   is istrtyp ID          ',
     1          '      c1     c2        c    tdiv',
     1          ' dat1_30 dat1_31 dat1_32 Fr Plan',/
     1          ' ____ ____ ____ _______ ____________',
     1          ' _______ _______ _______ _______',
     1          ' _______ _______ _______ _______')           
 310            format(i5, 1x, a4, i5, i8, 1x, a12, 20f8.0)    
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
cx     1             iz, is, AdjDc, AdjDcT, AdjTot
cx              endif  
cx            endif
c
c		The following are based on ndivO=38
              twell= twell+ dat1(27)
              tdep = tdep + dat1(23)
              
              tcu  = tcu  + dat1(16)
              ttosm= ttosm+ dat1(17)            
              tdelsm=tdelsm-dat1(17)+dat1(12)
              tcux = tcu
c
c rrb 2007/03/28; Carrier losses are not a loss to system
c		  Note if total returns are < 100 then
c		  it shows up ind dat1(19) system loss
cx            tloss= tloss+ dat1(6) + dat1(10) + dat1(19)
              tloss= tloss+ dat1(19)
              tsal = tsal + dat1(33)
c
c rrb 2009/04/22; Move above the goto 150 branck
c rrb 2009/04/24; Back to original
              do i=1,ix
                if(ioutx(i).eq.is) tout=tout+dat1(28)
              end do
c         
  150       continue
c
c ____________________________________________________
c
c		Step 11; Reservoir (*.xre) data Loop

            if(numres.eq.0.or.nrsact.eq.0) go to 190
            
            nrsactx=nrsact+numown             
            irt = 0
            
            do 180 ir=1,numres
              if(iressw(ir).eq.0) go to 180
  160         irt = irt+1
c
c ---------------------------------------------------------
c		Read data
              irecr=((iy-iystr0)*12+(im-1))*nrsactx+irt+numtop
              read (44,rec=irecr,err=270) (dat2(i),i=1,nres)
c             write(nlog,*) '  outRchR, Res', (dat2(i), i=1,nres)
c
c ---------------------------------------------------------
c              Skip account data
              ida = dat2(nidR)
              if(ida.ne.0) goto 160
c
c ---------------------------------------------------------
c rrb 2009/01/05; Add reach processing            
              is1=irssta(ir)
              iz1=iRch(is1)              
              if(iz1.ne.iz .and. iz.le.nreach) goto 180              
           
c
c ---------------------------------------------------------
c		Adjust units
              do i=1,nresX
                dat2(i)=dat2(i)*fac
              end do
c
c rrb 2006/04/18; Add seepage and seepage loss
              tsep   = tsep  + dat2(nSep)
              tloss  = tloss + dat2(nLos)
              tevap  = tevap + dat2(nEva)            
              tsto   = tsto  + dat2(nEom) - dat2(nBom)
              
cx              if(iz.eq.ioutIz) then
cx                 write(nlog,*) 
cx     1            ' OutRchR_2; ir, iz, iz1, nEOM, nBom, tSto',
cx     1            ir, iz, iz1, nEOM, nBom, tSto
cx              endif
              
c
c _________________________________________________________
c              Step 11a. Adjust stream inflow or diversions.
c		
c		For Reach Processing (iz.le.nreach) adjust
c		 the stream inflow term (tin) to include water
c		 carried to storage (dat2(ncar2).
c		For System Processing (iz.gt.nreach) adjust
c		 the diversion (tdiv) by water carried to storage
c		 dat2(nCar2) which is already included in the
c              reservoir delta storage term
c
              if(iz.le.nreach) then
                AdjSi=dat2(ncar2)
                AdjSiT=AdjSiT + AdjSi
                tin=tin+AdjSi
              else
                AdjRc=dat2(nCar2)
                AdjRcT=AdjRcT+AdjRc
                AdjTot=AdjTot-AdjRc                
                tdiv1=tdiv
                tdiv = amax1(0.0,tdiv-AdjRc)
              endif
c         
c ---------------------------------------------------------
c		  Print detailed carrier adjustment data            
              if (dat2(nCar2).gt.small .or. tdiv1.gt.small) then
                is=irssta(ir)      
                       
                if(iout.ge.1 .or. ioutR.eq.1) then
                  if(iz.eq.ioutIz) then
                    nout=nout+1
                    write(nlog,269) nout, iyrmo(im), xmonam(im),
     1                'Adj Res to Carrier      ', cstaid(is),
     1                iz, is, ir, AdjRc, AdjRcT, AdjTot, tdiv1, tdiv
                  endif
                endif
              endif
            
c
c ____________________________________________________
c
c               Step 11b Add reservoir evap to cu for global 
c                        balance (tcu) but not to gw balance
c                        calculations (tcux)
c rrb 01/11/03; Insure it is a positive evaporatoin
              tcu  = tcu + amax1(dat2(nEva), 0.0)
c
  180       continue
c
c
c		Exit if no reservoirs
  190     continue
c
c
c _______________________________________________
c               Step 12; Print Water Balance

          del =  tin  + tret  + tgwin + tfrsm + tplan
     1         - tdiv - twell - tdep - tevap - tsep - tout  - tsto 
     1         - ttosm- tdelsm 
         
          tswout=tdiv + twell + tdep + tevap + tsep + tout  + tsto 
     1         + ttosm+ tdelsm 
         
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
     1             toutt  + tstot  + ttosmt + tdelsmt
          
          delt   = delt   + del
          
          tcut   = tcut   + tcu
          tlosst = tlosst + tloss
          tpumpt = tpumpt + tpump
          tsalt  = tsalt  + tsal
          
          tgaint = tgaint + tgain
          
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
          dum(im,5)= dum(im,5)+ tplan                
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

c _______________________________________________
c               Step 16; Print Monthly Balance
c               
          if(cplot.eq.'Annual      ') then
          else
            if(isigfig.eq.0) then                      
              write(now,230) iyrmo(im),xmonam(im),
     1        tin,    tret,   tgwin,  tfrsm,  tplan,
     1        tswin,  tdiv,   twell,  tdep,   tevap, tsep,
     1        tout,   tsto,   ttosm,  tdelsm, tswout, 
     1        del,    tcu,    tloss,  tpump,  tsal, riz
            endif
            
            if(isigfig.eq.1) then
              write(now,2301) iyrmo(im),xmonam(im),
     1        tin,    tret,   tgwin,  tfrsm,  tplan,
     1        tswin,  tdiv,   twell,  tdep,   tevap, tsep,
     1        tout,   tsto,   ttosm,  tdelsm, tswout, 
     1        del,    tcu,    tloss,  tpump,  tsal, riz
            endif
            
            if(isigfig.eq.2) then
              write(now,2302) iyrmo(im),xmonam(im),
     1        tin,    tret,   tgwin,  tfrsm,  tplan,
     1        tswin,  tdiv,   twell,  tdep,   tevap, tsep,
     1        tout,   tsto,   ttosm,  tdelsm, tswout, 
     1        del,    tcu,    tloss,  tpump,  tsal, riz
            endif
           
          endif

c
c ________________________________________________________
c               Step 18; End month loop

  200     continue
c
c _______________________________________________
c               Step 19; Print annual total - Stream Balance
c
          if(cplot.eq.'Annual      ') then
          else
            write(now,260)
          endif
c       
          if(isigfig.eq.0) then
            write(now,230) iyrmo(13),' TOT',
     1        tint*ftot,    trett*ftot,   tgwint*ftot, tfrsmt*ftot, 
     1        tplant*ftot, 
     1        tswint*ftot,  tdivt*ftot,   twellt*ftot, tdept*ftot,
     1        tevapt*ftot,  tsept*ftot,
     1        toutt*ftot,   tstot*ftot,   ttosmt*ftot,  tdelsmt*ftot,
     1        tswoutt*ftot,
     1        delt*ftot,    tcut*ftot,    tlosst*ftot,  tpumpt*ftot,  
     1        tsalt*ftot,   riz
          endif
          
          if(isigfig.eq.1) then
            write(now,2301) iyrmo(13),' TOT',
     1        tint*ftot,    trett*ftot,   tgwint*ftot, tfrsmt*ftot, 
     1        tplant*ftot, 
     1        tswint*ftot,  tdivt*ftot,   twellt*ftot, tdept*ftot,
     1        tevapt*ftot,  tsept*ftot,
     1        toutt*ftot,   tstot*ftot,   ttosmt*ftot,  tdelsmt*ftot,
     1        tswoutt*ftot,
     1        delt*ftot,    tcut*ftot,    tlosst*ftot,  tpumpt*ftot,  
     1        tsalt*ftot,   riz
          endif
          
          if(isigfig.eq.2) then
            write(now,2302) iyrmo(13),' TOT',
     1        tint*ftot,    trett*ftot,   tgwint*ftot, tfrsmt*ftot, 
     1        tplant*ftot, 
     1        tswint*ftot,  tdivt*ftot,   twellt*ftot, tdept*ftot,
     1        tevapt*ftot,  tsept*ftot,
     1        toutt*ftot,   tstot*ftot,   ttosmt*ftot,  tdelsmt*ftot,
     1        tswoutt*ftot,
     1        delt*ftot,    tcut*ftot,    tlosst*ftot,  tpumpt*ftot,  
     1        tsalt*ftot,   riz
          endif
     
c
c ________________________________________________________
c               Step 21; End year loop

  210   continue
  

c
c _______________________________________________
c               Step 22; Print Average - Stream Balance
      
        write(now,250) cunitm,iz,RchidR(iz),RchNameR(iz), (i,i=1,nb1)
        write(now,260) 
        do im=1,12     
        
          if(isigfig.eq.0) then
            write(now,232) xmonam(im),
     1                    (dum(im,n)/ry, n=1,nb), riz
          endif
        
          if(isigfig.eq.1) then
            write(now,2321) xmonam(im),
     1                    (dum(im,n)/ry, n=1,nb), riz        
          endif
        
          if(isigfig.eq.2) then
            write(now,2322) xmonam(im),
     1                  (dum(im,n)/ry, n=1,nb), riz        
          endif
        end do
      
      
c
c ________________________________________________________
c               Step 23; Print Average Annual plus footnotes
         write(now,260)
c
        if(ioutS.eq.1) then
          write(nlog,232)  ' TOT', (dum(13,n)*ftot/ry, n=1,nb), riz  
        endif
          
        
        if(isigfig.eq.0) then
          write(now,232)  ' TOT', (dum(13,n)*ftot/ry, n=1,nb), riz                   
          write(now,280) AdjTot*ftot/ry, (dum(13,7)+AdjTot)*ftot/ry        
          write(now,282) 
     1      AdjTsT*ftot/ry, AdjDcT*ftot/ry, AdjRcT*ftot/ry,
     1      AdjPT*ftot/ry, AdjTot*ftot/ry, 
     1      AdjGain*ftot/ry,AdjSiT*ftot/ry
        endif
        
        if(isigfig.eq.1) then
          write(now,2321)  ' TOT', (dum(13,n)*ftot/ry, n=1,nb), riz                        
          write(now,2801) AdjTot*ftot/ry,(dum(13,7)+AdjTot)*ftot/ry
          
          write(now,282) 
     1      AdjTsT*ftot/ry, AdjDcT*ftot/ry, AdjRcT*ftot/ry,
     1      AdjPT*ftot/ry, AdjTot*ftot/ry, 
     1      AdjGain*ftot/ry,AdjSiT*ftot/ry
        endif
        
        if(isigfig.eq.2) then
          write(now,2322)  ' TOT', (dum(13,n)*ftot/ry, n=1,nb), riz                      
          write(now,2802) AdjTot*ftot/ry,(dum(13,7)+AdjTot)*ftot/ry
          
          write(now,282) 
     1      AdjTsT*ftot/ry, AdjDcT*ftot/ry, AdjRcT*ftot/ry,
     1      AdjPT*ftot/ry, AdjTot*ftot/ry, 
     1      AdjGain*ftot/ry, AdjSiT*ftot/ry
        endif
c
c _______________________________________________
c               Step 22X; Print Average - Stream Balance Report
c		              and CU comparison Report
c
c		Elements 1-11      
        if(ioutRe.eq.1) write(nlog,*) ' OutRchR; cplot = ', iz, cplot
        
        if(cplot.eq.'Report      ') then
          nbR=11
          write(113,251) cunitm, (i,i=1,nb1)
          
          do im=1,12
              write(113,234) xmonam(im), (dum(im,n)/ry, n=1,nbR), riz
          end do
          
          write(113,234)  ' TOT', (dum(13,n)*ftot/ry, n=1,nbR), riz                   
c       
c	 	Elements 12-21      
          nbR=12
          write(113,252) cunitm, (i,i=nbR,nb)
c         write(113,262)            
          
          do im=1,12
              write(113,234) xmonam(im), (dum(im,n)/ry, n=nbR,nb), riz
          end do
          
c         write(113,262)
          write(113,234)  ' TOT', (dum(13,n)*ftot/ry, n=nb1,nb), riz  
c         
c		   Print CU comparison 
          ca=(dum(13,18) -  dum(13,10))*ftot/ry
          cr=dum(13,10)*ftot/ry            
          ct=dum(13,18)*ftot/ry            
          
          write(115,375)  'Ag. CU      ', -1., ca, -1., -1.
          write(115,375)  'Res. CU     ', -1., cr, -1., -1.
          write(115,375)  'Total CU    ', -1., ct, -1., -1.
        endif                 

c
c ________________________________________________________
c               Step 26; Print Delta to screen and *.log
c
c rrb 2006/05/30; Correction
cr      write(6,265) delt*ftot
cr      write(nlog,265) delt*ftot
        if(ioutRe.eq.1) write(nlog,*) ' OutRchR; ndelta = ', iz, ndelta
        
        write(6,256) dum(13,ndelta)*ftot/ry, riz
        if(ioutRe.eq.1) write(nlog,256) dum(13,ndelta)*ftot/ry, riz
c
c ________________________________________________________
c               Step 18; End reach loop
  240 continue      
c
c ____________________________________________________
c		Step 27; Return
      close(113)
      close(115)
      return
        
c
c ________________________________________________________
c               Formats
  220 format('  outRchR; nreach = ',i5,/)
 
  230 format(i5, 1x, a4, 30f12.0)
 2301 format(i5, 1x, a4, 30f12.1)
 2302 format(i5, 1x, a4, 30f12.2)

  232 format('  AVE', 1x, a4, 30f12.0)
 2321 format('  AVE', 1x, a4, 30f12.1)
 2322 format('  AVE', 1x, a4, 30f12.2)
 
  234 format('  AVE', 1x, a4,',', 10(f10.0,','), f10.0)
 2341 format('  AVE', 1x, a4,',', 10(f10.1,','), f10.1)
 2342 format('  AVE', 1x, a4,',', 10(f10.2,','), f10.2)
 
  236 format('  AVE', 1x, a4, 30f12.0)
 2361 format('  AVE', 1x, a4, 30f12.1)
 2362 format('  AVE', 1x, a4, 30f12.2)
c


  250 FORMAT(
     1 /,30x, ' Water Budget ', a5, 
     1 ' Reach # = ', i2, ' Reach ID = ', a12,' Reach Name = ',a24,//
     1 '          ',
     1 '  Stream (7)                 From/To        From        From',
     1 '       Total              From River        Well   Reservoir',
     1 '   Reservoir      Stream   Reservoir          To       SoilM',
     1 '       Total                                    '/
     1 ' Year   Mo',
     1 '      Inflow      Return   GWStorage       SoilM    Plan (5)',
     1 '      Inflow  Divert (6)     by Well   Depletion Evaporation',
     1 '     Seepage     Outflow      Change       SoilM      Change',
     1 '     Outflow       Delta      CU (1)    Loss (2) Pumping (3)',
     1 ' Salvage (4)       Reach',/, 
     1 '          ',
     1 '         (+)         (+)         (+)         (+)         (+)',
     1 '          NA',
     1 '         (-)         (-)         (-)         (-)',
     1 '         (-)         (-)         (-)         (-)         (-)',
     1 '          NA          NA          NA          NA          NA',
     1 '          NA          NA'/
     1 10x,
     1 22('        (', i2,')'))
     
  251 FORMAT(/,30x, ' Water Budget Report Format', a5,//
     1 '     ,     ,',
     1 'Stream (7),          ,   From/To,      From,      From,',
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
     1 ' ___________ ___________')

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

c 262 format(70x, f12.0,' (6)',/ 70x, ' ___________',/,70x, f12.0,//,
  280 format(82x, f12.0,' (6)',/ 82x, ' ___________',/,82x, f12.0,/)
 2801 format(82x, f12.1,' (6)',/ 82x, ' ___________',/,82x, f12.1,/)
 2802 format(82x, f12.2,' (6)',/ 82x, ' ___________',/,82x, f12.2,/)
 
  
  282 format(
     1  '  Note: (1) Consumptive Use (CU) = Diversion ',
     1              '(Divert) * Efficiency + From Well * Efficiency ',/
     1  '           + max (Resevoir Evaporation (Evap), 0.0).'/,
     1  '        (2) Loss is not part of the Stream Water Balance.',/
     1  '            It is the portion of a diversion, well pumping',/
     1  '            and reservoir seepage that does not return to',/
     1  '            the stream',/,
     1  '        (3) Pumping is not part of the Stream Balance.',/
     1  '            Its impact on the stream is included in the ', 
     1              'From River by Well and Well Depletion columns',/
     1  '        (4) Salvage is not part of the Stream Water Balance.',/
     1  '            It is the portion of well pumping that does not ',
     1              'impact the stream',/
     1  '        (5) From Plan is water from a reuse plan. ',/     
     1  '        (6) Divert does not include diversions by an',/
     1  '            instream flow or a T&C plan. Also for System',/
     1  '            Processing only (e.g. not Reach Processing)',/
     1  '            Divert has been reduced to avoid double',/
     1  '            accounting with reservoir storage as follows:',/
     1  '              1 ', f8.0,' af/yr for Diverted to Storage.',/
     1  '              2 ', f8.0,' af/yr for a Diversion Carrier.',/
     1  '              3 ', f8.0,' af/yr for a Reservior Carrier.',/
     1  '              4 ', f8.0,' af/yr for a Plan Carrier.',/
     1  '                ', f8.0,' af/yr Total',/
     1  '        (7) For Reach processing only, Stream Inflow ',
     1               'includes:',/ 
     1  '              1 ', f8.0,' af/yr of Gain-Loss in this reach &',/
     1  '              2 ', f8.0,' af/yr for Carrier to Storage.')

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
     1 '  OutRchR; Average Inflow - Outflow = ', f8.0,' Reach = ', f8.0)          
 266  format(' outRchR Diversion adjustment;',
     1          ' #, dat1(30), dat1(31), dat1(32) = ',2i5, 20f8.1)  
 268  format(/,' outRchR; Balance adjustment;',/
     1 '    # Year  Mon Type                     River ID       is ',
     1 'ip/ir     AdjXX      AdjT    AdjToT',/
     1 ' ____ ____ ____ ________________________ ____________ _____',
     1 ' ____ _________ _________ _________')

 269  format(2i5,1x,a4,1x, a24, 1x,a12, 1x,3i5, 20f10.1) 
 
  331 format(/,
     1  'Item       ,   StateCU,  StateMod,     Delta,   Delta %')
  375  format(a12,',', 3(f10.0,','), f10.0)
     
     
 900  format(
     1 ' outRchR; Problem key output variables for ', a10, 
     1 ' were developed',/
     1 10x,'Based on ', i5, ' output variables but ndivO = ', i4,/
     1 10x,'Reconmend you revise outRchR appropriately')
c
c ____________________________________________________
c
c       Error Processing

  270 write(6,*)  '   outRchR; Requested data exceeds binary file size'
      write(99,*) '   outRchR; Requested data exceeds binary file size'
      goto 9999

 9999 write(6,*)
      write(nlog,*) '  Stopped in outRchR'
      write(6,*) 'Stop 1' 
      call flush(6)
      call exit(1)

      stop 
c
c ____________________________________________________
c
      end






