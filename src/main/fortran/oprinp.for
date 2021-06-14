c oprinp - reads operational right data
c_________________________________________________________________NoticeStart_
c StateMod Water Allocation Model
c StateMod is a part of Colorado's Decision Support Systems (CDSS)
c Copyright (C) 1994-2021 Colorado Department of Natural Resources
c 
c StateMod is free software:  you can redistribute it and/or modify
c     it under the terms of the GNU General Public License as published by
c     the Free Software Foundation, either version 3 of the License, or
c     (at your option) any later version.
c 
c StateMod is distributed in the hope that it will be useful,
c     but WITHOUT ANY WARRANTY; without even the implied warranty of
c     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c     GNU General Public License for more details.
c 
c     You should have received a copy of the GNU General Public License
c     along with StateMod.  If not, see <https://www.gnu.org/licenses/>.
c_________________________________________________________________NoticeEnd___
c
      SUBROUTINE OPRINP(IIN)
c ______________________________________________________________________
c	Program Description
c       Oprinp; It reads operational right data
c ______________________________________________________________________
c  
c     Update History
c
c rrb 2021/04/18; Compiler warning
c
c rrb 2021/02/14 (16.00.47)
c                 Revised to check maximum number of carriers against
c                 maxcary.
c                 Updated Oprinp to include check c17 to
c                 check a type 27 or 28 operating rune if the
c                 destination is a carrier to a Aug plan (type 1)
c                 or a T&C plan (type 2) with a carrier with losses
c                 as a non-fatal error and resetting the loss to 0.
c
c rrb 2021/02/14 (16.00.47)
c                 Revised C5 checks to print to a table in *.chk/',
c                 Updated Oprinp for a type 29 - Plan or Reservoir',/
c                 to print C16 spill destination checks to a table',/
c                 in *.chk as follows:',/
c                 An Admin Plan (type 11) destination is to itself',/
c                 A Changed WR Plan (type 13) is to the downstream node',/
c                 The WWSP Supply (14) and User (15) plans are a ',/
c                 reset only',/
c
c rrb 2020/11/29(16.00.44); 
c                 Update Oprinp for type 48 to require a source',
c                 plan type be a type 4 - Reuse to a diversion, 
c                 6 - Reuse to a diversion by Tmtn, 8- Recharge,
c                 11 - Administrative or 13 - Changed WR plan
c                 else warn the user and turn the operating right off
c                 Specifically it cannot be a 1 - T&C plan, 
c                 2 - Aug Plan, 3 - Reuse to a Reservoir,
c                 5 - Reuse to a Reservoir from Tmtn,
c                 7 - Import Plan, 9 - OOP Diversion, 
c                 10 - Special Well Aug Plan, 12 - Release limit
c                 14 - WWSP Supply or 15 - WWSP User
c
c rrb 2020/09/06; Version 16.00.40
c                   Update to use iopdesr() Destination type to
c                   support To_Stream and To_Conduit and  still       
c                   support diversion = To Stream and 
c                   support Divert = To_Conduit
c
c rrb 2020/07/28; Version 16.00.38
c                   Revise Oprinp.f for a type 46 rule to include a 
c                     do loop when checking if the rule has data that 
c                     indicates it should operates less than a full 
c                     month that is not supported
c                   Revise Oprinp.f for a type 45 rule to correct an
c                     error when variable ciospoX5 is set to the plan 
c                     associated with the operating rule being read
c                     specifically revise ip5=iopsou(1,nx) to 
c                     ip5=iopsou(5,nx)
c
c rrb 2020/03/01; Version 16.00.27',/
c                   Revised Oprinp to require a type 46, 51, 52 and 53',/
c                     to enter multiple as the destination to simplify',/
c                     the logic used to read the operating rights',/
c
c rrb 2020/02/24; Version 16.00.26',/
c                   Revise a type 45 to operate with
c                     the source equal to a diversion water right 
c                     without a carrier
c
c rrb 2020/01/24; Version 16.00.23 
c                   Revise a type 45 to operate with
c                     the source equal to a reservoir water right 
c                     without a carrier and operate a spill order by 
c                     setting oprimit = 7.  Specifically store from 
c                     river into one reservoir account and spill from
c                     another reservoir account
c
c rrb 2019/08/11; Version 16.00.15 
c                   Revise check in Step C13 to not',/
c                     allow iopsou(2,k1) to be reset when checking ',/
c                     WWSP data',/
c
c rrb 2019/07/28; Use iwwPlan=n to indicate we have a WWSP plan being
c                 operated.  Note iwwPlan is the month the type 46 
c                 multi-split was operated for a WWSP plan and 
c                 is used in BomSec to know when to initialize key 
c                 WWSP plan variables.
c
c rrb 2019/07/21; Set iwwPlan=1 to indicate we have at least one 
c                 type 45 direct diversion that is part of a WWSP Plan
c                 to be used in BomSec to know when to initialize
c
c rrb 2019/05/26; Revised type 29 to require ciopde = -1 when
c                  there is a reset (no physical release)
c                 Revised type 46 to allow a WWSP Supply (14) Plan
c                   and a WWSP User (15) plan
c
c rrb 2019/04/20; Revise to recognize a WWSP Supply plan is a plan 
c                  type 14 and a WWSP User plan is a plan type 15
c
c rrb 2019/01/17; Add a check for WWSP input data
c
c rrb 2018/12/08; Revise type 52 to read a WWSP plan associated with a
c                 multi reservoir bookover
c rrb 2018/10/19; Revise type 29 to allow source 1 to be a WWSP plan
c                 that is being reset (no physical release).
c
c rrb 2018/08/24; Revise type 45 to allow two Type 45 limits when
c                 ioprlim = 14
c rrb 2018/08/24; Added type 53 Winter Water Storage Program
c rrb 2018/08/06; Added type 52 Multiple Reservoir Bookover
c rrb 2018/07/29; Added type 51 Flow Reservoir Control
c jhb 2014/10/31; skip reading secondary records if ioprsw()=0
c                 only allow plan type 11 as a type 35 destination
c rrb 2008/01/02; Move tie to operating rules to SetPlanO
c rrb 2006/11/14; All rules are read by Oprfind
c rrb 06/23/2006; Add type 39 Alternate Point and
c                 iopdesr(k) = the destination type (1=isf, 2=reservoir,
c                 3=diversion, ...
c rrb 06/14/2006; Add type 38 Out-of-Priority Diversion
c rrb 06/01/21; Add type 37 Well Augmentation
c rrb 06/01/18; Add type 36 Seasonal Diversion 
c rrb 04/12/30; Add type 27 Plan to a Diversion and Type 28 Plan to a 
c               diversion by exchange
c rrb 04/11/28; Add type 23 Downstream Call (variable amount and admin date)
c rrb 04/08/24; Add type 23 Downstream Call (variable amount and admin date)
c		idcall = 0 no Type 23 right (downstream call)
c		idcall = k = pointer to the operating right that is a 
c                            downstream call
c rrb 04/06/20; Type 25 Direct Flow Bypass to a Diversion, 
c                       Reservoir or carrier
c rrb 04/06/20; Type 24 Direct Flow Exchange to a Diversion, 
c                       Reservoir or carrier
c rrb 04/06/20; Type 23 Downstream call
c rrb 03/08/18; Revise to allow random file read
c rrb 02/10/22; Allow type 10 to handle monthly on/off switches
c rrb 02/02/25; Revised to allow ieffmax = 2 
c               which allows IWR data to be read and limit',/
c               reservoir releases to occur only when an IWR',/
c               exists if the reservoir release variable is > 0',/
c rrb 01/08/23; Began to use variable iopdes(6,k) as a code to 
c               indicate type of release 
c               =0 release to meet demand
c               = -1 release to meet CU of structure
c               = +n release only if a CIR (IWR) exists
c               Note iopsou(6,k) is set to iopsou(4,k) because 
c               iopsou(4,k) is the correct variable to read
c               and because iopsou(4,k) is
c               sometimes used when reservoir releases are tied to 
c               another right (see type 6) or for depletion Vs
c               diversion offset (see type 4) or for a second reservoir
c               (see type 2?)
c rrb 01/12/26; Remove equivalence & revised dumx = dumc & idumx=idumc
c rrb 01/08/18; Reset iopdes(2,k) = 1 rather than stop
c rrb 01/01/18; Type 22 Soil Moisture Use
c rrb 01/01/05; Type 21 Sprinkler Use
c rrb 00/11/03; Type 20 San Juan RIP operation of Navajo begun
c rrb 00/11/02; Added check that iopdes(2,k) (destination account)
c                 is not zero as appropriate (see 924)
c rrb 00/02/21; Type 19 Split Channel started
c rrb 99/11/23; Type 18 Rio Grande Compact for Conejos 
c rrb 99/09/14; Type 17 Rio Grande Compact for Rio Grande
c rrb 99/08/30; Type 16 Direct Flow Storage added
c rrb 99/05/24; Type 15 Interruptible supply added
c rrb 99/08/10; Type 14 Carrier with an annual limit on diversion
c                       when iopsou(2,k) .gt. 1)
c rrb 99/08/30; Began to simplify input with type 15 and 16 approach
c                 to read data
c ______________________________________________________________________
c	Documentation               
c               Read operational data
c               Type 1  From Reservoir to Instream
c               Type 2  From Reservoir to Diversion or Reservoir
c                       by River
c               Type 3  From Reservoir to Diversion by Carrier
c               Type 4  From Reservoir to Diversion by Exchange
c               Type 5  From Reservoir to Reservoir by Exchange
c               Type 6  From Reservoir to Reservoir by Bookover
c               Type 7  From Reservoir to Diversion or Reservoir
c                       by Carrier by Exchange
c               Type 8  From Reservoir to Reservoir by an
c                       Out of Priority water right
c               Type 9  From Reservoir to River to meet a target
c               Type 10 From Reservoir to Diversion by River for
c                       a general replacement
c               Type 11 From Water Right to Diversion or Reservoir
c                       by a carrier
c               Type 12 Reoperation right
c               Type 13 From River to Instream Flow constrained by
c                       an Indexed flow
c               Type 14 Same as 11 but demands may be constrained
c               Type 15 Interruptible supply & alternate point
c               Type 16 Direct flow storage
c               Type 17 Rio Grande Compact for Rio Grande
c               Type 18 Rio Grande Compact for Conejos
c               Type 19 Split Channel
c               Type 20 San Juan RIP
c               Type 21 Sprinkler use
c               Type 22 Soil Moisture Storage
c		            Type 23 Downstream Call (variable date and amount)
c               Type 24 Direct Flow Exchange to a Diversion, 
c                       Reservoir or Plan with or without a Carrier
c		            Type 25 Direct Flow Bypass to a Diversion,
c                       Reservoir or Plan with or without a Carrier
c		            Type 26 Reservoir or Plan to a T&C or Well Aug Plan 
c		            Type 27 ReUse Plan to a diversion direct for Reuse
c		            Type 28 ReUse Plan to a diversion by exchange for Reuse
c		            Type 29 Plan or Plan and Reservoir spill
c		            Type 30 Re Store water released for a T&C plan
c		            Type 30 Carrier with reuse
c ______________________________________________________________________
c	Dimensions
      include 'common.inc'
      real*8 rtem
c
c rrb 2021/04/18; Compiler warning
      real*8 ropnk1, ropsrc, c8, small8
      
      character*12 cidvri,  ciopde,  ciopso1, ciopso2, ciopso3,
     1             ciopso4, ciopso5, blank,   czero,  cx, cx2
      character recin*256, creuse*12, rec12*12, rec2*2,
     1          csource*12,cdest*12,  rec132*132, rec1*1,
c
c rrb 2015/02/03; Add additional constraint capability     
cx   1          cassoc*12, cdestyp*12
     1          cAssoc*12, cdestyp*12, cAssoc2*12, cAssoc3*12,
c
c rrb 2021/02/11; Additional rec12 variable = rec12b
     1          rec12b*12
c
c rrb 2018/07/15; Increase dimension for 60 operating rules
cx    dimension ntype(50), oprtype(50)  
      dimension ntype(60), oprtype(60) 
c
      character oprtype*25
c rrb 209/01/26; Correction; initialize ntype to a dimension of 50
c rrb 2018/07/15; Increase dimension for 60 operating rules
cx      data ntype/50*0/
      data ntype/60*0/
      data oprtype/
     1  'Res to ISF',                  'Res to Diversion, etc.',
     1  'Res to Carrier',              'Res Exch to a Diversion',
     1  'Res to Storage Exchange',     'Bookover',    
     1  'Res to a Carrier by Exch',    'OOP Bookover',
     1  'Release to Target',           'General Replacement Res',
     1  'Carrier to Ditch or Res',
     1  'Reoperate',                   'La Plata Compact',
     1  'Carrier with Const Demand',   'Interruptible Supply',
     1  'Direct Flow Storage',         'Rio Grande Compact - RG',
     1  'Rio Grande Compact - CR',     'Split Channel',
     1  'San Juan RIP',            
     1  'Wells with Sprikler Use',    
     1  'Soil Moisture Use',           'Downstream Call',
     1  'Direct Flow Exchange',        'Direct Flow Bypass',
     1  'Direct Water Right',          'Plan Use Direct',
     1  'Plan Use Exchange',           'Plan Spill',
     1  'Reservoir Re Diversion',      
     1  'Carrier with Reuse',          'Reuse Plan Direct',  
     1  'Reuse Plan Exchange',         'Bookover with Reuse',     
     1  'Import with Reuse',           'Seasonal Water Right',  
     1  'Augmentation Well',           'OOP Diversion',
     1  'Alternate Point Diversion',   'South Platte Compact', 
     1  'Storage w/ Special Limits',   'Plan Reset',
     1  'In-Priority Well Supply',     'Recharge Well', 
     1  'Carrier with Loss',           'Multiple Ownership',
     1  'Admin Plan Limits',           'Reuse to a Plan Direct',
     1  'Reuse to a Plan Exchange',    'South Platte Compact',
     1  'Flow-Reservoir Control',      'Multiple Res Bookover',
     1  'JMartin-Storage',             'JMartin-Flow Partition',
     1  ' ', ' ',' ', ' ', ' ', ' '/
c ______________________________________________________________________
c
c rrb 2021/04/18; Compiler warning
      ndd=0
      isa=0
      ip=0
      nds=0
      nx5=0
      iin2=0
      small8=0.001
      
c		Step 1; Detailed Checks

c		iout = 0 no details
c		       1 details
c          2 summary (echo input and opr right type)
c		       3 summary of on/off dates
c		iecho= 0 do not echo *.opr input to *.chk
c		       1 do echo *.opr input to *.chk
c		iechoX=0 do not echo *.opr input to *.chk
c		       1 do echo *.opr input to *.chk
c		iechoX 2 do echo *.opr and include a header (____)
c			       above a comment
c		ioutSm 0 no details on small
c		       1 details on small
c		ioutLim 0 no details on diversion limit
c		        1 details on diversion limit
      iout=0
      ioutSM=0  
      ioutLim=0
c
      nout=0
c
c rrb 2018/04/08; Reduce output to *.log                                             
cx    iecho=1
      iecho=0
      iechoX=iecho
      rewind(ntmp)
      iok29=0
c ______________________________________________________________________
c		Step 1; Print Subroutine Name      
      write(nlog,102)
      write(6,102)      
  102   format(/,72('_'),/,
     1 '  Oprinp; Operational Right File (*.opr) ')
      if(iout.eq.1) write(nlog,*) ' Subroutine Oprinp'      
c     write(nlog,*) '  Oprinp; iout = ', iout
c ______________________________________________________________________
c		Step 2; Initialize      
      blank = '            '                                            
      czero = '0           '          
      cx='NA'
      nchk2=nlog                                       
      irepn = 0
      numrg = 0
      iprinto = 0
      iprinta = 0
      iprintf = 0
      idcall=0
      noprwr=0
      ideplete=0
      iexchang=0
      iwarno=0
      koff=0
      isp1=0
      irg1=0
      irg2=0
      iops1=0
      iops2=0
c
c ---------------------------------------------------------------
c rrb 2019/07/21; Initialize iwwPlan to 0.  Note it is set to 1
c                 if there is a type 46 direct diversion that 
c                 is associated with a wwsp Supply (type 14) plan
      iwwPlan=0
      small=0.001
      smallN=-1.0*small
c
c rrb 2015/03/24; Add file type warning indicator
      ioutW1=0
      if(ioutSm.eq.1) write(nlog,*) ' Oprinp_01; small ', small
      DO ND=1,NUMDIV
        IDRGST(ND)=0
      enddo  
c ______________________________________________________________________
c		Initialize every operating rule
cx    write(nlog,*) ' Oprinp_0; maxopr ', maxopr
      do k=1,maxopr
cx        write(nlog,*) ' '
cx        write(nlog,*) ' Oprinp x1; k = ', k
        divdS(k)=0.0
        divdE(k)=0.0
        ireuse1=0
        ireuse(k)=0
        oprmaxA(k)=0.0
        iSetOpr(k)=0
        iopdesr(k)=0
        iopsour(k)=0
        divreqa(k) = 0.0
        oprmaxM(k)=0.0
c jhb 2014/10/31 init ioprsw()
        ioprsw(k) = 0
        ioBeg(k) = 0
        ioEnd(k) = 9999
        divOpr(k)= 0
        cdivtyp(k)='Diversion   '
        oprPct(k)=0
        ciopdeX2(k)= 'NA'
        creuseX(k) = 'NA'
        ciopsoX2(k)= 'NA'
        ioprlim(k)=0
        
c rrb 209/01/26; Correction; this variable is only dimensioned to 50
cx      ntype(k)=0
c rrb 2011/11/25; Allow up to 10 destinations        
cx      do i=1,5
        do i=1,maxopr2
          ciopsoX(i,k) = 'NA'
          ciopdeX(i,k) = 'NA'
        end do
        do im=1,12
          imonsw(k,im)=1
        end do
        do i=1,10                                                   
          intern(k,i)=0                                                 
          cntern(i)=blank
          ropdes(k,i)=0.0
          internT(k,i)=0          
        end do
c rrb 2012/05/25; Initialize destination and source arrays
        do i=1,maxopr2
          iopdes(i,k)=0
          iopsou(i,k)=0
        end do
        
cx        write(nlog,*) ' Oprinp; k = ', k
      end do  
      if(ioutSm.eq.1) write(nlog,*) ' Oprinp_02; small ', small
c ______________________________________________________________________
c		Step 3; Open file      
c     write(nlog,200)
c     write(6,200)
      iin2=iin
c ______________________________________________________________________
c rrb 2008/02/22; Read new or old response file formats (infile)
c		  add path (fpath1), open file (filena), get
c		  number of stations (numopr) and get 
c		  version number (ioprX)
      inf=13
      nf=55
      call GetFile(
     1   nlog, nchk, iin, infile, maxfile, maxfn, 
     1   nf, ioprX, numOpr, fileType(inf), fileSuf(inf), 
     1   fileName(inf), fpath1, filena)
      if(ioutSm.eq.1) then
        write(nlog,*) ' Oprinp_03; small ', small
        write(nlog,*) ' Oprinp_03; numopr ', numopr
        write(nlog,*) ' Oprinp_03; ioprX  ', ioprX
      endif
c ______________________________________________________________________
c rrb 208/09/12; Exit if no file is provided
      if(numOpr.eq.0) goto 500      
c ______________________________________________________________________
c		Check for old file format if not specified
c		Note 13 (itype) is the file # in GetFn
      if(iOprX.eq.0) then
        itype=13
        inX=55
        if(ioutsm.eq.1) write(nlog,*) ' Oprinp_4; small ', small
        call ChkVer(nlog, inX, itype, iOprX, filena) 
           
        if(ioutSm.eq.1) write(nlog,*) ' Oprinp_5; small ', small
        small=0.001
        if(ioutSm.eq.1) write(nlog,*) ' Oprinp_6; small ', small        
      endif  
      if(ioutSM.eq.1) write(nlog,*) ' Oprinp_07; small ', small
      maxops=maxopr*2
      k=0
      iwarn1=0
c ______________________________________________________________________
c		Detailed headers
      if(iout.eq.1) write(nlog,1260)
      if(iecho.eq.1) write(nchk,1260)     
c ______________________________________________________________________
c		Loop for the maximum number of rules
      do 1190 k1=1,maxops
c
c rrb 2018/08/19; Initialize
        cAssoc  = 'NA'
        cAssoc2 = 'NA'
        cAssoc3 = 'NA'
        cx      = 'NA'
        cx2     = 'NA'
        
        call comment(55, nlog, iocode, nchk, iechoX)
c       if(iout.eq.1) write(nlog,*) ' Oprinp; iocode ', iocode
        if(iocode.eq.2) goto 1210
        if(iocode.eq.3) goto 928
c ______________________________________________________________________
c               Step 5a; Read data (new format)
        k=k+1
c		Read New Format Only   
        if(iOprX.eq.2) then  
          if(iout.eq.1) write(nchk,*) ' Oprinp; k', k  
          read(55,1321,end=1210,err=928)
     1      cidvri,      nameo(k),    cgoto,
     1      rtem,        dumc,        ioprsw(K),
     1      ciopde,      ciopdes, 
     1      ciopso1,     ciopso1x,    ciopso2, ciopso2x, 
     1      ITYOPR(K),   creuse,      cdivtyp(k), OprLoss(k),
     1      OprLimit(k), ioBeg(k),    ioEnd(k)
c rrb 2008/03/12; Initialize ioprlim     
          iOprLim(k) = int(oprlimit(k))     
          if(iout.eq.1) then
            write(nlog,*)
     1      ' oprinp; k cdivtyp(k) OprLoss(k) OprLimit(k) iOprLim(k)',
     1      k, cdivtyp(k), OprLoss(k), OprLimit(k), iOprLim(k)
            write(nchk,*) ' Oprinp; New ioprX, k', ioprX, k  
          endif
          goto 101
        endif
c ______________________________________________________________________
c		Unknown Format, try to read new one
c		If an error goto to 100 to read old one
        if(iOprX.eq.0) then    
          read(55,1321,end=1210,err=100)
     1      cidvri,      nameo(k),    cgoto,                             
     1      rtem,        dumc,        ioprsw(K),                         
     1      ciopde,      ciopdes, 
     1      ciopso1,     ciopso1x,    ciopso2, ciopso2x, 
     1      ITYOPR(K),   creuse,      cdivtyp(k), OprLoss(k),
     1      OprLimit(k), ioBeg(k),    ioEnd(k)
          if(iout.eq.1)write(nchk,*)
     1      ' Oprinp; cidvri ', cidvri, k,ioBeg(k),ioEnd(k)
c rrb 2008/03/12; Initialize ioprlim     
          iOprLim(k) = int(oprlimit(k))     
c
c rrb 2015/03/24; Revise file type warning
cx          write(nlog,*)
cx     1    ' oprinp; unknown format; k iOprLim(k) oprlimit(k)',
cx     1    k, iOprLim(k), oprlimit(k)
          if(ioutW1.eq.0) then
            write(nlog,1290)
            ioutW1=ioutW1+1
          endif
     
          if(iout.eq.1) write(nchk,*) ' Oprinp; Unknown ioprX, k',
     1      ioprX, k  
          goto 101
        endif
c ______________________________________________________________________
c		Read Old or Unknown Format
 100    continue
        if(iout.eq.1) write(nlog,*) ' Oprinp; backspace 55'
        if(iOprX.eq.0) backspace(55)
        if(iOprX.eq.0 .or. iOprX.eq.1) then     
          read(55,1322,end=1210,err=928)
     1      cidvri,      nameo(k),    cgoto,                             
     1      rtem,        dumc,        ioprsw(K),                         
     1      ciopde,      ciopdes, 
     1      ciopso1,     ciopso1x,    ciopso2, ciopso2x,
     1      ITYOPR(k) 
          creuse=    'NA          '
          OprLoss(k)=0.0
          OprLimit(k)=9999.
c rrb 2008/03/12; Correction          
c         ioprlim(k) = 9999
          ioprlim(k) = 0
          cdivtyp(k) = 'Diversion   '
          if(iout.eq.1) write(nchk,*) ' Oprinp; Old ioprX, k', ioprX, k  
c
c rrb 2015/03/24; Revise file type warning          
cx          write(nlog,*)
cx     1    ' Oprinp; old format; k iOprLim(k) oprlimit(k)',
cx     1    k, iOprLim(k), oprlimit(k)
c
c rrb 2015/03/24; Revise file type warning
          if(ioutW1.eq.0) then
            write(nlog,1290)
            ioutW1=ioutW1+1
          endif
          goto 101
        endif
c ______________________________________________________________________
c               Step 5b; Initialize selected variables        
c		         Irregardless of the file type read
 101    continue
        if(cdivtyp(k).eq.'            ') cdivtyp(k)='Diversion   '
        if(iout.eq.1) write(nlog,*)'  Oprinp; cidvri = ', cidvri     
        if(ioutSm.eq.1) write(nlog,*) ' Oprinp_11; small ', small
        small=0.001
        if(ioutSm.eq.1) write(nlog,*) ' Oprinp_12; small ', small
        if(ityopr(k).eq.11) then             
          cz=small-oprlimit(k)
          if(ioutSm.eq.1) write(nlog,*) ' Oprinp_13; small ', 
     1      k, ityopr(k),oprlimit(k),small,cz
          if(OprLimit(k).lt.small) then
            oprLimit(k)=9999.
          endif  
        endif    
        if(ioutSm.eq.1) write(nlog,*) ' Oprinp_14; small ', small
c rrb 2008/03/20; Initialize associated operating rule
        cAssoc= 'NA'   
c
c rrb 2015/02/03; Allow additional constraints to be specified
        cAssoc2='NA'
        cAssoc3='NA'     
c ______________________________________________________________________
c               Step 6; Process Eof or End
        if(cidvri.eq.blank .or. cidvri.eq.czero .or. 
     1     cidvri(1:3).eq.'End') then
c
c rrb 2021/04/18; Compiler warning
cx         k=amax0(k-1,0)
           k=max(k-1,0)
           goto 1210
        endif   
c ______________________________________________________________________
c		Step 7; Set beginning and ending data if data is provided
c           in the old format (w/o ioBeg and ioEnd)
c           Note execut handles ioBeg and ioEnd during
c           simulation
        if(ioBeg(k)+ioEnd(k).le.0) then
          if(ioprsw(k).eq.0) then
            ioBeg(k) = 0
            ioEnd(k) = 0
          endif
          if(ioprsw(k).eq.1) then
            ioBeg(k) = 0
            ioEnd(k) = 9999
          endif
          if(ioprsw(k).lt.0) then
            ioBeg(k) = 0
            ioEnd(k) = -1*ioprsw(k)
          endif
          if(ioprsw(k).gt.1) then
            ioBeg(k) = ioprsw(k)
            ioEnd(k) = 9999
          endif
        endif
        if(iout.eq.3) then
          write(nlog,105) k, cidvri, ioprsw(k), iobeg(k), ioend(k)
        endif  
c ______________________________________________________________________
c rrb 2006/03/20; 
c		Step 8; Adjust character strings to left     
        cidvri=adjustl(cidvri)  
c       cgoto=adjustl(cgoto) 
        ciopso1=adjustl(ciopso1)
        ciopso2=adjustl(ciopso2)
        creuse=adjustl(creuse)    
c ______________________________________________________________________
c		Step 9; Set integer data     
        iopdes(2,k)=ifix(ciopdes)
        iopsou(2,k)=ifix(ciopso1x)
        iopsou(4,k)=ifix(ciopso2x)
        iops2=iopdes(2,k)
        rec12=cdivtyp(k)
        if(rec12(1:9).eq.'Depletion') ideplete=1
        if(ityopr(k).eq.24 .or. ityopr(k).eq.25) iexchang=1
c ______________________________________________________________________
c		Step 10; Set NA data        
        NAuse=0
        if(creuse(1:3) .eq. 'N/A') creuse(1:12) = 'NA          '
        if(creuse(1:3) .eq. '   ') creuse(1:12) = 'NA          '
        if(creuse(1:2) .eq. 'NA') NAuse=1        
        NAs2=0
        if(ciopso2(1:3).eq.'N/A') ciopso2(1:12) = 'NA         '
        if(ciopso2(1:2).eq.'0 ')  ciopso2(1:12) = 'NA         '
        if(ciopso2(1:2).eq.'  ')  ciopso2(1:12) = 'NA         '
        if(ciopso2(1:2).eq.'NA')  NAs2=1
        if(ciopde(1:3).eq.'N/A') ciopde(1:12) = 'NA          '
c ______________________________________________________________________
c		Step 10a; Detailed output to log     
      if(iout.eq.1) write(nlog,1324)
     1       cidvri,      nameo(k),    cgoto,                             
     1       rtem,        dumc,        ioprsw(K),                         
     1       ciopde,      ciopdes, 
     1       ciopso1,     ciopso1x,    ciopso2, ciopso2x, 
     1       ITYOPR(K),   creuse,      cdivtyp(k), OprLoss(k),
     1       OprLimit(k), ioBeg(k),    ioEnd(k), k
c ______________________________________________________________________
c		Step 10b; Detailed output to check
      if(iecho.eq.1) write(nchk,1324)
     1       cidvri,      nameo(k),    cgoto,                             
     1       rtem,        dumc,        ioprsw(K),                         
     1       ciopde,      ciopdes, 
     1       ciopso1,     ciopso1x,    ciopso2, ciopso2x, 
     1       ITYOPR(K),   creuse,      cdivtyp(k), OprLoss(k),
     1       OprLimit(k), ioBeg(k),    ioEnd(k), k
c ______________________________________________________________________
c		Step 11; Set source and destination strings for 
c                        plan reporting     
        ciopdeX(1,k) = ciopde
        ciopdeX2(k)='NA'
        ciopsoX(1,k) = ciopso1
        creuseX(k) = creuse
        ciopsoX2(k)= ciopso2
c ______________________________________________________________________
c		Sep 12; Set Initialization for types 13, 14 and 15
        if(ityopr(k).eq. 13) iSetOpr(13)=1
        if(ityopr(k).eq. 14) iSetOpr(14)=1
        if(ityopr(k).eq. 15) iSetOpr(15)=1
        if(ityopr(k).eq. 47) iSetOpr(47)=1     
        
c
c rrb 2018/08/05; Direct Flow control (type 51)
        if(ityopr(k).eq. 51) isetOpr(51)=k
c
c rrb 2019/07/21; Set diversion by carrier (type 45)
        if(ityopr(k).eq.45) iSetOpr(45)=1 
c ______________________________________________________________________
c rrb 01/08/23; Step 13; Set release type switch 
c               iopsou(6,k) = iopsou(4,k) 
c               Note use iopsou(6,k) because iopsou(4,k) may be 
c               used for another purpose later.
        if(ityopr(k).eq. 2 .or. ityopr(k).eq.3 .or.        
     1     ityopr(k).eq. 7 .or. ityopr(k).eq.10) then
          iopsou(6,k) = iopsou(4,k)
          if(iopsou(6,k).gt.0) then 
            iopsou(4,k)=0
c rrb 02/02/25; Test to allow ieffmax = 2 (read IWR for daily running
c               demand but do not use variable efficiency capability)
c           if(ieffmax.ne.1) then
            if(ieffmax.le.0) then
              iopsou(6,k)=0
            endif
          endif
        endif
c ______________________________________________________________________
c               Step 14; Estimate "End of File" if a blank is read
        if(cidvri.eq.blank .or. cidvri.eq.czero) goto 1210
c ______________________________________________________________________
c               Step 15; Set the ID (corid), right (rionk) and 
c                        counter (idumc)
        corid(k)=cidvri
        ropnk(k)=rtem
        idumc=ifix(dumc)
        
        if(ityopr(k).le.0 .or. ityopr(k).gt.maxoprin) then
          write(nlog,590) cidvri, ityopr(k), maxoprin
          goto 9999
        endif
        ityopr1=ityopr(k)
        ntype(ityopr1)=ntype(ityopr1) + 1
c ______________________________________________________________________
c       Step 16; Process a right that is off
c         Read misc data so checks can be skipped
        if(ioprsw(k).eq.0) then
c ______________________________________________________________________
c jhb 2014/10/31 start
c         this block of secondary record processing code is not working
c           so skip it, and require NO secondary records when
c           op rule is turned off
c         leave the code below so this block can be corrected and
c           added back later if desired
c ______________________________________________________________________
c         added the records not inside if blocks below,
c           in case they are needed
          koff=koff+1
          ioprloss=int(oprloss(k))
c ______________________________________________________________________
c         read the next record
          goto 1190
c ______________________________________________________________________
c jhb 2014/10/31 end
c         iout=1
          if(iout.eq.1) then
            write(nchk,*) ' Oprinp; Right Off', k, cidvri
            write(nlog,*) ' Oprinp; Right Off', k, cidvri        
          endif
          koff=koff+1
c ______________________________________________________________________
c		Read Monthly on/off          
          if(idumc.eq.12 .or. idumc.lt.-12) then
            if(iout.eq.1) write(nchk,*) ' Oprinp; Monthly on-off ',
     1       idumc
            read(55,'(a256)',end=926,err=928) rec256
c           write(nlog,*) ' Monthly on/off data'
c           write(nlog,*) rec256
            if(iecho.eq.1) write(nchk,'(a256)') rec256
          endif  
c _________________________________________________________
c		Read Carrier Data (without loss)
          ioprloss=int(oprloss(k))
          if(ioprloss.eq.0) then
            if(iout.eq.1) write(nchk,*) ' Oprinp; Carrier No Loss ',
     1          ioprloss, idumc
            idumy=idumc
c
c rrb 2021/04/18; Compiler warning
cx          idumy=amax0(idumc, iabs(idumc)-12)
            idumy=max(idumc, iabs(idumc)-12)
c rrb 2008/06/04; Correction            
            if(idumc.eq.12) idumy=0
c
c ---------------------------------------------------------
c rrb 2021/02/14; Check carrier dimension
            if(idumy.gt.maxcary) then
              write(nlog,1186) corid(k), idumy, maxcary
              goto 9999
            endif
c
            if(idumy.gt.0) then
              do i=1,idumy
                if(iout.eq.1) write(nchk,*) ' Oprinp; idumy', idumy
                read(55,'(a256)',end=926,err=928) rec256
                if(iecho.eq.1) write(nchk,'(a256)') rec256
              end do  
            endif  
          endif
c ______________________________________________________________________
c		Read Carrier Data (with loss)
          ioprloss=int(oprloss(k))
          if(iabs(ioprloss).gt.0) then
            if(iout.eq.1) write(nchk,*) ' Oprinp; Carrier with Loss ',
     1          ioprloss, idumc
            idumy=idumc
c
c rrb 2021/04/18; Compiler warning
cx          idumy=amax0(idumc, iabs(idumc)-12)
            idumy=max(idumc, iabs(idumc)-12)
c rrb 2008/06/04; Correction            
            if(idumc.eq.12) idumy=0
c
c ---------------------------------------------------------
c rrb 2021/02/14; Check carrier dimension
            if(idumy.gt.maxcary) then
              write(nlog,1186) corid(k), idumy, maxcary
              goto 9999
            endif
            
            if(idumy.gt.0) then
              do i=1,idumy
                read(55,'(a256)',end=926,err=928) rec256
                if(iecho.eq.1) write(nchk,'(a256)') rec256
              end do  
            endif            
          endif  
cr          if(iout.eq.1) write(nlog,*) rec256          
cr          write(nlog,*) '  Oprinp; rec256 ', rec256
c ______________________________________________________________________
c		Rio Grande Compact Treatment          
c		Read extra stream gage data
          if(ityopr(k).eq.17 .or. ityopr(k).eq.18) then 
            read(55,'(a256)',end=926,err=928) rec256
            if(iecho.eq.1) write(nchk,'(a256)') rec256
          endif
c ______________________________________________________________________
c		Type 24 and 25 Exchange to ....
c		Read Exchnage limits          
          if(ityopr(k).eq.24 .or. ityopr(k).eq. 25) then
            if(iout.eq.1)  write(nchk,*) ' Oprinp; Exchange Limits ', 
     1        k, ityopr(k)
            read(55,'(a256)',end=926,err=928) rec256     
            if(iecho.eq.1) write(nchk,'(a256)') rec256           
          endif  
c ______________________________________________________________________
c rrb 2008/02/21;  
c		Type 27 and 28 Plan and 45 with an operating rule limit
          if(ityopr(k).eq.27 .or. ityopr(k).eq. 28) then
            if(iout.eq.1) write(nchk,*) ' Oprinp; Associated Opr Rule ', 
     1        k, ityopr(k), oprlimit(k)
c ______________________________________________________________________
c		              Associated Operating Rule
            if(oprlimit(k).gt.small .and. oprlimit(k).lt.9999) then
              read(55,'(a256)',end=926,err=928) rec256     
              if(iecho.eq.1) write(nchk,'(a256)') rec256           
            endif
c ______________________________________________________________________
c		              T&C CU factors            
c rrb 2008/04/23; Revise to be more robust 
            if(iopsou(4,k) .gt. 0) then              
              if(iout.eq.1) write(nchk,*) ' Oprinp; T&C CU Factors ', 
     1          k, iopsou(4,k), nas2
c rrb 2008/11/25; Revise to warn and allow a bad iopsou(4 entry when 
c		              Nas2=1 (ciopso2=NA)            
              if(nas2.eq.1) then
                write(nlog,916) corid(k), ityopr(k),
     1            ciopso2, iopsou(4,k) 
              endif
              if(nas2.eq.0) then
                read(55,'(a256)',end=926,err=928) rec256     
                if(iecho.eq.1) write(nchk,'(a256)') rec256           
              endif  
            endif
          endif 
c ______________________________________________________________________
c rrb 2011/10/15; Update to type 45 operating rule that allows 
c		              an operating rule limit
          if(ityopr(k).eq.45) then
            if(iout.eq.1) write(nchk,*) ' Oprinp; Associated Opr Rule ', 
     1        k, ityopr(k), oprlimit(k)
c		              Associated Operating Rule
            if(oprlimit(k).gt.small .and. oprlimit(k).lt.9999) then
              read(55,'(a256)',end=926,err=928) rec256     
              if(iecho.eq.1) write(nchk,'(a256)') rec256           
            endif
          endif       
c ______________________________________________________________________
c rrb 2008/02/21;           
c		Type 10 and 29 Spill
c		Associated Operating Rule Data (5)
          if(ityopr(k).eq.10 .or. ityopr(k).eq.29) then
c		Monthly and Annual Limit Data
            if(oprlimit(k).gt.small .and. oprlimit(k).lt.9999) then
              read(55,'(a256)',end=926,err=928) rec256     
c             write(nlog,*) ' Associated Operating Rule'
c             write(nlog,*) rec256
              if(iecho.eq.1) write(nchk,'(a256)') rec256           
            endif
          endif  
c ______________________________________________________________________
c         finished processing secondary records when opr rule is OFF
          goto 1190
        endif
c ______________________________________________________________________
c		Step 17; Branch for operating rule specific processing
c               For type 1, Reservoir to a ISF 
        if (ityopr(k).eq.1) goto 1001
c               For type 2, Reservoir to diversion or reservoir or Carrier
        if (ityopr(k).eq.2) goto 1002
c               For type 3, Reservoir to a carrier
        if (ityopr(k).eq.3) goto 1003
c               For type 4, Reservoir to a diversion by Exchange
        if (ityopr(k).eq.4) goto 1004
c               For type 5, Reservoir storage by Exchange
        if (ityopr(k).eq.5) goto 1005
c               For type 6, Reservoir to Reservoir Transfer
        if (ityopr(k).eq.6) goto 1006
c               For type 7, Diversion by Carrier by Exchange
        if (ityopr(k).eq.7) goto 1007
c               For type 8, Out of Priority Bookover
        if (ityopr(k).eq.8) goto 1008
c               For type 9, Release to Target
        if (ityopr(k).eq.9) goto 1009
c               For type 10, Replacement Reservoir
        if (ityopr(k).eq.10) goto 1010
c               For type 11, Carrier
        if (ityopr(k).eq.11) goto 1011
c               For type 12, reoperation right, we're done
        if (ityopr(k).eq.12) goto 1190
c               For type 13, index River Flow
        if (ityopr(k).eq.13) goto 1013
c               For type 14, Carrier with a Constrained Demand
        if (ityopr(k).eq.14) goto 1014
c               For type 15, interruptible supply, process in 1 place
        if(ityopr(k).eq.15) goto 1015
c               For type 16, direct flow storage, process in 1 place
        if(ityopr(k).eq.16) goto 1016
c               For type 17, Rio Grande Compact-RG, process in 1 place
        if(ityopr(k).eq.17) goto 1017
c               For type 18, Rio Grande Compact-Co, process in 1 place
        if(ityopr(k).eq.18) goto 1018
c               For type 19, Split Channel, process in 1 place
        if(ityopr(k).eq.19) goto 1019                         
c               For type 20, San Juan RIP for Navajo 
        if(ityopr(k).eq.20) goto 1020                         
c               For type 21, Sprinkler Use
        if(ityopr(k).eq.21) goto 1021
c               For type 22, Soil Moisture Use
        if(ityopr(k).eq.22) goto 1022        
c               For type 23, Downstream Call Data
        if(ityopr(k).eq.23) goto 1023        
c               For type 24, Direct Flow Exchange 
        if(ityopr(k).eq.24) goto 1024                
c               For type 25, Direct Flow Bypass
        if(ityopr(k).eq.25) goto 1025        
c               For type 26, Reservoir or Plan to a Plan
        if(ityopr(k).eq.26) goto 1026        
c               For type 27, Plan to a Diversion Direct
        if(ityopr(k).eq.27) goto 1027        
c               For type 28, Plan to a Diversion by Exchange
        if(ityopr(k).eq.28) goto 1028        
c               For type 29, Plan spill
        if(ityopr(k).eq.29) goto 1029        
c               For type 30, Redivert T&C Plan release
        if(ityopr(k).eq.30) goto 1030
c               For type 31, Carrier with Reuse
        if(ityopr(k).eq.31) goto 1031
c               For type 32, Reuse Reservoir and Plan to Diversion, 
c                            Reservoir or Carrier with Reuse Direct
        if(ityopr(k).eq.32) goto 1032
c               For type 33, Reuse Reservoir and Plan to a Diversion, 
c                            Reservoir or Carrier with Reuse by Exchange
        if(ityopr(k).eq.33) goto 1033
c               For type 34, Bookover with Reuse
        if(ityopr(k).eq.34) goto 1034
c               For type 35, Import 
        if(ityopr(k).eq.35) goto 1035
c               For type 36, Meadow Rights
        if(ityopr(k).eq.36) goto 1036
c               For type 37, Well Augmentation
        if(ityopr(k).eq.37) goto 1037
c               For type 38, OOP Diversion
        if(ityopr(k).eq.38) goto 1038
c               For type 39, Alternate Point
        if(ityopr(k).eq.39) goto 1039
c               For type 40, South Platte Compact
        if(ityopr(k).eq.40) goto 1040
c               For type 41, Storage limited by an OOP Plan Volume
        if(ityopr(k).eq.41) goto 1041
c               For type 42, Plan Spill
        if(ityopr(k).eq.42) goto 1042
c               For type 43, In-Priority Supply
        if(ityopr(k).eq.43) goto 1043
c               For type 44, Recharge Well
        if(ityopr(k).eq.44) goto 1044
c               For type 45, Carrier with Losses
        if(ityopr(k).eq.45) goto 1045
c               For type 46, Multiple Ownership
        if(ityopr(k).eq.46) goto 1046
c               For type 47, Administratve Ownership
        if(ityopr(k).eq.47) goto 1047
c               For type 48, Plan or Res. reuse to a Plan Direct
        if(ityopr(k).eq.48) goto 1048
c               For type 49, Plan or Res. reuse to a Plan Exchange
        if(ityopr(k).eq.49) goto 1049
c               For type 50, South Platte compact Storage
        if(ityopr(k).eq.50) goto 1050
c
c rrb 2018/07/29; Add type 51 and 52 operating rules
c               For type 51, Flow-Reservoir Control
        if(ityopr(k).eq.51) goto 1051
c
c               For type 52, Multiple Reservoir AccountBookover
        if(ityopr(k).eq.52) goto 1052
c
c               For type 53, JMartin Storage
        if(ityopr(k).eq.53) goto 1053
c
c               For type 54, JMartin Flow Partition
        if(ityopr(k).eq.54) goto 1054
c        
        write(nlog,1277) ityopr(k),cidvri
        goto 9999
c ______________________________________________________________________
 1001   continue 
c ______________________________________________________________________
c               Type 1; Reservoir to a ISF
c                  ion=1 means turn off opr right if right is off
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        ion=1
        istop=0
        idcdd=0
        iss=0
        idumc=ifix(dumc)
c ______________________________________________________________________
c               a. Read monthly constraints 
        istop=0
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2, ioprsw(k), cidvri)
c ______________________________________________________________________
c               a2. Read intervening structures
        istop=0
        call oprFind(ityopr(k), 21, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2, ioprsw(k), cidvri)
c ______________________________________________________________________
c               c1. Find destination ISF
c                  Note itype=1 for a ISF structure
c                       istop=0 stop if not found)
        itype=1
        istop=0
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1, iopdes(2,k), nx,ciopde, 1, istop, rops2,
     1       ioprsw(k), cidvri)
        iopdes(1,k) = iops1
        iopdesr(k)=1
        if(iops1.gt.0) idcdD=ifrsta(iops1)        
c ______________________________________________________________________
c               d. Find source 1 a reservoir (type 2)
c                  Note itype=2 for a ISF reservoir
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
c		        iacc=0 allows account to be 0 (since 
c                         it is ownership %)
c		        ion=0 leaves the original water right on
        itype=2
        ion=0
        iacc=1
        istop=0
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1,iopsou(2,k), nx, ciopso1, 1, istop, rops2, 
     1       ioprsw(k), cidvri)
     
        iopSouR(k)=itype     
        iopsou(1,k)=iops1
c ______________________________________________________________________
c               e. Check that the source reservoir is upstream 
c                  of the destination ISF
c	           trying to find destination (idcdD)
c                  downstream of  source (iscdS) 
        iss=irssta(iops1)
        ndns=ndnnod(iss)
        csource=cstaid(iss)
        cdest=cstaid(idcdD)
        call oprdown(nlog, maxsta, ndns, iss, idcdD, idncod,
     1       cidvri, csource, cdest)
c ______________________________________________________________________
c		f. Detailed output
        iout01=0
        if(iout01.eq.1) then
          write(nlog,2020) ityopr(k), cidvri, ityopr(k),
     1      ciopde,  iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k), iopsou(2,k),
     1      ciopso2, iopsou(3,k), iopsou(4,k), creuse, ireuse1,
     1      oprlimit(k), iopSou(5,k),
     1      cdivtyp(k), iopdesR(k)
        endif
        goto 1190
 1002   continue 
c ______________________________________________________________________
c               Type 2; Reservoir to a Diversion, Reservoir or Carrier
c                  ion=1 means turn off opr right if right is off
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        ion=1
        istop=0
        idumc=ifix(dumc)
c ______________________________________________________________________
c               a. Read monthly constraints 
        istop=0
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2, ioprsw(k), cidvri)
c ______________________________________________________________________
c               a2. Read intervening structures
        istop=0
        call oprFind(ityopr(k), 21, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2, ioprsw(k), cidvri)
c ______________________________________________________________________
c               c1. Find destination diversion 
c                  Note itype=3 for a diversion 
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        itype=3
        istop=1
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1, iopdes(2,k), nx,ciopde, 1, istop, rops2, 
     1       ioprsw(k), cidvri)
        iopdes(1,k) = iops1
        iopdesr(k)=3
        if(iops1.gt.0) idcdD=idvsta(iops1)        
c ______________________________________________________________________
c               c2. Find destination reservoir (type 2)
c                  Note itype=2 for a reservoir
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        if(iops1.eq.0) then
          itype=2
          istop=0
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         iops1, iopdes(2,k), nx,ciopde, 1, istop, rops2, 
     1         ioprsw(k), cidvri)
          iopdes(1,k)=-iops1
          idcdD=irssta(iops1)                  
          iopdesr(k)=2          
c         write(nlog,*) '  Oprinp; #2 iopdes(1,k) = ',iopdes(1,k),idcdd
        endif
c ______________________________________________________________________
c               c3. If a carrier then reset the destination location
        nc=intern(k,1)
        if(nc.gt.0) then
          idcdD=idvsta(nc)                  
c         write(nlog,*) '  Oprinp; #3 iopdes(1,k) = ',iopdes(1,k),idcdd
        endif
c ______________________________________________________________________
c               d. Find source 1 a reservoir (type 2)
c		   Note ion=0 leaves the original water right on
c		        iacc=0 allows account to be 0 (since 
c                       it is ownership %)
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        ion=0
        iacc=1
        istop=0
        itype=2
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iopsou(1,k),iopsou(2,k), nx, ciopso1, 1, 
     1       istop, rops2, ioprsw(k), cidvri)
c ______________________________________________________________________
c               e. Check that the source reservoir is upstream 
c                  of the destination diversion, reservoir or carrier
c	           trying to find destination (idcdD)
c                  downstream of  source (iscdS) 
        iopSouR(k)=itype
        iops1=iopsou(1,k)
        iss=irssta(iops1)
        ndns=ndnnod(iss)
        csource=cstaid(iss)
        cdest=cstaid(idcdD)
        call oprdown(nlog, maxsta, ndns, iss, idcdD, idncod,
     1       cidvri, csource, cdest)
c ______________________________________________________________________
c		h. Detailed output
        iout02=0
        if(iout02.eq.1) then
          write(nlog,2020) ityopr(k), cidvri, ityopr(k),
     1      ciopde, iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k),iopsou(2,k),
     1      ciopso2, iopsou(3,k),iopsou(4,k), creuse, ireuse1,
     1      oprlimit(k), iopSou(5,k),     
     1      cdivtyp(k), iopdesR(k)
        endif  
        goto 1190
 1003   continue 
c ______________________________________________________________________
c               Type 3; Reservoir to a .Conduit
c                  ion=1 means turn off opr right if right is off
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        ion=1
        istop=0
        idumc=ifix(dumc)
c ______________________________________________________________________
c               a. Read monthly constraints 
        istop=0
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2, 
     1       ioprsw(k), cidvri)
c ______________________________________________________________________
c               a2. Read intervening structures
        istop=0
        call oprFind(ityopr(k), 21, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2, ioprsw(k), cidvri)
c ______________________________________________________________________
c               c1. Find destination diversion 
c                  Note itype=3 for To_Conduit or direct diversion that 
c                  is not delivered by the stream system. 
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        itype=3
        istop=1
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1, iopdes(2,k), nx,ciopde, 1, istop, rops2, 
     1       ioprsw(k), cidvri)
        iopdes(1,k) = iops1
        iopdesr(k)=3        
        if(iops1.gt.0) idcdD=idvsta(iops1)        
c ______________________________________________________________________
c               c2. Find destination reservoir (type 2)
c                  Note itype=2 for a reservoir
c		   Note istop=0 Stop if not found
c		        istop=1 Do not stop if not found
        if(iops1.eq.0) then
          itype=2
          istop=1
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         iops1, iopdes(2,k), nx,ciopde, 1, istop, rops2, 
     1         ioprsw(k), cidvri)
          iopdes(1,k)=-iops1
          idcdD=irssta(iops1)                  
          iopdesr(k)=2          
c         write(nlog,*) '  Oprinp; #2 iopdes(1,k) = ',iopdes(1,k),idcdd
        endif
c ______________________________________________________________________
c rrb 2008/03/21; Revised warning when several destinations are possible        
        if(iops1.eq.0) then
          write(nlog,1300) cidvri, ityopr(k), ciopde
          goto 9999
        endif   
c ______________________________________________________________________
c               c3. If a carrier then reset the destination location
        nc=intern(k,1)
        if(nc.gt.0) then
          idcdD=idvsta(nc)                  
c         write(nlog,*) '  Oprinp; #3 iopdes(1,k) = ',iopdes(1,k),idcdd
        endif
c ______________________________________________________________________
c               d. Find source 1 a reservoir (type 2)
c		   Note ion=0 leaves the original water right on
c		        iacc=0 allows account to be 0 (since 
c                       it is ownership %)
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        ion=0
        iacc=1
        istop=0
        itype=2
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1,iopsou(2,k), nx, ciopso1, 1, istop, rops2, 
     1       ioprsw(k), cidvri)
        iopsou(1,k) = iops1
        iopSouR(k)=itype
c ______________________________________________________________________
c                  Note this check is not required since the release is
c                  To_Conduit or Divert that is not released to the stream system
c               e. Check that the source reservoir is upstream 
c                  of the destination diversion, reservoir or carrier
c	                 trying to find destination (idcdD)
c                  downstream of  source (iscdS) 
c rrb Do not check location type 3 is to a carrier
c       iss=irssta(iops1)
c       ndns=ndnnod(iss)
c       csource=cstaid(iss)
c       cdest=cstaid(idcdD)
c          
c       call oprdown(nlog, maxsta, ndns, iss, idcdD, idncod,
c     1       cidvri, csource, cdest)
c ______________________________________________________________________
c		h. Detailed output
        iout03=0
        if(iout03.eq.1) then
          write(nlog,2020) ityopr(k), cidvri, ityopr(k),
     1      ciopde, iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k),iopsou(2,k),
     1      ciopso2, iopsou(3,k),iopsou(4,k), creuse, ireuse1,
     1      oprlimit(k), iopSou(5,k),     
     1      cdivtyp(k), iopdesR(k)
        endif  
        goto 1190
 1004   continue 
c ______________________________________________________________________
c               Type 4; Reservoir to a Diversion by Exchange
c                  ion=1 means turn off opr right if right is off
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        ion=1
        istop=0
        idumc=ifix(dumc)
c ______________________________________________________________________
c               a. Read monthly constraints 
        istop=0
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2, ioprsw(k), cidvri)
c ______________________________________________________________________
c               a2. Read intervening structures
        istop=0
        call oprFind(ityopr(k), 21, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2, ioprsw(k), cidvri)
c ______________________________________________________________________
c               c1. Find destination diversion 
c                  Note itype=3 for a diversion 
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        itype=3
        istop=1
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1, iopdes(2,k), nx,ciopde, 1, istop, rops2,
     1       ioprsw(k), cidvri)
        iopdes(1,k) = iops1
        idcdD=idvsta(iops1)        
        iopdesr(k)=3
c ______________________________________________________________________
c               c2. Find destination diversion water right iopdes(3,k)
c		                and assign destination (iopdes(1,k) appropriately
c                   Note itype=13 for a diversion water right
c		                     istop=0 Stop if not found
c		                     istop=1 Do not stop if not found
c			                   ion=0 leave the water right on
        if(iops1.eq.0) then
          itype=13
          istop=1
          ion=0
c rrb 2012/05/23; set iopdes(3,lr) to be the water right limit           
cx        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
cx   1       iops1, iopdes(2,k), nx,ciopde, 1, istop, rops2,
cx   1       ioprsw(k), cidvri)
cx        iopdesr(k) = iops1
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1, iopdes(2,k), nx,ciopde, 1, istop, rops2,
     1       ioprsw(k), cidvri)
cx        iopdesr(k) = iops1
          iopdes(3,k)=iops1
          nd=idivco(1,iops1)
          iopdes(1,k)=nd
          iopdesr(k)=3
          if(iops1.gt.0) idcdD=idvsta(nd)        
        endif  
c ______________________________________________________________________
c rrb 2008/03/21; Revised warning when several destinations are possible        
        if(iops1.eq.0) then
          write(nlog,1300) cidvri, ityopr(k), ciopde
          goto 9999
        endif   
c ______________________________________________________________________
c               c3. If a carrier then reset the destination location
        nc=intern(k,1)
        if(nc.gt.0) then
          idcdD=idvsta(nc)                  
cx         write(nlog,*) '  Oprinp; #3 iopdes(1,k) = ',iopdes(1,k),idcdd
        endif
c ______________________________________________________________________
c               d. Find source 1 a reservoir (type 2)
c		   Note ion=0 leaves the original water right on
c		        iacc=0 allows account to be 0 (since 
c                       it is ownership %)
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        ion=0
        iacc=1
        istop=0
        itype=2
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1,iopsou(2,k), nx, ciopso1, 1, istop, rops2, 
     1       ioprsw(k), cidvri)

        iopSouR(k)=itype
        iopsou(1,k)=iops1
        iscdS=irssta(iops1)
c
c ---------------------------------------------------------
c               j. Find the exchange point (iExPoint)
c                  for the source and destination
        
        call oprExp(nlog, maxsta, idcdD, iscdS, idncod, ndnnod, 
     1                    iExPoint(k), cidvri)

c       write(nlog,*) ' Oprinp; cidvri, idcdD, iscdS, iexPoint(k)'
c       write(nlog,*) ' Oprinp;', cidvri, idcdD, iscdS, iexPoint(k)
c	
c
c ---------------------------------------------------------
c		h. Detailed output
     
        iout04=0
        if(iout04.eq.1) then
          write(nlog,2020) ityopr(k), cidvri, ityopr(k),
     1      ciopde, iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k),iopsou(2,k),
     1      ciopso2, iopsou(3,k),iopsou(4,k), creuse, ireuse1,
     1      oprlimit(k), iopSou(5,k),
     1      cdivtyp(k), iopdesR(k)
        endif  

        goto 1190
c                                                                       
c _________________________________________________________
c
 1005   continue 
c 
c               Type 5; Reservoir to a Reservoir by Exchange
c
c                  ion=1 means turn off opr right if right is off
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        ion=1
        istop=0
        
        idumc=ifix(dumc)
c
c ---------------------------------------------------------
c               a. Read monthly constraints 
        istop=0
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2, ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               a2. Read intervening structures
        istop=0
        call oprFind(ityopr(k), 21, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2, ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               c1. Find destination reservoir 
c                  Note itype=2 for a reservoir
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        itype=2
        istop=1
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1, iopdes(2,k), nx,ciopde, 1, istop, rops2, 
     1       ioprsw(k), cidvri)
        iopdes(1,k) = iops1
        iopdesr(k)=itype
        
        if(iops1.gt.0) idcdD=irssta(iops1)        
c
c ---------------------------------------------------------
c               c2. Find destination reservoir right iopdesr(k)
c		                and assign destination (iopdes(1,k) appropriately
c                   Note itype=12 for a reservoir water right
c		   Note istop=0 Stop if not found
c		        istop=1 Do not stop if not found
        if(iops1.eq.0) then
          itype=12
          istop=1
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1, iopdes(2,k), nx,ciopde, 1, istop, rops2, 
     1       ioprsw(k), cidvri)
          
          iopdesr(k) = iops1
          nr=iresco(1,iops1)
          iopdes(1,k)=nr
          if(iops1.gt.0) idcdD=irssta(nr)        
        endif  
c        
c ---------------------------------------------------------
c rrb 2008/03/21; Revised warning when several destinations are possible        
        if(iops1.eq.0) then
          write(nlog,1300) cidvri, ityopr(k), ciopde
          goto 9999
        endif   
c
c ---------------------------------------------------------
c               c3. If a carrier then reset the destination location
cx        nc=intern(k,1)
cx        if(nc.gt.0) then
cx          idcdD=idvsta(nc)                  
cx         write(nlog,*) '  Oprinp; #3 iopdes(1,k) = ',iopdes(1,k),idcdd
cx        endif
c
c
c ---------------------------------------------------------
c               d. Find source 1 a reservoir (type 2)
c		   Note ion=0 leaves the original water right on
c		        iacc=0 allows account to be 0 (since 
c                       it is ownership %)
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found

        ion=0
        iacc=1
        istop=0
        itype=2        
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1,iopsou(2,k), nx, ciopso1, 1, istop, rops2,
     1       ioprsw(k), cidvri)
     
        iopSouR(k)=itype        
        iopsou(1,k)=iops1
        iscdS=irssta(iops1)
c
c ---------------------------------------------------------
c               j. Find the exchange point (iExPoint)
c                  for the source and destination
        
        call oprExp(nlog, maxsta, idcdD, iscdS, idncod, ndnnod, 
     1                    iExPoint(k), cidvri)

c       write(nlog,*) ' Oprinp; cidvri, idcdD, iscdS, iexPoint(k)'
c       write(nlog,*) ' Oprinp;', cidvri, idcdD, iscdS, iexPoint(k)
c	
c
c ---------------------------------------------------------
c		h. Detailed output
     
        iout05=0
        if(iout05.eq.1) then
          write(nlog,2020) ityopr(k), cidvri, ityopr(k),
     1      ciopde, iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k),iopsou(2,k),
     1      ciopso2, iopsou(3,k),iopsou(4,k), creuse, ireuse1,
     1      oprlimit(k), iopSou(5,k),
     1      cdivtyp(k), iopdesR(k)
        endif  

        goto 1190
c                                                                       
c _________________________________________________________
c
 1006   continue 
c 
c               Type 6; Reservoir to a Reservoir Bookover
c
c                  ion=1 means turn off opr right if right is off
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        ion=1
        istop=0
        
        idumc=ifix(dumc)
c
c ---------------------------------------------------------
c               a. Read monthly constraints 
        istop=0
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2, ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               a2. Read intervening structures
        istop=0
        call oprFind(ityopr(k), 21, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2,ioprsw(k),  cidvri)
c
c ---------------------------------------------------------
c               b. Find destination reservoir 
c                  Note itype=2 for a reservoir
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        itype=2
        istop=0
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1, iopdes(2,k), nx,ciopde, 1, istop, rops2, 
     1       ioprsw(k), cidvri)
        iopdes(1,k) = iops1
        iopdesr(k)=2
        
        if(iops1.gt.0) idcdD=irssta(iops1)        

c
c ---------------------------------------------------------
c               c. If a carrier then reset the destination location
        nc=intern(k,1)
        if(nc.gt.0) then
          idcdD=idvsta(nc)                  
c         write(nlog,*) '  Oprinp; #3 iopdes(1,k) = ',iopdes(1,k),idcdd
        endif
c
c
c ---------------------------------------------------------
c               d1. Find source 1 a reservoir (type 2)
c		   Note ion=0 leaves the original water right on
c		        iacc=0 allows account to be 0 (since 
c                       it is ownership %)
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found

        ion=0
        iacc=1
        istop=0
        itype=2        
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1,iopsou(2,k), nx, ciopso1, 1, istop, rops2, 
     1       ioprsw(k), cidvri)
     
        iopSouR(k)=itype     
        iopsou(1,k)=iops1
        iscdS=irssta(iops1)
c
c ---------------------------------------------------------
c               e1. Find source 2 operational right (if any)
c                  Note itype=14 for an operational right
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        itype=14
        istop=1
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1, iopsou(4,k), nx,ciopso2, 1, istop, rops2, 
     1       ioprsw(k), cidvri)
        nr = iops1      
c
c rrb 2006/11/27; Correction and Test for bad iopsou(4,k)  
        if(nr.gt.0) iopsou(3,k) =-((nr*10)+2) 
        
        if(nr.gt.0) then
          if(iopsou(4,k).ne.0) then
            write(nlog,1270) cidvri, ityopr(k), iopsou(4,k)
            goto 9999          
          endif
        endif                  
                                                                  
c
c ---------------------------------------------------------
c               e2. Find source 2 diversion (if any)
c                  Note itype=3 for a diversion
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
c			iacc=0 do not check the account
c			iacc=1 do check the account
c rrb 2006/11/27; Set iacc=0 since the account may be 99 to indicate
c		  iopsou(3,k) is a diversion
        if(iops1.eq.0) then
          itype=3
          istop=1
          iacc=0
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1, iopsou(4,k), nx,ciopso2, iacc, 
     1       istop, rops2, ioprsw(k), cidvri)
          nd = iops1  
c
c rrb 2006/11/27; Correction
c         iopsou(3,k) =-((nd*10)+1)                                                   
          if(nd.gt.0) iopsou(3,k) =-((nd*10)+1)                                                   
        endif  
c	
c
c ---------------------------------------------------------
c		h. Detailed output
     
        iout06=0
        if(iout06.eq.1) then
          write(nlog,2020) ityopr(k), cidvri, ityopr(k),
     1      ciopde, iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k),iopsou(2,k),
     1      ciopso2, iopsou(3,k),iopsou(4,k), creuse, ireuse1,
     1      oprlimit(k), iopSou(5,k),
     1      cdivtyp(k), iopdesR(k)
        endif  

        goto 1190
c                                                                       
c _________________________________________________________
c
 1007   continue 
c 
c               Type 7; Diversion by a Carrier by Exchange
c
c                  ion=1 means turn off opr right if right is off
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        ion=1
        istop=0
        
        idumc=ifix(dumc)
c
c ---------------------------------------------------------
c               a. Read monthly constraints 
        istop=0
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2, ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               a2. Read intervening structures
        istop=0
c jhb 11/20/2014 fix a missing arg error causing intermittent runtime data crashes
c        call oprFind(ityopr(k), 21, idumc,k,ion,iprinto,
c     1       ix, ix, nx, cx, 1, istop, rops2, cidvri)
        call oprFind(ityopr(k), 21, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2, ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               b1. Find destination operational right 
c                   Note itype=14 for an operational right
c		      Note ion=0 leaves the original water right on
c		      Note istop=0 Stop if not found
c		           istop=1 Do not Stop if not found
        itype=14
        istop=1
        ion=0
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1, iopdes(2,k), nx,ciopde, 1, istop, rops2,
     1       ioprsw(k),  cidvri)
        iopdes(1,k) = iops1
c
c		Find destination based on operational right data        
        nd  =IOPDES(1,iops1)
        nr  = -1*nd
        if(nd.gt.0) idcdD=idvsta(nd)
        if(nd.lt.0) idcdD=irssta(nr)
        if(nd.eq.0) then
          write(nlog,939) cidvri, ityopr(k), ciopde
          write(nlog,*) ' Oprinp; iops1, nd, nr', iops1, nd, nr
          goto 9999
        endif
c
c ---------------------------------------------------------
c               c1. If a carrier then reset the destination location
        nc=intern(k,1)
        if(nc.gt.0) then
          idcdD=idvsta(nc)                  
c         write(nlog,*) '  Oprinp; #3 iopdes(1,k) = ',iopdes(1,k),idcdd
        endif
c
c
c ---------------------------------------------------------
c               d. Find source 1 a reservoir (type 2)
c		   Note ion=0 leaves the original water right on
c		        iacc=0 allows account to be 0 (since 
c                       it is ownership %)
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found

        ion=0
        iacc=1
        istop=1
        itype=2        
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1,iopsou(2,k), nx, ciopso1, 1, istop, rops2,
     1       ioprsw(k), cidvri)

        iopSouR(k)=itype
        iopsou(1,k)=iops1
        iscdS=irssta(iops1)
c
c ---------------------------------------------------------
c               j. Find the exchange point (iExPoint)
c                  for the source and destination
        
        call oprExp(nlog, maxsta, idcdD, iscdS, idncod, ndnnod, 
     1                    iExPoint(k), cidvri)

c       write(nlog,*) ' Oprinp; cidvri, idcdD, iscdS, iexPoint(k)'
c       write(nlog,*) ' Oprinp;', cidvri, idcdD, iscdS, iexPoint(k)
c	
c
c ---------------------------------------------------------
c		h. Detailed output
     
        iout07=0
        if(iout07.eq.1) then
          write(nlog,2020) ityopr(k), cidvri, ityopr(k),
     1      ciopde, iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k),iopsou(2,k),
     1      ciopso2, iopsou(3,k),iopsou(4,k), creuse, ireuse1,
     1      oprlimit(k), iopSou(5,k),
     1      cdivtyp(k), iopdesR(k)
        endif  

        goto 1190

c _________________________________________________________
c
c         
 1008   continue 
        
c               Type 8; Out of Priority Bookover 2 where
c                Destination  is a Reservoir or a Plan
c		 Source 1 is an associated OOP reservoir or NA
c		 Source 2 is the subordinated water right
c		 For a reservoir destination Interv is the 
c                  OOP water right
c		 For a plan destination interv is a list
c		   of associated reservoirs
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
c
        ion=1
        istop=0
        iwarnr=0
        iwarno=0
c
c ---------------------------------------------------------
c               a. Read monthly constraints. 
        istop=0
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2, ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               b. Read the Junior water right and
c		     associated OOP diversion
        idumc2=iabs(idumc)-12
c
c rrb 2021/04/18; Compiler warning
cx      idumc3=amax0(idumc, idumc2)
        idumc3=max(idumc, idumc2)
        if(idumc.eq.2 .or. idumc2.eq.2) then
          read(55,'(36x,10a12)',end=2000,err=2000)
     1             (cntern(i),i=1,idumc3)
          if(iecho.eq.1) write(nchk,'(36x,10a12)') 
     1      (cntern(i),i=1,idumc3)
     
c         write(nlog,*) (cntern(i), i=1,idumc3)
        else
          write(nlog,1209) cidvri, ityopr(k)
          goto 9999
        endif
        
        
c      
c ---------------------------------------------------------
c               b1. Find the Junior RESERVOIR water right in intern(k,1)
c		    Note:
c		    ion=-1 leaves the original water right on
c			   and does not check if the right if off
c		    iacc=0 allows account to be 0 
c		    istop=0 Stop if not found
c		    istop=1 Do not Stop if not found
c                   itype=12 for a reservoir right  
        ion=-1
        iacc=0
        istop=1
        itype=12
        
        ciopso5=cntern(1)
c       write(nlog,*) ' OPrinp; Res Right ciopso5 ', ciopso5
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1,iops2, nx, ciopso5, iacc, istop, rops2, 
     1       ioprsw(k), cidvri)
        intern(k,1) = iops1
c      
c ---------------------------------------------------------
c               b1. Find the Junior DIVERSION water right in intern(k,1)
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
c		        ion=-1 leaves the original water right on
c			       and does not check if the right if off
c                   itype=13 for a diversion right          
        if(iops1.eq.0) then
          istop=0
          itype=13
          ion=-1
          ciopso5=cntern(1)
c         write(nlog,*) ' OPrinp; Div Right ciopso5 ', ciopso5
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         iops1,iops2, nx, ciopso5, iacc, istop, rops2,
     1         ioprsw(k), cidvri)
          intern(k,1) = -1*iops1        
        endif  
c      
c ---------------------------------------------------------
c		b3. Find the 'associated' OOP operatinal right in intern(k,2)
c                   itype=14 for an operational right  
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found

        istop=0
        itype=14
c
        ciopso5=cntern(2)        
c       write(nlog,*) ' OPrinp; Opr Right ciopso5 ', ciopso5
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1,iops2, nx, ciopso5, iacc, istop, rops2,
     1       ioprsw(k), cidvri)
        intern(k,2) = iops1
        
c      
c ---------------------------------------------------------
c               c1. Find the destination reservoir (type 2). Note:
c                   itype=2 for a reservoir
c	            iacc=1 requires an account number
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        itype=2
        istop=1
        iacc=1
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1, iopdes(2,k), nx,ciopde, iacc, 
     1       istop, rops2,ioprsw(k), cidvri)
        iopdes(1,k)=iops1
        iopdesr(k)=2
        
c       write(nlog,*) ' Oprinp; 4 k, ioprsw(k)', k, ioprsw(k)
c      
c ---------------------------------------------------------
c               c2. Find the destination Plan. Note:
c                   itype=7 for a plan
c	            iacc=1 requires an account number
c		    istop=0 Stop if not found
c		    istop=1 Do not stop if not found
        if(iops1.eq.0) then
          itype=7
          istop=1
          iacc=1
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         iops1, iopdes(2,k), nx,ciopde, iacc, 
     1         istop, rops2,ioprsw(k), cidvri)
          iopdes(1,k)=-1*iops1
          iopdesr(k)=7          
        endif  
c        
c ---------------------------------------------------------
c rrb 2008/03/21; Revised warning when several destinations are possible        
        if(iops1.eq.0) then
          write(nlog,1300) cidvri, ityopr(k), ciopde
          goto 9999
        endif   
        
c
c ---------------------------------------------------------
c               d1. Find source 1 a reservoir accout or NA. Note:
c                   itype=2 for a reservoir
c 		    iacc=1 requires an account number
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
c rrb 05/06/15; Note leave original right on (ion=1)
        itype=2
        ion=1
        iacc=1
        istop=1
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1,iopsou(2,k), nx, ciopso1, iacc, 
     1       istop, rops2,ioprsw(k), cidvri)
     
        iopSouR(k)=itype     
        iopsou(1,k) = iops1
c
c ---------------------------------------------------------
c               d2. Find source 2 the subordinated reservoir 
c                   water right (12). Note
c                   itype=12 for a reservoir right
c		    ion=-1 leaves the original water right on
c			   and does not check if the right if off
c		    iacc=0 allows account to be 0 
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        itype=12
        ion=-1
        iacc=0
        istop=0
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1,iops2, nx, ciopso2, iacc, istop, rops2,
     1       ioprsw(k), cidvri)
        iopsou(3,k) = iops1
c
c ---------------------------------------------------------
c		e1. Check if the destination is a reservoir
c                   the source should be the same reservoir
        if(iopdes(1,k).gt.0) then
          if(iopsou(1,k).ne.iopdes(1,k)) then
            write(nlog,732) cidvri, ciopso1, ciopde
            goto 9999
          endif  
        endif
c
c ---------------------------------------------------------
c               e2. Check if the destination is a plan
c		    the source should be null
        if(iopdes(1,k).lt.0) then
          if(iopsou(1,k).ne.0) then
            write(nlog,734) cidvri, ciopso1, ciopde
            goto 9999
          endif
        endif
c
c ---------------------------------------------------------
c               e3. Check if the destination is reservoir
c		    the variable cntern should be a Res (+) water right
        
        if(iopdes(1,k).gt.0 .and. intern(k,1).le.0) then
          write(nlog,1264) ityopr(k),cidvri, idumc, cntern(1)
          goto 9999         
        endif
c
c ---------------------------------------------------------
c               e4. Check if the destination is plan
c		    the variable cntern should be a Div (-) water right
        
        if(iopdes(1,k).lt.0 .and. intern(k,1).ge.0) then
          write(nlog,1264) ityopr(k),cidvri, idumc, cntern(1)
          goto 9999         
        endif

c
c ------------------------------------------------------------------
c               f1. Find destination reuse plan named Creuse, if any, 
c		     and store in ireuse(k)
c			ion  =0 Leave original water right ON
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
c		        iacc =0 Allow account to be 0 
        iacc=0
        ion=-1
        istop=0
        ireuse1=0        
        if(NAuse.eq.0) then                
          call oprFind(ityopr(k), 7, idumc,k,ion,iprinto,
     1         ireuse1,iops2, nx, creuse, iacc, istop, rops2,
     1         ioprsw(k), cidvri)     
        else
          write(nlog,1350) ityopr(k),cidvri, creuse
          goto 9999
        endif
        
c
c ---------------------------------------------------------
c		f2. Check proper type of plan 
c                   Note iplntyp 9 is a OOP diversion or storage
        if (ireuse1.gt.0) then
          ireuse(k)=ireuse1        
          iok=1
c
c		Allow destination to be a plan          
cr        if(iplntyp(ireuse1).eq.9 .or. iplntyp(ireuse1).eq.10) iok=0
          if(iplntyp(ireuse1).eq.9) iok=0

          if(iok.eq.1) then
            write(nlog,1263) ityopr(k),cidvri, creuse, iplntyp(ireuse1)
            goto 9999
          endif
        endif        
c
c ---------------------------------------------------------
c		g. Detailed output
     
        iout8=0
        if(iout8.eq.1) then
          write(nlog,2020) ityopr(k), cidvri, ityopr(k),
     1      ciopde,  iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k), iopsou(2,k),
     1      ciopso2, iopsou(3,k), iopsou(4,k), creuse, ireuse1,
     1      oprlimit(k), iopSou(5,k),
     1      cdivtyp(k), iopdesR(k)
     
         
          do i=1,10
            if(intern(k,i).ne.0) then
              if(i.eq.1) then
                write(nlog,2021) cntern(i), intern(k,i)
              else
                write(nlog,2022) cntern(i), intern(k,i)
              endif  
            endif  
          end do
        endif  
        goto 1190
c _________________________________________________________
c
c         
 1009   continue 
c               Type 9; Release to Target
c                destination = NA
c                source 1 (iopsou(1,k) = reservoir
c                source 2 (iopsou(3,k) = NA
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
c
        idumc=ifix(dumc)
        ireuseS=0
c
c ---------------------------------------------------------
c               a1. Read monthly constraints 
        istop=0
        cx='NA'
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               a2. Read intervening structures
        istop=0
        call oprFind(ityopr(k), 21, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               b. Find source 1 a reservoir (type 2)
c			itype=2 reservoir
c			iacc=0 account may be 0 (treat proportionally)
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        itype=2
        iacc=0
        istop=0
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1,iopsou(2,k), nx, ciopso1, iacc, 
     1       istop, rops2,ioprsw(k), cidvri)
     
        iopSouR(k)=itype     
        iopsou(1,k)=iops1
c
c ---------------------------------------------------------
c		c. Detailed output
     
        iout09=0
        if(iout09.eq.1) then
          write(nlog,2020) ityopr(k), cidvri, ityopr(k),
     1      ciopde, iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k),iopsou(2,k),
     1      ciopso2, iopsou(3,k),iopsou(4,k), creuse, ireuse1,
     1      oprlimit(k), iopSou(5,k),
     1      cdivtyp(k), iopdesR(k)
        endif  
        
        goto 1190
        
c                                                                       
c _________________________________________________________
c
 1010   continue 
c 
c               Type 10; Replacement Reservoir
c
        idumc=ifix(dumc)
c
c ---------------------------------------------------------
c               a. Read monthly constraints 
c                  ion=1 means turn off opr right if right is off
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        ion=1
        istop=0        
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               a2. Read intervening structures
        ion=1
        istop=0
        call oprFind(ityopr(k), 21, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c rrb 2007/10/26; 
c		            a3. Read the operating rule associated with
c		                 a monthly or annual plan limit adjustment
c		                 when Oprlimit(k) > 0.  Note:
c		                 istop=0  Stop if not found
c		                 itype=24 Operating Rule ID with monthly and annual
c                             plan limits
c       write(nlog,*) ' Oprinp; ioprlim(k) ', ioprlim(k)
        if(ioprlim(k).gt.0) then
          istop=0
          itype=24          
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1      iopsou(5,k),iopsou(6,k), nx, cAssoc, 1, 
     1      istop, rops2,ioprsw(k), cidvri)
          iopsou(5,k)=nx 
c
c		Set ciospoX5 to the plan associated with the 
c               above operating rule          
          ip5=iopsou(1,nx)
          ciopsoX5(k)=pid(ip5)
        endif          
c
c ---------------------------------------------------------
c               c1. Find destination diversion 
c                  Note no need to find destination
c ---------------------------------------------------------
c
c               d1. Find source 1 a reservoir (type 2)
c		   Note ion=0 leaves the original water right on
c		        iacc=0 allows account to be 0 (since 
c                       it is ownership %)
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        ion=0
        iacc=1
        istop=0
        itype=2
        call oprFind(ityopr(k), itype,idumc,k,ion,iprinto,
     1       iopsou(1,k),iopsou(2,k), nx, ciopso1, 1, 
     1       istop, rops2,ioprsw(k), cidvri)
     
        iopSouR(k)=itype
     
 
c
c ---------------------------------------------------------
c               d2 Check dimension of replacement reservoirs and 
c		assing the exchagne point to iopsou(3,k)
        irepn=irepn+1                               
        if(irepn.gt.maxrep) then
          write(nlog,1208) cidvri, ityopr(k), maxrep
          goto 9999
        else
c         write(nlog,*) '  Oprinp; calling Getrep'
          call getrep(irepn,k)
        endif     
 
c
c ------------------------------------------------------------------
c               d1. Find reuse plan named Creuse, if any, 
c		   and store in ireuse(k)
        iacc=0
        ion=-1
        istop=0
        ireuse1=0        
        if(NAuse.eq.0) then                
          call oprFind(ityopr(k), 7, idumc,k,ion,iprinto,
     1       ireuse1,iops2, nx, creuse, iacc, istop, rops2,
     1       ioprsw(k), cidvri)     
        endif
c
c ---------------------------------------------------------
c		g. Check proper type of plan 
c                  Note iplntyp 3 & 5 are reservoir, 
c                       iplntyp 4 & 6 are diversion
        if (ireuse1.gt.0) then
          ireuse(k)=ireuse1        
          iok=1
c
c		Check an Admin Plan was specified
          if(iplntyp(ireuse1).eq.11) iok=0

          if(iok.eq.1) then
            write(nlog,1255) ityopr(k),cidvri, ciopde, creuse,
     1         iplntyp(ireuse1)
            goto 9999
          endif          
        endif  
     
c
c ---------------------------------------------------------
c		h. Detailed output
     
        iout10=0
        if(iout10.eq.1) then
          write(nlog,2020) ityopr(k), cidvri, ityopr(k),
     1      ciopde, iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k),iopsou(2,k),
     1      ciopso2, iopsou(3,k),iopsou(4,k), creuse, ireuse1,
     1      oprlimit(k), iopSou(5,k),
     1      cdivtyp(k), iopdesR(k)
        endif  
        
c		
        goto 1190
        
c        
c _________________________________________________________
c
c         
 1011   continue 
c               Type 11; Carrier to a diversion or reservoir
c		             where soruce is a diversion, diversion water right
c		             or reservoir water right
c rrb 01/06/20; 
c                destination = diversion or reservoir ID
c                source 1 = a diversion or water right 
c                source 2 = water right type >=0 = diversion, <0=reservoir
c		             source 3 = NA
c                source 4 = NA
c                source 5 = NA
c		             source 6 = NA
c		             source 7 = NA
c
c                  ion=1 means turn off opr right if right is off
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found

        ion=1
        istop=0
        iwarnr=0
        iwarno=0
        
        idumc=ifix(dumc)
c
c ---------------------------------------------------------
c               a. Read monthly constraints 
        istop=0
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               a2. Read intervening structures
        istop=0
        call oprFind(ityopr(k), 21, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               c1. Find destination diversion 
c                   Note itype=3 for a diversion 
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
c                        iacc=1 Check the diversion account
        itype=3
        istop=1
        iacc=1
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1, iopdes(2,k), nx,ciopde, iacc, 
     1       istop, rops2,ioprsw(k), cidvri)
        iopdes(1,k) = iops1
        ndD=iopdes(1,k)
        iopDesr(k)=3
        cdestyp='Diversion   '
c        
c ---------------------------------------------------------
c               c2. Find destination reservoir (type 2)
c                   Note itype=2 for a reservoir
c		   Note istop=0 Stop if not found
c		        istop=1 (OK if not found)
c			 iacc=1  Check reservoir account 
        if(iops1.eq.0) then
          itype=2
          istop=1
          iacc=1
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         iops1, iopdes(2,k), nx,ciopde, iacc, 
     1         istop, rops2,ioprsw(k), cidvri)
          iopdes(1,k)=-iops1
          ndD=iopdes(1,k)
          iopDesR(k)=2    
          cdestyp='Reservoir   '
                
c         write(nlog,*) ' Oprinp;  k, iopdes(1,k), iopdes(2,k) = ', 
c    1      k, iopdes(1,k), iopdes(2,k)
        endif
c        
c ---------------------------------------------------------
c rrb 2008/03/21; Revised warning when several destinations are possible        
        if(iops1.eq.0) then
          write(nlog,1300) cidvri, ityopr(k), ciopde
          goto 9999
        endif   
        
c
c ---------------------------------------------------------
c               d1. Find source 1 a diversion structure (3)
c 		    Note ion=0 leaves the original water right on
c		        iacc=0 allows account to be 0 (since 
c                       it is ownership %)
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
c rrb 05/06/15; Note turn original right off (ion=1)
        ion=1
        iacc=0
        istop=1
        itype=3

        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1,iops2, nx, ciopso1, iacc, istop, rops2,
     1       ioprsw(k), cidvri)

        iopSouR(k)=itype
        iopsou(1,k) = iops1
c
c ---------------------------------------------------------
c               d2. Find source 1 a diversion water right (13)
c		    Note ion=0 leaves the original water right on
c		         istop=0 Stop if not found
c		         istop=1 Do not Stop if not found
c                        iacc=0 do not check account (iops2)
c rrb 2005/06/15; Source right ON (ion=0)
c rrb 2006/06/19; Source right OFF (ion=1)
c rrb 2006/07/19; Source right ON (ion=0)
c rrb 2006/11/21; Let source 4 control the water right ON/Off       
c rrb 2006/11/27; Let source 2 control the water right ON/Off       
        if(iops1.eq.0) then
c         ion=0
c
c rrb 2006/11/27; Tie on/off to source 2
c         ion=iopsou(4,k)        
          ion=iopsou(2,k)        
          iacc=0
          istop=1
          itype=13

          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         iops1,iops2, nx, ciopso1, iacc, istop, rops2,
     1         ioprsw(k), cidvri)
          
c
c		Set Source 1 and type (iopdesr = 1 = diversion water right
          if(iops1.gt.0) then
            iopSouR(k)=itype          
            iopsou(1,k) = -iops1
          endif  
c
c		Set operating right admin number to water right value 
          if(iops1.gt.0) then            
            if(abs(rdvnk(iops1)-ropnk(k)).gt. small) then          
              ropnk1=ropnk(k)
              ropsrc=rdvnk(iops1)
              ropnk(k)=rdvnk(iops1)
              iwarnr=1
              iwarno=1
            endif  
          endif  
        endif  
c
c ---------------------------------------------------------
c               d3. Find source 1 a reservoir water right (12)
c		    Note ion=0 leaves the original water right on
c                        iacc=0 do not check account (iops2)
c		         istop=0 Stop if not found
c		         istop=1 Do not Stop if not found
c
c rrb 2005/06/15; Note turn original right off (ion=1)
c rrb 2006/08/28; Leave original right on (ion=0
        if(iops1.eq.0) then
c
c rrb 2006/11/21; Let source 4 control the water right ON/Off       
c rrb 2006/11/27; Tie on/off to source 2
c         ion=iopsou(4,k)        
          ion=iopsou(2,k)        
          iacc=0
          istop=0
          itype=12
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         iops1,iops2, nx, ciopso1, iacc, istop, rops2,
     1         ioprsw(k), cidvri)
     
c
c		Set Source 1 and type (iopSour = reservoir water right)
          if(iops1.gt.0) then
            iopSouR(k)=itype          
            iopsou(1,k) = -iops1
          endif  
c
c		Set operating right admin number to water right value
          if(iops1.gt.0) then
            if(abs(rrsnk(iops1)-ropnk(k)).gt. small) then
              ropnk1=ropnk(k)        
              ropsrc=rrsnk(iops1)    
              ropnk(k)=rrsnk(iops1)
              iwarnr=1
              iwarno=1
            endif  
          endif  
c
c		Check if there is a carrier 
          nc=intern(k,1)
          if(nc.le.0) then
            write(nlog,1211) cidvri, ityopr(k), ciopso1
            goto 9999
          endif  
        endif  
c
c ---------------------------------------------------------
c               e. Find source 2 the location where water is 
c                   diverted. Note:
c		                ion=0 leaves the original water right on
c                   iacc=1 Check the account variable (iops2) > 0
c		                iacc=0 Do not check the account variable
c                   itype=3 a diversion
c                   istop = 1 Do not Stop if not found
        if(NAs2.eq.1) then        
          iopsou(3,k)=0
        else 
c
c		Diversion location          
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found

          ion=1
          iacc=0
          istop=1
          itype=3
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         iops1,iops2, nx, ciopso2, iacc, istop, rops2,
     1         ioprsw(k), cidvri)
          iopsou(3,k) = -iops1
c
c	  Reservoir location     
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found

          if(iops1.eq.0) then      
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
            istop=0          
            itype=2
            call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1           iops1,iops2, nx, ciopso2, iacc, istop, rops2,
     1           ioprsw(k), cidvri)
            iopsou(3,k) = iops1     
          endif
        endif  
        
c
c ------------------------------------------------------------------
c               f-1. Find destination reuse plan named Creuse, if any, 
c		   and store in ireuse(k)
        iacc=0
        ion=-1
        istop=0
        ireuse1=0        
        if(NAuse.eq.0) then                
          call oprFind(ityopr(k), 7, idumc,k,ion,iprinto,
     1       ireuse1,iops2, nx, creuse, iacc, istop, rops2,
     1       ioprsw(k), cidvri)     
        endif
c
c ---------------------------------------------------------
c		g. Check proper type of plan 
c                  Note iplntyp 3 & 5 are reservoir, 
c                       iplntyp 4 & 6 are diversion
        if (ireuse1.gt.0) then
          ireuse(k)=ireuse1        
          iok=1
c
c		If the source is a plan and the destination is a reservoir
c               the reuse plan should be types 3 or 5
          if(iplntyp(ireuse1).eq.8) iok=0

          if(iok.eq.1) then
            write(nlog,1255) ityopr(k),cidvri, ciopde, creuse,
     1         iplntyp(ireuse1)
            goto 9999
          endif          
        endif  
c
c
c ---------------------------------------------------------
c		h. For a diversion water right  (iopdesr(k)>0
c                  Check if the carrier is the same structure 
c                  as the supply
        ndc=0
c
c rrb 2008/09/26; Correction iopDesR is the destination        
cx      if(iopdesr(k).ge.0) then
        if(iopSouR(k).eq.13) then
          if(iopsou(1,k).lt.0) then
            nd=-1*iopsou(1,k)
            ndc=idivco(1,nd)
          else
            ndc=iopsou(1,k)
          endif
          
          nc=intern(k,1)
c         write(nlog,*)' Oprinp; nc,ndc', nc, ndc
          if(nc.eq.ndc) then
            write(nlog,1212) cidvri, ityopr(k),
     1        ciopso1, cdivid(ndc), cdivid(nc)
            goto 9999
          endif
c
c ---------------------------------------------------------
c		i. For a diversion water right  (iopdesr(k)=13
c                  Check if the source is the same structure 
c                  Note OK if source 2 (iopsou(3,k).ne.0 to indicate
c		   the right is administered at a different location
c		   or there are intermediate carriers
c rrb 2007/11/19; Allow if there are intermediate carriers          
c         if(iopdes(1,k).eq.ndc .and. iopsou(3,k).eq.0) then
          if(iopdes(1,k).eq.ndc .and. iopsou(3,k).eq.0 .and. 
     1       nc.eq.0) then
            write(nlog,1214) cidvri, ityopr(k),
     1        ciopso1, cdivid(ndc), ciopde
            goto 9999
          endif
        endif
c
c ---------------------------------------------------------
c 		j. Print warning about admin number
        if(iwarnr .gt. 0) then
          if(iprinta.eq.0) write(ntmp,728)
          iprinta=iprinta+1
          write(ntmp,729) iprinta, ityopr(k), cidvri, ciopso1,
     1      ropsrc, ropnk1, ropnk(k) 
        endif
c
c ---------------------------------------------------------
c 		k. Print warning and stop regarding carrier loss
        if(iopdes(1,k).gt.0 .and. oprloss(k).gt.small) then
          write(nlog,735) cidvri, ityopr(k), Oprloss(k), cdestyp
          goto 9999
        endif
c
c ---------------------------------------------------------
c		l. Detailed output
     
        iout11=0
        if(iout11.eq.1) then
          write(nlog,2020) ityopr(k), cidvri, ityopr(k),
     1      ciopde, iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k),iopsou(2,k),
     1      ciopso2, iopsou(3,k),iopsou(4,k), creuse, ireuse1,
     1      oprlimit(k), iopSou(5,k),
     1      cdivtyp(k), iopdesR(k)
        endif  
        
        goto 1190
        
c                                                                       
c _________________________________________________________
c
 1013   continue 
c 
c               Type 13; Instream flow tied to a index station
c
c                  ion=1 means turn off opr right if right is off
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        ion=1
        istop=0
        idcdd=0
        iss=0
        
        idumc=ifix(dumc)
c
c ---------------------------------------------------------
c               a. Read monthly constraints 
        istop=0
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               a2. Read intervening structures
        istop=0
        call oprFind(ityopr(k), 21, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               c1. Find destination ISF
c                  Note itype=1 for a ISF structure
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found 
        itype=1
        istop=0
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1, iopdes(2,k), nx,ciopde, 1, istop, rops2,
     1       ioprsw(k), cidvri)
        iopdes(1,k) = iops1
        iopdesr(k)=1
        
        idcdD=ifrsta(iops1)        
c
c
c ---------------------------------------------------------
c               d. Find source 1 a stream gage 
c                  Note itype=0 for a stream ID
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
c		        iacc=0 allows account to be 0 (since 
c                         it is ownership %)
c		        ion=0 leaves the original water right on
c           itype = 0 for a stream gage
        itype=0
        ion=0
        iacc=1
        istop=0
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1,iopsou(2,k), nx, ciopso1, 1, istop, rops2,
     1       ioprsw(k), cidvri)
     
        iopSouR(k)=itype     
        iopsou(1,k)=iops1   
        iscdS=iops1
        
c
c ---------------------------------------------------------
c rrb 2011/12/05; Find the ISF water right as source 2
c		   Note:
c                  ion=1 turn off source water right 
c                  istop=1 OK if not found
c		    itype=11 = ISF flow right
        ion=1
        iacc=0
        istop=0
        itype=11
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1     iopS1,iops2, nx, ciopso2, iacc,
     1     istop, rops2,ioprsw(k), cidvri)
     
        iopsou(3,k) = iopS1
        iopSouR(k)=itype               
c        
c ---------------------------------------------------------
c rrb 2011/11/05; Update
c               e. Check that the source stream gage is upstream 
c                  of the destination ISF
c	                 trying to find destination (idcdD)
c                  downstream of  source (iscdS) 
c rrb 2007/11/19; Correction iops1 is the river node
c       iss=irusta(iops1)
cx      iss=iops1
        ndns=ndnnod(iscdS)
        csource=cstaid(iscdS)
        cdest=cstaid(idcdD)
        
        write(nlog,*) ' Oprinp; ', iscdS, idcdD, ndns
                  
        call oprdown(nlog, maxsta, ndns, iscdS, idcdD, idncod,
     1       cidvri, csource, cdest)
c
c ---------------------------------------------------------
c		f. Detailed output
     
        iout13=0
        if(iout13.eq.1) then
          write(nlog,2020) ityopr(k), cidvri, ityopr(k),
     1      ciopde,  iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k), iopsou(2,k),
     1      ciopso2, iopsou(3,k), iopsou(4,k), creuse, ireuse1,
     1      oprlimit(k), iopSou(5,k),
     1      cdivtyp(k), iopdesR(k)
        endif
        
c
        goto 1190
c        
c _________________________________________________________
c
c         
 1014   continue 
c               Type 14; Carrier with a constrained demand
c rrb 01/06/20; 
c                destination = diversion or reservoir ID
c                source 1 = a diversion or water right 
c                source 2 = NA
c		 source 3 = water right type 
c                source 4 = 0 or annual limit
c
c                  ion=1 means turn off opr right if right is off
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found

        ion=1
        istop=0
        iwarnr=0
        iwarno=0
        
        idumc=ifix(dumc)
c
c ---------------------------------------------------------
c               a. Read monthly constraints 
        istop=0
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               a2. Read intervening structures
        istop=0
        call oprFind(ityopr(k), 21, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               c1. Find destination diversion 
c                   Note itype=3 for a diversion 
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
c                        iacc=1 Check the diversion account
        itype=3
        istop=1
        iacc=1
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1, iopdes(2,k), nx,ciopde, iacc, istop, rops2,
     1       ioprsw(k), cidvri)
        iopdes(1,k) = iops1
        ndD=iopdes(1,k)
        iopdesr(k)=3
        cdestyp='Diversion   '
        
        
c        
c ---------------------------------------------------------
c               c2. Find destination reservoir (type 2)
c                   Note itype=2 for a reservoir
c		   Note istop=0 Stop if not found
c		        istop=1 Do not stop if not found
c			 iacc=1  Check reservoir account 
        if(iops1.eq.0) then
          itype=2
          istop=1
          iacc=1
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         iops1, iopdes(2,k), nx,ciopde, iacc, 
     1         istop, rops2,ioprsw(k), cidvri)
          iopdes(1,k)=-iops1
          ndD=iopdes(1,k)
          iopdesr(k)=2    
          cdestyp='Reservoir   '
                
c         write(nlog,*) ' Oprinp;  k, iopdes(1,k), iopdes(2,k) = ', 
c    1      k, iopdes(1,k), iopdes(2,k)
        endif
c        
c ---------------------------------------------------------
c rrb 2008/03/21; Revised warning when several destinations are possible        
        if(iops1.eq.0) then
          write(nlog,1300) cidvri, ityopr(k), ciopde
          goto 9999
        endif   
        
c
c ---------------------------------------------------------
c               d. Find source 1 a diversion water right (13)
c		    Note:
c                       ion=0 leaves the original water right on
c                       ion=1 turns the original water right off
c		        iacc=0 allows account to be 0 (since 
c                       it is ownership %)
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        itype=13
        ion=iopsou(2,k)
        iacc=0
        istop=0
        
        call oprFind(ityopr(k), itype, idumc, k, ion,iprinto,
     1       iops1,iops2, nx, ciopso1, iacc, istop, rops2,
     1       ioprsw(k), cidvri)
     
        if(iops1.gt.0) then
          iopSouR(k)=itype     
          iopdesr(k)=1
          iopsou(1,k) = -1*iops1
        endif
c
c		Check the monthly or annual limit (iopsou(4,k)) 
c               is set properly
        if(iopsou(4,k).le.0) then
          write(nlog,1269) ityopr(k),cidvri, iopsou(4,k)
          goto 9999
        endif        
c
c		Set operating right admin number to water right value
        if(iops1.gt.0) then
          if(abs(rdvnk(iops1)-ropnk(k)).gt. small) then          
            ropnk1=ropnk(k)
            ropsrc=rdvnk(iops1)
            ropnk(k)=rdvnk(iops1)
            iwarnr=1
            iwarno=1
          endif  
        endif  
c
c ---------------------------------------------------------
c
c		e. Check if there is a carrier 
c		   Note carrier is not required. When the source
c		   is a water right the assocaited diversion is
c		   an implied carrier
        nc=0
cr      nc=intern(k,1)
cr      if(nc.le.0) then
cr        write(nlog,1211) cidvri, ityopr(k), ciopso1
cr        goto 9999
cr      endif  
     

c
c ---------------------------------------------------------
c		f. For a diversion water right  (iopdesr(k)>0
c                  Check if the carrier is the same structure 
c                  as the supply
        ndc=0
c
        if(iopdesr(k).ge.0) then
          if(iopsou(1,k).lt.0) then
            nd=-1*iopsou(1,k)
            ndc=idivco(1,nd)
          else
            ndc=iopsou(1,k)
          endif
          
          nc=intern(k,1)
c         write(nlog,*)' Oprinp; nc,ndc', nc, ndc
          if(nc.eq.ndc) then
            write(nlog,1212) cidvri, ityopr(k),
     1        ciopso1, cdivid(ndc), cdivid(nc)
            goto 9999
          endif
c
c ---------------------------------------------------------
c		g. For a diversion water right  (iopdesr(k)>0
c                  Check if the source is the same structure 
c                  Note OK if source 2 (iopsou(3,k).ne.0 to indicate
c		   the right is administered at a different location
c		   or there are intermediate carriers
c rrb 2007/11/19; Allow if there are intermediate carriers          
c         if(iopdes(1,k).eq.ndc .and. iopsou(3,k).eq.0) then          
          if(iopdes(1,k).eq.ndc .and. iopsou(3,k).eq.0 .and. 
     1       nc.eq.0) then
            write(nlog,1214) cidvri, ityopr(k),
     1        ciopso1, cdivid(ndc), ciopde
            goto 9999
          endif
        endif
c
c ---------------------------------------------------------
c 		g. Print warning about admin number
        if(iwarnr .gt. 0) then
          if(iprinta.eq.0) write(ntmp,728)
          iprinta=iprinta+1
          write(ntmp,729) iprinta, ityopr(k), cidvri, ciopso1,
     1     ropsrc, ropnk1, ropnk(k) 
        endif
c
c ---------------------------------------------------------
c 		h. Print warning and stop regarding carrier loss
        if(iopdes(1,k).gt.0 .and. oprloss(k).gt.small) then
          write(nlog,735) cidvri, ityopr(k), Oprloss(k), cDesTyp
          goto 9999
        endif
c
c ---------------------------------------------------------
c		h. Detailed output
     
        iout14=0
        if(iout14.eq.1) then
          write(nlog,2020) ityopr(k), cidvri, ityopr(k),
     1      ciopde, iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k),iopsou(2,k),
     1      ciopso2, iopsou(3,k),iopsou(4,k), creuse, ireuse1,
     1      oprlimit(k), iopSou(5,k),
     1      cdivtyp(k), iopdesR(k)
        endif  
        
        goto 1190
        
c                                                                       
c _________________________________________________________
c
 1015   continue
c 
c      Type 15; Interruptible supply source
c                destination = instream flow
c                source read = used
c                source 1 = 1 source stream used as an on/off switch
c                source 3 = 2 water right with an interruptible supply
c                source 2 = 2 stream discharge (part of on/off switch)
c                source 4 = 4 code 0 transfer diversion 1 transfer CU

        idumc=ifix(dumc)
c
c ---------------------------------------------------------
c               a. Read monthly constraints          
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               b. Find destination instream flow
c			istop = 0 Stop if not found
c			itype = 1 Instream Flow
        istop=0
        itype=1
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iopd1, iopdes(2,k), nx,ciopde, 1, 
     1       istop, rops2,ioprsw(k), cidvri)
     
        iopdes(1,k)=iopd1
        nf=iopd1
        iopdesr(k)=1
c
c ---------------------------------------------------------
c               b. Find source 1 (iopsou(1,k)) to be a Stream ID
c                  for the interruptible supply switch
c			itype = 0 Stream ID
        istop=0
        itype=0
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1, iops2, nx,ciopso1, 1, istop, rops2,
     1       ioprsw(k), cidvri)
     
        iopSouR(k)=itype     
        iopsou(1,k) = iops1     
        iopsou(3,k) = iops2
c
c ---------------------------------------------------------
c               c. Find source 2 it should be an agricultural water right
c			istop = 0 Stop if not found
c			itype = 1 Instream Flow
        istop=0
        itype=13
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1, iops2, nx,ciopso2, 1, istop, rops2,
     1       ioprsw(k), cidvri)
     
        isd=idivco(1,iops1)

        iopsou(2,k) = iops1
        iopsou(4,k) = iopsou(4,k)
        ropnk(k)=rdvnk(iops1)
c
c ---------------------------------------------------------
c		d. Set monthly switch data
          ifound2=0
          do im=1,12
            if(imonsw(k,im).eq.-1) ifound2 = ifound2+1
            if(imonsw(k,im).eq.2)  ifound2 = ifound2+1
          end do

          if(ifound2.ne.2) then
            write(99,1195) cidvri, (imonsw(k,im), im=1,12)
            goto 9999
          endif
c
c ---------------------------------------------------------
c               e. Turn off Opr right if source water right is off
        if(idvrsw(iops1).eq.0) then
          write(nlog,1252) ityopr(k), cidvri  ,24          
          ioprsw(k) = 0
        endif  

c
c ---------------------------------------------------------
c               f. Check that the instream flow is downstream 
c               of the source water rights location on stream 
        isx=ifrsta(nf)
        iscd=idvsta(isd)
        ndns=ndnnod(iscd)
c
c               Search every river node downstream of the source
c               water rights structure (isd)
c               to see if it can be serve the instream flow directly
        ifound=0
        iss=iscd
        do is=1,ndns
          if(iss.eq.isx) ifound = 1
          iss=idncod(iss)
        end do
c
c               Problem destination is not downstream of source
        if(ifound.eq.0) then
          write(nlog,1197) cidvri, ciopde, ciopso2
          goto 9999
        endif
c
c ---------------------------------------------------------
c		l. Detailed output
c     
        iout15=0
        if(iout15.eq.1) then
          call OprinOut(nlog, maxopr, k, 
     1      ityopr(k), cidvri, 
     1      ciopde, iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k),iopsou(2,k),
     1      ciopso2, iopsou(3,k),iopsou(4,k), creuse, ireuse1,
     1      oprloss(k),  oprlimit(k), iopSou(5,k),
     1      iopSou(6,k), iopsou(7,k),
     1      cdivtyp(k), intern, cntern, cAssoc, cAssoc2, cAssoc3)
        endif  
        

        goto 1190

c                                                                       
c _________________________________________________________
c         
c               Type 16; Direct Flow Storage
c rrb 99/11/02; revised to allow DFS by a right tied to a carrier
c
c                destination = reservoir
c                source 1 = water right or operation right
c                           that does DFS
c                source 2 = source demand user (should be 1)
c                source 4 = maximum diversion % (1-bypass)
c                source 5 = diversion ID where demand is located
c                           (for a water right, it is the structure ID)
c                           (for an opr right, it is the carrier
c                            destination id)
c
 1016   continue

        idumc=ifix(dumc)
c
c ---------------------------------------------------------
c               a. Read monthly constraints          
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               b. Read intervening carrier structures
        call oprFind(ityopr(k), 21, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               c. Find destination reservoir (type 2)
c			istop = 0 Stop if not found
        istop=0
        call oprFind(ityopr(k), 2, idumc,k,ion,iprinto,
     1       iopdes(1,k), iopdes(2,k), nx,ciopde, 1, 
     1       istop, rops2,ioprsw(k), cidvri)
        iopdesr(k)=3
     
c
c ---------------------------------------------------------
c               d. Determine if source 1 is a div water right
c
c rrb 2006/04/10; Correction. Turn off when the Opr Rule fires
c       ion=1
        ion=0
        
        call oprFind(ityopr(k), 13, idumc,k,ion,iprinto,
     1       iopsou(1,k),iopsou(2,k), nx, ciopso1, 1, 
     1       istop, rops2,ioprsw(k), cidvri)
c       write(nlog,*) '  Oprinp;  nx = ', nx
        if(nx.gt.0) then
          nd1=idivco(1,nx)
          iopsou(5,k) = nd1
          iscd=idvsta(nd1)
c
c rrb 2004/04/10; Turn off direct flow right when operating right is on
          if(ioprsw(k).eq.1) then
            idvrsw(nx)=0
          endif  

          if(ioprsw(k).lt.0) then
            idvrsw(nx)=-1*(ioprsw(k)-1)          
          endif
          
          if(ioprsw(k).gt.1) then
            idvrsw(nx)=-1*(ioprsw(k)-1)          
          endif
            
c         write(nlog,*) 
c    1      '  Oprinp type 16; nx, k, ioprsw(k), idvrsw(nx)'
c         write(nlog,*) 
c    1     '  Oprinp type 16;', nx, k, ioprsw(k), idvrsw(nx)

        endif
c
c ---------------------------------------------------------
c               e. Determine if source 1 is an operation right
c                  iok=0 means OK if not found, iok=1 problem if not
c                  ion=1 means turn off opr right if right is off
        if(nx.eq.0) then
          iok=0
          ion=1
          call oprFind(ityopr(k), 15, idumc,k,ion,iprinto,
     1         iopsou(1,k), iopsou(2,k), nx,ciopso1, 1, 
     1         istop, rops2,ioprsw(k), cidvri)
          if(nx.gt.0) then
            nd1=idivco(1,nx)
            iscd=idvsta(nd1)
            iopsou(5,k) = nd1
          endif
        endif
c
c ---------------------------------------------------------
c               f. Check that the DFS right is downstream of reservoir
        iscd2=irssta(iopdes(1,k))
        ndns2=ndnnod(iscd2)
        iss=iscd2
c
c               Adjust iscd if the destination is tied to a carrier
        if((dumc.gt.0.1 .and. dumc.lt.10.1) .or. (dumc.lt.-12.1)) then
          iscd=idvsta(intern(k,1))
        endif
        
        csource=cstaid(iss)
        cdest=cstaid(iscd)
                  
        call oprdown(nlog, maxsta, ndns2, iss, iscd, idncod, 
     1       cidvri, csource, cdest)
c
c ---------------------------------------------------------
c               Check the max diversion % (iopsou(4,k) 
c               Note for a type 16 right iopsou(4,k) is the
c               max diversion %
        if(iopsou(4,k).lt.0 .or. iopsou(4,k).gt.100) then
          write(nlog,1191) cidvri, ityopr(k), iopsou(4,k)
          goto 9999
        endif
c
c ---------------------------------------------------------
c rrb 01/08/28; Following is OK; 0 is the default
        if(iopsou(4,k).le.0) then
          write(nlog,1192) cidvri, ityopr(k), iopsou(4,k)
        endif    
c
c ---------------------------------------------------------
c		l. Detailed output
c     
        iout16=0
        if(iout16.eq.1) then
          call OprinOut(nlog, maxopr, k, 
     1      ityopr(k), cidvri, 
     1      ciopde, iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k),iopsou(2,k),
     1      ciopso2, iopsou(3,k),iopsou(4,k), creuse, ireuse1,
     1      oprloss(k),  oprlimit(k), iopSou(5,k), 
     1      iopSou(6,k), iopsou(7,k),     
     1      cdivtyp(k),  intern, cntern, cAssoc, cAssoc2, cAssoc3)
        endif  

        goto 1190
c
c                                                                       
c _________________________________________________________
c
c               Type 17; Rio Grande Compact for Rio Grande
c
c ---------------------------------------------------------
c               a. Read second card of data
 1017   read(55,134,end=1210,err=928)
     1       qdebt1,  qdebt2,      ciopso3,     iopsou(6,k), 
     1       ciopso4, iopsou(8,k), ciopso5,     iopsou(10,k)
        if(iecho.eq.1) write(nchk,134) 
     1       qdebt1,  qdebt2,      ciopso3,     iopsou(6,k), 
     1       ciopso4, iopsou(8,k), ciopso5,     iopsou(10,k)
        
        if(iout.eq.1) then
          write(nlog,134) 
     1       qdebt1,  qdebt2,      ciopso3,     iopsou(6,k), 
     1       ciopso4, iopsou(8,k), ciopso5,     iopsou(10,k)
        endif
        
        ciopso4=ciopso4
        ciopso5=ciopso5
c
c ---------------------------------------------------------
c               b. Set on / off switch used in Execut for End of Year
        irg1=0
        if(ioprsw(k).ne.0 .and. ityopr(k).eq.17) then
          irg1=1
          numrg=2
        endif
c
c ---------------------------------------------------------
c               c. Set maximum debt
        qdebt(1)=qdebt1
        qdebtx(1)=qdebt2
c
c rrb 00/10/27; Allow code to specify a big number
        if(ifix(qdebt2).eq.-1) qdebtx(1) = 9999999.
c
c ---------------------------------------------------------
c               d. Read monthly constraints
        istop=0
        idumc=ifix(dumc)
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               e. Find Destination isf (Rio Grande at Labatos)
        call oprFind(ityopr(k), 1, idumc,k,ion,iprinto,
     1       iopdes(1,k),iopdes(2,k), nx, ciopde, 1, 
     1       istop, rops2,ioprsw(k), cidvri)
c
c rrb 00/04/13; Check that we have monthly isf data provided
        ix=iopdes(1,k)
c
c rrb 2006/03/21; Allow data to be provided as a RG forecast (*.rgf)        
c       if(iifcom(ix).ne.1) then
        if(iifcom(ix).eq.1 .or. iifcom(ix).eq.3) then
        else
          write(nlog,*) '  Oprinp; iifcom(ix)', ix, iifcom(ix)
          write(nlog,1199) cidvri
          goto 9999
        endif
c
c ---------------------------------------------------------
c               f. Find source 1 stream ID (Index RG @ Del Norte)
        itype=0
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iopsou(1,k),iopsou(2,k), nx, ciopso1, 1, 
     1       istop, rops2,ioprsw(k), cidvri)
     
        iopSouR(k)=itype     
     
c
c ---------------------------------------------------------
c               g. Find source 2 stream ID (Index Conejos @ La Sauses)
c                  used to adjust delivery from Rio Grande
        call oprFind(ityopr(k), 0, idumc,k,ion,iprinto,
     1       iopsou(3,k),iopsou(4,k), nx, ciopso2, 1, 
     1       istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c		l. Detailed output
c     
        iout17=0
        if(iout17.eq.1) then
          call OprinOut(nlog, maxopr, k, 
     1      ityopr(k), cidvri, 
     1      ciopde, iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k),iopsou(2,k),
     1      ciopso2, iopsou(3,k),iopsou(4,k), creuse, ireuse1,
     1      oprloss(k),  oprlimit(k), iopSou(5,k), 
     1      iopSou(6,k), iopsou(7,k),     
     1      cdivtyp(k),  intern, cntern, cAssoc, cAssoc2, cAssoc3)
        endif  
     

        goto 1190
c
c                                                                       
c _________________________________________________________
c
c               Type 18; Rio Grande Compact for Conejos
c
c ---------------------------------------------------------
c               a. Read second card of data
 1018   read(55,134,end=1210,err=928)
     1       qdebt1,  qdebt2,      ciopso3,     iopsou(6,k), 
     1       ciopso4, iopsou(8,k), ciopso5,     iopsou(10,k)
        if(iecho.eq.1) write(nchk,134) 
     1       qdebt1,  qdebt2,      ciopso3,     iopsou(6,k), 
     1       ciopso4, iopsou(8,k), ciopso5,     iopsou(10,k)
        
        if(iout.eq.1) then
          write(nlog,134)
     1       qdebt1,  qdebt2,      ciopso3,     iopsou(6,k), 
     1       ciopso4, iopsou(8,k), ciopso5,     iopsou(10,k)
        endif
c
c ---------------------------------------------------------
c               b. Set on / off switch used in Execut for End of Year
        irg2=0
        istop=0
        
        if(ioprsw(k).ne.0 .and. ityopr(k).eq.18) then
          irg2=1
          numrg=2
        endif
c
c ---------------------------------------------------------
c               c. Set maximum debt
        qdebt(2)=qdebt1
        qdebtx(2)=qdebt2
c
c rrb 00/10/27; Allow code to specify a big number
        if(ifix(qdebt2).eq.-1) qdebtx(1) = 9999999.
c
c ---------------------------------------------------------
c               d. Read monthly constraints
        istop=0
        idumc=ifix(dumc)
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               e. Find Destination isf (Conejos at La Sauses)
        call oprFind(ityopr(k), 1, idumc,k,ion,iprinto,
     1       iopdes(1,k),iopdes(2,k), nx, ciopde, 1, 
     1       istop, rops2,ioprsw(k), cidvri)
c
c rrb 00/04/13; Check that we have monthly isf data provided
        ix=iopdes(1,k)
c
c rrb 2006/03/21; Allow data to be provided as a RG forecast (*.rgf)        
c       if(iifcom(ix).ne.1) then
        if(iifcom(ix).eq.1 .or. iifcom(ix).eq.3) then
        else
          write(nlog,1199) cidvri
          goto 9999
        endif
c
c ---------------------------------------------------------
c               f. Find source 1 stream ID (Index Conejos at Magote)
        itype=0
        call oprFind(ityopr(k), 0, idumc,k,ion,iprinto,
     1       iopsou(1,k),iopsou(2,k), nx, ciopso1, 1, 
     1       istop, rops2,ioprsw(k), cidvri)
     
        iopSouR(k)=itype     
     
c
c ---------------------------------------------------------
c               g. Find source 2 stream ID (Index San Antonio @ Ortiz)
c                  used to adjust delivery from Rio Grande
        call oprFind(ityopr(k), 0, idumc,k,ion,iprinto,
     1       iopsou(3,k),iopsou(4,k),nx, ciopso2,1, 
     1       istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               h. Find source 3 stream ID (Index Los Pinos nr Ortiz)
        call oprFind(ityopr(k), 0, idumc,k,ion,iprinto,
     1       iopsou(5,k),iopsou(6,k), nx, ciopso3, 1, 
     1       istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               i. Note source 4 account is Closed Basin to RG af/yr
c                  Nothing to search upon (iopsou(8,k) has q already
c       call oprFind(ityopr(k), 0, idumc,k,ion,iprinto,
c            iopsou(7,k),iopsou(8,k),nx, ciopso4, 1, 
c    1       istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               j. Note source 5 account is Norton Drain South af/yr
c                  Nothing to search upon (iopsou(10,k) has q already
c        call oprFind(ityopr(k), 0, idumc,k,ion,iprinto,
c             iopsou(9,k),iopsou(10,k),nx, ciopso5, 1, 
c    1        istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c		l. Detailed output
c     
        iout18=0
        if(iout18.eq.1) then
          call OprinOut(nlog, maxopr, k, 
     1      ityopr(k), cidvri, 
     1      ciopde, iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k),iopsou(2,k),
     1      ciopso2, iopsou(3,k),iopsou(4,k), creuse, ireuse1,
     1      oprloss(k),  oprlimit(k), iopSou(5,k), 
     1      iopSou(6,k), iopsou(7,k),     
     1      cdivtyp(k),  intern, cntern, cAssoc, cAssoc2, cAssoc3)
        endif  

c
        goto 1190
c
c                                                                       
c _________________________________________________________
c         
c               Type 19; Split Channel
c
c               approach 
c                read source 1 = split channel structure
c                read destination = destination structure
c       
c                then for all water rights tied to the destination 
c                make a new operating rule where the source is a 
c                water right located at the split channel, not the
c                destination.  Note the original right still operates.
c
 1019   continue
        write(nlog,1276) ityopr(k),cidvri
        goto 9999
 
c
c ---------------------------------------------------------
c               a. Read monthly constraints
        idumc=ifix(dumc)
        istop=0        
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2,
     1       ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               b. Find source 1 a diversion water right 
c               store as iopsou(1,k)
c               iok=1 means not OK if right not found
c               ion=1 means do not 
        iok=1
        ion=0
        itype=13
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iopsou(1,k), iopsou(2,k),nx,ciopso1, 1, 
     1       istop, rops2,ioprsw(k), cidvri)
     
        iopSouR(k)=itype     
        iopsou(1,k) = -1*iopsou(1,k)
        ropnk(k)=rdvnk(nx) + small
        write(nlog,*) '  Oprinp; source 1, a water right = ', nx
c
c ---------------------------------------------------------
c               c. Find source 2 a diversion structure where
c                  the split occurs and store in as iopsou(5,k)
        call oprFind(ityopr(k), 3, idumc,k,ion,iprinto,
     1       iopsou(5,k), ix, nx,ciopso2, 1, istop, rops2,
     1       ioprsw(k), cidvri)
c       write(nlog,*) '  Oprinp; source 2, a diversion = ', nx
c
c ---------------------------------------------------------
c               d. Find destination 1 a diversion structure
        call oprFind(ityopr(k), 3, idumc,k,ion,iprinto,
     1       iopdes(1,k), iopdes(2,k),nx,ciopde, 1, 
     1       istop, rops2,ioprsw(k), cidvri)
        iopdesr(k)=3
     
c       write(nlog,*) '  Oprinp; destination 1, a diversion = ', nx
c
c ---------------------------------------------------------
c               e. Set source 3 and 4 to be safe when divcar2 is called
        iopsou(3,k) = 0
        iopsou(4,k) = 0
c
c ---------------------------------------------------------
c		l. Detailed output
c     
        iout19=0
        if(iout19.eq.1) then
          call OprinOut(nlog, maxopr, k, 
     1      ityopr(k), cidvri, 
     1      ciopde, iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k),iopsou(2,k),
     1      ciopso2, iopsou(3,k),iopsou(4,k), creuse, ireuse1,
     1      oprloss(k), oprlimit(k), iopSou(5,k), 
     1      iopSou(6,k),iopsou(7,k),     
     1      cdivtyp(k), intern, cntern, cAssoc, cAssoc2, cAssoc3)
        endif  

        goto 1190
c
c                                                                       
c _________________________________________________________
c         
c               Type 20; San Juan RIP operatin for Navajo
c
c               approach 
c                read source 1 = reservoir
c                read destination = River (target release)
c                read control data for SJRIP per SJRIP Hydrology
c                  Model Documentation (March 24, 2000)
c
c               Read second card for SJRIP
 1020   continue
cx      write(nlog,*) '  Oprinp, SJRIP beginning card 2'
c
c               Warn if SJRIP switch is not on 
        istop=0

        if(isjrip.ne.1) then
          write(nlog,1224) ityopr(k), cidvri
          goto 9999
        endif
c ---------------------------------------------------------
        read(55,134,end=1210,err=928)    sjmina,  sjrela
        if(iecho.eq.1) write(nchk,134)   sjmina,  sjrela
        if(iout.eq.1) then
          write(nlog,*) '  Oprinp, SJRIP data card 2', sjmina, sjrela
        endif
c
c ---------------------------------------------------------
c               a. Read monthly constraints
        idumc=ifix(dumc)
        istop=0
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,
     1       ix, ix, nx,cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               b. Find source 1 a reservoir (type 2)
c               store as iopsou(1,k)
c               iok=1 means not OK if right not found
c		iacc=0 means account = 0 is OK
c               ion=1 means do not 
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found

        iok=1
        ion=0
        itype=2
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iopsou(1,k), iopsou(2,k), nx,ciopso1, iacc, 
     1       istop, rops2,ioprsw(k), cidvri)
        iopSouR(k)=itype     
     
c
c ---------------------------------------------------------
c		l. Detailed output
c     
        iout20=0
        if(iout20.eq.1) then
          call OprinOut(nlog, maxopr, k, 
     1      ityopr(k), cidvri, 
     1      ciopde, iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k),iopsou(2,k),
     1      ciopso2, iopsou(3,k),iopsou(4,k), creuse, ireuse1,
     1      oprloss(k), oprlimit(k), iopSou(5,k), 
     1      iopSou(6,k),iopsou(7,k),     
     1      cdivtyp(k), intern, cntern, cAssoc, cAssoc2, cAssoc3)
        endif  
     
        goto 1190
c
c _________________________________________________________
c               Type 21; Sprinkler Use  
c               Note only admin number is used
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found

 1021   continue
        istop=0
 
c
c               For simulation option do misc checks
        if(ioptio.eq.2 .or. ioptio.eq.8) then
c
c ---------------------------------------------------------
c		 a3. Exit if wells are turned off     
        if(iwell.eq.0) then
          ioprsw(k)=0    
          goto 1190 
        endif        
c
c ---------------------------------------------------------
c               Warn if we do not have necessary input data
          if(ieffmax.eq.1 .or. itsfile.eq.10) then
          else
            ioprsw(k) = 0
            write(nlog,1220) cidvri, ityopr(k), 25
          endif
c
c ---------------------------------------------------------
c               Warn if no time series file
          if(itsfile.le.0) then
            ioprsw(k) = 0
            write(nlog,1221) ityopr(k), cidvri
          endif
c
c ---------------------------------------------------------
c               Warn if no wells 
          if(iwell.ne.1) then
c           ioprsw(k) = 0
            if(ioprsw(k).ne.0) then
              write(nlog,1222) ityopr(k), cidvri
              ioprsw(k)=0
cr            goto 9999
            endif
          endif
c
c ---------------------------------------------------------
c               Warn if no sprinklers
          if(isprink.ne.1) then
c           ioprsw(k) = 0
            if(ioprsw(k).ne.0) then
              write(nlog,1223) ityopr(k), cidvri
              ioprsw(k)=0              
cr            goto 9999
            endif
          endif
        endif
c
c ---------------------------------------------------------
c		l. Detailed output
c     
        iout21=0
        if(iout21.eq.1) then
          call OprinOut(nlog, maxopr, k, 
     1      ityopr(k), cidvri, 
     1      ciopde, iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k),iopsou(2,k),
     1      ciopso2, iopsou(3,k),iopsou(4,k), creuse, ireuse1,
     1      oprloss(k), oprlimit(k), iopSou(5,k), 
     1      iopSou(6,k),iopsou(7,k),     
     1      cdivtyp(k), intern, cntern, cAssoc, cAssoc2, cAssoc3)
        endif  

        goto 1190

c
c _________________________________________________________
c               Type 22; Soil Moisture Use  
c               Note only admin number is used
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found

 1022   continue
        istop=0
 
c       write(nlog,*) '  Oprinp; Soil Moisture Opr Rule read'
c       write(nlog,*) ' Oprinp_2; k, imonsw', (imonsw(k,im),im=1,12)
c
c               For simulation option do misc checks
        if(ioptio.eq.2 .or. ioptio.eq.8) then
c
c ---------------------------------------------------------
c               Warn if we do not have necessary input data
          if(ieffmax.eq.1 .or. itsfile.eq.10) then
          else
            ioprsw(k) = 0
            write(nlog,1220) cidvri, ityopr(k), 26
          endif
c
c ---------------------------------------------------------
c               Warn if we do not have necessary input data
          if(itsfile.le.0) then
            ioprsw(k) = 0
            write(nlog,1221) ityopr(k), cidvri
          endif
        endif
c
c ---------------------------------------------------------
c		l. Detailed output
c     
        iout22=0
        if(iout22.eq.1) then
          call OprinOut(nlog, maxopr, k, 
     1      ityopr(k), cidvri, 
     1      ciopde, iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k),iopsou(2,k),
     1      ciopso2, iopsou(3,k),iopsou(4,k), creuse, ireuse1,
     1      oprloss(k), oprlimit(k), iopSou(5,k), 
     1      iopSou(6,k),iopsou(7,k),     
     1      cdivtyp(k), intern, cntern, cAssoc, cAssoc2, cAssoc3)
     
          write(nlog,*) ' Oprinp_3; k, imonsw',k,(imonsw(k,im),im=1,12)
        endif  

        goto 1190
        
c                                                                       
c _________________________________________________________
c
c		Type 23. Downstream Call (variable date and amount)
c                                                                       
c                  ion=1 means turn off opr right if source right is off
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found

 1023   ion=1
        istop=0 
        idcall=k
        idumc=ifix(dumc)        
c
c ---------------------------------------------------------
c               a. Read monthly constraints 
        istop=0
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               b. Find destination instream flow (type 1)
        call oprFind(ityopr(k), 1, idumc,k,ion,iprinto,
     1       iopdes(1,k), iopdes(2,k), nx,ciopde, 1, 
     1       istop, rops2,ioprsw(k), cidvri)
        iopdesr(k)=1
c
c ---------------------------------------------------------
c		c. Turn off original instream structure rights
c                  since it is now controlled by an opr right 
        do nri=1, numfrr                                            
          if(iifrco(nri).eq.iopdes(1,k)) then    
c                 iifrsw .le. 0 do not operate                            
c                 iifrsw .eq. 0 do not calculate demand                   
c                 iifrsw = -1 do not operate but do calculate demand      
            iifrsw(nri)=-1
            goto 1190                                                   
          endif
        end do
c
c		Remove error warning. OK if there is no isf right 
c               for the downstream call.        
c       write(nlog,760) cidvri, ciopde
c       goto 9999
c
c ---------------------------------------------------------
c		l. Detailed output
c     
        iout23=0
        if(iout23.eq.1) then
          call OprinOut(nlog, maxopr, k, 
     1      ityopr(k), cidvri, 
     1      ciopde, iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k),iopsou(2,k),
     1      ciopso2, iopsou(3,k),iopsou(4,k), creuse, ireuse1,
     1      oprloss(k), oprlimit(k), iopSou(5,k), 
     1      iopSou(6,k),iopsou(7,k),     
     1      cdivtyp(k), intern, cntern, cAssoc, cAssoc2, cAssoc3)
        endif  

        goto 1190
c        
c _________________________________________________________
c
c         
 1024   continue 
c               Type 24; Direct Flow Exchange
c rrb 01/06/20; 
c                destination = diversion ID
c                source 1 = water right that is used as an Alt point
c                source 2 = not used
c		 source 3 = plan ID for a T&C Plan (return flow oblig.)
c                source 4 = CU switch 0=no, 1=yes limit to CU
c                source 5 = operating rule water right where the
c                           percent of water right exchanged is stored
c                           and operated upon during allocation
c		 iExPoint = exchange point
c		 source 7 = reuse switch 0=none, >0=reuse plan
c
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
c                  ion=1 means turn off opr right if right is off
        ion=1
        istop=0
        ndP=0
        
        idumc=ifix(dumc)
c
c ---------------------------------------------------------
c               a1. Read monthly constraints 
        istop=0
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               a2. Read intervening structures without loss
c       write(nlog,*) ' Oprinp; oprloss(k)', oprloss(k)
        ioprloss=int(oprloss(k))
        if(ioprloss.eq.0) then
          istop=0
          itype=21
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
        endif
c        
c ---------------------------------------------------------

c               a3. Read intervening structures plus loss (23)
c       write(nlog,*) 
        if(ioprloss.ne.0) then
          istop=0
          itype=23
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
        endif     
c
c ---------------------------------------------------------
c               b1. Read Monthly plus Annual maxima (oprmax)
        call oprFind(ityopr(k), 22, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
        
c
c ---------------------------------------------------------
c               c1. Find destination diversion 
c                  Note itype=3 for a diversion 
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        idcdX=0
        itype=3
        istop=1
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1, iopdes(2,k), nx,ciopde, 1, istop, rops2,
     1       ioprsw(k), cidvri)
        iopdes(1,k) = iops1
c
c rrb 2009/06/09; Correction        
        if(iops1.gt.0) then
          ndD=iopdes(1,k)
          idcdD=idvsta(ndD)
          idcdX=idcdD        
          iopdesr(k)=3
        endif
c
c ---------------------------------------------------------
c
c rr 2006/04/25; Plan destination
c               c2. Find destination plan
c		istop=0 Stop if a structure is not found
c		istop=1 Do not stop if a structure is not found
        if(iops1.eq.0) then
          istop=1
          call oprFind(ityopr(k), 7, idumc,k,ion,iprinto,
     1         iops1, iopdes(2,k), nx,ciopde, 1, 
     1         istop, rops2,ioprsw(k), cidvri)
          if(iops1.gt.0) then
c
c rrb 2014-06-15; Revise to use iopdesr(l2) as teh type indicator          
cx          iopdes(1,k) = iops1+10000
            iopdes(1,k) = iops1
            ndD=iops1
            ndP=iops1
            idcdD=ipsta(ndP) 
            idcdX=idcdD        
            iopdesr(k)=7
c
c		Check Plan type T&C or Aug Plan         
            iok=1          
            if(iplntyp(ndP).eq.1 .or. iplntyp(ndP).eq.2 .or.
     1         iplntyp(ndP).eq.10.or. iplntyp(ndP).eq.11) iok=0
            if(iok.eq.1) then
              write(nlog,1255) ityopr(k),cidvri, ciopde, creuse, 
     1         iplntyp(ndP)
              goto 9999
            endif            
          endif 
        endif   
c        
c ---------------------------------------------------------
c               c3. Find destination reservoir (type 2)
c                  Note itype=2 for a reservoir
c		   Note istop=0 Stop if not found
c		        istop=1 (OK if not found)
        if(iops1.eq.0) then
          itype=2
          istop=1
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         iops1, iopdes(2,k), nx,ciopde, 1, 
     1         istop, rops2,ioprsw(k), cidvri)
c
c rrb 2014-06-15; Revise to use iopdesr(l2) as the type indicator     
cx        iopdes(1,k)=-iops1
          iopdes(1,k)=iops1
          ndD=iopdes(1,k)
          idcdD=irssta(iops1)          
          idcdX=idcdD        
          iopdesr(k)=2
c         write(nlog,*) ' Oprinp;  iopdes(1,k) = ', iopdes(1,k)
        endif
c        
c ---------------------------------------------------------
c rrb 2008/03/21; Revised warning when several destinations are possible        
        if(iops1.eq.0) then
          write(nlog,1300) cidvri, ityopr(k), ciopde
          goto 9999
        endif   
        
c
c ---------------------------------------------------------
c               d1. Find source 1 a diversion water right
c		                and source account ownership %
c		                Note ion=0 leaves the original water right on
c		                     iacc=0 allows account to be 0 (since 
c                               it is ownership %)
c rrb 05/06/15; Leave original right on
c       ion=1
c rrb 2005/08/11; Turn original right off (ion=1)
cr      ion=0
        ion=1
        iacc=0
        istop=1
        itype=13
        call oprFind(ityopr(k), 13, idumc,k,ion,iprinto,
     1       iopsou(1,k),iopsou(2,k), nx, ciopso1, iacc, 
     1       istop, rops2,ioprsw(k), cidvri)
     
        iopSouR(k)=itype          
c
c
c ---------------------------------------------------------
c		            d2. Set and check percent value provided     
c		    as the second source variable
        oprpct(k)=ciopso1x
        if(oprPct(k).lt.0.0 .or. oprpct(k).gt.100.0) then
          write(nlog,1207) cidvri, ityopr(k), oprpct(k)
          goto 9999
        endif
c
c ---------------------------------------------------------
c		            d3. Set source structure ndS and
c		   decree limit to source (dcrdivS) and 
c                  exchange (dcrdivE)        
        if(nx.gt.0) then
          ndS=idivco(1,nx)     
          idcdS=idvsta(ndS)
          dcrdivS(k)=dcrdiv(nx)*(1.0-oprPct(k)/100.0)
          dcrdivE(k)=dcrdiv(nx)*oprPct(k)/100.0
        else
          write(nlog,1194) cidvri, ityopr(k), ciopso1 
          goto 9999
        endif       
c
c ---------------------------------------------------------
c               e1. Find Source 2 a T&C plan (type 7) 
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        iacc=0
        ion=-1
        istop=0
        if(NAs2.eq.0) then        
          call oprFind(ityopr(k), 7, idumc,k,ion,iprinto,
     1         iops1,iops2, nx, ciopso2, iacc, istop, rops2,
     1         ioprsw(k), cidvri)
          iopsou(3,k)=iops1  
          ipTC=iops1   
c
c		Check plan type (1=T&C)       
          if(iplntyp(iops1).ne.1) then
            write(nlog,1253) ityopr(k),cidvri, ciopso2,
     1       iplntyp(iops1)
            goto 9999
          endif  
          
c
c rrb 2007/08/22; Iopsou(4,k) is used to define the return pattern type
c		  standard = 1, constant = 2, 3 = Mixed, 4 = Default
c		Check that the return type has been specified
          if(iopsou(4,k).le.-2 .or. iopsou(4,k).gt.4) then
            write(nlog,1275) ityopr(k),cidvri, iopsou(4,k)
            goto 9999
          endif
     
        endif
c
c ---------------------------------------------------------
c rrb 2007/08/04;
c               e2. Get the CU factors used to calculate the 
c                    standard component to a T&C obligation
c		   Note istop=0 Stop if not found
c                       itype=26 read 12 efficiency values
       if(NAs2.eq.0 .and. iopsou(4,k).gt.0) then
         istop=0
         itype=26
         call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1        iops1,iops2, nx, creuse, iacc, istop, rops2,
     1        ioprsw(k), cidvri)     
       endif   
       
       
c
c ---------------------------------------------------------
c               f. Find plan named Creuse, if any, and
c		   Store in ireuse(k)
        iacc=0
        ion=-1
        istop=0
        ireuse1=0
        if(NAuse.eq.0) then        
          call oprFind(ityopr(k), 7, idumc,k,ion,iprinto,
     1       ireuse1,iops2, nx, creuse, iacc, 
     1       istop, rops2,ioprsw(k), cidvri)     
        endif
     
c
c ---------------------------------------------------------
c		g. Check proper type of plan 
c                  Note iplntyp 3 & 5 are reservoir, 
c                       iplntyp 4 & 6 are diversion
c                       iplntyp 14 is a WWSP-Supply plan
c
cx        write(nlog,*) ' Oprinp;  cidvri, creuse, ireuse1 ' 
cx        write(nlog,*) ' Oprinp;', cidvri, creuse, ireuse1
        
        if (ireuse1.gt.0) then
          ireuse(k)=ireuse1        

          iok=1  
c        
cx          write(nlog,*) 
cx     1     ' Oprinp; ityopr(k), cidvri, ireuse1, iopdesr(k), iplntyp'
cx          write(nlog,*)
cx     1     ' Oprinp; ',ityopr(k),cidvri, ireuse1, iopdesr(k), 
cx     1     iplntyp(ireuse1)
     
c
c		Reservoir destination (2)          
          if(iopdesr(k).eq.2) then
c
c rrb 2018/08/11; Allow type 14 WWSP Plan type           
cx          if(iplntyp(ireuse1).eq.3 .or. iplntyp(ireuse1).eq.5) iok=0
            if(iplntyp(ireuse1).eq.3 .or. iplntyp(ireuse1).eq.5 .or.
     1         iplntyp(ireuse1).eq.14)    iok=0
            
          endif  
c
c		Diversion destination (3)          
          if(iopdesr(k).eq.3) then
            if(iplntyp(ireuse1).eq.4 .or. iplntyp(ireuse1).eq.6) iok=0
          endif
          
          if(iok.eq.1) then
c 
c rrb 2019/05/17; Update
cx          write(nlog,1255) ityopr(k),cidvri, ciopde, creuse, 
cx   1        iplntyp(ireuse1)
cx
            if(iplntyp(ireuse1).eq.15) then
              write(nlog,12551) ityopr(k),cidvri, creuse, 
     1          iplntyp(ireuse1)
            else                      
              write(nlog,1255) ityopr(k),cidvri, ciopde, creuse, 
     1          iplntyp(ireuse1)
            endif
            goto 9999
          endif  
        endif        
c
c ---------------------------------------------------------
c               g. If a carrier then reset the destination location
        nc=intern(k,1)
        if(nc.gt.0) idcdX=nc
c
c ---------------------------------------------------------
c               h. Find the exchange point (iopsou(6,k)
c                  for the source and destination
cx        itypeEx=1        
cx        call getExPt(nlog, maxdivw, maxuse, maxres, maxplan, 
cx     1    itypeEx, ndS, ndD, ndP, idvsta, irssta, ipsta,
cx     1	  ndnnod, idncod, iopsouX,ioprsw(k), cidvri)
cx        iExPoint(k)=iopsouX
cxc
c ---------------------------------------------------------
c               h. Find the exchange point (iExPoint)
c                  for the source and destination
        
        call oprExp(nlog, maxsta, idcdX, idcdS, idncod, ndnnod, 
     1                    iExPoint(k), cidvri)
        
c
c ---------------------------------------------------------
c               i. Check the Source and Carrier are not the same
c		   Check the Destination and Carrier are not the same
        do i=1,10
          nc=intern(k,i)
          if(nc.gt.0) then
c          
c rrb 2008/06/29; Do not allow the source to be a carrier
c		  (Else not really an exchange)
            if(ndS.eq.nc) then
              write(nlog,1254) ityopr(k), cidvri, ciopso1, cdivid(nc)
              goto 9999
            endif
            
            if(nc.eq.ndd) then
              write(nlog,1261) ityopr(k),cidvri, ciopde, cdivid(nc)
              goto 9999
            endif
          endif
        end do

c        
c ---------------------------------------------------------
c		j. Check the return type matches the data provided
       if(NAs2.eq.0 .and. iopsou(4,k).gt.0) then
         ipTC=iopsou(3,k)
c        write(nlog,*) ' Oprinp; iptc, iprf(iptc)', iptc, iprf(iptc)
         call chkPrf(nlog, ipTC, iprf(ipTC), ityopr(k), iopsou(4,k), 
     1     maxPlan, maxRtnPP, nrtnPP, pcttotPP, iprob,ioprsw(k), cidvri)
         if(iprob.eq.1) then
           write(nlog,1262)  ityopr(k),cidvri, iopsou(4,k)
           goto 9999
         endif
       endif  
c        
c ---------------------------------------------------------
c rrb 2018/10/15; Update to not allow a depletion option
c		k. Check if a depletion option is specified
       if(ideplete.eq.1) then
         write(nlog,1284)  ityopr(k),cidvri, cdivtyp(k)
         goto 9999
       endif
c        
c ---------------------------------------------------------
c		l. Check a return to the river is located properly
c		   itypeR=0 Served directly,
c			 =1 Served by Exchange
cx       if(NAs2.eq.0) then
cx         itypeR=1
cx         call ChkRivRF(nlog, 24, k, fac, maxopr, maxsta,  
cx     1    intern,  idcdD,  itypeR, idncod, ndnnod, cstaid,ioprsw(k), cidvri)
cx       endif    
c
c ---------------------------------------------------------
c		m. Detailed output
c     
        iout24=0
        if(iout24.eq.1) then
          call OprinOut(nlog, maxopr, k, 
     1      ityopr(k), cidvri, 
     1      ciopde, iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k),iopsou(2,k),
     1      ciopso2, iopsou(3,k),iopsou(4,k), creuse, ireuse1,
     1      oprloss(k), oprlimit(k), iopSou(5,k), 
     1      iopSou(6,k),iopsou(7,k),     
     1      cdivtyp(k), intern, cntern, cAssoc, cAssoc2, cAssoc3)
        endif  
c

        
        goto 1190
c _________________________________________________________
c
c         
 1025   continue 
c               Type 25; Direct Flow Bypass
c rrb 01/06/20; 
c                destination = diversion ID or plan ID or reservoir ID
c                source 1 = water right that is used as the source
c                           (Alt point)
c                source 2 = source demand owner (may be > 1)
c                source 4 = N/A, later = maximum diversion % (1-bypass)
c                source 5 = diversion ID where demand is located
c                           for a direct flow exchange it should be the
c                           destination ID.
c                           for an alternate point it should be the 
c                           structure tied to the source water right
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
c
c                  ion=1 means turn off opr right if right is off
        ion=1
        istop=0
        
        idumc=ifix(dumc)
c
c ---------------------------------------------------------
c               a1. Read monthly constraints 
        istop=0
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               a2. Read intervening structures without loss
c       write(nlog,*) ' Oprinp; oprloss(k)', oprloss(k)
        ioprloss=int(oprloss(k))
        if(ioprloss.eq.0) then
          istop=0
          itype=21
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
        endif
     
c
c ---------------------------------------------------------
c               a3. Read intervening structures plus loss (23)
        if(ioprloss.ne.0) then
          istop=0
          itype=23
          call Oprfind(ityopr(k), itype, idumc,k,ion,iprinto,
     1     ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
        endif
c
c ---------------------------------------------------------
c               b1. Read Monthly plus Annual maxima (oprmax)
        call oprFind(ityopr(k), 22, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               c1. Find destination diversion
c		istop=0 Stop if a structure is not found
c		istop=1 Do not stop if a structure is not found
        istop=1
        call oprFind(ityopr(k), 3, idumc,k,ion,iprinto,
     1       iops1, iopdes(2,k),nx,ciopde, 1, istop, rops2,
     1       ioprsw(k), cidvri)
        if(iops1.gt.0) then
          iopdes(1,k) = iops1
          ndD=iops1
          idcdD=idvsta(ndD)        
          idcdX=idcdD            
          iopdesr(k)=3
        endif  
c
c ---------------------------------------------------------
c
c rr 2006/03/29; Plan destination
c               c2. Find destination plan
c		istop=0 Stop if a structure is not found
c		istop=1 Do not stop if a structure is not found            
          if(iops1.eq.0) then
          istop=1
          itype=7
          iacc=1
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         iops1, iopdes(2,k), nx,ciopde, iacc, 
     1         istop, rops2,ioprsw(k), cidvri)
c
          if(iops1.gt.0) then
c
c rrb 2011/11/29; Revise so generic checks work
cx          iopdes(1,k) = iops1+10000
            iopdes(1,k) = iops1
            ndD=iops1
            idcdD=ipsta(ndD) 
            idcdX=idcdD            
            iopdesr(k)=7
c
c		Check Plan type T&C or Aug Plan         
            iok=1 
            if(iplntyp(ndD).eq.1 .or. iplntyp(ndD).eq.2 .or.
     1         iplntyp(ndD).eq.10.or. iplntyp(ndD).eq.11) iok=0  
c               
            if(iok.eq.1) then
              write(nlog,1255) ityopr(k),cidvri, ciopde, creuse, 
     1         iplntyp(ndD)
              goto 9999
            endif            
          endif 
        endif   
c        
c ---------------------------------------------------------
c               c3. Find destination reservoir (type 2)
c                  Note itype=2 for a reservoir
c		istop=0 Stop if a structure is not found
c		istop=1 OK if not found
c        
        if(iops1.eq.0) then
          istop=1
          itype=2
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         iops1, iopdes(2,k), nx,ciopde, 1, 
     1         istop, rops2,ioprsw(k), cidvri)
c
c rrb 2011/11/29; Revise so generic checks work
          iopdes(1,k)=iops1
          ndD=iops1
          idcdD=irssta(ndD)
          idcdX=idcdD            
          iopdesr(k)=2
c         write(nlog,*) ' Oprinp;  iopdes(1,k) = ', iopdes(1,k)
        endif
c        
c ---------------------------------------------------------
c rrb 2008/03/21; Revised warning when several destinations are possible        
        if(iops1.eq.0) then
          write(nlog,1300) cidvri, ityopr(k), ciopde
          goto 9999
        endif   
        
c
c ---------------------------------------------------------
c               d1. Find source 1 a diversion water right
c		   Note:
c                  ion=1 turn off source water right 
c                  istop=1 OK if not found
c		     itype=13 = direct flow right

c rrb 05/06/15; Leave original right on
c       ion=1
c rrb 2005/08/11; Turn original right off (ion=1)
cr      ion=0
        ion=1
        iacc=0
        istop=1
        itype=13
c
c rrb 2011/02/01; Correction iopPs1 should be iops1
cx        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
cx     1       iopPs1,iopsou(2,k), nx, ciopso1, iacc,
cx     1       istop, rops2,ioprsw(k), cidvri)
c
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1,iopsou(2,k), nx, ciopso1, iacc,
     1       istop, rops2,ioprsw(k), cidvri)
c
c rrb 2009/06/12; Enhance to accept an ISF water right
c		   itype=13 = direct flow right

        if(iopS1.ne.0) then
          iopsou(1,k) = iopS1
          iopSouR(k)=itype          
        else
c
c rrb 2009/06/12; Enhance to accept an ISF water right
c		   itype=13 = direct flow right
c		   Note:
c                  ion=1 turn off source water right 
c                  istop=1 OK if not found
c		     itype=11 = ISF flow right
          ion=1
          iacc=0
          istop=0
          itype=11
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iopS1,iopsou(2,k), nx, ciopso1, iacc,
     1       istop, rops2,ioprsw(k), cidvri)
     
          iopsou(1,k) = iopS1
          iopSouR(k)=itype          
        endif
             
c
c ---------------------------------------------------------
c		d2. Set and Check the exchange % provided is OK
        oprpct(k)=ciopso1x        
        if(oprpct(k).lt.0.0 .or. oprpct(k).gt.100.0) then
          write(nlog, 1207) cidvri, ityopr(k), oprpct(k)
          goto 9999
        endif     

c
c ---------------------------------------------------------
c		d3. Set source structure ndS and
c		   decree limit to source (dcrdivS) and 
c                  exchange (dcrdivE)        
        if(nx.gt.0) then
          ndS=idivco(1,nx)                  
          idcdS=idvsta(ndS)
          
          dcrdivS(k)=dcrdiv(nx)*(1.0-oprPct(k)/100.0)
          dcrdivE(k)=dcrdiv(nx)*oprPct(k)/100.0
        else
          write(nlog,1194) cidvri, ityopr(k), ciopso1 
          goto 9999
        endif       
c
c ---------------------------------------------------------
c               e1 Find Source 2
c			T&C source plan (type 7) under iopsou(3,k) 
c                       iopsou(4,k) = 1 yes limit to cu
c		        iopsou(4,k) = 0 no limit to cu
        iacc=0
        ion=-1
        istop=0
cr      if(ciopso2(1:3).ne.'N/A') then        
        if(NAs2.eq.0) then        
          call oprFind(ityopr(k), 7, idumc,k,ion,iprinto,
     1        iops1,iops2, nx, ciopso2, iacc, 
     1        istop, rops2,ioprsw(k), cidvri)
          iopsou(3,k)=iops1
c
c		Check plan type (1=T&C)       
          if(iplntyp(iops1).ne.1) then
            write(nlog,1253) ityopr(k),cidvri, ciopso2,
     1       iplntyp(iops1)
            goto 9999
          endif  
          
c
c rrb 2007/08/22; Iopsou(4,k) is used to define the return pattern type
c		  standard = 1, constant = 2, 3 = Mixed, 4 = Default
c		Check that the return type has been specified
          if(iopsou(4,k).le.-2 .or. iopsou(4,k).gt.4) then
            write(nlog,1275) ityopr(k),cidvri, iopsou(4,k)
            goto 9999
          endif
        endif
c
c ---------------------------------------------------------
c rrb 2007/08/04;
c               e2. Get the CU factors used to calculate the 
c                    standard component to a T&C obligation
c		   Note istop=0 Stop if not found
c                       itype=26 read 12 efficiency values
       if(NAs2.eq.0 .and. iopsou(4,k).gt.0) then
         istop=0
         itype=26
         call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1, iops2, nx, creuse, iacc, istop, rops2,
     1       ioprsw(k), cidvri)     
       endif  
       
c
c ---------------------------------------------------------
c               f. Find plan named Creuse, if any, and
c		               Store in ireuse(k)
        iacc=0
        ion=-1
        istop=0
        ireuse1=0
cr      if(creuse(1:3).ne.'N/A') then
        if(NAuse.eq.0) then        
          call oprFind(ityopr(k), 7, idumc,k,ion,iprinto,
     1       ireuse1,iops2, nx, creuse, iacc, 
     1       istop, rops2,ioprsw(k), cidvri)     
        endif
     
c
c ---------------------------------------------------------
c		g. Check proper type of plan 
c                  Note iplntyp 3 & 5 & 14 are reservoir, 
c                       iplntyp 4 & 6 are diversion
c                       iplntyp 14 is a source plan

        if (ireuse1.gt.0) then
          ireuse(k)=ireuse1        

          iok=1          
c
c		Reservoir destination          
          if(iopdesr(k).eq.2) then
c
c rrb 2018/08/11; Allow type 14 WWSP Plan type          
cx          if(iplntyp(ireuse1).eq.3 .or. iplntyp(ireuse1).eq.5) iok=0
            if(iplntyp(ireuse1).eq.3 .or. iplntyp(ireuse1).eq.5 .or.
     1         iplntyp(ireuse1).eq.14)    iok=0
          endif  
c
c		Diversion destination          
          if(iopdesr(k).eq.3) then
            if(iplntyp(ireuse1).eq.4 .or. iplntyp(ireuse1).eq.6) iok=0
          endif
c
c		Plan type check          
          if(iok.eq.1) then
c 
c rrb 2019/05/17; Update
cx          write(nlog,1255) ityopr(k),cidvri, ciopde, creuse, 
cx   1        iplntyp(ireuse1)
cx
            if(iplntyp(ireuse1).eq.15) then
              write(nlog,12551) ityopr(k),cidvri, creuse, 
     1          iplntyp(ireuse1)
            else            
              write(nlog,1255) ityopr(k),cidvri, ciopde, creuse, 
     1          iplntyp(ireuse1)
            endif
            goto 9999
          endif  
        endif        
        
c
c ---------------------------------------------------------
c               h. If a carrier then reset the destination location
        nc=intern(k,1)
        if(nc.gt.0) idcdX=idvsta(nc)

c
c ---------------------------------------------------------
c               i. Check that the source water right is upstream 
c                  of the destination diversion
c	           trying to find destination (idcdX)
c                  downstream of  source (iscdS) 
c		   Note idcdX= destination diversion, reservoir,
c		   plan or carrier
        iscdS=idvsta(ndS)
        ndns=ndnnod(iscdS)
        iss=iscdS
        csource=cstaid(iss)
        cdest=cstaid(idcdX)
                  
        call oprdown(nlog, maxsta, ndns, iss, idcdX, idncod,
     1       cidvri, csource, cdest)
     
     
c
c ---------------------------------------------------------
c               i. Check the Source and Carrier are not the same
c		   Check the Destination and Carrier are not the same
        do i=1,10
          nc=intern(k,i)
          if(nc.gt.0) then
c          
c rrb 2008/06/29; Do not allow the source to be a carrier
c		  (Else not really an exchange)
            if(ndS.eq.nc) then
              write(nlog,1254) ityopr(k), cidvri, ciopso1, cdivid(nc)
              goto 9999
            endif
            
            if(nc.eq.ndd) then
              write(nlog,1261) ityopr(k),cidvri, ciopde, cdivid(nc)
              goto 9999
            endif
          endif
        end do
c        
c ---------------------------------------------------------
c		k. Check the return type matches the data provided
       if(NAs2.eq.0 .and. iopsou(4,k).gt.0) then
         ipTC=iopsou(3,k)
         call chkPrf(nlog, ipTC, iprf(ipTC),ityopr(k), iopsou(4,k), 
     1     maxPlan, maxRtnPP, nrtnPP, pcttotPP, iprob,ioprsw(k), cidvri)
         if(iprob.eq.1) then
           write(nlog,1262)  ityopr(k),cidvri, iopsou(4,k)
           goto 9999
         endif
       endif    
c        
c ---------------------------------------------------------
c rrb 2018/10/15; Update to not allow a depletion option
c		k. Check if a depletion option is specified
       if(ideplete.eq.1) then
         write(nlog,1284)  ityopr(k),cidvri, cdivtyp(k)
         goto 9999
       endif
c        
c ---------------------------------------------------------
c		l. Check a return to the river is located properly
c		   Note idcdD=destination diversion, reservoir or 
c		   plan, but not the carrier
c		   itypeR=0 Served directly,
c                        =1 Served by Exchange
cx       if(NAs2.eq.0) then
cx         itypeR=0
cx         call ChkRivRF(nlog, 25, k, fac, maxopr, maxsta,  
cx     1    intern,  idcdD,  itypeR, idncod, ndnnod, cstaid,ioprsw(k), cidvri)
cx       endif    
       
c        
c
c ---------------------------------------------------------
c		m. Detailed output
c     
        iout25=0
        if(iout25.eq.1) then
          write(nlog,*) ' Oprinp; ndD, iplntyp(ndD)',ndd,iplntyp(ndD)
          call OprinOut(nlog, maxopr, k, 
     1      ityopr(k), cidvri, 
     1      ciopde, iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k),iopsou(2,k),
     1      ciopso2, iopsou(3,k),iopsou(4,k), creuse, ireuse1,
     1      oprloss(k), oprlimit(k), iopSou(5,k), 
     1      iopSou(6,k),iopsou(7,k),     
     1      cdivtyp(k), intern, cntern, cAssoc, cAssoc2, cAssoc3)
        endif  

        goto 1190
c _________________________________________________________
c
c         
 1026   continue 
c
c rrb 2014-11-24; Add Type 26
c               Type 26; Water Right to a Diversion, Reservoir or Plan
c
cx        write(nlog,1276) ityopr(k),cidvri
cx        goto 9999
c                destination = diversion ID
c                source 1 = water right that is used as an Alt point
c                source 2 = not used
c		             source 3 = plan ID for a T&C Plan (return flow oblig.)
c                source 4 = CU switch 0=no, 1=yes limit to CU
c                source 5 = operating rule water right where the
c                           percent of water right exchanged is stored
c                           and operated upon during allocation
c		             source 7 = reuse switch 0=none, >0=reuse plan
c
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
c           ion=1   means turn off opr right if right is off
c       write(nlog,*) '  Oprinp; Operating type 26'
        ion=1
        istop=0
        ndP=0
        
        idumc=ifix(dumc)
c
c ---------------------------------------------------------
c               a1. Read monthly constraints 
        istop=0
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               b1. Read Monthly plus Annual maxima (oprmax)
        call oprFind(ityopr(k), 22, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c  
c
c ---------------------------------------------------------
c rrb 2018/07/29; 
c		            a5. Read the operating rule associated with
c		                a  Flow Control operating rule
c		                when Oprlimit(k) = 5
c		                istop=0  Stop if not found
c	                 	itype=27 find an operating rule ID & on/off switch
c               
cx     write(nlog,*) ' Oprinp; type 26 ioprlim ', ioprlim(k)
      if(ioprlim(k).eq.5) then
        oprlimit(i) = ioprlim(k)
        istop=0
        itype=27          
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1    iops1, iops2, nx, cAssoc, 1, 
     1    istop, rops2,ioprsw(k), cidvri)
c    
        iopsou(5,k)=iops1
        iopsou(6,k)=iops2 
      endif      
c
c ---------------------------------------------------------
c
c rr 2006/04/25; Plan destination
c               c2. Find destination plan
c		                istop=0 Stop if a structure is not found
c		                istop=1 Do not stop if a structure is not found
c        write(nlog,*) ' '
c        write(nlog,*) '  Oprinp; type 26, iops1 = ', iops1
c        write(nlog,*) ' '
c
        istop=0
        call oprFind(ityopr(k), 7, idumc,k,ion,iprinto,
     1       iops1, iopdes(2,k), nx,ciopde, 1, 
     1       istop, rops2,ioprsw(k), cidvri)
        if(iops1.gt.0) then
c
          iopdes(1,k) = iops1
          ndD=iops1
          ndP=iops1
          idcdD=ipsta(ndP) 
          idcdX=idcdD        
          iopdesr(k)=7
c
c		Check Plan type T&C or Aug Plan         
          iok=1        
c
c rrb 2015/02/03; Require a type 13 plan  
cx        if(iplntyp(ndP).eq.11) iok=0
          if(iplntyp(ndP).eq.13) iok=0
          if(iok.eq.1) then
            write(nlog,1382) ityopr(k),cidvri, ciopde,  
     1       iplntyp(ndP)
            goto 9999
          endif            
        endif 
c ---------------------------------------------------------       
        if(iops1.eq.0) then
          write(nlog,1300) cidvri, ityopr(k), ciopde
          goto 9999
        endif   
        
c
c ---------------------------------------------------------
c               d1. Find source 1 a diversion water right
c		               and source account ownership %
c		   Note ion=0 leaves the original water right on
c		        iacc=0 allows account to be 0 (since 
c                       it is ownership %)
c rrb 05/06/15; Leave original right on
c       ion=1
c rrb 2005/08/11; Turn original right off (ion=1)
cr      ion=0
        ion=1
        iacc=0
        istop=1
        itype=13
        call oprFind(ityopr(k), 13, idumc,k,ion,iprinto,
     1       iopsou(1,k),iopsou(2,k), nx, ciopso1, iacc, 
     1       istop, rops2,ioprsw(k), cidvri)
     
        iopSouR(k)=itype          
c
c
c ---------------------------------------------------------
c		d2. Set and check percent value provided     
c		    as the second source variable
        oprpct(k)=ciopso1x
        if(oprPct(k).lt.0.0 .or. oprpct(k).gt.100.0) then
          write(nlog,1207) cidvri, ityopr(k), oprpct(k)
          goto 9999
        endif
c
c ---------------------------------------------------------
c		d3. Set source structure ndS and
c		    decree limit to source (dcrdivS) and changed WR
c       destination (dcrdivE)        
        if(nx.gt.0) then
          ndS=idivco(1,nx)     
          idcdS=idvsta(ndS)
          dcrdivS(k)=dcrdiv(nx)*(1.0-oprPct(k)/100.0)
          dcrdivE(k)=dcrdiv(nx)*oprPct(k)/100.0
        else
          write(nlog,1194) cidvri, ityopr(k), ciopso1 
          goto 9999
        endif       
c
c ---------------------------------------------------------
c		l. Detailed output
c     
        iout26=0
        if(iout26.eq.1) then
          call OprinOut(nlog, maxopr, k, 
     1      ityopr(k), cidvri, 
     1      ciopde, iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k),iopsou(2,k),
     1      ciopso2, iopsou(3,k),iopsou(4,k), creuse, ireuse1,
     1      oprloss(k), oprlimit(k), iopSou(5,k),
     1      iopSou(6,k),iopsou(7,k),     
     1      cdivtyp(k), intern, cntern, cAssoc, cAssoc2, cAssoc3)
        endif  
c
        goto 1190
     
c _________________________________________________________
c
c         
 1027   continue 
c               Type 27 Reservoir or Reuse Plan (4 or 6) or
c                 Accounting plan (11) to a Diversion or Reservoir 
c			            or Plan or Instream Flow
c			          If the source is a Reservoir the destination
c			            Plan should be a type 9 (OOP Plan)
c			          If the source is a Plan the 
c			            Plan should be a Reuse plan (type 4 or 6) or
c                 OOP plan (type 9) or an Accounting Plan (type 11)
c rrb 2014/11/24 If the source is an admin plan, variable creuse
c                 should be the water right source
c rrb 2006/07/26; Revised to allow the source to be a reservoir
c rrb 2007/08/17; Revised to allow the destination to be a plan
        ion=1
        istop=0
        
        idumc=ifix(dumc)
c
c ---------------------------------------------------------
c               a1. Read monthly constraints 
        istop=0
        itype=20
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               a2. Read intervening structures without loss
c       write(nlog,*) ' Oprinp; oprloss(k)', oprloss(k)
        ioprloss=int(oprloss(k))
        if(ioprloss.eq.0) then
          istop=0
          itype=21
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)   
c
c rrb 2009/06/25; Remove capability to read without loss data
          if(nx.gt.0) goto 2050         
        endif
c        
c ---------------------------------------------------------

c               a3. Read intervening structures plus loss (23)
c		    Note type 23 checks for a diversion or a
c		    stream node
c       write(nlog,*) 
        if(ioprloss.ne.0) then
          istop=0
          itype=23
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
        endif          
c
c ---------------------------------------------------------
c rrb 2007/07/03; 
c		            a4. Read the operating rule associated with
c		                a monthly or annual plan limit adjustment
c		                when Oprlimit(k) > 0
c		                istop=0  Stop if not found
c	     	            itype=24 Operating Rule ID with monthly and annual
c                   plan limits
c
c       write(nlog,*) ' Oprinp; type 27 ioprlim ', ioprlim(k) 
c
c ---------------------------------------------------------
c rrb 2015/03/07; Update
c              a4-1 Save type 1 & 6 for future applications
        if(ioprlim(k).eq.1 .or. ioprlim(k).eq.6) then
          write(nlog,12660) ityopr(k),cidvri, ioprlim(k)
          goto 9999          
        endif
c 
c ---------------------------------------------------------
c rrb 2015/03/07; Update
c		            a4-2. Read ioprlim = 2,4,7 & 9
c	     	              itype=24 Operating Rule ID 
cx      if(ioprlim(k).gt.0) then 
        if(ioprlim(k).eq.2 .or. ioprlim(k).eq.4 .or.
     1     ioprlim(k).eq.7 .or. ioprlim(k).eq.9) then
          istop=0
          itype=24          
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1      ix, ix, nx, cAssoc, 1, istop, rops2,ioprsw(k), cidvri)
          nx5=nx
          iopsou(5,k)=nx5         
c
c rrb 2015-03-07; Revise the first entry is a plan limit (ytpe 47)
cx          if(ioprlim(k).eq. 2 .or. ioprlim(k).eq. 4) then
          lopr5=iopsou(5,k)
          if(ityopr(lopr5).ne.47) then
            write(nlog,12662) ityopr(k),cidvri, ioprlim(k), cAssoc 
            goto 9999           
          endif
cx        endif
        endif
c
c ----------------------------------------------------------------
c rrb 2015/03/07; Update
c		            a4-3. Read ioprlim = 3, 4, 8 & 9           
c	     	              itype=24 Operating Rule ID 
        if(ioprlim(k).eq.3 .or. ioprlim(k).eq.4 .or.
     1     ioprlim(k).eq.8 .or. ioprlim(k).eq.9) then
          istop=0
          itype=24          
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1      ix, ix, nx, cAssoc2, 1, istop, rops2,ioprsw(k), cidvri)
          nx6=nx
          iopsou(6,k)=nx6             
        endif
c
c ----------------------------------------------------------------
c rrb 2015/03/07; Update
c		            a4-4. Read ioprlim = 5, 7, 8 & 9
c	     	              itype=24 Operating Rule ID 
        if(ioprlim(k).eq.5 .or. ioprlim(k).eq.7 .or.
     1    ioprlim(k).eq.8 .or. ioprlim(k).eq.9) then     
          istop=0
          itype=24          
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1      ix, ix, nx, cAssoc3, 1, istop, rops2,ioprsw(k), cidvri)
          nx7=nx
          iopsou(7,k)=nx7 
        endif
c
c ----------------------------------------------------------------
c rrb 2015/03/07; Update
c		            a4-5. Set ciopsoX5 for reporting
cx        if(ioprlim(k).eq.1 .or. ioprlim(k).eq.2 .or.
cx     1     ioprlim(k).eq.4) then
        if(ioprlim(k).eq.2 .or. ioprlim(k).eq.4 .or.
     1    ioprlim(k).eq.7 .or. ioprlim(k).eq.9) then     
cx        ip5=iopsou(1,nx)
          ip5=iopsou(1,nx5)
          ciopsoX5(k)=pid(ip5)
        endif            
c
c ---------------------------------------------------------
c               b1. Find the destination, a diversion (type 3)
c		                set istop=1 (OK if not found)
c		                itype=3 diversion
        istop=1
        itype=3
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1, iopdes(2,k), nx,ciopde, 0, istop, rops2,
     1       ioprsw(k), cidvri)
        if(iops1.gt.0) then
          iopdes(1,k)=iops1
          idcdD=idvsta(iopdes(1,k))
          idcdX=idcdD
c
c rrb 2007/08/17; Set destination type            
          iopdesr(k)=3          
        endif  
c        
c ---------------------------------------------------------
c               b2. Find destination reservoir (type 2)
c		                istop=1 (OK if not found)
c			              iacc=1 (check account number)
c			              itype=2 Reservoir
        if(iops1.eq.0) then
          istop=1
          iacc=1
          itype=2
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         iops1, iopdes(2,k), nx,ciopde, iacc, 
     1         istop, rops2,ioprsw(k), cidvri)
          if(iops1.gt.0) then
            iopdes(1,k)=iops1
            idcdD=irssta(iops1)
            idcdX=idcdD
            iopdesr(k)=2
c           write(nlog,*) ' Oprinp;  iopdes(1,k) = ', iopdes(1,k)
          endif
        endif
c        
c ---------------------------------------------------------
c rrb 2007/08/17; 
c               b3. Find destination plan.  Should be a T&C plan
c                   (type 1) or a augmentation Plan (type 2) or
c                    an Accounting Plan (type 11) or a Changed Water 
c                    Right Plan (type 13)
c                    istop=0 (Stop if not found)
c			iacc=1 (check account number)
c			itype=7 Plan
        if(iops1.eq.0) then
          istop=1
          iacc=1
          itype=7
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         iops1, iopdes(2,k), nx,ciopde, iacc, 
     1         istop, rops2,ioprsw(k), cidvri)    
c
c rrb 2012/01/19; Correction 
          if(iops1.gt.0) then     
            iopdes(1,k)=iops1
            idcdD=ipsta(iops1)
            idcdX=idcdD
            iopdesR(k)=7
c           write(nlog,*) ' Oprinp;  iopdes(1,k) = ', iopdes(1,k)              
c
c rrb 2011/11/27; Check the Destination Plan type provided
c                 Note may be a T&C Plan (type 1), Aug Plan (type 2),
c                 
            ndP=iops1         
            iok=1          
c
c rrb 2015/03/07; Allow new plan type
            if(iplntyp(ndP).eq.1 .or. iplntyp(ndP).eq.2 .or.
cx   1         iplntyp(ndP).eq.11) iok=0
     1         iplntyp(ndP).eq.11 .or. iplntyp(ndP).eq.13) iok=0
            if(iok.eq.1) then
              write(nlog,12661) ityopr(k),cidvri, ciopde, iplntyp(ndP) 
              goto 9999
            endif    
          endif             
        endif
c        
c ---------------------------------------------------------
c rrb 2008/03/21; Allow the destination to be an ISF structure
c               b4. Find the destination instream flow (type 1)
c                       istop=0 (Stop if not found)
c			istop=1 (OK if not found)
c			iacc=1 (check account number)
c			itype=1 ISF structure
        if(iops1.eq.0) then
          istop=1
          iacc=1
          itype=1
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         iops1, iopdes(2,k), nx,ciopde, iacc, 
     1         istop, rops2,ioprsw(k), cidvri)
          iopdes(1,k)=iops1
          idcdD=ifrsta(iops1)
          idcdX=idcdD
          iopdesR(k)=1
c         write(nlog,*) ' Oprinp;  iopdes(1,k) = ', iopdes(1,k)
        endif
c        
c ---------------------------------------------------------
c rrb 2008/03/21; Revised warning when several destinations are possible        
        if(iops1.eq.0) then
          write(nlog,1300) cidvri, ityopr(k), ciopde
          goto 9999
        endif   
        
c
c ---------------------------------------------------------
c               c1. Find source 1 a plan where the plan type
c                   is a Reuse Plan (type 4 or 6), an OOP plan 
c                   (type 9), an Accounting Plan (type 11) or
c                   a Changed Water Right Plan (type 13)
c                    Note: istop=1 OK if not found)
c			                     itype=7 Plan structure
        iopsouP=0
        istop=1
        itype=7
        iacc=1
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1,iopsou(2,k), nx, ciopso1, iacc, istop, rops2,
     1       ioprsw(k), cidvri)
     
        if(iops1.gt.0) then
        
          iopSouR(k)=itype             
          iopsou(1,k)=-iops1
          idcdS=ipsta(iops1)        
c
c rrb 2014/12/14; Set the plan type for checking
          iopsouP=iplntyp(iops1)
c
c		Set Plan Ownership 
          oprpct(k)=ciopso1x
          if(oprPct(k).lt.small) then
            write(nlog,1206) cidvri, ityopr(k), oprpct(k)
            goto 9999
          endif          
          
          if(oprPct(k).lt.0.0 .or. oprpct(k).gt.100.0) then
            write(nlog,1207) cidvri, ityopr(k), oprpct(k)
            goto 9999
          endif          
        endif  
c
c ---------------------------------------------------------
c               c2. Find source 1 a reservoir (type 2)
c                       istop=0 Stop if not found
c			iacc=1 (check account number)
c			itype=2 Reservoir
        if(iops1.eq.0) then
          istop=0
          iacc=1
          itype=2
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         iops1,iopsou(2,k), nx, ciopso1, iacc, istop, rops2,
     1         ioprsw(k), cidvri)
     
          iopSouR(k)=itype     
          iopsou(1,k)=iops1
          idcdS=irssta(iops1)                       
        endif  
c
c ---------------------------------------------------------
c               d1. Find Source 2 a T&C plan (type 7) or a
c                   WWSP User Plan (type 15) 
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found

        iacc=0
        ion=-1
        istop=0
        itype=7
        if(NAs2.eq.0) then        
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         iops1,iops2, nx, ciopso2, iacc, istop, rops2,
     1         ioprsw(k), cidvri)
          iopsou(3,k)=iops1     
c
c		Check for the proper plan type          
c rrb 2018/09/15; Update for a WWSP limit
cx        if(iplntyp(iops1).ne.1) then
cx
          iok=0 
          if(iplntyp(iops1).eq.1) iok=1
c
c rrb 2019/04/20; Revise to be a WWSP User Plan (type 15)
cx        if(iplntyp(iops1).eq.14) iok=1
          if(iplntyp(iops1).eq.15) iok=1
          
          if(iok.eq.0) then       
            write(nlog,1253) ityopr(k),cidvri, ciopso2,
     1       iplntyp(iops1)
            goto 9999
          endif  
c
c rrb 2007/08/22; Iopsou(4,k) is used to define the return pattern type
c		Check that the return type has been specified where
c		  standard = 1, fixed (constant) = 2, & mixed (standard and fixed)  = 3
c
c rrb 2018/09/15; Update for a WWSP limit
          if(iplntyp(iops1).eq.1) then
            if(iopsou(4,k).le.0 .or. iopsou(4,k).ge.4) then
              write(nlog,1275) ityopr(k),cidvri, iopsou(4,k)
              goto 9999
            endif
          endif
     
        endif
c
c ---------------------------------------------------------
c rrb 2007/08/04;
c               d2. Get the CU factors to calculate the 
c                    Standard Component to a T&C obligation
c		   Note istop=0 Stop if not found
c                       itype=26 read 12 efficiency values
       if(NAs2.eq.0 .and. iopsou(4,k).gt.0) then
         istop=0
         itype=26
         call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1        iops1, iops2, nx, creuse, iacc, istop, rops2,
     1        ioprsw(k), cidvri)     
       endif    
        
        
c     
c ---------------------------------------------------------
c               e. If a carrier then reset the destination location
c		               to the first carrier
        nc=intern(k,1)
        if(nc.gt.0) idcdX=idvsta(nc)
c
c ---------------------------------------------------------
c		            f.Check the source is upstream of the destination 
c		              where idcdX is the destination diversion, reservoir,
c		              plan or carrier   
        ndns=ndnnod(idcdS)
        iss=idcdS
        csource=cstaid(iss)
        cdest=cstaid(idcdX)
                  
        call oprdown(nlog, maxsta, ndns, idcdS, idcdX, idncod, 
     1       cidvri, csource, cdest)
c
c ---------------------------------------------------------
c               g. Set the Exchange point to the destination location
c		               required to analyze depletion Vs diversion
c       iopsou(3,k) = idcdd
        iExPoint(k) = idcdd
c
c ------------------------------------------------------------------
c               h. Find a destination reuse plan named Creuse, if any, 
c		                and store in ireuse(k)
c		                istop=0  Stop if not found
c                   ion=1 means turn off opr right if right is off
c                   itype=7 is a plan
c                   Nause=0 means creuse has data (it is not 'NA')

        iacc=0
        ion=-1
c
c rrb 2015/02/03X; When the source is an admin plan, revise to allow
c                  this read to fail (creuse is not a plan
c                  so that creuse can be a source operating rule 
c rrb 2015/03/07; Back to Oprlimit =1-9 logic
cx      istop=0
cx      istop=1
        istop=0
        itype=7
        ireuse1=0        
        if(NAuse.eq.0) then                
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         ireuse1,iops2, nx, creuse, iacc, istop, rops2,
     1         ioprsw(k), cidvri)          
        endif
c
c ---------------------------------------------------------
c		            j. Check proper type of reuse plan 
c                  Note iplntyp 3 & 5 are reservoir, 
c                       iplntyp 4 & 6 are diversion
c                       iplntyp 7 is an admin plan
c        write(nlog,*) '  Oprinp; NAuse, creuse, ireuse1'
c        write(nlog,*) '  Oprinp; ', NAuse, creuse, ireuse1
        if(ireuse1.gt.0) then
          ireuse(k)=ireuse1        
          iok=1
c
c		If the source is a plan and the destination is a reservoir
c               the reuse plan should be types 3 or 5
          if(iopsou(1,k).lt.0 .and. iopdesr(k).eq.2) then          
            if(iplntyp(ireuse1).eq.3 .or. iplntyp(ireuse1).eq.5) iok=0
          endif  
c
c		If the source is a plan and the destination is a diversion 
c               the reuse plan should be type 4 or 6          
          if(iopsou(1,k).lt.0 .and. iopdesr(k).eq.3) then
            if(iplntyp(ireuse1).eq.4 .or. iplntyp(ireuse1).eq.6) iok=0
          endif
c
c		If the source is a reservoir the reuse plan should be type 9 
c             (OOP Plan)          
          if(iopsou(1,k).gt.0) then
            if(iplntyp(ireuse1).eq.9) iok=0
          endif

          if(iok.eq.1) then
            write(nlog,1255) ityopr(k),cidvri, ciopde, creuse,
     1         iplntyp(ireuse1)
            goto 9999
          endif
        endif        
c
c ------------------------------------------------------------------
c rrb 2015/03/07; Revise to store source plan in iopsou(7)
cxc rrb 2015/02/03X; Read source water right from variable Creuse
cxc                k. If the source is an admin plan, find 
cxc                   a source operating rule named Creuse, if any, 
cxc		                and store in ireuse(k)
cxc
cxc		                istop=0  Stop if not found
cxc                   ion=1 means turn off opr right if right is off
cxc                   itype=14 is an operating rule
cxc                   Nause=0 means creuse has data (it is not 'NA')
cxc        write(nlog,*) '  Oprinp; getting operating rule near 4812'
cxc        write(nlog,*) '  Oprinp; NAuse, iopsouR(k), iopsouP'
cxc        write(nlog,*) '  Oprinp; ', NAuse, iopsouR(k), iopsouP
cx        if(iopsouR(k).eq.7 .and. iopsouP.eq.11) then
cx          iacc=0
cx          ion=0
cx          istop=0
cx          ireuse1=0        
cx          if(NAuse.eq.0) then    
cx            istop=0
cx            itype=14                    
cx            call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
cx     1           ireuse1,iops2, nx, creuse, iacc, istop, rops2,
cx     1           ioprsw(k), cidvri)     
cx            ireuse(k)=ireuse1       
cx          else
cx            write(nlog,937)  corid(k), ityopr(k), ciopso1, cReuse
cx            goto 9999       
cx          endif 
cx             
cxc          write(nlog,*) '  Oprinp;    ireuse1, ireuse(k)'
cxc          write(nlog,*) '  Oprinp; ', ireuse1, ireuse(k)             
cxc       
cxc rrb 25/02/03X; Check the source is a type 26 plan
cx          if(ireuse1.gt.0) then           
cx            lr5=ireuse(k)          
cx            if(ityopr(lr5).ne.26) then
cx              write(nlog,937)  corid(k), ityopr(k), ciopso1, cReuse
cx              goto 9999
cx            endif    
cx          endif               
cx        endif
cxc
c ---------------------------------------------------------
c               l. Set the release type demand or depletion
c		   Note a destination reservoir is always a diversion
        rec12=cdivtyp(k)
        iok=1
        if(iopdes(1,k).gt.0) then
          if(rec12(1:9).eq.'Diversion') iok=0
          if(rec12(1:9).eq.'Depletion') iok=0
        else
          if(rec12(1:9).eq.'Diversion') iok=0
        endif  
        if(iok.eq.1) then 
          write(nlog,1258) ityopr(k), cidvri, cdivtyp(k)
          goto 9999
        endif    
c        
c ---------------------------------------------------------
c		            m. Check the return type matches the data provided
       if(NAs2.eq.0 .and. iopsou(4,k).gt.0) then
         ipTC=iopsou(3,k)
         call chkPrf(nlog, ipTC, iprf(ipTC), ityopr(k), iopsou(4,k), 
     1     maxPlan, maxRtnPP, nrtnPP, pcttotPP, iprob,ioprsw(k), cidvri)           
         if(iprob.eq.1) then
           write(nlog,1262)  ityopr(k),cidvri, iopsou(4,k)
           goto 9999
         endif
       endif  
c
c ---------------------------------------------------------
c rrb 2015/03/07; Update
c             n. Check the source water right and Ioprlim are set 
c                properly when the source is a Changed Water Right
c                Plan (type 13)
c
       if(iopsouR(k).eq.7 .and. iopsouP.eq.13) then
         iok=1
         if(ioprlim(k).eq.5 .or. ioprlim(k).eq. 7 .or.
     1     ioprlim(k).eq.8 .or. ioprlim(k).eq. 9) iok=0
c
         if(iok.eq.1) then
           write(nlog,937)  corid(k), ityopr(k), ciopso1, iopsouP,
     1                     ioprlim(k)
           goto 9999
         endif
       endif  
c  
       if(ioprlim(k).eq.5 .or. ioprlim(k).eq. 7 .or.
     1   ioprlim(k).eq.8 .or. ioprlim(k).eq. 9) then
         iok=1
         if(iopsouR(k).eq.7 .and. iopsouP.eq.13) iok=0
c         
         if(iok.eq.1) then
           write(nlog,937)  corid(k), ityopr(k), ciopso1, iopsouP,
     1                     ioprlim(k)
           goto 9999
         endif
       endif 
c
c ---------------------------------------------------------
c		            n. Detailed output
c     
        iout27=0
        if(iout27.eq.1) then
          call OprinOut(nlog, maxopr, k, 
     1      ityopr(k), cidvri, 
     1      ciopde, iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k),iopsou(2,k),
     1      ciopso2, iopsou(3,k),iopsou(4,k), creuse, ireuse1,
     1      oprloss(k), oprlimit(k), iopSou(5,k),
     1      iopSou(6,k),iopsou(7,k),     
     1      cdivtyp(k), intern, cntern, cAssoc, cAssoc2, cAssoc3)
        endif  
c
c        
        goto 1190
     
        
c _________________________________________________________
c
c         
 1028   continue 
c               Type 28; Plan to a Div or Res by Exchange
c rrb 01/06/20; 
c                destination = a diversion
c                source 1 (iopsou(1,k) = a plan
c                source 2 (iopsou(3,k) = N/A
c                  ion=1 means turn off opr right if right is off
        ion=1
        idumc=ifix(dumc)  
c
c ---------------------------------------------------------
c               a1. Read monthly constraints 
        istop=0
        cx='NA'
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,ix, ix,
     1               nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               a2. Read intervening structures without loss
        ioprloss=int(oprloss(k))
        if(ioprloss.eq.0) then
          istop=0
          itype=21
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c rrb 2009/06/25; Remove capability to read without loss data
          if(nx.gt.0) goto 2050     
        endif
c        
c ---------------------------------------------------------

c               a3. Read intervening structures plus loss (23)
c		               Note type 23 checks for a diversion or a
c		               stream node
c       write(nlog,*) 
        if(ioprloss.ne.0) then
c         write(nlog,*) ' Oprinp; ioprloss', ioprloss
          istop=0
          itype=23
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
        endif          
     
c
c ---------------------------------------------------------
c rrb 2007/07/03; 
c		            a4. Read the operating rule associated with
c		               a monthly or annual plan limit adjustment
c		               when Oprlimit(k) > 0
c		               istop=0  Stop if not found
c		               itype=24 Operating Rule with monthly and annual
c                        plan limits
c
c ---------------------------------------------------------
c rrb 2015/03/07; Update
c              a4-1 Save type 1 & 6 for future applications
        if(ioprlim(k).eq.1 .or. ioprlim(k).eq.6) then
c
c rrb 2019/09/07; Correct warning
cx        write(nlog,12662) ityopr(k),cidvri, ioprlim(k)
          write(nlog,12660) ityopr(k),cidvri, ioprlim(k)
          goto 9999          
        endif
c       
c ---------------------------------------------------------
c rrb 2015/03/07; Update
c		            a4-2 Read ioprlim = 2, 4, 7 & 9
cx        if(ioprlim(k).eq.1 .or. ioprlim(k).eq.2 .or. 
cx     1     ioprlim(k).eq.4) then
        if(ioprlim(k).eq.2 .or. ioprlim(k).eq.4 .or.
     1     ioprlim(k).eq.7 .or. ioprlim(k).eq.9) then
     
          istop=0
          itype=24
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1      ix, ix, nx, cAssoc, 1, istop, rops2,ioprsw(k), cidvri)
          nx5=nx
          iopsou(5,k)=nx5
          if(ityopr(nx5).ne.47) then
            write(nlog,12662) ityopr(k),cidvri, ioprlim(k), cAssoc 
            goto 9999           
          endif       
        endif
c
c ---------------------------------------------------------
c 
c rrb 2015/03/07; Update
c		            a4-3. Read ioprlim = 3, 4, 8 & 9         
cx      if(ioprlim(k).eq.3 .or. ioprlim(k).eq.4) then
        if(ioprlim(k).eq.3 .or. ioprlim(k).eq.4 .or.
     1    ioprlim(k).eq.8 .or. ioprlim(k).eq.9) then        
          istop=0
          itype=24          
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1      ix, ix, nx, cAssoc2, 1, istop, rops2,ioprsw(k), cidvri)
          nx6=nx
          iopsou(6,k)=nx6             
        endif
c
c ----------------------------------------------------------------
c rrb 2015/03/07; Update 
c		            a4-4. Read ioprlim = 5, 7, 8 & 9
        if(ioprlim(k).eq.5 .or. ioprlim(k).eq.7 .or.
     1    ioprlim(k).eq.8 .or. ioprlim(k).eq.9) then     
          istop=0
          itype=24          
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1      ix, ix, nx, cAssoc3, 1, istop, rops2,ioprsw(k), cidvri)
          nx7=nx
          iopsou(7,k)=nx7 
        endif
        
c ----------------------------------------------------------------
c rrb 2015/03/07; Update 
c		            a4-5. Set CiopsoX5 for reporting
cx     1     ioprlim(k).eq.4) then
        if(ioprlim(k).eq.2 .or. ioprlim(k).eq.4 .or.
     1    ioprlim(k).eq.7 .or. ioprlim(k).eq.9) then     
cx        ip5=iopsou(1,nx)
          ip5=iopsou(1,nx5)
          ciopsoX5(k)=pid(ip5)
        endif           
        
c
c ---------------------------------------------------------
c               b1. Find the destination, a diversion ID (type 3)
c		    istop=1 (OK if not found)
        istop=1
        itype=3
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1, iopdes(2,k), nx,ciopde, 0, istop, rops2,
     1       ioprsw(k), cidvri)
     
        if(iops1.gt.0) then
c
c rrb 2007/10/26; Revise to rely on variaable iopdesr        
c         iopdes(1,k)=-iops1
          iopdes(1,k)=iops1
          idcdD=idvsta(iops1)
          idcdX=idcdD
c
c rrb 2007/08/17; Set destination type            
          iopdesr(k)=3
        endif  
c        
c ---------------------------------------------------------
c               b2. Find destination reservoir (type 2)
c  		    istop=1 (OK if not found)
        if(iops1.eq.0) then
          istop=1
          itype=2
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         iops1, iopdes(2,k), nx,ciopde, 1, istop, rops2,
     1         ioprsw(k), cidvri)
          if(iops1.gt.0) then
            iopdes(1,k)=iops1
            idcdD=irssta(iops1)
            idcdX=idcdD
            iopdesr(k)=2
          endif  
c         write(nlog,*) '  Oprinp; Type 28 iopdes(1,k) = ', iopdes(1,k)
        endif
c        
c ---------------------------------------------------------
c               b3. Find destination plan (type 7)
c  		    istop=1 (OK if not found)
c
c rrb 2007/08/17; Allow the destination to be a Plan
        if(iops1.eq.0) then
          istop=1
          itype=7
          iacc=1
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         iops1, iopdes(2,k), nx,ciopde, iacc, istop, rops2,
     1         ioprsw(k), cidvri)
          if(iops1.gt.0) then
            iopdes(1,k)=iops1
            idcdD=ipsta(iops1)
            idcdX=idcdD
            iopdesr(k)=7
          endif  
c         write(nlog,*) '  Oprinp; Type 28 iopdes(1,k) = ', iopdes(1,k)
        endif
c        
c ---------------------------------------------------------
c rrb 2008/03/21; 
c               b4. Find the destination ISF (type 1)
c                    istop=0 Stop if not found
c			istop=1 Do not stop if not found
c			iacc=1 (check account number)
c			itype=1 ISF structure
        if(iops1.eq.0) then
          istop=1
          iacc=1
          itype=1
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         iops1, iopdes(2,k), nx,ciopde, iacc, 
     1         istop, rops2,ioprsw(k), cidvri)
          iopdes(1,k)=iops1
          idcdD=ifrsta(iops1)
          idcdX=idcdD
          iopdesr(k)=1
c         write(nlog,*) ' Oprinp;  iopdes(1,k) = ', iopdes(1,k)
        endif
c        
c ---------------------------------------------------------
c rrb 2008/03/21; Revised warning when several destinations are possible        
        if(iops1.eq.0) then
          write(nlog,1300) cidvri, ityopr(k), ciopde
          goto 9999
        endif   
     
c
c ---------------------------------------------------------
c               c1. Find source 1 a plan (type 7)
c                       istop=1 OK if not found
        iopsouP=0
        istop=1
        itype=7
        iacc=1
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1,iopsou(2,k), nx, ciopso1, iacc, istop, rops2,
     1       ioprsw(k), cidvri)

        if(iops1.gt.0) then
        
          iopSouR(k)=itype        
          iopsou(1,k)=-iops1
          idcdS=ipsta(iops1)  
c
c rrb 2014/12/14; Set the plan type for checking
          iopsouP=iplntyp(iops1)     
c
c		Set Ownership 
          oprpct(k)=ciopso1x
          
          if(oprPct(k).lt.small) then
            write(nlog,1206) cidvri, ityopr(k), oprpct(k)
            goto 9999
          endif          
          
          if(oprPct(k).lt.0.0 .or. oprpct(k).gt.100.0) then
            write(nlog,1207) cidvri, ityopr(k), oprpct(k)
            goto 9999
          endif          
        endif  
c
c ---------------------------------------------------------
c               c2. Find source 1 a reservoir (type 2)
c                       istop=0 Stop if not found
c			iacc=1 (check account number)
c			itype=2 Reservoir
        if(iops1.eq.0) then
          istop=0
          iacc=1
          itype=2
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1        iops1,iopsou(2,k), nx, ciopso1, iacc, istop, rops2,
     1         ioprsw(k), cidvri)
     
          iopSouR(k)=itype     
          iopsou(1,k)=iops1
          idcdS=irssta(iops1)                       
        endif  
c
c ---------------------------------------------------------
c               d1. Find Source 2 a T&C plan (type 7) or a
c                     WWSP plan (type 7) 
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found

        ion=-1
        istop=0
        iacc=1
        if(NAs2.eq.0) then 
          itype=7       
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         iops1,iops2, nx, ciopso2, iacc, istop, rops2,
     1         ioprsw(k), cidvri)

          iopSouR(k)=itype
          iopsou(3,k)=iops1    
c
c		     Check plan type (1=T&C & 14 = WWSP) 
c rrb 2018/09/15; Update for a WWSP limit      
cx        if(iplntyp(iops1).ne.1) then
cx
          iok=0 
          if(iplntyp(iops1).eq.1) iok=1
c
c rrb 2019/04/20; Revise to be a WWSP User Plan (type 15)
cx        if(iplntyp(iops1).eq.14) iok=1
          if(iplntyp(iops1).eq.15) iok=1
          
          if(iok.eq.0) then          
            write(nlog,1253) ityopr(k),cidvri, ciopso2,
     1       iplntyp(iops1)
            goto 9999
          endif           
c
c rrb 2007/08/22; Iopsou(4,k) is used to define the return pattern type
c		  standard = 1, constant = 2
c		Check that the return type has been specified
          if(iplntyp(iops1).eq.1) then
            if(iopsou(4,k).le.0 .or. iopsou(4,k).gt.4) then
              write(nlog,1275) ityopr(k),cidvri, iopsou(4,k)
              goto 9999
            endif
          endif
     
        endif
     
c
c ---------------------------------------------------------
c rrb 2007/08/04;
c               d2. Get the CU factors used to calculate the 
c                    standard component to a T&C obligation
c		   Note istop=0 Stop if not found
c                       itype=26 read 12 efficiency values
       if(NAs2.eq.0 .and. iopsou(4,k).gt.0) then
         istop=0
         itype=26
         call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1        iops1 ,iops2, nx, creuse, iacc, istop, rops2,
     1        ioprsw(k), cidvri)     
       endif    
c
c ---------------------------------------------------------
c               e. If a carrier then reset the destination location
        nc=intern(k,1)
        if(nc.gt.0) idcdX=idvsta(nc)
c
c ---------------------------------------------------------
c               f. Find the Exchange point and store in iExPoint
c		   Note idcdX is the destination diversion, reservoir,
c		   plan or carrier
        call oprExp(nlog, maxsta, idcdX, idcdS, idncod, ndnnod, 
     1                    iExPoint(k),cidvri)
c
c ------------------------------------------------------------------
c               g. Find destination reuse plan named Creuse, if any, 
c		               and store in ireuse(k)
c
c		                istop=0  Stop if not found
c                   ion=1 means turn off opr right if right is off
c                   itype=24 is an operating rule
c                   Nause=0 means creuse has data (it is not 'NA')

        iacc=0
        ion=-1
c
c rrb 2015/02/03X; When the source is an admin plan, revise to allow
c                  this read to fail (creuse is not a plan
c                  so that creuse can be a source operating rule 
c rrb 2015/03/07; Back to Oprlimit = 1-9 logic
cx      istop=0    
cx      istop=1
        istop=0
        itype=7
        ireuse1=0        
cr      if(creuse(1:3).ne.'N/A') then
        if(NAuse.eq.0) then        
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       ireuse1,iops2, nx, creuse, iacc, istop, rops2,
     1        ioprsw(k), cidvri)     
        endif
c
c ---------------------------------------------------------
c		h. Check proper type of plan for the reuse
c                  Note iplntyp 3 & 5 are reservoir, 
c                       iplntyp 4 & 6 are diversion
        if(ireuse1.gt.0) then
          ireuse(k)=ireuse1        
          iok=1
c
c		              Diversion destination (3)
          if(iopdesr(k).eq.3) then
            if(iplntyp(ireuse1).eq.4 .or. iplntyp(ireuse1).eq.6) iok=0
          endif  
c
c		              Reservoir destination (2)          
          if(iopdesr(k).eq.2) then
            if(iplntyp(ireuse1).eq.3 .or. iplntyp(ireuse1).eq.5) iok=0
          endif

          if(iok.eq.1) then
            write(nlog,1255) ityopr(k),cidvri, ciopde, creuse,
     1         iplntyp(ireuse1)
            goto 9999
          endif  
        endif
c
c ------------------------------------------------------------------
c rrb 2015/03/07; Revise to use Oprlimit = 5 and 7-9
cxc rrb 2015/02/03X; Read source water right from variable Creuse
cxc               h2. If the source is an admin plan, find the 
cxc                   a source operating rule named Creuse, if any, 
cxc		                and store in ireuse(k)
cxc
cxc		                istop=0  Stop if not found
cxc                   ion=1 means turn off opr right if right is off
cxc                   itype=24 is an operating rule
cxc                   Nause=0 means creuse has data (it is not 'NA')
cx        if(iopsouR(k).eq.7 .and. iopsouP.eq.11) then
cx          iacc=0
cx          ion=0
cx          istop=0
cx          ireuse1=0        
cx          if(NAuse.eq.0) then    
cx           istop=0
cx           itype=14                   
cx            call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
cx     1           ireuse1,iops2, nx, creuse, iacc, istop, rops2,
cx     1           ioprsw(k), cidvri)  
cx            ireuse(k)=ireuse1    
cx          else
cx            write(nlog,937)  corid(k), ityopr(k), ciopso1, cReuse
cx            goto 9999       
cx          endif
cxc      
cxc rrb 2015/02/03X; Check the source is a type 26 plan
cx          if(ireuse1.gt.0) then
cx            lr5=ireuse(k)          
cx            if(ityopr(lr5).ne.26) then
cx              write(nlog,937)  corid(k), ityopr(k), ciopso1, cReuse
cx              goto 9999
cx            endif    
cx          endif                  
cx        endif                 
cxc
c ---------------------------------------------------------
c               i. Set the release type demand or depletion
c		   Note a destination reservoir is always a diversion
        rec12=cdivtyp(k)
        iok=1
        if(iopdes(1,k).gt.0) then
          if(rec12(1:9).eq.'Diversion') iok=0
          if(rec12(1:9).eq.'Depletion') iok=0
        else
          if(rec12(1:9).eq.'Diversion') iok=0
        endif  
        if(iok.eq.1) then 
          write(nlog,1258) ityopr(k), cidvri, cdivtyp(k)
          goto 9999
        endif      
c        
c ---------------------------------------------------------
c		          j. Check the return type matches the data provided
       if(NAs2.eq.0 .and. iopsou(4,k).gt.0) then
         ipTC=iopsou(3,k)
         call chkPrf(nlog, ipTC, iprf(ipTC), ityopr(k), iopsou(4,k), 
     1     maxPlan, maxRtnPP, nrtnPP, pcttotPP, iprob,ioprsw(k), cidvri)           
         if(iprob.eq.1) then
           write(nlog,1262)  ityopr(k),cidvri, iopsou(4,k)
           goto 9999
         endif
       endif   
c
c ---------------------------------------------------------
c rrb 2015/03/07; 
c             k. Check the source water right and Ioprlim are set
c                properly when the source is a Changed Water Right 
c                Plan (type 13)          
       if(iopsouR(k).eq.7 .and. iopsouP.eq.13) then
         iok=1
         if(ioprlim(k).eq.5 .or. ioprlim(k).eq. 7 .or.
     1     ioprlim(k).eq.8 .or. ioprlim(k).eq. 9) iok=0
     
         if(iok.eq.1) then
             write(nlog,937)  corid(k), ityopr(k), ciopso1, iopsouP,
     1                       ioprlim(k)
             goto 9999
         endif
       endif  
c  
       if(ioprlim(k).eq.5 .or. ioprlim(k).eq. 7 .or.
     1   ioprlim(k).eq.8 .or. ioprlim(k).eq. 9) then
         iok=1
         if(iopsouR(k).eq.7 .and. iopsouP.eq.13) iok=0
         
         if(iok.eq.1) then
           write(nlog,937)  corid(k), ityopr(k), ciopso1, iopsouP,
     1                     ioprlim(k)
           goto 9999
         endif
       endif 
        
c
c ---------------------------------------------------------
c		          l. Detailed output
c     
        iout28=0
        if(iout28.eq.1) then
          call OprinOut(nlog, maxopr, k, 
     1      ityopr(k), cidvri, 
     1      ciopde, iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k),iopsou(2,k),
     1      ciopso2, iopsou(3,k),iopsou(4,k), creuse, ireuse1,
     1      oprloss(k), oprlimit(k), iopSou(5,k), 
     1      iopSou(6,k),iopsou(7,k),     
     1      cdivtyp(k), intern, cntern, cAssoc, cAssoc2, cAssoc3)
        endif  
c

        goto 1190
c _________________________________________________________
c
c         
 1029   continue 
c               Type 29; Plan or Reservoir and Plan Spill
c rrb 01/06/20; 
c                source 1 (iopsou(3,k) = a plan ID
c                  ion=1 means turn off opr right if right is off
cr      write(nlog,*) '  Oprinp; Type 29'
        ion=1
        istop=0
        nr=0
        np1=0
        np2=0
        
        idumc=ifix(dumc)
c
c ---------------------------------------------------------
c               a1. Read monthly constraints 
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,ix, ix,
     1               nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c rrb 2007/07/03; 
c		            a2. Read the operating rule associated with
c		               a monthly or annual plan limit adjustment
c		               when Oprlimit(k) > 0
        if(ioprlim(k).gt.0) then
          istop=0
          itype=24
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1      iopsou(5,k),iopsou(6,k), nx, cAssoc, 1, 
     1      istop, rops2,ioprsw(k), cidvri)
          iopsou(5,k)=nx 
        endif
c rrb ***************
c
c rrb 2014-04-26; Allow the destination to be specified
c rrb 2014/12/14; Revise logic to require the destination
c                 be specified when the source is an admin plan
c   
c ---------------------------------------------------------
c               b1. Find the destination, a river ID (itype 0)
c		                Note: istop=1, do not stop if not found
c                                 to allow a -1 reset to be specified
c                         itype=0 is a stream Id
        istop=1
        itype=0
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1, iopdes(2,k), nx,ciopde, itype, istop, rops2,
     1       ioprsw(k), cidvri)
   
cx      write(nlog,*) '  Oprinp; iops1, ciopde', iops1, ciopde  
        if(iops1.gt.0) then
          iopdes(1,k)=iops1
          idcdD=idvsta(iops1)
          iopdesr(k)=itype
        endif   
c   
c ---------------------------------------------------------
c rrb 2019/05/26; Find destination = -1 (no physical spill)
c               b2. Set iopdes(1,k) = -1 when the destination 
c                   is -1 (reset)
        rec2=ciopde(1:2) 
        if(rec2 .eq. '-1') then
          iopdes(1,k) = -1
          idcdD=0
          iopdesr(k) = -1
cx          write(nlog,*) '  Oprinp; ',
cx     1      'nx, rec2, iopdes(1,k)', nx, rec2, iopdes(1,k)
        endif   
c   
c ---------------------------------------------------------
c rrb 2021/02/14; Find destination = NA (destination is the plan or
c                 reservoir location)
c               b3. Set iopdes(1,k) when the destination is NA
        rec2=ciopde(1:2) 
        if(rec2 .eq. 'NA') then
          iopdes(1,k) = 0
          idcdD=0
          iopdesr(k) = 0
cx          write(nlog,*) '  Oprinp; ',
cx     1      'nx, rec2, iopdes(1,k)', nx, rec2, iopdes(1,k)
        endif   
c
c ---------------------------------------------------------
c               c1. Find source 1 a reservoir if any (type 2)
c		                Note istop=1, do not stop if not found
c                     to allow the source to be a plan
        istop=1
        itype=2
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iopsou(1,k),iopsou(2,k), nx, ciopso1, 1, 
     1       istop, rops2,ioprsw(k), cidvri)

        iopSouR(k)=itype
        nr=iopsou(1,k)
        
c       write(nlog,*) ' Oprinp 1; type 29 nr, np1, np2', nr, np1, np2
c
c ---------------------------------------------------------
c              c2. Find source 1 a plan (type 7)
c		               Note istop=1, do not stop if not found
        istop=1
        itype=7
        iacc=1
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       np1,iopsou(2,k), nx, ciopso1, iacc, istop, rops2,
     1       ioprsw(k), cidvri)

        if(np1.gt.0) then        
          iopSouR(k)=itype
c
c rrb 2021/02/14; Cleanup        
cx        iopsou(1,k)=-1*np1
          iopsou(1,k)=np1
c
c		Check for proper plan type (e.g. NOT 1, 2, 9, or 10)
          if(iplntyp(np1).eq.1  .or. iplntyp(np1).eq.2 .or.
     1       iplntyp(np1).eq.10 .or. iplntyp(np1).eq.9) then
              write(nlog,1267)  ityopr(k),cidvri, ciopso1, iplntyp(np1)
              goto 9999
          endif 
        endif  
c
c ---------------------------------------------------------
c               d. Exit if a source 1 is not found
        if(nr.eq.0 .and. np1.eq.0) then
          write(nlog,936) corid(k), ityopr(k), ciopso1                  
          goto 9999
        endif
c       write(nlog,*) '  Oprinp 3; type 29 nr, np1, np2', nr, np1, np2
        
c
c ---------------------------------------------------------
c               e. Find source 2 a plan (type 7)
c		               Note istop=1, do not stop if not found
c                  NAs2 = 1 when iopsou(2, ) = NA, 0, or blank
c rrb 2008/08/06; Simplify logic and warnings
        if(NAs2.eq.0) then
          istop=1
          itype=7
          iacc=1
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         iopsou(3,k),iopsou(4,k), nx, ciopso2, iacc, 
     1         istop, rops2,ioprsw(k), cidvri)

          iopSouR(k)=itype
          np2=iopsou(3,k)
          
c       
c	  	Check for proper plan type (not a 1, 2, 9 or 10
          if(iplntyp(np2).eq.1  .or. iplntyp(np2).eq.2 .or.
     1       iplntyp(np2).eq.10 .or. iplntyp(np2).eq.9) then         
              write(nlog,1267)  ityopr(k),cidvri, ciopso2, iplntyp(np2)
              goto 9999
          endif    
c         write(nlog,*) '  Oprinp 4; type 29 nr, np1, np2', nr, np1, np2
        endif  
c
c ---------------------------------------------------------
c
c		            f. Check if a source 1 and source 2 are a plan
        if(np1.gt.0 .and. np2.gt.0) then
          write(nlog,938) corid(k), ityopr(k), ciopso1, ciopso2          
          goto 9999
        endif
c       write(nlog,*) '  Oprinp 5; type 29 nr, np1, np2', nr, np1, np2
        
c
c ---------------------------------------------------------
c
c		            g. Check if a source 1 is a reservoir plan (3 or 5) 
c                  that is not tied to a reservoir
        if(np1.gt.0) then
          if(iplntyp(np1).eq. 3 .or. iplntyp(np1).eq.5) then
            write(nlog,934) corid(k), ityopr(k), ciopso1          
            goto 9999
          endif
        endif
c
c ---------------------------------------------------------
c rrb 2021/02/14
c		            h. See Check C16 nr bottom for 
c                  Destination checks printed to *.chk
c
c ---------------------------------------------------------
c		            j. Detailed output
     
        iout29=0
        if(iout29.eq.1) then
          call OprinOut(nlog, maxopr, k, 
     1      ityopr(k), cidvri, 
     1      ciopde, iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k),iopsou(2,k),
     1      ciopso2, iopsou(3,k),iopsou(4,k), creuse, ireuse1,
     1      oprloss(k), oprlimit(k), iopSou(5,k), 
     1      iopSou(6,k),iopsou(7,k),     
     1      cdivtyp(k), intern, cntern, cAssoc, cAssoc2, cAssoc3)
        endif  
c         write(nlog,*) '  Oprinp 6; type 29 nr, np1, np2', 
c     1                 nr, np1, np2
c
c               k. Exit the type 29 block
        goto 1190
c
c _________________________________________________________
c               Type 30; Re Store T&C release
 1030   continue 
c rrb 05/02/01; 
        ion=1
        istop=0
        
        idumc=ifix(dumc)
c
c ---------------------------------------------------------
c               a. Read monthly constraints 
        istop=0
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,ix, ix,
     1               nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               a2. Read intervening structures
        istop=0
        call oprFind(ityopr(k), 21, idumc,k,ion,iprinto,ix, ix,
     1               nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
     
c
c ---------------------------------------------------------
c               b. Find the destination, a reservoir ID (type = 2)
c		   Note stored as a negative value
        call oprFind(ityopr(k), 2, idumc,k,ion,iprinto,
     1       iopdes(1,k), iopdes(2,k), nx,ciopde, 0, 
     1       istop, rops2,ioprsw(k), cidvri)
        iopdesr(k)=2
     
c
c ---------------------------------------------------------
c               c. Find source 1 a simple operating right (type = 14)
        itype=14
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iopsou(1,k),iopsou(2,k),nop,ciopso1, 1, 
     1       istop, rops2,ioprsw(k), cidvri)

        iopSouR(k)=itype
     
c
c ---------------------------------------------------------
c               d. Find source 2 a T&C plan (type 7)
        call oprFind(ityopr(k), 7, idumc,k,ion,iprinto,
     1       iopsou(3,k),iopsou(4,k),nx, ciopso2, 1, 
     1       istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c		e. Check the specified opr right points to:
c                  (1) the same reservoir,
c                  (2) the same reservoir account, and
c                  (3) the same destination
        if(nop.gt.0) then
          iprob=0
          if(iopdes(1,k).ne.iopsou(1,nop)) iprob=1
          if(iopdes(2,k).ne.iopsou(2,nop)) iprob=2
          nop2=-1*iopdes(1,nop)                   
          if(iopsou(3,k).ne.nop2) iprob=3
          
          if(iprob.gt.0) then          
            write(nlog,*) '  Oprinp; iprob, nop',iprob, nop,
     1       iopsou(3,k), iopsou(3,nop)
            write(nlog,594) cidvri,
     1      cresid(iopdes(1,k)),   iopdes(2,k), pid(iopsou(3,k)),
     1      cresid(iopsou(1,nop)), iopsou(2,nop), pid(nop2), 
     1      ciopso1
            
  594 format(/, 72('_'), /'  Oprinp; ',
     1     'Problem with Operation Right:                         ',
     1 a12,/                   
     1 10x,'The destination reservoir, account, and plan:         ',
     1 a12, 1x,i3, 1x,a12,/
     1 10x,'Must match the source reservoir, account and plan:    ',
     1 a12, 1x,i3, 1x,a12,/
     1 10x,'Associated with this oper rule source (an oper rule): ',
     1 a12)
            
            goto 9999          
          endif  
        endif  
c
c ---------------------------------------------------------
c		l. Detailed output
c     
        iout30=0
        if(iout30.eq.1) then
          call OprinOut(nlog, maxopr, k, 
     1      ityopr(k), cidvri, 
     1      ciopde, iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k),iopsou(2,k),
     1      ciopso2, iopsou(3,k),iopsou(4,k), creuse, ireuse1,
     1      oprloss(k), oprlimit(k), iopSou(5,k), 
     1      iopSou(6,k),iopsou(7,k),     
     1      cdivtyp(k), intern, cntern, cAssoc, cAssoc2, cAssoc3)
        endif  
c        
        goto 1190
c _________________________________________________________
c
c         
 1031   continue 
c               Type 31; Carrier with Reuse
c rrb 01/06/20; 
c                destination = a diversion
c                source 1 (iopsou(1,k) = a water right
c                source 2 (iopsou(3,k) = N/A
c                reuse1 = Transmountain reuse plan
c                  ion=1 means turn off opr right if right is off
        ion=1
        idumc=ifix(dumc)
c
c ---------------------------------------------------------
c               a1. Read monthly constraints 
        istop=0
        cx='NA'
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,ix, ix,
     1               nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               a2. Read intervening structures
        istop=0
        call oprFind(ityopr(k), 21, idumc,k,ion,iprinto,ix, ix,
     1               nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               b1. Find the destination, a diversion ID (type 3)
c		   set istop=1 (OK if not found)
        istop=1
        call oprFind(ityopr(k), 3, idumc,k,ion,iprinto,
     1       iops1, iopdes(2,k), nx,ciopde, 0, istop, rops2,
     1       ioprsw(k), cidvri)
     
        if(iops1.gt.0) then
          iopdes(1,k)=iops1
          idcdD=idvsta(iops1)
          iopdesr(k)=3
        endif  
c        
c ---------------------------------------------------------
c               b2. Find destination reservoir (type 2)
c                       istop=0 Stop if not found
        if(iops1.eq.0) then
          istop=0
          call oprFind(ityopr(k), 2, idumc,k,ion,iprinto, 
     1         iops1, iopdes(2,k), nx,ciopde, 1, istop, rops2,
     1         ioprsw(k), cidvri)
          iopdes(1,k)=-iops1
          idcdD=irssta(iops1)
          iopdesr(k)=2
        endif
c       write(nlog,*) '  Oprinp; Type 31 iopdes(1,k) = ', iopdes(1,k)
     
c
c ---------------------------------------------------------
c               c. Find source 1 a diversion water right (type 13)
        iacc=0
        itype=13
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iopsou(1,k),iopsou(2,k), nx, ciopso1, iacc, 
     1       istop, rops2,ioprsw(k), cidvri)

        iopSouR(k)=itype     
     
c
c ---------------------------------------------------------
c               d. If a carrier then reset the destination location
        nc=intern(k,1)
        if(nc.gt.0) idcdD=idvsta(nc)
c
c ---------------------------------------------------------
c               e. Find reuse plan named Creuse, if any, and
c		   Store in ireuse(k)
        iacc=0
        ion=-1
        istop=0
        ireuse1=0
        
cr      if(creuse(1:3).ne.'N/A') then
        if(NAuse.eq.0) then        
          call oprFind(ityopr(k), 7, idumc,k,ion,iprinto,
     1         ireuse1,iops2, nx, creuse, iacc, istop, rops2,
     1         ioprsw(k), cidvri)     
        endif
     
c
c ---------------------------------------------------------
c		f. Check proper type of plan 
c                  Note iplntyp 3 & 5 are reservoir, 
c                       iplntyp 4 & 6 are diversion
        if (ireuse1.gt.0) then
          ireuse(k)=ireuse1        
          iok=1
c
c		Diversion destination (3)		          
          if(iopdesr(k).eq.3) then
            if(iplntyp(ireuse1).eq.4 .or. iplntyp(ireuse1).eq.6) iok=0
          endif  
c
c		Reservoir destination  (2)        
          if(iopdesr(k).eq.2) then
            if(iplntyp(ireuse1).eq.3 .or. iplntyp(ireuse1).eq.5) iok=0
          endif

          if(iok.eq.1) then
            write(nlog,1255) ityopr(k),cidvri, ciopde, creuse,
     1         iplntyp(ireuse1)
            goto 9999
          endif  
        endif        
c
c ---------------------------------------------------------
c		e. Detailed output
     
        iout31=0
        if(iout31.eq.1) then
          write(nlog,2020) ityopr(k), cidvri, ityopr(k), 
     1      ciopde,  iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k), iopsou(2,k),
     1      ciopso2, iopsou(3,k), iopsou(4,k), creuse, ireuse1,
     1      oprlimit(k), iopSou(5,k),
     1      cdivtyp(k), iopdesR(k)
        endif  

        goto 1190



c _________________________________________________________
c
c         
 1032   continue 
c               Type 32; Reservoir plus a Reservoir plan 
c                 to Diversion or Reservoir or Instream Flow
c                 with or without a Carrier,with or without
c                 Reuse
c                destination = a diversion or a reservoir or instream 
c                   flow
c                source 1 (iopsou(1,k) = a reservoir
c                source 2 (iopsou(3,k) = a reuse plan
c                  ion=1 means turn off opr right if right is off
        ion=1
        idumc=ifix(dumc)
        ireuseS=0
c
c ---------------------------------------------------------
c               a1. Read monthly constraints 
        istop=0
        cx='NA'
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,ix, ix,
     1               nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               a2. Read intervening structures
c rrb 2015/10/28; Revise to read intervening structures plus loss
cx      itype = 21
        itype = 23
        istop=0
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,ix, ix,
     1               nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               b1. Find the destination, a diversion ID (type 3)
c		                istop=1 (OK if not found)
        istop=1
        itype=3
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1, iopdes(2,k), nx,ciopde, 0, istop, rops2,
     1       ioprsw(k), cidvri)
     
        if(iops1.gt.0) then
          iopdes(1,k)=iops1
          idcdD=idvsta(iops1)
          iopdesr(k)=3
        endif  
c        
c ---------------------------------------------------------
c               b2. Find destination reservoir (type 2)
c                   istop=1 (OK if not found)
c                   itype=3 Diversion
        if(iops1.eq.0) then
c
c rrb 2011/11/27; Add an instream flow destination
cx        istop=0
          istop=1
          itype=2
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         iops1, iopdes(2,k), nx,ciopde, 1, istop, rops2,
     1         ioprsw(k), cidvri)
c
c rrb 2011/11/27; Upgrade to rely on iopdesr(k)
cx        iopdes(1,k)=-iops1
          iopdes(1,k)=iops1
          idcdD=irssta(iops1)
          iopdesr(k)=2
c         write(nlog,*) '  Oprinp; Type 28 iopdes(1,k) = ', iopdes(1,k)
        endif
c        
c ---------------------------------------------------------
c rrb 2011/11/27; Allow the destination to be an ISF structure
c               b3. Find the destination instream flow (type 1)
c                   istop=0 (Stop if not found)
c			              itype=1 ISF structure
        if(iops1.eq.0) then
          istop=0
          itype=1
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         iops1, iopdes(2,k), nx,ciopde, 1, istop, rops2,
     1         ioprsw(k), cidvri)
          iopdes(1,k)=iops1
          idcdD=ifrsta(iops1)
          idcdX=idcdD
          iopdesr(k)=1
c         write(nlog,*) ' Oprinp;  iopdes(1,k) = ', iopdes(1,k)
        endif


c
c ---------------------------------------------------------
c               c. If a carrier then reset the destination location
        nc=intern(k,1)
        if(nc.gt.0) idcdD=idvsta(nc)
c
c ---------------------------------------------------------
c               d. Find source 1 a reservoir (type 2)
c                       istop=0 (Stop if not found)
        istop=0
        itype=2
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1,iopsou(2,k), nx, ciopso1, 1, 
     1       istop, rops2,ioprsw(k), cidvri)
        iopsou(1,k) =iops1    
        iscdS=irssta(iops1)     
        iopSouR(k)=itype
             
c
c ---------------------------------------------------------
c               e. Find source 2 a reuse plan (type 7)
c                       istop=0 (Stop if not found)
c
        istop=0
c
c rrb 2011/11/27; For a Type 32 Warn if source 2 is not provided
        if(NAs2.eq.1) then
          write(nlog,1282) ityopr(k),cidvri, ciopso2        
          goto 9999
        endif
        
        if(NAs2.eq.0) then                
          call oprFind(ityopr(k), 7, idumc,k,ion,iprinto,
     1         iopsou(3,k),iopsou(4,k), nx, ciopso2, 1, 
     1         istop, rops2,ioprsw(k), cidvri)
          ireuseS=iopsou(3,k)
c          
c ---------------------------------------------------------
c		f. Check source 2 is a Reservoir Plan (type 3 or 5)           
c rrb 2006/04/25; 
          iok=1
          if(ireuseS.gt.0) then
            if(iplntyp(ireuseS).eq.3 .or. iplntyp(ireuseS).eq.5) iok=0
            if(iok.eq.1) then
              write(nlog,1256) ityopr(k),cidvri, ciopso2, 
     1          iplntyp(ireuseS)
              goto 9999
            endif  
          endif
     
        endif
c
c ---------------------------------------------------------
c               g. Find destination reuse plan named Creuse, if any, and
c		   Store in ireuse(k)
c                       istop=0 (Stop if not found)
        iacc=0
        ion=-1
        istop=0
        ireuse1=0
cr      if(creuse(1:3).ne.'N/A') then
        if(NAuse.eq.0) then        
          call oprFind(ityopr(k), 7, idumc,k,ion,iprinto,
     1       ireuse1,iops2, nx, creuse, iacc, istop, rops2,
     1       ioprsw(k), cidvri)     
        endif
c
c ---------------------------------------------------------
c		h. Check proper type of Reuse plan 
c                  Note iplntyp 3 & 5 are reservoir, 
c                       iplntyp 4 & 6 are diversion
        if (ireuse1.gt.0) then
          ireuse(k)=ireuse1        
          iok=1          
c
c		Diversion destination (3)          
          if(iopdesr(k).eq.3) then
            if(iplntyp(ireuse1).eq.4 .or. iplntyp(ireuse1).eq.6) iok=0
          endif
c
c		Reservoir destination (2)          
          if(iopdesr(k).eq.2) then
            if(iplntyp(ireuse1).eq.3 .or. iplntyp(ireuse1).eq.5) iok=0
          endif
            
          if(iok.eq.1) then
            write(nlog,1255) ityopr(k),cidvri, ciopde, creuse, 
     1        iplntyp(ireuse1)
            goto 9999
          endif  
        endif   
        
c
c ---------------------------------------------------------
c               i. Check the release type demand or depletion
c		   Note a destination reservoir or ISF is always a diversion
        rec12=cdivtyp(k)
        iok=1
c
c
c ---------------------------------------------------------
c rrb 2020/09/06; Update to use iopdesr() To Stream and To Conduit          
cx       if(iopdesr(k).eq.3) then
cx          if(rec12(1:9).eq.'Diversion') iok=0
cx        else
cx          if(rec12(1:9).eq.'Diversion') iok=0
cx        endif  
cx        if(rec12(1:6).eq.'Direct') iok=0 
cx          
        if(rec12(1:9).eq.'Diversion') rec12(1:12) = 'To_Stream   '    
        if(rec12(1:6).eq.'Direct')    rec12(1:12) = 'To_Conduit  '
c
        if(rec12(1:12) .eq. 'To_Stream   ') iok=0
        if(rec12(1:12) .eq. 'To_Conduit  ') iok=0
                
        if(iok.eq.1) then 
          write(nlog,1258) ityopr(k), cidvri, cdivtyp(k)
          goto 9999
        endif          
c
c ---------------------------------------------------------
c               h. Check that the destination located at idcdD
c                        is downstream of a structure loacted at iscdS
c                        with ndns2 downstream nodes
        ndns2=ndnnod(iscdS)
        csource=cstaid(iscdS) 
        cdest=cstaid(idcdD) 
        
        call oprdown(nlog, maxsta, ndns2, iscdS, idcdD, idncod,
     1    cidvri, csource, cdest)
          
c
c ---------------------------------------------------------
c		e. Detailed output
     
        iout32=0
        if(iout32.eq.1) then
          write(nlog,2020) ityopr(k), cidvri, ityopr(k), 
     1      ciopde,  iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k), iopsou(2,k),
     1      ciopso2, iopsou(3,k), iopsou(4,k), creuse, ireuse1,
     1      oprlimit(k), iopSou(5,k),
     1      cdivtyp(k), iopdesR(k)
        endif  

        goto 1190
c _________________________________________________________
c
c         
 1033   continue 
c               Type 33; Res and Plan to a Div or Res or Carrier
c                        with reuse by Exchange
c rrb 01/06/20; 
c                destination = a diversion
c                source 1 (iopsou(1,k) = a plan
c                source 2 (iopsou(3,k) = NA
c                  ion=1 means turn off opr right if right is off
        ion=1
        idumc=ifix(dumc)
        ireuseS=0
c
c ---------------------------------------------------------
c               a1. Read monthly constraints 
        istop=0
        cx='NA'
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,ix, ix,
     1               nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               a2. Read intervening structures
        istop=0
        call oprFind(ityopr(k), 21, idumc,k,ion,iprinto,ix, ix,
     1               nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               b1. Find the destination, a diversion ID (type 3)
c		   set istop=1 (OK if not found)
        istop=1
        itype=3
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1, iopdes(2,k),nx,ciopde, 0, istop, rops2,
     1       ioprsw(k), cidvri)
     
        if(iops1.gt.0) then
          iopdes(1,k)=iops1
          idcdD=idvsta(iops1)
          iopdesr(k)=3
        endif  
c
c ---------------------------------------------------------
c        
c               b2. Find the destination a reservoir (type 2)
c                   istop=1 OK if not found)
        if(iops1.eq.0) then
          istop=1
          itype=2
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         iops1, iopdes(2,k), nx,ciopde, 1, istop, rops2,
     1         ioprsw(k), cidvri)
c
c rrb 2011/11/27; Revise to rely on iopdesr(k)
cx        iopdes(1,k)=-iops1
          iopdes(1,k)=iops1
          idcdD=irssta(iops1)
          iopdesr(k)=2
c         write(nlog,*) '  Oprinp; Type 28 iopdes(1,k) = ', iopdes(1,k)
        endif
c        
c ---------------------------------------------------------
c rrb 2011/11/27; Find the destination an ISF structure
c               b3. Find the destination instream flow (type 1)
c                   istop=0 (Stop if not found)
c			              istop=1 (Stop if not found)
c			              iacc=1 (check account number)
c			              itype=1 ISF structure
        if(iops1.eq.0) then
          istop=0
          itype=1
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         iops1, iopdes(2,k), nx,ciopde, 1, istop, rops2,
     1         ioprsw(k), cidvri)
          iopdes(1,k)=iops1
          idcdD=ifrsta(iops1)
          idcdX=idcdD
          iopdesr(k)=1
c         write(nlog,*) ' Oprinp;  iopdes(1,k) = ', iopdes(1,k)
        endif


c        
c ---------------------------------------------------------
c rrb 2008/03/21; Revised warning when several destinations are possible        
        if(iops1.eq.0) then
          write(nlog,1300) cidvri, ityopr(k), ciopde
          goto 9999
        endif   
        
c
c ---------------------------------------------------------
c               c. If a carrier then reset the destination location
        nc=intern(k,1)
        if(nc.gt.0) idcdD=idvsta(nc)
c
c ---------------------------------------------------------
c               d. Find source 1 a reservoir (type 2)
c                       istop=0 (Stop if not found)
        istop=0
        itype=2
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iopsou(1,k),iopsou(2,k), nx, ciopso1, 1, 
     1       istop, rops2,ioprsw(k), cidvri)

        iopSouR(k)=itype
     
c
c ---------------------------------------------------------
c               e. Find source 2 a reuse plan (type 7)
c                       istop=0 (Stop if not found)
c
c
c rrb 2011/11/27; For a Type 32 Warn if source 2 is not provided
        if(NAs2.eq.1) then
          write(nlog,1282) ityopr(k),cidvri, ciopso2        
          goto 9999
        endif
c
        istop=0
        if(NAs2.eq.0) then        
          call oprFind(ityopr(k), 7, idumc,k,ion,iprinto,
     1      iopsou(3,k),iopsou(4,k),nx, ciopso2, 1, 
     1      istop, rops2,ioprsw(k), cidvri)
        ireuseS=iopsou(3,k)
     
c          
c ---------------------------------------------------------
c		f. Check source 2 is a Reservoir Plan (type 3 or 5)
c rrb 2006/04/25; 
          iok=1
          if(ireuseS.gt.0) then
            if(iplntyp(ireuseS).eq.3 .or. iplntyp(ireuseS).eq.5) iok=0
            if(iok.eq.1) then
              write(nlog,1256) ityopr(k),cidvri, ciopso2, 
     1          iplntyp(ireuseS)
              goto 9999
            endif  
          endif
     
        endif
        ireuseS=iopsou(3,k)
c
c ---------------------------------------------------------
c               f. Find the Exchange point from reservoir and store 
c                  in iopsou(5
        iscdS=irssta(iopsou(1,k))        
        call oprExp(nlog, maxsta, idcdD, iscdS, idncod, ndnnod, 
     1                    iExPoint(k), cidvri)
c
c ------------------------------------------------------------------
c               g. Find destination reuse plan named Creuse, if any, 
c		               and store in ireuse(k)
        iacc=0
        ion=-1
        istop=0
        ireuse1=0        
    
cr      if(creuse(1:3).ne.'N/A') then
        if(NAuse.eq.0) then        
          call oprFind(ityopr(k), 7, idumc,k,ion,iprinto,
     1       ireuse1,iops2, nx, creuse, iacc, istop, rops2, 
     1       ioprsw(k), cidvri)        
       
        endif
c
c ---------------------------------------------------------
c		            h. Check proper type of plan 
c                  Note iplntyp 3 & 5 are reservoir, 
c                       iplntyp 4 & 6 are diversion
        if (ireuse1.gt.0) then
          ireuse(k)=ireuse1        
          iok=1
c
c		Diversion destination (3)          
          if(iopdesr(k).eq.3) then
            if(iplntyp(ireuse1).eq.4 .or. iplntyp(ireuse1).eq.6) iok=0
          endif  
c
c		Reservoir destination (2)          
          if(iopdesr(k).eq.2) then
            if(iplntyp(ireuse1).eq.3 .or. iplntyp(ireuse1).eq.5) iok=0
          endif

          if(iok.eq.1) then
            write(nlog,1255) ityopr(k),cidvri, ciopde, creuse,
     1         iplntyp(ireuse1)
            goto 9999
          endif  
        endif        
c
c ---------------------------------------------------------
c               i. Check the release type demand or depletion
c		   Note a destination reservoir is always a diversion
        rec12=cdivtyp(k)
        iok=1
c
c rrb 2011/11/27; Update to use iopdesr()
cx      if(iopdes(1,k).gt.0) then
        if(iopdesr(k).eq.3) then
          if(rec12(1:9).eq.'Diversion') iok=0
          if(rec12(1:9).eq.'Depletion') iok=0
        else
          if(rec12(1:9).eq.'Diversion') iok=0
        endif  
        if(iok.eq.1) then 
          write(nlog,1258) ityopr(k), cidvri, cdivtyp(k)
          goto 9999
        endif            
c
c ---------------------------------------------------------
c		e. Detailed output
     
        iout33=0
        if(iout33.eq.1) then
          write(nlog,2020) ityopr(k), cidvri, ityopr(k), 
     1      ciopde,  iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k), iopsou(2,k),
     1      ciopso2, iopsou(3,k), iopsou(4,k), creuse, ireuse1,
     1      oprlimit(k), iopSou(5,k),
     1      cdivtyp(k), iopdesR(k)
        endif  

        goto 1190
        
c                                                                       
c _________________________________________________________
c
 1034   continue 
c               Type 34; Reservoir to Reservoir with Reuse
c
c                  ion=1 means turn off opr right if right is off
        ion=1
        istop=0
        idumc=ifix(dumc)
c
c ---------------------------------------------------------
c               a. Read monthly constraints 
        istop=0
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               a2. Read intervening structures w/o loss
        istop=0
        call oprFind(ityopr(k), 21, idumc,k,ion,iprinto,ix, ix,
     1               nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               a3. Read intervening structures plus loss (23)
        ioprloss=int(oprloss(k))
        if(ioprloss.ne.0) then
          istop=0
          itype=23
          call Oprfind(ityopr(k), itype, idumc,k,ion,iprinto,
     1     ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
        endif
c
c ---------------------------------------------------------
c rrb 2007/07/03; 
c		            a4. Read the operating rule associated with
c		                a monthly or annual plan limit adjustment
c		                when Oprlimit(k) > 0
c		                istop=0  Stop if not found
c		                itype=24 Read a operating Rule ID with monthly and annual
c                         plan limits
c
c rrb 2008/04/01; Enhancement
        if(ioprlim(k).eq.2) then
          istop=0
          itype=24          
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1      iopsou(5,k),iopsou(6,k), nx, cAssoc, 1, 
     1      istop, rops2,ioprsw(k), cidvri)
     
          iopsou(5,k)=nx 
c
c		Set ciospoX5 to the plan associated with the 
c               above operating rule          
          ip5=iopsou(1,nx)
          ciopsoX5(k)=pid(ip5)
        endif     
c
c ---------------------------------------------------------
c               c. Find destination reservoir (type 2)
c                  Note itype=2 for a reservoir
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        itype=2
        istop=0
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iopdes(1,k), iopdes(2,k), nx,ciopde, 1, 
     1       istop, rops2,ioprsw(k), cidvri)
c
c rrb 2010/02/05; Correction     
cx      idcdD=irssta(iops1)   
        idcdD=irssta(nx)               
        iopdesr(k)=2
        
c
c               c2. If a carrier then reset the destination location
        nc=intern(k,1)
        if(nc.gt.0) then
          idcdD=idvsta(nc)                  
c         write(nlog,*) '  Oprinp; #3 iopdes(1,k) = ',iopdes(1,k),idcdd
        endif
c
c ---------------------------------------------------------
c               d. Find source 1 a reservoir (type 2)
c		   Note ion=0 leaves the original water right on
c		        iacc=0 allows account to be 0 (since 
c                       it is ownership %)
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found

        ion=0
        iacc=1
        istop=0
        istop=1
        itype=2
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iopsou(1,k),iopsou(2,k), nx, ciopso1, 1, 
     1       istop, rops2,ioprsw(k), cidvri)
     
        iopSouR(k)=itype
     
c
c ---------------------------------------------------------
c               e1. Find diversion that limits (if any)(type 3)
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
c			      ion=-1 do not check or turn anything off
        iops1=0
        iopsou(4,k)=0
        
        istop=1
        itype=3
        ion=-1
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1,iopsou(4,k), nx, ciopso2, 1, istop, rops2,
     1       ioprsw(k), cidvri)
        if(iops1.gt.0) then
          iopsou(3,k)=iops1
          iopsou(4,k)=3
        endif  
c
c ---------------------------------------------------------
c               e2. Find operating rule that limits (if any)(type 14)
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        if(iops1.eq.0) then
          istop=1
          itype=14
          ion=-1
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         iops1,iopsou(4,k), nx, ciopso2, 1, 
     1         istop, rops2,ioprsw(k), cidvri)
          if(iops1.gt.0) then
            iopsou(3,k)=iops1
            iopsou(4,k)=14
          endif  
        endif  
c
c ---------------------------------------------------------
c               e2. Find a plan ID that limits (if any)(type 7)
c                   This works for a OOP plan (type 9), special
c                   well augmentation (type 10) and a WWSP-User plan
c                   (type 15)
c		                Note istop=0 Stop if not found
c		                istop=1 Do not Stop if not found
        if(iops1.eq.0) then
          istop=1
          itype=7
          ion=-1
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         iops1,iopsou(4,k), nx, ciopso2, 1, istop, rops2,
     1         ioprsw(k), cidvri)
          if(iops1.gt.0) then
            iopsou(3,k)=iops1
            iopsou(4,k)=7
            
            iok=1
c
c rrb 2018/10/21; Add WWSP Plan
c rrb 2019/04/20; Revise to recognize a WWSP Supply (type 14) and
c                 WWSP User (type 15)
cx          if(iplntyp(iops1).eq.9 .or. iplntyp(iops1).eq.10) iok=0
cx          if(iplntyp(iops1).eq.9 .or. iplntyp(iops1).eq.10 .or.
cx   1         iplntyp(iops1).eq.14) iok=0
            if(iplntyp(iops1).eq.9 .or. iplntyp(iops1).eq.10 .or.
     1         iplntyp(iops1).eq.15) iok=0
            if(iok.eq.1) then
              write(nlog,1268) ityopr(k),cidvri, ciopso2,iplntyp(iops1)
              goto 9999
            endif              
          endif  
        endif  
c
c ---------------------------------------------------------
c               e. Find reuse plan named Creuse, if any, and
c		   Store in ireuse(k)
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        iacc=0
        ion=-1
        istop=0
        ireuse1=0
cr      if(creuse(1:3).ne.'N/A') then
        if(NAuse.eq.0) then                
          call oprFind(ityopr(k), 7, idumc,k,ion,iprinto,
     1       ireuse1,iops2, nx, creuse, iacc, istop, rops2,
     1       ioprsw(k), cidvri)     
        endif
        
c
c ---------------------------------------------------------
c		i. Check proper type of Reuse plan 
c                  Note iplntyp 3 is reuse to a reservoir and 
c                       iplntyp 5 is reuse to a reservoir transmountain, 
        if (ireuse1.gt.0) then
          ireuse(k)=ireuse1        

          iok=1          
          if(iplntyp(ireuse1).eq.3 .or. iplntyp(ireuse1).eq.5) iok=0
c
c rrb 2006/10/17; Allow destination to be an OOP plan (plan type 9) and
c                 a special plan (plan type 10)         
          if(iplntyp(ireuse1).eq.9 .or. iplntyp(ireuse1).eq.10) iok=0
  
          
          if(iok.eq.1) then
            write(nlog,1255) ityopr(k),cidvri, ciopde, creuse, 
     1       iplntyp(ireuse1)
            goto 9999
          endif  
        endif 
c ______________________________________________________________________
c		e. Detailed output
        iout34=0
        if(iout34.eq.1) then
          write(nlog,2020) ityopr(k), cidvri, ityopr(k), 
     1      ciopde,  iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k), iopsou(2,k),
     1      ciopso2, iopsou(3,k), iopsou(4,k), creuse, ireuse1,
     1      oprlimit(k), iopSou(5,k),
     1      cdivtyp(k), iopdesR(k)
        endif  
        goto 1190
c ______________________________________________________________________
 1035   continue 
c               Type 35; Transmountain import
c                destination = a diversion or a reservoir or carrier
c jhb 2014/08    or an acct plan (plan type 11)
c                source 1 (iopsou(1,k) = a diversion (import)
c                source 2 (iopsou(3,k) = N/A
c                  ion=1 means turn off opr right if right is off
        ion=1
        idumc=ifix(dumc)
c ______________________________________________________________________
c               a1. Read monthly constraints 
        istop=0
        cx='NA'
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,ix, ix,
     1               nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c ______________________________________________________________________
c               a2. Read intervening structures
        istop=0
        call oprFind(ityopr(k), 21, idumc,k,ion,iprinto,ix, ix,
     1               nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c ______________________________________________________________________
c jhb 2014/10/31 change the following logic so that the ONLY
c         allowed destination type is a type 11 plan, but leave as
c         much functionality from before so additional types can be
c         (re)added later without too much work
c       do this by copying the plan type search here first,
c         set istop=0 to exit if a plan destination is not found
c         and jump over the old code...
c ______________________________________________________________________
c       Destination MUST be a plan when reaching here...
c
c		    Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        istop=0
        itype=7
        iacc=1
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1    iops1,iopdes(2,k), nx, ciopde, iacc,
     1    istop, rops2,ioprsw(k), cidvri)
        iopdes(1,k)=iops1
        idcdD=ipsta(iops1)
        iopdesr(k)=7
c ______________________________________________________________________
c       Check proper type
c jhb 2014/08 for now the only plan type that is allowed is a plan type 11
        iok=1
        if(iplntyp(nx).eq.11) iok=0
        if(iok.eq.1) then
          write(nlog,1256) ityopr(k),cidvri, ciopso1, iplntyp(nx)
          goto 9999
        endif
c ______________________________________________________________________
c jhb 2014/10/31 old code start...jump over this
        goto 1111
c ______________________________________________________________________
c               b1. Find the destination, a diversion ID (type 3)
c		   set istop=1 Do not Stop if not found
        istop=1
        call oprFind(ityopr(k), 3, idumc,k,ion,iprinto,
     1       iops1, iopdes(2,k), nx,ciopde, 0, istop, rops2,
     1       ioprsw(k), cidvri)
     
        if(iops1.gt.0) then
          iopdes(1,k)=iops1
          idcdD=idvsta(iops1)
          iopdesr(k)=3
        endif  
c        
c ---------------------------------------------------------
c               b2. Find destination reservoir (type 2)
c                       istop=1 (OK if not found)
        if(iops1.eq.0) then
          istop=1
          call oprFind(ityopr(k), 2, idumc,k,ion,iprinto,
     1         iops1, iopdes(2,k), nx,ciopde, 1, istop, rops2,
     1         ioprsw(k), cidvri)
c         this is a gnu fortran precompiler thing
c         but sometimes the whole if block is evaluated at once
c         causing array bounds runtime errors even though the
c         if statement in theory would avoid it...
c         so pop an empty block in front...
          if(iops1.eq.0) then
c           can not get here, but put a fake block anyway. see above.
            istop=1
          else
            iopdes(1,k)=-iops1
            idcdD=irssta(iops1)
            iopdesr(k)=2
c           write(nlog,*) '  Oprinp; Type 28 iopdes(1,k) = ', iopdes(1,k)
          endif
        endif
c        
c ---------------------------------------------------------
c jhb 2014/08 added another transbasin import destination type,
c             an accounting plan (type 11)
c             note: this should be the default and possibly only
c                   mode for a type 35 rule in the future
c                   according to the state's statemod modeling experts
c                   but leave it as is for backward compatibility,
c                   and just remove the documentation of the other modes
c ---------------------------------------------------------
c               b2.5 Find destination = an acct plan (plan type 11)
c          Note istop=0 Stop if not found
c               istop=1 Do not Stop if not found
c jhb 2014/08 note that itype=7 means find any plan type,
c             not just a type 7 plan!
        if(iops1.eq.0) then
c         destination MUST be a plan when reaching here...
          istop=0
          itype=7
          iacc=1
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1,iopdes(2,k), nx, ciopde, iacc,
     1       istop, rops2,ioprsw(k), cidvri)

          iopdes(1,k)=iops1
          idcdD=ipsta(iops1)
          iopdesr(k)=7
c
c         Check proper type
c jhb 2014/08 for now the only plan type that is allowed is a plan type 11
          iok=1
          if(iplntyp(nx).eq.11) iok=0
          if(iok.eq.1) then
            write(nlog,1256) ityopr(k),cidvri, ciopso1, iplntyp(nx)
            goto 9999
          endif
        endif
c ---------------------------------------------------------
c rrb 2008/03/21; Revised warning when several destinations are possible        
        if(iops1.eq.0) then
          write(nlog,1300) cidvri, ityopr(k), ciopde
          goto 9999
        endif   
c ______________________________________________________________________
c jhb 2014/10/31 old code end...jump over this
 1111   continue
c ______________________________________________________________________
c jhb 2014/08 the following is a problem since intern(k,1) has not been
c             set and is still = 0 (the initialized value)
c             but it doesn't break anything, so leave it alone for now
c ---------------------------------------------------------
c               b3. If a carrier then reset the destination location
        nc=intern(k,1)
        if(nc.gt.0) idcdD=idvsta(nc)
c
c ---------------------------------------------------------
c               c. Find source 1 a transmountain import plan (type 7)
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        istop=0
        itype=7
        iacc=1
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iopsou(1,k),iopsou(2,k), nx, ciopso1, iacc, 
     1       istop, rops2,ioprsw(k), cidvri)
     
        iopSouR(k)=itype
     
c
c		Check proper type
        iok=1          
        if(iplntyp(nx).eq.7) iok=0          
        if(iok.eq.1) then
          write(nlog,1256) ityopr(k),cidvri, ciopso1, iplntyp(nx)
          goto 9999
        endif  
c
c ---------------------------------------------------------
c jhb 2014/08 don't deal with reuse plans if the destination is a type 11 plan
c ---------------------------------------------------------
c               d. Find destination reuse plan named Creuse, if any, and
c		   Store in ireuse(k)
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        if(iopdesr(k).eq.7) then
        else
          iacc=0
          ion=-1
          istop=0
          ireuse1=0
cr        if(creuse(1:3).ne.'N/A') then
          if(NAuse.eq.0) then
            call oprFind(ityopr(k), 7, idumc,k,ion,iprinto,
     1         ireuse1,iops2, nx, creuse, iacc, istop, rops2,
     1         ioprsw(k), cidvri)     
          endif
c
c ---------------------------------------------------------
c         f. Check proper type of plan for a TransMountain Import
c                  Note iplntyp 3 & 5 are reservoir, 
c                       iplntyp 4 & 6 are diversion
          if (ireuse1.gt.0) then
            ireuse(k)=ireuse1
            iok=1
            if(iopdes(1,k).lt.0 .and. iplntyp(ireuse1).eq.5) iok=0
            if(iopdes(1,k).gt.0 .and. iplntyp(ireuse1).eq.6) iok=0
            if(iok.eq.1) then
              write(nlog,1255) ityopr(k),cidvri, ciopde, creuse,
     1          iplntyp(ireuse1)
              goto 9999
            endif
          endif
        endif
c
c ---------------------------------------------------------
c		e. Detailed output
     
        iout35=0
        if(iout35.eq.1) then
          write(nlog,2020) ityopr(k), cidvri, ityopr(k), 
     1      ciopde,  iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k), iopsou(2,k),
     1      ciopso2, iopsou(3,k), iopsou(4,k), creuse, ireuse1,
     1      oprlimit(k), iopSou(5,k),
     1      cdivtyp(k), iopdesR(k)
        endif  

        goto 1190
c        
c _________________________________________________________
c
c         
 1036   continue 
c               Type 36; Diversion with seasonal constraints
c		 (e.g. Meadow Rights)
c                destination = a diversion
c                source 1 (iopsou(1,k) = a diversion water right
c                source 2 (iopsou(3,k) = N/A
c                  ion=1 means turn off opr right if right is off
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found

        ion=1
        idumc=ifix(dumc)
c
c ---------------------------------------------------------
c               a1. Read monthly constraints 
        istop=0
        cx='NA'
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,ix, ix,
     1               nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               a2. Read intervening structures
        istop=0
        call oprFind(ityopr(k), 21, idumc,k,ion,iprinto,ix, ix,
     1               nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               b1. Find the destination, a diversion ID (type 3)
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        istop=0
        call oprFind(ityopr(k), 3, idumc,k,ion,iprinto,
     1       iopdes(1,k), iopdes(2,k), nx,ciopde, 0, 
     1       istop, rops2,ioprsw(k), cidvri)
        nd=iopdes(1,k)
        iopdesr(k)=3

c
c ---------------------------------------------------------
c               c. Find source 1 a diversion water right (type 13)
c		   Note ion  =1 Turn original water right off
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
c		        iacc =0 Allow account to be 0 (since 
c                                 it is ownership %)
c
c rrb 2006/11/27; Tie on/off to iopsou(2
cx      ion=1
        ion=iopsou(2,k)   
        istop=0
        iacc=0
        itype=13
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iopsou(1,k),iopsou(2,k), nx, ciopso1, iacc, 
     1       istop, rops2,ioprsw(k), cidvri)
     
     
        iopSouR(k)=itype
     
        ndr=iopsou(1,k)     
        nd1=idivco(1,ndr)
c
c ---------------------------------------------------------
c		d. Check the source water right is tied to the 
c		   same structure as the destination
     
         if(nd.ne.nd1) then
           write(nlog,1360) ityopr(k),cidvri, iopsou( 1,k),
     1      cdivid(nd1), cdivid(nd)
           goto 9999
         endif  
c
c ---------------------------------------------------------
c		e. Detailed output
     
        iout36=0
        if(iout36.eq.1) then
          write(nlog,2020) ityopr(k), cidvri, ityopr(k), 
     1      ciopde,  iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k), iopsou(2,k),
     1      ciopso2, iopsou(3,k), iopsou(4,k), creuse, ireuse1,
     1      oprlimit(k), iopSou(5,k),
     1      cdivtyp(k), iopdesR(k)
        endif  
        goto 1190
c        
c _________________________________________________________
c
c         
 1037   continue 
c               Type 37; Augmentation Well
c                source 1 (iopsou(1,k) = a well water right
c                source 2 (iopsou(3,k) = a Well Augmentation Plan
c		   for the pumping well
c		 destination 1 (iopdes(1,k) = a T&C or Augmentation
c                  plan
c                Note istop=0 Stop if not found
c		      istop=1 Do not Stop if not found

        idumc=ifix(dumc)
c
c ---------------------------------------------------------
c               a1. Read monthly constraints 
        istop=0
        cx='NA'
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,ix, ix,
     1               nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               a2. Read intervening structures
        istop=0
        call oprFind(ityopr(k), 21, idumc,k,ion,iprinto,ix, ix,
     1               nx, cx, 1, istop, rops2,ioprsw(k), cidvri) 
c
c ---------------------------------------------------------
c		Exit if wells are turned off     
        if(iwell.eq.0) then
          ioprsw(k)=0    
          goto 1190 
        endif
c
c ---------------------------------------------------------
c               b1. Find the destination, a well Plan (type 7)
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        istop=0
        call oprFind(ityopr(k), 7, idumc,k,ion,iprinto,
     1       iopdes(1,k),iopdes(2,k), nP,ciopde, 0, 
     1       istop, rops2,ioprsw(k), cidvri)
        iopdes(1,k)=nP        
        idcdD=idvsta(np)
        iopdesr(k)=7
        
c
c		Check proper type
        iok=1          
        if(iplntyp(nP).eq.1 .or. iplntyp(nP).eq.2 .or.
     1     iplntyp(nP).eq.10) iok=0          
        if(iok.eq.1) then
          write(nlog,1255) ityopr(k),cidvri, ciopde, creuse, 
     1    iplntyp(nP)
          goto 9999
        endif  
c
c ---------------------------------------------------------
c               c. Find source 1 a well water right (type 16)
c		   Note ion  =1 Turn original water right OFF
c			ion  =0 Leave original water right ON
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found

c		        iacc =0 Allow account to be 0 (since 
c                                 it is ownership %)
        iacc=0
        ion=0
c
c rrb 2009/03/30; Allow simulation to conitnue while StateDMI 
c	 	    is being updated
cx      istop=0
        istop=1
        itype=16
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iopsou(1,k),iopsou(2,k),nw, ciopso1, iacc, 
     1       istop, rops2,ioprsw(k), cidvri)
c
c rrb 2009/03/30; Allow simulation to conitnue while StateDMI 
c	 	    is being updated     
        if(nw.eq.0) then
          ioprsw(k)=0
          ioBeg(k) = 0
          ioEnd(k) = 0    
          write(nlog,1352)  ityopr(k),cidvri, ciopso1
          goto 1190
        endif
        
        iopSouR(k)=itype
        iopsou(1,k)=nw
c
c		Allow operating right admin number to control
c rrb 2008/09/23; Do not warn, Augmentation WElls are expected to 
c		  operate at a low priority
cx        if(abs(rdvnkw(nw)-ropnk(k)).gt. small) then          
cx          if(iprinta.eq.0) write(ntmp,728)
cx          iprinta=iprinta+1
cx          write(ntmp,729) iprinta, ityopr(k), cidvri, ciopso1,
cx     1      rdvnkW(nw), ropnk(k), ropnk(k)
cx        endif
c
c ---------------------------------------------------------
c               d. Find the source plan (type 7) under iopsou(3,k) 
c		   Note a Aug Plan is required for a type 37 Aug Well
c		   this may or many not be the same as the 
c		   destination plan
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        iacc=0
        ion=-1
        istop=0
        call oprFind(ityopr(k), 7, idumc,k,ion,iprinto,
     1       np2,iops2, nx, ciopso2, iacc, istop, rops2,
     1       ioprsw(k), cidvri)
        iopsou(3,k)=np2
        
        if(iplntyp(np2) .ne. 2) then
          write(nlog,1263) ityopr(k),cidvri, ciopso2, iplntyp(np2)
          goto 9999
        endif     
c
c ---------------------------------------------------------
c		e. Detailed output
     
        iout37=0
        if(iout37.eq.1) then
          write(nlog,2020) ityopr(k), cidvri, ityopr(k),
     1      ciopde,  iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k), iopsou(2,k),
     1      ciopso2, iopsou(3,k), iopsou(4,k), creuse, ireuse1,
     1      oprlimit(k), iopSou(5,k),
     1      cdivtyp(k), iopdesR(k)
        endif             

        goto 1190
c        
c _________________________________________________________
c
c         
 1038   continue 
c               Type 38; Out of Priority Direct Diversion
c                source 1 (iopsou(1,k) = a reservoir water right
c                source 2 (iopsou(3,k) = a Well Augmentation Plan
c		   for the pumping well
c		 destination 1 (iopdes(1,k) = a T&C or Augmentation
c                  plan
c                Note istop=0 Stop if not found
c		      istop=1 Do not Stop if not found

        idumc=ifix(dumc)
c
c ---------------------------------------------------------
c               a1. Read monthly constraints 
        istop=0
        cx='NA'
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,ix, ix,
     1               nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               a2. Read intervening structures
        istop=0
        call oprFind(ityopr(k), 21, idumc,k,ion,iprinto,ix, ix,
     1               nx, cx, 1, istop, rops2,ioprsw(k), cidvri)     
c
c ---------------------------------------------------------
c               b1. Find destination diversion 
c                   Note itype=3 for a diversion 
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        itype=3
        istop=1
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1, iopdes(2,k), nx,ciopde, 1, istop, rops2,
     1       ioprsw(k), cidvri)
        iopdes(1,k) = iops1
        ndD=iopdes(1,k)
        iopdesr(k)=3
        
c        
c ---------------------------------------------------------
c               b2. Find destination reservoir (type 2)
c                   Note itype=2 for a reservoir
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        if(iops1.eq.0) then
          itype=2
          istop=0
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         iops1, iopdes(2,k), nx,ciopde, 1, istop, rops2,
     1         ioprsw(k), cidvri)
          iopdes(1,k)=-iops1
          ndD=iopdes(1,k)
          iopdesr(k)=2          
c         write(nlog,*) ' Oprinp;  iopdes(1,k) = ', iopdes(1,k)
        endif
c
c ---------------------------------------------------------
c               c1. Find source 1 a reservoir water right (type 12)
c		    Note ion  =1 Turn original water right OFF
c			ion  =0 Leave original water right ON
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
c		        iacc =0 Allow account to be 0 
        itype=12
        iacc=0
        ion=0
        istop=0
        call oprFind(ityopr(k), itype,idumc,k,ion,iprinto,
     1    nS,nSA, nX, ciopso1, iacc, istop, rops2,ioprsw(k), cidvri)
     
        iopSouR(k)=itype
        iopsou(1,k)=nS
c
c		Check the operating right admin number is less
c               than the senior subordinated right
        if((ropnk(k) - dcrres(nS)).gt. small) then   
          write(nlog,*) cidvri, ropnk(k), dcrres(nS), k, ns       
          write(nlog,733) cidvri, ropnk(k), dcrres(nS)
c
c rrb 2010/02/05; Allow execution to proceed          
cx        goto 9999
        endif
        
c
c ---------------------------------------------------------
c               c2. Find source 2 a diversion water right (type 13)
c		    Note ion  =1 Turn original water right OFF
c			ion  =0 Leave original water right ON
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
c		        iacc =0 Allow account to be 0 
        itype=13
        iacc=0
        ion=0
        istop=1
        call oprFind(ityopr(k), itype,idumc,k,ion,iprinto,
     1    nR, nrA,nX, ciopso2, iacc, istop, rops2,ioprsw(k), cidvri)
        iopsou(3,k)=nR
c
c ---------------------------------------------------------
c               c3. Find source 2 a reservoir water right (type 12)
c		    Note ion  =1 Turn original water right OFF
c			ion  =0 Leave original water right ON
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
c		        iacc =0 Allow account to be 0 
        if(nr.eq.0) then
          itype=12
          iacc=0
          ion=0
          istop=0
          call oprFind(ityopr(k), itype,idumc,k,ion,iprinto,
     1      nR, nRa, nX, ciopso2, iacc, istop, rops2,ioprsw(k), cidvri)
          iopsou(3,k)=-nR
        endif  
c
c ------------------------------------------------------------------
c               d1. Find destination reuse plan named Creuse, if any, 
c		    and store in ireuse(k)
c			ion  =0 Leave original water right ON
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
c		        iacc =0 Allow account to be 0 
        iacc=0
        ion=-1
        istop=0
        ireuse1=0        
        if(NAuse.eq.0) then                
          call oprFind(ityopr(k), 7, idumc,k,ion,iprinto,
     1         ireuse1,iops2, nx, creuse, iacc, istop, rops2,
     1         ioprsw(k), cidvri)     
        endif
        
c
c ---------------------------------------------------------
c		d2. Check proper type of plan 
c                   Note iplntyp 9 is a OOP diversino or storage
        if (ireuse1.gt.0) then
          ireuse(k)=ireuse1        
          iok=1
          if(iplntyp(ireuse1).eq.9 .or. iplntyp(ireuse1).eq.10) iok=0

          if(iok.eq.1) then
            write(nlog,1263) ityopr(k),cidvri, creuse, iplntyp(ireuse1)
            goto 9999
          endif
        endif        
c
c ---------------------------------------------------------
c		e. Detailed output
     
        iout38=0
        if(iout38.eq.1) then
          write(nlog,2020) ityopr(k), cidvri, ityopr(k),
     1      ciopde,  iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k), iopsou(2,k),
     1      ciopso2, iopsou(3,k), iopsou(4,k), creuse, ireuse1,
     1      oprlimit(k), iopSou(5,k),
     1      cdivtyp(k), iopdesR(k)
        endif  
        goto 1190
        
c        
c _________________________________________________________
c
c         
 1039   continue 
c               Type 39; Alternate Point
c                source 1 (iopsou(1,k) = a diversion water right
c                source 2 (iopsou(3,k) = NA
c		 destination 1 (iopdes(1,k) = a Well or Diversion
c		 Note istop=0 Stop if not found
c		      istop=1 Do not Stop if not found

        idumc=ifix(dumc)
c
c ---------------------------------------------------------
c               a1. Read monthly constraints 
        istop=0
        cx='NA'
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,ix, ix,
     1               nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               a2. Read intervening structures
        istop=0
        call oprFind(ityopr(k), 21, idumc,k,ion,iprinto,ix, ix,
     1               nx, cx, 1, istop, rops2,ioprsw(k), cidvri)   
c
c ---------------------------------------------------------
c		 a3. Exit if wells are turned off     
        if(iwell.eq.0) then
          ioprsw(k)=0    
          goto 1190 
        endif       
c
c ---------------------------------------------------------
c               b1. Find a diversion destination
c                   Note:
c                     itype=3 for a diversion 
c		                  istop=0 Stop if not found
c		                  istop=1 Do not Stop if not found
        itype=3
        istop=0
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1, iopdes(2,k), nx,ciopde, 0, istop, rops2,
     1       ioprsw(k), cidvri)
        iopdes(1,k) = iops1
        iopdesr(k) = 3
        ndD=iopdes(1,k)
c        
c ---------------------------------------------------------
c               b2. Find destination well (type 6)
c                Note itype=6 for a well
c		   Note istop=0 Stop if not found
c		        istop=1 (OK if not found)
c rrb 2010/11/15; Revise the destination cannot be a well
cx        if(iops1.eq.0) then
cx          itype=6
cx          istop=1
cx          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
cx     1         iops1, iopdes(2,k), nx,ciopde, 1, istop, rops2,
cx     1         ioprsw(k), cidvri)
cx          iopdes(1,k)=iops1
cx          iopdesr(k)=6
cx          ndD=iopdes(1,k)
cxc         write(nlog,*) ' Oprinp;  iopdes(1,k) = ', iopdes(1,k)
cx        endif    
c
c ---------------------------------------------------------
c               c1. Find source 1 a diversion water right (type 13).
c			ion  =0 Leave original water right ON
c		        ion  =1 Turn original water right OFF	
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
c		        iacc =0 Allow account to be 0 
        itype=13
        iacc=0
        ion=iopsou(2,k)
        istop=0
        call oprFind(ityopr(k), itype,idumc,k,ion,iprinto,
     1       iops1, iops2,nX, ciopso1, iacc, istop, rops2,
     1       ioprsw(k), cidvri)
     
        iopSouR(k)=itype
        iopsou(1,k)=iops1
c
c ---------------------------------------------------------
c rrb 2008/09/04; 
c               d1. Get source 2 data, an alternate point diversion
c                   itype=3
c                   istop=0 (Stop if not found)
c                   istop=1 (Do not stop if not found)
c
        itype=3
        istop=1
        if(NAs2.eq.0) then        
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1      iops1, iops2, nx, ciopso2, 1, istop, rops2,
     1      ioprsw(k), cidvri)
          iopsou(3,k)=iops1          
        endif
c
c ---------------------------------------------------------
c rrb 2008/09/04; 
c               d2. Get source 2 data, an alternate point well
c                   itype=6
c                   istop=0 (Stop if not found)
        itype=6
        istop=0
        if(iops1.eq.0) then        
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1      iops1, iops2, nx, ciopso2, 1, istop, rops2,
     1      ioprsw(k), cidvri)
          iopsou(3,k)=-1*iops1          
        endif
c
c ---------------------------------------------------------
c		e. Detailed output
     
        iout39=0
        if(iout39.eq.1) then
          write(nlog,2020) ityopr(k), cidvri, ityopr(k), 
     1      ciopde,  iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k), iopsou(2,k),
     1      ciopso2, iopsou(3,k), iopsou(4,k), creuse, ireuse1,
     1      oprlimit(k), iopSou(5,k),
     1      cdivtyp(k), iopdesR(k)
        endif  
        goto 1190
c        
c _________________________________________________________
c
c         
 1040   continue 
c 
c               Type 40; South Platte Compact Release
c		   Instream flow tied to a stream gage
c		   Demand = max(0, min(ifa, qindex-q@ifa))
c
c                  ion=1 means turn off opr right if right is off
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        ion=1
        istop=0
        idcdd=0
        iss=0
           
        idumc=ifix(dumc)
c
c ---------------------------------------------------------
c               a. Read monthly constraints 
        istop=0
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)      
c
c ---------------------------------------------------------
c
c               b. Set the Destination
c                  Note for a type 40, a destination like 64x means
c                  allow all structures to try and divert if their
c                  id does not begin with 64.  The S Platte plan 
c                  can serve all water rights not in WD 64
c
        if(ciopde(3:3) .eq.'x' .or. ciopde(3:3).eq.'X') then
          isp1=k         
          itype=3
          iopdes(1,k) = -1          
          iopDesR(k)=itype 
        else
c
c ---------------------------------------------------------
c               b1. Find an instream flow destination
c                   Note:
c                     itype=3 for a diversion 
c		                  istop=0 Stop if not found
c		                  istop=1 Do not Stop if not found
          itype=1
          istop=0
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         iops1, iopdes(2,k), nx, ciopde, 0, istop, rops2,
     1         ioprsw(k), cidvri)
          iopdes(1,k) = iops1
          iopdesr(k) = itype
          ndD=iopdes(1,k)       
        endif
c
c
c ---------------------------------------------------------
c               c. Find source 1 a Plan
c                  Note itype=0 for a stream ID
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
c		        iacc=0 allows account to be 0 (since 
c                         it is ownership %)
c		        ion=0 leaves the original water right on
c           ion=1 turn off the origianl right
c		        itype=7= plan structure
        itype=7
        ion=1
        iacc=1
        istop=0
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1,iopsou(2,k), nx, ciopso1, 1, istop, rops2,
     1       ioprsw(k), cidvri)
        
        iopSouR(k)=itype
        iopsou(1,k)=iops1
        iscd=ipsta(iops1)
        iexpoint(k)=iscd
        np=iops1
c
c		Check Plan type is an Admin Plan         
         if(iplntyp(np).ne.11) then
           write(nlog,1255) ityopr(k),cidvri, ciopde, creuse, 
     1      iplntyp(ndD)
           goto 9999
         endif   
c
c
c ---------------------------------------------------------
c		            d. Detailed output
     
        iout40=0
        if(iout40.eq.1) then
          write(nlog,2020) ityopr(k), cidvri, ityopr(k),
     1      ciopde,  iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k), iopsou(2,k),
     1      ciopso2, iopsou(3,k), iopsou(4,k), creuse, ireuse1,
     1      oprlimit(k), iopSou(5,k),
     1      cdivtyp(k), iopdesR(k)
        endif
        
        goto 1190
c        
c _________________________________________________________
c
c         
 1041   continue 
c 
c               Type 41; Reservoir Storage Limited by Volume in Plans
c
c                  ion=1 means turn off opr right if right is off
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        ion=1
        istop=0
        idcdd=0
        iss=0
        
        idumc=ifix(dumc)
c
c ---------------------------------------------------------
c               a. Read monthly constraints 
        istop=0
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c     
c ---------------------------------------------------------
c               b. Read the associated plans 
        idumc2=iabs(idumc)-12
c
c rrb 2021/04/18; Compiler warning
cx      idumc3=amax0(idumc, idumc2)
        idumc3=max(idumc, idumc2)
        if(idumc.ge.1 .or. idumc2.ge.1) then
          read(55,'(36x,10a12)',end=2000,err=2000)
     1             (cntern(i),i=1,idumc3)
          if(iecho.eq.1) write(nchk,'(36x,10a12)') 
     1             (cntern(i),i=1,idumc3)       
c         write(nlog,*) (cntern(i), i=1,idumc3)
        else
          write(nlog,1213) cidvri, ityopr(k)
          goto 9999
        endif     
c      
c ---------------------------------------------------------
c               b1. Find the Associated Plans
c                   and assign to intern(k,1)
c		    Note:
c		    ion=1 turn the source water right off
c			   and does check if the source right if off
c		    iacc=0 allows account to be 0 
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
c                   itype=7 for a plan structure
        ion=0
        istop=0
        itype=7
        iacc=1
        do i=1,idumc3
          ciopso5=cntern(i)
c         write(nlog,*) ' Oprinp; Type 41 limiting Plan ID ', ciopso5
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         iops1,iops2, nx, ciopso5, iacc, istop, rops2,
     1         ioprsw(k), cidvri)
          intern(k,i) = iops1
c
c		Check the plan specified is an OOP Plan          
          iok=1
          if(iplntyp(iops1).eq.9 .or. iplntyp(iops1).eq.10) iok=0
          if(iok.eq.1) then
            write(nlog,1263) ityopr(k),cidvri, creuse, iplntyp(iops1)
            goto 9999
          endif
        end do
c
c ---------------------------------------------------------
c               c1. Find destination Reservoir
c                  Note itype=12 for a reservoir ID
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        itype=2
        istop=0
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1, iopdes(2,k), nx,ciopde, 1, istop, rops2,
     1       ioprsw(k), cidvri)
        iopdes(1,k) = iops1
        if(iops1.gt.0) idcdD=ifrsta(iops1)        
c
c
c ---------------------------------------------------------
c               d1. Find source 1 a reservoir right
c                   Note itype=12 for a reservoir right ID
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
c		        iacc=0 allows account to be 0
c		        ion=1 turns the original water right off
        itype=12
        ion=1
        iacc=0
        istop=0
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1,iopsou(2,k), nx, ciopso1, 1, istop, rops2,
     1       ioprsw(k), cidvri)
        
        iopSouR(k)=itype
        iopsou(1,k)=iops1
c
c ---------------------------------------------------------
c		d2. Check the source right is tied to the destination ID        
        nr=iresco(1,iops1)
        if(iopdes(1,k).ne.nr) then
          write(nlog,1265) ityopr(k),cidvri,ciopso1,cresid(nr),ciopde
          goto 9999
        endif
c
c ---------------------------------------------------------
c		d3. Allow operating right admin number to control
        if(abs(rrsnk(iops1)-ropnk(k)).gt. small) then          
          ropnk(k)=rrsnk(iops1)
          if(iprinta.eq.0) write(ntmp,728)
          iprinta=iprinta+1
          write(ntmp,729) iprinta, ityopr(k), cidvri, ciopso1,
     1      rrsnk(iops1), ropnk(k), ropnk(k)
        endif
        
c
c ---------------------------------------------------------
c		f. Detailed output
     
        iout41=0
        if(iout41.eq.1) then
          write(nlog,2020) ityopr(k), cidvri, ityopr(k),
     1      ciopde,  iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k), iopsou(2,k),
     1      ciopso2, iopsou(3,k), iopsou(4,k), creuse, ireuse1,
     1      oprlimit(k), iopSou(5,k),
     1      cdivtyp(k), iopdesR(k)
        endif
        
        goto 1190
c _________________________________________________________
c
c         
 1042   continue 
c               Type 42; Plan Reset
c                source 1 (iopsou(3,k) = a plan ID
c                  ion=1 means turn off opr right if right is off
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
cr      write(nlog,*) '  Oprinp; Type 29'
        ion=1
        istop=0
        nr=0
        np1=0
        np2=0
        
        idumc=ifix(dumc)
c
c ---------------------------------------------------------
c               a. Read monthly constraints 
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,ix, ix,
     1               nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               b. Find source 1 a plan (type 7)
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        istop=0
        itype=7
        iacc=1
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       np1,iopsou(2,k), nx, ciopso1, iacc, istop, rops2,
     1       ioprsw(k), cidvri)
c
        if(np1.gt.0) then
          iopSouR(k)=itype        
          iopsou(1,k)=-1*np1
        endif
        
c
c ---------------------------------------------------------
c               c. Check the proper plan type is specifiec
        iok=1
        if(iplntyp(np1).eq.1  .or. iplntyp(np1).eq.2 .or.
     1     iplntyp(np1).eq.10 .or. iplntyp(np1).eq.9) iok=0

        if(iok.eq.1) then
          write(nlog,1266) ityopr(k),cidvri, ciopso1, iplntyp(np1)
          goto 9999
        endif
c
c ---------------------------------------------------------
c		d. Detailed output
     
        iout42=0
        if(iout42.eq.1) then
          write(nlog,2020) ityopr(k), cidvri, ityopr(k),
     1      ciopde,  iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k), iopsou(2,k),
     1      ciopso2, iopsou(3,k), iopsou(4,k), creuse, ireuse1,
     1      oprlimit(k), iopSou(5,k),
     1      cdivtyp(k), iopdesR(k)
        endif
        
        
        goto 1190
        
c _________________________________________________________
c
c         
 1043   continue 
c               Type 43; In-Priority Supply
c                source 1 (iopsou(1,k) = NA
c		             destination (iopdes(1,k) a Well Augmentation Plan
c                  ion=1 means turn off opr right if right is off
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
cr      write(nlog,*) '  Oprinp; Type 29'
        istop=0
        nr=0
        np1=0
        np2=0
        
        idumc=ifix(dumc)
c
c ---------------------------------------------------------
c               a. Read monthly constraints 
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,ix, ix,
     1               nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               b. Find source 1 NA
        iopsou(1,k)=0
c
c ---------------------------------------------------------
c               c. Find Destination a plan (type 7)
c		   Note itype =7 for a plan
c                       istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        itype=7
        istop=0
        iacc=1
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1,iopsou(2,k), nx, ciopde, iacc, istop, rops2,
     1       ioprsw(k), cidvri)
c
        iopdes(1,k)=iops1
        iopdesr(k)=7
c
c ---------------------------------------------------------
c               d. Check the proper plan type is specified
        iok=1
c       if(iplntyp(iops1).ne.2) then
c rrb 2007/10/30; Allow a T&C plan to be supplied in Priority
        if(iplntyp(iops1).eq.1 .or. iplntyp(iops1).eq.2 .or.
     1     iplntyp(iops1).eq.10) iok=0
        if(iok.eq.1) then
          write(nlog,12661) ityopr(k),cidvri, ciopso1, iplntyp(iops1)
          goto 9999
        endif
c
c ---------------------------------------------------------
c		d. Detailed output
     
        iout43=0
        if(iout43.eq.1) then
          write(nlog,2020) ityopr(k), cidvri, ityopr(k),
     1      ciopde,  iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k), iopsou(2,k),
     1      ciopso2, iopsou(3,k), iopsou(4,k), creuse, ireuse1,
     1      oprlimit(k), iopSou(5,k),
     1      cdivtyp(k), iopdesR(k)
        endif
        
        
        goto 1190
c        
c _________________________________________________________
c
c         
 1044   continue 
c               Type 44; Recharge Well
c                source 1 (iopsou(1,k) = a well water right
c                source 2 (iopsou(3,k) = NA
c		   for the pumping well
c		 destination 1 (iopdes(1,k) = a Recharge Reservoir
c                Note istop=0 Stop if not found
c		      istop=1 Do not Stop if not found

        idumc=ifix(dumc)
c
c ---------------------------------------------------------
c               a1. Read monthly constraints 
        istop=0
        cx='NA'
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,ix, ix,
     1               nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               a2. Read intervening structures
        istop=0
        call oprFind(ityopr(k), 21, idumc,k,ion,iprinto,ix, ix,
     1               nx, cx, 1, istop, rops2,ioprsw(k), cidvri)   
c
c ---------------------------------------------------------
c		 a3. Exit if wells are turned off     
        if(iwell.eq.0) then
          ioprsw(k)=0    
          goto 1190 
        endif
       
c
c ---------------------------------------------------------
c               b1. Find the destination, a recharge Reservoir (2)
c		   Note itype =2 for a REservoir
c                       istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        itype=2
        istop=0
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iopdes(1,k),iopdes(2,k), nr,ciopde, 0, 
     1       istop, rops2,ioprsw(k), cidvri)
        iopdes(1,k)=nr
        idcdD=irssta(nr)
        iopdesr(k)=2
        
c
c ---------------------------------------------------------
c               c. Find source 1 a well water right (type 16)
c		               Note ion  =1 Turn original water right OFF
c			                  ion  =0 Leave original water right ON
c		               Note istop=0 Stop if not found
c		                    istop=1 Do not Stop if not found
c			             itype = 16 for a well water right
c		               iacc =0 Allow account to be 0 (since 
c                          it is ownership %)
        itype=16
        iacc=0
        ion=0
c
c rrb 2009/03/30; Allow simulation to conitnue while StateDMI 
c	 	    is being updated     
cx      istop=0
        istop=1
        
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iopsou(1,k),iopsou(2,k),nwr, ciopso1, iacc, 
     1       istop, rops2,ioprsw(k), cidvri)
     
c
c rrb 2009/03/30; Allow simulation to conitnue while StateDMI 
c	 	    is being updated 
c rrb 2009/05/21; Correction    
cx      if(nwr.eq.0) then
        if(nwr.le.0) then
          ioprsw(k)=0
          ioBeg(k) = 0
          ioEnd(k) = 0    
          write(nlog,1352)  ityopr(k),cidvri, ciopso1
          goto 1190
        endif     
     
        iopSouR(k)=itype     
        iopsou(1,k)=nwr
c
c ---------------------------------------------------------
c		d. Allow operating right admin number to control
        if(abs(rdvnkw(nwr)-ropnk(k)).gt. small) then          
          if(iprinta.eq.0) write(ntmp,728)
          iprinta=iprinta+1
          write(ntmp,729) iprinta, ityopr(k), cidvri, ciopso1,
     1      rdvnkW(nwr), ropnk(k), ropnk(k)
        endif
c
c ---------------------------------------------------------
c		e. Find Plan, if any, for depletion obligation
c		Note: itype = 7 for a Plan
        if(NAuse.eq.0) then                
          iacc=0
          ion=0
          istop=0
          call oprFind(ityopr(k), 7, idumc,k,ion,iprinto,
     1         ireuse1,iops2, nx, creuse, iacc, istop, rops2,
     1         ioprsw(k), cidvri)     
        endif

        goto 1190
        
        
c        
c _________________________________________________________
c
c         
 1045   continue 
c               Type 45; Carrier with Losses
c                to a diversion or reservoir
c		             where source is a diversion, diversion water right
c		             or reservoir water right
c rrb 01/06/20; 
c                destination = diversion or reservoir ID
c                source 1 = a diversion or water right 
c                source 2 = water right type >=0 = diversion, <0=reservoir
c		             source 3 = NA
c                source 4 = NA
c                source 5 = NA
c		             source 6 = NA
c		             source 7 = NA
c
c                ion=1 means turn off opr right if right is off
c		             Note istop=0 Stop if not found
c		             istop=1 Do not Stop if not found
        iout45X=0
        if(iout45X.eq.1) write(nlog,*) '  Oprinp; Reading type 45 rule'
 
        ion=1
        istop=0
        iwarnr=0
        iwarno=0
        nc=0
        
        idumc=ifix(dumc)
c
c ---------------------------------------------------------
c               a. Read monthly constraints 
c		               Note istop=0 Stop if not found
        istop=0
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               a2. Read intervening structures plus loss (23)
c		                Note istop=0 Stop if not found
        istop=0
        call oprFind(ityopr(k), 23, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c rrb 2007/07/03; 
c		            a3. Read the miscellaneous limit data. Note:
c	 	                Oprlimit(k)	= 0 no limit
c		 			          = 2 diversion demand limit
c		 			          = 3 reservoir target limit
c                   = 4 limited by a type 47 operating rule
c                   = 5 limited by a type 51 operating rule
c                   = 6 limited by a WWSP User (plan type 14)
c                   = 7 limited by a spill order (e.g. store from river but
c                       spill from another account 
c                   =14 limited by two Plan limits (type 12) with monthly
c                         and annual limits
      iopsou(5,k)=0
      iopsou(6,k)=0
c
c rrb 2020/01/25; Setting oprlimit(k) = 9999 is necessary to
c                 allow oprlimit(k) to be used as a switch
c                 for various controls.
      oprlimit(k)=9999.
      do i=1,12
        oprmax(k,i)=9999.
      end do  
c
c
      if(iout45X.eq.1) then
        write(nlog,*) '  Oprinp; Reading misc, ioprlim(k) ', ioprlim(k) 
      endif
c
c ---------------------------------------------------------
c               a3-0. Ioprlim(k).lt 0
c                     Find the miscellaneous shared right limit.
      if(ioprlim(k).lt.0) then
        write(nlog, 1218) cidvri, ityopr(k), oprlimit(k)
        goto 9999
      endif
c
c ---------------------------------------------------------
c rrb 2019/09/07; Add a warning
c               a3-1 Save type 1 for future applications
      if(ioprlim(k).eq.1) then
        write(nlog,12660) ityopr(k),cidvri, ioprlim(k)
        goto 9999          
      endif
c
c ---------------------------------------------------------
c               a3-2. Ioprlim(k).eq.2
c                     Find the miscellaneous reservoir limit. Note
c                	    type=2 for a reservoir
c		   	              istop=0 Stop if not found
c			                iacc=1 Check the diversion account
      if(ioprlim(k).eq.2) then
        read(55,*,end=2048,err=2048) cx
        itype=2
        istop=0
        iacc=0
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1, iopdes(2,k), nx,cx, iacc, 
     1       istop, rops2,ioprsw(k), cidvri)
        iopsou(5,k)=iops1       
        
cx        write(nlog,*) ' Oprinp; Reservoir limit ioprlim(k)= ', 
cx     1    ioprlim(k), iopsou(5,k)
      endif
c
c ---------------------------------------------------------
c               a3-3. Ioprlim(k).eq.3
c                 Find the miscellaneous diversion limit. Note
c                	itype=3 for a diversion
c		   	                 istop=0 Stop if not found
c 			                 iacc=1 Check the diversion account

cx    write(nlog,*) ' Oprinp; Get Div limit ioprlim(k)= ', ioprlim(k)
      if(ioprlim(k).eq.3) then
        read(55,*,end=2048,err=2048) cx
        itype=3
        istop=0
        iacc=0
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1, iopdes(2,k), nx,cx, iacc, 
     1       istop, rops2,ioprsw(k), cidvri)
        iopsou(5,k)=iops1       
        
        if(ioutlim.eq.1) then
          write(nlog,*) ' Oprinp; Diversion limit ioprlim(k)= ', 
     1      ioprlim(k), iopsou(5,k)
        endif
      endif

c
c rrb 2011/10/15; Update for a monthly and annual limit
c
c ---------------------------------------------------------
c rrb 2007/07/03; 
c		            a3-4. Ioprlim(k).eq.4 .or .eq. 14
c                   Read the operating rule associated with
c		                a monthly or annual plan limit adjustment
c		                when Oprlimit(k) > 0
c		                istop=0  Stop if not found
c                   iacc=1 Check the destination account

c	                 	itype=24 find monthly and annual plan limits
c                                within the operating rule
c               
cx    write(nlog,*) ' Oprinp; type 45 ioprlim ', ioprlim(k)
c
c rrb 2018/08/24; Allow TWO type 4 values
cx    if(ioprlim(k).eq.4) then
      if(ioprlim(k).eq.4 .or. ioprlim(k).eq.14) then
c      
        istop=0
        itype=24    
        iacc=0      
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1    iopsou(5,k),iopsou(6,k), nx, cAssoc, iacc, 
     1    istop, rops2,ioprsw(k), cidvri)
        iopsou(5,k)=nx 
c
c rrb 2020/07/28; Check that the operating rule that contains
c                 a plan limit is a type 47 operating rule
c                 else the limit is not set properly
        if(ityopr(nx).ne.47) then       
          write(nlog,12722) ityopr(k),cidvri, cAssoc, ityopr(nx)
          goto 9999
        endif
c
c		            Set ciospoX5 to source 1 of the plan associated with 
c               the above operating rule  
c
c rrb 2020/07/28; Correction
c rrb 2020/07/28; Back to original
        ip5=iopsou(1,nx)
cx        write(nlog,*) '  Oprinp; k, nx, iopsou(1,nx),ip5'
cx        write(nlog,*) '  Oprinp;', k, nx, iopsou(1,nx), ip5
        ciopsoX5(k)=pid(ip5)
      endif    
      
c
c ---------------------------------------------------------
c rrb 2007/07/03; 
c		            a3-14. Ioprlim(k) .eq. 14
c rrb 2018/08/24; Allow TWO type 4 values
      if(ioprlim(k).eq.14) then
      
        istop=0
        itype=24    
        iacc=0      
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1    iops1,iops2, nx, cAssoc2, iacc, 
     1    istop, rops2,ioprsw(k), cidvri)
        iopsou(6,k)=nx 
c
c		            Set ciospoX5 to the plan associated with the 
c               above operating rule  
        ip6=iopsou(1,nx)
        ciopsoX6(k)=pid(ip6)
      endif           
c
c ---------------------------------------------------------
c rrb 2018/08/19; 
c		            a3-5. Ioprlim(k).eq.5
c                   Call Oprfind that reads the operating rule 
c                   and on/off switch associated with
c		                a Type 53 - WWSP operating rule
c		                when Oprlimit(k) = 5
c		                istop=0  Stop if not found
c	                 	itype=27 Read and find an operating rule ID 
c                            and on/off switch
c               
cx    write(nlog,*) ' Oprinp; type 45 ioprlim ', ioprlim(k)
      if(ioprlim(k).eq.5) then      
c
c rrb 2018/08/12; Correction; not used
cx      oprlimit(i) = ioprlim(k)      
        istop=0
        itype=27          
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1    iops1, iops2, nx, cx, 1, 
     1    istop, rops2, ioprsw(k), cidvri)
c
        iopsou(5,k) = iops1
        iopsou(6,k) = iops2  
        write(nlog,*) '  Oprinp; ', cidvri, cAssoc, iops1, iops2    
c
      endif      
c
c ---------------------------------------------------------
c rrb 2018/07/29; 
c		            a3-6. Ioprlim(k) = 6
c                   Read WWSP User plan data ssociated with
c		                a Type 53 WWSP operating rule
c		                when Oprlimit(k) = 6
c               
cx    write(nlog,*) ' Oprinp; type 45 ioprlim ', ioprlim(k)
      if(ioprlim(k).eq.6) then
        read(55,*,end=2048,err=2048) cx

c
c                   Read WWSP User Plan
c		                istop=0 Stop if not found
c	                 	itype=7 Plan
c                   iacc=1 Check the destination account
        istop=0
        itype=7
        iacc=0                  
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1    iops1, iops2, nx, cx, iacc, 
     1    istop, rops2, ioprsw(k), cidvri)
c
        iopsou(5,k) = iops1 
        ciopsoX5(k) = pid(iops1)
c
c rrb 2019/07/21; Set iwwPlan=1 to indicate we have at least one 
c                 direct diversion that is part of a WWSP Plan
c                 to be used in BomSec to know when to initialize
c rrb 2019/07/28; Move initialization of iwwPlan to a type 46 
c                 multi-split operating rule
cx        iwwPlan=k
c
c rrb 2019/04/20; Revise to recognize a WWSP User is a type 15 plan
cx      if(iplntyp(iops1).ne.14) then
cx        write(nlog,1272) ityopr(k),cidvri, ciopso1, iplntyp(iops1),14
        if(iplntyp(iops1).ne.15) then
c
c rrb 2019/05/17; Update
cx        write(nlog,1272) ityopr(k),cidvri, 'WWSP User', cx,
cx   1                     iplntyp(iopd1), 15, 0, 0
cx
          write(nlog,12721) ityopr(k),cidvri, 'Constraint',cx, 
     1                         iplntyp(iops1), 'Constraint', 15
          goto 9999
        endif
c
        write(nlog,*) '  Oprinp; ', cidvri, cx, iops1, pid(iops1)    
      endif 


c
c ---------------------------------------------------------
c rrb 2020/01/24; Add Spill Order (store in a reservoir if another 
c                   if account has storage and spills
c               a3-7. Ioprlim(k).eq.7
c                     Find the reservoir and account that will 
c                     limit the amount stored and spill when the right
c                     is operated
c                	    type=2 for a reservoir
c		   	              istop=0 Stop if not found
c			                iacc=1 Check the diversion account
      if(ioprlim(k).eq.7) then
        read(55,*,end=2048,err=2048) cx, ix   
        cAssoc=cx
        cAssoc2= 'Account ='        
        itype=2
        istop=0
        iacc=0
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1, iopdes(2,k), nx,cx, iacc, 
     1       istop, rops2,ioprsw(k), cidvri)
        iopsou(5,k)=iops1   
        iopsou(6,k) = ix
c
c               Checks
        if(iout45X.eq.1) then
          write(nlog,*) '  Oprinp; iout45X ', iout45x
          write(nlog,*) '  Oprinp; Type 45 with ioprlim=7'
          write(nlog,*)   ioprlim(k), oprlimit(k), cx, ix
          write(nlog,*)   iopsou(5,k), iopsou(6,k)
        endif 
      endif


c
c ---------------------------------------------------------
c               c1. Find destination diversion 
c                   Note itype=3 for a diversion 
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
c                        iacc=1 Check the destination account
c     
      if(iout45X.eq.1) then
        write(nlog,*) '  Oprinp; Reading Destination, ciopde ', ciopde 
      endif      
      
      itype=3
      istop=1
      iacc=0
      call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1     iops1, iopdes(2,k), nx,ciopde, iacc, 
     1     istop, rops2,ioprsw(k), cidvri)
c
c rrb 2008/05/21; Correction   
      if(iops1.gt.0) then  
        iopdes(1,k) = iops1
        ndD=iopdes(1,k)
        isD=idvsta(ndD)
        iopDesr(k)=3
        cdestyp='Diversion   '
      endif      
c        
c ---------------------------------------------------------
c               c2. Find destination reservoir (type 2)
c                   Note itype=2 for a reservoir
c		   Note istop=0 Stop if not found
c		        istop=1 (OK if not found)
c			 iacc=1  Check reservoir account 
      if(iops1.eq.0) then
        itype=2
        istop=1
        iacc=1
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1, iopdes(2,k), nx,ciopde, iacc, 
     1       istop, rops2,ioprsw(k), cidvri)
        iopdes(1,k)=-iops1
        ndD=iops1
        isD=irssta(ndD)
        iopDesr(k)=2
        cdestyp='Reservoir   '
        
cx       write(nlog,*) 
cx     1   ' Oprinp;  k, iopdes(1,k), iopdes(2,k), iopdesr(k) = ', 
cx     1    k, iopdes(1,k), iopdes(2,k), iopdesr(k)
      endif  

c        
c ---------------------------------------------------------
c rrb 2008/03/21; Revised warning when several destinations are possible        
      if(iops1.eq.0) then
        write(nlog,1300) cidvri, ityopr(k), ciopde
        goto 9999
      endif   
        
c
c ---------------------------------------------------------
c               d1. Find source 1 a diversion structure (3).  Note:
c 		              ion=0  leaves the original water right on
c		                iacc=0 allows account to be 0 (since 
c                          it is ownership %)
c		                istop=0 Stop if not found
c		                istop=1 Do not Stop if not found
c rrb 05/06/15; Note turn original right off (ion=1)
      ion=1
      iacc=0
      istop=1
      itype=3

      call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1     iops1,iops2, nx, ciopso1, iacc, istop, rops2,
     1     ioprsw(k), cidvri)
      iopsou(1,k) = iops1
      if(iopS1.gt.0) then
        iopSouR(k)=itype
        isS=idvsta(iops1)
      endif          
c
c ---------------------------------------------------------
c               d2. Find source 1 a diversion water right (13)
c		    Note ion=0 leaves the original water right on
c		         istop=0 Stop if not found
c		         istop=1 Do not Stop if not found
c                        iacc=0 do not check account (iops2)
      if(iops1.eq.0) then
c       write(nlog,*) ' Oprinp; Get Source Div Right'
c
c rrb 2006/11/27; Tie on/off to iopsou(2
        ion=iopsou(2,k)        
        iacc=0
        istop=1
        itype=13

        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1,iops2, nx, ciopso1, iacc, istop, rops2,
     1       ioprsw(k), cidvri)
        
c
c		Set Source 1 and type (iopdesr = 1 = diversion water right
        if(iops1.gt.0) then
          iopSouR(k)=itype
          iopsou(1,k) = -iops1
c
c rrb 2008/09/26; Correction            
cx        iopDesr(k) = 1            
          iopSouR(k) = 13            
c
c rrb 2007/03/21; Set diversion and stream location for testing
          ndS=idivco(1,iops1)
          isS=idvsta(ndS)
        endif  
      endif  
c
c ---------------------------------------------------------
c               d3. Find source 1 a reservoir water right (12)
c                   Note ion=0 leaves the original water right on
c                   iacc=0 do not check account (iops2)
c                   istop=0 Stop if not found
c                   istop=1 Do not Stop if not found
c
      if(iops1.eq.0) then
c
c rrb 2006/11/27; Tie on/off to iopsou(2
        ion=iopsou(2,k)        
        iacc=0
        istop=0
        itype=12
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1,iops2, nx, ciopso1, iacc, istop, rops2,
     1       ioprsw(k), cidvri)
c
c               Set Source 1 and type 
c               (iopdesr = -1 = reservoir water right
        if(iops1.gt.0) then
          iopSouR(k)=itype          
          iopsou(1,k) = -iops1
          iopSouR(k) = itype           
c
c rrb 2007/03/21; Set reservoir and stream location for testing
          ndS=iresco(1,iops1)
          isS=irssta(ndS)
        endif  
      endif  
c
c ---------------------------------------------------------
c               e. Find source 2 the location where water is 
c                   diverted. Note:
c		                ion=0 leaves the original water right on
c                   iacc=1 Check the account variable (iops2) > 0
c		                iacc=0 Do not check the account variable
c                   itype=3 a diversion
c                   istop = 1 Do not Stop if not found
      if(NAs2.eq.1) then        
        iopsou(3,k)=0
        isA=0
      else 
c
c		Diversion location          
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found

        ion=1
        iacc=0
        istop=1
        itype=3
c
c		Diversion location     
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
c			itype=3 Diversion
        
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1,iops2, nx, ciopso2, iacc, istop, rops2,
     1       ioprsw(k), cidvri)
c
c rrb 2008/08/06; Revise to allow iops1=0     
        if(iops1.gt.0) then
          iopsou(3,k) = -iops1
          isA=idvsta(iops1)
        endif
c
c		Reservoir location     
        if(iops1.eq.0) then      
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
c			itype=2 reservoir
          istop=0          
          itype=2
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         iops1,iops2, nx, ciopso2, iacc, istop, rops2,
     1         ioprsw(k), cidvri)
          iopsou(3,k) = iops1     
          isA=irssta(iops1)
        endif
      endif  
        
c
c ------------------------------------------------------------------
c               f. Find destination reuse plan named Creuse, if any, 
c rrb 2006/12/27; Allow creuse to be used to store carrier losses 
c                 that goto a plan
      if(iout45X.eq.1) write(nlog,*) ' Oprinp; Reading creuse ',creuse
      iacc=0
      ion=-1
      istop=0
      ireuse1=0        
      if(NAuse.eq.0) then                
        call oprFind(ityopr(k), 7, idumc,k,ion,iprinto,
     1       ireuse1,iops2, nx, creuse, iacc, istop, rops2,
     1       ioprsw(k), cidvri)     
      endif
      
c
c -------------------------------------------------------
c		            g. Check proper type of plan 
c                  Note iplntyp 8 is recharge
c                  Note iplntyp 14 is a WWSP Supply plan
      if (ireuse1.gt.0) then
        ireuse(k)=ireuse1             
        iok=1
c
c		If the destination is a reservoir
c             the plan should be types 8 (recharge)
        if(iplntyp(ireuse1).eq.8) iok=0
c
c		If the destination is a WWSP Supply plan 
c             the plan should be type 14 (WWSP)
        if(iplntyp(ireuse1).eq.14) iok=0

        if(iok.eq.1) then
c
c rrb 2019/05/17; Update
cx        write(nlog,1255) ityopr(k),cidvri, ciopde, creuse,
cx   1       iplntyp(ireuse1)
     
          if(iplntyp(ireuse1).eq.15) then
              write(nlog,12551) ityopr(k),cidvri, creuse, 
     1          iplntyp(ireuse1)
          else
            write(nlog,1255) ityopr(k),cidvri, ciopde, creuse,
     1         iplntyp(ireuse1)
          endif     
          goto 9999
        endif          
      endif  
c
c
c ---------------------------------------------------------
c		            h. When the source is a diversion water right
c                    iopsour(k)=13
c                    Check if the carrier is the same structure 
c                    as the supply
      ndc=0
c
      if(iout45x.eq.1) then
        write(nlog,*) ' Oprinp; Reading source water right'
      endif

cx    if(iopdesr(k).ge.0) then
      if(iopSour(k).eq.13) then
        if(iopsou(1,k).lt.0) then
          nd=-1*iopsou(1,k)
          ndc=idivco(1,nd)
        else
          ndc=iopsou(1,k)
        endif
c
c		Require carrier 1 be at at the destination location
        nc=intern(k,1)
c
c rrb 2020/02/24; Allow a type 45 to operate with no carrier
cx        if(nc.ne.ndc) then
cx          write(nlog,1217) cidvri, ityopr(k), ciopso1, ciopde, ciopso2 
cx          goto 9999
cx        endif
c
c ---------------------------------------------------------
c		            i. When the source is a diversion water right  
c                    (iopSour=13)
c                    Check if the source is the same structure 
c                    Note OK if source 2 (iopsou(3,k).ne.0 to indicate
c		                 the right is administered at a different location
c		                 or there are intermediate carriers
c rrb 2007/11/19; Allow if there are intermediate carriers          
c       if(iopdes(1,k).eq.ndc .and. iopsou(3,k).eq.0) then 
c
c rrb 2020/02/24; Allow a diversion right source that is not a carrier         
cx        if(iopdes(1,k).eq.ndc .and. iopsou(3,k).eq.0 .and. 
cx     1     nc.eq.0) then
cx          write(nlog,1214) cidvri, ityopr(k),
cx     1      ciopso1, cdivid(ndc), ciopde
cx          goto 9999
cx        endif
      endif    
c
c ---------------------------------------------------------
c rrb 2008/08/06; Add a new check
c		            j. Check if the diversion location (source 2) is
c		               a ditch (iopsou(3,k) < 0) it must be the
c		               source (ndS) or a carrier (cntern)
      if(iopsou(3,k).lt. 0) then
        iok=0
        if(ndS.eq.isA) iok=1
        
        do i=1,10
          if (cntern(i).eq.ciopso2) iok=1
        end do  
        
        if(iok.eq.0) then
          write(nlog,1215) cidvri, ityopr(k), ciopso2
          goto 9999
        endif
      endif
        
c
c ---------------------------------------------------------
c 		            j. Print warning about admin number
      if(iwarnr .gt. 0) then
        if(iprinta.eq.0) write(ntmp,728)
        iprinta=iprinta+1
        write(ntmp,729) iprinta, ityopr(k), cidvri, ciopso1,
     1    ropsrc, ropnk1, ropnk(k) 
      endif
c
c ---------------------------------------------------------
c 		            k. Print warning and stop regarding carrier loss
c rrb 2008/09/26; Correction
      if(Oprloss(k).lt.smalln .or. Oprloss(k).gt.100+small) then
        write(nlog,739) cidvri, ityopr(k), Oprloss(k), cdesTyp
        goto 9999
      endif
cxc
cxc ---------------------------------------------------------
cxc		            l. Set Shared Water Right indicator (iopsou(5,k)=l2)
cxc		               when Oprlimit(k) < 0
cxc       write(nlog,*) ' Oprinp; k, Oprlimit = ', k, OPrlimit(k)
cx        if(ioprlim(k).gt.0) then
cx          iopsou(5,k)=0 
cx        else
cx          iopsou(5,k)=k
cx          do i=1,12
cx            oprmax(k,i)=-1.0
cx          end do  
cx        endif
cxc        
cxc ---------------------------------------------------------        
cxc		            m. For shared water 
cxc		                 Reset subsequent water right shares (2-n)
cxc                    to the water right with same source and admin #
cx        k2=k
cx        if(oprlimit(k).lt.smalln) then
cx          do i=1,k2
cx            if(ityopr(i).eq.45 .and. iopsou(1,i) .eq. iopsou(1,k)) then
cx              if(i.ne.k) then
cx                iopsou(5,k) = i
cx                ropnk1 =  ropnk(k)- ropnk(i) 
cx                write(nlog,*) ' Oprinp; Shared water',i, k, iopsou(5,k)
cx                if(ropnk1 .gt. small) then
cx                  write(nlog,738) ityopr(k), cidvri, oprlimit(k),
cx     1              corid(i), ropnk(i),  ropnk(k)
cx                  goto 9999
cx                endif 
cx              endif  
cx            endif
cx          end do    
cx        endif
cx        oprlimit(k)=9999.
cxc
cxc rrb 2008/03/12; Correction 0 is the default        
cxc       ioprlim(k)=9999
cx        ioprlim(k)=0
cxc
cxc
cxc ---------------------------------------------------------
c
c rrb 2009/02/04; Move above to section a31 & a32 for consistency 
c		    with other operating rules          
cxc rrb 2008/08/06; Set default data related to having water sharing
cxc		  turned off
cx          if(ioprlim(k).lt.0) then
cx            write(nlog, 1218) cidvri, ityopr(k), oprlimit(k)
cx            goto 9999
cx          endif
cx          iopsou(5,k)=0
cx          oprlimit(k)=9999.
cx          ioprlim(k)=0
cx          do i=1,12
cx            oprmax(k,i)=9999.
cx          end do  
c ---------------------------------------------------------
c rrb 2007/03/21;
c 		            n. Check the Admin Location and Diversion Point
c		                 For a Diversion Destination the Admin point
c		                 must be the source location.
c		                 For a Reservoir Destination the Admin point
c		                 must be the source or destination location
c		                 (e.g. not an intermediate point)
      if(isA.gt.0) then
        iok=0
c
c		Diversion Destination
        if(iopdes(1,k).gt.0 .and. isA.eq.isS) iok=1
c
c		Reservoir Destination          
        if(iopdes(1,k).lt.0) then
cx          if(isA.eq.isD) iok=1
cx          if(isA.eq.isS) iok=1
          iok=1
        endif
                             
        if(iok.eq.0) then
          write(nlog,736) ityopr(k), cidvri, ciopde, ciopso2
          goto 9999
        endif
      endif
c
c ---------------------------------------------------------
c rrb 2007/03/24;
c 		            o. Check the miscellanesous limit and water right 
c		                 control (iopsou(2,k) are set properly
c		                 For a diversion and a rservoir
c     write(nlog,*) ' Oprinp; Check Mis limits ', ioprlim(k),
c    1  iopDesr(k), iopsou(2,k) 
      if(ioprlim(k).ne.0) then
        iok=0
        if(ioprlim(k).lt.0) iok=1
        if(ioprlim(k).eq.2 .and. iopDesr(k).eq.2 .and. 
     1    iopsou(2,k).eq.1) iok=1
        if(ioprlim(k).eq.3 .and. iopDesr(k).eq.3 .and. 
     1    iopsou(2,k).eq.1) iok=1
c
c rrb 2009/03/02; Test with iopsou(2,k) left on     
cx      if(iok.eq.0) goto 2048
      endif
        
c
c ---------------------------------------------------------
c rrb 2007/03/24;
c 		            p. Print warning if the water right ownership is = zero
      OprPct(k)=float(iopsou(4,k))
      if(iopsou(4,k).le.1 .or. iopsou(4,k).gt.100) then
cx      write(nlog,737) ityopr(k), cidvri, iopsou(4,k)
        iopsou(4,k)=100
        OprPct(k)=float(iopsou(4,k))
c       goto 9999
      endif
c
c ---------------------------------------------------------
c rrb 2014-07-29
c 		            q. Print warning if right ownership is not 100%
      OprPct(k)=float(iopsou(4,k))
      if(OprPct(k). le.99.9) then
        write(nlog,740) ityopr(k), cidvri, iopsou(4,k)
        iopsou(4,k)=100
        OprPct(k)=float(iopsou(4,k))
        goto 9999
      endif
c ---------------------------------------------------------
c rrb 2018-10-19; 
c                 r. Check if ioprlim(k) is set properly for a diversion 
c                    to irrigate
      if(ioprlim(k).eq.6 .and. iopdesr(k).ne.3) then
        write(nlog,742) ityopr(k), cidvri, ioprlim(k)
        goto 9999
      endif
c
c ---------------------------------------------------------
c rrb 2020/01/24; 
c                 s. Check Spill Order data when ioprlim(k)=7   
c                    Note iopdes(1,k) is < 0 since its a reservoir  
      if(ioprlim(k).eq.7) then             
        iok=0
        if(iopsou(5,k) .ne. iabs(iopdes(1,k))) iok=1 
        if(iopsou(6,k) .lt. 0) iok=1
        if(iopsou(6,k) .eq. iopdes(2,k)) iok=1  
c
c rrb 2020-02-23; additional Checks
        if(iopsouR(k) .ne. 12) iok=1
        if(iopdesr(k) .ne. 2) iok=1              
        if(iok.eq.1) then        
          write(nlog,2024) cidvri, ITYOPR(K), oprlimit(k), ioprlim(k), 
     1      ciopso1, ciopde,
     1      ciopde, iabs(iopdes(1,k)), iopdes(2,k), 
     1      cx,     iopsou(5,k), iopsou(6,k)
          goto 9999
        endif
      endif
      
c
c ---------------------------------------------------------
c		r. Detailed output
      iout45=0
      if(iout45.eq.1) then     
          call OprinOut(nlog, maxopr, k, 
     1      ityopr(k), cidvri, 
     1      ciopde, iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k),iopsou(2,k),
     1      ciopso2, iopsou(3,k),iopsou(4,k), creuse, ireuse1,
     1      oprloss(k), oprlimit(k), iopSou(5,k), 
     1      iopSou(6,k),iopsou(7,k),     
     1      cdivtyp(k), intern, cntern, cAssoc, cAssoc2, cx2)
          write(nlog,*) (imonsw(k,im),im=1,12)
 
       endif  
      goto 1190
        
c _________________________________________________________
c
c               Type 46; Multiple Ownership
 1046   continue 
c rrb 2007/08/20;         
c               Type 46; Multiple Ownership
c                source 1 (iopsou(1,k) = Accounting Plan
c		             destination (iopdes(1,k)= Accounting Plan
c		             Oprlimit number of destinations (max=10)
c                  ion=1 means turn off opr right if right is off
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
cr      write(nlog,*) '  Oprinp; Type 29'
        istop=0
        nr=0
        np1=0
        np2=0
        
c
c ---------------------------------------------------------
c rrb 2020/03/01; a. Require the destination to be Multiple; even if 
c                 just one destination is provided to simplify logic.
        imult=0
        if(ciopde(1:8).eq.'Multiple') then
          imult=1
        else
          write(nlog,1292) ityopr(k),cidvri
          goto 9999
        endif 
cxc
cxc ---------------------------------------------------------
cxc               b. Read monthly constraints 
cxc
cxc rrb 2018/08/28; Correction, cannot read monthly on/off unless
cxc                 Multiple is specified as a destination
cxc                 Note if idumc is 0, Oprfind will return w/o an error
cx        imult=0
cx        if(ciopde(1:8).eq.'Multiple') imult=1
cx        
cx        if(imult.eq.1) then 
cx
        idumc=ifix(dumc)
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,ix, ix,
     1                 nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
cx
cx      endif
cxc
cxc rrb 2019/04/29; Warn if monthly provided and not Multiple
cx        if(imult.eq.0 .and. idumc.gt.0) then
cx          write(nlog,*) '   Oprinp; ityopr(k), imult, idumc', 
cx     1                              ityopr(k), imult, idumc
cx          write(nlog,1291) ityopr(k),cidvri
cx          goto 9999
cx        endif 
cx
c
c _________________________________________________________
c               c. Find Source 1 a plan (type 7)
c		                Note itype =7 for a plan
c                        istop=0 Stop if not found
c		                     istop=1 Do not Stop if not found
        itype=7
        istop=0
        iacc=1
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1,iopsou(2,k), nx, ciopso1, iacc, istop, rops2,
     1       ioprsw(k), cidvri)
c
        iopSouR(k)=itype
        iopsou(1,k)=iops1
        iopsou(2,k)=0
c
c ---------------------------------------------------------
c               d. Check the proper plan type is specified
c
c rrb 2015/02/03; Allow a type 13 Changed Water Right Plan
c rrb 2018/08/12; Allow a type 14 WWSP Supply Plan (type 14)
c rrb 2019/05/26; Allow a type 15 WWSP User Plan (type 15)
cx      if(iplntyp(iops1).ne.11) then
c
cx      iok=1
cx      if(iplntyp(iops1).eq.11 .or. iplntyp(iops1).eq.13) iok=0
        
        iok=1
        if(iplntyp(iops1).eq.11 .or. iplntyp(iops1).eq.13 .or.
     1     iplntyp(iops1).eq.14 .or. iplntyp(iops1).eq.15) iok=0
        
        if(iok.eq.1) then
c
c rrb 2019/05/17; Update          
cx          write(nlog,1272) ityopr(k),cidvri, 'Destination',cx, 
cx   1                       iplntyp(iopd1), 11, 13, 15
cx
          if(iplntyp(iops1).eq.14 .or. iplntyp(iops1).eq.15) then
            write(nlog,1255) ityopr(k),cidvri, ciopde, creuse,
     1       iplntyp(ireuse1)
         else
            write(nlog,1272) ityopr(k),cidvri, 'Source',ciopso1, 
     1                       iplntyp(iops1), 11, 13, 14, 15
          endif     
          goto 9999
        endif
        
c
c
c ---------------------------------------------------------
c rrb 2019/07/28; 
c               e. Set iwwplan as an indicator for Bomsec to know
c                   a WWSP-Supply (type 14) Plan has been specified 
c                   and the month used in Bomsec.f to initialize key 
c                   WWSP variables.  
c                   Also warn if more than 1 WWSP-Supply (14) plan is
c                   specified since code has not been tested with
c                   more than 1.
        if(iplntyp(iops1).eq.14) then
          iok=0
c          
          do mon=2,12
            if(imonsw(k,mon).gt.0) then
              if(iwwPlan.gt.0) iok=1 
c              
              iwwPlan=mon
              if(iok.eq.1) then 
                write(nlog,12552) ityopr(k),cidvri, ciopso1
                goto 9999
              endif
            endif
          end do
        endif
c
c ---------------------------------------------------------
c rrb 2019/07/28; Warn if user tried to operate for less than
c                 a full month
c
c rrb 2020/07/28; Correction (add do loop)
cx        if(imonsw(k,mon).lt.0 .or. imonsw(k,mon).gt.1) then
cx          write(nlog,12553) ityopr(k),cidvri, 
cx     1                      xmonam(mon),imonsw(k,mon)
cx          goto 9999
cx        endif
cx
        do mon=1,12
          if(imonsw(k,mon).lt.0 .or. imonsw(k,mon).gt.1) then
            write(nlog,12553) ityopr(k),cidvri, 
     1                        xmonam(mon),imonsw(k,mon)
            goto 9999
          endif
        end do
c
c _________________________________________________________
c               f1. Read destination plans (type 7)
c		                 Note itype =7 for a plan
c                    istop=0 Stop if not found
c		                 istop=1 Do not Stop if not found
        if(imult.eq.0) then
          backspace(55)
        endif
        
        ndes=ioprlim(k)
c       write(nlog,*) ' Oprinp; Type 46 ndes = ', ndes
c rrb 2011/05/23; revise to allow up to 10 owners
cx      if(ndes.lt.0 .or. ndes.gt.5) then
c
c rrb 2018/04/08; Correction to fix water balance for South Platte to 
c                 allow Divmulti to include up to 20 accounts)
cx      if(ndes.lt.0 .or. ndes.gt.maxopr2) then
        if(ndes.lt.0 .or. ndes.gt.maxopr2/2) then
          write(nlog,1271) cidvri, ityopr(k), ndes
          goto 9999
        endif
c        
c ---------------------------------------------------------
c 		          f2; Loop for number of destinations
        sum=0.0
        n1=0
        n2=0
        do n=1,ndes
          n1=n2+1
          n2=n1+1
c          
c ---------------------------------------------------------
c		            f3. Read and set destination plan and %
c			               itype=25 Read destination Plan
c                    istop=0 Stop if not found
c		                 istop=1 Do not Stop if not found
c rrb 2018/08/28; Revise to read monthly on/off above and allow
c                 free format read
 250      continue
          if(imult.eq.0) then
            read(55,'(a1, 80x, a12, f8.0)',end=2040,err=2040) rec1,cx,rx
          else
            read(55,*, end=2040,err=2040) cx,rx
            rec1=' '
          endif
          
          if(iecho.eq.1 .and. n.gt.1) then
            write(nchk,'(a1,80x,a12,f8.0)') rec1, cx, rx
          endif  
          
          if(iout.eq.1) write(nlog,'(a1,80x,a12,f8.0)') rec1, cx, rx 
          if(rec1.eq.'#') goto 250

          itype=7
          istop=0
          iacc=1
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iopd1,iopd2, nx, cx, iacc, istop, rops2,ioprsw(k), cidvri)
          iopdes(n1,k)=iopd1
          iopdesr(k)=7
          
c          write(nlog,*) ' '
c          write(nlog,*) ' Oprinp; n1, k, cx, iopd1'
c          write(nlog,*) ' Oprinp;', n1, k, cx, iopd1
          
          ropdes(k,n2)=rx
          iopd2=ifix(rx)          
          iopdes(n2,k)=iopd2
c
c rrb 2008/01/02;    
c rrb 2018/09/30 Correction     
cx          ciopsoX(n,k)=ciopso1
cx          ciopdeX(n,k)=cx         
          ciopsoX(n1,k)=ciopso1
          ciopdeX(n1,k)=cx
          
          if(int(pon(iopd1)).ne.0) then
            sum=sum+rx
          endif  
c         write(nlog,*) ' Oprinp; ', n1, iopd1, n2, iopd2
c
c ---------------------------------------------------------
c               f4. Check the proper plan type is specified
c rrb 2015/03/07; Revise to allow a Changed Water Right Plan (type 13)
cx        if(iplntyp(iopd1).ne.11) then
          iok=1
c
c rrb 2018/08/12; Correction and allow a wwsp User plan (type 15)
cx        if(iplntyp(iops1).eq.11 .or. iplntyp(iops1).eq.13) iok=0        
c
c rrb 2019/04/20; Revise to recognize a WWSP Supply (type 14) and a
c                 WWSP User (type 15)
c rrb 2019/05/26; Revise to allow a WWSP Supply (14) and User (15)
cx        if(iplntyp(iopd1).eq.11 .or. iplntyp(iopd1).eq.13 .or.
cx   1       iplntyp(iopd1).eq.14) iok=0
          if(iplntyp(iopd1).eq.11 .or. iplntyp(iopd1).eq.13 .or.
     1       iplntyp(iopd1).eq.14 .or. iplntyp(iopd1).eq.15) iok=0
c     
          if(iok.eq.1) then 
c
c rrb 2019/05/17; Update         
cx          write(nlog,1272) ityopr(k),cidvri, 'Destination',cx, 
cx   1                       iplntyp(iopd1), 11, 13, 15
            if(iplntyp(iopd1).eq.14) then
              write(nlog,12721) ityopr(k),cidvri, 'Destination',cx, 
     1                         iplntyp(iopd1), 'Destination', 15
            else
              write(nlog,1272) ityopr(k),cidvri, 'Destination',cx, 
     1                         iplntyp(iopd1), 11, 13, 15, 0
            endif
            goto 9999
          endif
c        
c ---------------------------------------------------------
c               f5. Check that the source plan is upstream 
c                  of the destination plan
c	                 e.g. check the destination (idcdD)
c                  downstream of  source (iss) 
          idcdD=ipsta(iopd1)
          iops1=iopsou(1,k)
          iss=ipsta(iops1)
          ndns=ndnnod(iss)
          csource=cstaid(iss)
          cdest=cstaid(idcdD)
          
          call oprdown(nlog, maxsta, ndns, iss, idcdD, idncod,
     1       cidvri, csource, cdest)         
        enddo
c
c ---------------------------------------------------------
c		            g. Check total percent
        if(abs(sum-100.0).gt.small) then
          write(nlog,1273) ityopr(k),cidvri, sum
          goto 9999
        endif
c
c ---------------------------------------------------------
c		            h. Check if the source is a WWSP Plan (type 14) that
c      monthly data is provided and the system is only allowed
c        to operate once month per year.
c
       itype = iopSouR(k)
c       
       if(itype.eq.7) then
         ip=iopsou(1,k)
         ip1=iplntyp(ip)
c
         if(ip1.eq.14) then
           imx=0
           do i=1,12
             imx=imx+imonsw(k,i)  
           end do
                    
           if(imx.ne.1) then
             write(nlog,1287) ityopr(k),cidvri
             goto 9999
           endif  
         endif
       endif      
c
c ---------------------------------------------------------
c		            i. Detailed output
     
        iout46=0  
c
c rrb 2018/03/27; Test
cx      if(cidvri.eq.'060580_CH.02') iout46=1
        if(iout46.eq.1) then
          n1=0
          n2=0
          do n=1,ndes
            n1=n2+1
            n2=n1+1
            ciopde=pid(iopdes(n1,k))
cx            write(nlog,*) ' '
cx            write(nlog,*) '  Oprinp; Type 46 ', n, n1, n2
c            
          call OprinOut(nlog, maxopr, k, 
     1      ityopr(k), cidvri, 
     1      ciopde, iopdes(n1,k), iopdes(n2,k), 
     1      ciopso1, iopSou(1,k),iopsou(2,k),
     1      ciopso2, iopsou(n1+2,k),iopsou(n2+2,k), 
     1      creuse, ireuse1,
     1      oprloss(k), oprlimit(k), iopSou(5,k), 
     1      iopSou(6,k),iopsou(7,k),     
     1      cdivtyp(k), intern, cntern, cAssoc, cAssoc2, cx2)  
          end do
        endif

        goto 1190
c _________________________________________________________
c
c              Type 47 Administrative Plan Limits
 1047   continue 
c rrb 2007/10/12;         
c              source 1 (iopsou(1,k)) = Accounting Plan
c		           destination (iopdes(1,k)= NA
c              ion=1 means turn off opr right if right is off
c              Note istop=0 Stop if not found
c		      istop=1 Do not Stop if not found
c       write(nlog,*) '  Oprinp; Type 47'
        istop=0
        nr=0
        np=0
        np1=0
        np2=0
        
        idumc=ifix(dumc)
c
c ---------------------------------------------------------
c               a1. Read monthly constraints 
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,ix, ix,
     1               nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               b1. Read Monthly plus Annual maxima (oprmax)
        if(ioprlim(k).eq.1) then
          call oprFind(ityopr(k), 22, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
        endif
c
c _________________________________________________________
c               c1. Find Source a plan (type 7)
c		   Note itype =7 for a plan
c                     istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
c		        iacc=1  Allow source 2 to represent the
c			         month when limits are reset
        itype=7
        istop=0
        iacc=0
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1,iopsou(2,k), nx, ciopso1, iacc, istop, rops2,
     1       ioprsw(k), cidvri)
c
        iopSouR(k)=itype        
        iopsou(1,k)=iops1
c
c rrb 2009/01/15; Revise to allow iopsou(2,k) to represent the month
c		    when the operational limits are reset         
        if(iopsou(2,k).lt.1 .or. iopsou(2,k).gt.12) then
          goto 2044
        endif
        
c
c ---------------------------------------------------------
c               c2. Check the proper plan type is specified
        if(iplntyp(iops1).ne.12) then
          write(nlog,1272) ityopr(k),cidvri, 'Source 1',ciopso1,
     1                     iplntyp(iops1), 12, 0, 0, 0
          goto 9999
        endif
c
c ---------------------------------------------------------
c		e. Detailed output
     
        iout47=0
        if(iout47.eq.1) then
          write(nlog,2020) ityopr(k), cidvri, ityopr(k),
     1      ciopde,  iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k), iopsou(2,k),
     1      ciopso2, iopsou(3,k), iopsou(4,k), creuse, ireuse1,
     1      oprlimit(k), iopSou(5,k),
     1      cdivtyp(k), iopdesR(k)
        endif

        goto 1190
c _________________________________________________________
c
c         
 1048   continue 
c               Type 48; Reservoir or Recharge Plan to a Plan Direct
c rrb 01/06/20; 
c                destination = a river ID
c                source 1 (iopsou(1,k) = a reservoir
c                source 2 (iopsou(3,k) = a plan ID
c                  ion=1 means turn off opr right if right is off
        ion=1
        idumc=ifix(dumc)
c
c ---------------------------------------------------------
c               a. Read monthly constraints 
        istop=0
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               a2. Read intervening structures without loss
c       write(nlog,*) ' Oprinp; oprloss(k)', oprloss(k)
        ioprloss=int(oprloss(k))
        if(ioprloss.eq.0) then
          istop=0
          itype=21
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
        endif
c        
c ---------------------------------------------------------

c               a3. Read intervening structures plus loss (23)
c       write(nlog,*) 
        if(ioprloss.ne.0) then
          istop=0
          itype=23
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
        endif          
     
     
c
c ---------------------------------------------------------
c               b1. Find the destination Plan (str type 7), 
c                   where the plan type must be a T&C (1), 
c                   Well Aug (2), or Special (10)
c		               Note: istop = 0 stop if not found
c		        
        istop=0
c       write(nlog,*) ' Oprinp; ciopde = ', ciopde
        call oprFind(ityopr(k), 7, idumc,k,ion,iprinto,
     1       iops1, iopdes(2,k), nx,ciopde, 0, 
     1       istop, rops2,ioprsw(k), cidvri)
        iopdes(1,k) = iops1 
        idcdD=ipsta(iops1)
        iopdesr(k)=7
c
c ---------------------------------------------------------
c		b2. Find the secondary destination plan if any        
c		   Note only checking for existence
c		   Note: istop = 0 stop if not found
        if(NAuse.eq.0) then                
c         write(nlog,*) ' Oprinp; ciopde = ', creuse
          istop=0        
          call oprFind(ityopr(k), 7, idumc,k,ion,iprinto,
     1         ireuse1,iops2, nx, creuse, iacc, 
     1         istop, rops2,ioprsw(k), cidvri)     
          iopdes(3,k)=ireuse1             
          ciopdeX2(k)=creuse
          idcdD2=ipsta(ireuse1)
        endif
                
c
c ---------------------------------------------------------
c               c1. Find source 1 a reservoir (type 2) 
c		   Note: istop = 1 OK if not found
        iP1=0
        istop=1
        itype=2
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iopsou(1,k),iopsou(2,k),nx, ciopso1, 1, 
     1       istop, rops2,ioprsw(k), cidvri)
        if(iopsou(1,k).gt.0) then
          iopSouR(k)=itype
          iscdS=irssta(iopsou(1,k))
        endif  
     
c
c ---------------------------------------------------------
c               c2. If no reservoir, find source 1 a plan (type 7)
c		   Note: istop = 0 stop if not found
        np=0
        if(iopsou(1,k).eq.0) then
          istop=0
          itype=7
          iacc=1
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         iopsou(1,k),iops2, nx, ciopso1, iacc, 
     1         istop, rops2,ioprsw(k), cidvri)
     
          iopSouR(k)=itype     
          np=nx
          iopsou(1,k)=-nx
          iP1=iPlnTyp(np)          
          iscdS=ipsta(nx)          
        endif  
c
c ---------------------------------------------------------
c               d. Find source 2 a reservoir (type 2) 
c          	   Note should only occur if source 1 is a 
c                  Note itype=2 for a reservoir
c		   Recharge (type 8) plan 
c		   and source 2 is not NA
c   		   Note: istop = 0 stop if not found
c
        if(np.gt.0 .and. naS2.eq.0) then
          istop=0
cx          ipRes1=0
          call oprFind(ityopr(k), 2, idumc,k,ion,iprinto,
     1         iopsou(3,k),iopsou(4,k), nx, ciopso2, 1, 
     1         istop, rops2,ioprsw(k), cidvri)
cx          iPRes(nx)=np
cx          iPRes1=nx
        endif  
c
c		Check source 2 data was provided when source 1
c		is a recharge plan (type 8)
c
c rrb 2008/03/18; Revise recharge can come from a canal
cx        if(iopsou(1,k).lt.0 .and. iP1.eq.8 .and. iPRes1.eq.0) then
cx            write(nlog,1278) ityopr(k),cidvri, ciopso1, iP1, ciopso2
cx            goto 9999
cx        endif
        
c
c ---------------------------------------------------------
c               f. Check that the source reservoir or Plan is upstream 
c                  of the destination stream ID
c	           (e.g. determine if the destination node (iscdd)
c                  is downstream of the source node (iscds))
        ndns=ndnnod(iscdS)
        iss=iscds
        csource=cstaid(iss)    
c
c rrb 2008/12/16; correction        
cx      cdest=cstaid(iscdS)
        cdest=cstaid(idcdD)
                  
        call oprdown(nlog, maxsta, ndns, iscdS, idcdD, idncod, 
     1       cidvri, csource, cdest)
c        
c
c ---------------------------------------------------------
c		g. If Source 1 is a Plan
c		   Check if it is the proper type
c                  Note iplntyp 3 & 5 are reservoir, 
c                       iplntyp 4 & 6 are diversion
        if (np.gt.0) then
c
c ---------------------------------------------------------
c rrb 2020/11/29; Update check for proper source type
cx        if(iP1.le.2) then
          if(iP1.eq.4  .or. iP1.eq.6 .or. iP1.eq.8 .or. 
     1       iP1.eq.11 .or. iP1.eq.13) then 
          else 
            ioprsw(K)=0
            write(nlog,12561) ityopr(k),cidvri, ciopso1, iP1
cx          goto 9999
          endif  
c
c rrb 2008/03/18; Revise recharge can come from a canal          
cx          if(iP1.eq.8 .and. iPRes1.eq.0) then
cx            write(nlog,1257) ityopr(k),cidvri, ciopso1, iP1,
cx     1        ciopso2
cx            goto 9999          
cx          endif
        endif   
c
c ---------------------------------------------------------
c		l. Detailed output
c     
        iout48=0
        if(iout48.eq.1) then
          call OprinOut(nlog, maxopr, k, 
     1      ityopr(k), cidvri, 
     1      ciopde, iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k),iopsou(2,k),
     1      ciopso2, iopsou(3,k),iopsou(4,k), creuse, ireuse1,
     1      oprloss(k), oprlimit(k), iopSou(5,k),
     1      iopSou(6,k),iopsou(7,k),     
     1      cdivtyp(k), intern, cntern, cAssoc, cAssoc2, cAssoc3)
        endif 
         
        iout48=0
        if(iout48.eq.2) then
          nout=nout+1
          if(nout.eq.1) then
            write(nlog,*)
     1      '   # Type ID          Dest         Type Plan',
     1      ' Source       Type Plan'
          endif
          
          iptypD=0
          iptypS=0
          iopdes1=abs(iopdes(1,k))
          iopsou1=abs(iopsou(1,k))           
          if(iopdesr(k).eq.7) iptypD=iPlnTyp(iopdes1)
          if(iopsour(k).eq.7) iptypS=iPlnTyp(iopsou1)
          
          write(nlog,'(2i5, 1x,2a12,2i5, 1x,a12,2i5)') 
     1      nout, ityopr(k), cidvri, ciopde, iopdesr(k), iptypD, 
     1      ciopso1, iopsour(k), iptypS
        endif
c
        goto 1190
c _________________________________________________________
c
c         
 1049   continue 
c               Type 49; Reservoir or Recharge Plan to a Plan by Exchange
c rrb 01/06/20; 
c                destination = a river ID
c                source 1 (iopsou(1,k) = a reservoir
c                source 2 (iopsou(3,k) = a plan ID
c                ion=1 means turn off opr right if right is off
        ion=1
        np=0
        idumc=ifix(dumc)
c
c ---------------------------------------------------------
c               a. Read monthly constraints 
        istop=0
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               a2. Read intervening structures without loss
c       write(nlog,*) ' Oprinp; oprloss(k)', oprloss(k)
        ioprloss=int(oprloss(k))
        if(ioprloss.eq.0) then
          istop=0
          itype=21
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
        endif
c        
c ---------------------------------------------------------

c               a3. Read intervening structures plus loss (23)
c       write(nlog,*) 
        if(ioprloss.ne.0) then
          istop=0
          itype=23
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
        endif          
c
c ---------------------------------------------------------
c              b1. Find the destination, a T&C or Well Aug plan (type 7)
c		   Note: istop = 0 stop if not found
c			 iopdes(1,k) is stored as a negative value
c		        
        istop=0
        call oprFind(ityopr(k), 7, idumc,k,ion,iprinto,
     1       iops1, iopdes(2,k), nx,ciopde, 0, 
     1       istop, rops2,ioprsw(k), cidvri)
        iopdes(1,k) = -iops1 
        idcdD=ipsta(iops1)
        iopdesr(k)=7
c
c ---------------------------------------------------------
c		b2. Find the secondary destination plan if any        
c		   Note only checking for existence
c		   Note: istop = 0 stop if not found
        if(NAuse.eq.0) then                
c         write(nlog,*) ' Oprinp; ciopde = ', creuse
          istop=0        
          call oprFind(ityopr(k), 7, idumc,k,ion,iprinto,
     1         ireuse1,iops2, nx, creuse, iacc, 
     1         istop, rops2,ioprsw(k), cidvri)     
          iopdes(3,k)=ireuse1             
          ciopdeX2(k)=creuse
          idcdD2=ipsta(ireuse1)
        endif
                
c
c ---------------------------------------------------------
c               c1. Find source 1 a reservoir (type 2) 
c		   Note: istop = 1 OK if not found
        iP1=0
        istop=1
        itype=2
        iacc=1
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iopsou(1,k),iopsou(2,k),nx, ciopso1, iacc, 
     1       istop, rops2,ioprsw(k), cidvri)
        if(iopsou(1,k).gt.0) then
          iopSouR(k)=itype
          iscdS=irssta(iopsou(1,k))
        endif
c
c ---------------------------------------------------------
c               c2. If no reservoir, find source 1 a plan (type 7)
c		   Note: istop = 0 stop if not found
        np=0
        if(iopsou(1,k).eq.0) then
          istop=0
          itype=7
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         iopsou(1,k),iops2, nx, ciopso1, 1, 
     1         istop, rops2,ioprsw(k), cidvri)
c
c rrb 2012/02/15; Revise warning
          if(nx.gt.0) then     
            iopSouR(k)=itype
     
            np=nx
            iopsou(1,k)=-nx
            iP1=iPlnTyp(np)          
            iscdS=ipsta(nx)
          endif 
        endif
c
c rrb 2012/02/15; Revise warning
        if(nx.eq.0) then
          write(nlog,721) cidvri, ciopso1, iopsou(2,k)
          goto 9999
        endif  
c
c ---------------------------------------------------------
c               d. Find source 2 a reservoir (type 2) 
c          	   Note should only occur if source 1 is a 
c		   Reservoir Recharge (type 8) plan 
c		   and source 2 is not NA
c   		   Note: istop = 0 stop if not found

        if(np.gt.0 .and. naS2.eq.0) then
          istop=0
          call oprFind(ityopr(k), 2, idumc,k,ion,iprinto,
     1         iopsou(3,k),iopsou(4,k), nx, ciopso2, 1, 
     1         istop, rops2,ioprsw(k), cidvri)
          iscdd=irssta(nx)
        endif  
c
c		Check source 2 data was provided when source 1
c		is a reservoir recharge plan (type 8)
c
c rrb 2008/03/18; Revise recharge can come from a canal
cx        if(iopsou(1,k).lt.0 .and. iP1.eq.8 .and. iPRes1.eq.0) then
cx            write(nlog,1278) ityopr(k),cidvri, ciopso1, iP1, ciopso2
cx            goto 9999
cx        endif
c
c ---------------------------------------------------------
c               f. Check that the source reservoir or Plan is upstream 
c                  of the destination stream ID
c	           (e.g. determine if the destination node (iscdd)
c                  is downstream of the source node (iscds))
cx        npf=-iopdes(1,k)
cx        iscdd=iPsta(npf)
cx        if(iopsou(1,k).gt.0) then
cx          iscds=irssta(iopsou(1,k))
cx        else
cx          iscdS=ipsta(-iopsou(1,k))          
cx        endif  
cx        
cx        ndns=ndnnod(iscdS)
cx        iss=iscds
cx        csource=cstaid(iss)
cx        cdest=cstaid(idcdD)
cx                  
cx        call oprdown(nlog, maxsta, ndns, iss, iscdD, idncod, 
cx     1       cidvri, csource, cdest)
c        
c
c ---------------------------------------------------------
c		            g. If Source 1 is a Plan
c		               Check if it is the proper type
c                  Note iplntyp 3 & 5 are reservoir, 
c                       iplntyp 4 & 6 are diversion
        if (np.gt.0) then
          if(iP1.le.2) then
            write(nlog,1256) ityopr(k),cidvri, ciopso1, iP1
            goto 9999
          endif  
          
c
c rrb 2008/03/18; Revise recharge can come from a canal
cx          if(iP1.eq.8 .and. iPRes1.eq.0) then
cx            write(nlog,1257) ityopr(k),cidvri, ciopso1, iP1,
cx     1        ciopso2
cx            goto 9999          
cx          endif
        endif   
c ______________________________________________________________________
c               h. Find the exchange point (iExPoint)
c                  for the source and destination
        call oprExp(nlog, maxsta, idcdD, iscdS, idncod, ndnnod, 
     1                    iExPoint(k), cidvri)
c ______________________________________________________________________
c		l. Detailed output
        iout49=0
        if(iout49.eq.1) then
          call OprinOut(nlog, maxopr, k, 
     1      ityopr(k), cidvri, 
     1      ciopde, iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k),iopsou(2,k),
     1      ciopso2, iopsou(3,k),iopsou(4,k), creuse, ireuse1,
     1      oprloss(k), oprlimit(k), iopSou(5,k), 
     1      iopSou(6,k),iopsou(7,k),     
     1      cdivtyp(k), intern, cntern, cAssoc, cAssoc2, cAssoc3)
        endif  
        goto 1190
c _________________________________________________________
 1050   continue 
c ______________________________________________________________________
c               Type 50; South Platte Compact Storage
c		   Instream flow tied to a stream gage
c		   Demand = max(0, min(ifa, qindex-q@ifa))
c      ion=1 means turn off opr right if right is off
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        ion=1
        istop=0
        idcdd=0
        iss=0
        idumc=ifix(dumc)
c ______________________________________________________________________
c               a. Read monthly constraints 
        istop=0
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)      
c ______________________________________________________________________
c rr 2006/03/29; Plan destination
c               b. Find the destination, a plan
c		istop=0 Stop if a structure is not found
c		istop=1 Do not stop if a structure is not found
c		itype=7= plan structure
        istop=0
        itype=7
        iacc=1
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         iops1, iopdes(2,k), nx,ciopde, iacc, 
     1         istop, rops2,ioprsw(k), cidvri)
         iopdes(1,k) = iops1
         ndD=iops1
         idcdD=ipsta(ndD) 
         idcdX=idcdD            
         iopDesR(k)=itype
c		Check Plan type is an Admin Plan         
         if(iplntyp(ndD).ne.11) then
           write(nlog,1255) ityopr(k),cidvri, ciopde, creuse, 
     1      iplntyp(ndD)
           goto 9999
         endif   
c ______________________________________________________________________
c               c. Find source 1 an ISF Right
c                  Note itype=0 for a stream ID
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
c		        iacc=0 allows account to be 0 (since 
c                  it is ownership %)
c		        ion=0 leaves the original water right on
c           ion=1 turn off the original right
c			  itype=11 = ISF Right
        itype=11
        ion=1
        iacc=1
        istop=0
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1,iopsou(2,k), nx, ciopso1, 1, istop, rops2,
     1       ioprsw(k), cidvri)
        iopSouR(k)=itype
        iopsou(1,k)=iops1
c ______________________________________________________________________
c		            d. Detailed output
        iout50=0
        if(iout50.eq.1) then
          write(nlog,2020) ityopr(k), cidvri, ityopr(k),
     1      ciopde,  iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k), iopsou(2,k),
     1      ciopso2, iopsou(3,k), iopsou(4,k), creuse, ireuse1,
     1      oprlimit(k), iopSou(5,k),
     1      cdivtyp(k), iopdesR(k)
        endif
        goto 1190
c _________________________________________________________
c
c rrb 2018/07/15; Add Flow Reservoir Control
c
c               Type 51; Flow-Reservoir Control
 1051   continue 
c                Source 1 (iopsou(1,k) = NA
c		             destination (iopdes(1,k)= Reservoir to check
c		             Oprlimit number of destinations (max=10)
c                  ion=1 means turn off opr right if right is off
c		               istop=0 Stop if not found
c		               istop=1 Do not Stop if not found
cr      write(nlog,*) '  Oprinp; Type 51'
        iout=0
        istop=0
        nr=0
        np1=0
        np2=0
c
c ---------------------------------------------------------
c rrb 2020/03/01; a. Require the destination to be Multiple; even if 
c                 just one destination is provided to simplify logic.
        imult=0
        if(ciopde(1:8).eq.'Multiple') then
          imult=1
        else
          write(nlog,1292) ityopr(k),cidvri
          goto 9999
        endif 
c
c ---------------------------------------------------------
c               b. Read monthly constraints 
        idumc=ifix(dumc)
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,ix, ix,
     1               nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c _________________________________________________________
c               c. Find Source 1 a reservoir (type 2)
c                   Note Source account 1 (iopsou(2,k) is used to
c                   initialize project condition where:
c                   project off=0
c                   project on = 1
c                   iacc = 0 do not stop if iopsou(2,k) = 0
c		   Note itype =2 for a plan
c           istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        itype=2
        istop=0
c
c rrb 2018/08/14; Correction allow source 2 to be used fto initialize
        iacc=1
        iacc=0
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1,iopsou(2,k), nx, ciopso1, iacc, istop, rops2,
     1       ioprsw(k), cidvri)
c
        iopSouR(k)=itype
        iopsou(1,k)=iops1
        iopsou(2,k)=iopsou(2,k)
c
c _________________________________________________________
c               d. Read destination reservoirs (type 2)
c		                 Note itype = 2 for a reservoir
c                    istop=0 Stop if not found
c		                 istop=1 Do not Stop if not found
cx      backspace(55)
c   
        ndes=ioprlim(k)
        if(iout.eq.1) write(nlog,*) ' Oprinp; Type 51 ndes = ', ndes
c
        if(ndes.lt.0 .or. ndes.gt.maxopr2/2) then
          write(nlog,1271) cidvri, ityopr(k), ndes
          goto 9999
        endif
c        
c ---------------------------------------------------------
c 		          d1; Loop for number of destinations
        itype=2 
        sum=0.0
        n1=0
        n2=0
        
        do n=1,ndes
          n1=n2+1
          n2=n1+1
c          
c ---------------------------------------------------------
c		            d2. Read and set destination reservoir data
c			               itype=25 Read destination Plan
c                    istop=0 Stop if not found
c		                 istop=1 Do not Stop if not found
 251      read(55,'(a1)',end=2040,err=2040) rec1
          if(rec1.eq.'#') goto 251
          
          backspace (55)
          read(55,*,end=2040,err=2040) cx, ix
          if(iecho.eq.1 .and. n.gt.1) then
            write(nchk,*) cx, rx
          endif    
                 
          if(iout.eq.1) write(nlog,*) cx, ix                                                
          iout=0
          
          iopd2=ix         

          itype=2
          istop=0
          iacc=1
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iopd1,iopd2, nx, cx, iacc, istop, rops2,ioprsw(k), cidvri)
     
          iopdes(n1,k)=iopd1
          iopdesr(k)=itype
          
cx        ropdes(k,n2)=rx        
          iopdes(n2,k)=iopd2
c
c               Set the source for this destination         
          ciopsoX(n,k)=ciopso1
          ciopdeX(n,k)=cx
c
c rrb 2018/08/05; 
c               Check the source and destination reservoir are the same
          if(iops1 .ne. iopd1) then
             write(nlog,1283) ityopr(k),cidvri, ciopso1, cx
            goto 9999          
          endif         
c
          if(iout.eq.1) then      
            write(nlog,*) ' '
            write(nlog,*) ' Oprinp; n1, k, cx, iopd1'
            write(nlog,*) ' Oprinp;', n1, k, cx, iopd1
          endif
          
        end do  
c
c ---------------------------------------------------------
c		            e. Detailed output
     
       iout51=0
c
       if(iout51.eq.1) then
          n1=0
          n2=0
          do n=1,ndes
            n1=n2+1
            n2=n1+1
            nr = iopdes(n1,k)
            ciopde=cresid(nr)
            
cx            write(nlog,*) ' '
cx            write(nlog,*) '  Oprinp; Type 51 ', n, n1, n2            
            write(nlog,2023) ityopr(k), cidvri, ityopr(k),
     1        ciopde,  iopdes(n2,k), 
     1        ciopso1, iopSou(1,k), 
     1        creuse, ireuse1,
     1        oprlimit(k), 
     1        cdivtyp(k), iopdesR(k)
     
          end do
        endif
        goto 1190     
c _________________________________________________________
c
c
c rrb 2018/08/12; Multiple Reservoir Bookover (type 52
 1052   continue 
c rrb 2007/08/20;         
c               Type 52; Multiple Reservoir Bookover
c                source 1 (iopsou(1,k) = Reservoir and account
c		             destination (iopdes(1,k)= Reservoir and Plan
c		             Oprlimit number of destinations (max=10)
c
c                  ion=1 means turn off opr right if right is off
c		   Note        istop=0 Stop if not found
c		               istop=1 Do not Stop if not found
c
        iout52=0
cx      write(nlog,*) '  Oprinp; Type 52'
        istop=0
        nr=0
        np1=0
        np2=0
c
c ---------------------------------------------------------
c rrb 2020/03/01; a. Require the destination to be Multiple; even if 
c                 just one destination is provided to simplify logic.
        imult=0
        if(ciopde(1:8).eq.'Multiple') then
          imult=1
        else
          write(nlog,1292) ityopr(k),cidvri
          goto 9999
        endif 
        
c
c ---------------------------------------------------------
c               b. Read monthly constraints 
        idumc=ifix(dumc)
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,ix, ix,
     1               nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c _________________________________________________________
c               c. Find Source 1 a reservoir (type 2)
c		   Note itype =2 for a reservoir
c           istop =0 Stop if not found
c		        istop =1 Do not Stop if not found
        itype=2
        istop=0
        iacc=1
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1,iopsou(2,k), nx, ciopso1, iacc, istop, rops2,
     1       ioprsw(k), cidvri)
c
        iopSouR(k)=itype
        iopsou(1,k)=iops1
        iopsou(2,k)=iopsou(2,k)

c _________________________________________________________
c               d. Read destination reservoirs and accounts (type 2)
c		                 Note itype =7 for a plan
c                    istop=0 Stop if not found
c		                 istop=1 Do not Stop if not found
cx      backspace(55)
        ndes=ioprlim(k)
        if(ndes.lt.0 .or. ndes.gt.maxopr2/2) then
          write(nlog,1271) cidvri, ityopr(k), ndes
          goto 9999
        endif
c        
c ---------------------------------------------------------
c 		          d1; Loop for number of destinations
        sum=0.0
        n1=0
        n2=0
        do n=1,ndes
          n1=n2+1
          n2=n1+1
c          
c ---------------------------------------------------------
c		            d2. Read and set destination res, account & %
c                    Note the destination in the main read
c                    is not used because a % cannot be specified
c
c			               itype=2 Read destination reservoir
c                    istop=0 Stop if not found
c		                 istop=1 Do not Stop if not found
 252      read(55,'(a1)',end=2040,err=2040) rec1
          if(rec1.eq.'#') goto 252
          
          backspace (55)
c
c rrb 2018/11/27; Allow a maximum transfer rate to be specified
c rrb 2018/12/08; Allow a WWSP plan 
c                 Note: cx=reservoir ID, ix=res account, 
c                       rx = % or volume, rx2=max transfer rate
c                       cx2 = NA or WWSP Plan ID
c                 
cx        read(55,*,end=2040,err=2040) cx, ix, rx
          read(55,*,end=2040,err=2040) cx, ix, rx, rx2, cx2
          cAssoc = cx
          
          if(iecho.eq.1 .and. n.gt.1) write(nchk,*) cx, ix, rx, rx2          
          if(iout52.eq.1) write(nlog,*) cx, ix, rx, rx2, cx2
c          
c ---------------------------------------------------------
c               d3. Find cx, a reservoir, and set iopdes
          itype=2
          istop=0
          iacc=ix
          iopd2=ix
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iopd1,iopd2, nx, cx, iacc, istop, rops2,ioprsw(k), cidvri)
     
          iopdes(n1,k)=iopd1
          iopdes(n2,k)=ix
          iopdesr(k)=2

c        
c ---------------------------------------------------------
c               d4. Check that the source reservoir is the same as 
c                   the destination reservoir
          if(iops1.ne.iopd1) then
            write(nlog,1283) ityopr(k),cidvri, ciopso1, cx
            goto 9999          
          endif
c          
c ---------------------------------------------------------
c               d5. Set Percent or volume allocation (ropdes)               
          ropdes(k,n2)=rx          
          sum=sum+rx
          if(iout52.eq.1) then
            write(nlog,*) ' Oprinp; ', n1, iopd1, n2, iopd2
          endif      
c          
c ---------------------------------------------------------
c rrb 2018/11/27; Set a maximum transfer rate 
          ropdes(k,n1)=rx2
c        
c ---------------------------------------------------------
c               Find cx2, a plan, and set ioppln
c			               itype=7 Find a plan
c		                 istop=0 Stop if not found
          itype=7
          istop=0
          iacc=ix
          iopd2=ix
          ioppln(n1,k) = -1
          
cx          rec2=cx2(1:2)        
cx          write(nlog,*) '  Oprinp; type 52 cx2, ',cx2, rec2
          
          if(cx2(1:2).ne.'NA') then
            call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         iopd1,iopd2,nx,cx2,iacc,istop,rops2,ioprsw(k),cidvri)
     
            ioppln(n1,k)=iopd1                      
            ciopdeX(n1,k)=cx2
c       
cx            write(nlog,*) '  Oprinp; type 52 ',
cx     1        iopd1, ityopr(k),cidvri, cx2, iplntyp(iopd1) 
            
            if(iplntyp(iopd1).ne.14) then
              write(nlog,1286) ityopr(k),cidvri, cx2, iplntyp(iopd1) 
              goto 9999
            endif    
          endif
c
c ---------------------------------------------------------
c               End do for number of destinations          
        enddo
c        
c ---------------------------------------------------------
c rrb 2018/11/27; Allow a volumetric limit to be specified
c               c4. Check that the source reservoir is the same as 

        rec12=cdivtyp(k)
        iok=1
        if(rec12.eq.'Percent     ') iok=0
        if(rec12.eq.'Volume      ') iok=0
        if(iok.eq.1) then 
          write(nlog,1258) ityopr(k), cidvri, cdivtyp(k)
          goto 9999
        endif            
c
c ---------------------------------------------------------
c		            e. Check total percent
c
c rrb 2018/11/27; Allow a volumetric limit to be specified
        if(rec12.eq.'Percent     ') then
          if(abs(sum-100.0).gt.small) then
            write(nlog,1273) ityopr(k),cidvri, sum
            goto 9999
          endif
        endif
c
c ---------------------------------------------------------
c		            f. Detailed output
     
        iout52=0  
c
        if(iout52.eq.1) then
          n1=0
          n2=0
          do n=1,ndes
            n1=n2+1
            n2=n1+1
c            
          call OprinOut(nlog, maxopr, k, 
     1      ityopr(k), cidvri, 
     1      ciopde, iopdes(n1,k), iopdes(n2,k), 
     1      ciopso1, iopSou(1,k),iopsou(2,k),
     1      ciopso2, iopsou(n1+2,k),iopsou(n2+2,k), creuse, ireuse1,
     1      oprloss(k), oprlimit(k), iopSou(5,k), 
     1      iopSou(6,k),iopsou(7,k),     
     1      cdivtyp(k), intern, cntern, cAssoc, cAssoc2, cx2)



    
          end do
        endif

        goto 1190 
c _________________________________________________________
c
c rrb 2018/08/14; Add JMartin Storage Winter Water Program Allocation  
c                 DELETE LATER (CURRENTLY NOT USED)    

c               Type 53; JMartin Storage
c                  source 1 = Stream Gage ID iopsouW = WWSP user plan (type 14)
c                  source 2 = NA or Operating Rule ID
c                  destination = a reservoir
 1053   continue 
c        
cx      write(nlog,*) '  Oprinp; Type 53'
        
        istop=0
        nr=0
        np1=0
        np2=0
c
c ---------------------------------------------------------
c rrb 2020/03/01; a. Require the destination to be Multiple; even if 
c                 just one destination is provided to simplify logic.
        imult=0
        if(ciopde(1:8).eq.'Multiple') then
          imult=1
        else
          write(nlog,1292) ityopr(k),cidvri
          goto 9999
        endif 
c
c ---------------------------------------------------------
c               b. Read monthly constraints 
        idumc=ifix(dumc)
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,ix, ix,
     1               nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c
c ---------------------------------------------------------
c               c. Find source 1 a stream gage (Ark @ LA)
c                  Note itype=0 for a stream ID
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
c		        iacc=0 allows account to be 0 (since 
c                         it is ownership %)
c		        ion=0 leaves the original water right on
c           itype = 0 for a stream gage
        itype=0
        ion=0
        iacc=0
        istop=0
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1,iopsou(2,k), nx, ciopso1, iacc, istop, rops2,
     1       ioprsw(k), cidvri)
     
        iopSouR(k)=itype     
        iopsou(1,k)=iops1   
        iscdS=iops1
c
c
c ---------------------------------------------------------
c rrb 2018/12/15; Add Purgatoire as source 2 to Type 54
c               d. Find source 2 a stream gage (Purgatoire @ LA) 
c                  Note itype=0 for a stream ID
c		                    istop=0 Stop if not found
c		                    istop=1 Do not Stop if not found
c		                    iacc=0 allows account to be 0 (since 
c                                     it is ownership %)
c		                    ion=0 leaves the original water right on
c                       itype = 0 for a stream gage
        itype=0
        ion=0
        iacc=1
        istop=0
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops2,iopsou(4,k), nx, ciopso2, 1, istop, rops2,
     1       ioprsw(k), cidvri)
     
        iopsou(3,k)=iops2         
c
c ---------------------------------------------------------
c rrb 2007/10/26; 
c		            e. Read the operating rule associated with
c		                 this rule (e.g. a Type 54 operating rule).  Note:
c		                 istop=0  Stop if not found
c		                 itype=24 Operating Rule ID associated with this 
c                             operating rule
c       write(nlog,*) ' Oprinp; ioprlim(k) ', ioprlim(k)
        if(ioprlim(k).gt.0) then
          istop=0
          itype=24          
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1      iopsou(5,k),iopsou(6,k), nx, cAssoc, 1, 
     1      istop, rops2,ioprsw(k), cidvri)
     
cx        write(nlog,*) '  Oprinp; iopsou(5,k)', iopsou(5,k)
        endif             
c        
c ---------------------------------------------------------
c               f.   Find destination reservoir or Multi code(type 2)
        imult=0
        if(ciopde(1:8).eq.'Multiple') imult=1
c
c ---------------------------------------------------------
c
c rrb 2020/05/22; This is never used because imult is not allowed to 
c                 not be Multiple (see a above)
c               e1.   Find single destination reservoir
c                    itype=2 reservoir
c  		               istop=0 Stop if not found)
c                    iacc  = 1 stop if iopsou(2,k) = 0
         
        if(imult.eq.0) then
          ndes=0
          istop=0
          itype=2
          iacc=1
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         iopd1, iopdes(2,k), nx,ciopde, iacc, istop, rops2,
     1         ioprsw(k), cidvri)
          if(iopd1.gt.0) then
            iopdes(1,k)=iopd1
            iopdes(2,k)=iopdes(2,k)
            iopdesr(k)=2
          endif  
c         write(nlog,*) '  Oprinp; Type 53 iopdes(1,k) = ', iopdes(1,k)
        endif

c
c ---------------------------------------------------------
c               e2.   Find multiple destination reservoir account data
        if(imult.eq.1) then      
c
c rrb 2018/12/15; Subtract 1 since ioprlim(k) includes an assocated 
c                 operating rule
cxx       ndes=ioprlim(k)
c
c rrb 2021/04/18; Compiler warning
cx        ndes=amax0(0, ioprlim(k)-1)   
          ndes=max(0, ioprlim(k)-1)   
          n1=0
          n2=0
c        
          do n=1,ndes
            n1=n2+1
            n2=n1+1
c          
c ---------------------------------------------------------
c		            e2a. Allow a # to be included in column 1 of the data
 254        read(55,*,end=2040,err=2040) rec1
            
            if(iecho.eq.1 .and. n.gt.1) then
              write(nchk,'(a1,80x,a12,f8.0)') rec1
            endif  
            if(rec1.eq.'#') goto 254
            backspace (55)
c
c          
c ---------------------------------------------------------
c              e2b.  Read and set destination reservoir, account and
c                    pointer from a type 54 operating rule used to
c                    set the Baseflow % (1) or enhanced baseflow % (2)

            read(55,*,end=2040,err=2040) cx,ix,rx
            
            if(iecho.eq.1) then
              write(nchk,*) cx, ix, rx
            endif  
c          
c ---------------------------------------------------------
c               e2c. Find Reservoir and account
c                    itype=2 = reservoir
c                    istop=0 Stop if not found
c		                 istop=1 Do not Stop if not found
c                    iacc  = 1 stop if iopsou(2,k) = 0
           
            itype=2
            istop=0
            iacc=1
            iopd2=ix
            call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1         iopd1,iopd2, nx, cx, iacc, istop, rops2,
     1         ioprsw(k), cidvri)

            iopdes(n1,k)=iopd1
            iopdesr(k)=2
          
c           write(nlog,*) ' '
c           write(nlog,*) ' Oprinp; type 53 n1, k, cx, iopd1'
c           write(nlog,*) ' Oprinp; type 53 ', n1, k, cx, iopd1
            
            iopdes(n2,k)=ix
            ropdes(k,n2)=rx
            ciopdeX(n1,k)=cx
            
            
            ix2=ifix(rx)
            if(ix.le.0 . or. ix2.le.0) then
              write(nlog,1285) ityopr(k),cidvri
              goto 9999
            endif
          end do
        endif
c
c ---------------------------------------------------------
c		            f. Detailed output
     
        iout53=1  
c
        if(iout53.eq.1) then
          n1=0
          n2=0
c
c rrb 2021/04/18; Compiler warning
cx        ndes1=amax0(ndes,1)
          ndes1=max(ndes,1)
          do n=1,ndes1
            n1=n2+1
            n2=n1+1
            iopd1=iopdes(n1,k)
            
            if(iopd1.eq.0) then
              write(nlog,*) 'Opring; problem iopd1 = ', iopd1, ndes1
            else
              ciopde=cresid(iopd1)
            endif
c            
          call OprinOut(nlog, maxopr, k, 
     1      ityopr(k), cidvri, 
     1      ciopde, iopdes(n1,k), iopdes(n2,k), 
     1      ciopso1, iopSou(1,k),iopsou(2,k),
     1      ciopso2, iopsou(n1+2,k),iopsou(n2+2,k), creuse, ireuse1,
     1      oprloss(k), oprlimit(k), iopSou(5,k), 
     1      iopSou(6,k),iopsou(7,k),     
     1      cdivtyp(k), intern, cntern, cAssoc, cAssoc2, cx2)
     
     
     
          end do
        endif

        goto 1190         


c _________________________________________________________
c
c rrb 2018/08/14; Add JMartin Storage Winter Water Program Allocation  
c                 DELETE LATER (CURRENTLY NOT USED)    

c               Type 54; JMartin Flow Partition
c                  source 1 = Stream Gage ID iopsouW = WWSP user plan (type 14)
c                  source 2 = NA or Operating Rule ID
c                  destination = a reservoir
 1054   continue 

c ---------------------------------------------------------
c               a. Read monthly constraints 
        idumc=ifix(dumc)
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,ix, ix,
     1               nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c
c ---------------------------------------------------------
c               b. Find source 1 a stream gage (Arkansas @ LA)
c                  Note itype=0 for a stream ID
c		                    istop=0 Stop if not found
c		                    istop=1 Do not Stop if not found
c		                    iacc=0 allows account to be 0 (since 
c                                     it is ownership %)
c		                    ion=0 leaves the original water right on
c                       itype = 0 for a stream gage
        itype=0
        ion=0
        iacc=1
        istop=0
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1,iopsou(2,k), nx, ciopso1, 1, istop, rops2,
     1       ioprsw(k), cidvri)
     
        iopSouR(k)=itype     
        iopsou(1,k)=iops1   
        iscdS=iops1
c
c
c ---------------------------------------------------------
c rrb 2018/12/15; Add Purgatoire as source 2 to Type 54
c               c. Find source 2 a stream gage (Purgatoire @ LA) 
c                  Note itype=0 for a stream ID
c		                    istop=0 Stop if not found
c		                    istop=1 Do not Stop if not found
c		                    iacc=0 allows account to be 0 (since 
c                                     it is ownership %)
c		                    ion=0 leaves the original water right on
c                       itype = 0 for a stream gage
        itype=0
        ion=0
        iacc=1
        istop=0
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops2,iopsou(4,k), nx, ciopso2, 1, istop, rops2,
     1       ioprsw(k), cidvri)
     
        iopsou(3,k)=iops2   
c          
c ---------------------------------------------------------
c		            d1. Begin to read beginning and ending time periods
c                    allow a # to be provided in column 1
 255    read(55,*,end=2040,err=2040) rec1
        
        if(iecho.eq.1 .and. n.gt.1) then
          write(nchk,'(a1,80x,a12,f8.0)') rec1
        endif  
        if(rec1.eq.'#') goto 255
        backspace (55)
c
c          
c ---------------------------------------------------------
c               d2. Read begin and end time periods for JM Storage
c                    1 = Storage Season (11/1 - 3/31)
c                    2 = Period to calculate ave baseflow (11/8 - 1/14)
c                    3 = Period to use Ave Baseflow and Enhanced
c                        Baseflow (11/22 - 11/28)
c                    4 = Period at end to goto conservation (3/16 - 3/31)
c        
c ---------------------------------------------------------
c 		c1b; Loop for number of time periods
        ndes=ioprlim(k)     

        n1=0
        n2=0
        do n=1,ndes
          n1=n2+1
          n2=n1+3
          read(55,*,end=2040,err=2040) (iopdes(i,k), i=n1,n2) 
        end do          
c ______________________________________________________________________
c		            e. Detailed output
        iout54=0
        if(iout54.eq.1) then
c            
          call OprinOut(nlog, maxopr, k, 
     1      ityopr(k), cidvri, 
     1      ciopde,  iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k), iopsou(2,k),
     1      ciopso2, iopsou(3,k), iopsou(4,k), creuse, ireuse1,
     1      oprloss(k), oprlimit(k), iopSou(5,k), 
     1      iopSou(6,k),iopsou(7,k),     
     1      cdivtyp(k), intern, cntern, cAssoc, cAssoc2, cx2)
     
          n1=0
          n2=0
          do n=1,ndes
            n1=n2+1
            n2=n1+3
            write(nlog,*) (iopdes(i,k), i=n1,n2) 
          end do       
        endif

        goto 1190         
c ______________________________________________________________________
c          End operation file loop                                      
 1190 CONTINUE
c ______________________________________________________________________
c		Print warning if the dimension is exceeded
      write(nlog,1200) cidvri, MAXOPR, maxops
      goto 9999                                                         
c ______________________________________________________________________
c		Normal Exit from reading data
c		Set number of rules read
 1210 NUMOPR=K                                                      
c ______________________________________________________________________
c		Print warning regarding Admin number
      if(iprinta.gt.0) then
        rewind(ntmp)
c rrb revise to read comments plus operating rule data
cs      do i=1,numopr
        do i=1,maxopr
          read(ntmp,'(a132)',end=902) rec132
          write(nlog,'(a132)') rec132
        end do
 902    rewind(ntmp)
      endif         
c ______________________________________________________________________
c		Print warning to review check file 
c     if(iwarno.gt.0) write(nlog,1281)  'Operating Rights'
c ______________________________________________________________________
c		Print number of rights read      
      write(nlog,640)
      i1=0
      do i=1,maxoprin             
        if(ntype(i).gt.0) then
          i1=i1+1
          write(nlog,642) i1, i, oprtype(i), ntype(i)
        endif  
      end do
      write(nlog,644) i1, numopr
      write(nlog,*) 
      write(nlog,630) 'Operating   ', numopr, koff, numopr-koff                                                       
c ______________________________________________________________________
c               Step C1; 
c               Check that all out of priority water rights
c               are tied to a type 8 operation right
      if(ioptio.eq.2 .or. ioptio.eq.8) then
        do n=1,numrsr
          if(ityopr(n).eq.-1 .and. iopid(n).eq.0) then
            write(nlog,1193) cidvri, ityopr(k), n
            goto 9999
          endif
        end do
      endif
c ______________________________________________________________________
c               Step C2; 
c               Check if simulating sprinkler use with Max Supply
c                 but no operating rule
      if(ioptio.eq.2 .or. ioptio.eq.8) then
        ifound=0
        do k=1,numopr
          if(ityopr(k).eq.21 .and. ioprsw(k).ge.1) ifound=1
        end do
        if(ifound.eq.0 .and. isprink.eq.1) then
          write(nlog,1201) cidvri, ityopr(k), isprink
          goto 9999
        endif
c rrb 2009/04/24; Allow isprink=2 to be mutual approach
cx      if(ifound.eq.1 .and. isprink.eq.0) then
        if(ifound.eq.1 .and. isprink.ne.1) then
          write(nlog,1204) cidvri, ityopr(k), isprink
          ioprsw(k)=0
        endif
      endif
c ______________________________________________________________________
c               Step C3; 
c               Check if simulating Soil Moisture use 
c                 but no operating rule
      if(ioptio.eq.2 .or. ioptio.eq.8) then
        ifound=0
        do k=1,numopr
          if(ityopr(k).eq.22 .and. ioprsw(k).ge.1) ifound=1
        end do
        if(ifound.eq.0 .and. isoil.eq.1) then
          write(nlog,1202) cidvri, ityopr(k), isoil
          goto 9999
        endif
c rrb01/04/01; Allow -1 code
c       if(ifound.eq.1.and. isoil.eq.0) then
        if(ifound.eq.1.and. isoil.le.0) then
          write(nlog,1203) cidvri, ityopr(k), isoil
          ioprsw(k)=0
        endif
      endif
c ______________________________________________________________________
c               Step C4; 
c               Check if simulating a diversion reuse plan (type 4 or 6)
c               but no operating rule to spill (type 29) is specified
      iwarnp=0
      ifound=0
      do k=1,numopr
c       write(nlog,*) ' Oprinp; Test k', k
        creuse = creuseX(k)        
        if(creuse(1:2).ne.'NA' .and. ioprsw(k).ne.0) then
          ip=ireuse(k)
          if(ip.gt.0) then       
            if(iPlntyp(ip).eq.4 .or. iPlntyp(ip).eq.6) then
              ifound=0
              do k2=1,numopr
                if(creuse.eq.ciopsoX(1,k2) .and. 
     1            ityopr(k2).eq.29) ifound=1
              end do
              if(ifound.eq.0) then
                write(nlog,932) corid(k), ityopr(k), creuseX(k)
                iwarnp=iwarnp+1
              endif  
            endif  
          endif  
        endif            
      end do 
      if(iwarnp .gt. 0) goto 9999
c ______________________________________________________________________
c rrb 2015/03/30
c               Step C5; 
c               Check & warn the user if the operating rule specified 
c               a operating rule id to limit the diversion that 
c               is used more than once.       
      iokc5=0
      iwarnp=0
      ifound=0
      do k=1,numopr
        ciopde=ciopsoX(1,k)     
        ioprlim2=ioprlim(k)
c
c ------------------------------------------------------------
c                Check for a non-type 47 operating rule limit              
        if(ioprlim2.eq.3 .or. ioprlim2.eq.4 .or.
     1     ioprlim2.eq.8 .or. ioprlim2.eq.9) then
c
          nx = iopsou(6,k)  
c          
c         Check if another limit has the same value is specified
          if(nx.gt.0) then
            ifound=0      
            k1=k+1 
            
            do k2=k1,numopr
              if(iopsou(6,k2).eq.nx) ifound = k2
            end do
c           
            if(ifound.gt.0) then
c
              if(ifound.eq.1) then
                iokc5=iokc5+1
                if(iokc5.eq.1) then
                  write(nlog,1281) ' Operating Right Limits'
                  write(nchk,940)
                endif
c
                write(nchk,9401) iokc5, corid(k), ityopr(k), 
     1            ciopsoX(1,k), ioprlim(k), corid(nx), corid(ifound),
     1            'Separate    '       
              endif
            endif   
          endif
        endif 
c
c -----------------------------------------------------------
c                Check for a type 47 operating rule limit   
        if(ioprlim2.eq.2 .or. ioprlim2.eq.4 .or.
     1     ioprlim2.eq.7 .or. ioprlim2.eq.9) then
c
          nx = iopsou(5,k)  
c          
c         Check if another limit has the same value is specified
          if(nx.gt.0) then
            ifound=0      
            k1=k+1 
            
            do k2=k1,numopr
              if(iopsou(5,k2).eq.nx) ifound = k2
            end do
c           
            if(ifound.gt.0) then
c  
              iokc5=iokc5+1
              if(iokc5.eq.1) then
                write(nlog,1281) ' Operating Right Limits'
                write(nchk,940)
              endif
c
              write(nchk,9401) iokc5, corid(k), ityopr(k), 
     1          ciopsoX(1,k), ioprlim(k), corid(nx), corid(ifound),
     1          'Shared      '              
            endif 
          endif
        endif 
      end do 
c          
c ______________________________________________________________________
c rrb 2011/07/28
c               Step C6; 
c               Check if simulating an Accounting Plan (type 11)
c               or a Changed Water Right Plan (type 13)
c               as a destination that a type 29 (spill) rule
c               has been specified.  Note it does not work with a 
c               type 46 (multiple user) operating rule so that 
c               check is done separately (just below)
      iwarnp=0
      ifound=0
      do k=1,numopr
c               Check if the destination is a plan
        if(iopdesR(k).eq.7) then      
c          write(nlog,*) ' Oprinp; Plan spill check k', k
c rrb 2014-06-15; Revise to allow a negative value to be used
c                 to indicate a plan
cx        ip=iopdes(1,k)
          ip1=iopdes(1,k)
c
c rrb 2021/04/18; Compiler warning
cx        ip=amax0(ip1, -1*ip1)
          ip=max(ip1, -1*ip1)
          ciopde=ciopdeX(1,k)     
          if(ip.gt.0) then   
c
c rrb 2015/03/30; Allow a Changed Water Right Plan (type 13)    
cx          if(iPlntyp(ip).eq.11) then
            if(iPlntyp(ip).eq.11 .or. iPlntyp(ip).eq.13) then
c               Check if a type 29 rule has been specified      
              ifound=0       
              do k2=1,numopr
                if(ciopde.eq.ciopsoX(1,k2) .and. 
     1            ityopr(k2).eq.29) ifound=1
              end do
c              Special treatment when the Accounting plan is
c              the South Platte Compact  
              if(ityopr(k).eq.50) ifound=1
              if(ifound.eq.0) then
                write(nlog,933) corid(k), ityopr(k), ciopdeX(1,k)
                iwarnp=iwarnp+1
              endif  
            endif  
          endif  
        endif  
      end do 
      if(iwarnp .gt. 0) goto 9999          
c ______________________________________________________________________
c rrb 2011/07/28
c               Step C7; 
c               If simulating a type 46 multi user rule check
c               that any destination with an Accounting Plan
c               (type 11) or Changed Water Right Plan (type 13) 
c               has a type 29 spill rule specified.
      iwarnp=0
      ifound=0
      do k=1,numopr
c               Check if the destination is a plan
        if (ityopr(k).eq.46) then
c 		          c1b; Loop for number of destinations
          n1=0
          n2=0 
          ndes=ioprlim(k)
          do n=1,ndes 
            n1=n2+1 
            n2=n1+1 
c
c rrb 2019/03/25; Correction
cx          ciopde=ciopdeX(n,k)   
            ciopde=ciopdeX(n1,k) 
                          
c rrb 2015/03/30; Revise to allow a negative value to be used
c                 to indicate a plan
cx          ip=iopdes(n1,k)
            ip1=iopdes(n1,k)
c
c rrb 2021/04/18; Compiler warning
cx          ip=amax0(ip1, -1*ip1)            
            ip=max(ip1, -1*ip1)            
cx            write(nlog,*) '  Oprinp;', k, ityopr(k),n, n1, n2, ip, 
cx     1        iplntyp(ip), ciopde  
            if(ip.gt.0) then       
c
c rrb 2014/03/30; Add type 13
cx            if(iPlntyp(ip).eq.11) then
              if(iPlntyp(ip).eq.11 .or. iplntyp(ip).eq. 13) then
c                 Check if a type 29 rule has been specified      
                ifound=0       
                do k2=1,numopr
                  if(ciopde.eq.ciopsoX(1,k2) .and. 
     1              ityopr(k2).eq.29) ifound=1
                end do
                if(ifound.eq.0) then
c
c rrb 2019/03/25; Correction
cx                write(nlog,933) corid(k), ityopr(k), ciopdeX(n,k)
                  write(nlog,933) corid(k), ityopr(k), ciopdeX(n1,k)
                  
                  iwarnp=iwarnp+1
                endif  
              endif  
            endif    
          end do
        endif  
      end do 
      if(iwarnp .gt. 0) goto 9999          
c ______________________________________________________________________
c   		        Step C8; 
c                 Check every reservoir plan is tied to a reservoir
c		              Note copied from PlanEva on 6/5/06 to allow 
c                 other checks to occur here
c                 Note Type 3=Reuse_Reservoir and 
c                 Type 5=Reuse_Reservoir_Tmtn      
c		Initialize check
      do i=1,maxown
        iwarn(i)=0
        idum(i)=0
      end do
c ______________________________________________________________________
c		Warn if more that one reservoir plan 
c               is tied to the same reservoir and owner
      do i=1,maxown
        if(iwarn(i).gt.1) then
          ir=idum(i)
          write(nlog,1340) cresid(ir) 
          goto 9999
        endif
      end do
c ______________________________________________________________________
c 		          Step C9; 
c                 Check if the same plan (source 3) 
c                 is tied to a different reservoir and account
      do np=1,nplan
        idum(np)=0
      end do
      do k1=1,numopr
        if(ityopr(k1).eq.32 .or. ityopr(k1).eq.33) then      
          np1=iopsou(3,k1)
          nr1=iopsou(1,k1)
          na1=iopsou(2,k1)
          do k2=k1+1,numopr
            if(ityopr(k2).eq.32 .or. ityopr(k2).eq.33) then      
              np2=iopsou(3,k2)
              nr2=iopsou(1,k2)
              na2=iopsou(2,k2)
              if(np1.eq.np2 .and. nr1.eq.nr2 .and. na1.ne.na2) then
                write(nlog,1330) pid(np), cresid(nr1), na1, 
     1            cresid(nr2), na2
                goto 9999
              endif  
            endif
          end do  
        endif
      end do   
       
c ______________________________________________________________________
c rrb 2007/07/09; 
c 		          Step C10; 
c                 Check conflicting direct flow exchange (24)
c			            and direct flow bypass (25) operating rules
      do k1=1,numopr
        if(ityopr(k1).eq.24 .or. ityopr(k1).eq.25) then        
          if(iopsou(3,k1).gt.0) then          
            do k2=1,numopr
              if((ityopr(k2).eq.27 .or. ityopr(k2).eq.28) .and. 
     1          iopsou(3,k2).gt.0 .and. 
     1          (iopsou(3,k2).eq.iopsou(3,k1))) then      
                write(nlog,1370) ityopr(k1), corid(k1), ciopsoX2(k1),
     1            corid(k2), ciopsoX2(k2)
                goto 9999
              endif  
            end do  
          endif  
        endif
c		Check if the same source right is used by multiple
c		operating rules
        if(ityopr(k1).eq.24 .or. ityopr(k1).eq.25) then  
          do k2=1,numopr
cx          if((k1.ne.k2) .and. (iopsou(1,k1).eq.iopsou(1,k2))) then
            if( (k1.ne.k2) .and. (ciopsoX(1,k1).eq.ciopsoX(1,k2))) then
              write(nlog,1380) ityopr(k1),
     1          corid(k1), ciopsoX(1,k1), iopsou(1,k1),
     1          corid(k2), ciopsoX(1,k2), iopsou(1,k2)
              goto 9999
            endif  
          end do  
        endif
      end do    
c ______________________________________________________________________
c rrb 2018/11/02; 
c 		          Step C11; 
c                 Check the user has not specified a fixed 
c                 percent at the same gage more than once.  This
c                 is because the flow is adjusted each time the
c                 operating rule is called.
      do k1=1,numopr
        if(ityopr(k1).eq.53) then 
          iops1=iopsou(1,k1)
          iops2=iopsou(2,k1)
          
          do k2=k1+1,numopr
            if(ityopr(k2).eq.53) then
              iops3=iopsou(1,k2)
              iops4=iopsou(2,k2)
              
              if(iops1.eq.iops3) then
                if(iops2.gt.0 .or. iops4.gt.0) then
                  write(nlog,1372) ityopr(k1),corid(k1), corid(k2),
     1                             ciopsoX(1,k1)
                  goto 9999 
                endif              
              endif
            endif
          end do
        endif
      end do       
c _________________________________________________________
c
c rrb 2015/07/08
c 		          Step C12; 
c                 Warn the user if there are one or more 
c                 accounts in a reservoir that book water (type 8) to
c                 another account in the same reservoir and
c                 then book water back into an account in the same
c                 reservoir.  Note
c                   this check was implemented for the reallocation
c                     issues on teh San Juan that caused a reoperation
c
      do k=1,numopr
        ciopso1=ciopsoX(1,k)
        ciopde=ciopdeX(1,k)
c
c               Determine if the first operating right is a type 6 (book over) 
c               and the source and destination are the same ID.
        if(ityopr(k).eq.6 .and. ciopso1.eq.ciopde) then
          iopdesA=iopdes(2,k)
          
          k2b=k+1
          do k2=k2b, numopr
c
c               Determine if the source reservoirs are the same for both operating
c               rules and one part of the reoperation control (ioprlim(k)) is not
c               set to 1          
            if(ciopso1 .eq. ciopsoX(1,k2) .and. ioprlim(k).ne.1) then
              ciopso1=ciopsoX(1,k2)
              ciopde=ciopdeX(1,k2) 
c
c               Determine if the second operating right is a type 6 (book over) 
c               and the source and destination are the same ID.
              if(ityopr(k2).eq.6 .and. ciopso1.eq.ciopde) then 
                iopdesB=iopdes(2,k2)
c
c               Determine if water is booked in by first rule and out by the second
                if((iopdesA.gt.0 .and. iopdesB.le.0) .or.
     1             (iopdesA.lt.0 .and. iopdesB.gt.0)) then
c  
c               Warn user           
                   write(nlog,1384) ityopr(k), corid(k), 
     1             ciopsoX(1,k), iopsou(2,k), ciopdeX(1,k), iopdes(2,k),
     1             ityopr(k2), corid(k2),      
     1             ciopsoX(1,k2),iopsou(2,k2),ciopdex(1,k2),iopdes(2,k2)
                endif
              endif
            endif
          end do
        endif
      end do  
c     
c _________________________________________________________
c
c rrb 2019/01/15 (version 15.00.35)
c		            Step C13; Check the WWSP data is provided correctly.
c                 Specifically if creuse (for a type 24, 25 or 45)
c                 is a WWSP Source Plan (type 14), 
c                 it must be a source for a type 46 plan.
c                 Note the logic is intended to allow multiple
c                 type 46 rules each with a different WWSP Source Plans
c
c ---------------------------------------------------------
c               Step 13a; Find a type 46 operating right
      do k1=1,numopr
c
c ---------------------------------------------------------
c               Step 13b; Find creuse a plan.  Note:
c		                     itype =7 for a plan
c                        istop=0 Stop if not found
c		                     istop=1 Do not Stop if not found

        creuse=creuseX(k1)
        cidvri=corid(k1)
        
        itype=7
        istop=1
        iacc=1
c
c rrb 2019/08/11; Revise to not allow iopsou(2,k1) to be reset
c                   by passing variable iops2, not iopsou(2,k1)
cx        call oprFind(ityopr(k1), itype, idumc,k,ion,iprinto,
cx     1   iops1,iopsou(2,k1), nx, creuse, iacc, istop, rops2,
cx     1   ioprsw(k1), cidvri)
        call oprFind(ityopr(k1), itype, idumc,k,ion,iprinto,
     1   iops1,iops2, nx, creuse, iacc, istop, rops2,
     1   ioprsw(k1), cidvri)
c
c ---------------------------------------------------------
c               Step 13c; If a plan was found as a variable creuse,
c                         determine if its a WWSP User Plan (type 15).
c                         if it is a type 14 determine if there is a
c                         type 46 plan with this ID as a source.  
c                         If not, print warning & stop
       if(iops1.gt.0) then
          if(iplnTyp(iops1).eq.14) then
cx            write(nlog,*) '  Oprinp; WWSP Check',
cx     1        k1, corid(k1), creuse 
c       
            iok=1
            do k2=1,numopr
cx            if(k1.ne.k2) then
              if(ityopr(k2).eq.46) then
                ciopso1=ciopsoX(1,k2)  
                if(ciopso1.eq.creuse) iok=0
              endif
            end do 
c ---------------------------------------------------------
c               Step 13d; Warn and Stop if creuse is a WWSP Plan 
c                        (type 14) and it is not a source to a 
c                         type 46 operating rule                
            if(iok.eq.1) then 
              write(nlog,1288) ityopr(k1),cidvri, creuse, 14,
     1                         creuse, creuse
              goto 9999
            endif  
c
c               Endif for a creuse value is a type 14 plan
          endif
c
c               Endif for a creuse value is a plan
        endif
c
c               End do for operating rules
      end do
c _________________________________________________________
c
c rrb 2019/6/16
c		            Step C14; Check the JMartin data is provided correctly.
c                  ichkJM = 0 Indicates both a type 53 & 54 were found
c                             This is also an indicator to perform 
c                             detailed checks  
c                  ichkX  = 0 At least one type 53 or 54 was found 
c                  ichk53 > 0 Pointer to a type 53 opr rule
c                  ichk54 > 0 Pointer to a type 54 opr rule 
c
c                  iokP   = 0 Priority of type 54 is senior to type 53      
c                  iokS1  = 0 Source 1 of type 54 is same as type 53      
c                  iokS2  = 0 Source 2 of type 54 is same as type 53      
c
c ---------------------------------------------------------
      ichkJM=1
      ichkX=1
      ichk53=0
      ichk54=0
c
      iokP=1
      iokS1=1
      iokS2=1
c
c ---------------------------------------------------------
c               Step 14a; Process *.opr file and 
c                 to determine if type 54 is senior to type 53
c
      do k1=1,numopr
c      
c ---------------------------------------------------------
c                 Check if a type 53 was specified first
        if(ichkJM.eq.1 .and. ityopr(k1).eq.53) then  
          ichkX=0
          ichk53=k1
        
          do k2=k1+1,numopr
            if(ityopr(k2).eq.54) then
              ichkJM=0
              ichk54=k2
c
c rrb 2021/04/18; Compiler warning
cx              c=ropnk(k2) - ropnk(k1)
cx              if(c.gt.small) iokP=0
              c8=ropnk(k2) - ropnk(k1)
              if(c8.gt.small8) iokP=0
            endif
          end do
        endif
c
c ---------------------------------------------------------
c                 Check if a type 54 was specified first
        if(ichkJM.eq.1 .and. ityopr(k1).eq.54) then
          ichkX=0
          ichk54=k1
          do k2=k1+1,numopr
            if(ityopr(k2).eq.53) then
              ichkJM=0         
              ichk53=k2   
c
c rrb 2021/04/18; Compiler warning
cx            c=ropnk(k2) - ropnk(k1)
cx            if(c.gt.small) iokP=0
              c8=ropnk(k2) - ropnk(k1)
              if(c8.gt.small8) iokP=0
            endif
          end do
        endif
c
c ---------------------------------------------------------
c               End do for type 53 & 54 operating rule checks
      end do
c
c ---------------------------------------------------------
c               Step 14b; Warn and Stop if a priority problem
      if(ichkJM.eq.0 .and. iokP.eq.1) then 
        write(nlog,1386) corid(ichk53), ropnk(ichk53),
     1                   corid(ichk54), ropnk(ichk54)
        goto 9999
      endif  
c
c ---------------------------------------------------------
c               Step 14c; Warn and Stop if a type 53 was specified and
c                 not a type 54 and visa versa
      if(ichkX.eq.0) then
        if(ichk53.eq.0) then 
          write(nlog,1388) corid(ichk54), 54, 53
          goto 9999
        endif
        
        if(ichk54.eq.0) then
          write(nlog,1388) corid(ichk53), 53, 54
          goto 9999
        endif
      endif  
c
c ---------------------------------------------------------
c               Step 14d; Warn and Stop if different sources
c                 are provided.  This check knows both exist
c                 because it passed check 14c
      if(ichkJM.eq.0) then
        if(ciopsoX(1,ichk53).eq. ciopsoX(1,ichk54)) iokS1=0 
        if(ciopsoX2(ichk53) .eq. ciopsoX2(ichk54))  iokS2=0 
        if(iokS1.eq.1 .or. iokS2.eq.1) then
          write(nlog,1390) corid(ichk53), ciopsoX(1,ichk53),
     1                     corid(ichk54), ciopsoX(1,ichk54),
     1                     corid(ichk53), ciopsoX2(ichk53),
     1                     corid(ichk54), ciopsoX2(ichk54)     
          goto 9999
        endif
      endif  
c ______________________________________________________________________
c rrb 2020/05/31
c               Step C15; 
c               If simulating a type 46 multi user rule check
c               if the destination is the same as another
c               type 46 operating rule.  This is currently not
c               allowed to simplify reporting


c               that any destination with an Accounting Plan
c               (type 11) or Changed Water Right Plan (type 13) 
c               has a type 29 spill rule specified.
c          
      iwarnp=0
      ifound=0
      iok46=0
      do k=1,numopr
c               Check if the destination is a plan
        if (ityopr(k).eq.46) then
c 		          c1b; Loop for number of destinations
          n1=0
          n2=0 
          ndes=ioprlim(k)
          
          do n=1,ndes 
            n1=n2+1 
            n2=n1+1 
            
            ciopde=ciopdeX(n1,k) 
            ip1=iopdes(n1,k)
c
c rrb 2021/04/18; Compiler warning
cx          ip=amax0(ip1, -1*ip1) 
            ip=max(ip1, -1*ip1) 
                       
cx            write(nlog,*) '  Oprinp;', k, ityopr(k),n, n1, n2, ip, 
cx     1        iplntyp(ip), ciopde  
            if(ip.gt.0) then       
              ifound=0       
              
              do k2=k+1,numopr
                if(ityopr(k2).eq.46) then
                  n12=0
                  n22=0
                  ndes2=ioprlim(k2)
                  
                  do n2=1,ndes2
                    n12=n22+1
                    n22=n12+1
c
c                       Warn if a duplicate was found                  
                    if(ciopde.eq.ciopdeX(n12,k2)) then
                      iok46=iok46 + 1
                      if(iok46.eq.1) write(nchk,941)
                      write(nchk,9411) iok46, corid(k), ityopr(k), 
     1                     ciopde, corid(k2), ciopdeX(n12,k2)                
                      iwarnp=iwarnp+1
                      
                    endif    
c
c                     Enddo for 2'nd destination loop
                  end do
c
c                     Endif for a 2'nd type 46 opr rule
                endif               
c
c                     Enddo for 2'nd operating rule loop
              end do
c
c                     Endif for ip > 0
            endif               
c
c               Endif for 1'st destinaiton loop
          end do
c
c               Endif for 1'st operating rule type 46
        endif  
c
c               Endif for 1'st operating rule loop
      end do 
cx    if(iwarnp .gt. 0) goto 9999          
c ______________________________________________________________________
c rrb 2021/02/14;
c               Step C16; Check destinations for a type 29 spill
c                         for various plan sources
      iwarnp=0
      ifound=0
      iok29=0
      do k=1,numopr
        if(ityopr(k).eq.29 .and. iopsouR(k).eq.7) then
          np1=iopsou(1,k)
c
c ---------------------------------------------------------
c rrb 2021/02/14;  Add Destination Checks as Warnings
c
c		              Step 16a Destination check #1 for an Admin Plan 
c                          (type 11) 
c                  Note the destination must be specified and
c                  must be the plan location.
        if(np1.gt.0) then    
          iok=0  
          if(iplntyp(np1).eq.11) then
            if(iopdes(1,k).le.0) iok=1
            if(iopdes(1,k) .ne. ipsta(np1)) iok=1
          endif

          if(iok.eq.1) then
            iok29=iok29+1
            if(iok29.eq.1) write (nchk,935)
            iok=0
            write(nchk,9351) iok29, corid(k), ityopr(k), ciopsoX(1,k), 
     1        iplntyp(np1), ciopdeX(1,k), 'Plan Loc.  =', ciopsoX(1,k)  
cx            goto 9999
          endif
        endif      
c
c ---------------------------------------------------------
c rrb 2021/02/14;  Add Destination Checks as Warnings
c		               Step 16b  Dest check #2 for a Changed WR Plan 
c                            (type 13)
c                  or Import plan (type 7)
c                  Note the destination must be specified and
c                  must be the stream location downstream of the plan
c                  where downstream is ndnnod(iopdes1)
        if(np1.gt.0) then    
          iok=0           

          if(iplntyp(np1).eq.7 .or. iplntyp(np1).eq.13) then
            iopdes1=iopdes(1,k)
            
            if(iopdes1.le.0) then
              iok=1
              rec12=ciopdeX(1,k)
              rec12b='IN NETWORK  '
            else         
              iscd = ipsta(np1)
              idown =idncod(iscd)            
              if(ciopdeX(1,k) .ne. cstaid(idown)) iok=2            
              rec12=ciopdeX(1,k)
              rec12b=cstaid(idown)  
            endif 
          endif

          if(iok.eq.1) then
            iok29=iok29+1
            if(iok29.eq.1) write (nchk,935)
            write(nchk,9351) iok29, corid(k), ityopr(k), ciopsoX(1,k), 
     1        iplntyp(np1), rec12, '*   DEST NOT', rec12b 
cx            goto 9999
          endif
          
          if(iok.eq.2) then
            iok29=iok29+1
            if(iok29.eq.1) write (nchk,935)
            write(nchk,9351) iok29, corid(k), ityopr(k), ciopsoX(1,k), 
     1        iplntyp(np1), rec12, 'Downstream =', rec12b 
cx            goto 9999
          endif
         endif
c     
c ---------------------------------------------------------
c rrb 2021/02/14;  Add Destination Checks as Warnings
c		            Step 16c Dest check #3 for a  WWSP Supply Plan (14) 
c                        or WWSP User Plan (15)
        if(np1.gt.0) then 
          iok=0  
          if(iplntyp(np1).eq. 14 .or. iplntyp(np1) .eq. 15) then 
            if(iopdes(1,k).ge.0) iok=1
          endif
            
          if(iok.eq.1) then  
            iok29=iok29+1
            iok=0
            
            if(iok29.eq.1) write (nchk,935)
            write(nchk,9351) iok29, corid(k), ityopr(k), ciopsoX(1,k), 
     1        iplntyp(np1), ciopdeX(1,k), 'No Spill  =','-1          ' 
cx            goto 9999
          endif
        endif
c
c               Endif for type 29 with a plan source
        endif      
c
c               Enddo for opr loop      
      end do
      
      
c ______________________________________________________________________
c rrb 2021/02/14; 
c 		          Step C17; 
c                 Check if a type 27 or 28 deliver to a Aug (type 1)
c                 or T&C (type 2) plan by a carrier with losses.  If
c                 found set losses to zero.
      iok17=0
      do k=1,numopr
        if(ityopr(k).eq.27 .or. ityopr(k).eq.28) then  
c
c ---------------------------------------------------------
c              Step 17b Check for a plan destination    
          if(iopdesR(k).eq.7) then
c
c ---------------------------------------------------------
c               Step 17c Check for a type 1 or 2 plan destination    
            ip1=iopdes(1,k)
            if(iplntyp(ip1).eq.1 .or. iplntyp(ip1).eq.2) then
c
c ---------------------------------------------------------
c               Step 17c Check for a carrier loss & if non-zero
c                        print warning
             ioprloss=int(oprloss(k))
cx           write(nchk,*)  'Oprinp;',corid(k), ityopr(k), ioprloss            

             if(ioprloss.eq.-1) then
                iok=0
                do i=1,maxcary
                  if(oprlossC(k,i).gt.small) then
                    iok=1  
                    oprlossC(k,i)=0.0      
                  endif           
                end do
c
c ---------------------------------------------------------
c               Step 17d Print warning to *.chk file
                
                if(iok.eq.1) iok17=iok17+1
                if(iok17.eq.1) write(nchk,942)
                
                if(iok.eq.1) write (nchk,9421) 
     1            iok17, corid(k), ityopr(k), ciopdeX(1,k), 
     1            iplntyp(ip), oprloss(k)
              endif
            endif
          endif
        endif
c
c               End Operating right loop
      end do
      
      
c
c ______________________________________________________________________
cx901 close(55)
      close(55)
 500  return
c ______________________________________________________________________
c		Warnings
  916 format(/, 72('_'), /,
     1 '  Oprinp; Warning *.opr rule ID ', a12 ' Type ', i5,/  
     1 '          has source 2 (ciopso(2) = ',a12,/
     1 '          and a return pattern switch (iopsou(4,1) = ',i5,/
     1 '          which is not allowed. ',/
     1 '          recommend you revise the operating rule file.',/
     1 '          StateMod is continuing to operate as if it is zero')
cx  918 format(/, 72('_'), /,
cx     1 '  Oprinp; Warning *.opr rule ID ', a12,/  
cx     1 '          has a release type (iopdes(4,k)) = ', i5,/
cx     1 '          which means make a reservoir release only if a', /
cx     1 '          ditch has a CIR.  Since you have the variable'/
cx     1 '          efficiency off in the *.ctl file this has no effect')
cx  919 format(/, 72('_'), /,
cx     1 '  Oprinp; Warning *.opr rule ID ', a12,  
cx     1          ' has a destination account = ', i5,/
cx     1 '          which means the opr rule treats the reservoir', 
cx     1          ' as a total, not by an account')
cx  925 format(/72('_'),/  
cx     1 '  Oprinp; Problem with *.opr rule ID = ', a12, / 
cx     1 '          destination account = ', i5, ' Reset to 1') 
cx 
c ______________________________________________________________________
c               Error Handling         
  926 write(nlog,927) iin2, filena
  927 format(/, 72('_'),/
     1 '  Oprinp; Problem. End of file # ', i4, ' encountered',/,
     1       '   File name: ', a256)
      goto 9999
c ______________________________________________________________________
  928 write(nlog,929) iin2, filena, cidvri
  929 format(/, 72('_'),/
     1 '  Oprinp; Problem reading file # ', i4,/,
     1 '          File name    = ', a256,/
     1 '          Last ID read = ', a12,/
     1 '          ():')
c     1 '          Last record read:')
c ______________________________________________________________________
      backspace(iin2)
      recin=' '
      read (iin2, '(a256)',end=926,err=926) recin
      write(nlog,'(a256)') recin
      write(nlog,931)
  931  format(/,       
     1 '        Check input data of *.opr',/      
     1 '        Also be sure Names and IDs have no blanks',/
     1 '        (e.g. revise "My Name" to "My_Name").')
      goto 9999
c ______________________________________________________________________
 2000 write(nlog,2010) cidvri, ITYOPR(K)      
 2010 format(
     1 ' Oprinp; Problem with *.opr rule ID = ', a12,
     1 ' itype ', i5, / 
     1 10x,'Cannot read destination water right')
      goto 9999
c ______________________________________________________________________
 2040 write(nlog,2042) cidvri, ITYOPR(K)      
 2042 format(/,72('_'),/
     1 '  Oprinp; Problem with *.opr rule ID = ', a12,' type ', i5,/  
     1 9x,'Cannot read the multiple destination data',/
     1 9x,'Recommend you revise the Operating rule (*.opr) file data')
      goto 9999
c ______________________________________________________________________
 2044 write(nlog,2046) cidvri, ITYOPR(K), iopsou(2,k)      
 2046 format(/,72('_'),/
     1 '  Oprinp; Problem with *.opr rule ID = ', a12,' type ', i5,/  
     1 9x,'Variable iopsou(2,k) = ', i5,/
     1 9x,'It should be the month when the operational limit is',/
     1 9x,'reset. For example 1= January, 2=March, etc.',/
     1 9x,'Recommend you revise the Operating rule (*.opr) file data')
      goto 9999      
c ______________________________________________________________________
 2048 write(nlog,2049) cidvri, ITYOPR(K), ioprlim(k)
 2049 format(/, 72('_'), /,
     1 '  Oprinp; Problem with *.opr rule ID = ', a12,' type ', i5,/  
     1 10x,'The miscellaneous limit (oprlimit) = ', i5,/
     1 10x,'When oprlimit is > 0, the source constraint ',/ 
     1 10x,'  (iopsou(2,1) must be set to 1 to insure the ',/
     1 10x,'  constraint operates and the source right does not.',/
     1 10x,'When oprlimit is 2 the destination should be a ',/
     1 10x,'  reservoir and a reservoir demand limit is expected.',/
     1 10x,'When oprlimit is 3 the destination should be a',/
     1 10x,'  diversion and a diversion demand limit is expected.',/
     1 10x,'When oprlimit is 7 the destination should be a',/
     1 10x,'  reservoir and a spill order reservoir is expected.',/
     1 10x,'Recommend you revise the operating rule data.')  
      goto 9999    
c ______________________________________________________________________
 2050 write(nlog,2051) cidvri, ITYOPR(K)
 2051 format(/,72('_'),/
     1 '  Oprinp; Problem with *.opr rule ID = ', a12,' type ', i5,/  
     1 10x, 'The capability to read carrier data without a loss',/
     1 10x, '  factor is not operational.',/
     1 10x, 'Recommend you set variable OprLoss to a non zero',/
     1 10x, '  value to indicate carrier with loss data is provided',/
     1 10x, '  and provide carrier with loss data. Note if there',/
     1 10x, '  is no loss, set variable OprLoss = 0')
      goto 9999
c ______________________________________________________________________
 9999 write(6,1250)
      write(nlog,1250)
      write(6,*) 'Stop 1'
      call flush (6)
      call exit(1)
      stop 
c ______________________________________________________________________
c     Formats
cx  200 format(/, 72('_'), /,'  Oprinp; Operational File (*.opr) ')
cx  201 format(/,
cx     1 '  Oprinp; Old operational right (*.opr) file provided',/
cx     1 '          That DOES NOT INCLUDE variable OprLoss and OprLimit'/
cx     1 '          Start Date and End Date')  
cx       
cx  202 format(/,72('_'), /,
cx     1 '  Oprinp; New operational right (*.opr) file provided',/
cx     1 '          That DOES INCLUDE variable OprLoss and OprLimit'/
cx     1 '          Start Date and End Date')  
cx       
cx  203 format(/,72('_'), /,
cx     1 '  Oprinp; Warning a possible mixture of new and old ',/
cx     1 '          operarating right formats determined',/
cx     1 '          This might cause problems with recent updates to ',/
cx     1 '          allow carrier losses. Recommend you:',/
cx     1 '          1. Revise the operating rule file to include',/
cx     1 '             Format=2.00 as the first data entry in the file',/ 
cx     1 '          2.Include variables OprLoss, OprLimit, ioBeg',/ 
cx     1 '             and ioEnd with your data.',/
cx     1 '          Note the *.chk file includes data in the new',/
cx     1 '          format including any comments in the original file') 
cx
cx  104 format(/,72('_'), /,
cx     1  '  Oprinp; Warning StateMod Version 11.46 and greater revised',/
cx     1  '          the input data used by a Carrier (Type 11) and ',/
cx     1  '          Constrained Carrier (Type 14) operating rules.',/
cx     1  '          Specifically both allow the user to control when a',/
cx     1  '          source water right may be used by both a standard',/
cx     1  '          diversion and the carrier (iopsou(2,1)=0) or ',/
cx     1  '          by the carrier only (iopsou(2,1)=1). Also this',/
cx     1  '          change revised how data is provided to a ',/
cx     1  '          Constrained Demand (Type 14). ',/
cx     1  '          Recommend you check your operating rule data ',/
cx     1  '          (*.opr) and results accordingly.')
cx     
  105 format('  Oprinp; Operating Rule dates ',i5, 1x, a12, 1x, 3i5)           
cx  110 FORMAT(A256)                                                       
cx  120 format(4x, a256)
cx  131 format('  Oprinp; k, cidvri, nameo ', i5, 1x, a12, 1x, a24)
cx
cx  132 format(a12,a24,a12,f16.0,f8.0,i8, 3(1x,a12,i8), i8,
cx     1         1x,a12)             
cx
cx  140 format(/, 
cx     1 '  Oprinp; k, ityopr(k) = ', 2i5)
 1321   format(a12,a24,a12,f16.0,f8.0,i8, 3(1x,a12,f8.0), i8,
     1         1x,a12, 1x,a12, 1x, 2f8.0, 2i8)      
 1322   format(        a12,a24,a12,f16.0,f8.0,i8, 3(1x,a12,f8.0), i8)
 1324   format(a12,a24,a12,f16.5,f8.0,i8, 3(1x,a12,f8.0), i8,
     1         1x,a12, 1x,a12, 1x, 2f8.0,2i8, i5)             
cx  133   format(i5, 1x, a12, 1x, i5)
  134   format(12x,24x,12x,16x,  f8.0,f8.0, 3(1x,a12,i8), 20i8)  
  590 format(/, 72('_'),/
     1  '  Oprinp; Problem with Operation right = ', a12,/                 
     1 10x,'Operation type  = ', i5,/
     1 10x,'Allowable range = ', i5)
cx  592 format(/, 72('_'),/
cx     1 '  Oprinp; Problem with Operation right = ', a12,/                 
cx     1 10x,'Do not expect ', f8.0, ' structures or',                 
cx     1 10x,' month codes for this type of operating rule.'/
cx     1 10x,' Note: a negative value indicates 12 monthly codes will',
cx     1 10x,' be provided plus some n-12 intervening structures')     
cx        
  630 format(/,72('_'),/  
     1  '  Oprinp; Number of ', a12,    ' Rights = ', i5,/
     1  '          Number turned off             = ', i5,/
     1  '          Number active                 = ', i5)
  640  format(/,72('_'),//
     1 '  OprinP; Number of Operating Rule Types '/,
     1 '    # Type Description                Number',/
     1 ' ____ ____ _________________________ _______' )
  642  format(2i5, 1x, a25, i8)      
  644  format(
     1 ' ____ ____ _________________________ _______' ,/
     1 i5,  '   NA Total                    ',i8)
     
cx  672  FORMAT(/, 72('_'), /,    
cx     1  '  Oprinp; Warning for Operation right = ', a12,/,
cx     1 10x,'It is carrying water from source ', a12, 
cx     1 10x,'through itself ', a12,/
cx     1 10x,'To fix: 1. Delete carrier ', a12, /,
cx     1 10x,'        2. Revise # of carriers')
cx
cx  720 format(/, 72('_'),/
cx     1  '  Oprinp; Problem with Operation right = ', a12,/
cx     1 10x,'Cannot find source ID  ',a12, 'at location ', i5,/
cx     1 10x,'Note if the source is a operating right',/,
cx     1 10x,'that right must be on and occur in the *.opr',/
cx     1 10x,'file before this right')
c
  721 format(/, 72('_'),/
     1  '  Oprinp; Problem with Operation right = ', a12,/
     1 10x,'Cannot find source ID  ',a12,' or source account ', i8)

cx  722 format(/, 72('_'),/
cx     1  '  Oprinp; Problem with Operation right = ', a12,/
cx     1 10x,'Cannot find destination ID or Account  ',a12) 
cx     
cx  723 format(/, 72('_'),/
cx     1   '  Oprinp; Problem with Operation right = ', a12,/,
cx     1 10x,'Destination or Source structure ',a12,/, 
cx     1 10x,'is the same as the carrier ',a12,/
cx     1 10x,'Probably remove the carrier, since it is implied by the ',/
cx     1 10x,'destination or source specification')
cx
cx  724 format(/, 72('_'),/
cx     1  '  Oprinp; Problem with Operation right = ', a12,/, 
cx     1 10x,'Destination or Source ',a12,' cannot be found',/, 
cx     1 10x,'Note for a (reservoir to carrier by exchange)',/,
cx     1 10x,'1) the destination must be a type 11 operating right',/
cx     1 10x,'2) the operating right must occur before this right, and'/
cx     1 10x,'3) the operating right must be on',/
cx     1 10x,'Note for a type 10 (replacement reservoir)',/,
cx     1 10x,'1) the second source, if provided, must be a type 8',/
cx     1 10x,'   operation right',/
cx     1 10x,'2) the operating right must occur before this right, and'/
cx     1 10x,'3) the operating right must be on')
cx     
cx  725   format(/, 72('_'),/
cx     1  '  Oprinp; Problem with Operation right = ', a12,/
cx     1 10x,'Cannot find intervening ID  ',a12,/)
cx
  728 format(/,72('_'),/
     1 '  Oprinp; Warning the following Operation Rights have a ',/
     1 '          source Admin # (*.ddr, *.rer, *.wer, etc) not',/
     1 '          equal to the value in the operational right file',/
     1 '          The operational right value used may or may not ',/
     1 '          be adjusted as presented below.',/
     1 '          Note Augmentation Well (type 37) rules are not',/
     1 '          reported below since an adjusted (lower) operating',/
     1 '          rule priority is very common'//
     1 '                                      ',
     1 '        Source     Operational     Value Used',/
     1 '      # Type Opr ID       Source ID   '
     1 '        Admin #        Admin #        Admin #',/
     1 '  _____ ____ ____________ ___________ ',
     1 ' ______________ ______________ ______________')
     

  729 format(2x, 2i5, 1x,a12, 1x,a12, 20(1x, f14.5))
  
cx  731 format(/, 72('_'),/
cx     1  '  Oprinp; Problem with Operation right = ', a12,/,
cx     1 '    Cannot find a water right for destination ID  ',a12)
cx       
  732 format(/, 72('_'),/
     1   '  Oprinp; Problem with Operation right = ', a12,/,
     1 10x,'For a type 8 rule the Source ID = ', a12,/
     1 10x,'and Destination ID              = ', a12,/, 
     1 10x,'should be the same',/
     1 10x,'Recommend you revise accordingly')
     
  733 format(/, 72('_'),/
     1   '  Oprinp; Problem with Operation right = ', a12,/,
     1 10x,'For a type 38 rule the Admin number          = ',f15.5,/
     1 10x,'Should be less than the Source rights number = ',f15.5,/
     1 10x,'Recommend you revise one or both as appropriate')
     
  734 format(/, 72('_'),/
     1   '  Oprinp; Problem with Operation right = ', a12,/,
     1 10x,'For a Book Over (type 8) when the Source 1 ID = ', a12,/
     1 10x,'The Destination ID should be a plan, not       = ', a12,/, 
     1 10x,'Recommend you revise accordingly')
     
  735 format(/, 72('_'),/
     1   '  Oprinp; Problem with Operation right = ', a12,/,
     1 10x,'For a Type ', i3, ' rule with a destination = ',a12,/
     1 10x,'The operating loss should be zero, not ', f8.2,/
     1 10x,'Note all losses and returns for a Diversion destination',/
     1 10x,'are calculated using the structures efficiency data.',/
     1 10x,'Recommend you set the operating loss to zero and check',/
     1 10x,'that your structure efficiency values are correct')

  736 format(/, 72('_'),/
     1   '  Oprinp; ',/
     1 10x,'Problem with Operation Type  = ',i3,' Right = ', a12,/,
     1 10x,'The Source location         = ', a12, ' and ',/
     1 10x,'the Administration location = ', a12,/
     1 10x,'which is not currently supported. Note:',/
     1 10x,'A diversion destination must have its admin point',/
     1 10x,'at the source structure',/
c    1 10x,'2. A reservoir destination must have its admin point',/
c    1 10x,'   at the source or destination structure',/
     1 10x,'Recommend you revise your data or for a diversion',/
     1 10x,'destination to use a Type 16, 24 or 29 rule')

cx  737 format(/, 72('_'),/
cx     1   '  Oprinp; ',/
cx     1 10x,'Problem with Operation Type  = ',i3,' Right = ', a12,/,
cx     1 10x,'The Ownership percent (iopsou(4,l) = ', i5,'%',/
cx     1 10x,'The ownership percent was added in Version 11.55',/
cx     1 10x,'For backward compatiblity a value of 0 is reset to = 100%',/
cx     1 10x,'Recommend you revise iopsou(4,l) to be > 0 and </= 100.')
cx
cx  738 format(/, 72('_'),/
cx     1   '  Oprinp; ',/
cx     1 10x,'Problem with Operation Type  = ',i3,' Right = ', a12,/,
cx     1 10x,'The variable Oprlimit = ', f5.0, ' that indicates water',/
cx     1 10x,'is shared with Operating Rule = ', a12,' But the ',/
cx     1 10x,'Administration dates do not equal as follows:',/
cx     1 10x,'Administration date 1 = ' f16.5,/
cx     1 10x,'Administration date 2 = ' f16.5,/
cx     1 10x,'Recommend you revise your operating rule data')
cx     
  739 format(/, 72('_'),/
     1   '  Oprinp; Problem with Operation right = ', a12,/,
     1 10x,'For a Type ', i3, ' rule with a destination = ',a12,/
     1 10x,'The operating loss should be zero or greater not ', f8.2,/
     1 10x,'Recommend you set the operating loss to zero and check',/
     1 10x,'that your structure efficiency values are correct')

  740 format(/, 72('_'),/
     1   '  Oprinp; ',/
     1 10x,'Problem with Operation Type  = ',i3,' Right = ', a12,/,
     1 10x,'The percent of water right as a source (iopsou(4,l) = ',
     1       i5,'%',/
     1 10x,'A value less than 100% is not operatonal',/
     1 10x,'Recommend you revise iopsou(4,l) to be 100.')

cx  741 format(/, 72('_'),/
cx     1   '  Oprinp; ',/
cx     1 10x,'Problem with Operation Type  = ',i3,' Right = ', a12,/,
cx     1 10x 'Variable iopsou(2,k) = ', i5, ' that allows the source',/
cx     1 10x,'right to be left on is not operational'/
cx     1 10x,'Recommend you revise iopsou(2,l) to be 1')
cx
  742 format(/, 72('_'),/
     1   '  Oprinp; ',/
     1 10x,'Problem with Operation Type  = ',i3,' Right = ', a12,/,
     1 10x 'Variable ioprlim(k) = ', i5, ' that indicates a',/
     1 10x,'  WWSP plan is diverting to irrigate but the',/
     1 10x '  destination is not a diversion.',/
     1 10x,'Recommend you revise the destination type or variable',/
     1 10x,'  ioprlim.')
     
cx  760 FORMAT(/, 72('_'),/
cx     1  '  Oprinp; Problem with Operation right = ', a12,/                     
cx     1 '   Destination ID ',a12,' from the operation file (*.opr)',              
cx     1 '   is not in the instream flow station file (*.ifs)')
cx
cx  842 format(/, 72('_'),/
cx     1  '  Oprinp; Problem with Operation right = ', a12,/,                     
cx     1 10x,'water right id ',a12,' in operation file (*.opr)',/
cx     1 10x,'is not in the diversion right file (*.ddr)',/
cx     1 10x,'Note, type 11 and 14 operation rights must have a water',/
cx     1 10x,'right as a source')
cx
cx  923 format(/, 72('_'),/
cx     1 '  Oprinp; Problem with Operating  ID        = ', a12,/
cx     1 '          Operation type                     = ', i5,/     
cx     1 '          Cannot find a corresponding reservoir type -1')
cx     
  932 format(/,72('_'), /,
     1 '  Oprinp; Problem with *.opr rule ID = ', a12, /
     1 '          Operation type                     = ', i5,/     
     1 '          A Reuse Plan (type 4 or 6) is specified ID = ',a12/
     1 '          However a Plan Spill (type 29) is not.',/
     1 '          Recommend you add a Plan Spill operating ',
     1            'right (type 29)')
          
  933 format(/,72('_'), /,
     1 '  Oprinp; Problem with *.opr rule ID = ', a12, /
     1 '          Operation type                     = ', i5,/     
     1 '          An Accounting Plan (type 11) is specified ID = ',a12/
     1 '          However a Plan Spill (type 29) is not.',/
     1 '          Recommend you add a Plan Spill operating ',
     1            'right (type 29)')
      
  934 format(/,72('_'), /,
     1 '  Oprinp; Problem with *.opr rule ID = ', a12, /
     1 '          Operation type                     = ', i5,/     
     1 '          Your source 1 is a type 3 or 5 Plan ID = ',a12,/
     1 '          that is associated with a reservoir. ',/
     1 '          If this is the correct type of plan then',/
     1 '          Recommend you revise source 1 to be a Reservoir ID',/
     1 '          and set source 2 a plan ID')
c
  935 format(
     1    //, 60('_'),/
     1   'Oprinp; Warning C16 the following type 29 (Spill) operating ',
     1           'rules with a plan source spill ',/
     1   '          to a non standard location.  Specifically:',/    
     1   '        When the source is an Admin Plan (type 11) ',
     1           'the destination (spill location) should be ',/
     1   '          the location of the plan.',/
     1   '        When the source is a Changed WR Plan(type 13) ',
     1           'the destination (spill location) should be ',/,
     1   '          at the location of the changed water right that',
     1             'should be located at the node downstream',/
     1   '          from the type 13 plan.',/
     1   '        When the source is a WWSP Supply Plan (type 14) ',
     1           'or WWSP User Plan (type 15), the destination ',/
     1   '          (spill location) should be -1 no spill)',/
     1   '        Recommend you revise the destination to be ',
     1           'consistent with the above unless you are ',/
     1   '          simulating an unusual condition.',// 
     1   '    # Opr Id       OprType Source       PlnType ',
     1   'Destination  Recommended Destination',/
     1   ' ____ ___________  _______ ___________  _______',
     1   ' ____________ _________________________')
     
 9351  format(i5, 1x, a12, i8, 1x, a12, i8, 3(1x,a12)) 
     
  936 format(/,
     1 '  Oprinp; Problem with *.opr rule ID = ', a12, /
     1 '          Operation type                     = ', i5,/          
     1 '          Source 1 a reservoir ID or plan ID = ', a12,/
     1 '          Cannot be found',/
     1 '          Recommend you revise the source 1 ID')
     
  937 format(/,72('_'), /,
     1 '  Oprinp; Problem with *.opr rule ID = ', a12, /
     1 '          Operation type                         = ',i5,/ 
     1 '          Source 1 is plan           = ', a12,/ 
     1 '          Source 1 is plan type                  = ',i5,/     
     1 '          and the variable Oprlimit              = ',i5,/ 
     1 '          When the source is a Changed Water Right Plan ',/
     1 '          (type 13) Oprlimit must be set to 5, 7-9.',/   
     1 '          Similarly if the variable Oprlimit = 5 or 7-9,',/
     1 '          source 1 must be Changed Water Right Plan (type 13)',/
     1 '          Recommend you revise the source plan type',/
     1 '          or the variable Oprlimit')
          
  938 format(/,72('_'), /,
     1 '  Oprinp; Problem with *.opr rule ID         = ', a12, /
     1 '          Operation type                     = ', i5,/     
     1 '          Source 1 is a plan ID = ', a12,/
     1 '          and Source 2 is a plan ID = ', a12,/
     1 '          If you have two sources, source 1 should be a',/
     1 '          a reservoir and source 2 should be a plan ID',/
     1 '          Recommend you revise your source 1 and or 2 IDs')
     
  939 format(/,72('_'), /,
     1 '  Oprinp; Problem with *.opr rule ID         = ', a12, /
     1 '          Operation type                     = ', i5,/     
     1 '          The destination is an operational right = ',a12,/
     1 '          whose destination cannot be found',/
     1 '          Recommend you review the operational right data')
c
c rrb 2021/02/14; Format warning as a table in *.chk file
 940   format(
     1 //, 60('_'),/
     1 '  Oprinp; Warning C5; the following operating rules have',/
     1 '            an operating limit (oprlimit) that is the same',/
     1 '            limit used by another operating rule.  Note:',/
     1 '            - If OprLimit = 3, 4, 8, 9 (Use = Separate) and',/
     1 '              the limit is 1000 af both operating rules can',/
     1 '              seperately divert up to 1000 af for a total',/
     1 '              of up to 2000 af',/
     1 '            - If oprlimit = 2, 4, 7, 9 (Use = Shared)and the',/
     1 '              limit is 1000 af both operating rules share',/
     1 '              up to 1000 af for a total of up to 1000 af',/    
     1 '          Your input data is OK; this is just a reminder',/
     1 '            of how StateMod simulates this type of limit.',//
     1 '    # Opr ID-1     Opr Type Source ID    OprLimit',
     1 ' Op Limit ID  Opr ID-2     Use         ',/
     1 ' ____ ____________ ________ ____________ ________',
     1 ' ____________ ____________ ____________')   
 9401  format(i5, 1x,a12, 1x,i8, 1x,a12, 1x,i8, 3(1x,a12)) 
c
c rrb 2020/05/31; Warn if the same destination is use for two type
c                   46 operating rules
 941   format(
     1    //, 60('_'),/
     1   'Oprinp; Problem C15; the following type 46 operating',/
     1   '          rules have a destination plan ID that is used',/
     1   '          by another type 46 operating rule.',/
     1   '        This is currently not supported.',/
     1   '        Recommend you separate into two destination plans.'//,
 
     1   '    # Opr Id-1     OprType Dest-1       Opr ID-2',
     1   '       Dest-2',/
     1   ' ____ ___________  _______ ___________  ____________',
     1   ' ____________')
     
 9411  format(i5, 1x, a12, i8, 3(1x,a12)) 
c
  942   format(
     1    //, 60('_'),/
     1   'Oprinp; Warning C17 the following type 27 (Plan or ',
     1           'Reservoir Release - Direct) or type 28 (Plan or',/
     1   '          Reservoir Release - Exchange) release water to ',
     1             'an Augmentation (type 1) or T&C (type 2) plan',/,
     1   '          by a carrier with losses that is not supported. ',
     1             'Therfore the losses have been set to zero.'/
     1   '        Recommend you revise the carrier losses to be ',
     1           'zero or request the operating rule be enhanced.',//
      
     1   '    # Opr Id       OprType Destination  PlnType Oprloss',/
     1   ' ____ ____________ _______ ____________ _______ _______')
     
 9421  format(i5, 1x,a12, i8, 1x,a12, i8, f8.0) 
c
c rrb 2015/03/30
     
cx 1185 format(/,72('_'),/
cx     1 '  Oprinp; Problem with Operation right       = ', a12,/                     
cx     1 '          Operation type                     = ', i5,/          
cx     1 10x,'   Source Stream id ',a12,' in operations file (*.opr)',/              
cx     1 10x,' is not in the steam network file (*.rin)')
cx     
 1186 format(/,72('_'),/
     1 ' Opring; Problem with Operating right        = ', a12,/
     1 '         the number of carriers              = ', i5,/
     1 '         exceeds the maximum (maxcary)       = ', i5,/
     1 '         Recommend you reduce the number or enhance',/
     1 '         the StateMod dimension.')
 1191 format(/, 72('_'),/
     1 '  Oprinp; Problem with Operational right     = ', a12,/
     1 '          Operation type                     = ', i5,/     
     1 '          Has a max diversion %              = ', i5,/
     1 10x,'It should be between 0 and 100')

 1192 format(/,
     1 '  Oprinp; Warning for Operational right      = ', a12,/
     1 '          Operation type                     = ', i5,/     
     1 '          Has a max diversion %              = ', i5,/
     1 10x,'This is equivalent to having the rule off.')
     
 1193 format(/, 72('_'),/
     1 '  Oprinp; Problem with operating right       = ',a12,/
     1 '          Operation type                     = ', i5,/     
     1 'The ', i4,' Reservoir Right type -1 has a problem;'/,
     1 10x, '    Cannot find a corresponding operational right type')
     
 1194 format(/, 72('_'), /,
     1'  Oprinp; Problem with Operational right      = ', a12,/
     1 '          Operation type                     = ', i5,/     
     1 10x,' Cannot find source 1 or 2 (a water right) = ', a12,/
     1 10x,' in the direct diversion right (*.ddr) file) or',/
     1 10x,' in the operation (*.opr) file above this opr right')

 1195 format(/, 72('_'), /,
     1'  Oprinp; Problem with Operation right = ', a12,/                     
     1 10x,'   The monthly switches must include:',/
     1 10x,'   a 2 for month on and a -1 for month off you provided:',/
     1 10x, 12i5)
     
cx 1196 format(/, 72('_'), /,
cx     1 '  Oprinp; Problem with Operational right = ', a12,/
cx     1 10x,' type 15 opr rules can only be on (1) or off (0) ',/
cx     1 10x,' for the entire study period.  They cannot begin in ', i5)
cx     
 1197 format(/, 72('_'), /,
     1 '  Oprinp; Problem with Operational right = ', a12,/
     1 10x,' The destination instream flow ', a12,/
     1 10x,' is not downstream of the source water right = ', a12)
     
cx 1198 format(/, 72('_'),/,
cx     1  '  Oprinp; Problem with Operational right = ', a12,/
cx     1 10x,'    The destination reservoir ', a12,/
cx     1 10x,'    is not upstream of the source (direct flow storage)',/
cx     1 10x,'    water right = ', a12)
cx     
 1199 format(/, 72('_'),/,
     1 '  Oprinp; Problem with Operational right = ', a12,/
     1 10x,'    The Rio Grande compact requires the instream flow',/
     1 10x,'    station file (*.ifs) associated with the destination',/
     1 10x,'    instream flow have a monthly data source ',/
     1 10x,'    (iifcom) = 1 if data is provided in *.ifm or '/
     1 10x,'    (iifcom) = 3 if data is provided in *.rfg')
     
 1200 FORMAT(/, 72('_'),/,
     1 '  Oprinp; Problem with operating right        = ',a12,/
     1 ' too many operation rights in *.opr,  maximum = ',I5,/
     1 ' or number of operation rights plus comment is gt max * 2 = ',
     1 i5)
     
 1201 FORMAT(/, 72('_'),/,
     1 '  Oprinp; Problem',/
     1 10x,' The control file sprinkler switch (isprink) = ',i5,/
     1 10x,' but a type 21 (Sprinkler Use) operating rule',/
     1 10x,' is not provided or it is turned off.',/ 
     1 10x,'Recommend you revise the control file',/
     1 10x,'or add a Type 21 operating rule')
     
 1202 FORMAT(/, 72('_'),/,
     1 '  Oprinp; Problem with Operating right       = ',a12,/
     1 '          Operation type                     = ', i5,/     
     1 10x,' Soil Moisture switch on (isoil) = ',i5,' but no type 22', /
     1 10x,' operating rule provided or it is off.',/ 
     1 10x,'To do: Turn off soil moisture use or', 
     1 ' add an operating rule or turn opr rule on')
     
 1203 FORMAT(/, 72('_'),/,
     1 '  Oprinp; Warning for Operating right        = ', a12,/
     1 '          Operation type                     = ', i5,/     
     1 10x,'Soil Moisture switch off (isoil) = ',i5,' but a type 22', /
     1 10x,' operating rule is provided.  Rule turned off & moving on')
     
 1204 FORMAT(/, 72('_'),/,
     1 '  Oprinp; Warning for Operating right        = ',a12,/
     1 '          Operation type                     = ', i5,/     
     1 10x,'Sprinkler switch off (isoil) = ',i5,' but a type 21', /
     1 10x,'operating rule provided.  Rule turned off & moving on')  
     
 1206 format(/, 72('_'),/,
     1 '  Oprinp; Warning with Operational right     = ', a12,/
     1 '          Operation type                     = ', i5,/     
     1 '          The ownership percent              = ',f8.2,/
     1 10x,'A value of zero effectively turns this right off.',/
     1 10x,'recommend you revise the operating rule data',/
     1 10x,'or turn the right off.')
     
     
 1207 format(/, 72('_'),/,
     1 '  Oprinp; Problem with Operational right     = ', a12,/
     1 '          Operation type                     = ', i5,/     
     1 10x,'The percent (account value) must be between 0 and 100',/
     1 10x,'But it equals ', f8.2)
     
     
 1208 format(/, 72('_'),/,
     1 '  Oprinp; Problem with Operatingal right     = ', a12,/
     1 '          Operation type                     = ', i5,/     
     1 10x,'The dimension for the number of replacement reservoirs',/
     1 10x,'(maxrep) of ',i5,' is exceeded',/
     1 10x,'Revise common.for, statem.f & repsort.f')
     
 1209 format(/, 72('_'),/,
     1 '  Oprinp; Problem with Operatingal right     = ', a12,
     1 '          Operation type                     = ', i5,/     
     1 10x,'A Bookover Right (type 8) requires two intern values',/
     1 10x,'intern(1) = the destination water right ID and',/
     1 10x,'intern(2) = the associated OOP right ID',/
     1 10x,'Recommend you revise the *.opr file')
     
 1211 format(/, 72('_'),/,
     1 '  Oprinp; Problem with Operational right           = ', a12,/
     1 '          Operation type                           = ', i5,/
     1 10x,'The source is a reservoir right           = ',a12,/
     1 10x,'Therefore the destination should be a reservoir and ',/
     1 10x,'there should be a carrier structure',/
     1 10x,'Recommend you adjust accordingly')

 1212 format(/, 72('_'),/,
     1 '  Oprinp; Problem with Operational right           = ',a12,/
     1 '          Operation type                           = ', i5,/
     1 10x,'The source is a diversion right          = ',a12,/
     1 10x,'that is located at diversion structure   = ',a12,/     
     1 10x,'that is located at the carrier structure = ',a12,/
     1 10x,'When the source is a water right it automatically',/
     1 10x,'gets limited by its diversion structure',/
     1 10x,'Recommend you remove the carrier structure.')
     
 1213 format(/, 72('_'),/,
     1 '  Oprinp; Problem with Operatingal right           = ', a12,/
     1 '          Operation type                           = ', i5,/
     1 10x,'This rule requires idum = 1 or -13 so that at least one',/
     1 10x,'(intern(i)) value is read. Note:',/
     1 10x,'For a type 40 rule intern(1) is an associated instream ',
     1     'flow water right ID',/
     1 10x,'For a type 41 rule intern(1) is an Out-of-Priority Plan',/
     1 10x,'Recommend you revise the *.opr file')
     
 1214 format(/, 72('_'),/,
     1 '  Oprinp; Problem with Operational right           = ',a12,/
     1 '          Operation type                           = ', i5,/     
     1 '          The source is a diversion right          = ',a12,/
     1 '          That is located at diversion structure   = ',a12,/     
     1 '          That is the same as the destination      = ',a12,/
     1 '          This is not really a carrier unless source 2',/ 
     1 '          says the right is operated at a different location',/
     1 '          Recommend you revise the operating rule.')
     
 1215 format(/, 72('_'),/,
     1 '  Oprinp; Problem with Operational right           = ',a12,/
     1 '          Operation type                           = ', i5,/     
     1 '          The source right is administered at a ',/
     1 '            diversion (ciopso(2))                  = ',a12,/
     1 '          This diversion is not the source right or a carrier',/
     1 '          Recommend you revise the operating rule.')

cx 1216 format(2x, i5, 1x, a12, 1x, a24)       
cx 
cx 1217 format(/, 72('_'),/,
cx     1 '  Oprinp; Problem with Operational right           = ',a12,/
cx     1 '          Operation type                           =',  i3,/     
cx     1 10x,      'The source                               = ', a12,/
cx     1 10x,      'The destination                          = ',a12,/
cx     1 10x,      'The diversion location                   = ',a12,/
cx     1 10x,'When the source is a diversion water right',/
cx     1 10x,'A type 45 rule requires at least 1 carrier (with loss)',/
cx     1 10x,'be provided if a destination is not the same as the',/
cx     1 10x,'diversion location. Also if the source is a diversion',/
cx     1 10x,'water right the first carrier must be the source',/
cx     1 10x,'structure. Recommend you revise the operating rule.')
cx 
 1218 format(/, 72('_'),/,
     1 '  Oprinp; Problem with Operational right           = ',a12,/
     1 '          Operation type                           =',  i3,/
     1 '          Variable Oprlimit                        = ',f5.0,/
     1 10x,'Which indicates water right sharing is occurring',/
     1 10x,'This capability is not currently active',/
     1 10x,'Recommend you revise the operating rule.')
     
 1220 FORMAT(/, 72('_'), /,
     1 '  Oprinp; Warning for Operating ID                 = ', a12,/
     1 '          Operation type                           = ', i5,/
     1 10x,'It is Turned off because Variable efficiency ',/
     1 10x,'(iefmfax) is off',/
     1 10x,'Called at Location ', i5)
     
 1221 FORMAT(/, 72('_'), /,'  Oprinp; ',
     1'Warning for Operating right type = ',i2,' ID = ', a12,/
     1 10x,'Turned off because the Annual Time Series file',/
     1 10x,'(itsfile) is off')

 1222 FORMAT(/, 72('_'), /,'  Oprinp; ',
     1'Warning Operating type = ',i2,' ID = ', a12,/
     1 10x,'It is turned off because Wells (iwell) are off') 
     
 1223 FORMAT(/, 72('_'), /,'  Oprinp; ',
     1 'Warning for Operating right type = ',i2,' ID = ', a12,/
     1 10x,'It is turned off because sprinklers (isprink) are off') 

 1224 FORMAT(/, 72('_'),/, '  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'Type 20 (SJRIP) in *.opr is on but ',/
     1 10x,' SJRIP control is off (isjrip=0 in *.ctl).')
     
 1250 format(/, 72('_'), /,
     1'  Oprinp;  Stopped in Oprinp, see the log file (*.log)')
     
cx 1251 format(/, 72('_'), /,'  Oprinp; ',
cx     1'Problem with Operating right type = ',i2,' ID = ', a12,/
cx     1 10x,'Source 2 ID          = ', a12,' and the',/
cx     1 10x,'The Plan ID          = ', a12,/
cx     1 10x,'Note: A Type 10 (General Replacement) rule expects',/
cx     1 10x,'      No OOP Plan ID when no source 2 is specified',/
cx     1 10x,'      A OOP Plan ID when source 2 is specified',/
cx     1 10x,'Recommend you correct the operating rule file (*.opr)')
cx
 1252 FORMAT(/, 72('_'), /,'  Oprinp; ',
     1'Warning for Operating right type = ',i2,' ID = ', a12,/
     1 10x,'It is turned off because the Source ID or',/
     1 10x,'destination ID is turned off',/
     1 10x,'Called at Location ', i5)
     

 1253 format(/, 72('_'), /,'  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'The Plan ID        = ', a12,' and the',/
     1 10x,'Plan Type          = ',i2,10x,' are incorrect.',/
     1 10x,'Note: A Type 10, General Replacement, rule expects',/
     1 10x,'        an OOP Plan (type = 9)',/
     1 10x,'      A Type 27, 28, or 29 expects a T&C Plan (type 1)'/
     1 10x,'        or a WWSP Plan (type 15)',/
cx   1 20x,'      A Type 53 expects a WWSP Plan (type 14)',/
     1 10x,'Recommend you correct the operating rule file (*.opr)',
     1        ' or the plan (*.pln) file')

 1254 format(/, 72('_'), /,'  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'the Source ID = ', a12,' and the first carrier = ', a12,/
     1 10x,'This is not really an exchange or bypass',/
     1 10x,'recommend you correct the operating rule file (*.opr)',/
     1 10x,'or if the diversion does pass through the source',/
     1 10x,'recommend you use a type 45 (carrier with loss). ')

          
     
 1255 format(/, 72('_'), /,'  Oprinp; ',
     1 'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'The destination ID = ', a12,' and the',/
     1 10x,'The Plan ID        = ', a12,' and the',/
     1 10x,'Plan Type          = ',i4,11x,' are inconsistent.',/
     1 10x,'Note: A T&C or Well Aug Plan should be type:     1 or 2',/
     1 10x,'      A Reservoir Reuse Plan should be type:     3 or 5',/
     1 10x,'      A Non Reservoir Reuse Plan should be type: 4 or 6',/
     1 10x,'      A Transmountain Reuse Plan should be type: 7',/
     1 10x,'      A Recharge Plan should be type:            8',/     
     1 10x,'      A OOP Plan should be type:                 9',/
     1 10x,'      A Special Well Aug Plan should be type:   10',/
     1 10x,'      A Accounting Plan should be type:         11',/
     1 10x,'      A WWSP Supply Plan should be type:        14 or 15',/
     1 10x,'Recommend you revise the plan or operating rule data')
     
12551 format(/, 72('_'), /,'  Oprinp; ',
     1 'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'The variable Creuse is a plan = ', a12,' type ',i4,/
     1 10x,'That is  inconsistent.',/
     1 10x,'The variable Creuse should be a WWSP Supply Plan type 14',/
     1 10x,'Recommend you revise the plan or operating rule data.')
     
12552 format(/, 72('_'), /,'  Oprinp; ',
     1 'Problem with Operating right type = ',i2,' ID = ', a12,/,
     1 10x,'Source 1 is a type 14 (WWSP-Supply) plan = ', a12,/, 
     1 10x,'This is the second WWSP-User supply provided but ',/,
     1 10x,'only 1 is currently allowed or the operating rule',/,
     1 10x,'is operating in more than 1 month that is now allowed',/
     1 10x,'Recommend you revise the plan type or operating rule data')
     
12553 format(/, 72('_'), /,'  Oprinp; ',
     1 'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'The monthly on/off switch for calendar month ', a4,
     1     ' = ', i5,/
     1 10x,'that indicates the rule operates less than a full month',/
     1 10x,'This operation is not supported for a type 46 opr rule ',/
     1 10x,'simulating a WWSP plan',/
     1 10x,'Recommend you revise the plan type or operating rule data')

 1256 format(/, 72('_'), /,'  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'The source 1 (Plan) = ', a12,' and the',/
     1 10x,'Plan Type           = ',i1,11x,' are inconsistent.',/
     1 10x,'Note: A Reservoir Reuse Plan should be type:      3 or 5',/
     1 10x,'      A Non Reservoir Reuse Plan should be type:  4 or 6',/
     1 10x,'      A Transmountain Import Plan should be type: 7',/
     1 10x,'      A Recharge Plan should be type:             8',/
     1 10x,'Revise the plan or operating rule data')
     
12561 format(/, 72('_'), /,'  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'The source 1 (Plan) = ', a12,' and the',/
     1 10x,'Plan Type           = ',i1,11x,' are inconsistent.',/
     1 10x,'The plan type must be a:',/
     1 10x,'Type 4 (non Res reuse) or Type 8 (recharge plan)',/
     1 10x,'Type 11 (administrative) or Type 13 (changed WR)',/ 
     1 10x,'Turning this right off and continuing to simulate')

cx 1257 format(/, 72('_'), /,'  Oprinp; ',
cx     1'Problem with Operating right type = ',i2,' ID = ', a12,/
cx     1 10x,'The source 1 (Plan) = ', a12,', the',/
cx     1 10x,'Plan Type           = ',i1,11x,', and the',/
cx     1 10x,'The source 2 (Res)  = ', a12,' are inconsistent.',/
cx     1 10x,'A source 1 Recharge Plan should have a',/
cx     1 10x,'a source 2 Reservoir ID',/
cx     1 10x,'Revise the plan or operating rule data')
cx  
 1258 format(/, 72('_'), /
     1 '  Oprinp; ',
     1 'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'Diversion Type (CdivTyp) = ',a12, /
     1 10x,'For a DIVERSION destination it should be Diversion ',
     1     'or a Depletion',/
     1 10x,'For a RESERVOIR destination it should be Diversion',/
     1 10x,'For a ISF destination it should be Diversion',/
     1 10x,'For a type 52 operating rule it should be Percent ',
     1     'or Volume',/
     1 10x,'Recommend you revise the operating rule data.',/
     1 10x,'Note the file currently requires the variables OprLoss',
     1     ' and OprLimit')         
     
cx 1259 format(/, 72('_'),/ '  Oprinp; ',
cx     1 'Problem with Operating right type = ',i2,' ID = ',a12,/
cx     1 10x,'Destination = ',a12,' a ', a9,/
cx     1 10x,'Source 2 = ',a12,/     
cx     1 10x,'A destination = Reservoir should have ',/
cx     1 'Source 2 = Diversion',/
cx     1 10x,'Revise the operating rule data')
cx     
 1260 format(/, 72('_'),/
     1 '  Oprinp; Operating right data read (or estimated)',//
     1 '# ID        Name                    NA                  ',
     1 '  Admin#   # Str  On/Off Dest Id     Dest Ac  Sou1 Id     ',
     1 'Sou1 Ac  Sou2 Id     Sou2 Ac     Type Reuse Plan   ',
     1 'DivType       Loss(%)   Limit   ioBeg   ioEnd    #'/
     1 '#__________eb______________________eb__________exxxxb___',
     1 '_______eb______eb______exb__________eb______exb__________e',
     1 'b______exb__________eb______eb______exb__________ex',
     1 'b__________exb______eb______eb______eb______eb___e')
     
     
 1261 format(/, 72('_'),/, '  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'the Destination ID = ', a12,' and carrier ID = ', a12,/
     1 10x,'are located at the same point on the river.',/
     1 10x,'This is not allowed. Recommend you revise the operating',/
     1 10x,'rule file (*.opr)')
     
 1262 format(/, 72('_'),/, '  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'the T&C Return Type  = ', i5, ' is inconsistent with',/
     1 10x,'the data provided in the plan return file (*.prf)',/
     1 10x,'Recommend you review the documentation and revise ',/
     1 10x,'the operating rule file (*.opr) or the plan return',/
     1 10x,'file (*.prf)')
     
 1263 format(/, 72('_'),/, '  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'The plan specified = ', a12,/
     1 10x,'is a type = ', i3, ', that is incorrect',/
     1 10x,'For a diversion destination it should be a type 1',/
     1 10x,'For a well augmentation supply it should be a type 2',/
     1 10x,'For a reservoir destination it should be a type 3 or 5',/
     1 10x,'For a Out-of-Priority Plan it should be type 9',/
     1 10x,'Recommend you revise the plan type')
     
 1264 format(/, 72('_'),/, '  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'A type 8 with a reservoir destination requires',/
     1 10x,'variable dumx > 0 and so that an Out-of-Priority water',/
     1 10x,'right can be read as variable cntern(1)',/
     1 10x,'Note dumx = ', i5, ' and cntern(1) = ', a12)
     
 1265 format(/, 72('_'),/, '  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'The source reservoir right = ', a12,/
     1 10x,'is tied to reservoir ID = ', a12,/
     1 10x,'It should be the same as the destination ID = ',a12,/
     1 10x,'Recommend you revise the operating rule.') 
     
 1266 format(/, 72('_'),/, '  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'The source or destination plan = ', a12,' a type = ', i3,/
     1 10x,'For this operating rule the plan type should be a:',/
     1 10x,'  Type 1 for a Term and Condition Plan',/
     1 10x,'  Type 2 for a Well Augmentation Plan',/
     1 10x,'  Type 9 for an Out-of-Priority Plan',/
     1 10x,'Recommend you revise the plan type.')
     
12660 format(/, 72('_'),/, '  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'The variable Oprlimit = ',i2,/
     1 10x,'A value of 1 or 6 is not currently supported but',/
     1 10x,'is being reserved for potential use in the future.',/
     1 10x,'Recommend you revise the variable Oprlimit',/
     1 10x,'to 0, 2-5, or 7-9.')      
          
12661 format(/, 72('_'),/, '  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'The destination plan = ', a12,' a type = ', i3,/
     1 10x,'For this operating rule the plan type should be a:',/
     1 10x,'  Type 1 for a Term and Condition Plan',/
     1 10x,'  Type 2 for a Well Augmentation Plan',/
     1 10x,'  Type 10 for a Special Well Augmentation Plan',/
     1 10x,'  Type 11 for an Accounting Plan',/
     1 10x '  Type 13 for a Changed Water Right Plan',/
     1 10x,'Recommend you revise the plan type.')
     
12662 format(/, 72('_'),/, '  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'The variable Oprlimit             = ',i2,' and',/
     1 10x,'The operating rule specified in row 4     = ', a12,/
     1 10x,'When the variable Oprlimit = 2, 4, 7 or 9',/
     1 10x,'The operating rule specified in row 4 should be',/
     1 10x,'a type 47 operating rule',/
     1 10x,'Recommend you revise the operating rule provided in row 4.')
              
 1267 format(/, 72('_'),/, '  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'The source plan specified = ', a12,' is a type = ', i3, 
     1 10x,'For this operating rule the plan type should be a:',/
     1 10x,'  Type 3 for a CU Reuse Reservoir',/
     1 10x,'  Type 4 for a CU Reuse Diversion',/
     1 10x,'  Type 5 for a Tmtn Reuse Reservoir',/
     1 10x,'  Type 6 for a Tmtn Reuse Diversion',/
     1 10x,'Recommend you revise the operating rule or plan type.')
     
     
 1268 format(/, 72('_'),/, '  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'The source 2 plan specified = ', a12,' is a type = ', i3, /
     1 10x,'For this operating rule the plan type should be a:',/
     1 10x,'  Type 9 for Out-of-Priority Diversion or',/
     1 10x,'  Type 10 for Out-of-Priority Storage or',/
     1 10x,'  Type 15 for a WWSP User Plan',/
     1 10x,'Recommend you revise the operating rule or plan type.')

 1269 format(/, 72('_'),/, '  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'Variable iopsou(4,k) = ',i5, ' but it should be > or = 1',/
     1 10x,'to indicate a monthly (1) or annual (>0) diversion limit',/
     1 10x,'Recommend you revise the operating rule.')
     
 1270 format(/, 72('_'),/,
     1 '  Oprinp; Problem with Operational right     = ', a12,/
     1 '          Operation type                     = ', i5,/     
     1 '          When source 2 (ciopso(2) is an operational right',/
     1 '          iopsou(4,k) should be 0, not ', i5'.')   
     
 1271 format(/, 72('_'),/,
     1 '  Oprinp; Problem with Operational right     = ', a12,/
     1 '          Operation type                     = ', i5,/     
     1 '          Expect 1 to 10 destinations but oprlimit = ', i5,/
     1 '          Recommend you revise the operating rule file.')
     
 1272 format(/, 72('_'),/, '  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'The ', a12, ' is a plan = ', a12,' type = ', i3,/
     1 10x,'For this operating rule the plan type should be a ',/
     1 10x,  'type ',i3, ' or type ', i3, ' or type ', i3,
     1                   ' or type ', i3, ' where 0 is NA',/
     1 10x,'Recommend you revise the plan type.')

12721 format(/, 72('_'),/, '  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'The ', a12, ' is a plan = ', a12,' type = ',i3,/
     1 10x,'That is  inconsistent.',/     
     1 10x,'For this operating rule the ',a12, /
     1 10x,'Should be a WWSP Supply plan type       = ',i3,/
     1 10x,'Recommend you revise the plan type.')

12722 format(/, 72('_'),/, '  Oprinp; ',
     1'Problem with Operating rule type = ',i2,' ID = ', a12,/
     1 10x,'The operating rule has a constraint = ', a12,/
     1 10x,'That is an operating rule type      = ', i3,/
     1 10x,'That is  inconsistent. For this operating rule the',/
     1 10x,'constraint should be a type 47 (Accounting Plan Limit)',/
     1 10x,'Recommend you revise the constraint to be a type 47 ',
     1     'operating rule')      
        
 1273 format(/, 72('_'),/, '  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'The destinations specified % sum to = ', f8.3,/
     1 10x,'For this operating rule the sum should be = 100%',/
     1 10x,'Note if a plan is off the % is not counted',/
     1 10x,'Recommend you revise the plan data.')

cx 1274 format(/, 72('_'),/, '  Oprinp; ',
cx     1'Problem with Operating right type = ',i2,' ID = ', a12,/
cx     1 10x,'The return flow fractions equal ', f8.1, ' not 100',/
cx     1 10x,'Recommend you revise your return percents.')
cx
 1275 format(/, 72('_'),/, '  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'Source account 2 (iopsou(4,k)) = ', i5,' but it'/
     1 10x,'should be 1 or 2 or 3; where:',/
     1 10x,'  1 indicates a STANDARD return pattern is used and',/
     1 10x,'  2 indicates a FIXED return pattern is used',/
     1 10x,'  3 indicates a MIXED return pattern is used',/
     1 10x,'Recommend you revise the operating rule file.')
     
 1276 format(/, 72('_'),/, '  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'This operating rule type is not currently operational',/
     1 10x,'Note the old type 26 rule is now a type 47 rule',/
     1 10x,'Recommend you revise the operating rule file.')
     
 1277 format(/, 72('_'),/, '  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'This operating rule type is not defined',/
     1 10x,'Recommend you revise the operating rule file.')
     
cx 1278 format(/, 72('_'), /,'  Oprinp; ',
cx     1'Problem with Operating right type = ',i2,' ID = ', a12,/
cx     1 10x,'The source 1 (Plan) = ', a12,' and the',/
cx     1 10x,'Plan Type = ',i2,' (recharge) requires',/
cx     1 10x,'Source 2 be a reservoir, not = ', a12,/
cx     1 10x,'Recommend you revise the operating rule data.')     
cx     
cx 1279 format(/, 72('_'), /,'  Oprinp; ',
cx     1'Problem with Operating right type = ',i2,' ID = ', a12,/
cx     1 10x,'The miscellaneous limit (oprlimit) = ', i5,/
cx     1 10x,'When this value is 0 no additional data is expected',/
cx     1 10x,'When this value is 2 a diversion ID is read',/
cx     1 10x,'When this value is 3 a reservoir ID is read',/
cx     1 10x,'Recommend you revise the operating rule data.')     
cx   
cx 1280 format(/, 72('_'),/, '  Oprinp; ',
cx     1'Problem with Operating right type = ',i2,' ID = ', a12,/
cx     1 10x,'The source plan = ', a12,' a type = ', i3,/
cx     1 10x,'For this operating rule the plan type should be a:',/
cx     1 10x,'  Type 11 (administrative) for the South Platte Compact',/
cx     1 10x,'Recommend you revise the plan type.') 
cx    
 1281  FORMAT(/,72('_'),/
     1  '  Oprinp; Warning See *.chk for details regarding: ',a32)

     
 1282 format(/, 72('_'),/, '  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'Source 2 = ', a12, ' cannot be found or is set to NA',/
     1 10x,'This is not allowed. Recommend you revise the operating',/
     1 10x,'rule file (*.opr).')
 
 1283 format(/, 72('_'),/, '  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x, 'The source and destination reservoirs must be the same',/
     1 10x,'The source reservoir = ', a12,/
     1 10x,'The destination reservoir ', a12)
 
 1284 format(/, 72('_'),/, '  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x, 'Variable cdivtyp = Depletion that is not supported'/
     1 10x, 'for this operating rule type.')
 
 1285 format(/, 72('_'),/, '  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x, 'Either the account, or Baseflow %, or ',/
     1 10x, 'Enhanced baseflow % are = 0',/
     1 10x, 'Recommend you correct your input data.')

 1286 format(/, 72('_'),/, '  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x, 'The destination is tied to plan = ', a12,' type = ',i5,/
     1 10x, 'but a type 52 operating rule requires a type 14',
     1      ' (wwsp) plan type',/
     1 10x, 'Recommend you correct your input data.')
     
 1287 format(/, 72('_'),/, '  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'When the source is a WWSP plan (type 14), monthly',/
     1 10x,'on-off data is required and the operating rule is',/
     1 10x,'only allowed to operate once per year (e.g. only 1 month',/
     1 10x,'can be turned on for the year).',/
     1 10x,'Recommend you revise to include 1 on-off value.')
                               
                               
 1288 format(/, 72('_'),/, '  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'Variable creuse = ', a12,' that is a plan type', i3,/
     1,10x,'When the variable creuse is a type 14 plan, there must ',/
     1 10x,'be a type 46 operating rule with a source = ', a12,/
     1 10x,'but none can be found.',/
     1 10x,'Recommend you revise variable creuse or add a  type 46',/
     1 10x,'operating rule with the source = ', a12'.')

cx 1289 format(/, 72('_'),/, '  Oprinp; ',
cx     1'Problem with Operating right type = ',i2,' ID = ', a12,/
cx     1 10x,'Variable creuse = ', a12,' that is a plan type', i3,/
cx     1 10x,'For this operating rule variable creuse must be a ',/
cx     1 10x,'Reuse Plan (type 3, 4, 5 or 6) or a ',/
cx     1 10x,'WWSP User Plan (type 15)',/
cx     1 10x,'Recommend you revise variable creuse')
cx                                                           
 1290   format(/, 72('_'),/, '  Oprinp; ',
     1 'Warning the version of the operating rule',/
     1 12x,'file (*.opr) provided is not specified.',/
     1 12x,'Suggest you add the following to the',/
     1 12x,'first line of the operating rule (*.opr) file to',/
     1 12x,'indicate the version of the file being provided',/
     1 12x,'#FileFormatVersion X; where X is the version #',/
     1 12x,'See Section 4.13 of the documentation for more ',/
     1 12x,'additional information.')          
     
cx 1291 format(/, 72('_'),/, '  Oprinp; ',
cx     1'Problem with Operating right type = ',i2,' ID = ', a12,/
cx     1 10x,'This operating right has monthly on-off data provided',/
cx     1 10x '(dumx = 12 or less than -12) and the destination',/
cx     1 10x,'provided is not Multiple (free format option).',/
cx     1 10x,'Recommend you revise to not include monthly data',/
cx     1 10x,'or provide the destination data to Multiple (free',/
cx     1 10x,'format option).')
cx     
 1292 format(/, 72('_'),/,'  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 '          Expect the destination to be Multiple in ',/
     1 '          order to allow 1 to 10 destinations to be',/
     1 '          specified.',/
     1 '          Recommend you revise the operating rule file.')
     
 1300 format(/,72('_'),/
     1 '  Oprinp; Problem with Operating Right ', a12, ' Type = ',i2,/
     1 '          The destination ID = ',a12, ' cannot be found',/
     1 '          Recommend you confirm it exists and is a structure',/
     1 '          type supported by this operating rule.')
     
 1330  format(/,72('_'),/
     1 '  Oprinp; Problem Reservoir Plan ID ',a12,/
     1 '          Is tied to more than one Reservoir and Account',/
     1 '          Reservoir ID 1 = ', a12,' Account 1 = ', i5,/
     1 '          Reservoir ID 2 = ', a12,' Account 2 = ', i5,/
     1 '          Recommend you revise the plan data or ',/
     1 '          the operating rule data.')
 1340  format(/,72('_'),/
     1 '  Oprinp; Problem reservoir ID ',a12,' has one account ',/
     1 '          Tied to more than one Reservoir Plan',/
     1 '          Recommend you revise the plan data or ',/
     1 '          the operating rule data.')
     
 1350  format(/, 72('_'),/, 
     1 '  Oprinp; Problem with Operating right type = ',i2,
     1            ' ID = ', a12,/
     1 '          It is tied to a plan ID = ', a12,/
     1 '          It must be tied to a Out-of-Priority Plan',
     1            ' (type 9)',/
     1 '          Recommend you revise the plan data in ',/
     1 '          the operating rule data.')
     
 1352  format(/, 72('_'),/, 
     1 '  Oprinp; Problem with Operating right type = ',i2,
     1            ' ID = ', a12,/
     1 '          The source well right = ', a12,/
     1 '          Cannot be found in the well station file (*.wer)',/
     1 '          Recommend you revise the operating rule file',/
     1 '          file (*.opr) or the well right file (*.wer)',/
     1 '          Continuing with the right turned off.')
     
 1360  format(/, 72('_'),/, 
     1 '  Oprinp; Problem with Operating right type = ',i2,
     1            ' ID = ', a12,/
     1 '          The source diversion right ID = ',a12,/
     1 '          The source diversion ID       = ',a12,/
     1 '          The destination diversion ID  = ',a12 ,/
     1 '          The source and destination ID should be the same',/
     1 '          Recommend you revise the source or destination in ',
     1 '          the operating rule data.')
     
 1370  format(/, 72('_'),/, 
     1 '  Oprinp; Problem with Operating right type     = ', i2,/
     1 '          Operating right ID                    = ', a12,/
     1 '          has a T&C requirement with the source = ', a12,/
     1 '          but operating rule ID                 = ', a12,/
     1 '          has a T&C requirement with the use    = ', a12,/
     1 '          Therefore the T&C requirments are included twice',/
     1 '          Recommend you remove the T&C requirement on one',/
     1 '          of the above operating rules.')

 1372  format(/, 72('_'),/, 
     1 '  Oprinp; Problem with Operating right type     = ', i2,/
     1 '          Operating right ID and                = ', a12,/
     1 '          Operating right ID                    = ', a12,/
     1 '          have the same source                  = ', a12,/
     1 '          and at least one has a constant percent specified.',/
     1 '          This is not allowed because the avaialble flow ',/
     1 '            gets reduced after it is called the first time.',/
     1 '          Recommend you have the percent calculated by an',
     1              ' operating rule.')
     
 1380  format(/, 72('_'),/,      
     1 '  Oprinp; Problem with Operating right type     = ', i2,/
     1 '          Operating right ID                    = ', a12,/
     1 '          has a water right source              = ', a12,i5,/
     1 '          Operating rule ID                     = ', a12,/
     1 '          has the same water right source       = ', a12,i5,/
     1 '          This is not allowed because a type 24 rule',/
     1 '          (direct flow exchange) and a type 25 rule',/
     1 '          (direct flow bypass) must completely  control',/
     1 '          the source water right.',/
     1 '          Recommend you exchange or bypass the water',/
     1 '          right to an Administration Plan, then release',/
     1 '          from that plan to as many users as necessary.')
c
 1382 format(/, 72('_'), /,'  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'The destination (Plan) = ', a12,' and the',/
     1 10x,'Plan type              = ',i2,10x,' are inconsistent.',/
     1 10x,'Note: The only destination plan type allowed for a',/
     1 10x,'      type 26 operating rule is a type 13, Changed',/
     1 10x,'      Water Right Plan.')
c
 1384 format(/, 72('_'), /,'  Oprinp; Warning',/
     1 10x,'Operating right type =  ',i2,' ID = ', a12,/
     1 10x,'Books water from reservoir ID = ', a12,' account ', i5,/
     1 10x,'to destination reservoir   ID = ', a12,' account ', i5,
     1 ' and',/     
     1 10x,'Operating right type =  ',i2,' ID = ', a12,/
     1 10x,'Books water from reservoir ID = ', a12,' account ', i5,/
     1 10x,'to destination reservoir   ID = ', a12,' account ', i5,//
     1 10x,'This is OK but you may need to set variable ciopso(2)',/
     1 10x 'to an operating rule Id, iopsou(4,1) to 0 and oprlimit',/
     1 10x,'to 1 to insure the system does not continuously',/
     1 10x,'reoperate by booking water into and out of an account',/
     1 10x,'that are located within the same reserovoir.',/
     1 10x,'See section 4.13.6 of the documentation.')
        
 1386 format(/, 72('_'),/, '  Oprinp; ',
     1 'Problem with the John Martin (type 53 and 54) Opr rules  ',/
     1 10x,'The JM Flow  (type 54) must be senior to a JM Store ',
     1 '(type 53)',/
     1 10x,'The JM Store (type 53) ID & priority = ', a12, f12.5,/
     1 10x,'The JM Flow  (type 54) ID & priority = ', a12, f12.5,/
     1 10x,'Recommend you correct your input data.')
      
 1388 format(/, 72('_'),/, '  Oprinp; ',
     1 'Problem with the John Martin (type 53 and 54) Opr rules  ',/
     1 10x,'When one is specified the other is required',/
     1 10x,'ID = ', a12, ' is a type ', i2 ' but no type ', i2, 
     1 ' was found',/
     1 10x, 'Recommend you correct your input data.')
     
 1390 format(/, 72('_'),/, '  Oprinp; ',
     1 'Problem with the John Martin (type 53 and 54) Opr rules  ',/
     1 10x,'both source 1 and 2 must be the same river node',/
     1 10x,'The JM Store (type 53) ID & Source 1 ` = ', a12, a12,/
     1 10x,'The JM Flow  (type 54) ID & source 1   = ', a12, a12,/
     1 10x,'The JM Store (type 53) ID & Source 2 ` = ', a12, a12,/
     1 10x,'The JM Flow  (type 54) ID & source 2   = ', a12, a12,/
     1 10x, 'Recommend you correct your input data.')
        
cx 2019   format(/,60('_'),/
cx     1  '  Oprinp; Detailed output for type ', i5,/
cx     1  10x, 'ID and Type      = ', a12, i5,/
cx     1  10x, 'Destination      = ', a12, i5,/        
cx     1  10x, 'Source 1         = ', a12, i5,/
cx     1  10x, 'Source 2         = ', a12, i5,/
cx     1  10x, 'Reuse            = ', a12, i5,/
cx     1  10x, 'OprLimit         = ', f12.0,i5,/
cx     1  10x, 'Diversion type   = ', a12,/
cx     1  10x, 'Destination type = ', i5)  
cx        
 2020   format(/,60('_'),/
     1  '  Oprinp; Detailed output for type ', i5,/
     1  10x, 'ID and Type      = ', a12, i5,/
     1  10x, 'Destination      = ', a12, 2i5,/        
     1  10x, 'Source 1         = ', a12, 2i5,/
     1  10x, 'Source 2         = ', a12, 2i5,/
     1  10x, 'Reuse            = ', a12, i5,/
     1  10x, 'OprLimit         = ', f12.0,i5,/
     1  10x, 'Diversion type   = ', a12,/
     1  10x, 'Destination type = ', i5)
     
 2021   format(
     1  10x, 'Junior Right     = ', a12, i5)
 2022   format(    
     1  10x, 'Opr Right        = ', a12, i5)
     
 2023   format(/,60('_'),/
     1  '  Oprinp; Detailed output for type ', i5,/
     1  10x, 'ID and Type      = ', a12, i5,/
     1  10x, 'Destination      = ', a12, i5,/                    
     1  10x, 'Source 1         = ', a12, i5,/
     1  10x, 'Reuse            = ', a12, i5,/
     1  10x, 'OprLimit         = ', 6x, f12.0,/
     1  10x, 'Diversion type   = ', a12,/
     1  10x, 'Destination type = ', 12x, i5)
c   
c               Warning for type 45 spill rule (oprlimit = 7)
 2024 format(/, 72('_'), /,
     1 '  Oprinp; Problem with *.opr rule ID = ', a12,' type ', i5,/  
     1 10x,'The miscellaneous limit (oprlimit) = ', f5.0,' and ', i5,/
     1 10x,'The source is not a res. water right, check: ', a12,/
     1 10x,'The destination is not a reservoir, check:   ', a12,/ 
     1 10x,'When oprlimit is 7 the ',/
     1 10x,'  destination reservoir should be the same as the',/
     1 10x,'  "spill order" reservoir and the accounts should',/
     1 10x,'  be different, check:',/,
     1 10x,'Destination reservoir, pointer & account =  ',a12,1x,2i5,/
     1 10x,'Spill Order reservoir, pointer & account =  ',a12,1x,2i5)
c
c
c _________________________________________________________
c

      END
