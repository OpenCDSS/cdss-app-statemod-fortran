c
c *********************************************************
c
c
      SUBROUTINE OPRINP(IIN)
c
c
c _________________________________________________________
c	Program Description
c
c       Oprinp; It reads operational right data
c
c _________________________________________________________
c       Update History
c
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
c
c rrb 04/08/24; Add type 23 Downstream Call (variable amount and admin date)
c		idcall = 0 no Type 23 right (downstream call)
c		idcall = k = pointer to the operating right that is a 
c                            downstream call
c rrb 04/06/20; Type 25 Direct Flow Bypass to a Diversion, 
c                       Reservoir or carrier
c rrb 04/06/20; Type 24 Direct Flow Exchange to a Diversion, 
c                       Reservoir or carrier
c rrb 04/06/20; Type 23 Downstream call
c
c rrb 03/08/18; Revise to allow random file read
c
c rrb 02/10/22; Allow type 10 to handle monthly on/off switches
c
c rrb 02/02/25; Revised to allow ieffmax = 2 
c               which allows IWR data to be read and limit',/
c               reservoir releases to occur only when an IWR',/
c               exists if the reservoir release variable is > 0',/
c
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
c
c		
c rrb 01/12/26; Revmove equivlence & revised dumx = dumc & idumx=idumc
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
c rrb 99/05/24; Type 15 Interruptable supply added
c rrb 99/08/10; Type 14 Carrier with an annual limit on diversion
c                       when iopsou(2,k) .gt. 1)
c rrb 99/08/30; Began to simplify input with type 15 and 16 approach
c                 to read data
c
c
c _________________________________________________________
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
c               Type 15 Interruptable supply & alternate point
c               Type 16 Direct flow storage
c               Type 17 Rio Grande Compact for Rio Grande
c               Type 18 Rio Grande Compact for Conejos
c               Type 19 Split Channel
c               Type 20 San Juan RIP
c               Type 21 Sprinker use
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
c
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'

      real*8 rtem
c
      character*12 cidvri,  ciopde,  ciopso1, ciopso2, ciopso3,
     1             ciopso4, ciopso5, blank,   czero,  cx
      character recin*256, creuse*12, rec12*12, rec2*2,
     1          csource*12,cdest*12,  rec132*132, rec1*1,
     1          cassoc*12, cdestyp*12
      dimension ntype(50), oprtype(50)      
      character oprtype*25
c        
c rrb 209/01/26; Correction; initilize ntype to a dimension of 50
      data ntype/50*0/
      data oprtype/
     1  'Res to ISF',                  'Res to Diversion, etc.',
     1  'Res to Carrier',              'Res Exchange to a Diversion',
     1  'Res to Storage Exchange',     'Bookover',    
     1  'Res to a Carrier by Exchange','OOP Bookover',  
     1  'Release to Target',           'General Replacement Res',
     
     1  'Carrier to a Ditch or Res',    
     1  'Reoperate',                   'La Plata Compact',
     1  'Carrier with Const Demand',   'Interruptable Supply',
     1  'Direct Flow Storage',         'Rio Grande Compact - RG',
     1  'Rio Grande Compact - CR',     'Split Channel',
     1  'San Juan RIP',            
     
     1  'Wells with Sprikler Use',    
     1  'Soil Moisture Use',           'Downstream Call',
     1  'Direct Flow Exchange',        'Direct Flow Bypass',
     1  'NA',                          'Plan Use Direct',
     1  'Plan Use Exchange',           'Plan Spill',
     1  'Reservoir Re Diversion',      
     
     1  'Carrier with Reuse',          'Reuse Plan Direct',  
     1  'Reuse Plan Exchange',         'Bookover with Reuse',     
     1  'Import with Reuse',           'Seasonal Water Right',  
     1  'Augmentation Well',           'OOP Diversion',
     1  'Alternate Point Diversion',   'South Platte Compact', 
     
     1  'Storage with Special Limits', 'Plan Reset',            
     1  'In-Priority Well Supply',     'Recharge Well', 
     1  'Carrier with Loss',           'Multiple Ownership',
     1  'Administration Plan Limits',  'Reuse to a Plan Direct', 
     1  'Reuse to a Plan Exchange',    ' '/
c
c _________________________________________________________
c
c		Step 1; Detailed Checks
c
c		iout = 0 no details
c		       1 details
c          2 summary (echo input and opr right type)
c		       3 summary of on/off dates
c		iecho= 0 do not echo *.opr input to *.chk
c		       1 do echo *.opr input to *.chk
c		iechoX=0 do not echo *.opr input to *.chk
c		       1 do echo *.opr input to *.chk
c		iechoX 2 do echo *.opr and include a header (____)
c			 above a comment
c		ioutSm 0 no details on small
c		       1 details on small
c		ioutLim 0 no details on diversion limit
c		       1 details on diversion limit

      iout=0
      ioutSM=0  
      ioutLim=0
      
      iecho=1
      iechoX=iecho
      rewind(ntmp)
c
c _________________________________________________________
c		Step 1; Print Subroutine Name      
      write(nlog,102)
      write(6,102)      
  102   format(/,72('_'),/,
     1 '  Oprinp; Operational Right File (*.opr) ')
      
      
      if(iout.eq.1) write(nlog,*) ' Subroutine Oprinp'      
c     write(nlog,*) '  Oprinp; iout = ', iout

c
c _________________________________________________________
c
c		Step 2; Initilize      
      
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
      small=0.001
      smallN=-1.0*small
      if(ioutSm.eq.1) write(nlog,*) ' Oprinp_01; small ', small

      DO ND=1,NUMDIV
        IDRGST(ND)=0
      enddo  
c
c ---------------------------------------------------------      
c		Initilize every operating rule
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
        
        ioBeg(k) = 0
        ioEnd(k) = 9999
        
        divOpr(k)= 0
        
        cdivtyp(k)='Diversion   '
        
        oprPct(k)=0

        ciopdeX2(k)= 'NA'
        creuseX(k) = 'NA'
        ciopsoX2(k)= 'NA'
        ioprlim(k)=0
c        
c rrb 209/01/26; Correction; this variable is only dimensioned to 50
cx      ntype(k)=0
c
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
c
c rrb 2012/05/25; Initilize destination and source arrays
        do i=1,maxopr2
          iopdes(i,k)=0
          iopsou(i,k)=0
        end do
        
cx        write(nlog,*) ' Oprinp; k = ', k
      end do  
      
      if(ioutSm.eq.1) write(nlog,*) ' Oprinp_02; small ', small
c _________________________________________________________
c
c		Step 3; Open file      
c     write(nlog,200)
c     write(6,200)

      iin2=iin
c      
c ---------------------------------------------------------      
c
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
     
c
c ---------------------------------------------------------
c rrb 208/09/12; Exit if no file is provided
      if(numOpr.eq.0) goto 500      
c
c ---------------------------------------------------------
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
c
c ---------------------------------------------------------      
c
c		Detalied headers
      if(iout.eq.1) write(nlog,1260)
      if(iecho.eq.1) write(nchk,1260)     
c
c _________________________________________________________
c		Loop for the maximum number of rules
      
      do 1190 k1=1,maxops
        call comment(55, nlog, iocode, nchk, iechoX)
c       if(iout.eq.1) write(nlog,*) ' Oprinp; iocode ', iocode
        if(iocode.eq.2) goto 1210
        if(iocode.eq.3) goto 928
c
c _________________________________________________________
c
c               Step 5a; Read data (new format)
c
        k=k+1
c
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
c
c rrb 2008/03/12; Initilize ioprlim     
          iOprLim(k) = int(oprlimit(k))     
          if(iout.eq.1) write(nchk,*) ' Oprinp; New ioprX, k', ioprX, k  
          goto 101
        endif
c
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
c
c rrb 2008/03/12; Initilize ioprlim     
          iOprLim(k) = int(oprlimit(k))     
          if(iout.eq.1) write(nchk,*) ' Oprinp; Unknown ioprX, k',
     1      ioprX, k  
          goto 101
        endif
c
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
c
c rrb 2008/03/12; Correction          
c         ioprlim(k) = 9999
          ioprlim(k) = 0
          cdivtyp(k) = 'Diversion   '
          if(iout.eq.1) write(nchk,*) ' Oprinp; Old ioprX, k', ioprX, k  
          
          goto 101
        endif
c
c _________________________________________________________
c
c               Step 5b; Initilize selected variables        
c		         Irregardless of the file type read
 101    continue
        if(cdivtyp(k).eq.'            ') cdivtyp(k)='Diversion   '
        if(iout.eq.1) write(nlog,*)'  Oprinp; cidvri = ', cidvri     
c
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
        
c

c
c rrb 2008/03/20; Initilize associated operating rule
        cAssoc= 'NA'        
c
c _________________________________________________________
c
c               Step 6; Process Eof or End
c
        if(cidvri.eq.blank .or. cidvri.eq.czero .or. 
     1     cidvri(1:3).eq.'End') then
           k=amax0(k-1,0)
           goto 1210
        endif   
c
c _________________________________________________________
c 
c		Step 7; Set beginning and ending data if data is provided
c           in the old format (w/o ioBeg and ioEnd)
c           Note execut handles ioBeg and ioEnd during
c           simulation
c  
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
c            
        if(iout.eq.3) then
          write(nlog,105) k, cidvri, ioprsw(k), iobeg(k), ioend(k)
        endif  
c
c _________________________________________________________
c
c rrb 2006/03/20; 
c		Step 8; Adjust character strings to left     
        cidvri=adjustl(cidvri)  
c       cgoto=adjustl(cgoto) 
        ciopso1=adjustl(ciopso1)
        ciopso2=adjustl(ciopso2)
        creuse=adjustl(creuse)    
c
c _________________________________________________________
c
c		Step 9; Set integer data     
        iopdes(2,k)=ifix(ciopdes)
        iopsou(2,k)=ifix(ciopso1x)
        iopsou(4,k)=ifix(ciopso2x)
        iops2=iopdes(2,k)
        
        rec12=cdivtyp(k)
        if(rec12(1:9).eq.'Depletion') ideplete=1
        
        if(ityopr(k).eq.24 .or. ityopr(k).eq.25) iexchang=1
c
c _________________________________________________________
c
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
c
c _________________________________________________________
c
c		Step 10a; Detailed output to log     
      if(iout.eq.1) write(nlog,1324)
     1       cidvri,      nameo(k),    cgoto,                             
     1       rtem,        dumc,        ioprsw(K),                         
     1       ciopde,      ciopdes, 
     1       ciopso1,     ciopso1x,    ciopso2, ciopso2x, 
     1       ITYOPR(K),   creuse,      cdivtyp(k), OprLoss(k),
     1       OprLimit(k), ioBeg(k),    ioEnd(k), k
c
c _________________________________________________________
c
c		Step 10b; Detailed output to check
      if(iecho.eq.1) write(nchk,1324)
     1       cidvri,      nameo(k),    cgoto,                             
     1       rtem,        dumc,        ioprsw(K),                         
     1       ciopde,      ciopdes, 
     1       ciopso1,     ciopso1x,    ciopso2, ciopso2x, 
     1       ITYOPR(K),   creuse,      cdivtyp(k), OprLoss(k),
     1       OprLimit(k), ioBeg(k),    ioEnd(k), k
     
c
c _________________________________________________________
c		Step 11; Set source and destination strings for 
c                        plan reporting     
        ciopdeX(1,k) = ciopde
        ciopdeX2(k)='NA'
        ciopsoX(1,k) = ciopso1
        creuseX(k) = creuse
        ciopsoX2(k)= ciopso2
c
c _________________________________________________________
c       
c		Sep 12; Set Initilization for types 13, 14 and 15
        if(ityopr(k).eq. 13) iSetOpr(13)=1
        if(ityopr(k).eq. 14) iSetOpr(14)=1
        if(ityopr(k).eq. 15) iSetOpr(15)=1
        if(ityopr(k).eq. 47) iSetOpr(47)=1
        
c
c _________________________________________________________
c       
c rrb 01/08/23; Step 13; Set release type switch 
c               iopsou(6,k) = iopsou(4,k) 
c               Note use iopsou(6,k) because iopsou(4,k) may be 
c               used for another purpose later.
c       
        if(ityopr(k).eq. 2 .or. ityopr(k).eq.3 .or.        
     1     ityopr(k).eq. 7 .or. ityopr(k).eq.10) then

          iopsou(6,k) = iopsou(4,k)
          if(iopsou(6,k).gt.0) then 
            iopsou(4,k)=0
c
c rrb 02/02/25; Test to allow ieffmax = 2 (read IWR for daily running
c               demand but do not use variable efficiency capability)
c           if(ieffmax.ne.1) then
            if(ieffmax.le.0) then
              iopsou(6,k)=0
            endif
          endif
        endif

c
c _________________________________________________________
c
c               Step 14; Estimate "End of File" if a blank is read
        if(cidvri.eq.blank .or. cidvri.eq.czero) goto 1210
c
c _________________________________________________________
c
c
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
c
c
c _________________________________________________________
c
c               Step 16; Process a right that is off
c                        Read misc data so checks can be skipped
        if(ioprsw(k).eq.0) then
c         iout=1
          if(iout.eq.1) then
            write(nchk,*) ' Oprinp; Right Off', k, cidvri
            write(nlog,*) ' Oprinp; Right Off', k, cidvri        
          endif
          
          koff=koff+1
c
c ---------------------------------------------------------
c		Read Monthly on/off          
          if(idumc.eq.12 .or. idumc.lt.-12) then
            if(iout.eq.1) write(nchk,*) ' Oprinp; Monthly on-off ',
     1       idumc
          
            read(55,'(a256)',end=926,err=928) rec256
c           write(nlog,*) ' Monthly on/off data'
c           write(nlog,*) rec256
            if(iecho.eq.1) write(nchk,'(a256)') rec256
          endif  
c
c ---------------------------------------------------------
c		Read Carrier Data (without loss)
          ioprloss=int(oprloss(k))
          if(ioprloss.eq.0) then
            if(iout.eq.1) write(nchk,*) ' Oprinp; Carrier No Loss ',
     1          ioprloss, idumc
            idumy=idumc
            idumy=amax0(idumc, iabs(idumc)-12)
c
c rrb 2008/06/04; Correction            
            if(idumc.eq.12) idumy=0
            if(idumy.gt.0) then
              do i=1,idumy
                if(iout.eq.1) write(nchk,*) ' Oprinp; idumy', idumy
                read(55,'(a256)',end=926,err=928) rec256
                if(iecho.eq.1) write(nchk,'(a256)') rec256
              end do  
            endif  
          endif
          
c
c ---------------------------------------------------------
c		Read Carrier Data (with loss)
          ioprloss=int(oprloss(k))
          
          if(iabs(ioprloss).gt.0) then
            if(iout.eq.1) write(nchk,*) ' Oprinp; Carrier with Loss ',
     1          ioprloss, idumc
     
            idumy=idumc
            idumy=amax0(idumc, iabs(idumc)-12)
c
c rrb 2008/06/04; Correction            
            if(idumc.eq.12) idumy=0
            if(idumy.gt.0) then
              do i=1,idumy
                read(55,'(a256)',end=926,err=928) rec256
                if(iecho.eq.1) write(nchk,'(a256)') rec256
              end do  
            endif            
          endif  
c          
cr          if(iout.eq.1) write(nlog,*) rec256          
cr          write(nlog,*) '  Oprinp; rec256 ', rec256
c
c ---------------------------------------------------------
c		Rio Grande Compact Treatment          
c		Read extra stream gage data
          if(ityopr(k).eq.17 .or. ityopr(k).eq.18) then 
            read(55,'(a256)',end=926,err=928) rec256
            if(iecho.eq.1) write(nchk,'(a256)') rec256
          endif
c
c ---------------------------------------------------------
c		Type 24 and 25 Exchange to ....
c		Read Exchnage limits          
          
          if(ityopr(k).eq.24 .or. ityopr(k).eq. 25) then
            if(iout.eq.1)  write(nchk,*) ' Oprinp; Exchange Limits ', 
     1        k, ityopr(k)
            
            read(55,'(a256)',end=926,err=928) rec256     
            if(iecho.eq.1) write(nchk,'(a256)') rec256           
          endif  
c        
c ---------------------------------------------------------
c rrb 2008/02/21;  
c		Type 27 and 28 Plan and 45 with an operating rule limit
c
          if(ityopr(k).eq.27 .or. ityopr(k).eq. 28) then
            if(iout.eq.1) write(nchk,*) ' Oprinp; Associated Opr Rule ', 
     1        k, ityopr(k), oprlimit(k)
     
c
c		              Associated Operating Rule
            if(oprlimit(k).gt.small .and. oprlimit(k).lt.9999) then
              read(55,'(a256)',end=926,err=928) rec256     
              if(iecho.eq.1) write(nchk,'(a256)') rec256           
            endif
c
c		              T&C CU factors            
c rrb 2008/04/23; Revise to be more robust 
            if(iopsou(4,k) .gt. 0) then              
              if(iout.eq.1) write(nchk,*) ' Oprinp; T&C CU Factors ', 
     1          k, iopsou(4,k), nas2
c
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
c            
c ---------------------------------------------------------
c rrb 2011/10/15; Update to type 45 operating rule
c		Type 27 and 28 Plan and 45 with an operating rule limit
c
          if(ityopr(k).eq.45) then

            if(iout.eq.1) write(nchk,*) ' Oprinp; Associated Opr Rule ', 
     1        k, ityopr(k), oprlimit(k)
     
c
c		              Associated Operating Rule
            if(oprlimit(k).gt.small .and. oprlimit(k).lt.9999) then
              read(55,'(a256)',end=926,err=928) rec256     
              if(iecho.eq.1) write(nchk,'(a256)') rec256           
            endif
          endif       
c
c rrb 2008/02/21;           
c ---------------------------------------------------------
c		Type 10 and 29 Spill
c		Associated Operating Rule Data (5)
          if(ityopr(k).eq.10 .or. ityopr(k).eq.29) then
c
c		Monthly and Annual Limit Data
            if(oprlimit(k).gt.small .and. oprlimit(k).lt.9999) then
              read(55,'(a256)',end=926,err=928) rec256     
c             write(nlog,*) ' Associated Operating Rule'
c             write(nlog,*) rec256
              if(iecho.eq.1) write(nchk,'(a256)') rec256           
            endif
          endif  
          
          goto 1190
        endif
c _________________________________________________________
c
c		Step 17; Branch for operating rule specific processing
c
c
c               For type 1, Reservoir to a ISF 
        if (ityopr(k).eq.1) goto 1001
c        
c               For type 2, Reservoir to diversion or reservior 
c                           or Carrier
        if (ityopr(k).eq.2) goto 1002
c        
c               For type 2, Reservoir to a carrier
        if (ityopr(k).eq.3) goto 1003
c        
c               For type 4, Reservoir to a dversion by Exchange
        if (ityopr(k).eq.4) goto 1004
c        
c               For type 5, Reservoir storage by Exchange
        if (ityopr(k).eq.5) goto 1005
c        
c               For type 6, Reservoir to Reservoir Transfer
        if (ityopr(k).eq.6) goto 1006
c        
c               For type 7, Diversion by Carrier by Exchange
        if (ityopr(k).eq.7) goto 1007
c
c               For type 8, Out of Priority Bookover
        if (ityopr(k).eq.8) goto 1008
c
c               For type 9, Release to Target
        if (ityopr(k).eq.9) goto 1009
c
c               For type 10, Replacement Reservoir
        if (ityopr(k).eq.10) goto 1010
c
c               For type 11, Carrier
        if (ityopr(k).eq.11) goto 1011
c
c               For type 12, reoperation right, we're done
        if (ityopr(k).eq.12) goto 1190
c
c               For type 13, index River Flow
        if (ityopr(k).eq.13) goto 1013
c
c               For type 14, Carrier with a Constrained Demand
        if (ityopr(k).eq.14) goto 1014
c
c               For type 15, interruptable supply, process in 1 place
        if(ityopr(k).eq.15) goto 1015
c
c               For type 16, direct flow storage, process in 1 place
        if(ityopr(k).eq.16) goto 1016
c
c               For type 17, Rio Grande Compact-RG, process in 1 place
        if(ityopr(k).eq.17) goto 1017
c
c               For type 18, Rio Grande Compact-Co, process in 1 place
        if(ityopr(k).eq.18) goto 1018
c
c               For type 19, Split Channel, process in 1 place
        if(ityopr(k).eq.19) goto 1019                         
c
c               For type 20, San Juan RIP for Navajo 
        if(ityopr(k).eq.20) goto 1020                         
c
c               For type 21, Sprinkler Use
        if(ityopr(k).eq.21) goto 1021
c
c               For type 22, Soil Moisture Use
        if(ityopr(k).eq.22) goto 1022        
c
c               For type 23, Downstream Call Data
        if(ityopr(k).eq.23) goto 1023        
c
c               For type 24, Direct Flow Exchange 
        if(ityopr(k).eq.24) goto 1024                
c
c               For type 25, Direct Flow Bypass
        if(ityopr(k).eq.25) goto 1025        
c
c               For type 26, Reservoir or Plan to a Plan
        if(ityopr(k).eq.26) goto 1026        
c
c               For type 27, Plan to a Diversion Direct
        if(ityopr(k).eq.27) goto 1027        
c
c               For type 28, Plan to a Diversion by Exchange
        if(ityopr(k).eq.28) goto 1028        
c
c               For type 29, Plan spill
        if(ityopr(k).eq.29) goto 1029        
c
c               For type 30, Redivert T&C Plan release
        if(ityopr(k).eq.30) goto 1030
c
c               For type 31, Carrier with Reuse
        if(ityopr(k).eq.31) goto 1031
c
c               For type 32, Reuse Reservoir and Plan to Diversion, 
c                            Reservoir or Carrier with Reuse Direct
        if(ityopr(k).eq.32) goto 1032
c
c               For type 33, Reuse Reservoir and Plan to a Diversion, 
c                            Reservoir or Carrier with Reuse by Exchange
        if(ityopr(k).eq.33) goto 1033
c
c               For type 34, Bookover with Reuse
        if(ityopr(k).eq.34) goto 1034
c
c               For type 35, Import 
        if(ityopr(k).eq.35) goto 1035
c
c               For type 36, Meadow Rights
        if(ityopr(k).eq.36) goto 1036
c
c               For type 37, Well Augmentation
        if(ityopr(k).eq.37) goto 1037
c
c               For type 38, OOP Diversion
        if(ityopr(k).eq.38) goto 1038
c
c               For type 39, Alternate Point
        if(ityopr(k).eq.39) goto 1039
c
c               For type 40, South Platte Compact
        if(ityopr(k).eq.40) goto 1040
c
c               For type 41, Storage limited by an OOP Plan Volume
        if(ityopr(k).eq.41) goto 1041
c
c               For type 42, Plan Spill
        if(ityopr(k).eq.42) goto 1042
c
c               For type 43, In-Priority Supply
        if(ityopr(k).eq.43) goto 1043
c
c               For type 44, Recharge Well
        if(ityopr(k).eq.44) goto 1044
c
c               For type 45, Carrier with Losses
        if(ityopr(k).eq.45) goto 1045
c
c               For type 46, Multiple Ownership
        if(ityopr(k).eq.46) goto 1046
c
c               For type 47, Administratve Ownership
        if(ityopr(k).eq.47) goto 1047
c
c               For type 48, Plan or Res. reuse to a Plan Direct
        if(ityopr(k).eq.48) goto 1048
c
c               For type 49, Plan or Res. reuse to a Plan Exchange
        if(ityopr(k).eq.49) goto 1049
c
c               For type 50, South Platte compact Storage
        if(ityopr(k).eq.50) goto 1050
        
        write(nlog,1277) ityopr(k),cidvri
        goto 9999
        
        
c                                                                       
c _________________________________________________________
c
 1001   continue 
c 
c               Type 1; Reservoir to a ISF
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
     1       ix, ix, nx, cx, 1, istop, rops2, ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               a2. Read intervening structures
        istop=0
        call oprFind(ityopr(k), 21, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2, ioprsw(k), cidvri)
c
c ---------------------------------------------------------
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
c
c
c ---------------------------------------------------------
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
        
c        
c ---------------------------------------------------------
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
c
c ---------------------------------------------------------
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
        
c		
        goto 1190
        
c                                                                       
c _________________________________________________________
c
 1002   continue 
c 
c               Type 2; Reservoir to a Diversion, Reservoir or Carrier
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
        
c        
c ---------------------------------------------------------
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

c
c ---------------------------------------------------------
c               c3. If a carrier then reset the destination location
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
        istop=0
        itype=2
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iopsou(1,k),iopsou(2,k), nx, ciopso1, 1, 
     1       istop, rops2, ioprsw(k), cidvri)
c        
c ---------------------------------------------------------
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
        
c		
c
c ---------------------------------------------------------
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

c                                                                       
c _________________________________________________________
c
 1003   continue 
c 
c               Type 3; Reservoir to a Carrier
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
     1       ix, ix, nx, cx, 1, istop, rops2, 
     1       ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               a2. Read intervening structures
        istop=0
        call oprFind(ityopr(k), 21, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2, ioprsw(k), cidvri)
c
c ---------------------------------------------------------
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
        
c        
c ---------------------------------------------------------
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
        istop=0
        itype=2
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1,iopsou(2,k), nx, ciopso1, 1, istop, rops2, 
     1       ioprsw(k), cidvri)
        iopsou(1,k) = iops1
        iopSouR(k)=itype
        
c        
c ---------------------------------------------------------
c               e. Check that the source reservoir is upstream 
c                  of the destination diversion, reservoir or carrier
c	           trying to find destination (idcdD)
c                  downstream of  source (iscdS) 
c rrb Do not check location type 3 is to a carrier
c       iss=irssta(iops1)
c       ndns=ndnnod(iss)
c       csource=cstaid(iss)
c       cdest=cstaid(idcdD)
c          
c       call oprdown(nlog, maxsta, ndns, iss, idcdD, idncod,
c     1       cidvri, csource, cdest)
c
c ---------------------------------------------------------
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
c		
        goto 1190
c                                                                       
c _________________________________________________________
c
 1004   continue 
c 
c               Type 4; Reservoir to a Diversion by Exchange
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
        
c
c ---------------------------------------------------------
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
c
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
        nc=intern(k,1)
        if(nc.gt.0) then
          idcdD=idvsta(nc)                  
cx         write(nlog,*) '  Oprinp; #3 iopdes(1,k) = ',iopdes(1,k),idcdd
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
c               j. Find the exchange point (iopsouX)
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
c               j. Find the exchange point (iopsouX)
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
        call oprFind(ityopr(k), 21, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2, cidvri)
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
c               j. Find the exchange point (iopsouX)
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
        idumc3=amax0(idumc, idumc2)
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
c			iacc=0 account may be 0 (treat porportionally)
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
c		a3. Read the operating rule associated with
c		   a monthly or annual plan limit adjustment
c		   when Oprlimit(k) > 0
c		istop=0  Stop if not found
c		itype=24 Operating Rule ID with monthly and annual
c                        plan limits
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
c		 where soruce is a diversion, diversion water right
c		 or reservoir water right
c rrb 01/06/20; 
c                destination = diversion or reservoir ID
c                source 1 = a diversion or water right 
c                source 2 = water right type >=0 = diversion, <0=reservoir
c		 source 3 = NA
c                source 4 = NA
c                source 5 = NA
c		 source 6 = NA
c		 source 7 = NA
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
c		    ion=0 leaves the original water right on
c                   iacc=1 Check the account varaible (iops2) > 0
c		    iacc=0 Do not check the account variable
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
c		Reservoir location     
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
c      Type 15; Interruptable supply source
c                destination = instream flow
c                source read = used
c                source 1 = 1 source stream used as an on/off switch
c                source 3 = 2 water right with an interruptable supply
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
c                  for the interruptable supply switch
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
c               Search every river node downstram of the source
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
     1      oprloss(k), oprlimit(k), iopSou(5,k),
     1      cdivtyp(k), intern, cntern, cAssoc)
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
     1      oprloss(k), oprlimit(k), iopSou(5,k),
     1      cdivtyp(k), intern, cntern, cAssoc)
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
     1      oprloss(k), oprlimit(k), iopSou(5,k),
     1      cdivtyp(k), intern, cntern, cAssoc)
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
     1      oprloss(k), oprlimit(k), iopSou(5,k),
     1      cdivtyp(k), intern, cntern, cAssoc)
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
     1      cdivtyp(k), intern, cntern, cAssoc)
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
     1      cdivtyp(k), intern, cntern, cAssoc)
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
     1      cdivtyp(k), intern, cntern, cAssoc)
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
     1      cdivtyp(k), intern, cntern, cAssoc)
     
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
     1      cdivtyp(k), intern, cntern, cAssoc)
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
c               f. Find reuse plan named Creuse, if any, and
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

        if (ireuse1.gt.0) then
          ireuse(k)=ireuse1        

          iok=1          
c
c		Reservoir destination (2)          
          if(iopdesr(k).eq.2) then
            if(iplntyp(ireuse1).eq.3 .or. iplntyp(ireuse1).eq.5) iok=0
          endif  
c
c		Diversion destination (3)          
          if(iopdesr(k).eq.3) then
            if(iplntyp(ireuse1).eq.4 .or. iplntyp(ireuse1).eq.6) iok=0
          endif
          
          if(iok.eq.1) then
            write(nlog,1255) ityopr(k),cidvri, ciopde, creuse, 
     1       iplntyp(ireuse1)
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
c               h. Find the exchange point (iopsouX)
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
c		k. Check a return to the river is located properly
c		   itypeR=0 Served directly,
c			 =1 Served by Exchange
cx       if(NAs2.eq.0) then
cx         itypeR=1
cx         call ChkRivRF(nlog, 24, k, fac, maxopr, maxsta,  
cx     1    intern,  idcdD,  itypeR, idncod, ndnnod, cstaid,ioprsw(k), cidvri)
cx       endif    
c
c ---------------------------------------------------------
c		l. Detailed output
c     
        iout24=1
        if(iout24.eq.1) then
          call OprinOut(nlog, maxopr, k, 
     1      ityopr(k), cidvri, 
     1      ciopde, iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k),iopsou(2,k),
     1      ciopso2, iopsou(3,k),iopsou(4,k), creuse, ireuse1,
     1      oprloss(k), oprlimit(k), iopSou(5,k),
     1      cdivtyp(k), intern, cntern, cAssoc)
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
c               f. Find reuse plan named Creuse, if any, and
c		   Store in ireuse(k)
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
c                  Note iplntyp 3 & 5 are reservoir, 
c                       iplntyp 4 & 6 are diversion

        if (ireuse1.gt.0) then
          ireuse(k)=ireuse1        

          iok=1          
c
c		Reservoir destination          
          if(iopdesr(k).eq.2) then
            if(iplntyp(ireuse1).eq.3 .or. iplntyp(ireuse1).eq.5) iok=0
          endif  
c
c		Diversion destination          
          if(iopdesr(k).eq.3) then
            if(iplntyp(ireuse1).eq.4 .or. iplntyp(ireuse1).eq.6) iok=0
          endif
          
          if(iok.eq.1) then
            write(nlog,1255) ityopr(k),cidvri, ciopde, creuse, 
     1       iplntyp(ireuse1)
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
          call OprinOut(nlog, maxopr, k, 
     1      ityopr(k), cidvri, 
     1      ciopde, iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k),iopsou(2,k),
     1      ciopso2, iopsou(3,k),iopsou(4,k), creuse, ireuse1,
     1      oprloss(k), oprlimit(k), iopSou(5,k),
     1      cdivtyp(k), intern, cntern, cAssoc)
        endif  

        goto 1190
c _________________________________________________________
c
c         
 1026   continue 
c               Type 26; Not Active (replaced with type 47)
c
        write(nlog,1276) ityopr(k),cidvri
        goto 9999

c       goto 1190
     
c _________________________________________________________
c
c         
 1027   continue 
c               Type 27 Reservoir or Reuse Plan (4 or 6) or
c                 Accounting plan (11) to a Diversion or Reservoir 
c			            or Plan or Instream Flow
c			          If the source is a Resevoir the destination
c			            Plan should be a type 9 (OOP Plan)
c			          If the source is a Plan the 
c			            Plan should be a Reuse plan (type 4 or 6) or
c                 OOP plan (type 9) or an Accounting Plan (type 11)
c
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
c		a4. Read the operating rule associated with
c		    a monthly or annual plan limit adjustment
c		    when Oprlimit(k) > 0
c		    istop=0  Stop if not found
c	     	itype=24 Operating Rule ID with monthly and annual
c                 plan limits
c
c       write(nlog,*) ' Oprinp; type 27 ioprlim ', ioprlim(k)
        if(ioprlim(k).gt.0) then
          istop=0
          itype=24          
          call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1      iopsou(5,k),iopsou(6,k), nx, cAssoc, 1, 
     1      istop, rops2,ioprsw(k), cidvri)
          iopsou(5,k)=nx 
c
c		              Set ciospoX5 to the plan associated with the 
c                 above operating rule  
c rrb 2009/01/15; Revise to allow iopsou(k)=3 to limit to the
c		              amount diverted by another operating rule
          if(ioprlim(k).eq.1 .or. ioprlim(k).eq.2) then
            ip5=iopsou(1,nx)
            ciopsoX5(k)=pid(ip5)
          endif
        endif     
c
c ---------------------------------------------------------
c               b1. Find the destination, a diversion (type 3)
c		   set istop=1 (OK if not found)
c		   itype=3 diversion
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
c		        istop=1 (OK if not found)
c			iacc=1 (check account number)
c			itype=2 Reservoir
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
c                   (type 2) or a augmentation Plan (type 2) or
c                    an Accounting Plan (type 11)
c                       istop=0 (Stop if not found)
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
            iopdesr(k)=7
c           write(nlog,*) ' Oprinp;  iopdes(1,k) = ', iopdes(1,k)              
c
c rrb 2011/11/27; Check the Destination Plan type provided
c                 Note may be a T&C Plan (type 1), Aug Plan (type 2),
c                 
            ndP=iops1         
            iok=1          
            if(iplntyp(ndP).eq.1 .or. iplntyp(ndP).eq.2 .or.
     1         iplntyp(ndP).eq.11) iok=0
          
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
          iopdesr(k)=1
          write(nlog,*) ' Oprinp;  iopdes(1,k) = ', iopdes(1,k)
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
c                   (type 9) or an Accounting Plan (type 11)
c                    Note: istop=1 OK if not found)
c			                     itype=7 Plan structure
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
c               d1. Find Source 2 a T&C plan (type 7) 
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
c
c		Check for the proper plan type          
          if(iplntyp(iops1).ne.1) then
            write(nlog,1253) ityopr(k),cidvri, ciopso2,
     1       iplntyp(iops1)
            goto 9999
          endif  
c
c rrb 2007/08/22; Iopsou(4,k) is used to define the return pattern type
c		  standard = 1, constant = 2
c		Check that the return type has been specified
          if(iopsou(4,k).le.0 .or. iopsou(4,k).ge.4) then
            write(nlog,1275) ityopr(k),cidvri, iopsou(4,k)
            goto 9999
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
c		to the first carrier
        nc=intern(k,1)
        if(nc.gt.0) idcdX=idvsta(nc)
c
c ---------------------------------------------------------
c		f. Check the source is upstream of the destination 
c		   where idcdX is the destination diversion, reservoir,
c		   plan or carrier   
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
c               h. Find destination reuse plan named Creuse, if any, 
c		               and store in ireuse(k)
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
c		i. Check proper type of reuse plan 
c                  Note iplntyp 3 & 5 are reservoir, 
c                       iplntyp 4 & 6 are diversion
        if (ireuse1.gt.0) then
          ireuse(k)=ireuse1        
          iok=1
c
c		If the source is a plan and the destination is a reservoir
c               the reuse plan should be types 3 or 5
c         if(iopsou(1,k).lt.0 .and. iopdes(1,k).lt.0) then
          if(iopsou(1,k).lt.0 .and. iopdesr(k).eq.2) then          
            if(iplntyp(ireuse1).eq.3 .or. iplntyp(ireuse1).eq.5) iok=0
          endif  
c
c		If the source is a plan and the destination is a diversion 
c               the reuse plan shuld be type 4 or 6          
c         if(iopsou(1,k).lt.0 .and. iopdes(1,k).gt.0) then
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
c ---------------------------------------------------------
c               j. Set the release type demand or depletion
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
c		j. Check the return type matches the data provided
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
c		k. Check a return to the river is located properly
c		   Note idcdD=destination diversion, reservoir or plan
c		   but not carrier
c		   itypeR=0 Served directly,
c                        =1 Served by Exchange
cx       if(NAs2.eq.0) then
cx         itypeR=0
cx         call ChkRivRF(nlog, 27, k, fac, maxopr, maxsta,  
cx     1    intern,  idcdD,  itypeR, idncod, ndnnod, cstaid,ioprsw(k), cidvri)
cx       endif    
       
c
c ---------------------------------------------------------
c		l. Detailed output
c     
        iout27=0
        if(iout27.eq.1) then
          call OprinOut(nlog, maxopr, k, 
     1      ityopr(k), cidvri, 
     1      ciopde, iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k),iopsou(2,k),
     1      ciopso2, iopsou(3,k),iopsou(4,k), creuse, ireuse1,
     1      oprloss(k), oprlimit(k), iopSou(5,k),
     1      cdivtyp(k), intern, cntern, cAssoc)
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
c		istop=0  Stop if not found
c		itype=24 Operating Rule with monthly and annual
c                        plan limits

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
c rrb 2008/01/15; Revise to allow iopsou(k)=3 to limit to the
c		              amount diverted by another operating rule
          if(ioprlim(k).eq.1 .or. ioprlim(k).eq.2) then        
            ip5=iopsou(1,nx)
            ciopsoX5(k)=pid(ip5)          
          endif
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
c rrb 2007/10/26; Revise to rely on varaiable iopdesr        
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
c               d1. Find Source 2 a T&C plan (type 7) 
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
c		Check plan type (1=T&C)       
          if(iplntyp(iops1).ne.1) then
            write(nlog,1253) ityopr(k),cidvri, ciopso2,
     1       iplntyp(iops1)
            goto 9999
          endif  
          
c
c rrb 2007/08/22; Iopsou(4,k) is used to define the return pattern type
c		  standard = 1, constant = 2
c		Check that the return type has been specified
          if(iopsou(4,k).le.0 .or. iopsou(4,k).gt.4) then
            write(nlog,1275) ityopr(k),cidvri, iopsou(4,k)
            goto 9999
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
c		   and store in ireuse(k)
        iacc=0
        ion=-1
        istop=0
        ireuse1=0        
cr      if(creuse(1:3).ne.'N/A') then
        if(NAuse.eq.0) then        
          call oprFind(ityopr(k), 7, idumc,k,ion,iprinto,
     1       ireuse1,iops2, nx, creuse, iacc, istop, rops2,
     1        ioprsw(k), cidvri)     
        endif
c
c ---------------------------------------------------------
c		h. Check proper type of plan for the reuse
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
c		j. Check the return type matches the data provided
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
c		k. Check a return to the river is located properly
c		   Note idcdD is the destination diversion, reservoir,
c		   or plan (not the carrier)
c		   itypeR=0 Served directly,
c			 =1 Served by Exchange
cx       if(NAs2.eq.0) then
cx         itypeR=1
cx         call ChkRivRF(nlog, 28, k, fac, maxopr, maxsta,  
cx     1    intern,  idcdD,  itypeR, idncod, ndnnod, cstaid,ioprsw(k), cidvri)
cx       endif    
c
c ---------------------------------------------------------
c		l. Detailed output
c     
        iout28=0
        if(iout28.eq.1) then
          call OprinOut(nlog, maxopr, k, 
     1      ityopr(k), cidvri, 
     1      ciopde, iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k),iopsou(2,k),
     1      ciopso2, iopsou(3,k),iopsou(4,k), creuse, ireuse1,
     1      oprloss(k), oprlimit(k), iopSou(5,k),
     1      cdivtyp(k), intern, cntern, cAssoc)
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
c		a2. Read the operating rule associated with
c		   a monthly or annual plan limit adjustment
c		   when Oprlimit(k) > 0
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
c rrb_2014-04-26; Allow the destination to be specified
c ---------------------------------------------------------
c               b1. Find the destination, a river ID (type 0)
c		   set istop=1 (OK if not found)
        istop=1
        itype=0
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       iops1, iopdes(2,k), nx,ciopde, 0, istop, rops2,
     1       ioprsw(k), cidvri)
     
        if(iops1.gt.0) then
          iopdes(1,k)=iops1
          idcdD=idvsta(iops1)
          iopdesr(k)=itype
        endif  
c *********     
c
c ---------------------------------------------------------
c                b. Find source 1 a reservoir if any (type 2)
c		   Note istop=1, do not stop if not found
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
c               c. Find source 1 a plan (type 7)
c		   Note istop=1, do not stop if not found
        istop=1
        itype=7
        iacc=1
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1       np1,iopsou(2,k), nx, ciopso1, iacc, istop, rops2,
     1       ioprsw(k), cidvri)
c
c rrb 2006/04/31; Correction     
cr      if(np1.gt.0) iopsou(1,k)=-1*np2

        if(np1.gt.0) then        
          iopSouR(k)=itype        
          iopsou(1,k)=-1*np1
c
c		Check for proper plan type
          if(iplntyp(np1).eq.1  .or. iplntyp(np1).eq.2 .or.
     1       iplntyp(np1).eq.10 .or. iplntyp(np1).eq.9) then         
              write(nlog,1267)  ityopr(k),cidvri, ciopso1, iplntyp(np1)
              goto 9999
          endif    
          
        endif  
c       write(nlog,*) '  Oprinp 2; type 29 nr, np1, np2', nr, np1, np2
        
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
c		   Note istop=1, do not stop if not found
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
c	  	Check for proper plan type
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
c		f. Check if a source 1 reservoir plan is a reservoir 
c                  when source 2 is a plan
        if(np1.gt.0 .and. np2.gt.0) then
          write(nlog,938) corid(k), ityopr(k), ciopso1, ciopso2          
          goto 9999
        endif
c       write(nlog,*) '  Oprinp 5; type 29 nr, np1, np2', nr, np1, np2
        
c
c ---------------------------------------------------------
c
c		g. Check if a source 1 reservoir plan is not 
c                  tied to a reservoir
        if(np1.gt.0) then
c
c rrb 2006/05/01; Correction        
cr        if(iplntyp(np1).eq. 3 .or. iplntyp(np2).eq.5) then
          if(iplntyp(np1).eq. 3 .or. iplntyp(np1).eq.5) then
            write(nlog,934) corid(k), ityopr(k), ciopso1          
            goto 9999
          endif
        endif
c
c ---------------------------------------------------------
c		h. Detailed output
     
        iout29=1
        if(iout29.eq.1) then
          call OprinOut(nlog, maxopr, k, 
     1      ityopr(k), cidvri, 
     1      ciopde, iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k),iopsou(2,k),
     1      ciopso2, iopsou(3,k),iopsou(4,k), creuse, ireuse1,
     1      oprloss(k), oprlimit(k), iopSou(5,k),
     1      cdivtyp(k), intern, cntern, cAssoc)
        endif  
c       write(nlog,*) '  Oprinp 6; type 29 nr, np1, np2', nr, np1, np2        
        goto 1190
c _________________________________________________________
c
c         
 1030   continue 
c               Type 30; Re Store T&C release
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
     1      cdivtyp(k), intern, cntern, CAssoc)
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
c                source 1 (iopsou(1,k) = a reservior
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
        istop=0
        call oprFind(ityopr(k), 21, idumc,k,ion,iprinto,ix, ix,
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
c rrb 2011/11/27; Update to use iopdesr()
cx      if(iopdes(1,k).gt.0) then
        if(iopdesr(k).eq.3) then
          if(rec12(1:9).eq.'Diversion') iok=0
          if(rec12(1:9).eq.'Depletion') iok=0
        else
          if(rec12(1:9).eq.'Diversion') iok=0
        endif  
c
c rrb 2008/12/01; Addition accept direct        
        if(rec12(1:6).eq.'Direct') iok=0        
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
c		   and store in ireuse(k)
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
c		h. Check proper type of plan 
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
c		a4. Read the operating rule associated with
c		   a monthly or annual plan limit adjustment
c		   when Oprlimit(k) > 0
c		istop=0  Stop if not found
c		itype=24 Operating Rule ID with monthly and annual
c                        plan limits
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
c			ion=-1 do not check or turn anything off
        iops1=0
        iopsou(4,k)=0
        
        istop=1
        itype=3
        ion=-1
        call oprFind(ityopr(k), 3, idumc,k,ion,iprinto,
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
          ion=-1
          call oprFind(ityopr(k), 14, idumc,k,ion,iprinto,
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
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
        if(iops1.eq.0) then
          istop=1
          ion=-1
          call oprFind(ityopr(k), 7, idumc,k,ion,iprinto,
     1         iops1,iopsou(4,k), nx, ciopso2, 1, istop, rops2,
     1         ioprsw(k), cidvri)
          if(iops1.gt.0) then
            iopsou(3,k)=iops1
            iopsou(4,k)=7
            
            iok=1
            if(iplntyp(iops1).eq.9 .or. iplntyp(iops1).eq.10) iok=0
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
c                  Note iplntyp 3 and 5 are a reservoir, 
        if (ireuse1.gt.0) then
          ireuse(k)=ireuse1        

          iok=1          
          if(iplntyp(ireuse1).eq.3 .or. iplntyp(ireuse1).eq.5) iok=0
c
c rrb 2006/10/17; Allow destination to be an OOP plan          
          if(iplntyp(ireuse1).eq.9 .or. iplntyp(ireuse1).eq.10) iok=0
          
          if(iok.eq.1) then
            write(nlog,1255) ityopr(k),cidvri, ciopde, creuse, 
     1       iplntyp(ireuse1)
            goto 9999
          endif  
        endif        
c
c ---------------------------------------------------------
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


c        
c _________________________________________________________
c
c         
 1035   continue 
c               Type 35; Transmountain import
c                destination = a diversion or a reservoir or carrier
c                source 1 (iopsou(1,k) = a diversion (import)
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
c               a2. Read intervening structures
        istop=0
        call oprFind(ityopr(k), 21, idumc,k,ion,iprinto,ix, ix,
     1               nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
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
          iopdes(1,k)=-iops1
          idcdD=irssta(iops1)
          iopdesr(k)=2
c         write(nlog,*) '  Oprinp; Type 28 iopdes(1,k) = ', iopdes(1,k)
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
c               d. Find destination reuse plan named Creuse, if any, and
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
     1         ireuse1,iops2, nx, creuse, iacc, istop, rops2,
     1         ioprsw(k), cidvri)     
        endif
c
c ---------------------------------------------------------
c		f. Check proper type of plan for a TransMountain Import
c                  Note iplntyp 3 & 5 are reservoir, 
c                       iplntyp 4 & 6 are diversion
        if (ireuse1.gt.0) then
          ireuse(k)=ireuse1        
          
          iok=1          
          if(iopdes(1,k).lt.0 .and. iplntyp(ireuse1).eq.5) iok=0
          if(iopdes(1,k).gt.0 .and. iplntyp(ireuse1).eq.6) iok=0
          if(iok.eq.1) then
            write(nlog,1255) ityopr(k),cidvri, ciopde, creuse, 
     1        iplntyp(ireuse1)
            goto 9999
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
c               c3. Find source 2 a reservior water right (type 12)
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
        idumc3=amax0(idumc, idumc2)
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
          if(iplntyp(iops1).eq.9 .or. iplntyp(iopd1).eq.10) iok=0
          if(iok.eq.1) then
            write(nlog,1263) ityopr(k),cidvri, creuse, iplntyp(iops1)
            goto 9999
          endif
        end do
c
c ---------------------------------------------------------
c               c1. Find destination Reservoir
c                  Note itype=12 for a reservor ID
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
c                   Note itype=12 for a reservor right ID
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
c		   Note ion  =1 Turn original water right OFF
c			ion  =0 Leave original water right ON
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
c			itype =16 for a well water right
c		        iacc =0 Allow account to be 0 (since 
c                                 it is ownership %)
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
c                ion=1 means turn off opr right if right is off
c		             Note istop=0 Stop if not found
c		             istop=1 Do not Stop if not found
   
c       write(nlog,*) '  Oprinp; Reading type 45 rule'
        ion=1
        istop=0
        iwarnr=0
        iwarno=0
        nc=0
        
        idumc=ifix(dumc)
c
c ---------------------------------------------------------
c               a. Read monthly constraints 
        istop=0
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c               a2. Read intervening structures plus loss (23)
        istop=0
        call oprFind(ityopr(k), 23, idumc,k,ion,iprinto,
     1       ix, ix, nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c ---------------------------------------------------------
c rrb 2007/07/03; 
c		a3. Read the miscellaneous limit data. Note:
c	 	      Oprlimit(k)	= 0 no limit
c		 			= 2 diversion demand limit
c		 			= 3 reservoir target limit
      iopsou(5,k)=0
      oprlimit(k)=9999.
      do i=1,12
        oprmax(k,i)=9999.
      end do  
c
c ---------------------------------------------------------
c               a3-1. Find the miscellaneous shared right limit.
      if(ioprlim(k).lt.0) then
        write(nlog, 1218) cidvri, ityopr(k), oprlimit(k)
        goto 9999
      endif
c
c ---------------------------------------------------------
c               a3-2. Find the miscellaneous reservoir limit. Note
c                	itype=2 for a reservoir
c		   	istop=0 Stop if not found
c			iacc=1 Check the diversion account
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
c               a3-3. Find the miscellaneous diversion limit. Note
c                	itype=3 for a diversion
c		   	                 istop=0 Stop if not found
c 			                 iacc=1 Check the diversion account
c                        iacc=1 Check the destination account

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
c		            a4. Read the operating rule associated with
c		                a monthly or annual plan limit adjustment
c		                when Oprlimit(k) > 0
c		                istop=0  Stop if not found
c	                 	itype=24 Operating Rule ID with monthly and annual
c                             plan limits
c               
cx    write(nlog,*) ' Oprinp; type 45 ioprlim ', ioprlim(k)
      if(ioprlim(k).eq.4) then
        istop=0
        itype=24          
        call oprFind(ityopr(k), itype, idumc,k,ion,iprinto,
     1    iopsou(5,k),iopsou(6,k), nx, cAssoc, 1, 
     1    istop, rops2,ioprsw(k), cidvri)
        iopsou(5,k)=nx 
c
c		            Set ciospoX5 to the plan associated with the 
c               above operating rule  
        ip5=iopsou(1,nx)
        ciopsoX5(k)=pid(ip5)
      endif     
c
c ---------------------------------------------------------
c               c1. Find destination diversion 
c                   Note itype=3 for a diversion 
c		   Note istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
c                        iacc=1 Check the destination account
c     write(nlog,*) ' Oprinp; Get Destination'
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
c		    Note ion=0 leaves the original water right on
c                        iacc=0 do not check account (iops2)
c		         istop=0 Stop if not found
c		         istop=1 Do not Stop if not found
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
c		Set Source 1 and type (iopdesr = -1 = reservoir water right
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
c		    ion=0 leaves the original water right on
c                   iacc=1 Check the account varaible (iops2) > 0
c		    iacc=0 Do not check the account variable
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
c rrb 2006/12/27; Allow carrier losses or seepage to go to a plan
c               f. Find destination reuse plan named Creuse, if any, 
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
c -------------------------------------------------------
c		g. Check proper type of plan 
c                Note iplntyp 8 is recharge
      if (ireuse1.gt.0) then
        ireuse(k)=ireuse1        
        
        iok=1
c
c		If the destination is a reservoir
c             the plan should be types 8 (recharge)
        if(iplntyp(ireuse1).eq.8) iok=0

        if(iok.eq.1) then
          write(nlog,1255) ityopr(k),cidvri, ciopde, creuse,
     1       iplntyp(ireuse1)
          goto 9999
        endif          
      endif  
c
c
c ---------------------------------------------------------
c		h. When the source is a diversion water right  (iopsour(k)=13
c                  Check if the carrier is the same structure 
c                  as the supply
      ndc=0
c
cx    if(iopdesr(k).ge.0) then
      if(iopSour(k).eq.13) then
        if(iopsou(1,k).lt.0) then
          nd=-1*iopsou(1,k)
          ndc=idivco(1,nd)
        else
          ndc=iopsou(1,k)
        endif
c
c		Require carrier 1 be at at the destintion location
        nc=intern(k,1)
        if(nc.ne.ndc) then
          write(nlog,1217) cidvri, ityopr(k), ciopso1, ciopde, ciopso2 
          goto 9999
        endif
c
c ---------------------------------------------------------
c		i. When the source is a diversion water right  (iopSour=13
c                  Check if the source is the same structure 
c                  Note OK if source 2 (iopsou(3,k).ne.0 to indicate
c		   the right is administered at a different location
c		   or there are intermediate carriers
c rrb 2007/11/19; Allow if there are intermediate carriers          
c       if(iopdes(1,k).eq.ndc .and. iopsou(3,k).eq.0) then          
        if(iopdes(1,k).eq.ndc .and. iopsou(3,k).eq.0 .and. 
     1     nc.eq.0) then
          write(nlog,1214) cidvri, ityopr(k),
     1      ciopso1, cdivid(ndc), ciopde
          goto 9999
        endif
      endif
        
        
        
c
c ---------------------------------------------------------
c rrb 2008/08/06; Add a new check
c		j. Check if the diversion location (source 2) is
c		   a ditch (iopsou(3,k) < 0) it must be the
c		   source (ndS) or a carrier (cntern)
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
c 		j. Print warning about admin number
      if(iwarnr .gt. 0) then
        if(iprinta.eq.0) write(ntmp,728)
        iprinta=iprinta+1
        write(nlog,729) iprinta, ityopr(k), cidvri, ciopso1,
     1    ropsrc, ropnk1, ropnk(k) 
      endif
c
c ---------------------------------------------------------
c 		k. Print warning and stop regarding carrier loss
c rrb 2008/09/26; Correction
      if(Oprloss(k).lt.smalln .or. Oprloss(k).gt.100+small) then
        write(nlog,739) cidvri, ityopr(k), Oprloss(k), cdesTyp
        goto 9999
      endif
cxc
cxc ---------------------------------------------------------
cxc		l. Set Shared Water Right indicator (iopsou(5,k)=l2)
cxc		   when Oprlimit(k) < 0
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
cxc		m. For shared water 
cxc		   Reset subsequent water right shares (2-n)
cxc                  to the water right with same source and admin date
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
c 		n. Check the Admin Location and Diversion Point
c		   For a Diversion Destination the Admin point
c		   must be the source location.
c		   For a Reservoir Destination the Admin point
c		   must be the source or destination location
c		   (e.g. not an intermediate point)
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
c 		o. Check the miscellanesous limit and water right 
c		   control (iopsou(2,k) are set properly
c		   For a diversion and a rservoir
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
c 		p. Print warning if the water right ownership is = zero
      OprPct(k)=float(iopsou(4,k))
      if(iopsou(4,k).le.1 .or. iopsou(4,k).gt.100) then
        write(nlog,737) ityopr(k), cidvri, iopsou(4,k)
        iopsou(4,k)=100
        OprPct(k)=float(iopsou(4,k))
c       goto 9999
      endif
c
c ---------------------------------------------------------
c rrb 2014-07-29
c 		q. Print warning if the water right ownership is not 100%
      OprPct(k)=float(iopsou(4,k))
      if(OprPct(k). le.99.9) then
        write(nlog,740) ityopr(k), cidvri, iopsou(4,k)
        iopsou(4,k)=100
        OprPct(k)=float(iopsou(4,k))
        goto 9999
      endif
c
c ---------------------------------------------------------
c rrb 2014-07-29
c 		q. Print warning if the source water right i ssupposed to be
c        left on (iopsou(2,k) = 0
      if(iopsou(2,k) .ne. 1) then
        write(nlog,741) ityopr(k), cidvri, iopsou(2,k)
        iopsou(2,k)=0
        goto 9999
      endif
c
c ---------------------------------------------------------
c		r. Detailed output
     
      iout45=0
      if(iout45.eq.1) then
        write(nlog,2020) ityopr(k), cidvri, ityopr(k),
     1    ciopde, iopdes(1,k), iopdes(2,k), 
     1    ciopso1, iopSou(1,k),iopsou(2,k),
     1    ciopso2, iopsou(3,k),iopsou(4,k), creuse, ireuse1,
     1    oprlimit(k), iopSou(5,k),
     1    cdivtyp(k), iopdesR(k)
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
        
        idumc=ifix(dumc)
c
c ---------------------------------------------------------
c               a. Read monthly constraints 
        call oprFind(ityopr(k), 20, idumc,k,ion,iprinto,ix, ix,
     1               nx, cx, 1, istop, rops2,ioprsw(k), cidvri)
c
c _________________________________________________________
c               b1. Find Source 1 a plan (type 7)
c		   Note itype =7 for a plan
c                       istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
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
c               b2. Check the proper plan type is specified
        if(iplntyp(iops1).ne.11) then
          write(nlog,1272) ityopr(k),cidvri, ciopso1, iplntyp(iops1), 11
          goto 9999
        endif
c
c _________________________________________________________
c               c1a. Read destination plans (type 7)
c		                 Note itype =7 for a plan
c                    istop=0 Stop if not found
c		                 istop=1 Do not Stop if not found
        backspace(55)
        ndes=ioprlim(k)
c       write(nlog,*) ' Oprinp; Type 46 ndes = ', ndes
c rrb 2011/05/23; revise to allow up to 10 owners
cx      if(ndes.lt.0 .or. ndes.gt.5) then
        if(ndes.lt.0 .or. ndes.gt.maxopr2) then
          write(nlog,1271) cidvri, ityopr(k), ndes
          goto 9999
        endif
c        
c ---------------------------------------------------------
c 		c1b; Loop for number of destinations
        sum=0.0
        n1=0
        n2=0
        do n=1,ndes
          n1=n2+1
          n2=n1+1
c          
c ---------------------------------------------------------
c		c2d. Read and set destination plan and %
c			itype=25 Read destination Plan
c                       istop=0 Stop if not found
c		        istop=1 Do not Stop if not found
 250      read(55,'(a1, 80x, a12, f8.0)',end=2040,err=2040) rec1,cx,rx
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
          ciopsoX(n,k)=ciopso1
          ciopdeX(n,k)=cx
          
          if(int(pon(iopd1)).ne.0) then
            sum=sum+rx
          endif  
c         write(nlog,*) ' Oprinp; ', n1, iopd1, n2, iopd2
c
c ---------------------------------------------------------
c               c2. Check the proper plan type is specified
          if(iplntyp(iopd1).ne.11) then
            write(nlog,1272) ityopr(k),cidvri, ciopde, iplntyp(iopd1), 11
            goto 9999
          endif
c        
c ---------------------------------------------------------
c               c3. Check that the source plan is upstream 
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
c		d. Check total percent
        if(abs(sum-100.0).gt.small) then
          write(nlog,1273) ityopr(k),cidvri, sum
          goto 9999
        endif
c
c ---------------------------------------------------------
c		e. Detailed output
     
        iout46=0
        if(iout46.eq.1) then
          n1=0
          n2=0
          do n=1,ndes
            n1=n2+1
            n2=n1+1
            ciopde=pid(iopdes(n1,k))
            write(nlog,2020) ityopr(k), cidvri, ityopr(k),
     1        ciopde,  iopdes(n1,k), iopdes(n2,k), 
     1        ciopso1, iopSou(1,k), iopsou(2,k),
     1        ciopso2, iopsou(3,k), iopsou(4,k), creuse, ireuse1,
     1        oprlimit(k), iopSou(5,k),
     1        cdivtyp(k), iopdesR(k)
          end do
        endif

        goto 1190
c _________________________________________________________
c
c              Type 47 Administrative Plan Limits
 1047   continue 
c rrb 2007/10/12;         
c              source 1 (iopsou(1,k)) = Accounting Plan
c		 destination (iopdes(1,k)= NA
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
c		        iacc=1  Allow souce 2 to represent the
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
          write(nlog,1272) ityopr(k),cidvri, ciopso1, iplntyp(iops1), 12
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
     1      cdivtyp(k), intern, cntern, cAssoc)
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
c		g. If Source 1 is a Plan
c		   Check if it is the proper type
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
c
c ---------------------------------------------------------
c               h. Find the exchange point (iopsouX)
c                  for the source and destination
        
        call oprExp(nlog, maxsta, idcdD, iscdS, idncod, ndnnod, 
     1                    iExPoint(k), cidvri)
c
c ---------------------------------------------------------
c		l. Detailed output
c     
        iout49=0
        if(iout49.eq.1) then
          call OprinOut(nlog, maxopr, k, 
     1      ityopr(k), cidvri, 
     1      ciopde, iopdes(1,k), iopdes(2,k), 
     1      ciopso1, iopSou(1,k),iopsou(2,k),
     1      ciopso2, iopsou(3,k),iopsou(4,k), creuse, ireuse1,
     1      oprloss(k), oprlimit(k), iopSou(5,k),
     1      cdivtyp(k), intern, cntern, cAssoc)
        endif  
c
        goto 1190
c        
c _________________________________________________________
c
c         
 1050   continue 
c 
c               Type 50; South Platte Compact Storage
c		   Instream flow tied to a stream gage
c		   Demand = max(0, min(ifa, qindex-q@ifa))
c
c      ion=1 means turn off opr right if right is off
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
c
c		Check Plan type is an Admin Plan         
         if(iplntyp(ndD).ne.11) then
           write(nlog,1255) ityopr(k),cidvri, ciopde, creuse, 
     1      iplntyp(ndD)
           goto 9999
         endif   
c
c
c ---------------------------------------------------------
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
c
c ---------------------------------------------------------
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
        
c
c *********************************************************
c _________________________________________________________
c          End operation file loop                                      
c
 1190 CONTINUE
c
c ---------------------------------------------------------
c		Print warning if the dimension is exceeded
C
      write(nlog,1200) cidvri, MAXOPR, maxops
      goto 9999                                                         
c
c ---------------------------------------------------------
c		Normal Exit from reading data
c		Set number of rules read
      
 1210 NUMOPR=K                                                      
c
c ---------------------------------------------------------
c		Print warning regarding Admin number
      if(iprinta.gt.0) then
        rewind(ntmp)
c
c rrb revise to read comments plus operating rule data
cs      do i=1,numopr
        do i=1,maxopr
          read(ntmp,'(a132)',end=902) rec132
          write(nlog,'(a132)') rec132
        end do
 902    rewind(ntmp)
      endif         
c
c ---------------------------------------------------------
c		Print warning to review check file 
c     if(iwarno.gt.0) write(nlog,1281)  'Operating Rights'
c
c ---------------------------------------------------------
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
 
c 
c _________________________________________________________
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
c
c _________________________________________________________
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
c
c rrb 2009/04/24; Allow isprink=2 to be mutual approach
cx      if(ifound.eq.1 .and. isprink.eq.0) then
        if(ifound.eq.1 .and. isprink.ne.1) then
          write(nlog,1204) cidvri, ityopr(k), isprink
          ioprsw(k)=0
        endif

      endif
c
c _________________________________________________________
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
c
c rrb01/04/01; Allow -1 code
c       if(ifound.eq.1.and. isoil.eq.0) then
        if(ifound.eq.1.and. isoil.le.0) then
          write(nlog,1203) cidvri, ityopr(k), isoil
          ioprsw(k)=0
        endif
      endif
c
c _________________________________________________________
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
c           
      if(iwarnp .gt. 0) goto 9999
c
c rrb 2011/07/28; the following logic is no longer required since
c                 logic is added to check all destinations including
c                 those assigned to a type 46,multi user, rule      
c rrbc
c rrbc _________________________________________________________
c rrbc               Step C5; 
c rrbc               Check if simulating an Accounting Plan (type 11) as
c rrbc               a source but no operating rule to spill (type 29) is
c rrbc               specified
c rrb      iwarnp=0
c rrb      ifound=0
c rrb      do k=1,numopr
c rrbc
c rrbc rrb 2008/12/10; Revise to check only when the source is a plan (iopsouR(k)=7)
c rrb        if(iopsouR(k).eq.7) then      
c rrbc         write(nlog,*) ' Oprinp; Test k', k
c rrb          if(ityopr(k).eq. 26 .or. ityopr(k).eq.27 .or.
c rrb     1      ityopr(k).eq.28 .or. ityopr(k).eq.46) then
c rrb            ciopso1 = ciopsoX(1,k)        
c rrb            if(ciopso1(1:2).ne.'NA' .and. ioprsw(k).ne.0) then
c rrb
c rrb              ip=iopsou(1,k)
c rrb              if(ip.gt.0) then       
c rrb                if(iPlntyp(ip).eq.11) then
c rrbc
c rrbc               Check if a type 29 rule has been specified                              
c rrb                  ifound=0
c rrb                  do k2=1,numopr
c rrb                    if(ciopso1.eq.ciopsoX(1,k2) .and. 
c rrb     1                ityopr(k2).eq.29) ifound=1
c rrb                  end do
c rrb                
c rrb                  if(ifound.eq.0) then
c rrb                    write(nlog,933) corid(k), ityopr(k), ciopsoX(1,k)
c rrb                    iwarnp=iwarnp+1
c rrb                  endif  
c rrb                endif  
c rrb              endif  
c rrb            endif  
c rrb          endif  
c rrb        endif            
c rrb      end do 
c rrbc                                          
c rrb      if(iwarnp .gt. 0) goto 9999            
c
c _________________________________________________________
c rrb 2011/07/28
c               Step C6; 
c               Check if simulating an Accounting Plan (type 11)
c               as a destination.  Note it does not work with a 
c               type 46 (multiple user) operating rule so that 
c               check is done separately (just below)
      iwarnp=0
      ifound=0
      do k=1,numopr
c
c               Check if the destination is a plan
        if(iopdesR(k).eq.7) then      
          write(nlog,*) ' Oprinp; Plan spill check k', k
c
c rrb 2014-06-15; Revise to allow a negative value to be used
c                 to indicate a plan
cx        ip=iopdes(1,k)
          ip1=iopdes(1,k)
          ip=amax0(ip1, -1*ip1)
          ciopde=ciopdeX(1,k)     
              
          if(ip.gt.0) then       
            if(iPlntyp(ip).eq.11) then
c
c               Check if a type 29 rule has been specified      
              ifound=0       
              do k2=1,numopr
                if(ciopde.eq.ciopsoX(1,k2) .and. 
     1            ityopr(k2).eq.29) ifound=1
              end do
c
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
c                                          
      if(iwarnp .gt. 0) goto 9999          
c
c _________________________________________________________
c rrb 2011/07/28
c               Step C7; 
c               If simulating a type 46 multi user rule check
c               that any Accounting Plan (type 11) destination
c               has a type 29 spill rule specified.
      iwarnp=0
      ifound=0
      do k=1,numopr
c
c               Check if the destination is a plan
        if (ityopr(k).eq.46) then
c        
c ---------------------------------------------------------
c 		c1b; Loop for number of destinations
          n1=0
          n2=0 
          ndes=ioprlim(k)
          do n=1,ndes 
            n1=n2+1 
            n2=n1+1 
            ciopde=ciopdeX(n,k)                  
            ip=iopdes(n1,k)
            
cx            write(nlog,*) '  Oprinp;', k, ityopr(k),n, n1, n2, ip, 
cx     1        iplntyp(ip), ciopde  
c                                
            if(ip.gt.0) then       
              if(iPlntyp(ip).eq.11) then
c           
c                 Check if a type 29 rule has been specified      
                ifound=0       
                do k2=1,numopr
                  if(ciopde.eq.ciopsoX(1,k2) .and. 
     1              ityopr(k2).eq.29) ifound=1
                end do
c           
                if(ifound.eq.0) then
                  write(nlog,933) corid(k), ityopr(k), ciopdeX(n,k)
                  iwarnp=iwarnp+1
                endif  
              endif  
            endif    
          end do
        endif  
      end do 
c                                          
      if(iwarnp .gt. 0) goto 9999          
c                           
c _________________________________________________________
c
c   		        Step C8; Check every reservoir plan is tied to a 
c                 reservoir
c		              Note copied from PlanEva on 6/5/06 to allow 
c                 other checks to occurr here
c                 Note Type 3=Reuse_Reservoir and 
c                 Type 5=Reuse_Reservoir_Tmtn      
c                           
c ---------------------------------------------------------
c		Initilize check
      do i=1,maxown
        iwarn(i)=0
        idum(i)=0
      end do
            
c
c ---------------------------------------------------------
c		Warn if more that one reservoir plan 
c               is tied to the same reservoir and owner
      do i=1,maxown
        if(iwarn(i).gt.1) then
          ir=idum(i)
          write(nlog,1340) cresid(ir) 
          goto 9999
        endif
      end do
c ____________________________________________________
c
c 		          Step C9; Check if the same plan (source 3) 
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

c ____________________________________________________
c
c rrb 2007/07/09; 
c 		          Step C10; Check conflicting direct flow exchange (24)
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
c
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

c
c ____________________________________________________
c
c		Close
 901  close(55)
c
c ____________________________________________________
c
c		Return                                                                       

 500  return
c
c ____________________________________________________
c
c		Warnings
c
  916 format(/
     1 '  Oprinp; Warning *.opr rule ID ', a12 ' Type ', i5,/  
     1 '          has source 2 (ciopso(2) = ',a12,/
     1 '          and a return pattern switch (iopsou(4,1) = ',i5,/
     1 '          which is not allowed. ',/
     1 '          recommend you revise the operating rule file.',/
     1 '          StateMod is continuing to operate as if it is zero')
     
  918 format(/
     1 '  Oprinp; Warning *.opr rule ID ', a12,/  
     1 '          has a release type (iopdes(4,k)) = ', i5,/
     1 '          which means make a reservoir release only if a', /
     1 '          ditch has a CIR.  Since you have the variable'/
     1 '          efficiency off in the *.ctl file this has no effect')
  
  919 format(/
     1 '  Oprinp; Warning *.opr rule ID ', a12,  
     1          ' has a destination account = ', i5,/
     1 '          which means the opr rule treats the reservoir', 
     1          ' as a total, not by an account')

  925 format(/
     1 72('_'),/  
     1 '  Oprinp; Problem with *.opr rule ID = ', a12, / 
     1 '          destination account = ', i5, ' Reset to 1')
     

c ____________________________________________________
c
c
c               Error Handling         
c
  926 write(nlog,927) iin2, filena
  927 format(/, 72('_'),/
     1 '  Oprinp; Problem. End of file # ', i4, ' encountered',/,
     1       '   File name: ', a256)
      goto 9999
c        
  928 write(nlog,929) iin2, filena, cidvri
  929 format(/, 72('_'),/
     1 '  Oprinp; Problem reading file # ', i4,/,
     1 '          File name    = ', a256,/
     1 '          Last ID read = ', a12,/
     1 '          Last record read:')

c ---------------------      
c

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
     
 2000 write(nlog,2010) cidvri, ITYOPR(K)      
 2010 format(
     1 ' Oprinp; Problem with *.opr rule ID = ', a12,
     1 ' itype ', i5, / 
     1 10x,'Cannot read destination water right',)
      goto 9999

 2020   format(/,60('_'),/
     1  '  Oprinp; Detailed output for type ', i5,/
     1  10x, 'ID and Type      = ', a12, i5,/
     1  10x, 'Destination      = ', a12, i5,/        
     1  10x, 'Destination Acct = ', 12x, i5,/             
     1  10x, 'Source 1         = ', a12, i5,/
     1  10x, 'Source 1 Acct    = ', 12x, i5,/             
     1  10x, 'Source 2         = ', a12, i5,/
     1  10x, 'Source 2 Acct    = ', 12x, i5,/             
     1  10x, 'Reuse            = ', a12, i5,/
     1  10x, 'OprLimit         = ', f12.0,i5,/
     1  10x, 'Diversion type   = ', a12,/
     1  10x, 'Destination type = ', i5)
 2021   format(
     1  10x, 'Junior Right     = ', a12, i5)
 2022   format(    
     1  10x, 'Opr Right        = ', a12, i5)
  
 2040 write(nlog,2042) cidvri, ITYOPR(K)      
 2042 format(/,72('_'),/
     1 '  Oprinp; Problem with *.opr rule ID = ', a12,' type ', i5,/  
     1 9x,'Cannot read the data associated with the ownership %',/
     1 9x,'recommend you revise the Operating rule (*.opr) file data')
      goto 9999
      
 2044 write(nlog,2046) cidvri, ITYOPR(K), iopsou(2,k)      
 2046 format(/,72('_'),/
     1 '  Oprinp; Problem with *.opr rule ID = ', a12,' type ', i5,/  
     1 9x,'Variable iopsou(2,k) = ', i5,/
     1 9x,'It should be the month when the operational limit is',/
     1 9x,'reset. For example 1= January, 2=March, etc.',/
     1 9x,'Recommend you revise the Operating rule (*.opr) file data')
      goto 9999      
      
 2048 write(nlog,2049) cidvri, ITYOPR(K), ioprlim(k)
 2049 format(/, 72('_'), /,
     1 '  Oprinp; Problem with *.opr rule ID = ', a12,' type ', i5,/  
     1 10x,'The miscellaneous limit (oprlimit) = ', i5,/
     1 10x,'When this value is > 0, the source constraint ',/ 
     1 10x,'  (iopsou(2,1) must be set to 1 to insure the ',/
     1 10x,'  constraint operates and the source right does not.',/
     1 10x,'When this value is 2 the destination should be a ',/
     1 10x,'  reservoir and a reservoir demand limit is expected.',/
     1 10x,'When this value is 3 the destination should be a',/
     1 10x,'  diversion and a diversion demand limit is expected.',/
     1 10x,'Recommend you revise the operating rule data.')  
      goto 9999    
      
 2050 write(nlog,2051) cidvri, ITYOPR(K)
 2051 format(/,72('_'),/
     1 '  Oprinp; Problem with *.opr rule ID = ', a12,' type ', i5,/  
     1 10x, 'The capability to read carrier data without a loss',/
     1 10x, '  factor is not operational.',/
     1 10x, 'Reconmend you set variable OprLoss to a non zero',/
     1 10x, '  value to indicate carrier with loss data is provided',/
     1 10x, '  and provide carrier with loss data. Note if there',/
     1 10x, '  is no loss, set variable OprLossC = 0')
      goto 9999
      
      
 9999 write(6,1250)
      write(nlog,1250)
      write(6,*) 'Stop 1'
      call flush (6)
      call exit(1)

      stop 
c
c              Formats
c _________________________________________________________
  200 format(/, 72('_'), /,'  Oprinp; Operational File (*.opr) ')
  
  201 format(/,
     1 '  Oprinp; Old operational right (*.opr) file provided',/
     1 '          That DOES NOT INCLUDE variable OprLoss and OprLimit'/
     1 '          Start Date and End Date')         
     
  202 format(/,
     1 '  Oprinp; New operational right (*.opr) file provided',/
     1 '          That DOES INCLUDE variable OprLoss and OprLimit'/
     1 '          Start Date and End Date')         
          
  203 format(/,
     1 '  Oprinp; Warning a possible mixture of new and old ',/
     1 '          operarating right formats determined',/
     1 '          This might cause problems with recent updates to ',/
     1 '          allow carrier losses. Recommend you:',/
     1 '          1. Revise the operating rule file to include',/
     1 '             Format=2.00 as the first data entry in the file',/ 
     1 '          2.Include variables OprLoss, OprLimit, ioBeg',/ 
     1 '             and ioEnd with your data.',/
     1 '          Note the *.chk file includes data in the new',/
     1 '          format including any comments in the original file')
     
  104 format(/,
     1  '  Oprinp; Warning StateMod Version 11.46 and greater revised',/
     1  '          the input data used by a Carrier (Type 11) and ',/
     1  '          Constrained Carrier (Type 14) operating rules.',/
     1  '          Specifically both allow the user to control when a',/
     1  '          source water right may be used by both a standard',/
     1  '          diversion and the carrier (iopsou(2,1)=0) or ',/
     1  '          by the carrier only (iopsou(2,1)=1). Also this',/
     1  '          change revised how data is provided to a ',/
     1  '          Constrained Demand (Type 14). ',/
     1  '          Recommend you check your operating rule data ',/
     1  '          (*.opr) and results accordingly.')
     
  105 format('  Oprinp; Operating Rule dates ',i5, 1x, a12, 1x, 3i5)           
     
  110 FORMAT(A256)                                                       
  120 format(4x, a256)
  131 format('  Oprinp; k, cidvri, nameo ', i5, 1x, a12, 1x, a24)
  132 format(a12,a24,a12,f16.0,f8.0,i8, 3(1x,a12,i8), i8,
     1         1x,a12)             
  140 format(/, 
     1 '  Oprinp; k, ityopr(k) = ', 2i5)
     
 1321   format(a12,a24,a12,f16.0,f8.0,i8, 3(1x,a12,f8.0), i8,
     1         1x,a12, 1x,a12, 1x, 2f8.0, 2i8)      
 1322   format(        a12,a24,a12,f16.0,f8.0,i8, 3(1x,a12,f8.0), i8)
 1324   format(a12,a24,a12,f16.5,f8.0,i8, 3(1x,a12,f8.0), i8,
     1         1x,a12, 1x,a12, 1x, 2f8.0,2i8, i5)             
            
  133   format(i5, 1x, a12, 1x, i5)
  134   format(12x,24x,12x,16x,  f8.0,f8.0, 3(1x,a12,i8), 20i8)  


  590 format(/, 72('_'),/
     1  '  Oprinp; Problem with Operation right = ', a12,/                 
     1 10x,'Operation type  = ', i5,/
     1 10x,'Allowable range = ', i5)
     
  592 format(/, 72('_'),/
     1 '  Oprinp; Problem with Operation right = ', a12,/                 
     1 10x,'Do not expect ', f8.0, ' structures or',                 
     1 10x,' month codes for this type of operating rule.'/
     1 10x,' Note: a negative value indicates 12 monthly codes will',
     1 10x,' be provided plus some n-12 intervening structures')             
     
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
  
  
  672  FORMAT(/,     
     1  '  Oprinp; Warning for Operation right = ', a12,/,
     1 10x,'It is carrying water from source ', a12, 
     1 10x,'through itself ', a12,/
     1 10x,'To fix: 1. Delete carrier ', a12, /,
     1 10x,'        2. Revise # of carriers')

  720 format(/, 72('_'),/
     1  '  Oprinp; Problem with Operation right = ', a12,/
     1 10x,'Cannot find source ID  ',a12, 'at location ', i5,/
     1 10x,'Note if the source is a operating right',/,
     1 10x,'that right must be on and occur in the *.opr',/
     1 10x,'file befor this right')
     
  721 format(/, 72('_'),/
     1  '  Oprinp; Problem with Operation right = ', a12,/
     1 10x,'Cannot find source ID  ',a12,' or source account ', i8)

  722 format(/, 72('_'),/
     1  '  Oprinp; Problem with Operation right = ', a12,/
     1 10x,'Cannot find destination ID or Account  ',a12) 
     
  723 format(/, 72('_'),/
     1   '  Oprinp; Problem with Operation right = ', a12,/,
     1 10x,'Destination or Source structure ',a12,/, 
     1 10x,'is the same as the carrier ',a12,/
     1 10x,'Probably remove the carrier, since it is implied by the ',/
     1 10x,'destination or source specification')

  724 format(/, 72('_'),/
     1  '  Oprinp; Problem with Operation right = ', a12,/, 
     1 10x,'Destination or Source ',a12,' cannot be found',/, 
     1 10x,'Note for a (reservoir to carrier by exchange)',/,
     1 10x,'1) the destination must be a type 11 operating right',/
     1 10x,'2) the operating right must occur befor this right, and'/
     1 10x,'3) the operating right must be on',/
     1 10x,'Note for a type 10 (replacement reservoir)',/,
     1 10x,'1) the second source, if provided, must be a type 8',/
     1 10x,'   operation right',/
     1 10x,'2) the operating right must occur befor this right, and'/
     1 10x,'3) the operating right must be on')
     
  725   format(/, 72('_'),/
     1  '  Oprinp; Problem with Operation right = ', a12,/
     1 10x,'Cannot find intervening ID  ',a12,/)

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
  
  731 format(/, 72('_'),/
     1  '  Oprinp; Problem with Operation right = ', a12,/,
     1 '    Cannot find a water right for destination ID  ',a12)
     
     
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

  737 format(/, 72('_'),/
     1   '  Oprinp; ',/
     1 10x,'Problem with Operation Type  = ',i3,' Right = ', a12,/,
     1 10x,'The Ownership percent (iopsou(4,l) = ', i5,'%',/
     1 10x,'The ownership percent was added in Version 11.55',/
     1 10x,'For backward compatiblity a value of 0 is reset to = 100%',/
     1 10x,'Recommend you revise iopsou(4,l) to be > 0 and </= 100.')

  738 format(/, 72('_'),/
     1   '  Oprinp; ',/
     1 10x,'Problem with Operation Type  = ',i3,' Right = ', a12,/,
     1 10x,'The variable Oprlimit = ', f5.0, ' that indicates water',/
     1 10x,'is shared with Operating Rule = ', a12,' But the ',/
     1 10x,'Administration dates do not equal as follows:',/
     1 10x,'Administration date 1 = ' f16.5,/
     1 10x,'Administration date 2 = ' f16.5,/
     1 10x,'Recommend you revise your operating rule data')
     
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

  741 format(/, 72('_'),/
     1   '  Oprinp; ',/
     1 10x,'Problem with Operation Type  = ',i3,' Right = ', a12,/,
     1 10x 'Variable iopsou(2,k) = ', i5, ' that allows the source',/
     1 10x,'right to be left on is not operational'/
     1 10x,'Recommend you revise iopsou(2,l) to be 1')

  760 FORMAT(/, 72('_'),/
     1  '  Oprinp; Problem with Operation right = ', a12,/                     
     1 '   Destination ID ',a12,' from the operation file (*.opr)',              
     1 '   is not in the instream flow station file (*.ifs)')

  842 format(/, 72('_'),/
     1  '  Oprinp; Problem with Operation right = ', a12,/,                     
     1 10x,'water right id ',a12,' in operation file (*.opr)',/
     1 10x,'is not in the diversion right file (*.ddr)',/
     1 10x,'Note, type 11 and 14 operation rights must have a water',/
     1 10x,'right as a source')

  923 format(/, 72('_'),/
     1 '  Oprinp; Problem with Operating  ID        = ', a12,/
     1 '          Operation type                     = ', i5,/     
     1 '          Cannot find a corresponding reservoir type -1')
     
  932 format(/,
     1 '  Oprinp; Problem with *.opr rule ID = ', a12, /
     1 '          Operation type                     = ', i5,/     
     1 '          A Reuse Plan (type 4 or 6) is specified ID = ',a12/
     1 '          However a Plan Spill (type 29) is not.',/
     1 '          Recommend you add a Plan Spill operating ',
     1            'right (type 29)')
          
  933 format(/,
     1 '  Oprinp; Problem with *.opr rule ID = ', a12, /
     1 '          Operation type                     = ', i5,/     
     1 '          An Accounting Plan (type 11) is specified ID = ',a12/
     1 '          However a Plan Spill (type 29) is not.',/
     1 '          Recommend you add a Plan Spill operating ',
     1            'right (type 29)')
      
  934 format(/,
     1 '  Oprinp; Problem with *.opr rule ID = ', a12, /
     1 '          Operation type                     = ', i5,/     
     1 '          Your source 1 is a type 3 or 5 Plan ID = ',a12,/
     1 '          that is associated with a reservoir. ',/
     1 '          If this is the correct type of plan then',/
     1 '          Recommend you revise source 1 to be a Reservoir ID',/
     1 '          and source 2 a plan ID')
     
  936 format(/,
     1 '  Oprinp; Problem with *.opr rule ID = ', a12, /
     1 '          Operation type                     = ', i5,/          
     1 '          Source 1 a reservoir ID or plan ID = ', a12,/
     1 '          Cannot be found',/
     1 '          Recommend you revise the source 1 ID')
     
  938 format(/,
     1 '  Oprinp; Problem with *.opr rule ID         = ', a12, /
     1 '          Operation type                     = ', i5,/     
     1 '          Source 1 is a plan ID = ', a12,/
     1 '          and Source 2 is a plan ID = ', a12,/
     1 '          If you have two sources, source 1 should be a',/
     1 '          a reservoir and source 2 should be a plan ID',/
     1 '          Recommend you revise your source 1 and or 2 IDs')
     
  939 format(/,
     1 '  Oprinp; Problem with *.opr rule ID         = ', a12, /
     1 '          Operation type                     = ', i5,/     
     1 '          The destination is an operational right = ',a12,/
     1 '          whose destination cannot be found',/
     1 '          Recommend you review the operational right data')
     
 1185 format(/,72('_'),/
     1 '  Oprinp; Problem with Operation right       = ', a12,/                     
     1 '          Operation type                     = ', i5,/          
     1 10x,'   Source Stream id ',a12,' in operations file (*.opr)',/              
     1 10x,' is not in the steam network file (*.rin)')
     
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
     
 1196 format(/, 72('_'), /,
     1 '  Oprinp; Problem with Operational right = ', a12,/
     1 10x,' type 15 opr rules can only be on (1) or off (0) ',/
     1 10x,' for the entire study period.  They cannot begin in ', i5)
     
 1197 format(/, 72('_'), /,
     1 '  Oprinp; Problem with Operational right = ', a12,/
     1 10x,' The destination instream flow ', a12,/
     1 10x,' is not downstream of the source water right = ', a12)
     
 1198 format(/, 72('_'),/,
     1  '  Oprinp; Problem with Operational right = ', a12,/
     1 10x,'    The destination reservoir ', a12,/
     1 10x,'    is not upstream of the source (direct flow storage)',/
     1 10x,'    water right = ', a12)
     
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
     1 10x,'Reconmend you revise the control file',/
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
     1 10x,'The source is a reservor right           = ',a12,/
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

 1216 format(2x, i5, 1x, a12, 1x, a24)       
 
 1217 format(/, 72('_'),/,
     1 '  Oprinp; Problem with Operational right           = ',a12,/
     1 '          Operation type                           =',  i3,/     
     1 10x,      'The source                               = ', a12,/
     1 10x,      'The destination                          = ',a12,/
     1 10x,      'The diversion location                   = ',a12,/
     1 10x,'A type 45 rule requires at least 1 carrier (with loss)',/
     1 10x,'be provided if a destination is not the same as the',/
     1 10x,'diversion location. Also if the source is a diversion',/
     1 10x,'water right the first carrier must be the source',/
     1 10x,'structure. Recommend you revise the operating rule.')
 
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
     
 1251 format(/, 72('_'), /,'  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'Source 2 ID          = ', a12,' and the',/
     1 10x,'The Plan ID          = ', a12,/
     1 10x,'Note: A Type 10 (General Replacement) rule expects',/
     1 10x,'      No OOP Plan ID when no source 2 is specified',/
     1 10x,'      A OOP Plan ID when source 2 is specified',/
     1 10x,'Recommend you correct the operating rule file (*.opr)')

 1252 FORMAT(/, 72('_'), /,'  Oprinp; ',
     1'Warning for Operating right type = ',i2,' ID = ', a12,/
     1 10x,'It is turned off because the Source ID or',/
     1 10x,'destination ID is turned off',/
     1 10x,'Called at Location ', i5)
     

 1253 format(/, 72('_'), /,'  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'The Plan ID        = ', a12,' and the',/
     1 10x,'Plan Type          = ',i1,11x,' are incorrect.',/
     1 10x,'Note: A Type 10, General Replacement, rule expects',/
     1 10x,'      an OOP Plan (type = 9)',/
     1 10x,'Note: A Type 27, 28, or 29 expects a T&C Plan (type 1)',/
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
     1 10x,'Plan Type          = ',i1,11x,' are inconsistent.',/
     1 10x,'Note: A T&C or Well Aug Plan should be type:     1 or 2',/     
     1 10x,'      A Reservoir Reuse Plan should be type:     3 or 5',/
     1 10x,'      A Non Reservoir Reuse Plan should be type: 4 or 6',/
     1 10x,'      A Transmountain Reuse Plan should be type: 7',/
     1 10x,'      A Recharge Plan should be type:            8',/     
     1 10x,'      A OOP Plan should be type:                 9',/
     1 10x,'      A Special Well Aug Plan should be type:   10',/
     1 10x,'      A Accounting Plan should be type:         11',/
     1 10x,'Recommend you revise the plan or operating rule data')
     
 1256 format(/, 72('_'), /,'  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'The source 1 (Plan) = ', a12,' and the',/
     1 10x,'Plan Type           = ',i1,11x,' are inconsistent.',/
     1 10x,'Note: A Reservoir Reuse Plan should be type:      3 or 5',/
     1 10x,'      A Non Reservoir Reuse Plan should be type:  4 or 6',/
     1 10x,'      A Transmountain Import Plan should be type: 7',/
     1 10x,'      A Recharge Plan should be type:             8',/
     1 10x,'Revise the plan or operating rule data')
     
 1257 format(/, 72('_'), /,'  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'The source 1 (Plan) = ', a12,', the',/
     1 10x,'Plan Type           = ',i1,11x,', and the',/
     1 10x,'The source 2 (Res)  = ', a12,' are inconsistent.',/
     1 10x,'A source 1 Recharge Plan should have a',/
     1 10x,'a source 2 Reservoir ID',/
     1 10x,'Revise the plan or operating rule data')
 
 
 1258 format(/, 72('_'), /
     1 '  Oprinp; ',
     1 'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'Diversion Type (CdivTyp) = ',a12, /
     1 10x,'For a DIVERSION destination it should be Diversion ',
     1     'or a Depletion',/
     1 10x,'For a RESERVOIR destination it should be Diversion',/
     1 10x 'For a ISF destination it should be Divesion',/
     1 10x,'Recommend you revise the operating rule data.',/
     1 10x,'Note the file currently requires the variables OprLoss',
     1     ' and OprLimit')         
     
 1259 format(/, 72('_'),/ '  Oprinp; ',
     1 'Problem with Operating right type = ',i2,' ID = ',a12,/
     1 10x,'Destination = ',a12,' a ', a9,/
     1 10x,'Source 2 = ',a12,/     
     1 10x,'A destination = Reservoir should have Source 2 = Diversion',/
     1 10x,'Revise the operating rule data')
     
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
     1 10x,'Recommend you revise the operating rule') 
     
 1266 format(/, 72('_'),/, '  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'The source or destination plan = ', a12,' a type = ', i3,/
     1 10x,'For this operating rule the plan type should be a:',/
     1 10x,'  Type 1 for a Term and Condition Plan',/
     1 10x,'  Type 2 for a Well Augmentation Plan',/
     1 10x,'  Type 9 for an Out-of-Priority Plan',/
     1 10x,'Recommend you revise the plan type')
     
12661 format(/, 72('_'),/, '  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'The destination plan = ', a12,' a type = ', i3,/
     1 10x,'For this operating rule the plan type should be a:',/
     1 10x,'  Type 1 for a Term and Condition Plan',/
     1 10x,'  Type 2 for a Well Augmentation Plan',/
     1 10x,'  Type 10 for a Special Well Augmentation Plan',/
     1 10x,'  Type 11 for an Accounting Plan',/
     1 10x,'Recommend you revise the plan type')
          
 1267 format(/, 72('_'),/, '  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'The source plan specified = ', a12,' is a type = ', i3, 
     1 10x,'For this operating rule the plan type should be a:',/
     1 10x,'  Type 3 for a CU Reuse Reservoir',/
     1 10x,'  Type 4 for a CU Reuse Diversion',/
     1 10x,'  Type 5 for a Tmtn Reuse Reservoir',/
     1 10x,'  Type 6 for a Tmtn Reuse Diversion',/
     1 10x,'Recommend you revise the operating rule or plan type')
     
     
 1268 format(/, 72('_'),/, '  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'The source 2 plan specified = ', a12,' is a type = ', i3, 
     1 10x,'For this operating rule the plan type should be a:',/
     1 10x,'  Type 9 for Out-of-Priority Diversion or',/
     1 10x,'  Type 10 for Out-of-Priority Storage',/
     1 10x,'Recommend you revise the operating rule or plan type')

 1269 format(/, 72('_'),/, '  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'Variable iopsou(4,k) = ',i5, ' but it should be > or = 1',/
     1 10x,'to indicate a monthly (1) or annual (>0) diversion limit',/
     1 10x,'Recommend you revise the operating rule')
     
 1270 format(/, 72('_'),/,
     1 '  Oprinp; Problem with Operational right     = ', a12,/
     1 '          Operation type                     = ', i5,/     
     1 '          When source 2 (ciopso(2) is an operational right',/
     1 '          iopsou(4,k) should be 0, not ', i5)   
     
 1271 format(/, 72('_'),/,
     1 '  Oprinp; Problem with Operational right     = ', a12,/
     1 '          Operation type                     = ', i5,/     
     1 '          Expect 1 to 10 destinations but oprlimit = ', i5,/
     1 '          Recommend you revise the operating rule file')
     
 1272 format(/, 72('_'),/, '  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'The plan specified = ', a12,' is a type = ', i3,/
     1 10x,'For this operating rule the plan type should be a type ',i3/
     1 10x,'Recommend you revise the plan type')

 1273 format(/, 72('_'),/, '  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'The destinations specified % sum to = ', f8.3,/
     1 10x,'For this operating rule the sum should be = 100%',/
     1 10x,'Note if a plan is off the % is not counted',/
     1 10x,'Recommend you revise the plan data')

 1274 format(/, 72('_'),/, '  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'The return flow fractions equal ', f8.1, ' not 100',/
     1 10x,'Recommend you revise your return percents')

 1275 format(/, 72('_'),/, '  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'Source account 2 (iopsou(4,k)) = ', i5,' but it'/
     1 10x,'should be 1 or 2 or 3; where:',/
     1 10x,'  1 indicates a STANDARD return pattern is used and',/
     1 10x,'  2 indicates a FIXED return pattern is used',/
     1 10x,'  3 indicates a MIXED return pattern is used',/
     1 10x,'Recommend you revise the operating rule file')
     
 1276 format(/, 72('_'),/, '  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'This operating rule type is not currently operational',/
     1 10x,'Note the old type 26 rule is now a type 47 rule',/
     1 10x,'Recommend you revise the operating rule file')
     
 1277 format(/, 72('_'),/, '  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'This operating rule type is not defined',/
     1 10x,'Recommend you revise the operating rule file')
     
 1278 format(/, 72('_'), /,'  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'The source 1 (Plan) = ', a12,' and the',/
     1 10x,'Plan Type = ',i2,' (recharge) requires',/
     1 10x,'Source 2 be a reservoir, not = ', a12,/
     1 10x,'Recommend you revise the operating rule data')     
     
 1279 format(/, 72('_'), /,'  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'The miscellaneous limit (oprlimit) = ', i5,/
     1 10x,'When this value is 0 no additional data is expected',/
     1 10x,'When this value is 2 a diversion ID is read',/
     1 10x,'When this value is 3 a reservoir ID is read',/
     1 10x,'Recommend you revise the operating rule data')     

     
 1280 format(/, 72('_'),/, '  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'The source plan = ', a12,' a type = ', i3,/
     1 10x,'For this operating rule the plan type should be a:',/
     1 10x,'  Type 11 (administrative) for the South Platte Compact',/
     1 10x,'Recommend you revise the plan type') 
    
 1281  FORMAT(/,72('_'),/
     1  '  Oprinp; Warning See *.chk for details regarding: ',a24)

     
 1282 format(/, 72('_'),/, '  Oprinp; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'Source 2 = ', a12, ' cannot be found or is set to NA',/
     1 10x,'This is not allowed. Recommend you revise the operating',/
     1 10x,'rule file (*.opr)')
     
 1300 format(/,72('_'),/                                                          
     1 '  Oprinp; Problem with Operating Right ', a12,, ' Type = ',i2,/
     1 '          The destination ID = ',a12, ' cannot be found',/
     1 '          Recommend you confirm it exists and is a structure',/
     1 '          type supported by this operating rule')
     
 1330  format(/,72('_'),/
     1 '  Oprinp; Problem Reservoir Plan ID ',a12,/
     1 '          Is tied to more than one Reservoir and Account',/
     1 '          Reservoir ID 1 = ', a12,' Account 1 = ', i5,/
     1 '          Reservoir ID 2 = ', a12,' Account 2 = ', i5,/
     1 '          Recommend you revise the plan data or ',/
     1 '          the operating rule data')
 1340  format(/,72('_'),/
     1 '  Oprinp; Problem reservoir ID ',a12,' has one account ',/
     1 '          Tied to more than one Reservoir Plan',/
     1 '          Recommend you revise the plan data or ',/
     1 '          the operating rule data')
     
 1350  format(/, 72('_'),/, 
     1 '  Oprinp; Problem with Operating right type = ',i2,
     1            ' ID = ', a12,/
     1 '          It is tied to a plan ID = ', a12,/
     1 '          It must be tied to a Out-of-Priority Plan',
     1            ' (type 9)',/
     1 '          Recommend you revise the plan data in ',/
     1 '          the operating rule data')
     
 1352  format(/, 72('_'),/, 
     1 '  Oprinp; Problem with Operating right type = ',i2,
     1            ' ID = ', a12,/
     1 '          The source well right = ', a12,/
     1 '          Cannot be found in the well station file (*.wer)',/
     1 '          Recommend you revise the operating rule file',/
     1 '          file (*.opr) or the well right file (*.wer)',/
     1 '          Continuing with the right turned off')
     
 1360  format(/, 72('_'),/, 
     1 '  Oprinp; Problem with Operating right type = ',i2,
     1            ' ID = ', a12,/
     1 '          The source diversion right ID = ',a12,/
     1 '          The source diversion ID       = ',a12,/
     1 '          The destination diversion ID  = ',a12 ,/
     1 '          The source and destination ID should be the same',/
     1 '          Recommend you revise the source or destination in ',
     1 '          the operating rule data')
     
 1370  format(/, 72('_'),/, 
     1 '  Oprinp; Problem with Operating right type     = ', i2,,/
     1 '          Operating right ID                    = ', a12,/
     1 '          has a T&C requirement with the source = ', a12,/
     1 '          but operating rule ID                 = ', a12,/
     1 '          has a T&C requirement with the use    = ', a12,/
     1 '          Therefore the T&C requirments are included twice',/
     1 '          Recommend you remove the T&C requirement on one',/
     1 '          of the above operating rules')
     
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
c _________________________________________________________
c

      END
