c
      Subroutine GetPln(numstax)
c
c _________________________________________________________
c	Program Description
c	 GetPln It reads plan data
c _________________________________________________________
c	Dimensions
      include 'common.inc'
c
      DIMENSION ITEMP(numstax), ntype(25)
C
      character ch3*3, ch4*4, blank*12, crtnid*12, xmon*4,
     1          recin*256, cgoto2*12, cx*12, rec4*4, rec24*24,
     1          rec1*1, rec12*12, rec72*72,
     1          filenaR*256, idreqX*12
     
c     
c _________________________________________________________
c
c     write(6,*) ' Subroutine GetPln'
      write(nlog,*) ' Subroutine GetPln ', nlog

c     
c _________________________________________________________
c
c		Step 1; Initilize 
c
c		iout = 0 No details on plan data
c		       1 Plan data Details
c		       2 Plan data Summary
c		       5 Plan Return flow summary
      iout=0
c
c rrb 2013/02/03; Allow type 13 = Changed Water Right     
cx    maxPlnT1=12
      maxPlnT1=13
      
      ibad=0
      small=0.001
      
      if(iout.eq.1) write(nlog,*) ' GetPln; maxplnT ', maxplnT
      do i=1,MaxPlnT
        plntypX(i)='NA'
        ntype(i)=0        
      end do
      
      plntypX(1) = 'T&C_Requirement'
      plntypX(2) = 'Well_Augmentation'
      plntypX(3) = 'Reuse_Reservoir'
      plntypX(4) = 'Reuse_Diversion'    
      plntypX(5) = 'Reuse_Reservoir_Tmtn'
      plntypX(6) = 'Reuse_Diversion_Tmtn'
      plntypX(7) = 'Transmountain_Import'
      plntypX(8) = 'Recharge_Plan'
      plntypX(9) = 'Out-of-Priority_Reservoir'
      plntypX(10)= 'Special_Well_Augmentation'
      plntypX(11)= 'Accounting_Plan'
      plntypX(12)= 'Release_Plan_Limit'     
c
c rrb 2015-03-07
      plntypX(13)= 'Changed Water Right'

      ifn=53
      filena=fileName(ifn)
      if(iout.eq.1) write(nlog,*) ' GetPln; filena = ', filena
        
      if(filena(1:2).eq.'-1') then
        write(nlog,*) ' GetPln; FYI no plan data provided'
        nplan=0
        goto 906
      endif
c
c		Initilize  
      if(iout.eq.1) write(nlog,*) ' GetPln; maxplan, ', maxplan
      do i=1,maxplan
        Pid(i)=' '
        Pon(i)=0
        iPlnTyp(i)=0
        iPrf(i)=999
        iPfail(i)=0
        Psource(i)='NA'
        ipSource(i)=0
c
c rrb 2009/03/10; Move plan reservoir data to GetPlnR        
cx        iplnR(i)=0
        nrtnpp(i)=0
      end do  
      
      iin2=55
c
      if(iout.eq.1) write(nlog,*) ' GetPln; nchk, ', nchk
       write(nchk,*) ' GetPln'
  
       
      if(iout.eq.1) write(nlog,*) ' GetPln; filena ', filena
      write(nchk,940) filena
      call skipn(55)
c     
c _________________________________________________________
c
c		Step 2; Read Plan data 
      if(iout.eq.1) write(nlog,*) ' GetPln; maxplan ', maxplan
      nplan=0      
      ibadR=0
      
      if(iout.eq.2) then
        write(nlog,*) '  GetPln;      i Pid          ',
     1          'Psource        iresP   Cresid'
      endif
 
      Numplan=0
      do i=1,maxplan        
c           
c		Note iocode 1 = Data, 2 = EOF, 3 = Error                    
        call comment(55, nlog, iocode, nchk, 0)
        if(iout.eq.1) write(nlog,*) ' GetPln; iocode = ',iocode
        if(iocode.eq.2) goto 906
        if(iocode.eq.3) goto 928      
      
          read(55,*,end=906, err=906) 
     1      Pid(i), rec24, cgoto, Pon(i), iplnTyp(i), 
     1      Peff1, iPrf(i), iPfail(i), psup1, Psource(i),
     1      iPacct(i)
     
          Numplan=numplan+1    
c
c ---------------------------------------------------------     
          if(iout.eq.1) then
            if(i.eq.1) then
cx            write(6,*)
cx              write(nlog,903)
cx            write(nlog,*)
            endif
            
            write(nlog,*)  
     1        i, Pid(i), rec24, cgoto, Pon(i), iplnTyp(i),
     1        peff1, iPrf(i), iPfail(i), Psup1, Psource(i),
     1        iPacct(i)
          endif
     
c
c		Pack Id to left     
          pid(i)=adjustl(pid(i))
          psource(i)=adjustl(psource(i))
          pname1(i)=rec24
cx        nameP(i)=rec24
          
c
c		Use scratch file to put name into a real variable     
cx          write(4,*) rec24
cx          backspace(4)
cx          read(4,'(a24)') (pname(j,i), j=1,6)


cyyyy           read(rec24,'(a24)') (pname(j,i), j=1,6)
c          if(iout.eq.1) write(nlog,'(a24)') (pname(j,i), j=1,6)
c     
c _________________________________________________________
c
c		Step 3; Set Plan Type
c
c		Set plan type           
          do it=1,maxPlnT
            if(iplnTyp(i).eq.it) then
              plntypc(i) = plntypX(it)
              ntype(it)=ntype(it)+1
            endif
            if(iplnTyp(i).gt.maxplnT1) then
              write(nlog,910) iplnTyp(i)
              goto 9999
            endif
          end do 
c     
c _________________________________________________________
c
c		Step 4; Set Plan Efficiency data 
c		NOte +n read 12 values
c		     -1 read with operating rule data
c		    999 default (set to destination)
c
c		Get monthly efficiency data
          ipEff1=peff1
c         if(peff1.gt.0.0 .and. peff1.le.998.9) then
          if(ipeff1.gt.0 .and. ipeff1.lt.999) then
            write(nlog,*) ' GetPln; Reading efficiency data', 
     1       peff1, ipEff1
            read(55,*,end=928,err=928) (peff(j,i), j=1,12)
          endif  
c          
c		Initilze default efficiency
          if(peff1.gt.998.0) then
            do j=1,12
              peff(j,i) = -1.
            end do
          endif  
c     
c _________________________________________________________
c
c		Step 5; Initilize plan failure and initial plan supply 
c		Note Psto1 and Psto2 are in acft
c		Psup and PsupD are in cfs
          pfail(i)=0.0
c
c rrb 2009/01/23; update to include type 12
cx     1       iplnTyp(i).eq.9) then
          if(iplnTyp(i).eq.3 .or. iplnTyp(i).eq.5 .or.
     1       iplnTyp(i).eq.9 .or. iplntyp(i).eq.12) then
            psto1(i)=psup1
            psto2(i)=psup1
            psup(1,i)=psup1/factor/mthday(1)
            psupD(1,i)=psup1/factor
          endif  
c     
c _________________________________________________________
c
c		Step 6;	Set counter (nplan) and plan location (ipsta)
          nplan=nplan+1
          ipsta(i)=0
          DO IS=1,NUMSTA
            if(cstaid(is).eq.cgoto) then
              ipsta(i)=is
c     
c ---------------------------------------------------------
c
c
c		If a reuse plan; set point on the river to on
c rrb 05/03/10
c
c rrb 2015-03-07; New plan type
c             if(iplnTyp(i).eq.4) then
              if(iplnTyp(i).eq.3 .or. iplnTyp(i).eq.4 .or.
cx   1           iplnTyp(i).eq.11) then
     1           iplnTyp(i).eq.11 .or. iplnTyp(i).eq.13) then     
                ipnode(is)= i            
                
                if(iout.eq.1) 
     1          write(nlog,*) ' GetPln; Found reuse node = ', is,
     1          ' for plan ID ', pid(i)
              endif
            endif  
          end do
c     
c ---------------------------------------------------------
C
          if(ipsta(i).eq.0) then
            write(nlog,950) pid(i), cgoto
            write(6,950) pid(i), cgoto
            ibad=ibad+1
cx          Goto 9999
          endif  
c     
c _________________________________________________________
c
c		Step 7; Recharge Plan (type 8) 
c		  Check the Source & Return Type are set properly.
          if(iplnTyp(i).eq.8) then
            if(Psource(i).eq.'Reservoir   ' .or.
     1         Psource(i).eq.'Diversion   ') then
            else
              write(nlog,922) pid(i), psource(i)
              goto 9999
            endif            
          endif
c     
c _________________________________________________________
c
c		Step 7; Recharge Plan (type 8) Check 
c		Note ipsource must be a reservoir 
c                   Note itype=2 for a reservoir
c                        istop=1 Do not Stop if not found
c			 iacc=0 Do not check reservoir account
c
c
c ---------------------------------------------------------
c		  Tie data in a res recharge plan (type 8, account>0)
c		  to a reservoir
c		  Note the data in creuse is for the carrier loss          
c                   Note itype=2 for a reservoir
c                        istop=1 Do not Stop if not found
c			 iacc=0 Do not check reservoir account
c
c  2009/03/04; Revise to put reservoir data in *.plr so that
c		 one plan can be tied to many reservoirs
c rrb           if(iplnTyp(i).eq.8 .and. ipAcct(i).gt.0) then
c rrb             itype=2
c rrb             istop=1
c rrb             iacc=0
c rrb             rops2=0.0
c rrb             call oprFind(-1, itype, 0, i, ion, -1,
c rrb      1           nr, iacc, nx, psource(i), iacc, istop,rops2,
c rrb      1           ioprsw1,Pid(i))
c rrb cx          ipRes(i)=nr
c rrb             
c rrb             if(nr.gt.0) then
c rrb               iresP(nr)=i
c rrb               iplnR(i)=nr
c rrb               if(iout.eq.2) then
c rrb      
c rrb                 write(nlog,'(a12,i5, 1x,a12, 1x,a12, 2i8, 1x,a12)')
c rrb      1            ' GetPln;  ', i, pid(i), psource(i),
c rrb      1          iresP(nr), iplnR(i),  cresid(nr)
c rrb               endif
c rrb             else
c rrb               if(pon(i).ge.small) then
c rrb                 ibadR=ibadR+1
c rrb                 if(ibadR.eq.1) write(nlog,918)
c rrb                 write(nlog,919) ibadR, pid(i), psource(i)                
c rrb               endif  
c rrb             endif
c rrb           endif  

    
c     
c _________________________________________________________
c		Step 9; End Plan loop
      end do    
      
      
c     
c _________________________________________________________
c		Step 10; Stop if the Dimension exceeded. Stop
cx    write(nlog, 905) maxplan
      write(nlog,*) ' '
      write(nlog,*) ' GetPln; Problem with maximum dimension ', maxPlan
      goto 9999
c     
c _________________________________________________________
c		Step 11; Normal exit; print number of plans
      
 906  continue
      write(nlog,912)
      i1=0
      do i=1,maxplnT1
        if(ntype(i).gt.0) then
          i1=i1+1
          write(nlog,914) i1, i,  plntypX(i), ntype(i)
        endif
      end do
      write(nlog,916) i1, nplan
 
cx      write(nlog,907) nplan      
        write(nlog,*) ' '
        write(nlog,*) ' GetPln; Number of Plans        = ', nplan
c
c _________________________________________________________
c		Step 12; Warn if plan data was expected but not provided     
      if(filena(1:2).ne.'-1' .and. nplan.eq.0) then
        write(nlog,930) nplan
        goto 9999
      endif   
c
c _________________________________________________________
c		Step 13; Check if more than 1 plan is tied to the same 
c                        reservoir
c
c rrb 2009/03/10; Move plan reservoir data to GetPlnR        
c rrb
c rrb      if(iout.eq.1) write(nlog,*) ' GetPln; Check res plans'
c rrb
c rrb      ibad=0
c rrb      do np=1,nplan-1
c rrb      
c rrb        iplnR1=iplnR(np)
c rrb        ipacct1=ipacct(np)
c rrb        if(iout.eq.1) write(nlog,*) ' GetPln; np ', np, iplnR1
c rrb        
c rrb        if(iplnR1.gt.0) then
c rrb        
c rrb          do j=np+1,nplan
c rrb            if(iout.eq.1) write(nlog,*) ' GetPln; ', 
c rrb     1        j, iplnR(j), ipacct(j)
c rrb            
c rrb            if(iplnR1.eq. iplnR(j) .and. ipacct1.eq.ipacct(j)) then
c rrbcx            write(nlog,970) pid(j), psource(j)            
c rrb              write(nlog,*) ' Getpln; ',
c rrb     1        'Problem multiple plans tied to the same reservoir ',
c rrb     1        '& account ', pid(j), psource(j), ipacct(j)            
c rrb     
c rrb              ibad=ibad+1
c rrb            endif        
c rrb          end do  
c rrb        endif
c rrb      end do
c rrb
c rrb      if(ibad.ge.1) goto 9999      

      
c _________________________________________________________
c		Step 14; Close plan file  
      if(iout.eq.1) write(nlog,*) ' GetPln; close 55'
    
      close(55)
c      
c _________________________________________________________
c
c		Step 14 Get Plan return flow data (*.prf)
c		        Note: Originally developed for Recharge Plans
c			            but works generically to route seepage
c                 to the stream as a return flow 
c
c 		Open file
      inP=0
      numrtnPP=0  
      
      write(nlog,901)
c
c rrb 2008/02/22; Read new or old response file formats (infile)
c		  add path (fpath1), open file (filena), get
c		  number of stations (numopr, if 0) and get 
c		  version number (ioprX)
c		  inf = filename
c		  nf = file #
      inf=70
      nf=55
      if(iout.eq.1) write(nlog,*) ' GetPln; Calling GetFile'
      call GetFile(
     1   nlog, nchk, iin, infile, maxfile, maxfn, 
     1   nf, iprnX, numPrf, fileType(inf), fileSuf(inf), 
     1   fileName(inf), fpath1, filena)
     
      if(numPrf.gt.0) inP=55
      if(iout.eq.1) write(nlog,*) ' GetPln; numprf, inp', numprf, inp
c
c _________________________________________________________
c
c		Step 15; IF a plan return file (*.prf) is provided (inP>0) get 
c                       Plan Return Flow data
c			Note ityp=0 is for return flows
c			istrTyp=7 for a plan

      if(inP.gt.0) then           
c       if(iout.eq.5)
        if(iout.eq.5) then
          write(nlog,*) ' GetPln; Calling getRtnX for Plan Returns'
          write(nlog,*) ' GetPln; Ret_1',
     1    maxrtnA,   maxrtnPP, maxsta,   maxplan,  nstrtn   
        endif
               
        ityp=0
        istrTyp=7
        call getrtnX(
     1    nlog,      nchk,     iprintx,  istrTyp,  maxrtnA,
     1    maxrtnPP,  maxsta,   maxplan,  nstrtn,   in1,
     1    inP,       nw,       ji,       je,       iin2,
     1    numsta,    ityp,     iloss,    interv,   pcttotPP,
     1    pctlosPP,  irtndlPP, irnordPP, irnstaPP, numrtnPP, 
     1    nrtnPP,    nplan,    istrtn,   ndnnod,   idncod,
     1    cgoto,     pname1,    Pid,      cstaid,   cstadn,
     1    filenaR)
     
        if(iout.eq.5) then
          write(nlog,*) ' GetPln; Back from getRtnX for Plan Returns'        
          write(nlog,*) ' GetPln; Calling getRtnX for Plan Returns'
          write(nlog,*) ' GetPln; Ret_2',
     1    maxrtnA,   maxrtnPP, maxsta,   maxplan,  nstrtn  
        endif    
        
        if(iout.eq.1) 
     1    write(nlog,*) ' GetPln; Back from getRtnX for Plan Returns'
      endif
c
c _________________________________________________________
c
c		Step 16; Check the Plan return location is
c		         a Plan and is the correct type
c			 Also print detailed results
c
        if(iout.eq.5) write(nlog,1430)
        j2=0
        do np=1,nPlan
          jb=nrtnPP(np)
          je=nrtnPP(np+1)-1
c
c		Print non default return flow data
          do j=jb,je
            j2=j2+1
            is = irnstaPP(j)
            irnPlan(j)=0
            idreqX='NA'
c
c		Check the plan return file returns to a river location
c		with a T&C plan
            if(iprf(np).eq.1) then
              call getsta(7, is, is, ip, idreqX)
              if(ip.gt.0 .and. iplntyp(ip).eq.1) then
                irnPlan(j)=ip
              else
                write(nlog,990) Pid(np), cstaid(is), iplntyp(ip)
                goto 9999
              endif  
            endif
c
c		Print non-default return data            
            if(iout.eq.5) then
              write(nlog,1432) j2, pid(np), np, jb, je, j, 
     1          cstaid(is), pcttotPP(j), irtndlPP(j), irnPlan(j)            
            endif
          end do
c
c		Print default return data          
          if(je.lt.jb) then          
            j2=j2+1
            if(iout.eq.5) then
              write(nlog,1432) j2, pid(np), np, jb, je, -1  
            endif
          endif
        end do    
c     endif  
c _________________________________________________________
c
c		Step 18; Close Plan Return File
      if(iout.eq.1) write(nlog,*) ' Getpln; close nf ', nf
      close(nf)     

c
c _________________________________________________________
c
c		Step 17; Warn the user if plan return flow is expected
c			 but data is not provided
      if(iout.eq.1) 
     1 write(nlog,*) ' Getpln; check plan return data ', nplan
     
      do i=1,nplan
cx      write(nlog,*) ' GetPln; ', i, iprf(i)
        if(iprf(i).eq.-1) then
          jb=nrtnPP(i)
          je=nrtnPP(i+1)-1
          if(je-jb.lt.0) then
            write(nlog,980) Pid(i), iplntyp(i), iprf(i)
            goto 9999
          endif
        endif
      end do    
            
c      
c _________________________________________________________
c
c		Step 19; Return
      if(iout.eq.1) write(nlog,*) ' GetPln; return'
      return
c      
c _________________________________________________________
c
c               Formats
 
    
 114  format(/,72('_'),/
     1  '  GetPln; Plan Return File (*.prf) ')      
     
 901  format(/,72('__'),/' GetPln; Plan Return File (*.prf) ')
     
 903  format(/,
     1 '  GetPln; Plan Data:',/
     1 '    # Plan ID      Plan Name                Location     ',
     1 '  On/Off    Type     Eff  Return Failure    Sto1',
     1 ' Source  ',/
     1 ' ____ ____________ ________________________ ____________',
     1 '  _______ _______ _______ _______ _______ _______',
     1 ' _______')  
     
     
 904  format(
     1 i5, 1x,a12, 1x,a24, 1x,a12,1x, f8.0, i8,
     1 f8.0, i8, i8, f8.0, 1x,a12, f8.0)
     
 905  format(/,72('_'),/,
     1 '  GetPln; Problem number of plans exceeds the dimension',
     1 ' of ', i5,/, 10x, ' Revise StateM and Common.')
     

 907  format(/,
     1 '  GetPln; Number of Plans        = ', i3)
     

 908  format(/,
     1 '  GetPln; Number of Plan Returns = ', i3)
      
 910  format(/,72('_'),/,
     1 '  GetPln; Problem plan type = ', i5, ' is not defined',/
     1 '          recommend you revise the Plan Station Data (*.pln)')
         
 912  format(/,72('_'),//
     1 '  GetPln; Number of Plan Types '/,
     1 '    # Type Description                Number',/
     1 ' ____ ____ _________________________ _______' )
     
 914  format(2i5, 1x, a25, i8)    
 
 916  format(
     1 ' ____ ____ _________________________ _______' ,/
     1 i5,  '   NA Total                    ',i8) 

 918  format(/,72('_'),//
     1 '  GetPln;   Warning, the following Recharge Plans',
     1                ' are not tied reservoir '/
     1 12x,'This Recharge Plan is being ignored. ',/
     1 12x,'recommend you revise the plan station file (*.pln)',/
     1 12x,'or the Reservoir Station file (*.res)',/
     1 '    # Plan ID      Reservoir ID')
 919   format(i5, 20(1x,a12))    
     
 922  FORMAT(/,72('_'),/
     1  '  GetPln;   Warning. Plan ID = ',a12,' of the Plan Station ',/
     1  12x,'(*.pln) file is Recharge Plan (type 8). ',/
     1  12x,'With a Source = ', a12, '. A Recharge Plan must have ',/
     1  12x,'a Source = Reservoir or Diversion',/
     1  12x,'Recommend you revise the Plan Type or Source in the',/
     1  12x,'Plan Station File (*.pln)')
     
 924  FORMAT(/,72('_'),/
     1  '  GetPln;   Warning. Plan ID = ',a12,' of the Plan Station',/
     1  12x,'(*.pln) file is Recharge Plan (type 8). ',/
     1  12x,'With a Source = ', a12, ' and a Plan Return Type = ', i5,/
     1  12x,'When the Source is a Reservoir it should have a',
     1      ' Return Type = 8',/
     1  12x,'When the Source is a Diversion it should have a',
     1      ' Return Type = 999',/
     1  12x,'Recommend you revise the Plan Type, Source, or',/
     1  12x,'Plan Return Type in the Plan Station File (*.pln).')
     
 930    format(/,72('_'),/
     1  ' GetPln; Problem provided a plan file but number of ',
     1           'plans read = ',i5,/
     1  '         Reconmmend you check plan input or remove the file',/
     1  '         Note an inital plan storage is required on input') 

 940  format(4x, a256)
     
 950  format(/, 72('_'), /,
     1  '  GetPln; Problem with Plan ID = ', a12,' the location = ',/
     1  '          ',a12,' cannot be found in the river station ',/
     1  '          (*.rin) file',/
     1  '          recommend you add to the river station file')
     
 960  format(/, 72('_'), /,'  GetPln; ',
     1     'Problem with Plan ID = ', a12,' Plan Type = ', i5,/
     1 10x,'Source ID = ', a12, ' and Return Flow ID = ', i12,/
     1 10x,'are inconsistent. A recharge plan (type 8) requires',/
     1 10x,'the Source ID be a Reservoir',/            
     1 10x,'recommend you revise the plan data')
     
 970  FORMAT(/,72('_'),/
     1  '  GetPln; Problem. ',
     1  'Plan ID ',a12,' of the Plan Station (*.pln) file is a ',/
     1  '          Reservoir Recharge Plan (type 8 with account > 0)',/
     1  '          that is tied to the same reservoir ', a12,/
     1  '          as another recharge plan. This is not allowed',/
     1  '          recommend you revise your plan data')
   
 980  FORMAT(/,72('_'),/ '  GetPln; ',
     1      'Problem with Plan ID = ', a12,' Plan Type = ', i5,/ 
     1 10x, 'When the Return Flow ID = ', i5, ' return flow data',/
     1 10x, 'is expected in the plan return (*.prf) file.',/
     1 10x, 'recommend you:',/
     1 10x, '  Revise the response (*.rsp) file to include a plan',/
     1 10x, '    return file (*.prf) or',/
     1 10x, '  Revise the plan return (*.prf) file or',/
     1 10x, '  Revise the plan (*.pln) file')
     
 990   FORMAT(/,72('_'),/
     1 '  GetPln;  Problem.'
     1 ' Structure ',a12,' of:',/
     1 '           The Plan Return File (*.prf)',/
     1 '           Returns to ', a12, ' where a plan is not located',/
     1 '           or it is a plan type = ', i5,/
     1 '           The return location should be a T&C (type 1) Plan',/
     1 '           Recommend you revise the plan (*.pln) or plan ',
     1            'return (*.prf) file') 
     
 1021  FORMAT(/,72('_'),/
     1  '  GetPln; Problem.',
     1  ' Number of return stations = ',i5, 
     1  9x, 'Exceeds the dimension = ', i5,/
     1  ' recommend you revise the common block size')

c      
c _________________________________________________________
c
c               Error Tracking
 1410 write(nlog,1420) filena
 1420 format(/,72('_'),/
     1 '  GetPln; Problem opening file: ', a256)
      goto 9999
      
 1430 format(/,
     1 '  GetPln;',/
     1 '      J2 ID                 Np      jb      je       j',
     1 ' cstaidX      pcttotX irtndlX  irnPlan',/
     1 ' _______ ____________  _______ _______ _______ _______',
     1 ' ____________ _______ _______ _______')
      
 1432 format(i8, 1x,a12,1x, 4i8, 1x,a12, f8.0, 2i8)               
      

      
  926 write(nlog,927) iin2, filena
  927 format(/,72('_'),/
     1 ' GetPln; Problem. End of file # ', i4, ' encountered',/,
     1 '   File name: ', a256)
      goto 9999
c
  928 write(nlog,929) iin2, filena
  929 format(/,72('_'),/
     1 ' GetPln; Problem reading file # ', i4,/,
     1       '   File name: ', a256,/
     1       '   Problem record (next line):')

cx      backspace(iin2)
cx      read(iin2, '(a256)',end=926,err=926) recin
cx      write(nlog,'(a256)') recin
cx      goto 9999
c      
c _________________________________________________________
c
c               Warn and Stop
      
 9999 write(6,1440) 
      write(nlog,1450) 
      call flush(6)
 1440 format(/, 72('_'),/,
     1 ' Getpln;  Stopped in GetPln, see the log file (*.log)')
 1450 format(/, 72('_'),/,' GetPln; Stopped in GetPln')
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)
      stop     
      
      end
