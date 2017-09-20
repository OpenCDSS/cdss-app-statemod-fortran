c
c *********************************************************
c
      subroutine replace(iw,l2,nrepcall,divactx,ncall)
c
c
c _________________________________________________________
c	Program Description
c
c       Replace a Type 10 Operating Rule
c       for a general replacement reservoir
c
c _________________________________________________________
c      Update History
c
c rrb 2007/10/26; Add maximum release limits by calling
c                 DivresP2 and DivRplP
c
c rrb 2006/10/04; Revised to no longer tie to a subordinated right or
c		  an OOP Plan. Non Green Mountain Replacements will 
c		  be implemented by:
c		    Using a type 34 rule to transfer an OOP obligation
c		    to an OOP account in Wolford, etc.
c		    Therefore when this rule files there is no need
c		    to limit Wolford, etc. to water in an OOP account.
c		    Also there is no need to book water from the OOP
c		    account.
c rrb 2006/08/04; Revised to no longer tie to an operating rule (lk)
c		  instead if associated with an OOP diversion
c		  provide subordinated reservoir water right in
c		  variable iopsou(3,k) and OOP Plan in ireuse(k)
c rrb 2006/07/27; Revise to recognize an OOP Plan
c rrb 2004/31/96; Add constraint for replacement limited by a bookover
c rrb 2002/05/28; Add exit if no water right remains
c
c
c _________________________________________________________
c      Documentation
c               iw = water right counter
c               iwx = iteration counter
c               l2 = destination diversion water right pointer
c               n1 = replacement reservoir counter
c               lr = pointer for a replacement reservoir operation right
c                    note changes based on number of replacement 
c                    reservoirs (irepn)
c               irepn = number of replacement reservoirs
c
c               nrepcall=# of calls to replace per month
c
c               divactx = reservoir release associated with this
c                         call. Note if releasing for depletion
c                         the release and diversion do not equal.
c                         (see Divrpl.f divactx=-repact).
c                         Note currently only divrpl allows a
c                         depletion replacement.
c
c               divacty = diversion associated with this call.
c               
c               numopr = total number of operation rights (riginp.for) 
c
c               idivco(1,l2)  = structure associated with water right l2
c               idivco(2,l2)  = user associated with water right l2
c               iopdes(1,lr)  = destination diversion
c               iopdes(2,lr)  = destination diversion account
c               
c               ireprnk(n1)   = Rank of replacement reservoir
c               irepk(n1)     = Operation ID of replacement reservoir
c               irepnr(n1)    = replacement reservoir counter
c               irepown(n1)   = replacement reservoir owner
c               irepexp(n,n1) = Exchange point for replacement 
c                               reservoir n1 destination n 
c                             0=N/A (direct supply)
c                             +=river ID of exchange point
c               ireptyp(n1)   = Replacement type from datinp.for
c                             1 = 100% replacement
c                            -1 = depletion replacement
c                             0 =no replacement reservoir
c                                  (checked in execut befor call)
c               iopsou(3,n1)  = 0 
c               tranlim       = Transfer limit to amount in secondary
c                                source storage or source right (nrwr)    
c               dcrdivx       = decree of destination diversion right
c               divdx         = diversion to date of diversion right
c               divo(lr)      = cumulative diversion to date of
c                               replacement
c
c               ioprsw        = 0 opr right is off 
c                             = -n; simulate until year n (e.g. -1980)
c                             = n; begin to simulate in year n (e.g. 1980)
c
c               iout          = 0 no detailed printout
c                             = 1 detailed printout
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
      dimension ncall(200)      
      character creptyp*8, cwhy*48, cDest*12, cDest1*12, cSour*12
c
c _________________________________________________________
c               Step 1;Initilize
c
c     iout=3 details for replacement results
      iout=0
      ioutiw=0

      cDest='NA'
      cSour='NA'
      
      
cx      cDest1='510585      '
cx      monout=1
      cDest1='500734      '
      monout=11
c
c rrb 2008/03/13; Initilize
      divactx=0.0     
c
c rrb 2015/09/06; Initilize
      divacty=0.0 
c
c ---------------------------------------------------------
c		Note:
c                 cridid(l2) is the right being replaced      
c		              corid(xx) is the replacement reservoir rule
c		              not known until the reservoir loop

      if(ichk.eq.110) iout=2
c     if(corid(l2).eq. ccall) ioutiw=iw
      if(crigid(l2).eq. ccall) ioutiw=iw
      
cx      write(nlog,*) ' Replace; iout, ioutiw, ccall, l2, crigid(l2)', 
cx     1                         iout, ioutiw, ccall, l2, crigid(l2)
cx      
cx      if(iout.gt.0 .and. ioutiw.eq.iw) then
cx        write(nlog,*) ' '
cx        write(nlog,*)' ___________________________________________'
cx        write(nlog,*) ' Replace; ccall, crigid(l2) = ', ccall, iw
cx      endif  
c
c		iout=0 no detials
c		     1 details
c		     2 summary
c		     3 partial summary
c		    99 print for a specific ditch cDest1
c             
cx      iout=3
cx      ioutiw=0

      small = 0.001
      small2=0.01
      
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif
c
c		Initilze first time replace is called per month
      iprint=0
      if(nrepcall.eq.0) then
        divtot=0.0
        reltot=0.0
      endif
      
c     write(nlog,*) ' Replace in; divactx', divactX*fac
      
c
c _________________________________________________________
c		Step 2; Set replacement info. Note
c			n1 = replacement reseroivr 			
c			lr = replacement reservoir operating rule
c			l2 = destination diversion water right
c			nd = destination diversion 
      do 100 n = 1,irepn
c       write(nlog,*) ' Replace; n, irepn = ', n,  irepn
        crepTyp='NA      '
        iwhy=0
        cwhy='NA'
        
        n1 = ireprnk(n)
        
        lr=irepk(n1)
        nd=idivco(1,l2)
c       cDest=crigid(l2)
        cDest=cdivid(nd)
        divactx=0.0
c        
c       write(nlog,*) '  Replace  n, n1', n, n1
c
c ---------------------------------------------------------	
c		Set Shortage Switch (for reoperation)
        ishort=-1        
c
c ---------------------------------------------------------	
c		Set type 1=direct, 2= Exchange       
        if(irepexp(n1,nd).eq.0) then
          irepT=1
        else
          irepT=2
        endif
c

c _________________________________________________________
c		Step 4; Skip if the Rule is off or not active
        IF(ioprsw(lr).eq.0) then        
          iwhy=1
          cwhy='Operating rule is turned off'
          goto 98
        endif  
        
        ioff=0
c
c ---------------------------------------------------------	
c		Rule is inactive
        if(iyrmo(12).lt.ioBeg(lr)) ioff=1
        if(iyrmo(12).gt.ioEnd(lr)) ioff=1
          
        if(ioff.eq.1) then
          iwhy=2
          cwhy='Operating rule is off this year'
          goto 98
        endif
c _________________________________________________________
c		Step 5; Set up data for this right
c					
        iopdes(1,lr) = idivco(1,l2)
        iopdes(2,lr) = idivco(2,l2)
        
        iopsou(1,lr) = irepnr(n1)
        iopsou(2,lr) = irepown(n1)

        iExPoint(lr) = iRepExp(n1,nd)
        iopsou(4,lr) = ireptyp(nd)        
c
c rrb 2007/10/26; Add destination type (iopdesr) and max release
c		  operating rule (irepMax)         
        iopdesr(lr)  = 3
        iopsou(5,lr) = irepMax(n1)       
        iOprlim(lr) =  iRepLim(n1)
c _________________________________________________________
c               Step 6; Set water right decree and amount 
c                 diverted to this point
        tranlim = -1.0
        divdx   = divd(l2)                                  
        dcrdivx = dcrdiv(l2)
c
c _________________________________________________________
c               Step 7; Set source data
c			nr = reservoir
c			cSour = Reservoir ID
        nr=iopsou(1,lr)
        cSour=cresid(nr)
c
c _________________________________________________________
c               Step X; Detailed printout
c       if(iout.eq.1) then
cx      if(iout.eq.3 .and. cDest.eq.cDest1 .and. mon.eq.monout) then    
        if(iout.eq.3 .and. mon.eq.monout) then            
          write(nlog,122) cDest, cDest1, cSour,
     1    iyrmo(mon), xmonam(mon), n, n1, irepn, lr, nd, ioprsw(lr), 
     1    ioBeg(lr), ioEnd(lr), ishort, 18, divo(18)*fac
        endif
        
c
c _________________________________________________________
c               Step 8; Detailed Output
c		Warning BIG OUTPUT        
        if(iout.eq.1 .and. cDest.eq.cDest1.and. mon.eq.monout) then      
         iprint=iprint+1
         if(iprint.eq.1) write(nlog,124)
          if(iprint.eq.1 .or.iout.eq.99) write(nlog,124)
          
          write(nlog,125) 
     1      iprint, cDest, cSour, iyrmo(mon), xmonam(mon),
     1      iwx, n, n1, lr, nd, nr, 
     1      irepn, ishort, dcrdivx*fac, divdx*fac
        endif
c
c _________________________________________________________
c		Step 9 
c rrb 02/05/28; Exit if no remaining water right exists 
        if(dcrdivx-divdx.le.small) then
          iwhy=3
          cwhy='Remaining water right is zero'
          goto 98
        endif  
c
c _________________________________________________________
c               Step 10 Reservoir to diversion 
c
c ---------------------------------------------------------
c		a. Release Direct
        if(irepexp(n1,nd).eq.0) then
          creptyp = 'Direct  '
c
c		Note divactx is the amount released
c		     divacty is the amount diverted          
c ---------------------------------------------------------
c rrb 2007/10/26; Add ability to limit to a plan
c          call divres(iw,lr,ishort,n,tranlim,dcrdivx,divdx,
c    1       divactx,divacty,ncall(2))

           call divresP2(iw,lr,ishort,n,tranlim,dcrdivx,divdx,
     1       divactx,divacty,ncall(2))
     
cx         if(iout.eq.3 .and. cDest.eq.cDest1 .and.mon.eq.monout) then
           if(iout.eq.3 .and. mon.eq.monout)  then
cx            .and. divactx.gt.small) then        
             write(nlog,*) ' Replace; Direct Release for cdest, Res # ',
     1       ' ishort ', cdest, n, ishort, divactx*fac, divacty*fac,
     1       18, divo(18)*fac
           endif
c
c rrb 2015/09/06; Correction          
cx        if(divacty*fac.gt.0.001) then
          if(divactX.gt.small) then          
            reltot=reltot+divactx*fac
            divtot=divtot+divacty*fac
            divd(l2) = divd(l2)+divacty

            relpct=0.0                              
            if (divacty.gt.small) relpct=divactx/divacty*100.0
          else
            iwhy=4
            cwhy='Direct Diversion is zero'            
          endif
        else
c
c ---------------------------------------------------------
c		b. Release by Exchange
c               Reservoir to diversion - Exchange
          divo1=divo(lr)
c ---------------------------------------------------------
c rrb 2007/10/26; Add ability to limit to a plan
c        call divrpl(iw,lr,ishort,n,tranlim,dcrdivx,divdx,
c    1      divactx,divacty,ncall(4))
c
cx        if(iout.ge.1 .and. iw.eq.ioutiw) then      
cx          write(nlog,*) ' Replace; Calling DivrplP for ', ccall
cx        endif  

          call divrplP(iw,lr,ishort,n,tranlim,dcrdivx,divdx,
     1      divactx,divacty,ncall(4))
          creptyp = 'Exchange'
          cDest=cdivid(nd)
          
cx        if(iout.eq.3 .and. cDest.eq.cDest1 .and. mon.eq.monout) then
          if(iout.eq.3 .and. mon.eq.monout) then
cx           .and. divactx.gt.small) then           
            write(nlog,*) ' Replace; Exchange for cdest, Res # ', 
     1      cdest, n, divactx*fac, divacty*fac, 18, divo(18)*fac
          endif
c
c		Note divactx is the amount released
c		     divacty is the amount diverted  
c
c rrb 2015/09/06; Correction                  
cx        if(divacty*fac.gt.0.001) then
          if(divactX.gt.small) then           
            reltot=reltot+divactx*fac
            divtot=divtot+divacty*fac
            divd(l2) = divd(l2)+divacty
            relpct=0.0                              
            if (divacty.gt.small) relpct=divactx/divacty*100.0
          else
            iwhy=5
            cwhy='Exchange Diversion is zero'
          endif
        endif   

c                          
c _________________________________________________________
c               Step 11; Check diversion against decree
        if((dcrdiv(l2)-divd(l2)).le.-0.001) then
          write(nlog,*) ' Replace; Problem diversion > decree ',
     1      cSour, cDest, crepTyp, l2, dcrdiv(l2), divd(l2), 
     1      dcrdiv(l2)-divd(l2)
        endif
        
c      
c _________________________________________________________
c               Step 12; Detailed printout
c       if(iout.eq.1) then
        if(iout.eq.1 .and. cDest.eq.cDest1 .and. mon.eq.monout) then      
        
          write(nlog,*) '  Replace; ', cSour, cDest
          write(nlog,*) '  Replace,   n1, l2, lr, nd, ',
     1     '     irepexp(n1,nd) ishort'
          write(nlog,'(10x,20i5)')    n1, l2, lr, nd, irepexp(n1,nd),
     1                  ishort
          write(nlog,*) ' '
          write(nlog,130) 
          write(nlog,'(10x, 20i14)')  iopdes(1,lr), iopdes(2,lr), 
     1                              iopsou(1,lr), iopsou(2,lr), 
     1                              iExPoint(lr), iopsou(4,lr)
        endif
c
c _________________________________________________________
c		Step 13; Continue for quick exits for this replacement
c                        reservoir
  98    continue
c
c _________________________________________________________
c		Step 14; Detailed output to Detailed Replacement
c		         Reservoir Output (File 51) 
      if(divactX.gt.small) then  
          if(ncall(10).eq.0) then
            write(51,282) 
            if(iout.eq.2 .or. iout.eq.99) write(nlog,280)
            ncall(10)=ncall(10)+1
          endif  
          
          write(51,272) iyrmo(mon), xmonam(mon), iwx, nrepcall+1, 
     1      corid(lr),
     1      creptyp,  cresid(nr), resnam1(nr),
     1      cdivid(nd), divnam1(nd),
     1      divactx*fac, reltot, divacty*fac, divtot, 
     1      (divacty-divactx)*fac, relpct, 
     1      divo(lr)*fac, ishort
        endif
c
c _________________________________________________________
c		Step 15; Detailed output to Log File
c
c     if(iout.eq.2 .or. iout.eq.99) then      
      if((iout.ge.1 .and. iw.eq.ioutiw) .or. iout.eq.99) then      
cx      if(iout.eq.1 .and. cDest.eq.cDest1 .and. mon.eq.monout) then      
        
        if(divactX.le.small) then
          if(ncall(10).eq.0) write(nlog,280)
        else
          if(ncall(10).eq.1) write(nlog,280)
        endif
        
cxx         if(divactX.gt.small) then                 
            write(nlog,270) iyrmo(mon), xmonam(mon), iwx, nrepcall+1, 
     1        corid(lr),
     1        creptyp,   cresid(nr), resnam1(nr),
     1        cdivid(nd), divnam1(nd),
     1        divactx*fac, reltot, l2, lr, ishort,
     1        divacty*fac, divtot,
     1        (divacty-divactx)*fac, relpct,
     1        divo(lr)*fac, divactx*fac, iwhy, cwhy
     
cxx         endif

        endif
        
c
c _________________________________________________________
c rrb 2006/08/19
c               Step 16; Check reservoir roundoff. Note:
c			in1=0 into a routine, 1 out of a routine
c			isub1 = subroutine calling chekres
cx      if(iresw.eq.1) then
cx      nr=nd2
        in1=1
        isub1=10
        call chekres(nlog, maxres, in1, isub1, iyr, mon, nr,nowner,
     1                     curown,cursto,cresid)
cx      endif
        
c
c _________________________________________________________
c		Step 17; Exit if not short
c
        if(ishort.eq.0) then
cx        if(iout.eq.3 .and. cDest.eq.cDest1 .and. mon.eq.monout) then    
          if(iout.eq.3 .and. mon.eq.monout) then              
            write(nlog,*) '  Replace; ishort = ', Ishort,
     1       divactx*fac, divacty*fac, 18, divo(18)*fac
          endif
          goto 110
        endif  
c
c _________________________________________________________
c		Step 18; Continue for next Replacement Reservoir
        
  100   continue                
c            
c _________________________________________________________
c		Step 19; Return

  110   continue
c
c rrb 2008/03/13; Convergence limit  
        if(divactX.lt.small2) divactX=0.0
c       write(nlog,*) ' Replace out; divactx', divactX*fac
        return
c
c _________________________________________________________
c               Formats

  120   format(
     1      '  Replace;  iyr  mon    n   nr irow',
     1                ' curown ritrem tranlim',/
     1      '         ',6i5, 20f8.0)

  122   format(/, 72('_'),/
     1 '  Replace_1; cDest       cDest1       cSour      ',
     1 '     iyr     mon       n      n1   irepn      lr',
     1 '      nd  ioprsw   ioBeg   ioEnd',
     1 '  ishort      18 divo(18)',/
     1 '  Replace_1; ', a12,a12,a12, i8, 4x, a4, 10i8, 20f10.4)

  124   format(/,
     1 '  Replace_2;    # Dest ID      Source ID        iyr     mon',
     1 '     iwx       n      n1      lr',
     1 '      nd      nr    irep  ishort dcrdivx   divdx',/
     1 ' ___________ ____ ____________ ____________ _______ _______',
     1 ' _______ _______ _______ _______',
     1 ' _______ _______ _______ _______ _______ _______')
  125   format(
     1 '  Replace_2;', i5, 1x,a12, 1x,a12, i8,4x,a4,8i8, 20f8.0)

  130   format('  Replace,   iopdes(1,lr), iopdes(2,lr),',
     1                          ' iopsou(1,lr), iopsou(2,lr),',
     1                          ' iExPoint(lr), iopsou(4,lr)')

  270   format(i5,1x, a4,2i8, 1x,a12,1x, a8,1x, 
     1   2(a12,1x,a24,1x), f10.3, f10.1, 2i5, i8, 6f10.3, 
     1   i5, 1x, a48)
     
  272   format(i5,1x, a4,2i8, 1x,a12,1x, a8,1x, 
     1   2(a12,1x,a24,1x), 7f10.2, i10)
     
     
  280 format(/,72('_'),/,' Replace  '/
     1    ' Year Mon     Iter   Count Opr ID       Type     ',
     1    'Source ID    Source Name              ',
     1    'Destin. ID   Destin. Name             ',
     1    '   Release   Tot-Rel   l2   lr  ishort',
     1    '    Divert   Tot-Del    DepAdj      Rel%      Divo',
     1    '   DivactX',
     1    ' iwhy cwhy'/
     1    '  (1) (2)      (3)     (4) (5)          (6)      ',
     1    '(7)          (8)                      ',
     1    '(9)          (10)                     ',
     1    '      (11)      (12) (13) (14)    (15)      (16)',
     1    '      (17)      (18)      (19)      (20)      (21)'
     1    ' (22) (23)'/
     1    ' ____ ____ _______ _______ ____________ ________ ',
     1    '____________ ________________________ ',
     1    '____________ ________________________ ',
     1    '__________ _________ ____ ____ _______',
     1    ' _________ _________ _________ _________ _________',
     1    ' _________',
     1    ' ____ ________________________________________________')

  282 format('#',/,'#',72('_'),/,
     1    '#  Yr Mon     Iter   Count Opr ID       Type     ',
     1    'Source ID    Source Name              ',
     1    'Dest. ID     Dest. Name               ',
     1    '   Release   Tot-Rel    Divert   Tot-Div',
     1    '    DepAdj      Rel%      Divo    Ishort',/
     1    '# (1) (2)      (3)     (4) (5)          (6)      ',
     1    '(7)          (8)                      ',
     1    '(9)          (10)                     ',
     1    '      (11)      (12)      (13)      (14)',
     1    '      (15)      (16)      (17)      (18)',/
     1    '#____ ____ _______ _______ ____________ ________ ',
     1    '____________ ________________________ ',
     1    '____________ ________________________ ',
     1    '__________ _________ _________ _________',
     1    ' _________ _________ _________ _________')
c _________________________________________________________
c
c               Error warnings
cr510 write(nlog,*)
cr   1 '  ResRgP; Problem a Type 41 Operating Rule requires',/
cr   1 '          Out-of-Priority Plan data',/
cr   1 '          Reconmend you revise the operating rule (*.opr) file')
cr    goto 9999

 9999 write(6,1050) 
      write(99,1051) 
    
 1050 format('    Stopped in Replace',/,
     1       '    See the *.log file')
 1051 format('    Stopped in Replace')
      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)
c
c _________________________________________________________
c

      stop 
      END
