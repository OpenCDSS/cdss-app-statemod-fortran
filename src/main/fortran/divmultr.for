c divmultR - Type 52 operating rule.
c            It simulates multiple ownership.
c            Currently transfers water from a reservoir to 
c            to one or more reservoir accounts.
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
      SUBROUTINE DivMultR(IW,L2,ncallX)
c
c _________________________________________________________
c	Program Description
c
c  DivMultR; Type 52.
c		 It simulates multiple ownership
c		 Currently transfers water from a reservoir to 
c		 to one or more reservoir accounts.
c
c    Initially developed for the Trinidad project operation in
c    Colorado's Arkansas River Basin but may have generic
c    applications.
c
c		Source 1 is a Reservoir account
c		Destination(s) 1-n are reservoir accounts
c
c_____________________________________________________________
c
c       Update History
c 
c rrb 2021/08/15; Set source reservoir (nsr) near top of code
c                   to simplify in case this rule is not operated this
c                   time step or iteration (e.g. goto 260) and 
c                 Fix typo when calling chekres (nsr not nrs) and
c                 Revise go to 9999 to 300 for consistency to use
c                   goto 9999 when stopping in a subroutine
c
c rrb 2021/04/18; Compiler warning
c
c rrb 2018/07/29; Copied divmultR.for and replaced plans with 
c                 Reservoirs
c rrb 2018/11/27; Added capability for ropdes(l2,2) to be a
c                 percent or volume based on the variable
c                 cdivtyp(l2)
c rrb 2018/11/27; Added capability to limit the transfer to a
c                 maximum rate in cfs (ropdes(l2,1))
c		           
c _________________________________________________________
c
c       Documentation
c	
c       IW              Global water right ID
c       L2              LOC. OF operation right  in opr RIGHT TABLE
c       ipct            0 if allocation data is provided as %
c                       1 if allocation data is provided as AF
c       nsR             Source reservoir
c       ndR             Destination reservoir 
c       divact					Bookover * %
c       divactx         min(divact, account capacity)            
c
c_____________________________________________________________
c	      Dimensions
c	
      include 'common.inc'
      character cwhy*48, cdestyp*12, ccarry*3, cpuse*3, cstaid1*12,
     1          rec12*12, cTandC*3, cidRiv*12, cidBal*12,
     1          cresid1*12, subtypX*8
c
c
c _________________________________________________________
c
c       Step 1 Common Initialization
c
c rrb 2021/04/18; Compiler warning
      cidbal=' '
      cidriv=' '
      cresid1=' '
      rec12=' '
      pct=0.0
c
c
c ---------------------------------------------------------
c		      iout = 0 No details
c		             1 Details
c                2 Summary      
c		a. OutPut control
      subtypX='divmultR'
      
      iout=0
      ioutX=0
      ioutiw=0
      
      if(ichk.eq.152) iout=2
      if(corid(l2).eq. ccall) ioutiw=iw
c
c rrb 2018/04/08; Reduce output to *.log      
      if(ioutX.ge.1 .and. ncallx.eq.0) then
        write(nlog,102) corid(l2)
 102    format(/, 72('_'),/ '  divmultR; ID = ', a12)
      endif             
      
c
c ---------------------------------------------------------
c		b. Factor
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif
c
c ---------------------------------------------------------
c		c. Miscellaneous
      
      imcP=-1
      small = 0.001
      smalln=-1*small
      
      divact= 0.0
      divactT=0.0
      divalo = 0.0     
c
      ishort=0    
      
      ipct=-1
      if(cdivtyp(l2).eq.'Percent     ') ipct=0
      if(cdivtyp(l2).eq.'Volume      ') ipct=1 
      
cx    write(nlog,*) '  DivmultR; ', corid(l2), cdivtyp(l2)
      
      if(ipct.eq.-1) then
        write(nlog,290) corid(l2), cdivtyp(l2)
c
c rrb 2021/08/15; Revise to be consistent with use of goto 9999
cx      goto 9999
        goto 300
      endif                 
c
c ---------------------------------------------------------
c		f. Detailed Output
      iwhy=0
      cwhy='NA'
      cdestyp='NA'
      ccarry='No'
      cpuse='No'
      cTandC='No'
      cstaid1='NA'
c _________________________________________________________
c
c rrb 2021/08/15; Error Checking
c		 g. Set source reservoir here to simplify checks by
c       call chekres in case this rule is not operated
c       this time step or iteration (e.g. goto 260)
c
      nsR  =IOPSOU(1,L2)
      if(nsR.le.0) then
        write(io99,*) '  DivMultR; Problem reservoir ID (nsr) = ', nsr
        goto 9999
      endif  
c
c _________________________________________________________
c		    Step 2; Check for On/Off Switches      
c
c ---------------------------------------------------------
c		Monthly ON/Off switch

      if(imonsw(l2,mon).eq.0) then
        iwhy=1
        cwhy='Monthly switch Off'
        goto 260
      endif  
c
c ---------------------------------------------------------
c		Daily ON/Off switch start on day x
      if(iday.eq.1 .and. imonsw(l2,mon).gt.0) then
        if (idy.lt.imonsw(l2,mon)) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 260
        endif  
      endif  
c
c ---------------------------------------------------------
c		Daily On/Off Switch end on day x
      if(iday.eq.1 .and. imonsw(l2,mon).lt.0) then
        if (idy.gt.iabs(imonsw(l2,mon))) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 260
        endif  
      endif  
c
c ____________________________________________________
c       Step 3; Check reoperation control and exit
c               if already operated once this time step
c rrb 2018/07/29 
       icallOP(l2)=icallOP(l2) + 1
c
       if(icallOP(l2).gt.1) then
         iwhy=1
         cwhy='Limited to one call per time step'
         goto 260
       endif        
c _________________________________________________________
c
c		    Step 3; FIND Source data (a RESERVOIR)
c
      nsR  =IOPSOU(1,L2)

      IF(IRESSW(nsR).EQ.0) then
        iwhy=2
        cwhy='Source Reservoir is off'
        goto 260
      endif  
      
C
      IROWs=NOWNER(nsR)+IOPSOU(2,L2)-1
      ISCD=IRSSTA(nsR)
      cstaid1=cresid(nsR) 
      RESAVL=CUROWN(IROWs)        

c
c rrb 2015/06/25; Additional output control for debugging
      if(ioutX.eq.1) then
        write(nlog,*) ' '
        write(nlog,*) 'DivmultR_Step 3;     nsR    irows  RESAVL'
        write(nlog,'(a17, 2i8, 20f8.0)') 
     1    ' DivmultR_Step 3;     ', nsR, irows, RESAVL 
        write(nlog,*) ' '
      endif
c 
c
c ---------------------------------------------------------
c		b. Exit if supply is zero
      if(RESAVL.lt.small) then
        iwhy=3
        cwhy='Source reservoir storage = zero'
        goto 260
      endif
     
c _________________________________________________________
c
c		Step 4; FIND DESTINATION DATA (a RESERVOIR)
c
      cdestyp='Reservoir'
c
      ndes=int(oprlimit(l2))
      divactT=0.0
      n1=0
      n2=0
      
      do n=1,ndes
        n1=n2+1
        n2=n1+1
        
        ndR  =Iopdes(n1,L2)
        IROWd  =nowner(ndR)+iopdes(n2,l2)-1
c
c rrb 2018/11/27; Allow percent or volume to be specified.
        if(ioutX.eq.1) then
          write(nlog,*) 
     1     '  DivmultR;', n1, ropdes(l2,n1), n2, ropdes(l2,n2),
     1        fac, ropdes(l2,n1)*fac
        endif
c        
        if(ipct.eq.0) then
          pct=ropdes(l2,n2)/100.0
          divAF=resavl*Pct
        else
          divAF=amin1(resavl, ropdes(l2,n2))
        endif
c
c rrb 2018/11/27; Allow a maximum transfer rate to be specified.
        divAF1=divAF
        divAF=amin1(divAF, ropdes(l2,n1)*fac)
        
        divactT=divactT+divAF
c
c
c ---------------------------------------------------------
c             Update account storage    
        curown1=curown(irowd)  
        CUROWN(IROWs)=CUROWN(IROWs)-DIVAF
        CUROWN(IROWd)=CUROWN(IROWd)+DIVAF 
        divafx=divAF
c
c ---------------------------------------------------------
c             Check storage has not gone negative 
        if(curown(IROWs).lt.smalln) then
          write(nlog,250) curown(IROWs)
 250      format(
     1     ' DivmultR;', 
     1     'Problem source storage = ',f8.0 ' that is < zero')
c
c rrb 2021/08/15; Revise to be consistent with use of goto 9999
cx        goto 9999
          goto 300
        endif
c
c
c ---------------------------------------------------------        
c             Check storage has not gone above account capacity 
c             If it does; adjust transfer       
        if(curown(IROWd).gt.ownmax(IROWd)) then
          c = curown(IROWd) - ownmax(IROWd)
          curown(IROWs) = curown(IROWs) + c
          curown(IROWd) = curown(IROWd) - c 
          divactT = divactT - c
          divafx = divaf-c
c
c rrb 2021/04/18; Compiler warning
cx        write(nlog,252) curown(IROWd)+c, ownmax(IROWd)
cx 252      format(
cx     1     ' DivmultR;', 
cx     1     'Warning destination storage = ',f8.0,' is > capacity',f8.0,
cx     1     '          Excess storage is left at source')
cx        goto 9999
        endif    
c
c
c ---------------------------------------------------------
c             Update reservoir accounting variables
c               nsR nd = destination reservoir
c               ndR nr = source reservoir
c               qres(4          From carrier to storage by Other
c		            qres(22         From storage to carrier
c
        QRES(4,ndr)=QRES(4,ndr)+divafx
        QRES(22,nsr)=QRES(22,nsr)+divafx
        qres(29,ndr)=qres(29,ndr) + divafx 
        
        
        accr(4,irowD)  = accr(4,irowD)+divafx
        accr(22,irowS) = accr(22,irowS)+divafx 
       
c
c rrb 2015/06/25; Additional output for checking
        if(ioutX.eq.1) then
          write(nlog,*) ' '
          write(nlog,*) 
     1      'DivmultR_Step 4;',
     1      '   irows   irowd  resavl     pct  divaf curown1  curown',
     1      '  ownmax       c'
          write(nlog,'(a17, 2i8, 20f8.0)') 
     1      ' DivmultR_Step 4; ',irows,irowd,resavl, pct*100, divaf, 
     1        curown1, curown(irowd), ownmax(irowd), c
          write(nlog,*) ' '
        endif
        
        
c
c _________________________________________________________
c               Step 24; Set WWSP results
c rrb 2018/12/08; Allow allocation to accumulate to a WWSP Plan
c
        iwwsP=ioppln(n1,l2)
cx        write(nlog,*) '  DivmultR; ',
cx     1    'WWSP plan search for Opr rule = ', corid(l2),
cx     1    ' n1, l2, iwwsp = ', n1, l2, iwwsp        

        if(iwwsP.gt.0) then
          psuply(iwwsP)=psuply(iwwsP)+divafx/fac
          psuplyT(iwwsP)=psuplyT(iwwsP)+divafx/fac
          
          psto21=psto2(iwwsP)          
          psto2(iwwsP)=psto2(iwwsP)+divafx
          psto22=psto2(iwwsP)  
          
          if(ioutX.eq.1) then         
            write(nlog,*) '  DivmultR; ',
     1       'WWSP plan found for Opr rule = ', corid(l2),
     1       ' Plan ID = ', pid(iwwsP), '  divafx = ', divafx
          endif        
        endif      
c      
c _________________________________________________________
c               
c               Step X; Store data for detailed reporting
        divoWW(n,l2) = divoWW(n,l2) + divafx/fac   
        if(ioutX.eq.1) then
          write(nlog,*) ' DivMultiR; n, l2, divoww(n,l2)', 
     1                  n, l2, divoww(n,l2)*fac
        endif
      end do
c      
c _________________________________________________________
c               
c               b; Update operating rule output (DIVO)
      divo(l2)=divo(l2)+divactT/fac
      
c _________________________________________________________
c
c               Step 15.  Detailed output
c

 260  if(iout.ge.2 .and. iw.eq.ioutiw) then
c
c ---------------------------------------------------------
c		a. Header for this time step
c
        ncallX=ncallX+1
        if(ncallX.eq.1)then
          write(nlog,270) corid(l2),cdestyp, ccarry,cTandC
        else
c          write(nlog,*) ' '
        endif  
c
c ---------------------------------------------------------
c		b. Data for every time step and iteration
c
        write(nlog,280) '  divmultR  ',
     1    iyrmo(mon),xmonam(mon), idy,
     1     cstaid1, iwx, iw, nsP, ndes,
     1     RESAVL, (iopdes(n,l2), ropdes(l2,n), n=2,20,2),
     1     divactT, iwhy, cwhy
     
  280     FORMAT(a12, i5,1x,a4,i5, 1x,a12, 4i8,
     1     F8.1, 10(i8, f8.2), f8.1, i5,1x,a48)
     
      endif    
c
c _________________________________________________________
c
c               Step 16; Check Avail for Roundoff issues
c                        Note nsr is set prior to any goto 260
c                        quick exits (goto 260) for this time step 
c                        or iteration
c
c rrb 2021/08/15; Typo when calling chekres (nsr not nrs)
cx      call chekres(io99,maxres, 1, 52, iyr, mon, nrs,nowner,
cx    1               curown,cursto,cresid)
        call chekres(io99,maxres, 1, 52, iyr, mon, nsr,nowner,
     1               curown,cursto,cresid)    
c
c _________________________________________________________
c
c                Step 17; Return
      RETURN
c
c _________________________________________________________
c
c                Formats
c
  270   format(/, 
     1  '  divmultR (Type 52); Operation Right ID = ', a12,
     1  ' Destination Type = ', a12,
     1  ' Carrier (Y/N) = ',a3, ' Associated Plan (Y/N) = ', a3/    
     1  '  divmultR    iyr  mon  idy Source_ID   ',
     1  '   Iter#  Right#     nsP    ndes  RESAVL',
     1  ' iopdes2       % iopdes4       % iopdes6       %',
     1  ' iopdes8       % iopdes10      % iopdes12      %',
     1  ' iopdes14      % iopdes16      % iopdes18      %',
     1  ' iopdes20      % DIVACTT',
     1  ' iwhy cwhy',/
     1  '____________ ____ ____ ____ ____________',
     1  5(' _______'), 21(' _______'),' ____', 1x, 48('_'))
     
 290   format(/,    
     1  '  DivmultR (Type 52); Operation Right ID = ', a12,
     1    ' Diversion Type = ', a12,/     
     1 10x,'The diversion type should be Percent or Volume')
    
c
c rrb 2021/04/18; Compiler warning     
cx  320   format(/, '  divmultR; avail  ',/,(10f10.2))
cx  330   format(/, '  divmultR; river  ',/,(10f10.2))
     
c
c _________________________________________________________
c
c              Error warnings
c
c
c rrb 2021/08/15; Revise to be consistent with use of goto 9999
cx 9999 write(nlog,270) corid(l2),cdestyp, ccarry,cTandC
 300    write(nlog,270) corid(l2),cdestyp, ccarry,cTandC
c
c rrb 2018/11/27; Allow percent or volume to be specified
       if(ipct.eq.0) then
         write(nlog,280) '  divmultR   ',
     1    iyrmo(mon),xmonam(mon), idy,
     1     cstaid1,iwx, iw, nsP, ndes,
     1     RESAVL, (iopdes(n,l2), ropdes(l2,n+1), n=2,20,2),
     1     divactT, iwhy, cwhy
       else
         write(nlog,280) '  divmultR   ',
     1    iyrmo(mon),xmonam(mon), idy,
     1     cstaid1,iwx, iw, nsP, ndes,
     1     RESAVL, (iopdes(n,l2), ropdes(l2,n), n=2,20,2),
     1     divactT, iwhy, cwhy   
       endif 
c    
c
c rrb 2021/08/15; Revise to be consistent with use of goto 9999
cx    write(6,340)
 9999 write(6,340) 
      write(nlog,350) 
      call flush(6)
 340  format('    Stopped in divmultR',/,
     1       '    See the *.log file')
 350  format('    Stopped in divmultR')
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop 
      END

