c execut - the main subroutine that controls a StateMod simulation
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

      SUBROUTINE EXECUT
c_______________________________________________________________________
c       Program Description
c
c       Execut; Min subroutine that controls a StateMod simulation

c_____________________________________________________________
c
c       Update History
c
c rrb 2021/08/15; Runtime error initialization related to not saving
c                   local variables with Gfortran compilation moved
c                   from JMFlow to Execut (here)
c
c rrb 2021/08/10; Correction to fix a runtime error where ndlymx (# of delay
c                 elements) varies for daily and monthly model run and
c                 ndlymxX is the # for both a monthly & daily model
c
c rrb 2021/05/02; Runtime error tracking.  Pass itarx (type of 
c                 reservoir target file) to & from mdainp
c
c rrb 2021/04/18; Miscellaneous updates to compile without warnings
c
c rrb 2020/07/27; Revised to print execution time in sec, min & hours
c                   and adjust if the run went past midnight
c
c rrb 2020/07/07; Revise to include ability to exit at a  
c                 selected water right rank (icall) for Testing
c                 when Ichk = 210 and Ccall = the water right rank
c
c rrb 2018/08/20; Revise to include operating rule 
c                   type  54. JM Flow
c
c rrb 2018/08/20; Revise to include operating rule
c                   type  53. JMStorage
c
c rrb 2007/08/20; Revise to include operating rule
c                   type  52. Multiple Reservoir 
c
c rrb 2018/07/13; Revise to include operating rule 
c                   type  51. Flow-Reservoir Control
c
c rrb 2006/08/24; Revise to include operating rule
c                   type  50. South Platte Compact Storage
c
c rrb 2005/01/28; Revise to include operating rule
c                   type  49. Reservoir or Reuse Plan to a 
c                   T&C Plan exchange
c
c rrb 2005/01/28; Revise to include operating rule
c                   type  48. Reservoir or Reuse Plan to a
c                   T&C Plan  direct
c
c rrb 2007/08/20; Revise to include operating rule
c                   type  47. Release Limit
c
c rrb 2007/08/20; Revise to include operating rule
c                   type  46. Multiple Ownership
c
c rrb 2005/01/30; Revise to include operating rule
c                   type  45. Carrier with Loss
c
c rrb 2005/01/30; Revise to include operating rule
c                   type  44. Recharge Well to a Reservoir
c
c rrb 2005/01/30; Revise to include operating rule
c                   type  43. In-Priority Supply
c
c rrb 2005/01/30; Revise to include operating rule
c                   type  42. Plan Reset
c
c rrb 2006/08/24; Revise to include operating rule
c                   type  41. Reservoir Storage with Limits
c
c rrb 2006/08/24; Revise to include operating rule 
c                   type  40. South Platte Compact release
c                   to Compact
c_______________________________________________________________________
c       Documentation
c
c      iw       = water right loop counter
c      iwx      = total number of reoperations per time step
c      iwxt     = total number of reoperations per simulation
c      iwxo     = counter for output of reoperations per time step
c      iwxmax   = maximum reoperations that occurred in a given run
c      iwxlimit = maximum reoperations allows in a given time step
c
c      iyrmax   = year for maximum reoperation
c      monmax   = month for maximum reoperation
c      idymax   = day for maximum reoperation
c
c      ireop    = reoperation code (set in rtnsec.for)
c                   0=No; 1=Yes, reoperate because of return location 
c      iretsw   = reoperation code used by type 12 (set in divrig.for)
c                   0=No; 1=Yes, reoperate because of operation right
c                     and diversions have occurred in divrig.for
c      ireopx   = global control on reoperation 
c                   0=reoperate as needed by divactx
c                   1=reoperation only for type 12 operation rights
c                  -n=reoperate every -1*ireopx af
c      isp1     = the operating right pointer for the Splatte 
c                   operating rule set in Oprinp
c      isp2     = Number of times SPlatte is called per time step
c      ispK     = SPlatte compact operating rule
c                  set below when a S platte compact (type 40)
c                  is called

c
c       icallsp = switch that insures a type 21 Sprinkler Use is only
c                 called once per time step
c          ichk =  0 do not print detail;
c                  -n do at river id ichk,
c                  + print detailed information (datinp)
c                  1 Network and downstream data (datinp)
c                  4 Calls from Execut (execut)
c                  5 Demand & sprinkler (demand)
c                  6 Daily data (daydist)
c                  7 Return flow data (closs via mdainp or datest)
c                  8 Print detailed daily baseflow data to *.xtp
c                  9 or 109 Reoperation information from Execut
c                 10 Details on reading operating right data 
c
c      l1       = water rith type (1=ISF, 2=Res, 3=Opr, 4=Power
c                 5=well
c      l2       = pointer for a given water right (e.g. if l1 = 1
c                 and l2 = 10, then we are operating the 10'th ISF
c                 water right
c
c      nrg1     = counter # of calls to rgrg.f per time step 
c      nrg2     = counter # of calls to rgco.f per time step
c
c      divactx  = diversion or reservoir release that requires reoperation
c      divchk   = max change in divactx before reoperation (cfs
c      divchkr  = change associated with a non downstream return
c      divsum   = cumulation of divactx (af/time step)
c
c      nrepcall = # of calls to replace per time step
c      nrepcalt = # of calls to replace total
c
c
c   Downstream Call (type 23) control data
c        idcall = 0 no Type 23 right
c        idcall = k = pointer to the operating right that is a 
c                     downstream call
c       idcallx = switch that a type 23 right has been called in
c                 a time step
c dcall(ix,mon) = admin date for a downstream call this 
c                 time step. Note the admin date may change 
c                 every time step for a downstream call. 
c
c ---------------------------------------------------------
c   Downstream Call documentation
c        idcall = 0 No
c                 > 0 yes
c        dcall1 = downstream call admin number 
c                 set in bomsec or dayset
c        dcallx = ISF, res, or well call associated with 
c                 making a downstream call
c       idcallx = counter to insure 1 call per reoperation
c                 0 not called this reoperation
c                 1 called this reoperation
c
c_______________________________________________________________________
c     Dimensions
      include 'common.inc'
c
c rrb 2021/04/18; Miscellaneous updates to compile without warnings
cx    real*8 dcallx, cpri
      real*8 dcallx
c
c rrb 2021/04/18; Miscellaneous updates to compile without warnings
cx    character rec12*12, fileOpen*40, ctype*16, rec12b*12,
cx   1          nameX*24, cstaid1*12, rec2*2     
      character rec12*12, fileOpen*40, rec12b*12,
     1          nameX*24
      dimension idatx(3), itim1(4), itim2(4), ncall(200)
c    
c_______________________________________________________________________
c     Step 1; Initialize
c
c rrb 2006/08/21; Test      
cx      call namext(maxfn, filenc, 'xxx', filena)
cx      open(23,FILE=filena,STATUS='replace')   
cx      nlogx=23
      nlogx=nlog                                      
      write(6,*) ' '
      write(6,*) ' Subroutine Execut'
      write(nlog,10)
 10   format(/, 72('_'),/,'  Execut; Execut Option',//)
c
c     iout = 0 no details on reoperation
c     iout = 1 details on reoperation
c     iout=2 details on downstream call
c     ioutR= details on call replacement 
c     ioutSP=details on South Platte Compact
c     ioutSep=details on call sepsec (seepage)
c     ioutGVC = details on Grand Valley Check
c     ioutRep = details on replacement rule
c     ioutMin = control output to the screen 

      ! Assign logging variables to those from command line:
      ! - can set to hard-coded values after initialization if need to
      iout=log_IOUT
      ioutR=log_IOUTR
      ioutSP=log_IOUTSP
      ioutSep=log_IOUTSEP
      ioutGVC=log_IOUTGVC
      noutGVC=0
      ioutRep=log_IOUTREP
c
c rrb 2017/12/11; Control amount of output to
c                 screen for Gfortran that does not
c                 understand carriage control ('+')
c                 0 = print every iteration
c                 1 = print every month
      ioutmin=1
c jhb 2014/07/04 debugging
c      ichk = 94

      iwxt = 0
cx    iwxlimit=500
c     iwxlimit=2500
c
c rrb 2011/10/15; east slope linked application
cx    iwxlimit=5000
      iwxlimit=10000
      
      nrepcalt=0
c     maxoprin=28
c
c rrb 98/10/07
      monmax=0
      iwxmax=0
      iwxmaxY=0
      iyrmax=0
      idymax=0
      
      ioutc=log_IOUTC
      ioutcS=log_IOUTCS
      ioutcX=log_IOUTCX
      ipReop=0

      small = 0.001
      
c
      call dattim(idatx, itim1)
c
c TEST
c      goto 480      
      
c     write(nlogx,'(4i2)') (itim1(j),j=1,4)
c
c_______________________________________________________________________
c               Step 2; Open Files
c
c               Open scratch file
c     open(4,status='scratch')
c
c               Open file *.b42; Binary Wells
cx      call namext(maxfn, filenc, 'b42', filena) 
cx      open(42,file=filena,  status='replace',access='direct',recl=92)
c
c               Open file *.b43; Binary Direct Diversion
c               Note 40*4=160
      call namext(maxfn, filenc, 'b43', filena)
      open(43,file=filena,  status='replace',access='direct',recl=160)
c
c               Open file *.b44; Binary Reservoir
c               Note 40*4=160
      call namext(maxfn, filenc, 'b44', filena) 
      open(44,file=filena,  status='replace',access='direct',recl=160)
c
c               Open file *.b45; Binary Operating Rule information
      call namext(maxfn, filenc, 'b45', filena) 
      open(45,file=filena,  status='replace',access='direct',recl=4)
c
c               Open file *.b47; Binary Instream flow reach
      call namext(maxfn, filenc, 'b47', filena)
      open(47,file=filena,  status='replace',access='direct',recl=4)
c
c               Open file *.b49; Binary Daily Diversion
c               Note 40*4=160
cx      call namext(maxfn, filenc, 'b49', filena) 
cx      open(49,file=filena,  status='replace',access='direct',recl=160)

c
c               Open file *.b50; Binary Daily Reservoir
cx      call namext(maxfn, filenc, 'b50', filena)
cx      open(50,file=filena,  status='replace',access='direct',recl=160)
c
c               Open file *.b65; Binary Daily Wells
cx      call namext(maxfn, filenc, 'b65', filena) 
cx      open(65,file=filena,  status='replace',access='direct',recl=92)
c
c rrb 99/12/16; Rio Grande Compact (*.xrg)
c               Open file *.b66; Binary Rio Grande Compact
cx      call namext(maxfn, filenc, 'b66', filena) 
cx      open(66,file=filena,  status='Replace',access='direct',recl=96) 
c
c rrb 01/01/20; Structure Summary (*.b67) (33*4=128)
      call namext(maxfn, filenc, 'b67', filena) 
      open(67,file=filena,status='replace',access='direct',recl=132)

      call namext(maxfn, filenc, 'xdd', filena) 
      open(33,FILE=filena,STATUS='replace')
 
      call namext(maxfn, filenc, 'xre', filena) 
      open(34,FILE=filena,STATUS='replace')
      
cx      call namext(maxfn, filenc, 'xwe', filena)
cx      open(41,FILE=filena,STATUS='replace')

      call namext(maxfn, filenc, 'xop', filena) 
      open(46,FILE=filena,STATUS='replace')
 
      call namext(maxfn, filenc, 'xir', filena) 
      open(48,FILE=filena,STATUS='replace')

      call namext(maxfn, filenc, 'xca', filena)
      open(53,FILE=filena,STATUS='replace')  
       
c
c rrb; 01/01/20; Structure Summary
      call namext(maxfn, filenc, 'xss', filena)
      open(40,FILE=filena,STATUS='Replace') 
      
c
c       Replacement Reservoir File
cx      call namext(maxfn, filenc, 'xrp', filena)
cx      open(51,FILE=filena,STATUS='replace')   
c
c               Open daily output files 
cr      call namext(maxfn, filenc, 'xdy', filena) 
cr      open(35,FILE=filena,STATUS='replace')

cr      call namext(maxfn, filenc, 'xry', filena) 
cr      open(36,FILE=filena,STATUS='replace')

cr      call namext(maxfn, filenc, 'xwy', filena)
cr      open(37,FILE=filena,STATUS='replace')
c
c rrb; 99/12/16; Rio Grande Compact
cx      call namext(maxfn, filenc, 'xrg', filena) 
cx      open(52,FILE=filena,STATUS='Replace')
c
c               Open file *.b78; Binary Return File
c     call namext(maxfn, filenc, 'b78', filena) 
c     open(78,file=filena,  status='replace',access='direct',recl=8) 
c
c rrb; 05/01/07; Plan report
cx      call namext(maxfn, filenc, 'xpl', filena) 
cx      open(21,FILE=filena,STATUS='Unknown')
      
cx      call namext(maxfn, filenc, 'b68', filena)       
cx      open(68,file=filena,  status='replace',access='direct',recl=180)       
c
C-------------------------------------------------------------------
C
C------  READ IN RIVER SYSTEM DATA AND WATER RIGHT DATA
C
C-------------------------------------------------------------------
C
      call namext(maxfn, filenc, 'rsp', filena)
      fileOpen='Response File (*.rsp)' 
      write(nlog,101) fileOpen, filena      
      
      open(20,file=filena,status='old',err=9997)
      call skipn(20)

      IIN=20
      numstax=maxsta
c
c_______________________________________________________________________
c     Step 3; Read control & station data
c
      CALL DATINP(IIN,0,numstax)
      if(ichk.eq.94) write(nlogx,*) ' Execut; Out of Datinp'
      
      call outtop(53,1,51)
c
c_______________________________________________________________________
c rrb 2020/07/07; Set ability to stop at a selected water right
c                 rank (ichk - 1000000 and print detailed results 
c                 before this occurs by setting ichk=4
      ichkS=0
      if(ichk.gt.100000) then
        ichkS=ichk
        ichk=4
        write(nlogx,*)'  Execut; Stopped in Execut for Testing'
        write(nlogx,*)'  Execut;   ichkS, ichk' 
        write(nlogx,*)'  Execut;', ichkS, ichk           
      endif

c
c TEST
c     goto 480   
c     write(nlog,*) ' Execut_0; icall =', icall   
      
c
c_______________________________________________________________________
c     Step 4; Set factors Monthly (iday=0) or Daily (iday=1)
      if(ichk.eq.94) write(nlogx,*) ' Execut; Setting factors'      
      f = factor*31
      if(iday.eq.0) then
        imd=1
        fac=f
      else
        imd=mthday(1)
        fac=factor
      endif
c
c_______________________________________________________________________
c rrb 96/06/06; Write header info to binary files
c          Note  43=monthlydiversion, 
c                44=monthly reservoir
c                67=structure summary
c
      if(ichk.eq.94) write(nlogx,*) ' Execut; Calling bintop'      
      iystr0=iystr
      iyend0=iyend

      call bintop(43,0,nlog,ichk)
      call bintop(44,0,nlog,ichk)
      call bintop(67,0,nlog,ichk)
c
c rrb 2011/08/04; Add header to top of binary ISF reach
c                 No the record length is only 4
c      call bintop(47,0,nlog,ichk)
      
      if(ichk.eq.94) write(nlogx,*) ' Execut; Back from bintop'      
c
c_______________________________________________________________________
c               Open daily output files if appropriate 
      if(iday.eq.1) then
        call namext(maxfn, filenc, 'xdy', filena) 
        open(35,FILE=filena,STATUS='replace')
        if(ichk.eq.94) write(nlogx,*) ' Execut; Opened ',filena      

        call namext(maxfn, filenc, 'xry', filena) 
        open(36,FILE=filena,STATUS='replace')
        if(ichk.eq.94) write(nlogx,*) ' Execut; Opened ',filena       
c
c               Open file *.b49; Binary Daily Diversion
c               Note 40*4=160
        call namext(maxfn, filenc, 'b49', filena) 
        open(49,file=filena,status='replace',access='direct',recl=160)
        call bintop(49,0,nlog,ichk)
c
c               Open file *.b50; Binary Daily Reservoir
        call namext(maxfn, filenc, 'b50', filena)
        open(50,file=filena,status='replace',access='direct',recl=160)
        call bintop(50,0,nlog,ichk)

      endif
      
      if(ichk.eq.94) write(nlogx,*) ' Execut; Back from daily namext'
      
c
c_______________________________________________________________________
c               Open well output files if appropriate 
      if(iwell.gt.0) then
c
c               Open file *.xwe; Well Output
        call namext(maxfn, filenc, 'xwe', filena)
        open(41,FILE=filena,STATUS='replace')
c
c               Open file *.b42; Binary Wells
        call namext(maxfn, filenc, 'b42', filena) 
        open(42,file=filena,  status='replace',access='direct',recl=92)
        call bintop(42,0,nlog,ichk)
        
c
c               Open file *.b65; Binary Daily Wells
        if(iday.eq.1) then
          call namext(maxfn, filenc, 'xwy', filena)
          open(37,FILE=filena,STATUS='replace')
          
          call namext(maxfn, filenc, 'b65', filena) 
          open(65,file=filena,status='replace',access='direct',recl=92)
          call bintop(65,0,nlog,ichk)
        endif          
      endif
c
c_______________________________________________________________________
c               Open plan output files if appropriate 
      if(nplan.gt.0) then
c rrb; 05/01/07; Plan report
        call namext(maxfn, filenc, 'xpl', filena) 
        open(21,FILE=filena,STATUS='Unknown')
      
        call namext(maxfn, filenc, 'b68', filena)       
        open(68,file=filena,status='replace',access='direct',recl=180)
      endif
       
c
c_______________________________________________________________________
c
c rrb 2018/09/09; Open WWSP report (29)
      if(nplan.gt.0) then
        call namext(maxfn, filenc, 'xww', filena) 
        open(29,FILE=filena,STATUS='Unknown')   
      endif
c
c_______________________________________________________________________
c
c rrb 2018/09/09; Open WWSP report (29)
        write(nlog,*) '  Execut; ijm ', ijm
cx      if(imj.gt.0) then
        call namext(maxfn, filenc, 'xjm', filena) 
        open(30,FILE=filena,STATUS='Unknown')   
cx      endif
      
c
c_______________________________________________________________________
c rrb 2018/10/07; Open binary WWSP file for WWSP and JMartin opr rule 
c                   data
c                 Note 41*4 bytes = 164
      call namext(maxfn, filenc, 'b39', filena)       
      open(39,file=filena,status='replace',access='direct',recl=164)
c
c_______________________________________________________________________
c               Open San Juan Files if appropriate 
      if(isjrip.ge.1) then                          
      endif                                         
c
c_______________________________________________________________________
c               Set irymo based on starting year type
      if(ichk.eq.94) write(nlogx,*) ' Execut; Calling year'      
      call year(iystr, iyrmo, imomo, cyr1)
c
c rrb 03/01/16; Open temporary file (96) for selected debug options
cx    if(ichk.eq.8) then
      if(ichk.eq.6 .or. ichk.eq.8) then
        call namext(maxfn, filenc, 'xtp', filena) 
        fileOpen='Temporary File (*.xtp)' 
        write(nlogx,101) fileOpen, filena        
        open(96,file=filena, status='unknown', err=9997)
      endif
c
c_______________________________________________________________________
c rrb 96/06/06; Write header info to binary files
cx      iystr0=iystr
cx      iyend0=iyend
cxc
cx      if(ichk.eq.94) write(nlogx,*) ' Execut; Calling bintop'      
cx
cx      call bintop(43,0,nlog,ichk)
cx      call bintop(44,0,nlog,ichk)
cx      call bintop(65,0,nlog,ichk)
cx      call bintop(67,0,nlog,ichk)
cx      
cxcx      if(ichk.eq.94) write(nlogx,*) ' Execut; Calling outtop'            
cxcx      call outtop(51,2,32)      
cxcx      call outtop(53,1,51)

c
c
c_______________________________________________________________________
c rrb 04/20/96; set data for variable reoperation capability
c rrb 05/06/29; Revise convergence check for daily model
c               Note:
c                 ireopx  0=reoperate as needed by divactx
c                         1=reoperation only for type 12 operation rights
c                        -n=reoperate every -1*ireopx af
c                 divchk is in cfs
c                 iday = 0 is Monthly, iday = 1 is daily
c                 0.01 cfs = 0.61 af/mo (0.01*3600*24*31/43560=0.61)

      if(ireopx.ge.0) then
        if(iday.eq.0) then 
          divchk=0.01
        else
          divchk=0.001
        endif  
      else
        divchk=-ireopx*43560./24./3600./31.0
      endif 
c
c rrb 2015-09-25; Control detailed output
      if(iout.eq.1) then      
        write(nlogx,*) '  Execut; divchk (af/time step) = ', 
     1   divchk, ireopx,iday
        
        write(nlog,92) divchk, divchk*factor*31.0
 92     format(/,72('_'),/,
     1   '  Execut; FYI Reoperation check = ',f10.3,' cfs',/
     1   '                                = ',f10.3,' af/mo')
      endif
c          
c_______________________________________________________________________
c               Step 5; Print call data header if requested
c
c     write(nlog,*) ' Execut_1; icall =', icall   
      if(icall.gt.0) then
        if(ichk.eq.94) write(nlogx,*) ' Execut; Calling CallDat'
        call calldat(-1,  0,  0, icallx, ishort, fac)
      endif  
c
c rrb Test                
cr                write(nlogx,*) 'Execut; 8 qdiv(14,17)', qdiv(14,17)
      
c          
c_______________________________________________________________________
c               Step 6; Read all water rights
c
      maxwrx=maxwr
      if(ichk.eq.94) write(nlogx,*) ' Execut; Calling riginp'  
c
c rrb 2009/06/09; Correction                
cx    CALL RIGINP(IIN,maxwrx)
      maxres1=maxres
      maxdvr1=maxdvr
      call riginp(iin, maxres1, maxdvr1)
c      
      if(ichk.eq.94) write(nlogx,*) ' Execut; Out of riginp'
c          
c_______________________________________________________________________
c               Step 7; Open Rio Grande Output Files
c rrb 99/12/16; Rio Grande Compact
      if(irg1+irg2.ge.1) then
c
        call namext(maxfn, filenc, 'xrg', filena) 
        open(52,FILE=filena,STATUS='Replace')
c        
c               Open file *.b66; Binary Rio Grande Compact
        call namext(maxfn, filenc, 'b66', filena) 
        open(66,file=filena,status='Replace',access='direct',recl=96)
      endif  
c          
c_______________________________________________________________________
c               Step 8; Open Replacement Reservoir Output Files
c
      if(irepn.ge.1) then   
        call namext(maxfn, filenc, 'xrp', filena)
        open(51,FILE=filena,STATUS='replace')   
        call outtop(51,2,32)              
      endif  
c          
c_______________________________________________________________________
c               Step 9; Sort Water Rights

      maxnwrx=maxnwr
      if(ichk.eq.94) write(nlogx,*) ' Execut; Calling rigsor'      
      CALL RIGSOR(maxnwrx)
      if(ichk.eq.94) write(nlogx,*) ' Execut; Out of rigsor'
c
c ____________________________________________________
c
c               Step 10; Call SetPlanO to tie plans to operating rules
 
      call SetPlanO      
c          
c_______________________________________________________________________
c               Step 11; Get Plan Well Data
c                        Note call after Riginp because
c                        well rights must be known

      if(ichk.eq.94) write(nlogx,*) ' Execut; Calling GetPlnW'
      ifn=63
      rec256=fileName(ifn)
      filena=rec256(1:72)
      
      if(filena(1:2).eq.'-1') then
        nplanw=0
      else
        call GetPlnW(maxdvrW)
        if(ichk.eq.94) write(nlogx,*) ' Execut; Out of GetPlnW'
      endif  
c          
c_______________________________________________________________________
c               Step 12; Get Plan Reservoir Data
c                       Note call after Datinp because
c                       resrvoir stations & plan stations must be known

      if(ichk.eq.94) write(nlogx,*) ' Execut; Calling GetPlnR'
      ifn=79
      rec256=fileName(ifn)
      filena=rec256(1:72)
      
      if(filena(1:2).eq.'-1') then
        nplanw=0
      else
        call GetPlnR
        if(ichk.eq.94) write(nlogx,*) ' Execut; Out of GetPlnR'
      endif 
c
c
c          
c_______________________________________________________________________
c               Step 13; Check demands

c rrb 01/05/95; I/O Addition
      call demcons(0)  
c
c TEST
c     goto 480      
      
c          
c_______________________________________________________________________
c               Step 14; Find the starting point in all monthly files
C
      I12=0
C
      if(ichk.eq.94) write(nlogx,*) ' Execut; Calling Mdainp; i12=0'
c
c rrb 2021/05/02; Runtime error tracking - Save issue
cx      CALL MDAINP(IIN,I12)
        itarx=0
        iter=0
        CALL MDAINP(IIN,I12,itarx,iter)
      if(ichk.eq.94) write(nlogx,*) ' Execut; Out of Mdainp i12=0'
c
c TEST 
c     goto 480      
      
c
c rrb 0/03/28; Initialize binary return file
c     irec1=0
c     write(nlogx,*) '  Execut; numsta, ndlymx', numsta, ndlymx
c     do i=1,nstrtn
c       do j=1,ndlymx
c         irec1=irec1+1
c         write(78,rec=irec1) 0.0,0.0
c       end do
c     end do

c     write(6,*) ' Execut; out of mdainp'
c          
c_______________________________________________________________________
c     Step 15;  Open daily files
      if(iday.eq.1) then
c       Open *.out for daily
        if(infile.eq.1) then
c rrb 04/08/31
c         ifn=29
          ifn=35
          rec256=fileName(ifn)
          filena=rec256(1:72)
        else
          if(iwell.eq.0) then
            call skip(iin,5)
          else
            call skip(iin,6)
          endif
          read(iin,'(a72)',err=9996,end=9996) filena
        endif
c rrb 99/06/20
        call putpath(maxfn, filena, fpath1)
        fileOpen='Output Request File (*.out or *.xou)'
        write(nlogx,101) fileOpen, filena
c       write(6,101) fileOpen, filena
        open(22, file=filena,status='old',err=9997)
        ioutx=1
c rrb 99/08/20; Daily needs to know the year type       
        call year(iystr, iyrmo, imomo, cyr1)
        if(ichk.eq.94) write(nlogx,*) ' Execut; Calling Dayest'
          call dayest(iin,i12)
        if(ichk.eq.94) write(nlogx,*) ' Execut; Out of Dayest'
      endif
c_______________________________________________________________________
c     Begin year loop
      I12=12
      IYR=IYSTR-1
      IMO=0
c rrb 98/03/17; Daily capability
      ido=0
C-------------------------------------------------------------------
C-----YEAR LOOP
C-------------------------------------------------------------------
C------  STEP 1: THE DATA OF CURRENT YEAR IN ALL DATA FILES IS READ
C------          IN.
C------       2: DIVERSIONS, INSTREAM FLOW REQUIREMENTS, AND RESERVOIR
C------          WATER RIGHTS ARE PROCESSED AGAINST EACH MONTH OF
C------          RIVER FLOW. 
C------       3: THE STATUS OF WATER IN THE BASIN IS PRINTED OUT
C------          TO TEMPORARY FILES FOR EACH MONTH
C------       4: START AGAIN AT STEP 1
C------   WHEN ENDING YEAR OF THE SIMULATION IS COMPLETED, PRINT
C------   OUT FINAL REPORT USING DATA IN THE TEMPORARY FILES
C-------------------------------------------------------------------
      write(6,*) ' Subroutine Execut'
      write(6,*)
  130 IYR=IYR+1
      iwxmaxY=0
c_______________________________________________________________________
c     Set irymo based on year type
      call year(iyr, iyrmo, imomo, cyr1)
c_______________________________________________________________________
      IF(IYR.GT.IYEND) GOTO 480
c_______________________________________________________________________
      write(nlogx,131) iyr
 131  format(/, 72('_'),/'  Execut; Year = ', i5)
c_______________________________________________________________________
c     Call Mdainp for Time Series Data
      if(ichk.eq.94) write(nlogx,*) ' Execut; Calling Mdainp ', iyr
c
c rrb 2021/05/02; Runtime error tracking - Save issue
cx      CALL MDAINP(IIN,I12)
        CALL MDAINP(IIN,I12,itarx,iter)
cx
      if(ichk.eq.94) write(nlogx,*) ' Execut; Out of Mdainp ', iyr
c rrb 00/11/11; Set Sjrip (San Juan recovery implementation plan
c               variable that controls operation for 1x year stuff
      isjon=0
c rrb 01/05/95; I/O Addition
c_______________________________________________________________________
c     Call Demcons for constrained demand
      if(ichk.eq.94) 
     1  write(nlogx,*) ' Execut; Calling Demcons ', iyr
      call demcons(1)
      if(ichk.eq.94) 
     1  write(nlogx,*) ' Execut; Out of Demcons ', iyr
c_______________________________________________________________________
c     Step 16; Month Loop
c     write(6,*) ' '
      DO 1100 MON=1,12
c
c rrb 2021/05/30; Runtime Check       
c       if(ichk.eq.4 .and. mon.ge.3) then
c         write(nlog,*) '  Execut; Stopping in month 2'
c         stop
c       endif
c smalers 2021/08/05 used the following to debug Yampa daily model bug #64
c         When bug was fixed, it results in A LOT of output.
c       if(imo.ge.238) then
c         ichk = 4
c         !write(nlog,*) '  Execut; Stopping in month 2'
c         !stop
c       endif

c
c rrb 2019/07/21; Print to log beginning of every month
        if(ichk.ge.90) write(nlogx,540) iyrmo(mon), xmonam(mon)
cx      write(nlogx,540) iyrmo(mon), xmonam(mon)
        
        ioutc=log_IOUTC
        ioutcR=log_IOUTCR
        ioutcS=log_IOUTCS

cr      if(ioptio.eq.8) then
cr        write(6,106) iyrmo(mon), xmonam(mon)
cr      endif 
c
c rrb 2017/12/11; Control output  to the screen for Gfortran
        if(ioutmin.eq.1) then
          write(6,106) iyrmo(mon), xmonam(mon)
        endif
c_______________________________________________________________________
c       Step 17; Print call information
        nrepcall=0
        IMO=IMO+1
c smalers 2021-09-05 Add to help with troubleshooting
        if(ichk.eq.4) write(nlogx,*)' Execut; imo=',imo
c jhb 2014/07/23 IMO can't get any bigger than maxdlm (240)
c                because it is used in the first dimension of retur() and depl()
c                which are declared as retur(240,) and depl(240,)
c                but ndlymx is the number of days in the dld file time series
c                which can be much larger.
c                ran into errors in bomsec on a daily model
c                when the bounds check compiler flag is turned on.
c                Try changing this to maxdlm.
c jhb 2014/08/19 This change broke the return flow calculations in monthly models
c                Revert it back and find another way to solve the array bounds problem
c smalers 2021/08/05 Revert back to check on maxdlm
c       IF(IMO.GT.ndlymx) IMO=1
c
c --------------------------------------------------------------------
c rrb 2021/08/10; Correction to fix a runtime error where ndlymx (# of delay
c                 elements) varies for daily and monthly model runx and
c                 ndlymxX is the # for both a monthly & daily model
cx      IF(IMO.GT.maxdlm) IMO=1
        IF(IMO.GT.ndlymxX) IMO=1
c
c rrb 2021/08/15; Remove I/O to screen for every month
cx      write(nlogx,*)
cx   1  ' Execut; imo=',imo,' ndlymxX=',ndlymxX,' maxdlm=',maxdlm
c_______________________________________________________________________
c         Step 18; Set factors Monthly (iday=0) or Daily (iday=1)
          f= factor*mthday(mon)
          if(iday.eq.0) then
            imd=1
            fac=f
          else
            imd=mthday(mon)
            fac=factor
          endif
cx      write(nlogx,*) ' '
cx      write(nlogx,*) '  Execut; iyr, mon ', iyr, mon, fac
c_______________________________________________________________________
c       Step 19; Monthly initialization
        if(ichk.eq.94) write(nlogx,*)' Execut; Calling Bomsec',iyr,mon
        CALL BOMSEC(iflow)
c_______________________________________________________________________
c       Step 20; Daily initialization
        if(iday.eq.1) call dayest(iin,i12)
c_______________________________________________________________________
c       Step 21; Print call and reoperation headers
c         Print call information header
c       write(nlog,*) ' Execut_2; icall =', icall         
        if(icall.gt.0) call calldat(0, l1, l2, icallx, ishort, fac)
c rrb 97/10/16; Set switch to print call information at first reoperation only
        icallx=0
c_______________________________________________________________________
c       Step 22; Daily Loop
          do 1000 idy=1,imd
c           Print reoperation information header
          if(ioutR.ge.1) ioutR=1
          ipReop=0
          ido=ido+1

c
c --------------------------------------------------------------------
c rrb 2021/08/10; Correction to fix a runtime error where ndlymx (# of delay
c                 elements) varies for daily and monthly model run and
c                 ndlymxM2 is the # for both a monthly & daily model
c                 Note this update is for consistency but not required since
c                 ndlymx = ndlymxD for a daily model
cx        if(ido.gt.ndlymx) ido=1
          if(ido.gt.ndlymxD) ido=1
c
c         Step 23; Daily Initialization 1x/day
          if(iday.eq.1) then
cr          write(nlogx,*) ' Execut; Calling DaySet'  
            call dayset
cr          write(nlogx,*) ' Execut; Out of Dayset'
          endif  
          iw  = 0
          iwx = 1
          iwxo= 0
          iretsw = 0
          ireop=0
          divsum=0.0
          ireop12=0
c rrb 2006/09/18;
          divactX=0.0
c rrb 99/10/07; Rio Grande Compact stuff
          nrg1 = 0
          nrg2 = 0 
          l2rgrg=0
          l2rgco=0
c rrb 01/02/19; Set reoperation return flow check
          divchkr=0.0
          icallsp=0
          icallsm=0
          idcallx=0
c
c rrb 2014-11-24; Set icall26 to insure its called only once per time step
c rrb 2015/07/08; Add capability to not operate any more
c                 this time step by water right using
c                 icallOP(l2) not icall26  that controls by
c                 operating rule
cx        icall26=0
c rrb 05/05/12; Set call to subroutine counter
          do i=1,200
            ncall(i)=0
          enddo
c rrb 2006/11/20; Replacement counter every time step          
          nrepcall=0
c rrb 2011/07/11; Set Call OutIchk counter
          ichk4n=0
c rrb 2008/05/07; Set reservoir seepage indicator iseep) 
c                 and total seepage (seepT) every time step
cx        iseep=0
          seepT=0.0  
c  
c rrb 2011/01/02; Set S Platte compact controls
c         isp2 = Number of times SPlatte is called per time step
c         ispK = SPlatte compact operating rule
c                set below when a S Platte compact (type 40)
c                is called
          isp2=0
          ispK=0
c
c rrb 2019/07/29; ArkDSS Initialize flow control variable (iflow0
c                 iflow=0 Project Off and iflow=1 Project On
c rrb 2018/08/12; Correction; set in Bomsec, then carry forward
cx        iflow=0
c_______________________________________________________________________
c rrb     Exit if Testing only
c         if(ichk.eq.6) goto 339
          if(ioutGVC.eq.1) then 
            call outGVC(nlogx, 1, divact1, divsum, gvCot,
     1            noutGVC, fac,-1, 'NA          ', 
     1            -1, 'NA          ','NA                      ')
          endif
c_______________________________________________________________________
c         Step 24; Water Right Loop
 135      iw = iw+1
 
 
 
c_______________________________________________________________________
c rrb 2020/07/07; 
c rrb     Exit at a selected water right rank (icall) for Testing and
c         if not there print detailed data
c
          if(ichkS.gt.100000) then
            
            iwStop=ichkS-100000
            if(iw.ge.iwStop) then
              write(nlogx,*)'  Execut; Stopped in Execut for Testing'
              write(nlogx,*)'  Execut;   ichkS, ichk, iw, iwStop' 
              write(nlogx,*)'  Execut;', ichkS, ichk, iw, iwStop
              goto 9999
            endif
          endif
 


c_______________________________________________________________________
c         Save maximum reoperation by year
c           iwx      = total number of reoperations per time step
c           iwxo     = counter for output of reoperations per time step
c           Therefore if iwx.ne.iwxo means it wont print unless the 
c                     there is a new reoperation

          if(iwx.ne.iwxo) then
            iwxo=iwx
            iwxmax=max0(iwxmax, iwx)
            if(iwxmax.eq.iwx) then
              monmax=mon
              iyrmax=iyrmo(mon)
              idymax=idy
            endif
c           if(ioptio.ne.8) then
c
c rrb 2017/12/11; Control amount of output to screen for Gfortran
cx          write(6,103) iyrmo(mon), xmonam(mon), idy, iwx, iwxmaxY
            if(ioutMin.eq.0) then
              write(6,103) iyrmo(mon), xmonam(mon), idy, iwx, iwxmaxY
            endif
c           endif
          endif
c
c rrb 2018/03/19; Add printout of annual iterations.
c                 Note this is very slow so comment out
cx          if(ioutMin.eq.0) then
cx            write(6,104) iyrmo(mon), xmonam(mon), idy, iwx, iwxmaxY
cx          endif        
c_______________________________________________________________________
c rrb 2006/11/01; Initialized inside water right loop
          divchkR=0.0
          divactx = 0.0
          ireop=0
          GVCot=0.0  
          uDem=0.0
          divx=0.0
          ispOpr=0
          rec12b='NA          '
          L1=NWRORD(1,IW)  
          L2=NWRORD(2,IW)
c_______________________________________________________________________
          ityoprX=0
          if(l1.eq.5) ityoprX=ityopr(l2)
          if(ichk.eq.94) then
            write(nlog,*) ' Execut; iwx, iw, l1, l2, iretSW, ityopr',
     1        iwx, iw, l1,l2, iretSW, ityoprX, divsum, divactx, divx
          endif
c         if(l1.eq.0) then
c           write(nlog,*) ' Execut; problem l1 =0, l1, l2, iw',l1,l2,iw
c           stop
c         endif
c_______________________________________________________________________
c         Step 25; Check Avail for Roundoff

cx        write(nlogx,*) ' Execut; Calling RoundOf'
          if(ichk.eq.94) write(nlogx,*) ' Execut; Calling RoundOf'
          call roundof(avail, numsta, 2, 1, nbug)
cx          if(ichk.eq.94) write(nlogx,*) ' Execut; Out of RoundOf'
c         write(nlogx,*) 'Execut;', iyr,mon,iw,iwx,l1,l2
c         write(nlogx,*) 'Execut; 0, RRivpri', qres(1,1)*fac
c          if(ichk.eq.94) then
cx            write(nlogx,*) ' '
cx            write(nlogx,*) 'Execut;  iyr  mon   iw  iwx   l1   l2'
cx            write(nlogx,'(a8, 20i5)') 'Execut;', iyr,mon,iw,iwx,l1,l2            
cx          endif  
          if(nbug.eq.1) then
            write(nlogx,*) 'Execut; Problem ',iyr,mon,iw,iwx,l1,l2
            goto 9999
          endif
c_______________________________________________________________________
c         Step 26; INSTREAM FLOW RIGHT
          if(l1.eq.1) then
c rrb 99/03/22
c         Skip if the water right is not active for this year
c         iifrsw     =  1; on
c                    =  0; off
c                    = -n; simulate until year n (e.g. -1980)
c                    =  n; begin to simulate in year n (e.g. 1980)
            if(iifrsw(l2).ne.1) then
              if(iifrsw(l2).eq.0) goto 410
              if(iifrsw(l2).gt.0 .and. iyr-iifrsw(l2).lt.0) goto 410
              if(iifrsw(l2).lt.0 .and. iyr+iifrsw(l2).gt.0) goto 410
            endif
c_______________________________________________________________________
c           Test for a downstream call before an instream flow
c           Note dcall1 is set in bomsec or dayest
            if(idcall.ne.0) then
              dcallx=rfrnk(l2)
              if(rfrnk(l2).gt.dcall1 .and. idcallx.eq.0) then
                if(iout.eq.2) then
                  write(nlogx,188) 'Isf',IYRmo(mon), xmonam(mon), idy,
     1              dcall1, rfrnk(l2)                  
                endif
                idcallx=1
                iwtemp=iw             
                l2Temp = idcall
                ncloc=1
                call ifrdown(iwTemp,l2Temp,l1,fac,ncloc,dcallx)
c rrb               
cr              write(nlogx,*) 'Execut; 1 qdiv(14,17)', qdiv(14,17)
              endif
c             endif (rfrnk(l2).gt.dcall1 .and. idcallx.eq.0)
            endif
c           endif (idcall.ne.0)
c_______________________________________________________________________
c           Instream Flow Operation with or without reach option
            if(ireach.eq.0) then
              CALL IFRRIG(IW,L2,ncall(101))
            else
              call ifrrig2(iw,l2,ncall(101))
            endif
c_______________________________________________________________________
c rrb 2011/05/07; Detailed output for an ISF right       
            if(ichk.eq.94 .or. ichk.eq.4) then
c
c rrb 2021/03/20; Correction            
cx              call outIchk(
cx     1          1, ichk4n, l1, l2, iw, 0, ishort, fac,
cx     1          uDem, divactx, divx, divsum, 0, iwx, rec12b)         
              call outIchk(
     1          1, ichk4n, l1, l2, iw, 0, ishort, fac,
     1          uDem, divactx, divx, divsum, 0, -1.0, rec12b)  
            endif
c rrb Test                
cr          write(nlogx,*) 'Execut; 1 qdiv(14,17)', qdiv(14,17)
              GOTO 410
          endif
c         endif INSTREAM FLOW RIGHT (l1.eq.1)
c_______________________________________________________________________
c               Step 27; RESERVOIR RIGHTS
          if(l1.eq.2) then
c rrb 99/03/22
c            Skip if not active for this year
c              irsrsw     =  1; on
c                         =  0; off
c                         = -n; simulate until year n (e.g. -1980)
c                         =  n; begin to simulate in year n (e.g. 1980)
            if(irsrsw(l2).ne.1) then
c             write(nlogx,*) '  Execut;',
c    1          l2, iyr, irsrsw(l2), iyr-irsrsw(l2)
c             IF(irsrsw(L2).LE.0) GOTO 410
              if(irsrsw(l2).eq.0) goto 410
              if(irsrsw(l2).gt.0 .and. iyr-irsrsw(l2).lt.0) goto 410
              if(irsrsw(l2).lt.0 .and. iyr+irsrsw(l2).gt.0) goto 410
            endif
c_______________________________________________________________________
c           Test for a downstream call before a reservoir
c           Note dcall1 is set in bomsec or dayest
            if(idcall.ne.0) then
              dcallx=rrsnk(l2)
              if(rrsnk(l2).gt.dcall1 .and. idcallx.eq.0) then
                if(iout.eq.2) then
                  write(nlogx,188) 'Res',IYRmo(mon), xmonam(mon), idy,
     1              dcall1, rrsnk(l2)                  
                endif
                idcallx=1
                iwTemp=iw             
                l2Temp = idcall
                ncloc=2
                call ifrdown(iwTemp,l2Temp,l1,fac,ncloc,dcallx)
              endif
            endif    
c_______________________________________________________________________
            CALL RESRG1(IW,L2,divx,short,ncall(102))
c_______________________________________________________________________
c rrb 2011/05/07; Detailed output for a reservoir right        
            if(ichk.eq.94 .or. ichk.eq.4) then
              udem=short/fac
c
c rrb 2021/03/20; Correction            
cx              call outIchk(
cx     1          2, ichk4n,  l1, l2, iw, 0, ishort, fac,
cx     1          udem, divx, divx, divsum, 0, iwx, rec12b)
              call outIchk(
     1          2, ichk4n,  l1, l2, iw, 0, ishort, fac,
     1          udem, divx, divx, divsum, 0, -1.0, rec12b)
            endif 
c_______________________________________________________________________
            GOTO 410
          endif
c         endif RESERVOIR RIGHTS (l1.eq.2)           
c_______________________________________________________________________
c         Step 28; DIRECT DIVERSION RIGHTS
          if(l1.eq.3) then
c rrb 99/03/22
c           Skip if not active for this year
c             idvrsw     =  1; on
c                        =  0; off
c                        = -n; simulate until year n (e.g. -1980)
c                        =  n; begin to simulate in year n (e.g. 1980)
            if(idvrsw(l2).ne.1) then
              if(ichk.eq.94) write(nlogx,*)
     1          ' Execut; Check Diversion on/off'
c             write(nlogx,*) '  Execut;',
c    1          l2, iyr, idvrsw(l2), iyr-idvrsw(l2)
c             IF(IDVRSW(L2).LE.0) GOTO 410
              if(idvrsw(l2).eq.0) goto 410
              if(idvrsw(l2).gt.0 .and. iyr-idvrsw(l2).lt.0) goto 410
              if(idvrsw(l2).lt.0 .and. iyr+idvrsw(l2).gt.0) goto 410
            endif
c_______________________________________________________________________
c                     Test for a downstream call before a diversion
c                     Note dcall1 is set in bomsec or dayest
            if(idcall.ne.0) then
              if(ichk.eq.94) write(nlogx,*) 
     1          ' Execut; Check downstream call'
              dcallx=rdvnk(l2)
              if(rdvnk(l2).gt.dcall1 .and. idcallx.eq.0) then
                if(iout.eq.2) then
                  write(nlogx,188) 'Div',IYRmo(mon), xmonam(mon), idy,
     1              dcall1, rdvnk(l2)                  
                endif
                idcallx=1
                iwTemp=iw             
                l2Temp = idcall
                ncloc=3
                call ifrdown(iwTemp,l2Temp,l1,fac,ncloc,dcallx)
              endif
            endif    
c_______________________________________________________________________
c           Call Divrig to divert water
            if(ichk.eq.94) write(nlogx,*)
     1        ' Execut; Calling Divrig ', ireop, crigid(l2)
            CALL DIVRIG(IW,L2,ISHORT,divx,ncall(103))
            if(ichk.eq.94) write(nlogx,*)
     1        ' Execut; Back From Divrig', ireop
c_______________________________________________________________________
c rrb 2011/05/07; Detailed output for a diversion right       
            if(ichk.eq.94 .or. ichk.eq.4) then
c
c rrb 2021/03/20; Correction            
cx              call outIchk(
cx     1          3, ichk4n, l1, l2, iw, 0, ishort, fac,
cx     1          udem, divx, divx, divsum, 0, iwx, rec12b)
              call outIchk(
     1          3, ichk4n, l1, l2, iw, 0, ishort, fac,
     1          udem, divx, divx, divsum, 0, -1.0, rec12b)
            endif
c_______________________________________________________________________
c rrb 2011/01/02; South Platte Compact
c                 isp1 is the operating right pointer for the Splatte 
c                 operating rule set in Oprinp
            if(isp1.ge.1 .and. ishort.eq.1) then
              nd = idivco(1,l2)    
              IUSE=NDUSER(ND)+IDIVCO(2,L2)-1
              divreq1=divreq(iuse)
              if(ichk.eq.94) write(nlogx,*) ' Execut; Calling SPlatte'
              rec12=cdivid(nd)
c rrb 2011/05/04; Limit calls to diversion rights junior to the compact          
              if(rec12(1:2).ne.'64' .and. divreq1.gt.small .and.
     1          rdvnk(l2).gt.ropnk(isp1)) then        
cx              write(nlogx,*) ' Execut; Calling SPlatte ',nd, iuse, 
cx   1          divreq1*fac, l2, rdvnk(l2),  isp1, ropnk(isp1)           
                CALL SPlatte(IW,isp1,l2,ISHORT,nd,divactx,ncall(140))  
c_______________________________________________________________________
c rrb 2011/05/07; Detailed output for South Platte 
                if(ichk.eq.94 .or. ichk.eq.4) then 
                  divsum1=divsum+divactx*fac
c
c rrb 2021/03/20; Correction            
cx                  call outIchk(
cx     1              4, ichk4n, 5, isp1, iw, ityopr(isp1), ishort, fac,
cx     1              uDem, divactx, divx, divsum1, 0, iwx, rec12b)
                  call outIchk(
     1              4, ichk4n, 5, isp1, iw, ityopr(isp1), ishort, fac,
     1              uDem, divactx, divx, divsum1, 0, -1.0, rec12b)
                endif                
c rrb 2011/04/25; Allow reoperation
c rrb 2011/05/12; Correction
                if(divactx.gt.small) then
                  ispOpr=1
                  goto 400
                endif
                goto 410
              endif               
c_______________________________________________________________________
c             Print detailed Compact Data
              if(ioutSP.ge.1) then     
                isp2=isp2+1                  
                write(nlogx,590)
                write(nlogx,592) isp2, cdivid(nd), ciopdeX(1,isp1), 
     1            isp1, nd, iuse,ishort,
     1            divmon(nd)*fac, divreq(iuse)*fac  
              endif
            endif
c           endif South Platte Compact (isp1.ge.1 .and. ishort.eq.1)  
c_______________________________________________________________________
c rrb 02/26/96; Replacement reservoir logic (completely revised)
c           Type 10 operating rule
c           irepn  = number of replacement reservoirs
c           ishort = 0 water right is not shorted
c                  = 1 water right is shorted
c           ireptyp(nd) from datinp.for
c                     = 1 replace 100% (diversion)
c                     = -1 replace depletion
c                     = -999 does not get replacement 
c           reprnkx     = from getrep.for most senior replacement res Admin#
c           rdvnk       = water right Admin#
c           write(nlogx,*) ' '
c           write(nlogx,*) '  Execut; iyr, mon, l2, ishort',
c    1                     iyr,mon,l2,ishort
            nd = idivco(1,l2)
c           Print detailed Replacement Data
            if(ioutR.ge.1) then
              if(ioutR.ge.1) write(nlogx,560)
              write(nlogx,570) ioutR, cdivid(nd), l2,nd,irepn,
     1          ireptyp(nd),  ishort,rdvnk(l2),reprnkx
              ioutR=ioutR+1
            endif
c_______________________________________________________________________
c              Do not call replace if the structure is not short
            if(irepn.eq.0 .or. ishort.eq.0) goto 410
            nd = idivco(1,l2)
c_______________________________________________________________________
c rrb 04/31/96; Revised interpretation of ireptyp(nd)
c           ireptyp =  0 off
c                   =  1 100% replacement
c                   = -1 depletion replacement
            if(ireptyp(nd).ne.0 .and. rdvnk(l2).le.reprnkx) then
c
c rrb 2015/09/06; Test
            if(ioutRep.eq.1) then
              write(nlog,*) ' '
              write(nlog,*) '  Execut; before replace',divo(18)*fac
            endif
c            
            call replace(iw,l2,nrepcall,divactx, ncall)
c
c rrb 2015/09/06; Test
            if(ioutRep.eq.1) then
              write(nlog,*) '  Execut; after replace',divo(18)*fac
            endif 
c            
            nrepcall=nrepcall+1
            nrepcalt=nrepcalt+1
c           write(nlog,*) ' Called by replace ', l1, l2
            goto 400
          endif
        endif
c           endif for DIRECT DIVERSION RIGHTS (l1.eq.3) 
c_______________________________________________________________________
c         Step 29; OTHER WATER RIGHTS (POWER DEMAND, ETC.)
c                 NOT Active
          if(l1.eq.4) then
c rrb 99/03/22
c           Skip if not active for this year
c           ipowsw     =  1; on
c                      =  0; off
c                      = -n; simulate until year n (e.g. -1980)
c                      =  n; begin to simulate in year n (e.g. 1980)
c                      =  1; on for all practical purposes
            if(ipowsw(l2).ne.1) then
c             write(nlogx,*) '  Execut;',
c    1          l2, iyr, ipowsw(l2), iyr-ipowsw(l2)
c             IF(IPOWSW(L2).LE.0) GOTO 410
              IF(ipowsw(L2).eq.0) GOTO 410
              if(ipowsw(l2).gt.0 .and. iyr-ipowsw(l2).lt.0) goto 410
              if(ipowsw(l2).lt.0 .and. iyr+ipowsw(l2).gt.0) goto 410
            endif
c_______________________________________________________________________
c           Test for a downstream call
c           if(idcall.ne.0 .and. idcallx.eq.0) then
c             dcallx=rdvxxx(l2)
c             idcallx=1
c             iwTemp=iw             
c             l2Temp = idcall
c             ncloc = 4
c             call ifrdown(iwTemp,l2Temp,l1,fac,ncloc,dcallx)
c           endif    
c_______________________________________________________________________
            CALL POWRIG(IW,L2)
            GOTO 410
          endif
c         endif OTHER WATER RIGHTS (POWER DEMAND, ETC.) (l1.eq.4)
c_______________________________________________________________________
c         Step 30; OPERATION Rights
            if(l1.eq.5) then
cr            write(nlogx,*) '  Execut; l1, l2, ityopr = ', l1,l2,ityopr(l2)
c_______________________________________________________________________
c           Skip if not active for this year
c           ioprsw     =  1; on
c                      =  0; off
c                      = -n; simulate until year n (e.g. -1980)
c                      =  n; begin to simulate in year n (e.g. 1980)
            IF(ioprsw(l2).eq.0) GOTO 410
            ioff=0
            if(iyr.lt.ioBeg(l2)) ioff=1
            if(iyr.gt.ioEnd(l2)) ioff=1
            if(ioff.eq.1) then
cr            write(nlogx,107)iyr,imo,iwx, corid(l2),
cr   1          iobeg(l2),ioend(l2),ioprsw(l2)
cr  107       format('  Execut; FYI Rule turned off in year, month, iter ',
cr   1               3i5, 1x, a12, 1x, 3i8)                       
              goto 410
            endif
c_______________________________________________________________________
c           Step 31; Branch for Operation Type
            if(ityopr(l2).le.0 .or. ityopr(l2).gt.maxOprin) then
              write(nlogx,550) ityopr(l2), maxOprin
              goto 9999
            endif
c
c rrb 2018/07/13; Add type 51 Reservoir-Flow Control
c _______________________________________________________________________
            go to (190,200,200,220,230,240,250,252,300,410,
     1             311,312,313,314,315,316,317,318,319,320,
     1             321,322,323,324,325,326,327,328,329,330,
     1             331,332,333,334,335,336,337,338,339,340,
cx   1             341,342,343,344,345,346,347,348,349,350) ityopr(l2)
     1             341,342,343,344,345,346,347,348,349,350,
     1             351,352,353,354) ityopr(l2)
     
c_______________________________________________________________________
c           Step 32; Problem if the operating rule does not exist
            write(nlogx,*) 
     1        ' Problem operating rule type not defined = ',ityopr(l2)
            goto 9999
c_______________________________________________________________________
c               Type 1. Reservoir to Instream flow
c
  190   if(ireach.eq.0) then
          if(ichk.eq.94) write(nlogx,*) 
     1                 ' Execut; Call 1-PowRes ',corid(l2)   
            call powres(iw,l2,divactx,ncall(1))
          else
          if(ichk.eq.94) write(nlogx,*) 
     1                 ' Execut; Call 1-PowRes2 ',corid(l2)
          call powres2(iw,l2,divactx,ncall(1))
            endif
            goto 400
c
c_______________________________________________________________________
                
c               Type 2 or Type 3. Reservoir release to diversion 
c                 or reservoir via the river or to a carrier
c
  200  irep=0
       tranlim=0.0
       dcrdivx=0.0
       divdx=0.0
c      
       if(ichk.eq.94) write(nlogx,*) ' Execut; 2,3-DivRes ',corid(l2) 
       call divres(iw,l2,ishort,irep,tranlim,dcrdivx,divdx,
     1   divactx,divacty,ncall(2))
         goto 400
c
c_______________________________________________________________________
                      
c
c               Type 4. Diversion by Exchange with a reservoir                  
c
  220  irep=0
       tranlim=0.0
       dcrdivx=0.0
       divdx=0.0
c      
       if(ichk.eq.94) write(nlogx,*) ' Execut; Call 4-DivRpl',corid(l2) 
       call divrpl(iw,l2,ishort,irep,tranlim,dcrdivx,divdx,
     1   divactx,divacty,ncall(4))
           goto 400
c 
c_______________________________________________________________________
              
c
c              Type 5. Reservoir storage by Exchange with a reservoir
c
  230 continue
      if(ichk.eq.94) write(nlogx,*) ' Execut; Call 5-ResRpl',corid(l2)
      call resrpl(iw,l2,divactx,ncall(5))
      goto 400
c
c_______________________________________________________________________
             
c
c              Type 6. Transfer from reservoir to reservoir by carrier
c               (aka bookover)   Note: No returns !
  240 continue
c
c rrb 2021/05/30; Runtime Check 
cx    if(ichk.eq.94) write(nlogx,*) ' Execut; Call 6-RsSpu',corid(l2)
cx    if(ichk.eq.94 .or. ichk.eq.4) then
cx      write(nlogx,*) ' Execut; Call 6-RsSpu',corid(l2)
cx    endif  
c
c rrb 2015/07/08; Add capability to not call this iteration based 
c                 on user provided data (See documentation for
c                 Type 6 operating rule)
        CALL RSRSPU(IW,L2,ncall(6))
c
c rrb 2015/07/30; Add detailed output for a type 6 operating rule
c rrb 2015/05/30; This was revised and is now reported with other
c                 operating rules below if sent to 400
cx     if(ichk.eq.94 .or. ichk.eq.4) then  
cx        rec12b='Opr Rule    '
cx        call outIchk(
cx     1    ichkX, ichk4n, l1, l2, iw, ityopr(l2), ishort, fac, 
cx     1    uDem, divact2, divx, divsum, 4, divact2, rec12b) 
cx      endif 
cx             
cx    goto 410
      goto 400
c
c_______________________________________________________________________
                      
c
c               Type 7. Exchange to a Carrier System
c
  250 continue 
      if(ichk.eq.94) write(nlogx,*) ' Execut; Call 7-Carrpl',corid(l2)   
      call carrpl(iw,l2,divactx, ncall(7))
      goto 400
c
c_______________________________________________________________________
c
c               Type 8. Reservoir to reservoir bookover with      
c                       additional constraints (e.g. Blue River Decree)
c
  252 continue
      if(ichk.eq.94) write(nlogx,*) ' Execut; Call 8-OopBook2',corid(l2)   
      call OopBook2(iw,l2,divactx, ncall(8))            
      goto 400
c
c_______________________________________________________________________
                                                      
c
c              Type 9. Target release (spill) for power or whatever
c
  300 continue    
      if(ichk.eq.94) write(nlogx,*) ' Execut; Call 9-PowSea ',corid(l2)   
      call powsea(iw,l2,divactx,ncall(9))
      goto 400
c
c_______________________________________________________________________
                      
c               Type 10. Not called in the operational Right loop
c               called after each direct flow water right 
c               Currently sent to 410 to allow variable year option
c_______________________________________________________________________
c
c               Type 11. Diversions by demand through carrier 
c
  311 continue
c     write(nlog,*) ' '
c     write(nlog,*) ' Execut; In divsum ', divactx*fac, divsum 
c
c rrb 2010/01/25; Revise to reoperate by passing divactx
c           call divcar(iw,l2,ishort,divx,ncall(11))
      if(ichk.eq.94) write(nlogx,*) ' Execut; Call 11-DivCar ',corid(l2) 
      call divcar(iw,l2,ishort,divactx,ncall(11))            
c     write(nlog,*) ' Execut; Out divsum ', divactx*fac, divsum 
      goto 400
c
c_______________________________________________________________________
c rrb 01/31/95; Code Addition
c               Type 12. Reoperation
c
  312 divactx = 0.0
      if(ichk.eq.94) write(nlogx,*) ' Execut; Call 12-NA ',corid(l2)   
c
c rrb 2008/05/07; Revise to insure reoperation at least once
c                 per iteration (ireop12=0) and if change is significant

      if(ireop12.eq.0 .or. divsum.gt.divchk*fac) then
        ireop12=ireop12+1
        iw = 0
        iwxx=0
        iwx=iwx+1
        iwxmaxY=iwxmaxY+1
      
        iretsw= 0
        ireop= 0
      
cx      iout=1
c
c -------------------------------------------------------
c Print detail for a type 12 reoperation
        if(iout.eq.1 .or. ichk.eq.9 .or. ichk.eq.109) then      
           rec12b='Opr Rule    '
           call outIchk(
     1       ichkX, ichk4n, l1, l2, iw, ityopr(l2), ishort, fac,
     1       uDem, divact2, divx, divsum, 1, divact1, rec12b)      
        endif
        divsum=0.0
      
c
c ____________________________________________
c rrb 20100123; OMID Check   
        if(ioutGVC.eq.1) then
          call outGVC(nlogx, 2, divact1, divsum, gvCot,
     1      noutGVC, fac, l2, 'Opr Rule    ', 
     1      ityopr(l2)+100, corid(l2), nameo(l2))
        endif
      endif
c
c       Goto 400 to avoid resetting variable ireop12
      goto 410
c
c_______________________________________________________________________
c rrb 01/17/96; 
c               Type 13. Index flow constraint (La Plata Compact)
c                        Note Execute on first reoperation per time
c                        step (iwx = 1)
  313 continue
      if(ichk.eq.94) write(nlogx,*)' Execut; Call 13-IfrrigX',corid(l2)   
      call ifrrigx(iw,l2, ncall(13))
      goto 400
c    
c_______________________________________________________________________
c
c               Type 14. Diversions by demand through carrier 
c                        ditch system with an annual limit on the carrier
c
  314       continue
c
c rrb 2010/01/25; Revise to reoperate by passing divactx  
c           call divcar1(iw,l2,ishort,divx,ncall(14))
      if(ichk.eq.94) write(nlogx,*)' Execut; Call 14-DivCar1',corid(l2) 
      call divcar1(iw,l2,ishort,divactx,ncall(14))            
      goto 400
c
c_______________________________________________________________________
c
c rrb 99/06/23; Type 15. Interruptible Supply
c
  315 continue
      if(ichk.eq.94) write(nlogx,*)' Execut; Call 15-InterSup',corid(l2)   
      call intersup(iw,l2,1)
      goto 400
c
c_______________________________________________________________________
c
c rrb 99/06/23; Type 16. Direct Flow Storage
c
  316 continue   
      if(ichk.eq.94) write(nlogx,*)' Execut; 16-Call DirectFS',corid(l2)   
      call directfs(iw,l2,ishort,divx,ncall(16))
      goto 400
c
c_______________________________________________________________________
c
c rrb 99/06/23; Type 17. Rio Grande Compact for Rio Grande
c
  317 continue
      if(ichk.eq.94) write(nlogx,*) 'Execut; Call 17-RgRg',corid(l2)   
        call rgrg(iw,l2,1,nrg1,0,0)
        l2rgrg=l2
        goto 400
c
c_______________________________________________________________________
c
c rrb 99/06/23; Type 18. Rio Grande Compact for Conejos
c
  318 continue
      if(ichk.eq.94) write(nlogx,*) ' Execut; Call 18-RgRg',corid(l2)   
        call rgrg(iw,l2,2,nrg2,0,0)
        l2rgco=l2
        goto 400
c
c_______________________________________________________________________
c
c rrb 99/06/23; Type 19. Split Channel
c
  319 continue
      if(ichk.eq.94) write(nlogx,*)' Execut; Call 19-DivCar2',corid(l2)   
      call divcar2(iw,l2,ishort,divx) 
        goto 400
c

c_______________________________________________________________________
c
c rrb 00/11/05; Type 20. San Juan RIP
  320 continue  
      if(ichk.eq.94) write(nlogx,*)' Execut; Call 20-SjRip',corid(l2)   
      call sjrip(iw,l2,isjon,divactx)
        goto 400
c
c_______________________________________________________________________
c
c rrb 00/11/05; Type 21. Sprinkler Use 1x/time step
c
  321 continue
      if(icallsp.eq.0) then
        if(ichk.eq.94) write(nlogx,*)' Execut; Call 21-Spruse',corid(l2)
          call spruse(iw,l2,divx,ncall(21))
          icallsp=1
        endif
        goto 400
c
c_______________________________________________________________________
c
c rrb 00/11/05; Type 22. Soil Moisture Use 1x/time step
c
  322       continue
     
      if(icallsm.eq.0) then 
        if(ichk.eq.94) write(nlogx,*)' Execut; Call 22 SoilM',corid(l2)
        call soilm(iw,l2,divx)
        icallsm=1
      endif
      goto 400
c
c_______________________________________________________________________
c
c rrb 04/08/24; Type 23. Downstream Call
c               Note the Downstream operating rule has a variable
c               admin number. Therefore it is typically called before 
c               an instream flow, reservoir, diversion or well right. 
c               If not called by any of the above then is called here
c               at the admin number specified in the operation right
c               file. Therefore it should be entered as the most 
c               junior right per documentation.
c               Note dcall1 is set in bomsec or dayest
c
  323 dcallx=-1.0
      if(idcall.gt. 0 .and. idcallx.eq.0) then
        if(iout.eq.2) then
          write(nlogx,580)IYRmo(mon),xmonam(mon),idy,rdvnk(l2),dcall1
        endif
        idcallx=1
        ncloc=5
        if(ichk.eq.94) write(nlogx,*)
     1               ' Execut; Call 23-IfrDown',corid(l2)
        call ifrDown(iw,l2,l1,fac,ncloc,dcallx)
      endif  
      goto 410
c_______________________________________________________________________
c rrb 99/06/23; Type 24. Direct Flow Exchange (Alt. Point)
  324  continue
c      write(nlog,*) ' Execut; type 24 In Avail(8) ', avail(8)*fac,
c    1   avinp(8)*fac         
       if(ichk.eq.94) write(nlogx,*)
     1               'Execut; Calling 24-DirectEX',corid(l2)
cx       write(nlog,*)
cx     1   ' Execut; type 24; iw iwx l2 iOprLim(l2) oprlimit(l2)',
cx
c jhb 2014/10/27 check for the reop step limit for type 24 rules
       if (iOprLim(l2).lt.0) then
c        the oprlimit value in the opr file is less than 0
c        this indicates the modeler wants to freeze this rule's result
c        after a number of reop steps, almost certainly after ONE reop step
         if(iwx.le.-iOprLim(l2))then
           call directEX(iw,l2,ishort,divactX,ncall(24))
         endif
       else
         call directEX(iw,l2,ishort,divactX,ncall(24))
       endif
c      write(nlog,*) ' Execut; type 24 Out Avail(8)', avail(8)*fac,
c    1   avinp(8)*fac       
       goto 400
c_______________________________________________________________________
c
c rrb 05/01/28; Type 25. Direct Flow Bypass (Alt. Point)
c
c 325  write(nlog,*) ' Execut; Calling DirectBY'
  325  continue
c      write(nlog,*) ' Execut; type 25 In Avail(8) ', avail(8)*fac,
c    1   avinp(8)*fac       
       if(ichk.eq.94) write(nlogx,*)
     1               ' Execut; Calling 25-DirectBy',corid(l2) 
       call DirectBy(iw,l2,ishort,divactX,ncall(25))
            
c      write(nlog,*) ' Execut; type 25 Out Avail(8)', avail(8)*fac,
c    1  avinp(8)*fac   
       goto 400
c_______________________________________________________________________
c
c rrb 2014-11-24; Type 26. Changed Water Right
cx
cx 326       call PowResP(iw,l2,divactX,ncall(26))
cx rrb 2007/12/26; Move to type 48
cx  326    goto 400
  326  continue
c
c rrb 2015/07/08; Add capability to not operate any more
c                 this time step by water right using
c                 icallOP(l2) that is set in DirectWR
c                 not by operating rule.  With this correction
c                 more than one type 26 operating rule
c                 can be provided as input
cx    if(icall26.eq.0) then
c     
c        write(nlogx,*) ' Execut; Calliing directWR, icall26 ', icall26 
c        write(nlogx,*) ' Execut; type 26 In Avail(8) ', avail(8)*fac,
c     1                avinp(8)*fac 
        if(ichk.eq.94) write(nlogx,*) 
     1               ' Execut; Calliing 26 directWR',corid(l2)
c
c
C
c rrb 2018/07/29; Add Reservoir Control when ioprlim(l2)=5
       call directWR(iw,l2,ishort,divactX,ncall(26),iflow)
c        
c      icall26=1
c     endif    
c          
c      write(nlog,*) ' Execut; type 26 Out Avail(8)', avail(8)*fac,
c    1               avinp(8)*fac   
       goto 400
c_______________________________________________________________________
c
c rrb 05/01/30; Type 27. Direct Release 
c                        From a Reservoir or ReUse Plan 
c                        To a Diversion, Reservoir, or Carrier 
c                        With Reuse
c
c ---------------------------------------------------------
c rrb 2007/10/26; Add ability to be called by Replace
  327       continue
            irep=0
            tranlim=0.0
            dcrdivx=0.0
            divdx=0.0
cx       write(nlog,*) ' Execut; type 27 In Avail(8) ', avail(8)*fac,
cx     1   avinp(8)*fac
cx       write(nlog,*)'  Execut; Warning type 27 off'   
cx
       if(ichk.eq.94) write(nlogx,*) 
     1              ' Execut; Calling 27-DivResP2',corid(l2) 
c
c rrb 2018/03/09; Test
cx
       call DivResP2(iw,l2,ishort,irep,tranlim,dcrdivx,divdx,
     1   divactx,divacty,ncall(27))
       
c      write(nlog,*) ' Execut; type 27 Out Avail(8)', avail(8)*fac,
c    1   avinp(8)*fac
       goto 400
c_______________________________________________________________________
c
c rrb 05/01/29; Type 28. ReUse Plan to a Diversion by Exchange
c
c
c ---------------------------------------------------------
c
  328 continue 
      irep=0
      tranlim=0.0
      dcrdivx=0.0
      divdx=0.0  
cx      write(nlog,*)'  Execut; Warning type 28 off'    
      if(ichk.eq.94) write(nlogx,*) 
     1             ' Execut; Calling 28-DivRplP',corid(l2) 
c
      call divRplP(iw,l2,ishort,irep,tranlim,dcrdivx,divdx,
     1             divactx,divacty,ncall(28))
      goto 400
c_______________________________________________________________________
c
c rrb 05/01/30; Type 29. Plan Spill
c
  329 continue
c     write(nlog,*) ' Execut; type 29 In Avail(8) ', avail(8)*fac,
c    1    avinp(8)*fac    
cx       write(nlog,*)'  Execut; Warning type 29 off'    
      if(ichk.eq.94) write(nlogx,*) 
     1             ' Execut; Calling 29-PowSeaP',corid(l2) 
c
c rrb 2018/03/09; test
cx    
       call powseaP(iw,l2,divactx,ncall(29))  
        
c      write(nlog,*) ' Execut; type 29 Out Avail(8)', avail(8)*fac,
c    1    avinp(8)*fac     
       goto 400
c_______________________________________________________________________
c
c rrb 05/02/01; type 30 Re store a T&C Plan release
c
  330  continue
       if(ichk.eq.94) write(nlogx,*) 
     1              ' Execut; Calling 30 ResRglP',corid(l2)   
       call Resrg1P(iw,l2,ncall(30))
       goto 400
c_______________________________________________________________________
c
c rrb 05/03/29; type 31 Import with Reuse via a carrier 
c
  331  continue
       if(ichk.eq.94) write(nlogx,*) 
     1              ' Execut; Calling 31-DivCarR',corid(l2)   
       call divCarR(IW,L2,ISHORT,divactx,ncall(31))
       goto 400
c_______________________________________________________________________
c
c rrb 05/03/29; type 32 Res and ReUse Plan to a Div, Res or Carrier
c               with reuse Direct
  332  continue
       if(ichk.eq.94) write(nlogx,*) 
     1              ' Execut; Calling 32-DivResR',corid(l2)  
cx     write(nlog,*)'  Execut; Warning type 32 off'       
       call divResR(iw,l2,ishort,divactx, ncall(32))  
       goto 400
c_______________________________________________________________________
c
c rrb 05/01/29; Type 33. Res and ReUse Plan to a Div, Res or Carrier
c               with reuse Exchange
  333  continue
       if(ichk.eq.94) write(nlogx,*) 
     1              ' Execut; Calling 33-DivRplR',corid(l2)   
       call divRplR(iw,l2,ishort,divactx,divacty, ncall(33))
       goto 400
c
c_______________________________________________________________________
             
c
c              Type 34. Bookover reservoir to reservoir with Reuse
  334  continue
       if(ichk.eq.94) write(nlogx,*) 
     1              ' Execut; Calling 43-RsrSpuP',corid(l2)   
       call RsrSpuP(IW,L2,ncall(34))
       goto 410
c_______________________________________________________________________
c
c rrb 05/03/29; Type 35 Import with Reuse (NO CARRIER)
c
  335  continue
       if(ichk.eq.94) write(nlogx,*) 
     1              ' Execut; Calling 35-DivIMpR',corid(l2) 
c
       call divImpR(iw,l2,ishort,divactx,ncall(35))  
       goto 400
c_______________________________________________________________________
c
c rrb 06/01/18; Type 36 Diversion with seasonal constraint
c
  336  continue
       if(ichk.eq.94) write(nlogx,*) 
     1              ' Execut; Calling 36-DivRigS',corid(l2)   
       call divRigS(iw,l2,ishort,ncall(36))
       goto 400
c_______________________________________________________________________
c
c rrb 06/01/18; Type 37 Augmentation Well
c
  337  continue
       if(ichk.eq.94) write(nlogx,*) 
     1              ' Execut; Calling 37-WelAugP',corid(l2)   
       call WelAugP(iw,l2,retx,divx,ncall(37))
       goto 400
c_______________________________________________________________________
c
c rrb 06/01/18; Type 38 Out-of-Priority Diversion

  338  continue
       if(ichk.eq.94) write(nlogx,*) 
     1              ' Execut; Calling 38-OopDiv',corid(l2)   
       call OopDiv(iw,l2,ishort,divactx, ncall(38)) 
       goto 400
c_______________________________________________________________________
c
c rrb 06/01/18; Type 39 Alternate Point

  339  continue
       if(ichk.eq.94) write(nlogx,*) 
     1              ' Execut; Calling 39-DivAlt',corid(l2)   
       call DivAlt(iw,l2,ishort,divactx, ncall(39)) 
       goto 400
c
c_______________________________________________________________________
c
c rrb 2006/08/24; Type 40. South Platte Compact release to Compact
c
  340  continue

c
c rrb 2011/04/04; Correction only call if the destination is
c                 an ISF (iopDesR(l2)=1)
       if(iopDesR(l2).eq.1) then 
cx       write(nlogx,*) ' Execut; Calling SPlatte to release'        
       if(ichk.eq.94) write(nlogx,*) 
     1              ' Execut; Calling 40-SPlatte',corid(l2)
         CALL SPlatte(IW, l2, l2, ISHORT, nd, divactx,ncall(140)) 
       endif    
       goto 400
c
c_______________________________________________________________________
c
c rrb 2006/08/24; Type 41. Reservoir Storage with Limits

  341  continue
       if(ichk.eq.94) write(nlogx,*) 
     1              ' Execut; Calling 41-DivRgP',corid(l2)   
       call ResRgP(iw,l2, ncall(41))
       goto 400
c_______________________________________________________________________
c
c rrb 05/01/30; Type 42. Plan Reset
c
  342  continue
       if(ichk.eq.94) write(nlogx,*) 
     1              ' Execut; Calling 42-PowSeaR',corid(l2)   
       call powseaR(iw,l2,ncall(42))  
       goto 400
c_______________________________________________________________________
c
c rrb 05/01/30; Type 43. In-Priority Supply
c
  343  continue
       if(ichk.eq.94) write(nlogx,*) 
     1              ' Execut; Calling 43-WelPrity',corid(l2)   
       call WelPrity(iw,l2,ncall(43))  
       goto 400
c_______________________________________________________________________
c
c rrb 05/01/30; Type 44. Recharge Well to a Reservoir
c
  344  continue
       if(ichk.eq.94) write(nlogx,*) 
     1              ' Execut; Calling 44-WelRech',corid(l2) 
c
c rrb 2018/03/09; test
         call WelRech(iw,l2,ncall(44))  
       goto 400
c_______________________________________________________________________
c
c rrb 05/01/30; Type 45. Carrier with Loss
c
  345  continue  
       if(ichk.eq.94) write(nlogx,*) 
     1              ' Execut; Calling 45-DivCarL' ,corid(l2)
c
c rrb 2018/07/29; Add reservoir-flow control variable (iflow) where
c                 iflow = 1 is Project on and =0 is Project off
cx     call DivCarL(iw,l2,ishort,divactx, ncall(45))    
       call DivCarL(iw,l2,ishort,divactx, ncall(45),iflow)  
c      write(nlog,*) '  Execut; ID = ',corid(l2)
       
       goto 400
c_______________________________________________________________________
c
c rrb 2007/08/20; Type 46. Multiple Ownership
c
  346 continue
       if(ichk.eq.94) write(nlogx,*) 
     1              ' Execut; Calling 46-DivMulti',corid(l2)   
cx     write(nlog,*)'  Execut; Warning type 46 off'       
       call DivMulti(iw,l2,ncall(46))  
      
       goto 400
c_______________________________________________________________________
c
c rrb 2007/08/20; Type 47. Release Limit
c                 This rule only sets limits.
c
  347  continue
       goto 400
c_______________________________________________________________________
c
c rrb 05/01/28; Type 48. Res or Reuse Plan to a T&C or Aug Plan direct
c
  348  continue
       if(ichk.eq.94) write(nlogx,*) 
     1              ' Execut; Calling 48-PowResP',corid(l2)
c
c rrb 2018/03/09; Test
         call PowResP(iw,l2,divactX,ncall(48))
       goto 400
c_______________________________________________________________________
c
c rrb 05/01/28; Type 49. Res or Reuse Plan to a T&C or Aug Plan by exch
c
  349  continue
       if(ichk.eq.94) write(nlogx,*) 
     1              ' Execut; Calling 49-DivRplP2',corid(l2)   
       call divRplP2(iw,l2,divactX,ncall(49))   
       if(ichk.eq.94) then
         write(nlogx,*) ' Execut; Back From DivRplP2'  
         write(nlogx,*) ' Execut;', 
     1    qdiv(26,179)*fac, qdiv(29,179)*fac, qdiv(30,179)*fac
       endif
       goto 400
c
c_______________________________________________________________________
c
c rrb 2006/08/24; Type 50. South Platte Compact Storage
c
  350  continue 
       if(ichk.eq.94) write(nlogx,*) 
     1              ' Execut; Calling 50-IfrRigSP',corid(l2) 
       ispK=l2  
       call IfrRigSP(IW,L2,ISHORT,divactX,ncall(50))  
       goto 400
c
c_______________________________________________________________________
c
c rrb 2018/07/13; Type 51. Flow-Reservoir Control
c
  351  continue 
       if(ichk.eq.94) write(nlogx,*) 
     1              ' Execut; Calling 51-IfrRigSP',corid(l2) 
       call FlowRes(IW,L2,Iflow,ncall(51))
       goto 400
c_______________________________________________________________________
c
c rrb 2007/08/20; Type 52. Multiple Reservoir 
c
  352 continue
       if(ichk.eq.94) write(nlogx,*) 
     1              ' Execut; Calling 52-DivMultR',corid(l2)   
       call DivMultR(iw,l2,ncall(52))        
       goto 400
c_______________________________________________________________________
c
c rrb 2018/08/20; Type 53. JMStorage
c
  353 continue
       if(ichk.eq.94) write(nlogx,*) 
     1              ' Execut; Calling 53-JMStore',corid(l2)   
       call JMStore(iw,l2,ncall(53))       
       goto 400
c_______________________________________________________________________
c
c rrb 2018/08/20; Type 54. JM Flow
c
  354 continue
       if(ichk.eq.94) write(nlogx,*) 
     1              ' Execut; Calling 54-JMFlow',corid(l2)
c
c ---------------------------------------------------------
c  rrb 2021/08/15; Runtime error initialization related to not saving local
c                    variables with Gfortran compilation
c                  Initialize percentage and average once per year
       if(mon.eq.1 .and. idy.eq.1 .and. icallOP(l2).eq.0) then
         pctBjm=0.0
         pctEjm=0.0
         aveBjm=0.0
         aveEjm=0.0
         rday1jm=0.0
         rday2jm=0.0
cx         write(nlog,*) '  Execut; JMFlow initializing:',
cx     1     ' iyr = ', iyr, ' mon = ',mon, ' idy = ', idy,
cx     1     ' icallop = ', icallop(l2)
       endif

       call JMFlow(iw,l2,ncall(54),
     1             pctBjm, pctEjm, aveBjm, aveEjm, rday1jm, rday2jm)
       goto 400
c
c_______________________________________________________________________
c
            goto 9999            
c_______________________________________________________________________
c
c               Endif for Operational Rights
       endif
c
c__________________________________________________
c      Step 33; WELL RIGHTS
c
       if(l1.eq.6) then
c
c rrb 99/03/22
c
c               Skip if not active for this year
c               idvrsww    =  0; opr right is off
c                          = -n; simulate until year n (e.g. -1980)
c                          =  n; begin to simulate in year n (e.g. 1980)
c                          =  1; on for all practical purposes
       if(idvrsww(l2).ne.1) then
c              write(nlogx,*) '  Execut; Well',
c    1            l2, iyr, idvrsww(l2), iyr-idvrsww(l2)
         IF(idvrsww(L2).eq.0) goto 410
         if(idvrsww(l2).gt.0 .and. iyr-idvrsww(l2).lt.0) goto 410
         if(idvrsww(l2).lt.0 .and. iyr+idvrsww(l2).gt.0) goto 410
       endif
c
c rrb 01/01/13; Sprinkler Use
       ispruse=0
c
c ---------------------------------------------------------
c               Test for a downstream call before a well
c               Note dcall1 is set in bomsec or dayest

        if(idcall.ne.0) then
          dcallx=rdvnkw(l2)
          if(rdvnkw(l2).gt.dcall1 .and. idcallx.eq.0) then
c           write(nlogx,*) '  Execut before a Wel ',rdvnkw(l2),dcall1
            idcallx=1
            iwTemp=iw             
            l2Temp = idcall
            ncloc=6
            call ifrdown(iwTemp,l2Temp,l1,fac,ncloc,dcallx)
          endif
        endif    
c
c ---------------------------------------------------------
c               Call Welrig
        if(iplanwOn.eq.0) then
          call welrig3(iw,l2,ispruse,retx,divx,ncall(106))
        else
             
          call Welrig3P(iw,l2,ispruse,retx,divx,ncall(106))
        endif  
c
c ---------------------------------------------------------
c rrb 2011/05/07; Detailed output for a well water right      
      if(ichk.eq.94 .or. ichk.eq.4) then
c
c rrb 2021/03/20; Correction            
cx        call outIchk(
cx     1    6, ichk4n, l1, l2, iw, 0, ishort, fac,
cx     1    uDem, divactx, divx, divsum, 0, iwx, rec12b)
        call outIchk(
     1    6, ichk4n, l1, l2, iw, 0, ishort, fac,
     1    uDem, divactx, divx, divsum, 0, -1.0, rec12b)
      endif          
c
c rrb 2008/01/23; Correction 400 is an operating rule check
c       goto 400
        goto 410
      endif
c
c_______________________________________________________________________
c
c               Step 34; Reoperation Check for operating rules
c                        (new flows)
c                        Note divsum is in af
 400  continue

c_______________________________________________________________________
c
c               Step 35; Reoperation Check for (new flows) from operating rules
c                        Note divsum is in af
      divact2=divactx
      if(ireopx.ge.0) then
        divact1=divactx*fac
        divsum=divactx*fac
        divactx=0.0
      else
        divact1=divactx*fac
        divsum=divsum+divactx*fac
        divactx=0.0
      endif
c
c_______________________________________________________________________
c
c      Step 36; Detailed Output for an Operating Rule (ichk=4 or 94)
c rrb 2011/04/25; Limit output for ichk=4
      if(l1.eq.5 .and. (ichk.eq.94 .or. ichk.eq.4)) then   
c
c rrb 2021/03/20; Correction
cx        call outIchk(
cx     1    14, ichk4n, l1, l2, iw, ityopr(l2), ishort, fac,
cx     1    uDem, divact2, divx, divsum, 0, iwx, rec12b)
        call outIchk(
     1    14, ichk4n, l1, l2, iw, ityopr(l2), ishort, fac,
     1    uDem, divact2, divx, divsum, 0, -1.0, rec12b)
c
cx        write(nlog,*) 'Execut Type 5; 18, divo(18)', 18, divo(18)*fac
      endif
c_______________________________________________________________________
c rrb 04/22/96; 
c      Step 37; Global control on reoperations (ireopx) 
c              Reoperation check regarding an operating rule
c              (e.g. res release, exchange, etc.)
cx    write(nlog,*) ' Execut; ireopx, divsum, divchk, iwx', 
cx   1  ireopx, divsum, divchk*fac, iwx
      if (ireopx.le.0 .and. divsum.gt.divchk*fac .and.
     1   iwx.lt.iwxlimit) then
        iw = 0
        iwx=iwx+1
        iwxmaxY=iwxmaxY+1
    
cx      write(nlog,*) ' Execut; Operating Rule ',
cx      iout=1
        if(iout.eq.1 .or. ichk.eq.9 .or. ichk.eq.109) then
c ____________________________________________
c rrb 2008/03/13; Check if replace was called; else an operating rule
c           Detailed Reoperation Output - Diversion return Flow
c           Note ispOut =0 indicates Splatte was NOT called after divrig
          if(l1.eq.3 .and. ispOpr.eq.0) then
            rec12b='Div Return  '     
            call outIchk(
     1        ichkX, ichk4n, l1, l2, iw, ityopr(l2), ishort, fac, 
     1        uDem, divact2, divx, divsum, 2, divchkR, rec12b)  
c_______________________________________________________________________
c rrb 20100123; OMID Check for Return Flows caused by Replace   
            if(ioutGVC.eq.1) then
              call outGVC(nlog, 3, divact1, divsum, gvCot,
     1                    noutGVC, fac, l2, rec12b, l1, rec12, nameX)   
            endif            
          endif
c_______________________________________________________________________
c rrb 2011/05/17; Print when Splatte is called after divert
c         Note ispOut =1 indicates Splatte was called after divrig     
          if(l1.eq.3 .and. ispOpr.eq.1) then
            rec12b='Opr Rule    '
            call outIchk(
     1        ichkX, ichk4n, l1, isp1, iw, ityopr(l2), ishort, fac, 
     1        uDem, divact2, divx, divsum, 3, divact2, rec12b)   
          endif                
c_______________________________________________________________________
c         Detailed Reoperation Output - Operating Rule 
          if(l1.eq.5) then    
            rec12b='Opr Rule    '
            call outIchk(
     1        ichkX, ichk4n, l1, l2, iw, ityopr(l2), ishort, fac, 
     1        uDem, divact2, divx, divsum, 4, divact2, rec12b)
c_______________________________________________________________________
c rrb 20100123; OMID Check for an Operating Rule
            if(ioutGVC.eq.1) then
              call outGVC(nlog, 4, divact1, divsum, gvCot,     
     1          noutGVC, fac, l2, 'Opr Rule    ', 
     1          ityopr(l2)+100, corid(l2), nameo(l2))                  
            endif
          endif
c         endif (l1.eq.5)
        endif
c       endif (iout.eq.1 .or. ichk.eq.9 .or. ichk.eq.109)
c_______________________________________________________________________
c       Step 38; Initialize for reoperation
cx      iout=0 
        divsum = 0.0
        ireop12=0
c rrb 2008/03/13; Addition
        divactx=0.0
        iretsw = 0
        ireop  = 0
        idcallx=0
c_______________________________________________________________________
c       Step 39; Reset call indicator
        do i=1,numsta
          imcdL(i)=-1
        end do
c_______________________________________________________________________
c       Initialize operating limit for shared water right
c       associated with a type 45
        do k=1,numopr
          if(ityopr(k).eq.945) then
            do i=1,12
              oprmax(k,i)=-1
            end do
c           write(nlog,*) '  Execut;  oprmax reset k=', k
          endif
        end do  
c_______________________________________________________________________
      endif
c     endif reoperation  (ireopx.le.0 .and. divsum.gt.divchk*fac .and. iwx.lt.iwxlimit)              
c_______________________________________________________________________
 410  continue
c_______________________________________________________________________
c     Step 40: Reoperation Check for maximum iterations allowed
      if(iwx.ge.iwxlimit) then
        write(nlogx,*) 
     1   '  Execut; warning reoperation limit of ',
     1   iwxlimit, ' Exceeded'
        write(nlogx,*) '  Execut; ireopx, ireop', ireopx, ireop
        iw = ntorig+1           
        goto 9999           
      endif
c_______________________________________________________________________
c     Step 41; Reoperation Check for return flows (ireop=1)
c         from non operating rules (l1.ne.5)
c         Note if reoperation is necessary iw is set to 0
c         which triggers a goto 135 below
c rrb 2006/11/17; Isolate to non operating rules
cr    if(ireopx.le.0 .and. ireop.eq.1 .and.
      if(ireopx.le.0 .and. ireop.eq.1 .and. l1.ne.5 .and. 
     1      iwx.lt.iwxlimit) then
        iw = 0
        iwx = iwx+1
        iwxmaxY=iwxmaxY+1      
cx      write(nlog,*) ' Execut; Return Flow ', iwx
        ireop  = 0
        idcallx=0
c rrb 2006/09/18
        divsum=divchkR*fac
c       Print detailed call data
        if(iout.eq.1 .or. ichk.eq.9 .or.ichk.eq.109) then
          rec12b='XXX Return  '
          call outIchk(
     1      ichkX, ichk4n, l1, l2, iw, ityopr(l2), ishort, fac, 
     1      uDem, divact2, divx, divsum, 5, divchkR, rec12b)
        endif
c       Reset call indicator and Divsum
        do i=1,numsta
          imcdL(i)=-1
        end do
        divsum = 0.0
        ireop12=0
c rrb 2011/05/11; Correction reset divsum
cx      endif
c rrb 20100123; OMID Check
        if(ioutGVC.eq.1) then
          call outGVC(nlog, 5, divact1, divsum, gvCot,      
     1                noutGVC, fac, l2, rec12b, l1, rec12, nameX)
        endif    
      endif
c_______________________________________________________________________
c     Step 42; Print Detailed Call Data
c       write(nlog,*) ' Execut_3; icall =', icall   
      if(icall.gt.0) call calldat(1, l1, l2, icallx, ishort, fac)
c_______________________________________________________________________
c       Test for end of Water right and reoperation loop
c       Note if reoperating iw is set to 0 above
c       Note ntorig is the number of water rights from riginp
c rrb 2011/05/12; Correction
cx    if(iw.le.ntorig) goto 135
      if(iw.lt.ntorig) goto 135
c        
c     End of Water Right and Reoperation Loop
c       Because the number of rights simulated (iw)
c       is greater than the total number of rights (ntorig)
      iwxt = iwxt + iwx
c__________________________________________________________
c     Step 43; Reservoir Seepage at  at end of time step
c rrb 2006/10/18; Moved from beginning of time step
c rrb 2008/05/07; Re-operate because of seepage returns once
c                 (when iseep=0) if total seepage (seepT) > 0
c
c rrb 2008/09/28; REvise to be called multiple times per iteration
cx    IF(NUMRES.ne.0 .and. iseep.eq.0) then
      IF(NUMRES.ne.0) then
c
c rrb 2008/09/30; Use iall = 0 do all reservoirs, =n do reservoir n only
        iall=0
cx      write(nlog,*) '  Execut; Call SepSec;', iyr, imo, idy, iwx
        CALL SEPSEC(SeepT,iall,'Execut      ')    
cx      iseep=iseep+1
        iw = 0
        iwx=iwx+1
        iwxmaxY=iwxmaxY+1
        iretsw= 0
        ireop= 0
cx      iout=1
        if(iout.eq.1 .or. ichk.eq.9 .or. ichk.eq.109) then
          rec12b='Res Seepage '
          call outIchk(
     1      ichkX, ichk4n, l1, l2, iw, ityopr(l2), ishort, fac, 
     1      uDem, divact2, divx, divsum, 6, SeepT, rec12b)
        endif
        divsum=0.0
cx      endif
cx      iout=0
c__________________________________________________________
c rrb 20100123; OMID Check  
        if(ioutGVC.eq.1) then            
          call outGVC(nlog, 6, divact1, divsum, gvCot,     
     1        noutGVC, fac, -1, 'Res Seepage ', 
     1        -1, 'NA          ',
     1        'NA                      ')
        endif              
c__________________________________________________________
c rrb 2008/09/29; Check
        if(ioutSep.eq.1) then
          iplan1=41
          nr1=18      
          write(nlog,*) ' '
          write(nlog,*) ' Execut inside SepSec '
          write(nlog,*) ' Execut; ', 
     1      nr1, iplan1, pid(iplan1), iyr, mon, 
     1      psuply(iplan1)*fac, psuplyT(iplan1)*fac     
            c=0.0
            write(nlog,270)
          write(nlog,280) '  Execut    ',
     1      iyrmo(mon),xmonam(mon), idy,
     1      cresid(nr1),pid(iplan1),iwx,nr1, iplan1, 
     1      cursto(nr1), cursto(nr1), 
     1      sepact(nr1), c, rlossR(nr1)*fac,
     1      psuply(iplan1)*fac, psuplyT(iplan1)*fac,
     1      pdrive(iplan1)*fac, seepT    
        endif
c__________________________________________________________
c       Return to 135 to allow water rights to benefit
c       from reservoir seepage once per time step
        if(SeepT.gt.small) goto 135
      endif  
c__________________________________________________________
c rrb 2008/09/29; Check
      if(ioutSep.eq.1) then
        iplan1=45
        nr=18      
        write(nlog,*) ' Execut outside SepSec '
        write(nlog,*) ' Execut; ', 
     1    nr, iplan1, pid(iplan1), iyr, mon, 
     1    psuply(iplan1)*fac, PsuplyT(iplan1)*fac
      endif
c__________________________________________________________
c     Task X; Reservoir Evaporation at end of time step 
      IF(NUMRES.ne.0) call evasec
c__________________________________________________________
c     Distribute Evaporation to a Plan
c   Once per time step only
      IF(NUMRES.ne.0 .and. nplan.gt.0) call planeva
c__________________________________________________________
c     Task X; Calculate monthly totals for daily model
c             and print daily results.  
c              Note (0) indicates the routine will sum
c              daily results into monthly totals
      if(iday.eq.1) call daymon(0)
c__________________________________________________________
c rrb 99/10/06; Rio Grande Compact
c               At end of day (month) set index and delivery
c               data to final value and carryover
c
      if(irg1.gt.0.and.l2rgrg.gt.0) 
     1      call rgrg(iw,l2rgrg,1,nrg1,0,1)
      if(irg2.gt.0.and.l2rgco.gt.0) 
     1      call rgrg(iw,l2rgco,2,nrg2,0,1)
c
c rrb Test                
cr                write(nlogx,*) 'Execut; 6 qdiv(14,17)', qdiv(14,17)
     
c
c
c *********************************************************
c               End of Day Loop
c
 1000   continue
c
c __________________________________________________
c               For a daily model set monthly totals back
c               to daily or monthly calculated values
c               Note (1) indicates the routine turns monthly totals
c                 into monthly values
c
       if(iday.eq.1) then
         call daymon(1)
       endif
c
c __________________________________________________
c               Print monthly data to binary files
c       write(6,*) '  Execut; Printing to binary file'
        numstax=maxsta
c
c rrb 10/09/01; Test        
cx        write(nlog,*) '  Execut; Test  ', imo, qdiv(25, 246)*fac,
cx     1    depl(imo,104)*fac           
        CALL OUTMON(numstax)
c
c __________________________________________________
c               Print unique call data
c       write(6,*) '  Execut; Printing unique call data
        CALL outcallR
c
c __________________________________________________
c rrb 01/03/28; Set return values for reuse 
c               Moved from outmon to here.
c               Note daily is set in daymon
        irecx=(imo-1)*nstrtn
        irecx=irecx
        do nr=1,nstrtn
          retur(imo,nr)=0.0
          depl(imo,nr)=0.0
c               irec1=irecx+nr
c               write(78,rec=irec1) 0.0,0.0
        end do
c
c __________________________________________________
c       Set plan obligation values for reuse
        do ip=1,nplan
          pobl(imo,ip)=0.0
          psup(imo,ip)=0.0
        end do
c
c *************************************************
c               End month loop
 1100 CONTINUE
c
c __________________________________________________
c rrb 99/10/06; Rio Grande Compact
c               At end of year set index and delivery
c               data to final value and carryover
c
      if(irg1.gt.0.and.l2rgrg.gt.0) 
     1      call rgrg(iw,l2rgrg,1,nrg1,1,1)
      if(irg2.gt.0.and.l2rgco.gt.0) 
     1      call rgrg(iw,l2rgco,2,nrg2,1,1) 
c
c rrb Test                
cr                write(nlogx,*) 'Execut; 7 qdiv(14,17)', qdiv(14,17)
     
c
c *************************************************
c               End of Year Loop
c
      goto 130
c
c __________________________________________________
c               Close input files
  480 close( 1)
      close( 2)
      close( 3)
      close( 4)
      close(55)
      if(iday.eq.1) then
      close(13)
      close(14)
      close(15)
      close(16)
      endif
c __________________________________________________

      IF(NWRITE.GT.0) CLOSE(24)
c
c __________________________________________________
c
c               Step 44; Print performance data
      write(6,489) iyrmax, xmonam(monmax), idymax, iwxmax
      write(nlogx,489) iyrmax, xmonam(monmax), idymax, iwxmax
      
      IYS=IYSTR
      IYE=IYEND

      c=1.0/float(iyend-iystr+1)/12.0
      write(nlogx,490) ireopx, divchk, divchk*fac,
     1                float(iwxt)*c, float(nrepcalt)*c
 490  format(/,72('_'),/
     1  '  Execut; Performance Info:',/
     1  '    Reoperation switch (ireopx fr *.ctl) = ',i12,/,
     1  '    Reoperation comparison (cfs)         = ',f16.3,/,
     1  '    Reoperation comparison (af/mo)       = ',f16.3,/,
     1  '    Ave reoperations per month           = ',f16.3,/, 
     1  '    Ave calls to replace per month       = ',f16.3)
c
c ---------------------------------------------------------
c
      call dattim(idatx, itim2)

      ctime1x = itim1(1)+itim1(2)/60.+itim1(3)/3600.+itim1(4)/100./3600.
      ctime2x = itim2(1)+itim2(2)/60.+itim2(3)/3600.+itim2(4)/100./3600.
c
c rrb 2020/07/27; Check if the run went past midnight
cx      ctimed = (ctime2x-ctime1x)*60.             
      if(ctime2x .gt. ctime1x) then
        ctimed = (ctime2x-ctime1x)*60.
      else
        ctimed = (ctime2x+24-ctime1x)*60.
      endif
        
c
c rrb 2020/07/27; print time in hours
cx      write(nlogx,500) ctimed*60.0, ctimed*60./float(iye-iys+1),
cx     1                 ctimed, ctimed/float(iye-iys+1)

      riy = float(iye-iys+1)
      write(nlogx,500) ctimed*60.0, ctimed*60.0/riy,
     1                 ctimed,      ctimed/riy,
     1                 ctimed/60.0, ctimed/60.0/riy
c
 500  format(/,72('_'),/
     1 '  Execut; Time to Process:',/
     1 41x, '= ', f12.3,' seconds',/  
     1 41x, '= ', f12.3,' seconds/year',/
     1 41x, '= ', f12.3,' minutes',/
     1 41x, '= ', f12.3,' minutes/year',/
     1 41x, '= ', f12.3,' hours',/
     1 41x, '= ', f12.3,' hours/year')
     
c
c __________________________________________________
c
c               Print standard files if requested
      if(ioptio.ne.8) then
        write(6,491)
        write(nlogx,491)
 491    format(/,72('_'),/
     1   '  Execut; Writing reports')

c
c               Skip over historic files to get *.out or *.xou file
c               Note Daily model has already read *.out 
c
c rrb 01/04/03; Daily model has already opened *.out
        if(iday.eq.0) then
          ioutx=0
c
c rrb 98/12/31; Wells
          if(infile.eq.1) then
            ifn=35
            rec256=fileName(ifn)
            filena=rec256(1:72)

          else
            if(iwell.eq.0) then
              call skip(iin,5)
            else
              call skip(iin,6)
            endif

            filena=' '
            read(iin,'(a72)',err=9994,end=9994) filena
          endif

 9994     if(filena(1:5) .eq. '     ') then
          write(nlogx,102) 
        else
c
c rrb 99/06/20
          call putpath(maxfn, filena, fpath1)
          fileOpen='Output Request File 2 (*.out or *.xou)'
          write(nlogx,101) fileOpen, filena
          write(6,101)
          open(22, file=filena,status='old',err=9995)
          ioutx=1
        endif
      endif

 9995 continue
c     write(nlogx,*)' Execut; calling outres'
      call outres
c     write(nlogx,*)' Execut; calling outdivw'
      call outdivw
c     write(nlogx,*)' Execut; calling outopr'
      call outopr

c
c     write(nlogx,*)' Execut; calling outxss'
      call outxss
c
c ---------------------------------------------------------
c               Instream Flow Reach
      if(ireach.eq.1)  then
c       write(nlogx,*)' Execut; calling outifr' 
        call outifr
      endif  
c
c ---------------------------------------------------------
c               Wells
      if(iwell.ge.1)   then
c       write(nlogx,*)' Execut; calling outwel' 
        call outwel
      endif  
c
c ---------------------------------------------------------
c rrb 99/12/16; Rio Grande Compact
      if(irg1+irg2.ge.1) then
c       write(nlogx,*)' Execut; calling outrg'
        call outrg
      endif  
c
c ---------------------------------------------------------
c              Plans
      if(nplan.gt.0) then
c       write(nlogx,*)' Execut; calling outpln' 
        call outpln 
      endif  
c
c ---------------------------------------------------------
c              WWSP
      if(nplan.gt.0) then
c       write(nlogx,*)' Execut; calling outWW'  
        call outWW  
      endif  
c
c ---------------------------------------------------------
c              JMartin Output
cx    write(nlog,*) '  Execut; ijm ', ijm
      if(ijm.gt.0) then
c       write(nlogx,*)' Execut; calling outJM'  
        call outJM  
      endif  
c      
c
c
c ---------------------------------------------------------
c               Daily
      if(iday.eq.1) then
c        write(nlogx,*)' Execut; calling daydivo' 
         call daydivo
c        write(nlogx,*)' Execut; calling dayreso'
         call dayreso
c
         if(iwell.ge.1) then
c          write(nlogx,*)' Execut; calling daywelo'
           call daywelo
         endif  
      endif
      endif
c
c __________________________________________________
c
c     Print performance and print data
      call dattim(idatx, itim2)
c     write(nlogx,'(4i2)') (itim2(j),j=1,4)

      ctime1x = itim1(1)+itim1(2)/60.+itim1(3)/3600.+itim1(4)/100./3600.
      ctime2x = itim2(1)+itim2(2)/60.+itim2(3)/3600.+itim2(4)/100./3600.
     
c
c rrb 2020/07/27; Check if the run went past midnight
cx      ctimed = (ctime2x-ctime1x)*60.             
      if(ctime2x .gt. ctime1x) then
        ctimed = (ctime2x-ctime1x)*60.
      else
        ctimed = (ctime2x+24-ctime1x)*60.
      endif   
c
c
c __________________________________________________
c
c     Print output files to screen
      do i=1,2
        if(i.eq.1) nf=6
        if(i.eq.2) nf=nlog
        write(nf,503)
 503    format(/,72('_'),/ 
     1  '  Execut; Successful Run output files are:')
        if(ioptio.ne.8) then                       
        write(nf,*) ' '
        write(nf,*)   ' Diversion output:             *.xdd'
        write(nf,*)   ' Reservoir output:             *.xre'
        write(nf,*)   ' Operating Rule Info:          *.xop'
        write(nf,*)   ' Instream Reach Info:          *.xir'
        write(nf,*)   ' Structure Summary:            *.xss'  
        write(nf,*)   ' Call (Control) Summary:       *.xca'  
                                                            
        if(iwell.ge.1) then                           
          write(nf,*) ' Well output:                  *.xwe'
        endif                                         
                                                            
        if(isjrip.ge.1) then                          
          write(nf,*) ' SJRIP Output:                 *.xsj'
        endif                                         
                                                        
        if(nplan.ge.1) then                           
          write(nf,*) ' Plan Output:                  *.xpl'          
        endif  
                                               
        if(nplan.ge.1) then                           
          write(nf,*) ' WWSP Output:                  *.xww'          
        endif                                         
c
c rrb 2018/11/18; JMartin output
        if(ijm.ge.1) then
          write(nf,*) ' JMartin Output;               *.xjm'
        endif
                                                              
        if(irepn.ge.1) then                           
          write(nf,*) ' Replacement Reservoir Output: *.xrp'
        endif                                         
                                                      
        if(irg1+irg2.ge.1)                            
     1      write(nf,*) ' Rio Grande Compact Info:      *.xrg' 
c
c ---------------------------------------------------------
c   Daily           
        if(iday.eq.1) then
        write(nf,*) ' Daily Diversion output:       *.xdy'
        write(nf,*) ' Daily Reservoir output:       *.xry'
        if(iwell.ge.1)                              
     1         write(nf,*) ' Daily Well output:            *.xwy'
        if(ichk.eq.8) 
     1         write(nf,*) ' Daily Naturalized Streamflow  *.xtp'
        write(nf,*) ' Note, other daily data not available'
      endif
        else
      write(nf,*) ' '
      write(nf,*) ' No standard reports generated'
          write(nf,*) ' To get them run report -xst'
      endif
      end do
      
      if(ideplete.eq.1) write(nlog,9000)
      if(iexchang.eq.1) write(nlog,9002)
      if(ideplete.eq.1 .or. iexchang.eq.1) write(nlog,9004)
c
c __________________________________________________
c
c   Close Files
      close(9)
      close(10)
      close(12)
      close(14)
      close(15)
      close(20)
c     close(23)
      close(24)
c
c rrb 00/03/06; Monthly ISF               
      close(25)
      close(31)
      close(36)
      close(37)
      close(41)
      close(42)
      close(43)
      close(44)
      close(45)
      close(46)
      close(47)
      close(48)
      close(49)
      close(50)
      close(51)
      close(52)
      close(53)
      close(55)
      close(65)
      close(67)
      close(77)
c
c __________________________________________________
c
c   Print time data
c
c rrb 2020/07/27; Print 
cx      write(nlogx,501) ctimed*60.0, ctimed*60./float(iye-iys+1),
cx     1                 ctimed,      ctimed/float(iye-iys+1)

      riy = float(iye-iys+1)
      write(nlogx,501) ctimed*60.0, ctimed*60.0/riy,
     1                 ctimed,      ctimed/riy,
     1                 ctimed/60.0, ctimed/60.0/riy

 501  format(/,72('_'),/
     1'  Execut; Time to Process & Report:',/,
     1 41x, '= ', f15.3,' seconds',/  
     1 41x, '= ', f15.3,' seconds/year',/
     1 41x, '= ', f15.3,' minutes',/
     1 41x, '= ', f15.3,' minutes/year',/
     1 41x, '= ', f15.3,' hours',/
     1 41x, '= ', f15.3,' hours/year')
      
      write(nlogx,9991)
      write(6,9991)
 9991 format(/,72('_'),/ '  Execut; Successful Termination')
c
c __________________________________________________
c
c   Return
      RETURN
c
c __________________________________________________
c
c     Formats
c
c_______________________________________________________________________
c   Exit with an error
 9999 write(6,9992)  
      write(nlogx,9992)
 9992 format(/,72('_'),/, '  Execut; Stopped in Execut')
      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)
      stop
c
c_______________________________________________________________________
c               Formats
 101  format(/,72('_'),/
     1 '  Execut; ', a40, /
     1 5x, a72)

 102  format(/,72('_'),/'  Execut; ',
     1 'No *.out file found, will print all stations')
     
 103   format(
     1   '+', ' Execut; Year ', i5, ' Month ', a4, ' Day ', i3,
     1   ' Reoperation ', i5, ' Annual Maximum ', i5)
     
c
c rrb 2021/04/18; Miscellaneous updates to compile without warnings
cx 104   format(
cx     1   '+', ' Execut; Year ', i5, ' Month ', a4, ' Day ', i3,
cx     1   ' Reoperation ', i5, ' Annual Total ', i5)
     
     
 106   format(
     1   '+', ' Execut;  Year ', i5, ' Month ', a4)  
 

     
 188  format('  Execut; Downstream call before a ', a3,i5, 1x, a4, i3,/
     1       '          DS call admin # = ', f15.5,/
     1       '          Right admin #   = ', f15.5)
     
 270  format(/,      
     1  '  Execut      iyr mon   day Res ID       Plan ID     ',
     1  '  iwx   nr ipln',
     1  '   CurSto1   CurSto2  Tot Seep   Tot Ret  Tot Loss',
     1  '    Psuply   PsuplyT    Pdrive     SeepT',/
     1  ' ___________ ____ ____ ____ ____________ ____________',
     1  ' ____ ____ ____',
     1  ' _________ _________ _________ _________ _________',
     1  ' _________ _________ _________ _________')
 280  format(a12, i5, 1x, a4,i5, 1x,a12, 1x,a12, 3i5, 20f10.1)     
      
 489  format(/,72('_'),/
     1  '  Execut; On Year ', i5, ' Month ', a4, ' Day ', i4,/
     1  '          The maximum number of reoperations  ', i5)
     
c
c rrb 2021/04/18; Miscellaneous updates to compile without warnings
cx
cx 520  format(/,72('_'),/
cx     1 ' Execut; ', i5, 1x, a4, i3, 
cx     1 ' Reoperation by a ', a12,
cx     1 ' Opr Type = ',i2,' Reoperation = ', i4,' Diversion = ', f10.2,
cx     1 ' Opr_ID = ', a12)
c
c rrb 2021/04/18; Miscellaneous updates to compile without warnings   
cx 523  format(/,72('_'),/
cx     1 '  Execut;  Reoperation Report (ichk=9) for = ',i5,1x, a4,i3,//
cx     1 '                                                     ',
cx     1 '                               ',
cx     1 '   Change Value      Check Sum    Check Value',/
cx     1 ' Year Mon Day Reop   l2 Reason       Opr Type Right ID    ',
cx     1 ' Right Name               ',
cx     1 '     af/mo     af/mo     af/mo',
cx     1 '    721239    720645    950003    950006',
cx     1 '  ShortGVC    GVC',/     
cx     1 ' ____ ___ ___ ____ ____ ____________ ________ ____________',
cx     1 ' ________________________ ',
cx     1 9(' _________'))

 540  format(/, 72('_'),/,'  Execut; Year and month = ',i5, 1x, a4)
 550  format(/,72('_'),/ 
     1 '  Execut; Problem ',/
     1 '          Operating right number = ', i5, /
     1 '          It must be greater than 0 and less than ', i5)
     
 560  format(/,'  Execut; ',
     1   '       # ID                l2      nd   irepn ireptyp',
     1   '  ishort   rdvnk reprnkx')

 570  format(10x, i8,1x,a12,5i8, 20f8.2)
 
 
 580  format(              
     1         '  Execut; Note at time ', i5,1x, a4, i5, /
     1         10x,'The downstream call (Type 23) operating right',/
     1         10x,'is being called at operating rule priority = ',
     1         f15.5, /
     1         10x,'not the priority in the call file = ',f15.5,/
     1         10x,'Probably OK if a free river')
     

                 
 590  format(/,
     1   '  Execut; SPlatte     # cdivid      ',
     1   '    isp1      nd    iuse  ishort  divmon  divreq',/
     1   ' _________________ ____ ____________',
     1   ' _______ _______ _______ _______ _______ _______')

 592  format(
     1   '  Execut; SPlatte ', i5, 1x, a12, 4i8, 20f8.0)
 
 9000 format(/,72('_'),/,
     1 '  Execut; Warning at least one EXCHANGE or BYPASS ',/
     1 '          (Type 24 or 25) Operating rule was specified.')

 9002 format(/,72('_'),/,
     1 '  Execut; Warning at least one operating rule specified a ',/
     1 '          Plan or reservoir release to meet a structures ',/
     1 '          DEPLETION not its DIVERSION (Types 27,28,32 or 33)')

 9004 format(/,72('_'),/,
     1 '  Execut; These activities typically require an engineering',/
     1 '          analysis to insure no injury occurs. Note:',/
     1 '          1. StateMod can provide an estimate of the',/
     1 '             associated Terms and Conditions and weather they',/
     1 '             are satisfied in the plan output file (*.xpl).',/ 
     1 '          2. StateMod assumes the user is applying these',/
     1 '             rules properly because this information is often',/
     1 '             negotiated from historic ditch operations')

 9996 write(nlogx,9993)
 9993 format(/,72('_'),/ 
     1 '  Execut; Problem daily model requires a *.out file')
      goto 9999

 9997 write(6,9998) filena
      write(nlogx,9998) filena
 9998 format(/,72('_'),/
     1 '  Execut; Problem opening file: ', a72)
      goto 9999
     
      goto 9999
      END
