c outpln - prints plan data
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
      SUBROUTINE OutPln
c
c
c _________________________________________________________
c	Program Description
c
c       Outpln; It prints plan data
c
c _________________________________________________________
c       Update History
c
c rrb 2021/04/18; Compiler warning
c
c rrb 2019/04/20; Revised to recognize a WWSP Supply plan is a
c                   type 14 and a WWSP User Plan is a type 15
c rrb 2018/08/05; Revise to allow a WWSP Plan or User (type 14) 
c                   tied to a reservoir
c
c _________________________________________________________
c
c       Documentation
c		iop     = counter for use and source output
c		iops    = counter for storage
c		maxTC   = max for type 1 T&C (up to failure)
c		maxAug  = max for type 2 WEll Agumentation
c		maxResP = max for type 3 and 5 Res
c		maxResPX = max for type 4 and 6 Non Res
c		maxRch  = max for type 8 recharge
c		maxAll  = max for any type
c
c		iPtotal(i)= code to print summary for plan type i
c		PlanTot(i,j,k)= total to plan type i, year j, 
c			        use or source k
c
c _________________________________________________________
c	Dimension
c
      include 'common.inc'
      character ftype*24, parType*24, cfail1*3, cfail2*3, cfail2T*3,
     1          copOff*3, rec12*12, rec24*24, ciy*4
      dimension 
     1  pfailx(maxplan), planTot(maxplnT,maxyrs,40), 
     1  iptotal(40),     datBeg(maxPlan)
c _________________________________________________________
c
c              Step 1; Initialize 
c                                        
c rrb 2021/04/18; Compiler warning 
      ciy=' '
      ftype=' '
      partype=' '     
      iprob=0                    
      if(iprob.gt.0) goto 9999   

c		
c		iout = 1 detailed output
c          2 summary of print arrays
c		ioutN= plan pointer for detailed output 

      iout = 0
c     ioutN=52

      rec12=' '
      rec24=' '

      write(6,101) 'OutPln  '
      write(nlog,101) 'OutPln  '
 101  format(/,72('_'),/'  Subroutine ', a8)

c
      small=0.001
c
c		Set Max for various plan types
c rrb 2006/09/28; Revise to allow 20 columns of output
cr    maxTc=13
cr    maxAug=9
cr    maxResP=12
cr    maxResPX=12
            
      maxTc=28
      maxAug=24
      maxResP=22
      maxResPX=22      
      maxRch=23
c
c rrb 2021/04/18; Compiler warning
cx    maxAll=amax0(maxTC, maxAug, maxResP, maxResPX)
      maxAll=max(maxTC, maxAug, maxResP, maxResPX)
       
      do np=1,nplan
        pfailx(np)=0.0
      end do
c
c		Initialize total per plan type      
      do np=1,maxPlnT
        do iy=1,100
          do i=1,40
            planTot(np,iy,i)=0.0
            ipTotal(i)=0
          end do
        end do
      end do

c ---------------------------------------------------------
c		            Detailed Output of print arrays when iout.gt.1
c                 (same as available in SetPlanO.for)

      if(iout.ge.1) then
        do np=1,nplan
          write(nlog,400) np, pid(np), iplntyp(np), plntypC(np), maxplnU
          write(nlog,410) (i, i=1,60)
          
          write(nlog,420) 'iplnoprE', (iplnoprE(np,nop), nop=1,maxplnU)
          write(nlog,420) 'iplnoprS', (iplnoprS(np,nop), nop=1,maxplnU)
          write(nlog,420) 'iplnoprR', (iplnoprR(np,nop), nop=1,maxplnU)
          write(nlog,420) 'iplnoprO', (iplnoprO(np,nop), nop=1,maxplnU)
          write(nlog,420) 'iplnoprU', (iplnoprU(np,nop), nop=1,maxplnU)            
          write(nlog,420) 'iplnoprP', (iplnoprP(np,nop), nop=1,maxplnU) 
        end do
      endif        
      
c
c _________________________________________________________
c
c               Step 2; Print Banner
      call outtop(21, 1, 50)
c     write(nlog,*) '  OutPln;  nplan, numopr',  nplan, numopr
c
c _________________________________________________________ 
c
c               Step 4b. Process each plan
c		            Concept for every plan, find uses by scanning 
c               the operation right file.
       
        if(iout.eq.1) write(nlog,*) ' Outpln; nplan', nplan
        
        do 170 np=1,nplan
        
          if(iout.eq.1) write(nlog,*) ' Outpln; np', np
 
          is=ipsta(np)       
          cfail1='Off'
          if(ipfail(np).eq.1) cfail1 = ' On'
c
c rrb 2009/05/26; Correction
          DatBeg(np)=0          
c
c _________________________________________________________ 
c
c               Step 4c; Print Header 
c
c rb 2011/07/28; print the location to the header
c smalers 2017-11-07 Check to avoid invalid array index
c
c
c rrb 2017/12/11; Correction variable is is set in getpln
cx        if (is.gt.0) then
            write(21,300) cunitm, np, iplntyp(np),plntypC(np),Pid(np),
     1        Pname1(np), Psource(np), cstaid(is)
cx        endif
     
          iop=0
          iops=0
          kUP=0
c
c rrb 2006/08/08; Evap depends on the operating rule type          
cr        kE=iplnoprE(np)
c
c		Find other sources by scanning the Operating rule file 
c		Note iplnoprS, etc are defined at bottom of Oprinp.f        
       
c
cr        write(nlog,*) ' Outpln; maxplnU', maxplnU
          do nop=1,maxplnU          
            copOff=' On'
            if(ioprsw(nop).eq.0) copOff='Off'
c
c rrb 2006/08/08; Evap depends on the operating rule type                      
            kE=iplnoprE(np,nop)              
            kS=iplnoprS(np,nop)
            kR=iplnoprR(np,nop)
            kU=iplnoprU(np,nop)
            kO=iplnoprO(np,nop)
            kP=iplnoprP(np,nop)
            
c           if(iout.eq.1 .and. ioutN.eq.52) then
            if(iout.eq.1) then
              write(nlog,*) 
     1         ' OutPln;   np   nop   ke   ks   kr   ku   ko   kp'
              write(nlog,'(a10,20i5)') 
     1          ' OutPln; ',  np, nop, ke, ks, kr, ku, ko,kp
            endif  
c
c
c ---------------------------------------------------------
c
c rrb 2006/03/31; Print Source for a Well Augmentation Plan (-1)
            if(kS.eq.-1) then
              iop=iop+1      
              copOff=' On'
c
c rrb 2008/10/07; Print decree weighted priority              
cx            write(21,301) 'Src  ',iop, 'In_Priority ', -1.0,
              write(21,301) 'Src  ',iop, 'In_Priority ', rdvnkwp(np),
     1          'In_Priority_Current_Time ',-1,
     1          'In_Priority ', copOff
            endif
      
c
c ---------------------------------------------------------
c
c		Print Source T&C Plan Destination is a reuse           
c rrb 2006/03/23; Well Augmentation
            if(kS.gt.0) then
              iop=iop+1                  
              
              copOff=' On'              
              if(ioprsw(ks).eq.0) copOff='Off'  
              if(ityopr(ks).ne.43) then                        
                write(21,301) 'Src  ',iop, corid(kS), ropnk(kS),
     1            nameo(kS), ityopr(kS), ciopsoX(1,ks), copOff
              else
                write(21,301) 'Src  ',iop, corid(kS), ropnk(kS),
     1            nameo(kS),  ityopr(kS), 'In_Priority ', copOff 
              endif
            endif
c
c ---------------------------------------------------------
c
c		Print re diversion amount             
            if(kR.gt.0) then
              iop=iop+1   
              
              copOff=' On'                             
              if(ioprsw(kr).eq.0) copOff='Off'                                        
              write(21,301) 'Store',iop,corid(kR), ropnk(kR),
     1         nameo(kR),ityopr(kR), ciopsoX(1,ks), copOff
            endif
c
c ---------------------------------------------------------
c
c		Source is a Non reservoir
c               Set Use based on opr right source 1 or 2
            if(kU.gt.0 .and. kP.eq.0) then
              iop=iop+1   
              
              copOff=' On'                             
              if(ioprsw(ku).eq.0) copOff='Off'                                        
              write(21,303) 'Use  ', iop,  corid(kU), ropnk(kU),
     1          nameo(kU), ityopr(kU), ciopdeX(1,kU), copOff
            endif     
c
c ---------------------------------------------------------
c
c		Source is a Plan
c               Set Use based on opr right source 1 or 2
            if(kU.gt.0 .and. kP.gt.0) then
              iop=iop+1   
c
c rrb 2019/04/29; Test
cx            kUP=kUP+1
              if(kUP.eq.0) then
                kUP=1
              else
                kUP=kUp+2
              endif
              
              copOff=' On'                             
c             write(nlog,*) ' OutPln; kUP ', kUP
              if(ioprsw(ku).eq.0) copOff='Off'                                        
              write(21,303) 'Use  ', iop,  corid(kU),  ropnk(kU),
     1          nameo(kU), ityopr(kU), ciopdeX(kUP,kU), copOff
            endif     
            
c
c ---------------------------------------------------------
c
c		Out of Priority Storage
            if(kO.gt.0) then
              iop=iop+1                 
              copOff=' On'                             
              if(ioprsw(kO).eq.0) copOff='Off'                                        
              write(21,303) 'Src  ', iop, corid(kO), ropnk(kO),
     1          nameo(kO),  ityopr(kO), 'Various', copOff
            endif     
c
c ---------------------------------------------------------
c
c rrb 2005/11/08; Print evaporation as the last use
c
            if(kE.gt.0) then
              copOff=' On'          
              iop=iop+1
              rec12(1:2) ='NA'
              rec24(1:2) = 'NA'
              write(21,303) 'Use  ', iop, rec12,  -1.0,
     1          'Net_Evap    ', ityopr(kE),  rec12, copOff
            endif
c
c ---------------------------------------------------------
c
c		End Source and Use Loop           
          end do    
c
c
c _________________________________________________________ 
c
c               Step 5; Year Loop
          if(iout.eq.1) then
            write(nlog,*) ' OutPln; iystr, iyend ', iystr, iyend
          endif
          
          iy1=0
          DO IY=IYSTR,IYEND
            iy1=iy1+1
            cfail2T = 'Off'
            pfailT  = 0
            
            call year(iy, iyrmo, imomo, cyr1)
            do i=1,40
              dat2T(i)=0.0
            end do

c
c
c _________________________________________________________ 
c
c               Step 6; Print annual title     
c
c ---------------------------------------------------------
c		a. Standard T&C Output
            if(iplntyp(np).eq.1) then
              write(21,230) (i, i=1,maxTC+3)
            endif
c
c ---------------------------------------------------------
c
c rrb 2006/03/23; Well Augmentation          
c		b. Well Augmentation     
c rrb 2006/12/19; Type 10 is a Special Well Augmentation 
c                 (e.g. Designated Basin, Coffin Well, etc.)
c           if(iplntyp(np).eq.2) then
            if(iplntyp(np).eq.2 .or. iplntyp(np).eq.10) then
              write(21,231) (i, i=1,maxAug)
            endif
c
c ---------------------------------------------------------
c		c. Reservoir Plans            
            if(iplntyp(np).eq.3 .or. iplntyp(np).eq.5) then            
              write(21,233) (i, i=1,maxResP+2)
            endif    
c
c ---------------------------------------------------------
c
c		d. Non reservoir and Accounting and Changed Water Right plans
c
c rrb 2015/03/07; Allow a Changed Water Right Plan (type 13)   
cx   1         iplntyp(np).eq.11) then         
            if(iplntyp(np).eq.4  .or. iplntyp(np).eq.6 .or. 
     1         iplntyp(np).eq.11 .or. iplntyp(np).eq.13) then
              write(21,232) ' Supply', 'Total', (i, i=1,maxResPX)
            endif    
c
c ---------------------------------------------------------
c
c		e. Transmountain Import            
            if(iplntyp(np).eq.7) then
              write(21,232) ' Supply', 'Total', (i, i=1,maxResPX)
            endif    
c
c ---------------------------------------------------------
c
c		f. Recharge Plan 
            if(iplntyp(np).eq.8) then
              write(21,235) (i, i=1,maxRch)
            endif    
c
c ---------------------------------------------------------
c
c		g. OOP Plans (like a reservoir but driven by demand
c		   not supply)             
            if(iplntyp(np).eq.9) then
              write(21,234) (i, i=1,maxResP+2)
            endif    
c
c ---------------------------------------------------------
c
c		h. Release Limit
            if(iplntyp(np).eq.12) then
              write(21,232) 'Release', 'Limit', (i, i=1,maxResPX)
            endif    
c
c ---------------------------------------------------------
c
c rrb 2018/08/28; Allow type 14
c		i. WWSP Supply Plan (type 14)
            if(iplntyp(np).eq.14) then
              write(21,236) (i, i=1,maxResPX+2)            
            endif               
c
c rrb 2019/04/20; Allow type 15
c		i. WWSP User Plan (type 15)
            if(iplntyp(np).eq.15) then
              write(21,236) (i, i=1,maxResPX+2)            
            endif               
cc
c _________________________________________________________
c
c		Step 7; Month Loop              
            DO IM=1,12
c
c rrb 2021/04/18; Compiler warning
cx            imx=amax0(1, im-1)
              imx=max(1, im-1)
              
              do i=1,40
                dat2(i)=0.0
              end do
c
              fac=fmo(im)              
c
c _________________________________________________________
c
c		Step 8; Read Binary File
              irec1=((iy-iystr0)*12*nplan)+((im-1)*(nplan))+np
              ip1=iplntyp(np)
c
c rrb 2020/08/31; Check
              if(ip1.lt. 1) then
                write(nlog,*) '  Outpln; Problem ip1 = 0'
                write(nlog,*) '  Outpln; np, pid(np)', np, pid(np)
              endif
              
              ipTotal(ip1)=1
c
c ---------------------------------------------------------
c		a. Standard T&C Output             
              if(iplntyp(np).eq.1) then
c smalers 2017-11-07 Check to avoid invalid array index
                if (is.gt.0) then
                  read(68,rec=irec1) pid(np), cstaid(is),
     1             iyrmo(im), xmonam(im), (dat2(i), i=1,maxTC), 
     1             cfail1, cfail2, pfail(np) 
                endif
              endif
c
c ---------------------------------------------------------
c		b. Well Augmentation plan Output              
c rrb 2006/12/19; Type 10 is a Special Well Augmentation 
c                 (e.g. Designated Basin, Coffin Well, etc.)
c             if(iplntyp(np).eq.2) then
              if(iplntyp(np).eq.2 .or. iplntyp(np).eq.10) then
c smalers 2017-11-07 Check to avoid invalid array index
                if (is.gt.0) then
                  read(68,rec=irec1) pid(np), cstaid(is),
     1              iyrmo(im), xmonam(im), (dat2(i), i=1,maxAug), 
     1              cfail1, cfail2, pfail(np) 
                endif
              endif
c
c ---------------------------------------------------------
c		c. Reservoir Plans  
c
c rrb 2018/08/05; Add WWSP Supply Plan (type 14) 
c rrb 2019/04/20; Add WWSP User Plan (type 15)         
cx            if(iplntyp(np).eq.3 .or. iplntyp(np).eq.5 .or.
cx   1           iplntyp(np).eq.9) then                   
cx            if(iplntyp(np).eq.3 .or. iplntyp(np).eq.5 .or.
cx   1           iplntyp(np).eq.9 .or. iplntyp(np).eq.14) then                   
              if(iplntyp(np).eq.3 .or. iplntyp(np).eq.5 .or.
     1           iplntyp(np).eq.9 .or. iplntyp(np).eq.14 .or.
     1           iplntyp(np).eq.15) then                   
c smalers 2017-11-07 Check to avoid invalid array index
                if (is.gt.0) then
                  read(68,rec=irec1) pid(np), cstaid(is),
     1            iyrmo(im), xmonam(im), (dat2(i), i=1,maxResP), 
     1            psto1X, psto2X, pfail(np)                  
                endif
              endif
c
c ---------------------------------------------------------
c
c		d. Non reservoir and Transmountain
c
c rrb 2015/03/07; Allow a Changed Water Right Plan (type 13)   
cx   1          iplntyp(np).eq.12) then 
             if(iplntyp(np).eq.4 .or. iplntyp(np).eq.6 .or.
     1          iplntyp(np).eq.7 .or. iplntyp(np).eq.11.or.
     1          iplntyp(np).eq.12.or. iplntyp(np).eq.13) then           
c smalers 2017-11-07 Check to avoid invalid array index
                if (is.gt.0) then
                  read(68,rec=irec1) pid(np), cstaid(is),
     1            iyrmo(im), xmonam(im), (dat2(i), i=1,maxResP), 
     1            psto1X, psto2X, pfail(np) 
                endif
              endif
c
c ---------------------------------------------------------
c		e. Recharge              
              if(iplntyp(np).eq.8) then
c smalers 2017-11-07 Check to avoid invalid array index
                if (is.gt.0) then
                  read(68,rec=irec1) pid(np), cstaid(is),
     1              iyrmo(im), xmonam(im), (dat2(i), i=1,maxRch), 
     1              psto1X, psto2X, pfail(np) 
                endif
              endif
c
c ---------------------------------------------------------
c
c		f. Detailed output     
              if(iout.eq.1) write(nlog,*) '  OutPln; np,', np, 
     1          (dat2(i)*fac, i=1,maxAll)
c     
c
c _________________________________________________________
c
c		Step 9; Calculate annual total and plan total
              do i=1,maxAll
                  dat2T(i) = dat2T(i) + dat2(i)*fac
                  planTot(ip1,iy1,i)=planTot(ip1,iy1,i) + dat2(i)*fac                
              end do  
c
c rrb 2009/01/23; Revise to keep the maximum		              
cx            if(iplntyp(np).eq.12 .and. im.eq.1) datBeg(np)=dat2(1)*fac
              if(iplntyp(np).eq.12 .and. im.eq.1) then
                datBeg(np)=dat2(1)*fac
              else
                datBeg(np)=amax1(datBeg(np), dat2(1)*fac)
              endif             
              
              cfail2 = 'Off'
              if(ipfail(np).gt.0 .and. pfailX(np) .gt. small) then
                cfail2 = ' On'
                cfail2T = ' On'
                pfailT  = pfailT + pfail(np)
              endif  
c
c		Set failure value for next month based on this month
              pfailX(np)=pfail(np)
c _________________________________________________________
c
c		Step 10; Print Monthly results
c
c ---------------------------------------------------------
c		a. Standard T&C Output
              if(iplntyp(np).eq.1) then
                if(isigfig.eq.0) then
c smalers 2017-11-07 Check to avoid invalid array index
                  if (is.gt.0) then
                    write(21,240) pid(np), cstaid(is),
     1                iyrmo(im), xmonam(im), (dat2(i)*fac, i=1,maxTC), 
     1                cfail1, cfail2, pfail(np) 
                  endif
                endif
                
                if(isigfig.eq.1) then
c smalers 2017-11-07 Check to avoid invalid array index
                  if (is.gt.0) then
                    write(21,2401) pid(np), cstaid(is),
     1                iyrmo(im), xmonam(im), (dat2(i)*fac, i=1,maxTC), 
     1                cfail1, cfail2, pfail(np) 
                  endif
                endif
                
                if(isigfig.eq.2) then
c smalers 2017-11-07 Check to avoid invalid array index
                  if (is.gt.0) then
                    write(21,2402) pid(np), cstaid(is),
     1                iyrmo(im), xmonam(im), (dat2(i)*fac, i=1,maxTC),
     1                cfail1, cfail2, pfail(np)
                  endif
                endif
              endif
c
c ---------------------------------------------------------
c		b. Well Augmentation plan Output
c rrb 2006/12/19; Type 10 is a Special Well Augmentation 
c                 (e.g. Designated Basin, Coffin Well, etc.)
              if(iplntyp(np).eq.2 .or. iplntyp(np).eq.10) then
                if(isigfig.eq.0) then
c smalers 2017-11-07 Check to avoid invalid array index
                  if (is.gt.0) then
                    write(21,244) pid(np), cstaid(is),
     1              iyrmo(im), xmonam(im), (dat2(i)*fac, i=1,maxAug)
                  endif
                endif
                
                if(isigfig.eq.1) then
c smalers 2017-11-07 Check to avoid invalid array index
                  if (is.gt.0) then
                    write(21,2441) pid(np), cstaid(is),
     1              iyrmo(im), xmonam(im), (dat2(i)*fac, i=1,maxAug)
                  endif
                endif
                
                if(isigfig.eq.2) then
c smalers 2017-11-07 Check to avoid invalid array index
                  if (is.gt.0) then
                    write(21,2442) pid(np), cstaid(is),
     1              iyrmo(im), xmonam(im), (dat2(i)*fac, i=1,maxAug)
                  endif
                endif
              endif  
c
c ---------------------------------------------------------
c		c. Reservoir Output   
c rrb 2018/08/05; Add WWSP (type 14)     
cx            if(iplntyp(np).eq.3 .or. iplntyp(np).eq.5 .or.
cx   1           iplntyp(np).eq.9) then 
c
c rrb 2019/04/20; Revise to allow WWSP Supply Plan (type 14) and
c                 WWSP User Plan (type 15)     
cx            if(iplntyp(np).eq.3 .or. iplntyp(np).eq.5 .or.
cx   1           iplntyp(np).eq.9 .or. iplntyp(np).eq.14) then      
              if(iplntyp(np).eq.3 .or. iplntyp(np).eq.5  .or.
     1           iplntyp(np).eq.9 .or. iplntyp(np).eq.14 .or.
     1           iplntyp(np).eq.15) then      
     
                if(isigfig.eq.0) then
c smalers 2017-11-07 Check to avoid invalid array index
                  if (is.gt.0) then
                    write(21,242) pid(np), cstaid(is),
     1              iyrmo(im), xmonam(im), psto1X*fac, 
     1              (dat2(i)*fac, i=1,maxResP), psto2X*fac
                  endif
                endif
                
                if(isigfig.eq.1) then
c smalers 2017-11-07 Check to avoid invalid array index
                  if (is.gt.0) then
                    write(21,2421) pid(np), cstaid(is),
     1              iyrmo(im), xmonam(im), psto1X*fac,
     1              (dat2(i)*fac, i=1,maxResP), psto2X*fac
                  endif
                endif
                
                if(isigfig.eq.2) then
c smalers 2017-11-07 Check to avoid invalid array index
                  if (is.gt.0) then
                    write(21,2422) pid(np), cstaid(is),
     1              iyrmo(im), xmonam(im), psto1X*fac,
     1              (dat2(i)*fac, i=1,maxResP), psto2X*fac
                  endif
                endif
              endif
c
c ---------------------------------------------------------
c		d. Non Reservoir Output        
c
c rrb 2015/03/07; Allow a Changed Water Right Plan (type 13)   
cx   1          iplntyp(np).eq.12) then 
              if(iplntyp(np).eq. 4 .or. iplntyp(np).eq.6 .or.
     1           iplntyp(np).eq. 7 .or. iplntyp(np).eq.11.or.
     1           iplntyp(np).eq.12 .or. iplntyp(np).eq.13) then      
     
                if(isigfig.eq.0) then
c smalers 2017-11-07 Check to avoid invalid array index
                  if (is.gt.0) then
                    write(21,242) pid(np), cstaid(is),
     1              iyrmo(im), xmonam(im), (dat2(i)*fac, i=1,maxResPX)
                  endif
                endif
                
                if(isigfig.eq.1) then
c smalers 2017-11-07 Check to avoid invalid array index
                  if (is.gt.0) then
                    write(21,2421) pid(np), cstaid(is),
     1              iyrmo(im), xmonam(im), (dat2(i)*fac, i=1,maxResPX)
                  endif
                endif
     
                if(isigfig.eq.2) then
c smalers 2017-11-07 Check to avoid invalid array index
                  if (is.gt.0) then
                    write(21,2422) pid(np), cstaid(is),
     1              iyrmo(im), xmonam(im), (dat2(i)*fac, i=1,maxResPX)
                  endif
                endif
              endif
c
c ---------------------------------------------------------
c		e. Recharge
              if(iplntyp(np).eq. 8) then      
              
                if(isigfig.eq.0) then
c smalers 2017-11-07 Check to avoid invalid array index
                  if (is.gt.0) then
                    write(21,242) pid(np), cstaid(is),
     1              iyrmo(im), xmonam(im), (dat2(i)*fac, i=1,maxRch)
                  endif
                endif
                
                if(isigfig.eq.1) then
c smalers 2017-11-07 Check to avoid invalid array index
                  if (is.gt.0) then
                    write(21,2421) pid(np), cstaid(is),
     1              iyrmo(im), xmonam(im), (dat2(i)*fac, i=1,maxRch)
                  endif
                endif
              
                if(isigfig.eq.2) then
c smalers 2017-11-07 Check to avoid invalid array index
                  if (is.gt.0) then
                    write(21,2422) pid(np), cstaid(is),
     1              iyrmo(im), xmonam(im), (dat2(i)*fac, i=1,maxRch)
                  endif
                endif
              endif
c
c ---------------------------------------------------------
c               End month loop                                          
            end do
c
c _________________________________________________________
c
c		Step 11; Write annual total            
c
c ---------------------------------------------------------
c		a. Standard T&C
            if(iplntyp(np).eq.1) then
              write(21,250)
              if(isigfig.eq.0) then
c smalers 2017-11-07 Check to avoid invalid array index
                if (is.gt.0) then
                  write(21,240) pid(np), cstaid(is),
     1            iyrmo(13), xmonam(13), (dat2T(i), i=1,maxTC),
     1            cfail1, cfail2T, pfail(np)            
                endif
              endif
              
              if(isigfig.eq.1) then
c smalers 2017-11-07 Check to avoid invalid array index
                if (is.gt.0) then
                  write(21,2401) pid(np), cstaid(is),
     1            iyrmo(13), xmonam(13), (dat2T(i), i=1,maxTC),
     1            cfail1, cfail2T, pfail(np)            
                endif
              endif
              
              if(isigfig.eq.2) then
c smalers 2017-11-07 Check to avoid invalid array index
                if (is.gt.0) then
                  write(21,2402) pid(np), cstaid(is),
     1            iyrmo(13), xmonam(13), (dat2T(i), i=1,maxTC),
     1            cfail1, cfail2T, pfail(np)            
                endif
              endif
            endif  
c
c ---------------------------------------------------------
c 		b. Well Augmentation T&C
c rrb 2006/12/19; Type 10 is a Special Well Augmentation 
c                 (e.g. Designated Basin, Coffin Well, etc.)
c           if(iplntyp(np).eq.2) then
            if(iplntyp(np).eq.2 .or. iplntyp(np).eq.10) then
              write(21,251)
              if(isigfig.eq.0) then
c smalers 2017-11-07 Check to avoid invalid array index
                if (is.gt.0) then
                  write(21,240) pid(np), cstaid(is),
     1            iyrmo(13), xmonam(13), (dat2T(i), i=1,maxAug)
                endif
              endif
              
              if(isigfig.eq.1) then
c smalers 2017-11-07 Check to avoid invalid array index
                if (is.gt.0) then
                  write(21,2401) pid(np), cstaid(is),
     1            iyrmo(13), xmonam(13), (dat2T(i), i=1,maxAug)
                endif
              endif
              
              if(isigfig.eq.2) then
c smalers 2017-11-07 Check to avoid invalid array index
                if (is.gt.0) then
                  write(21,2402) pid(np), cstaid(is),
     1            iyrmo(13), xmonam(13), (dat2T(i), i=1,maxAug)
                endif
              endif
            endif  
            

c ---------------------------------------------------------
c		c. Reservoirs        
c rrb 2018/08/05; Add WWSP (type 14)
cx          if(iplntyp(np).eq.3 .or. iplntyp(np).eq.5 .or.
cx   1         iplntyp(np).eq.9) then      
c
c rrb 2019/04/20; Revise to allow WWSP Supply Plan (type 14) and
c                 WWSP User Plan (type 15)
cx          if(iplntyp(np).eq.3 .or. iplntyp(np).eq.5 .or.
cx   1         iplntyp(np).eq.9 .or. iplntyp(np).eq.14) then     
cx 
            if(iplntyp(np).eq.3 .or. iplntyp(np).eq.5 .or.
     1         iplntyp(np).eq.9 .or. iplntyp(np).eq.14 .or.
     1         iplntyp(np). eq.15) then      
            
              write(21,253)
              if(isigfig.eq.0) then
c smalers 2017-11-07 Check to avoid invalid array index
                if (is.gt.0) then
                  write(21,242) pid(np), cstaid(is),
     1            iyrmo(13), xmonam(13), psto1X*fac,
     1            (dat2T(i), i=1,maxResP), psto2X*fac
                endif
              endif
              
              if(isigfig.eq.1) then
c smalers 2017-11-07 Check to avoid invalid array index
                if (is.gt.0) then
                  write(21,2421) pid(np), cstaid(is),
     1            iyrmo(13), xmonam(13), psto1X*fac,
     1            (dat2T(i), i=1,maxResP), psto2X*fac
                endif
              endif
              
              if(isigfig.eq.2) then
c smalers 2017-11-07 Check to avoid invalid array index
                if (is.gt.0) then
                  write(21,2422) pid(np), cstaid(is),
     1            iyrmo(13), xmonam(13), psto1X*fac,
     1            (dat2T(i), i=1,maxResP), psto2X*fac
                endif
              endif
            endif
c
c ---------------------------------------------------------
c		d. Non Reservoirs     
c
c rrb 2015/03/07; Allow a Changed Water Right Plan (type 13) 
cx   1          iplntyp(np).eq.12) then     
            if(iplntyp(np).eq. 4 .or. iplntyp(np).eq.6 .or.
     1         iplntyp(np).eq. 7 .or. iplntyp(np).eq.11.or.
     1         iplntyp(np).eq.12 .or. iplntyp(np).eq.13) then      
c
c		For a Release limit Plan (type 12)
c		Set annual total to beginning of year value      
              if(iplntyp(np).eq.12) dat2T(1)=datBeg(np)
              
              write(21,252)
              if(isigfig.eq.0) then
c smalers 2017-11-07 Check to avoid invalid array index
                if (is.gt.0) then
                  write(21,242) pid(np), cstaid(is),
     1            iyrmo(13), xmonam(13), (dat2T(i), i=1,maxResPX)
                endif
              endif
              
              if(isigfig.eq.1) then
c smalers 2017-11-07 Check to avoid invalid array index
                if (is.gt.0) then
                  write(21,2421) pid(np), cstaid(is),
     1            iyrmo(13), xmonam(13), (dat2T(i), i=1,maxResPX)
                endif
              endif
              
              if(isigfig.eq.2) then
c smalers 2017-11-07 Check to avoid invalid array index
                if (is.gt.0) then
                  write(21,2422) pid(np), cstaid(is),
     1            iyrmo(13), xmonam(13), (dat2T(i), i=1,maxResPX)
                endif
              endif
            endif
c
c ---------------------------------------------------------
c		e. Recharge
            if(iplntyp(np).eq. 8) then
              write(21,256)
              if(isigfig.eq.0) then
c smalers 2017-11-07 Check to avoid invalid array index
                if (is.gt.0) then
                  write(21,242) pid(np), cstaid(is),
     1            iyrmo(13), xmonam(13), (dat2T(i), i=1,maxRch)
                endif
              endif
              
              if(isigfig.eq.1) then
c smalers 2017-11-07 Check to avoid invalid array index
                if (is.gt.0) then
                  write(21,2421) pid(np), cstaid(is),
     1            iyrmo(13), xmonam(13), (dat2T(i), i=1,maxRch)
                endif
              endif
              
              if(isigfig.eq.2) then
c smalers 2017-11-07 Check to avoid invalid array index
                if (is.gt.0) then
                  write(21,2422) pid(np), cstaid(is),
     1            iyrmo(13), xmonam(13), (dat2T(i), i=1,maxRch)
                endif
              endif
            endif
c
c _________________________________________________________
c
c               End year loop
          end do
c
c
c               End plan loop
  170   continue
c
c _________________________________________________________
c
c		Step 12; Calculate Average for Annual Summary
c			 Put in year + 1
        iyx=iyend-iystr+2
        rn=iyend-iystr+1
        do np=1,maxplnT
          iy1=0
          do iy=iystr,iyend  
            iy1=iy1+1
            do i=1,MaxAll
              planTot(np,iyX,i) = planTot(np,iyX,i) + planTot(np,iy1,i)
            end do
          end do
        end do  
c
c _________________________________________________________
c     			
c   Step 13; Print Annual Summary for T&C Plans (type 1)
        if(ipTotal(1).ne.0) then
          NumplnT=NumplnT+1
          do np=1,1
            write(21,310) cunitm, np, plntypX(np)
            write(21,232) ' Supply', 'Total', (i, i=1,maxResPX)
            iy1=0
            do iy=iystr,iyend  
              iy1=iy1+1
              if(isigfig.eq.0) then
                write(21,242) 'Total       ', 'NA          ', iy, 
     1           'TOT', (planTot(np,iy1,i), i=1,MaxResPX)
              endif
              
              if(isigfig.eq.1) then
                write(21,2421) 'Total       ', 'NA          ', iy,             
     1           'TOT', (planTot(np,iy1,i), i=1,MaxResPX)
              endif
              
              if(isigfig.eq.2) then
                write(21,2422) 'Total       ', 'NA          ', iy,             
     1           'TOT', (planTot(np,iy1,i), i=1,MaxResPX)
              endif
            end do
c
c			Annual Average          
            write(21,252)
            if(isigfig.eq.0) then
              write(21,243) 'Total       ', 'NA          ', 
     1          'TOT', (planTot(np,iyX,i)/rn, i=1,MaxResPX)
            endif
            
            if(isigfig.eq.1) then
              write(21,2431) 'Total       ', 'NA          ', 
     1          'TOT', (planTot(np,iyX,i)/rn, i=1,MaxResPX)
            endif         
            
            if(isigfig.eq.2) then
              write(21,2432) 'Total       ', 'NA          ',
     1          'TOT', (planTot(np,iyX,i)/rn, i=1,MaxResPX)
            endif         
          end do 
        endif     


c
c _________________________________________________________
c
c		Step 14; Print Annual Summary for Well Augmentation type 2
        if(ipTotal(2).ne.0) then
          NumplnT=NumplnT+1       
          do np=2,2
            write(21,310) cunitm, np, plntypX(np)
            write(21,231) (i, i=1,maxAug)
            iy1=0
            do iy=iystr,iyend  
              iy1=iy1+1
              if(isigfig.eq.0) then
                write(21,240) 'Aug Wells   ', 'NA          ', iy, 
     1           'TOT', (planTot(np,iy1,i), i=1,MaxAug)
              endif
              
              if(isigfig.eq.1) then
                write(21,2401) 'Aug Wells   ', 'NA          ', iy, 
     1           'TOT', (planTot(np,iy1,i), i=1,MaxAug)
              endif
              
              if(isigfig.eq.2) then
                write(21,2402) 'Aug Wells   ', 'NA          ', iy, 
     1           'TOT', (planTot(np,iy1,i), i=1,MaxAug)
              endif
            end do
c
c ---------------------------------------------------------
c
c			Annual Average          
            write(21,251)
            if(isigfig.eq.0) then
              write(21,241) 'Aug Wells   ', 'NA          ', 
     1          'TOT', (planTot(np,iyX,i)/rn, i=1,MaxAug)
              endif
              
            if(isigfig.eq.1) then
              write(21,2411) 'Aug Wells   ', 'NA          ',
     1          'TOT', (planTot(np,iyX,i)/rn, i=1,MaxAug)
            endif  
            
            if(isigfig.eq.2) then
              write(21,2412) 'Aug Wells   ', 'NA          ',
     1          'TOT', (planTot(np,iyX,i)/rn, i=1,MaxAug)
            endif  
          end do    
        endif
        
        
c
c _________________________________________________________
c
c   Step 15; Print Annual Summary for Res Reuse (type 3)
        if(ipTotal(3).ne.0) then
          NumplnT=NumplnT+1
          do np=3,3
            write(21,310) cunitm, np, plntypX(np)
            write(21,232) ' Supply', 'Total', (i, i=1,maxResPX)
            iy1=0
            do iy=iystr,iyend  
              iy1=iy1+1
              if(isigfig.eq.0) then
                write(21,242) 'Total       ', 'NA          ', iy, 
     1           'TOT', (planTot(np,iy1,i), i=1,MaxResPX)
              endif
              
              if(isigfig.eq.1) then
                write(21,2421) 'Total       ', 'NA          ', iy,             
     1           'TOT', (planTot(np,iy1,i), i=1,MaxResPX)
              endif
              
              if(isigfig.eq.2) then
                write(21,2422) 'Total       ', 'NA          ', iy,             
     1           'TOT', (planTot(np,iy1,i), i=1,MaxResPX)
              endif
            end do
c
c			Annual Average          
            write(21,252)
            if(isigfig.eq.0) then
              write(21,243) 'Total        ', 'NA          ', 
     1          'TOT', (planTot(np,iyX,i)/rn, i=1,MaxResPX)
            endif
            
            if(isigfig.eq.1) then
              write(21,2431) 'Total       ', 'NA          ', 
     1          'TOT', (planTot(np,iyX,i)/rn, i=1,MaxResPX)
            endif         
            
            if(isigfig.eq.2) then
              write(21,2432) 'Total       ', 'NA          ',
     1          'TOT', (planTot(np,iyX,i)/rn, i=1,MaxResPX)
            endif         
          end do 
        endif     
c
c _________________________________________________________
c 
c   Step 16; Print Annual Summary for NonReservoir Reuse (type 4)
        if(ipTotal(4).ne.0) then
          NumplnT=NumplnT+1       
          do np=4,4
            write(21,310) cunitm, np, plntypX(np)
            write(21,232) ' Supply', 'Total', (i, i=1,maxResPX)
            iy1=0
            do iy=iystr,iyend  
              iy1=iy1+1
              if(isigfig.eq.0) then
                write(21,242) 'Total        ', 'NA          ', iy, 
     1           'TOT', (planTot(np,iy1,i), i=1,MaxResPX)
              endif
              
              if(isigfig.eq.1) then
                write(21,2421) 'Total       ', 'NA          ', iy,             
     1           'TOT', (planTot(np,iy1,i), i=1,MaxResPX)
              endif
              
              if(isigfig.eq.2) then
                write(21,2422) 'Total       ', 'NA          ', iy,             
     1           'TOT', (planTot(np,iy1,i), i=1,MaxResPX)
              endif
            end do
c
c			Annual Average          
            write(21,252)
            if(isigfig.eq.0) then
              write(21,243) 'Total        ', 'NA          ', 
     1          'TOT', (planTot(np,iyX,i)/rn, i=1,MaxResPX)
            endif
            
            if(isigfig.eq.1) then
              write(21,2431) 'Total       ', 'NA          ', 
     1          'TOT', (planTot(np,iyX,i)/rn, i=1,MaxResPX)
            endif         
            
            if(isigfig.eq.2) then
              write(21,2432) 'Total       ', 'NA          ',
     1          'TOT', (planTot(np,iyX,i)/rn, i=1,MaxResPX)
            endif         
          end do 
        endif     
        
c
c _________________________________________________________
c
c   Step 17; Print Annual Summary Reuse to a Reservoir Tmtn (type 5)
        if(ipTotal(5).ne.0) then
          NumplnT=NumplnT+1        
          do np=5,5
            write(21,310) cunitm, np, plntypX(np)
            write(21,232) ' Supply', 'Total', (i, i=1,maxResPX)
            iy1=0
            do iy=iystr,iyend  
              iy1=iy1+1
              if(isigfig.eq.0) then
                write(21,242) 'Total        ', 'NA          ', iy, 
     1           'TOT', (planTot(np,iy1,i), i=1,MaxResPX)
              endif
              
              if(isigfig.eq.1) then
                write(21,2421) 'Total       ', 'NA          ', iy,             
     1           'TOT', (planTot(np,iy1,i), i=1,MaxResPX)
              endif
              
              if(isigfig.eq.2) then
                write(21,2422) 'Total       ', 'NA          ', iy,             
     1           'TOT', (planTot(np,iy1,i), i=1,MaxResPX)
              endif
            end do
c
c			Annual Average          
            write(21,252)
            if(isigfig.eq.0) then
              write(21,243) 'Total       ', 'NA          ', 
     1          'TOT', (planTot(np,iyX,i)/rn, i=1,MaxResPX)
            endif
            
            if(isigfig.eq.1) then
              write(21,2431) 'Total       ', 'NA          ', 
     1          'TOT', (planTot(np,iyX,i)/rn, i=1,MaxResPX)
            endif         
            
            if(isigfig.eq.2) then
              write(21,2432) 'Total       ', 'NA          ',
     1          'TOT', (planTot(np,iyX,i)/rn, i=1,MaxResPX)
            endif         
          end do 
        endif     
        
c
c _________________________________________________________
c
c   step 18; Print Annual Summary for Reuse to a Diversion tmtn (type 6)
        if(ipTotal(6).ne.0) then
          NumplnT=NumplnT+1        
          do np=6,6
            write(21,310) cunitm, np, plntypX(np)
            write(21,232) ' Supply', 'Total', (i, i=1,maxResPX)
            iy1=0
            do iy=iystr,iyend  
              iy1=iy1+1
              if(isigfig.eq.0) then
                write(21,242) 'Total        ', 'NA          ', iy, 
     1           'TOT', (planTot(np,iy1,i), i=1,MaxResPX)
              endif
              
              if(isigfig.eq.1) then
                write(21,2421) 'Total       ', 'NA          ', iy,             
     1           'TOT', (planTot(np,iy1,i), i=1,MaxResPX)
              endif
              
              if(isigfig.eq.2) then
                write(21,2422) 'Total       ', 'NA          ', iy,             
     1           'TOT', (planTot(np,iy1,i), i=1,MaxResPX)
              endif
            end do
c
c			Annual Average          
            write(21,252)
            if(isigfig.eq.0) then
              write(21,243) 'Total       ', 'NA          ', 
     1          'TOT', (planTot(np,iyX,i)/rn, i=1,MaxResPX)
            endif
            
            if(isigfig.eq.1) then
              write(21,2431) 'Total       ', 'NA          ', 
     1          'TOT', (planTot(np,iyX,i)/rn, i=1,MaxResPX)
            endif         
            
            if(isigfig.eq.2) then
              write(21,2432) 'Total       ', 'NA          ',
     1          'TOT', (planTot(np,iyX,i)/rn, i=1,MaxResPX)
            endif         
          end do 
        endif     
            
c
c _________________________________________________________
c 
c   Step 19; Print Annual Summary for TransMtn (type 7)
        if(ipTotal(7).ne.0) then
          NumplnT=NumplnT+1        
          do np=7,7
            write(21,310) cunitm, np, plntypX(np)
            write(21,232) ' Supply', 'Total', (i, i=1,maxResPX)
            iy1=0
            do iy=iystr,iyend  
              iy1=iy1+1
              if(isigfig.eq.0) then
                write(21,242) 'Total       ', 'NA          ', iy, 
     1           'TOT', (planTot(np,iy1,i), i=1,MaxResPX)
              endif
              
              if(isigfig.eq.1) then
                write(21,2421) 'Total       ', 'NA          ', iy,             
     1           'TOT', (planTot(np,iy1,i), i=1,MaxResPX)
              endif
              
              if(isigfig.eq.2) then
                write(21,2422) 'Total       ', 'NA          ', iy,             
     1           'TOT', (planTot(np,iy1,i), i=1,MaxResPX)
              endif
            end do
c
c			Annual Average          
            write(21,252)
            if(isigfig.eq.0) then
              write(21,243) 'Total       ', 'NA          ', 
     1          'TOT', (planTot(np,iyX,i)/rn, i=1,MaxResPX)
            endif
            
            if(isigfig.eq.1) then
              write(21,2431) 'Total       ', 'NA          ', 
     1          'TOT', (planTot(np,iyX,i)/rn, i=1,MaxResPX)
            endif         
            
            if(isigfig.eq.2) then
              write(21,2432) 'Total       ', 'NA          ',
     1          'TOT', (planTot(np,iyX,i)/rn, i=1,MaxResPX)
            endif         
          end do 
        endif     
c
c _________________________________________________________
c
c		Step 20; Print Annual Summary for Recharge type 8
        if(ipTotal(8).ne.0) then
          NumplnT=NumplnT+1        
          do np=8,8
            write(21,310) cunitm, np, plntypX(np)
            write(21,235) (i, i=1,maxRch)
            iy1=0
            do iy=iystr,iyend  
              iy1=iy1+1
              
              if(isigfig.eq.0) then
                write(21,242) 'Recharge    ', 'NA          ', iy, 
     1           'TOT', (planTot(np,iy1,i), i=1,MaxRch)
              endif
              
              if(isigfig.eq.1) then
                write(21,2421) 'Recharge    ', 'NA          ', iy,             
     1           'TOT', (planTot(np,iy1,i), i=1,MaxRch)
              endif
              
              if(isigfig.eq.2) then
                write(21,2422) 'Recharge    ', 'NA          ', iy,             
     1           'TOT', (planTot(np,iy1,i), i=1,MaxRch)
              endif
            end do
c
c ---------------------------------------------------------
c
c			Annual Average          
            write(21,256)
            
            if(isigfig.eq.0) then
              write(21,243) 'Recharge    ', 'NA          ', 
     1          'TOT', (planTot(np,iyX,i)/rn, i=1,MaxRch)
            endif
            
            if(isigfig.eq.1) then
              write(21,2431) 'Recharge    ', 'NA          ',
     1          'TOT', (planTot(np,iyX,i)/rn, i=1,MaxRch)
            endif  
            
            if(isigfig.eq.2) then
              write(21,2432) 'Recharge    ', 'NA          ',
     1          'TOT', (planTot(np,iyX,i)/rn, i=1,MaxRch)
            endif  
          end do 
        endif     
c
c _________________________________________________________
c
c		Step 21; Print Annual Summary for OOP Plans type 9
        if(ipTotal(9).ne.0) then
          NumplnT=NumplnT+1        
          do np=9,9
            write(21,310) cunitm, np, plntypX(np)
            write(21,234) (i, i=1,maxResP+2)
            iy1=0
            do iy=iystr,iyend  
              iy1=iy1+1
              if(isigfig.eq.0) then
                write(21,242) 'OOP Plan    ', 'NA          ', iy, 
     1           'TOT', -1.0, (planTot(np,iy1,i), i=1,MaxResP), -1.0
              endif
              
              if(isigfig.eq.1) then
                write(21,2421) 'OOP Plan    ', 'NA          ', iy,             
     1           'TOT', -1.0, (planTot(np,iy1,i), i=1,MaxResP), -1.0
              endif
              
              if(isigfig.eq.2) then
                write(21,2422) 'OOP Plan    ', 'NA          ', iy,             
     1           'TOT', -1.0, (planTot(np,iy1,i), i=1,MaxResP), -1.0
              endif
            end do
c
c ---------------------------------------------------------
c			Annual Average          
            write(21,253)
            if(isigfig.eq.0) then
              write(21,243) 'OOP Plan    ', 'NA          ', 
     1          'TOT', -1.0, (planTot(np,iyX,i)/rn, i=1,MaxResP)
            endif
            
            if(isigfig.eq.1) then
              write(21,2431) 'OOP Plan    ', 'NA          ', 
     1          'TOT', -1.0, (planTot(np,iyX,i)/rn, i=1,MaxResP)
            endif  
            
            if(isigfig.eq.2) then
              write(21,2432) 'OOP Plan    ', 'NA          ', 
     1          'TOT', -1.0, (planTot(np,iyX,i)/rn, i=1,MaxResP)
            endif  
          end do 
        endif     
c
c ---------------------------------------------------------
c   
c   Step 22; Print Annual Summary for Release Limit Plans (type 11)
        if(ipTotal(11).ne.0) then
          NumplnT=NumplnT+1        
          do np=11,11
            write(21,310) cunitm, np, plntypX(np)
            write(21,232) ' Supply', 'Total', (i, i=1,maxResPX)
            iy1=0
            do iy=iystr,iyend  
              iy1=iy1+1
              if(isigfig.eq.0) then
                write(21,242) 'Accounting  ', 'NA          ', iy, 
     1           'TOT', (planTot(np,iy1,i), i=1,MaxResPX)
              endif
              
              if(isigfig.eq.1) then
                write(21,2421) 'Accounting   ', 'NA          ', iy,             
     1           'TOT', (planTot(np,iy1,i), i=1,MaxResPX)
              endif
              
              if(isigfig.eq.2) then
                write(21,2422) 'accounting   ', 'NA          ', iy,             
     1           'TOT', (planTot(np,iy1,i), i=1,MaxResPX)
              endif
            end do
c
c			Annual Average          
            write(21,252)
            if(isigfig.eq.0) then
              write(21,243) 'Accounting   ', 'NA          ', 
     1          'TOT', (planTot(np,iyX,i)/rn, i=1,MaxResPX)
            endif
            
            if(isigfig.eq.1) then
              write(21,2431) 'Accounting   ', 'NA          ', 
     1          'TOT', (planTot(np,iyX,i)/rn, i=1,MaxResPX)
            endif         
            
            if(isigfig.eq.2) then
              write(21,2432) 'Accounting   ', 'NA          ',
     1          'TOT', (planTot(np,iyX,i)/rn, i=1,MaxResPX)
            endif         
          end do 
        endif     
        
        
        
        
c
c ---------------------------------------------------------
c 
c   Step 23; Print Annual Summary for Release Limit Plans (type 12)
        if(ipTotal(12).ne.0) then
          NumplnT=NumplnT+1       
          do np=12,12
            write(21,310) cunitm, np, plntypX(np)
            write(21,232) ' Supply', 'Total', (i, i=1,maxResPX)
            iy1=0
            do iy=iystr,iyend  
              iy1=iy1+1
              if(isigfig.eq.0) then
                write(21,242) 'Rel Limit   ', 'NA          ', iy, 
     1           'TOT', -1.0, (planTot(np,iy1,i), i=2,MaxResPX)
              endif
              
              if(isigfig.eq.1) then
                write(21,2421) 'Rel Limit   ', 'NA          ', iy,             
     1           'TOT', -1.0, (planTot(np,iy1,i), i=2,MaxResPX)
              endif
              
              if(isigfig.eq.2) then
                write(21,2422) 'Rel Limit   ', 'NA          ', iy,             
     1           'TOT', -1.0, (planTot(np,iy1,i), i=2,MaxResPX)
              endif
            end do
c
c			Annual Average          
            write(21,252)
            if(isigfig.eq.0) then
              write(21,243) 'Rel Limit   ', 'NA          ', 
     1          'TOT', -1.0, (planTot(np,iyX,i)/rn, i=2,MaxResPX)
            endif
            
            if(isigfig.eq.1) then
              write(21,2431) 'Rel Limit   ', 'NA          ', 
     1          'TOT', -1.0, (planTot(np,iyX,i)/rn, i=2,MaxResPX)
            endif         
            
            if(isigfig.eq.2) then
              write(21,2432) 'Rel Limit   ', 'NA          ',
     1          'TOT', -1.0, (planTot(np,iyX,i)/rn, i=2,MaxResPX)
            endif         
          end do 
        endif    
c
c rrb 2015-03-24
c
c ---------------------------------------------------------
c   
c   Step 22; Print Annual Summary for Changed Water Right (type 13)
        if(ipTotal(13).ne.0) then
          NumplnT=NumplnT+1        
          do np=13,13
            write(21,310) cunitm, np, plntypX(np)
            write(21,232) ' Supply', 'Total', (i, i=1,maxResPX)
            iy1=0
            do iy=iystr,iyend  
              iy1=iy1+1
              if(isigfig.eq.0) then
                write(21,242) 'Accounting  ', 'NA          ', iy, 
     1           'TOT', (planTot(np,iy1,i), i=1,MaxResPX)
              endif
              
              if(isigfig.eq.1) then
                write(21,2421) 'Accounting   ', 'NA          ', iy,             
     1           'TOT', (planTot(np,iy1,i), i=1,MaxResPX)
              endif
              
              if(isigfig.eq.2) then
                write(21,2422) 'Changed WR   ', 'NA          ', iy,             
     1           'TOT', (planTot(np,iy1,i), i=1,MaxResPX)
              endif
            end do
c
c			Annual Average          
            write(21,252)
            if(isigfig.eq.0) then
              write(21,243) 'Changed WR   ', 'NA          ', 
     1          'TOT', (planTot(np,iyX,i)/rn, i=1,MaxResPX)
            endif
            
            if(isigfig.eq.1) then
              write(21,2431) 'Changed WR   ', 'NA          ', 
     1          'TOT', (planTot(np,iyX,i)/rn, i=1,MaxResPX)
            endif         
            
            if(isigfig.eq.2) then
              write(21,2432) 'Changed WR   ', 'NA          ',
     1          'TOT', (planTot(np,iyX,i)/rn, i=1,MaxResPX)
            endif         
          end do 
        endif     
 
 
c
c _________________________________________________________
c
c   Step 23; Print Annual Summary for WWSP Source (type 14)
        if(ipTotal(14).ne.0) then
          NumplnT=NumplnT+1
          do np=14,14
            write(21,310) cunitm, np, plntypX(np)
            write(21,232) ' Supply', 'Total', (i, i=1,maxResPX)
            iy1=0
            do iy=iystr,iyend  
              iy1=iy1+1
              if(isigfig.eq.0) then
                write(21,242) 'Total       ', 'NA          ', iy, 
     1           'TOT', (planTot(np,iy1,i), i=1,MaxResPX)
              endif
              
              if(isigfig.eq.1) then
                write(21,2421) 'Total       ', 'NA          ', iy,             
     1           'TOT', (planTot(np,iy1,i), i=1,MaxResPX)
              endif
              
              if(isigfig.eq.2) then
                write(21,2422) 'Total       ', 'NA          ', iy,             
     1           'TOT', (planTot(np,iy1,i), i=1,MaxResPX)
              endif
            end do
c
c			Annual Average          
            write(21,252)
            if(isigfig.eq.0) then
              write(21,243) 'Total        ', 'NA          ', 
     1          'TOT', (planTot(np,iyX,i)/rn, i=1,MaxResPX)
            endif
            
            if(isigfig.eq.1) then
              write(21,2431) 'Total       ', 'NA          ', 
     1          'TOT', (planTot(np,iyX,i)/rn, i=1,MaxResPX)
            endif         
            
            if(isigfig.eq.2) then
              write(21,2432) 'Total       ', 'NA          ',
     1          'TOT', (planTot(np,iyX,i)/rn, i=1,MaxResPX)
            endif         
          end do 
        endif  
        
c
c _________________________________________________________
c
c rrb 2019/04/20; Revise to include WWSP Source (type 14 and 
c                 WWSP Use (type 15)
c   Step 24; Print Annual Summary for WWSP Use (type 15)
        if(ipTotal(15).ne.0) then
          NumplnT=NumplnT+1
          do np=15,15
            write(21,310) cunitm, np, plntypX(np)
            write(21,232) ' Supply', 'Total', (i, i=1,maxResPX)
            iy1=0
            do iy=iystr,iyend  
              iy1=iy1+1
              if(isigfig.eq.0) then
                write(21,242) 'Total       ', 'NA          ', iy, 
     1           'TOT', (planTot(np,iy1,i), i=1,MaxResPX)
              endif
              
              if(isigfig.eq.1) then
                write(21,2421) 'Total       ', 'NA          ', iy,             
     1           'TOT', (planTot(np,iy1,i), i=1,MaxResPX)
              endif
              
              if(isigfig.eq.2) then
                write(21,2422) 'Total       ', 'NA          ', iy,             
     1           'TOT', (planTot(np,iy1,i), i=1,MaxResPX)
              endif
            end do
c
c			Annual Average          
            write(21,252)
            if(isigfig.eq.0) then
              write(21,243) 'Total        ', 'NA          ', 
     1          'TOT', (planTot(np,iyX,i)/rn, i=1,MaxResPX)
            endif
            
            if(isigfig.eq.1) then
              write(21,2431) 'Total       ', 'NA          ', 
     1          'TOT', (planTot(np,iyX,i)/rn, i=1,MaxResPX)
            endif         
            
            if(isigfig.eq.2) then
              write(21,2432) 'Total       ', 'NA          ',
     1          'TOT', (planTot(np,iyX,i)/rn, i=1,MaxResPX)
            endif         
          end do 
        endif        
        
              
c
c _________________________________________________________
c
c               Step 25; Return
c
      return
c
c _________________________________________________________
c
c               Formats
c
 
 240    format(a12,1x,a12, i5,2x,a3,28f8.0, 2(5x, a3),f8.0)  
 2401   format(a12,1x,a12, i5,2x,a3,28f8.1, 2(5x, a3),f8.1)  
 2402   format(a12,1x,a12, i5,2x,a3,28f8.2, 2(5x, a3),f8.2)  
  
 242    format(a12,1x,a12, i5,2x,a3,22f8.0, 20f8.0)  
 2421   format(a12,1x,a12, i5,2x,a3,22f8.1, 20f8.1)  
 2422   format(a12,1x,a12, i5,2x,a3,22f8.2, 20f8.2)  

 244    format(a12,1x,a12, i5,2x,a3,40f8.0)
 2441   format(a12,1x,a12, i5,2x,a3,40f8.1)
 2442   format(a12,1x,a12, i5,2x,a3,40f8.2)
c
c ---------------------------------------------------------
c		Average Annual 

 241    format(a12,1x,a12, '  AVE',2x,a3,28f8.0, 2(5x, a3),f8.0)  
 2411   format(a12,1x,a12, '  AVE',2x,a3,28f8.1, 2(5x, a3),f8.1)  
 2412   format(a12,1x,a12, '  AVE',2x,a3,28f8.2, 2(5x, a3),f8.2)  
  
 243    format(a12,1x,a12, '  AVE',2x,a3,22f8.0, 20f8.0)  
 2431   format(a12,1x,a12, '  AVE',2x,a3,22f8.1, 20f8.1)  
 2432   format(a12,1x,a12, '  AVE',2x,a3,22f8.2, 20f8.2)  

cx 245    format(a12,1x,a12, '  AVE',2x,a3,40f8.0)
cx 2451   format(a12,1x,a12, '  AVE',2x,a3,40f8.1)
cx 2452   format(a12,1x,a12, '  AVE',2x,a3,40f8.2)
c
c		Type 1 Standard T&C
  230   format(/,
     1  '                                                    ',
     1  'Plan Sources                            ',
     1  '                                       ',
     1  '                                                        ',
     1  '                                        ',
     1  'ReDivert                        ',     
     1  'Performance',/
     1  '                                       From    Plan ',     
     1  '________________________________________',
     1  '________________________________________',     
     1  '________________________________________',     
     1  '______________________________________________________',
     1  ' _______________________________',     
     1  ' _______________________'/
     1  'Plan         River                  Exc_Byp  Demand',
     1  '   Src 1   Src 2   Src 3   Src 4   Src 5',
     1  '   Src 6   Src 7   Src 8   Src 9  Src 10',
     1  '  Src 11  Src 12  Src 13  Src 14  Src 15',
     1  '  Src 16  Src 17  Src 18  Src 19  Src 20   Short   Total',
     1  ' Store 1 Store 2 Store 3   Total',
     1  '  Switch  Status   Total',/
     1  'ID           ID           Year   Mo     N/A     N/A',
     1  '     (+)     (+)     (+)     (+)     (+)',
     1  '     (+)     (+)     (+)     (+)     (+)',
     1  '     (+)     (+)     (+)     (+)     (+)',
     1  '     (+)     (+)     (+)     (+)     (+)     (+)     N/A',
     1  '     (+)     (+)     (+)     N/A',     
     1  '     N/A     N/A     N/A',/
     1  '                                    ', 31('    (',i2,')'),/     
     1  '____________ ____________ ____ ____ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______',
     1  ' _______ _______ _______')
c
c		Type 2 Well Augmentation
  231   format(/,
     1  '                                                    ',
     1  'Plan Sources                        ',/
     1  '                                       From    Plan ',
     1  '________________________________________',
     1  '________________________________________',
     1  '________________________________________',
     1  '_______________________________________________________',/
     1  'Plan         River                     Well  Demand',
     1  '   Src 1   Src 2   Src 3   Src 4   Src 5',
     1  '   Src 6   Src 7   Src 8   Src 9  Src 10',
     1  '  Src 11  Src 12  Src 13  Src 14  Src 15',
     1  '  Src 16  Src 17  Src 18  Src 19  Src 20   Short   Total',/
     1  'ID           ID           Year   Mo     N/A     N/A',
     1  '     (+)     (+)     (+)     (+)     (+)',
     1  '     (+)     (+)     (+)     (+)     (+)',
     1  '     (+)     (+)     (+)     (+)     (+)',
     1  '     (+)     (+)     (+)     (+)     (+)     (+)     N/A',/
     1  '                                   ', 24('    (',i2,')'),/     
     1  '____________ ____________ ____ ____ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______ _______ _______')
c
c		Type 4, 6, and 7 Non Reservoir and Transmountain   
  232   format(/,
     1  '                                            ',
     1  'Plan Uses                         ',/
     1  '                                    ',a7,
     1  ' _________________________________________',
     1  '________________________________________',
     1  '________________________________________',     
     1  '______________________________________________',/
     1  'Plan         River                    ',a5,
     1  '   Use 1   Use 2   Use 3   Use 4   Use 5',
     1  '   Use 6   Use 7   Use 8   Use 9  Use 10',
     1  '  Use 11  Use 12  Use 13  Use 14  Use 15',
     1  '  Use 16  Use 17  Use 18  Use 19  Use 20   Total',/
     1  'ID           ID           Year   Mo     N/A     (+)',
     1  '     (+)     (+)     (+)     (+)     (+)',
     1  '     (+)     (+)     (+)     (+)     (+)',
     1  '     (+)     (+)     (+)     (+)     (+)',          
     1  '     (+)     (+)     (+)     (+)     N/A',/
     1  '                                   ', 22('    (',i2,')'),/
     1  '____________ ____________ ____ ____ _______',      
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______ _______')
c
c		Type 3 & 5 Reservoir
     
  233   format(/,
     1  '                                                    ',
     1  'Plan Uses                 ',/
     1  '                                    Initial  Supply',
     1  ' _______________________________________',
     1  '________________________________________',     
     1  '________________________________________',
     1  '________________________________________________  Ending',/
     1  'Plan         River                 ',
     1  ' Storage   Total',
     1  '   Use 1   Use 2   Use 3   Use 4   Use 5',
     1  '   Use 6   Use 7   Use 8   Use 9  Use 10',
     1  '  Use 11  Use 12  Use 13  Use 14  Use 15',
     1  '  Use 16  Use 17  Use 18  Use 19  Use 20   Total Storage',/
     1  'ID           ID           Year   Mo     N/A     N/A',
     1  '     (+)     (+)     (+)     (+)     (+)',
     1  '     (+)     (+)     (+)     (+)     (+)',
     1  '     (+)     (+)     (+)     (+)     (+)',
     1  '     (+)     (+)     (+)     (+)     (+)     N/A     N/A',/
     1  '                                   ', 24('    (',i2,')'),/
     1  '____________ ____________ ____ ____ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______ _______ _______')
c
c		Type 9; Plans     
  234   format(/,
     1  '                                                    ',
     1  'Plan Sources                ',/
     1  '                                    Initial  Demand',
     1  ' _______________________________________',
     1  '________________________________________',
     1  '________________________________________',          
     1  '________________________________________________  Ending',/
     1  'Plan         River                   Demand   Total',
     1  '   Src 1   Src 2   Src 3   Src 4   Src 5',
     1  '   Src 6   Src 7   Src 8   Src 9  Src 10',
     1  '  Src 11  Src 12  Src 13  Src 14  Src 15',
     1  '  Src 16  Src 17  Src 18  Src 19  Src 20   Total  Demand',/
     1  'ID           ID           Year   Mo     N/A     N/A',
     1  '     (+)     (+)     (+)     (+)     (+)',
     1  '     (+)     (+)     (+)     (+)     (+)',
     1  '     (+)     (+)     (+)     (+)     (+)',
     1  '     (+)     (+)     (+)     (+)     (+)     N/A     N/A',/
     1  '                                   ', 24('    (',i2,')'),/
     1  '____________ ____________ ____ ____ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______ _______ _______')
c
c		Type 8 Recharge
  235   format(/,
     1  '                                                    ',
     1  'Plan Uses                         ',/
     1  '                                             Supply',
     1  ' _________________________________________',
     1  '________________________________________',
     1  '________________________________________',     
     1  '______________________________________________',/
     1  'Plan         River                 Recharge   Total',
     1  '   Use 1   Use 2   Use 3   Use 4   Use 5',
     1  '   Use 6   Use 7   Use 8   Use 9  Use 10',
     1  '  Use 11  Use 12  Use 13  Use 14  Use 15',
     1  '  Use 16  Use 17  Use 18  Use 19  Use 20   Total',/
     1  'ID           ID           Year   Mo     N/A     N/A',
     1  '     (+)',
     1  '     (+)     (+)     (+)     (+)     (+)',
     1  '     (+)     (+)     (+)     (+)     (+)',
     1  '     (+)     (+)     (+)     (+)     (+)',          
     1  '     (+)     (+)     (+)     (+)     N/A',/
     1  '                                   ', 23('    (',i2,')'),/
     1  '____________ ____________ ____ ____ _______ _______',      
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______ _______')  
c
c		Type 14 WWSP-Supply and type 15 WWSP-User Plans
     
  236   format(/,
     1  '                                                    ',
     1  'Plan Uses (1-20) '/
     1  '                                    Initial  Supply',
     1  ' _______________________________________',
     1  '________________________________________',     
     1  '________________________________________',
     1  '________________________________________________  Ending',/
     1  'Plan         River                 ',
     1  ' Storage   Total',
     1  '   Use 1   Use 2   Use 3   Use 4   Use 5',
     1  '   Use 6   Use 7   Use 8   Use 9  Use 10',
     1  '   Use 1   Use 2   Use 3   Use 4   Use 5',
     1  '   Use 6   Use 7   Use 8   Use 9  Use 10   Total Storage',/
     1  'ID           ID           Year   Mo     N/A     N/A',
     1  '     (-)     (-)     (-)     (-)     (-)',
     1  '     (-)     (-)     (-)     (-)     (-)',
     1  '     (-)     (-)     (-)     (-)     (-)',
     1  '     (-)     (-)     (-)     (-)     (-)     N/A     N/A',/
     1  '                                   ', 24('    (',i2,')'),/
     1  '____________ ____________ ____ ____ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______ _______ _______')
     
 250  format(
     1  '____________ ____________ ____ ____ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______',
     1  ' _______ _______ _______')
     
 251  format(
     1  '____________ ____________ ____ ____ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______ _______ _______')
     
 252  format(
     1  '____________ ____________ ____ ____ _______',      
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______ _______')
     
 253  format(
     1  '____________ ____________ ____ ____ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______ _______ _______')
 
cx 254  format(
cx     1  '____________ ____________ ____ ____',
cx     1  ' _______ _______ _______ _______ _______',
cx     1  ' _______ _______ _______ _______ _______',
cx     1  ' _______ _______ _______ _______')
cx
 256  format(
     1  '____________ ____________ ____ ____ _______ _______',      
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______ _______')
     
 300  FORMAT(/,    /
     1 'Plan Summary   ', a5,/
     1 'Plan Number    = ',i5,/
     1 'Plan Type      = ',i5, 1x, a25,1x/
     1 'Plan ID        = ',a12,/
     1 'Plan Name      = ',a24,/
     1 'Plan Source    = ',a12,/
     1 'River Location = ',a12,/)
 301  format(
     1 a5,i2, 
     1 '   ID = ', a12, ' Admin # = ', f11.5,'   Name = ',a24, 
     1 '   Opr Type = ', i3,
     1 '   Source = ', a12, '   Status = ', a3)    

 303  format(
     1 a5,i2, 
     1 '   ID = ', a12, ' Admin # = ', f11.5,'   Name = ',a24, 
     1 '   Opr Type = ', i3,
     1 '   Destination = ', a12,'   Status = ', a3)    
cx 304  format(/,'Note: Any sources > 6 are summed under use 6')
cx 305  format(/,'Note: Any uses > 9 are summed under use 9')
cx 
 310  FORMAT(//,72('_'),//
     1 'Total Plan Summary', a5,/
     1 'Plan Type      = ',i5, 1x, a25)
  
 400  format(/,  
     1  'OutPln; Detailed output for Plan # = ', i5,
     1  ' ID = ', a12, ' Type = ',i5, ' Name = ', a25, 
     1  ' maxplnU = ', i5)
 410  format(/, 12x, 60('  ', i3),/
     1          12('_'), 60(' ____'))      
 420  format(a12, 100i5)


c
c _________________________________________________________ 
c        Error Messages
c
c                                        
c rrb 2021/04/18; Compiler warning       
cx350 write(6,*)  '  Stopped in OutPln; see the log file (*.log)' 
 9999 write(6,*)  '  Stopped in OutPln; see the log file (*.log)'
      write(nlog,*) '  Stopped in OutPln'                        
      write(6,*) 'Stop 1' 
      call flush(6)
      call exit(1)

      stop 
      END
