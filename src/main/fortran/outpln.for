C
c *********************************************************
c
      SUBROUTINE OutPln
c
c
c _________________________________________________________
c	Program Description
c
c       Outpln; It prints plan data
c
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
c              Step 1; Initilize
c		
c		iout = detailed output
c		ioutN= plan pointer for detailed output 
      iout = 0
c      ioutN=52

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
      maxAll=amax0(maxTC, maxAug, maxResP, maxResPX)
      
      do np=1,nplan
        pfailx(np)=0.0
      end do
c
c		Initilze total per plan type      
      do np=1,maxPlnT
        do iy=1,100
          do i=1,40
            planTot(np,iy,i)=0.0
            ipTotal(i)=0
          end do
        end do
      end do

      
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
c		Concept for every plan, find uses by scanning 
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
           
          write(21,300) cunitm, np, iplntyp(np),plntypC(np),Pid(np),
     1      Pname1(np), Psource(np), cstaid(is)
     
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
              write(nlog,*) ' OutPln;  np, nop, ke, ks, kr, ku, ko'
              write(nlog,*) ' OutPln; ',  np, nop, ke, ks, kr, ku, ko
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
              kUP=kUP+1
              
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
          if(iout.eq.1) write(nlog,*) ' OutPln; iystr, iyend ', iystr, iyend
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
c
c		c. Reservoir Plans            
            if(iplntyp(np).eq.3 .or. iplntyp(np).eq.5) then
              write(21,233) (i, i=1,maxResP+2)
            endif    
c
c ---------------------------------------------------------
c
c		d. Non reservoir and Accounting plans            
            if(iplntyp(np).eq.4  .or. iplntyp(np).eq.6 .or. 
     1         iplntyp(np).eq.11) then
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
c _________________________________________________________
c
c		Step 7; Month Loop              
            DO IM=1,12
              imx=amax0(1, im-1)
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
              ipTotal(ip1)=1
c
c ---------------------------------------------------------
c		a. Standard T&C Output             
              if(iplntyp(np).eq.1) then
                read(68,rec=irec1) pid(np), cstaid(is),
     1           iyrmo(im), xmonam(im), (dat2(i), i=1,maxTC), 
     1           cfail1, cfail2, pfail(np) 
              endif
c
c ---------------------------------------------------------
c		b. Well Augmentation plan Output              
c rrb 2006/12/19; Type 10 is a Special Well Augmentation 
c                 (e.g. Designated Basin, Coffin Well, etc.)
c             if(iplntyp(np).eq.2) then
              if(iplntyp(np).eq.2 .or. iplntyp(np).eq.10) then
                read(68,rec=irec1) pid(np), cstaid(is),
     1           iyrmo(im), xmonam(im), (dat2(i), i=1,maxAug), 
     1           cfail1, cfail2, pfail(np) 
              endif
c
c ---------------------------------------------------------
c		c. Reservoir Plans            
              if(iplntyp(np).eq.3 .or. iplntyp(np).eq.5 .or.
     1           iplntyp(np).eq.9) then                   
                read(68,rec=irec1) pid(np), cstaid(is),
     1           iyrmo(im), xmonam(im), (dat2(i), i=1,maxResP), 
     1           psto1X, psto2X, pfail(np)                  
              endif
c
c ---------------------------------------------------------
c
c		d. Non reservoir and Transmountain
             if(iplntyp(np).eq.4 .or. iplntyp(np).eq.6 .or.
     1          iplntyp(np).eq.7 .or. iplntyp(np).eq.11.or.
     1          iplntyp(np).eq.12) then           
               read(68,rec=irec1) pid(np), cstaid(is),
     1           iyrmo(im), xmonam(im), (dat2(i), i=1,maxResP), 
     1           psto1X, psto2X, pfail(np) 
              endif
c
c ---------------------------------------------------------
c		e. Recharge              
              if(iplntyp(np).eq.8) then
                read(68,rec=irec1) pid(np), cstaid(is),
     1            iyrmo(im), xmonam(im), (dat2(i), i=1,maxRch), 
     1            psto1X, psto2X, pfail(np) 
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
cxxxx              
              
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
                  write(21,240) pid(np), cstaid(is),
     1              iyrmo(im), xmonam(im), (dat2(i)*fac, i=1,maxTC), 
     1              cfail1, cfail2, pfail(np) 
                endif
                
                if(isigfig.eq.1) then
                  write(21,2401) pid(np), cstaid(is),
     1              iyrmo(im), xmonam(im), (dat2(i)*fac, i=1,maxTC), 
     1              cfail1, cfail2, pfail(np) 
                endif
                
                if(isigfig.eq.2) then
                  write(21,2402) pid(np), cstaid(is),
     1              iyrmo(im), xmonam(im), (dat2(i)*fac, i=1,maxTC), 
     1              cfail1, cfail2, pfail(np) 
                endif
              endif  
c
c ---------------------------------------------------------
c		b. Well Augmentation plan Output
c rrb 2006/12/19; Type 10 is a Special Well Augmentation 
c                 (e.g. Designated Basin, Coffin Well, etc.)
              if(iplntyp(np).eq.2 .or. iplntyp(np).eq.10) then
                if(isigfig.eq.0) then
                  write(21,244) pid(np), cstaid(is),
     1              iyrmo(im), xmonam(im), (dat2(i)*fac, i=1,maxAug)
                endif
                
                if(isigfig.eq.1) then
                  write(21,2441) pid(np), cstaid(is),
     1              iyrmo(im), xmonam(im), (dat2(i)*fac, i=1,maxAug)
                endif
                
                if(isigfig.eq.2) then
                  write(21,2442) pid(np), cstaid(is),
     1              iyrmo(im), xmonam(im), (dat2(i)*fac, i=1,maxAug)
                endif
              endif  
c
c ---------------------------------------------------------
c		c. Reservoir Output        
              if(iplntyp(np).eq.3 .or. iplntyp(np).eq.5 .or.
     1           iplntyp(np).eq.9) then      
     
                if(isigfig.eq.0) then
                  write(21,242) pid(np), cstaid(is),
     1              iyrmo(im), xmonam(im), psto1X*fac, 
     1              (dat2(i)*fac, i=1,maxResP), psto2X*fac
                endif
                
                if(isigfig.eq.1) then
                  write(21,2421) pid(np), cstaid(is),
     1              iyrmo(im), xmonam(im), psto1X*fac,
     1              (dat2(i)*fac, i=1,maxResP), psto2X*fac
                endif
                
                if(isigfig.eq.2) then
                  write(21,2422) pid(np), cstaid(is),
     1              iyrmo(im), xmonam(im), psto1X*fac,
     1              (dat2(i)*fac, i=1,maxResP), psto2X*fac
                endif
              endif
c
c ---------------------------------------------------------
c		d. Non Reservoir Output        
              if(iplntyp(np).eq. 4 .or. iplntyp(np).eq.6 .or.
     1           iplntyp(np).eq. 7 .or. iplntyp(np).eq.11.or.
     1           iplntyp(np).eq.12) then      
     
                if(isigfig.eq.0) then
                  write(21,242) pid(np), cstaid(is),
     1              iyrmo(im), xmonam(im), (dat2(i)*fac, i=1,maxResPX)
                endif
                
                if(isigfig.eq.1) then
                  write(21,2421) pid(np), cstaid(is),
     1              iyrmo(im), xmonam(im), (dat2(i)*fac, i=1,maxResPX)
                endif
     
                if(isigfig.eq.2) then
                  write(21,2422) pid(np), cstaid(is),
     1              iyrmo(im), xmonam(im), (dat2(i)*fac, i=1,maxResPX)
                endif
              endif
c
c ---------------------------------------------------------
c		e. Recharge
              if(iplntyp(np).eq. 8) then      
              
                if(isigfig.eq.0) then
                  write(21,242) pid(np), cstaid(is),
     1              iyrmo(im), xmonam(im), (dat2(i)*fac, i=1,maxRch)
                endif
                
                if(isigfig.eq.1) then
                  write(21,2421) pid(np), cstaid(is),
     1              iyrmo(im), xmonam(im), (dat2(i)*fac, i=1,maxRch)
                endif
              
                if(isigfig.eq.2) then
                  write(21,2422) pid(np), cstaid(is),
     1              iyrmo(im), xmonam(im), (dat2(i)*fac, i=1,maxRch)
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
                write(21,240) pid(np), cstaid(is),
     1            iyrmo(13), xmonam(13), (dat2T(i), i=1,maxTC),
     1            cfail1, cfail2T, pfail(np)            
              endif
              
              if(isigfig.eq.1) then
                write(21,2401) pid(np), cstaid(is),
     1            iyrmo(13), xmonam(13), (dat2T(i), i=1,maxTC),
     1            cfail1, cfail2T, pfail(np)            
              endif
              
              if(isigfig.eq.2) then
                write(21,2402) pid(np), cstaid(is),
     1            iyrmo(13), xmonam(13), (dat2T(i), i=1,maxTC),
     1            cfail1, cfail2T, pfail(np)            
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
                write(21,240) pid(np), cstaid(is),
     1            iyrmo(13), xmonam(13), (dat2T(i), i=1,maxAug)
              endif
              
              if(isigfig.eq.1) then
                write(21,2401) pid(np), cstaid(is),
     1            iyrmo(13), xmonam(13), (dat2T(i), i=1,maxAug)
              endif
              
              if(isigfig.eq.2) then
                write(21,2402) pid(np), cstaid(is),
     1            iyrmo(13), xmonam(13), (dat2T(i), i=1,maxAug)
              endif
            endif  
            

c ---------------------------------------------------------
c		c. Reservoirs        
            if(iplntyp(np).eq.3 .or. iplntyp(np).eq.5 .or.
     1         iplntyp(np).eq.9) then      
            
              write(21,253)
              if(isigfig.eq.0) then
                write(21,242) pid(np), cstaid(is),
     1            iyrmo(13), xmonam(13), psto1X*fac,
     1            (dat2T(i), i=1,maxResP), psto2X*fac
              endif
              
              if(isigfig.eq.1) then
                write(21,2421) pid(np), cstaid(is),
     1            iyrmo(13), xmonam(13), psto1X*fac,
     1            (dat2T(i), i=1,maxResP), psto2X*fac
              endif
              
              if(isigfig.eq.2) then
                write(21,2422) pid(np), cstaid(is),
     1            iyrmo(13), xmonam(13), psto1X*fac,
     1            (dat2T(i), i=1,maxResP), psto2X*fac
              endif
            endif
c
c ---------------------------------------------------------
c		d. Non Reservoirs        
            if(iplntyp(np).eq. 4 .or. iplntyp(np).eq.6 .or.
     1         iplntyp(np).eq. 7 .or. iplntyp(np).eq.11.or.
     1         iplntyp(np).eq.12) then      
c
c		For a Release limit Plan (type 12)
c		Set annual total to beginning of year value      
              if(iplntyp(np).eq.12) dat2T(1)=datBeg(np)
              
              write(21,252)
              if(isigfig.eq.0) then
                write(21,242) pid(np), cstaid(is),
     1            iyrmo(13), xmonam(13), (dat2T(i), i=1,maxResPX)
              endif
              
              if(isigfig.eq.1) then
                write(21,2421) pid(np), cstaid(is),
     1            iyrmo(13), xmonam(13), (dat2T(i), i=1,maxResPX)
              endif
              
              if(isigfig.eq.2) then
                write(21,2422) pid(np), cstaid(is),
     1            iyrmo(13), xmonam(13), (dat2T(i), i=1,maxResPX)
              endif
            endif
c
c ---------------------------------------------------------
c		e. Recharge
            if(iplntyp(np).eq. 8) then
              write(21,256)
              if(isigfig.eq.0) then
                write(21,242) pid(np), cstaid(is),
     1            iyrmo(13), xmonam(13), (dat2T(i), i=1,maxRch)
              endif
              
              if(isigfig.eq.1) then
                write(21,2421) pid(np), cstaid(is),
     1            iyrmo(13), xmonam(13), (dat2T(i), i=1,maxRch)
              endif
              
              if(isigfig.eq.2) then
                write(21,2422) pid(np), cstaid(is),
     1            iyrmo(13), xmonam(13), (dat2T(i), i=1,maxRch)
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

 245    format(a12,1x,a12, '  AVE',2x,a3,40f8.0)
 2451   format(a12,1x,a12, '  AVE',2x,a3,40f8.1)
 2452   format(a12,1x,a12, '  AVE',2x,a3,40f8.2)
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
 
 254  format(
     1  '____________ ____________ ____ ____',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______')

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
 304  format(/,'Note: Any sources > 6 are summed under use 6')
 305  format(/,'Note: Any uses > 9 are summed under use 9')
 
 310  FORMAT(//,72('_'),//
     1 'Total Plan Summary', a5,/
     1 'Plan Type      = ',i5, 1x, a25)


c
c _________________________________________________________ 
c        Error Messages
c

  350 write(6,*)  '  Stopped in OutPln; see the log file (*.log)'
      write(nlog,*) '  Stopped in OutPln'                        
      write(6,*) 'Stop 1' 
      call flush(6)
      call exit(1)

      stop 
      END
