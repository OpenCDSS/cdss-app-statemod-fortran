c outWW - prints Winter Water Program for a WWSP Source only
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
      SUBROUTINE outWW
c
c
c _________________________________________________________
c	Program Description
c
c       outWW; It prints Winter Water Program for a WWSP Source only
c
c Approach
c       Copy outPln and revise to read *.opr,  Note:
c         1. To get detail associated with a type 46 operating rule
c            that distributes a plan to multiple plans
c            read new binary opr file for WWSP (*.b39)
c         2. To print detail associated with a type 45 operating rule
c            that diverts to a WWSP Plan and to Irrigate
c            increment the output (iop1) to print both.
c
c _________________________________________________________
c       Update History
c
c rrb 2021/04/18; Compiler warning
c
c rrb 2019/04/20; Revised to recognize a WWSP Supply plan is a
c                   type 14 and a WWSP User Plan is a type 15
c rrb 2019/02/17; Add ownership Percent to the output file header
c rrb 2018/08/05; Revise to allow a WWSP tied to a reservoir (type 14)
c
c _________________________________________________________
c
c       Documentation
c		iop     = counter for use and source output
c		iPtotal(i)= code to print summary for plan type i
c		PlanTot(i,j,k)= total to plan type i, year j, 
c			        use or source k
c
c _________________________________________________________
c	Dimension
c
      include 'common.inc'
      character ftype*24, parType*24, cfail1*3, cfail2*3, cfail2T*3,
     1          copOff*3, rec12*12, rec24*24, ciy*4, cwwX*3
c                                 
c rrb 2021/04/18; Compiler warning     
cx    dimension                                     
cx   1  pfailx(maxplan), planTot(maxplnT,maxyrs,40), 
cx
      dimension 
     1  planTot(maxplnT,maxyrs,40), 
     1  iptotal(40),     datBeg(maxPlan),
     1  dat1x(40)
c
c rrb 2018/10/07; Define variables for monthly average
      dimension divm(13),divt(13)
      data divm/13*0.0/, divt/13*0.0/
     
c _________________________________________________________
c

c              Step 1; Initialize
c
c rrb 2021/04/18; Compiler warning
      n52=0
      i52=0
      c=0.0   
      fac=0.0   
      iprob=0   
      cfail1=' '
      cfail2=' '      
      cfail2t=' '              
      ciy=' '
      ftype=' '
      partype=' '
      if(iprob.gt.0) goto 9999      
c		
c		           ioutWW = detailed output
      ioutWW = 0
c
      maxResP=22
      nmax=maxopr2/2
c
      rec12=' '
      rec24=' '
      copOff=' On'
      cwwX='   '
c
c rrb 2019/02/17; Add ownership Percent      
      pctX=-1.0

      write(6,101) 'OutWW   '
      write(nlog,101) 'OutWW   '
 101  format(/,72('_'),/'  Subroutine ', a8)

c
      small = 0.001
      smalln=-1*small
           

c		Initialize total per plan type      
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
      call outtop(29, 1, 65)
c
c _________________________________________________________ 
c
c               Step 3. Process each plan
c		            Concept for every plan, find uses by scanning 
c               the operation right file.
       
        if(ioutWW.eq.1) then
          write(nlog,*) ' '
          write(nlog,*) '______________________________________'
          write(nlog,*) 'OutWW (2); nplan', nplan
        endif
        
        do 150 np=1,nplan
        
          if(ioutWW.eq.1) then
            write(nlog,*) ' '
            write(nlog,*) '____________________________________'
            write(nlog,*) 'OutWW (3); np, pid(np)', np, pid(np)
          endif
c
c _________________________________________________________ 
c               Step 3b; Exit if this is not a WWSP Supply (type 14)
c                        or WWSP User (type 15)          
          iP1=iPlnTyp(np) 
c
c rrb 2019/04/20 Revise to recognize a WWSP Supply (type 14)
c                        or WWSP User (type 15)
cx        if(ip1.ne.14) goto 150
cx
          iok=0
          if(ip1.eq.14) iok=1
          if(ip1.eq.15) iok=1
          if(iok.eq.0) goto 150
 
          is=ipsta(np)   
          ipTotal(ip1)=1

          
          if(is.eq.0) then
            write(nlog,*) '  OutWW; Problem is = ', is
          endif
c
c _________________________________________________________ 
c
c               Step 3b; Print Plan Header 
c rrb 2018/09/13 Move below to skip if no records are found
c                This is related to not print for a WWSP User
            
c
c rrb 2009/05/26; Correction
          DatBeg(np)=0          
c
c _________________________________________________________ 
c
c               Step 4; Year Loop
c
c rrb 2018/010/07; Calculate storage
          psto1Y=0.0
          psto2Y=0.0
          
          iy1=0
          DO 120 IY=IYSTR,IYEND
            iy1=iy1+1
            
            call year(iy, iyrmo, imomo, cyr1)
            do i=1,40
              dat2T(i)=0.0
            end do
c     
            totpln=0.0       
            totplnA=0.0
            divirrT=0.0
c
c               Temporarily save variables sto1y & psto2y to
c                 calculate storage instead of using data
c                 read from the plan binary file.            
c
c _________________________________________________________
c
c		            Step 5; Month Loop              
            DO 110 IM=1,12
            
              if(ioutWW.eq.1) then
                write(nlog,*) ' '
                write(nlog,*) '______________________________________'                
                write(nlog,*) ' OutWW (5); ', iyrmo(im), xmonam(im)
              endif
              
c
c rrb 2021/04/18; Compiler warning
cx            imx=amax0(1, im-1)
              imx=max(1, im-1)
              
              do i=1,40
                dat2(i)=0.0
              end do
c
c rrb 2018/10/21; Set warning for this plan, year and month
              cwwX='   '                
c
              fac=fmo(im) 
              psto1Y=psto2Y  
c _________________________________________________________
c
c		            Step 7; Loop for every operating right

              iop=0
              iop1=0
              totmo=0.0
              i46=0  
              n46=0       

              do 100 k=1, numopr 
c                
                ifound=0
c
c rrb 2018/09/30; Set i46=0, a code to indicate if a WWSP-user plan is 
c                 both a destination (WWSP User) and part of a type 46 
c                 multiple plan ownership operating rule
                i46=0
                n46=0
c
c _________________________________________________________
c
c								PLAN IS A REUSE FOR VARIOUS OPR RULES (24 & 25 
C               BUT NOT 45)
c               Note: type 24 is an exchange
c                     type 25 is a bypass
c                     type 45 is a carrier to irrigate
c               Step 7a; Find a WWSP Supply specified as Creuse 
c                  in the binary operating rule file 
c                  This finds all sources to the WWSP_Suply
c                  including type 24 (Exchange)& 25 (Bypass)
c                  but not type 45 (Carrier used for a WWSP diversion
c                  to irrigate)
                if(creuseX(k).eq.Pid(np)) then
                  if(ityopr(k).ne.45) then
                
                    ifound=1
                    iop=iop+1
                    c=1.0
c
c rrb 2019/02/25; Add ownership Percent for a source
                    pctX=100.0
                    
                    if(ioutWW.eq.1) then
cx                    write(nlog,*) ' '                     
                      write(nlog,*) 
     1                  '  OutWW (7a); Plan ID = creuseX(k) type 45', 
     1                  ityopr(k), iop, creuseX(k), pid(np)  
                    endif
                  endif  
                endif
c
c _________________________________________________________
C               PLAN IS SOURCE 2 FOR MULTIPLE OPERATING RULES
c               Step 7b; Find a WWSP Supply specified as Source 2 
c                  in the binary operating rule file 
c                  This finds all WWSP_Users uses including:
c                  Type 27 (Res. & WWSP Supply to a WWSP User Direct) 
c                  Type 28 (Res. & WWSP Supply to a WWSP User Bypass) 
c                  Type 29 (Reservoir and plan spill)
C                  Type 34 (Bookover with a plan)
                if(ciopsoX2(k).eq.Pid(np)) then 
                  ifound=1
                  iop=iop+1
                  c=-1.0
c
c rrb 2019/02/25; Add ownership Percent for a source
                  pctX=100.0
               
                  if(ioutWW.eq.1) then  
cx                    write(nlog,*) ' '                    
                    write(nlog,*) 
     1                '  OutWW (7b); Plan ID = ciopsoX2(k) Source 2', 
     1                ityopr(k), iop, ciopsoX2(k), pid(np)  
                  endif   
               endif
c
c _________________________________________________________
c
C               PLAN IS REUSE FOR A TYPE 45 RULE
c               Step 7c1; Find WWSP data for a type 45 
c                  specified as creuse or as a destination
c                  This is a diversion to irrigate.
                if(ityopr(k).eq.45) then
c
c ---------------------------------------------------------
c                        Find a WWSP Supply as variable creuseX
                  if(creuseX(k).eq.Pid(np)) then
                    ifound=1
                    iop=iop+1
                    c=1.0
c
c rrb 2019/02/25; Add ownership Percent for a source
                    pctX=100.0
                    
                    if(ioutWW.eq.1) then
cx                      write(nlog,*) ' '                     
                      write(nlog,*) 
     1                  '  OutWW (7c-1); Plan ID = creuseX Reuse', 
     1                  ityopr(k), iop, creuseX(k), pid(np)  
                    endif
                  endif
c                  
c ---------------------------------------------------------
C               PLAN IS SOURCE 5 DESTINATION
c               Step 7c2; Find a WWSP Supply as a 2'nd destination 
c                 (cioipsoX5).  This occurs if its a 
c                 diversion to irrigate
                
                  if(ciopsoX5(k).eq.Pid(np)) then 
                    ifound=1
                    iop=iop+1
                    c=1.0
c
c rrb 2019/02/25; Add ownership Percent for a source
                    pctX=100.0
                  
                    if(ioutWW.eq.1) then
cx                      write(nlog,*) ' '                      
                      write(nlog,*) 
     1                  '  OutWW (7c-2); Plan ID = ciopsoX5 Source5', 
     1                  ityopr(k), iop, ciopsoX5(k), pid(np)  
                    endif  
                  endif  
c
c                     Endif for type 45                  
                endif

c
c _________________________________________________________
c
C               PLAN IS SOURCE 1 FOR A TYPE 29 (SPILL OR RESET)
C               OPR RULE
c               Step 7d; Find a WWSP Supply as a source 
c rrb 2018/09/25; Add WWSP Users 
c
                if(ityopr(k).eq.29) then
                 if(ciopsoX(1,k).eq.Pid(np)) then
                    ifound=1
                    iop=iop+1
                    c=-1.0
c
c rrb 2019/02/25; Add ownership Percent for a source
                    pctX=100.0
                    
                    if(ioutWW.eq.1) then
                      write(nlog,*) ' '                     
                      write(nlog,*) 
     1                  '  OutWW (7e); Plan ID = ciopsoX(1,k)', 
     1                  ityopr(k), iop, ciopsoX(1,k), pid(np)  
                    endif                 
                  endif 
                endif  
c
c _________________________________________________________
c
C                PLAN IS SOURCE 1 FOR A TYPE 46 OPR RULE
cc               Step 7e1; Find a WWSP Supply specified as a type 46
c                 in the binary operating rule file 
c
c rrb 2018/09/25; Add WWSP Users 
c
                if(ityopr(k).eq.46) then
c
c ---------------------------------------------------------
c                      Set the WWSP Supply data                 
                  if(ciopsoX(1,k).eq.Pid(np)) then
                    ifound=1
                    iop=iop+1
                    c=-1.0
c
c rrb 2019/02/17; Add ownership Percent for a source
                    pctX=100.0
                    
                    if(ioutWW.eq.1) then
cx                     write(nlog,*) ' '                     
                      write(nlog,*) 
     1                  '  OutWW (7f); Plan ID = ciopsoX(1,k)', 
     1                  ityopr(k), iop, ciopsoX(1,k), pid(np)  
                    endif                 
                  endif   
c
c ---------------------------------------------------------
c
c rrb 2018/09/25; Add WWSP Users           
c               Step 7e2; Find the WWSP User value by checking each 
c                        possible destination.  Notr:
c                        destination 1 is ciopdeX(1,k),
c                        destination 2 is ciopdeX(3,k)
c                        destination 3 is ciopdeX(5,k)
                    i46=0
                    n46=0
                    n1=0
                    n2=0
                    
                    do n=1,nmax    
                      n1=n2+1
                      n2=n1+1
cx                    write(nlog,*) ' OutWW ', n, n1, n2 
                                 
                      if(ciopdeX(n1,k).eq.Pid(np)) then 
                        i46=1
c
c rrb 2018/09/07; Store index for detailed type 46 output
                        n46=n
                        ifound=1
                        iop=iop+1
c
c rrb 2018/10/07; Allow detailed WWSP data to be read 
cx                        c=-1.0
                        c=1.0
c
c rrb 2019/02/17; Add ownership Percent for a destination
c
c rrb 2019/02/25; Add ownership Percent for a destination
cx                      pctX=10.0
                        pctX=iopdes(n2,k)
                        
                        if(ioutWW.eq.1) then
                          write(nlog,*) 
     1                    '  OutWW (7g); Plan ID = ciopsoX(1,k)', 
     1                    ityopr(k), iop, n, n1, n2, n46,
     1                    ciopdeX(n,k), pid(np), pctX  
                        endif
                      endif  
                    end do                              
c
c                     Endif for type 46                  
               endif
c
c
c _________________________________________________________
c
c rrb 2018/12/08; Add Type 52 Multi-Reservoir
cc               Step 7f; Find a WWSP Supply specified as a type 52
c                  in the binary operating rule file 
c
                if(ityopr(k).eq.52) then
c
c rrb 2018/09/25; Add WWSP Users           
c                 Find the WWSP User value by checking each 
c                 possible destination.  Note:
c                 destination 1 is ciopdeX(1,k),
c                 destination 2 is ciopdeX(3,k)
c                 destination 3 is ciopdeX(5,k)
c
                  i52=0
                  n52=0
                  n1=0
                  n2=0
                  
                  do n=1,nmax    
                    n1=n2+1
                    n2=n1+1
cx                  write(nlog,*) ' OutWW ', n, n1, n2 
                               
                    if(ciopdeX(n1,k).eq.Pid(np)) then 
                      i52=1
                      n52=n
                      ifound=1
                      iop=iop+1
c
c rrb 2018/10/07; Allow detailed WWSP data to be read 
cx                        c=-1.0
                        c=1.0
c
c rrb 2019/02/25; Add ownership Percent for a source
                        pctX=100.0
                      
                      if(ioutWW.eq.1) then
cx                      write(nlog,*) ' ' 
                        write(nlog,*) 
     1                    ' OutWW (7h); Plan ID = ciopsoX(1,k)', 
     1                    ityopr(k), iop, n, n1, n2, n52,
     1                    ciopdeX(n1,k), pid(np)  
                      endif
                    endif  
                  end do                              
c
c                     Endif for type 52                  
                endif              
c
c _________________________________________________________
c
c               Step 8; Read Binary Operating Rule File.  Note: 
c                       - File 45 is standard binary *.xop
c                       - File 39 is detailed binary output (e.g. used
c                         when there are multiple destinations)   
c
c 50             continue
                if(ioutWW.eq.1) then 
                  if(ifound.eq.1) then
                    write(nlog,320) 'OutWW (8a); ',  pID(np), 
     1                np, ifound, k, ityopr(k), iop, i46, p46*fac
                  
                  endif 
                endif
 
                DivIrr=0.0
                iop1=0
                
                if(ifound.eq.1) then 
c                                
c _________________________________________________________
c rrb 2018/09/25; Step 8a; Read operating rule data if not a type 46
c                 destination (i46=0).  Else set dat2(iop) = p46 
c                 (Total Supply)from the binary plan file

                  if(i46.eq.0) then
                    irec1=((iy-iystr0)*12*numopr)+((im-1)*(numopr))+k
                    read(45,rec=irec1) dat2(iop)
                    dat2(iop)=dat2(iop)*fac*c                   
c
c rrb 2018/10/21; set warning for the WWSP Source
                    read(39,rec=irec1) (dat1x(j), j=1,maxopr2), rx
cx                    cwwX='   '                    
                    if(rx.gt.small) cwwX='YES'

                  endif
c
                  if(ioutWW.eq.1) then
                    write(nlog,*) ' OutWW (X), Read *.xop, k, iop dat2',
     1              ' p46 ',k, iop, dat2(iop), p46*fac
                  endif
c
c _________________________________________________________
c rrb 2018/10/07; Step 8b Read detailed divoWW data for a type 46 rule                  
                  if(i46.eq.1) then
                    irec1=((iy-iystr0)*12*numopr)+((im-1)*(numopr))+k
                    read(39,rec=irec1) (dat1x(j), j=1,maxopr2), rx
                    dat2(iop)=dat1x(n46)*fac*c 
c
c rrb 2019/10/14; Set warning                    
cx                    cwwX='   '                    
                    if(rx.gt.small) cwwX='YES'
                    
                    if(ioutWW.eq.1) then
                      write(nlog,*) ' '
                      write(nlog,*) 
     1                ' OutWW (Y)  Year  Mon    k  iop  n46',
     1                '    dat2   dat1x      rx cwwX'
                      write(nlog,'(a12, i5, 2x, a3, 3i5,3f8.0,2x,a3)')
     1                '  Outww (Y) ',iyrmo(im), xmonam(im),
     1                k, iop, n46, dat2(iop), dat1x(n46)*fac, rx, cwwX
                    endif                  
                  endif
c
c _________________________________________________________
c rrb 2018/12/08; Step 8c Read detailed divoWW data for a type 52 rule                  
                  if(i52.eq.1) then
                    irec1=((iy-iystr0)*12*numopr)+((im-1)*(numopr))+k
                    read(39,rec=irec1) (dat1x(j), j=1,maxopr2), rx
                    dat2(iop)=dat1x(n52)*fac*c 
c
c rrb 2019/10/14; Set warning                    
cx                    cwwX='   '                    
                    if(rx.gt.small) cwwX='YES'
                    
                    if(ioutWW.eq.1) then
                      write(nlog,*) ' '
                      write(nlog,*) 
     1                ' OutWW (Y)  Year  Mon    k  iop  n52',
     1                '    dat2   dat1x      rx cwwX'
                      write(nlog,'(a12, i5, 2x, a3, 3i5,3f8.0,2x,a3)')
     1                '  Outww (Y) ',iyrmo(im), xmonam(im),
     1                k, iop, n52, dat2(iop), dat1x(n52)*fac, rx, cwwX
                    endif                  
                  endif
c
c ---------------------------------------------------------
c               Step 8d; Account for diversions to Irrigate for the 
c                     WWSP Supply and WWSP User by 
c                       Setting divIrr to be the diversion * -1.
c                       incrementing iop to iop1 and 
c                       storing as iop1
c rrb 2018/10/19; Correction only adjust for div to irrigate if the destinaion
c                 is a diversion (iopdesr(k) = 3
cx                if(ityopr(k).eq.45) then
                  if(ityopr(k).eq.45 .and. iopdesr(k).eq.3) then
                  
                      divIrr=-1*dat2(iop)
                      iop1=iop+1
                      
                      dat2(iop1)  = divIrr
                      divIrrT     = divIrrT+divIrr
                      dat2T(iop1) = dat2T(iop1) + dat2(iop1)
c
                      if(ioutWW.eq.1) then                      
                        write(nlog,*) 
     1                    '  OutWW (8b); ityopr(k), creuse,',
     1                    'iop, iop1, divIrr'
                        write(nlog,*) 
     1                    '  OutWW (8b);',                         
     1                    ityopr(k), creuseX(k), iop, iop1, divIrr
                      endif
c
c                    endif for a type 45 diversion to irrigate
                  endif
c
c ---------------------------------------------------------
c               Step 8e; Calculate total to a WWSP Plan
                  totpln=totpln+amax1(0.0, dat2(iop))
                  totplnA=amax1(totpln, totplnA)
c                
c                   Calculate total for this month
                  totmo=totmo+dat2(iop)+divIrr
c                
c							     Calculate annual total                
                  dat2T(iop) = dat2T(iop) + dat2(iop) 
c
c rrb 2018/10/07;  Calculate ending storage
                  psto2Y = psto2Y+ dat2(iop) + divIrr
c
c rrb 2018/10/07;  Calcuulate average monthly
                  divm(im)=divm(im)+ dat2(iop) + divIrr
                  divt(im)=divt(im)+(divm(im)/(iyend-iystr+1))
                  divt(13)=divt(13)+(divm(im)/(iyend-iystr+1)*ftot)  
c _________________________________________________________ 
c            
c                    Step 9; Print Plan Header once per plan
c                         
                  if(iy1.eq.1 .and. im.eq.1) then
c
c _________________________________________________________ 
c                    Step 9a; Print Header once per year and per plan
                    if(iop.eq.1) then
                      write(29,300) cunitm, np, iplntyp(np),plntypC(np),
     1                Pid(np), Pname1(np), Psource(np), cstaid(is)
                    endif
c
c _________________________________________________________ 
c
c                    Step 9b; Print Source data once per year and plan
c
c
c rrb 2018/02/25; Update 
cx                    write(29,301) 'Src  ',iop, corid(k), ropnk(k),
cx     1                nameo(k), ityopr(k), ciopsoX(1,k), 100.0,copOff 
                    write(29,301) 'Src  ',iop, corid(k), ropnk(k),
     1                nameo(k), ityopr(k), ciopsoX(1,k), pctX,copOff 
c _________________________________________________________ 
c
c                    Step 9c; Print two sources to include the diversion to
c                          irrigate for a type 45    
                   if(iop1.gt.0) then    
c
c rrb 2018/02/25; Update 
cx                      write(29,301) 'Src  ',iop1, corid(k), ropnk(k),
cx     1                  nameo(k), ityopr(k), ciopsoX(1,k), pctX,copOff 
                      write(29,301) 'Src  ',iop1, corid(k), ropnk(k),
     1                  nameo(k), ityopr(k), ciopsoX(1,k), pctX,copOff        
                    endif
c
c                        Endif for year 1, month 1
                  endif
c
c _________________________________________________________ 
c
c                    Step 10; Increment iop to include the diversion to 
c                          irrigate for a type 45
c
                  if(ioutWW.eq.1) then
                    write(nlog,*) ' OUTWW (10a) iop0, iop, iop1', 
     1                iop0, iop, iop1
                  endif
     
                  iop0=iop
                  if(iop1.gt.1) iop=iop+1
c
                  if(ioutWW.eq.1) then                  
                    write(nlog,*) ' OUTWW (10b) iop0, iop, iop1', 
     1                iop0, iop, iop1
                  endif
c
c                        Endif for finding an operating rule
                endif                  
                  
c              
c ---------------------------------------------------------
c               End Operating Right loop
                
  100         continue
c
c _________________________________________________________ 
c
c               Step 11a; Print annual title once per year per plan    
              if(im.eq.1) write(29,236) (i, i=1,nMax+5) 
c                    
c _________________________________________________________ 
c               Step 11b; Print 1 month of data
              if(ioutWW.eq.1) then
cx                write(nlog,242) pid(np), cstaid(is), cwwX,             
                  write(nlog,*) pid(np), cstaid(is), cwwX,
     1            iyrmo(im), xmonam(im), totpln, psto1Y, totMo,
     1            (dat2(i), i=1,nmax), totMo, psto2Y
              endif
                           
              if(isigfig.eq.0) then
                  write(29,242) pid(np), cstaid(is), cwwX,
     1            iyrmo(im), xmonam(im), totpln, psto1Y, totMo,
     1            (dat2(i), i=1,nmax), totMo, psto2Y
              endif
                          
              if(isigfig.eq.1) then
                 write(29,2421) pid(np), cstaid(is), cwwX,
     1            iyrmo(im), xmonam(im), totpln, psto1Y, totMo, 
     1            (dat2(i), i=1,nmax), totMo, psto2Y
              endif
              
              if(isigfig.eq.2) then
                  write(29,2422) pid(np), cstaid(is), cwwX,
     1            iyrmo(im), xmonam(im), totpln, psto1Y, totMo,
     1            (dat2(i), i=1,nmax), totMo, psto2Y
              endif
c
c ---------------------------------------------------------
c               End Month loop
  110       continue
c
c ---------------------------------------------------------
c		            Step 12; Write annual total once per year per plan           
            write(29,253)
            if(isigfig.eq.0) then
                write(29,242) pid(np), cstaid(is), cwwX,
     1          iyrmo(13), xmonam(13), totplnA, psto1Y, totMo,
     1          (dat2T(i), i=1,nmax), totMo, psto2Y
            endif
            
            if(isigfig.eq.1) then
                write(29,2421) pid(np), cstaid(is), cwwX,
     1          iyrmo(13), xmonam(13), totplnA, psto1Y, totMo,
     1          (dat2T(i), i=1,nmax), totMo, psto2Y
            endif
            
            if(isigfig.eq.2) then
                write(29,2422) pid(np), cstaid(is), cwwX,
     1          iyrmo(13), xmonam(13), totplnA, psto1Y, totMo,
     1          (dat2T(i), i=1,nmax), totMo, psto2Y
            endif 
c
c ---------------------------------------------------------
c               End Year loop
  120     continue
cyyyy  
cyyyy          if(isigfig.eq.0) then
cyyyy            write(46,170) (divt(im), im=1,13)
cyyyy          else
cyyyy            write(46,172) (divt(im), im=1,13)
cyyyy          endif 
cyyyy
c
c ---------------------------------------------------------
c
c		f. Detailed output     
          if(ioutWW.eq.1) then
            write(nlog,*) ' '
            write(nlog,'(a12,1x,i5, 40f8.0)') 
     1      '  OutWW (12); np,', np, (dat2(i), i=1,nmax)
          endif
c     
c
c _________________________________________________________
c
c		           Step 13; Calculate annual total and plan total
          do i=1,maxAll
              dat2T(i) = dat2T(i) + dat2(i)*fac
              planTot(ip1,iy1,i)=planTot(ip1,iy1,i) + dat2(i)*fac                
          end do  
c
c ---------------------------------------------------------
c               End plan loop
  150   continue
c
c               Skip annual printout until I have more time
        goto 500
c
c _________________________________________________________
c
c		            Step 13; Calculate Average for Annual Summary
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
c               Step 14; Print Annual Summary for WWSP Supply (type 14)
        if(ipTotal(14).ne.0) then
          NumplnT=NumplnT+1
          do np=14,14
            write(29,310) cunitm, np, plntypX(np)
cx            write(29,236) ' Supply', 'Total', (i, i=1,nmax+4)
            
            write(29,*) '  Outww; iystr, iyend', iystr, iyend
            iy1=0
            do iy=iystr,iyend  
              iy1=iy1+1
              if(isigfig.eq.0) then
                write(29,242) 'Total       ', 'NA          ',
     1          cwwX, iy, 
     1           'TOT', (planTot(np,iy1,i), i=1,nmax+5)
              endif
              
              if(isigfig.eq.1) then
                write(29,2421) 'Total       ', 'NA          ', 
     1          cwwX, iy,             
     1           'TOT', (planTot(np,iy1,i), i=1,nmax+5)
              endif
              
              if(isigfig.eq.2) then
                write(29,2422) 'Total       ', 'NA          ', 
     1          cwwX, iy,             
     1           'TOT', (planTot(np,iy1,i), i=1,nmax+5)
              endif
            end do
c
c			Annual Average          
            write(29,252)
            if(isigfig.eq.0) then
              write(29,243) 'Total        ', 'NA          ', 
     1          cwwX, 'TOT', (planTot(np,iyX,i)/rn, i=1,MaxResP)
            endif
c            
            if(isigfig.eq.1) then
              write(29,2431) 'Total       ', 'NA          ', 
     1          cwwX, 'TOT', (planTot(np,iyX,i)/rn, i=1,MaxResP)
            endif         
c            
            if(isigfig.eq.2) then
              write(29,2432) 'Total       ', 'NA          ',
     1          cwwX, 'TOT', (planTot(np,iyX,i)/rn, i=1,MaxResP)
            endif         
          end do 
        endif        




c
c _________________________________________________________
c
c               Step 15; Print Annual Summary for WWSP User (type 15)
c
c rrb 2019/04/20; Revise to recognize WWSP Supply is type 14 and
c                 WWSP User is a type 15
c
        if(ipTotal(15).ne.0) then
          NumplnT=NumplnT+1
          do np=14,14
            write(29,310) cunitm, np, plntypX(np)
cx            write(29,236) ' Supply', 'Total', (i, i=1,nmax+4)
            
            write(29,*) '  Outww; iystr, iyend', iystr, iyend
            iy1=0
            do iy=iystr,iyend  
              iy1=iy1+1
              if(isigfig.eq.0) then
                write(29,242) 'Total       ', 'NA          ',
     1          cwwX, iy, 
     1           'TOT', (planTot(np,iy1,i), i=1,nmax+5)
              endif
              
              if(isigfig.eq.1) then
                write(29,2421) 'Total       ', 'NA          ', 
     1          cwwX, iy,             
     1           'TOT', (planTot(np,iy1,i), i=1,nmax+5)
              endif
              
              if(isigfig.eq.2) then
                write(29,2422) 'Total       ', 'NA          ', 
     1          cwwX, iy,             
     1           'TOT', (planTot(np,iy1,i), i=1,nmax+5)
              endif
            end do
c
c			Annual Average          
            write(29,252)
            if(isigfig.eq.0) then
              write(29,243) 'Total        ', 'NA          ', 
     1          cwwX, 'TOT', (planTot(np,iyX,i)/rn, i=1,MaxResP)
            endif
c            
            if(isigfig.eq.1) then
              write(29,2431) 'Total       ', 'NA          ', 
     1          cwwX, 'TOT', (planTot(np,iyX,i)/rn, i=1,MaxResP)
            endif         
c            
            if(isigfig.eq.2) then
              write(29,2432) 'Total       ', 'NA          ',
     1          cwwX, 'TOT', (planTot(np,iyX,i)/rn, i=1,MaxResP)
            endif         
          end do 
        endif        


c
c _________________________________________________________
c
c               Step 15; Return
c
  500   return
c
c _________________________________________________________
c
c               Formats
c
 242    format(a12,1x,a12, 5x,a3, i5,2x,a3,23f8.0, 20f8.0)  
 2421   format(a12,1x,a12, 5x,a3, i5,2x,a3,23f8.1, 20f8.1)  
 2422   format(a12,1x,a12, 5x,a3, i5,2x,a3,23f8.2, 20f8.2)  
c
c ---------------------------------------------------------
c		Average Annual 

 243    format(a12,1x,a12, 5x,a3, '  AVE',2x,a3,23f8.0, 20f8.0)  
 2431   format(a12,1x,a12, 5x,a3, '  AVE',2x,a3,23f8.1, 20f8.1)  
 2432   format(a12,1x,a12, 5x,a3, '  AVE',2x,a3,23f8.2, 20f8.2)  
 
c
c		Type 14 WWSP Plan
     
  236   format(/,
     1  'Winter Water Program Summary                        ',
     1  '                Plan Sources (1-20) ',/
     1  '        ',
     1  '                                        Max Initial  Source',
     1  ' _______________________________________',
     1  '________________________________________',     
     1  '________________________________________',
     1  '________________________________________________  Ending',/
     1  'Plan         River          Alloc          ',
     1  ' To Plan Storage   Total',
     1  '   Src 1   Src 2   Src 3   Src 4   Src 5',
     1  '   Src 6   Src 7   Src 8   Src 9  Src 10',
     1  '  Src 11  Src 12  Src 13  Src 14  Src 15',
     1  '  Src 16  Src 17  Src 18  Src 19  Src 20   Total Storage',/
     1  'ID           ID           Warning Year   Mo',
     1  '     N/A     N/A     N/A',
     1  '     (+)     (+)     (+)     (+)     (+)',
     1  '     (+)     (+)     (+)     (+)     (+)',
     1  '     (+)     (+)     (+)     (+)     (+)',
     1  '     (+)     (+)     (+)     (+)     (+)     N/A     N/A',/
     1  '                                           ',
     1  25('    (',i2,')'),/
     1  '____________ ____________ _______',
     `  ' ____ ____ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______ _______ _______')
              
 252  format(
     1  '____________ ____________ _______',
     1  ' ____ ____ _______',      
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______ _______ _______')
     
 253  format(
     1  '____________ ____________ _______',
     1  ' ____ ____ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______',
     1  ' _______ _______ _______ _______ _______ _______ _______')
 
     
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
     1 '   Source = ', a12, ' Percent =', f8.3, '   Status = ', a3)    
 
 310  FORMAT(//,72('_'),//
     1 'WWSP Summary', a5,/
     1 'Plan Type      = ',i5, 1x, a25)

 320   format('  OutWW; Reading Binary Operating Rule file'/,
     1              '  OutWW (8a); np, pID,  ifound, k, ityopr iop',
     1              ' i46, p46',/, a14, 1x,a12, 1x, 6i5,20f8.0)

c
c _________________________________________________________ 
c        Error Messages
c
c                                 
c rrb 2021/04/18; Compiler warning
cx900 write(6,*)  '  Stopped in outWW; see the log file (*.log)' 
 9999 write(6,*)  '  Stopped in outWW; see the log file (*.log)'
      write(nlog,*) '  Stopped in outWW'                        
      write(6,*) 'Stop 1' 
      call flush(6)
      call exit(1)

      stop 
      END
