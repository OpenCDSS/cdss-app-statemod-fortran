c
      Subroutine GetPlnW(maxdvrX)
c
c
c _________________________________________________________
c	Program Description
c
c		It reads Plan Well association relationship      
c	
c		Called by Execut
c
c _________________________________________________________
c
c	Documentation
c
c		iin2 response file number
c
c		iprintA	Print warnings if not found
c		iprintT Print error if a match is not found
c
c		cistatP Plan ID
c		cistatW Well Right ID
c		cistatS Well Structure ID
c
c		iplanw(i) Plan associated with well right i
c		
c		idum = 0 Plan well is not tied to a well right
c		idum = 1 Plan well is tied to 1 well right
c		idum > 1 Plan well is tied more than 1 well right
c
c
c _________________________________________________________
c	Dimensions
c
c
      include 'common.inc'
      dimension idumP(maxdvrX)
      
      character 
     1  cistatW*12, cistatP*12, cistatS*12, namedwX*24,
     1  rec12*12, cfound*3, rec24*24, cidvri*12
c     
c _________________________________________________________
c
c		Step 1; Initilize
c		iout =  0 no details
c			1 details
c			2 summary
      iout=0
      
      if(iout.ge.1) write(nlog,100) 
 100  format(/,72('_'),/'  Subroutine GetPlnW')   
 
      cidvri='GetPlnW'
      iplanWon=0      
      nplanw=0    
      iprinto=0 
      iprintA=0 
      iprintT=0
      iprob=0
      cfound='NA '
      small=0.001
c
c rrb 2010/09/15; Revised to a variable dimension      
cr      if(maxdvrW*2.gt.22000) then
cr        goto 9999
cr      endif
      
      ndum=0
      do i=1,maxdvrW   
        idumP(i)=0
      end do  
      
      do i=1,numdvrW
        iplanw(i)=0
      end do  
      
      do np=1,nplan
        rdvnkwP(np)=0.0
        dumx(np)=0.0
      end do
c     
c _________________________________________________________
c
c		Step 2; Open File
      ifn=63
      rec256=fileName(ifn)
      filena=rec256(1:72)
        
      if(filena(1:2).eq.'-1') then
        if(iout.gt.0) write(nlog,*) 
     1   ' GetPlnW; FYI no Well Augmentation Plan data provided'
        i=1
        nplanw=0
        goto 200
      endif     

      call putpath(maxfn, filena, fpath1)
      open(55, file=filena,status='old',err=928)
      iin2=55
      iplanWon=1
c     
c _________________________________________________________
c
c		Step 3; Print file Opened     
      write(nlog,113)
  113 format(/,72('_'),/,
     1 '  GetPlnW; Plan Well File (*.plw) ')

      write(nlog,270) filena
      call skipn(55)
c     
c _________________________________________________________
c
c		Step 4; Read Data
c
c		Loop for maximum number of well rights (maxdvrW)      
c
c rrb 2006/12/01; Insure EOR is found
c     do i=1,maxdvrW   
c
c rrb 2009/10/15; Insure dimension is not exceeded
cr    do i=1,maxdvrW*2   
      do i=1,maxdvrW  
c           
c		Note iocode 1 = Data, 2 = EOF, 3 = Error                    
        call comment(55, nlog, iocode, nchk, 0)
        if(iocode.eq.2) goto 200
        if(iocode.eq.3) goto 928      
  
        read(55,*,end=200, err=928) cistatP, cistatW, cistatS
        
        if(iout.eq.1) then
          write(nlog,*) ' '
          write(nlog,*) ' GetplnW; Read ', i, cistatP, cistatW,cistatS
        endif
           
        cistatP=adjustl(cistatP)
        cistatW=adjustl(cistatW)
        cistatS=adjustl(cistatS)
c     
c _________________________________________________________
c
c		Step 5; Find Well Right Pointer (iplanWR)
c
c		Find well Id (cistatW) in Well Right list (type 16)
        iplanWr=0
        do k=1,numdvrw
          nw=idivcow(1,k)
          rec12=cdividw(nw)
          if(crigidw(k).eq.cistatW) then          
c
c ---------------------------------------------------------
c rrb 2007/03/03; Try to find a unique match (idumP(i)=0) then
c		  a duplicate
c           if(rec12.eq.cistatS) iplanWr=k
            if(rec12.eq.cistatS .and. idumP(k).eq.0) iplanWr=k
            
            if(iout.eq.1) then
              write(nlog,*) 
     1          ' GetPlnW; Unique Right cistatW, cistatS, iplanWR = ',
     1          i, cistatW, cistatS, iplanWR
              write(nlog,*)
     1          ' GetPlnW; Right crigidw, rec12,   iplanWR = ',
     1          i, crigidw(k), rec12, iplanWR
            endif            
          endif  
        end do
c
c ---------------------------------------------------------
c rrb 2007/03/03; Try to find a non unique match (idumP(i)=0) then
        if(iplanWr.eq.0) then
          do k=1,numdvrw
            nw=idivcow(1,k)
            rec12=cdividw(nw)
            if(crigidw(k).eq.cistatW) then          
              if(rec12.eq.cistatS) iplanWr=k
              
              if(iout.eq.1) then
                write(nlog,*) 
     1            ' GetPlnW; Duplicate Right ',
     1            'cistatW, cistatS, iplanWR = ',
     1            i, cistatW, cistatS, iplanWR
                write(nlog,*)
     1            ' GetPlnW; Right crigidw, rec12,   iplanWR = ',
     1            i, crigidw(k), rec12, iplanWR
              endif            
            endif  
          end do
        endif
c
c ---------------------------------------------------------
c        
        if(iout.eq.1) then
          write(nlog,*) ' GetPlnW; Right cistatW, iplanWR = ',
     1      i, cistatW, cistatS, iplanWR
        endif  
c     
c _________________________________________________________
c
c		Step 6; Find Augmentation Plan Pointer (iplanP)
c		(type 7). Stop if not found (istop =1)
c  
        if(iplanWR.eq.0) then         
          if(iprintA.eq.0) then
c           write(nlog,228)
            write(nchk,230) 
          endif  
c
          iprintA=iprintA+1
          write(nchk,240) iprintA, cistatW, namedw(i), cistatP, cistatS 
          cfound=' No'
        else        
          istop=0               
          dumc=0
          rops2=0.0
          
          call oprfind(ityoprX, 7, idumc,i,ion,iprinto,
     1         iplanP,iops2, nx, cistatP, iacc, istop, rops2, 
     1         ioprsw1,cidvri)
     
          if(iout.eq.1)
     1      write(nlog,*) ' GetPlnW; Plan cistatP, iplanWR, iplanP = ',
     1        i,cistatP,cistatW, cistatS, iplanWR, iplanP 
c     
c _________________________________________________________
c
c		Step 7; Set Well right Pointer (iplanWR) to
c                       Augmentation Plan Pointer (iplanP)		
c

          nplanW=nplanW+1            
c
          iplanw(iplanWR)=iplanP     
c
c		Check proper type of plan (type 2 is well agumentation)              
          iok=0
          if(iplnTyp(iplanP).eq.2 .or. iplnTyp(iplanP).eq.10) iok=1
          if(iok.eq.0) then
            iprob=1
            if(iprintT.eq.0) write(nlog,250)
            iprintT=iprintT+1  
            write(nlog,260) iprintT, cistatW, cistatP, iplntyp(iplanP)            
          endif 
          
          idumP(iplanWR)=idumP(iplanWR)+1  
          
c
c ---------------------------------------------------------
c rrb 2009/03/16; Check output to *.chk
cx          if(idumP(iplanWR).gt.1) then     
cx            if(cistatW.eq.'0105187     ') then
cx              write(nchk,*) ' GetPlnW for 0105187', 
cx     1         i1, iplanWR, cistatW, cistatP, idumP(iplanWR)
cx            endif
cx          endif
          
          if(idumP(iplanWR).gt.1) ndum=ndum+1
          
          cfound='Yes'
c
c rrb 2008/10/07; Begin to calculate weighted well priority
c		  Note denominator (dumx) is the total decree for this plan
          rdvnkwp(iplanP)  = rdvnkwp(iplanP) + 
     1      rdvnkw(iplanWR) * dcrdivw(iplanWR)       
          dumx(iplanP) = dumx(iplanP) + dcrdivw(iplanWR)
        endif  
c
c		Print summary to *.log        
        if(iout.ge.2) then
          if(nplanW.eq.1) write(nlog,280)
          rec24='NA                      '
          if(cfound.eq.'Yes') rec24=pname1(iplanP)
          write(nlog,290) i, cistatP, rec24, cistatW,
     1     iplanWR, iplanw(iplanWR), cfound, nplanW, iprintA,
     1     idumP(iplanWR)
        endif
        
      end do  
c     
c _________________________________________________________
c
c		Step 8; Warn dimension exceeded
      goto 924
c     
c _________________________________________________________
c
c		Step 9; Print Warnings and Summaries
  200 nplanIN=i-1
c
c ---------------------------------------------------------
c		Print warning some well plans are not tied to a well  
      if(iprintA.gt.0) write(nlog,228)

c
c ---------------------------------------------------------
c		Print warning wells tied to multiple plans
      i1=0      
      ndumT=0
      do i=1,numdvrw 
        if(idumP(i).gt.1) then
          i1=i1+1
          if(i1.eq.1) then
cx          write(nlog,229)
            write(nchk,232)
          endif  
          ip=iplanw(i)
          ndumT=ndumT+idumP(i)-1
          write(nchk,233) i1, crigidw(i), namedw(i), Pid(ip), 
     1      pname1(ip), idumP(i), ndumT
        endif
      end do  
c
c ---------------------------------------------------------
c	  Print warning to *.log 
      if(i1.gt.0) then
        cp=float(i1)/float(numdvrw)*100.
        write(nlog,229) i1, numdvrw, cp
      endif                  
       

c
c ---------------------------------------------------------
c		Print warning to *.chk for well rights not tied to a plan
      i1=0      
      ndumT=0
      do i=1,numdvrw 
        if(iplanw(i).eq.0) then
          i1=i1+1
          if(i1.eq.1) then
cx          write(nlog,234)
            write(nchk,235)
          endif  
          ip=iplanw(i)
          iwx=idivcow(1,i)
          ndumT=ndumT+idumP(i)-1
          write(nchk,236) i1, crigidw(i), namedw(i), cdividw(iwx),
     1      dcrdivw(i)
        endif
      end do  
c
c ---------------------------------------------------------
c	  Print warning to *.log
      noPlan=i1      
      if(i1.gt.0) then
        cp=float(noPlan)/float(numdvrw)*100.
        write(nlog,234) noPlan, numdvrw, cp
      endif                  
      
c
c ---------------------------------------------------------
c		Print Summary      
      if(nplanIN.gt.0 .or. iout.ge.1) then
cx      nx=max(numdvrw-nplanw,0)
cx        write(nlog,210) nplanIN, nplanw, nplanIn-nplanw,
cx     1   ndum, numdvrw, nx
cx        write(nchk,210) nplanIN, nplanw, nplanIn-nplanw,
cx     1   ndum, numdvrw, nx

        write(nlog,210) nplanIN, nplanw, nplanIn-nplanw,
     1   ndum, numdvrw, noPlan
        write(nchk,210) nplanIN, nplanw, nplanIn-nplanw,
     1   ndum, numdvrw, noPlan
      endif
      
c
c _________________________________________________________
c
c rrb 2008/10/07; Calculate average well priority
      do np=1,nplan
        if(dumx(np).gt. small) then
          rdvnkwp(np)  = rdvnkwp(np) / dumx(np)
        else
          rdvnkwp(np)  = 0.0
        endif
      end do
      
c
c _________________________________________________________
c
c		Step 10; Warn if plan data was expected 
c                 but not provided     
      if(filena(1:2).ne.'-1' .and. nplanW.eq.0) then
        write(nlog,220) nplanW
        goto 9999
      endif   
     
      close(55)
c
c _________________________________________________________
c
c		Step 11; Stop if a problem as found
c                 but not provided     
      if(iprob.eq.1) goto 9999
      
c      
c _________________________________________________________
c
c		Step 11; Return
      return
 
c      
c _________________________________________________________
c
c               Formats
 210  format(/,72('_'),/
     1 '  GetPlnW;  ', 
     1     'Number of Well Plan Well Right Combinations     = ', i5,//
     1 12x,'Number of Well Plans TIED to a Well Right       = ', i5,/
     1 12x,'Number of Well Plans NOT TIED to a Well Right   = ', i5,/
     1 12x,'Number of Well Plans TIED TO MORE than 1 Right  = ', i5,//
     1 12x,'Number of Well Rights                           = ', i5,/
     1 12x,'Number of Well Rights NOT TIED TO a Well Plan   = ', i5,//
     1 12x,'Note StateMod expects a free format for this file.',/
     1 12x,'Therefore be sure Names and IDs have no blanks',/
     1 12x,'or enclosed in double quotes (e.g. revise My Name',/
     1 12x,'to My_Name or "My Name").')
     
 220    format(/,72('_'),/
     1 '  GetPlnW;  ',
     1     'Problem provided a plan file but the number of ',
     1           'well plans read = ',i5,/
     1 12x,'Reconmmend you check plan input or remove the file')
     
 228  format(/,72('_'),/
     1 '  GetPlnW;  ',
     1     'Warning data in Plan Well file (*.plw)',/
     1 12x,'not found in Well Right File (*.wer)',/
     1 12x,'See *.chk for a listing of all occurances',/
     1 12x,'Note: OK if this wells pumping is not simulated')
          
     
 229  format(/,72('_'),/
     1 '  GetPlnW;  ',
     1     'Warning well rights tied to more than one Augmentation Plan',/
     1     'in the',/
     1 12x,'Plan Well file (*.plw) = ',i5,' of ',i5,' or ',f5.0,'%',/     
     1 12x,'See *.chk for a listing of all occurances')
     
 230  format(/,72('_'),/
     1 '  GetPlnW;  ',
     1     'Warning data in Plan Well file (*.plw)',/
     1 12x,'not found in Well Right File (*.wer) or',/
     1 12x,'the Structure ID in *.wer and *.plw are not equal',/
     1 12x,'See *.chk for a listing of all occurances',/
     1 12x,'Note: OK if this wells pumping is not simulated',//
     1 '    # W.Right ID   W.Right Name             Plan ID     ',
     1 ' Structure ID',/
     1 '_____ ____________ ________________________ ____________',
     1 ' ____________')
     
 232  format(/,72('_'),/
     1 '  GetPlnW;  ',
     1     'Warning data in Plan Well file (*.plw)',/
     1 12x,'tied to more than one Augmentation Plan',/
     1 12x,'Note: Only last Plan is listed',//
     1 '    #',
     1 ' W.Right ID   W.Right Name            ',
     1 ' Plan ID      Plan Name               ',
     1 '  # Plans    Total',/
     1 '_____',
     1 ' ____________ ________________________',
     1 ' ____________ ________________________',
     1 ' ________ ________')
     
 233  format(i5, 1x, a12,1x, a24,1x, a12,1x, a24,1x, i8, 1x,i8)
     
 234  format(/,72('_'),/
     1 '  GetPlnW;  ',
     1     'Warning well rights not assigned to a plan in the ',/
     1 12x,'Plan Well file (*.plw) = ',i5,' of ',i5,' or ',f5.0,'%',/
     1 12x,'See *.chk for a listing of all occurances')
     
 235  format(/,72('_'),/
     1 '  GetPlnW;  ',
     1     'Warning well rights not assigned to a plan in the ',/
     1 12x,'Plan Well file (*.plw) or the Structure ID ',/
     1 12x,'in the well right file (*.wer) and Plan well',/
     1 12x,'file (*.plw) are not equal',/
     1 '    #',
     1 ' W.Right ID   W.Right Name             Structure ID',
     1 '    Right',/
     1 '_____',
     1 ' ____________ ________________________ ____________',
     1 ' ________')

 236  format(i5, 1x, a12,1x, a24,1x, a12, 1x, f8.2)

 240  format(i5, 1x,a12, 1x,a24, 1x,a12, 1x,a12)     
 
 250  format(/,72('_'),/
     1 '  GetplnW;  ',
     1     'Problem the Plan specified is incorrect for ',/
     1 12x,'the following: It should be type 2 (well augmentation)',/
     1 12x,'or type 10 (Special Well Augmentation)',//
     1 '    # Right ID     Plan ID       Type',/
     1 '_____ ____________ ____________ _____')
 260  format(i5, 1x, a12, 1x, a12, 1x, i5)    
 270  format(4x, a256)
 
 280  format(/,72('_'),/
     1 '  GetplnW;  ',
     1     'FYI Plan Well Data Summary. Note:',/
     1 12x,'iplanWR = Well Water Right pointer',/
     1 12x,'iplanW  = Augmentation Plan pointer',/
     1 12x,'Found   = Yes means the well was tied to a well right',/
     1 12x,'# Tied  = Number of Well Plans TIED to a Well Right',/
     1 12x,'# No Tie= Number of Well Plans NOT TIED to a Well Right',/
     1 12X,'Count   = Number of times a Well Right is tied to a Plan',//
     1 '    # Plan ID      Plan Name                Right ID    ',
     1 '  iplanWR  iplanW   Found   # Tied # No Tie    Count',/
     1 '_____ ____________ ________________________ ____________',
     1 ' ________ _______ _______ ________ ________ ________')
     
 290  format(i5, 1x,a12, 1x, a24, 1x,a12,1x, 2i8, 5x,a3, 3(1x,i8))
 
     
 320  format(i5,1x,a12,1x,a12)    
   
c      
c _________________________________________________________
c
c               Error Tracking
  924 write(nlog,925) maxdvrW
  925 format(/,72('_'),/,
     1 '  GetPlnW; Problem number of Plan Wells exceeds the dimension',
     1 ' of ', i5,/, 10x, ' Revise StateM and Common.')
      goto 9999
      
  926 write(nlog,927) iin2, filena
  927 format(/,72('_'),/
     1 ' GetPlnW; Problem. End of file # ', i4, ' encountered',/,
     1 '   File name: ', a256)
      goto 9999
c
  928 write(nlog,929) iin2, filena
  929 format(/,72('_'),/
     1 ' GetPlnW; Problem reading file # ', i4,/,
     1 '   File name: ', a256,/
     1 '   Problem record (next line):')

      backspace(iin2)
      read(iin2, '(a256)',end=926,err=926) recin
      write(nlog,'(a256)') recin
      goto 9999
      
 9999 write(6,1440) 
      write(nlog,1450) 
      call flush(6)
 1440 format(/, 72('_'),/,
     1 ' GetPlnW;  Stopped in GetPlnW, see the log file (*.log)')
 1450 format(/, 72('_'),/,' GetPlnW; Stopped in GetPlnW')
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)
      
      stop
      end
