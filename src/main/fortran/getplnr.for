c
      Subroutine GetPlnR
c
c
c _________________________________________________________
c	Program Description
c
c
c		It reads Plan Reservoir relationship      
c	
c		Called by Execut
c _________________________________________________________
c
c	Documentation
c
c		iin2 file # (55)
c
c		cistatP Plan ID
c		cistatW Reservoir Right ID (not used)
c		cistatS Reservoir Structure ID
c		iprAcct(ip) Reservoir account associated with 
c			 plan ip
c
c
c _________________________________________________________
c	Dimensions
      include 'common.inc'
      
      character 
     1  cistatP*12, cistatW*12, cistatS*12,
     1  rec12*12, cfound*3, rec24*24, cidvri*12
c     
c _________________________________________________________
c
c		Step 1; Initilize
c		iout =  0 no details
c			1 details
c			2 summary
      iout=2
      iout=0
      
      if(iout.ge.1) write(nlog,100) 
 100  format(/,72('_'),/'  Subroutine GetPlnR')   
 
      
      nplanR=0  
      cidvri='GetPlnR'  

      do i=1,numres
        iresP(i)=0
      end do  
c
c rrb 2009/03/10; Since many reservoirs may be tied to the same plan
c		    iplnR(i) is non unique and not required.
cx      do i=1,nplan
cx        iplnR(i)= 0
cx      end do
c     
c _________________________________________________________
c
c		Step 2; Open File
      ifn=79
      rec256=fileName(ifn)
      filena=rec256(1:72)
        
      if(filena(1:2).eq.'-1') then
        if(iout.gt.0) write(nlog,*) 
     1   ' GetPlnR; FYI no Plan Recharge data provided'
        i=1
        nplanR=0
        goto 200
      endif     

      call putpath(maxfn, filena, fpath1)
      open(55, file=filena,status='old',err=928)
      iin2=55
      iplanRon=1
      call skipn(55)
c     
c _________________________________________________________
c
c		Step 3; Print file Opened     
      write(nlog,113)
  113 format(/,72('_'),/,
     1 '  GetPlnR; Plan Recharge File (*.plr) ')
      write(nlog,270) filena
c     
c _________________________________________________________
c
c		Step 3; Summary Output header
      
      if(iout.eq.2) then
        write(nlog,*)
     1    ' GetPlnR; Plan cistatP      cistatW      cistatS     ',
     1    '      np      nr   iresP'
      endif
      
c     
c _________________________________________________________
c
c		Step 4; Read Plan Reservoir Data (*.plR)
c
c		Loop for maximum number of reservoir accounts
      do i=1,maxown
c           
c		Note iocode 1 = Data, 2 = EOF, 3 = Error                    
        call comment(55, nlog, iocode, nchk, 0)
        if(iocode.eq.2) goto 200
        if(iocode.eq.3) goto 928      
  
        read(55,*,end=200, err=928) 
     1    cistatP, cistatW, cistatS, iPRacct1
        
        if(iout.eq.1) then
          write(nlog,*) ' '
          write(nlog,*) ' GetPlnR; Read ', 
     1     i, cistatP, cistatW, cistatS, iPRacct1
        endif
           
        cistatP=adjustl(cistatP)
        cistatW=adjustl(cistatW)
        cistatS=adjustl(cistatS)
c     
c _________________________________________________________
c
c		Step 5; Find Reservoir Pointer (iplanWR)
c
c		Find reservoir station custatS
c		Note istop=1 OK if not found
        istop=1  
        itype=2
        ityoprX=-1             
        dumc=0
        iacc=0
        rops2=0.0
        call oprfind(ityoprX, itype, idumc,i,ion,iprinto,
     1       nr,iops2, nx, cistatS, iacc, istop, rops2, 
     1       ioprsw1, cidvri)
     
        if(nr.le.0) then     
          write(nlog,226) cistatS
          goto 9999
        endif
c
c		Check reservoir account
        if(iPRacct1.le.0) goto 930
c     
c _________________________________________________________
c
c		Step 5; Find Plan Pointer (iplanWR)
c		Note: istop=1 OK if not found
        istop=1  
        itype=7
        ityoprX=-1             
        dumc=0
        iacc=0
        rops2=0.0
        call oprfind(ityoprX, itype, idumc,i,ion,iprinto,
     1       np,iops2, nx, cistatP, iacc, istop, rops2, 
     1       ioprsw1, cidvri)
       
        if(np.gt.0) then     
          nplanR=nplanR+1
          iprAcct(np)=iprAcct1
cx          iplnR(np)=nr
          iresP(nr)=np
        else
          write(nlog,228) cistatP
          goto 9999
        endif
c
c		Check plan type
        if(iplnTyp(np).ne.8) goto 922
c
c		Detailed reporting        
        if(iout.eq.2) then
          write(nlog,'(a10,i5, 1x,a12, 1x,a12, 1x,a12, 20i8)') 
     1      '  GetPlnR;',i, cistatP, cistatW, cistatS, np, nr, 
     1      iresP(nr)
        endif
      end do
c     
c _________________________________________________________
c		Abnormal Exit; Goto error processing
      goto 924
c     
c _________________________________________________________
c		Normal Exit
c	
 200  write(nlog,210) nplanR
      close(55)

c _________________________________________________________
c
c		Step 11; Return
      return
 
c      
c _________________________________________________________
c
c               Formats
 210  format(
c    1 /,72('_'),
     1 /,
     1 '  GetPlnR;  ', 
     1     'Number of Plan Reservoirs Combinations     = ', i5)
     
 226  format(/,72('_'),/
     1 '  GetPlnR;  ',
     1     'Warning the Plan Recharge file (*.plr)',/
     1 12x,'has a Reservoir ID = ', a12, /
     1 12x,'that is not in the Reservoir Station File (*.res)',/
     1 12x,'Reconmend you revise the Plan Recharge File (*.plr)',/
     1 12x,'or the Reservoir Station File (*.res)')
     
 228  format(/,72('_'),/
     1 '  GetPlnR;  ',
     1     'Warning the Plan Recharge file (*.plr)',/
     1 12x,'has a Plan ID = ', a12, 
     1 12x' that is not in the plan Plan File (*.pln)',/
     1 12x,'Reconmend you revise the Plan Recharge File (*.plr)',/
     1 12x,'or the Plan Station File (*.pln)')
     
 270  format(4x, a256)     
c      
c _________________________________________________________
c
c               Error Tracking
  922 write(nlog,923) cistatP, cistatS, iplnTyp(np)
  923 format(/,72('_'),/,
     1 '  GetPlnR; Problem with the Plan Reservoir file (*.plR)',/
     1 10x,' For Plan ID               = ', a12,/
     1 10x,' and Reservoir Station ID  = ', a12,/
     1 10x,' The Plan Type             = ', i3,/
     1 10x,' It should be  type =   8 (recharge)',/
     1 10x,' Reconmend you revise the Plan res file (*.plR) or',/
     1 10x,' the Plan Station file (*.pln)')
      goto 9999

  924 write(nlog,925) maxown
  925 format(/,72('_'),/,
     1 '  GetPlnR; Problem number of Plan Reservoirs exceeds the',/
     1 ' dimension of ', i5,/, 10x, ' Revise StateM and Common.')
      goto 9999
      
  926 write(nlog,927) iin2, filena
  927 format(/,72('_'),/
     1 ' GetPlnR; Problem. End of file # ', i4, ' encountered',/,
     1 '   File name: ', a256)
      goto 9999
c
  928 write(nlog,929) iin2, filena
  929 format(/,72('_'),/
     1 ' GetPlnR; Problem reading file # ', i4,/,
     1 '   File name: ', a256,/
     1 '   Problem record (next line):')

      backspace(iin2)
      read(iin2, '(a256)',end=926,err=926) recin
      write(nlog,'(a256)') recin
      goto 9999
      
  930 write(nlog,932) cistatP, cistatS, iPRacct(i)
  932 format(/,72('_'),/,
     1 '  GetPlnR; Problem with the Plan Reservoir file (*.plR)',/
     1 10x,' For Plan ID               = ', a12,/
     1 10x,' and Reservoir Station ID  = ', a12,/
     1 10x,' The account               = ', i3,/
     1 10x,' It should be > 0',/
     1 10x,' Reconmend you revise the Plan res file (*.plR)')
      goto 9999
      
 9999 write(6,1440) 
      write(nlog,1450) 
      call flush(6)
 1440 format(/, 72('_'),/,
     1 '  GetPlnR;  Stopped in GetPlnR, see the log file (*.log)')
 1450 format(/, 72('_'),/,'  GetPlnR; Stopped in GetPlnR')
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)
      
      stop
      end
