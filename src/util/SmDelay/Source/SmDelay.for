c
c
c       SmDelay; It reads a sequential StateMod input file
c               and prints it as a random input file

        include 'SmDelay.inc'        
c _________________________________________________________       
c

        ver=1.0
        vdate='2006/03/15'
        nlog=99
        maxdlm=240
        
        maxfile = 50
c       open(nout, file='SmDelay.out', status='unknown')
        open(nlog, file='SmDelay.log', status='unknown')
        write(nlog,530) ver, vdate
c
c _________________________________________________________       
c
c		Get response file from the command line        
        maxcl=127
        filenx='-1'
        filelog='SmDelay.log'
        call parse(maxcl, ioptio, ioptio2,
     1             filenx, filelog, filenc)
     
        write(nlog,*) ' SmDelay; Response file = ', filenc
c
c _________________________________________________________       
c
c		Open Response file
        if(filenc(1:2).eq.'-1') then
          write(6,520)
          write(nlog,520) 
          goto 900     
        endif
        open(20,  file=filenc,  status='old')
c
c _________________________________________________________       
c
c		Read input file        
        call skipn(20)
        read(20,*,end=500,err=500) fileN1
        write(Nlog,*) ' '
        write(nlog,*) ' Input file  = ', fileN1
        open(55, file=fileN1, status='old')
c
c _________________________________________________________       
c
c		Read out file        
        call skipn(20)
        read(20,*,end=500,err=500) fileN1
        write(nlog,*) ' Output file = ', fileN1
        open(10, file=fileN1, status='unknown')
        write(10,530) ver, vdate
c
c _________________________________________________________       
c
c		Get monthly return data
        call GetDly        
c
c _________________________________________________________       
c
c		Get monthly return data
      nmax=3100
      ichk1=0
      ityp=15
      iyr=0      
      monx=1
      mon=1
      imd=30
      nmax=1   
      n=1
      fx=1.9835*30.
      do i=1,12
        mthday(i)=30
      end do 
        
      write(6,*) ' '
      do is=1,numdly
        write(6,100) is
 100    format('+', ' Processing table ', i5)
        icx=0      
        cidy(1)=cirtnid(is)
        cidx(1)=cirtnid(is)
      
        ndlyx=ndly(is)
c
c		Print ID and counter      
        write(10,400) cidy(1), ndly(is)*30
 400    format(a12, i5)     
c
c		Set up for Daydist, (estimate month 2)
        do j=1,ndlyx
          monx=2
          mon=2
          jmon=mon
          jmonM=mon-1
          jmonP=mon+1
      
          jM=j-1
          jP=j+1
c
c		Set Qm(im,1) to dlyrat(j,is) 
c		Note / fac to get average for month
c		Set first mont -1 to 0.0
          if(jM.gt.0) then
            qm(jmonM,1) = dlyrat(jM,is)/30.
          else
            qm(jmonM,1) = 0.0
          endif  
        
          qm(jmon,1)=dlyrat(j,is)/30.
         
          if(jp.le.ndlyx) then
            qm(jmonP,1)=dlyrat(jP,is)/30.
          else
c
c rrb 2006/03/15; Set last month +1 to last month          
cr          qm(jmonP,1)=0.0
            qm(jmonP,1)=dlyrat(j,is)/30.
          endif  
      
          call daydistD(ichk1,  icx, ityp,iyr,monx,mon,imd,
     1                    mthday, nmax, n, idayU,
     1                    qm,qdx, qd,cidy,cidx)
     
          write(10,410) (qd(id,1), id=1,30)
 410      format(30f10.6)     
c
c		End delay loop for 1 month
        end do
c
c		End delay table loop        
      end do
c
c _________________________________________________________       
c
c		Success
        
      write(6,510) fileN1       
      goto 900
c
c _________________________________________________________       
c
c		Error Processing        
 500    write(nlog,560)
        write(6,560)
        goto 900
c
c _________________________________________________________       
c
c		Formats        
 510    format(
     1    ' SmDelay; Successful Execution',/
     1    '         Log file = SmDelay.log',/
     1    '         New StateMod Daily Delay file (*.urD) = ',a256)
 520    format(
     1    ' SmDelay; Problem',/
     1    '          Response file not found',/
     1    '          To execute the program type SmDelay responsefile')
 530    format(
     1 '# _______________________________________________________'/
     1 '#',/     
     1 '#        SmDelay (create a daily URF file from a Monthly',/
     1 '#',/
     1 '#        State of Colorado'/
     1 '#        Version: ',f5.2,/,
     1 '#        Last revision date: ',a10,/
     1 '#',/
     1 '# _______________________________________________________')
 560   format(
     1    ' SmDelay; Problem reading *.rsp',/
     1    '          See the log file (*.log)')
     
 900    stop
        end
c
c *********************************************************
c
c		GetDly; read delay data
       subroutine getDly
       include 'SmDelay.inc'        
       
       iout=0
       do idl=1,10000
         read (55,*,end=500,err=500)
     1   cirtnid(idl), ndly(idl), (dlyrat(i,idl),i=1,ndly(idl))
     
         if(cirtnid(idl).eq.'            ') goto 500
c
c		Set daily to type 4, connect midpoints         
         idayU(idl)=4
	
         if(iout.eq.1) then    
           write(nlog,300) idl,
     1       cirtnid(idl),ndly(idl), (dlyrat(i,idl),i=1,ndly(idl))
         endif
       
c
c               Sum total pattern
 200	 dlytot(idl) = 0.0
	 do i=1,ndly(idl)
	   dlytot(idl) = dlytot(idl) + dlyrat(i,idl)
         end do
c
 	 if(iout.ge.1) then
 	   if(idl.eq.1) write(nlog,310)
           write(nlog,320) idl,           
     1       cirtnid(idl),ndly(idl), dlytot(idl)
         endif
         
       end do  
c
c _________________________________________________________
c
c		Formats       
 300   format(a12, i5, /,(10f10.2))      
       
 310   format(/,72('_'),/,
     1 '    # ID              #    Tot(%)',/
     1 '_____ ____________ ____ _________')      
 320   format(i5,1x,a12, i5, f10.2)      
       
       
 500  numdly=idl-1
      write(nlog,*) ' ' 
      write(Nlog,510) numdly
 510  format(/,72('_'),/,
     1 '  GetDly; Number of delay tables read = ', i5)
     
      close(55)
c
c _________________________________________________________
c
c		Formats       
      
      return
      end
         
