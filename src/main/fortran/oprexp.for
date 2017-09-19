c
c *********************************************************
        subroutine oprExp(nlog, maxsta, idcdD, idcdS, idncod, ndnnod, 
     1                    iopsouX, cidvri)
c
c _________________________________________________________
c	Program Description
c
c
c               OprExp; it finds an exchange point between
c                       a destination station idcdD 
c		                  	and a supply station idcdS
c
c		iopsouX = exchange point
c
c _________________________________________________________
c	Documentation
c		nlog	= output file
c		maxsta	= dimension of river nodes
c		idcdD	= destination node
c		idcdS	= source node
c		idncod	= network linkage
c		ndnndo	= number of downstream nodes
c		iopsouX	= exchange point
c		cidvri	= operating rule ID
c
c _________________________________________________________
c	Dimensions
c
        dimension idncod(maxsta), ndnnod(maxsta)
        character cidvri*12
        
c		      
c _________________________________________________________
c		Step 1; Initilze
c
        iout=0
              
        ndndD=ndnnod(idcdD)      
        ndndR=ndnnod(idcdS)
        
        if(iout.eq.1) then
          write(nlog,*) ' OprexP; idcdD, idcdS', idcdD, idcdS
          write(nlog,*) ' OprexP; ndndD, ndndR', ndndD, ndndR
        endif
        
        issS=idcdS
c
c		Loop for every staion below the Source (idcdS)        
        DO 110 NDr=1,NDNdR-1
          issD=idcdD
c
c		Loop for every station below the Destiation (idcdD)          
          do 100 ndd=1,ndndD-1
            if(issS.eq.issD) then
              if(ndd.eq.1) then
                write(nlog,220) cidvri, idcdS, idcdD
                goto 9999
              endif
              iopsouX=issS
              goto 500
            endif
            issD=idncod(issD)
 100      continue
          issS=idncod(issS)
 110    continue

cx      write(nlog,220) cidvri, idcdS, idcdD
        goto 9999
c
c _________________________________________________________
        
 500    if(iout.eq.1) then 
          write(nlog,210) cidvri, iopsouX
        endif
        return       
c
c _________________________________________________________
c
c		Formats        
 210   format(/
     1 '  OprExp; Exchange point for operating rule ', a12,/
     1 '          Found at river node (iopsouX) = ', i5)
     
 220   format(/, 72('_'),/
     1 '  OprExp; Problem with Operating Rule ', a12,/
     1 '          The exchange point cannot be found',/ 
     1 '          Source = ', i5, ' Destination = ',i5,/
     1 '          Reconmend you check:',/
     1 '            1. Can the destination can be served directly',/
     1 '            2. Is your network connected properly')
c
c _________________________________________________________
c
c		Error Processing     
 9999 write(nlog,*) ' OprExp (Oprinp); Problem see log file (*.log)'
      write(6,*) 'Stop 1'
      call flush (6)
      call exit(1)

      stop 
      end
