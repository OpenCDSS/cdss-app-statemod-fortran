c
        subroutine getExPt(nlog, maxdivw, maxuse, maxres, maxplan,
     1    itypeEx, ndS, ndD, ndP, idvsta, irssta, ipsta, 
     1    ndnnod, idncod, iopsouX, cidvri)
c
c _________________________________________________________
c	Program Description
c
c       GetExPt; It processes replacement reservoir information
c       
c
c _________________________________________________________
c	Documentation
c
c               irepexp(n,nd) = Exchange point
c                               (n=rep reservoir, nd = diversion)
c                                0=N/A (direct supply)
c                                +=river ID of exchange point
c               ndS             source structure
c	
c		ndD             if > 0 and NDP=0 
c                                  destination diversion ID
c		ndP             if > 0 and NDP>0 
c                                  destination plan ID
c		ndD             if < 0, destination reservoir ID
c               iopsouX         exchange point (typically stored
c				as iopsou(6,k)
c
c               iout          = 0 no detailed printout
c                               1 yes detailed printout
c
c _________________________________________________________
c	Dimensions
       dimension idvsta(maxdivw), irssta(maxres), ipSta(maxplan),
     1   ndnnod(maxuse), idncod(maxuse)
       character cidvri*12, cDest*12
c     
c _________________________________________________________
c               Step 1; Initilize

      iout = 0
      
      ndd2=0
c
c _________________________________________________________
c		Set Source Data (a right located at diversion ndS)
      idcdS=idvsta(ndS)
      ndndS=ndnnod(idcdS)
      issS=idcdS
c
c _________________________________________________________
c		Destination is a Diversion
c rrb 2006/4/25; Allow a destination plan		 
c     if(ndd.gt.0) then                  
      if(ndd.gt.0 .and. ndp.eq.0) then                  
        idcdD=idvsta(ndD)
        ndd2=ndd
        cDest='Diversion   '
      endif  
c
c _________________________________________________________
c		Destination is a Plan
c
c rrb 2006/4/25; Allow a destination plan		 
      if(ndd.gt.0 .and. ndP.gt.0) then                  
        idcdD=ipSta(ndP)
        ndd2=ndP
        cDest='Plan        '
      endif  
c
c _________________________________________________________
c		Destination is a reservoir
      
      if(ndd.lt.0) then
        ndR=-ndD        
        idcdD=irssta(ndr)
        ndd2=ndR
        cDest='Reservoir   '        
      endif    
      
      ndndD=ndnnod(idcdD)
      issD=idcdD
c
c _________________________________________________________
c
c               Step 2; Determine if the structure can be served 
c                       by a direct replacement or exchange
      if(itypeEx.eq.0) then
c
c               Error with network
        if(issS.eq.0.or.ndndS.le.1) then
            write(nlog,190) cidvri, idcdS, idcdD
          goto 9999
        endif
c
c               Search every river node downstram of the reservoir
c               to see if it can be served directly
cr       do ndS=1,ndndS-1
cr         if(issS.eq.idcdD) then
cr           irepexp(n,nd) = 0
cr           goto 150
cr         endif
cr         issS=idncod(issS)                        
cr       end do
      endif
c
c               Could not be served directly
c               Must be served by an exchange
c               Find the exchange point
c
c _________________________________________________________
c
c               Step 3; Determine if the structure can be served 
c                       Exchange
      if(itypeEx.eq.1) then
c
c               Error with network
        if(issS.eq.0 .or. ndndS.le.1 .or. 
     1     issD.eq.0 .or. ndndD.le.1) then
            write(nlog,190) cidvri, idcdS, idcdD
          goto 9999
        endif    
        
        issS=idcdS
        issD=idcdD
c
c		Loop downstream of the source (ndndS)
        do ns=1,ndndS-1
c
c		Search for a match downstream of the destination           
          do nd=1,ndndD
            if(issS.eq.issD) then
              iopsouX=issS
              go to 150
            endif
            issD=idncod(issD)
c
c		Problem             
            if(issD.eq.0) then
              write(Nlog,210) cidvri, idcdS, idcdD
              goto 9999
            endif 
          end do
           
          issS=idncod(issS)
        end do
      endif  
c
c _________________________________________________________
c		Nothing found, Print problem
      write(nlog,210) cidvri, idcdS, idcdD
      goto 9999
         
c
c _________________________________________________________
c
c               Step 4; Print results
 150  continue
c
c		Detailed output       
      if(iout.eq.1) then
        write(nlog,200) cidvri, cDest, idcdS, idcdD, iopsouX
      endif  
c
c		Summary Output        
      if(iout.ge.1) then
        write(nlog,220)
        write(nlog,230) idcdS, idcdD, iopsouX
      endif  
      goto 500
c
c _________________________________________________________
c
c               Step 5; Error Processing

 9999 write(6,*) ' GetExPt; Stopped in GetExPt' 
      write(6,*) '          See the log file'       
      write(nlog,*) ' GetExPt; Stopped in GetExPt'
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)
      stop

c
c _________________________________________________________
c
c               Step 6; Return

 500  return
c
c _________________________________________________________
c
c               Formats

 190  format(/,'  GetExPt; Problem with Operation Right ', a12,/,                      
     1 11x,'The destination pointer = ', i5, 
     1     ' and Source pointer = ', i5, 
     1 11x,'have a problem related to the network')
     
 200  format(/,'  GetExPt;  Operating Right ', a12,/
     1 11x,' Finding an exchange point for a ', a12/
     1 11x,' Destination Location           = ', i5,/
     1 11x,' Source Location                = ', i5,/
     1 11x,' Exchange Point (Location)      = ', i5)

 210  format(/,'  GetExPt; Problem with Operation Right ', a12,/
     1 11x,'The destination cannot be served by exchange',/
     1 11x,'The destination pointer = ', i5, 
     1     ' and Source pointer = ', i5)
     
 220  format(/,'  GetExPt; Exchange Point iopsou) found',/
     1  '      Source Destination Exchange Pt',/
     1  '     (idcdS)     (idcdD)   (iopsouX)',/   
     1  ' ___________ ___________ ___________')

 230  format(20i12)

      stop
      END                                                               

