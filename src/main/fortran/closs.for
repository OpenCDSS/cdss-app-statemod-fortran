c
c *********************************************************
c 
       subroutine Closs(icx)
c
c
c _________________________________________________________
c	Program Description
c
c       Closs; it calcualtes a diversion loss percent (pctlos())
c                when the product of 
c                (return table sum) * (percent on the river) < 100%.
c
c               Repeat for well returns (pctlosw())
c               Repeat for well depletion (pctlosw2()
c
c               Called by Mdainp 1x per simulation for monthly 
c                       return data icx=0
c               Called by Dayest 1x per simulation for daily
c                       return data icx=1
c
c _________________________________________________________
c		Upate History
c rrb 2006/04/18 Add reservoir return flow loss
c		 Initilize loss to 100% (applies when no return
c                  data is provided)
c		     
c _________________________________________________________
c
c               Documentation
c
c               idivsw(nd)      > 0 SW strcuture is on
c               idivsww(nw)     > 0 GW structure is on
c
c               nrtnw(nd)       # of well returns
c               dlytot(i)       sum of returns for return table i
c
c               pctlos(nd)      percent of diversion lost
c               pctlosw(nd)     percent of diversion lost
c               pctlosw2(nd)    percent of diversion lost
c		  iStrTyp         2 for a reservoir,
c                               3 for a diversion, 
c                               6 for a well
c				7 for a plan
c
c _________________________________________________________
c	Dimensions
       include 'common.inc'
c       
       character rec12*12,
     1           cstrtypX*12,   cstridX*5
 
       dimension cstrtypX(10), cstridX(10)     
       
       data cstrtypX/
     1   'InStream    ', 'Reservoir   ', 'Diversion   ', 'NA          ',
     1   'NA          ', 'Well        ', 'Plan        ', 'NA          ', 
     1   'NA          ', 'NA          '/
     
       data cstridX/
     1   '*.NA ',        '*.rrf',        '*.drf',        '*.NA ',
     1   '*.NA ',        '*.wrf',        '*.prf',        '*.NA', 
     1   '*.NA ',        '*.NA '/

c
c _________________________________________________________
c               Step 1; Calculate Diversion Return Losses
c                       For Diversion structures that are on
c                      (idivsw(nd)>0)
c
      small = 0.001
      small2= 0.100
      rec12='N/A'
c     rec12 = '220500'

      iprintd=0
      
      iprob=0
      iprob2=0
      
      istrtyp=3      
      do nd=1,numdiv
        if(idivsw(nd).gt.0) then
c
c
          pctlos(nd)=100.0        
          iusei=nduser(nd)
          iusee=nduser(nd+1)-1
c
c               User loop
          do iuse=iusei,iusee
            nui=nrtn(iuse)
            nue=nrtn(iuse+1)-1

            ctot=0.0
            ptot=0.0
            if(nui.le.nue) then
c
c               Return flow loop
              do n=nui,nue
                irid = irtndl(n)
                ctot=ctot + pcttot(n) * dlytot(irid)/100.0
                ptot=ptot + pcttot(n)
                
                if(cdivid(nd).eq.rec12) then
                  write(nlog,*) n, irid, pcttot(n), dlytot(irid), 
     1             ptot, ctot
                endif  
              end do
              pctlos(nd) = 100.0 - ctot
c
c rrb 00/02/08; Print return and loss data
              if(ichk.eq.7) then
                if(iprintd.eq.0) write(nlog,100) icx
                iprintd=iprintd+1
                write(nlog,110) iprintd,'Diversion Return', cdivid(nd),
     1            divnam1(nd),ctot, pctlos(nd), 
     1            100.0, ' Div Ret'
              endif
c
c               Stop if gt 100
              if(ctot.gt.100.0+small) then
                iprob=iprob+1              
                write(nlog,120) iprob, cdivid(nd),
     1            cstrtypX(istrtyp), cstridX(istrtyp), ptot, ctot 
     
                if(ctot.gt.100.0+small2) iprob2=iprob2+1

c               goto 9999
              endif
            endif
          end do
        endif
      end do
c
c _________________________________________________________
c               Step 2; Calculate Well Return Losses
c                       For Well structures that are on
c                      (idivsws(nd)>0)
c
      istrtyp=6      
      do nd=1,numdivw
        if(idivsww(nd).gt.0) then
c
          pctlosw(nd)=100.0

          nui=nrtnw(nd)
          nue=nrtnw(nd+1)-1
c         ct=0.0

          if(nui.le.nue) then
            ctot=0.0
            ptot=0.0
c
            do n=nui,nue
              irid = irtndlw(n)
              ctot=ctot + pcttotw(n) * dlytot(irid)/100.0
              ptot=ptot + pcttotw(n)
            end do
            pctlosw(nd) = 100.0 - ctot            
c
c rrb 00/02/08; Print return and loss data
            if(ichk.eq.7) then
              if(iprintd.eq.0) write(nlog,100) icx
              iprintd=iprintd+1
              write(nlog,110) iprintd,'Well Return     ',cdividw(nd),
     1          divnamw1(nd), ctot, pctlosw(nd), 
     1          100.0, ' Wel Ret'
            endif
c
c               Stop if gt 100
            if(ctot.gt.100.0+small) then
              iprob=iprob+1
              write(nlog,120) iprob, cdividw(nd), 
     1            cstrtypX(istrtyp), cstridX(istrtyp), ptot, ctot
     
              if(ctot.gt.100.0+small2) iprob2=iprob2+1              
c             goto 9999
            endif
          endif
        endif
      end do
c
c _________________________________________________________
c               Step 3; Calculate Well Salvage
c
      istrtyp=6    
      do nd=1,numdivw
        if(idivsww(nd).gt.0) then
c
          pctlosw2(nd)=100.0

          nui=nrtnw2(nd)
          nue=nrtnw2(nd+1)-1
c         ct=0.0
          if(nui.le.nue) then
            ctot=0.0
            ptot=0.0
c
            do n=nui,nue
              irid = irtndlw2(n)
              ctot=ctot + pcttotw2(n) * dlytot(irid)/100.0
              ptot=ptot + pcttotw2(n)
            end do
            pctlosw2(nd) = 100.0 - ctot 
c
c rrb 00/02/08; Print depletion and salvage data
            if(ichk.eq.7) then
              if(iprintd.eq.0) write(nlog,100)
              iprintd=iprintd+1
              write(nlog,110) iprintd,'Well Depletion  ',cdividw(nd),
     1          divnamw1(nd),
     1          ctot, pctlosw2(nd), 100.0, ' Wel Dep'
            endif
c
c               Stop if gt 100
            if(ctot.gt.100.0+small) then
              iprob=iprob+1            
              write(nlog,120) iprob, cdividw(nd),
     1            cstrtypX(istrtyp), cstridX(istrtyp), ptot, ctot
              
              if(ctot.gt.100.0+small2) iprob2=iprob2+1
              
c             goto 9999
            endif
          endif
        endif
      end do
c
c _________________________________________________________
c               Step 4; Calculate Reservoir Return Losses
c
      istrtyp=2      
      do nr=1,numres      
        if(iressw(nr).gt.0) then
          pctlosRP(nr)=100.0
c
          nui=nrtnRP(nr)
          nue=nrtnRP(nr+1)-1
c         ct=0.0

          if(nui.le.nue) then
            ctot=0.0
            ptot=0.0
c
c           write(nlog,*) ' Closs; nr, nui, nue'
c           write(nlog,*) ' Closs;', nr, nui, nue
            
            do n=nui,nue
              irid = irtndlRP(n)
c             write(nlog,*) 'Closs; n, irid, pcttotrp(n), dlytot(irid)'
c             write(nlog,*) 'Closs;', n, irid, pcttotrp(n), dlytot(irid)
              ctot=ctot + pcttotRP(n) * dlytot(irid)/100.0
              ptot=ptot + pcttotRP(n)
            end do
            pctlosRP(nr) = 100.0 - ctot            
c
c rrb 00/02/08; Print return and loss data
            if(ichk.eq.7) then
              if(iprintd.eq.0) write(nlog,100) icx
              iprintd=iprintd+1
              write(nlog,110) iprintd,'Reservoir Return',cresid(nr),
     1          resnam1(nr), ctot, pctlosRP(nr), 
     1          100.0, ' Res Ret'
            endif
c
c               Stop if gt 100
            if(ctot.gt.100.0+small) then
              iprob=iprob+1
              write(nlog,120) iprob, cresid(nr), 
     1            cstrtypX(istrtyp), cstridX(istrtyp), ptot, ctot
              
              if(ctot.gt.100.0+small2) iprob2=iprob2+1              
c             goto 9999
            endif
          endif
        endif
      end do
c
c _________________________________________________________
c               Step 5; Calculate Plan Return Losses
c
      istrtyp=7     
      do nr=1,nplan      
        if(Pon(nr).gt.0.1) then
          pctlosPP(nr)=100.0
c
          nui=nrtnPP(nr)
          nue=nrtnPP(nr+1)-1
c         ct=0.0

          if(nui.le.nue) then
            ctot=0.0
            ptot=0.0
c
c           write(nlog,*) ' Closs; nr, nui, nue'
c           write(nlog,*) ' Closs;', nr, nui, nue
            
            do n=nui,nue
              irid = irtndlPP(n)
cx              write(nlog,*) 
cx     1         'Closs; PId, n, irid, pcttotPP(n), dlytot(irid)'
cx              write(nlog,*) 
cx     1          'Closs;', Pid(nr), n, irid, pcttotPP(n), dlytot(irid)
              ctot=ctot + pcttotPP(n) * dlytot(irid)/100.0
              ptot=ptot + pcttotPP(n)
            end do
            pctlosPP(nr) = 100.0 - ctot            
c
c rrb 00/02/08; Print return and loss data
            if(ichk.eq.7) then
              if(iprintd.eq.0) write(nlog,100) icx
              iprintd=iprintd+1
              write(nlog,110) iprintd,'Plan Return',cresid(nr),
     1          pname1(nr), ctot, pctlosPP(nr), 
     1          100.0, ' Res Ret'
            endif
c
c               Stop if gt 100
            if(ctot.gt.100.0+small) then
              iprob=iprob+1
              write(nlog,120) iprob, Pid(nr), 
     1            cstrtypX(istrtyp), cstridX(istrtyp),ptot, ctot

              if(ctot.gt.100.0+small2) iprob2=iprob2+1              
c             goto 9999
            endif
          endif
        endif
      end do
      
c
c _________________________________________________________
c
c		Step 5; Stop if we have a problem
      if(iprob2.gt.0) goto 9999      
      if(iprob.gt.0) write(6,9997)
      if(iprob.gt.0) write(nlog,9998) iprob, small2
c
c _________________________________________________________
c
c               Step 6; Return
      return
c
c _________________________________________________________
c               Error Processing

 9997 format(/,'  Closs; Warning in Closs, see log file (*.log)')
 9998 format(/,
     1 '  Closs; Warning in Closs there were', i5,' warnings',/
     1 '         Proceeding on since none were > ',f8.3)
      
 9999 write(6,*)  '  Stopped in Closs, see log file (*.log)'
      write(nlog,*) '  Stopped in Closs after ', iprob, ' warnings'
      write(6,*) 'Stop 1'
      call flush (6)
      call exit(1)

      stop 

c
c
c _________________________________________________________
c               Formats
c
 100  FORMAT(/,72('_'),//
     1  '  Closs; Calling type = ', i5, /,
     1  '         Where 0 = Mdainp (monthly) and ',
     1                 '1 = Dayset (daily)',//
     1  '         Diversion Return, Well Return, ', 
     1          ' Well Depletion or Res Return Summary (%)',/
     1  '         Note Total return or salvage for a station is',/ 
     1  '           the (total return % from the station file)',/
     1  '           * (total return for a table)'//
     1  '                          ',
     1  '                                    ', 
     1  '      Return        Loss                     ',/
     1  '      # Type             ID          ',
     1  ' Name                     ', 
     1  '     or  Dep     or Salv       Total     Type',/
     1  '  _____ ________________ ____________',
     1  ' ________________________ ', 
     1  ' ___________ ___________ ___________ ________')
 110  format(2x, i5, 1x, a16, 1x, a12, 1x, a24, 1x,3f12.4, 1x, a8)
c110  format(2x, i5, 1x, a16, 1x, a12, 1x, a24, 1x,3f12.4, 1x, a8)
 120  FORMAT(/
     1 '  Closs; Problem # ', i5, ' station ',a12,/
     1   10x  'of the ',a12,' station return or depletion file (',
     1         a5,')',/
     1   10x, 'Total returns or depletion > 100%',/
     1   10x, 'Total of % return from the station file      = ', f10.4,/
     1   10x, 'Total of (% return/dep) * (sum of delay tbl) = ', f10.4) 
c
c
c _________________________________________________________
c               End

      end

