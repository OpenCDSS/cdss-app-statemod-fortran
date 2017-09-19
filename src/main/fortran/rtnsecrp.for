c
c *********************************************************
c
      SUBROUTINE RtnSecRP(iplan, nr, rettot, pctlosR1, rlossR1)
c
c
c
c _________________________________________________________
c	Program Description
c
c       RtnSecRP; It calculates return flows assocaited with a
c		    Reservoir or Canal Seepage
c
c		Called by DivcarL, DirectBy, & DirectEx 
c
c _________________________________________________________
c       Update History
c rrb 2006/04/11; Copy RtnsecP (plans) edit as follows:
c		Add returns to Psuply (RtnsecP added returns to Pdem)
c		Route return flows to the river via Call Takout
c
c
c
c _________________________________________________________
c        Documentation
c		iplan	= plan pointer
c		rettot  = total seepage this time step
c
c               nr     =      Reservoir or Canal ID pointer
c               idcd   =      river station ID where the diversion
c                             is located.  Note set to 0
c                             for baseflow operation
c               nd     =      diversion ID  
c
c               const       = return flow to a given location
c
c               dlyrat(im,idly) =  % return in month im for table idly
c               dlyratd(id,idly)=  % return in day id for table idly
c               diveff( )   = diversion efficiency  
c
c
c               iout        = 0 no detailed output, 1 yes detailed output
c               ichkwr      = water right ID for detailed printout
c
c               imd    =      days this month from execut.for
c               idy    =      day of month
c               imo         = from execut via common block
c                             circular monthly counter
c               idy         = from execut vial common block
c                             daily counter
c               ido         = from execut via common block
c                             circular daily counter
c
c               interv = +n = number of returns for all patterns
c                      = -n = variable number of returns per pattern
c
c               irnstaRP(irn) = ircd = return location node
c               irnord      = set in datinp.  refers to river node??
c
c               irtndlRP(irn) = idly = return table
c
c               ireop       = reoperation code if returns are not
c                             downstream 0=no, 1=yes
c
c               mon         = from execut via common block
c                             monthly counter
c               ndlyRP(n)     = # of returns for pattern n
c               ndlymx      = max # of returns for any pattern
c                             from mdainp.for for a monthly model
c                             from dayest.for for a daily model
c
c               nrtnRP(nr)  =  irni = beginning # of return locations
c               nrtnRP(nr+1)=  irne = ending # of return locations
c
c               pcttotRP(irn) = percent return to a given location
c		            pctlosr1      = percent loss via closs
c               ret           = return flow to a location in a month
c                               ret = const * dlyrat(imo,idly)
c
c               psup(kk,iplan) =future returns in month kk for 
c                               plan iplan
c                               note kk is a circular pointer that
c                               is a function of the  maximum
c                               return interval e.g if max is 5, then
c                               at time 1 kk=1-5 at time 2 kk=2-4,1, 
c                               etc.
c               psupD(kk,iplan)=same as above but for daily
c
c		            psuply(np)     =running suppoy for this month. It 
c		  		                      increases or decreases based on opr 
c                               rules
c		            psuplyT(np)    =total demand this month (may increase 
c                               but will not decrease based on
c                               operating rules
c
c _________________________________________________________
c	Dimensions

      include 'common.inc'
      character pid1*12
c
c _________________________________________________________
c
c               Step 1 - Initilize
c
c
c		iout =  1 details
c                       2 summary
c                       3 print future return data
c
c		ioutR= reservoir to print
c		ioutP= plan to print
c     iout = 1
      iout=0
c      
c     ioutR=18      
c     ioutP=21      
      ioutR=10
      ioutP=0
c
c rrb 2009/06/09; Correction      
cx      if(iday.eq.0) then
cx        fac=factor*mthday(mon) 
cx      else  
cx        fac=1.0
cx      endif
      
      
      if(iday.eq.0) then
        fac=mthday(mon)*factor
        fday=1.0        
      else
        fac=factor
        fday=mthday(mon)
      endif
      
      
      if(iout.ge.1) then
        write(nlog,*) ' '
        write(nlog,*) '______________________________________________'
        write(nlog,*) ' RtnSecRP;    nr iplan rettot'
        write(nlog,'(a12,2i5,f8.0)') '  RtnSecRP; ',
     1     nr, iplan, rettot*fac
      endif
c
      ichkwr = 1
      ireop=0
      iprintR=0
      iprintR2=0

      rett=0.0
      psup1=0.0
      psuply1=-1/fac
      psuply2=-1/fac
      PsuplyT1=-1/fac
      Psuply2=-1/fac
      
      small=0.001
      
      pid1='NA'      
      if(iplan.gt.0) pid1=pid(iplan)
      
      
      if(iout.eq.1 .and. nr.eq.ioutR) then
        write(nlog,*) ' '
        write(nlog,*) ' RtnSecRP;   iyr  mon  idy   nr iplan'
        write(nlog,'(a12, 20i5)') '  RtnSecRP; ', 
     1    iyr, mon, idy, nr, iplan
      endif      
      
c               Calculate Loss
      rlossR1=rlossR1+rettot*pctlosR1/100.0     

c
c _________________________________________________________
c
c               Step 2 - Set return flow counters
c
c ---------------------------------------------------------
c		a. Seepage is not tied to a reservoir return flow plan
      if(iplan.eq.0) then
        IRNI=nrtnRP(nr)
        IRNE=nrtnRP(nr+1)-1
      endif
c      
c ---------------------------------------------------------
c		b. Seepage is tied to a reservoir return flow plan
c		Allow plan return flow data to be different than the 
c		reservoir itself
c      
      if(iplan.gt.0) then
        pdrive(iplan)=pdrive(iplan)+rettot
        if(ipRF(iplan).eq.8) then
          IRNI=nrtnRP(nr)
          IRNE=nrtnRP(nr+1)-1
        endif  
        
        if(ipRF(iplan).eq.999) then
          irni=iprf(iplan)
          irne=iprf(iplan)
c          write(nlog,*) ' RtnSecRP; 999, iplan, irni, inre',
c     1     iplan,irni,irne
          IF(IRNI.GT.IRNE) goto 500
        endif  
      endif    
c
c ---------------------------------------------------------
c		Detailed Output
      
      if(iout.eq.1 .and. nr.eq.ioutR) then
        if(iplan.gt.0) then 
          write(nlog,100) pid1, iplan,
     1      iprf(iplan), irni, irne,  rettot*fac, pctlosR1
        else
          write(nlog,102) 
     1      nr, irni, irne,  rettot*fac, pctlosR1
        endif
      endif
     
     
c
c ---------------------------------------------------------
c rrb 2007/08/28; Performance     
      if(rettot.lt.small) goto 500
c
c ---------------------------------------------------------
c		c. If no return flow send seepage to loss and exit
      if(irne.lt.irni) then
        rlossR1= rettot
        if(iout.eq.1 .and. nr.eq.ioutR) 
     1   write(nlog,*) ' RtnSecRP; Reservoir Seepage Loss',
     1    ' for reservoir nr of rettot',  nr, rlossR1*fac, irni, irne
        goto 500
      endif  
c
c _________________________________________________________
c
c               Step 2 - Return flow location loop
c      
      if(iout.eq.1 .and. nr.eq.ioutR) then
        write(nlog,*) ' RtnSecRP; Reservoir Seepage Loss'
        write(nlog,*) ' RtnSecRP;    nr irn1 irne  rlossR1'
        write(nlog,'(a12,3i5,f10.2)') '  RtnSecRP; ', 
     1    nr, irni, irne, rlossR1*fac 
      endif         
c      
      DO 150 IRN=IRNI,IRNE
        IRCD=irnstaRP(irn)
        NDNS=NDNNOD(IRCD)
c
c rrb 2007/08/28; Correction        
cx      IORD=IRNORD(IRCD)
        IORD=irnordRP(IRCD)
        
        if(iout.eq.1 .and. nr.eq.ioutR) then
          write(nlog,*) ' RtnsecRP;    nr  irn ircd iord iplan' 
          write(nlog,'(a12,20i5)')
     1       '  RtnsecRP; ', nr, irn, ircd, iord, iplan
        endif  
        
c _________________________________________________________
c
c               Step 3 - Calculate return to location irn (const)
c                        and delay table (idly). Note even though
c			                   the location is not needed; this calculation 
c                        is required to get the proper return table ID
c       CONST=rettot*pcttotRP(IRN)/10000.
c        
        if(iout.eq.1 .and. nr.eq.ioutR) then
          write(nlog,*) ' RntsecRP; ',
     1     'iplan  irn irtnd pcttotRP    retTot'
          write(nlog,'(a12,3i5,20f10.0)') '  RntsecRP; ',
     1      iplan, irn, irtndlRP(irn), pcttotRP(irn), rettot*fac 
        endif
c
c		Allow plan return flow data
        if(iplan.eq.0) then
          CONST=rettot*pcttotRP(IRN)/10000.
          idly=irtndlRP(irn)
        endif  
        
        if(iplan.gt.0) then
c
c rrb 2008/09/29; Update        
          if(ipRF(iplan).eq.8) then            
            CONST=rettot*pcttotRP(IRN)/10000.
            idly=irtndlRP(irn)
          endif  
          
          if(ipRF(iplan).eq.999) then
            CONST=rettot*pcttot(IRN)/10000.          
            idly=iprf(iplan)
          endif  
        endif  
c
c _________________________________________________________
c
c               Step 4 - Calculate return flows 
c                        in month 1 or day 1 (ret) by delay table dlyrat
c			 Adjust demand (pdem) for this month
c
        if(iday.eq.0) then
          RET=CONST*DLYRAT(1,IDLY)
          dlyrat1=dlyrat(1,idly)
        else
          ret =const*dlyratd(1,idly)
          dlyrat1=dlyratd(1,idly)
        endif
        
        if(iout.eq.1 .and. nr.eq.ioutR) then
          write(nlog,*) ' RntsecRP;  idly     const    dlyrat       ret' 
          write(nlog,'(a12,i5,20f10.2)') '  RtnSecRP; ',
     1      idly, const*fac, dlyrat(1,idly), ret*fac
        endif
c
c _________________________________________________________
c
c               Step 5 - Adjust avail and river at return location
c                        in month 1.  Note do not adjust AVINP, flow
c                        into (upstream) of river node.
         retAdd=-1.*ret
c        write(nlog,*) ' RtnSecRP; Calling Takout'
         call Takout(maxsta, avail, river, avinp, qtribU, idncod,
     1               retAdd, ndns, IRCD)
c        write(nlog,*) ' RtnSecRP; Back from Takout'
c
c _________________________________________________________
c
c		Step 6; Add to plan supply
c		Note pdem is current demand (may go up or down)
c		     pdemT() is total demand this day or month 
c		             (can only go up)
c
        if(iplan.gt.0) then
          psuply1=psuply(iplan)
          psuplyT1=psuplyT(iplan)
        
          psuply(iplan) =psuply(iplan) +ret
          psuplyT(iplan)=psuplyT(iplan)+ret
        
          psuply2=psuply(iplan)
          psuplyT2=psuplyT(iplan)
        endif
        
        if(iout.ge.1 .and. ioutR.eq.nr) then
          if(iprintr.eq.0) then
            write(nlog,310)
            iprintR=iprintR+1
          endif  
          
          write(nlog,312)  '  RtnSecRP;  ', 
     1      iprintR, pid1, iyr, mon, imo, idy, ido, 
     1      iord, idly, iplan, 1,  
     1      rettot*fac, ret*fac, dlyrat1, 
     1      psuply1*fac, psuply2*fac, PsuplyT1*fac, Psuply2*fac
     
        endif
c
c _________________________________________________________
c
c               Step 8 - Calculate future returns (pobl1)
  130   IM=0
        IEND=IMO+ndly(idly)-1
c
c _________________________________________________________
c
c 		Step 8a - Monthly returns
        if(iday.eq.0) then
          DO K=IMO,IEND
            IM=IM+1
c
c               Adjust monthly model for # of days in a month
            imx = mon+im-1
            ixe=imx/12+1

            do ix=1,ixe
              if(imx.gt.12) imx=imx-12
            end do

            c  = float(mthday(mon))/float(mthday(imx))
            ret=const*dlyrat(im,idly)*c
            
            rett=rett+ret
            KK=K
c
c               Check for wrap around
            IF(K.GT.ndlymx) then
              KK=K-ndlymx
            endif
c
c		Add to future return and Plan Supply arrays
            RETUR(KK,IORD)=RETUR(KK,IORD)+RET
c            
            if(iplan.gt.0) then
              psup(kk,iplan)=psup(kk,iplan) + ret
              psup1=psup(kk,iplan)
            endif  
            
c
            if(iout.ge.2 .and. nr.eq.ioutR) then
              if(iprintr2.eq.0) then
                write(nlog,300)
                iprintR2=iprintR2+1
              endif  
              
              IS=ISTRTN(iord)           
                 
              write(nlog,302)  '  RtnSecRP  ', 
     1          iprintR2, Pid1, iyr, mon, imo, idy, ido,
     1          iord, idly, iplan, kk, 
     1          psup1*fac, rettot*fac, rlossR1*fac, rett*fac,cstaid(is)
cr            iprintr=1
            endif
c
c               End Loop for number of return flow time intervals
          end do
c
c               Print results for detailed checking
          if(iout.ge.3 .and. iplan.gt.0) then
              write(nlog,318)
              write(nlog,320) 1, imo-1,
     1                        (psup(k,iplan)*fac, k=1,imo-1)
              write(nlog,320) imo, ndlymx, 
     1                       (psup(k,iplan)*fac, k=imo,ndlymx)
          endif
        end if

c _________________________________________________________
c
c rrb 98/03/17; 
c		Step 8b; Daily return capability
c
        if(iday.eq.1) then
          id=0
          iend=ido+ndly(idly)-1

          do k=ido,iend 
            id=id+1

            ret=const*dlyratd(id,idly)
c
c               Check for wrap around
            kk=k
            if(k.gt.ndlymx) then
              kk=k-ndlymx
            endif
c
c		Add to future return and Plan Supply arrays            
            returd(kk,iord)=returd(kk,iord)+ret
c
c               Print results for detailed checking of returns
          if(iout.ge.3 .and. ioutR.eq.nr) then
            write(nlog,*) 
     1       '  RtnSecRP; iord  ido iend idly idly ndlymx'
            write(nlog,'(a12, 7i5, 10f8.0)')
     1        '  RtnSecRP; ',iord, ido,iend,idly, ndlymx, k, kk, 
     1        returd(kk,iord)*fac
          endif               
            
c            
            if(iplan.gt.0) then
              psupD(kk,iplan)=psupD(kk,iplan)+ret         
            endif  
c
c               End Loop for number of return flow time intervals (k)
          end do
c
c               Print results for detailed checking of plans
          if(iout.ge.3 .and. iplan.gt.0) then
            write(nlog,*) '  RtnSecRP; ido, ndlymx', ido, ndlymx
            write(nlog,'(10f8.2)') (psupD(k,iplan)*fac, k=1,ndlymx)
          endif               
c
c               Print results for detailed checking of returns
          if(iout.ge.3 .and. ioutR.eq.nr) then
            write(nlog,*) '  RtnSecRP;  ido ndlymx'
            write(nlog,'(a12, 20i5)') '  RtnSecRP; ',ido,ndlymx
            write(nlog,'(10f8.2)') (returd(k,iord)*fac, k=1,ndlymx)
          endif  
                    
        endif
c
c               End Loop for number of return flow locations
  150 CONTINUE
c
c _________________________________________________________
c
c               Step 11; Return
 500  continue
c
c
c _________________________________________________________
c		Detailed Output
      if(iout.eq.1 .and. nr.eq.ioutR .and. ioutP.gt.0) then
        write(nlog,*)  ' RtnSecRP; before Return  ', nr, ioutP,  
     1    psuply(ioutP)*fac, psuplyT(ioutP)*fac       
      endif
 


      RETURN
c
c
c _________________________________________________________
c
c               Formats

 100    format(/, '  RtnsecRP; ',
     1  ' Pid1        iplan iprf irni irne    rettot  pctLosR1',/
     1  '  RtnsecRP; ',a12, 1x, 4i5, 20f10.2)    
     
 102    format(/, '  RtnsecRP; ',
     1  '   nr irni irne    rettot  pctLosR1',/
     1  '  RtnsecRP; ',3i5, 20f10.2)    
     
 300  format(/'  RtnSecRP; Recharge Plan Supply Summary',/
     1 '  RtnSecRP      # Plan ID     ',
     1 '  iyr  mon  imo  idy  ido iord idly ipln   kk',
     1 '    psuply    rettot   rlossR1      rett cstaid',/
     1 ' ___________ ____ ____________',
     1 ' ____ ____ ____ ____ ____ ____ ____ ____ ____',
     1 ' _________ _________ _________ _________ ____________')
 302  format(a12, i5, 1x, a12, 9i5, 4f10.2, 1x, a12)
 
 310  format(/'  RtnSecRP; Recharge Plan Supply Summary',/
     1 '  RtnSecRP      # Plan ID     ',
     1 '  iyr  mon  imo  idy  ido iord idly ipln   kk',
     1 '    RetTot       Ret   DdyRat1   Psuply1   Psuply2',
     1 '  PsuplyT1  PsuplyT2',/
     1 ' ___________ ____ ____________',
     1 ' ____ ____ ____ ____ ____ ____ ____ ____ ____',
     1 ' _________ _________ _________ _________ _________',
     1 ' _________ _________')
 312  format(a12, i5, 1x, a12, 9i5, 20f10.2)
 
 318  format(/, '  RtnSecRP; Plan Supply array')

 320  format('  From ', i5, ' To ', i5, 10f8.2)             
c
c
c _________________________________________________________
c
c               Error Processing
 200  write(nlog,210) nr
 210  format('  RtnSecRP; Problem when called by SepSec',
     1       ' nr = ', i15)
                                      
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop
      END





