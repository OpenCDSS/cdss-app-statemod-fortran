c
c *********************************************
c
        subroutine AccouL(maxacc,maxown,nr,ownmon,curown,accr,ia,
     1    ownmax, iown, nrown1,cursa, actaf, resLoss, iresty1,
     1    icx, cresid1)
c
c _________________________________________________________
c
c               Program Description;
c
c       It distributes a reservoir right to individual accounts
c	Same as Accou but includes loss to reservoir (resLoss)
c
c _________________________________________________________
c
c               Update History
c	2006/09/25; Added icx calling subroutine
c       2003/10/27; Add ability to distribute based on ownership ratio
c                   (e.g. iresty1 = 0)
c
c
c _________________________________________________________
c               Documentation
c       maxacc
c       maxown
c       nr              reservoir ID
c       ownmon(i)       ownership for account i
c       curown(i)       current storage for account i
c       accr(i)
c       ownmax(i)       maximum storage for account i
c       iown            first owner associate with this water right
c       nrown1          number of owners associated with this
c                       water right
c       cursta          total available in all ownership accounts
c       actaf           amount stored
c
c        iresty1	type of account distribution
c			0 Ownership Ratio 
c                       1 Available Space 
c                      -1 One Account  
c	ia		account to adjust
c
c       icx    		subroutine call ID
c                             111=divcar,   114=divcar1,  116=DirectFS,
c                             119=divcar2,  124=directEx, 125=directBy,
c			      127=DivresP,  202=resrg1   

c _________________________________________________________
c
c               Dimensions
c
        dimension ownmon(maxown), curown(maxown), ownmax(maxown)
        dimension accr(maxacc,maxown)
        dimension curown1(maxown)
        character cresid1*12, ctype*16
c
c
c _________________________________________________________
c
c               Initilize
        iout=0
        nlog=99
        nr=nr
        ct = 0.0                                                    
        vola = 0.0
        
        if(iresty1.gt.0) ctype='Available Space '
        if(iresty1.eq.0) ctype='Ownership Ratio '
        if(iresty1.lt.0) ctype='One Account     '

        if(iout.eq.1) write(nlog,100) icx, cresid1, 
     1   actaf, ctype
c _________________________________________________________
c
c               Distribute based on available space 
c		to first n accounts
c
cx      if(iresty1.ne.0) then
        if(iresty1.ge.1) then
          do n=1,nrown1
            n1 = iown+n-1
            curown1(n1) = curown(n1)

            f=amax1(0.0,(ownmax(n1)-curown(n1))/cursa)
            ftot=cursa
            c = actaf*f
            ct = ct+c
            ownmon(n1)=ownmon(n1)+c
            curown(n1)=curown(n1)+c
            accr(ia,n1)=accr(ia,n1)+c
c
c rrb 2008/09/26; do not show loss at reservoir, it is with the carrier		         
c           accr(27,n1)=accr(27,n1)+resLoss*f
            vola = vola + amax1(0.0, ownmax(n1)-curown(n1))
          end do
        endif
c
c _________________________________________________________
c
c               Distribute based on ownership ratio
c
        if(iresty1.eq.0) then
c
c               Get total of all owners
          ctot=0.0
          do n=1,nrown1
            n1 = iown+n-1
            ctot=ctot+ ownmax(n1)
          end do

          do n=1,nrown1
            n1 = iown+n-1
            curown1(n1) = curown(n1)

            f=amax1(0.0,ownmax(n1)/ctot)
            ftot=ctot
            c = actaf*f
c
c               Limit storage in this account to amount available
c               in this account
            c = amin1(c, ownmax(n1)-curown(n1))
            ct = ct+c
            ownmon(n1)=ownmon(n1)+c
            curown(n1)=curown(n1)+c
            accr(ia,n1)=accr(ia,n1)+c
c
c rrb 2008/09/26; do not show loss at reservoir, it is with the carrier		                     
c           accr(27,n1)=accr(27,n1)+resLoss*f
            vola = vola + amax1(0.0, ownmax(n1)-curown(n1))
          end do
        endif
c _________________________________________________________
c
c               Distribute to one account
        if(iresty1.le.-1) then
          n1 = iown+1-1
          write(nlog,*)' AccouL; ', n1, curown(n1), accr(27,n1), resloss
          curown1(n1) = curown(n1)

          c = actaf
          ct = ct+c
          ownmon(n1)=ownmon(n1)+c
          curown(n1)=curown(n1)+c
          accr(ia,n1)=accr(ia,n1)+c
c
c rrb 2008/09/26; do not show loss at reservoir, it is with the carrier		                   
c         accr(27,n1)=accr(27,n1)+resLoss
          vola = vola + amax1(0.0, ownmax(n1)-curown(n1))
          write(nlog,*)' AccouL; ', n1, curown(n1), accr(27,n1), resloss
        endif
        
c
c _________________________________________________________
c
c              Distribute unallocated right (if any) 
c              based on ownership space
        if(actaf-ct .gt. 0.1) then
          crem = actaf-ct
          do n=1,nrown1
            n1 = iown+n-1
c
c rrb 04/10/96
            f = amax1(0.0,(ownmax(n1)-curown(n1))/vola)
            c = crem*f
            ct = ct+c
      
            ownmon(n1)=ownmon(n1)+c
            curown(n1)=curown(n1)+c
            accr(ia,n1)=accr(ia,n1)+c
cx          accr(27,n1)=accr(27,n1)+resLoss
          end do
        endif
c
c _________________________________________________________
c
c               Detailed Printout

      if(iout.eq.1) then
c       write(nlog,200) 'Detailed Information',
c    1    nr, nrown1, actaf, ct, crem
        ctot=0.0
        ctot1=0.0
        ctot2=0.0
        ctot3=0.0
        ctot4=0.0
c
        write(nlog,210)
        do n=1,nrown1
          n1 = iown+n-1
          ctot=ctot+ownmax(n1)
          ctot1=ctot1 + curown1(n1)
          ctot2=ctot2 + curown(n1)
          ctot3=ctot3 + curown(n1) - curown1(n1)

          if(actaf.gt.0.0) then
            divp=(curown(n1) - curown1(n1))/actaf*100.0
            ctot4=ctot4+divp
          else
            divp=-1.0
          endif

          write(nlog,220) n, n1, ownmax(n1), curown1(n1), curown(n1),
     1      curown(n1) - curown1(n1), divp
        end do
        write(nlog,230) ctot, ctot1, ctot2, ctot3, ctot4
      endif
c
c _________________________________________________________
c
c               Check Results
      if (actaf-ct.gt.0.1) then
        write(nlog,200) 'Problem',
     1    nr, nrown1, actaf, ct, crem
c
        ctot=0.0
        ctot1=0.0
        ctot2=0.0
        ctot3=0.0
        ctot4=0.0

        write(nlog,210)
        do n=1,nrown1
          ctot=ctot+ownmax(n1)
          ctot1=ctot1 + curown1(n1)
          ctot2=ctot2 + curown(n1)
          ctot3=ctot3 + curown(n1) - curown1(n1)

          if(actaf.gt.0.0) then
            divp=(curown(n1) - curown1(n1))/actaf*100.0
            ctot4=ctot4+divp
          else
            divp=-1.0
          endif

          n1 = iown+n-1
          write(nlog,220) n, n1, ownmax(n1), curown1(n1), curown(n1),
     1      curown(n1) - curown1(n1), divp
        end do
        write(nlog,230) ctot, ctot1, ctot2, ctot3, ctot4

        goto 9999
      endif
c
c _________________________________________________________
c
c               Return
      return
c
c _________________________________________________________
c
c               Formats
 100  format(/
     1 '  AccouL; Distributes a diversion to accounts',/
     1 '          Called by Rouine ID   = ', i8,/
     1 '          Reservoir ID          = ',a12,/
     1 '          Diversion amount (af) = ',f8.0,/
     1 '          Distribution Type     = ',a16)
 
 200  format(/,
     1 'AccouL; ', a20, ' allocating a right to owners ',/
     1 8x,'      nr  nrown1       actaf          ct       crem',/
     1 8x,' _______ _______ ___________ ___________ __________',/
     1 8x, 2i8, 20f12.4)
 210  format(//,
     1 '    #   n1  OwnerAmt   Curown1    Curown Divert-af  Divert-%'/
     1 ' ____ ____ _________ _________ _________ _________ _________')
 220  format(2i5, 20f10.1)
 230  format('Total', 5x, 20f10.1)
c
c
c _________________________________________________________
c
c               Error Processing

 9999 write(6,*)  '  Stopped in AccouL, see the log file (*.log)'
      write(99,*) '  Stopped in AccouL'
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)
      stop 
      end



