c
c _________________________________________________________
c     SetWeld; It Sets demand data for a diversion


        subroutine SetGw(isub, small, fac, corid1)
c
c _________________________________________________________
c     SetGW; It Sets to GW when a well depletion drives available
c            flow (Avail) negative
        
c _____________________________________________________________
c	Dimensions
c
      include 'common.inc'        
      character corid1*12
c _____________________________________________________________
c
      iout=1
c rrb 00/05/03; Check entire array, not just downstream         
      do nx=1,numsta
        iss=nx
        ndns1=ndnnod(iss)
c
c ---------------------------------------------------------
c              a Find negative
        if(avail(iss).lt.(-1.*small)) then
c
c ---------------------------------------------------------
c              b Calculate gw2riv
          gx = avail(iss)
          gw2riv(iss)=gw2riv(iss) - gx
c
c ---------------------------------------------------------
c              c Route gw2riv downstream
          CALL TAKOUT(maxsta, AVAIL, RIVER, AVINP, QTRIBU, idncod,
     1                gx,     ndns1, iss)            
        endif
      end do
c
c ---------------------------------------------------------
c              d Detailed Output      
        if(iout.eq.1) then
          write(nlog,*) 
     1      ' SetGW; Avail after GW adjustments gx = ',gx*fac
          write(nlog,'(20f8.2)') (avail(i)*fac, i=1,numsta)
        endif  
c
c ---------------------------------------------------------
c              d Double check the entire Avail Array
c		 Note: 
c		 istop =  0 DO NOT STOP if a negative is found
c		          1 DO STOP if a negative is found
        istop=0      
        call chekav2(
     1   icall, maxsta, numsta, istop, fac, AVAIL, IMCD, AvMin)
        AvailC2=avail(imcd)   
c
c_____________________________________________________________
c		           Step 3; Print warning if negative available flow
      IF(AVAILC2 .le.(-1.*small)) then
        write(nlog,*) ' SetGW; problem with operating rule ',
     1    ' isub =', isub
cx        WRITE(nlog,*) IYRmo(mon),xmonam(MON),IW,NWRORD(1,IW),L2,
cx     1                IUSE,DIVREQx*fac,
cx     1                ISCD,ISCD,IMCD,DIVACT*fac, avail(imcd)*fac
        write(nlog,320) (avail(iss)*fac,iss=1,numsta)
        write(nlog,330) (river(iss)*fac,iss=1,numsta)
        
        goto 9999           
      endif
c
c_____________________________________________________________
c              Formats
c        
        
  310 FORMAT(/, '  DivAlt Print 5',I10,6x,a4,4i10,
     1             F10.2,3I10,F10.2, f20.10)
      
  320 format(/, '  SetGW: avail  ',/,(10f10.2))
  330 format(/, '  SetGW: river  ',/,(10f10.2))        
c
c_____________________________________________________________
c              Return
      return
c
c_____________________________________________________________
      
 9999 write(6,1050) 
      write(nlog,1051) 
      
 1050 format('    Stopped in SetGW',/,
     1       '    See the *.log file')
 1051 format('    Stopped in SetGW')
      write (6,*) 'Stop 1'
      call flush(6)
      call exit(1)


      stop 
      END      
