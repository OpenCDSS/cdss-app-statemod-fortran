c _________________________________________________________
c	Dimensions
cC     Last change:  C    19 May 97   11:56 pm
c
c                                        
c *******************************************************
c      
      subroutine demcons(ioptiox) 
c
c
c _________________________________________________________
c	Program Description
c

c     Demcons It calculates the demand constrained by
c                 water right and capacity                  
c
c rrb 00/02/23; Warn user if demand is constrained by water
c               right or capacity
c !!!! Subroutine with ioptiox used requires local variables
c               demconi and demcond be remembered
c
c _________________________________________________________
c
c       Documentation
c     
c              ioptiox = 0 calculate constant water right and 
c                          diversion limit data; 
c                          called by execut after riginp (once)
c              ioptiox = 1 include time varying demand
c                          limit; 
c                          called by execut after mdainp (every year)
c                                                      
c              demconx; partially constrained demand
c              demcon(iss,im); constrained demand 
c              divcap;  diversion capacity
c              diver;   demand by user
c
c _________________________________________________________
c
c	Dimensions
      include 'common.inc'
      character rec32*32
c
c _________________________________________________________
c
c               Step 1: Initilize

c
c rrb 2009/05/21; correction 
cx    istart=0     
cx    if(iyr.eq.iystr .and. mon.eq.1) istart = 1
cx    if(istart.eq.1) iprint=0
      if(ioptiox.eq.0) iprint=0
      iprint=1
c      
      small=0.1

c
c =====================================================
c               Step 2; Calculate sum of rights 
      if(ioptiox.eq.0) then
c
c -----------------------------------------------------
c               Step 2a; Initilize
        do 100 k = 1, numdvr
          nd = idivco(1,k)
          if(nd.gt.0) demcond(nd) = 0.0
  100   continue          
c
c ---------------------------------------------------------
c               Step 2b; Initilize Instream flow
        do 110 k = 1, numfrr
          nf = iifrco(k)
          if(nf.gt.0) demconi(nf) = 0.0
  110   continue
c
c _________________________________________________________       
c               Step 3; Sum direct flow rights
        do 120 k = 1, numdvr          
c
c rrb 8/29/95; do not add if right is turned off
          if(idvrsw(k).eq.0) goto 120
          nd = idivco(1,k)
          demcond(nd) = demcond(nd) + dcrdiv(k)
c
c rrb 00/02/23; Limit by capacity later
c         demcond(nd) = amin1(demcond(nd), divcap(nd))
  120   continue          
c
c _________________________________________________________
c               Step 4; Sum instream flow rights
        do 130 k = 1, numfrr
c
c rrb 8/29/95; do not add if right is turned off
          if(iifrsw(k).eq.0) goto 130
          nf = iifrco(k)
          demconi(nf) = demconi(nf) + dcrifr(k)
c       write(99,*) '  Demcons;    k   nf  dcrifr demconi'
c       write(99,'(2i10, 2f8.0)')  k, nf, dcrifr(k),demconi(nf)
  130   continue
        goto 500
      endif
c                                                            
c =========================================================
c              Every year, Limit diversion demand by their request
c
c              Initilize             
c     write(99,400)
                        
      do 140 iss = 1,numsta
        do 140 im=1,12
          demcon(im,iss) = 0.0
          dum(im,iss) = 0.0
  140 continue

c
c              Calculate the total demand
      do 170 nd=1,numdiv
        if(idivsw(nd).eq.0) go to 170
c
        nui=nduser(nd)
        nue=nduser(nd+1)-1
c
        if(nui.gt.nue) go to 170
        iss =idvsta(nd)
c
        do 160 nu=nui,nue
c         if(irturn(nu).gt.3) go to 160

c
          ipm=0
          do 150 i=1,12
            if(diver(i,nu).lt.0.00001) go to 150
            dum(i,iss)=dum(i,iss)+diver(i,nu)    
c
c rrb 00/02/23; Print warning if demand .gt. water rights or capacity
            if(dum(i,iss)-small.gt.demcond(nd) .or.
     1         dum(i,iss)-small.gt.divcap(nd)) then

cx              iprint=iprint+1
cx              if(iprint.eq.1) then
cx                rec32='Demand Vs Div Rights'
cx                write(nlog,1281) iyr, rec32
cx                write(nchk,230)
cx              endif  
              
              ipm=ipm+1
              fac=mthday(i)*factor
c
c 			Use ipm to print just once per year		
              if(ipm.eq.1) then
                iprint=iprint+1
                if(iprint.eq.1) then
                  rec32='Demand Vs Div Rights'
                  write(nlog,1281) iyr, rec32
                  write(nchk,230)
                endif  
              
              
                write(nchk,240) iprint, iyrmo(i), xmonam(i),
     1          cdivid(nd),divnam1(nd),
     1          dum(i,iss)*fac, demcond(nd)*fac, divcap(nd)*fac,' Div'
              endif
            endif

            if(icondem.eq.0) then
              demcon(i,iss)=demcon(i,iss)+
     1                      amin1(divert(i,nu),demcond(nd),divcap(nd))
            else
              demcon(i,iss)=demcon(i,iss)+divert(i,nu)           
            endif
  150     continue 
c  
c rrb 00/06/22; Test
c       write(nlog,'(i5, 12f8.0)') nu, 
c    1    (divert(i,nu) * dfacto * mthday(i), i=1,12)
  
  160   continue                           
  170 continue
c
c                                                      
c              Limit instream diversion demand by their request
c -----------------------------------------------------
c
c------  initialize STATION array
c
      do 180 is=1,numsta+1
      do 180 i=1,12
      dum(i,is)=0.
  180 continue
c
c------ compute instream demand at each node
c
      do 200 nf=1,numifr
        iss =ifrsta(nf)
c
        ipm=0
        do 190 i=1,12
          if(flowr(i,nf).lt.0.00001) go to 190
          dum(i,iss)=dum(i,iss)+flowr(i,nf)
c
c rrb 00/02/23; Print warning if demand .gt. water rights or capacity
            if(dum(i,iss).gt.demconi(nf)) then  

              fac=mthday(i)*factor
              iprint=iprint+1
              ipm=ipm+1
              if(iprint.eq.1) write(nchk,230)
c
              if(ipm.eq.1) then
                write(nchk,240) iprint, iyrmo(i), xmonam(i),
     1            cifrid(nf),xfrnam1(nf),
     1            dum(i,iss)*fac, demconi(nf)*fac, -1.0, ' Isf'
              endif
            endif

c               Do not constrain demand as a 
c               Test for comparison to Modsim or historic data
          if(icondem.eq.0) then
            demcon(i,iss)=demcon(i,iss)+amin1(dum(i,iss),demconi(nf))
          else
            demcon(i,iss)=demcon(i,iss)+dum(i,iss)
          endif
  190   continue

c       do 260 i=1,12
c         f=factor*mthday(i)
c         write(99,'(4i5, 20f8.0)') iyr, i, iss, nf, 
c    1           demconi(nf)*f, dum(i,iss)*f, demcon(i,iss)*f
c 260   continue
c     write(99,*) ' '
c
  200 continue
c
c              Preliminary Check on results
  210 format( '  Demcons; ',/
     1        '                      direct  direct',/  
     1        '  iyr  mon  iss   nd  limit1  demand  limit2',/
     1        ' ---- ---- ---- ----', 3(' -------'))
  220 format( '  Demcons; ',/
     1        '                      instrm  instrm    both',/  
     1        '  iyr  mon  iss   nd  limit1  demand  limit2',/
     1        ' ---- ---- ---- ----', 3(' -------'))
  230 format(/ 
     1  72('_'),//   
     1     '  Demcons; Warning the following structure has a',
     1     ' demand that is limited by water rights',/ 
     1         '           or capacity. This limit may be OK if', 
     1     ' the structure is controlled by an operating right'/
     1         '           Note; only 1 value is printed per year',/
     1         '           All units are af',//
     1     '    # Year  Mon  ID           Name                   ',
     1     '    Demand   Rights Capacity Type',/
     1     ' ___eb___exxb__exb__________exb_____________________e',
     1     'xb_______exb______exb______exb__e')
  240 format(i5, i5, 2x, a4,1x, a12, 1x,a24, 3(1x,f8.0),1x,a4)            
     
 1281  FORMAT(/,72('_'),/
     1  '  DemCons; Warning See *.chk for details in year',i5,/
     1  '           Regarding: ', a32,/
     1  '           Note only first occurance (year) is printed')


  500 return
c
  999 write(6,*) '  Stopped in Demcons, see the log file (*.log)'
      write(99,*) '  Stopped in Demcons'
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop 
      END


