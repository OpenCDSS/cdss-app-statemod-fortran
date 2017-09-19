C
      subroutine soilm(iw,l2,divactx)
c
c
c _________________________________________________________
c	Program Description
c
c       Type 22 Operating Rule Soil Moisture Use
c       Approach
c               Call 1x per time step
c               Loop for every diversion
c               Satisfy remaing demand with available Soil Storage
c               Loop for every well only land
c               Satisfy remaining demand with avalable Soil Storage
c               
c
c rrb 01/07/31; Revised to allow CU as a function of IWR only 
c               not IWR and demand
c
c               
c       Note datip checks if isoil.ne.1 or itsfile.ne.1 or
c               ieffmax.ne.1 
c       Also oprinp checks if isoil=1 but no operating rule provided 
c
c               iw= global water right counter
c               l2 = operation right counter
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
      character cwhy*48
c
c _________________________________________________________
c               Step 1; Initilize
c
c     write(nlog,*) '  Soilm; idy, mon, iyr', idy, mon, iyr
c
c		iout=1 details
c		iout=2 summary 
      iout=0
c     if(ichk.eq.122) iout=1
      if(ichk.eq.122) iout=2

      if(iout.eq.1) then
        write(nlog,*) ' '
        write(nlog,*) ' ___________________________________'
        write(nlog,*) '  Into Soilm'
      endif
      iw=iw
      small=0.001

      divtot=0.0
c
      if(iday.eq.0) then
        fac=mthday(mon)*factor
      else
        fac=factor
      endif  
      
c
c ---------------------------------------------------------
c
c               Step 5; Check
      if(iout.eq.1) then
        write(nlog,*) ' SoilM; l2, mon, imonsw', l2,mon,imonsw(l2,mon)
      endif        
c
c _________________________________________________________
c
c               Step X; Branch if not on this month
c
c rrb 04/22/96; Allow month switch
      if(imonsw(l2,mon).eq.0) then
        iwhy=1
        cwhy='Monthly switch is off'
        goto 500
      endif
c
c ---------------------------------------------------------
c		For a daily model set demand for beginning of season
      if(iday.eq.1 .and. imonsw(l2,mon).gt.0) then
        if (idy.lt.imonsw(l2,mon)) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 500
        endif  
      endif  
c
c ---------------------------------------------------------
c		For a daily model set demand for end of season
      if(iday.eq.1 .and. imonsw(l2,mon).lt.0) then
        if (idy.gt.iabs(imonsw(l2,mon))) then
          iwhy=1
          cwhy='Daily switch Off'
          goto 500
        endif  
      endif  
c
c ---------------------------------------------------------
c
c               Step 5; Check
      if(iout.eq.1) then
        write(nlog,*) ' SoilM; ', numdiv, numdivw
      endif             
      
c
c _________________________________________________________
c               Step 2; Soil Moisure to Div Only & D&W 
      nw=0
      do nd=1,numdiv
        iuse1=nduser(nd)
        iuse2=nduser(nd+1)-1
        do iuse=iuse1, iuse2
          if(iuse.ne.nd) then
            write(nlog,*) '  Soilm; Problem'
            goto9999
          endif
c
c rrb 01/07/31; Revised to allow as a function of IWR only 
c               not IWR and demand 
          soils1=soils(nd)
          dcut1=dcut(nd)
          qdivso1=qdivso(nd)
          
          call soilcu(nlog,  nd, nw,    small,    fac,
     1      divreq(iuse), diwrreq(iuse),
     1      soils(nd),    dcut(nd), qdivso(nd),ichk,
     1      cdivid(nd))
          divtot=divtot+qdivso(nd)
c
c ---------------------------------------------------------
c
c               Step 5; Check
          if(iout.eq.2) then
            write(nlog,100) cdivid(nd), nd, nw, iuse
            write(nlog,110) divreq(iuse)*fac, diwrreq(iuse)*fac, 
     1        soils1*fac, soils(nd)*fac,  dcut1*fac,  dcut(nd)*fac,
     1        qdivso1*fac, qdivso(nd)*fac
          endif             
        end do
      end do
c
c _________________________________________________________
c               Step 3; Soil Moisture to Well Only Lands
      do nw=1,numdivw
        nd=idivcow2(nw)
c
c rrb 01/07/31; Revised to allow as a function of IWR only 
c               not IWR and demand
c       if(nd.eq.0 .and. divreqw(nw).gt.small) then
        if(nd.eq.0) then  
          call soilcu(nlog, nd, nw,     small,     fac,
     1         divreqw(nw), diwrreqw(nw),
     1         soilsw(nw),  dcutw(nw), qdivswo(nw), ichk,
     1         cdividw(nw))
          divtot=divtot+qdivswo(nw)
        endif
      end do
c
c _________________________________________________________
c               Step 4; Total
      divo(l2) = divo(l2) + divtot
      divactx  = divtot

c
c _________________________________________________________
c
c               Formats
c
 100  format(/,
     1 '   SoilM; for ID = ', a12, ' Div# = ', i5, ' Well# = ', i5,
     1 '   User# = ', i5,/,
     1 '   Dem-1   IWR-1 Soils-1  Soils-2   CU-1    CU-2',
     1 ' Qdivso1 Qdivso2'/
     1 ' _______ _______ _______ _______ _______ _______',
     1 ' _______ _______')
 110  format(20f8.0)      
      
c
c _________________________________________________________
c               Step 5; Return
 500  return
c
c _________________________________________________________
c               Print warning
c 
 9999 write(6,*) '  Stopped in Soilm, see the log file (*.log)'
      write(99,*) '  Stopped in Soilm'
      write(6,*) 'Stop 1' 
      call flush(6)
      call exit(1)
      stop 
      end
     
