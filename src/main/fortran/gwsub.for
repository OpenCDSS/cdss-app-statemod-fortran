c
      subroutine gwsub
c
c
c _________________________________________________________
c	Program Description
c
c       Gwsub; It  adjusts river, etc to pay back 
c               water to gw storage
c               and sets Avail to River
c
c               Called by dayset every day
c               Called by bomsec every month
c
c _________________________________________________________
c	Dimensions

      include 'common.inc'
c
c _________________________________________________________       
c               Step 1; Initilize
c               
      ichk1=0
      cx=1.0
      small=0.01

      if(iday.eq.0) then
        fac=factor*mthday(mon)
      else
        fac=factor
      endif


c     if(iday.eq.0) then
c       monx=mon-1
c       if(monx.le.0) monx=12
c       cx=float(mthday(monx))/float(mthday(mon))
c     endif

      if(ichk1.eq.1) write(io99,100) mon  
c
c _________________________________________________________       
c               Step 2; Adjust River for ground water storage use
c               with this months inflows  

      do is=1,numsta
        gwstor(is) = gwstor(is)*cx - gw2riv(is) + gw2gw(is)
        gw2riv(is) = 0.0
        gw2gw(is)  = 0.0

        if(-1.0*gwstor(is).gt.small) then
          ichk1=0
          if(ichk1.eq.1) then
            write(io99,110) is, gw2riv(is)
            write(io99,120) (river(ix),  ix=1,numsta)
            write(io99,130) (gwstor(ix), ix=1,numsta)
            write(io99,140) (gw2riv(ix), ix=1,numsta)
            write(io99,150) (gw2gw(ix),  ix=1,numsta) 
          endif
c
c ---------------------------------------------------------
c               Step 2a; Unlimited pay back
c rrb 00/04/14; 
          gwx=0.0
          if(iwell.eq.1) then
            gwx=-1.0*gwstor(is)*cx
            gw2gw(is)=gwx
            gwstor(is)=0.0
          endif
c
c ---------------------------------------------------------
c               Step 2b; Limited pay back
c rrb 00/04/14; Include maximum stream recharge limit 
          if(iwell.ge.2) then
            gwx=amin1(-1.0*gwstor(is)*cx, gwmaxr(is))
            gw2gw(is)=gwx
            gwstor(is)=gwstor(is)*cx + gwx
          endif
c
c ---------------------------------------------------------
c               Step 2c; Route downstream
          iss=is
          QTRIBU(ISS)=QTRIBU(ISS)-gwx
          RIVER (ISS)=RIVER (ISS)-gwx
          ISS=IDNCOD(ISS)

          IF(ISS.gt.0) then
            NDNN=NDNNOD(ISS)
            DO N=1,NDNN
c
              RIVER (ISS)=RIVER (ISS)-gwx
              AVINP (ISS)=AVINP (ISS)-gwx  
              gwstor(iss)=amin1(gwstor(iss)*cx+gwx, 0.0)
              ISS=IDNCOD(ISS)
            end do
          endif

          ichk1=0
          if(ichk1.eq.1) then
            write(io99,220) (river(ix),  ix=1,numsta)
            write(io99,230) (gwstor(ix), ix=1,numsta)
            write(io99,240) (gw2riv(ix), ix=1,numsta)
            write(io99,250) (gw2gw(ix),  ix=1,numsta) 
          endif
        endif

c
c _________________________________________________________
c               Step 3; Check and warn if negative
c rrb 12/12/95; warn user of negative flows befor allocation

        if(river(is).lt. -0.01) then
          if(ichk1.eq.1) then
            write(io99,410)  iyrmo(mon), xmonam(mon), 
     1      stanam1(is), river(is), river(is)*fac
          endif
          gwx=-1.0*river(is)
c
c rrb 00/04/14; Maximum recharge 
          gw2riv(is)=gwx
          gwstor(is)=gwstor(is)*cx-gwx

          iss=is

          QTRIBU(ISS)=QTRIBU(ISS)+gwx
          RIVER (ISS)=RIVER (ISS)+gwx
          ISS=IDNCOD(ISS)

          IF(ISS.gt.0) then
            NDNN=NDNNOD(ISS)
            DO N=1,NDNN
c
              RIVER (ISS)=RIVER (ISS)+gwx
              AVINP (ISS)=AVINP (ISS)+gwx
              gwstor(iss)=amin1(gwstor(iss)*cx+gwx, 0.0)

              ISS=IDNCOD(ISS)
            end do
          endif

          if(ichk1.eq.1) then
            write(io99,320) (river(ix),  ix=1,numsta)
            write(io99,330) (gwstor(ix), ix=1,numsta)
            write(io99,340) (gw2riv(ix), ix=1,numsta)
            write(io99,350) (gw2gw(ix),  ix=1,numsta) 
          endif
        endif

        AVAIL(IS)=RIVER(IS)
        IF(QTRIBU(IS).LE.small) QTRIBU(IS)=0.
      end do
c
c _________________________________________________________
c               Step 4; Return
      return
c
c _________________________________________________________
c               Formats

 100  format(/,        
     1 '  _________________________________________________',/
     1 '  GWsub, mon =', i5)
 110    format(/
     1  '  ________________________________________________',/
     1  '  GWsub; is, gw2riv(is) = ', i5, f8.0)
 120    format(    
     1  '  Gwsub; River  Original',/, (10f10.2))
 130    format(    
     1  '  Gwsub; Gwstor Original',/, (10f10.2)) 
 140    format(
     1  '  GWsub; Gw2riv Original',/, (10f10.2))
 150    format(
     1  '  Gwsub; Gw2gw  Original',/, (10f10.2))

 220    format(    
     1  '  Gwsub; River  After Payback',/, (10f10.2))
 230    format(    
     1  '  Gwsub; Gwstor After Payback',/, (10f10.2)) 
 240    format(
     1  '  GWsub; Gw2riv After Payback',/, (10f10.2))
 250    format(
     1  '  Gwsub; Gw2gw  After Payback',/, (10f10.2))

 320    format(    
     1  '  Gwsub; River  After Adjustment',/, (10f10.2))
 330    format(    
     1  '  Gwsub; Gwstor After Adjustment',/, (10f10.2)) 
 340    format(
     1  '  GWsub; Gw2riv After Adjustment',/, (10f10.2))
 350    format(
     1  '  Gwsub; Gw2gw  After Adjustment',/, (10f10.2))
  
 410    format(
     1 '  Bomsec; Warning negative flow (af) befor allocation ', /
     1 '   on ', i4, 1x, a3, ' at ', a24, ' It was set to zero',/
     1 '   from', f10.2, ' cfs or ', f10.0, ' acft')

      end
