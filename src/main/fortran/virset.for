c
c *********************************************************
c
       subroutine virset(ityp)
c
c
c _________________________________________________________
c	Program Description
c
c       Virset; It sets virgen (base) flow data for both
c                 daily and monthly model
c
c
c _________________________________________________________
c	Documentation
c
c		Called by Vircom.f
c
c               ityp = 0 Monthly Initilization
c               ityp = 1 Daily Initilization
c               ityp = 2 Daily Model Sum generics into monthly totals
c               ityp = 3 Daily Model Set monthly totals to generics
c               ityp = 4 Monthly Model Set qhisto(mon,is) = qhistox(is)
c               ityp = 5 Daily Model Set qhisto(mon,is) =qhistod(32,is)
c
c _________________________________________________________
c
c       Update History
c rrb 01/07/30; Added To Soil Moisture, From Soil Moisture and CU
c rrb 02/10/25; Added call seteff to set default efficiency data
c               when running with a variable efficiency
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
c
c
c _________________________________________________________
c		Step 1; Initilize
c
c     write(99,*)    '  Virset; ityp, numstax = ', ityp, numstax
c
c	  	ioutR= Reservoir details
c	  	ioutRe=Diversion to Recharge details
c		  ioutRe2=Reservoir recharge data
c
      iout=0

      if(iout.gt.0 .or. ichk.eq.4) then
        write(nlog,*) ' VirSet'
        write(6,*) ' VirSet'
      endif
      
      ioutR=0
      ioutRe=0
      ioutRe2=0
      small=0.001

c               Monthly model
      if(ityp.eq.0) then
c
c _________________________________________________________
c
c               Step 1; Initilize for monthly
        fac = mthday(mon)*factor    

        do is=1,numsta
          dumx(is)=0.0
          dumy(is)=0.0
          dumz(is)=0.0
          dumU(is)=0.0
          dumR(is)=0.0
          
          qhistox(IS)=0.
          rtnacux(IS)=0.
          divacux(is)=0.
          
          depacu(is)=0.0
          qdivsx(is)=0.0
          qdivsox(is)=0.0
          qcux(is)=0.0
c
c rrb 2006/07/31; Add loss          
          qloss(is)=0.0
          qpump(is)=0.0
        end do 
c
c _________________________________________________________
c
c               Step 2; Set Monthly station data
c
        do iru=1,numrun
          virinpx(iru)=virinp(mon,iru)           
        end do  
        
        do is=1,numsta
          tempq(is) = 0.0
          tempi(is) = 0.0
          tempd(is) = 0.0
          tempr(is) = 0.0
          tempw(is) = 0.0
          tempe(is) = 0.0
          temps(is) = 0.0
          temph(is) = 0.0
          tempno(is) = 0.0
c
c rrb 01/07/30; Add Soil Moisture and CU
          tempts(is)=0.0
          tempfs(is)=0.0
          tempcu(is)=0.0
c
c rrb 2006/07/31; Add Loss and pumping                      
          templ(is)=0.0
          tempp(is)=0.0
c
c rrb 2008/10/28; Add Reservoir Seepage & Diversion to Seepage          
          tempRre(is)=0.0            
          tempDre(is)=0.0 
          tempUse(is)=0.0                     
        end do
c
c _________________________________________________________
c
c               Step 3; Set Monthly diversion data
c
        do nd=1,numdiv
          diverx(nd)=diver(mon,nd)
          diwrx(nd)=diwr(mon,nd)
          qdivs(nd)=0.0
          qdivso(nd)=0.0
c
c ---------------------------------------------------------
c               Diversion to Recharge & Use       
          dRech(nd)=dRechM(mon,nd)   
          dUse(nd)=dUseM(mon,nd)
          
          if(ioutRe.eq.1) then
            if(nd.eq.1) write(nlog,300)
            write(nlog,'(3i5,1x,a12,20f8.0)')
     1        nd, iyr, mon, cdivid(nd), 
     1        diverx(nd)*fac, drech(nd)*fac,Duse(nd)*fac 
          endif
        end do
c
c _________________________________________________________
c
c               Step 4; Set Monthly well data
c                                       
        do nw=1,numdivw
          diverwx(nw)=diverw(mon,nw)
          diwrwx(nw)=diwrw(mon,nw)
          qdivsw(nw)=0.0
          qdivswo(nw)=0.0   
          
cx          if(nw.eq.44) write(nlog,*) ' VirSet; ',iyr,mon,nw,
cx     1     Diverwx(nw)*fac       
        end do
c
c _________________________________________________________
c
c               Step 5; Set Monthly reservoir data
c                                                 
        do nr=1,numres
          resvolx(nr)=resvol(mon,nr)
          RlossR(nr)=0.0
c
c ---------------------------------------------------------
c rrb 2008/10/28; Reservoir to recharge
          Rrech(nr)=RrechM(mon,nr)            
          if(ioutRe2.eq.1) then
            if(nr.eq.1) write(nlog,310)
            write(nlog,'(3i5,1x,a12,20f8.0)')
     1        nr, iyr, mon, cresid(nr), rrech(nr)*fac
          endif          
        end do
      endif
c
c =========================================================
c               Daily model Initilize
      if(ityp.eq.1) then
c
c _________________________________________________________
c
c               Step 6; Day 1; Set Daily station data
c                                           
        if(idy.eq.1) then
          do is=1, numsta
            tempqm(is) = 0.0
            tempim(is) = 0.0
            tempdm(is) = 0.0
            temprm(is) = 0.0
            tempwm(is) = 0.0
            tempem(is) = 0.0
            tempsm(is) = 0.0
            temphm(is) = 0.0
            tempnom(is) = 0.0
c
c rrb 01/07/30; Add soil moisture
            temptsm(is) = 0.0
            tempfsm(is) = 0.0
            tempcum(is) = 0.0
c
c rrb 2006/07/31; Add Loss and pumping            
            templm(is) = 0.0
            temppm(is) = 0.0
c
c rrb 2008/10/28; Add Reservoir Seepage
            tempRreM(is)=0.0            
            tempDreM(is)=0.0
            tempUseM(is)=0.0
            
            qhistod(32,is)=0.0
          end do
        endif    
c
c _________________________________________________________
c
c               Step 7; Initilize for monthly or daily model
        do is=1,numsta
          qhistox(IS)=0.
          rtnacux(IS)=0.
          divacux(is)=0.
          dumz(is)=0.
          depacu(is)=0.0
c
c rrb 01/12/31; Initilize on a daily basis
          qcux(is)=0.0
c
c rrb 2006/07/31; Add loss         
          qloss(is)=0.0
          qpump(is)=0.0
          
          qdivsx(is)=0.0
          qdivsox(is)=0.0

          tempq(is) = 0.0
          tempi(is) = 0.0
          tempd(is) = 0.0
          tempr(is) = 0.0
          tempw(is) = 0.0
          tempe(is) = 0.0
          temps(is) = 0.0
          temph(is) = 0.0
          tempno(is) = 0.0
c
c rrb 01/07/30; Add soil moisture and CU
          tempts(is) = 0.0
          tempfs(is) = 0.0
          tempcu(is) = 0.0
c
c rrb 2006/07/31; Add Loss and pumping            
          templ(is)  = 0.0          
          tempp(is)  = 0.0
c
c rrb 2008/10/28; Add Reservoir Seepage          
          tempRre(is)=0.0            
          tempDre(is)=0.0
          tempUse(is)=0.0
          
        end do 
c
c _________________________________________________________
c
c               Step 8; Set Daily station data
c    
        do iru=1,numrun
          virinpx(iru)=virind(idy,iru)           
        end do  
c
c _________________________________________________________
c
c               Step X; Set Daily station data
c    
        
        do is=1,numsta
          qhistod(idy,is)=0.0
        end do
c
c _________________________________________________________
c
c               Step 9; Set Daily diversion data
c                                         
        do nd=1,numdiv
          diverx(nd)=diverd(idy,nd)
          diwrx(nd)=diwrd(idy,nd)
c
c rrb 2008/01/10; Correction. Initilize to and from storage          
          qdivs(nd)=0.0
          qdivso(nd)=0.0          
c
c ---------------------------------------------------------
c               Diversion to Recharge                    
          drech(nd)=drechd(idy,nd)

c
c rrb 2006/09/11; New CU Approach
          dIwrSF(nd)=diwrx(nd)*AreaSF(nd)	
          dIwrSS(nd)=diwrx(nd)*AreaSS(nd)	
          dIwrGF(nd)=diwrx(nd)*AreaGF(nd)	
          dIwrGS(nd)=diwrx(nd)*AreaGS(nd)
          
        end do
c
c _________________________________________________________
c
c               Step 10; Set Daily well data
c                                     
        do nw=1,numdivw
          diverwx(nw)=diverdw(idy,nw)
          diwrwx(nw)=diwrdw(idy,nw)
c
c rrb 2008/01/10; Correction. Initilize to and from storage          
          qdivsw(nw)=0.0
          qdivswo(nw)=0.0
 
c          
c rrb 2006/09/11; New CU
c                Step 4; Set IWR for gw lands irregardless of method
          dIWRGFw(nw)=diwrwx(nw)*AreaGFw(nw)
          dIwrGSw(nw)=diwrwx(nw)*AreaGSw(nw)   
        end do
c
c _________________________________________________________
c
c               Step 11; Set Daily reservoir data
c                                                
        do nr=1,numres
          resvolx(nr)=targetd(idy,nr)   
c
c ---------------------------------------------------------
c               Reservoir to Recharge                        
          rrech(nr)=rrechD(idy,nr)
cr        write(nlog,*) '  Virset Daily reservoir; ', 
cr   1      cresid(nr),iyr, mon, idy,nr,resvolx(nr)
        end do
      endif
c
c _________________________________________________________
c
c               Step 12; Set Daily or monthly data
c                                         
      if(ityp.eq.0 .or. ityp.eq.1) then
        if(ieffmax.eq.1) then
          do nd=1,numdiv
            nui =nduser(nd)
            nue =nduser(nd+1)-1
            if(nui.le.nue) then
              do nu=nui,nue
                diwrreq(nu)=diwrx(nu)
                dcu(nu)=0.0
                dcut(nu)=0.0
c
c rrb 01/07/30; Add for soil moisture use
                divreq(nu)=diverx(nu)
c
c rrb 2006/08/01; Add Loss
                rloss(nu)=0.0                
c
c rrb 2006/09/11; New CU Approach
                dIwrSF(nu)=diwrreq(nu)*AreaSF(nu)	
                dIwrSS(nu)=diwrreq(nu)*AreaSS(nu)	
                dIwrGF(nu)=diwrreq(nu)*AreaGF(nu)	
                dIwrGS(nu)=diwrreq(nu)*AreaGS(nu)
c               write(nlog,*) ' Virset;', mon, nu, diwrreq(nu), 
c    1            AreaSF(nu), diwrSF(nu)
                
              end do
            endif
          end do

          do nw=1,numdivw
            diwrreqw(nw)=diwrwx(nw)
            dcuw(nw)=0.0
            dcutw(nw)=0.0
c
c rrb 01/07/30; Add soil moisture use
            divreqw(nw)=diverwx(nw)
c
c rrb 2006/08/01; Add Loss
            rlossW(nw)=0.0                
c          
c rrb 2006/09/11; New CU
c                Step 4; Set IWR for gw lands irregardless of method
            dIWRGFw(nw)=diwrreqw(nw)*AreaGFw(nw)
            dIwrGSw(nw)=diwrreqw(nw)*AreaGSw(nw)   
            
          end do
c
c rrb 02/10/25; Move inside monthly or daily (ityp=0 or 1)
c         write(nlog,*) ' Virset; calling Seteff'
          call seteff
c         write(nlog,*) ' Virset; back from  Seteff'
c
c               Go to Return
          goto 500
c
c rrb 2004/04/17; Correction
        else          
          do nd=1,numdiv
            nui =nduser(nd)
            nue =nduser(nd+1)-1
            if(nui.le.nue) then
              do nu=nui,nue
                dcu(nu)=0.0
                dcut(nu)=0.0
c
c rrb 2006/08/01; Add Loss
                rloss(nu)=0.0                                
              end do
            endif
          end do      
          do nw=1,numdivw
            dcuw(nw)=0.0
            dcutw(nw)=0.0
c
c rrb 2006/08/01; Add Loss
            rlossW(nw)=0.0                
            
          end do 
        endif
      endif
c
c rrb 02/10/09; Set efficiency data
c rrb 02/10/25; moved above
cx    call seteff
c
c =========================================================
c               Daily model Sum monthly totals
      if(ityp.eq.2) then

        do is=1, numsta
          tempqm(is) = tempqm(is) + tempq(is)
          tempim(is) = tempim(is) + tempi(is)
          tempdm(is) = tempdm(is) + tempd(is)
          temprm(is) = temprm(is) + tempr(is)
          tempwm(is) = tempwm(is) + tempw(is)
          tempem(is) = tempem(is) + tempe(is)
          tempsm(is) = tempsm(is) + temps(is)
c
c rrb 01/07/30; Include soil moisture and CU
          temptsm(is) = temptsm(is) + tempts(is)
          tempfsm(is) = tempfsm(is) + tempfs(is)          
          tempcum(is) = tempcum(is) + tempcu(is)
c
c rrb 2006/07/31; Add Loss & Pumpnig          
          templm(is)  = templm(is)  + templ(is)          
          temppm(is)  = temppm(is)  + tempp(is)
c
c rrb 2008/10/28; Add Reservoir Seepage          
          tempRreM(is)= tempRreM(is)+ tempRre(is)
          tempDreM(is)= tempDreM(is)+ tempDre(is)
          tempUseM(is)= tempUseM(is)+ tempUse(is)
          
c
c               Note the following allows adjustments for negative
c                 flows to be correct since
c                 qhistox is max(0,qhistox) around step 12 of vircom

          temphm(is) = temphm(is) + temph(is)
          tempnom(is) = tempnom(is) + tempno(is)

          qhistod(idy,is)=qhistox(is)
          qhistod(32,is)=qhistod(32,is) + qhistox(is)

        end do
c
c rrb 02/01/14; Test
        irx=irusta(1)
        fx=factor
c       write(99,*) ' '
c       write(99,*) '  Virset (2); irx, tempqm, tempq', irx,
c    1        tempqm(irx)*fx,tempq(irx)*fx
c       write(99,*) '  Virset (2); irx, tempim, tempi', irx,
c    1        tempim(irx)*fx,tempi(irx)*fx

c
c               Go to Return
        goto 500
      endif
c
c =========================================================
c               Daily model Set Monthly running total to standard
      if(ityp.eq.3) then
        rimd=float(imd)
c       rimd=1.0
        do is=1, numsta
          tempq(is) = tempqm(is)/rimd
          tempi(is) = tempim(is)/rimd
          tempd(is) = tempdm(is)/rimd
          tempr(is) = temprm(is)/rimd
          tempw(is) = tempwm(is)/rimd
          tempe(is) = tempem(is)/rimd
          temps(is) = tempsm(is)/rimd
          temph(is) = temphm(is)/rimd
          tempno(is) = tempnom(is)/rimd
c
c rrb 01/07/30; Include soil moisture and CU
          tempts(is) = temptsm(is)/rimd
          tempfs(is) = tempfsm(is)/rimd
          tempcu(is) = tempcum(is)/rimd
c
c rrb 2006/07/31; Add Loss and pumping            
          templ(is)  = templm(is)/rimd
          tempp(is)  = temppm(is)/rimd          
c
c rrb 2008/10/28; Add Reservoir Seepage          
          tempRre(is)=tempRreM(is)/rimd
          tempDre(is)=tempDreM(is)/rimd
          tempUse(is)=tempUseM(is)/rimd
          

          qhistod(32,is) = qhistod(32,is)/rimd
        end do
c
c rrb 02/01/14; Test
        irx=irusta(1)
        fx=factor
c       write(99,*) ' ' 
c       write(99,*) '___________________________'
c       write(99,*) '  Virset (3); irx, tempqm, tempq', irx,
c    1        tempqm(irx)*fx,tempq(irx)*fx
c       write(99,*) '  Virset (3); irx, tempim, tempi', irx,
c    1        tempim(irx)*fx,tempi(irx)*fx
c
c               Go to Return
        goto 500
      endif
c
c =========================================================
c               Monthly model Set Monthly value
      if(ityp.eq.4) then
        do is=1, numsta
          qhisto(mon,is) = qhistox(is)            
c
c rrb 2009/06/15; Store negative flows          
          if(temph(is).le.small) then          
            dum3(mon,is)=-1.*temph(is)
          else
            dum3(mon,is)=0.0
          endif              
          
        end do
c
c               Go to Return
        goto 500
      endif
c
c =========================================================
c               Daily model Set Monthly value
      if(ityp.eq.5) then
        do is=1, numsta
c         write(nlog,*) ' Virset(4); qhistox(is), mon',qhistox(is),mon
          qhisto(mon,is) = qhistod(32,is)  
        end do
c
c               Go to Return
        goto 500
      endif  

c
c _________________________________________________________
c
c               Step 12; Return
c    

 500  return
c
c _________________________________________________________
c		Formats 
 300   format(/,72('_'),/
     1   '  VirSet;    Diversion to Recharge Data (*.dre)',//
     1   '   nd  iyr  mon ID            diverX   dRech   dUuse'/
     1   ' ____ ____ ____ ____________ _______ _______ _______')

 310   format(/,72('_'),/
     1   '  VirSet;    Reservoir to Recharge Data (*.rre)',//
     1   '   nd  iyr  mon ID             rRech',/
     1   ' ____ ____ ____ ____________ _______')
          
 
      end
