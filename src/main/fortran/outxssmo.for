C
c *********************************************************
c
      SUBROUTINE outXssMo(numstax)
c
c
c _________________________________________________________
c	Program Description
c
c       outxssMo; It prints a structure Summary to a binary file
c
c _________________________________________________________
c       Update History
c
c	2007/09/18; Broken out from OutMon
c _________________________________________________________
c       Documentation
c
c	numstax         Number of diversions and wells
c	totothw 	Surface Water Supply
c
c _________________________________________________________
c	Dimensions
      include 'common.inc'
      
      character ftype*24, parType*24, cfail1*3, cfail2*3
      dimension
     1  totothw(numstax)
c
c _________________________________________________________
c		Step 1; Initilze
c
     
      iout=0
      
      if(iout.gt.0 .or. ichk.eq.94) then
        write(nlog,*) ' OutXssMo'
        write(6,*) ' OutXssMo'
      endif      
c
c _________________________________________________________
c		Simulate Option, Set Total supply (totothw)     
      if(ioptio.eq.2 .or. ioptio.eq.8) then
        DO ND=1,NUMDIV
          IS =IDVSTA(ND)   
          totothw(nd)=TotSup(is)
        end do     
      endif
c
c _________________________________________________________
c		Baseflow Option, Set Total supply (totothw)
c		and GW supply (divmonw)     
      if(ioptio.eq.1 .or. ioptio.eq.9) then
        DO ND=1,NUMDIV
          totothw(nd)=0.0
          NUI=NDUSER(ND)
	        NUE=NDUSER(ND+1)-1
          DO NU=NUI,NUE
            totothw(nd)=diverx(NU)
          end do  
        end do     
        
        DO Nw=1,NUMDIVw
          divmonw(nw) = diverwX(nw)
          
          nd2=idivcow2(nw)
          if(nd2.gt.0) then
            totothw(nd2)=totothw(nd2) + diverwX(nw)
          endif  
        end do     
        
      endif
c
c _________________________________________________________
c		Initilize constants
c               fx (fmo) = units for output (cfs, af, kaf, or cms)
c               fy (faf) = unit conversion af to cfs for a day
c               fz (faf) = unit conversion cfs to af/mo 
c               Monthly model fx=fy.  Daily model fx .ne. fy
      fac=MTHDAY(MON)*factor
      fx=fmo(mon)
      fz=faf(mon)
      
      small = 0.001
      smalln=-1.0*small
      irecmax=0
      
      
     
c
c _________________________________________________________
c		Print Binary (*.xss)
      
      IREC=(IYR-IYSTR)*12*numdxw+(MON-1)*numdxw+ numtop 
      
      if(iout.eq.1)
     1 write(nlog,*) ' OutXssMo;', iyr, iystr, numdxw, mon, numtop
     
      if(numdiv.gt.0) then
        do nd=1,numdiv
          irecs=irec+nd
          is=idvsta(nd)
          nw=idivco2(nd)
c
c		Calculate area from fraction data
          Area1=Area(nd)
          AreaSF1=AreaSF(nd)*Area1
          AreaSS1=AreaSS(nd)*Area1
          AreaGF1=AreaGF(nd)*Area1
          AreaGS1=AreaGS(nd)*Area1
c
c ---------------------------------------------------------
c               Task 7a; Structure Summary Diversions only 
          if(nw.eq.0) then
            rnd=float(nd)
            rnw=float(nw)
            divx=totothw(nd)
            conlos=divx*(1.0-effmaxt(nd)/100.)

            if(divx.gt.small) then
c
c               Do not include from soil (qdivso) 
c             effactd=(dcu(nd)+qdivs(nd)-qdivso(nd))
c    1                /divx*100.
              effactd=(dcu(nd)+qdivs(nd)) / divx*100.

            else
              effactd=0.0
            endif

            retd=divx - dcu(nd) - qdivs(nd) - rloss(nd)
            rett=retd
c
c rrb 2007/09/06; New Format            
            shortiw1=diverirt(mon,nd)-dcu(nd)-qdivso(nd)

c           write(io99,*) ' outmon nd, dcu, dcut', nd,dcu(nd),dcut(nd)
            irecmax=amax0(irecmax,irecs)   
c
c rrb 2007/09/06; Revised format                             
cx            write(67,rec=irecs)
cx     1        0.0,           0.0,              area(nd)/fx,
cx     1        divert(mon,nd),diverirt(mon,nd),  
cx     1        divx,          effmaxt(nd)/fx,   conlos,
cx     1        effmaxd(nd)/fx,dcu(nd),
cx     1        qdivs(nd),     retd,             rloss(nd),  effactd/fx,
cx     1        0.0,           0.0,              
cx     1        0.0/fx,        0.0/fx,           0.0,     
cx     1        0.0,           0.0,              0.0,         0.0/fx,
cx     1        soils(nd)/fz,  dcu(nd),          qdivso(nd),  dcut(nd),
cx     1        rett,          1.0,              rnd,         rnw

            write(67,rec=irecs)
     1        AreaSF1/fx, AreaSS1/fx, AreaGF1/fx, AreaGS1/fx, Area1/fx,
     1        divert(mon,nd),diverirt(mon,nd), effF(nd)*100./fx,
     1          effS(nd)*100./fx,   divx,
     1        effmaxt(nd)/fx, conlos,     dcu(nd), qdivs(nd), retd,     
     1        rloss(nd),      effactd/fx, 0.0,     0.0,       0.0,
     1        0.0,            0.0,        0.0,     0.0,  soils(nd)/fz, 
     1        dcu(nd),        qdivso(nd), dcut(nd),rett, shortiw1, 
     1        1.0,           rnd,         rnw
     
            if(iout.eq.1) write(nlog,*)
     1        ' OutXssMo; Div Only', 
     1        iyr, mon, numdxw, irec, irecs, cdivid(nd)      
     
 501      format(30f8.0)
          else
c
c ---------------------------------------------------------
c               Task 7b; Structure Summary D&W Structures
c
            rnd=float(nd)
            rnw=float(nw)

            divx=totothw(nd)-divmonw(nw)  
            conlos=divx*(1.0-effmaxt(nd)/100.0)            
            
            if(divx.gt.small) then
c
c               Do not include from soil (qdivso) 
c             effactd=(dcu(nd)+qdivs(nd)-qdivso(nd))
c    1                /divx*100.
              effactd=(dcu(nd)+qdivs(nd)) / divx*100.

            else
              effactd=0.0
            endif

            if(divmonw(nw).gt.small) then
c
c               Do not include from soil (qdivswo)
c             effactw=(dcuw(nw)+qdivsw(nw)-qdivswo(nw))
c    1                /divmonw(nw)*100.
              effactw=(dcuw(nw)+qdivsw(nw)) / divmonw(nw)*100.  

            else
              effactw=0.0
            endif

            retd=divx        - dcu(nd)  - qdivs(nd)  - rloss(nd)
            retw=divmonw(nw) - dcuw(nw) - qdivsw(nw) - rlossw(nw)
            rett=retd+retw
c
c rrb 2007/09/06; New Format            
            shortiw1=diverirt(mon,nd)-dcu(nd)-dcuw(nw)-qdivso(nd)


c
c rrb 2007/09/06; Revised format                             
cx            write(67,rec=irecs)
cx     1        areasp(nw)/fx,  areawa(nw)/fx,    area(nd)/fx,
cx     1        divert(mon,nd), diverirt(mon,nd),
cx     1        divx,           effmaxt(nd)/fx,   conlos,
cx     1        effmaxd(nd)/fx, dcu(nd),
cx     1        qdivs(nd),      retd,             rloss(nd),  effactd/fx,
cx     1        divmonw(nw),    divcapwa(nw),     
cx     1        effmaxw(nw)/fx, effmaxs(nw)/fx,   dcuw(nw),
cx     1        qdivsw(nw),     retw,             rlossw(nw), effactw/fx,
cx     1        soils(nd)/fz,   dcu(nd)+dcuw(nw), qdivso(nd), dcut(nd),
cx     1        rett,           2.0,              rnd,        rnw
cx     
     
     
            write(67,rec=irecs)
     1        AreaSF1/fx, AreaSS1/fx, AreaGF1/fx, AreaGS1/fx, Area1/fx,     
     1        divert(mon,nd), diverirt(mon,nd), effF(nd)*100./fx, 
     1          effS(nd)*100./fx,  divx, 
     1        effmaxt(nd)/fx,   conlos, dcu(nd), qdivs(nd),    retd,
     1        rloss(nd),   effactd/fx, divmonw(nw), divcapwa(nw), 
     1          dcuw(nw),
     1        qdivsw(nw),  retw, rlossw(nw), effactw/fx, soils(nd)/fz, 
     1        dcu(nd)+dcuw(nw), qdivso(nd), dcut(nd),  rett, shortiw1,
     1        2.0,            rnd,        rnw
     
            if(iout.eq.1) write(nlog,*)
     1        ' OutXssMo; Div and Well', 
     1        iyr, mon, numdxw, irec, irecs, cdivid(nd)      
     
          endif
        end do
      endif
c
c ---------------------------------------------------------
c               Task 7c; Structure Summary Well Only
c
      if(numdivw.gt.0) then

        nw1=0
        do nw=1,numdivw
          nd=idivcow2(nw)
          if(nd.eq.0) then
            nw1=nw1+1
            rnd=0.0
            rnw=float(nw)
            
            if(divmonw(nw).gt.small) then
c
c rrb 01/01/30; Do not include from soil (qdivswo)
c             effactw=(dcuw(nw)+qdivsw(nw)-qdivswo(nw))
c    1                /divmonw(nw)*100.
              effactw=(dcuw(nw)+qdivsw(nw)) / divmonw(nw)*100.

            else
              effactw=0.0
            endif

            retw=divmonw(nw) - dcuw(nw) - qdivsw(nw) - rlossw(nw)
            rett=retw
c
c rrb 2007/09/06; New Format            
            shortiw1=diverirw(mon,nw)-dcuw(nw)- qdivswo(nw)
            

            irecs=irec+numdiv+nw1
            irecmax=amax0(irecmax,irecs)   
c
c		Calculate area from fraction data
            Area1=Areaw(nw)
            AreaSF1=0.0
            AreaSS1=0.0
            AreaGF1=AreaGFw(nw)*Area1
            AreaGS1=AreaGSw(nw)*Area1          

c
c rrb 2007/09/06; Revised format                             
cx            write(67,rec=irecs)
cx    1        areasp(nw)/fx,   areawa(nw)/fx,     areaw(nw)/fx,
cx     1        diverw(mon,nw), diverirw(mon,nw), 
cx     1        0.0,            0.0,            0.0,  
cx     1        0.0,            0.0,      
cx     1        0.0,            0.0,            0.0,         0.0/fx,
cx     1        divmonw(nw),    divcapwa(nw),
cx     1        effmaxw(nw)/fx,  effmaxs(nw)/fx,  dcuw(nw),
cx     1        qdivsw(nw),     retw,             rlossw(nw),  effactw/fx,
cx     1        soilsw(nw)/fz,  dcuw(nw),        qdivswo(nw), dcutw(nw),
cx     1        rett,           3.0,              rnd,         rnw
     
     
c
c rrb 2007/09/06; Revised format                             
            write(67,rec=irecs)
     1        AreaSF1/fx, AreaSS1/fx, AreaGF1/fx, AreaGS1/fx, Area1/fx,          
     1        diverw(mon,nw), diverirw(mon,nw), 
     1        effFw(nw)*100./fx,   effSw(nw)*100./fx,          
     1        0.0,            0.0,            0.0,  
     1                        0.0, 
     1        0.0,            0.0,            0.0,         0.0/fx,
     1        divmonw(nw),    divcapwa(nw),
     1                                          dcuw(nw),
     1        qdivsw(nw),     retw,             rlossw(nw),  effactw/fx,
     1        soilsw(nw)/fz,  dcuw(nw),         qdivswo(nw), dcutw(nw),
     1        rett,           shortiw1, 
     1        3.0,            rnd,         rnw
     
            if(iout.eq.1) write(nlog,*)
     1        ' OutXssMo; Well Only', 
     1        iyr, mon, numdxw, irec, irecs, cdividw(nw)      
          endif
        end do
      endif
c
c _________________________________________________________
c		Return      
      return
      
c
c _________________________________________________________
c
c               Formats
c
 
  260 format(/,
     1 ' OutXssMo; Problem cannot find a supply or demand for ',/
     1 '           Plan number ', i5,' Plan Type ', i5,' Plan ID ',a12)
    
  500 format( ' Stopped in OutXssMo, see log file (*.log)')
c
c _________________________________________________________
c
c               Formats
  
 9999 write(6,500)
      write(nlog,500)
      write(6,*) 'Stop 1'
      call flush (6)
      call exit(1)      

      stop 
      END      
