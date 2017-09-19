c
c *********************************************************
c
      subroutine outdivc(cplot, nreach)
c
c
c _________________________________________________________
c	Program Description
c
c       Outdivc; it prints a comparison of:
c                Historic Vs simulated Streamflows
c               Historic Vs Simulated Diversions
c
c _________________________________________________________
c       Update History
c
c rrb 95/06/26; Revised to handle less diversions than simulated
c               and in any order
c rrb 96/06/11; Revised to use a temporary binary file to 
c               reduce dimension size
c rrb 99/12/27; Revised to print averages when data is available
c
c rrb 99/12/28; Revised to handle new number convention as follows:
c                  1 -  5000  diversions
c               5001 -  7500  instream flows
c               7501 -  10000 reservoirs
c               -1   - -10000 baseflow plus structure
c               < -10000     baseflow only
c
c rrb 00/11/29; Added Basin Total Capability
c rrb 01/03/06; Added adjustments to diversion totals to better match
c               data for carriers and structures downstream of carriers.
c               see skipped and added.
c rrb 02/05/09; Revised call average (maxuse = (maxsta.  Yes it
c               caused the basin totals to be wrong.               
c
c               file 80 = temporary diversions
c               file 81 = temporary streamflows
c
c _________________________________________________________
c       Documentation
c
c               Following is how the common variable dum is used
c                       in this subroutine and subroutine average.
c
c               dum(1-13, 1-4)   = average by month (1-12) and year (13)
c                                  for elements 1-4
c               dum(1-4, 5-iy+5) = annual total for elements 1-4
c                                  in years iy=1,nyr
c               cplot = blank print monthly, annual, ave monthly and, 
c                       for diversions, basin total
c               cplot = Annual print annual, ave monthly and 
c                       for diversions, basin total
c               cplot = BasinTotal, for diversions, prints basin total 
c             
c               nsup = column of binary data with total supply
c               nstr = column of stream outflow from node
c               nwell= column of From Well
c               nod  = diversion file number
c               nos  = stream file number
c 		          ndid = location of stream ID
c		            ndst = location of stream type
c
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'

      dimension nmo(13), recAdj(100)

      character 
     1  blank*12,  cdx*12,   cplot*12, cistat*12, recin*256,
     1  ctemp*12,  rec12*12, rec24*24, recAdj*50, rec5*5, rec50*50,
     1  rec12b*12, rec72*72, recTypX*16
c
c _________________________________________________________
c		Step 1; Initilze
c
c          iout=1 print informatoin on every diversion
c          ioutAdj = 1 print adjustment information
      ioutD=0
      ioutS=0
      ioutAdj=0
      iout =0
      
c     write(6,*) ' '
      write(6,*) ' Subroutine Outdivc; nreach ', nreach
      write(6,*) ' '             
      write(nlog,101) nreach
 101    format(/,72('_'),/
     1  '  OutDivC; Stream and Diversion Comparison ', i5)
      
c
c-------------------------------------------------------------------
c		Open and Print header to Report File
      if(cplot.eq.'Report      ') then
        call namext(maxfn, filenc, 'Ysc', filena) 
        open(111,FILE=filena,STATUS='Unknown')
        
        call outtop(111,1,54)
        write(111,331)
        
        call namext(maxfn, filenc, 'Ydc', filena)         
        open(112,file=filena,status='unknown')
        call outtop(112,1,55)
        write(112,332)
      endif  

      ctemp='N/A'
c
c		Set number of output values 1x in StateM.for
c	        and store in common.inc      
      ndiv = ndivO        
      nres = nresO
      ndivw= nwelO
      
      ndid=ndivO-3
      ndst=ndivO-2
      
c rrb 99/01/11; Wells
c     nsup = 8                                    
c     nstr = 17
c
c rrb 2005/11/29; River and Carrier Loss 
c     nsup = 11
c     nstr = 26
c     nwell = 6
c     ncarry= 9
c     ndid = 33
c
      if(ndivO.ne.38) then
        write(nlog,900) 38, ndivO
        goto 9999
      endif
      
      nsup = 13
      nstr = 28      
      nwell = 7
      ncarry= 11

      nwell=nwell
      ncarry=ncarry

      nod  = 9
      nos  = 10
      

      numdivh=0
      numhis=0

      small=0.01
      skipped=0.0
c     skipped=skipped
      added=0.0
      blank = '            '
      iprintx=0

      d1=0.0
      d2=0.0
      d3=0.0
      d4=0.0
      d5=0.0
c
      do i=1,100
        recAdj(i)=' '
      end do
        
      do is=1,numsta
        idum2(is) = 0
        idum(is) = 0
      end do  
      
      call outtop(nos,1,12)
      call outtop(nod,1,9)
      
c
c     cu=1.0
c     if(iresop.eq.2) cu=factor
c     if(iresop.eq.3) cu=factor*0.001                        

c
c               Step 2; Process & Store Historic Diversions
c-------------------------------------------------------------------
C
      if(numdiv.eq.0) then
        Write(6,*) ' NO ACTIVE DIVERSIONS FOR THE CURRENT JOB'
        Write(33,*) ' NO ACTIVE DIVERSIONS FOR THE CURRENT JOB'
        Goto 240
      endif
      
      nd=0
      iin2=4
      filena='*.ddh'
      write(nlog,394) iin2, 'Diversions (*.ddh)    '
      
  100 read(4,390,end=118,err=928) idyr,cistat,(diverm(im),im=1,12)
      nd=nd+1
      if(ioutD.eq.1) then
        write(nlog,392) nd,idyr,cistat, (diverm(im),im=1,12)
      endif
 
c rrb 01/02/27; Set to 0 if negative (an import)
      do im=1,12
        if(diverm(im).lt.small) diverm(im)=0.0
      end do

      write(1,rec=nd) idyr, (diverm(im),im=1,12)

      if(numdivh.eq.0) idyr1 = idyr

      if(idyr1 .ne. idyr) then
        goto 100
      else
        numdivh = numdivh + 1
c
c               Find corresponding diversion id
        do 110 n=1,numdiv
          if(cistat.eq.cdivid(n)) then
            idum(n) = numdivh
            goto 100
          endif
  110   continue
c       goto 420  
        write(nlog,430)  cistat,idyr
        goto 100
      endif
c
c               Step 3 Process & Store Historic Streamflows
c-------------------------------------------------------------------
c
c               numrun = # of streamflow nodes
c               numhis = # of historic gages
  118 nd=0
      iin2=3
      filena='*.rih'
      write(nlog,394) iin2, 'Stream Flow (*.rih)    '

  120 read(3,390,end=140,err=928) isyr,cistat,(runoff(im),im=1,12)
      nd=nd+1
      write(2,rec=nd) isyr, (runoff(im),im=1,12)
      
      if(ioutD.eq.1) then
        write(nlog,392) nd,isyr,cistat, (runoff(im),im=1,12)
      endif

      if(numhis.eq.0) isyr1 = isyr

      if(isyr1 .ne. isyr) then
        goto 120
      else
        numhis=numhis+1
c
c               Find streamflow gage
        do 130 is=1,numsta
          if(cistat.eq.cstaid(is)) then
            idum2(is) = numhis
            go to 120
          endif
  130   continue                            
        goto 400
      endif

  140 continue
c
c _________________________________________________________
c               Step x; Initilize Total Basin data
c			Note iopt=-1 (initilize basin total) 
c			dum=average
c			dum2=basin total
c			dum3D(iz,iy,i)= total reach iz, year iy, month i
      iopt=-1
      call average(maxsta,iopt,1,im,iy1,nyr1,
     1             1.0,d1,d2,d3,d4,d5,dum,dum2,
     1             iz, maxrch, maxyrs, dum3D) 
c
c _________________________________________________________

c               Step 4 Begin Station Loop (is=1,numsta)
c
cx140 ip = 0
      ip = 0    
      nan=0                                     

      do 230 is=1,numsta
        ndx=0
c
c               Step 5 Print Title
c-------------------------------------------------------------------
c 
c               Initilize structure data
c			Note iopt=0 (initilize average monthly data) 
        iopt=0
        call average(maxsta,iopt,1,im,iy1,nyr1,
     1   1.0,d1,d2,d3,d4,d5,dum,dum2,
     1   iz, maxrch, maxyrs, dum3D) 
c
c               Print to Screen
        c = float(is)/float(numsta)*100.0
        write(6,150) is, numsta, c
        call flush(6)
  150   format('+', '  Printing Diversion & Stream Comparison',
     1              i5,' of ', i5, '; or ',f8.0, ' % Complete')
c
c              Step 6 Get station ID (assumes one per station (na=1?)
c-------------------------------------------------------------------
c
        irecs=numsta+is+numtop
        read(43,rec=irecs,err=450) (dat1(i),i=1,ndiv)
c
c rrb 2005/11/29; River and Carrier Loss                     
c       nd = dat1(ndiv-1) 
c       na = dat1(ndiv)
        nd = dat1(ndid) 
        na = dat1(ndst)
        nd1 = abs(nd)
c       write(nlog,*) ' Outdivc; is, nd, nd1, na ', is, nd, nd1, na
c
c               Step 6a; Process a stream gage
c------------------------------------------------------------------- 
c rrb 99/12/99; New structure ID
c       if(nd.eq.0) then
        if(nd1.gt.10000) then
          if(idum2(is).eq.0) goto 230
c
c               Skip if a river station without a gage
          cdx  = blank
c
c              Print brief title for streamflow
          write(nos,260) cunitm,
     1      headin1(1), HEADIN1(2),ip
c
c              Print header for monthly streamflow
c rrb; 00/11/27; Add Basin Total
          if(cplot.eq.'BasinTotal  ' .or. 
     1       cplot.eq.'Annual      ') then
          else
            write(nos,330)
          endif
        else                           
c
c               Step 6b Process a diversion
c------------------------------------------------------------------- 
c   
c rrb 2011/02/18; Check multiple activities at a diversion
          if(na.gt.1) then  
            nan=nan+1 
            nd1=abs(nd)        
cx            iprintx=iprintx+1 
            if(ioutAdj.eq.1) then
              write(nlog,940) -1, cdivid(nd1), 
     1          divnam1(nd1), 'Multiple IDs    ', -1.    
            endif     
            na=1
          endif
c          
          if(na.le.1) then
c
c rrb 99/12/28; Note for diversions nd = 1-5000 or -1 to -5000)
c               changed nd to nd1 in this section
            nd1=abs(nd)
            if(nd1.gt.0 .and. nd1.lt.5000) then    
c
c               Skip if no historic data provided
              iskip=0
              if(idum(nd1).eq.0) then
                write(nlog,432)  nd1, cdivid(nd1)
c
c rrb 01/03/05; Total diversions skipped because no historic data
c               goto 230
                iskip=1
              endif
c
              cdx = cdivid(nd1)
              ip=ip+1         
c
c rrb 00/11/27; Add Basin total
              if(cplot.eq.'BasinTotal  ' .or. 
     1          cplot.eq.'Annual      ' .or. iskip.eq.1) then 
              else
                write(nod,250) cunitm, 
     1            headin1(1), HEADIN1(2), ip,
     1            cdx,ctemp, divnam1(nd1)
              endif
            else    
c
c ----------   Instream Diversion (no diversion data)
              goto 230
            endif
          else
c
c
c ----------   Multiple structures (do not process for now)
            goto 230
          endif

c         write(nlog,*) '  Outdivc; cplot = ', cplot

c              Print water right info for Total only
          maxwrx=maxwr
c        
          if(cplot.eq.blank) call outwr2(maxwrx, 1, is, is, nod)
c
c              Print title for monthly diversion data
c rrb 00/11/27; Add Basin total
          if(cplot.eq.'BasinTotal  ' .or. 
     1       cplot.eq.'Annual      ' .or. iskip.eq.1) then
          else
            write(nod,310)
          endif
        endif
c 
c
c rrb 99/12/27; 
        do im=1,13
          nmo(im)=0.0
        end do
        nyr1=0 
        iy1 = 0                     
c
c               Step 7: Year loop
c------------------------------------------------------------------- 
        do 182 iy=iystr,iyend
          iy1 = iy1+1
          call year(iy, iyrmo, imomo, cyr1)
c
c               Read streamflow or diversions from temp file
c rrb 99/12/28; New station numbers
          if(nd1.gt.0 .and. nd1.lt.5000) then
c
c               Step 7a; Read historic diversions
c------------------------------------------------------------------- 
c
c rrb; 01/03/05; Store skipped data
            if(iskip.eq.0) then
              irecd=(iy-iystr)*numdivh+idum(nd1)
              read(1,rec=irecd,err=460) iryr, (diverm(im), im=1,12)
            else
              do im=1,12
c               diverm(im)=-999.0
                diverm(im)=0.0
              end do
            endif
          else
c
c               Step 7a; Read historic streamflows
c------------------------------------------------------------------ 
c
            irecq=(iy-iystr)*numhis+idum2(is)
            read(2,rec=irecq,err=470) iryr, (diverm(im), im=1,12)
          endif
c
c               Step 8; Month loop
c------------------------------------------------------------------- 
          nout=0
          do 180 im=1,12
c
c               Step 8a; read binary diversion or stream data
c                        note earlier processing removed
c                        other sturucture types
c------------------------------------------------------------------- 
            irecs=((iy-iystr0)*12+(im-1))*numsta+is+numtop
            read(43,rec=irecs,err=450) (dat1(i),i=1,ndiv)

c           cx = cu         
c           if(iresop.ne.1) cx=cu*mthday(im)
c
c rrb 2005/11/29; River and Carrier Loss           
c           do i=1,ndiv-2
            do i=1,ndiv-4
              dat1(i)=dat1(i)*fmo(im)
            end do

c 
c               Step 8b; Process diversions
c------------------------------------------------------------------- 
          if(nd1.gt.0 .and. nd1.lt.5000) then
  170       format(' nd = ', i5, ' cdivid = ', a12)
c
c ---------------------------------------------------------
c rrb 2008/12/24; Reach Data from OutRch via *.rch
            iss=idvsta(nd1)
            iz=iRch(iss)          
              
c
c rrb 00/11/29; TEST to match *.xwb (except reservoir part)
c               Problem shortages at destination structures for cariers
c rrb 01/02/05; Back to original again
c              dat1(30) = Instream from river by priority
c              dat1(31) = Instream from river by storage
c              dat1(32) = From carrier by storage by operation type 3
c
c rrb 2005/11/29; River and Carrier Loss. Note
c		  Dat1 (3) is River by Priority
c		  Dat1 (4) is River by Storage
c		  Dat1 (5) is River by Other (Exchange, Plan, Carrier)
c		  Dat1 (30) is River to ISF qdiv(14)
c		  Dat1 (31) is River to Power qdiv(15) + qdiv(30)
c		  Dat1 (32) is Carrier by Sto Exch qdiv(22)
c
            dat1(nsup) = dat1(3) + dat1(4) + dat1(5)
     1                   -dat1(30) - dat1(31) + dat1(32)
cx            if(iout.eq.1) then
cx              write(nlog,*) ' OutDivC;', iy, im, iss, cdivid(nd1),
cx     1          dat1(nsup), dat1(3), dat1(4), dat1(5), 
cx     1          dat1(30), dat1(31), dat1(32)         
cx            endif
     c
c
c _________________________________________________________
c
c			Step X;   Track (do not add or subtract) water moving through
c               a carrier (dat1(ncarry) > 0 but no water supply 
c               (dat1(nsup) =0
c		            iadj=0 No adjustments
c		            iadj=1 DO NOT ADJUST for Carried water
c               iadj=1 Do adjust for Carried water
c
            iadj=1
            if(iadj.ge.1) then
              if(dat1(1).lt.small .and. dat1(ncarry).gt.small) then
c
c rrb 2006/08/14; Do not adjust for CARRIER          
c		  But total for detailed reporting
                skip1=dat1(nsup)      
                skip2=amax1(0.0,dat1(nsup)-dat1(nwell)-
     1                dat1(ncarry))
                skipX=amax1(skip1-skip2,0.0)
                skipped=skipped+skipX
                if(iadj.eq.2) dat1(nsup)=skip2
c
c ---------------------------------------------------------              

                if(iprintx.eq.0 .and. ioutAdj.eq.1) write(nlog,930)
                if(ndx.eq.0) then
                  iprintx=iprintx+1 
                  if(ioutAdj.eq.1) then
                    write(nlog,940) iprintx, cdivid(nd1), 
     1              divnam1(nd1), 'Omitted         ', skipX, skipped,
     1              dat1(nsup), dat1(3), dat1(4), dat1(5),
     1              dat1(8), dat1(9), dat1(11), 
     1              dat1(30), dat1(31), dat1(32)         
     
                  endif
     
                  recTypX='Carry Type B    '
                  call setRec50(nlog, na1, iprintX, cdivid(nd1), 
     1                 divnam1(nd1), recadj(iprintx), rectypX)      
                  ndx=nd1
                  
                endif
              endif
c
c _________________________________________________________
c
c			Step X;   Add water diverted from Carrier by Priority and
c               From Carrier by Storage/Exchange
c rrb 01/03/06; Special treatment for destination from a carrier
c               by adding from Carrier by Priority (8) and Carrier
c               by Storage/Exchange (9) when the demand is > 0.0
c               Note NOT INCLUDED in water budget to avoid double 
c               accounting with carrier itself
              c=dat1(8) + dat1(9)
              if(dat1(1).gt.small .and. c.gt.small) then
                added=added+ c  
c
c rrb 2005/11/29; River and Carrier Loss                
c               dat1(nsup) = dat1(3)  + dat1(4)  + dat1(5) + c
c    1                      -dat1(28) - dat1(29) + dat1(30)  
                dat1(nsup) = dat1(3)  + dat1(4)  + dat1(5) + c
     1                      -dat1(30) - dat1(31) + dat1(32) 
c
c ---------------------------------------------------------

                if(iprintx.eq.0 .and. ioutAdj.eq.1) write(nlog,930)
c
c ndx controls printing to once per structure
                if(ndx.eq.0) then
                  iprintx=iprintx+1 
                  if(ioutAdj.eq.1) then
                    write(nlog,940) iprintx, cdivid(nd1), 
     1              divnam1(nd1), 'Added           ', c, added
                  endif
                  recTypX='Carry Type A    '
                  call setRec50(nlog, na1, iprintX, cdivid(nd1), 
     1                 divnam1(nd1), recadj(iprintx), rectypX) 
   
                  ndx=nd1 
                  
                endif
              endif        
            endif

c
c rrb 99/06/23; Handle missing data
            if(abs(diverm(im)+999.0).lt.0.01) then
              c = -999.
              cp= -999.
              nmo1=0
            else
c
              c = diverm(im)-dat1(nsup)
              cp = 0.
              if(diverm(im).gt.0.001) cp = (c/diverm(im))*100.0
              nmo1=1
              nyr1=nyr1+1
              nmo(im)=nmo(im)+1
            endif
c 
c               Print total station output
c
c rrb 00/11/27; Add Basin total
            if(cplot.eq.'BasinTotal  ' .or. 
     1         cplot.eq.'Annual      ' .or. iskip.eq.1) then
            else
               write(nod,350)   cdivid(nd1), iyrmo(im),xmonam(im), 
     1                         diverm(im), dat1(nsup), c, cp,
     1                         divnam1(nd1), iz, nmo1
c
c ---------------------------------------------------------
 
               if(iout.eq.1) then
                 nout=nout+1
                 write(nlog,350) cdivid(nd1), iyrmo(im),xmonam(im), 
     1                         diverm(im), dat1(nsup), c, cp,
     1                         divnam1(nd1), iz, nmo1   
                endif
    
            endif
c
c ---------------------------------------------------------
c               Calculate diversion stats including basin total
c               Skip if no historic data provided
            if(iskip.eq.0) then
              
c
c ---------------------------------------------------------
c               Sum structure data
c		Note iopt=1 (sum average monthly & basin total)    
              iopt=1       
              call average(maxsta, iopt, 1, im, iy1, nyr1,  
     1                   1.0,diverm(im),dat1(nsup),c,d4,d5,dum,dum2,
     1                   iz, maxrch, maxyrs, dum3D) 
c           write(nlog,*) ' Outdivc; iz, iy, 1',iz,iy1,dum3d(iz,iy1,1)
            else
c
c rrb 2006/08/14; Double accounting skipped data? NO OK
cx            skipped=skipped+dat1(nsup)
            endif
          else
c 
c               Step 8c; Process streamflows
c------------------------------------------------------------------- 
c
c              Print station data for a gage
c rrb 99/06/23; Handle missing data
            if(abs(diverm(im)+999.0).lt.0.01) then
              c = -999.
              cp = -999.
              nmo1=0
            else
              c = diverm(im)-dat1(nstr)
              cp = 0.
              if(diverm(im).gt.0.001) cp = (c/diverm(im))*100.0
              nmo1=1
              nyr1=nyr1+1
              nmo(im)=nmo(im)+1
            endif
c
c rrb; 00/11/27; Add Basin Total
            if(cplot.eq.'BasinTotal  ' .or. 
     1         cplot.eq.'Annual      ') then
            else
              write(nos,350)   cstaid(is), iyrmo(im), xmonam(im), 
     1                         diverm(im), dat1(nstr), c, cp,
     1                         stanam1(is), iz, nmo1
     
            endif
c
c               Calculate streamflow stats but not basin total
c		Note iopt=1 (calculate statistics) 
           iopt=1
           call average(maxsta, iopt, 0, im, iy1, nyr1,
     1                   1.0,diverm(im),dat1(nstr),c,d4,d5,dum,dum2,
     1                   iz, maxrch, maxyrs, dum3D) 
          endif
c
c               End Month Loop      
  180     continue
c
c               End Year Loop
  182   continue
c 
c _________________________________________________________
c               Step 9; Statistics for diversions
c
c
        if(nd1.gt.0 .and. nd1.lt.10000) then
c
c               Print title for annual totals
c rrb 00/11/27; Add Basin total
          if(cplot.eq.'BasinTotal  ') then
          else
            write(nod,310) 
          endif

          iy5 = 5
          do 190 iy=iystr,iyend
            iy5 = iy5+1

            dum(4,iy5) = 0.0
            if(dum(1, iy5) .gt. 0.001) then
              dum(4, iy5) = dum(3, iy5) / dum(1,iy5)*100.0
            endif
c
c rrb 99/12/27;
            nyr2=1
            if(abs(dum(1,iy5) + 999.0).lt.0.01) nyr2=0
c
c               Step 9a: Print annual totals
c
c               Print Annual Diversion title
c rrb 00/11/27; Add Basin total
            if(cplot.eq.'BasinTotal  ') then 
            else
              write(nod,350)   cdivid(nd1),  iy, xmonam(13),
     1                        (dum(j,iy5), j=1,4),
     1                        divnam1(nd1), iz, nyr2
            endif              
  190     continue      
c
c
c               Step 9b: Calculate average annual for years
c                        with no missing data
c------------------------------------------------------------------- 
          iy5=5
          do iy=iystr, iyend
            iy5=iy5+1
            if(abs(dum(1,iy5)+999.0).gt.0.01) then
              nmo(13) = nmo(13) + 1
              dum(13,1) = dum(13,1) + dum(1, iy5)
              dum(13,2) = dum(13,2) + dum(2, iy5)
              dum(13,3) = dum(13,3) + dum(3, iy5)
c
c               Basin Total
              dum2(13,1) = dum2(13,1) + dum(1, iy5)
              dum2(13,2) = dum2(13,2) + dum(2, iy5)
              dum2(13,3) = dum2(13,3) + dum(3, iy5)

            endif
          end do
c
c               Print title average diversions                  
c               Print title for annual totals
c rrb 00/11/27; Add Basin total
          if(cplot.eq.'BasinTotal  ') then
          else 
            write(nod,310) 
          endif  

          ry = (iyend-iystr+1)  
          if(ry.lt.0.01) goto 230

          do 200 im=1,13                                    
c
c               Print title for annual totals
c rrb 00/11/27; Add Basin total
            if(cplot.eq.'BasinTotal  ') then
            else
              if(im.eq.13) write(nod,*) ' '
            endif
c
c rrb 99/06/23; Check for missing data
            ryx=nmo(im)
            ryy=nmo(im)
            if(nmo(im).eq.0) then
              ryx=1.
              ryy=1.
              dum(im,1)=-999.0
              dum(im,2)=-999.0
              dum(im,3)=-999.0
            endif
c
c rrb 01/02/27; Set default average annual delta % to 0 if no data
c           dum(im,4) = -999.0
            dum(im,4) = 0.0

            if(dum(im,1) .gt. 0.001) then
              dum(im,4) = dum(im,3) / dum(im,1)*100.0*ryx
            endif
c 
c               Step 9c; Print average output for diversions
c------------------------------------------------------------------- 
c               Print title for annual totals
c rrb 00/11/27; Add Basin total
            if(cplot.eq.'BasinTotal  ') then
            else     
              write(nod,370)   cdivid(nd1), xmonam(im), 
     1          dum(im,1)/ryx, dum(im,2)/ryy,
     1          dum(im,3)/ryx, dum(im,4)/ryx,
     1          divnam1(nd1), iz, nmo(im)
            endif
  200     continue
  
  
c       
        else

c 
c               Step 10; Print header for Annual Streamflow
c-------------------------------------------------------------------
c          
c rrb 00/11/28; Add Basin Total 
          if(cplot.eq.'BasinTotal  ') then 
          else
            write(nos,330)
          endif

          iy5 = 5
          do 210 iy=iystr,iyend
            iy5 = iy5+1

            dum(4,iy5) = 0.0
            if(dum(1, iy5) .gt. 0.001) then
              dum(4, iy5) = dum(3, iy5) / dum(1,iy5)*100.0
            endif
c
c rrb 99/12/27;
            nyr2=1
            if(abs(dum(1,iy5) + 999.0).lt.0.01) nyr2=0
c
c               Step 10a: Annual Streamflow totals
c------------------------------------------------------------------- 
            if(cplot.eq.'BasinTotal  ') then 
            else
              write(nos,350)   cstaid(is),  iy, xmonam(13),
     1                         (dum(j,iy5), j=1,4),
     1                         stanam1(is), iz, nyr2
            endif
  210     continue      
c
c
c               Step 10b: Calculate average annual for years
c                         without missing data
c------------------------------------------------------------------- 

          iy5=5
          do iy=iystr, iyend
            iy5=iy5+1
            if(abs(dum(1,iy5)+999.0).gt.0.01) then
              nmo(13) = nmo(13) + 1
              dum(13,1) = dum(13,1) + dum(1, iy5)
              dum(13,2) = dum(13,2) + dum(2, iy5) 
              dum(13,3) = dum(13,3) + dum(3, iy5)
            endif
          end do
c
c               Print title for average streamflows                   
c rrb 00/11/27; Add Basin total
          if(cplot.eq.'BasinTotal  ') then
          else   
            write(nos,330) 
          endif

          ry = (iyend-iystr+1)
          if(ry.lt.0.01) goto 230

          do 220 im=1,13                                    
c             
c rrb 00/11/27; Add Basin total
          if(cplot.eq.'BasinTotal  ') then
          else
            if(im.eq.13) write(nos,*) ' '
          endif
c
c rrb 99/06/23; Check for missing data
c rrb 99/12/27; Print if some data is mising
c           ryx=ry
c           if(abs(dum(im,1)+999.0) .lt. 0.01) ryx=1
            ryx=nmo(im)
            ryy=nmo(im)
            if(nmo(im).eq.0) then
              ryx=1.
              ryy=1.
              dum(im,1)=-999.0
              dum(im,2)=-999.0
              dum(im,3)=-999.0
            endif

c
c rrb 01/02/27; Set default average annual delta % to 0 if no data
c           dum(im,4) = -999.0
            dum(im,4) = 0.0

            if(dum(im,1) .gt. 0.001) then
              dum(im,4) = dum(im,3) / dum(im,1)*100.0*ryx
            endif
c 
c               Step 10c; Print average output for streamflow
c------------------------------------------------------------------- 
c               Print title for annual totals
c rrb 00/11/27; Add Basin total
            if(cplot.eq.'BasinTotal  ') then
            else 
              write(nos,370)   cstaid(is), xmonam(im),
     1             dum(im,1)/ryx, dum(im,2)/ryy,
     1             dum(im,3)/ryx, dum(im,4)/ryx,
     1             stanam1(is), iz, nmo(im)
            endif
            
            
  220     continue
c 
c               Step 10d; Print average streamflow Report
c------------------------------------------------------------------- 
            if(cplot.eq.'Report      ') then
              write(111,375)   cstaid(is), 
     1             dum(13,1)/ryx, dum(13,2)/ryy,
     1             dum(13,3)/ryx, dum(13,4)/ryx,
     1             stanam1(is)
            endif
c       
        endif
c
c        End River Station Loop
c-------------------------------------------------------------------
  230 continue
c _________________________________________________________   
c 
c               Step 11; Print annual totals for a Reach (Zone)
c	                 Diversions Only
          ry = (iyend-iystr+1)
          nyr3=ry
          write(nlog,*) ' '
          write(nlog,*)
     1     ' OutDivc; Number of Stream Reaches (nreach) = ', nreach
          do iz=1,nreach
c
c               Print title
            write(nod,310)
            
            do j=1,4
              dum3(iz,j)=0.0
            end do            
            iy1=0
            
            do iy=iystr,iyend
              iy1=iy1+1
              dum3D(iz,iy1,4) = 0.0
              if(dum3D(iz,iy1,1) .gt. 0.001) then
                dum3D(iz,iy1,4) = dum3D(iz,iy1,3)/dum3D(iz,iy1,1)*100.0
              endif

              nyr2=1
              if(abs(dum3D(iz,iy1,1) + 999.0) .lt. 0.01) nyr2=0
c
c               Print standard ASCII output
cx              write(nod,351)  RchIDR(iz), iy, xmonam(13),
                write(nod,351)  RchidR(iz), iy, xmonam(13),
     1            (dum3D(iz,iy1,j), j=1,4), RchNameR(iz), iz, nyr2
c
c		Calculate average          
                do j=1,4
                  dum3(iz,j)=dum3(iz,j) + dum3D(iz,iy1,j)/ry
                end do  
            end do
c
c ---------------------------------------------------------
c		Print average per Reach (zone)            
            write(nod,373)           
            dum3(iz,4) = 0.0
            if(dum3(iz,1) .gt. 0.001) then
              dum3(iz,4) = dum3(iz,3)/dum3(iz,1)*100.0
            endif
cx          write(nod,371)  RchIdR(iz), xmonam(13),
            write(nod,371)  RchIdR(iz), xmonam(13),
     1        (dum3(iz,j), j=1,4), RchNameR(iz), iz, nmo(13)
     
     
c
c ---------------------------------------------------------
c		Print Reach Average to Reach Report
            if(cplot.eq.'Report      ') then

cx            write(112,375) RchIdR(iz),(dum3(iz,j), j=1,4),RchidDR(iz)
              write(112,375) RchIdR(iz),(dum3(iz,j), j=1,4),RchNameR(iz)
            endif
     
          end do  
          

c _________________________________________________________   
c 
c               Step 12; Print Basin annual totals for Diversions Only
c
c               Print title
          write(nod,310)
          
          iy5 = 5
          do iy=iystr,iyend
            iy5 = iy5+1

            dum2(4,iy5) = 0.0
            if(dum2(1, iy5) .gt. 0.001) then
              dum2(4, iy5) = dum2(3, iy5) / dum2(1,iy5)*100.0
            endif

            nyr2=1
            if(abs(dum2(1,iy5) + 999.0) .lt. 0.01) nyr2=0
c
c               Print standard ASCII output
              write(nod,352)  iy, xmonam(13),
     1          (dum2(j,iy5), j=1,4), -1, nyr2
          end do

c
c _________________________________________________________
c               Step 13; Print Total Basin Averages (monthly and annual) 
c               for Diversions only
c
c               Print title
          write(nod,310) 

          ry = (iyend-iystr+1)
          if(ry.lt.0.01) goto 240

          do im=1,13                                    
            if(im.eq.13) write(nod,373)           
c                         
c rrb 99/12/28; Missing data
            ryx = nmo(im)
            if(nmo(im).eq.0) then
              ryx = 1.0
              dum2(im,1) = -999.0
              dum2(im,2) = -999.0
              dum2(im,3) = -999.0
            endif

            dum2(im,4) = 0.0
            if(dum2(im,1) .gt. 0.001) then
              dum2(im,4) = dum2(im,3) / dum2(im,1)*100.0*ry
            endif
c 
c               Print average data
              write(nod,372)   xmonam(im), 
     1                        (dum2(im,j)/ryx, j=1,4),-1, nmo(im)
          end do
c 
c __________________________________________________________________
c               Step 14; Print average diversion to Reach Report
          if(cplot.eq.'Report      ') then
            write(112,375)   'Basin Total ', 
     1                        (dum2(13,j)/ryx, j=1,4), 'Basin Total '
          endif
c 
c __________________________________________________________________
c		Step 15;  
c               Print Skipped data because no historic diversion exist
          c = (dum2(13,2)-skipped-added)/ryx

c
c ---------------------------------------------------------
c			Print Omitted
c
c rrb 2011/02/17; Correction to match *.xwb 
cx          rec12='Carried (1) '
cx          write(nod,374) rec12, xmonam(13), -1*skipped/ryx
           rec12='Carry Type A'
          write(nod,374) rec12, xmonam(13), -1*added/ryx 
         
c
c ---------------------------------------------------------
c			Print Subtotal
          write(nod,378) 
          rec12='Subtotal    '
c
c rrb 2011/02/17; Correction to match *.xwb 
cx        write(nod,374) rec12, xmonam(13), 
cx   1      (dum2(13,2) - skipped)/ryx
          write(nod,374) rec12, xmonam(13), 
     1      (dum2(13,2) - added)/ryx     
c
c ---------------------------------------------------------
c			Print Skipped
          write(nod,*) ' '
c
c rrb 2011/02/17; Correction to match *.xwb 
cx        rec12='Carried (2) '
cx        write(nod,374) rec12, xmonam(13),  -1*added/ryx 
          rec12='Carry Type B'          
          write(nod,374) rec12, xmonam(13),  skipped/ryx
c
c
C------------------------------------------------------------------
c			Print Note              
          write(nod,377) added/ryx, skipped/ryx 
c
c ---------------------------------------------------------
c			Print adjusted structures                
          write(nod,930)
          do i=1,iprintX
            write(nod,379) i, recAdj(i)
          end do    
c __________________________________________________________________
c		Return
  240 return
      close(111)
      close(112)
c
c __________________________________________________________________
c
c        Formats
c
c 250 FORMAT('',/, '    Diversion Summary ',a5,/,3x,20a4,/,
  250 FORMAT(    /, '    Diversion Summary ',a5,/,3x,a80,/,
     1  3X,A80,33X, 'PAGE NO. ',I3,//,
     1  '    STRUCTURE ID (0 = total)  : ',a12,/,
     1  '    STRUCTURE ACCT (0 = total): ',a12,/,
     1  '    STRUCTURE NAME            : ',a24,/)
c 260 FORMAT('',/, '    Gage Summary ',a5,/,3x,a80,/,
  260 FORMAT(    /, '    Gage Summary ',a5,/,3x,a80,/,
     1  3X,a80,33X, 'PAGE NO. ',I3,//)
  270  format(2a12,i5, 2x, a3, 20f8.0)
  280  format(2(a12,','),i5,',', 2x, a3, ',', 20(f10.0,','))
  290  format(a12,i5, 2x, a3, 20f10.0)
  300  format(a12,',', i5,',', 2x, a3, ',', 20(f10.0,','))

  310   format(/,
     1  'Structure                      Gauged      Est.',
     1                             '     Delta     Delta',/
     1  'ID            Acc Year   Mo    Divert    Divert',
     1                             '    Divert   Divert%',
     1  ' Name',19x, '     Reach    #',/
     1  '___________  ____ ____ ____ _________ _________',
     1                             ' _________ _________ ',
     1  24('_'), ' ________ ____')

  330   format(/,
     1  'River                          Gauged      Est.',
     1                             '     Delta     Delta',/
     1  'ID                Year   Mo      Flow      Flow',
     1                             '      Flow    Flow %',
     1  ' Name',19x, '     Reach    #',/
     1  '___________       ____ ____ _________ _________',
     1                             ' _________ _________ ',
     1  24('_'), ' ________ ____')
     
  331   format(/,
     1  'River       ,    Gauged,      Est.,     Delta,     Delta,',
     1  '     ',/
     1  'ID          ,      Flow,      Flow,      Flow,    Flow %,',
     1  '                    Name')
     
  332   format(/,
     1  'River       ,    Gauged,      Est.,     Delta,     Delta,',
     1  '     ',/
     1  'ID          ,    Divert,    Divert,    Divert,  Divert %,',
     1  '                    Name')
     
  350  format(a12,           5x,i5, 2x, a3, 4f10.0,1x, a24, 1x i8,i5)
  351  format(a12,           5x,i5, 2x, a3, 4f10.0,1x, a24, 1x,i8,i5)  
  352  format('Basin Total ',5x,i5, 2x, a3, 4f10.0,1x, 
     1 'Basin Total ',11x, i8, i5)
  
  360  format(a12,',', 5x,',',i5,',', 2x, a3, ',', 4(f10.0,','),
     1        1x, a24, ',', 1x, ',', i5, ',')  

  370  format(a12,5x,'  Ave' 2x, a3, 4f10.0,1x,a24,1x,i8, i5)
  371  format(a12,5x,'  Ave' 2x, a3, 4f10.0,1x,a24,1x,i8, i5)
  372  format('Basin Total ',5x,'  Ave' 2x, a3, 4f10.0,1x,24x,1x,i8,i5)

  373  format(
     1  '___________  ____ ____ ____',
     1  ' _________ _________ _________ _________ ',
     1  24('_'), ' ________ ____')


  374  format(a12, 5x,'  Ave' 2x, a3, 8x, f12.0)
  
  375  format(a12,',', 4(f10.0,','), 1x,a24)
  
  378  format(
     1  '                                      _________')

  377  format(/, 'Note:',/
     1 '(1) The objective of this report is to compare',/
     1 4x, 'historic to estimated diversions.',//  
     1 4x, 'A Carrier Type A is when water is diverted by a carrier',/
     1 4x  '  that is then rediverted from that carrier. Note:',/
     1 4x  '  Type A Carrier water IS ADDED to the diversion estimate',/
     1 4x  '    in this diversion comparison report to get total',/
     1 4x  '    through headgate for comparison to gaged data.',/
     1 4x, '  Type A Carrier water IS NOT INCLUDED as a diversion in',/
     1 4x, '    water budget report.  Therefore when subtracted from',/
     1 4x  '    the diversion comparison total the results will the',/
     1 4x, '    same as the total diversion in a water budget report',/
     1 4x, '  Type A Carrier is identified when its demand is not ',/
     1 4x  '    zero and the From Carrier by Priority or From Carrier',/
     1 4x  '    by Sto_Exc is not zero.'//     
     
     1 4x, 'A Carrier Type B is when water is being carried',/
     1 4x  '  by a structure for use by another structure.  Note:',/
     1 4x  '  Type B Carrier water DOES NOT REQUIRE AN ADJUSTMENT in',/
     1 4X, '   this diversion comparison report to get total',/
     1 4x  '    through headgate for comparison to gaged data.',/
     1 4x, '  A type B Carrier to a reservoir IS NOT INCLUDED as a',/
     1 4x, '    diversion in a water budget report to avoid double',/
     1 4x, '    accounting with reservoir storage change',/
     1 4x, '  A type B Carrier is identified when its demand is zero',/  
     1 4x  '    and the Carried by Exchange or Bypass is non zero',//
c     

c     
     1 4x, 'Total Carrier Type A  equals: ', f12.0, ' af/yr',/ 
     1 4x, 'Total Carrier Type B  equals: ', f12.0, ' af/yr')
     
  379  format(i5, 1x, a50)
  380  format(a12,',', 5x,',', '  Ave,',2x, a3, ',', 4(f8.0,','),
     1 1x, a24, 1x, i5)  
     
  390 format(i4, 1x, a12, 12f8.0)
  392 format(2i5,1x, a12, 12f8.0)
  
  394 format(/, ' OutDivc; Reading file ', i4, 1x, a24)

     
  900  format(
     1 ' OutDivC; Problem key output variables were developed',/
     1 10x,'Based on ', i5, ' output variables but ndivO = ', i4,/
     1 10x,'Reconmend you revise OutBal2 appropriately')

  930  FORMAT(/,72('_'), /,
     1  ' Outdivc; The following structure(s) were adjusted',/
     1  '          in the diversion comparison report',//
     1  '    # Str ID       Str Name                 Comment',/
     1  ' ____ ____________ ________________________ ________________') 
  940  format(i5,1x, a12, 1x, a24, 1x, a16, 20f10.0)

c
c               Error Messages
c -----------------------------------------------------

c
  400 write(nlog,410) cistat,iryr
      write(6,410) cistat,iryr
  410 FORMAT('Outdivc: Streamflow Problem',/
     1  ' River Station ',a12,' of Historic Streamflow file',
     1  ' in year ',I5, ' not found')
      goto 440

c 420 write(nlog,430) cistat,idyr
c     write(6,430) cistat,idyr
  430 FORMAT('Outdivc: Historic  Diversion Station ',a12,' not found',
     1  ' in year ',I5)
  432 FORMAT('Outdivc: FYI',
     1  ' Diversion ID ', i5, ' Station ',a12,' has no historic data')

c     goto 440

c
  440 write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop 
c
  450 write(6,*)  '   Outdivc; Request exceeds binary file 43 size'
      write(nlog,*) '   Outdivc; Request exceeds binary file 43 size'
      goto 9999
  460 write(6,*)  '   Outdivc; Request exceeds temp diversion file'
      write(nlog,*) '   Outdivc; Request exceeds temp diversion file'
      goto 9999
  470 write(6,*)  '   Outdivc; Request exceeds temp stream file'
      write(nlog,*) '   Outdivc; Request exceeds temp stream file'
      goto 9999

c
c rrb 97/11/02; Error Handling
c _____________________________________________________________
  926 write(nlog,927) iin2, filena
  927 format(' Outdivc.f; End of file # ', i4, ' encountered',/,
     1       '   File name: ', a256)
      goto 9999
c
  928 write(nlog,929) iin2, filena
  929 format(' Outdivc.f; Problem reading file # ', i4,/,
     1       '   File name: ', a256,/
     1       '   Problem record (next line):')

      backspace(iin2)
      read(iin2, '(a256)',end=926,err=926) recin
      write(nlog,'(a256)') recin
      goto 9999

 9999 write(6,*) '  Stopped in Outdivc, see the log file (*.log)'
      write(nlog,*)'  Stopped in Outdivc'
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)
      stop 
      END      
cx       
c
c ********************************************************************  
c     SetRec50; It consolidates information for detailed log reporting 
c               into a 50 character record string (rectypX)
c        
      subroutine SetRec50(nlog, nd1, iprintX, cdividX, divnamX, 
     1           recadjX, rectypX) 
      character cdividX*12, divnamX*24, recadjX*50, recTypX*16
      character rec12*12, rec50*50, rec24*24
      
      rec50=' '
      
      i1=1
      i2=i1+12-1
      rec12=cdividX                  
      rec50(i1:i2) = rec12
      
      rec24=divnamX
      i1=i2+2
      i2=i1+24-1
      rec50(i1:i2) = rec24
      
      i1=i2+2
      i2=i1+16-1
      rec50(i1:i2) = recTypX
      recAdjX=rec50
      
      return
      end
