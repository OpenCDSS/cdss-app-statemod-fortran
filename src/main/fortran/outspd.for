
c
c _________________________________________________________
c	Update History
c

c
c _________________________________________________________
c	Documentation
c

c
c _________________________________________________________
c	Dimensions
cC     Last change:  C    20 May 97    0:08 am
C
      SUBROUTINE OUTspd
c
c _________________________________________________________
c	Program Description
c
c               Daily Special parameter and id printout
c               Same as outsp but daily
c
c _________________________________________________________
c	Documentation
c
c               itype = 0  diversion
c                       1  streamflow or stream gage
c                       2  reservoir
c                       6  well
c                       99 help
c rrb 01/06/06; Added *.xs2 column output
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
c
      character ftype*24, ptype*24
c
c _________________________________________________________
c               Step 1; Initilize
      ichk1 = 0
c     ndiv = 23
c rrb 2005/11/22; River Loss and Carrier Loss
c     ndiv = 35
c     ndiv = 37

c     ndivw= 10
c rrb 2005/11/22; River Loss and Carrier Loss
c     nres = 24
c     nres = 26
      nmaxd = maxparm
c
c		Set number of output values 1x in StateM.for
c	        and store in common.inc      
      ndiv = ndivO        
      nres = nresO
      ndivw= nwelO
      
      
      nresx = nres - 3                                          
      nmaxr = nresx+1

      nfd=49
      nfr=50
      nfw=65

      factor=1.9835

c
c _________________________________________________________
c               Step 2; Print Banner
      call outtop(21, 2, 46)
      call outtop(24, 2, 47)
c
c _________________________________________________________
c               Step 3; Get data type, etc
      call getin(9, iid, maxsta, idallx, idtype, ftype, ptype, idreq)

      if(ftype(1:9) .eq.'Diversion')  itype = 0
      if(ftype(1:9) .eq.'InstreamF')  itype = 0
      if(ftype(1:10).eq.'StreamGage') itype = 1
      if(ftype(1:10).eq.'Streamflow') itype = 1

      if(ftype(1:9) .eq.'Reservoir')  itype = 2
      if(ftype(1:4) .eq.'Well')       itype = 6

      itype2=itype
      if(ftype(1:9) .eq.'InstreamF')  itype2 = 3  
c
c               Set id screen switch
c     if(idreq(1).ne.'0           ') then
      if(idallx.ne.0) then
        idall=iid
        nid=iid
      else
        idall=0
        nid=numsta
        if(itype.eq.2) nid=numres
        if(itype.eq.6) nid=numdivw
      endif

c     write(io99,*) '  Outspd; ', numsta, numdiv, numres, numdivw
c
c _________________________________________________________ 
c               Step 4; Process a diversion
c
      if(itype.le.1) then
c
c _________________________________________________________ 

c               Step 4a. ITEM TO BE OUTPUT 
        call getpar(itype, it, ptype)
c
c _________________________________________________________ 
c
c               Step 4b. Process each station requested
        ip1=0
        do 170 ip=1,nid

          if(idall.eq.0) then                           
            call getsta(itype, ip, ist, idx, idreq(ip))
            if(idx.eq.0)  goto 170
          else
            call getid(itype,ist,idx,irt2,idreq(ip))
c
c               Handle non stream request
            if(ist.le.0) goto 170
          endif

          ip1=ip1+1
c
c
c _________________________________________________________ 

c               Step 4c; Print Title
c
c               Stream gage  or streamflow
          if(itype2.eq.1) then
            write(21,300) idreq(ip),
     1                  STANAM1(IST),ptype, ip1, cunitm2,
     1                  (i,i=1,31)
            write(24,302) idreq(ip),
     1                  STANAM1(IST),ptype, ip1, cunitm2
          endif
c
c               Diversion
          if(itype2.eq.0) then
            write(21,300) idreq(ip),
     1                  divnam1(idx),ptype, ip1, cunitm2,
     1                  (i, i=1,31)
            write(24,302) idreq(ip),
     1                  divnam1(idx),ptype, ip1, cunitm2
          endif
c
c               Instream Flow
          if(itype2.eq.3) then
            write(21,300) idreq(ip),
     1                  xfrnam1(idx),ptype, ip1, cunitm2,
     1                  (i,i=1,31)
            write(24,302) idreq(ip),
     1                  xfrnam1(idx),ptype, ip1, cunitm2
          endif

          do id=1,32
            datad(id,nmaxd)=0.0
          end do
C
          DO IY=IYSTR,IYEND
            call year(iy, iyrmo, imomo, cyr1)
            
            DO IM=1,12
              do id=1,mthday(im)
c
c               IRECS=((IY-IYSTR0)*12+(IM-1))*NUMSTA+IST+numtop
                IRECS=((IY-IYSTR0)*12+(IM-1))*NUMSTA*31 +
     1                (id-1)*numsta+IST+numtop   
                if(ichk1.eq.1) then
                  write(99,*)'  Outspd; irecs = ',irecs,ichk1
                endif

                nf=nfd
                READ(nfd,REC=IRECS,err=340) (DAT1(I),I=1,ndiv)
                if(ichk1.eq.1) write(99,'(i5, 30f8.0)') 
     1                      irecs, (dat1(i), i=1,ndiv)
C                                          
                do i=1,ndiv
                  DATAd(id,i)=DAT1(i)*fdy(im)
                end do
              end do
c
c rrb 01/06/06; Column output plus monthly total
              datad(32,it)=0.0 
              do id=1,mthday(im)
                write(24,360) idreq(ip),iyrmo(im),imomo(im),id,
     1                      datad(id,it)

                DATAd(id,nmaxd)=DATAd(id,nmaxd)+DATAd(id,IT)  
c               write(io99,*) datad(id,it), datad(id,nmaxd)

                DATAd(32,nmaxd)=DATAd(32,nmaxd)+DATAd(id,IT)                   
                datad(32,it)   =datad(32,it)   +datad(id,it)
              end do
c
c               Matrix output
              WRITE(21,310) idreq(ip),iyrmo(im), imomo(im),
     1            (DATAd(Id,IT),Id=1,32)
c
c               End month loop
            end do
c
c               End year loop
          end do
c
c
c _________________________________________________________ 

c               Step 4e; Print average
          DO Id=1,32
            DATAd(Id,nmaxd)=DATAd(Id,nmaxd)/((IYEND-IYSTR+1)*12.)
          end do
C
          WRITE(21,320) idreq(ip), (DATAd(Id,nmaxd),Id=1,32)
c
c               Return for another ID
  170   continue
c       goto 290
      endif
c
c
c _________________________________________________________ 
c               Step 5; RESERVOIR OUTPUT
c
      if(itype.eq.2) then
        itype = 2
        ir1=0
        nrsactx = nrsact + numown
C
        IF(NUMRES.eq.0 .or. NRSACT.eq.0) then
          WRITE(99,*) ' NO ACTIVE RESERVOIR FOR THE CURRENT JOB'
          GO TO 350
        endif
c
c
c _________________________________________________________ 
c               Step 5a; ITEM TO BE OUTPUT  
c
        call getpar(itype, it, ptype)
c
c _________________________________________________________ 
c               Step 5b; RESERVOIR TO BE OUTPUT [ID] 
c
        ir1 = 0              
        nr = 0
        ip1=0

        do 280 ip=1,nid

          if(idall.eq.0) then                           
            nr=ip
            idreq(ip) = cresid(nr)
          else
            call getid(itype,ns,nr,nr2,idreq(ip))
c
c               Handle a non reservoir data request
            if(ns.le.0) goto 280
c           ir1=nowner(nr)+nr-1
            ir1=0
            do j=1,nr-1               
              if(iressw(j).eq.1) ir1 = ir1+nowner(j+1)-nowner(j)+1
            end do
          endif

          ip1=ip1+1
          ir1=ir1+1
C
  200     write(21,330) idreq(ip),
     1                RESNAM1(nr),ptype, ip1,cunitm2,
     1                (i,i=1,31)
          write(24,332) idreq(ip),
     1                RESNAM1(nr),ptype, ip1,cunitm2

C
          DO IM=1,13
            DATA2(IM,nmaxr)=0.
          end do
C
          DO IY=IYSTR,IYEND
            call year(iy, iyrmo, imomo, cyr1)
            DO IM=1,12

              do id=1,mthday(im)
c    
c               IRECR=((IY-IYSTR0)*12+(IM-1))*NRSACTx+IR1+numtop
                IRECR=((IY-IYSTR0)*12+(IM-1))*NRSACTx*31 +
     1                (id-1)*nrsactx+IR1+numtop

                nf=nfr
                READ(nfr,REC=IRECR,err=340) (DAT2(I),I=1,nres)

                ida  = dat2(nres-1) 
                nacc = dat2(nres)
C                                           
                do i=1,nresx
                  DATAd(Id,i)=DAT2(i)*fdy(im)
                end do
              end do
c
c rrb 01/06/06; Column output
              datad(32,it)=0.0
              do id=1,mthday(im)
                write(24,360) idreq(ip),iyrmo(im),imomo(im),id,
     1                        datad(id,it)

                DATAd(Id,nmaxr)=DATAd(Id,nmaxr)+DATAd(Id,IT)
                DATAd(32,nmaxr)=DATAd(32,nmaxr)+DATAd(Id,IT)
                datad(32,it)=datad(32,it)+datad(id,it)
              end do                                             

c
              WRITE(21,310) idreq(ip),iyrmo(im), imomo(im),
     1                 (DATAd(Id,IT),Id=1,32)
c
c               End month loop
            end do  
c
c               End year loop
          end do
c
          DO Id=1,32
             DATAd(Id,nmaxr)=DATAd(Id,nmaxr)/((IYEND-IYSTR+1)*12.)
          end do
          WRITE(21,320) idreq(ip), (DATAd(Id,nmaxr),Id=1,32)
c
c               Skip over subaccounts
          if(nacc-ida.gt.1) ir1 = ir1 + nacc-ida -1
  280   continue
      endif
c
c
c _________________________________________________________ 

c               Step 6; SELECTIVE WELL OUTPUT
c ___________________________________________________
c
      if(itype.eq.6) then
c
c _________________________________________________________ 
c               Step 6a; ITEM TO BE OUTPUT 
c
        call getpar(itype, it, ptype)
c
c
c _________________________________________________________ 
c               Step 6b; Process each station requested
c
        ip1=0
        do 282 ip=1,nid

          if(idall.eq.0) then                           
            iwt=ip
            idreq(ip)=cdividw(iwt)
          else
            call getid(itype,ist,iwt,iwt2,idreq(ip))
c
c               Handle non well requests
            if(iwt.le.0) goto 282
          endif

          ip1=ip1+1
c
c               Title
c         write(io99,*) '  Outspd; ip, ist', ip, ist
          write(21,300) idreq(ip),
     1                divnamw1(Iwt),ptype, ip1, cunitm2,
     1                (i,i=1,31)
          write(24,302) idreq(ip),
     1                divnamw1(Iwt),ptype, ip1, cunitm2

C
          DO IM=1,13
            DATA1(IM,nmaxd)=0.
          end do
C
          DO IY=IYSTR,IYEND
            call year(iy, iyrmo, imomo, cyr1)
            DO IM=1,12
              do id=1,mthday(im)
c
c               IRECS=((IY-IYSTR0)*12+(IM-1))*NUMdivw+iwt+numtop
                IRECS=((IY-IYSTR0)*12+(IM-1))*NUMdivw*31 +
     1                (id-1)*numdivw+iwt+numtop
                if(ichk1.eq.1) write(99,*)'  Outspd; irecs = ',
     1                         irecs, ichk1

                nf=nfw
                READ(nfw,REC=IRECS,err=340) (DAT1(I),I=1,ndivw)
                if(ichk1.eq.1) write(99,'(i5, 30f8.0)') 
     1                      irecs, (dat1(i), i=1,ndivw)
C                                          
                do i=1,ndivw
                  DATAd(Id,i)=DAT1(i)*fdy(im)
                end do
              end do
c
c rrb 01/06/06; Column output
              do id=1,mthday(im)
                write(24,360) idreq(ip),iyrmo(im),imomo(im),id,
     1           datad(id,it)

                 DATAd(Id,nmaxd)=DATAd(Id,nmaxd)+DATAd(Id,IT)
                 DATAd(32,nmaxd)=DATAd(32,nmaxd)+DATAd(Id,IT)
                 datad(32,it)=DATAd(32,it)+DATAd(Id,IT)
              end do

c
c               Matrix output
              WRITE(21,310) idreq(ip),iyrmo(im), imomo(im),
     1                   (DATAd(Id,IT),Id=1,32)
c
c               End month loop
            end do
c
          end do
c
c               Calculate and print average
          DO Id=1,32
            DATAd(Id,nmaxd)=DATAd(Id,nmaxd)/((IYEND-IYSTR+1)*12.)
          end do
C
          WRITE(21,320) idreq(ip), (DATAd(Id,nmaxd),Id=1,32)
c
c _________________________________________________________ 
c               Step 7; Return for another ID
  282   continue
c       goto 290
      endif
c
c _________________________________________________________ 
c               Step 8; Return
c
  290 return
c
c _________________________________________________________ 
c               Formats
c ___________________________________________________
c 300 FORMAT(/,'',/
  300 FORMAT(/,'',/
     1       'Station ID  : ',a12,72X,/
     1       'Station Name: ',a24,/
     1       'Parameter   : ',A20,/
     1       'Table       : ',1x,i4,/
     1       'Units       : ',A5,//,
     1       'ID          Year   Mon',
     1       31(4X,i4),'   TOTAL',/
     1       '____________ ____ ____', 32(' _______'))
  302 FORMAT(/,/
     1       'Station ID  : ',a12,72X,/
     1       'Station Name: ',a24,/
     1       'Parameter   : ', A20,/
     1       'Table       : ',1x,i4,/ 
     1       'Units       : ',A5,//,
     1       'ID           Year  Mon  Day   Value',/,
     1       '____________ ____ ____ ____ _______')

  310 FORMAT(a12, i5, i5, 32f8.0)
  320 FORMAT(/,a12, '  AVE',5X, 32F8.0)
c 330 FORMAT(/,'',/,
  330 FORMAT(/,'',/,
     1       'Reservoir ID  : ',a12,69X,/
     1       'Reservoir Name: ',a24,/,
     1       'Parameter     : 'A20,/,
     1       'Table         : ',1x,i4,/,
     1       'Units         : ',A5,//,
     1       'ID          Year   Mon',
     1       31(4X,i4),'   TOTAL',/
     1       '____________ ____ ____', 32(' _______'))
  332 FORMAT(/,/,
     1       'Reservoir ID  : ',a12,69X,/
     1       'Reservoir Name: ',a24,/,
     1       'Parameter     : 'A20,/,
     1       'Table         : ',1x,i4,/,
     1       'Units         : ',A5,//,
     1       'ID           Year  Mon  Day   Value',/,
     1       '____________ ____ ____ ____ _______')
  360    format(a12, i5, i5, i5, f8.0)
c
c _________________________________________________________ 
c        Error Messages
c
c 340 write(99,*)  '  Outspd; Problem with binary data'
  340 write(99,341) nf
  341 format(
     1  '  Outspd; Problem with binary data file = ', i5,/
     1  '          where 49 = Daily Direct diversion',/
     1  '                50 = Daily Reservoir',/
     1  '                65 = Daily Wells')

      goto 350

  350 write(6,*)  '  Stopped in Outspd; see the log file (*.log)'
      write(99,*) '  Stopped in Outspd'                        
      write(6,*) 'Stop 1' 
      call flush(6)
      call exit(1)

      stop 
      END
