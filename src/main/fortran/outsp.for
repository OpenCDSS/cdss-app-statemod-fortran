C
c *********************************************************
c
      SUBROUTINE OUTsp
c
c
c _________________________________________________________
c	Program Description
c
c       Outsp; It prints a special parameter output file (*.xsp, *.xs2)
c
c _________________________________________________________
c       Documentation
c               itype = 0  diversion
c                       1  streamflow
c                       2  reservoir
c                       6  well
c                       99 help
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
      character ftype*24, ptype*24
c
c _________________________________________________________
c
c              Step 1; Initilize
      iout = 0
c rrb 2005/11/22; River Loss and Carrier Loss
c     ndiv = 35
c     ndiv = 37
c     ndivw= 18
c     nres = 24
c     nres = 26
c
c		Set number of output values 1x in StateM.for
c	        and store in common.inc      
      ndiv = ndivO        
      nres = nresO
      ndivw= nwelO
      
      
      nmaxd = maxparm

      nresx = nres - 3                                          
      nmaxr = nresx+1

      factor=1.9835
c
c _________________________________________________________
c
c               Step 2; Print Banner
      call outtop(21, 1, 26)
      call outtop(24, 1, 45)
c
c _________________________________________________________
c
c               Step 3; Get data type, etc
      call getin(8, iid, maxsta, idallx, idtype, ftype, ptype, idreq)
      
      if(iout.eq.1) then
        write(io99,*) ' Outsp; Ftype = ', ftype
        write(io99,*) ' Outsp; Ptype = ', ptype
        write(io99,*) '  Outsp; iid, idallx ', iid, idallx
      endif  
      
c
c rrb 05/01/12; Cannot support all. Need a tie between data type
c               and parameter type
cr    if(ftype(1:3) .eq.'All')        itype = 0 
c
      if(ftype(1:3) .eq.'All') goto 342
      
      if(ftype(1:9) .eq.'Diversion')  itype = 0 
      if(ftype(1:9) .eq.'InstreamF')  itype = 0  
      if(ftype(1:10).eq.'StreamGage') itype = 1
      if(ftype(1:10).eq.'Streamflow') itype = 1 
      if(ftype(1:10).eq.'StreamFlow') itype = 1 

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

c     write(io99,*) '  Outsp; ', numsta, numdiv, numres, numdivw
c
c _________________________________________________________ 
c
c               Step 4; Process a stream gage, diversion or ISF
c               which are all in one file as a function of location
c               on the river
c
      if(itype.le.1) then
c
c _________________________________________________________ 

c               Step 4a. Get parameter to be output
        call getpar(itype, it, ptype)
c
c _________________________________________________________ 
c
c               Step 4b. Process each station requested
        ip1=0
        ipout=0
        do 170 ip=1,nid
c
c               For a given data type (itype) get station ID to 
c               be printed
          if(idall.eq.0) then                           
            call getsta(itype2, ip, ist, idx, idreq(ip))
            idtype(ip)=itype2
            if(idx.eq.0)  then
c             write(nlog,321) ip, idreq(ip)
              goto 170
            endif  
          else
            call getid(itype,ist,idx,irt2,idreq(ip))

c
c               Handle a request located in *.out that 
c               is not a stream, diversion, or ISF by checking that
c               it has a location on the river (ist)
            if(ist.le.0) goto 170
          endif
c
c
c _________________________________________________________ 
c
c		Print Details
c rrb 05/03/23
c         ip1=ip1+1

          if(iout.eq.1) then
            write(io99,100) ip, idreq(ip)
 100        format('  Outsp; Printing ', i5, 1x, a12)
          endif
c
c
c _________________________________________________________ 


          if(iout.eq.1) then
            write(nlog,*) ' Outsp; itype, itype2, ip, idtype(ip)'
            write(nlog,*) ' Outsp;', itype, itype2, ip, idtype(ip)
          endif
c
c
c _________________________________________________________ 
c               Step 4c; Print Title
c
c               Stream gage  
c rrb 05/03/23
c         if(itype2.eq.1) then
          if(itype2.eq.1 .and. idtype(ip).eq.1) then          
            ip1=ip1+1
            ipout=1
            write(21,300) 'StreamGage      ', idreq(ip),
     1                  stanam1(IST),ptype, ip1, cunitm,
     1                  (XMONAM(I),I=1,12)
            write(24,302) idreq(ip),
     1                  stanam1(IST),ptype, ip1, cunitm            
          endif
c
c               Diversion
c rrb 05/03/23
c         if(itype2.eq.0) then
          if(itype2.eq.0 .and. idtype(ip).eq.0) then                    
            ip1=ip1+1
            ipout=1
            write(21,300) 'Diversion       ', idreq(ip),
     1                  divnam1(idx),ptype, ip1, cunitm,
     1                  (XMONAM(I),I=1,12)
            write(24,302) idreq(ip),
     1                  divnam1(idx),ptype, ip1, cunitm
          endif
c
c               Instream Flow
c rrb 05/03/23
c         if(itype2.eq.3) then
          if(itype2.eq.3 .and. idtype(ip).eq.0) then                    
            ip1=ip1+1
            ipout=1
            write(21,300) 'InstreamFlow    ',idreq(ip),
     1                  xfrnam1(idx),ptype, ip1, cunitm,
     1                  (XMONAM(I),I=1,12)
            write(24,302) idreq(ip),
     1                  xfrnam1(idx),ptype, ip1, cunitm
          endif
c
c rrb 05/03/23; 
c		Get another station of combination is not found
          if(ipout.eq.0) then
            goto 170
          endif 
         
C
          DO IM=1,13
            DATA1(IM,nmaxd)=0.
          end do
C
          DO IY=IYSTR,IYEND
            call year(iy, iyrmo, imomo, cyr1)
            DO IM=1,12
c
              IRECS=((IY-IYSTR0)*12+(IM-1))*NUMSTA+IST+numtop
              if(iout.eq.1) 
     1          write(99,*)'  Outsp; irecs = ', irecs, iout

              nf=43
              READ(43,REC=IRECS,err=340) (DAT1(I),I=1,ndiv)
              if(iout.eq.1) write(99,'(i5, 40f8.0)') 
     1                    irecs, (dat1(i), i=1,ndiv)
C                                          
              do i=1,ndiv
                DATA1(IM,i )=DAT1(i)
              end do
            end do
C
            DATA1(13,IT)=0.
            DO IM=1,12
c             DATA1(IM,IT)=DATA1(IM,IT)*cu*MTHDAY(IM)
              DATA1(IM,IT)=DATA1(IM,IT)*fmo(im)
              DATA1(13,IT)=DATA1(13,IT)+DATA1(IM,IT)
            end do
C
            DO IM=1,13
              DATA1(IM,nmaxd)=DATA1(IM,nmaxd)+DATA1(IM,IT)
            end do
C
            if(isigfig.eq.0) then
              WRITE(21,310) idreq(ip),iy, (DATA1(IM,IT),IM=1,13)
              do im=1,12
                write(24,360) idreq(ip),iyrmo(im),imomo(im),data1(im,it)
              end do
            else
              WRITE(21,312) idreq(ip),iy, (DATA1(IM,IT),IM=1,13)
              do im=1,12
                write(24,362) idreq(ip),iyrmo(im),imomo(im),data1(im,it)
              end do
            endif  
c
c               End year loop
          end do
c
c
c _________________________________________________________ 

c               Step 4e; Print average
          DO IM=1,13
            DATA1(IM,nmaxd)=DATA1(IM,nmaxd)/(IYEND-IYSTR+1)
          end do
C
          if(isigfig.eq.0) then
            WRITE(21,320) idreq(ip), (DATA1(IM,nmaxd),IM=1,13)
          else
            WRITE(21,322) idreq(ip), (DATA1(IM,nmaxd),IM=1,13)
          endif
c
c               Return for another ID
  170   continue
        goto 290
      endif
c
c
c _________________________________________________________ 
c
c               Step 5; RESERVOIR OUTPUT
c
c rrb 05/03/23; 
c     if(itype.eq.2) then
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
c
c               Step 5b; RESERVOIR TO BE OUTPUT [ID]
c
        ir1 = 0              
        nr = 0
        ip1=0

        ipout=0
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
          ipout=1
C
  200     write(21,330) idreq(ip),
     1                RESNAM1(nr),ptype, ip1,cunitm,
     1                (XMONAM(I),I=1,12)
          write(24,330) idreq(ip),
     1                RESNAM1(nr),ptype, ip1,cunitm

C
          DO IM=1,13
            DATA2(IM,nmaxr)=0.
          end do
C
          DO IY=IYSTR,IYEND
            call year(iy, iyrmo, imomo, cyr1)

            DO IM=1,12
c    
              nf=44
              IRECR=((IY-IYSTR0)*12+(IM-1))*NRSACTx+IR1+numtop
              READ(44,REC=IRECR,err=340) (DAT2(I),I=1,nres)

              ida  = dat2(nres-1) 
              nacc = dat2(nres)
C                                           
              do i=1,nresx
                DATA2(IM,i )=DAT2(i)
              end do
            end do
C
            DATA2(13,IT)=0.
            DO IM=1,12
c             DATA2(IM,IT)=DATA2(IM,IT)*cu*MTHDAY(IM)
              DATA2(IM,IT)=DATA2(IM,IT)*fmo(im)
              DATA2(13,IT)=DATA2(13,IT)+DATA2(IM,IT)
            end do
C
            DO IM=1,13
              DATA2(IM,nmaxr)=DATA2(IM,nmaxr)+DATA2(IM,IT)
            end do

            if(isigfig.eq.0) then
              WRITE(21,310) idreq(ip),iy, (DATA2(IM,IT),IM=1,13)
              do im=1,12
                write(24,360) idreq(ip),iyrmo(im),imomo(im),data2(im,it)
              end do                                             
            else
              WRITE(21,312) idreq(ip),iy, (DATA2(IM,IT),IM=1,13)
              do im=1,12
                write(24,362) idreq(ip),iyrmo(im),imomo(im),data2(im,it)
              end do                                             
            endif
c
c               End year loop
          end do
C
          DO IM=1,13
            DATA2(IM,nmaxr)=DATA2(IM,nmaxr)/(IYEND-IYSTR+1)
          end do
          
          if(isigfig.eq.0) then          
            WRITE(21,320) idreq(ip), (DATA2(IM,nmaxr),IM=1,13)
          else  
            WRITE(21,322) idreq(ip), (DATA2(IM,nmaxr),IM=1,13)
          endif  
c
c               Skip over subaccounts
          if(nacc-ida.gt.1) ir1 = ir1 + nacc-ida -1
  280   continue
        goto 290
      endif
c
c
c _________________________________________________________ 
c
c               Step 6; SELECTIVE WELL OUTPUT
c
c rrb 05/03/23
c     if(itype.eq.6) then
      ipout=0
      if(itype.eq.6) then
c
c _________________________________________________________ 
c
c               Step 6a; ITEM TO BE OUTPUT
c
        call getpar(itype, it, ptype)
        if(iout.eq.1) write(io99,*) '  outsp; it = ', it
c
c
c _________________________________________________________
c
c               Step 6b; Process each station requested
c
        ip1=0
        do 282 ip=1,nid

          if(idall.eq.0) then                           
c          
c rrb 01/06/06; Problem
c           call getsta(itype, ip, ist, ifound, idreq(ip))
c           if(ifound.eq.0)  goto 282
            iwt=ip
            idreq(ip)=cdividw(iwt)
          else
            call getid(itype,ist,iwt,iwt2,idreq(ip))
c
c               Handle non well request located in *.out
            if(iwt.le.0) goto 282
c
          endif

          ip1=ip1+1
          ipout=1
c
c               Well
          write(21,300) 'Well            ', idreq(ip),
     1                divnamw1(Iwt),ptype, ip1, cunitm,
     1                (XMONAM(I),I=1,12)
          write(24,302) idreq(ip),
     1                divnamw1(Iwt),ptype, ip1, cunitm

C
          DO IM=1,13
            DATA1(IM,nmaxd)=0.
          end do
C
          nf=42
          DO IY=IYSTR,IYEND
            call year(iy, iyrmo, imomo, cyr1)
            DO IM=1,12
c
              IRECS=((IY-IYSTR0)*12+(IM-1))*NUMdivw+iwt+numtop

              if(iout.eq.1) write(99,*)'  Outsp; irecs = ',
     1                       irecs, iout

              iout=1

              nf=42
              READ(42,REC=IRECS,err=340) (DAT1(I),I=1,ndivw)
              if(iout.eq.1) write(99,'(i5, 40f8.0)') 
     1                    irecs, (dat1(i), i=1,ndivw)
C                                          
              do i=1,ndivw
                DATA1(IM,i )=DAT1(i)
              end do
            end do
C
            DATA1(13,IT)=0.
            DO IM=1,12
c             DATA1(IM,IT)=DATA1(IM,IT)*cu*MTHDAY(IM)
              DATA1(IM,IT)=DATA1(IM,IT)*fmo(im)
              DATA1(13,IT)=DATA1(13,IT)+DATA1(IM,IT)
            end do
C
            DO IM=1,13
              DATA1(IM,nmaxd)=DATA1(IM,nmaxd)+DATA1(IM,IT)
            end do
C
            if(isigfig.eq.0) then
              WRITE(21,310) idreq(ip),iy, (DATA1(IM,IT),IM=1,13)
              do im=1,12
                write(24,360) idreq(ip),iyrmo(im),imomo(im),data1(im,it)
              end do
            else
              WRITE(21,312) idreq(ip),iy, (DATA1(IM,IT),IM=1,13)
              do im=1,12
                write(24,362) idreq(ip),iyrmo(im),imomo(im),data1(im,it)
              end do
            endif  
C
          end do
C
c               Calculate and print average
          DO IM=1,13
            DATA1(IM,nmaxd)=DATA1(IM,nmaxd)/(IYEND-IYSTR+1)
          end do
C
          if(isigfig.eq.0) then
            WRITE(21,320) idreq(ip), (DATA1(IM,nmaxd),IM=1,13)
          else  
            WRITE(21,322) idreq(ip), (DATA1(IM,nmaxd),IM=1,13)
          endif  
c
c _________________________________________________________
c
c               Step 7; Return for another ID
  282   continue
        goto 290
      endif
c
c _________________________________________________________
c
c               Step 8; Return
c
  290 if(ipout.eq.0) goto 344
      write(nlog,324) ip1, ftype
      return
c
c _________________________________________________________
c
c               Formats
c
  300 FORMAT(/,
     1       'Data TypeD  : ',a16,/
     1       'Station ID  : ',a12,/
     1       'Station Name: ',a24,/
     1       'Parameter   : ',A20,/
     1       'Table       : ',1x,i4,/
     1       'Units       : ',A5,//,
     1       'ID           Year',
     1       12(4X,A4),'   TOTAL',/
     1       '____________ ____', 13(' _______'))
  302 FORMAT(/,/
     1       'Station ID  : ',a12,72X,/
     1       'Station Name: ',a24,/
     1       'Parameter   : ', A20,/
     1       'Table       : ',1x,i4,/ 
     1       'Units       : ',A5,//,
     1       'ID           Year  Mon   Value',/,
     1       '____________ ____ ____ _______')

  310 FORMAT(a12,i5, 13f8.0)
  312 FORMAT(a12,i5, 13f8.1)
  
  320 FORMAT(/,'AVE ',1X,a12, 13F8.0)
  322 FORMAT(/,'AVE ',1X,a12, 13F8.1)
  
  321   format(/,
     1 '  Outsp; Warning structure # =', i5, ' Id ', a12, 'not found')            
  324   format(/,
     1 '  Outsp; FYI',i5, ' structures printed for data type = ', a24)            
  
c 330 FORMAT(/,'',/,
  330 FORMAT(/,    /,
     1       'Reservoir ID  : ',a12,69X,/
     1       'Reservoir Name: ',a24,/,
     1       'Parameter     : 'A20,/,
     1       'Table         : ',1x,i4,/,
     1       'Units         : ',A5,//,
     1       'ID           Year',
     1       12(4X,A4),'   TOTAL',/
     1       '____________ ____', 13(' _______'))
  332 FORMAT(/,/,
     1       'Reservoir ID  : ',a12,69X,/
     1       'Reservoir Name: ',a24,/,
     1       'Parameter     : 'A20,/,
     1       'Table         : ',1x,i4,/,
     1       'Units         : ',A5,//,
     1       'ID           Year  Mon   Value',/,
     1       '____________ ____ ____ _______')
  360    format(a12, i5, i5, f8.0)
  362    format(a12, i5, i5, f8.1)
c
c _________________________________________________________ 
c        Error Messages
c
  340 write(99,341) nf
  341 format(/,
     1  '  Outsp; Problem with binary data file = ', i5,/
     1  '         where 42 = Wells',/
     1  '               43 = Direct diversion',/
     1  '               44 = Reservoir')
      goto 350
c      
  342 write(99,343) 
  343 format(/,
     1  '  Outsp; Problem Data Type All is not supported for ',/
     1  '         a special Parameter output (*.xsp) because a 1:1',/
     1  '         relationship between Data Type and Parameter Type',/
     1  '         are required. Supported Data Types are:',/
     1  10x,' Diversion',/,  10x,' InstreamFlow',/, 10x,' StreamGage',/
     1  10x,' StreamFlow',/, 10x,' Reservoir',/,    10x,' Well')
      goto 350
      
  344 write(99,345) ftype
  345 format(/,
     1  '  Outsp; Problem No matches found for Data Type = ',a24/
     1  '         and the IDs turned on in the output request',/
     1  '         (*.out or *.xou) file')
      goto 350

  350 write(6,*)  ' Stopped in Outsp; see the log file (*.log)'
      write(99,*) ' Stopped in Outsp'                        
      write(6,*) 'Stop 1' 
      call flush(6)
      call exit(1)

      stop 
      END
