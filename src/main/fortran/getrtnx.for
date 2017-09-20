c
c **********************************************************

        subroutine getRtnX(
     1    nlog,     nchk,    iprintx, istrTyp, maxrtnA,  
     1    maxrtnX,  maxsta,  maxdivX, nstrtn,  in1, 
     1    in2,      nw,      ji,      je,      iin2,
     1    numsta,   ityp,    iloss,   interv,  pcttotX,  
     1    pctlosX,  irtndlX, irnordX, irnstaX, numrtnX,  
     1    nrtnX,    numdivX, istrtn,  ndnnod,  idncod, 
     1    cgoto,    divnamX, cdividX, cstaid,  cstadn, 
     1    filena)
c
c
c _________________________________________________________
c	Program Description
c
c	GetRtnX; A generic code used to read return flow data 
c		 for a Well, Reservoir, or Plan
c
c		 Note in this routine:
c		   nrtnX(nw+1) is:
c		   1. Read as the number of returns for structure nw, then
c                  2. Defined to be a running pointer
c		 In Mdainp intndlX is revised to be a pointer
c		   (e.g. the n'th reservoir return flow table)
c		 The above separation was done to be consistent
c		   with how returns for diversions and wells have
c		   historically been processed.
c
c
c
c _________________________________________________________
c
c       Update History
c
c rrb 2007/12/26; For a T&C Plan Store the Plan ID in irntsa1 not
c		  the river ID. This allows return various T&C 
c		  locations to be tracked separately
c
c rrb 2007/08/27; Add variable reset, etc. previously
c		  included in GetPln and GetRes.        
c rrb 2005/10/15; Copied GetRtnW2
c		  Future Goal, use to read all returns 
c                 (div, well and reservoirs)
c                 Revised to handle both wells and reservoirs
c
c		  Add iStrTyp = 2 for a reservoir,
c                               3 for a diversion, 
c                               6 for a well
c				7 for a plan

c
c
c _________________________________________________________
c
c       Documentation
c                  cgoto=  diversion or well location on river
c                  ityp =  0 return pattern
c                          1 depletion pattern
c
c		   maxrtnA maximum # of all returns (diversion,
c		           wells, reservoir and plans)
c		   maxrtnX maximum # of returns for type X
c		   maxsta  maximum number of river nodes
c		   
c _________________________________________________________
c
c       Dimensions
c
c		Dimension for nubmer of returns per station type
c		 (reservoir, plan, etc.)
        dimension
     1    pcttotX(maxrtnX),   pctlosX(maxrtnX),  irtndlX(maxrtnX),   
     1    irnstaX(maxrtnX),   nrtnX(maxrtnX)     
c
c		Dimension for number of structures (MaxdivX)
        dimension
     1    divnamX(maxdivX),   cdividX(maxdivX)
c
c		Dimension for nubmer of stream nodes (maxsta)     
        dimension  
     1    cstaid(maxsta),     cstadn(maxsta),     ndnnod(maxsta),   
     1    idncod(maxsta)     
c
c		Dimension for diversions, wells, reservoir and plans     
        dimension  
     1    istrtn(maxrtnA), irnordX(maxrtnA)

        dimension  
     1   cstrtypX(10),       cstridX(10)
c
c		Local variables        
        dimension  
     1    pcttot1(maxrtnX),   irtndl1(maxrtnX),    tempX(maxrtnX),  
     1    irnsta1(maxrtnX),     itemp(maxsta) 
     

        character  
     1   cdividX*12,   cstaid*12,    cstadn*12,  crtnid*12,
     1   cgoto*12,     blank*12,     filena*256, cirtndlX*12,
     1   cistat*12,    rec32*32,     divnamX*24, 
     1   cstrtypX*12,   cstridX*5,   cCallBy*12
     
        data cstrtypX/
     1   'InStream    ', 'Reservoir   ', 'Diversion   ', 'NA          ',
     1   'NA          ', 'Well        ', 'Plan        ', 'NA          ', 
     1   'NA          ', 'NA          '/
     
        data cstridX/
     1   '*.NA ',        '*.rrf',        '*.drf',        '*.NA ',
     1   '*.NA ',        '*.wrf',        '*.prf',        '*.NA', 
     1   '*.NA ',        '*.NA '/
c
c _________________________________________________________
c
c		Step 1; Initilize
c			  iout=1 print return flow details
c		         2 summary
        iout=0

c       write(nlog,900) ityp, istrtyp
        blank = '            '
        numrtnX=0
        cCallBy='GetRtnX     '

        
        if(iout.gt.0) then
          write(nlog,900) ityp, istrtyp
        endif  
     
c
c rrb 2008/08/24; Initilize to be consistent with 
c                 diversion array nrtn      
c rrb 2009/05/26 
        if(maxdivX.gt.maxRtnX) goto 1408
        do nw=1,maxdivX
          nrtnX(nw)=0
        end do  
c
c _________________________________________________________
c
c		Step 2; Read data
c			Loop for maximum and exit when not found        
        do j=1,maxrtnX
c
c		New format (*.wrf or *.wde)         
 100      read(in2,*,end=500,err=500) cistat
          if(cistat(1:1).eq.'#') goto 100
          backspace(in2)
          if(iout.eq.1) 
     1      write(nlog,*) ' GetRtnX; RF or Dep for ID = ', cistat          

          read(in2,*,end=500,err=500) 
     1        cistat,crtnid,pcttot1(j),irtndl1(j)          
          if(iout.eq.1) write(nlog,*) ' GetRtnX; ',j,  
     1        cistat,crtnid,pcttot1(j),irtndl1(j)          
c
c _________________________________________________________
c
c		Step 3; Find structure with return data
c		Note istop=0 Stop if not found
c		     istop=1 OK if not found
c
c	  write(nlog,*) '  GetRtnX; Step 3'		          
          istop=0
          istr=istrTyp
          call stafind(nlog,istop,istrTyp,numdivX,nw,cistat,
     1      cdividX,cCallBy) 
          if(nw.eq.0) goto 1400
          
          nrtnX(nw+1)=nrtnX(nw+1)+1
c         write(nlog,*) ' GetRtnX; nw ', nw, nw+1, nrtnx(nw+1)
          numrtnX=numrtnX+1     
          tempX(j)=nw
c
c _________________________________________________________
c
c 		Step 4; Locate return on network
c		Note istop=0 Stop if not found
c		     istr=0 for stream network
c         write(nlog,*) '  GetRtnX; Step 4a'		          
          istop=0
          istr=0
          call stafind(nlog,istop,istr,numsta,is,crtnid,
     1      cstaid,cCallBy)            
          if(is.eq.0) goto 1404          
          irnsta1(j)=is
          
c
c _________________________________________________________
c
c		Step 5; Check data check for return to end of network
          if(cstadn(is).eq.blank) then
            write(nlog,1272) crtnid,cistat
            write(6,1272) crtnid, cistat
            goto 9999
          endif
c
c _________________________________________________________
c
c		Step 6; For a return flow, check if the structure returns
c                       to itself
          if(ityp.eq.0) then
            iss=is
            ndns = ndnnod(iss)
            do n=1,ndns           
cr            if(cstaid(iss).eq.cgoto) then
              if(cstaid(iss).eq.cistat) then
              
              if(iprintX.eq.0) then
                rec32='Well Depletion Location'                  
                write(nlog,1281) rec32                  
                write(nchk,1284)                                    
              endif  
              
              iprintX=iprintX+1              
              write(nchk,1285) 
     1          iprintX, cdividX(nw), divnamX(nw),
     1          'Return Flow ', cistat, crtnid
cr              goto 9999
              endif
              iss=idncod(iss)
            end do
          endif
c
c _________________________________________________________
c
c		Step 7; For a depletion, check if the structure returns
c                       upstream of it's location on river  
          if(ityp.eq.1) then
            iss=is
            ndns = ndnnod(iss)            
            do n=1,ndns           
              iss=is
              ndns = ndnnod(iss)
            
              if(cstaid(iss).eq.cgoto .and. crtnid.ne.cgoto) then
                if(iprintX.eq.0) then
                  rec32='Well Depletion Location'                  
                  write(nlog,1281) rec32                  
                  write(nchk,1284)                                    
                endif  
                
                iprintX=iprintX+1
                write(nchk,1285) 
     1            iprintX, cdividX(nw), divnamX(nw),
     1            'Depletion   ',cgoto, crtnid
cr              goto 9999
              endif
              iss=idncod(iss)
           end do
         endif
c
c _________________________________________________________
c
c		Step 8; End Return Read Loop
        end do
c
c _________________________________________________________
c
c		Step 9; Warn the dimension is exceeded        
c
c rrb 2007/08/04; Revise to read with operating rule data
c       write(nlog,1300) maxrtnX
        if(j.gt.maxrtnX) then
          write(nlog,1300) maxrtnX
          goto 9999
        endif
 500  continue
c _________________________________________________________
c    
c 2005/10/14; 
c		Step 10; Store in historic array positions
c                        even if provided in a random order        
      j1=0
      do nw=1,numdivX
        do j=1,numrtnX
          k=tempX(j)
          if(k.eq.nw) then
            j1=j1+1
            pcttotX(j1) = pcttot1(j)
            irtndlX(j1) = irtndl1(j)          
            irnstaX(j1) = irnsta1(j)
          endif
        end do
      end do      
c _________________________________________________________
c rrb 2008/08/27; Add the following and remove from GetPln
c		  and GetResc
c               Step 10; Redefine # of returns (nrtnX) and 
c			 Set Order and check array size limits
      nrtnX(1)=1
      do n=1,numdivX
        nrtnX(n+1) =nrtnX(n+1) +nrtnX(n)
      end do
C
      DO IS=1,NUMSTA
        ITEMP(IS)=0
      end do
C

      DO Nx=1,numrtnX
        IS=irnstaX(Nx)
        ITEMP(IS)=1
      end do
C
c rrb 98/12/11; Increment nstrtn = total number of returns (diversions,
c		wells, reservoirs and plans). 
c		Set istrtn(nstrtn) its location	on the river.
c		Set irnordX(is) the return ID given the river location.
      DO IS=1,NUMSTA
        IF(ITEMP(IS).ne.0) then
          NSTRTN=NSTRTN+1
          istrtn(nstrtn)=is
          irnordX(is)=nstrtn
           if(iout.eq.1) then
             write(nlog,910) is, nstrtn, irnordX(is),cstaid(is)
           endif
        endif
      end do
c
c rrb01/10/08; Check dimension all return flow data, where all
c		includes diversions, wells, reservoirs and plans
c rrb 2010/09/15; Update
cx      if(nstrtn.gt.maxrtnX) then
cx        write(nlog,1021) nstrtn, maxrtnX
      if(nstrtn.gt.maxrtnA) then
        write(nlog,1021) nstrtn, maxrtnA
        goto 9999
      endif
c
c _________________________________________________________
c
c		Step 11; Detailed Check 
      if(iout.ge.1) then
        write(nlog,1430)
        j2=0
        do n=1,numdivX
          jb=nrtnX(n)
          je=nrtnX(n+1)-1

          do j=jb,je
            j2=j2+1
            is = irnstaX(j)
            write(nlog,1432) j2, cdividX(n), n, jb, je, j, 
     1        cstaid(is), pcttotX(j), irtndlX(j)            
          end do
          
          if(je.lt.jb) then          
            j2=j2+1
            write(nlog,1432) j2, cdividX(n), n, jb, je, -1 
          endif
        end do    
      endif  
c     
c _________________________________________________________
c
c		Step 12; Print summary     
      write(nlog,927) numrtnX
 927  format(/,
     1      '  GetRtnX; Number of return or depletions read = ', i5)
       
c _________________________________________________________
c
c		Step 13; Return
      return 
c
c _________________________________________________________
c
c               Error Handling
c
  928 write(nlog,929) in2, filena
  929 format(/,72('_'),/
     1' GetRtnX; Problem reading file # ', i4,/,
     1       '   File name: ', a256,/
     1       '   Problem record (next line):')
      goto 9999
      
      
 1400 write(nlog,1402) cistat, cstrtypX(istrtyp), cstridX(istrtyp)
 1402 FORMAT(/,72('_'),/
     1 '  GetRtnX; Problem.'
     1 ' Structure ',a12,' of:',/
     1 '           The ',a12,' station return (',a5, ') file ',
     1            'not found.',/
     1 '           Recommend you revise the plan (*.pln) or plan',
     1            'return (*.prf) file') 
      goto 9999
      
 1404 write(nlog,1406) cistat, cstrtypX(istrtyp), cstridX(istrtyp),
     1  crtnid
 1406 FORMAT(/,72('_'),/
     1 '  GetRtnX; Problem.'
     1 ' Structure ',a12,' of:',/
     1 '           The ',a12,' station return (',a5, ') file ',/
     1 '           Returns to ', a12, ' that cannot be found.',/
     1 '           Recommend you revise the plan (*.pln) file or',/
     1 '           plan return (*.prf) file or reservoir return',/
     1 '           file (*.rrf)')      
      goto 9999
      
      
 1408 write(nlog,1410) istrtyp, maxDivX, maxRtnX
 1410 FORMAT(/,72('_'),/
     1 '  GetRtnX; Problem'/
     1 11x,'For structure type ', i5,/
     1 11x '  where 1=isf, 2=res, 3=div, 4=power, 6=well and 7=plan',/
     1 11x,'The structure dimension (maxDivX)           = ', i5,/
     1 11x,'Exceeds the return flow dimension (maxRtnX) = ', i5,/
     1 11x,'Reconmend you revise Statem.F & the common block',/
     1 11x,'  (common.inc)')
 
      
c
c _________________________________________________________
c               Formats
     
 900   format(/,'  GetRtnX; ',
     1     'Return data for ityp = ', i2, ' and istrtyp = ',i2,
     1     '. Where:',/
     1 11x, ' Ityp    = 0 for a return,    1 for a depletion and',/
     1 11x, ' IstrTyp = 2 for a reservoir, 3 for a diversion,',/
     1 11x, '           6 for a well, and  7 for a plan')

 910  format(
     1 '  GetRtnX;      is  nstrtn irnordX cstaid',/
     1 10x, 3i8,1x, a12)

1021  FORMAT(/,72('_'),/
     1  ' GetRtnX; Problem.',
     1  ' Number of return stations = ',i5, 
     1  9x, 'Exceeds the dimension = ', i5,/
     1  ' Reconmend you revise the common block size')
     
 1260 format(36x,a12,f8.0,i8)
 
 1262 format(36x,a12,f8.0,a12)
 
 1272 FORMAT(/,72('_'),/
     1 '  GetRtnX: Problem; Return flow to ',a12,
     1 ' from Structure',a12, /
     1 '           cannot be the most downstream node in a ',/
     1 '           reach. Add another node to the river ', 
     1            'network (*.rin) file')
     
 1281  FORMAT(/,
     1  '  GetRtnX; Warning See *.chk for details regarding: ', a32)
     
 1284 format(/,
     1  '  GetRtnX; Warning the following well has a depletion or ',/
     1  '           return flow that is upstream of its location on ',
     1             'the river',//
     1  '    # Well ID      Well Name               ',
     1  ' Type         River ID     Return ID',/
     1  ' ____ ____________ ________________________',
     1  ' ____________ ____________ ____________')
 1285 format(i5, 1x,a12, 1x,a24, 1x,a12, 1x,a12, 1x,a12)
 
 1290 FORMAT(/, 
     1 '  GetRtnX;',
     1       ' FYI Well Structues with Loss or Salvage Data ',/
     1       '          ID            Return    Loss Deplete',
     1                            ' Salvage   Total',/
     1       '          ____________ _______ _______ _______',
     1                            ' _______ _______')
     
 1291 format(10x,a12, 2f8.0, 16x,f8.0)
 
 1293 format(10x,a12, 16x, 3f8.0)
 
 1292 FORMAT(/,72('_'),/
     1 '  GetRtnX; Problem station ',a12,
     1 ' of the Well Station File (*.dds)',/
     1 10x, ' has the following return data:',/
     1 10x, ' Total of all returns = ', f8.2)

 1300 FORMAT(/,72('_'),/
     1 ' GetRtnX; Problem TOO MANY RETURN FLOW STATIONS, MAXIMUM = ',I5) 
 
 1430 format(/,
     1 '  GetRtnX;',/
     1 '      J2 ID                 Nx      jb      je       j',
     1 ' cstaidX      pcttotX irtndlX',/
     1 ' _______ ____________  _______ _______ _______ _______',
     1 ' ____________ _______ _______')
      
 1432 format(i8, 1x,a12,1x, 4i8, 1x,a12, f8.0, i8)               
     

 9999 write(6,1440)
      write(nlog,1450) 
      call flush(6)
 1440 format('  Stopped in GetRtnX, see the log file (*.log)')
 1450 format(/,72('_'),/
     1 '  GetRtnX; Stopped in GetRtnX')
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop 
      END
