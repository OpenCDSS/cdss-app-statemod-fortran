c
c **********************************************************
c
        subroutine getRtnw2(
     1    nlog, nchk, iprintx,        
     1    maxrtnw,  maxsta,  maxdivw, 
     1    in1, in2, nw,ji,je, iin2,   numsta,
     1    ityp,     iloss,   interv,
     1    pcttotw,  pctlosw, irtndlw, irnstaw,
     1    numrtnX,  nrtnX,   numdivw,
     1    ndnnod,   idncod,  cgoto,   divnamw1,
     1    cdividw,  cstaid,  cstadn,  filena)
c
c
c _________________________________________________________
c	Program Description
c
c
c _________________________________________________________
c
c       Update History
c        
c rrb 2005/10/15; Copied GetRtn
c                 Revised to handle new station format
c
c
c _________________________________________________________
c
c       Documentation
c                  cgoto=  diversion or well location on river
c                  ityp =  0 return pattern
c                          1 depletion pattern
c
c
c _________________________________________________________
c
c       Dimensions

        dimension  pcttotw(maxrtnw),   pctlosw(maxrtnw),
     1             irtndlw(maxrtnw),   irnstaw(maxrtnw),
     1             ndnnod(maxsta),     idncod(maxsta),
     1             divnamw1(maxdivw), cdividw(maxdivw),
     1             cstaid(maxsta),     cstadn(maxsta)

        dimension  pcttotwX(maxrtnw), irtndlwX(maxrtnw),          
     1             irnstawX(maxrtnw), 
     1             nrtnX(maxdivw),    tempX(maxrtnw)


        character  cdividw*12,   cstaid*12,    cstadn*12, crtnid*12,
     1             cgoto*12,     blank*12,     filena*256, cirtndlw*12,
     1             cistat*12,    rec32*32,     cCallBy*12
c
c _________________________________________________________
c
c
c		iout=1 print return flow details
        iout=0

        blank = '            '
        numrtnX=0
        cCallBy='GetRtnw2    '

c       write(nlog,*) '  GetRtnW2; ityp ', ityp
        
        do nw=1,maxdivw
          nrtnX(nw)=0
        end do  

c
c _________________________________________________________
c
c		Loop for maximum        
        do j=1,maxrtnw
c
c		New format (*.wrf or *.wde)         
 100      read(in2,*,end=500,err=500) cistat
          if(cistat(1:1).eq.'#') goto 100
          backspace(in2)
          if(iout.eq.1) 
     1      write(nlog,*) '  GetRtnW2; RF or Dep for ID = ', cistat
          

          read(in2,*,end=500,err=500) 
     1        cistat,crtnid,pcttotwX(j),irtndlwX(j)          
          if(iout.eq.1) write(nlog,*) '  GetRtnW2; ',j,  
     1        cistat,crtnid,pcttotwX(j),irtndlwX(j)          
c
c _________________________________________________________

c 		Tie to well structure
          call stafind(nlog,1,6,numdivw,nw,cistat,cdividw,cCallBy) 
          if(nw.eq.0) goto 1400
          
          nrtnX(nw+1)=nrtnX(nw+1)+1
c         write(nlog,*) ' Getrtnw2; nw ', nw, nw+1, nrtnx(nw+1)
          numrtnX=numrtnX+1     
          tempX(j)=nw
c
c _________________________________________________________

c                       Locate return on network
          call stafind(nlog,0,7,numsta,is,crtnid,cstaid,cCallBy)            
          if(is.eq.0) goto 1400
          
          irnstawX(j)=is
c
c _________________________________________________________

c                       Data check for return to end of network
          if(cstadn(is).eq.blank) then
            write(nlog,1272) crtnid,cistat
            write(6,1272) crtnid, cistat
            goto 9999
          endif
c
c _________________________________________________________
c
c                       For a return flow, check if the well returns
c                         to it's location on river  
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
              write(nlog,1285) 
     1          iprintX, cdividw(nw), divnamw1(nw),
     1          'Return Flow ', cistat, crtnid
cr              goto 9999
              endif
              iss=idncod(iss)
            end do
          endif
c
c _________________________________________________________

c                       For a depletion, check if the well returns
c                         upstream of it's location on river  
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
                write(nlog,1285) 
     1            iprintX, cdividw(nw), divnamw1(nw),
     1            'Depletion   ',cgoto, crtnid
cr              goto 9999
              endif
              iss=idncod(iss)
           end do
         endif
c
c _________________________________________________________

c		End Return Read Loop
        end do
c
c		Dimension Exceeded        
        write(nlog,1302) maxrtnW
 500  continue
c _________________________________________________________
c    
c
c 2005/10/14; Store in historic array positions (by well)
c             even if provided in a random order        
      j1=0
      do nw=1,numdivw
        do j=1,numrtnX
          k=tempX(j)
          if(k.eq.nw) then
            j1=j1+1
            pcttotw(j1) = pcttotwX(j)
            irtndlw(j1) = irtndlwX(j)          
            irnstaw(j1) = irnstawX(j)
          endif
        end do
      end do              
c
c _________________________________________________________
c
c		Detailed Check     
      if(iout.ge.1) then
        write(nlog,1410)
        j2=0
        do nw=1,numdivw
          j1=nrtnX(nw+1)
c         write(nlog,*) '  Getrtnw2; nw, j1, ', nw, j1
          do j=1,j1
            j2=j2+1
            write(nlog,1412) j2, nw, nrtnX(nw+1), pcttotw(j2)
          end do
        end do    
      endif  
c     
c _________________________________________________________
c
c		Print summary     
      write(nlog,927) numrtnX
 927  format(/,72('_'),/
     1      '  GetRtnW2; Number of return or depletions read = ', i5)
       
c _________________________________________________________
c
c               Return

      return 
c
c               Error Handling
c _________________________________________________________
c
  928 write(nlog,929) in2, filena
  929 format(/,72('_'),/
     1' GetRtnw2; Problem reading file # ', i4,/,
     1       '   File name: ', a256,/
     1       '   Problem record (next line):')
      goto 9999
      
      
 1400 write(nlog,1402) cistat
 1402 FORMAT(/,72('_'),/
     1 '  GetRtnW2; Problem.'
     1 ' Structure ',a12,' of well station return flow (*.wrf)',
     1 ' or depletion (*.wde) not found in well station file') 
      goto 9999
      

 1260 format(36x,a12,f8.0,i8)
 1262 format(36x,a12,f8.0,a12)
 1272 FORMAT(/,72('_'),/
     1 '  GetRtnw2: Return flow to ',a12,' from Well',a12, /
     1 '           cannot be the most downstream node in a ',/
     1 '           reach. Add another node to the river ', 
     1            'network (*.rin) file')
     
 1281  FORMAT(/,72('_'),/
     1  '  GetDiv; Warning See *.chk for details regarding: ', a32)
     
 1284 format(/,72('_'),/
     1  '  Getrtnw; Warning the following well has a depletion or ',/
     1  '           return flow that is upstream of its location on ',
     1             'the river',//
     1  '    # Well ID      Well Name               ',
     1  ' Type         River ID     Return ID',/
     1  ' ____ ____________ ________________________',
     1  ' ____________ ____________ ____________')
 1285 format(i5, 1x,a12, 1x,a24, 1x,a12, 1x,a12, 1x,a12)
 
 1290 FORMAT(/,72('_'),/ 
     1 '  GetRtnw2;',
     1       ' FYI Well Structues with Loss or Salvage Data ',/
     1       '          ID            Return    Loss Deplete',
     1                            ' Salvage   Total',/
     1       '          ____________ _______ _______ _______',
     1                            ' _______ _______')
     
 1291 format(10x,a12, 2f8.0, 16x,f8.0)
 
 1293 format(10x,a12, 16x, 3f8.0)
 
 1292 FORMAT(/,72('_'),/
     1 '  GetRtnw2; Problem station ',a12,
     1 ' of the Well Station File (*.dds)',/
     1 10x, ' has the following return data:',/
     1 10x, ' Total of all returns = ', f8.2)

 1300 FORMAT(/,72('_'),/
     1 ' GetRtnw2; TOO MANY DIVERSION PROJECTS,   MAXIMUM = ',I5)
     
 1302 FORMAT(/,72('_'),/
     1 ' GetRtnw2; TOO MANY RETURN FLOW STATIONS, MAXIMUM = ',I5) 
 
 1410 format(/,72('_'),/
     1 '  GetRtnW2;',/
     1 '      J2      Nw   Nrtnx pcttotw',/
     1 ' _______ _______ _______ _______')
     
 1412 format(3i8, f8.0)               

 9999 write(6,1440)
      write(nlog,1450) 
      call flush(6)
 1440 format('  Stopped in GetRtnw2, see the log file (*.log)')
 1450 format(/,72('_'),/
     1 '  GetRtnW2; Stopped in GetRtnw2')
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop 
      END
