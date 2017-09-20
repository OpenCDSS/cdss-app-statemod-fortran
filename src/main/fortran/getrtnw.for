c
c **********************************************************
c
        subroutine getrtnw(
     1    nlog, nchk, iprintx,
     1    maxrtnw,  maxsta,  maxdivw, maxdvrw,
     1    nw,ji,je, iin2,    numsta,
     1    ityp,     iloss,   interv,
     1    pcttotw,  pctlosw, irtndlw, irnstaw,
     1    ndnnod,   idncod,  cgoto,   divnamw1,
     1    cdividw,  cstaid,  cstadn,  filena, cirtndlw)
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
c rrb 00/02/09; Revised checks on return % to mdainp to allow
c               check on (reutn %) * (sum return table)
c rrb 01/10/08; Revise dimension 20000 - 10000
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
     1             divnamw1(maxdivw),  cdividw(maxdivw),
     1             cstaid(maxsta),     cstadn(maxsta),
     1             cirtndlw(maxdvrw)


        character  cdividw*12,   cstaid*12,    cstadn*12, crtnid*12,
c     1             cgoto*12,     blank*12,     filena*256, cirtndlw*12,
     1             cgoto*12,     blank*12,                 cirtndlw*12,
     1             rec32*32
c jhb 2014/06/26 gfortran is more strict about the dummy char arg fortran requirement than lahey is
        character(len=*)  filena
c
c _________________________________________________________
c
c
c       write(nlog,*) ' Getrtnw; iprintX ', iprintX


        iout=0
        
        blank = '            '
        if(maxrtnw.gt.10000) then
          write(nlog, 1302) maxrtnw
          goto 9999
        endif
        
c
c _________________________________________________________
c
        DO J=JI,JE
c
c rrb 99/08/26; Character ID
          if(interv.ne.-999) then
            read(55,1260,end=926,err=928) crtnid,pcttotw(j),irtndlw(j)
            if(iout.eq.1) write(nlog,*) ' GetRtnW;', 
     1       crtnid,pcttotw(j),irtndlw(j)
          else
            read(55,1262,end=926,err=928) crtnid,pcttotw(j),cirtndlw(j)
            if(iout.eq.1) write(nlog,*) ' GetRtnW;', 
     1       crtnid,pcttotw(j),cirtndlw(j)
          endif
c
c rrb 2006/03/20; Adjust character string to left     
          crtnid=adjustl(crtnid)          
c
c _________________________________________________________

c                       Locate return on network
          ifound=0
          DO is=1,NUMSTA -1
            if(cstaid(is).eq.crtnid) ifound=is
          end do
c
c _________________________________________________________
c
c                       Print warning if ret/dep ID is not in network
          if(ifound.eq.0) then
            write(nlog,1270) crtnid, cdividw(nw),divnamw1(nw)
            write(6,1270)    crtnid, cdividw(nw),divnamw1(nw) 
            Goto 9999
          endif
c
          is=ifound
          irnstaw(j)=is
c
c _________________________________________________________
c
c                       Data check for return to end of network
          if(cstadn(is).eq.blank) then
            write(nlog,1272) crtnid,cdividw(nw),divnamw1(nw)
            write(6,1272) crtnid, cdividw(nw),divnamw1(nw)
            goto 9999
          endif
c
c _________________________________________________________
c
c                       For a return flow, check if the well returns
c                         to it's location on river  
c rrb 2006/04/03; Print warning to *.chk
cr        iwarn=0
          iwarn=1
          iss=is
          ndns = ndnnod(iss)
          
          
          if(iwarn.eq.1) then
c
c			Return Flow (type 0)
c                       Check if the well returns 
c                       upstream of its location
            if(ityp.eq.0) then
              iss=is
              do n=1,ndns           
                if(cstaid(iss).eq.cgoto .and. crtnid.ne.cgoto) then
                  if(iprintX.eq.0) then
                    rec32='Well Return Location'                  
                    write(nlog,1281) rec32                  
                    write(nchk,1284)
                  endif                    
                  
                  iprintX=iprintX+1                  
                  write(nchk,1285) 
     1              iprintx,cdividw(nw), divnamw1(nw),
     1              'Return Flow ', cgoto, crtnid
cr                goto 9999
                endif
                iss=idncod(iss)
              end do
            else
c
c _________________________________________________________
c       
c                         b. Depletion (type 0)
c                            Check if the well depletes
c                            upstream of it's location on river  
              iss=is
              do n=1,ndns           
                if(cstaid(iss).eq.cgoto .and. crtnid.ne.cgoto) then
                  if(iprintX.eq.0) then
                    rec32='Well Depletion Location'                  
                    write(nlog,1281) rec32                  
                    write(nchk,1284)                                    
                  endif  
                  
                  iprintX=iprintX+1                    
                  write(nchk,1285) 
     1              iprintX, cdividw(nw), divnamw1(nw),
     1              'Depletion   ',cgoto, crtnid
cr                goto 9999
                endif
                iss=idncod(iss)
        
              end do
            endif
          endif  
c       
c                         End Return Read Loop
        end do
c
c
c _________________________________________________________
c               Check
c rrb 11/15/94 I/O check the total return equals 100%
c rrb 00/02/08; Moved checks to mdainp to evaluate
c               (return %) and (sum return table) 
c
c _________________________________________________________
c
c               Return
      return 
c
c _________________________________________________________
c
c               Error Handling
c
  926 write(nlog,927) iin2, filena
  927 format(' Getrtnw.f; End of file # ', i4, ' encountered',/,
     1       '   File name: ', a256)
      goto 9999
c
  928 write(nlog,929) iin2, filena
  929 format(' Getrtnw.f; Problem reading file # ', i4,/,
     1       '   File name: ', a256,/
     1       '   Problem record (next line):')
      goto 9999

 1260 format(36x,a12,f8.0,i8)
 1262 format(36x,a12,f8.0,a12)
 1270 FORMAT(
     1 '  Getrtnw: Return flow to ',a12,' from Well ',
     1 a12, 1x, a24,/
     1 '           was not found in the river network file (*.rin)')
 1272 FORMAT('  Getrtnw: Return flow to ',a12,' from Well',/
     1       '          ',a12, 1x, a24,/
     1       '           cannot be the most downstream node in a ',/
     1       '           reach. Add another node to the river ', 
     1                  'network (*.rin) file')
 1282 FORMAT(/,
     1       '  Getrtnw; Warning the following wells have a return ',/
     1       '           location that is available to the diverter:',/
     1       '           Well ID:                       ',a12,/, 
     1       '           Well name:                     ',a24,/, 
     1       '           Located at river ID:           ',a12,/,  
     1       '           Has a return flow to river ID: ',a12)
 1281  FORMAT(/,
     1  '  GetRtnW; Warning See *.chk for details regarding: ', a32)
     
 1284 format(/,72('_'),/
     1  '  Getrtnw; Warning the following wells have a depletion or ',/
     1  '           return that is upstream of its location on the ',
     1             'river',//
     1  '    # Well ID      Well Name               ',
     1  ' Type         River ID     Return ID',/
     1  ' ____ ____________ ________________________',
     1  ' ____________ ____________ ____________')
 1285 format(i5, 1x,a12, 1x,a24, 1x,a12, 1x,a12, 1x,a12)

 1290 FORMAT(/, '  GetrtnW;',
     1       ' FYI Well Structues with Loss or Salvage Data ',/
     1       '          ID            Return    Loss Deplete',
     1                            ' Salvage   Total',/
     1       '          ____________ _______ _______ _______',
     1                            ' _______ _______')
 1291 format(10x,a12, 2f8.0, 16x,f8.0)
 1293 format(10x,a12, 16x, 3f8.0)
 1292 FORMAT('  GetrtnW; Problem station ',a12,
     1 ' of the Well Station File (*.dds)',/
     1 10x, ' has the following return data:',/
     1 10x, ' Total of all returns = ', f8.2)

 1300 FORMAT(' Getrtnw; TOO MANY DIVERSION PROJECTS,     MAXIMUM = ',I5)
 1302 FORMAT(' Getrtnw; TOO MANY RETURN FLOW STATIONS,   MAXIMUM = ',I5) 

 9999 write(6,1440)
      write(nlog,1450) 
      call flush(6)
 1440 format('  Stopped in Getrtnw, see the log file (*.log)')
 1450 format('  Stopped in Getrtnw')
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)
c
c _________________________________________________________

      stop 
      END
