c
c **********************************************************
c
        subroutine getRtn(maxrtnw,     maxsta,  maxdivw, maxdvrw,
     1               in1, in2,
     1               nw,ji,je, nlog,    iin2,    numsta,
     1               ityp,     iloss,   interv,
     1               pcttotw,  pctlosw, irtndlw, irnstaw,
     1               numrtnX,  nrtnX,   numdivw,
     1               ndnnod,   idncod,  cgoto,   divnamw1,
     1               cdividw,  cstaid,  cstadn,  filena,
     1               cirtndlw)
c
c _________________________________________________________
c	Program Description
c
c 	Read return flow data for a diversion or reservoir
c _________________________________________________________
c
c       Update History
c        
c rrb 2005/10/15; Copied GetRtnW2 (includes new station format)
c                 Revised to handle Diversions
c
c _________________________________________________________
c       Documentation
c               numdivw	number of diversions
c		numrtnx		number of returns
c               cistat		Structure ID
c
c
c _________________________________________________________
c       Dimensions

        dimension  pcttotw(maxrtnw),   pctlosw(maxrtnw),
     1             irtndlw(maxrtnw),   irnstaw(maxrtnw),
     1             ndnnod(maxsta),     idncod(maxsta),
     1             divnamw1(maxdivw),  cdividw(maxdivw),
     1             cstaid(maxsta),     cstadn(maxsta),
cr   1             cirtndlw(maxdvrw)
     1             cirtndlw(maxsta)

        dimension  pcttotwX(maxrtnw), irtndlwX(maxrtnw),          
     1             irnstawX(maxrtnw), 
     1             nrtnX(maxdivw),    tempX(maxrtnw)


        character  cdividw*12, cstaid*12, cstadn*12,  crtnid*12,
     1             cgoto*12,   blank*12,  filena*256, cirtndlw*12,
     1             cistat*12,  ctype*12,  csufxRF*5,  csufxST*5
c
c _________________________________________________________
c
c
        blank = '            '
        numrtnX=0
c
c		iout = 	0 No details
c			1 Maximum details
c			2 Summary details        
        iout=1
c       write(nlog,*) ' GetRtn; '

        if(ityp.eq.0) then
          ctype='Diversion'
          csufxRF='*.drf'
          csufxST='*.dst'
        endif
        if(ityp.eq.1) then
          ctype='Reservoir'
          csufxRF='*.rrf'
          csufxST='*.res'
        endif  
        
        do nw=1,maxdivw
          nrtnX(nw)=0
        end do  

c
c _________________________________________________________
c
c		Loop for maximum        
        ioutp=0
        do 500 j=1,maxrtnw
c
c		New format (*.wrf or *.wde)         
 100      read(in2,*,end=501,err=501) cistat
          if(cistat(1:1).eq.'#') goto 100
          backspace(in2)
          if(iout.eq.1) write(nlog,*) '  GetRtn; RF or Dep ', cistat
          

          read(in2,*,end=501,err=501) 
     1        cistat,crtnid,pcttotwX(j),irtndlwX(j)          
          if(iout.eq.1) write(nlog,*) '  GetRtn; ',j,  
     1        cistat,crtnid,pcttotwX(j),irtndlwX(j)    
c
c rrb 2006/03/20; Adjust character string to left     
          cistat=adjustl(cistat)           
c
c _________________________________________________________

c 		Tie to Div structure
          call stafind(nlog,1,3,numdivw,nw,cistat,cdividw) 
          if(nw.eq.0) goto 1400
          
          nrtnX(nw+1)=nrtnX(nw+1)+1
c         write(nlog,*) ' GetRtn; nw ', nw, nw+1, nrtnX(nw+1)
          numrtnX=numrtnX+1     
          tempX(j)=nw
c
c _________________________________________________________

c                       Locate return on network
          call stafind(nlog,0,7,numsta,is,crtnid,cstaid)            
          if(is.eq.0) goto 1400
          
          irnstawX(j)=is
c
c _________________________________________________________

c                       Data check for return to end of network
          if(cstadn(is).eq.blank) then
            write(nlog,1272) ctype, crtnid,cistat
            write(6,1272) ctype, crtnid, cistat
            goto 9999
          endif
c
c _________________________________________________________
c
c                       For a return flow, check if the structure
c                         returns to it's location on river  
cr        iss=is
          iss=idncod(is)
          ndns = ndnnod(iss)
          do n=1,ndns           
            if(cstaid(iss).eq.cistat) then
              if(ioutP.eq.0) write(nlog,1280) ctype
              ioutP=ioutP+1
              write(nlog,1282) ioutP, cdividw(nw), 
     1          divnamw1(nw), cistat, crtnid
              goto 9999
            endif
            iss=idncod(iss)
          end do
c
c _________________________________________________________
c
c                       End Return Read Loop
 500    continue
 501  write(nlog,502) ctype, numrtnX
 502  format(/,72('_'),/
     1 '  GetRtn; Total number of ', a12,' Return Flow ',
     1           'Locations = ', i5)
c _________________________________________________________
c    
c
c 2005/10/14; Store in historic array positions (by structure)
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
        write(nlog,1410) ctype
        j2=0
        do nw=1,numdivw
          j1=nrtnX(nw+1)
c         write(nlog,*) '  GetRtn; nw, j1, ', nw, j1
          do j=1,j1
            j2=j2+1
            write(nlog,1412) j2, nw, nrtnX(nw+1), pcttotw(j2),
     1        irnstaw(j2)
            if(irnstaw(j2).eq.0) goto 9999
          end do
        end do    
      endif  
c     
c _________________________________________________________
c
c		Print summary     
      write(nlog,504) ctype, numrtnX
 504  format(/,72('_'),/
     1  '  GetRtn; Total Number of ', a12, 'Return Flows ',
     1            'Locations read = ', i5)
       
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
     1  ' GetRtn; Problem reading file # ', i4,/,
     1  '   File name: ', a256,/
     1  '   Problem record (next line):')
      goto 9999
      
      
 1400 write(nlog,1402) ctype, csufxRF, cistat, ctype, csufxST

 1402 FORMAT(/,72('_'),/
     1  '  GetRtn; Problem the ',a12, ' Return Flow file (',a5,')'/
     1  '          has a Stateion ID = ',a12,' that cannot be',/
     1  '          found in the ', a12, ' Station File (',a5,')') 
      goto 9999
      

 1260 format(36x,a12,f8.0,i8)

 1262 format(36x,a12,f8.0,a12)

 1272 FORMAT(/,72('_'),/
     1  '  GetRtn:  Problem the ', a12, ' station return flow to ',a12,
     1  '           from structure ',a12, ' cannot be the most down',/
     1  '           stream node in the network ',/
     1  '           Reconmend you add another node to the river ', 
     1             'network (*.rin) file')

 1280 FORMAT(/,72('_'),/
     1  '  GetRtn;  Warning the ',a12, ', station file has a return ',/
     1  '           location that is available to the diverter:',/
     1  '    # Str_ID       Str_Name                 River_ID     ',
     1  ' Return_ID',/
     1  ' ____ ____________ ________________________ _____________',
     1  ' ___________')
     
 1282 format(i5, 1x, a12, 1x, a24, 1x, a12, 1x, a12)    
    
 1410 format(/,72('_'),/
     1  '  GetRtn; ', a12,' Return Flow Data Summary (iout=2)',/
     1  '       #      Nx   NrtnX  pcttot  irnsta',/
     1  ' _______ _______ _______ _______ _______')
 1412 format(3i8, f8.0, i8)               

 9999 write(6,1440)
      write(nlog,1450) 
      call flush(6)
 1440 format('  Stopped in GetRtn, see the log file (*.log)')
 1450 format('  Stopped in GetRtn')
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop 
      END
