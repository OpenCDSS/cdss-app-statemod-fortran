C     Last change:  C    19 May 97   11:52 pm
c                            
c ************************************************************
        subroutine bintop(nf,in, nout, nout2)
c
c
c _________________________________________________________
c	Program Description
c
c       Bintop it writes or reads the top of a StateMod
c         binary data file
c
c
c _________________________________________________________
c	Update History
c		NA
c _________________________________________________________
c	Documentation
c
c               nf = file # to read or write
c               in = 0 write
c                  = 1 read
c               nout= file # to print detailed results when
c                     in=1 and iout>0
c		  nout2=dummy, not currently used
c
c _________________________________________________________
c	Dimensions
c
        include 'common.inc'
c        
        dimension CunitX(40)
        character CodeName*8, CunitX*4
        CodeName='StateMod'
        
        iout=0
        
        if(maxparm.ne.40) then
          write(nlog,250) maxparm
          goto 900
        endif

        ndiv = ndivO        
        nres = nresO
        ndivw= nwelO
        nout2= nout2

c
c               Print top of binary file
c --------------------------------------------------------------
        if(in.eq.0) then
          do j=1,maxparm
            cunitX(j) = '  NA'
          end do  
c
c rrb 2005/12/02; Version etc
          j1=1
          write(nf,rec=j1) CodeName, ver, vdate
          j1=j1+1
          write(nf,rec=j1) iystr0, iyend0
c
c rrb 2005/12/02; Version etc          
c         write(nf,rec=2) 
c    1     numsta, numdiv, numifr, numres, numown, nrsact, numrun,
c    1     numdivw, numdxw
          j1=j1+1
          write(nf,rec=j1) 
     1     numsta, numdiv, numifr, numres, numown, nrsact, numrun,
     1     numdivw, numdxw, maxparm,ndivO, nresO, nwelO
     
          j1=j1+1
          write(nf,rec=j1) (xmonam(j), j=1,14)
          j1=j1+1
          write(nf,rec=j1) (mthday(j), j=1,12)

          do j=1,numsta
            j1=j1+1
            write(nf,rec=j1) j, cstaid(j),stanam1(j)
          end do

          do j=1,numdiv
            j1=j1+1
            write(nf,rec=j1) j,cdivid(j),divnam1(j),idvsta(j)
          end do

          do j=1,numifr
            j1=j1+1
            write(nf,rec=j1) j,cifrid(j),xfrnam1(j),ifrsta(j)
          end do

          do j=1,numres+1
            j1=j1+1
            write(nf,rec=j1) j,cresid(j),resnam1(j),
     1        irssta(j), iressw(j), nowner(j)
          end do

          do j=1,numrun
            j1=j1+1
            write(nf,rec=j1) j,crunid(j),runnam1(j),irusta(j)
          end do
c
c rrb 99/03/25; Add Wells
          do j=1,numdivw
           j1=j1+1
           write(nf,rec=j1) j,cdividw(j),divnamw1(j),idvstaw(j) 
          end do
c
c rrb 2005/12/02; Add Diversion Parameter Types
          do j=1,maxparm
           j1=j1+1
           write(nf,rec=j1) j,paramD(j)
          end do
c
c rrb 2005/12/02; Add Reservoir Parameter Types          
          do j=1,maxparm
           j1=j1+1
           write(nf,rec=j1) j,paramR(j)
          end do
c
c rrb 2005/12/02; Add Well Parameter Types          
          do j=1,maxparm
           j1=j1+1
           write(nf,rec=j1) j,paramW(j)
          end do
c
c rrb 2005/12/02; Diversion Units          
          if(nf.eq.43 .or. nf.eq.49) then          
            nx=ndiv
            do j=1,ndiv-3
              cunitX(j)=' CFS'
            end do
          endif    
c
c		Reservoir Units          
          if(nf.eq.44 .or. nf.eq.50) then
            nx=nres
            do j=1,nres-2
              cunitX(j)=' CFS'
            end do
          endif    
c
c		Well Units          
          if(nf.eq.42 .or. nf.eq.65) then
            nx=ndivw
            do j=1,ndivw
              cunitX(j)=' CFS'
            end do
          endif    
c
c       structure Units
          if(nf.eq.67) then
            nx=33
            do j=1,33
              cunitX(j)=' CFS'
            end do
          endif
          
          j1=j1+1
c
c rrb 2014/12/14; Remove print statement
cx        write(nlog,*)'nf',nf,'nx',nx
          write(nf,rec=j1) (cunitX(j), j=1,nx)

c
c               Include one additional for numown (see reservoirs)
c         numtop=numsta+numdiv+numifr+numres+numrun+5
c
c		Include one for version and 3*maxparm for parameters
c rrb 2005/12/19; Include units
c         numtop=numsta+numdiv+numifr+numres+numrun+numdivw+5  
c         numtop=numsta+numdiv+numifr+numres+numrun+numdivw+5+
c    1           1 + maxparm*3  
          numtop=numsta+numdiv+numifr+numres+numrun+numdivw+5+
     1           2 + maxparm*3  
c
c --------------------------------------------------------------

c		Detailed output     
          if(iout.eq.1) then
            write(nlog,*) ' BinTop; ', CodeName, ver, vdate
            write(nlog,*) ' BinTop; ', iystr0, iyend0
            write(nlog,*) ' BinTop; ', 
     1        numsta, numdiv, numifr, numres, numown, nrsact, numrun,
     1        numdivw, numdxw, maxparm,ndivO, nresO, nwelO
     
     
            write(nlog,*) '  Bintop; ', (xmonam(j), j=1,14)
            write(nlog,*) '  Bintop; ', (mthday(j), j=1,12)     
          endif 
          if(numtop.ne.j1) goto 900
          goto 500
        endif
c        
c _________________________________________________________
c               Read top of binary file
        if(in.eq.1) then
c
c rrb 2005/12/02; Version etc
          j1=1
          read(nf,rec=j1,err=900) CodeName, ver, vdate        
          if(iout.eq.1) write(nout,160) j1, 1, CodeName, ver, vdate
c
c rrb 12/12/94; Get date stamp from direct diversion file and check
          j1=j1+1
          read(nf,rec=j1,err=900) iystr0, iyend0
          if(iout.eq.1) write(nout,162) j1, 1, iystr0, iyend0
          
c
c rrb 99/03/25; Add wells
c         read(nf,rec=2,err=900) numsta, numdiv, numifr, numres, 
c    1                           numown, nrsact, numrun
c
c rrb 2005/12/02; Add parameter types		
          j1=j1+1
          read(nf,rec=j1,err=900) 
     1      numsta, numdiv, numifr, numres, 
     1      numown, nrsact, numrun, numdivw,
     1      numdxw, maxparm           
     
          if(iout.eq.1) write(nout,164) j1, 1, 
     1      numsta, numdiv, numifr, numres, 
     1      numown, nrsact, numrun, numdivw,
     1      numdxw, maxparm     
     
          j1=j1+1
          read(nf,rec=j1,err=900) (xmonam(j), j=1,14)
          if(iout.eq.1) write(nout,166) j1, 1, (xmonam(j), j=1,14)
          
          
          j1=j1+1
          read(nf,rec=j1,err=900) (mthday(j), j=1,12)
          if(iout.eq.1) write(nout,168) j1, 1, (mthday(j), j=1,12)
        
          if(iout.eq.1) then
            write(nlog,205) 
     1        '# codename, ver, vdate  ',codename,ver,vdate
     
            write(nlog,210) '# iystr0 =  ', iystr0
            write(nlog,210) '# iyend0 =  ', iyend0
            write(nlog,210) '# numsta =  ', numsta
            write(nlog,210) '# numdiv =  ', numdiv
            write(nlog,210) '# numifr =  ', numifr
            write(nlog,210) '# numres =  ', numres
            write(nlog,210) '# numown =  ', numown
            write(nlog,210) '# nrsact =  ', nrsact
            write(nlog,210) '# numrun =  ', numrun
            write(nlog,210) '# numdivw=  ', numdivw
            write(nlog,220) '# xmonam =  ', (xmonam(j), j=1,14)
            write(nlog,230) '# mthday =  ', (mthday(j), j=1,12)
            write(nlog,210) '# mamparm=  ', maxparm
            write(nlog,210) '# ndivO  =  ', ndivO
            write(nlog,210) '# nresO  =  ', nresO
            write(nlog,210) '# nwelO  =  ', nwelO
 205        format(a24, 1x, a8, 1x, a8, 1x, a8)             
 210        format(a12, i5)
 220        format(a12, 20a5)
 230        format(a12, 20i5)
          endif
c
c _________________________________________________________
c

          if(iout.eq.1) write(nout,240) 'River Nodes'
        
          do j=1,numsta
            j1=j1+1
            read(nf,rec=j1,err=900) j2, cstaid(j),stanam1(j)
            if(iout.eq.1) 
     1        write(nout,170) j1,j, cstaid(j),stanam1(j)
          end do
c
c _________________________________________________________
c
c               Diversions

          if(iout.eq.1) write(nout,240) 'Diversions'
          do j=1,numdiv
            j1=j1+1
            read(nf,rec=j1,err=900)
     1        j2, cdivid(j),divnam1(j),idvsta(j) 
            if(iout.eq.1) 
     1        write(nout,170)j1,j,cdivid(j),
     1          divnam1(j),idvsta(j)
          end do
c
c _________________________________________________________
c
c               Instream Flow Stations

          if(iout.eq.1) write(nout,240) 'Instream Flows'

          do j=1,numifr
            j1=j1+1
            read(nf,rec=j1,err=900)
     1        j2, cifrid(j),xfrnam1(j),ifrsta(j) 
            if(iout.eq.1)
     1        write(nout,170)j1,j,cifrid(j),
     1          xfrnam1(j),ifrsta(j)
          end do
c
c _________________________________________________________
c
c               Reservoirs

          if(iout.eq.1) write(nout,242) 'Reservoirs'

c
c               Note +1 to account for variable nowner
          do j=1,numres+1
            j1=j1+1
            read(nf,rec=j1,err=900)
     1        j2, cresid(j),resnam1(j),
     1        irssta(j),iressw(j), nowner(j) 
            if(iout.eq.1)
     1        write(nout,170) 
     1        j1,j,cresid(j),resnam1(j),
     1        irssta(j),iressw(j),nowner(j)
          end do
c
c _________________________________________________________
c
c               Base Flows

          if(iout.eq.1) write(nout,240) 'Base Flows'

          do j=1,numrun
            j1=j1+1
            read(nf,rec=j1,err=900) 
     1        j2, crunid(j),runnam1(j),irusta(j) 
            if(iout.eq.1) 
     1        write(nout,170) j1, j, crunid(j),
     1        runnam1(j),irusta(j)
          end do
c
c _________________________________________________________
c
c               Wells

          if(iout.eq.1) write(nout,240) 'Wells'
c
c rrb 99/03/25; Add Wells
          do j=1,numdivw
            j1=j1+1
            read(nf,rec=j1,err=900)
     1        j2, cdividw(j),divnamw1(j),idvstaw(j) 
            if(iout.eq.1) 
     1        write(nout,170)j1,j,cdividw(j),
     1          divnamw1(j),idvstaw(j)
          end do
          
c
c _________________________________________________________
c
c               Diversion Types
          if(iout.eq.1) write(nout,241) 'Div Types'
          do j=1,maxparm
            j1=j1+1
            read(nf,rec=j1,err=900)
     1        j2, paramD(j)
            if(iout.eq.1) 
     1        write(nout,190)j1,j,paramD(j)
          end do
c
c _________________________________________________________
c
c               Reservoir Types
          if(iout.eq.1) write(nout,241) 'Res Types'
          do j=1,maxparm
            j1=j1+1
            read(nf,rec=j1,err=900)
     1        j2, paramR(j)
            if(iout.eq.1) 
     1        write(nout,190)j1,j,paramR(j)
          end do
c
c _________________________________________________________
c
c               Well Types
          if(iout.eq.1) write(nout,241) 'Wel Types'
          do j=1,maxparm
            j1=j1+1
            read(nf,rec=j1,err=900)
     1        j2, paramW(j)
            if(iout.eq.1) 
     1        write(nout,190)j1,j,paramW(j)
          end do
c _________________________________________________________
c
c               Units
c
c rrb 2005/12/02; Diversion Units          
          if(nf.eq.43 .or. nf.eq.49) nx=ndiv
          if(nf.eq.44 .or. nf.eq.50) nx=nres
          if(nf.eq.42 .or. nf.eq.65) nx=ndivw
          
          j1=j1+1
          read(nf,rec=j1,err=900) (cunitX(j), j=1,nx)          
          if(iout.eq.1) write(nout,180) j1, 1, (cunitX(j), j=1,nx)
          
          
c
c _________________________________________________________
c
c         numtop=numsta+numdiv+numifr+numres+numrun+5
c
c rrb 2005/12/02; Add 1 for title and 3 for parameters
c         numtop=numsta+numdiv+numifr+numres+numrun+numdivw+5
c rrb 2005/12/19; Include units
c         numtop=numsta+numdiv+numifr+numres+numrun+numdivw+5+
c    1           1 + maxparm*3        
          numtop=numsta+numdiv+numifr+numres+numrun+numdivw+5+
     1           2 + maxparm*3        
          if(numtop.ne.j1) goto 900
          goto 500
        endif

  500   return
c
c _________________________________________________________
c
c               Formats
  160     format('#', 2i5, 1x, a8, 1x, a8, a10)
  162     format('#', 20i5)
  164     format('#', 20i5)  
  166     format('#', 2i5, 100(1x,a4))  
  168     format('#', 20i5)  
  
  170     format('#', 2i5, 1x, a12, 1x, a24, 1x, 20i5)  
  180     format('#', 2i5,40(a4))
  190     format('#', i5, i5, 1x, a24)
  240     format(
     1    '#',/
     1    '#',a16,/
     1    '# rec# Typ# ID           Name                     ',
     1    ' Riv#',/
     1    '#b___eb___exb__________exb______________________ex',
     1    'b___e')
  241     format(
     1    '#',/
     1    '#',a16,/
     1    '#    # rec# Name                     ',/
     1    '#b___eb___exb_______________________e')
     
  242     format(
     1    '#',/
     1    '#',a16,/
     1    '#    # rec# ID           Name                     ',
     1    ' Riv# OnOff Own#',/
     1    '#b___eb___exb__________exb______________________ex',
     1    'b___eb___eb___e')
     
  250     format(' Bintop; Problem with dimension = ', i5)
c
c _________________________________________________________
c
c               Error Processing 
  900  write(nlog,*) ' Bintop; Problem reading binary file'
       write(nlog,*) ' Bintop; numtop, j1 ', numtop, j1
       write(6,*)  '  Bintop; Problem see log file (*.log)'
       write(6,*) 'Stop 1'
       call flush (6)
       call exit(1)
       
       stop 
       end
