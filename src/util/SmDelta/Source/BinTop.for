c                            
c ************************************************************
        subroutine bintop(nf)
c
c               Read the top of the binary files
        include 'SmDelta.inc'

c       ichk=1
        ichk=0
c
c rrb 12/12/94; Get date stamp from direct diversion file and check
        read(nf,rec=1,err=160) iystr0, iyend0
        read(nf,rec=2,err=160) numsta, numdiv, numifr, numres, 
     1                         numown, nrsact, numrun,         
     1                         numdivw

        read(nf,rec=3,err=160) (xmonam(j), j=1,14)
        read(nf,rec=4,err=160) (mthday(j), j=1,12)
        
        if(ichk.ne.0) then
          write(nlog,*) iystr0, iyend0
          write(nlog,*) numsta, numdiv, numifr, numres, 
     1                numown, nrsact, numrun, numdivw
          write(nlog,*) (xmonam(j), j=1,14)
          write(nlog,*) (mthday(j), j=1,12)
        endif
c
c               Include one additional for numown
c
c rrb 99/12/03
c       numtop=numsta+numdiv+numifr+numres+numrun+5
        numtop=numsta+numdiv+numifr+numres+numrun+numdivw+5     

        j1=4
        do 100 j=1,numsta
          j1=j1+1
          read(nf,rec=j1,err=160) j2, cstaid(j),stanam(j)
          if(ichk.ne.0) write(nlog,110) j,j1,j2, cstaid(j),stanam(j)
  100   continue
  110   format(3i5, 1x, a12, 1x, a24, 1x, 20i5)
   
        do 120 j=1,numdiv
          j1=j1+1
          read(nf,rec=j1,err=160)
     1      j2, cdivid(j),divnam(j),idvsta(j) 
          if(ichk.ne.0) 
     1      write(nlog,110)j,j1,j2, cdivid(j), divnam(j), idvsta(j)
  120   continue

        do 130 j=1,numifr
          j1=j1+1
          read(nf,rec=j1,err=160)
     1      j2, cifrid(j),xfrnam(j),ifrsta(j) 
          if(ichk.ne.0)
     1      write(nlog,110)j,j1, j2, cifrid(j),xfrnam(j),ifrsta(j) 
  130   continue
c
c               Note +1 to account for variable nowner
        do 140 j=1,numres+1
          j1=j1+1
          read(nf,rec=j1,err=160)
     1      j2, cresid(j),resnam(j),irssta(j),iressw(j), nowner(j) 
          if(ichk.ne.0)
     1      write(nlog,110) 
     1      j, j1,j2, cresid(j),resnam(j),irssta(j),iressw(j),nowner(j)
  140   continue        

        do 150 j=1,numrun
          j1=j1+1
          read(nf,rec=j1,err=160) 
     1      j2, crunid(j),runnam(j),irusta(j) 
          if(ichk.ne.0) 
     1      write(nlog,110) j, j1,j2, crunid(j),runnam(j),irusta(j) 
  150  continue 
c
c rrb 99/12/03
        do j=1,numdivw
          j1=j1+1
          read(nf,rec=j1,err=160)
     1      j2, cdividw(j),divnamw(j),idvstaw(j) 
          if(ichk.ne.0) 
     1      write(nlog,110)j,j1,j2, cdividw(j), divnamw(j), idvstaw(j)
        end do

       return
c
c               Error Processing
  160  numsta=0
       numdiv=0
       numifr=0
       numres=0
       numown=0
       nrsact=0
       numrun=0
       numdivw=0

       write(nlog,*) '  Bintop; Blank or problem reading binary file'
       write(6,*)  '  Bintop; Blank or problem reading binary file'
       write(6,*)  '  Bintop; Warning see log file (*.log)'
c
c rrb 02/07/22; Stop if problems occur while reading a file
       write(6,*) 'Stop 1'
       call flush (6)

cx     return

       stop 
       end
