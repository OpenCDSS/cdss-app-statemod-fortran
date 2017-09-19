c
c *********************************************************
c
        subroutine outriv
c
c
c _________________________________________________________
c	Program Description
c
c       Outriv.for; it prints a summary table of the river
c               network file and activity at each river node
c
c _________________________________________________________
c       Documentation
c               crunid = river id of upstream node in network
c               runnam = river id name
c               crunid = base flow id
c               crigid = historic river id
c
c _________________________________________________________
c	Dimensions
        include 'common.inc'
        character cistat*12, did*12, iid*12, rid*12,
     1            nodata*12, recin*256, sname1*24
c
c _________________________________________________________
c		Step 1; Initize
c
        nodata = '0           '

        call outtop(33,1,11)
        write(33,230)
c
c               Read 1 year of historic stream file to see what 
c               was provided
        iin2=3
        filena='*.rih'
        numhis = 1
        read(3,'(i4, 1x, a12)',end=926,err=928) iryr1,cistat
        crigid(numhis) = cistat

  100   read(3,'(i4, 1x, a12)',end=926,err=928) iryr,cistat
        if (iryr.ne.iryr1) goto 110
        numhis = numhis+1
        crigid(numhis) = cistat
        goto 100
c
c               Loop for every node in the network file
c               Identify who is in the base flow and who
c               is in the historic file
  110   do 220 is=1,numsta
          idum(is) = 0
          idum2(is) = 0                                           
          did=nodata
          iid=nodata
          rid=nodata
          inum=0
          sname1 = ""
c
c                See if it's a base flow point (from *.ris)
          do 130 j=1, numrun
            if(cstaid(is).eq.crunid(j)) then
              idum(is) = 1
              inum = inum+1
            endif
  130     continue
c
c                See if it's a historic gage (from *.rih)
          do 140 j=1, numhis
            if(cstaid(is).eq.crigid(j)) then
              idum2(is) = 1
              inum = inum+1
            endif
  140     continue
c
c                See if it's a diversion point (from *.ris)
          do 160 nd=1, numdiv
            if(idvsta(nd).eq.is) then
              did = cdivid(nd)
              inum = inum+1
              sname1 = divnam1(nd)
            endif
  160     continue
c
c                See if it's an instream point (from *.ifs)
          do 180 nf=1, numifr
            if(ifrsta(nf).eq.is) then
              inum = inum+1
              iid = cifrid(nf)
              sname1 = xfrnam1(nf)
            endif
  180     continue
c
c                See if it's a reservoir point (from *.res)
          do 200 nr=1, numres
            if(irssta(nr).eq.is) then
              inum = inum+1
              rid = cresid(nr)
              sname1 = resnam1(nr)
            endif
  200     continue
c
c               Print results
          write(33,210) cstaid(is),stanam1(is),cstadn(is),
     1     idum(is), idum2(is), did, iid, rid, inum, sname1, 
     1     is
  210     format(a12,a24,a12,2i8,3(1x,a12), 1x,i8, 1x, a24, 1x, i5)
c
c               Store for return flow output
         cretid(is) = nodata
         if(did.ne.nodata) cretid(is) = did
         if(iid.ne.nodata) cretid(is) = iid
         if(rid.ne.nodata) cretid(is) = rid

         usenam1(is) = sname1

  220   continue
c
c               Print return flow data
c               Assumes one user per diversion
        write(33,232)
        j = 0
        do 224 nd=1,numdiv
          is =idvsta(nd)         
          write(33,234) cdivid(nd), divnam1(nd), 
     1                  cstaid(is), stanam1(is)
          n1 = nrtn(nd)
          n2 = nrtn(nd+1)-1
            do 222 n=n1,n2
              j=j+1                                                   
              isr = irnsta(j)
c             write(33,*) 'nd, j, isr, n1, n2'
c             write(33,*)  nd, j, isr, n1, n2
              write(33,236)  pcttot(j), 
     1                       cstaid(isr),  stanam1(isr),
     1                       cretid(isr),  usenam1(isr)
 222        continue
 224   continue


        return

c
c _________________________________________________________
c
c
c                      Formats
  230   format(
     1   '#',/
     1   '# *******************************************************',/
     1   '#',/
     1   '#     Card 1   Control',/
     1   '#     format:  (a12, a24, a12, 2i8)',/                   
     1   '#',/
     1   '#     ID              River Station ID',/
     1   '#     Name:           River Station name',/
     1   '#     To:             Downstream river station ID',/
     1   '#     Base:           River Baseflow data 0=no, 1=yes',/
     1   '#     Hist:           River Historic data 0=no, 1=yes',/
     1   '#     Diversion ID:   Diversion location 0=no, ID=yes',/
     1   '#     InStream ID:    Instream flow location 0=no, ID=yes',/
     1   '#     Reservoir ID:   Reservoir location 0=no, ID=yes',/
     1   '#     Number:         Number of activities at this node',/
     1   '#     Structure Name: Diversion, Instream or Reservoir Name',/
     1   '#',/,'#',/,                                                      
     1   '# ID        Name                    To          ',
     1      '    Base    Hist',
     1      ' Diversion ID InStream ID  Reservoir ID ',
     1      '  Number Structure Name',/
     1   '#----------eb----------------------eb----------e',
     1      'b------eb------e',3(' b----------e'),
     1      ' b------e b----------------------e',/
     1   '#')
c 232   format('',//, 
  232   format(    //, 
     1   ' Return Flow Information (assuming one user per ditch)',//,
     1   'Diver ID    Div Name                ',
     1   'Fr River ID Fr River Name            Percent ',
     1   'To River ID To River Name           ',
     1   'To Str ID   To Str Name',/
     1   '----------- ----------------------- ',
     1   '----------- ----------------------- -------- ',
     1   '----------- ----------------------- ',
     1   '----------- ----------------------- ')
  234   format(a12, a24, a12, a24, f8.2, 1x, a12, a24, a12, a24)
  236   format(72x,                f8.2, 1x, a12, a24, a12, a24)

c
c _________________________________________________________
c
c
c       Error Handling
c _____________________________________________________________
  926 write(io99,927) iin2, filena
  927 format(' Outriv.f; End of file # ', i4, ' encountered',/,
     1       '   File name: ', a256)
      goto 9999
c
  928 write(io99,929) iin2, filena
  929 format(' Outriv.f; Problem reading file # ', i4,/,
     1       '   File name: ', a256,/
     1       '   Problem record (next line):')

      backspace(iin2)
      read(iin2, '(a256)',end=926,err=926) recin
      write(io99,'(a256)') recin
      goto 9999

 9999 write(6,*) '  Stopped in Outriv, see the log file (*.log)'
      write(io99,*)'  Stopped in Outriv'
      write(6,*) 'Stop 1' 
      call flush(6)
      call exit(1)

c
c _________________________________________________________
c
      stop 
      END      

