c
C-------------------------------------------------------------------
C
      SUBROUTINE outifr
c
c _________________________________________________________
c	Program Description
c
c       Outifr; It prints instrea flow reach information
c
c _________________________________________________________
c	Documentaion
c               ndiv    number of columns of data in *.b43
c               it      total supply from *.b43
c
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
      character ftype*24, ptype*24
c                           
      dimension divm(13),divt(13)
c
c _________________________________________________________
c		Step 1; Initilize
c
      write(6,101) 'Outifr'
 101  format(/,72('_'),/' Subroutine ', a8)

      ichk1=0
c
c rrb 00/06/22; Correction output file size incresed (it=total supply)
c     it=8   
c     ndiv = 23
      it=13
c     ndiv = 28
      
c
c		Set number of output values 1x in StateM.for
c	        and store in common.inc      
      ndiv = ndivO        
      nres = nresO
      ndivw= nwelO
      
      call outtop(48,1,27)
c
c               Get requested ID's
      idallx=0
      if(ioutx.eq.1) then
        call getin(4, iid, maxsta, idallx, idtype, ftype, ptype, idreq)
      endif
c
c rrb 04/18/97; Return if no instream flow stations
      if(numifr.le.0) goto 172
c
c               Set ID screen switch
      n=ndnifb(numifr)+ndnifs(numifr)-1
      if(idallx.eq.0) then
        nid=numifr
      else             
        nid=iid
      endif

      iop=0
      do 170 ip=1,nid
c
c               Get requested ID
        if(idallx.eq.0) then
          iop=iop+1
          is=ifrsta(iop)
        else
          call getid(-2, is, iop, iop2,idreq(ip))
          if(ichk1.eq.1) write(99,*) '  Outifr; ',is, iop, idreq(ip)
          if(is.eq.0) goto 170
        endif
c
c
c               Print average from file *.xdd
c -------------------------------------------------------------------
        if(ichk1.eq.1) write(99,*) '  Outifr; iop, is', iop, is
        write(48,190) cunitm,
     1    xfrnam1(iop),cifrid(iop),ifrrsw(iop),
     1    cstaid(is), stanam1(is)
        write(48,200) (xmonam(j), j=1,13)
        do im=1,13
          divt(im)=0.0
        end do

        do 120 iy=1,(iyend-iystr+1)
          divm(13) = 0.0
          do 110 im=1,12
c
            irecs=((iy-1)*12+(im-1))*numsta+is +numtop
            
            
            
            
            if(ichk1.eq.1) write(99,*)' irecs = ', irecs

            read(43,rec=irecs,ERR=230) (dat1(i),i=1,ndiv)
            if(ichk1.eq.1) write(99,'(i5, 30f8.0)') 
     1                  irecs, (dat1(i), i=1,ndiv)
c
c               Note: it = total supply
c             divm(im)=dat1(it)*fac*mthday(im)
              divm(im)=dat1(it)*fmo(im)

              divm(13)=divm(13)+divm(im)*ftot
              divt(im)=divt(im)+(divm(im)/(iyend-iystr+1))
              divt(13)=divt(13)+(divm(im)/(iyend-iystr+1)*ftot)
c
  110       continue
            if(isigfig.eq.0) then  
              write(48,210) (iystr+iy-1),(divm(im), im=1,13)
            else
              write(48,212) (iystr+iy-1),(divm(im), im=1,13)
            endif
  120     continue
        write(48,220) (divt(im), im=1,13)
c
c               Print data by reach
c               Include reservoir releases into reach 1 data
c -------------------------------------------------------------------

        i1=0
        ib=ndnifb(iop) 
        ie=ndnifb(iop) + ndnifs(iop) - 1

        do 160 i=ib,ie
          i1=i1+1
          write(48,180) cunitm,
     1      xfrnam1(iop),cifrid(iop),ifrrsw(iop),
     1      i1, ndnifs(iop), cstaid(is), 
     1      stanam1(is)
          write(48,200) (xmonam(j), j=1,13)

          do im=1,13
            divt(im)=0.0
          end do

          do 150 iy=1,(iyend-iystr+1)
            divm(13)=0.0
            do 140 im=1,12
c
c               Note numtop is not included because bintop was not used
cr            irec1=((iy-1)*12*n)+((im-1)*(n))+i
cr            irec1=((iy-1)*12+(im-1))*n+i
cr            IREC1=(IYR-IYSTR)*12*NUMrea+(im-1)*NUMrea + i

              IREC1=(IY-1)*12*NUMrea+(im-1)*NUMrea + i      
              
              read(47,rec=irec1,err=240) divm(im)
c             divm(im)=divm(im)*factor*mthday(im)
              divm(im)=divm(im)*fmo(im)
c
              divm(13)=divm(13)+divm(im)*ftot
              divt(im)=divt(im)+(divm(im)/(iyend-iystr+1))
              divt(13)=divt(13)+(divm(im)/(iyend-iystr+1)*ftot)
  140       continue
            if(isigfig.eq.0) then
              write(48,210) (iystr+iy-1),(divm(im), im=1,13)
            else
              write(48,212) (iystr+iy-1),(divm(im), im=1,13)
            endif
c           write(nlog,*) '  Outifr;',
c    1        i, (iystr+iy-1), (divm(im), im=1,13)
  150     continue
          write(48,220) (divt(im), im=1,13)
cr        write(nlog,*) ' '
          is=idncod(is)
  160   continue
  170 continue
  172 return
c
c               Formats
  180 format (
c    1 /,'_______________________________________________________',
     1 /,'Instream Reach Summary ', a5,/
     1 /,'Name _ ',a24,' ID _ ',a12, ' On(1)/Off(0) _ ',i3,
     1 /,'Subreach Data ', i3, '/', i3,' At ', a12, 1x, a24) 
  190 format (
     1 /,'Instream Reach Summary ', a5,/
     1 /,'Name _ ',a24,' ID _ ',a12, ' On(1)/Off(0) _ ',i3,
     1 /,'Minimum Data  ', 7x,        ' At ', a12, 1x, a24) 
  200    format(/,'YEAR', 13(4X,a4),/,
     1   '____',13(' _______'))
  210       format(i4,13f8.0)
  212       format(i4,13f8.0)
  220     format('AVG ',13f8.0)
c
c               Error messages
c -------------------------------------------------------------------

  230 write(99,*)  '  Outifr; Problem with binary diversion data *.b43'
      goto 250
  240 write(99,*)  '  Outifr; Problem with binary instream data *.b47'
      goto 250

  250 write(6,*)  '  Stopped in Outifr; see the log file (*.log)'
      write(99,*) '  Stopped in Outifr'                        
      write(6,*) 'Stop 1'      
      call flush(6)
      call exit(1)

      stop 
      END
