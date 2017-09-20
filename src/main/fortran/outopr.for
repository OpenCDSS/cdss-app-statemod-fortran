c
C *********************************************************
C
      SUBROUTINE outopr
c
c
c _________________________________________________________
c	Program Description
c
c       Outopr; It prins operational rule information
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
      character ftype*24, ptype*24                           
      dimension divm(13),divt(13)
      data divm/13*0.0/, divt/13*0.0/
c
c _________________________________________________________
c
c
      write(6,101) 'OutOpr  '
      write(nlog,101) 'OutOpr  '
 101  format(/,72('_'),/'  Subroutine ', a8)
      call outtop(46,1,20)
c
c
c _________________________________________________________
c
c               Get requested ID's
      idallx=0
      if(ioutx.eq.1) then
        call getin(3, iid, maxsta, idallx, idtype, ftype, ptype, idreq)
      endif
c
c               Set ID screen switch
      if(idallx.eq.0) then
        nid = numopr
      else          
        nid=iid
      endif
          
      iop=0

      do 130 ip=1,nid
c
c
c _________________________________________________________
c
c               Get requested ID
        if(idallx.eq.0) then
          iop=iop+1
        else
          call getid(3, is, iop, iop2, idreq(ip))
          if(is.ne.-1) goto 130
        endif

        write(46,140) cunitm,
     1    corid(iop), nameo(iop),ityopr(iop),ropnk(iop), 
     1    ciopsoX(1,iop), ciopdeX(1,iop),iobeg(iop),ioend(iop)
        write(46,150) (xmonam(j), j=1,13)

          do im=1,13
            divt(im)=0.0
          end do
c
c rrb 99/06/28; Revise to handle report POR < simulation POR
c         do 120 iy=1,(iyend-iystr+1)
          do 120 iy=iystr, iyend
            divm(13)=0.0
c
c		Test begin at month imstr2          
            imstr2=1
            imb=imstr2            
            do 110 im=imb,12
c
c rrb 06/06/96; Include header information
c               N/A because bintop was not called
c             irec1=((iy-1)*12*numopr)+((im-1)*(numopr))+iop+numtop
c
c rrb 99/06/28; Revise to handle report POR < simulation POR
c             irec1=((iy-1)*12*numopr)+((im-1)*(numopr))+iop
              irec1=((iy-iystr0)*12*numopr)+((im-1)*(numopr))+iop
              read(45,rec=irec1) divm(im)
c
c rrb 01/03/28; Variable units (also adjusted outmon.f)
c             divm(13)=divm(13)+divm(im)
c             divm(13)=divm(13)+divm(im)*fac*mthday(mon)
              divm(im)=divm(im)*fmo(im)
              divm(13)=divm(13)+divm(im)*ftot


              divt(im)=divt(im)+(divm(im)/(iyend-iystr+1))
              divt(13)=divt(13)+(divm(im)/(iyend-iystr+1)*ftot)
  110       continue
c
c rrb 99/06/28; Revise to handle report POR < simulation POR
c           write(46,160) (iystr+iy-1),(divm(im), im=1,13)
            if(isigfig.eq.0) then
              write(46,160)  iy,         (divm(im), im=1,13)
            else
              write(46,162)  iy,         (divm(im), im=1,13)
            endif  
  120     continue
          if(isigfig.eq.0) then
            write(46,170) (divt(im), im=1,13)
          else
            write(46,172) (divt(im), im=1,13)
          endif  
  130   continue
        return
c
c
c _________________________________________________________
c
c               Formats
  140 format (
c    1 /,'_______________________________________________________',
     1 /,' Operational Right Summary ', a5,/
     1 /,' ID = ',a12, '       Name = ',a24, ' Opr Type = ', i4,
     1   '   Admin # = ',f16.5,
     1 /,' Source 1 = ',a12, ' Destination = ', a12,
     1   '      Year On = ',i5, '   Year Off = ',i5)
  150  format('YEAR', 13(4X,a4),/,
     1   '____',13(' _______'))
  160  format(i4,13f8.0)
  162  format(i4,13f8.1)
  170  format('AVG ',13f8.0)
  172  format('AVG ',13f8.1)
c
c _________________________________________________________
c
       end
