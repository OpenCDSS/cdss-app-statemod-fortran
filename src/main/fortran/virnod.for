c
c *********************************************************
c
       subroutine virnod 
c
c
c _________________________________________________________
c	Program Description
c
c       Virnod; It estimates the flow at a gauge using the following:
c             flow = (sum of upper gauges) + c * (gain)
c
c _________________________________________________________
c	Dimensions
c
      include 'common.inc'
c
c _________________________________________________________
c
c     write(6,*)    '  Virnod; mon', mon
c     write(io99,*) '  Virnod; mon', mon

c
c rrb 01/03/12; Moved virnod inside vircom month loop for easier daily
c     do 170 mon=1,12
c
c rrb 11/20/95 Additional Output
       ft = mthday(mon)*factor
       iout = 0
c
c               Zero out all non-gaged arrays
       do 90 is=1,numsta
         do 80 np = 1,numrun
           if(irusta(np).eq.is) goto 90
 80      continue
         qhistox(is) = 0.0
 90    continue
         

       do 160 i=1,nbaset
         c = coeff(mon,i)
c
c               Find node station to be estimated
         do 100 is=1,numsta
           if(cstaid(is) .eq. cgagen(i)) then
             iss = is
             qhistox(iss) = 0.0
             goto 110
           endif
 100     continue
c
         write(6,102) cgagen(i)
 102     format( ' Virgen: could not find node to be estimated = ', a12)
         goto 180
c
c               Calculate flow of upper gauges
 110     do 121 m=1,mbase(i)
           do 120 ib=1,numrun
             if(crunid(ib) .eq. cupper(m,i)) then
               ibb = irusta(ib)
               qhistox(iss) = qhistox(iss) + 
     1               cnodm(m,i)*qhistox(ibb)
                goto 121
             endif
 120       continue
c
           write(6,122) cupper(m,i)
 122       format(' Virnog; could not find upper gage ',
     1          'cupper(m,i) = ', a12)

           goto 180
 121     continue

c
c               Gain part of equation
          do 150 n=1,nbase(i)
            do 140 ib=1,numrun
              if(crunid(ib) .eq. cgagex(n,i)) then
                ibb = irusta(ib)
                qhistox(iss) = qhistox(iss) + 
     1                   c * cnode(n,i) * qhistox(ibb)
                goto 150
              endif
 140        continue
            write(6,142) cgagex(n,i)
            write(99,142) cgagex(n,i)
 142        format(
     1        ' Virnod; could not find gain station (cagex) = ',
     1        a12,//,
     1        '         Following are the gage ids compared to',/)
            do 143 ix=1,numrun
              write(99,141) ix, crunid(ix)
 141          format(i5, 2(1x,a12))
 143        continue
            goto 180
 150      continue
c
c               Insure the base flow is positive
          if(qhistox(iss).lt.-0.01) then
c           if(iout.eq.0) write(99,*) ' '
            iout = 1
c           write(99,152) iyr, mon, idy, qhistox(iss)*ft, cstaid(iss),
c    1            (stanam(ix,iss), ix=1,6)
 152        format('  Virnod; Warning for ', 3i5, 
     1             ' The Base Flow of ',f10.0,
     1             ' (af) was set to zero at ',a12,'; ', a24)
c
c rrb 11/22/95 do not allocate negative flows to tribs
            ineg(iss) = ineg(iss) + 1
            if(iday.eq.0) then
              qneg(iss) = qneg(iss) + qhistox(iss)*ft
            else
              qneg(iss) = qneg(iss) + qhistox(iss)*factor
            endif

            qhistox(iss) = amax1(0.0, qhistox(iss))
          endif
c
c rrb 99/06/22; Insure negatives between 0 and -0.01 are set to zero
c               Note by locating here, we only count negatives < -0.01
          qhistox(iss) = amax1(0.0, qhistox(iss))
c
c               End base flow station loop
 160    continue
c
c               End month loop
c
c rrb 01/03/12; Moved virnod inside vircom month loop for easier daily 
c170  continue
      return
 180  write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop 
      end
   


