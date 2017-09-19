c
c
       subroutine ChkAvail(nlog, icx, nchkA, maxsta, 
     1   avail, q, n1, n2, n3, fac)
     
       dimension avail(maxsta)

       iout=1
c      nchkA=nchkA+1
cx     if(nchkA.eq.1) write(nlog,200) icx, nchkA
       if(iout.eq.1) write(nlog,200) icx, nchkA

       write(nlog,210) nchkA, q*fac, n1, avail(n1)*fac, n2, 
     1  avail(n2)*fac, n3, avail(n3)*fac

 200   format(/,
     1 ' ChkAvail; Called by subroutine ', i5,' nchkA = ', i5/
     1 '    #    Divert   N1  Avail(1)   N2  Avail(2)   N3  Avail(3)',/
     1 ' ____ _________ ____ _________ ____ _________ ____ _________')
 210   format(20(i5, f10.2))

       return
       stop
       end
