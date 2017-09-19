c
c      ChkAvail2; It prints the entire avail array
c
       subroutine ChkAvail2(nlog, ifirst, icx, nchkA, maxsta, numsta, 
     1   fac, avail)
c _________________________________________________________
c	Update History
c		NA
c
c _________________________________________________________
c
c	Documentaion
c		nlog    output file #
c   ifirst  counter for printing header
c   icx    	calling routine
c   nchkA   calling location in routine ics
c		maxsta	dimension for maximum number of stations
c   numsta  number of stations
c
c		avail   available flow array 
c   fac     factor to convert to af/mo
c
c _______________________________________________________     
     
       dimension avail(maxsta)

       ifirst=ifirst+1
       if(ifirst.eq.1) write(nlog,200) icx, nchkA, (n, n=1,10)

       write(nlog,210) ifirst, icx, nchkA, (avail(n)*fac, n=1,numsta) 
     
 200   format(/,
     1 ' ChkAvail2; Called by subroutine ', i5,' nchkA = ', i5,/,
     1 '    #  icx     nchkA', 10('  Avail-',i2),/
     1 ' ____  ___ _________', 10(' _________'))
 210   format(2i5, i10, 10f10.2(/,10x10f8.2))

       return
       stop
       end
