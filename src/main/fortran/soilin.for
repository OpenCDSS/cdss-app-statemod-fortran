
c
c _________________________________________________________
c	Update History
c

c
c _________________________________________________________
c	Documentation
c

c
c _________________________________________________________
c	Dimensions
cc
c *********************************************************************
c
         subroutine SoilIn(nlog, itypSM, awcr, soils, Area, AreaT, 
     1     cumax, cuact, qdivs, fac, cdividx, ccall)
c
c
c _________________________________________________________
c	Program Description
c
c		It calculates inflow to Soil Moisture
c		Called by Return2.for     
c
c _________________________________________________________
c	Documentation
c
c		itypSM	0=Do not limit by separate areas
c			  (e.g. soil is 1 big bucket)
c			1 Do limit by separate areas
c			  (e.g. Soil has an account for each)
c
c _________________________________________________________
c	Dimensions
c
       character cdividx*12, ccall*12
c
c _________________________________________________________       
c		Step 1; Initilize       
       iout=0
c
c		iout=0 no details
c		    =1 details
c		    =2 summary
c		    =  details
       if(cdividx.eq.ccall) iout=1
       iout=0
       
       small=0.001     
       c1 = amax1(0.0, cumax-cuact)
       
       if(itypSM.eq.0) then
         Afac=1.0
       else
         Afac=Area/AreaT
       endif  
c
c _________________________________________________________       
       
       if(c1.gt.small) then
         c2=amax1(0.0, (awcr-soils)*Afac)
         if(c2.gt.small/fac) then
           qdivs=amin1(c1*fac, c2)/fac
           soils1=soils
           soils=soils+qdivs*fac
         endif
       endif  
c
c _________________________________________________________       
       if(iout.gt.0) then
         write(nlog,300) 
         write(nlog,310)
     1     cdividx, itypSM, Afac, cumax*fac, cuact*fac, c1*fac, awcr, 
     1     Soils, C2, soils1, qdivs*fac, soils
       endif
c
c _________________________________________________________       
       
       return 
c
c _________________________________________________________       
 300   format(/
     2 '  SoilIn; ID          ',
     1 '  itypSM    Afac   cuMax   cuAct      c1    Awcr   Soils',
     1 '      c2  soils1   qdivs   soils',/
     1 '          ____________',
     1 ' _______ _______ _______ _______ _______ _______ _______',
     1 ' _______ _______ _______ _______')      
 310   format(10x,a12,i8,20f8.0)             
       
       end
