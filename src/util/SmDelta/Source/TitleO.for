
c                            
c ************************************************************
c
       subroutine titleo(itime, ptype, filen1, nameo)
         character ptype*24, itime*12, filen1*72, nameo*144, namez*144

c
c               Initilize
         do 100 i=1,144
           namez(i:i) = ' '
           nameo(i:i) = ' '
  100    continue

         namez(1:12)  = itime(1:12)
         namez(13:36) = ptype(1:24)
         namez(37:96) = filen1(1:72)
  
         ii = 0
         ib = 0
         do 110 i=1,144
           if(namez(i:i).ne.' ') then 
             ib=0
             ii=ii+1
             nameo(ii:ii) = namez(i:i)
           else
             ib=ib+1
             if(ib.eq.1) then
               ii=ii+1
               nameo(ii:ii) = namez(i:i)
             endif
           endif
  110    continue
         return
         end

