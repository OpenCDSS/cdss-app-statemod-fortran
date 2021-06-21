
c                            
c ************************************************************
       subroutine scryr(ichk,iystr,iyend,iystr0,iyend0,iyreq,imreq,
     1            fillog)
       character imreq*3, fillog*72
c
c               Screen for the requested year
        if(iyreq.eq.0) then
          iystr=iystr0
          iyend=iyend0
        else                                               
          iystr=iyreq
          iyend=iyreq
          if(iyreq.lt.iystr0 .or. iyreq.gt.iyend0) goto 100
        endif    
        
        if(ichk.ne.0) then
          write(99,*)  '  iystr0, iyend0, iyreq, iystr, iyend'
          write(99,*)     iystr0, iyend0, iyreq, iystr, iyend
        endif
        return

  100    write(99,110) iyreq, imreq, iystr0, iyend0
  110    format(
     1     '   Scryr; Problem reading diversion data',/,
     1     '     Requested year and month:          ', i4, 1x, a3,/
     1     '     Beginning and Ending Year in file: ', i4, 1x, i4)
c
         write(6,120) fillog
  120    format('  Scryr - Unsuccessful termination, see ', a72)
         write(6,*) 'Stop 1'
         call flush(6)
         stop 
         end
