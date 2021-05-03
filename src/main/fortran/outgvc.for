c outgvc
c_________________________________________________________________NoticeStart_
c StateMod Water Allocation Model
c StateMod is a part of Colorado's Decision Support Systems (CDSS)
c Copyright (C) 1994-2021 Colorado Department of Natural Resources
c 
c StateMod is free software:  you can redistribute it and/or modify
c     it under the terms of the GNU General Public License as published by
c     the Free Software Foundation, either version 3 of the License, or
c     (at your option) any later version.
c 
c StateMod is distributed in the hope that it will be useful,
c     but WITHOUT ANY WARRANTY; without even the implied warranty of
c     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c     GNU General Public License for more details.
c 
c     You should have received a copy of the GNU General Public License
c     along with StateMod.  If not, see <https://www.gnu.org/licenses/>.
c_________________________________________________________________NoticeEnd___

      Subroutine outGVC(nlogx, nout, divact1, divsum, gvCot,
     1            noutGVC, fac, l2, cRule,  
     1            l1, rec12, nameX)
c    
c
c_________________________________________________________________
c
c       Update History
c
c rrb 2021/04/18; Compiler warning
c
c _________________________________________________________
c      Dimensions
      include 'common.inc'
      character cRule*12, nameX*24
      character rec12*12, fileOpen*40, ctype*16, rec12b*12,
     1          cstaid1*12 

c
c _________________________________________________________
c		Step 1; Initialize
c                                  
c
c rrb 2021/04/18; Compiler warning
      crule=crule
      cstaid1=' '
      ctype=' '
      rec12b=' '
      fileopen=' '
c      
      if(mon.ne.7) goto 500
      ioutGVC=0
      nout=0
      small=0.001
      
c
c ____________________________________________      
      if(nout.eq.1) then
        if(ioutGVC.eq.1) then       
          write(nlogx,*) '  OutGVC;  iyr, mon, idy'
          write(nlogx,'(8x,6i5)')    iyr, mon, idy
        endif
        goto 500
      endif
c
c
c _________________________________________________________
c 
      if(nout.eq.2) then
        shortGVC = amax1(divert(mon,325)-qdiv(5,697),0.0)   
        GVC=0  
        if(shortGVC.gt.small.and.qdiv(5,696).gt.small) GVC=1.
        if(shortGVC.gt.small.and.qdiv(5,695).gt.small) GVC=-1.
        if(shortGVC.lt.small.and.qdiv(5,696).gt.small) GVC=-2.
 
        GVCo= qdiv(5,697) + qdiv(5,696)+ qdiv(5,695)
        if(GVCo.gt.GVCot) then
          GVCot=GVCo    
          
          noutGVC=noutGVC+1
          if(noutGVC.eq.1.or. noutGVC.eq.5)
     1      write(nlogx,523) IYRmo(mon), xmonam(mon),idy
                     
          write(nlogx,524) IYRmo(mon), xmonam(mon), idy, iwx,
     1    l2, 'Opr Rule    ', ityopr(l2)+100, corid(l2), 
     1    nameo(l2), divact1, divsum, divchk*fac,
     1    qdiv(5,685)*fac, qdiv(5,697)*fac, 
     1    qdiv(5,696)*fac, qdiv(5,695)*fac,
     1    shortGVC*fac,GVC
        endif
        goto 500
      endif
c
c ____________________________________________
c rrb 20100123; OMID Check for Return Flows caused by Replace   
c 
      if(nout.eq.3) then   
        shortGVC = amax1(divert(mon,325)-qdiv(5,697),0.0)   
        GVC=0  
        if(shortGVC.gt.small.and.qdiv(5,696).gt.small) GVC=1.
        if(shortGVC.gt.small.and.qdiv(5,695).gt.small) GVC=-1.  
        if(shortGVC.lt.small.and.qdiv(5,696).gt.small) GVC=-2.
     
        GVCo= qdiv(5,697) + qdiv(5,696)+ qdiv(5,695)
        if(GVCo.gt.GVCot) then
          GVCot=GVCo  
            
          noutGVC=noutGVC+1
          if(noutGVC.eq.1.or. noutGVC.eq.5)
     1      write(nlogx,523) IYRmo(mon), xmonam(mon),idy
                       
          write(nlogx,524) IYRmo(mon), xmonam(mon), idy, iwx,
     1      l2, 'Opr Rule    ', l1, rec12, 
     1      nameX, divact1, divsum, divchk*fac,
     1      qdiv(5,685)*fac, qdiv(5,697)*fac, 
     1      qdiv(5,696)*fac, qdiv(5,695)*fac,
     1      shortGVC*fac,GVC
        endif 
        goto 500           
      endif
      
c
c ____________________________________________
c rrb 20100123; OMID Check for an Operating Rule
c 
      if(nout.eq.4) then   
        shortGVC = amax1(divert(mon,325)-qdiv(5,697),0.0)   
        GVC=0  
        if(shortGVC.gt.small.and.qdiv(5,696).gt.small) GVC=1.
        if(shortGVC.gt.small.and.qdiv(5,695).gt.small) GVC=-1.  
        if(shortGVC.lt.small.and.qdiv(5,696).gt.small) GVC=-2.                                 
                  
        GVCo= qdiv(5,697) + qdiv(5,696)+ qdiv(5,695)
        if(GVCo.gt.GVCot) then
          GVCot=GVCo    
          noutGVC=noutGVC+1
          
          if(noutGVC.eq.1.or. noutGVC.eq.5)
     1      write(nlogx,523) IYRmo(mon), xmonam(mon),idy
                       
          write(nlogx,524) IYRmo(mon), xmonam(mon), idy, iwx,
     1    l2, 'Opr Rule    ', ityopr(l2)+100, corid(l2),                  
     1      nameo(l2),divact1, divsum, divchk*fac,
     1      qdiv(5,685)*fac, qdiv(5,697)*fac, 
     1      qdiv(5,696)*fac, qdiv(5,695)*fac,
     1      shortGVC*fac,GVC
        endif 
        goto 500           
      endif
      
c
c ____________________________________________
c rrb 20100123; OMID Check   
c
      if(nout.eq.5) then
        shortGVC = amax1(divert(mon,325)-qdiv(5,697),0.0)   
        GVC=0  
        if(shortGVC.gt.small.and.qdiv(5,696).gt.small) GVC=1.
        if(shortGVC.gt.small.and.qdiv(5,695).gt.small) GVC=-1. 
        if(shortGVC.lt.small.and.qdiv(5,696).gt.small) GVC=-2.

        GVCo= qdiv(5,697) + qdiv(5,696)+ qdiv(5,695)
        if(GVCo.gt.GVCot) then
          GVCot=GVCo   
           
          noutGVC=noutGVC+1
          if(noutGVC.eq.1.or. noutGVC.eq.5)
     1      write(nlogx,523) IYRmo(mon), xmonam(mon),idy
                                     
          write(nlogx,524) IYRmo(mon), xmonam(mon), idy, iwx,
     1      l2, 'Opr Rule    ', l1, rec12, 
     1      nameX, divact1, divsum, divchk*fac,
     1      qdiv(5,685)*fac, qdiv(5,697)*fac, 
     1      qdiv(5,696)*fac, qdiv(5,695)*fac, 
     1      shortGVC*fac, GVC
        endif

        goto 500
      endif
c
c ____________________________________________
c rrb 20100123; OMID Check  
      if(nout.eq.6) then
        shortGVC = amax1(divert(mon,325)-qdiv(5,697),0.0)   
        GVC=0  
        if(shortGVC.gt.small.and.qdiv(5,696).gt.small) GVC=1.
        if(shortGVC.gt.small.and.qdiv(5,695).gt.small) GVC=-1. 
        if(shortGVC.lt.small.and.qdiv(5,696).gt.small) GVC=-2.              
                  
        GVCo= qdiv(5,697) + qdiv(5,696)+ qdiv(5,695)
        
        if(GVCo.gt.GVCot) then
          GVCot=GVCo    
          
          noutGVC=noutGVC+1
          if(noutGVC.eq.1.or. noutGVC.eq.5)
     1      write(nlogx,523) IYRmo(mon), xmonam(mon),idy
                      
          write(nlogx,524) IYRmo(mon), xmonam(mon), idy, iwx,
     1      -1, 'Res Seepage ', -1, 'NA          ', 
     1      'NA                      ',     
     1      divact1, divsum, divchk*fac,
     1      qdiv(5,685)*fac, qdiv(5,697)*fac, 
     1      qdiv(5,696)*fac, qdiv(5,695)*fac,
     1      shortGVC*fac,GVC
        endif
        goto 500
      endif
c
c ____________________________________________      
 500  if(noutGVC.eq.5) noutGVC=0
      return
c
c ____________________________________________ 
 523  format(/,72('_'),/
     1 '  OutGVC;  Reoperation Report for = ',i5,1x, a4,i3,//
     1 '                                                     ',
     1 '                               ',
     1 '   Change Value      Check Sum    Check Value',/
     1 ' Year Mon Day Reop   l2 Reason       Opr Type Right ID    ',
     1 ' Right Name               ',
     1 '     af/mo     af/mo     af/mo',
     1 '    721239    720645    950003    950006',
     1 '  ShortGVC    GVC',/     
     1 ' ____ ___ ___ ____ ____ ____________ ________ ____________',
     1 ' ________________________ ',
     1 9(' _________'))

 524  format(i5, 1x,a4, i3, 2i5, 1x,a12,1x,i8, 1x,a12,1x,a24,1x,
     1 20f10.2)     
c
c ____________________________________________ 
      stop
      end     
