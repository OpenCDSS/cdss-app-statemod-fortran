c chkrivrf - checks whether return to river data is located properly
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

c     Last change:  RRB  18 Dec 100    2:29 pm
c
      SUBROUTINE ChkRivRF(nlog, icx, k, fac, maxopr, maxsta,  
     1  intern,  idcdD,  itypeR, idncod, ndnnod, cstaid, cidvri)
c
c		It check return to river data is 
c		located properly
c
c 2008/01/04; Added nRiver to indicate a carrier release to river
c _________________________________________________________
c		Documentation
c
c	 itypeR         0 = served directly
c	 itypeR         1 = served by exchange
c	 nCarry         Indicator at least 1 carrier
c	 nRiver		Indicator release to the River
c	 ncnum          # of carriers
c	 corid1         Operating rule ID
c	 idcdD		Destination location
c
c
c _________________________________________________________
c		Dimension and Character        
      dimension intern(maxopr,10) 
      character corid1*12
c
      dimension idncod(maxsta), ndnnod(maxsta), cstaid(maxsta)
      character cidvri*12, csource*12, cdest*12, cstaid*12   
c
c
c _________________________________________________________
c		Step 1;  Initialize
      iout=1      
      if(iout.eq.1) then
        write(nlog,250) 
        write(nlog,*) ' ChkRivRF; ', cidvri
      endif  
        
      nCarry=intern(k,1)
      nRiver=0
      small=0.001
      
      idcdD1=idcdD
      cDest=cstaid(idcdD1)
      
      if(iout.eq.1) write(nlog,*) ' ChkRivRF; nCarry = ',k, nCarry
c
c _________________________________________________________
c		Step 2; Calculate location of return (Source)
c		        to location of demand (idcdD)
      ncnum=0
      if(ncarry.gt.0) then
        do i=1,10
          ncar=intern(k,i)
      
          if(iout.eq.1) write(nlog,*) ' ChkRivRF; i, ncar = ',i, k,ncar
c
c ---------------------------------------------------------
c		Ncar<0 a river return
          if(ncar.lt.0) then
            nRiver=-1*ncar   
            ndns=ndnnod(nRiver)
            cSource=cstaid(nRiver)
            if(iout.eq.1) write(nlog,*) ' ChekRivRF; nRiver = ',nRiver
            
c
c ---------------------------------------------------------
c		Served directly            
            if(itypeR.eq.0) then            
              call oprdown(nlog, maxsta,
     1        ndns, nRiver, idcdD, idncod, cidvri,
     1        csource, cdest)   
            endif  
c
c ---------------------------------------------------------
c		Served by Exchange
            if(itypeR.eq.1) then            
              call oprExp(nlog, maxsta, idcdD, nRiver, idncod, 
     1          ndnnod, iExPoint1, cidvri)
            endif                     
c
c ---------------------------------------------------------
c		Detailed Outuput
            if(iout.eq.1) then
              if(itypeR.eq.1) write(nlog,170) cidvri, icx,
     1          csource, nRiver, cdest, nRiver
              if(itypeR.eq.0) write(nlog,172) cidvri, icx, 
     1          csource, nRiver, cdest, idcdD1
            endif  
            
          endif
        end do  
      endif
c
c _________________________________________________________
c		Step 3; Detailed Output
      
 500  continue
c
c _________________________________________________________
c		Step 4; Return
      return
c _________________________________________________________
c
c               Formats
  170 format(/,
     1 '  ChkRivRF; Operational right ', a12,' Type = ',i5/
     1 11x,'The destination river return' ,/
     1 11x,'IS LOCATED PROPERLY to be served directly',/
     1 11x,'(e.g the destination is downstream from the source)',/
     1 11x,'Source River ID and pointer (iss1)      = ',a12, 1x, i5,/
     1 11x,'Destination River ID and pointer (iscd) = ',a12, 1x, i5)
     
  172 format(/,
     1 '  ChkRivRF; Operational right ', a12,' Type = ',i5/
     1 11x,'The destination river return' ,/
     1 11x,'IS LOCATED PROPERLY to be served by exchange',/
     1 11x,'(e.g. the destination is upstream from the source)',/
     1 11x,'Source River ID and pointer (nriver)       = ',a12, 1x, i5,/
     1 11x,'Destination River ID and pointer (nRiver) = ',a12, 1x, i5)
     
 250  format(/,72('_'))       
      
c
c _________________________________________________________
c		Step 3; Return
      return 
c
c _________________________________________________________
c		Formats      
c
c_____________________________________________________________
c               Error warnings
      end
