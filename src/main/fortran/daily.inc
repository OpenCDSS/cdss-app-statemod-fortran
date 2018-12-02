c daily.inc - daily data common block
c_________________________________________________________________NoticeStart_
c StateMod Water Allocation Model
c StateMod is a part of Colorado's Decision Support Systems (CDSS)
c Copyright (C) 1994-2018 Colorado Department of Natural Resources
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

C     Last change:  C    26 Apr 97   11:25 pm
C
c
c
c		Daily.inc
c		Daily values only to save space
c
c _________________________________________________________
c rrb 04/29/97; Daily model
      COMMON/dayD1/
     1 virind(32,3000),  diverd(32,2288), diverdw(32,602),
     1 flowrd(32,546),   targetd(32,555),  
     1 riverm(3000),     availm(3000),    avinpm(3000), returm(3000),
c
     1 returd(7320,1725), dlyratd(7320,3100),

     1 divreqm(2288),    divreqwm(3600),
     1 flowrqm(546),     qdivm(34,3000),  qdivrm(2299),
     1 qresm(34,555),    accrm(34,555),   volintm(555), evapm(555),
     1 sepactM(555), 
     1 ritremm(201),     tarmaxm(555),    divom(601),
     1 rlossm(2288),     rlosswm(10000),   
     1 rlossw2m(10000),  divmonwm(602),
     1 rdepwm(3600),
     1 dcum(2288),       dcutm(2288),     dcuwm(602),   dcutwm(602),
     1 gw2rivm(3000),    gw2gwm(3000), 
     1 qdivsm(2299),     qdivsom(2299),   qdivswm(602), qdivswom(602), 
     1 diverird(32,2288),diwrd(32,2288),
     1 diveridw(32,602), diwrdw(32,602),
     1 divertd(32,2288), diveritd(32,2288)
c    1 evapd(32,555),    


c
c
c _________________________________________________________
c               Daily data read
      COMMON/dayD2/    
     1 virindx(32,3000), diverdx(32,2288),diverdxw(32,602),
     1 flowrdx(32,546),  targex2(32,555), evapdx(32,555),
     1 diverd2(32,2288)
