c setpar - set parameter types
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

        subroutine setpar(maxparm, nlog, paramd, paramr, paramw)
c
c
c _________________________________________________________
c       Program Description
c
c       Setpar; It set parameter types
c               Called by Statem
c
c _________________________________________________________
c       Documentation               
c
c
c _________________________________________________________
c       Dimensions
c
       dimension  paramd(maxparm), paramr(maxparm), paramw(maxparm),
     1            paramdX(40),     paramrX(40),     paramwX(40)
       character  paramd*24,       paramr*24,       paramw*24,
     1            paramdx*24,      paramrX*24,      paramwX*24

       data paramdX/
     1 'Total_Demand            ', 'CU_Demand               ',
     1 'From_River_By_Priority  ', 'From_River_By_Storage   ',
     1 'From_River_By_Other     ', 'From_River_Loss         ',
     1 'From_Well               ', 'From_Carrier_By_Priority',
     1 'From_Carrier_By_Other   ', 'From_Carrier_Loss       ',
     1 'Carried_Water           ', 'From_Soil               ',
     1 'Total_Supply            ', 'Total_Short             ',
     1 'CU_Short                ', 'Consumptive_Use         ',
     1 'To_Soil                 ', 'Total_Return            ',
     1 'Loss                    ', 'Upstream_Inflow         ',
     1 'Reach_Gain              ', 'Return_Flow             ',
     1 'Well_Depletion          ', 'To_From_GW_Storage      ',
     1 'River_Inflow            ', 'River_Divert            ',
     1 'River_By_Well           ', 'River_Outflow           ',
     1 'Available_Flow          ', 'Divert_For_Instream_Flow',
     1 'Divert_For_Power        ', 'Divert_From_Carrier     ',
     1 'rlossX                  ', 'rid                     ',
     1 'xstr                    ', 'Control_Location        ',
     1 'Control_Right           ', 'NA                      ',
     1 'NA                      ', 'NA                      '/

       data paramrX/
     1 'Initial_Storage',          'River_Priority',
     1 'River_Storage',            'River_Other',
     1 'River_Loss',               'Carrier_Priority',
     1 'Carrier_Other',            'Carrier_Loss',
     1 'Total_Supply',             'Storage_Use',
     1 'Storage_Exchange',         'Carrier_Use',
     1 'Total_Release',            'Evap',
     1 'Seep_Spill',               'Sim_EOM',
     1 'Target_Limit',             'Fill_Limit',
c
c rrb 2017/11/13; Correction, Add River_Release and
c                 River_Divert
cx   1 'River_Inflow',             'Total_Release',
cx   1 'Total_Supply',             'River_By_Well',
     1 'River_Inflow',             'River_Release',
     1 'River_Divert',             'River_By_Well',
c
c rrb 2017/11/13; Correction, Add Reservoir_Carry
c                 Reservoir_Loss and Reservoir Seep
cx     1 'River_Outflow',            'Divert_From_Carrier ',
cx     1 'Seep_Loss',                'ridr',
     1 'River_Outflow',            'Reservoir_Carry',
     1 'Reservoir_Loss',           'Reservoir_Seep',
     1 'ridr',
     1 'acc',                      'rnr',
     1 'NA',                       'NA',
     1 'NA',                       'NA',
     1 'NA',                       'NA',
     1 'NA',                       'NA',
     1 'NA',                       'NA',
     1 'NA'/
cx smalers 2017-11-20 removed one NA due to too many values being initialized
c    1 'NA',                       'NA'/

       data paramwX/
     1 'Total_Demand',             'CU_Demand',
     1 'From_Well',                'From_SW',
     1 'From_Soil',                'Total_Supply',
     1 'Total_Short',              'CU_Short',
     1 'Total_CU',                 'To_Soil',
     1 'Total_Return',             'Loss',
     1 'Total_Use',                'From_River',
     1 'To_From_GW_Storage',       'From_Salvage',
     1 'From_Soil',                'Total_Supply',
     1 'NA',                       'NA',
     1 'NA',                       'NA',
     1 'NA',                       'NA',
     1 'NA',                       'NA',
     1 'NA',                       'NA',
     1 'NA',                       'NA',
     1 'NA',                       'NA',
     1 'NA',                       'NA',
     1 'NA',                       'NA',
     1 'NA',                       'NA',
     1 'NA',                       'NA'/
c
c
c _________________________________________________________
c               Step 1; Initialize
      iout=0
      do i=1,maxparm
        paramd(i)=paramdX(i)
        paramr(i)=paramrX(i)
        paramw(i)=paramwX(i)
      end do
c
c               Print available parameters
      if(iout.eq.1) then           
        write(nlog,140) (j, paramd(j), j=1,maxparm) 
        write(nlog,141) (j, paramr(j), j=1,maxparm) 
        write(nlog,142) (j, paramw(j), j=1,maxparm)
        call flush(6)
      endif
      
      return
  140 format(/,
     1     ' Available diversion or streamflow parameters:',/
     1     (2x, i5, 1x, a24))
  141 format(/,
     1     ' Available reservoir parameters:',/
     1     (2x, i5, 1x, a24))
  142 format(/,
     1     ' Available well parameters:',/
     1     (2x, i5, 1x, a24))
      

      stop 
      end
