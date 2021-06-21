c                            
c ************************************************************
c
        subroutine getpar(itype, ip, iw, ptypex, fillog)
c
c rrb 99/08/27; Revised to match new well related data in
c               *.xdd and *.xre.  Added paramw data but did not
c               yet enhance to work with well data
c
c               itype = 0 diversion or stream
c                     = 1 reservoir
c
c                       4 well
c               iw    = 1 for StateMod w/o wells
c                     = 2 for StateMod with wells
c		      = 3 for StateMod version 11.x

         dimension  paramd(3,30), paramr(3,30), paramw(3,30)
         character  paramd*24,    paramr*24,    paramw*24,   ptypex*24
         character  fillog*72
c
       data (paramd(3,i), i=1,30) /
     1 'Total_Demand            ', 'CU_Demand               ',
     3 'From_River_By_Priority  ', 'From_River_By_Storage   ',
     5 'From_River_By_Exchange  ', 'From_River_Loss         ',
     7 'From_Well               ', 'From_Carrier_By_Priority',
     9 'From_Carrier_By_Storage ', 'From_Carrier_Loss       ',
     2 'Carried_Water           ', 'From_Soil               ',
     3 'Total_Supply            ', 'Total_Short             ',
     5 'CU_Short                ', 'Consumptive_Use         ',
     7 'To_Soil                 ', 'Total_Return            ',
     9 'Loss                    ', 'Upstream_Inflow         ',
     3 'Reach_Gain              ', 'Return_Flow             ',
     3 'Well_Depletion          ', 'To_From_GW_Storage      ',
     5 'River_Inflow            ', 'River_Divert            ',
     7 'River_By_Well           ', 'River_Outflow           ',
     9 'Available_Flow          ', 'NA                      '/

       data (paramr(3,i), i=1,30) /
     1 'Initial_Storage',          'River_Priority',
     1 'River_Storage',            'River_Exchange',
     1 'River_Loss',               'Carrier_Priority',
     1 'Carrier_Storage',          'Carrier_Loss',
     1 'Total_Supply',             'Storage_Use',
     1 'Storage_Exchange',         'Carrier_Use',
     1 'Total_Release',            'Evap',
     1 'Seep_Spill',               'Sim_EOM',
     1 'Target_Limit',             'Fill_Limit',
     1 'River_Inflow',             'Total_Release',
     1 'Total_Supply',             'River_By_Well',
     1 'River_Outflow',            'Divert_From_Carrier ',
     1 'Seep_Loss',                'ridr', 
     1 'acc',                      'rnr', 
     1 'NA',                        'NA'/

       data (paramw(3,i),i=1,30) /
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
     1 'NA',                       'NA'/

       data (paramd(2,i), i=1,30) /
     1 'Total_Demand',             'CU_Demand',
     1 'From_River_By_Priority',   'From_River_By_Storage',
     1 'From_River_By_Exchange',   'From_River_Loss',
     1 'From_Well',                'From_Carrier_By_Priority',
     1 'From_Carrier_By_Storage',  'From_Carrier_Loss',
     1 'Carried_Water',            'From_Soil',
     1 'Total_Supply',             'Total_Short',
     1 'CU_Short',                 'Consumptive_Use',
     1 'To_Soil',                  'Total_Return',
     1 'Loss',                     'Upstream_Inflow',
     1 'Reach_Gain',               'Return_Flow',
     1 'Well_Depletion',           'To/From_GW_Storage',
     1 'River_Inflow',             'River_Divert', 
     1 'River_By_Well',            'River_Outflow',
     1 'Available_Flow',            ' ' /

       data (paramr(2,i), i=1,30) /
     1 'Initial_Storage',       'River_Priority',
     1 'River_Storage',         'River_Exchange',
     1 'River_Loss',            'Carrier_Priority',
     1 'Carrier_Storage',       'Carrier_Loss',
     1 'Total_Supply',          'Storage_Use',
     1 'Storage_Exchange',      'Carrier_Use',
     1 'Total_Release',         'Evap',
     1 'Seep_Spill',            'Sim_EOM',
     1 'Target_Limit',          'Fill_Limit',
     1 'River_Inflow',          'Total_Release',
     1 'Total_Supply',          'River_By_Well',
     1 'River_Outflow',         ' ',
     1  ' ',' ',' ',' ', ' ', ' '/

       data (paramw(2,i),i=1,30) /
     1 'Total_Demand',          'CU_Demand',
     1 'From_Well',             'From_SW',
     1 'From_Soil',             'Total_Supply',
     1 'Total_Short',           'CU_Short',
     1 'Total_CU',              'To_Soil',
     1 'Total_Return',          'Loss',
     1 'Total_Use',             'From_River',
     1 'From_GwStor',           'From_Salvage',
     1 'From_Soil',             'Total_Source',
     1 ' ',' ',' ',' ',' ',' ',' ',' ',' ',' ', ' ', ' '/
                                                              
       data (paramd(1,i), i=1,30) /
     1 'ConsDemand             ',  
     1 'FromRiverByPriority',  'FromRiverByStorage',
     1 'FromRiverByExchange',  'FromCarrierByPriority',
     1 'FromCarrierByStorage', 'CarriedWater',
     1 'TotalSupply',          'Short',
     1 'ConsumptiveWaterUse',  'WaterUse,TotalReturn',
     1 'UpstreamInflow',       'ReachGain',
     1 'ReturnFlow',           'RiverInflow',
     1 'RiverDivert',          'RiverOutflow',
     1 ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
     1 ' ', ' ', ' ', ' ', ' '/

       data (paramr(1,i), i=1,30) /
     1 'InitialStorage',       'RiverPriority',
     1 'RiverStorage',         'RiverExchange',
     1 'CarrierPriority',      'CarrierStorage',
     1 'TotalSupply',          'StorageUse',
     1 'StorageExchange',      'CarrierUse',
     1 'TotalRelease',         'Evap',
     1 'SeepSpill',            'SimEOM',
     1 'TargetLimit',          'Fill Limit',
     1 'Inflow',               'Outflow',
     1 ' ',  ' ', ' ', ' ', ' ', ' ', ' ',
     1 ' ', ' ', ' ', ' ', ' '/

       data (paramw(1,i),i=1,30) /
     1 'Demand',               'FromWell',
     1 'FromOther',            'Short',
     1 'ConsumptiveUse',       'Return',
     1 'Loss',                 'River',
     1 'GWStor',               'Salvage',
     1 ' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',
     1 ' ',' ',' ',' ',' ',
     1 ' ', ' ', ' ', ' ', ' '/
c
c _________________________________________________________
c
         iout=0
c               Process diversion or streamGage type
         do 100 i=1,30
           write(99,*) i, ptypex, paramd(iw,i)
           if(itype.eq.0 .and. ptypex.eq.paramd(iw,i)) then
             ip = i
             write(99,*) ' GetPar Found type ', iw, ip, 
     1        ptypeX, paramd(iw,i)
             goto 110
           endif        
c
c               Process reservoir type
           if(itype.eq.1 .and. ptypex.eq.paramr(iw,i)) then
             ip = i
             goto 110
           endif
c
c               Process well type
           if(itype.eq.4 .and. ptypex.eq.paramw(iw,i)) then
             ip = i
             goto 110
           endif   
c
c               Process Operational Right type
           if(itype.eq.6) then
             ip=1
             goto 110
           endif  
c
c               Process Water Budget type
           if(itype.eq.7) then
             ip=1
             goto 110
           endif  

c
c               Print available parameters
           if(itype.eq.99) then           
             write(99,150) 
             write(6,140)  (paramd(1,j),j=1,30),(paramr(1,j),j=1,30)
             write(99,140) (paramd(1,j),j=1,30),(paramr(1,j),j=1,30) 
             
             write(6,142)  (paramd(2,j),j=1,30),(paramr(2,j),j=1,30)
             write(99,142) (paramd(2,j),j=1,30),(paramr(2,j),j=1,30), 
     1                     (paramw(2,j),j=1,30)

             call flush(6)
             goto 110
           endif
  100    continue
         goto 120

  110    if(iout.eq.1) write(99,*) '  Getpar, ptypex, ip ', ptypex, ip
         return
c
c               Error Messages
  120    write(99,*) '   Parameter not found ', ptypex
         write(99,140) (paramd(1,j), j=1,30), 
     1                 (paramr(1,j), j=1,30)
         write(99,142) (paramd(2,j), j=1,30), 
     1                 (paramr(2,j), j=1,30),
     1                 (paramw(2,j), j=1,30)

         write(6,130) fillog
         write(99,130) fillog
  130    format(' Getpar - Unsuccessful termination, see ', a72)
  140    format(
     1      /,' Available diversion or streamflow parameters:',/
     1        ' StateMod versions 1 - 8 (without wells)',/
     1   20(2x, a24,/),
     1      /,' Available reservoir parameters:',/
     1        ' StateMod versions 1 - 8 (without wells)',/ 
     1   20(2x, a24,/))
  142    format(
     1      /,' Available diversion or streamflow parameters:',/
     1        ' StateMod versions 9 - n (with wells)',/
     1   20(2x, a24,/),
     1      /,' Available reservoir parameters:',/
     1        ' StateMod versions 9 - n (with wells',/ 
     1   20(2x, a24,/),
     1      /,' Available well parameters (not operational):',/
     1        ' StateMod versions 9 - n (with wells',/ 
     1   20(2x, a24,/))
  

  150    format(/
     1     ' Acceptable run types:',/
     1     '   Single',/             
     1     '   Multiple',/
     1     '   Difference',/
     1     '   Merge',/
     1     '   Diffx',/
     1     '   Help',/
     1     '   Version')

         write(6,*) 'Stop 1'
         call flush(6)

         stop 
         end
