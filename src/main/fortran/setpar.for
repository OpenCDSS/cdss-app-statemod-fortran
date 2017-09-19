c                            
c ************************************************************
c
        subroutine setpar(maxparm, nlog, paramd, paramr, paramw)
c
c
c _________________________________________________________
c	Program Description
c
c       Setpar; It set parameter types
c		Called by Statem
c
c _________________________________________________________
c       Documentation               
c
c
c _________________________________________________________
c	Dimensions
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
     1 'River_Inflow',             'Total_Release',
     1 'Total_Supply',             'River_By_Well',
     1 'River_Outflow',            'Divert_From_Carrier ',
     1 'Seep_Loss',                'ridr', 
     1 'acc',                      'rnr', 
     1 'River_Exchange',           'Carrier_Storage', 
     1 'NA',                       'NA',
     1 'NA',                       'NA',
     1 'NA',                       'NA',
     1 'NA',                       'NA',
     1 'NA',                       'NA'/

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
c		Step 1; Initilize
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
