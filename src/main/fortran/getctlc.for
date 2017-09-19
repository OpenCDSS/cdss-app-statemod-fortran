c
c *********************************************************
c
      subroutine GetCtlC(nlog, nf, iok, itype, ctypeX, iX, rX, cX, cY)
c
c
c _________________________________________________________
c	Program Description
c
c       GetCtlC; A control file reader taht recognizes command arguements
c		 (e.g. Starting_Year = 1950)
c
c _________________________________________________________
c	Documentation
c
c               iok     0 not OK if not found
c                       1 OK if not found
c			-1 Stop read exit
c               itype   0 integer
c                       1 = real
c                       2 = character *12
c			3 = character *80
c               iX = integer value output
c               rX = real value output
c               cX = character *12 value output
c		cY = character *80 value output
c
c		ifound=0 data not found
c		ifound=1 data is found
c		ifound=-1 data not found but ok or stop
c
c		Data Types
c	Title
c
c	Year_Type
c	Starting_Year
c	Ending_Year
c
c	Time_Step	
c	Output_Units    
c
c	Stream_Inflow_Type
c
c	Detailed_Call_Switch
c
c	Detailed_Output_Type     1,2, ...
c	Detailed_Output_ID
c
c	Variable_Efficiency
c	Sprinkler_Approach	Maximum Supply(1),Mutual Supply(2)
c
c       Soil_Moisture_Depth  
c _________________________________________________________
c	Dimensions
      character ctype*24, ctype1*24, ctype2*24, cdata*132, 
     1 rec1*1, rec132*132, cx*12, cy*80, c*12, ctypeX*24
      dimension ctype(35)
      
      data ctype/
     1 'Title_1                 ',     
     2 'Starting_Year           ', 
     3 'Ending_Year             ',
     4 'Output_Units            ',
c     
     5 'Climate_Type            ',
     6 'Streamflow_Type         ',
c     
     7 'Precipation_Stations    ',
     8 'Evaporation_Stations    ',
     9 'URF_Values              ',
c     
     1 'CFS_Factor              ',
     1 'Stream_Factor           ',
     2 'Diversion_Factor        ',
     3 'Instream_Flow_Factor    ',
     4 'Reservoir_Factor        ',
     5 'Evaporation_Factor      ',
     6 'Precipitation_Factor    ',
c
     7 'Year_Type               ', 
     8 'Demand_Type             ',
     9 'Detailed_Output         ',
     2 'Reoperation_Control     ',
     1 'Instream_Flow_Type      ',
c
     2 'Call_Switch             ',
     3 'Call_ID                 ', 
c
     4 'Time_Step               ',  
     5 'Well_Control            ',
     6 'Maximum_Stream_Loss     ',
     7 'San_Juan_RIP            ',
c
     8 'Annual_Time_Series      ', 
     9 'Variable_Efficiency     ',
     3 'Sprinkler_Approach      ',	
     1 'Soil_Moisture_Depth     ',   
     2 'Significant_Figures     ',
     3 'CU_Approach             ',
     4 'Title_2                 ',     
     5 'Stop                    '/
c
c _________________________________________________________
c		Step 1; Initilize
c		iout=0 no details
c		iout=1 max details
c		iout=2 summary detail
      iout=2
      maxtyp=35
      istop=0
      nin=0
      iX=0
      rX=0.0
      cX=' '
      rec132=' '
      ctypeX=adjustl(cTypeX)
      ctype1=' '

      
      if(iout.eq.1) then
        write(Nlog,*) ' '
        write(nlog,*) '_________________________________________'
        write(nlog,*) ' GetCtlC; Reading: ', ctypeX
      endif  
c
c		Allow data to be provided in any order      
      rewind(nf)
c
c _________________________________________________________
c               Step 2; Read Data

 100  read(nf,'(a132)',end=500,err=500) rec132
cx    write(nlog,'(a132)') rec132
cx    if(iout.eq.1) write(nlog,*) ' GetCtlC; Input  = ', rec132
      if(rec132(1:1).eq.'#') goto 100
c
c _________________________________________________________
c
c               Step 3; Set Data Type
c
      rec132=adjustl(rec132)
      ifound=0
      ctype1 = ' '
      
      if(rec132(1:4).eq.'Stop' .or. rec132(1:1).eq.' ') then
        ifound=-1
        goto 500
      endif
        
      do i=1,132
        if(rec132(i:i) .ne. ' ' .and. rec132.ne.'=') then
          ifound=1
          ctype1(i:i) = rec132(i:i)
          i1 = i
          nin=nin+1
        else
          goto 110
        endif
      end do
c
c -----------------------------------------------------------     
     
 110  if(iout.eq.1) then
        write(nlog,200) 'FYI Data Type   ', rec132, 'ctype1  ', ctype1,
     1    'ctypeX  ', ctypeX, 'itype   ', itype
      endif  
c
c -----------------------------------------------------------     
c		Exit if Stop is provided
      ctype2=ctype1
      if(ctype2(1:4).eq.'Stop') then
        istop=1
        goto 500
      endif  
      
c
c -----------------------------------------------------------     
      if(ifound.eq.0) then
        write(nlog,210) 'Problem; an equal (=) sign is not provided'
        goto 900
      endif  
c
c _________________________________________________________
c		Step 4; Check proper data type else read again
      ifound=0
      ic=0
      inz=0
      do i=1,24
        if(ctype1(i:i).ne.' ') then
          inz=inz+1
          if(ctype1(i:i).eq.ctypeX(i:i)) ic=ic+1
        endif  
      end do  
      
cx       if(iout.eq.1) write(nlog,*)
cx     1    ' GetCtlC; Type Check, inz, ic ', inz, ic, ctype1, ctypeX      
c
c -----------------------------------------------------------     
      
      if(inz.gt.0 .and. inz.eq.ic) then
        if(iout.eq.1) write(nlog,*)
     1    ' GetCtlC; Type found ctype1, inz, ic ', 
     1    ctype1, inz, ic
      else
        goto 100
      endif

c
c _________________________________________________________
c
c               Step 5; Find Data Value
c
      ifound=0
      do i=i1,132
        ifound=0
        if(rec132(i:i) .eq. '=') then
c         write(nlog,*) ' GetCtlC; equal found at i = ', i
c
c		skip blanks befor =        
          ifound=1
          do ix=i+1,132
            if(rec132(ix:ix).eq.' ') then
            else
              ifound=1
              cData=' '
c             write(nlog,*) ' GetCtlC; data found at ix = ', ix
              
              do j=1,132
                j1=ix+j-1
                rec1=rec132(j1:j1)
                if(rec1.ne.' ') then
                  cData(j:j) = rec1
                else
                  cData=adjustl(cData)
                  goto 120
                endif              
              end do
            endif  
          end do  
        endif  
      end do
c
c -----------------------------------------------------------     
     
 120  if(iout.eq.1) then
        write(nlog,200) 'FYI Data Value  ', rec132, 'ctype1   ', ctype1,
     1    'cData   ', cData, 'itype   ', itype
      endif  
c
c -----------------------------------------------------------     
     
      if(ifound.eq.0) then
        write(nlog,210) '  GetCtlC; Problem; reading a data value'
        write(nlog,210) '  GetCtlC; Chek an equal (=) sign provided'
      endif  
     
c
c _________________________________________________________
c		Step 5; Read data type
      ifound=0
      select case(itype)
c
c
c -----------------------------------------------------------     
c               Type 0; Integer
        case(0)
          read(cData,*,end=300,err=300) iX
          ifound=1
          if(iout.ge.1) write(nlog,240) ctype1,iX
c
c
c -----------------------------------------------------------     
c               Type 1; Real
        case(1)
          read(cData,*,end=300,err=300) rX
          ifound=1
          if(iout.ge.1) write(nlog,242) ctype1, rX
          goto 500
c
c -----------------------------------------------------------     
c               Type 2; Character *4
        case(2)
          cX(1:12) = cData(1:12)
          ifound=1
          if(iout.ge.1) write(nlog,244) ctype1, cX
          goto 500
c
c -----------------------------------------------------------     
c               Type 3; Character *80
        case(3)
          cY(1:80) = rec132(ix:ix+79)
          ifound=1
          if(iout.ge.1) write(nlog,246) ctype1, cY
          goto 500
c
c -----------------------------------------------------------     
c		Default
c
        case default
          write(nlog,220) 'Problem', itype
          ifound=0          
          goto 900
      end select
c
c _________________________________________________________
c               Check if eof is OK
 500  if(iout.eq.1) write(nlog,*) ' GetCtlC; ifound, iok = ',ifound,iok
      if(ifound.eq.0 .and. iok.eq.0) goto 900
      if(ifound.eq.-1 .and. iok.eq.1) write(nlog,248) ctypeX
c
c _________________________________________________________
c               Return
      return
c
c _________________________________________________________
c               Formats     
      
 200  format(
     1 '  GetCtlC; ', a16, /
     1 '            Rec132  = ', a132,/
     1 '            ',a8,  '= ', a24,/
     1 '            ',a8,  '= ', a24,/
     1 '            ',a8,  '= ', i4)
 210  format(
     1 '  GetCtlC; ', a64)
 220  format(/,72('_'),/
     1 '  GetCtlC; ', a8, ' Case ', i5, ' not available')
 230  format(
     1 '  GetCtlC; ', /
     1 '           ' a24,/
     1 '           ' a24)
     
 240  format(
     1 '  GetCtlC; Data type ', a24, ' Integer Value  = ',i8)        
 242  format(
     1 '  GetCtlC; Data type ', a24, ' Real Value     = ',f8.0)        
 244  format(
     1 '  GetCtlC; Data type ', a24, ' Char*12 Value  = ',a12)        
 246  format(
     1 '  GetCtlC; Data type ', a24, ' Char*80 Value = ',a80)        
 248  format(
     1 '  GetCtlC; Data type ', a24, ' NOT FOUND')        
c
c _________________________________________________________
c               Error Processing
 300  write(nlog,210) '  GetCtlC; Problem reading data'
      goto 900
      
 310  write(nlog,320) ctypeX
 320  format(
     1 ' GetCtlC; Problem cannot find data type = ',a24)
      goto 900
      
 900  write(6,910) 
      write(nlog,920) 
      call flush(6)
 910  format('  Stopped in GetCtlC')
 920  format(72('_'),/
     1 '  GetCtlC; Stopped in GetCtlC, see the log file (*.log)')
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)

      stop 
      END


