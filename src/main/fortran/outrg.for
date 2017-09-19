c
C-------------------------------------------------------------------
C
      SUBROUTINE outrg
c
c
c
c _________________________________________________________
c	Program Description
c
c       Outrg; It prints Rio Grande Compact operational rule information
c               Note: 
c               (1) Monthly and annual data is printed to a binary 
c                   file (*.b66) by routine rgrg.for 
c               (2) The state summary is calculated herein.
c
c _________________________________________________________
c
c       Update History
c
c rrb 01/01/26; Revised state summary output to reflect a spill
c rrb 01/08/09; Revised state summary to reflect revised spill
c               treatment for surplus calculations.
c               Added average w/o spills
c
c _________________________________________________________
c	Documentation
c               numrg = 1 rio grande or conejos only
c                       2 both rio grande and conejos
c
c _________________________________________________________
c               Dimensions
      include 'common.inc'
      dimension rindex(100,2), roblig(100,2), rdel(100,2), 
     1          rsurp(100,2),  rspill(100,2)
      dimension criver(2)
      character criver*12
      data criver/
     1       'Rio Grande  ', 'Conejos     '/
c
c _________________________________________________________
c               Step 1; Print to screen and Log
c
      write(6,210) 
      write(io99,210) 
 210  format(/,72('_'),/,'  Subroutine Outrg')
c
c _________________________________________________________
c               Step 2; Initilize.  Note
c               nrg = # of values in binary rio grande file               
c               numrg = # of compact reservoirs
      nrg=23
      numrg=2
      small = 0.001
c
c     cu=1.0
      do i=1,13
        dummyd(i)=0.0
      end do

c     if(iresop.eq.2) cu=factor
c     if(iresop.eq.3) cu=factor*0.001
c
c _________________________________________________________
c               Step 3; Print header
      call outtop(52,1,36)
      do ip=1,numrg
          write(52,179) cunitm
          write(52,180)
c
c rrb 01/08/10; Variables qdebt and qdebtx now represent 
c               year of payback and initial condition
          write(52,181) qdebtx(1), 60000., qdebt(1),
     1                  qdebtx(2), 40000., qdebt(2),
     1                  qdebtx(1)+qdebtx(2), 100000.0, qdebt(1)

          iy1=0
c
c _________________________________________________________
c               Step 4; Begin Year Loop

          do iy=iystr, iyend
          write(52,182)
          call year(iy, iyrmo, imomo, cyr1)
          iy1=iy1+1

            do i=1,nrg
              dat1t(i)= 0.0
            end do
c
c _________________________________________________________
c               Step 5; Begin Month Loop
            do im=1,12
c             cx=cu
c             cx=cx
c             if(iresop.ne.1) cx=cu*mthday(im)
c
c _________________________________________________________
c               Step 6; Read monthly data
c
              irec1=((iy-iystr0)*13*numrg)+((im-1)*(numrg))+ip
c             write(io99,*) '  Outrg_1, irec1 = ', irec1    
              read(66,rec=irec1) (dat1(i),i=1,nrg)
c
c _________________________________________________________
c               Step 7; Convert Units and Total
              do i=1,nrg-2
c               dat1(i) =dat1(i)*cx
                dat1t(i)=dat1t(i) + dat1(i)
              end do
c
c _________________________________________________________
c               Step 8; Print monthly data
c
              write(52,160)  criver(ip), iyrmo(im), xmonam(im), 
     1                      (dat1(i), i=1,nrg)
c
c               End month loop
            end do
c
c _________________________________________________________
c               Step 9; Begin Annual Output

            write(52,150)
c
c _________________________________________________________
c               Step 10; Read annual data
c
            irec1=((iy-iystr0)*13*numrg)+(12*(numrg))+ip 
            read(66,rec=irec1) (dat1(i),i=1,nrg)
c
c _________________________________________________________
c               Step 11; Reset selected annual data to calculated
c                        values.  
c                        Note:
c                        dat1(18) = Demand
c                        dat1(19) = Divert
c                        date1(20) = Actual Delivery
            dat1(18) = dat1t(18) 
            dat1(19) = dat1t(19)
            dat1(20) = dat1t(20)
c
c _________________________________________________________
c               Step 12; Calculate Annual curtailment %
c                        dat1(21) = Annual Curtailment %
c                        dat1(7) = Total Obligation
c                        dat1(19) = Divert
            if(dat1(7) .gt.small) then
              dat1(21) = dat1t(19) / dat1(7) * 100.0
            else
              dat1(21) = 0.0
            endif
c
c _________________________________________________________
c               Step 13; Print Annual Data
c
            write(52,160) criver(ip), iyrmo(13), xmonam(13), 
     1                   (dat1(i), i=1,nrg)
c
c _________________________________________________________
c               Step 14; Set State Summary Variables Note:
c                       dat1(7) = Total index
c                       dat1(8) = Obligaton
c                       dat1(16)= Surplus Spill
c                       dat1(20)= Actual Delivery
c                       dat1(23)= Spill
c
            rindex(iy1,ip) = dat1(7)
            roblig(iy1,ip) = dat1(8)
            rdel(iy1,ip)   = dat1(20)
            rspill(iy1,ip) = dat1(23)
c
c
c _________________________________________________________ 
c               Step 15; Calculate Annual Surplus for State Summary.
c                        Note the annual value read is cumulative, 
c                        not annual.
c                        dat1(16) = annual surplus, 
c                        dat1(23) = spill code
c
c rrb 01/08/10; Use annual results 
            if(dat1(23).le.small) then
              rsurp(iy1,ip)  = rdel(iy1,ip) - roblig(iy1,ip) + 5000.0
            else
              rsurp(iy1,ip) = dat1(16)
            endif
c
c               End year loop
          end do
c
c               End station loop
        end do
c
c _________________________________________________________
c               Step 16; Begin State Summary Output
        iy1=0
        iy2=0
        iysp=0
c
c rrb 01/08/10; Initilize cumulative to initial conditions
c       ctot = 0.0
        ctot = qdebtx(1) + qdebtx(2)
        iprint = 0

        do iy=iystr,iyend
          iy1=iy1+1
          nrgspil=0
c
c               Annual total and average for years with data only
          if(rindex(iy1,1).gt.0.001) then
c
c
c _________________________________________________________
c               Step 17; Print State Summary Title
c
            if(iprint.eq.0) then
              write(52,184) cunitm
              write(52,181) qdebtx(1), 60000., qdebt(1),
     1                      qdebtx(2), 40000., qdebt(2),
     1                      qdebtx(1)+qdebtx(2), 100000.0, qdebt(1)
              write(52,185)
            endif

            iprint=1
            iy2=iy2+1
c
c _________________________________________________________
c               Step 18; Calculate cumulative Surplus
c                        Note reset cululative to zero if a spill
c
            if(rspill(iy1,1).gt.small) then
              ctot=0.0
              nrgspil=1
            endif
            ctot=ctot+rsurp(iy1,1) + rsurp(iy1,2)
            ctot = amin1(ctot, 100000.)
c
c _________________________________________________________
c               Step 19; Print State Summary - Annual
c
            write(52,186) iy, 
     1      rindex(iy1,1), roblig(iy1,1), rdel(iy1,1), rsurp(iy1,1),
     1      rindex(iy1,2), roblig(iy1,2), rdel(iy1,2), rsurp(iy1,2),  
     1      roblig(iy1,1)+ roblig(iy1,2),
     1      rdel(iy1,1)  + rdel(iy1,2), 
     1      rsurp(iy1,1) + rsurp(iy1,2), ctot, rspill(iy1,1)
c
c _________________________________________________________ 
c               Step 20; Calculate Average State Summary
c                        for all years
c
            dummyd(1) = dummyd(1) + rindex(iy1,1)
            dummyd(2) = dummyd(2) + roblig(iy1,1)
            dummyd(3) = dummyd(3) + rdel(iy1,1)
            dummyd(4) = dummyd(4) + rsurp(iy1,1)

            dummyd(5) = dummyd(5) + rindex(iy1,2)
            dummyd(6) = dummyd(6) + roblig(iy1,2)
            dummyd(7) = dummyd(7) + rdel(iy1,2)
            dummyd(8) = dummyd(8) + rsurp(iy1,2)
         

            dummyd(9)  = dummyd(9)  + roblig(iy1,1)  + roblig(iy1,2)
            dummyd(10) = dummyd(10) + rdel(iy1,1)    + rdel(iy1,2)
            dummyd(11) = dummyd(11) + rsurp(iy1,1)   + rsurp(iy1,2)
c            
            dummyd(12) = dummyd(12) + ctot
            dummyd(13) = dummyd(13) - 1.0
c
c rrb 01/08/10; 
c _________________________________________________________ 
c               Step 21; Calculate Average State Summary
c                        for years without Spill
c
            if(nrgspil.eq.0) then
              iysp=iysp+1
              dumsta(1) = dumsta(1) + rindex(iy1,1)
              dumsta(2) = dumsta(2) + roblig(iy1,1)
              dumsta(3) = dumsta(3) + rdel(iy1,1)
              dumsta(4) = dumsta(4) + rsurp(iy1,1)

              dumsta(5) = dumsta(5) + rindex(iy1,2)
              dumsta(6) = dumsta(6) + roblig(iy1,2)
              dumsta(7) = dumsta(7) + rdel(iy1,2)
              dumsta(8) = dumsta(8) + rsurp(iy1,2)
         

              dumsta(9)  = dumsta(9)  + roblig(iy1,1) + roblig(iy1,2)
              dumsta(10) = dumsta(10) + rdel(iy1,1)   + rdel(iy1,2)
              dumsta(11) = dumsta(11) + rsurp(iy1,1)  + rsurp(iy1,2)
c            
              dumsta(12) = dumsta(12) + ctot
              dumsta(13) = dumsta(13) - 1.0
            endif
          endif
        end do
c
c _________________________________________________________
c               Step 22; Print State Summary Annual Average
c                        for all years
c
        if(iy2.gt.0) then
          write(52,188)
          c=float(iy2)
          write(52,189) (dummyd(i)/c, i=1,13)
        endif
c
c _________________________________________________________
c               Step 23; Print State Summary Annual Average
c                        for years without a spill
c
        if(iysp.gt.0) then
          write(52,*) ' '
          write(52,188)
          c=float(iysp)
          write(52,191) (dumsta(i)/c, i=1,13)  
        endif
        write(52,190)       
c
c _________________________________________________________
c               Step 22; Return
c
        return
c
c _________________________________________________________
c               Formats
c
  140  format (/,'Rio Grande Compact ',/)
  150  format ('____________ ____ ____',23(' ________'))
  160  format(a12, i5, 2x, a3, 22f9.0, f9.2)
  170  format(a12, i5, 5x, 25f9.0)

  179  format(''/,' Rio Grande Compact (*.xrg) ',a5,/,
     1    '    Where monthly data is:',/
     1    '    (1)  River Basin is Rio Grande or Conejos',/
     1    '    (2)  Yr is the year',/
     1    '    (3)  Mo is the month',//
     1    '    (4)  Forecast is for season (April - Sept).',/
     1    '    (6)  Apr-Mo1 is Apr to current month - 1 for',
     1            ' the forecast season (April - Sept.)',/
     1    '    (7)  AdjFor is adjusted forecast (7=4-6)',/
     1    '    (8)  Oct is estimate until actual is known (Nov)',/    
     1    '    (9)  Nov-Dec is estimate until actual is known',
     1             ' (Nov is known in Dec)',/
     1    '    (10) Total is sum (10=5+6+7+8+9)',/
     1    ' ',/
     1    '    (11) Obligation is annual obligation via Article III',/
     1    ' ',/
     1    '    (12) Jan-Mar to delivery Jan-Mar',/   
     1    '    (13) Apr-Mo1 is delivery April to current month - 1',/
     1    '    (14) Need is the amount required to meet the annual',
     1             ' Obligation for the rest of the year',/
     1    '         If Year < Payback Year the Surplus (19) is not', 
     1             ' included (14=max(0.0, 11-12-13-15-16-17-18))',/
     1    '         If Year >= Payback Year the Surplus (19) is', 
     1             ' included (14=max(0.0, 11-12-13-15-16-17-18-19))',/

     1    '    (15) CBP is closed basin project contribution to', 
     1             ' this basin',/
     1    '    (16) Nov-Dec is an estimated inflow',/
     1    '    (17) Norton is estimated Norton drain to this basin',/
     1    '    (18) Paper is paper credit to this basin',/
     1    '    (19) Surplus is carryover from prior year',/
     1    '    (20) Total if Year < Payback Year Surplus (19) is not', 
     1             ' included (20=12+13+14+15+16+17+18); ',/
     1    '         Total if Year >= Payback Year Surplus (19) is', 
     1             ' included (20=12+13+14+15+16+17+18+19)',//

     1    '    (21) Demand for Jan - Oct is the (Need)/',
     1            ' (# of Months to and including October)',/ 
     1    '         Demand for Nov and Dec is the (Need)',/
     1    '    (22) Divert is the portion of the Demand controlled',
     1            ' by compact demand.',/ 
     1    '         Note-1 Divert (21) may exceed the Demand (21)', 
     1            ' because it is the maximum from every iteration.',/ 
     1    '         Note-2 Divert (21) may be less than the', 
     1            ' Demand (21) if water is unavailable to the', 
     1            ' compact demand',/  
     1    '    (23) Actual Delivery for Rio Grande is RG @Labatos', 
     1            ' less Conejos at Mouths (sum of branches at', 
     1            ' La Sauses)',/ 
     1    '         Note-3 Actual Delivery (23) may be less than', 
     1            ' Divert(22) for the Rio Grande since Actual ',/
     1    '         Deilvery is RG @ Labatos - Conejos at Mouths',
     1            ' (e.g. Conejos supplies some of diverted water)',/
     1    '    (23) Actual Delivery for Conejos is Conejos at', 
     1            ' Mouths (sum of branches at La Sauses)',/
     1    '    (24) Max Curtail% is min((Need/(AdjFor+Oct))',
     1            ' *100, 100)') 

 180   format(
     1    '    (25) Payback Year is the year the (14) Need calcs.',
     1            ' include the Surplus (19) value.',/
     1    '    (26) Spill is historic spill where the integer is the',
     1            ' month of spill and decimal is % of spilled water', 
     1            ' owned by Colorado',//
     1    '    Annual data is similar except:',/   
     1    '    (19) Surplus is cumulative carryover to this year.',/     
     1    '         If there was no spill at E. Butte then Surplus =', 
     1            ' Prior Surplus + Actual Delivery (25) -',
     1            ' Obligation (11) + Paper Credit (19)',/
     1    '         If there was a spill at E. Butte and CO was in', 
     1            ' debt then Surplus = 0',/
     1    '         If there was a spill at E. Butte and CO was in',
     1            ' surplus then Surplus = Prior Surplus *',
     1            ' portion of spill that was a Colorado Credit',/
     1    '    (24) Max Curtail% is (Total Divert/Total Index)*100')

 181      format(/
     1    '    Operational data:',/
     1    '         Rio Grande: Initial Surplus/Shortage = ', f8.0,
     1            ' Max Deficit = ',f8.0,
     1            ' Year Payback Begins = ',f8.0,/
     1    '         Conejos:    Initial Surplus/Shortage = ', f8.0,   
     1            ' Max Deficit = ',f8.0,
     1            ' Year Payback Begins = ',f8.0,/ 
     1    '         Total:      Initial Surplus/Shortage = ', f8.0,
     1            ' Max Deficit = ',f8.0,
     1            ' Year Payback Begins = ',f8.0)      
 182      format(//
     1    45x, ' Index Supply Calculations', 
     1    54x, 'Delivery Calculations',/
     1    21x,'          ', 1x, 53('_'), 10x,80('_'),18x,
     1    '   Actual      Max  Payback'/ 
     1    'River Basin    Yr   Mo',
     1    '  Forcast  Jan-Mar  Apr-Mo1   AdjFor',
     1    '      Oct  Nov-Dec    Total',
     1    '    Oblig  Jan-Mar  Apr-Mo1     Need',
     1    '      CBP  Nov-Dec   Norton    Paper',
     1    '  Surplus    Total   Demand   Divert Delivery', 
     1    ' Curtail%     Year    Spill',/
     1    '(1)           (2)  (3)',     
     1    '      (4)      (5)      (6)      (7)',
     1    '      (8)      (9)     (10)',
     1    '     (11)     (12)     (13)     (14)',      
     1    '     (15)     (16)     (17)     (18)'  
     1    '     (19)     (20)     (21)     (22)     (23)',      
     1    '     (24)     (25)     (26)',/  
     1    '____________ ____ ____', 23(' ________'))
c
c               State Summary
 184  Format(''//
     1 '    Rio Grande Compact (*.xrg) ',a5,/,
     1 '    State Summary (years with data only)')

 185  format(//
     1 '     ',
     1 '                Rio Grande             ',                 
     1 '                 Conejos               ',  
     1 '                  Total                ',/
     1 '      ',
     1 '_______________________________________ ', 
     1 '_______________________________________ ',
     1 '_______________________________________ ',/
     1 '     ',   
     1 '     Index            Delivery    Annual',   
     1 '     Index            Delivery    Annual', 
     1 '     Total  Delivery    Annual Cumulative',/
     1 '     ', 
     1 ' RioGrande    Oblig-   Adj. RG  Surplus-',  
     1 '  Conejos+    Oblig- Conejos @  Surplus-',    
     1 '    Oblig- RioGrande  Surplus-  Surplus-',/
     1 ' Year', 
     1 ' @DelNorte     ation @ Labatos Short (1)',    
     1 '    RSA+LP     ation La Sauses Short (1)',     
     1 '     ation @ Labatos Short (2) Short (3) Spill (4)',/
     1 ' ____',  
     1 ' _________ _________ _________ _________', 
     1 ' _________ _________ _________ _________', 
     1 ' _________ _________ _________ _________ _________') 
 186   format(i5, 12f10.0, f10.2)                  
 188   format(
     1 ' ____',  
     1 ' _________ _________ _________ _________', 
     1 ' _________ _________ _________ _________', 
     1 ' _________ _________ _________ _________ _________') 
 189   format(
     1 'Ave(5)', f9.0, 11f10.0, f10.2)
 191   format(
     1 'Ave(6)', f9.0, 11f10.0, f10.2)

 190   format(/, 
     1  ' (1) Annual Surplus-Short is 0 in a spill year or',
     1  ' Obligation - Delivery +  5,000 paper.',
     1  ' Positives are surpluses, negatives are shortages.',/
     1  ' (2) Annual Surplus-Short is 0 in a spill year or', 
     1  ' Obligation - Delivery + 10,000 paper.'
     1  ' Positives are surpluses, negatives are shortages.'/
     1  ' (3) Cumulative Surplus-Short is 0 in a spill year or',
     1  ' the cumulation of Initial plus Annual Surplus-Short.',/ 
     1  ' (4) Spill indicates a spill at Elephant Butte.  The integer',
     1  ' value is the month of spill, the decimal value is the %',/ 
     1  '     of any surplus owned by Colorado that spilled',/
     1  ' (5) Average for years with compact operation',/
     1  ' (6) Average for years with compact operation w/o a spill')     
c
c _________________________________________________________
c
       end                                           
