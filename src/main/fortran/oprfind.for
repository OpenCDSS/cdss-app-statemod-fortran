c
c**********************************************************
c
c  
c rrb 2011/07/28; Revised to pass in ioprsw1 to turn off the
c                 operating rule if the source or destination
c                 is off
          subroutine Oprfind(ityopr1, itype, idumx,k,ion,iprinto,
     1      iops1, iops2, nx, cx, iacc, istop, rops2, ioprsw1, cidvri)
c
c
c _________________________________________________________
c	Program Description
c
c       Oprfind; it reads various operational right data
c                called by OprInp.f
c
c	Same as OprFind but revised to recognize command line
c	arguements
c
c
c _________________________________________________________
c	Update History
c	
c       2008/09/22; Revise to check on/off switch read 
c		                (e.g. idvrsw1 not idvrsw) in order
c		                to allow more than one operating rule
c		                to refrence a water right
c       2008/09/10; Revise to allow the number of structures
c		                to be zero to allow sequential searches.
c	2006/03/21; Revise to all Well Right Special 
c                   (no ..W or ..W.01)
c	2004/09/20; Revise to allow a diversion and a water right
c		                 owner to be > 1 (e.g. iops2=iops2)
c
c _________________________________________________________
c       Documentaiton               
c
c               ion=-1 means do not check right on/off switch
c               ion= 0 means do not turn off opr right 
c               ion= 1 means turn off source right (controlled by
c		                an operating rule
c
c		            iacc=1 Check the account varaible (iops2) > 0
c		            iacc=0 Do not check the account variable
c		                   (e.g. for type 24 and 25 it is ownership %)
c               
c		            istop=0 Stop if a structure is not found
c		            istop=1 Do not stop if a structure is not found
c		            istop=-1 Do not stop if a structure is not found
c		            	       But print warning
c		   
c               ityopr1 If called by Oprinp Operational rule type
c                       If called by other, -1
c		            rops2   Source 2 as a Real value (see type 25)
c
c               itype = 0       find stream struture
c                       1       find ISF structure
c                       2       find reservoir structure
c                       3       find diversion structure
c                       6       find well
c			                  7       find a plan
c
c                       11      find isf right ID
c                       12      find res right ID
c                       13      find div right ID
c                       14      find opr right ID
c                       15      find special opr right ID
c                       16      find well right ID
c                       17      find well right ID special
c				                        (e.g. no ..W or ..W.01)
c
c                       20      read monthly switches
c                       21      read intervening structures
c		                    22      read monthly and annual maxima
c			                  23      read intervening structures with loss %
c			                  24      read Operating Rule ID for a
c		                              monthly or annual plan adjustment
c			                  25      read multiple destinations and percent
c			                  26      read 12 efficiency values (for a T&C
c                                 obligation)
c _________________________________________________________
c	Dimensions
c                       
        include 'common.inc'
        character cidvri*12, cx*12, rec12*12, rec12X*12, rec132*132, 
     1    rec1*1
c
c _________________________________________________________
c
c		Detailed output switch
c		iout=0 none, iout=1 detailed
c		ioutP=0 No plan output, 1 Plan output itype 7 and 25
c		ioutR=0 No reservoir output, 1 Reservoir output itype 2
c		iecho=0 Do not  print data read to *.chk
c		iecho=1 do print data read to *.chk
c
        iout=0
        ioutP=0
        ioutR=0
c        
        iecho=1
        small=0.001
        
        
        iops1=0
        nx=0
        
        if(iout.eq.1) then
          write(nlog,*) 
          write(nlog,*) '___________________________________________'
          write(nlog,*) ' Oprfind; itype = ', itype
        endif  

        select case(itype)
c
c               Type 0; stream gage
c _________________________________________________________
c
        case(0)
          do is =1,numsta                                              
            if (cstaid(is).eq.cx) then
              iops1 = is
              goto 500
            endif
          end do
          
          if(istop.eq.1) then
            goto 500
          endif

          nx=-1
          write(nlog,100) cidvri, cx 
          goto 9999
c       endif
c _________________________________________________________ 
c
c
c               Type 1; instream flow station
         case(1)
cx        if(numifr.eq.0) istop=0
c
c ---------------------------------------------------------
          do nx =1,numifr                                             
            if (cifrid(nx).eq.cx) then
              iops1 = nx
c  
c rrb 00/11/02; Check isf destination account is set proprely               
              if(iops2.ne.1) goto 924
              iops2 = 1
c
c ---------------------------------------------------------
c		            Warn if the station is off                            
              if(ifrrsw(nx).eq.0) then   
c
c rrb 2011/07/28; Update             
                ioprsw1=0
                write(nlog,182) cidvri, ityopr1, itype, 'Isf', cx 
              endif  
c
c ---------------------------------------------------------
c		Detailed Output              
              if(iout.eq.1) then
                write(nlog,180) cidvri, ityopr1, itype, 
     1            'Isf', cx, 'Isf', iops1
              endif                
              goto 500
            endif
          end do
c
c               Print problem could not find isf station
          nx=-1
          if(istop.eq.0) then
            write(nlog,170) 'Problem', cidvri, 
     1       'Instream', cx, 'Instream', '(*.ifs)', numifr            
            goto 9999
          endif  
c
c _________________________________________________________ 
c
c               Type 2; reservoir station
        case(2)
cx        if(Numres.eq.0) istop=0
c
c ---------------------------------------------------------
c         
       
          do nx =1,numres                                             
            if(ioutR.gt.0) 
     1        write(nlog,*) ' Oprfind; ',  numres, nx, cx, cresid(nx)
            if (cresid(nx).eq.cx) then
              iops1=nx
c  
              n1=nowner(nx+1)-nowner(nx)
              if(iacc.eq.1) then
c
c rrb 2006/08/18; Allow the account value to be negative
c		              in order to identify the first n accounts
c               if(iops2.gt.n1 .or. iops2.eq.0) then
                if(iabs(iops2).gt.n1 .or. iops2.eq.0) then
                  write(nlog,920) cidvri, iops2
                  goto 9999
                endif
              endif  
                
              iops2=iops2
c
c ---------------------------------------------------------

c		Warn if the station is off                            
              if(iressw(nx).eq.0) then   
c                        
c rrb 2011/07/28; Update 
                ioprsw1=0              
                write(nlog,182) cidvri, ityopr1, itype, 'Res', cx 
              endif  
c
c ---------------------------------------------------------
c		Detailed Output              
              if(iout.eq.1 .or. ioutR.eq.1) then
                write(nlog,180) cidvri, ityopr1, itype, 
     1            'Res', cx, 'Res', iops1, numres
              endif  
              goto 500
            endif
          end do
c                                                        
c               Print problem could not find reservoir station
          nx=-1
          if(istop.eq.0) then
            write(nlog,170) 'Problem', cidvri, 
     1       'Reservoir', cx, 'Reservoir', '(*.res)', numres
            goto 9999
          endif  
c       endif
c _________________________________________________________ 
c
c
c               Type 3; Diversion Station
        case(3)
cx        if(numdiv.eq.0) istop=0
          ifound=0
c
c ---------------------------------------------------------
          do nx =1,numdiv                                              
            if (cdivid(nx).eq.cx) then
              iops1=nx
c  
              if(iacc.eq.1 .and. iops2.ne.1) goto 924
              
c
c rrb 04/09/20; Allow owner to be > 1              
c             iops2=1
              iops2=iops2
c
c ---------------------------------------------------------
c		Warn if the station is off              
              if(idivsw(nx).eq.0) then
c                        
c rrb 2011/07/28; Update 
                ioprsw1=0              
                write(nlog,182) cidvri, ityopr1, itype, 'Div', cx 
              endif  
              
              if(iout.eq.1) then
                write(nlog,180) cidvri, ityopr1, itype,
     1            'Div', cx, 'Div', iops1, numdiv
              endif                
              
              goto 500
            endif
          end do
c
c ---------------------------------------------------------
c               Print problem could not find div station
          nx=-1
          if(istop.eq.0) then
            write(nlog,170) 'Problem', cidvri, 
     1       'Diversion', cx, 'Diversion', '(*.dds)', numdiv
            goto 9999
          else
c
c ---------------------------------------------------------
c		Detailed Output                      
            if(iout.eq.1) then
              write(nlog,180) cidvri,ityopr1, itype,
     1        'Div-Station',cx, 'Div-Station',iops1, numdiv
            endif           
          endif
c
c _________________________________________________________ 
c
c               Type 6; Well Station
c
        case(6)
cx        if(numdivw.eq.0) istop=0
c
c ---------------------------------------------------------
          do nx =1,numdivw                                              
            if (cdividw(nx).eq.cx) then
              iops1=nx
c  
c rrb 00/11/02; Check well destination account is set proprely               
              if(iacc.eq.1 .and. iops2.ne.1) goto 924
              iops2=1
c
c ---------------------------------------------------------

c		Warn if the station is off                            
              if(idivsww(nx).eq.0) then
c                        
c rrb 2011/07/28; Update 
                ioprsw1=0              
                write(nlog,182) cidvri, ityopr1, itype, 'Res', cx 
              endif  
              
              if(iout.eq.1) then
                write(nlog,180) cidvri, ityopr1, itype, 
     1            'Well', cx, 'Well',iops1, numdivw
              endif                
              goto 500
            endif
          end do
c
c ---------------------------------------------------------
c                                                        
c               Print problem could not find well station
          nx=-1
          if(istop.eq.-1) then          
            write(nlog,170) 'Problem', cidvri, 
     1       'Well', cx, 'Well', '(*.wel)', numdivw
            goto 500
          endif
c
c ---------------------------------------------------------
c          
          if(istop.eq.0) then          
            write(nlog,170) 'Problem', cidvri, 
     1       'Well', cx, 'Well', '(*.wel)', numdivw
            goto 9999            
          endif

c _________________________________________________________ 
c
c
c               Type 7; Plan
        case(7)
cx        if(nplan.eq.0) istop=0
cx        if(nplan.eq.0) istop=1
c
c ---------------------------------------------------------
c         loop through all the plans
          do nx =1,nplan
c           debugging output
            if(ioutP.eq.1 .or. iout.eq.1)
     1        write(nlog,*) ' Oprfind; ',nx, cx, pid(nx)
c           check to see if this plan id matches the given structure id
            if (pid(nx).eq.cx) then
c             found a match, it's a plan, save the plan index
              iops1=nx
c
c rrb 2009/01/15; Set the default plan account = 1 when the account
c		              check variable (iacc=1). When iacc=0 the account
c		              is allowed represent the month the operatinal limits 
c		              are reset for a type 47 rule.              
              if(iacc.eq.1) iops2=1
c
c ---------------------------------------------------------
c		Warn if the plan is off                            
              if(ifix(pon(nx)).eq.0) then
c                        
c rrb 2011/07/28; Update 
                ioprsw1=0              
                write(nlog,182) cidvri, ityopr1, itype, 'Plan', cx 
              endif  
c
c ---------------------------------------------------------
c              
              if(iout.eq.1 .or. ioutP.eq.1) then
                write(nlog,180) cidvri, itype, ityopr1, 
     1            'Plan', cx, 'Plan', iops1, nplan
              endif  
              goto 500
            endif
          end do
c
c ---------------------------------------------------------
                                                        
c               Print problem could not find a Plan
          nx=-1
          if(istop.eq.0) then
            write(nlog,170) ' Problem', cidvri,
     1         'Plan', cx, 'Plan', '(*.pln)', nplan
            goto 9999
          else
cx            write(nlog,170) ' Warning', cidvri,
cx     1         'Plan', cx, 'Plan', '(*.pln)', nplan
            goto 500
          endif
          
c
c _________________________________________________________ 
c
c               Type 11; Instream Right
c
        case(11)
cx        if(NumFrr.eq.0) istop=0
          
          do nx =1,numfrr
            if (cisfwr(nx).eq.cx) then
c             write(nlog,*) '  Oprfind; nx = ', nx
              iops1=nx
c
c ---------------------------------------------------------
c     
c               Turn off Opr right if source water right is off
              if(ion.ne.-1) then
                if(iifrsw1(nx).eq.0) then
                  ioprsw(k) = 0
                  if(iprinto.eq.0) write(nlog,300)
                  
                  iprinto=iprinto+1
                  write(nlog,302) iprinto, ityopr1, cidvri, nameo(k), 
     1              cx, iifrsw1(nx), nx, cidvri
                endif
              endif
c

c ---------------------------------------------------------
c rrb 04/09/20; turn off source right, it is now controlled by an
c		            operating right                            
              if(ion.eq.1) then
                iifrsw(nx)=0
              endif
c
c ---------------------------------------------------------
c		Detailed Output                      
              if(iout.eq.1) then
                write(nlog,180) cidvri,ityopr1, itype,
     1           'Isf-Right',cx, 'Isf-Right',iops1, numfrr
              endif 
              
              goto 500
            endif
          end do
c
c ---------------------------------------------------------
                                                        
c               Print problem could not find isf right
          nx=-1
          if(istop.eq.0) then
            write(nlog,170) 'Problem', cidvri, 
     1       'Isf-Right', cx, 'Isf-Right', '(*.ifr)', numfrr
     
            goto 9999
          endif
          
          
          
c
c _________________________________________________________ 
c
c               Type 12; Reservoir Right
c
        case(12)
cx        if(numrsr.eq.0) istop=0
          if(cidvri(1:7).eq.'Opr_OOP') then
c           write(nlog,*) 
c           write(nlog,*) ' Oprfind; ', cidvri, nameo(k), cx
          endif
          
c
c ---------------------------------------------------------
c          
          do nx =1,numrsr
            if (creswr(nx).eq.cx) then
c             write(nlog,*) '  Oprfind; nx = ', nx
              iops1=nx
c
c     
c
c ---------------------------------------------------------
c               Turn off Opr right if source water right is off
c		            Either way turn off the source right; it is now 
c		            controlled by an operating rule
              if(ion.ne.-1) then
                if(irsrsw1(nx).eq.0) then
                  ioprsw(k) = 0
cr                if(iprinto.eq.0) write(nlog,1281)
                  if(iprinto.eq.0) write(nlog,300)
                  
                  iprinto=iprinto+1
                  write(nlog,302) iprinto, ityopr1, cidvri, nameo(k), 
     1              cx, irsrsw1(nx), nx, cidvri
                endif
              endif
c
c ---------------------------------------------------------
c
              if(ion.eq.1) then
                irsrsw(nx)=0
                if(iprinto.eq.0) write(nlog,300)
                iprinto=iprinto+1                
                write(nlog,302) iprinto, ityopr1, cidvri, nameo(k),
     1              cx, irsrsw(nx), nx, cx
              endif
              
              if(iout.eq.1) then
                write(nlog,*) '  Oprfind; dcrres(nx) ', dcrres(nx)
                write(nlog,180) cidvri, ityopr1, itype,
     1            'Res-Right',cx, 'Res-Right',iops1, numrsr
              endif 
              
              goto 500
            endif
          end do
c                                                        
c ---------------------------------------------------------
c               Print problem could not find div station
          nx=-1
          if(istop.eq.0) then
            write(nlog,170) 'Problem', cidvri, 
     1       'Res-Right', cx, 'Res-Right', '(*.rer)', numrsr
     
            goto 9999
          endif
c
c _________________________________________________________ 
c
c               Type 13; Diversion Right
c
        case(13)
cx        if(numdvr.eq.0) istop=0
          do nx =1,numdvr
            if (crigid(nx).eq.cx) then
c             write(nlog,*) '  Oprfind; nx = ', nx
              iops1=nx
c
c ---------------------------------------------------------
c
c		Test account number if on              
              if(iacc.eq.1 .and. iops2.eq.0) goto 926
c
c rrb 04/09/20; Source 2 is source water right %
              iops2=iops2
c
c ---------------------------------------------------------
     
              if(idvrsw1(nx).eq.0) then
                ioprsw(k) = 0
cr              if(iprinto.eq.0) write(nlog,1281)
                if(iprinto.eq.0) write(nlog,300)
                
                iprinto=iprinto+1
                write(nlog,302) iprinto, ityopr1, cidvri, nameo(k),
     1              cx, idvrsw1(nx), nx, cidvri
              endif
c
c
c ---------------------------------------------------------
c rrb 04/09/20; turn off source right, it is now controlled by an
c		operating right  
              if(ion.eq.1) then
                idvrsw(nx)=0
                
                if(iprinto.eq.0) write(nlog,300)
                iprinto=iprinto+1                
                write(nlog,302) iprinto, ityopr1, cidvri, nameo(k),
     1              cx, idvrsw(nx), nx, cx
              endif
c
c ---------------------------------------------------------
c		Detailed Output              
              if(iout.eq.1) then
                write(nlog,180) cidvri,ityopr1, itype,
     1            'Div-Right',cx, 'Div-Right',iops1, numdvr
              endif  
              
              goto 500
            endif
          end do
c
c ---------------------------------------------------------
c                                                        
c               Print problem could not find div station
          nx=-1
          if(istop.eq.0) then
            write(nlog,170) 'Problem', cidvri, 
     1       'D-Right', cx, 'D-Right', '(*.ddr)', numdvr
     
            goto 9999
          endif
          
          
c
c _________________________________________________________ 
c
c               Type 14; Simple Operating Right
        case(14)
          if(iout.eq.1) write(nlog,*) ' Oprfind; cx, k ', cx, k
          ifound=0
          do nx=1, k-1
c           write(nlog,*) ' Oprfind; ', nx, corid(nx), cx
            if(corid(nx).eq.cx) then
              iops1 = nx
c
c ---------------------------------------------------------

c		   Turn off right if the source right is off
              if(ion.eq.1 .and. ioprsw(nx).eq.0) then
                ioprsw(k) = 0
                if(iprinto.eq.0) write(nlog,300)
                
                iprinto=iprinto+1
                write(nlog,*) iprinto, cidvri, nameo(k),
     1              cx, corid(nx), nx
                write(nlog,*) iprinto, cidvri, nameo(k),
     1              cx, corid(nx), nx
                write(nlog,302) iprinto, ityopr1, cidvri, nameo(k),
     1              cx, corid(nx), nx, cidvri
              endif
c
c ---------------------------------------------------------
c		Detailed Output                
              if(iout.eq.1) then
                write(nlog,180) cidvri, ityopr1, itype,
     1            'Opr', cx, 'Opr',iops1, k
              endif  
              goto 500
                
            endif
          end do
c
c               Warn user source water right or opr rule not found
          nx=-1
          if(ifound.eq.0 .and. istop.eq.0) then
            write(nlog,1500) 'Problem', cidvri, cx
            goto 9999
          endif
c
c _________________________________________________________ 
c
c               Type 15; Special Operating Right
        case(15)
c                  Determine if source 1 is an Operating Right
c                  Note to do DFS for a carrier the source water
c                  right is off once it tied to a carrier.
c                  Therefore must get Opr rule for carrier to
c                  determine if the source water right is on
c
c ---------------------------------------------------------
c
            do nx=1, k-1
              if((corid(nx).eq.cx .and. ityopr(nx).eq.11) .or.
     1          (corid(nx).eq.cx .and. ityopr(nx).eq.19)) then
                ndr=iopsou(1,nx)*(-1)
c                 nd1=idivco(1,ndr)
c                 iscd=idvsta(nd1)
                iops1 = ndr
                iops2 = 1
c
c
c ---------------------------------------------------------
c               Turn off Opr right if source opr right is off
                if(ion.le.0) then
cr                goto 500
                else
                  if(ioprsw(nx).eq.0) then
                    ioprsw(k) = 0
                    if(iprinto.eq.0) write(nlog,300)
                    
                    iprinto=iprinto+1
                    write(nlog,302) iprinto, ityopr1, cidvri, nameo(k),
     1                cx, ioprsw(nx), nx, cidvri                    
                  endif
cr                goto 500
                endif
c
c ---------------------------------------------------------
c		Detailed Output
                if(iout.eq.1) then
                  write(nlog,180) cidvri, ityopr1, itype,
     1              'Opr', cx, 'Opr',iops1, k
                endif  
                goto 500
                
              endif
            end do
c
c ---------------------------------------------------------
c               Warn user source water right or opr rule not found
          nx=-1
          if(ifound.eq.0 .and. istop.eq.0) then
            write(nlog,1500) 'Problem', cidvri, cx
            goto 9999
          endif
c
c _________________________________________________________ 
c
c               Type 16; Well Right
c
        case(16)
cx        if(numdvrw.eq.0) istop=0
c
c ---------------------------------------------------------
          do nx =1,numdvrw
            if (crigidw(nx).eq.cx) then
              iops1=nx
c
c		Test account number if on              
              if(iacc.eq.1 .and. iops2.eq.0) goto 926
c
c rrb 04/09/20; Source 2 is source water right %
              iops2=iops2
c     
c ---------------------------------------------------------
c               Turn off Opr right if source water right is off
c		            Either way turn off the source right; it is now 
c		            controlled by an operating rule

              if(idvrsww1(nx).eq.0) then
                ioprsw(k) = 0
cr              if(iprinto.eq.0) write(nlog,1281)
                if(iprinto.eq.0) write(nlog,300)
                
                iprinto=iprinto+1
                write(nlog,302) iprinto, ityopr1, cidvri, nameo(k),
     1              cx, idvrsww1(nx), nx, cidvri
                
              endif
c
c ---------------------------------------------------------
c rrb 04/09/20; turn off source right, it is now controlled by an
c		            operating right                            
              if(ion.eq.1) then
                idvrsww(nx)=0
              endif
              
              if(iout.eq.1) then
                write(nlog,180) cidvri,ityopr1, itype,
     1           'Wel-Right',cx, 'Wel-Right',iops1, numdvrw
              endif  
              
              goto 500
            endif
          end do
c                                                        
c ---------------------------------------------------------
c               Print problem could not find well right
          nx=-1
          if(istop.eq.-1) then
            write(nlog,170) 'Problem', cidvri, 
     1       'W-Right', cx, 'W-Right', '(*.wer)', numdvr     
            goto 500            
          endif
          
          if(istop.eq.0) then
            write(nlog,170) 'Problem', cidvri, 
     1       'W-Right', cx, 'W-Right', '(*.wer)', numdvr     
            goto 9999
          endif
c
c rrb 2010/02/05; Warning          
          if(istop.eq.1) then
            write(nlog,170) 'Problem', cidvri, 
     1       'W-Right', cx, 'W-Right', '(*.wer)', numdvr
            write(nlog,*) ' Oprinp; Simulation allowed to continue'     
cx            goto 9999
          endif          
c
c _________________________________________________________ 
c
c               Type 17; Well Right Special (..W or ..W.01)
c
        case(17)
cx        if(numdvrw.eq.0) istop=0
c
c ---------------------------------------------------------
          
          do nx =1,numdvrw
c
c		Remove any ..W or ..W.01 befor the test          
            rec12=' '
            rec12X=crigidw(nx)
            ifound=0
            do i=1,12
              if(rec12X(i:i).eq.'W' .or. rec12X(i:i).eq.'.') ifound=1
              if(ifound.eq.0) rec12(i:i) = rec12X(i:i)
            end do  
c
c ---------------------------------------------------------
            if (rec12.eq.cx) then
              iops1=nx
c
c		Test account number if on              
              if(iacc.eq.1 .and. iops2.eq.0) goto 926
c
c rrb 04/09/20; Source 2 is source water right %
              iops2=iops2
c     
c               Turn off Opr right if source water right is off
c		            Either way turn off the source right; it is now 
c		            controlled by an operating rule
c
c ---------------------------------------------------------
              if(idvrsww1(nx).eq.0) then
                ioprsw(k) = 0
cr              if(iprinto.eq.0) write(nlog,1281)
                if(iprinto.eq.0) write(nlog,300)
                
                iprinto=iprinto+1
                write(nlog,302) iprinto, ityopr1, cidvri, nameo(k),
     1              cx, idvrsww1(nx), nx, cidvri
                
              endif
c
c ---------------------------------------------------------
c rrb 04/09/20; turn off source right, it is now controlled by an
c		operating right                            
              if(ion.eq.1) then
                idvrsww(nx)=0
              endif
              
              if(iout.eq.1) then
                write(nlog,180) cidvri,ityopr1, itype,
     1           'Wel-Right',cx, 'Wel-Right',iops1, numdvrw
              endif  
              
              goto 500
            endif
          end do
c                                                        
c ---------------------------------------------------------
c               Print problem could not find well right
          nx=-1
          if(istop.eq.0) then
            write(nlog,170) 'Problem', cidvri, 
     1       'W-Right', cx, 'W-Right', '(*.wer)', numdvr
     
            goto 9999
          endif
          
c _________________________________________________________
c
c               Type 20, Monthly constraints
c        
        case(20)
c
c ---------------------------------------------------------
c               a. idumx=0 means all months on
c         write(nlog,*) '  Oprfind; idumx = ', idumx
c
          do im=1,12
            imonsw(k,im)=1
          end do
          if(idumx.eq.0) goto 500
c
c ---------------------------------------------------------
c               b. idumx=12 means read all months
          if(idumx.eq.12) then
c
c rrb 2007/07/03; Allow command line arguements          
            read(55,*,end=2000,err=2000) (imonsw(k,im),im=1,12)
            if(iecho.eq.1) write(nchk,'(12i8)') (imonsw(k,im),im=1,12)
            goto 500
          endif
c
c ---------------------------------------------------------
c rrb 00/02/28; c. Allow monthly and intervening structures
c                  Note Recognize -8 and -20 for GUI operation 
c		   of RG compact 
          if(idumx.lt.0) then
            if(idumx.eq.-8) then
              do im=1,12
                imonsw(k,im)=1
              end do  
              if(iout.eq.1) then
                write(nlog,180) cidvri, ityopr1, itype, 
     1           'Mon', cx, 'Mon', iops1, -1
              endif                
              goto 500
            endif 

            idumy = iabs(idumx)-12.0
c
c rrb 2007/11/05; Correction -12 is monthly switches only            
            if (idumy.ge.0) then
c
c ---------------------------------------------------------
c rrb 2007/07/03; Allow command line arguements          
              read(55,*,end=2000,err=2000) (imonsw(k,im),im=1,12)            
              if(iecho.eq.1) write(nchk,'(12i8)')(imonsw(k,im),im=1,12)
              
              if(iout.eq.1) then
                write(nlog,180) cidvri, ityopr1, itype, 
     1            'Mon', cx, 'Mon', iops1, -1
              endif  
              goto 500
            endif
          endif

c
c               c. Problem idumx is a problem
          nx=-1
c
c rrb 2005/08/29; Revise to provide a better check.
c         if(istop.eq.0) then
          if(idumx.lt.0 .and. istop.eq.0) then
            write(nlog,200) ' Problem', cidvri, idumx
            goto 9999            
          endif

c _________________________________________________________
c
c
c               Type 21; Intervening Structures
c
c rrb 99/11/02; Allow monthly switch only
        case(21)
          idumy=idumx
          if(idumx.eq.12) idumy = 0
c
c               Allow monthly and intervening structures
          if(idumx.lt.0) idumy = iabs(idumx)-12.0
c
c ---------------------------------------------------------
c               Read intervening structure info
c         write(nlog,*) '  Oprfind; idumx = ', idumx
          if (idumy.gt.0) then
            read(55,'(36x,10a12)',end=2000,err=2000)
     1           (cntern(i),i=1,idumy)
            if(iout.eq.1) write(nlog,*) (cntern(i),i=1,idumy)
c           write(nlog,'(2x,a12)') (cntern(i),i=1,idumy)
            if(iecho.eq.1) write(nchk,'(36x,10a12)') 
     1        (cntern(i),i=1,idumy)
c     
c ---------------------------------------------------------                                                                        
            do i=1,idumy
              ifound=0
              do nx=1,numdiv                                              
                if(cdivid(nx).eq.cntern(i)) then
                  intern(k,i)=nx
c                 write(nlog,*) '  OprFind; nx = ', nx
                  ifound=nx
                endif
              end do
c              
c ---------------------------------------------------------              
c               Error message
              if(ifound.eq.0 .and. istop.eq.0) then
                write(nlog,210) ' Problem', cidvri, cntern(i)
                goto 9999              
              endif
c
c ---------------------------------------------------------
c		Detailed Output
              if(iout.eq.1) then
                write(nlog,180) cidvri, itype, ityopr1, 
     1           'Interv', cntern(i), 'Interv',ifound, -1
              endif                
            end do
          endif
          
          
c _________________________________________________________
c
c               Type 22, Maximum constraints
c        
        case(22)
          read(55,*,end=2000,err=2000) (oprMax(k,im),im=1,13)
          if(iecho.eq.1) write(nchk,410) (oprMax(k,im),im=1,13)

c         iout=1
          if(iout.eq.1) then
            write(nlog,*) ' Oprfind; OprMax=', (oprmax(k,im), im=1,13)
          endif
c _________________________________________________________
c
c
c               Type 23; Intervening Structures with Loss
c
c rrb 99/11/02; Allow monthly switch only
        case(23)
          idumy=idumx
          if(idumx.eq.12) idumy = 0
c
c               Allow monthly and intervening structures
          if(idumx.lt.0) idumy = iabs(idumx)-12.0
c
c               Read intervening structure plus loss info
          if(iout.eq.1)
     1     write(nlog,*) '  Oprfind; idumx, idumy = ', idumx, idumy
c
c ---------------------------------------------------------
          if (idumy.gt.0) then
            do i=1,idumy
c
c
              read(55,*,end=2000,err=2000) cntern(i), OprLossC(k,i),
     1          rec12
              if(iout.eq.1) write(nlog,*)  cntern(i), OprLossC(k,i),
     1          rec12
              if(iecho.eq.1) write(nchk,'(36x,a12,1x, f8.2,1x,a12)') 
     1          cntern(i), OprLossC(k,i), rec12
     
              if(rec12(1:7).eq. 'Carrier') internT(k,i) = 1
              if(rec12(1:6).eq. 'Return')  internT(k,i) = 2
c             write(nlog,*) ' Oprfind; k, i, ',rec12, k, i, internT(k,i)
              if(internT(k,i).eq. 0) goto 930
     
c          
c ---------------------------------------------------------
c		  Find diversion ID                                                                        
              ifound=0
              if(internT(k,i).eq.1) then
                do nd=1,numdiv                                              
                  if(cdivid(nd).eq.cntern(i)) then
                    intern(k,i)=nd
c                   write(nlog,*) '  OprFind; nd = ', nd, cdivid(nd)
                    ifound=nd
                  endif
                end do
              endif
c          
c ---------------------------------------------------------
c rrb 2008/01/04; Find Stream ID                                                                        
              if(internT(k,i).eq.2) then
                do is =1,numsta                                              
                  if (cstaid(is).eq.cntern(i)) then
c                   write(nlog,*) '  OprFind; is = ', is, cstaid(is)
                    ifound=is
                    intern(k,i)=is
                  endif
                end do
              endif  
c
c ---------------------------------------------------------
c               Error structure not found or wrong structure type
              if(ifound.eq.0 .and. istop.eq.0) then
                write(nlog,210) ' Problem', cidvri, cntern(i)
                goto 9999              
              endif
c
c ---------------------------------------------------------
c               Error structure 1 is not a diverion (carrier)
              if(internT(k,1).ne.1) then
                write(nlog,214) ' Problem', cidvri,rec12
                goto 9999              
              endif
c
c ---------------------------------------------------------
c               Error structure n is a return & loss is not allowed
              if(internT(k,i).eq.2 .and. OprLossC(k,i).gt. small) then
                write(nlog,216) ' Problem', cidvri, rec12,OprLossC(k,i)
                goto 9999              
              endif
c          
c ---------------------------------------------------------
c		  Check percent loss provided
              if(OprLossC(k,i) .le. (-0.001)) goto 928
              if(OprLossC(k,i) .ge. (100.01)) goto 928
c          
c		  Detailed Output              
              if(iout.eq.1) then
                write(nlog,180) cidvri, ityopr1, itype, 
     1           'Interv', cntern(i), 'Interv', ifound, -1
              endif                
            end do
          endif
          
          
c
c
c _________________________________________________________ 
c
c rrb 2007/07/03
c               Type 24; Read and locate an operating rule
c		                     to allow monthly and annual limits
        case(24)
          ifound=0
c         read(55,'(36x,10a12)',end=2004,err=2004) cx
          read(55,*,end=2004,err=2004) cx
          if(iout.eq.1) write(nlog,'(36x,10a12)') ' Oprfind; cx = ', cx
          if(iecho.eq.1) write(nchk,'(36x,10a12)') cx
          
c ---------------------------------------------------------          
          do nx=1, k-1
            if(corid(nx).eq.cx) then
              iops1 = nx
              if(iout.eq.1) then
                write(nlog,180) cidvri, ityopr1, itype,
     1            'Opr', cx, 'Opr',iops1, k
              endif 
              goto 500                
            endif
          end do
c
c ---------------------------------------------------------
c               Warn user source water right or opr rule not found
          nx=-1
          if(ifound.eq.0 .and. istop.eq.0) then
            write(nlog,1500) 'Problem', cidvri, cx
            goto 9999
          endif
          
          
          
c
c _________________________________________________________ 
c
c rrb 2007/07/03
c               Type 25; Read and locate multiple plan destinations
c			 with % ownership
cx        case(25)
cx          write(nlog,*) ' Oprfind; type 25, nplan = ', nplan, ioutP
cx          ifound=0
cx          cx='            '
cx
cx 250      read(55,'(a1, 80x, a12, f8.0)',end=2004,err=2004) rec1,cx,rx
cx          if(iecho.eq.1) write(nchk,'(a1,80x,a12,f8.0)') rec1, cx, rx
cx          if(iout.eq.1) write(nlog,'(a1,80x,a12,f8.0)') rec1, cx, rx
cx
cx          if(rec1.eq.'#') goto 250
cx          if(iout.eq.1) write(nlog,*) ' Oprfind; cx, rx = ', cx, rx
cx          
cx          do nx =1,nplan
cx            if(ioutP.eq.1) write(nlog,*) ' Oprfind; ',nx, cx, pid(nx)
cx            if (pid(nx).eq.cx) then
cx              iops1=nx
cx              iops2=ifix(rx)
cx              rops2=rx
cxc
cxc		Warn if the plan is off                            
cx              if(ifix(pon(nx)).eq.0) then
cx                write(nlog,182) cidvri, ityopr1, itype, 'Plan', cx 
cx              endif  
cx              
cx              if(iout.eq.1 .or. ioutP.eq.1) then
cx                write(nlog,180) cidvri, ityopr1, itype, 
cx     1            'Plan', cx, 'Plan', iops1, nplan
cx              endif  
cx              goto 500
cx            endif
cx          end do
cxc                                                        
cxc               Print problem could not find a Plan
cx          nx=-1
cx          if(istop.eq.0) then
cx            write(nlog,170) ' Problem', cidvri,
cx     1         'Plan', cx, 'Plan', '(*.pln)', nplan
cx            goto 9999
cx          else
cx            goto 500
cx          endif

c _________________________________________________________
c
c               Type 26, Efficiency Values
c        
        case(26)
          read(55,*,end=2000,err=2006) (oprEff(im,k),im=1,12)
          if(iecho.eq.1) write(nchk,'(12f8.2)') (oprEff(im,k),im=1,12)

          if(iout.eq.1) then
            write(nlog,*) ' Oprfind; OprEff=', (oprEff(im,k), im=1,12)
          endif
          
c _________________________________________________________
c
c		Default
c
        case default
          write(nlog,3000) 'Problem', itype
          goto 9999
        end select
c
c               Return
c _________________________________________________________
  500 return
C
c               Formats
c __________________________________________________________
  100 format(/,72('_'),/                                                          
     1'  Oprfind; Problem with Right ID ', a12,/                       
     1 11x,' Source Stream ID = ',a12,' in operations file (*.opr) ',/              
     1 11x,' is not in the steam network file (*.rin)')

  160 format(/,72('_'),/                                                          
     1'  Oprfind; Problem with Right ID ', a12,/                       
     1 11x,' Destination or source Well ID = ',a12,/
     1 11x,' in the operations right file (*.opr) ',/              
     1 11x,' is not in the Well station file (*.wes)') 
     
  170 format(/,72('_'),/                                                         
     1'  Oprfind; ', a8, ' Finding ID = ', a12,/                       
     1 11x,' The Structure type = ', a12, /
     1 11x,' with ID = ',a12,/
     1 11x,' is not in the ',a12, ' file ',a8,/
     1 11x,' Remember enter NA if none are required',/ 
     1 11x,' Number of entries read from the file = ', i5)

  180 format(/,
     1 '  Oprfind; Detatiled Output for ',/
     1 11x, 'Operating Right or Calling Routine = ', a12, 
     1 11x, 'Calling Type ',i4,' Data Type ',i4,/
     1 11x, 'Found ', a8, '  ID        =    ', a12,/
     1 11x, 'Found ', a8, '  Pointer   = ', i4,/
     1 11x, 'Number of values searched = ', i5)
     
  182 format(/,72('_'),/
     1 '  Oprfind; Warning Operating Right ', a12, ' Type ',i4,
     1 ' Data Type ',i4,/
     1 11x, 'Has a source ', a8, ' ID      =    ', a12,/
     1 11x, 'That is turned off.  Therefore the operating',/
     1 11x, 'rule has been turned off')
     
  200 format(/,72('_'),/
     1 '  Oprfind; ',a8,' Operating Right ', a12,/                 
     1 11x,' Do not expect ', i8, ' structures or',/                 
     1 11x,' month codes for this type of operating rule.'/
     1 11x,' Note: a negative value indicates 12 monthly codes will',/
     1 11x,' be provided plus iabs(n)-12 intervening structures')             
     
  210 format(/,72('_'),/
     1 '  Oprfind; ',a8, ' Operating Right ', a12,/                       
     1 11x, ' Cannot find intervening ID  ',a12,/
     1 11x, ' or the structure type specified is incorrect. Note:',/
     1 11x, ' The the first structure must be type = Carrier.',/
     1 11x, ' If the intervening structure has type = Carrier it',/
     1 11x, ' must be in the diversion station file (*.dds)',/
     1 11x, ' If the intervening structure has type = Return',/
     1 11x, ' it must be in the river network file (*.rin)',/
     1 11x, ' Recommend you adjust accordingly')
     
  212 format(/,72('_'),/
     1 '  Oprfind; ',a8, ' Operating Right ', a12,/                       
     1 11x, ' Found intervening ID  ',a12)
     
  214 format(/,72('_'),/
     1 '  Oprfind; ',a8, ' Operating Right ', a12,/                       
     1 11x, ' The first intervening structure has type = ', a12,/
     1 11x, ' It should be type = Carrier',/
     1 11x, ' Recommend you adjust accordingly')

  216 format(/,72('_'),/
     1 '  Oprfind; ',a8, ' Operating Right ', a12,/                       
     1 11x, ' The intervening structure has type = ', a12,/
     1 11x, ' and a carrier loss = ', f8.2,/
     1 11x, ' Return types are not allowed to have carrier losses',/
     1 11x, ' Recommend you adjust accordingly')

  300  format(/,72('_'),/
     1 '  OprFind; FYI the following Rights are off because',/
     1 '           1. The source is off, or ',/
     1 '           2. The on date is after the starting year, or',/
     1 '           3. The user set variable iopsou(4,k)=1 in the',/
     1 '              operating rule (*.opr) file, or',/,
     1 '           4. Statemod expects this right to be controlled',/
     1 '              by an operating rule.',//
     1 '      # Type Opr ID       Opr Name                ',
     1 ' Source ID    OnOff   Nx Right Turned Off',/
     1 '  _____ ____ ____________ ________________________',
     1 ' ____________ _____ ____ __________________')
     
  302  format(2x, i5,i5, 1x, a12, 1x, a24, 1x, a12, 1x, 2i5, 1x,a12)       
  400 format(36x,12i8)
  410 format(36x,12f8.0)
  
 1100  format(/,72('_'),/                                                          
     1'  Oprfind; Problem with Right ID ', a12,/                      
     1 11x,' Destination ISF ID = ',a12,' in operations file (*.opr)',/              
     1 11x,' cannot be found in ISF station file (*.ifs)')

 1300 format(/,72('_'),/                                                          
     1'  Oprfind; ', a8, ' Operating Right ', a12,/                      
     1 11x,' Destination or source water right Div ID = ',a12,/
     1 11x,' in the operations right file (*.opr) ',/              
     1 11x,' is not in the Div right file (*.ddr)')
     
 1304 format(/,72('_'),/
     1 '  Oprfind; Problem with Operating Right ', a12,/
     1 11x,' Source 2 (a plan ID) = ', a12 ,/
     1 11x,' Cannot be found in the plan data file (*.pln). Check',/
     1 11x,' 1. The specified ID is in the plan data (*.pln)',/
     1 11x,' 2. The plan data (*.pln) is provided, and',/
     1 11x,' 3. The plan data is in the response file (*.rsp)')

 1305 format(/,72('_'),/
     1 '  Oprfind; Warning for Operating Right ', a12,/
     1 11x,' Source 2 (a plan ID) = ', a12 ,/
     1 11x,' Since it is blank StateMod assumes this operating rule',/
     1 11x,' has no terms and conditions')


 1500 format(/,72('_'),/
     1'  Oprfind; ', a8, ' Operating Right ', a12,/                      
     1 11x,' Cannot find an Operating Right',
     1     ' = ', a12,/
     1 11x,' in the Operating Right (*.opr) file above this opr rule')
     
 3000 format(/,72('_'),/
     1 '  Oprfind; ', a8, ' Case ', i5, ' not available')
c
c               Warnings         
c ____________________________________________________
  919 format(/,72('_'),/
     1 '  OprFind; FYI *.opr rule ID = ', a12,  /
     1 '           has a destination account = ', i5,/
     1 '           which means the opr rule treats the reservoir',/ 
     1 '           as a total, not by an account')
  920 format(/,72('_'),/
     1 '  OprFind; Problem with *.opr rule ID = ', a12,/  
     1 '           It has a source or destination account = ', i5,/
     1 '           which is not associated with this reservoir',/
     1 '           recommend you revise the reservoir data (*.res)',/
     1 '           or operating rule (*.opr) data')
     
 1281  FORMAT(/,72('_'),/
     1  '  Oprfind; Warning See *.chk for details.')
     
c              
c               Error Processing
c _________________________________________________________
 2000 write(nlog,2010) cidvri, ityopr1, itype      
 2010 format(/,72('_'),/
     1 ' Oprfind; Problem with *.opr rule ID = ', a12,' type ', i5, 
     1 ' Data Type ',i4,/     
     1 10x,'Cannot read monthly on/off or intervening structures',/
     1 10x,'or (for a Oprfind type 24 or 25) exchange % and ',
     1 '13 maximum exchange values (one per month plus an annual)' ) 
      goto 9999
      
 2002 write(nlog,2012) cidvri, ityopr1, itype      
 2012 format(/,72('_'),/
     1 ' Oprfind; Problem with *.opr rule ID = ', a12,' type ', i5,
     1 ' Data type ', i5, / 
     1 10x,'Cannot read the exchange % and 13 maximum exchange',/
     1 10x,' values (one per month plus an annual)' ) 
      goto 9999
      
 2004 write(nlog,2014) cidvri, ityopr1, itype      
 2014 format(/,72('_'),/
     1 ' Oprfind; Problem with *.opr rule ID = ', a12,' type ', i5,  
     1 ' Data Type ',i4,/     
     1 10x,'Cannot read the operating rule associated with a monthly',/
     1 10x,'or annual limitation adustment')
      goto 9999
      
 2006 write(nlog,2016) cidvri, ityopr1, itype      
 2016 format(/,72('_'),/
     1 ' Oprfind; Problem with *.opr rule ID = ', a12, ' type ', i5, 
     1 ' Data Type ',i4,/      
     1 10x,'Cannot read monthly efficiency data',/
     1 10x,'Note a type 26 rule with source 2 = a T&C Plan',/
     1 10x,'requires monthly efficiency data',/
     1 10x,'Recommend you revise your operating rule file')
      goto 9999
      
  924 write(nlog,925) cidvri, ityopr1, itype, iops2
  925 format(/,72('_'),/
     1 ' Oprfind; Problem with *.opr rule ID = ', a12, ' type ', i5,
     1 ' Data type ', i5, / 
     1 '          Destination account = ', i5,' but it should be 1')
      goto 9999                                  
      
  926 write(nlog,*) ' OprFind; ', cidvri, ityopr1, itype, iops2
      write(nlog,927) cidvri, ityopr1, itype, iops2
  927 format(/,72('_'),/
     1 ' Oprfind; Problem with *.opr rule ID = ', a12, ' type ', i5,
     1 ' Data type ', i5, / 
     1 '          Destination Account = ', i5,' but it should be >0')
      goto 9999
      
  928 write(nlog,929) cidvri, ityopr1, itype, OprLossC(k,i)
  929 format(/,72('_'),/
     1 ' Oprfind; Problem with *.opr rule ID = ', a12, ' type ', i5,
     1 ' Data type ', i5, / 
     1 '          Carrier loss = ', f8.2,' (%)',/
     1 '          It should be between 0% and 100%.')
      goto 9999                                  
      
  930 write(nlog,931) cidvri, ityopr1, itype, rec12
  931 format(/,72('_'),/
     1 ' Oprfind; Problem with *.opr rule ID = ', a12, ' type ', i5,
     1 ' Data type ', i5, / 
     1 '          StateMod Version 12.28 and greater require a data',/
     1 '          type when intervening structures with loss are ',
     1           'simulated',/
     1 '          Carrier Type = ', a12, /
     1 '          It should be = Carrier or',/
     1 '                       = Return',/
     1 '          Following is an example carrier (Dem_Tunnel) that ',
     1           'looses 5%',/
     1 '          Dem_Tunnel 5  Carrier')
      goto 9999                                  


 9999 write(6,150)
      write(nlog,150)
  150 format(/,72('_'),/
     1  '  Stopped in Oprfind, see the log file (*.log)')      
      write(6,*) 'Stop 1'
      call flush (6)
      call exit(1)

c
c _________________________________________________________
c

      stop
      end
