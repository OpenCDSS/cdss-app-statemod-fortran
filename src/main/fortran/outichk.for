c
c
      SUBROUTINE OutIchk(ichkX, ichk4n,l1, l2, iw, itypeX, 
     1           ishort, fac, 
     1           uDem, divactx, divX, divsum,
     1           ichk9, div9, rec12b)  
c
c	OutIchk; It prints detailed data for various values of Ichk
c      
c         Called by Execut.

c     uDem = unmet demand (cfs)  
c     Divactx = diversion by an operating rule
c     DivX    = diversion by a non operating rule
c     Divsum = new water from an operating rule (af)
c     iwx = reoperation counter
c     Div9    = diversion or return for ichk=9
c     Rec12b  = reoperation type for ichk=9
c     ichk4n  = counter for header output
c _________________________________________________________
c
      include 'common.inc'
      character rec12*12, ctype*16, rec12b*12, nameX*24
c
c
c rrb 2015/06/25; Add code to limit the volume of output when
c                 variable ichk = 4 or 94
      iout=0
      if(iout.eq.1) then
        if(iyrmo(mon).ne. 1998) goto 100
        if(mon.ne.7) goto 100
        write(nlog,*) '  OutIchk; curown(13) = ', curown(13)
      endif                   
      
cx    npage=51
      npage=11
      
      
      if(ichk.eq.4 .or. ichk.eq.94) then
        divreqx=-1.0/fac      
        dem1=-1.0/fac
c
c         Detailed Operating Data (ichk=4)
        ichk4n=ichk4n+1        

c
c rrb 2011/05/04; Detailed Check Output 
c rrb 2011/07/20; Revise if the first right is turned off
cx        if(iw.eq.1 .and. iwx.eq.1) write(nlog,200) ichk   
          if((iw.eq.1 .and. iwx.eq.1) .or. ichk4n.eq.1) 
     1      write(nlog,200) ichk          
          if(iw.eq.1 .or. ichk4n.eq.1 .or. ichk4n.eq.npage) 
     1      write(nlog,202) 
            if(ichk4n.ge.npage) ichk4n=1
          
            if(l1.eq.5) then
              ctype='Operating Rule'              
              rec12=corid(l2)
              cpri=ropnk(l2)
              write(nlog,210) ichk4n, iyrmo(mon),xmonam(mon),
     1          idy,iwx,iw, 
     1          ctype, itypeX, rec12, cpri, -1., -1.,
     1          dem1*fac, divactx*fac, divreqx*fac, divchk*fac,
     1          divsum, ireop, ichkX, 18, divo(18)*fac
           else
              rec12='NA'
              cwr=-1.0
              
              if (l1.eq.1) then
                ctype='Instream Flow'
                Nf  =iifrco(L2)
                rec12=cisfwr(l2)
                cwr=dcrifr(l2)
                cpri=rfrnk(l2)
                divreqx=flowr(mon,nf) 
                dem1=flowrq(nf)
              endif
c              
c ---------------------------------------------------------              
c		Reservoir
              if(l1.eq.2) then
                ctype='Reservoir'
                NR  =IRESCO(1,L2)
                rec12=creswr(l2)
                cwr=dcrres(l2)/fac
                cpri=rrsnk(l2)  
cx              divreqx=udem
                divreqx=-1./fac
                dem1=(VOLMAX(NR)-CURSTO(NR))/fac
              endif  
c              
c ---------------------------------------------------------              
c		Diversion
              if(l1.eq.3) then
                ctype='Diversion'
                Nd  =IdivCO(1,L2)
                rec12=crigid(l2)
                cwr=dcrdiv(l2)
                cpri=rdvnk(l2)  
                IUSE=NDUSER(ND)+IDIVCO(2,L2)-1              
                divreqx=divreq(iuse)
                dem1=divert(mon,iuse)
              endif
c              
c ---------------------------------------------------------              
c		Well
              if(l1.eq.6) then
                ctype='Well'              
                Nw  =IdivCOw(1,L2)
                rec12=crigidw(l2)
                cwr=dcrdivw(l2)
                cpri=rdvnkw(l2)                
                nwe =idivcow(1,L2)
                divreqx=divreqw(nwe)     
                dem1=diverw(mon,nwe)           
              endif
c              
c ---------------------------------------------------------              
c  Print non operating rules
c                   
              write(nlog,210) ichk4n, iyrmo(mon),xmonam(mon),
     1          idy,iwx, iw,
     1          ctype, itypeX, rec12, cpri, cwr, cwr*fac, 
     1          dem1*fac,  divx*fac, divreqx*fac, divchk*fac, 
     1          divsum, ireop, ichkx, 18, divo(18)*fac   
            endif
          goto 100
        endif
c
c _________________________________________________________
c          Step X; Printout for Ichk=9, Reopeation details
        if(ichk.eq.9 .or. ichk.eq.109) then
          if(iwx.eq.2) write(nlog,522) ichk
c
c ---------------------------------------------------------
c          Reoperation details - type 12 Operating Rule         
          if(ichk9.eq.1) then    
            rec12=corid(l2)
            nameX=nameo(l2)      
            write(nlog,524) ichk4n, IYRmo(mon), xmonam(mon), 
     1        idy, iwx, l2,
     1        rec12b, ityopr(l2), rec12, nameX,
     1        div9*fac, divsum, divchk*fac 
            goto 100
          endif
          
c
c ---------------------------------------------------------
c          Reoperation details - Return flow for a call by replace
          if(ichk9.eq.2) then
	          rec12 = crigid(l2)
	          rec12b='Div Return  '
	          nameX=named(l2)
            write(nlog,524) ichk4n, IYRmo(mon), xmonam(mon),
     1        idy,  iwx, l2,
     1        rec12b, l1, rec12, nameX, 
     1        div9*fac, divsum, divchk*fac
            goto 100
          endif
c
c ---------------------------------------------------------
c          Reoperation details - Diversion from a call to Splatte 
c          Note l2=isp1
          if(ichk9.eq.3) then
	          rec12 = corid(l2)
	          rec12b='Opr Rule    '
	          nameX=nameo(l2)
         
            write(nlog,524) ichk4n, IYRmo(mon), xmonam(mon), 
     1        idy, iwx, l2,         
     1        rec12b, ityopr(l2), rec12, nameX, 
     1        div9*fac, divsum, divchk*fac  
            goto 100
          endif
c
c ---------------------------------------------------------
c          Reoperation details - Operating Rule 
          if(ichk9.eq.4) then
	          rec12 = corid(l2)
	          rec12b='Opr Rule    '
	          nameX=nameo(l2)
         
            write(nlog,524) ichk4n, IYRmo(mon), xmonam(mon), 
     1        idy, iwx, l2,         
     1        rec12b, ityopr(l2), rec12, nameX, 
     1        div9*fac, divsum, divchk*fac  
            goto 100
          endif          
  
c
c ---------------------------------------------------------
c          Reoperation details - Non downstream Return Flows 
          if(ichk9.eq.5) then
c		Detailed Output - Instream flow	    
	          if(l1.eq.1) then
	            rec12 = cisfwr(l2)
	            rec12b='ISF Return  '
	            nameX=namei(l2)	        
	          endif  
c
c -------------------------------------------------------
c		Detailed Output - Reservoir	      
	          if(l1.eq.2) then
	            rec12 = creswr(l2)
	            rec12b='Res Return  '
	            nameX=namer(l2)	        
	          endif
c
c -------------------------------------------------------
c		Detailed Output - Diversion	      
	          if(l1.eq.3) then
	            rec12 = crigid(l2)
	            rec12b='Div Return  '
	            nameX=named(l2)
	          endif
c
c -------------------------------------------------------
c		Detailed Output - Well	      
	          if(l1.eq.6) then
	            rec12 = crigidw(l2)
	            rec12b='Well Return '
	            nameX=namedw(l2)
	          endif  
          
            write(nlog,524) ichk4n, IYRmo(mon), xmonam(mon), 
     1        idy, iwx, l2,         
     1        rec12b, -1, rec12, nameX, 
     1        div9*fac, divsum, divchk*fac  
            goto 100
          endif          
c
c ---------------------------------------------------------
c          Reoperation details - Reservoir Seepage 
    
          if(ichk9.eq.6) then
	          rec12  = 'NA          '
	          rec12b = 'Res Seepage '
	          nameX='NA                      '
  
            write(nlog,524) ichk4n, IYRmo(mon), xmonam(mon),
     1        idy, iwx, -1,         
     1        rec12b, -1, rec12, nameX, 
     1          div9,   divsum, divchk*fac 
            goto 100
          endif 
        endif
c
c _________________________________________________________
c          Step X; Return
c
c rrb 2015/07/30; Test output
cx 100   return
 100  continue
 
cx      if(iyrmo(mon).eq.1998 .and. mon.eq.11) then
cx        write(nlog,*) 
cx     1  '  OutIchk; divo(18)', l2,
cx     1  iyrmo(mon), xmonam(mon), divo(18)*fac
cx      endif                          
c
      return
c
c _________________________________________________________
c      Formats
c      
 200   format(/, 120('_'),/
     1   '  Detailed check option (ac-ft) = ', i5,/
     1   '  This report summarizes key information related ',
     1     'to the general operation of every water right. ',/
     1   '  Note: a -1 indicates general operating data is too ',
     1     'complex to report herein.  To view that information ',/
     1   '  along with some constraints that may not be shown ',
     1     'a detailed water right report should be requested',/
     1   '  (see the control file (*.ctl) variables ichk & ccall)',//
     1   '  Iter = iteration #, iw = water right pointer, Type = ',
     1     'water right type, OprType = -1 or operating rule #, ',/
     1   '  Right ID = water right ID, Priority = priority,',
     1     'Dec-cfs = water right (cfs), Dec-af = water right (af), ',
     1     'Demand = total demand',/
     1   '  Divert = diversion this iteration, Short = constrained ',
     1     'shortage min(demand, capacity, water right)',/
     1   '  DivChk = reoperaton control, DivSum = reoperaton ',
     1     'sum (if > DivChk reoperate), Ireop = return flow ',
     1     'reoperation (if = 1 reoperate)',/
     1   '  IchkX = location the subroutine is called from Execut',/)
     
 202  format(/  ' OutIchk ;',
     1   '    # Year Mon   Day Iter   iw Type             OprType',
     1   ' Right ID         Priority Dec-cfs  Dec-af  Demand  Divert',
     1   '   Short  DivChk  DivSum   Ireop   ichkX      l2 divo(l2)',/
     1   ' ____ ____ ____ ____ ____ ____ ________________ _______',
     1   ' ____________ ____________ _______ _______ _______ _______',
     1   ' _______ _______ _______ _______ _______ _______ _______')
    
 210  format(' OutIchk ;', 
     1   i5, i5, 1x, a4, i5,i5, i5, 1x, a16, i8, 1x,a12, 1x,f12.5, 
     1   7f8.0, 3i8, 2f10.3)
     
 522  format(/,72('_'),/
     1 '  Detailed check option (ac-ft) = ', i5,/
     1 '  This report summarizes key information related ',
     1   'to re-operation',/
     1 '  Divert = diversion this iteration, ', 
     1   'DivChk = reoperaton control, DivSum = reoperaton ',
     1   'sum (if > DivChk reoperate)',//
     1 '                                                     ',
     1 '                               ',
     1 '    Divert    DivSum    DivChk',/
     1 '    # Year Mon Day Reop   l2 Reason       ',
     1 'Opr Type Right ID    ',
     1 ' Right Name               ',
     1 '     af/mo     af/mo     af/mo',/
     1 ' ____ ____ ___ ___ ____ ____ ____________ ',
     1 '________ ____________',
     1 ' ________________________ ',
     1 ' _________ _________ _________')
     
 524  format(i5, i5, 1x,a4, i3, 2i5, 1x,a12,1x,i8,1x,a12, 1x, a24,1x,
     1 20f10.2)    
     
      end
      
