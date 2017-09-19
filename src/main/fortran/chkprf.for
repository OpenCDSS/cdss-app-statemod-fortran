
         subroutine chkPrf(nlog, ipTC, iprf1, ityopr1, iopsou4, 
     1     maxPlan, maxRtnPP, nrtnPP, pcttotPP, iprob, cidvri)
c
c		It checks if an opreating rule has the 
c		correct type of plan return flow data
c
     
      dimension nrtnPP(maxPlan), pcttotPP(maxRtnPP)
      character cidvri*12
      
      iout=0
      small=0.001
      smalln=-1*small
      
      iprob=0
      iokS=0
      iokF=0
      iokM=0
      
      if(iprf1.ne.1) then
        write(nlog,210)  ityopr1, cidvri, iprf1
        goto 9999
      endif
      
      irni=nrtnPP(ipTC)
      irne=nrtnPP(ipTC+1)-1
      
      do irn=irni,irne
        pct1=pcttotPP(irn)
        if(pct1.gt.small) iokS=1
        if(pct1.lt.smalln)iokF=1
      end do
      if(iokS.eq.1 .and. iokF.eq.1) iokM=1
      
      if(iopsou4.eq.1 .and. iokS.eq.0) iprob=1
      if(iopsou4.eq.2 .and. iokF.eq.0) iprob=1
      if(iopsou4.eq.3 .and. iokM.eq.0) iprob=1
      if(iopsou4.le.2 .and. iokM.eq.1) iprob=1           
      
      if(iprob.eq.1) then
        write(nlog,200)  ityopr1, cidvri, iopsou4
        goto 9999
      endif
     
      return
       
 200  format(/, 72('_'),/, '  ChkPrf; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'the T&C Return Type  = ', i5, ' is inconsistent with',/
     1 10x,'the data provided in the plan return file (*.prf)',/
     1 10x,'Recommend you review the documentation and revise ',/
     1 10x,'the operating rule file (*.opr) or the plan return',/
     1 10x,'file (*.prf)')
     
 210  format(/, 72('_'),/, '  ChkPrf; ',
     1'Problem with Operating right type = ',i2,' ID = ', a12,/
     1 10x,'Source 2 (a T&C Plan) has a Plan Return Type = ',i5,/
     1 10x,'in the plan station file (*.pln). A default value of ',/
     1 10x,'-999 has not been tested while a value of 0 means no',/
     1 10x,'T&C Obligations can be calculated.',/
     1 10x,'TO SIMULATE T&C Obligations you need to set the Plan',/
     1 10x,'Return Type to 1 in the plan station file (*.pln) and',/
     1 10x,'include data in the plan return file (*.prf)',/
     1 10x,'TO NOT SIMULATE T&C Obligations you should remove source',/
     1 10x,'2 (a T&C Plan) from the operating rule file (*.opr)')

 9999 write(6,1440)
      write(nlog,1450) 
      call flush(6)
 1440 format('  Stopped in ChkPrf, see the log file (*.log)')
 1450 format(/,72('_'),/
     1 '  ChkPrf; Stopped in ChkPrf')
      write(6,*) 'Stop 1'
      call flush(6)
      call exit(1)
      
      stop     
      end
