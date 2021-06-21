c                                                                       
c ************************************************************          
c                                                                       
	    subroutine CompURM(nlog, nout, maxnf,maxsta,ip, idly, fileidX)
c
c               Compare a *.urm file (ID on one line and other
c                 information on other lines)
c               Similar to CompSta but revised to handle free format
c                 associated with *.dly (*.urm) better
c
c               APPROACH
c               1. Reads and stores file 1 
c                  where id = recid(i) and record data = rec1(i,j)
c                  Note j=1 is the ID and j=2,MaxLine are substation data
c               2. Read file 2 until data for the first ID and all the
c                  substation data is found
c                  where id = recidx and record data = rec2x(j)
c               3. compares file 1 to on station in file 2
c               3. goes back to read the next station data from file 2
c               
c 
c               station data then compares to another
c                       idly = 0 delay file processing
c                       maxsta = max # of stations
c                       nl   = max # of lines per station
c
c rrb similar to CompSta but recid = *16
	     dimension rec1(maxsta,500), recid(maxsta),ifound(maxsta),
     1            rec2x(500)
	     character rec1*140,  recid*12, rec1x*140, recTemp*140,
     1            recidx*12, rec2x*140
	     character blank*140, fileidX*5

c                                                                       
c               Initilize                                      
	     maxLine=500
	     nf=10
	     idiff=0
       nin=0
c
c rrb 2011/09/09; iout=1 max detail
c                 iout=2 detail on ID's
        iout=0
c
c rrb 03/01/30; Correction
c       in2=0
        nin2=0
c       blank = ' '
	      blank = '* Not Available * '
        write(6,*) ' '
c
	      do 100 i=1,maxsta
	        do 100 j=1,maxLine
	          rec1(i,j)=blank
  100     ifound(i) = 0
						  
	      do 110 i=1,maxnf                                                
	        nf1 = nf+i                                                    
	        call skipn(nf1,0) 
  110   continue 
	
	      if(ip.eq.0) then
	        write(26,102)   'Full Printout   '
	        write(nlog,102) 'Full Printout   '
	      else
	        write(26,102)   'Differences only'
	        write(nlog,102) 'Differences only'
	      endif
 102    format(/,' ',/ ' CompURM; File Comparison - ', a16,/, ' ')
c                                                                       
	      nf = 11                                                         
	      i=0
	      j=0
	      iend=0

c       write(nlog,*) '  CompURM; idly = ', idly
c
c _________________________________________________________
c
c               File 1
 

 120    rec1x=' '
        read(nf,'(a140)',end=130,err=270) rec1x
        if(iout.eq.1) write(nlog,'(a140)') rec1x
c
c               Skip comments
        if(rec1x(1:1).eq.'#') goto 120
        
	      ib=1        
cz      if(idly.eq.0 .and. rec1x(1:3).eq.'   ')      ib=0
c
c _________________________________________________________
c               Delay file with free format (Idly = 1)
c               An ID has the following characteristics:
c               1. It is limited to 12 characters
c               2. It begins in columns 1-12 (ibeg,=12)
c               3. It must end with a blank after it begins and before column 13

        if(idly.eq.1) then
          idBeg=0
          idEnd=0
c
c rrb 2011/09/07; handle Splatte format
          do k=1,16
            if(rec1x(k:k).ne.' ' .and. idBeg.eq.0)  idBeg=k
            if(rec1x(k:k).eq.' ' .and. idBeg.gt.0 .and. idEnd.eq.0)
     1        idEnd=k
          end do
          
          ib=1
          if(idBeg.eq.0 .or. idBeg.gt.12) ib=0
          if(idEnd.eq.0 .or. idEnd.ge.13) ib=0
        endif 
        if(iout.eq.1) write(nlog,*) '  CompURM; idBeg, idEnd = ',
     1    idBeg, idEnd              
c
c             Check if it is a data record (ib.eq.0) or a record with an id (ib.gt.0)
      	if(ib.eq.0) then
      	  j=j+1
      	  if(j.gt.maxLine) goto 290
      	else
      	  i=i+1
      	  if(i.gt.maxsta) goto 272

      	  j=1       
c
c rrb 2011/09/09; get rid of characters to the right of the ID 
          n1=0
          recTemp=adjustL (rec1x)
          do n=1,16
            if(recTemp(n:n).eq.' ') n1=1
            if(n1.eq.1) recTemp(n:n) = ' '
          end do
c
c           Set the ID to the first 12 characters                       
          recid(i)=recTemp(1:12)
          if(iout.ge.1) then
            write(nlog,*) '  CompURM; File 1 i = ', i, ' id = ',recid(i)
          endif

c         write(6,122) i
 122      format('+', ' CompURM; Processing station ', i5)
	      endif

        if(iout.eq.1) write(nlog,*) '  CompURM; i,j,rec1x', i, j,rec1x
	      rec1(i,j) = rec1x                         
	      goto 120

c _________________________________________________________
c
c                       File 2
  130   nin=i
        if(iout.ge.1) write(nlog,*) ' '
c
c rrb 2011/09/09; Allow one file to be compared to itself
        close(nf)
        rewind(nf+1)
        if(iout.eq.1) write(nlog,*) '  CompURM; nin = ', nin
	      j1=0
	      i1=0
c
c _________________________________________________________
c              Step X; Read next record in file 2
c
  140   rec1x = ' '
        read(nf+1,'(a140)',end=160,err=270) rec1x
c
c rrb 02/01/24; Skip comments
        if(rec1x(1:1).eq.'#') goto 140

	      j1=j1+1
	      if(j1.gt.maxLine) goto 290
c       write(nlog,*) j1, rec1x
c
c              Step X; Store file 2 record
	      rec2x(j1) = rec1x     
c
c _________________________________________________________
c                       New Station
c
c               Delay file check for a decimal to determine if an ID
        if(idly.eq.1) then
          k1=0
          idBeg=0
          idEnd=0
c
c rrb 2011/09/07; handle Splatte format
          do k=1,16
            if(rec1x(k:k).ne.' ' .and. idBeg.eq.0)  idBeg=k
            if(rec1x(k:k).eq.' ' .and. idBeg.gt.0 .and. idEnd.eq.0)
     1        idEnd=k
          end do
          
          ib=1
          if(idBeg.eq.0 .or. idBeg.gt.12) ib=0
          if(idEnd.eq.0 .or. idEnd.ge.13) ib=0
        endif

c
c _________________________________________________________
c
c              Step X; File 2.  For ib=1 (new station) find the ID
	      if(ib.eq.1) then
	        if(j1.eq.1) then
	          i1=i1+1
c
c rrb 2011/09/09; get rid of characters to the right of the ID 
            n1=0
            recTemp=adjustL (rec1x)
            do n=1,16
              if(recTemp(n:n).eq.' ') n1=1
              if(n1.eq.1) recTemp(n:n) = ' '
            end do	          
	          
	          recidx=recTemp(1:12)
            if(iout.ge.1) then
              write(nlog,*) '  CompURM; File 2 i1= ',i1, ' id = ',recidx             
            endif
	          nin2=nin2+1
c
c ---------------------------------------------------------
c              Initilize substation records         
	          do 150 j=2,maxLine
	            rec2x(j) = blank
  150       continue      
c
c rrb 2011/09/09; Go back for substation data 
            goto 140
	        else
	          backspace(nf+1)
	          rec2x(j1) = blank
	          j1=0
	          goto 170
	        endif
	      endif
	      goto 140
c       write(6,*) '  CompURM; at 160'
c _________________________________________________________
c		           Step X; Compare ID's
c

  160   iend=1
        if(iout.eq.1) write(nlog,*) '  CompURM; at 160'
  170   do 190 i=1,nin
c         write(6,172) i, nin
c 172     format('+', '  CompURM; Comparing station', i5 , ' of', i5)
	        if(recid(i).eq.recidx) then
	          ifound(i) = 1
            if(iout.eq.1) write(nlog,*) ' CompUrm; Match for ',recidx
c _________________________________________________________
c
c                       Substation loop
	          do 180 j=1,maxLine
	            if(rec1(i,j).eq.blank .and. 
     1                 rec2x(j).eq.blank) goto 210
	            if(rec1(i,j).eq.rec2x(j)) then
cx             ip=0
		            if(ip.eq.0) then
                  write(26,280) ' Same        ',recid(i),
     1                                           1, rec1(i,j), 
     1                                           2, rec2x(j)
c                 write(nlog,280) ' Same        ',recid(i),
c    1                                           1, rec1(i,j), 
c    1                                           2, rec2x(j)

		            endif
	            else
                write(26,280) ' Difference 0 ', recid(i),
     1                                          1, rec1(i,j), 
     1                                          2, rec2x(j)
c               write(nlog,280) ' Difference 0 ', recid(i),
c    1                                          1, rec1(i,j), 
c    1                                          2, rec2x(j)
		            idiff=idiff+1
	            endif                 
  180       continue
	          goto 210
	        endif
  190   continue
c
c               Not found in file 1; write file 2 data
	      idiff=idiff+1 
	      do 200 j=1,maxLine
	        if(rec2x(j).eq.blank) goto 210
          write(26,280) ' Diff 1 ',recidx,1,blank,2,rec2x(j)
c         write(nlog,280) ' Difference 1 ',recidx,1,blank,2,rec2x(j)
  200   continue
  210   if(iend.eq.0) goto 140
c
c               Check and print if file 1 data is not found
  220   do 240 i=1,nin
	       if(ifound(i).eq.0) then
	         idiff=idiff+1 
	         do 230 j=1,maxLine
	           if(rec1(i,j).eq.blank) goto 250
                   write(26,280) ' Difference 2 ',recid(i),
     1                        1,rec1(i,j), 2, blank
c             write(nlog,280) ' Difference 2 ',recid(i),
c    1                        1,rec1(i,j), 2, blank
  230       continue
	        endif
  240   continue   
c                                                                       
c              Close input and output file                              
  250   do 260 nf=11,10+maxnf                                           
	        close(nf)                                                     
  260   continue                 
	
c       close(26) 
	      write(26,182)  nin, nin2,idiff
	      write(nlog,182) nin, nin2,idiff
     
 182    format(/
     1         '  Number of stations and differences:',/,
     1         '  File 1 Stations = ', i10,/,
     1         '  File 2 Stations = ', i10,/,
     1         '  Differences     = ', i10)
     
	      write(90,183) nout, fileidX, nin, nin2, idiff        
cx      write(nlog,183) nout, fileidX, nin, nin2, idiff 
 183    format(i4, 1x, a5, 3i10)    
     
     
     
	return                             
c
c               Error Messages              
  270   write(nlog,*) '  Problem reading file ', nf,' in CompURM.f'
	      write(6,*)  '  Unsuccessful termination, see smfc.log'
	      write(nlog,*) '  Unsuccessful termination, see smfc.log'
	      goto 300
	      
  272   write(nlog,*) '  Problem dimension size in CompURM.f', maxsta
	      write(6,*)  '  Unsuccessful termination, see smfc.log'
	      write(nlog,*) '  Unsuccessful termination, see smfc.log'
	      goto 300

  280   format(a14,' for ',a12,':', /, 2x, i2, ' = ', a140,/ 
     1                                 2x, i2, ' = ', a140,/)

  290   write(6,292) maxLine
	      write(nlog,292) maxLine
  292   format('  CompURM, Dimension problem maximum number '/
     1         '           of lines per station ', i5, ' exceeded')
  300   call flush(6)
        stop 1
	      end                                                             



