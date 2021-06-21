c                            
c ************************************************************
       subroutine getin(iend)
c
c rrb 2020/12/29; Correction to allow *.xop to be read (see itime)
       include 'SmDelta.inc'     
c
c rrb 1999/12/02
c      character rec3*3
c
c rrb 2020/12/29
cx     character rec3*3, rec25*25
       character rec3*3, rec25*25, itime3*3

       iend = 0
c
c               Get control info once
       if(ifx.eq.0) then
c
c rrb 02/08/09; Allow a title to be provided
         call skipn(5)
         read(5,'(a72)') ctitle
         rec6=ctitle(1:6)
         if(rec6.eq.'-Title' .or. rec6.eq.'-title') then
           i1=6
           ie=72-i1
           do i=1,ie
             i1=i1+1
             ctitle(i:i) = ctitle(i1:i1)
           end do
           ctitle(ie+1:72) = '      '
           write(99,'(a72)') ctitle
         else
           backspace(5)
         endif
c
c               Read run type

         call skipn(5)
         ir = -1
         read(5,'(a10)',end=150,err=150) rtype
         if(rtype.eq.'Single    ') ir = 0
         if(rtype.eq.'Multiple  ') ir = 1
         if(rtype.eq.'Difference') ir = 2
         if(rtype.eq.'Merge')      ir = 3
         if(rtype.eq.'Diffx     ') ir = 4
         if(rtype.eq.'Diff2     ') ir = 5

         if(rtype.eq.'Help')       goto 130
         if(rtype.eq.'Version')    goto 130
         if(ir.eq.-1) goto 150
       endif
c
c               Check dimension
        if(ifx+1.gt.5) then
          write(6,*) '   Number of files exceeds max of 5'
          call flush(6)
          goto 260
        endif
c
c              Get file name
       close(1)
       call skipn(5)
  100  read(5,'(a72)',end=170,err=170) filenx
       if(filenx(1:1).eq.' ' .or. filenx(1:4).eq.'-999') goto 140
c
c rrb 99/05/17; 
       call putpath(filenx, fpath1)
       write(99,102) filenx
  102  format(/, 72('_'),/,' Getin; Read Input file:   ', a72)
c
c               Determine if binary (assumes file is *.b??
       ibin = 1
       do 110 i=1,72
         i1=73-i
c
c               Diversion, Stream, etc.
         if(filenx(i1:i1).eq.'.') then
           if(filenx(i1+1:i1+3).eq.'b43')  then
             ibin=0                            
             idivx=0
             iwellw=0
           endif
c
c               Reservoir
           if(filenx(i1+1:i1+3).eq.'b44')  then
             ibin=0                            
             idivx=2
             iwellw=0
           endif
c
c               Well
           if(filenx(i1+1:i1+3).eq.'b42')  then
             ibin=0                            
             idivx=4
             iwellw=2
           endif

         endif
  110  continue

       if(ibin.eq.0 .and. idivx.eq.0) then
         iwelld=0
c
c _________________________________________________________
c rrb 02/01/03; Binary Diversion file with well data
c        open(43,file=filenx,  status='Unknown',access='direct',
c    1        recl=124, err=221)
c        open(43,file=filenx,  status='Unknown',access='direct',
c    1        recl=140, err=221)              
c rrb 2006/08/08; Include loss
         open(43,file=filenx,  status='Unknown',access='direct',
     1        recl=160, err=221)              

         iwelld=2
c
c rrb 99/12/02; Binary Diverion file without well data
c                (get here via err=221)
  221    if(iwelld.eq.0) then
           open(43,file=filenx,  status='Unknown',access='direct',
     1          recl=92, err=220)
           iwelld=1
         endif
       endif             
c
c _________________________________________________________
c rrb 99/12/02; Binary Reservoir file with well data
       if(ibin.eq.0 .and. idivx.eq.2) then
         iwellr=0
c
c rrb 2006/08/08; Include loss, etc         
c        open(44,file=filenx,  status='Unknown',access='direct',
c    1        recl=96, err=222)              
         open(44,file=filenx,  status='Unknown',access='direct',
     1        recl=160, err=222)              
         iwellr=2
         write(99,*) '  Bintop; iwellr ', iwellr
c
c rrb 99/12/02; Binary Reservoir file without well data
c               (get here via err=222)
  222    if(iwellr.eq.0) then
           open(44,file=filenx,  status='Unknown',access='direct',
     1          recl=84, err=220)
           iwellr=1
         endif
       endif

c
c _________________________________________________________ 
c rrb 02/01/07; Binary Well file
       if(ibin.eq.0 .and. idivx.eq.4) then
         iwellr=0
         open(42,file=filenx,  status='Unknown',access='direct',
     1        recl=92, err=220)
         iwellr=2
         write(99,*) '  Bintop; iwellr ', iwellr
       endif

c
c _________________________________________________________
c               Read ASCII input
       if(ibin.ne.0) then
         write(99,*) '  Getin; Opening Ascii file'
         open(1,file=filenx, status='old',err=220)
c
c               Determine if old or new data set
         do i=1,100
           read(1,'(a25)',end=230, err=230) rec25
c          write(99,'(a25)') rec25

           if(rec25(1:1).ne.'#') goto 230
c
c rrb 02/04/26; Revise to handle 9.96 Vs 9.58 better
cc          if(rec25(4:11).eq. 'Statemod') then
c           if(rec25(3:10).eq. 'Statemod') then

cc            rec3=rec25(21:23)
c             rec3=rec25(20:22)
           ifound=0
           if(rec25(4:11).eq. 'Statemod') then
             rec3=rec25(21:23)
             ifound=1
           endif
c
c		Version 12 (for sure)
           if(rec25(3:10).eq. 'Statemod') then
             rec3=rec25(20:22)
             ifound=1
           endif
c
c		Version 11 
           if(rec25(3:10).eq. 'Statemod' .and. rec3.eq.'   ') then
             rec3=rec25(22:24)
             ifound=1
           endif

           if(ifound.eq.1) then
             read(rec3, '(i3)') iver
             write(99,*) '  Getin; iver = ', iver

             if(iver.le.8) then
               iwelld=1
               iwellr=1
             endif
             
             if(iver.ge.9 .and. iver.le.10) then  
               iwelld=2
               iwellr=2
             endif
             
             if(iver.ge.11) then  
               iwelld=3
               iwellr=3
             endif
             
             write(99,*) 
     1         '  Getin; Diversion Well Data (iwelld) = ', iwelld
             write(99,*) 
     1         '  Genin; Reservoir Well Data (iwellr) = ', iwellr
             goto 115
           endif
         end do
         goto 220
       endif     

 115   filen(ifx+1) = filenx
c
c               Check for correct input
       if(ir.eq.2 .and. ifx+1.gt.2) then
         write(6,*) ' Problem, can difference two files only'
         call flush(6)
         goto 260
       endif
c
c               Get file type
       call skipn(5)
       read(5,'(a24)',end=180,err=180) ftype(ifx+1)
c
c               Get parameter type
       call skipn(5)
       read(5,'(a24)',end=190,err=190) ptype(ifx+1)  
c
c               Get Id requested (0=all)
       iid = 0
       idallx = 1
       call skipn(5)
  120  read(5,'(a12)',end=200,err=200) rec12
       rec3=rec12(1:3)
       if(rec3.eq.'all' .or. rec3.eq.'All' .or. 
     1    rec3.eq.'ALL' .or. rec3.eq.'0  ') then
         idallx=0
       endif                         
c       write(99,*) ' idallx = ', idallx

       if(rec12(1:4).ne.'-999') then
         iid = iid+1
         idreq(iid) = rec12
         if(rec12(1:1).eq.'#') goto 240
         goto 120
       endif                
c
c               Warn user no ID's read
       if(iid.eq.0) goto 200
c
c               Get time requested (0=ave)
       call skipn(5)             
       read(5,'(a12)',end=210,err=210) itime(ifx+1)
c
c rrb 2020/12/29; correction to read AVG in *.xop
cx        if(itime(ifx+1).eq.'Ave         '.or.
cx     1     itime(ifx+1).eq.'AVE         '.or.
cx     1     itime(ifx+1).eq.'ave         ') then
cx
        rec12=itime(ifx+1)
        itime3=rec12(1:3)
        
        if(itime3.eq.'Ave'.or.
     1     itime3.eq.'AVE'.or.
     1     itime3.eq.'ave'.or.
     1     itime3.eq.'AVG') then
          iyx = 0
        else
          iyx = 1
          backspace(5)
          read(5,'(i4, 1x, a3)',err=150) iyreq, imreq
          if(imreq.ne.'   ') iyx = 2
        endif                              
                             
  130   return  

  140   iend = 1       
        return             
c
c               Error messages    
c -----------------------------------------
c               Bad run type
  150    write(99,160) rtype
  160    format('    Bad run type = ', a10,/
     1     ' Acceptable run types:',/
     1     '   Single',/             
     1     '   Multiple',/
     1     '   Difference',/
     1     '   Merge',/
     1     '   Diffx',/
     1     '   Diff2',/
     1     '   Help',/
     1     '   Version')
        goto 260
c
c               Bad file name
  170   write(99,*) '   End of file or error reading file name'
        goto 260
c
c               Problem reading file type
  180   write(99,*) '   End of file or error reading file type'
        goto 260
c
c               Problem reading file type
  190   write(99,*) '   End of file or error reading parameter type'
        goto 260
c
c               Problem reading id
  200   write(99,*) '   End of file or error reading ID'
        write(99,*) '   Remember to end with a -999'
        write(99,*) '   Number of IDs read (should be > or = 1): ',iid
        goto 260
c
c               Problem reading time
  210   write(99,*) '   End of file or error reading time'
        goto 260
c
c               Problem reading time data
  220   write(99,224)  filenx
  224   format('   Getin; Cannot open file ',a72,/
     1        '           Check name, path, etc.')
        goto 260
c
c		Problem reading any data        
  230   write(99,232)  filenx
  232   format('   Getin; Problem reading file ',a72,/
     1        '           Check file, name, path, etc.')
        goto 260
c
c               Problem reading time
  240   write(99,250)  rec12
  250   format('   Problem with ID provided = ', a12,/,
     1        '   Remember end the Id specification with -999')
        goto 260

  260   write(6,270) fillog
        write(99,270) fillog
  270   format(' Getin - Unsuccessful termination, see ', a72)
        write(6,*) 'Stop 1'
        call flush(6)
        stop 
        end
