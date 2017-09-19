c
          subroutine GetOpr(nlog, nf, itype, nopr, cidvri)
c
c
c _________________________________________________________
c	Program Description
c
c       GetOpr; it reads various operational right data
c
c
c _________________________________________________________
c	Update History
c	
c	2007/07/03; Created
c _________________________________________________________
c
c       Documentaiton               
c
c               ion=-1 means do not check right on/off switch
c               ion= 0 means do not turn off opr right 
c               ion= 1 means turn off source right (controlled by
c		       an operating rule
c
c _________________________________________________________
c	Dimensions
        dimension OprType(25), nopr(25)
        character OprType*36, rec132*132, rec36*36, cidvri*12,
     1    rec1*1
        
        data OprType/
     1    'On_Off_Data                         ',
     1    'Carrier_Data                        ',                
     1    'Carrier_With_Loss_Data              ',
     1    'Diversion_Limit_Data                ', 
     1    'Return_Flow_Factors                 ',
     1    'Return_Flow_Data                    ',           
     1    'Annual_Limit_Data                   ',
     1    ' ',
     1    17*' '/
c     
c _________________________________________________________
c		Step 1; Initilize
        
        iout=1
        
        n=10
        write(nlog,*) ' GetOpr; ' 
        ntype=0
        rec36=' '
        rec36(1:2) = 'NA'              
        
        do i=1,n
          nopr(i)=0
        end do      
c     
c _________________________________________________________
c		Step 2; Read Data
        
 100    read(55,'(a132)',end=2000,err=2000) rec132
        if(iout.eq.1) write(nlog,*) ' Getopr; ', rec132
        if(rec132(1:1).eq.'#') goto 100
c     
c _________________________________________________________
c		Step 3; Find Code if any
        do i=1,132
          if(rec132(i:i).eq.'-') then
            rec36=' '
            jb=i+1
            je=i+36
            
            j1=0
            do j=jb,je
              j1=j1+1
              rec1=rec132(j:j)
              if(rec1.eq.' ') goto 10
              rec36(j1:j1)=rec1
            end do  
 10         write(nlog,*) ' GetOpr; rec36 = ', rec36
c     
c _________________________________________________________
c		Step 4; Tie code to type
            
            do j=1,n
              if(rec36.eq.OprType(j)) then
                ntype=j
                nopr(j)=nopr(j)+1
              endif  
            end do  
            
          endif
        end do    
c     
c _________________________________________________________
c		Step 5; Detailed Output
        
        if(iout.eq.1) then
          write(nlog,200) cidvri, itype, ntype, rec36, Oprtype(ntype)
        endif  
c     
c _________________________________________________________
c		Step 6; Return
        
        return
c     
c _________________________________________________________
c		Formats
 200  format(/,72('_'),/
     1 ' GetOpr; Reading .opr rule ID = ', a12,' itype ', i5, / 
     1 10x,' Found Type = ',i5,/
     1 10x,' Data       = ', a36,/
     1 10x,' Match      = ', a36)
    
 2000 write(nlog,2010) cidvri, itype      
 2010 format(/,72('_'),/
     1 ' GetOpr; Problem with *.opr rule ID = ', a12,
     1 ' itype ', i5, / 
     1 10x,'Cannot read the operating rule secondary data')
      goto 9999
      
c     
c _________________________________________________________
c		Error Processing

 9999 write(6,150)
      write(nlog,150)
  150 format(/,72('_'),/
     1  '  Stopped in GetOpr, see the log file (*.log)')      
      write(6,*) 'Stop 1'
      call flush (6)
      call exit(1)
c
c _________________________________________________________
c

      stop
      end
          
