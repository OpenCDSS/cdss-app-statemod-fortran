c	
	Subroutine OprinOut(nlog, maxopr, k, 
     1    ityopr1, cidvri, 
     1    ciopde,  iopdes1, iopdes2, 
     1    ciopso1, iopSou1,iopsou2,
     1    ciopso2, iopsou3,iopsou4, creuse, ireuse1,
     1    oprloss1, oprlimit1, iopSou5, iopSou6, iopsou7,
     1    cdivtyp1, intern, cntern, cAssoc, cAssoc2, cAssoc3)
c
c _________________________________________________________
c	Program Description
c
c	OprinOut; It prints results of reading data in Oprinp.f
c
c _________________________________________________________        
c	Dimensions
c	
      dimension intern(maxopr,10), cntern(10)
      character*12 cidvri,  ciopde,  ciopso1, ciopso2, ciopso3,
     1             ciopso4, ciopso5, cntern,  creuse, cdivtyp1,
     1             cAssoc,  cAssoc2, cAssoc3
c
c _________________________________________________________
c		Step 1; Print operational rule data
c
        write(nlog,2020) ityopr1, cidvri, ityopr1,    
     1    ciopde, iopdes1, iopdes2, 
     1    ciopso1, iopSou1,iopsou2,
     1    ciopso2, iopsou3,iopsou4, creuse, ireuse1,
     1    oprloss1,oprlimit1,
     1    cAssoc,  iopSou5, 
     1    cAssoc2, iopSou6, cAssoc3, iopSou7, cdivtyp1
        
        do i=1,10
          if(intern(k,i).ne.0) then
            write(nlog,2021) cntern(i), intern(k,i)
          endif  
        end do
c
c _________________________________________________________        
        return
c
c _________________________________________________________        
        
 2020   format(/,60('_'),/
     1  '  OprinOut; Detailed output for operating rule type ', i5,/
     1  10x, 'ID and Type       = ', a12, i5,/
     1  10x, 'Destination       = ', a12, i5,/        
     1  10x, 'Destination Acct  = ', 12x, i5,/             
     1  10x, 'Source 1          = ', a12, i5,/
     1  10x, 'Source 1 Acct     = ', 12x, i5,/             
     1  10x, 'Source 2          = ', a12, i5,/
     1  10x, 'Source 2 Acct     = ', 12x, i5,/             
     1  10x, 'cReuse            = ', a12, i5,/
     1  10x, 'Loss              = ', 12x, f6.0,/
     1  10x, 'OprLimit          = ', 12x, f6.0,/
     1  10x, 'Associated Rule 1 = ', a12, i5,/
     1  10x, 'Associated Rule 2 = ', a12, i5,/ 
     1  10x, 'Associated Rule 3 = ', a12, i5,/              
     1  10x, 'Diversion type    = ', a12)
                                
 2021   format(                 
     1  10x, 'Carrier           = ', a12, i5)
c
c _________________________________________________________        
      stop
      end
	
