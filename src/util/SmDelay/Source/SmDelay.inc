c
c
c 	SmDelay.inc; common block for SmDelay
c
c _________________________________________________________

c
c ________________________________________________________
c		File Data
     
       common/comF/
     1 ifileNum(50), fileName(50), FileType(50),
     1 fileID(50), ifileNx(50)

       character filena*256,
     1 fileType*40, FileName*256, FileId*5,
     1 fileT1*40,   fileN1*256
c
c		Data for parse     
       character filenx*127, filelog*127, filenc*127, vdate*10
     
c
c ________________________________________________________
c		Delay Data    
       COMMON/SY3/
     1 interv, idly, nlog, ver, numdly,
     1 DLYRAT(240, 3100),RETUR (240,3000,2), ndly(3100),
     1 irtnid(3100), dlytot(3100),
     1 dlyratd(7320,3100), idayU(3100)

     
       Common/char1/
     1 cirtnid(3100)
     
       Character cirtnid*12     
c
c ________________________________________________________
c		Daily distribution Data    
     

       
      dimension iday(3100), qm(12,3100), qd(32,3100), qdx(32,3100),
     1           qp(32),    mthday(12)
c
      dimension cidy(3100), cidx(3100), dtype(14)

      character cidy*12, cidx*12, cidy1*12, dtype*55, blank*12,cidz*12
       
