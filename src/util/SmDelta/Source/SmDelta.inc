c
c Smdelta.inc
c 
c rrb 2019/08/11; Revised dimension from 218 to 5001 and 
c                 Smdelta.for maxsta to 5001
       common/c1/
     1            iyx,   iyreq, ip,  iid,  ir, ibin, idiv, ichk, ifx,
     1            idallx,iout,
     1            ftype(10), ptype(10), idreq(5001),  itime(10),
     1            filen(10),
     1            rtype, imreq, nlog, iver, 
c
c rrb 99/12/2
     1            iwelld,iwellr, iwellw, ctitle, rec6
       character
     1            ftype*24, ptype*24,  idreq*12,  itime*12, filen*72,
     1            rtype*10, imreq*3,   ctitle*72, rec6*6
                                                                      
       common/c2/
     1           is, ry2(10), dels(10,5001), namex(10,5001),
     1           idz(10,5001),
     1           isx(10)
       character namex*24, idz*12
c
c rrb 2019/08/11; Add operating type 
       common/c2b/
     1           ctypeX(10,5001)  
       character ctypeX*2  
        
       common/c3/
     1           numsta, numdiv, numifr, numres, numown, nrsact,
     1           numrun, numtop, maxsta, numdivw,
     1           mthday(12),  dat1(40), dat1t(40),
     1           iystr0,      iyend0,
     1           idvsta(5001), ifrsta(545), irssta(155), irusta(5001),
     1           iressw(155),  nowner(155), idvstaw(5001),
     1           xmonam(14)
        character xmonam*4

       common/c4/
     1           cstaid(5001), stanam(5001),
     1           cdivid(5001), divnam(5001), 
     1           cifrid(545),  xfrnam(545), 
     1           cresid(155),  resnam(155),  
     1           crunid(5001), runnam(5001), 
     1           cdividw(5001),divnamw(5001)

       character cstaid*12, cdivid*12, cifrid*12, cresid*12,
     1           stanam*24, divnam*24, xfrnam*24, resnam*24,
     1           crunid*12, runnam*24, cdividw*12,divnamw*24

        common/c5/ x1(50)

        common/c6/ filenc, filenx, rec12, rec1,
     1            filena, fpath1, fillog, filout
        character filenc*72, filenx*72, rec12*12, rec1*1,
     1            filena*72, fpath1*72, fillog*72, filout*72



