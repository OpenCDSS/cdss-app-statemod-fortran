rem 
rem l.bat
rem
rem lf90 @linkme.rsp -out=smtest.exe
rem lf90 -nobanner -nomap -pack -nwin -bind -exe delplt.exe delplt.obj 
lf90 -nobanner -nomap -pack -nwin -bind -exe %1.exe %1.obj 
