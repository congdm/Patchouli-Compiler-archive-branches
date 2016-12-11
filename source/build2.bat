..\bin1\AyaCompile1 Crypt.mod
..\bin1\AyaCompile1 BaseSys.mod
..\bin1\AyaCompile1 Scanner1.mod
..\bin1\AyaCompile1 Base1.mod
..\bin1\AyaCompile1 Generator1.mod
..\bin1\AyaCompile1 Parser1.mod
..\bin1\AyaCompile1 AyaCompile1.mod

echo off
move /Y BaseSys.dll ..\bin2\
move /Y Crypt.dll ..\bin2\
move /Y Base1.dll ..\bin2\
move /Y Scanner1.dll ..\bin2\
move /Y Generator1.dll ..\bin2\
move /Y Parser1.dll ..\bin2\
move /Y AyaCompile1.exe ..\bin2\
move /Y BaseSys.sym ..\bin2\
move /Y Crypt.sym ..\bin2\
move /Y Base1.sym ..\bin2\
move /Y Scanner1.sym ..\bin2\
move /Y Generator1.sym ..\bin2\
move /Y Parser1.sym ..\bin2\
move /Y AyaCompile1.sym ..\bin2\
echo on