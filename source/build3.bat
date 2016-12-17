..\bin2\AyaCompile1 BaseSys.mod
..\bin2\AyaCompile1 Crypt.mod
..\bin2\AyaCompile1 Scanner1.mod
..\bin2\AyaCompile1 Base1.mod
..\bin2\AyaCompile1 Generator1.mod
..\bin2\AyaCompile1 Parser1.mod
..\bin2\AyaCompile1 AyaCompile1.mod

echo off
move /Y BaseSys.dll ..\bin3\
move /Y Crypt.dll ..\bin3\
move /Y Base1.dll ..\bin3\
move /Y Scanner1.dll ..\bin3\
move /Y Generator1.dll ..\bin3\
move /Y Parser1.dll ..\bin3\
move /Y AyaCompile1.exe ..\bin3\
echo on