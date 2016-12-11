..\bin3\AyaCompile1 Crypt.mod
..\bin3\AyaCompile1 BaseSys.mod
..\bin3\AyaCompile1 Scanner1.mod
..\bin3\AyaCompile1 Base1.mod
..\bin3\AyaCompile1 Generator1.mod
..\bin3\AyaCompile1 Parser1.mod
..\bin3\AyaCompile1 AyaCompile1.mod

echo off
move /Y BaseSys.dll ..\bin4\
move /Y Crypt.dll ..\bin4\
move /Y Base1.dll ..\bin4\
move /Y Scanner1.dll ..\bin4\
move /Y Generator1.dll ..\bin4\
move /Y Parser1.dll ..\bin4\
move /Y AyaCompile1.exe ..\bin4\
move /Y BaseSys.sym ..\bin4\
move /Y Crypt.sym ..\bin4\
move /Y Base1.sym ..\bin4\
move /Y Scanner1.sym ..\bin4\
move /Y Generator1.sym ..\bin4\
move /Y Parser1.sym ..\bin4\
move /Y AyaCompile1.sym ..\bin4\
echo on