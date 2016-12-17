..\bin\AyaCompile BaseSys.mod
..\bin\AyaCompile Crypt.mod
..\bin\AyaCompile Scanner1.mod
..\bin\AyaCompile Base1.mod
..\bin\AyaCompile Generator1.mod
..\bin\AyaCompile Parser1.mod
..\bin\AyaCompile AyaCompile1.mod

echo off
move /Y BaseSys.dll ..\bin1\
move /Y Crypt.dll ..\bin1\
move /Y Base1.dll ..\bin1\
move /Y Scanner1.dll ..\bin1\
move /Y Generator1.dll ..\bin1\
move /Y Parser1.dll ..\bin1\
move /Y AyaCompile1.exe ..\bin1\
echo on
