..\bin\AyaCompile Strings.mod
..\bin\AyaCompile Crypt.mod
..\bin\AyaCompile Kernel32.mod
..\bin\AyaCompile Console.mod
..\bin\AyaCompile Base.mod
..\bin\AyaCompile Scanner.mod
..\bin\AyaCompile SymTable.mod
..\bin\AyaCompile Generator.mod
..\bin\AyaCompile Parser.mod
..\bin\AyaCompile Parser2.mod
..\bin\AyaCompile AyaCompile.mod

echo off
move /Y Strings.dll ..\bin\
move /Y Crypt.dll ..\bin\
move /Y Console.dll ..\bin\
move /Y Base.dll ..\bin\
move /Y Scanner.dll ..\bin\
move /Y SymTable.dll ..\bin\
move /Y Generator.dll ..\bin\
move /Y Parser.dll ..\bin\
move /Y Parser2.dll ..\bin\
move /Y AyaCompile.exe ..\bin\
echo on