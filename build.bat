echo off
cd lib
echo on
..\bin\Poc Crypt.mod
..\bin\Poc Strings.mod
..\bin\Poc Rtl.mod
..\bin\Poc Out.mod
..\bin\Poc MemFiles.mod
echo off
move /Y *.dll ..\source\
move /Y *.sym ..\source\
cd ..
cd source
echo on
..\bin\Poc Scanner.mod
..\bin\Poc Base.mod
..\bin\Poc Generator.mod
..\bin\Poc Parser.mod
..\bin\Poc Poc.mod
echo off
cd ..
echo on

