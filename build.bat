echo off
cd lib
echo on
..\bin\Poc Rtl.mod
echo off
move /Y *.dll ..\source\
move /Y *.sym ..\source\
cd ..
cd source
echo on
..\bin\Poc BaseSys.mod
..\bin\Poc Crypt.mod
..\bin\Poc Scanner.mod
..\bin\Poc Base.mod
..\bin\Poc Generator.mod
..\bin\Poc Parser.mod
..\bin\Poc Poc.mod
echo off
cd ..
echo on

