MODULE AyaCompile1;
(*$CONSOLE*)

IMPORT SYSTEM,
	Sys := BaseSys, Scanner := Scanner1, Parser := Parser1;
	
VAR
	srcfile: Sys.File;
	str: ARRAY 256 OF CHAR;
	len, sym: INTEGER;
	
BEGIN
	Sys.GetArg (str, len, 1);
	IF str[0] # 0X THEN
		IF Sys.Existed(str) THEN
			Sys.Open(srcfile, str); Scanner.Init(srcfile, 0);
			Sys.Close(srcfile); Scanner.Get(sym); 
			IF sym = Scanner.module THEN Parser.Module
			ELSE Scanner.Mark('MODULE?')
			END
		ELSE Sys.Console_WriteStr('File not found')
		END
	ELSE
		Sys.Console_WriteStr('AyaCompiler v0.8-alpha for Oberon-07 language');
		Sys.Console_WriteLn;
 		Sys.Console_WriteStr('Usage: AyaCompile <inputfile>');
		Sys.Console_WriteLn
	END
END AyaCompile1.