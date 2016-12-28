MODULE Poc;
(*$CONSOLE*)

IMPORT
	Sys := BaseSys, Scanner, Parser;
	
VAR
	srcfile: Sys.File;
	str: ARRAY 256 OF CHAR;
	len, sym: INTEGER;
	
BEGIN
	Sys.GetArg (str, len, 1);
	IF str[0] # 0X THEN
		IF Sys.Existed(str) THEN
			Sys.OpenReadOnly(srcfile, str); Scanner.Init(srcfile, 0);
			Sys.Close(srcfile); Scanner.Get(sym);
			IF sym = Scanner.module THEN Parser.Module
			ELSE Scanner.Mark('MODULE?')
			END
		ELSE Sys.Console_WriteStr('File not found')
		END
	ELSE
		Sys.Console_WriteStr('Patchouli Oberon-07 Compiler v0.8c');
		Sys.Console_WriteLn;
 		Sys.Console_WriteStr('Usage: Poc <inputfile>');
		Sys.Console_WriteLn
	END
END Poc.