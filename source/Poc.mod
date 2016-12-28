MODULE Poc;
(*$CONSOLE*)

IMPORT
	Rtl, Out, Scanner, Parser;
	
VAR
	srcfile: Rtl.File;
	str: ARRAY 256 OF CHAR;
	len, sym: INTEGER;
	
BEGIN
	Rtl.GetArg (str, len, 1);
	IF str[0] # 0X THEN
		IF Rtl.ExistFile(str) THEN
			Rtl.Reset(srcfile, str); Scanner.Init(srcfile, 0);
			Rtl.Close(srcfile); Scanner.Get(sym);
			IF sym = Scanner.module THEN Parser.Module
			ELSE Scanner.Mark('MODULE?')
			END
		ELSE Out.String('File not found')
		END
	ELSE
		Out.String('Patchouli Oberon-07 Compiler v0.8c'); Out.Ln;
 		Out.String('Usage: Poc <inputfile>'); Out.Ln
	END
END Poc.