MODULE AyaCompile1;
(*$CONSOLE*)

IMPORT
	Console, Base := Base1, Scanner := Scanner1, Parser := Parser1;
	
VAR
	srcfile: Base.FileHandle;
	str: ARRAY 256 OF CHAR;
	len, sym: INTEGER;
	
BEGIN
	Base.GetArg (str, len, 1);
	IF str[0] # 0X THEN Base.Open (srcfile, str);
		Scanner.Init (srcfile, 0); Scanner.Get (sym);
		IF sym = Scanner.module THEN Parser.Module
		ELSE Scanner.Mark ('Not a MODULE or LIBRARY')
		END;
		Base.Close (srcfile)
	ELSE
		Console.WriteString ('AyaCompiler v0.7a for Oberon-07 language');
		Console.WriteLn; 
 		Console.WriteString ('Usage: AyaCompile <inputfile>');
		Console.WriteLn
	END
END AyaCompile1.