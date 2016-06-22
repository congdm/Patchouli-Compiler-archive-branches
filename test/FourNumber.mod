MODULE FourNumber;
(*$CONSOLE*)

IMPORT
	Console;
	
TYPE
	Number = ARRAY 4 OF BYTE;
	
PROCEDURE Min (n: Number; VAR result: Number);
	VAR i, j, t: INTEGER;
BEGIN i := 0; result := n;
	WHILE i < 4 DO j := i + 1;
		WHILE j < 4 DO
			IF result[i] > result[j] THEN
				t := result[i]; result[i] := result[j]; result[j] := t
			END;
			INC (j)
		END;
		INC (i)
	END
END Min;

PROCEDURE Max (n: Number; VAR result: Number);
	VAR i, j, t: INTEGER;
BEGIN i := 0; result := n;
	WHILE i < 4 DO j := i + 1;
		WHILE j < 4 DO
			IF result[i] < result[j] THEN
				t := result[i]; result[i] := result[j]; result[j] := t
			END;
			INC (j)
		END;
		INC (i)
	END
END Max;

PROCEDURE Subtract (VAR a: Number; b: Number);
	VAR i, carry: INTEGER;
BEGIN i := 3; carry := 0;
	WHILE i >= 0 DO
		IF a[i] - carry >= b[i] THEN a[i] := a[i] - carry - b[i]; carry := 0
		ELSE a[i] := 10 + a[i] - carry - b[i]; carry := 1
		END;
		DEC (i)
	END
END Subtract;

PROCEDURE Print (n: Number);
BEGIN
	Console.WriteInt (n[0]);
	Console.WriteInt (n[1]);
	Console.WriteInt (n[2]);
	Console.WriteInt (n[3]);
	Console.Write (' ')
END Print;

PROCEDURE Run (n: Number);
	VAR num, min, max: Number; i: INTEGER;
BEGIN num := n; Print (n);
	FOR i := 1 TO 8 DO
		Min (num, min); Max (num, max); Subtract (max, min);
		WHILE max[0] = 0 DO
			max[0] := max[1]; max[1] := max[2]; max[2] := max[3]; max[3] := 0
		END;
		num := max; Print (max)
	END;
	Console.WriteLn
END Run;

PROCEDURE Convert (n: INTEGER; VAR result: Number);
BEGIN
	result[3] := n MOD 10; n := n DIV 10;
	result[2] := n MOD 10; n := n DIV 10;
	result[1] := n MOD 10; n := n DIV 10;
	result[0] := n MOD 10
END Convert;

PROCEDURE MainProgram;
	VAR n: Number; i: INTEGER;
BEGIN
	FOR i := 1000 TO 9998 DO
		IF (i # 1111) & (i # 2222) & (i # 3333) & (i # 4444)
			& (i # 5555) & (i # 6666) & (i # 7777) & (i # 8888) THEN
			Convert (i, n); Run (n)
		END
	END
END MainProgram;

BEGIN
	MainProgram
END FourNumber.