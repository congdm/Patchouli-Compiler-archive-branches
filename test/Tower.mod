MODULE Tower;
(*$CONSOLE*)

IMPORT
	Console;
	
VAR
	memory: ARRAY 101 OF INTEGER;
	i: INTEGER;

PROCEDURE Drop(bh: INTEGER): INTEGER;
	VAR dp, result, r2: INTEGER;
BEGIN
	IF bh = 1 THEN result := 0
	ELSIF bh = 2 THEN result := 1
	ELSE dp := 2; result := -1;
		WHILE dp <= bh DO
			r2 := dp - 1;
			IF r2 < memory[bh-dp+1] + 1 THEN r2 := memory[bh-dp+1] + 1 END;
			IF (result = -1) OR (result > r2) THEN result := r2 END;
			INC (dp)
		END
	END;
	RETURN result
END Drop;
	
BEGIN
	FOR i := 1 TO 100 DO
		memory[i] := Drop(i);
		Console.WriteInt (memory[i]);
		Console.Write(CHR(9))
	END
END Tower.