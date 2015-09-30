MODULE PrintNumber;
(*$CONSOLE*)

IMPORT
	SYSTEM, Kernel32;
	
CONST
	N = 7;
	
VAR
	arr: ARRAY N OF INTEGER;
	i: INTEGER;
	done, stillBacking: BOOLEAN;
	buffer: ARRAY 65536 OF BYTE;
	bufPos, stdout: INTEGER;
	
PROCEDURE FlushBuffer;
	VAR bRes: INTEGER; nWrite: CARD32;
BEGIN
	bRes := Kernel32.WriteFile (stdout, buffer, bufPos, nWrite, NIL);
	bufPos := 0
END FlushBuffer;
	
PROCEDURE Print (arr: ARRAY OF INTEGER);
	VAR i: INTEGER;
BEGIN
	i := 0;
	WHILE i < LEN(arr) DO
		IF bufPos = LEN(buffer) THEN FlushBuffer END;
		buffer[bufPos] := ORD('0') + arr[i]; INC (bufPos);
		INC (i)
	END;
	IF bufPos = LEN(buffer) THEN FlushBuffer END;
	buffer[bufPos] := ORD(' '); INC (bufPos)
END Print;

BEGIN
	(* Giả sử ban đầu mảng arr toàn 0 *)
	stdout := Kernel32.GetStdHandle(Kernel32.STD_OUTPUT_HANDLE);
	bufPos := 0; i := 0; done := FALSE;
	WHILE ~done DO
		IF (i = 0) & (arr[i] = 10) THEN
			done := TRUE
		ELSIF i < LEN(arr) - 1 THEN
			INC (i)
		ELSIF i = LEN(arr) - 1 THEN
			Print (arr);
			IF arr[i] < 9 THEN INC (arr[i])
			ELSIF arr[i] = 9 THEN (* backtrack *)
				arr[i] := 0; stillBacking := TRUE;
				WHILE stillBacking DO
					DEC (i); INC (arr[i]);
					IF arr[i] = 10 THEN
						IF i > 0 THEN
							arr[i] := 0; stillBacking := TRUE
						ELSIF i = 0 THEN stillBacking := FALSE
						END
					ELSE stillBacking := FALSE
					END
				END
			END
		END
	END;
	IF bufPos # 0 THEN FlushBuffer END
END PrintNumber.