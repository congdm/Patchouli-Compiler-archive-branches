MODULE Test;
(*$CONSOLE*)

IMPORT
	SYSTEM, Sys := BaseSys, Crypt;
	
VAR
	hash: Crypt.MD5Hash;
	data: ARRAY 256 OF BYTE;
	low, high, i: INTEGER;
	
PROCEDURE SetStr(VAR dst: ARRAY OF BYTE; src: ARRAY OF CHAR): INTEGER;
	VAR i: INTEGER;
BEGIN
	FOR i := 0 TO LEN(src)-1 DO dst[i] := ORD(src[i]) END;
	RETURN i
END SetStr;

BEGIN
	Crypt.InitMD5Hash(hash);
	i := SetStr(data, '12345678901234567890123456789012345678901234567890123456789012345678901234567890');
	Crypt.MD5Compute(hash, data, i-1);
	low := Crypt.MD5GetLowResult(hash);
	high := Crypt.MD5GetHighResult(hash);
	FOR i := 0 TO 7 DO
		Sys.Console_WriteHex(low MOD 256);
		Sys.Console_Write(' '); low := ASR(low, 8)
	END;
	FOR i := 0 TO 7 DO
		Sys.Console_WriteHex(high MOD 256);
		Sys.Console_Write(' '); high := ASR(high, 8)
	END;
END Test.