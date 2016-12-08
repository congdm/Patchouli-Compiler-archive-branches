MODULE Test;
(*$MAIN*)

IMPORT
	SYSTEM;
	
TYPE
	Str = ARRAY 256 OF CHAR;

	Rec = RECORD
		x: INTEGER
	END;
	
	Rec2 = RECORD (Rec)
	END;
	
	Rec3 = RECORD (Rec2)
	END;

VAR
	MessageBoxW: PROCEDURE(hWnd: INTEGER; lpText, lpCaption: Str; uType: INTEGER);

PROCEDURE P1*;
	VAR a: ARRAY 10 OF INTEGER; i, j, min: INTEGER;
		user32: INTEGER;
	
	PROCEDURE P2;
		VAR hModule: INTEGER;
	BEGIN SYSTEM.LoadLibraryW(hModule, 'Hello.dll'); P1
	END P2;
	
BEGIN SYSTEM.LoadLibraryW(user32, 'User32.dll');
	SYSTEM.GetProcAddress(MessageBoxW, user32, 2050);
	MessageBoxW(0, 'Hello, world!', 'Test', 0)
END P1;

BEGIN P1
END Test.