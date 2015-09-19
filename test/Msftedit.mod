DEFINITION Msftedit;

IMPORT
	User32, Gdi32;
	
CONST
	(* Rich Edit Messages *)
	EM_GETCHARFORMAT* = User32.WM_USER + 58;
	EM_SETCHARFORMAT* = User32.WM_USER + 68;
	
	(* EM_SETCHARFORMAT wParam masks *)
	SCF_DEFAULT* = {};
	SCF_SELECTION* = {0};

TYPE
	CHARFORMATW* = RECORD
		cbSize*: CARD32;
		dwMask*, dwEffects*: CARD32;
		yHeight*, yOffset*: INT32;
		crTextColor*: CARD32;
		bCharSet*, bPitchAndFamily*: BYTE;
		szFaceName*: ARRAY Gdi32.LF_FACESIZE OF CHAR
	END;

END Msftedit.