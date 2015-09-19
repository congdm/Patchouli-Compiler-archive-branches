DEFINITION Gdi32;

CONST
	LF_FACESIZE* = 32;
	
	(* Device Parameters for GetDeviceCaps() *)
	LOGPIXELSX* = 88;
	LOGPIXELSY* = 90;

TYPE
	LOGFONTW* = RECORD
		lfHeight*: INT32;
		lfWidth*: INT32;
		lfEscapement*: INT32;
		lfOrientation*: INT32;
		lfWeight*: INT32;
		lfItalic*: BYTE;
		lfUnderline*: BYTE;
		lfStrikeOut*: BYTE;
		lfCharSet*: BYTE;
		lfOutPrecision*: BYTE;
		lfClipPrecision*: BYTE;
		lfQuality*: BYTE;
		lfPitchAndFamily*: BYTE;
		lfFaceName*: ARRAY LF_FACESIZE OF CHAR
	END;
	
PROCEDURE GetDeviceCaps* (hdc: INTEGER; nIndex: INT32): INT32;

END Gdi32.