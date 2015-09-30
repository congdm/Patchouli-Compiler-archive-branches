MODULE TextFrames;

IMPORT
	SYSTEM, Kernel32, User32, Texts;
	
CONST
	className = 'TextFrame';
	
TYPE
	TextFrame = POINTER TO EXTENSIBLE RECORD
		hwnd: INTEGER;
		txt: Texts.Text
	END;
	
PROCEDURE NewTextFrame (VAR tf: TextFrame; txt: Texts.Text; hwndParent: INTEGER);
BEGIN NEW (tf);
	(* tf.hwnd := User32.CreateWindowExW(
		0, className, NIL
	); *)
END NewTextFrame;

PROCEDURE WndProc (hWnd: INTEGER; uMsg: CARD32; wParam, lParam: INTEGER): INTEGER;
BEGIN
	RETURN User32.DefWindowProcW (hWnd, uMsg, wParam, lParam)
END WndProc;

PROCEDURE InitModule;
	VAR wordRes: CARD16; wcls: User32.WNDCLASSW;
BEGIN
	wcls.style := ORD(User32.CS_GLOBALCLASS);
	wcls.lpfnWndProc := WndProc;
	wcls.cbClsExtra := 0;
	wcls.cbWndExtra := 0;
	wcls.hInstance := Kernel32.GetModuleHandleW(NIL);
	wcls.hIcon := 0;
	wcls.hCursor := 0;
	wcls.hbrBackground := 0;
	wcls.lpszMenuName := 0;
	wcls.lpszClassName := SYSTEM.ADR(className);
	
	wordRes := User32.RegisterClassW (wcls)
END InitModule;

BEGIN
	InitModule
END TextFrames.